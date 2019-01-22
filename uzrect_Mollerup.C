// uzrect_Mollerup.C --- A 2D solution to Richard's equation in a rect. grid.
// 
// Copyright 2006, 2007, 2008 Mikkel Mollerup, Per Abrahamsen and KVL.
//
// This file
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL// Uncomment for fast code that does not catches bugs.
#define BOOST_UBLAS_NDEBUG
#define NDEBUG

#include "uzrect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "groundwater.h"
#include "surface.h"
#include "solver.h"
#include "log.h"
#include "frame.h"
#include "block_model.h"
#include "mathlib.h"
#include "assertion.h"
#include "librarian.h"
#include "anystate.h"
#include "condedge.h"
#include "treelog.h"

#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/io.hpp>

#include <sstream>
#include <algorithm>

namespace ublas = boost::numeric::ublas;

struct UZRectMollerup : public UZRect
{
  // Types.
  enum top_state { top_undecided, top_flux, top_pressure };

  // Parameters.  
  const std::unique_ptr<Solver> solver;
  std::unique_ptr<const Condedge> K_average;  
  const int max_time_step_reductions;
  const int time_step_reduction;
  const int max_iterations; 
  const int max_iterations_timestep_reduction_factor;
  const int max_number_of_small_time_steps;
  const int msg_number_of_small_time_steps;
  const double max_absolute_difference;
  const double max_relative_difference;
  const double max_pressure_potential;
  const double min_pressure_potential;
  const bool use_forced_T;
  const double forced_T;
  const int debug;

  // Log variable.
  ublas::vector<double> Theta_error;
  ublas::vector<double> Kedge;

  // Interface.
  void tick (const GeometryRect&, const std::vector<size_t>& drain_cell,
	     const double drain_water_level, // [cm]
	     const Soil&, SoilWater&, const SoilHeat&, 
             const Surface&, const Groundwater&, 
             double dt, Treelog&);
  void output (Log&) const;
  
  // Internal functions.
  double find_K_edge (const Soil& soil, const Geometry& geo, 
                      const size_t e,
                      const ublas::vector<double>& h, 
                      const ublas::vector<double>& h_ice, 
                      const ublas::vector<double>& h_old, 
                      const ublas::vector<double>& T) const;
  bool converges (const ublas::vector<double>& previous,
		  const ublas::vector<double>& current) const;
  static void Neumann (const size_t edge, const size_t cell, 
                       const double area, const double in_sign,
                       const double flux, 
                       ublas::vector<double>& dq, ublas::vector<double>& B);
  static void Dirichlet (const size_t edge, const size_t cell, 
                         const double area, const double in_sign,
                         const double sin_angle, 
                         const double K_cell, const double h_old,
                         const double K_area_per_length, const double pressure,
                         ublas::vector<double>& dq,
                         ublas::banded_matrix<double>& Dm_mat, 
                         ublas::vector<double>& Dm_vec, 
                         ublas::vector<double>& Gm);
  static void lowerboundary (const GeometryRect& geo,
			     const Groundwater&,
			     const std::vector<bool>& active_lysimeter,
			     const ublas::vector<double>& h,
			     const ublas::vector<double>& K,
			     ublas::vector<double>& q,
			     ublas::banded_matrix<double>& Dm_mat, 
			     ublas::vector<double>& Dm_vec, 
			     ublas::vector<double>& Gm, 
			     ublas::vector<double>& B, Treelog& msg);
  static void upperboundary (const GeometryRect& geo,
                             const Soil& soil, 
                             const ublas::vector<double>& T,
			     const Surface& surface,
                             std::vector<top_state>& state,
			     const ublas::vector<double>& remaining_water,
			     const ublas::vector<double>& h,
			     const ublas::vector<double>& K,
			     ublas::vector<double>& q,
			     ublas::banded_matrix<double>& Dm_mat, 
			     ublas::vector<double>& Dm_vec, 
			     ublas::vector<double>& Gm, 
			     ublas::vector<double>& B,
			     const double dt,
			     const int debug,
                             Treelog& msg, const double BIG_DT);
  static void drain (const GeometryRect& geo,
                     const std::vector<size_t>& drain_cell,
		     const double drain_water_level,
                     const ublas::vector<double>& h,
                     const ublas::vector<double>& Theta_previous,
                     const ublas::vector<double>& Theta,
                     const ublas::vector<double>& S_vol,
#ifdef TEST_OM_DEN_ER_BRUGT
                     const ublas::vector<double>& S_macro,
#endif
                     const ublas::vector<double>& dq,
                     const double& ddt,
                     std::vector<bool>& drain_cell_on,
                     Solver::Matrix& A,
                     ublas::vector<double>& b, 
                     const int debug, Treelog& msg);
  static void diffusion (const GeometryRect& geo,
			 const ublas::vector<double>& Kedge,
			 Solver::Matrix& diff);
  static void gravitation (const GeometryRect& geo,
			   const ublas::vector<double>& Kedge,
			   ublas::vector<double>& grav);  
  static void Darcy (const GeometryRect& geo,
                     const ublas::vector<double>& Kedge,
                     const ublas::vector<double>& h,
                     ublas::vector<double>& dq);


  // Create and Destroy.
  void initialize (const Geometry& geo, const bool has_macropores);
  UZRectMollerup (const BlockModel& al);
  ~UZRectMollerup ();
};

void 
UZRectMollerup::tick (const GeometryRect& geo,
		      const std::vector<size_t>& drain_cell,
		      const double drain_water_level,
		      const Soil& soil, 
		      SoilWater& soil_water, const SoilHeat& soil_heat,
		      const Surface& surface, const Groundwater& groundwater,
		      const double dt, Treelog& msg)

{
  daisy_assert (K_average.get ());
  const size_t edge_size = geo.edge_size (); // number of edges 
  const size_t cell_size = geo.cell_size (); // number of cells 

  // Insert magic here.
  
  ublas::vector<double> Theta (cell_size); // water content 
  ublas::vector<double> Theta_previous (cell_size); // at start of small t-step
  ublas::vector<double> h (cell_size); // matrix pressure
  ublas::vector<double> h_previous (cell_size); // at start of small timestep
  ublas::vector<double> h_ice (cell_size); // 
  ublas::vector<double> S (cell_size); // sink term
  ublas::vector<double> S_vol (cell_size); // sink term
#ifdef TEST_OM_DEN_ER_BRUGT
  ublas::vector<double> S_macro (cell_size);  // sink term
  std::vector<double> S_drain (cell_size, 0.0); // matrix-> macro -> drain flow 
  std::vector<double> S_drain_sum (cell_size, 0.0); // For large timestep
  const std::vector<double> S_matrix (cell_size, 0.0);  // matrix -> macro 
  std::vector<double> S_matrix_sum (cell_size, 0.0); // for large timestep
#endif
  ublas::vector<double> T (cell_size); // temperature 
  ublas::vector<double> Kold (edge_size); // old hydraulic conductivity
  ublas::vector<double> Ksum (edge_size); // Hansen hydraulic conductivity
  ublas::vector<double> Kcell (cell_size); // hydraulic conductivity
  ublas::vector<double> Kold_cell (cell_size); // old hydraulic conductivity
  ublas::vector<double> Ksum_cell (cell_size); // Hansen hydraulic conductivity
  ublas::vector<double> h_lysimeter (cell_size);
  std::vector<bool> active_lysimeter (cell_size);
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  ublas::vector<double> remaining_water (edge_above_size);
  std::vector<bool> drain_cell_on (drain_cell.size (),false); 
  

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      remaining_water (i) = surface.h_top (geo, edge);
    }
  ublas::vector<double> q;	// Accumulated flux
  q = ublas::zero_vector<double> (edge_size);
  ublas::vector<double> dq (edge_size); // Flux in small timestep.
  dq = ublas::zero_vector<double> (edge_size);

  //Make Qmat area diagonal matrix 
  //Note: This only needs to be calculated once... 
  ublas::banded_matrix<double> Qmat (cell_size, cell_size, 0, 0);
  for (int c = 0; c < cell_size; c++)
    Qmat (c, c) = geo.cell_volume (c);
 
  // make vectors 
  for (size_t cell = 0; cell != cell_size ; ++cell) 
    {				
      Theta (cell) = soil_water.Theta (cell);
      h (cell) =  soil_water.h (cell);
      h_ice (cell) = soil_water.h_ice (cell);
      S (cell) =  soil_water.S_sum (cell);
      S_vol (cell) = S (cell) * geo.cell_volume (cell);
      if (use_forced_T)
	T (cell) = forced_T;
      else 
	T (cell) = soil_heat.T (cell); 
      h_lysimeter (cell) = geo.zplus (cell) - geo.cell_z (cell);
    }

  // Remember old value.
  Theta_error = Theta;

  // Start time loop 
  double time_left = dt;	// How much of the large time step left.
  double ddt = dt;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;
  

  int n_small_time_steps = 0;
  
  while (time_left > 0.0)
    {
      if (ddt > time_left)
	ddt = time_left;

      std::ostringstream tmp_ddt;
      tmp_ddt << "Time t = " << (dt - time_left) 
              << "; ddt = " << ddt
              << "; steps " << n_small_time_steps 
              << "; time left = " << time_left;
      Treelog::Open nest (msg, tmp_ddt.str ());

      if (n_small_time_steps > 0
          && (n_small_time_steps%msg_number_of_small_time_steps) == 0)
        {
          msg.touch ();
          msg.flush ();
        }
      
      n_small_time_steps++;
      if (n_small_time_steps > max_number_of_small_time_steps) 
        {
          msg.debug ("Too many small timesteps");
          throw "Too many small timesteps";
        }
      
      // Initialization for each small time step.

      if (debug > 0)
	{
	  std::ostringstream tmp;
	  tmp << "h = " << h << "\n";
	  tmp << "Theta = " << Theta;
	  msg.message (tmp.str ());
	}

      int iterations_used = 0;
      h_previous = h;
      Theta_previous = Theta;

      if (debug == 5)
	{
	  std::ostringstream tmp;
	  tmp << "Remaining water at start: " << remaining_water;
	  msg.message (tmp.str ());
	}

      ublas::vector<double> h_conv;

      for (size_t cell = 0; cell != cell_size ; ++cell)
        active_lysimeter[cell] = h (cell) > h_lysimeter (cell);

      for (size_t edge = 0; edge != edge_size ; ++edge)
        {
          Kold[edge] = find_K_edge (soil, geo, edge, h, h_ice, h_previous, T);
          Ksum [edge] = 0.0;
        }

      std::vector<top_state> state (edge_above.size (), top_undecided);
      
      // We try harder with smaller timesteps.
      const int max_loop_iter 
        = max_iterations * (number_of_time_step_reductions 
                            * max_iterations_timestep_reduction_factor + 1);
      do // Start iteration loop
	{
	  h_conv = h;
	  iterations_used++;
          

          std::ostringstream tmp_conv;
          tmp_conv << "Convergence " << iterations_used; 
          Treelog::Open nest (msg, tmp_conv.str ());
          if (debug == 7)
            msg.touch ();

	  // Calculate conductivity - The Hansen method
	  for (size_t e = 0; e < edge_size; e++)
	    {
              Ksum[e] += find_K_edge (soil, geo, e, h, h_ice, h_previous, T);
              Kedge[e] = (Ksum[e] / (iterations_used  + 0.0)+ Kold[e]) / 2.0;
	    }

	  //Initialize diffusive matrix
	  Solver::Matrix diff (cell_size);
	  // diff = ublas::zero_matrix<double> (cell_size, cell_size);
	  diffusion (geo, Kedge, diff);

	  //Initialize gravitational matrix
	  ublas::vector<double> grav (cell_size); //ublass compatibility
	  grav = ublas::zero_vector<double> (cell_size);
	  gravitation (geo, Kedge, grav);

	  // Boundary matrices and vectors
	  ublas::banded_matrix<double>  Dm_mat (cell_size, cell_size, 
                                                0, 0); // Dir bc
	  Dm_mat = ublas::zero_matrix<double> (cell_size, cell_size);
	  ublas::vector<double>  Dm_vec (cell_size); // Dir bc
	  Dm_vec = ublas::zero_vector<double> (cell_size);
	  ublas::vector<double> Gm (cell_size); // Dir bc
	  Gm = ublas::zero_vector<double> (cell_size);
	  ublas::vector<double> B (cell_size); // Neu bc 
	  B = ublas::zero_vector<double> (cell_size);

	  lowerboundary (geo, groundwater, active_lysimeter, h,
                         Kedge,
                         dq, Dm_mat, Dm_vec, Gm, B, msg);
	  upperboundary (geo, soil, T, surface, state, remaining_water, h,
                         Kedge,
                         dq, Dm_mat, Dm_vec, Gm, B, ddt, debug, msg, dt);
          Darcy (geo, Kedge, h, dq); //for calculating drain fluxes 


	  //Initialize water capacity  matrix
	  ublas::banded_matrix<double> Cw (cell_size, cell_size, 0, 0);
	  for (size_t c = 0; c < cell_size; c++)
	    Cw (c, c) = soil.Cw2 (c, h[c]);
	  
          std::vector<double> h_std (cell_size);
          //ublas vector -> std vector 
          std::copy(h.begin (), h.end (), h_std.begin ());

#ifdef TEST_OM_DEN_ER_BRUGT
          for (size_t cell = 0; cell != cell_size ; ++cell) 
            {				
              S_macro (cell) = (S_matrix[cell] + S_drain[cell]) 
                * geo.cell_volume (cell);
            }
#endif

	  //Initialize sum matrix
	  Solver::Matrix summat (cell_size);  
	  noalias (summat) = diff + Dm_mat;

	  //Initialize sum vector
	  ublas::vector<double> sumvec (cell_size);  
	  sumvec = grav + B + Gm + Dm_vec - S_vol
#ifdef TEST_OM_DEN_ER_BRUGT
            - S_macro
#endif
            ; 

	  // QCw is shorthand for Qmatrix * Cw
	  Solver::Matrix Q_Cw (cell_size);
	  noalias (Q_Cw) = prod (Qmat, Cw);

	  //Initialize A-matrix
	  Solver::Matrix A (cell_size);  
	  noalias (A) = (1.0 / ddt) * Q_Cw - summat;  

	  // Q_Cw_h is shorthand for Qmatrix * Cw * h
	  const ublas::vector<double> Q_Cw_h = prod (Q_Cw, h);

	  //Initialize b-vector
	  ublas::vector<double> b (cell_size);  
	  //b = sumvec + (1.0 / ddt) * (Qmatrix * Cw * h + Qmatrix *(Wxx-Wyy));
	  b = sumvec + (1.0 / ddt) * (Q_Cw_h
				      + prod (Qmat, Theta_previous-Theta));

	  // Force active drains to zero h.
          drain (geo, drain_cell, drain_water_level,
		 h, Theta_previous, Theta, S_vol,
#ifdef TEST_OM_DEN_ER_BRUGT
                 S_macro,
#endif
                 dq, ddt, drain_cell_on, A, b, debug, msg);  
          
          try {
            solver->solve (A, b, h); // Solve Ah=b with regard to h.
          } catch (const char *const error) {
              std::ostringstream tmp;
              tmp << "Could not solve equation system: " << error;
              msg.warning (tmp.str ());
              // Try smaller timestep.
              iterations_used = max_loop_iter + 100;
              break;
          }

	  for (int c=0; c < cell_size; c++) // update Theta 
	    Theta (c) = soil.Theta (c, h (c), h_ice (c)); 

	  if (debug > 1)
	    {
	      std::ostringstream tmp;
	      tmp << "Time left = " << time_left << ", ddt = " << ddt 
		  << ", iteration = " << iterations_used << "\n";
	      tmp << "B = " << B << "\n";
	      tmp << "h = " << h << "\n";
	      tmp << "Theta = " << Theta;
	      msg.message (tmp.str ());
	    }
          
          for (int c=0; c < cell_size; c++)
            {
              if (h (c) < min_pressure_potential || h (c) > max_pressure_potential)
                {
                  std::ostringstream tmp;
                  tmp << "Pressure potential out of realistic range, h[" 
                      << c << "] = " << h (c);
                  msg.debug (tmp.str ());
                  iterations_used = max_loop_iter + 100;
                  break;
                } 
            }
        }

      while (!converges (h_conv, h) && iterations_used <= max_loop_iter);
      

      if (iterations_used > max_loop_iter)
	{
          number_of_time_step_reductions++;
          
	  if (number_of_time_step_reductions > max_time_step_reductions)
            {
              msg.debug ("Could not find solution");
              throw "Could not find solution";
            }

          iterations_with_this_time_step = 0;
	  ddt /= time_step_reduction;
	  h = h_previous;
	  Theta = Theta_previous;
	}
      else
	{
          // Update dq for new h.
	  ublas::banded_matrix<double>  Dm_mat (cell_size, cell_size, 
                                                0, 0); // Dir bc
	  Dm_mat = ublas::zero_matrix<double> (cell_size, cell_size);
	  ublas::vector<double>  Dm_vec (cell_size); // Dir bc
	  Dm_vec = ublas::zero_vector<double> (cell_size);
	  ublas::vector<double> Gm (cell_size); // Dir bc
	  Gm = ublas::zero_vector<double> (cell_size);
	  ublas::vector<double> B (cell_size); // Neu bc 
	  B = ublas::zero_vector<double> (cell_size);
	  lowerboundary (geo, groundwater, active_lysimeter, h,
                         Kedge,
                         dq, Dm_mat, Dm_vec, Gm, B, msg);
	  upperboundary (geo, soil, T, surface, state, remaining_water, h,
                         Kedge,
                         dq, Dm_mat, Dm_vec, Gm, B, ddt, debug, msg, dt);
          Darcy (geo, Kedge, h, dq);

#ifdef TEST_OM_DEN_ER_BRUGT
          // update macropore flow components 
          for (int c = 0; c < cell_size; c++)
            {
              S_drain_sum[c] += S_drain[c] * ddt/dt;
              S_matrix_sum[c] += S_matrix[c] * ddt/dt;
            }
#endif

          std::vector<double> h_std_new (cell_size);
          std::copy(h.begin (), h.end (), h_std_new.begin ());

	  // Update remaining_water.
	  for (size_t i = 0; i < edge_above.size (); i++)
	    {
	      const int edge = edge_above[i];
	      const int cell = geo.edge_other (edge, Geometry::cell_above);
	      const double out_sign = (cell == geo.edge_from (edge))
		? 1.0 : -1.0;
	      remaining_water[i] += out_sign * dq (edge) * ddt;
              daisy_assert (std::isfinite (dq (edge)));
	    }

	  if (debug == 5)
	    {
	      std::ostringstream tmp;
	      tmp << "Remaining water at end: " << remaining_water;
	      msg.message (tmp.str ());
	    }

	  // Contribution to large time step.
          daisy_assert (std::isnormal (dt));
          daisy_assert (std::isnormal (ddt));
	  q += dq * ddt / dt;
          for (size_t e = 0; e < edge_size; e++)
            {
              daisy_assert (std::isfinite (dq (e)));
              daisy_assert (std::isfinite (q (e)));
            }
          for (size_t e = 0; e < edge_size; e++)
            {
              daisy_assert (std::isfinite (dq (e)));
              daisy_assert (std::isfinite (q (e)));
            }

	  time_left -= ddt;
	  iterations_with_this_time_step++;

	  if (iterations_with_this_time_step > time_step_reduction)
	    {
	      number_of_time_step_reductions--;
	      iterations_with_this_time_step = 0;
	      ddt *= time_step_reduction;
	    }
	}
      // End of small time step.
    }

  // Mass balance.
  // New = Old - S * dt + q_in * dt - q_out * dt + Error =>
  // 0 = Old - New - S * dt + q_in * dt - q_out * dt + Error
  Theta_error -= Theta;         // Old - New
  Theta_error -= S * dt;
#ifdef TEST_OM_DEN_ER_BRUGT
  for (size_t c = 0; c < cell_size; c++)
    Theta_error (c) -= (S_matrix_sum[c] + S_drain_sum[c]) * dt;
#endif
  
  for (size_t edge = 0; edge != edge_size; ++edge) 
    {
      const int from = geo.edge_from (edge);
      const int to = geo.edge_to (edge);
      const double flux = q (edge) * geo.edge_area (edge) * dt;
      if (geo.cell_is_internal (from))
        Theta_error (from) -= flux / geo.cell_volume (from);
      if (geo.cell_is_internal (to))
        Theta_error (to) += flux / geo.cell_volume (to);
    }

  // Find drain sink from mass balance.
#ifdef TEST_OM_DEN_ER_BRUGT
  std::fill(S_drain.begin (), S_drain.end (), 0.0);
#else
  std::vector<double> S_drain (cell_size);
#endif
  for (size_t i = 0; i < drain_cell.size (); i++)
    {
      const size_t cell = drain_cell[i];
      S_drain[cell] = Theta_error (cell) / dt;
      Theta_error (cell) -= S_drain[cell] * dt;
    }

  if (debug == 2)
    {
      double total_error = 0.0;
      double total_abs_error = 0.0;
      double max_error = 0.0;
      int max_cell = -1;
      for (size_t cell = 0; cell != cell_size; ++cell) 
        {
          const double volume = geo.cell_volume (cell);
          const double error = Theta_error (cell);
          total_error += volume * error;
          total_abs_error += std::fabs (volume * error);
          if (std::fabs (error) > std::fabs (max_error))
            {
              max_error = error;
              max_cell = cell;
            }
        }
      std::ostringstream tmp;
      tmp << "Total error = " << total_error << " [cm^3], abs = " 
	  << total_abs_error << " [cm^3], max = " << max_error << " [] in cell " 
	  << max_cell;
      msg.message (tmp.str ());
    }
  
  // Make it official.
  for (size_t cell = 0; cell != cell_size; ++cell) 
    soil_water.set_content (cell, h (cell), Theta (cell));
  
#ifdef TEST_OM_DEN_ER_BRUGT
  soil_water.add_tertiary_sink (S_matrix_sum);
  soil_water.drain (S_drain_sum, msg);
#endif


  for (size_t edge = 0; edge != edge_size; ++edge) 
    {
      daisy_assert (std::isfinite (q[edge]));
      soil_water.set_flux (edge, q[edge]);
    }

  soil_water.drain (S_drain, msg);

  // End of large time step.
}

void
UZRectMollerup::output (Log& log) const
{
  output_lazy (std::vector<double> (Theta_error.begin (), Theta_error.end ()),
               "Theta_error", log);
  output_lazy (std::vector<double> (Kedge.begin (), Kedge.end ()),
               "Kedge", log);
}

double 
UZRectMollerup::find_K_edge (const Soil& soil, const Geometry& geo, 
                             const size_t e,
                             const ublas::vector<double>& h, 
                             const ublas::vector<double>& h_ice, 
                             const ublas::vector<double>& h_old, 
                             const ublas::vector<double>& T) const
{
  const double anisotropy = soil.anisotropy_edge (e);
  const int from = geo.edge_from (e);
  const int to = geo.edge_to (e);

  // External edges.
  if (!geo.cell_is_internal (from))
    return soil.K (to, h (to), h_ice (to), T (to)) * anisotropy;

  if (!geo.cell_is_internal (to))
    return soil.K (from, h (from), h_ice (from), T (from)) * anisotropy;
  
  // Internal edges.
  const double K_from = soil.K (from, h (from), h_ice (from), T (from));
  const double K_to = soil.K (to, h (to), h_ice (to), T (to));
  return  K_average->average (soil, geo, e, 
                              K_from, h (from), h_ice (from), h_old (from), T (from),
                              K_to, h (to), h_ice (to), h_old (from), T (to)) * anisotropy;
}

bool
UZRectMollerup::converges (const ublas::vector<double>& previous,
			   const ublas::vector<double>& current) const
{
  size_t size = previous.size ();
  daisy_assert (current.size () == size);

  for (unsigned int i = 0; i < size; i++)
    {
      if (   fabs (current[i] - previous[i]) > max_absolute_difference
	  && (   iszero (previous[i])
              || iszero (current[i])
	      || (  fabs ((current[i] - previous[i]) / previous[i])
		  > max_relative_difference)))
	return false;
    }
  return true;
}

void 
UZRectMollerup::Neumann (const size_t edge, const size_t cell,
                         const double area, const double in_sign,
                         const double flux, 
                         ublas::vector<double>& dq, ublas::vector<double>& B)
{
  B (cell) = flux * area;
  dq (edge) = in_sign * flux;
  daisy_assert (std::isfinite (dq (edge)));
}

void 
UZRectMollerup::Dirichlet (const size_t edge, const size_t cell,
                           const double area, const double in_sign,
                           const double sin_angle,
                           const double K_cell,
                           const double h_old,
                           const double K_area_per_length, 
                           const double pressure,
                           ublas::vector<double>& dq,
                           ublas::banded_matrix<double>& Dm_mat, 
                           ublas::vector<double>& Dm_vec, 
                           ublas::vector<double>& Gm)
{
  daisy_assert (std::isnormal (area));
  daisy_assert (std::isfinite (K_area_per_length));
  daisy_assert (std::isfinite (h_old));
  daisy_assert (std::isfinite (pressure));
  daisy_assert (std::isfinite (K_cell));
  daisy_assert (std::isfinite (in_sign));

  daisy_approximate (sin_angle, 1.0);

  Dm_mat (cell, cell) += K_area_per_length;
  const double Dm_vec_val = -K_area_per_length * pressure;
  Dm_vec (cell) += Dm_vec_val; 
 
  // Entry is 1 for upper boundary, and -1 for lower boundary.
  double entry = -sin_angle * in_sign;
  const double Gm_val = entry * K_cell * area;
  Gm (cell) += Gm_val;
  dq (edge) = in_sign * (K_area_per_length * h_old 
                         + Dm_vec_val + Gm_val) / area;
  daisy_assert (std::isfinite (dq (edge)));
}


void 
UZRectMollerup::lowerboundary (const GeometryRect& geo,
			       const Groundwater& groundwater,
			       const std::vector<bool>& active_lysimeter,
			       const ublas::vector<double>& h,
			       const ublas::vector<double>& Kedge,
			       ublas::vector<double>& dq,
			       ublas::banded_matrix<double>& Dm_mat, 
			       ublas::vector<double>& Dm_vec, 
			       ublas::vector<double>& Gm, 
			       ublas::vector<double>& B, Treelog& msg)
{
  const std::vector<size_t>& edge_below = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();

  for (size_t i = 0; i < edge_below_size; i++)
    {
      const size_t edge = edge_below[i];
      const int cell = geo.edge_other (edge, Geometry::cell_below);
      daisy_assert (geo.cell_is_internal (cell));
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign > 0);
      const double area = geo.edge_area (edge);
      const double sin_angle = geo.edge_sin_angle (edge);

      switch (groundwater.bottom_type ())
        {
        case Groundwater::free_drainage:
          {
            const double sin_angle = geo.edge_sin_angle (edge);
            //const double flux = -in_sign * sin_angle * K (cell) * area; //old
            const double flux = -in_sign * sin_angle * Kedge (edge);
            Neumann (edge, cell, area, in_sign, flux, dq, B);
          }
          break;
          
        case Groundwater::forced_flux:
          {
            const double flux = groundwater.q_bottom (edge);
            Neumann (edge, cell, area, in_sign, flux, dq, B);
          }
          break;
        
        case Groundwater::pressure:
          {
            const double value = -Kedge (edge) * geo.edge_area_per_length (edge);
            const double pressure =  groundwater.table () - geo.zplus (cell);
            
            Dirichlet (edge, cell, area, in_sign, sin_angle, 
                       Kedge (edge), 
                       h (cell),
                       value, pressure,
                       dq, Dm_mat, Dm_vec, Gm);
          }
          break;

        case Groundwater::lysimeter:
          {
            if (active_lysimeter[cell])
              {
                //Neumann - not so good
                //const double flux = -in_sign * sin_angle * K (cell);
                //Neumann (edge, cell, area, in_sign, flux, dq, B);
                //Dirichlet - better
                const double value = -Kedge (edge) * geo.edge_area_per_length (edge);
                const double pressure =  0.0;
                Dirichlet (edge, cell, area, in_sign, sin_angle,
                           Kedge (edge), 
                           h (cell),
                           value, pressure, dq, Dm_mat, Dm_vec, Gm);
              }
            else
              // Indsat af pa@life.ku.dk Fri Jul 10 11:21:14     2009
              {
                const double flux = 0.0;
                Neumann (edge, cell, area, in_sign, flux, dq, B);
              }

          }
          break;
          
        default:
          daisy_panic ("Unknown groundwater type");
        }
    }
}


void 
UZRectMollerup::upperboundary (const GeometryRect& geo,
                               const Soil& soil,
                               const ublas::vector<double>& T,
			       const Surface& surface,
                               std::vector<top_state>& state,
			       const ublas::vector<double>& remaining_water,
			       const ublas::vector<double>& h,
			       const ublas::vector<double>& Kedge,
			       ublas::vector<double>& dq,
			       ublas::banded_matrix<double>& Dm_mat, 
			       ublas::vector<double>& Dm_vec, 
			       ublas::vector<double>& Gm, 
			       ublas::vector<double>& B,
			       const double ddt,
			       const int debug,
                               Treelog& msg, const double BIG_DT)
{
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      daisy_assert (geo.cell_is_internal (cell));
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign < 0);
      const double area = geo.edge_area (edge);
      const double sin_angle = geo.edge_sin_angle (edge);

      switch (surface.top_type (geo, edge))
	{
	case Surface::forced_flux: 
          {
            const double flux = -surface.q_top (geo, edge, BIG_DT);
            Neumann (edge, cell, area, in_sign, flux, dq, B);
          }
	  break;
	case Surface::forced_pressure:
          {
            const double value = -Kedge (edge) * geo.edge_area_per_length (edge);
            const double pressure = surface.h_top (geo, edge);
            Dirichlet (edge, cell, area, in_sign, sin_angle, 
                       Kedge (edge), 
                       h (cell), value, pressure, dq, Dm_mat, Dm_vec, Gm);
          }
	  break;
	case Surface::limited_water:
          {
            const double h_top = remaining_water (i);

            // We pretend that the surface is particlaly saturated.
            const double K_sat = soil.K (cell, 0.0, 0.0, T (cell));
            const double K_cell = Kedge (edge);
            const double K_edge = 0.5 * (K_cell + K_sat);
            
            const double dz = geo.edge_length (edge);
            daisy_assert (approximate (dz, -geo.cell_z (cell)));
            double q_in_avail = h_top / ddt;
            const double q_in_pot = K_edge * (h_top - h (cell) + dz) / dz;
            // Decide type.
            bool is_flux = h_top <= 0.0 || q_in_pot > q_in_avail;

            if (is_flux)
              {
                state[i] = top_flux;
                Neumann (edge, cell, area, in_sign, q_in_avail, dq, B);
              }
            else			// Pressure
              {
                state[i] = top_pressure;
                if (debug > 0 && q_in_pot < 0.0)
                  {
                    std::ostringstream tmp;
                    tmp << "q_in_pot = " << q_in_pot << ", q_avail = " 
                        << q_in_avail << ", h_top = " << h_top 
                        << ", h (cell) = " << h (cell) 
                        << " K (edge) = " << Kedge (edge) 
                        << ", K_sat = " << K_sat << ", K_edge = "
                        << K_edge <<", dz = " << dz << ", ddt = " << ddt
                        << ", is_flux = " << is_flux << "\n";
                    msg.message (tmp.str ());
                  }
                const double value = -K_edge * geo.edge_area_per_length (edge);
                const double pressure = h_top;
                Dirichlet (edge, cell, area, in_sign, sin_angle, 
                           K_edge, h (cell),
                           value, pressure, dq, Dm_mat, Dm_vec, Gm);
              }
            if (debug == 3)
              {
                std::ostringstream tmp;
                tmp << "edge = " << edge << ", K_edge = " << K_edge 
                    << ", h_top = "
                    << h_top << ", dz = " << dz << ", q_avail = " << q_in_avail
                    << ", q_pot = " << q_in_pot << ", is_flux = " << is_flux;
                msg.message (tmp.str ());
              }
          }
	  break;
	default:
	  daisy_panic ("Unknown surface type");
	}
    }
}

void 
UZRectMollerup::drain (const GeometryRect& geo,
                       const std::vector<size_t>& drain_cell,
		       const double drain_water_level,
                       const ublas::vector<double>& h,
                       const ublas::vector<double>& Theta_previous,
                       const ublas::vector<double>& Theta,
                       const ublas::vector<double>& S_vol,
#ifdef TEST_OM_DEN_ER_BRUGT
                       const ublas::vector<double>& S_macro,
#endif
                       const ublas::vector<double>& dq,
                       const double& ddt,
                       std::vector<bool>& drain_cell_on,
                       Solver::Matrix& A,
                       ublas::vector<double>& b,
                       const int debug, Treelog& msg)
{
  const size_t drain_size  = drain_cell.size (); // // number of drains   
    
  std::ostringstream tmp;

  for (size_t d = 0; d < drain_size; d++)
    {
      const size_t cell = drain_cell[d];

      // Pressure in drain cell [cm].
      const double drain_h = drain_water_level - geo.cell_z (cell); 
      
      if (drain_cell_on[d])    //drain on
        {
          //Calculate fluxes to drain from last timestep 
          
          double drain_sink = Theta_previous (cell);
          drain_sink -= Theta (cell);
          drain_sink -= ddt * (S_vol (cell)
#ifdef TEST_OM_DEN_ER_BRUGT
                               + S_macro (cell)
#endif
                               )/
            geo.cell_volume (cell);
                    
          const std::vector<size_t>& edges = geo.cell_edges (cell);
          const size_t edge_size = edges.size ();
          for (size_t i = 0; i < edge_size; i++)
            {
              const size_t edge = edges[i]; 
              const double flux = dq (edge) * geo.edge_area (edge) * ddt;
              const int from = geo.edge_from (edge);
              const int to = geo.edge_to (edge);
              
              if (cell == from)
                drain_sink  -= flux / geo.cell_volume (cell);
              else if (cell == to)
                drain_sink  += flux / geo.cell_volume (cell); 
            }
          if (drain_sink <= 0.0)
            drain_cell_on[d] = false;
        }
      else			// drain off
	if (h (cell) > 0.0)
	  drain_cell_on[d] = true;   	
          
      if (drain_h > 0.0 || drain_cell_on[d] == true)
        {
          const std::vector<size_t>& edges = geo.cell_edges (cell);
          const size_t edge_size = edges.size ();
          
          // Force pressure to be zero.
          for (size_t i = 0; i < edge_size; i++)
            {
              const size_t edge =  edges[i];
              if (!geo.edge_is_internal (edge))
                continue;
              
              const size_t other = geo.edge_other (edge, cell);
              A (cell, other) = 0.0;
            }
          A (cell, cell) = 1.0;
          b (cell) = std::max (drain_h, 0.0);
        }
    }
}




#if 0
      // Guestimate pressure in cell from surrounding cells.
      const std::vector<size_t>& edges = geo.cell_edges (cell);
      const size_t edge_size = edges.size ();
      const double z_drain = geo.cell_z (cell);      
      double h_sum = h (cell);

      for (size_t i = 0; i < edge_size; i++)
	{
	  const size_t edge =  edges[i];
	  if (!geo.edge_is_internal (edge))
	    continue;
	  const size_t other = geo.edge_other (edge, cell);
	  const double z_other = geo.cell_z (other);	// Compensate for z difference.
	  h_sum += h (other) + (z_other - z_drain);  
	}

      if (debug == 4)
	tmp << "drain[" << d << "], cell " << cell << ", has h_sum = " 
	    << h_sum << "\n";

      if (h_sum < 0)
	// Drain not active, treat as normal cell.
	continue;

      // Force pressure to be zero.
      for (size_t i = 0; i < edge_size; i++)
	{
	  const size_t edge =  edges[i];
	  if (!geo.edge_is_internal (edge))
	    continue;

	  const size_t other = geo.edge_other (edge, cell);
	  A (cell, other) = 0.0;
	}
      A (cell, cell) = 1.0;
      b (cell) = 0.0;

      const size_t cell_size = geo.cell_size ();
      for (size_t other = 0; other < cell_size; other++)
	daisy_assert (cell == other 
		      ? approximate (A (cell, other), 1.0)
		      : iszero (A (cell, other)));
    }
  if (tmp.str ().size () > 0)
    msg.message (tmp.str ());
}
#endif 
//-------End, dr-----





void 
UZRectMollerup::diffusion (const GeometryRect& geo,
			   const ublas::vector<double>& Kedge,
			   Solver::Matrix& diff)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
    
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  const double magnitude = geo.edge_area_per_length (e) * Kedge[e]; 
	  diff (from, from) -= magnitude;
	  diff (from, to) += magnitude;
	  diff (to, to) -= magnitude;
	  diff (to, from) += magnitude; 
	} 
    }
}

void 
UZRectMollerup::gravitation (const GeometryRect& geo,
			     const ublas::vector<double>& Kedge,
			     ublas::vector<double>& grav)
{
  const size_t edge_size = geo.edge_size (); // number of edges  

  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  const double magnitude = geo.edge_area (e) * Kedge[e];
	  const double sin_angle =  geo.edge_sin_angle (e);
	  const double value = magnitude * sin_angle;
	  grav[from] += value;
	  grav[to] -= value;
	} 
    }
}

void 
UZRectMollerup::Darcy (const GeometryRect& geo,
                       const ublas::vector<double>& Kedge,
                       const ublas::vector<double>& h,
                       ublas::vector<double>& dq)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
    
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
          const double length = geo.edge_length (e);
          const double sin_angle = geo.edge_sin_angle (e);
          const double K = Kedge (e);
          const double dh = h (to) - h (from);
          const double dq_diff = -(K / length) * dh;   
          const double dq_grav = -K * sin_angle; 
          daisy_assert (std::isfinite (dq_diff));
          daisy_assert (std::isfinite (dq_grav));
          dq (e) = dq_diff + dq_grav;
	} 
    }
}

void 
UZRectMollerup::initialize (const Geometry& geo, const bool /* has_macropores */)
{ 
  const size_t edge_size = geo.edge_size ();
  Kedge = ublas::zero_vector<double> (edge_size);
}

UZRectMollerup::UZRectMollerup (const BlockModel& al)
  : UZRect (al),
    solver (Librarian::build_item<Solver> (al, "solver")),
    K_average (Librarian::build_item<Condedge> (al, "K_average")),
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_iterations_timestep_reduction_factor (al.integer ("max_iterations_timestep_reduction_factor")),
    max_number_of_small_time_steps (al.integer ("max_number_of_small_time_steps")),
    msg_number_of_small_time_steps (al.integer ("msg_number_of_small_time_steps")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    max_pressure_potential (al.number ("max_pressure_potential")),
    min_pressure_potential (al.number ("min_pressure_potential")),
    use_forced_T (al.check ("forced_T")),
    forced_T (al.number ("forced_T", -42.42e42)),
    debug (al.integer ("debug"))
{ }

UZRectMollerup::~UZRectMollerup ()
{ }

static struct UZRectMollerupSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZRectMollerup (al); }
  UZRectMollerupSyntax ()
    : DeclareModel (UZRect::component, "Mollerup", "\
A finite volume solution to matrix water transport.\n\
See Mollerup 2007 for details.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("solver", Solver::component, 
                       Attribute::Const, Attribute::Singleton, "\
Model used for solving matrix equation system.");
    frame.set ("solver", "cxsparse");
    frame.declare_object ("K_average", Condedge::component,
                          Attribute::Const, Attribute::Singleton, "\
Model for calculating average vertical K between cells.");
    frame.set ("K_average", "arithmetic");
    frame.declare_integer ("max_time_step_reductions", Attribute::Const, "\
Number of times we may reduce the time step before giving up");
    frame.set ("max_time_step_reductions", 16);
    frame.declare_integer ("time_step_reduction", Attribute::Const, 
                "Divide the time step with this at each reduction.");
    frame.set ("time_step_reduction", 4);
    frame.declare_integer ("max_iterations", Attribute::Const, "\
Maximum number of iterations when seeking convergence before reducing\n\
the time step.");
    frame.set ("max_iterations", 12);
    frame.declare_integer ("max_iterations_timestep_reduction_factor",
                           Attribute::Const, "\
Multiply 'max_iterations' with this factor for each timestep reduction.");
    frame.set ("max_iterations_timestep_reduction_factor", 0);
    frame.declare_integer ("max_number_of_small_time_steps", Attribute::Const, "\
Maximum number of small time steps in a large time step.");
    frame.set ("max_number_of_small_time_steps", 20000);  
    frame.declare_integer ("msg_number_of_small_time_steps", Attribute::Const, "\
Number of small time steps in a large time step between message.");
    frame.set ("msg_number_of_small_time_steps", 100);  
    frame.declare ("max_absolute_difference", "cm", Attribute::Const, "\
Maximum absolute difference in 'h' values for convergence.");
    frame.set ("max_absolute_difference", 0.02);
    frame.declare ("max_relative_difference", Attribute::None (), Attribute::Const, "\
Maximum relative difference in 'h' values for convergence.");
    frame.set ("max_relative_difference", 0.001); 
    frame.declare ("max_pressure_potential", Attribute::None (), Attribute::Const, "\
Maximum pressure potential for convergence.");
    frame.set ("max_pressure_potential", 1e9); 
    frame.declare ("min_pressure_potential", Attribute::None (), Attribute::Const, "\
minimum pressure potential for convergence.");
    frame.set ("min_pressure_potential", -1.0e9); 
    frame.declare ("forced_T", "dg C", Attribute::OptionalConst, "\
Force transport equations to use this water temperature.");
    frame.declare_integer ("debug", Attribute::Const, "\
Level of debug messages:\n                              \
 \n                                                     \
= 0: no debug messages.\n                               \
> 0: Initial h and Theta per time step.\n\
> 1: Same, per iteration.\n              \
= 3: Upper boundary extra info.\n        \
= 4: Drain extra info.\n\
= 5: Remaining water.");
    frame.set ("debug", 0);
    frame.declare ("Theta_error",
                Attribute::None (), Attribute::LogOnly, Attribute::SoilCells, "\
Water mass balance error per cell.");
    frame.declare ("Kedge", "cm/h", Attribute::LogOnly, Attribute::SoilEdges, "\
Conductivity between cells.\n\
The value logged is the value used for the last small timestep in\n\
the previous large timestep.");
    }
} UZRectMollerup_syntax;

// uzrect_Mollerup.C ends here.
