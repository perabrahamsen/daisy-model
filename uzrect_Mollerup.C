// uzrect_Mollerup.C --- A 2D solution to Richard's equation in a rect. grid.
// 
// Copyright 2006, 2007 Mikkel Mollerup, Per Abrahamsen and KVL.
//
// This file is part of Daisy.
// 
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

#define BUILD_DLL

#include "uzrect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "groundwater.h"
#include "surface.h"
#include "log.h"
#include "syntax.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "assertion.h"
#include "librarian.h"

// Uncomment for fast code that does not catches bugs.
#define BOOST_UBLAS_NDEBUG

#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>
// #include <boost/numeric/ublas/matrix_sparse.hpp>

#include <sstream>

namespace ublas = boost::numeric::ublas;

struct UZRectMollerup : public UZRect
{
  // Types.
  enum top_state { top_undecided, top_flux, top_pressure };

  // Parameters.  
  const double edge_arithmetic_height;
  const int max_time_step_reductions;
  const int time_step_reduction;
  const int max_iterations; 
  const double max_absolute_difference;
  const double max_relative_difference;  
  const bool use_forced_T;
  const double forced_T;
  const int debug;

  // Log variable.
  ublas::vector<double> Theta_error;

  // Interface.
  void tick (const GeometryRect&, std::vector<size_t>& drain_cell,
	     const Soil&, SoilWater&, const SoilHeat&, 
             const Surface&, const Groundwater&, double dt, Treelog&);
  void output (Log&) const;
  
  // Internal functions.
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
			     ublas::vector<double>& B);
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
                             Treelog& msg);
  static void drain (const GeometryRect& geo,
		     const std::vector<size_t>& drain_cell,
		     const ublas::vector<double>& h,
		     ublas::matrix<double>& A,
		     ublas::vector<double>& b, 
		     const int debug, Treelog& msg);
  static void diffusion (const GeometryRect& geo,
			 const ublas::vector<double>& Kedge,
			 ublas::matrix<double>& diff);
  static void gravitation (const GeometryRect& geo,
			   const ublas::vector<double>& Kedge,
			   ublas::vector<double>& grav);  
  static void Darcy (const GeometryRect& geo,
                     const ublas::vector<double>& Kedge,
                     const ublas::vector<double>& h,
                     ublas::vector<double>& dq);


  // Create and Destroy.
  void has_macropores (bool);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UZRectMollerup (Block& al);
  ~UZRectMollerup ();
};

static double anisotropy_factor (const Geometry& geo, size_t edge, 
				 const Soil& soil, size_t cell)
{
  const double sin_angle = geo.edge_sin_angle (edge);
  const double cos_angle = geo.edge_cos_angle (edge);
  const double factor = soil.anisotropy (cell);

  return sqrt (sqr (sin_angle) + sqr (factor * cos_angle));
}

void 
UZRectMollerup::tick (const GeometryRect& geo, std::vector<size_t>& drain_cell,
		      const Soil& soil, 
                      SoilWater& soil_water, const SoilHeat& soil_heat,
                      const Surface& surface, const Groundwater& groundwater,
                      const double dt,
                      Treelog& msg)

{
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
  ublas::vector<double> T (cell_size); // temperature 
  ublas::vector<double> K (cell_size); // hydraulic conductivity
  ublas::vector<double> Kold (cell_size); // old hydraulic conductivity
  ublas::vector<double> Ksum (cell_size); // Hansen hydraulic conductivity
  ublas::vector<double> Kedge (edge_size); // edge (inter cell) conductivity
  ublas::vector<double> h_lysimeter (cell_size);
  std::vector<bool> active_lysimeter (cell_size);
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  ublas::vector<double> remaining_water (edge_above_size);
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      remaining_water (i) = surface.h_top (geo, edge);
    }
  ublas::vector<double> q;	// Accumulated flux
  q = ublas::zero_vector<double> (edge_size);
  ublas::vector<double> dq (edge_size); // Flux in small timestep.

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
      h_lysimeter (cell) = geo.zplus (cell) - geo.z (cell);
    }

  // Remember old value.
  Theta_error = Theta;

  // Start time loop 
  double time_left = dt;	// How much of the large time step left.
  double ddt = dt;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;

  while (time_left > 0.0)
    {
      // Initialization for each small time step.
      int iterations_used = 0;
      if (ddt > time_left)
	ddt = time_left;

      if (debug > 0)
	{
	  std::ostringstream tmp;
	  tmp << "Time left = " << time_left << ", ddt = " << ddt 
	      << ", iteration = " << iterations_used << "\n";
	  tmp << "h = " << h << "\n";
	  tmp << "Theta = " << Theta << "\n";
	  msg.message (tmp.str ());
	}

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
        {
          active_lysimeter[cell] = h (cell) > h_lysimeter (cell);
          Kold [cell] = soil.K (cell, h (cell), h_ice (cell), T (cell));
          Ksum [cell] = 0.0;
        }

      std::vector<top_state> state (edge_above.size (), top_undecided);

      do // Start iteration loop
	{
	  h_conv = h;
	  iterations_used++;

	  // Calculate conductivity - The Hansen method
	  for (size_t cell = 0; cell !=cell_size ; ++cell)
            {  
              Ksum[cell] += soil.K (cell, h (cell), h_ice (cell), T (cell));
              K[cell] = (Ksum[cell] / iterations_used + Kold[cell]) / 2.0;
	    }

	  for (size_t e = 0; e < edge_size; e++)
	    {
	      if (geo.edge_is_internal (e))
		{
		  const int from = geo.edge_from (e);
		  const int to = geo.edge_to (e);	   

		  const double K_from 
		    = K[from] * anisotropy_factor (geo, e, soil, from);
		  const double K_to
		    = K[to] * anisotropy_factor (geo, e, soil, to);

                  // We have to use arithmetic average near the top of
                  // the soil, otherwise we risk development an water
                  // resistent crust to appear dut to soil
                  // evaporation.  In Danish soil heterogeneity will
                  // allow water to wet up the soil anyway.
                  const bool top_edge 
                    = geo.edge_center_z (e) > edge_arithmetic_height;

                  if (top_edge)
                    Kedge[e] = (K_from + K_to) / 2.0; 
                  else
                    // Hormonic average is more correct.
                    Kedge[e] = 2.0/(1.0/K_from + 1.0/K_to);
		} 
	    }
	  
	  //Initialize diffusive matrix
	  ublas::matrix<double> diff (cell_size, cell_size);
	  diff = ublas::zero_matrix<double> (cell_size, cell_size);
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
			 K, dq, Dm_mat, Dm_vec, Gm, B);
	  upperboundary (geo, soil, T, surface, state, remaining_water, h,
			 K, dq, Dm_mat, Dm_vec, Gm, B, ddt, debug, msg);

	  //Initialize water capacity  matrix
	  ublas::banded_matrix<double> Cw (cell_size, cell_size, 0, 0);
	  for (size_t c = 0; c < cell_size; c++)
	    Cw (c, c) = soil.Cw2 (c, h[c]);
	  
	  //Initialize sum matrix
	  ublas::matrix<double> summat (cell_size, cell_size);  
	  summat = diff + Dm_mat;

	  //Initialize sum vector
	  ublas::vector<double> sumvec (cell_size);  
	  sumvec = grav + B + Gm + Dm_vec - S_vol; 

	  //Initialize A-matrix
	  ublas::matrix<double> A (cell_size, cell_size);  
	  A = (1.0 / ddt) * prod (Qmat, Cw) - summat;  

	  //Initialize b-vector
	  ublas::vector<double> b (cell_size);  
	  //b = sumvec + (1.0 / ddt) * (Qmatrix * Cw * h + Qmatrix *(Wxx-Wyy));
	  b = sumvec + (1.0 / ddt) * (prod (prod (Qmat, Cw),  h) 
				      + prod (Qmat, Theta_previous-Theta));


	  // Force active drains to zero h.
	  drain (geo, drain_cell, h, A, b, debug, msg);

	  // Solve Ax=b (maybe)
	  ublas::permutation_matrix<double> piv (cell_size);
	  const bool singular = ublas::lu_factorize(A, piv);
	  daisy_assert (!singular);
	  ublas::lu_substitute (A, piv, b); // b now contains solution 
	  
	  h = b; // new solution :-)
	  
	  for (int c=0; c < cell_size; c++) // update Theta - not neccessary???
	    Theta (c) = soil.Theta (c, h (c), h_ice (c)); 

	  if (debug > 1)
	    {
	      std::ostringstream tmp;
	      tmp << "Time left = " << time_left << ", ddt = " << ddt 
		  << ", iteration = " << iterations_used << "\n";
	      tmp << "B = " << B << "\n";
	      tmp << "h = " << h << "\n";
	      tmp << "Theta = " << Theta << "\n";
	      msg.message (tmp.str ());
	    }
	}
      while (!converges (h_conv, h)
	     && iterations_used <= max_iterations);

      if (iterations_used > max_iterations)
	{
	  number_of_time_step_reductions++;

	  if (number_of_time_step_reductions > max_time_step_reductions)
	    throw "Could not find solution";

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
			 K, dq, Dm_mat, Dm_vec, Gm, B);
	  upperboundary (geo, soil, T, surface, state, remaining_water, h,
			 K, dq, Dm_mat, Dm_vec, Gm, B, ddt, debug, msg);
          Darcy (geo, Kedge, h, dq);

	  // Update remaining_water.
	  for (size_t i = 0; i < edge_above.size (); i++)
	    {
	      const int edge = edge_above[i];
	      const int cell = geo.edge_other (edge, Geometry::cell_above);
	      const double out_sign = (cell == geo.edge_from (edge))
		? 1.0 : -1.0;
	      remaining_water[i] += out_sign * dq (edge) * ddt;
	    }

	  if (debug == 5)
	    {
	      std::ostringstream tmp;
	      tmp << "Remaining water at end: " << remaining_water;
	      msg.message (tmp.str ());
	    }

	  // Contribution to large time step.
	  q += dq * ddt / dt;

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
  
  // New = Old - S * dt + q_in * dt - q_out * dt + Error =>
  // 0 = Old - New - S * dt + q_in * dt - q_out * dt + Error
  Theta_error -= Theta;         // Old - New
  Theta_error -= S * dt;
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
  if (debug == 2)
    {
      std::ostringstream tmp;
      tmp << "Total error = " << total_error << " [cm^3], abs = " 
	  << total_abs_error << " [cm^3], max = " << max_error << " [] in cell " 
	  << max_cell;
      msg.message (tmp.str ());
    }
  
  // Make it official.
  for (size_t cell = 0; cell != cell_size; ++cell) 
    soil_water.set_content (cell, h (cell), Theta (cell));
  for (size_t edge = 0; edge != edge_size; ++edge) 
    soil_water.set_flux (edge, q[edge]);

  // End of large time step.
}

void
UZRectMollerup::output (Log& log) const
{
  output_variable ("Theta_error", log);
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
  Dm_mat (cell, cell) += K_area_per_length;
  const double Dm_vec_val = -K_area_per_length * pressure;
  Dm_vec (cell) += Dm_vec_val;
  // Entry is 1 for upper boundary, and -1 for lower boundary.
  double entry = -sin_angle * in_sign;
  const double Gm_val = entry * K_cell * area;
  Gm (cell) += Gm_val;
  //dq (edge) = in_sign * (K_area_per_length * h_old 
  //                       + Dm_vec_val + Gm_val) / area;
  dq (edge) = in_sign * (K_area_per_length * h_old 
                      + Dm_vec_val + Gm_val) / area;

}

void 
UZRectMollerup::lowerboundary (const GeometryRect& geo,
			       const Groundwater& groundwater,
			       const std::vector<bool>& active_lysimeter,
			       const ublas::vector<double>& h,
			       const ublas::vector<double>& K,
			       ublas::vector<double>& dq,
			       ublas::banded_matrix<double>& Dm_mat, 
			       ublas::vector<double>& Dm_vec, 
			       ublas::vector<double>& Gm, 
			       ublas::vector<double>& B)
{
  const std::vector<int>& edge_below = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();

  for (size_t i = 0; i < edge_below_size; i++)
    {
      const int edge = edge_below[i];
      const int cell = geo.edge_other (edge, Geometry::cell_below);
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
            const double flux = -in_sign * sin_angle * K (cell) * area;
            Neumann (edge, cell, area, in_sign, flux, dq, B);
          }
          break;
        case Groundwater::forced_flux:
          {
            const double flux = groundwater.q_bottom () * area;
            Neumann (edge, cell, area, in_sign, flux, dq, B);
          }
          break;
        case Groundwater::pressure:
          {
            const double value = -K (cell) * geo.edge_area_per_length (edge);
            const double pressure =  groundwater.table () - geo.zplus (cell);
            Dirichlet (edge, cell, area, in_sign, sin_angle, 
                       K (cell), h (cell),
                       value, pressure,
                       dq, Dm_mat, Dm_vec, Gm);
          }
          break;
        case Groundwater::lysimeter:
          if (active_lysimeter[cell])
            {
              const double value = -K (cell) * geo.edge_area_per_length (edge);
              const double pressure =  0.0;
              Dirichlet (edge, cell, area, in_sign, sin_angle,
                         K (cell), h (cell),
                         value, pressure, dq, Dm_mat, Dm_vec, Gm);
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
			       const ublas::vector<double>& K,
			       ublas::vector<double>& dq,
			       ublas::banded_matrix<double>& Dm_mat, 
			       ublas::vector<double>& Dm_vec, 
			       ublas::vector<double>& Gm, 
			       ublas::vector<double>& B,
			       const double ddt,
			       const int debug,
                               Treelog& msg)
{
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign < 0);
      const double area = geo.edge_area (edge);
      const double sin_angle = geo.edge_sin_angle (edge);

      switch (surface.top_type (geo, edge))
	{
	case Surface::forced_flux: 
          {
            const double flux = -surface.q_top (geo, edge);
            Neumann (edge, cell, area, in_sign, flux, dq, B);
          }
	  break;
	case Surface::forced_pressure:
          {
            const double value = -K (cell) * geo.edge_area_per_length (edge);
            const double pressure = surface.h_top (geo, edge);
            Dirichlet (edge, cell, area, in_sign, sin_angle, K (cell),
                       h (cell), value, pressure, dq, Dm_mat, Dm_vec, Gm);
          }
	  break;
	case Surface::limited_water:
          {
            const double h_top = remaining_water (i);

            // We pretend that the surface is particlaly saturated.
            const double K_sat = soil.K (cell, 0.0, 0.0, T (cell));
            const double K_cell = K (cell);
            const double K_edge = K_cell;
            const double dz = geo.edge_length (edge);
            daisy_assert (approximate (dz, -geo.z (cell)));
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
                        << ", h (cell) = " << h (cell) << " K (cell) = " 
                        << K (cell) << ", K_sat = " << K_sat << ", K_edge = "
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
	case Surface::soil:
	  throw "Don't know how to handle this surface type";
	default:
	  daisy_panic ("Unknown surface type");
	}
    }
}

void 
UZRectMollerup::drain (const GeometryRect& geo,
		       const std::vector<size_t>& drain_cell,
		       const ublas::vector<double>& h,
		       ublas::matrix<double>& A,
		       ublas::vector<double>& b,
		       const int debug, Treelog& msg)
{
  const size_t drain_size  = drain_cell.size (); // // number of drains   
    
  std::ostringstream tmp;

  for (size_t d = 0; d < drain_size; d++)
    {
      const size_t cell = drain_cell[d];

      // Guestimate pressure in cell from surrounding cells.
      const std::vector<int>& edges = geo.cell_edges (cell);
      const size_t edge_size = edges.size ();
      const double z_drain = geo.z (cell);      
      double h_sum = h (cell);

      for (size_t i = 0; i < edge_size; i++)
	{
	  const size_t edge =  edges[i];
	  if (!geo.edge_is_internal (edge))
	    continue;
	  const size_t other = geo.edge_other (edge, cell);
	  const double z_other = geo.z (other);	// Compensate for z difference.
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









void 
UZRectMollerup::diffusion (const GeometryRect& geo,
			   const ublas::vector<double>& Kedge,
			   ublas::matrix<double>& diff)
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
          dq (e) = dq_diff + dq_grav;
	} 
    }
}

void 
UZRectMollerup::has_macropores (const bool)
{ /* Ignore for now. */ }

void 
UZRectMollerup::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("edge_arithmetic_height", "cm", Syntax::Const, "\
\n\
We have to use arithmetic average near the top of the soil, otherwise\n\
we risk development an water resistent crust to appear dut to soil\n\
evaporation.  In Danish soil heterogeneity will allow water to wet up\n\
the soil anyway.");
  alist.add ("edge_arithmetic_height", -15.0);

  syntax.add ("max_time_step_reductions",
              Syntax::Integer, Syntax::Const, "\
Number of times we may reduce the time step before giving up");
  alist.add ("max_time_step_reductions", 4);
  syntax.add ("time_step_reduction", Syntax::Integer, Syntax::Const, 
              "Divide the time step with this at each reduction.");
  alist.add ("time_step_reduction", 4);
  syntax.add ("max_iterations", Syntax::Integer, Syntax::Const, "\
Maximum number of iterations when seeking convergence before reducing\n\
the time step.");
  alist.add ("max_iterations", 12);
  syntax.add ("max_absolute_difference", "cm", Syntax::Const, "\
Maximum absolute difference in 'h' values for convergence.");
  alist.add ("max_absolute_difference", 0.02);
  syntax.add ("max_relative_difference", Syntax::None (), Syntax::Const, "\
Maximum relative difference in 'h' values for convergence.");
  alist.add ("max_relative_difference", 0.001); 
  syntax.add ("forced_T", "dg C", Syntax::OptionalConst, "\
Force transport equations to use this water temperature.");
  syntax.add ("debug", Syntax::Integer, Syntax::Const, "\
Level of debug messages:\n\
 \n\
= 0: no debug messages.\n\
> 0: Initial h and Theta per time step.\n\
> 1: Same, per iteration.\n\
= 3: Upper boundary extra info.\n\
= 4: Drain extra info.\n\
= 5: Remaining water.");
  alist.add ("debug", 0);
  syntax.add ("Theta_error",
              Syntax::None (), Syntax::LogOnly, Syntax::Sequence, "\
Water mass balance error per cell.");
}

UZRectMollerup::UZRectMollerup (Block& al)
  : UZRect (al),
    edge_arithmetic_height (al.number ("edge_arithmetic_height")),
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    use_forced_T (al.check ("forced_T")),
    forced_T (al.number ("forced_T", -42.42e42)),
    debug (al.integer ("debug"))
{ }

UZRectMollerup::~UZRectMollerup ()
{ }

const AttributeList& 
UZRect::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      UZRectMollerup::load_syntax (dummy, alist);
      alist.add ("type", "Mollerup");

    }
  return alist;
}

static struct UZRectMollerupSyntax
{
  static Model& make (Block& al)
  { return *new UZRectMollerup (al); }
  UZRectMollerupSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
A finite volume solution to matrix water transport.\n\
See Mollerup 2007 for details.");
    UZRectMollerup::load_syntax (syntax, alist);
    Librarian::add_type (UZRect::component, "Mollerup", alist, syntax, &make);
  }
} UZRectMollerup_syntax;

// uzrect_Mollerup.C ends here.
