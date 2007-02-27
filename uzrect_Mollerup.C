// uzrect_Mollerup.C --- A 2D solution to Richard's equation in a rect. grid.
// 
// Copyright 2006 Mikkel Mollerup and KVL.
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

#include "uzrect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "groundwater.h"
#include "surface.h"
#include "syntax.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "assertion.h"
#include <sstream>
#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace ublas = boost::numeric::ublas;

struct UZRectMollerup : public UZRect
{
  // Parameters.  
  const int max_time_step_reductions;
  const int time_step_reduction;
  const int max_iterations; 
  const double max_absolute_difference;
  const double max_relative_difference;  



  // Interface.
  void tick (const GeometryRect&, const Soil&, SoilWater&, const SoilHeat&, 
             const Surface&, const Groundwater&, double dt, Treelog&);

  // Internal functions.
  bool converges (const ublas::vector<double>& previous,
		  const ublas::vector<double>& current) const;
  static void lowerboundary (const GeometryRect& geo,
			     const Groundwater&,
			     const std::vector<bool>& active_lysimeter,
			     const ublas::vector<double>& K,
			     ublas::matrix<double>& Dm_mat, 
			     ublas::vector<double>& Dm_vec, 
			     ublas::vector<double>& Gm, 
			     ublas::vector<double>& B);
  static void upperboundary (const GeometryRect& geo,
			     const Surface& surface,
			     const ublas::vector<double>& remaining_water,
			     const ublas::vector<double>& K,
			     ublas::matrix<double>& Dm_mat, 
			     ublas::vector<double>& Dm_vec, 
			     ublas::vector<double>& Gm, 
			     ublas::vector<double>& B,
			     const double dt);
  static void diffusion (const GeometryRect& geo,
			 const ublas::vector<double>& Kedge,
			 ublas::matrix<double>& diff);
  static void gravitation (const GeometryRect& geo,
			   const ublas::vector<double>& Kedge,
			   ublas::vector<double>& grav);  



  // Create and Destroy.
  void has_macropores (bool);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UZRectMollerup (Block& al);
  ~UZRectMollerup ();
};



void 
UZRectMollerup::tick (const GeometryRect& geo, const Soil& soil, 
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
  ublas::vector<double> T (cell_size); // temperature 
  ublas::vector<double> K (cell_size); // hydraulic conductivity
  ublas::vector<double> Kedge (edge_size); // edge (inter cell) conductivity
  ublas::vector<double> h_lysimeter (cell_size);
  std::vector<bool> active_lysimeter (cell_size);
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  ublas::vector<double> remaining_water (edge_above_size);
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      remaining_water (edge) = surface.h_top (geo, edge);
    }

  //Make Qmat area diagonal matrix 
  ublas::banded_matrix<double> Qmat (cell_size, cell_size, 0, 0);
  for (int c = 0; c < cell_size; c++)
    Qmat (c, c) = geo.cell_volume (c);
 
  // make vectors 
  for (size_t cell = 0; cell != cell_size ; ++cell) 
    {				
      Theta[cell] = soil_water.Theta (cell);
      h[cell] =  soil_water.h (cell);
      h_ice[cell] = soil_water.h_ice (cell);
      S[cell] =  soil_water.S_sum (cell);
      T[cell] = soil_heat.T (cell); 
      h_lysimeter[cell] = geo.zplus (cell) - geo.z (cell);
    }

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

      std::ostringstream tmp;
      tmp << "Time left = " << time_left << ", ddt = " << ddt 
	  << ", iteration = " << iterations_used << "\n";
      tmp << "h = " << h << "\n";
      tmp << "Theta = " << Theta << "\n";
      msg.message (tmp.str ());

      h_previous = h;
      Theta_previous = Theta;  
      ublas::vector<double> h_conv;

      for (size_t cell = 0; cell != cell_size ; ++cell)
	active_lysimeter[cell] = h (cell) > h_lysimeter (cell);
      
      do // Start iteration loop
	{
	  h_conv = h;
	  iterations_used++;

	  // Calculate conductivity
	  for (size_t cell = 0; cell !=cell_size ; ++cell) 
	    K[cell] =  soil.K (cell, h[cell], h_ice[cell], T[cell]); 
	    
	  for (size_t e = 0; e < edge_size; e++)
	    {
	      if (geo.edge_is_internal (e))
		{
		  const int from = geo.edge_from (e);
		  const int to = geo.edge_to (e);	   
		  Kedge[e] = 2.0/(1.0/K[from] + 1.0/K[to]); 
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
	  ublas::matrix<double>  Dm_mat (cell_size, cell_size); // Dir bc
	  Dm_mat = ublas::zero_matrix<double> (cell_size, cell_size);
	  ublas::vector<double>  Dm_vec (cell_size); // Dir bc
	  Dm_vec = ublas::zero_vector<double> (cell_size);
	  ublas::vector<double> Gm (cell_size); // Dir bc
	  Gm = ublas::zero_vector<double> (cell_size);
	  ublas::vector<double> B (cell_size); // Neu bc 
	  B = ublas::zero_vector<double> (cell_size);
	  lowerboundary (geo, groundwater, active_lysimeter, 
			 K, Dm_mat, Dm_vec, Gm, B);
	  upperboundary (geo, surface, remaining_water, 
			 K, Dm_mat, Dm_vec, Gm, B, dt);

	  //Initialize water capacity  matrix
	  ublas::banded_matrix<double> Cw (cell_size, cell_size, 0, 0);
	  for (size_t c = 0; c < cell_size; c++)
	    Cw (c, c) = soil.Cw2 (c, h[c]);
	  
	  //Initialize sum matrix
	  ublas::matrix<double> summat (cell_size, cell_size);  
	  summat = diff + Dm_mat;

	  //Initialize sum vector
	  ublas::vector<double> sumvec (cell_size);  
	  sumvec = grav + B + Gm + Dm_vec; 

	  //Initialize A-matrix
	  ublas::matrix<double> A (cell_size, cell_size);  
	  A = (1.0 / ddt) * prod (Qmat, Cw) - summat;  

	  //Initialize b-vector
	  ublas::vector<double> b (cell_size);  
	  //b = sumvec + (1.0 / ddt) * (Qmatrix * Cw * h + Qmatrix *(Wxx-Wyy));
	  b = sumvec + (1.0 / ddt) * (prod (prod (Qmat, Cw),  h) 
				      + prod (Qmat, Theta_previous-Theta));


	  // Any drain ?

	  // Solve Ax=b (maybe)
	  ublas::permutation_matrix<double> piv (cell_size);
	  const bool singular = ublas::lu_factorize(A, piv);
	  daisy_assert (!singular);
	  ublas::lu_substitute (A, piv, b); // b now contains solution 
	  
	  h = b; // new solution :-)
	  
	  for (int c=0; c < cell_size; c++) // update Theta - maybe not neccessary???
	    Theta (c) = soil.Theta (c, h (c), h_ice (c)); 

	  std::ostringstream tmp;
	  tmp << "Time left = " << time_left << ", ddt = " << ddt 
	      << ", iteration = " << iterations_used << "\n";
	  tmp << "B = " << B << "\n";
	  tmp << "h = " << h << "\n";
	  tmp << "Theta = " << Theta << "\n";
	  msg.message (tmp.str ());

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

  // Make it official.
  for (size_t cell = 0; cell != cell_size ; ++cell) 
    soil_water.set_content (cell, h[cell], Theta[cell]);
  for (size_t edge = 0; edge != edge_size ; ++edge) 
    soil_water.set_flux (edge, 0.0);

  // End of large time step.
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
UZRectMollerup::lowerboundary (const GeometryRect& geo,
			       const Groundwater& groundwater,
			       const std::vector<bool>& active_lysimeter,
			       const ublas::vector<double>& K,
			       ublas::matrix<double>& Dm_mat, 
			       ublas::vector<double>& Dm_vec, 
			       ublas::vector<double>& Gm, 
			       ublas::vector<double>& B)
{
  const std::vector<int>& edge_below = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();

  switch (groundwater.bottom_type ())
    {
    case Groundwater::free_drainage:
      for (size_t i = 0; i < edge_below_size; i++)
	{
	  const int edge = edge_below[i];
	  const int cell = geo.edge_other (edge, Geometry::cell_below);
	  B (cell) = - K (cell) * geo.edge_area (edge);
	}
      break;
    case Groundwater::forced_flux:
      for (size_t i = 0; i < edge_below_size; i++)
	{
	  const int edge = edge_below[i];
	  const int cell = geo.edge_other (edge, Geometry::cell_below);
	  B (cell) = groundwater.q_bottom () * geo.edge_area (edge);
	}
      break;
    case Groundwater::pressure:
     for (size_t i = 0; i < edge_below_size; i++)
       {
	 const int edge = edge_below[i];
	 const int cell = geo.edge_other (edge, Geometry::cell_below);
	 const double value = -K (cell) * geo.edge_area_per_length (edge);
	 Dm_mat (cell, cell) += value;
	 const double h_bottom =  groundwater.table () - geo.zplus (cell);
	 Dm_vec (cell) -= value * h_bottom;
	 Gm (cell) -= K (cell) * geo.edge_area (edge); 
       }
      break;
    case Groundwater::lysimeter:
      for (size_t i = 0; i < edge_below_size; i++)
	{
	  const int edge = edge_below[i];
	  const int cell = geo.edge_other (edge, Geometry::cell_below);
	  if (active_lysimeter[cell])
	    {
	      const double value = -K (cell) * geo.edge_area_per_length (edge);
	      Dm_mat (cell, cell) += value;
	      // const double h_bottom =  0.0;
	      // Dm_vec (cell) -= value * h_bottom;
	      Gm (cell) -= K (cell) * geo.edge_area (edge); 
	    }
	}
      break;
    default:
      daisy_panic ("Unknown groundwater type");
    }
}

void 
UZRectMollerup::upperboundary (const GeometryRect& geo,
			       const Surface& surface,
			       const ublas::vector<double>& remaining_water,
			       const ublas::vector<double>& K,
			       ublas::matrix<double>& Dm_mat, 
			       ublas::vector<double>& Dm_vec, 
			       ublas::vector<double>& Gm, 
			       ublas::vector<double>& B,
			       const double ddt)
{
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);

      switch (surface.top_type (geo, edge))
	{
	case Surface::forced_flux: 
	  const double q = surface.q_top (geo, edge);
	  B (cell) = - q * geo.edge_area (edge);
	  break;
	case Surface::forced_pressure:
	  const double value = -K (cell) * geo.edge_area_per_length (edge);
	  Dm_mat (cell, cell) += value;
	  Dm_vec (cell) -= value *  surface.h_top (geo, edge, dt);
	  Gm (cell) += K (cell) * geo.edge_area (edge); 
	  break;
	case Surface::limited_water:
	  const double h_top = remaining_water (i);
	  const double dz = geo.length (edge);
	  const double K = K (cell);
	  const double h = h (cell);
	  // Postive upwards.
	  const double q_avail = -remaining_water / dt;
	  daisy_assert (q_avail <= 0.0);
	  const double q_pot = -K * (h_top - h + dz) / dz;
	  // Decide type.
	  const bool is_flux = -q_pot > -q_avail;
	  if (is_flux)
	    B (cell) = - q_avail * geo.edge_area (edge);
	  else			// Pressure
	    {
	      B (cell) = - q_pot * geo.edge_area (edge);
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
UZRectMollerup::has_macropores (const bool)
{ /* Ignore for now. */ }

void 
UZRectMollerup::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
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
}

UZRectMollerup::UZRectMollerup (Block& al)
  : UZRect (al),
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference"))
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
  static UZRect& make (Block& al)
  { return *new UZRectMollerup (al); }
  UZRectMollerupSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
A finite volume solution to matrix water transport.\n\
See Mollerup, 2007 for details.");
    UZRectMollerup::load_syntax (syntax, alist);
    Librarian<UZRect>::add_type ("Mollerup", alist, syntax, &make);
  }
} UZRectMollerup_syntax;

// uzrect_Mollerup.C ends here.
