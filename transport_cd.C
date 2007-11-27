// transport_cd.C --- Using convection-dispersion.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "transport.h"
#include "block.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

struct TransportCD : public Transport
{
  // Parameters.
  const int max_time_step_reductions;
  
  // Simulation.
  void tick (Treelog&, const Geometry1D& geo,
             const Soil&, const SoilWater&, 
	     double diffusion_coefficient,
	     std::vector<double>& M, 
	     std::vector<double>& C,
	     const std::vector<double>& S,
	     std::vector<double>& J, double C_below, double dt);

  // Create.
  TransportCD (Block& al)
    : Transport (al),
      max_time_step_reductions (al.integer ("max_time_step_reductions"))
  { }
  static void load_syntax (Syntax& syntax, AttributeList& alist);
};

void 
TransportCD::tick (Treelog&, const Geometry1D& geo,
                   const Soil& soil, const SoilWater& soil_water,
		   const double diffusion_coefficient,
		   std::vector<double>& M, 
		   std::vector<double>& C,
		   const std::vector<double>& S,
		   std::vector<double>& J, const double C_below,
		   const double dt)
{
  double J_in = J[0];

  // Remember old values.
  const std::vector<double> C_prev = C;
  const std::vector<double> M_prev = M;

  // Constants.
  const size_t size = geo.cell_size (); // Number of soil layers.

  // Check that incomming C and M makes sense.
  for (unsigned int i = 0; i < size; i++)
    {
      daisy_assert (C[i] >= 0.0);
      daisy_assert (M[i] >= 0.0);
      if (iszero (C[i]))
	daisy_assert (iszero (M[i]));
      else 
        daisy_assert (approximate (M[i], soil_water.Theta_old (i) * C[i]));
    }

  // Note: q, D, and alpha depth indexes are all [j-½].

  // Dispersion coefficient [cm²/s]
  std::vector<double> D (size + 1);

  for (unsigned int j = 1; j < size; j++)
    {
      // Dispersion length [cm]
      const double lambda = soil.dispersivity (j);

      // Water flux [cm³ /cm² / h]
      const double q = soil_water.q (j);
      
      // Theta middled in time and space.
      const double Theta = 
	(soil_water.Theta (j) + soil_water.Theta (j-1)
	 + soil_water.Theta_old (j) + soil_water.Theta_old (j-1)) / 4.0;
      // From equation 7-39:
      D[j] = (lambda * fabs (-q / Theta)
	      + soil.tortuosity_factor (j, Theta)
	      * diffusion_coefficient)
	* Theta;

      // Check for NaN.
      daisy_assert (std::isfinite (D[j]));
    }
  // Lower boundary.
  {
    // Dispersion length [cm]
    const double lambda = soil.dispersivity (size-1);

    // Water flux [cm³ /cm² / h]
    const double q = soil_water.q (size);
      
    // Theta middled in time and space.
    const double Theta = 
      (soil_water.Theta (size - 1) + soil_water.Theta_old (size  - 1)) / 2.0;
    // From equation 7-39:
    D[size] = (lambda * fabs (-q / Theta)
	       + soil.tortuosity_factor (size-1, Theta) 
 	       * diffusion_coefficient)
      * Theta;

  }
  // Upper boundary (no dispersion over soil surface).
  D[0] = 0.0;

  // Weight factor (how important is this flux for the concentration)
  // This is 1 for incomming flux and 0 for outgoing flux.
  std::vector<double> alpha (size + 1);

  for (unsigned int j = 0; j < size + 1; j++)
    {
      if (soil_water.q (j) < 0.0)
	alpha[j] = 1.0;
      else
	alpha[j] = 0.0;
    }

  const double dz_top = 0 - geo.z (0);
  // Or: - 2.0 * geo.z (0)
  // Or: geo.z (0) - geo.z(1)

  double C_top = 0.0;
  double S_top = 0.0;
#ifdef DISABLE_MIXING
  daisy_assert (J_in <= 0.0);
#endif
  if (std::isnormal (J_in))
    {
#ifdef DISABLE_MIXING
      daisy_assert (J_in < 0.0);
#endif
      if (soil_water.q (0) < 0.0)
	// Normal condition, stuff is in solute.
	if (J_in < 0.0)
	  C_top = J_in / soil_water.q (0);
	else
	  {
	    S_top = -J_in / geo.dz (0);
	    J_in = 0.0;
	  }
      else
	{
	  // This should only happen if Surface::total_matter_flux.
	  S_top = -J_in / geo.dz (0);
	  J_in = 0.0;
	}
    }

  // Find the time step using Courant.
  double ddt = dt;
  for (unsigned int i = 0; i < size; i++)
    ddt = std::min (ddt, std::pow (geo.dz (i), 2) / (2 * D[i + 1]));
  int time_step_reductions = 0;

  // We restart from here if anything goes wrong.
 try_again:;

  // Loop through small time steps.
  for (double old_t = 0.0, t = ddt; 
       old_t < t;
       old_t = t, t = std::min (dt, t + ddt))
    {
      // Parameters for tridiagonal matrix.
      std::vector<double> a (size);
      std::vector<double> b (size);
      std::vector<double> c (size);
      std::vector<double> d (size);
  
      // Water content at start and end of small timestep.
      std::vector<double> Theta_old (size);
      std::vector<double> Theta_new (size);
      for (unsigned int j = 0; j < size; j++)
	{
	  const double Theta_ratio 
	    = (soil_water.Theta (j) - soil_water.Theta_old (j)) / dt;
	  Theta_new[j] = soil_water.Theta_old (j) + Theta_ratio * t;
	  Theta_old[j] = soil_water.Theta_old (j) + Theta_ratio * old_t;
	}

      for (unsigned int j = 1; j < size; j++)
	{
	  const double dz_minus	// Size of layer above current cell.
	    = geo.z (j-1) - geo.z (j);
	  const double dz_plus	// Size of layer below current cell.
	    = (j == size - 1) ? dz_minus : (geo.z (j) - geo.z (j+1));

	  const double dz = geo.dz (j); // Size of current cell.
	  double q_minus = soil_water.q (j); // Flow to above.
	  const double q_plus = soil_water.q (j+1);	// Flow from below.
	  const double alpha_minus = alpha[j]; // Direction above.
	  const double alpha_plus = alpha[j+1]; // Direction below.
	  double D_minus = D[j]; // Dispertion above.
	  const double D_plus = D[j+1]; // Dispertion below.

	  // Concentration above and below current cell.
	  const double C_minus = C[j-1];
	  const double C_plus = (j == size - 1) ? 
	    (C_below < 0.0 ? C[j] : C_below) : C[j+1];

	  a[j] = - D_minus / (2.0 * dz_minus * dz) 
	    + (alpha_minus * q_minus) / (2.0 * dz);
	  b[j] = (Theta_new[j] / (t - old_t)
		  + D_minus / (2.0 * dz_minus * dz)
		  + D_plus / (2.0 * dz_plus * dz)
		  + ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		  - (alpha_plus * q_plus) / (2.0 * dz));
	  c[j] = - D_plus / (2.0 * dz_plus * dz)
	    - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	  d[j] = (Theta_old[j] * C[j] / (t - old_t)
		  + S[j]
		  + ((D_minus * (C_minus - C[j])) / (2.0 * dz_minus * dz))
		  - ((D_plus * (C[j] - C_plus)) / (2.0 * dz_plus * dz))
		  - (q_minus * (alpha_minus * C_minus
				+ (1.0 - alpha_minus) * C[j])
		     / (2.0 * dz))
		  + (q_plus * (alpha_plus * C[j] + (1.0 - alpha_plus) * C_plus)
		     / (2.0 * dz)));

	  // Check for NaN.
	  daisy_assert (std::isfinite (a[j]));
	  daisy_assert (std::isfinite (b[j]));
	  daisy_assert (std::isfinite (c[j]));
	  daisy_assert (std::isfinite (d[j]));
	}
      // Adjust for upper boundary condition.
      {
	// Size of layer above current cell.
	const double dz_minus = dz_top;
	// Size of layer below current cell.
	const double dz_plus = geo.z (0) - geo.z (1);

	// Size of current cell.
	const double dz = geo.dz (0);
	// Flow to above.
	double q_minus = std::isnormal (J_in) ? soil_water.q (0) : 0.0;
	// Flow from below.
	const double q_plus = soil_water.q (1);
	const double alpha_minus = alpha[0]; // Direction above.
	const double alpha_plus = alpha[1]; // Direction below.
	double D_minus = D[0]; // Dispertion above.
	const double D_plus = D[1]; // Dispertion below.

	// Concentration above and below current cell.
	const double C_minus = C_top;
	const double C_plus = C[1];

	a[0] = - D_minus / (2.0 * dz_minus * dz) 
	  + (alpha_minus * q_minus) / (2.0 * dz);
	b[0] = (Theta_new[0] / (t - old_t)
		+ D_minus / (2.0 * dz_minus * dz)
		+ D_plus / (2.0 * dz_plus * dz)
		+ ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		- (alpha_plus * q_plus) / (2.0 * dz));
	c[0] = - D_plus / (2.0 * dz_plus * dz)
	  - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	d[0] = (Theta_old[0] * C[0] / (t - old_t)
		+ S[0] + S_top
		+ ((D_minus * (C_minus - C[0])) / (2.0 * dz_minus * dz))
		- ((D_plus * (C[0] - C_plus)) / (2.0 * dz_plus * dz))
		- (q_minus * (alpha_minus * C_minus
			      + (1.0 - alpha_minus) * C[0])
		   / (2.0 * dz))
		+ (q_plus * (alpha_plus * C[0] + (1.0 - alpha_plus) * C_plus)
		   / (2.0 * dz)));

	// Check for NaN.
	daisy_assert (std::isfinite (a[0]));
	daisy_assert (std::isfinite (b[0]));
	daisy_assert (std::isfinite (c[0]));
	daisy_assert (std::isfinite (d[0]));
	d[0] -= a[0] * C_top;
      }
      // Adjust for lower boundary condition.
      b[size - 1] += c[size - 1];
      c[size - 1] = -42.42e42;

      // Calculate new concentration.
      tridia (0, size, a, b, c, d, C.begin ());

      // Check solution.
      for (unsigned int j = 0; j < size; j++)
	if (C[j] < 0.0)
	  {
	    ddt *= 0.5;
	    C = C_prev;
	    M = M_prev;
	    time_step_reductions++;
	    if (time_step_reductions > max_time_step_reductions)
	      throw ("convection-dispersion gave negative solution");
	    goto try_again;
	  }

      // Update M and C.
      for (size_t j = 0; j < size; j++)
	{
	  // We use the old absorbed stuff plus the new dissolved stuff.
 	  M[j] = Theta_new[j] * C[j];
	  daisy_assert (M[j] >= 0.0);
	}
    }

  // Calculate flux with mass conservation.
  if (size + 1 > J.size ())
    J.insert (J.begin (), size + 1 - J.size (), 0.0);

  J[0] = J_in - S_top * geo.dz (0);
  for (unsigned int i = 0; i < size; i++)
    {
      daisy_assert (M[i] >= 0.0);
      J[i + 1] = (((M[i] - M_prev[i]) / dt) - S[i]) * geo.dz (i) + J[i];
    }
}

const AttributeList& 
Transport::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      TransportCD::load_syntax (dummy, alist);
      alist.add ("type", "cd");
    }
  return alist;
}

void 
TransportCD::load_syntax (Syntax& syntax, AttributeList& alist)
{
    syntax.add ("max_time_step_reductions",
		Syntax::Integer, Syntax::Const, "\
Number of times we may reduce the time step before giving up");
    alist.add ("max_time_step_reductions", 20);
}

static struct TransportCDSyntax
{
  static Model& make (Block& al)
  {
    return *new TransportCD (al);
  }

  TransportCDSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Solute transport using convection-dispersion.");
    TransportCD::load_syntax (syntax, alist);
    Librarian::add_type (Transport::component, "cd", alist, syntax, &make);
  }
} TransportCD_syntax;
