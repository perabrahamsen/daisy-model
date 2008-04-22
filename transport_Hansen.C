// transport_Hansen.C --- Using convection-dispersion.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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
#include "adsorption.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

struct TransportHansen : public Transport
{
  // Simulation.
  void flow (const Geometry& geo, 
             const Soil& soil, 
             const std::vector<double>& Theta_old,
             const std::vector<double>& Theta_new,
             const std::vector<double>& q,
             symbol name,
             const std::vector<double>& S, 
             const std::map<size_t, double>& J_forced,
             const std::map<size_t, double>& C_border,
             std::vector<double>& C, 
             std::vector<double>& J, 
             double diffusion_coefficient, double dt,
             Treelog& msg) const;

  // Create.
  // Create.
  bool check (const Geometry&, Treelog&) const;
  TransportHansen (Block& al)
    : Transport (al)
  { }
  static void load_syntax (Syntax& syntax, AttributeList& alist);
};

void 
TransportHansen::flow (const Geometry& geo_base, 
                           const Soil& soil, 
                           const std::vector<double>& Theta_begin,
                           const std::vector<double>& Theta_end,
                           const std::vector<double>& q_primary,
                           symbol name,
                           const std::vector<double>& S, 
                           const std::map<size_t, double>& J_forced,
                           const std::map<size_t, double>& C_border,
                           std::vector<double>& C, 
                           std::vector<double>& J, 
                           double diffusion_coefficient, double dt,
                           Treelog& msg) const
{
  const Geometry1D& geo = dynamic_cast<const Geometry1D&> (geo_base);

  // Constants.
  const size_t size = geo.cell_size (); // Number of soil layers.

  // Border conditions
  daisy_assert (J_forced.size () == 1);
  daisy_assert (J_forced.begin ()->first == 0);
  double J_in = J_forced.begin ()->second;
  daisy_assert (C_border.size () < 2);
  daisy_assert (C_border.size () == 0 || C_border.begin ()->first == size);
  const double C_below = (C_border.begin () == C_border.end ())
    ? -42.42e42
    : C_border.begin ()->second;

  // Remember old values.
  const std::vector<double> C_prev = C;
  std::vector<double> M_prev (size);
  for (size_t c = 0; c < size; c++)
    M_prev[c] = C[c] * Theta_begin[c];

  // Dispersion coefficient [cm²/s]
  std::vector<double> D (size + 1);

  for (unsigned int j = 1; j < size; j++)
    {
      // Dispersion length [cm]
      const double lambda = soil.dispersivity (j);

      // Water flux [cm³ /cm² / h]
      const double q = q_primary[j];
      
      // Theta middled in time and space.
      const double Theta = (Theta_begin[j] + Theta_begin[j-1]
                            + Theta_end[j] + Theta_end[j-1]) / 4.0;
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
    const double q = q_primary[size];
      
    // Theta middled in time and space.
    const double Theta = (Theta_end[size - 1] + Theta_begin[size  - 1]) / 2.0;
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
      if (q_primary[j] < 0.0)
	alpha[j] = 1.0;
      else
	alpha[j] = 0.0;
    }

  const double dz_top = 0 - geo.z (0);
  // Or: - 2.0 * geo.z (0)
  // Or: geo.z (0) - geo.z(1)

  double C_top = 0.0;
  double S_top = 0.0;

  if (std::isnormal (J_in))
    {
      if (q_primary[0] < 0.0)
	// Normal condition, stuff is in solute.
	if (J_in < 0.0)
	  C_top = J_in / q_primary[0];
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
  
      // Old absorbed matter.
      std::vector<double> A (size);
      // Water content at start and end of small timestep.
      std::vector<double> Theta_old (size);
      std::vector<double> Theta_new (size);
      for (unsigned int j = 0; j < size; j++)
	{
	  const double Theta_ratio = (Theta_end[j] - Theta_begin[j]) / dt;
	  Theta_new[j] = Theta_begin[j] + Theta_ratio * t;
	  Theta_old[j] = Theta_begin[j] + Theta_ratio * old_t;
	}

      for (unsigned int j = 1; j < size; j++)
	{
	  const double dz_minus	// Size of layer above current cell.
	    = geo.z (j-1) - geo.z (j);
	  const double dz_plus	// Size of layer below current cell.
	    = (j == size - 1) ? dz_minus : (geo.z (j) - geo.z (j+1));

	  const double dz = geo.dz (j); // Size of current cell.
	  double q_minus = q_primary[j]; // Flow to above.
	  const double q_plus = q_primary[j+1];	// Flow from below.
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
	double q_minus = std::isnormal (J_in) ? q_primary[0] : 0.0;
	// Flow from below.
	const double q_plus = q_primary[1];
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
    }

  // Calculate flux with mass conservation.
  std::vector<double> M (size);
  for (size_t c = 0; c < size; c++)
    M[c] = C[c] * Theta_end[c];

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
Transport::vertical_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      TransportHansen::load_syntax (dummy, alist);
      alist.add ("type", "Hansen");
    }
  return alist;
}

bool 
TransportHansen::check (const Geometry& geo, Treelog& msg) const
{
  bool ok = true;

  if (!dynamic_cast<const Geometry1D*> (&geo))
    {
      msg.error ("\
This primary solute transport model only works with 'vertical' movement");
      ok = false;
    }
  return ok;
}

void 
TransportHansen::load_syntax (Syntax& syntax, AttributeList& alist)
{ }

static struct TransportHansenSyntax
{
  static Model& make (Block& al)
  {
    return *new TransportHansen (al);
  }

  TransportHansenSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Solute transport using convection-dispersion.");
    TransportHansen::load_syntax (syntax, alist);
    Librarian::add_type (Transport::component, "Hansen", 
                         alist, syntax, &make);
  }
} TransportHansen_syntax;
