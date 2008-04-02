// msoltranrect.C -- Matrix solute transport in rectangular grid.
// 
// Copyright 2006, 2008 Per Abrahamsen and KVL.
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

#include "msoltranrect.h"
#include "chemical.h"
#include "doe.h"
#include "geometry.h"
#include "adsorption.h"
#include "block.h"
#include "librarian.h"
#include "soil_water.h"
#include "soil.h"
#include "mobsol.h"

const char *const Msoltranrect::component = "msoltranrect";

symbol 
Msoltranrect::library_id () const
{
  static const symbol id (component);
  return id;
}

void
Msoltranrect::solute (const Geometry& geo,
                      const Soil& soil, const SoilWater& soil_water,
                      const double J_in, Chemical& solute, 
                      const bool flux_below, const double dt,
                      const Scope& scope, Treelog& msg)
{ 
  Treelog::Open nest (msg, "Msoltranrect: " + name);
  const size_t edge_size = geo.edge_size ();
  const size_t cell_size = geo.cell_size ();

  std::vector<double> C (cell_size); // Concentration given to flow.
  std::vector<double> A (cell_size); // Immobile mass not given to flow.
  std::vector<double> S (cell_size); // Source given to flow.
  std::vector<double> J (edge_size); // Flux delivered by flow.

  // Initialize edges.
  if (J_in > 0)
    msg.warning ("flux out ignored");

  for (size_t e = 0; e < edge_size; e++)
    {
      if (J_in > 0)
        J[e] = 0.0;
      else if (geo.edge_to (e) == Geometry::cell_above)
        J[e] = J_in;
      else
        J[e] = 0.0;
    }

  // Initialize cells.
  for (size_t c = 0; c < cell_size; c++)
    {
      switch (soil_water.mobile_solute_old (c))
        {
        case SoilWater::immobile:
        case SoilWater::mobile:
          // M and C immobile refer to total content for !mixed case.
          {
            const double M_total = solute.M_immobile (c);
            C[c] = solute.C_immobile (c);
            daisy_approximate (C[c], solute.C_mobile (c));
            const double Theta_total_old = soil_water.Theta_old (c);
            A[c] = M_total - C[c] * Theta_total_old;
            S[c] = solute.S_mobile (c) + solute.S_immobile (c);
          }
          break;
        case SoilWater::mixed:
          // Mixed cells may exchange matter between phases.
          {
            const double C_mobile = solute.C_mobile (c);
            const double C_immobile = solute.C_immobile (c);
            const double M_total = solute.M_total (c);
            const double M_immobile = solute.M_immobile (c);
            const double M_mobile = M_total - M_immobile;
            double S_mobilize;
            switch (soil_water.mobile_solute (c))
              {
              case SoilWater::immobile:
                // Mixed -> Immobile.
                S_mobilize = -M_mobile / dt;
                break;
              case SoilWater::mobile:
                // Mixed -> Mobile.
                daisy_notreached ();
                break;
              case SoilWater::mixed:
                // Mixed -> Mixed.
                const double alpha = soil.mobile_solute (c).alpha ();
                // alpha is really [cm^3 SPACE/cm^3 H2O/h] ...
                S_mobilize = alpha * (C_immobile - C_mobile);
                // Never convert more than what is needed for equilibrium.
                const double Theta_new = soil_water.Theta (c);
                const double C_avg_new = M_total / Theta_new;
                const double Theta_mobile = soil_water.Theta_mobile (c);
                const double Theta_immobile = soil_water.Theta_immobile (c);
                const double M_mobile_new = C_avg_new * Theta_mobile;
                const double M_immobile_new = C_avg_new * Theta_immobile;
                if (S_mobilize > 0)
                  {
                    if (M_mobile + S_mobilize * dt > M_mobile_new)
                      {
                        if (M_mobile_new > M_mobile)
                          S_mobilize = (M_mobile_new - M_mobile) / dt;
                        else
                          S_mobilize = 0.0;
                      }
                  }
                else
                  {
                    if (M_immobile - S_mobilize * dt > M_mobile_new)
                      {
                        if (M_immobile_new > M_immobile)
                          S_mobilize = -(M_immobile_new - M_immobile) / dt;
                        else
                          S_mobilize = 0.0;
                      }
                  }
                break;
              }
            const double S_immobile = solute.S_immobile (c) - S_mobilize;
            const double S_mobile = solute.S_mobile (c) + S_mobilize;
            C[c] = solute.C_mobile (c);
            A[c] = solute.M_immobile (c) - S_immobile * dt;
            S[c] = S_mobile;
          }
          break;
        }
    }
  
  // Flow.
  flow (geo, soil, soil_water, solute.name, 
        C, S, J, solute.C_below (), flux_below,
        solute.diffusion_coefficient (), 
        dt, msg);

  // Update M & C with old sorbed matter.
  for (size_t c = 0; c < cell_size; c++)
    {
      const double Theta_mobile = soil_water.Theta_mobile (c);
      const double Theta_immobile = soil_water.Theta_immobile (c);
      const SoilWater::mobile_solute_t mobile_solute_old
        = soil_water.mobile_solute_old (c);
      const SoilWater::mobile_solute_t mobile_solute 
        = soil_water.mobile_solute (c);
      
      switch (mobile_solute_old)
        {
        case SoilWater::immobile:
          switch (mobile_solute)
            {
            case SoilWater::immobile:
              // Immobile -> Immobile.
              {
                const double M_total = A[c] + C[c] * Theta_immobile;
                solute.set_uniform (soil, soil_water, c, M_total);
              }
              break;
            case SoilWater::mobile:
              // Immobile -> Mobile.
              daisy_notreached (); // Not possible.
              break;
            case SoilWater::mixed:
              // Immobile -> Mixed.
              {
                const double C_mobile = 0.0; // New water is clean.
                const double C_immobile = C[c];
                const double M_immobile = A[c] + C_immobile * Theta_immobile;
                const double M_total = M_immobile;
                solute.set_mixed (soil, soil_water, c, M_total, C_mobile);
              }
              break;
            }
          break;
        case SoilWater::mobile:
          // Mobile -> Mobile.
          {
            daisy_assert (mobile_solute == SoilWater::mobile);
            const double M_total = A[c] + C[c] * Theta_mobile;
            solute.set_uniform (soil, soil_water, c, M_total);
          }
          break;
        case SoilWater::mixed:
          switch (mobile_solute)
            {
            case SoilWater::immobile:
              // Mixed -> Immobile.
              {
                daisy_assert (iszero (C[c]));
                const double M_total = A[c];
                solute.set_uniform (soil, soil_water, c, M_total);
              }
            case SoilWater::mobile:
              // Mixed -> Mobile.
              daisy_notreached (); // Not possible.
              break;
            case SoilWater::mixed:
              // Mixed -> Mixed.
              {
                const double C_mobile = C[c];
                const double M_mobile = C_mobile * Theta_mobile;
                const double M_total = A[c] + M_mobile;
                solute.set_mixed (soil, soil_water, c, M_total, C_mobile);
              }
              break;
            }
          break;
        }
    }

  // Update edges.
  for (size_t e = 0; e < edge_size; e++)
    solute.set_matrix_flux (e, J[e]);
}

void 
Msoltranrect::element (const Geometry& geo, 
                       const Soil& soil, const SoilWater& soil_water,
                       DOE& element, const double diffusion_coefficient, 
                       const double dt, Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  element.tick (cell_size, soil_water, dt);
  static const symbol DOM_name ("DOM");
  flow (geo, soil, soil_water, DOM_name, 
        element.C, element.S, element.J, 0.0, false,
        diffusion_coefficient, dt, msg);
  for (size_t c = 0; c < cell_size; c++)
    element.M[c] = element.C[c] * soil_water.Theta (c);
}

bool 
Msoltranrect::check (const Geometry&, Treelog&)
{ return true; }

Msoltranrect::Msoltranrect (Block& al)
  : ModelAListed (al.alist ())
{ }

Msoltranrect::~Msoltranrect ()
{ }

static Librarian Msoltranrect_init (Msoltranrect::component, "\
Matrix solute transport in rectangular grid.");

// msoltranrect.C ends here
