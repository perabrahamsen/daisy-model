// movement_solute.C --- Geometry independent solute movement.
// 
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
#include "movement_solute.h"
#include "geometry.h"
#include "soil_water.h"
#include "msoltranrect.h"
#include "chemical.h"
#include "adsorption.h"
#include "alist.h"
#include "librarian.h"

void 
MovementSolute::secondary_flow (const Geometry& geo, 
                              const std::vector<double>& Theta_old,
                              const std::vector<double>& Theta_new,
                              const std::vector<double>& q,
                              const symbol name,
                              std::vector<double>& M, 
                              const std::vector<double>& S, 
                              std::vector<double>& J_sum, 
                              const double C_below,
                              const double dt,
                              Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  double time_left = dt;

  // Keep upper boundary constant during simulation.
  const std::vector<double> J = J_sum;
  fill (J_sum.begin (), J_sum.end (), 0.0);
  
  // Initial water content.
  std::vector<double> Theta (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    Theta[c] = Theta_old[c];

  // Small timesteps.
  for (;;)
    {
      // Are we done yet?
      const double min_timestep_factor = 0.001;
      if (time_left < 0.1 * min_timestep_factor * dt)
        break;

      // Find new timestep.
      double ddt = time_left;
  
      // Limit timestep based on water flux.
      for (size_t e = 0; e < edge_size; e++)
        {
          const int cell = (q[e] > 0.0 ? geo.edge_to (e) : geo.edge_from (e));
          if (geo.cell_is_internal (cell))
            {
              const double loss_rate = std::fabs (q[e]) * geo.edge_area (e);
              const double content = Theta[cell] * geo.cell_volume (cell); 
              const double time_to_empty = content / loss_rate;
              if (time_to_empty < min_timestep_factor * dt)
                // Unreasonable small time step.  Give up.
                continue;
              
              // Go down in timestep while it takes less than two to empty cell.
              while (time_to_empty < 2.0 * ddt)
                ddt *= 0.5;
            }
        }

      // Cell source.
      for (size_t c = 0; c < cell_size; c++)
        M[c] += S[c] * ddt;

      // Find fluxes using new values (more stable).
      std::vector<double> dJ (edge_size);
      for (size_t e = 0; e < edge_size; e++)
        {
          const int edge_from = geo.edge_from (e);
          const int edge_to = geo.edge_to (e);
          const bool in_flux = q[e] > 0.0;
          const int flux_from = in_flux ? edge_from : edge_to;
          double C_flux_from;
          switch (flux_from)
            {
            case Geometry::cell_above:
            case Geometry::cell_left:
            case Geometry::cell_right:
            case Geometry::cell_front:
            case Geometry::cell_back:
              dJ[e] = J[e];
              continue;
            case Geometry::cell_below:
              C_flux_from = C_below;
              break;
            default:
              daisy_assert (geo.cell_is_internal (flux_from));
              if (geo.edge_other (e, flux_from) == Geometry::cell_above)
                // No flux upwards.
                continue;
              if (Theta[flux_from] > 1e-6 && M[flux_from] > 0.0)
                // Positive content in positive water.
                C_flux_from = M[flux_from] / Theta[flux_from];
              else
                // You can't cut the hair of a bald guy.
                C_flux_from = 0.0;
            }

          // Convection.
          dJ[e] = q[e] * C_flux_from;
        }

      // Update values for fluxes.
      for (size_t e = 0; e < edge_size; e++)
        {
          const double value = ddt * dJ[e] * geo.edge_area (e);

          const int from = geo.edge_from (e);
          if (geo.cell_is_internal (from))
            M[from] -= value / geo.cell_volume (from);

          const int to = geo.edge_to (e);
          if (geo.cell_is_internal (to))
            M[to] += value / geo.cell_volume (to);

          J_sum[e] += dJ[e] * ddt;
        }

      // Update time left.
      time_left -= ddt;

      // Interpolate Theta.
      for (size_t c = 0; c < cell_size; c++)
        Theta[c] = time_left * Theta_old[c] + (1.0 - time_left) * Theta_new[c];
    }
}

void
MovementSolute::secondary_transport (const Geometry& geo,
                                   const Soil& soil,
                                   const SoilWater& soil_water,
                                   const double J_above, Chemical& solute, 
                                   std::vector<double>& S_extra,
                                   const bool flux_below, const double dt,
                                   const Scope& scope, Treelog& msg)
{ 
  
  // Edges.
  const size_t edge_size = geo.edge_size ();

  std::vector<double> q (edge_size); // Water flux [cm].
  std::vector<double> J (edge_size); // Flux delivered by flow.

  if (J_above > 0)
    msg.warning ("flux out ignored");

  for (size_t e = 0; e < edge_size; e++)
    {
      q[e] = soil_water.q_secondary (e);

      if (J_above > 0)
        J[e] = 0.0;             // Wrong direction.
      else if (q[e] > 0.0)
        J[e] = 0.0;             // Handled by primary flow.
      else if (geo.edge_to (e) == Geometry::cell_above)
        J[e] = J_above;         // Top edge.
      else
        J[e] = 0.0;             // Not a top edge.
    }

  // Cells.
  const size_t cell_size = geo.cell_size ();

  std::vector<double> Theta_old (cell_size); // Water content at start...
  std::vector<double> Theta_new (cell_size); // ...and end of timestep.
  std::vector<double> M (cell_size); // Content given to flow.
  std::vector<double> S (cell_size); // Source given to flow.

  for (size_t c = 0; c < cell_size; c++)
    {
      Theta_old[c] = soil_water.Theta_secondary_old (c);
      Theta_new[c] = soil_water.Theta_secondary (c);
      const double source = solute.S_secondary (c);
      M[c] = solute.C_secondary (c) * Theta_old[c];
      if (Theta_new[c] > 0)
        {
          if (Theta_old[c] > 0)
            // Secondary water fully active.
            S[c] = source;
          else if (source > 0.0)
            // Fresh water and source.
            S[c] = source;
          else
            // Fresh water and sink.
            S[c] = 0.0;
        }
      else
        // No secondary water at end of timestep.
        S[c] = 0.0;
      
      // Put any remaining source in S_extra.
      S_extra[c] += source - S[c];
    }
  
  // Flow.
  secondary_flow (geo, Theta_old, Theta_new, q, solute.name, 
                  M, S, J, solute.C_below (), dt, msg);

  // Negative content should be handled by primary transport.
  std::vector<double> C (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    if (Theta_new[c] > 1e-6 &&  M[c] > 0.0)
      // Positive mass in positive water.
      C[c] = M[c] / Theta_new[c];
    else
      // Otherwise, pass to primary transport.
      {
        S_extra[c] += M[c] / dt;
        C[c] = 0.0;
      }
  solute.set_secondary (soil, soil_water, C, J);
}

void
MovementSolute::primary_transport (const Geometry& geo,
                                 const Soil& soil, const SoilWater& soil_water,
                                 const Msoltranrect& transport,
                                 const double J_above, Chemical& solute, 
                                 const std::vector<double>& S_extra,
                                 const bool flux_below, const double dt,
                                 const Scope& scope, Treelog& msg)
{ 
  
  // Edges.
  const size_t edge_size = geo.edge_size ();

  std::vector<double> q (edge_size); // Water flux [cm].
  std::vector<double> J (edge_size); // Flux delivered by flow.

  if (J_above > 0)
    msg.warning ("flux out ignored");

  for (size_t e = 0; e < edge_size; e++)
    {
      q[e] = soil_water.q_primary (e);

      if (J_above > 0)
        J[e] = 0.0;             // Wrong direction.
      else if (soil_water.q_secondary (e) < 0.0)
        J[e] = 0.0;             // Handled by secondary transport.
      else if (geo.edge_to (e) == Geometry::cell_above)
        J[e] = J_above;         // Top edge.
      else
        J[e] = 0.0;             // Not a top edge.
    }

  // Cells.
  const size_t cell_size = geo.cell_size ();

  std::vector<double> Theta_old (cell_size); // Water content at start...
  std::vector<double> Theta_new (cell_size); // ...and end of timestep.
  std::vector<double> C (cell_size); // Concentration given to flow.
  std::vector<double> A (cell_size); // Sorbed mass not given to flow.
  std::vector<double> S (cell_size); // Source given to flow.

  for (size_t c = 0; c < cell_size; c++)
    {
      Theta_old[c] = soil_water.Theta_primary_old (c);
      Theta_new[c] = soil_water.Theta_primary (c);
      C[c] = solute.C_primary (c);
      const double M = solute.M_primary (c);
      A[c] = M - C[c] * Theta_old[c];
      S[c] = solute.S_primary (c) + S_extra[c];
    }
  
  // Flow.
  transport.flow (geo, soil, Theta_old, Theta_new, q, solute.name, 
                  C, S, J, solute.C_below (), flux_below,
                  solute.diffusion_coefficient (), 
                  dt, msg);

  // Update with new content.
  std::vector<double> M (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    M[c] = A[c] + C[c] * Theta_new[c];

  solute.set_primary (soil, soil_water, M, J);
}

void
MovementSolute::solute (const Soil& soil, const SoilWater& soil_water,
                        const double J_above, Chemical& chemical, 
                        const bool flux_below, 
                        const double dt,
                        const Scope& scope, Treelog& msg)
{
  const size_t cell_size = geometry ().cell_size ();
  std::vector<double> S_extra (cell_size, 0.0);

  // Fully adsorbed.
  if (chemical.adsorption ().full ())
    {
      Treelog::Open nest (msg, "solid " + matrix_solid->library_id ());
      for (size_t c = 0; c < cell_size; c++)
        S_extra[c] = chemical.S_secondary (c);
      primary_transport (geometry (), soil, soil_water, *matrix_solid, J_above,
                         chemical, S_extra, flux_below, dt, scope, msg);
      return;
    }

  // Secondary transport activated.
  secondary_transport (geometry (), soil, soil_water, J_above, 
                       chemical, S_extra, flux_below, dt, scope, msg);

  // Primary transport.
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, "solute", i, matrix_solute[i]->library_id ());
      try
        {
          primary_transport (geometry (), soil, soil_water, 
                             *matrix_solute[i], J_above,
                             chemical, S_extra, flux_below, dt, scope, msg);
          if (i > 0)
            msg.message ("Succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("Solute problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("Solute trouble: ") + error);
        }
    }
  throw "Matrix solute transport failed";
}

void 
MovementSolute::element (const Soil& soil, const SoilWater& soil_water,
                       DOE& element, 
                       const double diffusion_coefficient, double dt, 
                       Treelog& msg)
{
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, "element", i, matrix_solute[i]->library_id ());
      try
        {
          matrix_solute[i]->element (geometry (), soil, soil_water, element, 
                                     diffusion_coefficient, dt, 
                                     msg);
          if (i > 0)
            msg.message ("Succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("DOM problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("DOM trouble: ") + error);
        }
    }
  throw "Matrix element transport failed";
}

bool
MovementSolute::check_solute (Treelog& msg) const
{
  bool ok = true; 
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, 
                          "matrix_solute", i, matrix_solute[i]->library_id ());
      if (!matrix_solute[i]->check (geometry (), msg))
        ok = false;
    }
  return ok;
}

MovementSolute::MovementSolute (Block& al)
  : Movement (al),
    matrix_solute (Librarian::build_vector<Msoltranrect> 
                   (al, "matrix_solute")),
    matrix_solid (Librarian::build_item<Msoltranrect>
		  (al, "matrix_solid"))
{ }


void
MovementSolute::load_solute (Syntax& syntax, AttributeList& alist)
{
    syntax.add_object ("matrix_solute", Msoltranrect::component, 
                       Syntax::State, Syntax::Sequence,
                       "Matrix solute transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    std::vector<const AttributeList*> matrix_solute_models;
#if 0 // Need to distinguish between 1D & 2D
    AttributeList matrix_solute_default (Msoltranrect::default_model ());
    matrix_solute_models.push_back (&matrix_solute_default);
#endif
    AttributeList matrix_solute_reserve (Msoltranrect::reserve_model ());
    matrix_solute_models.push_back (&matrix_solute_reserve);
    AttributeList matrix_solute_none (Msoltranrect::none_model ());
    matrix_solute_models.push_back (&matrix_solute_none);
    alist.add ("matrix_solute", matrix_solute_models);

    syntax.add_object ("matrix_solid", Msoltranrect::component, 
                       Syntax::Const, Syntax::Singleton, "\
Matrix solute transport model used for fully sorbed constituents.");
    alist.add ("matrix_solid", Msoltranrect::none_model ());
}

// movement_solute.C ends here.

