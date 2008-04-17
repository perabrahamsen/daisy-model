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
#include "transport.h"
#include "chemical.h"
#include "adsorption.h"
#include "alist.h"
#include "librarian.h"
#include "block.h"

void 
MovementSolute::secondary_flow (const Geometry& geo, 
                                const std::vector<double>& Theta_old,
                                const std::vector<double>& Theta_new,
                                const std::vector<double>& q,
                                const symbol name,
                                const std::vector<double>& S, 
                                const std::map<size_t, double>& J_forced,
                                const std::map<size_t, double>& C_border,
                                std::vector<double>& M, 
                                std::vector<double>& J, 
                                const double dt,
                                Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  
  // Full timstep left.
  daisy_assert (dt > 0.0);
  double time_left = dt;

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
      std::vector<double> dJ (edge_size, -42.42e42);
      for (size_t e = 0; e < edge_size; e++)
        {
          std::map<size_t, double>::const_iterator i = J_forced.find (e);
          if (i != J_forced.end ())
            // Forced flux.
            {
              dJ[e] = (*i).second;
              daisy_assert (std::isfinite (dJ[e]));
              continue;
            }

          const int edge_from = geo.edge_from (e);
          const int edge_to = geo.edge_to (e);
          const bool in_flux = q[e] > 0.0;
          int flux_from = in_flux ? edge_from : edge_to;
          double C_flux_from = -42.42e42;

          if (geo.cell_is_internal (flux_from))
            // Internal cell, use its concentration.
            {
              if (Theta[flux_from] > 1e-6 && M[flux_from] > 0.0)
                // Positive content in positive water.
                C_flux_from = M[flux_from] / Theta[flux_from];
              else
                // You can't cut the hair of a bald guy.
                C_flux_from = 0.0;
            }
          else
            {
              i = C_border.find (e);
              if (i != C_border.end ())
                // Specified by C_border.
                C_flux_from = (*i).second;
              else
                // Assume no gradient.
                {
                  const int flux_to = in_flux ? edge_to : edge_from;
                  daisy_assert (geo.cell_is_internal (flux_to));
                  if (Theta[flux_to] > 1e-6 && M[flux_to] > 0.0)
                    // Positive content in positive water.
                    C_flux_from = M[flux_to] / Theta[flux_to];
                  else
                    // You can't cut the hair of a bald guy.
                    C_flux_from = 0.0;
                }
            }

          // Convection.
          daisy_assert (std::isfinite (q[e]));
          daisy_assert (C_flux_from >= 0.0);
          dJ[e] = q[e] * C_flux_from;
          daisy_assert (std::isfinite (dJ[e]));
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

          J[e] += dJ[e] * ddt / dt;
        }

      // Update time left.
      time_left -= ddt;

      // Interpolate Theta.
      for (size_t c = 0; c < cell_size; c++)
        {
          const double left = time_left / dt;
          const double done = 1.0 - left;
          Theta[c] = left * Theta_old[c] + done * Theta_new[c];
        }
    }
}

void
MovementSolute::secondary_transport (const Geometry& geo,
                                     const Soil& soil,
                                     const SoilWater& soil_water,
                                     const std::map<size_t, double>& J_forced,
                                     const std::map<size_t, double>& C_border,
                                     Chemical& solute, 
                                     std::vector<double>& S_extra,
                                     const double dt,
                                     const Scope& scope, Treelog& msg)
{ 
  // Edges.
  const size_t edge_size = geo.edge_size ();

  std::vector<double> q (edge_size); // Water flux [cm].
  std::vector<double> J (edge_size, 0.0); // Flux delivered by flow.

    for (size_t e = 0; e < edge_size; e++)
    {
      q[e] = soil_water.q_secondary (e);
      daisy_assert (std::isfinite (q[e]));
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
      daisy_assert (Theta_old[c] >= 0.0);
      Theta_new[c] = soil_water.Theta_secondary (c);
      daisy_assert (Theta_new[c] >= 0.0);
      const double source = solute.S_secondary (c);
      daisy_assert (std::isfinite (source));
      M[c] = solute.C_secondary (c) * Theta_old[c];
      daisy_assert (M[c] >= 0.0);
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
      daisy_assert (std::isfinite (S_extra[c]));
    }
  
  // Flow.
  secondary_flow (geo, Theta_old, Theta_new, q, solute.name, 
                  S, J_forced, C_border, M, J, dt, msg);

  // Check fluxes.
  for (size_t e = 0; e < edge_size; e++)
    daisy_assert (std::isfinite (J[e]));

  // Negative content should be handled by primary transport.
  std::vector<double> C (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      daisy_assert (std::isfinite (M[c]));
      if (Theta_new[c] > 1e-6 &&  M[c] > 0.0)
        // Positive mass in positive water.
        C[c] = M[c] / Theta_new[c];
      else
        // Otherwise, pass to primary transport.
        {
          S_extra[c] += M[c] / dt;
          C[c] = 0.0;
        }
      daisy_assert (std::isfinite (S_extra[c]));
    }
  solute.set_secondary (soil, soil_water, C, J);
}

void
MovementSolute::primary_transport (const Geometry& geo, const Soil& soil,
                                   const SoilWater& soil_water,
                                   const Transport& transport,
                                   const std::map<size_t, double>& J_forced,
                                   const std::map<size_t, double>& C_border,
                                   Chemical& solute, 
                                   const std::vector<double>& S_extra,
                                   const double dt,
                                   const Scope& scope, Treelog& msg)
{ 
  
  // Edges.
  const size_t edge_size = geo.edge_size ();

  std::vector<double> q (edge_size); // Water flux [cm].
  std::vector<double> J (edge_size); // Flux delivered by flow.

  for (size_t e = 0; e < edge_size; e++)
    {
      q[e] = soil_water.q_primary (e);
      daisy_assert (std::isfinite (q[e]));
      J[e] = 0.0;
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
      daisy_assert (Theta_old[c] > 0.0);
      Theta_new[c] = soil_water.Theta_primary (c);
      daisy_assert (Theta_new[c] > 0.0);
      C[c] = solute.C_primary (c);
      daisy_assert (C[c] >= 0.0);
      const double M = solute.M_primary (c);
      daisy_assert (M >= 0.0);
      A[c] = M - C[c] * Theta_old[c];
      daisy_assert (std::isfinite (A[c]));
      if (A[c] < 0.0)
        {
          daisy_approximate (M,  C[c] * Theta_old[c]);
          A[c] = 0.0;
        }
      daisy_assert (A[c] >= 0.0);
      S[c] = solute.S_primary (c) + S_extra[c];
      daisy_assert (std::isfinite (S[c]));
    }
  
  // Flow.
  transport.flow (geo, soil, Theta_old, Theta_new, q, solute.name, 
                  S, J_forced, C_border, C, J, 
                  solute.diffusion_coefficient (), 
                  dt, msg);

  // Check fluxes.
  for (size_t e = 0; e < edge_size; e++)
    daisy_assert (std::isfinite (J[e]));


  // Update with new content.
  std::vector<double> M (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      daisy_assert (std::isfinite (C[c]));
      M[c] = A[c] + C[c] * Theta_new[c];
    }

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

  // Source term transfered from secondary to primary domain.
  std::vector<double> S_extra (cell_size, 0.0);

  // Divide incomming flux according to water.
  daisy_assert (J_above <= 0.0);
  std::map<size_t, double> J_tertiary;
  std::map<size_t, double> J_secondary; 
  std::map<size_t, double> J_primary;

  const std::vector<size_t>& edge_above 
    = geometry ().cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  double total_water_in = 0.0;

  // Find incomming water in all domain.
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const int cell = geometry ().edge_other (edge, Geometry::cell_above);
      daisy_assert (geometry ().cell_is_internal (cell));
      const double area = geometry ().edge_area (edge);
      const double in_sign 
        = geometry ().cell_is_internal (geometry ().edge_to (edge)) 
        ? 1.0
        : -1.0;
      daisy_assert (in_sign < 0);

      // Tertiary domain.
      const double tertiary_in = soil_water.q_tertiary (edge) * in_sign;
      if (tertiary_in > 0)
        {
          const double flow = tertiary_in * area;
          total_water_in += flow;
          J_tertiary[edge] = flow * in_sign;
        }
      else
        J_tertiary[edge] = 0.0;

      // Secondary domain.
      const double secondary_in = soil_water.q_secondary (edge) * in_sign;
      if (secondary_in > 0)
        {
          const double flow = secondary_in * area;
          total_water_in += flow;
          J_secondary[edge] = flow * in_sign;
          
        }
      else
        J_secondary[edge] = 0.0;

      // Primary domain.
      const double primary_in = soil_water.q_primary (edge) * in_sign;
      if (primary_in > 0)
        {
          const double flow = primary_in * area;
          total_water_in += flow;
          J_primary[edge] = flow  * in_sign;
          
        }
      else
        J_primary[edge] = 0.0;
    }

  // Scale with incomming solute.
  if (std::isnormal (total_water_in))
    {
      const double scale = -J_above / total_water_in;
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const size_t edge = edge_above[i];
          
          J_tertiary[edge] *= scale;
          J_secondary[edge] *= scale;
          J_primary[edge] *= scale;
        }
    }

  // We set a fixed concentration below lower boundary, if specified.
  std::map<size_t, double> C_border;

  const double C_below = chemical.C_below ();
  if (C_below >= 0.0)
    {
      const std::vector<size_t>& edge_below 
        = geometry ().cell_edges (Geometry::cell_below);
      const size_t edge_below_size = edge_below.size ();

      for (size_t i = 0; i < edge_below_size; i++)
        {
          const size_t edge = edge_below[i];
          C_border[edge] = C_below;
        }
    }

#if 0
  // Tertiary transport.
  if (tertiary.get ())
    tertiary_transport (geometry (), soil, soil_water, *tertiary, 
                        J_tertiary, C_border, chemical, dt, scope, msg);
#endif

  // Secondary transport activated.
  secondary_transport (geometry (), soil, soil_water, J_secondary, C_border,
                       chemical, S_extra, dt, scope, msg);

  // Fully adsorbed primary transport.
  if (chemical.adsorption ().full ())
    {
      daisy_assert (iszero (J_above));
      Treelog::Open nest (msg, "solid " + matrix_solid->library_id ());
      primary_transport (geometry (), soil, soil_water,
                         *matrix_solid, J_primary, C_border,
                         chemical, S_extra, dt, scope, msg);
      return;
    }

  // Solute primary transport.
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, "solute", i, matrix_solute[i]->library_id ());
      try
        {
          primary_transport (geometry (), soil, soil_water, 
                             *matrix_solute[i], J_primary, C_border,
                             chemical, S_extra, dt, scope, msg);
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
  // Primary domain
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, 
                          "matrix_solute", i, matrix_solute[i]->library_id ());
      if (!matrix_solute[i]->check (geometry (), msg))
        ok = false;
    }
  // Solid.
  {
      Treelog::Open nest (msg, "matrix_solid: '"
                          + matrix_solid->library_id () + "'");
      if (!matrix_solid->check (geometry (), msg))
        ok = false;
  }
  return ok;
}

MovementSolute::MovementSolute (Block& al)
  : Movement (al),
    matrix_solute (Librarian::build_vector<Transport> (al, "matrix_solute")),
    matrix_solid (Librarian::build_item<Transport> (al, "matrix_solid"))
{ }


void
MovementSolute::load_solute (Syntax& syntax, AttributeList& alist, 
                             const AttributeList& prefered_solute)
{
  syntax.add_object ("matrix_solute", Transport::component, 
                     Syntax::State, Syntax::Sequence,
                     "Matrix solute transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
  std::vector<const AttributeList*> matrix_solute_models;
  AttributeList matrix_solute_default (prefered_solute);
  matrix_solute_models.push_back (&matrix_solute_default);
  AttributeList matrix_solute_reserve (Transport::reserve_model ());
  matrix_solute_models.push_back (&matrix_solute_reserve);
  AttributeList matrix_solute_none (Transport::none_model ());
  matrix_solute_models.push_back (&matrix_solute_none);
  alist.add ("matrix_solute", matrix_solute_models);

  syntax.add_object ("matrix_solid", Transport::component, 
                     Syntax::Const, Syntax::Singleton, "\
Matrix solute transport model used for fully sorbed constituents.");
  alist.add ("matrix_solid", Transport::none_model ());
}

// movement_solute.C ends here.

