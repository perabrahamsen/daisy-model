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
#include "tertiary.h"
#include "frame.h"
#include "librarian.h"
#include "block_model.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include "log.h"
#include <sstream>

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
      const double min_timestep_factor = 1e-19;
      if (time_left < 0.1 * min_timestep_factor * dt)
        break;

      // Find new timestep.
      double ddt = time_left;
  
      // Limit timestep based on water flux.
      for (size_t e = 0; e < edge_size; e++)
        {
          const int cell = (q[e] > 0.0 ? geo.edge_from (e) : geo.edge_to (e));
          if (geo.cell_is_internal (cell) 
              && Theta[cell] > 1e-6 && M[cell] > 0.0)
            {
              const double loss_rate = std::fabs (q[e]) * geo.edge_area (e);
              const double content = Theta[cell] * geo.cell_volume (cell); 
              const double time_to_empty = content / loss_rate;
              if (time_to_empty < min_timestep_factor * dt)
                {
                  msg.warning ("Too fast water movement in secondary domain");
                  ddt = min_timestep_factor * dt;
                  break;
                }
              
              // Go down in timestep while it takes less than two to empty cell.
              while (time_to_empty < 2.0 * ddt)
                ddt *= 0.5;
            }
        }

      // Cell source.  Must be before transport to avoid negative values.
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
  std::vector<double> A (cell_size); // Content ignored by flow.
  std::vector<double> Mf (cell_size); // Content given to flow.
  std::vector<double> S (cell_size); // Source given to flow.

  // Mass balance.
  double total_A = 0.0;
  double total_Mf = 0.0;
  double total_source = 0.0;

  for (size_t c = 0; c < cell_size; c++)
    {
      Theta_old[c] = soil_water.Theta_secondary_old (c);
      daisy_assert (Theta_old[c] >= 0.0);
      Theta_new[c] = soil_water.Theta_secondary (c);
      daisy_assert (Theta_new[c] >= 0.0);
      const double source = solute.S_secondary (c);
      daisy_assert (std::isfinite (source));
      Mf[c] = solute.C_secondary (c) * Theta_old[c];
      daisy_assert (Mf[c] >= 0.0);
      A[c] = solute.M_secondary (c) - Mf[c];
      daisy_assert (std::isfinite (A[c]));
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
      daisy_assert (iszero (S_extra[c]));
      S_extra[c] = source - S[c];
      daisy_assert (std::isfinite (S_extra[c]));

      // Mass Balance;
      const double V = geo.cell_volume (c);
      total_Mf += V * Mf[c];
      total_source += V * source * dt;
      total_A += V * A[c];
    }
  
  // Flow.
  secondary_flow (geo, Theta_old, Theta_new, q, solute.objid, 
                  S, J_forced, C_border, Mf, J, dt, msg);

  // Mass balance.
  double total_in = 0.0;
  double total_out = 0.0;

  // Check fluxes.
  for (size_t e = 0; e < edge_size; e++)
    {
      daisy_assert (std::isfinite (J[e]));

      // Mass balance.
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);
      const double area = geo.edge_area (e);
      if (!geo.cell_is_internal (from))
	total_in += J[e] * area * dt;
      if (!geo.cell_is_internal (to))
	total_out += J[e] * area * dt;
    }

  // Mass balance.
  double total_extra = 0.0;
  double total_Mn = 0.0;
  
  // Negative content should be handled by primary transport.
  std::vector<double> Mn (cell_size); // New content.
  std::vector<double> C (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      Mn[c] = A[c] + Mf[c] + S_extra[c] * dt;
      if (Mn[c] < 0.0 || Theta_new[c] < 1e-6)
        {
          S_extra[c] = Mn[c] / dt;
          Mn[c] = 0.0;
        }
      else
        S_extra[c] = 0.0;

      daisy_assert (std::isfinite (S_extra[c]));

      // Mass balance.
      const double V = geo.cell_volume (c);
      total_extra += S_extra[c] * V * dt;
      total_Mn += Mn[c] * V;
    }

  solute.set_secondary (soil, soil_water, Mn, J);

  // Mass balance
  const double increase = total_Mn - total_A - total_Mf;
  const double input = total_in + total_source;
  const double output = total_out + total_extra;
  const double error = input - increase - output;
  const double magnitude
    = std::abs (total_Mn)
    + std::abs (total_A)
    + std::abs (total_Mf)
    + std::abs (total_in)
    + std::abs (total_source)
    + std::abs (total_out)
    + std::abs (total_extra);
  if (std::abs (error) > magnitude * 1e-10)
    {
      std::ostringstream tmp;
      tmp << "Mass balance error in secondary transport of " << error
	  << " g (input - increase - output)\n"
	  << "dt = " << dt << "h\n"
	  << "input = " << input << "g (in + source)\n"
	  << "increase = " << increase << "g (Mn - Mf - A)\n"
	  << "output = " << output << "g (out + extra)\n"
	  << "Mf = " << total_Mf << "g (old solute mass)\n"
	  << "A = " << total_A << "g (old sorbed mass)\n"
	  << "in = " << total_in << "g (from bottom)\n"
	  << "out = " << total_out << "g (to top)\n"
	  << "source = " << total_source << "g\n"
	  << "extra = " << total_extra << "g (source pass to primary domain)\n"
	  << "Mn = " << total_Mn << "g (new content)";
	msg.error (tmp.str ());
    }
}

void
MovementSolute::primary_transport (const Geometry& geo, const Soil& soil,
                                   const SoilWater& soil_water,
                                   const Transport& transport,
                                   const bool sink_sorbed,
                                   const size_t transport_iteration,
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
      if (sink_sorbed && S[c] < 0.0)
        {
          A[c] += S[c] * dt;
          S[c] = 0.0;
          if (A[c] < 0.0)
            {
              S[c] = A[c] / dt;
              A[c] = 0.0;
            }
        }

      daisy_assert (std::isfinite (S[c]));
    }
  
  // Flow.
  transport.flow (geo, soil, Theta_old, Theta_new, q, solute.objid, 
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

      if (M[c] < 0.0)
        {
	  if (!daisy_full_debug ())
	    {
	      std::ostringstream tmp;
	      tmp << "M[" << c << "] = " << M[c] 
		  << " @ " << geo.cell_name (c)
		  << ", C = " << C[c]
		  << ", A = " << A[c]
		  << ", M_new = " << M[c]
		  << ", M_old = " << solute.M_primary (c) << ", dt " << dt
		  << ", S = " << S[c] 
		  << ", S_extra = " << S_extra[c];
	      solute.debug_cell (tmp, c);
	      tmp << ", Theta_old " << Theta_old[c]
		  << ", Theta_new " << Theta_new[c]
		  << ", root " << soil_water.S_root (c)
		  << ", drain " << soil_water.S_drain (c)
		  << ", B2M " << soil_water.S_B2M (c)
		  << ", M2B " << soil_water.S_M2B (c)
		  << ", forward_total " << soil_water.S_forward_total (c)
		  << ", forward_sink " << soil_water.S_forward_sink (c)
		  << ", sum " << soil_water.S_sum (c)
		  << ", v1 " << soil_water.velocity_cell_primary (geo, c)
		  << ", v2 " << soil_water.velocity_cell_secondary (geo, c);
	      const std::vector<size_t>& edges = geo.cell_edges (c);
	      for (size_t i = 0; i < edges.size (); i++)
		{
		  const size_t e = edges[i];
		  tmp  << "\n" << geo.edge_name (e) 
		       << ": q = " << q[e] << ", J = " << J[e];
		}
	      msg.debug (tmp.str ());
	    }
          if (transport_iteration == 0)
            throw "Negative concentration";
        }
    }

  solute.set_primary (soil, soil_water, M, J);
}

void
MovementSolute::divide_top_incomming (const Geometry& geo, 
                                      const SoilWater& soil_water, 
                                      const double J_above, // [g/cm^2/h]
                                      std::map<size_t, double>& J_primary,
                                      std::map<size_t, double>& J_secondary,
                                      std::map<size_t, double>& J_tertiary)
{
  daisy_assert (J_above < 0.0); // Negative upward flux.
  const std::vector<size_t>& edge_above 
    = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  double total_water_in = 0.0;  // [cm^3 W/h]
  double total_area = 0.0;      // [cm^2 S]

  // Find incomming water in all domain.
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      daisy_assert (geo.cell_is_internal (cell));
      const double area = geo.edge_area (edge); // [cm^2 S]
      total_area += area;
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) 
        ? 1.0
        : -1.0;
      daisy_assert (in_sign < 0);

      // Tertiary domain.
      const double q_tertiary = soil_water.q_tertiary (edge);
      daisy_assert (std::isfinite (q_tertiary));
      const double tertiary_in = q_tertiary * in_sign; // [cm^3 W/cm^2 S/h]
      if (tertiary_in > 0)
        {
          total_water_in += tertiary_in * area;
          J_tertiary[edge] = q_tertiary; // [cm^3 W/cm^2 S/h]
        }
      else
        J_tertiary[edge] = 0.0;

      // Secondary domain.
      const double q_secondary = soil_water.q_secondary (edge);
      const double secondary_in = q_secondary * in_sign; // [cm^3 W/cm^2 S/h]
      if (secondary_in > 0)
        {
          total_water_in += secondary_in * area;
          J_secondary[edge] = q_secondary; // [cm^3 W/cm^2 S/h]
          
        }
      else
        J_secondary[edge] = 0.0;

      // Primary domain.
      const double q_primary = soil_water.q_primary (edge);
      const double primary_in = q_primary * in_sign; // [cm^3 W/cm^2 S/h]
      if (primary_in > 0)
        {
          total_water_in += primary_in * area;
          J_primary[edge] = q_primary; // [cm^3 W/cm^2 S/h]
        }
      else
        J_primary[edge] = 0.0;
    }
  daisy_approximate (total_area, geo.surface_area ());

  if (total_water_in > 1e-9 * total_area)
    // Scale with incomming solute.
    {
      // [g/cm^3 W] = [g/cm^2 S/h] * [cm^2 S] / [cm^3 W/h] 
      const double C_above = -J_above * total_area / total_water_in;
      daisy_assert (std::isfinite (C_above));
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const size_t edge = edge_above[i];
          // [g/cm^2 S/h] = [cm^3 W/cm^2 S/h] * [g/cm^3 W]
          J_tertiary[edge] *= C_above;
          J_secondary[edge] *= C_above;
          J_primary[edge] *= C_above;
        }
    }
  else
    {
      daisy_assert (total_water_in >= 0.0);
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const size_t edge = edge_above[i];
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          J_tertiary[edge] = 0.0;
          J_secondary[edge] = 0.0;
          J_primary[edge] = -J_above * in_sign;
        }
    }
}

void
MovementSolute::divide_top_outgoing (const Geometry& geo, 
                                     const Chemical& chemical,
                                     const double J_above,
                                     std::map<size_t, double>& J_primary,
                                     std::map<size_t, double>& J_secondary,
                                     std::map<size_t, double>& J_tertiary)
{
  daisy_assert (J_above > 0.0); // Positive upward flux.
  const std::vector<size_t>& edge_above 
    = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  double total_amount = 0.0;    // [g/cm]
  double total_area = 0.0;      // [cm^2]

  // Find total content in primary domain in cells connected to border.
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      daisy_assert (geo.cell_is_internal (cell));
      const double area = geo.edge_area (edge);

      // No flux out of tertiary or secondary domains.
      J_tertiary[edge] = 0.0;
      J_secondary[edge] = 0.0;

      // Find content 
      const double M = chemical.M_primary (cell); // [g/cm^3]
      const double amount =  M * area;            // [g/cm]
      total_amount += amount;                     // [g/cm]
      total_area += area;
      J_primary[edge] = M;                        // [g/cm^3]
    }

  // Scale with content
  if (total_amount > 0.0)
    {
      // [cm/h] = [g/cm^2/h] * [cm^2] / [g/cm]
      const double scale = -J_above * total_area / total_amount;
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const size_t edge = edge_above[i];
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          // [g/cm^2/h] = [g/cm^3] * [cm/h]
          J_tertiary[edge] = 0.0;
          J_secondary[edge] = 0.0;
          J_primary[edge] *= in_sign * scale;
        }
    }
  else
    {
      daisy_assert (iszero (total_amount));
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const size_t edge = edge_above[i];
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          J_tertiary[edge] = 0.0;
          J_secondary[edge] = 0.0;
          J_primary[edge] = -J_above * in_sign;
        }
    }
}

void
MovementSolute::zero_top (const Geometry& geo, 
                          std::map<size_t, double>& J_primary,
                          std::map<size_t, double>& J_secondary,
                          std::map<size_t, double>& J_tertiary)
{
  const std::vector<size_t>& edge_above 
    = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  // Clear all.
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];

      // No flux out of any domains.
      J_tertiary[edge] = 0.0;
      J_secondary[edge] = 0.0;
      J_primary[edge] = 0.0;
    }
}

void
MovementSolute::solute (const Soil& soil, const SoilWater& soil_water,
                        const double J_above, Chemical& chemical, 
                        const double dt,
                        const Scope& scope, Treelog& msg)
{
  daisy_assert (std::isfinite (J_above));
  const size_t cell_size = geometry ().cell_size ();
  const size_t edge_size = geometry ().edge_size ();

  // Source term transfered from secondary to primary domain.
  std::vector<double> S_extra (cell_size, 0.0);

  // Divide top solute flux according to water.
  std::map<size_t, double> J_tertiary;
  std::map<size_t, double> J_secondary; 
  std::map<size_t, double> J_primary;

  if (J_above > 0.0)
    // Outgoing, divide according to content in primary domain only.
    divide_top_outgoing (geometry (), chemical, J_above, 
                         J_primary, J_secondary, J_tertiary);
  else if (J_above < 0.0)
    // Incomming, divide according to all incomming water.
    divide_top_incomming (geometry (), soil_water, J_above, 
                          J_primary, J_secondary, J_tertiary);
  else
    // No flux.
    zero_top (geometry (), J_primary, J_secondary, J_tertiary);

  // Check result.
  {
    const std::vector<size_t>& edge_above 
      = geometry ().cell_edges (Geometry::cell_above);
    const size_t edge_above_size = edge_above.size ();
    double J_sum = 0.0;
    for (size_t i = 0; i < edge_above_size; i++)
      {
        const size_t edge = edge_above[i];
        const double in_sign 
          = geometry ().cell_is_internal (geometry ().edge_to (edge)) 
          ? 1.0 : -1.0;
        const double area = geometry ().edge_area (edge); // [cm^2 S]
        const double J_edge       // [g/cm^2 S/h]
          = J_tertiary[edge] + J_secondary[edge] + J_primary[edge];
        J_sum += in_sign * J_edge * area; // [g/h]
        if (in_sign * J_tertiary[edge] < 0.0)
          {
            std::ostringstream tmp;
            tmp << "J_tertiary[" << edge << "] = " << J_tertiary[edge]
                << ", in_sign = " << in_sign << ", J_above = " << J_above;
            msg.bug (tmp.str ());
          }
        if (in_sign * J_secondary[edge] < 0.0)
          {
            std::ostringstream tmp;
            tmp << "J_secondary[" << edge << "] = " << J_secondary[edge]
                << ", in_sign = " << in_sign << ", J_above = " << J_above;
            msg.bug (tmp.str ());
          }
      }
    J_sum /= geometry ().surface_area (); // [g/cm^2 S/h]
    daisy_approximate (-J_above, J_sum);
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

  // Tertiary transport.
  tertiary->solute (geometry (), soil_water, J_tertiary, dt, chemical, msg);

  // Fully adsorbed.
  if (chemical.adsorption ().full ())
    {
      static const symbol solid_name ("immobile transport");
      Treelog::Open nest (msg, solid_name);
      if (!iszero (J_above))
        {
          std::ostringstream tmp;
          tmp << "J_above = " << J_above << ", expected 0 for full sorbtion";
          msg.error (tmp.str ());
        }

      // Secondary "transport".
      std::vector<double> J2 (edge_size, 0.0); // Flux delivered by flow.
      std::vector<double> Mn (cell_size); // New content.
      for (size_t c = 0; c < cell_size; c++)
        {
          Mn[c] = chemical.M_secondary (c) + chemical.S_secondary (c) * dt;
          if (Mn[c] < 0.0)
            {
              S_extra[c] = Mn[c] / dt;
              Mn[c] = 0.0;
            }
          else
            S_extra[c] = 0.0;
        }
      chemical.set_secondary (soil, soil_water, Mn, J2);

      // Primary "transport".
      primary_transport (geometry (), soil, soil_water,
                         *matrix_solid, sink_sorbed, 0, J_primary, C_border,
                         chemical, S_extra, dt, scope, msg);
      return;
    }

  // Secondary transport activated.
  secondary_transport (geometry (), soil, soil_water, J_secondary, C_border,
                       chemical, S_extra, dt, scope, msg);

  // Solute primary transport.
  for (size_t transport_iteration = 0; 
       transport_iteration < 2; 
       transport_iteration++)
    for (size_t i = 0; i < matrix_solute.size (); i++)
      {
        solute_attempt (i);
        static const symbol solute_name ("solute");
        Treelog::Open nest (msg, solute_name, i, matrix_solute[i]->objid);
        try
          {
            primary_transport (geometry (), soil, soil_water, 
                               *matrix_solute[i], sink_sorbed, 
                               transport_iteration,
                               J_primary, C_border,
                               chemical, S_extra, dt, scope, msg);
            if (i > 0 && !daisy_full_debug ())
              msg.debug ("Succeeded");
            return;
          }
        catch (const char* error)
          {
	    if (!daisy_full_debug ())
	      msg.debug (std::string ("Solute problem: ") + error);
          }
        catch (const std::string& error)
          {
	    if (!daisy_full_debug ())
	      msg.debug(std::string ("Solute trouble: ") + error);
          }
        solute_failure (i);
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

void 
MovementSolute::output_solute (Log& log) const
{ 
  output_base (log);
  // output_list (matrix_solute, "matrix_solute", log, Transport::component);
}

bool
MovementSolute::check_derived (Treelog& msg) const
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

MovementSolute::MovementSolute (const BlockModel& al)
  : Movement (al),
    matrix_solute (Librarian::build_vector<Transport> (al, "matrix_solute")),
    matrix_solid (Librarian::build_item<Transport> (al, "matrix_solid")),
    sink_sorbed (al.flag ("sink_sorbed"))
{ }

static struct MovementSoluteSyntax : public DeclareBase
{
  MovementSoluteSyntax ()
    : DeclareBase (Movement::component, "solute", "\
Shared paramaters for handling solutes.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("matrix_solute", Transport::component, 
                       Attribute::Const, Attribute::Variable,
                       "Matrix solute transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    frame.declare_object ("matrix_solid", Transport::component, 
                       Attribute::Const, Attribute::Singleton, "\
Matrix solute transport model used for fully sorbed constituents.");
    frame.set ("matrix_solid", "none");
    frame.declare_boolean ("sink_sorbed", Attribute::Const,
                           "Substract sink term from sorbed matter.");
    frame.set ("sink_sorbed", true);
  }
} MovementSolute_syntax;

// movement_solute.C ends here.

