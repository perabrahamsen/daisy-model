// transport_convection.C --- Pure forward convection.
// 
// Copyright 2007, 2008 Per Abrahamsen and KVL.
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
#include "geometry.h"
#include "soil.h"
#include "adsorption.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "log.h"
#include "mathlib.h"
#include <sstream>

struct TransportConvection : public Transport
{
  // Solute.
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
  TransportConvection (const BlockModel& al);
  ~TransportConvection ();
};

void
TransportConvection::flow (const Geometry& geo, 
                              const Soil& soil, 
                              const std::vector<double>& Theta_old,
                              const std::vector<double>& Theta_new,
                              const std::vector<double>& q,
                              const symbol /* name */,
                              const std::vector<double>& S, 
                              const std::map<size_t, double>& J_forced,
                              const std::map<size_t, double>& C_border,
                              std::vector<double>& C, 
                              std::vector<double>& J, 
                              double /* diffusion_coefficient */, double dt,
                              Treelog& msg) const
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  // One timestep left.
  double time_left = dt;

  // Initial water content.
  std::vector<double> Theta (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    Theta[c] = Theta_old[c];

  // Small timesteps.
  while (time_left > 0.0)
    {
      // Solute M.
      std::vector<double> M (cell_size);
      for (size_t c = 0; c < cell_size; c++)
        M[c] = Theta[c] * C[c];

      // Are we done yet?
      const double min_timestep_factor = 0.001;
      if (time_left < 0.1 * min_timestep_factor * dt)
        break;

      // Find new timestep.
      double ddt = time_left;
  
      // Limit timestep based on source term.
      for (size_t c = 0; c < cell_size; c++)
        if (S[c] < 0.0 && M[c] > 0.0) // If it is a sink.
          {
            const double time_to_empty = -M[c] / S[c];
            if (time_to_empty < min_timestep_factor * dt)
              // Unreasonable small time step.  Give up.
              continue;
            
            // Go down in timestep while it takes less than two to empty cell.
            while (time_to_empty < 2.0 * ddt)
              ddt *= 0.5;
          }

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

      // Update C.
      for (size_t c = 0; c < cell_size; c++)
        {
          daisy_assert (Theta[c] > 0.0);
          daisy_assert (std::isfinite (M[c]));
          C[c] = M[c] / Theta[c];
          daisy_assert (std::isfinite (C[c]));
        }

      // Find fluxes using new values (more stable).
      std::vector<double> dJ (edge_size);
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
          const int flux_from = in_flux ? edge_from : edge_to;
          double C_flux_from = -42.42e42;

          if (geo.cell_is_internal (flux_from))
            {
              // Internal cell, use its concentration.
              C_flux_from = C[flux_from];
              daisy_assert (std::isfinite (C[flux_from]));
            }
          else
            {
              i = C_border.find (e);
              if (i != C_border.end ())
                {
                  // Specified by C_border.
                  C_flux_from = (*i).second;
                  daisy_assert (std::isfinite ((*i).second));
                }
              else
                // Assume no gradient.
                {
                  const int flux_to = in_flux ? edge_to : edge_from;
                  daisy_assert (geo.cell_is_internal (flux_to));
                  C_flux_from = C[flux_to];
                  daisy_assert (std::isfinite (C[flux_to]));
                }
            }

          // Convection.
          daisy_assert (std::isfinite (q[e]));
          daisy_assert (std::isfinite (C_flux_from));
          dJ[e] = q[e] * C_flux_from;
          daisy_assert (std::isfinite (dJ[e]));
        }

      // Update values for fluxes.
      for (size_t e = 0; e < edge_size; e++)
        {
          const double value = ddt * dJ[e] * geo.edge_area (e);
          daisy_assert (std::isfinite (value));

          const int from = geo.edge_from (e);
          if (geo.cell_is_internal (from))
            {
              M[from] -= value / geo.cell_volume (from);
              daisy_assert (std::isfinite (M[from]));

            }

          const int to = geo.edge_to (e);
          if (geo.cell_is_internal (to))
            {
              M[to] += value / geo.cell_volume (to);
              daisy_assert (std::isfinite (M[to]));
            }

          daisy_assert (std::isfinite (J[e]));
          daisy_assert (std::isfinite (dJ[e]));
          J[e] += dJ[e] * ddt / dt;
        }

      // Update time left.
      time_left -= ddt;

      if (time_left < 0.1 * min_timestep_factor * dt)
        time_left = 0.0;

      const double time_spend = dt - time_left;

      // Interpolate Theta.
      for (size_t c = 0; c < cell_size; c++)
        {
          Theta[c] = (time_left * Theta_old[c] + time_spend * Theta_new[c])
            / dt;
          daisy_assert (Theta[c] > 0.0);
          daisy_assert (std::isfinite (Theta[c]));
        }

      // Update C.
      for (size_t c = 0; c < cell_size; c++)
        {
          daisy_assert (std::isfinite (M[c]));
          daisy_assert (Theta[c] > 0.0);
          daisy_assert (std::isfinite (Theta[c]));
          C[c] = M[c] / Theta[c];
          if (!std::isfinite (C[c]))
            {
              std::ostringstream tmp;
              tmp << geo.cell_name (c)
                  << ": C[" << c << "] = " << C[c]
                  << ", M[" << c << "] = " << M[c]
                  << ", Theta[" << c << "] = " << Theta[c]
                  << ", dt = " << dt
                  << ", ddt = " << ddt
                  << ", time_left = " << time_left
                  << ", Theta_old = " << Theta_old[c] 
                  << ", time_spend = " << time_spend
                  << ", Theta_new = " << Theta_new[c];
              msg.error (tmp.str ());
              C[c] = 0.0;
            }
        }
    }
}

TransportConvection::TransportConvection (const BlockModel& al)
  : Transport (al)
{ }

TransportConvection::~TransportConvection ()
{ }

static struct TransportConvectionSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new TransportConvection (al); }

  TransportConvectionSyntax ()
    : DeclareModel (Transport::component, "convection", "\
Pure forward calculation of flow except through upper boundary.\n\
J[edge] = q[edge] * C_old[upstream]")
  { }
  void load_frame (Frame&) const
  { }
} TransportConvection_syntax;

// transport_convection.C ends here.

