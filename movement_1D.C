// movement_1D.C --- Movement in a 1D system.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "movement.h"
#include "uzmodel.h"
#include "macro.h"
#include "soil_water.h"
#include "soil.h"
#include "time.h"
#include "log.h"

class Movement1D : public Movement
{
  // Water.
  std::auto_ptr<UZmodel> water_matrix;
  std::auto_ptr<UZmodel> water_matrix_reserve;
  std::auto_ptr<Macro> water_macro;
  std::vector<double> water_matrix_flux;
  std::vector<double> water_macro_flux;

  // Soil heat
  double T_top;
  double T_top_old;
  double T_bottom;
  std::vector<double> heat_flux;
  void heat (const Time& time,
             const Soil& soil,
             const SoilWater& soil_water,
             const double T_top_new,
             const double T_bottom);
  void calculate_heat_flux (const Soil& soil,
                            const SoilWater& soil_water);

  // Simulation.
public:
  void output (Log& log) const
  { 
    output_derived (water_matrix, "soil_matrix", log);
    output_variable (water_matrix_flux, log);
    output_variable (water_macro_flux, log);
    output_variable (heat_flux, log);
  }

  // Create.
public:
  Movement1D (Block& al)
    : Movement (al),
      water_matrix (Librarian<UZmodel>::build_item (al, "water_matrix")),
      water_matrix_reserve (Librarian<UZmodel>::build_item 
                            /**/ (al, "water_matrix_reserve")),
      water_macro (NULL),
      T_top (al.number ("T_top", -500.0))
  { }
};

void
Movement1D::heat (const Time& time,
                  const Soil& soil,
                  const SoilWater& soil_water,
                  const double T_top_new,
                  const double T_bottom)
{
  // Initial state.
  if (T_top < -400.0)
    T_top = T_top_new;

  int size = soil.size ();

  // Tridiagonal matrix.
  std::vector<double> a (size, 0.0);
  std::vector<double> b (size, 0.0);
  std::vector<double> c (size, 0.0);
  std::vector<double> d (size, 0.0);

  // Inner nodes.
  for (int i = 0; i < size; i++)
    {
      // Soil Water
      const double Theta = soil_water.Theta (i);
      const double X_ice = soil_water.X_ice (i);
      const double h = soil_water.h (i);
      const double h_ice = soil_water.h_ice (i);

      const int prev = i - 1;
      const int next = i + 1;

      // Calculate average heat capacity and conductivity.
      const double conductivity = soil.heat_conductivity (i, Theta, X_ice);

      // Calculate distances.
      const double dz_next 
        = (i == size - 1)
        ? soil.z (i) - soil.z (prev)
        : soil.z (next) - soil.z (i);
      const double dz_prev 
        = (i == 0)
        ? soil.z (i) - 0.0
        : soil.z (i) - soil.z (prev);
      const double dz_both = dz_prev + dz_next;

      // Calculate temperature differences.
      const double dT_next = ((i == size - 1)
                              ? T_bottom - T[i] 
                              : T[next] - T[i]);
      const double dT_prev = (i == 0) ? T[i] - T_top : T[i] - T[prev];
      const double dT_both = dT_prev + dT_next;
      
      // Calculate conductivity gradient.
      double gradient;
      if (i == 0)
        gradient = 0.0;
      else if (i == size - 1)
        gradient 
          = (soil.heat_conductivity (i, 
                                     soil_water.Theta (i),
                                     soil_water.X_ice (i))
             - soil.heat_conductivity (prev, 
                                       soil_water.Theta (prev),
                                       soil_water.X_ice (prev)))
          / dz_prev;
      else
        gradient 
          = (soil.heat_conductivity (next, 
                                     soil_water.Theta (next),
                                     soil_water.X_ice (next))
             - soil.heat_conductivity (prev, 
                                       soil_water.Theta (prev),
                                       soil_water.X_ice (prev)))
          / dz_both;
      
      // Computational,
      const double Cx = gradient
        + water_heat_capacity
        * (soil_water.q (i) + soil_water.q (next)) / 2.0;

      // Heat capacity including thawing/freezing.
      C_apparent[i] = capacity[i];
      switch (state[i])
        {
        case freezing:
          C_apparent[i] += (latent_heat_of_fussion * latent_heat_of_fussion
                            * rho_water * soil.Cw2 (i, h)
                            / (273. * gravity));
          break;
        case thawing:
          C_apparent[i] += (latent_heat_of_fussion * latent_heat_of_fussion
                            * rho_water * soil.Cw2 (i, h_ice)
                            / (273. * gravity));
          break;
        case liquid:
        case frozen:
          break;
        }

      // Setup tridiagonal matrix.
      a[i] = - conductivity / dz_both / dz_prev + Cx / 2.0 / dz_both;
      b[i] = C_apparent[i] / dt
        + conductivity / dz_both * (1.0 / dz_next + 1.0 / dz_prev);
      c[i] = - conductivity / dz_both / dz_next - Cx / 2.0 / dz_both;
      const double x2 = dT_next / dz_next - dT_prev/ dz_prev;
      if (i == 0)
        d[i] = T[i] * C_apparent[i] / dt
          + conductivity / soil.z (1) * (x2 + T_top_new / soil.z (0))
          + Cx * (T[1] - T_top + T_top_new) / (2.0 * soil.z (1));
      else
        d[i] = T[i] * C_apparent[i] / dt + (conductivity / dz_both) * x2
          + Cx * dT_both / dz_both / 2.0;
      
      if (state[i] == freezing || state[i] == thawing)
        d[i] -= latent_heat_of_fussion * rho_water
          * (soil_water.q (i) - soil_water.q (next)) / soil.dz (i) / dt;

      // External heat source.
      d[i] += S[i];
    }
  d[size - 1] = d[size - 1] - c[size - 1] * T_bottom;
  tridia (0, size, a, b, c, d, T.begin ());
  T_top_old = T_top;
  T_top = T_top_new;
  daisy_assert (T[0] < 50.0);

  calculate_heat_flux (soil, soil_water);
}

void
Movement1D::calculate_heat_flux (const Soil& soil,
                                 const SoilWater& soil_water)
{
  // Top and inner nodes.
  double T_prev = (T_top + T_top_old) / 2.0;
  double z_prev = 0.0;
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta = soil_water.Theta (i);
      const double X_ice = soil_water.X_ice (i);
      const double K = soil.heat_conductivity (i, Theta, X_ice);
      const double T_next = (T[i] + T_old[i]) / 2.0;
      const double dT = T_prev - T_next;
      const double dz = z_prev - soil.z (i);
      const double q_water = soil_water.q (i);
      const double T = (T_prev + T_next) / 2.0;

      q[i] = - K * dT/dz + water_heat_capacity * rho_water *  q_water * T;
      T_prev = T_next;
      z_prev = soil.z (i);
    }
  // Lower boundary.
  const unsigned int i = soil.size ();
  const unsigned int prev = i - 1U;
  const double Theta = soil_water.Theta (prev);
  const double X_ice = soil_water.X_ice (prev);
  const double K = soil.heat_conductivity (prev, Theta, X_ice);
  const double T_next = T_bottom;
  const double dT = T_prev - T_next;
  const double dz = soil.z (prev-1U) - soil.z (prev);
  const double q_water = soil_water.q (i);
  const double T = (T_prev + T_next) / 2.0;

  q[i] = - K * dT/dz - water_heat_capacity * rho_water *  q_water * T;
}

static struct Movement1DSyntax
{
  static Movement& make (Block& al)
  { return *new Movement1D (al); }

  Movement1DSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "One dimensional movement.");

    // Soil water.
    syntax.add ("water_matrix", Librarian<UZmodel>::library (),
                "Main water transport model in unsaturated zone.");
    AttributeList richard;
    richard.add ("type", "richards");
    richard.add ("max_time_step_reductions", 4);
    richard.add ("time_step_reduction", 4);
    richard.add ("max_iterations", 25);
    richard.add ("max_absolute_difference", 0.02);
    richard.add ("max_relative_difference", 0.001);
    alist.add ("water_matrix", richard);
    syntax.add ("water_matrix_reserve", Librarian<UZmodel>::library (),
                "Reserve transport model if UZtop fails.");
    // Use lr as UZreserve by default.
    AttributeList lr;
    lr.add ("type", "lr");
    lr.add ("h_fc", -100.0);
    lr.add ("z_top", -10.0);
    alist.add ("water_matrix_reserve", lr);
    syntax.add ("water_macro", Librarian<Macro>::library (),
                Syntax::OptionalState, Syntax::Singleton,
                "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
    syntax.add ("water_matrix_flux", "cm/h", Syntax::LogOnly, Syntax::Sequence,
                "Matrix water flux (positive numbers mean upward).");
    syntax.add ("water_macro_flux", "cm/h", Syntax::LogOnly, Syntax::Sequence,
                "Water flux in macro pores (positive numbers mean upward).");

    // Soil heat.
    output_value (impl.T_top, "T_top", log);

    Librarian<Movement>::add_type ("1D", alist, syntax, &make);
  }
} Movement1D_syntax;
