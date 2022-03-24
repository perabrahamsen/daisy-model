// movement_1D.C --- Movement in a 1D system.
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
#include "movement_solute.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "groundwater.h"
#include "surface.h"
#include "weather.h"
#include "chemical.h"
#include "transport.h"
#include "adsorption.h"
#include "log.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "transport.h"
#include "tertiary.h"
#include "treelog.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"
#include "block_model.h"
#include "uzmodel.h"
#include <sstream>

struct Movement1D : public MovementSolute
{
  // Geometry.
  std::unique_ptr<Geometry1D> geo;
  Geometry& geometry () const;

  // Water.
  const auto_vector<UZmodel*> matrix_water;
  void tick_water (const Geometry1D& geo,
                   const Soil& soil, const SoilHeat& soil_heat, 
                   Surface& surface, Groundwater& groundwater,
                   const std::vector<double>& S,
                   std::vector<double>& h_old,
                   const std::vector<double>& Theta_old,
                   const std::vector<double>& h_ice,
                   std::vector<double>& h,
                   std::vector<double>& Theta,
                   std::vector<double>& q,
                   std::vector<double>& q_p,
                   double dt, Treelog& msg);

  // Heat.
  /* const */ double delay;     // Period delay [ cm/rad ??? ]
  double surface_snow_T (const Soil& soil,
                         const SoilWater& soil_water,
                         const SoilHeat& soil_heat,
                         const double T_snow,
                         const double K_snow,
                         const double dZs) const;
  double bottom_heat (const Time& time, const Weather& weather) const ;
  std::vector<double> default_heat (const Soil& soil, 
                                    const Time& time, const Weather& weather);
  static void solve_heat (const Geometry1D& geo,
                          const std::vector<double>& q_water,
                          const std::vector<double>& S_water,
                          const std::vector<double>& S_heat,
                          const std::vector<double>& capacity_new,
                          const std::vector<double>& conductivity,
                          const double T_top,
                          const double T_top_new,
                          const double T_bottom,
                          std::vector<double>& T,
                          const double dt);
  void heat (const std::vector<double>& q_water,
             const std::vector<double>& S_water,
             const std::vector<double>& S_heat,
               const std::vector<double>& capacity_new,
             const std::vector<double>& conductivity,
             double T_top,
             double T_top_new,
             double T_bottom,
             std::vector<double>& T,
             const double dt, Treelog&) const;

  // Simulation.
  void tick (const Soil& soil, SoilWater& soil_water, const SoilHeat& soil_heat,
             Surface& surface, Groundwater& groundwater,
             const Time& time, const Scope&, const Weather& weather, double dt, 
             Treelog& msg);
  void output (Log&) const;

  // Create.
  void initialize_derived (const Time&, const Scope&,
			   const Soil& soil, const Groundwater& groundwater,
                           bool has_macropores, 
                           Treelog&);
  Movement1D (const BlockModel& al);
  ~Movement1D ();
};


Geometry& 
Movement1D::geometry () const
{ return *geo; }

void 
Movement1D::tick_water (const Geometry1D& geo,
                        const Soil& soil, const SoilHeat& soil_heat, 
                        Surface& surface, Groundwater& groundwater,
                        const std::vector<double>& S,
                        std::vector<double>& h_old,
                        const std::vector<double>& Theta_old,
                        const std::vector<double>& h_ice,
                        std::vector<double>& h,
                        std::vector<double>& Theta,
                        std::vector<double>& q,
                        std::vector<double>& q_p,
                        const double dt, 
                        Treelog& msg)
{
  const size_t top_edge = 0U;
  const size_t bottom_edge = geo.edge_size () - 1U;

  // Limit for groundwater table.
  size_t last  = soil.size () - 1;

  // Limit for ridging.
  const size_t first = 0U;
  
  // Calculate matrix flow next.

  for (size_t m = 0; m < matrix_water.size (); m++)
    {
      water_attempt (m);
      Treelog::Open nest (msg, matrix_water[m]->name);
      try
        {
          matrix_water[m]->tick (msg, geo, soil, soil_heat,
                                 first, surface, top_edge, 
                                 last, groundwater, bottom_edge,
                                 S, h_old, Theta_old, h_ice, h, Theta, 0U, q, 
                                 dt);

          for (size_t i = last + 2; i <= soil.size (); i++)
            {
              q[i] = q[i-1];
              q_p[i] = q_p[i-1];
            }

          // Update surface and groundwater reservoirs.
          surface.accept_top (q[0] * dt, geo, 0U, dt, msg);
          surface.update_pond_average (geo);
          const double q_down = q[soil.size ()] + q_p[soil.size ()];
          groundwater.accept_bottom (q_down * dt, geo, soil.size ());
          if (m > 0)
            msg.debug ("Reserve model succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.debug (std::string ("UZ problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.debug (std::string ("UZ trouble: ") + error);
        }
      
      water_failure (m);
    }
  throw "Water matrix transport failed"; 
}


double 
Movement1D::surface_snow_T (const Soil& soil,
                            const SoilWater& soil_water,
                            const SoilHeat& soil_heat,
                            const double T_snow,
                            const double K_snow,
                            const double dZs) const
{
  // Information about soil.
  const double K_soil 
    = soil.heat_conductivity (0, soil_water.Theta (0),
                              soil_water.X_ice (0)) 
    * 1e-7 * 100.0 / 3600.0; // [erg/cm/h/dg C] -> [W/m/dg C]
  const double Z = -geo->cell_z (0) / 100.0; // [cm] -> [m]
  const double T_soil = soil_heat.T (0); // [dg C]
  daisy_assert (T_soil > -100.0);
  daisy_assert (T_soil < 50.0);

  const double T = (K_soil / Z * T_soil + K_snow / dZs * T_snow) 
    / (K_soil / Z + K_snow / dZs);
  daisy_assert (T > -100.0);
  daisy_assert (T < 50.0);
  return T;
}

double 
Movement1D::bottom_heat (const Time& time, const Weather& weather) const 
{ return weather.T_normal (time, delay); }

std::vector<double> 
Movement1D::default_heat (const Soil& soil, 
                          const Time& time, const Weather& weather)
{
  // Fetch average temperatur.
  const double rad_per_day = 2.0 * M_PI / 365.0;

  // Calculate delay.
  const double pF_2_0 = -100.0;
  double k = 0;
  double C = 0;

  std::vector<double> T;
  const size_t cell_size = geo->cell_size ();

  for (unsigned int i = 0; i < cell_size; i++)
    {
      const double Theta_pF_2_0 = soil.Theta (i, pF_2_0, 0.0);
      k += geo->dz (i) * soil.heat_conductivity (i, Theta_pF_2_0, 0.0);
      C += geo->dz (i) * soil.heat_capacity (i, Theta_pF_2_0, 0.0);
      const double a = k / C;
      delay = geo->zplus (i) / sqrt (24.0 * 2.0 * a / rad_per_day);

      T.push_back (bottom_heat (time, weather));
    }
  daisy_assert (T.size () == cell_size);
  return T;
}

void 
Movement1D::solve_heat (const Geometry1D& geo,
                        const std::vector<double>& q_water,
                        const std::vector<double>& /* S_water */,
                        const std::vector<double>& S, // Heat.
                        const std::vector<double>& capacity, // New.
                        const std::vector<double>& conductivity,
                        const double T_top,
                        const double T_top_new,
                        const double T_bottom,
                        std::vector<double>& T,
                        const double dt)
{
  const size_t size = geo.cell_size ();

  // Tridiagonal matrix.
  std::vector<double> a (size, 0.0);
  std::vector<double> b (size, 0.0);
  std::vector<double> c (size, 0.0);
  std::vector<double> d (size, 0.0);

  // Inner cells.
  for (int i = 0; i < size; i++)
    {
      // Surrounding cells.
      const int prev = i - 1;
      const int next = i + 1;

      // Calculate average heat capacity and conductivity.
      const double conductivity_cell = conductivity[i];

      // Calculate distances.
      const double dz_next 
        = (i == size - 1)
        ? geo.cell_z (i) - geo.cell_z (prev)
        : geo.cell_z (next) - geo.cell_z (i);
      const double dz_prev 
        = (i == 0)
        ? geo.cell_z (i) - 0.0
        : geo.cell_z (i) - geo.cell_z (prev);
      const double dz_both = dz_prev + dz_next;

      // Calculate temperature differences.
      const double dT_next = ((i == size - 1)
                              ? T_bottom - T[i] 
                              : T[next] - T[i]);
      const double dT_prev = (i == 0) ? T[i] - T_top : T[i] - T[prev];
      const double dT_both = dT_prev + dT_next;

      // Calculate conductivity gradient.
      double gradient_cell;
      if (i == 0)
        gradient_cell = 0.0;
      else if (i == size - 1)
        gradient_cell = (conductivity_cell - conductivity[prev]) / dz_prev;
      else
        gradient_cell = (conductivity[next] - conductivity[prev]) / dz_both;

      // Computational,
      const double Cx = gradient_cell
        + water_heat_capacity * (q_water[i] + q_water[next]) / 2.0;

      // Heat capacity including thawing/freezing.
      const double capacity_cell = capacity[i];

      // Setup tridiagonal matrix.
      a[i] = - conductivity_cell / dz_both / dz_prev + Cx / 2.0 / dz_both;
      b[i] = capacity_cell / dt
        + conductivity_cell / dz_both * (1.0 / dz_next + 1.0 / dz_prev);
      c[i] = - conductivity_cell / dz_both / dz_next - Cx / 2.0 / dz_both;
      const double x2 = dT_next / dz_next - dT_prev/ dz_prev;
#if 1
      // Why do we need this special case?
      if (i == 0)
        d[i] = T[i] * capacity_cell / dt
          + conductivity_cell / geo.cell_z (1) * (x2 + T_top_new / geo.cell_z (0))
          + Cx * (T[1] - T_top + T_top_new) / (2.0 * geo.cell_z (1));
      else
#endif
        d[i] = T[i] * capacity_cell / dt + (conductivity_cell / dz_both) * x2
          + Cx * dT_both / dz_both / 2.0;

      // External heat source + thawing/freezing.
      d[i] += S[i];
    }
  d[size - 1] = d[size - 1] - c[size - 1] * T_bottom;
  tridia (0, size, a, b, c, d, T.begin ());
  if (T[0] > 50.0)
    {
      std::ostringstream tmp;
      tmp << "T[0] = " << T[0] << ", T_top = " << T_top 
          << ", T_top_new = " << T_top_new << ", T_bottom = " << T_bottom;
      daisy_bug (tmp.str ());
    }
}

void 
Movement1D::heat (const std::vector<double>& q_water,
                  const std::vector<double>& S_water,
                  const std::vector<double>& S_heat,
                  const std::vector<double>& capacity_new,
                  const std::vector<double>& conductivity,
                  const double T_top,
                  const double T_top_new,
                  const double T_bottom,
                  std::vector<double>& T,
                  const double dt, Treelog&) const
{
  solve_heat (*geo, q_water, S_water, S_heat, 
              capacity_new, conductivity,
              T_top, T_top_new, T_bottom, T, dt);
}

void 
Movement1D::tick (const Soil& soil, SoilWater& soil_water, 
                  const SoilHeat& soil_heat,
                  Surface& surface, Groundwater& groundwater,
                  const Time& time, const Scope&, const Weather& weather, 
                  const double dt, Treelog& msg) 
{
  const size_t edge_size = geo->edge_size ();
  const size_t cell_size = geo->cell_size ();

  TREELOG_MODEL (msg);

  // Cells.
  std::vector<double> S_sum (cell_size);
  std::vector<double> h_old (cell_size);
  std::vector<double> Theta_old (cell_size);
  std::vector<double> h_ice (cell_size);
  std::vector<double> h (cell_size);
  std::vector<double> Theta (cell_size);
  
  for (size_t c = 0; c < cell_size; c++)
    {
      S_sum[c] = soil_water.S_sum (c);
      h_old[c] = soil_water.h_old (c);
      Theta_old[c] = soil_water.Theta_old (c);
      h_ice[c] = soil_water.h_ice (c);
      h[c] = soil_water.h (c);
      Theta[c] = soil_water.Theta (c);
    }

  // Edges.
  std::vector<double> q (edge_size, 0.0);
  std::vector<double> q_p (edge_size, 0.0);

  for (size_t e = 0; e < edge_size; e++)
    {
      q[e] = soil_water.q_matrix (e);
      q_p[e] = soil_water.q_tertiary (e);
    }
  tick_water (*geo, soil, soil_heat, surface, groundwater, 
              S_sum, h_old, Theta_old, h_ice, h, Theta,
              q, q_p, dt, msg);
  soil_water.set_matrix (h, Theta, q);
}

void 
Movement1D::output (Log& log) const
{ 
  output_solute (log);
  // output_list (matrix_water, "matrix_water", log, UZmodel::component);
  // output_submodule (*geo, "Geometry", log);
}

void 
Movement1D::initialize_derived (const Time&, const Scope&, const Soil& soil,
                                const Groundwater& groundwater,
                                bool has_macropores, Treelog& msg)
{
  TREELOG_MODEL (msg);

  for (size_t i = 0; i < matrix_water.size (); i++)
    matrix_water[i]->has_macropores (has_macropores);

  std::ostringstream tmp;
  tmp << "Bottom heat delay = " << delay;
  msg.debug (tmp.str ());
  
}

Movement1D::Movement1D (const BlockModel& al)
  : MovementSolute (al),
    geo (submodel<Geometry1D> (al, "Geometry")),
    matrix_water (Librarian::build_vector<UZmodel> (al, "matrix_water"))
{ }

Movement1D::~Movement1D ()
{ }

Movement*
Movement::build_vertical (const BlockModel& al)
{ return new Movement1D (al); }

static struct Movement1DSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new Movement1D (al); }

  Movement1DSyntax ()
    : DeclareModel (Movement::component, "vertical", "solute", "\
One dimensional movement.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("Tertiary", "old");
    frame.set_strings ("matrix_solute", "Hansen", "convection", "none");

    frame.declare_submodule ("Geometry", Attribute::Const,
                         "Discretization of the soil.",
                         Geometry1D::load_syntax);
    frame.declare_object ("matrix_water", UZmodel::component, 
                      Attribute::Const, Attribute::Variable,
                      "Vertical matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    frame.set_strings ("matrix_water", "richards", "lr");
 
  }
} Movement1D_syntax;

// movement_1D.C ends here.
