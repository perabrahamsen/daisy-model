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
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "macro.h"
#include "groundwater.h"
#include "surface.h"
#include "weather.h"
#include "solute.h"
#include "element.h"
#include "log.h"
#include "submodeler.h"
#include "memutils.h"
#include <sstream>

static const double rho_water = 1.0; // [g/cm^3]

struct Movement1D : public Movement
{
  // Geometry.
  std::auto_ptr<Geometry1D> geo;
  Geometry& geometry () const;

  // Water.
  const std::vector<UZmodel*> matrix_water;
  std::auto_ptr<Macro> macro;
  void macro_tick (const Soil& soil, SoilWater& soil_water,
                   Surface& surface, const double dt, Treelog& msg);
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

  // Solute.
  const std::vector<Transport*> matrix_solute;
  std::auto_ptr<Transport> transport_solid; // Pseudo transport for non-solutes
  std::auto_ptr<Mactrans> mactrans; // Solute transport model in macropores.
  void solute (const Soil& soil, 
               const SoilWater& soil_water, 
               const double J_in, Solute& solute, 
               const double dt,
               Treelog& msg);
  void element (const Soil& soil, 
                const SoilWater& soil_water, 
                Element& element,
                Adsorption& adsorption,
                double diffusion_coefficient,
                double dt,
                Treelog& msg);
  void flow (const Soil& soil, const SoilWater& soil_water, 
             const std::string& name,
             std::vector<double>& M, 
             std::vector<double>& C, 
             std::vector<double>& S, 
             std::vector<double>& S_p, 
             std::vector<double>& J, 
             std::vector<double>& J_p, 
             Adsorption& adsorption,
             double diffusion_coefficient,
             double dt,
             Treelog& msg);

  // Heat.
  /* const */ double delay;	// Period delay [ cm/rad ??? ]
  double surface_snow_T (const Soil& soil,
                         const SoilWater& soil_water,
                         const SoilHeat& soil_heat,
                         const double T_snow,
                         const double K_snow,
                         const double dZs) const;
  double bottom_heat (const Time& time, const Weather& weather) const ;
  std::vector<double> default_heat (const Soil& soil, 
                                    const Time& time, const Weather& weather);
  static void calculate_heat_flux (const Geometry& geo,
                                   const Soil& soil,
                                   const SoilWater& soil_water,
                                   const std::vector<double>& T_old,
                                   double T_prev,
                                   const std::vector<double>& T,
                                   double T_bottom,
                                   std::vector<double>& q);
  static void solve_heat (const Geometry1D& geo,
                          const Soil& soil,
                          const SoilWater& soil_water,
                          SoilHeat& soil_heat,
                          const Surface& surface,
                          double T_bottom, double dt);
  void tick_heat (const Geometry1D& geo,
                  const Soil& soil,
                  SoilWater& soil_water,
                  SoilHeat& soil_heat,
                  const Surface& surface, 
                  double T_bottom, double dt);

  // Management.
  void ridge (Surface& surface, const Soil& soil, const SoilWater& soil_water,
              const AttributeList& al);

  // Simulation.
  void tick (const Soil& soil, SoilWater& soil_water, SoilHeat& soil_heat,
             Surface& surface, Groundwater& groundwater,
             const Time& time, const Weather& weather, double dt, 
             Treelog& msg);
   void output (Log&) const;

  // Create.
  bool check (Treelog& err) const;
  void initialize (Block&, const AttributeList& alist,
                   const Soil& soil, const Groundwater& groundwater);
  Movement1D (Block& al);
  ~Movement1D ();
};


Geometry& 
Movement1D::geometry () const
{ return *geo; }

void 
Movement1D::macro_tick (const Soil& soil, SoilWater& soil_water,
                        Surface& surface, const double dt, Treelog& msg)
{ 
  if (!macro.get ())			// No macropores.
    return;

  // Calculate preferential flow first.
  std::fill (soil_water.S_p_.begin (), soil_water.S_p_.end (), 0.0);
  std::fill (soil_water.q_p_.begin (), soil_water.q_p_.end (), 0.0);
  macro->tick (*geo, soil, 0, soil.size () - 1, surface, 
               soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
               soil_water.S_sum_, soil_water.S_p_, soil_water.q_p_, dt, msg);
}

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
  // Limit for groundwater table.
  size_t last  = soil.size () - 1;
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      daisy_assert (soil.size () > 1);
      if (groundwater.table () <= geo.zplus (soil.size () - 2))
        throw ("Groundwater table in or below lowest cell.");
      last = geo.interval_plus (groundwater.table ());
      if (last >=  soil.size () - 1)
        daisy_assert ("Groundwater too low.");
      // Pressure at the last cell is equal to the water above it.
      for (size_t i = last + 1; i < soil.size (); i++)
        {
          h_old[i] = groundwater.table () - geo.z (i);
          h[i] = groundwater.table () - geo.z (i);
        }
    }

  // Limit for ridging.
  const size_t first = (surface.top_type (geo, 0U) == Surface::soil)
    ? surface.last_cell (geo, 0U) 
    : 0U;
  // Calculate matrix flow next.

  for (size_t m = 0; m < matrix_water.size (); m++)
    {
      Treelog::Open nest (msg, matrix_water[m]->name);
      try
        {
          matrix_water[m]->tick (msg, geo, soil, soil_heat,
                                 first, surface, 0U, last, groundwater,
                                 S, h_old, Theta_old, h_ice, h, Theta, 0U, q, 
                                 dt);

          for (size_t i = last + 2; i <= soil.size (); i++)
            {
              q[i] = q[i-1];
              q_p[i] = q_p[i-1];
            }

          // Update Theta below groundwater table.
          if (groundwater.bottom_type () == Groundwater::pressure)
            {
              for(size_t i = last + 1; i < soil.size (); i++)
                Theta[i] = soil.Theta (i, h[i], h_ice[i]);
            }

          // Update surface and groundwater reservoirs.
          surface.accept_top (q[0] * dt, geo, 0U, dt, msg);
          groundwater.accept_bottom ((q[soil.size ()]
                                      + q_p[soil.size ()]) * dt,
                                     geo, soil.size ());
          if (m > 0)
            msg.message ("Reserve model succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("UZ problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("UZ trouble: ") + error);
        }
    }
  throw "Water matrix transport failed"; 
}

void 
Movement1D::solute (const Soil& soil, 
                    const SoilWater& soil_water, 
                    const double J_in, Solute& solute, 
                    const double dt,
                    Treelog& msg)
{ 
  solute.tick (geo->cell_size (), soil_water, dt);

  // Upper border.
  if (soil_water.q_p (0) < 0.0)
    {
      if (soil_water.q (0) >= 0.0)
        {
          if (soil_water.q (0) > 1.0e-10)
            {
              std::ostringstream tmp;
              tmp << "BUG: q_p[0] = " << soil_water.q_p (0) 
                  << " and q[0] = " << soil_water.q (0);
              msg.error (tmp.str ());
            }
          solute.J_p[0] = J_in;
          solute.J[0] = J_in;
        }
      else
        {
          const double macro_fraction
            = soil_water.q_p (0) / (soil_water.q_p (0) + soil_water.q (0));
          solute.J_p[0] = J_in * macro_fraction;
          solute.J[0] = J_in - solute.J_p[0];
        }
    }
  else
    solute.J[0] = J_in;

  // Flow.
  flow (soil, soil_water, solute.submodel, 
        solute.M_, solute.C_, 
        solute.S_, solute.S_p_,
        solute.J, solute.J_p, 
        *solute.adsorption, solute.diffusion_coefficient (), dt, msg);
}

void 
Movement1D::element (const Soil& soil, 
                     const SoilWater& soil_water, 
                     Element& element,
                     Adsorption& adsorption,
                     const double diffusion_coefficient,
                     const double dt,
                     Treelog& msg)
{
  element.tick (geo->cell_size (), soil_water, dt);
  flow (soil, soil_water, "DOM", element.M, element.C, 
        element.S, element.S_p, element.J, element.J_p, 
        adsorption, diffusion_coefficient, dt, msg);
}

void 
Movement1D::flow (const Soil& soil, const SoilWater& soil_water, 
                  const std::string& name,
                  std::vector<double>& M, 
                  std::vector<double>& C, 
                  std::vector<double>& S, 
                  std::vector<double>& S_p, 
                  std::vector<double>& J, 
                  std::vector<double>& J_p, 
                  Adsorption& adsorption,
                  double diffusion_coefficient,
                  const double dt,
                  Treelog& msg)
{
  const double old_content = geo->total_surface (M);

  // Flow.
  if (adsorption.full ())
    {
      transport_solid->tick (msg, *geo, soil, soil_water, adsorption, 
                             diffusion_coefficient, M, C, S, J, dt);
      goto done;
    }

  mactrans->tick (*geo, soil_water, M, C, S, S_p, J_p, dt, msg);

  for (size_t m = 0; m < matrix_solute.size (); m++)
    {
      Treelog::Open nest (msg, matrix_solute[m]->name);

      try
        {
          matrix_solute[m]->tick (msg, *geo, soil, soil_water, adsorption, 
                                  diffusion_coefficient, 
                                  M, C, S, J, dt);
          if (m > 0)
            msg.message ("Reserve model succeeded");
          goto done;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("Transport problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("Transport trouble: ") + error);
        }
    }
  throw "Water matrix transport failed"; 
 done:
  const double new_content = geo->total_surface (M);
  const double delta_content = new_content - old_content;
  const double source = geo->total_surface (S) * dt;
  const double in = -J[0] * dt;	// No preferential transport, it is 
  const double out = -J[geo->edge_size () - 1] * dt; // included in S.
  const double expected = source + in - out;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10
      )
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
          << ": mass balance new - old != source + in - out\n"
          << new_content << " - " << old_content << " != " 
          << source << " + " << in << " - " << out << " (error "
          << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
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
  const double Z = -geo->z (0) / 100.0; // [cm] -> [m]
  const double T_soil = soil_heat.T (0); // [dg C]

  return (K_soil / Z * T_soil + K_snow / dZs * T_snow) 
    / (K_soil / Z + K_snow / dZs);
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
Movement1D::calculate_heat_flux (const Geometry& geo,
                                 const Soil& soil,
                                 const SoilWater& soil_water,
                                 const std::vector<double>& T_old,
                                 double T_prev,
                                 const std::vector<double>& T,
                                 const double T_bottom,
                                 std::vector<double>& q)
{
  // Top and inner cells.
  double z_prev = 0.0;
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta = soil_water.Theta (i);
      const double X_ice = soil_water.X_ice (i);
      const double K = soil.heat_conductivity (i, Theta, X_ice);
      const double T_next = (T[i] + T_old[i]) / 2.0;
      const double dT = T_prev - T_next;
      const double dz = z_prev - geo.z (i);
      const double q_water = soil_water.q (i);
      const double T = (T_prev + T_next) / 2.0;

      q[i] = - K * dT/dz + water_heat_capacity * rho_water *  q_water * T;
      T_prev = T_next;
      z_prev = geo.z (i);
    }
  // Lower boundary.
  const unsigned int i = soil.size ();
  const unsigned int prev = i - 1U;
  const double Theta = soil_water.Theta (prev);
  const double X_ice = soil_water.X_ice (prev);
  const double K = soil.heat_conductivity (prev, Theta, X_ice);
  const double T_next = T_bottom;
  const double dT = T_prev - T_next;
  const double dz = geo.z (prev-1U) - geo.z (prev);
  const double q_water = soil_water.q (i);
  const double my_T = (T_prev + T_next) / 2.0;

  q[i] = - K * dT/dz - water_heat_capacity * rho_water *  q_water * my_T;
}

void 
Movement1D::solve_heat (const Geometry1D& geo,
                        const Soil& soil,
                        const SoilWater& soil_water,
                        SoilHeat& soil_heat,
                        const Surface& surface,
                        const double T_bottom,
                        const double dt)
{
  double& T_top = soil_heat.T_top;
  std::vector<double>& T = soil_heat.T_;
  std::vector<double>& q = soil_heat.q;
  const std::vector<SoilHeat::state_t>& state = soil_heat.state;
  const std::vector<double>& S = soil_heat.S;
  const std::vector<double>& T_old = soil_heat.T_old;

  // Border conditions.
  const double T_top_new = surface.temperature ();

  if (T_top < -400.0)
    T_top = T_top_new;

  int size = soil.size ();

  // Tridiagonal matrix.
  std::vector<double> a (size, 0.0);
  std::vector<double> b (size, 0.0);
  std::vector<double> c (size, 0.0);
  std::vector<double> d (size, 0.0);

  // Inner cells.
  for (int i = 0; i < size; i++)
    {
      // Soil Water
      const double Theta = soil_water.Theta (i);
      const double X_ice = soil_water.X_ice (i);

      const int prev = i - 1;
      const int next = i + 1;

      // Calculate average heat capacity and conductivity.
      const double conductivity = soil.heat_conductivity (i, Theta, X_ice);

      // Calculate distances.
      const double dz_next 
        = (i == size - 1)
        ? geo.z (i) - geo.z (prev)
        : geo.z (next) - geo.z (i);
      const double dz_prev 
        = (i == 0)
        ? geo.z (i) - 0.0
        : geo.z (i) - geo.z (prev);
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
      const double capacity 
        = soil_heat.capacity_apparent (soil, soil_water, i);

      // Setup tridiagonal matrix.
      a[i] = - conductivity / dz_both / dz_prev + Cx / 2.0 / dz_both;
      b[i] = capacity / dt
        + conductivity / dz_both * (1.0 / dz_next + 1.0 / dz_prev);
      c[i] = - conductivity / dz_both / dz_next - Cx / 2.0 / dz_both;
      const double x2 = dT_next / dz_next - dT_prev/ dz_prev;
      if (i == 0)
        d[i] = T[i] * capacity / dt
          + conductivity / geo.z (1) * (x2 + T_top_new / geo.z (0))
          + Cx * (T[1] - T_top + T_top_new) / (2.0 * geo.z (1));
      else
        d[i] = T[i] * capacity / dt + (conductivity / dz_both) * x2
          + Cx * dT_both / dz_both / 2.0;

      if (state[i] == SoilHeat::freezing || state[i] == SoilHeat::thawing)
        d[i] -= latent_heat_of_fussion * rho_water
          * (soil_water.q (i) - soil_water.q (next)) / geo.dz (i) / dt;

      // External heat source.
      d[i] += S[i];
    }
  d[size - 1] = d[size - 1] - c[size - 1] * T_bottom;
  tridia (0, size, a, b, c, d, T.begin ());
  double T_prev = (T_top + T_top_new) / 2.0;
  T_top = T_top_new;
  daisy_assert (T[0] < 50.0);

  calculate_heat_flux (geo, soil, soil_water, 
                       T_old, T_prev, T, T_bottom, q);
}

void 
Movement1D::tick_heat (const Geometry1D& geo,
                       const Soil& soil,
                       SoilWater& soil_water,
                       SoilHeat& soil_heat,
                       const Surface& surface,
                       const double T_bottom,
                       const double dt)
{
  // Update freezing and melting points.
  soil_heat.update_freezing_points (soil, soil_water);

  // Solve with old state.
  soil_heat.T_old = soil_heat.T_;
  solve_heat (geo, soil, soil_water, soil_heat, 
              surface, T_bottom, dt);

  // Check if ice is enabled.
  if (!soil_heat.enable_ice)
    return;

  // Update state according to new temperatures.
  const bool changed 
    = soil_heat.update_state (geo, soil, soil_water, soil_heat.T_, dt);

  if (changed)
    {
      // Solve again with new state.
      soil_heat.T_ = soil_heat.T_old;
      solve_heat (geo, soil, soil_water, soil_heat, surface, T_bottom, dt);

      // Check if state match new temperatures.
      const bool changed_again = soil_heat.check_state (soil, soil_heat.T_);

      if (changed_again)
        {
          // Force temperatures to match state.
          soil_heat.force_state (soil_heat.T_);
        }
    }
  // Update ice in water.
  soil_water.freeze (soil, soil_heat.freezing_rate);
}

void 
Movement1D::ridge (Surface& surface, const Soil& soil,
                   const SoilWater& soil_water,
                   const AttributeList& al)
{ surface.ridge (*geo, soil, soil_water, al); }

void 
Movement1D::tick (const Soil& soil, SoilWater& soil_water, SoilHeat& soil_heat,
                  Surface& surface, Groundwater& groundwater,
                  const Time& time, const Weather& weather, 
                  const double dt, Treelog& msg) 
{
  Treelog::Open nest (msg, "Movement: " + name.name ());

  tick_heat (*geo, soil, soil_water, soil_heat, surface, 
             bottom_heat (time, weather), dt);
  soil_water.tick (geo->cell_size (), soil, dt, msg);
  tick_water (*geo, soil, soil_heat, surface, groundwater, 
              soil_water.S_sum_, soil_water.h_old_, soil_water.Theta_old_,
              soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
              soil_water.q_, soil_water.q_p_,
              dt, msg);
}

void 
Movement1D::output (Log&) const
{ }

bool
Movement1D::check (Treelog&) const
{ return true; }

void 
Movement1D::initialize (Block& block, const AttributeList& al,
                        const Soil& soil, const Groundwater& groundwater)
{
  Treelog::Open nest (block.msg (), "Movement: " + name.name ());

  const size_t cell_size = geo->cell_size ();

  // Macropores.
  if (al.check ("macro"))
    macro.reset (Librarian<Macro>::build_alist (block, al.alist ("macro"), 
                                                "macro"));
  else if (soil.humus (0) + soil.clay (0) > 0.05)
    // More than 5% clay (and humus) in first horizon.
    {
      // Find first non-clay layer.
      size_t lay = 1;
      while (lay < cell_size && soil.humus (lay) + soil.clay (lay) > 0.05)
        lay++;

      // Don't go below 1.5 m.
      double height = std::max (geo->zplus (lay-1), -150.0);

      // Don't go below drain pipes.
      if (groundwater.is_pipe ())
        height = std::max (height, groundwater.pipe_height ());

      // Add them.
      macro = Macro::create (height);

      block.msg ().debug ("Adding macropores");
    }

  // Let 'macro' choose the default method to average K values in 'uz'.
  const bool has_macropores = (macro.get () && !macro->none ());
  for (size_t i = 0; i < matrix_water.size (); i++)
    matrix_water[i]->has_macropores (block, has_macropores);
}

Movement1D::Movement1D (Block& al)
  : Movement (al),
    geo (submodel<Geometry1D> (al, "Geometry")),
    matrix_water (Librarian<UZmodel>::build_vector (al, "matrix_water")),
    macro (NULL), 
    matrix_solute (Librarian<Transport>::build_vector (al, "matrix_solute")),
    transport_solid (Librarian<Transport>::build_item (al, "transport_solid")),
    mactrans  (Librarian<Mactrans>::build_item (al, "mactrans"))
{ }

Movement1D::~Movement1D ()
{
  sequence_delete (matrix_water.begin (), matrix_water.end ()); 
  sequence_delete (matrix_solute.begin (), matrix_solute.end ()); 
}

void 
Movement::load_vertical (Syntax& syntax, AttributeList& alist)
{
   syntax.add_submodule ("Geometry", alist, Syntax::State,
                         "Discretization of the soil.",
                         Geometry1D::load_syntax);
   syntax.add_object ("matrix_water", UZmodel::component, 
                      Syntax::Const, Syntax::Sequence,
                      "Vertical matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
   std::vector<AttributeList*> vertical_models;
   AttributeList vertical_default (UZmodel::default_model ());
   vertical_models.push_back (&vertical_default);
   AttributeList vertical_reserve (UZmodel::reserve_model ());
   vertical_models.push_back (&vertical_reserve);
   alist.add ("matrix_water", vertical_models);
   syntax.add_object ("matrix_solute", Transport::component, 
                      Syntax::Const, Syntax::Sequence,
                      "Vertical matrix solute transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
   std::vector<AttributeList*> transport_models;
   AttributeList transport_default (Transport::default_model ());
   transport_models.push_back (&transport_default);
   AttributeList transport_reserve (Transport::reserve_model ());
   transport_models.push_back (&transport_reserve);
   AttributeList transport_last_resort (Transport::none_model ());
   transport_models.push_back (&transport_last_resort);
   alist.add ("matrix_solute", transport_models);
   syntax.add_object ("transport_solid", Transport::component,
                      "Transport model for non-dissolvable chemicals.\n\
Should be 'none'.");
   alist.add ("transport_solid", Transport::none_model ());
   syntax.add_object ("macro", Macro::component,
                      Syntax::OptionalState, Syntax::Singleton,
                      "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
   syntax.add_object ("mactrans", Mactrans::component, 
                      "Solute transport model in macropores.");
   alist.add ("mactrans", Mactrans::default_model ());
}

const AttributeList& 
Movement::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      Movement::load_vertical (dummy, alist);
      alist.add ("type", "vertical");
    }
  return alist;
}

Movement*
Movement::build_vertical (Block& al)
{ return new Movement1D (al); }

static struct Movement1DSyntax
{
  static Model& make (Block& al)
  { return *new Movement1D (al); }

  Movement1DSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "One dimensional movement.");
    Movement::load_vertical (syntax, alist);
 
    Librarian<Movement>::add_type ("vertical", alist, syntax, &make);
  }
} Movement1D_syntax;
