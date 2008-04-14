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
#include "macro.h"
#include "groundwater.h"
#include "surface.h"
#include "weather.h"
#include "chemical.h"
#include "doe.h"
#include "transport.h"
#include "mactrans.h"
#include "adsorption.h"
#include "log.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "transport.h"
#include <sstream>

static const double rho_water = 1.0; // [g/cm^3]

struct Movement1D : public MovementSolute
{
  // Geometry.
  std::auto_ptr<Geometry1D> geo;
  Geometry& geometry () const;

  // Water.
  const auto_vector<UZmodel*> matrix_water;
  std::auto_ptr<Macro> macro;
  void macro_tick (const Soil&, SoilWater&,
                   Surface&, const double dt, Treelog&);
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
  std::auto_ptr<Mactrans> mactrans; // Solute transport model in macropores.

  // Heat.
  /* const */ double delay;	// Period delay [ cm/rad ??? ]
  double T_bottom;		// [dg C]
  double surface_snow_T (const Soil& soil,
                         const SoilWater& soil_water,
                         const SoilHeat& soil_heat,
                         const double T_snow,
                         const double K_snow,
                         const double dZs) const;
  double bottom_heat (const Time& time, const Weather& weather) const ;
  double bottom_T () const;
  std::vector<double> default_heat (const Soil& soil, 
                                    const Time& time, const Weather& weather);
  static void solve_heat (const Geometry1D& geo,
			  const std::vector<double>& q_water,
			  const std::vector<double>& S_water,
			  const std::vector<double>& S_heat,
			  const std::vector<double>& capacity_old,
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
	     const std::vector<double>& capacity_old,
	     const std::vector<double>& capacity_new,
	     const std::vector<double>& conductivity,
	     double T_top,
	     double T_top_new,
	     std::vector<double>& T,
	     const double dt, Treelog&) const;


  // Management.
  void ridge (Surface& surface, const Soil& soil, const SoilWater& soil_water,
              const AttributeList& al);

  // Simulation.
  void tick (const Soil& soil, SoilWater& soil_water, const SoilHeat& soil_heat,
             Surface& surface, Groundwater& groundwater,
             const Time& time, const Weather& weather, double dt, 
             Treelog& msg);
  void output (Log&) const;

  // Create.
  bool check (Treelog& err) const;
  void initialize (const Soil& soil, const Groundwater& groundwater,
		   Treelog&);
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

  // Edges.
  const size_t edge_size = geo->edge_size ();
  std::vector<double> q_p (edge_size, 0.0);

  // Cells.
  const size_t cell_size = geo->cell_size ();
  std::vector<double> S_p (cell_size, 0.0);
  std::vector<double> h_ice (cell_size);
  std::vector<double> h (cell_size);
  std::vector<double> Theta (cell_size);
  std::vector<double> S_sum (cell_size);
  
  for (size_t c = 0; c < cell_size; c++)
    {
      h_ice[c] = soil_water.h_ice (c);
      h[c] = soil_water.h (c);
      Theta[c] = soil_water.Theta (c);
      S_sum[c] = soil_water.S_sum (c);
    }

  // Calculate and update.
  macro->tick (*geo, soil, 0, soil.size () - 1, surface, 
               h_ice, h, Theta, S_sum, S_p, q_p, dt, msg);
  soil_water.set_tertiary (S_p, q_p);
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
  const size_t top_edge = 0U;
  const size_t bottom_edge = geo.edge_size () - 1U;
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
                                 first, surface, top_edge, 
                                 last, groundwater, bottom_edge,
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
          surface.update_pond_average (geo);
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

#if 0
void 
Movement1D::solute (const Soil& soil, 
                    const SoilWater& soil_water, 
                    const double J_in, Chemical& solute, 
		    const bool flux_below, 
                    const double dt,
		    const Scope& scope,
                    Treelog& msg)
{ 
  Treelog::Open nest (msg, "Solute: " + solute.name);

  const size_t cell_size = geo->cell_size ();
  const size_t edge_size = geo->edge_size ();

  // Content.
  std::vector<double> M (cell_size);
  std::vector<double> C (cell_size);
  std::vector<double> S (cell_size);
  std::vector<double> S_p (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      M[c] = solute.M (c);
      C[c] = solute.C (c);
      S[c] = solute.S (c);
      S_p[c] = solute.S_p (c);
    }
  
  // Fluxes.
  std::vector<double> J (edge_size, 0.0);
  std::vector<double> J_p (edge_size, 0.0);

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
          J[0] = J_in;
          J_p[0] = J_in;
        }
      else
        {
          const double macro_fraction
            = soil_water.q_p (0) / (soil_water.q_p (0) + soil_water.q (0));
	  J_p[0] = J_in * macro_fraction;
          J[0] = J_in - J_p[0];
        }
    }
  else
    J[0] = J_in;
  
  // Flow.
  if (solute.adsorption ().full ())
    transport_solid->tick (msg, *geo, soil, soil_water, solute.adsorption (),
			   solute.diffusion_coefficient (),
			   M, C, S, J, solute.C_below (), dt);
  else
    flow (soil, soil_water, solute.adsorption (), solute.name, 
	  M, C, S, S_p, J, J_p, 
	  solute.C_below (), solute.diffusion_coefficient (),
	  dt, msg);
  
  for (size_t c = 0; c < cell_size; c++)
    solute.set_content (c, M[c], C[c]);

  for (size_t e = 0; e < edge_size; e++)
    {
      solute.set_matrix_flux (e, J[e]);
      solute.set_macro_flux (e, J_p[e]);
    }
}

void 
Movement1D::element (const Soil& soil, 
                     const SoilWater& soil_water, 
                     DOE& element,
                     const double diffusion_coefficient,
                     const double dt,
                     Treelog& msg)
{
  element.tick (geo->cell_size (), soil_water, dt);
  static const symbol DOM_name ("DOM");
  flow (soil, soil_water, Adsorption::none (), DOM_name, element.M, element.C, 
        element.S, element.S_p, element.J, element.J_p, 0.0, 
	diffusion_coefficient, dt, msg);
}

void 
Movement1D::flow (const Soil& soil, const SoilWater& soil_water, 
                  const Adsorption& adsorption, const symbol name,
                  std::vector<double>& M, 
                  std::vector<double>& C, 
                  std::vector<double>& S, 
                  std::vector<double>& S_p, 
                  std::vector<double>& J, 
                  std::vector<double>& J_p, 
		  const double C_below,
                  double diffusion_coefficient,
                  const double dt,
                  Treelog& msg) const
{
  const double old_content = geo->total_surface (M);

  mactrans->tick (*geo, soil_water, M, C, S, S_p, J_p, dt, msg);

  for (size_t m = 0; m < matrix_solute.size (); m++)
    {
      Treelog::Open nest (msg, matrix_solute[m]->name);

      try
        {
          matrix_solute[m]->tick (msg, *geo, soil, soil_water, adsorption,
				  diffusion_coefficient, 
                                  M, C, S, J, C_below, dt);
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
#endif

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

double
Movement1D::bottom_T () const
{ return T_bottom; }

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
			const std::vector<double>& /* capacity_old */,
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
      if (i == 0)
        d[i] = T[i] * capacity_cell / dt
          + conductivity_cell / geo.z (1) * (x2 + T_top_new / geo.z (0))
          + Cx * (T[1] - T_top + T_top_new) / (2.0 * geo.z (1));
      else
        d[i] = T[i] * capacity_cell / dt + (conductivity_cell / dz_both) * x2
          + Cx * dT_both / dz_both / 2.0;

      // External heat source + thawing/freezing.
      d[i] += S[i];
    }
  d[size - 1] = d[size - 1] - c[size - 1] * T_bottom;
  tridia (0, size, a, b, c, d, T.begin ());
  daisy_assert (T[0] < 50.0);
}

void 
Movement1D::heat (const std::vector<double>& q_water,
		  const std::vector<double>& S_water,
		  const std::vector<double>& S_heat,
		  const std::vector<double>& capacity_old,
		  const std::vector<double>& capacity_new,
		  const std::vector<double>& conductivity,
		  const double T_top,
		  const double T_top_new,
		  std::vector<double>& T,
		  const double dt, Treelog&) const
{
  solve_heat (*geo, q_water, S_water, S_heat, 
	      capacity_old, capacity_new, conductivity,
	      T_top, T_top_new, T_bottom, T, dt);
}

void 
Movement1D::ridge (Surface& surface, const Soil& soil,
                   const SoilWater& soil_water,
                   const AttributeList& al)
{ surface.ridge (*geo, soil, soil_water, al); }

void 
Movement1D::tick (const Soil& soil, SoilWater& soil_water, 
		  const SoilHeat& soil_heat,
                  Surface& surface, Groundwater& groundwater,
                  const Time& time, const Weather& weather, 
                  const double dt, Treelog& msg) 
{
  const size_t edge_size = geo->edge_size ();
  const size_t cell_size = geo->cell_size ();

  Treelog::Open nest (msg, "Movement: " + name.name ());

  T_bottom = bottom_heat (time, weather);
  soil_water.tick (cell_size, soil, dt, msg);

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
Movement1D::output (Log&) const
{ }

bool
Movement1D::check (Treelog& msg) const
{ return check_solute (msg); }

void 
Movement1D::initialize (const Soil& soil, const Groundwater& groundwater,
			Treelog& msg)
{
  Treelog::Open nest (msg, "Movement: " + name.name ());

  const size_t cell_size = geo->cell_size ();

  // Macropores.
  if (macro.get ())
    /* Already got them. */;
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

      msg.debug ("Adding macropores");
    }

  // Let 'macro' choose the default method to average K values in 'uz'.
  const bool has_macropores = (macro.get () && !macro->none ());
  for (size_t i = 0; i < matrix_water.size (); i++)
    matrix_water[i]->has_macropores (has_macropores);
}

Movement1D::Movement1D (Block& al)
  : MovementSolute (al),
    geo (submodel<Geometry1D> (al, "Geometry")),
    matrix_water (Librarian::build_vector<UZmodel> (al, "matrix_water")),
    macro (al.check ("macro")
	   ? Librarian::build_item<Macro> (al, "macro")
	   : NULL), 
    mactrans  (Librarian::build_item<Mactrans> (al, "mactrans")),
    T_bottom (-42.42e42)
{ }

Movement1D::~Movement1D ()
{ }

void 
Movement::load_vertical (Syntax& syntax, AttributeList& alist)
{
  MovementSolute::load_solute (syntax, alist, Transport::vertical_model ());
  syntax.add_submodule ("Geometry", alist, Syntax::State,
                        "Discretization of the soil.",
                        Geometry1D::load_syntax);
  syntax.add_object ("matrix_water", UZmodel::component, 
                     Syntax::Const, Syntax::Sequence,
                     "Vertical matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
  std::vector<const AttributeList*> vertical_models;
  AttributeList vertical_default (UZmodel::default_model ());
  vertical_models.push_back (&vertical_default);
  AttributeList vertical_reserve (UZmodel::reserve_model ());
  vertical_models.push_back (&vertical_reserve);
  alist.add ("matrix_water", vertical_models);
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
 
    Librarian::add_type (Movement::component, "vertical", alist, syntax, &make);
  }
} Movement1D_syntax;

// movement_1D.C ends here.
