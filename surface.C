// surface.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "surface.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include "plf.h"
#include "ridge.h"
#include "check.h"
#include "treelog.h"
#include "frame_submodel.h"
#include <sstream>
#include <map>

struct Surface::Implementation
{
  // Content.
  const double temperature_change_rate; // [h^-1]
  const double EpFactor;		// []
  const PLF EpFactor_SWE;	// [pF] -> []
  double EpFactor_current;	// []
  const double albedo_wet;
  const double albedo_dry;
  const bool use_forced_pressure;
  const double forced_pressure_value;
  const bool use_forced_flux;
  const double forced_flux_value;
  typedef std::map<size_t, size_t> pond_map;
  pond_map pond_edge;
  double pond_average;          // [mm]
  double pond_max;              // [mm]
  std::vector<double> pond_section; // [mm]
  double EvapSoilSurface;       // [mm/h]
  double Eps;                   // [mm/h]
  double T;                     // [dg C]
  double DetentionCapacity;     // [mm]
  const double ReservoirConstant; // [h^-1]
  const double LocalDetentionCapacity; // [mm]
  double runoff;                // [mm/h]
  double runoff_rate;          // [h^-1]
  const double R_mixing;
  const double z_mixing;
  std::unique_ptr<Ridge> ridge_;
  
  // UZ top.
  Surface::top_t top_type (const Geometry& geo, size_t edge) const;
  double q_top (const Geometry& geo, size_t edge, const double dt) const;

  // Functions.
  void ridge (const Geometry1D& geo,
              const Soil& soil, const SoilWater& soil_water,
	      const FrameSubmodel&);
  void exfiltrate (const Geometry& geo, size_t edge,
                   double water, double dt, Treelog&);
  void update_pond_average (const Geometry& geo);
  void tick (Treelog&, double PotSoilEvaporationWet,
             double PotSoilEvaporationDry,
             double water, double temp,
	     const Geometry&, const Soil&, const SoilWater&,
             double T, double dt);
  double albedo (const Geometry&, const Soil&, const SoilWater&) const;
  void output (Log& log) const;
  double exfiltration (double dt) const; // [mm/h]
  

  // Create and Destroy.
  void initialize (const Geometry&);
  Implementation (const FrameSubmodel& al);
  ~Implementation ();
};

Surface::top_t 
Surface::top_type (const Geometry& geo, size_t edge) const
{ return impl->top_type (geo, edge); }

Surface::top_t 
Surface::Implementation::top_type (const Geometry& geo, size_t edge) const
{
  if (use_forced_flux)
    return forced_flux;

  if (ridge_.get ())
    return soil;

  if (use_forced_pressure)
    return forced_pressure;
  
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  pond_map::const_iterator i = pond_edge.find (edge);
  daisy_assert (i != pond_edge.end ());
  const size_t c = (*i).second;
  if (pond_section[c] <= 0.0)
    return forced_flux;
  
  return limited_water;
}

double 
Surface::q_top (const Geometry& geo, const size_t edge,
                const double dt) const
{ return impl->q_top (geo, edge, dt); }
  
double 
Surface::Implementation::q_top (const Geometry& geo, const size_t edge,
                                const double dt) const
{
  if (use_forced_pressure)
    return forced_pressure_value * 0.1 / 1.0; // mm -> cm/h.

  if (use_forced_flux)
    return forced_flux_value * 0.1; // mm/h -> cm/h.

  if (ridge_.get ())
    return -ridge_->h () / 1.0 /* [h] */;

  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  pond_map::const_iterator i = pond_edge.find (edge);
  daisy_assert (i != pond_edge.end ());
  const size_t c = (*i).second;
  daisy_assert (pond_section[c] < std::max (1000.0, 10.0 * DetentionCapacity));
  daisy_assert (pond_section[c] > -1000.0);
  return -pond_section[c] * 0.1 / dt /* [h] */; // mm -> cm/h.
}
  
double
Surface::h_top (const Geometry& geo, size_t edge) const
{ 
  const double dt = 1.0;       // [h]
  return -q_top (geo, edge, dt) * dt; 
}

void
Surface::accept_top (double water /* [cm] */,
                     const Geometry& geo, size_t edge,
                     double dt, Treelog& msg)
{ 
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);

  if (impl->ridge_.get ())
    return;		// Handled by ridge based on flux.

  impl->exfiltrate (geo, edge, water * 10.0, dt, msg); 
}

size_t 
Surface::last_cell (const Geometry& geo, size_t edge) const 
{ 
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  daisy_assert (edge == 0);
  daisy_assert (impl->ridge_.get ());
  return impl->ridge_->last_cell ();
}

void
Surface::update_water (const Geometry1D& geo,
                       const Soil& soil,
		       const std::vector<double>& S_,
		       std::vector<double>& h_,
		       std::vector<double>& Theta_,
		       std::vector<double>& q,
		       const std::vector<double>& q_p, 
                       const double dt)
{
  if (impl->ridge_.get ())
    impl->ridge_->update_water (geo, soil, S_, h_, Theta_, q, q_p, dt); 
}

void 
Surface::ridge (const Geometry1D& geo,
                const Soil& soil, const SoilWater& soil_water, 
		const FrameSubmodel& al)
{ impl->ridge (geo, soil, soil_water, al); }

void 
Surface::Implementation::ridge (const Geometry1D& geo,
                                const Soil& soil, const SoilWater& soil_water,
				const FrameSubmodel& al)
{
  // No permanent ponding.
  daisy_assert (!use_forced_pressure);

  // Create new ridge system.
  ridge_.reset (new Ridge (al));
  ridge_->initialize (geo, soil, soil_water);
}

void 
Surface::unridge ()
{ 
  impl->ridge_.reset (NULL);
}

void
Surface::Implementation::exfiltrate (const Geometry& geo, const size_t edge,
                                     const double water /* [mm] */, 
                                     const double dt /* [h] */, Treelog& msg)
{
  if (use_forced_pressure)
    return;

  const size_t c = pond_edge[edge];
#ifdef NO_NEGATIVE_POND
  if (pond_section[c] + water < 0.0)
    // - std::max (fabs (pond_edge[edge]), fabs (water)) / 100.0)
    {
      if (!approximate (fabs (pond_section[c]), fabs (water), 0.001))
      {
        Treelog::Open nest (msg, 
                            "Surface exfiltration for edge " 
                            + geo.edge_name (edge));
        
        std::ostringstream tmp;
        tmp << "edge " << geo.edge_name (edge) << ": pond (" << pond_section[c]
            << ") + exfiltration (" << water << ") in dt (" << dt << ") = " 
            << pond_section[c] + water << ", should be non-negative";
        msg.warning (tmp.str ());
      }
      pond_section[c] = 0.0;
      return;
    }
#endif
  
  daisy_assert (pond_section[c] < std::max(1000.0, 10.0 * DetentionCapacity));
  daisy_assert (pond_section[c] > -1000.0);
  pond_section[c] += water;
  daisy_assert (pond_section[c] < std::max (1000.0, 10.0 * DetentionCapacity));
  daisy_assert (pond_section[c] > -1000.0);
}

double
Surface::ponding_average () const
{ return impl->pond_average; }

double
Surface::ponding_max () const
{ return impl->pond_max; }

double
Surface::runoff_rate () const
{ return impl->runoff_rate; }

double
Surface::mixing_resistance () const
{ return impl->R_mixing; }

double
Surface::mixing_depth () const
{ return impl->z_mixing; }

double
Surface::temperature () const
{ return impl->T; }

void
Surface::tick (Treelog& msg,
	       const double PotSoilEvaporationWet, 
	       const double PotSoilEvaporationDry, 
               const double flux_in, const double temp,
	       const Geometry& geo,
               const Soil& soil, const SoilWater& soil_water, 
               const double soil_T,
               const double dt)
{ impl->tick (msg, PotSoilEvaporationWet, PotSoilEvaporationDry,
              flux_in, temp, geo, 
             soil, soil_water, soil_T, dt); }

void
Surface::update_pond_average (const Geometry& geo)
{ impl->update_pond_average (geo); }

void
Surface::Implementation::update_pond_average (const Geometry& geo)
{
  const std::vector<size_t>& top_edges = geo.cell_edges (Geometry::cell_above);
  const size_t top_edges_size = top_edges.size ();

  // Find total pond.
  double total_pond = 0.0;      // [mm cm^2]
  double total_area = 0.0;      // [cm^2]
  pond_max = 0.0;
  for (int i = 0; i < top_edges_size; i++)
    {
      const size_t edge = top_edges[i];
      const size_t c = pond_edge[edge];
      const double area = geo.edge_area (edge);
      total_area += area;
      total_pond += pond_section[c] * area;
      if (pond_section[c] > pond_max)
        pond_max = pond_section[c];
    }
  daisy_approximate (total_area, geo.surface_area ());
  pond_average = total_pond / total_area; // [mm];
}

void
Surface::Implementation::tick (Treelog& msg,
			       const double PotSoilEvaporationWet,
			       const double PotSoilEvaporationDry,
			       const double flux_in, const double temp,
                               const Geometry& geo,
                               const Soil& soil, const SoilWater& soil_water,
			       const double soil_T,
                               const double dt)
{
  // Runoff out of field.
  const double old_pond_average = pond_average;
  runoff = 0.0;
  runoff_rate = 0.0;
  double total_area = 0.0;
  for (pond_map::iterator i = pond_edge.begin ();
       i != pond_edge.end ();
       i++)
    {
      const double runoff_speed = // [h^-1]
        (ReservoirConstant * dt > 1.0)
        ? 1.0 / dt
        : ReservoirConstant;
      const size_t edge = (*i).first;
      const size_t c = (*i).second;
      const double area = geo.edge_area (edge);
      total_area += area;
      if (pond_section[c] > DetentionCapacity)
        {
          const double runoff_section // [mm/h]
            = (pond_section[c] - DetentionCapacity) * runoff_speed;
          runoff += area * runoff_section; // [A mm/h]
          runoff_rate +=          // [A h^-1]
            area * runoff_section / pond_section[c]; 
          pond_section[c] -= runoff_section * dt; // [mm]
        }
    }
  runoff /= total_area;         // [mm]
  runoff_rate /= total_area; // [h^-1]
  update_pond_average (geo);
  daisy_balance (old_pond_average, pond_average, -runoff * dt);

  // Runoff internal.
  const double local_pond_average = pond_average;
  if (pond_average > LocalDetentionCapacity)
    {
      for (pond_map::iterator i = pond_edge.begin ();
           i != pond_edge.end ();
           i++)
        {
          const size_t c = (*i).second;
          pond_section[c] = pond_average;
        }
      update_pond_average (geo);
    }
  else
    {
      // Any sections above local capacity?
      double extra = 0.0;       // [mm cm^2] 
      double total_area = 0.0;        // [cm^2]
      double free_area = 0.0;         // [cm^2]
      const double epsilon = 1e-9; // [mm]
      for (pond_map::iterator i = pond_edge.begin ();
           i != pond_edge.end ();
           i++)
        {
          const size_t edge = (*i).first;
          const size_t c = (*i).second;
          const double area = geo.edge_area (edge);
          total_area += area;
          if (pond_section[c] > LocalDetentionCapacity + epsilon)
            {
              extra += area * (pond_section[c] - LocalDetentionCapacity);
              pond_section[c] = LocalDetentionCapacity;
            }
          else if (pond_section[c] + epsilon < LocalDetentionCapacity)
            free_area += area;
        }
      update_pond_average (geo);
      daisy_balance (local_pond_average, pond_average, -extra / total_area);

      // Redistribute.
      while (extra > epsilon * total_area && free_area > 1e-9)
        {
          const double old_extra = extra;
          const double fill = extra / free_area;
          free_area = 0.0;
          for (pond_map::iterator i = pond_edge.begin ();
               i != pond_edge.end ();
               i++)
            {
              const size_t edge = (*i).first;
              const size_t c = (*i).second;
              const double area = geo.edge_area (edge);
              
              if (pond_section[c] + fill + epsilon < LocalDetentionCapacity)
                {
                  extra -= fill * area;
                  pond_section[c] += fill;
                  free_area += area;
                }
              else if (pond_section[c] < LocalDetentionCapacity)
                {
                  extra -= (LocalDetentionCapacity - pond_section[c]) * area;
                  pond_section[c] = LocalDetentionCapacity;
                }
            }
          daisy_assert (extra < old_extra);
        }
    }
  update_pond_average (geo);
  daisy_approximate (local_pond_average, pond_average);
  
  // Remember potential evaporation
  Eps = PotSoilEvaporationWet;

  double EvapSoilTotal = 0.0;   // [mm cm^2/h]

  // Update pond above each top cell.
  const std::vector<size_t>& top_edges = geo.cell_edges (Geometry::cell_above);
  const size_t top_edges_size = top_edges.size ();
  for (size_t i = 0; i < top_edges_size; i++)
    {
      const size_t edge = top_edges[i];
      const size_t c = pond_edge[edge];
      const double area = geo.edge_area (edge); // [cm^2]

      // Exfiltration.
      const double MaxExfiltration // [mm/h]
        = bound (0.0, 
                 soil_water.MaxExfiltration (geo, edge, soil, soil_T) * 10.0,
                 PotSoilEvaporationDry); 

      const double epond = pond_section[c]; // [mm]

#ifdef NO_NEGATIVE_POND
      if (epond < 0.0)
        {
          std::ostringstream tmp;
          tmp << "pond_edge[" << edge << "] = "<< pond_section[c];
          msg.warning (tmp.str ());
        }
#endif
      double evap;              // [mm/h]
      if (epond + flux_in * dt + MaxExfiltration * dt < Eps * dt)
        evap = epond / dt + flux_in + MaxExfiltration;
      else
        evap = Eps;

      EvapSoilTotal += evap * area;    // [mm cm^2/h]

      pond_section[c] += flux_in * dt - evap * dt;
      daisy_assert (pond_section[c]
                    < std::max (1000.0, 10.0 * DetentionCapacity));
      daisy_assert (pond_section[c] > -1000.0);
      daisy_assert (evap < 1000.0);

      if (ridge_.get ())
        {
          const Geometry1D& geo1d = dynamic_cast<const Geometry1D&> (geo);
          ridge_->tick (geo1d, soil, soil_water, pond_section[c], dt);
          exfiltrate (geo, edge, ridge_->exfiltration (), dt, msg);
        }
    }
  EvapSoilSurface = EvapSoilTotal / geo.surface_area (); // [mm/h]
  update_pond_average (geo);

  // Temperature
  double new_T = T;
  if (old_pond_average < 1e-6)
    new_T = temp;
  else if (flux_in < 0.0)
    {
      if (old_pond_average - EvapSoilSurface * dt + flux_in * dt < 1e-6)
        new_T = temp;
      // else use old temperature.
    }
  else
    new_T = (T * old_pond_average + temp * flux_in * dt)
      / (old_pond_average + flux_in * dt);

  // Slow down changes to surface temperature.
  if (temperature_change_rate > 0.0)
    T += (new_T - T) * std::min (temperature_change_rate * dt, 1.0);
  else 
    T = new_T;

  daisy_assert (T > -100.0 && T < 50.0);

  // Adjust EpFactor after soil water.
  const double h
    = geo.content_hood (soil_water, &SoilWater::h, Geometry::cell_above);
  EpFactor_current = (h < 0.0)
    ? EpFactor * EpFactor_SWE (h2pF (h))
    : EpFactor;
}

double 
Surface::EpFactor () const
{ return impl->EpFactor_current; }

double
Surface::albedo (const Geometry& geo, 
                 const Soil& soil, const SoilWater& soil_water) const
{ return impl->albedo (geo, soil, soil_water); }

double
Surface::Implementation::albedo (const Geometry& geo, const Soil& soil,
				 const SoilWater& soil_water) const
{ 
  double Theta_pf_3 = 0.0;
  double Theta_pf_1_7 = 0.0;
  double Theta = 0.0; 
  double volume = 0.0;
  
  const size_t cell_size = geo.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    if (geo.contain_z (i, 0.0))
      {
        const double v = geo.cell_volume (i);
        volume += v;
        Theta_pf_3 += soil_water.Theta_ice (soil, i, pF2h (3.0)) * v;
        Theta_pf_1_7 += soil_water.Theta_ice (soil, i, pF2h (1.7)) * v;
        Theta += soil_water.Theta (i) * v;
      }
  daisy_assert (volume > 0.0);
  Theta_pf_3 /= volume;
  Theta_pf_1_7 /= volume;
  Theta /= volume;

  daisy_assert (Theta_pf_1_7 >= Theta_pf_3);

  if (Theta <= Theta_pf_3)
    return albedo_dry;
  if (Theta > Theta_pf_1_7)
    return albedo_wet;

  if (approximate (Theta_pf_1_7, Theta_pf_3))
    return albedo_wet;

  return albedo_dry + (albedo_wet - albedo_dry)
    * (Theta - Theta_pf_3) / (Theta_pf_1_7 - Theta_pf_3);
}

void
Surface::set_detention_capacity (const double height)
{ impl->DetentionCapacity = height; }

void
Surface::output (Log& log) const
{ impl->output (log); }

void
Surface::Implementation::output (Log& log) const
{
  output_variable (T, log);
  output_value (pond_average, "pond", log);
  output_variable (pond_section, log);
  output_variable (EvapSoilSurface, log);
  output_variable (Eps, log);
  output_variable (runoff, log);
  if (ridge_.get ())
    output_submodule (*ridge_, "ridge", log);
}

double
Surface::exfiltration (const double dt) const // [mm/h]
{ return impl->exfiltration (dt); }

double
Surface::Implementation::exfiltration (const double dt) const // [mm/h]
{
  // Negative pond == amount extracted from soil.
  if (pond_average < 0.0)
    return -pond_average / dt;
  else
    return 0.0;
}

double
Surface::evap_soil_surface () const // [mm/h]
{ return impl->EvapSoilSurface; }

double 
Surface::evap_pond (const double dt, Treelog& msg) const	// [mm/h]
{ 
  const double ep = evap_soil_surface () - exfiltration (dt); 

  if (ep >= 0.0)
    return ep;
  if (ep < -1e-5)		// ca = 0.1 mm/y
    {
      Treelog::Open nest (msg, "Surface evap pond");
      std::ostringstream tmp;
      tmp << "evap_pond = " << ep << ", evap_soil_surface = " << evap_soil_surface () << ", exfiltration = " << exfiltration (dt);
      msg.warning (tmp.str ());
      static int wcount = 10;
      if (--wcount < 0)
        throw "Too many evap pond errors, giving up";
    }
  return 0.0;
}

void
Surface::put_ponding (double p)	// [mm]
{ 
  for (Implementation::pond_map::iterator i = impl->pond_edge.begin ();
       i != impl->pond_edge.end ();
       i++)
    {
      const size_t c = (*i).second;
      impl->pond_section[c] = p;
    }

  impl->pond_average = p; 
}

void
Surface::set_svat_temperature (double T_surface /* dg C */)
{ 
  if (impl->pond_average < 1e-5)
    impl->T = T_surface;
}
  

static bool
check_alist (const Metalib&, const Frame& al, Treelog& msg)
{
  bool ok = true;

  if (al.check ("forced_flux") && al.check ("forced_pressure"))
    {
      msg.error ("Can't have both 'forced_pressure' and 'forced_flux'");
      ok = false;
    }

  return ok;
}

void 
Surface::initialize (const Geometry& geo)
{ impl->initialize (geo); }

void 
Surface::Implementation::initialize (const Geometry& geo)
{ 
  const std::vector<size_t>& top_edges = geo.cell_edges (Geometry::cell_above);
  const size_t top_edges_size = top_edges.size ();

  for (size_t i = 0; i < top_edges_size; i++)
    {
      const size_t edge = top_edges[i];
      pond_edge[edge] = i;
      if (pond_section.size () <= i)
        pond_section.push_back (0.0);
    }
  update_pond_average (geo);
}

void
Surface::load_syntax (Frame& frame)
{
  frame.add_check (check_alist);
  frame.declare ("temperature_change_rate", "h^-1",
                 Attribute::Const,
                 "Relative change of surface temperature.\n\
If you set this to a negative value, surface temperature will\n\
reflect temperature of incomming water or ponded water, if any,\n\
snow, if any, or air temperature.\n\
Set this parameter to a positive value to dampen changes.\n\
Especially useful if you feed the model with daily weather data\n\
and average temperature.");
  frame.set ("temperature_change_rate", 0.5);
  frame.declare ("EpFactor", Attribute::None (), Check::non_negative (), 
	      Attribute::Const,
	      "Convertion of reference evapotranspiration to\n\
potential evaporation for bare soil.");
  frame.set_cited ("EpFactor", 0.6, "\
See figure 4 in the cited paper.\n\
\n\
The autumn value can be lower, due to muching.  With a crop factor of\n\
1.2 a combined Kc of 1.15 is reached at LAI=5.",
                   "kjaersgaard2008crop");
  frame.declare ("EpFactor_SWE",
		 "pF", Attribute::None (), Attribute::Const, "\
Effect of soil water on EpFactor.");
  frame.set ("EpFactor_SWE", PLF::always_1 ());
  frame.declare ("albedo_dry", Attribute::None (), Check::non_negative (),
	      Attribute::Const,
	      "Albedo of dry soil (pF >= 3)");
  frame.set ("albedo_dry", 0.15);
  frame.declare ("albedo_wet", Attribute::None (), Check::non_negative (),
	      Attribute::Const,
	      "Albedo of wet soil (pf <= 1.7)");
  frame.set ("albedo_wet", 0.08);
    frame.declare ("forced_pressure", "mm", Attribute::OptionalConst, "\
Set this to force a permanent pressure top.");
  frame.declare ("forced_flux", "mm/h", Attribute::OptionalConst, "\
Set this to force a permanent flux top.  Positive upwards (exfiltration).");
  frame.declare ("pond", "mm", Attribute::LogOnly, "\
Amount of ponding on the surface.\n\
Negative numbers indicate soil exfiltration.");
  frame.declare ("pond_section", "mm",
                 Attribute::OptionalState, Attribute::Variable, "\
Amount of ponding on each section of the surface.\n\
By default, there will be no ponding. In an 1D simulation, there\n\
will only be one section.  In general, there will be a section for each\n\
numeric cell in the soil matrix with an edge towards the surface.");
  frame.declare ("EvapSoilSurface", "mm/h", Attribute::LogOnly, "\
Water evaporated from the surface, including the pond and exfiltration.");
  frame.declare ("Eps", "mm/h", Attribute::LogOnly, "\
Potential evaporation from the surface.");
  frame.declare ("T", "dg C", Attribute::LogOnly, "\
Temperature of water or air directly above the surface.");
  frame.declare ("DetentionCapacity", "mm", Check::non_negative (),
	      Attribute::State, "Amount of ponding the surface can retain.\n\
If ponding in any part of the surface is above this, exceed will runoff.");
  frame.set ("DetentionCapacity", 1000.0);
  frame.declare ("ReservoirConstant", "h^-1", Check::positive (), 
	      Attribute::Const, "\
Fraction of ponding above DetentionCapacity that runoffs each hour.");
  frame.set ("ReservoirConstant", 1.0);
  frame.declare ("LocalDetentionCapacity", "mm", Check::non_negative (),
                 Attribute::State, "\
Amount of ponding the surface can retain locally.\n                     \
If ponding in any part of the surface is above this, exceed will be\n\
distributed to the rest of the surface.");
  frame.set ("LocalDetentionCapacity", 10.0);
  frame.declare ("runoff", "mm/h", Attribute::LogOnly, "\
Amount of water runoff from ponding this hour.");
  frame.declare ("z_mixing", "cm", Check::non_negative (), Attribute::Const, "\
Depth of mixing layer in the top of the soil.\n\
The mixing layer affect exchange between soil coloids, soil water\n\
and the surface, especially in connection with intense rainfall.");
  frame.set ("z_mixing", 0.1);
  frame.declare ("R_mixing", "h/mm", Check::non_negative (), Attribute::Const, "\
Resistance to mixing inorganic compounds between soil and ponding.");
  frame.set ("R_mixing", 1.0e9);
  frame.declare_submodule ("ridge", Attribute::OptionalState, "\
Active ridge system, if any.",
			Ridge::load_syntax);
}

Surface::Surface (const FrameSubmodel& al)
  : impl (new Implementation (al))
{ }

Surface::Implementation::Implementation (const FrameSubmodel& al)
  : temperature_change_rate (al.number ("temperature_change_rate", -1.0)),
    EpFactor (al.number ("EpFactor")),
    EpFactor_SWE (al.plf ("EpFactor_SWE")),
    EpFactor_current (EpFactor),
    albedo_wet (al.number ("albedo_wet")),
    albedo_dry (al.number ("albedo_dry")),
    use_forced_pressure (al.check ("forced_pressure")),
    forced_pressure_value (al.number ("forced_pressure", -42.42e42)),
    use_forced_flux (al.check ("forced_flux")),
    forced_flux_value (al.number ("forced_flux", -42.42e42)),
    pond_average (NAN),
    pond_section (al.check ("pond_section")
                  ? al.number_sequence ("pond_section")
                  : std::vector<double> ()),
    EvapSoilSurface (0.0),
    Eps (0.0),
    T (0.0),
    DetentionCapacity (al.number ("DetentionCapacity")),
    ReservoirConstant (al.number ("ReservoirConstant")),
    LocalDetentionCapacity (al.number ("LocalDetentionCapacity")),
    runoff (0.0),
    runoff_rate (0.0),
    R_mixing (al.number ("R_mixing")),
    z_mixing (al.number ("z_mixing")),
    ridge_ (al.check ("ridge") ? new Ridge (al.submodel ("ridge")) : NULL)
{ }

Surface::~Surface ()
{ }

Surface::Implementation::~Implementation ()
{ }

static DeclareSubmodel 
surface_submodel (Surface::load_syntax, "Surface", "\
Keep track of things on the soil surface.");

// surface.C ends here.
