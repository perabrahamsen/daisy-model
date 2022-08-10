// surface_std.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2022 UCPH
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

// The 'default' model.

#include "surface.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include "plf.h"
#include "check.h"
#include "treelog.h"
#include "block_model.h"
#include <memory>
#include <sstream>
#include <map>


struct SurfaceStandard : public Surface
{
  // Content.
  const double temperature_change_rate; // [h^-1]
  const double EpFactor_;		// []
  const PLF EpFactor_SWE;	// [pF] -> []
  double EpFactor_current;	// []
  const double albedo_wet;
  const double albedo_dry;
#ifdef FORCED_BOUNDARY
  const bool use_forced_pressure;
  const double forced_pressure_value;
  const bool use_forced_flux;
  const double forced_flux_value;
#endif // FORCED_FLUX_VALUE
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
  double runoff_rate_;          // [h^-1]
  const double R_mixing;
  const double z_mixing;

public:
  // Communication with soil water.
  top_t top_type (const Geometry&, size_t edge) const;
  double q_top (const Geometry&, size_t edge, const double dt) const; // [cm/h]
  double h_top (const Geometry&, size_t edge) const; // [cm]
  void accept_top (double amount, const Geometry&, size_t edge, 
                   double dt, Treelog&);

  // Column.
  double runoff_rate () const; // [h^-1]
  double mixing_resistance () const; // [h/mm]
  double mixing_depth () const; // [cm]
  
  // Manager.
  void set_detention_capacity (double);

  // Simulation.
  void output (Log&) const;
  void update_pond_average (const Geometry& geo);
  void tick (const Time&, double dt /* [h] */, 
             double PotSoilEvaporationWet, 
             double PotSoilEvaporationDry, 
             double flux_in /* [mm/h] */,
             double temp /* [dg C] */, const Geometry& geo,
             const Soil&, const SoilWater&,
             double soil_T /* [dg C] */,
	     Treelog&);

  // Communication with bioclimate.
  double ponding_average () const; // [mm]
  double ponding_max () const;     // [mm]
  double temperature () const;     // [dg C]
  double EpFactor () const;        // []
  double albedo (const Geometry&, const Soil&, const SoilWater&) const;
  double exfiltration (double dt) const; // [mm/h]
  double evap_soil_surface () const; // [mm/h]
  double evap_pond (double dt, Treelog&) const; // [mm/h]
  void put_ponding (double pond);	// [mm]
  void set_svat_temperature (double T /* dg C */);
  
  // Create.
  void initialize (const Geometry&);
  SurfaceStandard (const BlockModel& al);
  ~SurfaceStandard ();
};

Surface::top_t 
SurfaceStandard::top_type (const Geometry& geo, size_t edge) const
{
#ifdef FORCED_BOUNDARY
  if (use_forced_flux)
    return forced_flux;

  if (use_forced_pressure)
    return forced_pressure;
#endif // FORCED_BOUNDARY  
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  pond_map::const_iterator i = pond_edge.find (edge);
  daisy_assert (i != pond_edge.end ());
  const size_t c = (*i).second;
  if (pond_section[c] <= 0.0)
    return forced_flux;
  
  return limited_water;
}

double 
SurfaceStandard::q_top (const Geometry& geo, const size_t edge,
			const double dt) const
{
#ifdef FORCED_BOUNDARY
  if (use_forced_pressure)
    return -forced_pressure_value * 0.1 / 1.0; // mm -> cm/h.

  if (use_forced_flux)
    return forced_flux_value * 0.1; // mm/h -> cm/h.
#endif // FORCED_BOUNDARY
  
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  pond_map::const_iterator i = pond_edge.find (edge);
  daisy_assert (i != pond_edge.end ());
  const size_t c = (*i).second;
  daisy_assert (pond_section[c] < std::max (1000.0, 10.0 * DetentionCapacity));
  daisy_assert (pond_section[c] > -1000.0);
  return -pond_section[c] * 0.1 / dt /* [h] */; // mm -> cm/h.
}
  
double
SurfaceStandard::h_top (const Geometry& geo, size_t edge) const
{ 
  const double dt = 1.0;       // [h]
  return -q_top (geo, edge, dt) * dt; 
}

void
SurfaceStandard::accept_top (double water /* [cm] */,
			     const Geometry& geo, size_t edge,
			     double dt, Treelog& msg)
{ 
#ifdef FORCED_BOUNDARY
  if (use_forced_pressure)
    return;
#endif // FORCED_BOUNDARY

  water *= 10.0;		// [cm] -> [mm]

  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  const size_t c = pond_edge[edge];
  daisy_assert (pond_section[c] < std::max(1000.0, 10.0 * DetentionCapacity));
  daisy_assert (pond_section[c] > -1000.0);
  pond_section[c] += water;
  daisy_assert (pond_section[c] < std::max (1000.0, 10.0 * DetentionCapacity));
  daisy_assert (pond_section[c] > -1000.0);
}

double
SurfaceStandard::ponding_average () const
{ return pond_average; }

double
SurfaceStandard::ponding_max () const
{ return pond_max; }

double
SurfaceStandard::runoff_rate () const
{ return runoff_rate_; }

double
SurfaceStandard::mixing_resistance () const
{ return R_mixing; }

double
SurfaceStandard::mixing_depth () const
{ return z_mixing; }

double
SurfaceStandard::temperature () const
{ return T; }

void
SurfaceStandard::update_pond_average (const Geometry& geo)
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
SurfaceStandard::tick (const Time&, const double dt,
		       const double PotSoilEvaporationWet,
		       const double PotSoilEvaporationDry,
		       const double flux_in, const double temp,
		       const Geometry& geo,
		       const Soil& soil, const SoilWater& soil_water,
		       const double soil_T,
		       Treelog& msg)
{
  // Runoff out of field.
  const double old_pond_average = pond_average;
  runoff = 0.0;
  runoff_rate_ = 0.0;
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
          runoff_rate_ +=          // [A h^-1]
            area * runoff_section / pond_section[c]; 
          pond_section[c] -= runoff_section * dt; // [mm]
        }
    }
  runoff /= total_area;         // [mm]
  runoff_rate_ /= total_area; // [h^-1]
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
    ? EpFactor_ * EpFactor_SWE (h2pF (h))
    : EpFactor_ * EpFactor_SWE (-99.99);
}

double 
SurfaceStandard::EpFactor () const
{ return EpFactor_current; }

double
SurfaceStandard::albedo (const Geometry& geo, const Soil& soil,
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
SurfaceStandard::set_detention_capacity (const double height)
{ DetentionCapacity = height; }

void
SurfaceStandard::output (Log& log) const
{
  output_variable (T, log);
  output_value (pond_average, "pond", log);
  output_variable (pond_section, log);
  output_variable (EvapSoilSurface, log);
  output_variable (Eps, log);
  output_variable (runoff, log);
}

double
SurfaceStandard::exfiltration (const double dt) const // [mm/h]
{
  // Negative pond == amount extracted from soil.
  if (pond_average < 0.0)
    return -pond_average / dt;
  else
    return 0.0;
}

double
SurfaceStandard::evap_soil_surface () const // [mm/h]
{ return EvapSoilSurface; }

double 
SurfaceStandard::evap_pond (const double dt, Treelog& msg) const // [mm/h]
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
    }
  return 0.0;
}

void
SurfaceStandard::put_ponding (double p)	// [mm]
{ 
  for (pond_map::iterator i = pond_edge.begin ();
       i != pond_edge.end ();
       i++)
    {
      const size_t c = (*i).second;
      pond_section[c] = p;
    }

  pond_average = p; 
}

void
SurfaceStandard::set_svat_temperature (double T_surface /* dg C */)
{ 
  if (pond_average < 1e-5)
    T = T_surface;
}
  

void 
SurfaceStandard::initialize (const Geometry& geo)
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

SurfaceStandard::SurfaceStandard (const BlockModel& al)
  : Surface (al),
    temperature_change_rate (al.number ("temperature_change_rate", -1.0)),
    EpFactor_ (al.number ("EpFactor")),
    EpFactor_SWE (al.plf ("EpFactor_SWE")),
    EpFactor_current (EpFactor_),
    albedo_wet (al.number ("albedo_wet")),
    albedo_dry (al.number ("albedo_dry")),
#ifdef FORCED_BOUNDARY
    use_forced_pressure (al.check ("forced_pressure")),
    forced_pressure_value (al.number ("forced_pressure", -42.42e42)),
    use_forced_flux (al.check ("forced_flux")),
    forced_flux_value (al.number ("forced_flux", -42.42e42)),
#endif // FORCED_BOUNDARY
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
    runoff_rate_ (0.0),
    R_mixing (al.number ("R_mixing")),
    z_mixing (al.number ("z_mixing"))
{ }

SurfaceStandard::~SurfaceStandard ()
{ }

static struct SurfaceStandardSyntax : DeclareModel
{
  bool used_to_be_a_submodel () const
  { return true; }

  Model* make (const BlockModel& al) const
  { return new SurfaceStandard (al); }

  SurfaceStandardSyntax () 
    : DeclareModel (Surface::component, "default", "\
Keep track of soil surface.")
  { }

  static bool
  check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;

#ifdef FORCED_BOUNDARY
    if (al.check ("forced_flux") && al.check ("forced_pressure"))
      {
	msg.error ("Can't have both 'forced_pressure' and 'forced_flux'");
	ok = false;
      }
#endif // FORCED_BOUNDARY
    return ok;
  }

  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
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
#ifdef FORCED_BOUNDARY
    frame.declare ("forced_pressure", "mm", Attribute::OptionalConst, "\
Set this to force a permanent pressure top.");
    frame.declare ("forced_flux", "mm/h", Attribute::OptionalConst, "\
Set this to force a permanent flux top.  Positive upwards (exfiltration).");
#endif // FORCED_BOUNDARY
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
  }
} SurfaceStandard_syntax;

// surface_std.C ends here.
