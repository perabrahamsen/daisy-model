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
#include "syntax.h"
#include "alist.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "mathlib.h"
#include "submodel.h"
#include "plf.h"
#include "ridge.h"
#include "check.h"
#include <sstream>
#include <map>

struct Surface::Implementation
{
  // Content.
  const double EpFactor;
  const double albedo_wet;
  const double albedo_dry;
  const bool use_forced_pressure;
  const double forced_pressure_value;
  const bool use_forced_flux;
  const double forced_flux_value;
  typedef std::map<size_t, double> pond_map;
  pond_map pond_edge;
  double pond_average;
  double EvapSoilSurface;
  double Eps;
  double T;
  double DetentionCapacity;
  const double ReservoirConstant;
  double runoff;
  double runoff_fraction;
  const double R_mixing;
  std::auto_ptr<Ridge> ridge_;

  // UZ top.
  Surface::top_t top_type (const Geometry& geo, size_t edge) const;
  double q_top (const Geometry& geo, size_t edge) const;

  // Functions.
  void ridge (const Geometry1D& geo,
              const Soil& soil, const SoilWater& soil_water,
	      const AttributeList&);
  void exfiltrate (const Geometry& geo, size_t edge,
                   double water, double dt, Treelog&);
  void update_pond_average (const Geometry& geo);
  void tick (Treelog&, double PotSoilEvaporation, double water, double temp,
	     const Geometry&, const Soil&, const SoilWater&,
             double T, double dt);
  double albedo (const Geometry&, const Soil&, const SoilWater&) const;
  void output (Log& log) const;
  double exfiltration () const; // [mm/h]
  

  // Create and Destroy.
  void initialize (const Geometry&);
  Implementation (const AttributeList& al);
  ~Implementation ();
};

Surface::top_t 
Surface::top_type (const Geometry& geo, size_t edge) const
{ return impl->top_type (geo, edge); }

Surface::top_t 
Surface::Implementation::top_type (const Geometry& geo, size_t edge) const
{
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  pond_map::const_iterator i = pond_edge.find (edge);
  daisy_assert (i != pond_edge.end ());

  if (use_forced_flux)
    return forced_flux;

  if (ridge_.get ())
    return soil;

  if (use_forced_pressure)
    return forced_pressure;
  
  if ((*i).second <= 0.0)
    return forced_flux;
  
  return limited_water;
}

double 
Surface::q_top (const Geometry& geo, const size_t edge) const
{ return impl->q_top (geo, edge); }
  
double 
Surface::Implementation::q_top (const Geometry& geo, const size_t edge) const
{
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  pond_map::const_iterator i = pond_edge.find (edge);
  daisy_assert (i != pond_edge.end ());

  if (use_forced_pressure)
    return forced_pressure_value * 0.1 / 1.0; // mm -> cm/h.

  if (use_forced_flux)
    return forced_flux_value * 0.1; // mm/h -> cm/h.

  if (ridge_.get ())
    return -ridge_->h () / 1.0 /* [h] */;
  else
    return -(*i).second * 0.1 / 1.0 /* [h] */; // mm -> cm/h.
}
  
double
Surface::h_top (const Geometry& geo, size_t edge) const
{ 
  return -q_top (geo, edge) * 1.0 /* h */; 
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
		const AttributeList& al)
{ impl->ridge (geo, soil, soil_water, al); }

void 
Surface::Implementation::ridge (const Geometry1D& geo,
                                const Soil& soil, const SoilWater& soil_water,
				const AttributeList& al)
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

  if (fabs (water) < 1e-99)
    return;

  // Exfiltration.
  if (water >= 0)
    {
      pond_edge[edge] += water;
      return;
    }

  Treelog::Open nest (msg, 
                      "Surface exfiltration for edge " + geo.edge_name (edge));

  // Infiltration.
  if (pond_edge[edge] + water
      < - std::max (fabs (pond_edge[edge]), fabs (water)) / 100.0)
    {
      std::ostringstream tmp;
      tmp << "edge " << geo.edge_name (edge) << ": pond (" << pond_edge[edge]
          << ") + exfiltration (" << water << ") in dt (" << dt << ") = " 
          << pond_edge[edge] + water << ", should be non-negative";
      msg.warning (tmp.str ());
      return;
    }
  pond_edge[edge] += water;
}

double
Surface::ponding () const
{ return impl->pond_average; }

double
Surface::runoff_rate (const double dt) const
{ return impl->runoff_fraction / dt; }

double
Surface::mixing_resistance () const
{ return impl->R_mixing; }

double
Surface::temperature () const
{ return impl->T; }

void
Surface::tick (Treelog& msg,
	       const double PotSoilEvaporation, 
               const double flux_in, const double temp,
	       const Geometry& geo,
               const Soil& soil, const SoilWater& soil_water, 
               const double soil_T,
               const double dt)
{ impl->tick (msg, PotSoilEvaporation, flux_in, temp, geo, 
             soil, soil_water, soil_T, dt); }

void
Surface::Implementation::update_pond_average (const Geometry& geo)
{
  const std::vector<int>& top_edges = geo.cell_edges (Geometry::cell_above);
  const size_t top_edges_size = top_edges.size ();

  // Find total pond.
  double total_pond = 0.0;      // [mm cm^2]
  double total_area = 0.0;      // [cm^2]
  for (int i = 0; i < top_edges_size; i++)
    {
      daisy_assert (top_edges[i] >= 0);
      const size_t edge = top_edges[i];
      const double area = geo.edge_area (edge);
      total_area += area;
      total_pond += pond_edge[edge] * area;
    }
  daisy_approximate (total_area, geo.surface_area ());
  pond_average = total_pond / total_area; // [mm];
}

void
Surface::Implementation::tick (Treelog& msg,
			       const double PotSoilEvaporation,
			       const double flux_in, const double temp,
                               const Geometry& geo,
                               const Soil& soil, const SoilWater& soil_water,
			       const double soil_T,
                               const double dt)
{

  // Runoff and average pond size.
  update_pond_average (geo);
  if (pond_average > DetentionCapacity)
    {
      runoff = (pond_average - DetentionCapacity) * ReservoirConstant;
      runoff_fraction = runoff / pond_average;
      pond_average -= runoff * dt;
    }
  else
    {
      runoff = 0.0;
      runoff_fraction = 0.0;
    }
  for (pond_map::iterator i = pond_edge.begin ();
       i != pond_edge.end ();
       i++)
    (*i).second = pond_average;
  
  // Remember potential evaopration
  Eps = PotSoilEvaporation;

  double EvapSoilTotal = 0.0;   // [mm cm^2/h]

  // Update pond above each top cell.
  const std::vector<int>& top_edges = geo.cell_edges (Geometry::cell_above);
  const size_t top_edges_size = top_edges.size ();
  for (size_t i = 0; i < top_edges_size; i++)
    {
      daisy_assert (top_edges[i] >= 0);
      const size_t edge = top_edges[i];
      const double area = geo.edge_area (edge); // [cm^2]

      // Exfiltration.
      const double MaxExfiltration // [mm]
        = soil_water.MaxExfiltration (geo, edge, soil, soil_T) * 10.0; 

      const double epond = pond_edge[edge]; // [mm]

      double evap;              // [mm/h]
      if (epond + flux_in * dt + MaxExfiltration * dt < Eps * dt)
        evap = epond / dt + flux_in + MaxExfiltration;
      else
        evap = Eps;

      EvapSoilTotal += evap * area;    // [mm cm^2/h]

      pond_edge[edge] += flux_in * dt - evap * dt;
      daisy_assert (evap < 1000.0);

      if (ridge_.get ())
        {
          const Geometry1D& geo1d = dynamic_cast<const Geometry1D&> (geo);
          ridge_->tick (geo1d, soil, soil_water, pond_edge[edge], dt);
          exfiltrate (geo, edge, ridge_->exfiltration (), dt, msg);
        }
    }
  EvapSoilSurface = EvapSoilTotal / geo.surface_area (); // [mm/h]
  update_pond_average (geo);

  // Temperature
  if (pond_average < 1e-6)
    T = temp;
  else if (flux_in < 0.0)
    {
      if (pond_average - EvapSoilSurface * dt + flux_in * dt < 1e-6)
        T = temp;
      // else use old temperature.
    }
  else
    T = (T * pond_average + temp * flux_in * dt)
      / (pond_average + flux_in * dt);

  daisy_assert (T > -100.0 && T < 50.0);
}

double 
Surface::EpFactor () const
{ return impl->EpFactor; }

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

  daisy_assert (Theta_pf_1_7 > Theta_pf_3);

  if (Theta < Theta_pf_3)
    return albedo_dry;
  if (Theta > Theta_pf_1_7)
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
  output_variable (EvapSoilSurface, log);
  output_variable (Eps, log);
  output_variable (runoff, log);
  if (ridge_.get ())
    output_submodule (*ridge_, "ridge", log);
}

double
Surface::exfiltration () const // [mm/h]
{ return impl->exfiltration (); }

double
Surface::Implementation::exfiltration () const // [mm/h]
{
  // Negative pond == amount extracted from soil.
  if (pond_average < 0.0)
    return -pond_average;
  else
    return 0.0;
}

double
Surface::evap_soil_surface () const // [mm/h]
{ return impl->EvapSoilSurface; }

double 
Surface::evap_pond (Treelog& msg) const	// [mm/h]
{ 
  const double ep = evap_soil_surface () - exfiltration (); 
  if (ep >= 0.0)
    return ep;
  if (ep < -1e-13)
    {
      Treelog::Open nest (msg, "Surface evap pond");
      std::ostringstream tmp;
      tmp << "evap_pond = " << ep << ", evap_soil_surface = " << evap_soil_surface () << ", exfiltration = " << exfiltration ();
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
    (*i).second = p;

  impl->pond_average = p; 
}

static bool
check_alist (const AttributeList& al, Treelog& msg)
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
  const std::vector<int>& top_edges = geo.cell_edges (Geometry::cell_above);
  const size_t top_edges_size = top_edges.size ();

  for (size_t i = 0; i < top_edges_size; i++)
    {
      daisy_assert (top_edges[i] >= 0);
      const size_t edge = top_edges[i];
      pond_edge[edge] = pond_average;
    }
}

void
Surface::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Surface");
  alist.add ("description", "Keep track of things on the soil surface.");
  syntax.add_check (check_alist);
  syntax.add ("EpFactor", Syntax::None (), Check::non_negative (), 
	      Syntax::Const,
	      "Convertion of reference evapotranspiration to\n\
potential evaporation for bare soil.");
  alist.add ("EpFactor", 1.0);
  syntax.add ("albedo_dry", Syntax::None (), Check::non_negative (),
	      Syntax::Const,
	      "Albedo of dry soil (pF >= 3)");
  alist.add ("albedo_dry", 0.15);
  syntax.add ("albedo_wet", Syntax::None (), Check::non_negative (),
	      Syntax::Const,
	      "Albedo of wet soil (pf <= 1.7)");
  alist.add ("albedo_wet", 0.08);
    syntax.add ("forced_pressure", "mm", Syntax::OptionalConst, "\
Set this to force a permanent pressure top.");
  syntax.add ("forced_flux", "mm/h", Syntax::OptionalConst, "\
Set this to force a permanent flux top.  Positive upwards (exfiltration).");
  syntax.add ("pond", "mm", Syntax::State, "\
Amount of ponding on the surface.\n\
Negative numbers indicate soil exfiltration.");
  alist.add ("pond", 0.0);
  syntax.add ("EvapSoilSurface", "mm/h", Syntax::LogOnly, "\
Water evaporated from the surface, including the pond and exfiltration.");
  syntax.add ("Eps", "mm/h", Syntax::LogOnly, "\
Potential evaporation from the surface.");
  syntax.add ("T", "dg C", Syntax::LogOnly, "\
Temperature of water or air directly above the surface.");
  syntax.add ("DetentionCapacity", "mm", Check::non_negative (),
	      Syntax::State, "Amount of ponding the surface can retain.");
  alist.add ("DetentionCapacity", 1000.0);
  syntax.add ("ReservoirConstant", "h^-1", Check::fraction (), 
	      Syntax::Const, "\
Fraction of ponding above DetentionCapacity that runoffs each hour.");
  alist.add ("ReservoirConstant", 1.0);
  syntax.add ("runoff", "mm/h", Syntax::LogOnly, "\
Amount of water runoff from ponding this hour.");
  syntax.add ("R_mixing", "h/mm", Check::non_negative (), Syntax::Const, "\
Resistance to mixing inorganic N between soil and ponding.");
  alist.add ("R_mixing", 1.0e9);
  syntax.add_submodule ("ridge", alist, Syntax::OptionalState, "\
Active ridge system, if any.",
			Ridge::load_syntax);
}

Surface::Surface (const AttributeList& al)
  : impl (new Implementation (al))
{ }

Surface::Implementation::Implementation (const AttributeList& al)
  : EpFactor (al.number ("EpFactor")),
    albedo_wet (al.number ("albedo_wet")),
    albedo_dry (al.number ("albedo_dry")),
    use_forced_pressure (al.check ("forced_pressure")),
    forced_pressure_value (al.number ("forced_pressure", -42.42e42)),
    use_forced_flux (al.check ("forced_flux")),
    forced_flux_value (al.number ("forced_flux", -42.42e42)),
    pond_average (al.number ("pond")),
    EvapSoilSurface (0.0),
    Eps (0.0),
    T (0.0),
    DetentionCapacity (al.number ("DetentionCapacity")),
    ReservoirConstant (al.number ("ReservoirConstant")),
    runoff (0.0),
    runoff_fraction (0.0),
    R_mixing (al.number ("R_mixing")),
    ridge_ (al.check ("ridge") ? new Ridge (al.alist ("ridge")) : NULL)
{ }

Surface::~Surface ()
{ }

Surface::Implementation::~Implementation ()
{ }

static Submodel::Register 
surface_submodel ("Surface", Surface::load_syntax);

// surface.C ends here.
