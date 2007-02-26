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


#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "im.h"
#include "mathlib.h"
#include "submodel.h"
#include "chemicals.h"
#include "soil_chemicals.h"
#include "plf.h"
#include "ridge.h"
#include <sstream>
#include "check.h"

struct Surface::Implementation
{
  // Content.
  const double minimal_matter_flux;
  const bool total_matter_flux;
  const double EpFactor;
  const double albedo_wet;
  const double albedo_dry;
  const bool use_forced_pressure;
  const double forced_pressure_value;
  const bool use_forced_flux;
  const double forced_flux_value;
  double pond;
  IM im_flux;
  double EvapSoilSurface;
  double Eps;
  double T;
  IM im;
  double DetentionCapacity;
  const double ReservoirConstant;
  double runoff;
  IM im_runoff;
  const double R_mixing;
  Chemicals chemicals_storage;
  Chemicals chemicals_out;
  Chemicals chemicals_runoff;
  const bool chemicals_can_enter_soil;
  Ridge* ridge_;

  // Functions.
  void ridge (const Geometry1D& geo,
              const Soil& soil, const SoilWater& soil_water,
	      const AttributeList&);
  void mixture (const IM& soil_im /* g/cm^2/mm */, double dt);
  void mixture (const Geometry& geo,
                const SoilChemicals& soil_chemicals, double dt);
  void exfiltrate (double water, double dt, Treelog&);
  double ponding () const;
  void tick (Treelog&, double PotSoilEvaporation, double water, double temp,
	     const Geometry&, const Soil&, const SoilWater&, double T, 
             double dt);
  double albedo (const Geometry&, const Soil&, const SoilWater&) const;
  void fertilize (const IM& n);
  void spray (const Chemicals& chemicals_in);
  void output (Log& log) const;
  double exfiltration () const; // [mm/h]
  

  // Create and Destroy.
  Implementation (const AttributeList& al);
  ~Implementation ();
};

Surface::top_t 
Surface::top_type (const Geometry& geo, size_t edge) const
{
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);

  if (impl.use_forced_flux)
    return forced_flux;

  if (impl.ridge_)
    return soil;

  if (impl.use_forced_pressure)
    return forced_pressure;
  
  if (impl.pond <= 0.0)
    return forced_flux;
  
  return limited_water;
}

double 
Surface::q_top (const Geometry& geo, const size_t edge, const double dt) const
{
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);

  if (impl.use_forced_flux)
    return impl.forced_flux_value * 0.1; // mm -> cm.

  if (impl.ridge_)
    return -impl.ridge_->h () / dt;
  else
    return -ponding () * 0.1 / dt; // mm -> cm.
}
  
double
Surface::h_top (const Geometry& geo, size_t edge, double dt) const
{ 
  return -q_top (geo, edge, dt) * dt; 
}

void
Surface::accept_top (double water /* [cm] */,
                     const Geometry& geo, size_t edge, 
                     double dt, Treelog& msg)
{ 
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);

  if (impl.ridge_)
    return;		// Handled by ridge based on flux.

  impl.exfiltrate (water * 10.0 * geo.edge_area (edge) / geo.surface_area (),
                   dt, msg); 
}

size_t 
Surface::last_cell (const Geometry& geo, size_t edge) const 
{ 
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);
  daisy_assert (edge == 0);
  daisy_assert (impl.ridge_);
  return impl.ridge_->last_cell ();
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
  if (impl.ridge_)
    impl.ridge_->update_water (geo, soil, S_, h_, Theta_, q, q_p, dt); 
}

void 
Surface::ridge (const Geometry1D& geo,
                const Soil& soil, const SoilWater& soil_water, 
		const AttributeList& al)
{ impl.ridge (geo, soil, soil_water, al); }

void 
Surface::Implementation::ridge (const Geometry1D& geo,
                                const Soil& soil, const SoilWater& soil_water,
				const AttributeList& al)
{
  // No permanent ponding.
  daisy_assert (!use_forced_pressure);

  // Get rid of old ridge system.
  if (ridge_)
    delete ridge_;

  // Create new ridge system.
  ridge_ = new Ridge (al);
  ridge_->initialize (geo, soil, soil_water);
}

void 
Surface::unridge ()
{ 
  delete impl.ridge_;
  impl.ridge_ = NULL;
}

void
Surface::mixture (const IM& soil_im /* g/cm^2/mm */, const double dt)
{
  impl.mixture (soil_im, dt);
}

void
Surface::mixture (const Geometry& geo,
                  const SoilChemicals& soil_chemicals, const double dt)
{
  impl.mixture (geo, soil_chemicals, dt);
}

void
Surface::Implementation::mixture (const IM& soil_im /* g/cm^2/mm */, 
                                  const double dt /* [h] */)
{
  if (!total_matter_flux && pond > 1e-6 && R_mixing > 0.0)
    {
      // [g/cm^2/h] = ([g/cm^2/mm] - [g/cm^2] / [mm]) / [h/mm]
      im_flux.NO3 = std::max (-im.NO3 / dt,
                              (soil_im.NO3 - im.NO3 / pond) / R_mixing);
      im_flux.NH4 = std::max (-im.NH4 / dt,
                              (soil_im.NH4 - im.NH4 / pond) / R_mixing);
      im += im_flux * dt;
    }
  else
    im_flux.clear ();
}

void
Surface::Implementation::mixture (const Geometry& geo,
                                  const SoilChemicals& soil_chemicals,
                                  const double dt)
{
  if (chemicals_can_enter_soil)
    {
      chemicals_out.clear ();

      soil_chemicals.mixture (geo, chemicals_storage, chemicals_out,
			      pond, R_mixing, dt);
    }
}

void
Surface::Implementation::exfiltrate (const double water /* [mm] */, 
                                     const double dt /* [h] */, Treelog& msg)
{
  if (use_forced_pressure)
    return;

  if (fabs (water) < 1e-99)
    return;

  // Exfiltration.
  if (water >= 0)
    {
      pond += water;
      return;
    }

  Treelog::Open nest (msg, "Surface exfiltration");

  // Infiltration.
  if (pond + water < - std::max (fabs (pond), fabs (water)) / 100.0)
    {
      std::ostringstream tmp;
      tmp << "pond (" << pond << ") + exfiltration (" << water << ") in dt ("
          << dt << ") = " << pond + water << ", should be non-negative";
      msg.warning (tmp.str ());
      return;
    }
  if (im.NO3 < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Added " << -im.NO3 << " NO3 to surface";
      msg.warning (tmp.str ());
      im.NO3 = 0.0;
    }
  if (im.NH4 < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Added " << -im.NH4 << " NH4 to surface\n";
      msg.warning (tmp.str ());
      im.NH4 = 0.0;
    }
  if (total_matter_flux)
    {
      IM delta_matter (im, 1.0);
      delta_matter /= dt;
      im_flux -= delta_matter;
      im.clear ();
      if (chemicals_can_enter_soil)
        {
          chemicals_out += chemicals_storage;
          chemicals_storage.clear ();
        }
    }
  else if (-water / dt > minimal_matter_flux)
    {
      IM delta_matter (im, -water / pond);
      im -= delta_matter;
      delta_matter /= dt;
      im_flux -= delta_matter;
      if (chemicals_can_enter_soil)
        Chemicals::move_fraction (chemicals_storage, chemicals_out,
                                  -water / pond);
    }

  pond += water;
}

double
Surface::ponding () const
{ return impl.ponding (); }

double
Surface::Implementation::ponding () const
{
  if (use_forced_pressure)
    return forced_pressure_value;

  return pond;
}

const IM& 
Surface::matter_flux ()
{ return impl.im_flux; }

const Chemicals& 
Surface::chemicals_down () const
{ return impl.chemicals_out; }

double
Surface::temperature () const
{ return impl.T; }

void
Surface::tick (Treelog& msg,
	       const double PotSoilEvaporation, 
               const double flux_in, const double temp,
	       const Geometry& geo,
               const Soil& soil, const SoilWater& soil_water, 
               const double soil_T,
               const double dt)
{ impl.tick (msg, PotSoilEvaporation, flux_in, temp, geo, 
             soil, soil_water, soil_T, dt); }

void
Surface::Implementation::tick (Treelog& msg,
			       const double PotSoilEvaporation,
			       const double flux_in, const double temp,
                               const Geometry& geo,
                               const Soil& soil, const SoilWater& soil_water,
			       const double soil_T,
                               const double dt)
{
  if (pond > DetentionCapacity)
    {
      runoff = (pond - DetentionCapacity) * ReservoirConstant;
      Chemicals::move_fraction (chemicals_storage, chemicals_runoff, 
				runoff / pond);
      im_runoff = im * (runoff / pond);
      pond -= runoff;
      im -= im_runoff;
    }
  else
    {
      im_runoff.clear ();
      chemicals_runoff.clear ();
      runoff = 0.0;
    }

  const double MaxExfiltration
    = soil_water.MaxExfiltration (geo, soil, soil_T) * 10.0; // cm -> mm.

  Eps = PotSoilEvaporation;

  if (pond + flux_in * dt + MaxExfiltration * dt < Eps * dt)
    EvapSoilSurface = pond / dt + flux_in + MaxExfiltration;
  else
    EvapSoilSurface = Eps;

  if (pond < 1e-6)
    T = temp;
  else if (flux_in < 0.0)
    {
      if (pond - EvapSoilSurface * dt + flux_in * dt < 1e-6)
	T = temp;
      // else use old temperature.
    }
  else
    T = (T * pond + temp * flux_in * dt) / (pond + flux_in * dt);

  daisy_assert (T > -100.0 && T < 50.0);
  pond = pond - EvapSoilSurface * dt + flux_in * dt;
  daisy_assert (EvapSoilSurface < 1000.0);

  if (ridge_)
    {
      const Geometry1D& geo1d = dynamic_cast<const Geometry1D&> (geo);
      ridge_->tick (geo1d, soil, soil_water, pond, dt);
      exfiltrate (ridge_->exfiltration (), dt, msg);
    }
}

double 
Surface::EpFactor () const
{ return impl.EpFactor; }

double
Surface::albedo (const Geometry& geo, 
                 const Soil& soil, const SoilWater& soil_water) const
{ return impl.albedo (geo, soil, soil_water); }

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
Surface::fertilize (const IM& n)
{ impl.fertilize (n); }

void 
Surface::Implementation::fertilize (const IM& n)
{ 
  daisy_assert (n.NO3 >= 0.0);
  daisy_assert (n.NH4 >= 0.0);

  im += n;
}

void
Surface::spray (const Chemicals& chemicals_in)
{ impl.spray (chemicals_in); }

void
Surface::set_detention_capacity (const double height)
{ impl.DetentionCapacity = height; }

void
Surface::Implementation::spray (const Chemicals& chemicals_in)
{
  chemicals_storage += chemicals_in;
}

void
Surface::output (Log& log) const
{ impl.output (log); }

void
Surface::Implementation::output (Log& log) const
{
  output_variable (T, log);
  output_variable (pond, log);
  output_variable (EvapSoilSurface, log);
  output_variable (Eps, log);
  output_variable (runoff, log);
  output_submodule (im, "IM", log);
  output_submodule (im_runoff, "IM_runoff", log);
  output_submodule (chemicals_storage, "chemicals_storage", log);
  output_submodule (chemicals_out, "chemicals_out", log);
  output_submodule (chemicals_runoff, "chemicals_runoff", log);
  if (ridge_)
    output_submodule (*ridge_, "ridge", log);
}

double
Surface::exfiltration () const // [mm/h]
{ return impl.exfiltration (); }

double
Surface::Implementation::exfiltration () const // [mm/h]
{
  // Negative pond == amount extracted from soil.
  if (pond < 0.0)
    return -pond;
  else
    return 0.0;
}

double
Surface::evap_soil_surface () const // [mm/h]
{ return impl.EvapSoilSurface; }

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
        throw 3;
    }
  return 0.0;
}

void
Surface::put_ponding (double p)	// [mm]
{ impl.pond = p; }
  
void
Surface::put_no3 (double no3) // [g/cm^2]
{ impl.im.NO3 = no3; }

double
Surface::get_no3 () const // [g/cm^2]
{ return impl.im.NO3; }

void 
Surface::put_chemical (symbol name , double amount) // [g/cm^2]
{
  // [g/cm^2] -> [g/m^2]
  impl.chemicals_storage.set_to (name, amount * 1.0e4);
}

double 
Surface::get_chemical (symbol name) const // [g/cm^2]
{ 
  // [g/m^2] -> [g/cm^2]
  return impl.chemicals_storage.amount (name) * 1.0e-4;		
}

static bool
check_alist (const AttributeList& al, Treelog& msg)
{
  bool ok = true;

  if (al.check ("forced_flux") && al.number ("forced_pressure") >= 0)
    {
      msg.error ("Can't have both 'forced_pressure' and 'forced_flux'");
      ok = false;
    }

  return ok;
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
  syntax.add ("minimal_matter_flux", "mm", Syntax::Const, "\
Minimal amount of of precipitation in order for inorganic N\n\
to enter the soil.");
  alist.add ("minimal_matter_flux", 1.0e-10);
  syntax.add ("total_matter_flux", Syntax::Boolean, Syntax::Const, "\
Set this to true to force all inorganic N on the surface to enter the\n\
soil immediately, even when there is no precipitation.");
  alist.add ("total_matter_flux", false);
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
  syntax.add_submodule ("IM", alist, Syntax::State, "\
Inorganic nitrogen on the surface [g/cm^2].",
			IM::load_soil);
  syntax.add_submodule ("IM_runoff", alist, Syntax::LogOnly, "\
Inorganic nitrogen on the runoff water this hour [g/cm^2/h].",
			IM::load_soil_flux);
  syntax.add ("R_mixing", "h/mm", Check::non_negative (), Syntax::Const, "\
Resistance to mixing inorganic N between soil and ponding.");
  alist.add ("R_mixing", 1.0e9);
  Chemicals::add_syntax  ("chemicals_storage", syntax, alist, Syntax::State,
			  "Chemicals on the soil surface.");
  Chemicals::add_syntax  ("chemicals_out", syntax, alist, Syntax::LogOnly,
			  "Chemicals entering the soil.");
  Chemicals::add_syntax  ("chemicals_runoff", syntax, alist, Syntax::LogOnly,
			  "Chemicals in the runoff water this hour.");
  syntax.add ("chemicals_can_enter_soil",
	      Syntax::Boolean, Syntax::Const, "\
If this is set to false, the chemicals will stay on the surface.");
  alist.add ("chemicals_can_enter_soil", true);
  syntax.add_submodule ("ridge", alist, Syntax::OptionalState, "\
Active ridge system, if any.",
			Ridge::load_syntax);
}

Surface::Surface (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Surface::Implementation::Implementation (const AttributeList& al)
  : minimal_matter_flux (al.number ("minimal_matter_flux")),
    total_matter_flux (al.flag ("total_matter_flux")),
    EpFactor (al.number ("EpFactor")),
    albedo_wet (al.number ("albedo_wet")),
    albedo_dry (al.number ("albedo_dry")),
    use_forced_pressure (al.check ("forced_pressure")),
    forced_pressure_value (al.number ("forced_pressure", -42.42e42)),
    use_forced_flux (al.check ("forced_flux")),
    forced_flux_value (al.number ("forced_flux", -42.42e42)),
    pond (al.number ("pond")),
    im_flux (),
    EvapSoilSurface (0.0),
    Eps (0.0),
    T (0.0),
    im (al.alist ("IM")),
    DetentionCapacity (al.number ("DetentionCapacity")),
    ReservoirConstant (al.number ("ReservoirConstant")),
    runoff (0.0),
    im_runoff (),
    R_mixing (al.number ("R_mixing")),
    chemicals_storage (al.alist_sequence ("chemicals_storage")),
    chemicals_out (),
    chemicals_runoff (),
    chemicals_can_enter_soil (al.flag ("chemicals_can_enter_soil")),
    ridge_ (al.check ("ridge") ? new Ridge (al.alist ("ridge")) : NULL)
{
  daisy_assert (iszero (im_flux.NO3));
  daisy_assert (iszero (im_flux.NH4));
}

Surface::~Surface ()
{ delete &impl; }

Surface::Implementation::~Implementation ()
{ 
  if (ridge_)
    delete ridge_;
}

static Submodel::Register 
surface_submodel ("Surface", Surface::load_syntax);
