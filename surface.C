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
#include "timestep.h"
#include "im.h"
#include "mathlib.h"
#include "submodel.h"
#include "chemicals.h"
#include "soil_chemicals.h"
#include "plf.h"
#include "ridge.h"
#include <sstream>
#include "check.h"

using namespace std;

struct Surface::Implementation
{
  // Content.
  const double minimal_matter_flux;
  const bool total_matter_flux;
  const double EpFactor;
  const double albedo_wet;
  const double albedo_dry;
  const double lake;
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
  void mixture (const IM& soil_im /* g/cm^2/mm */);
  void mixture (const Geometry& geo,
                const SoilChemicals& soil_chemicals);
  void exfiltrate (double water, Treelog&);
  double ponding () const;
  void tick (Treelog&, double PotSoilEvaporation, double water, double temp,
	     const Geometry& geo,
             const Soil& soil, const SoilWater& soil_water, double T);
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

  if (impl.ridge_)
    return soil;

  if (impl.lake >= 0.0)
    return forced_pressure;
  
  if (impl.pond <= 0.0)
    return forced_flux;
  
  return limited_water;
}

double 
Surface::q_top (const Geometry& geo, size_t edge) const
{
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);

  if (impl.ridge_)
    return -impl.ridge_->h () / dt;
  else
    return -ponding () * 0.1;		// mm -> cm.
}
  
double
Surface::h_top (const Geometry& geo, size_t edge) const
{ 
  return -q_top (geo, edge) * dt; 
}

void
Surface::accept_top (double water /* cm */, const Geometry& geo, size_t edge, 
                     Treelog& msg)
{ 
  daisy_assert (geo.edge_to (edge) == Geometry::cell_above);

  if (impl.ridge_)
    return;		// Handled by ridge based on flux.

  impl.exfiltrate (water * 10.0 * geo.edge_area (edge) / geo.surface_area (),
                   msg); 
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
		       const vector<double>& S_,
		       vector<double>& h_,
		       vector<double>& Theta_,
		       vector<double>& q,
		       const vector<double>& q_p)
{
  if (impl.ridge_)
    impl.ridge_->update_water (geo, soil, S_, h_, Theta_, q, q_p); 
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
  daisy_assert (lake < 0.0);

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
Surface::mixture (const IM& soil_im /* g/cm^2/mm */)
{
  impl.mixture (soil_im);
}

void
Surface::mixture (const Geometry& geo,
                  const SoilChemicals& soil_chemicals)
{
  impl.mixture (geo, soil_chemicals);
}

void
Surface::Implementation::mixture (const IM& soil_im /* g/cm^2/mm */)
{
  if (!total_matter_flux && pond > 1e-6 && R_mixing > 0.0)
    {
      // [g/cm^2/h] = ([g/cm^2/mm] - [g/cm^2] / [mm]) / [h/mm]
      im_flux.NO3 = max (-im.NO3 / dt,
			 (soil_im.NO3 - im.NO3 / pond) / R_mixing);
      im_flux.NH4 = max (-im.NH4 / dt,
			 (soil_im.NH4 - im.NH4 / pond) / R_mixing);
      im += im_flux * dt;
    }
  else
    im_flux.clear ();
}

void
Surface::Implementation::mixture (const Geometry& geo,
                                  const SoilChemicals& soil_chemicals)
{
  if (chemicals_can_enter_soil)
    {
      chemicals_out.clear ();

      soil_chemicals.mixture (geo, chemicals_storage, chemicals_out,
			      pond, R_mixing);
    }
}

void
Surface::Implementation::exfiltrate (double water /* mm */, Treelog& msg)
{
  if (lake >= 0.0)
    return;

  if (fabs (water) < 1e-99)
    return;

  // Exfiltration.
  if (water >= 0)
    {
      pond += water * dt;
      return;
    }

  Treelog::Open nest (msg, "Surface exfiltration");

  // Infiltration.
  if (pond + water * dt < - max (fabs (pond), fabs (water)) / 100.0)
    {
      std::ostringstream tmp;
      tmp << "pond (" << pond << ") + flux (" << water << ") * dt (" << dt 
          << ") = " << pond + water * dt << ", should be non-negative";
      msg.error (tmp.str ());
      return;
    }
  if (im.NO3 < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Added " << -im.NO3 << " NO3 to surface";
      msg.error (tmp.str ());
      im.NO3 = 0.0;
    }
  if (im.NH4 < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Added " << -im.NH4 << " NH4 to surface\n";
      msg.error (tmp.str ());
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
  else if (-water > minimal_matter_flux)
    {
      IM delta_matter (im, (-water * dt) / pond);
      im -= delta_matter;
      delta_matter /= dt;
      im_flux -= delta_matter;
      if (chemicals_can_enter_soil)
        Chemicals::move_fraction (chemicals_storage, chemicals_out,
                                  (-water * dt) / pond);
    }

  pond += water * dt;
}

double
Surface::ponding () const
{ return impl.ponding (); }

double
Surface::Implementation::ponding () const
{
  if (lake < 0.0)
    {
      // daisy_assert (!flux_top ());
      return pond;
    }
  else
    return lake;
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
	       double PotSoilEvaporation, double water, double temp,
	       const Geometry& geo,
               const Soil& soil, const SoilWater& soil_water, double soil_T)
{ impl.tick (msg, PotSoilEvaporation, water, temp, geo, 
             soil, soil_water, soil_T); }

void
Surface::Implementation::tick (Treelog& msg,
			       double PotSoilEvaporation,
			       double water, double temp,
                               const Geometry& geo,
                               const Soil& soil, const SoilWater& soil_water,
			       double soil_T)
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

  if (pond + water * dt + MaxExfiltration * dt < Eps * dt)
    EvapSoilSurface = pond / dt + water + MaxExfiltration;
  else
    EvapSoilSurface = Eps;

  if (pond < 1e-6)
    T = temp;
  else if (water < 0.0)
    {
      if (pond - EvapSoilSurface * dt + water * dt < 1e-6)
	T = temp;
      // else use old temperature.
    }
  else
    T = (T * pond + temp * water * dt) / (pond + water * dt);

  daisy_assert (T > -100.0 && T < 50.0);
  pond = pond - EvapSoilSurface * dt + water * dt;
  daisy_assert (EvapSoilSurface < 1000.0);

  if (ridge_)
    {
      const Geometry1D& geo1d = dynamic_cast<const Geometry1D&> (geo);
      ridge_->tick (geo1d, soil, soil_water, pond);
      exfiltrate (ridge_->exfiltration (), msg);
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
        const double v = geo.volume (i);
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
      tmp << "BUG: evap_pond = " << ep;
      msg.error (tmp.str ());
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

void
Surface::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Surface");
  alist.add ("description", "Keep track of things on the soil surface.");
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
  syntax.add ("lake", "mm", Syntax::Const, "\
Set this to a positive number to force a permanent pressure top.");
  alist.add ("lake", -1.0);
  syntax.add ("pond", "mm", Syntax::State, "\
Amount of ponding on the surface.\n\
Negative numbers indicate soil exfiltration.");
  alist.add ("pond", 0.0);
  syntax.add ("EvapSoilSurface", "mm", Syntax::LogOnly, "\
Water evaporated from the surface, including the pond and exfiltration.");
  syntax.add ("Eps", "mm", Syntax::LogOnly, "\
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
    lake (al.number ("lake")),
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
  daisy_assert (im_flux.NO3 == 0.0);
  daisy_assert (im_flux.NH4 == 0.0);
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
