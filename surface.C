// surface.C

#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "soil_water.h"
#include "soil.h"
#include "log.h"
#include "common.h"
#include "filter.h"
#include "am.h"
#include "im.h"
#include "mathlib.h"

struct Surface::Implementation
{
  // Content.
  const double minimal_matter_flux;
  const bool total_matter_flux;
  const double EpFactor;
  const double EpInterchange;
  const double albedo_wet;
  const double albedo_dry;
  const double lake;
  double pond;
  bool flux;
  IM im_flux;
  double EvapSoilSurface;
  double Eps;
  double T;
  IM im;

  // Functions.
  bool flux_top () const;
  void  flux_top_on ();
  bool accept_top (double water);
  double ponding () const;
  void tick (double PotSoilEvaporation, double water, double temp,
	     const Soil& soil, const SoilWater& soil_water);
  double albedo (const Soil& soil, const SoilWater& soil_water) const;
  void fertilize (const IM& n);
  void output (Log& log, Filter& filter) const;
  double exfiltration () const; // [mm/h]
  

  // Create and Destroy.
  Implementation (const AttributeList& al);
  ~Implementation ();
};


bool 
Surface::flux_top () const
{ return impl.flux_top (); }

bool 
Surface::Implementation::flux_top () const
{
  return lake < 0.0 && flux;
}

double 
Surface::q () const
{
  return -impl.pond / 10.0;		// mm -> cm.
}
  
void  
Surface::flux_top_on ()
{ impl.flux_top_on (); }

void  
Surface::Implementation::flux_top_on ()
{ 
  flux = true;
}

void  
Surface::flux_top_off ()
{ 
  impl.flux = false;
}

bool  
Surface::accept_top (double water)
{ return impl.accept_top (water); }

bool  
Surface::Implementation::accept_top (double water)
{
  assert (im_flux.NO3 == 0.0);
  assert (im_flux.NH4 == 0.0);

  if (lake >= 0.0)
    return true;

  if (fabs (water) < 1e-99)
    return true;

  water *= 10.0;		// cm -> mm.

  if (water >= 0)		// Exfiltration.
    {
      pond += water;
      return true;
    }
  if (pond + water * dt >= - max (fabs (pond), fabs (water)) / 100.0)
    {
      if (im.NO3 < 0.0)
	{
	  CERR << "BUG: Added " << -im.NO3 << " NO3 to surface\n";
	  im.NO3 = 0.0;
	}
      if (im.NH4 < 0.0)
	{
	  CERR << "BUG: Added " << -im.NH4 << " NH4 to surface\n";
	  im.NH4 = 0.0;
	}
      if (total_matter_flux)
	{
	  IM delta_matter (im, 1.0);
	  delta_matter /= dt;
	  im_flux -= delta_matter;
	  im.clear ();
	}
      else if (-water > minimal_matter_flux)
	{
	  IM delta_matter (im, (-water * dt) / pond);
	  im -= delta_matter;
	  delta_matter /= dt;
	  im_flux -= delta_matter;
	}
      assert (im_flux.NO3 <= 0.0);
      assert (im_flux.NH4 <= 0.0);
      pond += water * dt;
      return true;
    }
  else
    return false;
}

double
Surface::ponding () const
{ return impl.ponding (); }

double
Surface::Implementation::ponding () const
{
  if (lake < 0.0)
    {
      assert (!flux_top ());
      return pond;
    }
  else
    return lake;
}

double
Surface::temperature () const
{ return impl.T; }

void
Surface::clear ()
{ impl.im_flux.clear (); }

const IM& 
Surface::matter_flux ()
{ return impl.im_flux; }

void
Surface::tick (double PotSoilEvaporation, double water, double temp,
	       const Soil& soil, const SoilWater& soil_water)
{ impl.tick (PotSoilEvaporation, water, temp, soil, soil_water); }

void
Surface::Implementation::tick (double PotSoilEvaporation,
			       double water, double temp,
			       const Soil& soil, const SoilWater& soil_water)
{
  static const double dt = 1.0; // Time step [h].
  const double MaxExfiltration
    = soil_water.MaxExfiltration (soil) * 10.0; // cm -> mm.

  Eps = PotSoilEvaporation;

  if (pond + water * dt < Eps * dt)
    flux_top_on ();

  if (pond + water * dt + MaxExfiltration * dt < Eps * dt)
    EvapSoilSurface = pond / dt + water + MaxExfiltration;
  else
    EvapSoilSurface = Eps;

  if (pond < 1e-6)
    T = temp;
  else
    T = (T * pond + temp * water * dt) / (pond + water * dt);

  assert (T > -100.0 && T < 50.0);
  pond = pond - EvapSoilSurface * dt + water * dt;
  assert (EvapSoilSurface < 1000.0);
}

double 
Surface::EpFactor () const
{ return impl.EpFactor; }

double
Surface::EpInterchange () const
{ return impl.EpInterchange; }

double
Surface::albedo (const Soil& soil, const SoilWater& soil_water) const
{ return impl.albedo (soil, soil_water); }

double
Surface::Implementation::albedo (const Soil& soil,
				 const SoilWater& soil_water) const
{ 
  const double Theta_pf_3 = soil.Theta (0, pF2h (3.0));
  const double Theta_pf_1_7 = soil.Theta (0, pF2h (1.7));
  const double Theta = soil_water.Theta (0);

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
  assert (n.NO3 >= 0.0);
  assert (n.NH4 >= 0.0);

  im += n;
}

void
Surface::output (Log& log, Filter& filter) const
{ impl.output (log, filter); }

void
Surface::Implementation::output (Log& log, Filter& filter) const
{
  log.output ("pond", filter, pond);
  log.output ("flux", filter, flux);
  log.output ("EvapSoilSurface", filter, EvapSoilSurface, true);
  log.output ("Eps", filter, Eps, true);
  output_submodule (im, "IM", log, filter);
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
Surface::evap_pond () const	// [mm/h]
{ return evap_soil_surface () - exfiltration (); }

void
Surface::put_ponding (double p)	// [mm]
{ impl.pond = p; }
  
void
Surface::put_no3 (double no3) // [g/cm^2]
{ impl.im.NO3 = no3; }

double
Surface::get_no3 () const // [g/cm^2]
{ return impl.im.NO3; }

#ifdef BORLAND_TEMPLATES
template class add_submodule<IM>;
#endif

void
Surface::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("EpFactor", Syntax::None (), Syntax::Const,
	      "Convertion of reference evapotranspiration to \
potential evaporation for bare soil.");
  alist.add ("EpFactor", 0.8);
  syntax.add ("EpInterchange", Syntax::None (), Syntax::Const,
	      "\
Canopy adsorbtion fraction of unreached potential soil evaporation.");
  alist.add ("EpInterchange", 0.6);
  syntax.add ("albedo_dry", Syntax::None (), Syntax::Const,
	      "Albedo of dry soil (pF >= 3)");
  alist.add ("albedo_dry", 0.15);
  syntax.add ("albedo_wet", Syntax::None (), Syntax::Const,
	      "Albedo of wet soil (pf <= 1.7)");
  alist.add ("albedo_wet", 0.08);
  syntax.add ("minimal_matter_flux", Syntax::Number, Syntax::Const);
  alist.add ("minimal_matter_flux", 1.0e-10);
  syntax.add ("total_matter_flux", Syntax::Boolean, Syntax::Const);
  alist.add ("total_matter_flux", false);
  syntax.add ("lake", Syntax::Number, Syntax::Const);
  alist.add ("lake", -1.0);
  syntax.add ("pond", Syntax::Number, Syntax::State);
  alist.add ("pond", 0.0);
  syntax.add ("flux", Syntax::Boolean, Syntax::State);
  alist.add ("flux", true);
  syntax.add ("EvapSoilSurface", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Eps", Syntax::Number, Syntax::LogOnly);
  syntax.add ("T", Syntax::Number, Syntax::LogOnly);
  add_submodule<IM> ("IM", syntax, alist);
}

Surface::Surface (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Surface::Implementation::Implementation (const AttributeList& al)
  : minimal_matter_flux (al.number ("minimal_matter_flux")),
    total_matter_flux (al.flag ("total_matter_flux")),
    EpFactor (al.number ("EpFactor")),
    EpInterchange (al.number ("EpInterchange")),
    albedo_wet (al.number ("albedo_wet")),
    albedo_dry (al.number ("albedo_dry")),
    lake (al.number ("lake")),
    pond (al.number ("pond")),
    flux (al.flag ("flux")),
    im_flux (),
    EvapSoilSurface (0.0),
    Eps (0.0),
    T (0.0),
    im (al.alist ("IM"))
{
  assert (im_flux.NO3 == 0.0);
  assert (im_flux.NH4 == 0.0);
}

Surface::~Surface ()
{ delete &impl; }

Surface::Implementation::~Implementation ()
{ }
