// surface.C

#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "soil_water.h"
#include "log.h"
#include "common.h"
#include "filter.h"
#include "am.h"

extern double abs (double);

bool 
Surface::flux_top () const
{
  return lake < 0.0 && flux;
}

double 
Surface::q () const
{
  return -pond / 10.0;		// mm -> cm.
}
  
void  
Surface::flux_top_on ()
{ 
  flux = true;
}

void  
Surface::flux_top_off ()
{ 
  flux = false;
}

bool  
Surface::accept_top (double water)
{
  if (lake >= 0.0)
    return true;

  water *= 10.0;			// cm -> mm.

  if (pond + water * dt >= - max (abs (pond), abs (water)) / 100)
    {
      if (-water > minimal_matter_flux)
	{
	  InorganicMatter delta_matter (im, (-water * dt) / pond);
	  im -= delta_matter;
	  delta_matter /= dt;
	  im_flux -= delta_matter;
	}
      pond += water * dt;
      return true;
    }
  else
    return false;
}

double
Surface::ponding () const
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
{
  return T;
}

void
Surface::clear ()
{
  im_flux.clear ();
}

const InorganicMatter& 
Surface::matter_flux ()
{
  return im_flux;
}

double
Surface::evaporation (double PotSoilEvaporation, double water, double temp,
		      const Soil& soil, const SoilWater& soil_water)
{
  static const double dt = 1.0; // Time step [h].
  const double MaxExfiltration
    = soil_water.MaxExfiltration (soil) / 10.0; // mm -> cm.
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

  pond = pond - EvapSoilSurface * dt + water * dt;
  return EvapSoilSurface;
}

void 
Surface::fertilize (const InorganicMatter& n)
{ 
  im += n;
}

void
Surface::output (Log& log, const Filter& filter) const
{
  log.output ("pond", filter, pond);
  log.output ("flux", filter, flux);
  log.output ("EvapSoilSurface", filter, EvapSoilSurface, true);
  log.output ("Eps", filter, Eps, true);
  output_submodule (im, "InorganicMatter", log, filter);
}

void
Surface::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("minimal_matter_flux", Syntax::Number, Syntax::Const);
  alist.add ("minimal_matter_flux", 1.0e-10);
  syntax.add ("lake", Syntax::Number, Syntax::Const);
  alist.add ("lake", -1.0);
  syntax.add ("pond", Syntax::Number, Syntax::State);
  alist.add ("pond", 0.0);
  syntax.add ("flux", Syntax::Boolean, Syntax::State);
  alist.add ("flux", true);
  syntax.add ("EvapSoilSurface", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Eps", Syntax::Number, Syntax::LogOnly);
  syntax.add ("T", Syntax::Number, Syntax::LogOnly);
  add_submodule<InorganicMatter> ("InorganicMatter", syntax, alist);
}

Surface::Surface (const AttributeList& al)
  : minimal_matter_flux (al.number ("minimal_matter_flux")),
    lake (al.number ("lake")),
    pond (al.number ("pond")),
    flux (al.flag ("flux")),
    EvapSoilSurface (0.0),
    Eps (0.0),
    T (0.0),
    im (al.list ("InorganicMatter")),
    im_flux ()
{ }
