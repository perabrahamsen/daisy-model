// surface.C

#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "soil_water.h"
#include "log.h"
#include "common.h"
#include "matter.h"
#include "filter.h"

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
	  InorganicMatter delta_matter (iom, (water * dt) / pond);
	  iom -= delta_matter;
	  delta_matter /= dt;
	  iom_flux += delta_matter;
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

void
Surface::clear ()
{
  iom_flux.clear ();
}

const InorganicMatter& 

Surface::matter_flux ()
{
  return iom_flux;
}

double
Surface::evaporation (double PotSoilEvaporation, double water, 
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

  pond = pond - EvapSoilSurface * dt + water * dt;
  return EvapSoilSurface;
}

void
Surface::fertilize (const OrganicMatter& n)
{ 
  om += n;
}

void 
Surface::fertilize (const InorganicMatter& n)
{ 
  iom += n;
}

void
Surface::output (Log& log, const Filter& filter) const
{
  log.output ("pond", filter, pond);
  log.output ("flux", filter, flux);
  log.output ("EvapSoilSurface", filter, EvapSoilSurface, true);
  log.output ("Eps", filter, Eps, true);
  output_submodule (om, "OrganicMatter", log, filter);
  output_submodule (iom, "InorganicMatter", log, filter);
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
  syntax.add ("OrganicMatter", OrganicMatterSyntax (), Syntax::State);
  alist.add ("OrganicMatter", OrganicMatterAlist ());
  syntax.add ("InorganicMatter", InorganicMatterSyntax (), Syntax::State);
  alist.add ("InorganicMatter", InorganicMatterAlist ());
}

Surface::Surface (const AttributeList& al)
  : minimal_matter_flux (al.number ("minimal_matter_flux")),
    lake (al.number ("lake")),
    pond (al.number ("pond")),
    flux (al.flag ("flux")),
    EvapSoilSurface (0.0),
    Eps (0.0),
    om (al.list ("OrganicMatter")),
    iom (al.list ("InorganicMatter")),
    iom_flux ()
{ }
