// surface.C

#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "soil_water.h"
#include "log.h"

extern double abs (double);

bool 
Surface::flux_top () const
{
  return lake < 0.0 && flux;
}

double 
Surface::q () const
{
  assert (flux_top ());
  return -pond / 10;		// mm -> cm.
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

  water *= 10;			// cm -> mm.

  static const double dt = 1.0; // Time step [h].

  if (pond + water * dt >= - max (abs (pond), abs (water)) / 1000)
    {
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
Surface::evaporation (double PotSoilEvaporation, double water, 
		      const Soil& soil, const SoilWater& soil_water)
{
  static const double dt = 1.0; // Time step [h].
  const double MaxExfiltration = soil_water.MaxExfiltration (soil) / 10; // mm -> cm.
  Eps = PotSoilEvaporation;

  if (pond + water * dt < Eps * dt)
    flux_top_on ();

  if (pond + water * dt + MaxExfiltration * dt < Eps * dt)
    Es = pond / dt + water + MaxExfiltration;
  else
    Es = Eps;

  pond = pond - Es * dt + water * dt;
  return Es;
}

void
Surface::output (Log& log, const Filter* filter) const
{
  log.output ("pond", filter, pond);
  log.output ("flux", filter, flux);
  log.output ("Es", filter, Es, true);
  log.output ("Eps", filter, Eps, true);
}

void
Surface::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("lake", Syntax::Number, Syntax::Const);
  alist.add ("lake", -1.0);
  syntax.add ("pond", Syntax::Number, Syntax::InOut);
  alist.add ("pond", 0.0);
  syntax.add ("flux", Syntax::Boolean, Syntax::InOut);
  alist.add ("flux", true);
  syntax.add ("Es", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Eps", Syntax::Number, Syntax::LogOnly);
}

Surface::Surface (const AttributeList& al)
  : lake (al.number ("lake")),
    pond (al.number ("pond")),
    flux (al.flag ("flux")),
    Es (0.0),
    Eps (0.0)
{ }
