// vegetation.C

#include "vegetation.h"
#include "log.h"

Librarian<Vegetation>::Content* Librarian<Vegetation>::content = NULL;

const char *const Vegetation::description = "\
That green stuff.";

void 
Vegetation::force_production_stress  (double)
{ }

void
Vegetation::output (Log& log) const
{
  log.output ("LAI", LAI ());
  log.output ("height", height ());
  log.output ("cover", cover ());
  log.output ("LAIvsH", LAIvsH ());
  log.output ("HvsLAI", HvsLAI ());
  log.output ("ACExt", ACExt ());
  log.output ("ACRef", ACRef ());
  log.output ("ARExt", ARExt ());
  log.output ("ARExt", ARExt ());
  log.output ("EpFactor", EpFactor ());
  log.output ("albedo", albedo ());
  log.output ("interception_capacity", interception_capacity ());
}

void
Vegetation::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this vegetation.");
  syntax.add ("LAI", "m^2/m^2", Syntax::LogOnly,
	      "Total LAI of all crops on this column");
  syntax.add ("height", "cm", Syntax::LogOnly,
	      "Max crop height in canopy");
  syntax.add ("cover", "m^2/m^2", Syntax::LogOnly,
	      "Fraction of soil covered by crops");
  syntax.add ("LAIvsH", "m^2/m^2", "cm", Syntax::LogOnly,
	      "Total canopy LAI below given height");
  syntax.add ("HvsLAI", "cm", "m^2/m^2", Syntax::LogOnly, "\
Height in which there is a given LAI below in total canopy");
  syntax.add ("ACExt", Syntax::None (), Syntax::LogOnly,
	      "Canopy extinction coefficient\n\
\(how fast the light dim as a function of LAI passed)");
  syntax.add ("ACRef", Syntax::None (), Syntax::LogOnly,
	      "Canopy reflection coefficient");
  syntax.add ("ARExt", Syntax::None (), Syntax::LogOnly,
	      "Radiation Extinction coefficient\n\
\(like ACExt, but for all radiation, not just light)");
  syntax.add ("EpFactor", Syntax::None (), Syntax::LogOnly,
	      "Reference to potential evapotranspiration");
  syntax.add ("albedo", Syntax::None (), Syntax::LogOnly,
	      "Another reflection factor");
  syntax.add ("interception_capacity", "mm", Syntax::LogOnly,
	      "Canopy water storage capacity");
}

Vegetation::Vegetation (const AttributeList& al)
  : name (al.name ("type"))
{ }

Vegetation::~Vegetation ()
{ }
