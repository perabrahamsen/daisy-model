// pt.C  -- Potential transpiration

#include "pt.h"
#include "log.h"

Librarian<PT>::Content* Librarian<PT>::content = NULL;

const char *const PT::description = "\
The task of the `pt' component is to calculate the potential\n\
transpiration, given the potential evapotranspiration, the actual\n\
evaporation from the surface, meteorological data, and the vegetation\n\
and soil state.";

void
PT::output (Log& log, Filter& filter) const
{
  log.output ("potential_transpiration", 
	      filter, potential_transpiration (), true);
}

void 
PT::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("potential_transpiration", "mm/h", Syntax::LogOnly,
	      "Potential transpiration calculated this hour.");
}

PT::PT (const AttributeList& al)
  : name (al.name ("type"))
{ }

PT::~PT ()
{ }

// pt.C ends here.
