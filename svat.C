// svat.C  -- Soil, Vegetation and ATmostphere.

#include "svat.h"
#include "log.h"

Librarian<SVAT>::Content* Librarian<SVAT>::content = NULL;

const char *const SVAT::description = "\
The task of the `svat' component is to calculate the production\n\
stress, given the potential evapotranspiration, the actual\n\
evaporation from the surface, meteorological data, and the vegetation\n\
and soil state.";

void
SVAT::output (Log&) const
{ }

void 
SVAT::load_syntax (Syntax&, AttributeList&)
{ }

SVAT::SVAT (const AttributeList& al)
  : name (al.name ("type"))
{ }

SVAT::~SVAT ()
{ }

// svat.C ends here.
