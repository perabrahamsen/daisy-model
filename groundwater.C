// groundwater.C

#include "groundwater.h"
#include "log.h"

Librarian<Groundwater>::Content* Librarian<Groundwater>::content = NULL;

const char *const Groundwater::description = "\
The `groundwater' component is responsible for specifying the\n\
groundwater table at each timestep.";

void
Groundwater::output (Log& log) const
{ 
  log.output ("height", table ());
}

void
Groundwater::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("height", "cm", Syntax::LogOnly,
	      "Groundwater level.  Positive numbers indicate free drainage.");
}

void
Groundwater::initialize (const Time&)
{ }

Groundwater::Groundwater (const string& n)
  : name (n)
{ }

Groundwater::~Groundwater ()
{ }

