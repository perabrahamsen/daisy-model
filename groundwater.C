// groundwater.C

#include "groundwater.h"
#include "log.h"

Librarian<Groundwater>::Content* Librarian<Groundwater>::content = NULL;

void
Groundwater::output (Log& log, Filter& filter) const
{ 
  log.output ("height", filter, table (), true);
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

