// pt.C  -- Potential transpiration

#include "pt.h"
#include "log.h"

Librarian<PT>::Content* Librarian<PT>::content = NULL;

void
PT::output (Log& log, Filter& filter) const
{
  log.output ("potential_transpiration", 
	      filter, potential_transpiration (), true);
}

void 
PT::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("potential_transpiration", Syntax::Number, Syntax::LogOnly);
}

PT::PT (const AttributeList& al)
  : name (al.name ("type"))
{ }

PT::~PT ()
{ }

// pt.C ends here.
