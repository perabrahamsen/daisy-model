// pet.C  -- Potential evopotranspiration

#include "pet.h"
#include "log.h"

Librarian<Pet>::Content* Librarian<Pet>::content = NULL;

double
Pet::dry () const 
{ return wet (); }

void
Pet::output (Log& log, Filter& filter) const
{
  log.output ("wet", filter, wet (), true);
  log.output ("dry", filter, dry (), true);
}

void 
Pet::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("wet", Syntax::Number, Syntax::LogOnly);
  syntax.add ("dry", Syntax::Number, Syntax::LogOnly);
}

Pet::Pet (const AttributeList& al)
  : name (al.name ("type"))
{ }

Pet::~Pet ()
{ }

