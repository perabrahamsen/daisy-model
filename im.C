// im.C

#include "im.h"
#include "log.h"
#include "alist.h"
#include "syntax.h"

void
IM::output (Log& log, const Filter& filter) const
{
  log.output ("NO3", filter, NO3);
}

void IM::clear ()
{ 
  NO3 = 0.0;
}

void
IM::operator += (const IM& n)
{ 
  NO3 += n.NO3;
}

void
IM::operator -= (const IM& n)
{ 
  NO3 -= n.NO3;
}

void
IM::operator *= (double n)
{ 
  NO3 *= n;
}

void
IM::operator /= (double n)
{ 
  *this *= (1.0 / n);
}

bool
IM::empty () const
{
  return NO3 < 1e-20;
}

IM::IM ()
  : NO3 (0.0)
{ }

IM::IM (const AttributeList& al)
  : NO3 (al.number ("NO3"))
{ }

IM::IM (const IM& n, double flux)
  : NO3 (n.NO3 * flux)
{ }

IM::~IM ()
{ }

void 
IM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("NO3", Syntax::Number, Syntax::State);
  alist.add ("NO3", 0.0);
}
