// iom.C

#include "iom.h"
#include "log.h"
#include "alist.h"
#include "syntax.h"

void
IOM::output (Log& log, const Filter& filter) const
{
  log.output ("NO3", filter, NO3);
}

void IOM::clear ()
{ 
  NO3 = 0.0;
}

void
IOM::operator += (const IOM& n)
{ 
  NO3 += n.NO3;
}

void
IOM::operator -= (const IOM& n)
{ 
  NO3 -= n.NO3;
}

void
IOM::operator *= (double n)
{ 
  NO3 *= n;
}

void
IOM::operator /= (double n)
{ 
  *this *= (1.0 / n);
}

IOM::IOM ()
  : NO3 (0.0)
{ }

IOM::IOM (const AttributeList& al)
  : NO3 (al.number ("NO3"))
{ }

IOM::IOM (const IOM& n, double flux)
  : NO3 (n.NO3 * flux)
{ }

IOM::~IOM ()
{ }

void 
IOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("NO3", Syntax::Number, Syntax::State);
  alist.add ("NO3", 0.0);
}
