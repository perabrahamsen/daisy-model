// inorganic_matter.C

#include "inorganic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "log.h"
#include "im.h"

void
InorganicMatter::output (Log& log, const Filter& filter) const
{
  im.output (log, filter);
}

void InorganicMatter::clear ()
{ 
  im.clear ();
}

void
InorganicMatter::operator += (const InorganicMatter& n)
{ 
  im += n.im;
}

void
InorganicMatter::operator -= (const InorganicMatter& n)
{ 
  im -= n.im;
}

void
InorganicMatter::operator *= (double n)
{ 
  im *= n;
}

void
InorganicMatter::operator /= (double n)
{ 
  im /= n;
}

bool
InorganicMatter::empty () const
{
  return im.empty ();
}

InorganicMatter::InorganicMatter ()
  : im (*new IM ())
{ }

InorganicMatter::InorganicMatter (const AttributeList& al)
  : im (*new IM (al))
{ }

InorganicMatter::InorganicMatter (const InorganicMatter& n, double flux)
  : im (*new IM (n.im, flux))
{ }

InorganicMatter::InorganicMatter (const SoluteMatter& n, double flux)
  : im (*new IM (n.im, flux))
{ }

InorganicMatter::~InorganicMatter ()
{ }

void 
InorganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{
  IM::load_syntax (syntax, alist);
}

InorganicMatter 
SoluteMatter::operator* (double flux) const
{
  return InorganicMatter (*this, flux);
}

void 
SoluteMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{
  IM::load_syntax (syntax, alist);
}

SoluteMatter::SoluteMatter (const AttributeList& al)
  : InorganicMatter (al)
{ }

SoluteMatter::~SoluteMatter ()
{ }
