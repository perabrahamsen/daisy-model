// inorganic_matter.C

#include "inorganic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "log.h"
#include "iom.h"

void
InorganicMatter::output (Log& log, const Filter& filter) const
{
  iom.output (log, filter);
}

void InorganicMatter::clear ()
{ 
  iom.clear ();
}

void
InorganicMatter::operator += (const InorganicMatter& n)
{ 
  iom += n.iom;
}

void
InorganicMatter::operator -= (const InorganicMatter& n)
{ 
  iom -= n.iom;
}

void
InorganicMatter::operator *= (double n)
{ 
  iom *= n;
}

void
InorganicMatter::operator /= (double n)
{ 
  iom /= n;
}

InorganicMatter::InorganicMatter ()
  : iom (*new IOM ())
{ }

InorganicMatter::InorganicMatter (const AttributeList& al)
  : iom (*new IOM (al))
{ }

InorganicMatter::InorganicMatter (const InorganicMatter& n, double flux)
  : iom (*new IOM (n.iom, flux))
{ }

InorganicMatter::~InorganicMatter ()
{ }

void 
InorganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{
  IOM::load_syntax (syntax, alist);
}

InorganicMatter 
SoluteMatter::operator* (double flux) const
{
  return InorganicMatter (*this, flux);
}

void 
SoluteMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{
  IOM::load_syntax (syntax, alist);
}

SoluteMatter::SoluteMatter (const AttributeList& al)
  : InorganicMatter (al)
{ }

SoluteMatter::~SoluteMatter ()
{ }
