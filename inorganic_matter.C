// inorganic_matter.C

#include "inorganic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "log.h"

void
InorganicMatter::output (Log& log, const Filter& filter) const
{
  log.output ("NO3", filter, NO3);
}

void InorganicMatter::clear ()
{ 
  NO3 = 0.0;
}

void
InorganicMatter::operator += (const InorganicMatter& n)
{ 
  NO3 += n.NO3;
}

void
InorganicMatter::operator -= (const InorganicMatter& n)
{ 
  NO3 -= n.NO3;
}

void
InorganicMatter::operator *= (double n)
{ 
  NO3 *= n;
}

void
InorganicMatter::operator /= (double n)
{ 
  *this *= (1.0 / n);
}

InorganicMatter::InorganicMatter ()
  : NO3 (0.0)
{ }

InorganicMatter::InorganicMatter (const AttributeList& al)
  : NO3 (al.number ("NO3"))
{ }

InorganicMatter::InorganicMatter (const InorganicMatter& n, double flux)
  : NO3 (n.NO3 * flux)
{ }

InorganicMatter::~InorganicMatter ()
{ }

const InorganicMatter& MakeInorganicMatter (const AttributeList& al)
{
  return *new InorganicMatter (al);
}

const Syntax& InorganicMatterSyntax ()
{ 
  static Syntax* syntax = 0;

  if (syntax == 0)
    {
      syntax = new Syntax;
      syntax->add ("NO3", Syntax::Number, Syntax::State);
    }
  return *syntax;
}

AttributeList& InorganicMatterAlist ()
{ 
  static AttributeList* alist = 0;

  if (alist == 0)
    {
      alist = new AttributeList;
      alist->add ("NO3", 0.0);
    }
  return *alist;
}

InorganicMatter 
SoluteMatter::operator* (double flux) const
{
  return InorganicMatter (*this, flux);
}

SoluteMatter::SoluteMatter (const AttributeList& al)
  : InorganicMatter (al)
{ }

SoluteMatter::~SoluteMatter ()
{ }

const SoluteMatter& MakeSoluteMatter (const AttributeList& al)
{
  return *new SoluteMatter (al);
}

const Syntax& SoluteMatterSyntax ()
{ return InorganicMatterSyntax (); }

AttributeList& SoluteMatterAlist ()
{ return InorganicMatterAlist (); }
