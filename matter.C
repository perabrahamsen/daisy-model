// matter.C

#include "matter.h"
#include "syntax.h"
#include "alist.h"

InorganicMatter::InorganicMatter (const AttributeList&)
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
    }
  return *syntax;
}

AttributeList& InorganicMatterAlist ()
{ 
  static AttributeList* alist = 0;

  if (alist == 0)
    {
      alist = new AttributeList;
    }
  return *alist;
}

SoluteMatter::SoluteMatter (const AttributeList&)
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
