// organic_matter.C

#include "organic_matter.h"
#include "matter.h"
#include "syntax.h"
#include "alist.h"

OrganicMatter::OrganicMatter (const AttributeList&)
{ }

OrganicMatter::~OrganicMatter ()
{ }

const OrganicMatter& MakeOrganicMatter (const AttributeList& al)
{
  return *new OrganicMatter (al);
}

const Syntax& OrganicMatterSyntax ()
{
  static Syntax* syntax = 0;

  if (syntax == 0)
    {
      syntax = new Syntax;
    }
  return *syntax;
}

AttributeList& OrganicMatterAttributeList ()
{ 
  static AttributeList* alist = 0;

  if (alist == 0)
    {
      alist = new AttributeList;
    }
  return *alist;
}

void
SoilOrganicMatter::load_syntax (Syntax&, AttributeList&)
{ }

SoilOrganicMatter::SoilOrganicMatter (const AttributeList&)
{ }
