// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

class AttributeList;
class Syntax;

class OrganicMatter
{ 
public:
  OrganicMatter (const AttributeList&);
  ~OrganicMatter ();
};

class SoilOrganicMatter
{
public:
  static void load_syntax (Syntax&, AttributeList&);
  SoilOrganicMatter (const AttributeList&);
};


#endif ORGANIC_MATTER_H
