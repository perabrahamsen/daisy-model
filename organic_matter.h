// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

class AttributeList;
class Syntax;

class OrganicMatter
{
public:
  static void load_syntax (Syntax&, AttributeList&);
  OrganicMatter (const AttributeList&);
};


#endif ORGANIC_MATTER_H
