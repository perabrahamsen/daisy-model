// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

class AttributeList;
class Syntax;
class Log;
class Filter;

class OrganicMatter
{
  struct Implementation;
  Implementation& impl;
public:
  void output (Log& log, const Filter& filter) const;
  static void load_syntax (Syntax&, AttributeList&);
  OrganicMatter (const AttributeList&);
  ~OrganicMatter ();
};

#endif ORGANIC_MATTER_H
