// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

class AttributeList;
class Syntax;
class Log;
class Filter;
class AOM;

class OrganicMatter
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
  
  // Simulation.
public:
  void add (AOM&);
  void output (Log& log, const Filter& filter) const;
  static bool check (const AttributeList&);
  bool check () const;

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  OrganicMatter (const AttributeList&);
  ~OrganicMatter ();
};

#endif ORGANIC_MATTER_H
