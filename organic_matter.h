// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

class AttributeList;
class Syntax;
class Log;
class Filter;
class AM;
class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class SoilNH4;

class OrganicMatter
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
  
  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&);
  void output (Log&, const Filter&, const Soil&) const;
  double CO2 (int i) const;
  void mix (const Soil&, double from, double to, double penetration = 1.0);
  void swap (const Soil&, double from, double middle, double to);

  static bool check (const AttributeList&);
  bool check () const;
  bool check_am (const AttributeList& am) const;
  void add (AM&);

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  OrganicMatter (const Soil&, const AttributeList&);
  ~OrganicMatter ();
};

#endif ORGANIC_MATTER_H
