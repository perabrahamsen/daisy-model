// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

class AttributeList;
class Syntax;
class Log;
class Filter;
class AOM;
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
  void output (Log& log, const Filter& filter) const;
  double CO2 (int i) const;

  static bool check (const AttributeList&);
  bool check () const;
  bool check_am (const AttributeList& am) const;
  void add (AOM&);

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil& soil);
  OrganicMatter (const AttributeList&);
  ~OrganicMatter ();
};

#endif ORGANIC_MATTER_H
