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
class Groundwater;
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
  void monthly (const Soil& soil);
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     const Groundwater&, SoilNO3&, SoilNH4&);
  void output (Log&, Filter&, const Soil&) const;
  double CO2 (unsigned int i) const;	// [g C/cm³]
  void mix (const Soil&, double from, double to, double penetration = 1.0);
  void swap (const Soil&, double from, double middle, double to);

  // Communication with external model.
  double get_smb_c_at (unsigned int i) const; // [g C/cm³]

  // Create and Destroy.
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
