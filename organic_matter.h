// organic_matter.h

#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

#include "common.h"

class AttributeList;
class Syntax;
class Log;
class AM;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class SoilNH4;
class Time;
class Treelog;

class OrganicMatter
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
  
  // Simulation.
public:
  void monthly (const Geometry& soil);
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&);
  void output (Log&, const Geometry&) const;
  double CO2 (unsigned int i) const;	// [g C/cm³]
  void mix (const Geometry&, double from, double to, double penetration, 
	    const Time& time);
  void swap (const Geometry&, double from, double middle, double to, 
	     const Time& time);

  // Communication with external model.
  double get_smb_c_at (unsigned int i) const; // [g C/cm³]

  // Create and Destroy.
  bool check (Treelog& err) const;
  bool check_am (const AttributeList& am, Treelog& err) const;
  void add (AM&);
  AM* find_am (const string& sort, const string& part) const;
public:
  void initialize (const AttributeList&, const Soil&);
  static void load_syntax (Syntax&, AttributeList&);
  OrganicMatter (const AttributeList&);
  ~OrganicMatter ();
};

#endif // ORGANIC_MATTER_H
