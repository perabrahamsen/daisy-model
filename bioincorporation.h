// bioincorporation.h --- Biological incorporation of organic matter in soil.

#ifndef BIOINCORPORATION_H
#define BIOINCORPORATION_H

#include "common.h"
#include <vector>

class AttributeList;
class Syntax;
class Log;
class AM;
class Geometry;
class Soil;

class Bioincorporation
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
  
  // Simulation.
public:
  void tick (const Geometry&, vector <AM*>&, double T, double& CO2);
  void output (Log&, const Geometry&) const;

  // Create and Destroy.
public:
  void initialize (const Soil&);
  AM* create_am (const Geometry&);
  void set_am (AM*);
  static void load_syntax (Syntax&, AttributeList&);
  Bioincorporation (const AttributeList&);
  ~Bioincorporation ();
};

#endif BIOINCORPORATION_H
