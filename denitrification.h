// denitrification.h

#ifndef DENITRIFICATION_H
#define DENITRIFICATION_H

#include "common.h"
#include <vector>
class AttributeList;
class Syntax;
class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class OrganicMatter;
class CSMP;
class Log;
class Filter;

class Denitrification
{
  // Parameters.
private: 
  const double K;
  const double alpha;

  // Log variable.
private:
  vector<double> converted;
  
  // Simulation.
public:
  void output (Log&, Filter&) const;
  void tick (Soil&, SoilWater&, SoilHeat&, SoilNO3&, OrganicMatter&);

  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Denitrification (const AttributeList&);
};


#endif DENITRIFICATION_H
