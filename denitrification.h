// denitrification.h

#ifndef DENITRIFICATION_H
#define DENITRIFICATION_H

#include "common.h"
#include "csmp.h"
#include <vector>
class AttributeList;
class Syntax;
class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class OrganicMatter;
class Log;

class Denitrification
{
  // Parameters.
private: 
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double K;
  const double alpha;
  const CSMP heat_factor;
  const CSMP water_factor;

  // Log variable.
private:
  vector<double> converted;
  
  // Simulation.
public:
  void output (Log&) const;
  void tick (const Soil&, const SoilWater&, const SoilHeat&, SoilNO3&,
	     const OrganicMatter&);

  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Denitrification (const AttributeList&);
};


#endif DENITRIFICATION_H
