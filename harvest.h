// harvest.h

#ifndef HARVEST_H
#define HARVEST_H

#include "chemicals.h"
#include "time.h"
#include <string>

class AttributeList;
class Syntax;
class Log;

class Harvest
{
  // Content:
public:
  const string column;
  const Time time;
  const string crop;
  const double stem_DM;
  const double stem_N;
  const double stem_C;
  const double dead_DM;
  const double dead_N;
  const double dead_C;
  const double leaf_DM;
  const double leaf_N;
  const double leaf_C;
  const double sorg_DM;
  const double sorg_N;
  const double sorg_C;
  const Chemicals chemicals;

  // Simulation:
public:
  void output (Log&) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Harvest (const AttributeList& al);
  Harvest (string col, Time t, string crp, 
	   double sDM, double sN, double sC, double dDM, double dN, double dC,
	   double lDM, double lN, double lC, double oDM, double oN, double oC,
	   const Chemicals& chem);
};      

#endif // HARVEST_H
