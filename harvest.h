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
  const double leaf_DM;
  const double leaf_N;
  const double sorg_DM;
  const double sorg_N;
  const Chemicals chemicals;

  // Simulation:
public:
  void output (Log&) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Harvest (const AttributeList& al);
  Harvest (string col, Time t, string crp, 
	   double sC, double sN, double lC, double lN, 
	   double oC, double oN,
	   const Chemicals& chem);
};      

#endif HARVEST_H
