// om.h

#ifndef OM_H
#define OM_H

#include <string>
#include <vector>

class AttributeList;
class Syntax;
class Log;
class Filter;
class Soil;

class OM
{ 
  // Content.
public:
  double top_C;			// Carbon on the ground.
  vector<double> C;		// Carbon in each node.
  /* const */ vector<double> C_per_N;	// Ratio of carbon per nitrogen.
  const double turnover_rate;	// How fast this is it turned over?
  const vector <double> efficiency;	// How digestible is this?
  const double maintenance;	// How fast does it eat itself?
  const vector<double> fractions;	// How much is turned into SMB and SOM?

  // Simulation.
public:
  void output (Log&, const Filter&) const;
  void mix (const Soil&, double from, double to, double penetration = 1.0);
  void distribute (const vector<double>& density);
  void swap (const Soil&, double from, double middle, double to);
  void tick (int i, double turnover_factor, double N_soil, double& N_used,
	     double& CO2, const vector<OM*>& smb, const vector<OM*>&som);
  void tick (int i, double turnover_factor, double N_soil, double& N_used,
	     double& CO2, const vector<OM*>& smb, double& som_C,double& som_N);
private:
  void tock (int i, double rate, double efficiency, 
	     double N_soil, double& N_used,
	     double& CO2, OM& om);
  
  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil& soil);
  OM (const AttributeList& al);
  OM (const AttributeList& al, double C, double N);
};

#endif OM_H
