// ridge.h

#ifndef RIDGE_H
#define RIDGE_H

struct AttributeList;
struct Syntax;
struct Log;
struct Soil;
struct SoilWater;

#include <vector>
using namespace std;

class Ridge
{ 
  struct Implementation;
  Implementation& impl;

  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, 
	     double external_ponding /* [mm] */);
  void update_water (const Soil&, const vector<double>& S_,
		     vector<double>& h_, vector<double>& Theta_,
		     vector<double>& q, const vector<double>& q_p);
  void output (Log&) const;
  int last_node () const;
  double h () const;		// [cm]
  double exfiltration () const;	// [mm]

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil&, const SoilWater&);
  Ridge (const AttributeList& al);
  ~Ridge ();
};

#endif // RIDGE_H
