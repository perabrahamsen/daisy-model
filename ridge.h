// ridge.h

#ifndef RIDGE_H
#define RIDGE_H

struct AttributeList;
struct Syntax;
struct Log;
struct Soil;
struct SoilWater;

class Ridge
{ 
  struct Implementation;
  Implementation& impl;

  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, 
	     double external_ponding /* [mm] */);
  void update_water (const Soil& soil, const SoilWater& soil_water);
  void output (Log&) const;
  unsigned int node_below () const;
  double h () const;		// [cm]

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil&, const SoilWater&);
  Ridge (const AttributeList& al);
  ~Ridge ();
};

#endif RIDGE_H
