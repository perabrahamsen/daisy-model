// chemicals.h

#ifndef CHEMICALS_H
#define CHEMICALS_H

#include "syntax.h"
#include <vector>

class Log;
class Filter;
class AttributeList;

class Chemicals
{ 
  // Content.
private:
  struct Implementation;	// Top secret internal state.
  Implementation& impl;

  // Utilities.
public:
  static void move_fraction (Chemicals& from, Chemicals& to, double fraction);
  static void copy_fraction (const Chemicals& from, Chemicals& to,
			     double fraction);

  // Canopy functions.
public:
  void canopy_update (const Chemicals& canopy_chemicals_in, 
		      double canopy_water_storage,
		      double canopy_water_out);
  void canopy_dissipate (Chemicals& canopy_chemicals_dissipate) const;
  void canopy_out (Chemicals& canopy_chemicals_out,
		   double canopy_water_storage,
		   double canopy_water_out) const;

  // Simulation
public:
  void output (Log&, Filter&) const;
  void add (const string& chemical, double amount); // [g/m^2]

  // Create and Destroy.
public:
  void clear ();
  void cleanup (Chemicals& out);
  void operator += (const Chemicals&);
  void operator = (const Chemicals&);
  Chemicals (const Chemicals&);
  static void add_syntax (const char* name, Syntax& syntax, 
			  AttributeList& alist,
			  Syntax::category cat, 
			  const string& description);
  Chemicals ();
  Chemicals (const vector<AttributeList*>&);
  ~Chemicals ();
};

#endif CHEMICALS_H
