// am.h

#ifndef AM_H
#define AM_H

#include "librarian.h"
#include <vector>

class Geometry;
class Time;
class OM;

class AM
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  const string name;
  void append_to (vector<OM*>& added);
  
  // Simulation.
public:
  void output (Log&, Filter&) const;
  bool check () const;
  void mix (const Geometry&, double from, double to, double penetration = 1.0);
  void swap (const Geometry&, double from, double middle, double to);
  double total_C (const Geometry& geometry) const;
  double total_N (const Geometry& geometry) const;
  void pour (vector<double>& cc, vector<double>& nn);
  void add (double C, double N);// Add dead leafs.
  void add (const Geometry&,	// Add dead roots.
	    double C, double N, 
	    const vector<double>& density);

  // Crop Locks.
  enum lock_type { Unlocked, Locked };
  void unlock ();		// Crop died.
  bool locked () const;		// Test if this AM can be safely removed.
  const string crop_name () const;	// Name of locked crop.
  const string crop_part_name () const; // Name of locked crop part.

  // Create and Destroy.
public:
  // Initialization & Fertilizer.
  static AM& create (const AttributeList&, const Geometry&);
  // Crop part.
  static AM& create (const Geometry&, const Time&,
		     const vector<AttributeList*>&,
		     const string& sort, const string& part,
		     lock_type lock = Unlocked);
  void initialize (const Geometry&);
  AM (const AttributeList&);
  virtual ~AM ();
};

static Librarian<AM> AM_init ("am");

#endif AM_H
