// am.h

#ifndef AM_H
#define AM_H

#include "common.h"
#include <vector>

class AttributeList;
class Library;
class Syntax;
class Log;
class Filter;
class Geometry;
class Time;
class OrganicMatter;
class OM;

class AM
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
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

  // Library.
public:
  static Library& library ();
  static void derive_type (const string&, AttributeList&, const string& super);
  // Initialization.
  static AM& create (const AttributeList&, const Geometry&);
  // Fertilizer.
  static AM& create (const AttributeList&, const Geometry&, const Time&);
  // Crop part.
  static AM& create (const Geometry&, const Time&,
		     vector<AttributeList*>,
		     const string name, const string part,
		     lock_type lock = Unlocked);
  
private:
  AM (const Geometry&, const Time&, vector<AttributeList*>,
      const string name, const string part);
  AM (const AttributeList&, const Geometry&, const Time&);
public:
  virtual ~AM ();
};

// Ensure the AM library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class AM_init
{
  static int count;
public:
  AM_init ();
  ~AM_init ();
} AM_init;

#endif AM_H
