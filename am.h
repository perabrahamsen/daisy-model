// am.h

#ifndef AM_H
#define AM_H

#include <string>
#include <vector>
#include "time.h"
#include "om.h"

class AttributeList;
class Library;
class Syntax;
class Log;
class Filter;
class Soil;
class Time;
class OrganicMatter;

class AM
{
  // Content.
public:
  const Time creation;		// When it was created.
  const string name;		// Name of this kind of am.
  vector<OM*> om;		// Organic matter pool.

  // Simulation.
public:
  void output (Log&, const Filter&) const;
  bool check () const;
  void mix (const Soil&, double from, double to, double penetration = 1.0);
  void swap (const Soil&, double from, double middle, double to);

  // Library.
public:
  static const Library& library ();
  static void derive_type (const string, const AttributeList&, string super);
  // Initialization.
  static AM& create (const AttributeList&, const Soil&);
  // Fertilizer.
  static AM& create (const AttributeList&, const Soil&, const Time&);
  // Crop part.
  static AM& create (const Soil&, const Time&, vector<const AttributeList*>,
		     const string name, const string part, 
		     double C, double N, const vector<double>& density);
  
private:
  AM (const Soil&, const Time&, vector<const AttributeList*>,
      const string name, const string part, 
      double C, double N, const vector<double>& density);
  AM (const AttributeList&, const Soil&, const Time&);
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
