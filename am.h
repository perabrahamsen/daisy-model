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
  static bool check (const AttributeList&);
  bool check () const;
  void mix (const Soil&, double from, double to, double penetration = 1.0);
  void swap (const Soil&, double from, double middle, double to);

  // Library.
public:
  static const Library& library ();
  static void derive_type (const string, const AttributeList&, string super);
  static AM& create (const AttributeList&);
  static AM& create (const Time&, const AttributeList&);
  static AM& create (const Time&, const AttributeList&, 
		      const string name, const string part,
		      double C, double N);
  static AM& create (const Time&, const AttributeList&, 
		      const string name, const string part,
		      double C, double N, const vector<double>& density);

  // Create and Destroy.
private: 
  static vector<OM*>& create_om (const AttributeList&);
  static vector<OM*>& create_om (const AttributeList& al, 
				 double crop_C, double crop_N,
				 const vector<double>& density);
  static vector<OM*>& create_om (const AttributeList& al, 
				 double crop_C, double crop_N);
public:
  void initialize (const Soil& soil);
private:
  AM (const Time&, const AttributeList&, 
       const string name, const string part,
       double C, double N, const vector<double>& density);
  AM (const Time&, const AttributeList&);
  AM (const AttributeList&);
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
