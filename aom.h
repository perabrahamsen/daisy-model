// aom.h

#ifndef AOM_H
#define AOM_H

#include <string>
#include <vector>
#include "time.h"
#include "inorganic_matter.h"

class AttributeList;
class Library;
class Syntax;
class Log;
class Filter;
class Soil;
class Time;
class OrganicMatter;

struct OM  { 
  // Content.
  double top_C;			// Carbon on the ground.
  vector<double> C;		// Carbon in each node.
  const double C_per_N;	// Ratio of carbon per nitrogen.
  const double turnover_rate;	// How fast this is it turned over?
  const double efficiency;	// How digestible is this?
  const double maintenance;	// How fast does it eat itself?
  const vector<double> fractions;	// How much is turned into SMB and SOM?

  // Simulation.
  void output (Log&, const Filter&) const;
  void mix (const Soil&, double from, double to);
  void tick (int i, double turnover_factor, double N_soil, double& N_used,
	     double& CO2, const vector<OM*>& smb, const vector<OM*>&som);
  void tick (int i, double turnover_factor, double N_soil, double& N_used,
	     double& CO2, const vector<OM*>& smb, double& som_C,double& som_N);
private:
  void tock (int i, double rate, double N_soil, double& N_used,
	     double& CO2, OM& om);
  
  // Create & Destroy.
public:
  static const Syntax& syntax ();
  OM (const AttributeList& al);
  OM (const AttributeList& al, double C, double N);
};

class AOM
{
  // Content.
public:
  const Time creation;		// When it was created.
  const string name;		// Name of this kind of aom.
  vector<OM*> om;		// Organic matter pool.

  // Simulation.
public:
  void output (Log&, const Filter&) const;
  static bool check (const AttributeList&);
  bool check () const;
  void mix (const Soil&, double from, double to);

  // Library.
public:
  static const Library& library ();
  static void derive_type (const string, const AttributeList&, string super);
  static AOM& create (const Time&, const AttributeList&);

  // Create and Destroy.
private: 
  static vector<OM*>& create_om (const AttributeList&);
public:
  static InorganicMatter im (const AttributeList&);
  AOM (const Time&, const AttributeList&);
  AOM (const AttributeList&);
  virtual ~AOM ();
};

// Ensure the AOM library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class AOM_init
{
  static int count;
public:
  AOM_init ();
  ~AOM_init ();
} AOM_init;

#endif AOM_H
