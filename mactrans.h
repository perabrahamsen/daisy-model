// mactrans.h --- Transportation of solute in macropores.

#ifndef MACTRANS_H
#define MACTRANS_H

#include "librarian.h"
#include <vector>

class Soil;
class SoilWater;
class Log;

class Mactrans
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Soil& soil, const SoilWater&,
		     const vector<double>& C,
		     vector<double>& S,
		     vector<double>& S_p,
		     vector<double>& J_p) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
protected:
  Mactrans (const AttributeList& al);
public:
  virtual ~Mactrans ();
};

static Librarian<Mactrans> Mactrans_init ("mactrans");

#endif MACTRANS_H
