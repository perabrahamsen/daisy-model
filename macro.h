// macro.h --- preferential flow.

#ifndef MACRO_H
#define MACRO_H

#include "librarian.h"
#include <vector>

class Soil;
class Log;
class UZtop;

class Macro
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Soil& soil, unsigned int first, unsigned int last,
		     UZtop& surface,
		     const vector<double>& h,
		     const vector<double>& Theta,
		     vector<double>& S,
		     vector<double>& S_p,
		     vector<double>& q_p) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
protected:
  Macro (const AttributeList& al);
public:
  virtual ~Macro ();
};

static Librarian<Macro> Macro_init ("macro");

#endif MACRO_H
