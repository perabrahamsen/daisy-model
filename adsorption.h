// adsorption.h

#ifndef ADSORPTION_H
#define ADSORPTION_H

#include "librarian.h"

class Soil;

class Adsorption
{
  // Content.
public:
  static const char *const description;
  const string name;

  // Simulation.
public:
  virtual void output (Log&, Filter&) const;
  virtual double C_to_M (const Soil&, double Theta, int i, double C) const = 0;
  virtual double M_to_C (const Soil&, double Theta, int i, double M) const = 0;

  // Create and Destroy.
protected:
  Adsorption (const string& name);
public:
  virtual ~Adsorption ();
};

static Librarian<Adsorption> Adsorption_init ("adsorption");

#endif ADSORPTION_H
