// adsorbtion.h

#ifndef ADSORBTION_H
#define ADSORBTION_H

#include "librarian.h"

class Soil;

class Adsorbtion
{
  // Content.
public:
  const string name;

  // Simulation.
public:
  virtual double C_to_M (const Soil&, double Theta, int i, double C) const = 0;
  virtual double M_to_C (const Soil&, double Theta, int i, double M) const = 0;

  // Create and Destroy.
protected:
  Adsorbtion (const string& name);
public:
  virtual ~Adsorbtion ();
};

static Librarian<Adsorbtion> Adsorbtion_init ("adsorbtion");

#endif ADSORBTION_H
