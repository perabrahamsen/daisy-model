// tortuosity.h

#ifndef TORTUOSITY_H
#define TORTUOSITY_H

#include "librarian.h"

class Hydraulic;

class Tortuosity
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual double factor (const Hydraulic&, double Theta) const = 0;

  // Create and Destroy.
protected:
  Tortuosity (const string& name);
public:
  virtual ~Tortuosity ();
};

static Librarian<Tortuosity> Tortuosity_init ("tortuosity");

#endif TORTUOSITY_H
