// average.h --- Find the average of two numbers.

#ifndef AVERAGE_H
#define AVERAGE_H

#include "librarian.h"

class Average
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual double operator()(double a, double b) const = 0;

  // Create and Destroy.
protected:
  Average (const AttributeList& al);
public:
  virtual ~Average ();
};

static Librarian<Average> Average_init ("average");

#endif // AVERAGE_H
