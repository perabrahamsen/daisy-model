// condition.h -- Logic expressions

#ifndef CONDITION_H
#define CONDITION_H

#include "librarian.h"

class Daisy;

class Condition
{  
  // Content.
public:
  const string name;
  static const char *const description;
  virtual const string timestep ();

  // Simulation.
public:
  virtual bool match (const Daisy&) const = 0;
  virtual void output (Log&) const = 0;

  // Create & Destroy.
protected:
  Condition (const AttributeList& al);
public:
  virtual ~Condition ();
};

static Librarian<Condition> Condition_init ("condition");

#endif // CONDITION_H
