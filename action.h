// action.h -- Manager actions

#ifndef ACTION_H
#define ACTION_H

#include "librarian.h"

class Daisy;

class Action
{
  // Content.
public:
  const string name;
  const AttributeList alist;

  // Simulation.
public:
  virtual void doIt (Daisy&) = 0;
  virtual bool done (const Daisy&) const;
  virtual void output (Log&) const;

  // Create and Destroy.
public: 
  virtual bool check (const Daisy&) const;
  static const char *const description;
protected:
  Action (const AttributeList& al);
public:
  virtual ~Action ();
};

static Librarian<Action> Action_init ("action");

#endif ACTION_H
