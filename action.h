// action.h -- Manager actions

#ifndef ACTION_H
#define ACTION_H

#include "librarian.h"

class Daisy;

class Action
{
  // Content.
  const string name;

  // Simulation.
public:
  virtual void doIt (Daisy&) = 0;

  // Create and Destroy.
public: 
  virtual bool check (Daisy&) const;
protected:
  Action (const string& name);
public:
  virtual ~Action ();
};

static Librarian<Action> Action_init ("action");

#endif ACTION_H
