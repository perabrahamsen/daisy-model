// condition_daisy.C
//
// Checking daisy state.

#include "condition.h"
#include "daisy.h"

struct ConditionRunning : public Condition
{
  bool match (const Daisy& daisy) const
    { return daisy.running; }
  void output (Log&, Filter&) const
    { }
  ConditionRunning (const AttributeList& al)
    : Condition (al)
    { }
};

struct ConditionFinished : public Condition
{
  bool match (const Daisy& daisy) const
    { return !daisy.running; }
  void output (Log&, Filter&) const
    { }
  ConditionFinished (const AttributeList& al)
    : Condition (al)
    { }
};

static struct ConditionDaisySyntax
{
  static Condition& make_running (const AttributeList& al)
    { return *new ConditionRunning (al); }

  static Condition& make_finished (const AttributeList& al)
    { return *new ConditionFinished (al); }

  ConditionDaisySyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist_running = *new AttributeList ();
      alist_running.add ("description", 
			 "True iff the simulation is still running.");
      AttributeList& alist_finished = *new AttributeList ();
      alist_finished.add ("description", 
			  "True iff the simulation has finished.");
      Librarian<Condition>::add_type ("running",
				      alist_running, syntax, &make_running);
      Librarian<Condition>::add_type ("finished",
				      alist_finished, syntax, &make_finished);
    }
} ConditionDaisy_syntax;
