// condition_daisy.C
//
// Checking daisy state.

#include "condition.h"
#include "daisy.h"

struct ConditionRunning : public Condition
{
  bool match (const Daisy& daisy) const
    { return daisy.running; }
  ConditionRunning (const AttributeList&)
    { }
};

struct ConditionFinished : public Condition
{
  bool match (const Daisy& daisy) const
    { return !daisy.running; }
  ConditionFinished (const AttributeList&)
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
      AttributeList& alist = *new AttributeList ();
      Librarian<Condition>::add_type ("running",
				      alist, syntax, &make_running);
      Librarian<Condition>::add_type ("finished",
				      alist, syntax, &make_finished);
    }
} ConditionDaisy_syntax;
