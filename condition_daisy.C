// condition_daisy.C
//
// Checking daisy state.

#include "condition.h"
#include "daisy.h"

struct ConditionRunning : public Condition
{
  bool match (const Frame&, const Daisy& daisy) const
    { return daisy.running; }
  ConditionRunning (const AttributeList&)
    { }
};

static struct ConditionDaisySyntax
{
  static Condition& make_running (const AttributeList& al)
    { return *new ConditionRunning (al); }

  ConditionDaisySyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Librarian<Condition>::add_type ("running",
				      alist, syntax, &make_running);
    }
} ConditionDaisy_syntax;
