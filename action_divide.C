// action_divide.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionDivide : public Action
{
  const string original;
  const string copy;
  void doIt (Daisy& daisy)
    {
      COUT << "[Dividing " << original << " into " << copy << "]\n";
      daisy.field.divide (original, copy, daisy.time, daisy.weather);
    }

  ActionDivide (const AttributeList& al)
    : Action (al.name ("type")),
      original (al.name ("original")), 
      copy (al.name ("copy"))
    { }
};

static struct ActionDivideSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionDivide (al); }
  ActionDivideSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("original", Syntax::String, Syntax::Const,
		  "Column to divide");
      syntax.add ("copy", Syntax::String, Syntax::Const,
		  "Name of new divide.");
      syntax.order ("original", "copy");
      Librarian<Action>::add_type ("divide", alist, syntax, &make);
    }
} ActionDivide_syntax;
