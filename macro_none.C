// macro_none.C -- No preferential flow.

#include "macro.h"

struct MacroNone : public Macro
{
  // Simulation.
  void tick (const Soil& /* soil */, 
	     unsigned int /* first */, unsigned int /* last */,
	     UZtop& /* surface */,
	     const vector<double>& /* h */,
	     const vector<double>& /* Theta */,
	     vector<double>& /* S */,
	     vector<double>& /* S_p */,
	     vector<double>& /* q_p */)
    { }
  void output (Log&) const
    { }

  // Create and Destroy.
  MacroNone (const AttributeList& al)
    : Macro (al)
    { }
  ~MacroNone ()
    { }
};

static struct MacroNoneSyntax
{
  static Macro&
  make (const AttributeList& al)
    { return *new MacroNone (al); }
  MacroNoneSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "No macropores.");
      Librarian<Macro>::add_type ("none", alist, syntax, &make);
    }
} MacroNone_syntax;
