// action_surface.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionSetSurfaceDetentionCapacity : public Action
{
  // Content.
  const double height;

  // Simulation.
  void doIt (Daisy& daisy)
  {
    COUT << " [Surface]\n";
    daisy.field.set_surface_detention_capacity (height);
  }

  ActionSetSurfaceDetentionCapacity (const AttributeList& al)
    : Action (al),
      height (al.number ("height"))
  { }
};

static struct ActionSurfaceSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSetSurfaceDetentionCapacity (al); }
  
  static bool check (const AttributeList& al)
  {
    const double height  = al.number ("height");
    bool ok = true;
    non_negative (height, "height", ok);
    if (!ok)
      CERR << "in set_surface_detention_capacity action\n";
    return ok;
  }
  ActionSurfaceSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Set amount of ponding the surface can retain.");
    syntax.add ("height", "cm", Syntax::Const,
		"Max ponding height before runoff.");
    syntax.order ("height");
    Librarian<Action>::add_type ("set_surface_detention_capacity", 
				 alist, syntax, &make);
  }
} ActionSurface_syntax;

