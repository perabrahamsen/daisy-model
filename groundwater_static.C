// groundwater_static.C

#include "groundwater.h"

class GroundwaterStatic : public Groundwater
{
  // Content.
private:
  const double depth;
  
  // UZbottom.
public:
  bool flux_bottom () const;
  bool accept_bottom (double);

  // Simulation.
public:
  void tick (const Time&);
  double table () const;

  // Create and Destroy.
public:
  GroundwaterStatic (const AttributeList&);
  ~GroundwaterStatic ();
};

bool
GroundwaterStatic::flux_bottom () const
{
  return depth > 0;		// Positive numbers indicate flux bottom.
}

bool 
GroundwaterStatic::accept_bottom (double)
{
  return true;
}

void
GroundwaterStatic::tick (const Time&)
{ }

double
GroundwaterStatic::table () const
{
  return depth;
}

GroundwaterStatic::GroundwaterStatic (const AttributeList& al)
  : Groundwater (al),
    depth (al.number ("table"))
{ }

GroundwaterStatic::~GroundwaterStatic ()
{ }

static struct GroundwaterStaticSyntax
{
  static Groundwater& make (const AttributeList& al)
  { 
    return *new GroundwaterStatic (al);
  }
  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    static bool warned = false;
    if (warned)
      return true;
    else if (al.number ("table") > 0)
      err.entry ("OBSOLETE: Use `deep' instead `table' groundwater");
    else
      err.entry ("OBSOLETE: Use `fixed' instead `table' groundwater");
    warned = true;
    return true;
  }
  GroundwaterStaticSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);
    alist.add ("description", "Static groundwater level.\n\
Provided for backward compatibility, use `deep' or `fixed' instead.");
    Groundwater::load_syntax (syntax, alist);
    syntax.add ("table", "cm", Syntax::Const,
		"Groundwater level.\n\
Positive numbers indicate free drainage.");
    alist.add ("table", 1.0);
    Librarian<Groundwater>::add_type ("static", alist, syntax, &make);
  }
} GroundwaterStatic_syntax;


