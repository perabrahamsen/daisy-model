// groundwater_static.C

#include "groundwater.h"
#include "syntax.h"
#include "alist.h"

class GroundwaterStatic : public Groundwater
{
  // Content.
private:
  const double depth;
  
  // UZbottom.
public:
  bool flux_bottom () const;
  bool accept_bottom (double) const;

  // Simulation.
public:
  void tick ();
  double table () const;

  // Create and Destroy.
private:
  friend class GroundwaterStaticSyntax;
  static Groundwater& make (const Time&, const AttributeList&);
  GroundwaterStatic (const Time&, const AttributeList&);
public:
  ~GroundwaterStatic ();
};

bool
GroundwaterStatic::flux_bottom () const
{
  return depth > 0;		// Positive numbers indicate flux bottom.
}

bool 
GroundwaterStatic::accept_bottom (double) const
{
  return true;
}

void
GroundwaterStatic::tick ()
{ }

double
GroundwaterStatic::table () const
{
  assert (!flux_bottom ());
  return depth;
}

GroundwaterStatic::GroundwaterStatic (const Time& t, const AttributeList& al)
  : Groundwater (t),
    depth (al.number ("table"))
{ }

GroundwaterStatic::~GroundwaterStatic ()
{ }

// Add the GroundwaterStatic syntax to the syntax table.
Groundwater&
GroundwaterStatic::make (const Time& t, const AttributeList& al)
{
  return *new GroundwaterStatic (t, al);
}

static struct GroundwaterStaticSyntax
{
  GroundwaterStaticSyntax ();
} GroundwaterStatic_syntax;

GroundwaterStaticSyntax::GroundwaterStaticSyntax ()
{ 
  Syntax* syntax = new Syntax ();
  AttributeList* alist = new AttributeList ();
  syntax->add ("table", Syntax::Number);
  alist->add ("table", 1.0);	// Positive number indicates flux bottom.
  Groundwater::add_type ("static", *alist, *syntax, &GroundwaterStatic::make);
}
