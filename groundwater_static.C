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
  assert (!flux_bottom ());
  return depth;
}

GroundwaterStatic::GroundwaterStatic (const AttributeList& al)
  : Groundwater (al.name ("type")),
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
  GroundwaterStaticSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("table", Syntax::Number, Syntax::Const);
      alist.add ("table", 1.0); // Positive number indicates flux bottom.
      Librarian<Groundwater>::add_type ("static", alist, syntax, &make);
    }
} GroundwaterStatic_syntax;


