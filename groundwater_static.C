// groundwater_static.C

#include "groundwater_static.h"
#include "syntax.h"
#include "alist.h"

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
