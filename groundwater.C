// groundwater.C

#include "groundwater.h"
#include "syntax.h"

bool 
Groundwater::flux_bottom () const
{
  return true;
}

double 
Groundwater::table () const
{
  assert (!flux_bottom ());
  return -10.0;
}

bool  
Groundwater::accept_bottom (double) const
{ 
  return true;
}
Groundwater::Groundwater (const AttributeList& /* al */)
{ }

// Add the Groundwater syntax to the syntax table.
static struct GroundwaterSyntax
{
  GroundwaterSyntax ();
} Groundwater_syntax;

GroundwaterSyntax::GroundwaterSyntax ()
{ 
  Syntax* syntax = new Syntax ();
  syntax_table->add ("groundwater", syntax);
}
