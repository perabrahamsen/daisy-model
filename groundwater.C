// groundwater.C

#include "groundwater.h"
#include "syntax.h"

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
