// surface.C

#include "surface.h"

bool Surface::flux_top () const
{
  return false;
}

double Surface::q_top () const
{
  return 0.0;
}
  
Surface::Surface (const AttributeList& /* par */, 
		  const AttributeList& /* var */)
{ }
