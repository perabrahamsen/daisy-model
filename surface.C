// surface.C

#include "surface.h"

bool 
Surface::flux_top () const
{
  return true;
}

double 
Surface::q_top () const
{
  return 0.0;
}
  
void  
Surface::flux_top_on () const
{ }

void  
Surface::flux_top_off () const
{ }

bool  
Surface::accept_top (double) const
{
  return true;
}

Surface::Surface (const AttributeList& /* par */, 
		  const AttributeList& /* var */)
{ }
