// surface.C

#include "surface.h"

bool 
Surface::flux_top () const
{
  return true;
}

double 
Surface::q () const
{
  assert (flux_top ());
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

double
Surface::ponding () const
{
  assert (!flux_top ());
  return 0.0;
}

double
Surface::evaporation (double /* PotSoilEvaporation */, double /* Water */)
{
  return 0.0;
}

void
Surface::load_syntax (Syntax&, AttributeList&)
{ }

Surface::Surface (const AttributeList&)
{ }
