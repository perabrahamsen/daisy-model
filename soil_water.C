// soil_water.C

#include "soil_water.h"
#include "log.h"
#include "alist.h"
#include "uzmodel.h"
#include "soil.h"
#include "surface.h"
#include "groundwater.h"
#include "syntax.h"

void
SoilWater::tick (Surface& surface, const Groundwater& groundwater,
		 const Soil& soil)
{
  Theta_old = Theta;
  h_old = h;

  // Limit for groundwater table.
  int last  = soil.size () - 1;
  if (!groundwater.flux_bottom ())
    {
      if (groundwater.table () < soil.z (last))
	THROW (Runtime ("Groundwater table below lowest node."));
      last = soil.interval (groundwater.table ());
      // Presure at the last + 1 node is equal to the water above it.
      for (int i = last + 1; i < soil.size (); i++)
	{
	  h_old[i] = groundwater.table () - soil.z (i);
	  h[i] = groundwater.table () - soil.z (i);
	}
    }

  // Limit for ponding.
  int first = 0;
  if (!surface.flux_top ())
    {
      first = 1;
      h_old[0] = surface.ponding ();
      h[0] = surface.ponding ();
    }

  if (bottom)
    {
      // We have two UZ models.
      top->tick (soil, 
		 first, surface, 
		 bottom_start - 1, *bottom, 
		 S, h_old, Theta_old,
		 h, Theta, q);
      bottom->tick (soil,
		    bottom_start, *top,
		    last, groundwater,
		    S, h_old, Theta_old,
		    h, Theta, q);
    }
  else
    {
      // We have only one UZ model.
      top->tick (soil, 
		 first, surface, 
		 last, groundwater,
		 S, h_old, Theta_old,
		 h, Theta, q);
    }
}

bool 
SoilWater::check (Log&, unsigned n) const
{
  bool ok = true;

  if (Theta.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << Theta.size () << " Theta values\n";
      ok = false;
    }
  if (h.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << h.size () << " h values\n";
      ok = false;
    }
  if (Xi.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << Xi.size () << " Xi values\n";
      ok = false;
    }
  return ok;
}

void 
SoilWater::output (Log& log, const Filter* filter) const
{
  log.output ("S", filter, S);
  log.output ("Theta", filter, Theta);
  log.output ("h", filter, h);
  log.output ("Xi", filter, Xi);
  log.output ("q", filter, q);
}

void
SoilWater::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_object ("UZtop", UZmodel::library ());
  syntax.add_object ("UZbottom", UZmodel::library (), Syntax::Optional);
  syntax.add ("UZborder", Syntax::Integer, Syntax::Optional);
  syntax.add ("S", Syntax::Array, Syntax::LogOnly);
  syntax.add ("Theta", Syntax::Array, Syntax::Optional);
  syntax.add ("h", Syntax::Array, Syntax::Optional);
  syntax.add ("Xi", Syntax::Array, Syntax::Optional);
  syntax.add ("q", Syntax::Array, Syntax::LogOnly);
}

SoilWater::SoilWater (const Soil& soil, 
		      const AttributeList& al)
  : top (UZmodel::create (al.list ("UZtop"))),
    bottom (  al.check ("UZbottom") 
	    ? UZmodel::create (al.list ("UZbottom"))
	    : 0),
    bottom_start (  al.check ("UZborder") 
		  ? al.integer ("UZborder")
		  : -1)
{ 
  int size = 0;
  
  if (al.check ("Theta"))
    {
      Theta = al.array ("Theta");
      size = Theta.size ();
    }
  if (al.check ("h"))
    {
      h = al.array ("h");
      size = h.size ();
    }
  if (!al.check ("Theta"))
    for (int i = 0; i < size; i++)
      Theta.push_back (soil.Theta (i, h[i]));

  if (!al.check ("h"))
    for (int i = 0; i < size; i++)
      h.push_back (soil.h (i, Theta[i]));

  if (al.check ("Xi"))
    Xi = al.array ("Xi");
  else 
    Xi.insert (Xi.begin (), size, 0.0);

  S.insert (S.begin (), size, 0.0);
  q.insert (q.begin (), size + 1, 0.0);
}

SoilWater::~SoilWater ()
{
  delete top;
}
