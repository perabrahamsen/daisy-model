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
  
  if (bottom)
    {
      // We have two UZ models.
      top->tick (soil, 
		 0, surface, 
		 bottom_start - 1, *bottom, 
		 S, h_old, Theta_old,
		 h, Theta, q);
      bottom->tick (soil,
		    bottom_start, *top,
		    soil.size () - 1, groundwater,
		    S, h_old, Theta_old,
		    h, Theta, q);
    }
  else
    {
      // We have only one UZ model.
      top->tick (soil, 
		 0, surface, 
		 soil.size () - 1, groundwater,
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

SoilWater::SoilWater (const Soil& soil, 
		      const AttributeList& /* par */, 
		      const AttributeList& var)
  : top (new UZRichard ()),
    bottom (0),
    bottom_start (  var.check ("UZborder") 
		  ? var.integer ("UZborder")
		  : -1)
{ 
  int size = 0;
  
  if (var.check ("Theta"))
    {
      Theta = var.array ("Theta");
      size = Theta.size ();
    }
  if (var.check ("h"))
    {
      h = var.array ("h");
      size = h.size ();
    }
  if (!var.check ("Theta"))
    for (int i = 0; i < size; i++)
      Theta.push_back (soil.Theta (i, h[i]));

  if (!var.check ("h"))
    for (int i = 0; i < size; i++)
      h.push_back (soil.h (i, Theta[i]));

  if (var.check ("Xi"))
    Xi = var.array ("Xi");
  else 
    Xi.insert (Xi.begin (), size, 0.0);

  S.insert (S.begin (), size, 0.0);
  q.insert (q.begin (), size, 0.0);
}

SoilWater::~SoilWater ()
{
  delete top;
}

const Syntax*
SoilWater::parameter_syntax ()
{
  Syntax* syntax = new Syntax (); 
  syntax->add ("UZtop", Syntax::UZmodel);
  syntax->add ("UZbottom", Syntax::UZmodel, Syntax::Optional);
  syntax->add ("UZborder", Syntax::UZmodel, Syntax::Optional);
  return syntax;
}

const Syntax*
SoilWater::variable_syntax ()
{
  Syntax* syntax = new Syntax ();
  syntax->add ("S", Syntax::Array, Syntax::LogOnly);
  syntax->add ("Theta", Syntax::Array, Syntax::Optional);
  syntax->add ("h", Syntax::Array, Syntax::Optional);
  syntax->add ("Xi", Syntax::Array, Syntax::Optional);
  syntax->add ("q", Syntax::Array, Syntax::LogOnly);
  return syntax;
}
