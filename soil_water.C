// soil_water.C

#include "soil_water.h"
#include "log.h"
#include "alist.h"
#include "uzmodel.h"
#include "soil.h"
#include "surface.h"
#include "groundwater.h"
#include "syntax.h"
#include "mathlib.h"

void
SoilWater::clear ()
{
  fill (S.begin (), S.end (), 0.0);
}

void
SoilWater::add_to_sink (const vector<double>& v)
{
  assert (S.size () == v.size ());
  for (unsigned i = 0; i < S.size (); i++)
    S[i] += v[i];
}


double
SoilWater::pF (int i) const
{
  if (h (i) < 0.0)
    return log (-100 * h (i));
  else
    return 0.0;
}

void
SoilWater::tick (Surface& surface, Groundwater& groundwater,
		 const Soil& soil)
{
  Theta_old_ = Theta_;
  h_old = h_;

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
	  h_[i] = groundwater.table () - soil.z (i);
	}
    }

  // Limit for ponding.
  const int first = 0;

  if (bottom)
    {
      // We have two UZ models.
      top->tick (soil, 
		 first, surface, 
		 bottom_start - 1, *bottom, 
		 S, h_old, Theta_old_,
		 h_, Theta_, q_);
      bottom->tick (soil,
		    bottom_start, *top,
		    last, groundwater,
		    S, h_old, Theta_old_,
		    h_, Theta_, q_);
    }
  else
    {
      // We have only one UZ model.
      top->tick (soil, 
		 first, surface, 
		 last, groundwater,
		 S, h_old, Theta_old_,
		 h_, Theta_, q_);
    }
}

bool 
SoilWater::check (unsigned n) const
{
  bool ok = true;

  if (Theta_.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << Theta_.size () << " Theta values\n";
      ok = false;
    }
  if (h_.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << h_.size () << " h values\n";
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
SoilWater::output (Log& log, const Filter& filter) const
{
  log.output ("S", filter, S, true);
  log.output ("Theta", filter, Theta_);
  log.output ("h", filter, h_);
  log.output ("Xi", filter, Xi);
  log.output ("q", filter, q_, true);
  top->output ("UZtop", log, filter);
  if (bottom)
    bottom->output ("UZbottom", log, filter);
}

double
SoilWater::MaxExfiltration (const Soil& soil) const
{
  return - ((soil.K (0, h_[0]) / soil.Cw2 (0, h_[0])) 
	    * ((Theta_[0] - soil.Theta_res (0)) / soil.z(0)));
}

void
SoilWater::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("UZtop", UZmodel::library ());
  syntax.add ("UZbottom", UZmodel::library (), Syntax::Optional);
  syntax.add ("UZborder", Syntax::Integer, Syntax::Optional);
  syntax.add ("S", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("Theta", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("h", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("Xi", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("q", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
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
      Theta_ = al.number_sequence ("Theta");
      size = Theta_.size ();
    }
  if (al.check ("h"))
    {
      h_ = al.number_sequence ("h");
      size = h_.size ();
    }
  if (!al.check ("Theta"))
    for (int i = 0; i < size; i++)
      Theta_.push_back (soil.Theta (i, h_[i]));

  if (!al.check ("h"))
    for (int i = 0; i < size; i++)
      h_.push_back (soil.h (i, Theta_[i]));

  if (al.check ("Xi"))
    Xi = al.number_sequence ("Xi");
  else 
    Xi.insert (Xi.begin (), size, 0.0);

  S.insert (S.begin (), size, 0.0);
  q_.insert (q_.begin (), size + 1, 0.0);
}

SoilWater::~SoilWater ()
{
  delete top;
}
