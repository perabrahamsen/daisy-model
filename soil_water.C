// soil_water.C

#include "soil_water.h"
#include "log.h"
#include "filter.h"
#include "alist.h"
#include "uzmodel.h"
#include "soil.h"
#include "surface.h"
#include "groundwater.h"
#include "syntax.h"
#include "mathlib.h"
#include "mike_she.h"

#ifdef MIKE_SHE
void
SoilWater::clear (const Geometry& geometry)
{
  mike_she->put_water_sink (geometry, S);

  fill (S.begin (), S.end (), 0.0);
}
#else
void
SoilWater::clear (const Geometry&)
{
  fill (S.begin (), S.end (), 0.0);
}
#endif

void
SoilWater::add_to_sink (const vector<double>& v)
{
  assert (S.size () == v.size ());
  for (unsigned i = 0; i < S.size (); i++)
    S[i] += v[i];
}

void
SoilWater::add_to_sink (const vector<double>& v, const Soil& soil)
{
  assert (S.size () == v.size ());
  for (unsigned i = 0; i < S.size (); i++)
    S[i] += v[i] / soil.z (i);
}


double
SoilWater::pF (int i) const
{
  if (h (i) < 0.0)
    return log10 (-h (i));
  else
    return 0.0;
}

void
SoilWater::tick (Surface& surface, Groundwater& groundwater,
		 const Soil& soil)
{
  Theta_old_ = Theta_;
  h_old = h_;

#ifdef MIKE_SHE
  mike_she->get_water_pressure (h_);
  
  // Update Theta.
  for(unsigned int i = 0; i < Theta_.size (); i++)
    Theta_[i] = soil.Theta (i, h_[i]);

  // Flux is unknown.
  for (unsigned int i = 0; i <= q_.size (); i++)
    q_[i] = -42.42e42;
#else
  // Limit for groundwater table.
  int last  = soil.size () - 1;
  if (!groundwater.flux_bottom ())
    {
      if (groundwater.table () < soil.z (last))
	THROW (runtime_error ("Groundwater table below lowest node."));
      last = soil.interval (groundwater.table ());
      // Presure at the last + 1 node is equal to the water above it.
      for (unsigned int i = last + 1; i < soil.size (); i++)
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
  
  // Update values in groundwater.
  // Bug: Is this also done somewhere else?   Where?
  for (unsigned int i = last + 1; i < soil.size (); i++)
    {
      h_[i] = 0.0;
      Theta_[i] = soil.Theta (i, 0.0);
      q_[i] = q_[i-1];
    }
#endif
}

void 
SoilWater::mix (const Soil& soil, double from, double to)
{
  soil.mix (Theta_, from, to);
  for (unsigned int i = 0; i < soil.size(); i++)
    h_[i] = soil.h (i, Theta_[i]);
}

void
SoilWater::swap (const Soil& soil, double from, double middle, double to)
{
  soil.swap (Theta_, from, middle, to);

  for (unsigned int i = 0; i < soil.size(); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0);
      if (Theta_[i] > Theta_sat)
	{
	  cerr << "\nBUG: Theta[ " << i << "] (" << Theta_[i]
	       << ") > Theta_sat (" << Theta_sat << ")\n";
	  Theta_[i] = Theta_sat;
	}
      h_[i] = soil.h (i, Theta_[i]);
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
SoilWater::output (Log& log, Filter& filter) const
{
  log.output ("S", filter, S, true);
  log.output ("Theta", filter, Theta_);
  log.output ("h", filter, h_);
  log.output ("Xi", filter, Xi);
  log.output ("q", filter, q_, true);
  output_derived (*top, "UZtop", log, filter);
  if (bottom)
    output_derived (*bottom, "UZbottom", log, filter);
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
  : top (UZmodel::create (al.alist ("UZtop"))),
    bottom (  al.check ("UZbottom") 
	    ? UZmodel::create (al.alist ("UZbottom"))
	    : 0),
    bottom_start (  al.check ("UZborder") 
		  ? al.integer ("UZborder")
		  : -1)
{ 
  const unsigned int size = soil.size ();

  if (al.check ("Theta"))
    {
      Theta_ = al.number_sequence ("Theta");
      assert (Theta_.size () > 0);
      while (Theta_.size () < size)
	Theta_.push_back (Theta_[Theta_.size () - 1]);
    }
  if (al.check ("h"))
    {
      h_ = al.number_sequence ("h");
      assert (h_.size () > 0);
      while (h_.size () < size)
	h_.push_back (h_[h_.size () - 1]);
    }
  if (!al.check ("Theta"))
    {
      assert (h_.size () == size);
      for (unsigned int i = 0; i < size; i++)
	Theta_.push_back (soil.Theta (i, h_[i]));
    }

  if (!al.check ("h"))
    {
      assert (Theta_.size () == size);
      for (unsigned int i = 0; i < size; i++)
	h_.push_back (soil.h (i, Theta_[i]));
    }
  if (al.check ("Xi"))
    {
      Xi = al.number_sequence ("Xi");
      if (Xi.size () == 0)
	Xi.push_back (0.0);
      while (Xi.size () < size)
	Xi.push_back (Xi[Xi.size () - 1]);
    }
  else 
    Xi.insert (Xi.begin (), size, 0.0);

  S.insert (S.begin (), size, 0.0);
  q_.insert (q_.begin (), size + 1, 0.0);
}

SoilWater::~SoilWater ()
{
  delete top;
}
