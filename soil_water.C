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

void
SoilWater::clear (const Geometry&)
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

  // Limit for groundwater table.
  int last  = soil.size () - 1;
  if (!groundwater.flux_bottom ())
    {
      if (groundwater.table () < soil.z (last))
	THROW ("Groundwater table below lowest node.");
      last = soil.interval (groundwater.table ());
      // Presure at the last node is equal to the water above it.
      for (unsigned int i = last; i < soil.size (); i++)
	{
	  h_old[i] = groundwater.table () - soil.z (i);
	  h_[i] = groundwater.table () - soil.z (i);
	}
    }

  // Limit for ponding.
  const int first = 0;

#ifdef HANDLE_EXCEPTIONS

  try
    {
#endif
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
#ifdef HANDLE_EXCEPTIONS
    }
  catch (const char* error)
    {
      CERR << "UZ problem: " << error << "\n"
           << "Using reserve uz model.\n";
      reserve->tick (soil,
                     first, surface,
                     last, groundwater,
                     S, h_old, Theta_old_,
                     h_, Theta_, q_);
    }
#endif

  // Update flux in groundwater.
  for (unsigned int i = last + 1; i < soil.size (); i++)
    q_[i] = q_[i-1];
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
	  CERR << "\nBUG: Theta[ " << i << "] (" << Theta_[i]
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
      CERR << "You have " << n 
	   << " intervals but " << Theta_.size () << " Theta values\n";
      ok = false;
    }
  if (h_.size () != n)
    {
      CERR << "You have " << n 
	   << " intervals but " << h_.size () << " h values\n";
      ok = false;
    }
  if (Xi.size () != n)
    {
      CERR << "You have " << n 
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
SoilWater::put_h (const Soil& soil, const vector<double>& v) // [cm]
{
  const int size = soil.size ();
  assert (v.size () == size);
  assert (h_.size () == size);
  assert (Theta_.size () == size);

  h_ = v;

  for (unsigned int i = 0; i < size; i++)
    Theta_[i] = soil.Theta (i, h_[i]);
}

void
SoilWater::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("UZtop", Librarian<UZmodel>::library ());
  AttributeList richard;
  richard.add ("type", "richard");
  richard.add ("max_time_step_reductions", 4);
  richard.add ("time_step_reduction", 4);
  richard.add ("max_iterations", 25);
  richard.add ("max_absolute_difference", 0.02);
  richard.add ("max_relative_difference", 0.001);
  alist.add ("UZtop", richard);

  syntax.add ("UZbottom", Librarian<UZmodel>::library (),
	      Syntax::OptionalState);
  syntax.add ("UZborder", Syntax::Integer, Syntax::OptionalConst);
  syntax.add ("UZreserve", Librarian<UZmodel>::library ());
  // Use lr as UZreserve by default.
  AttributeList lr;
  lr.add ("type", "lr");
  lr.add ("h_fc", -100.0);
  lr.add ("z_top", -10.0);
  alist.add ("UZreserve", lr);
  syntax.add ("S", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  Geometry::add_layer (syntax, "Theta");
  Geometry::add_layer (syntax, "h");
  syntax.add ("Xi", Syntax::Number, Syntax::OptionalState, Syntax::Sequence);
  syntax.add ("q", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
}

SoilWater::SoilWater (const AttributeList& al)
  : top (&Librarian<UZmodel>::create (al.alist ("UZtop"))),
    bottom (  al.check ("UZbottom") 
	    ? &Librarian<UZmodel>::create (al.alist ("UZbottom"))
	    : 0),
    bottom_start (  al.check ("UZborder") 
		  ? al.integer ("UZborder")
		  : -1),
    reserve (&Librarian<UZmodel>::create (al.alist ("UZreserve")))
{ }

void
SoilWater::initialize (const AttributeList& al,
		       const Soil& soil, const Groundwater& groundwater)
{
  const unsigned int size = soil.size ();

  soil.initialize_layer (Theta_, al, "Theta");
  soil.initialize_layer (h_, al, "h");

  if (Theta_.size () > 0)
    {
      while (Theta_.size () < size)
	Theta_.push_back (Theta_[Theta_.size () - 1]);
      if (h_.size () == 0)
	for (unsigned int i = 0; i < size; i++)
	  h_.push_back (soil.h (i, Theta_[i]));
    }
  if (h_.size () > 0)
    {
      while (h_.size () < size)
	h_.push_back (h_[h_.size () - 1]);
      if (Theta_.size () == 0)
	for (unsigned int i = 0; i < size; i++)
	  Theta_.push_back (soil.Theta (i, h_[i]));
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

  assert (h_.size () == Theta_.size ());
  if (h_.size () == 0)
    {
      if (groundwater.flux_bottom ())
	{
	  const double h = -100.0; // pF 2.0;
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h_.push_back (h);
	      Theta_.push_back (soil.Theta (i, h));
	    }
	}
      else
	{
	  const double table = groundwater.table ();
	  
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h_.push_back (table - soil.z (i));
	      Theta_.push_back (soil.Theta (i, h_[i]));
	    }
	}
    }
  assert (h_.size () == soil.size ());

  // We just assume no changes.
  Theta_old_ = Theta_;
  h_old = h_;
}

SoilWater::~SoilWater ()
{
  delete top;
}
