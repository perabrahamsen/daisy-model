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
#include "submodel.h"

void
SoilWater::clear (const Geometry&)
{
  fill (S_.begin (), S_.end (), 0.0);
  fill (S_ice_.begin (), S_ice_.end (), 0.0);
}

void
SoilWater::add_to_sink (const vector<double>& v)
{
  assert (S_.size () == v.size ());
  for (unsigned i = 0; i < S_.size (); i++)
    S_[i] += v[i];
}

void
SoilWater::add_to_sink (const vector<double>& v, const Geometry& geometry)
{
  assert (S_.size () == v.size ());
  for (unsigned i = 0; i < S_.size (); i++)
    S_[i] += v[i] / geometry.z (i);
}


void 
SoilWater::freeze (const Soil&, const vector<double>& v)
{
  assert (v.size () == S_ice_.size ());
  add_to_sink (v);
  for (unsigned int i = 0; i < S_ice_.size (); i++)
    S_ice_[i] -= v[i] * rho_water / rho_ice;
}

double
SoilWater::pF (int i) const
{
  if (h (i) < 0.0)
    return log10 (-h (i));
  else
    return 0.0;
}

unsigned int 
SoilWater::first_groundwater_node () const
{ 
  for (unsigned int i = h_.size (); i > 0u; i--)
    if (h_[i-1] < 0.0)
      return i;
  return 0u;
}

double 
SoilWater::Theta (const Soil& soil, int i, double h) const
{ return soil.Theta (i, h, h_ice_[i]); }

void
SoilWater::tick (Surface& surface, Groundwater& groundwater,
		 const Soil& soil)
{
  // Ice first.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      X_ice_[i] -= S_ice_[i];
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);

      // Move extra ice to buffer.
      const double available_space
	= Theta_sat - Theta_[i] - X_ice_[i] + S_[i] * dt;
      if (available_space < 0.0)
	{
	  X_ice_[i] += available_space;
	  X_ice_buffer[i] -= available_space;
	}
      else if (X_ice_buffer[i] > 0.0)
	{
	  if (X_ice_buffer[i] < available_space)
	    { 
	      X_ice_[i] += X_ice_buffer[i];
	      X_ice_buffer[i] = 0.0;
	    }
	  else
	    {
	      X_ice_[i] += available_space;
	      X_ice_buffer[i] -= available_space;
	    }
	}

      if (X_ice_[i] < 0.0)
	{
	  CERR << "BUG: X_ice[" << i << "] = " << X_ice_[i] << "\n";
	  X_ice_[i] = 0.0;
	}

      // Update ice presure.
      h_ice_[i] = soil.h (i, Theta_sat - X_ice_[i]);
    }

  // Remember old values.
  Theta_old_ = Theta_;
  h_old = h_;

  // Limit for groundwater table.
  int last  = soil.size () - 1;
  if (!groundwater.flux_bottom ())
    {
      if (groundwater.table () < soil.z (last))
	throw ("Groundwater table below lowest node.");
      last = soil.interval (groundwater.table ());
      if (last >=  soil.size () - 1)
	assert ("Groundwater too low.");
      // Presure at the last node is equal to the water above it.
      for (unsigned int i = last + 1; i < soil.size (); i++)
	{
	  h_old[i] = groundwater.table () - soil.z (i);
	  h_[i] = groundwater.table () - soil.z (i);
	}
    }

  // Limit for ponding.
  const int first = 0;

  // Calculate preferential flow first.
  fill (S_p_.begin (), S_p_.end (), 0.0);
  fill (q_p_.begin (), q_p_.end (), 0.0);
  macro.tick (soil, first, last, surface, h_ice_, h_, Theta_, S_, S_p_, q_p_);

  // Calculate matrix flow next.
  try
    {
      if (bottom)
	{
	  // We have two UZ models.
	  top->tick (soil,
		     first, surface,
		     bottom_start - 1, *bottom,
		     S_, h_old, Theta_old_, h_ice_,
		     h_, Theta_, q_);
	  bottom->tick (soil,
			bottom_start, *top,
			last, groundwater,
			S_, h_old, Theta_old_, h_ice_,
			h_, Theta_, q_);
	}
      else
	{
	  // We have only one UZ model.
	  top->tick (soil,
		     first, surface,
		     last, groundwater,
		     S_, h_old, Theta_old_, h_ice_,
		     h_, Theta_, q_);
	}
    }
  catch (const char* error)
    {
      CERR << "UZ problem: " << error << "\n"
           << "Using reserve uz model.\n";
      reserve->tick (soil,
                     first, surface,
                     last, groundwater,
                     S_, h_old, Theta_old_, h_ice_,
                     h_, Theta_, q_);
    }

  for (unsigned int i = last + 1; i <= soil.size (); i++)
    {
      q_[i] = q_[i-1];
      q_p_[i] = q_p_[i-1];
    }

  // Update Theta below groundwater table.
  if (!groundwater.flux_bottom ())
    {
      for(unsigned int i = last + 1; i < soil.size (); i++)
	Theta_[i] = soil.Theta (i, h_[i], h_ice_[i]);
    }

  // Update surface and groundwater reservoirs.
  const bool top_accepted = surface.accept_top (q_[0] * dt);
  assert (top_accepted);
  const bool bottom_accepted 
    = groundwater.accept_bottom ((q_[last + 1] + q_p_[last + 1]) * dt);
  assert (bottom_accepted);

  // Update flux in groundwater.
  groundwater.update_water (soil,
			    S_, h_, h_ice_, Theta_, q_, q_p_);
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
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
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
  if (X_ice_.size () != n)
    {
      CERR << "You have " << n 
	   << " intervals but " << X_ice_.size () << " X_ice values\n";
      ok = false;
    }
  if (X_ice_buffer.size () != n)
    {
      CERR << "You have " << n 
	   << " intervals but " << X_ice_buffer.size () << " X_ice_buffer values\n";
      ok = false;
    }
  return ok;
}

void 
SoilWater::output (Log& log) const
{
  log.output ("S", S_);
  log.output ("S_p", S_p_);
  log.output ("Theta", Theta_);
  log.output ("h", h_);
  log.output ("S_ice", S_ice_);
  log.output ("X_ice", X_ice_);
  log.output ("X_ice_buffer", X_ice_buffer);
  log.output ("h_ice", h_ice_);
  log.output ("q", q_);
  log.output ("q_p", q_p_);
  output_derived (*top, "UZtop", log);
  if (bottom)
    output_derived (*bottom, "UZbottom", log);
}

double
SoilWater::MaxExfiltration (const Soil& soil) const
{
  return - ((soil.K (0, h_[0], h_ice_[0]) / soil.Cw2 (0, h_[0])) 
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
    Theta_[i] = soil.Theta (i, h_[i], h_ice_[i]);
}

void
SoilWater::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilWater");
  alist.add ("description", "Water content of soil.");

  syntax.add ("UZtop", Librarian<UZmodel>::library (),
	      "Main water transport model in unsaturated zone.");
  AttributeList richard;
  richard.add ("type", "richards");
  richard.add ("max_time_step_reductions", 4);
  richard.add ("time_step_reduction", 4);
  richard.add ("max_iterations", 25);
  richard.add ("max_absolute_difference", 0.02);
  richard.add ("max_relative_difference", 0.001);
  alist.add ("UZtop", richard);

  syntax.add ("UZbottom", Librarian<UZmodel>::library (),
	      Syntax::OptionalState, Syntax::Singleton, "\
Water transport model for the bottom of the unsaturated zone.\n\
If this is given, `UZtop' will be used down to `UZborder', and `UZbottom'\n\
will be used from there to the bottom.");
  syntax.add ("UZborder", Syntax::Integer, Syntax::OptionalConst,
	      "Top node to use `UZbottom' in.");
  syntax.add ("UZreserve", Librarian<UZmodel>::library (),
	      "Reserve transport model if UZtop fails.");
  // Use lr as UZreserve by default.
  AttributeList lr;
  lr.add ("type", "lr");
  lr.add ("h_fc", -100.0);
  lr.add ("z_top", -10.0);
  alist.add ("UZreserve", lr);
  syntax.add ("S", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink (due to root uptake and macropores).");
  syntax.add ("S_p", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink (due to macropores).");
  Geometry::add_layer (syntax, "Theta", "cm^3/cm^3", "Soil water content.");
  Geometry::add_layer (syntax, "h", "cm", "Soil water pressure.");
  syntax.add ("S_ice", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Ice sink (due to thawing or freezing).");
  syntax.add ("X_ice", Syntax::None (), 
	      Syntax::OptionalState, Syntax::Sequence,
	      "Ice volume fraction in soil.");
  syntax.add ("X_ice_buffer", Syntax::None (), 
	      Syntax::OptionalState, Syntax::Sequence,
	      "Ice volume that didn't fit the soil durin freezing.");
  syntax.add ("h_ice", Syntax::None (), Syntax::LogOnly, Syntax::Sequence,
	      "Pressure at which all air is out of the matrix.\n\
When there are no ice, this is 0.0.  When there are ice, the ice is\n\
presummed to occupy the large pores, so it is h (Theta_sat - X_ice).");
  syntax.add ("q", "cm/h", Syntax::LogOnly, Syntax::Sequence,
	      "Matrix water flux (positive numbers mean upward).");
  syntax.add ("q_p", "cm/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water flux in macro pores (positive numbers mean upward).");

  syntax.add ("macro", Librarian<Macro>::library (),
	      "Preferential flow model.");
  AttributeList macro;
  macro.add ("type", "none");
  alist.add ("macro", macro);
}

SoilWater::SoilWater (const AttributeList& al)
  : top (&Librarian<UZmodel>::create (al.alist ("UZtop"))),
    bottom (  al.check ("UZbottom") 
	    ? &Librarian<UZmodel>::create (al.alist ("UZbottom"))
	    : 0),
    bottom_start (  al.check ("UZborder") 
		  ? al.integer ("UZborder")
		  : -1),
    reserve (&Librarian<UZmodel>::create (al.alist ("UZreserve"))),
    macro (Librarian<Macro>::create (al.alist ("macro")))
{ }

void
SoilWater::initialize (const AttributeList& al,
		       const Soil& soil, const Groundwater& groundwater)
{
  const unsigned int size = soil.size ();

  if (al.check ("X_ice"))
    {
      X_ice_ = al.number_sequence ("X_ice");
      if (X_ice_.size () == 0)
	X_ice_.push_back (0.0);
      while (X_ice_.size () < size)
	X_ice_.push_back (X_ice_[X_ice_.size () - 1]);
    }
  else 
    X_ice_.insert (X_ice_.begin (), size, 0.0);

  if (al.check ("X_ice_buffer"))
    {
      X_ice_buffer = al.number_sequence ("X_ice_buffer");
      if (X_ice_buffer.size () == 0)
	X_ice_buffer.push_back (0.0);
      while (X_ice_buffer.size () < size)
	X_ice_buffer.push_back (X_ice_buffer[X_ice_buffer.size () - 1]);
    }
  else 
    X_ice_buffer.insert (X_ice_buffer.begin (), size, 0.0);

  h_ice_.insert (h_ice_.begin (), size, 0.0);
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      assert (Theta_sat >= X_ice_[i]);
      h_ice_[i] = soil.h (i, Theta_sat - X_ice_[i]);
    }

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
	  Theta_.push_back (soil.Theta (i, h_[i], h_ice_[i]));
    }

  S_.insert (S_.begin (), size, 0.0);
  S_p_.insert (S_p_.begin (), size, 0.0);
  q_.insert (q_.begin (), size + 1, 0.0);
  q_p_.insert (q_p_.begin (), size + 1, 0.0);
  S_ice_.insert (S_ice_.begin (), size, 0.0);

  assert (h_.size () == Theta_.size ());
  if (h_.size () == 0)
    {
      if (groundwater.flux_bottom ())
	{
	  const double h = -100.0; // pF 2.0;
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h_.push_back (h);
	      Theta_.push_back (soil.Theta (i, h, h_ice_[i]));
	    }
	}
      else
	{
	  const double table = groundwater.table ();
	  
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h_.push_back (table - soil.z (i));
	      Theta_.push_back (soil.Theta (i, h_[i], h_ice_[i]));
	    }
	}
    }
  assert (h_.size () == soil.size ());

  // We just assume no changes.
  Theta_old_ = Theta_;
  h_old = h_;

  // Let `macro' choose the default method to average K values in `uz'.
  const bool has_macropores = (al.alist ("macro").name ("type") != "none");
  top->has_macropores (has_macropores);
  if (bottom)
    bottom->has_macropores (has_macropores);
}

SoilWater::~SoilWater ()
{
  delete top;
  delete &macro;
}

static Submodel::Register 
soil_water_submodel ("SoilWater", SoilWater::load_syntax);
