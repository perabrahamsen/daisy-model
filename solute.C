// solute.C

#include "solute.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "soil_water.h"
#include "mathlib.h"

void
Solute::clear ()
{
  fill (S.begin (), S.end (), 0.0);
}

void
Solute::add_to_source (const vector<double>& v)
{
  assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] += v[i];
      assert (finite (S[i]));
    }
}

void
Solute::add_to_sink (const vector<double>& v)
{
  assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] -= v[i];
      assert (finite (S[i]));
    }
}

void 
Solute::tick (const Soil& soil, 
	      const SoilWater& soil_water, 
	      double J_in)
{
  transport.tick (soil, soil_water, *this, M_, C_, S, J_in);
}

bool 
Solute::check (unsigned) const
{
  return true;
}

void
Solute::output (Log& log, Filter& filter) const
{
  output_derived (transport, "transport", log, filter);
  output_derived (adsorption, "adsorption", log, filter);
  log.output ("C", filter, C_);
  log.output ("M", filter, M_);
  log.output ("S", filter, S, true);
}

void 
Solute::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("transport", Librarian<Transport>::library (), 
	      "Solute transport model.");
  AttributeList& cd = *new AttributeList ();
  cd.add ("type", "cd");
  alist.add ("transport", cd);
  syntax.add ("adsorption", Librarian<Adsorption>::library (), 
	      "Soil adsorption properties.");
  Geometry::add_layer (syntax, "C", "g/cm^3", "Concentration in water.");
  Geometry::add_layer (syntax, "M", "g/cm^3", "Mass in water and soil.");
  syntax.add ("S", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term.");
  Geometry::add_layer (syntax, "soil_ppm", "ppm", "Concentration in water.\n\
Only for initialization of the `M' parameter.");
  Geometry::add_layer (syntax, "solute_ppm", "ppm", "Mass in water and soil.\n\
Only used for initialization of the `C' parameter.");
}

Solute::Solute (const AttributeList& al)
  : transport (Librarian<Transport>::create (al.alist ("transport"))),
    adsorption (Librarian<Adsorption>::create (al.alist ("adsorption")))
{ }

Solute::~Solute ()
{ 
  delete &transport; 
  delete &adsorption; 
}

void 
Solute::add (const Soil& soil, const SoilWater& soil_water, 
	     double amount, double from, double to)
{ 
  soil.add (M_, from, to, amount);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::mix (const Soil& soil, const SoilWater& soil_water, 
	     double from, double to)
{ 
  soil.mix (M_, from, to);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::swap (const Soil& soil, const SoilWater& soil_water,
	      double from, double middle, double to)
{ 
  soil.swap (M_, from, middle, to);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::put_M (const Soil& soil, const SoilWater& soil_water,
	       const vector<double>& v)
{
  const unsigned int size = soil.size ();
  assert (M_.size () == size);
  assert (C_.size () == size);
  assert (v.size () == size);

  M_ = v;

  for (unsigned int i = 0; i < size; i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void
Solute::initialize (const AttributeList& al, 
		    const Soil& soil, const SoilWater& soil_water)
{
  vector<double> soil_ppm;
  vector<double> solute_ppm;
  soil.initialize_layer (C_, al, "C");
  soil.initialize_layer (M_, al, "M");
  soil.initialize_layer (soil_ppm, al, "soil_ppm");
  soil.initialize_layer (solute_ppm, al, "solute_ppm");

  if (C_.size () > 0)
    {
      // Fill it up.
      while (C_.size () < soil.size ())
	C_.push_back (C_[C_.size () - 1]);
      if (C_.size () > soil.size ())
	throw ("To many members of C sequence");
    }
  if (M_.size () > 0)
    {
      // Fill it up.
      while (M_.size () < soil.size ())
	M_.push_back (M_[M_.size () - 1]);
      if (M_.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (soil_ppm.size () > 0)
    {
      // Fill it up.
      while (soil_ppm.size () < soil.size ())
	soil_ppm.push_back ( soil_ppm[soil_ppm.size () - 1]);
      if (soil_ppm.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (solute_ppm.size () > 0)
    {
      // Fill it up.
      while (solute_ppm.size () < soil.size ())
	solute_ppm.push_back ( solute_ppm[solute_ppm.size () - 1]);
      if (solute_ppm.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (M_.size () == 0 && C_.size () == 0)
    {
      if (soil_ppm.size () != 0)
	{
	  assert (soil_ppm.size () == soil.size ());

	  for (unsigned int i = M_.size (); i < soil_ppm.size (); i++)
	    // ppm -> g / cm^3.
	    M_.push_back (1.0e-6 * soil_ppm[i] * soil.dry_bulk_density (i));
	}
      if (solute_ppm.size () != 0)
	{
	  assert (solute_ppm.size () == solute_ppm.size ());

	  for (unsigned int i = M_.size (); i < solute_ppm.size (); i++)
	    // ppm -> g / cm^3.
	    C_.push_back (1.0e-6 * solute_ppm[i]);
	}
      if (M_.size () == 0 && C_.size () == 0)
	{
	  C_.insert (C_.begin (), soil.size (), 0.0);
	  M_.insert (M_.begin (), soil.size (), 0.0);
	}
    }
  for (unsigned int i = C_.size (); i < M_.size (); i++)
    C_.push_back (M_to_C (soil, soil_water.Theta (i), i, M_[i]));
  for (unsigned int i = M_.size (); i < C_.size (); i++)
    M_.push_back (C_to_M (soil, soil_water.Theta (i), i, C_[i]));

  assert (C_.size () == M_.size ());
  assert (C_.size () == soil.size ());

  for (unsigned int i = 0; i < soil.size (); i++)
    {
      if (C_[i] == 0.0)
	{
	  if (M_[i] != 0.0)
	    throw ("C & M mismatch in solute");
	}
      else
	{
	  if (fabs (M_[i] / C_to_M (soil, soil_water.Theta (i), i, C_[i]) 
		   - 1.0) >= 0.001)
	    throw ("Solute C does not match M");
	}
    }

  S.insert (S.begin (), soil.size (), 0.0);
}
