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
  syntax.add ("transport", Librarian<Transport>::library (), Syntax::State);
  AttributeList& cd = *new AttributeList ();
  cd.add ("type", "cd");
  alist.add ("transport", cd);
  syntax.add ("adsorption", Librarian<Adsorption>::library (), Syntax::Const);
  Geometry::add_layer (syntax, "C");
  Geometry::add_layer (syntax, "M");
  syntax.add ("S", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  Geometry::add_layer (syntax, "ppm");
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
  vector<double> ppm;
  soil.initialize_layer (C_, al, "C");
  soil.initialize_layer (M_, al, "M");
  soil.initialize_layer (ppm, al, "ppm");

  if (C_.size () > 0)
    {
      // Fill it up.
      if (C_.size () < soil.size ())
	C_.insert (C_.end (), soil.size () - C_.size (), C_[C_.size () - 1]);
      else if (C_.size () > soil.size ())
	throw ("To many members of C sequence");
    }
  if (M_.size () > 0)
    {
      // Fill it up.
      if (M_.size () < soil.size () + 0U)
	M_.insert (M_.end (), soil.size () - M_.size (), M_[M_.size () - 1]);
      else if (M_.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (ppm.size () > 0)
    {
      // Fill it up.
      if (ppm.size () < soil.size () + 0U)
	ppm.insert (ppm.end (), soil.size () - ppm.size (), 
		    ppm[ppm.size () - 1]);
      else if (ppm.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (M_.size () == 0 && C_.size () == 0)
    {
      if (ppm.size () != 0)
	{
	  assert (ppm.size () == soil.size ());

	  for (unsigned int i = M_.size (); i < ppm.size (); i++)
	    // ppm -> g / cm^3.
	    M_.push_back (1.0e-6 * ppm[i] * soil.dry_bulk_density (i));
	}
      else
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
