// solute.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "solute.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

double
Solute::total (const Geometry& geo, double from, double to) const
{ return geo.total (M_, from, to); }

void
Solute::clear ()
{
  fill (S.begin (), S.end (), 0.0);
  fill (S_external.begin (), S_external.end (), 0.0);
  fill (S_root.begin (), S_root.end (), 0.0);
  fill (tillage.begin (), tillage.end (), 0.0);
}

void
Solute::add_to_source (const vector<double>& v)
{
  daisy_assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] += v[i];
      daisy_assert (isfinite (S[i]));
      daisy_assert (M_left (i) >= 0.0);
    }
}

void
Solute::add_to_sink (const vector<double>& v)
{
  daisy_assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] -= v[i];
      daisy_assert (isfinite (S[i]));
      daisy_assert (M_left (i) >= 0.0);
    }
}

void
Solute::add_to_root_sink (const vector<double>& v)
{
  daisy_assert (S_root.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_root[i] -= v[i];
  add_to_sink (v);
}

void 
Solute::tick (const size_t cell_size,
	      const SoilWater& soil_water)
{
  for (unsigned i = 0; i < cell_size; i++)
    daisy_assert (M_left (i) >= 0.0);

  // Initialize.
  fill (S_p.begin (), S_p.end (), 0.0);
  fill (J_p.begin (), J_p.end (), 0.0);

  // Permanent source.
  for (size_t i = 0; i < cell_size; i++)
    {
      S_external[i] += S_permanent[i];
      S[i] += S_external[i];
      daisy_assert (M_left (i) >= 0.0);
    }

  // Drainage.
  for (size_t i = 0; i < cell_size; i++)
    {
      S_drain[i] = -soil_water.S_drain (i) * dt * C (i);
      S[i] += S_drain[i];
      daisy_assert (M_left (i) >= 0.0);
    }
}

bool 
Solute::check (unsigned, Treelog&) const
{
  bool ok = true;
  return ok;
}

void
Solute::output (Log& log) const
{
  output_derived (adsorption, "adsorption", log);
  output_value (C_, "C", log);
  output_value (M_, "M", log);
  output_variable (S, log);
  output_variable (S_p, log);
  output_variable (S_drain, log);
  output_variable (S_external, log);
  output_variable (S_permanent, log);
  output_variable (S_root, log);
  output_variable (J, log);
  output_variable (J_p, log);
  output_variable (tillage, log);
}

static bool check_alist (const AttributeList&, Treelog&)
{
  bool ok = true;
  return ok;
}

void 
Solute::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add ("adsorption", Librarian<Adsorption>::library (), 
	      "Soil adsorption properties.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "C", Syntax::Fraction (),
                       "Concentration in water.\n\
This number does not include adsorped matter.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "M", "g/cm^3", 
                       "Total mass per volume water, soil and air.\n\
This number include both adsorped and dissolved matter.");
  Geometry::add_layer (syntax, Syntax::OptionalConst,
                       "Ms", Syntax::Fraction (), "Mass in dry soil.\n\
This include all matter in both soil and water, relative to the\n\
dry matter weight.\n\
Only for initialization of the 'M' parameter.");
  syntax.add ("S", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term.");
  syntax.add ("S_p", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (macropore transport only).");
  syntax.add ("S_drain", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (soil drainage only).");
  syntax.add ("S_external", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "External source, such as incorporated fertilizer.");
  syntax.add ("S_permanent", "g/cm^3/h", Syntax::State, Syntax::Sequence,
	      "Permanent external source, e.g. subsoil irrigation.");
  vector<double> empty;
  alist.add ("S_permanent", empty);
  syntax.add ("S_root", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (root uptake only, always negative).");
  syntax.add ("J", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in matrix (positive up).");
  syntax.add ("J_p", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in macropores (positive up).");
  syntax.add ("tillage", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Changes during tillage.");
}

Solute::Solute (const AttributeList& al)
  : submodel (al.name ("submodel")),
    S_permanent (al.number_sequence ("S_permanent")),
    adsorption (Librarian<Adsorption>::build_cheat (al, "adsorption"))
{ }

Solute::~Solute ()
{ }

void 
Solute::incorporate (const Geometry& geo, 
                     double amount, double from, double to)
{ 
  daisy_assert (amount >= 0.0);
  daisy_assert (from <= 0.0);
  daisy_assert (to <= from);
  geo.add (S_external, from, to, amount);
}

void 
Solute::set_external_source (const Geometry& geo, 
			     double amount, double from, double to)
{
  fill (S_permanent.begin (), S_permanent.end (), 0.0);
  geo.add (S_permanent, from, to, amount);
}

void 
Solute::mix (const Geometry& geo,
             const Soil& soil, const SoilWater& soil_water, 
	     double from, double to)
{ 
  geo.mix (M_, from, to, tillage);
  for (size_t i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::swap (const Geometry& geo,
              const Soil& soil, const SoilWater& soil_water,
	      double from, double middle, double to)
{ 
  geo.swap (M_, from, middle, to, tillage);
  for (size_t i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::put_M (const Soil& soil, const SoilWater& soil_water,
	       const vector<double>& v)
{
  const size_t size = soil.size ();
  daisy_assert (M_.size () == size);
  daisy_assert (C_.size () == size);
  daisy_assert (v.size () == size);

  M_ = v;

  for (size_t i = 0; i < size; i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::default_initialize (const Soil& soil, const SoilWater&)
{ 
  M_.insert (M_.begin (), soil.size (), 0.0);
}

void
Solute::initialize (const AttributeList& al, 
		    const Geometry& geo,
                    const Soil& soil, const SoilWater& soil_water, 
		    Treelog& out)
{
  vector<double> Ms;
  geo.initialize_layer (C_, al, "C", out);
  geo.initialize_layer (M_, al, "M", out);
  geo.initialize_layer (Ms, al, "Ms", out);

  if (C_.size () > 0)
    {
      // Fill it up.
      while (C_.size () < geo.cell_size ())
	C_.push_back (C_[C_.size () - 1]);
      if (C_.size () > geo.cell_size ())
	throw ("To many members of C sequence");
    }
  if (M_.size () > 0)
    {
      // Fill it up.
      while (M_.size () < geo.cell_size ())
	M_.push_back (M_[M_.size () - 1]);
      if (M_.size () > geo.cell_size ())
	throw ("To many members of M sequence");
    }
  if (Ms.size () > 0)
    {
      // Fill it up.
      while (Ms.size () < geo.cell_size ())
	Ms.push_back ( Ms[Ms.size () - 1]);
      if (Ms.size () > geo.cell_size ())
	throw ("To many members of Ms sequence");
    }
  if (M_.size () == 0 && C_.size () == 0)
    {
      if (Ms.size () != 0)
	{
	  daisy_assert (Ms.size () == geo.cell_size ());

	  for (size_t i = M_.size (); i < Ms.size (); i++)
	    M_.push_back (Ms[i] * soil.dry_bulk_density (i));
	}
      if (M_.size () == 0 && C_.size () == 0)
	{
	  default_initialize (soil, soil_water);
	}
    }
  for (size_t i = C_.size (); i < M_.size (); i++)
    C_.push_back (M_to_C (soil, soil_water.Theta (i), i, M_[i]));
  for (size_t i = M_.size (); i < C_.size (); i++)
    M_.push_back (C_to_M (soil, soil_water.Theta (i), i, C_[i]));

  daisy_assert (C_.size () == M_.size ());
  daisy_assert (C_.size () == geo.cell_size ());

  for (size_t i = 0; i < geo.cell_size (); i++)
    {
      if (!std::isnormal (M_[i]))
	{
	  if (std::isnormal (C_[i]))
	    throw ("C & M mismatch in solute");
	}
      else
	{
	  
	  if (!approximate (C_[i],
			    M_to_C (soil, soil_water.Theta (i), i, M_[i])))
	    throw ("Solute C does not match M");
	}
    }

  S.insert (S.begin (), geo.cell_size (), 0.0);
  S_p.insert (S_p.begin (), geo.cell_size (), 0.0);
  S_drain.insert (S_drain.begin (), geo.cell_size (), 0.0);
  S_external.insert (S_external.begin (), geo.cell_size (), 0.0);
  if (S_permanent.size () < geo.cell_size ())
    S_permanent.insert (S_permanent.end (), 
			geo.cell_size () - S_permanent.size (),
			0.0);
  S_root.insert (S_root.begin (), geo.cell_size (), 0.0);
  J.insert (J_p.begin (), geo.edge_size (), 0.0);
  J_p.insert (J_p.begin (), geo.edge_size (), 0.0);
  tillage.insert (tillage.begin (), geo.cell_size (), 0.0);
}
