// soil.C
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


#include "soil.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include "submodel.h"
#include "log.h"
#include "message.h"
#include "check.h"
#include <assert.h>
#include <iomanip.h>

struct Soil::Implementation
{
  // Layers.
  struct Layer
  {
    // Content.
    const double end;
    Horizon& horizon;

    // Simulation.
    void output (Log& log) const
    { output_derived (horizon, "horizon", log); }

    // Create and Destroy.
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    { 
      alist.add ("description", "\
A location and content of a soil layer.");
      syntax.add ("end", "cm", Check::negative (), Syntax::Const,
		  "End point of this layer (a negative number).");
      syntax.add ("horizon", Librarian<Horizon>::library (), 
		  "Soil properties of this layer.");
      syntax.order ("end", "horizon");
    }
    Layer (const AttributeList& al)
      : end (al.number ("end")),
	horizon (Librarian<Horizon>::create (al.alist ("horizon")))
    { }
    ~Layer ()
    { delete &horizon; }
  };
  const vector<Layer*> layers;

  vector<Horizon*> make_horizons (const Geometry& geometry)
  {
    vector<Horizon*> result;

    vector<Layer*>::const_iterator layer = layers.begin ();
    const vector<Layer*>::const_iterator end = layers.end ();

    assert (layer != end);
    for (unsigned int i = 0; i < geometry.size (); i++)
      {
	if (geometry.zplus (i) < (*layer)->end)
	  {
	    layer++;
	    assert (layer != end);
	  }
	result.push_back (&((*layer)->horizon));
      }
    return result;
  }
  
  // Parameters
  const double MaxRootingDepth;
  const double dispersivity;

  bool has_attribute (const string& name) const
  { 
    bool missing = false;
    for (unsigned int i = 0; i < layers.size (); i++)
      if (!layers[i]->horizon.has_attribute (name))
	missing = true;
    return !missing;
  }
  
  // Create and Destroy.
  Implementation (const AttributeList& al)
    : layers (map_construct<Layer> (al.alist_sequence ("horizons"))),
      MaxRootingDepth (al.number ("MaxRootingDepth")),
      dispersivity (al.number ("dispersivity"))
  { }
  ~Implementation ()
  { 
    sequence_delete (layers.begin (), layers.end ()); 
  }
};

double 
Soil::K (int i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return horizon_[i]->hydraulic.K (h); 
  else
    return horizon_[i]->hydraulic.K (h_ice); 
}

double Soil::Theta (int i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return horizon_[i]->hydraulic.Theta (h);
  else
    return horizon_[i]->hydraulic.Theta (h_ice);
}

double
Soil::Cw2 (int i, double h) const
{ 
  const double answer = horizon_[i]->hydraulic.Cw2 (h); 
  if (answer > 0.0)
    return answer;
  // We divide with this.
  return 1.0e-8;
}

double 
Soil::dispersivity (int) const
{ return impl.dispersivity; }

bool
Soil::has_attribute (const string& name) const
{ return impl.has_attribute (name); }

void
Soil::output (Log& log) const
{ output_vector (impl.layers, "horizons", log); }

double
Soil::MaxRootingDepth () const
{
  return max (-impl.MaxRootingDepth, z (size () - 1));
}

bool 
Soil::check (Treelog& err) const
{
  bool ok = Geometry::check (err);
  return ok;
}

static bool
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const vector<AttributeList*>& layers = al.alist_sequence ("horizons");

  if (layers.size () < 1U)
    {
      err.entry ("You need at least one horizon");
      ok = false;
    }
  double last = 0.0;

  for (unsigned int i = 0; i < layers.size (); i++)
    {
      double end = layers[i]->number ("end");
      if (end >= last)
	{
	  err.entry ("Horizon endpoints must be monotonically decreasing");
	  ok = false;
	  break;
	}
      last = end;
    }

  if (ok)
    {
      // This check is only meaningful if zplus and layers are ok.
      const vector<double> zplus = al.number_sequence ("zplus");
  
      if (last != zplus[zplus.size() - 1])
	{
	  err.entry ("The last horizon must end the same place as the "
		     "last interval");
	  ok = false;
	}
    }
  return ok;
}  

void 
Soil::make_table (int i)
{
  COUT << "pF   Theta   Cw2           K           (depth " << z (i) << ").\n";
  for (double pF = 0.00; pF <= 5.0; pF += 0.01)
    {
      const double h = pF2h (pF);
      COUT << setw (4) << setprecision (3) << pF << " "
	   << setw (6) << setprecision (5) << Theta (i, h, 0.0) << " "
	   << setw (12) << setprecision (11) << Cw2 (i, h) * 100.0 << " "
	   << setw (12) << setprecision (11) << K (i, h, 0.0) / 3.6e5 << "\n";
    }
}

void
Soil::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Geometry::load_syntax (syntax, alist);
  syntax.add_check (check_alist);
  alist.add ("submodel", "Soil");
  alist.add ("description", "\
The soil component provides the numeric and physical properties of the soil.");
  syntax.add_submodule_sequence ("horizons", Syntax::State, "\
Layered description of the soil properties.",
				 Implementation::Layer::load_syntax);
  syntax.add ("MaxRootingDepth", "cm", Check::positive (), Syntax::Const,
	      "Depth at the end of the root zone (a positive number).");
  //  alist.add ("MaxRootingDepth", 100.0);
  syntax.add ("dispersivity", "cm", Check::positive (), 
	      Syntax::Const, "Dispersion length.");
  alist.add ("dispersivity", 6.0);
}
  
Soil::Soil (const AttributeList& al)
  : Geometry (al),
    impl (*new Implementation (al)),
    horizon_ (impl.make_horizons (*this))
{ };

Soil::~Soil ()
{ delete &impl; }

static Submodel::Register 
soil_submodel ("Soil", Soil::load_syntax);
