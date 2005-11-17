// domsorp_std.C -- By default, we transform with a single SOM pool.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#include "domsorp.h"
#include "transform.h"
#include "dom.h"
#include "som.h"
#include "soil.h"
#include "log.h"
#include <memory>

using namespace std;

struct DomsorpStandard : public Domsorp
{
  // Parameters.
  const size_t dom_pool;
  const size_t som_pool;
  const auto_ptr<Transform> transform;
  
  // Output.
  vector<double> S_C;
  vector<double> S_N;
  void output (Log& log) const
  { 
    output_variable (S_C, log); 
    output_variable (S_N, log); 
  }

  // Simulation.
  void tick (const Soil& soil, const SoilWater& soil_water, 
             const vector<DOM*>& dom, const vector<SOM*>& som, Treelog& msg)
  { 
    daisy_assert (dom.size () > dom_pool);
    daisy_assert (som.size () > som_pool);
    DOM& d = *dom[dom_pool];
    SOM& s = *som[som_pool];
    vector<double> dC;
    for (size_t i = 0; i < soil.size (); i++)
      dC.push_back (d.C_at (i));
    daisy_assert (s.C.size () == soil.size ());
    transform->tick (soil, soil_water, dC, s.C, S_C, msg);

    for (size_t i = 0; i < soil.size (); i++)
      {
        if (S_C[i] > 1e-100)    // DOM -> SOM
          {
            const double fraction = S_C[i] / d.C_at (i);
            S_N[i] = fraction * d.N_at (i);
          }
        else if (S_C[i] < -1e-100) // SOM -> DOM
          {
            const double fraction = S_C[i] / s.C[i];
            S_N[i] = fraction * s.N[i];
          }
        else
          S_C[i] = S_N[i] = 0.0;

        d.add_to_source (i, -S_C[i], -S_N[i]);
        s.C[i] += S_C[i];
        s.N[i] += S_N[i];
      }
  }

  // Create.
  bool check (const Soil& soil, const size_t dom_size, const size_t som_size,
              Treelog& msg) const
  { 
    Treelog::Open nest (msg, "Domsorp: " + name);
    bool ok = true;
    {
      Treelog::Open nest (msg, "transform: " + transform->name);
      if (!transform->check (soil, msg))
        ok = false; 
    }
    if (dom_pool >= dom_size)
      { 
        msg.error ("'dom_pool' too high");
        ok = false;
      }
    if (som_pool >= som_size)
      { 
        msg.error ("'som_pool' too high");
        ok = false;
      }
    return ok;
  }
  void initialize (const Soil& soil, Treelog& msg)
  { 
    transform->initialize (soil, msg); 
    S_C.insert (S_C.begin (), soil.size (), 0.0);
    daisy_assert (S_C.size () == soil.size ());
    S_N.insert (S_N.begin (), soil.size (), 0.0);
    daisy_assert (S_N.size () == soil.size ());
  }
  DomsorpStandard (Block& al)
    : Domsorp (al),
      dom_pool (al.integer ("dom_pool") - 1),
      som_pool (al.integer ("som_pool") - 1),
      transform (Librarian<Transform>::build_item (al, "transform"))
  { }
};

static struct DomsorpStandardSyntax
{
  static Domsorp& make (Block& al)
  { return *new DomsorpStandard (al); }
  DomsorpStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Domsorp::load_syntax (syntax, alist);

    alist.add ("description", 
	       "Transformation between two soil chemicals.");
    syntax.add ("transform", Librarian<Transform>::library (),
		"Tranformation process between DOM and SOM.");
    syntax.add ("dom_pool", Syntax::Integer, Syntax::Const,
		"Number of the DOM pool affected by the transformation.");
    syntax.add ("som_pool", Syntax::Integer, Syntax::Const,
		"Number of the SOM pool affected by the transformation.");
    syntax.add ("S_C", "g C/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Carbon converted from DOM to SOM (may be negative).");
    syntax.add ("S_N", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Carbon converted from DOM to SOM (may be negative).");

    Librarian<Domsorp>::add_type ("default", alist, syntax, &make);
  }
} DomsorpStandard_syntax;
