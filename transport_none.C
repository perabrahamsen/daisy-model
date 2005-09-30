// transport_none.C
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


#include "transport.h"
#include "soil.h"
#include "soil_water.h"
#include "adsorption.h"
#include "log.h"
#include "mathlib.h"
#include "timestep.h"
#include <sstream>

using namespace std;

class TransportNone : public Transport
{
  // Simulation.
public:
  void tick (Treelog&, const Soil&, const SoilWater&, const Adsorption&,
	     double diffusion_coefficient,
	     vector<double>& M, 
	     vector<double>& C,
	     const vector<double>& S,
	     vector<double>& J);
  void output (Log&) const
    { }

  // Create.
public:
  TransportNone (const AttributeList& al)
    : Transport (al)
    { }
};

void 
TransportNone::tick (Treelog& msg,
		     const Soil& soil, const SoilWater& soil_water,
		     const Adsorption& adsorption, 
		     const double,
		     vector<double>& M, 
		     vector<double>& C,
		     const vector<double>& S,
		     vector<double>& J)
{
  Treelog::Open* nest = NULL;
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      M[i] += S[i] *dt;

      if (i == 0)
	M[i] -= J[0] / soil.dz (0);
      else
	J[i] = 0.0;

      C[i] = adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]);
      if (!(M[i] >= 0.0))
	{
	  if (!nest)
	    nest = new Treelog::Open (msg, "Transport none");
	  std::ostringstream tmp;
	  tmp << "BUG: M[" << i << "] = " << M[i] 
		 << " (J_in = " << J[0] << ") S[" << i << "] = " << S[i];
	  msg.error (tmp.str ());
	}
      daisy_assert (M[i] >= 0.0);
      daisy_assert (C[i] >= 0.0);
    }
  if (nest)
    delete nest;
}

static struct TransportNoneSyntax
{
  static Transport& make (const AttributeList& al)
  {
    return *new TransportNone (al);
  }

  TransportNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No solute transport.");
    Librarian<Transport>::add_type ("none", alist, syntax, &make);
  }
} TransportNone_syntax;
