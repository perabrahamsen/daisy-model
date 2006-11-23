// element.C --- A single element in a compund solute.
// 
// Copyright 2002, 2006 Per Abrahamsen and KVL.
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


#include "element.h"
#include "log.h"
#include "geometry.h"
#include "adsorption.h"
#include "submodel.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "timestep.h"
#include <string>

void 
Element::output (Log& log) const
{
  output_variable (M, log);
  output_variable (C, log);
  output_variable (S, log);
  output_variable (S_p, log);
  output_variable (S_drain, log);
  output_variable (J, log);
  output_variable (J_p, log);
}

void 
Element::mix (const Geometry& geo, 
              const Soil& soil, const SoilWater& soil_water, 
              Adsorption& adsorption,
              double from, double to)
{
  geo.mix (M, from, to);
  for (unsigned int i = 0; i < C.size (); i++)
    C[i] = adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]);
}

void 
Element::swap (const Geometry& geo, 
               const Soil& soil, const SoilWater& soil_water,
               Adsorption& adsorption,
               double from, double middle, double to)
{
  geo.swap (M, from, middle, to);
  for (unsigned int i = 0; i < C.size (); i++)
    C[i] = adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]);
}

void 
Element::tick (const size_t cell_size,
               const SoilWater& soil_water)
{
  // Initialize.
  fill (S_p.begin (), S_p.end (), 0.0);
  fill (J_p.begin (), J_p.end (), 0.0);

  daisy_assert (S_drain.size () >= cell_size);
  daisy_assert (S.size () >= cell_size);

  // Drainage.
  for (size_t i = 0; i < cell_size; i++)
    {
      S_drain[i] = -soil_water.S_drain (i) * dt * C[i];
      S[i] += S_drain[i];
    }
}

void 
Element::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Submodel.
  alist.add ("submodel", "DOM-Element");
  alist.add ("description", "\
A single element in a Dissolved Organic Matter pool.");

  // Content.
  syntax.add ("M", "g/cm^3", Syntax::State, Syntax::Sequence,
	      "Mass in water and soil.");
  syntax.add ("C", "g/cm^3", Syntax::LogOnly, Syntax::Sequence,
	      "Concentration in water.");
  syntax.add ("S", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Combined source term.");
  syntax.add ("S_p", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (macropore transport only).");
  syntax.add ("S_drain", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (soil drainage only).");
  syntax.add ("J", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in matrix (positive up).");
  syntax.add ("J_p", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in macropores (positive up).");
}

void 
Element::initialize (const Geometry& geo, 
                     const Soil& soil, const SoilWater& soil_water,
                     Adsorption& adsorption, Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  
  if (cell_size >= M.size ())
    M.insert (M.end (), cell_size - M.size (), 0.0);
  else
    msg.warning ("Too many elements of M in DOM pool");

  for (unsigned int i = C.size (); i < M.size (); i++)
    C.push_back (adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]));

  S.insert (S.begin (), cell_size, 0.0);
  S_p.insert (S_p.begin (), cell_size, 0.0);
  S_drain.insert (S_drain.begin (), cell_size, 0.0);
  J.insert (J_p.begin (), edge_size, 0.0);
  J_p.insert (J_p.begin (), edge_size, 0.0);
}

Element::Element (const AttributeList& al)
  : M (al.number_sequence ("M"))
{ }

Element::~Element ()
{ }

static Submodel::Register 
dom_element_submodel ("DOM-Element", Element::load_syntax);

// element.C ends here
