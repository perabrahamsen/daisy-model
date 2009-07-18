// doe.C --- A single element in a compund solute.
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

#define BUILD_DLL

#include "doe.h"
#include "log.h"
#include "geometry.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "soil.h"
#include "soil_water.h"
#include "treelog.h"
#include "assertion.h"
#include <string>

void 
DOE::output (Log& log) const
{
  output_variable (M, log);
  output_variable (C, log);
  output_variable (S, log);
  output_variable (S_p, log);
  output_variable (S_drain, log);
  output_variable (J_matrix, log);
  output_variable (J_tertiary, log);
}

void 
DOE::mix (const Geometry& geo, 
          const Soil& soil, const SoilWater& soil_water, 
          double from, double to)
{
  geo.mix (M, from, to);
  for (size_t i = 0; i < C.size (); i++)
    C[i] = M[i] / soil_water.Theta (i);
}

void 
DOE::swap (const Geometry& geo, 
           const Soil& soil, const SoilWater& soil_water,
           double from, double middle, double to)
{
  geo.swap (M, from, middle, to);
  for (size_t i = 0; i < C.size (); i++)
    C[i] = M[i] / soil_water.Theta (i);
}

void 
DOE::tick (const size_t cell_size, const SoilWater& soil_water, const double dt)
{
  // Initialize.
  fill (S_p.begin (), S_p.end (), 0.0);
  fill (J_tertiary.begin (), J_tertiary.end (), 0.0);

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
DOE::load_syntax (Frame& frame)
{
  // Submodel.

  // Content.
  frame.declare ("M", "g/cm^3", Attribute::State, Attribute::SoilCells,
                 "Mass in water and soil.");
  frame.declare ("C", "g/cm^3", Attribute::LogOnly, Attribute::SoilCells,
                 "Concentration in water.");
  frame.declare ("S", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Combined source term.");
  frame.declare ("S_p", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Source term (macropore transport only).");
  frame.declare ("S_drain", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Source term (soil drainage only).");
  frame.declare ("J_matrix", "g/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
                 "Transportation in matrix (positive up).");
  frame.declare ("J_tertiary", "g/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
                 "Transportation outside matrix (positive up).");
}

void 
DOE::initialize (const Geometry& geo, 
                 const Soil& soil, const SoilWater& soil_water,
                 Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  
  if (cell_size >= M.size ())
    M.insert (M.end (), cell_size - M.size (), 0.0);
  else
    msg.warning ("Too many elements of M in DOM pool");

  for (size_t i = C.size (); i < M.size (); i++)
    C.push_back (M[i] / soil_water.Theta (i));

  S.insert (S.begin (), cell_size, 0.0);
  S_p.insert (S_p.begin (), cell_size, 0.0);
  S_drain.insert (S_drain.begin (), cell_size, 0.0);
  J_matrix.insert (J_matrix.begin (), edge_size, 0.0);
  J_tertiary.insert (J_tertiary.begin (), edge_size, 0.0);
}

DOE::DOE (const FrameSubmodel& al)
  : M (al.number_sequence ("M"))
{ }

DOE::~DOE ()
{ }

static DeclareSubmodel 
dom_element_submodel (DOE::load_syntax, "DOM-Element", "\
A single element in a Dissolved Organic Matter pool.");

// doe.C ends here
