// tertiary.C --- Transport of water and solute outside the matrix.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "tertiary.h"
#include "block.h"
#include "alist.h"
#include "librarian.h"

// The 'tertiary' component.

const char *const Tertiary::component = "tertiary";

symbol
Tertiary::library_id () const
{
  static const symbol id (component);
  return id;
}

Tertiary::Tertiary (Block& al)
  : ModelAListed (al.alist ())
{ }

Tertiary::~Tertiary ()
{ }

static Librarian Tertiary_init (Tertiary::component, "\
Transport of water and solute outside the matrix.");

// The 'none' model.

class TertiaryNone : public Tertiary
{
  // Simulation.
  void tick_water (const Geometry&, const Soil&, const SoilWater&,
                   const double /* dt */,
                   Surface& /* surface */,
                   std::vector<double>& /* S_drain */,
                   std::vector<double>& /* S_matrix */,
                   std::vector<double>& /* q_tertiary */, 
                   Treelog&)
  { }
  void output (Log&) const
  { }

  // Create and Destroy.
  bool initialize (const Geometry&, const Soil&, const Scope&,  
                   const double /* pipe_position */, Treelog&)
  { return true; }
  bool check (const Geometry&, Treelog&) const
  { return true; }
public:
  TertiaryNone (Block& al)
    : Tertiary (al)
  { }
};

static struct TertiaryNoneSyntax
{
  static Model& make (Block& al)
  { return *new TertiaryNone (al); }
  TertiaryNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No tertiary transport.");
    Librarian::add_type (Tertiary::component, "none", alist, syntax, &make);
  }
} TertiaryNone_syntax;

const AttributeList& 
Tertiary::none_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    alist.add ("type", "none");

  return alist;
}

// tertiary.C ends here.
