// ABAprod.C  -- Production of ABA in soil.
// 
// Copyright 2007 Per Abrahamsen and KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.5
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

#include "ABAprod.h"
#include "block.h"
#include "librarian.h"
#include "alist.h"

const char *const ABAProd::component = "ABAproduction";

symbol 
ABAProd::library_id () const
{
  static const symbol id (component);
  return id;
}

ABAProd::ABAProd (Block& al)
  : ModelLogable (al.identifier ("type"))
{ }

ABAProd::~ABAProd ()
{ }

static Librarian ABAProd_init (ABAProd::component, "\
The 'ABAproduction' component calculates the prod of ABA in soil.");

struct ABAProdNone : public ABAProd
{
  // Solve.
  void production (const Geometry&, const SoilWater&,
		   const std::vector<double>& /* [cm^3/cm^3] */,
		   const std::vector<double>& /* [cm/cm^3] */,
		   std::vector<double>& ABA /* [g/cm/h] */,
		   Treelog&) const
  { fill (ABA.begin (), ABA.end (), 0.0); }
  void output (Log&) const
  { }

  // Create and Destroy.
  void initialize (Treelog&)
  { }
  bool check (Treelog&) const
  { return true; }
  ABAProdNone (Block& al)
    : ABAProd (al)
  { }
  ~ABAProdNone ()
  { }
};

static struct ABAProdNoneSyntax
{
  static Model& make (Block& al)
  { return *new ABAProdNone (al); }
  ABAProdNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No ABA production.");
    Librarian::add_type (ABAProd::component, "none", alist, syntax, &make);
  }
} ABAProdNone_syntax;

const AttributeList& 
ABAProd::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      alist.add ("type", "none");
    }
  return alist;
}

// ABAprod.C ends here

