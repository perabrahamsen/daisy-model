// ABAprod_root.C  -- ABA production based on root length.
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
#include "number.h"
#include "scope_id.h"
#include "geometry.h"
#include "soil_water.h"
#include "units.h"
#include "assertion.h"
#include "librarian.h"
#include "syntax.h"
#include "alist.h"

struct ABAProdRoot : public ABAProd
{
  // Units.
  static const symbol h_name;
  static const symbol ABA_unit;

  // Parameters.
  mutable ScopeID scope;
  const std::auto_ptr<Number> expr;
  
  // Solve.
  void production (const Geometry&, const SoilWater&,
		   const std::vector<double>& S /* [cm^3/cm^3/h] */,
		   const std::vector<double>& l /* [cm/cm^3] */,
		   std::vector<double>& ABA /* [g/cm^3/h] */,
		   Treelog&) const;
  void output (Log&) const
  { }

  // Create and Destroy.
  void initialize (Treelog&);
  bool check (Treelog&) const;
  ABAProdRoot (Block& al);
  ~ABAProdRoot ();
};

const symbol 
ABAProdRoot::h_name ("h");

const symbol 
ABAProdRoot::ABA_unit ("g/cm");

void
ABAProdRoot::production (const Geometry& geo, const SoilWater& soil_water,
			 const std::vector<double>& /* [cm^3/cm^3/h] */,
			 const std::vector<double>& l /* [cm/cm^3] */,
			 std::vector<double>& ABA    /* [g/cm^3/h] */,
			 Treelog& msg) const
{
  // Check input.
  const size_t cell_size = geo.cell_size ();
  daisy_assert (ABA.size () == cell_size);
  daisy_assert (l.size () == cell_size);
  
  // For all cells.
  for (size_t c = 0; c < cell_size; c++)
    {
      // Set up 'h' in scope.
      scope.set_number (h_name, soil_water.h (c));

      // Find root value.
      double value = 0.0;
      if (!expr->tick_value (value, ABA_unit, scope, msg))
	msg.error ("No ABA production value found");

      // Find ABA uptake.
      ABA[c] = value * l[c];	// [g/cm^3 S/h] = [g/cm/h] * [cm/cm^3 S]
    }
}

void 
ABAProdRoot::initialize (Treelog& msg)
{ expr->initialize (msg); }

bool 
ABAProdRoot::check (Treelog& msg) const
{
  bool ok = true;

  if (!expr->check_dim (scope, ABA_unit, msg))
    ok = false;

  return ok;
}

ABAProdRoot::ABAProdRoot (Block& al)
  : ABAProd (al),
    scope (h_name, Units::cm ()),
    expr (Librarian::build_item<Number> (al, "expr"))
{ }

ABAProdRoot::~ABAProdRoot ()
{ }

static struct ABAProdRootSyntax
{
  static Model& make (Block& al)
  { return *new ABAProdRoot (al); }
  ABAProdRootSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
ABA production based on production in roots.\n\
\n\
The assumptions are that that each length of root will produce ABA\n\
with a rate that depends solely on the water pressure in that cell,\n\
and that all the ABA will be included in the water uptake.");
    syntax.add_object ("expr", Number::component, 
                       Syntax::Const, Syntax::Singleton, "\
Expression to evaluate to ABA production per root length [g/cm/h].\n\
The symbol 'h' will be bound to the water pressure [cm].");
    syntax.order ("expr");
    Librarian::add_type (ABAProd::component, "root", alist, syntax, &make);
  }
} ABAProdRoot_syntax;

// ABAprod_root.C ends here

