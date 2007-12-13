// ABAprod_uptake.C  -- ABA production based on water uptake.
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

struct ABAProdUptake : public ABAProd
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
  ABAProdUptake (Block& al);
  ~ABAProdUptake ();
};

const symbol 
ABAProdUptake::h_name ("h");

const symbol 
ABAProdUptake::ABA_unit ("g/cm^3");

void
ABAProdUptake::production (const Geometry& geo, const SoilWater& soil_water,
			 const std::vector<double>& S /* [cm^3/cm^3/h] */,
			 const std::vector<double>& /* l [cm/cm^3] */,
			 std::vector<double>& ABA    /* [g/cm^3/h] */,
			 Treelog& msg) const
{
  // Check input.
  const size_t cell_size = geo.cell_size ();
  daisy_assert (ABA.size () == cell_size);
  daisy_assert (S.size () == cell_size);
  
  // For all cells.
  for (size_t c = 0; c < cell_size; c++)
    {
      // Set up 'h' in scope.
      scope.set_number (h_name, soil_water.h (c));

      // Find soil value.
      double value = 0.0;
      if (!expr->tick_value (value, ABA_unit, scope, msg))
	msg.error ("No ABA production value found");

      // Find ABA uptake.
      ABA[c] = value * S[c];
      // [g/cm^3 S/h] = [g/cm^3 W] * [cm^3 W/cm^3 S/h]
    }
}

void 
ABAProdUptake::initialize (Treelog& msg)
{ expr->initialize (msg); }

bool 
ABAProdUptake::check (Treelog& msg) const
{
  bool ok = true;

  if (!expr->check_dim (scope, ABA_unit, msg))
    ok = false;

  return ok;
}

ABAProdUptake::ABAProdUptake (Block& al)
  : ABAProd (al),
    scope (h_name, Units::cm ()),
    expr (Librarian::build_item<Number> (al, "expr"))
{ }

ABAProdUptake::~ABAProdUptake ()
{ }

static struct ABAProdUptakeSyntax
{
  static Model& make (Block& al)
  { return *new ABAProdUptake (al); }
  ABAProdUptakeSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
ABA production based on concentration in water uptake.\n\
\n\
The assumption is water uptake from roots in specific region of the soil\n\
comes with a specific ABA concentration, which depends solely on the\n\
water pressure in that region.");
    syntax.add_object ("expr", Number::component, 
                       Syntax::Const, Syntax::Singleton, "\
Expression to evaluate to ABA concentration in water uptake [g/cm^3].\n\
The symbol 'h' will be bound to the water pressure [cm].");
    Librarian::add_type (ABAProd::component, "uptake", alist, syntax, &make);
  }
} ABAProdUptake_syntax;

// ABAprod_uptake.C ends here

