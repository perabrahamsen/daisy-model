// groundwater_extern.C --- Get groundwater table from external model.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "groundwater.h"
#include "output.h"
#include "number.h"
#include "block.h"
#include "alist.h"
#include "units.h"
#include "check.h"
#include "assertion.h"
#include "librarian.h"
#include <sstream>

class GroundwaterExtern : public Groundwater
{
  // Content.
private:
  const std::auto_ptr<Number> expr;
  bool has_table;
  double depth;
  
  // Groundwater.
public:
  bottom_t bottom_type () const
  { return has_table ? pressure : free_drainage; }
  double q_bottom () const
  { daisy_notreached (); }

  // Simulation.
public:
  void tick (const Geometry&,
             const Soil&, SoilWater&, double, 
	     const SoilHeat&, const Time&, const Scope& scope, Treelog& msg)
  { has_table = expr->tick_value (depth, Units::cm, scope, msg); }
  double table () const
  { return depth; }

  // Create and Destroy.
public:
  void initialize (const Geometry&, const Time&, const Scope&, Treelog& msg)
  { expr->initialize (msg); }

  bool check (const Geometry&, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    if (!expr->check_dim (scope, Units::cm, msg))
      ok = false;
    return ok;
  }
      
  GroundwaterExtern (Block& al)
    : Groundwater (al),
      expr (Librarian::build_item<Number> (al, "table")),
      has_table (al.check ("initial_table")),
      depth (al.number ("initial_table", -42.42e42))
  { }
  ~GroundwaterExtern ()
  { }
};

static struct GroundwaterExternSyntax
{
  static Model& make (Block& al)
  { return *new GroundwaterExtern (al); }
  GroundwaterExternSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Look up groundwater table in an scope.  ");
    Groundwater::load_syntax (syntax, alist);
    syntax.add_object ("table", Number::component, 
                       Syntax::Const, Syntax::Singleton, "\
Expression that evaluates to groundwate table in.");
    syntax.add ("initial_table", "cm", Check::none (), Syntax::OptionalConst,
		"Groundwater level for initialization of soil water.");
    Librarian::add_type (Groundwater::component, "extern", alist, syntax, &make);
  }
} GroundwaterExtern_syntax;
