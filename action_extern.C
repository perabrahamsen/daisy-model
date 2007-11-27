// action_extern.C -- Make external information available.
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

#include "action.h"
#include "scope_multi.h"
#include "scopesel.h"
#include "number.h"
#include "daisy.h"
#include "field.h"
#include "am.h"
#include "im.h"
#include "chemical.h"
#include "log.h"
#include "treelog.h"
#include "librarian.h"
#include "syntax.h"
#include "block.h"
#include "check.h"
#include "assertion.h"
#include "units.h"
#include <memory>

struct ActionExtern : public Action
{
  const std::auto_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;
  const std::auto_ptr<Action> child;

  void tick (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    ScopeMulti multi (*extern_scope, parent_scope);
    child->tick (daisy, multi, msg);  
  }

  void doIt (Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    ScopeMulti multi (*extern_scope, parent_scope);
    child->doIt (daisy, multi, msg);
  }

  bool done (const Daisy& daisy, const Scope& parent_scope, Treelog& msg) const
  { 
    ScopeMulti multi (*extern_scope, parent_scope);
    return child->done (daisy, multi, msg); 
  }

  void output (Log& log) const
  { output_derived (child, "action", log); }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    extern_scope = scopesel->lookup (*daisy.output_log, msg); 
    if (extern_scope)
      {
        ScopeMulti multi (*extern_scope, parent_scope);
        child->initialize (daisy, multi, msg);
      }
  }

  bool check (const Daisy& daisy, const Scope& parent_scope, 
              Treelog& msg) const
  { 
    bool ok = true; 

    if (!extern_scope)
      {
        msg.error ("Extern scope not found");
        ok = false;
      }
    else
      {
        ScopeMulti multi (*extern_scope, parent_scope);
        if (!child->check (daisy, multi, msg))
          ok = false;
      }
    return ok;
  }

  ActionExtern (Block& al)
    : Action (al),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      extern_scope (NULL),
      child (Librarian::build_item<Action> (al, "action"))
  { }
  ~ActionExtern ()
  { }
};

static struct ActionExternSyntax
{
  static Model& make (Block& al)
  { return *new ActionExtern (al); }
  ActionExternSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Select an external scope, and perform action.");

    syntax.add_object ("scope", Scopesel::component, 
                       Syntax::Const, Syntax::Singleton, "\
Scope to evaluate expessions in.");
    syntax.add_object ("action", Action::component, 
                       "Action to perform if the condition is false.");
    syntax.order ("scope", "action");

    Librarian::add_type (Action::component, "extern", alist, syntax, &make);
  }
} ActionExtern_syntax;

struct ActionExternFertigation : public Action
{
  const std::auto_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;

  const std::auto_ptr<Number> surface_expr;
  double surface_value;
  const std::auto_ptr<Number> subsoil_expr;
  double subsoil_value;
  const std::auto_ptr<Number> overhead_expr;
  double overhead_value;
  const std::auto_ptr<Number> NO3_expr;
  double NO3_value;
  const std::auto_ptr<Number> NH4_expr;
  double NH4_value;
  const double from;
  const double to;

  static const symbol kg_N_per_ha_per_h;
  static const symbol mm_per_h;

  void tick (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    daisy_assert (extern_scope);
    ScopeMulti multi (*extern_scope, parent_scope);
    if (!surface_expr->tick_value (surface_value, mm_per_h, multi, msg))
      surface_value = 0.0;
    if (!subsoil_expr->tick_value (subsoil_value, mm_per_h, multi, msg))
      subsoil_value = 0.0;
    if (!overhead_expr->tick_value (overhead_value, mm_per_h, multi, msg))
      overhead_value = 0.0;
    if (!NO3_expr->tick_value (NO3_value, kg_N_per_ha_per_h, multi, msg))
      NO3_value = 0.0;
    if (!NH4_expr->tick_value (NH4_value, kg_N_per_ha_per_h, multi, msg))
      NH4_value = 0.0;
  }

  void doIt (Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    Field& field = *daisy.field;
    const double dt = daisy.dt;

    daisy_assert (extern_scope);
    ScopeMulti multi (*extern_scope, parent_scope);

    const double total_flux
      = surface_value + subsoil_value + overhead_value;

    if (total_flux > 0.0)
      {
	static const symbol mg_per_square_m ("mg/m^2");
	static const symbol kg_per_ha ("kg/ha");
	IM im (mg_per_square_m);
	im.set_value (Chemical::NH4_solute (), kg_per_ha, NH4_value * dt);
	im.set_value (Chemical::NO3 (),        kg_per_ha, NO3_value * dt);
	im *= Scalar (total_flux * dt, Units::per_mm ());

	if (surface_value > 0)
	  field.irrigate_surface (surface_value, im, dt, msg); 
	if (overhead_value > 0)
	  field.irrigate_overhead (overhead_value, im, dt, msg); 
	if (subsoil_value > 0)
	  field.irrigate_subsoil (subsoil_value, im, from, to, dt, msg); 
      }
    else if (NH4_value + NO3_value > 0.0)
      {
	AttributeList alist;
	AM::set_mineral (alist, NH4_value, NO3_value);
	field.fertilize (alist, dt, msg);
      }
  }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    extern_scope = scopesel->lookup (*daisy.output_log, msg); 
    if (!extern_scope)
      return;

    surface_expr->initialize (msg);
    subsoil_expr->initialize (msg);
    overhead_expr->initialize (msg);
    NH4_expr->initialize (msg);
    NO3_expr->initialize (msg);
  }

  bool check (const Daisy& daisy, const Scope& parent_scope, 
              Treelog& msg) const
  { 
    bool ok = true; 

    if (!extern_scope)
      {
        msg.error ("Extern scope not found");
        ok = false;
      }
    else
      {
        ScopeMulti multi (*extern_scope, parent_scope);
	if (!surface_expr->check_dim (multi, mm_per_h, msg))
	  ok = false;
	if (!subsoil_expr->check_dim (multi, mm_per_h, msg))
	  ok = false;
	if (!overhead_expr->check_dim (multi, mm_per_h, msg))
	  ok = false;
	if (!NH4_expr->check_dim (multi, kg_N_per_ha_per_h, msg))
	  ok = false;
	if (!NO3_expr->check_dim (multi, kg_N_per_ha_per_h, msg))
	  ok = false;
      }
    return ok;
  }

  ActionExternFertigation (Block& al)
    : Action (al),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      extern_scope (NULL),
      surface_expr (Librarian::build_item<Number> (al, "surface")),
      surface_value (0.0),
      subsoil_expr (Librarian::build_item<Number> (al, "subsoil")),
      subsoil_value (0.0),
      overhead_expr (Librarian::build_item<Number> (al, "overhead")),
      overhead_value (0.0),
      NO3_expr (Librarian::build_item<Number> (al, "NO3")),
      NO3_value (0.0),
      NH4_expr (Librarian::build_item<Number> (al, "NH4")),
      NH4_value (0.0),
      from (al.number ("from")),
      to (al.number ("to"))
  { }
  ~ActionExternFertigation ()
  { }
};

const symbol 
ActionExternFertigation::kg_N_per_ha_per_h ("kg N/ha/h");

const symbol
ActionExternFertigation::mm_per_h ("mm/h");


static struct ActionExternFertigationSyntax
{
  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    bool ok = true;
    const double from = al.number ("from");
    const double to = al.number ("to");
    if (from <= to)
      {
	err.entry ("'from' must be higher than 'to' in"
		   " the subsoil irrigation zone");
	ok = false;
      }
    return ok;
  }

  static Model& make (Block& al)
  { return *new ActionExternFertigation (al); }
  ActionExternFertigationSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);	
    
    alist.add ("description", "\
Continiues irrigation with mineral nitrogen mix.\n\
\n\
If the nitrogen amount is non-zero, it will be applied in the\n\
irrigation water if available, and otherwise be spread on the soil\n\
surface.");

    syntax.add_object ("scope", Scopesel::component, 
                       Syntax::Const, Syntax::Singleton, "\
Scope to evaluate expessions in.");
    alist.add ("scope", Scopesel::default_model ());

    syntax.add_object ("surface", Number::component, 
		       Syntax::Const, Syntax::Singleton, 
"Amount of surface irrigation applied.");
    syntax.add_object ("overhead", Number::component, 
		       Syntax::Const, Syntax::Singleton, 
"Amount of overhead irrigation applied.");
    syntax.add_object ("subsoil", Number::component, 
		       Syntax::Const, Syntax::Singleton, 
"Amount of subsoil irrigation applied.");
    syntax.add_object ("NO3", Number::component, 
		       Syntax::Const, Syntax::Singleton, 
"Amount of NO3 in irrigation.");
    syntax.add_object ("NH4", Number::component, 
		       Syntax::Const, Syntax::Singleton, 
"Amount of NH4 in irrigation.");

    syntax.add ("from", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Check::negative (), Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");
    alist.add ("from", -10.0);

    Librarian::add_type (Action::component, "extern_fertigation",
			 alist, syntax, &make);
  }
} ActionExternFertigation_syntax;

// action_extern.C ends here.
