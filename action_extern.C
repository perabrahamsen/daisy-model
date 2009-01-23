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
#include "block.h"
#include "check.h"
#include "assertion.h"
#include "units.h"
#include "frame.h"
#include "metalib.h"
#include "library.h"
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

static struct ActionExternSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionExtern (al); }
  ActionExternSyntax ()
    : DeclareModel (Action::component, "extern", "\
Select an external scope, and perform action.")
  { }
  
  void load_frame (Frame& frame) const
  {
    frame.add_object ("scope", Scopesel::component, 
                       Value::Const, Value::Singleton, "\
Scope to evaluate expessions in.");
    frame.add_object ("action", Action::component, 
                       "Action to perform if the condition is false.");
    frame.order ("scope", "action");
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
    daisy_assert (extern_scope)       ;
    ScopeMulti multi (*extern_scope, parent_scope);
    const Units& units = daisy.units ();
    if (!surface_expr->tick_value (units, surface_value, mm_per_h, multi, msg))
      surface_value = 0.0;
    if (!subsoil_expr->tick_value (units, subsoil_value, mm_per_h, multi, msg))
      subsoil_value = 0.0;
    if (!overhead_expr->tick_value (units, overhead_value,
                                    mm_per_h, multi, msg))
      overhead_value = 0.0;
    if (!NO3_expr->tick_value (units, NO3_value, kg_N_per_ha_per_h, multi, msg))
      NO3_value = 0.0;
    if (!NH4_expr->tick_value (units, NH4_value, kg_N_per_ha_per_h, multi, msg))
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
        const Units& units = daisy.units ();
        const Unit& u_kg_per_ha = units.get_unit (kg_per_ha);
        const Unit& u_mg_per_square_m = units.get_unit (mg_per_square_m);
        const Unit& u_ppm = units.get_unit (Units::ppm ());
        const Unit& u_per_mm = units.get_unit (Units::per_mm ());
	IM im (u_mg_per_square_m);
	im.set_value (Chemical::NH4 (), u_kg_per_ha, NH4_value * dt);
	im.set_value (Chemical::NO3 (), u_kg_per_ha, NO3_value * dt);
	im.multiply_assign (Scalar (total_flux * dt, u_per_mm), u_ppm);
        
	if (surface_value > 0)
	  field.irrigate_surface (surface_value, im, dt, msg); 
	if (overhead_value > 0)
	  field.irrigate_overhead (overhead_value, im, dt, msg); 
	if (subsoil_value > 0)
	  field.irrigate_subsoil (subsoil_value, im, from, to, dt, msg); 
      }
    else if (NH4_value + NO3_value > 0.0)
      {
        Metalib& metalib = daisy.metalib;
        const Library& library = metalib.library (AM::component);
	AttributeList alist (library.lookup ("mineral"));
	AM::set_mineral (metalib, alist, NH4_value, NO3_value);
        field.fertilize (metalib, alist, daisy.time, dt, msg);
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

    ScopeMulti multi (*extern_scope, parent_scope);
    const Units& units = daisy.units ();
    surface_expr->initialize (units, multi, msg);
    subsoil_expr->initialize (units, multi, msg);
    overhead_expr->initialize (units, multi, msg);
    NH4_expr->initialize (units, multi, msg);
    NO3_expr->initialize (units, multi, msg);
  }

  bool check (const Daisy& daisy, const Scope& parent_scope, 
              Treelog& msg) const
  { 
    bool ok = true; 

    if (!extern_scope)
      {
        msg.error ("Extern scope not found");
        return false;
      }

    ScopeMulti multi (*extern_scope, parent_scope);
    const Units& units = daisy.units ();
    if (!surface_expr->check_dim (units, multi, mm_per_h, msg))
      ok = false;
    if (!subsoil_expr->check_dim (units, multi, mm_per_h, msg))
      ok = false;
    if (!overhead_expr->check_dim (units, multi, mm_per_h, msg))
      ok = false;
    if (!NH4_expr->check_dim (units, multi, kg_N_per_ha_per_h, msg))
      ok = false;
    if (!NO3_expr->check_dim (units, multi, kg_N_per_ha_per_h, msg))
      ok = false;

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


static struct ActionExternFertigationSyntax : public DeclareModel
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

  Model* make (Block& al) const
  { return new ActionExternFertigation (al); }

  ActionExternFertigationSyntax ()
    : DeclareModel (Action::component, "extern_fertigation", "\
Continiues irrigation with mineral nitrogen mix.\n\
\n\
If the nitrogen amount is non-zero, it will be applied in the\n\
irrigation water if available, and otherwise be spread on the soil\n\
surface.")
  { }
  
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);	
    frame.add_object ("scope", Scopesel::component, 
                       Value::Const, Value::Singleton, "\
Scope to evaluate expessions in.");
    frame.add ("scope", "null");

    frame.add_object ("surface", Number::component, 
		       Value::Const, Value::Singleton, 
"Amount of surface irrigation applied.");
    frame.add_object ("overhead", Number::component, 
		       Value::Const, Value::Singleton, 
"Amount of overhead irrigation applied.");
    frame.add_object ("subsoil", Number::component, 
		       Value::Const, Value::Singleton, 
"Amount of subsoil irrigation applied.");
    frame.add_object ("NO3", Number::component, 
		       Value::Const, Value::Singleton, 
"Amount of NO3 in irrigation.");
    frame.add_object ("NH4", Number::component, 
		       Value::Const, Value::Singleton, 
"Amount of NH4 in irrigation.");

    frame.add ("from", "cm", Check::non_positive (), Value::Const, "\
Height where you want to start the incorporation (a negative number).");
    frame.add ("from", 0.0);
    frame.add ("to", "cm", Check::negative (), Value::Const, "\
Height where you want to end the incorporation (a negative number).");
    frame.add ("from", -10.0);
  }
} ActionExternFertigation_syntax;

// action_extern.C ends here.
