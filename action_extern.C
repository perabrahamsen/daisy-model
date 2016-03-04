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
#include "block_model.h"
#include "check.h"
#include "assertion.h"
#include "units.h"
#include "frame_model.h"
#include "metalib.h"
#include "library.h"
#include "volume.h"
#include <memory>

// The 'extern' action.

struct ActionExtern : public Action
{
  const std::unique_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;
  const std::unique_ptr<Action> child;

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
  { output_object (child, "action", log); }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    extern_scope = scopesel->lookup (daisy.scopes (), msg); 
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

  ActionExtern (const BlockModel& al)
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
  Model* make (const BlockModel& al) const
  { return new ActionExtern (al); }
  ActionExternSyntax ()
    : DeclareModel (Action::component, "extern", "\
Select an external scope, and perform action.")
  { }
  
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("scope", Scopesel::component, 
                       Attribute::Const, Attribute::Singleton, "\
Scope to evaluate expessions in.");
    frame.declare_object ("action", Action::component, 
                       "Action to perform if the condition is false.");
    frame.order ("scope", "action");
  }
} ActionExtern_syntax;

// The 'extern_fertigation' action.

struct ActionExternFertigation : public Action
{
  const Metalib& metalib;
  const std::unique_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;

  const std::unique_ptr<Number> surface_expr;
  double surface_value;
  const std::unique_ptr<Number> subsoil_expr;
  double subsoil_value;
  const std::unique_ptr<Number> overhead_expr;
  double overhead_value;
  const std::unique_ptr<Number> NO3_expr;
  double NO3_value;
  const std::unique_ptr<Number> NH4_expr;
  double NH4_value;
  const boost::shared_ptr<Volume> volume;

  static const symbol kg_N_per_ha_per_h;

  void tick (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    daisy_assert (extern_scope)       ;
    ScopeMulti multi (*extern_scope, parent_scope);
    const Units& units = daisy.units ();
    if (!surface_expr->tick_value (units, surface_value,
                                   Units::mm_per_h (), multi, msg))
      surface_value = 0.0;
    if (!subsoil_expr->tick_value (units, subsoil_value,
                                   Units::mm_per_h (), multi, msg))
      subsoil_value = 0.0;
    if (!overhead_expr->tick_value (units, overhead_value,
                                    Units::mm_per_h (), multi, msg))
      overhead_value = 0.0;
    if (!NO3_expr->tick_value (units, NO3_value, kg_N_per_ha_per_h, multi, msg))
      NO3_value = 0.0;
    if (!NH4_expr->tick_value (units, NH4_value, kg_N_per_ha_per_h, multi, msg))
      NH4_value = 0.0;
  }

  void doIt (Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    Field& field = daisy.field ();
    const double duration = 1.0; // [h]

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
	im.set_value (Chemical::NH4 (), u_kg_per_ha, NH4_value * duration);
	im.set_value (Chemical::NO3 (), u_kg_per_ha, NO3_value * duration);
        const double water = total_flux * duration; // [mm]
	im.multiply_assign (Scalar (1.0 / water, u_per_mm), u_ppm);
        
	if (surface_value > 0)
	  field.irrigate (duration, surface_value,
                          Irrigation::at_air_temperature,
                          Irrigation::surface, im, boost::shared_ptr<Volume> (),
                          false, msg); 
	if (overhead_value > 0)
	  field.irrigate (duration, overhead_value,
                          Irrigation::at_air_temperature,
                          Irrigation::overhead, im, 
                          boost::shared_ptr<Volume> (), false, msg); 
	if (subsoil_value > 0)
	  field.irrigate (duration, subsoil_value,
                          Irrigation::at_air_temperature,
                          Irrigation::subsoil, im, volume, false, msg); 

      }
    else if (NH4_value + NO3_value > 0.0)
      {
        const Library& library = metalib.library (AM::component);
	FrameModel frame (library.model ("mineral"), Frame::parent_link);
	AM::set_mineral (metalib, frame, NH4_value, NO3_value);
        field.fertilize (metalib, frame, daisy.time (), msg);
      }
  }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    extern_scope = scopesel->lookup (daisy.scopes (), msg); 
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
    if (!surface_expr->check_dim (units, multi, Units::mm_per_h (), msg))
      ok = false;
    if (!subsoil_expr->check_dim (units, multi, Units::mm_per_h (), msg))
      ok = false;
    if (!overhead_expr->check_dim (units, multi, Units::mm_per_h (), msg))
      ok = false;
    if (!NH4_expr->check_dim (units, multi, kg_N_per_ha_per_h, msg))
      ok = false;
    if (!NO3_expr->check_dim (units, multi, kg_N_per_ha_per_h, msg))
      ok = false;

    return ok;
  }

  ActionExternFertigation (const BlockModel& al)
    : Action (al),
      metalib (al.metalib ()),
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
      volume (Volume::build_obsolete (al).release ())
  { }
  ~ActionExternFertigation ()
  { }
};

const symbol 
ActionExternFertigation::kg_N_per_ha_per_h ("kg N/ha/h");

static struct ActionExternFertigationSyntax : public DeclareModel
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
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

  Model* make (const BlockModel& al) const
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
    Model::load_model (frame);
    frame.add_check (check_alist);	
    frame.declare_object ("scope", Scopesel::component, 
                       Attribute::Const, Attribute::Singleton, "\
Scope to evaluate expessions in.");
    frame.set ("scope", "null");

    frame.declare_object ("surface", Number::component, 
		       Attribute::Const, Attribute::Singleton, 
"Amount of surface irrigation applied.");
    frame.declare_object ("overhead", Number::component, 
		       Attribute::Const, Attribute::Singleton, 
"Amount of overhead irrigation applied.");
    frame.declare_object ("subsoil", Number::component, 
		       Attribute::Const, Attribute::Singleton, 
"Amount of subsoil irrigation applied.");
    frame.declare_object ("NO3", Number::component, 
		       Attribute::Const, Attribute::Singleton, 
"Amount of NO3 in irrigation.");
    frame.declare_object ("NH4", Number::component, 
		       Attribute::Const, Attribute::Singleton, 
"Amount of NH4 in irrigation.");

    frame.declare_object ("volume", Volume::component, 
                       Attribute::Const, Attribute::Singleton,
                       "Soil volume to add irritaion.");
    frame.set ("volume", "box");
    frame.declare ("from", "cm", Check::non_positive (), Attribute::Const, "\
Height where you want to start the incorporation (a negative number).");
    frame.set ("from", 0.0);
    frame.declare ("to", "cm", Check::negative (), Attribute::Const, "\
Height where you want to end the incorporation (a negative number).");
    frame.set ("from", -10.0);
  }
} ActionExternFertigation_syntax;

// The 'extern_subsoil' action.

struct ActionExternSubsoil : public Action
{
  const std::unique_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;

  const std::unique_ptr<Number> expr_flux;
  double flux;
  
  const std::vector<symbol> constituents;
  IM sm;

  boost::shared_ptr<Volume> volume;

  void tick (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    daisy_assert (extern_scope);
    ScopeMulti multi (*extern_scope, parent_scope);
    const Units& units = daisy.units ();
    if (expr_flux->tick_value (units, flux, Units::mm_per_h (), multi, msg))
      {
        for (size_t i = 0; i < constituents.size (); i++)
          {
            const symbol name = constituents[i];
            daisy_assert (multi.lookup (name) == Attribute::Number);
            daisy_assert (multi.type_size (name) == Attribute::Singleton);
            daisy_assert (multi.check (name));
            const double value = multi.number (name);
            const symbol dim = multi.dimension (name);
            daisy_assert (units.has_unit (dim));
            const Unit& unit = units.get_unit (dim);
            sm.set_value (name, unit, value);
          }
      }
    else
      flux = 0.0;
  }

  void doIt (Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    if (flux < 1e-100)
      return;
    const double duration = 1.0; // [h]
    daisy.field ().irrigate (duration, flux, Irrigation::at_air_temperature,
                           Irrigation::subsoil, sm, volume, false, msg); 
  }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    extern_scope = scopesel->lookup (daisy.scopes (), msg); 
    if (!extern_scope)
      return;
    
    ScopeMulti multi (*extern_scope, parent_scope);
    const Units& units = daisy.units ();
    expr_flux->initialize (units, multi, msg);
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
    if (!expr_flux->check_dim (units, multi, Units::mm_per_h (), msg))
      ok = false;
    
    for (size_t i = 0; i < constituents.size (); i++)
      {
        const symbol name = constituents[i];
        if (multi.lookup (name) != Attribute::Number)
          {
            msg.error ("'" + name + "': No such number");
            ok = false;
            continue;
          }
        if (multi.type_size (name) != Attribute::Singleton)
          {
            msg.error ("'" + name + "': Not a singleton");
            ok = false;
            continue;
          }
        const symbol dim = multi.dimension (name);
        if (!units.can_convert (dim, Units::ppm ()))
          ok = false;
      }

    return ok;
  }

  ActionExternSubsoil (const BlockModel& al)
    : Action (al),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      extern_scope (NULL),
      expr_flux (Librarian::build_item<Number> (al, "flux")),
      flux (-42.42e42),
      constituents (al.name_sequence ("constituents")),
      sm (al.units ().get_unit (Units::ppm ())),
      volume (Volume::build_obsolete (al).release ())
  { }
  ~ActionExternSubsoil ()
  { }
};

static struct ActionExternSubsoilSyntax : public DeclareModel
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  { 
    bool ok = true;
    if (al.check ("from") && al.check ("to"))
      {
        const double from = al.number ("from");
        const double to = al.number ("to");
        if (from <= to)
          {
            err.entry ("'from' must be higher than 'to' in"
                       " the subsoil irrigation zone");
            ok = false;
          }
      }
    return ok;
  }

  Model* make (const BlockModel& al) const
  { return new ActionExternSubsoil (al); }

  ActionExternSubsoilSyntax ()
    : DeclareModel (Action::component, "extern_subsoil", "\
Subsoil irrigation controlled externally.")
  { }
  
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.add_check (check_alist);	
    frame.declare_object ("scope", Scopesel::component, 
                       Attribute::Const, Attribute::Singleton, "\
Scope to evaluate expessions in.");

    frame.declare_object ("flux", Number::component, 
                          Attribute::Const, Attribute::Singleton, 
"Amount of irrigation applied.");

    frame.declare_string ("constituents", 
                          Attribute::Const, Attribute::Variable, "\
List of solutes to add to the irrigation water.\n\
The values are taken from the external scope, dimensions must be\n      \
convertible to ppm.");

    frame.declare_object ("volume", Volume::component, 
                       Attribute::Const, Attribute::Singleton,
                       "Soil volume to add irritaion.");
    frame.set ("volume", "box");
    frame.declare ("from", "cm", Check::non_positive (), Attribute::OptionalConst, "\
Height where you want to start the incorporation (a negative number).\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    frame.declare ("to", "cm", Check::non_positive (), Attribute::OptionalConst, "\
Height where you want to end the incorporation (a negative number).\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
  }
} ActionExternSubsoil_syntax;

// action_extern.C ends here.
