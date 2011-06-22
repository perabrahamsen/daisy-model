// number_soil.C -- Extract soil properties.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "number.h"
#include "metalib.h"
#include "library.h"
#include "block_model.h"
#include "column.h"
#include "horizon.h"
#include "hydraulic.h"
#include "wsource.h"
#include "output.h"
#include "time.h"
#include "librarian.h"
#include "scope.h"
#include "units.h"
#include "treelog.h"
#include "frame_model.h"
#include <memory>

struct NumberByDepth : public Number
{
  const Units& units;

  // Parameters.
  const std::auto_ptr<Column> column;
  /* const */ double max_depth;
  const std::auto_ptr<Number> h;
  const std::auto_ptr<Number> z;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { 
    h->tick (units, scope, msg);
    z->tick (units, scope, msg);
  }
  bool missing (const Scope& scope) const 
  { 
    if (h->missing (scope) 
        || !units.can_convert (h->dimension (scope), Units::cm (),
                               h->value (scope))
        || z->missing (scope) 
        || !units.can_convert (z->dimension (scope), Units::cm (), 
                               z->value (scope)))
      return true;

    const double height = units.convert (z->dimension (scope), 
                                         Units::cm (), 
                                         z->value (scope));
    if (height > 0 || height < max_depth)
      return true;

    return false;
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    bool ok = true;
    TREELOG_MODEL (msg);
    if (!h->initialize (units, scope, msg))
      ok = false;
    if (!z->initialize (units, scope, msg))
      ok = false;
    return ok;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    bool ok = true;
    TREELOG_MODEL (msg);
    if (!h->check (units, scope, msg))
      ok = false;
    else if (!units.can_convert (h->dimension (scope), Units::cm ()))
      {
        msg.error ("Cannot convert pressure [" + h->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        ok = false;
      }
    if (!z->check (units, scope, msg))
      ok = false;
    else if (!units.can_convert (z->dimension (scope), Units::cm ()))
      {
        msg.error ("Cannot convert height [" + z->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        ok = false;
      }
    return ok;
  }
  NumberByDepth (const BlockModel& al)
    : Number (al),
      units (al.units ()),
      column (Librarian::build_item<Column> (al, "column")),
      h (Librarian::build_item<Number> (al, "h")),
      z (Librarian::build_item<Number> (al, "z"))
  { 
    Output output;
    Time time (9999, 1, 1, 0);
    const Library& wlib = al.metalib ().library (WSource::component);
    const double T = 10.0;
    FrameModel frame (wlib.model ("const"), Frame::parent_link);
    frame.set ("average", T);
    frame.set ("amplitude", 0.0);
    frame.set ("air_temperature", T);
    std::auto_ptr<WSource> weather (Librarian::build_frame<WSource>
                                    (al, frame, "initialize"));
    column->initialize (al, output.scopes (),
                        time, weather.get (), Scope::null ());
    max_depth = column->bottom ();
  }
};

static struct NumberDepthSyntax : public DeclareBase
{
  NumberDepthSyntax ()
    : DeclareBase (Number::component, "depth", 
                   "Find soil value at specific depth.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("column", Column::component, "\
The soil column whose properties we want to examine.");
    frame.declare_object ("h", Number::component, "\
The tension we want to compare with.");
    frame.declare_object ("z", Number::component, "\
The height we want to compare with.");
  }
} NumberDepth_syntax;

struct NumberDepthTheta : public NumberByDepth
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double pressure 
      = units.convert (h->dimension (scope), Units::cm (), h->value (scope));
    const double height
      = units.convert (z->dimension (scope), Units::cm (), z->value (scope));
    const Horizon& horizon = column->horizon_at (height, 0.5, 0.5);
    return horizon.hydraulic->Theta (pressure);
  }

  symbol dimension (const Scope&) const 
  { return Attribute::Fraction (); }

  // Create.
  NumberDepthTheta (const BlockModel& al)
    : NumberByDepth (al)
  { }
};

static struct NumberDepthThetaSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberDepthTheta (al); }
  NumberDepthThetaSyntax ()
    : DeclareModel (Number::component, "depth_Theta", "depth",
                    "Find water content (Theta) for a given pressure (h).")
  { }
  void load_frame (Frame&) const
  { }
} NumberDepthTheta_syntax;

struct NumberDepthK : public NumberByDepth
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double pressure 
      = units.convert (h->dimension (scope), Units::cm (), h->value (scope));
    const double height
      = units.convert (z->dimension (scope), Units::cm (), z->value (scope));
    const Horizon& horizon = column->horizon_at (height, 0.5, 0.5);
    return horizon.hydraulic->K (pressure);
  }

  symbol dimension (const Scope&) const 
  { return units.cm_per_h (); }

  // Create.
  NumberDepthK (const BlockModel& al)
    : NumberByDepth (al)
  { }
};

static struct NumberDepthKSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberDepthK (al); }
  NumberDepthKSyntax ()
    : DeclareModel (Number::component, "depth_K", "depth", 
                    "Find water conductivity (K) for a given pressure (h).")
  { }
  void load_frame (Frame&) const
  { }
} NumberDepthK_syntax;

struct NumberByTension : public Number
{
  const Units& units;

  // Parameters.
  const std::auto_ptr<Horizon> horizon;
  const std::auto_ptr<Number> h;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { h->tick (units, scope, msg); }
  bool missing (const Scope& scope) const 
  { return h->missing (scope) 
      || !units.can_convert (h->dimension (scope), Units::cm (),
                              h->value (scope)); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    bool ok = true;
    TREELOG_MODEL (msg);
    if (!h->initialize (units, scope, msg))
      ok = false;
    return ok;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    if (!h->check (units, scope, msg))
      return false;
    if (!units.can_convert (h->dimension (scope), Units::cm ()))
      {
        msg.error ("Cannot convert [" + h->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        return false;
      }
    return true;
  }
  NumberByTension (const BlockModel& al)
    : Number (al),
      units (al.units ()),
      horizon (Librarian::build_item<Horizon> (al, "horizon")),
      h (Librarian::build_item<Number> (al, "h"))
  { horizon->initialize (al.flag ("top_soil"), 2, al.msg ()); }
};

static struct NumberHorizonSyntax : public DeclareBase
{
  NumberHorizonSyntax ()
    : DeclareBase (Number::component, "horizon", 
                   "Find soil value at specific horizon.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("horizon", Horizon::component, "\
The soil horizon whose properties we want to examine.");
    frame.declare_object ("h", Number::component, "\
The tension we want to compare with.");
    frame.declare_boolean ("top_soil", Attribute::Const, "\
Set this to true for the A horizon.");
  }
} NumberHorizon_syntax;


struct NumberSoilTheta : public NumberByTension
{
  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->Theta (units.convert (h->dimension (scope), 
                                                      Units::cm (), 
                                                      h->value (scope)));
  }
  symbol dimension (const Scope&) const 
  { return Attribute::Fraction (); }

  // Create.
  NumberSoilTheta (const BlockModel& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilThetaSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSoilTheta (al); }
  NumberSoilThetaSyntax ()
    : DeclareModel (Number::component, "soil_Theta", "horizon",
	       "Find water content (Theta) for a given pressure (h).")
  { }
  void load_frame (Frame&) const
  { }
} NumberSoilTheta_syntax;

struct NumberSoilK : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    return horizon->K (units.convert (h->dimension (scope), 
                                      Units::cm (), 
                                      h->value (scope)));
  }
  symbol dimension (const Scope&) const 
  { return units.cm_per_h (); }

  // Create.
  NumberSoilK (const BlockModel& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilKSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSoilK (al); }
  NumberSoilKSyntax ()
    : DeclareModel (Number::component, "soil_K", "horizon",
	       "Find hydraulic conductivity (K) for a given pressure (h).")
  { }
  void load_frame (Frame&) const
  { }
} NumberSoilK_syntax;

struct NumberSoilHeatCapacity : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const symbol my_dim = h->dimension (scope);
    const double my_val = h->value (scope);
    const double my_h = units.convert (my_dim, Units::cm (), my_val);
    const double Theta = horizon->hydraulic->Theta (my_h);
    return horizon->heat_capacity (Theta, 0.0);
  }
  symbol dimension (const Scope&) const 
  { 
    static const symbol dim ("erg/cm^3/dg C");
    return dim;
  }

  // Create.
  NumberSoilHeatCapacity (const BlockModel& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilHeatCapacitySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSoilHeatCapacity (al); }
  NumberSoilHeatCapacitySyntax ()
    : DeclareModel (Number::component, "soil_heat_capacity", "horizon",
	       "Find heat capacity for a given pressure (h).")
  { }
  void load_frame (Frame&) const
  { }
} NumberSoilHeatCapacity_syntax;

struct NumberSoilHeatConductivity : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const symbol my_dim = h->dimension (scope);
    const double my_val = h->value (scope);
    const double my_h = units.convert (my_dim, Units::cm (), my_val);
    const double Theta = horizon->hydraulic->Theta (my_h);
    return horizon->heat_conductivity (Theta, 0.0);
  }
  symbol dimension (const Scope&) const 
  { 
    static const symbol dim ("erg/cm/h/dg C");
    return dim;
  }

  // Create.
  NumberSoilHeatConductivity (const BlockModel& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilHeatConductivitySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSoilHeatConductivity (al); }
  NumberSoilHeatConductivitySyntax ()
    : DeclareModel (Number::component, "soil_heat_conductivity", "horizon",
	       "Find heat conductivity for a given pressure (h).")
  { }
  void load_frame (Frame&) const
  { }
} NumberSoilHeatConductivity_syntax;

struct NumberTensionByTheta : public Number
{
  const Units& units;

  // Parameters.
  const std::auto_ptr<Horizon> horizon;
  const std::auto_ptr<Number> Theta;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { Theta->tick (units, scope, msg); }
  bool missing (const Scope& scope) const 
  { return Theta->missing (scope) 
      || !units.can_convert (Theta->dimension (scope), Attribute::Fraction (),
                             Theta->value (scope)); }
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->h (units.convert (Theta->dimension (scope), 
                                                  Attribute::Fraction (), 
                                                  Theta->value (scope)));
  }
  symbol dimension (const Scope&) const 
  { return Units::cm (); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  {
    TREELOG_MODEL (msg);
    return Theta->initialize (units, scope, msg);
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    if (!Theta->check (units, scope, msg))
      return false;
    if (!units.can_convert (Theta->dimension (scope), Attribute::Fraction ()))
      {
        msg.error ("Cannot convert [" + Theta->dimension (scope) 
                   + "] to fraction for soil hydraulics");
        return false;
      }
    return true;
  }
  NumberTensionByTheta (const BlockModel& al)
    : Number (al),
      units (al.units ()),
      horizon (Librarian::build_item<Horizon> (al, "horizon")),
               Theta (Librarian::build_item<Number> (al, "Theta"))
  { horizon->initialize (al.flag ("top_soil"), 2, al.msg ()); }
};

static struct NumberTensionByThetaSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberTensionByTheta (al); }
  NumberTensionByThetaSyntax()
    : DeclareModel (Number::component, "soil_h", 
                    "Find pressure (h) for a given water content (Theta).")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("horizon", Horizon::component, "\
The soil horizon whose properties we want to examine.");
    frame.declare_object ("Theta", Number::component, "\
The water content we want to compare with.");
    frame.declare_boolean ("top_soil", Attribute::Const, "\
Set this to true for the A horizon.");
  }
} NumberTensionByTheta_syntax;

// number_soil.C ends here
