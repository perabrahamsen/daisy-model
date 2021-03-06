// tertiary_instant.C --- Instant movement in vertical geometry.
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
#include "geometry_vert.h"
#include "plf.h"
#include "check.h"
#include "vcheck.h"
#include "librarian.h"
#include "block.h"
#include "frame.h"

struct TertiaryInstant : public Tertiary
{
  // Parameters.
  const PLF distribution;       // Where they end [cm ->]
  const double height_start;	// Height macropores start [cm]
  const double height_end;	// Height macropores end [cm]
  const double pressure_initiate;// Pressure needed to init pref.flow [cm]
  const double pressure_end;	 // Pressure after pref.flow has been init [cm]
  const double pond_max;	 // Pond height before activating pref.flow [mm]

  // Identity.
  bool has_macropores ()
  { return true; }

  // Simulation.
  void tick_water (const Geometry&, const Soil&, const SoilWater&, 
                   const SoilHeat&, const double dt,
                   Surface& surface,
                   std::vector<double>& S_drain,
                   std::vector<double>& S_matrix,
                   std::vector<double>& q_tertiary, 
                   Treelog& msg);
  void remove_solute (const symbol)
  { }
  double total_solute (const Geometry&, const symbol) const //[g/m^2]
  { return 0.0; }
  void solute (const Geometry&, const SoilWater&, 
               const std::map<size_t, double>& J_tertiary,
               const double /* dt */,
               Chemical&, Treelog&)
  { }
  void output (Log&) const;
  
  // Create and Destroy.
public:
  bool initialize (const Geometry&, const Soil&, const Scope& parent_scope, 
                   const double pipe_position, Treelog& msg);
  bool check (const Geometry&, Treelog& msg) const;
  TertiaryInstant (Block& al);
};


void
TertiaryInstant::tick_water (const Geometry& geometry, const Soil& soil,
                             const SoilWater& soil_water, const SoilHeat&,
                             const double dt,
                             Surface& surface,
                             std::vector<double>& S_drain,
                             std::vector<double>& S_matrix,
                             std::vector<double>& q_tertiary, 
                             Treelog& msg)
{
#if 0
  const GeometryVert& geo = dynamic_cast<const GeometryVert&> (geometry);
  const size_t column_size = geo.column_size ();
  const size_t row_size = geo.row_size ();

  for (size_tt col = 0; col < column_size; col++)
    {
      // row_start and row_end should be set in initialize
      
      
      
      
    }
#endif
}

void 
TertiaryInstant::output (Log&) const
{ }

bool 
TertiaryInstant::initialize (const Geometry& geo, const Soil&, 
                             const Scope& scope, const double pipe_position, 
                             Treelog& msg)
{ 
  bool ok = true;
  // TODO
  return ok;
}

bool 
TertiaryInstant::check (const Geometry& geo, Treelog& msg) const
{
  
  bool ok = true;
  if (!dynamic_cast<const GeometryVert*> (&geo))
    {
      msg.error ("\
This tertiary water transport model only works with vertical grid geometries");
      ok = false;
    }

  return ok;
}

TertiaryInstant::TertiaryInstant (Block& al)
  : Tertiary (al),
    distribution (al.plf ("distribution")),
    height_start (al.check ("height_start") 
                  ? al.number ("height_start")
                  : distribution.x (distribution.size () - 1)),
    height_end (al.check ("height_end")
                ? al.number ("height_end")
                : distribution.x (0)),
    pressure_initiate (al.number ("pressure_initiate")),
    pressure_end (al.number ("pressure_end")),
    pond_max (al.number ("pond_max"))
{ }

static struct TertiaryInstantSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new TertiaryInstant (al); }

  TertiaryInstantSyntax ()
    : DeclareModel (Tertiary::component, "instant", "\
The area between 'height_start' and 'height_end' contains macropores,\n\
which are initiated when the water potential reach 'pressure_initiate',\n\
and then immediately emptied down to 'pressure_end'.  The water entering\n\
the macropore is distributed in soil below as a source term, according\n\
to the 'distribution' parameter.")
  { }
  void load_frame (Frame& frame) const
  { 

    frame.add ("height_start", "cm", Check::non_positive (), 
                Syntax::OptionalConst, 
                "Macropores starts at this depth (a negative number).\n\
If not specified, use the last point in 'distribution'.");
    frame.add ("height_end", "cm", Check::non_positive (),
                Syntax::OptionalConst, 
                "Macropores ends at this depth (a negative number).\n\
If not specified, use the first point in 'distribution'.");
    frame.add ("distribution", "cm", Syntax::Fraction (), Syntax::Const, "\
Distribution of macropore end points as a function of height.\n\
The function should start with '1' at 'height_end', and then decrease to\n\
'0' at 'height_start'.  It can be constant, but may never increase.\n\
The value indicates the fraction of macropores which ends at the given\n\
where all macropores is assumed to start at the top.");
    static VCheck::StartValue start (1.0);
    static VCheck::EndValue end (0.0);
    static VCheck::FixedPoint fixpoint (0.0, 0.0);
    static VCheck::All distcheck (start, end, fixpoint, 
                                  VCheck::non_increasing ());
    frame.add_check ("distribution", distcheck);

    frame.add ("pressure_initiate", "cm", Syntax::Const, 
                "Pressure needed to initiate biopore flow.");
    frame.add ("pressure_initiate", -3.0);
    frame.add ("pressure_end", "cm", Syntax::Const, 
                "Pressure after biopore flow has been initiated.");
    frame.add ("pressure_end", -30.0);
    frame.add ("pond_max", "mm", Check::non_negative (), Syntax::Const, "\
Maximum height of ponding before spilling into instant.\n\
After macropores are activated pond will have this height.");
    frame.add ("pond_max", 0.5);

  }
} TertiaryInstant_syntax;

// tertiary_instant.C ends here.
