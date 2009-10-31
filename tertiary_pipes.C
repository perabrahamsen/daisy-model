// tertiary_pipes.C --- Pipe drainage.
// 
// Copyright 2008 and 2009 Per Abrahamsen and KU.
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
#include "tertsmall.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "surface.h"
#include "librarian.h"
#include "block_model.h"
#include "frame.h"
#include "mathlib.h"
#include "log.h"
#include "check.h"

struct TertiaryPipes : public Tertiary
{
  // Parameters
  const double L;               // Distance between pipes. [cm]
  const double x;               // Distance to nearest pipe. [cm]
  const double pipe_position;   // Height pipes are placed above surface. [cm]
  const double K_to_pipes_;     // Horizontal sat. conductivity. [cm h^-1]

  // Data.
  double height;                // Groundwater table height above surface. [cm]
  double EqDrnFlow;
  double DrainFlow;             // Drain flow [cm/h]
  std::vector<double> S;        // Pipe drainage. [cm^3/cm^3/h]

  // Identity.
  bool has_macropores ()
  { return false; }

  // Simulation.
  void deactivate (const int)
  { }
  void tick (const Units&, const Geometry& geo, const Soil& soil, 
             const SoilHeat& soil_heat, const double dt, 
             SoilWater& soil_water, Surface& surface, Treelog& msg);
  Tertsmall& implicit ()
  { return Tertsmall::none (); }
  void solute (const Geometry&, const SoilWater&, 
               const std::map<size_t, double>&,
               const double, Chemical&, Treelog&)
  { }
  void output (Log&) const;
  
  // Helpers.
  double K_to_pipes (const unsigned int i, 
                     const Soil& soil, 
                     const SoilHeat& soil_heat) const;
  double EquilibriumDrainFlow (const Geometry& geo,
                               const Soil&, const SoilHeat&);

  // Create and Destroy.
public:
  bool initialize (const Units&, 
                   const Geometry&, const Soil&, const Scope& parent_scope, 
                   const Groundwater&, Treelog& msg);
  bool check (const Geometry&, Treelog&) const
  { return true; }
  TertiaryPipes (const BlockModel& al);
};

void 
TertiaryPipes::tick (const Units&, const Geometry& geo, const Soil& soil, 
                     const SoilHeat& soil_heat, const double, 
                     SoilWater& soil_water, Surface& surface,
                     Treelog&)
{
  const size_t cell_size = geo.cell_size ();

  // Empty source.
  fill (S.begin (), S.end (), 0.0);
  
  // Find groundwater height.
  const double h_surface = surface.ponding () * 0.1;

  const double old_height = height;
  height = 1.0;
  double lowest = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      // Look for an unsaturated node.
      const double h = soil_water.h (i);
      if (h >= 0)
        continue;
      // as low as possible.
      const double z = geo.cell_top (i);
      if (approximate (z, lowest))
        {
          const double new_height = z + h;
 
          // Use closest value to old height;
          if (height >= 0.0
              || (std::fabs (new_height - old_height)
                  < std::fabs (height - old_height)))
            height = new_height;
        }
      else if (z < lowest)
        {
          lowest = z;
          height = z + h;
        }
    }    
  if (height > 0.0)
    height = h_surface;

  // Find sink term.
  EqDrnFlow = EquilibriumDrainFlow (geo, soil, soil_heat);
  DrainFlow= geo.total_surface (S);
  soil_water.drain (S);
}

void 
TertiaryPipes::output (Log& log) const
{
  output_variable (height, log);
  output_variable (DrainFlow, log);
  output_variable (EqDrnFlow, log);
  output_variable (S, log);
}

double
TertiaryPipes::K_to_pipes (const unsigned int i, 
                           const Soil& soil, 
                           const SoilHeat& soil_heat) const
{
  if (K_to_pipes_ < 0)
    return soil.K (i, 0.0, 0.0, soil_heat.T (i))
      * soil.anisotropy_cell (i);
  return K_to_pipes_;
}

double
TertiaryPipes::EquilibriumDrainFlow (const Geometry& geo,
                                     const Soil& soil, 
                                     const SoilHeat& soil_heat)
{
  // If groundwater table is below pipes, there is no flow.
  if (height <= pipe_position)
    return 0.0;

  const size_t cell_size = geo.cell_size ();

  double Ha = 0.0;            // Volume above pipes.
  double Ka = 0.0;            // Conductivity above pipes.
  double Hb = 0.0;            // Volume below pipes.
  double Kb = 0.0;            // Conductivity below pipes.
    
  for (size_t i = 0; i < cell_size; i++)
    {
      const double z_bottom = geo.cell_bottom (i);

      // No contribution from cells wholy above the groundwater table.
      if (z_bottom >= height)
        continue;

      // Do not count part of cell above groundwater level.
      const double z_top = std::min (geo.cell_top (i), height);
      
      // Ignore insignificant intervals.
      if (approximate (z_top, z_bottom))
        continue;

      // Sanity check.
      daisy_assert (z_top > z_bottom);

      // Find fraction above and below pipes.
      const double f_above = 
        (z_top > pipe_position)
        ? geo.fraction_in_z_interval (i, z_top, pipe_position)
        : 0.0;
      const double f_below =
        (z_bottom < pipe_position)
        ? geo.fraction_in_z_interval (i, pipe_position, z_bottom)
        : 0.0;

#if 1
      const double volume = geo.cell_volume (i);
#else
      const double volume = geo.cell_bottom (i) - geo.cell_top (i);
#endif
      const double K = K_to_pipes (i, soil, soil_heat);
      Ha += f_above * volume;
      Ka += f_above * volume * K;
      Hb += f_below * volume;
      Kb += f_below * volume * K;
    }
  
  // There may be no nodes with pipe_position < z < height.
  if (iszero (Ha))
    return 0.0;

  // Make it 1D.  Only works for rectangular domain.
  const double soil_width = geo.right () - geo.left ();
  Ha /= soil_width;
  Hb /= soil_width;

  // Average conductivity.
  Ka /= Ha;
  daisy_assert (std::isnormal (Hb));
  Kb /= Hb;
  
  const double Flow = (4*Ka*Ha*Ha + 2*Kb*Hb*Ha) / (L*x - x*x);

  // Distribution of drain flow among numeric soil layers
  const double soil_bottom = geo.bottom ();
  const double a = Flow / (Ka*Ha + Kb*Hb);
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_z_interval (i, height, soil_bottom);
      S[i] = f * a * K_to_pipes (i, soil, soil_heat);
    }
  daisy_assert (std::isfinite (Flow));
  return Flow;
}

bool 
TertiaryPipes::initialize (const Units&,
                           const Geometry& geo, const Soil&,
                           const Scope&, const Groundwater&, 
                           Treelog&)
{
  const size_t cell_size = geo.cell_size ();
  S.insert (S.end (), cell_size, 0.0);
  return true;
}

TertiaryPipes::TertiaryPipes (const BlockModel& al)
  : Tertiary (al),
    L (al.number ("L")),
    x (al.number ("x", L / 2.0)),
    pipe_position (al.number ("pipe_position")),
    K_to_pipes_ (al.number ("K_to_pipes", -1.0)),
    height (al.number ("height", pipe_position))
{ }

static struct TertiaryPipesSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new TertiaryPipes (al); }

  TertiaryPipesSyntax ()
    : DeclareModel (Tertiary::component, "pipes", "Pipe drainage.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("L", "cm", Check::positive (), Attribute::Const,
                   "Distance between pipes.");
    frame.set ("L", 1800.0);
    frame.declare ("x", "cm", Check::positive (), Attribute::OptionalConst,
                   "Horizontal distance to nearest pipe.\n\
By default, this is 1/2 L.");
    frame.declare ("pipe_position", "cm", Check::negative (), Attribute::Const,
                   "Height pipes are placed in the soil (a negative number).");
    frame.set ("pipe_position", -110.0);
    frame.declare ("K_to_pipes", "cm/h", Check::non_negative (), 
                   Attribute::OptionalConst,
                   "Horizontal conductivity in saturated soil.\n\
By default this is calculated from the horizontal conductivity and the\n\
anisotropy of the horizon.");
    frame.declare ("height", "cm", Check::non_positive (), 
                   Attribute::OptionalState,
                   "Current groundwater level (a negative number).");
    frame.declare ("DrainFlow", "cm/h", Attribute::LogOnly,
                   "Drain flow to pipes.");
    frame.declare ("EqDrnFlow", "cm/h", Attribute::LogOnly,
                   "Equilibrium drain flow to pipes.");
    frame.declare ("S", "cm^3/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Pipe drainage.");
  }
} TertiaryPipes_syntax;

// tertiary_pipes.C ends here.
