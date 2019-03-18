// drain_lateral.C --- Pipe drainage.
// 
// Copyright 2008 and 2009 Per Abrahamsen and KU.
//
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

#include "drain.h"
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
#include "draineqd.h"
#include "depth.h"

struct DrainLateral : public Drain
{
  // Parameters
  std::unique_ptr<const Draineqd> eq_depth; 
  const double L;               // Distance between pipes. [cm]
  const double rad;             // Inner radius of drain pipes. [cm]
  const double x;               // Distance to nearest pipe. [cm]
  const double pipe_position;   // Height pipes are placed above surface. [cm]
  std::unique_ptr<Depth> pipe_outlet; // Water level at pipe outlet. [cm]
  const double K_to_pipes_;     // Horizontal sat. conductivity. [cm h^-1]

  // Data.
  double pipe_level;            // Highest if pipe_position & pipe_outlet.
  double height;                // Groundwater table height above surface. [cm]
  double EqDrnFlow;
  double DrainFlow;             // Drain flow [cm/h]
  std::vector<double> S;        // Pipe drainage. [cm^3/cm^3/h]

  void set_pipe_level ()
  { pipe_level = std::max (pipe_position, pipe_outlet->operator()()); }

  void tick (const Time&, const Scope&, const Geometry& geo, const Soil& soil, 
             const SoilHeat& soil_heat, const Surface& surface, 
             SoilWater& soil_water, Treelog& msg);
  void output (Log&) const;
  
  // Helpers.
  double K_to_pipes (const unsigned int i, 
                     const Soil& soil, 
                     const SoilHeat& soil_heat) const;
  double EquilibriumDrainFlow (const Geometry& geo,
                               const Soil&, const SoilHeat&);
    
  // Create and Destroy.
  void initialize (const Time&, const Scope&, const Geometry&, Treelog&);
  bool check (const Scope&, Treelog&) const;
  DrainLateral (const BlockModel& al);
};

void 
DrainLateral::tick (const Time& time, const Scope& scope, 
                    const Geometry& geo, const Soil& soil, 
                    const SoilHeat& soil_heat, const Surface& surface, 
                    SoilWater& soil_water, 
                    Treelog& msg)
{
  // Current pipe level.
  pipe_outlet->tick (time, scope, msg);
  set_pipe_level ();

  const size_t cell_size = geo.cell_size ();

  // Empty source.
  fill (S.begin (), S.end (), 0.0);
  
  // Find groundwater height.
  const double old_height = height;
  height = surface.ponding_average () * 0.1;
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

  // Find sink term.
  EqDrnFlow = EquilibriumDrainFlow (geo, soil, soil_heat);   
  DrainFlow= geo.total_surface (S);
  soil_water.drain (S, msg);
}

void 
DrainLateral::output (Log& log) const
{
  output_variable (pipe_level, log);
  output_variable (height, log);
  output_variable (DrainFlow, log);
  output_variable (EqDrnFlow, log);
  output_variable (S, log);
}

double
DrainLateral::K_to_pipes (const unsigned int i, 
                            const Soil& soil, 
                            const SoilHeat& soil_heat) const
{
  if (K_to_pipes_ < 0)
    return soil.K (i, 0.0, 0.0, soil_heat.T (i))
      * soil.anisotropy_cell (i);
  return K_to_pipes_;
}


double
DrainLateral::EquilibriumDrainFlow (const Geometry& geo,
                                     const Soil& soil, 
                                     const SoilHeat& soil_heat)
{
  
  // If groundwater table is below pipes, there is no flow.
  if (height <= pipe_level)
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
        (z_top > pipe_level)
        ? geo.fraction_in_z_interval (i, z_top, pipe_level)
        : 0.0;
      const double f_below =
        (z_bottom < pipe_level)
        ? geo.fraction_in_z_interval (i, pipe_level, z_bottom)
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
  
  // There may be no nodes with pipe_level < z < height.
  if (iszero (Ha))
    return 0.0;

  // Make it 1D.  Only works for rectangular domain.
  const double soil_width = geo.right () - geo.left ();
  Ha /= soil_width;
  Hb /= soil_width;

  // Average conductivity.
  Ka /= (Ha*soil_width);
  daisy_assert (std::isnormal (Hb));
  Kb /= (Hb*soil_width);
  

  //--- Calculate equivalent depth ---
  double De;
  if (Hb<=0)
    De = 0.0;
  else 
    De = eq_depth->value (L, rad, Hb); 
  //----------------------------------


  //const double Flow = (4*Ka*Ha*Ha + 2*Kb*Hb*Ha) / (L*x - x*x); //original
  // const double Flow = (Ka*Ha*Ha + 2*Kb*Hb*Ha) / (L*x - x*x);  //corrected
  const double Flow_a = Ka*Ha*Ha / (L*x - x*x);   //Flow above drainpipes
  const double Flow_b = 2*Kb*De*Ha / (L*x - x*x); //Flow below drainpipes 
  const double Flow = Flow_a + Flow_b;

  

  //-------old-----
  //// Distribution of drain flow among numeric soil layers 
#if 0
  const double a = Flow / (Ka*Ha + Kb*Hb);
  for (size_t i = 0; i < cell_size; i++)
    {
      const double soil_bottom = geo.bottom ();
      const double f = geo.fraction_in_z_interval (i, height, soil_bottom);
      S[i] = f * a * K_to_pipes (i, soil, soil_heat);
    }
#endif
  //---------------


#if 1
  // New Distribution of drain flow among numeric soil layers  
  for (size_t i = 0; i < cell_size; i++)
    {   
      // Find fraction above and below pipes.
      double z_bottom = geo.cell_bottom (i);
      double z_top = std::min (geo.cell_top (i), height); 
      // double Deltaz = z_top - z_bottom;
   
      double f_above = 
        (z_top > pipe_level)
        ? geo.fraction_in_z_interval (i, z_top, pipe_level)
        : 0.0;
      double f_below =
        (z_bottom < pipe_level)
        ? geo.fraction_in_z_interval (i, pipe_level, z_bottom)
        : 0.0;
            
      S[i] = Flow_a * f_above *  K_to_pipes (i, soil, soil_heat) / (Ka*Ha);
      S[i] += Flow_b * f_below * K_to_pipes (i, soil, soil_heat) / (Kb*Hb);    
    }
#endif  

  daisy_assert (std::isfinite (Flow));
  return Flow;
 
}

//---------------------------------------------






void
DrainLateral::initialize (const Time& time, const Scope& scope, 
                          const Geometry& geo, Treelog& msg)
{
  pipe_outlet->initialize (time, scope, msg);

  if (pipe_outlet->check (scope, msg))
    set_pipe_level ();

  const size_t cell_size = geo.cell_size ();
  S.insert (S.end (), cell_size, 0.0);
}

bool 
DrainLateral::check (const Scope& scope, Treelog& msg) const
{ 
  bool ok = true;
  if (!pipe_outlet->check (scope, msg))
    ok = false;
  return ok;
}

DrainLateral::DrainLateral (const BlockModel& al)
  : Drain (al),
    eq_depth (Librarian::build_item<Draineqd> (al, "eq_depth")),
    L (al.number ("L")),
    rad (al.number ("rad")),
    x (al.number ("x", L / 2.0)),
    pipe_position (al.number ("pipe_position")),
    pipe_outlet (Librarian::build_item<Depth> (al, "pipe_outlet")),
    K_to_pipes_ (al.number ("K_to_pipes", -1.0)),
    pipe_level (pipe_position),
    height (al.number ("height", pipe_position))
{ }

static struct DrainLateralSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DrainLateral (al); }

  DrainLateralSyntax ()
    : DeclareModel (Drain::component, "lateral", "Pipe drainage.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "hooghoudt");
    frame.declare_object ("eq_depth", Draineqd::component,
                          Attribute::Const, Attribute::Singleton, "\
Model for calculating equivalent depth for drains.");
    frame.set ("eq_depth", "MolenWesseling");
    frame.declare ("L", "cm", Check::positive (), Attribute::Const,
                   "Distance between pipes.");
    frame.set ("L", 1800.0);
    frame.declare ("rad", "cm", Check::positive (), Attribute::Const,
                   "Inner radius of drain pipes.");
    frame.set ("rad", 3.5);
    frame.declare ("x", "cm", Check::positive (), Attribute::OptionalConst,
                   "Horizontal distance to nearest pipe.\n\
By default, this is 1/2 L.");
    frame.declare ("pipe_position", "cm", Check::negative (), Attribute::Const,
                   "Height pipes are placed in the soil (a negative number).");
    frame.set ("pipe_position", -110.0);
    frame.declare_object ("pipe_outlet", Depth::component,
                          Attribute::Const, Attribute::Singleton, "\
Water table in drain pipe outlet.\n\
\n\
By default this will be identical to `pipe_position', meaning free\n\
flow of water through drains. Specifying a lower water level will not\n\
affect the simulation. Specifying a higher water level is functionally\n\
equivalent to temporarily increasing the 'pipe_position', lowering the\n\
ability of the drain system to drain the soil.\n\
\n\
Currently, there is no posibility of water flowing from the pipe\n\
outlet to soil.");
    frame.set ("pipe_outlet", "deep");
    frame.declare ("K_to_pipes", "cm/h", Check::non_negative (), 
                   Attribute::OptionalConst,
                   "Horizontal conductivity in saturated soil.\n\
By default this is calculated from the horizontal conductivity and the\n\
anisotropy of the horizon.");
    frame.declare ("pipe_level", "cm", Check::non_positive (), 
                   Attribute::OptionalState,
                   "Current effective pipe position (a negative number).");
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
} DrainLateral_syntax;

// drain_Lateral.C ends here.
