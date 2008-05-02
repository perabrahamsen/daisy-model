// groundwater_pipe.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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
#include "block.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "depth.h"
#include "treelog.h"
#include "mathlib.h"
#include "check.h"
#include "librarian.h"
#include <memory>
#include <sstream>

class GroundwaterPipe : public Groundwater
{
  // Parameters
  const double L;		// Distance between pipes. [cm]
  const double x;		// Distance to nearest pipe. [cm]
  const double pipe_position;	// Height pipes are placed above surface. [cm]
  const double K_to_pipes_;	// Horizontal sat. conductivity. [cm h^-1]
  const double K_aquitard_;	// Conductivity of the aquitard. [cm h^-1]
  const double Z_aquitard_;	// Vertical length of the aquitard. [cm]
  std::auto_ptr<Depth> pressure_table; // Virtual groundwater height. [cm]
  double original_bottom;       // Bottom of soil above aquitard. [cm]

  // Accessors.
  double Z_aquitard () const
  { return Z_aquitard_; }
  double K_aquitard () const
  { return K_aquitard_; }
  void set_original_bottom (double value)
  { original_bottom = value; }

  // Data.
  double height;		// Groundwater table height above surface. [cm]
  double EqDrnFlow;
  double DrainFlow;		// Drain flow [cm/h]
  std::vector<double> S;        // Pipe drainage. [cm^3/cm^3/h]
  double deep_percolation;	// [cm^3/cm^2/h]
  double h_aquifer;          // Pressure potential in the aquifer [cm]


  // Groundwater.
public:
  bottom_t bottom_type () const
  { return forced_flux; }
  double q_bottom (size_t) const
  { return -deep_percolation; }

  // Identity
  bool is_pipe () const
  { return true; }
  double pipe_height () const
  { return pipe_position; }

  // Simulation.
public:
  void tick (const Geometry& geo,
             const Soil&, SoilWater&, double,
	     const SoilHeat&, const Time&, const Scope&, Treelog&);
  void output (Log& log) const;

private:
  void set_h_aquifer (const Geometry& geo)
  {
    const double aquitart_bottom = original_bottom - Z_aquitard_;
    h_aquifer = pressure_table->operator()() - aquitart_bottom;
  }
  double DeepPercolation ();
  double K_to_pipes (const unsigned int i, 
                     const Soil& soil, 
                     const SoilHeat& soil_heat) const;
  double EquilibriumDrainFlow (const Geometry& geo,
                               const Soil&, const SoilHeat&);

  // Accessors.
  double table () const
  { return height; }

  // Create and Destroy.
public:
  void initialize (const Geometry&, const Time&, const Scope&, Treelog&);
  bool check (const Geometry&, const Scope&, Treelog&) const;
  GroundwaterPipe (Block&);
  ~GroundwaterPipe ()
  { }
};

void 
GroundwaterPipe::tick (const Geometry& geo,
                       const Soil& soil, SoilWater& soil_water, 
		       const double h_surface,
		       const SoilHeat& soil_heat, const Time& time,
                       const Scope& scope, Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  // Empty source.
  fill (S.begin (), S.end (), 0.0);
  
  // Virtual pressure table.
  pressure_table->tick (time, scope, msg);
  set_h_aquifer (geo);

  // Find groundwater height.
#if 1                           // This works with any geometry, 
                                // but is less precise.
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
      const double z = geo.cell_z (i);
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
#else  // This only works with Geometry1D.
  height = h_surface;
  for (int i = size - 1; i >= 0; i--)
    {
      const double h = soil_water.h (i);
      if (h < 0)
	{
	  const double zplus = geo.zplus (i);
	  const double z = (i == 0) ? 0.0 : geo.zplus (i-1);
	  const double zx = z - zplus; 
	  if (h + zx > 0)
	    // Groundwater in this cell.
	    height = zplus + h + zx;
	  else
	    // Groundwater between cells.
	    height = zplus;
	  break;
	}
    }
#endif
  // Find sink term.
  EqDrnFlow = EquilibriumDrainFlow (geo, soil, soil_heat);
  DrainFlow= geo.total_surface (S);
  soil_water.drain (S);

  // Find deep percolation.
  deep_percolation = DeepPercolation ();
}


double
GroundwaterPipe::DeepPercolation ()
{
  const double hb = height - original_bottom;
#if 0
  std::ostringstream tmp;
  tmp << "height = " << height << ", bottom = " << original_bottom
      << ", hb = " << hb << ", h_aquifer = " << h_aquifer << "\n"
      << "Z_aquitard = " << Z_aquitard_ << ", deep = " 
      << K_aquitard_ * (1.0 + (hb - h_aquifer) / Z_aquitard_)
      << "deep = " 
      << K_aquitard_ * (hb - (h_aquifer - Z_aquitard_)) / Z_aquitard_;
  Assertion::message (tmp.str ());
#endif
  if (hb > 0)
    return K_aquitard_ * (hb - (h_aquifer - Z_aquitard_)) / Z_aquitard_;
  else
    return 0;
}

double
GroundwaterPipe::K_to_pipes (const unsigned int i, 
                             const Soil& soil, 
                             const SoilHeat& soil_heat) const
{
  if (K_to_pipes_ < 0)
    return soil.K (i, 0.0, 0.0, soil_heat.T (i))
      * soil.anisotropy (i);
  return K_to_pipes_;
}

double
GroundwaterPipe::EquilibriumDrainFlow (const Geometry& geo,
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
      const double z = geo.cell_z (i);

      // No contribution from cells above the groundwater table.
      if (z >= height)
        continue;

      const double volume = geo.cell_volume (i);

      if (z >= pipe_position)
        {
          Ha += volume;
          Ka += volume * K_to_pipes (i, soil, soil_heat);
        }
      else
        {
          Hb += volume;
          Kb += volume * K_to_pipes (i, soil, soil_heat);
        }
    }

  // There may be no nodes with pipe_position < z < height.
  if (iszero (Ha))
    return 0.0;

  // Average conductivity.
  Ka /= Ha;
  daisy_assert (std::isnormal (Hb));
  Kb /= Hb;
  
  const double Flow = (4*Ka*Ha*Ha + 2*Kb*Hb*Ha) / (L*x - x*x);

  // Distribution of drain flow among numeric soil layers
  const double a = Flow / (Ka*Ha + Kb*Hb);
  for (size_t i = 0; i < cell_size; i++)
    if (geo.cell_z (i) < height)
      S[i] = a * K_to_pipes (i, soil, soil_heat);

  daisy_assert (std::isfinite (Flow));
  return Flow;
}

void
GroundwaterPipe::output (Log& log) const
{
  Groundwater::output (log);
  output_variable (DrainFlow, log);
  output_variable (EqDrnFlow, log);
  output_value (deep_percolation, "DeepPercolation", log);
  output_variable (S, log);
  output_variable (h_aquifer, log);
}

void
GroundwaterPipe::initialize (const Geometry& geo, const Time& time,
			     const Scope& scope, Treelog& msg)
{
  const int size = geo.cell_size ();
  double largest = 0.0;
  for (unsigned int i = 0; i < size; i++)
    if (geo.cell_volume (i) > largest)
      largest = geo.cell_volume (i);
  if (largest > 10.0)
    {
      Treelog::Open nest (msg, "Groundwater pipe");
      std::ostringstream tmp;
      tmp << "WARNING: drained soil needs soil intervals < 10.0 cm; "
	  << "largest is " << largest << "";
      msg.warning (tmp.str ());
    }

  S.insert (S.end (), size, 0.0);

  if (!pressure_table.get ())
    {
      // GCC 2.95 need the extra variable for the assignment.
      std::auto_ptr<Depth> depth (Depth::create ((original_bottom - Z_aquitard_)
						 + h_aquifer));
      pressure_table = depth;
    }
  pressure_table->initialize (msg);
  // Pressure below aquitard.
  if (pressure_table->check (scope, msg))
    set_h_aquifer (geo);
  else
    pressure_table.reset (NULL);
}

bool 
GroundwaterPipe::check (const Geometry& geo, const Scope& scope,
			Treelog& msg) const
{
  bool ok = true;
  if (!pressure_table.get ())
    {
      ok = false;
      msg.error ("No pressure table");
    }
  else if (!pressure_table->check (scope, msg))
    ok = false;
  
  // Check that we have a volume below the pipes.
  for (size_t i = 0; i < geo.cell_size (); i++)
    if (geo.cell_z (i) < pipe_position - 1e-10)
      goto found_cell_center_below_pipe;

  msg.error ("Insufficient soil defined below the pipe drains");
  ok = false;
 found_cell_center_below_pipe:
  return ok;
}
 
GroundwaterPipe::GroundwaterPipe (Block& al)
  : Groundwater (al),
    L (al.number ("L")),
    x (al.number ("x", L / 2.0)),
    pipe_position (al.number ("pipe_position")),
    K_to_pipes_ (al.number ("K_to_pipes", -1.0)),
    K_aquitard_ (al.number ("K_aquitard")),
    Z_aquitard_ (al.number ("Z_aquitard")),
    height (al.number ("height", pipe_position)),
      h_aquifer (al.number ("h_aquifer", Z_aquitard_))
{
  if (al.check ("pressure_table"))
    {
      // GCC 2.95 needs the extra variable for the asignment.
      std::auto_ptr<Depth> depth
	(Librarian::build_item<Depth> (al, "pressure_table"));
      pressure_table = depth;
    }
}

static struct GroundwaterPipeSyntax
{
  static Model& make (Block& al)
    {
      return *new GroundwaterPipe (al);
    }
  GroundwaterPipeSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Groundwater for pipe (tile) drained soil.\n\
If you specify this groundwater model, and does not specify the 'zplus' Soil\n\
discretization parameter, an extra aquitard soil horizon approximately a third\n\
of the size of 'Z_aquitart' will be added.  This will allow the grounwater\n\
level to sink into the aquitart.  The model cannot handle groundwater levels\n\
below the last cell, or above the soil surface.");
      // We define our own "height", so don't load from here.
      // Groundwater::load_syntax (syntax, alist);

      syntax.add ("L", "cm", Check::positive (), Syntax::Const,
		  "Distance between pipes.");
      alist.add ("L", 1800.0);
      syntax.add ("x", "cm", Check::positive (), Syntax::OptionalConst,
		  "Horizontal distance to nearest pipe.\n\
By default, this is 1/2 L.");
      syntax.add ("pipe_position", "cm", Check::negative (), Syntax::Const,
		  "Height pipes are placed in the soil (a negative number).");
      alist.add ("pipe_position", -110.0);
      syntax.add ("K_to_pipes", "cm/h", Check::non_negative (), 
                  Syntax::OptionalConst,
		  "Horizontal conductivity in saturated soil.\n\
By default this is calculated from the horizontal conductivity and the\n\
anisotropy of the horizon.");
      syntax.add ("K_aquitard", "cm/h", Check::non_negative (), Syntax::Const,
		  "Conductivity of the aquitard.");
      alist.add ("K_aquitard", 1e-3);
      syntax.add ("Z_aquitard", "cm", Check::positive (), Syntax::Const,
		  "Thickness of the aquitard.\n\
The aquitard begins below the bottommost soil horizon.");
      alist.add ("Z_aquitard", 200.0);
      syntax.add ("h_aquifer", "cm", Check::positive (), Syntax::OptionalState,
		  "Pressure potential in the aquifer below the aquitard.\n\
By default. this is Z_aquitard.\n\
You can alternatively specify the pressure as a virtual groundwater level.\n\
See 'pressure_table'.");
      syntax.add_object ("pressure_table", Depth::component,
                         Syntax::OptionalConst, Syntax::Singleton, "\
Height of groundwater the corresponds to the pressure in the aquifer.  \n\
\n\
If you drilled a well down to the aquifer, this is number what the\n\
water level in the well would be as height above ground (a negative\n\
number).  This is different from the actual groundwater table, because\n\
the aquitart block the water, and the pipes lead the water away.\n\
You can alternatively specify the pressure directly, with 'h_aquifer'.");
      syntax.add ("height", "cm", Check::non_positive (), 
		  Syntax::OptionalState,
		  "Current groundwater level (a negative number).");
      syntax.add ("DrainFlow", "cm/h", Syntax::LogOnly,
		  "Drain flow to pipes.");
      syntax.add ("EqDrnFlow", "cm/h", Syntax::LogOnly,
		  "Equilibrium drain flow to pipes.");
      syntax.add ("deficit", "cm", Syntax::LogOnly,
		  "Deficit.");
      syntax.add ("DeepPercolation", "cm/h", Syntax::LogOnly,
		  "Deep percolation to aquifer.");
      syntax.add ("S", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		  "Pipe drainage.");
      Librarian::add_type (Groundwater::component, "pipe", alist, syntax, &make);
    }
} GroundwaterPipe_syntax;


