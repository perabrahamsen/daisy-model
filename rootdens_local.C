// rootdens_local.C -- Local model for root growth.
// 
// Copyright 2012, 2023 Per Abrahamsen and KU.
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

#include "rootdens.h"
#include "block_model.h"
#include "geometry.h"
#include "log.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "iterative.h"
#include "treelog.h"
#include "frame_model.h"
#include "metalib.h"
#include "library.h"
#include "plf.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "abiotic.h"
#include "function.h"
#include <sstream>
#include <algorithm>

struct RootdensLocal : public Rootdens
{
  // Parameters.
  const double row_position;	// Horizontal position of row crops. [cm]
  const double row_distance;	// Distance betweeen rows. [cm]
  const double DensRtTip;	// Root density at (pot) pen. depth. [cm/cm^3]
  const PLF depth_factor;	// Depth affect on root growth. [cm] -> []
  const double neighbor_effect; // Effect on neighbor cells. [cm/d]
  const double max_internal_growth_rate; // [d^-1]
  const double h_threshold;     // Root drowns above this pressure [cm]
  const PLF death_rate;		// Death rate as function of time [h] -> [h^-1]
  const std::unique_ptr<Function> death_T_factor; // fT [dgC] -> []
  
  // State.
  bool emerging;		// True until root mass > DensRtTip * zone
  double LastDepth;		// [cm]
  double LastWidth;		// [cm]
  std::vector<double> flooded;	  // Time flooded (temperature modified) [h]

  // Log.
  std::vector<double> E;	  // Expansion [cm/cm^3/d]
  double expansion_volume;	  // Expansion volume [cm^3]
  double E_tot;			  // Total expansion root length [cm/d]
  double A_tot;			  // Total expansion area [cm^2]
  std::vector<double> I;	  // Internal adjustment [cm/cm^3/d]
  double I_tot;			  // Total internal adj. root length [cm/d]
  std::vector<double> D;	  // Death [cm/cm^3/h]
  double D_tot;			  // Total death [cm/h]

  // Utility.
  double find_row (const double x /* [cm] */) const; // [cm]
  double find_length (const double WRoot /* [g DM/m^2] */) const; // [cm/cm^2]

  // Simulation.
  void expansion (const Geometry& geo,
		  const double SoilDepth,   // [cm]
		  const double delta_depth, // [cm/d]
		  const double delta_width, // [cm/d]
		  const double delta_root,  // [cm/d]
		  std::vector<double>& L);
  void internal_growth (const Geometry& geo,
			const double SoilDepth,   // [cm]
			const double delta_root,  // [cm/d]
			std::vector<double>& L);   // [cm/cm^3]
  void emergence (const Geometry& geo, 
		  const double SoilDepth /* [cm] */, 
		  const double CropDepth /* [cm] */,
		  const double CropWidth /* [cm] */,
		  std::vector<double>& Density  /* [cm/cm^3] */,
		  Treelog& msg);
  
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth, double CropWidth,
		    double WRoot, double DS, std::vector<double>& Density,
		    Treelog&);
  void tick (const Geometry& geo,
	     const SoilHeat& soil_heat, const SoilWater& soil_water, 
	     std::vector<double>& L, const double dt, Treelog& msg);
  void output (Log& log) const;

  const std::vector<double>& dynamic_root_death () const
  { return D; }
  double dynamic_root_death_DM () const 	// [g DM/h]
  { return 0.01 /* [m/cm] */
      * D_tot /* [cm/h] */
      / SpRtLength /* [m/g DM] */; }

  // Create.
  void initialize (const Geometry&, 
                   double row_width, double row_pos, Treelog&);
  explicit RootdensLocal (const BlockModel&);
};

double // [cm/cm^2]
RootdensLocal::find_length (const double WRoot /* [g DM/m^2] */) const 
{

  const double cm_per_m = 100.0;       // [cm/m]
  const double m_per_cm = 0.01;       // [m/cm]
  const double root_length            // [cm/cm^2]
    = WRoot                           // [g/m^2]
    * m_per_cm * m_per_cm             // [m^2/cm^2]
    * SpRtLength                      // [m/g]
    * cm_per_m;                       // [cm/m]
  return root_length;                 // [cm/cm^2]
}

double 
RootdensLocal::find_row (const double x /* [cm] */) const // [cm]
{
  if (row_distance < 0.0)
    // Not a row crop.
    return 0.0;

  // Relative to row.
  double p = x - row_position;
  p -= std::floor (p / row_distance) * row_distance;
  p = std::min (p, row_distance - p);
  daisy_assert (p >= 0 && p <= 0.50001 * row_distance);
  return p;
}

void
RootdensLocal::expansion (const Geometry& geo,
			  const double SoilDepth,   // [cm]
			  const double delta_depth, // [cm/d]
			  const double delta_width, // [cm/d]
			  const double delta_root,  // [cm/d]
			  std::vector<double>& L)   // [cm/cm^3]
{
  const size_t cell_size = geo.cell_size ();
  daisy_assert (L.size () == cell_size);

  // Clear old values.
  E = std::vector<double> (cell_size, 0.0);
  expansion_volume = 0.0;	// [cm^3]
  E_tot = 0.0;		// [cm/d]
  A_tot = 0.0;		// [cm^2]

  if (delta_root <= 0.0)
    // No expansion if roots are shrinking.
    return;
  
  const double dt = 1.0;	// Daily timestep [d] 
    
  for (size_t c = 0; c < cell_size; ++c)
    {
      if (geo.cell_z (c) < -SoilDepth)
	// Don't go below this depth.
	continue;
      
      const double fE = 1.0;		      // Expansion factor []
	
      if (L[c] > DensRtTip)
	// Ignore existing root zone
	continue;

      // Other cells will get expansion from their neighbors.
      const double V = geo.cell_volume (c); // [cm^3]
      double A_con = 0.0;	// Connecting area with RZ cells [cm^2]
      double V_con = 0.0;	// Connecting volume with RZ cells [cm^3]
	  
      for (size_t e: geo.cell_edges (c))
	{
	  if (!geo.edge_is_internal (e))
	    continue;

	  const int o = geo.edge_other (e, c);
	  daisy_assert (geo.cell_is_internal (o));
	  daisy_assert (o >= 0 && o < cell_size);

	  if (L[o] < DensRtTip)
	    // Only cells above DensRtTip expand to neighbors.
	    continue;

	  const double A = geo.edge_area (e); // [cm^2]
	  A_con += A;

	  const double dx = delta_width * geo.edge_cos_angle (e); // [cm/d]
	  const double dz = delta_depth * geo.edge_sin_angle (e); // [cm/d]
	  const double dl = std::sqrt (dx*dx + dz*dz); // Pythagoras [cm/d].
	  V_con += A * dl * dt;			       // [cm^3]
	}

      // The expansion from neighbors + internal expansion
      // can never be larger than the cell.
      const double V_exp = std::min (V, V_con);
      const double E_length = fE * DensRtTip * V_exp;
      E[c] = E_length / V / dt;
      E_tot += E_length;
      expansion_volume += V_exp;
      A_tot += A_con;
    }

  daisy_approximate (geo.total_soil (E), E_tot);
  const double factor = delta_root / E_tot;
  if (factor > 1.0)
    // Sufficient roots to expand with with DensRtTip.
    return;

  // Shrink for available root length.
  for (int c = 0; c < cell_size; c++)
    E[c] *= factor;
  daisy_approximate (geo.total_soil (E), delta_root);
  E_tot = delta_root;
}

void
RootdensLocal::internal_growth (const Geometry& geo,
				const double SoilDepth,   // [cm]
				const double delta_root,  // [cm/d]
				std::vector<double>& L)   // [cm/cm^3]
{
  const double dt = 1.0;		  // Daisy timestep [d]
  const double dl = neighbor_effect * dt;	  // [cm/d]

  // Clear old values.
  std::fill (I.begin (), I.end (), 0.0);

  daisy_assert (delta_root > 0.0);
  
  const size_t cell_size = geo.cell_size ();
  daisy_assert (L.size () == cell_size);
  daisy_assert (I.size () == cell_size);

  for (size_t c = 0; c < cell_size; ++c)
    {
      if (iszero (L[c]))
	// Ignore cells outside root zone.
	continue;

      // Other cells will get expansion from their neighbors.
      const double V = geo.cell_volume (c); // [cm^3]
      double extended_V = V / dt;		    // [cm^3/d]
      
      // Ampount of roots in cell and neighbors.
      double extended_cell_roots = V * L[c]; // [cm]

	for (size_t e: geo.cell_edges (c))
	{
	  if (!geo.edge_is_internal (e))
	    continue;

	  const int o = geo.edge_other (e, c);
	  daisy_assert (geo.cell_is_internal (o));
	  daisy_assert (o >= 0 && o < cell_size);

	  if (iszero (L[o]))
	    continue;
	  
	  const double A = geo.edge_area (e); // [cm^2]
	  const double V_con = A * dl * dt;   // [cm^3]
	  
	  extended_cell_roots += A * dl * L[o]; // [cm^3/d]
	  extended_V += V_con;
	}
	// Put all the roots in the cell.
	I[c] = extended_cell_roots / extended_V;
    }

  // Scale to available roots.
  const double I_tot = geo.total_soil (I);
  const double factor = delta_root / I_tot;
  for (int c = 0; c < cell_size; c++)
    I[c] *= factor;
  
  daisy_approximate (geo.total_soil (I), delta_root);

  for (int c = 0; c < cell_size; c++)
    I[c] = std::min (I[c], L[c] * max_internal_growth_rate);
}

void
RootdensLocal::emergence (const Geometry& geo, 
			  const double SoilDepth /* [cm] */, 
			  const double CropDepth /* [cm] */,
			  const double CropWidth /* [cm] */,
			  std::vector<double>& Density  /* [cm/cm^3] */,
			  Treelog& msg)
{
  TREELOG_MODEL (msg);
  const size_t cell_size = geo.cell_size ();

  // Emergence volume.
  double volume = 0.0;	// [cm^3]
  for (size_t c = 0; c < cell_size; c++)
    {
      // Cell position relative to root.
      const double z = -geo.cell_z (c); // [cm]
      const double x = find_row (geo.cell_x (c)); // [cm]
      if (x/CropWidth + z/CropDepth < 1.0
	  && z < SoilDepth)
	{
	  const double V =  geo.cell_volume (c); // [cm^3]
	  volume += V;
	}
    }
  if (!(volume > 0.0))
    {			// Poor discretization?
      msg.error ("No root volume at emergence");
      return;
    }

  // Fill volume
  for (size_t c = 0; c < cell_size; c++)
    {
      // Cell position relative to root.
      const double z = -geo.cell_z (c);
      const double x = find_row (geo.cell_x (c));
      if (x/CropWidth + z/CropDepth < 1.0
	  && z < SoilDepth)
	Density[c] = DensRtTip;
      else
	Density[c] = 0.0;
    }
  return;
}

void
RootdensLocal::set_density (const Geometry& geo, 
			    const double SoilDepth /* [cm] */, 
			    const double CropDepth /* [cm] */,
			    const double CropWidth /* [cm] */,
			    const double WRoot /* [g DM/m^2] */, const double,
			    std::vector<double>& Density  /* [cm/cm^3] */,
			    Treelog& msg)
{
  TREELOG_MODEL (msg);
  const size_t cell_size = geo.cell_size ();

  // Check input.
  daisy_assert (Density.size () == cell_size);
  daisy_assert (CropDepth > 0);
  daisy_assert (WRoot > 0);

  const double dt = 1.0;	// Daily timestep. [d]
  
  const double old_root = geo.total_soil (Density);
  const double new_root		// [cm]
    = find_length (WRoot) * geo.surface_area ();

  if (emerging)
    {
      emergence (geo, SoilDepth, CropDepth, CropWidth, Density, msg);
      LastDepth = CropDepth;
      LastWidth = CropWidth;
      if (geo.total_soil (Density) > new_root)
	return;
      msg.message ("Root system self sustained");
      emerging = false;
    }

  const double root_growth = (new_root - old_root) / dt; //  [cm/d]
  expansion (geo,
	     SoilDepth,
	     (std::min (SoilDepth, CropDepth) - LastDepth) / dt,
	     (CropWidth - LastWidth) / dt,
	     root_growth,
	     Density);
  daisy_assert (E.size () == cell_size);
  
  // Adjust existing root zone.
  I_tot = root_growth - E_tot; // [cm/d]
  if (root_growth > E_tot)
    internal_growth (geo, SoilDepth, I_tot, Density);
  else
    {
      I.resize (cell_size);
      const double I_factor = I_tot / old_root; // [d^-1]
      for (size_t c = 0; c < cell_size; ++c)
	I[c] = Density[c] * I_factor;
    }

  // Calculate new density.
  for (size_t c = 0; c < cell_size; ++c)
    Density[c] += (E[c] + I[c]) * dt;
  
  // Remember values.
  LastDepth = CropDepth;
  LastWidth = CropWidth;
}

void
RootdensLocal::tick (const Geometry& geo,
		     const SoilHeat& soil_heat, const SoilWater& soil_water,
		     std::vector<double>& L, const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  const size_t cell_size = geo.cell_size ();

  // Initialize.
  if (flooded.size () != cell_size)
    flooded = std::vector<double> (cell_size, 0.0);

  for (size_t c = 0; c < cell_size; c++)
    {
      // Update flooded
      if (soil_water.h (c) > h_threshold)
	// Abiotic::f_T0 ()
	flooded[c] += death_T_factor->value (soil_heat.T (c)) * dt;
      else
	flooded[c] = 0.0;
  
      // Calculate D
      const double L_old = L[c];
      first_order_change (L_old, 0.0, death_rate (flooded[c]), dt, L[c], D[c]);
      D_tot = D[c] * geo.cell_volume (c);
    }
}

void 
RootdensLocal::output (Log& log) const
{
  output_variable (emerging, log);
  output_value (LastDepth, "Depth", log);
  output_value (LastWidth, "Width", log);
  output_variable (flooded, log);
  output_variable (E, log);
  output_variable (expansion_volume, log);
  output_variable (E_tot, log);
  output_variable (A_tot, log);
  output_variable (I, log);
  output_variable (I_tot, log);
  output_variable (D, log);
  output_variable (D_tot, log);
}

void 
RootdensLocal::initialize (const Geometry& geo, 
                           const double row_width, const double row_pos,
                           Treelog& msg)
{ 
  TREELOG_MODEL (msg);

  const size_t cell_size = geo.cell_size ();
  E = I = D = std::vector<double> (cell_size, 0.0);

  if (row_width <= 0)
    {
      if (row_distance >= 0)
        msg.error ("Is this a row crop?");
      // Not a row crop.
      return;
    }

  const double geo_width = geo.right () - geo.left ();
  daisy_assert (row_width > 0.0);
  const double half_rows = geo_width / (0.5 * row_width);
  if (!approximate (half_rows, static_cast<int> (half_rows)))
    {
      std::ostringstream tmp;
      tmp << "Geometry width (" << geo_width 
          << ") should be an integral number of half rows (" 
          << 0.5 * row_width << ")";
      msg.warning (tmp.str ());
    }
  if (!approximate (row_distance, row_width))
    {
      std::ostringstream tmp;
      tmp << "Row width (" << row_width << ") does not match root distance ("
          << row_distance << ")";
      msg.warning (tmp.str ());
    }
  if (!approximate (row_position, row_pos))
    {
      std::ostringstream tmp;
      tmp << "Row position (" << row_pos << ") does not match root position ("
          << row_position << ")";
      msg.warning (tmp.str ());
    }
}

RootdensLocal::RootdensLocal (const BlockModel& al)
  : Rootdens (al),
    row_position (al.number ("row_position")),
    row_distance (al.number ("row_distance", -1.0)),
    DensRtTip (al.number ("DensRtTip")),
    depth_factor (al.plf ("depth_factor")),
    neighbor_effect (al.number ("neighbor_effect")),
    max_internal_growth_rate (al.number ("max_internal_growth_rate")),
    h_threshold (al.number ("h_threshold")),
    death_rate (al.plf ("death_rate")),
    death_T_factor (Librarian::build_item<Function> (al, "death_T_factor")),
    emerging (al.flag ("emerging")),
    LastDepth (al.number ("Depth", 0.0)),
    LastWidth (al.number ("Width", 0.0)),
    flooded (al.check ("flooded")
	   ? al.number_sequence ("flooded")
	   : std::vector<double> ()),
    expansion_volume (0.0),
    E_tot (0.0),
    A_tot (0.0),
    I_tot (0.0),
    D_tot (0.0)
{ }

static struct RootdensLocalSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RootdensLocal (al); }
  RootdensLocalSyntax ()
    : DeclareModel (Rootdens::component, "local", "\
Dynamic root growth model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("row_position", "cm", Check::non_negative (),
                   Attribute::Const, "\
Horizontal position of row crops.");
    frame.set ("row_position", 0.0);
    frame.declare ("row_distance", "cm", Attribute::OptionalConst, 
                   "Distance between rows of crops.");
    frame.declare ("DensRtTip", "cm/cm^3", Check::positive (), Attribute::Const,
                "Root density at (potential) penetration depth.");
    frame.set ("DensRtTip", 0.1);
    frame.declare ("depth_factor", "cm", Attribute::None (),
		   Check::non_negative (), Attribute::Const, "\
Depth (negative) affect on root growth.\n\
Specify a value less than one to decrease root growth, and larger\n\
than one to increase root growth at the specified depth.");
    frame.set ("depth_factor", PLF::always_1 ());
    frame.declare ("neighbor_effect", "cm/d", Check::non_negative (),
		   Attribute::Const, "\
How fast root density in neighbor cells affect growth.\n\
After expansion, excess roots are distributed propertional to existing\n\
root desinity in each cell, plus a part of the neighbor cells.");
    frame.set ("neighbor_effect", 0.0);
    frame.declare ("max_internal_growth_rate", "d^-1", Check::non_negative (),
		   Attribute::Const, "\
Root density within root zone cannot grow faster than this.");
    frame.set ("max_internal_growth_rate", 0.1);
    frame.declare ("h_threshold", "cm", Check::none (), Attribute::Const, "\
Root starts dying if pressure is above this threshold.");
    frame.set ("h_threshold", 0.0);
    frame.declare ("death_rate", "h", "h^-1", Attribute::Const, "\
Death rate as function of temperature modified time flooded.");
    PLF death_rate;
    death_rate.add (36.0, 0.0);
    death_rate.add (60.0, 0.1);
    frame.set ("death_rate", death_rate);
    frame.declare_function ("death_T_factor", "dg C", Attribute::None (), "\
Temperature effect on flooding.");
    frame.set ("death_T_factor", "T_min_15");
    frame.declare_boolean ("emerging", Attribute::State, "\
True until root mass is large enough to sustain root length in RZ.\n\
Until then, L within RZ is set to DensRtTip.");
    frame.set ("emerging", true);
    frame.declare ("Depth", "cm",
                   Check::non_negative (), Attribute::OptionalState,
                   "Expected depth of root zone (positive).");
    frame.declare ("Width", "cm",
                   Check::non_negative (), Attribute::OptionalState,
                   "Expected width of root zone.");
    frame.declare ("flooded", "h",
		   Attribute::OptionalState, Attribute::SoilCells, "\
Time flooded, adjusted for temperature.\n\
The temperature adjustment is the same used for mineralization.");
    frame.declare ("E", "cm/cm^3/d", Attribute::LogOnly, Attribute::SoilCells,
		   "Expansion");
    frame.declare ("expansion_volume", "cm^3", Attribute::LogOnly, "\
Expansion volume.");
    frame.declare ("E_tot", "cm/d", Attribute::LogOnly, "\
Total expansion root length.");
    frame.declare ("A_tot", "cm^2", Attribute::LogOnly, "\
Total expansion area.");
    frame.declare ("I", "cm/cm^3/d", Attribute::LogOnly, Attribute::SoilCells,
		   "Internal adjustment.");
    frame.declare ("I_tot", "cm/d", Attribute::LogOnly, "\
Total internal adjustment root length.");
    frame.declare ("D", "cm/cm^3/d", Attribute::LogOnly, Attribute::SoilCells,
		   "Death.");
    frame.declare ("D_tot", "cm/d", Attribute::LogOnly, "\
Total death root length.");
    }
} RootdensLocal_syntax;

// rootdens_local.C ends here.
