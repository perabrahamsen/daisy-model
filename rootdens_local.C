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

#include <sstream>

struct RootdensLocal : public Rootdens
{
  // Parameters.
  const double row_position;	// Horizontal position of row crops. [cm]
  const double row_distance;	// Distance betweeen rows. [cm]
  const double DensRtTip;	// Root density at (pot) pen. depth. [cm/cm^3]
  const PLF depth_factor;	// Depth affect on root growth. [cm] -> []

  // State.
  double LastWRoot;             // [g DM/m^2]
  double LastDepth;		// [cm]
  double LastWidth;		// [cm]

  // Log.
  std::vector<double> E;	  // Expansion [cm/cm^3]
  double expansion_volume;	  // Expansion volume [cm^3]
  double E_tot;			  // Total expansion root length [cm]
  double A_tot;			  // Total expansion area [cm^2]
  std::vector<double> I;	  // Internal adjustment [cm/cm^3]
  std::vector<double> D;	  // Death [cm/cm^3]
  
  
  // Utility.
  double find_row (const double x /* [cm] */) const; // [cm]
  double find_length (const double WRoot /* [g DM/m^2] */) const; // [cm/cm^2]

  // Simulation.
  void expansion (const Geometry& geo,
		  const double delta_depth, // [cm]
		  const double delta_width, // [cm]
		  const double delta_root,  // [cm]
		  std::vector<double>& L);
  
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth, double CropWidth,
		    double WRoot, double DS, std::vector<double>& Density,
		    Treelog&);
  void output (Log& log) const;

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
			  const double delta_depth, // [cm]
			  const double delta_width, // [cm]
			  const double delta_root,  // [cm]
			  std::vector<double>& L)   // [cm/cm^3]
{
  if (delta_root <= 0.0)
    // No expansion if roots are shrinking.
    {
      std::fill (E.begin (), E.end (), 0.0);
      expansion = 0.0;
      E_tot = 0.0;
      A_tot = 0.0;
      return;
    }
  
  const size_t cell_size = geo.cell_size ();
  daisy_assert (L.size () == cell_size);
  daisy_assert (E.size () == cell_size);

  // Clear integrated values.
  expansion_volume = 0.0;	// [cm^3]
  E_tot = 0.0;		// [cm]
  A_tot = 0.0;		// [cm^2]
    
  for (size_t c = 0; c < cell_size; ++c)
    {
      const double fE = 1.0;		      // Expansion factor []
	
      if (L[c] > DensRtTip)
	// Ignore existing root zone
	{
	  E[c] = 0.0; // [cm/cm^3]
	  continue;
	}

      // Other cells will get expansion from their neighbors.
      const double A_con = 0.0;	// Connecting area with RZ cells [cm^2]
      const double V = geo.cell_volume (c); // [cm^3]
      const double V_con = 0.0;	// Connecting volume with RZ cells [cm^3]
	  
      const std::vector<size_t>& edges = geo.cell_edges (c);
      const size_t edge_size = edges.size ();
      for (size_t e = 0; e < edge_size; ++e)
	{
	  if (!edge_is_internal (e))
	    continue;

	  const int o = geo.other (e, c);
	  daisy_assert (geo.cell_is_internal (o));
	  daisy_assert (o >= 0 && o < cell_size);

	  if (L[o] < DensRtTip)
	    // Only cells above DensRtTip expand to neighbors.
	    continue;

	  const double A = geo.edge_area (e); // [cm^2]
	  A_con += A;

	  const double dx = delta_width * geo.edge_cos_angle (e);
	  const double dy = delta_depth * geo.edge_sin_angle (e);
	  const double dl = std::sqrt (dx*dx + dy*dy); // Pythagoras...
	  V_con += A * dl;
	}
	    
      // The expansion from neighbors + internal expansion
      // can never be larger than the cell.
      const double V_exp = std::min (V, V_con);
      const double E_length = fE * DensRtTip * V_exp;
      E[c] = E_length / V;
      E_tot += E_length;
      expansion_volume += V_exp;
    }

  daisy_approximate (geo.total_soil (E), E_tot);
  counst double factor = delta_root / E_tot;
  if (factor > 1.0)
    // Sufficient roots to expand with with DensRtTip.
    return;

  // Shrink for available root length.
  for (int c = 0; c < cell_size; c++)
    E[c] *= facor;
  daisy_approximate (geo.total_soil (E), delta_root);
  E_tot = delta_root;
}

void
RootdensLocal::set_density (const Geometry& geo, 
			    const double SoilDepth /* [cm] */, 
			    const double CropDepth /* [cm] */,
			    const double CropWidth /* [cm] */,
			    const double WRoot /* [g DM/m^2] */, const double,
			    std::vector<double>& Density  /* [cm/cm^3] */,
			    Treelog& msg)
// Check emergence
//   If so, put WRoot in seed cells
// Check WRoot > LastWRoot
//   If so, do expansion
//   Check if expansion needs to be limited by delta WRoot
// Do internal adjustment

{
  TREELOG_MODEL (msg);
  const size_t cell_size = geo.cell_size ();

  // Check input.
  daisy_assert (Density.size () == cell_size);
  daisy_assert (CropDepth > 0);
  daisy_assert (WRoot > 0);

    {
      if (WRoot <= LastWRoot)
        // Decrease.
        {
          const double RelWRoot = WRoot / LastWRoot;
          for (size_t c = 0; c < cell_size; c++)
            Density[c] *= RelWRoot;
        }
      else
        // Increase.
        {
          // What is available?
          const double new_root 
            = find_length (WRoot - LastWRoot) * geo.surface_area (); // [cm]

          // What is needed to fill the root zone?
          double missing = 0.0; // [cm]

          // What is already in the root zone?
          double total = 0.0;   // [cm]
          double total_weight = 0.0;
	  
          // Find it.
          for (size_t c = 0; c < cell_size; c++)
            {
              // Cell position relative to root.
              const double z = -geo.cell_z (c);
              const double x = find_row (geo.cell_x (c));
              const double V =  geo.cell_volume (c); // [cm^3]
              total += Density[c] * V;
	      total_weight += Density[c] * V * depth_factor (-z);
	      
	      daisy_assert (CropWidth > 0.0);
	      daisy_assert (CropDepth > 0.0);
	      
              if (x/CropWidth + z/CropDepth < 1.0
                  && z < SoilDepth
                  && Density[c] < DensRtTip)
                missing += (DensRtTip - Density[c]) * V;
            }
          daisy_approximate (total, geo.total_soil (Density));

	  // Relative to zero?
	  if (total_weight <= 0.0 && total > 0)
	    {
	      msg.error ("Weighted root lenght <= 0.0");
	      return;
	    }
	  
          // To each according to need.
          const double fill_factor = missing > new_root
            ? new_root / missing
            : 1.0;

          // To those who have shall be given.
          const double growth_factor = (total > 0 && missing < new_root)
            ? (new_root - missing) / total_weight
            : 0.0;

          // Fill it.
          for (size_t c = 0; c < cell_size; c++)
            {
              // Cell position relative to root.
              const double z = -geo.cell_z (c);
              const double x = find_row (geo.cell_x (c));
              const double old = Density[c];
              Density[c] += old * growth_factor * depth_factor (-z);

              if (x/CropWidth + z/CropDepth < 1.0
                  && z < SoilDepth
                  && old < DensRtTip)
                Density[c] += (DensRtTip - old) * fill_factor;
              
            }

          // Check.
          const double old_root = find_length (LastWRoot) * geo.surface_area ();
          daisy_approximate (old_root + new_root, geo.total_soil (Density));
        }
    }
  // Remember values.
  LastWRoot = WRoot;
}

void 
RootdensLocal::output (Log& log) const
{
  output_value (LastWRoot, "WRoot", log);
}

void 
RootdensLocal::initialize (const Geometry& geo, 
                           const double row_width, const double row_pos,
                           Treelog& msg)
{ 
  if (row_width <= 0)
    {
      if (row_distance >= 0)
        msg.error ("Is this a row crop?");
      // Not a row crop.
      return;
    }

  TREELOG_MODEL (msg);

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
    LastWRoot (al.number ("WRoot", 0.0))
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
                   Attribute::State, "\
Horizontal position of row crops.");
    frame.set ("row_position", 0.0);
    frame.declare ("row_distance", "cm", Attribute::OptionalState, 
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
    frame.declare ("Depth", "cm",
                   Check::non_negative (), Attribute::OptionalState,
                   "Expected depth of root zone (positive).");
    frame.declare ("Width", "cm",
                   Check::non_negative (), Attribute::OptionalState,
                   "Expected width of root zone.");
    frame.declare ("WRoot", "g DM/m^2", 
                   Check::non_negative (), Attribute::OptionalState,
                   "Total root mass.");
    }
} RootdensLocal_syntax;

// rootdens_local.C ends here.
