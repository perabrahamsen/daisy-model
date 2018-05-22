// rootdens_growth.C -- Dynamic model for root growth.
// 
// Copyright 2012 Per Abrahamsen and KU.
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

struct RootdensGrowth : public Rootdens
{
  // Parameters.
  const double row_position;	// Horizontal position of row crops. [cm]
  const double row_distance;	// Distance betweeen rows. [cm]
  const double DensRtTip;	// Root density at (pot) pen. depth. [cm/cm^3]
  const PLF depth_factor;	// Depth affect on root growth. [cm] -> []

  // State.
  double LastWRoot;             // [g DM/m^2]

  // Utility.
  double find_row (const double x /* [cm] */) const; // [cm]
  double find_length (const double WRoot /* [g DM/m^2] */) const; // [cm/cm^2]

  // simulation.
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth, double CropWidth,
		    double WRoot, double DS, std::vector<double>& Density,
		    Treelog&);
  void output (Log& log) const;

  // Create.
  void initialize (const Geometry&, 
                   double row_width, double row_pos, Treelog&);
  explicit RootdensGrowth (const BlockModel&);
};

double // [cm/cm^2]
RootdensGrowth::find_length (const double WRoot /* [g DM/m^2] */) const 
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
RootdensGrowth::find_row (const double x /* [cm] */) const // [cm]
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
RootdensGrowth::set_density (const Geometry& geo, 
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
RootdensGrowth::output (Log& log) const
{
  output_value (LastWRoot, "WRoot", log);
}

void 
RootdensGrowth::initialize (const Geometry& geo, 
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

RootdensGrowth::RootdensGrowth (const BlockModel& al)
  : Rootdens (al),
    row_position (al.number ("row_position")),
    row_distance (al.number ("row_distance", -1.0)),
    DensRtTip (al.number ("DensRtTip")),
    depth_factor (al.plf ("depth_factor")),
    LastWRoot (al.number ("WRoot", 0.0))
{ }

static struct RootdensGrowthSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RootdensGrowth (al); }
  RootdensGrowthSyntax ()
    : DeclareModel (Rootdens::component, "growth", "\
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
} RootdensGrowth_syntax;

// rootdens_growth.C ends here.
