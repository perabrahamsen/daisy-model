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

#include <sstream>

struct RootdensGrowth : public Rootdens
{
  // Parameters.
  const double DensRtTip;	// Root density at (pot) pen. depth. [cm/cm^3]

  // State.
  double LastDepth;
  double LastWidth;
  double LastWRoot;

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

  if (LastDepth < 0.0)
    // Initialize.
    {
      LastDepth = CropDepth;
      LastWidth = CropWidth;
      LastWRoot = WRoot;
      const double seed_radius = 1.0; // [cm]
      const double top = std::min (0.0, -CropDepth + seed_radius);
      const double bottom = std::max (-SoilDepth, -CropDepth -seed_radius);
      for (size_t c = 0; c < cell_size; c++)
        Density[c] = geo.fraction_in_z_interval (c, top, bottom);
      const double sum = geo.total_surface (Density); // [m/m^2]
      daisy_assert (sum > 0.0);
      const double root_length = WRoot * SpRtLength;  // [m/m^2]
      const double factor = root_length / sum;        // []
      for (size_t c = 0; c < cell_size; c++)
        Density[c] *= factor;
      daisy_approximate (root_length, geo.total_surface (Density));
      return;
    }

  // Change.
  const double DeltaDepth = CropDepth - LastDepth;
  const double RelWRoot = WRoot / LastWRoot;

  if (RelWRoot <= 1.0)
    // Decrease.
    {
      for (size_t c = 0; c < cell_size; c++)
        Density[c] *= RelWRoot;
      return;
    }

  // Increase.
  std::vector<double> Extra (cell_size, 0.0);
  
  for (size_t c = 0; c < cell_size; c++)
    {
      const double old_top = geo.cell_top (c);
      const double old_bottom = std::max (geo.cell_bottom (c), 
                                          -std::min (LastDepth, SoilDepth));
      const double old_height = old_bottom - old_top;
      if (old_height <= 0)
        continue;
      const double new_top = std::min (0.0, old_top + DeltaDepth);
      const double new_bottom = std::max (geo.cell_bottom (c) - DeltaDepth,
                                          -std::min (CropDepth, SoilDepth));
      const double new_height = new_bottom - new_top;
      if (new_height <= 0)
        continue;

      // Divide it.
      const double cell_fraction = old_height / new_height;
      const double top_fraction = (old_top - new_top) / new_height;
      const double bottom_fraction = (old_bottom - new_bottom) / new_height;
      std::ostringstream tmp;
      tmp << "old_top = " << old_top;
      tmp << ", old_bottom = " << old_bottom;
      tmp << ", old_height = " << old_height;
      tmp << ", new_top = " << new_top;
      tmp << ", new_bottom = " << new_bottom;
      tmp << ", new_height = " << new_height << "\n";
      tmp << "cell = " << cell_fraction << ", top = " << top_fraction << ", bottom = " << bottom_fraction;
      msg.message (tmp.str ());
      daisy_approximate (cell_fraction + top_fraction + bottom_fraction, 1.0);

      // Find growth.
      const double cell_volume = geo.cell_volume (c); // [cm^3]
      const double cell_growth                        // [cm]
        = Density[c] * (RelWRoot - 1.0) * cell_volume;

      // Contribution within cell.
      Extra[c] += cell_growth * cell_fraction / cell_volume;

      // Add remaining to neighbor cells.
      const std::vector<size_t>& edges = geo.cell_edges (c);
      for (size_t i = 0; i < edges.size (); i++)
        {
          const size_t e = edges[i];
          if (!geo.edge_is_internal (e))
            continue;
     
          const size_t o = geo.edge_other (e, c);
          const double o_volume = geo.cell_volume (o);
          if (geo.cell_z (o) > geo.cell_z (c))
            Extra[o] += cell_growth * top_fraction / o_volume;
          else if (geo.cell_z (o) < geo.cell_z (c))
            Extra[o] += cell_growth * bottom_fraction / o_volume;
        }
    }

  for (size_t c = 0; c < cell_size; c++)
    Density[c] += Extra[c];

  // Remember values.
  LastDepth = CropDepth;
  LastWidth = CropWidth;
  LastWRoot = WRoot;
}

void 
RootdensGrowth::output (Log& log) const
{
  output_value (LastDepth, "Depth", log);
  output_value (LastWidth, "Width", log);
  output_value (LastWRoot, "WRoot", log);
}

void 
RootdensGrowth::initialize (const Geometry& geo, 
                           const double row_width, const double row_pos,
                           Treelog& msg)
{ 
  if (!iszero (row_width))
    msg.warning ("Row width not supported for '" + objid 
                 + "' root density model");
}

RootdensGrowth::RootdensGrowth (const BlockModel& al)
  : Rootdens (al),
    DensRtTip (al.number ("DensRtTip")),
    LastDepth (al.number ("Depth", -1.0)),
    LastWidth (al.number ("Width", 0.0)),
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
    frame.declare ("DensRtTip", "cm/cm^3", Check::positive (), Attribute::Const,
                "Root density at (potential) penetration depth.");
    frame.set ("DensRtTip", 0.1);
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
