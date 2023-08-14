// uzlr.C --- using linear reservoirs to calculate water flow.
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

#include "uzmodel.h"
#include "block_model.h"
#include "frame.h"
#include "surface.h"
#include "groundwater.h"
#include "geometry_vert.h"
#include "soil.h"
#include "soil_heat.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include <sstream>

struct UZlr : public UZmodel
{
  // Parameters.
  bool overflow_warn;     // Warn first time profile is oversaturared.
  const double h_fc;		// Field Capacity. [cm]
  const double z_top;		// Depth of layer with upw. water movement [cm]

  // Simulate.
  void tick (Treelog&, const GeometryVert& geo,
             const Soil& soil, const SoilHeat& soil_heat,
	     unsigned int first, const Surface& top, 
             size_t top_edge,
	     unsigned int last, const Groundwater& bottom, 
             const size_t bottom_edge,
	     const std::vector<double>& S,
	     const std::vector<double>& h_old,
	     const std::vector<double>& Theta_old,
	     const std::vector<double>& h_ice,
	     std::vector<double>& h,
	     std::vector<double>& Theta,
             size_t q_offset,
	     std::vector<double>& q_base,
             double dt);

  // Create and Destroy.
  void has_macropores (bool)
  { }
  UZlr (const BlockModel& par);
  ~UZlr ();
};

void
UZlr::tick (Treelog& msg, const GeometryVert& geo,
            const Soil& soil, const SoilHeat& soil_heat,
	    unsigned int first, const Surface& top, 
            const size_t top_edge,
	    unsigned int last, const Groundwater& bottom, 
            const size_t bottom_edge,
	    const std::vector<double>& S,
	    const std::vector<double>& h_old,
	    const std::vector<double>& Theta_old,
	    const std::vector<double>& h_ice,
	    std::vector<double>& h,
	    std::vector<double>& Theta,
            const size_t q_offset,
            std::vector<double>& q_base,
            const double dt)
{
  double *const q = &q_base[q_offset];
  double q_up = 0.0;
  double q_down = 0.0;
  const Surface::top_t top_type = top.top_type (geo, top_edge);

  // Limit flux by soil capacity.
  const double K_sat = soil.K (first, 0.0, h_ice[first], 
			       soil_heat.T (first));
  daisy_assert (K_sat > 0.0);

  if (top_type == Surface::forced_pressure)
    {
      const double dz = 0.0 - geo.cell_z (first);
      const double dh = top.h_top (geo, top_edge) - h_old[first];
      q_up = q[first] = -K_sat * (dh/dz + 1.0);
    }
  else
    // Limited water or forced flux.
    q_up = q[first] = std::max (top.q_top (geo, top_edge, dt), -K_sat);

  //  Use darcy for upward movement in the top.
  const bool use_darcy = (h_old[first] < h_fc) && (q_up > 0.0);

  // Intermediate cells.
  for (int i = first; i <= last; i++)
    {
      const double z = geo.cell_z (i);
      const double dz = geo.dz (i);
      const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
      const double Theta_res = soil.Theta_res (i);
      const double h_min = pF2h (10.0);
      const double Theta_min = soil.Theta (i, h_min, h_ice[i]);
      double Theta_new = Theta_old[i] - q[i] * dt / dz - S[i] * dt;
      if (Theta_new < Theta_min)
        {
          // Extreme dryness.
          q[i+1] = (Theta_min - Theta_new) * dz / dt;
          Theta[i] = Theta_min;
          h[i] = h_min;
          daisy_assert (std::isfinite (h[i]));
          continue;
        }

      daisy_assert (std::isfinite (h_old[i]));
      const double h_new = Theta_new >= Theta_sat 
        ? std::max (h_old[i], 0.0)
        : soil.h (i, Theta_new);
      daisy_assert (std::isfinite (h_new));
      double K_new = soil.K (i, h_new, h_ice[i], soil_heat.T (i));

      // If we have free drainage bottom, we go for field capacity all
      // the way.  Otherwise, we assume groundwater start at the
      // bottom of the last cell, and attempt equilibrium from there.
      // This asumption is correct for lysimeter bottom, adequate for
      // pressure bottom (where groundwater table is in the last
      // cell), and wrong for forced flux (= pipe drained soil) where
      // the groundwater is usually much higher.  Still, it is better
      // than using h_fc.
      double h_lim;
      switch (bottom.bottom_type ())
        {
        case Groundwater::free_drainage:
          h_lim = h_fc;
          break;
        case Groundwater::pressure:
          h_lim = std::max (bottom.table () - z, h_fc);
          break;
        case Groundwater::lysimeter:
        default:
          h_lim = std::max (geo.zplus (last) - z, h_fc);
          break;
        }

      if (use_darcy && z > z_top && i < last && h_fc < h_ice[i] )
        // Dry earth, near top.  Use darcy to move water up.
        {
	  const double dist = z - geo.cell_z (i+1);
	  q[i+1] = std::max (K_new * ((h_old[i+1] - h_new) / dist - 1.0), 0.0);
          const double Theta_next = Theta_new + q[i+1] * dt / dz;
	  if (Theta_next > Theta_sat)
	    {
	      q[i+1] = (Theta_sat - Theta_new) * dz / dt;
	      Theta[i] = Theta_sat;
	      h[i] = std::max (0.0, h_new);
              daisy_assert (std::isfinite (h[i]));
	    }
	  else
	    {
	      Theta[i] = Theta_next;
	      h[i] = soil.h (i, Theta[i]);
              daisy_assert (std::isfinite (h[i]));
	    }
	}
      else if (h_new <= h_lim)
	// Dry earth, no water movement.
	{
	  if (Theta_new <= Theta_sat)
            {
              q[i+1] = 0.0;
              Theta[i] = Theta_new;
              h[i] = h_new;
              daisy_assert (std::isfinite (h[i]));
            }
          else 
            {
              q[i+1] = (Theta_sat - Theta_new) * dz / dt;
              Theta[i] = Theta_sat;
              h[i] = std::max (0.0, h_new);
              daisy_assert (std::isfinite (h[i]));
            }
	}
      else
	// Gravitational water movement.
	{
	  if (i < last)
	    {
	      // Geometric average K.
	      if (h_ice[i+1] < h_fc) // Blocked by ice.
		K_new = 0.0;
	      else
		K_new = sqrt (K_new * soil.K (i+1, h_old[i+1], h_ice[i+1],
					      soil_heat.T (i+1)));
	    }
	  else if (bottom.bottom_type () == Groundwater::forced_flux)
	    K_new = -bottom.q_bottom (bottom_edge);
          // else keep K_new from the node.
            
	  const double Theta_lim = soil.Theta (i, h_lim, h_ice[i]);
	  const double Theta_next = Theta_new - K_new * dt / dz;

	  if (Theta_next < Theta_lim)
	    {
              if (Theta_lim < Theta_new)
                {
                  q[i+1] = (Theta_lim - Theta_new) * dz / dt;
                  Theta[i] = Theta_lim;
                  h[i] = h_lim;
                  daisy_assert (std::isfinite (h[i]));
                }
              else
                {
                  q[i+1] = 0.0;
                  Theta[i] = Theta_new;
                  h[i] = h_new;
                  daisy_assert (std::isfinite (h[i]));
                }
	    }
	  else if (Theta_next >= Theta_sat)
	    {
	      q[i+1] = (Theta_sat - Theta_new) * dz / dt;
	      Theta[i] = Theta_sat;
	      h[i] = std::max (h_old[i], 0.0);
              daisy_assert (std::isfinite (h[i]));
	    }
	  else
	    {
	      q[i+1] = -K_new;
	      Theta[i] = Theta_next;
	      h[i] = soil.h (i, Theta[i]);
              daisy_assert (std::isfinite (h[i]));
	    }
	}
      daisy_assert (std::isfinite (h[i]));
      daisy_assert (std::isfinite (Theta[i]));
      daisy_assert (std::isfinite (q[i+1]));
      daisy_assert (Theta[i] <= Theta_sat);
#ifdef THETA_RES
      daisy_assert (Theta[i] > Theta_res);
#endif
    }

  // Lower border.
  q_down = q[last + 1];

  if (bottom.bottom_type () == Groundwater::forced_flux)
    // Ensure forced bottom.
    {
      double extra_water = (bottom.q_bottom (bottom_edge) - q_down) * dt;

      for (int i = last; true; i--)
	{
	  q[i+1] += extra_water / dt;
	  if (i < static_cast<int> (first))
            {
              if (extra_water > 0.0 && overflow_warn)
                {
                  msg.warning ("Soil profile saturated, water flow to surface");
                  overflow_warn = false;
                }
              break;
            }
	  const double dz = geo.dz (i);
          const double h_min = pF2h (10.0);
          const double Theta_min = soil.Theta (i, h_min, h_ice[i]);
	  const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
	  Theta[i] += extra_water / dz;
          if (Theta[i] <= Theta_min)
            {
              extra_water = (Theta[i] - Theta_min) * dz;
	      Theta[i] = Theta_min;
	      h[i] = h_min;
            }
	  else if (Theta[i] <= Theta_sat)
	    {
	      extra_water = 0;
	      h[i] = soil.h (i, Theta[i]);
              break;
	    }
	  else
	    {
	      extra_water = (Theta[i] - Theta_sat) * dz;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	}
      q_up = q[first];
      q_down = q[last + 1];
    }

  // Saturated pressure.
  double table = geo.cell_z (last) + h[last];
  for (int i = last; i > first; i--)
    if (h[i] < 0.0)
      {
        table = geo.cell_z (i) + h[i];
        break;
      }
  for (int i = last; i > first; i--)
    if (geo.cell_z (i) < table)
      {
        daisy_assert (h[i] >= 0.0);
        h[i] = table - geo.cell_z (i);
        daisy_assert (h[i] >= 0.0);
      }
    else
      break;

  // Check mass conservation.
  double total_old = 0.0;
  double total_new = 0.0;
  double total_S = 0.0;
  for (unsigned int i = first; i <= last; i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      daisy_assert (Theta[i] <= Theta_sat + 1e-10);
      total_old += geo.dz (i) * Theta_old[i];
      total_new += geo.dz (i) * Theta[i];
      total_S += geo.dz (i) * S[i];
      daisy_assert (std::isfinite (Theta[i]));
      if (Theta[i] <= 0.0)
        {
          std::ostringstream tmp;
          tmp << "Theta[" << i << "] = " << Theta[i];
          daisy_bug (tmp.str ());
          Theta[i] = 1e-9;
        }
    }
  daisy_balance (total_old, total_new, (-q_up + q_down - total_S) * dt);
}

UZlr::UZlr (const BlockModel& al)
  : UZmodel (al),
    overflow_warn (al.flag ("overflow_warn")),
    h_fc (al.number ("h_fc")),
    z_top (al.number ("z_top"))
{ }

UZlr::~UZlr ()
{ }

static struct UZlrSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZlr (al); }

  UZlrSyntax ()
    : DeclareModel (UZmodel::component, "lr", "\
Use gravitational water movement for wet soil, where h > h_fc.\n\
There are no water movement when h < h_fc, except at the layers down\n\
to z_top, where there can be Darcy movement.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_boolean ("overflow_warn", Attribute::Const, "\
If true, warn the first time the soil profile is oversaturated.");
    frame.set ("overflow_warn", true);
    frame.declare ("h_fc", "cm", Attribute::Const, "Field capacity.");
    frame.set ("h_fc", -100.0);
    frame.declare ("z_top", "cm", Attribute::Const, 
               "Depth of layer where upward water movement is possible.");
    frame.set ("z_top", -10.0);
  }
} UZlr_syntax;

// uzlr.C ends here.
