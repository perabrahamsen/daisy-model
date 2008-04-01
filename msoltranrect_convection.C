// msoltranrect_convection.C --- Pure forward convection.
// 
// Copyright 2007, 2008 Per Abrahamsen and KVL.
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
#include "msoltranrect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "adsorption.h"
#include "alist.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "log.h"
#include "syntax.h"
#include <sstream>

struct MsoltranrectConvection : public Msoltranrect
{
  // Log variables.
  double ddt; //size of small timestep 

  // Solute.
  void flow (const GeometryRect& geo, 
             const Soil& soil, 
             const SoilWater& soil_water, 
             const symbol name,
             std::vector<double>& C, 
             const std::vector<double>& S, 
             std::vector<double>& J, 
	     const double C_below,
	     const bool flux_below,
             double diffusion_coefficient, double dt,
             Treelog& msg);
  void output (Log&) const;

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  MsoltranrectConvection (Block& al);
  ~MsoltranrectConvection ();
};

void
MsoltranrectConvection::flow (const GeometryRect& geo, 
                              const Soil& soil, 
                              const SoilWater& soil_water, 
                              const symbol name,
                              std::vector<double>& C, 
                              const std::vector<double>& S, 
                              std::vector<double>& J_sum, 
                              const double C_below,
                              const bool /* flux_below */,
                              double diffusion_coefficient,
                              const double dt,
                              Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  double time_left = dt;

  // Keep upper boundary constant during simulation.
  const std::vector<double> J = J_sum;
  fill (J_sum.begin (), J_sum.end (), 0.0);
  
  // Initial water content.
  std::vector<double> Theta (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    Theta[c] = soil_water.Theta_old (c);

  // Small timesteps.
  for (;;)
    {
      // Solute M.
      std::vector<double> M (cell_size);
      for (size_t c = 0; c < cell_size; c++)
        M[c] = Theta[c] * C[c];

      // Are we done yet?
      const double min_timestep_factor = 0.001;
      if (time_left < 0.1 * min_timestep_factor * dt)
        break;

      // Find new timestep.
      ddt = time_left;
  
      // Limit timestep based on source term.
      for (size_t c = 0; c < cell_size; c++)
        if (S[c] < 0.0 && M[c] > 0.0) // If it is a sink.
          {
            const double time_to_empty = -M[c] / S[c];
            if (time_to_empty < min_timestep_factor * dt)
              // Unreasonable small time step.  Give up.
              continue;
            
            // Go down in timestep while it takes less than two to empty cell.
            while (time_to_empty < 2.0 * ddt)
              ddt *= 0.5;
          }
  
      // Limit timestep based on water flux.
      for (size_t e = 0; e < edge_size; e++)
        {
          const double q = soil_water.q (e);
          const int cell = (q < 0.0 ? geo.edge_to (e) : geo.edge_from (e));
          if (geo.cell_is_internal (cell))
            {
              const double water = std::fabs (q) * geo.edge_area (e);
              const double solute = water * C[cell];
              const double time_to_empty = M[cell] / solute;
              if (time_to_empty < min_timestep_factor * dt)
                // Unreasonable small time step.  Give up.
                continue;
              
              // Go down in timestep while it takes less than two to empty cell.
              while (time_to_empty < 2.0 * ddt)
                ddt *= 0.5;
            }
        }

      // Cell source.
      for (size_t c = 0; c < cell_size; c++)
        M[c] += S[c] * ddt;

      // Find fluxes using new values (more stable).
      std::vector<double> dJ (edge_size);
      for (size_t e = 0; e < edge_size; e++)
        {
          const int edge_from = geo.edge_from (e);
          const int edge_to = geo.edge_to (e);
          const double q = soil_water.q (e);
          const bool in_flux = q > 0.0;
          const int flux_from = in_flux ? edge_from : edge_to;
          double C_flux_from;
          switch (flux_from)
            {
            case Geometry::cell_above:
            case Geometry::cell_left:
            case Geometry::cell_right:
            case Geometry::cell_front:
            case Geometry::cell_back:
              dJ[e] = J[e];
              continue;
            case Geometry::cell_below:
              C_flux_from = C_below;
              break;
            default:
              daisy_assert (geo.cell_is_internal (flux_from));
              if (geo.edge_other (e, flux_from) == Geometry::cell_above)
                // No flux upwards.
                continue;
              C_flux_from = C[flux_from];
            }

          // Convection.
          dJ[e] = q * C_flux_from;
        }

      // Update values for fluxes.
      for (size_t e = 0; e < edge_size; e++)
        {
          const double value = ddt * dJ[e] * geo.edge_area (e);

          const int from = geo.edge_from (e);
          if (geo.cell_is_internal (from))
            M[from] -= value / geo.cell_volume (from);

          const int to = geo.edge_to (e);
          if (geo.cell_is_internal (to))
            M[to] += value / geo.cell_volume (to);

          J_sum[e] += dJ[e] * ddt;
        }

      // Update time left.
      time_left -= ddt;

      // Interpolate Theta.
      for (size_t c = 0; c < cell_size; c++)
        Theta[c] = time_left * soil_water.Theta_old (c) 
          + (1.0 - time_left) * soil_water.Theta (c);

      // Update C.
      for (size_t c = 0; c < cell_size; c++)
        C[c] = M[c] / Theta[c];
    }
}

void 
MsoltranrectConvection::output (Log& log) const
{ output_variable (ddt, log); }

MsoltranrectConvection::MsoltranrectConvection (Block& al)
  : Msoltranrect (al),
    ddt (-42.42e42)
{ }

MsoltranrectConvection::~MsoltranrectConvection ()
{ }

void 
MsoltranrectConvection::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("ddt", "h", Syntax::LogOnly, "Small timestep.\n\
Note that this changes dynamically during a large timestep, the value\n\
logged corresponds to the size of last small timestep during the large\n\
timestep.");
}

const AttributeList& 
Msoltranrect::reserve_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      MsoltranrectConvection::load_syntax (dummy, alist);
      alist.add ("type", "convection");
    }
  return alist;
}

static struct MsoltranrectConvectionSyntax
{
  static Model& make (Block& al)
  { return *new MsoltranrectConvection (al); }

  MsoltranrectConvectionSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Pure forward calculation of flow except through upper boundary.\n\
J[edge] = q[edge] * C_old[upstream]");
    MsoltranrectConvection::load_syntax (syntax, alist);
 
    Librarian::add_type (Msoltranrect::component, "convection",
                         alist, syntax, &make);
  }
} MsoltranrectConvection_syntax;

// msoltranrect_convection.C ends here.

