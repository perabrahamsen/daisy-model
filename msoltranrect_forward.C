// msoltranrect_forward.C --- Pure forward convection.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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
#include <sstream>

struct MsoltranrectForward : public Msoltranrect
{
  // Solute.
  void flow (const GeometryRect& geo, 
             const Soil& soil, 
             const SoilWater& soil_water, 
	     const Adsorption&, 
             const symbol name,
             std::vector<double>& M, 
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
  MsoltranrectForward (Block& al);
  ~MsoltranrectForward ();
};

void
MsoltranrectForward::flow (const GeometryRect& geo, 
                           const Soil& soil, 
                           const SoilWater& soil_water, 
                           const Adsorption& adsorption, 
                           const symbol name,
                           std::vector<double>& M, 
                           std::vector<double>& C, 
                           const std::vector<double>& S, 
                           std::vector<double>& J, 
                           const double C_below,
                           const bool /* flux_below */,
                           double diffusion_coefficient,
                           const double dt,
                           Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  // Remember old content for checking mass balance later.
  const double old_content = geo.total_soil (M);
  double border = 0.0;	

  // Find fluxes using old values.
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
          // Already handled.
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
      daisy_assert (iszero (J[e]));
      J[e] = q * C_flux_from;

#if 0
      // Diffusion.
      double M_edge_from;
      if (edge_from == Geometry::cell_below)
        M_edge_from = C_below;
      else if (!geo.cell_is_internal (edge_from))
        continue;
      else
        M_edge_from = C[edge_from];
      
      double M_edge_to;
      if (edge_to == Geometry::cell_below)
        M_edge_to = C_below;
      else if (!geo.cell_is_internal (edge_to))
        continue;
      else
        M_edge_to = C[edge_to];
      
      const double l = geo.edge_length (e); // Fick's first law.
      J[e] += -diffusion_coefficient * (M_edge_to - M_edge_from) / l;
#endif
    }

  // Update values for fluxes.
  for (size_t e = 0; e < edge_size; e++)
    {
      const double value = dt * J[e] * geo.edge_area (e);

      const int from = geo.edge_from (e);
      if (geo.cell_is_internal (from))
        M[from] -= value / geo.cell_volume (from);
      else 
        border -= value * dt;

      const int to = geo.edge_to (e);
      if (geo.cell_is_internal (to))
        M[to] += value / geo.cell_volume (to);
      else 
        border += value * dt;
    }

  // Update cell for source term and concentration.
  for (size_t c = 0; c < cell_size; c++)
    {
      M[c] += S[c] * dt;
      if (M[c] < 0.0)
        throw "Negative content in solution";
      C[c] = adsorption.M_to_C (soil, soil_water.Theta (c), c, M[c]);
      if (C[c] < 0.0)
        throw "Negative concentration in solution";
    }

  // Mass balance.
  const double new_content = geo.total_soil (M);
  const double delta_content = new_content - old_content;
  const double source = geo.total_soil (S) * dt;
  const double expected = (source - border);
  if (!approximate (delta_content, expected)
      && !approximate (new_content, old_content + expected))
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
          << ": mass balance new - old != source - border\n"
          << new_content << " - " << old_content << " != " 
          << source << " - " << border << " (error "
          << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

void 
MsoltranrectForward::output (Log&) const
{ }

MsoltranrectForward::MsoltranrectForward (Block& al)
  : Msoltranrect (al)
{ }

MsoltranrectForward::~MsoltranrectForward ()
{ }

void 
MsoltranrectForward::load_syntax (Syntax&, AttributeList&)
{ }

const AttributeList& 
Msoltranrect::reserve_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      MsoltranrectForward::load_syntax (dummy, alist);
      alist.add ("type", "forward");
    }
  return alist;
}

static struct MsoltranrectForwardSyntax
{
  static Model& make (Block& al)
  { return *new MsoltranrectForward (al); }

  MsoltranrectForwardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Pure forward calculation of flow except through upper boundary.\n\
J[edge] = q[edge] * C_old[upstream]");
    MsoltranrectForward::load_syntax (syntax, alist);
 
    Librarian::add_type (Msoltranrect::component, "forward",
                         alist, syntax, &make);
  }
} MsoltranrectForward_syntax;

// msoltranrect_forward.C ends here.

