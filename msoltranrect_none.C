// msoltranrect_none.C --- No transport.
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

struct MsoltranrectNone : public Msoltranrect
{
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
  MsoltranrectNone (Block& al);
  ~MsoltranrectNone ();
};

void
MsoltranrectNone::flow (const GeometryRect& geo, 
                        const Soil& soil, 
                        const SoilWater& soil_water, 
                        const symbol name,
                        std::vector<double>& C, 
                        const std::vector<double>& S, 
                        std::vector<double>& J, 
			const double /* C_below */,
			const bool /* flux_below */,
                        double /* diffusion_coefficient */,
                        const double dt,
                        Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  // Solute M.
  std::vector<double> M (cell_size);
  for (size_t i = 0; i < M.size (); i++)
    M[i] = soil_water.Theta_old (i) * C[i];

  // Upper border.
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      const double value = in_sign * J[edge] * dt * geo.edge_area (edge); // [g]
      M[cell] += value / geo.cell_volume (cell);
    }

  // Cell source.
  for (size_t n = 0; n < cell_size; n++)
    M[n] += S[n] * dt;

  // Update C.
  for (size_t n = 0; n < cell_size; n++)
    C[n] = M[n] * soil_water.Theta (n) * C[n];
}

void 
MsoltranrectNone::output (Log&) const
{ }

MsoltranrectNone::MsoltranrectNone (Block& al)
  : Msoltranrect (al)
{ }

MsoltranrectNone::~MsoltranrectNone ()
{ }

void 
MsoltranrectNone::load_syntax (Syntax&, AttributeList&)
{ }

const AttributeList& 
Msoltranrect::none_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      MsoltranrectNone::load_syntax (dummy, alist);
      alist.add ("type", "none");
    }
  return alist;
}

static struct MsoltranrectNoneSyntax
{
  static Model& make (Block& al)
  { return *new MsoltranrectNone (al); }

  MsoltranrectNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Disable all transport except through boundaries.");
    MsoltranrectNone::load_syntax (syntax, alist);
 
    Librarian::add_type (Msoltranrect::component, "none", alist, syntax, &make);
  }
} MsoltranrectNone_syntax;

// msoltranrect_none.C ends here.
