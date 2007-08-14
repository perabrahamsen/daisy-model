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
             const std::string& name,
             std::vector<double>& M, 
             std::vector<double>& C, 
             const std::vector<double>& S, 
             std::vector<double>& J, 
	     const double C_below,
             Adsorption& adsorption,
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
                        const std::string& name,
                        std::vector<double>& M, 
                        std::vector<double>& C, 
                        const std::vector<double>& S, 
                        std::vector<double>& J, 
			const double /* C_below */,
                        Adsorption& adsorption,
                        double /* diffusion_coefficient */,
                        const double dt,
                        Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  // Remember old content for checking mass balance later.
  const double old_content = geo.total_soil (M);
  double in = 0.0;	
  double out = 0.0; 

  // Upper border.
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      
      const double sin_angle = geo.edge_sin_angle (edge);
      const double value = J[edge] * geo.edge_area (edge) * sin_angle;
      M[cell] -=  dt * value / geo.cell_volume (cell);
      in -= value;
    }

  // Cell fluxes.
  for (size_t n = 0; n < cell_size; n++)
    {
      M[n] += S[n] * dt;
      C[n] = adsorption.M_to_C (soil, soil_water.Theta (n), n, M[n]);

      if (!(M[n] >= 0.0))
        {

          std::ostringstream tmp;
          tmp << "BUG: M[" << n << "] = " << M[n] 
              << " (J[0] = " << J[0] << ") S[" << n << "] = " << S[n];
          msg.error (tmp.str ());
          M[n] = C[n] = 0.0;
        }
      daisy_assert (M[n] >= 0.0);
      daisy_assert (C[n] >= 0.0);
    }

  // Mass balance.
  const double new_content = geo.total_soil (M);
  const double delta_content = new_content - old_content;
  const double source = geo.total_soil (S);
  // BUG: ASSume uniform boundaries.
  const double expected = (source + in - out) * dt;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
          << ": mass balance new - old != source + in - out\n"
          << new_content << " - " << old_content << " != " 
          << source * dt << " + " << in * dt << " - " << out * dt << " (error "
          << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
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
