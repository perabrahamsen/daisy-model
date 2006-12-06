// msoltranrect_2x1.C --- Decoupled vertical and horizontal transport.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "msoltranrect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "solute.h"
#include "element.h"
#include "alist.h"
#include "submodeler.h"
#include "memutils.h"
#include <sstream>

struct Msoltranrect2x1 : public Msoltranrect
{
  // Solute.
  std::auto_ptr<Transport> transport; // Solute transport model in matrix.
  std::auto_ptr<Transport> reserve; // Reserve solute transport model in matr.
  std::auto_ptr<Transport> last_resort; // Last resort solute transport model.
  std::auto_ptr<Transport> transport_solid; // Pseudo transport for non-solutes
  void solute (const GeometryRect&, const Soil&, const SoilWater&,
               const double J_in, Solute&, Treelog& msg);
  void element (const GeometryRect&, const Soil&, const SoilWater&,
                Element&, Adsorption&,
                const double diffusion_coefficient, Treelog&);
  static void flow (const GeometryRect& geo, 
                    const Soil& soil, 
                    const SoilWater& soil_water, 
                    const std::string& name,
                    std::vector<double>& M, 
                    std::vector<double>& C, 
                    std::vector<double>& S, 
                    std::vector<double>& /* S_p */, 
                    std::vector<double>& J, 
                    std::vector<double>& /* J_p */, 
                    Adsorption& adsorption,
                    double /* diffusion_coefficient */,
                    Treelog& msg);
  void output (Log&) const;

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Msoltranrect2x1 (Block& al);
  ~Msoltranrect2x1 ();
};

void
Msoltranrect2x1::solute (const GeometryRect& geo,
                         const Soil& soil, const SoilWater& soil_water,
                         const double J_in, Solute& solute,
                         Treelog& msg)
{ 
  Treelog::Open nest (msg, "Msoltranrect: " + name);
  const size_t edge_size = geo.edge_size ();
  const size_t cell_size = geo.cell_size ();

  solute.tick (cell_size, soil_water);

  // Upper border.
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_to (e) != Geometry::cell_above)
        continue;

      solute.J_p[e] = 0.0;
      solute.J[e] = J_in;
    }

  // Flow.
  flow (geo, soil, soil_water, solute.submodel, 
        solute.M_, solute.C_, 
        solute.S, solute.S_p,
        solute.J, solute.J_p, 
        *solute.adsorption, solute.diffusion_coefficient (), msg);
}

void 
Msoltranrect2x1::element (const GeometryRect& geo, 
                          const Soil& soil, const SoilWater& soil_water,
                          Element& element, Adsorption& adsorption,
                          const double diffusion_coefficient, Treelog& msg)
{
  element.tick (geo.cell_size (), soil_water);
  flow (geo, soil, soil_water, "DOM", 
        element.M, element.C, element.S, element.S_p, 
        element.J, element.J_p, 
        adsorption, diffusion_coefficient, msg);
}

void
Msoltranrect2x1::flow (const GeometryRect& geo, 
                       const Soil& soil, 
                       const SoilWater& soil_water, 
                       const std::string& name,
                       std::vector<double>& M, 
                       std::vector<double>& C, 
                       std::vector<double>& S, 
                       std::vector<double>& /* S_p */, 
                       std::vector<double>& J, 
                       std::vector<double>& /* J_p */, 
                       Adsorption& adsorption,
                       double /* diffusion_coefficient */,
                       Treelog& msg)
{
  const size_t edge_size = geo.edge_size ();
  const size_t cell_size = geo.cell_size ();

  // Remember old content for checking mass balance later.
  const double old_content = geo.total_soil (M);
  double in = 0.0;	
  double out = 0.0; 

  // Upper border.
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_to (e) == Geometry::cell_above)
        {
          const size_t n = geo.edge_from (e);
          M[n] -= J[e] * geo.edge_area (e) / geo.volume (n);
          in -= J[e] * geo.edge_area (e);
        }
      else
        daisy_assert (iszero (J[e]));
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
  const double expected = source + in - out;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
          << ": mass balance new - old != source + in - out\n"
          << new_content << " - " << old_content << " != " 
          << source << " + " << in << " - " << out << " (error "
          << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

void 
Msoltranrect2x1::output (Log&) const
{ }

Msoltranrect2x1::Msoltranrect2x1 (Block& al)
  : Msoltranrect (al),
    transport (Librarian<Transport>::build_item (al, "transport")),
    reserve (Librarian<Transport>::build_item (al, "transport_reserve")),
    last_resort (Librarian<Transport>::build_item (al, 
                                                   "transport_last_resort")),
    transport_solid (Librarian<Transport>::build_item (al, "transport_solid"))
{ }

Msoltranrect2x1::~Msoltranrect2x1 ()
{ }

void 
Msoltranrect2x1::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("transport", Librarian<Transport>::library (),
              "Primary solute transport model.");
  alist.add ("transport", Transport::default_model ());
  syntax.add ("transport_reserve", Librarian<Transport>::library (),
              "Reserve solute transport if the primary model fails.");
  alist.add ("transport_reserve", Transport::reserve_model ());
  syntax.add ("transport_last_resort", Librarian<Transport>::library (),
              "Last resort solute transport if the reserve model fails.");
  alist.add ("transport_last_resort", Transport::none_model ());
  syntax.add ("transport_solid", Librarian<Transport>::library (),
              "Transport model for non-dissolvable chemicals.\n\
Should be 'none'.");
  alist.add ("transport_solid", Transport::none_model ());
}

const AttributeList& 
Msoltranrect::default_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      Msoltranrect2x1::load_syntax (dummy, alist);
      alist.add ("type", "v+h");
    }
  return alist;
}

static struct Msoltranrect2x1Syntax
{
  static Msoltranrect& make (Block& al)
  { return *new Msoltranrect2x1 (al); }

  Msoltranrect2x1Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Decoupled vertical and horizontal transport.");
    Msoltranrect2x1::load_syntax (syntax, alist);
 
    Librarian<Msoltranrect>::add_type ("v+h", alist, syntax, &make);
  }
} Msoltranrect2x1_syntax;
