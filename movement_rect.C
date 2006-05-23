// movement_rect.C --- Movement in a rectangular 2D grid.
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

#include "movement.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat_rect.h"
#include "solute.h"
#include "element.h"
#include "surface.h"
#include "log.h"
#include "submodeler.h"

#include <sstream>

struct MovementRect : public Movement
{
  // Geometry and heat.
  std::auto_ptr<GeometryRect> geo;
  std::auto_ptr<SoilHeatRect> heat;

  // Water.
  std::auto_ptr<UZmodel> uzdefault;
  std::auto_ptr<UZmodel> uzreserve;

  // Solute.
  std::auto_ptr<Transport> transport; // Solute transport model in matrix.
  std::auto_ptr<Transport> reserve; // Reserve solute transport model in matr.
  std::auto_ptr<Transport> last_resort; // Last resort solute transport model.
  std::auto_ptr<Transport> transport_solid; // Pseudo transport for non-solutes

  // Simulation.
  Geometry& geometry () const
  { return *geo; }
  SoilHeat& soil_heat () const
  { return *heat; }

  void macro_tick (const Soil&, SoilWater&, Surface&, Treelog&)
  { }

  void tick (const Soil& soil, SoilWater& soil_water,
             Surface& surface, Groundwater& groundwater, const Time&,
             const Weather&, Treelog& msg) 
  {
    soil_water.tick (geo->cell_size (), soil, msg); 
    tick_water (*geo, soil, *heat, surface, groundwater, 
                soil_water.S_sum_, soil_water.h_old_, soil_water.Theta_old_,
                soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
                soil_water.q_, soil_water.q_p_,
                msg);
  }

  static void tick_water (const Geometry& geo,
                          const Soil& soil, const SoilHeat&, 
                          Surface& surface, Groundwater&,
                          const std::vector<double>& S,
                          std::vector<double>& /* h_old */,
                          const std::vector<double>& /* Theta_old */,
                          const std::vector<double>& /* h_ice */,
                          std::vector<double>& h,
                          std::vector<double>& Theta,
                          std::vector<double>& q,
                          std::vector<double>& /* q_p */,
                          Treelog& msg)
  {
    const size_t cell_size = geo.cell_size ();

    // Update water.
    for (size_t i = 0; i < cell_size; i++)
      {
        Theta[i] -= S[i] * dt;
        h[i] = soil.h (i, Theta[i]);
      }

    const double q_up = surface.q ();
    const size_t edge_size = geo.edge_size ();
    const double surface_area = geo.surface_area ();

    for (size_t e = 0; e < edge_size; e++)
      if (geo.edge_to (e) == Geometry::cell_above)
        {
          if (q_up > 0)
            // We obey exfiltration.
            {
              const size_t n = geo.edge_from (e);
              q[e] = q_up / surface_area;
              Theta[n] -= q[e] * geo.edge_area (e) * dt / geo.volume (n);
              surface.accept_top (q_up, geo, e, msg);
            }
          else
            q[e] = 0.0;
        }
  }
  void solute (const Soil& soil, const SoilWater& soil_water,
               const double J_in, Solute& solute,
               Treelog& msg)
  { 
    Treelog::Open nest (msg, "Movement: " + name);
    const size_t edge_size = geo->edge_size ();
    const size_t cell_size = geo->cell_size ();
    
    solute.tick (cell_size, soil_water);
    
    // Upper border.
    for (size_t e = 0; e < edge_size; e++)
      {
        if (geo->edge_to (e) != Geometry::cell_above)
          continue;

        solute.J_p[e] = 0.0;
        solute.J[e] = J_in;
      }

    // Flow.
    flow (*geo, soil, soil_water, solute.submodel, 
          solute.M_, solute.C_, 
          solute.S, solute.S_p,
          solute.J, solute.J_p, 
          *solute.adsorption, solute.diffusion_coefficient (), msg);
  }

  void element (const Soil& soil, const SoilWater& soil_water,
                Element& element, Adsorption& adsorption,
                const double diffusion_coefficient, Treelog& msg)
  {
    element.tick (geo->cell_size (), soil_water);
    flow (*geo, soil, soil_water, "DOM", 
          element.M, element.C, element.S, element.S_p, 
          element.J, element.J_p, 
          adsorption, diffusion_coefficient, msg);
  }

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
                    Treelog& msg)
  {
    const size_t edge_size = geo.edge_size ();
    const size_t cell_size = geo.cell_size ();
    
    // Remember old content for checking mass balance later.
    const double old_content = geo.total (M);
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
          daisy_assert (J[e] == 0.0);
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
    const double new_content = geo.total (M);
    const double delta_content = new_content - old_content;
    const double source = geo.total (S);
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


  void ridge (Surface&, const Soil&, const SoilWater&, const AttributeList&)
  { throw "Can't make ridges on a rectangular grid"; }

  void output (Log& log) const
  { 
    output_submodule (*heat, "Heat", log);
  }

  // Create.
  bool check (Treelog& err) const;
  bool volatile_bottom () const
  { return false; }
  void initialize_soil (Soil&, Treelog&) const
  { }

  void initialize (const AttributeList& alist,
                   const Soil&, const Groundwater&, 
                   const Time&, const Weather&, Treelog& msg)
  {
    heat->initialize (alist.alist ("Heat"), *geo, msg);
  }
  MovementRect (Block& al)
    : Movement (al),
      geo (submodel<GeometryRect> (al, "Geometry")),
      heat (submodel<SoilHeatRect> (al, "Heat")),
      uzdefault (Librarian<UZmodel>::build_item (al, "UZdefault")),
      uzreserve (Librarian<UZmodel>::build_item (al, "UZreserve")),
      transport (Librarian<Transport>::build_item (al, "transport")),
      reserve (Librarian<Transport>::build_item (al, "transport_reserve")),
      last_resort (Librarian<Transport>::build_item (al,
                                                     "transport_last_resort")),
      transport_solid (Librarian<Transport>::build_item (al,
                                                         "transport_solid"))
  { }
};

bool
MovementRect::check (Treelog& err) const
{
  const size_t n = geo->cell_size ();

  bool ok = true;
  {
    Treelog::Open nest (err, "Heat");
    if (!heat->check (n, err))
      ok = false;
  }
  return ok;
}

static struct MovementRectSyntax
{
  static Movement& make (Block& al)
  { return *new MovementRect (al); }

  MovementRectSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Two dimensional movement in a rectangular grid.");
    syntax.add_submodule ("Geometry", alist, Syntax::State,
                          "Discretization of the soil.",
                          GeometryRect::load_syntax);
    syntax.add_submodule ("Heat", alist, Syntax::State,
                          "Soil heat and flux.",
                          SoilHeatRect::load_syntax);
    syntax.add ("UZdefault", Librarian<UZmodel>::library (),
                "Main water transport model in unsaturated zone.");
    alist.add ("UZdefault", UZmodel::default_model ());
    syntax.add ("UZreserve", Librarian<UZmodel>::library (),
                "Reserve transport model if UZtop fails.");
    alist.add ("UZreserve", UZmodel::reserve_model ());
    syntax.add ("transport", Librarian<Transport>::library (), 
                "Solute transport model in matrix.");
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
 
    Librarian<Movement>::add_type ("rectangle", alist, syntax, &make);
  }
} MovementRect_syntax;
