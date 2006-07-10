// movement_1D.C --- Movement in a 1D system.
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
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat1d.h"
#include "macro.h"
#include "groundwater.h"
#include "surface.h"
#include "solute.h"
#include "element.h"
#include "log.h"
#include "submodeler.h"
#include <sstream>

struct Movement1D : public Movement
{
  // Geometry & heat.
  std::auto_ptr<Geometry1D> geo;
  std::auto_ptr<SoilHeat1D> heat;

  // Water.
  std::auto_ptr<UZmodel> uzdefault;
  std::auto_ptr<UZmodel> uzreserve;
  std::auto_ptr<Macro> macro;

  // Solute.
  std::auto_ptr<Transport> transport; // Solute transport model in matrix.
  std::auto_ptr<Transport> reserve; // Reserve solute transport model in matr.
  std::auto_ptr<Transport> last_resort; // Last resort solute transport model.
  std::auto_ptr<Transport> transport_solid; // Pseudo transport for non-solutes
  std::auto_ptr<Mactrans> mactrans; // Solute transport model in macropores.

  // Simulation.
  Geometry& geometry () const
  { return *geo; }
  SoilHeat& soil_heat () const
  { return *heat; }

  void macro_tick (const Soil& soil, SoilWater& soil_water,
                   Surface& surface, Treelog& msg)
  { 
    if (!macro.get ())			// No macropores.
      return;

    // Calculate preferential flow first.
    std::fill (soil_water.S_p_.begin (), soil_water.S_p_.end (), 0.0);
    std::fill (soil_water.q_p_.begin (), soil_water.q_p_.end (), 0.0);
    macro->tick (*geo, soil, 0, soil.size () - 1, surface, 
                 soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
                 soil_water.S_sum_, soil_water.S_p_, soil_water.q_p_, msg);
  }

  void tick (const Soil& soil, SoilWater& soil_water,
             Surface& surface, Groundwater& groundwater,
             const Time& time, const Weather& weather, Treelog& msg) 
  {
    Treelog::Open nest (msg, "Movement: " + name.name ());

    heat->tick (time, *geo, soil, soil_water, surface, weather);
    soil_water.tick (geo->cell_size (), soil, msg);
    tick_water (*geo, soil, *heat, surface, groundwater, 
                soil_water.S_sum_, soil_water.h_old_, soil_water.Theta_old_,
                soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
                soil_water.q_, soil_water.q_p_,
                msg);
  }

  void tick_water (const Geometry1D& geo,
                   const Soil& soil, const SoilHeat& soil_heat, 
                   Surface& surface, Groundwater& groundwater,
                   const std::vector<double>& S,
                   std::vector<double>& h_old,
                   const std::vector<double>& Theta_old,
                   const std::vector<double>& h_ice,
                   std::vector<double>& h,
                   std::vector<double>& Theta,
                   std::vector<double>& q,
                   std::vector<double>& q_p,
                   Treelog& msg)
  {
    // Limit for groundwater table.
    size_t last  = soil.size () - 1;
    if (groundwater.bottom_type () == Groundwater::pressure)
      {
        daisy_assert (soil.size () > 1);
        if (groundwater.table () <= geo.zplus (soil.size () - 2))
          throw ("Groundwater table in or below lowest cell.");
        last = geo.interval_plus (groundwater.table ());
        if (last >=  soil.size () - 1)
          daisy_assert ("Groundwater too low.");
        // Pressure at the last cell is equal to the water above it.
        for (size_t i = last + 1; i < soil.size (); i++)
          {
            h_old[i] = groundwater.table () - geo.z (i);
            h[i] = groundwater.table () - geo.z (i);
          }
      }

    // Limit for ridging.
    const size_t first = (surface.top_type (geo, 0U) == Surface::soil)
      ? surface.last_cell (geo, 0U) 
      : 0U;
    bool ok = true;

    // Calculate matrix flow next.
    try
      {
        ok = uzdefault->tick (msg, geo, soil, soil_heat,
                              first, surface, 0U, last, groundwater,
                              S, h_old, Theta_old, h_ice, h, Theta, 0U, q);
      }
    catch (const char* error)
      {
        msg.warning (std::string ("UZ problem: ") + error);
        ok = false;
      }
    catch (const std::string& error)
      {
        msg.warning (std::string ("UZ problem: ") + error);
        ok = false;
      }
    if (!ok)
      {
        msg.message ("Using reserve uz model.");
        uzreserve->tick (msg, geo, soil, soil_heat,
                         first, surface, 0U, last, groundwater,
                         S, h, Theta_old, h_ice, h, Theta, 0U, q);
      }

    for (size_t i = last + 2; i <= soil.size (); i++)
      {
        q[i] = q[i-1];
        q_p[i] = q_p[i-1];
      }

    // Update Theta below groundwater table.
    if (groundwater.bottom_type () == Groundwater::pressure)
      {
        for(size_t i = last + 1; i < soil.size (); i++)
          Theta[i] = soil.Theta (i, h[i], h_ice[i]);
      }

    // Update surface and groundwater reservoirs.
    surface.accept_top (q[0] * dt, geo, 0U, msg);
    groundwater.accept_bottom ((q[soil.size ()] + q_p[soil.size ()]) * dt,
                               geo, soil.size ());
  }


  void ridge (Surface& surface, const Soil& soil, const SoilWater& soil_water,
              const AttributeList& al)
  { surface.ridge (*geo, soil, soil_water, al); }

  void solute (const Soil& soil, 
               const SoilWater& soil_water, 
               const double J_in, Solute& solute, 
               Treelog& msg)
  { 
    solute.tick (geo->cell_size (), soil_water);

    // Upper border.
    if (soil_water.q_p (0) < 0.0)
      {
        if (soil_water.q (0) >= 0.0)
          {
            if (soil_water.q (0) > 1.0e-10)
              {
                std::ostringstream tmp;
                tmp << "BUG: q_p[0] = " << soil_water.q_p (0) 
                    << " and q[0] = " << soil_water.q (0);
                msg.error (tmp.str ());
              }
            solute.J_p[0] = J_in;
            solute.J[0] = J_in;
          }
        else
          {
            const double macro_fraction
              = soil_water.q_p (0) / (soil_water.q_p (0) + soil_water.q (0));
            solute.J_p[0] = J_in * macro_fraction;
            solute.J[0] = J_in - solute.J_p[0];
          }
      }
    else
      solute.J[0] = J_in;

    // Flow.
    flow (soil, soil_water, solute.submodel, 
          solute.M_, solute.C_, 
          solute.S, solute.S_p,
          solute.J, solute.J_p, 
          *solute.adsorption, solute.diffusion_coefficient (), msg);
  }

  void element (const Soil& soil, 
                const SoilWater& soil_water, 
                Element& element,
                Adsorption& adsorption,
                double diffusion_coefficient,
                Treelog& msg)
  {
    element.tick (geo->cell_size (), soil_water);
    flow (soil, soil_water, "DOM", element.M, element.C, 
          element.S, element.S_p, element.J, element.J_p, 
          adsorption, diffusion_coefficient, msg);
  }

  void flow (const Soil& soil, const SoilWater& soil_water, 
             const std::string& name,
             std::vector<double>& M, 
             std::vector<double>& C, 
             std::vector<double>& S, 
             std::vector<double>& S_p, 
             std::vector<double>& J, 
             std::vector<double>& J_p, 
             Adsorption& adsorption,
             double diffusion_coefficient,
             Treelog& msg)
  {
    const double old_content = geo->total (M);

    // Flow.
    if (adsorption.full ())
      transport_solid->tick (msg, *geo, soil, soil_water, adsorption, 
                            diffusion_coefficient, M, C, S, J);
    else
      {
        mactrans->tick (*geo, soil_water, M, C, S, S_p, J_p, msg);

        try
          {
            transport->tick (msg, *geo, soil, soil_water, adsorption, 
                             diffusion_coefficient, 
                             M, C, S, J);
          }
        catch (const char* error)
          {
            msg.warning (std::string ("Transport problem: ") + error +
                         ", trying reserve.");
            try
              {
                reserve->tick (msg, *geo, soil, soil_water, adsorption, 
                               diffusion_coefficient, M, C, S, J);
              }
            catch (const char* error)
              {
                msg.warning (std::string ("Reserve transport problem: ") 
                             + error + ", trying last resort.");
                last_resort->tick (msg, *geo, soil, soil_water, adsorption, 
                                   diffusion_coefficient, M, C, S, J);
              }
          }
      }
    const double new_content = geo->total (M);
    const double delta_content = new_content - old_content;
    const double source = geo->total (S);
    const double in = -J[0];	// No preferential transport, it is 
    const double out = -J[geo->edge_size () - 1]; // included in S.
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

  void output (Log& log) const
  { output_submodule (*heat, "Heat", log); }

  // Create.
  bool check (Treelog& err) const;
  void initialize (const AttributeList& alist,
                   const Soil& soil, const Groundwater& groundwater,
                   const Time& time, const Weather& weather,
                   Treelog& msg)
  {
    Treelog::Open nest (msg, "Movement: " + name.name ());
    heat->initialize (alist.alist ("Heat"), 
                      *geo, soil, time, weather, msg);
    initialize_macro (alist, soil, groundwater, msg);
  }

  void initialize_macro (const AttributeList& al,
                         const Soil& soil,
                         const Groundwater& groundwater, 
                         Treelog& msg)
  {
    const size_t cell_size = geo->cell_size ();

    // Initialize base (requires ice!).

    // Macropores.
    if (al.check ("macro"))
      macro.reset (Librarian<Macro>::build_free (msg, al.alist ("macro"), 
                                                 "macro"));
    else if (soil.humus (0) + soil.clay (0) > 0.05)
      // More than 5% clay (and humus) in first horizon.
      {
        // Find first non-clay layer.
        size_t lay = 1;
        while (lay < cell_size && soil.humus (lay) + soil.clay (lay) > 0.05)
          lay++;

        // Don't go below 1.5 m.
        double height = std::max (geo->zplus (lay-1), -150.0);

        // Don't go below drain pipes.
        if (groundwater.is_pipe ())
          height = std::max (height, groundwater.pipe_height ());

        // Add them.
        macro = Macro::create (height);

        msg.debug ("Adding macropores");
      }

    // Let 'macro' choose the default method to average K values in 'uz'.
    const bool has_macropores = (macro.get () && !macro->none ());
    uzdefault->has_macropores (has_macropores);
    uzreserve->has_macropores (has_macropores);
  }

  Movement1D (Block& al)
    : Movement (al),
      geo (submodel<Geometry1D> (al, "Geometry")),
      heat (submodel<SoilHeat1D> (al, "Heat")),
      uzdefault (Librarian<UZmodel>::build_item (al, "UZdefault")),
      uzreserve (Librarian<UZmodel>::build_item (al, "UZreserve")),
      macro (NULL),
      transport (Librarian<Transport>::build_item (al, "transport")),
      reserve (Librarian<Transport>::build_item (al, "transport_reserve")),
      last_resort (Librarian<Transport>::build_item (al, 
                                                     "transport_last_resort")),
      transport_solid (Librarian<Transport>::build_item (al,
                                                         "transport_solid")),
      mactrans  (Librarian<Mactrans>::build_item (al, "mactrans"))
  { }
};

bool
Movement1D::check (Treelog& err) const
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

void 
Movement::load_vertical (Syntax& syntax, AttributeList& alist)
{
   syntax.add_submodule ("Geometry", alist, Syntax::State,
                         "Discretization of the soil.",
                         Geometry1D::load_syntax);
   syntax.add_submodule ("Heat", alist, Syntax::State,
                         "Soil heat and flux.",
                         SoilHeat1D::load_syntax);
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
   syntax.add ("macro", Librarian<Macro>::library (),
               Syntax::OptionalState, Syntax::Singleton,
               "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
   syntax.add ("mactrans", Librarian<Mactrans>::library (), 
               "Solute transport model in macropores.");
   alist.add ("mactrans", Mactrans::default_model ());
}

const AttributeList& 
Movement::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      Movement::load_vertical (dummy, alist);
      alist.add ("type", "vertical");
    }
  return alist;
}

Movement*
Movement::build_vertical (Block& al)
{ return new Movement1D (al); }

static struct Movement1DSyntax
{
  static Movement& make (Block& al)
  { return *new Movement1D (al); }

  Movement1DSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "One dimensional movement.");
    Movement::load_vertical (syntax, alist);
 
    Librarian<Movement>::add_type ("vertical", alist, syntax, &make);
  }
} Movement1D_syntax;
