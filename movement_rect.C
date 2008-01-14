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

#define BUILD_DLL
#include "movement.h"
#include "geometry_rect.h"
#include "heat_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "msoltranrect.h"
#include "chemical.h"
#include "groundwater.h"
#include "surface.h"
#include "weather.h"
#include "uzrect.h"
#include "adsorption.h"
#include "check.h"
#include "alist.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"

struct MovementRect : public Movement
{
  // Geometry.
  std::auto_ptr<GeometryRect> geo;
  Geometry& geometry () const;

  // Drains
  struct Point;
  const auto_vector<const Point*> drain_position;
  std::vector<size_t> drain_cell;

  // Water.
  const auto_vector<UZRect*> matrix_water;
  void macro_tick (const Soil&, SoilWater&, Surface&, double dt, Treelog&);

  // Solute.
  const auto_vector<Msoltranrect*> matrix_solute;
  const std::auto_ptr<Msoltranrect> matrix_solid;
  void solute (const Soil& soil, const SoilWater& soil_water,
               double J_in, Chemical&, const bool flux_below, 
	       double dt, const Scope&, Treelog&);
  void element (const Soil& soil, const SoilWater& soil_water,
                DOE& element, 
		double diffusion_coefficient, double dt, Treelog& msg);

  // Management.
  void ridge (Surface&, const Soil&, const SoilWater&, const AttributeList&);

  // Heat.
  std::auto_ptr<HeatRect> heat_rect;

  /* const */ double delay;	// Period delay [ cm/rad ??? ]
  double T_bottom;		// [dg C]
  double bottom_heat (const Time&, const Weather&) const ;
  double bottom_T () const;
  std::vector<double> default_heat (const Soil&, 
                                    const Time&, const Weather&);
  double surface_snow_T (const Soil&, const SoilWater&, const SoilHeat&,
                         const double T_snow, const double K_snow,
                         const double dZs) const;
  void heat (const std::vector<double>& q_water,
	     const std::vector<double>& S_water,
	     const std::vector<double>& S_heat,
	     const std::vector<double>& capacity_old,
	     const std::vector<double>& capacity_new,
	     const std::vector<double>& conductivity,
	     double T_top,
	     double T_top_new,
	     std::vector<double>& T,
	     const double dt, Treelog&) const;

  // Simulation.
  void tick (const Soil& soil, SoilWater& soil_water, const SoilHeat& soil_heat,
             Surface& surface, Groundwater& groundwater, const Time&,
             const Weather&, double dt, Treelog& msg);
  void output (Log&) const;

  // Create.
  bool check (Treelog&) const;
  void initialize (const Soil&, const Groundwater&, Treelog&);
  MovementRect (Block& al);
  ~MovementRect ();
};

Geometry& 
MovementRect::geometry () const
{ return *geo; }

struct MovementRect::Point 
{
  const double z;
  const double x;
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add ("z", "cm", Check::negative (), Syntax::Const, 
		"Vertical position.");
    syntax.add ("x", "cm", Check::positive (), Syntax::Const,
		"Horizontal position.");
    syntax.order ("z", "x");
  }
  Point (const AttributeList& al)
    : z (al.number ("z")),
      x (al.number ("x"))
  { }
};

void
MovementRect::macro_tick (const Soil&, SoilWater&, Surface&, 
                          const double /* dt */, Treelog&)
{ }

void
MovementRect::solute (const Soil& soil, const SoilWater& soil_water,
                      const double J_in, Chemical& chemical, 
		      const bool flux_below, 
		      const double dt,
                      const Scope& scope, Treelog& msg)
{
  if (chemical.adsorption ().full ())
    {
      matrix_solid->solute (*geo, soil, soil_water, J_in, chemical,
			    flux_below, dt, scope, msg);
      return;
    }
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, matrix_solute[i]->name);
      try
        {
          matrix_solute[i]->solute (*geo, soil, soil_water, J_in, chemical, 
                                    flux_below, dt, scope, msg);
          if (i > 0)
            msg.message ("Succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("Solute problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("Solute trouble: ") + error);
        }
    }
  throw "Matrix solute transport failed";
}

void 
MovementRect::element (const Soil& soil, const SoilWater& soil_water,
                       DOE& element, 
                       const double diffusion_coefficient, double dt, 
                       Treelog& msg)
{
  for (size_t i = 0; i < matrix_solute.size (); i++)
    {
      Treelog::Open nest (msg, matrix_solute[i]->name);
      try
        {
          matrix_solute[i]->element (*geo, soil, soil_water, element, 
                                     diffusion_coefficient, dt, 
                                     msg);
          if (i > 0)
            msg.message ("Succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("DOM problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("DOM trouble: ") + error);
        }
    }
  throw "Matrix element transport failed";
}

void
MovementRect::ridge (Surface&, const Soil&, const SoilWater&, 
                     const AttributeList&)
{ throw "Can't make ridges on a rectangular grid"; }

double 
MovementRect::bottom_heat (const Time& time, const Weather& weather) const 
{ return weather.T_normal (time, delay); }

double
MovementRect::bottom_T () const
{ return T_bottom; }

std::vector<double> 
MovementRect::default_heat (const Soil& soil, 
                            const Time& time, const Weather& weather)
{
  // Fetch average temperatur.
  const double rad_per_day = 2.0 * M_PI / 365.0;

  // Calculate delay.
  const double pF_2_0 = -100.0;
  double k = 0;
  double C = 0;

  std::vector<double> T;
  T.insert (T.begin (), geo->cell_size (), -42.42e42);

  const size_t cell_rows = geo->cell_rows ();
  const size_t cell_columns = geo->cell_columns ();
  
  for (size_t row = 0; row < cell_rows; row++)
    {
      for (size_t column = 0; column < cell_columns; column++)
        {
          const size_t cell = geo->cell_index (row, column);
          const double volume = geo->cell_volume (cell);
          const double Theta_pF_2_0 = soil.Theta (cell, pF_2_0, 0.0);
          k += volume * soil.heat_conductivity (cell, Theta_pF_2_0, 0.0);
          C += volume * soil.heat_capacity (cell, Theta_pF_2_0, 0.0);
        }
      const double depth = geo->zplus (geo->cell_index (row, 0));
      const double a = k / C;
      delay = depth / sqrt (24.0 * 2.0 * a / rad_per_day);
      const double heat = bottom_heat (time, weather);

      for (size_t column = 0; column < cell_columns; column++)
        T[geo->cell_index (row, column)] = heat;
    }
  return T;
}

double
MovementRect::surface_snow_T (const Soil& soil,
                              const SoilWater& soil_water,
                              const SoilHeat& soil_heat,
                              const double T_snow,
                              const double K_snow,
                              const double dZs) const
{
  // We just use the first cell, ignore rest of surface.

  // Information about soil.
  const double K_soil 
    = soil.heat_conductivity (0, soil_water.Theta (0),
                              soil_water.X_ice (0)) 
    * 1e-7 * 100.0 / 3600.0; // [erg/cm/h/dg C] -> [W/m/dg C]
  const double Z = -geo->z (0) / 100.0; // [cm] -> [m]
  const double T_soil = geo->content_at (soil_heat, &SoilHeat::T, Z); // [dg C]

  return (K_soil / Z * T_soil + K_snow / dZs * T_snow) 
    / (K_soil / Z + K_snow / dZs);
}

void 
MovementRect::heat (const std::vector<double>& q_water,
		    const std::vector<double>& S_water,
		    const std::vector<double>& S_heat,
		    const std::vector<double>& capacity_old,
		    const std::vector<double>& capacity_new,
		    const std::vector<double>& conductivity,
		    const double T_top,
		    const double T_top_new,
		    std::vector<double>& T,
		    const double dt, Treelog& msg) const
{
  heat_rect->solve (*geo, q_water, S_water, S_heat,
		    capacity_old, capacity_new, conductivity, 
		    T_top, T_top_new, T_bottom, T, dt, msg);
}

void
MovementRect::tick (const Soil& soil, SoilWater& soil_water, 
                    const SoilHeat& soil_heat,
                    Surface& surface, Groundwater& groundwater, 
                    const Time& time,
                    const Weather& weather, const double dt, Treelog& msg) 
{
  T_bottom = bottom_heat (time, weather);

  const size_t cell_size = geo->cell_size ();
  const size_t edge_size = geo->edge_size ();

  soil_water.tick (cell_size, soil, dt, msg); 

  bool obey_surface;
 
  for (size_t i = 0; i < matrix_water.size (); i++)
    {
      Treelog::Open nest (msg, matrix_water[i]->name);
      try
        {
          matrix_water[i]->tick (*geo, drain_cell, soil, soil_water, soil_heat,
                                 surface, groundwater, dt, msg);
	  obey_surface = matrix_water[i]->obey_surface ();
          goto update_borders;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("UZ problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("UZ trouble: ") + error);
        }
    }
  throw "Matrix water transport failed";

  // Update surface and groundwater reservoirs.
 update_borders:
  for (size_t edge = 0; edge < edge_size; edge++)
    {
      if (geo->edge_to (edge) == Geometry::cell_above)
	{
	  if (obey_surface)
	    surface.accept_top (soil_water.q (edge) * dt, *geo, edge, dt, msg);
	  else
	    surface.accept_top (surface.q_top (*geo, edge), *geo, edge, dt, msg);
	}
      if (geo->edge_from (edge) == Geometry::cell_below)
        groundwater.accept_bottom ((soil_water.q (edge)
                                    + soil_water.q_p (edge)) * dt,
                                   *geo, edge);
    }
}

void 
MovementRect::output (Log&) const
{ }

bool
MovementRect::check (Treelog&) const
{ return true; }

void 
MovementRect::initialize (const Soil&, const Groundwater&, Treelog&)
{
  const bool has_macropores = false;
  for (size_t i = 0; i < matrix_water.size (); i++)
    matrix_water[i]->has_macropores (has_macropores);
}

MovementRect::MovementRect (Block& al)
  : Movement (al),
    geo (submodel<GeometryRect> (al, "Geometry")),
    drain_position (map_construct_const<Point> (al.alist_sequence ("drain"))),
    matrix_water (Librarian::build_vector<UZRect> (al, "matrix_water")),
    matrix_solute (Librarian::build_vector<Msoltranrect> 
                   (al, "matrix_solute")),
    matrix_solid (Librarian::build_item<Msoltranrect>
		  (al, "matrix_solid")),
    heat_rect (submodel<HeatRect> (al, "heat")),
    T_bottom (-42.42e42)
{ 
  for (size_t i = 0; i < drain_position.size (); i++)
    {
      const Point& point = *drain_position[i];
      const double z = point.z;
      const double x = point.x;
      const double y = 0.5;
      if (z >= geo->top () || z <= geo->bottom ()
	  || x <= geo->left () || x >= geo->right ()
	  || y < geo->front () || y >= geo->back ())
	al.error ("Drain cell placed at or outside soil boundary");
      else
	drain_cell.push_back (geo->cell_at (z, x, y));
    }
}

MovementRect::~MovementRect ()
{ }

static struct MovementRectSyntax
{
  static Model& make (Block& al)
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
    syntax.add_submodule_sequence ("drain", Syntax::Const,
				   "Location of cells with drain pipes.",
				   MovementRect::Point::load_syntax);
    alist.add ("drain", std::vector<const AttributeList*> ());
    syntax.add_object ("matrix_water", UZRect::component, 
                       Syntax::Const, Syntax::Sequence,
                       "Matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    std::vector<const AttributeList*> matrix_water_models;
    AttributeList matrix_water_reserve (UZRect::reserve_model ());
    matrix_water_models.push_back (&matrix_water_reserve);
    alist.add ("matrix_water", matrix_water_models);
    syntax.add_object ("matrix_solute", Msoltranrect::component, 
                       Syntax::Const, Syntax::Sequence,
                       "Matrix solute transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    std::vector<const AttributeList*> matrix_solute_models;
    AttributeList matrix_solute_default (Msoltranrect::default_model ());
    matrix_solute_models.push_back (&matrix_solute_default);
    AttributeList matrix_solute_reserve (Msoltranrect::reserve_model ());
    matrix_solute_models.push_back (&matrix_solute_reserve);
    alist.add ("matrix_solute", matrix_solute_models);

    syntax.add_object ("matrix_solid", Msoltranrect::component, 
                       Syntax::Const, Syntax::Singleton, "\
Matrix solute transport model used for fully sorbed constituents.");
    alist.add ("matrix_solid", Msoltranrect::none_model ());
    syntax.add_submodule ("heat", alist, Syntax::Const,
                          "Heat transport.", HeatRect::load_syntax);


    Librarian::add_type (Movement::component, "rectangle",
                         alist, syntax, &make);
  }
} MovementRect_syntax;
