// movement_rect.C --- Movement in a rectangular 2D grid.
// 
// Copyright 2006, 2008 Per Abrahamsen and KVL.
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
#include "movement_solute.h"
#include "geometry_rect.h"
#include "heatrect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "transport.h"
#include "chemical.h"
#include "groundwater.h"
#include "surface.h"
#include "weather.h"
#include "uzrect.h"
#include "adsorption.h"
#include "log.h"
#include "check.h"
#include "frame_submodel.h"
#include "submodeler.h"
#include "tertiary.h"
#include "librarian.h"
#include "anystate.h"
#include "treelog.h"
#include "mathlib.h"
#include "block_model.h"
#include "point.h"
#include "depth.h"
#include <sstream>

struct MovementRect : public MovementSolute
{
  // Geometry.
  std::unique_ptr<GeometryRect> geo;
  Geometry& geometry () const;

  // Failures.
  std::set<symbol> seen;
  void report (const symbol error, Treelog& msg);
  void summarize (Treelog& msg) const;

  // Drains
  const auto_vector<const ZXPoint*> drain_position;
  std::vector<size_t> drain_cell;
  std::unique_ptr<Depth> pipe_outlet; // Water level at pipe outlet. [cm]

  // Water.
  const auto_vector<UZRect*> matrix_water;

  // Management.
  void ridge (Surface&, const Soil&, const SoilWater&, const FrameSubmodel&);

  // Heat.
  std::unique_ptr<Heatrect> heatrect;

  /* const */ double delay;	// Period delay [ cm/rad ??? ]
  double bottom_heat (const Time&, const Weather&) const ;
  std::vector<double> default_heat (const Soil&, 
                                    const Time&, const Weather&);
  double surface_snow_T (const Soil&, const SoilWater&, const SoilHeat&,
                         const double T_snow, const double K_snow,
                         const double dZs) const;
  void heat (const std::vector<double>& q_water,
	     const std::vector<double>& S_water,
	     const std::vector<double>& S_heat,
	     const std::vector<double>& capacity_new,
	     const std::vector<double>& conductivity,
	     double T_top, double T_top_new, double T_bottom,
	     std::vector<double>& T,
	     const double dt, Treelog&) const;

  // Simulation.
  void tick (const Soil& soil, SoilWater& soil_water, const SoilHeat& soil_heat,
             Surface& surface, Groundwater&, const Time&, const Scope&, 
             const Weather&, double dt, Treelog& msg);
  void output (Log& log) const;

  // Create.
  void initialize_derived (const Time&, const Scope&,
			   const Soil&, const Groundwater&, 
                           bool has_macropores, Treelog&);
  MovementRect (const BlockModel& al);
  ~MovementRect ();
};

Geometry& 
MovementRect::geometry () const
{ return *geo; }

void
MovementRect::report (const symbol error, Treelog& msg)
{
  if (seen.find (error) != seen.end ())
    msg.debug ("UZ problem: " + error);
  else 
    {
      seen.insert (error);
      msg.message ("UZ problem: " + error);
      msg.message ("\
Further messages of this will only be shown in the daisy.log file.");
    }
}

void 
MovementRect::summarize (Treelog& msg) const
{
  Movement::summarize (msg);
  TREELOG_MODEL (msg);
  for (size_t i = 0; i < matrix_water.size (); i++)
    {
      Treelog::Open nest (msg, "matrix_water", i, objid);
      matrix_water[i]->summarize (msg);
    }
}

void
MovementRect::ridge (Surface&, const Soil&, const SoilWater&, 
                     const FrameSubmodel&)
{ throw "Can't make ridges on a rectangular grid"; }

double 
MovementRect::bottom_heat (const Time& time, const Weather& weather) const 
{ return weather.T_normal (time, delay); }

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
  const double Z = -geo->cell_z (0) / 100.0; // [cm] -> [m]
  const double T_soil
    = geo->content_height (soil_heat, &SoilHeat::T, Z); // [dg C]

  return (K_soil / Z * T_soil + K_snow / dZs * T_snow) 
    / (K_soil / Z + K_snow / dZs);
}

void 
MovementRect::heat (const std::vector<double>& q_water,
		    const std::vector<double>& S_water,
		    const std::vector<double>& S_heat,
		    const std::vector<double>& capacity_new,
		    const std::vector<double>& conductivity,
		    const double T_top,
		    const double T_top_new,
                    const double T_bottom, 
		    std::vector<double>& T,
		    const double dt, Treelog& msg) const
{
  heatrect->solve (*geo, q_water, S_water, S_heat,
                   capacity_new, conductivity, 
                   T_top, T_top_new, T_bottom, T, dt, msg);
}

void
MovementRect::tick (const Soil& soil, SoilWater& soil_water, 
                    const SoilHeat& soil_heat,
                    Surface& surface, Groundwater& groundwater, 
                    const Time& time, const Scope& scope, 
                    const Weather& weather, 
                    const double dt, Treelog& msg) 
{
  pipe_outlet->tick (time, scope, msg);
  const double dwl = pipe_outlet->operator ()();
  
  const size_t edge_size = geo->edge_size ();

  for (size_t i = 0; i < matrix_water.size (); i++)
    {
      water_attempt (i);
      Treelog::Open nest (msg, matrix_water[i]->objid);
      try
        {
          matrix_water[i]->tick (*geo, drain_cell, dwl,
				 soil, soil_water, soil_heat,
                                 surface, groundwater, dt, msg);
	  const bool obey_surface = matrix_water[i]->obey_surface ();

          for (size_t edge = 0; edge < edge_size; edge++)
            {
              if (geo->edge_to (edge) == Geometry::cell_above)
                {
                  const double q_up = obey_surface
                    ? soil_water.q_matrix (edge)
                    : surface.q_top (*geo, edge, dt);

                  surface.accept_top (q_up * dt, *geo, edge, dt, msg);
                  surface.update_pond_average (*geo);
                }
              if (geo->edge_from (edge) == Geometry::cell_below)
                {
                  const double q_down = soil_water.q_matrix (edge) 
                    + soil_water.q_tertiary (edge);
                  groundwater.accept_bottom (q_down * dt, *geo, edge);
                }
            }
          if (i > 0)
            msg.debug ("Reserve model succeeded");
          return;
        }
      catch (const char* error)
        { report (error, msg); }
      catch (const std::string& error)
        { report (error, msg); }

      water_failure (i);
    }
  throw "Matrix water transport failed";
}

void 
MovementRect::output (Log& log) const
{ 
  output_solute (log);
  output_list (matrix_water, "matrix_water", log, UZRect::component);
  // output_submodule (*geo, "Geometry", log);
}

void 
MovementRect::initialize_derived (const Time& time, const Scope& scope,
				  const Soil&, const Groundwater&, 
                                  const bool has_macropores, Treelog& msg)
{
  pipe_outlet->initialize (time, scope, msg);

  for (size_t i = 0; i < matrix_water.size (); i++)
    matrix_water[i]->initialize (geometry (), has_macropores);
}

MovementRect::MovementRect (const BlockModel& al)
  : MovementSolute (al),
    geo (submodel<GeometryRect> (al, "Geometry")),
    drain_position (map_submodel_const<ZXPoint> 
                    (al, "drainpoints")),
    pipe_outlet (Librarian::build_item<Depth> (al, "pipe_outlet")),
    matrix_water (Librarian::build_vector<UZRect> (al, "matrix_water")),
    heatrect (Librarian::build_item<Heatrect> (al, "heat"))
{ 
  for (size_t i = 0; i < drain_position.size (); i++)
    {
      const ZXPoint& point = *drain_position[i];
      const double z = point.z;
      const double x = point.x;
      const double y = 0.5;
      if (z >= geo->top () || z <= geo->bottom ()
	  || x <= geo->left () || x >= geo->right ()
	  || y < geo->front () || y >= geo->back ())
	al.error ("Drain cell placed at or outside soil boundary");
      else
	{
	  const int cell = geo->cell_at (z, x, y);
	  std::ostringstream tmp;
	  tmp << "cell[" << cell << "] named " << geo->cell_name (cell)
	      << " at (" << z << ", " << x << ", " << y << ") is a drain cell";
	  al.msg ().debug (tmp.str ());
	  drain_cell.push_back (cell);
	}
    }
}

MovementRect::~MovementRect ()
{ }

static struct MovementRectSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new MovementRect (al); }

  MovementRectSyntax ()
    : DeclareModel (Movement::component, "rectangle", "solute",
                    "Two dimensional movement in a rectangular grid.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("matrix_solute", "Mollerup", "convection", "none");
    frame.set ("Tertiary", "none");
    frame.declare_submodule ("Geometry", Attribute::Const,
                          "Discretization of the soil.",
                          GeometryRect::load_syntax);
    frame.declare_submodule_sequence ("drainpoints", Attribute::Const,
				   "Location of cells with drain pipes.",
				   ZXPoint::load_syntax);
    frame.set_empty ("drainpoints");
    frame.declare_object ("pipe_outlet", Depth::component,
                          Attribute::Const, Attribute::Singleton, "\
Water table in drain pipe outlet.\n\
\n\
By default this will be considered deep enough to ensure free\n\
flow of water out of drains. If higher than the drain pipes,\n\
water may flow either way depending on the pressure in the soil.");
    frame.set ("pipe_outlet", "deep");
    frame.declare_object ("matrix_water", UZRect::component, 
                          Attribute::State, Attribute::Variable,
                       "Matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    frame.set_strings ("matrix_water", "Mollerup", "v+h", "const");
    frame.declare_object ("heat", Heatrect::component, 
                       Attribute::Const, Attribute::Singleton, "\
Heat transport model.");
    frame.set ("heat", "Mollerup");

  }
} MovementRect_syntax;

// movement_rect.C ends here.
