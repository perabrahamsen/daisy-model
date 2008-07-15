// groundwater_aquitard.C --- Free drainage.
// 
// Copyright 2008 Mikkel Mollerup, Per Abrahamsen and KU.
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

#include "groundwater.h"
#include "syntax.h"
#include "alist.h"
#include "block.h"
#include "librarian.h"
#include "check.h"
#include "assertion.h"
#include "depth.h"
#include "geometry.h"
#include "soil_water.h"
#include "log.h"
#include <map>
#include <sstream>

#include <iostream>   //mmo

struct GroundwaterAquitard : public Groundwater
{
  // Parameters
  const double K_aquitard;  // Conductivity of the aquitard. [cm h^-1]
  const double Z_aquitard;    // Vertical length of the aquitard. [cm]
  std::auto_ptr<Depth> pressure_table; // Virtual groundwater height. [cm]
  double h_aquifer;          // Pressure potential in the aquifer [cm]

  // Utility.
  void set_h_aquifer (const Geometry& geo)
  {
    const double aquitard_bottom = geo.bottom () - Z_aquitard;
    h_aquifer = pressure_table->operator()() - aquitard_bottom;
  }

  // Bottom flux.
  typedef std::map<size_t, double> edge_flux_map;
  edge_flux_map edge_flux;
  bottom_t bottom_type() const
  { return forced_flux; }
  double q_bottom (size_t edge) const
  {
    edge_flux_map::const_iterator i = edge_flux.find (edge);
    daisy_assert (i != edge_flux.end ());
    return (*i).second;
  }
    
  // Simulation.
  void tick (const Geometry& geo,
             const Soil&, SoilWater& soil_water, double, 
	     const SoilHeat&, const Time& time, 
             const Scope& scope, Treelog& msg)
  { 
    // Virtual pressure table.
    pressure_table->tick (time, scope, msg);
    set_h_aquifer (geo);

    // Deep percolation.
    const std::vector<size_t>& bottom_edges 
      = geo.cell_edges (Geometry::cell_below);
    const size_t bottom_edges_size = bottom_edges.size ();

    for (size_t i = 0; i < bottom_edges_size; i++)
      {
        const size_t edge = bottom_edges[i];
        const int cell = geo.edge_other (edge, Geometry::cell_below);
        daisy_assert (geo.cell_is_internal (cell));
        const double in_sign = (geo.edge_to (edge) == cell) ? 1.0 : -1.0;

        // Multiplied with 2 because it is a boundary cell...
        const double Dz_i = 2 * geo.edge_length (edge);  
    
        const double K_i = soil_water.K (cell);   //Conductivity in cell
        const double h_i = soil_water.h (cell);   //Pressure in cell
        

        const double numerator = K_i * (2.0 * h_i / Dz_i + 1.0)
          + K_aquitard * (h_aquifer / Z_aquitard - 1.0);
        const double denominator = K_aquitard + 2.0 * K_i * Z_aquitard / Dz_i;
    
        // Flux into domain.
        const double q_up = -K_aquitard * (numerator / denominator 
                                           - h_aquifer / Z_aquitard + 1.0);
        edge_flux[edge] = in_sign * q_up;

#if 0
        Treelog::Open nest (msg, geo.edge_name (edge));
        std::ostringstream tmp;
        tmp << "cell = " << geo.cell_name (cell) << "\n"
            << "in_sign = " << in_sign << "\n"
            << "Dz_i = " <<  Dz_i << "\n"
            << "K_i = " << K_i << "\n"
            << "h_i = " << h_i << "\n"
            << "numerator = " << numerator << "\n"
            << "denominator = " << denominator << "\n"
            << "q_up = " << q_up << "\n"
            << "h_aquifer = " << h_aquifer << "\n"
            << "Z_aquitard = " << Z_aquitard << "\n"
            << "K_aquitard = " << K_aquitard;
        msg.message (tmp.str ());
#endif
      }
  }
  void output (Log& log) const
  {
    Groundwater::output (log);
    output_variable (h_aquifer, log);
  }

  double table () const
  { return pressure_table->operator()(); }

  // Create and Destroy.
  void initialize (const Geometry& geo, const Time&,
                   const Scope& scope, Treelog& msg)
  {
    if (!pressure_table.get ())
      pressure_table.reset (Depth::create ((geo.bottom () - Z_aquitard)
                                           + h_aquifer));
    pressure_table->initialize (msg);
    // Pressure below aquitard.
    if (pressure_table->check (scope, msg))
      set_h_aquifer (geo);
    else
      pressure_table.reset (NULL);
  }
  bool check (const Geometry& geo, const Scope& scope,
              Treelog& msg) const
  {
    bool ok = true;
    if (!pressure_table.get ())
      {
        ok = false;
        msg.error ("No pressure table");
      }
    else if (!pressure_table->check (scope, msg))
      ok = false;
    return ok;
  }
  GroundwaterAquitard (Block& al)
    : Groundwater (al),
      K_aquitard (al.number ("K_aquitard")),
      Z_aquitard (al.number ("Z_aquitard")),
      pressure_table (al.check ("pressure_table")
                      ? Librarian::build_item<Depth> (al, "pressure_table")
                      : NULL),
      h_aquifer (al.number ("h_aquifer", Z_aquitard))
  {}
  ~GroundwaterAquitard ()
  { }
};

static struct GroundwaterAquitardSyntax
{
  static Model& make (Block& al)
  { return *new GroundwaterAquitard (al); }

  GroundwaterAquitardSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Aquitard groundwater, free drainage.");
      Groundwater::load_syntax (syntax, alist);
      syntax.add ("K_aquitard", "cm/h", Check::non_negative (), Syntax::Const,
		  "Conductivity of the aquitard.");
      alist.add ("K_aquitard", 1e-3);
      syntax.add ("Z_aquitard", "cm", Check::positive (), Syntax::Const,
		  "Thickness of the aquitard.\n\
The aquitard begins below the bottommost soil horizon.");
      alist.add ("Z_aquitard", 200.0);
      syntax.add ("h_aquifer", "cm", Check::positive (), Syntax::OptionalState,
		  "Pressure potential in the aquifer below the aquitard.\n\
By default. this is Z_aquitard.\n\
You can alternatively specify the pressure as a virtual groundwater level.\n\
See 'pressure_table'.");
      syntax.add_object ("pressure_table", Depth::component,
                         Syntax::OptionalConst, Syntax::Singleton, "\
Height of groundwater the corresponds to the pressure in the aquifer.  \n\
\n\
If you drilled a well down to the aquifer, this is number what the\n\
water level in the well would be as height above ground (a negative\n\
number).  This is different from the actual groundwater table, because\n\
the aquitart block the water, and the pipes lead the water away.\n\
You can alternatively specify the pressure directly, with 'h_aquifer'.");
      Librarian::add_type (Groundwater::component, "aquitard", alist, syntax, &make);
    }
} GroundwaterAquitard_syntax;

// groundwater_aquitard.C ends here.
