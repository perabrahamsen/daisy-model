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
#include "assertion.h"
#include "librarian.h"
#include <map>

struct GroundwaterAquitard : public Groundwater
{
  // We keep fluxes in a map.
  typedef std::map<size_t, double> edge_flux_map;
  edge_flux_map edge_flux;

public:
  bottom_t bottom_type() const
  { return forced_flux; }

  double q_bottom (size_t edge) const
  {
    edge_flux_map::const_iterator i = edge_flux.find (edge);
    daisy_assert (i != edge_flux.end ());
    return (*).second;
  }
    
  // Simulation.
public:
  void tick (const Geometry& geo,
             const Soil&, SoilWater& soil_water, double, 
	     const SoilHeat&, const Time&, const Scope&, Treelog&)
  { 
    const std::vector<int>& bottow_edges 
      = geo.cell_edges (Geometry::cell_below);
    const size_t bottom_edges_size = bottom_edges.size ();

    for (size_t i = 0; i < bottom_edges_size; i++)
      {
        const int edge = bottom_edges[i];
        const int cell = geo.edge_other (edge, Geometry::cell_below);
        
        // Parameters.
        const double K_aq = 0.001;   //Conductivity of aquitard
        const double Dz_aq = 100;      //Thickness of aquitard
        const double h_aq = 1000;       //Pressure below aquitard

        // Calculation.
        // Multiplied with 2 because it is a boundary cell...
        const double Dz_i = 2 * geo.edge_length (edge);  
    
        double K_i = soil_water.K (cell);   //Conductivity in cell
        double h_i = soil_water.h (cell);   //Pressure in cell
        
        double taeller = K_i * (2*h_i/Dz_i+1) + K_aq * (h_aq/Dz_aq-1);
        double naevner = K_aq + 2*K_i*Dz_aq/Dz_i;
    
        // Flux into domain.
        const double q_up = -K_aq * (taeller/naevner - h_aq/Dz_aq +1);
        edge_flux[edge] = q_up;
      }
  }

  double table () const
  { return 42.42e42; }

  // Create and Destroy.
public:
  void initialize (const Geometry&, const Time&, const Scope&, Treelog&)
  { }
  bool check (const Geometry&, const Scope&, Treelog&) const
  { return true; }
  GroundwaterAquitard (Block& al)
    : Groundwater (al)
  { }
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
      Librarian::add_type (Groundwater::component, "aquitard", alist, syntax, &make);
    }
} GroundwaterAquitard_syntax;

// groundwater_aquitard.C ends here.
