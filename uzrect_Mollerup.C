// uzrect_Mollerup.C --- A 2D solution to Richard's equation in a rect. grid.
// 
// Copyright 2006 Mikkel Mollerup and KVL.
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

#include "uzrect.h"
#include "geometry_rect.h"
#include "soil_water.h"
#include "groundwater.h"
#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "mathlib.h"
#include "assertion.h"
#include <sstream>

struct UZRectMollerup : public UZRect
{
  // Parameters.

  // Interface.
  void tick (const GeometryRect&, const Soil&, SoilWater&, const SoilHeat&, 
             const Surface&, const Groundwater&, double dt, Treelog&);

  // Internal functions.
  void lowerboundary ()    
  void upperboundary ()




  // Create and Destroy.
  void has_macropores (bool);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UZRectMollerup (Block& al);
  ~UZRectMollerup ();
};

void 
UZRectMollerup::tick (const GeometryRect& geo, const Soil& soil, 
                      SoilWater& soil_water, const SoilHeat& soil_heat,
                      const Surface& surface, const Groundwater& groundwater,
                      const double dt,
                      Treelog& msg)

{
#if 0
  const size_t edge_size = geo.edge_size (); // number of edges 
  const size_t edge_rows = geo.edge_rows (); // number of edge rows 
  const size_t cell_size = geo.cell_size (); // number of cells 
  const size_t cell_rows = geo.cell_rows (); // number of cell rows
  const size_t cell_columns = geo.cell_columns (); // number of cell columns

  // Insert magic here.
  
  std::vector<double> Theta (cell_size); // water content 
  std::vector<double> h (cell_size); // matrix pressure
  std::vector<double> h_ice (cell_size); // 
  std::vector<double> S (cell_size); // sink term
  std::vector<double> dx (cell_size); // width of cells
  std::vector<double> dz (cell_size); // height of cells 
  std::vector<double> T (cell_size); // temperature 
  std::vector<double> K (cell_size); // hydraulic conductivity
  std::vector<double> Cw (cell_size); // specific water capacity
 
      
  std::matrix<double>  Dm_mat (cell_size,0.0); // upper Dir bc  	      
  std::vector<double>  Dm_vec (cell_size,0.0); // upper Dir bc
  std::vector<double>  Gm (cell_size,0.0); // upper Dir bc
  std::vector<double>  B (cell_size,0.0); // upper Neu bc 






  // make vectors 
  for (size_t cell = 0; cell !=cell_size ; ++cell) 
    {				
      Theta[cell] = soil_water.Theta (cell);
      h[cell] =  soil_water.h (cell);
      h_ice[cell] = soil_water.h_ice (cell);
      S[cell] =  soil_water.S_sum (cell);
      dx[cell] = geo.dx (cell);
      dz[cell] = geo.dz (cell);	
      T[cell] = soil_heat.T (cell); 
      K[cell] =  soil.K (cell, h[cell], h_ice[cell], T[cell]); 
      Cw[cell] = soil.Cw2 (cell, h[cell], h_ice[cell]);

      
		
	         }

  // check gaussj.h and gaussj.C to see how it works...
  // ms = matrixsolve
  GaussJordan ms (cell_size);	// initialize

    
  // void set_value (int row, double); 
  // double get_value (int row) const;
  // void set_entry (int row, int column, double);
  // double get_entry (int row, int column) const;
  // inline double operator() (int row, int column) const
  // { return get_entry (row, column); }
  // void solve ();
  // double result (int row) const;
  // GaussJordan (int s);  
  
#endif
}

  
void 
UZRectMollerup::lowerboundary (std::matrix<double>& Dm_mat 
			       std::vector<double>& Dm_vec 
			       std::vector<double>& Gm 
			       std::vector<double>& B 
			       const size_t cell_size, 
			       const Groundwater::bottom_t boundtype
			       const std::vector<double>& K

)

{



// Initialise vectors+matrices 
for (size_t i = 0; i !=cell_size ; ++i) 
    {				
      for (size_t j = 0; j !=cell_size ; ++j)
	{
	  Dm_mat[i,j] = 0.0;
	}
      Dm_vec[i] = 0.0; 
      Gm[i] = 0.0;
      B[i] = 0.0;
     }


switch boundtype
 
case 'pressure' 

case 'lysimeter'

case 'forced_flux'

case 'free_drainage'



if neumanntype






}

void 
UZRectMollerup::upperboundary ()
{}

void 
UZRectMollerup::has_macropores (const bool)
{ /* Ignore for now. */ }

void 
UZRectMollerup::load_syntax (Syntax&, AttributeList&)
{ }

UZRectMollerup::UZRectMollerup (Block& al)
  : UZRect (al)
{ }

UZRectMollerup::~UZRectMollerup ()
{ }

const AttributeList& 
UZRect::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      UZRectMollerup::load_syntax (dummy, alist);
      alist.add ("type", "Mollerup");

    }
  return alist;
}

static struct UZRectMollerupSyntax
{
  static UZRect& make (Block& al)
  { return *new UZRectMollerup (al); }
  UZRectMollerupSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
A finite volume solution to matrix water transport.\n\
See Mollerup, 2007 for details.");
    UZRectMollerup::load_syntax (syntax, alist);
    Librarian<UZRect>::add_type ("Mollerup", alist, syntax, &make);
  }
} UZRectMollerup_syntax;

// uzrect_Mollerup.C ends here.
