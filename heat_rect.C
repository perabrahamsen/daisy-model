// heat_rect.C -- Heat transport in a rectangular grid.
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

#include "heat_rect.h"
#include "geometry_rect.h"
#include "syntax.h"
#include "alist.h"
#include "block.h"
#include "submodel.h"
#include "treelog.h"
#include "plf.h"


// Uncomment for fast code that does not catches bugs.
#define NDEBUG
//#define BOOST_UBLAS_NDEBUG

#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace ublas = boost::numeric::ublas;


static void 
convection (const GeometryRect& geo,
            const ublas::vector<double>& q_edge,
            ublas::matrix<double>& convec)  
{
  const size_t edge_size = geo.edge_size (); // number of edges  

  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  const double value = geo.edge_area (e) * q_edge[e];
	
	  //Equal weight: upstream_weight = 0.5
	  //Upstr weight: upstream_weight = 1.0
	  const double upstream_weight = 0.5;
          //const double upstream_weight = 1.0;
          const double alpha = (q_edge[e] >= 0) 
	    ? upstream_weight 
	    : 1.0 - upstream_weight;
	  convec (from, from) += alpha*value;
	  convec (from, to)   += (1.0-alpha)*value;
	  convec (to, from)   -= alpha*value;
	  convec (to, to)     -= (1.0-alpha)*value;
	} 
    }
}


static void 
cond_cell2edge (const GeometryRect& geo,
                const std::vector<double>& conductivity,
                ublas::vector<double>& cond_edge)
{
  const size_t edge_size = geo.edge_size ();
  //cond_edge = ublas::zero_vector<double> (edge_size);
  
  for (int e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);    
      
      if (geo.edge_is_internal (e))
	cond_edge[e] = 0.5 *
	  (conductivity[from] + conductivity[to]);
      else if (geo.cell_is_internal (from))
	cond_edge[e] = conductivity[from];
      else
	cond_edge[e] = conductivity[to];
    }
}



static void 
conduction (const GeometryRect& geo,
            const ublas::vector<double>& cond_edge,
            ublas::matrix<double>& conduc)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);
	  const double magnitude = geo.edge_area_per_length (e) 
            * cond_edge[e]; 
	  
          conduc (from, from) -= magnitude;
	  conduc (from, to) += magnitude;
	  conduc (to, to) -= magnitude;
	  conduc (to, from) += magnitude; 
	} 
    }
}


static void
Dirichlet_expl(const size_t cell,
               const double area,
               const double area_per_length, 
               const double in_sign,
               const double conductivity_cell,
               const double T_border,
               const double T_cell,
               const double q,
               const bool enable_boundary_conduction,
               ublas::vector<double>& B_dir_vec)
{
  //Estimate flux 
  double Q_convec_out;      //convection
  double Q_conduc_out;      //conduction
  double Q_out;
  

  // Boundary convection
  if (q*in_sign >= 0)     //Inflow
    Q_convec_out = - in_sign * q * area * T_border;
  else                    //Outflow 
    Q_convec_out = - in_sign * q * area * T_cell;
  
  if (enable_boundary_conduction)
    {
      const double gradient =  area_per_length * (T_border - T_cell);
      Q_conduc_out = -conductivity_cell * gradient;
    }
  else 
    Q_conduc_out = 0.0; 
    
  // Boundary flux by advection and diffusion
  Q_out = Q_convec_out + Q_conduc_out;
  
  // Write to Neumann-type vector 
  B_dir_vec (cell) = Q_out;
}



/////XXXXXX Hvad bruger jeg den her til?????
static void 
Dirichlet (const size_t cell,
           const double area, 
           const double area_per_length, 
           const double in_sign,
           const double conductivity_cell,
           const double T_border,
           const double q,
           ublas::banded_matrix<double>& conducm_mat,
           ublas::vector<double>& conducm_vec, 
           ublas::banded_matrix<double>& convecm_mat,
           ublas::vector<double>& convecm_vec) 
{
  // Boundary advection
  // Use C_border for influx and C_cell for outflux 
  const double value = area * q;
  if (q*in_sign >= 0)     //Inflow
    convecm_vec (cell) -= in_sign * value * T_border; 
  else                    //Outflow 
    convecm_mat (cell, cell) -= in_sign * value;   
  
  //Boundary xx_zz diffusion
  const double cond_area_per_length = conductivity_cell * area_per_length;
  conducm_mat (cell, cell) -= cond_area_per_length;
  
  const double conducm_vec_val = cond_area_per_length * T_border;
  conducm_vec (cell) += conducm_vec_val;
}


#if 0
static void 
lowerboundary (const GeometryRect& geo,
               const bool isflux,
               const double T_border,
               const ublas::vector<double>& conductivity,
               const ublas::vector<double>& T,
               const bool enable_boundary_conduction,
               ublas::vector<double>& B_dir_vec)
{
  if (!isflux)  // do nothing for (no)flux bc
    { 
      const std::vector<int>& edge_below = geo.cell_edges (Geometry::cell_below);
      const size_t edge_below_size = edge_below.size ();
      
      for (size_t i = 0; i < edge_below_size; i++)
        {
          const int edge = edge_below[i];
          const int cell = geo.edge_other (edge, Geometry::cell_below);
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          daisy_assert (in_sign > 0);
          const double area = geo.edge_area (edge);
          const double area_per_length = geo.edge_area_per_length (edge);
          
          double C_cell = C (cell);
	  Dirichlet_expl (cell, area, area_per_length, in_sign, 
                          ThetaD_xx_zz (edge), C_border, C_cell, 
                          q_edge (edge), enable_boundary_conduction, B_dir_vec); 
          
    
        }      
    }


  
  const std::vector<int>& edge_below = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();
  
  for (size_t i = 0; i < edge_below_size; i++)
    {
      const int edge = edge_below[i];
      const int cell = geo.edge_other (edge, Geometry::cell_below);
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign > 0);
      const double area = geo.edge_area (edge);
      const double area_per_length = geo.edge_area_per_length (edge);
      
      if (isflux)               // Flux BC
        {
          const bool influx = in_sign * q_edge (edge) > 0;
          if (influx)
            {
              edge_type[edge] = Neumann_explicit_lower;
              const double J_in = C_border * q_edge (edge); 
              Neumann_expl (cell, area, in_sign, J_in, B_vec);
            }
          else
            {
              edge_type[edge] = Neumann_implicit;
              Neumann_impl (cell, area, in_sign, q_edge (edge), B_mat);
            }
        }
      else                      // C_Border. BC
        {
          edge_type[edge] = Dirichlet;
          // write something
          double C_cell = C (cell);
	  Dirichlet_expl (cell, area, area_per_length, in_sign, 
                          ThetaD_xx_zz (edge), C_border, C_cell, 
                          q_edge (edge), enable_boundary_conduction, B_dir_vec); 
        }
    }
}
#endif 




void
HeatRect::solve (const GeometryRect& geo,
		 const std::vector<double>& q_water,
		 const std::vector<double>& S,
		 const std::vector<double>& capacity,
		 const std::vector<double>& conductivity,
		 const double T_top_old,
		 const double T_top_new,
		 const double T_bottom,
		 std::vector<double>& T,
		 const double dt, Treelog& msg) const
{
  const size_t cell_size = geo.cell_size ();
  const double T_top = (T_top_new + T_top_old) / 2.0;

 
  

  
  // Linear interpolation between bottom and top.
  PLF plf;
  plf.add (geo.bottom (), T_bottom);
  plf.add (geo.top (), T_top);

  for (size_t c = 0; c < cell_size; c++)
    T[c] = plf (geo.z (c));
}



#if 0


void
HeatRect::solve (const GeometryRect& geo,
		 const std::vector<double>& q_water,
		 const std::vector<double>& S,
		 const std::vector<double>& capacity,
		 const std::vector<double>& conductivity,
		 const double T_top_old,
		 const double T_top_new,
		 const double T_bottom,
		 std::vector<double>& T,
		 const double dt, Treelog& msg) const
{
  const size_t cell_size = geo.cell_size ();

  const double T_top = (T_top_new + T_top_old) / 2.0;

  // Linear interpolation between bottom and top.
  PLF plf;
  plf.add (geo.bottom (), T_bottom);
  plf.add (geo.top (), T_top);

  for (size_t c = 0; c < cell_size; c++)
    T[c] = plf (geo.z (c));
}


#endif




void
HeatRect::load_syntax (Syntax&, AttributeList& alist)
{
  alist.add ("submodel", "HeatRect");
  alist.add ("description", "Heat transport in a rectangular grid.");
}

HeatRect::HeatRect (Block&)
{ }

HeatRect::~HeatRect ()
{ }

static Submodel::Register 
heat_rect_submodel ("HeatRect", HeatRect::load_syntax);

// heat_rect.C ends here.
