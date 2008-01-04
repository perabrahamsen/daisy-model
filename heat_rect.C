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


static void
lowerboundary (const GeometryRect& geo,
               const bool isflux,
               const double T_border,
               const ublas::vector<double>& q_edge,
               const ublas::vector<double>& cond_edge,
               const ublas::vector<double>& T,
               const bool enable_boundary_conduction,
               ublas::vector<double>& B_dir_vec)
{
  // change to isflux_lower?????
  if (!isflux)   //Nothing to do for (no) flux bc 
    {
      const std::vector<int>& edge_below = geo.cell_edges (Geometry::cell_below);
      const size_t edge_below_size = edge_below.size ();
      
      for (size_t i = 0; i < edge_below_size; i++)
        {
          const int edge = edge_below[i];
          const int cell = geo.edge_other (edge, Geometry::cell_below);
          const double area = geo.edge_area (edge);
          const double area_per_length = geo.edge_area_per_length (edge);
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;         
          double T_cell = T (cell);
	  Dirichlet_expl (cell, area, area_per_length, in_sign, 
                          cond_edge (edge), T_border, T_cell, 
                          q_edge (edge), enable_boundary_conduction, B_dir_vec); 
        }
    }
}


static void
upperboundary (const GeometryRect& geo,
               const bool isflux,
               const double T_border,
               const ublas::vector<double>& q_edge,
               const ublas::vector<double>& cond_edge,
               const ublas::vector<double>& T,
               const bool enable_boundary_conduction,
               ublas::vector<double>& B_dir_vec)
{
  // change to isflux_lower?????
  if (!isflux)   //Nothing to do for (no) flux bc 
    {
      
      const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
      const size_t edge_above_size = edge_above.size ();
      
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const int edge = edge_above[i];
          const int cell = geo.edge_other (edge, Geometry::cell_above);
          const double area = geo.edge_area (edge);
          const double area_per_length = geo.edge_area_per_length (edge);
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          daisy_assert (in_sign < 0);
          
          double T_cell = T (cell);
	  Dirichlet_expl (cell, area, area_per_length, in_sign, 
                          cond_edge (edge), T_border, T_cell, 
                          q_edge (edge), enable_boundary_conduction, B_dir_vec);
        }
    }
}


#if 0
// XXXX work in progress 
static void 
fluxes (const GeometryRect& geo,
        const std::vector<edge_type_t>& edge_type, 
        const ublas::vector<double>& q_edge,
        const ublas::vector<double>& cond_edge,
        const ublas::vector<double>& T,
        const double T_top,
        const double T_bottom,
        const ublas::vector<double>& B_dir_vec,
        ublas::vector<double>& dJ) 
{
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  daisy_assert (edge_type.size () == edge_size);
  daisy_assert (q_edge.size () == edge_size);
  daisy_assert (cond_edge.size () == edge_size);
  daisy_assert (dJ.size () == edge_size);


  for (size_t e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);  

      switch (edge_type[e])
        {
        case Unhandled:
          dJ[e] = 0.0;
          break;
        case Internal:
          {
            daisy_assert (from >= 0);
            daisy_assert (to >= 0);
            daisy_assert (from < C.size ());
            daisy_assert (to < C.size ());
            
            //--- Advective part ---
            const double upstream_weight = 0.5;  //Should be taken from  outside
            const double alpha = (q_edge[e] >= 0) 
              ? upstream_weight 
              : 1.0 - upstream_weight;
            dJ[e] = alpha * q_edge[e] * C[from] + (1.0-alpha) * C[to];
            
            //--- Diffusive part - xx_zz --- 
            const double gradient = geo.edge_area_per_length (e) *
              (C[to] - C[from]);
            dJ[e] -= ThetaD_xx_zz[e]*gradient;  //xx_zz diffusion
              
            //--- Diffusive part - xz_zx ---
            const std::vector<int>& corners = geo.edge_corners (e);
            daisy_assert (corners.size () == 2);
            const int A = corners[0];
            const int B = corners[1];
            const std::vector<int>& A_cells = geo.corner_cells (A);
            const std::vector<int>& B_cells = geo.corner_cells (B);
            const bool A_is_border = A_cells.size () == 2;
            daisy_assert (A_is_border || A_cells.size () == 4);
            const bool B_is_border = B_cells.size () == 2;
            daisy_assert (B_is_border || B_cells.size () == 4);
          
            if (A_is_border && B_is_border)
              // Both corners of the edge touches the border.
              continue;
            
            // We use the fact that the geometry is rectangular and
            // alligned with our coordinate system to ignore whether
            // we are looking at the z or the x dimension.
            const double dkz = geo.corner_z (B) - geo.corner_z (A);
            const double dkx = geo.corner_x (B) - geo.corner_x (A);
            const double area = geo.edge_area (e);
            daisy_assert (approximate (fabs (dkz + dkx), area));
            double magnitude = -ThetaD_xz_zx (e) * area / (dkz + dkx);

            // On a border we calculate from edge center, rather than corner.
            if (A_is_border || B_is_border)
              magnitude *= 2.0;

            const double dcz = geo.z (to) - geo.z (from);
            const double dcx = geo.x (to) - geo.x (from);
            const double length = geo.edge_length (e);
            daisy_assert (approximate (fabs (dcz + dcx), length));
            //const double sign =  length / (dcz + dcx);
            
            double Sum_C_A = 0.0;
            for (size_t i = 0; i < A_cells.size (); i++)
              Sum_C_A += C[A_cells[i]];
            const double C_A = A_is_border
              ? Sum_C_A / 4.0
              : Sum_C_A / 2.0;

            double Sum_C_B = 0.0;
            for (size_t i = 0; i < B_cells.size (); i++)
              Sum_C_B += C[B_cells[i]];
            const double C_B = B_is_border
              ? Sum_C_B / 4.0
              : Sum_C_B / 2.0;
            
            dJ[e] -= ThetaD_xz_zx (e) * (C_B-C_A)/(dcz + dcx);  
          }
          break;

        case Neumann_explicit_upper:
          dJ[e] = 0.0; // use existing J 
          break;

        case Neumann_explicit_lower:
          dJ[e] = q_edge[e] * C_below; 
          break;
          
        case Neumann_implicit:
          {
            const int cell = geo.cell_is_internal (to) ? to : from;
            daisy_assert (cell >= 0);
            daisy_assert (cell < C.size ());
            dJ[e] = q_edge[e] * C[cell];
          }
          break;
          
        case Dirichlet:      //Only lower boundary 
          {
            const int cell = geo.cell_is_internal (to) ? to : from;
            daisy_assert (cell >= 0);
            daisy_assert (cell < C.size ());
            
            const double in_sign 
              = geo.cell_is_internal (geo.edge_to (e)) ? 1.0 : -1.0;

            dJ[e] = -in_sign * B_dir_vec (cell) / geo.edge_area (e); 

            // std::cout << "Dirichlet flux: \n";
            // std::cout << "in_sign: " << in_sign << '\n';
            // std::cout << "B_dir_vec (cell):" << B_dir_vec (cell) << '\n';  
            // std::cout << "dJ[e]: " << dJ[e] << '\n';
            
            
            
            /*
            //Advective transport
            const double in_sign 
              = geo.cell_is_internal (geo.edge_to (e)) ? 1.0 : -1.0;

            if (q_edge[e] * in_sign >= 0)       //Inflow
              dJ[e] = q_edge[e] * C_below;
            else                                //Outflow
              dJ[e] = q_edge[e] * C[cell];
            
            //Diffusive transport - xx_zz diffusion  
            const double gradient = geo.edge_area_per_length (e) *
              (C[cell]-C_below)*in_sign;
            dJ[e] -= ThetaD_xx_zz[e]*gradient;
         
            std::cout << "cell: " << cell << '\n';
            std::cout << "in_sign:" << in_sign << '\n';
            std::cout << "C[cell]:" << C[cell] << '\n';
            std::cout << "C_below" << C_below << '\n';
            std::cout << "gradient" << gradient << '\n';

            //Diffusive transport - xz_zx diffusion
            //Constant values along border direction ->
            //no flux
            */
          }
          break;
        }
    }
}


#endif








#if 0
void 
MsoltranrectMollerup::fluxes (const GeometryRect& geo,
                              const std::vector<edge_type_t>& edge_type, 
                              const ublas::vector<double>& q_edge,
                              const ublas::vector<double>& ThetaD_xx_zz,
                              const ublas::vector<double>& ThetaD_xz_zx,
                              const ublas::vector<double>& T,
                              const double C_below,
                              const ublas::vector<double>& B_dir_vec,
                              ublas::vector<double>& dJ) 
{
  const size_t edge_size = geo.edge_size (); // number of edges  

  daisy_assert (edge_type.size () == edge_size);
  daisy_assert (q_edge.size () == edge_size);
  daisy_assert (ThetaD_xx_zz.size () == edge_size);
  daisy_assert (ThetaD_xz_zx.size () == edge_size);
  daisy_assert (dJ.size () == edge_size);

  for (size_t e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);  

      switch (edge_type[e])
        {
        case Unhandled:
          dJ[e] = 0.0;
          break;
        case Internal:
          {
            daisy_assert (from >= 0);
            daisy_assert (to >= 0);
            daisy_assert (from < C.size ());
            daisy_assert (to < C.size ());
            
            //--- Advective part ---
            const double upstream_weight = 0.5;  //Should be taken from  outside
            const double alpha = (q_edge[e] >= 0) 
              ? upstream_weight 
              : 1.0 - upstream_weight;
            dJ[e] = alpha * q_edge[e] * C[from] + (1.0-alpha) * C[to];
            
            //--- Diffusive part - xx_zz --- 
            const double gradient = geo.edge_area_per_length (e) *
              (C[to] - C[from]);
            dJ[e] -= ThetaD_xx_zz[e]*gradient;  //xx_zz diffusion
              
            //--- Diffusive part - xz_zx ---
            const std::vector<int>& corners = geo.edge_corners (e);
            daisy_assert (corners.size () == 2);
            const int A = corners[0];
            const int B = corners[1];
            const std::vector<int>& A_cells = geo.corner_cells (A);
            const std::vector<int>& B_cells = geo.corner_cells (B);
            const bool A_is_border = A_cells.size () == 2;
            daisy_assert (A_is_border || A_cells.size () == 4);
            const bool B_is_border = B_cells.size () == 2;
            daisy_assert (B_is_border || B_cells.size () == 4);
          
            if (A_is_border && B_is_border)
              // Both corners of the edge touches the border.
              continue;
            
            // We use the fact that the geometry is rectangular and
            // alligned with our coordinate system to ignore whether
            // we are looking at the z or the x dimension.
            const double dkz = geo.corner_z (B) - geo.corner_z (A);
            const double dkx = geo.corner_x (B) - geo.corner_x (A);
            const double area = geo.edge_area (e);
            daisy_assert (approximate (fabs (dkz + dkx), area));
            double magnitude = -ThetaD_xz_zx (e) * area / (dkz + dkx);

            // On a border we calculate from edge center, rather than corner.
            if (A_is_border || B_is_border)
              magnitude *= 2.0;

            const double dcz = geo.z (to) - geo.z (from);
            const double dcx = geo.x (to) - geo.x (from);
            const double length = geo.edge_length (e);
            daisy_assert (approximate (fabs (dcz + dcx), length));
            //const double sign =  length / (dcz + dcx);
            
            double Sum_C_A = 0.0;
            for (size_t i = 0; i < A_cells.size (); i++)
              Sum_C_A += C[A_cells[i]];
            const double C_A = A_is_border
              ? Sum_C_A / 4.0
              : Sum_C_A / 2.0;

            double Sum_C_B = 0.0;
            for (size_t i = 0; i < B_cells.size (); i++)
              Sum_C_B += C[B_cells[i]];
            const double C_B = B_is_border
              ? Sum_C_B / 4.0
              : Sum_C_B / 2.0;
            
            dJ[e] -= ThetaD_xz_zx (e) * (C_B-C_A)/(dcz + dcx);  
          }
          break;

        case Neumann_explicit_upper:
          dJ[e] = 0.0; // use existing J 
          break;

        case Neumann_explicit_lower:
          dJ[e] = q_edge[e] * C_below; 
          break;
          
        case Neumann_implicit:
          {
            const int cell = geo.cell_is_internal (to) ? to : from;
            daisy_assert (cell >= 0);
            daisy_assert (cell < C.size ());
            dJ[e] = q_edge[e] * C[cell];
          }
          break;
          
        case Dirichlet:      //Only lower boundary 
          {
            const int cell = geo.cell_is_internal (to) ? to : from;
            daisy_assert (cell >= 0);
            daisy_assert (cell < C.size ());
            
            const double in_sign 
              = geo.cell_is_internal (geo.edge_to (e)) ? 1.0 : -1.0;

            dJ[e] = -in_sign * B_dir_vec (cell) / geo.edge_area (e); 

            // std::cout << "Dirichlet flux: \n";
            // std::cout << "in_sign: " << in_sign << '\n';
            // std::cout << "B_dir_vec (cell):" << B_dir_vec (cell) << '\n';  
            // std::cout << "dJ[e]: " << dJ[e] << '\n';
            
            
            
            /*
            //Advective transport
            const double in_sign 
              = geo.cell_is_internal (geo.edge_to (e)) ? 1.0 : -1.0;

            if (q_edge[e] * in_sign >= 0)       //Inflow
              dJ[e] = q_edge[e] * C_below;
            else                                //Outflow
              dJ[e] = q_edge[e] * C[cell];
            
            //Diffusive transport - xx_zz diffusion  
            const double gradient = geo.edge_area_per_length (e) *
              (C[cell]-C_below)*in_sign;
            dJ[e] -= ThetaD_xx_zz[e]*gradient;
         
            std::cout << "cell: " << cell << '\n';
            std::cout << "in_sign:" << in_sign << '\n';
            std::cout << "C[cell]:" << C[cell] << '\n';
            std::cout << "C_below" << C_below << '\n';
            std::cout << "gradient" << gradient << '\n';

            //Diffusive transport - xz_zx diffusion
            //Constant values along border direction ->
            //no flux
            */
          }
          break;
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
