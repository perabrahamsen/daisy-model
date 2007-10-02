// msoltranrect_Mollerup.C --- Coupled vertical and horizontal transport.
// 
// Copyright 2007 Mikkel Mollerup, Per Abrahamsen and KVL.
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

//#if 1  //mmo udkomenteret

#define BUILD_DLL
#include "msoltranrect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "adsorption.h"
#include "alist.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "mathlib.h"
#include <sstream>

// Uncomment for fast code that does not catches bugs.
// #define BOOST_UBLAS_NDEBUG

#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace ublas = boost::numeric::ublas;

struct MsoltranrectMollerup : public Msoltranrect
{
 
  // Water flux.
  static void cell_based_flux (const GeometryRect& geo,  
			       const ublas::vector<double>& q_edge,
                               ublas::vector<double>& qx_cell,
                               ublas::vector<double>& qz_cell);

  // Edge water content 
  static void edge_watercontent (const GeometryRect& geo,
				 const ublas::vector<double>& Theta_cell,
				 ublas::vector<double>& Theta_edge);
  
  // Interpolation function for vectors (for water content)
  static void interpol(const ublas::vector<double>& V_start,
                       const ublas::vector<double>& V_end,
                       const double dt,
                       const double ddt,
                       ublas::vector<double>& V);

  static double anisotropy_factor (const Geometry& geo, size_t edge, 
				   const double Dxx, 
				   const double Dzz);

  static void diffusion_tensor (const GeometryRect& geo, 
				const Soil& soil, 
				const ublas::vector<double>& q_edge, 
				const ublas::vector<double>& Theta,
				const double diffusion_coefficient,
				ublas::vector<double>& Dxx_cell,
				ublas::vector<double>& Dzz_cell,
				ublas::vector<double>& Dxz_cell,
				Treelog& msg);
  
  static void thetadiff_longtran (const GeometryRect& geo,
				  const ublas::vector<double>& Theta,
				  const ublas::vector<double>& Dxx_cell,
				  const ublas::vector<double>& Dzz_cell,
				  const ublas::vector<double>& Dxz_cell,
				  ublas::vector<double>& ThetaD_long,
				  ublas::vector<double>& ThetaD_tran);
  
  static void diffusion_long (const GeometryRect& geo,
			      const ublas::vector<double>& ThetaD_long,
			      ublas::matrix<double>& diff_long);
  
  static void diffusion_tran (const GeometryRect& geo,
			      const ublas::vector<double>& ThetaD_tran,	
			      ublas::matrix<double>& diff_tran);

  static void advection (const GeometryRect& geo,
			 const ublas::vector<double>& q_edge,
			 ublas::matrix<double>& advec);
  
  static void Neumann_expl (const size_t cell, const double area, 
                            const double in_sign, const double J, 
                            ublas::vector<double>& B_vec);

  static void Neumann_impl (const size_t cell, const double area, 
                            const double in_sign, const double q, 
                            ublas::banded_matrix<double>& B_mat);
  
  static void Dirichlet_long (const size_t cell,
			      const double area, 
			      const double area_per_length, 
			      const double in_sign,
			      const double ThetaD_long,
			      const double C_border,
			      const double C_cell,
			      const double q,
			      double& J,
			      ublas::banded_matrix<double>& diffm_long_mat,
			      ublas::vector<double>& diffm_long_vec, 
			      ublas::banded_matrix<double>& advecm); 

  static void lowerboundary (const GeometryRect& geo,
			     const bool isflux,
			     const double C_border,
			     const std::vector<double>& C,
                             const ublas::vector<double>& q_edge,
                             const ublas::vector<double>& ThetaD_long,
			     std::vector<double>& J,
                             ublas::banded_matrix<double>& B_mat,
                             ublas::vector<double>& B_vec,
                             ublas::banded_matrix<double>& diffm_long_mat, 
                             ublas::vector<double>& diffm_long_vec,
			     ublas::banded_matrix<double>& advecm);

  static void upperboundary (const GeometryRect& geo,
              		     std::vector<double>& J,
                             ublas::vector<double>& B_vec,
                             Treelog& msg);

  static void fluxes (const GeometryRect& geo,
                      const ublas::vector<double>& q_edge,
                      const ublas::vector<double>& ThetaD_long,
                      const ublas::vector<double>& ThetaD_tran,
                      const ublas::vector<double>& C,
                      ublas::vector<double>& dJ); 

  // Solute.
  void flow (const GeometryRect& geo, 
             const Soil& soil, 
             const SoilWater& soil_water, 
             const std::string& name,
             std::vector<double>& M, 
             std::vector<double>& C, 
             const std::vector<double>& S, 
             std::vector<double>& J, 
	     const double C_below,
             Adsorption& adsorption,
             double diffusion_coefficient, double dt,
             Treelog& msg);
  
  void output (Log&) const;

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  MsoltranrectMollerup (Block& al);
  ~MsoltranrectMollerup ();
};

void
MsoltranrectMollerup::cell_based_flux (const GeometryRect& geo,  
				       const ublas::vector<double>& q_edge, 
				       ublas::vector<double>& qx_cell,
                                       ublas::vector<double>& qz_cell)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  
  qx_cell = ublas::zero_vector<double> (cell_size);
  qz_cell = ublas::zero_vector<double> (cell_size);
  ublas::vector<double> wx_cell = ublas::zero_vector<double> (cell_size);
  ublas::vector<double> wz_cell = ublas::zero_vector<double> (cell_size);

  for (int e = 0; e < edge_size; e++)
    {
      const double q = q_edge (e);
      const double sin_angle = geo.edge_sin_angle (e);
      const double cos_angle = cos (asin (sin_angle));
      const double area = geo.edge_area (e);
      const double x_wall = area * cos_angle;
      const double z_wall = area * sin_angle;
      const double qx = q * x_wall;
      const double qz = q * z_wall;

      const int from = geo.edge_from (e);
      if (geo.cell_is_internal (from))
        {
          qx_cell[from] += qx;
          qz_cell[from] += qz;
          wx_cell[from] += x_wall;
          wz_cell[from] += z_wall;
        }

      const int to = geo.edge_to (e);
      if (geo.cell_is_internal (to))
        {
          qx_cell[to] += qx;
          qz_cell[to] += qz;
          wx_cell[to] += x_wall;
          wz_cell[to] += z_wall;
        }
    }

  for (int c = 0; c < cell_size; c++)
    {
      
      if (wx_cell[c] > 0.0)
        qx_cell[c] /= wx_cell[c];
      if (wz_cell[c] > 0.0)
        qz_cell[c] /= wz_cell[c];
    }
}

void 
MsoltranrectMollerup::edge_watercontent (const GeometryRect& geo,
					 const ublas::vector<double>& Theta_cell,
					 ublas::vector<double>& Theta_edge)
{
  const size_t edge_size = geo.edge_size ();
  //Theta_edge = ublas::zero_vector<double> (edge_size);
  
  for (int e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);    
      
      if (geo.edge_is_internal (e))
	Theta_edge[e] = 0.5 *
	  (Theta_cell[from] + Theta_cell[to]);
      else if (geo.cell_is_internal (from))
	Theta_edge[e] = Theta_cell[from];
      else
	Theta_edge[e] = Theta_cell[to];
    }
}

void
MsoltranrectMollerup::interpol(const ublas::vector<double>& V_start,
                       const ublas::vector<double>& V_end,
                       const double dt,
                       const double ddt,
                       ublas::vector<double>& V)
{
  //Linear interpolation
  V = (1 - ddt/dt)*V_start + ddt/dt*V_end; 
}


double
MsoltranrectMollerup::anisotropy_factor (const Geometry& geo, size_t edge, 
                                         const double Dxx, 
                                         const double Dzz)
{
  const double sin_angle = geo.edge_sin_angle (edge);
  const double cos_angle = geo.edge_cos_angle (edge);
  
  return sqrt (sqr (Dzz*sin_angle) + sqr (Dxx * cos_angle));
}



void
MsoltranrectMollerup::diffusion_tensor (const GeometryRect& geo, 
					const Soil& soil, 
					const ublas::vector<double>& q_edge,
					const ublas::vector<double>& Theta,
					const double diffusion_coefficient,
					ublas::vector<double>& Dxx_cell,
					ublas::vector<double>& Dzz_cell,
					ublas::vector<double>& Dxz_cell,
					Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  
  // Calculate cell based water flux.
  ublas::vector<double> qx_cell;
  ublas::vector<double> qz_cell;
  cell_based_flux (geo, q_edge, qx_cell, qz_cell);
  
  
  for (int c = 0; c < cell_size; c++)
    {
      const double Theta_cell = Theta (c);
      daisy_assert(Theta_cell > 0);
      const double qx = qx_cell (c);
      const double qz = qz_cell (c);
      const double q = sqrt (sqr (qx) + sqr (qz));
      const double tau = soil.tortuosity_factor (c, Theta_cell);
      const double alpha_L = soil.dispersivity (c);
      const double alpha_T = soil.dispersivity_transversal (c);
      
      if (q > 0)
        {
          Dxx_cell (c) = (alpha_L * sqr (qx) + alpha_T * sqr (qz) )
            / (q * Theta_cell) + diffusion_coefficient * tau;
          Dzz_cell (c) = (alpha_L * sqr (qz) + alpha_T * sqr (qx) )
            / (q * Theta_cell) + diffusion_coefficient * tau;   
          Dxz_cell (c) = (alpha_L - alpha_T) * qx * qz / (q * Theta_cell);
        }  
      else
        { 
          Dxx_cell (c) = diffusion_coefficient * tau;
          Dzz_cell (c) = diffusion_coefficient * tau;
          Dxz_cell (c) = 0.0;
        }
    }
}


void 
MsoltranrectMollerup::thetadiff_longtran (const GeometryRect& geo,
					  const ublas::vector<double>& Theta,
					  const ublas::vector<double>& Dxx_cell,
					  const ublas::vector<double>& Dzz_cell,
					  const ublas::vector<double>& Dxz_cell,
					  ublas::vector<double>& ThetaD_long,
					  ublas::vector<double>& ThetaD_tran)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  for (size_t e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);
      //Debug
      //std::cout << "e" << e << '\n';
      
      if (geo.edge_is_internal (e))
	{  
	  //Debug
	  //std::cout << "internal\n";
	  // Arithmetic average of Theta * D is used
	  const double ThetaD_long_from = Theta[from] 
	    * anisotropy_factor (geo, e, Dxx_cell[from], Dzz_cell[from]);
	  const double ThetaD_long_to = Theta[to] 
	    * anisotropy_factor (geo, e, Dxx_cell[to], Dzz_cell[to]); 
	  ThetaD_long[e] = (ThetaD_long_from + ThetaD_long_to) / 2.0;
	  ThetaD_tran[e] = (Theta[from] * Dxz_cell[from] + 
			    Theta[to] * Dxz_cell[to]) / 2.0;
	  //Debug
	  //std::cout << "ThetaD_long" << ThetaD_long[e] << "\n"; 
	  //std::cout << "ThetaD_tran" << ThetaD_tran[e] << "\n";
	}  
      else if (geo.cell_is_internal (from))
	{
	  //Debug
	  //std::cout << "External, from\n";
	  ThetaD_long[e] = Theta[from] 
	    * anisotropy_factor (geo, e, Dxx_cell[from], Dzz_cell[from]);
	  ThetaD_tran[e] = Theta[from] * Dxz_cell[from];
	  //Debug
	  //std::cout << "ThetaD_long" << ThetaD_long[e] << "\n"; 
	  //std::cout << "ThetaD_tran" << ThetaD_tran[e] << "\n";
	}
      else 
	{
	  //Debug
	  //std::cout << "External, to\n";
	  daisy_assert(geo.cell_is_internal (to));
	  ThetaD_long[e] = Theta[to] 
	    * anisotropy_factor (geo, e, Dxx_cell[to], Dzz_cell[to]);
	  ThetaD_tran[e] = Theta[to] * Dxz_cell[to];
	  //Debug
	  //std::cout << "ThetaD_long" << ThetaD_long[e] << "\n"; 
	  //std::cout << "ThetaD_tran" << ThetaD_tran[e] << "\n";
	}
    }
  //Debug 
  //std::cout << "Inside routine: ThetaD_long" << ThetaD_long << "\n"; 
  //std::cout << "Inside routine: ThetaD_tran" << ThetaD_tran << "\n";
}


void 
MsoltranrectMollerup::diffusion_long (const GeometryRect& geo,
				      const ublas::vector<double>& ThetaD_long,
				      ublas::matrix<double>& diff_long)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);
	  const double magnitude = geo.edge_area_per_length (e) 
            * ThetaD_long[e]; 
	  diff_long (from, from) -= magnitude;
	  diff_long (from, to) += magnitude;
	  diff_long (to, to) -= magnitude;
	  diff_long (to, from) += magnitude; 
	} 
    }
}


void 
MsoltranrectMollerup::diffusion_tran (const GeometryRect& geo,
				      const ublas::vector<double>& ThetaD_tran,
                                      ublas::matrix<double>& diff_tran)
{

  //copy from long....
  const size_t edge_size = geo.edge_size (); // number of edges  
    
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  /*
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);
	  const double magnitude = geo.edge_area_per_length (e) * D_tran[e]; 
	  diff_tran (from, from) -= magnitude;
	  diff_tran (from, to) += magnitude;
	  diff_tran (to, to) -= magnitude;
	  diff_tran (to, from) += magnitude;
	  */ 
	} 
    }
}



void 
MsoltranrectMollerup::advection (const GeometryRect& geo,
				 const ublas::vector<double>& q_edge,
				 ublas::matrix<double>& advec)  
{
  const size_t edge_size = geo.edge_size (); // number of edges  

  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  const double value = geo.edge_area (e) * q_edge[e];
	  //Equal weighting
	  //advec (from, from) += 0.5*value;
	  //advec (from, to) += 0.5**value;
	  //advec (to, from) -= 0.5*value;
	  //advec (to, to) -= 0.5*value;

	  //Upstream formulation 
	  //advec (from, from) += 1.0*value;
	  //advec (to, from) -= 1.0*value;

	  //More flexible
	  //Equal weight: upstream_weight = 0.5
	  //Upstr weight: upstream_weight = 1.0
	  const double upstream_weight = 0.5;
	  const double alpha = (q_edge[e] >= 0) 
	    ? upstream_weight 
	    : 1.0 - upstream_weight;
	  advec (from, from) += alpha*value;
	  advec (from, to)   += (1.0-alpha)*value;
	  advec (to, from)   -= alpha*value;
	  advec (to, to)     -= (1.0-alpha)*value;
	} 
    }
}


void 
MsoltranrectMollerup::Neumann_expl (const size_t cell,
                                    const double area, 
                                    const double in_sign,
                                    const double J, 
                                    ublas::vector<double>& B_vec)
{
  B_vec (cell) = J * area * in_sign;   //J*in_sign pos for fluc into domain (cell)  
  //Debug
  //std::cout << "Neumann expl, cell no " << cell << '\n';
  //std::cout << "J = " << J << '\n';
  //std::cout << "in_sign = " << in_sign << '\n';  
  //std::cout << "B_vec(cell) = " << B_vec (cell) << '\n';
}


void 
MsoltranrectMollerup::Neumann_impl (const size_t cell,
                                    const double area, 
                                    const double in_sign,
                                    const double q, 
                                    ublas::banded_matrix<double>& B_mat)
{
  daisy_assert (q * in_sign <= 0.0);
  B_mat (cell, cell) = q * area * in_sign;  // q * in_sign pos for flux into domain  
  //Debug
  //std::cout << "Neumann impl, cell no " << cell << '\n';
  //std::cout << "q = " << q << '\n';
  //std::cout << "in_sign = " << in_sign << '\n';  
  //std::cout << "B_mat(cell,cell) = " << B_mat (cell, cell) << '\n';
}


void 
MsoltranrectMollerup::Dirichlet_long (const size_t cell,
                                      const double area, 
                                      const double area_per_length, 
                                      const double in_sign,
                                      const double ThetaD_long,
                                      const double C_border,
                                      const double C_cell,
                                      const double q,
                                      double& J,
                                      ublas::banded_matrix<double>& diffm_long_mat,
                                      ublas::vector<double>& diffm_long_vec, 
                                      ublas::banded_matrix<double>& advecm) 
{
  // Boundary advection
  const double value = area  * q;
  advecm (cell, cell) -= in_sign * value;  //q*in_sign pos for inflow
  
  //Boundary longitudinal diffusion
  const double D_area_per_length = ThetaD_long * area_per_length;
  diffm_long_mat (cell, cell) -= D_area_per_length;
  
  const double diffm_long_vec_val = D_area_per_length * C_border;
  diffm_long_vec (cell) += diffm_long_vec_val;
  
  //Calculate fluxes 
  J = in_sign * (-value 
		 -D_area_per_length * C_cell
		 + diffm_long_vec_val) / area;
}


void 
MsoltranrectMollerup::lowerboundary (const GeometryRect& geo,
                                     const bool isflux,
                                     const double C_border,
				     const std::vector<double>& C,
                                     const ublas::vector<double>& q_edge,
				     const ublas::vector<double>& ThetaD_long,
				     std::vector<double>& J,
                                     ublas::banded_matrix<double>& B_mat,
                                     ublas::vector<double>& B_vec,
                                     ublas::banded_matrix<double>
                                     & diffm_long_mat, 
                                     ublas::vector<double>& diffm_long_vec,
				     ublas::banded_matrix<double>& advecm)
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
      
      if (isflux)               // Flux BC
        {
          const bool influx = in_sign * q_edge (edge) > 0;
          if (influx)
            {
              J[edge] = C_border * q_edge (edge); 
              Neumann_expl (cell, area, in_sign, J[edge], B_vec);
            }
          else
            {
              Neumann_impl (cell, area, in_sign, q_edge (edge), B_mat);
            }
        }
      else                      // C_Border. BC
        {
          // write something
	  Dirichlet_long (cell, area, area_per_length, in_sign, 
			  ThetaD_long (cell),
			  C_border, C[cell], q_edge (edge),  J[edge],
			  diffm_long_mat, diffm_long_vec, advecm);  
        }
    }
}
   

#if 0 
case Groundwater::pressure:
{
  const double value = -K (cell) * geo.edge_area_per_length (edge);
  const double pressure =  groundwater.table () - geo.zplus (cell);
  Dirichlet (edge, cell, area, in_sign, sin_angle, 
	     K (cell), h (cell),
	     value, pressure,
	     dq, Dm_mat, Dm_vec, Gm);
}
break;
case Groundwater::lysimeter:
if (active_lysimeter[cell])
{
  const double value = -K (cell) * geo.edge_area_per_length (edge);
  const double pressure =  0.0;
  Dirichlet (edge, cell, area, in_sign, sin_angle,
	     K (cell), h (cell),
	     value, pressure, dq, Dm_mat, Dm_vec, Gm);
}
break;
default:
daisy_panic ("Unknown groundwater type");
#endif 


void 
MsoltranrectMollerup::upperboundary (const GeometryRect& geo,
				     std::vector<double>& J,
				     ublas::vector<double>& B_vec,
                                     Treelog& msg)
{
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      //Debug 
      //std::cout << "Edge no" << edge << '\n';
      //std::cout << "J" << J[edge] << '\n';
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign < 0);
      const double area = geo.edge_area (edge);
      Neumann_expl (cell, area, in_sign, J[edge], B_vec);
    }
}

void MsoltranrectMollerup::fluxes (const GeometryRect& geo,
                                   const ublas::vector<double>& q_edge,
                                   const ublas::vector<double>& ThetaD_long,
                                   const ublas::vector<double>& ThetaD_tran,
                                   const ublas::vector<double>& C,
                                   ublas::vector<double>& dJ) 
{
  
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))   //Do something else on outher boundaries
	{
          const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  
          //--- Advective part ---
          const double upstream_weight = 0.5;  //Should be taken from  outside
          const double alpha = (q_edge[e] >= 0) 
	    ? upstream_weight 
	    : 1.0 - upstream_weight;
          dJ[e] = alpha * q_edge[e] * C[from] 
            + (1.0-alpha)*C[to];
          
          //--- Diffusive part --- 
          const double gradient = geo.edge_area_per_length (e) *
            (C[to] - C[from]);
          dJ[e] += ThetaD_long[e]*gradient;  //Longitudinal diffusion

          dJ[e] += 0.0;                      //Transversal diffusion
	}
      else //edge is external
        {
          
        }
    }
}


void MsoltranrectMollerup::flow (const GeometryRect& geo, 
				 const Soil& soil, 
				 const SoilWater& soil_water, 
				 const std::string& name,
				 std::vector<double>& M, 
                                 std::vector<double>& C, 
				 const std::vector<double>& S, 
				 std::vector<double>& J, 
				 const double C_below,
				 Adsorption& adsorption,
				 double diffusion_coefficient,
				 const double dt,
				 Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
 
  // Solution old and new
  ublas::vector<double> C_old (cell_size);
  for (int c = 0; c < cell_size; c++)
    C_old (c) = C[c];
  ublas::vector<double> C_new (cell_size);
  
  //Water content old and new 
  ublas::vector<double> Theta_cell_old (cell_size);	
  for (int c = 0; c < cell_size; c++)
    Theta_cell_old (c) = soil_water.Theta_old (c);
  ublas::vector<double> Theta_cell (cell_size);	
  for (int c = 0; c < cell_size; c++)
    Theta_cell (c) = soil_water.Theta (c);

  //Average water content in large timestep
  ublas::vector<double> Theta_cell_avg (cell_size);     //Using avg cell size
  Theta_cell_avg = 0.5 * (Theta_cell + Theta_cell_old);
  
  //Flux in timestep
  ublas::vector<double> q_edge (edge_size);	
  for (int e = 0; e < edge_size; e++)
    q_edge (e) = soil_water.q (e);
  
  //For stabilizers
  //ublas::vector<double> Theta_edge = ublas::zero_vector<double> (edge_size);

  // BUG: Adsorption done wrong.
  std::vector<double> Ads (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      Ads[c] = M[c] - C[c] * soil_water.Theta_old (c);
      if (Ads[c] < 0)
	{
	  daisy_assert (approximate (M[c], C[c] * soil_water.Theta_old (c)));
	  Ads[c] = 0.0;
	}
    }
  
  //Cell diffusion tensor
  ublas::vector<double> Dxx_cell (cell_size);
  ublas::vector<double> Dzz_cell (cell_size);
  ublas::vector<double> Dxz_cell (cell_size);
  
  //Calculate cell based diffusion based on average water content
  diffusion_tensor (geo, soil, q_edge, Theta_cell_avg, diffusion_coefficient,
		    Dxx_cell, Dzz_cell, Dxz_cell, msg);
  
  //Theta * D - old and new and average 
  ublas::vector<double> ThetaD_long_old (edge_size); 
  ublas::vector<double> ThetaD_tran_old (edge_size);
  thetadiff_longtran (geo, Theta_cell_old, Dxx_cell, Dzz_cell, Dxz_cell,
		      ThetaD_long_old, ThetaD_tran_old);
  ublas::vector<double> ThetaD_long (edge_size); 
  ublas::vector<double> ThetaD_tran (edge_size);
  thetadiff_longtran (geo, Theta_cell, Dxx_cell, Dzz_cell, Dxz_cell,
		      ThetaD_long, ThetaD_tran);
  
  ublas::vector<double> ThetaD_long_avg (edge_size); 
  ublas::vector<double> ThetaD_tran_avg (edge_size);
  thetadiff_longtran (geo, Theta_cell_avg, Dxx_cell, Dzz_cell, Dxz_cell,
		      ThetaD_long_avg, ThetaD_tran_avg);
 
    
  //Begin small timestep stuff  
  enum stabilizing_method_t { None, Timestep_reduction, Streamline_diffusion };
  //stabilizing_method_t stabilizing_method = None;
  stabilizing_method_t stabilizing_method = Timestep_reduction;
  double dt_min = 1e-10;
  double gamma_stabilization = 10.0;
  int nddt;        //number of small timesteps

  
  switch (stabilizing_method)
    {
    case None:
      {
        //No stabilization!!!
        std::cout << "No stabilization\n";
        nddt = 1;    //Number of small timesteps 
        break;
      }

    case Timestep_reduction:
      {
        //Use smaller time steps
        std::cout << "Smaller timesteps\n";      
          
        ublas::vector<double> Theta_edge 
          = ublas::zero_vector<double> (edge_size);
        edge_watercontent (geo, Theta_cell_avg, Theta_edge);      
      
        double dt_PeCr;
        double dt_PeCr_min = 2*dt;
      
        for (int e = 0; e < edge_size; e++)
          {
            if (fabs(q_edge[e]) > 0)
              dt_PeCr = gamma_stabilization * ThetaD_long_avg[e] *
                Theta_edge[e]/(q_edge[e]*q_edge[e]);
            else 
              dt_PeCr = 2*dt;
	  
            if (dt_PeCr < dt_PeCr_min)
              dt_PeCr_min = dt_PeCr;
          }

        std::cout << "dt_PeCr_min = " << dt_PeCr_min << '\n';
        
        //Number of small timesteps 
        int divres = double2int(dt/dt_PeCr_min);
        double remainder = dt - divres*dt_PeCr_min; 
        if (remainder <= dt_min*1e-3)
          nddt = divres;
        else 
          nddt = divres + 1;
        
        //Print out some results
        std::cout << "divres = " << divres << '\n';
        std::cout << "remainder = " << remainder << '\n';
        std::cout << "nddt = " << nddt << '\n';  
        break;
      }
    case Streamline_diffusion:
      {
        //Add some ekstra diffusion in the streamline 
        std::cout << "Smaller timesteps\n";      
        
        ublas::vector<double> Theta_edge 
          = ublas::zero_vector<double> (edge_size);
        edge_watercontent (geo, Theta_cell_avg, Theta_edge);
      
        for (int e = 0; e < edge_size; e++)
          {
            const double PeCr = q_edge[e]*q_edge[e] * Theta_edge[e] * dt
              / ThetaD_long[e];
            if (PeCr > gamma_stabilization)    //Need to ad some extra diffusion
              ThetaD_long_avg[e] 
                = q_edge[e] * q_edge[e] * dt / Theta_edge[e];     
          }
        nddt = 1; //Dont use smaller timesteps 
        break;
      }
    } 
  
  //Calculate size of smal timesteps
  double ddt = dt/nddt;
  std::cout << "ddt = " << ddt << '\n';
  
  
  //Initialise water content stuff
  //ublas::banded_matrix<double>QTheta_mat_old (cell_size, cell_size, 0 ,0);
  //ublas::banded_matrix<double>QTheta_mat (cell_size, cell_size, 0, 0); 
  // for (int c = 0; c < cell_size; c++)
  //  {
  //    QTheta_mat_old (c, c) = geo.cell_volume (c) * Theta_cell_old (c);
  //    QTheta_mat (c, c) = geo.cell_volume (c) * Theta_cell (c) ;
  //  }

    
  //interpol(Kurt_start, Kurt_end, 1.0, 1.0, Kurt_res);
  //std::cout << "Kurt_res" << Kurt_res << '\n';
    
  //--------------------------------------
  //--- For moving in/out of tick loop ---
  //--------------------------------------

  //Initialize longitudinal diffusion matrix - old and new
  ublas::matrix<double> diff_long_old 
    = ublas::zero_matrix<double> (cell_size, cell_size);
  diffusion_long (geo, ThetaD_long_old, diff_long_old);  	   
  ublas::matrix<double> diff_long 
    = ublas::zero_matrix<double> (cell_size, cell_size);
  diffusion_long (geo, ThetaD_long, diff_long);    

  //Initialize transversal diffusion matrix
  ublas::matrix<double> diff_tran 
    = ublas::zero_matrix<double> (cell_size, cell_size);
  diffusion_tran (geo, ThetaD_tran, diff_tran); 
  
  
  //--- Things that not changes in smal timesteps --- 
 
  //Advection
  ublas::matrix<double> advec  
    = ublas::zero_matrix<double> (cell_size, cell_size);
  advection (geo, q_edge, advec);  
  
  //Sink term
   ublas::vector<double> S_vol (cell_size); // sink term 
  for (size_t cell = 0; cell != cell_size ; ++cell) 
    S_vol (cell) = S[cell] * geo.cell_volume (cell);
  
  //Boundary matrices and vectors  
  ublas::banded_matrix<double> B_mat (cell_size, cell_size, 0, 0); 
  for (int c = 0; c < cell_size; c++)
    B_mat (c, c) = 0.0;
  ublas::vector<double> B_vec = ublas::zero_vector<double> (cell_size); 
  ublas::banded_matrix<double>  diffm_long_mat (cell_size, cell_size,      
						0, 0); // Dir bc
  for (int c = 0; c < cell_size; c++)
    diffm_long_mat (c, c) = 0.0;
  
  ublas::vector<double> diffm_long_vec (cell_size); // Dir bc
  diffm_long_vec = ublas::zero_vector<double> (cell_size);
  ublas::banded_matrix<double> advecm (cell_size, cell_size, 0, 0);   
  for (int c = 0; c < cell_size; c++)
    advecm (c, c) = 0.0;
  
  //Debug - flow c ndition!!!
  //J[0] = -0.5; 
  //J[101] = -0.5;
  upperboundary (geo, J, B_vec, msg);

  //XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  //bool isflux = true;
  bool isflux = false;
  double C_border = 1;
  lowerboundary (geo, isflux, C_border, C, q_edge, ThetaD_long, J, B_mat, 
		 B_vec, diffm_long_mat, diffm_long_vec, advecm);
  //XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  //-----------------------------------------------------------------

 
  
  // Solver parameter , gamma
  // gamma = 0      : Backward Euler 
  // gamma = 0.5    : Crank - Nicholson
  double gamma = 0.5;
   
  //solver type for dc
  bool simple_dcthetadt = true;
     

  //Initialize A-matrix (left hand side)
  ublas::matrix<double> A (cell_size, cell_size);  
  
  //Initialize b-vector (right hand side)
  ublas::vector<double> b (cell_size);   
  ublas::matrix<double> b_mat (cell_size, cell_size);  

  //-------------------------------------------
  //--- End, For moving in/out of tick loop ---
  //-------------------------------------------
  
  ublas::vector<double> Theta_cell_n (cell_size);	
  Theta_cell_n = Theta_cell_old;
  ublas::vector<double> Theta_cell_np1 (cell_size);	
  
  ublas::banded_matrix<double>QTheta_mat_n (cell_size, cell_size, 0 ,0);
  for (int c = 0; c < cell_size; c++)
    QTheta_mat_n (c, c) = geo.cell_volume (c) * Theta_cell_n (c);
  ublas::banded_matrix<double>QTheta_mat_np1 (cell_size, cell_size, 0, 0);


  ublas::vector<double> dJ = ublas::zero_vector<double> (edge_size);

  double dtime = 0.0;
  for (int dtstep = 0; dtstep < nddt; dtstep++)
    {
      dtime += ddt;       //update time 
      std::cout << "dtime = " << dtstep << '\n';
      
      //Calculate water content 
      interpol(Theta_cell_old, Theta_cell, dt, dtime, Theta_cell_np1);
      
      for (int c = 0; c < cell_size; c++)
        QTheta_mat_np1 (c, c) = geo.cell_volume (c) * Theta_cell_np1 (c);
      
      
      if (simple_dcthetadt)
        {
          A = (1.0 / dt) * QTheta_mat_np1          // dtheta/dt
            - gamma * diff_long                    // long diffusion
            + gamma * advec                        // advec
            - gamma * B_mat                        // impl Neumann BC 
            - gamma * diffm_long_mat               // Dirichlet BC
            + gamma * advecm;                      // Dirichlet BC
          
          b_mat =  (1.0 / dt) * QTheta_mat_n 
            + (1 - gamma) * diff_long_old
            - (1 - gamma) * advec 
            + (1 - gamma) * B_mat
            + (1 - gamma) * diffm_long_mat
            - (1 - gamma) * advecm;
      
          b = prod (b_mat, C_old)
            + B_vec                                    
            + diffm_long_vec;                      //Dirichlet BC
          //- S_vol;                               //Sink term        
        }
      else  
        {
          A = A;
          b = b;     	
        }
      
      // Solve Ax=b (maybe)
      ublas::permutation_matrix<double> piv (cell_size);
      const bool singular = ublas::lu_factorize(A, piv);
      daisy_assert (!singular);
      ublas::lu_substitute (A, piv, b); // b now contains solution 
  
      C_new = b; // new solution :-)

      //Solution checks???
      //Calculate and sum up fluxes 
      
      
        
      fluxes (geo, q_edge, ThetaD_long, ThetaD_tran, C_new, dJ); 


      


      //Update Theta and QTheta
      Theta_cell_n = Theta_cell_np1;
      QTheta_mat_n = QTheta_mat_np1;

    } //End small timestep loop


  //debug Print new solution
  std::ostringstream tmp;
  tmp << "C_new" << C_new;
  msg.message (tmp.str ());
 
  // Write solution into C (std::vec)
  for (int c=0; c < cell_size; c++)
    {
      C[c] = C_new (c); 
      //daisy_assert (C[c] >= 0.0);
    }

  // BUG: Adsorption done wrong.
  for (size_t c = 0; c < cell_size; c++)
    {
      // We use the old absorbed stuff plus the new dissolved stuff.
      M[c] = Ads[c] + Theta_cell (c) * C[c];
      daisy_assert (M[c] >= 0.0);
      
      // We calculate new C by assumining instant absorption.
      C[c] = adsorption.M_to_C (soil, Theta_cell (c), c, M[c]);

      // Check that it goes both ways.
      if (iszero (C[c]))
	daisy_assert (iszero (M[c]));
      else
	daisy_assert (approximate (M[c], 
				   adsorption.C_to_M (soil, Theta_cell (c),
						      c, C[c])));
    }

  // BUG: No J for inner nodes.

}

void 
MsoltranrectMollerup::output (Log&) const
{ }

MsoltranrectMollerup::MsoltranrectMollerup (Block& al)
  : Msoltranrect (al)
  
{ }


//* place something like this inside MsoltranrectMollerup

/*
UZRectMollerup::UZRectMollerup (Block& al)
  : UZRect (al),
    edge_arithmetic_height (al.number ("edge_arithmetic_height")),
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    use_forced_T (al.check ("forced_T")),
    forced_T (al.number ("forced_T", -42.42e42)),
    debug (al.integer ("debug"))
{ }

*/





MsoltranrectMollerup::~MsoltranrectMollerup ()
{ }

void 
MsoltranrectMollerup::load_syntax (Syntax&, AttributeList&)
{ }

const AttributeList& 
Msoltranrect::default_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      MsoltranrectMollerup::load_syntax (dummy, alist);
      alist.add ("type", "Mollerup");
    }
  return alist;
}

static struct MsoltranrectMollerupSyntax
{
  static Model& make (Block& al)
  { return *new MsoltranrectMollerup (al); }

  MsoltranrectMollerupSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "\
Coupled vertical and horizontal transport.\n\
See Mollerup 2007 for details.");
    MsoltranrectMollerup::load_syntax (syntax, alist);
 
    Librarian::add_type (Msoltranrect::component, "Mollerup", alist, syntax, &make);
  }
} MsoltranrectMollerup_syntax;


