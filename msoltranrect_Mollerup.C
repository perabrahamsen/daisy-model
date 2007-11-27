// msoltranrect_Mollerup.C --- Coupled vertical and horizontal transport.
// 
// Copyright 2007 Mikkel Mollerup, Per Abrahamsen and KVL.
//
// This file is part of Daisy.
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
#include "alist.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "mathlib.h"
#include <sstream>

// Uncomment for fast code that does not catches bugs.
#define BOOST_UBLAS_NDEBUG

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
  const bool enable_boundary_diffusion;

  // Keep track of edge types in small time steps.
  enum edge_type_t { Unhandled, Internal, Neumann_explicit_upper,
                     Neumann_explicit_lower, Neumann_implicit, Dirichlet };
 
  // Water flux.
  static void cell_based_flux (const GeometryRect& geo,  
			       const ublas::vector<double>& q_edge,
                               ublas::vector<double>& qx_cell,
                               ublas::vector<double>& qz_cell);

  // Edge water content 
  static void edge_water_content (const GeometryRect& geo,
				 const ublas::vector<double>& Theta_cell,
				 ublas::vector<double>& Theta_edge);
  
  // Interpolation function for vectors (for water content)
  static void interpol (const ublas::vector<double>& V_start,
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
  
  static void thetadiff_xx_zz_xz_zx (const GeometryRect& geo,
                                     const ublas::vector<double>& Theta,
                                     const ublas::vector<double>& Dxx_cell,
                                     const ublas::vector<double>& Dzz_cell,
                                     const ublas::vector<double>& Dxz_cell,
                                     ublas::vector<double>& ThetaD_xx_zz,
                                     ublas::vector<double>& ThetaD_xz_zx);
  
  static void diffusion_xx_zz (const GeometryRect& geo,
                               const ublas::vector<double>& ThetaD_xx_zz,
                               ublas::matrix<double>& diff_xx_zz);
  
  static void diffusion_xz_zx (const GeometryRect& geo,
                               const ublas::vector<double>& ThetaD_xz_zx,      
                               ublas::matrix<double>& diff_xz_zx);

  static void advection (const GeometryRect& geo,
			 const ublas::vector<double>& q_edge,
			 ublas::matrix<double>& advec);
  
  static void Neumann_expl (const size_t cell, const double area, 
                            const double in_sign, const double J, 
                            ublas::vector<double>& B_vec);

  static void Neumann_impl (const size_t cell, const double area, 
                            const double in_sign, const double q, 
                            ublas::banded_matrix<double>& B_mat);
  
  static void Dirichlet_expl(const size_t cell,
                             const double area,
                             const double area_per_length, 
                             const double in_sign,
                             const double ThetaD_xx_zz,
                             const double C_border,
                             const double C_cell,
                             const double q,
                             const bool enable_boundary_diffusion,
                             ublas::vector<double>& B_dir_vec);

  static void Dirichlet_xx_zz (const size_t cell,
                               const double area, 
                               const double area_per_length, 
                               const double in_sign,
                               const double ThetaD_xx_zz,
                               const double C_border,
                               const double q,
                               ublas::banded_matrix<double>& diffm_xx_zz_mat,
                               ublas::vector<double>& diffm_xx_zz_vec, 
                               ublas::banded_matrix<double>& advecm_mat,
                               ublas::vector<double>& advecm_vec); 

  static void lowerboundary_new (const GeometryRect& geo,
                                 const bool isflux,
                                 const double C_border,
                                 const ublas::vector<double>& q_edge,
                                 const ublas::vector<double>& ThetaD_xx_zz,
                                 const ublas::vector<double>& C,
                                 const bool enable_boundary_diffusion,
                                 std::vector<edge_type_t>& edge_type,      
                                 ublas::banded_matrix<double>& B_mat,
                                 ublas::vector<double>& B_vec,
                                 ublas::vector<double>& B_dir_vec);
  
  static void lowerboundary (const GeometryRect& geo,
			     const bool isflux,
			     const double C_border,
                             const ublas::vector<double>& q_edge,
                             const ublas::vector<double>& ThetaD_xx_zz,
                             std::vector<edge_type_t>& edge_type,
                             ublas::banded_matrix<double>& B_mat,
                             ublas::vector<double>& B_vec,
                             ublas::banded_matrix<double>& diffm_xx_zz_mat, 
                             ublas::vector<double>& diffm_xx_zz_vec,
			     ublas::banded_matrix<double>& advecm_mat,
                             ublas::vector<double>& advecm_vec);

  static double Dirichlet_timestep_new (const GeometryRect& geo,
                                        const ublas::vector<double>& ThetaD_xx_zz,
                                        const double dt);
  
  static double Dirichlet_timestep (const GeometryRect& geo,
                                    const double C_border,
                                    const ublas::vector<double>& ThetaD_xx_zz,
                                    const ublas::vector<double>& C,
                                    const double dt);
  
  static void upperboundary (const GeometryRect& geo,
                             std::vector<edge_type_t>& edge_type,
                             const std::vector<double>& J,
                             ublas::vector<double>& B_vec,
                             Treelog& msg);

  static void fluxes_new (const GeometryRect& geo,
                          const std::vector<edge_type_t>& edge_type,
                          const ublas::vector<double>& q_edge,
                          const ublas::vector<double>& ThetaD_xx_zz,
                          const ublas::vector<double>& ThetaD_xz_zx,
                          const ublas::vector<double>& C,
                          const double C_below,
                          const ublas::vector<double>& B_dir_vec,
                          ublas::vector<double>& dJ); 

  static void fluxes (const GeometryRect& geo,
                      const std::vector<edge_type_t>& edge_type,
                      const ublas::vector<double>& q_edge,
                      const ublas::vector<double>& ThetaD_xx_zz,
                      const ublas::vector<double>& ThetaD_xz_zx,
                      const ublas::vector<double>& C,
                      const double C_below,
                      ublas::vector<double>& dJ); 

  // Solute.
  void flow (const GeometryRect& geo, 
             const Soil& soil, 
             const SoilWater& soil_water, 
             symbol name,
             std::vector<double>& M, 
             std::vector<double>& C, 
             const std::vector<double>& S, 
             std::vector<double>& J, 
	     const double C_below,
	     const bool flux_below,
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
MsoltranrectMollerup::edge_water_content 
/**/ (const GeometryRect& geo,
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
MsoltranrectMollerup::thetadiff_xx_zz_xz_zx
/**/ (const GeometryRect& geo,
      const ublas::vector<double>& Theta,
      const ublas::vector<double>& Dxx_cell,
      const ublas::vector<double>& Dzz_cell,
      const ublas::vector<double>& Dxz_cell,
      ublas::vector<double>& ThetaD_xx_zz,
      ublas::vector<double>& ThetaD_xz_zx)
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
          const double ThetaD_xx_zz_from = Theta[from] 
	    * anisotropy_factor (geo, e, Dxx_cell[from], Dzz_cell[from]);
	  const double ThetaD_xx_zz_to = Theta[to] 
	    * anisotropy_factor (geo, e, Dxx_cell[to], Dzz_cell[to]); 
	  ThetaD_xx_zz[e] = (ThetaD_xx_zz_from + ThetaD_xx_zz_to) / 2.0;
	  ThetaD_xz_zx[e] = (Theta[from] * Dxz_cell[from] + 
                             Theta[to] * Dxz_cell[to]) / 2.0;
	}  
      else if (geo.cell_is_internal (from))
	{
          ThetaD_xx_zz[e] = Theta[from] 
	    * anisotropy_factor (geo, e, Dxx_cell[from], Dzz_cell[from]);
	  ThetaD_xz_zx[e] = Theta[from] * Dxz_cell[from];
        }
      else 
	{
          daisy_assert(geo.cell_is_internal (to));
	  ThetaD_xx_zz[e] = Theta[to] 
	    * anisotropy_factor (geo, e, Dxx_cell[to], Dzz_cell[to]);
	  ThetaD_xz_zx[e] = Theta[to] * Dxz_cell[to];
        }
    }
}


void 
MsoltranrectMollerup::diffusion_xx_zz (const GeometryRect& geo,
                                       const ublas::vector<double>& ThetaD_xx_zz,
                                       ublas::matrix<double>& diff_xx_zz)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);
	  const double magnitude = geo.edge_area_per_length (e) 
            * ThetaD_xx_zz[e]; 
	  diff_xx_zz (from, from) -= magnitude;
	  diff_xx_zz (from, to) += magnitude;
	  diff_xx_zz (to, to) -= magnitude;
	  diff_xx_zz (to, from) += magnitude; 
	} 
    }
}


void 
MsoltranrectMollerup::diffusion_xz_zx (const GeometryRect& geo,
                                       const ublas::vector<double>& ThetaD_xz_zx,
                                       ublas::matrix<double>& diff_xz_zx)
{

  const size_t edge_size = geo.edge_size (); // number of edges  
    
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
          const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);
	  
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
          const double sign =  length / (dcz + dcx);

          // On the border, we average over two cell. 
          // For interior corners, over four cells.
          const double A_magnitude = A_is_border 
            ? magnitude / 2.0
            : magnitude / 4.0;

          const double B_magnitude = B_is_border 
            ? magnitude / 2.0
            : magnitude / 4.0;

          for (size_t i = 0; i < A_cells.size (); i++)
            {
              diff_xz_zx (from, A_cells[i]) += A_magnitude * sign;
              diff_xz_zx (to, A_cells[i]) -= A_magnitude * sign;
            }

          for (size_t i = 0; i < B_cells.size (); i++)
            {
              diff_xz_zx (from, B_cells[i]) -= B_magnitude * sign;
              diff_xz_zx (to, B_cells[i]) += B_magnitude * sign;
            }
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
  B_vec (cell) = J * area * in_sign;   
  //J*in_sign pos for flux into domain (cell)  
}


void 
MsoltranrectMollerup::Neumann_impl (const size_t cell,
                                    const double area, 
                                    const double in_sign,
                                    const double q, 
                                    ublas::banded_matrix<double>& B_mat)
{
  daisy_assert (q * in_sign <= 0.0);
  B_mat (cell, cell) = q * area * in_sign; 
  // q * in_sign pos for flux into domain  
}


void
MsoltranrectMollerup::Dirichlet_expl(const size_t cell,
                                     const double area,
                                     const double area_per_length, 
                                     const double in_sign,
                                     const double ThetaD_xx_zz,
                                     const double C_border,
                                     const double C_cell,
                                     const double q,
                                     const bool enable_boundary_diffusion,
                                     ublas::vector<double>& B_dir_vec)
{

  //Estimate flux 
  double Q_advec_out;
  double Q_diff_out;
  double Q_out;


  // Boundary advection
  if (q*in_sign >= 0)     //Inflow
    Q_advec_out = - in_sign * q * area * C_border;
  else                    //Outflow 
    Q_advec_out = - in_sign * q * area * C_cell;
    
  

  if (enable_boundary_diffusion)
    {
      // Boundary xx_zz diffusion
      
       const double gradient =  area_per_length * (C_border - C_cell);
       Q_diff_out = -ThetaD_xx_zz * gradient;
       

       /*
       //the best until now ...
       Q_diff_out = -ThetaD_xx_zz * gradient;
       double ddt = 1.0/60.0;
       double V_cell = 0.1;
       double newC =  C_cell - Q_diff_out*ddt/V_cell;
       

       if (newC > C_border)   //some overshoot is ok... 
         Q_diff_out = 0.5 * V_cell * (C_cell-C_border)/ddt;  //almost stable - not best for only diff
       //Q_diff_out *= 0.5;   //svingende 
       else if (newC < 0.0)
         Q_diff_out = 0.5 * V_cell * C_cell/ddt;
       //Q_diff_out *= 0.5;   //svingende  */
       
      

      /*
      // Doesnt works well
      const double gradient_max = area_per_length * C_border; // Depending on in or outflow
      const double gradient =  area_per_length * (C_border - C_cell);
      
      std::cout << "gradient_max: " << gradient_max << '\n';
      std::cout << "gradient: " << gradient << '\n';
      
      double sign;
      if (gradient >= 0.0)
        sign = 1.0;
      else 
        sign = -1.0;
 
      if (fabs(gradient) > 0.5 * gradient_max)
        Q_diff_out = - 0.5 *sign * 0.1* ThetaD_xx_zz * gradient_max;
      else 
        Q_diff_out = - ThetaD_xx_zz * gradient;
      */
     
        
      /* //Long term oscilations      
      if (C_cell > C_border)
        Q_diff_out = 0.0;
      else 
        Q_diff_out -= ThetaD_xx_zz * gradient;
      */


       /* Not only diffusion very stable but too low diffusion
       Q_diff_out -= ThetaD_xx_zz * gradient;
       
       double ddt = 1.0/60.0;
       double V_cell = 0.1;
       double newC =  C_cell - Q_diff_out*ddt/V_cell;
       if (newC > 1.0*C_border)   //some overshoot is ok... 
         Q_diff_out = 0.0;
       */ 
       
       //
       
       
       /*
       //New one - elendig!
       Q_diff_out = -ThetaD_xx_zz * gradient;
       double Q_tot_out = Q_advec_out + Q_diff_out;
       double ddt = 1.0/60.0;
       double V_cell = 0.1;
       double newC =  C_cell - (Q_diff_out+Q_advec_out)*ddt/V_cell;
       
       std::cout << "C_cell" << C_cell << '\n';
       std::cout << "V_cell = " << V_cell << '\n';
       std::cout << "ddt = " << ddt << '\n';
       std::cout << "newC = " << newC << '\n';
       std::cout << "gradient = " << gradient << '\n';
       std::cout << "ThetaD_xx_zz = " << ThetaD_xx_zz << '\n';
       std::cout << "Q_diff_out = " << Q_diff_out << '\n';
       std::cout << "Q_advec_out = " << Q_advec_out << '\n';


       if (newC > C_border)    
       Q_tot_out = V_cell * (C_cell-C_border)/ddt;  //almost stable - not best for only diff
       
       else if (newC < 0.0)
       Q_tot_out = V_cell * C_cell/ddt;
       //Q_diff_out *= 0.5;   //svingende  
       
       Q_diff_out = 1.1*(Q_tot_out - Q_advec_out);
       */

    }
  else 
    Q_diff_out = 0.0; 
  
  
  // Boundary flux by advection and diffusion
  Q_out = Q_advec_out + Q_diff_out;
  
  // Write to Neumann-type vector 
  B_dir_vec (cell) = Q_out;
}



void 
MsoltranrectMollerup::Dirichlet_xx_zz 
/**/ (const size_t cell,
      const double area, 
      const double area_per_length, 
      const double in_sign,
      const double ThetaD_xx_zz,
      const double C_border,
      const double q,
      ublas::banded_matrix<double>& diffm_xx_zz_mat,
      ublas::vector<double>& diffm_xx_zz_vec, 
      ublas::banded_matrix<double>& advecm_mat,
      ublas::vector<double>& advecm_vec) 
{
  // Boundary advection
  // Use C_border for influx and C_cell for outflux 
  const double value = area * q;
  if (q*in_sign >= 0)     //Inflow
    advecm_vec (cell) -= in_sign * value * C_border; 
  else                    //Outflow 
    advecm_mat (cell, cell) -= in_sign * value;   
  
  //Boundary xx_zz diffusion
  const double D_area_per_length = ThetaD_xx_zz * area_per_length;
  diffm_xx_zz_mat (cell, cell) -= D_area_per_length;
  
  const double diffm_xx_zz_vec_val = D_area_per_length * C_border;
  diffm_xx_zz_vec (cell) += diffm_xx_zz_vec_val;

  //std::cout << "cell = " << cell << ", area_per_length = " << area_per_length
  //          << ", D_area_per_length = " << D_area_per_length << ", C_border = "
  //          << C_border << "\n";
}



void 
MsoltranrectMollerup::lowerboundary_new
/**/ (const GeometryRect& geo,
      const bool isflux,
      const double C_border,
      const ublas::vector<double>& q_edge,
      const ublas::vector<double>& ThetaD_xx_zz,
      const ublas::vector<double>& C,
      const bool enable_boundary_diffusion,
      std::vector<edge_type_t>& edge_type,      
      ublas::banded_matrix<double>& B_mat,
      ublas::vector<double>& B_vec,
      ublas::vector<double>& B_dir_vec)
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
                          q_edge (edge), enable_boundary_diffusion, B_dir_vec); 
        }
    }
}
 

void 
MsoltranrectMollerup::lowerboundary 
/**/ (const GeometryRect& geo,
      const bool isflux,
      const double C_border,
      const ublas::vector<double>& q_edge,
      const ublas::vector<double>& ThetaD_xx_zz,
      std::vector<edge_type_t>& edge_type,      
      ublas::banded_matrix<double>& B_mat,
      ublas::vector<double>& B_vec,
      ublas::banded_matrix<double>& diffm_xx_zz_mat, 
      ublas::vector<double>& diffm_xx_zz_vec,
      ublas::banded_matrix<double>& advecm_mat,
      ublas::vector<double>& advecm_vec)
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
	  Dirichlet_xx_zz (cell, area, area_per_length, in_sign, 
                           ThetaD_xx_zz (cell), C_border, 
                           q_edge (edge), diffm_xx_zz_mat, 
                           diffm_xx_zz_vec, advecm_mat, advecm_vec);  
        }
    }
}

double 
MsoltranrectMollerup::Dirichlet_timestep_new 
/**/ (const GeometryRect& geo,
      const ublas::vector<double>& ThetaD_xx_zz,
      const double dt)
{
  double ddt_dir = dt; 
  double ddt_dir_new = dt;
  
  const std::vector<int>& edge_below
    = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();
  
  for (size_t i = 0; i < edge_below_size; i++)
    {
      // For diffusion into the cell, only half of the volume of the 
      // conc difference between border and cell can be transported by 
      // diffusion over the boundary into the cell in a timestep. 
      //  
      // For diffusion out from the cell, only half of the volume of the
      // conc difference between cell and border can be transported by 
      // diffusion over the boundary out from the cell in a timestep. 
          
      const int edge = edge_below[i];
      const int cell = geo.edge_other (edge, Geometry::cell_below);
      const double area_per_length = geo.edge_area_per_length (edge);
      const double V_cell = geo.cell_volume (cell);
      
      //const double gradient =  area_per_length * (C_border - C_cell);
      //const double Q_diff_out = -ThetaD_xx_zz (edge) * gradient;
      //if  (Q_diff_out < 0) //Diff into cell
      //  ddt_dir_new = 0.5 * V_cell *  (C_cell-C_border) / Q_diff_out; 
      //else if  (Q_diff_out < 0) //Diff into cell
      //  ddt_dir_new = -0.5 * V_cell * (C_border-C_cell) / Q_diff_out; 
    
      if (ThetaD_xx_zz (edge) > 0)    //No diffusive transport if zero diff
        ddt_dir_new = 0.5 * V_cell / (ThetaD_xx_zz (edge) * area_per_length); 
      else 
        ddt_dir_new = ddt_dir;
      
      if (ddt_dir_new < ddt_dir)
        ddt_dir = ddt_dir_new;
    }
  return ddt_dir;
}

  
double 
MsoltranrectMollerup::Dirichlet_timestep
/**/ (const GeometryRect& geo,
      const double C_border,
      const ublas::vector<double>& ThetaD_xx_zz,
      const ublas::vector<double>& C,
      const double dt)
{
  double ddt_dir = dt; 
  
  //std::cout << "yes sir!! \n";  

  const std::vector<int>& edge_below
    = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();
    
  for (size_t i = 0; i < edge_below_size; i++)
    {
      // For diffusion into the cell, only half of the volume of the 
      // conc difference between border and cell can be transported by 
      // diffusion over the boundary into the cell in a timestep. 
      //  
      // For diffusion out from the cell, only half of the volume of the
      // conc difference between cell and border can be transported by 
      // diffusion over the boundary out from the cell in a timestep. 
          
      // The code is actally independent concs


      const int edge = edge_below[i];
      const int cell = geo.edge_other (edge, Geometry::cell_below);
      const double C_cell = C (cell);
      const double area_per_length = geo.edge_area_per_length (edge);
      const double V_cell = geo.cell_volume (cell);
      const double gradient =  area_per_length * (C_border - C_cell);
      const double Q_diff_out = -ThetaD_xx_zz (edge) * gradient;

      //std::cout << "dt " << dt << '\n';
      //std::cout << "V_cell " << V_cell << '\n';
      //std::cout << "gradient " << gradient << '\n';
      //std::cout << "Q_diff_out " << Q_diff_out << '\n'; 
          
      double ddt_dir_new = dt;

      if (Q_diff_out > 0)       //Diff out of cell
        ddt_dir_new = 0.5 * V_cell *  (C_cell-C_border) / Q_diff_out; 
      else if  (Q_diff_out < 0) //Diff into cell
        ddt_dir_new = -0.5 * V_cell * (C_border-C_cell) / Q_diff_out; 
          
      // std::cout << "ddt_dir_new " << ddt_dir_new << '\n';
          
      if (ddt_dir_new < ddt_dir)
        ddt_dir = ddt_dir_new;
    }

  return ddt_dir;
}




void 
MsoltranrectMollerup::upperboundary (const GeometryRect& geo,
                                     std::vector<edge_type_t>& edge_type,
				     const std::vector<double>& J,
                                     ublas::vector<double>& B_vec,
                                     Treelog& msg)
{
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign < 0);
      const double area = geo.edge_area (edge);
      Neumann_expl (cell, area, in_sign, J[edge], B_vec);
      edge_type[edge] = Neumann_explicit_upper;
    }
}


void 
MsoltranrectMollerup::fluxes_new (const GeometryRect& geo,
                                  const std::vector<edge_type_t>& edge_type, 
                                  const ublas::vector<double>& q_edge,
                                  const ublas::vector<double>& ThetaD_xx_zz,
                                  const ublas::vector<double>& ThetaD_xz_zx,
                                  const ublas::vector<double>& C,
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




void 
MsoltranrectMollerup::fluxes (const GeometryRect& geo,
                              const std::vector<edge_type_t>& edge_type, 
                              const ublas::vector<double>& q_edge,
                              const ublas::vector<double>& ThetaD_xx_zz,
                              const ublas::vector<double>& ThetaD_xz_zx,
                              const ublas::vector<double>& C,
                              const double C_below,
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
         
            //std::cout << "cell: " << cell << '\n';
            //std::cout << "in_sign:" << in_sign << '\n';
            //std::cout << "C[cell]:" << C[cell] << '\n';
            //std::cout << "C_below" << C_below << '\n';
            //std::cout << "gradient" << gradient << '\n';

            //Diffusive transport - xz_zx diffusion
            //Constant values along border direction ->
            //no flux 
          }
          break;
        }
    }
}


void
MsoltranrectMollerup::flow (const GeometryRect& geo, 
			    const Soil& soil, 
			    const SoilWater& soil_water, 
			    const symbol name,
			    std::vector<double>& M, 
			    std::vector<double>& C, 
			    const std::vector<double>& S, 
			    std::vector<double>& J, 
			    const double C_below,
			    const bool flux_below,
			    double diffusion_coefficient,
			    const double dt,
			    Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
 
  // Solution old
  ublas::vector<double> C_old (cell_size);
  for (int c = 0; c < cell_size; c++)
    C_old (c) = C[c];
  
  // Water content old and new 
  ublas::vector<double> Theta_cell_old (cell_size);	
  for (int c = 0; c < cell_size; c++)
    Theta_cell_old (c) = soil_water.Theta_old (c);
  ublas::vector<double> Theta_cell (cell_size);	
  for (int c = 0; c < cell_size; c++)
    Theta_cell (c) = soil_water.Theta (c);

  // Average water content in large timestep
  ublas::vector<double> Theta_cell_avg (cell_size);     //Using avg cell size
  Theta_cell_avg = 0.5 * (Theta_cell + Theta_cell_old);
  
  // Flux in timestep
  ublas::vector<double> q_edge (edge_size);	
  for (int e = 0; e < edge_size; e++)
    q_edge (e) = soil_water.q (e);
  
  
  //Cell diffusion tensor
  ublas::vector<double> Dxx_cell (cell_size);
  ublas::vector<double> Dzz_cell (cell_size);
  ublas::vector<double> Dxz_cell (cell_size);
  
  //Calculate cell based diffusion based on average water content
  diffusion_tensor (geo, soil, q_edge, Theta_cell_avg, diffusion_coefficient,
		    Dxx_cell, Dzz_cell, Dxz_cell, msg);
  
  //Theta * D - old and new and average 
  //ublas::vector<double> ThetaD_xx_zz_old (edge_size);                       //mmo 20071102 
  //ublas::vector<double> ThetaD_xz_zx_old (edge_size);                       //mmo 20071102
  //thetadiff_xx_zz_xz_zx (geo, Theta_cell_old, Dxx_cell, Dzz_cell, Dxz_cell, //mmo 20071102
  //		      ThetaD_xx_zz_old, ThetaD_xz_zx_old);                    //mmo 20071102
  //ublas::vector<double> ThetaD_xx_zz (edge_size);                           //mmo 20071102
  //ublas::vector<double> ThetaD_xz_zx (edge_size);                           //mmo 20071102
  //thetadiff_xx_zz_xz_zx (geo, Theta_cell, Dxx_cell, Dzz_cell, Dxz_cell,     //mmo 20071102
  //		      ThetaD_xx_zz, ThetaD_xz_zx);                            //mmo 20071102
  
  ublas::vector<double> ThetaD_xx_zz_avg (edge_size); 
  ublas::vector<double> ThetaD_xz_zx_avg (edge_size);
  thetadiff_xx_zz_xz_zx (geo, Theta_cell_avg, Dxx_cell, Dzz_cell, Dxz_cell,
                         ThetaD_xx_zz_avg, ThetaD_xz_zx_avg);
 
    
  //Begin small timestep stuff  
  enum stabilizing_method_t { None, Timestep_reduction, Streamline_diffusion };
  //const stabilizing_method_t stabilizing_method = Streamline_diffusion;
  const stabilizing_method_t stabilizing_method = Timestep_reduction;
  //const stabilizing_method_t stabilizing_method = None;
  const double ddt_min = 1e-10;
  const double gamma_stabilization = 10;

  std::ostringstream tmp_mmo;
  
  // Largest allowable timestep in loop.
  double ddt_max = dt;  

  double ddt; //size of small timestep

  switch (stabilizing_method)
    {
    case None:
      {
        //No stabilization!!!
        tmp_mmo << "No stabilization\n";
        ddt = dt;
        
        //msg.message(tmp_mmo.str ());
        //std::cout << "No stabilization\n";
        break;
      }

    case Timestep_reduction:
      {
        //Use smaller time steps
        tmp_mmo << "Smaller timesteps\n";      
        
        ublas::vector<double> Theta_edge 
          = ublas::zero_vector<double> (edge_size);
        edge_water_content (geo, Theta_cell_avg, Theta_edge);      
        
        double ddt_PeCr_min = dt;
        
        for (size_t e = 0; e < edge_size; e++)
          {
            if (iszero (q_edge[e]))
              continue;
            
            const double ddt_PeCr
              = gamma_stabilization * ThetaD_xx_zz_avg[e]
              * Theta_edge[e]/(q_edge[e]*q_edge[e]);
            if (ddt_PeCr < ddt_PeCr_min)
              ddt_PeCr_min = ddt_PeCr;
          }
    
        if (ddt_PeCr_min < ddt_max)
          ddt_max = ddt_PeCr_min;
        
        tmp_mmo << "ddt_PeCr_min: " << ddt_PeCr_min << '\n';
        
        if (!flux_below && enable_boundary_diffusion)
          {
            double ddt_dir  = Dirichlet_timestep_new (geo, ThetaD_xx_zz_avg, ddt_max);
            tmp_mmo << "ddt_dir: " << ddt_dir << '\n';
            
            if (ddt_dir < ddt_max)
              ddt_max = ddt_dir;
          }
        
                
        tmp_mmo << "ddt_max: " << ddt_max << '\n'; 
         
        if (ddt_max < ddt_min)
          ddt = ddt_min; // No timesteps smaller than ddt_min
        else 
          ddt = ddt_max; // Else use the maximum allowable timestep
              
        //tmp_mmo << "ddt: " << ddt << '\n';

       
        //Number of small timesteps 
        const int divres = double2int(dt/ddt);
        double remainder = dt - divres*ddt; 
        int nddt; //Number of small timesteps in a large timestep
        if (remainder <= ddt_min*1e-3)
          nddt = divres;
        else 
          nddt = divres + 1;
        
        ddt = dt/nddt;
        
        

        break;
      }
    case Streamline_diffusion:
      {
        //Add some ekstra diffusion in the streamline 
        tmp_mmo << "Streamline diffusion\n";      
        ddt = dt;
    
        ublas::vector<double> Theta_edge 
          = ublas::zero_vector<double> (edge_size);
        edge_water_content (geo, Theta_cell_avg, Theta_edge);
      
        for (size_t e = 0; e < edge_size; e++)
          {
            const double ThetaD_PeCr = q_edge[e]*q_edge[e] * dt
              / (Theta_edge[e] * gamma_stabilization);
            
            if (ThetaD_xx_zz_avg[e] < ThetaD_PeCr)  //Need extra diffusion
              ThetaD_xx_zz_avg[e] = ThetaD_PeCr;
          }
        break;
      }
    } 

  
  //--------------------------------------
  //--- For moving in/out of tick loop ---
  //--------------------------------------

  //Initialize xx_zz diffusion matrix - old and new
  //ublas::matrix<double> diff_xx_zz_old                       //mmo 20071102
  //  = ublas::zero_matrix<double> (cell_size, cell_size);     //mmo 20071102
  //diffusion_xx_zz (geo, ThetaD_xx_zz_old, diff_xx_zz_old);   //mmo 20071102	   
  //ublas::matrix<double> diff_xx_zz                           //mmo 20071102
  //  = ublas::zero_matrix<double> (cell_size, cell_size);     //mmo 20071102
  //diffusion_xx_zz (geo, ThetaD_xx_zz, diff_xx_zz);           //mmo 20071102 

  //Initialize xx_zz diffusion - average
  ublas::matrix<double> diff_xx_zz_avg 
    = ublas::zero_matrix<double> (cell_size, cell_size);
  diffusion_xx_zz (geo, ThetaD_xx_zz_avg, diff_xx_zz_avg);    

  //Initialize xz_zx diffusion matrix -average 
  ublas::matrix<double> diff_xz_zx_avg  
    = ublas::zero_matrix<double> (cell_size, cell_size);
  diffusion_xz_zx (geo, ThetaD_xz_zx_avg, diff_xz_zx_avg); 
  
  
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
  
  ublas::vector<double> B_dir_vec = ublas::zero_vector<double> (cell_size);
  
  ublas::banded_matrix<double>  diffm_xx_zz_mat (cell_size, cell_size,      
                                                 0, 0); // Dir bc
  for (int c = 0; c < cell_size; c++)
    diffm_xx_zz_mat (c, c) = 0.0;
  
  ublas::vector<double> diffm_xx_zz_vec (cell_size); // Dir bc
  diffm_xx_zz_vec = ublas::zero_vector<double> (cell_size);
  ublas::banded_matrix<double> advecm_mat (cell_size, cell_size, 0, 0);   
  for (int c = 0; c < cell_size; c++)
    advecm_mat (c, c) = 0.0;
  ublas::vector<double> advecm_vec (cell_size);
  advecm_vec = ublas::zero_vector<double> (cell_size);  

  //Debug - flow c ndition!!!
  //J[0] = -0.5;       //mmoxxx 
  //J[101] = -0.5;     //mmoxxx
  //xxxxxxxxx
  //Neumann_expl (0, 5.0, 1, 0.5, B_vec);  
  //Neumann_expl (1, 5.0, 1, 0.5, B_vec);  
  

  std::vector<edge_type_t> edge_type (edge_size, Unhandled);
  for (size_t e = 0; e < edge_size; e++)
    if (geo.edge_is_internal (e))
      edge_type[e] = Internal;


  upperboundary (geo, edge_type, J, B_vec, msg);


  //lowerboundary (geo, flux_below, C_below, q_edge, ThetaD_xx_zz_avg, 
  //               edge_type, B_mat, B_vec, diffm_xx_zz_mat, 
  //               diffm_xx_zz_vec, advecm_mat, advecm_vec);       
  

  // Solver parameter , gamma
  // gamma = 0      : Backward Euler 
  // gamma = 0.5    : Crank - Nicholson
  const double gamma = 0.5;
   
  //solver type for dc
  const bool simple_dcthetadt = true;
     

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
  
  ublas::banded_matrix<double> QTheta_mat_n (cell_size, cell_size, 0 ,0);
  for (int c = 0; c < cell_size; c++)
    QTheta_mat_n (c, c) = geo.cell_volume (c) * Theta_cell_n (c);
  ublas::banded_matrix<double> QTheta_mat_np1 (cell_size, cell_size, 0, 0);

  ublas::vector<double> C_n (cell_size);
  C_n = C_old;


  // Time left of current large timestep.
  double time_left = dt;
  // Time already processed of large timestep.
  double dtime = 0.0;

  while (dtime * 1.000001 < dt)
    {
      
      if (ddt * 1.0001 >= time_left)
        // We never use more time than is left.
        ddt = time_left;
      else if (ddt > time_left / 2.0)
        // If we use smaller timestep, only do half the rest in this try.
        // This should prevent orphans.
        ddt = time_left / 2.0;
      
            
      tmp_mmo << "ddt: " << ddt << '\n';


      time_left -= ddt;
      dtime += ddt;       //update time 
      
      //Calculate water content 
      interpol(Theta_cell_old, Theta_cell, dt, dtime, Theta_cell_np1);
      
      for (int c = 0; c < cell_size; c++)
        QTheta_mat_np1 (c, c) = geo.cell_volume (c) * Theta_cell_np1 (c);
      
      
      lowerboundary_new (geo, flux_below, C_below, q_edge, ThetaD_xx_zz_avg,
                         C_n, enable_boundary_diffusion, edge_type, B_mat,
                         B_vec, B_dir_vec);
      //lowerboundary (geo, flux_below, C_below, q_edge, ThetaD_xx_zz_avg, 
      //               edge_type, B_mat, B_vec, diffm_xx_zz_mat, 
      //               diffm_xx_zz_vec, advecm_mat, advecm_vec);       
  
     
      if (simple_dcthetadt)
        {
          A = (1.0 / ddt) * QTheta_mat_np1          // dtheta/ddt
            - gamma * diff_xx_zz_avg                // xx_zz diffusion
            - gamma * diff_xz_zx_avg                // xz_zx diffusion
            + gamma * advec                         // advec
            - gamma * B_mat                         // impl Neumann BC 
            - gamma * diffm_xx_zz_mat               // Dirichlet BC
            + gamma * advecm_mat;                   // Dirichlet BC
          
          b_mat =  (1.0 / ddt) * QTheta_mat_n 
            + (1 - gamma) * diff_xx_zz_avg 
            + (1 - gamma) * diff_xz_zx_avg 
            - (1 - gamma) * advec 
            + (1 - gamma) * B_mat
            + (1 - gamma) * diffm_xx_zz_mat
            - (1 - gamma) * advecm_mat;
   
          b = prod (b_mat, C_n)
            + B_vec                                 // expl Neumann BC
            - B_dir_vec                             // Dirichlet BC as Neumann
            + diffm_xx_zz_vec                       // Dirichlet BC
            - advecm_vec                            // Dirichlet BC 
            - S_vol;                                // Sink term        
        }
      else  
        {
	  daisy_notreached ();
          // A = ;
          // b = ;     	
        }
      
      // Solve Ax=b (maybe)
      ublas::permutation_matrix<double> piv (cell_size);
      const bool singular = ublas::lu_factorize(A, piv);
      daisy_assert (!singular);
      ublas::lu_substitute (A, piv, b); // b now contains solution 
      
      // C_n = C_np1
      C_n = b; // new solution :-)
      
            
      //Update fluxes 
      ublas::vector<double> dJ = ublas::zero_vector<double> (edge_size);
      //fluxes (geo, edge_type, q_edge, ThetaD_xx_zz_avg, ThetaD_xz_zx_avg,
      //        C_n, C_below, dJ); 
      fluxes_new (geo, edge_type, q_edge, ThetaD_xx_zz_avg, ThetaD_xz_zx_avg,
                  C_n, C_below, B_dir_vec, dJ); 
      
      for (int e=0; e<edge_size; e++)
        J[e] += dJ[e] * ddt/dt;
      
      //Update Theta and QTheta
      Theta_cell_n = Theta_cell_np1;
      QTheta_mat_n = QTheta_mat_np1;
      
      //debug Print new solution
      std::ostringstream tmp;
      //tmp_mmo << "C_n" << C_n;
      //msg.message (tmp.str ());
    } //End small timestep loop
  
  tmp_mmo << "C_n" << C_n;


  //debug Print new solution
  //std::ostringstream tmp;
  // tmp << "C_n" << C_n;
  //msg.message (tmp.str ());
 
  // Write solution into C (std::vector) and M
  for (size_t c=0; c < cell_size; c++)
    {
      C[c] = C_n (c); 
      M[c] = Theta_cell (c) * C[c];
      daisy_assert (C[c] >= 0.0);
      daisy_assert (M[c] >= 0.0);
    }
  
  // BUG: No J for inner nodes.
    msg.message(tmp_mmo.str ());
  
}

void 
MsoltranrectMollerup::output (Log&) const
{ }

MsoltranrectMollerup::MsoltranrectMollerup (Block& al)
  : Msoltranrect (al),
    enable_boundary_diffusion (al.flag ("enable_boundary_diffusion"))
{ }

MsoltranrectMollerup::~MsoltranrectMollerup ()
{ }

void 
MsoltranrectMollerup::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("enable_boundary_diffusion", Syntax::Boolean, Syntax::Const, "\
If this is set, diffusion over boundaries is enabled."); 
  alist.add ("enable_boundary_diffusion", true);
}

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

const AttributeList& 
Msoltranrect::reserve_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      MsoltranrectMollerup::load_syntax (dummy, alist);
      alist.add ("type", "Mollerup");
      alist.add ("enable_boundary_diffusion", false);
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
    alist.add ("description", "\
Coupled vertical and horizontal transport.\n\
See Mollerup 2007 for details.");
    MsoltranrectMollerup::load_syntax (syntax, alist);
 
    Librarian::add_type (Msoltranrect::component,
                         "Mollerup", alist, syntax, &make);
  }
} MsoltranrectMollerup_syntax;


