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

#if 0

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

  static double anisotropy_factor (const Geometry& geo, size_t edge, 
				   const double Dxx, 
				   const double Dzz);

  static void diffusion_tensor (const GeometryRect& geo, 
				const Soil& soil, 
				const ublas::vector<double>& q_edge, 
				const ublas::vector<double>& Theta,
				const double diffusion_coefficient,
				ublas::vector<double>& D_long,
				ublas::vector<double>& D_tran,
				Treelog& msg);

  static void diffusion_long (const GeometryRect& geo,
			      const ublas::vector<double>& D_long,	
			      ublas::matrix<double>& diff_long);

  static void advection (const GeometryRect& geo,
			 const ublas::vector<double>& q_edge,
			 ublas::banded_matrix<double>& advec);

  
  static void Neumann_expl (const size_t cell, const double area, 
                            const double in_sign, const double J, 
                            ublas::vector<double>& B_vec);

  static void Neumann_impl (const size_t cell, const double area, 
                            const double in_sign, const double q, 
                            ublas::banded_matrix<double>& B_mat);
  
  static void lowerboundary (const GeometryRect& geo,
			     const bool isflux,
			     const double conc,
			     const ublas::vector<double>& q_edge,
                             std::vector<double>& J,
                             ublas::banded_matrix<double>& B_mat,
                             ublas::vector<double>& B_vec,
                             ublas::banded_matrix<double>& Dlongm_mat, 
                             ublas::vector<double>& Dlongm_vec);


  static void upperboundary (const GeometryRect& geo,
              		     std::vector<double>& J,
                             ublas::vector<double>& B_vec,
                             Treelog& msg);



  // Solute.
  void flow (const GeometryRect& geo, 
             const Soil& soil, 
             const SoilWater& soil_water, 
             const std::string& name,
             std::vector<double>& M, 
             std::vector<double>& C, 
             const std::vector<double>& S, 
             std::vector<double>& J, 
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
					ublas::vector<double>& D_long,
					ublas::vector<double>& D_tran,
					Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  
  // Calculate cell based water flux.
  ublas::vector<double> qx_cell;
  ublas::vector<double> qz_cell;
  cell_based_flux (geo, q_edge, qx_cell, qz_cell);
  
  // Calculate diffusion matrix. 
  ublas::vector<double> Dxx_cell (cell_size);
  ublas::vector<double> Dzz_cell (cell_size);
  ublas::vector<double> Dxz_cell (cell_size);
  
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
   
  

  for (size_t e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);	 

      if (geo.edge_is_internal (e))
	{  
	  // Arithmetic average is used    
	  const double Dxx = (Dxx_cell[from] + Dxx_cell[to]) / 2.0;
	  const double Dzz = (Dzz_cell[from] + Dzz_cell[to]) / 2.0;
	  D_long[e] = anisotropy_factor (geo, e, Dxx, Dzz);
	  D_tran[e] = (Dxz_cell[from] + Dxz_cell[to]) / 2.0;	   
	}	 
      else if (geo.cell_is_internal (from))
	{
	  const double Dxx = Dxx_cell[from]; 
	  const double Dzz = Dzz_cell[from];
	  D_long[e] = anisotropy_factor (geo, e, Dxx, Dzz);
	  D_tran[e] = Dxz_cell[from];
	}
      else 
	{
	  daisy_assert(geo.cell_is_internal (to));
	  const double Dxx = Dxx_cell[to]; 
	  const double Dzz = Dzz_cell[to];
	  D_long[e] = anisotropy_factor (geo, e, Dxx, Dzz);
	  D_tran[e] = Dxz_cell[to];
	}
    }
}



void 
MsoltranrectMollerup::diffusion_long (const GeometryRect& geo,
				      const ublas::vector<double>& D_long,					      ublas::matrix<double>& diff_long)
{
  const size_t edge_size = geo.edge_size (); // number of edges  
    
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);
	  const double magnitude = geo.edge_area_per_length (e) * D_long[e]; 
	  diff_long (from, from) -= magnitude;
	  diff_long (from, to) += magnitude;
	  diff_long (to, to) -= magnitude;
	  diff_long (to, from) += magnitude; 
	} 
    }
}


void 
MsoltranrectMollerup::advection (const GeometryRect& geo,
				 const ublas::vector<double>& q_edge,
				 ublas::banded_matrix<double>& advec)  
{
  const size_t edge_size = geo.edge_size (); // number of edges  

  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  const double value = geo.edge_area (e) * q_edge[e];
	  advec (from, from) += value;
	  advec (to, to) -= value;
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
  daisy_assert (J * in_sign >= 0.0);
  B_vec (cell) = J * area * in_sign; 
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
}


void 
MsoltranrectMollerup::Dirichlet_long (const size_t cell,
                                      const double area, 
                                      const double area_per_length, 
                                      const double in_sign,
                                      const double D_long,
                                      const double C_bord,
                                      const double C_cell,
                                      const double q,
                                      double& J,
                                      ublas::banded_matrix<double>& Dlongm_mat,
                                      ublas::vector<double>& Dmlong_vec, 
                                      ublas::vector<double>& advecm) 
{

  // Boundary advection
  const double value = area  * q;
  advec (cell) -= in_sign * value;

  //Boundary longitudinal diffusion
  const double D_area_per_length = D_long * area_per_length;
  Dlongm_mat (cell, cell) -= D_area_per_length;
  
  const double Dlongm_vec_val = D_area_per_length * C_bord;
  Dlongm_vec (cell) += Dlongm_vec_val;
    
  J = in_sign * (D_area_per_length * C_cell + Dlongm_vec_val ) / area;  
}



void 
MsoltranrectMollerup::lowerboundary (const GeometryRect& geo,
                                     const bool isflux,
                                     const double conc,
                                     const  ublas::vector<double>& q_edge,
                                     std::vector<double>& J,
                                     ublas::banded_matrix<double>& B_mat,
                                     ublas::vector<double>& B_vec,
                                     ublas::banded_matrix<double>& Dlongm_mat, 
                                     ublas::vector<double>& Dlongm_vec)

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
    
      if (isflux)               // Flux BC
        {
          const bool influx = in_sign * q_edge (edge) > 0;
          if (influx)
            {
              J[edge] = conc * q_edge (edge); 
              Neumann_expl (cell, area, in_sign, J[edge], B_vec);
            }
          else
            {
              Neumann_impl (cell, area, in_sign, q_edge (edge), B_mat);
            }
        }
      else                      // Conc. BC
        {
          // write something
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
        }
    }
#endif 

}


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
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      daisy_assert (in_sign < 0);
      const double area = geo.edge_area (edge);
      Neumann_expl (edge, cell, area, J[edge], B_vec);
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
				 Adsorption& adsorption,
				 double diffusion_coefficient,
				 const double dt,
				 Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  ublas::vector<double> q_edge (edge_size);	
  for (int e = 0; e < edge_size; e++)
    q_edge (e) = soil_water.q (e);
  
  ublas::vector<double> Theta_cell (cell_size);	
  for (int c = 0; c < cell_size; c++)
    Theta_cell (c) = soil_water.Theta (c);
  
  
  // edge (inter cell) diffusion
  ublas::vector<double> D_long (edge_size); 
  ublas::vector<double> D_tran (edge_size);
   	
  diffusion_tensor (geo, soil, q_edge, Theta_cell, diffusion_coefficient, 
		    D_long, D_tran, msg);

  
  // Initialize longitudinal diffusion matrix
  ublas::matrix<double> diff_long 
    = ublas::zero_matrix<double> (cell_size, cell_size);
  diffusion_long (geo, D_long, diff_long);

  // Initialize transversal diffusion matrix
  // not today!!

  // Initialize advection diagonal matrix 
  ublas::banded_matrix<double> advec (cell_size, cell_size, 0, 0);   
  for (int c = 0; c < cell_size; c++)
    advec (c, c) = 0.0;
  advection (geo, q_edge, advec);  
  
  
  // Initialize and calculate sink term
  ublas::vector<double> S_vol (cell_size); // sink term 
  for (size_t cell = 0; cell != cell_size ; ++cell) 
    S_vol (cell) = S[cell] * geo.cell_volume (cell);
    
  
  // Boundary matrices and vectors
  

  ublas::banded_matrix<double> B_mat (cell_size, cell_size, 0, 0); 
  for (int c = 0; c < cell_size; c++)
    B_mat (c, c) = 0.0;
  ublas::vector<double> B_vec = ublas::zero_vector<double> (cell_size); 
  ublas::banded_matrix<double>  Dlongm_mat (cell_size, cell_size,      
                                            0, 0); // Dir bc
  Dlongm_mat = ublas::zero_matrix<double> (cell_size, cell_size);  
  ublas::vector<double>  Dlongm_vec (cell_size); // Dir bc
  Dlongm_vec = ublas::zero_vector<double> (cell_size);
  
    
  upperboundary (geo, J, B_vec, msg);


  // Why not dt and msg????
  const bool isflux = true;
  const double conc = 0.0;

  lowerboundary (geo, isflux, conc, q_edge, J, B_mat, B_vec, 
                 Dlongm_mat, Dlongm_vec);
  

  // Remember old content for checking mass balance later.
  const double old_content = geo.total_soil (M);
  double in = 0.0;	
  double out = 0.0; 

  // Upper border.
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      
      const double sin_angle = geo.edge_sin_angle (edge);
      const double value = J[edge] * geo.edge_area (edge) * sin_angle;
      M[cell] -=  dt * value / geo.cell_volume (cell);
      in -= value;
    }

  // Cell fluxes.
  for (size_t n = 0; n < cell_size; n++)
    {
      M[n] += S[n] * dt;
      C[n] = adsorption.M_to_C (soil, soil_water.Theta (n), n, M[n]);

      if (!(M[n] >= 0.0))
        {

          std::ostringstream tmp;
          tmp << "BUG: M[" << n << "] = " << M[n] 
              << " (J[0] = " << J[0] << ") S[" << n << "] = " << S[n];
          msg.error (tmp.str ());
          M[n] = C[n] = 0.0;
        }
      daisy_assert (M[n] >= 0.0);
      daisy_assert (C[n] >= 0.0);
    }

  // Mass balance.
  const double new_content = geo.total_soil (M);
  const double delta_content = new_content - old_content;
  const double source = geo.total_soil (S);
  // BUG: ASSume uniform boundaries.
  const double expected = (source + in - out) * dt;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
          << ": mass balance new - old != source + in - out\n"
          << new_content << " - " << old_content << " != " 
          << source * dt << " + " << in * dt << " - " << out * dt << " (error "
          << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

void 
MsoltranrectMollerup::output (Log&) const
{ }

MsoltranrectMollerup::MsoltranrectMollerup (Block& al)
  : Msoltranrect (al)
{ }

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

#endif
