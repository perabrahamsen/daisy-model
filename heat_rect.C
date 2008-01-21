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
#include "librarian.h"
#include "syntax.h"
#include "alist.h"
#include "block.h"
#include "submodel.h"
#include "treelog.h"
#include "plf.h"
#include "solver.h" //mmo

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
static const double water_heat_capacity = 4.2e7; // [erg/cm^3/dg C]
static const double rho_water = 1.0;             // [g/cm^3]

static void 
convection (const GeometryRect& geo,
            const ublas::vector<double>& q_edge,
            Solver::Matrix& convec)
{

  const size_t edge_size = geo.edge_size (); // number of edges  

  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
	{
	  const int from = geo.edge_from (e);
	  const int to = geo.edge_to (e);	   
	  const double value = geo.edge_area (e) *
            water_heat_capacity * rho_water * q_edge[e];
          
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
            Solver::Matrix& conduc)
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
    Q_convec_out = - in_sign * area * 
      water_heat_capacity * rho_water * T_border * q;
  else                    //Outflow 
    Q_convec_out = - in_sign * area * 
      water_heat_capacity * rho_water * T_cell * q;
    //Q_convec_out = - in_sign * q * area * T_cell;
  
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
               Solver::Matrix& A,
               ublas::vector<double>& b)
{
  //The upper boundary is made by forcing the temperatures in the upper cells to the given value 
  //This is maybe not the most precise method, but it is stable... 
  //First we tried to implement the upper boundary similar to the lower boundary but it is unstable
  // - probalbly because the large temperature fluctuations. It should also be possible to do it similar 
  //as in the solute movement calculations, but it divides the timestep into smaller timesteps. In it should 
  //in heat calculations probalbly be done very often....
  
  if (isflux)   //Nothing to do for (no)flux bc 
    return;
  
  
  const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const int edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      
      const std::vector<int>& edge_around = geo.cell_edges (cell);
      const size_t edge_around_size = edge_around.size ();
      
      for (size_t j= 0; j < edge_around_size; j++)
         {
           const int edge_next = edge_around[j];
           const int cell_next = geo.edge_other (edge_next, cell);
           if (geo.cell_is_internal (cell_next))
             A (cell, cell_next) = 0;
         }
      A (cell, cell) = 1.0;
      b (cell) = T_border;
    }
}


static void 
fluxes (const GeometryRect& geo,
        const bool isflux_lower,
        const bool isflux_upper,
        const ublas::vector<double>& q_edge,
        const ublas::vector<double>& cond_edge,
        const ublas::vector<double>& T,
        const double T_bottom, 
        const ublas::vector<double>& b_before,
        const ublas::vector<double>& b_after,
        const ublas::vector<double>& B_dir_vec,
        ublas::vector<double>& dQ) 
{
  const size_t edge_size = geo.edge_size (); // number of edges  
  
  daisy_assert (q_edge.size () == edge_size);
  daisy_assert (cond_edge.size () == edge_size);
  daisy_assert (dQ.size () == edge_size);

  //First all the internal edges...
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
        {
          const int from = geo.edge_from (e);
          const int to = geo.edge_to (e);  
          
          daisy_assert (from >= 0);
          daisy_assert (to >= 0);
          daisy_assert (from < T.size ());
          daisy_assert (to < T.size ());
          
          //--- Convective part ---
          const double upstream_weight = 0.5;  //Should be taken from  outside
          const double alpha = (q_edge[e] >= 0) 
            ? upstream_weight 
            : 1.0 - upstream_weight;
          dQ[e] = water_heat_capacity * rho_water * 
            (alpha * T[from] + (1.0-alpha) * T[to]) * q_edge[e];
          //dQ[e] = alpha * q_edge[e] * T[from] + (1.0-alpha) * T[to];    //mmo old and wrong!!!
          
          //--- Conductive part - xx_zz --- 
          const double gradient = geo.edge_area_per_length (e) *
            (T[to] - T[from]);
          dQ[e] -= cond_edge[e]*gradient;  //xx_zz convection
        }
    } 
    
  //Lower boundary 
  if (!isflux_lower)   //Do nothing for no-flux boundaries
    {
      const std::vector<int>& edge_below = geo.cell_edges (Geometry::cell_below);
      const size_t edge_below_size = edge_below.size ();
      for (size_t i = 0; i < edge_below_size; i++)
        {
          const int edge = edge_below[i];
          const int cell = geo.edge_other (edge, Geometry::cell_below);
          daisy_assert (cell >= 0);
          daisy_assert (cell < T.size ());
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;         
          dQ[edge] = -in_sign * B_dir_vec (cell) / geo.edge_area (edge); 
        }
    }

  //Upper boundary  
  if (!isflux_upper) 
    {
      const size_t cell_size = geo.cell_size ();
      ublas::vector<double> B_dir_vec_new (cell_size);
      B_dir_vec_new = b_before + B_dir_vec - b_after; 

      #if 0
      std::cout << "B_dir_vec: " << B_dir_vec << '\n';
      std::cout << "B_dir_vec_new: " << B_dir_vec_new << '\n';
      #endif
        
      const std::vector<int>& edge_above = geo.cell_edges (Geometry::cell_above);
      const size_t edge_above_size = edge_above.size ();
      
      for (size_t i = 0; i < edge_above_size; i++)
        {
          const int edge = edge_above[i];
          const int cell = geo.edge_other (edge, Geometry::cell_above);
          daisy_assert (cell >= 0);
          daisy_assert (cell < T.size ());
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          dQ[edge] = -in_sign * B_dir_vec_new (cell) / geo.edge_area (edge); 
        }
    }
}





#if 0
void
HeatRect::solve (const GeometryRect& geo,
		 const std::vector<double>& q_water,
		 const std::vector<double>& S_water,
		 const std::vector<double>& S_heat,
		 const std::vector<double>& capacity_old,
		 const std::vector<double>& capacity_new,
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


// ------ New version of Solve ------
void
HeatRect::solve (const GeometryRect& geo,
                 const std::vector<double>& q_water,
                 const std::vector<double>& S_water,
                 const std::vector<double>& S_heat,
		 const std::vector<double>& capacity_old,
		 const std::vector<double>& capacity_new,
                 const std::vector<double>& conductivity,
                 double T_top_old,      //mmo should not be constant
                 double T_top_new,      //mmo should not be constant
                 double T_bottom,       //mmo should not be constant 
                 std::vector<double>& T,
                 const double dt, Treelog& msg) const
{
  // mmo
  // Note S_h should be calculated correctly the value imported 
  // here is most probalbly only from heatsource like wires and 
  // not added/removed water
  //
  // S_h_water = S_water * water_heat_capacity * T 
  

  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();  

  //mmo testing 
  //T_top_old = 3.188;
  //T_top_new = 3.188;
  //T_bottom = 10;


  // Solution old
  ublas::vector<double> T_old (cell_size);
  for (int c = 0; c < cell_size; c++)
    T_old (c) = T[c];
  ublas::vector<double> T_n (cell_size);  // Maybe not neccessary with both T_old and T_n
  T_n = T_old;

  // Mean temp at upper boundary
  double T_top_mean = 0.5 * (T_top_old + T_top_new);

  // Area (volume) Multiplied with heat capacity 
  ublas::banded_matrix<double> Q_Ch_mat_n (cell_size, cell_size, 0 ,0);
  for (int c = 0; c < cell_size; c++)
    Q_Ch_mat_n (c, c) = geo.cell_volume (c) * capacity_new[c];
  ublas::banded_matrix<double> Q_Ch_mat_np1 (cell_size, cell_size, 0 ,0);
  for (int c = 0; c < cell_size; c++)
    Q_Ch_mat_np1 (c, c) = geo.cell_volume (c) * capacity_new[c];
  
  // Flux in timestep
  ublas::vector<double> q_edge (edge_size);	
  for (int e = 0; e < edge_size; e++)
    q_edge (e) = q_water[e];
  
  //Convection
  Solver::Matrix convec (cell_size);
  convection (geo, q_edge, convec);  

  //Conduction
  ublas::vector<double> cond_edge (edge_size); 
  cond_cell2edge (geo, conductivity, cond_edge);
  Solver::Matrix conduc (cell_size);
  conduction (geo, cond_edge, conduc);

  //Sink term
  ublas::vector<double> S_vol (cell_size); // sink term 
  for (size_t cell = 0; cell != cell_size ; ++cell) 
    S_vol (cell) = - geo.cell_volume (cell) *             //water 
      water_heat_capacity * rho_water *  S_water[cell]    //water
      - geo.cell_volume (cell) * S_heat[cell];            //electricity
  
  //Boundary vectors  
  ublas::vector<double> B_dir_vec = ublas::zero_vector<double> (cell_size);

  const bool isflux_lower = false;      //true;    //lower BC 
  const bool isflux_upper = false; //true;   //upper BC
  const bool enable_boundary_conduction = true; //mmo should be changed....

  lowerboundary (geo, isflux_lower, T_bottom,
                 q_edge, cond_edge, T_old,
                 enable_boundary_conduction, B_dir_vec);
  //upperboundary (geo, isflux_upper, T_bottom,  //T_top_mean,
  //               q_edge, cond_edge, T_old,
  //               enable_boundary_conduction, B_dir_vec);


  // Solver parameter , gamma
  // gamma = 0      : Backward Euler 
  // gamma = 0.5    : Crank - Nicholson
  const double gamma = 0.5;


  //Initialize A-matrix (left hand side)
  Solver::Matrix A (cell_size);  
  
  //Initialize b-vector (right hand side)
  ublas::vector<double> b (cell_size);   
  ublas::vector<double> b_before (cell_size);   
  Solver::Matrix b_mat (cell_size);  

  A = (1.0 / dt) * Q_Ch_mat_np1             // dT/dt
    - gamma * conduc                        // conduction
    + gamma * convec;                       // convection
  
  b_mat = (1.0 / dt) * Q_Ch_mat_n 
    + (1 - gamma) * conduc                  // conduction  
    - (1 - gamma) * convec;                 // convection
   
  b = prod (b_mat, T_n)
    - B_dir_vec                             // Dirichlet BC as Neumann
    - S_vol;                                // Sink term        

  if (debug > 0)
    {
      std::ostringstream tmp;
      //tmp << "A: \n" << A << '\n'
      //	  << "b_mat \n" << b_mat << '\n'
      //  << "b \n" << b << '\n';
      
      tmp << "T \n" << T_n << '\n';

      tmp << "T_top_new: \n" << T_top_new << '\n'
          << "T_top_old: \n" << T_top_old << '\n'
          << "T_top_mean: \n" << T_top_mean << '\n'
          << "T_bottom: \n" << T_bottom << '\n';
     
      msg.message (tmp.str ());
    }
  
  
  //Forced temperature in upper cell

  b_before = b;  //for computing 
  upperboundary (geo, isflux_upper, T_top_mean, A, b);


  if (debug > 0)
    {
      std::ostringstream tmp;
      //tmp << "A: \n" << A << '\n'
      //	  << "b_mat \n" << b_mat << '\n'
      //  << "b \n" << b << '\n';
      
      tmp << "T \n" << T_n << '\n';
      
      tmp << "T_top_new: \n" << T_top_new << '\n'
          << "T_top_old: \n" << T_top_old << '\n'
          << "T_top_mean: \n" << T_top_mean << '\n'
          << "T_bottom: \n" << T_bottom << '\n'
          << "A: \n" << A << '\n'
          << "b: \n" << b << '\n';
      
      msg.message (tmp.str ());
    }
  
  solver->solve (A, b, T_n); // Solve A T_n = b with regard to T_n.
  

  //New solution into T
  for (size_t c = 0; c < cell_size; c++)
    T[c] = T_n (c);

  //Calculate fluxes 
  ublas::vector<double> dQ = ublas::zero_vector<double> (edge_size);


  fluxes (geo, isflux_lower, isflux_upper, q_edge, cond_edge,
          T_n, T_bottom, b_before, b, B_dir_vec, dQ); 


}
// --- End of new version of solve ---



void
HeatRect::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_object ("solver", Solver::component, 
		     Syntax::Const, Syntax::Singleton, "\
Model used for solving matrix equation system.");
  alist.add ("solver", Solver::default_model ());
  syntax.add ("debug", Syntax::Integer, Syntax::Const, "\
Enable additional debug message.\n\
A value of 0 means no message, higher numbers means more messages.");
  alist.add ("debug", 0);

  alist.add ("submodel", "HeatRect");
  alist.add ("description", "Heat transport in a rectangular grid.");
}

HeatRect::HeatRect (Block& al)
  : solver (Librarian::build_item<Solver> (al, "solver")),
    debug (al.integer ("debug"))
{ }

HeatRect::~HeatRect ()
{ }

static Submodel::Register 
heat_rect_submodel ("HeatRect", HeatRect::load_syntax);

// heat_rect.C ends here.
