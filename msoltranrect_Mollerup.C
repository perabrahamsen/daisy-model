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
                               const SoilWater& soil_water,
                               ublas::vector<double>& qx_cell,
                               ublas::vector<double>& qz_cell);
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
                                       const SoilWater& soil_water,
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
      const double q = soil_water.q (e);
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
MsoltranrectMollerup::flow (const GeometryRect& geo, 
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
  
  // Calculate cell based water flux.
  ublas::vector<double> qx_cell;
  ublas::vector<double> qz_cell;
  cell_based_flux (geo, soil_water, qx_cell, qz_cell);
  
  // Calculate diffusion matrix. 
  ublas::vector<double> Dxx_cell (cell_size);
  ublas::vector<double> Dzz_cell (cell_size);
  ublas::vector<double> Dxz_cell (cell_size);
  ublas::vector<double>& Dzx_cell = Dxz_cell;

  for (int c = 0; c < cell_size; c++)
    {
      const double Theta = soil_water.Theta (c);
      daisy_assert(Theta > 0);
      const double qx = qx_cell (c);
      const double qz = qz_cell (c);
      const double q = sqrt (sqr (qx) + sqr (qz));
      const double tau = soil.tortuosity_factor (c, Theta);
      const double alpha_L = soil.dispersivity (c);
      const double alpha_T = soil.dispersivity_transversal (c);
      if (q > 0)
        {
          Dxx_cell (c) = (alpha_L * sqr (qx) + alpha_T * sqr (qz) )
            / (q * Theta) + diffusion_coefficient * tau;
          Dzz_cell (c) = (alpha_L * sqr (qz) + alpha_T * sqr (qx) )
            / (q * Theta) + diffusion_coefficient * tau;   
          Dxz_cell (c) = (alpha_L - alpha_T) * qx * qz / (q * Theta);
        }  
      else
        { 
          Dxx_cell (c) = diffusion_coefficient * tau;
          Dzz_cell (c) = diffusion_coefficient * tau;
          Dxz_cell (c) = 0.0;
        }
    }
  
    


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
