// condedge.C --- Find the hydraulic conductivity between two cells.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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

#include "condedge.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "soil.h"
#include "geometry.h"

// condedge component.

const char *const Condedge::component = "condedge";

symbol 
Condedge::library_id () const
{
  static const symbol id (component);
  return id;
}

Condedge::Condedge ()
{ }

Condedge::~Condedge ()
{ }

static struct CondedgeInit : public DeclareComponent 
{
  CondedgeInit ()
    : DeclareComponent (Condedge::component, "\
Find the hydraulic conductivity between two cells.")
  { }
} Condedge_init;

// The 'arithmetic' model.

struct CondedgeArithmetic : public Condedge
{
  // Simulation.
  double average (const Soil&, const Geometry&, size_t, 
                  double K1, double, double, double, 
                  double K2, double, double, double) const
  { return (K1 + K2) / 2.0; }
  // Create and Destroy.
  CondedgeArithmetic (const BlockModel&)
  { }
  CondedgeArithmetic (const char *const)
  { }
  ~CondedgeArithmetic ()
  { }
};

static struct CondedgeArithmeticSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CondedgeArithmetic (al); }
  CondedgeArithmeticSyntax ()
    : DeclareModel (Condedge::component, "arithmetic", "\
Use the arithmetic average of the conductivity in the two cells.")
  { }
  void load_frame (Frame& frame) const
  { }
} CondedgeArithmetic_syntax;


std::auto_ptr<const Condedge>
Condedge::build_arithmetic ()
{ return std::auto_ptr<const Condedge> (new CondedgeArithmetic (__FUNCTION__)); }

// The 'harmonic' model.

struct CondedgeHarmonic : public Condedge
{
  // Simulation.
  double average (const Soil&, const Geometry&, size_t, 
                  double K1, double, double, double, 
                  double K2, double, double, double) const
  { return 2.0 * K1 * K2 / (K1 + K2); }
  // Create and Destroy.
  CondedgeHarmonic (const BlockModel&)
  { }
  ~CondedgeHarmonic ()
  { }
};

static struct CondedgeHarmonicSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CondedgeHarmonic (al); }
  CondedgeHarmonicSyntax ()
    : DeclareModel (Condedge::component, "harmonic", "\
Use harmonic average of the conductivity of the two cells.\n\
This corresponds to using the average hydraulic resistence.")
  { }
  void load_frame (Frame& frame) const
  { }
} CondedgeHarmonic_syntax;

// The 'geometric' model.

struct CondedgeGeometric : public Condedge
{
  // Simulation.
  double average (const Soil&, const Geometry&, size_t, 
                  double K1, double, double, double, 
                  double K2, double, double, double) const
  { return std::sqrt (K1 * K2); }

  // Create and Destroy.
  CondedgeGeometric (const BlockModel&)
  { }
  CondedgeGeometric (const char *const)
  { }
  ~CondedgeGeometric ()
  { }
};

static struct CondedgeGeometricSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CondedgeGeometric (al); }
  CondedgeGeometricSyntax ()
    : DeclareModel (Condedge::component, "geometric", "\
Geometric average 'sqrt(a*b)'.")
  { }
  void load_frame (Frame& frame) const
  { }
} CondedgeGeometric_syntax;

std::auto_ptr<const Condedge>
Condedge::build_geometric ()
{ return std::auto_ptr<const Condedge> (new CondedgeGeometric (__FUNCTION__)); }

// The 'pressure' model.

struct CondedgePressure : public Condedge
{
  const double h_lim;

  // Simulation.
  double average (const Soil& soil, const Geometry& geo, const size_t edge,
                  double K1, double h1, double h1_ice, double T1,
                  double K2, double h2, double h2_ice, double T2) const
  {
    const double K_harmonic = 2.0 * K1 * K2 / (K1 + K2);

    if (h1 < h_lim && h2 < h_lim)
      // Harmonic average for dry soil.
      return K_harmonic; 

    // Find cells.
    daisy_assert (geo.edge_is_internal (edge));
    const size_t c1 = geo.edge_from (edge);
    const size_t c2 = geo.edge_to (edge);

    // First we test for downward flow.
    const double z1 = geo.cell_z (c1);
    const double z2 = geo.cell_z (c2);
    
    size_t c_top;
    double h_top;
    double K_top;
    double z_top;
    double h_top_ice;
    double T_top;
    size_t c_bottom;
    double h_bottom;
    double K_bottom;
    double z_bottom;
    double h_bottom_ice;
    double T_bottom;
    if (z1 > z2)
      {
        c_top = c1;
        h_top = h1;
        K_top = K1;
        z_top = z1;
        h_top_ice = h1_ice;
        T_top = T1;
        c_bottom = c2;
        h_bottom = h2;
        K_bottom = K2;
        z_bottom = z2;
        h_bottom_ice = h1_ice;
        T_bottom = T1;
      }
    else
      {
        c_top = c2;
        h_top = h2;
        K_top = K2;
        z_top = z2;
        h_top_ice = h2_ice;
        T_top = T2;
        c_bottom = c1;
        h_bottom = h1;
        K_bottom = K1;
        z_bottom = z1;
        h_bottom_ice = h1_ice;
        T_bottom = T1;
      }

    // Gravity assisted downflow in large pores.
    if (h_top > h_bottom && K_top > K_bottom)
      {
        const double K_lim = soil.K (c_bottom, h_top, h_bottom_ice, T_bottom);
        return std::max (std::min (K_top, K_lim),
                         K_harmonic);
      }

    const double h_sat = 0.0;   // Saturated soil potential [cm].

    if (h1 < h_sat && h2 < h_sat)
      // Harmonic average for unsaturated soil.
      return 2.0 * K1 * K2 / (K1 + K2);

    // Then we test for satuared pressure induced flow.
    size_t c_max;
    double h_max;
    double K_max;
    double z_max;
    double h_max_ice;
    double T_max;
    size_t c_min;
    double h_min;
    double K_min;
    double z_min;
    double h_min_ice;
    double T_min;
    if (h1 > h2)
      {
        c_max = c1;
        h_max = h1;
        K_max = K1;
        z_max = z1;
        h_max_ice = h1_ice;
        T_max = T1;
        c_min = c2;
        h_min = h2;
        K_min = K2;
        z_min = z2;
        h_min_ice = h1_ice;
        T_min = T1;
      }
    else
      {
        c_max = c2;
        h_max = h2;
        K_max = K2;
        z_max = z2;
        h_max_ice = h2_ice;
        T_max = T2;
        c_min = c1;
        h_min = h1;
        K_min = K1;
        z_min = z1;
        h_min_ice = h1_ice;
        T_min = T1;
      }
    const double dh = h_max - h_min;
    const double dz = z_max - z_min;

    const double h_trigger = 1.0;     // Pressure triggering 
    
    if (dh + dz > h_trigger)
      {
        const double K_lim = soil.K (c_min, h_sat, h_min_ice, T_min);
        return std::max (std::min (K_max, K_lim), K_harmonic);
      }

    return K_harmonic;
  }
  // Create and Destroy.
  CondedgePressure (const BlockModel& al)
    : h_lim (al.number ("h_lim"))
  { }
  ~CondedgePressure ()
  { }
};

static struct CondedgePressureSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CondedgePressure (al); }
  CondedgePressureSyntax ()
    : DeclareModel (Condedge::component, "pressure", "\
Pressure dependent average of the two cells.\n\
Use harmonic average of the conductivity of unsaturated cells.\n\
This corresponds to using the average hydraulic resistence.  For\n\
saturated cells, water may stream into unsaturated neigbor cells with\n\
saturated conductivity.  For cells where pressure is above 'h_lim',\n\
water may stream downward to dryer cell with a conductivity\n\
corresponding to 'h_lim'.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("h_lim", "cm", Attribute::Const, "\
Lower pressure limit for fast downward flow.");    
  }
} CondedgePressure_syntax;

// condedge.C ends here.
