// biopore_drain.C --- Static vertical biopores that ends in drain pipes.
// 
// Copyright 2008 Per Abrahamsen and KU.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
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

#include "biopore.h"
#include "block_model.h"
#include "vcheck.h"
#include "librarian.h"
#include "submodeler.h"
#include "check.h"
#include "geometry.h"
#include "soil.h"
#include "anystate.h"
#include "chemical.h"
#include "groundwater.h"
#include "treelog.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"
#include "log.h"
#include <sstream>

// The 'drain' model.

struct BioporeDrain : public Biopore
{
  // Parameters.
  /* const */ double pipe_position;   // [cm]

  // State
  Anystate get_state () const
  { return Anystate::none (); }
  void set_state (const Anystate&)
  { }

  std::vector<double> S_to_drain;
  IMvec S_chem_to_drain;

  // Simulation.
  double total_water () const
  { return 0.0; }
  double total_solute (const Geometry&, const symbol) const
  { return 0.0; }
  void get_solute (IM&) const
  { }
  double air_bottom (size_t) const    // Lowest point with air [cm]
  { return pipe_position; }
  
  double infiltration_capacity (const Geometry& geo, size_t e, 
                                const double dt) const
  { 
    // Any macropores that reach the surface.
    if (height_start < 0.0)
      return 0.0;

    return max_infiltration_rate (geo, e) * dt; 
  }

  double matrix_biopore_drain (size_t c, const Geometry& geo, 
                               bool active, 
                               const double h_barrier, double pressure_limit,
                               double K_xx, double h) const;
  void update_cell_solute (const Geometry& geo, const symbol chem, 
			   const double dt);
  void forward_sink (const Geometry& geo,    
                     const std::vector<bool>& active,
                     const std::vector<double>& K, 
                     const std::vector<double>& K_crack, 
                     const double h_barrier,
                     const double pressure_limit,
                     const std::vector<double>& h, 
                     std::vector<double>& S3) const;
  void tick_source (const Geometry&, const std::vector<bool>&,
                    const std::vector<double>&)
  { }
  void update_matrix_sink (const Geometry& geo,    
                           const std::vector<bool>& active,
                           const std::vector<double>& K,
                           const std::vector<double>& K_crack,
                           const double h_barrier,
                           const double pressure_limit,
                           const std::vector<double>& h,
                           const double dt);
  void update_water ()
  { }
  void update_cell_water (const Geometry& geo, const double)
  {
    std::fill (S_to_drain.begin (), S_to_drain.end (), 0.0);
    geo.biopore_pass_pipes (pipe_position, S, q, S_to_drain);
    daisy_approximate (geo.total_surface (S) + infiltration, 
                       geo.total_surface (S_to_drain));
  }
  void update_soil_tertiary (std::vector<double>&, std::vector<double>& q_p)
  {
    const size_t edge_size = q.size ();
    daisy_assert (edge_size == q_p.size ());
    for (size_t i = 0; i < edge_size; i++)
      q_p[i] += q[i];
  }
  void add_to_sink (std::vector<double>&,
                    std::vector<double>&,
                    std::vector<double>& S_drain,
                    std::vector<double>& S_tertiary_drain) const
  {
    const size_t cell_size = S.size ();
    daisy_assert (S_drain.size () == cell_size);
    for (size_t c = 0; c < cell_size; c++)
      {
        S_drain[c] += S[c];
        S_tertiary_drain[c] += S_to_drain[c];
      }
  }
  void add_solute (symbol, size_t, const double)
  { }
  void remove_solute (symbol)
  { }
  void matrix_solute (const Geometry&, const double, 
                      Chemical&, Treelog&);
  void output (Log& log) const
  { 
    output_base (log); 
    output_variable (S_to_drain, log);
    output_submodule (S_chem_to_drain, "S_chem_to_drain", log);
  }

  // Create and Destroy.
  bool initialize (const Units& units, const Geometry& geo, const Scope& scope,
                   const Groundwater& groundwater, Treelog& msg)
  {
    bool ok = initialize_base (units, geo, scope, msg); 
    if (pipe_position > 0)
      {
        msg.error ("Unknown pipe position");
        ok = false;
      }
    const size_t cell_size = geo.cell_size ();
    S_to_drain.insert (S_to_drain.begin (), cell_size, 0.0);
    daisy_assert (S_to_drain.size () == cell_size);
    return ok;
  }
  bool check (const Geometry& geo, Treelog& msg) const
  { return check_base (geo, msg); }
  BioporeDrain (const BlockModel& al);
};

double 
BioporeDrain::matrix_biopore_drain (size_t c, const Geometry& geo, 
                                    bool active, 
                                    const double h_barrier, 
                                    const double pressure_limit,
                                    double K_xx, double h) const
{
  const double M_c = density_cell[c];
  if (!std::isnormal (M_c))
    // No biopores here.
    return 0.0;
  const double r_c = diameter / 2.0;
  const double h_3 = air_bottom (c) - geo.cell_z (c);

#if 0
  //--------------------------------------
  std::ostringstream tmp;
  tmp << "c = " << c 
      << " airbottom = " << air_bottom (c) 
      << " cell_z = " <<  geo.cell_z (c)
      << " h = " << h
      << " h_3 = " << h_3 
      << " h + p_end = " << h+pressure_limit; 
  Assertion::message (tmp.str ());  
  //--------------------------------------
#endif


  double S;
  if (active && h>h_3 + h_barrier)
    {
      // The largest pressure gradient between the domains are
      // pressure_limit, above that we claim air will disrupt the suction.
      const double h_3_suck = std::max (h_3, h + pressure_limit);

      S = matrix_to_biopore (K_xx, M_c, r_c, h, h_3_suck)
        * geo.fraction_in_z_interval (c, height_start, height_end);
    }
  else 
    S = 0.0;
  return S;
}

void 
BioporeDrain::forward_sink (const Geometry& geo,    
                            const std::vector<bool>& active,
                            const std::vector<double>& K, 
                            const std::vector<double>& /* K_crack */, 
                            const double h_barrier,
                            const double pressure_limit,
                            const std::vector<double>& h, 
                            std::vector<double>& S3) const
{
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    S3[c] += matrix_biopore_drain (c, geo, active[c], h_barrier, 
                                   pressure_limit, K[c], h[c]);
}

void
BioporeDrain::update_matrix_sink (const Geometry& geo,    
                                  const std::vector<bool>& active,
                                  const std::vector<double>& K, 
                                  const std::vector<double>& K_crack, 
                                  const double h_barrier,
                                  const double pressure_limit,
                                  const std::vector<double>& h, 
                                  const double /* dt */)
{
  std::fill (S.begin (), S.end (), 0.0);
  forward_sink (geo, active, K, K_crack, h_barrier, pressure_limit, h, S);
}

void 
BioporeDrain::matrix_solute (const Geometry& geo, const double dt, 
			     Chemical& chemical, Treelog& msg)
{
  TREELOG_MODEL (msg);
  const symbol chem = chemical.objid;
  const size_t cell_size = geo.cell_size ();
  std::vector<double>& sink_chem = S_chem.get_array (chem);
  sink_chem.resize (cell_size);
  std::fill (sink_chem.begin (), sink_chem.end (), 0.0);

  // From matrix to biopore.
  for (size_t c = 0; c < cell_size; c++)
    sink_chem[c] = S[c] * chemical.C_to_drain (c); // [g/cm^3 S/h]
  
  update_cell_solute (geo, chem, dt);

  const std::vector<double> empty_cell (cell_size, 0.0);
  const std::vector<double>& Jc = J.get_array (chem);
  const std::vector<double>& S_indirect_drain = S_chem.get_array (chem);
  const std::vector<double>& S_p_drain = S_chem_to_drain.get_array (chem);
  chemical.add_tertiary (empty_cell, Jc, empty_cell,
			 S_indirect_drain, S_p_drain);
}

void
BioporeDrain::update_cell_solute (const Geometry& geo, const symbol chem,
				  const double dt)
{

  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  std::vector<double>& Sc_to_drain = S_chem_to_drain.get_array (chem);
  Sc_to_drain.resize (cell_size);
  std::fill (Sc_to_drain.begin (), Sc_to_drain.end (), 0.0);
  const std::vector<double>& Sc = S_chem.get_array (chem);
  std::vector<double>& Jc = J.get_array (chem);
  Jc.resize (edge_size);
  const double infiltrationc = solute_infiltration.get_value_raw (chem);

  geo.biopore_pass_pipes (pipe_position, Sc, Jc, Sc_to_drain);
  daisy_approximate (geo.total_surface (Sc) + infiltrationc, 
		     geo.total_surface (Sc_to_drain));
}  

BioporeDrain::BioporeDrain (const BlockModel& al)
  : Biopore (al),
    pipe_position (al.number ("pipe_position", 42.42e42)),
    S_chem_to_drain (al, "S_chem_to_drain")
{ }

static struct BioporeDrainSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BioporeDrain (al); }

  BioporeDrainSyntax ()
    : DeclareModel (Biopore::component, "drain", "\
Biopores that ends in the drain pipes.")
  { }
  static void load_S_chem (Frame& frame)
  { IMvec::add_syntax (frame, Attribute::LogOnly, Attribute::SoilCells, 
		       IM::sink_unit ()); }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("S_to_drain", "cm^3/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells,
		   "Total stream from biopore to drain.");
    frame.declare_submodule_sequence ("S_chem_to_drain",
				      Attribute::LogOnly, "\
Biopore to drain term for solutes.", load_S_chem);
    frame.declare ("pipe_position", "cm", Check::negative (), 
                   Attribute::OptionalConst,
		   "Height pipes are placed in the soil (a negative number).\n\
By default, use the height specified for pipes in the column.");
  }
} BioporeDrain_syntax;

// biopore_drain.C ends here.
