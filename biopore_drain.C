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
#include "block.h"
#include "vcheck.h"
#include "librarian.h"
#include "submodeler.h"
#include "check.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "anystate.h"
#include "chemical.h"
#include "groundwater.h"
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

  // Simulation.
  double total_water () const
  { return 0.0; }
  double air_bottom (size_t) const    // Lowest point with air [cm]
  { return pipe_position; }
  
  double capacity (const Geometry& geo, size_t e, const double dt) const
  { return max_infiltration_rate (geo, e) * dt; }
  void infiltrate (const Geometry&, size_t, double /* [cm] */)
  { }

  double matrix_biopore_drain (size_t c, const Geometry& geo, 
                               const Soil& soil, bool active, 
                               const double h_barrier, double pressure_end,
                               double K_xx, double h) const;
  void update_matrix_sink (const Geometry& geo,    
                           const Soil& soil,  
                           const SoilHeat& soil_heat, 
                           const std::vector<bool>& active,
                           const double h_barrier,
                           const double pressure_initiate,
                           const double pressure_end,
                           const std::vector<double>& h, const double dt);
  void update_water ()
  { }
  void add_to_sink (std::vector<double>&,
                    std::vector<double>& S_drain) const
  {
    const size_t cell_size = S.size ();
    daisy_assert (S_drain.size () == cell_size);
    for (size_t c = 0; c < cell_size; c++)
      S_drain[c] += S[c];
  }
  void add_solute (symbol, size_t, const double)
  { }
  void matrix_solute (const Geometry&, const double, 
                      const Chemical&, std::vector<double>&,
                      Treelog&)
  { /* Handled by S_drain. */ }
  void output (Log& log) const
  { output_base (log); }

  // Create and Destroy.
  bool initialize (const Units& units, const Geometry& geo, const Scope& scope,
                   const Groundwater& groundwater, Treelog& msg)
  {
    bool ok = initialize_base (units, geo, scope, msg); 
    if (pipe_position > 0)
      {
        if (groundwater.is_pipe ())
          // Pipe height not specified here, use value from column.
          pipe_position = groundwater.pipe_height ();
        else
          {
            msg.error ("Unknown pipe position");
            ok = false;
          }
      }
    return ok;
  }
  bool check (const Geometry& geo, Treelog& msg) const
  { return check_base (geo, msg); }
  BioporeDrain (Block& al);
};

double 
BioporeDrain::matrix_biopore_drain (size_t c, const Geometry& geo, 
                                    const Soil& soil, bool active, 
                                    const double h_barrier, 
                                    const double pressure_end,
                                    double K_xx, double h) const
{
  const double M_c = density_cell[c];
  if (!std::isnormal (M_c))
    // No biopores here.
    return 0.0;
  const double r_c = diameter / 2.0;
  const double h_3 = std::max (pressure_end, air_bottom (c) - geo.cell_z (c));

  double S;
  if (active && h>h_3 + h_barrier)
    S = matrix_to_biopore (K_xx, M_c, r_c, h, h_3);
  else 
    S = 0.0;
  return S;
}

void
BioporeDrain::update_matrix_sink (const Geometry& geo,    
                                  const Soil& soil,  
                                  const SoilHeat& soil_heat, 
                                  const std::vector<bool>& active,
                                  const double h_barrier,
                                  const double pressure_initiate,
                                  const double pressure_end,
                                  const std::vector<double>& h, 
                                  const double /* dt */)
{
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const double h_cond = std::min(pressure_initiate, h[c]);
      const double T = soil_heat.T (c);
      const double h_ice = 0.0;    //ice ignored 
      const double K_zz = soil.K (c, h_cond, h_ice, T);
      const double K_xx = K_zz * soil.anisotropy (c);
      S[c] = matrix_biopore_drain (c, geo, soil, active[c], h_barrier, 
                                   pressure_end, K_xx, h[c]);
    }
}

template <class T>
std::string stringify (const T& x)
{
  std::ostringstream tmp;
  tmp << x;
  return tmp.str ();
}

#define TREELOG_INDEX(msg, key, index) \
  Treelog::Open nest (msg, stringify (key) + " = " + stringify (index))

#define TREELOG_MODEL1(msg) \
  Treelog::Open nest (msg, this->library_id () + ": " + this->name \
                      + " " + __FUNCTION__)

#if 0
void 
BioporeDrain::matrix_solute (const Geometry& geo, const double dt, 
                             const Chemical& chemical, 
                             std::vector<double>& source_chem,
                             Treelog& msg)
{
  TREELOG_MODEL1 (msg);
  const symbol chem = chemical.name;
  const size_t cell_size = geo.cell_size ();
  daisy_assert (source_chem.size () == cell_size);
  
  // From matrix to biopore.
  for (size_t c = 0; c < cell_size; c++)
    {
      TREELOG_INDEX (msg, "cell", c);
      const double water_sink = S[c]; // [cm^3 W/cm^3 S/h]
      daisy_assert (water_sink >= 0.0);
      const double C = chemical.C_secondary (c); // [g/cm^3 W]
      source_chem[c] -= water_sink * C; // [g/cm^3 S/h]
      if (!iszero (source_chem[c]))
        {
          std::stringstream tmp;
          tmp << "soruce_chem = " << source_chem[c] << ", S = " << S[c] 
              << ", C = " << C;
          msg.message (tmp.str ());
        }
    }

}
#endif

BioporeDrain::BioporeDrain (Block& al)
  : Biopore (al),
    pipe_position (al.number ("pipe_position", 42.42e42))
{ }

static struct BioporeDrainSyntax
{
  static Model& make (Block& al)
  { return *new BioporeDrain (al); }

  BioporeDrainSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Biopores that ends in the drain pipes.");
    Biopore::load_base (syntax, alist);

    syntax.add ("pipe_position", "cm", Check::negative (), Syntax::Const,
                "Height pipes are placed in the soil (a negative number).\n\
By default, use the height specified for pipes in the column.");
    Librarian::add_type (Biopore::component, "drain", alist, syntax, &make);
  }
} BioporeDrain_syntax;

// biopore_drain.C ends here.
