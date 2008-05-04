// biopore_drain.C --- Static vertical biopores that ends in drain pipes.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "biopore.h"
#include "block.h"
#include "vcheck.h"
#include "librarian.h"
#include "submodeler.h"
#include "check.h"

// The 'drain' model.

struct BioporeDrain : public Biopore
{
  // Parameters.
  /* const */ double pipe_position;   // [cm]

  // Simulation.
  double air_bottom (size_t) const    // Lowest point with air [cm]
  { return pipe_position; }
  void extract_water (size_t c, const double volume /* [cm^3] */ ,
                      const double Theta /* [cm^3/cm^3 */,
                      const double dt /* [h] */,
                      std::vector<double>& S_drain /* [cm^3/cm^3/h */,
                      std::vector<double>& S_matrix);
  void release_water (const Geometry&, const Soil&, 
                      const SoilWater&,
                      const double /* [h] */,
                      std::vector<double>&)
  { }
  void update_water ()
  { }

  // Create and Destroy.
  bool initialize (const Geometry& geo, const Scope& scope, const double pipe,
                   Treelog& msg)
  {
    if (pipe_position > 0)
      // Pipe height not specified here, use value from column.
      pipe_position = pipe;

    return initialize_base (geo, scope, msg); 
  }
  bool check (const Geometry& geo, Treelog& msg) const
  { return check_base (geo, msg); }
  BioporeDrain (Block& al);
};

void 
BioporeDrain::extract_water (size_t c, const double /* [cm^3] */ ,
                             const double Theta /* [cm^3/cm^3] */,
                             const double dt /* [h] */,
                             std::vector<double>& S_drain /* [cm^3/cm^3/h] */,
                             std::vector<double>& )
{ S_drain[c] += Theta / dt; }

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
