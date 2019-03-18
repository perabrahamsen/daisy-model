// rootdens_GP2D.C -- Gerwitz and Page model extended for row crops.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007, 2008 Per Abrahamsen and KVL.
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

#include "program.h"
#include "geometry_rect.h"
#include "rootdens.h"
#include "treelog.h"
#include "block_model.h"
#include "submodeler.h"
#include "check.h"
#include "librarian.h"
#include "assertion.h"
#include "frame.h"
#include <sstream>

struct ProgramGP2D : public Program
{
  const Metalib& metalib;
  std::unique_ptr<GeometryRect> geo;
  const double row_width;
  const double row_position;
  const double soil_depth;
  const double crop_depth;
  const double crop_width;
  const double WRoot;
  const double DS;

  // Use.
  void table_center (const GeometryRect& geo, 
                     const std::vector<double>& Density, Treelog& msg)
  {
    // Print it.
    const size_t column_size = geo.cell_columns ();
    const size_t row_size = geo.cell_rows ();
    std::ostringstream tmp;

    // Top line
    tmp << "z\\x";
    for (size_t col = 0; col < column_size; col++)
      {
        daisy_assert (row_size > 0);
        tmp << "\t" << geo.cell_x (geo.cell_index (0, col));
      }

    // Rows.
    for (size_t row = 0; row < row_size; row++)
      {
        daisy_assert (column_size > 0);
        tmp << "\n" << geo.cell_z (geo.cell_index (row, 0));
        for (size_t col = 0; col < column_size; col++)
          {
            const size_t cell = geo.cell_index (row, col);
            daisy_assert (cell < Density.size ());
            tmp << "\t" << Density[cell];
          }
      }
    
    Treelog::Open nest (msg, "Root density table [(cm, cm) -> cm/cm^3]");
    msg.message (tmp.str ());
  }

  // Use.
  bool run (Treelog& msg)
  {
    // Find it.
    std::unique_ptr<Rootdens> rootdens 
      = Rootdens::create_row (metalib, msg, row_width, row_position, true);
    rootdens->initialize (*geo, row_width, row_position, msg);
    std::vector<double> Density (geo->cell_size ());
    rootdens->set_density (*geo, soil_depth, crop_depth, crop_width,
                           WRoot, DS, Density, msg);
    
    // Print it.
    table_center (*geo, Density, msg);

    // Ok.
    return true;
  }

  // Create and Destroy.
  void initialize (Block& al)
  { 
    const bool volatile_bottom = false;
    const std::vector<double> fixed;
    const double max_interval = 20;
    
    geo->initialize_zplus (volatile_bottom, fixed, soil_depth, max_interval, 
                           al.msg ());
  }

  bool check (Treelog& msg)
  {
    bool ok = true;
    if (!geo->check (msg))
      ok = false;
    return ok; 
  }

  ProgramGP2D (const BlockModel& al)
    : Program (al),
      metalib (al.metalib ()),
      geo (submodel<GeometryRect> (al, "Geometry")),
      row_width (al.number ("row_width")),
      row_position (al.number ("row_position")),
      soil_depth (al.number ("soil_depth")),
      crop_depth (al.number ("crop_depth")),
      crop_width (al.number ("crop_width")),
      WRoot (al.number ("WRoot")),
      DS (al.number ("DS"))
  { }
  ~ProgramGP2D ()
  { }
};

static struct ProgramGP2DSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramGP2D (al); }
  ProgramGP2DSyntax ()
    : DeclareModel (Program::component, "GP2D", "\
Write root density table using 2D extension to Gerwitz and Page")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule ("Geometry", Attribute::Const,
                          "Discretization of the soil.",
                          GeometryRect::load_syntax);
    frame.declare ("row_width", "cm", Check::positive (), Attribute::Const, "\
Distance between rows.");
    frame.declare ("row_position", "cm", Check::non_negative (), Attribute::Const, "\
Position of row on x-axis.");
    frame.set ("row_position", 0.0);
    frame.declare ("soil_depth", "cm", Check::positive (), Attribute::Const, "\
Limit on root depth by soil (no crops have roots below this).");
    frame.declare ("crop_depth", "cm", Check::positive (), Attribute::Const, "\
Limit of root depth by crop (no soil have roots below this).");
    frame.declare ("crop_width", "cm", Check::positive (), Attribute::Const, "\
Maximum horizontal distance of roots from plant.");
    frame.declare ("WRoot", "g DM/m^2", Check::positive (), Attribute::Const, "\
Totoal root dry matter.");
    frame.declare ("DS", "DS", Attribute::Const, "Development stage [0-2].\n\
Not currently used.");
    frame.set ("DS", 2.0);
  }
} ProgramGP2D_syntax;

// rootdens_GP2D.C ends here.
