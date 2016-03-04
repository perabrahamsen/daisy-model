// gnuplot_profile.C -- Plot 2D soil profile.
// 
// Copyright 2005 and 2010 Per Abrahamsen and KVL.
// Copyright 2012 KU.
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
#include "gnuplot_base.h"
#include "column.h"
#include "soil.h"
#include "horizon.h"
#include "geometry_rect.h"
#include "treelog.h"
#include "assertion.h"
#include "librarian.h"
#include "block_model.h"


#include <vector>
#include <map>

struct GnuplotProfile : public GnuplotBase
{
  //Content.
  const std::unique_ptr<Column> column;

  // Plot.
  std::vector<double> zplus;
  std::vector<double> xplus;
  std::vector<int> value;
  int max_value;
  
  // Use.
  bool initialize (const Units& units, Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotProfile (const BlockModel& al);
  ~GnuplotProfile ();
};

bool
GnuplotProfile::initialize (const Units& units, Treelog& msg)
{ 
  const Soil& soil = column->get_soil ();
  const Geometry& geo1 = column->get_geometry ();
  if (!dynamic_cast<const GeometryRect*> (&geo1))
    {
      msg.error ("Can only show profile for rectangular grid geometries");
      return false;
    }
  const GeometryRect& geo = dynamic_cast<const GeometryRect&> (geo1);
  
  std::map<symbol, int> all;
  int next = 0;
  for (size_t row = 0; row < geo.cell_rows (); row++)
    {
      for (size_t col = 0; col < geo.cell_columns (); col++)
        {
          const size_t cell = geo.cell_index (row, col);
          if (col == 0)
            zplus.push_back (geo.zplus (cell));
          if (row == 0)
            xplus.push_back (geo.xplus (cell));
          
          const Horizon& horizon = soil.horizon (cell);
          const symbol name = horizon.objid;
          std::map<symbol, int>::const_iterator i = all.find (name);
          if (i == all.end ())
            {
              value.push_back (next);
              all[name] = next;
              next++;
            }
          else
            {
              const int val = (*i).second;
              value.push_back (val);
            }
        }
    }
  daisy_assert (next > 0);
  max_value = next - 1;
  daisy_assert (value.size () == geo.cell_size ());
  daisy_assert (zplus.size () == geo.cell_rows ());
  daisy_assert (xplus.size () == geo.cell_columns ());
  
    // Done.
  return true;
}

bool
GnuplotProfile::plot (std::ostream& out, Treelog& msg)
{ 
  if (xplus.size () < 1 || zplus.size () < 1)
    {
      msg.warning ("Nothing to plot");
      return false;
    }

  // Header.
  plot_header (out);
  out << "\
set pm3d map\n\
set pm3d corners2color c4\n\
unset colorbox\n";
  // Same size axes.
  out << "\
set size ratio -1\n";

  // Legend.
  if (legend != "auto")
    out << "set key " << legend_table[legend] << "\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "splot '-' using 2:1:3 title \"\"\n";

  // Data.
  daisy_assert (value.size () == xplus.size () * zplus.size ());

  // Cell corners only.
  daisy_assert (value.size () > 0);
  out << "0 0 " << value[0] << "\n";
  daisy_assert (value.size () >= zplus.size ());
  for (size_t iz = 0; iz < zplus.size (); iz++)
    out << zplus[iz] << " 0 " << value[iz * xplus.size ()] << "\n";
  for (size_t ix = 0; ix < xplus.size (); ix++)
    {
      out << "\n0 " << xplus[ix] << " " << value[ix] << "\n";
      for (size_t iz = 0; iz < zplus.size (); iz++)
        {
          const size_t c = ix + iz * xplus.size ();
          daisy_assert (c < value.size ());
          out << zplus[iz] << " " << xplus[ix] << " " << value[c] << "\n";
        }
    }

  out << "e\n";

  // The end.
  if (interactive ())
    out << "pause mouse\n";

  return true;
}

GnuplotProfile::GnuplotProfile (const BlockModel& al)
  : GnuplotBase (al),
    column (Librarian::build_item<Column> (al, "column"))
{
  column->initialize (al);
}

GnuplotProfile::~GnuplotProfile ()
{ }

static struct GnuplotProfileSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GnuplotProfile (al); }
  GnuplotProfileSyntax ()
    : DeclareModel (Gnuplot::component, "profile", "common",
                    "Plot 2D soil profile.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("column", Column::component,
                          Attribute::Const, Attribute::Singleton, "\
Column whose soil profile to plot.");
  }
} GnuplotProfile_syntax;

// gnuplot_profile.C ends here.
