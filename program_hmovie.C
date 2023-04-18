// rootdens_hmovie.C -- Plot piezometer data.
// 
// Copyright 2011 KU.
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
#include "gnuplot.h"
#include "lexer_table.h"
#include "librarian.h"
#include "submodeler.h"
#include "vcheck.h"
#include "memutils.h"
#include "assertion.h"
#include "mathlib.h"
#include "path.h"
#include "time.h"
#include "units.h"

#define BOOST_UBLAS_NDEBUG
#define NDEBUG
#include <boost/numeric/ublas/matrix.hpp>
namespace ublas = boost::numeric::ublas;

#include <fstream>
#include <iomanip>

struct ProgramHMovie : public Program
{
  const Units& units;
  Path& path;
  const symbol command_file;
  const bool do_cd;

  LexerTable lex;
  const double x_min;
  const double x_max;
  const double y_min;
  const double y_max;
  const double z_min;
  const double z_max;
  const symbol dim;
  struct Tic
  {
    const double value;
    const symbol name;
    static void load_syntax (Frame&);
    Tic (const Block& al)
      : value (al.number ("value")),
        name (al.name ("name"))
    { }
        
  };
  auto_vector<const Tic*> x_tics;
  const double x_tic_max;
  const symbol output_base;
  const int output_width;
  struct Tag
  {
    double x;
    double y;
    symbol tag;
    static void load_syntax (Frame&);
    Tag (const Block& al)
      : x (al.number ("x")),
        y (al.number ("y")),
        tag (al.name ("tag"))
    { }
  };
  auto_vector<const Tag*> tags;
  const double missing_value;

  // Use.
  bool run (Treelog& msg)
  {
    const symbol dir = path.get_output_directory ();
    std::ofstream out (command_file.name ().c_str ());
    if (do_cd)
      out << "cd " << Gnuplot::quote (dir) << "\n";
    print_header (out);

    const size_t tag_size = tags.size ();

    // Find unique x and y values.
    std::set<double> x_set;
    std::set<double> y_set;
    for (size_t i = 0; i < tag_size; i++)
      {
        x_set.insert (tags[i]->x);
        y_set.insert (tags[i]->y);
      }

    // Sort them.
    std::vector<double> x_marks 
      = std::vector<double> (x_set.begin (), x_set.end ());
    std::vector<double> y_marks 
      = std::vector<double> (y_set.begin (), y_set.end ());
    std::sort (x_marks.begin (), x_marks.end ());
    std::sort (y_marks.begin (), y_marks.end ());

    const size_t x_size = x_marks.size ();
    const size_t y_size = y_marks.size ();

    // Index.
    std::map<double, size_t> x_i;
    for (size_t i = 0; i < x_size; i++)
      x_i[x_marks[i]] = i;
    std::map<double, size_t> y_i;
    for (size_t i = 0; i < y_size; i++)
      y_i[y_marks[i]] = i;
    std::map<size_t, size_t> tag_i;
    for (size_t i = 0; i < tag_size; i++)
      {
        int index = lex.find_tag (tags[i]->tag);
        if (index < 0)
          {
            msg.error ("Bad tag '" + tags[i]->tag + "'");
            return false;
          }
        tag_i[i] = index;
      }
    
    size_t number = 1;
    while (lex.good ())
      {
        // Read entries.
        std::vector<std::string> entries;
        if (!lex.get_entries (entries))
          {
            if (!lex.good ())
              break;
            lex.warning ("bad line");
            continue;
          }
        Time time;
        if (!lex.get_time_dh (entries, time, 8))
          {
            lex.warning ("bad time");
            continue;
          }
        // Extract entries.
        ublas::matrix<double> vals (x_size, y_size);
        for (size_t i = 0; i < tag_size; i++)
          {
            daisy_assert (x_i.find (tags[i]->x) != x_i.end ());
            daisy_assert (y_i.find (tags[i]->y) != y_i.end ());
            vals (x_i[tags[i]->x], y_i[tags[i]->y]) 
              = lex.is_missing (entries[tag_i[i]])
              ? NAN
              : units.convert (lex.dimension (tag_i[i]), dim,
                               lex.convert_to_double (entries[tag_i[i]]));
          }

        out << "set label 1 \"" 
            << time.print ().substr (0, 13) << "\" at screen 0.1,0.9\n"
            << "set output '" << output_base
            << std::setfill ('0') 
            << std::setw (output_width) << number << ".png'\n"
            << "splot '-' with lines\n";
        number++;
        print_entry (x_marks, y_marks, vals, out);
      }
    if (!out.good ())
      {
        msg.error ("Problems writing to temporary file '" + command_file + "'");
        return false;
      }
    return true;
  }
  
  void print_header (std::ostream& out) const
  {
    out << "\
set terminal png size 800,200\n\
set title \"piezometer\"\n\
unset key\n\
set xrange [" << x_min << ":" << x_max << "]\n   \
set yrange [" << y_min << ":" << y_max << "]\n\
set zrange [" << z_min << ":" << z_max << "]\n\
set xtics (\"\" 0 1, \"\" 4 1, \"\" 7.3 1, \"P1\" 8, \"\" 8.7 1, \"\" 12 1, \"\" 16 1, \"\" 20 1, \"\" 23.3 1, \"P2\" 24, \"\" 24.7 1, \"\" 28 1, \"\" 32 1, \"\" 36 1, \"\" 39.3 1, \"P3\" 40, \"\" 40.7 1, \"\" 44 1, \"\" 48 1, \"\" 52 1, \"\" 55.3 1, \"P4\" 56, \"\" 56.7 1, \"\" 60 1, \"\" 64 1 )\n\
set ytics ( \"10\" 10, \"40\" 40, \"70\" 70 )\n\
set ticslevel 0\n\
set origin 0,1.1\n\
set view equal xyz\n\
set view 80, 30, 8\n\
set hidden3d\n\
";
  }

  void print_entry (const std::vector<double>& x_marks, 
                    const std::vector<double>& y_marks, 
                    const ublas::matrix<double>& vals,
                    std::ostream& out) const
  {
    const size_t x_size = x_marks.size ();
    const size_t y_size = y_marks.size ();
    const size_t tic_size = x_tics.size ();

    for (size_t i_y = 0; i_y < y_size; i_y++)
      {
        const double y = y_marks[i_y];
        
        size_t i_tic = 0;
        double prev = NAN;
        for (size_t i_x = 0; i_x < x_size; i_x++)
          {
            const double x = x_marks[i_x];
            double value = vals (i_x, i_y);
            if (!std::isfinite (value))
              value = missing_value;
            if (i_tic < tic_size && x_tics[i_tic]->value < x)
              {
                if (!std::isfinite (prev))
                  throw "Bad x_tics";
                
                const double avg = std::min (x_tic_max, 0.5 * (value + prev));
                out << x_tics[i_tic]->value << "\t"
                    << y << "\t" << avg << "\n";
                
                i_tic++;
              }

            out << x << "\t" << y << "\t" << value << "\n";
            prev = value;
          }
        out << "\n";
      }
    out << "e\n";
  }

  // Create and Destroy.
  void initialize (Block& al)
  { 
    if (!lex.read_header (al.msg ()))
      return;

  }

  bool check (Treelog& msg)
  {
    bool ok = true;
    if (!lex.good ())
      ok = false;
    
    for (size_t i = 0; i < tags.size (); i++)
      if (lex.find_tag (tags[i]->tag) < 0)
        {
          ok = false;
          msg.error ("Tag '" + tags[i]->tag + "' missing");
        }

    return ok; 
  }

  ProgramHMovie (const BlockModel& al)
    : Program (al),
      units (al.units ()),
      path (al.path ()),
      command_file (al.name ("command_file")),
      do_cd (al.flag ("cd")),
      lex (al),
      x_min (al.number ("x_min")),
      x_max (al.number ("x_max")),
      y_min (al.number ("y_min")),
      y_max (al.number ("y_max")),
      z_min (al.number ("z_min")),
      z_max (al.number ("z_max")),
      dim (al.name ("dim")),
      x_tics (map_submodel_const<Tic> (al, "x_tics")),
      x_tic_max (al.number ("x_tic_max")),
      output_base (al.name ("output_base")),
      output_width (al.integer ("output_width")),
      tags (map_submodel_const<Tag> (al, "tags")),
      missing_value (al.number ("missing_value"))
  { }
  ~ProgramHMovie ()
  { }
};

void 
ProgramHMovie::Tic::load_syntax (Frame& frame)
{
  frame.declare ("value", Attribute::Unknown (), Attribute::Const, "\
Tic position.");
  frame.declare_string ("name", Attribute::Const, "\
Tic name.");
  frame.order ("value", "name");
}
        
void
ProgramHMovie::Tag::load_syntax (Frame& frame)
{
  frame.declare ("x", Attribute::Unknown (), Attribute::Const, "\
X position.");
  frame.declare ("y", Attribute::Unknown (), Attribute::Const, "\
Y position.");
  frame.declare_string ("tag", Attribute::Const, "\
Column name.");
  frame.order ("x", "y", "tag");
}

static struct ProgramHMovieSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramHMovie (al); }
  ProgramHMovieSyntax ()
    : DeclareModel (Program::component, "hmovie", "\
Manipulate data from Agrovand.")
  { }
  bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;

    const symbol dim = al.name ("dim");
    if (al.name ("x_min") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("x_max") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("y_min") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("y_max") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("z_min") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("z_max") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("x_tic_max") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }
    if (al.name ("missing_value") != dim)
      {
        ok = false;
        msg.error ("Expected dimension " + dim);
      }

    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame);
    frame.declare_string ("command_file", Attribute::Const, "\
File name for gnuplot commands.");
    frame.set ("command_file", "daisy.gnuplot");
    frame.declare_boolean ("cd", Attribute::Const, "\
Set this flag to add a 'cd' command to the current working directory.\n\
This is useful under MS Windows when dragging the file to a gnuplot icon.");
#if defined(__unix)
    frame.set ("cd", false);
#else
    frame.set ("cd", true);
#endif
    LexerTable::load_syntax (frame);
    frame.declare ("x_min", Attribute::User (), Attribute::Const, "\
Min value of x-axes.");
    frame.declare ("x_max", Attribute::User (), Attribute::Const, "\
Max value of x-axes.");
    frame.declare ("y_min", Attribute::User (), Attribute::Const, "\
Min value of y-axes.");
    frame.declare ("y_max", Attribute::User (), Attribute::Const, "\
Max value of y-axes.");
    frame.declare ("z_min", Attribute::User (), Attribute::Const, "\
Min value of z-axes.");
    frame.declare ("z_max", Attribute::User (), Attribute::Const, "\
Max value of z-axes.");
    frame.declare_string ("dim", Attribute::Const, "\
Dimension of all axes.");
    frame.declare_submodule_sequence ("x_tics", Attribute::Const, "\
List of tic markers for the x-axes.",
                                      ProgramHMovie::Tic::load_syntax);
    frame.declare ("x_tic_max", Attribute::User (), Attribute::Const, "\
Highest synthetic z value for at x tics.");
    frame.declare_string ("output_base", Attribute::Const, "\
Prefix used for all output files.");
    frame.declare_integer ("output_width", Attribute::Const, "\
Number of digits in output file name.");
    static VCheck::IRange digits (1, 24);
    frame.set_check ("output_width", digits);
    frame.declare_submodule_sequence ("tags", Attribute::Const, "\
List of column tags for the data file.",
                                      ProgramHMovie::Tag::load_syntax);
    frame.declare ("missing_value", Attribute::User (), Attribute::Const, "\
Replace misisng values with this.");
  }
} ProgramHMovie_syntax;

// program_hmovie.C ends here.
