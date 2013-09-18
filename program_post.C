// program_post.C -- Select a subset of a soil profile log file.
// 
// Copyright 2013 KU.
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
#include "dlf.h"
#include "lexer_soil.h"
#include "units.h"
#include "time.h"
#include "mathlib.h"
#include "submodeler.h"
#include "librarian.h"
#include "check.h"
#include "filepos.h"
#include <boost/scoped_ptr.hpp>
#include <fstream>
#include <sstream>

struct ProgramPost : public Program
{
  const Units& units;
  const symbol parsed_from_file;
  const bool specified_time_columns;
  std::vector<Time::component_t> time_columns;
  DLF print_header;		// How much header should be printed?
  const symbol where;           // Output file.
  const boost::scoped_ptr<Time> after;
  const boost::scoped_ptr<Time> before;
  const double top;
  const double bottom;
  const double left;
  const double right;
  const symbol file;            // Input file.
  LexerSoil lex;
  symbol dimension;
  
  // Use.
  bool run (Treelog&);
  
  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }
  explicit ProgramPost (const BlockModel& al);
  ~ProgramPost ();
};

bool
ProgramPost::run (Treelog& msg)
{ 
  // Read header.
  if (!lex.read_header (msg))
    return false;
  if (!lex.read_soil (msg))
    return false;

  if (!lex.good ())
    return false;

  // Array.
  const symbol tag = lex.soil_tag ();
  const std::vector<double> z = lex.soil_z ();
  const std::vector<double> x = lex.soil_x ();
  const size_t array_size = z.size ();
  const bool source_1D = x.size () == 0;
  daisy_assert (source_1D || array_size == x.size ());
  if (array_size < 1)
    {
      msg.warning ("Nothing to plot");
      return false;
    }

  // Use time columns of source by default.
  if (!specified_time_columns)
    {
      for (Time::component_t i = Time::First; i <= Time::Last; i++)
        if (lex.find_tag (Time::component_name (i)) >= 0)
          time_columns.push_back (i);
    }
  
  // Quick check of matching cells.
  bool sink_1D = true;
  double last_x = NAN;
  bool found = false;
  std::vector<bool> check;
  for (size_t i = 0; i < array_size; i++)
    {
      if (std::isfinite (top) && z[i] > top)
        check.push_back (false);
      else if (std::isfinite (bottom) && z[i] < bottom)
        check.push_back (false);
      else if (!source_1D && std::isfinite (left) && x[i] < left)
        check.push_back (false);
      else if (!source_1D && std::isfinite (right) && x[i] > right)
        check.push_back (false);
      else
        {
          check.push_back (true);
          found = true;
          if (!source_1D && sink_1D)
            {
              if (!std::isfinite (last_x))
                last_x = x[i];
              else if (!approximate (last_x, x[i]))
                sink_1D = false;
            }
        }
    }
  daisy_assert (check.size () == array_size);
  if (!found)
    lex.warning ("No matching data");

  // Dimension.
  const symbol original (lex.soil_dimension ());

  if (dimension == Attribute::Unknown ())
    dimension = original;
  else if (!units.can_convert (original, dimension))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert from [" << original 
          << "] to [" << dimension << "]";
      lex.error (tmp.str ());
      return false;
    }

  std::ofstream out (where.name ().c_str ());
  print_header.start (out, objid, where, parsed_from_file);
  print_header.parameter (out, "SOURCE", file);
  print_header.finish (out);

  // Tag line.
  bool first_tag = true;

  for (size_t i = 0; i < time_columns.size (); i++)
    {
      if (first_tag)
        first_tag = false;
      else
        out << "\t";
      
      out << Time::component_name (time_columns[i]);
    }

  for (size_t i = 0; i < array_size; i++)
    {
      if (!check[i])
        continue;
      
      if (first_tag)
        first_tag = false;
      else
        out << "\t";

      out << tag << " @ ";
      if (sink_1D)
        out << z[i];
      else
        out << "(" << z[i] << " " << x[i] << ")";
      found = true;
    }
  out << "\n";

  // Dimensions.
  bool first_dim = true;

  for (size_t i = 0; i < time_columns.size (); i++)
    {
      if (first_dim)
        first_dim = false;
      else
        out << "\t";
    }

  for (size_t i = 0; i < array_size; i++)
    {
      if (!check[i])
        continue;
      
      if (first_dim)
        first_dim = false;
      else
        out << "\t";
      
      out << dimension;
    }
    
  // End of header.
  print_header.finish (out);
  out << "\n";

  // Read data.
  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      // Read entries.
      if (!lex.get_entries (entries))
        continue;
      if (!lex.get_time (entries, time, 8))
        continue;

      if ((after.get () && time < *after)
           || (before.get () && time > *before))
        continue;

      bool first_data = true;
      
      for (size_t i = 0; i < time_columns.size (); i++)
        {
          if (first_data)
            first_data = false;
          else
            out << "\t";

          out << time.component_value (time_columns[i]);
        }
      
      std::vector<double> value;
      if (!lex.soil_cells (entries, value, msg))
        {
          msg.error ("Problem reading cell data");
          return false;
        }

      for (size_t i = 0; i < array_size; i++)
        {
          if (!check[i])
            continue;
          
          // Convert
          double number = value[i];
          if (dimension != original)
            {
              if (!units.can_convert (original, dimension, number))
                {
                  std::ostringstream tmp;
                  tmp << "Can't convert " << number << " from [" << original 
                      << "] to [" << dimension << "]";
                  msg.error (tmp.str ());
                  return false;
                }
              number = units.convert (original, dimension, number);
            }
          if (first_data)
            first_data = false;
          else
            out << "\t";
          out << number;
          if (!out.good ())
            {
              msg.error ("'" + where + "': file error");
              return false;
            }
        }
      out << "\n";
    }
  return true;
}

ProgramPost::ProgramPost (const BlockModel& al)
  : Program (al),
    units (al.units ()),
    parsed_from_file (al.frame ().inherited_position ().filename ()),
    specified_time_columns (al.check ("time_columns")),
    time_columns (al.check ("time_columns")
                  ? Time::find_time_components 
                  (al.name_sequence ("time_columns"))
                  : std::vector<Time::component_t> ()),
    print_header (al.name ("print_header")),
    where  (al.name ("where")),
    after ((al.check ("after"))
           ? submodel<Time> (al, "after")
           : NULL),
    before (al.check ("before")
            ? submodel<Time> (al, "before")
            : NULL),
    top (al.number ("top", NAN)),
    bottom (al.number ("bottom", NAN)),
    left (al.number ("left", NAN)),
    right (al.number ("right", NAN)),
    file (al.name ("file")),
    lex (al),
    dimension (al.name ("dimension", Attribute::Unknown ()))
{ }

ProgramPost::~ProgramPost ()
{ }

static struct ProgramPostSyntax : public DeclareModel

{
  Model* make (const BlockModel& al) const
  { return new ProgramPost (al); }
  ProgramPostSyntax ()
    : DeclareModel (Program::component, "post-process", 
                    "Extract a subset of a soil profile log file.")
  { }
  void load_frame (Frame& frame) const
  {
    Time::declare_time_components (frame, "time_columns", 
                                   Attribute::OptionalConst, "\
List of time components to include in output.\n\
By default, use the same as the source.");
    DLF::add_syntax (frame, "print_header");
    frame.declare_string ("where", Attribute::Const, "\
Name of output file.");
    frame.declare_submodule ("after", Attribute::OptionalConst, "\
Only include values after this time.", Time::load_syntax);
    frame.declare_submodule ("before", Attribute::OptionalConst, "\
Only include values before this time.", Time::load_syntax);
    frame.declare ("top", "cm", Check::non_positive (),
                   Attribute::OptionalConst, "\
Only include values below this height.");
    frame.declare ("bottom", "cm", Check::negative (),
                   Attribute::OptionalConst, "\
Only include values above this height.");
    frame.declare ("left", "cm", Check::non_negative (), 
                   Attribute::OptionalConst, "\
Only incluce values to the right of this position.");
    frame.declare ("right", "cm", Check::positive (), 
                   Attribute::OptionalConst, "\
Only include values to the left of this position.");
    LexerSoil::load_syntax (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
  }
} ProgramPost_syntax;

// program_post.C ends here.
