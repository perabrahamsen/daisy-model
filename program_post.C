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
#include "lexer_soil.h"
#include "units.h"
#include "time.h"
#include "mathlib.h"
#include "submodeler.h"
#include "librarian.h"
#include "check.h"
#include <boost/scoped_ptr.hpp>
#include <fstream>
#include <sstream>

struct ProgramPost : public Program
{
  const Units& units;
  const symbol where;
  const boost::scoped_ptr<Time> after;
  const boost::scoped_ptr<Time> before;
  const double top;
  const double bottom;
  const double left;
  const double right;
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
  const bool one_dimensional = x.size () == 0;
  daisy_assert (one_dimensional || array_size == x.size ());
  if (array_size < 1)
    {
      msg.warning ("Nothing to plot");
      return false;
    }

  // Quick check of matching cells.
  bool found = false;
  std::vector<bool> check;
  for (size_t i = 0; i < array_size; i++)
    {
      if (std::isfinite (top) && z[i] > top)
        check.push_back (false);
      else if (std::isfinite (bottom) && z[i] < bottom)
        check.push_back (false);
      else if (!one_dimensional && std::isfinite (left) && x[i] < left)
        check.push_back (false);
      else if (!one_dimensional && std::isfinite (right) && x[i] > right)
        check.push_back (false);
      else
        {
          check.push_back (true);
          found = true;
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

  // Tag line.
  out << "Time";
  for (size_t i = 0; i < array_size; i++)
    {
      if (!check[i])
        continue;
      
      out << "\t" << tag << " @ ";
      if (one_dimensional)
        out << z[i];
      else
        out << "(" << z[i] << " " << x[i] << ")";
      found = true;
    }

  // Dimensions.
  out << "\ntime";
  for (size_t i = 0; i < array_size; i++)
    {
      if (!check[i])
        continue;
      
      out << dimension;
    }
    

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

      if (time < *after || time > *before)
        continue;

      out << "\n" << time.print ();
      
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
          out << "\t" << number;
          if (!out.good ())
            {
              msg.error ("'" + where + "': file error");
              return false;
            }
        }
    }
  out << "\n";
  return true;
}

ProgramPost::ProgramPost (const BlockModel& al)
  : Program (al),
    units (al.units ()),
    where  (al.name ("where")),
    after (submodel<Time> (al, "after")),
    before (submodel<Time> (al, "before")),
    top (al.number ("top", NAN)),
    bottom (al.number ("bottom", NAN)),
    left (al.number ("left", NAN)),
    right (al.number ("right", NAN)),
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
