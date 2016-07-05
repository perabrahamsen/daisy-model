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
#include "vcheck.h"
#include "filepos.h"
#include <fstream>
#include <sstream>
#include <numeric>

struct ProgramPost : public Program
{
  // The Handle subtype.
  struct Handle
  {
    // Enum in a namespace.
    enum handle_t { all, sum, average };
    handle_t value;
    static handle_t symbol2handle (symbol s);
    operator handle_t () const
    { return value; }
    static const VCheck& check ();
    static void add_syntax (Frame&, const symbol name, const symbol value);
    Handle (handle_t v)
      : value (v)
    { }
    Handle (symbol s)
      : value (symbol2handle (s))
    { }
  };
  const Handle handle_z;
  const Handle handle_x;
  
  // Content.
  const Units& units;
  const symbol parsed_from_file;
  const bool specified_time_columns;
  std::vector<Time::component_t> time_columns;
  DLF print_header;		// How much header should be printed?
  const symbol where;           // Output file.
  const std::unique_ptr<Time> after;
  const std::unique_ptr<Time> before;
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

ProgramPost::Handle::handle_t
ProgramPost::Handle::symbol2handle (symbol s)
{
  static struct sym_set_t : std::map<symbol, handle_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,handle_t> ("all", all));
      insert (std::pair<symbol,handle_t> ("sum", sum));
      insert (std::pair<symbol,handle_t> ("average", average));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

const VCheck& 
ProgramPost::Handle::check ()
{
  static VCheck::Enum check ("all", "sum", "average");
  return check;
}

void
ProgramPost::Handle::add_syntax (Frame& frame, const symbol name, const symbol value)
{
  (void) symbol2handle (value);	// Check that value is valid.
  frame.declare_string (name, Attribute::Const, "\
How to handle multiple values for specified dimension.\n\
Legal values are:\n\
all: output all values.\n\
sum: output the sum of the values, multiplied by the length of the cell\n\
average: output the lenbth weighted average content of the cells.");
  frame.set_check (name, check ());
  frame.set (name, value);
}

static double map_sum (const std::map<double,double>& m)
{
  double sum = 0.0;
  for (auto i : m)
    sum += i.second;
  return sum;
}

static const char* tab (bool& first)
{
  if (first)
    {
      first = false;
      return "";
    }
  return "\t";
}

static void check_boundary (const symbol name, const double value,
			    const std::vector<double>& bounds, Treelog& msg)
{
  if (!std::isnormal (value) || bounds.size () == 0)
    return;
  double closest_bound = 0.0;
  double closest_dist = std::abs (value);
  for (auto bound : bounds)
    {
      const double dist = std::abs (value - bound);
      if (dist < closest_dist)
	{
	  closest_dist = dist;
	  closest_bound = bound;
	}
    }
  if (closest_dist > 1e-42)
    {
      std::ostringstream tmp;
      tmp << name << ": No boundary at " << value
	  << ", closest at "  << closest_bound;
      msg.warning (tmp.str ());
    }
}

bool
ProgramPost::run (Treelog& msg)
{ 
  // Read header.
  if (!lex.read_header (msg))
    {
      msg.error ("Couldn't read header");
      return false;
    }
  if (!lex.read_soil (msg))
    return false;

  if (!lex.good ())
    return false;

  // Array.
  const symbol tag = lex.soil_tag ();
  const std::vector<double> soil_zplus = lex.soil_zplus ();
  const std::vector<double> soil_xplus = lex.soil_xplus ();
  const std::vector<double> cell_z = lex.cell_z ();
  const std::vector<double> cell_x = lex.cell_x ();
  const std::vector<double> cell_dz = lex.cell_dz ();
  const std::vector<double> cell_dx = lex.cell_dx ();
  const size_t array_size = cell_z.size ();
  const bool source_1D = cell_x.size () == 0;
  daisy_assert (source_1D || array_size == cell_x.size ());
  if (array_size < 1)
    {
      msg.warning ("Nothing to plot");
      return false;
    }
  check_boundary ("left", left, soil_xplus, msg);
  check_boundary ("right", right, soil_xplus, msg);
  check_boundary ("top", top, soil_zplus, msg);
  check_boundary ("bottom", bottom, soil_zplus, msg);
  
  // Use time columns of source by default.
  if (!specified_time_columns)
    {
      for (Time::component_t i = Time::First; i <= Time::Last; i++)
        if (lex.find_tag (Time::component_name (i)) >= 0)
          time_columns.push_back (i);
    }
  
  // Quick check of matching cells.
  std::vector<bool> check;
  std::map<double,double> dx_x;
  std::map<double,double> dz_z;
  std::set<double> used_x;
  std::set<double> used_z;
  
  for (size_t i = 0; i < array_size; i++)
    {
      if (std::isfinite (top) && cell_z[i] > top)
        check.push_back (false);
      else if (std::isfinite (bottom) && cell_z[i] < bottom)
        check.push_back (false);
      else if (!source_1D && std::isfinite (left) && cell_x[i] < left)
        check.push_back (false);
      else if (!source_1D && std::isfinite (right) && cell_x[i] > right)
        check.push_back (false);
      else
        {
          check.push_back (true);
	  dz_z[cell_z[i]] = cell_dz[i];
	  used_z.insert (cell_z[i]);
          if (!source_1D)
	    {
	      used_x.insert (cell_x[i]);
	      dx_x[cell_x[i]] = cell_dx[i];
	    }
        }
    }
  daisy_assert (check.size () == array_size);
  if (dz_z.size () < 1)
    lex.warning ("No matching data");
  const bool sink_1D = dx_x.size () == 0;

  const double height = map_sum (dz_z);
  daisy_assert (height > 0.0);
  const double width = map_sum (dx_x);
  daisy_assert (sink_1D || width > 0.0);
  
  double size_factor = 1.0;
  if (handle_z == Handle::average)
    size_factor /= height;
  if (handle_x == Handle::average && !sink_1D)
    size_factor /= width;

  std::ostringstream tmp;
  tmp << "Height: " << height << ", width: " << width;
  msg.message (tmp.str ());
  
  // Dimension.
  const symbol lex_original (lex.soil_dimension ());
  const symbol original =
    (handle_z == Handle::sum && handle_x == Handle::sum)
    ? Units::multiply (lex_original, Units::cm2 ())
    : (handle_z == Handle::sum || handle_x == Handle::sum)
    ? Units::multiply (lex_original, Units::cm ())
    : lex_original;
  
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
  print_header.parameter (out, "WIDTH", width, Units::cm ());
  print_header.parameter (out, "HEIGHT", height, Units::cm ());
  print_header.finish (out);

  // Tag line.
  bool first_tag = true;

  for (size_t i = 0; i < time_columns.size (); i++)
    out << tab (first_tag) << Time::component_name (time_columns[i]);

  int count_columns = 0;
  if (handle_x == Handle::all && handle_z == Handle::all)
    for (size_t i = 0; i < array_size; i++)
      {
	if (!check[i])
	  continue;

	count_columns++;
	
	out << tab (first_tag) << tag << " @ ";
	if (sink_1D)
	  out << cell_z[i];
	else
	  out << "(" << cell_z[i] << " " << cell_x[i] << ")";
      }
  else if (handle_x == Handle::all && handle_z != Handle::all)
    for (auto x : used_x)
      {
	count_columns++;
	out << tab (first_tag) << tag << " @ " << x;
      }
  else if (handle_x != Handle::all && handle_z == Handle::all)
    for (std::set<double>::reverse_iterator i = used_z.rbegin ();
	 i != used_z.rend ();
	 i++)
      {
	count_columns++;
	out << tab (first_tag) << tag << " @ " << *i;
      }
  else
    {
      daisy_assert (handle_x != Handle::all && handle_z != Handle::all);
      count_columns++;
      out << tab (first_tag) << tag;
    }
  out << "\n";

  // Dimensions.
  bool first_dim = true;

  for (size_t i = 0; i < time_columns.size (); i++)
    out << tab (first_dim);

  for (size_t i = 0; i < count_columns; i++)
    out << tab (first_dim) << dimension;
    
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
      if (!lex.get_time_dh (entries, time, 8))
        continue;

      if ((after.get () && time < *after)
           || (before.get () && time > *before))
        continue;

      bool first_data = true;
      
      for (size_t i = 0; i < time_columns.size (); i++)
        {
          out << tab (first_data) << time.component_value (time_columns[i]);
        }
      
      std::vector<double> value;
      if (!lex.soil_cells (entries, value, msg))
        {
          msg.error ("Problem reading cell data");
          return false;
        }

      auto print_number = [&](double number) -> void
	{
          if (dimension != original)
            {
              if (!units.can_convert (original, dimension, number))
                {
                  std::ostringstream tmp;
                  tmp << "Can't convert " << number << " from [" << original 
                      << "] to [" << dimension << "]";
                  lex.error (tmp.str ());
                }
              number = units.convert (original, dimension, number);
            }
          out << tab (first_data) << number;
	};
	
      
      // Projections.
      std::map<double,double> x_sum;
      std::map<double,double> z_sum;
      double  sum = 0.0;
      
      for (size_t i = 0; i < array_size; i++)
        {
          if (!check[i])
            continue;
          
          if (!out.good ())
            {
              msg.error ("'" + where + "': file error");
              return false;
            }

          double number = value[i];

	  // Projections.
	  const double dz = cell_dz[i];
	  const double z = cell_z[i];
	  const double dx = source_1D ? 1.0 : cell_dx[i];
	  const double x = source_1D ? 0.0 : cell_x[i];
	  x_sum[x] += number * dz;
	  z_sum[z] += number * dx;
	  sum += number * dx * dz;
	  
	  if (handle_z != Handle::all || handle_x != Handle::all)
	    continue;
	  
	  // Convert
	  print_number (number);
        }

      if (handle_x == Handle::all && handle_z != Handle::all)
	{
	  for (auto x : used_x)
	    print_number (x_sum[x] * size_factor);
	}
      else if (handle_x != Handle::all && handle_z == Handle::all)
	{
	  for (std::set<double>::reverse_iterator i = used_z.rbegin ();
	       i != used_z.rend ();
	       i++)
	    print_number (z_sum[*i] * size_factor);
	}
      else
	print_number (sum * size_factor);

      out << "\n";
    }
  return true;
}

ProgramPost::ProgramPost (const BlockModel& al)
  : Program (al),
    handle_z (Handle::symbol2handle (al.name ("handle_z"))),
    handle_x (Handle::symbol2handle (al.name ("handle_x"))),
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
    ProgramPost::Handle::add_syntax (frame, "handle_z", "all");
    ProgramPost::Handle::add_syntax (frame, "handle_x", "all");
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
Dimension for data output.  By default, use dimension from file.");
  }
} ProgramPost_syntax;

// program_post.C ends here.
