// source_file.C -- File source for gnuplot interface 
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "source_file.h"
#include "frame.h"
#include "gnuplot_utils.h"
#include "vcheck.h"
#include "mathlib.h"
#include "submodeler.h"
#include <algorithm>
#include <numeric>

void
SourceFile::add_entry (const Time& time, std::vector<double>& vals)
{
  const Time modified = time + time_offset;

  const double total = std::accumulate (vals.begin (), vals.end (), 0.0);
  if (use_sum)
    {
      times.push_back (modified);
      values.push_back (total);
      ebars.push_back (0.0);
    }
  else if (use_all)
    {
      for (size_t i = 0; i < vals.size (); i++)
        {
          times.push_back (modified);
          values.push_back (vals[i]);
          ebars.push_back (0.0);
        }
    }
  else
    {
      if (vals.size () > 1 && !explicit_with)
        with_ = "errorbars";

      const double N = vals.size ();
      const double mean = total / N;
      double variance = 0;
      for (size_t i = 0; i < vals.size (); i++)
        { 
          const double diff = vals[i] - mean;
          variance += diff * diff;
        }
      variance /= N;
      const double std_deviation = sqrt (variance);
      times.push_back (modified);
      values.push_back (mean);
      ebars.push_back (std_deviation);
    }
  daisy_assert (times.size () == values.size ());
  daisy_assert (values.size () == ebars.size ());
  vals.clear ();
}

bool
SourceFile::read_header (Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;

  // Choose lines or points from type.
  if (with_ == "")
    {
      const std::string type = lex.type ();
      if (type == "dwf-0.0")
	with_ = "lines";
      else if (type == "dlf-0.0")
	with_ = "lines";
      else if (type == "ddf-0.0")
	with_ = "points";
    }
  
  return lex.good ();
}

bool
SourceFile::read_entry (std::vector<std::string>& entries, Time& time)
{
  // Read entries.
  if (!lex.get_entries (entries))
    return false;

  if (!lex.get_time_dh (entries, time, default_hour))
    return false;

  // If we survived here, everything is fine.
  return true;
}

void
SourceFile::load_style (Frame& frame, 
                        const symbol default_title)
{
  LexerTable::load_syntax (frame);
  GnuplotUtil::load_style (frame, "\
By default, data from dwf and dlf files will be\n\
drawn with lines, and data from ddf files will be drawn with points.", 
                           default_title);
  frame.declare_boolean ("accumulate", Attribute::Const, "\
Accumulate values.");
  frame.set ("accumulate", false);
  frame.declare_string ("handle", Attribute::Const, "\
Determine how to handle multiple simultaniously.  Possible values are:\n\
\n\
all: show all values.\n\
\n\
sum: use the sum of the values.\n\
\n\
normal: use the arithmetic average of the values, and calculate the\n\
standard deviation.");
  frame.declare_integer ("default_hour", Attribute::Const, "\
Hour to assume when nothing else is specified;");
  frame.set ("default_hour", 8);
  frame.set_check ("default_hour", VCheck::valid_hour ());
  frame.declare_submodule ("time_offset", Attribute::Const, "\
Add this to time from sources.\n\
By default, use unmodified times.",
                           Timestep::load_syntax);
  static VCheck::Enum handle_check ("sum", "normal", "all");
  frame.set_check ("handle", handle_check);
  frame.set ("handle", "normal");
  frame.declare_string ("timestep", Attribute::Const, "\
Multiple with this dimension when accumulating.");
  frame.set ("timestep", "h");
}

SourceFile::SourceFile (const BlockModel& al)
  : Source (al),
    lex (al),
    with_ (al.name ("with", "")),
    explicit_with (al.check ("with")),
    style_ (al.integer ("style", -1)),
    accumulate_ (al.flag ("accumulate")),
    timestep (al.name ("timestep")),
    use_sum (al.name ("handle") == "sum"),
    use_all (al.name ("handle") == "all"),
    default_hour (al.integer ("default_hour")),
    time_offset (submodel_value<Timestep> (al, "time_offset"))
{ }

SourceFile::~SourceFile ()
{ }

// source_file.h ends here.
