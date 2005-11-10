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


#include "source_file.h"
#include "vcheck.h"
#include "mathlib.h"
#include <algorithm>
#include <numeric>

void
SourceFile::add_entry (const Time& time, std::vector<double>& vals)
{
  if (vals.size () > 1 && !explicit_with)
    with_ = "errorbars";
  const double total = std::accumulate (vals.begin (), vals.end (), 0.0);
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
  times.push_back (time);
  values.push_back (mean);
  ebars.push_back (std_deviation);
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

  if (!lex.get_time (entries, time))
    return false;

  // If we survived here, everything is fine.
  return true;
}

void
SourceFile::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Source::load_syntax (syntax, alist);
  LexerTable::load_syntax (syntax, alist);

  syntax.add ("with", Syntax::String, Syntax::OptionalConst, "\
Specify 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.  By default, data from dwf and dlf files will be\n\
drawn with lines, and data from ddf files will be drawn with points.");
  static VCheck::Enum with ("lines", "points");
  syntax.add_check ("with", with);
  syntax.add ("style", Syntax::Integer, Syntax::OptionalConst, "\
Style to use for this dataset.  By default, gnuplot will use style 1\n\
for the first source to plot with lines, style 2 for the second, and\n\
so forth until it runs out of styles and has to start over.  Points\n\
work similar, but with its own style counter.  For color plots, points\n\
and lines with the same style number also have the same color.");
}

SourceFile::SourceFile (Block& al)
  : Source (al),
    lex (al),
    with_ (al.name ("with", "")),
    explicit_with (al.check ("with")),
    style_ (al.integer ("style", -1))
{ }

SourceFile::~SourceFile ()
{ }

// source_file.h ends here.
