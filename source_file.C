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

void 
SourceFile::Filter::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file to filter for.");
  syntax.add ("allowed", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of allowable values in filter.");
  syntax.add_check ("allowed", VCheck::min_size_1 ());
  syntax.order ("tag", "allowed");
}

SourceFile::Filter::Filter (const AttributeList& al)
  : tag (al.name ("tag")),
    allowed (al.name_sequence ("allowed"))
{ }

void
SourceFile::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Source::load_syntax (syntax, alist);
  syntax.add ("file", Syntax::String, Syntax::Const, "\
Name of Daisy log file where data is found.");
  syntax.add ("missing", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of strings indicating 'missing value'.");
  std::vector<symbol> misses;
  misses.push_back (symbol (""));
  misses.push_back (symbol ("00.00"));
  alist.add ("missing", misses);
  syntax.add_submodule_sequence ("filter", Syntax::Const, "\
Only include data from rows that passes all these filters.",
				 SourceFile::Filter::load_syntax);
  alist.add ("filter", std::vector<AttributeList*> ());
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

SourceFile::SourceFile (const AttributeList& al)
  : Source (al),
    filename (al.name ("file")),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1)),
    missing (al.name_sequence ("missing")),
    field_sep ("UNINITIALIZED"),
    filter (map_construct_const<Filter> (al.alist_sequence ("filter")))
{ }

// source_file.h ends here.
