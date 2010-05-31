// gnuplot_utils.C --- Various gnuplot related utilities.
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

#include "gnuplot_utils.h"
#include "frame.h"

void 
GnuplotUtil::load_style (Frame& frame,
                         const symbol default_with, 
                         const symbol default_title)
{
  Attribute::category with_cat = Attribute::Const;
  
  // With
  std::string with_doc = "\
Specify style used for the data series on the graph.\n\
use 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.";
  if (default_with.name ().size () > 1)
    {
      with_cat = Attribute::OptionalConst;
      with_doc += "\n\n" + default_with;
    }
  frame.declare_string ("with", with_cat, with_doc);

  // Style
  frame.declare_integer ("style", Attribute::OptionalConst, "\
Style to use for this dataset.\n\
\n\
By default, gnuplot will use style 1 for the first source to plot with\n\
lines, style 2 for the second, and so forth until it runs out of\n\
styles and has to start over.  Points work similar, but with its own\n\
style counter.  For color plots, points and lines with the same style\n\
number also have the same color.\n\
\n\
Set style to 0 to reuse the style of the previous series, or to a\n\
negative number to explicitly request the default behaviour.\n\
\n\
The 'style' parameter is only used if 'with' is either 'points' or 'lines'.");

  // Title.
  Attribute::category title_cat = Attribute::Const;
  std::string title_doc = "\
Name of data series for the legend on the graph.";
  if (default_title.name ().size () > 1)
    {
      title_cat = Attribute::OptionalConst;
      title_doc += "\n\n" + default_title;
    }
  frame.declare_string ("title", title_cat, title_doc);
}

// gnuplot_utils.C ends here
