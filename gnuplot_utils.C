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

#include "gnuplot_utils.h"
#include "syntax.h"
#include "alist.h"

void 
GnuplotUtil::load_style (Syntax& syntax, AttributeList&, 
                         const std::string& default_with, 
                         const std::string& default_title)
{
  Syntax::category with_cat = Syntax::Const;
  
  // With
  std::string with_doc = "\
Specify style used for the data series on the graph.\n\
use 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.";
  if (default_with.size () > 1)
    {
      with_cat = Syntax::OptionalConst;
      with_doc += "\n\n" + default_with;
    }
  syntax.add ("with", Syntax::String, with_cat, with_doc);

  // Style
  syntax.add ("style", Syntax::Integer, Syntax::OptionalConst, "\
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
  Syntax::category title_cat = Syntax::Const;
  std::string title_doc = "\
Name of data series for the legend on the graph.";
  if (default_title.size () > 1)
    {
      title_cat = Syntax::OptionalConst;
      title_doc += "\n\n" + default_title;
    }
  syntax.add ("title", Syntax::String, title_cat, title_doc);
}

// gnuplot_utils.C ends here
