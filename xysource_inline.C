// xysource_inline.h -- Specify points inline.
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

#include "xysource.h"
#include "number.h"
#include "vcheck.h"


class XYSourceInline : public XYSource
{
  // Content.
  std::string with_;
  const bool explicit_with;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  PLF plf;
  const std::string title_;
  std::string x_dimension_;
  std::string y_dimension_;

  // Interface.
public:
  const std::string& title () const
  { return title_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  const std::string& with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::string& x_dimension () const 
  { return x_dimension_; }
  const std::string& y_dimension () const 
  { return y_dimension_; }

  // Read.
public:
 bool load (Treelog& msg);

  // Create.
public:
  explicit XYSourceInline (Block&);
private:
  XYSourceInline (const XYSourceInline&);
  XYSourceInline& operator= (const XYSourceInline&);
  ~XYSourceInline ();
};

bool
XYSourceInline::load (Treelog&)
{
  // Read data.
  daisy_assert (xs.size () == ys.size ());
  for (size_t i = 0; i < plf.size (); i++)
    {
      xs.push_back (plf.x (i));
      ys.push_back (plf.y (i));
    }
  daisy_assert (xs.size () == ys.size ());

  // Done.
  return true;
}

XYSourceInline::XYSourceInline (Block& al)
  : XYSource (al),
    with_ (al.name ("with", "")),
    explicit_with (al.check ("with")),
    style_ (al.integer ("style", -1)),
    plf (al.plf ("points")),
    title_ (al.name ("title")),
    x_dimension_ (al.name ("x_dimension")),
    y_dimension_ (al.name ("y_dimension"))
{ }

XYSourceInline::~XYSourceInline ()
{ }


static struct XYSourceInlineSyntax
{
  static XYSource& make (Block& al)
  { return *new XYSourceInline (al); }

  XYSourceInlineSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    XYSource::load_syntax (syntax, alist);
    alist.add ("description", 
	       "A list of x, y pairs.");
    syntax.add ("with", Syntax::String, Syntax::Const, "\
Specify 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.");
    alist.add ("with", "points");
    static VCheck::Enum with ("lines", "points", "points pointsize 3 pointtype 2");
    syntax.add_check ("with", with);
    syntax.add ("style", Syntax::Integer, Syntax::OptionalConst, "\
Style to use for this dataset.  By default, gnuplot will use style 1\n\
for the first source to plot with lines, style 2 for the second, and\n\
so forth until it runs out of styles and has to start over.  Points\n\
work similar, but with its own style counter.  For color plots, points\n\
and lines with the same style number also have the same color.");
    syntax.add ("points", Syntax::Unknown (), Syntax::Unknown (), 
		Syntax::Const, Syntax::Singleton, "\
List of (x y) pairs.");
    syntax.add ("title", Syntax::String, Syntax::Const, "\
Name of data legend in plot.");
    syntax.add ("x_dimension", Syntax::String, Syntax::Const, "\
Dimension for x points.");
    syntax.add ("y_dimension", Syntax::String, Syntax::Const, "\
Dimension for y points.");

    Librarian<XYSource>::add_type ("inline", alist, syntax, &make);
  }
} XYSourceInline_syntax;

// xysource_inline.C ends here.
