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
#include "block.h"
#include "alist.h"
#include "gnuplot_utils.h"
#include "number.h"
#include "vcheck.h"
#include "assertion.h"


class XYSourceInline : public XYSource
{
  // Content.
  const std::string with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  PLF plf;
  const std::string title_;
  symbol x_dimension_;
  symbol y_dimension_;

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
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
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
public:
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
    with_ (al.name ("with")),
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
  static Model& make (Block& al)
  { return *new XYSourceInline (al); }

  XYSourceInlineSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    XYSource::load_syntax (syntax, alist);
    alist.add ("description", 
	       "A list of x, y pairs.");
    GnuplotUtil::load_style (syntax, alist, "", "\
By default the name of the 'x' and 'y' objects.");
    syntax.add ("points", Syntax::Unknown (), Syntax::Unknown (), 
		Syntax::Const, Syntax::Singleton, "\
List of (x y) pairs.");
    syntax.add ("x_dimension", Syntax::String, Syntax::Const, "\
Dimension for x points.");
    syntax.add ("y_dimension", Syntax::String, Syntax::Const, "\
Dimension for y points.");

    BuildBase::add_type (XYSource::component, "inline", alist, syntax, &make);
  }
} XYSourceInline_syntax;

// xysource_inline.C ends here.
