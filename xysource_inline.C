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

#define BUILD_DLL
#include "xysource.h"
#include "block_model.h"
#include "gnuplot_utils.h"
#include "number.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"
#include "plf.h"

class XYSourceInline : public XYSource
{
  // Content.
  const symbol with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  PLF plf;
  const symbol title_;
  symbol x_dimension_;
  symbol y_dimension_;

  // Interface.
public:
  symbol title () const
  { return title_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
  { return y_dimension_; }

  // Read.
public:
  bool load (const Units&, Treelog& msg);

  // Create.
public:
  explicit XYSourceInline (const BlockModel&);
private:
  XYSourceInline (const XYSourceInline&);
  XYSourceInline& operator= (const XYSourceInline&);
public:
  ~XYSourceInline ();
};

bool
XYSourceInline::load (const Units&, Treelog&)
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

XYSourceInline::XYSourceInline (const BlockModel& al)
  : XYSource (al),
    with_ (al.name ("with")),
    style_ (al.integer ("style", -1)),
    plf (al.plf ("points")),
    title_ (al.name ("title", "")),
    x_dimension_ (al.name ("x_dimension")),
    y_dimension_ (al.name ("y_dimension"))
{ }

XYSourceInline::~XYSourceInline ()
{ }


static struct XYSourceInlineSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceInline (al); }

  XYSourceInlineSyntax ()
    : DeclareModel (XYSource::component, "inline", 
	       "A list of x, y pairs.")
  { }
  void load_frame (Frame& frame) const
  { 
    GnuplotUtil::load_style (frame, "", "\
By default the name of the 'x' and 'y' objects.");
    frame.declare ("points", Attribute::Unknown (), Attribute::Unknown (), 
                   Attribute::Const, Attribute::Singleton, "\
List of (x y) pairs.");
    frame.declare_string ("x_dimension", Attribute::Const, "\
Dimension for x points.");
    frame.declare_string ("y_dimension", Attribute::Const, "\
Dimension for y points.");

  }
} XYSourceInline_syntax;

// xysource_inline.C ends here.
