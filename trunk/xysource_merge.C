// xysource_merge.C -- Merge data sources for gnuplot interface 
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
#include "scope_sources.h"
#include "units.h"
#include "vcheck.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>

struct XYSourceMerge : public XYSource
{
  // Source.
  const std::vector<XYSource*> source;
  const symbol title_;
  symbol x_dimension_;
  symbol y_dimension_;
  symbol with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;

  // Interface.
public:
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  symbol title () const
  { return title_; }
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
  { return y_dimension_; }

  // Read. 
public:
 bool load (const Units&, Treelog& msg);

  // Create and Destroy.
public:
  explicit XYSourceMerge (const BlockModel& al);
  ~XYSourceMerge ()
  { sequence_delete (source.begin (), source.end ()); }
};

bool
XYSourceMerge::load (const Units& units, Treelog& msg)
{
  // Propagate.
  bool ok = true;
  for (size_t i = 0; i < source.size (); i++)
    {
      // Load.
      std::ostringstream tmp;
      tmp << "[" << i << "] " << source[i]->title ();
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (units, msg))
        {
          ok = false;
          continue;
        }

      // Check convertions.
      if (!units.can_convert (source[i]->x_dimension (), x_dimension ()))
        {
          msg.error ("Cannot convert x dimension from [" 
                     + source[i]->x_dimension () + "] to ["
                     + x_dimension () + "]");
          ok = false;
        }
      if (!units.can_convert (source[i]->y_dimension (), y_dimension ()))
        {
          msg.error ("Cannot convert y dimension from [" 
                     + source[i]->y_dimension () + "] to ["
                     + y_dimension () + "]");
          ok = false;
        }
    }
  if (!ok)
    return false;
  
  // Inherit style.
  if (with_ == "")
    with_ = source[0]->with ();

  // Merge.
  for (size_t i = 0; i < source.size (); i++)
    {
      for (size_t j = 0; j < source[i]->x ().size (); j++)
        {
          static bool has_warned = true;

          const double x = source[i]->x ()[j];
          if (!units.can_convert (source[i]->x_dimension (), x_dimension (),
                                   x))
            {
              if (has_warned)
                continue;
              has_warned = true;
              std::stringstream tmp;
              tmp << "Cannot convert x value " << x << "from [" 
                  << source[i]->x_dimension () << "] to ["
                  << x_dimension () <<  "]";
              msg.debug (tmp.str ());
              continue;
            }

          const double y = source[i]->y ()[j];
          if (!units.can_convert (source[i]->y_dimension (), y_dimension (),
                                   y))
            {
              if (has_warned)
                continue;
              has_warned = true;
              std::stringstream tmp;
              tmp << "Cannot convert y value " << y << "from [" 
                  << source[i]->y_dimension () << "] to ["
                  << y_dimension () <<  "]";
              msg.debug (tmp.str ());
              continue;
            }

          xs.push_back (units.convert (source[i]->x_dimension (), 
                                       x_dimension (),
                                       x));
          ys.push_back (units.convert (source[i]->y_dimension (), 
                                       y_dimension (),
                                       y));
        }
    }

  // Done.
  return true;
}

XYSourceMerge::XYSourceMerge (const BlockModel& al)
  : XYSource (al),
    source (Librarian::build_vector<XYSource> (al, "source")),
    title_ (al.name ("title")),
    x_dimension_ (al.name ("x_dimension")),
    y_dimension_ (al.name ("y_dimension")),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1))
{ }

static struct XYSourceMergeSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceMerge (al); }

  XYSourceMergeSyntax ()
    : DeclareModel (XYSource::component, "merge", 
	       "Merge multiple xy data series into one.")
  { }
  void load_frame (Frame& frame) const
  { 
    GnuplotUtil::load_style (frame, "\
By default, let the first source decide.", "");

    frame.declare_object ("source", XYSource::component,
                       Attribute::State, Attribute::Variable,
                       "XY data series to merge.");
    frame.set_check ("source", VCheck::min_size_1 ());
    frame.declare_string ("x_dimension", Attribute::Const, "\
Dimension for x points.");
    frame.declare_string ("y_dimension", Attribute::Const, "\
Dimension for y points.");

  }
} XYSourceMerge_syntax;

// xysource_merge.C ends here.
