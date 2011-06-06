// xysource_profile.h -- Plot 1D soil profile at specific time.
// 
// Copyright 2005 Per Abrahamsen and KVL.
// Copyright 2010,2011 KU.
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
#include "gnuplot_utils.h"
#include "lexer_soil.h"
#include "check.h"
#include "vcheck.h"
#include "geometry.h"
#include "time.h"
#include "units.h"
#include "submodeler.h"
#include "assertion.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>
#include <boost/scoped_ptr.hpp>

struct XYSourceProfile : public XYSource
{ 
  // Profile parameters.
  const boost::scoped_ptr<Time> when;
  double plot_z;
  symbol dimension;
  const symbol pos_dim;

  // Content.
  LexerSoil lex;
  symbol with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  symbol title_;
  symbol value_dimension;

  // Interface.
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
  { return dimension; }
  symbol y_dimension () const 
  { return pos_dim; }

  // Read.
  bool load (const Units&, Treelog& msg);
  void limit (double& xmin, double& xmax, double& ymin, double& ymax) const;

  // Create.
  explicit XYSourceProfile (const BlockModel&);
  ~XYSourceProfile ();
};

bool
XYSourceProfile::load (const Units& units, Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;
  if (!lex.read_soil (msg))
    return false;
  if (!lex.good ())
    return false;

  // Array.
  symbol tag = lex.soil_tag ();
  if (lex.soil_x ().size () > 0)
    {
      msg.error ("Two dimensional data");
      return false;
    }

  ys = lex.soil_z ();
  const size_t array_size = ys.size ();
  if (array_size < 1)
    {
      msg.error ("No cells");
      return false;
    }
  symbol original (lex.soil_dimension ());
  
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

  // Read data.
  std::vector<double> all_values;

  double closest = -42.42e42; // [h]
  
  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      // Read entries.
      if (!lex.get_entries (entries))
        continue;
      if (!lex.get_time (entries, time, 8))
        continue;

      double distance = std::fabs (Time::fraction_hours_between (time, *when));

      if (closest < 0.0 || distance < closest)
        if (lex.soil_cells (entries, all_values, msg))
          closest = distance;

      if (all_values.size () != array_size)
        {
          lex.error ("Wrong number of columns");
          return false;
        }
    }
  if (closest < 0.0)
    {
      msg.error ("No date found");
      return false;
    }

  // Filter and convert
  for (size_t i = 0; i < array_size; i++)
    {
      // Convert.
      const double val = all_values[i];
      if (!units.can_convert (original, dimension, val))
        {
          std::ostringstream tmp;
          tmp << "Can't convert " << val << " from [" << original 
              << "] to [" << dimension << "]";
          msg.error (tmp.str ());
          return false;
        }
      xs.push_back (units.convert (original, dimension, val));
      const double pos = ys[i];
      if (!units.can_convert ("cm", pos_dim, pos))
        {
          std::ostringstream tmp;
          tmp << "Can't convert " << pos << " from [cm] to [" << pos_dim << "]";
          msg.error (tmp.str ());
          return false;
        }
      ys[i] = units.convert ("cm", pos_dim, pos);
    }
 
  if (!std::isfinite (plot_z))
    {
      plot_z = 0.0;
      for (size_t i = 0; i < array_size; i++)
        plot_z += (ys[i] - plot_z) * 2.0;
    }
  else
    plot_z = units.convert ("cm", pos_dim, plot_z);

  std::reverse (xs.begin (), xs.end ());
  std::reverse (ys.begin (), ys.end ());

  if (title_ == Attribute::Unknown ())
    {
      std::ostringstream tmp;
      tmp << when->print () << " " << tag;
      title_ = tmp.str ();
    }

  // Done.
  return true;
}

void 
XYSourceProfile::limit (double& xmin, double& xmax,
                     double& ymin, double& ymax) const
{
  std::ostringstream tmp;
  tmp << "1: [" << ymin << ":" << ymax << "]";
  XYSource::limit (xmin, xmax, ymin, ymax);
  tmp << "\n2: [" << ymin << ":" << ymax << "]";
  if (ymin < plot_z)
    ymin = plot_z;
  if (ymax < 0.0)
    ymax = 0.0;
  tmp << "\n3: [" << ymin << ":" << ymax << "]";
  Assertion::message (tmp.str ());
}

XYSourceProfile::XYSourceProfile (const BlockModel& al)
  : XYSource (al),
    when (submodel<Time> (al, "when")),
    plot_z (al.number ("to", NAN)),
    dimension (al.name ("dimension", Attribute::Unknown ())),
    pos_dim (al.name ("pos_dim")),
    lex (al),
    with_ (al.name ("with", "linespoints")),
    style_ (al.integer ("style", -1)),
    title_ (al.name ("title", Attribute::Unknown ()))
{ }

XYSourceProfile::~XYSourceProfile ()
{ }


static struct XYSourceProfileSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceProfile (al); }

  XYSourceProfileSyntax ()
    : DeclareModel (XYSource::component, "profile", 
                    "Plot soil profile.")
  { }
  static bool check_alist (const Metalib&, const Frame& al,
                           Treelog& msg)
  {
    bool ok = true;
    return ok;

  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_submodule ("when", Attribute::Const, "\
Use value closest to this time.", Time::load_syntax);
    frame.declare ("to", "cm", Check::non_positive (),
                   Attribute::OptionalConst, "\
Plot profile to this depth.  By default, plot full profile.");
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
    frame.declare_string ("pos_dim", Attribute::Const, "\
Dimension for soil position.");
    static VCheck::Compatible is_length ("cm");
    frame.set_check ("pos_dim", is_length);
    frame.set ("pos_dim", "cm");

    LexerTable::load_syntax (frame);
    GnuplotUtil::load_style (frame, "\
By default, data will be drawn with linespoints.", "\
By default the specified 'z' value, time, and tag.");
  }
} XYSourceProfile_syntax;

// xysource_profile.C ends here.
