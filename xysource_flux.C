// xysource_flux.h -- Plot flux at specific time.
// 
// Copyright 2005 Per Abrahamsen and KVL.
// Copyright 2010 KU.
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
#include "lexer_flux.h"
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

class XYSourceFlux : public XYSource
{ 
  // Flux parameters.
  const boost::scoped_ptr<Time> when;
  const double plot_z;
  const double plot_x;
  symbol dimension;
  const symbol pos_dim;

  // Content.
  LexerFlux lex;
  symbol with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  symbol title_;
  symbol x_dimension_;
  symbol y_dimension_;
  double soil_bottom;
  double soil_right;

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
  void limit (double& xmin, double& xmax, double& ymin, double& ymax) const;

  // Create.
public:
  explicit XYSourceFlux (const BlockModel&);
  ~XYSourceFlux ();
};

static bool approx2 (double a, double b)
{ return approximate (a, b); }

bool
XYSourceFlux::load (const Units& units, Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;
  if (!lex.read_flux (msg))
    return false;
  if (!lex.good ())
    return false;

  // Array.
  symbol tag = lex.flux_tag ();
  if (lex.edge_z ().size () > 0)
    {
      msg.error ("One dimensional data");
      return false;
    }

  const std::vector<int>& flux_from = lex.edge_from ();
  const std::vector<int>& flux_to = lex.edge_to ();
  const size_t array_size = flux_from.size ();
  daisy_assert (flux_to.size () == array_size);
  const std::vector<double>& center_z = lex.cell_z ();
  const std::vector<double>& center_x = lex.cell_x ();
  daisy_assert (center_z.size () == center_x.size ());
  if (array_size < 1)
    {
      msg.error ("No cells");
      return false;
    }
  
  symbol original (lex.flux_dimension ());

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
  double closest = -42.42e42; // [h]
  
  std::vector<double> all_values;
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

      double distance = std::fabs (Time::hours_between (time, *when));

      if (closest < 0.0 || distance < closest)
        if (lex.flux_edges (entries, all_values, msg))
          closest = distance;
    }
  if (closest < 0.0)
    {
      msg.error ("No date found");
      return false;
    }

  daisy_assert (all_values.size () == array_size);

  // Find right and bottom limits.
  std::vector<double> all_z;
  std::vector<double> all_x;

  for (size_t i = 0; i < array_size; i++)
    {
      int from = flux_from[i];
      int to = flux_to[i];

      if (from > 0)
        {
          daisy_assert (center_z.size () > from);
          daisy_assert (center_x.size () > from);
          all_z.push_back (center_z[from]);
          all_x.push_back (center_x[from]);
        }

      if (to > 0)
        {
          daisy_assert (center_z.size () > to);
          daisy_assert (center_x.size () > to);
          all_z.push_back (center_z[to]);
          all_x.push_back (center_x[to]);
        }
    }
  if (all_z.size () < 1 || all_x.size () < 1)
    {
      msg.error ("No internal cells");
      return false;
    }

  std::sort (all_z.begin (), all_z.end ());

  all_z.erase (std::unique (all_z.begin (), all_z.end (), approx2),
               all_z.end ());
  std::reverse (all_z.begin (), all_z.end ());
  double zplus = 0;
  for (size_t i = 0; i < all_z.size (); i++)
    zplus += (all_z[i] - zplus) * 2.0;

  std::sort (all_x.begin (), all_x.end ());
  all_x.erase (std::unique (all_x.begin (), all_x.end (), approx2),
               all_x.end ());
  double xplus = 0;
  for (size_t i = 0; i < all_x.size (); i++)
    xplus += (all_x[i] - xplus) * 2.0;
  
  // Filter and convert
  std::vector<double> value;
  std::vector<double> position;
  for (size_t i = 0; i < array_size; i++)
    {
      double val = all_values[i];
      double pos;
      int from = flux_from[i];
      int to = flux_to[i];

      if (std::isfinite (plot_z))
        {
          daisy_assert (!std::isfinite (plot_x));
          double from_z;
          double to_z;
          
          if (from == Geometry::cell_above)
            from_z = 0.0;
          else if (from == Geometry::cell_below)
            from_z = zplus;
          else if (from < 0)
            continue;
          else
            from_z = center_z[from];

          if (to == Geometry::cell_above)
            to_z = 0.0;
          else if (to == Geometry::cell_below)
            to_z = zplus;
          else if (to < 0)
            continue;
          else
            to_z = center_z[to];

          if ((to_z < plot_z) == (from_z < plot_z))
            continue;

          if (from < 0)
            pos = center_x[to];
          else if (to < 0)
            pos = center_x[from];
          else 
            pos = (center_x[to] + center_x[from]) / 2.0;
        }
      else 
        {
          daisy_assert (std::isfinite (plot_x));
          double from_x;
          double to_x;
          
          if (from == Geometry::cell_left)
            from_x = 0.0;
          else if (from == Geometry::cell_right)
            from_x = xplus;
          else if (from < 0)
            continue;
          else
            from_x = center_x[from];

          if (to == Geometry::cell_left)
            to_x = 0.0;
          else if (to == Geometry::cell_right)
            to_x = xplus;
          else if (to < 0)
            continue;
          else
            to_x = center_x[to]; 

          if ((to_x < plot_x) == (from_x < plot_x))
            continue;

          if (from < 0)
            pos = center_z[to];
          else if (to < 0)
            pos = center_z[from];
          else 
            pos = (center_z[to] + center_z[from]) / 2.0;
       }
      
      // Convert.
      if (!units.can_convert (original, dimension, val))
        {
          std::ostringstream tmp;
          tmp << "Can't convert " << val << " from [" << original 
              << "] to [" << dimension << "]";
          msg.error (tmp.str ());
          return false;
        }
      val = units.convert (original, dimension, val);
      if (!units.can_convert ("cm", pos_dim, pos))
        {
          std::ostringstream tmp;
          tmp << "Can't convert " << pos << " from [cm] to [" << pos_dim << "]";
          msg.error (tmp.str ());
          return false;
        }
      pos = units.convert ("cm", pos_dim, pos);

      // Store.
      value.push_back (val);
      position.push_back (pos);
    }
  
  if (std::isfinite (plot_x))
    {
      ys = position;
      y_dimension_ = pos_dim;
      
      xs = value;
      x_dimension_ = dimension;
      if (title_ == Attribute::Unknown ())
        {
          std::ostringstream tmp;
          tmp << plot_x << " " << when->print () << " " << tag;
          title_ = tmp.str ();
        }
    }
  else
    {
      xs = position;
      x_dimension_ = pos_dim;
      ys = value;
      y_dimension_ = dimension;
      if (title_ == Attribute::Unknown ())
        {
          std::ostringstream tmp;
          tmp << plot_z << " " << when->print () << " " << tag;
          title_ = tmp.str ();
        }
    }

  if (!units.can_convert ("cm", pos_dim, zplus))
    {
      std::ostringstream tmp;
      tmp << "Can't convert " << zplus << " from [cm] to [" << pos_dim << "]";
      msg.error (tmp.str ());
      return false;
    }
  soil_bottom = units.convert ("cm", pos_dim, zplus);
  if (!units.can_convert ("cm", pos_dim, xplus))
    {
      std::ostringstream tmp;
      tmp << "Can't convert " << xplus << " from [cm] to [" << pos_dim << "]";
      msg.error (tmp.str ());
      return false;
    }
  soil_right = units.convert ("cm", pos_dim, xplus);

  // Done.
  return true;
}

void 
XYSourceFlux::limit (double& xmin, double& xmax,
                     double& ymin, double& ymax) const
{
  XYSource::limit (xmin, xmax, ymin, ymax);
  if (std::isfinite (plot_x))
    {
      if (ymin > soil_bottom)
        ymin = soil_bottom;
      if (ymax < 0.0)
        ymax = 0.0;
    }
  if (std::isfinite (plot_z))
    {
      if (xmin > 0.0)
        xmin = 0.0;
      if (xmax < soil_right)
        xmax = soil_right;
    }
}

XYSourceFlux::XYSourceFlux (const BlockModel& al)
  : XYSource (al),
    when (submodel<Time> (al, "when")),
    plot_z (al.number ("z", NAN)),
    plot_x (al.number ("x", NAN)),
    dimension (al.name ("dimension", Attribute::Unknown ())),
    pos_dim (al.name ("pos_dim")),
    lex (al),
    with_ (al.name ("with", "lines")),
    style_ (al.integer ("style", -1)),
    title_ (al.name ("title", Attribute::Unknown ())),
    x_dimension_ ("UNINITIALIZED"),
    y_dimension_ ("UNINITIALIZED"),
    soil_bottom (NAN),
    soil_right (NAN)
{ }

XYSourceFlux::~XYSourceFlux ()
{ }


static struct XYSourceFluxSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceFlux (al); }

  XYSourceFluxSyntax ()
    : DeclareModel (XYSource::component, "flux", 
                    "Read a daisy 2d log file, extract flux through line.")
  { }
  static bool check_alist (const Metalib&, const Frame& al,
                           Treelog& msg)
  {
    bool ok = true;

    if (al.check ("z") == al.check ("x"))
      {
        msg.error ("You must specify exactly one of 'x' and 'z'");
        ok = false;
      }
    return ok;

  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_submodule ("when", Attribute::Const, "\
Use value closest to this time.", Time::load_syntax);
    frame.declare ("z", "cm", Check::non_positive (),
                   Attribute::OptionalConst, "\
Plot flux through this depth.");
    frame.declare ("x", "cm", Check::non_negative (),
                   Attribute::OptionalConst, "\
Plot flux through this position.");
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
    frame.declare_string ("pos_dim", Attribute::Const, "\
Dimension for soil position.");
    static VCheck::Compatible is_length ("cm");
    frame.set_check ("pos_dim", is_length);
    frame.set ("pos_dim", "cm");

    LexerTable::load_syntax (frame);
    GnuplotUtil::load_style (frame, "\
By default, data will be drawn with lines.", "\
By default the specified 'z' or 'x' value.");
  }
} XYSourceFlux_syntax;

// xysource_flux.C ends here.
