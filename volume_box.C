// volume_box.C - A volume defined by intervals on each axis.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "volume.h"
#include "border.h"
#include "mathlib.h"

class VolumeBox : public Volume
{
  bool has_lower_bound;
  double lower_bound;
  bool has_upper_bound;
  double upper_bound;
  bool has_left_bound;
  double left_bound;
  bool has_right_bound;
  double right_bound;
  bool has_near_bound;
  double near_bound;
  bool has_far_bound;
  double far_bound;

  // Use.
public:
  bool has_bottom () const
  { return has_lower_bound; }
  void limit_bottom (const double limit)
  { 
    daisy_assert (!has_lower_bound);
    has_lower_bound = true;
    lower_bound = limit;
  }
  bool has_top () const
  { return has_upper_bound; }
  void limit_top (const double limit)
  { 
    daisy_assert (!has_upper_bound);
    has_upper_bound = true;
    upper_bound = limit;
  }
  bool check_border (const Border& border, 
                     const double default_upper,
                     const double default_lower,
                     Treelog& msg) const
  { 
    bool ok = true;
    if (has_upper_bound 
        && !approximate (upper_bound, default_upper)
        && !border.check_border (upper_bound, msg))
      ok = false;
    if (has_lower_bound 
        && !approximate (lower_bound, default_lower)
        && !border.check_border (lower_bound, msg))
      ok = false;
    return ok; 
  }
  double box_fraction (const double zm, const double zp, 
                       const double xm, const double xp,
                       const double ym, const double yp) const
  {
    return fraction_within (zm, zp, 
                            (has_lower_bound ? lower_bound : zm - 1.0),
                            (has_upper_bound ? upper_bound : zp + 1.0))
      * fraction_within (xm, xp, 
                         (has_left_bound ? left_bound : xm - 1.0),
                         (has_right_bound ? right_bound : xp + 1.0))
      * fraction_within (ym, yp, 
                         (has_near_bound ? near_bound : ym - 1.0),
                         (has_far_bound ? far_bound : yp + 1.0)); }

  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  VolumeBox (Block& al)
    : Volume (al),
      has_lower_bound (al.check ("lower")),
      lower_bound (al.number ("lower", -42.42e42)),
      has_upper_bound (al.check ("upper")),
      upper_bound (al.number ("upper", -42.42e42)),
      has_left_bound (al.check ("left")),
      left_bound (al.number ("left", -42.42e42)),
      has_right_bound (al.check ("right")),
      right_bound (al.number ("right", -42.42e42)),
      has_near_bound (al.check ("near")),
      near_bound (al.number ("near", -42.42e42)),
      has_far_bound (al.check ("far")),
      far_bound (al.number ("far", -42.42e42))
  { }
  ~VolumeBox ()
  { }
};

void 
VolumeBox::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("lower", "cm", Syntax::OptionalConst,
              "Lower boundary on the z-axis.  By default it is unbounded.");
  syntax.add ("upper", "cm", Syntax::OptionalConst,
              "Upper boundary on the z-axis.  By default it is unbounded.");
  syntax.add ("left", "cm", Syntax::OptionalConst,
              "Lower boundary on the x-axis.  By default it is unbounded.");
  syntax.add ("right", "cm", Syntax::OptionalConst,
              "Upper boundary on the x-axis.  By default it is unbounded.");
  syntax.add ("near", "cm", Syntax::OptionalConst,
              "Lower boundary on the y-axis.  By default it is unbounded.");
  syntax.add ("far", "cm", Syntax::OptionalConst,
              "Upper boundary on the y-axis.  By default it is unbounded.");
}

static struct Volume_BoxSyntax
{
  static Volume& make (Block& al)
  { return *new VolumeBox (al); }
  Volume_BoxSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    VolumeBox::load_syntax (syntax, alist);

    alist.add ("description", "A volume defined by intervals on each axis.\n\
By default, the intervals fill the entire axis.  You can modify this by\n\
setting the parameters.");

    Librarian<Volume>::add_type ("box", alist, syntax, &make);
  }
} VolumeBox_syntax;

const AttributeList& 
Volume::infinite_box ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      VolumeBox::load_syntax (dummy, alist);
      alist.add ("type", "box");
    }
  return alist;
}

// volume_box.C ends here.

#if 0
struct ext_number
{
  // Content.
  enum type {
    minus_infinite,
    finite,
    plus_infinite } state;
  double value;
  
  // Create and Destroy.
  ext_number (double v)
    : state (finite),
      value (v)
  { }
  ext_number (type s)
    : state (s),
      value (-42.42e42)
  { daisy_assert (state != finite); }
  ext_number (const ext_number& other)
    : state (other.state),
      value (other.value)
  { }
};

struct ext_interval
{
  // Content.
  ext_number from;
  ext_number to;

  // Create and Destroy;
  ext_interval (const ext_number& f, const ext_number& t)
    : from (f),
      to (t)
  { daisy_assert (f < t); }
  ext_interval (const ext_interval& other)
    : from (other.from),
      to (other.to)
  { }
};

struct Box
{
  // Content.
  ext_interval x;
  ext_interval y;
  ext_interval z;

  // Create and Destroy.
  Box (const ext_interval& x_, const ext_interval& y _, const ext_interval& z_)
    : x (x_),
      y (y_),
      z (z_)
  { }
};

#endif
