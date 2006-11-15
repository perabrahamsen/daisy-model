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
#include "bound.h"
#include "border.h"
#include "mathlib.h"

class VolumeBox : public Volume
{
  std::auto_ptr<Bound> bottom;
  std::auto_ptr<Bound> top;
  std::auto_ptr<Bound> left;
  std::auto_ptr<Bound> right;
  std::auto_ptr<Bound> front;
  std::auto_ptr<Bound> back;

  // Use.
public:
  bool has_bottom () const
  { return bottom->type () != Bound::none; }
  void limit_bottom (const double limit)
  { 
    daisy_assert (!has_bottom ());
    bottom->set_finite (limit);
  }
  bool has_top () const
  { return top->type () != Bound::none; }
  void limit_top (const double limit)
  { 
    daisy_assert (!has_top ());
    top->set_finite (limit);
  }
  static bool bound_check (const Border& border, 
                           const Bound& bound,
                           const double default_value,
                           Treelog& msg)
  {
    if (bound.type () != Bound::finite)
      // Nothing to test.
      return true;
    const double value = bound.value ();
    if (approximate (value, default_value))
      // Already tested.
      return true;

    return border.check_border (value, msg);
  }
  bool check_border (const Border& border, 
                     const double default_upper,
                     const double default_lower,
                     Treelog& msg) const
  { 
    bool ok = true;
    if (!bound_check (border, *top, default_upper, msg))
      ok = false;
    if (!bound_check (border, *bottom, default_lower, msg))
      ok = false;
    return ok; 
  }

  static double bound_default (const Bound& bound, const double value)
  { return (bound.type () == Bound::finite ? bound.value () : value); }

  static double fraction_interval (const double min, const double max,
                                   const Bound& from, const Bound& to)
  { return fraction_within (min, max, 
                            bound_default (from, min - 1.0),
                            bound_default (to, max + 1.0)); }
                            
  double box_fraction (const double zm, const double zp, 
                       const double xm, const double xp,
                       const double ym, const double yp) const
  { return fraction_interval (zm, zp, *bottom, *top)
      * fraction_interval (xm, xp, *left, *right)
      * fraction_interval (ym, yp, *front, *back); }

  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  VolumeBox (Block& al)
    : Volume (al),
      bottom (Librarian<Bound>::build_item (al, "bottom")),
      top (Librarian<Bound>::build_item (al, "top")),
      left (Librarian<Bound>::build_item (al, "left")),
      right (Librarian<Bound>::build_item (al, "right")),
      front (Librarian<Bound>::build_item (al, "front")),
      back (Librarian<Bound>::build_item (al, "back"))
  { }
  ~VolumeBox ()
  { }
};

void 
VolumeBox::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("bottom", Librarian<Bound>::library (), 
              Syntax::Const, Syntax::Singleton,
              "Lower boundary on the z-axis.");
  alist.add ("bottom", Bound::none_model ());
  syntax.add ("top", Librarian<Bound>::library (),
              Syntax::Const, Syntax::Singleton,
              "Upper boundary on the z-axis.");
  alist.add ("top", Bound::none_model ());
  syntax.add ("left", Librarian<Bound>::library (),
              Syntax::Const, Syntax::Singleton,
              "Lower boundary on the x-axis.");
  alist.add ("left", Bound::none_model ());
  syntax.add ("right", Librarian<Bound>::library (),
              Syntax::Const, Syntax::Singleton,
              "Upper boundary on the x-axis.");
  alist.add ("right", Bound::none_model ());
  syntax.add ("front", Librarian<Bound>::library (),
              Syntax::Const, Syntax::Singleton,
              "Lower boundary on the y-axis.");
  alist.add ("front", Bound::none_model ());
  syntax.add ("back", Librarian<Bound>::library (),
              Syntax::Const, Syntax::Singleton,
              "Upper boundary on the y-axis.");
  alist.add ("back", Bound::none_model ());
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
