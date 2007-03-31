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
#include "syntax.h"
#include "alist.h"
#include "bound.h"
#include "border.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

struct VolumeBox : public Volume
{
  std::auto_ptr<Bound> bottom;
  std::auto_ptr<Bound> top;
  std::auto_ptr<Bound> left;
  std::auto_ptr<Bound> right;
  std::auto_ptr<Bound> front;
  std::auto_ptr<Bound> back;

  struct bounds_t
  { 
    std::auto_ptr<Bound> VolumeBox::* bound;
    bool (Border::*check_border) (double, Treelog&) const;
  };
  static const bounds_t bounds[];
  static const size_t bounds_size;

  std::string one_line_description () const 
  {
    std::ostringstream tmp;
    tmp << "box ";

    bool all_none = true;
    bool all_full = true;
    bool has_finite = false;

    for (size_t i = 0; i < bounds_size; i++)
      {
        Bound& bound = *(this->*(bounds[i].bound));
        switch (bound.type ())
          {
          case Bound::none:
            all_full = false;
            break;
          case Bound::full:
            all_none = false;
            break;
          case Bound::finite:
            all_none = false;
            all_full = false;
            has_finite = true;
            break;
          }
      }
    if (all_full)
      tmp << "full";
    else if (all_none)
      tmp << "none";
    else if (!has_finite)
      tmp << "mixed";
    else
      {
        tmp << "[" << bottom->describe () << "; " << top->describe () << "]";
        const bool has_x = (left->type () == Bound::finite 
                            || right->type () == Bound::finite);
        const bool has_y = (front->type () == Bound::finite 
                            || back->type () == Bound::finite);
        if (has_x || has_y)
          tmp << ", [" << left->describe ()
              << "; " << right->describe () << "]";
        if (has_y)
          tmp << ", [" << front->describe () 
              << "; " << back->describe () << "]";
      }
    return tmp.str ();
  }

  // Use.
  double height (double low, double high) const
  {
    if (bottom->type () == Bound::finite)
      low = bottom->value ();
    if (top->type () == Bound::finite)
      high = top->value ();
    daisy_assert (high > low);
    return high - low;
  }
  double width (double low, double high) const
  {
    if (left->type () == Bound::finite)
      low = left->value ();
    if (right->type () == Bound::finite)
      high = right->value ();
    daisy_assert (high > low);
    return high - low;
  }
  double depth (double low, double high) const
  {
    if (front->type () == Bound::finite)
      low = front->value ();
    if (back->type () == Bound::finite)
      high = back->value ();
    daisy_assert (high > low);
    return high - low;
  }

  void limit_top (const double limit)
  { top->set_finite (limit); }
  void limit_bottom (const double limit)
  { bottom->set_finite (limit); }
  bool limit (const Volume& other, Treelog& msg)
  { 
    if (const VolumeBox* limit = dynamic_cast<const VolumeBox*> (&other))
      {
        for (size_t i = 0; i < bounds_size; i++)
          {
            Bound& bound = *(this->*(bounds[i].bound));
            if (bound.type () == Bound::none)
              {
                const Bound& lim = *(limit->*(bounds[i].bound));
                switch (lim.type ())
                  {
                  case Bound::none:
                    /* do nothing */;
                  break;
                  case Bound::full:
                    bound.set_full ();
                    break;
                  case Bound::finite:
                    bound.set_finite (lim.value ());
                    break;
                  }
              }
          }
        return true;
      }
    msg.error ("Don't know how to limit a '" + name 
               + "' to a '" + other.name + "'");
    return false;
  }
  bool check_border (const Border& border, 
                     Treelog& msg) const
  { 
    bool ok = true;

    for (size_t i = 0; i < bounds_size; i++)
      {
        const Bound& bound = *(this->*(bounds[i].bound));
        if (!bound.type () != Bound::finite)
          continue;
        if (!((border.*(bounds[i].check_border)) (bound.value (), msg)))
          ok = false;
      }
    return ok;
  }
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const
  { 
    if (const VolumeBox* box_volume
        = dynamic_cast<const VolumeBox*> (&default_volume))
      {
        bool ok = true;

        for (size_t i = 0; i < bounds_size; i++)
          {
            const Bound& bound = *(this->*(bounds[i].bound));
            if (bound.type () != Bound::finite)
              continue;
            const Bound& other = *(box_volume->*(bounds[i].bound));
            if (other.type () != Bound::finite)
              continue;
            if (approximate (bound.value (), other.value ()))
              continue;
            if (!((border.*(bounds[i].check_border)) (bound.value (), msg)))
              ok = false;
          }
        return ok;
      }
    return check_border (border, msg);
  }

  static double bound_default (const Bound& bound, const double value)
  { return (bound.type () == Bound::finite ? bound.value () : value); }

  static double fraction_interval (const double min, const double max,
                                   const Bound& from, const Bound& to)
  { 
    const double begin = bound_default (from, min - 1.0);
    const double end = bound_default (to, max + 1.0);
    if (begin >= end)
      return 0.0;
    return fraction_within (min, max, begin, end); 
  }
                            
  double box_fraction (const double zm, const double zp, 
                       const double xm, const double xp,
                       const double ym, const double yp) const
  { return fraction_interval (zm, zp, *bottom, *top)
      * fraction_interval (xm, xp, *left, *right)
      * fraction_interval (ym, yp, *front, *back); }
  static bool in_interval (const double point,
                           const Bound& from, const Bound& to)
  {
    if (from.type () == Bound::finite
        && point <= from.value ())
      // Point before interval.
      return false;
    if (to.type () == Bound::finite
        && point > to.value ())
      // Point after interval.
      return false;

    // Point in interval.
    return true;
  }
  bool contain_point (double z, double x, double y) const
  { return in_interval (z, *bottom, *top)
      && in_interval (x, *left, *right)
      && in_interval (y, *front, *back);
  }

  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  VolumeBox (Block& al)
    : Volume (al),
      bottom (Librarian::build_item<Bound> (al, "bottom")),
      top (Librarian::build_item<Bound> (al, "top")),
      left (Librarian::build_item<Bound> (al, "left")),
      right (Librarian::build_item<Bound> (al, "right")),
      front (Librarian::build_item<Bound> (al, "front")),
      back (Librarian::build_item<Bound> (al, "back"))
  { }
  VolumeBox (const char *const id)
    : Volume (id),
      bottom (new Bound ("none", Bound::none, -42.42e42)),
      top (new Bound ("none", Bound::none, -42.42e42)),
      left (new Bound ("none", Bound::none, -42.42e42)),
      right (new Bound ("none", Bound::none, -42.42e42)),
      front (new Bound ("none", Bound::none, -42.42e42)),
      back (new Bound ("none", Bound::none, -42.42e42))
  { }
  ~VolumeBox ()
  { }
};

std::auto_ptr<Volume> 
Volume::build_none ()
{ return std::auto_ptr<Volume> (new VolumeBox ("none")); }

const VolumeBox::bounds_t 
VolumeBox::bounds[] = {
  { &VolumeBox::top, &Border::check_z_border },
  { &VolumeBox::bottom, &Border::check_z_border },
  { &VolumeBox::left, &Border::check_x_border },
  { &VolumeBox::right, &Border::check_x_border },
  { &VolumeBox::front, &Border::check_y_border },
  { &VolumeBox::back, &Border::check_y_border },
};

const size_t 
VolumeBox::bounds_size = sizeof (VolumeBox::bounds) 
  / sizeof (VolumeBox::bounds_t);

void 
VolumeBox::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_object ("bottom", Bound::component, 
                     Syntax::Const, Syntax::Singleton,
                     "Lower boundary on the z-axis.");
  alist.add ("bottom", Bound::none_model ());
  syntax.add_object ("top", Bound::component,
                     Syntax::Const, Syntax::Singleton,
                     "Upper boundary on the z-axis.");
  alist.add ("top", Bound::none_model ());
  syntax.add_object ("left", Bound::component,
                     Syntax::Const, Syntax::Singleton,
                     "Lower boundary on the x-axis.");
  alist.add ("left", Bound::none_model ());
  syntax.add_object ("right", Bound::component,
                     Syntax::Const, Syntax::Singleton,
                     "Upper boundary on the x-axis.");
  alist.add ("right", Bound::none_model ());
  syntax.add_object ("front", Bound::component,
                     Syntax::Const, Syntax::Singleton,
                     "Lower boundary on the y-axis.");
  alist.add ("front", Bound::none_model ());
  syntax.add_object ("back", Bound::component,
                     Syntax::Const, Syntax::Singleton,
                     "Upper boundary on the y-axis.");
  alist.add ("back", Bound::none_model ());
}

static struct Volume_BoxSyntax
{
  static Model& make (Block& al)
  { return *new VolumeBox (al); }
  Volume_BoxSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    VolumeBox::load_syntax (syntax, alist);

    alist.add ("description", "A volume defined by intervals on each axis.\n\
By default, the intervals fill the entire axis.  You can modify this by\n\
setting the parameters.");

    Librarian::add_type (Volume::component, "box", alist, syntax, &make);
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
