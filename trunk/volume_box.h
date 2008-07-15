// volume_box.h - A volume defined by intervals on each axis.
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

#ifndef VOLUME_BOX_H
#define VOLUME_BOX_H

#include "volume.h"

class Syntax;
class AttributeList;
class Bound;
class Border;

class VolumeBox : public Volume
{
private:
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

  std::string one_line_description () const;

  // Use.
public:
  double volume () const;
private:
  double height (double low, double high) const;
  double width (double low, double high) const;
  double depth (double low, double high) const;
  void limit_top (const double limit);
  void limit_bottom (const double limit);
  bool limit (const Volume& other, Treelog& msg);
  bool check_border (const Border& border, Treelog& msg) const;
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const;
  static double bound_default (const Bound& bound, const double value);
  static double fraction_interval (const double min, const double max,
                                   const Bound& from, const Bound& to);
  double box_fraction (const double zm, const double zp, 
                       const double xm, const double xp,
                       const double ym, const double yp) const;
  static bool in_interval (const double point, 
                           const Bound& from, const Bound& to);
  bool contain_point (double z, double x, double y) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  VolumeBox (Block& al);
  VolumeBox (const char *const id);
  VolumeBox (const char *const id, 
             const double zm, const double zp, 
             const double xm = 0.0, const double xp = 1.0,
             const double ym = 0.0, const double yp = 1.0);
  ~VolumeBox ();
};

#endif // VOLUME_BOX_H
