// volume.h -- A volume specify a connected subset of 3D space.
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
//
// A volume specify a connected subset of 3D space.

#ifndef VOLUME_H
#define VOLUME_H

#include "librarian.h"
#include "bound.h" // Needed for static initialization order.

class Border;
class Treelog;

class Volume : public Model 
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;
  virtual std::string one_line_description () const = 0;

  // Use.
private:
  virtual void limit_top (double) = 0;
  virtual void limit_bottom (double) = 0;
public:
  virtual double height (const double low, const double high) const = 0;
  virtual double width (const double low, const double high) const = 0;
  virtual double depth (const double low, const double high) const = 0;
  virtual bool limit (const Volume&, Treelog& msg) = 0;
  virtual bool check_border (const Border& border, 
                             Treelog& msg) const = 0;
  virtual bool check_border (const Border& border, 
                             const Volume& default_volume,
                             Treelog& msg) const = 0;
  virtual double box_fraction (double zm, double zp, 
                               double xm = 0.0, double xp = 1.0,
                               double ym = 0.0, double yp = 1.0) const = 0;
  virtual bool contain_point (double z, double x, double y) const = 0;

  // Create and Destroy.
public:
  static Volume* build_obsolete (Block&);
  static const AttributeList& infinite_box ();
  Volume (Block&);
  ~Volume ();
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<Volume>::content;
#endif

static Librarian<Volume> Volume_init;

#endif // VOLUME_H
