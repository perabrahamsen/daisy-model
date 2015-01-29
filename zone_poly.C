// zone_box.C - A zone defined by intervals on each axis.
// 
// Copyright 2006, 2012 Per Abrahamsen and KVL.
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
#include "zone.h"
#include "librarian.h"
#include "block_model.h"
#include "frame_submodel.h"
#include "vcheck.h"
#include "assertion.h"
#include "point.h"

#include <boost/geometry/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>

typedef boost::geometry::model::d2::point_xy<double> point_2d; 
typedef boost::geometry::model::polygon<point_2d> polygon_2d; 

struct ZonePoly : public Zone
{
  // Content.
  const polygon_2d polygon;
  const double my_top;
  const double my_bottom;

  // Use.
  bool contain_point (double z, double x, double y) const;
  bool overlap_interval (const double from, const double to) const;
  double center_z () const
  { return 0.5 * (my_top + my_bottom); }

  // Create and Destroy.
  static polygon_2d make_polygon 
  /**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&);
  static double find_top 
  /**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&);
  static double find_bottom
  /**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&);
  ZonePoly (const BlockModel& al);
  ~ZonePoly ();
};

bool 
ZonePoly::contain_point (double z, double x, double) const
{ 
  const point_2d p (x, z);
  return boost::geometry::within (p, polygon);
}

bool 
ZonePoly::overlap_interval (const double from, const double to) const
{
  if (to >= my_top)
    // Entire zone is below interval.
    return false;
  if (from <= my_bottom)
    // Entire zone is above interval.
    return false;

  return true;
}

polygon_2d 
ZonePoly::make_polygon (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& seq)
{
  polygon_2d polygon;

  for (size_t i = 0; i < seq.size (); i++)
    {
      const Frame& frame = *seq[i];
      const double z = frame.number ("z");
      const double x = frame.number ("x");
      boost::geometry::append (polygon, point_2d (x, z));
    }
  boost::geometry::correct (polygon);
  return polygon;
}

double
ZonePoly::find_top (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& seq)
{
  double my_top = NAN;
  for (size_t i = 0; i < seq.size (); i++)
    {
      const Frame& frame = *seq[i];
      const double z = frame.number ("z");
      if (i == 0 || z > my_top)
        my_top = z;
    }      
  return my_top;
}

double
ZonePoly::find_bottom (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& seq)
{
  double my_bottom = NAN;
  for (size_t i = 0; i < seq.size (); i++)
    {
      const Frame& frame = *seq[i];
      const double z = frame.number ("z");
      if (i == 0 || z < my_bottom)
        my_bottom = z;
    }      
  return my_bottom;
}

ZonePoly::ZonePoly (const BlockModel& al)
  : Zone (al),
    polygon (make_polygon (al.submodel_sequence ("outer"))),
    my_top (find_top (al.submodel_sequence ("outer"))),
    my_bottom (find_bottom (al.submodel_sequence ("outer")))
{ }
  
ZonePoly::~ZonePoly ()
{ }

static struct Zone_PolySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ZonePoly (al); }
  Zone_PolySyntax ()
    : DeclareModel (Zone::component, "polygon", "\
A 2D region defined by points connected by lines.\n\
The object is assumed to be infinite in the third dimension.")
  { }
  static bool check_alist (const Metalib& metalib, const Frame& al, Treelog& msg)
  {
    bool ok = true;

    if (al.check ("outer"))
      {
        const polygon_2d polygon 
          = ZonePoly::make_polygon (al.submodel_sequence ("outer"));
        const double area = boost::geometry::area (polygon);
        daisy_assert (std::isfinite (area));
        daisy_assert (area >= 0.0);
        if (!std::isnormal (area))
          {
            std::ostringstream tmp;
            tmp << "polygon " << boost::geometry::dsv (polygon)
                << " has area of " << area;
            msg.warning (tmp.str ());
          }
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_submodule_sequence ("outer", Attribute::Const, "\
Points surrounding the polygon in clockwise order.",
                                      ZXPoint::load_syntax);
    static VCheck::MinSize min_size_3 (3);
    frame.set_check ("outer", min_size_3);
  }
} ZonePoly_syntax;

// zone_poly.C ends here.
