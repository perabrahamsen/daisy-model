// groundwater_static.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "groundwater.h"
#include "block_model.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

class GroundwaterStatic : public Groundwater
{
  // Content.
private:
  const double depth;
  
  // Groundwater.
public:
  bottom_t bottom_type () const;
  double q_bottom (size_t) const
  { daisy_notreached (); }

  // Simulation.
public:
  void tick (const Geometry& geo, const Soil&, SoilWater&, double, 
	     const SoilHeat&, const Time&, const Scope&, Treelog&);
  double table () const;

  // Create and Destroy.
public:
  void initialize (const Geometry&, const Time&, const Scope&, Treelog&)
  { }
  bool check (const Geometry&, const Scope&, Treelog&) const
  { return true; }

  GroundwaterStatic (const BlockModel&);
  ~GroundwaterStatic ();
};

Groundwater::bottom_t 
GroundwaterStatic::bottom_type () const
{
  if (depth > 0)	     // Positive numbers indicate flux bottom.
    return free_drainage;
  else
    return pressure;
}

void
GroundwaterStatic::tick (const Geometry&,
                         const Soil&, SoilWater&, double, const SoilHeat&,
			 const Time&, const Scope&, Treelog&)
{ }

double
GroundwaterStatic::table () const
{
  return depth;
}

GroundwaterStatic::GroundwaterStatic (const BlockModel& al)
  : Groundwater (al),
    depth (al.number ("table"))
{ }

GroundwaterStatic::~GroundwaterStatic ()
{ }

static struct GroundwaterStaticSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { 
    return new GroundwaterStatic (al);
  }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    static bool warned = false;
    if (warned)
      return true;
    else if (al.number ("table") > 0)
      err.entry ("OBSOLETE: Use 'deep' instead 'table' groundwater");
    else
      err.entry ("OBSOLETE: Use 'fixed' instead 'table' groundwater");
    warned = true;
    return true;
  }
  GroundwaterStaticSyntax ()
    : DeclareModel (Groundwater::component, "static", "common", "\
Static groundwater level.\n\
Provided for backward compatibility, use 'deep' or 'fixed' instead.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare ("table", "cm", Attribute::Const,
		"Groundwater level.\n\
Positive numbers indicate free drainage.");
    frame.set ("table", 1.0);
  }
} GroundwaterStatic_syntax;


