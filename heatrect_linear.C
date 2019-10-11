// heatrect_linear.C --- Linear interpolation between top and bottom.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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
#include "heatrect.h"
#include "geometry_rect.h"
#include "plf.h"
#include "librarian.h"
#include "frame.h"

// The 'linear' model.

struct HeatrectLinear : public Heatrect
{
  void solve (const GeometryRect& geo,
              const std::vector<double>& q_water,
              const std::vector<double>& S_water,
              const std::vector<double>& S_heat,
              const std::vector<double>& capacity_new,
              const std::vector<double>& conductivity,
              const double T_top,
              const double T_top_new,
              const double T_bottom,
              std::vector<double>& T,
              const double dt, Treelog&) const;
  // Create.
  HeatrectLinear (const BlockModel& al)
    : Heatrect (al)
  { }
  ~HeatrectLinear ()
  { }
};

void
HeatrectLinear::solve (const GeometryRect& geo,
                       const std::vector<double>& /* q_water */,
                       const std::vector<double>& /* S_water */,
                       const std::vector<double>& /* S_heat */,
                       const std::vector<double>& /* capacity_new */,
                       const std::vector<double>& /* conductivity */,
                       const double T_top_old,
                       const double T_top_new,
                       const double T_bottom,
                       std::vector<double>& T,
                       const double /* dt*/ , Treelog&) const
{
  const size_t cell_size = geo.cell_size ();
  const double T_top = (T_top_new + T_top_old) / 2.0;

  // Linear interpolation between bottom and top.
  PLF plf;
  plf.add (geo.bottom (), T_bottom);
  plf.add (geo.top (), T_top);

  for (size_t c = 0; c < cell_size; c++)
    T[c] = plf (geo.cell_z (c));
}

static struct HeatrectLinearSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HeatrectLinear (al); }

  HeatrectLinearSyntax ()
    : DeclareModel (Heatrect::component, "linear", "\
Linear temperature interpolation between top and bottom.")
  { }
  void load_frame (Frame&) const
  { }
} HeatrectLinear_syntax;

// The 'top' model.

struct HeatrectTop : public Heatrect
{
  void solve (const GeometryRect& geo,
              const std::vector<double>& q_water,
              const std::vector<double>& S_water,
              const std::vector<double>& S_heat,
              const std::vector<double>& capacity_new,
              const std::vector<double>& conductivity,
              const double T_top,
              const double T_top_new,
              const double T_bottom,
              std::vector<double>& T,
              const double dt, Treelog&) const;
  // Create.
  HeatrectTop (const BlockModel& al)
    : Heatrect (al)
  { }
  ~HeatrectTop ()
  { }
};

void
HeatrectTop::solve (const GeometryRect& geo,
		    const std::vector<double>& /* q_water */,
		    const std::vector<double>& /* S_water */,
		    const std::vector<double>& /* S_heat */,
		    const std::vector<double>& /* capacity_new */,
		    const std::vector<double>& /* conductivity */,
		    const double T_top_old,
		    const double T_top_new,
		    const double /* T_bottom */,
		    std::vector<double>& T,
		    const double /* dt*/ , Treelog&) const
{
  const size_t cell_size = geo.cell_size ();
  const double T_top = (T_top_new + T_top_old) / 2.0;

  for (size_t c = 0; c < cell_size; c++)
    T[c] = T_top;
}

static struct HeatrectTopSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HeatrectTop (al); }

  HeatrectTopSyntax ()
    : DeclareModel (Heatrect::component, "top", "\
Soil temperature is equal to upper boundary.\n\
This can be used for a poorly isolated soil column.")
  { }
  void load_frame (Frame&) const
  { }
} HeatrectTop_syntax;

// heatrect_linear.C ends here.
