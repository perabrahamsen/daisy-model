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
#include "syntax.h"
#include "alist.h"
#include "librarian.h"

struct HeatrectLinear : public Heatrect
{
  void solve (const GeometryRect& geo,
              const std::vector<double>& q_water,
              const std::vector<double>& S_water,
              const std::vector<double>& S_heat,
              const std::vector<double>& capacity_old,
              const std::vector<double>& capacity_new,
              const std::vector<double>& conductivity,
              const double T_top,
              const double T_top_new,
              const double T_bottom,
              std::vector<double>& T,
              const double dt, Treelog&) const;
  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  HeatrectLinear (Block& al)
    : Heatrect (al)
  { }
  ~HeatrectLinear ()
  { }
};

void
HeatrectLinear::solve (const GeometryRect& geo,
                       const std::vector<double>& q_water,
                       const std::vector<double>& S_water,
                       const std::vector<double>& S_heat,
                       const std::vector<double>& capacity_old,
                       const std::vector<double>& capacity_new,
                       const std::vector<double>& conductivity,
                       const double T_top_old,                   
                       const double T_top_new,
                       const double T_bottom,
                       std::vector<double>& T,
                       const double dt, Treelog& msg) const
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

void 
HeatrectLinear::load_syntax (Syntax&, AttributeList&)
{ }

static struct HeatrectLinearSyntax
{
  static Model& make (Block& al)
  { return *new HeatrectLinear (al); }

  HeatrectLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Linear temperature interpolation between top and bottom.");
    HeatrectLinear::load_syntax (syntax, alist);
 
    Librarian::add_type (Heatrect::component, "linear", alist, syntax, &make);
  }
} HeatrectLinear_syntax;

// heatrect_linear.C ends here.
