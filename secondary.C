// secondary.C --- Secondary domain for solute transport.
// 
// Copyright 2008 Per Abrahamsen, Mikkel Mollerup and KU.
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

#include "secondary.h"
#include "block_model.h"
#include "librarian.h"
#include "assertion.h"
#include "frame.h"
#include "soil.h"
#include "check.h"

// secondary component.

const char *const Secondary::component = "secondary";

symbol
Secondary::library_id () const
{
  static const symbol id (component);
  return id;
}

Secondary::Secondary (const BlockModel& al)
  : name (al.type_name ())
{ }

Secondary::Secondary (const symbol name_)
  : name (name_)
{ }

Secondary::~Secondary ()
{ }

static struct SecondaryInit : public DeclareComponent 
{
  SecondaryInit ()
    : DeclareComponent (Secondary::component, "\
Specify secondary domain.\n\
\n\
The secondary domain consist typically of soil fractures or other\n\
inter-aggregate pores small enough to be dominated by capillarity, yet\n\
so large that water moves fast enough that the solute equilibrium with\n\
the primary domain (typically intra-aggregate pores) can not be maintained.\n\
\n\
This allows a pulse of water to be move through saturated or near\n\
saturated soil without solutes in the new water being mixed with\n\
solutes in the old water.  The effects are twofold: It allows solutes\n\
applied to the surface to reach deeper soil layers much faster than it\n\
would otherwise, and it protects solutes in the soil matrix from being\n\
washed out with fast moving new water.")
  { }
} Secondary_init;

// "none" model.

struct SecondaryNone : public Secondary
{
  double h_lim (const size_t, const Soil&) const 
  // Pressure act. secondary domain. [cm]
  { return 0.0; }
  double K (const size_t, const Soil&, const double h) const 
  // Conductivity in sec. dom. [cm/h]
  { return 0.0; }
  double alpha () const         // Exchange rate between domain [h^-1]
  { return 0.0; }

  explicit SecondaryNone (const BlockModel& al)
    : Secondary (al)
  {}
  SecondaryNone ()
    : Secondary ("none")
  {}
};

std::auto_ptr<Secondary>
Secondary::create_none ()
{ return std::auto_ptr<Secondary> (new SecondaryNone ());  }

static struct SecondaryNoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SecondaryNone (al); }
  SecondaryNoneSyntax ()
    : DeclareModel (Secondary::component, "none", "No secondary domain.\n\
\n\
There is always full equilibrium between solute in different size\n\
matrix pores.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} SecondaryNone_syntax;

// "alpha" model.

struct SecondaryAlpha : public Secondary
{
  const double alpha_;

  double alpha () const         // The value of the 'alpha' parameter.
  { return alpha_; }
  
  SecondaryAlpha (const BlockModel& al)
    : Secondary (al),
      alpha_ (al.number ("alpha"))    
  {}
};

static struct SecondaryAlphaSyntax : public DeclareBase
{
  SecondaryAlphaSyntax ()
    : DeclareBase (Secondary::component, "alpha", 
                   "Shared base class for non-empty secondary domains.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("alpha", "h^-1", Attribute::Const, "\
Exchange rate between primary and secondary water."); 
  }
} SecondaryAlpha_syntax;

// "pressure" model.

struct SecondaryPressure : public SecondaryAlpha
{
  const double h_lim_;
  const double K_;

  double h_lim (const size_t, const Soil&) const
  // The value of the 'h_lim' parameter.
  { return h_lim_; }
  double K (const size_t cell, const Soil& soil, const double h) const
  { 
    if (h < h_lim (cell, soil))
      return 0.0;
    else
      return K_; 
  }
  SecondaryPressure (const BlockModel& al)
    : SecondaryAlpha (al),
      h_lim_ (al.number ("h_lim")),
      K_ (al.number ("K"))
  {}
};

static struct SecondaryPressureSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SecondaryPressure (al); }
  SecondaryPressureSyntax ()
    : DeclareModel (Secondary::component, "pressure", "alpha", "\
Horizon has secondary domain specifyed by pressure thresshold.\n\
\n\
The secondary domain consist of water in matrix pores larger than\n\
what corresponds to the specified pressure. ")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("h_lim", "cm", Check::positive (), Attribute::Const, "\
Minimal pressure needed for activating secondary domain.");
    frame.declare ("K", "cm/h", Check::non_negative (), Attribute::Const, "\
Water conductivity when secondary domain is active.\n\
If the secondary domain is already included in the normal conductivity\n\
curve, specify 0.0 use that value instead.");
    frame.set ("K", 0.0);
  }
} SecondaryPressure_syntax;

// "cracks" model.

struct SecondaryCracks : public SecondaryAlpha
{
  const double aperture;       // [m]
  const double density;         // [m^-1]
  const double Theta_crack;     // []
  const double K_crack;              // [cm/h]

  double h_lim (const size_t cell, const Soil& soil) const 
  { 
    const double Theta_sat = soil.Theta (cell, 0.0, 0.0);
    if (Theta_crack >= Theta_sat)
      throw "Space occupied by cracks larger than soil porosity";
    const double Theta_lim = Theta_sat - Theta_crack;
    daisy_assert (Theta_lim > 0.0);
    return soil.h (cell, Theta_lim); 
  }
  double K (const size_t cell, const Soil& soil, const double h) const
  { 
    if (h < h_lim (cell, soil))
      return 0.0;
    else
      return K_crack; 
  }

  static double find_K (const double aperture, const double density)
  { return -42.42e42; }
  SecondaryCracks (const BlockModel& al)
    : SecondaryAlpha (al),
      aperture (al.number ("h_aperture")),
      density (al.number ("density")),
      Theta_crack (aperture * density),
      K_crack (find_K (aperture, density))    
  { 
    daisy_assert (Theta_crack >= 0.0);
    daisy_assert (Theta_crack <= 1.0);
  }
};

static struct SecondaryCracksSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SecondaryCracks (al); }
  SecondaryCracksSyntax ()
    : DeclareModel (Secondary::component, "aperture", "alpha", "\
Secondary domain specified by aperture and density of soil cracks.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  { 
    bool ok = true;

    const double Theta = al.number ("aperture") * al.number ("density");
    if (Theta >= 1.0)
      {
        ok = false;
        msg.error ("Volume fraction of cracks exceed 1.0");
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare ("aperture", "m", Check::positive (), Attribute::Const, "\
Average distance between walls in cracks.");
    frame.declare ("density", "m^-1", Check::positive (), Attribute::Const, "\
Density of cracks.");
  }
} SecondaryCracks_syntax;

// secondary.C ends here.
