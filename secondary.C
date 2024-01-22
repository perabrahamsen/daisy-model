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
#include "check.h"
#include "water.h"
#include <cmath>
#include <sstream>

// secondary component.

const char *const Secondary::component = "secondary";

symbol
Secondary::library_id () const
{
  static const symbol id (component);
  return id;
}

Secondary::Secondary (const BlockModel& al)
  : objid (al.type_name ())
{ }

Secondary::Secondary (const symbol name_)
  : objid (name_)
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
  double h_lim () const 
  // Pressure act. secondary domain. [cm]
  { return 0.0; }
  double K (const double h) const 
  // Conductivity in sec. dom. [cm/h]
  { return 0.0; }
  double alpha () const         // Exchange rate between domain [h^-1]
  { return 0.0; }
  void initialize (Treelog&)
  { }
  explicit SecondaryNone (const BlockModel& al)
    : Secondary (al)
  {}
  SecondaryNone ()
    : Secondary ("none")
  {}
};

std::unique_ptr<Secondary>
Secondary::create_none ()
{ return std::unique_ptr<Secondary> (new SecondaryNone ());  }

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
  const bool use_secondary;          // Enable secondary domain.

  double h_lim () const
  // The value of the 'h_lim' parameter.
  { return use_secondary ? h_lim_ : 0.0; }

  double K (const double h) const
  { 
    if (h < h_lim_)
      return 0.0;
    else
      return K_; 
  }
  void initialize (Treelog&)
  { }
  SecondaryPressure (const BlockModel& al)
    : SecondaryAlpha (al),
      h_lim_ (al.number ("h_lim")),
      K_ (al.number ("K")),
      use_secondary (al.flag ("use_secondary"))
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
    frame.declare ("h_lim", "cm", Check::negative (), Attribute::Const, "\
Minimal pressure needed for activating secondary domain.");
    frame.declare ("K", "cm/h", Check::non_negative (), Attribute::Const, "\
Water conductivity when secondary domain is active.\n\
If the secondary domain is already included in the normal conductivity\n\
curve, specify 0.0 to use that value instead.");
    frame.set ("K", 0.0);
    frame.declare_boolean ("use_secondary", Attribute::Const, "\
Divide soil matrix into two domains for solute transport.\n\
Set this to false to make h_lim affect only the conductivity curve.");
    frame.set ("use_secondary", true);
  }
} SecondaryPressure_syntax;

// "cracks" model.

struct SecondaryCracks : public SecondaryAlpha
{
  const double aperture;       // [m]
  const double density;         // [m^-1]
  const double K_crack;              // [cm/h]
  const double h_crack;              // [cm]
  const bool use_secondary;          // Enable secondary domain.

  double h_lim () const 
  { return use_secondary ? h_crack : 0.0; }

  double K (const double h) const
  { 
    if (h < h_crack)
      return 0.0;
    else
      return K_crack; 
  }
  void initialize (Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    std::ostringstream tmp;
    tmp << "K = " << K_crack << " [cm/h]\n"
        << "h = " << h_crack << " [cm]";
    msg.debug (tmp.str ());
  }
  static double find_K (const double aperture, const double density)
  { 
    const double a = aperture;  // [m]
    const double B = 1.0 / density;   // [m]
    const double g = 9.82;      // [m s^-2] Gravitational acceleration.
    const double rho_w = Water::density;   // [kg m^-3] 
    const double T = 20.0;                 // [dg C]
    const double eta_w = Water::viscosity (T); // [N s m^-2]

    // Poiseuille's law.
    const double K_f = (rho_w * g     *    a     * a * a)
                     / (12.0 * eta_w  *    B);
    //   ([kg m^-3] * [m ^ s^-2] * [m] * [m] * [m])
    // / ([N s m^-2] * [m])
    // = [kg m^2 N^-1 s^-3]
    // = [kg m^2 s^-3 s^2 kg^-1 m^-1] 
    // = [m/s]
    
    // Convert to Daisy units.
    return K_f /* [m/s] */ * 100.0 /* [cm/m] */ * 3600.0 /* [s/h] */; // [cm/h]
  }

  static double find_h_lim (const double aperture)
  {
    const double a = aperture;  // [m]
    const double g = 9.82;      // [m s^-2] Gravitational acceleration.
    const double rho_w = Water::density;   // [kg m^-3] 
    const double gamma_w = 7.28e-2;        // [N m^-1] Surface tension.
    const double alpha = 0.0;   // [rad] Water angle with surface.
    
    const double h_c = (   2.0 * gamma_w * std::cos (alpha)
                        / (rho_w * g * a)); // [Pa]
    //   [N m^-1] [kg^-1 m^3] [m^-1 s^2] [m^-1]
    // = [N kg^-1 s^2]
    // = [kg m s^-2 kg^-1 s^2]
    // = [m]

    return h_c * -100.0;        // [cm H2O/m]
  }

  SecondaryCracks (const BlockModel& al)
    : SecondaryAlpha (al),
      aperture (al.number ("aperture")),
      density (al.number ("density")),
      K_crack (find_K (aperture, density)),
      h_crack (find_h_lim (aperture)),
      use_secondary (al.flag ("use_secondary"))
  { }
};

static struct SecondaryCracksSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SecondaryCracks (al); }
  SecondaryCracksSyntax ()
    : DeclareModel (Secondary::component, "cracks", "alpha", "\
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
    frame.declare_boolean ("use_secondary", Attribute::Const, "\
Divide soil matrix into two domains for solute transport.\n\
Set this to false to make cracks affect only the conductivity curve.");
    frame.set ("use_secondary", true);
  }
} SecondaryCracks_syntax;

// secondary.C ends here.
