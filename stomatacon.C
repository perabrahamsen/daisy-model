// stomatacon.C  -- Calculating stomata conductance
// 
// Copyright 2008 Birgitte Gjettermann, Per Abrahamsen and KU.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.5
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

#include "stomatacon.h"
#include "mathlib.h"
#include "block_model.h"
#include "librarian.h"
#include "check.h"
#include <sstream>

// The 'stomatacon' component.

const char *const StomataCon::component = "stomatacon";

symbol 
StomataCon::library_id () const
{
  static const symbol id (component);
  return id;
}

StomataCon::StomataCon (const BlockModel& al)
  : ModelLogable (al.type_name ())
{ }

StomataCon::~StomataCon ()
{ }

static struct StomataConInit : public DeclareComponent 
{
  StomataConInit ()
    : DeclareComponent (StomataCon::component, "\
The 'Stomatacon' component calculates the stomata conductance of water vapour.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} StomataCon_init;

// The 'WSF' stomatacon base model.

double
StomataCon_WSF_base::wsf (const double ABA /* [g/cm^3] */,
                          const double h_x /* [MPa] */) const
{
  daisy_assert (ABA >= 0.0);
  daisy_assert (h_x >= 0.0);
  const double wsf 
    = std::exp (-beta * std::max (ABA - ABA_min, 0.0))
    * std::exp (-delta * h_x);
  if  (wsf <= 0.0)
    {
      std::ostringstream tmp;
      tmp << "wsf = " << wsf
          << ", beta = " << beta
          << ", ABA = " << ABA
          << ", ABA_min = " << ABA_min
          << ", ABA effect = "
          << std::exp (-beta * std::max (ABA - ABA_min, 0.0)) // 
          << ", delta = " << delta
          << ", h_x = " << h_x
          << ", Psi effect = " << std::exp (-delta * h_x);
      Assertion::message (tmp.str ());
      return 1.0;
    }
    
  return wsf;
}
                      
StomataCon_WSF_base::StomataCon_WSF_base (const BlockModel& al)
  : StomataCon (al),
    beta (al.number ("beta")),
    ABA_min (al.number ("ABA_min")),
    delta (al.number ("delta"))
{ }

static struct StomataConWSFSyntax : public DeclareBase
{
  StomataConWSFSyntax ()
    : DeclareBase (StomataCon::component, "WSF", "\
Common water stress effect parameters.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("beta", "cm^3/g", Check::non_negative (), Attribute::Const,
                   "Effect of ABA concentration.\n\
The effect is exp (-beta (|ABA| - ABA_min)), where |ABA| is the ABA\n\
concentration in the xylem.");
    frame.set ("beta", 0.0);
    frame.declare ("ABA_min", "g/cm^3", Check::non_negative (),
                   Attribute::Const,
                   "Level of ABA with unstressed production.");
    frame.set ("ABA_min", 0.0);
    frame.declare ("delta", "MPa^-1", Check::non_negative (), Attribute::Const,
                   "Effect of crown water potential.\n\
The effect is exp (-delta |psi_c|), where psi_c is the crown potential.");
    frame.set ("delta", 0.0);
  }
} StomataConWSF_syntax;

// The 'BB_base' stomatacon base model.

struct StomataCon_BB_base : public StomataCon_WSF_base
{
  // Parameters.
  const double m;     // Stomatal slope factor.
  const double b;     // Stomatal intercept.

  // Simulation.
  double minimum () const;

  // Create.
  StomataCon_BB_base (const BlockModel&);
};

double 
StomataCon_BB_base::minimum () const
{ return b; }

StomataCon_BB_base::StomataCon_BB_base (const BlockModel& al)
  : StomataCon_WSF_base (al),
    m (al.number("m")),
    b (al.number("b"))
{ }

static struct StomataConBBbaseSyntax : public DeclareBase
{
  StomataConBBbaseSyntax ()
    : DeclareBase (StomataCon::component, "BB_base", "WSF", "\
Common parameters for Ball&Berry derived models.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("m", Attribute::None (), Check::positive (),
                   Attribute::Const, "\
Stomatal slope factor.\n\
Ball and Berry (1982): m = 9 for soyabean.\n\
Wang and Leuning(1998): m = 11 for wheat");
    frame.declare ("b", "mol/m^2/s", Check::positive (), Attribute::Const, "\
Stomatal intercept.\n                                                   \
Ball and Berry (1982) & Wang and Leuning(1998): (0.01 mol/m2/s)");
  }
} StomataConBBbase_syntax;

// The 'BB' stomatacon model.

struct StomataCon_BB : public StomataCon_BB_base
{
  double stomata_con (const double ABA,  // [g/cm^3]
                      const double h_x,  // [MPa]
                      const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, 
                      const double,  Treelog&);

  void output (Log&) const
  { }

  // Create.
  StomataCon_BB (const BlockModel& al)
    : StomataCon_BB_base (al)
  { }
};

double
StomataCon_BB::stomata_con (const double ABA,  // [g/cm^3]
                            const double h_x,  // [MPa]
                            const double hs /*[]*/, 
                            const double pz /*[mol/m²leaf/s]*/,
                            const double Ptot /*[Pa]*/,
                            const double cs /*[Pa]*/,
                            const double, 
                            const double, Treelog&)
{
  
  if (pz <= 0.0)
    return b;
  daisy_assert (cs > 0.0);
  const double gsw = wsf (ABA, h_x) * (m * hs * pz * Ptot)/cs + b;
  
  daisy_assert (gsw >= 0.0);
  return gsw;
}

static struct StomataConBBSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_BB (al); }
  StomataConBBSyntax ()
    : DeclareModel (StomataCon::component, "BB", "BB_base",
                    "Stomata conductance calculated by the Ball & Berry model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "bb87");
  }
} StomataConBBsyntax;

// The 'Leuning' stomatacon model.

struct StomataCon_Leuning : public StomataCon_BB_base
{
  // Parameters.
  const double Do;

  // Simulation.
  double stomata_con (const double ABA,  // [g/cm^3]
                      const double h_x,  // [MPa]
                      const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  StomataCon_Leuning (const BlockModel& al)
    : StomataCon_BB_base (al),
      Do (al.number ("Do"))
  { }
};

double
StomataCon_Leuning::stomata_con (const double ABA,  // [g/cm^3]
                                 const double h_x,  // [MPa]
                                 const double,
                                 const double pz /*[mol/m²leaf/s]*/,
                                 const double Ptot /*[Pa]*/,
                                 const double cs /*[Pa]*/,
                                 const double Gamma /*[Pa]*/, 
                                 const double Ds /*[Pa]*/, Treelog& msg)
{
  if (pz <= 0.0)
    return b;

  daisy_assert (cs > Gamma);
  daisy_assert (cs > 0.0);
  daisy_assert (Gamma >= 0.0);
  daisy_assert (pz > 0.0);
  daisy_assert (Ds >= 0.0);
  daisy_assert (((cs - Gamma)*(1 + Ds/Do)) > 0.0);

  const double gsw 
    = wsf (ABA, h_x) * (m * pz * Ptot)/((cs - Gamma)*(1 + Ds/Do)) + b;
  daisy_assert (gsw >= 0.0);

  return gsw;
}

static struct StomataConLeuningSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_Leuning (al); }
  StomataConLeuningSyntax ()
    : DeclareModel (StomataCon::component, "Leuning", "BB_base", "\
Stomata conductance calculated by the Leuning model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "Leuning95");
    frame.declare ("Do", "[Pa]", Check::positive (), Attribute::Const,
                   "Empirical coefficient.");
    frame.set ("Do", 1500.);
  }
} StomataConLeuningsyntax;

// StomataCon.C ends here.
