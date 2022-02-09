// seed_release.C -- Initial growth is governed by carbonm release from seeds.
// 
// Copyright 2008 Per Abrahamsen and KU.
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
#include "seed.h"
#include "block_model.h"
#include "librarian.h"
#include "log.h"
#include "check.h"
#include "treelog.h"
#include "frame.h"
#include "plf.h"
#include "mathlib.h"

struct SeedRelease : public Seed
{
  const double initial_weight;  // [g w.w./m^2]

  // Parameters.
  const double DM_fraction;      // Dry matter content in seeds. [g DM/g w.w.]
  const double C_fraction;      // Carbon content in seeds. [g C/g DM]
  const double N_fraction;    // Nitrogen content in seeds. [g N/g DM]
  const PLF T_factor;	      // Soil temp influence on rate []
  const double rate;            // Seed carbon release rate. [h^-1]

  // State.
  double C;                     // Unreleased carbon in seeds. [g C/m^2]

  // Simulation.
  double forced_CAI (const double WLeaf, const double SpLAI, const double DS)
  { return -1.0; }
  double release_C (double T, double dt);
  void output (Log& log) const;
  
  // Create and Destroy.
  double initial_N () const; // [g N/m^2]
  void initialize (double seed_w, Treelog& msg);
  bool check (Treelog& msg) const;
  SeedRelease (const BlockModel& al);
  ~SeedRelease ();
};

double 
SeedRelease::release_C (const double T, const double dt)
{
  const double factor_T
    = (T_factor == PLF::empty ())
    ? std::max (0.0, 
                0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
                          - exp (0.57 - 0.042 * T - 0.0051 * T * T)))
    : T_factor (T);
    
  const double released = std::min (C * factor_T * rate * dt, C);
  C -= released;
  return released;
}

void 
SeedRelease::output (Log& log) const
{ output_variable (C, log); }

double 
SeedRelease::initial_N () const
{ return (C / C_fraction) * N_fraction; }

void 
SeedRelease::initialize (const double seed_w, Treelog&)
{ 
  
  if (C < -1.0)
    {
      if (seed_w > 0.0)
        C = seed_w * DM_fraction * C_fraction; 
      else
        C = initial_weight * DM_fraction * C_fraction; 
    }
}

bool 
SeedRelease::check (Treelog& msg) const
{
  Treelog::Open nest (msg, library_id () + ": " + objid);

  bool ok = true;
  if (C < 0.0)
    {
      ok = false;
      msg.error ("Seed weight must be specified for use with this model");
    }
  return ok;
}

SeedRelease::SeedRelease (const BlockModel& al)
  : Seed (al),
    initial_weight (al.number ("initial_weight", -42.42e42)),
    DM_fraction (al.number ("DM_fraction")),
    C_fraction (al.number ("C_fraction")),
    N_fraction (al.number ("N_fraction")),
    T_factor (al.check ("T_factor") ? al.plf ("T_factor") : PLF::empty ()),
    rate (al.number ("rate")),
    C (al.number ("C", -42.42))
{ }

SeedRelease::~SeedRelease ()
{ }

static struct Seed_ReleaseSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SeedRelease (al); }
  Seed_ReleaseSyntax ()
    : DeclareModel (Seed::component, "release", "\
Initial crop growth is governed by carbon released from seeds.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("initial_weight", "g w.w./m^2",
                Check::positive (), Attribute::OptionalConst, "\
Initial seed weight to use when not specified by the sow operation.\n\
If not specified here, specifying seed amount when sowing is mandatory.");
    frame.declare ("DM_fraction", Attribute::Fraction (), Attribute::Const, "\
Dry matter content in seeds.");
    frame.declare ("C_fraction", Attribute::Fraction (), Attribute::Const, "\
Carbon content in seeds.");
    frame.declare ("N_fraction", Attribute::Fraction (), Attribute::Const, "\
Nitrogen content in seeds.");
    frame.declare ("T_factor", "dg C", Attribute::Fraction (),
		   Attribute::OptionalConst, "\
Soil temperature effect on release rate.\n\
By default use the same as for maintenance respiration:\
0.4281 (exp (0.57 - 0.024 T + 0.0020 T^2) - exp (0.57 - 0.042 T - 0.0051 T^2))\
\n\
This will give a rate of 1 at 20 dg C, slightly above 2 at 30 dg C, and a \
bit below 0.5 at 10 dg C.");
    frame.declare ("rate", "h^-1", Check::positive (), Attribute::Const, "\
Release rate of seed carbon to assimilate pool.");
    frame.declare ("C", "g C/m^2", Check::non_negative (), Attribute::OptionalState, "\
Unreleased carbon left in seeds.");
  }
} SeedRelease_syntax;

// seed_release.C ends here.
