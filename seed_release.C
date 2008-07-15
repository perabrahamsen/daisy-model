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
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "librarian.h"
#include "log.h"
#include "check.h"

struct SeedRelease : public Seed
{
  // Parameters.
  const double C_fraction; // Carbon content in seeds. []
  const double N_fraction; // Nitrogen content in seeds. []
  const double rate;            // Seed carbon release rate. [h^-1]

  // State.
  double C;                     // Unreleased carbon in seeds. [g C/m^2]

  // Simulation.
  void output (Log& log) const;
  
  // Create and Destroy.
  void initialize (double weight);
  bool check (Treelog& msg) const;
  static void load_syntax (Syntax& syntax, AttributeList&);
  SeedRelease (Block& al);
  ~SeedRelease ();
};

void 
SeedRelease::output (Log& log) const
{ 
  output_variable (C, log);
}

void 
SeedRelease::initialize (double weight)
{
  C = weight * C_fraction;
}

bool 
SeedRelease::check (Treelog& msg) const
{
  bool ok = true;
  Treelog::Open nest (msg, library_id () + ": " + name);
  if (C < 0.0)
    {
      ok = false;
      msg.error ("Seed weight must be specified for use with this model");
    }
  return ok;
}

void
SeedRelease::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("C_fraction", Syntax::Fraction (), Syntax::Const, "\
Carbon content in seeds.");
  syntax.add ("N_fraction", Syntax::Fraction (), Syntax::Const, "\
Nitrogen content in seeds.");
  syntax.add ("rate", "h^-1", Check::positive (), Syntax::Const, "\
Release rate of seed carbon to assimilate pool.");
  syntax.add ("C", "g C/m^2", Check::non_negative (), Syntax::OptionalState, "\
Unreleased carbon left in seeds.");
}

SeedRelease::SeedRelease (Block& al)
  : Seed (al),
    C_fraction (al.number ("C_fraction")),
    N_fraction (al.number ("N_fraction")),
    rate (al.number ("rate")),
    C (al.number ("C", -42.42))
{ }

SeedRelease::~SeedRelease ()
{ }

#if 0
const AttributeList& 
Seed::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      SeedRelease::load_syntax (dummy, alist);
      alist.add ("type", "release");
    }
  return alist;
}
#endif

static struct Seed_ReleaseSyntax
{
  static Model& make (Block& al)
  { return *new SeedRelease (al); }
  Seed_ReleaseSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SeedRelease::load_syntax (syntax, alist);
    alist.add ("description", "\
Initial crop growth is governed by carbon released from seeds.");

    Librarian::add_type (Seed::component, "release", alist, syntax, &make);
  }
} SeedRelease_syntax;

// seed_release.C ends here.
