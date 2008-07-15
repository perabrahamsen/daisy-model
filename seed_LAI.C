// seed_LAI.C -- Initial growth is governed by a forced LAI function.
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

struct SeedLAI : public Seed
{
  // Simulation.
  void output (Log&) const
  { }
  
  // Create and Destroy.
  bool check (Treelog&) const
  { return true; }
  void initialize (double /* weight */)
  { }
  SeedLAI (Block& al)
    : Seed (al)
  { }
  ~SeedLAI ()
  { }
};


const AttributeList& 
Seed::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
#if 0
      Syntax dummy;
      SeedRelease::load_syntax (dummy, alist);
#endif
      alist.add ("type", "LAI");
    }
  return alist;
}

static struct Seed_LAISyntax
{
  static Model& make (Block& al)
  { return *new SeedLAI (al); }
  Seed_LAISyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "\
Initial crop growth is governed by a forced LAI function.");

    Librarian::add_type (Seed::component, "LAI", alist, syntax, &make);
  }
} SeedLAI_syntax;

// seed_LAI.C ends here.
