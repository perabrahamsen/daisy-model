// mobsol.C --- Specify mobile/immonile solute parameters.
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

#include "mobsol.h"
#include "block.h"
#include "alist.h"
#include "librarian.h"
#include "assertion.h"

// mobsol component.

const char *const Mobsol::component = "mobsol";

Mobsol::Mobsol (Block& al)
  : name (al.identifier ("type"))
{ }

Mobsol::~Mobsol ()
{ }

static Librarian Mobsol_init (Mobsol::component, "\
Specify one end of an interval mobsolary.");

// "full" model.

struct MobsolFull : public Mobsol
{
  bool full () const            // True iff all water is mobile.
  { return true; }
  double h_lim () const         // The value of the 'h_lim' parameter.
  { daisy_notreached (); }
  double alpha () const         // The value of the 'alpha' parameter.
  { daisy_notreached (); }

  MobsolFull (Block& al)
    : Mobsol (al)
  {}
};

static struct MobsolFullSyntax
{
  static Model& make (Block& al)
  { return *new MobsolFull (al); }
  MobsolFullSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Solute is fully mobile.");
    Librarian::add_type (Mobsol::component, "full", alist, syntax, &make);
  }
} MobsolFull_syntax;

const AttributeList& 
Mobsol::full_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    alist.add ("type", "full");

  return alist;
}

// "mixed" model.

struct MobsolMixed : public Mobsol
{
  const double h_lim_;
  const double alpha_;

  bool full () const            // True iff all water is mobile.
  { return false; }
  double h_lim () const         // The value of the 'h_lim' parameter.
  { return h_lim_; }
  double alpha () const         // The value of the 'alpha' parameter.
  { return alpha_; }
  
  MobsolMixed (Block& al)
    : Mobsol (al),
      h_lim_ (al.number ("h_lim")),
      alpha_ (al.number ("alpha"))    
  {}
};

static struct MobsolMixedSyntax
{
  static Model& make (Block& al)
  { return *new MobsolMixed (al); }
  MobsolMixedSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Solute is mobile and immobile.");
  
    syntax.add ("h_lim", "cm", Syntax::Const, "\
Pressure for activating mobile water.");
    syntax.add ("alpha", "h^-1", Syntax::Const, "\
Exchange rate between mobile and immobile water."); 
   Librarian::add_type (Mobsol::component, "mixed", alist, syntax, &make);
  }
} MobsolMixed_syntax;

// mobsol.C ends here.
