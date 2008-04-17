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
#include "block.h"
#include "alist.h"
#include "librarian.h"
#include "assertion.h"

// secondary component.

const char *const Secondary::component = "secondary";

symbol
Secondary::library_id () const
{
  static const symbol id (component);
  return id;
}

Secondary::Secondary (Block& al)
  : name (al.identifier ("type"))
{ }

Secondary::~Secondary ()
{ }

static Librarian Secondary_init (Secondary::component, "\
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
washed out with fast moving new water.");

// "none" model.

struct SecondaryNone : public Secondary
{
  bool none () const            // True iff all water is in primary domain.
  { return true; }
  double h_lim () const         // The value of the 'h_lim' parameter.
  { daisy_notreached (); }
  double alpha () const         // The value of the 'alpha' parameter.
  { daisy_notreached (); }

  SecondaryNone (Block& al)
    : Secondary (al)
  {}
};

static struct SecondaryNoneSyntax
{
  static Model& make (Block& al)
  { return *new SecondaryNone (al); }
  SecondaryNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No secondary domain.\n\
\n\
There is always full equilibrium between solute in different size\n\
matrix pores.");
    Librarian::add_type (Secondary::component, "none", alist, syntax, &make);
  }
} SecondaryNone_syntax;

const AttributeList& 
Secondary::none_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    alist.add ("type", "none");

  return alist;
}

// "active" model.

struct SecondaryPressure : public Secondary
{
  const double h_lim_;
  const double alpha_;

  bool none () const            // True iff all water is in primary domain.
  { return false; }
  double h_lim () const         // The value of the 'h_lim' parameter.
  { return h_lim_; }
  double alpha () const         // The value of the 'alpha' parameter.
  { return alpha_; }
  
  SecondaryPressure (Block& al)
    : Secondary (al),
      h_lim_ (al.number ("h_lim")),
      alpha_ (al.number ("alpha"))    
  {}
};

static struct SecondaryPressureSyntax
{
  static Model& make (Block& al)
  { return *new SecondaryPressure (al); }
  SecondaryPressureSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Horizon has secondary domain.\n\
\n\
The secondary domain consist of water in matrix pores larger than\n\
what corresponds to the specified pressure. "); 
  
    syntax.add ("h_lim", "cm", Syntax::Const, "\
Pressure for activating secondary domain.");
    syntax.add ("alpha", "h^-1", Syntax::Const, "\
Exchange rate between primary and secondary water."); 
   Librarian::add_type (Secondary::component, "pressure", alist, syntax, &make);
  }
} SecondaryPressure_syntax;

// secondary.C ends here.
