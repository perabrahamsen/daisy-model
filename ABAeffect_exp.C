// ABAeffect_exp.C -- xylem ABA effect on photosynthesis
// 
// Copyright 2006 Birgitte Gjettermann and KVL
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "ABAeffect.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"

struct ABAEffect_exp : public ABAEffect
{
  // Parameters.
private:
  const double k;  //Coefficient
  
  // Simulation.
  double ABA_effect (const double ABA_xylem /* [g/cm^3] */,
		     Treelog& msg);	    // []
  void output (Log&) const
  { }
  const AttributeList& default_model ();

  // Create.
  public:
  ABAEffect_exp (Block& al)
    : ABAEffect (al),
       k (al.number ("k"))
  { }
};

double
ABAEffect_exp::ABA_effect (const double ABA_xylem /* [g/cm^3] */, Treelog&)
{
  const double ABAeffect = exp(-k * ABA_xylem); //[]
  return ABAeffect;
}

static struct ABAEffectexpSyntax
{
  static Model& make (Block& al)
  { return *new ABAEffect_exp (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("k", "cm^3/g", Check::positive (), Syntax::Const,
                "Coefficient");
    alist.add ("k", 1.0);
  }  
  ABAEffectexpSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Exponential curve for ABA-xylem effect on photosynthesis.");

    load_syntax (syntax, alist);

    Librarian::add_type (ABAEffect::component, "ABA-exp", alist, syntax, &make);
  }
} ABAEffectexpsyntax;


const AttributeList& 
ABAEffect::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax syntax;
      ABAEffectexpSyntax::load_syntax (syntax, alist);
      alist.add ("type", "ABA-exp");
    }
  return alist;
}
