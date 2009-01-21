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
#include "frame.h"

struct ABAEffect_exp : public ABAEffect
{
  // Parameters.
private:
  const double k;  //Coefficient
  const double alpha;  //Coefficient
  
  // Simulation.
  double ABA_effect (const double ABA_xylem /* [g/cm^3] */,
		     Treelog& msg);	    // []
  void output (Log&) const
  { }

  // Create.
  public:
  ABAEffect_exp (Block& al)
    : ABAEffect (al),
      k (al.number ("k")),
      alpha (al.number ("alpha"))

  { }
};

double
ABAEffect_exp::ABA_effect (const double ABA_xylem /* [g/cm^3] */, Treelog&)
{
  daisy_assert (ABA_xylem >= 0.0);
  //  const double Mw = 264.32; //Molecular weight of ABA (Abscisic Acid)[g mol^-1]
  const double ABA_xylem_ = ABA_xylem; // [g/cm^3]
  const double ABAeffect = alpha * exp(-k * ABA_xylem_); //[]
  return ABAeffect;
}

static struct ABAEffectexpSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ABAEffect_exp (al); }
  void load_frame (Frame& frame) const
  {
    frame.add ("k", "cm^3/g", Check::non_negative (), Value::Const,
                "Coefficient");
    frame.add ("k", 0.0);
    frame.add ("alpha", Value::None(), Check::non_negative (), Value::Const,
                "Coefficient");
    frame.add ("alpha", 1.0);
  }  
  ABAEffectexpSyntax ()
    : DeclareModel (ABAEffect::component, "ABA-exp", "\
Exponential curve for ABA-xylem effect on photosynthesis.")
  { }
} ABAEffectexpsyntax;

// ABAeffect.C ends here.
