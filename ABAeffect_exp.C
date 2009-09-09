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
#include "block_model.h"
#include "librarian.h"
#include "frame.h"

struct ABAEffect_exp : public ABAEffect
{
  // Parameters.
private:
  const double k;               // [cm^3/g]
  const double l;               // [MPa^-1]
  const double alpha;           // []
  const double ABA_min;         // [g/cm^3]

  // Simulation.
  double ABA_effect (const double ABA_xylem /* [g/cm^3] */, 
                     const double psi_c /* [cm] */,
		     Treelog& msg);	    // []
  void output (Log&) const
  { }

  // Create.
  public:
  ABAEffect_exp (const BlockModel& al)
    : ABAEffect (al),
      k (al.number ("k")),
      l (al.number ("l")),
      alpha (al.number ("alpha")),
      ABA_min (al.number ("ABA_min"))
  { }
};

double
ABAEffect_exp::ABA_effect (const double ABA_xylem /* [g/cm^3] */,
                           const double psi_c /* [cm] */, Treelog&)
{
  daisy_assert (ABA_xylem >= 0.0);
  // double Mw = 264.32; //Molecular weight of ABA (Abscisic Acid)[g mol^-1]
  const double ABA_use = std::max (ABA_xylem - ABA_min, 0.0);
  const double ABAeffect = std::exp(-k * (ABA_use)); //[]
  const double psi_use = std::fabs (psi_c) * 1.0e-4  /* [MPa/cm] */; // MPa
  const double psi_c_effect =  std::exp(-l * psi_use);
  return alpha * ABAeffect * psi_c_effect;
}

static struct ABAEffectexpSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ABAEffect_exp (al); }
  void load_frame (Frame& frame) const
  {
    frame.declare ("k", "cm^3/g", Check::non_negative (), Attribute::Const,
                   "Effect of ABA concentration.");
    frame.set ("k", 0.0);
    frame.declare ("l", "MPa^-1", Check::non_negative (), Attribute::Const,
                   "Effect of crown water potential.");
    frame.set ("l", 0.0);
    frame.declare ("alpha", Attribute::None(), Check::non_negative (),
                   Attribute::Const, "\
Fudge factor to ensure unstressed production is 1.0.");
    frame.set ("alpha", 1.0);
    frame.declare ("ABA_min", "g/cm^3", Check::non_negative (), Attribute::Const,
                   "Level of ABA with unstressed production.");
    frame.set ("ABA_min", 0.0);
  }  
  ABAEffectexpSyntax ()
    : DeclareModel (ABAEffect::component, "ABA-exp", "\
Exponential curve for ABA_xylem and crown water potential effect on gs.\n\
\n\
The water stress factor is calculates as\n\
\n\
  wsf = alpha * exp (-k * (ABA_xylem - ABA_min)) * exp (-l |psi_c|)\n\
\n\
where ABA_xylem and psi_c are calculated by Daisy, and the rest are\n\
parameters.")
  { }
} ABAEffectexpsyntax;

// ABAeffect.C ends here.
