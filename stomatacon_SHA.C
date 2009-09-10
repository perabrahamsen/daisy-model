// stomatacon_SHA.C -- Stomata conductance calculated by Seyed Hamid Ahmadi.
// 
// Copyright 2008 Birgitte Gjettermann and KU
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

#include "stomatacon.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"

struct StomataCon_SHA : public StomataCon
{
  // Parameters.
private:
  const double lambda, alpha;  // Coefficients []
  const double M;              // [?]

  
  // Simulation.
  double stomata_con (const double wsf, const double m, const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, const double intercept,
                      const double CO2_atm, const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_SHA (const BlockModel& al)
    : StomataCon (al),
      lambda (al.number ("lambda")),
      alpha (al.number ("alpha")),
      M (al.number ("M"))
  { }
};

double
StomataCon_SHA::stomata_con (const double wsf /*[]*/, 
                             const double,
                             const double hs /*[]*/, 
                             const double pz /*[mol/m²leaf/s]*/,
                             const double Ptot /*[Pa]*/, const double cs /*[Pa]*/,
                             const double, const double intercept /*[mol/m²leaf/s]*/,
                             const double CO2_atm, const double, Treelog&)
{
  const double cs_ppm = cs /*[Pa]*/ / Ptot /*[Pa]*/ * 1.0e6 /*[ppm]*/;
  const double A = pz * 1e6;    // [umol/m^2 LEAF/s]
  const double gsw = std::max (wsf * (M * pow(hs, alpha) * pow(A, lambda))
                               /(cs_ppm),
                               intercept);
  daisy_assert (gsw >= 0.0);
  return gsw;
}

static struct StomataConSHASyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_SHA (al); }
  StomataConSHASyntax ()
    : DeclareModel (StomataCon::component, "SHA", "\
Stomata conductance calculated by the model given by Eq. 12.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "Ahmadi20091541");

    frame.declare ("alpha", Attribute::None (), Check::non_negative (),
                   Attribute::Const,
                   "Humidity effect");
    frame.set ("alpha", 1.0);
    frame.declare ("lambda", Attribute::None (), Check::non_negative (),
                   Attribute::Const,
                   "Net photosyhtesis effect");
    frame.set ("lambda", 1.0);
    frame.declare ("M", Attribute::Unknown (), 
                   Check::non_negative (), Attribute::Const,
                   "Parameter ??");
    frame.set ("M", 1.0);
  }
} StomataConSHAsyntax;


struct StomataCon_SHA14 : public StomataCon
{
  // Parameters.
private:
  const double lambda; // Net photosynthesis effect [m^2 leaf s/mol CO2]
  const double alpha;  // Humidity effect []
  const double M;      // Conductivity factor [mol H2O/m^2 leaf/s]

  // Simulation.
  double stomata_con (const double wsf, const double m, const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, const double intercept,
                      const double CO2_atm, const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_SHA14 (const BlockModel& al)
    : StomataCon (al),
      lambda (al.number ("lambda")),
      alpha (al.number ("alpha")),
      M (al.number ("M"))
  { }
};

double
StomataCon_SHA14::stomata_con (const double wsf /*[]*/, 
                               const double,
                               const double hs /*[]*/, 
                               const double pz /*[mol/m²leaf/s]*/,
                               const double Ptot /*[Pa]*/, 
                               const double cs /*[Pa]*/,
                               const double, const double,
                               const double CO2_atm, const double, Treelog&)
{
  const double cs_ppm = cs /*[Pa]*/ / Ptot /*[Pa]*/ * 1.0e6 /*[ppm]*/;
  const double A = pz * 1e6;    // [umol/m^2 LEAF/s]
  const double gsw = wsf * M * exp (hs * alpha) * exp (A * lambda) / cs_ppm;
  daisy_assert (gsw >= 0.0);
  return gsw;
}

static struct StomataConSHA14Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_SHA14 (al); }
  StomataConSHA14Syntax ()
    : DeclareModel (StomataCon::component, "SHA14", "\
Stomata conductance calculated by the model given by Eq. 14.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "Ahmadi20091541");

    frame.declare ("alpha", Attribute::None (), Check::non_negative (),
                   Attribute::Const,
                   "Humidity effect");
    frame.declare ("lambda", "m^2 leaf s/umol CO2", Check::non_negative (),
                   Attribute::Const,
                   "Net photosyhtesis effect");
    frame.declare ("M", "mol H2O/m^2 leaf/s", 
                   Check::non_negative (), Attribute::Const,
                   "Conductivity factor.");
  }
} StomataConSHA14syntax;

// stomatacon_SHA.C ends here.
