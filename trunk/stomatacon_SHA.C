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

struct StomataCon_SHA12 : public StomataCon_WSF_base
{
  // Parameters.
private:
  const double lambda;          // Assimilate exponent. []
  const double alpha;           // Humidity exponent. []
  const double m;               // Conductivity factor. [?]
  const double gs_min;          // Minimal gs value. [mol H2O/m^2 leaf/s]
  
  // Simulation.
  double minimum () const
  { return gs_min; }
  double stomata_con (const double ABA,  // [g/cm^3]
                      const double h_x,  // [MPa]
                      const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_SHA12 (const BlockModel& al)
    : StomataCon_WSF_base (al),
      lambda (al.number ("lambda")),
      alpha (al.number ("alpha")),
      m (al.number ("m")),
      gs_min (al.number ("min"))
  { }
};

double
StomataCon_SHA12::stomata_con (const double ABA,  // [g/cm^3]
                               const double h_x,  // [MPa]
                               const double hs /*[]*/, 
                               const double pz /*[mol/m²leaf/s]*/,
                               const double Ptot /*[Pa]*/,
                               const double cs /*[Pa]*/,
                               const double, const double, Treelog&)
{
  if (pz <= 0.0)
    return gs_min;
  const double cs_ppm = cs /*[Pa]*/ / Ptot /*[Pa]*/ * 1.0e6 /*[ppm]*/;
  const double A = pz * 1e6;    // [umol/m^2 LEAF/s]
  return std::max (wsf (ABA, h_x) 
                   * (m * pow(hs, alpha) * pow(A, lambda)) /(cs_ppm),
                   gs_min);
}

static struct StomataConSHA12Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_SHA12 (al); }
  StomataConSHA12Syntax ()
    : DeclareModel (StomataCon::component, "SHA12", "WSF", "\
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
    frame.declare ("m", Attribute::Unknown (), 
                   Check::non_negative (), Attribute::Const,
                   "Slope parameter, dimension depends on alpha and lambda.");
    frame.declare ("min", "mol H2O/m^2 leaf/s", 
                   Check::positive (), Attribute::OptionalConst,
                   "Minimal conductivity.");
  }
} StomataConSHA12syntax;


struct StomataCon_SHA14 : public StomataCon_WSF_base
{
  // Parameters.
private:
  const double lambda; // Net photosynthesis effect [m^2 leaf s/mol CO2]
  const double alpha;  // Humidity effect []
  const double m;      // Conductivity factor [mol H2O/m^2 leaf/s]
  const double gs_max; // Maximum gs [mol/m^2 leaf/s]

  // Simulation.
  double minimum () const
  { 
    // Assuming wsf = 1.0, A = 0, hs = 0.0.
    const double cs_min = 2.0;  // [ppm]
    return m / cs_min; 
  }
  double stomata_con (const double ABA,  // [g/cm^3]
                      const double h_x,  // [MPa]
                      const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, 
                      const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_SHA14 (const BlockModel& al)
    : StomataCon_WSF_base (al),
      lambda (al.number ("lambda")),
      alpha (al.number ("alpha")),
      m (al.number ("m")),
      gs_max (al.number ("max", -42.42e42))
  { }
};

double
StomataCon_SHA14::stomata_con (const double ABA,  // [g/cm^3]
                               const double h_x,  // [MPa]
                               const double hs /*[]*/, 
                               const double pz /*[mol/m²leaf/s]*/,
                               const double Ptot /*[Pa]*/, 
                               const double cs /*[Pa]*/,
                               const double,
                               const double, Treelog&)
{
  const double cs_ppm = cs /*[Pa]*/ / Ptot /*[Pa]*/ * 1.0e6 /*[ppm]*/;
  const double A = std::max (pz, 0.0) * 1e6;    // [umol/m^2 LEAF/s]
  const double gsw = wsf (ABA, h_x) 
    * m * exp (hs * alpha) * exp (A * lambda) / cs_ppm;
  daisy_assert (gsw >= 0.0);

  if (gs_max < 0.0)
    return gsw;
  return std::min (gsw, gs_max);
}

static struct StomataConSHA14Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_SHA14 (al); }
  StomataConSHA14Syntax ()
    : DeclareModel (StomataCon::component, "SHA14", "WSF", "\
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
    frame.declare ("m", "mol H2O/m^2 leaf/s", 
                   Check::non_negative (), Attribute::Const,
                   "Conductivity factor.");
    frame.declare ("max", "mol H2O/m^2 leaf/s", 
                   Check::none (), Attribute::OptionalConst,
                   "Maximal conductivity.\n\
By default, there is no maxium.");
  }
} StomataConSHA14syntax;

struct StomataCon_MNA : public StomataCon_WSF_base
{
  // Parameters.
private:
  const double lambda; // Net photosynthesis effect [m^2 leaf s/mol CO2]
  const double alpha;  // Humidity effect []
  const double m;      // Conductivity factor [mol H2O/m^2 leaf/s]
  const double b;     // Stomatal intercept.
  const double gs_max; // Maximum gs [mol/m^2 leaf/s]

  // Simulation.
  double minimum () const
  { return b; }
  double stomata_con (const double ABA,  // [g/cm^3]
                      const double h_x,  // [MPa]
                      const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, 
                      const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_MNA (const BlockModel& al)
    : StomataCon_WSF_base (al),
      lambda (al.number ("lambda")),
      alpha (al.number ("alpha")),
      m (al.number ("m")),
      b (al.number ("b")),
      gs_max (al.number ("max", -42.42e42))
  { }
};

double
StomataCon_MNA::stomata_con (const double ABA,  // [g/cm^3]
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

  const double cs_ppm = cs /*[Pa]*/ / Ptot /*[Pa]*/ * 1.0e6 /*[ppm]*/;
  const double A = pz * 1e6;    // [umol/m^2 LEAF/s]
  daisy_assert (A > 0.0);
  const double gsw = b 
    + wsf (ABA, h_x) * m * exp (hs * alpha) * exp (lambda / A) / cs_ppm;
  daisy_assert (gsw >= 0.0);
  if (gs_max < 0.0)
    return gsw;
  return std::min (gsw, gs_max);
}

static struct StomataConMNASyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_MNA (al); }
  StomataConMNASyntax ()
    : DeclareModel (StomataCon::component, "MNA", "WSF", "\
Stomata conductance calculated by the model given by Eq. 14.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("alpha", Attribute::None (), Check::non_negative (),
                   Attribute::Const,
                   "Humidity effect");
    frame.declare ("lambda", "umol CO2/m^2 leaf/s", Check::non_positive (),
                   Attribute::Const,
                   "Net photosyhtesis effect");
    frame.declare ("m", "mol H2O/m^2 leaf/s", 
                   Check::non_negative (), Attribute::Const,
                   "Conductivity factor.");
    frame.declare ("b", "mol/m^2/s", Check::positive (), Attribute::Const, "\
Stomatal intercept.\n\
Ball and Berry (1982) & Wang and Leuning(1998): (0.01 mol/m2/s)");
    frame.declare ("max", "mol H2O/m^2 leaf/s", 
                   Check::none (), Attribute::OptionalConst,
                   "Maximal conductivity.\n\
By default, there is no maixum.");
  }
} StomataConMNAsyntax;

// stomatacon_SHA.C ends here.
