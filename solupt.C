// solupt.C  -- Uptake of nitrogen through roots.
// 
// Copyright 2016 Per Abrahamsen and KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.5
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

#include "solupt.h"
#include "block_model.h"
#include "librarian.h"
#include "mathlib.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "chemical.h"
#include "assertion.h"
#include "number.h"
#include "scope_multi.h"
#include "frame_submodel.h"
#include "units.h"
#include "iterative.h"
#include "check.h"
#include "log.h"
#include <vector>
#include <sstream>
#include <cmath>

// The 'solute_uptake' component.

const char *const Solupt::component = "solute_uptake";

symbol 
Solupt::library_id () const
{
  static const symbol id (component);
  return id;
}

Solupt::Solupt (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Solupt::~Solupt ()
{ }

static struct SoluptInit : public DeclareComponent
{
  SoluptInit () 
    : DeclareComponent (Solupt::component, "\
The 'solute_uptake' component calculates uptake of nitrogen through roots.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Solupt_init;

// The 'none' model.

struct SoluptNone final : public Solupt
{
  // Solve.
  double value (const Geometry& geo, const Soil& soil,
                const SoilWater& soil_water,
                const std::vector<double> Density,
                const std::vector<double> H2OExtraction,
                const double Rad,
                Chemical& solute,
                const double PotNUpt, // [g/m^2/h]
                std::vector<double>& uptake,
                const double I_max,      // [g/cm R/h]
                const double C_root_min) // [g/cm^3 W]
  {
    std::fill (uptake.begin (), uptake.end (), 0.0);
    return 0.0;
  }
  void output (Log&) const
  { }

  // Create and Destroy.
  void initialize (const Geometry&, Treelog&)
  { }
  SoluptNone (const BlockModel& al)
    : Solupt (al)
  { }
  ~SoluptNone ()
  { }
};

static struct SoluptNoneSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SoluptNone (al); }
  SoluptNoneSyntax ()
    : DeclareModel (Solupt::component, "none", "No uptake.")
  { }
  void load_frame (Frame& frame) const
  { }
} SoluptNone_syntax;

// The 'diffusion' base model.

struct SoluptDiffusion : public Solupt
{
  std::vector<double> D; // [cm^2/h]
  std::vector<double> alpha; // Internal variable []
  std::vector<double> beta; // Internal variable []
  std::vector<double> A; // Internal variable []
  std::vector<double> B; // Internal variable []
  
  // Solve.
private:
  static void find_A_B (const double D,
                        const double alpha, const double beta,
                        const double q_r, const double C_l,
                        double& A, double& B);
public:
  void diffusion (const Geometry& geo, const Soil& soil,
                  const SoilWater& soil_water,
                  const std::vector<double> Density,
                  const std::vector<double> H2OExtraction,
                  const double Rad,
                  Chemical& solute);
  

  void output_diffusion (Log& log) const
  {
    output_variable (D, log);
    output_variable (alpha, log);
    output_variable (beta, log);
    output_variable (A, log);
    output_variable (B, log);
  }

  // Create and Destroy.
  void initialize (const Geometry& geo, Treelog&)
  { D = alpha = beta = A = B = std::vector<double> (geo.cell_size (), NAN); }
  SoluptDiffusion (const BlockModel& al)
    : Solupt (al)
  { }
  ~SoluptDiffusion ()
  { }
};

void
SoluptDiffusion::find_A_B (const double D,
                           const double alpha, const double beta,
                           const double q_r, const double C_l,
                           double& A, double& B)
{
  const double beta_squared = beta * beta;
  if (alpha < 1e-10)
    {
      // [cm^3 W/cm R/h] = [cm^2/h] / ([] * log ([]/[]) - [])
      B = 4.0 * M_PI * D
        / (beta_squared * log (beta_squared) / (beta_squared - 1.0)
           - 1.0);
      // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
      A = B * C_l;
    }
  else
    { 
      const double divisor // []
        = ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
           - (pow (beta, 2.0 - alpha) - 1.0));

      if (std::isnormal (divisor))
        {
          // [cm^3 W/cm R/h] = [cm^3 W/cm R/h]
          B = q_r * (pow (beta, 2.0 - alpha) - 1.0)
            / divisor;
          // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
          A = q_r * (beta_squared - 1.0) * (1.0 - 0.5 * alpha) * C_l
            / divisor;
        }
      else              
        {
          daisy_assert (approximate (alpha, 2.0));
          const double div2 // []
            = ((beta_squared - 1.0) - log (beta_squared));
          daisy_assert (std::isnormal (div2));
          // [cm^3 W/cm R/h] = [cm^3 W/cm R/h]
          B = q_r * log (beta_squared) / div2;
          // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
          A = q_r * (beta_squared - 1.0) * C_l / div2;
        }
    }
}

void
SoluptDiffusion::diffusion (const Geometry& geo, const Soil& soil,
                            const SoilWater& soil_water,
                            const std::vector<double> Density,
                            const std::vector<double> H2OExtraction,
                            const double Rad,
                            Chemical& solute)
{
  const size_t size = geo.cell_size ();
  for (size_t i = 0; i < size; i++)
    {
      const double L = Density[i];                   // [cm R/cm^3 S]
      if (L > 0 && soil_water.h_old (i) <= 0.0)
        {
          const double C_l = solute.C_secondary (i);     // [g/cm^3 W]
          const double Theta = soil_water.Theta_old (i); // [cm^3 W/cm^3 S]
          // [cm^3 W/cm R/h] = [cm^3 W/cm^3 S/h] / [cm R/cm^3 S]
          const double q_r = H2OExtraction[i] / L;

          // [cm^2/h] = [cm^2/h] * [cm^3/cm^3] * [cm^3 W/cm^3 S]
          D[i] = solute.diffusion_coefficient ()
            * soil.tortuosity_factor (i, Theta)
            * Theta;
          // [] = [cm^3 W/cm R/h] / [cm^2/h]
          alpha[i] = q_r / ( 2 * M_PI * D[i]);
          
          // [] = [] / ([cm] * sqrt ([cm^-2]))
          beta[i] = 1.0 / (Rad * sqrt (M_PI * L));
          // [] = [] * []

          find_A_B (D[i], alpha[i], beta[i], q_r, C_l, A[i], B[i]);
        }
      else
        D[i] = alpha[i] = beta[i] = A[i] = B[i] = NAN;
    }
}

static struct SoluteDiffusionSyntax : DeclareBase
{
  SoluteDiffusionSyntax ()
    : DeclareBase (Solupt::component, "diffusion", "\
N is transported to the root surface based on diffusion and convection.\n\
\n\
The convection is based on concentration in the soil, while the\n\
diffusion is based on concentration in both soil and at root surfaces.\n\
\n\
This base model find variables 'A' and 'B', so the transport of\n\
nitrogen to the root surface is calculated as A - B * CRoot, where\n\
CRoot is the concentration at the root surface.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "daisyN");
    frame.declare ("D", "cm^2/h", Check::none (), 
                   Attribute::LogOnly, Attribute::SoilCells,
                   "Diffusion variable.");
    frame.declare ("alpha", Attribute::None (), Check::none (), 
                   Attribute::LogOnly, Attribute::SoilCells,
                   "Internal variable.");
    frame.declare ("beta", Attribute::None (), Check::none (), 
                   Attribute::LogOnly, Attribute::SoilCells,
                   "Internal variable.");
    frame.declare ("A", Attribute::None (), Check::none (), 
                   Attribute::LogOnly, Attribute::SoilCells,
                   "CRoot independent N uptake.");
    frame.declare ("B", Attribute::None (), Check::none (), 
                   Attribute::LogOnly, Attribute::SoilCells,
                   "CRoot dependent N uptake factor.");
  }
} SoluteDiffusion_syntax;

// The 'fixed_sink' model.

struct SoluptFixed final : public SoluptDiffusion
{
  // Log variable.
  double CRoot;       // Concentration at root surface [g N/cm^3 W]

  // Solve.
  double value (const Geometry& geo, const Soil& soil,
                const SoilWater& soil_water,
                const std::vector<double> Density,
                const std::vector<double> H2OExtraction,
                const double Rad,
                Chemical& solute,
                double PotNUpt, // [g/m^2/h]
                std::vector<double>& uptake,
                const double I_max,      // [g/cm R/h]
                const double C_root_min); // [g/cm^3 W]
  void output (Log& log) const
  {
    output_diffusion (log);
    output_variable (CRoot, log);
  }

  // Create and Destroy.
  SoluptFixed (const BlockModel& al)
    : SoluptDiffusion (al),
      CRoot (NAN)
  { }
  ~SoluptFixed ()
  { }
};

double                   // [g N/m^2/h]
SoluptFixed::value (const Geometry& geo, const Soil& soil,
                    const SoilWater& soil_water,
                    const std::vector<double> Density,
                    const std::vector<double> H2OExtraction,
                    const double Rad,
                    Chemical& solute,
                    double PotNUpt, // [g/m^2/h]
                    std::vector<double>& uptake,
                    const double I_max,      // [g/cm R/h]
                    const double C_root_min) // [g/cm^3 W]
{
  // Find uptake (C_root) variables.
  diffusion (geo, soil, soil_water, Density, H2OExtraction, Rad, solute);

  // Limit for potential uptake.
  if (PotNUpt <= 0.0)
    {
      CRoot = NAN;
      fill (uptake.begin (), uptake.end (), 0.0);
      return 0.0;
    }
  daisy_assert (PotNUpt > 0.0);
  PotNUpt /= 1.0e4;             // g/m^2/h -> g/cm^2/h
  PotNUpt *= geo.surface_area (); // [g/h]
  
  const size_t size = geo.cell_size ();

  // U: Uptake per root length.
  // U = A - B * C_root

  double U = 0.0;               // [g/h] Total uptake a C_root_min.
  double A_total = 0.0;         // [g/h] "A" part of uptake.
  double B_total = 0.0;         // [cm^3 W/h] "B" part of uptake.

  for (size_t i = 0; i < size; i++)
    {
      const double L = Density[i];                   // [cm R/cm^3 S]
      if (L > 0 && soil_water.h_old (i) <= 0.0)
        {
          daisy_assert (std::isfinite (A[i]));
          daisy_assert (std::isfinite (B[i]));
          const double r_l = L * geo.cell_volume (i); // [cm R] Root length
          // [cm^3 W/h] = [cm R] * [cm^3 W/cm R/h]
          B_total += r_l * B[i];
          // [g/h] = [cm R]  * ([g/cm R/h] - [cm^3 W/cm R/h] * [g/cm^3 W])
          U += r_l * bound (0.0, A[i] - B[i] * C_root_min, I_max);
          A_total +=  r_l * bound (0.0, A[i], I_max);
        }
    }
  if (U > PotNUpt)
    // [g/cm^3 W] = max (([g/h] - [g/h]) / [cm^3 W/h], [g/cm^3 W])
    CRoot = (A_total - PotNUpt) / B_total;
  else 
    CRoot = C_root_min;   // [g/cm^3 W]

  daisy_assert (CRoot + 0.01 > C_root_min);

  for (size_t i = 0; i < size; i++)
    {
      const double L = Density[i]; // [cm R/cm^3 S]
      if (L > 0 && soil_water.h_old (i) <= 0.0)
        // [g/cm^3 S/h]
        //  = [cm R/cm^3 S] * ([g/cm R/h] - [cm^3 W/cm R/h] * [g/cm^3 W])
        uptake[i] = bound (0.0, L * (A[i] - B[i] * CRoot), I_max);
      else
        uptake[i] = 0.0;
      daisy_assert (uptake[i] >= 0.0);
    }
  solute.add_to_root_sink (uptake);

  // gN/cm³/h -> gN/m²/h
  return geo.total_surface (uptake) * 1.0e4;
}

static struct SoluptFixedSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SoluptFixed (al); }
  SoluptFixedSyntax ()
    : DeclareModel (Solupt::component, "fixed_sink", "diffusion", "\
Find the highest value of 'CRoot' that meets the N demand.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("CRoot", "g N/cm^3", Check::none (), Attribute::LogOnly, 
                   "Nitrogen concentration at root surface.");
  }
} SoluptFixed_syntax;

// The 'variable_sink' model.

struct SoluptVariable : public SoluptDiffusion
{
  // Parameters;
  const double K1;
  const double K2;
  const double F_relative;
  
  // Log variable.
  std::vector<double> C1; // Concentration at root surface [g N/cm^3 W]
  std::vector<double> C2; // Concentration at root surface [g N/cm^3 W]
  std::vector<double> CEst; // Concentration at root surface [g N/cm^3 W]
  std::vector<double> CRoot; // Concentration at root surface [g N/cm^3 W]

  // Simulation.
  void estimate_C_root (const double I_max);
  void find_C_root (const double I_max);
  double value (const Geometry& geo, const Soil& soil,
                const SoilWater& soil_water,
                const std::vector<double> Density,
                const std::vector<double> H2OExtraction,
                const double Rad,
                Chemical& solute,
                double PotNUpt, // [g/m^2/h]
                std::vector<double>& uptake,
                const double I_max,      // [g/cm R/h]
                const double C_root_min); // [g/cm^3 W]
  void output (Log& log) const
  {
    output_diffusion (log);
    output_variable (C1, log);
    output_variable (C2, log);
    output_variable (CEst, log);
    output_variable (CRoot, log);
  }

  // Create and Destroy.
  void initialize (const Geometry& geo, Treelog& msg)
  {
    SoluptDiffusion::initialize (geo, msg);
    C1 = C2 = CEst = CRoot = std::vector<double> (geo.cell_size (), NAN);
    std::vector<double> (geo.cell_size (), NAN);
  }
  SoluptVariable (const BlockModel& al)
    : SoluptDiffusion (al),
      K1 (al.number ("K1")),
      K2 (al.number ("K2")),
      F_relative (al.number ("F_relative"))
  { }
  ~SoluptVariable ()
  { }
};

void
SoluptVariable::estimate_C_root (const double I_max)
{
  const double F1 = I_max * F_relative;
  const double F2 = I_max - F1;

  const size_t size = A.size ();
  for (size_t i = 0; i < size; i++)
    {
      if (!std::isfinite (A[i]))
        {
          C1[i] = C2[i] = CEst[i] = NAN;
          continue;
        }
      // Solve A - B C = F1 C / (K1 + C) + F2 C / (K2 + C)

#if 1             // Textbook solution.
      // Case 1: C >> K1
      // A - B C ~= F1 + F2 C / (K2 + C) <=>
      // -B C^2 + (A - F1 - F2 - B K2) C + (A - F1) K2 = 0
      {
        const double a = - B[i];
        const double b = A[i] - F1 - F2 - B[i] * K2;
        const double c = (A[i] - F1) * K2;
        const double d = sqr (b) - 4 * a * c;
        C1[i] = (- b - std::sqrt (d)) / (2.0 * a);
      }
      // Case 2: C << K2
      // A - B C ~= F1 C / (K1 + C) + F2 C / K2
      // (-B - F2 / K2) C^2 + (A - B K1 - F1 - F2 K1 / K2 C + A K1 = 0
      {
        const double a = - B[i] - F2 / K2;
        const double b = A[i] - B[i] * K1 - F1 - F2 * K1 / K2;
        const double c = A[i] * K1;
        const double d = sqr (b) - 4 * a * c;
        C2[i] = (- b - std::sqrt (d)) / (2.0 * a);
      }
#else  // SH solution
      // Case 1: C >> K1
      // A - B C ~= F1 + F2 C / (K2 + C)
      {
        const double a1 = (A[i] - F1 - B[i] * K2 - F2) / B[i];
        const double a2 = (A[i] - F1) * K2 / B[i];
        C1[i] = 0.5 * a1 + std::sqrt (0.25 * a1 * a1 + a2);
      }
      // Case: C << K2
      // A - B C ~= F1 C / (K1 + C) + F2 C / K2
      {
        const double a1 = (A[i] - B[i] * K1 - F1 - F2 * K1 / K2)
          / (B[i] + F2 / K2);
        const double a2 = A[i] * K1 / (B[i] + F2 / K2);
        C2[i] = 0.5 * a1 + std::sqrt (0.25 * a1 * a1 + a2);
      }
#endif

      daisy_assert (std::isfinite (C1[i]) || std::isfinite (C2[i]));
      if (!std::isfinite (C1[i]))
        CEst[i] = C2[i];
      else if (!std::isfinite (C2[i]))
        CEst[i] = C1[i];
      else
        // Which assumption is best? C1 >> K1 or C2 << K2?
        CEst[i] = (C1[i] / K1 > K2 / C2[i]) ? C1[i] : C2[i];
      daisy_assert (std::isfinite (CEst[i]));        
    }
}

void
SoluptVariable::find_C_root (const double I_max)
{
  const double F1 = I_max * F_relative;
  const double F2 = I_max - F1;

  const size_t size = A.size ();
  for (size_t i = 0; i < size; i++)
    {
      if (!std::isfinite (A[i]))
        {
          CRoot[i] = NAN;
          continue;
        }
      // Solve A - B C = F1 C / (K1 + C) + F2 C / (K2 + C)

      auto fun = [=] (const double C) -> double
        { return -A[i] + B[i] * C + F1 * C / (K1 + C) + F2 * C / (K2 + C); };
      auto derived = [=] (const double C) -> double
        { return B[i] + F1 * K1 / sqr (K1 + C) + F2 * K2 / sqr (K2 + C); };

      CRoot[i] = Newton (CEst[i], fun, derived);
      daisy_assert (std::isfinite (CRoot[i]));
    }
}

double                   // [g N/m^2/h]
SoluptVariable::value (const Geometry& geo, const Soil& soil,
                       const SoilWater& soil_water,
                       const std::vector<double> Density,
                       const std::vector<double> H2OExtraction,
                       const double Rad,
                       Chemical& solute,
                       double PotNUpt, // [g/m^2/h]
                       std::vector<double>& uptake,
                       const double I_max,      // [g/cm R/h]
                       const double C_root_min) // [g/cm^3 W]
{
  const size_t size = geo.cell_size ();

  // Find uptake (C_root) variables.
  diffusion (geo, soil, soil_water, Density, H2OExtraction, Rad, solute);
  estimate_C_root (I_max);
  find_C_root (I_max);

  // Find uptake.
  for (size_t i = 0; i < size; i++)
    {
      const double L = Density[i]; // [cm R/cm^3 S]
      if (L > 0 && soil_water.h_old (i) <= 0.0)
        // [g/cm^3 S/h]
        //  = [cm R/cm^3 S] * ([g/cm R/h] - [cm^3 W/cm R/h] * [g/cm^3 W])
        uptake[i] = bound (0.0,
                           L * (A[i] - B[i] * std::max (CRoot[i], C_root_min)),
                           I_max);
      else
        uptake[i] = 0.0;
      daisy_assert (uptake[i] >= 0.0);
    }
  
  // gN/cm^3/h -> gN/m^2/h
  const double max_uptake = geo.total_surface (uptake) * 1.0e4;
  if (max_uptake > PotNUpt)
    {
      const double factor = PotNUpt / max_uptake;
      for (size_t i = 0; i < size; i++)
        uptake[i] *= factor;
    }

  solute.add_to_root_sink (uptake);

  return std::min (max_uptake, PotNUpt);
}

static struct SoluptVariableSyntax : public DeclareModel
{
  SoluptVariableSyntax ()
    : DeclareModel (Solupt::component, "variable_sink", "diffusion", "\
Nitrogen uptake limited by the root ability to assimilate N.\n\
\n\
The transport of N to the root surface is given as \n\
\n\
  A - B C\n\
\n\
The assimilation of N from root surface is given as\n\
\n\
  F1 C / (K1 + C) + F2 C / (K2 + C)  \n\
\n\
We find the C for which \n\
\n\
  A - B C = F1 C / (K1 + C) + F2 C / (K2 + C)\n\
\n\
If this is higher than the demand, uptake is scaled down.")
  { }
  Model* make (const BlockModel& al) const
  { return new SoluptVariable (al); }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "tsay2007nitrate", "miller2005root");
    frame.declare ("K1", "g N/cm^3", Check::positive (), Attribute::Const,
                   "K parameter for high affinity uptake.");
    frame.set_cited ("K1", 0.7e-6, "50 uM", "tsay2007nitrate");
    frame.declare ("K2", "g N/cm^3", Check::positive (), Attribute::Const,
                   "K parameter for low affinity uptake.");
    frame.set_cited ("K2", 70.0e-6, "5 mM", "tsay2007nitrate");
    frame.declare_fraction ("F_relative", Attribute::Const, "\
Fraction of max uptake accounted for by high affinity uptake.");
    frame.set ("F_relative", 0.1);
    frame.declare ("C1", "g N/cm^3", Check::none (), Attribute::LogOnly,
                   Attribute::SoilCells,
                   "CRoot calculated assuming C >> K1.\n\
Solution to A - B C = F1 + F2 C / (K2 + C).");
    frame.declare ("C2", "g N/cm^3", Check::none (), Attribute::LogOnly,
                   Attribute::SoilCells,
                   "CRoot calculated assuming C << K2.\n\
Solution to A - B C = F1 C / (K1 + C) + F2 C / K2.");
    frame.declare ("CEst", "g N/cm^3", Check::none (), Attribute::LogOnly,
                   Attribute::SoilCells,
                   "Best of C1 or C2..");
    frame.declare ("CRoot", "g N/cm^3", Check::none (), Attribute::LogOnly,
                   Attribute::SoilCells,
                   "Nitrogen concentration at root surface.");
  }
} SoluptVariable_syntax;

// The 'solute_uptake' Number model

static void
solute_uptake (const double Theta,            // []
               const double Theta_sat,        // []
               const double S_w,              // [h^-1]
               const double C_l,              // [g/cm^3]
               const double PotNUpt,          // [g/h]
               const double L,                // [cm R/cm^3]
               const double I_max,            // [g/cm R/h]
               const double C_root_min,       // [g/cm^3]
               const double diffusion_coef,   // [cm^2/h]
               const double volume,           // [cm^3]
               const double Rad,              // [cm]
               double& uptake,                // [g/cm^3/h]
               double& C_root,                // [g/cm^3]
               double& D,                     // [cm^2/h]
               double& alpha,                 // []
               double& beta)                  // []
{
  daisy_assert (std::isnormal (L) && L > 0.0);
  daisy_assert (std::isnormal (Theta) && Theta > 0.0);
  daisy_assert (std::isnormal (Theta_sat) && Theta_sat > 0.0);
  
  // [cm^3 W/cm R/h] = [cm^3 W/cm^3 S/h] / [cm R/cm^3 S]
  const double q_r = S_w / L;
  const double tortuosity_factor = pow (Theta, 7.0 / 3.0) / (Theta_sat * Theta_sat); // []
  // [cm^2/h] = [cm^2/h] * [cm^3/cm^3] * [cm^3 W/cm^3 S]
  D = diffusion_coef * tortuosity_factor * Theta;
  // [] = [cm^3 W/cm R/h] / [cm^2/h]
  alpha = q_r / ( 2 * M_PI * D);
  // [] = [] / ([cm] * sqrt ([cm^-2]))
  beta = 1.0 / (Rad * sqrt (M_PI * L));
  const double beta_squared = beta * beta;

  double B_zero = NAN;
  double I_zero = NAN;
  if (alpha < 1e-10)
    {
      // [cm^3 W/cm R/h] = [cm^2/h] / ([] * log ([]/[]) - [])
      B_zero = 4.0 * M_PI * D
        / (beta_squared * log (beta_squared) / (beta_squared - 1.0)
           - 1.0);
      // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
      I_zero = B_zero * C_l;
    }
  else
    { 
      const double divisor // []
        = ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
           - (pow (beta, 2.0 - alpha) - 1.0));

      if (std::isnormal (divisor))
        {
          // [cm^3 W/cm R/h] = [cm^3 W/cm R/h]
          B_zero = q_r * (pow (beta, 2.0 - alpha) - 1.0) / divisor;
          // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
          I_zero = q_r * (beta_squared - 1.0) * (1.0 - 0.5 * alpha) * C_l / divisor;
        }
      else              
        {
          daisy_assert (approximate (alpha, 2.0));
          const double div2 // []
            = ((beta_squared - 1.0) - log (beta_squared));
          daisy_assert (std::isnormal (div2));
          // [cm^3 W/cm R/h] = [cm^3 W/cm R/h]
          B_zero = q_r * log (beta_squared) / div2;
          // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
          I_zero = q_r * (beta_squared - 1.0) * C_l / div2;
        }
    }
  const double r_l = L * volume; // [cm R] Root length in cell.
  // [cm^3 W/h] = [cm R] * [cm^3 W/cm R/h]
  const double B = r_l * B_zero;
  // [g/h] = [cm R]  * ([g/cm R/h] - [cm^3 W/cm R/h] * [g/cm^3 W])
  const double U_zero = r_l * (bound (0.0, I_zero, I_max) - B_zero * C_root_min);
  const double I_zero_total =  r_l * bound (0.0, I_zero, I_max);

  C_root = C_root_min;   // [g/cm^3 W]
  if (U_zero > PotNUpt)
    {
      // [g/cm^3 W] = max (([g/h] - [g/h]) / [cm^3 W/h], [g/cm^3 W])
      C_root = (I_zero_total - PotNUpt) / B;
      if (C_root < C_root_min)
        {
          if (C_root < 0.99 * C_root_min)
            {
              std::ostringstream tmp;
              tmp << "C_root is " << C_root << ", should larger than " << C_root_min
                  << " (C_root_min)\nU_zero = " << U_zero
                  << "\nPotNUpt = " << PotNUpt
                  << "\nI_zero_total = " << I_zero_total
                  << "\nB = " << B;
              daisy_bug (tmp.str ());
            }
          C_root = C_root_min;
        }
    }

  // [g/cm^3 S/h] = [cm R/cm^3 S] * ([g/cm R/h] - [cm^3 W/cm R/h] * [g/cm^3 W])
  uptake = L * bound (0.0, I_zero - B_zero * C_root, I_max);
}

struct NumberSoluteUptake : public Number
{
  const Units& units;

  // Parameters.
  const std::unique_ptr<Number> Theta;
  const std::unique_ptr<Number> Theta_sat;
  const std::unique_ptr<Number> S_w;
  const std::unique_ptr<Number> C_l;
  const std::unique_ptr<Number> PotNUpt;
  const std::unique_ptr<Number> L;
  const std::unique_ptr<Number> I_max;
  const std::unique_ptr<Number> C_root_min;
  const std::unique_ptr<Number> diffusion_coef;
  const std::unique_ptr<Number> volume;
  const std::unique_ptr<Number> Rad;

  const symbol Theta_dim = "";
  const symbol Theta_sat_dim = "";
  const symbol S_w_dim = "h^-1";
  const symbol C_l_dim = "g/cm^3";
  const symbol PotNUpt_dim = "g/h";
  const symbol L_dim = "cm R/cm^3";
  const symbol I_max_dim = "g/cm R/h";
  const symbol C_root_min_dim = "g/cm^3";
  const symbol diffusion_coef_dim = "cm^2/h";
  const symbol volume_dim = "cm^3";
  const symbol Rad_dim = "cm";

  // Value.
  FrameSubmodelValue result;
  const std::unique_ptr<Number> myvalue;
  bool has_value;
  
  void find_result (const Scope& scope)
  {
    // Input.
    const double Theta_val
      = units.convert (Theta->dimension (scope), Theta_dim, Theta->value (scope));
    const double Theta_sat_val
      = units.convert (Theta_sat->dimension (scope), Theta_sat_dim, Theta_sat->value (scope));
    const double S_w_val
      = units.convert (S_w->dimension (scope), S_w_dim, S_w->value (scope));
    const double C_l_val
      = units.convert (C_l->dimension (scope), C_l_dim, C_l->value (scope));
    const double PotNUpt_val
      = units.convert (PotNUpt->dimension (scope), PotNUpt_dim, PotNUpt->value (scope));
    const double L_val
      = units.convert (L->dimension (scope), L_dim, L->value (scope));
    const double I_max_val
      = units.convert (I_max->dimension (scope), I_max_dim, I_max->value (scope));
    const double C_root_min_val
      = units.convert (C_root_min->dimension (scope), C_root_min_dim, C_root_min->value (scope));
    const double diffusion_coef_val
      = units.convert (diffusion_coef->dimension (scope), diffusion_coef_dim,
                       diffusion_coef->value (scope));
    const double volume_val
      = units.convert (volume->dimension (scope), volume_dim, volume->value (scope));
    const double Rad_val
      = units.convert (Rad->dimension (scope), Rad_dim, Rad->value (scope));

    // Output.
    double uptake = NAN;        // [g/cm^3/h]
    double C_root = NAN;        // [g/cm^3]
    double D = NAN;             // [cm^2/h]
    double alpha = NAN;         // []
    double beta = NAN;          // []

    solute_uptake (Theta_val,          // []
                   Theta_sat_val,          // []
                   S_w_val,            // [h^-1]
                   C_l_val,            // [g/cm^3]
                   PotNUpt_val,        // [g/h]
                   L_val,              // [cm R/cm^3]
                   I_max_val,          // [g/cm R/h]
                   C_root_min_val,     // [g/cm^3]
                   diffusion_coef_val, // [cm^2/h]
                   volume_val,         // [cm^3]
                   Rad_val,            // [cm]
                   uptake,             // [g/cm^3/h]
                   C_root,             // [g/cm^3]
                   D,                  // [cm^2/h]
                   alpha,              // []
                   beta);              // []
    
    result.set ("uptake", uptake);
    result.set ("C_root", C_root);
    result.set ("D", D);
    result.set ("alpha", alpha);
    result.set ("beta", beta);
  }
  
  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  {
    has_value = true;
    Theta->tick (units, scope, msg);
    if (!(Theta->value (scope) > 0.0))
      {
        msg.error ("Bad Theta");
        has_value = false;
      }
    Theta_sat->tick (units, scope, msg);
    if (!(Theta_sat->value (scope) > 0.0))
      {
        msg.error ("Bad Theta_sat");
        has_value = false;
      }
    S_w->tick (units, scope, msg);
    C_l->tick (units, scope, msg);
    PotNUpt->tick (units, scope, msg);
    L->tick (units, scope, msg);
    if (!(L->value (scope) > 0.0))
      {
        msg.error ("Bad L");
        has_value = false;
      }
    I_max->tick (units, scope, msg);
    C_root_min->tick (units, scope, msg);
    diffusion_coef->tick (units, scope, msg);
    volume->tick (units, scope, msg);
    Rad->tick (units, scope, msg);
    if (!has_value)
      return;
    find_result (scope);
    ScopeMulti multi (result, scope);
    myvalue->tick (units, multi, msg);
  }
  bool missing (const Scope& scope) const 
  {
    if (!has_value
        || Theta->missing (scope) 
        || !units.can_convert (Theta->dimension (scope), Theta_dim, Theta->value (scope))
        || Theta_sat->missing (scope) 
        || !units.can_convert (Theta_sat->dimension (scope), Theta_sat_dim, Theta_sat->value (scope))
        || S_w->missing (scope)
        || !units.can_convert (S_w->dimension (scope), S_w_dim, S_w->value (scope))
        || C_l->missing (scope)
        || !units.can_convert (C_l->dimension (scope), C_l_dim, C_l->value (scope))
        || PotNUpt->missing (scope)
        || !units.can_convert (PotNUpt->dimension (scope), PotNUpt_dim,
                               PotNUpt->value (scope))
        || L->missing (scope)
        || !units.can_convert (L->dimension (scope), L_dim, L->value (scope))
        || I_max->missing (scope)
        || !units.can_convert (I_max->dimension (scope), I_max_dim, I_max->value (scope))
        || C_root_min->missing (scope)
        || !units.can_convert (C_root_min->dimension (scope), C_root_min_dim,
                               C_root_min->value (scope))
        || diffusion_coef->missing (scope)
        || !units.can_convert (diffusion_coef->dimension (scope), diffusion_coef_dim,
                               diffusion_coef->value (scope))
        || volume->missing (scope)
        || !units.can_convert (volume->dimension (scope), volume_dim, volume->value (scope))
        || Rad->missing (scope)
        || !units.can_convert (Rad->dimension (scope), Rad_dim, Rad->value (scope)))
      return true;
    ScopeMulti multi (result, scope);
    return myvalue->missing (multi);
  }
  double value (const Scope& scope) const
  { 
    ScopeMulti multi (result, scope);
    return myvalue->value (multi);
  }
  symbol dimension (const Scope& scope) const 
  {
    ScopeMulti multi (result, scope);
    return myvalue->dimension (multi);
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  {
    bool ok = true;
    TREELOG_MODEL (msg);
    Theta->initialize (units, scope, msg) || (ok = false);
    Theta_sat->initialize (units, scope, msg) || (ok = false);
    S_w->initialize (units, scope, msg) || (ok = false);
    C_l->initialize (units, scope, msg) || (ok = false);
    PotNUpt->initialize (units, scope, msg) || (ok = false);
    L->initialize (units, scope, msg) || (ok = false);
    I_max->initialize (units, scope, msg) || (ok = false);
    C_root_min->initialize (units, scope, msg) || (ok = false);
    diffusion_coef->initialize (units, scope, msg) || (ok = false);
    volume->initialize (units, scope, msg) || (ok = false);
    Rad->initialize (units, scope, msg) || (ok = false);
    ScopeMulti multi (result, scope);
    myvalue->initialize (units, multi, msg) || (ok = false);
    return ok;
  }
  bool check_it (const Number& it, const symbol it_name, const symbol it_dim,
                 const Units& units, const Scope& scope, Treelog& msg) const
  {
    TREELOG_SUBMODEL (msg, it_name);
    if (!it.check (units, scope, msg))
      return false;
    if (!units.can_convert (it.dimension (scope), it_dim))
      {
        msg.error ("Cannot convert [" + it.dimension (scope) 
                   + "] to [" + it_dim + "]");
        return false;
      }
    return true;
  }
      
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    TREELOG_MODEL (msg);
    check_it (*Theta, "Theta", Theta_dim, units, scope, msg) || (ok = false);
    check_it (*Theta_sat, "Theta_sat", Theta_sat_dim, units, scope, msg) || (ok = false);
    check_it (*S_w, "S_w", S_w_dim, units, scope, msg) || (ok = false);
    check_it (*C_l, "C_l", C_l_dim, units, scope, msg) || (ok = false);
    check_it (*PotNUpt, "PotNUpt", PotNUpt_dim, units, scope, msg) || (ok = false);
    check_it (*L, "L", L_dim, units, scope, msg) || (ok = false);
    check_it (*I_max, "I_max", I_max_dim, units, scope, msg) || (ok = false);
    check_it (*C_root_min, "C_root_min", C_root_min_dim, units, scope, msg) || (ok = false);
    check_it (*diffusion_coef, "diffusion_coef",
              diffusion_coef_dim, units, scope, msg) || (ok = false);
    check_it (*volume, "volume", volume_dim, units, scope, msg) || (ok = false);
    check_it (*Rad, "Rad", Rad_dim, units, scope, msg) || (ok = false);
    ScopeMulti multi (result, scope);
    myvalue->check (units, multi, msg) || (ok = false);
    return ok;
  }
  static void result_syntax (Frame& frame)
  {
    frame.declare ("uptake", "g/cm^3/h", Attribute::Const, "Root uptake.");
    frame.declare ("C_root", "g/cm^3", Check::non_negative (), Attribute::Const, "\
Concentration at root surface.");
    frame.declare ("D", "cm^2/h", Attribute::Const, "Diffusion thingy.");
    frame.declare ("alpha", Attribute::None (), Attribute::Const, "Thingy.");
    frame.declare ("beta", Attribute::None (), Attribute::Const, "Other thingy.");
  }
  NumberSoluteUptake (const BlockModel& al)
    : Number (al),
      units (al.units ()),
      Theta (Librarian::build_item<Number> (al, "Theta")),
      Theta_sat (Librarian::build_item<Number> (al, "Theta_sat")),
      S_w (Librarian::build_item<Number> (al, "S_w")),
      C_l (Librarian::build_item<Number> (al, "C_l")),
      PotNUpt (Librarian::build_item<Number> (al, "PotNUpt")),
      L (Librarian::build_item<Number> (al, "L")),
      I_max (Librarian::build_item<Number> (al, "I_max")),
      C_root_min (Librarian::build_item<Number> (al, "C_root_min")),
      diffusion_coef (Librarian::build_item<Number> (al, "diffusion_coef")),
      volume (Librarian::build_item<Number> (al, "volume")),
      Rad (Librarian::build_item<Number> (al, "Rad")),
      result (*Librarian::submodel_frame (result_syntax), Frame::parent_link),
      myvalue (Librarian::build_item<Number> (al, "value")),
      has_value (false)
  { }
};

static struct NumberSoluteUptakeSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSoluteUptake (al); }
  NumberSoluteUptakeSyntax()
    : DeclareModel (Number::component, "solute_uptake", 
                    "Find root nitrogen uptake.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("Theta", Number::component, "See root system.");
    frame.declare_object ("Theta_sat", Number::component, "See root system.");
    frame.declare_object ("S_w", Number::component, "See root system.");
    frame.declare_object ("C_l", Number::component, "See root system.");
    frame.declare_object ("PotNUpt", Number::component, "See root system.");
    frame.declare_object ("L", Number::component, "See root system.");
    frame.declare_object ("I_max", Number::component, "See root system.");
    frame.declare_object ("C_root_min", Number::component, "See root system.");
    frame.declare_object ("diffusion_coef", Number::component, "See root system.");
    frame.declare_object ("volume", Number::component, "See root system.");
    frame.declare_object ("Rad", Number::component, "See root system.");
    frame.declare_object ("value", Number::component, "Value to return\n\
One of 'uptake', 'C_root', 'D', 'alpha', 'beta'.");
  }
} NumberSoluteUptake_syntax;

static DeclareSubmodel solute_uptake_submodel (NumberSoluteUptake::result_syntax,
                                               "SoluteUptakeResult", "\
Result of the 'solute_uptake' number model.");

// The 'solute_uptake2' Number model

static void
solute_uptake2 (const double Theta,            // []
                const double Theta_sat,        // []
                const double S_w,              // [h^-1]
                const double C_l,              // [g/cm^3]
                const double L,                // [cm R/cm^3]
                const double K1,               // [g/cm^3]
                const double F1,               // [g/cm R/h]
                const double K2,               // [g/cm^3]
                const double F2,               // [g/cm R/h]
                const double diffusion_coef,   // [cm^2/h]
                const double volume,           // [cm^3]
                const double Rad,              // [cm]
                double& uptake,                // [g/cm^3/h]
                double& C_root,                // [g/cm^3]
                double& C1,                    // [g/cm^3]
                double& C2,                    // [g/cm^3]
                double& D,                     // [cm^2/h]
                double& alpha,                 // []
                double& beta,                  // []
                double& I_zero,                // [g/cm R/h]
                double& B_zero)                // [cm^3 W/cm R/h]
{
  daisy_assert (std::isnormal (L) && L > 0.0);
  daisy_assert (std::isnormal (Theta) && Theta > 0.0);
  
  // [cm^3 W/cm R/h] = [cm^3 W/cm^3 S/h] / [cm R/cm^3 S]
  const double q_r = S_w / L;
  const double tortuosity_factor = pow (Theta, 7.0 / 3.0) / (Theta_sat * Theta_sat); // []
  // [cm^2/h] = [cm^2/h] * [cm^3/cm^3] * [cm^3 W/cm^3 S]
  D = diffusion_coef * tortuosity_factor * Theta;
  // [] = [cm^3 W/cm R/h] / [cm^2/h]
  alpha = q_r / ( 2 * M_PI * D);
  // [] = [] / ([cm] * sqrt ([cm^-2]))
  beta = 1.0 / (Rad * sqrt (M_PI * L));
  const double beta_squared = beta * beta;

  if (alpha < 1e-10)
    {
      // [cm^3 W/cm R/h] = [cm^2/h] / ([] * log ([]/[]) - [])
      B_zero = 4.0 * M_PI * D
        / (beta_squared * log (beta_squared) / (beta_squared - 1.0)
           - 1.0);
      // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
      I_zero = B_zero * C_l;
    }
  else
    { 
      const double divisor // []
        = ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
           - (pow (beta, 2.0 - alpha) - 1.0));

      if (std::isnormal (divisor))
        {
          // [cm^3 W/cm R/h] = [cm^3 W/cm R/h]
          B_zero = q_r * (pow (beta, 2.0 - alpha) - 1.0) / divisor;
          // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
          I_zero = q_r * (beta_squared - 1.0) * (1.0 - 0.5 * alpha) * C_l / divisor;
        }
      else              
        {
          daisy_assert (approximate (alpha, 2.0));
          const double div2 // []
            = ((beta_squared - 1.0) - log (beta_squared));
          daisy_assert (std::isnormal (div2));
          // [cm^3 W/cm R/h] = [cm^3 W/cm R/h]
          B_zero = q_r * log (beta_squared) / div2;
          // [g/cm R/h] = [cm^3 W/cm R/h] * [g/cm^3 W]
          I_zero = q_r * (beta_squared - 1.0) * C_l / div2;
        }
    }

  // Solve I_zero - B_zero C = F1 C / (K1 + C) + F2 C / (K2 + C)

  // Case 1: C >> K1
  // I_zero - B_zero C ~= F1 + F2 C / (K2 + C)
  {
#if 0
    // SH solution
    const double a1 = (I_zero - F1 - B_zero * K2 - F2) / B_zero;
    const double a2 = (I_zero - F1) * K2 / B_zero;
    C1 = 0.5 * a1 + std::sqrt (0.25 * a1 * a1 + a2);
#else
    // Textbook solution.
    const double a = - B_zero;
    const double b = I_zero - F1 - F2 - B_zero * K2;
    const double c = (I_zero - F1) * K2;
    const double d = sqr (b) - 4 * a * c;
    C1 = (- b - sqrt (d)) / (2.0 * a);
  }
#endif
  // Case: C << K2
  // I_zero - B_zero C ~= F1 C / (K1 + C) + F2 C / K2
  {
#if 0
    // SH solution.
    const double a1 = (I_zero - B_zero * K1 - F1 - F2 * K1 / K2)
      / (B_zero + F2 / K2);
    const double a2 = I_zero * K1 / (B_zero + F2 / K2);
    C2 = 0.5 * a1 + std::sqrt (0.25 * a1 * a1 + a2);
#else
    // Textbook solution.
    const double a = - B_zero - F2 / K2;
    const double b = I_zero - B_zero * K1 - F1 - F2 * K1 / K2;
    const double c = I_zero * K1;
    const double d = sqr (b) - 4 * a * c;
    C2 = (- b - sqrt (d)) / (2.0 * a);
  }
#endif

#if 0
  const double C_lim = std::exp (0.5 * (std::log (K1) + std::log (K2)));
  C_root = (0.5 * (C1 + C2) > C_lim) ? C1 : C2; // [g/cm^3]
#endif

  // Which assumption is best? C1 >> K1 or C2 << K2?
  C_root = (C1 / K1 > K2 / C2) ? C1 : C2;
#if 1
  auto fun = [=] (const double C) -> double
    {
      return -I_zero + B_zero * C + F1 * C / (K1 + C) + F2 * C / (K2 + C);
    };
  auto derived = [=] (const double C) -> double
    {
      return B_zero + F1 * K1 / sqr (K1 + C) + F2 * K2 / sqr (K2 + C);
    };

  C_root = Newton (0.0, fun, derived);
#endif
  // [g/cm^3 S/h] = [cm R/cm^3 S] * ([g/cm R/h] - [cm^3 W/cm R/h] * [g/cm^3 W])
  uptake = L * (I_zero - B_zero * C_root);
}

struct NumberSoluteUptake2 : public Number
{
  const Units& units;

  // Parameters.
  const std::unique_ptr<Number> Theta;
  const std::unique_ptr<Number> Theta_sat;
  const std::unique_ptr<Number> S_w;
  const std::unique_ptr<Number> C_l;
  const std::unique_ptr<Number> L;
  const std::unique_ptr<Number> K1;
  const std::unique_ptr<Number> F1;
  const std::unique_ptr<Number> K2;
  const std::unique_ptr<Number> F2;
  const std::unique_ptr<Number> diffusion_coef;
  const std::unique_ptr<Number> volume;
  const std::unique_ptr<Number> Rad;

  const symbol Theta_dim = "";
  const symbol Theta_sat_dim = "";
  const symbol S_w_dim = "h^-1";
  const symbol C_l_dim = "g/cm^3";
  const symbol L_dim = "cm R/cm^3";
  const symbol K1_dim = "g/cm^3";
  const symbol F1_dim = "g/cm R/h";
  const symbol K2_dim = "g/cm^3";
  const symbol F2_dim = "g/cm R/h";
  const symbol diffusion_coef_dim = "cm^2/h";
  const symbol volume_dim = "cm^3";
  const symbol Rad_dim = "cm";

  // Value.
  FrameSubmodelValue result;
  const std::unique_ptr<Number> myvalue;
  bool has_value;
  
  void find_result (const Scope& scope)
  {
    // Input.
    const double Theta_val
      = units.convert (Theta->dimension (scope), Theta_dim, Theta->value (scope));
    const double Theta_sat_val
      = units.convert (Theta_sat->dimension (scope), Theta_sat_dim, Theta_sat->value (scope));
    const double S_w_val
      = units.convert (S_w->dimension (scope), S_w_dim, S_w->value (scope));
    const double C_l_val
      = units.convert (C_l->dimension (scope), C_l_dim, C_l->value (scope));
    const double L_val
      = units.convert (L->dimension (scope), L_dim, L->value (scope));
    const double K1_val
      = units.convert (K1->dimension (scope), K1_dim, K1->value (scope));
    const double F1_val
      = units.convert (F1->dimension (scope), F1_dim, F1->value (scope));
    const double K2_val
      = units.convert (K2->dimension (scope), K2_dim, K2->value (scope));
    const double F2_val
      = units.convert (F2->dimension (scope), F2_dim, F2->value (scope));
    const double diffusion_coef_val
      = units.convert (diffusion_coef->dimension (scope), diffusion_coef_dim,
                       diffusion_coef->value (scope));
    const double volume_val
      = units.convert (volume->dimension (scope), volume_dim, volume->value (scope));
    const double Rad_val
      = units.convert (Rad->dimension (scope), Rad_dim, Rad->value (scope));

    // Output.
    double uptake = NAN;        // [g/cm^3/h]
    double C_root = NAN;        // [g/cm^3]
    double C1 = NAN;            // [g/cm^3]
    double C2 = NAN;            // [g/cm^3]
    double D = NAN;             // [cm^2/h]
    double alpha = NAN;         // []
    double beta = NAN;          // []
    double A = NAN;             // [g/cm R/h]
    double B = NAN;             // [cm^3 W/cm R/h]

    solute_uptake2 (Theta_val,          // []
                    Theta_sat_val,          // []
                    S_w_val,            // [h^-1]
                    C_l_val,            // [g/cm^3]
                    L_val,              // [cm R/cm^3]
                    K1_val,             // [g/cm^3]
                    F1_val,             // [g/cm R/h]
                    K2_val,             // [g/cm^3]
                    F2_val,             // [g/cm R/h]
                    diffusion_coef_val, // [cm^2/h]
                    volume_val,         // [cm^3]
                    Rad_val,            // [cm]
                    uptake,             // [g/cm^3/h]
                    C_root,             // [g/cm^3]
                    C1,                 // [g/cm^3]
                    C2,                 // [g/cm^3]
                    D,                  // [cm^2/h]
                    alpha,              // []
                    beta,               // []
                    A,                  // [g/cm R/h]
                    B);                 // [cm^3 W/cm R/h]
    
    result.set ("uptake", uptake);
    result.set ("C_root", C_root);
    result.set ("C1", C1);
    result.set ("C2", C2);
    result.set ("D", D);
    result.set ("alpha", alpha);
    result.set ("beta", beta);
    result.set ("A", A);
    result.set ("B", B);
    
  }
  
  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  {
    has_value = true;
    Theta->tick (units, scope, msg);
    if (!(Theta->value (scope) > 0.0))
      {
        msg.error ("Bad Theta");
        has_value = false;
      }
    Theta_sat->tick (units, scope, msg);
    if (!(Theta_sat->value (scope) > 0.0))
      {
        msg.error ("Bad Theta_sat");
        has_value = false;
      }
    S_w->tick (units, scope, msg);
    C_l->tick (units, scope, msg);
    L->tick (units, scope, msg);
    if (!(L->value (scope) > 0.0))
      {
        msg.error ("Bad L");
        has_value = false;
      }
    K1->tick (units, scope, msg);
    F1->tick (units, scope, msg);
    K2->tick (units, scope, msg);
    F2->tick (units, scope, msg);
    diffusion_coef->tick (units, scope, msg);
    volume->tick (units, scope, msg);
    Rad->tick (units, scope, msg);
    if (!has_value)
      return;
    find_result (scope);
    ScopeMulti multi (result, scope);
    myvalue->tick (units, multi, msg);
  }
  bool missing (const Scope& scope) const 
  {
    if (!has_value
        || Theta->missing (scope) 
        || !units.can_convert (Theta->dimension (scope), Theta_dim, Theta->value (scope))
        || Theta_sat->missing (scope) 
        || !units.can_convert (Theta_sat->dimension (scope), Theta_sat_dim, Theta_sat->value (scope))
        || S_w->missing (scope)
        || !units.can_convert (S_w->dimension (scope), S_w_dim, S_w->value (scope))
        || C_l->missing (scope)
        || !units.can_convert (C_l->dimension (scope), C_l_dim, C_l->value (scope))
        || L->missing (scope)
        || !units.can_convert (L->dimension (scope), L_dim, L->value (scope))
        || K1->missing (scope)
        || !units.can_convert (K1->dimension (scope), K1_dim,
                               K1->value (scope))
        || F1->missing (scope)
        || !units.can_convert (F1->dimension (scope), F1_dim,
                               F1->value (scope))
        || K2->missing (scope)
        || !units.can_convert (K2->dimension (scope), K2_dim,
                               K2->value (scope))
        || F2->missing (scope)
        || !units.can_convert (F2->dimension (scope), F2_dim,
                               F2->value (scope))
        || diffusion_coef->missing (scope)
        || !units.can_convert (diffusion_coef->dimension (scope), diffusion_coef_dim,
                               diffusion_coef->value (scope))
        || volume->missing (scope)
        || !units.can_convert (volume->dimension (scope), volume_dim, volume->value (scope))
        || Rad->missing (scope)
        || !units.can_convert (Rad->dimension (scope), Rad_dim, Rad->value (scope)))
      return true;
    ScopeMulti multi (result, scope);
    return myvalue->missing (multi);
  }
  double value (const Scope& scope) const
  { 
    ScopeMulti multi (result, scope);
    return myvalue->value (multi);
  }
  symbol dimension (const Scope& scope) const 
  {
    ScopeMulti multi (result, scope);
    return myvalue->dimension (multi);
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  {
    bool ok = true;
    TREELOG_MODEL (msg);
    Theta->initialize (units, scope, msg) || (ok = false);
    Theta_sat->initialize (units, scope, msg) || (ok = false);
    S_w->initialize (units, scope, msg) || (ok = false);
    C_l->initialize (units, scope, msg) || (ok = false);
    L->initialize (units, scope, msg) || (ok = false);
    K1->initialize (units, scope, msg) || (ok = false);
    F1->initialize (units, scope, msg) || (ok = false);
    K2->initialize (units, scope, msg) || (ok = false);
    F2->initialize (units, scope, msg) || (ok = false);
    diffusion_coef->initialize (units, scope, msg) || (ok = false);
    volume->initialize (units, scope, msg) || (ok = false);
    Rad->initialize (units, scope, msg) || (ok = false);
    ScopeMulti multi (result, scope);
    myvalue->initialize (units, multi, msg) || (ok = false);
    return ok;
  }
  bool check_it (const Number& it, const symbol it_name, const symbol it_dim,
                 const Units& units, const Scope& scope, Treelog& msg) const
  {
    TREELOG_SUBMODEL (msg, it_name);
    if (!it.check (units, scope, msg))
      return false;
    if (!units.can_convert (it.dimension (scope), it_dim))
      {
        msg.error ("Cannot convert [" + it.dimension (scope) 
                   + "] to [" + it_dim + "]");
        return false;
      }
    return true;
  }
      
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    TREELOG_MODEL (msg);
    check_it (*Theta, "Theta", Theta_dim, units, scope, msg) || (ok = false);
    check_it (*Theta_sat, "Theta_sat", Theta_sat_dim, units, scope, msg) || (ok = false);
    check_it (*S_w, "S_w", S_w_dim, units, scope, msg) || (ok = false);
    check_it (*C_l, "C_l", C_l_dim, units, scope, msg) || (ok = false);
    check_it (*L, "L", L_dim, units, scope, msg) || (ok = false);
    check_it (*K1, "K1", K1_dim, units, scope, msg) || (ok = false);
    check_it (*F1, "F1", F1_dim, units, scope, msg) || (ok = false);
    check_it (*K2, "K2", K2_dim, units, scope, msg) || (ok = false);
    check_it (*F2, "F2", F2_dim, units, scope, msg) || (ok = false);
    check_it (*diffusion_coef, "diffusion_coef",
              diffusion_coef_dim, units, scope, msg) || (ok = false);
    check_it (*volume, "volume", volume_dim, units, scope, msg) || (ok = false);
    check_it (*Rad, "Rad", Rad_dim, units, scope, msg) || (ok = false);
    ScopeMulti multi (result, scope);
    myvalue->check (units, multi, msg) || (ok = false);
    return ok;
  }
  static void result_syntax (Frame& frame)
  {
    frame.declare ("uptake", "g/cm^3/h", Attribute::Const, "Root uptake.");
    frame.declare ("C_root", "g/cm^3", Check::non_negative (), Attribute::Const, "\
Concentration at root surface.");
    frame.declare ("C1", "g/cm^3", Check::non_negative (), Attribute::Const, "\
Concentration at root surface (mechanism 1 dominating).");
    frame.declare ("C2", "g/cm^3", Check::non_negative (), Attribute::Const, "\
Concentration at root surface (mechanism 2 dominating).");
    frame.declare ("D", "cm^2/h", Attribute::Const, "Diffusion thingy.");
    frame.declare ("alpha", Attribute::None (), Attribute::Const, "Thingy.");
    frame.declare ("beta", Attribute::None (), Attribute::Const, "Other thingy.");
    frame.declare ("A", "g/cm R/h", Attribute::Const, "Uptake thingy.");
    frame.declare ("B", "cm^3 W/cm R/h", Attribute::Const, "\
'C_root' dependent uptake thingy.");
  }
  NumberSoluteUptake2 (const BlockModel& al)
    : Number (al),
      units (al.units ()),
      Theta (Librarian::build_item<Number> (al, "Theta")),
      Theta_sat (Librarian::build_item<Number> (al, "Theta_sat")),
      S_w (Librarian::build_item<Number> (al, "S_w")),
      C_l (Librarian::build_item<Number> (al, "C_l")),
      L (Librarian::build_item<Number> (al, "L")),
      K1 (Librarian::build_item<Number> (al, "K1")),
      F1 (Librarian::build_item<Number> (al, "F1")),
      K2 (Librarian::build_item<Number> (al, "K2")),
      F2 (Librarian::build_item<Number> (al, "F2")),
      diffusion_coef (Librarian::build_item<Number> (al, "diffusion_coef")),
      volume (Librarian::build_item<Number> (al, "volume")),
      Rad (Librarian::build_item<Number> (al, "Rad")),
      result (*Librarian::submodel_frame (result_syntax), Frame::parent_link),
      myvalue (Librarian::build_item<Number> (al, "value")),
      has_value (false)
  { }
};

static struct NumberSoluteUptake2Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSoluteUptake2 (al); }
  NumberSoluteUptake2Syntax()
    : DeclareModel (Number::component, "solute_uptake2", 
                    "Find root nitrogen uptake.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("Theta", Number::component, "See root system.");
    frame.declare_object ("Theta_sat", Number::component, "See root system.");
    frame.declare_object ("S_w", Number::component, "See root system.");
    frame.declare_object ("C_l", Number::component, "See root system.");
    frame.declare_object ("L", Number::component, "See root system.");
    frame.declare_object ("F1", Number::component, "See root system.");
    frame.declare_object ("K1", Number::component, "See root system.");
    frame.declare_object ("F2", Number::component, "See root system.");
    frame.declare_object ("K2", Number::component, "See root system.");
    frame.declare_object ("diffusion_coef", Number::component, "See root system.");
    frame.declare_object ("volume", Number::component, "See root system.");
    frame.declare_object ("Rad", Number::component, "See root system.");
    frame.declare_object ("value", Number::component, "Value to return\n\
One of 'uptake', 'C_root', 'C1', 'C2', 'D', 'alpha', 'beta', 'A', 'B'.");
  }
} NumberSoluteUptake2_syntax;

static DeclareSubmodel solute_uptake2_submodel (NumberSoluteUptake2::result_syntax,
                                                "SoluteUptake2Result", "\
Result of the 'solute_uptake2' number model.");

// solupt.C ends here
