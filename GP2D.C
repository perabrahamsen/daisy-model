// GP2D.C -- Gerwitz and Page model extended for row crops.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
// Copyright 2011 KU.
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

#include "GP2D.h"
#include "treelog.h"
#include "iterative.h"
#include "mathlib.h"
#include "assertion.h"
#include <sstream>
#include <algorithm>

// GP1D

struct GP1D::InvW  // LogProduct
{
  static double derived (const double W)
  { return std::exp (W) + W * std::exp (W); }
  const double k;
  double operator()(const double W) const
  { return W * std::exp (W) - k; }
  InvW (const double k_)
    : k (k_)
  { }
};

bool
GP1D::set_dynamic (const double SoilDepth, const double CropDepth, 
                   const double WRoot, const int debug, Treelog& msg)
{
  // Check input.
  daisy_assert (CropDepth > 0);
  daisy_assert (WRoot > 0);

  static const double m_per_cm = 0.01;

  // Root dry matter.
  const double M_r = WRoot /* [g/m^2] */ * m_per_cm * m_per_cm; // [g/cm^2]

  // Specific root length.
  const double S_r = SpRtLength /* [m/g] */ / m_per_cm; // [cm/g]

  // Root length (\ref{eq:root_length} Eq 3).
  const double l_r = S_r * M_r;	// [cm/cm^2]
  
  // Potential depth.
  const double d_c = CropDepth;	// [cm]

  // Soil depth.
  const double d_s = SoilDepth;	// [cm]

  // Actual depth.
  d_a = std::min (d_c, d_s); // [cm]

  // Minimum density.
  const double L_m = DensRtTip;	// [cm/cm^3]

  // Minimum root length in root zone.
  const double l_m = L_m * d_c;	// [cm/cm^2]

  // Minimum root length as fraction of total root length.
  const double D = l_m / l_r;	// []

  // Identity: W = - a d_c
  // Solve: W * exp (W) = -D (\ref{eq:Lambert} Eq 6):
  // IW (W) = W * exp (W) - D
  // Since we know D, we can construct the function f.
  const InvW f (-D);		// [] -> []

  // ... And the derived df/dW

  // The function f has a local minimum at -1.
  const double W_min = -1;	   // []

  // There is no solution when -D is below the value at W_min.
  const double D_max = -W_min * std::exp (W_min); // []

  // Too little root mass to fill the root zone.
  if (D > D_max)
    {
      // We warn once.
      if (debug > 0)
	{
	  // warn_about_to_little_root = false;
	  std::ostringstream tmp;
	  tmp << "Min ratio is " << D << ", max is " << D_max << ".\n"
	      << "Not enough root mass to fill root zone.\n"
	      << "Using uniform distribution.";
	  msg.warning (tmp.str ());
	}
      return false;
    }

  // There are two solutions to f (W) = 0, we are interested in the
  // one for W < W_min.  We start with a guess of -2.
  const double W = Newton (-2.0, f, f.derived);
  
  // Check the solution.
  const double f_W = f (W);
  if (!approximate (D, D + f_W))
    {
      if (debug > 0)
        {
          std::ostringstream tmp;
          tmp << "Newton's methods did not converge.\n";
          tmp << "W = " << W << ", f (W) = " << f_W << ", D = " << D << "\n";
          (void) Newton (-2.0, f, f.derived, &tmp);
          tmp << "Using uniform distribution.";
          msg.error (tmp.str ());
        }
      return false;
    }

  // Find a from W (\ref{eq:a-solved} Eq 7):
  a = -W / d_c;			// [cm^-1]
  // and L0 from a (\ref{eq:L0-found} Eq 8):
  L0 = L_m  * std::exp (a * d_c); // [cm/cm^3]

  // Then solve \ref{eq:scale-factor} Eq. 13.  With WolframAlpha we get:
  if (d_a > 0.0)
    {
      const double int_0_da_Lz = L0 * (1.0 - std::exp (-a * d_a)) / a;
      kstar = l_r / int_0_da_Lz;
    }
  else
    kstar = 1.0;

  return true;
}

double 
GP1D::density (const double z /* [cm] */) const
{
  daisy_assert (z >= 0.0);      // Positive depth.

  if (d_a > 0.0 && z > d_a)
    return 0.0;

  // \ref{eq:g+p} Eq 1.
  const double Lz = L0 * std::exp (-a * z);

  // \ref{eq:limited-depth} Eq. 12
  return kstar * Lz;
}

GP1D::GP1D (const double drt, const double srl)
  : DensRtTip (drt),
    SpRtLength (srl),
    a (NAN),
    L0 (NAN),
    d_a (NAN),
    kstar (NAN)
{ }

// GP2D

struct GP2D::InvQ // LogSquare
{
  static double derived (const double Q)
  { return 2.0 * Q * std::exp (Q) + sqr (Q) * std::exp (Q); }
  const double k;
  double operator()(const double Q) const
  { return sqr (Q) * std::exp (Q) - k; }
  InvQ (const double k_)
    : k (k_)
  { }
};

bool
GP2D::set_dynamic (const double SoilDepth, const double CropDepth, 
                   const double CropWidth, const double WRoot, 
                   const int debug, Treelog& msg)
{
  // Check input.
  daisy_assert (CropDepth > 0);
  daisy_assert (WRoot > 0);

  static const double m_per_cm = 0.01;

  // Row distance.
  const double R = row_distance; /* [cm] */ 

  // Root dry matter per area.
  const double M_r = WRoot /* [g/m^2] */ * m_per_cm * m_per_cm; // [g/cm^2]

  // Specific root length.
  const double S_r = SpRtLength /* [m/g] */ / m_per_cm; // [cm/g]

  // Root length (\ref{eq:root_length} Eq 3).
  const double l_r = S_r * M_r;	// [cm/cm^2]

  // Root length per half row.
  const double l_R = l_r * 0.5 * R; // [cm/cm]

  // Horizontal radius of root system.
  const double w_c = CropWidth;	// [cm]

  // Potential depth.
  const double d_c = CropDepth;	// [cm]

  // Soil depth.
  const double d_s = SoilDepth;	// [cm]

  // Actual depth.
  d_a = std::min (d_c, d_s); // [cm]

  // Minimum density.
  const double L_m = DensRtTip;	// [cm/cm^3]

  // Minimum root length in root zone.
  const double l_m = L_m * d_c;	// [cm/cm^2]

  // Minimum root length as fraction of total root length.
  const double D = l_m / l_r;	// []

  // Identity: Q = -a_z d_c
  // Solve: Q^2 * exp (Q) = D (\eqref{eq:logsquare} Eq 23):
  // IQ (Q) = Q^2 * exp (Q) + D
  // Since we know D, we can construct the function g.
  const InvQ g (D);		// [] -> []

  // The function g has a local maximum at -2.
  const double Q_max = -2;	   // []

  // There is no solution when D is above the value at Q_max.
  const double D_max = sqr (Q_max) * std::exp (Q_max); // []

  // Too little root mass to fill the root zone.
  if (D > D_max)
    {
      if (debug > 0)
        {
          std::ostringstream tmp;
          tmp << "Min ratio is " << D << ", max is " << D_max << ".\n"
              << "Not enough root mass to fill root zone.";
          msg.warning (tmp.str ());
        }
      return false;
    }

  // There are three solutions to g (Q) = 0, we are interested in the
  // one for Q < Q_max.  We start with a guess of -3.
  const double Q = Newton (-3.0, g, g.derived);
  
  // Check the solution.
  const double g_Q = g (Q);
  if (!approximate (D, D - g_Q))
    {
      if (debug > 0)
        {
          std::ostringstream tmp;
          tmp << "Newton's methods did not converge.\n";
          tmp << "Q = " << Q << ", g (Q) = " << g_Q << ", D = " << D << "\n";
          if (debug > 1)
            (void) Newton (-3.0, g, g.derived, &tmp);
          msg.error (tmp.str ());
        }
      return false;
    }

  // Find a_z from Q (\ref{eq:Qaz} Eq 22):
  a_z = -Q / d_c;		// [cm^-1]

  // Find a_x from a_z (\ref{eq:aztoax} Eq 20):
  a_x = (d_c / w_c) *  a_z;	// [cm^-1]

  // and L00 from a (\ref{{eq:root-integral2} Eq 16):
  L00 = l_R  * a_z * a_x;	// [cm/cm^3]

  // Redistribute roots from outside root zone.

  // We first convert to 1D.

  // \ref{eq:azisa} Eq. 30
  const double a = a_z;
  // \ref{eq:L0L00} Eq. 32
  const double L0 = (2.0 * L00) / (a_x * R);
  
  // Then solve \ref{eq:scale-factor} Eq. 13.  With WolframAlpha we get:
  if (d_a > 0.0)
    {
      const double int_0_da_Lz = L0 * (1.0 - std::exp (-a * d_a)) / a;
      kstar = l_r / int_0_da_Lz;
    }
  else
    kstar = 1.0;
  
  if (debug > 2)
    {
      std::ostringstream tmp;
      tmp << "R =\t" << R << "\tcm\tRow distance\n"
          << "M_r =\t" << M_r << "\tg/cm^2\tRoot dry matter per area\n"
          << "S_r =\t" << S_r << "\tcm/g\tSpecific root length\n"
          << "l_r =\t" << l_r << "\tcm/cm^2\tRoot length\n"
          << "l_R =\t" << l_R << "\tcm/cm\tRoot length per half row\n"
          << "w_c =\t" << w_c << "\tcm\tHorisontal radius of root system\n"
          << "d_c =\t" << d_c << "\tcm\tPotential depth\n"
          << "d_s =\t" << d_s << "\tcm\tSoil depth\n"
          << "d_a =\t" << d_a << "\tcm\tActual depth\n"
          << "L_m =\t" << L_m << "\tcm/cm^3\tMinimum density\n"
          << "l_m =\t" << l_m << "\tcm/cm^2\tMinimum root length in root zone\n"
          << "D =\t" << D << "\t\t\
Minimum root length as fraction of total root length\n"
          << "Q_max =\t" << Q_max << "\t\t\
The function g has a local maximum at -2\n"
          << "D_max =\t" << D_max << "\t\t\
There is no solution when D is above the value at Q_max\n"
          << "Q =\t" << Q << "\t\tSolution to 'g (Q) = 0'\n"
          << "g_Q =\t" << g_Q << "\t\tg (Q)\n"
          << "a_z =\t" << a_z << "\tcm^-1\tVertical decrease\n"
          << "a_x =\t" << a_x << "\tcm^-1\tHorisontal decrease\n"
          << "L00 =\t" << L00 << "\tcm/cm^3\tDensity at (0, 0)\t\n"
          << "kstar =\t" << kstar << "\t\tScale factor";
      msg.message (tmp.str ());
    }
  return true;
}

double 
GP2D::density (double x /* [cm] */, const double z /* [cm] */) const
{
  daisy_assert (z >= 0.0);      // Positive depth.

  if (d_a > 0.0 && z > d_a)
    return 0.0;

  // x should be relative to row position.
  x -= row_position;      

  const double row_center = row_distance / 2.0; // Row center. [cm]

  // While x is on the other size of the right center, go one row to the
  // left.
  while (x > row_center)
    x -= row_distance;

  // While x is on the other size of the left center, go one row to the
  // right.
  while (x < -row_center)
    x += row_distance;

  // Make it positive (mirror in row).
  x = std::fabs (x);
  
  // We should now be between the crop row, and the right center.
  daisy_assert (x <= row_center);
      
  // \ref{eq:Lzxstar-solved} Eq 27
  const double Lzxstar = L00 * std::exp (-a_z * z) 
    * (std::exp (-a_x * x) + std::exp (-a_x * (row_distance - x)))
    / (1.0 - std::pow (1.0/std::exp (1.0), a_x * row_distance));

  // \ref{eq:limited-depth} Eq. 12
  return kstar * Lzxstar;
}

GP2D::GP2D (const double rp, const double rd, 
            const double drt, const double srl)
  : row_position (rp),
    row_distance (rd),
    DensRtTip (drt),
    SpRtLength (srl),
    a_z (NAN),
    a_x (NAN),
    L00 (NAN),
    d_a (NAN),
    kstar (NAN)
{ }

// rootdens_GP2D.C ends here.
