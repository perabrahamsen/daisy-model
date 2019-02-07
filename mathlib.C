// mathlib.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "mathlib.h"
#include "assertion.h"
#include <sstream>

// #if !defined (__unix)
// Apparently not provided on Borland C++ 5.0 or 5.5.
// double cbrt (double x) 
// { return pow (x, 1.0/3.0); }
// #endif

void 
first_order_change (const double old_storage /* [M] */,
                    const double absolute_input_rate /* [M/T] */,
                    const double relative_loss_rate /* [T^-1] */,
                    const double dt /* [T] */,
                    double& new_storage /* [M] */,
                    double& absolute_loss_rate /* [M/T] */)
{
  // S (t) = storage

  const double I = absolute_input_rate;
  const double L = relative_loss_rate;
  const double S0 = old_storage; // [M]
  
  if (std::fabs (L) < 1e-99)
    {
      const double S1 = S0 + I * dt;
      new_storage = S1;
      absolute_loss_rate = 0.0;
      return;
    }
  
  // The equation for the system is
  //
  //    dS/dt = I - S L
  //
  // The solution is
  // 
  //    S (t) = c1 exp (-L t) + I/L
  // 
  // We insert S (0) in order to find c1.
  //
  //    S (0) = c1 + I/L => c1 = S (0) - I/L

  const double c1 = S0 - I/L;   // [M]
  
  // Knowing c1, we can find the content at the end of the timestep.
  const double S1 = c1 * std::exp (-L * dt) + I/L; // [M]
  
  // The gain is simply the constant input multiplied with the timestep.
  const double gain = I * dt;

  // We find loss from mass balance.
  const double loss = S0 - S1 + gain;

  if (loss < 0.0)
    {
      std::ostringstream tmp;
      tmp << "I = " << I;
      tmp << "; L = " << L;
      tmp << "; dt = " << dt;
      tmp << "; S0 = " << S0;
      tmp << "; S1 = " << S1;
      tmp << "; loss = " << loss;
      tmp << "; gain = " << gain;
      tmp << "\nS1 - S0 = " << S1 - S0;
      tmp << "; gain - loss = " << gain - loss;
      daisy_warning (tmp.str ());
    }
  
  if (S1 >= 0)
    {
      new_storage = S1;
      absolute_loss_rate = loss / dt;
    }
  else
    {
      // Decrease loss to get a non-negative solution.
      new_storage = 0.0;
      absolute_loss_rate = (loss + S1) / dt;
    }
}

// See _Computational_Techniques_for_Differential_Equations page 616.
void
tridia (int from,
	const unsigned int N,
	const std::vector<double>& a,
	const std::vector<double>& b, 
	const std::vector<double>& c,
	const std::vector<double>& d,
	std::vector<double>::iterator x)
{
  daisy_assert (a.size () >= N);
  daisy_assert (b.size () >= N);
  daisy_assert (c.size () >= N);
  daisy_assert (d.size () >= N);

  static std::vector<double> y;
  static std::vector<double> beta;
  
  if (y.size() < N)
    {
      daisy_assert (beta.size () == y.size ());
      y.insert (y.end(), from + N - y.size(), 0.0);
      beta.insert (beta.end(), from + N - beta.size(), 0.0);
    }
  
  beta[from] = b[from];
  y[from] = d[from];

  // Forward substitution.
  for (unsigned int i = from + 1; i < N; i++)
    {
      double amult = a[i] / beta[i - 1];
      beta[i] = b[i] - amult * c[i - 1];
      y[i] = d[i] - amult * y[i - 1];
    }
  // Backward substitution.
  x[N - 1] = y[N - 1] / beta[N - 1];
  for (int i = N - 2; i >= from; i--)
    x[i] = (y[i] - c[i] * x[i + 1]) / beta[i];
}

inline double pow2 (double x)
{ return x * x; }

inline double pow3 (double x)
{ return x * x * x; }

double
single_positive_root_of_cubic_equation (double a, double b, double c, double d)
{
  const double p = 1.0 / 3.0 * ( - 1.0/3.0 * pow2 (b / a) + c / a);
  const double q = 1.0 / 2.0 * ( 2.0 / 27.0 * pow3 (b / a)
				 - 1.0/3.0 * b * c / pow2 (a)
				 + d / a);
  const double r = pow2 (q) + pow3 (p);
  
  if (r >= 0)
    {
      const double sqrt_r = sqrt (r);
      return cbrt (-q + sqrt_r) + cbrt (-q - sqrt_r) - b / (3 * a);
    }
  else
    {
      const double psi = acos ( -q  / sqrt (pow3 (-p)));

      // A bit manual common subexpression removal.
      const double sqrt_mp2 = 2 * sqrt (-p);
      const double psi_3 = psi / 3.0;
      const double pi_3 = M_PI / 3.0;
      const double b_3a = b / (3 * a);

      const double y1 = sqrt_mp2 * cos (psi_3) - b_3a;
      const double y2 = - sqrt_mp2 * cos (psi_3 + pi_3) - b_3a;
      const double y3 = - sqrt_mp2 * cos (psi_3 - pi_3) - b_3a;
      
      if (y1 >= 0)
	{
	  daisy_assert (y2 < 0 && y3 < 0);
	  return y1;
	}
      if (y2 >= 0)
	{
	  daisy_assert (y3 < 0);
	  return y2;
	}
      if (y3 >= 0)
	return y3;
      
      
      daisy_assert (y3 >= 0);
      return y3;
    }
}

double 
single_positive_root_of_square_equation (double a, double b, double c)
{
  const double D = sqrt (pow2 (b) - 4.0 * a * c);
  
  // One solution.
  if (iszero (D))
    {
      const double x = -b / (2.0 * a);
      // Must be positive.
      daisy_assert (x >= 0.0);
      return x;
    }

  // Two solutions.
  const double x1 = (-b - D) / (2.0 * a);
  const double x2 = (-b + D) / (2.0 * a);

  // Return the positive solution.
  if (x1 > 0.0)
    {
      daisy_assert (x2 <= 0.0);
      return x1;
    }
  else
    {
      daisy_assert (x2 >= 0.0);
      return x2;
    }
}

bool approximate (const double a, const double b, const double noise)
{
  if (fabs (a) < 1.0e-100)
    return fabs (b) < 1.0e-100;
  return (iszero (b) ? iszero (a) : fabs (a / b - 1.0) < noise);
}

bool balance (const double oldval, const double newval, const double growth,
              const double noise)
{
  return approximate (newval - oldval, growth, noise)
    || approximate (oldval + growth, newval, noise);
}

double halftime_to_rate (double halftime)
{ return M_LN2 / halftime; }

double rate_to_halftime (double rate)
{ return M_LN2 / rate; }

double fraction_within (const double from, const double to, 
                        const double begin, const double end)
{ 
  daisy_assert (to > from);
  daisy_assert (end > begin);

  if (from >= end)
    // Fully after interval.
    return 0.0;
  if (to <= begin)
    // Fully before interval.
    return 0.0;

  if (to <= end)
    {
      if (from >= begin)
        // Fully within interval.
        return 1.0;
      
      // Overlaps start of interval.
      const double width = to - from;
      const double overlap = to - begin;
      daisy_assert (overlap > 0.0);
      daisy_assert (width > 0.0);
      daisy_soft_assert (width >= overlap);
      return overlap / width;
    }
  if (from >= begin)
    {
      // Overlaps end of interval.
      const double width = to - from;
      const double overlap = end - from;
      daisy_assert (overlap > 0.0);
      daisy_assert (width > 0.0);
      daisy_soft_assert (width >= overlap);
      return overlap / width;
    }

  // Interval is fully within.
  const double width = to - from;
  const double overlap = end - begin;
  daisy_assert (overlap > 0.0);
  daisy_soft_assert (width >= overlap);
  return overlap / width;
}

// mathlib.C ends here.
