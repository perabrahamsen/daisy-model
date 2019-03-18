// iterative.C --- Generic iterative root finding algorithms.
// 
// Copyright 2009 Per Abrahamsen and KU.
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

#include "iterative.h"
#include "assertion.h"
#include "treelog.h"
#include "mathlib.h"
#include <sstream>
#include <algorithm>
#include <limits>

// The 'Fixpoint' class.

double 
Fixpoint::find_epsilon () const
{ return max_distance ().size (); }

double
Fixpoint::diff (const Value& A, const Value& B) const
{
  const Value& max = max_distance ();
  const size_t size = max.size ();
  daisy_assert (A.size () == size);
  daisy_assert (B.size () == size);

  double sum = 0.0;
  for (size_t i = 0; i < size; i++)
    {
      daisy_assert (max[i] > 0.0);
      const double d = (A[i] - B[i]) / max[i];
      sum += d * d;
    }
  return sum;
}

Fixpoint::Value
Fixpoint::average (const Value& A, const Value& B) const
{
  const size_t size = A.size ();
  daisy_assert (B.size () == size);
  Value result;
  for (size_t i = 0; i < size; i++)
    result.push_back ((A[i] + B[i]) / 2.0);
  daisy_assert (result.size () == size);
  return result;
}

Fixpoint::Value
Fixpoint::solve (Treelog& msg)
{
  TREELOG_SUBMODEL (msg, "Fixpoint");

  const double epsilon = find_epsilon ();
  Value A = initial_guess ();
  Value B = f (A, msg);
  double err_A = diff (A, B);
  int iterations_used = 0;

#if 0
  std::ostringstream tmp;
  tmp << " epsilon = " << epsilon;
  const size_t size = A.size ();
  tmp << ", err_A = " << err_A;
  for (size_t i = 0; i < size; i++)
    tmp << "; " << i << ": A " << A[i] << " B " << B[i];
  msg.message (tmp.str ());
#endif

  while (err_A > epsilon)
    {
      // Find a better guess.
      Value C;
      double err_B;      
      for (;;)
        {
          C = f (B, msg);
          err_B = diff (B, C);
          
          std::ostringstream tmp;
          tmp << iterations_used
              << ": err_A = " << err_A << ", err_B = " << err_B;
          const size_t size = A.size ();
          for (size_t i = 0; i < size; i++)
            tmp << "; " << i << ": A " << A[i] << " B " << B[i]
                << " C " << C[i];
          msg.debug (tmp.str ());

          iterations_used++;
          if (iterations_used > max_iteration)
            throw "Too many iterations";

          // If diff (A, C) < diff (A, B) we are likely flip-flopping.
          const double err_X = diff (A, C);
          if (err_B < err_A && err_B < 0.9 * err_X)
            // New guess is better, use it.
            {
              A = B;
              B = C;
              err_A = err_B;
              break;
            }
          // No such luck, find a middle ground.
          B = average (A,  B);

          // Close enough.
          const double new_A = diff (A, B);
          if (new_A < epsilon * 0.1)
            {
              C = f (B, msg);
              const double new_B = diff (B, C);
              if (new_B > epsilon)
                {
                  std::ostringstream tmp;
                  tmp << "Unstable solution; " 
                      << iterations_used << " iterations used";
                  const size_t size = A.size ();
                  tmp << ", err_A = " << new_A << ", err_B = " << new_B;
                  for (size_t i = 0; i < size; i++)
                    tmp << "; " << i << ": A " << A[i] << " B " << B[i]
                        << " C " << C[i];
                  msg.debug (tmp.str ());
                  throw "Unstable solution";
                }
              return A;
            }
        }
    }
  return A;
}

Fixpoint::Fixpoint (const int max_iter)
  : max_iteration (max_iter)
{ }
 
Fixpoint::~Fixpoint ()
{ }

// The 'RSquared' function.

double 
Iterative::RSquared (const std::vector<Iterative::PointValue>& obs,
                     const Iterative::PointFunction& fun)
{
  const size_t n = obs.size ();
  daisy_assert (n > 0);
  std::vector<double> sim (n, NAN);
  double obs_sum = 0.0;
  for (size_t i = 0; i < n; i++)
    {
      sim[i] = fun.value (obs[i].point);
      obs_sum += obs[i].value;
    }
  const double obs_avg = obs_sum / (n + 0.0);
  double SS_tot = 0.0;
  double SS_err = 0.0;
  for (size_t i = 0; i < n; i++)
    {
      SS_tot += sqr (obs[i].value - obs_avg);
      SS_err += sqr (obs[i].value - sim[i]);
    }
  daisy_assert (SS_tot > 0);
  return 1.0 - (SS_err / SS_tot);
}

static void 
print (const Iterative::Point& point, const double value, Treelog& msg)
{
  if (&msg == &Treelog::null ())
    return;

  std::ostringstream tmp;
  daisy_assert (point.size () > 0);
  tmp << "(" << point[0];
  for (size_t i = 1; i < point.size (); i++)
    tmp << " " << point[i];
  tmp << ")";
  if (value >= std::numeric_limits<double>::max ())
    tmp << " out of bound";
  else if (std::isfinite (value))
    tmp << " = " << value;
  msg.message (tmp.str ());
}


bool
Iterative::NelderMead (const size_t min_iter, const size_t max_iter, 
                       const double epsilon, 
                       const Iterative::PointFunction& fun, 
                       const Iterative::Simplex& simplex,
                       Iterative::Point& result, Treelog& msg)
{
  daisy_assert (min_iter > 0);
  daisy_assert (min_iter <= max_iter);
  daisy_assert (epsilon >= 0.0);
  
  typedef std::vector<PointValue> SimplexValue;

  // From Wikipedia article 'Nelder–Mead method'.

  // Parameters;
  const double alpha = 1.0; // Reflection.
  const double gamma = 2.0; // Expansion.
  const double rho = 0.5;   // Contraction.
  const double sigma = 0.5; // Reduction.

  // Start value.
  daisy_assert (simplex.size () > 0);
  const size_t dim = simplex.size () - 1u;
  SimplexValue simplex_value (dim + 1);
  for (size_t i = 0; i < dim + 1; i++)
    {
      Treelog::Open nest (msg, "Initial value", i + 1, "");
      simplex_value[i].point = simplex[i];
      simplex_value[i].value = fun.value (simplex[i]);
      print (simplex_value[i].point, simplex_value[i].value, msg);
    }

  // Variables.
  Point x0 (dim, NAN);     // Center of gravity for dim best points.
  Point xr (dim, NAN);     // Reflection of worst point.
  Point xe (dim, NAN);     // Expanded point.
  Point xc (dim, NAN);     // Contracted point.
  PointValue& best = simplex_value[0];
  PointValue& worst = simplex_value[dim];
  PointValue& second_worst = simplex_value[dim-1];

  double old_f_worst = std::numeric_limits<double>::max ();
  size_t iter_worst = 0;
  for (size_t iter = 1;; iter++)
    {
      Treelog::Open nest (msg, "Iteration", iter, "");

      // 1. Order according to the values at the vertices:
      //    f(\textbf{x}_{1}) \leq f(\textbf{x}_{2}) \leq \cdots \leq f(\textbf{x}_{n+1})
      msg.message ("1: Sorting.");
      std::sort (simplex_value.begin (), simplex_value.end ());
      const double f_best = best.value;

      // Stop?
      const double f_worst = worst.value;
      const double change = std::fabs (old_f_worst - f_worst);
      if (change > epsilon)
        {
          old_f_worst = f_worst;
          iter_worst = 1;
        }
      else
        iter_worst++;

      if (iter_worst > min_iter)
        {
          result = best.point;
          return true;
        }
      if (iter > max_iter)
        {
          result = best.point;
          return false;
        }

      // 2. Calculate xo, the center of gravity of all points except xn + 1.
      msg.message ("2: Find center of gravity of all but worst.");
      std::fill (x0.begin (), x0.end (), 0.0);
      for (size_t i = 0; i < dim; i++)
        for (size_t j = 0; j < dim; j++)
          x0[j] += simplex_value[i].point[j];

      const double factor = 1.0 / (dim + 0.0);
      for (size_t i = 0; i < dim; i++)
        x0[i] *= factor;
      print (x0, NAN, msg);

      // 3. Reflection
      // Compute reflected point
      // \textbf{x}_r = \textbf{x}_o + \alpha (\textbf{x}_o - \textbf{x}_{n+1})
      msg.message ("3: Compute reflection point.");
      const Point& x_worst = worst.point;
      for (size_t i = 0; i < dim; i++)
        xr[i] = x0[i] + alpha * (x0[i] - x_worst[i]);
      const double f_xr = fun.value (xr);
      print (xr, f_xr, msg);
      
      // If the reflected point is the best point so far, 
      // f(\textbf{x}_{r}) < f(\textbf{x}_{1}), 
      // then goto step 4 (expansion).
      if (f_xr < f_best)
        {
          msg.message ("Reflection point is the best so far.");
          // 4. Expansion
          // Compute the expanded point 
          // \textbf{x}_{e} = \textbf{x}_o + \gamma (\textbf{x}_o - \textbf{x}_{n+1})
          msg.message ("4: Compute expanded point.");
          for (size_t i = 0; i < dim; i++)
            xe[i] = x0[i] + gamma * (x0[i] - x_worst[i]);
          const double f_xe = fun.value (xe);
          print (xe, f_xe, msg);

          // If the expanded point is better than the reflected point, 
          //   f(\textbf{x}_{e}) < f(\textbf{x}_{r})
          // then obtain a new simplex by replacing the worst point xn + 1 
          // with the expanded point xe, and go to step 1 (order).
          // Else obtain a new simplex by replacing the worst point xn + 1 
          // with the reflected point xr, and go to step 1 (order).
          if (f_xe < f_xr)
            {
              msg.message ("Expanded point best, replacing worst point.");
              worst.point = xe;
              worst.value = f_xe;
            }
          else
            {
              msg.message ("Reflection point best, replacing worst point.");
              worst.point = xr;
              worst.value = f_xr;
            }
          continue;             // Step 1 (order).
        }

      // Step 3 continued.
      const double f_second_worst = second_worst.value; 
      if (f_xr < f_second_worst)
        {
          msg.message ("\
Reflected point better than second worst, replace worst.");
          // If the reflected point is better than the second worst,
          // but not better than the best, i.e.: 
          // f(\textbf{x}_{1}) \leq f(\textbf{x}_{r}) < f(\textbf{x}_{n}),
          // then obtain a new simplex by replacing the worst point xn + 1 
          // with the reflected point xr, and go to step 1 (order).
          worst.point = xr;
          worst.value = f_xr;
          continue;           // Step 1 (order).
        }
      msg.message ("Reflected point worse than second worst.");

      // 5. Contraction
      // Here, it is certain that f(\textbf{x}_{r}) \geq f(\textbf{x}_{n}) 
      // Compute contracted point 
      // \textbf{x}_{c} = \textbf{x}_{n+1}+\rho(\textbf{x}_{o}-\textbf{x}_{n+1})
      msg.message ("5: Compute contraction point.");
      for (size_t i = 0; i < dim; i++)
        xc[i] = x_worst[i] + rho * (x0[i] - x_worst[i]);
      const double f_xc = fun.value (xc);
      print (xc, f_xc, msg);

      // If the contracted point is better than the worst point, i.e. 
      //  f(\textbf{x}_{c}) < f(\textbf{x}_{n+1})
      // then obtain a new simplex by replacing the worst point xn + 1 with
      // the contracted point xc, and go to step 1 (order).
      const double f_x_worst = worst.value;
      if (f_xc < f_x_worst)
        {
          msg.message ("Contracted point better than worst, replace it.");
          worst.point = xc;
          worst.value = f_xc;
          continue;           // Step 1 (order).
        }
      // Else go to step 6.
      msg.message ("Contracted point worse than worst.");

      // 6. Reduction
      // For all but the best point, replace the point with
      // x_{i} = x_{1} + \sigma(x_{i} - x_{1}) \text{ for all i } \in\{2,\dots,n+1\}. 
      msg.message ("6: Reduction.");
      const Point& x_best = best.point;
      for (size_t i = 1; i < dim + 1; i++)
        {
          Treelog::Open nest (msg, "Reducing", i, "");
          Point& xi = simplex_value[i].point;
          double& f_xi = simplex_value[i].value;

          for (size_t j = 0; j < dim; j++)
            xi[j] = x_best[j] + sigma * (xi[j] - x_best[j]);
          f_xi = fun.value (xi);
          print (xi, f_xi, msg);
        }
      // Goto step 1. (order).
    }
}

bool
Iterative::NelderMead (const size_t min_iter, const size_t max_iter, 
                       const double epsilon, 
                       const Iterative::PointFunction& fun, 
                       const Iterative::Point& start,
                       Iterative::Point& result, Treelog& msg)
{
  // Make a simplex by putting a bit of noise on starting point.
  const size_t dim = start.size ();
  Simplex simplex (dim + 1, start);
  for (size_t i = 0; i < dim; i++)
    if (std::isnormal (simplex[i][i]))
      simplex[i][i] *= 1.01;
    else
      simplex[i][i] = 0.01;

  return NelderMead (min_iter, max_iter, epsilon, fun, simplex, result, msg);
}

// iterative.C ends here.
