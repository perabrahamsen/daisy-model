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
#include <sstream>

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

// The 'NelderMead' class.



// iterative.C ends here.
