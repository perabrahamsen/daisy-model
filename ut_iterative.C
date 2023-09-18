// ut_iterative.C --- Unit tests for iterative methods.

#define BUILD_DLL
#include "iterative.h"
#include "assertion.h"
#include "treelog.h"
#include <gtest/gtest.h>

TEST (Iterative, NelderMead)
{
  class Fun : public Iterative::PointFunction
  {
    double value (const Iterative::Point& p) const
    {
      const size_t dim = p.size ();
      daisy_assert (dim == 2);
      const double x = p[0];
      const double y = p[1];

      return (x-2) * (x-2) + (y+1) * (y+1);
    }
  };
  Fun fun;

  const size_t dim = 2;
  Iterative::Point start (dim, 1.0);
  const double epsilon = 0.01;
  const size_t min_iter = 100;
  const size_t max_iter = 3000;
  Iterative::Point result;
  const bool solved = Iterative::NelderMead (min_iter, max_iter, epsilon,
                                             fun, start, result, 
                                             Treelog::null ());
  EXPECT_TRUE (solved);
  EXPECT_EQ (result.size (), 2);
  const double x = result[0];
  const double y = result[1];
  EXPECT_NEAR (x, 2.0, 0.01);
  EXPECT_NEAR (y, -1.0, 0.01);
}

// ut_iterative.C ends here.
