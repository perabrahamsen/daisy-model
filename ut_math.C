#define BUILD_DLL
#include "iterative.h"
#include "assertion.h"
#include <gtest/gtest.h>

static const size_t dim = 2;

static double fun (const NelderMead::Point& p)
{
  daisy_assert (dim == 2);
  daisy_assert (p.size () == dim);
  const double x = p[0];
  const double y = p[1];
  
  return (x-2) * (x-2) + (y+1) * (y+1);
}

TEST (Math, NelderMead)
{
  NelderMead::Point start (dim, 1.0);
  const double epsilon = 0.01;
  const size_t min_iter = 100;
  const size_t max_iter = 3000;
  NelderMead::Point result;
  const bool solved = NelderMead::solve (min_iter, max_iter, epsilon,
                                         dim, fun, start, result);
  EXPECT_TRUE (solved);
  EXPECT_EQ (result.size (), 2);
  const double x = result[0];
  const double y = result[1];
  EXPECT_NEAR (x, 2.0, 0.01);
  EXPECT_NEAR (y, -1.0, 0.01);
}
