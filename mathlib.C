// mathlib.C

#include "mathlib.h"
#include <assert.h>
#include <math.h>

// See _Computational_Techniques_for_Differential_Equations page 616.
void
tridia (const unsigned int N,
	const vector<double>& a,
	const vector<double>& b, 
	const vector<double>& c,
	const vector<double>& d,
	vector<double>::iterator x)
{
  assert (a.size () >= N);
  assert (b.size () >= N);
  assert (c.size () >= N);
  assert (d.size () >= N);

  static vector<double> y;
  static vector<double> beta;
  
  if (y.size() < N)
    {
      assert (beta.size () == y.size ());
      y.insert (y.end(), N - y.size(), 0.0);
      beta.insert (beta.end(), N - beta.size(), 0.0);
    }
  
  beta[0] = b[0];
  y[0] = d[0];

  // Forward substitution.
  for (unsigned int i = 1; i < N; i++)
    {
      double amult = a[i] / beta[i - 1];
      beta[i] = b[i] - amult * c[i - 1];
      y[i] = d[i] - amult * y[i - 1];
    }
  // Backward substitution.
  x[N - 1] = y[N - 1] / beta[N - 1];
  for (int i = N - 2; i >= 0; i--)
    {
      x[i] = (y[i] - c[i] * x[i + 1]) / beta[i];
    }
}

extern "C" int matherr () 
{
  abort ();
}

