// mathlib.C

#include "mathlib.h"
#include <assert.h>
#include <math.h>

// See _Computational_Techniques_for_Differential_Equations page 616.
void
tridia (int from,
	const unsigned int N,
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
    {
      x[i] = (y[i] - c[i] * x[i + 1]) / beta[i];
    }
}

extern "C" int matherr () 
{
  abort ();
}

