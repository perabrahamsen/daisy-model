// mathlib.C

#include "mathlib.h"
#include <assert.h>
#include <math.h>
#include <sys/fsr.h>

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

double
single_positive_root_of_cubic_equation 
(double a, double b, double c, double d)
{
  const double p = 1.0 / 3.0 * ( - 1.0/3.0 * pow (b / a, 2) + c / a);
  const double q = 1.0 / 2.0 * ( 2.0 / 27.0 * pow (b / a, 3)
				 - 1.0/3.0 * b * c / pow (a, 2)
				 + d / a);
  const double r = pow (q, 2) + pow (p, 3);
  
  if (r >= 0)
    return cbrt (-q + sqrt (r)) + cbrt (-q - sqrt (r)) - b / (3 * a);
  else
    {
      const double psi = acos ( -q  / sqrt (pow (-p, 3)));

      const double y1 = 2 * sqrt (-p)
	* cos (psi / 3.0) - b / (3 * a);
      const double y2 = - 2 * sqrt (-p)
	* cos (psi / 3.0 + M_PI / 3.0) - b / (3 * a);
      const double y3 = - 2 * sqrt (-p)
	* cos (psi / 3.0 - M_PI / 3.0) - b / (3 * a);
      
      if (y1 >= 0)
	{
	  assert (y2 < 0 && y3 < 0);
	  return y1;
	}
      if (y2 >= 0)
	{
	  assert (y3 < 0);
	  return y2;
	}
      if (y3 >= 0)
	return y3;
      
      
      assert (y3 >= 0);
      return y3;
    }
}

bool approximate (const double a, const double b, const double noise)
{
  return ((b == 0.0) ? (a == 0.0) : fabs (a / b - 1.0) < noise);
}


extern "C" int matherr () 
{
  abort ();
}

extern "C" set_exceptions ();

struct set_sparc_exceptions
{
  set_sparc_exceptions ()
  {
    set_exceptions(FSR_TEM_DZ);
    1.0 / 0.0;
  }
}  set_sparc_exceptions_dummy;

/*

If you only have gcc you don't have that function.  This assembly
language function can be used to enable floating point exceptions.  It
may not work on the new UltraSPARC processors.  Use the FSR_TEM_*
macros in <sys/fsr.h> as arguments, e.g. set_exceptions(FSR_TEM_DZ).

	.global set_exceptions
	.type set_exceptions,#function
	.align 8
set_exceptions:
	save %sp,-104,%sp
	st %fsr,[%fp-8]
	set (0x1f<<23),%i2
	ld [%fp-8],%i1
	and %i0,%i2,%i0
	andn %i1,%i2,%i1
	or %i0,%i1,%i0
	st %i0,[%fp-8]
	ld [%fp-8],%fsr
	ret
	restore

*/
