// mathlib.C

#include "mathlib.h"
#include <assert.h>
#include <math.h>

#ifndef __sparc__
// Sun has a cbrt function.
static double cbrt (double x) 
{
  return pow (x, 1.0/3.0);
}
#endif

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

inline double pow2 (double x)
{ return x * x; }

inline double pow3 (double x)
{ return x * x * x; }

double
single_positive_root_of_cubic_equation 
(double a, double b, double c, double d)
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
  if (fabs (a) < 1.0e-100)
    return fabs (b) < 1.0e-100;
  return ((b == 0.0) ? (a == 0.0) : fabs (a / b - 1.0) < noise);
}


// extern "C" int matherr (struct exception *exc) 
// {
  
//   abort ();
// }

#ifdef __sparc__

#include <sys/fsr.h>
extern "C" set_exceptions ();

struct set_sparc_exceptions
{
  set_sparc_exceptions ()
  {
    set_exceptions(FSR_TEM_DZ);
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
#endif /* __sparc */
