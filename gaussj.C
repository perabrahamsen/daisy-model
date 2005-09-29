// gaussj.C
//
// <http://www-ee.eng.hawaii.edu/Courses/EE150/Book/chap9/section2.1.5.html>

#include "gaussj.h"

#define DEBUG_MESSAGES

#ifdef DEBUG_MESSAGES
#include "mathlib.h"
#include "assertion.h"
#include <sstream>
#endif

using namespace std;

void 
GaussJordan::uptriangle ()
{ 
  for (int k = 0; k < size; k++)
    {
      find_pivot (k);
      process_column (k);
    }
}

void 
GaussJordan::process_column (const int k)
{
  for (int i = k + 1; i < size; i++)
    {
      const double m = -get_entry (i, k) / get_entry (k, k);
      for (int j = k; j < size; j++)
	set_entry (i, j, get_entry (i, j) + m * get_entry (k, j));
      value[i] += m * value[k];
    }
}

void 
GaussJordan::find_pivot (const int k)
{
  if (get_entry (k, k) == 0.0)
    swap_rows (k, find_nonzero (k));
}

int 
GaussJordan::find_nonzero (const int k)
{
  for (int i = k; i < size; i++)
    if (get_entry (i, k) != 0.0)
      return i;
  throw ("GaussJordan: Dependent equations.");
}

void 
GaussJordan::swap_rows (const int k, const int j)
{
  for (int i = k; i < size; i++)
    {
      const double tmp = get_entry (k, i);
      set_entry (k, i, get_entry (j, i));
      set_entry (j, i, tmp);
    }
  const double tmp = value[j];
  value[j] = value[k];
  value[k] = tmp;
}

void
GaussJordan::set_value (int row, double v)
{ value[row] = v; }

double
GaussJordan::get_value (int row) const
{ return value[row]; }

void 
GaussJordan::set_entry (int row, int column, double v)
{ matrix[row * size + column] = v; }

double
GaussJordan::get_entry (int row, int column) const
{ return matrix[row * size + column]; }

void 
GaussJordan::solve ()
{ 
  uptriangle ();

  for (int i = size - 1; i >= 0; i--)
    {
      double sum = 0;
      
      for (int j = i+1; j < size; j++)
	sum += get_entry (i, j) * result_[j];

      const double entry = get_entry (i, i);
      if (entry == 0.0)
	throw ("GaussJordan: zero solution");
#ifdef DEBUG_MESSAGES
      if (!isfinite (entry) || !isfinite (value[i]) || !isfinite (sum))
	throw ("GaussJordan: non-finite number");
      if (fabs (entry) < 1e-100)
	{
	  std::ostringstream tmp;
	  tmp << "GaussJordan[" << i << "," << i << "] = " << entry 
	      << ", value = " << value[i] << ", sum = " << sum;
	  Assertion::message (tmp.str ());
	}
#endif
      result_[i] = (value[i] - sum) / entry;
    }
}

double GaussJordan::result (int row) const
{ return result_[row]; }

GaussJordan::GaussJordan (int s)
  : size (s),
    matrix (size * size),
    value (size),
    result_ (size)
{ }
