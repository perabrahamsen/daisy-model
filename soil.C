// soil.C

#include "soil.h"
#include "alist.h"
#include <assert.h>

int 
Soil::interval_plus (double z) const
{
  int i;
  for (i = 0; i < size_; i++)
    {
      if (zplus_[i] < z)
	return i;
    }
  assert (0);
  return i;
}

int 
Soil::interval (double z) const
{
  int i;
  for (i = 0; i < size_; i++)
    {
      if (z_[i] < z)
	return i;
    }
  assert (0);
  return i;
}

double
Soil::MaxRootingDepth () const
{
  return 100.0;
}

bool 
Soil::check (Log& /* log */) const
{
  bool ok = true;
  if (zplus_.size () < 1)
    {
      cerr << "You need at least one interval\n";
      ok = false;
    }
  double last = 0.0;
  for (int i = 0; i < size_; i++)
    if (zplus_[i] > last)
      {
	cerr << "Intervals should be monotonically decreasing, but "
	     << zplus_[i] << " > " << last << "\n";
	ok = false;
	break;
      }
  else 
    last = zplus_[i];

  return ok;
}

Soil::Soil (const AttributeList& al)
  : zplus_ (al.array ("zplus"))
{
  const HorizonList& horizons = al.horizons ("horizons");
  size_ = zplus_.size ();
  double last = 0.0;
  for (int i = 0; i < size_; i++)
    {
      double zplus = zplus_[i];
      double dz = last - zplus;
      dz_.push_back (dz);
      double z = last - dz / 2;
      z_.push_back (z);
      horizon_.push_back (&horizons.horizon (z));
      last = zplus;
    }
  horizon_.push_back (horizon_[size_ - 1]);
};

Soil::~Soil ()
{ }
