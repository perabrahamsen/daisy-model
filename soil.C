// soil.C

#include "soil.h"
#include "alist.h"
#include "syntax.h"
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
  Layers::const_iterator layer = al.layers ("horizons").begin ();
  Layers::const_iterator end = al.layers ("horizons").end ();
  assert (layer != end);
  const Horizon* hor = &Horizon::create (*(*layer).second);
  size_ = zplus_.size ();
  double last = 0.0;
  for (int i = 0; i < size_; i++)
    {
      double zplus = zplus_[i];
      double dz = last - zplus;
      dz_.push_back (dz);
      double z = last - dz / 2;
      z_.push_back (z);
      if (zplus < (*layer).first)
	{
	  layer++;
	  assert (layer != end);
	  hor = &Horizon::create (*(*layer).second);
	}
      horizon_.push_back (hor);
      last = zplus;
    }
  horizon_.push_back (hor);
};

Soil::~Soil ()
{ }

const Syntax&
Soil::parameter_syntax ()
{
  Syntax& syntax = *new Syntax ();
  syntax.add_layers ("horizons", Horizon::library ());
  syntax.add ("zplus", Syntax::Array);
  return syntax;
}

  
