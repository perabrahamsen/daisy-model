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
  return max (-100.0, z (size () - 1));
}

double 
Soil::EpFactor () const
{
  return EpFactor_;
}

double
Soil::EpInterchange () const
{
  return EpInterchange_;
}

bool 
Soil::check () const
{
  bool ok = true;
  if (zplus_.size () < 1)
    {
      cerr << "You need at least one interval\n";
      ok = false;
    }
  if (horizon_.size () < 1)
    {
      cerr << "You need at least one horizon\n";
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

void
Soil::add (vector<double>& v, double from, double to, double amount) const
{
  while (v.size () < size () + 0U)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned i = 0; i < v.size () && old > to ; i++)
    {
      if (zplus_[i] < from)
	v[i] += density * (min (old, from) - max (zplus_[i], to));
      old = zplus_[i];
    }
}

void
Soil::mix (vector<double>& v, double from, double to) const
{
  add (v, from, to, extract (v, from, to));
}

double
Soil::extract (vector<double>& v, double from, double to) const
{
  while (v.size () < size () + 0U)
    v.push_back (0.0);
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i < v.size () && old > to ; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  amount += v[i] * height;
	  v[i] -= v[i] * height / (old - zplus_[i]);
	}
      old = zplus_[i];
    }
  return amount;
}

void
Soil::set (vector<double>& v, double from, double to, double amount) const
{
  while (v.size () < size () + 0U)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned i = 0; i < v.size () && old > to ; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  v[i] -= v[i] * height / (old - zplus_[i]); // Remove old.
	  v[i] += density * height; // Add new.
	}
      old = zplus_[i];
    }
}

void
Soil::swap (vector<double>& v, double from, double middle, double to) const
{
  const double top_content = extract (v, from, middle);
  const double bottom_content = extract (v, middle, to);
  const double new_middle = from + to - middle;

  set (v, from, to, 0.0);
  add (v, from, new_middle, bottom_content);
  add (v, new_middle, to, top_content);
}



void
Soil::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Syntax& layer_syntax = *new Syntax ();
  AttributeList& layer_alist = *new AttributeList ();
  layer_syntax.add ("end", Syntax::Number, Syntax::Const);
  layer_syntax.add ("horizon", Horizon::library (), Syntax::State);
  layer_syntax.order ("end", "horizon");
  syntax.add ("horizons", layer_syntax, Syntax::State, Syntax::Sequence);
  alist.add ("horizons", layer_alist);
  syntax.add ("zplus", Syntax::Number, Syntax::Const, Syntax::Sequence);
  syntax.add ("EpFactor", Syntax::Number, Syntax::Const);
  alist.add ("EpFactor", 0.8);
  syntax.add ("EpInterchange", Syntax::Number, Syntax::Const);
  alist.add ("EpInterchange", 0.6);
}
  
Soil::Soil (const AttributeList& al)
  : zplus_ (al.number_sequence ("zplus")),
    size_ (zplus_.size ()),
    EpFactor_ (al.number ("EpFactor")),
    EpInterchange_ (al.number ("EpInterchange"))
{
  vector<const AttributeList*>::const_iterator layer
    = al.list_sequence ("horizons").begin ();
  const vector<const AttributeList*>::const_iterator end 
    = al.list_sequence ("horizons").end ();

  if (layer != end)
    {
      const Horizon* hor = &Horizon::create ((*layer)->list ("horizon"));
      double last = 0.0;
      for (int i = 0; i < size_; i++)
	{
	  double zplus = zplus_[i];
	  double dz = last - zplus;
	  dz_.push_back (dz);
	  double z = last - dz / 2;
	  z_.push_back (z);
	  if (zplus < (*layer)->number ("end"))
	    {
	      layer++;
	      assert (layer != end);
	      hor = &Horizon::create ((*layer)->list ("horizon"));
	    }
	  horizon_.push_back (hor);
	  last = zplus;
	}
      horizon_.push_back (hor);
    }
};

Soil::~Soil ()
{ }

