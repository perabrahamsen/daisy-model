// geometry.C

#include "geometry.h"
#include "syntax.h"
#include "alist.h"
#include "mathlib.h"
#include <assert.h>

unsigned int 
Geometry::interval_plus (double z) const
{
  unsigned int i;
  for (i = 0; i < size_; i++)
    {
      if (zplus_[i] <= z)
	return i;
    }
  assert (0);
  return i;
}

unsigned int 
Geometry::interval (double z) const
{
  unsigned int i;
  for (i = 0; i < size_; i++)
    {
      if (z_[i] <= z)
	return i;
    }
  assert (0);
  return i;
}

bool 
Geometry::check () const
{
  bool ok = true;
  if (zplus_.size () < 1)
    {
      cerr << "You need at least one interval\n";
      ok = false;
    }
  double last = 0.0;
  for (unsigned int i = 0; i < size_; i++)
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

double
Geometry::total (const vector<double>& v) const
{
  assert (v.size () <= size ());
  const unsigned int to = min (v.size (), size ());
  double sum = 0.0;
  for (unsigned int i = 0; i < to; i++)
    sum += v[i] * dz (i);
  return sum;
}

double
Geometry::total (const vector<double>& v, double from, double to) const
{
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i < v.size () && old > to ; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  amount += v[i] * height;
	}
      old = zplus_[i];
    }
  return amount;
}

void
Geometry::add (vector<double>& v, double from, double to, double amount) const
{
  const double old_total = total (v);

  const unsigned int last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	v[i] += density * (min (old, from) - max (zplus_[i], to)) / dz_[i];
      old = zplus_[i];
    }

  assert (approximate (old_total + amount, total (v)));
}

void
Geometry::mix (vector<double>& v, double from, double to) const
{
  const double old_total = total (v);
  add (v, from, to, extract (v, from, to));
  assert (approximate (old_total, total (v)));
}

double
Geometry::extract (vector<double>& v, double from, double to) const
{
  const double old_total = total (v);
  const unsigned int last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  amount += v[i] * height;
	  v[i] -= v[i] * height / (old - zplus_[i]);
	}
      old = zplus_[i];
    }
  assert (approximate (old_total, total (v) + amount));
  return amount;
}

void
Geometry::set (vector<double>& v, double from, double to, double amount) const
{
  const unsigned int last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
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
Geometry::swap (vector<double>& v, double from, double middle, double to) const
{
  
  const double old_total = total (v);
  const double top_content = extract (v, from, middle);
  double bottom_content = extract (v, middle, to);
  // We want to extract 100% of the interval containing middle, since
  // we already extracted the rest into top_content.
  const int middle_interval = interval (middle);
  bottom_content += v[middle_interval] * dz (middle_interval);
  v[middle_interval] = 0.0;
  const double new_middle = from + to - middle;

  add (v, from, new_middle, bottom_content);
  add (v, new_middle, to, top_content);
  assert (approximate (old_total, total (v)));
}

void Geometry::add_layer (Syntax& syntax, const string& name)
{
  static Syntax& layer = *new Syntax ();
  if (!layer.ordered ())
    {
      // Initialize as first call.
      layer.add ("size", Syntax::Number, Syntax::Const);
      layer.add ("value", Syntax::Number, Syntax::Const);
      layer.order ("size", "value");
    }
  syntax.add (string ("initial_") + name, layer,
	      Syntax::Optional, Syntax::Sequence);
  syntax.add (name, Syntax::Number, Syntax::Optional, Syntax::Sequence);
}

void Geometry::initialize_layer (vector<double>& array, 
				 const AttributeList& al, 
				 const string& name) const
{
  const string initial = string ("initial_") + name;
  assert (array.size () == 0);
  if (al.check (name))
    // Specified by user.
    array = al.number_sequence (name);
  else if (al.check (initial))
    {
      // Initialize by layers.
      vector<AttributeList*>& layers = al.alist_sequence (initial);
      array.insert (array.begin (), size (), 0.0);
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  const double next = last - layers[i]->number ("size");
	  const double value = layers[i]->number ("value");
	  add (array, last, next, value);
	  last = next;
	}
    }
}

void
Geometry::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("zplus", Syntax::Number, Syntax::Const, Syntax::Sequence);
}
  
Geometry::Geometry (const AttributeList& al)
  : zplus_ (al.number_sequence ("zplus")),
    size_ (zplus_.size ())
{
  double last = 0.0;
  for (unsigned int i = 0; i < size_; i++)
    {
      double zplus = zplus_[i];
      double dz = last - zplus;
      dz_.push_back (dz);
      double z = last - dz / 2;
      z_.push_back (z);
      last = zplus;
    }
};

Geometry::~Geometry ()
{ }
