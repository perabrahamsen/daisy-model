// geometry.C

#include "geometry.h"
#include "syntax.h"
#include "alist.h"
#include "tmpstream.h"
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
  assert (false);
  return i;
}

unsigned int
Geometry::interval_border (double z) const
{
  double best = fabs (z - zplus_[0]);
  
  for (unsigned int i = 1; i < size_; i++)
    {
      double dist = fabs (z - zplus_[i]);
      if (dist > best)
	return i - 1;
      best = dist;
    }
  return size_;
}

bool 
Geometry::check (Treelog&) const
{
  bool ok = true;
  return ok;
}

static bool 
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  const vector<double> zplus = al.number_sequence ("zplus");
  
  if (zplus.size () < 1)
    {
      err.entry ("You need at least one interval");
      ok = false;
    }
  double last = 0.0;
  for (unsigned int i = 0; i < zplus.size (); i++)
    {
      if (zplus[i] >= last)
	{
	  TmpStream tmp;
	  tmp () << "Intervals should be monotonically decreasing, but "
		 << zplus[i] << " > " << last;
	  err.entry (tmp.str ());
	  ok = false;
	  break;
	}
      else 
	last = zplus[i];
    }
  return ok;
}

double
Geometry::total (const vector<double>& v) const
{
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
  assert (to < from);
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
  const int middle_interval = interval_plus (middle);
  bottom_content += v[middle_interval] * dz (middle_interval);
  v[middle_interval] = 0.0;
  const double new_middle = from + to - middle;

  add (v, from, new_middle, bottom_content);
  add (v, new_middle, to, top_content);
  assert (approximate (old_total, total (v)));
}

static bool 
check_layers (const vector<AttributeList*>& layers, Treelog& err)
{
  double last = 0.0;
  for (unsigned int i = 0; i < layers.size (); i++)
    {
      const double next = layers[i]->number ("end");
      if (next < last)
	last = next;
      else
	{
	  TmpStream tmp;
	  tmp () << "Layer ending at " << next 
		 << " should be below " << last;
	  err.entry (tmp.str ());
	  return false;
	}
    }
  return true;
}

void 
Geometry::add_layer (Syntax& syntax, const string& name,
		     const string& dimension, const string& description)
{
  Syntax& layer = *new Syntax ();
  layer.add_check (check_layers);
  if (!layer.ordered ())
    {
      // Initialize as first call.
      layer.add ("end", "cm", Syntax::Const, 
		 "End point of this layer (a negative number).");
      layer.add ("value", dimension, Syntax::Const, description);
      layer.order ("end", "value");
    }
  syntax.add (string ("initial_") + name, layer,
	      Syntax::OptionalConst, Syntax::Sequence, 
	      string ("Initial value of the `") + name + "' parameter.\n\
The initial value is given as a sequence of (END VALUE) pairs, starting\n\
from the top and going down.  The parameter will be initialized to\n\
VALUE from the END of the previous layer, to the END of the current layer.");
  syntax.add (name, dimension, Syntax::OptionalState, Syntax::Sequence, 
	      description);
}

void 
Geometry::initialize_layer (vector<double>& array, 
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
      const vector<AttributeList*>& layers = al.alist_sequence (initial);
      array.insert (array.begin (), size (), 0.0);
      const double soil_end = zplus (size () - 1);
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  double next = layers[i]->number ("end");
	  assert (next < last);
	  const double value = layers[i]->number ("value");
	  if (next < soil_end)
	    {
	      CERR << "WARNING: initial_" << name 
		   << " layer ends below the last node\n";
	      next = soil_end;
	      i = layers.size ();
	    }
	  add (array, last, next, value * (last - next));
	  last = next;
	}
    }
}

void
Geometry::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("zplus", "cm", Syntax::Const, Syntax::Sequence,
	      "Depth of each numeric layer (a negative number).\n\
The end points are listed descending from the surface to the bottom.");
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
