// filter.C

#include "filter.h"
#include "common.h"
#include <map.h>

class Value;

const FilterAll* Filter::all = new FilterAll ();
const FilterNone* Filter::none = new FilterNone ();

Filter::Filter ()
{ }

Filter::~Filter ()
{ }

bool 
FilterAll::check (string, bool log_only) const
{ return !log_only; }

const Filter& 
FilterAll::lookup (string) const
{ return *all; }

FilterAll::FilterAll ()
{ }

bool 
FilterNone::check (string, bool) const
{ return false; }

const Filter& 
FilterNone::lookup (string) const
{ 
  assert (false);
}

FilterNone::FilterNone ()
{ }

struct FilterSome::Implementation
{
  typedef map<string, const Filter*, less<string> > filter_map;
  filter_map filters;
  const Filter& lookup (string) const;
  bool check (string) const;
  void add (string, const Filter&);
};

const Filter& 
FilterSome::Implementation::lookup (string key) const
{ 
  filter_map::const_iterator i = filters.find (key);
  
  if (i != filters.end ())
    return *(*i).second;
  else
    THROW (UninitializedValue ());
}

bool
FilterSome::Implementation::check (string key) const
{ 
  return filters.find (key) != filters.end ();
}

void
FilterSome::Implementation::add (string key, const Filter& filter)
{
  filters[key] = &filter;
}

bool 
FilterSome::check (string key, bool) const
{ return impl.check (key); }

const Filter& 
FilterSome::lookup (string key) const
{ return impl.lookup (key); }

void 
FilterSome::add (string key, const Filter& filter)
{
  impl.add (key, filter);
}

FilterSome::FilterSome ()
  : impl (*new Implementation ())
{ }

FilterSome::~FilterSome ()
{
  delete &impl;
}

