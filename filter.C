// filter.C

#include "filter.h"

class Value;

const FilterAll* Filter::all = new FilterAll ();

Filter::Filter ()
{ }

Filter::~Filter ()
{ }

bool 
FilterAll::check (string) const
{ return true; }

const Filter* 
FilterAll::lookup (string) const
{ return Filter::all; }

FilterAll::FilterAll ()
{ }

struct FilterSome::Implementation
{
    // BUG: Insert some sorry excuse for not using an STL map.
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    const Filter* UGLY_filter[UGLY_MAX_SIZE];
    int size;
    const Filter* lookup (string) const;
    bool check (string) const;
    void add (string, const Filter*);
    Implementation ();
};

FilterSome::Implementation::Implementation () : size (0)
{ }

const Filter* 
FilterSome::Implementation::lookup (string key) const
{ 
    for (int i = 0; i < size; i++)
	if (UGLY_key[i] == key)
	    return UGLY_filter[i];
    assert (0);
}

bool
FilterSome::Implementation::check (string key) const
{ 
    for (int i = 0; i < size; i++)
	if (UGLY_key[i] == key)
	    return true;
    return false;
}

void
FilterSome::Implementation::add (string key, const Filter* filter)
{
    assert (size < UGLY_MAX_SIZE);
    UGLY_key[size] = key;
    UGLY_filter[size] = filter;
    size++;
}

bool 
FilterSome::check (string key) const
{ return impl.check (key); }

const Filter* 
FilterSome::lookup (string key) const
{ return impl.lookup (key); }

void 
FilterSome::add (string key, const Filter* filter)
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

