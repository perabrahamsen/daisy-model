// ftable.C

#include "ftable.h"
#include <vector.h>

FTable::~FTable ()
{ }

template <class T> bool 
FTable<T>::check (string key)
{
    for (int i = 0; i < impl.keys.size (); i++)
	if (key == imp.keys[i])
	    return true;

    return false;
}

template <class T> T
FTable<T>::lookup (string key)
{
    for (int i = 0; i < impl.keys.size (); i++)
	if (key == imp.keys[i])
	    return impl.values[i];
    assert (0);
}

template <class T> void
FTable<T>::add (string key, T value)
{
    impl.keys.push_back (key);
    impl.values.push_back (value);
}

template <class T> 
FTable<T>::FTable () 
    : impl(new *Implementation)
{ }

template <class T> 
FTable<T>::~FTable () 
{
    delete &impl;
}

