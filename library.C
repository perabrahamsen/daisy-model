// library.C

#include "library.h"
#include "alist.h"
#ifndef MODERN_LIBRARIES
#include "syntax.h"
#endif

struct Library::Implementation
{
    // This should be replaced with a STL map.
    // THIS SHOULD BE REPLACED WITH A STL MAP.
    // I'm not this stupid.  Honestly.
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    const AttributeList* UGLY_value[UGLY_MAX_SIZE];
    const Syntax* UGLY_syntax[UGLY_MAX_SIZE];
    int size;
    Implementation ();
};

Library::Implementation::Implementation () : size (0)
{ }


const AttributeList&
Library::lookup (string key) const
{ 
    for (int i = 0; i < impl.UGLY_MAX_SIZE; i++)
	if (impl.UGLY_key[i] == key)
	    return *impl.UGLY_value[i];
    THROW (UninitializedValue ());
}

bool
Library::check (string key) const
{ 
    for (int i = 0; i < impl.UGLY_MAX_SIZE; i++)
	if (impl.UGLY_key[i] == key)
	    return true;

    return false;
}

void
Library::add (string key, const AttributeList& value, const Syntax& syntax)
{
    assert (impl.size < impl.UGLY_MAX_SIZE);
    impl.UGLY_key[impl.size] = key;
    impl.UGLY_value[impl.size] = &value;
    impl.UGLY_syntax[impl.size] = &syntax;
    impl.size++;
}

const Syntax& 
Library::syntax (string key) const
{ 
    
    for (int i = 0; i < impl.UGLY_MAX_SIZE; i++)
	if (impl.UGLY_key[i] == key)
	    return *impl.UGLY_syntax[i];
    THROW (UninitializedValue ());    
}

Library::Library () : impl (*new Implementation ())
{ }

Library::~Library ()
{
    delete &impl;
}
