// library.C

#include "library.h"
#include "value.h"
#include "syntax.h"

struct Library::Implementation
{
    // This should be replaced with a STL map.
    // THIS SHOULD BE REPLACED WITH A STL MAP.
    // I'm not this stupid.  Honestly.
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    const ValueList* UGLY_value[UGLY_MAX_SIZE];
    const Syntax* UGLY_syntax[UGLY_MAX_SIZE];
    int size;
    Implementation ();
};

Library::Implementation::Implementation () : size (0)
{ }


const ValueList* 
Library::lookup (string key) const
{ 
    if (syntax_table->lookup (key) == Syntax::List)
	return &ValueList::empty;;

    for (int i = 0; i < impl.UGLY_MAX_SIZE; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_value[i];
    throw UninitializedValue ();
}

void
Library::add (string key, ValueList* value, const Syntax* syntax)
{
    assert (impl.size < impl.UGLY_MAX_SIZE);
    impl.UGLY_key[impl.size] = key;
    impl.UGLY_value[impl.size] = value;
    impl.UGLY_syntax[impl.size] = syntax;
    impl.size++;
}

const Syntax* 
Library::syntax (string key) const
{ 
    if (syntax_table->lookup (key) == Syntax::List)
	return syntax_table->syntax (key);
    
    for (int i = 0; i < impl.UGLY_MAX_SIZE; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_syntax[i];
    throw UninitializedValue ();    
}

Library::Library () : impl (*new Implementation ())
{ }

Library::~Library ()
{
    delete &impl;
}
