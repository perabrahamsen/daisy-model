// syntax.C

#include "syntax.h"
#include "value.h"

Syntax_init::Syntax_init ()
{ 
    if (count++ == 0)
	syntax_table = new Syntax ();
    assert (count > 0);
}

Syntax_init::~Syntax_init ()
{ 
    if (--count == 0)
	{
	    delete syntax_table;
	    syntax_table = NULL;
	}
    assert (count >= 0);
}

int Syntax_init::count;

struct Syntax::Implementation
{
    // BUG: This should be replaced with STL maps.
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    type UGLY_type[UGLY_MAX_SIZE];
    const Syntax* UGLY_syntax[UGLY_MAX_SIZE];
    const FTable* UGLY_function[UGLY_MAX_SIZE];
    int UGLY_size[UGLY_MAX_SIZE];
    int size;
};    

bool 
Syntax::check (string name, const ValueList* vl, const Log& log) const
{
    bool error = false;

    for (int i = 0; i < impl.size; i++)
	{
	    string key = impl.UGLY_key[i];
	    if (!vl->check (key))
		{
		    cerr << "Attributte " << key << "\n";
		    error = true;
		}
	    else if (impl.UGLY_type[i] == List)
		{
		    error |= !impl.UGLY_syntax[i]->check 
			(key, 
			 BUG_DYNAMIC_CAST (const ValueList*,
					   vl->lookup (key)),
			 log);
		}
	}
    if (error)
	cerr << "missing from " << name << "\n";

    return !error;
}

Syntax::type 
Syntax::lookup (string key) const
{
    for (int i = 0; i < impl.size; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_type[i];
    return Error;
}

const Syntax*
Syntax::syntax (string key) const
{
    for (int i = 0; i < impl.size; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_syntax[i];
    assert (0);
}

const FTable*
Syntax::function (string key) const
{
    for (int i = 0; i < impl.size; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_function[i];
    assert (0);
}

int
Syntax::size (string key) const
{
    for (int i = 0; i < impl.size; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_size[i];
    assert (0);
}

void
Syntax::add (string key, type t)
{
    assert (impl.size < impl.UGLY_MAX_SIZE);
    impl.UGLY_key[impl.size] = key;
    impl.UGLY_syntax[impl.size] = NULL;
    impl.UGLY_type[impl.size] = t;
    impl.UGLY_function[impl.size] = NULL;
    impl.UGLY_size[impl.size] = -1;
    impl.size++;
}

void
Syntax::add (string key, const Syntax* s)
{
    add (key, List);
    impl.UGLY_syntax[impl.size - 1] = (s);
}

void
Syntax::add (string key, const FTable* f)
{
    add (key, Function);
    impl.UGLY_function[impl.size - 1] = f;
}

void
Syntax::add (string key, int s)
{
    add (key, Array);
    impl.UGLY_size[impl.size - 1] = s;
}

Syntax::Syntax () : impl (*new Implementation ())
{ }

Syntax::~Syntax ()
{
    delete &impl;
}

Syntax* syntax_table = NULL;
