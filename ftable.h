// ftable.h

#ifndef FTABLE_H
#define FTABLE_H

#include <std/string.h>
#include <vector.h>

class FTable
{
public:
    virtual bool check (string) = 0;
    virtual ~FTable ();
};

template <class T> class dFTable : public FTable
{
    struct Implementation 
#ifdef OUTLINE_NESTED_CLASS_IN_TEMPLATE_WORKS
    ;
#else
    {
	// BUG: Should be outlined in `ftable.C'.
	// BUG: Should use a STL map!
	const int UGLY_MAX_SIZE = 1024;
	string UGLY_key[UGLY_MAX_SIZE];
	T UGLY_value[UGLY_MAX_SIZE];
	int size;
	Implementation () : size (0) { }
    };
#endif
    Implementation& impl;
public:
    bool check (string);
    T lookup (string);
    void add (string, T);
    dFTable ();
    ~dFTable ();
};

#endif FTABLE_H
