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
	vector<string> key;
	vector<T> value;
	Implementation () { }
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
