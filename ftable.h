// ftable.h

#include <std/string.h>

class FTable
{
public:
    virtual bool check (string) = 0;
    virtual ~FTable ();
};

template <class T> class FTable : public FTable
{
    struct Implementation
    {
	// BUG: Should be outlined in `ftable.C'.
	// BUG: Should use a STL map!
	vector<string> keys;
	vector<T> values;
    };
    Implementation& impl;
public:
    bool check (string);
    T lookup (string);
    void add (string, T);
    FTable ();
    ~FTable ();
};
