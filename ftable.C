// ftable.C

#include "ftable.h"

FTable::~FTable ()
{ }

#ifdef OUTLINE_NESTED_CLASS_IN_TEMPLATE_WORKS
template <class T>
struct dFTable<class T>::Implementation
{
    // BUG: Should use a STL map!
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    T UGLY_value[UGLY_MAX_SIZE];
    int size;
    Implementation () : size (0);
};


#endif

