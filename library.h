// library.h

#include <std/string.h>

struct Syntax;
struct ValueList;

class Library
{
    struct Implementation;
    Implementation& impl;
public:
    const ValueList* lookup (string) const;
    void add (string, ValueList*, const Syntax*);
    const Syntax* syntax (string) const;
    Library ();
    ~Library ();
};
