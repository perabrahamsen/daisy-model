// library.h

#include <std/string.h>

struct Syntax;
struct AttributeList;

class Library
{
    struct Implementation;
    Implementation& impl;
public:
    const AttributeList& lookup (string) const;
    bool check (string) const;
    void add (string, const AttributeList&, const Syntax*);
    const Syntax* syntax (string) const;
    Library ();
    ~Library ();
};
