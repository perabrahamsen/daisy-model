// syntax.h

#include <std/string.h>

struct FTable;

// Ensure the syntax table is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Syntax_init
{
    static int count;
public:
    Syntax_init ();
    ~Syntax_init ();
} syntax_init;

class Syntax 
{ 
    struct Implementation;
    Implementation& impl;
public:
    enum type 
    { Number, List, Rules, CSMP, Function, Array, Error };
    type lookup (string) const;
    const Syntax* syntax (string) const;
    const FTable* function (string) const;
    void add (string, type);
    void add (string, const Syntax*);
    void add (string, const FTable*);
    Syntax ();
    ~Syntax ();
};

extern Syntax* syntax_table;
