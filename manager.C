// manager.C

#include "manager.h"
#include "syntax.h"
#include "rules.h"
#include "alist.h"

struct Manager::Implementation
{ 
    const Rules& rules;
    Implementation (const AttributeList& vl);
};

Manager::Implementation::Implementation (const AttributeList& vl)
    : rules (vl.rules ("rules"))
{ }

const Action*
Manager::action (const Daisy& daisy)
{
    return impl.rules.match (daisy);
}

Manager::Manager (const AttributeList& vl)
    : impl (*new Implementation (vl))
{ }

Manager::~Manager () 
{
    delete &impl;
}


// Add the Manager syntax to the syntax table.
static struct ManagerSyntax
{
    ManagerSyntax ();
} manager_syntax;

ManagerSyntax::ManagerSyntax ()
{ 
    Syntax* syntax = new Syntax ();
    syntax->add ("rules", Syntax::Rules);
    syntax_table->add ("manager", syntax);
}
