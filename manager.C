// manager.C

#include "manager.h"
#include "syntax.h"
#include "value.h"

struct Manager::Implementation
{ 
    Log& log;
    const Rules& rules;
    Implementation (Log& l, const AttributeList& vl);
};

Manager::Implementation::Implementation (Log& l, 
					 const AttributeList& vl)
    : log (l),
      // BUG: SHOULD USE DYNAMIC CAST
      rules (vl.rules ("rules"))
{ }

const Action*
Manager::action (ColumnList& columns, const Wheather& wheather, 
		 int day, int hour)
{
    return impl.rules.match (columns, wheather, day, hour);
}

Manager::Manager (Log& l, const AttributeList& vl)
    : impl (*new Implementation (l, vl))
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
