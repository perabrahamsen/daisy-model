// horizon.C

#include "horizon.h"
#include "syntax.h"

struct Horizon::Implementation
{ };

Horizon::Horizon (Log& l, Column& c)
    : impl (*new Implementation ()),
      log (l), 
      column (c)
{ }

Horizon::~Horizon ()
{ }

// Add the Horizon syntax to the syntax table.
static struct HorizonSyntax
{
    HorizonSyntax ();
} horizon_syntax;

HorizonSyntax::HorizonSyntax ()
{ 
    Syntax* syntax = new Syntax ();
    syntax->add ("bum", Syntax::Number);
    syntax_table->add ("horizon", syntax);
}
