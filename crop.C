// crop.C

#include "crop.h"
#include "syntax.h"

struct Crop::Implementation
{ 
    const ValueList* values;
    Implementation (const ValueList*);
    ~Implementation ();
};

Crop::Implementation::Implementation (const ValueList* vl)
    : values (vl)
{ }

Crop::Implementation::~Implementation ()
{ }

void
Crop::tick (const Wheather& /* wheater */, int /* day */, int /* hour */)
{ 
    cout << "Crop `" << name << "' tick\n"; 
}

Crop::Crop (Log& l, const string n, const ValueList* vl, Column& c)
    : impl (*new Implementation (vl)),
      log (l), 
      name (n),
      column (c)
{ }

Crop::~Crop ()
{ }

// Add the Crop syntax to the syntax table.
static struct CropSyntax
{
    CropSyntax ();
} crop_syntax;

CropSyntax::CropSyntax ()
{ 
    Syntax* syntax = new Syntax ();
    syntax->add ("buh", Syntax::Number);
    syntax_table->add ("crop", syntax);
}
