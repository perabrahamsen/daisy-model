// column.C

#include "column.h"
#include "crop.h"
#include "value.h"
#include "syntax.h"
#include "library.h"
#include <iostream.h>

struct Column::Implementation
{ 
    Implementation ();
    ~Implementation ();
};

Column::Implementation::Implementation ()
{ }

Column::Implementation::~Implementation ()
{ }

void Column::tick (Column* /* left */, Column* /* rigth */,
		   const Wheather& wheater, int day, int hour)
{
    cout << "Column `" << name << "'tick\n"; 
    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	 (*crop)->tick (wheater, day, hour);
}

void Column::sow (const Library& croplib, string crop)
{
    try {
	const ValueList* values = croplib.lookup (crop);
	crops.push_back (new Crop (log, crop, values, *this));
    }
    catch (UninitializedValue) { 
	cerr << "Cannot sow unknow crop `" << crop << "'\n";
    }
}

Column::Column (Log& l, string n, const ValueList*, const Library&)
    : impl (*new Implementation ()),
      log (l),
      name (n)
{ }

Column::~Column ()
{ 
    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	delete *crop;
}

// Add the Column syntax to the syntax table.
static struct ColumnSyntax
{
    ColumnSyntax ();
} column_syntax;

ColumnSyntax::ColumnSyntax ()
{ 
    Syntax* syntax = new Syntax ();
    syntax->add ("bim", Syntax::Number);
    syntax_table->add ("column", syntax);
}
