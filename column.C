// column.C

#include "column.h"
#include "crop.h"
#include "syntax.h"
#include "log.h"
#include "filter.h"
#include "library.h"
#include <iostream.h>

struct AttributeList;

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
    cout << "Column `" << name << "' tick\n"; 
    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	 (*crop)->tick (wheater, day, hour);
}

void Column::sow (const Library& croplib, string crop, Log& log)
{
    if (croplib.check (crop))
	{
	    const AttributeList& values = croplib.lookup (crop);

	    if (syntax_table->syntax ("crop")->check (crop, values, log))
		crops.push_back (new Crop (crop, values));
	}
    else
	cerr << "Cannot sow unknow crop `" << crop << "'\n";
}

Column::Column (string n,
		const AttributeList& /*par*/, const AttributeList& /*var*/, 
		const Library&)
    : impl (*new Implementation ()),
      name (n)
{ }

void
Column::output (Log& log, const Filter* filter) const
{
    log.open (name);
    if (filter->check ("crops"))
	output_crops (log, filter->lookup ("crops"));
    log.close ();
}

void
Column::output_crops (Log& log, const Filter* filter) const
{
    log.open ("crops");
    for (CropList::const_iterator crop = crops.begin(); 
	 crop != crops.end();
	 crop++)
	{
	    if (filter->check ((*crop)->name))
		(*crop)->output (log, filter->lookup ((*crop)->name));
	}
    log.close ();
}

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
    Syntax* par = new Syntax ();
    par->add ("bim", Syntax::Number);
    syntax_table->add ("column", par);

    Syntax* var = new Syntax ();
    var->add ("crops", Syntax::Crops);
    syntax_table->add ("column/state", var);
}
