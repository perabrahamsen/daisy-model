// daisy.C

#include "daisy.h"
#include "input.h"
#include "manager.h"
#include "bioclimate.h"
#include "log.h"
#include "column.h"
#include "action.h"
#include "filter.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include <iostream.h>

struct Daisy::Implementation
{   
    Implementation () { };
};

Daisy::Daisy (const Input& input)
    : impl (*new Implementation ()), 
      log (input.makeLog ()), 
      time (input.makeTime ()),
      manager (input.makeManager ()),
      bioclimate (input.makeBioclimate ()), 
      columns (input.makeColumns ()),
      crops (input.makeCrops ())
{ }

void 
Daisy::run ()
{ 
    while (true)
	{
	    const Action* action
		= manager.action (*this);
	    
	    if (action->stop ())
		break;
	    
	    cout << "Tick " << time.year () << "-" << time.month () << "-"
		 << time.mday () << " " << time.hour () << " ";

	    action->doIt (columns, bioclimate, crops, log);

	    ColumnList::iterator prev;
	    ColumnList::iterator column = columns.end ();
	    ColumnList::iterator next = columns.begin ();

	    while (next != columns.end ())
		{
		    prev = column;
		    ColumnList::iterator column = next;
		    next++;

		    (*column)->tick (((prev != columns.end ()) ? *prev : 0),
				     ((next != columns.end ()) ? *next : 0),
				     bioclimate, time);
		}

	    time.step ();
	    log.tick (*this);
	}
}

bool
Daisy::match (const Condition* c) const
{
    return c->match (columns, bioclimate, time);
}

void
Daisy::output (Log& log, const Filter* filter) const
{
    log.open ();
    log.output ("time", filter, time);
    if (filter->check ("columns"))
	output_field (log, filter->lookup ("columns"));
    log.close ();
}

void
Daisy::output_field (Log&, const Filter* filter) const

{
    log.open ("field");
    for (ColumnList::iterator column = columns.begin();
	 column != columns.end();
	 column++)
	{
	    if (filter->check ((*column)->name))
		(*column)->output (log, filter->lookup ((*column)->name));
	}
    log.close ();
}

Daisy::~Daisy ()
{
    delete &manager;
    delete &bioclimate;
    delete &log;
    delete &impl;
    for (ColumnList::iterator column = columns.begin();
	 column != columns.end();
	 column++)
	delete *column;
}

// Add the Daisy syntax to the syntax table.
static struct DaisySyntax
{
    DaisySyntax ();
} Daisy_syntax;

DaisySyntax::DaisySyntax ()
{ 
    Syntax* syntax = new Syntax ();
    syntax->add ("columns", Syntax::Columns);
    syntax->add ("time", Syntax::Date);
    syntax_table->add ("daisy", syntax);
}
