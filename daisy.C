// daisy.C

#include "daisy.h"
#include "input.h"
#include "manager.h"
#include "wheather.h"
#include "log.h"
#include "column.h"
#include "action.h"
#include "library.h"
#include <iostream.h>

struct Daisy::Implementation
{
    int day;
    int hour;
    Implementation () : day (0), hour (0) { }
};

Daisy::Daisy (const Input& input)
    : impl (*new Implementation), 
      log (input.makeLog ()), 
      manager (input.makeManager ()),
      wheather (input.makeWheather ()), 
      columns (input.makeColumns ()),
      crops (input.makeCrops ())
{ }

void 
Daisy::run ()
{ 
    while (true)
	{
	    const Action* action
		= manager.action (columns, wheather, impl.day, impl.hour);
	    
	    if (action->stop ())
		break;
	    
	    cout << "Tick " << impl.day << ":" << impl.hour << " ";

	    action->doIt (columns, wheather, crops);

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
				     wheather, impl.day, impl.hour);
		}

	    if (++impl.hour > 23)
		{
		    impl.hour = 0;
		    impl.day++;
		}
	}
}

Daisy::~Daisy ()
{
    delete &manager;
    delete &wheather;
    delete &log;
    delete &impl;
    for (ColumnList::iterator column = columns.begin();
	 column != columns.end();
	 column++)
	delete *column;
}
