// daisy.C

#include "daisy.h"
#include <iostream.h>

struct Daisy::Implementation
{
    int day;
    int hour;
    Implementation () : day (0), hour (0) { }
};

Daisy::Daisy (const Input& input)
    : impl (*new Implementation), 
      log(input.makeLog ()), 
      manager (input.makeManager ()),
      wheather (input.makeWheather ())
{ 
    input.makeColumns (columns);
}

void 
Daisy::run ()
{ 
    while (!manager.stop (impl.day, impl.hour))
	{
	    cout << "Tick " << impl.day << ":" << impl.hour << "\n";

	    ColumnList::iterator prev;
	    ColumnList::iterator column = columns.end ();
	    ColumnList::iterator next = columns.begin ();

	    while (next != columns.end ())
		{
		    prev = column;
		    ColumnList::iterator column = next;
		    next++;

		    manager.manage(**column, wheather);
		    (*column)->tick (((prev != columns.end ()) ? *prev : 0),
				     ((next != columns.end ()) ? *next : 0),
				     wheather);
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
