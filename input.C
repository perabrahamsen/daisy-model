// input.C

#include "daisy.h"

Manager& 
Input::makeManager () const
{ 
    return *new Manager (log);
}

Wheather& 
Input::makeWheather () const 
{     
    return *new Wheather (log);
}

Log& 
Input::makeLog () const
{ 
    return log;
}

void 
Input::makeColumns (ColumnList& columns) const
{ 
    for (int i = 0; i < 3; i++)
	columns.push_back (new Column(log));
}

Input::Input (int& /* argc */, char**& /* argv */)
    : log (*new Log ())
{ }

