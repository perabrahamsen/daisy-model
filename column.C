// column.C

#include <iostream.h>
#include "daisy.h"

struct Column::Implementation
{ };

void Column::tick (Column* /* left */, Column* /* rigth */,
		   const Wheather& wheater)
{
    cout << "tick\n"; 
}

Column::Column (Log& l)
    : impl (*new Implementation ()),
      log (l)
{ }

Column::~Column ()
{ 
    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	delete *crop;
}

