// destination.C --- Destrination for a Select variable.
// 
// Copyright 2003 Per Abrahamsen and KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "destination.h"

using namespace std;

Destination::Destination ()
{ }

Destination::~Destination ()
{ }

void
MultiDest::error ()
{
  for (vector<Destination*>::iterator i = destinations.begin ();
       i != destinations.end ();
       i++)
    (*i)->error ();
}

void
MultiDest::missing ()
{
  for (vector<Destination*>::iterator i = destinations.begin ();
       i != destinations.end ();
       i++)
    (*i)->missing ();
}

void
MultiDest::add (const vector<double>& value)
{
  for (vector<Destination*>::iterator i = destinations.begin ();
       i != destinations.end ();
       i++)
    (*i)->add (value);
}

void
MultiDest::add (const double value)
{
  for (vector<Destination*>::iterator i = destinations.begin ();
       i != destinations.end ();
       i++)
    (*i)->add (value);
}

void
MultiDest::add (const symbol value)
{
  for (vector<Destination*>::iterator i = destinations.begin ();
       i != destinations.end ();
       i++)
    (*i)->add (value);
}

void
MultiDest::add_dest (Destination* dest)
{ destinations.push_back (dest); }

MultiDest::MultiDest ()
{ }

MultiDest::~MultiDest ()
{ }
