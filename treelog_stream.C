// treelog_stream.C -- Log hierarchical information in an ostream.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "treelog_stream.h"
// BCC5.01 lack ostream.
#include <iostream>
#include <deque>
// GCC 2.95.2 need a ".h".
#include <assert.h>
1
struct TreelogStream::Implementation
{
  ostream& out;
  deque<string> path;
  deque<bool> touched;

  Implementation (ostream& o)
    : out (o)
  { }
  ~Implementation ()
  {
    assert (path.size () == 0);
    assert (touched.size () == 0);
  }
};

void
TreelogStream::open (const string& name)
{ 
  impl.path.push_back (name); 
  impl.touched.push_back (false); 

}

void
TreelogStream::close ()
{
  impl.path.pop_back (); 
  impl.touched.pop_back (); 
}

void
TreelogStream::entry (const string& text)
{
  Treelog::entry (text);
  for (unsigned int i = 0; i < impl.touched.size (); i++)
    {
      if (!impl.touched[i])
	{
	  impl.touched[i] = true;
	  for (unsigned int j = 0; j <= i; j++)
	    impl.out << "*";
	  impl.out << " " << impl.path[i] << "\n";
	}
    }
  impl.out << text << "\n";
}

void
TreelogStream::flush ()
{ impl.out.flush (); }

TreelogStream::TreelogStream (ostream& out)
  : impl (*new Implementation (out))
{ }

TreelogStream::~TreelogStream ()
{ delete &impl; }

