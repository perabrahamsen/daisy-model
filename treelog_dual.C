// treelog_dual.C -- Log hierarchical information in two streams.
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


#include "treelog_dual.h"
#include "assertion.h"
// BCC5.01 lack ostream.
#include <iostream>
#include <fstream>
#include <deque>
// GCC 2.95.2 need a ".h".
#ifdef __unix
#include <unistd.h>
#else
#include <dir.h>
#endif

struct TreelogDual::Implementation
{
  const string file;
  string directory;
  ostream* one;
  ostream& two;
  deque<string> path;
  deque<bool> touched;

  void entry (const string& text);

  void flush ();

  Implementation (const string& filename, ostream& second)
    : file (filename),
      one (NULL),
      two (second)
  { }
  
  ~Implementation ()
  {
    if (one)
      {
	if (!one->good ())
	  two << "Problems writing `" << file << "' in '" << directory << "\n";
	else
	  two << "Wrote '" << file << "' in '" << directory << "'\n";
	delete one;
      }
    daisy_assert (path.size () == 0);
    daisy_assert (touched.size () == 0);
  }
};

void
TreelogDual::Implementation::entry (const string& text)
{
  if (!one)
    {
      char buffer[256];
      directory = getcwd (buffer, 255);
      one = new ofstream (file.c_str ());
      daisy_assert (one);
      if (!one->good ())
	two << "Problems opening `" << file << "' in '" << directory << "\n";
    }

  for (unsigned int i = 0; i < touched.size (); i++)
    {
      if (!touched[i])
	{
	  touched[i] = true;
	  for (unsigned int j = 0; j <= i; j++)
	    {
	      *one << "*";
	      two << "*";
	    }
	  *one << " " << path[i] << "\n";
	  two << " " << path[i] << "\n";
	}
    }
  *one << text << "\n";
  two << text << "\n";
}

void 
TreelogDual::Implementation::flush ()
{
  if (one)
    one->flush ();
  two.flush ();
}

void
TreelogDual::open (const string& name)
{ 
  impl.path.push_back (name); 
  impl.touched.push_back (false); 
}

void
TreelogDual::close ()
{
  impl.path.pop_back (); 
  impl.touched.pop_back (); 
}

void
TreelogDual::entry (const string& text)
{
  impl.entry (text);
  Treelog::entry (text);
}

void
TreelogDual::flush ()
{ impl.flush (); }

TreelogDual::TreelogDual (const string& file, ostream& two)
  : impl (*new Implementation (file, two))
{ }

TreelogDual::~TreelogDual ()
{ delete &impl; }

