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
#if defined (__unix)
#include <unistd.h>
#elif defined (__MINGW32__)
extern "C" char* getcwd (char*, int);
#else
#include <dir.h>
#endif
using namespace std;

struct TreelogDual::Implementation
{
  const string file;
  string directory;
  std::ostream* one;
  std::ostream& two;
  deque<string> path;
  deque<bool> touched_one;
  deque<bool> touched_two;

  void touch (std::ostream& out, deque<bool>& touched);
  void print (std::ostream& out, deque<bool>& touched,
	      const string& text);
  void init_one ();
  void debug (const string& text);
  void entry (const string& text);

  void flush ();

  Implementation (const string& filename, std::ostream& second)
    : file (filename),
      one (NULL),
      two (second)
  { }
  
  ~Implementation ()
  {
    flush ();
    if (one)
      {
	if (!one->good ())
	  two << "Problems writing '" << file << "' in '" << directory << "\n";
	else
	  two << "Wrote '" << file << "' in '" << directory << "'\n";
	delete one;
      }
    daisy_assert (path.size () == 0);
    daisy_assert (touched_one.size () == 0);
    daisy_assert (touched_two.size () == 0);
  }
};

void
TreelogDual::Implementation::touch (std::ostream& out, deque<bool>& touched)
{
  for (unsigned int i = 0; i < touched.size (); i++)
    {
      if (!touched[i])
	{
	  touched[i] = true;
	  for (unsigned int j = 0; j <= i; j++)
	    out << "*";

	  out << " " << path[i] << "\n";
	}
    }
}

void
TreelogDual::Implementation::print (std::ostream& out, deque<bool>& touched, 
				    const string& text)
{
  touch (out, touched);
  out << text << "\n";
}

void
TreelogDual::Implementation::init_one ()
{
  if (!one)
    {
      char buffer[256];
      directory = getcwd (buffer, 255);
      one = new ofstream (file.c_str ());
      daisy_assert (one);
      if (!one->good ())
	{
	  two << "Problems opening '" << file << "' in '" << directory << "\n";
	  throw 3;
	}
    }
}

void
TreelogDual::Implementation::debug (const string& text)
{ 
  init_one ();
  print (*one, touched_one, text); 
}

void
TreelogDual::Implementation::entry (const string& text)
{
  print (two, touched_two, text);
  two.flush ();
  init_one ();
  print (*one, touched_one, text); 
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
  impl.touched_one.push_back (false); 
  impl.touched_two.push_back (false); 
}

void
TreelogDual::close ()
{
  impl.path.pop_back (); 
  impl.touched_one.pop_back (); 
  impl.touched_two.pop_back (); 
}

void
TreelogDual::debug (const string& text)
{
  impl.debug (text);
  Treelog::debug (text);
}

void
TreelogDual::entry (const string& text)
{
  impl.entry (text);
  Treelog::entry (text);
}

void
TreelogDual::touch ()
{ 
  impl.touch (impl.two, impl.touched_two);
  impl.two.flush ();
  impl.init_one ();
  impl.touch (*impl.one, impl.touched_one); 
}

void
TreelogDual::flush ()
{ impl.flush (); }

TreelogDual::TreelogDual (const string& file, std::ostream& two)
  : impl (*new Implementation (file, two))
{ }

TreelogDual::~TreelogDual ()
{ delete &impl; }
