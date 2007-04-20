// treelog_text.C-- Log hierarchical information as text.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#include "treelog_text.h"
#include "assertion.h"
#include <deque>
#include <iostream>
#include <sstream>
#include <fstream>

struct TreelogText::Implementation
{
  std::deque<std::string> path;
  std::deque<bool> touched;

  Implementation ()
  { }
  ~Implementation ()
  {
    daisy_assert (path.size () == 0);
    daisy_assert (touched.size () == 0);
  }
};

void
TreelogText::header ()
{
  for (unsigned int i = 0; i < impl->touched.size (); i++)
    {
      if (!impl->touched[i])
	{
	  impl->touched[i] = true;
	  for (unsigned int j = 0; j <= i; j++)
	    write ("*");
	  write (" ");
          write (impl->path[i]);
          write ("\n");
	}
    }
}

void
TreelogText::open (const std::string& name)
{ 
  impl->path.push_back (name); 
  impl->touched.push_back (false); 

}

void
TreelogText::close ()
{
  impl->path.pop_back (); 
  impl->touched.pop_back (); 
}

void
TreelogText::entry (const std::string& text)
{
  header ();
  write (text);
  write ("\n");
}

TreelogText::TreelogText ()
  : impl (new Implementation ())
{ }

TreelogText::~TreelogText ()
{ }

void 
TreelogProgress::write (const std::string& text)
{
#if defined (__unix) || defined (__CYGWIN__)
  std::cerr << text;
#else // MSDOS
    // stderr can't be redirected under MSDOS
  std::cout << text;
#endif // MSDOS
}

void 
TreelogProgress::debug (const std::string&) 
{ }

void 
TreelogProgress::touch ()
{ header (); }

void
TreelogProgress::flush ()
{
  std::cerr.flush ();
  std::cout.flush ();
}

TreelogProgress::TreelogProgress ()
{ }

TreelogProgress::~TreelogProgress ()
{ }

class TreelogString::Implementation : public std::ostringstream
{ };

void 
TreelogString::write (const std::string& text)
{ (*impl) << text; }

void 
TreelogString::debug (const std::string&) 
{ }

void 
TreelogString::touch ()
{ }

void
TreelogString::flush ()
{ }

const std::string
TreelogString::str () const
{ return impl->str (); }

TreelogString::TreelogString ()
  : impl (new Implementation)
{ }

TreelogString::~TreelogString ()
{ }

class TreelogFile::Implementation : public std::ofstream
{ 
public:
  Implementation (const std::string& name)
    : std::ofstream (name.c_str ())
  { }
};

void 
TreelogFile::write (const std::string& text)
{ (*impl) << text; }

void 
TreelogFile::debug (const std::string& text) 
{ message (text); }

void 
TreelogFile::touch ()
{ }

void
TreelogFile::flush ()
{ impl->flush (); }

TreelogFile::TreelogFile (const std::string& name)
  : impl (new Implementation (name))
{ }

TreelogFile::~TreelogFile ()
{ }

// treelog_text.C ends here.
