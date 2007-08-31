// path.C -- Handle Unix and WinDOS style paths.
//
// Copyright 1996-2007 KVL, Per Abrahamsen and Søren Hansen
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

#define BUILD_DLL

#include "path.h"
#include "assertion.h"

// Get chdir.
#if defined (__unix)
#include <unistd.h>
#elif defined (__MINGW32__) || defined (_MSC_VER)
extern "C" int chdir (const char* dir);
extern "C" char *getcwd (char *buf, size_t size);
#else
#include <dir.h>
#endif
extern "C" int mkdir(const char *pathname, int mode);

#include <fstream>
#include <sstream>

#if defined (__unix) 
#define DIRECTORY_SEPARATOR "/"
#else
#define DIRECTORY_SEPARATOR "\\"
#endif

std::ostream& Path::Output::stream () const
{ return out; }

bool Path::Output::good () const
{ return owner && out.good (); }

Path::Output::Output (const std::string& file)
  : out (*new std::ofstream (file.c_str ())),
    owner (true)
{ }

Path::Output::~Output ()
{ 
  if (owner) 
    delete &out;
}

std::istream& 
Path::Input::stream () const
{ return in; }

bool 
Path::Input::good () const
{ return owner && in.good (); }

Path::Input::Input (const std::string& file)
  : in (open_file (file)),
    owner (true)
{ }

Path::Input::~Input ()
{ 
  if (owner)
    delete &in;
}

namespace Path
{
  // Relative filename, use path.
  static std::vector<std::string>* path = NULL;

  struct path_handler 
  { 
    path_handler ()		// Constructed by set_path.
    { }
    ~path_handler ()		// Delete memory at exit for leak detectors.
    { delete path; }
  } handle_path;
}

std::istream& 
Path::open_file (const std::string& name)
{
  struct Message : std::ostringstream 
  {
    ~Message ()
    { Assertion::debug (this->str ()); }
  } tmp;
  tmp << "In directory '" << get_directory () << ":";

  // Absolute filename.
  if (name[0] == '.' || name[0] == '/'
#ifndef __unix__
      || name[1] == ':'
#endif
      )
    {
      tmp << "\nOpening absolute file name '" << name << "'";
      return *new std::ifstream (name.c_str ());
    }

  tmp << "\nLooking for file '" << name << "'";

  // Look in path.
  daisy_assert (path);		// Must call set_path first.
  std::ifstream* in = NULL;
  for (unsigned int i = 0; i < path->size (); i++)
    {
      const std::string file = (*path)[i] + DIRECTORY_SEPARATOR + name;
      tmp << "\nTrying '" << file << "'";
      delete in;
      in = new std::ifstream (file.c_str ());
      if (in->good ())
	{
	  tmp << " success!";
	  return *in;
	}
    }
  tmp << "\nGiving up";
  daisy_assert (in);		
  return *in;			// Return last bad stream.
}

bool 
Path::set_directory (const std::string& directory)
{ 
  const char *const dir = directory.c_str ();
  const bool result 
    = chdir (dir) == 0 || (mkdir (dir, 0777) == 0 && chdir (dir) == 0); 
  
  std::ostringstream tmp;
  tmp << "Changing directory to '" << directory << "' " 
      << (result ? "success" : "failure");
  Assertion::debug (tmp.str ());

  return result;
}

const std::string 
Path::get_directory ()
{ 
  const size_t BUFFER_SIZE = 10000;
  char buffer[BUFFER_SIZE];
  char *wd = getcwd (buffer, BUFFER_SIZE);
  if (!wd)
    throw "Current directory path is too long";
  return wd; 
}

void 
Path::set_path (const std::vector<std::string>& value)
{ 
  if (!path)
    path = new std::vector<std::string> (value);
  else
   *path = value;

  std::ostringstream tmp;
  tmp << "Path set to:";
  for (size_t i = 0; i < value.size (); i++)
    tmp << "\n" << i << ": '" << value[i] << "'";
  tmp << "\ndone";
  Assertion::debug (tmp.str ());
  
}

void
Path::get_path (std::vector<std::string>& value)
{ 
  daisy_assert (path);		// Must call set_path first.
  value = *path;
}

Path::InDirectory::InDirectory (const std::string& to)
  : from (get_directory ()),
    ok (set_directory (to))
{ }

bool 
Path::InDirectory::check () const
{ return ok; }

Path::InDirectory::~InDirectory ()
{ set_directory (from); }

// path.C ends here
