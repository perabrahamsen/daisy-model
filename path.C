// path.C -- Handle Unix and WinDOS style paths.

#include "path.h"
#include "common.h"
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

#include <fstream>
#include <iostream>

using namespace std;

#if defined (__unix) 
#define DIRECTORY_SEPARATOR "/"
#else
#define DIRECTORY_SEPARATOR "\\"
#endif

std::ostream& Path::Output::stream () const
{ return out; }

bool Path::Output::good () const
{ return owner && out.good (); }

Path::Output::Output (const string& file)
  : out (*new ofstream (file.c_str ())),
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

Path::Input::Input (const string& file)
  : in (open_file (file)),
    owner (&in != &cin)
{ }

Path::Input::~Input ()
{ 
  if (owner)
    delete &in;
}

namespace Path
{
  // Relative filename, use path.
  static vector<string>* path = NULL;

  struct path_handler 
  { 
    path_handler ()		// Constructed by set_path.
    { }
    ~path_handler ()		// Delete memory at exit for leak detectors.
    { delete path; }
  } handle_path;
}

std::istream& 
Path::open_file (const string& name)
{
  // Absolute filename.
  if (name[0] == '.' || name[0] == '/'
#ifndef __unix__
      || name[1] == ':'
#endif
      )
    {
      return *new ifstream (name.c_str ());
    }

  // Look in path.
  daisy_assert (path);		// Must call set_path first.
  ifstream* in = NULL;
  for (unsigned int i = 0; i < path->size (); i++)
    {
      const string file = (*path)[i] + DIRECTORY_SEPARATOR + name;
      delete in;
      in = new ifstream (file.c_str ());
      if (in->good ())
	return *in;
    }
  return *in;
}

bool 
Path::set_directory (const string& directory)
{ return chdir (directory.c_str ()) == 0; }

const std::string 
Path::get_directory ()
{ 
  const size_t BUFFER_SIZE = 10000;
  char buffer[BUFFER_SIZE];
  char *wd = getcwd (buffer, BUFFER_SIZE);
  if (!wd)
    return "Current directory path is too long";
  return wd; 
}

void 
Path::set_path (const vector<string>& value)
{ 
  if (!path)
    path = new vector<string> (value);
  else
   *path = value;
}

void
Path::get_path (vector<string>& value)
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
