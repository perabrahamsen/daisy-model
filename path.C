// path.C -- Handle Unix and WinDOS style paths.

#include "path.h"

// Get chdir.
#ifdef __BORLANDC__
#include <dir.h>
#elif defined ( __GNUC__ )
#include <unistd.h>
#endif

#include <fstream>
#include <iostream>

using namespace std;

#if defined (__unix) 
#define DIRECTORY_SEPARATOR "/"
#else
#define DIRECTORY_SEPARATOR "\\"
#endif

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
};

istream& 
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
  assert (path);		// Must call set_path first.
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
  assert (path);		// Must call set_path first.
  value = *path;
}

// path.C ends here
