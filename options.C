// options.C --- handle options from the system ebvironment.

#include "options.h"
#include <vector>
#include <fcntl.h>

#ifdef __unix
#define PATH_SEPARATOR ":"
#define DIRECTORY_SEPARATOR "/"
#else
#define PATH_SEPARATOR ";"
#define DIRECTORY_SEPARATOR "\\"
#endif

int
Options::find_file (const string& name)
{
  static vector<string> path;
  if (path.size () == 0)
    {
      // Initialize path.
      const string colon_path
	= getenv ("DAISYPATH") ? getenv ("DAISYPATH") : ".";
      int last = 0;
      for (;;)
	{
	  const int next = colon_path.find (PATH_SEPARATOR, last);
	  if (next < 0)
	    break;
	  path.push_back (colon_path.substr (last, next - last));
	  last = next + 1;
	}
      path.push_back (colon_path.substr (last));
    }
  assert (path.size () > 0);
  if (name[0] == '.' || name[0] == '/'
#ifndef __unix
      || name[1] == ':'
#endif
      )
    {
      return open (name.c_str (), O_RDONLY);
    }
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const string file = path[i] + DIRECTORY_SEPARATOR + name;
      const int fd = open (file.c_str (), O_RDONLY);
      if (fd >= 0)
	return fd;
    }
  cerr << "Could not find `" << name << "' in";
  for (unsigned int i = 0; i < path.size (); i++)
    cerr << " `" << path[i] << "'";
  cerr << "\n";
  return -1;
}
