// options.C --- handle options from the system ebvironment.

#include "options.h"
#include <vector>

int
Options::find_file (const string name)
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
	  const int next = colon_path.find (":", last);
	  if (next < 0)
	    break;
	  path.push_back (colon_path.substr (last, next - last));
	  last = next + 1;
	}
      path.push_back (colon_path.substr (last));
    }
  assert (path.size () > 0);
  if (name[0] == '.' || name[0] == '/')
    {
      const char *const str = name.c_str ();
      const int fd = open (str, O_RDONLY);
#ifndef BORLAND_C_STR
      delete str;
#endif
      return fd;
    }
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const string file = path[i] + "/" + name;
      const char *const str = file.c_str ();
      const int fd = open (str, O_RDONLY);
#ifndef BORLAND_C_STR
      delete str;
#endif
      if (fd >= 0)
	return fd;
    }
  cerr << "Could not find `" << name << "' in";
  for (unsigned int i = 0; i < path.size (); i++)
    cerr << " `" << path[i] << "'";
  cerr << "\n";
  return -1;
}
