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
#include "w32reg.h"
#include "version.h"

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
#define PATH_SEPARATOR ":"
#else
#define DIRECTORY_SEPARATOR "\\"
#define PATH_SEPARATOR ";"
#endif

std::auto_ptr<std::istream> 
Path::open_file (const std::string& name) const
{
  struct Message : std::ostringstream 
  {
    ~Message ()
    { Assertion::debug (this->str ()); }
  } tmp;
  tmp << "In directory '" << get_directory () << ":";

  std::auto_ptr<std::istream> in;

  // Absolute filename.
  if (name[0] == '.' || name[0] == '/'
#ifndef __unix__
      || name[1] == ':'
#endif
      )
    {
      tmp << "\nOpening absolute file name '" << name << "'";
      in.reset (new std::ifstream (name.c_str ()));
      return in;
    }

  tmp << "\nLooking for file '" << name << "'";

  // Look in path.
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const std::string file = path[i] + DIRECTORY_SEPARATOR + name;
      tmp << "\nTrying '" << file << "'";
      in.reset (new std::ifstream (file.c_str ()));
      if (in->good ())
	{
	  tmp << " success!";
	  return in;
	}
    }
	 tmp << "\nGiving up";
       daisy_assert (in.get ());		
  return in;			// Return last bad stream.
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
Path::get_directory () const
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
  path = value;

  std::ostringstream tmp;
  tmp << "Path set to:";
  for (size_t i = 0; i < value.size (); i++)
    tmp << "\n" << i << ": '" << value[i] << "'";
  tmp << "\ndone";
  Assertion::debug (tmp.str ());
}

 void
Path::get_path (std::vector<std::string>& value) const
 { value = path; }
 
Path::InDirectory::InDirectory (Path& p, const std::string& to)
  : path (p),
    from (path.get_directory ()),
    ok (path.set_directory (to))
{ }

bool 
Path::InDirectory::check () const
{ return ok; }

Path::InDirectory::~InDirectory ()
{ path.set_directory (from); }

const std::string 
Path::nodir (const std::string& name)
{
  size_t start = name.size ();

  for (;start > 0; start--)
    {
      const char prev = name[start-1];

      if (prev == '/')
	break;
      
#if !defined (__unix) 
      if (prev == '\\' || prev == ':')
	break;
#endif // !unix
    }
  
  std::string result;

  for (;start < name.size (); start++)
    result += name[start];

  return result;
}

std::string
Path::get_daisy_home ()
{
  // Check DAISYHOME
  const char* daisy_home_env = getenv ("DAISYHOME");
  if (daisy_home_env)
    {
      Assertion::debug ("Has DAISYHOME environment variable");
      return daisy_home_env;
    }

  // Check MS Windows registry
#if defined (_WIN32) || defined (__CYGWIN32__)
  const std::string key = "Software\\Daisy " + std::string (version);
  char *const daisy_w32_reg 
    = read_w32_registry_string (NULL, key.c_str (), "Install Directory");
  if (daisy_w32_reg)
    {
      Assertion::debug ("Has '" + key + "' registry entry.");
      std::string result = daisy_w32_reg;
      free (daisy_w32_reg);
      return result;
    }
  Assertion::debug ("Using standard MS Windows home.");
  return "C:/daisy";
#else // !MS WINDOWS
  Assertion::debug ("Using standard Unix home.");
  return "/usr/local/daisy";
#endif // !MS WINDOWS
}

Path::Path ()
{
  // Initialize path.
  const char *const daisy_path_env = getenv ("DAISYPATH");
  if (daisy_path_env)
    {
      Assertion::debug ("Has DAISYPATH environment variable.");
      const std::string colon_path = daisy_path_env;
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
  else
    {
      const std::string daisy_home = get_daisy_home ();
      Assertion::debug ("Using '" + daisy_home + "' as daisy home.");
      path.push_back (".");
      path.push_back (daisy_home + "/lib");
      path.push_back (daisy_home + "/sample");
    }
  daisy_assert (path.size () > 0);
}

// path.C ends here
