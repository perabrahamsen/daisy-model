// common.C --- Portability hacks.

#include "common.h"
#include "parser_file.h"
#include "document.h"
#include "syntax.h"
#include "alist.h"
#include "version.h"
#include <vector>
#include <fcntl.h>
#include <iostream>

#ifdef MINGW

extern "C" {

#include <reent.h>

/* Note that there is a copy of this in sys/reent.h.  */
#ifndef __ATTRIBUTE_IMPURE_PTR__
#define __ATTRIBUTE_IMPURE_PTR__
#endif

#ifndef __ATTRIBUTE_IMPURE_DATA__
#define __ATTRIBUTE_IMPURE_DATA__
#endif

static struct _reent __ATTRIBUTE_IMPURE_DATA__ impure_data = _REENT_INIT (impure_data);
struct _reent *__ATTRIBUTE_IMPURE_PTR__ _impure_ptr = &impure_data;

#include <ctype.h>

int __errno = 0;

_CONST char _ctype_[1 + 256] = {
	0,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C|_S,	_C|_S,	_C|_S,	_C|_S,	_C|_S,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_S|_B,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_N,	_N,	_N,	_N,	_N,	_N,	_N,	_N,
	_N,	_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_P,	_P,	_P,	_P,	_P,
	_P,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_P,	_P,	_P,	_P,	_C
};

_CONST char _imp___ctype_ [1 + 256] = {
	0,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C|_S,	_C|_S,	_C|_S,	_C|_S,	_C|_S,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_S|_B,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_N,	_N,	_N,	_N,	_N,	_N,	_N,	_N,
	_N,	_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_P,	_P,	_P,	_P,	_P,
	_P,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_P,	_P,	_P,	_P,	_C
};

void
_DEFUN (__assert, (file, line, failedexpr),
	const char *file _AND
	int line _AND
	const char *failedexpr)
{
  cerr << "assertion \"" << failedexpr << "\" failed: file \""
       <<file << "\", line " << line << "\n";
  abort();
  /* NOTREACHED */
}

}

#endif

extern "C" int chdir (const char *path);

#ifdef BORLAND_ASSERT
extern "C"
{
  void _RTLENTRY _EXPFUNC _assert(char * __cond, char * __file, int __line)
  {
    CERR << __file << ":" << __line << ": `" << __cond 
	 << "' assertion failed\n";
    exit (1);
  }
}
#endif

#ifndef __unix
#define MESSAGE_LOG
#endif

#ifdef MESSAGE_LOG
#include <fstream>
ofstream message_log (getenv ("DAISY_LOG") ? getenv ("DAISY_LOG") : "nul");
#endif

ostream& 
Options::message ()
{
#ifdef MESSAGE_LOG
  if (getenv ("DAISY_LOG"))
    return message_log;
#endif
  return cout; 
}

ostream& 
Options::warning ()
{ return error (); }

ostream& 
Options::error ()
{ 
#ifdef MESSAGE_LOG
  if (getenv ("DAISY_LOG"))
    return message_log;
#endif

#ifdef USELESS_STDERR
  return cout;
#else
  return cerr; 
#endif
}

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
#ifndef __unix__
      || name[1] == ':'
#endif
      )
    {
      int fd = open (name.c_str (), O_RDONLY);
      if (fd < 0)
	{
	  CERR << "Could not open `" << name << "' for reading\n";
	  throw ("file error");
	}
      return fd;
    }
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const string file = path[i] + DIRECTORY_SEPARATOR + name;
      const int fd = open (file.c_str (), O_RDONLY);
      if (fd >= 0)
	return fd;
    }
  CERR << "Could not find `" << name << "' in";
  for (unsigned int i = 0; i < path.size (); i++)
    CERR << " `" << path[i] << "'";
  CERR << "\n";
  throw ("file error");
  return -1;
}


static string
get_arg (int& argc, char**& argv)
{
  assert (argc > 1);
  const string arg = argv[1];

  // Update argc and argv.
  for (int i = 2; i < argc; i++)
    argv[i - 1] = argv[i];
  argv[argc - 1] = NULL;
  argc--;

  return arg;
}

void
Options::usage () const
{
  CERR << "Usage: " << program_name << " [-p type] [-v] [-d dir] file...\n";
}

Options::Options (int& argc, char**& argv,
		  Syntax& syntax, AttributeList& alist)
  : program_name (argv[0])
{
  if (argc < 2)
    {
      // Usage.
      argc = -2;
      return;
    }
  bool file_found = false;
  bool options_finished = false;
  int errors_found = 0;
  while (argc > 1)
    {
      const string arg = get_arg (argc, argv);

      if (arg.size () < 1)
	{
	  argc = -2;
	  return;
	}
      else if (options_finished || arg[0] != '-')
	{
	  // Parse the file.
	  ParserFile parser (syntax, arg);
	  parser.load (alist);
	  file_found = true;
	  errors_found += parser.error_count ();
	}
      else if (arg.size () < 1)
	{
	  argc = -2;
	  return;
	}
      else
	{ 
	  // Parse options.
	  switch (arg[1])
	    {
	    case 'd':
	      if (argc > 1)
		// Change directory.
		{
		  const string dir = get_arg (argc, argv);
		  if (chdir (dir.c_str ()) != 0)
		    CERR << program_name << ":chdir (" << dir << ") failed\n";
		}
	      else
		// Usage.
		argc = -2;
              break;
	    case 'p':
	      if (argc > 1)
		{
		  const Library& library 
		    = Librarian<Document>::library ();
		  const string name = get_arg (argc, argv);
		  if (library.check (name))
		    {
		      const Syntax& syntax = library.syntax (name);
		      AttributeList alist;
		      alist.add ("type", name);
		      if (syntax.check (alist, name))
			{
			  Document& document = 
			    Librarian<Document>::create (alist);
			  document.print_document (COUT);
			  delete &document;
			}
		    }
		  else
		    {
		      CERR << program_name << ": `" << name 
			   << "' unknown document type\n";
		      argc = -2;
		    }
		}
	      else
		argc = -2;

	      break;
	    case 'v':
	      // Print version.
	      COUT << "Daisy crop/soil simulation version "
		   << version << ". (" __DATE__ ")\n"
		"Copyright 1996 - 2000 Per Abrahamsen\n"
		"Copyright 1996, 1999 Søren Hansen\n";
	      break;
	    case '-':
	      // Finish option list.
	      options_finished = true;
	      break;
	    default:
	      // Usage.
	      argc = -2;
	      break;
	    }
	}
    }
  if (errors_found > 0)
    argc = -1;

  if (!file_found)
    // Done.
    argc = 0;
}

// common.C ends here.
