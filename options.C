// options.C --- Parsing command line options.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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


#include "options.h"
#include "parser_file.h"
#include "document.h"
#include "syntax.h"
#include "alist.h"
#include "treelog_stream.h"
#include "version.h"
#include "path.h"

#include <iostream>
using namespace std;

#if defined (__unix) 
#define PATH_SEPARATOR ":"
#else
#define PATH_SEPARATOR ";"
#endif

const char *const Options::log_name = "DAISY_LOG";

string
Options::get_arg (int& argc, char**& argv)
{
  daisy_assert (argc > 1);
  const string arg = argv[1];

  // Update argc and argv.
  for (int i = 2; i < argc; i++)
    argv[i - 1] = argv[i];
  argv[argc - 1] = NULL;
  argc--;

  return arg;
}

void
Options::usage (Treelog& out) const
{
  out.error (string ("Usage: ")
	     + program_name + " [-p type] [-v] [-d dir] file...");
}

void
Options::copyright (Treelog& out)
{
  out.message (string ("Daisy crop/soil simulation version ")
	       + version + ". (" __DATE__ ")\n"
	       "Copyright 1996 - 2002 Per Abrahamsen, "
	       "Søren Hansen and KVL.");
}

void 
Options::initialize_path ()
{
  vector<string> path;

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
  daisy_assert (path.size () > 0);
  Path::set_path (path);
}

Options::Options (int& argc, char**& argv,
		  Syntax& syntax, AttributeList& alist, Treelog& out)
  : program_name (argv[0])
{
  string command_line;
  for (unsigned int i = 0; i < argc; i++)
    {
      if (i > 0)
	command_line += " ";
      command_line += argv[i];
    }
  
  if (argc < 2)
    {
      // Usage.
      argc = -2;
      return;
    }
  initialize_path ();
  bool file_found = false;
  bool options_finished = false;
  bool prevent_run = false;
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
	  Treelog::Open nest (out, string ("Parsing ") + arg);
	  ParserFile parser (syntax, arg, out);
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
		  if (!Path::set_directory (dir))
		    out.error (program_name + ": chdir (" + dir + ") failed");
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
		      Treelog::Open nest (out, name);
		      if (syntax.check (alist, out))
			{
			  Document& document = 
			    Librarian<Document>::create (alist);
			  document.print_document (cout);
			  delete &document;
			}
		      prevent_run = true;
		    }
		  else
		    {
		      out.error (program_name + ": `" + name 
				 + "' unknown document type");
		      argc = -2;
		    }
		}
	      else
		argc = -2;

	      break;
	    case 'v':
	      // Print version.
	      copyright (out);
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

  if (argc > 0 && (!file_found || prevent_run))
    // Done.
    argc = 0;

  out.debug (command_line);
}

// options.C ends here.
