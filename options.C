// options.C --- Parsing command line options.

#include "options.h"
#include "parser_file.h"
#include "document.h"
#include "syntax.h"
#include "alist.h"
#include "treelog_stream.h"
#include "version.h"
#include "message.h"

const char *const Options::log_name = "DAISY_LOG";
const char *const Options::path_name = "DAISYPATH";

string
Options::get_arg (int& argc, char**& argv)
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
	  TreelogStream treelog (CERR);
	  Treelog::Open nest (treelog, "Parsing");
	  ParserFile parser (syntax, arg, treelog);
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
		      TreelogStream treelog (CERR);
		      Treelog::Open nest (treelog, name);
		      if (syntax.check (alist, treelog))
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

// options.C ends here.
