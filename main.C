// main.C

#include "daisy.h"
#include "parser_file.h"
#include "syntax.h"
#include "alist.h"
#include "version.h"
#include "options.h"

int 
main (int argc, char* argv[])
{
#ifdef HANDLE_EXCEPTIONS
  try 
    {
#endif
      // We need exactly one argument.
      if (argc != 2)
	{
	  CERR << "Usage: " << argv[0] << " file\n";
	  return 2;
	}
      // Initialize syntax and attribute list.
      Syntax syntax;
      AttributeList alist;
      Daisy::load_syntax (syntax, alist);
      Library::load_syntax (syntax, alist);

      // Dump syntax, if specified.
      if (strcmp (argv[1], "-p") == 0)
	{
	  syntax.dump ();
	  return 0;
	}

      // print version, if specified.
      if (strcmp (argv[1], "-v") == 0)
	{
	  CERR << "Daisy crop/soil simulation version "
	       << version << ". (" __DATE__ ")\n"
	    "Copyright 1996 - 1998 Per Abrahamsen\n"
	    "Copyright 1996 Søren Hansen\n";
	  return 2;
	}

assert (1 == 2);

      // Parse the file.
      ParserFile parser (syntax, argv[1]);
      parser.load (alist);

      // Check the result.
      if (!syntax.check (alist, "daisy") || parser.error_count () > 0)
	return 1;

      // Create, check and run the simulation.
      Daisy daisy (alist);
      daisy.initialize (syntax);

      if (!daisy.check ())
	return 1;
      daisy.run ();

      // All is well.
      return 0;
#ifdef HANDLE_EXCEPTIONS
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
    }
  exit (1);
#endif
}
