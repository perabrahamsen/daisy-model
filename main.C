// main.C

#include "daisy.h"
#include "input.h"
#include "syntax.h"
#include "log.h"
#include "alist.h"
#include <iostream.h>

int 
main (int argc, char* argv[])
{
#ifdef HANDLE_EXCEPTIONS
  try 
    {
#endif HANDLE_EXCEPTIONS
      Syntax syntax;
      Daisy::load_syntax (syntax);
      const AttributeList& alist = parse (syntax, argc, argv);
      if (syntax.check (alist, "daisy"))
	{
	  Log log (alist.list_sequence ("log"));
	  Daisy daisy (log, alist);
	  daisy.run ();
	}
      delete &alist;
#ifdef HANDLE_EXCEPTIONS
    }
  catch (const Usage& usage)
    {
      cerr << usage.what () << "\n";
      return 2;
    }
  catch (const exception& except)
    {
      cerr << except.what () << "\n";
      return 1;
    }
  catch (...)
    { 
      cerr << "Unexpected exception." << "\n";
      cerr.flush ();
      abort ();
    }
#endif HANDLE_EXCEPTIONS
  return 0;
}
