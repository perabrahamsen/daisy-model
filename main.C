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
      pair<Log*, const AttributeList*> init = parse (syntax, argc, argv);
      const AttributeList& alist = *init.second;
      Log& log = *init.first;
      if (syntax.check ("daisy", alist, log))
	{
	  Daisy daisy (log, alist);
	  daisy.run ();
	}
      delete init.first;
      delete init.second;
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
