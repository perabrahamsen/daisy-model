// main.C

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <stdexcept>
#include <typeinfo>

int
main (int argc, char* argv[])
{
  try
    {
      // Initialize syntax and attribute list.
      Syntax syntax;
      AttributeList alist;
      Daisy::load_syntax (syntax, alist);
      Library::load_syntax (syntax, alist);

      Options options (argc, argv, syntax, alist);

      switch (argc)
	{
	case -2:
	  options.usage ();
	  return 2;
	case -1:
	  return 1;
	case 0:
	  return 0;
	case 1:
	  // Do nothing.
	  break;
	default:
	  assert (false);
	}

      // Check the result.
      if (!syntax.check (alist, CERR, "daisy"))
	return 1;

      // Create, check and run the simulation.
      Daisy daisy (alist);
      daisy.initialize (syntax);

      if (!daisy.check (CERR))
	return 1;
      daisy.run ();

      // All is well.
      return 0;
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
    }
  catch (const exception& e)
    {
      CERR << "Standard exception: " << typeid (e).name ()
	   << ": " << e.what () << "\n";
    }
  catch (...)
    {
      CERR << "Unknown exception\n";
    }
  exit (1);
}
