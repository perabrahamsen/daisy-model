// main.C

#include "daisy.h"
#include "input.h"

#include <iostream.h>

int 
main (int argc, char* argv[])
{
#ifdef HANDLE_EXCEPTIONS
    try 
	{
#endif HANDLE_EXCEPTIONS
	    Input input (argc, argv, cerr);
	    Daisy daisy (input);
	    daisy.run ();
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
