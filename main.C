// main.C

#include "daisy.h"
#include "input.h"

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
	    exit (2);
	}
    catch (const exception& except)
	{
	    cerr << except.what () << "\n";
	    exit (1);
	}
    catch (...)
	{ 
	    cerr << "Unexpected exception." << "\n";
	    cerr.flush ();
	    abort ();
	}
#endif HANDLE_EXCEPTIONS
    exit (0);
}
