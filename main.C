// main.C

#include "daisy.h"
#include "input.h"

int 
main (int argc, char* argv[])
{
    try 
	{
	    Input input (argc, argv, cerr);
	    Daisy daisy (input);
	    daisy.run ();
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
    exit (0);
}
