// mail.C

#include "daisy.h"

int 
main (int argc, char* argv[])
{
    Input input (argc, argv);
    Daisy daisy (input);
    daisy.run ();
}
