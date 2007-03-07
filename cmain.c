/* cmain.c --- C version of the Daisy main program.
  
   Copyright 1996-2001 Per Abrahamsen.
   Copyright 2000-2001 KVL.
  
   This file is part of Daisy.
   
   Daisy is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   Daisy is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with Daisy; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "cdaisy.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int 
main (int argc, char* argv[])
{
  /* Declarations. */
  daisy_syntax* syntax;
  daisy_alist* alist;
  daisy_parser* parser;
  daisy_daisy* daisy;
  const daisy_scope* scope;

  /* We need exactly one argument. */
  if (argc != 2)
    {
      fprintf (stderr,"Usage: %s file\n", argv[0]);
      exit (2);
    }

  /* Link and initialize the daisy subsystem. */
  daisy_initialize ();

  /* Check for -v */
  if (strcmp (argv[1], "-v") == 0)
    {
      fprintf (stderr, "Version %s\n", daisy_version ());
      exit (0);
    }

  /* Initialize syntax and attribute list. */
  syntax = daisy_syntax_create ();
  alist = daisy_alist_create ();
  daisy_load (syntax, alist);
  
  /* Parse the file. */
  parser = daisy_parser_create_file (syntax, argv[1]);
  daisy_parser_load (parser, alist);
  
  /* Check the result. */
  if (!daisy_syntax_check (syntax, alist, "daisy")
      || daisy_parser_error_count (parser) > 0)
    exit (1);

  /* Create, check and run the simulation. */
  daisy = daisy_daisy_create (syntax, alist);
  if (!daisy_daisy_check (daisy))
    exit (1);

  /* Run the simulation. */
  {
    
    const daisy_time *const time = daisy_daisy_get_time (daisy);
    const int columns = daisy_daisy_count_columns (daisy);
    int i;

    printf ("Starting simulation.\n");
    daisy_daisy_start (daisy);

    //
    scope = daisy_scope_find_extern ("check");
    if (scope)
      {
	printf ("check OK end.\n");
      }
    else printf ("check not recognized.\n");
    

    while (daisy_daisy_is_running (daisy))
      {
        daisy_daisy_tick_before (daisy);
        
        for (i = 0; i < columns; i++)
          {
            /* daisy_column* column = daisy_daisy_get_column (daisy, i); */
            daisy_daisy_tick_column (daisy, i);
          }

        daisy_daisy_tick_after (daisy);
	
        if (daisy_time_get_hour (time) == 0)
	  printf ("%04d-%02d-%02d\n", 
		  daisy_time_get_year (time),
		  daisy_time_get_month (time),
		  daisy_time_get_mday (time));

	if (scope)
	  {
	    if(daisy_scope_has_number (scope, "height"))
	      {
		printf ("Height %g [%s]\n", 
			daisy_scope_number(scope, "height"),
			daisy_scope_dimension (scope, "height"));
	      }
	    else  printf ("height not recognized.\n");
	  }
      }
    printf ("Simulation end.\n");
  }

  /* Cleanup. */
  daisy_syntax_delete (syntax);
  daisy_alist_delete (alist);
  daisy_parser_delete (parser);
  daisy_daisy_delete (daisy);

  /* All is well. */
  exit (0);
}
