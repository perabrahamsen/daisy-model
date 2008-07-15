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
#include <assert.h>

void
exit_on_failure (daisy_daisy* toplevel)
{
  if (!daisy_daisy_ok (toplevel))
    {
      daisy_daisy_delete (toplevel);
      exit (EXIT_FAILURE);
    }
}

static const int one_column_a_time = 0;

int 
main (int argc, char* argv[])
{
  printf ("Daisy version %s\n", daisy_version ());

  /* Declarations. */
  daisy_daisy* toplevel;
  const daisy_scope* scope = NULL;

  /* We need exactly one argument. */
  if (argc != 2)
    {
      fprintf (stderr,"Usage: %s file\n", argv[0]);
      exit (2);
    }

  /* Create a top level that logs to a daisy.log file. */
  toplevel = daisy_daisy_create ();
  assert (toplevel);

  /* Parse thecommand line. */
  daisy_daisy_parse_command_line (toplevel, argc, argv);
  if (daisy_daisy_done (toplevel))
    {                           /* We might be done now. */
      daisy_daisy_delete (toplevel);
      exit (EXIT_SUCCESS);
    }
  exit_on_failure (toplevel);

  /* Initialize */
  daisy_daisy_initialize (toplevel);
  exit_on_failure (toplevel);

  if (!daisy_daisy_is_daisy (toplevel))
    {
      /* Not a Daisy simulation, just run it. */
      daisy_daisy_run (toplevel);
      exit (daisy_daisy_done (toplevel) ? EXIT_SUCCESS : EXIT_FAILURE);
    }
  /* Run the simulation. */
  {
    const daisy_time *const time = daisy_daisy_get_time (toplevel);
    const int columns = daisy_daisy_count_columns (toplevel);
    int i;

    printf ("Starting simulation.\n");
    daisy_daisy_start (toplevel);
    exit_on_failure (toplevel);

    // Find a scope named 'check'.
    
    if (daisy_daisy_scope_extern_size (toplevel) < 1)
      printf ("No scope found\n");
    else
      scope = daisy_daisy_scope_extern_get(toplevel, 0);
    exit_on_failure (toplevel);

    while (daisy_daisy_is_running (toplevel))
      {
        if (one_column_a_time)
          {
            daisy_daisy_tick_before (toplevel);
            exit_on_failure (toplevel);
        
            for (i = 0; i < columns; i++)
              {
#if 0
                daisy_column* column = daisy_daisy_get_column (toplevel, i);
#endif

                daisy_daisy_tick_column (toplevel, i);
                exit_on_failure (toplevel);
              }

            daisy_daisy_tick_after (toplevel);
          }
	else
          // Just do all everything at once.
          daisy_daisy_tick (toplevel);

        exit_on_failure (toplevel);
        
        if (daisy_time_get_hour (time) == 0)
	  printf ("%04d-%02d-%02d\n", 
		  daisy_time_get_year (time),
		  daisy_time_get_month (time),
		  daisy_time_get_mday (time));

	if (scope)
	  {
	    if (daisy_scope_has_number (scope, "height"))
	      {
		printf ("Height %g [%s]\n", 
			daisy_scope_number(scope, "height"),
			daisy_scope_dimension (scope, "height"));
	      }
            exit_on_failure (toplevel);
	  }
      }
    printf ("Simulation end.\n");
  }
  exit_on_failure (toplevel);

  /* Cleanup. */
  daisy_daisy_delete (toplevel);

  /* All is well. */
  exit (EXIT_SUCCESS);
}
