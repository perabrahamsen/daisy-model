// cmain.c --- C version of the Daisy main program.

#include "cdaisy.h"
#include <stdio.h>

int 
main (int argc, char* argv[])
{
  /* Declarations. */
  daisy_syntax* syntax;
  daisy_alist* alist;
  daisy_parser* parser;
  daisy_daisy* daisy;

  /* We need exactly one argument. */
  if (argc != 2)
    {
      fprintf (stderr,"Usage: %s file\n", argv[0]);
      return 2;
    }

  /* Link and initialize the daisy subsystem. */
  daisy_initialize ();

  /* Initialize syntax and attribute list. */
  syntax = daisy_syntax_create ();
  alist = daisy_alist_create ();
  daisy_load (syntax, alist);
  
  /* Parse the file. */
  parser = daisy_parser_create_file (syntax, argv[1]);
  daisy_parser_load (parser, alist);
  
  /* Check the result. */
  if (!daisy_syntax_check (syntax, alist, "daisy"))
    return 1;

  /* Create, check and run the simulation. */
  daisy = daisy_daisy_create (syntax, alist);
  if (!daisy_daisy_check (daisy))
    return 1;

  /* Run the simulation. */
  {
    
    daisy_weather* weather = daisy_daisy_get_weather (daisy);
    const daisy_time *const time = daisy_daisy_get_time (daisy);
    const int columns = daisy_daisy_count_columns (daisy);
    int i;

    printf ("Starting simulation.\n");
    daisy_daisy_start (daisy);

    while (daisy_daisy_is_running (daisy))
      {
        daisy_daisy_tick_action (daisy);
        daisy_daisy_tick_weather (daisy);
        
        for (i = 0; i < columns; i++)
          {
            /* daisy_column* column = daisy_daisy_get_column (daisy, i); */
            daisy_daisy_tick_column (daisy, i);
          }

        daisy_daisy_tick_logs (daisy);
        daisy_daisy_tick_time (daisy);

        if (daisy_time_get_hour (time) == 0)
          printf ("%04d-%02d-%02d.\n", 
                  daisy_time_get_year (time),
                  daisy_time_get_month (time),
                  daisy_time_get_mday (time));
      }
    printf ("Simulation end.\n");
  }

  /* Cleanup. */
  daisy_syntax_delete (syntax);
  daisy_alist_delete (alist);
  daisy_parser_delete (parser);
  daisy_daisy_delete (daisy);

  /* All is well. */
  return 0;
}
