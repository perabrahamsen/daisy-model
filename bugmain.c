#include "cdaisy.h"
#include <stdio.h>

union 
{
  int a;
  
  struct 
  {
    double b;
    char c;
  } d;
} u;

int main (int argc, char* argv[]) 
{
#if 0
  int i;
  daisy_alist *manager, *action, *para, *time  ;
  daisy_syntax *syntax;
  daisy_time *date;
  manager       = daisy_alist_create();
  for (i = 0; i<10;i++) 
    {
      action = daisy_alist_create();
      daisy_alist_set_integer(action,"Id",i+1);
      daisy_alist_set_string(action,"Type","Test");
      daisy_alist_set_flag (action, "Flag", 1);
      para = daisy_alist_create();
      time = daisy_alist_create();
      daisy_alist_set_alist(action,"Params",para);
      daisy_alist_set_alist(action,"Time",time);
      daisy_alist_set_alist_at(manager,"Action",action,i);
    }
  for (i = 0; i < daisy_alist_size_alist (manager, "Action"); i++)
    {
      action = daisy_alist_get_alist_at (manager, "Action", i);
      printf ("%d: %s\n", i, daisy_alist_get_string (action, "Type"));
    }
  
  /* Try to save a flag. */
  action = daisy_alist_create ();
  syntax = daisy_syntax_create ();

  daisy_syntax_add (syntax, "Flag", 
		    daisy_category_number ("Const"),
		    daisy_type_number ("Boolean"),
		    daisy_size_singleton ());
  daisy_alist_set_flag (action, "Flag", 1);

  daisy_syntax_add (syntax, "Date", 
		    daisy_category_number ("Const"),
		    daisy_type_number ("Date"),
		    daisy_size_singleton ());
  date = daisy_time_create (1998, 2, 18, 9);
  daisy_alist_set_time (action, "Date", date);

  daisy_alist_save (action, syntax, "/dev/stderr");
  return 0;
#else
  /* Declarations. */
  daisy_syntax *syntax;
  daisy_alist *alist;
  daisy_parser *parser;
  daisy_daisy *daisy;
  daisy_printer* printer;

  /* We need exactly one argument. */
  if (argc != 3)
    {
      fprintf (stderr,"Usage: %s in out\n", argv[0]);
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

  
  /* Print result. */
  printer = daisy_printer_create_file (argv[2]);
  daisy_printer_comment (printer, "\
bugmain (" __DATE__ "): test of library file printing.\n\
Ignore this line as well.\n\
");
  daisy_printer_library_file (printer, argv[1]);
  daisy_printer_comment (printer, "\n\
bugmain (" __DATE__ "): test of alist printing.\n\
Just ignore this line.\n\
");
  daisy_printer_alist (printer, alist, syntax);

  if (!daisy_printer_good (printer))
    fprintf (stderr, "save failed.");
  
  daisy_printer_delete (printer);

  /* Cleanup. */
  daisy_syntax_delete (syntax);
  daisy_alist_delete (alist);
  daisy_parser_delete (parser);
  daisy_daisy_delete (daisy);

  /* All is well. */
  return 0;
#endif
}
