// main.C

#include "daisy.h"
#include "parser_file.h"
#include "syntax.h"
#include "alist.h"
#include <iostream.h>

int 
main (int argc, char* argv[])
{
  if (argc != 2)
    {
      cerr << "Usage: " << argv[0] << " file\n";
      return 2;
    }
  Syntax syntax;
  Daisy::load_syntax (syntax);
  ParserFile parser (syntax, argv[1]);
  AttributeList alist;
  parser.load (alist);
  if (!syntax.check (alist, "daisy"))
    return 1;
  Daisy daisy (alist);
  daisy.run ();
  return 0;
}
