// main.C -- Run the Daisy model from the command line.
//
// Copyright 1996-2001 Per Abrahamsen.
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include "treelog_stream.h"
#include "options.h"
#include <stdexcept>
#include <typeinfo>

#include "message.h"

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
      TreelogStream treelog (CERR);
      Treelog::Open nest (treelog, "Daisy");
      if (!syntax.check (alist, treelog))
	return 1;

      // Create, check and run the simulation.
      Daisy daisy (alist);
      daisy.initialize (syntax, treelog);

      if (!daisy.check (treelog))
	return 1;
      daisy.run ();

      // All is well.
      return 0;
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
    }
  catch (const string& error)
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
