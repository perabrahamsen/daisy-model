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
#include "treelog_dual.h"
#include "options.h"
#include "assertion.h"
#include <stdexcept>
#include <typeinfo>
#include <iostream>

int
main (int argc, char* argv[])
{
  TreelogDual treelog ("daisy.log", cerr);
  Assertion::Register reg (treelog);

  try
    {
      // Initialize syntax and attribute list.
      Syntax syntax;
      AttributeList alist;
      Daisy::load_syntax (syntax, alist);
      Library::load_syntax (syntax, alist);

      Options options (argc, argv, syntax, alist, treelog);

      switch (argc)
	{
	case -2:
	  options.usage (treelog);
	  return 2;
	case -1:
	  return 1;
	case 0:
	  return 0;
	case 1:
	  // Do nothing.
	  break;
	default:
	  daisy_assert (false);
	}

      // Check the result.
      { 
	Treelog::Open nest (treelog, "Daisy");
	if (!syntax.check (alist, treelog))
	  return 1;
      }
      // Create, check and run the simulation.
      Options::copyright (treelog);
      Options::timestamp (treelog);
      Daisy daisy (alist);
      daisy.initialize (syntax, treelog);
      if (!daisy.check (treelog))
	return 1;
      daisy.run (treelog);

      Options::timestamp (treelog);

      // All is well.
      return 0;
    }
  catch (const char* error)
    {
      treelog.error (string ("Exception: ") + error);
    }
  catch (const string& error)
    {
      treelog.error (string ("Exception raised: ") + error);
    }
  catch (const exception& e)
    {
      treelog.error (string ("Standard exception: ") + typeid (e).name ()
		     + ": " + e.what ());
    }
  catch (...)
    {
      treelog.error ("Unknown exception");
    }
  exit (1);
}
