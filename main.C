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
#include "tmpstream.h"
#include <stdexcept>
#include <typeinfo>
#include <iostream>
#include <time.h>

int
main (int argc, char* argv[])
{
  // We don't use stdio.
  ios::sync_with_stdio(false);

  time_t start_time = time (NULL);
#if defined (__unix) || defined (__CYGWIN__)
  TreelogDual treelog ("daisy.log", cerr);
#else // MSDOS
  // stderr can't be redirected under MSDOS
  TreelogDual treelog ("daisy.log", cout);
#endif // MSDOS

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
      Daisy daisy (alist);
      daisy.initialize (syntax, treelog);
      if (!daisy.check (treelog))
	return 1;

      const string when = string ("Simulation started ") + ctime (&start_time);
      TmpStream start_msg;
      start_msg () << when.substr (0, when.size () - 1);
      const time_t time_ago = time (NULL) - start_time;
      if (time_ago == 0)
	start_msg () << ".";
      if (time_ago == 1)
	start_msg () << ", 1 second ago.";
      else
	start_msg () << ", " << time_ago << " seconds ago.";
      treelog.message (start_msg.str ());

      daisy.run (treelog);

      const time_t time_used = time (NULL) - start_time;
      const int hours = time_used / 3600;
      const int minutes = (time_used % 3600) / 60;
      const int seconds = time_used % 60;
      TmpStream end_msg;
      end_msg () << "Simulation done after ";
      if (hours == 1)
	end_msg () << "1 hour, ";
      else if (hours > 0)
	end_msg () << hours << " hours, ";
      if (minutes == 1)
	end_msg () << " 1 minute and ";
      else if (hours > 0 || minutes > 0)
	end_msg () << minutes << " minutes and ";
      if (seconds == 1)
	end_msg () << "1 second.";
      else
	end_msg () << seconds << " seconds.";
      treelog.message (end_msg.str ());
      
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
  catch (const int error)
    {
      return error;
    }
  catch (...)
    {
      treelog.error ("Unknown exception");
    }
  return 1;
}
      
