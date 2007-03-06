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

#include "toplevel.h"
#include "syntax.h"
#include "alist.h"
#include "treelog_dual.h"
#include "options.h"
#include "assertion.h"
#include <stdexcept>
#include <iostream>
#include <time.h>

int
main (int argc, char* argv[])
{
  // We don't use stdio.
  std::ios::sync_with_stdio (false);

  time_t start_time = time (NULL);
#if defined (__unix) || defined (__CYGWIN__)
  TreelogDual msg ("daisy.log", std::cerr);
#else // MSDOS
  // stderr can't be redirected under MSDOS
  TreelogDual msg ("daisy.log", std::cout);
#endif // MSDOS

  Assertion::Register reg (msg);

  try
    {
      // Initialize syntax and attribute list.
      Syntax syntax;
      AttributeList alist;
      Toplevel::load_syntax (syntax, alist);
      Options options (argc, argv, syntax, alist, msg);
      options.copyright (msg);
      Toplevel toplevel (syntax, alist, msg);

      const std::string when 
        = std::string ("Program started ") + ctime (&start_time);
      std::ostringstream start_msg;
      start_msg << when.substr (0, when.size () - 1);
      const time_t time_ago = time (NULL) - start_time;
      if (time_ago == 0)
	start_msg << ".";
      if (time_ago == 1)
	start_msg << ", 1 second ago.";
      else
	start_msg << ", " << time_ago << " seconds ago.";
      msg.message (start_msg.str ());

      toplevel.run (msg);

      const time_t time_used = time (NULL) - start_time;
      const int hours = time_used / 3600;
      const int minutes = (time_used % 3600) / 60;
      const int seconds = time_used % 60;
      std::ostringstream end_msg;
      end_msg << "Program finished after ";
      if (hours == 1)
	end_msg << "1 hour, ";
      else if (hours > 0)
	end_msg << hours << " hours, ";
      if (minutes == 1)
	end_msg << " 1 minute and ";
      else if (hours > 0 || minutes > 0)
	end_msg << minutes << " minutes and ";
      if (seconds == 1)
	end_msg << "1 second.";
      else
	end_msg << seconds << " seconds.";
      msg.message (end_msg.str ());
      
      // All is well.
      throw EXIT_SUCCESS;

    }
  catch (const char* error)
    {
      msg.error (std::string ("Exception: ") + error);
    }
  catch (const std::string& error)
    {
      msg.error (std::string ("Exception raised: ") + error);
    }
  catch (const std::exception& e)
    {
      msg.error (std::string ("Standard exception: ") + typeid (e).name ()
		     + ": " + e.what ());
    }
  catch (const int exit_code)
    {
      // The program already reported the error, just exit.
      return exit_code;
    }
  catch (...)
    {
      msg.error ("Unknown exception");
    }
  return EXIT_FAILURE;
}
      
