// main.C -- Run the Daisy model from the command line.
//
// Copyright 1996-2001 Per Abrahamsen.
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
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
#include <stdexcept>
#include <typeinfo>

int
main (int argc, char* argv[])
{
  Toplevel toplevel;
  try
    {
      // toplevel.set_ui_progress ();
      toplevel.command_line (argc, argv);
      toplevel.user_interface ();

      switch (toplevel.state ())
        {
        case Toplevel::is_uninitialized:
          toplevel.initialize ();
          /* Fallthrough */;
        case Toplevel::is_ready:
          toplevel.run ();
          /* Fallthrough */;
        case Toplevel::is_done:
          throw EXIT_SUCCESS;
        case Toplevel::is_running:
          toplevel.error ("Aborted while simulation was running");
          throw EXIT_FAILURE;
        case Toplevel::is_error:
          throw EXIT_FAILURE;
        }
    }
  catch (const char* error)
    { toplevel.error (std::string ("Exception: ") + error); }
  catch (const std::string& error)
    { toplevel.error (std::string ("Exception raised: ") + error); }
  catch (const std::exception& e)
    {
      toplevel.error (std::string ("Standard exception: ") + typeid (e).name ()
		     + ": " + e.what ());
    }
  catch (const int exit_code)
    {
      // The program already reported the error, just exit.
      return exit_code;
    }
  catch (...)
    {
      toplevel.error ("Unknown exception");
    }
  return EXIT_FAILURE;
}
      
// main.C ends here.
