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
#include "parser.h"
#include "block.h"
#include "alist.h"
#include "library.h"
#include "block.h"
#include "treelog_dual.h"
#include "options.h"
#include "assertion.h"
#include <sstream>
#include <stdexcept>
#include <typeinfo>
#include <iostream>
#include <time.h>

int
main (int argc, char* argv[])
{
  // We don't use stdio.
  std::ios::sync_with_stdio(false);

  time_t start_time = time (NULL);
#if defined (__unix) || defined (__CYGWIN__)
  TreelogDual treelog ("daisy.log", std::cerr);
#else // MSDOS
  // stderr can't be redirected under MSDOS
  TreelogDual treelog ("daisy.log", std::cout);
#endif // MSDOS

  Assertion::Register reg (treelog);

  try
    {
      // Initialize syntax and attribute list.
      Syntax syntax;
      AttributeList alist;
      Daisy::load_syntax (syntax, alist);
      alist.add ("type", "Daisy");
      Library::load_syntax (syntax, alist);
      
      syntax.add ("directory", Syntax::String, Syntax::OptionalConst,
                  "Run program in this directory.\n\
This can affect both where input files are found and where log files\n\
are generated.");
      syntax.add ("path", Syntax::String,
                  Syntax::OptionalConst, Syntax::Sequence,
                  "List of directories to search for input files in.\n\
The special value \".\" means the current directory.");
      syntax.add ("input", Librarian<Parser>::library (),
                  Syntax::OptionalConst, Syntax::Singleton,
                  "Command to add more information about the simulation.");
      syntax.add ("run", Librarian<Program>::library (), 
                  Syntax::OptionalState, Syntax::Singleton, 
                  "Program to run.\n\
\n\
If this option is specified, all the 'Daisy' specific top-level attributes\n\
will be ignored.  If unspecified, run 'Daisy' on the current top-level\n\
attributes.");
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
	  daisy_notreached ();
	}

      // Explicit or implicit program?
      const Library& library = Librarian<Program>::library ();
      const Syntax* run_syntax;
      const AttributeList* run_alist;

      if (alist.check ("run"))
        {
          run_alist = &alist.alist ("run");
          daisy_assert (run_alist->check ("type"));
          run_syntax = &library.syntax (run_alist->identifier ("type"));
        }
      else
        {
          run_alist = &alist;
          run_syntax = &syntax;
        }

      // Create, check and run the program.
      options.copyright (treelog);
      if (!run_syntax->check (*run_alist, treelog))
        return 1;

      std::auto_ptr<Block> block (new Block (syntax, alist, 
                                             treelog, "Building"));
      std::auto_ptr<Program> program (Librarian<Program>::build_alist (*block, 
								  *run_alist,
 								  "run"));
      if (!block->ok ())
	return 1;
      block.reset ();
    
      program->initialize (&syntax, &alist, treelog);
      if (!program->check (treelog))
	return 1;

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
      treelog.message (start_msg.str ());

      program->run (treelog);

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
      treelog.message (end_msg.str ());
      
      // All is well.
      return 0;

    }
  catch (const char* error)
    {
      treelog.error (std::string ("Exception: ") + error);
    }
  catch (const std::string& error)
    {
      treelog.error (std::string ("Exception raised: ") + error);
    }
  catch (const std::exception& e)
    {
      treelog.error (std::string ("Standard exception: ") + typeid (e).name ()
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
      
