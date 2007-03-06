// options.C --- Parsing command line options.
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "options.h"
#include "daisy.h"
#include "library.h"
#include "block.h"
#include "parser_file.h"
#include "program.h"
#include "syntax.h"
#include "alist.h"
#include "treelog_stream.h"
#include "version.h"
#include "path.h"
#include <memory>

#if defined (__unix) 
#define PATH_SEPARATOR ":"
#else
#define PATH_SEPARATOR ";"
#endif

std::string
Options::get_arg (int& argc, char**& argv)
{
  daisy_assert (argc > 1);
  const std::string arg = argv[1];

  // Update argc and argv.
  for (int i = 2; i < argc; i++)
    argv[i - 1] = argv[i];
  argv[argc - 1] = NULL;
  argc--;

  return arg;
}

void
Options::usage (Treelog& out) const
{
  std::string s = "Usage: ";
  s += program_name;
  s += " [-v] [-d dir] file... [-p ";
  const Library& library = Librarian<Program>::library ();
  std::vector<symbol> entries;
  library.entries (entries);
  for (size_t i = 0; i < entries.size (); i++)
    {
      if (i > 0)
        s += " | ";
      s += entries[i].name ();
    }
  s += "]";
  out.error (s);
  throw 2;
}

void
Options::copyright (Treelog& out)
{
  if (has_printed_copyright)
    return;
  has_printed_copyright = true;
  out.lazy (std::string ("Daisy crop/soil simulation version ")
	    + version + ". (" + version_date + ")\n"
	    "Copyright 1996 - 2007 Per Abrahamsen, "
	    "Søren Hansen and KVL.");
}

void 
Options::initialize_path ()
{
  std::vector<std::string> path;

  // Initialize path.
  const std::string colon_path
    = getenv ("DAISYPATH") ? getenv ("DAISYPATH") : ".";
  int last = 0;
  for (;;)
    {
      const int next = colon_path.find (PATH_SEPARATOR, last);
      if (next < 0)
	break;
      path.push_back (colon_path.substr (last, next - last));
      last = next + 1;
    }
  path.push_back (colon_path.substr (last));
  daisy_assert (path.size () > 0);
  Path::set_path (path);
}

Options::Options (int& argc, char**& argv,
		  Syntax& syntax, AttributeList& alist, Treelog& msg)
  : has_printed_copyright (false),
    program_name (argv[0])
{ 
  // Create original command line string first, we modify argc and argv later.
  // However we only want to print it after we have parsed "(directory ...)".
  const struct CommandLine : public std::string
  {
    Treelog& msg;
    CommandLine (const int argc, char**const argv, Treelog& m)
      : msg (m)
    {
      for (size_t i = 0; i < argc; i++)
        {
          if (i > 0)
            *this += " ";
          *this += argv[i];
        }
    }
    ~CommandLine ()
    { msg.debug (*this); }
  } command_line (argc, argv, msg);
  
  // We need at least one argument.
  if (argc < 2)
    usage (msg);                  // Usage.

  // Parse DAISYPATH.
  initialize_path ();

  // Loop over all arguments.
  bool file_found = false;      // We only run the program if we find a file. 
  bool options_finished = false; // "--" ends command line options.
  bool prevent_run = false;     // We might already have run the program.
  int errors_found = 0;

  while (argc > 1)              // argc and argv updated as args are parsed.
    {
      const std::string arg = get_arg (argc, argv);

      if (arg.size () < 1)      
        usage (msg);              // No zero sized args.
      else if (options_finished || arg[0] != '-')
	{                       // Not an option, but a setup file.
          copyright (msg);
	  // Parse the file.
	  Treelog::Open nest (msg, "Parsing file");
	  ParserFile parser (syntax, arg, msg);
	  parser.load (alist);
	  file_found = true;
	  errors_found += parser.error_count ();
	}
      else if (arg.size () < 1)
        usage (msg);              // We don't allow a lone '-'.
      else
	{                       // Parse options.
	  switch (arg[1])
	    {
	    case 'd':
	      if (argc > 1)
		// Change directory.
		{
		  const std::string dir = get_arg (argc, argv);
		  if (!Path::set_directory (dir))
		    msg.error (program_name + ": chdir (" + dir + ") failed");
		}
	      else
		// Usage.
		usage (msg);
              break;
	    case 'p':
              {                 // Run a named program.
                if (argc < 2)
                  // We need a program name.
                  usage (msg);
                const Library& library 
                  = Librarian<Program>::library ();
                const symbol name = symbol (get_arg (argc, argv));
                if (!library.check (name))
                  {
                    msg.error (program_name + ": '" + name 
                               + "' unknown program");
                    usage (msg);
                  }
                const Syntax& p_syntax = library.syntax (name);
                AttributeList p_alist (library.lookup (name));
                p_alist.add ("type", name);
                Treelog::Open nest (msg, name);
                if (p_syntax.check (p_alist, msg))
                  {
		    std::auto_ptr<Block> block (new Block (syntax, alist, msg, 
                                                           "Building"));
                    std::auto_ptr<Program> program
                      (Librarian<Program>::build_alist (*block, p_alist, 
                                                        "Command line"));
		    const bool block_ok = block->ok ();
                    block.reset ();
		    if (block_ok)
		      program->run (msg);
                  }
                prevent_run = true;
              }
              break;
            case 'v':
	      // Print version.
	      copyright (msg);
	      break;
	    case '-':
	      // Finish option list.
	      options_finished = true;
	      break;
	    default:
	      // Usage.
	      usage (msg);
	    }
	}
    }
  if (errors_found > 0)
    throw EXIT_FAILURE;

  if (!file_found || prevent_run)
    // Done.
    throw EXIT_SUCCESS;

  // Done.
  daisy_assert (argc == 1);     // Only the program name remains.
}

// options.C ends here.
