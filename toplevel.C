// toplevel.h -- The top level syntax for .dai files.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#include "toplevel.h"
#include "daisy.h"
#include "library.h"
#include "parser_file.h"
#include "submodel.h"
#include "block.h"
#include "program.h"
#include "syntax.h"
#include "alist.h"
#include "path.h"
#include "version.h"
#include "assertion.h"
#include "treelog_dual.h"
#include <sstream>
#include <iostream>

#if defined (__unix) 
#define PATH_SEPARATOR ":"
#else
#define PATH_SEPARATOR ";"
#endif

Syntax& 
Toplevel::syntax () 
{ return top_syntax; }

AttributeList&
Toplevel::alist ()
{ return top_alist; }

const Syntax& 
Toplevel::program_syntax () const
{
  if (top_alist.check ("run"))
    {
      daisy_assert (program_alist ().check ("type"));
      const Library& library = Librarian<Program>::library ();
      return library.syntax (program_alist ().identifier ("type"));
    }
  return top_syntax;
}

const AttributeList& 
Toplevel::program_alist () const
{
  if (top_alist.check ("run"))
    return top_alist.alist ("run");

  return top_alist;
}
 
Program& 
Toplevel::program () const
{
  switch (state_)
    {
    case is_ready:
    case is_running:
    case is_done:
      break;
    case is_uninitialized:
    case is_error:
      daisy_notreached ();
    }
  return *program_;
}

void
Toplevel::usage ()
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
  error (s);
  throw 2;
}

void
Toplevel::copyright ()
{
  if (has_printed_copyright)
    return;
  has_printed_copyright = true;
  msg->lazy (std::string ("Daisy crop/soil simulation version ")
             + version + ". (" + version_date + ")\n"
             "Copyright 1996 - 2007 Per Abrahamsen, "
             "Søren Hansen and KVL.");
}

void
Toplevel::start_message () const
{
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
  msg->message (start_msg.str ());
}

void
Toplevel::end_message () const
{
  const std::time_t time_used = std::time (NULL) - start_time;
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
  msg->message (end_msg.str ());
}

void
Toplevel::run ()
{
  daisy_assert (state_ == is_ready);
  start_message ();
  program_->run (*msg);
  end_message ();
  state_ = is_done;
}

void 
Toplevel::error (const std::string& s)
{
  state_ = is_error;
  msg->error (s); 
}

Toplevel::state_t
Toplevel::state () const
{ return state_ ; }

void 
Toplevel::initialize_once ()
{
  // Only run once.
  static bool first_time = true;
  if (!first_time)
    return;
  first_time = false;

  // We don't use stdio.
  std::ios::sync_with_stdio (false);

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

void
Toplevel::initialize ()
{
  daisy_assert (state_ == is_uninitialized);

  copyright ();
  if (!program_syntax ().check (program_alist (), *msg))
    throw EXIT_FAILURE;
  {                             // Limit lifetime of block.
    Block block (top_syntax, top_alist, *msg, "Building");
    program_.reset (Librarian<Program>::build_alist (block,
                                                     program_alist (), "run"));
    if (!block.ok ())
      throw EXIT_FAILURE;
  }
  program_->initialize (&top_syntax, &top_alist, *msg);
  if (!program_->check (*msg))
    throw EXIT_FAILURE;

  state_ = is_ready;
}

std::string
Toplevel::get_arg (int& argc, char**& argv)
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
Toplevel::command_line (int& argc, char**& argv)
{ 
  daisy_assert (state_ == is_uninitialized);

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
  } command_line (argc, argv, *msg);
  
  // We need at least one argument.
  if (argc < 2)
    usage ();                   // Usage.

  // Loop over all arguments.
  bool file_found = false;      // We only run the program if we find a file. 
  bool options_finished = false; // "--" ends command line options.
  bool prevent_run = false;     // We might already have run the program.
  int errors_found = 0;

  while (argc > 1)              // argc and argv updated as args are parsed.
    {
      const std::string arg = get_arg (argc, argv);

      if (arg.size () < 1)      
        usage ();              // No zero sized args.
      else if (options_finished || arg[0] != '-')
	{                       // Not an option, but a setup file.
          copyright ();
	  // Parse the file.
	  Treelog::Open nest (*msg, "Parsing file");
	  ParserFile parser (top_syntax, arg, *msg);
	  parser.load (top_alist);
	  file_found = true;
	  errors_found += parser.error_count ();
	}
      else if (arg.size () < 1)
        usage ();              // We don't allow a lone '-'.
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
		    error (program_name + ": chdir (" + dir + ") failed");
		}
	      else
		// Usage.
		usage ();
              break;
	    case 'p':
              {                 // Run a named program.
                if (argc < 2)
                  // We need a program name.
                  usage ();
                const Library& library 
                  = Librarian<Program>::library ();
                const symbol name = symbol (get_arg (argc, argv));
                if (!library.check (name))
                  {
                    error (program_name + ": '" + name 
                               + "' unknown program");
                    usage ();
                  }
                const Syntax& p_syntax = library.syntax (name);
                AttributeList p_alist (library.lookup (name));
                p_alist.add ("type", name);
                Treelog::Open nest (*msg, name);
                if (p_syntax.check (p_alist, *msg))
                  {
		    std::auto_ptr<Block> block (new Block (top_syntax, 
                                                           top_alist, *msg, 
                                                           "Building"));
                    std::auto_ptr<Program> program
                      (Librarian<Program>::build_alist (*block, p_alist, 
                                                        "Command line"));
		    const bool block_ok = block->ok ();
                    block.reset ();
		    if (block_ok)
                      {
                        program->initialize (&top_syntax, &top_alist, *msg);
                        if (!program->check (*msg))
                          throw EXIT_FAILURE;
                       
                        program->run (*msg);
                      }
                  }
                prevent_run = true;
              }
              break;
            case 'v':
	      // Print version.
	      copyright ();
	      break;
	    case '-':
	      // Finish option list.
	      options_finished = true;
	      break;
	    default:
	      // Usage.
	      usage ();
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

void
Toplevel::parse_file (const std::string& filename)
{ 
  daisy_assert (state_ == is_uninitialized);
  copyright ();
  Treelog::Open nest (*msg, "Parsing file");
  ParserFile parser (top_syntax, filename, *msg);
  parser.load (top_alist);
  if (parser.error_count () > 0)
    throw EXIT_FAILURE;
}

void
Toplevel::load_run (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Toplevel");
  alist.add ("description", "\
\n\
The top level syntax for a Daisy setup file.\n\
\n\
The main parameter is 'run', which determines the program to run.  It\n\
is also the only real parameter, the rest are more appropriately\n\
called 'commands', as the have an immediate effect.  These are mostly\n\
concerned with reading additionally files ('input'), and where to find\n\
those files ('directory' and 'path').\n\
\n\
Also found at top level, but not listed here, are the named\n\
parameterizations.  The format of those are:\n\
\n\
   (defCOMP NEW OLD \"DOC\" PARS...)\n\
\n\
Here COMP is the the component for which you want to define a new\n\
parameterization, NEW is the name of the new paramterization, OLD the\n\
name of an already defined parameterization or model you want to base\n\
the new parameterization on, and PARS... is a list of parameters and\n\
values you want to set for this paramaterization. \"DOC\" is an\n\
optional decsription of the paramterization.\n\
\n\
You can declare new parameters both at the top level, and inside the\n\
PARS... list in the paramterization definition described above.  Once\n\
you have declared a new parameter, you can set it just like the\n\
build-in parameters.  The syntax for declaring a new parameter is\n\
\n\
  (declare PAR [SIZE] TYPE \"DOC\")\n\
\n\
Here PAR is the name of the new parameter we want to declare, [SIZE]\n\
is either missing, in which case the new parameter is a singleton, [],\n\
in which case the new parameter is a sequence with an arbitrary\n\
length, of SIZE is an integer, in which case the sequence must hold\n\
exactly that number of values.  TYPE is either a name of a component,\n\
in which case the parameter must hold a paramterization of a model\n\
within that component, or String, Integer, or Number, in which case\n\
the value must be a primitive of that type.  For Number, you can also\n\
specify a dimension in square brackets afterwards.  DOC is a\n\
non-optional description of the new parameter.");

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
}

void
Toplevel::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Top level Daisy syntax.
  Daisy::load_syntax (syntax, alist);
  alist.add ("type", "Daisy");
  Library::load_syntax (syntax, alist);
  load_run (syntax, alist);
}

Toplevel::Toplevel (const std::string& logname)
  : program_name ("daisy"),
#if defined (__unix) || defined (__CYGWIN__)
    msg (new TreelogDual (logname, std::cerr)),
#else // MSDOS
  // stderr can't be redirected under MSDOS
    msg (new TreelogDual (logname, std::cout)),
#endif // MSDOS
    start_time (std::time (NULL)),
    has_printed_copyright (false),
    state_ (is_uninitialized)
{ 
  Assertion::Register reg (*msg);
  load_syntax (top_syntax, top_alist); 
  initialize_once ();
}

static Submodel::Register 
toplevel_submodel ("Toplevel", Toplevel::load_run);

// toplevel.C ends here.
