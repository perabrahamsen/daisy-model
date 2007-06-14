// toplevel.C -- The top level syntax for .dai files.
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

#define BUILD_DLL

#define BUILD_DLL

#include "toplevel.h"
#include "metalib.h"
#include "daisy.h"
#include "ui.h"
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
#include "treelog_text.h"
#include "treelog_store.h"
#include "librarian.h"
#include "w32reg.h"

#include <sstream>
#include <iostream>
#include <ctime>

#if defined (__unix) 
#define PATH_SEPARATOR ":"
#else
#define PATH_SEPARATOR ";"
#endif

struct Toplevel::Implementation
{
  static std::vector<Toplevel::command_line_parser>* command_line_parsers;

  std::string program_name;
  std::auto_ptr<Program> program;
  TreelogStore msg;
  Assertion::Register reg;
  Metalib metalib;
  std::time_t start_time;
  bool has_printed_copyright;
  Toplevel::state_t state;
  
  void run_program (const std::string& name);

  std::vector<std::string> files_found;
  bool ran_user_interface;
  std::auto_ptr<UI> ui;
  void add_daisy_ui (Toplevel& toplevel, bool auto_run = false);

  bool has_daisy_log;
  void add_daisy_log ();

  static std::string get_daisy_home ();

  Implementation ();
  ~Implementation ();
};

std::vector<Toplevel::command_line_parser>* 
/**/ Toplevel::Implementation::command_line_parsers = NULL;

void
Toplevel::Implementation::run_program (const std::string& name_str)
{
  try 
    {
      Treelog::Open nest (msg, "Running '" + name_str + "' from command line");
  
      // Build alist.
      const symbol name (name_str);
      const Library& library = metalib.library (Program::component);
      if (!library.check (name))
        {
          msg.error (program_name + ": '" + name + "' unknown program");
          throw EXIT_FAILURE;
        }
      const Syntax& p_syntax = library.syntax (name);
      AttributeList p_alist (library.lookup (name));
      p_alist.add ("type", name);
      if (!p_syntax.check (metalib, p_alist, msg))
        {
          msg.error ("Cannot run incomplete program");
          throw EXIT_FAILURE;
        }

      // std::auto_ptr<Program> program;

      // Build.
      {
        Block block (metalib, msg, "Building");
        program.reset (Librarian::build_alist<Program> (block, p_alist,
                                                        "Command line"));
        if (!block.ok ())
          throw EXIT_FAILURE;
      }
      // Initialize.
      {
        Block block (metalib, msg, "Initializing");
        program->initialize (block);
        if (!block.ok ())
          throw EXIT_FAILURE;
      }
      // Check.
      if (!program->check (msg))
        throw EXIT_FAILURE;

      // Run.
      program->run (msg);
    }
  catch (...)
    {
      state = is_error;
      throw;
    }
}

void 
Toplevel::Implementation::add_daisy_ui (Toplevel& toplevel,
                                        const bool auto_run)
{ 
  add_daisy_log ();

  if (ui.get ())
    return;

  const Library& library = metalib.library (UI::component);
  static const symbol run_symbol ("run");
  static const symbol read_symbol ("read");
  static const symbol progress_symbol ("progress");

  if (metalib.alist ().check ("ui"))
    /* Do nothing */;
  else if ((auto_run || files_found.size () > 0)
           && library.check (run_symbol))
    {
      AttributeList alist (library.lookup (run_symbol));
      alist.add ("type", run_symbol);
      metalib.alist ().add ("ui", alist);
    }
  else if (library.check (read_symbol))
    {
      AttributeList alist (library.lookup (read_symbol));
      alist.add ("type", read_symbol);
      metalib.alist ().add ("ui", alist);
    }
  else
    {
      AttributeList alist (library.lookup (progress_symbol));
      alist.add ("type", progress_symbol);
      metalib.alist ().add ("ui", alist);
    }
  Block block (metalib, msg, "UI");
  ui.reset (Librarian::build_item<UI> (block, "ui"));
  if (!ui.get ())
    throw "Could not create UI";
  ui->attach (toplevel);
}
  
void 
Toplevel::Implementation::add_daisy_log ()
{ 
  if (!has_daisy_log)
    {
      has_daisy_log = true;
      msg.add_client (new TreelogFile ("daisy.log"));
      msg.message ("Storing 'daisy.log' in '" + Path::get_directory () + "'");
    }
}

std::string
Toplevel::Implementation::get_daisy_home ()
{
  // Check DAISYHOME
  const char* daisy_home_env = getenv ("DAISYHOME");
  if (daisy_home_env)
    return daisy_home_env;

  // Check MS Windows registry
#if defined (_WIN32) || defined (__CYGWIN32__)
  const std::string key = "Software\\Daisy " + std::string (version);
  char *const daisy_w32_reg 
    = read_w32_registry_string (NULL, key.c_str (), "Install Directory");
  if (daisy_w32_reg)
    {
      std::string result = daisy_w32_reg;
      free (daisy_w32_reg);
      return result;
    }
  return "C:/daisy";
#else // !MS WINDOWS
  return "/usr/local/daisy";
#endif // !MS WINDOWS
}

Toplevel::Implementation::Implementation ()
  : program_name ("daisy"),
    reg (msg),
    start_time (std::time (NULL)),
    has_printed_copyright (false),
    state (is_uninitialized),
    ran_user_interface (false),
    has_daisy_log (false)
{ }
  
Toplevel::Implementation::~Implementation ()
{ add_daisy_log (); }

const char *const
Toplevel::default_description = 
"\
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
non-optional description of the new parameter.";

void 
Toplevel::add_command_line_parser (command_line_parser parser)
{
  if (!Implementation::command_line_parsers)
    Implementation::command_line_parsers 
      = new std::vector<command_line_parser> ();

  Implementation::command_line_parsers->push_back (parser);
}

void
Toplevel::remove_command_line_parser ()
{
  daisy_assert (Implementation::command_line_parsers);
  Implementation::command_line_parsers->pop_back ();
  if (Implementation::command_line_parsers->size () < 1)
    {
      delete Implementation::command_line_parsers;
      Implementation::command_line_parsers = NULL;
    }
}

Syntax& 
Toplevel::syntax () 
{ return metalib ().syntax (); }

AttributeList&
Toplevel::alist ()
{ return metalib ().alist (); }

const Syntax& 
Toplevel::program_syntax () const
{
  if (impl->metalib.alist ().check ("run"))
    {
      daisy_assert (program_alist ().check ("type"));
      const Library& library = impl->metalib.library (Program::component);
      return library.syntax (program_alist ().identifier ("type"));
    }
  return impl->metalib.syntax ();
}

const AttributeList& 
Toplevel::program_alist () const
{
  if (impl->metalib.alist ().check ("run"))
    return impl->metalib.alist ().alist ("run");

  return impl->metalib.alist ();
}
 
Program& 
Toplevel::program () const
{
  switch (impl->state)
    {
    case is_ready:
    case is_running:
    case is_done:
      break;
    case is_uninitialized:
    case is_error:
      daisy_notreached ();
    }
  return *impl->program;
}

Metalib& 
Toplevel::metalib ()
{ return impl->metalib; }

Treelog& 
Toplevel::msg ()
{ return impl->msg; }

const std::vector<std::string>&
Toplevel::files_found () const
{ return impl->files_found; }

void
Toplevel::add_treelog (Treelog* client)
{ impl->msg.add_client (client); }

void 
Toplevel::set_ui_progress ()
{ 
  impl->ui.reset (new UIProgress ()); 
  if (impl->ui.get ())
    impl->ui->attach (*this);
  else
    error ("Could not set standard UI");
}

void 
Toplevel::set_ui_none ()
{
  impl->ui.reset (new UINone ()); 
  if (impl->ui.get ())
    impl->ui->attach (*this);
  else
    error ("Could not disable UI");
}

void
Toplevel::usage ()
{
  copyright ();

  std::string s = "Usage: ";
  s += impl->program_name;
  s += " [-v] [-d dir] [-q | -nw] file... [-p ";
  const Library& library = impl->metalib.library (Program::component);
  std::vector<symbol> entries;
  library.entries (entries);
  bool first = true;
  for (size_t i = 0; i < entries.size (); i++)
    {
      if (!library.complete (metalib (), entries[i]))
        continue;

      if (first)
        first = false;
      else
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
  if (impl->has_printed_copyright)
    return;
  impl->has_printed_copyright = true;
  msg ().message (std::string ("Daisy crop/soil simulation version ")
                  + version + ". (" + version_date + ")\n"
                  "Copyright 1996 - 2007 Per Abrahamsen, "
                  "Søren Hansen and KVL.");
}

void
Toplevel::start_message () const
{
  const std::string when 
    = std::string ("Program started ") + ctime (&impl->start_time);
  std::ostringstream start_msg;
  start_msg << when.substr (0, when.size () - 1);
  const time_t time_ago = time (NULL) - impl->start_time;
  if (time_ago == 0)
    start_msg << ".";
  if (time_ago == 1)
    start_msg << ", 1 second ago.";
  else
    start_msg << ", " << time_ago << " seconds ago.";
  impl->msg.message (start_msg.str ());
}

void
Toplevel::end_message () const
{
  const std::time_t time_used = std::time (NULL) - impl->start_time;
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
  impl->msg.message (end_msg.str ());
}

void
Toplevel::run ()
{
  impl->msg.no_more_clients ();
  daisy_assert (impl->state == is_ready);
  start_message ();
  impl->program->run (msg ());
  end_message ();
  impl->state = is_done;
}

void 
Toplevel::error (const std::string& s)
{
  impl->state = is_error;
  msg ().error (s); 
}

Toplevel::state_t
Toplevel::state () const
{ return impl->state ; }

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
  const char *const daisy_path_env = getenv ("DAISYPATH");
  if (daisy_path_env)
    {
      const std::string colon_path = daisy_path_env;
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
    }
  else
    {
      const std::string daisy_home = Implementation::get_daisy_home ();
      path.push_back (".");
      path.push_back (daisy_home + "/lib");
      path.push_back (daisy_home + "/sample");
    }
  daisy_assert (path.size () > 0);
  Path::set_path (path);
}

void
Toplevel::user_interface ()
{
  daisy_assert (!impl->ran_user_interface);
  impl->ran_user_interface = true;
  impl->add_daisy_ui (*this);
  impl->ui->run (*this);
}

void
Toplevel::initialize ()
{
  try 
    {
      impl->add_daisy_ui (*this);

      daisy_assert (impl->state == is_uninitialized);

      copyright ();
      if (!program_syntax ().check (metalib (), program_alist (), msg ()))
        throw EXIT_FAILURE;
      {                             // Limit lifetime of block.
        Block block (metalib (), msg (), "Building");
        impl->program.reset (Librarian::build_alist<Program> (block,
                                                              program_alist (),
                                                              "run"));
        if (!block.ok ())
          throw EXIT_FAILURE;
      }
      {
        Block block (metalib (), msg (), "Initializing");
        impl->program->initialize (block);
        if (!block.ok ())
          throw EXIT_FAILURE;
      }
      if (!impl->program->check (msg ()))
        throw EXIT_FAILURE;
    }
  catch (...)
    {
      impl->state = is_error;
      throw;
    }
  impl->state = is_ready;
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
  daisy_assert (impl->state == is_uninitialized);

  copyright ();

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
    { msg.debug ("Command line: " + *this); }
  } command_line (argc, argv, msg ());

  // If we use drag & drop, we want to cd to the directory containing the file
#ifdef _WIN32
  if (argc == 2 && strlen (argv[1]) > 1
      && argv[1][0] != '-' && argv[1][1] == ':')
    {
      // Only one argument, a file name with an absolute path.
      const std::string arg = argv[1];
      int last = arg.rfind ("\\");
      if (last == std::string::npos)
        last == arg.find (":");
      daisy_assert (last != std::string::npos);
      const std::string dir = arg.substr (0, last);
      Path::set_directory (dir);
    }
#endif // _WIN32

  if (Implementation::command_line_parsers)
    for (size_t i = 0; i < Implementation::command_line_parsers->size (); i++)
      Implementation::command_line_parsers->at (i)(argc, argv);
  
  // We need at least one argument.
  if (argc < 2)
    usage ();                   // Usage.

  // Loop over all arguments.
  bool options_finished = false; // "--" ends command line options.

  while (argc > 1)              // argc and argv updated as args are parsed.
    {
      const std::string arg = get_arg (argc, argv);

      if (arg.size () < 1)      
        usage ();              // No zero sized args.
      else if (options_finished || arg[0] != '-')
        parse_file (arg); // Not an option, but a setup file.
      else if (arg.size () < 2)
        usage ();              // We don't allow a lone '-'.
      else if (arg.size () == 2)
	{                       // Parse options.
	  switch (arg[1])
	    {
	    case 'd':
	      if (argc > 1)
		// Change directory.
		{
		  const std::string dir = get_arg (argc, argv);
		  if (!Path::set_directory (dir))
		    error (impl->program_name
                           + ": chdir (" + dir + ") failed");
		}
	      else
		// Usage.
		usage ();
              break;
	    case 'p':
              {                 // Run a named program.
                impl->add_daisy_ui (*this, true);

                if (argc < 2)
                  // We need a program name.
                  usage ();

                impl->run_program (get_arg (argc, argv));
                impl->state = is_done;
              }
              break;
            case 'q':
              set_ui_none ();
              break;
            case 'v':
	      // Print version.
	      copyright ();
              impl->state = is_done;
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
      else if (arg == "-nw")
        set_ui_progress ();
      else
        usage ();
    }

  if (state () == is_done)
    // Already done.
    throw EXIT_SUCCESS;

  if (impl->files_found.size () < 1)
    // Nothing to do.
    usage ();

  // Done.
  daisy_assert (argc == 1);     // Only the program name remains.
}

void
Toplevel::parse_file (const std::string& filename)
{ 
  copyright ();
  impl->files_found.push_back (filename);
  parse_system_file (filename);
}

void
Toplevel::parse_system_file (const std::string& filename)
{ 
  Treelog::Open nest (msg (), "Parsing '"+ filename + "'");
  if (impl->state != is_uninitialized)
    {
      error ("Program already initialized");
      throw EXIT_FAILURE;
    }
  ParserFile parser (metalib (), filename, msg ());
  if (!parser.check ())
    {
      impl->state = is_error;
      throw EXIT_FAILURE;
    }
  
  parser.load (impl->metalib.alist ());
  if (parser.error_count () > 0)
    {
      impl->state = is_error;
      throw EXIT_FAILURE;
    }
}

void
Toplevel::load_run (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Toplevel");
  alist.add ("description", Toplevel::default_description);

  syntax.add ("install_directory", Syntax::String, Syntax::Const,
              "Directory where Daisy has been installed.\n\
\n\
This is used for looking up files that came with the installation, in\n\
particular the parameter library.  By default, the value of the\n\
DAISYHOME environment variable is used.  If DAISYHOME is not set, and\n\
the program is running under MS Windows, the value of the \"Install\n\
Directory\" registry key is used.  If that is not set either (or we\n\
are not running MS Windows), a hardcoded value is used.  This is\n\
\"C:/daisy\" under MS Windows, or \"/usr/local/daisy\" on other systems.\n\
\n\
The value found in the manual corresponds to the system where the\n\
manual was generated.");
  alist.add ("install_directory", Implementation::get_daisy_home ());
  syntax.add ("directory", Syntax::String, Syntax::OptionalConst,
              "Run program in this directory.\n\
This can affect both where input files are found and where log files\n\
are generated.");
  syntax.add ("path", Syntax::String,
              Syntax::OptionalConst, Syntax::Sequence,
              "List of directories to search for input files in.\n\
The special value \".\" means the current directory.\n\
\n\
By default, this variable will be initialised from the DAISYPATH\n\
environment variable if it exists.  The value of the variable should\n\
be a list of directories to search for input files in, seperated by\n\
semicolon on MS Windows, or colon on other systems.  If the DAISYPATH\n\
environment variable is not set, the path will be initialized to the\n\
working directory followed by the standard parameter libraries.");

  std::vector<symbol> default_path;
  default_path.push_back (symbol ("."));
  default_path.push_back (symbol ("${install_directory}/lib"));
  default_path.push_back (symbol ("${install_directory}/sample"));
  alist.add ("path", default_path);

  syntax.add_object ("input", Parser::component,
                     Syntax::OptionalConst, Syntax::Singleton,
                     "Command to add more information about the simulation.");
  syntax.add_object ("run", Program::component, 
                     Syntax::OptionalState, Syntax::Singleton, 
                     "Program to run.\n\
\n\
If this option is specified, all the 'Daisy' specific top-level attributes\n\
will be ignored.  If unspecified, run 'Daisy' on the current top-level\n\
attributes.");
  syntax.add_object ("ui", UI::component, 
                     Syntax::OptionalState, Syntax::Singleton, 
                     "Top level user interface.");
  
}

void
Toplevel::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Top level Daisy syntax.
  Daisy::load_syntax (syntax, alist);
  alist.add ("type", "Daisy");
  Librarian::load_syntax (syntax, alist);
  load_run (syntax, alist);
}

Toplevel::Toplevel ()
  : impl (new Implementation)
{ 
  load_syntax (impl->metalib.syntax (), impl->metalib.alist ()); 
  initialize_once ();
}

Toplevel::~Toplevel ()
{ 
  if (!impl->ran_user_interface)
    user_interface ();
}

static Submodel::Register 
toplevel_submodel ("Toplevel", Toplevel::load_run);

// toplevel.C ends here.
