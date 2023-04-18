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

#include "toplevel.h"
#include "metalib.h"
#include "daisy.h"
#include "ui.h"
#include "library.h"
#include "parser_file.h"
#include "block_top.h"
#include "program.h"
#include "path.h"
#include "version.h"
#include "assertion.h"
#include "treelog_text.h"
#include "treelog_store.h"
#include "librarian.h"
#include "units.h"
#include "frame_model.h"

#include <sstream>
#include <ctime>
#include <cstring>
#include <locale.h>

#include <boost/noncopyable.hpp>

struct Toplevel::Implementation : boost::noncopyable
{
  const symbol preferred_ui;
  const std::string program_name;
  std::unique_ptr<Program> program;
  std::unique_ptr<FrameModel> program_frame_owner;

  TreelogServer msg;
  
  Assertion::Register reg;
  Metalib metalib;
  std::time_t start_time;
  bool has_printed_copyright;
  Toplevel::state_t state;
  
  void run_program (const std::string& name);

  std::vector<std::string> files_found;
  bool ran_user_interface;
  std::unique_ptr<UI> ui;
  void add_daisy_ui (Toplevel& toplevel);

  bool has_daisy_log;
  void add_daisy_log ();
  void no_daisy_log ();

  void reset ();

  Implementation (Frame::load_frame_t load_syntax, 
                  const std::string& preferred_ui);
  ~Implementation ();
};

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
      if (!library.model (name).check (metalib, msg))
        {
          msg.error (program_name + ": '" + name + "' not runable");
          throw EXIT_FAILURE;
        }
      program.reset (Librarian::build_stock<Program> (metalib, msg, name,
                                                      "Command line"));
      if (!program.get ())
        throw EXIT_FAILURE;

      // Initialize.
      {
        BlockTop block (metalib, msg, metalib);
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
Toplevel::Implementation::add_daisy_ui (Toplevel& toplevel)
{ 
  add_daisy_log ();

  if (ui.get ())
    return;

  if (!metalib.check ("ui"))
    metalib.set ("ui", preferred_ui);

  BlockTop block (metalib, msg, metalib);
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
      boost::shared_ptr<Treelog> daisy_log (new TreelogFile ("daisy.log"));
      msg.add_client (daisy_log);
      msg.message ("Storing 'daisy.log' in '" 
		   + metalib.path ().get_output_directory () + "'");
    }
}

void 
Toplevel::Implementation::no_daisy_log ()
{ 
  if (has_daisy_log)
    msg.message ("Failed to supress 'daisy.log'");
  else
    has_daisy_log = true;
}

void
Toplevel::Implementation::reset ()
{
  program.reset (NULL);
  start_time = std::time (NULL);
  metalib.reset ();
  state = is_unloaded;
  files_found.clear ();
}

Toplevel::Implementation::Implementation (Metalib::load_frame_t load_syntax,
                                          const std::string& pref_ui)
  : preferred_ui (pref_ui),
    program_name ("daisy"),
    // program (NULL),
    reg (msg),
    metalib (load_syntax),
    start_time (std::time (NULL)),
    has_printed_copyright (false),
    state (is_unloaded),
    ran_user_interface (false),
    has_daisy_log (false)
{ 
  (void) setlocale (LC_ALL, "C");
}
  
Toplevel::Implementation::~Implementation ()
{ add_daisy_log (); }

const char *const
Toplevel::default_description = "The top level syntax for a Daisy setup file.";

const Frame& 
Toplevel::program_frame () const
{
  if (impl->metalib.check ("run"))
    return impl->metalib.model ("run");
  return impl->metalib;
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
    case is_unloaded:
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
Toplevel::add_treelog (boost::shared_ptr<Treelog> client)
{ 
  impl->msg.add_client (client); 
}

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
  s += " [-v] [-d dir] [-L] [-q | -nw] file... [-p ";
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
                  "Copyright 1996 - 2016 Per Abrahamsen, "
		  // We are not going to win any awards for localization. 
#if defined (_WIN32) || defined (__CYGWIN32__)
		  "S\xF8ren Hansen "
#else
                  u8"S\u00F8ren Hansen "
#endif
		  "and KU."
		  );
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
Toplevel::user_interface ()
{
  daisy_assert (!impl->ran_user_interface);
  impl->ran_user_interface = true;
  impl->add_daisy_ui (*this);
  impl->ui->run (*this);
}

void
Toplevel::failure_interface ()
{
  daisy_assert (!impl->ran_user_interface);
  impl->ran_user_interface = true;
  impl->add_daisy_ui (*this);
  impl->ui->failure (*this);
}

void
Toplevel::initialize ()
{
  try 
    {
      impl->add_daisy_ui (*this);

      daisy_assert (impl->state == is_uninitialized);

      copyright ();
      if (!program_frame ().check (metalib (), msg ()))
        throw EXIT_FAILURE;
      {                             // Limit lifetime of block.
        BlockTop block (metalib (), msg (), metalib ());
        if (metalib ().check ("run"))
          impl->program.reset (Librarian::build_item<Program> (block, "run"));
        else
          {
            const Library& library = metalib ().library (Program::component);
            const FrameModel& old_frame = library.model ("Daisy");
            std::unique_ptr<FrameModel> frame 
              (new FrameModel (old_frame, Frame::parent_link));
            // Frame::overwrite only gives us the values, not the
            // types.  This means we avoid all the extra crap in
            // metalib, but also that we don't get any user defined
            // types.  Solution:  Use "run".
            frame->overwrite_values (metalib ());
            impl->program.reset (Librarian::build_frame<Program> (block, *frame,
                                                                  "toplevel"));
            impl->program_frame_owner = std::move (frame);
          }
        if (!block.ok ())
          throw EXIT_FAILURE;
      }
      {
        BlockTop block (metalib (), msg (), metalib ());
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

void
Toplevel::reset ()
{ impl->reset (); }

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
  daisy_assert (impl->state == is_unloaded);

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
        last = arg.find (":");
      daisy_assert (last != std::string::npos);
      const std::string dir = arg.substr (0, last);
      impl->metalib.path ().set_directory (dir);
    }
#endif // _WIN32

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
	    case 'D':
	      {
		const std::string dir = get_arg (argc, argv);
		impl->metalib.path ().set_input_directory (dir);
	      }
	      break;
	    case 'd':
	      if (argc > 1)
		// Change directory.
		{
		  const std::string dir = get_arg (argc, argv);
		  if (!impl->metalib.path ().set_directory (dir))
		    error (impl->program_name
                           + ": chdir (" + dir + ") failed");
		}
	      else
		// Usage.
		usage ();
              break;
	    case 'p':
              {                 // Run a named program.
                impl->add_daisy_ui (*this);

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
            case 'L':
              impl->no_daisy_log ();
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

  // Done.
  daisy_assert (argc == 1);     // Only the program name remains.
}

void
Toplevel::parse_file (const std::string& filename)
{ 
  copyright ();
  impl->files_found.push_back (filename);
  parse_system_file (filename);
  impl->state = is_uninitialized;
}

void
Toplevel::parse_system_file (const std::string& filename)
{ 
  if (impl->state > is_uninitialized)
    {
      error ("Program already initialized");
      throw EXIT_FAILURE;
    }
  ParserFile parser (metalib (), filename, msg ());
  parser.initialize (metalib ());
  if (!parser.check ())
    {
      impl->state = is_error;
      throw EXIT_FAILURE;
    }
  
  parser.load_top ();
  if (parser.error_count () > 0)
    {
      impl->state = is_error;
      throw EXIT_FAILURE;
    }
}

void
Toplevel::load_submodel (Frame& frame)
{
  load_frame (frame);
}

void
Toplevel::load_frame (Frame& frame)
{
  Units::load_syntax (frame);

  frame.declare_string ("install_directory", Attribute::Const,
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
  frame.set ("install_directory", Path::get_daisy_home ());
  frame.declare_string ("directory", Attribute::OptionalConst,
              "Run program in this directory.\n\
This can affect both where input files are found and where log files\n\
are generated.");
  frame.declare_string ("path", Attribute::Const, Attribute::Variable,
              "List of directories to search for input files in.\n\
The special value \".\" means the current directory.\n\
\n\
By default, this variable will be initialised from the DAISYPATH\n\
environment variable if it exists.  The value of the variable should\n\
be a list of directories to search for input files in, seperated by\n\
semicolon on MS Windows, or colon on other systems.  If the DAISYPATH\n\
environment variable is not set, the path will be initialized to the\n\
working directory followed by the standard parameter libraries.");
  // Give it a value so people can extend it with &old.
  frame.set ("path", Path::get_daisy_path ());

  frame.declare_object ("input", Parser::component,
                     Attribute::OptionalConst, Attribute::Singleton,
                     "Command to add more information about the simulation.");
  frame.declare_object ("run", Program::component, 
                     Attribute::OptionalState, Attribute::Singleton, 
                     "Program to run.\n\
\n\
If this option is specified, all the 'Daisy' specific top-level attributes\n\
will be ignored.  If unspecified, run 'Daisy' on the current top-level\n\
attributes.");
  frame.declare_object ("ui", UI::component, 
                     Attribute::OptionalState, Attribute::Singleton, 
                     "Top level user interface.");
}

void
Toplevel::load_syntax (Frame& frame)
{
  // Top level Daisy syntax.
  Daisy::load_syntax (frame);
  load_frame (frame);
}

Toplevel::Toplevel (const std::string& preferred_ui)
  : impl (new Implementation (load_syntax, preferred_ui))
{ 
  // We don't use stdio.
  std::ios::sync_with_stdio (false);
}

Toplevel::~Toplevel ()
{ 
  try 
    {
      if (!impl->ran_user_interface)
	failure_interface ();
    }
  catch (...)
    { error ("Exception occured during cleanup"); }
}

static DeclareSubmodel 
toplevel_submodel (Toplevel::load_submodel, "Toplevel",
                   Toplevel::default_description);

// toplevel.C ends here.
