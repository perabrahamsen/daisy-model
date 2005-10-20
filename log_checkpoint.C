// log_checkpoint.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "log_alist.h"
#include "condition.h"
#include "daisy.h"
#include "printer_file.h"
#include <sstream>

using namespace std;

struct LogCheckpoint : public LogAList
{
  // Content.
  const string file;		// Name of file to write checkpoint in.
  const string description;	// Comment to go to the start of the file.
  auto_ptr<Condition> condition; // Should we print a log now?
  Time time;			// Time of current checkpoint.
  const AttributeList* global_alist; // All attributes.

  // Start and end of time step.
  bool check_leaf (symbol) const;
  bool check_interior (symbol) const;
  bool match (const Daisy& daisy, Treelog& out);
  void done (const Time& time);

  // Create and Destroy.
  void initialize (Treelog&);
  LogCheckpoint (Block& al);
  ~LogCheckpoint ();
};

bool 
LogCheckpoint::check_leaf (symbol) const
{ return true; }

bool 
LogCheckpoint::check_interior (symbol) const
{ return true; }

bool
LogCheckpoint::match (const Daisy& daisy, Treelog& out)
{
  daisy_assert (nested == 0);
  condition->tick (daisy, out);
  is_active = condition->match (daisy);
  if (is_active)
    {
      static const symbol daisy_symbol ("daisy");
      push (daisy_symbol, *daisy.global_syntax, daisy.alist);
      global_alist = daisy.global_alist;
      time = daisy.time;
    }
  return is_active;
}

void
LogCheckpoint::done (const Time&)
{
  if (is_active)
    {
      // Check stacks.
      daisy_assert (syntax_stack.size () == 1U);
      daisy_assert (alist_stack.size () == 1U);
      daisy_assert (library_stack.size () == 1U);

      // Create file name.
      std::ostringstream scratch;
      scratch.fill (0);
      scratch.width (2);
      scratch << file.c_str () 
	      << "-" << time.year () << "-" << time.month () << "-" 
	      << time.mday () << "+" << time.hour () << ".dai";
      const string filename (scratch.str ());

      // Open log file.
      PrinterFile printer (filename);
      printer.print_comment (description);

      // Print "directory" and "path" before inputs.
      printer.print_entry (*global_alist, syntax (), "directory");
      printer.print_entry (*global_alist, syntax (), "path");
      alist ().remove ("directory"); // Avoid printing them twice.
      alist ().remove ("path"); 
      
      // Print input files.
      if (global_alist->check ("parser_inputs"))
	{
	  const vector<AttributeList*> inputs 
	    (global_alist->alist_sequence ("parser_inputs"));
	  printer.print_comment ("Input files.");
	  for (unsigned int i = 0; i < inputs.size (); i++)
	    printer.print_input (*inputs[i]);
	}

      // Print included files.
      if (global_alist->check ("parser_files"))
	{
	  const vector<symbol> 
            files (global_alist->identifier_sequence ("parser_files"));
	  const string lib_start = "From file '";
	  const string lib_end = "':";
	  for (unsigned int i = 0; i < files.size (); i++)
	    {
	      const symbol library = files[i];
	      printer.print_comment (lib_start + library + lib_end);
	      printer.print_library_file (library.name ());
	    }
	}

      // Print cloned objects.
      printer.print_comment ("Cloned objects:");
      printer.print_library_file ("*clone*");

      // Start checkpoint from next timestep.
      daisy_assert (alist ().check ("time"));
      Time time (alist ().alist ("time"));
      time.tick_hour ();
      AttributeList new_time;
      new_time.add ("year", time.year ());
      new_time.add ("month", time.month ());
      new_time.add ("mday", time.mday ());
      new_time.add ("hour", time.hour ());
      alist ().add ("time", new_time);

      // Print content.
      printer.print_comment ("Content");
      
      Syntax daisy_syntax;
      AttributeList default_alist;
      Daisy::load_syntax (daisy_syntax, default_alist);

      printer.print_alist (alist (), daisy_syntax, default_alist);

      if (!printer.good ())
	{
	  std::ostringstream tmp;
	  tmp << "Error writing to '" << filename << "'";
	  Assertion::error (tmp.str ());
	}
      // Close stack.
      delete &alist ();
      pop ();
    }
  daisy_assert (nested == 0);
}

void 
LogCheckpoint::initialize (Treelog&)
{ }

LogCheckpoint::LogCheckpoint (Block& al)
  : LogAList (al),
    file (al.name ("where")),
    description (al.name ("description")),
    condition (Librarian<Condition>::create (al.alist ("when"))),
    time (1, 1, 1, 1)
{ }

LogCheckpoint::~LogCheckpoint ()
{ }

static struct LogCheckpointSyntax
{
  static Log& make (Block& al)
  { return *new LogCheckpoint (al); }

  LogCheckpointSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Create a checkpoint of the entire simulation state, suitable for later\n\
hot start.");
    LogAList::load_syntax (syntax, alist);
    syntax.add ("where", Syntax::String, Syntax::Const,
                "File name prefix for the generated checkpoint.\n\
The time will be appended, together with the '.dai' suffix.");
    alist.add ("where", "checkpoint");
    syntax.add ("description", Syntax::String, Syntax::Const,
                "Description of this particular checkpoint.");
    alist.add ("description", "\
Create a checkpoint of the entire simulation state, suitable for later\n\
hot start.");
    syntax.add ("when", Librarian<Condition>::library (),
                "Make a checkpoint every time this condition is true.");
    AttributeList finished_alist;
    finished_alist.add ("type", "finished");
    alist.add ("when", finished_alist);
    Librarian<Log>::add_type ("checkpoint", alist, syntax, &make);
  }
} LogCheckpoint_syntax;

