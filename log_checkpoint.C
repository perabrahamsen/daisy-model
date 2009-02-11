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

#define BUILD_DLL

#include "log_alist.h"
#include "metalib.h"
#include "block.h"
#include "condition.h"
#include "daisy.h"
#include "printer_file.h"
#include "scope.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "library.h"
#include "frame.h"
#include "frame_model.h"
#include "path.h"
#include <sstream>

struct LogCheckpoint : public LogAList
{
  // Content.
  const symbol file;		// Name of file to write checkpoint in.
  const symbol description;	// Comment to go to the start of the file.
  std::auto_ptr<Condition> condition; // Should we print a log now?
  Time time;			// Time of current checkpoint.
  const Metalib* global_frame; // All attributes.

  // Start and end of time step.
  bool check_leaf (symbol) const;
  bool check_interior (symbol) const;
  bool match (const Daisy& daisy, Treelog& out);
  void done (const std::vector<Time::component_t>& time_columns,
	     const Time& time, double dt);

  bool initial_match (const Daisy&, Treelog&)
  { return false; }
  void initial_done (const std::vector<Time::component_t>& time_columns,
		     const Time&, double)
  { daisy_notreached (); }

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
LogCheckpoint::match (const Daisy& daisy, Treelog& msg)
{
  daisy_assert (nested == 0);
  condition->tick (daisy, Scope::null (), msg);
  is_active = condition->match (daisy, Scope::null (), msg);
  if (is_active)
    {
      TREELOG_MODEL (msg);
      msg.message ("Making checkpoint of " + daisy.frame.type_name ());
      static const symbol daisy_symbol ("daisy");
      global_frame = &daisy.metalib;
      push (daisy_symbol, daisy.frame);
      time = daisy.time;
    }
  return is_active;
}

class FrameNamed : public FrameModel
{
  const symbol name;
  
  symbol type_name () const
  { return name; }

  FrameNamed (const FrameNamed& frame, parent_clone_t)
    : FrameModel (frame, parent_clone),
      name (frame.name)
  { }
  FrameNamed& clone () const
  { return *new FrameNamed (*this, parent_clone); }

public:
  FrameNamed (const symbol n, const FrameModel& parent)
    : FrameModel (parent, parent_link),
      name (n)
  { }
  ~FrameNamed ()
  { }
};

class FrameDummy : public Frame
{
  const Frame& parent_;
  const Frame* parent () const
  { return &parent_; }
  FrameDummy& clone () const
  { return *new FrameDummy (parent_, parent_link); }

public:
  FrameDummy (const Frame& frame, parent_link_t)
    : Frame (),
      parent_ (frame)
  { }
};

void
LogCheckpoint::done (const std::vector<Time::component_t>& time_columns,
		     const Time&, double)
{
  if (is_active)
    {
      // Check stacks.
      daisy_assert (frame_stack.size () == 1U);
      daisy_assert (library_stack.size () == 1U);

      // Create file name.
      std::ostringstream scratch;
      scratch.fill (0);
      scratch.width (2);
      scratch << file << "-" << time.year () << "-" << time.month () << "-" 
	      << time.mday () << "+" << time.hour () << ".dai";
      const symbol filename (scratch.str ());

      // Open log file.
      PrinterFile printer (metalib (), filename);
      if (description != Value::None ())
        printer.print_comment (description);

      // Print "directory" and "path" before inputs.
      printer.print_entry (*global_frame, "directory");
      if (global_frame->name_sequence ("path") != Path::get_daisy_path ())
        printer.print_entry (*global_frame, "path");
      FrameModel& daisy = dynamic_cast<FrameModel&> (frame ());

      // Print input files.
      const std::vector<const Frame*>& inputs = global_frame->parser_inputs ();
      printer.print_comment ("Input files.");
      for (unsigned int i = 0; i < inputs.size (); i++)
        printer.print_input (*inputs[i]);

      // Print included files.
      const std::vector<symbol>& files = global_frame->parser_files ();
      const std::string lib_start = "From file '";
      const std::string lib_end = "':";
      for (unsigned int i = 0; i < files.size (); i++)
        {
          const symbol library = files[i];
          printer.print_comment (lib_start + library + lib_end);
          printer.print_library_file (library.name ());
        }

      // Print cloned objects.
      printer.print_comment ("Cloned objects:");
      printer.print_library_file ("*clone*");

      // Start checkpoint from next timestep.
      daisy_assert (daisy.check ("time"));
      Time time (daisy.submodel ("time"));
      time.tick_hour ();
      time.set_time (daisy, "time");

      // Print content.
      printer.print_comment ("Content");
      
      Library& library = metalib ().library (Program::component);
      const symbol super = daisy.type_name ();

      std::string name = super + " Checkpoint";
      while (library.check (name))
        name += "+";
      daisy_assert (library.check (super));
      std::auto_ptr<FrameNamed> program (new FrameNamed (name, daisy));
      library.add_model (name, *program);
      printer.print_parameterization (Program::component, name);
      FrameDummy run_frame (*global_frame, Frame::parent_link);
      run_frame.add ("run", *program);
      printer.print_entry (run_frame, "run");
      program.release ();
      library.remove (name);

      if (!printer.good ())
	{
	  std::ostringstream tmp;
	  tmp << "Error writing to '" << filename << "'";
	  Assertion::error (tmp.str ());
	}
      // Close stack.
      delete &frame ();
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
    description (al.frame ().description ()),
    condition (Librarian::build_item<Condition> (al, "when")),
    time (1, 1, 1, 1)
{ }

LogCheckpoint::~LogCheckpoint ()
{ }

static struct LogCheckpointSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new LogCheckpoint (al); }

  LogCheckpointSyntax ()
    : DeclareModel (Log::component, "checkpoint", "\
Create a checkpoint of the entire simulation state, suitable for later\n\
hot start.")
  { }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame);
    frame.add ("where", Value::String, Value::Const,
                "File name prefix for the generated checkpoint.\n\
The time will be appended, together with the '.dai' suffix.");
    frame.add ("where", "checkpoint");
    frame.add_object ("when", Condition::component,
                       "Make a checkpoint every time this condition is true.");
    frame.add ("when", "finished");
  }
} LogCheckpoint_syntax;

// log_checkpoint.C ends here.
