// log_checkpoint.C

#include "log_alist.h"
#include "condition.h"
#include "daisy.h"
#include "printer_file.h"

struct LogCheckpoint : public LogAList
{
  // Content.
  const string file;		// Name of file to write checkpoint in.
  const string description;	// Comment to go to the start of the file.
  const vector<string> libraries; // Extra library files to include in dump.
  Condition& condition;		// Should we print a log now?
  Time time;			// Time of current checkpoint.

  // Start and end of time step.
  bool check(const string&) const;
  bool match (const Daisy& daisy);
  void done ();

  // Create and Destroy.
  LogCheckpoint (const AttributeList& al);
  ~LogCheckpoint ();
};

bool 
LogCheckpoint::check(const string&) const
{ return true; }

bool
LogCheckpoint::match (const Daisy& daisy)
{
  assert (nested == 0);
  is_active = condition.match (daisy);
  if (is_active)
    {
      push ("daisy", *daisy.syntax, daisy.alist);
      time = daisy.time;
    }
  return is_active;
}

void
LogCheckpoint::done ()
{
  if (is_active)
    {
      // Check stacks.
      assert (syntax_stack.size () == 1U);
      assert (alist_stack.size () == 1U);
      assert (library_stack.size () == 1U);

      // Create file name.
      ostrstream scratch;
      scratch.fill (0);
      scratch.width (2);
      scratch << file.c_str () 
	      << "-" << time.year () << "-" << time.month () << "-" 
	      << time.mday () << "+" << time.hour () << ".dai" << '\0';
      const char* s = scratch.str ();
      const string filename (s);
      delete [] s;
      
      // Open log file.
      PrinterFile printer (filename);
      printer.print_comment (description);
	  
      // Print libraries.
      const string lib_start = "From file `";
      const string lib_end = "':";
      for (unsigned int i = 0; i < libraries.size (); i++)
	{
	  const string library = libraries[i];
	  printer.print_comment (lib_start + library + lib_end);
	  printer.print_library_file (library);
	}

      // Print cloned objects.
      printer.print_comment ("Cloned objects:");
      printer.print_library_file ("*clone*");

      // Print content.
      printer.print_comment ("Content");
      printer.print_alist (alist (), syntax ());
	  
      // Close stack.
      delete &alist ();
      pop ();
    }
  assert (nested == 0);
}

LogCheckpoint::LogCheckpoint (const AttributeList& al)
  : LogAList (al),
    file (al.name ("where")),
    description (al.name ("description")),
    libraries (al.name_sequence ("libraries")),
    condition (Librarian<Condition>::create (al.alist ("when"))),
    time (1, 1, 1, 1)
{ }

LogCheckpoint::~LogCheckpoint ()
{ delete &condition; }

static struct LogCheckpointSyntax
{
  static Log& make (const AttributeList& al)
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
The time will be appended, together with the `.dai' suffix.");
      alist.add ("where", "checkpoint");
      syntax.add ("description", Syntax::String, Syntax::Const,
		  "Description of this particular checkpoint.");
      alist.add ("description", "\
Create a checkpoint of the entire simulation state, suitable for later\n\
hot start.");
      syntax.add ("libraries", Syntax::String, 
		  Syntax::Const, Syntax::Sequence, "\
List of loaded files with parameterizations to dump together with the\n\
checkpoint.");
      const vector<string> empty_string_vector;
      alist.add ("libraries", empty_string_vector);
      syntax.add ("when", Librarian<Condition>::library (),
		  "Make a checkpoint every time this condition is true.");
      AttributeList finished_alist;
      finished_alist.add ("type", "finished");
      alist.add ("when", finished_alist);
      Librarian<Log>::add_type ("checkpoint", alist, syntax, &make);
    }
} LogCheckpoint_syntax;

