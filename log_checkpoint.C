// log_checkpoint.C

#include "log.h"
#include "condition.h"
#include "daisy.h"
#include "printer_file.h"
#include <deque.h>

struct LogCheckpoint : public Log, public Filter
{
  // Filter functions.
  bool check (const string&, bool) const
    { return is_active; }
  Filter& lookup (const string&) const
    { 
      // Bug: We should get rid of the filter all together.
      return const_cast<LogCheckpoint&> (*this); 
    }

  // Content.
  const string file;		// Name of file to write checkpoint in.
  const string description;	// Comment to go to the start of the file.
  const vector<string> libraries; // Extra library files to include in dump.
  Condition& condition;		// Should we print a log now?
  bool is_active;		// ... store the answer here.
  deque<const Syntax*> syntax_stack; // Syntax of the object we are logging.
  deque<AttributeList*> alist_stack; // Alist containing entire state of daisy.

  // Accessors.
  const Syntax& syntax () const
    { 
      assert (syntax_stack.size () > 0U);
      return *syntax_stack.front ();
    }
  AttributeList& alist ()
    {
      assert (alist_stack.size () > 0U);
      return *alist_stack.front ();
    }

  // Checking to see if we should log this time step.
  Filter& match (const Frame& frame, const Daisy& daisy)
    {
      is_active = condition.match (frame, daisy);
      if (is_active)
	{
	  syntax_stack.push_back (daisy.syntax);
	  alist_stack.push_back (new AttributeList (daisy.alist));
	}
      return *this;
    }

  void done ()
    {
      if (is_active)
	{
	  assert (syntax_stack.size () == 1U);
	  assert (alist_stack.size () == 1U);

	  PrinterFile printer (file);
	  printer.print_comment (description);
	  
	  // Print libraries.
	  const string lib_start = "From file `";
	  const string lib_end = "'";
	  for (unsigned int i = 0; i < libraries.size (); i++)
	    {
	      const string library = libraries[i];
	      printer.print_comment (lib_start + library + lib_end);
	      printer.print_library_file (library);
	    }

	  // Print content.
	  printer.print_comment ("Content");
	  printer.print_alist (alist (), syntax ());
	}
    }
  // Open normal items.
  void open (const string&)
    { 
      
    }
  void close ()
    { 
    }

  // Ignore unnamed items.
  void open_unnamed ()
    { }
  void close_unnamed ()
    { }

  // Open derived items two steps a time.
  void open_derived (const string& /* field */, const string& /* type */)
    { 
    }
  void close_derived ()
    { 
    }

  // Open derived items in list normally.
  void open_entry (const string& /* type */)
    {  }
  void close_entry ()
    {  }

  void output (const string& /* name */, Filter&, const Time& /* value */, bool)
    { 
    }
  void output (const string&, Filter&, const bool, bool)
    { }
  void output (const string& /* name */, Filter&, const double /* value */, bool)
    { 
    }
  void output (const string& /* name */, Filter&, const int /* value */, bool)
    { 
    }
  void output (const string&, Filter&, const string&, bool)
    { }
  void output (const string& /* name */, Filter&, const vector<double>& /* value */, bool)
    { 
    }
  void output (const string&, Filter&, const CSMP&, bool)
    { }

  // Create and Destroy.
  bool check (const Syntax&) const
    { return true; }

  LogCheckpoint (const AttributeList& al)
    : Log (),
      file (al.name ("where")),
      description (al.name ("description")),
      libraries (al.name_sequence ("libraries")),
      condition (Librarian<Condition>::create (al.alist ("when"))),
      is_active (false)
    { }

  ~LogCheckpoint ()
    {
#ifdef CONST_DELETE
      delete &condition;
#endif
    }
};

static struct LogCheckpointSyntax
{
  static Log& make (const AttributeList& al)
    { return *new LogCheckpoint (al); }

  LogCheckpointSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("where", Syntax::String, Syntax::Const);
      syntax.add ("description", Syntax::String, Syntax::Const);
      alist.add ("description", "Automatically generated Daisy checkpoint.");
      syntax.add ("libraries", Syntax::String, 
		  Syntax::Const, Syntax::Sequence);
      alist.add ("libraries", *new vector<string> ());
      syntax.add ("when", Librarian<Condition>::library (), Syntax::Const);
    }
} LogCheckpoint_syntax;

