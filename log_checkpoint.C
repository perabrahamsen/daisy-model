// log_checkpoint.C

#include "log.h"
#include "condition.h"
#include "daisy.h"
#include "printer_file.h"
#include <deque.h>

struct LogCheckpoint : public Log, public Filter
{
  // Filter functions.
  bool check (const string&, bool) const;
  bool check_derived (const string&, const Library&) const;
  Filter& lookup (const string&) const;
  Filter& lookup_derived (const string&, const Library&) const;

  // Content.
  const string file;		// Name of file to write checkpoint in.
  const string description;	// Comment to go to the start of the file.
  const vector<string> libraries; // Extra library files to include in dump.
  Condition& condition;		// Should we print a log now?
  bool is_active;		// ... store the answer here.
  unsigned int nested;		// Nesting iff active.

  // Stacks.
  deque<string> entry_stack;	// Name of the entity we are logging.
  deque<const Library*> library_stack; // Library of the object we are logging.
  deque<const Syntax*> syntax_stack; // Syntax of the alist we are logging.
  deque<AttributeList*> alist_stack; // State and parameters of entity.
  deque<vector<AttributeList*>/**/> alist_sequence_stack; // Ditto for lists.
  deque<int> unnamed_stack;	// Current element of AList sequence.

  // Stack Accessors.
  const string& entry () const;
  const Library& library () const;
  const Syntax& syntax () const;
  AttributeList& alist () const;
  vector<AttributeList*>& alist_sequence ();
  int unnamed ();

  // Stack Constructors.
  void push (const string& entry, 
	     const Library& library, const AttributeList& alist);
  void push (const string& entry, 
	     const Syntax& syntax, const AttributeList& alist);
  void push (const string& entry, 
	     const Syntax& syntax, 
	     const AttributeList& default_alist,
	     vector<AttributeList*> alist_sequence);
  void pop ();

  // Start and end of time step.
  Filter& match (const Daisy& daisy);
  void done ();

  // Nesting.
  void open_ignore ();		// Ignored items.
  void close_ignore ();
  void open (const string& name); // AList singletons. 
  void close ();
  void open_unnamed ();		// Items in a AList sequence.
  void close_unnamed ();	
  void open_derived (const string& field, // Object singletons.
		     const string& type); 
  void close_derived ();
  void open_entry (const string& type,   // Items in an Object sequence.
		   const AttributeList& alist);
  void close_entry ();

  // Logging.
  void output (const string& name, Filter&, const Time& value, bool);
  void output (const string& name, Filter&, const bool value, bool);
  void output (const string& name, Filter&, const double value, bool);
  void output (const string& name, Filter&, const int value, bool);
  void output (const string& name, Filter&, const string& value, bool);
  void output (const string& name, Filter&, const vector<double>& value, bool);
  void output (const string& name, Filter&, const CSMP& value, bool);

  // Create and Destroy.
  bool check (const Syntax&) const;
  LogCheckpoint (const AttributeList& al);
  ~LogCheckpoint ();
};

bool
LogCheckpoint::check (const string&, bool) const
{ return is_active; }

bool
LogCheckpoint::check_derived (const string&, const Library&) const
{ return is_active; }

Filter&
LogCheckpoint::lookup (const string&) const
{ 
  // Bug: We should get rid of the filter all together.
  return const_cast<LogCheckpoint&> (*this); 
}

Filter&
LogCheckpoint::lookup_derived (const string&, const Library&) const
{ 
  // Bug: We should get rid of the filter all together.
  return const_cast<LogCheckpoint&> (*this); 
}

const string&
LogCheckpoint::entry () const
{ 
  assert (entry_stack.size () > 0U);
  return entry_stack.front ();
}

const Library&
LogCheckpoint::library () const
{ 
  assert (library_stack.size () > 0U);
  assert (library_stack.front ());
  return *library_stack.front ();
}

const Syntax&
LogCheckpoint::syntax () const
{ 
  assert (syntax_stack.size () > 0U);
  assert (syntax_stack.front ());
  return *syntax_stack.front ();
}

AttributeList&
LogCheckpoint::alist () const
{
  assert (alist_stack.size () > 0U);
  assert (alist_stack.front ());
  return *alist_stack.front ();
}

vector<AttributeList*>&
LogCheckpoint::alist_sequence ()
{
  assert (alist_sequence_stack.size () > 0U);
  return alist_sequence_stack.front ();
}

int
LogCheckpoint::unnamed ()
{
  assert (unnamed_stack.size () > 0U);
  return unnamed_stack.front ();
}

void
LogCheckpoint::push (const string& entry, 
	   const Library& library, const AttributeList& alist)
{
  entry_stack.push_front (entry);
  library_stack.push_front (&library);
  syntax_stack.push_front (NULL);
  alist_stack.push_front (new AttributeList (alist));
  alist_sequence_stack.push_front (vector<AttributeList*> ());
  unnamed_stack.push_front (-1);
}

void
LogCheckpoint::push (const string& entry, 
	   const Syntax& syntax, const AttributeList& alist)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  syntax_stack.push_front (&syntax);
  alist_stack.push_front (new AttributeList (alist));
  alist_sequence_stack.push_front (vector<AttributeList*> ());
  unnamed_stack.push_front (-1);
}

void
LogCheckpoint::push (const string& entry, 
	   const Syntax& syntax, 
	   const AttributeList& default_alist,
	   vector<AttributeList*> alist_sequence)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  syntax_stack.push_front (&syntax);
  alist_stack.push_front (new AttributeList (default_alist));
  alist_sequence_stack.push_front (alist_sequence);
  unnamed_stack.push_front (0);
}

void
LogCheckpoint::pop ()
{
  // Check.
  assert (entry_stack.size () > 0);
  assert (library_stack.size () > 0);
  assert (syntax_stack.size () > 0);
  assert (alist_stack.size () > 0);
  assert (alist_sequence_stack.size () > 0);
  assert (unnamed_stack.size () > 0);
  assert (unnamed () < 0 || unnamed () == alist_sequence ().size ());

      // Clear old values.
  entry_stack.pop_front ();
  library_stack.pop_front ();
  syntax_stack.pop_front ();
  alist_stack.pop_front ();      
  alist_sequence_stack.pop_front ();      
  unnamed_stack.pop_front ();
}

Filter&
LogCheckpoint::match (const Daisy& daisy)
{
  assert (nested == 0);
  is_active = condition.match (daisy);
  if (is_active)
    push ("daisy", *daisy.syntax, daisy.alist);
  return *this;
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

      // Open log file.
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
	  
      // Close stack.
      delete &alist ();
      pop ();
    }
  assert (nested == 0);
}

void
LogCheckpoint::open_ignore ()
{ 
  if (nested > 0 || is_active)
    {
      nested++;
      is_active = false;
    }
}

void
LogCheckpoint::close_ignore ()
{ 
  if (nested > 0)
    {
      nested--;
      is_active = (nested == 0);
    }
}


void
LogCheckpoint::open (const string& name)
{
  if (is_active)
    {
      assert (!syntax ().is_const (name));
      if (syntax ().is_state (name))
	{
	  const int size = syntax ().size (name);
	  const bool has_value = alist ().check (name);
	  switch (syntax ().lookup (name))
	    {
	    case Syntax::AList:
	      if (size != Syntax::Singleton && has_value)
		push (name, 
		      syntax ().syntax (name), 
		      syntax ().default_alist (name),
		      alist ().alist_sequence (name));
	      else if (size != Syntax::Singleton)
		push (name, 
		      syntax ().syntax (name), 
		      syntax ().default_alist (name));
	      else if (has_value)
		push (name, 
		      syntax ().syntax (name), 
		      alist ().alist (name));
	      else
		push (name, 
		      syntax ().syntax (name), 
		      AttributeList ());
		      
	      break;
	    case Syntax::Object:
	      assert (size != Syntax::Singleton);
	      push (name, 
		    syntax ().library (name), 
		    syntax ().default_alist (name));
	      break;
	    default:
	      assert (false);
	    }
	  // We know how to handle this, continue.
	  return;
	}
    }
  // We couldn't use it, ignore children.
  open_ignore ();
}

void
LogCheckpoint::close ()
{ 
  if (is_active)
    {
      // Remember old values.
      AttributeList& old_alist = alist ();
      vector<AttributeList*> old_alist_sequence = alist_sequence ();
      const string old_entry = entry ();

      // Pop stack.
      pop ();

      // Assign new value to entry.
      if (entry_stack.size () > 0)
	{
	  assert (syntax_stack.front ());
	  const Syntax::type type = syntax ().lookup (old_entry);
	  switch (type)
	    { 
	    case Syntax::Object:
	      // Object sequence.
	      assert (syntax ().size (old_entry) != Syntax::Singleton);
	      alist ().add (old_entry, old_alist_sequence);
	      break;
	    case Syntax::AList:
	      // AList sequence or singleton.
	      if (syntax ().size (old_entry) == Syntax::Singleton)
		{
		  assert (old_alist_sequence.size () == 0);
		  alist ().add (old_entry, old_alist);
		}
	      else
		alist ().add (old_entry, old_alist_sequence);
	      delete &old_alist;
	      break;
	    default:
	      assert (false);
	    }
	}
    }
  else
    close_ignore (); 
}

void
LogCheckpoint::open_unnamed ()	
{ 
  if (is_active)
    {
      if (unnamed () < 0 || unnamed () >= alist_sequence ().size ())
	// Use default alist.
	push (entry (), syntax (), alist ());
      else
	{
	  // Use specified alist.
	  push (entry (), syntax (), *alist_sequence ()[unnamed ()]);
	}
    }
  else
    open_ignore (); 
}

void
LogCheckpoint::close_unnamed ()	
{
  if (is_active)
    {
      AttributeList& old_alist = alist ();
      pop ();
      if (unnamed () < 0 || unnamed () >= alist_sequence ().size ())
	// From default alist.
	alist_sequence ().push_back (&old_alist);
      else
	{
	  // Replace specified alist.
	  alist_sequence ()[unnamed ()] = &old_alist;
	  unnamed_stack[0]++;
	}
      assert (syntax_stack.size () > 1);
      assert (syntax_stack[1]->lookup (entry ()) == Syntax::AList);
      assert (syntax_stack[1]->size (entry ()) != Syntax::Singleton);
    }
  else
    close_ignore ();
}

void
LogCheckpoint::open_derived (const string& field, const string& type)
{ 
  if (is_active)
    {
      assert (syntax ().lookup (field) == Syntax::Object);
      assert (syntax ().size (field) == Syntax::Singleton);
      const Library& library = syntax ().library (field);
      assert (library.check (type));
      const Syntax& syntax = library.syntax (type);
      assert (alist ().check (field));
      push (field, syntax, alist ().alist (field));
    }
  else
    open_ignore ();
}
	
void
LogCheckpoint::close_derived ()
{ 
  if (is_active)
    {
      AttributeList& old_alist = alist ();
      const string old_entry = entry ();
      pop ();
      alist ().add (old_entry, old_alist);
      delete &old_alist;
    }
  else
    close_ignore (); 
}

void
LogCheckpoint::open_entry (const string& type, const AttributeList& alist)
{
  if (is_active)
    push (type, library ().syntax (type), alist);
  else
    open_ignore (); 
}

void
LogCheckpoint::close_entry ()
{
  if (is_active)
    {
      AttributeList& old_alist = alist ();
      pop ();
      alist_sequence ().push_back (&old_alist);
    }
  else
    close_ignore ();
}

void
LogCheckpoint::output (const string& name, Filter&, const Time& value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogCheckpoint::output (const string& name, Filter&, const bool value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogCheckpoint::output (const string& name, Filter&, const double value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogCheckpoint::output (const string& name, Filter&, const int value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogCheckpoint::output (const string& name, Filter&, const string& value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogCheckpoint::output (const string& name, Filter&, const vector<double>& value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogCheckpoint::output (const string& name, Filter&, const CSMP& value, bool)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

bool
LogCheckpoint::check (const Syntax&) const
{ return true; }

LogCheckpoint::LogCheckpoint (const AttributeList& al)
  : Log (),
    file (al.name ("where")),
    description (al.name ("description")),
    libraries (al.name_sequence ("libraries")),
    condition (Librarian<Condition>::create (al.alist ("when"))),
    is_active (false),
    nested (0)
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
      syntax.add ("where", Syntax::String, Syntax::Const);
      alist.add ("where", "checkpoint.dai");
      syntax.add ("description", Syntax::String, Syntax::Const);
      alist.add ("description", "Automatically generated Daisy checkpoint.");
      syntax.add ("libraries", Syntax::String, 
		  Syntax::Const, Syntax::Sequence);
      alist.add ("libraries", *new vector<string> ());
      Library& condition_lib = Librarian<Condition>::library ();
      syntax.add ("when", condition_lib, Syntax::Const);
      AttributeList finished_alist (condition_lib.lookup ("finished"));
      finished_alist.add ("type", "finished");
      alist.add ("when", finished_alist);
      Librarian<Log>::add_type ("checkpoint", alist, syntax, &make);
    }
} LogCheckpoint_syntax;
