// log_alist.C
//
// Log to an alist.

#include "log_alist.h"

bool
LogAList::check_entry (const string&, const Library&) const
{ return is_active; }

bool
LogAList::check_derived (const string&,
			 const string&, const Library&) const
{ return is_active; }

const string&
LogAList::entry () const
{ 
  assert (entry_stack.size () > 0U);
  return entry_stack.front ();
}

const Library&
LogAList::library () const
{ 
  assert (library_stack.size () > 0U);
  assert (library_stack.front ());
  return *library_stack.front ();
}

const Syntax&
LogAList::syntax () const
{ 
  assert (syntax_stack.size () > 0U);
  assert (syntax_stack.front ());
  return *syntax_stack.front ();
}

AttributeList&
LogAList::alist () const
{
  assert (alist_stack.size () > 0U);
  assert (alist_stack.front ());
  return *alist_stack.front ();
}

vector<AttributeList*>&
LogAList::alist_sequence ()
{
  assert (alist_sequence_stack.size () > 0U);
  return alist_sequence_stack.front ();
}

int
LogAList::unnamed ()
{
  assert (unnamed_stack.size () > 0U);
  return unnamed_stack.front ();
}

void
LogAList::push (const string& entry, 
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
LogAList::push (const string& entry, 
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
LogAList::push (const string& entry, 
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
LogAList::pop ()
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

void
LogAList::open_ignore ()
{ 
  if (nested > 0 || is_active)
    {
      nested++;
      is_active = false;
    }
}

void
LogAList::close_ignore ()
{ 
  if (nested > 0)
    {
      nested--;
      is_active = (nested == 0);
    }
}


void
LogAList::open (const string& name)
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
LogAList::close ()
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
LogAList::open_unnamed ()	
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
LogAList::close_unnamed ()	
{
  if (is_active)
    {
      AttributeList& old_alist = alist ();
      pop ();
      if (unnamed () < 0 || unnamed () >= alist_sequence ().size ())
	// From default alist.
	alist_sequence ().push_back (&old_alist);
      else
	// Replace specified alist.
	alist_sequence ()[unnamed ()] = &old_alist;
      // Use next entry.
      if (unnamed () >= 0)
	unnamed_stack[0]++;
      assert (syntax_stack.size () > 1);
      assert (syntax_stack[1]->lookup (entry ()) == Syntax::AList);
      assert (syntax_stack[1]->size (entry ()) != Syntax::Singleton);
    }
  else
    close_ignore ();
}

void 
LogAList::open_alist (const string& name, const AttributeList& alist)
{
  if (is_active)
    {
      assert (syntax ().lookup (name) == Syntax::AList);
      assert (syntax ().size (name) == Syntax::Singleton);
      const Syntax& syn = syntax ().syntax (name);
      push (name, syn, alist);
    }
  else
    open_ignore ();
}

void
LogAList::close_alist ()
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
LogAList::open_derived (const string& field, const string& type)
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
LogAList::close_derived ()
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
LogAList::open_entry (const string& type, const AttributeList& alist)
{
  if (is_active)
    push (type, library ().syntax (type), alist);
  else
    open_ignore (); 
}

void
LogAList::close_entry ()
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
LogAList::output (const string& name, const Time& value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogAList::output (const string& name, const bool value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogAList::output (const string& name, const double value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogAList::output (const string& name, const int value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogAList::output (const string& name, const string& value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogAList::output (const string& name, const vector<double>& value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

void
LogAList::output (const string& name, const PLF& value)
{ 
  if (!is_active)
    return;
  assert (!syntax ().is_const (name));
  if (syntax ().is_state (name))
    alist ().add (name, value);
}

bool
LogAList::check (const Syntax&) const
{ return true; }

void
LogAList::load_syntax (Syntax&, AttributeList&)
{ }

LogAList::LogAList ()
  : Log (),
    is_active (false),
    nested (0)
{ }

LogAList::~LogAList ()
{ }

