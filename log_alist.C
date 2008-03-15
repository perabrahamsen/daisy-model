// log_alist.C
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
//
// Log to an alist.

#define BUILD_DLL

#include "log_alist.h"
#include "library.h"
#include "syntax.h"
#include "assertion.h"
#include <sstream>

bool
LogAList::check_entry (symbol, const char*) const
{ return is_active; }

bool
LogAList::check_derived (symbol, symbol, const char*) const
{ return is_active; }

symbol
LogAList::entry () const
{ 
  daisy_assert (entry_stack.size () > 0U);
  return entry_stack.front ();
}

const Library&
LogAList::library () const
{ 
  daisy_assert (library_stack.size () > 0U);
  daisy_assert (library_stack.front ());
  return *library_stack.front ();
}

const Syntax&
LogAList::syntax () const
{ 
  daisy_assert (syntax_stack.size () > 0U);
  daisy_assert (syntax_stack.front ());
  return *syntax_stack.front ();
}

AttributeList&
LogAList::alist () const
{
  daisy_assert (alist_stack.size () > 0U);
  daisy_assert (alist_stack.front ());
  return *alist_stack.front ();
}

std::vector<const AttributeList*>&
LogAList::alist_sequence ()
{
  daisy_assert (alist_sequence_stack.size () > 0U);
  return alist_sequence_stack.front ();
}

int
LogAList::unnamed ()
{
  daisy_assert (unnamed_stack.size () > 0U);
  return unnamed_stack.front ();
}

void
LogAList::push (symbol entry, const Library& library, const AttributeList& alist)
{
  entry_stack.push_front (entry);
  library_stack.push_front (&library);
  syntax_stack.push_front (NULL);
  alist_stack.push_front (new AttributeList (alist));
  alist_sequence_stack.push_front (std::vector<const AttributeList*> ());
  unnamed_stack.push_front (-1);
}

void
LogAList::push (symbol entry, const Syntax& syntax, const AttributeList& alist)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  syntax_stack.push_front (&syntax);
  alist_stack.push_front (new AttributeList (alist));
  alist_sequence_stack.push_front (std::vector<const AttributeList*> ());
  unnamed_stack.push_front (-1);
}

void
LogAList::push (symbol entry, 
		const Syntax& syntax, 
		const AttributeList& default_alist,
		std::vector<const AttributeList*> alist_sequence)
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
  daisy_assert (entry_stack.size () > 0);
  daisy_assert (library_stack.size () > 0);
  daisy_assert (syntax_stack.size () > 0);
  daisy_assert (alist_stack.size () > 0);
  daisy_assert (alist_sequence_stack.size () > 0);
  daisy_assert (unnamed_stack.size () > 0);
  daisy_assert (unnamed () < 0 || unnamed () == alist_sequence ().size ());

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
LogAList::open (const symbol name)
{
  if (is_active)
    {
      const std::string& sname = name.name ();
      daisy_assert (!syntax ().is_const (sname));
      if (syntax ().is_state (sname))
	{
	  const int size = syntax ().size (sname);
	  const bool has_value = alist ().check (sname);
	  switch (syntax ().lookup (sname))
	    {
	    case Syntax::AList:
	      if (size != Syntax::Singleton && has_value)
		push (name, 
		      syntax ().syntax (sname), 
		      syntax ().default_alist (sname),
		      alist ().alist_sequence (sname));
	      else if (size != Syntax::Singleton || !has_value)
		push (name, 
		      syntax ().syntax (sname), 
		      syntax ().default_alist (sname));
	      else 
		push (name, 
		      syntax ().syntax (sname), 
		      alist ().alist (sname));
		      
	      break;
	    case Syntax::Object:
	      daisy_assert (size != Syntax::Singleton);
	      push (name, 
		    syntax ().library (metalib (), sname), 
		    syntax ().default_alist (sname));
	      break;
	    default:
	      daisy_notreached ();
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
      std::vector<const AttributeList*> old_alist_sequence = alist_sequence ();
      const symbol old_entry = entry ();

      // Pop stack.
      pop ();

      // Assign new value to entry.
      if (entry_stack.size () > 0)
	{
	  const std::string& sold_entry = old_entry.name ();
	  daisy_assert (syntax_stack.front ());
	  const Syntax::type type = syntax ().lookup (sold_entry);
	  switch (type)
	    { 
	    case Syntax::Object:
	      // Object sequence.
	      daisy_assert (syntax ().size (sold_entry) != Syntax::Singleton);
	      alist ().add (sold_entry, old_alist_sequence);
	      break;
	    case Syntax::AList:
	      // AList sequence or singleton.
	      if (syntax ().size (sold_entry) == Syntax::Singleton)
		{
		  daisy_assert (old_alist_sequence.size () == 0);
		  alist ().add (sold_entry, old_alist);
		}
	      else
		alist ().add (sold_entry, old_alist_sequence);
	      delete &old_alist;
	      break;
	    default:
	      daisy_notreached ();
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
      daisy_assert (syntax_stack.size () > 1);
      const std::string& sentry = entry ().name ();
      daisy_assert (syntax_stack[1]->lookup (sentry) == Syntax::AList);
      daisy_assert (syntax_stack[1]->size (sentry) != Syntax::Singleton);
    }
  else
    close_ignore ();
}

void 
LogAList::open_alist (symbol name, const AttributeList& alist)
{
  if (is_active)
    {
      const std::string& sname = name.name ();
      daisy_assert (syntax ().lookup (sname) == Syntax::AList);
      daisy_assert (syntax ().size (sname) == Syntax::Singleton);
      const Syntax& syn = syntax ().syntax (sname);
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
      const std::string& old_entry = entry ().name ();
      pop ();
      alist ().add (old_entry, old_alist);
      delete &old_alist;
    }
  else
    close_ignore (); 
}
void
LogAList::open_derived (symbol field, symbol type, const char *const library)
{ 
  const std::string& sfield = field.name ();
  if (!alist ().check (sfield))
    {
      std::ostringstream tmp;
      tmp << "No field '" << field << "' (type " << type
          << ") within library '" << library << "'";
      daisy_panic (tmp.str ());
    }
  open_object (field, type, alist ().alist (sfield), library);
}
	
void
LogAList::close_derived ()
{ close_object (); }

void
LogAList::open_object (symbol field, symbol type, 
                       const AttributeList& alist, const char* lib)
{ 
  if (is_active)
    {
      const std::string& sfield = field.name ();
      daisy_assert (syntax ().lookup (sfield) == Syntax::Object);
      daisy_assert (syntax ().size (sfield) == Syntax::Singleton);
      const Library& library = syntax ().library (metalib (), sfield);
      daisy_assert (library.name () == symbol (lib));
      if (!library.check (type))
        daisy_panic ("Field '" + sfield + "' containing component '"
                     + library.name () + "' has unknown model '" + type + "'");
      const Syntax& syntax = library.syntax (type);
      push (field, syntax, alist);
    }
  else
    open_ignore ();
}
	
void
LogAList::close_object ()
{ 
  if (is_active)
    {
      AttributeList& old_alist = alist ();
      const std::string& old_entry = entry ().name ();
      pop ();
      alist ().add (old_entry, old_alist);
      delete &old_alist;
    }
  else
    close_ignore (); 
}

void
LogAList::open_entry (symbol type, const AttributeList& alist, const char*)
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
LogAList::open_named_entry (const symbol, const symbol type,
			    const AttributeList& alist)
{ open_entry (type, alist, NULL); }

void
LogAList::close_named_entry ()
{ close_entry (); }

void
LogAList::output_entry (symbol name, const bool value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!syntax ().is_const (sname));
  if (syntax ().is_state (sname))
    alist ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const double value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!syntax ().is_const (sname));
  if (syntax ().is_state (sname))
    alist ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const int value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!syntax ().is_const (sname));
  if (syntax ().is_state (sname))
    alist ().add (sname, value);
}

void
LogAList::output_entry (symbol name, symbol value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!syntax ().is_const (sname));
  if (syntax ().is_state (sname))
    alist ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const std::vector<double>& value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!syntax ().is_const (sname));
  if (syntax ().is_state (sname))
    alist ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const PLF& value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!syntax ().is_const (sname));
  if (syntax ().is_state (sname))
    alist ().add (sname, value);
}

bool
LogAList::check (const Border&, Treelog&) const
{ return true; }

void
LogAList::load_syntax (Syntax&, AttributeList&)
{ }

LogAList::LogAList (Block& al)
  : Log (al),
    is_active (false),
    nested (0)
{ }

LogAList::~LogAList ()
{ }

