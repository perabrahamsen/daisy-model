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
#include "frame.h"
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

Frame&
LogAList::frame () const
{
  daisy_assert (frame_stack.size () > 0U);
  daisy_assert (frame_stack.front ());
  return *frame_stack.front ();
}

std::vector<const Frame*>&
LogAList::frame_sequence ()
{
  daisy_assert (frame_sequence_stack.size () > 0U);
  return frame_sequence_stack.front ();
}

int
LogAList::unnamed ()
{
  daisy_assert (unnamed_stack.size () > 0U);
  return unnamed_stack.front ();
}

void
LogAList::push (symbol entry, const Library& library, const Frame& frame)
{
  entry_stack.push_front (entry);
  library_stack.push_front (&library);
  frame_stack.push_front (&frame.clone ());
  frame_sequence_stack.push_front (std::vector<const Frame*> ());
  unnamed_stack.push_front (-1);
}

void
LogAList::push (symbol entry, const Frame& frame)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  frame_stack.push_front (&frame.clone ());
  frame_sequence_stack.push_front (std::vector<const Frame*> ());
  unnamed_stack.push_front (-1);
}

void
LogAList::push (symbol entry, 
		const Frame& default_frame,
		std::vector<const Frame*> frame_sequence)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  frame_stack.push_front (&default_frame.clone ());
  frame_sequence_stack.push_front (frame_sequence);
  unnamed_stack.push_front (0);
}

void
LogAList::pop ()
{
  // Check.
  daisy_assert (entry_stack.size () > 0);
  daisy_assert (library_stack.size () > 0);
  daisy_assert (frame_stack.size () > 0);
  daisy_assert (frame_sequence_stack.size () > 0);
  daisy_assert (unnamed_stack.size () > 0);
  daisy_assert (unnamed () < 0 || unnamed () == frame_sequence ().size ());

      // Clear old values.
  entry_stack.pop_front ();
  library_stack.pop_front ();
  frame_stack.pop_front ();      
  frame_sequence_stack.pop_front ();      
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
      if (frame ().is_const (sname))
        {
          std::stringstream tmp;
          tmp << "'" << name << "' is const;";
          for (size_t i = 0; i < entry_stack.size (); i++)
            tmp << " " << entry_stack[i];
          daisy_panic (tmp.str ());
        }
      if (frame ().is_state (sname))
	{
	  const int size = frame ().size (sname);
	  const bool has_value = frame ().check (sname);
	  switch (frame ().lookup (sname))
	    {
	    case Value::AList:
	      if (size != Value::Singleton && has_value)
		push (name, 
		      frame ().default_frame (sname),
		      frame ().frame_sequence (sname));
	      else if (size != Value::Singleton || !has_value)
		push (name, 
		      frame ().default_frame (sname));
	      else 
		push (name, 
		      frame ().frame (sname));
		      
	      break;
	    case Value::Object:
              {
                const Library& library = frame ().library (metalib (), sname);
                daisy_assert (size != Value::Singleton);
                push (name, library, library.frame ("component"));
              }
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
      Frame& old_frame = frame ();
      std::vector<const Frame*> old_frame_sequence = frame_sequence ();
      const symbol old_entry = entry ();

      // Pop stack.
      pop ();

      // Assign new value to entry.
      if (entry_stack.size () > 0)
	{
	  const std::string& sold_entry = old_entry.name ();
	  const Value::type type = frame ().lookup (sold_entry);
	  switch (type)
	    { 
	    case Value::Object:
	      // Object sequence.
	      daisy_assert (frame ().size (sold_entry) != Value::Singleton);
	      frame ().add (sold_entry, old_frame_sequence);
	      break;
	    case Value::AList:
	      // AList sequence or singleton.
	      if (frame ().size (sold_entry) == Value::Singleton)
		{
		  daisy_assert (old_frame_sequence.size () == 0);
		  frame ().add (sold_entry, old_frame);
		}
	      else
		frame ().add (sold_entry, old_frame_sequence);
	      delete &old_frame;
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
      if (unnamed () < 0 || unnamed () >= frame_sequence ().size ())
	// Use default frame.
	push (entry (), frame ());
      else
	{
	  // Use specified frame.
	  push (entry (), *frame_sequence ()[unnamed ()]);
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
      Frame& old_frame = frame ();
      pop ();
      if (unnamed () < 0 || unnamed () >= frame_sequence ().size ())
	// From default frame.
	frame_sequence ().push_back (&old_frame);
      else
	// Replace specified frame.
	frame_sequence ()[unnamed ()] = &old_frame;
      // Use next entry.
      if (unnamed () >= 0)
	unnamed_stack[0]++;
      const std::string& sentry = entry ().name ();
      daisy_assert (frame_stack[1]->lookup (sentry) == Value::AList);
      daisy_assert (frame_stack[1]->size (sentry) != Value::Singleton);
    }
  else
    close_ignore ();
}

void 
LogAList::open_alist (symbol name, const Frame& f)
{
  if (is_active)
    {
      const std::string& sname = name.name ();
      daisy_assert (frame ().lookup (sname) == Value::AList);
      daisy_assert (frame ().size (sname) == Value::Singleton);
      push (name, f);
    }
  else
    open_ignore ();
}

void
LogAList::close_alist ()
{ 
  if (is_active)
    {
      Frame& old_frame = frame ();
      const std::string& old_entry = entry ().name ();
      pop ();
      frame ().add (old_entry, old_frame);
      delete &old_frame;
    }
  else
    close_ignore (); 
}
void
LogAList::open_derived (const symbol field, const symbol type, 
                        const char *const library)
{ 
  if (!frame ().check (field))
    {
      std::ostringstream tmp;
      tmp << "No field '" << field << "' (type " << type
          << ") within library '" << library << "'";
      daisy_panic (tmp.str ());
    }
  open_object (field, type, frame ().frame (field), library);
}
	
void
LogAList::close_derived ()
{ close_object (); }

void
LogAList::open_object (symbol field, symbol type, 
                       const Frame& f, const char* lib)
{ 
  if (is_active)
    {
      const std::string& sfield = field.name ();
      daisy_assert (frame ().lookup (sfield) == Value::Object);
      daisy_assert (frame ().size (sfield) == Value::Singleton);
      const Library& library = frame ().library (metalib (), sfield);
      daisy_assert (library.name () == symbol (lib));
      if (!library.check (type))
        daisy_panic ("Field '" + sfield + "' containing component '"
                     + library.name () + "' has unknown model '" + type + "'");
      push (field, f);
    }
  else
    open_ignore ();
}
	
void
LogAList::close_object ()
{ 
  if (is_active)
    {
      Frame& old_frame = frame ();
      const std::string& old_entry = entry ().name ();
      pop ();
      frame ().add (old_entry, old_frame);
      delete &old_frame;
    }
  else
    close_ignore (); 
}

void
LogAList::open_entry (symbol type, const Frame& frame, const char*)
{
  if (is_active)
    push (type, frame);
  else
    open_ignore (); 
}

void
LogAList::close_entry ()
{
  if (is_active)
    {
      Frame& old_frame = frame ();
      pop ();
      frame_sequence ().push_back (&old_frame);
    }
  else
    close_ignore ();
}

void
LogAList::open_named_entry (const symbol, const symbol type,
			    const Frame& frame)
{ open_entry (type, frame, NULL); }

void
LogAList::close_named_entry ()
{ close_entry (); }

void
LogAList::output_entry (symbol name, const bool value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!frame ().is_const (sname));
  if (frame ().is_state (sname))
    frame ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const double value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!frame ().is_const (sname));
  if (frame ().is_state (sname))
    frame ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const int value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!frame ().is_const (sname));
  if (frame ().is_state (sname))
    frame ().add (sname, value);
}

void
LogAList::output_entry (symbol name, symbol value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!frame ().is_const (sname));
  if (frame ().is_state (sname))
    frame ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const std::vector<double>& value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!frame ().is_const (sname));
  if (frame ().is_state (sname))
    frame ().add (sname, value);
}

void
LogAList::output_entry (symbol name, const PLF& value)
{ 
  if (!is_active)
    return;
  const std::string& sname = name.name ();
  daisy_assert (!frame ().is_const (sname));
  if (frame ().is_state (sname))
    frame ().add (sname, value);
}

bool
LogAList::check (const Border&, Treelog&) const
{ return true; }

LogAList::LogAList (Block& al)
  : Log (al),
    is_active (false),
    nested (0)
{ }

LogAList::~LogAList ()
{ }

// log_alist.C ends here.
