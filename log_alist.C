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
#include "frame_submodel.h"
#include "frame_model.h"
#include "assertion.h"
#include "metalib.h"
#include <sstream>

bool
LogSubmodel::check_entry (symbol, const symbol) const
{ return is_active; }

bool
LogSubmodel::check_derived (symbol, symbol, const symbol) const
{ return is_active; }

symbol
LogSubmodel::entry () const
{ 
  daisy_assert (entry_stack.size () > 0U);
  return entry_stack.front ();
}

const Library&
LogSubmodel::library () const
{ 
  daisy_assert (library_stack.size () > 0U);
  daisy_assert (library_stack.front ());
  return *library_stack.front ();
}

Frame&
LogSubmodel::frame_entry () const
{
  daisy_assert (frame_stack.size () > 0U);
  daisy_assert (frame_stack.front ());
  return *frame_stack.front ();
}

std::vector<const Frame*>&
LogSubmodel::frame_sequence ()
{
  daisy_assert (frame_sequence_stack.size () > 0U);
  return frame_sequence_stack.front ();
}

int
LogSubmodel::unnamed ()
{
  daisy_assert (unnamed_stack.size () > 0U);
  return unnamed_stack.front ();
}

void
LogSubmodel::push (symbol entry, const Library& library, const Frame& frame)
{
  entry_stack.push_front (entry);
  library_stack.push_front (&library);
  frame_stack.push_front (&frame.clone ());
  frame_sequence_stack.push_front (std::vector<const Frame*> ());
  unnamed_stack.push_front (-1);
}

void
LogSubmodel::push (symbol entry, const Frame& frame)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  frame_stack.push_front (&frame.clone ());
  frame_sequence_stack.push_front (std::vector<const Frame*> ());
  unnamed_stack.push_front (-1);
}

void
LogSubmodel::push (symbol entry, 
		const Frame& default_frame,
		std::vector<boost::shared_ptr<const FrameModel>/**/> frame_sequence)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  frame_stack.push_front (&default_frame.clone ());
  std::vector<const Frame*> frames;
  for (std::vector<boost::shared_ptr<const FrameModel>/**/>::const_iterator i
         = frame_sequence.begin ();
       i != frame_sequence.end ();
       i++)
    frames.push_back ((*i).get ());
  frame_sequence_stack.push_front (frames);
  unnamed_stack.push_front (0);
}

void
LogSubmodel::push (symbol entry, 
		const Frame& default_frame,
		std::vector<boost::shared_ptr<const FrameSubmodel>/**/> frame_sequence)
{
  entry_stack.push_front (entry);
  library_stack.push_front (NULL);
  frame_stack.push_front (&default_frame.clone ());
  std::vector<const Frame*> frames;
  for (std::vector<boost::shared_ptr<const FrameSubmodel>/**/>::const_iterator i
         = frame_sequence.begin ();
       i != frame_sequence.end ();
       i++)
    frames.push_back ((*i).get ());
  frame_sequence_stack.push_front (frames);
  unnamed_stack.push_front (0);
}

void
LogSubmodel::pop ()
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
LogSubmodel::open_ignore ()
{ 
  if (nested > 0 || is_active)
    {
      nested++;
      is_active = false;
    }
}

void
LogSubmodel::close_ignore ()
{ 
  if (nested > 0)
    {
      nested--;
      is_active = (nested == 0);
    }
}


void
LogSubmodel::open (const symbol name)
{
  if (is_active)
    {
      if (frame_entry ().is_const (name))
        {
          std::stringstream tmp;
          tmp << "'" << name << "' is const;";
          for (size_t i = 0; i < entry_stack.size (); i++)
            tmp << " " << entry_stack[i];
          daisy_panic (tmp.str ());
        }
      if (frame_entry ().is_state (name))
	{
	  const int size = frame_entry ().type_size (name);
	  const bool has_value = frame_entry ().check (name);
	  switch (frame_entry ().lookup (name))
	    {
	    case Attribute::Submodel:
	      if (size != Attribute::Singleton && has_value)
		push (name, 
		      *frame_entry ().default_frame (name),
		      frame_entry ().submodel_sequence (name));
	      else if (size != Attribute::Singleton || !has_value)
		push (name, 
		      *frame_entry ().default_frame (name));
	      else 
		push (name, 
		      frame_entry ().submodel (name));
		      
	      break;
	    case Attribute::Model:
              {
                const symbol component = frame_entry ().component (name);
                const Library& library = metalib ().library (component);
                daisy_assert (size != Attribute::Singleton);
                push (name, library, library.model ("component"));
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
LogSubmodel::close ()
{ 
  if (is_active)
    {
      // Remember old values.
      Frame& old_frame = frame_entry ();
      std::vector<const Frame*> old_frame_sequence = frame_sequence ();
      const symbol old_entry = entry ();

      // Pop stack.
      pop ();

      // Assign new value to entry.
      if (entry_stack.size () > 0)
	{
	  const Attribute::type type = frame_entry ().lookup (old_entry);
	  switch (type)
	    { 
	    case Attribute::Model:
              {
                // Model sequence.
                daisy_assert (frame_entry ().type_size (old_entry)
                              != Attribute::Singleton);
                std::vector<boost::shared_ptr<const FrameModel>/**/> copy;
                for (size_t i = 0; i < old_frame_sequence.size (); i++)
                  {
                    const FrameModel* model 
                      = dynamic_cast<const FrameModel*> (old_frame_sequence[i]);
                    boost::shared_ptr<const FrameModel> entry (model);
                    copy.push_back (entry);
                  }
                frame_entry ().set (old_entry, copy);
              }
              break;
	    case Attribute::Submodel:
	      // Submodel sequence or singleton.
	      if (frame_entry ().type_size (old_entry) == Attribute::Singleton)
		{
		  daisy_assert (old_frame_sequence.size () == 0);
		  frame_entry ().set (old_entry, 
                                dynamic_cast<const FrameSubmodel&> (old_frame));
		}
	      else
                {
                  std::vector<boost::shared_ptr<const FrameSubmodel>/**/> copy;
                  for (size_t i = 0; i < old_frame_sequence.size (); i++)
                    {
                      const FrameSubmodel* submodel 
                        = dynamic_cast<const FrameSubmodel*> (old_frame_sequence[i]);
                      boost::shared_ptr<const FrameSubmodel> entry (submodel);
                      copy.push_back (entry);
                    }
                  frame_entry ().set (old_entry, copy);
                }
	      delete &old_frame;
	      break;
	    default:
              {
                std::ostringstream tmp;
                tmp << "Can't close " << old_entry << ", which is a " 
                    << Attribute::type_name (type) << "\nStack:";
                for (std::deque<symbol>::const_iterator i 
                       = entry_stack.begin ();
                     i != entry_stack.end ();
                     i++)
                  tmp << " " << *i;
                daisy_panic (tmp.str ());
              }
	    }
	}
    }
  else
    close_ignore (); 
}

void
LogSubmodel::open_unnamed ()	
{ 
  if (is_active)
    {
      if (unnamed () < 0 || unnamed () >= frame_sequence ().size ())
	// Use default frame.
	push (entry (), frame_entry ());
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
LogSubmodel::close_unnamed ()	
{
  if (is_active)
    {
      Frame& old_frame = frame_entry ();
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
      daisy_assert (frame_stack[1]->lookup (sentry) == Attribute::Submodel);
      daisy_assert (frame_stack[1]->type_size (sentry) != Attribute::Singleton);
    }
  else
    close_ignore ();
}

void 
LogSubmodel::open_alist (symbol name, const Frame& f)
{
  if (is_active)
    {
      daisy_assert (frame_entry ().lookup (name) == Attribute::Submodel);
      daisy_assert (frame_entry ().type_size (name) == Attribute::Singleton);
      push (name, f);
    }
  else
    open_ignore ();
}

void
LogSubmodel::close_alist ()
{ 
  if (is_active)
    {
      Frame& old_frame = frame_entry ();
      const std::string& old_entry = entry ().name ();
      pop ();
      frame_entry ().set (old_entry, dynamic_cast<const FrameSubmodel&> (old_frame));
      delete &old_frame;
    }
  else
    close_ignore (); 
}

void
LogSubmodel::open_derived (const symbol field, const symbol type, 
                           const symbol component)
{ 
  if (frame_entry ().is_log (field))
    open_ignore ();
  else if (frame_entry ().check (field))
    open_object (field, type, frame_entry ().model (field), component);
  else
    {
      daisy_assert (metalib ().exist (component));
      const Library& library = metalib ().library (component);
      if (library.check (type))
        open_object (field, type, library.model (type), component);
      else
        {
          std::ostringstream tmp;
          tmp << "No field '" << field << "' (type " << type
              << ") within library '" << component << "'";
          daisy_panic (tmp.str ());
        }
    }
}
	
void
LogSubmodel::close_derived ()
{ close_object (); }

void
LogSubmodel::open_object (symbol field, symbol type, 
                       const Frame& f, const symbol lib)
{ 
  if (is_active)
    {
      const std::string& sfield = field.name ();
      if (frame_entry ().lookup (sfield) != Attribute::Model)
	daisy_panic ("Field '" + sfield + "' should be a Model but is a '" +
		     Attribute::type_name (frame_entry ().lookup (sfield))
		     + "'");
      daisy_assert (frame_entry ().type_size (sfield) == Attribute::Singleton);
      const symbol component = frame_entry ().component (sfield);
      const Library& library = metalib ().library (component);
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
LogSubmodel::close_object ()
{ 
  if (is_active)
    {
      Frame& old_frame = frame_entry ();
      const symbol old_entry = entry ();
      pop ();
      frame_entry ().set (old_entry, dynamic_cast<const FrameModel&> (old_frame));
      delete &old_frame;
    }
  else
    close_ignore (); 
}

void
LogSubmodel::open_entry (symbol type, const Frame& frame, const symbol)
{
  if (is_active)
    push (type, frame);
  else
    open_ignore (); 
}

void
LogSubmodel::close_entry ()
{
  if (is_active)
    {
      Frame& old_frame = frame_entry ();
      pop ();
      frame_sequence ().push_back (&old_frame);
    }
  else
    close_ignore ();
}

void
LogSubmodel::open_named_entry (const symbol, const symbol type,
			    const Frame& frame)
{ open_entry (type, frame, Attribute::None ()); }

void
LogSubmodel::close_named_entry ()
{ close_entry (); }

void
LogSubmodel::open_shallow (const symbol, const symbol)
{ open_unnamed (); }

void
LogSubmodel::close_shallow ()
{ close_unnamed (); }

void
LogSubmodel::output_entry (symbol name, const bool value)
{ 
  if (!is_active)
    return;
  daisy_assert (!frame_entry ().is_const (name));
  if (frame_entry ().is_state (name))
    frame_entry ().set (name, value);
}

void
LogSubmodel::output_entry (symbol name, const double value)
{ 
  if (!is_active)
    return;
  daisy_assert (!frame_entry ().is_const (name));
  if (frame_entry ().is_state (name))
    frame_entry ().set (name, value);
}

void
LogSubmodel::output_entry (symbol name, const int value)
{ 
  if (!is_active)
    return;
  daisy_assert (!frame_entry ().is_const (name));
  if (frame_entry ().is_state (name))
    frame_entry ().set (name, value);
}

void
LogSubmodel::output_entry (symbol name, symbol value)
{ 
  if (!is_active)
    return;
  daisy_assert (!frame_entry ().is_const (name));
  if (frame_entry ().is_state (name))
    frame_entry ().set (name, value);
}

void
LogSubmodel::output_entry (symbol name, const std::vector<double>& value)
{ 
  if (!is_active)
    return;
  daisy_assert (!frame_entry ().is_const (name));
  if (frame_entry ().is_state (name))
    frame_entry ().set (name, value);
}

void
LogSubmodel::output_entry (symbol name, const PLF& value)
{ 
  if (!is_active)
    return;
  daisy_assert (!frame_entry ().is_const (name));
  if (frame_entry ().is_state (name))
    frame_entry ().set (name, value);
}

bool
LogSubmodel::check (const Border&, Treelog&) const
{ return true; }

LogSubmodel::LogSubmodel (const BlockModel& al)
  : Log (al),
    is_active (false),
    nested (0)
{ }

LogSubmodel::~LogSubmodel ()
{ }

// log_alist.C ends here.
