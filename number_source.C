// number_source.C -- Extract a single number from a time series.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#include "number.h"
#include "source.h"
#include <sstream>
#include <memory>

struct NumberSource : public Number
{
  const std::auto_ptr<Source> source;
  enum { uninitialized, error, is_missing, has_value } state;
  double val;

  const std::string& title () const
  {
    daisy_assert (state != uninitialized);
    return source->title ();
  }
  bool missing (const Scope&) const 
  { 
    daisy_assert (state != uninitialized);
    return state != has_value;
  }
  double value (const Scope&) const
  { 
    daisy_assert (state == has_value);
    return val;
  }
  const std::string& dimension (const Scope&) const 
  {     
    daisy_assert (state != uninitialized);
    return source->dimension ();
  }

  // Create.
  virtual void initialize_derived (Treelog& msg) = 0;
  bool initialize (Treelog& msg)
  {
    Treelog::Open nest (msg, name);
    daisy_assert (state == uninitialized);
    daisy_assert (source.get ());
    if (!source->load (msg))
      state = error;
    else
      initialize_derived (msg);
    return state != error;
  }
  bool check (const Scope&, Treelog&) const
  { 
    daisy_assert (state != uninitialized);
    return state != error; 
  }

  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add ("source", Librarian<Source>::library (), "\
The time series we want to extract a number from.");
  }
  NumberSource (Block& al)
    : Number (al),
      source (Librarian<Source>::build_item (al, "source")),
      state (uninitialized),
      val (-42.42e42)
  { }
};

struct NumberSourceUnique : public NumberSource
{
  void initialize_derived (Treelog& msg)
  {
    size_t size = source->value ().size ();
    if (size == 1U)
      {
        val = source->value ()[0];
        state = has_value;
      }
    else if (size == 0U)
      {
        msg.warning ("Got zero elements, expected one");
        state = is_missing;
      }
    else
      {
        std::ostringstream tmp;
        tmp << "Got " << size << " elements, expected 1";
        msg.error (tmp.str ());
        state = error;
      }
  }
  NumberSourceUnique (Block& al)
    : NumberSource (al)
  { }
};

static struct NumberSourceUniqueSyntax
{
  static Number& make (Block& al)
  { return *new NumberSourceUnique (al); }
  NumberSourceUniqueSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Find unique number in time series.");
    NumberSource::load_syntax (syntax, alist);
    Librarian<Number>::add_type ("source_unique", alist, syntax, &make);
  }
} NumberSourceUnique_syntax;

// number_source.C ends here
