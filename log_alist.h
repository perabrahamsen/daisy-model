// log_alist.h
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


#ifndef LOG_ALIST_H
#define LOG_ALIST_H

#include "log.h"
#include <deque>

struct LogAList : public Log
{
  // Filter functions.
  bool check_entry (symbol, const Library&) const;
  bool check_derived (symbol, symbol, const Library&) const;

  // Content.
  bool is_active;		// ... store the answer here.
  unsigned int nested;		// Nesting iff active.

  // Name of the entity we are logging.
  std::deque<symbol> entry_stack; 
  // Library of the object we are logging.
  std::deque<const Library*> library_stack; 
  // Syntax of the alist we are logging.
  std::deque<const Syntax*> syntax_stack; 
  // State and parameters of entity.
  std::deque<AttributeList*> alist_stack; 
  // Ditto for lists.
  std::deque<std::vector<AttributeList*>/**/> alist_sequence_stack; 
  // Current element of AList sequence.
  std::deque<int> unnamed_stack;

  // Stack Accessors.
  symbol entry () const;
  const Library& library () const;
  const Syntax& syntax () const;
  AttributeList& alist () const;
  std::vector<AttributeList*>& alist_sequence ();
  int unnamed ();

  // Stack Constructors.
  void push (symbol entry, 
	     const Library& library, const AttributeList& alist);
  void push (symbol entry, 
	     const Syntax& syntax, const AttributeList& alist);
  void push (symbol entry, 
	     const Syntax& syntax, 
	     const AttributeList& default_alist,
	     std::vector<AttributeList*> alist_sequence);
  void pop ();

  // Nesting.
  void open_ignore ();		// Ignored items.
  void close_ignore ();
  void open (symbol name); // AList singletons. 
  void close ();
  void open_unnamed ();		// Items in a AList sequence.
  void close_unnamed ();	
  void open_alist (symbol name, // AList singletons variant.
		   const AttributeList& alist);
  void close_alist ();

  void open_derived (symbol field, // Object singletons.
		     symbol type); 
  void close_derived ();
  void open_object (symbol field, // Object singletons with alist.
                    symbol type, const AttributeList&); 
  void close_object ();
  void open_entry (symbol type,   // Items in an Object sequence.
		   const AttributeList& alist);
  void close_entry ();
  void open_named_entry (symbol name,   // Named items in an Obj seq.
			 symbol type, 
			 const AttributeList& alist);
  void close_named_entry ();

  // Logging.
  void output (symbol name, bool value);
  void output (symbol name, double value);
  void output (symbol name, int value);
  void output (symbol name, symbol value);
  void output (symbol name, const std::vector<double>& value);
  void output (symbol name, const PLF& value);
  void output (symbol name, const Time& value);

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
  bool check (Treelog& err) const;
  LogAList (const AttributeList& al);
  ~LogAList ();
};

#endif // LOG_ALIST_H
