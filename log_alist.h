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
  bool check_member (symbol) const = 0;
  bool check_entry (symbol, const Library&) const;
  bool check_derived (symbol, symbol, const Library&) const;

  // Content.
  bool is_active;		// ... store the answer here.
  unsigned int nested;		// Nesting iff active.

  // Stacks.
  deque<symbol> entry_stack;	// Name of the entity we are logging.
  deque<const Library*> library_stack; // Library of the object we are logging.
  deque<const Syntax*> syntax_stack; // Syntax of the alist we are logging.
  deque<AttributeList*> alist_stack; // State and parameters of entity.
  deque<vector<AttributeList*>/**/> alist_sequence_stack; // Ditto for lists.
  deque<int> unnamed_stack;	// Current element of AList sequence.

  // Stack Accessors.
  symbol entry () const;
  const Library& library () const;
  const Syntax& syntax () const;
  AttributeList& alist () const;
  vector<AttributeList*>& alist_sequence ();
  int unnamed ();

  // Stack Constructors.
  void push (symbol entry, 
	     const Library& library, const AttributeList& alist);
  void push (symbol entry, 
	     const Syntax& syntax, const AttributeList& alist);
  void push (symbol entry, 
	     const Syntax& syntax, 
	     const AttributeList& default_alist,
	     vector<AttributeList*> alist_sequence);
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
  void open_entry (symbol type,   // Items in an Object sequence.
		   const AttributeList& alist);
  void close_entry ();
  void open_named_entry (symbol name,   // Named items in an Obj seq.
			 symbol type, 
			 const AttributeList& alist);
  void close_named_entry ();

  // Logging.
  void output (symbol name, const bool value);
  void output (symbol name, const double value);
  void output (symbol name, const int value);
  void output (symbol name, const string& value);
  void output (symbol name, const vector<double>& value);
  void output (symbol name, const PLF& value);
  void output (symbol name, const Time& value);

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
  bool check (const Syntax&, Treelog& err) const;
  LogAList (const AttributeList& al);
  ~LogAList ();
};

#endif // LOG_ALIST_H
