// log_alist.h

#ifndef LOG_ALIST_H
#define LOG_ALIST_H

#include "log.h"
#include <deque.h>

struct LogAList : public Log
{
  // Filter functions.
  bool check (const string&) const = 0;
  bool check_entry (const string&, const Library&) const;
  bool check_derived (const string&, const string&, const Library&) const;

  // Content.
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

  // Nesting.
  void open_ignore ();		// Ignored items.
  void close_ignore ();
  void open (const string& name); // AList singletons. 
  void close ();
  void open_unnamed ();		// Items in a AList sequence.
  void close_unnamed ();	
  void open_alist (const string& name, // AList singletons variant.
		   const AttributeList& alist);
  void close_alist ();

  void open_derived (const string& field, // Object singletons.
		     const string& type); 
  void close_derived ();
  void open_entry (const string& type,   // Items in an Object sequence.
		   const AttributeList& alist);
  void close_entry ();

  // Logging.
  void output (const string& name, const Time& value);
  void output (const string& name, const bool value);
  void output (const string& name, const double value);
  void output (const string& name, const int value);
  void output (const string& name, const string& value);
  void output (const string& name, const vector<double>& value);
  void output (const string& name, const PLF& value);

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
  bool check (const Syntax&, Treelog& err) const;
  LogAList (const AttributeList& al);
  ~LogAList ();
};

#endif // LOG_ALIST_H
