// syntax.h
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


#ifndef SYNTAX_H
#define SYNTAX_H

#include "common.h"
#include "treelog.h"
#include <vector>

class AttributeList;
class Library;
class Check;

class Syntax
{ 
  struct Implementation;
  friend struct Implementation;
  Implementation& impl;
public:
  // A syntax entry has an associated size.  If the size is a positive
  // integer, the syntax entry specifies an array of that size.  The
  // default size 'Singleton' indicating that the syntax entry match a
  // single item of the specified type, while the 'Sequence' used for
  // entries that contain an array of unspecified length. 
  static const int Singleton;	
  static const int Sequence;
  static const int Unspecified;

  // A syntax may have a dimension associated.
  static const string& Unknown ();
  static const string& None ();
  static const string& Fraction ();

  // Each syntax entry should have an associated type.
  enum type 
  { Number, AList, PLF, Boolean, String,
    Date, Integer, Object, Library, Error };
  static const char* type_name (type);
  static type type_number (const char* name);
    
  // The requirements with regard to input and output varies with each
  // syntax entry.
  enum category
  {
    // This is a parameter, i.e. its value doesn't change during the
    // compilation, and it cannot be written to the log.
    Const,
    // This a state variable, it must be provided at initialization
    // and can be written to the log.
    State,
    // This is a state variable that can be computed from other
    // parameters or state variables, and therefore does not need to
    // be specified before the simulation starts. 
    OptionalState, 
    // This is a paramter that can be computer from other parameters,
    // and therefore does not need to be specified before 
    // the simulation starts. 
    OptionalConst, 
    // This is a variable that is only computed for logging purposes
    // and not a part of the simulation state. 
    LogOnly
  };
  static const char* category_name (category);
  static int category_number (const char* name);

  // This function will check that an alist conform to the syntax.
  bool check (const AttributeList&, Treelog& err) const;
  
  // Check that a numeric value is within the allowed range.
  void check (const string& key, double value) const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (const string&) const;
  bool is_optional (const string&) const;
  bool is_log (const string&) const;
  bool is_state (const string&) const;

  type lookup (const string&) const;
  const Syntax& syntax (const string&) const;
public:
  ::Library& library (const string&) const;
  int  size (const string&) const;
  const string& dimension (const string&) const;
  const string& domain (const string&) const;
  const string& range (const string&) const;
  const string& description (const string&) const;
  bool ordered () const;
  const vector<string>& order () const;
  int order (const string& name) const;	// Return index in order, or -1.
  bool total_order () const;	// True iff all members are ordered.
  const AttributeList& default_alist (const string&) const;

  // Get a list of all entries.
  void entries (vector<string>&) const;
  unsigned int entries () const;

  // Add syntax entries
  void add (const string& key,	// Generic.
	    type t, 
	    category cat,
	    int size,
	    const string& description);
  void add (const string& key,
	    type t, 
	    category cat,
	    const string& description)
  { add (key, t, cat, Singleton, description); }

  void add (const string& key, // Number.
	    const string& dim,
	    category cat,
	    int size,
	    const string& description);
  void add (const string& key, 
	    const string& dim,
	    category cat,
	    const string& description)
  { add (key, dim, cat, Singleton, description); } 
  void add (const string& key,
	    const string& dim,
	    const Check& check,
	    category cat,
	    int size,
	    const string& description);
  void add (const string& key, 
	    const string& dim,
	    const Check& check,
	    category cat,
	    const string& description)
  { add (key, dim, check, cat, Singleton, description); } 

  void add (const string& key, // PLF.
	    const string& domain,
	    const string& range,
	    category cat,
	    int size,
	    const string& description);
  void add (const string& key, 
	    const string& domain,
	    const string& range,
	    category cat,
	    const string& description)
  { add (key, domain, range, cat, Singleton, description); } 

  void add (const string& key,  // AList
	    const Syntax& syntax,
	    int size,
	    const string& description)
  { add (key, syntax, State, size, description); }
  void add (const string& key,  // AList
	    const Syntax& syntax,
	    const string& description)
  { add (key, syntax, State, Singleton, description); }
  void add (const string&, const Syntax&,
	    category cat, int size, 
	    const string& description);
  void add (const string&, const Syntax&, const AttributeList&,	
	    // Alist sequence with default element.
	    category, int size, const string& description);

  void add (const string& key, // Object
	    ::Library& lib, 
	    int size,
	    const string& description)
  { add (key, lib, State, size, description); }
  void add (const string& key,
	    ::Library& lib, 
	    const string& description)
  { add (key, lib, State, Singleton, description); }
  void add (const string&, ::Library&,
	    category, int size, const string& description);

  void add_library (const string&, ::Library&);

  typedef void (*load_syntax_fun) (Syntax& syntax, AttributeList& alist);
  void add_submodule (const string& name, AttributeList& alist,
		      Syntax::category cat, const string& description,
		      load_syntax_fun load_syntax);
  void add_submodule_sequence (const string& name, Syntax::category cat, 
			       const string& description,
			       load_syntax_fun load_syntax);
		      
  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
  void order (const vector<string>&);
  void order (const string&);
  void order (const string&, const string&);
  void order (const string&, const string&, const string&);
  void order (const string&, const string&, const string&, const string&);
  void order (const string&, const string&, const string&, const string&,
	      const string&);

  // Create and Destroy.

  // A check_fun is a function used for extra syntax checking.
  typedef bool (*check_fun)(const AttributeList&, Treelog& err);
  typedef bool (*check_list_fun)(const vector<AttributeList*>&, Treelog& err);
  void add_check (check_fun);
  void add_check (check_list_fun);

  Syntax ();
  ~Syntax ();
private:
  Syntax (Syntax&);
  Syntax& operator= (Syntax&);
};

void check (const AttributeList& al, const string& s, bool& ok, Treelog& err);
void non_negative (double v, const string& s, bool& ok, Treelog& err, 
		   int index = -1);
void non_positive (double v, const string& s, bool& ok, Treelog& err, 
		   int index = -1);

#endif // SYNTAX_H
