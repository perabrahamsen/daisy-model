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

#include "treelog.h"
#include "symbol.h"
#include <vector>
#include <string>

class AttributeList;
class Library;
class Check;
class VCheck;

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
  static symbol unknown ();
  static const std::string& Unknown ();
  static symbol none ();
  static const std::string& None ();
  static symbol fraction ();
  static const std::string& Fraction ();
  static symbol user ();
  static const std::string& User ();

  // Each syntax entry should have an associated type.
  enum type 
  { Number, AList, PLF, Boolean, String,
    Integer, Object, Library, Error };
  static const std::string& type_name (type);
  static type type_number (const std::string& name);
    
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
  static const std::string& category_name (category);
  static int category_number (const std::string& name);

  // This function will check that an alist conform to the syntax.
  bool check (const AttributeList&, Treelog& err) const;
  
  // Check that a numeric value is within the allowed range.
  void check (const std::string& key, double value) const;

  // Check than an arbitrary alist member is valid.
  bool check (const AttributeList&, const std::string& key, Treelog&) const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (const std::string&) const;
  bool is_optional (const std::string&) const;
  bool is_log (const std::string&) const;
  bool is_state (const std::string&) const;

  type lookup (const std::string&) const;
  const Syntax& syntax (const std::string&) const;
  ::Library& library (const std::string&) const;
  int  size (const std::string&) const;
  const std::string& dimension (const std::string&) const;
  const std::string& domain (const std::string&) const;
  const std::string& range (const std::string&) const;
  const std::string& description (const std::string&) const;
  bool ordered () const;
  const std::vector<std::string>& order () const;
  int order (const std::string& name) const;	// Return index in order, or -1
  bool total_order () const;	// True iff all members are ordered.
  const AttributeList& default_alist (const std::string&) const;

  // Get a list of all entries.
  void entries (std::vector<std::string>&) const;
  unsigned int entries () const;

  // Add syntax entries
  void add (const std::string& key,	// Generic.
	    type t, 
	    category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key,
	    type t, 
	    category cat,
	    const std::string& description)
  { add (key, t, cat, Singleton, description); }

  void add (const std::string& key, // Number.
	    const std::string& dim,
	    category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& dim,
	    category cat,
	    const std::string& description)
  { add (key, dim, cat, Singleton, description); } 
  void add (const std::string& key,
	    const std::string& dim,
	    const Check& check,
	    category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& dim,
	    const Check& check,
	    category cat,
	    const std::string& description)
  { add (key, dim, check, cat, Singleton, description); } 
  void add_fraction (const std::string& key, 
		     category cat,
		     int size,
		     const std::string& description);
  void add_fraction (const std::string& key, 
		     category cat,
		     const std::string& description);

  void add (const std::string& key, // PLF.
	    const std::string& domain,
	    const std::string& range,
	    category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& domain,
	    const std::string& range,
	    category cat,
	    const std::string& description)
  { add (key, domain, range, cat, Singleton, description); } 
  void add (const std::string& key,
	    const std::string& domain,
	    const std::string& range,
	    const Check& check,
	    category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& domain,
	    const std::string& range,
	    const Check& check,
	    category cat,
	    const std::string& description)
  { add (key, domain, range, check, cat, Singleton, description); } 

  void add (const std::string& key,  // AList
	    const Syntax& syntax,
	    int size,
	    const std::string& description)
  { add (key, syntax, State, size, description); }
  void add (const std::string& key,  // AList
	    const Syntax& syntax,
	    const std::string& description)
  { add (key, syntax, State, Singleton, description); }
  void add (const std::string&, const Syntax&,
	    category cat, int size, 
	    const std::string& description);
  void add (const std::string&, const Syntax&, const AttributeList&,	
	    // Alist sequence with default element.
	    category, int size, const std::string& description);

  void add (const std::string& key,// Object
	    ::Library& lib, 
	    const std::string& description)
  { add (key, lib, State, Singleton, description); }
  void add (const std::string&, ::Library&,
	    category, int size, const std::string& description);

  void add_library (const std::string&, ::Library&);

  typedef void (*load_syntax_fun) (Syntax& syntax, AttributeList& alist);
  void add_submodule (const std::string& name, AttributeList& alist,
		      Syntax::category cat, const std::string& description,
		      load_syntax_fun load_syntax);
  void add_submodule_sequence (const std::string& name, Syntax::category cat, 
			       const std::string& description,
			       load_syntax_fun load_syntax);
		      
  void add_check (const std::string& name, const VCheck& vcheck);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
  void order (const std::vector<std::string>&);
  void order (const std::string&);
  void order (const std::string&, const std::string&);
  void order (const std::string&, const std::string&, const std::string&);
  void order (const std::string&, const std::string&, const std::string&,
	      const std::string&);
  void order (const std::string&, const std::string&, const std::string&,
	      const std::string&,
	      const std::string&);

  // Create and Destroy.

  // A check_fun is a function used for extra syntax checking.
  typedef bool (*check_fun)(const AttributeList&, Treelog& err);
  typedef bool (*check_list_fun)(const std::vector<AttributeList*>&,
				 Treelog& err);
  void add_check (check_fun);

  explicit Syntax ();
  explicit Syntax (const Syntax&);
  ~Syntax ();
private:
  Syntax& operator= (const Syntax&);
};

#endif // SYNTAX_H
