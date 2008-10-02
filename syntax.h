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
class Metalib;
class Check;
class VCheck;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT Syntax
{ 
  struct Implementation;
  friend struct Implementation;
  std::auto_ptr<Implementation> impl;
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
  static symbol Unknown ();
  static symbol None ();
  static symbol Fraction ();
  static symbol User ();

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
  bool check (const Metalib&, const AttributeList&, Treelog& err) const;
  
  // Check that a numeric value is within the allowed range.
  void check (symbol key, double value) const;

  // Check than an arbitrary alist member is valid.
  bool check (const Metalib&,
	      const AttributeList&, symbol key, Treelog&) const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (symbol) const;
  bool is_optional (symbol) const;
  bool is_log (symbol) const;
  bool is_state (symbol) const;

  type lookup (symbol) const;
  const Syntax& syntax (symbol) const;
  ::Library& library (const Metalib&, symbol) const;
  int  size (symbol) const;
  symbol dimension (symbol) const;
  symbol domain (symbol) const;
  symbol range (symbol) const;
  symbol description (symbol) const;
  bool ordered () const;
  const std::vector<symbol>& order () const;
  int order_index (symbol name) const; // Return index in order, or -1
  bool total_order () const;	// True iff all members are ordered.
  const AttributeList& default_alist (symbol) const;

  // Get a list of all entries.
  void entries (std::vector<symbol>&) const;
  unsigned int entries () const;

  // Add syntax entries
  void add (symbol key,	// Generic.
	    type t, 
	    category cat,
	    int size,
	    const symbol description);
  void add (symbol key,
	    type t, 
	    category cat,
	    const symbol description)
  { add (key, t, cat, Singleton, description); }

  void add (symbol key, // Number.
	    symbol dim,
	    category cat,
	    int size,
	    const symbol description);
  void add (symbol key, 
	    symbol dim,
	    category cat,
	    const symbol description)
  { add (key, dim, cat, Singleton, description); } 
  void add (symbol key,
	    symbol dim,
	    const Check& check,
	    category cat,
	    int size,
	    const symbol description);
  void add (symbol key, 
	    symbol dim,
	    const Check& check,
	    category cat,
	    const symbol description)
  { add (key, dim, check, cat, Singleton, description); } 
  void add_fraction (symbol key, 
		     category cat,
		     int size,
		     const symbol description);
  void add_fraction (symbol key, 
		     category cat,
		     const symbol description);

  void add (symbol key, // PLF.
	    symbol domain,
	    symbol range,
	    category cat,
	    int size,
	    const symbol description);
  void add (symbol key, 
	    symbol domain,
	    symbol range,
	    category cat,
	    const symbol description)
  { add (key, domain, range, cat, Singleton, description); } 
  void add (symbol key,
	    symbol domain,
	    symbol range,
	    const Check& check,
	    category cat,
	    int size,
	    const symbol description);
  void add (symbol key, 
	    symbol domain,
	    symbol range,
	    const Check& check,
	    category cat,
	    const symbol description)
  { add (key, domain, range, check, cat, Singleton, description); } 

  void add (symbol key,  // AList
	    const Syntax& syntax,
	    int size,
	    const symbol description)
  { add (key, syntax, State, size, description); }
  void add (symbol key,  // AList
	    const Syntax& syntax,
	    const symbol description)
  { add (key, syntax, State, Singleton, description); }
  void add (symbol, const Syntax&,
	    category cat, int size, 
	    const symbol description);
  void add (symbol, const Syntax&, const AttributeList&,	
	    // Alist sequence with default element.
	    category, int size, const symbol description);

  void add_object (symbol key,// Object
                   const char *const lib, 
                   const symbol description)
  { add_object (key, lib, State, Singleton, description); }
  void add_object (symbol, const char* lib,
                   category, int size, const symbol description);
  void add_object (symbol, symbol lib,
                   category, int size, const symbol description);

  void add_library (symbol, symbol lib);

  typedef void (*load_syntax_fun) (Syntax& syntax, AttributeList& alist);
  void add_submodule (symbol name, AttributeList& alist,
		      Syntax::category cat, const symbol description,
		      load_syntax_fun load_syntax);
  void add_submodule_sequence (symbol name, Syntax::category cat, 
			       const symbol description,
			       load_syntax_fun load_syntax);
		      
  void add_check (symbol name, const VCheck& vcheck);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
  void order (const std::vector<symbol>&);
  void order (symbol);
  void order (symbol, symbol);
  void order (symbol, symbol, symbol);
  void order (symbol, symbol, symbol, symbol);
  void order (symbol, symbol, symbol, symbol, symbol);

  // Create and Destroy.

  // A check_fun is a function used for extra syntax checking.
  typedef bool (*check_fun)(const AttributeList&, Treelog& err);
  typedef bool (*check_object)(const Metalib&, 
                               const AttributeList&, Treelog& err);
  void add_check (check_fun);
  void add_object_check (check_object);

  explicit Syntax ();
  explicit Syntax (const Syntax&);
  ~Syntax ();
private:
  Syntax& operator= (const Syntax&);
};

#endif // SYNTAX_H
