// frame.h -- Names with typed values.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "syntax.h"
#include <memory>
#include <string>
#include <vector>

class AttributeList;
class PLF;

#ifndef FRAME_H
#define FRAME_H

class Frame
{
  struct Implementation;
  std::auto_ptr<Implementation> impl;

  // Old style access.
private:
  const AttributeList& alist () const;
  const Syntax& syntax () const;
  
  // Common access.
public:
  // Get a list of all entries.
  void entries (std::vector<std::string>&) const;
  unsigned int entries () const;

  // This function will check that the alist conform to the syntax.
  bool check (const Metalib&, Treelog& err) const;
  
  // Check that a numeric value is within the allowed range.
  void check (const std::string& key, double value) const;

  // Check than an arbitrary attribute is valid.
  bool check (const Metalib&, const std::string& key, Treelog&) const;

  // Extract type information about a specific attribute.
  bool is_const (const std::string&) const;
  bool is_optional (const std::string&) const;
  bool is_log (const std::string&) const;
  bool is_state (const std::string&) const;
  Syntax::type lookup (const std::string&) const;
  const Syntax& syntax (const std::string&) const;
  ::Library& library (const Metalib&, const std::string&) const;
  int  size (const std::string&) const;
  const std::string& dimension (const std::string&) const;
  const std::string& domain (const std::string&) const;
  const std::string& range (const std::string&) const;
  const std::string& description (const std::string&) const;
  const AttributeList& default_alist (const std::string&) const;

  // Add attribute types.
public:
  void add (const std::string& key,	// Generic.
	    Syntax::type t, 
	    Syntax::category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key,
	    Syntax::type t, 
	    Syntax::category cat,
	    const std::string& description)
  { add (key, t, cat, Syntax::Singleton, description); }

  void add (const std::string& key, // Number.
	    const std::string& dim,
	    Syntax::category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& dim,
	    Syntax::category cat,
	    const std::string& description)
  { add (key, dim, cat, Syntax::Singleton, description); } 
  void add (const std::string& key,
	    const std::string& dim,
	    const Check& check,
	    Syntax::category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& dim,
	    const Check& check,
	    Syntax::category cat,
	    const std::string& description)
  { add (key, dim, check, cat, Syntax::Singleton, description); } 
  void add_fraction (const std::string& key, 
		     Syntax::category cat,
		     int size,
		     const std::string& description);
  void add_fraction (const std::string& key, 
		     Syntax::category cat,
		     const std::string& description);

  void add (const std::string& key, // PLF.
	    const std::string& domain,
	    const std::string& range,
	    Syntax::category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& domain,
	    const std::string& range,
	    Syntax::category cat,
	    const std::string& description)
  { add (key, domain, range, cat, Syntax::Singleton, description); } 
  void add (const std::string& key,
	    const std::string& domain,
	    const std::string& range,
	    const Check& check,
	    Syntax::category cat,
	    int size,
	    const std::string& description);
  void add (const std::string& key, 
	    const std::string& domain,
	    const std::string& range,
	    const Check& check,
	    Syntax::category cat,
	    const std::string& description)
  { add (key, domain, range, check, cat, Syntax::Singleton, description); } 

  void add (const std::string& key,  // AList
	    const Syntax& syntax,
	    int size,
	    const std::string& description)
  { add (key, syntax, Syntax::State, size, description); }
  void add (const std::string& key,  // AList
	    const Syntax& syntax,
	    const std::string& description)
  { add (key, syntax, Syntax::State, Syntax::Singleton, description); }
  void add (const std::string&, const Syntax&,
	    Syntax::category cat, int size, 
	    const std::string& description);
  void add (const std::string&, const Syntax&, const AttributeList&,	
	    // Alist sequence with default element.
	    Syntax::category, int size, const std::string& description);

  void add_object (const std::string& key,// Object
                   const char *const lib, 
                   const std::string& description)
  { add_object (key, lib, Syntax::State, Syntax::Singleton, description); }
  void add_object (const std::string&, const char* lib,
                   Syntax::category, int size, const std::string& description);
  void add_object (const std::string&, symbol lib,
                   Syntax::category, int size, const std::string& description);

  void add_library (const std::string&, symbol lib);

  typedef void (*load_syntax_fun) (Syntax& syntax, AttributeList& alist);
  void add_submodule (const std::string& name, AttributeList& alist,
		      Syntax::category cat, const std::string& description,
		      load_syntax_fun load_syntax);
  void add_submodule_sequence (const std::string& name, Syntax::category cat, 
			       const std::string& description,
			       load_syntax_fun load_syntax);

  // Additional type constraints for a single attribute value. 
  void add_check (const std::string& name, const VCheck& vcheck);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
public:
  void order (const std::vector<std::string>&);
  void order (const std::string&);
  void order (const std::string&, const std::string&);
  void order (const std::string&, const std::string&, const std::string&);
  void order (const std::string&, const std::string&, const std::string&,
	      const std::string&);
  void order (const std::string&, const std::string&, const std::string&,
	      const std::string&,
	      const std::string&);

  // Extract the imposed order.
  bool ordered () const;
  const std::vector<std::string>& order () const;
  int order_index (const std::string& name) const; // Return index in order, or -1
  bool total_order () const;	// True iff all members are ordered.

  // Additional constraints involving multiple attributes.
public:
  void add_check (Syntax::check_fun);
  void add_object_check (Syntax::check_object);

  // Is 'key' an element of this alist?
  bool check (const std::string& key) const;
  bool check (const symbol key) const;

  // Extract attribute values.
public:
  double number (const std::string&) const;
  double number (const std::string&, double default_value) const;
  const std::string name (const std::string&) const;
  const std::string name (const std::string&, 
			  const std::string& default_value) const;
  symbol identifier (const std::string&) const;
  symbol identifier (const symbol) const;
  bool flag (const std::string&) const;
  bool flag (const std::string&, bool default_value) const;
  const PLF& plf (const std::string&) const;
  AttributeList& alist (const std::string&) const;
  int integer (const std::string&) const;
  int integer (const std::string&, int default_value) const;
  const std::vector<double>& number_sequence (const std::string&) const;
  const std::vector<symbol> identifier_sequence (const std::string& key) const;
  std::vector<std::string> name_sequence (const std::string& key) const;
  const std::vector<bool>& flag_sequence (const std::string& key) const;
  const std::vector<int>& integer_sequence (const std::string& key) const;
  const std::vector<const PLF*>& plf_sequence (const std::string& key) const;
  const std::vector<const AttributeList*>& 
  /**/ alist_sequence (const std::string& key) const;

  // Set attribute values.
public:
  void add (const std::string&, double);
  void add (const std::string&, double, const std::string&);
  void add (const std::string&, const char*);
  void add (const std::string&, const std::string&);
  void add (const std::string&, symbol);
  void add (const std::string&, bool);
  void add (const std::string&, int);
  void add (const std::string&, const AttributeList&);
  void add (const std::string&, const PLF&);
  void add (const std::string&, const std::vector<double>&);
  void add (const std::string&, const std::vector<symbol>&);
  void add_strings (const std::string& key, const std::string& a);
  void add_strings (const std::string& key,
                    const std::string& a, const std::string& b);
  void add_strings (const std::string& key,
                    const std::string& a, const std::string& b,
                    const std::string& c);
  void add (const std::string&, const std::vector<bool>&);
  void add (const std::string&, const std::vector<int>&);
  void add (const std::string&, const std::vector<const AttributeList*>&);
  void add (const std::string&, const std::vector<const PLF*>&);

  // Create and Destroy.
private:
  Frame (const Frame&);         // Disable.
  Frame& operator= (const Frame&); // Disable.
public:
  enum parent_relationship_t { parent_inherit };
  Frame (const Frame&, parent_relationship_t);
  Frame ();
  ~Frame ();
};

#endif // FRAME_H
