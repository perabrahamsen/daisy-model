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

#include "scope.h"
#include <memory>
#include <vector>
#include <set>

class Frame;
class FrameModel;
class FrameSubmodel;
class PLF;
class Block;
class Check;
class VCheck;
class Treelog;
class Metalib;
class Filepos;

#ifndef FRAME_H
#define FRAME_H

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT Frame : public WScope
{

  // Content.
  struct Implementation;
public:
  std::auto_ptr<Implementation> impl;
  virtual symbol type_name () const;
  symbol base_name () const;
  virtual symbol description () const;

public:
  // Parser.
  virtual const Filepos& own_position () const;
  const Filepos& inherited_position () const;
  virtual void reposition (const Filepos&);
  virtual int sequence_id () const;
  virtual bool used_to_be_a_submodel () const;

  // Parent.
public:
  virtual const Frame* parent () const;
  void register_child (const Frame* child) const;
  void unregister_child (const Frame* child) const;
  void reparent_children (const Frame* new_parent) const;
  virtual void replace_parent (const Frame* new_parent) const;

  // Common access.
public:
  // Get a list of all entries.
  void entries (std::set<symbol>&) const;

  // This function will check that the alist conform to the syntax.
  bool check (Block&) const;
  bool check (Metalib&, Treelog&) const;
  bool check (Metalib&, const Frame& frame, Treelog&) const;
  
  // Check that a numeric value is within the allowed range.
  void check (const symbol key, double value) const;

  // Check than an arbitrary attribute is valid.
  bool check (Metalib&, const symbol key, Treelog&) const;
  bool check (Metalib&, const Frame& frame, const symbol key, Treelog&) const;

  // Extract type information about a specific attribute.
  bool is_const (const symbol) const;
  bool is_optional (const symbol) const;
  bool is_log (const symbol) const;
  bool is_state (const symbol) const;
  Value::type lookup (const symbol) const;
  symbol component (symbol) const;
  int type_size (const symbol) const;
  symbol dimension (const symbol) const;
  symbol domain (const symbol) const;
  symbol range (const symbol) const;
  symbol description (const symbol) const;
  const FrameSubmodel& default_frame (const symbol) const;
  symbol submodel_name (const symbol) const;

  // Add attribute types.
public:
  void add (const symbol key,	// Generic.
	    Value::type t, 
	    Value::category cat,
	    int size,
	    const symbol description);
  void add (const symbol key,
	    Value::type t, 
	    Value::category cat,
	    const symbol description)
  { add (key, t, cat, Value::Singleton, description); }

  void add (const symbol key, // Number.
	    const symbol dim,
	    Value::category cat,
	    int size,
	    const symbol description);
  void add (const symbol key, 
	    const symbol dim,
	    Value::category cat,
	    const symbol description)
  { add (key, dim, cat, Value::Singleton, description); } 
  void add (const symbol key,
	    const symbol dim,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description);
  void add (const symbol key, 
	    const symbol dim,
	    const Check& check,
	    Value::category cat,
	    const symbol description)
  { add (key, dim, check, cat, Value::Singleton, description); } 
  void add_fraction (const symbol key, 
		     Value::category cat,
		     int size,
		     const symbol description);
  void add_fraction (const symbol key, 
		     Value::category cat,
		     const symbol description);

  void add (const symbol key, // PLF.
	    const symbol domain,
	    const symbol range,
	    Value::category cat,
	    int size,
	    const symbol description);
  void add (const symbol key, 
	    const symbol domain,
	    const symbol range,
	    Value::category cat,
	    const symbol description)
  { add (key, domain, range, cat, Value::Singleton, description); } 
  void add (const symbol key,
	    const symbol domain,
	    const symbol range,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description);
  void add (const symbol key, 
	    const symbol domain,
	    const symbol range,
	    const Check& check,
	    Value::category cat,
	    const symbol description)
  { add (key, domain, range, check, cat, Value::Singleton, description); } 

  void add_object (const symbol key,// Object
                   const symbol lib, 
                   const symbol description)
  { add_object (key, lib, Value::State, Value::Singleton, description); }
  void add_object (const symbol, symbol lib,
                   Value::category, int size, const symbol description);

  typedef void (*load_syntax_t) (Frame&);
  void add_submodule (const symbol name, 
		      Value::category cat, const symbol description,
		      load_syntax_t load_syntax);
  void add_submodule_sequence (const symbol name, Value::category cat, 
			       const symbol description,
			       load_syntax_t load_syntax);

  // Additional type constraints for a single attribute value. 
  void add_check (const symbol name, const VCheck& vcheck);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
public:
  void order (const std::vector<symbol>&);
  void order (const symbol);
  void order (const symbol, const symbol);
  void order (const symbol, const symbol, const symbol);
  void order (const symbol, const symbol, const symbol,
	      const symbol);
  void order (const symbol, const symbol, const symbol,
	      const symbol,
	      const symbol);

  // Extract the imposed order.
  bool ordered () const;
  const std::vector<symbol>& order () const;
  int order_index (const symbol name) const; // Return index in order, or -1
  bool total_order () const;	// True iff all members are ordered.

  // Additional constraints involving multiple attributes.
  typedef bool (*check_fun)(Metalib&, const Frame&, Treelog&);
public:
  void add_check (check_fun);

public:
  // Is 'key' an element of this alist?
  bool check (const symbol key) const;
  // Is this frame a subset of 'other'?
  bool subset (Metalib&, const Frame& other) const;
  // Is the element 'key' in this alist a subset of the other entry.
  bool subset (Metalib&, const Frame& other, symbol key) const;
  int value_size (symbol key) const;

  // References.
public:
  void add_reference (symbol key, symbol val);
  bool is_reference (symbol key) const;
  symbol get_reference (symbol key) const;

  // Extract attribute values.
public:
  double number (const symbol) const;
  double number (const symbol, double default_value) const;
  symbol name (const symbol) const;
  symbol name (const symbol, const symbol default_value) const;
  bool flag (const symbol) const;
  bool flag (const symbol, bool default_value) const;
  const PLF& plf (const symbol) const;
  const Frame& frame (const symbol) const;
  const FrameModel& model (const symbol) const;
  const FrameSubmodel& submodel (const symbol) const;
  int integer (const symbol) const;
  int integer (const symbol, int default_value) const;
  const std::vector<double>& number_sequence (const symbol) const;
  const std::vector<symbol>& name_sequence (const symbol key) const;
  const std::vector<bool>& flag_sequence (const symbol key) const;
  const std::vector<int>& integer_sequence (const symbol key) const;
  const std::vector<const Frame*>& frame_sequence (const symbol key) const;
  const std::vector<const PLF*>& plf_sequence (const symbol key) const;

  // Set attribute values.
private:
  void verify (symbol key, Value::type want, int size = Value::Singleton) const;
public:
  void add (symbol, double);
  void add (symbol, double, symbol);
  void add (symbol, symbol);
  void add (symbol, const char*); // Avoid matching bool...
  void add (symbol, bool);
  void add (symbol, int);
  void add (symbol, const Frame&);
  void add (symbol, const PLF&);
  void add (symbol, const std::vector<double>&);
  void add (symbol, const std::vector<symbol>&);
  void add_strings (symbol key);
  void add_strings (symbol key, symbol a);
  void add_strings (symbol key, symbol a, symbol b);
  void add_strings (symbol key, symbol a, symbol b, symbol c);
  void add (symbol, const std::vector<bool>&);
  void add (symbol, const std::vector<int>&);
  void add (symbol, const std::vector<const Frame*>&);
  void add (symbol, const std::vector<const PLF*>&);
  void add_empty (symbol);

  // Create and Destroy.
protected:
  Frame (const Frame&);
  Frame (const Frame*);
  Frame ();
public:
  void overwrite_values (const Frame&); // For using metalib as program frame.
  enum parent_link_t { parent_link };
  enum parent_clone_t { parent_clone }; // For temporary models.
  virtual Frame& clone () const = 0;
  typedef void (*load_frame_t) (Frame&);
  virtual void reset ();
  virtual ~Frame ();
};

#endif // FRAME_H
