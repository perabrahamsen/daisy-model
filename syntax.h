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

#include "value.h"
#include "symbol.h"
#include <vector>
#include <set>
#include <memory>

class Metalib;
class Check;
class VCheck;
class Treelog;
class Frame;
class FrameSubmodel;

class Syntax
{ 
  struct Implementation;
  friend struct Implementation;
  std::auto_ptr<Implementation> impl;

public:
  // This function will check that an alist conform to the syntax.
  bool check (Metalib&, const Frame&, Treelog& err) const;
  
  // Check that a numeric value is within the allowed range.
  bool verify (symbol key, double value, Treelog&) const;

  // Check than an arbitrary alist member is valid.
  bool check (Metalib&, const Frame&, symbol key, Treelog&) const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (symbol) const;
  bool is_optional (symbol) const;
  bool is_log (symbol) const;
  bool is_state (symbol) const;

  Value::type lookup (symbol) const;
  symbol component (symbol) const;
  int  size (symbol) const;
  symbol dimension (symbol) const;
  symbol domain (symbol) const;
  symbol range (symbol) const;
  symbol description (symbol) const;
  bool ordered () const;
  const std::vector<symbol>& order () const;
  bool total_order () const;	// True iff all members are ordered.
  const FrameSubmodel& default_frame (symbol) const;
  symbol submodel_name (symbol) const;

  // Get a list of all entries.
  void entries (std::set<symbol>&) const;

  // Declare syntax entries
  void declare (symbol key,	// Generic.
	    Value::type t, 
	    Value::category cat,
	    int size,
	    const symbol description);
  void declare (symbol key,
	    Value::type t, 
	    Value::category cat,
	    const symbol description)
  { declare (key, t, cat, Value::Singleton, description); }

  void declare (symbol key, // Number.
	    symbol dim,
	    Value::category cat,
	    int size,
	    const symbol description);
  void declare (symbol key, 
	    symbol dim,
	    Value::category cat,
	    const symbol description)
  { declare (key, dim, cat, Value::Singleton, description); } 
  void declare (symbol key,
	    symbol dim,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description);
  void declare (symbol key, 
	    symbol dim,
	    const Check& check,
	    Value::category cat,
	    const symbol description)
  { declare (key, dim, check, cat, Value::Singleton, description); } 
  void declare_fraction (symbol key, 
		     Value::category cat,
		     int size,
		     const symbol description);
  void declare_fraction (symbol key, 
		     Value::category cat,
		     const symbol description);

  void declare (symbol key, // PLF.
	    symbol domain,
	    symbol range,
	    Value::category cat,
	    int size,
	    const symbol description);
  void declare (symbol key, 
	    symbol domain,
	    symbol range,
	    Value::category cat,
	    const symbol description)
  { declare (key, domain, range, cat, Value::Singleton, description); } 
  void declare (symbol key,
	    symbol domain,
	    symbol range,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description);
  void declare (symbol key, 
	    symbol domain,
	    symbol range,
	    const Check& check,
	    Value::category cat,
	    const symbol description)
  { declare (key, domain, range, check, cat, Value::Singleton, description); } 

  // Submodel.
  typedef void (*load_syntax_t) (Frame&);
  void declare (symbol, load_syntax_t, 
	    Value::category cat, int size, 
	    const symbol description);

  void declare_object (symbol key,// Object
                   const char *const lib, 
                   const symbol description)
  { declare_object (key, lib, Value::State, Value::Singleton, description); }
  void declare_object (symbol, const char* lib,
                   Value::category, int size, const symbol description);
  void declare_object (symbol, symbol lib,
                   Value::category, int size, const symbol description);

  void set_check (symbol name, const VCheck& vcheck);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // declare new entries.
  void order (const std::vector<symbol>&);
  void order (symbol);
  void order (symbol, symbol);
  void order (symbol, symbol, symbol);
  void order (symbol, symbol, symbol, symbol);
  void order (symbol, symbol, symbol, symbol, symbol);

  // Create and Destroy.

  // A check_fun is a function used for extra syntax checking.
  typedef bool (*check_fun)(Metalib&, const Frame&, Treelog&);
  void add_check (check_fun);

  explicit Syntax ();
  explicit Syntax (const Syntax&);
  ~Syntax ();
private:
  Syntax& operator= (const Syntax&);
};

#endif // SYNTAX_H
