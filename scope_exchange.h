// scope_exchange.h -- Exchange values with through a scope.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "model.h"
#include "symbol.h"
#include "scope.h"
#include "memutils.h"
#include <map>
#include <vector>

class Block;
class Syntax;
class AttributeList;

class Exchange : public Model
{
  // Model.
public:
  static const char *const component;
  symbol library_id () const;

  // Content.
public:
  const symbol name_;
  const symbol description;

  // Use.
public:
  virtual bool is_number () const;
  virtual double number () const;
  virtual bool has_number () const;
  virtual symbol dimension () const;
  virtual bool has_name () const;
  virtual symbol name () const;
  symbol get_description () const;
  virtual void set_number (const double);

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
protected:
  explicit Exchange (symbol name, symbol description);
public:
  ~Exchange ();
};

class ExchangeNumber : public Exchange
{
  // Content.
private:
  const symbol dimension_;
  bool has_value;
  double value;

  // Use 
public:
  bool is_number () const;
  double number () const;
  bool has_number () const;
  symbol dimension () const;
  void set_number (const double val);

  // Create and Destroy.
public:
  explicit ExchangeNumber (Block& al);
  explicit ExchangeNumber (symbol name, symbol dimension, symbol description);
  explicit ExchangeNumber (symbol name, double value, 
                           symbol dimension, symbol description);
  ~ExchangeNumber ();
};

class ExchangeName : public Exchange
{
  // Content.
private:
  const symbol value;

  // Use
public: 
  bool has_name () const;
  symbol name () const;

  // Create and Destroy.
public:
  explicit ExchangeName (Block& al);
  ~ExchangeName ();
};

class ScopeExchange : public WScope
{
  // Parameters.
private: 
  auto_vector<Exchange*> entries;
  std::vector<symbol> all_numbers_;
  std::map<symbol, Exchange*> named;

  // Scope.
public:
  const std::vector<symbol>& all_numbers () const;
  bool has_number (symbol tag) const;
  double number (symbol tag) const;
  symbol dimension (symbol tag) const;
  bool has_name (symbol tag) const;
  symbol name (symbol tag) const;
  symbol get_description (symbol tag) const;

  // WScope.
public:
  void set_number (symbol tag, double value);
  
  // Create.
private:
  static std::vector<symbol>
  /**/ find_all_numbers (const std::vector<Exchange*>& entries);
  static std::map<symbol, Exchange*> 
  /**/ find_named (const std::vector<Exchange*>& entries);
public:
  void add_item (Exchange* item);
  void done ();
  explicit ScopeExchange (Block& al);
  explicit ScopeExchange ();
  ~ScopeExchange ();
};

// scope_exchange.h ends here.
