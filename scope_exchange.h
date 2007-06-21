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

class Exchange : public Model
{
  // Content.
public:
  static const char *const component;

  // Use.
public:
  virtual bool is_number () const;
  virtual symbol name () const = 0;
  virtual double number () const;
  virtual bool has_number () const;
  virtual symbol dimension () const;
  virtual bool has_identifier () const;
  virtual symbol identifier () const;
  virtual symbol get_description () const = 0;
  virtual void set_number (const double);

  // Create and Destroy.
public:
  Exchange ();
  ~Exchange ();
};

class ExchangeNumber : public Exchange
{
  // Content.
private:
  const symbol name_;
  const symbol dimension_;
  const symbol description;
  bool has_value;
  double value;

  // Use 
public:
  bool is_number () const;
  symbol name () const;
  double number () const;
  bool has_number () const;
  symbol dimension () const;
  symbol get_description () const;
  void set_number (const double val);

  // Create and Destroy.
public:
  ExchangeNumber (Block& al);
  ~ExchangeNumber ();
};

class ExchangeName : public Exchange
{
  // Content.
private:
  const symbol name_;
  const symbol description;
  const symbol value;

  // Use
public: 
  symbol name () const;
  bool has_identifier () const;
  symbol identifier () const;
  symbol get_description () const;

  // Create and Destroy.
public:
  ExchangeName (Block& al);
  ~ExchangeName ();
};

class ScopeExchange : public WScope
{
  // Parameters.
private: 
  const auto_vector<Exchange*> entries;
  const std::vector<symbol> all_numbers_;
  const std::map<symbol, Exchange*> named;

  // Scope.
public:
  const std::vector<symbol>& all_numbers () const;
  bool has_number (symbol tag) const;
  double number (symbol tag) const;
  symbol dimension (symbol tag) const;
  bool has_identifier (symbol tag) const;
  symbol identifier (symbol tag) const;
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
  ScopeExchange (Block& al);
  ~ScopeExchange ();
};

// scope_exchange.h ends here.
