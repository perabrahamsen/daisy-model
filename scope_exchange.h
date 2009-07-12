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
#include "scope_model.h"
#include "memutils.h"
#include <map>
#include <vector>

class Frame;
class BlockModel;

class Exchange : public Model
{
  // Model.
public:
  static const char *const component;
  symbol library_id () const;

  // Content.
private:
  const symbol tag_;
  const symbol description;
public:
  symbol tag () const;

  // Use.
public:
  virtual Attribute::type lookup () const = 0;
  virtual bool check () const = 0;
  virtual double number () const;
  virtual symbol dimension () const;
  virtual symbol name () const;
  symbol get_description () const;
  virtual void set_number (const double);

  // Create and Destroy.
public:
  static void load_syntax (Frame&);
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
  Attribute::type lookup () const;
  bool check () const;
  double number () const;
  symbol dimension () const;
  void set_number (const double val);

  // Create and Destroy.
public:
  explicit ExchangeNumber (const BlockModel& al);
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
  Attribute::type lookup () const;
  bool check () const;
  symbol name () const;

  // Create and Destroy.
public:
  explicit ExchangeName (const BlockModel& al);
  ~ExchangeName ();
};

class ScopeExchange : public MScope
{
  // Parameters.
private: 
  auto_vector<Exchange*> all;
  std::map<symbol, Exchange*> named;

  // Scope.
public:
  void entries (std::set<symbol>&) const;
  Attribute::type lookup (symbol tag) const;
  bool check (symbol tag) const;
  double number (symbol tag) const;
  symbol dimension (symbol tag) const;
  symbol name (symbol tag) const;
  symbol description (symbol tag) const;

  // WScope.
public:
  void set (symbol tag, double value);
  
  // Create.
private:
  static std::map<symbol, Exchange*> 
  /**/ find_named (const std::vector<Exchange*>& entries);
public:
  void add_item (Exchange* item);
  void done ();
  explicit ScopeExchange (symbol title);
  explicit ScopeExchange (const BlockModel& al);
  ~ScopeExchange ();
private:
  ScopeExchange ();
};

// scope_exchange.h ends here.
