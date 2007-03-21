// scope_exchange.C -- Exchange values with external models.
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

#include "scope.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"
#include "memutils.h"
#include <map>

class Exchange : public Model
{
  // Content.
public:
  static const char *const description;
  static const char *const component;

  // Use.
  virtual bool is_number () const
  { return false; }
  virtual symbol name () const = 0;
  virtual double number () const
  { daisy_notreached (); }
  virtual bool has_number () const
  { return false; }
  virtual symbol dimension () const
  { daisy_notreached (); }
  virtual bool has_identifier () const
  { return false; }
  virtual symbol identifier () const
  { daisy_notreached (); }
  virtual symbol get_description () const = 0;
  virtual void set_number (const double)
  { daisy_notreached (); }

  // Create and Destroy.
public:
  Exchange ()
  { }
  ~Exchange ()
  { }
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<Exchange>::content;
#endif

static Librarian<Exchange> Exchange_init;

template<>
BuildBase* Librarian<Exchange>::content = NULL;

const char *const 
Exchange::description = "\
A named value to exchange with external models.";

const char *const Exchange::component = "exchange";

struct ExchangeNumber : public Exchange
{
  // Content.
  const symbol name_;
  const symbol dimension_;
  const symbol description;
  bool has_value;
  double value;

  // Use 
  bool is_number () const
  { return true; }
  symbol name () const
  { return name_; }
  double number () const
  { return value; }
  bool has_number () const
  { return has_value; }
  symbol dimension () const
  { return dimension_; }
  symbol get_description () const
  { return description; }
  void set_number (const double val) 
  { value = val; has_value = true; }

  // Create and Destroy.
  ExchangeNumber (Block& al)
    : name_ (al.identifier ("name")),
      dimension_ (al.identifier ("dimension")),
      description (al.identifier ("description")),
      has_value (al.check ("value")),
      value (al.number ("value", -42.42e42))
  { }
  ~ExchangeNumber ()
  { }
};

static struct ExchangeNumberSyntax
{
  static Model& make (Block& al)
  { return *new ExchangeNumber (al); }
  ExchangeNumberSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("description", Syntax::String, Syntax::Const, "\
Description of value to exchange.");
    alist.add ("description", "Exchange a numeric value.");
    syntax.add ("name", Syntax::String, Syntax::Const, "\
Name of value to exchange.");
    syntax.add ("dimension", Syntax::String, Syntax::Const, "\
Dimension of value to exchange.");
    syntax.add ("value", Syntax::Unknown (), Syntax::OptionalState, "\
Current value to exchange.");
    Librarian<Exchange>::add_type ("number", alist, syntax, &make);
  }
} ExchangeNumber_syntax;

struct ExchangeName : public Exchange
{
  // Content.
  const symbol name_;
  const symbol description;
  const symbol value;

  // Use 
  symbol name () const
  { return name_; }
  bool has_identifier () const
  { return true; }
  symbol identifier () const
  { return value; }
  symbol get_description () const
  { return description; }

  // Create and Destroy.
  ExchangeName (Block& al)
    : name_ (al.identifier ("name")),
      description (al.identifier ("description")),
      value (al.identifier ("value"))
  { }
  ~ExchangeName ()
  { }
};

static struct ExchangeNameSyntax
{
  static Model& make (Block& al)
  { return *new ExchangeName (al); }
  ExchangeNameSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("description", Syntax::String, Syntax::Const, "\
Description of value to exchange.");
    alist.add ("description", "Exchange a numeric value.");
    syntax.add ("name", Syntax::String, Syntax::Const, "\
Name of value to exchange.");
    syntax.add ("value", Syntax::String, Syntax::Const, "\
Current value to exchange.");
    Librarian<Exchange>::add_type ("name", alist, syntax, &make);
  }
} ExchangeName_syntax;

class ScopeExchange : public WScope
{
  // Parameters.
private: 
  const auto_vector<Exchange*> entries;
  const std::vector<symbol> all_numbers_;
  const std::map<symbol, Exchange*> named;

  // Scope.
public:
  const std::vector<symbol>& all_numbers () const
  { return all_numbers_; }
  bool has_number (symbol tag) const
  {
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    if (i == named.end ())
      return false;
    return (*i).second->has_number (); 
  }
  double number (symbol tag) const
  { 
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    daisy_assert (i != named.end ());
    return (*i).second->number (); 
  }
  symbol dimension (symbol tag) const
  { 
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    daisy_assert (i != named.end ());
    return (*i).second->dimension (); 
  }
  bool has_identifier (symbol tag) const
  {
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    daisy_assert (i != named.end ());
    return (*i).second->has_identifier (); 
  }
  symbol identifier (symbol tag) const
  {
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    daisy_assert (i != named.end ());
    return (*i).second->identifier (); 
  }
  symbol get_description (symbol tag) const
  { 
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    daisy_assert (i != named.end ());
    return (*i).second->get_description ();
  }

  // WScope.
public:
  void set_number (symbol tag, double value)
  { 
    const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
    daisy_assert (i != named.end ());
    (*i).second->set_number (value); 
  }
  
  // Create.
private:
  static std::vector<symbol>
  /**/ find_all_numbers (const std::vector<Exchange*>& entries)
  {
    std::vector<symbol> result;
    for (size_t i = 0; i < entries.size (); i++)
      if (entries[i]->is_number ())
        result.push_back (entries[i]->name ());
    return result;
  }
  static std::map<symbol, Exchange*> 
  /**/ find_named (const std::vector<Exchange*>& entries)
  {
    std::map<symbol, Exchange*> result;
    for (size_t i = 0; i < entries.size (); i++)
      result[entries[i]->name ()] = entries[i];
    return result;
  }
public:
  ScopeExchange (Block& al)
    : WScope (al),
      entries (Librarian<Exchange>::build_vector (al, "entries")),
      all_numbers_ (find_all_numbers (entries)),
      named (find_named (entries))
  { }
};

static struct ScopeExchangeSyntax
{
  static Model& make (Block& al)
  { return *new ScopeExchange (al); }
  ScopeExchangeSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Exchange values with an external model.");

    syntax.add ("entries", Librarian<Exchange>::library (), 
                Syntax::Const, Syntax::Sequence,
                "List of items to exchange.");
    Librarian<Scope>::add_type ("exchange", alist, syntax, &make);
  }
} ScopeExchange_syntax;
