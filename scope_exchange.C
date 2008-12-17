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

#define BUILD_DLL
#include "scope_exchange.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"
#include "librarian.h"

// The Exchange item model.

const char *const 
Exchange::component = "exchange";

symbol
Exchange::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol 
Exchange::tag () const
{ return tag_; }

symbol 
Exchange::name () const
{ daisy_notreached (); }

double 
Exchange::number () const
{ daisy_notreached (); }

symbol 
Exchange::dimension () const
{ daisy_notreached (); }

symbol 
Exchange::get_description () const
{ return description; }

void 
Exchange::set_number (const double)
{ daisy_notreached (); }

void
Exchange::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("description", Value::String, Value::Const, "\
Description of value to exchange.");
  syntax.add ("name", Value::String, Value::Const, "\
Name of value to exchange.");
}

Exchange::Exchange (const symbol n, const symbol d)
  : tag_ (n),
    description (d)
{ }
  
Exchange::~Exchange ()
{ }

// Exchanging a number.

Value::type 
ExchangeNumber::lookup () const
{ return Value::Number; }

bool
ExchangeNumber::check () const
{ return has_value; }

double 
ExchangeNumber::number () const
{ return value; }

symbol 
ExchangeNumber::dimension () const
{ return dimension_; }

void 
ExchangeNumber::set_number (const double val) 
{ value = val; has_value = true; }

ExchangeNumber::ExchangeNumber (Block& al)
  : Exchange (al.name ("name"), al.name ("description")),
    dimension_ (al.name ("dimension")),
    has_value (al.check ("value")),
    value (al.number ("value", -42.42e42))
{ }
  
ExchangeNumber::ExchangeNumber (const symbol n, const symbol dim, 
                                const symbol desc)
  : Exchange (n, desc),
    dimension_ (dim),
    has_value (false),
    value (-42.42e42)
{ }

ExchangeNumber::ExchangeNumber (const symbol n, const double val, 
                                const symbol dim, const symbol desc)
  : Exchange (n, symbol (desc)),
    dimension_ (dim),
    has_value (true),
    value (val)
{ }

ExchangeNumber::~ExchangeNumber ()
{ }

static struct ExchangeNumberSyntax
{
  static Model& make (Block& al)
  { return *new ExchangeNumber (al); }
  ExchangeNumberSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Exchange::load_syntax (syntax, alist);
    alist.add ("description", "Exchange a numeric value.");
    syntax.add ("dimension", Value::String, Value::Const, "\
Dimension of value to exchange.");
    syntax.add ("value", Value::Unknown (), Value::OptionalState, "\
Current value to exchange.");
    Librarian::add_type (Exchange::component, "number", alist, syntax, &make);
  }
} ExchangeNumber_syntax;

// Exchanging a name (or string).

Value::type 
ExchangeName::lookup () const
{ return Value::String; }

bool
ExchangeName::check () const
{ return true; }

symbol 
ExchangeName::name () const
{ return value; }

ExchangeName::ExchangeName (Block& al)
  : Exchange (al.name ("name"), al.name ("description")),
    value (al.name ("value"))
{ }

ExchangeName::~ExchangeName ()
{ }

static struct ExchangeNameSyntax
{
  static Model& make (Block& al)
  { return *new ExchangeName (al); }
  ExchangeNameSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Exchange::load_syntax (syntax, alist);
    alist.add ("description", "Exchange a string value.");
    syntax.add ("value", Value::String, Value::Const, "\
Current value to exchange.");
    Librarian::add_type (Exchange::component, "name", alist, syntax, &make);
  }
} ExchangeName_syntax;

// The scope.

void 
ScopeExchange::entries (std::vector<symbol>& result) const
{
  for (size_t i = 0; i < all.size (); i++)
    result.push_back (all[i]->tag ());
}

Value::type 
ScopeExchange::lookup (const symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  if (i == named.end ())
    return Value::Error;
  return (*i).second->lookup (); 
}

bool 
ScopeExchange::check (symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  if (i == named.end ())
    return false;
  return (*i).second->check ();
}

double 
ScopeExchange::number (symbol tag) const
{ 
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  return (*i).second->number (); 
}

symbol 
ScopeExchange::dimension (symbol tag) const
{ 
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  return (*i).second->dimension (); 
}

symbol 
ScopeExchange::name (symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  return (*i).second->name (); 
}

symbol 
ScopeExchange::description (symbol tag) const
{ 
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  return (*i).second->get_description ();
}

void 
ScopeExchange::add (symbol tag, double value)
{ 
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  (*i).second->set_number (value); 
}
  
std::map<symbol, Exchange*> 
ScopeExchange::find_named (const std::vector<Exchange*>& entries)
{
  std::map<symbol, Exchange*> result;
  for (size_t i = 0; i < entries.size (); i++)
    result[entries[i]->tag ()] = entries[i];
  return result;
}

void 
ScopeExchange::add_item (Exchange* item)
{ all.push_back (item); }

void 
ScopeExchange::done ()
{ named = find_named (all); }

ScopeExchange::ScopeExchange (Block& al)
  : WScope (al),
    all (Librarian::build_vector<Exchange> (al, "entries")),
    named (find_named (all))
{ }

ScopeExchange::ScopeExchange ()
  : WScope ("ScopeExchange")
{ }

ScopeExchange::~ScopeExchange ()
{ }

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

    syntax.add_object ("entries", Exchange::component, 
                       Value::Const, Value::Sequence,
                       "List of items to exchange.");
    Librarian::add_type (Scope::component, "exchange", alist, syntax, &make);
  }
} ScopeExchange_syntax;

static Librarian Exchange_init (Exchange::component, "\
A named value to exchange with external models.");

// scope_exchange.C ends here.
