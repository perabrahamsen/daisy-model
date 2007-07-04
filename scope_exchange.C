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

bool 
Exchange::is_number () const
{ return false; }

symbol 
Exchange::name () const
{ return name_; }

double 
Exchange::number () const
{ daisy_notreached (); }

bool 
Exchange::has_number () const
{ return false; }

symbol 
Exchange::dimension () const
{ daisy_notreached (); }

bool 
Exchange::has_identifier () const
{ return false; }

symbol 
Exchange::identifier () const
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
  syntax.add ("description", Syntax::String, Syntax::Const, "\
Description of value to exchange.");
  syntax.add ("name", Syntax::String, Syntax::Const, "\
Name of value to exchange.");
}

Exchange::Exchange (const symbol n, const symbol d)
  : name_ (n),
    description (d)
{ }
  
Exchange::~Exchange ()
{ }

// Exchanging a number.

bool 
ExchangeNumber::is_number () const
{ return true; }

double 
ExchangeNumber::number () const
{ return value; }

bool 
ExchangeNumber::has_number () const
{ return has_value; }

symbol 
ExchangeNumber::dimension () const
{ return dimension_; }

void 
ExchangeNumber::set_number (const double val) 
{ value = val; has_value = true; }

ExchangeNumber::ExchangeNumber (Block& al)
  : Exchange (al.identifier ("name"), al.identifier ("description")),
    dimension_ (al.identifier ("dimension")),
    has_value (al.check ("value")),
    value (al.number ("value", -42.42e42))
{ }
  
ExchangeNumber::ExchangeNumber (const symbol n, const std::string dim, 
                                const std::string desc)
  : Exchange (n, symbol (desc)),
    dimension_ (dim),
    has_value (false),
    value (-42.42e42)
{ }

ExchangeNumber::ExchangeNumber (const symbol n, const double val, 
                                const std::string dim, const std::string desc)
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
    syntax.add ("dimension", Syntax::String, Syntax::Const, "\
Dimension of value to exchange.");
    syntax.add ("value", Syntax::Unknown (), Syntax::OptionalState, "\
Current value to exchange.");
    Librarian::add_type (Exchange::component, "number", alist, syntax, &make);
  }
} ExchangeNumber_syntax;

// Exchanging a name (or string).

bool 
ExchangeName::has_identifier () const
{ return true; }

symbol 
ExchangeName::identifier () const
{ return value; }

ExchangeName::ExchangeName (Block& al)
  : Exchange (al.identifier ("name"), al.identifier ("description")),
    value (al.identifier ("value"))
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
    syntax.add ("value", Syntax::String, Syntax::Const, "\
Current value to exchange.");
    Librarian::add_type (Exchange::component, "name", alist, syntax, &make);
  }
} ExchangeName_syntax;

// The scope.

const std::vector<symbol>& 
ScopeExchange::all_numbers () const
{ return all_numbers_; }

bool 
ScopeExchange::has_number (symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  if (i == named.end ())
    return false;
  return (*i).second->has_number (); 
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

bool 
ScopeExchange::has_identifier (symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  if (i == named.end ())
    return false;

  return (*i).second->has_identifier (); 
}

symbol 
ScopeExchange::identifier (symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  return (*i).second->identifier (); 
}

symbol 
ScopeExchange::get_description (symbol tag) const
{ 
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  return (*i).second->get_description ();
}

void 
ScopeExchange::set_number (symbol tag, double value)
{ 
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  daisy_assert (i != named.end ());
  (*i).second->set_number (value); 
}
  
std::vector<symbol>
ScopeExchange::find_all_numbers (const std::vector<Exchange*>& entries)
{
  std::vector<symbol> result;
  for (size_t i = 0; i < entries.size (); i++)
    if (entries[i]->is_number ())
      result.push_back (entries[i]->name ());
  return result;
}

std::map<symbol, Exchange*> 
ScopeExchange::find_named (const std::vector<Exchange*>& entries)
{
  std::map<symbol, Exchange*> result;
  for (size_t i = 0; i < entries.size (); i++)
    result[entries[i]->name ()] = entries[i];
  return result;
}

void 
ScopeExchange::add_item (Exchange* item)
{ entries.push_back (item); }

void 
ScopeExchange::done ()
{
  all_numbers_ = find_all_numbers (entries);
  named = find_named (entries);
}

ScopeExchange::ScopeExchange (Block& al)
  : WScope (al),
    entries (Librarian::build_vector<Exchange> (al, "entries")),
    all_numbers_ (find_all_numbers (entries)),
    named (find_named (entries))
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
                       Syntax::Const, Syntax::Sequence,
                       "List of items to exchange.");
    Librarian::add_type (Scope::component, "exchange", alist, syntax, &make);
  }
} ScopeExchange_syntax;

static Librarian Exchange_init (Exchange::component, "\
A named value to exchange with external models.");

// scope_exchange.C ends here.
