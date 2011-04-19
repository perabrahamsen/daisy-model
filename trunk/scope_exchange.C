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
#include "block_model.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"

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

Exchange::Exchange (const symbol n, const symbol d)
  : tag_ (n),
    description (d)
{ }
  
Exchange::~Exchange ()
{ }

static struct ExchangeInit : public DeclareComponent 
{
  ExchangeInit ()
    : DeclareComponent (Exchange::component, "\
A named value to exchange with external models.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare_string ("name", Attribute::Const, "\
Name of value to exchange.");
  }
} Exchange_init;

// Exchanging a number.

Attribute::type 
ExchangeNumber::lookup () const
{ return Attribute::Number; }

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

ExchangeNumber::ExchangeNumber (const BlockModel& al)
  : Exchange (al.name ("name"), al.frame ().description ()),
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

static struct ExchangeNumberSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ExchangeNumber (al); }
  ExchangeNumberSyntax ()
    : DeclareModel (Exchange::component, "number", "Exchange a numeric value.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("dimension", Attribute::Const, "\
Dimension of value to exchange.");
    frame.declare ("value", Attribute::Unknown (), Attribute::OptionalState, "\
Current value to exchange.");
  }
} ExchangeNumber_syntax;

// Exchanging a name (or string).

Attribute::type 
ExchangeName::lookup () const
{ return Attribute::String; }

bool
ExchangeName::check () const
{ return true; }

symbol 
ExchangeName::name () const
{ return value; }

ExchangeName::ExchangeName (const BlockModel& al)
  : Exchange (al.name ("name"), al.frame ().description ()),
    value (al.name ("value"))
{ }

ExchangeName::~ExchangeName ()
{ }

static struct ExchangeNameSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ExchangeName (al); }
  ExchangeNameSyntax ()
    : DeclareModel (Exchange::component, "name", "Exchange a string value.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("value", Attribute::Const, "\
Current value to exchange.");
  }
} ExchangeName_syntax;

// The scope.

void 
ScopeExchange::entries (std::set<symbol>& result) const
{
  for (size_t i = 0; i < all.size (); i++)
    result.insert (all[i]->tag ());
}

Attribute::type 
ScopeExchange::lookup (const symbol tag) const
{
  const std::map<symbol, Exchange*>::const_iterator i = named.find (tag);
  if (i == named.end ())
    return Attribute::Error;
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
ScopeExchange::set (symbol tag, double value)
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

ScopeExchange::ScopeExchange (const symbol title)
  : MScope (title)
{ }

ScopeExchange::ScopeExchange (const BlockModel& al)
  : MScope (al),
    all (Librarian::build_vector<Exchange> (al, "entries")),
    named (find_named (all))
{ }

ScopeExchange::~ScopeExchange ()
{ }

static struct ScopeExchangeSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ScopeExchange (al); }
  ScopeExchangeSyntax ()
    : DeclareModel (MScope::component, "exchange", 
               "Exchange values with an external model.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare_object ("entries", Exchange::component, 
                          Attribute::Const, Attribute::Variable,
                          "List of items to exchange.");
  }
} ScopeExchange_syntax;

// scope_exchange.C ends here.
