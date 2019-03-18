// wsource_indirect.C -- Base class for wsources based on other wsources.
// 
// Copyright 2011 KU
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
#include "wsource_indirect.h"
#include "librarian.h"
#include "block_model.h"

void
WSourceIndirect::entries (std::set<symbol>& e) const
{ return source->entries (e); }

Attribute::type
WSourceIndirect::lookup (const symbol key) const
{ return source->lookup (key); }

symbol
WSourceIndirect::dimension (const symbol key) const
{ return source->dimension (key); }

symbol
WSourceIndirect::description (const symbol key) const
{ return source->description (key); }

bool
WSourceIndirect::check (const symbol key) const
{ return source->check (key); }

double
WSourceIndirect::number (const symbol key) const
{ return source->number (key); }

symbol
WSourceIndirect::name (const symbol key) const
{ return static_cast<const Scope&>(*source).name (key); }

int 
WSourceIndirect::type_size (symbol key) const
{ return source->type_size (key); }

int 
WSourceIndirect::value_size (symbol key) const
{ return source->value_size (key); }

bool
WSourceIndirect::end_check (symbol key) const
{ return source->end_check (key); }

double
WSourceIndirect::end_number (symbol key) const
{ return source->end_number (key); }

symbol
WSourceIndirect::end_name (symbol key) const
{ return source->end_name (key); }

const std::vector<double>& 
WSourceIndirect::number_sequence (const symbol key) const
{ return source->number_sequence (key); }

const std::vector<double>& 
WSourceIndirect::end_number_sequence (const symbol key) const
{ return source->number_sequence (key); }

double
WSourceIndirect::meta_timestep (const symbol key) const
{ return source->meta_timestep (key); }

bool
WSourceIndirect::meta_check (symbol key, symbol meta) const
{ return source->meta_check (key, meta); }

double
WSourceIndirect::meta_number (symbol key, symbol meta) const
{ return source->meta_number (key, meta); }

symbol
WSourceIndirect::meta_name (symbol key, symbol meta) const
{ return source->meta_name (key, meta); }

bool
WSourceIndirect::meta_end_check (symbol key, symbol meta) const
{ return source->meta_end_check (key, meta); }

double
WSourceIndirect::meta_end_number (symbol key, symbol meta) const
{ return source->meta_end_number (key, meta); }

symbol
WSourceIndirect::meta_end_name (symbol key, symbol meta) const
{ return source->meta_end_name (key, meta); }

const Time& 
WSourceIndirect::begin () const
{ return source->begin (); }

const Time&
WSourceIndirect::end () const
{ return source->end (); }

const Time& 
WSourceIndirect::data_begin () const
{ return source->data_begin (); }

const Time&
WSourceIndirect::data_end () const
{ return source->data_end (); }

double
WSourceIndirect::timestep () const
{ return source->timestep (); }

void
WSourceIndirect::source_tick (Treelog& msg)
{ source->source_tick (msg); }

bool
WSourceIndirect::done () const
{ return source->done (); }

void
WSourceIndirect::source_initialize (Treelog& msg)
{ source->source_initialize (msg); }

void
WSourceIndirect::skip_ahead (const Time& begin, Treelog& msg)
{ source->skip_ahead (begin, msg); }

bool
WSourceIndirect::source_check (Treelog& msg) const
{ return source->source_check (msg); }


WSourceIndirect::WSourceIndirect (const BlockModel& al)
  : WSourceWeather (al),
    source (Librarian::build_item<WSource> (al, "source"))
{ }

WSourceIndirect::~WSourceIndirect ()
{ }

static struct WSourceIndirectSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceIndirect (al); }
  WSourceIndirectSyntax ()
    : DeclareModel (WSource::component, "indirect", "weather",
                    "Delegate to another weather source.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("source", WSource::component,
                          Attribute::State, Attribute::Singleton, "\
Use this weather source.");
  }
} WSourceIndirect_syntax;

// wsource_indirect.C ends here.

