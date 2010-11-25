// wsource_base.C -- Weather data that never changes.
// 
// Copyright 2010 KU
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

#include "wsource_base.h"
#include "weatherdata.h"
#include "time.h"
#include "assertion.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "block_model.h"
#include "submodeler.h"
#include <map>

struct WSourceBase::Implementation : public FrameSubmodelValue
{
  boost::scoped_ptr<Time> begin;
  boost::scoped_ptr<Time> end;

  Implementation (const BlockModel& al)
    : FrameSubmodelValue (*Librarian::submodel_frame 
                          /**/ (Weatherdata::load_syntax), 
                          Frame::parent_link),
      begin (al.check ("begin") ? ::submodel<Time> (al, "begin") : NULL),
      end (al.check ("end") ? ::submodel<Time> (al, "end") : NULL)
  { 
    std::set<symbol> all;
    entries (all);
    for (std::set<symbol>::iterator i = all.begin ();
         i != all.end ();
         i++)
      {
        const symbol key = *i;
        if (!al.type_size (key) != Attribute::Singleton)
          continue;

        switch (al.lookup (*i))
          {
          case Attribute::Number:
            set (key, al.number (key));
            break;
          case Attribute::String:
            set (key, al.name (key));
            break;
          default:
            break;
          }
      }
  }
};

void 
WSourceBase::entries (std::set<symbol>& e) const
{ impl->entries (e); }

Attribute::type 
WSourceBase::lookup (const symbol key) const
{ return impl->lookup (key); }

symbol 
WSourceBase::dimension (const symbol key) const
{ return Weatherdata::dimension (key); }

symbol 
WSourceBase::description (const symbol key) const
{ return Weatherdata::description (key); }
  
bool 
WSourceBase::check (const symbol key) const
{ return impl->check (key); }

double 
WSourceBase::number (const symbol key) const
{ return impl->number (key); }

symbol
WSourceBase::name (const symbol key) const
{ return impl->name (key); }

Attribute::type 
WSourceBase::meta_lookup (const symbol, const symbol meta) const
{ return lookup (meta); }

double 
WSourceBase::meta_number (const symbol, const symbol meta) const
{ return number (meta); }

symbol 
WSourceBase::meta_name (const symbol, const symbol meta) const
{ return name (meta); }

const Time& 
WSourceBase::begin () const
{ 
  if (impl->begin.get ())
    return *impl->begin;
  static const Time time (1, 1, 1, 0); 
  return time; 
}

const Time& 
WSourceBase::end () const
{ 
  if (impl->end.get ())
    return *impl->end;
  static const Time time (9999, 12, 31, 23); 
  return time; 
}


WSourceBase::WSourceBase (const BlockModel& al)
  : WSource (),
    impl (new Implementation (al))
{ }

WSourceBase::~WSourceBase ()
{ }

static struct WSourceBaseSyntax : public DeclareBase
{
  WSourceBaseSyntax ()
    : DeclareBase (WSource::component, "base",
                   "Weather that does not change during the simulation.")
  { }
  void load_frame (Frame& frame) const
  {
    Weatherdata::load_syntax (frame); 
    frame.declare_submodule ("begin", Attribute::OptionalConst,
                             "Only use data after this date.", 
                             Time::load_syntax);
    frame.declare_submodule ("end", Attribute::OptionalConst,
                             "Only use data before this date.",
                             Time::load_syntax);
  }
} WSourceBase_syntax;

// wsource_base.C ends here.

