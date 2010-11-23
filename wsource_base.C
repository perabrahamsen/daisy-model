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
#include <map>

struct WSourceBase::Implementation
{
  FrameSubmodelValue frame;

  // Scope interface.
  void entries (std::set<symbol>& e) const
  { frame.entries (e); }

  Attribute::type lookup (const symbol key) const
  { return frame.lookup (key); }

  bool check (const symbol key) const
  { return frame.check (key); }

  double number (const symbol key) const
  { return frame.number (key); }

  // WSource interface.
  Implementation (const BlockModel& al)
    : frame (*Librarian::submodel_frame (Weatherdata::load_syntax), 
             Frame::parent_link)
  { 
    std::set<symbol> entries;
    frame.entries (entries);
    for (std::set<symbol>::iterator i = entries.begin ();
         i != entries.end ();
         i++)
      {
        const symbol key = *i;
        if (!al.type_size (key) != Attribute::Singleton)
          continue;

        switch (al.lookup (*i))
          {
          case Attribute::Number:
            frame.set (key, al.number (key));
            break;
          case Attribute::String:
            frame.set (key, al.name (key));
            break;
          default:
            break;
          }
      }
  }
  ~Implementation ()
  { }
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
WSourceBase::station (const symbol) const
{ return Attribute::Unknown (); }

double 
WSourceBase::screen_height (const symbol) const
{ return NAN; }

const Time& 
WSourceBase::begin () const
{ static const Time time (1, 1, 1, 0); return time; }

const Time& 
WSourceBase::end () const
{ static const Time time (9999, 12, 31, 23); return time; }


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
  { Weatherdata::load_syntax (frame); }
} WSourceBase_syntax;

// wsource_base.C ends here.

