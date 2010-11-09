// wsource_const.C -- Weather data that never changes.
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

#include "wsource.h"
#include "weatherdata.h"
#include "time.h"
#include "submodeler.h"
#include "block_model.h"
#include "librarian.h"
#include "mathlib.h"
#include <boost/scoped_ptr.hpp>
#include <map>

struct WSourceConst : public WSource
{
  std::map<symbol, double> numbers;
  std::map<symbol, symbol> names;

  // Scope interface.
  void entries (std::set<symbol>& e) const
  { 
    for (std::map<symbol, double>::const_iterator i = numbers.begin ();
         i != numbers.end ();
         i++)
      e.insert ((*i).first);
    for (std::map<symbol, symbol>::const_iterator i = names.begin ();
         i != names.end ();
         i++)
      e.insert ((*i).first);
  }

  Attribute::type lookup (const symbol key) const
  {
    if (numbers.find (key) != numbers.end ())
      return Attribute::Number;
    if (names.find (key) != names.end ())
      return Attribute::String;
    
    return Attribute::Error;
  }

  symbol dimension (const symbol key) const
  { return Weatherdata::dimension (key); }

  symbol description (const symbol key) const
  { return Weatherdata::description (key); }
  
  bool check (const symbol key) const
  {
    if (numbers.find (key) != numbers.end ())
      return true;
    if (names.find (key) != names.end ())
      return true;

    return false;
  }
  double number (const symbol key) const
  {
    std::map<symbol, double>::const_iterator i = numbers.find (key);
    if (i != numbers.begin ())
      return (*i).second;

    daisy_notreached ();
  }

  // WSource interface.
  symbol station (const symbol) const
  { return Attribute::Unknown (); }
  double screen_height (const symbol) const
  { return NAN; }
  const Time& begin () const
  { static const Time time (1, 1, 1, 0); return time; }
  const Time& end () const
  { static const Time time (9999, 12, 31, 23); return time; }
  void tick () const
  { }

  WSourceConst (const BlockModel& al)
    : WSource ()
  { }
  ~WSourceConst ()
  { }
};

static struct WSourceConstSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceConst (al); }
  WSourceConstSyntax ()
    : DeclareModel (WSource::component, "const",
                    "Weather that does not change during the simulation.")
  { }
  void load_frame (Frame& frame) const
  { Weatherdata::load_syntax (frame); }
} WSourceConst_syntax;

// wsource_const.C ends here.

