// wsource.C -- Selected weather data.
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

#include "wsource_weather.h"
#include "weatherdata.h"
#include "time.h"
#include "librarian.h"
#include "assertion.h"

// The "weather" component.

const char *const WSource::component = "weather";

symbol
WSource::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol 
WSource::title () const
{ return objid; }

WSource::WSource (const symbol name)
  : ModelDerived (name)
{ }

WSource::~WSource ()
{ }

static struct WSourceInit : public DeclareComponent
{
  WSourceInit ()
    : DeclareComponent (WSource::component, "\
A 'wsource' is a source of raw weatherdata.")
  { }
} WSource_init;

// wsource.C ends here.

