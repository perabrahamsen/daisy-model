// scope_xysources.C --- A scope based on XY data.
// 
// Copyright 2005 Per Abrahamsen and KVL.
// Copyright 2010 KU.
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

#include "scope_xysources.h"
#include "xysource.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include <sstream>

double
ScopeXYSources::range (const size_t i) const
{
  daisy_assert (i < source.size ());
  if (combine_x)
    {
      daisy_assert (index < source[i]->x ().size ());
      return source[i]->x ().at (index);
    }
  else
    {
      daisy_assert (index < source[i]->y ().size ());
      return source[i]->y ().at (index);
    }
}

double
ScopeXYSources::domain (const size_t i) const
{
  daisy_assert (i < source.size ());
  if (combine_x)
    {
      daisy_assert (index < source[i]->y ().size ());
      return source[i]->y ().at (index);
    }
  else
    {
      daisy_assert (index < source[i]->x ().size ());
      return source[i]->x ().at (index);
    }
}

symbol 
ScopeXYSources::range_dimension () const
{
  daisy_assert (source.size () > 0);
  return combine_x ? source[0]->x_dimension () :  source[0]->y_dimension ();
}

void 
ScopeXYSources::entries (std::set<symbol>& all) const
{ 
  for (size_t i = 0; i < source.size (); i++)
    all.insert (source[i]->title ());
}

Attribute::type 
ScopeXYSources::lookup (const symbol tag) const
{
  for (size_t i = 0; i < source.size (); i++)
    if (source[i]->title () == tag)
      return Attribute::Number;

  return Attribute::Error;
}

bool 
ScopeXYSources::check (const symbol tag) const
{
  for (size_t i = 0; i < source.size (); i++)
    if (source[i]->title () == tag)
      return true;
    
  return false;
}

double 
ScopeXYSources::number (const symbol tag) const
{
  for (size_t i = 0; i < source.size (); i++)
    if (source[i]->title () == tag)
      return domain (i);

  daisy_notreached ();
}

symbol 
ScopeXYSources::dimension (const symbol tag) const
{
  for (size_t i = 0; i < source.size (); i++)
    if (source[i]->title () == tag)
      return combine_x 
        ? source[i]->y_dimension ()
        : source[i]->x_dimension ();
  daisy_notreached ();
}

symbol
ScopeXYSources::description (const symbol) const
{ return "Descriptions not implemented yet"; }

bool 
ScopeXYSources::load (const Units& units, Treelog& msg)
{
  // Propagate.
  bool ok = true;

  for (size_t i = 0; i < source.size (); i++)
    {
      std::ostringstream tmp;
      tmp << "[" << i << "] " << source[i]->title ();
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (units, msg))
        ok = false;
    }

  if (source.size () < 2)
    {
      msg.error ("Need at least two sources");
      ok = false;
    }
  if (source[0]->x ().size () < 1)
    msg.warning ("No data to combine");
  
  if (!ok)
    return false;

  // Try x.
  combine_x = true;
  for (size_t i = 1; combine_x && i < source.size (); i++)
    {
      if (source[i]->x ().size () != source[0]->x ().size ())
        combine_x = false;
      else
        for (index = 0; index < source[0]->x ().size (); index++)
          if (!approximate (range (i), range (0)))
            combine_x = false;
    }
  if (combine_x)
    return true;

  // Try y.
  for (size_t i = 1; !combine_x && i < source.size (); i++)
    {
      if (source[i]->y ().size () != source[0]->y ().size ())
        combine_x = true;
      else
        for (index = 0; index < source[0]->y ().size (); index++)
          if (!approximate (range (i), range (0)))
            combine_x = true;
    }
  if (!combine_x)
    return true;

  msg.error ("No matching axes");
  return false;
}

symbol
ScopeXYSources::with ()
{
  if (source.size () > 0)
    return source[0]->with ();
    
  // No sources => no dates => no data.
  return "lines";
}

void 
ScopeXYSources::limit_range (double& xmin, double& xmax, 
                             double& ymin, double& ymax) const
{
  double dummy_min = NAN;
  double dummy_max = NAN;
  for (size_t i = 0; i < source.size (); i++)
    if (range_is_x ())
      source[i]->limit (xmin, xmax, dummy_min, dummy_max);
    else
      source[i]->limit (dummy_min, dummy_max, ymin, ymax);
}
  
void 
ScopeXYSources::first ()
{ index = 0; }

bool 
ScopeXYSources::done ()
{
  daisy_assert (source.size () > 0);
  const size_t size = combine_x
    ? source[0]->x ().size () 
    : source[0]->y ().size ();
  return index >= size;
}
      
void 
ScopeXYSources::next ()
{ index++; }

double
ScopeXYSources::current () const
{ return range (0); }

ScopeXYSources::ScopeXYSources (const std::vector<XYSource*>& s)
  : source (s),
    index (0),
    combine_x (false)
{ }

ScopeXYSources::~ScopeXYSources ()
{ }

// scope_xysources.C ends here
