// fetch.C --- Fetch data from a log model to a summary model.
// 
// Copyright 2003-2004 Per Abrahamsen and KVL
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

#include "fetch.h"
#include "select.h"
#include "treelog.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
// GCC 2.95 lack ostream.
#include <iostream>

using namespace std;

void 
Fetch::error ()
{ type = Error; }

void 
Fetch::missing ()
{ 
  switch (type)
    {
    case NewContent:
      type = Content;
      initial = last = 0.0;
      break;
    case Content:
      last = 0.0;
      break;
    case Flux:
      break;
    case Error:
      break;
    }
}

void 
Fetch::add (const vector<double>&)
{ type = Error; }

void 
Fetch::add (const double value)
{ 
  switch (type)
    {
    case NewContent:
      type = Content;
      initial = last = value;
      break;
    case Content:
      last = value;
      break;
    case Flux:
      sum += value;
      break;
    case Error:
      break;
    }
}

void 
Fetch::add (const symbol)
{ type = Error; }

double
Fetch::period_factor (const symbol period, const int hours)
{ 
  if (period.name () == "")
    return 1.0;
  if (period.name () == "y")
    return 365.2425 * 24.0 / hours;
  if (period.name () == "m")
    return 30.0 * 24.0 / hours;
  if (period.name () == "w")
    return 7.0 * 24.0 / hours;
  if (period.name () == "d")
    return 24.0 / hours;
  if (period.name () == "h")
    return 1.0 / hours;
  return -42.42e42;
}
    
int 
Fetch::width (const double value)
{
  if (!isnormal (value))
    return 0;

  const int absolute = double2int (floor (log10 (fabs (value)))) + 1;
  if (value < 0)
    return absolute + 1;
  else
    return absolute;
}

const string 
Fetch::dimension (const symbol period) const
{
  if (type != Flux)
    return select_dimension;

  const size_t size = select_dimension.size ();
  if (size > 1)
    {
      const char last = select_dimension[size - 1];
      const char second_last = select_dimension[size - 2];
      if (second_last == '/'
          && (last == 'h' || last == 'd' || last == 'w' 
              || last == 'm' || last == 'y'))
        {
          const string strip = select_dimension.substr (0, size - 2);
          if (period.name () != "")
            return strip + "/" + period;
          else
            return strip;
        }
    }
  if (period.name () == "h")
    return select_dimension;
  else if (period.name () == "")
    return select_dimension + "h";
  else
    return select_dimension + "h/" + period;
}

size_t 
Fetch::name_size () const
{ 
  if (type != Content)
    return name.name ().size ();
  else if (add_delta)
    return name.name ().size () + 6;
  else
    return name.name ().size () + 9;
}

int 
Fetch::value_size (double& total, const symbol period, const int hours) const
{
  double value = 0.0;
  switch (type)
    {
    case Error:
    case NewContent:
      break;
    case Content:
      value = (last - initial) * factor;
      break;
    case Flux:
      value = sum * factor * period_factor (period, hours);
      break;
    }
  total += value;
  return width (value);
}

void 
Fetch::summarize (ostream& out, const int width, 
                  const symbol period, const int hours) const
{
  if (type == Content && add_delta)
    out << "delta ";
  out << name;
  if (type == Content && !add_delta)
    out << " increase";
  out << " = ";
  out.width (width);
  switch (type)
    {
    case NewContent:
      out << "no data";
      break;
    case Content:
      {
        const double value = (last - initial) * factor;
        out << value;
      }
      break;
    case Flux:
      {
        const double value = sum * factor * period_factor (period, hours);
        out << value;
      }
      break;
    case Error:
      out << "bogus data";
      break;
    }
  out << " " << "[" << dimension (period) << "]\n";
}

void
Fetch::clear (const vector<Fetch*>& fetch)
{ 
  for (unsigned int i = 0; i != fetch.size (); i++)
    fetch[i]->clear ();
}

void 
Fetch::clear ()
{ 
  if (type == Content)
    type = NewContent;
  else if (type == Flux)
    sum = 0; 
}

void
Fetch::initialize (const vector<Fetch*>& fetch,
                   vector<Select*>& select, Treelog& msg)
{ 
  for (unsigned int i = 0; i != fetch.size (); i++)
    {
      Treelog::Open nest (msg, fetch[i]->tag);
      bool found = false;
      
      for (unsigned int j = 0; j != select.size (); j++)
	{
	  Treelog::Open nest (msg, select[j]->tag ());
	  
	  if (fetch[i]->tag == select[j]->tag ())
	    if (found)
	      msg.warning ("Duplicate tag ignored");
	    else
	      {	
		select[j]->add_dest (fetch[i]);
		fetch[i]->select_dimension = select[j]->dimension ().name ();
		fetch[i]->type = ((select[j]->handle != Handle::current)
                                  && !select[j]->accumulate)
		  ? Fetch::Flux 
		  : Fetch::NewContent;
		found = true;
	      }
	}
      if (!found)
	msg.warning ("No tag found");
    }
}

void
Fetch::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("description", "A summary file line.");
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
The tag of a column in the log file to summarize in this line.");
  syntax.add ("factor", Syntax::None (), Syntax::Const, "\
Factor to multiply with to get the sum.\n\
Typically 1.0 to add this line, or -1.0 to subtract it.");
  alist.add ("factor", 1.0);
  syntax.add ("name", Syntax::String, Syntax::OptionalConst, "\
Name to use for this line.  By default use the tag.");
  syntax.order ("tag");
}

Fetch::Fetch (const AttributeList& al)
  : tag (al.identifier ("tag")),
    factor (al.number ("factor")),
    name (al.check ("name") ? al.identifier ("name") : tag),
    add_delta (true),
    type (Error),
    initial (-42.42e42),
    last (-42.42e42),
    sum (-42.42e42)
{ }

Fetch::Fetch (const symbol key)
  : tag (key),
    factor (1.0),
    name (tag),
    add_delta (false),
    type (Error),
    initial (-42.42e42),
    last (-42.42e42),
    sum (-42.42e42)
{ }
