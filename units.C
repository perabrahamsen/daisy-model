// units.C -- conversion between different dimensions.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "units.h"
#include "mathlib.h"
#include <map>

struct Units::Content
{
  typedef map<string, Convert*> to_type;
  typedef map<string, to_type> table_type;
  table_type table;
  
  double convert (const string& from, const string& to, double value) const;
  bool can_convert (const string& from, const string& to) const;
  bool can_convert (const string& from, const string& to, double value) const;

  Content ();
  ~Content ();
};  

double 
Units::Content::convert (const string& from, const string& to, 
			 double value) const
{ 
  if (from == to)
    return value;

  table_type::const_iterator i = table.find (from);
  if (i == table.end ())
    throw string ("'") + from + "' unknown dimension, expected '" + to + "'";
  to_type::const_iterator j = (*i).second.find (to);
  if (j == (*i).second.end ())
    throw string ("Cannot convert '") + from + "' to '" + to + "'";
  if (!(*j).second->valid (value))
    throw string ("invalid value");

  return (*j).second->operator() (value);
}

bool 
Units::Content::can_convert (const string& from, const string& to) const
{ 
  if (from == to)
    return true;

  table_type::const_iterator i = table.find (from);
  if (i == table.end ())
    return false;
  to_type::const_iterator j = (*i).second.find (to);
  if (j == (*i).second.end ())
    return false;

  return true;
}

bool 
Units::Content::can_convert (const string& from, const string& to, 
			     double value) const
{ 
  if (from == to)
    return true;

  table_type::const_iterator i = table.find (from);
  if (i == table.end ())
    return false;
  to_type::const_iterator j = (*i).second.find (to);
  if (j == (*i).second.end ())
    return false;
  if (!(*j).second->valid (value))
    return false;

  return true;
}

Units::Content::Content ()
{ }
  
Units::Content::~Content ()
{
  for (table_type::iterator i = table.begin ();
       i != table.end ();
       i++)
    for (to_type::iterator j = (*i).second.begin ();
	 j != (*i).second.end ();
	 j++)
      {
	assert ((*j).second);
	delete (*j).second;
	(*j).second = NULL;
      }
}

Units::Content* Units::content = NULL;
int Units::count = 0;

bool
Units::Convert::valid (double)
{ return true; }

Units::Convert::Convert ()
{ }

Units::Convert::~Convert ()
{ }

struct ConvertLinear : public Units::Convert
{
  const double factor;
  const double offset;
    
  double operator() (double value)
  { return value * factor + offset; }

  ConvertLinear (double f, double o)
    : factor (f),
      offset (o)
  { }
};

void 
Units::add (const string& from, const string& to, double factor, double offset)
{ 
  assert (content);
  content->table[from][to] = new ConvertLinear (factor, offset);
}

void 
Units::add (const string& from, const string& to, Convert& convert)
{
  assert (content);
  assert (content->table[from].find (to) == content->table[from].end ());
  content->table[from][to] = &convert;
  
}

double 
Units::convert (const string& from, const string& to, double value)
{ 
  assert (content);
  return content->convert (from, to, value);
}

bool
Units::can_convert (const string& from, const string& to)
{ 
  assert (content);
  return content->can_convert (from, to);
}

bool
Units::can_convert (const string& from, const string& to, double value)
{ 
  assert (content);
  return content->can_convert (from, to, value);
}


class Convert_pF_cm : public Units::Convert
{
  double operator() (double value)
  { return pF2h (value); }
};

class Convert_cm_pF : public Units::Convert
{
  double operator() (double value)
  { return h2pF (value); }
};

void
Units::standard_conversions ()
{
  add ("m", "cm", 100.0);
  add ("pF", "cm", *new Convert_pF_cm ());
  add ("cm", "pF", *new Convert_cm_pF ());
}

Units::Units ()
{ 
  if (content)
    {
      assert (count > 0);
      count++;
    }
  else
    {
      assert (count == 0);
      count = 1;
      content = new Content;
      standard_conversions ();
    }
}

Units::~Units ()
{
  assert (content);
  assert (count > 0);
  count--;
  if (count < 1)
    {
      delete content;
      content = NULL;
    }
}
