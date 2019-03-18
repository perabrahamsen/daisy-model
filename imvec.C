// imvec.C -- Keep track of vectors of inorganic matter.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "imvec.h"
#include "assertion.h"
#include "log.h"
#include "chemical.h"
#include "check.h"
#include "block_model.h"
#include "units.h"
#include "frame_submodel.h"
#include <sstream>

const std::vector<double>& 
IMvec::get_array (symbol chem) const
{ 
  const map_type::const_iterator i = content.find (chem);
  if (i != content.end ())
    return (*i).second; 
  static const std::vector<double> empty;
  return empty;
}

std::vector<double>& 
IMvec::get_array (symbol chem)
{ return content[chem]; }

void
IMvec::set_array (symbol chem, const std::vector<double>& array)
{ content[chem] = array; }

double 
IMvec::get_value (symbol chem, size_t index) const
{
  map_type::const_iterator i = content.find (chem);
  if  (i == content.end ())
    return 0.0;
  
  const std::vector<double>& array = (*i).second; 
  if (array.size () > index)
    return array[index];
  
  return 0.0;
}

void 
IMvec::add_value (symbol chem, size_t index, double value)
{
  const map_type::iterator i = content.find (chem);
  if (i == content.end ())
    {
      std::vector<double> array (index + 1, 0.0);
      array[index] = value;
      set_array (chem, array);
    }
  else
    {
      std::vector<double>& array = (*i).second;
      if (array.size () < index + 1)
	array.resize (index + 1);
      array[index] += value;
    }
}

void
IMvec::clear ()
{
  for (map_type::iterator i = content.begin (); 
       i != content.end ();
       i++)
    {
      std::vector<double>& array = (*i).second;
      std::fill (array.begin (), array.end (), 0.0);
    }
}

void
IMvec::multiply (const double factor)
{
  for (map_type::iterator i = content.begin (); 
       i != content.end ();
       i++)
    for (auto& v : (*i).second)
      v *= factor;
}

void
IMvec::output (Log& log) const
{
  for (map_type::const_iterator i = content.begin (); 
       i != content.end ();
       i++)
    {
      const symbol name = (*i).first;
      if (!log.check_entry (name, Chemical::component))
	continue;

      Log::Named named (log, name);
      output_variable (name, log);
      const std::vector<double>& value = (*i).second;
      output_variable (value, log);
    }
}

const std::string 
IMvec::print () const
{
  std::ostringstream tmp;
  tmp << "(imvec";
  for (map_type::const_iterator i = content.begin (); 
       i != content.end ();
       i++)
    {
      const symbol name = (*i).first;
      const std::vector<double>& value = (*i).second;
      tmp << " (" << name;
      for (auto v : value)
	tmp << " " << v;
      tmp << ")";
    }
  tmp << ")";
  return tmp.str ();
}


void 
IMvec::add_syntax (Frame& frame,
                   const Attribute::category cat, 
		   const int size,
                   const symbol dimension)
{
  daisy_assert (size != Attribute::Singleton);
  daisy_assert (size != Attribute::Unspecified);
  frame.declare_string ("name", cat, "Name of chemical.");
  frame.set_check ("name", Chemical::check_buildable ());
  frame.declare ("value", dimension, Check::none/*_negative*/ (), cat,
		 size, "Value for chemical.");
  frame.order ("name", "value");
}

static const Unit&
find_unit (const BlockModel& parent, const char* key)
{
  const Units& units = parent.units ();
  const Frame& parent_frame = parent.find_frame (key);
  const Frame& child_frame = *parent_frame.default_frame (key);
  const symbol dim (child_frame.dimension ("value"));
  return units.get_unit (dim);
}

IMvec::IMvec (const BlockModel& parent, const char* key)
  : unit_ (find_unit (parent, key))
{
  if (!parent.check (key))
    return;

  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& alists
    = parent.submodel_sequence (key);
  for (size_t i = 0; i < alists.size (); i++)
    {
      const Frame& al = *alists[i];
      content[al.name ("name")] = al.number_sequence ("value");
    }
}

IMvec::~IMvec ()
{ }

// IMvec.C ends here.
