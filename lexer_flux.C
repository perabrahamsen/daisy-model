// lexer_flux.C --- Read tabular soil flux data from a file.
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

#include "lexer_flux.h"
#include "geometry.h"
#include "mathlib.h"
#include "assertion.h"
#include <sstream>

int
LexerFlux::read_cell (std::istream& in)
{
  // Internal cell.
  if (in.peek () == '(')
    {
      in.get ();
      double z;
      double x;
      in >> z >> x;
      if (in.get () != ')')
        return Geometry::cell_error;
      if (in.fail ())
        return Geometry::cell_error;
      
      daisy_assert (center_z.size () == center_x.size ());
      int c = 0;
      for (; c < center_z.size (); c++)
        if (approximate (center_z[c], z) && approximate (center_x[c], x))
          return c;

      center_z.push_back (z);
      center_x.push_back (x);
      daisy_assert (center_x.size () == c + 1);
      return c;
    }

  // External cell.
  std::string name;
  while (isalpha (in.peek ()))
    name += in.get ();

  if (in.fail ())
    return Geometry::cell_error;

  if (name == "top")
    return Geometry::cell_above;
  if (name == "bottom")
    return Geometry::cell_below;
  if (name == "left")
    return Geometry::cell_left;
  if (name == "right")
    return Geometry::cell_right;
  if (name == "front")
    return Geometry::cell_front;
  if (name == "back")
    return Geometry::cell_back;
  
  return Geometry::cell_error;
}

bool
LexerFlux::read_flux (Treelog& msg)
{
  for (size_t i = 0; i < tag_names ().size (); i++)
    {
      std::string name = tag_names ()[i].name ();
      const size_t pos = name.find (" @ ");
      if (pos == std::string::npos)
        continue;
      const std::string tag = name.substr (0, pos);

      if (array_name == Attribute::Unknown ())
        array_name = tag;
      else if (array_name != tag)
        continue;

      if (array_dimension == Attribute::Unknown ())
        array_dimension = dimension (i);
      else if (array_dimension != dimension(i))
        continue;
      
      daisy_assert (name.size () >= pos + 3);
      std::istringstream rest (name.substr (pos + 3));
      if (rest.peek () == '(')
        // 2D
        {
          daisy_assert (rest.get () == '(');
          int from = read_cell (rest);
          if (rest.get () != ' ')
            continue;
          int to = read_cell (rest);
          
          if (rest.get () != ')')
            continue;
          if (rest.fail ())
            continue;
          
          flux_from.push_back (from);
          flux_to.push_back (to);
        }
      else
        // 1D
        { 
          double z;
          rest >> z;

          if (rest.fail ())
            continue;
          
          flux_z.push_back (z);
        }
      array_c.push_back (i);
    }
  const size_t array_size = array_c.size ();
  daisy_assert (array_size == flux_z.size () + flux_from.size ());
  daisy_assert (flux_from.size () == flux_to.size ());
  if ((flux_z.size () == 0 && flux_from.size () != array_size)
      || (flux_from.size () == 0 && flux_z.size () != array_size))
    {
      msg.error ("Dimension mixup");
      return false;
    }
  
  return true;
}

bool
LexerFlux::flux_edges (const std::vector<std::string>& entries,
                       std::vector<double>& values,
                       Treelog& msg) const
{
  const size_t size = array_c.size ();
  if (values.size () < size)
    values.insert (values.end (), size - values.size (), 0.0);
  if (values.size () != size)
    {
      std::ostringstream tmp;
      tmp << "values:" << values.size () << ", columns:" << size;
      daisy_bug (tmp.str ());
      return false;
    }
  for (size_t i = 0; i < size; i++)
    {
      const size_t c = array_c[i];
      daisy_assert (c < entries.size ());
      const char *const str = entries[c].c_str ();
      const char* end_ptr = str;
      const double value = strtod (str, const_cast<char**> (&end_ptr));
      values[i] = value;
      if (*end_ptr != '\0')
        {
          msg.error (std::string ("Junk at end of number '") + end_ptr + "'");
          return false;
        }
    }
  return true;
}

void 
LexerFlux::load_syntax (Frame& frame)
{ LexerTable::load_syntax (frame); }

LexerFlux::LexerFlux (const BlockModel& al)
  : LexerTable (al),
    array_name (Attribute::Unknown ()),
    array_dimension (Attribute::Unknown ())
{ }

LexerFlux::~LexerFlux ()
{ }

// lexer_flux.C ends here.
