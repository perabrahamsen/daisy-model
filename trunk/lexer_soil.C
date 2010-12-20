// lexer_soil.C --- Read tabular soil data from a file.
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

#include "lexer_soil.h"
#include "assertion.h"
#include <sstream>

bool
LexerSoil::read_soil (Treelog& msg)
{
  // Array tags.
  std::vector<double> array_z;
  std::vector<double> array_x;
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
      const std::string rest = name.substr (pos + 3);
      if (rest.size () < 1)
        continue;
      if (rest[0] == '(')
        // 2D
        {
          std::istringstream in (rest);
          in.get ();
          double z;
          double x;
          in >> z >> x;
          if (in.get () != ')')
            continue;
          if (in.fail ())
            continue;
          array_z.push_back (z);
          array_x.push_back (x);
        }
      else
        // 1D
        { 
          std::istringstream in (rest);
          double z;
          in >> z;
          if (in.fail ())
            continue;
          array_z.push_back (z);
        }
      array_c.push_back (i);
    }
  const size_t array_size = array_c.size ();
  daisy_assert (array_z.size () == array_size);
  const bool has_x = array_x.size () > 0;
  if (has_x && array_x.size () != array_size)
    {
      msg.error ("Dimension mixup");
      return false;
    }

  // Find interval ends.
  bool done_z = false;
  double last_z = 0.0;
  double last_x = 0.0;
  for (size_t i = 0; i < array_size; i++)
    {
      const double z = array_z[i];
      if (done_z)
        /* Do nothing. */;
      else if (z > last_z)
        done_z = true;
      else
        {
          last_z += (z - last_z) * 2.0;
          matrix_zplus.push_back (last_z);
        }

      if (!has_x)
        continue;
      
      const double x = array_x[i];
      if (x > last_x)
        {
          last_x += (x - last_x) * 2.0;
          matrix_xplus.push_back (last_x);
        }
    }

  if (has_x 
      ? matrix_zplus.size () * matrix_xplus.size () != array_size
      : matrix_zplus.size () != array_size)
    {
      std::ostringstream tmp;
      tmp << "Array location mismatch: #a = " << array_size
          << "; #z = " << matrix_zplus.size ()
          << "; #x = " << matrix_xplus.size ();
      msg.error (tmp.str ());
      return false;
    }

  return true;
}

bool
LexerSoil::soil_cells (const std::vector<std::string>& entries,
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
LexerSoil::load_syntax (Frame& frame)
{ LexerTable::load_syntax (frame); }

LexerSoil::LexerSoil (const BlockModel& al)
  : LexerTable (al),
    array_name (Attribute::Unknown ()),
    array_dimension (Attribute::Unknown ())
{ }

LexerSoil::~LexerSoil ()
{ }

// lexer_soil.C ends here.
