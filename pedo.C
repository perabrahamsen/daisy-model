// pedo.C --- Pedotransfer functions based on soil attributes.
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


#include "pedo.h"
#include "soil.h"
#include "units.h"
#include "tmpstream.h"
#include "treelog.h"

using namespace std;

EMPTY_TEMPLATE
Librarian<Pedotransfer>::Content* Librarian<Pedotransfer>::content = NULL;

const char *const Pedotransfer::description = "\
Pedotransfer functions based on soil attributes.";

void 
Pedotransfer::set (const Soil& soil, vector<double>& array,
                   const string& dim) const
{
  array.insert (array.end (), soil.size () - array.size (), 0.0);

  for (unsigned int i = 0; i < soil.size (); i++)
    if (known (dim) && known (dimension ()))
      array[i] = Units::convert (dimension (), dim, value (soil, i));
}


bool
Pedotransfer::check (const Soil& soil, const string& dim, Treelog& msg) const
{ 
  Treelog::Open nest (msg, name);
  bool ok = true;

  if (known (dim) && known (dimension ())
      && !Units::can_convert (dimension (), dim))
    {
      msg.error ("Cannot convert [" + dimension () + "] to [" + dim + "]");
      ok = false;
    }
  
  if (!check_nested (soil, msg))
    ok = false;

  return ok; 
}

bool 
Pedotransfer::known (const std::string& dim)
{ return dim != Syntax::Unknown () && (dim.size () < 1 || dim[0] != '?'); }

void 
Pedotransfer::debug_message (const string& name,
                             const vector<double>& value, 
                             Treelog& msg)
{ 
  TmpStream tmp;
  tmp () << "(" << name;
  for (unsigned int i = 0; i < value.size (); i++)
    tmp () << " " << value[i];
  tmp () << ")";
  msg.debug (tmp.str ());
}

Pedotransfer::Pedotransfer (const AttributeList& al)
  : name (al.identifier ("type"))
{ }

Pedotransfer::~Pedotransfer ()
{ }
