// submodeler.h  --- Utilities for handling submodels.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#ifndef SUBMODELER_H
#define SUBMODELER_H

#include "block.h"
#include "assertion.h"
#include <vector>

// Old style (no block scope).
template <class T> 
std::vector<T*>
map_construct (const std::vector<const AttributeList*>& f)
{ 
  std::vector<T*> t;
  for (std::vector<const AttributeList*>::const_iterator i = f.begin ();
       i != f.end ();
       i++)
    t.push_back (new T (**i));
  return t;
}

template <class T> 
std::vector<const T*>
map_construct_const (const std::vector<const AttributeList*>& f)
{ 
  std::vector<const T*> t;
  for (std::vector<const AttributeList*>::const_iterator i = f.begin ();
       i != f.end ();
       i++)
    t.push_back (new T (**i));
  return t;
}

// New style (block scope).

template <class T> 
T
submodel_value (Block& parent, const std::string& key)
{ 
  Block nested (parent, key);
  try
    { return T (nested); }
  catch (const std::string& err)
    { nested.error ("Submodel value build failed: " + err); }
  catch (const char *const err)
    { nested.error ("Submodel value build failed: " + std::string (err)); }
  return T::null ();
}

template <class T> 
T*
submodel_block (Block& nested)
{ 
  try
    { return new T (nested); }
  catch (const std::string& err)
    { nested.error ("Submodel build failed: " + err); }
  catch (const char *const err)
    { nested.error ("Submodel build failed: " + std::string (err)); }
  return NULL;
}
template <class T> 
T*
submodel (Block& parent, const std::string& key)
{ 
  Block nested (parent, key);
  return submodel_block<T> (nested);
}

// Sequences
template <class T> 
std::vector<T*>
map_submodel (Block& parent, const std::string& key)
{ 
  std::vector<T*> t;
  const std::vector<const AttributeList*> f (parent.alist_sequence (key));
  const Syntax& syntax = parent.syntax ().syntax (key);
  for (size_t i = 0; i < f.size (); i++)
    {
      const AttributeList& alist = *f[i];
      daisy_assert (syntax.check (parent.metalib (), 
                                  alist, Treelog::null ()));      
      Block nested (parent, syntax, alist, key, i);
      t.push_back (submodel_block<T> (nested));
    }
  return t;
}

template <class T> 
std::vector<const T*>
map_submodel_const (Block& parent, const std::string& key)
{ 
  std::vector<const T*> t;
  const std::vector<const AttributeList*> f (parent.alist_sequence (key));
  const Syntax& syntax = parent.syntax ().syntax (key);
  for (size_t i = 0; i < f.size (); i++)
    {
      const AttributeList& alist = *f[i];
      daisy_assert (syntax.check (parent.metalib (), 
                                  alist, Treelog::null ()));      
      Block nested (parent, syntax, alist, key, i);
      t.push_back (submodel_block<T> (nested));
    }
  return t;
}

#endif // SUBMODELER_H
