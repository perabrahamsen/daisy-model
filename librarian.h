// librarian.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef LIBRARIAN_H
#define LIBRARIAN_H

#include "common.h"		// For BORLAND TEMPLATES and EMPTY_TEMPLATE
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "treelog.h"
#include "assertion.h"
#include <map>
#include <vector>

class Log;

template <class T>
class Librarian
{
  // Types.
private:
  typedef T& (*constructor) (const AttributeList&);
  typedef std::map<symbol, constructor> map_type;

  // Content.
private:
  struct Content;
  friend struct Librarian::Content;
  static struct Content
  {
    Library lib;
    map_type constructors;
    int count;
    Content (const char *const name, derive_fun derive, 
	     const char *const description)
      : lib (name, derive, description),
	constructors (),
	count (0)
    { }
  } *content;

  // Functions.
public:
  static T* create (const AttributeList& al)
  {
    daisy_assert (al.check ("type"));
    const symbol name = al.identifier ("type");
    daisy_assert (library ().check (name));
    daisy_assert (library ().syntax (name).check (al, Treelog::null ()));
    return &(content->constructors)[name] (al);
  }
  static void add_base (AttributeList& al, const Syntax& syntax)
  { library ().add_base (al, syntax); }
  static void add_type (const symbol name, AttributeList& al,
			const Syntax& syntax,
			constructor cons)
  {
    library ().add (name, al, syntax);
    content->constructors.insert(std::make_pair (name, cons));
  }
  static void add_type (const char *const name, AttributeList& al,
			const Syntax& syntax,
			constructor cons)
  { add_type (symbol (name), al, syntax, cons); }
  static void derive_type (symbol name, AttributeList& al, symbol super)
  {
    add_type (name, al, library ().syntax (super),
	      (content->constructors)[super]);
  }
  static Library& library ()
  {
    daisy_assert (content);
    return content->lib;
  }

  // Create and Destroy.
public:
  Librarian (const char *const name)
  { 
    if (!content)
      content = new Content (name, &derive_type, T::description);
    content->count++;

  }
  ~Librarian ()
  { 
    daisy_assert (content);
    content->count--;
    if (content->count == 0)
      {
	delete content;
	content = 0;
      }
    else
      daisy_assert (content->count > 0);
  }
};

// This cutie will create a vector of objects from a vector of alists.
#ifndef BORLAND_TEMPLATES
template <class T> 
std::vector<T*>
map_create (const std::vector<AttributeList*>& f)
{ 
  std::vector<T*> t;
  for (std::vector<AttributeList*>::const_iterator i = f.begin ();
       i != f.end ();
       i++)
    t.push_back (Librarian<T>::create (**i));
  return t;
}

template <class T> 
std::vector<const T*>
map_create_const (const std::vector<AttributeList*>& f)
{ 
  std::vector<const T*> t;
  for (std::vector<AttributeList*>::const_iterator i = f.begin ();
       i != f.end ();
       i++)
    t.push_back (Librarian<T>::create (**i));
  return t;
}

template <class T> 
std::vector<T*>
map_construct (const std::vector<AttributeList*>& f)
{ 
  std::vector<T*> t;
  for (std::vector<AttributeList*>::const_iterator i = f.begin ();
       i != f.end ();
       i++)
    t.push_back (new T (**i));
  return t;
}

template <class T> 
std::vector<const T*>
map_construct_const (const std::vector<AttributeList*>& f)
{ 
  std::vector<const T*> t;
  for (std::vector<AttributeList*>::const_iterator i = f.begin ();
       i != f.end ();
       i++)
    t.push_back (new T (**i));
  return t;
}

#else

template <class T> 
struct map_create
{
  std::vector<T*> t;
  map_create (const std::vector<AttributeList*>& f)
  { 
    for (std::vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (Librarian<T>::create (**i));
  }
  operator std::vector<T*> ()
  { return t; }
};

template <class T> 
struct map_create_const
{
  std::vector<const T*> t;
  map_create_const (const std::vector<AttributeList*>& f)
  { 
    for (std::vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (Librarian<T>::create (**i));
  }
  operator std::vector<const T*> ()
  { return t; }
};

template <class T> 
struct map_construct
{
  std::vector<T*> t;
  map_construct (const std::vector<AttributeList*>& f)
  { 
    for (std::vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i));
  }
  operator std::vector<T*> ()
  { return t; }
};

template <class T> 
struct map_construct_const
{
  std::vector<const T*> t;
  map_construct_const (const std::vector<AttributeList*>& f)
  { 
    for (std::vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i));
  }
  operator std::vector<const T*> ()
  { return t; }
};
#endif

#endif // LIBRARIAN_H
