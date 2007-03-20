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

#include "model.h"
#include "symbol.h"
#include <vector>

class Block;
class AttributeList;
class Syntax;
class Treelog;
class Library;

struct BuildBase 
{
  // Avoid calls to daisy_assert
  static void non_null (void*);

  // Types.
  typedef Model& (*builder) (Block&);

  // Content.
  std::auto_ptr<Library> lib;
  int count;

  // Build.
  Model* build_cheat (const AttributeList& parent, 
                      const std::string& key) const;
  Model* build_free (Treelog&, const AttributeList&, 
                     const std::string& scope_id) const;
  Model* build_alist (Block& parent, const AttributeList&, 
                      const std::string& scope_id) const;
  Model* build_item (Block& parent, const std::string& key) const;
  std::vector<Model*> build_vector (Block& al, const std::string& key) const;
  std::vector<const Model*> build_vector_const (Block& al, 
                                               const std::string& key) const;
  // Library.
  void add_base (AttributeList& al, const Syntax& syntax) const;
  void add_type (const symbol name, AttributeList& al,
                 const Syntax& syntax, builder build) const;

  // Create and destroy.
  BuildBase (const char *const name, const char *const description);
  ~BuildBase ();
};

template <class T>
class Librarian
{
  // Class specific builder
  typedef BuildBase::builder builder;

  // Content.
private:
  static BuildBase *content;

  // Functions.
public:
  static T* build_free (Treelog& msg, const AttributeList& alist, 
			const std::string& scope_id)
  {
    BuildBase::non_null (content);
    return dynamic_cast<T*> (content->build_free (msg, alist, scope_id)); 
  }
  static T* build_cheat (const AttributeList& parent, const std::string& key)
  {
    BuildBase::non_null (content);
    return dynamic_cast<T*> (content->build_cheat (parent, key)); 
  }
  static T* build_alist (Block& parent, const AttributeList& alist, 
			 const std::string& scope_id)
  {
    BuildBase::non_null (content);
    return dynamic_cast<T*> (content->build_alist (parent, alist, scope_id)); 
  }
  static T* build_item (Block& parent, const std::string& key)
  { 
    BuildBase::non_null (content);
    return dynamic_cast<T*> (content->build_item (parent, key)); 
  }
  static std::vector<T*> build_vector (Block& al, const std::string& key)
  {  
    BuildBase::non_null (content);
    std::vector<Model*> c = content->build_vector (al, key);
    std::vector<T*> t;
    for (size_t i = 0; i < c.size (); i++)
      t.push_back (dynamic_cast<T*> (c[i]));
    return t;
  }
  static std::vector<const T*> build_vector_const (Block& al,
						   const std::string& key)
  {  
    BuildBase::non_null (content);
    std::vector<const Model*> c = content->build_vector_const (al, key);
    std::vector<const T*> t;
    for (size_t i = 0; i < c.size (); i++)
     t.push_back (dynamic_cast<const T*> (c[i]));
    return t;
  }

  static void add_base (AttributeList& al, const Syntax& syntax)
  { 
    BuildBase::non_null (content);
    content->add_base (al, syntax); 
  }
  static void add_type (const symbol name, AttributeList& al,
			const Syntax& syntax,
			builder build)
  {
    BuildBase::non_null (content);
    content->add_type (name, al, syntax, build);
  }
  static void add_type (const char *const name, AttributeList& al,
			const Syntax& syntax,
			builder build)
  { add_type (symbol (name), al, syntax, build); }

  static Library& library ()
  {
    BuildBase::non_null (content);
    return *(content->lib);
  }

  // Create and Destroy.
public:
  Librarian (const char *const name)
  { 
    if (!content)
      content = new BuildBase (name, T::description);
    content->count++;

  }
  ~Librarian ()
  { 
    BuildBase::non_null (content);
    content->count--;
    if (content->count == 0)
      {
	delete content;
	content = 0;
      }
  }
};

#endif // LIBRARIAN_H
