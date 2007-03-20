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
#include "assertion.h"
#include <map>
#include <vector>

class Log;
class Block;
class AttributeList;
class Syntax;
class Treeelog;
class Library;

struct BuildBase 
{
  // Types.
  typedef void (*derive_fun) (symbol name, const Syntax& syn,
                              AttributeList& al, symbol super);
  typedef Model& (*builder) (Block&);
  typedef std::map<symbol, builder> bmap_type;

  // Content.
  std::auto_ptr<Library> lib;
  int count;
  bmap_type builders;


  // Build.
  virtual Model* build_raw (symbol type, Block&) const = 0;
  Model* build_free (Treelog& msg, const AttributeList& alist, 
                    const std::string& scope_id) const;
  Model* build_cheat (const AttributeList& parent, 
                     const std::string& key) const;
  Model* build_alist (Block& parent, const AttributeList& alist, 
                     const std::string& scope_id) const;
  Model* build_item (Block& parent, const std::string& key) const;
  std::vector<Model*> build_vector (Block& al, const std::string& key) const;
  std::vector<const Model*> build_vector_const (Block& al, 
                                               const std::string& key) const;

  // Library.
  void add_base (AttributeList& al, const Syntax& syntax) const;
  void add_type (const symbol name, AttributeList& al,
                 const Syntax& syntax) const;

  // Create and destroy.
protected:
  BuildBase (const char *const name, derive_fun derive, 
	     const char *const description);
public:
  virtual ~BuildBase ();
};

template <class T>
class Librarian
{
  // Class specific builder
  typedef BuildBase::builder builder;

  // Content.
private:
  static struct Content : BuildBase
  {
    Model* build_raw (const symbol type, Block& block) const
    { 
      daisy_assert (builders.find (type) != builders.end ());
      return &(content->builders)[type] (block);
    }
    Content (const char *const name, BuildBase::derive_fun derive, 
	     const char *const description)
      : BuildBase (name, derive, description)
    { }
  } *content;

  // Functions.
public:
  static T* build_free (Treelog& msg, const AttributeList& alist, 
			const std::string& scope_id)
  {
    daisy_assert (content);
    return dynamic_cast<T*> (content->build_free (msg, alist, scope_id)); 
  }
  static T* build_cheat (const AttributeList& parent, const std::string& key)
  {
    daisy_assert (content);
    return dynamic_cast<T*> (content->build_cheat (parent, key)); 
  }
  static T* build_alist (Block& parent, const AttributeList& alist, 
			 const std::string& scope_id)
  {
    daisy_assert (content);
    return dynamic_cast<T*> (content->build_alist (parent, alist, scope_id)); 
  }
  static T* build_item (Block& parent, const std::string& key)
  { 
    daisy_assert (content);
    return dynamic_cast<T*> (content->build_item (parent, key)); 
  }
  static std::vector<T*> build_vector (Block& al, const std::string& key)
  {  
    daisy_assert (content);
    std::vector<Model*> c = content->build_vector (al, key);
    std::vector<T*> t;
    for (size_t i = 0; i < c.size (); i++)
      t.push_back (dynamic_cast<T*> (c[i]));
    return t;
  }
  static std::vector<const T*> build_vector_const (Block& al,
						   const std::string& key)
  {  
    daisy_assert (content);
    std::vector<const Model*> c = content->build_vector_const (al, key);
    std::vector<const T*> t;
    for (size_t i = 0; i < c.size (); i++)
     t.push_back (dynamic_cast<const T*> (c[i]));
    return t;
  }

  static void add_base (AttributeList& al, const Syntax& syntax)
  { 
    daisy_assert (content);
    content->add_base (al, syntax); 
  }
  static void add_type (const symbol name, AttributeList& al,
			const Syntax& syntax,
			builder build)
  {
    daisy_assert (content);
    content->add_type (name, al, syntax);
    content->builders.insert (std::make_pair (name, build));
  }
  static void add_type (const char *const name, AttributeList& al,
			const Syntax& syntax,
			builder build)
  { add_type (symbol (name), al, syntax, build); }
  static void derive_type (symbol name, const Syntax& syn, 
			   AttributeList& al, symbol super)
  { add_type (name, al, syn, (content->builders)[super]); }
  static Library& library ()
  {
    daisy_assert (content);
    return *(content->lib);
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

#endif // LIBRARIAN_H
