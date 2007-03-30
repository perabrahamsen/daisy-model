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
class Metalib;
class Format;

class BuildBase 
{
  // Avoid calls to daisy_assert
protected:
  static void non_null (const void*);

  // Types.
protected:
  typedef Model& (*builder) (Block&);

  // Content.
protected:
  class Content;
  static Content* content;

  // Build.
protected:
  static Model* build_free (const char* component,
                            Metalib&, Treelog&, const AttributeList&, 
                            const std::string& scope_id);
  static Model* build_alist (const char* component,
                             Block& parent, const AttributeList&, 
                             const std::string& scope_id);
  static Model* build_item (const char* component,
                            Block& parent, const std::string& key);
  static std::vector<Model*> build_vector (const char* component,
                                           Block& al, 
                                           const std::string& key);
  static std::vector<const Model*>
  /**/ build_vector_const (const char* component,
                           Block& al, const std::string& key);

  // Library.
protected:
  static void add_base (const char* component,
                        AttributeList& al, const Syntax& syntax);
  static void add_type (const char* component,
                        const symbol name, AttributeList& al,
                        const Syntax& syntax, builder build);
  static void add_type (const char* component,
                        const char* name, AttributeList& al,
                        const Syntax& syntax, builder build);
  static Library& library (const char* component);
                      
private:
  typedef void (*doc_fun) (Format&, Metalib&, Treelog&, 
                           const AttributeList& al);
public:
  static void add_alias (const char* component, symbol derived, symbol base);
  static void add_doc_fun (const char* component, doc_fun);

  // Create and destroy.
protected:
  BuildBase (const char *const component, const char *const description);
  ~BuildBase ();
};

template <class T>
class Librarian : private BuildBase
{
  // Functions.
public:
  static T* build_free (Metalib& metalib, Treelog& msg,
                        const AttributeList& alist, 
			const std::string& scope_id)
  { 
    T* x = dynamic_cast<T*> (BuildBase::build_free (T::component, metalib, msg,
                                                    alist, scope_id)); 
    non_null (x);
    return x;
  }
  static T* build_alist (Block& parent, const AttributeList& alist, 
			 const std::string& scope_id)
  { 
    T* x = dynamic_cast<T*> (BuildBase::build_alist (T::component, 
                                                     parent, alist, scope_id));
    non_null (x);
    return x;
  }
  static T* build_item (Block& parent, const std::string& key)
  { 
    T* x = dynamic_cast<T*> (BuildBase::build_item (T::component, 
                                                    parent, key)); 
    non_null (x);
    return x;
  }
  static std::vector<T*> build_vector (Block& al, const std::string& key)
  {  
    const std::vector<Model*> c 
      = BuildBase::build_vector (T::component, al, key);
    std::vector<T*> t;
    for (size_t i = 0; i < c.size (); i++)
      {
        T* x = dynamic_cast<T*> (c[i]);
        non_null (x);
        t.push_back (x);
      }
    return t;
  }
  static std::vector<const T*> build_vector_const (Block& al,
						   const std::string& key)
  {  
    const std::vector<const Model*> c
      = BuildBase::build_vector_const (T::component, al, key);
    std::vector<const T*> t;
    for (size_t i = 0; i < c.size (); i++)
      {
        const T* x = dynamic_cast<const T*> (c[i]);
        non_null (x);
        t.push_back (x);
      }
    return t;
  }
  static void add_base (AttributeList& al, const Syntax& syntax)
  { BuildBase::add_base (T::component, al, syntax); }
  static void add_type (const symbol name, AttributeList& al,
			const Syntax& syntax,
			builder build)
  { BuildBase::add_type (T::component, name, al, syntax, build); }
  static void add_type (const char *const name, AttributeList& al,
			const Syntax& syntax,
			builder build)
  { BuildBase::add_type (T::component, name, al, syntax, build); }

  // Create and Destroy.
private:                        // Disable.
  Librarian (const Librarian&);
  Librarian& operator= (const Librarian&);
public:
  explicit Librarian ()
    : BuildBase (T::component, T::description)
  { }
  ~Librarian ()
  { }
};

#endif // LIBRARIAN_H
