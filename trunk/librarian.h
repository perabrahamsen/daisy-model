// librarian.h -- Manage components and models.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen.
// Copyright 2000-2001 KVL.
// Copyright 2006-2007 Per Abrahamsen and KVL.
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
#include <boost/shared_ptr.hpp>

class Block;
class BlockModel;
class Treelog;
class Library;
class Metalib;
class Format;
class FrameModel;
class FrameSubmodel;
class Intrinsics;
class Declare;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT Librarian 
{
  // Content.
private:
  static Intrinsics* content;
public:
  static const Intrinsics& intrinsics ();
  typedef void (*load_syntax_t) (Frame&);
  static void submodel_instantiate (load_syntax_t);
  static bool submodel_registered (symbol);
  static boost::shared_ptr<const FrameSubmodel> submodel_frame (symbol);
  static boost::shared_ptr<const FrameSubmodel> submodel_frame (load_syntax_t);
  static symbol submodel_name (load_syntax_t);
  static load_syntax_t submodel_load (symbol);
  static symbol submodel_description (symbol);
  static void declare_submodel (load_syntax_t, symbol name, symbol desc);
  static void submodel_all (std::vector<symbol>&);

  // Build.
private:
  typedef Model& (*builder) (const BlockModel&);
  static void non_null (const Block&, const void*);
  static Model* build_frame (symbol component,
                             const Block&, const FrameModel&, 
                             symbol scope_id);
  static Model* build_frame (symbol component,
                             const Block&, const FrameModel&, 
                             symbol scope_id, size_t index);
  static Model* build_frame (symbol component,
                             const Metalib&, Treelog&, const FrameModel&, 
                             symbol scope_id);
  static Model* build_stock (symbol component,
                             const Metalib&, Treelog&, symbol name, symbol scope_id);
  static Model* build_item (symbol component,
                            const Block& parent, symbol key);
  static std::vector<Model*> build_vector (symbol component,
                                           const Block& al, 
                                           symbol key);
  static std::vector<const Model*>
  /**/ build_vector_const (symbol component,
                           const Block& al, symbol key);

public:
  template <class T> static T* 
  build_frame (const Block& block, const FrameModel& frame, symbol scope_id)
  { 
    T* x = dynamic_cast<T*> (Librarian::build_frame (T::component, block,
                                                     frame, scope_id)); 
    non_null (block, x);
    return x;
}

  template <class T> static T* 
  build_frame (const Block& block, const FrameModel& frame, const symbol scope_id, 
               const size_t index)
  { 
    T* x = dynamic_cast<T*> (Librarian::build_frame (T::component, block,
                                                     frame, scope_id, index)); 
    non_null (block, x);
    return x;
  }

  template <class T> static T* 
  build_frame (const Metalib& metalib, Treelog& msg, const FrameModel& frame,
               symbol scope_id)
  { return dynamic_cast<T*> (Librarian::build_frame (T::component, metalib, msg,
                                                     frame, scope_id)); }

  template <class T> static T* 
  build_stock (const Metalib& metalib, Treelog& msg, symbol name, symbol scope_id)
  { return dynamic_cast<T*> (Librarian::build_stock (T::component, metalib, msg,
                                                     name, scope_id)); }

  template <class T> static T* 
  build_item (const Block& parent, symbol key)
  { 
    T* x = dynamic_cast<T*> (Librarian::build_item (T::component, 
                                                    parent, key)); 
    non_null (parent, x);
    return x;
  }

  template <class T> static std::vector<T*> 
  build_vector (const Block& al, symbol key)
  {  
    const std::vector<Model*> c 
      = Librarian::build_vector (T::component, al, key);
    std::vector<T*> t;
    for (size_t i = 0; i < c.size (); i++)
      {
        T* x = dynamic_cast<T*> (c[i]);
        non_null (al, x);
        t.push_back (x);
      }
    return t;
  }
  template <class T> static std::vector<const T*> 
  build_vector_const (const Block& al, symbol key)
  {  
    const std::vector<const Model*> c
      = Librarian::build_vector_const (T::component, al, key);
    std::vector<const T*> t;
    for (size_t i = 0; i < c.size (); i++)
      {
        const T* x = dynamic_cast<const T*> (c[i]);
        non_null (al, x);
        t.push_back (x);
      }
    return t;
  }

  // Library.
private:
  static Library& library (symbol component);
public:
  typedef void (*doc_fun) (Format&, const Metalib&, Treelog&, symbol name);
  static void add_doc_fun (symbol component, doc_fun);

  // Declarations.
  static void declare (symbol component, symbol name, 
                       const Declare& declaration);

  // Create and destroy.
public:
  Librarian (symbol component, symbol description);
  ~Librarian ();
};

class EXPORT Declare : private boost::noncopyable
{
public:
  const symbol component;
  const symbol name;
  const symbol description_;
  struct Builder
  { 
    virtual Model* make (const BlockModel&) const = 0; 
    virtual ~Builder ();
  };

public:
  virtual bool used_to_be_a_submodel () const;
  virtual boost::shared_ptr<const FrameModel> create_frame () const;
  virtual void load (Frame&) const = 0;
protected:
  virtual void load_frame (Frame&) const = 0;
public:
  static symbol root_name ();

  virtual const FrameModel* parent_model () const = 0;

protected:
  Declare (symbol component, symbol name, symbol description);
  virtual ~Declare ();
};

class DeclareComponent : public Declare
{
  Librarian librarian;
  void load (Frame&) const;
  void load_frame (Frame&) const;
  const FrameModel* parent_model () const;
public:
  DeclareComponent (symbol component, symbol description);
};

class DeclareSolo : public DeclareComponent, public Declare::Builder
{
  boost::shared_ptr<const FrameModel> create_frame () const;
public:
  DeclareSolo (symbol component, symbol description);
};
  
class EXPORT DeclareSuper : public Declare
{
protected:
  const symbol super;
  const FrameModel* parent_model () const;
  DeclareSuper (symbol component, symbol name, symbol super, symbol description);
};

class DeclareBase : public DeclareSuper
{
  void load (Frame&) const;
public:
  DeclareBase (symbol component, symbol name, symbol super, symbol description);
  DeclareBase (symbol component, symbol name, symbol description);
};

class EXPORT DeclareModel : public DeclareSuper, public Declare::Builder
{
  boost::shared_ptr<const FrameModel> create_frame () const;
  void load (Frame&) const;
protected:
  DeclareModel (symbol component, symbol name, symbol super, 
                symbol description);
  DeclareModel (symbol component, symbol name, symbol description);
  ~DeclareModel ();
};

class DeclareParam : public DeclareSuper
{
  void load (Frame&) const;
public:
  DeclareParam (symbol component, symbol name, symbol super, 
                symbol description);
};

class DeclareAlias : public DeclareParam
{
  void load_frame (Frame&) const;
public:
  DeclareAlias (symbol component, symbol name, symbol super);
};

struct DeclareSubmodel : private boost::noncopyable
{
  DeclareSubmodel (Librarian::load_syntax_t, symbol name, symbol description);
  ~DeclareSubmodel ();
};

#endif // LIBRARIAN_H
