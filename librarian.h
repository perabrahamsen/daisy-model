// librarian.h

#ifndef LIBRARIAN_H
#define LIBRARIAN_H

#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <string>
#include <map>

class Log;
class Filter;

template <class T>
class Librarian
{
  // Types.
private:
  typedef T& (*constructor) (const AttributeList&);
  typedef map<string, constructor, less<string>/**/> map_type;

  // Content.
private:
  static struct Content
  {
    Library lib;
    map_type constructors;
    int count;
    Content (const char *const name)
      : lib (name),
	constructors (),
	count (1)
    { }
  } *content;

  // Functions.
public:
  static T& create (const AttributeList& al)
  {
    assert (al.check ("type"));
    const string name = al.name ("type");
    assert (library ().check (name));
    assert (library ().syntax (name).check (al));
    return (content->constructors)[name] (al);
  }
  static void add_type (const string name, const AttributeList& al,
			const Syntax& syntax,
			constructor cons)
  {
    library ().add (name, al, syntax);
    content->constructors.insert(map_type::value_type (name, cons));
  }
  static void derive_type (const string name, const AttributeList& al,
			   string super)
  {
    add_type (name, al, library ().syntax (super),
	      (content->constructors)[super]);
  }
  static Library& library ()
  {
    assert (content);
    return content->lib;
  }
  static void add_library (Syntax& syntax, const string name)
  {
    syntax.add_library (name, library (), &derive_type);
  }

  // Create and Destroy.
public:
  Librarian (const char *const name)
  { 
    if (content)
      content->count++;
    else 
      content = new Content (name);
  }
  ~Librarian ()
  { 
    if (--content->count == 0)
      {
	delete content;
	content = 0;
      }
  }
};

// This cutie will create a vector of objects from a vector of alists.
// I'd like it to be a member of AttributeList, but template members
// wasn't implemented in G++ 2.7.2.
template <class T> 
struct map_create
{
  vector<T*>& t;
  map_create (const vector<const AttributeList*>& f)
    : t (*new vector<T*> ())
  { 
    for (vector<const AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (&Librarian<T>::create (**i));
  }
  operator vector<T*>& ()
  { return t; }
};

template <class T> 
struct map_create_const
{
  vector<const T*>& t;
  map_create_const (const vector<const AttributeList*>& f)
    : t (*new vector<const T*> ())
  { 
    for (vector<const AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (&Librarian<T>::create (**i));
  }
  operator vector<const T*>& ()
  { return t; }
};

template <class T, class U> 
struct map_create1
{
  vector<T*>& t;
  map_create1 (const vector<const AttributeList*>& f, U u)
    : t (*new vector<T*> ())
  { 
    for (vector<const AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (&T::create (**i, u));
  }
  operator vector<T*>& ()
  { return t; }
};

template <class T> 
struct map_construct
{
  vector<T*>& t;
  map_construct (const vector<const AttributeList*>& f)
    : t (*new vector<T*> ())
  { 
    for (vector<const AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i));
  }
  operator vector<T*>& ()
  { return t; }
};

template <class T, class U> 
struct map_construct1
{
  vector<T*>& t;
  map_construct1 (const vector<const AttributeList*>& f, U u)
    : t (*new vector<T*> ())
  { 
    for (vector<const AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i, u));
  }
  operator vector<T*>& ()
  { return t; }
};

template <class T> 
struct map_construct_const
{
  vector<const T*>& t;
  map_construct_const (const vector<const AttributeList*>& f)
    : t (*new vector<const T*> ())
  { 
    for (vector<const AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i));
  }
  operator vector<const T*>& ()
  { return t; }
};

#endif LIBRARIAN_H
