// librarian.h

#ifndef LIBRARIAN_H
#define LIBRARIAN_H

#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "tmpstream.h"
#include <map>

class Log;

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
    Content (const char *const name, derive_fun derive, 
	     const char *const description)
      : lib (name, derive, description),
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
    TmpStream dummy_stream;
    assert (library ().syntax (name).check (al, dummy_stream ()));
    return (content->constructors)[name] (al);
  }
  static void add_type (const string& name, AttributeList& al,
			const Syntax& syntax,
			constructor cons)
  {
    library ().add (name, al, syntax);
    content->constructors.insert(map_type::value_type (name, cons));
  }
  static void derive_type (const string& name, AttributeList& al,
			   const string& super)
  {
    add_type (name, al, library ().syntax (super),
	      (content->constructors)[super]);
  }
  static Library& library ()
  {
    assert (content);
    return content->lib;
  }

  // Create and Destroy.
public:
  Librarian (const char *const name)
  { 
    if (content)
      content->count++;
    else 
      content = new Content (name, &derive_type, T::description);
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
template <class T> 
struct map_create
{
  vector<T*>& t;
  map_create (const vector<AttributeList*>& f)
    : t (*new vector<T*> ())
  { 
    for (vector<AttributeList*>::const_iterator i = f.begin ();
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
  map_create_const (const vector<AttributeList*>& f)
    : t (*new vector<const T*> ())
  { 
    for (vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (&Librarian<T>::create (**i));
  }
  operator vector<const T*>& ()
  { return t; }
};

template <class T> 
struct map_construct
{
  vector<T*>& t;
  map_construct (const vector<AttributeList*>& f)
    : t (*new vector<T*> ())
  { 
    for (vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i));
  }
  operator vector<T*>& ()
  { return t; }
};

template <class T> 
struct map_construct_const
{
  vector<const T*>& t;
  map_construct_const (const vector<AttributeList*>& f)
    : t (*new vector<const T*> ())
  { 
    for (vector<AttributeList*>::const_iterator i = f.begin ();
	 i != f.end ();
	 i++)
      t.push_back (new T (**i));
  }
  operator vector<const T*>& ()
  { return t; }
};

#endif LIBRARIAN_H
