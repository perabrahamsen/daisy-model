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

#endif LIBRARIAN_H

