// alist.h -- attribute list

#ifndef ALIST_H
#define ALIST_H

#include "common.h"

#include <std/stdexcept.h>
#include <string>
#include <vector>
#include <list>

struct Time;
struct CSMP;
struct Filter;
struct AttributeList;

class AttributeList
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  static const AttributeList empty;

  // Exceptions.
#ifdef HANDLE_EXCEPTIONS
  struct Invalid : runtime_error
  { 
    // Tried to extract the wrong type from a Value object.
    const char* what () const;
  };

  struct Uninitialized : runtime_error
  { 
    // Tried to lookup an uninitialized value in an AttributeList.
    const char* what () const;
  };
#endif HANDLE_EXCEPTIONS

  // Use.
  bool check (string) const 
       throw0 ();
  double number (string) const
       throw2 (Invalid, Uninitialized);
  string name (string) const
       throw2 (Invalid, Uninitialized);
  bool flag (string) const
       throw2 (Invalid, Uninitialized);
  const CSMP& csmp (string) const 
       throw2 (Invalid, Uninitialized);
  const Filter& filter (string) const 
       throw2 (Invalid, Uninitialized);
  const AttributeList& list (string) const
       throw2 (Invalid, Uninitialized);
  int integer (string) const
       throw2 (Invalid, Uninitialized);
  const Time& time (string) const
       throw2 (Invalid, Uninitialized);
  const vector<double>& number_sequence (string) const
       throw2 (Invalid, Uninitialized);
  const vector<string>& name_sequence (string key) const
       throw2 (Invalid, Uninitialized);
  const vector<bool>& flag_sequence (string key) const
       throw2 (Invalid, Uninitialized);
  const vector<int>& integer_sequence (string key) const
       throw2 (Invalid, Uninitialized);
  const vector<const Time*>& time_sequence (string key) const
       throw2 (Invalid, Uninitialized);
  const vector<const CSMP*>& csmp_sequence (string key) const
       throw2 (Invalid, Uninitialized);
  const vector<const Filter*>& filter_sequence (string key) const
       throw2 (Invalid, Uninitialized);
  const vector<const AttributeList*>& list_sequence (string key) const
       throw2 (Invalid, Uninitialized);

  // Create and Destroy.
  void add (string, double);
  void add (string, string);
  void add (string, bool);
  void add (string, int);
  void add (string, const AttributeList&);
  void add (string, const CSMP&);
  void add (string, const Filter&);
  void add (string, const Time&);
  void add (string, const vector<double>&);
  void add (string, const vector<string>&);
  void add (string, const vector<bool>&);
  void add (string, const vector<int>&);
  void add (string, const vector<const AttributeList*>&);
  void add (string, const vector<const CSMP*>&);
  void add (string, const vector<const Filter*>&);
  void add (string, const vector<const Time*>&);

  void operator += (const AttributeList&);
  AttributeList (const AttributeList& old);
  AttributeList ();
  ~AttributeList ();
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
      t.push_back (&T::create (**i));
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

#endif ALIST_H

// alist.h ends here
