// alist.h -- attribute list

#ifndef ALIST_H
#define ALIST_H

#include "common.h"

#include <stdexcept>
#include <string>
#include <vector>
#include <list>

struct Time;
struct CSMP;
struct AttributeList;
struct Syntax;

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
    Invalid () 
      : runtime_error ("Tried to extract the wrong type from a Value object")
    { }
  };

  struct Uninitialized : runtime_error
  { 
    Uninitialized ()
      : runtime_error ("Tried to lookup an uninitialized value in an alist")
    { }
  };
#endif HANDLE_EXCEPTIONS

  // Use.
  bool check (const string&) const 
       throw0 ();
  void dump (const Syntax&, int indent = 0) const;

  double number (string) const
       throw2 (Invalid, Uninitialized);
  const string& name (string) const
       throw2 (Invalid, Uninitialized);
  bool flag (string) const
       throw2 (Invalid, Uninitialized);
  const CSMP& csmp (string) const 
       throw2 (Invalid, Uninitialized);
  AttributeList& alist (string) const
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
  const vector<AttributeList*>& alist_sequence (string key) const
       throw2 (Invalid, Uninitialized);

  // Create and Destroy.
  void add (const string&, double);
  void add (const string&, const char*);
  void add (const string&, const string&);
  void add (const string&, bool);
  void add (const string&, int);
  void add (const string&, AttributeList&);
  void add (const string&, const CSMP&);
  void add (const string&, const Time&);
  void add (const string&, const vector<double>&);
  void add (const string&, const vector<string>&);
  void add (const string&, const vector<bool>&);
  void add (const string&, const vector<int>&);
  void add (const string&, const vector<AttributeList*>&);
  void add (const string&, const vector<const CSMP*>&);
  void add (const string&, const vector<const Time*>&);

  void operator += (const AttributeList&);
  AttributeList (const AttributeList& old);
  AttributeList ();
  ~AttributeList ();
};

#endif ALIST_H

// alist.h ends here
