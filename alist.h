// alist.h -- attribute list

#ifndef ALIST_H
#define ALIST_H

#include "common.h"

#include <stdexcept>
#include <string>
#include <vector>
#include <list>

struct Time;
struct PLF;
struct AttributeList;
struct Syntax;

class AttributeList
{
  // Content.
  struct Implementation;
public:				// EGCS require this to be public...
  Implementation& impl;

public:
  // Is `key' an element of this alist?
  bool check (const string& key) const;
  // Is this alist a subset of `other'?
  bool subset (const AttributeList& other, const Syntax& syntax) const;
  // Is the element `key' in this alist a subset of the correspi
  bool subset (const AttributeList& other, const Syntax& syntax,
	       const string& key) const;
  int size (const string& key) const;

  // Extract values.
  double number (string) const;
  const string& name (string) const;
  bool flag (string) const;
  const PLF& plf (string) const;
  AttributeList& alist (string) const;
  int integer (string) const;
  const Time& time (string) const;
  const vector<double>& number_sequence (string) const;
  const vector<string>& name_sequence (string key) const;
  const vector<bool>& flag_sequence (string key) const;
  const vector<int>& integer_sequence (string key) const;
  const vector<const Time*>& time_sequence (string key) const;
  const vector<const PLF*>& plf_sequence (string key) const;
  const vector<AttributeList*>& alist_sequence (string key) const;

  // Create and Destroy.
  void add (const string&, double);
  void add (const string&, const char*);
  void add (const string&, const string&);
  void add (const string&, bool);
  void add (const string&, int);
  void add (const string&, const AttributeList&);
  void add (const string&, const PLF&);
  void add (const string&, const Time&);
  void add (const string&, const vector<double>&);
  void add (const string&, const vector<string>&);
  void add (const string&, const vector<bool>&);
  void add (const string&, const vector<int>&);
  void add (const string&, const vector<AttributeList*>&);
  void add (const string&, const vector<const PLF*>&);
  void add (const string&, const vector<const Time*>&);

  void remove (const string&);
  void operator += (const AttributeList&);
  void operator = (const AttributeList&);
  void clear ();
  AttributeList (const AttributeList& old);
  AttributeList ();
  ~AttributeList ();
};

#endif ALIST_H

// alist.h ends here
