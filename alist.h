// alist.h -- attribute list
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


#ifndef ALIST_H
#define ALIST_H

#include "common.h"

#include <stdexcept>
#include <string>
#include <vector>
#include <list>

class Time;
class PLF;
class AttributeList;
class Syntax;

class AttributeList
{
  // Content.
  struct Implementation;
public:				// EGCS require this to be public...
  Implementation& impl;

public:
  // Is 'key' an element of this alist?
  bool check (const string& key) const;
  // Is this alist a subset of 'other'?
  bool subset (const AttributeList& other, const Syntax& syntax) const;
  // Is the element 'key' in this alist a subset of the correspi
  bool subset (const AttributeList& other, const Syntax& syntax,
	       const string& key) const;
  int size (const string& key) const;

  // Extract values.
  double number (const string&) const;
  double number (const char*) const;
  const string& name (const string&) const;
  const string& name (const char*) const;
  bool flag (const string&) const;
  bool flag (const char*) const;
  const PLF& plf (const string&) const;
  const PLF& plf (const char*) const;
  AttributeList& alist (const string&) const;
  AttributeList& alist (const char*) const;
  int integer (const string&) const;
  int integer (const char*) const;
  const Time& time (const string&) const;
  const Time& time (const char*) const;
  const vector<double>& number_sequence (const string&) const;
  const vector<double>& number_sequence (const char*) const;
  const vector<string>& name_sequence (const string& key) const;
  const vector<string>& name_sequence (const char* key) const;
  const vector<bool>& flag_sequence (const string& key) const;
  const vector<bool>& flag_sequence (const char* key) const;
  const vector<int>& integer_sequence (const string& key) const;
  const vector<int>& integer_sequence (const char* key) const;
  const vector<const Time*>& time_sequence (const string& key) const;
  const vector<const Time*>& time_sequence (const char* key) const;
  const vector<const PLF*>& plf_sequence (const string& key) const;
  const vector<const PLF*>& plf_sequence (const char* key) const;
  const vector<AttributeList*>& alist_sequence (const string& key) const;
  const vector<AttributeList*>& alist_sequence (const char* key) const;

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
  bool revert (const string&, const AttributeList&, const Syntax&);
  void operator += (const AttributeList&);
  void operator = (const AttributeList&);
  void clear ();
  AttributeList (const AttributeList& old);
  AttributeList ();
  ~AttributeList ();
};

#endif // ALIST_H

// alist.h ends here
