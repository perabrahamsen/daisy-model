// rules.h
//
// A Rules object is an ordered list of Condition, Action pairs.
// 
// You can match some state in the Rules object, and it will return
// the Action part of the first pair whose condition evaluates to true
// for the state.
//
// You add one Condition, Action pair a time, the first pair will also
// be the first in the list.

#ifndef RULES_H
#define RULES_H

#include <std/string.h>
#include <vector.h>

struct Action;
struct Condition;
struct Daisy;
struct Syntax;
struct AttributeList;

class Rules
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  // Use.
  const Action* match (const Daisy&) const;
  // Create and Destroy.
  void add (const Condition*, const Action*);
  static const Syntax& syntax ();
  Rules (const vector <const AttributeList*>& rl);
  ~Rules ();
};

// Ensure the Rules syntax is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Rules_init
{
  static int count;
public:
  Rules_init ();
  ~Rules_init ();
} Rules_init;

#endif RULES_H
