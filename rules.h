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
//
// When creating a new Rules object, you may specify it as an
// extention to an old object.  The rules of the old object will be
// less significant than any of the new rules.

#ifndef RULES_H
#define RULES_H

struct Action;
struct Condition;
struct Daisy;

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
    Rules (const Rules* = 0);
    ~Rules ();
};

#endif RULES_H
