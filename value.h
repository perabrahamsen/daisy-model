// value.h -- attribute value types
//
// This file species a number of new primitive types in daisy, which
// all can be stored in an AttributeList.

// @ Begin

#ifndef VALUE_H
#define VALUE_H

#include "daisy.h"
#include <std/stdexcept.h>
#include <vector.h>

struct Rules;
struct CSMP;

// @ AttributeList.
//
// A map from attribute names to attribute values.

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
    const vector<double>& array (string) const
        throw2 (Invalid, Uninitialized);
    const Rules& rules (string) const
        throw2 (Invalid, Uninitialized);
    const CSMP& csmp (string) const 
       throw2 (Invalid, Uninitialized);
    const AttributeList& list (string) const
        throw2 (Invalid, Uninitialized);

    // Create and Destroy.
    void add (string, double);
    void add (string, const AttributeList*);
    void add (string, const Rules*);
    void add (string, const CSMP*);
    void add (string, string);
    void add (string, const vector<double>&);
    void add (string, bool);
    AttributeList ();
    ~AttributeList ();
};

// @ Rules
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

struct Action;
struct Condition;

class Rules
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    // Use.
    const Action* match (ColumnList&, const Wheather&, 
			 int day, int hour) const;
    // Create and Destroy.
    void add (Condition*, Action*);
    Rules (const Rules* = NULL);
    ~Rules ();
};

// @ CSMP
// 
// A CSMP is a function defined by a number of points.  
//
// The points must be added by increasing x value, and the function is
// defined by drawing a straight line from each added point to the next.

class CSMP
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    // Use.
    double operator ()(double x) const;
    // Create and Destroy.
    void add (double, double);
    CSMP ();
    ~CSMP ();
};

// @ End

#endif VALUE_H

// value.h ends here
