// value.h -- attribute value types

#ifndef VALUE_H
#define VALUE_H

#include "daisy.h"
#include <std/stdexcept.h>
#include <vector.h>

struct Action;
struct Condition;

struct InvalidValue : runtime_error
{ 
    const char* what () const;
};

struct UninitializedValue : runtime_error
{ 
    const char* what () const;
};

class Value
{
public:
    virtual double number ();
    virtual Value* lookup (string) const 
      throw (UninitializedValue, InvalidValue);
    virtual Value* check (string) const throw (InvalidValue);
    virtual const Action* match (ColumnList&, const Wheather&,
				 int day, int hour) const throw (InvalidValue);
    virtual double y (double x) const throw (InvalidValue);
    virtual double operator[] (int index) const throw (InvalidValue);
    virtual string name () const throw (InvalidValue);
protected:
    Value ();
    virtual ~Value ();
};

class ValueNumber : public Value
{
    const double value;
public:
    double number ();
    ValueNumber (double n);
};

class ValueList : public Value
{
    struct Implementation;
    Implementation& impl;
public:
    static const ValueList empty;
    Value* lookup (string) const throw (UninitializedValue);
    Value* check (string) const throw ();
    void add (string, Value*);
    ValueList ();
    ValueList (const ValueList&);
    ~ValueList ();
};

class ValueRules : public Value
{
    struct Implementation;
    Implementation& impl;
    friend class Input;
    void add (Condition*, Action*);
public:
    const Action* match (ColumnList&, const Wheather&, 
			 int day, int hour) const;
    ValueRules (const ValueRules* = NULL);
    ~ValueRules ();
};

class ValueCSMP : public Value
{
    struct Implementation;
    Implementation& impl;
    friend class Input;
    void add (double, double);
public:
    double y (double x) const;
    ValueCSMP ();
    ~ValueCSMP ();
};

class ValueArray : public Value
{
    vector <double> impl;
    friend class Input;
    void add (double);
public:
    double operator[] (int index);
    ValueArray ();
    ~ValueArray ();
};

class ValueString : public Value
{
    string impl;
public:
    string name ();
    ValueString (string s);
    ~ValueString ();
};

#endif VALUE_H
