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

struct ValueCSMP;

class Value
{
public:
    virtual double number () const throw (InvalidValue);
    virtual string name () const throw (InvalidValue);
    virtual bool flag () const throw (InvalidValue);
#ifdef USE_VIRTUAL_VALUE
    virtual Value* lookup (string) const 
      throw (UninitializedValue, InvalidValue);
    virtual Value* check (string) const throw (InvalidValue);
    virtual const Action* match (ColumnList&, const Wheather&,
				 int day, int hour) const throw (InvalidValue);
    virtual double y (double x) const throw (InvalidValue);
    virtual double operator[] (int index) const throw (InvalidValue);
#endif
protected:
    Value ();
    virtual ~Value ();
};

class ValueNumber : public Value
{
    const double value;
public:
    double number () const;
    ValueNumber (double n);
};

class ValueList : public Value
{
    struct Implementation;
    Implementation& impl;
public:
    static const ValueList empty;
    Value* lookup (string) const throw (UninitializedValue);
    Value* check (string) const throw0 ();
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
    double operator[] (int index) const;
    ValueArray ();
    ~ValueArray ();
};

class ValueString : public Value
{
    string impl;
public:
    string name () const;
    ValueString (string);
    ~ValueString ();
};

class ValueBool : public Value
{
    bool impl;
public:
    bool flag () const;
    ValueBool (bool);
    ~ValueBool ();
};

#endif VALUE_H
