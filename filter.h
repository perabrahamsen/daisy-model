// filter.h

#ifndef FILTER_H
#define FILTER_H

#include <std/string.h>

class FilterAll;

class Filter
{
    // Use.
public:
    virtual bool check (string) const = 0;
    virtual const Filter* lookup (string) const = 0;

    // Content.
public:
    static const FilterAll* all;

    // Create and Destroy.
public:
    virtual ~Filter ();
protected:
    Filter ();
};

class FilterAll : public Filter
{
    // Use.
public:
    bool check (string) const;
    const Filter* lookup (string) const;

    // Create and Destroy.
private:
    friend class Filter; // Only create from Filter.
    FilterAll ();
};

class FilterSome : public Filter
{
    // Use.
public:
    bool check (string) const;
    const Filter* lookup (string) const;

    // Content.
private:
    struct Implementation;
    Implementation& impl;
	
    // Create and Destroy.
public:
    ~FilterSome ();
private:
    friend class Input; // Only create from Input.
    void add (string, const Filter* = Filter::all);
    FilterSome ();
};

#endif FILTER_H
