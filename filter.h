// filter.h

#ifndef FILTER_H
#define FILTER_H

#include <string>

class FilterAll;
class FilterNone;

class Filter
{
  // Use.
public:
  virtual bool check (string, bool log_only = false) const = 0;
  virtual const Filter& lookup (string) const = 0;

    // Content.
public:
  static const FilterAll* all;
  static const FilterNone* none;

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
  bool check (string, bool log_only = false) const;
  const Filter& lookup (string) const;

    // Create and Destroy.
public:
  FilterAll ();
};

class FilterNone : public Filter
{
  // Use.
public:
  bool check (string, bool log_only = false) const;
  const Filter& lookup (string) const;

    // Create and Destroy.
public:
  FilterNone ();
};

class FilterSome : public Filter
{
  // Use.
public:
  bool check (string, bool log_only = false) const;
  const Filter& lookup (string) const;

    // Content.
private:
  struct Implementation;
  Implementation& impl;
	
  // Create and Destroy.
public:
  ~FilterSome ();
  friend class Parser;
  void add (string, const Filter& = *Filter::all);
  FilterSome ();
};

#endif FILTER_H
