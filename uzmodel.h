// uzmodel.h

#ifndef UZMODEL_H
#define UZMODEL_H


#include <std/string.h>
#include <vector.h>

class Soil;
class Library;
class Syntax;
class AttributeList;
class Log;
class Filter;

class UZtop
{
public:
  virtual bool flux_top () const = 0;
  virtual double q () const = 0;
  virtual void flux_top_on () = 0;
  virtual void flux_top_off () = 0;
  virtual bool accept_top (double) = 0;
  virtual ~UZtop ();
};

class UZbottom
{
public:
  virtual bool flux_bottom () const = 0;
  virtual bool accept_bottom (double) = 0;
  virtual ~UZbottom ();
};

class UZmodel : public UZtop, public UZbottom
{
  // UZtop.
public:
  bool flux_top () const = 0;
  double q () const = 0;
  void flux_top_on () = 0;
  void flux_top_off () = 0;
  bool accept_top (double) = 0;

  // UZbottom.
public:
  bool flux_bottom () const = 0;
  bool accept_bottom (double) = 0;
  
  // Simulate.
public:
  virtual void tick (const Soil& soil,
		     int first, UZtop& top, 
		     int last, UZbottom& bottom, 
		     const vector<double>& S,
		     const vector<double>& h_old,
		     const vector<double>& Theta_old,
		     vector<double>& h,
		     vector<double>& Theta,
		     vector<double>& q) = 0;
  virtual void output (const string, Log&, const Filter*) const;

  // Library.
public:
  static const Library& library ();
  static UZmodel* create (const AttributeList&);
  typedef UZmodel* (*constructor) (const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);

  // Create and Destroy.
public:
  virtual ~UZmodel ();
};

// Ensure the UZ library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class UZ_init
{
  static int count;
public:
  UZ_init ();
  ~UZ_init ();
} uz_init;

#endif UZMODEL_H
