// syntax.h

#ifndef SYNTAX_H
#define SYNTAX_H

#include "common.h"
#include <vector>

struct AttributeList;
struct Library;

typedef void (*derive_fun) (const string&, AttributeList&, const string&);

class Syntax
{ 
  struct Implementation;
  Implementation& impl;
public:
  // A syntax entry has an associated size.  If the size is a positive
  // integer, the syntax entry specifies an array of that size.  The
  // default size `Singleton' indicating that the syntax entry match a
  // single item of the specified type, while the `Sequence' used for
  // entries that contain an array of unspecified length. 
  static const int Singleton;	
  static const int Sequence;
  static const int Unspecified;

  // Each syntax entry should have an associated type.
  enum type 
  { Number, AList, CSMP, Boolean, String,
    Date, Integer, Object, Library, Error };
  static const char* type_name (type);
    
  // The requirements with regard to input and output varies with each
  // syntax entry.
  enum category
  {
    // This is a parameter, i.e. its value doesn't change during the
    // compilation, and it cannot be written to the log.
    Const,
    // This a state variable, it must be provided at initialization
    // and can be written to the log.
    State,
    // This is a state variable that can be computer from other state
    // variables, and therefore does not need to be specified before
    // the simulation starts. 
    Optional, 
    // This is a variable that is only computed for logging purposes
    // and not a part of the simulation state. 
    LogOnly
  };
  static const char* category_name (category);

  // These functions will check that an alist conform to the syntax.
  bool check (const AttributeList&, const string& = "<unknown>") const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (const string&) const;
  type lookup (const string&) const;
  const Syntax& syntax (const string&) const;
public:
  const ::Library& library (const string&) const;
  derive_fun derive (const string&) const;
  int  size (const string&) const;
  bool ordered () const;
  const vector<string>& order () const;
  int order (const string& name) const;	// Return index in order, or -1.

  // Print whole syntax table.
  void dump (int indent = 0) const;

  // Get a list of all entries.
  void entries (vector<string>&) const;

  // Add syntax entries
  void add (const string&, type, category, int size = Singleton);
  void add (const string&, const Syntax&, 
	    category = State, int size = Singleton);
  void add (const string&, const ::Library&, 
	    category = State, int size = Singleton);
  void add_library (const string&, const ::Library&, derive_fun);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
  void order (const vector<string>&);
  void order (const string&);
  void order (const string&, const string&);
  void order (const string&, const string&, const string&);
  void order (const string&, const string&, const string&, const string&);
  void order (const string&, const string&, const string&, const string&,
	      const string&);

  // Create and Destroy.

  // A check_fun is a function used for extra syntax checking.
  typedef bool (*check_fun)(const AttributeList&);

  Syntax (check_fun = 0);
  ~Syntax ();
private:
  Syntax (Syntax&);
  Syntax& operator= (Syntax&);
};

template <class T> 
struct add_submodule
{
  add_submodule(const char* name, Syntax& syntax, AttributeList& alist,
		Syntax::category cat = Syntax::State, 
		int size = Syntax::Singleton)
  {
    Syntax& s = *new Syntax ();
    AttributeList& a = *new AttributeList ();
    T::load_syntax (s, a);
    syntax.add (name, s, cat, size);
    alist.add (name, a);
  }
};

void check (const AttributeList& al, const string& s, bool& ok);
void non_negative (double v, const string& s, bool& ok, int index = -1);
void non_positive (double v, const string& s, bool& ok, int index = -1);
void is_fraction (double v, const string& s, bool& ok, int index = -1);

#endif SYNTAX_H
