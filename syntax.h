// syntax.h

#ifndef SYNTAX_H
#define SYNTAX_H

#include "common.h"
#include <list>

struct AttributeList;
struct Library;

typedef void (*derive_fun) (string, const AttributeList&, string);

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

  // Each syntax entry should have an associated type.
  enum type 
  { Number, List, CSMP, Boolean, String,
    Date, Integer, Class, Object, Error };
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
  // The first is quite chatty about it.
  bool check (const AttributeList&, string = "<unknown>") const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (string) const;
  type lookup (string) const;
  const Syntax& syntax (string) const;
public:
  const Library& library (string) const;
  derive_fun derive (string) const;
  int  size (string) const;
  bool ordered () const;
  const list<string>& order () const;

  // Add syntax entries
  void add (string, type, category, int size = Singleton);
  void add (string, const Syntax&, category = State, int size = Singleton);
  void add (string, const Library&, category = State, int size = Singleton);
  void add_class (string, const Library&, derive_fun);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
  void order (const list<string>&);
  void order (string);
  void order (string, string);
  void order (string, string, string);
  void order (string, string, string, string);
  void order (string, string, string, string, string);

  // Create and Destroy.
  void dump (int indent = 0) const;

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

void check (const AttributeList& al, string s, bool& ok);
void non_negative (double v, string s, bool& ok, int index = -1);
void non_positive (double v, string s, bool& ok, int index = -1);
void is_fraction (double v, string s, bool& ok, int index = -1);

#endif SYNTAX_H
