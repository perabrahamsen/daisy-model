// syntax.h

#ifndef SYNTAX_H
#define SYNTAX_H

#include <std/string.h>
#include <list.h>

struct FTable;
struct AttributeList;
struct Log;
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
  const int Singleton = -117;	
  const int Sequence = -3212;	

  // Each syntax entry should have an associated type.
  enum type 
  { Number, List, CSMP, Function, Boolean, String,
    Date, Integer, Output, Class, Object, Error };

  // The requirements with regard to input and output varies with each
  // syntax entry.
  enum required
  {
    // This is a parameter, i.e. its value doesn't change during the
    // compilation, and it cannot be written to the log.
    Const,
    // This a state variable, it must be provided at initialization
    // and can be written to the log.
    InOut,
    // This is a complex item which can contain both parameters and
    // state variables. 
    Mixed,
    // This is a complex object that is not required to be fully
    // specified before the simulation starts.
    Sparse, 
    // This is a state variable that can be computer from other state
    // variables, and therefore does not need to be specified before
    // the simulation starts. 
    Optional, 
    // This is a variable that is only computed for logging purposes
    // and not a part of the simulation state. 
    LogOnly
  };

  // These functions will check that an alist conform to the syntax.
  // The first is quite chatty about it.
  bool check (string, const AttributeList&, const Log&, 
	      bool sparse = false) const;
  bool check (const AttributeList&) const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  required status (string) const;
  type lookup (string) const;
  const Syntax& syntax (string) const;
  const FTable* function (string) const;
  const Library& library (string) const;
  derive_fun derive (string) const;
  int  size (string) const;
  bool ordered () const;
  const list<string>& order () const;

  // Add syntax entries
  void add (string, type, required, int size = Singleton);
  void add (string, const Syntax&, required = Mixed, int size = Singleton);
  void add (string, const FTable*, required, int size = Singleton);
  void add (string, const Library&, required = Mixed, int size = Singleton);
  void add_output (string, const Syntax&, required);
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
  Syntax ();
  ~Syntax ();
};

template <class T> 
struct add_submodule
{
  add_submodule(const char* name, Syntax& syntax, AttributeList& alist)
  {
    Syntax& s = *new Syntax ();
    AttributeList& a = *new AttributeList ();
    T::load_syntax (s, a);
    syntax.add (name, s);
    alist.add (name, a);
  }
};

#endif SYNTAX_H
