// syntax.h

#ifndef SYNTAX_H
#define SYNTAX_H

#include <string>
#include <list>

struct FTable;
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
  const int Singleton = -117;	
  const int Sequence = -3212;	

  // Each syntax entry should have an associated type.
  enum type 
  { Number, List, CSMP, Function, Boolean, String,
    Date, Integer, Filter, Class, Object, Error };

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

  // These functions will check that an alist conform to the syntax.
  // The first is quite chatty about it.
  bool check (const AttributeList&, string = "<unknown>") const;

  // These functions will allow you to lookup information about a
  // specific syntax entry. 
  bool is_const (string) const;
  type lookup (string) const;
  const Syntax& syntax (string) const;
  const FTable* function (string) const;
  const Library& library (string) const;
  derive_fun derive (string) const;
  int  size (string) const;
  bool ordered () const;
  const list<string>& order () const;

  // Add syntax entries
  void add (string, type, category, int size = Singleton);
  void add (string, const Syntax&, category = State, int size = Singleton);
  void add (string, const FTable*, category, int size = Singleton);
  void add (string, const Library&, category = State, int size = Singleton);
  void add_filter (string, const Syntax&, category);
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

void check (const AttributeList& al, string s, bool& ok);
void non_negative (double v, string s, bool& ok);

#endif SYNTAX_H
