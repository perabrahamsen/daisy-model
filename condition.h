// condition.h -- Logic expressions

#ifndef CONDITION_H
#define CONDITION_H

class Daisy;
class AttributeList;
class Library;
class Syntax;

#include <string>

class Condition
{  
  // Simulation.
public:
  virtual bool match (const Daisy&) const = 0;

  // Library.
public:
  static const Library& library ();
  static Condition& create (const AttributeList&);
  typedef Condition& (*constructor) (const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);

  // Create & Destroy.
protected:
  Condition ();
public:
  virtual ~Condition ();
};

// Ensure the Condition library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Condition_init
{
  static int count;
public:
  Condition_init ();
  ~Condition_init ();
} Condition_init;

#endif CONDITION_H
