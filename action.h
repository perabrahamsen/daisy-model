// action.h -- Manager actions

#ifndef ACTION_H
#define ACTION_H

#include <string>

class Daisy;
class AttributeList;
class Library;
class Syntax;
class Column;

class Action
{
  // Content.
private:
  const Action *const parent;

  // Simulation.
public:
  virtual void doIt (Daisy&) = 0;
  virtual bool match (const Column&) const;

  // Library.
public:
  static const Library& library ();
  static Action& create (const AttributeList&, const Action *const);
  typedef Action& (*constructor) (const AttributeList&, const Action *const);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);
 
  // Create and Destroy.
public: 
  virtual bool check (Daisy&) const;
protected:
  Action (const Action *const);
public:
  virtual ~Action ();
};

// Ensure the Action library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Action_init
{
  static int count;
public:
  Action_init ();
  ~Action_init ();
} Action_init;

#endif ACTION_H
