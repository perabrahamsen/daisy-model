// manager.h

#ifndef MANAGER_H
#define MANAGER_H

#include <std/string.h>

struct AttributeList;
struct Action;
struct Daisy;
struct Syntax;
struct Library;

class Manager
{
  // Simulation.
public:
  virtual const Action* action (const Daisy&) = 0;

  // Library.
public:
  static const Library& library ();
  static Manager& create (const AttributeList&);
  typedef Manager* (*constructor) (const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);

    // Create and Destroy.
protected:
  Manager ();
public:
  virtual ~Manager ();
};

// Ensure the Manager library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Manager_init
{
  static int count;
public:
  Manager_init ();
  ~Manager_init ();
} Manager_init;

#endif MANAGER_H
