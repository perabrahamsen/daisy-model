// condition.h -- Logic expressions

class ColumnList;
class Weather;
class Time;
class Log;
class AttributeList;
class Library;
class Syntax;
#include <std/string.h>

class Condition
{  
  // Simulation.
public:
  virtual bool match (ColumnList&, const Weather&, const Time&) const;
  static Condition null;

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
