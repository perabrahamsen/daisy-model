// log_select.h

#ifndef LOG_SELECT_H
#define LOG_SELECT_H

#include "log.h"
#include "select.h"		// Need to be here to ensure proper
#include "condition.h"		// initialization in derived classes.

struct Select;
struct Condition;

struct LogSelect : public Log
{
  // State. 
  bool is_printing;		// True iff this time step should be logged.
  bool is_active;		// True iff we need values for this time step.

  // Filter functions.
  bool check (const string&) const;
  bool check_derived (const string& field, const string& name,
		      const Library&) const;
  const string description;	// Description of log file.
  Condition& condition;	// Should we print a log now?

  vector<Select*> entries;

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy);

  // Obey conditionals.
  void open_maybe (const string& value);
  void close_maybe ();

  // Open normal items.
  void open (const string& name);
  void close ();

  // Ignore unnamed items.
  void open_unnamed ();
  void close_unnamed ();

  // Open derived items two steps a time.
  void open_derived (const string& field, const string& type);
  void close_derived ();

  // Open derived items in list normally.
  void open_entry (const string& type, const AttributeList&);
  void close_entry ();

  void output (const string& name, const Time& value);
  void output (const string&, const bool);
  void output (const string& name, const double value);
  void output (const string& name, const int value);
  void output (const string& name, const string& value);
  void output (const string& name, const vector<double>& value);
  void output (const string&, const PLF&);

  // Create and Destroy.
  bool check (const Syntax&, Treelog& err) const;
  static void load_syntax (Syntax&, AttributeList&);
  LogSelect (const AttributeList& al);
  ~LogSelect ();
};

#endif // LOG_SELECT_H
