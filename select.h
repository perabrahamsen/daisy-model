// select.h --- Select a state variable.

#ifndef SELECT_H
#define SELECT_H

#include "condition.h"		// Needed for proper initialization.
#include "librarian.h"
#include <map>

struct Geometry;
struct Daisy;

typedef map<string, string, less<string>/**/> string_map;

class Select
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
protected:
  const bool accumulate;	// Accumulate numbers over time.
  const double factor;		// Convert value.
  const double offset;		// - || -
  int count;			// Number of accumulated values.
public:
  static const char *const description;
  const string tag;		// Name of this entry.
  string dimension;		// Physical dimension of this entry.
  virtual const Geometry* geometry () const; // For array tags.
  virtual int size () const;	// For array tags.
  
  // Destination
  class Destination
  {
    // Add data.
  public:
    virtual void error (const string& tag) = 0;
    virtual void missing (const string& tag) = 0;
    virtual void add (const string& tag, const vector<double>& value) = 0;
    virtual void add (const string& tag, double value) = 0;
    virtual void add (const string& tag, const string& value) = 0;

    // Create and Destroy
  public:
    Destination ();
    virtual ~Destination ();
  };

  // Nesting.
public:
  bool is_active ();		// Active time step.
  bool valid ();		// Currently path entry is valid.
  bool valid (const string& next); // Next path entry is valid.

  void open_maybe (const string& value); // Check special values.
  void close_maybe ();
  void open_group (const string&); // Open one group level.
  void open (const string&);	// Open one leaf level.
  void close ();		// Close one level.

  // Output routines.
  virtual void output_time (const string& name, const Time& time);
  virtual void output_number (const string& name, const double number);
  virtual void output_integer (const string& name, const int integer);
  virtual void output_name (const string& name, const string& a_name);
  virtual void output_array (const string& name, const vector<double>& array,
			     const Geometry* geometry);

  // Reset at start of time step.
  bool match (const Daisy& daisy, bool is_printing);

  // Print result at end of time step.
  virtual void done (Destination& dest) = 0;

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
  virtual void initialize (const string_map& conv, double from, double to,
			   const string& timestep);
  Select (const AttributeList& al);
  virtual ~Select ();
};

static Librarian<Select> Select_init ("select");

#endif // SELECT_H
