// syntax.h

#include <std/string.h>

struct FTable;
struct AttributeList;
struct Log;

// Ensure the syntax table is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Syntax_init
{
  static int count;
public:
  Syntax_init ();
  ~Syntax_init ();
} syntax_init;

class Syntax 
{ 
  struct Implementation;
  Implementation& impl;
public:
  enum type 
  { Number, List, Rules, CSMP, Function, Array, Boolean, String,
    Date, Crops, Columns, Integer, Soil, UZmodel, Error };
  enum required
  { Mandatory, Optional, LogOnly };
  bool check (string, const AttributeList&, const Log&) const;
  required status (string) const;
  type lookup (string) const;
  const Syntax* syntax (string) const;
  const FTable* function (string) const;
  int  size (string) const;
  string find (const Syntax*) const;
  void add (string, type, required = Mandatory);
  void add (string, const Syntax*, required = Mandatory);
  void add (string, const FTable*, required = Mandatory);
  void add (string, const int, required = Mandatory);
  Syntax ();
  ~Syntax ();
};

extern Syntax* syntax_table;
