// printer.h --- Support for printing alist structures.

#ifndef PRINTER_H
#define PRINTER_H

#include "librarian.h"

class Printer
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Interface.
public:
  // Print comment.
  virtual void print_comment (const string& comment) = 0;
  // Print content of alist.
  virtual void print_alist (const AttributeList& alist, const Syntax&) = 0;
  // Print all elements in all libraries associated with 'filename'.
  virtual void print_library_file (const string& filename) = 0;
  // Print a parser input.
  virtual void print_input (const AttributeList& alist) = 0;

  // True iff no errors have occured.
  virtual bool good () = 0;

  // Create and Destroy.
protected:
  Printer (const string& name);
public:
  virtual ~Printer ();
};

static Librarian<Printer> Printer_init ("printer");

#endif // PRINTER_H
