// printer.h --- Support for printing alist structures.

#ifndef PRINTER_H
#define PRINTER_H

#include "librarian.h"

class Printer
{
  // Content.
public:
  const string name;

  // Interface.
public:

  // Create and Destroy.
protected:
  Printer (const string& name);
public:
  virtual ~Printer ();
};

static Librarian<Printer> Printer_init ("printer");

#endif PRINTER_H
