// printer_file.h -- Print alist in ParserFile format.

#ifndef PRINTER_FILE_H
#define PRINTER_FILE_H

#include "printer.h"

class PrinterFile : public Printer
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Interface.
public:
  // Print comment.
  void print_comment (const string& comment);
  // Print content of alist.
  void print_alist (const AttributeList& alist, const Syntax&);
  // Print all elements in all libraries associated with 'filename'.
  void print_library_file (const string& filename);
  // Print a parser input.
  void print_input (const AttributeList& alist);

  // True iff no errors have occured.
  bool good ();

  // Create and Destroy.
public:
  PrinterFile (const AttributeList& al);
  PrinterFile (const string& filename);
  ~PrinterFile ();
};

#endif // PRINTER_FILE_H
