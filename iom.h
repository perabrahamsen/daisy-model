// iom.h

#ifndef IOM_H
#define IOM_H

struct Log;
struct Filter;
struct Syntax;
struct AttributeList;

class IOM
{
  // Content.
public:
  double NO3;

  // Operations.
public:
  void output (Log& log, const Filter& filter) const;
  void clear ();
  void operator += (const IOM&);
  void operator -= (const IOM&);
  void operator *= (double);
  void operator /= (double);

  // Create. 
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  IOM ();
  IOM (const AttributeList&);
  IOM (const IOM&, double flux);
  ~IOM ();
};

#endif IOM_H
