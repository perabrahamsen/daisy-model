// im.h

#ifndef IM_H
#define IM_H

struct Log;
struct Filter;
struct Syntax;
struct AttributeList;

class IM
{
  // Content.
public:
  double NO3;

  // Operations.
public:
  void output (Log& log, const Filter& filter) const;
  void clear ();
  void operator += (const IM&);
  void operator -= (const IM&);
  void operator *= (double);
  void operator /= (double);
  bool empty () const;

  // Create. 
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  IM ();
  IM (const AttributeList&);
  IM (const IM&, double flux);
  ~IM ();
};

#endif IM_H
