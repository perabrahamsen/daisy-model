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
  double NH4;
  double NO3;

  // Operations.
public:
  void output (Log&, Filter&) const;
  void clear ();
  void operator+= (const IM&);
  void operator-= (const IM&);
  void operator*= (double);
  void operator/= (double);
  bool empty () const;

  // Create. 
public:
  IM operator* (double flux) const;
  IM operator+ (const IM&) const;
  static void load_syntax (Syntax&, AttributeList&);
  IM (const IM& im);
  IM (const AttributeList&);
  IM (const IM&, double flux);
  IM ();
  ~IM ();
};

#endif IM_H
