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
  double NH4;

  // Operations.
public:
  void output (Log&, Filter&) const;
  void clear ();
  void operator+= (const IM&);
  void operator-= (const IM&);
  void operator*= (double);
  void operator/= (double);
  bool empty () const;

  // Utilities.
public:
  static double N_left (const AttributeList&);

  // Create. 
public:
  IM operator* (double flux) const;
  IM operator+ (const IM&) const;
  static void load_syntax (Syntax&, AttributeList&);
  IM ();
  IM (const IM& im);
  IM (const AttributeList&);
  IM (const IM&, double flux);
  ~IM ();
};

#endif IM_H
