// inorganic_matter.h

#ifndef INORGANIC_MATTER_H
#define INORGANIC_MATTER_H

struct AttributeList;
struct SoluteMatter;
struct Log;
struct Filter;
struct IOM;
struct Syntax;

class InorganicMatter
{ 
  // Content.
public:
  IOM& iom;

  // Opertations.
public:
  void output (Log& log, const Filter& filter) const;
  void clear ();
  void operator += (const InorganicMatter&);
  void operator -= (const InorganicMatter&);
  void operator *= (double);
  void operator /= (double);

  // Create & Destroy.
  static void load_syntax (Syntax&, AttributeList&);
  InorganicMatter ();
  InorganicMatter (const AttributeList&);
  InorganicMatter (const InorganicMatter&, double flux);
  ~InorganicMatter ();
};

class SoluteMatter : private InorganicMatter
{ 
public:
  static void load_syntax (Syntax&, AttributeList&);
  friend class InorganicMatter;
  InorganicMatter operator* (double flux) const;
  SoluteMatter (const AttributeList&);
  ~SoluteMatter ();
};

# endif INORGANIC_MATTER_H
