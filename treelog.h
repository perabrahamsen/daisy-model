// treelog.h -- Log hierarchical information.

#ifndef TREELOG_H
#define TREELOG_H

#include <string>
using namespace std;

class Treelog
{
  // Content.
public:
  int count;

  // Nesting.
public:
  class Open
  {
  private:
    Treelog& log;
  public:
    Open (Treelog& l, const string& name);
    ~Open ();
  };
  virtual void open (const string& name) = 0;
  virtual void close () = 0;
  
  // Use.
public:
  virtual void entry (const string&);
  
  // Create and Destroy.
public:
  static Treelog& null ();
  Treelog ();
  virtual ~Treelog ();
};

#endif // TREELOG_H
