// treelog_stream.h -- Log hierarchical information in an ostream.

#ifndef TREELOG_STREAM_H
#define TREELOG_STREAM_H

#include "treelog.h"

struct ostream;

class TreelogStream : public Treelog
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Nesting.
public:
  void open (const string& name);
  void close ();

  // Use.
public:
  void entry (const string&);
  
  // Create and Destroy.
public:
  TreelogStream (ostream&);
  ~TreelogStream ();
};

#endif TREELOG_STREAM_H
