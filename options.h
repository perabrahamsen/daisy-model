// options.h --- handle options from the system ebvironment.

#ifndef OPTIONS_H
#define OPTIONS_H

#include "common.h"
#ifdef MISSING_OSTREAM
#include <iostream.h>
#else 
#include <ostream.h>
#endif

struct Syntax;
struct AttributeList;

#define CERR (Options::error ())
#define CWAR (Options::warning ())
#define COUT (Options::message ()) 

class Options
{
public: 
  const string program_name;
  static ostream& message ();
  static ostream& warning ();
  static ostream& error ();
  static int find_file (const string& name);
  void usage () const;
  Options (int& argc, char**& argv, 
	   Syntax& syntax, AttributeList& alist);
};

#endif OPTIONS_H
