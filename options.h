// options.h --- handle options from the system ebvironment.

#ifndef OPTIONS_H
#define OPTIONS_H

#include "common.h"
#ifdef MISSING_OSTREAM
#include <iostream.h>
#else 
#include <ostream.h>
#endif

#define CERR (Options::error ())
#define CWAR (Options::warning ())
#define COUT (Options::message ()) 

class Options
{
public: 
  static ostream& message ();
  static ostream& warning ();
  static ostream& error ();
  static int find_file (const string& name);
};

#endif OPTIONS_H
