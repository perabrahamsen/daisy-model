// options.h -- Parsing command line options.

#ifndef OPTIONS_H
#define OPTIONS_H

#include "common.h"

class Syntax;
class AttributeList;

class Options
{
  static string get_arg (int& argc, char**& argv);
public: 
  static const char *const log_name;
  static const char *const path_name;
  const string program_name;
  void usage () const;
  Options (int& argc, char**& argv, 
	   Syntax& syntax, AttributeList& alist);
};

#endif // OPTIONS_H
