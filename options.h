// options.h --- handle options from the system ebvironment.

#ifndef OPTIONS_H
#define OPTIONS_H

#include "common.h"
// Doesn't exists in Borland C++ 5.01.
// #include <osfcn.h>

class Options
{
public: 
  static int find_file (const string name);
};

#endif OPTIONS_H
