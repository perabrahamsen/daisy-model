// options.h --- handle options from the system ebvironment.

#ifndef OPTIONS_H
#define OPTIONS_H

#include <string>
// #include <sys/types.h>
// #include <sys/stat.h>
// #include <fcntl.h>
#include <osfcn.h>

class Options
{
public: 
  static int find_file (const string name);
};

#endif OPTIONS_H
