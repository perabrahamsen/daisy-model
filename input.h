// input.h

#ifndef INPUT_H
#define INPUT_H

#include <std/stdexcept.h>

struct AttributeList;
struct Syntax;

struct Usage : runtime_error
{ 
  const char* what () const;
};

const AttributeList&
parse (const Syntax&, int& argc, char**& argv);

#endif INPUT_H
