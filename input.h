// input.h

#ifndef INPUT_H
#define INPUT_H

#include <std/stdexcept.h>
#include <pair.h>

struct Log;
struct AttributeList;
struct Syntax;

struct Usage : runtime_error
{ 
  const char* what () const;
};

pair<Log*, const AttributeList*>
parse (const Syntax&, int& argc, char**& argv);

#endif INPUT_H
