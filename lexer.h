// lexer.h --- Lexical analysis

#ifndef LEXER_H
#define LEXER_H

#include "common.h"
#include <fstream.h>

class Lexer
{
  // State.
private:
  ifstream in;
public:
  ostream& err;
private:
  int line;
  int column;
public:
  const string file;
  int error_count;

  // Operations.
public:
  int get ();
  int peek ();
  bool good ();
  void error (const string& str);
  void eof ();

  // Create and destroy.
public:
  Lexer (const string&, ostream&);
  ~Lexer ();
};

#endif // LEXER_H
