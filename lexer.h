// lexer.h --- Daisy lexical analysis

#ifndef LEXER_H
#define LEXER_H

#include "common.h"
#include <fstream.h>

class Lexer
{
  // State.
private:
  ifstream in;
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
  void error (string str);
  void eof ();

  // Create and destroy.
public:
  Lexer (const string&);
  ~Lexer ();
};

#endif LEXER_H
