// lexer.h --- Lexical analysis

#ifndef LEXER_H
#define LEXER_H

#include "common.h"
#include <fstream>
using namespace std;

class Treelog;

class Lexer
{
  // State.
private:
  static bool open_file (ifstream& in, const string& name);
  ifstream in;
public:
  Treelog& err;
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
  Lexer (const string&, Treelog&);
  ~Lexer ();
};

#endif // LEXER_H
