// lexer.h --- Daisy lexical analysis

#ifndef LEXER_H
#define LEXER_H

#include "common.h"
#include <fstream.h>

struct Lexer
{
  int get ();
  int peek ();
  bool good ();
  string get_string ();
  int get_integer ();
  double get_number ();
  void error (string str);
  void skip (const char*);
  void skip ();
  void skip_to_end ();
  void skip_token ();
  bool looking_at (char);
  void eof ();
  ifstream in;
  string file;
  int line;
  int column;
  int error_count;
  Lexer (const string&);
  ~Lexer ();
};

#endif LEXER_H
