// lexer_data.h --- Lexical analysis for data files.

#ifndef LEXER_DATA_H
#define LEXER_DATA_H

#include "lexer.h"

class Time;

class LexerData : public Lexer
{
  // Lex me!
public:
  string get_word ();
  double get_number ();
  int get_cardinal ();
  void read_date (Time&);
  void skip (const char* str);
  void skip_line ();
  void skip_space ();
  void skip_hyphens ();
  void next_line ();

  // Create and Destroy.
public:
  LexerData (const string& name, Treelog&);
  ~LexerData ();
};

#endif // LEXER_H
