// parser_file.h

#ifndef PARSER_FILE_H
#define PARSER_FILE_H

#include "parser.h"

class ParserFile : public Parser
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Use.
public:
  void load (AttributeList&);

  // Create and Destroy.
private:
  friend class ParserFileSyntax;
  static Parser& make (const Syntax& syntax, const AttributeList&);
  ParserFile (const Syntax& syntax, const AttributeList&);
public:
  ParserFile (const Syntax& syntax, string n);
  ~ParserFile ();
};

#endif PARSER_FILE_H
