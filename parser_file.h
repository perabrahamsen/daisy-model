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
public:
  void initialize (const Syntax&);
  ParserFile (const AttributeList&);
  ParserFile (const Syntax& syntax, const string& n);
  ~ParserFile ();
};

#endif PARSER_FILE_H
