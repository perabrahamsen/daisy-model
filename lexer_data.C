// lexer_data.C --- Reda data from a file.

#include "lexer_data.h"
#include "time.h"

string
LexerData::get_word ()
{
  string tmp;
  while (good () && !isspace (peek ()))
    tmp += static_cast<char> (get ());
  return tmp;
}

double
LexerData::get_number ()
{
  string str;
  int c = peek ();

  while (good () && (isdigit (c) 
		     || c == '.' || c == '-' || c == '+' 
		     || c == 'e' || c == 'E'))
    {
      str += static_cast<char> (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Number expected");
      return -42.42e42;
    }
  const char *c_str = str.c_str ();
  const char *endptr = c_str;
  const double value = strtod (c_str, (char**) &endptr);
  
  if (*endptr != '\0')
    error (string ("Junk at end of number `") + endptr + "'");

  return value;
}

int
LexerData::get_cardinal ()
{
  string str;
  int c = peek ();

  while (isdigit (c))
    {
      str += static_cast<char> (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Integer expected");
      return -42;
    }
  return atoi (str.c_str ());
}

void 
LexerData::read_date (Time& time)
{
  int year = get_cardinal ();
  skip ("-");
  int month = get_cardinal ();
  skip ("-");
  int mday = get_cardinal ();
  int hour = 0;
  if (peek () == 'T' || peek () == ':')
    {
      (void) get ();
      hour = get_cardinal ();
    }
  if (Time::valid (year, month, mday, hour))
    time = Time (year, month, mday, hour);
  else
    error ("Invalid date");
}

void
LexerData::skip (const char* str)
{ 
  for (const char* p = str; *p; p++)
    if (*p != peek ())
      {
	error (string("Expected `") + str + "'");
	break;
      }
    else
      get ();
}

void
LexerData::skip_line ()
{
  while (good () && peek () != '\n')
    (void) get ();
}

void
LexerData::skip_space ()
{
  while (good () && (peek () == ' ' || peek () == '\t'))
    (void) get ();
}

void
LexerData::skip_hyphens ()
{
  while (good () && peek () == '-')
    (void) get ();
  if (get () != '\n')
    {
      error ("Expected line of hyphens only");
      skip_line ();
      get ();
    }
}

void
LexerData::next_line ()
{
  skip_space ();
  if (peek () != '\n')
    {
      error ("Expected end of line");
      skip_line ();
    }
  (void) get ();

  while (good ())
    {
      skip_space ();
      if (peek () == '#')
	skip_line ();
      else if (peek () != '\n')
	break;
      else
	(void) get ();
    }
}

LexerData::LexerData (const string& name, Treelog& out)
  : Lexer (name, out)
{ }

LexerData::~LexerData ()
{ }
