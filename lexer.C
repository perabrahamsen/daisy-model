// lexer.C --- Lexical analysis.

#include "lexer.h"

int
Lexer::get ()
{
  int c = in.get ();

  switch (c)
    {
    case '\n':
      column = 0;
      line++;
      break;
    case '\t':
      column += 8 - column % 8;
      break;
    default:
      column++;
    }
  return c;
}

int
Lexer::peek ()
{
  return in.peek ();
}

bool
Lexer::good ()
{
#ifdef BORLAND_EOF  
// BCC requires that you try to read beyond the eof to detect eof.
  char c;
  in.get (c);
  bool ok = in.good ();
  if (ok)
    in.putback (c);
  return ok;
#else
  return in.good ();
#endif
}

string
Lexer::get_string ()
{
  skip ();
  int c = peek ();
  
  if (c == '"')
    {
      // Get a string.
      string str ("");
      skip ("\"");

      for (c = get (); good() && c != '"'; c = get ())
	{
	  if (c == '\\')
	    {
	      c = get ();
	      switch (c)
		{
		case '\\':
		case '"':
		  break;
		default:
		  error (string ("Unknown character escape '")
			 // BUG: SHOULD USE DYNAMIC CAST!
			 + char (c) + "'");
		}
	    }
	  str += static_cast<char> (c);
	}
      return str;
    }
  else if (c != '_' && c != '-' && !isalpha (c))
    {
      error ("Identifier or string expected");
      skip_to_end ();
      return "error";
    }
  else
    {
      // Get an identifier.
      string str ("");
      do
	{
	  str += char (c);
	  get ();
	  c = peek ();
	}
      while (good() && (c == '_' || c == '-' || isalnum (c)));
    
      return str;
    }
}

double
Lexer::get_number ()
{
  skip ();
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
      skip_to_end ();
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
Lexer::get_integer ()
{
  skip ();
  string str;
  int c = peek ();

  while (isdigit (c) || c == '-' || c == '+')
    {
      str += static_cast<char> (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Integer expected");
      skip_to_end ();
      return -42;
    }
  return atoi (str.c_str ());
}

void 
Lexer::error (string str)
{
  error_count++;
  CERR << file << ":" << line << ":" << (column + 1) << ": " << str << "\n";
}

void
Lexer::skip (const char* str)
{ 
  skip ();
  for (const char* p = str; *p; p++)
    if (*p != peek ())
      {
	error (string("Expected `") + str + "'");
	skip_token ();
	break;
      }
    else
      get ();
}

void
Lexer::skip ()
{ 
  while (true)
    if (!good ())
      return;
    else if (isspace (peek ()))
      get ();
    else if (peek () == ';')
      while (good () && get () != '\n')
      ;
    else
      return;
}

void
Lexer::skip_token ()
{
  if (peek () == ';' || isspace (peek ()))
    skip ();
  if (peek () == '"')
    get_string ();
  else if (peek () == '.' || isdigit (peek ()))
    get_number ();
  else if (peek () == '(') 
    {
      get ();
      skip_to_end ();
      skip (")");
    }
  else if (isalnum (peek ()) || peek () == '_' || peek () == '-')
    get_string ();
  else
    get ();
}

void
Lexer::skip_to_end ()
{
  while (peek () != ')' && good ())
    skip_token ();
}

bool
Lexer::looking_at (char c)
{ 
  skip ();
  return peek () == c;
}

void
Lexer::eof ()
{ 
  skip ();
  if (!in.eof ())
    error ("Expected end of file");
}
    
Lexer::Lexer (const string& name)
  : in (Options::find_file (name)),
    file (name),
    line (1),
    column (0),
    error_count (0)
{  
  if (!in.good ())
    CERR << "Open `" << file << "' failed\n";
}

Lexer::~Lexer ()
{
  if (in.bad ())
    CERR << "There were trouble parsing `" << file << "'\n";
  close (in.rdbuf ()->fd ());
}
