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

void 
Lexer::error (const string& str)
{
  error_count++;
  err << file << ":" << line << ":" << (column + 1) << ": " << str << "\n";
}

void
Lexer::eof ()
{ 
  if (!in.eof ())
    error ("Expected end of file");
}
    
Lexer::Lexer (const string& name, ostream& out)
  : in (Options::find_file (name)),
    err (out),
    line (1),
    column (0),
    file (name),
    error_count (0)
{  
  if (!in.good ())
    err << "Open `" << file << "' failed\n";
}

Lexer::~Lexer ()
{
  if (in.bad ())
    err << "There were trouble parsing `" << file << "'\n";
  close (in.rdbuf ()->fd ());
}
