// lexer.C --- Lexical analysis.

#include "lexer.h"
#include "tmpstream.h"
#include "treelog.h"
#include "options.h"

#include <string>
#include <vector>
using namespace std;

bool
Lexer::open_file (ifstream& in, const string& name)
{
  // Absolute filename.
  if (name[0] == '.' || name[0] == '/'
#ifndef __unix__
      || name[1] == ':'
#endif
      )
    {
      in.open (name.c_str ());
      return in.good ();
    }

  // Relative filename, use path.
  static vector<string> path;
  if (path.size () == 0)
    {
      // Initialize path.
      const string colon_path
	= getenv (Options::path_name) ? getenv (Options::path_name) : ".";
      int last = 0;
      for (;;)
	{
	  const int next = colon_path.find (PATH_SEPARATOR, last);
	  if (next < 0)
	    break;
	  path.push_back (colon_path.substr (last, next - last));
	  last = next + 1;
	}
      path.push_back (colon_path.substr (last));
    }
  assert (path.size () > 0);
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const string file = path[i] + DIRECTORY_SEPARATOR + name;
      in.open (file.c_str ());
      if (in.good ())
	return true;
    }
  return false;
}

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
  TmpStream tmp;
  tmp () << file << ":" << line << ":" << (column + 1) << ": " << str;
  err.entry (tmp.str ());
}

void
Lexer::eof ()
{ 
  if (!in.eof ())
    error ("Expected end of file");
}
    
Lexer::Lexer (const string& name, Treelog& out)
  : err (out),
    line (1),
    column (0),
    file (name),
    error_count (0)
{  
  if (!open_file (in, name))
    err.entry (string ("Open '") + file + "' failed");
}

Lexer::~Lexer ()
{
  if (in.bad ())
    err.entry (string ("There were trouble parsing '") + file  + "'");
}
