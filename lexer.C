// lexer.C --- Lexical analysis.

#include "lexer.h"
#include "tmpstream.h"
#include "treelog.h"
#include "options.h"
#include "message.h"

#include <string>
#include <vector>
#include <fstream>
#include <iostream>

using namespace std;

istream& 
Lexer::open_file (const string& name)
{
  // Absolute filename.
  if (name[0] == '.' || name[0] == '/'
#ifndef __unix__
      || name[1] == ':'
#endif
      )
    {
      return *new ifstream (name.c_str ());
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
  ifstream* in = NULL;
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const string file = path[i] + DIRECTORY_SEPARATOR + name;
      CERR << "Trying " << file << "...\n";
      delete in;
      in = new ifstream (file.c_str ());
      if (in->good ())
	return *in;
      CERR << "Failed.\n";
    }
  return *in;
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
#if 1 
  // BCC and GCC 3.0 requires that you try to read beyond the eof
  // to detect eof.
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
  : in (open_file (name)),
    err (out),
    line (1),
    column (0),
    file (name),
    error_count (0)
{  
  if (&in == &cin || in.bad ())
    err.entry (string ("Open '") + file + "' failed");
}

Lexer::~Lexer ()
{
  if (in.bad ())
    err.entry (string ("There were trouble parsing '") + file  + "'");
  if (&in != &cin)
    delete &in;
}
