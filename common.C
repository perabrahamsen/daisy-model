// common.C --- Portability hacks.

#include "common.h"
#include "options.h"
#include <iostream>

extern "C" int chdir (const char *path);

#ifdef BORLAND_ASSERT
extern "C"
{
  void _RTLENTRY _EXPFUNC _assert(char * __cond, char * __file, int __line)
  {
    CERR << __file << ":" << __line << ": `" << __cond 
	 << "' assertion failed\n";
    exit (1);
  }
}
#endif

#ifndef __unix
#define MESSAGE_LOG
#endif

#ifdef MESSAGE_LOG
#include <fstream>
ofstream message_log (getenv (Options::log_name)
		      ? getenv (Options::log_name) 
		      : "nul");
#endif

ostream& 
DebugMessages::message ()
{
#ifdef MESSAGE_LOG
  if (getenv (Options::log_name))
    return message_log;
#endif
  return cout; 
}

ostream& 
DebugMessages::warning ()
{ return error (); }

ostream& 
DebugMessages::error ()
{ 
#ifdef MESSAGE_LOG
  if (getenv (Options::log_name))
    return message_log;
#endif

#ifdef USELESS_STDERR
  return cout;
#else
  return cerr; 
#endif
}

DebugMessages::DebugMessages ()
{ assert (false); }

// common.C ends here.
