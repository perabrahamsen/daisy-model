// message.C -- console messages

#include "message.h"
#include "options.h"

#include <iostream>

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

