// message.h -- console messages

#ifndef MESSAGE_H
#define MESSAGE_H

#include "common.h"

#if defined (MISSING_OSTREAM)
#include <iostream.h>
#elif defined (BROKEN_HEADERS)
#include <ostream.h>
#else
#include <ostream>
#endif

#define CERR (DebugMessages::error ())
#define CWAR (DebugMessages::warning ())
#define COUT (DebugMessages::message ()) 

class DebugMessages
{
public: 
  static ostream& message ();
  static ostream& warning ();
  static ostream& error ();
private:
  DebugMessages ();
};

#endif // MESSAGE_H
