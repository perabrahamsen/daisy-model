// common.C --- Portability hacks.

#include "common.h"
#include "options.h"

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

// common.C ends here.
