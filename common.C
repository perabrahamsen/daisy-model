// common.C --- Portability hacks.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "common.h"
#include "message.h"

#if 0 //def BORLAND_ASSERT
extern "C"
{
  void _RTLENTRY _EXPFUNC _assert(char * __cond, char * __file, int __line)
  {
    CERR << __file << ":" << __line << ": '" << __cond 
	 << "' assertion failed\n";
    exit (1);
  }
}
#endif

// common.C ends here.
