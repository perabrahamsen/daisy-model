// assertion.C -- Managed assertions.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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

#include "assertion.h"
#include "treelog.h"
#include "tmpstream.h"

#include <vector>
#include <algorithm>

namespace Assertion
{
  vector<Treelog*> logs;
}

void 
Assertion::failure (const char* file, int line, const char* fun,
		    const char* test)
{
  TmpStream tmp;
  tmp () << file << ":" << line << ": assertion '" << test << "' failed";
  if (fun)
    tmp () << " in " << fun;
  
  for (unsigned int i = 0; i < logs.size (); i++)
    {
      logs[i]->error (tmp.str ());
      logs[i]->flush ();
    }
  exit (3);
};

void 
Assertion::bug (const char* file, int line, const char* fun,
		const string& msg)
{
  TmpStream tmp;
  tmp () << file << ":" << line << ": Daisy bug: '" << msg << "'";
  if (fun)
    tmp () << " in " << fun;
  
  for (unsigned int i = 0; i < logs.size (); i++)
    {
      logs[i]->error (tmp.str ());
      logs[i]->flush ();
    }
};

void 
Assertion::panic (const char* file, int line, const char* fun,
		  const string& msg)
{
  bug (file, line, fun, msg);
  exit (3);
};

Assertion::Register::Register (Treelog& log)
  : treelog (log)
{
  for (unsigned int i = 0; i < logs.size (); i++)
    daisy_assert (&log != logs[i]);

  logs.push_back (&log);
}

Assertion::Register::~Register ()
{
  daisy_assert (find (logs.begin (), logs.end (), &treelog) != logs.end ());
  logs.erase (find (logs.begin (), logs.end (), &treelog));
  daisy_assert (find (logs.begin (), logs.end (), &treelog) == logs.end ());
}
