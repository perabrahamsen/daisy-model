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
#include "mathlib.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>

using namespace std;

namespace Assertion
{
  vector<Treelog*> logs;
}

void 
Assertion::message (const string& msg)
{
  if (logs.size () == 0)
    cout << msg;

  for (unsigned int i = 0; i < logs.size (); i++)
    {
      logs[i]->message (msg);
      logs[i]->flush ();
    }
}

void 
Assertion::error (const string& msg)
{
  if (logs.size () == 0)
    cerr << msg;

  for (unsigned int i = 0; i < logs.size (); i++)
    {
      logs[i]->error (msg);
      logs[i]->flush ();
    }
}

void 
Assertion::warning (const string& msg)
{
  if (logs.size () == 0)
    cerr << msg;

  for (unsigned int i = 0; i < logs.size (); i++)
    {
      logs[i]->warning (msg);
      logs[i]->flush ();
    }
}

void 
Assertion::debug (const string& msg)
{
  if (logs.size () == 0)
    cerr << msg;

  for (unsigned int i = 0; i < logs.size (); i++)
    {
      logs[i]->debug (msg);
      logs[i]->flush ();
    }
}

void 
Assertion::failure (const char* file, int line, const char* fun,
		    const char* test)
{
  ostringstream tmp;
  tmp << file << ":" << line << ": assertion '" << test << "' failed";
  if (fun)
    tmp << " in " << fun;
  
  error (tmp.str ());
  throw 3;
}

void 
Assertion::bug (const char* file, int line, const char* fun,
		const string& msg)
{
  ostringstream tmp;
  tmp << file << ":" << line << ": Daisy bug: '" << msg << "'";
  if (fun)
    tmp << " in " << fun;

  error (tmp.str ());
}

void 
Assertion::warning (const char* file, int line, const char* fun,
		    const string& msg)
{
  ostringstream tmp;
  tmp << file << ":" << line << ": ";
  if (fun)
    tmp << "(" << fun << ") ";
  tmp << "warning: " << msg;

  warning (tmp.str ());
}

void 
Assertion::panic (const char* file, int line, const char* fun,
		  const string& msg)
{
  bug (file, line, fun, msg);
  throw 3;
}

void 
Assertion::non_negative (const char* file, int line, const char* fun,
			 const std::vector<double>& v)
{
  for (unsigned int i = 0; i < v.size (); i++)
    if (v[i] < 0 || !isfinite (v[i]))
      {
	ostringstream tmp;
	tmp << "v[" << i << "] >= 0";
	Assertion::failure (file, line, fun, tmp.str ().c_str ());
      }
}

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
