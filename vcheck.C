// check.C -- Check validity of arbitrary alist members.
// 
// Copyright 2001, 2003 Per Abrahamsen and KVL.
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

#include "vcheck.h"
#include "syntax.h"
#include "alist.h"
#include "time.h"
#include "tmpstream.h"
#include "assertion.h"
#include <algorithm>

// GCC 2.95 -O2 dislike declaring these classes local.
struct ValidYear : public VCheck
{
  static void validate (int year) throw (string)
  {
    if (!Time::valid (year, 1, 1, 1))
      {
	TmpStream tmp;
	tmp () << year << " is not a valid year";
	throw string (tmp.str ());
      }
  }

  void check (const Syntax& syntax, const AttributeList& alist, 
	      const string& key) const throw (string)
  { 
    daisy_assert (alist.check (key));
    daisy_assert (syntax.lookup (key) == Syntax::Integer);
    daisy_assert (!syntax.is_log (key));

    if (syntax.size (key) == Syntax::Singleton)
      validate (alist.integer (key));
    else
      {
	const vector<int> years = alist.integer_sequence (key);
	for_each (years.begin (), years.end (), validate);
#if 0
	for (unsigned int i = 0; i < years.size (); i++)
	  {
	    const int year = years[i];
	    if (!Time::valid (year, 1, 1, 1))
	      {
		TmpStream tmp;
		tmp () << year << " is not a valid year";
		throw string (tmp.str ());
	      }
	  }
#endif
      }
}
};

const VCheck& 
VCheck::valid_year ()
{
  static ValidYear valid_year;
  return valid_year;
}

VCheck::VCheck ()
{ }

VCheck::~VCheck ()
{ }
