// check_std.C -- Check validity of alist members.
// 
// Copyright 2001 Per Abrahamsen and KVL.
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


#include "check_std.h"
#include "common.h"

class CheckManager::Content
{
public:
  int count;
  Check* non_negative;
};

CheckManager::Content* CheckManager::content = NULL;

CheckManager::CheckManager ()
{
  if (content)
    content->count++;
  else 
    {
      content = new Content ();
      content->count = 1;
      content->non_negative = new NonNegative ();
    }
}

CheckManager::~CheckManager ()
{
  assert (content);
  assert (content->count > 0);
  content->count--;
  if (content->count == 0)
    {
      delete content->non_negative;
      delete content;
      content = NULL;
    }
}

void
NonNegative::check (const double value) const throw (string)
{
  if (value < 0.0)
    throw string ("Negative value not permitted");
}

const Check&
NonNegative::value ()
{ 
  assert (CheckManager::content);
  assert (CheckManager::content->non_negative);
  return *(CheckManager::content->non_negative); 
}

