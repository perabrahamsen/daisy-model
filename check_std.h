// check_std.h -- Check validity of alist members.
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


#ifndef CHECK_STD_H
#define CHECK_STD_H

#include "check.h"

class NonNegative : public Check
{ 
  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  static const Check& value ();
private:
  friend class CheckManager;
};

static class CheckManager
{
  class Content;
public:
  static Content* content;
  CheckManager ();
  ~CheckManager ();
} check_manager;

#endif // CHECK_STD_H
