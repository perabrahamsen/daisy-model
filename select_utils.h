// select_utils.h --- Utilities for log selections.
// 
// copyright 2006 Per Abrahamsen and KVL.
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

class Border;
class Treelog;
class Syntax; 
class AttributeList;
class Block;

namespace SelectUtil
{
  struct Interval
  {
    // Content.
    double from;
    double to;

    // Create and Destroy.
    void initialize (double default_from, double default_to);
#if 0
    bool check_border (const Border& border, 
                       const double default_from, const double default_to,
                       Treelog& msg) const;
#endif
    static void load_syntax (Syntax&, AttributeList&);
    Interval (Block& al);
    ~Interval ();
  };
}

// select_utils.C ends here.
