// volume.C - a subset of 3D space.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "volume.h"
#include "block.h"

template<>
BuildBase* Librarian<Volume>::content = NULL;

const char *const Volume::description = "\
A subset of 3D space.";

Volume*
Volume::build_obsolete (Block& al)
{
  Volume *const vol = Librarian<Volume>::build_item (al, "volume");
  daisy_assert (vol);
  if (al.check ("from"))
    {
      const double from = al.number ("from");
      if (from < 0)
        vol->limit_top (from);
    }
  if (al.check ("to"))
    {
      const double to = al.number ("to");
      if (to < 0)
        vol->limit_bottom (to);
    }
  return vol;
}

Volume::Volume (Block& al)
  : name (al.identifier ("type"))
{ }

Volume::~Volume ()
{ }

// volume.C ends here.
