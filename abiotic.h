// abiotic.h -- Standard abiotic factors.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef ABIOTIC_H
#define ABIOTIC_H

class BlockModel;
class Frame;

namespace Abiotic
{
  double f_h (double h);
  double f_T2 (double T);
  double f_T0 (double T);

  double find_T_scale (const BlockModel& al);
  double find_SMB_scale (const BlockModel& al);

  void load_frame (Frame&);

}

#endif // ABIOTIC_H
