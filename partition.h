// partition.h -- Assimilate partioning for the default crop model.
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

#ifndef PARTITION_H
#define PARTITION_H

class PLF;
class AttributeList;
class Syntax;
class Log;

#include <vector>
using namespace std;

class Partition 
{
  // Parameters.
private:
  const PLF& Root;		// Partitioning functions for root
  const PLF& Leaf;		//   leaf, and stem as function of DS
  const PLF& Stem;
public:
  const PLF& RSR;		// Root/Shoot ratio.

  // Utilities.
public:
  void operator () (double DS, double current_RSR,
		    double& f_Leaf, double& f_Stem,
		    double& f_Root, double& f_SOrg);

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Partition (const AttributeList&);
  ~Partition ();
};

#endif // PARTITION_H
