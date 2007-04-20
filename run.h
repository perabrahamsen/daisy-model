// run.h -- Top level user interface, as seen from the program.
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

#ifndef RUN_H
#define RUN_H

#include "model.h"

class Run : public Model
{
  // Use.
public:
  virtual bool running () const = 0;
  virtual void set_progress (double value) = 0; // -1 (unknown) or [0:1].

  // Create and Destroy.
private:
  explicit Run (const Run&);      // Disable.
  Run& operator= (const Run&);    // Disable.
protected:
  Run ();
  ~Run ();
};

#endif // RUN_H
