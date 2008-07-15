// vernalization.h -- Default crop vernalization submodel.
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

#ifndef VERNALIZATION_H
#define VERNALIZATION_H

class AttributeList;
class Syntax;
class Log;
class PLF;

class Vernalization 
{
  // Parameters.
public:
  const bool required;
private:
  const double DSLim;		// Max DS without vernalization
  const double TaLim;		// Vernalization temp threshold

  // State.
private:
  double TaSum;		// Vernalization T-sum requirement

  // Simulation.
public:
  void operator () (double Ta, double& DS);
  void output (Log& log) const;

  // Create and Destroy.
public:
  static const AttributeList& no_vernalization ();
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Vernalization (const AttributeList&);
  ~Vernalization ();
};

#endif // VERNALIZATION_H
