// submodel.h  --- A registry of submodels.
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


#ifndef SUBMODEL_H
#define SUBMODEL_H

#include <string>
#include <vector>

using namespace std;

class Syntax;
class AttributeList;

class Submodel
{
public:
  typedef void (*load_fun) (Syntax&, AttributeList&);
  
  static void all (vector<string>& entries);
  static void load_syntax (const string& model, Syntax&, AttributeList&);
  static bool registered (const string& submodel);

  class Register
  {
  public:
      Register (const string& name, load_fun fun);
      ~Register ();
  };

  Submodel ();
  ~Submodel ();
};

#endif // SUBMODEL_H
