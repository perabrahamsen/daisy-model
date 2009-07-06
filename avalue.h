// avalue.h -- Attribute values.
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

#ifndef AVALUE_H
#define AVALUE_H

#include "symbol.h"
#include "value.h"
#include <vector>
#include <boost/shared_ptr.hpp>

class PLF;
class FrameModel;
class FrameSubmodel;
class Metalib;

struct AValue
{
  struct Scalar
  {
    double number;
    symbol name;
    Scalar (double d, symbol s);
    bool operator== (const Scalar& s) const;
  };

  const union
  {
    double number;
    const Scalar* scalar;
    symbol* name;
    bool flag;
    const PLF* plf;
    const FrameModel* model;
    const FrameSubmodel* submodel;
    int integer;
    const std::vector<double>* number_sequence;
    const std::vector<symbol>* name_sequence;
    const std::vector<bool>* flag_sequence;
    const std::vector<int>* integer_sequence;
    const std::vector<boost::shared_ptr<const PLF>/**/>* plf_sequence;
    std::vector<boost::shared_ptr<const FrameModel>/**/>* model_sequence;
    std::vector<boost::shared_ptr<const FrameSubmodel>/**/>* submodel_sequence;
  };
  Value::type type;
  bool is_sequence;
  int* ref_count;

  bool subset (const Metalib&, const AValue& other) const;

  void expect (const symbol key, Value::type expected) const;
  void singleton (const symbol key) const;
  void sequence (const symbol key) const;

  // Variable
  AValue (const symbol v, int);
  AValue (double v);
  AValue (double v, const symbol s);
  AValue (const symbol v);
  AValue (bool v);
  AValue (const PLF& v);
  AValue (const FrameModel& f);
  AValue (const FrameSubmodel& f);
  AValue (int v);
  AValue (const std::vector<double>& v);
  AValue (const std::vector<symbol>& v);
  static std::vector<symbol>* 
  /**/ new_symbol_vector (const std::vector<std::string>& org);
  AValue (const std::vector<std::string>& v);
  AValue (const std::vector<bool>& v);
  AValue (const std::vector<int>& v);
  AValue (const std::vector<boost::shared_ptr<const PLF>/**/>& v);
  AValue (const std::vector<boost::shared_ptr<const FrameModel>/**/>& v);
  AValue (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& v);
  AValue ();
  AValue (const AValue& v);
  AValue& operator = (const AValue& v);
  ~AValue ();
  void cleanup ();
};

#endif // AVALUE_H
