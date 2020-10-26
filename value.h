// value.h -- Attribute values.
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

#ifndef VALUE_H
#define VALUE_H

#include "symbol.h"
#include <vector>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>

class PLF;
class FrameModel;
class FrameSubmodel;

class Value : public boost::noncopyable
{
public:
  virtual int size () const;
  virtual const std::vector<symbol>& cite () const;
  virtual symbol description () const;
  virtual double number () const;
  virtual symbol name () const;
  virtual bool flag () const;
  virtual boost::shared_ptr<const PLF> plf () const;
  virtual boost::shared_ptr<const FrameModel> model () const;
  virtual boost::shared_ptr<const FrameSubmodel> submodel () const;
  virtual int integer () const;
  virtual const std::vector<double>& number_sequence () const;
  virtual const std::vector<symbol>& name_sequence () const;
  virtual const std::vector<bool>& flag_sequence () const;
  virtual const std::vector<int>& integer_sequence () const;
  virtual const std::vector<boost::shared_ptr<const PLF>/**/>& plf_sequence () const;
  virtual const std::vector<boost::shared_ptr<const FrameModel>/**/>& model_sequence () const;
  virtual const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& submodel_sequence () const;
  virtual bool is_reference () const;
protected:
  Value ();
public: 
  virtual ~Value ();
};

class ValueNumber : public Value
{
  const double value;
  double number () const;
public:
  ValueNumber (const double v);
};

class ValueNumberDescription : public ValueNumber
{
  const symbol desc;
  symbol description () const;
public:
  ValueNumberDescription (const double v, const symbol description);
};

class ValueNumberCite : public ValueNumberDescription
{
  const std::vector<symbol> citations;
  const std::vector<symbol>& cite () const;
public:
  ValueNumberCite (const double v, const symbol d, 
                   const std::vector<symbol>& c);
};

class ValueReference : public Value
{
  const symbol value;
  int size () const;
  symbol name () const;
  bool is_reference () const;
public:
  ValueReference (const symbol v);
};

class ValueScalar : public Value
{
  const double number_;
  const symbol name_;
  double number () const;
  symbol name () const;
public:
  ValueScalar (const double v, const symbol s);
};

class ValueString : public Value
{
  const symbol value;
  symbol name () const;
public:
  ValueString (const symbol v);
};

class ValueBoolean : public Value
{
  const bool value;
  bool flag () const;
public:
  ValueBoolean (const bool v);
};

class ValuePLF : public Value
{
  boost::shared_ptr<const PLF> value;
  boost::shared_ptr<const PLF> plf () const;
public:
  ValuePLF (boost::shared_ptr<const PLF> v);
};

class ValuePLFDescription : public ValuePLF
{
  const symbol desc;
  symbol description () const;
public:
  ValuePLFDescription (boost::shared_ptr<const PLF> v, const symbol description);
};

class ValuePLFCite : public ValuePLFDescription
{
  const std::vector<symbol> citations;
  const std::vector<symbol>& cite () const;
public:
  ValuePLFCite (boost::shared_ptr<const PLF> v, const symbol d, 
		const std::vector<symbol>& c);
};

class ValueModel : public Value
{
  boost::shared_ptr<const FrameModel> value;
  boost::shared_ptr<const FrameModel> model () const;
public:
  ValueModel (boost::shared_ptr<const FrameModel> f);
};

class ValueSubmodel : public Value
{
  boost::shared_ptr<const FrameSubmodel> value;
  boost::shared_ptr<const FrameSubmodel> submodel () const;
public:
  ValueSubmodel (boost::shared_ptr<const FrameSubmodel> f);
};

class ValueInteger : public Value
{
  const int value;
  int integer () const;
public:
  ValueInteger (const int v);
};

class ValueNumberSeq : public Value
{
  const std::vector<double> value;
  int size () const;
  const std::vector<double>& number_sequence () const;
public:
  ValueNumberSeq (const std::vector<double>& v);
};

class ValueStringSeq : public Value
{
  const std::vector<symbol> value;
  int size () const;
  const std::vector<symbol>& name_sequence () const;
public:
  ValueStringSeq (const std::vector<symbol>& v);
};

class ValueBooleanSeq : public Value
{
  const std::vector<bool> value;
  int size () const;
  const std::vector<bool>& flag_sequence () const;
public:
  ValueBooleanSeq (const std::vector<bool>& v);
};

class ValueIntegerSeq : public Value
{
  const std::vector<int> value;
  int size () const;
  const std::vector<int>& integer_sequence () const;
public:
  ValueIntegerSeq (const std::vector<int>& v);
};

class ValuePLFSeq : public Value
{
  const std::vector<boost::shared_ptr<const PLF>/**/> value;
  int size () const;
  const std::vector<boost::shared_ptr<const PLF>/**/>& plf_sequence () const;
public:
  ValuePLFSeq (const std::vector<boost::shared_ptr<const PLF>/**/>& v);
};

class ValueModelSeq : public Value
{
  const std::vector<boost::shared_ptr<const FrameModel>/**/> value;
  int size () const;
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& model_sequence () const;
public:
  ValueModelSeq (const std::vector<boost::shared_ptr<const FrameModel>/**/>& v);
};

class ValueSubmodelSeq : public Value
{
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/> value;
  int size () const;
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& submodel_sequence () const;
public:
  ValueSubmodelSeq (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& v);
};

#endif // VALUE_H
