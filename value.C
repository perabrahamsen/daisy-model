// value.C -- Attribute values.
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

#define BUILD_DLL

#include "value.h"
#include "attribute.h"
#include "assertion.h"

int
Value::size () const
{ return Attribute::Singleton; }

const std::vector<symbol>& 
Value::cite () const
{ 
  static const std::vector<symbol> empty;
  return empty;
}

symbol
Value::description () const
{ return Attribute::None (); }

double 
Value::number () const
{
  if (is_reference ())
    throw "Unhandled reference: '" + name () + "'";
  daisy_notreached ();
}

symbol 
Value::name () const
{ daisy_notreached (); }

bool 
Value::flag () const
{ daisy_notreached (); }

boost::shared_ptr<const PLF> 
Value::plf () const
{ daisy_notreached (); }

boost::shared_ptr<const FrameModel>
Value::model () const
{ daisy_notreached (); }

boost::shared_ptr<const FrameSubmodel>
Value::submodel () const
{ daisy_notreached (); }

int 
Value::integer () const
{ daisy_notreached (); }

const std::vector<double>& 
Value::number_sequence () const
{ daisy_notreached (); }

const std::vector<symbol>& 
Value::name_sequence () const
{ daisy_notreached (); }

const std::vector<bool>& 
Value::flag_sequence () const
{ daisy_notreached (); }

const std::vector<int>& 
Value::integer_sequence () const
{ daisy_notreached (); }

const std::vector<boost::shared_ptr<const PLF>/**/>&
Value::plf_sequence () const
{ daisy_notreached (); }

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
Value::model_sequence () const
{ daisy_notreached (); }

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
Value::submodel_sequence () const
{ daisy_notreached (); }

bool 
Value::is_reference () const
{ return false; }

Value::Value ()
{ }

Value::~Value ()
{ }

int
ValueReference::size () const
{ return -1; }

symbol 
ValueReference::name () const
{ return value; }

bool 
ValueReference::is_reference () const
{ return true; }

ValueReference::ValueReference (const symbol v)
  : value (v)
{ }

double 
ValueNumber::number () const
{ return value; }

ValueNumber::ValueNumber (double v)
  : Value (),
    value (v)
{ }

symbol
ValueNumberDescription::description () const
{ return desc; }

ValueNumberDescription::ValueNumberDescription (const double v, 
                                                const symbol description)
  : ValueNumber (v),
    desc (description)
{ }

const std::vector<symbol>& 
ValueNumberCite::cite () const
{ return citations; }

ValueNumberCite::ValueNumberCite (const double v, const symbol d, 
                                  const std::vector<symbol>& c)
  : ValueNumberDescription (v, d),
    citations (c)
{ }

double 
ValueScalar::number () const
{ return number_; }

symbol
ValueScalar::name () const
{ return name_; }

ValueScalar::ValueScalar (double v, const symbol s)
  : number_ (v),
    name_ (s)
{ }

symbol
ValueString::name () const
{ return value; }

ValueString::ValueString (const symbol v)
  : value (v)
{ }

bool 
ValueBoolean::flag () const
{ return value; }

ValueBoolean::ValueBoolean (const bool v)
  : value (v)
{ }

boost::shared_ptr<const PLF>
ValuePLF::plf () const
{ return value; }

ValuePLF::ValuePLF (boost::shared_ptr<const PLF> v)
  : value (v)
{ }

symbol
ValuePLFDescription::description () const
{ return desc; }

ValuePLFDescription::ValuePLFDescription (boost::shared_ptr<const PLF> v, 
					  const symbol description)
  : ValuePLF (v),
    desc (description)
{ }

const std::vector<symbol>& 
ValuePLFCite::cite () const
{ return citations; }

ValuePLFCite::ValuePLFCite (boost::shared_ptr<const PLF> v, const symbol d, 
			    const std::vector<symbol>& c)
  : ValuePLFDescription (v, d),
    citations (c)
{ }

boost::shared_ptr<const FrameModel>
ValueModel::model () const
{ return value; }

ValueModel::ValueModel (boost::shared_ptr<const FrameModel> v)
  : value (v)
{ }

boost::shared_ptr<const FrameSubmodel>
ValueSubmodel::submodel () const
{ return value; }

ValueSubmodel::ValueSubmodel (boost::shared_ptr<const FrameSubmodel> v)
  : value (v)
{ }

int 
ValueInteger::integer () const
{ return value; }

ValueInteger::ValueInteger (const int v)
  : value (v)
{ }

int
ValueNumberSeq::size () const
{ return value.size (); }

const std::vector<double>& 
ValueNumberSeq::number_sequence () const
{ return value; }

ValueNumberSeq::ValueNumberSeq (const std::vector<double>& v)
  : value (v)
{ }

int
ValueStringSeq::size () const
{ return value.size (); }

const std::vector<symbol>& 
ValueStringSeq::name_sequence () const
{ return value; }

ValueStringSeq::ValueStringSeq (const std::vector<symbol>& v)
  : value (v)
{ }

int
ValueBooleanSeq::size () const
{ return value.size (); }

const std::vector<bool>& 
ValueBooleanSeq::flag_sequence () const
{ return value; }

ValueBooleanSeq::ValueBooleanSeq (const std::vector<bool>& v)
  : value (v)
{ }

int
ValueIntegerSeq::size () const
{ return value.size (); }

const std::vector<int>& 
ValueIntegerSeq::integer_sequence () const
{ return value; }

ValueIntegerSeq::ValueIntegerSeq (const std::vector<int>& v)
  : value (v)
{ }

int
ValuePLFSeq::size () const
{ return value.size (); }

const std::vector<boost::shared_ptr<const PLF>/**/>& 
ValuePLFSeq::plf_sequence () const
{ return value; }

ValuePLFSeq::ValuePLFSeq (const std::vector<boost::shared_ptr<const PLF>/**/>& v)
  : value (v)
{ }

int
ValueModelSeq::size () const
{ return value.size (); }

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
ValueModelSeq::model_sequence () const
{ return value; }

ValueModelSeq::ValueModelSeq (const std::vector<boost::shared_ptr<const FrameModel>/**/>& v)
  : value (v)
{ }

int
ValueSubmodelSeq::size () const
{ return value.size (); }

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
ValueSubmodelSeq::submodel_sequence () const
{ return value; }

ValueSubmodelSeq::ValueSubmodelSeq (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& v)
  : value (v)
{ }

// value.C ends here.
