// val.C -- Attribute values.
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

#include "val.h"
#include "assertion.h"

double 
Val::number () const
{ daisy_notreached (); }

symbol 
Val::name () const
{ daisy_notreached (); }

bool 
Val::flag () const
{ daisy_notreached (); }

const PLF& 
Val::plf () const
{ daisy_notreached (); }

const FrameModel& 
Val::model () const
{ daisy_notreached (); }

const FrameSubmodel& 
Val::submodel () const
{ daisy_notreached (); }

int 
Val::integer () const
{ daisy_notreached (); }

const std::vector<double>& 
Val::number_sequence () const
{ daisy_notreached (); }

const std::vector<symbol>& 
Val::name_sequence () const
{ daisy_notreached (); }

const std::vector<bool>& 
Val::flag_sequence () const
{ daisy_notreached (); }

const std::vector<int>& 
Val::integer_sequence () const
{ daisy_notreached (); }

const std::vector<boost::shared_ptr<const PLF>/**/>&
Val::plf_sequence () const
{ daisy_notreached (); }

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
Val::model_sequence () const
{ daisy_notreached (); }

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
Val::submodel_sequence () const
{ daisy_notreached (); }

Val::Val ()
{ }

Val::~Val ()
{ }

ValReference::ValReference (const symbol v)
  : value (v)
{ }

double 
ValNumber::number () const
{ return value; }

ValNumber::ValNumber (double v)
  : Val (),
    value (v)
{ }

double 
ValScalar::number () const
{ return number_; }

symbol
ValScalar::name () const
{ return name_; }

ValScalar::ValScalar (double v, const symbol s)
  : number_ (v),
    name_ (s)
{ }

symbol
ValString::name () const
{ return value; }

ValString::ValString (const symbol v)
  : value (v)
{ }

bool 
ValBoolean::flag () const
{ return value; }

ValBoolean::ValBoolean (const bool v)
  : value (v)
{ }

const PLF& 
ValPLF::plf () const
{ return *value; }

ValPLF::ValPLF (boost::shared_ptr<const PLF> v)
  : value (v)
{ }

const FrameModel& 
ValObject::model () const
{ return *value; }

ValObject::ValObject (boost::shared_ptr<const FrameModel> v)
  : value (v)
{ }

const FrameSubmodel& 
ValAList::submodel () const
{ return *value; }

ValAList::ValAList (boost::shared_ptr<const FrameSubmodel> v)
  : value (v)
{ }

int 
ValInteger::integer () const
{ return value; }

ValInteger::ValInteger (const int v)
  : value (v)
{ }

const std::vector<double>& 
ValNumberSeq::number_sequence () const
{ return value; }

ValNumberSeq::ValNumberSeq (const std::vector<double>& v)
  : value (v)
{ }

const std::vector<symbol>& 
ValStringSeq::name_sequence () const
{ return value; }

ValStringSeq::ValStringSeq (const std::vector<symbol>& v)
  : value (v)
{ }

const std::vector<bool>& 
ValBooleanSeq::flag_sequence () const
{ return value; }

ValBooleanSeq::ValBooleanSeq (const std::vector<bool>& v)
  : value (v)
{ }

const std::vector<int>& 
ValIntegerSeq::integer_sequence () const
{ return value; }

ValIntegerSeq::ValIntegerSeq (const std::vector<int>& v)
  : value (v)
{ }

const std::vector<boost::shared_ptr<const PLF>/**/>& 
ValPLFSeq::plf_sequence () const
{ return value; }

ValPLFSeq::ValPLFSeq (const std::vector<boost::shared_ptr<const PLF>/**/>& v)
  : value (v)
{ }

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
ValObjectSeq::model_sequence () const
{ return value; }

ValObjectSeq::ValObjectSeq (const std::vector<boost::shared_ptr<const FrameModel>/**/>& v)
  : value (v)
{ }

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
ValAListSeq::submodel_sequence () const
{ return value; }

ValAListSeq::ValAListSeq (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& v)
  : value (v)
{ }

// val.C ends here.
