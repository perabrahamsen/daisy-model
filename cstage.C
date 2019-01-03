// cstage.C --- Alternative crop stage definitions.
// 
// Copyright 1996-2001, 2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2005 KVL.
// Copyrigth 2018 KU.
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

#include "cstage.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "memutils.h"
#include "submodeler.h"
#include "check.h"
#include "log.h"

// The 'cstage' component.

const char *const CStage::component = "cstage";

symbol 
CStage::library_id () const
{
  static const symbol id (component);
  return id;
}

CStage::CStage (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

CStage::~CStage ()
{ }

static struct CStageInit : public DeclareComponent 
{
  CStageInit ()
    : DeclareComponent (CStage::component, "\
Crop phenological stage.\n\
The 'default' crop model uses a continius development stage [-1:2]\n	\
in order to track the phenological age of a crop.\n\
The 'cstage' library allows translation of the internal model to\n\
other phenological models.")
  { }
} CStage_init;

// The 'table' model.

struct CStageTable : public CStage
{
  double current_DS;
  double current_value;
  
  struct Entry
  {
    const double DS;		// Daisy DS stage.
    const double value;		// Stage in other model.
    const symbol message;	// Message to display when reaching this stage.

    static void load_syntax (Frame& frame)
    {
      frame.declare ("DS", "DS", Check::DS (), Attribute::Const, "\
Daisy development stage where this stage starts.");
      frame.declare ("value", Attribute::Unknown (), Check::none (),
		     Attribute::Const, "\
Numeric value of stage in other model.");
      frame.declare_string ("message", Attribute::OptionalConst, "\
Message to display when entering this stage.");
      frame.order ("DS", "value", "message");
    }
  Entry (const Block& al)
      : DS (al.number ("DS")),
	value (al.number ("value")),
	message (al.name ("message", Attribute::Unknown ()))
    { }
  };
  auto_vector<const Entry*> table;
  
  // Simulation.
  void tick (const double DS, Treelog& msg)
  {
    if (DS < current_DS)
      msg.message ("Phenology reverted");
    for (auto e : table)
      if (current_DS > e->DS)
	continue;
      else if (DS >= e->DS)
	{
	  if (e->message != Attribute::Unknown ())
	    msg.message (e->message.name ());
	  
	  current_value = e->value;
	  break;
	}
    current_DS = DS;
  }
  double stage () const
  { return current_value; }
  void output (Log& log) const
  {
    output_value (current_DS, "DS", log);
    output_value (current_value, "value", log);
  }  
  
  // Create and Destroy.
  CStageTable (const BlockModel& al)
    : CStage (al),
      current_DS (al.number ("DS")),
      current_value (al.number ("value")),
      table (map_submodel_const<Entry> (al, "table"))
  { }
  ~CStageTable ()
  { }
};

static struct CStageTableSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CStageTable (al); }
  CStageTableSyntax ()
    : DeclareModel (CStage::component, "table", "\
A table mapping Daisy DS to other model.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.declare ("DS", "DS", Check::DS (), Attribute::State, "\
Last Daisy development stage.");
      frame.set ("DS", -1.0);
      frame.declare ("value", Attribute::Unknown (), Check::none (),
		     Attribute::State, "\
Current stage in this phenological model.");
    frame.declare_submodule_sequence ("table", Attribute::Const, "\
List of Daisy and other model stages", CStageTable::Entry::load_syntax);
  }
} CStageTable_syntax;

// The 'Daisy' parameterization.

static struct CStageDaisySyntax : public DeclareParam
{
  CStageDaisySyntax ()
    : DeclareParam (CStage::component, "Daisy", "table", "\
DS 0, 1, and 2 have meaning.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("value", -1.0);
    const FrameSubmodel& default_frame = *frame.default_frame ("table");
    boost::shared_ptr<FrameSubmodel> DS0
      (new FrameSubmodelValue (default_frame, Frame::parent_link));
    DS0->set ("DS", 0.0);
    DS0->set ("value", 0.0);
    DS0->set ("message", "Emerging");
    boost::shared_ptr<FrameSubmodel> DS1
      (new FrameSubmodelValue (default_frame, Frame::parent_link));
    DS1->set ("DS", 1.0);
    DS1->set ("value", 1.0);
    DS1->set ("message", "Flowering");
    boost::shared_ptr<FrameSubmodel> DS2
      (new FrameSubmodelValue (default_frame, Frame::parent_link));
    DS2->set ("DS", 2.0);
    DS2->set ("value", 2.0);
    DS2->set ("message", "Ripe");
    std::vector<boost::shared_ptr<const FrameSubmodel>/**/>
      sequence;
    sequence.push_back (DS0);
    sequence.push_back (DS1);
    sequence.push_back (DS2);
    frame.set ("table", sequence);
  }
} CStageDaisy_syntax;

// The 'BBCH' base parameterization.

static struct CStageBBCHSyntax : public DeclareParam
{
  CStageBBCHSyntax ()
    : DeclareParam (CStage::component, "BBCH", "table", "\
Parent paramterization for all BBCH derived cropstage definitions.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("value", 0.0);
  }
} CStageBBCH_syntax;

// cstage.C ends here.
