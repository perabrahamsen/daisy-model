// uifilter.C -- Abstract user interface details.
// 
// Copyright 2012 KU.
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

#include "uifilter.h"
#include "block_model.h"
#include "librarian.h"

// The 'uifilter' component.

const char *const
UIFilter::component = "uifilter";

symbol 
UIFilter::library_id () const
{ 
  static const symbol name = component;
  return name;
}

UIFilter::UIFilter (const BlockModel& al)
  : name (al.type_name ())
{ }

UIFilter::~UIFilter ()
{ }

static struct UIFilterInit : public DeclareComponent 
{
  UIFilterInit ()
    : DeclareComponent (UIFilter::component, "\
Presentation of data in the user interface.")
  { }
} UIFilter_init;

// The 'default' uifilter model.

struct UIFilterStandard : UIFilter
{
  UIFilterStandard (const BlockModel& al)
    : UIFilter (al)
  { }
  ~UIFilterStandard ()
  { }
};

static struct UIFilter_StandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIFilterStandard (al); }
  UIFilter_StandardSyntax ()
    : DeclareModel (UIFilter::component, "default", "\
Standard filter for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIFilterStandard_syntax;

// The 'uigroup' component.

class UIGroup : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;

  // Create and Destroy.
protected:
  explicit UIGroup (const BlockModel&);
public:
  virtual ~UIGroup ();
};

const char *const
UIGroup::component = "uigroup";

symbol 
UIGroup::library_id () const
{ 
  static const symbol name = component;
  return name;
}

UIGroup::UIGroup (const BlockModel& al)
  : name (al.type_name ())
{ }

UIGroup::~UIGroup ()
{ }

static struct UIGroupInit : public DeclareComponent 
{
  UIGroupInit ()
    : DeclareComponent (UIGroup::component, "\
User interface information about a group of parameters.")
  { }
} UIGroup_init;

// The 'default' uigroup model.

struct UIGroupStandard : UIGroup
{
  UIGroupStandard (const BlockModel& al)
    : UIGroup (al)
  { }
  ~UIGroupStandard ()
  { }
};

static struct UIGroup_StandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIGroupStandard (al); }
  UIGroup_StandardSyntax ()
    : DeclareModel (UIGroup::component, "default", "\
Standard group for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIGroupStandard_syntax;

// The 'uiparameter' component.

class UIParameter : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;

  // Create and Destroy.
protected:
  explicit UIParameter (const BlockModel&);
public:
  virtual ~UIParameter ();
};

const char *const
UIParameter::component = "uiparameter";

symbol 
UIParameter::library_id () const
{ 
  static const symbol name = component;
  return name;
}

UIParameter::UIParameter (const BlockModel& al)
  : name (al.type_name ())
{ }

UIParameter::~UIParameter ()
{ }

static struct UIParameterInit : public DeclareComponent 
{
  UIParameterInit ()
    : DeclareComponent (UIParameter::component, "\
User interface information about a parameter.")
  { }
} UIParameter_init;

// The 'default' uiparameter model.

struct UIParameterStandard : UIParameter
{
  UIParameterStandard (const BlockModel& al)
    : UIParameter (al)
  { }
  ~UIParameterStandard ()
  { }
};

static struct UIParameter_StandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIParameterStandard (al); }
  UIParameter_StandardSyntax ()
    : DeclareModel (UIParameter::component, "default", "\
Standard parameter for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIParameterStandard_syntax;

// uifilter.C ends here.
