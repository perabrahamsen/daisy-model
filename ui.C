// ui.C -- Top level user interface.
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

#define BUILD_DLL

#include "ui.h"
#include "toplevel.h"
#include "treelog_text.h"
#include "librarian.h"
#include "block_model.h"
#include "assertion.h"
#include "frame.h"

#if defined (__MINGW32__) || defined (_MSC_VER)
#include <windows.h>
#endif // win32 API

// UI

const char *const UI::component = "ui";

symbol
UI::library_id () const
{
  static const symbol id (component);
  return id;
}

void
UI::set_low_priority () const
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  (void) SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS); 
#endif // win32 API
}

UI::UI (const BlockModel& al)
  : name (al.type_name ())
{ }

UI::UI (const char *const id)
  : name (id)
{ }

UI::~UI ()
{ }

static struct UIInit : public DeclareComponent 
{
  UIInit ()
    : DeclareComponent (UI::component, "\
Top level user interface.")
  { }
} UI_init;

// UIProgress

void 
UIProgress::attach (Toplevel& toplevel)
{ 
  boost::shared_ptr<Treelog> progress (new TreelogProgress);
  toplevel.add_treelog (progress); 
}

void 
UIProgress::run (Toplevel& toplevel)
{ 
  set_low_priority ();

  switch (toplevel.state ())
    {
    case Toplevel::is_unloaded:
      toplevel.usage ();
      /* Not reached*/
    case Toplevel::is_uninitialized:
      toplevel.initialize ();
      /* Fallthrough */
    case Toplevel::is_ready:
      toplevel.run ();
      /* Fallthrough */
    case Toplevel::is_done:
      throw EXIT_SUCCESS;
    case Toplevel::is_running:
      toplevel.error ("Aborted while simulation was running");
      throw EXIT_FAILURE;
    case Toplevel::is_error:
      throw EXIT_FAILURE;
    }
  daisy_notreached ();
}

void 
UIProgress::failure (Toplevel&)
{ }

bool 
UIProgress::running () const
{ return true; }

UIProgress::UIProgress ()
  : UI ("progress")
{ }

UIProgress::UIProgress (const BlockModel& al)
  : UI (al)
{ }

UIProgress::~UIProgress ()
{ }

static struct UIProgressSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIProgress (al); }

  UIProgressSyntax ()
    : DeclareModel (UI::component, "progress", 
               "Write progress on standard output (or standard error).\n\
\n\
This is useful when starting the program from a text terminal, or from\n\
inside another program such as an editor that can capture the output.")
  { }
  void load_frame (Frame& frame) const
  {

  }
} UIProgress_syntax;

// UINone

void 
UINone::attach (Toplevel&)
{ }

void 
UINone::run (Toplevel& toplevel)
{
  set_low_priority ();

  switch (toplevel.state ())
    {
    case Toplevel::is_unloaded:
      toplevel.usage ();
      /* Not reached*/
    case Toplevel::is_uninitialized:
      toplevel.initialize ();
      /* Fallthrough */
    case Toplevel::is_ready:
      toplevel.run ();
      /* Fallthrough */
    case Toplevel::is_done:
      throw EXIT_SUCCESS;
    case Toplevel::is_running:
      toplevel.error ("Aborted while simulation was running");
      throw EXIT_FAILURE;
    case Toplevel::is_error:
      throw EXIT_FAILURE;
    }
  daisy_notreached ();
}

void 
UINone::failure (Toplevel&)
{ }

bool 
UINone::running () const
{ return true; }

UINone::UINone ()
  : UI ("none")
{ }

UINone::UINone (const BlockModel& al)
  : UI (al)
{ }

UINone::~UINone ()
{ }

static struct UINoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UINone (al); }

  UINoneSyntax ()
    : DeclareModel (UI::component, "none", 
               "No user unterface.\n\
\n\
This is useful when running from a batch program, or as a component in\n\
a larger system.")
  { }
  void load_frame (Frame& frame) const
  {

  }
} UINone_syntax;

// ui.C ends here.
