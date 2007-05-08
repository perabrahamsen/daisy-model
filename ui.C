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
#include "block.h"
#include "alist.h"

// UI

const char *const UI::component = "ui";

UI::UI (Block& al)
  : name (al.identifier ("type"))
{ }

UI::UI (const char *const id)
  : name (id)
{ }

UI::~UI ()
{ }

static Librarian UI_init (UI::component, "\
Top level user interface.");

// UIProgress

void 
UIProgress::attach (Toplevel& toplevel)
{ toplevel.add_treelog (new TreelogProgress); }

void 
UIProgress::run (Toplevel&)
{ }

bool 
UIProgress::running () const
{ return true; }

UIProgress::UIProgress ()
  : UI ("progress")
{ }

UIProgress::UIProgress (Block& al)
  : UI (al)
{ }

UIProgress::~UIProgress ()
{ }

static struct UIProgressSyntax
{
  static Model& make (Block& al)
  { return *new UIProgress (al); }

  UIProgressSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
               "Write progress on standard output (or standard error).\n\
\n\
This is useful when starting the program from a text terminal, or from\n\
inside another program such as an editor that can capture the output.");
    Librarian::add_type (UI::component, "progress", alist, syntax, &make);
  }
} UIProgress_syntax;

// UINone

void 
UINone::attach (Toplevel&)
{ }

void 
UINone::run (Toplevel&)
{ }

bool 
UINone::running () const
{ return true; }

UINone::UINone ()
  : UI ("none")
{ }

UINone::UINone (Block& al)
  : UI (al)
{ }

UINone::~UINone ()
{ }

static struct UINoneSyntax
{
  static Model& make (Block& al)
  { return *new UINone (al); }

  UINoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
               "No user unterface.\n\
\n\
This is useful when running from a batch program, or as a component in\n\
a larger system.");
    Librarian::add_type (UI::component, "none", alist, syntax, &make);
  }
} UINone_syntax;

// ui.C ends here.
