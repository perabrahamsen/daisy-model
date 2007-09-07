// ui_Qt.C -- Qt based top level user interface.
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

#include "ui_Qt.h"
#include "toplevel.h"
#include "librarian.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"

#include <QtGui/QApplication>
#include <QtCore/QMetaType>

#ifdef SELECTABLE_UI
struct UIQt::Content
{
  static struct Registrate 
  {
    Registrate ()
    { 
      qRegisterMetaType<std::string>("std::string");
      qRegisterMetaType<Toplevel::state_t>("Toplevel::state_t");
      Toplevel::add_command_line_parser (setup); 
    }
    ~Registrate ()
    { 
      Toplevel::remove_command_line_parser (); 
#if 0
      delete UIQt::content;
      UIQt::content = NULL;
#endif
    }
  } registrate;

  static void setup (int& argc, char**& argv)
  {
    if (UIQt::content)
      daisy_panic ("Can't parse Qt command line twice");

    UIQt::content = new UIQt::Content (argc, argv);
  }

  QApplication app;

  Content (int& argc, char**& argv)
    : app (argc, argv)
  { 
    app.setApplicationName ("Daisy");
    app.setOrganizationDomain ("life.ku.dk");
    app.setOrganizationName ("University of Copenhagen");
  }
};

UIQt::Content::Registrate UIQt::Content::registrate;

UIQt::Content* UIQt::content = NULL;

void
UIQt::run_user_interface ()
{ 
  daisy_assert (UIQt::content);
  UIQt::content->app.exec (); 
}

QString
UIQt::application_name () const
{
  daisy_assert (UIQt::content);
  return UIQt::content->app.applicationName (); 
}
#else

QApplication* UIQt::app = NULL;

void
UIQt::set_application (QApplication& a)
{
  daisy_assert (!app);
  app = &a; 
  a.setApplicationName ("Daisy");
  a.setOrganizationDomain ("life.ku.dk");
  a.setOrganizationName ("University of Copenhagen");
  qRegisterMetaType<std::string>("std::string");
  qRegisterMetaType<Toplevel::state_t>("Toplevel::state_t");
}

void
UIQt::run_user_interface ()
{ 
  daisy_assert (app);
  app->exec (); 
}

QString
UIQt::application_name () const
{
  daisy_assert (app);
  return app->applicationName (); 
}
#endif // SELECTABLE_UI

UIQt::UIQt (Block& al)
  : UI (al)
{ }

UIQt::~UIQt ()
{ }

// ui.C ends here.
