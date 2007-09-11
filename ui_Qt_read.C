// ui_Qt_read.C -- Qt based top level user interface for selecting a program.
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

#include "ui_Qt_read.h"
#include "toplevel.h"
#include "program.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"
#include "memutils.h"
#include "path.h"

#include <QtGui/QVBoxLayout>
#include <QtGui/QMenuBar>
#include <QtGui/QFileDialog>
#include <QtGui/QStatusBar>

#include <map>

void 
UIRead::attach (Toplevel& toplevel)
{
  // Attach menubar.
  QMenuBar* menuBar = qt_main.menuBar ();

  // Create File menu.
  QMenu* fileMenu = menuBar->addMenu ("&File");

  // Open setup.
  QAction* openAction = new QAction ("&Open setup...", this);
  openAction->setStatusTip ("Open a setup file");
  connect (openAction, SIGNAL(triggered()), this, SLOT(open_setup()));
  fileMenu->addAction (openAction);

  // We organize items in a boxes.
  QWidget *const center = new QWidget (&qt_main);
  QVBoxLayout *const layout = new QVBoxLayout (center);
  
  // The top line.
  QHBoxLayout *const top_layout = new QHBoxLayout;
  layout->addLayout (top_layout);

  // The program name.
  qt_name->setToolTip ("The name of the program or simulation to run.");
  QFont font = qt_name->font ();
  font.setBold (true);
  qt_name->setFont (font);
  top_layout->addWidget (qt_name /* , Qt::AlignLeft */);
  top_layout->addStretch ();

  // The file name.
  // (The tooltip is set dynamically.)
  top_layout->addStretch ();
  top_layout->addWidget (qt_file /* , Qt::AlignRight */);

  layout->addStretch ();

  // The program description.
  qt_description->setToolTip ("The description of the selected program.");
  layout->addWidget (qt_description /* , Qt::AlignLeft */);

  // All of this in our central widget.
  qt_main.setCentralWidget (center);

  // Add content.
  daisy_assert (top_level == NULL);
  top_level = &toplevel;
  reset ();

  // Show it all.
  qt_main.show ();
}

void
UIRead::run (Toplevel& toplevel)
{
  daisy_assert (top_level == &toplevel);

  run_user_interface ();       // Start the UI.
}

void
UIRead::reset ()
{
  // Fetch data.
  daisy_assert (top_level);
  const AttributeList& alist = top_level->program_alist ();
  const std::vector<std::string> files = top_level->files_found ();

  // The program name.
  if (alist.check ("type"))
    qt_name->setText (alist.name ("type").c_str ());
  else
    qt_name->setText ("No program");

  // The file name.
  switch (files.size ())
    {
    case 0:
      qt_file->setToolTip ("No setup file has been loaded.");
      qt_file->setText ("No file");
      break;
    case 1:
      qt_file->setToolTip ("This is the name of the loaded setup file.");
      qt_file->setText (Path::nodir (files[0]).c_str ());
      break;
    default:
      qt_file->setToolTip ("Multiple setup files have been loaded.");
      qt_file->setText ("Multiple files");
    }

  // The program description.
  if (alist.check ("description")
      && alist.name ("description") != Toplevel::default_description)
    qt_description->setText (alist.name ("description").c_str ());
  else
    qt_description->setText ("No setup file has been loaded.\n\
You can drag a setup file to the Daisy icon to run a simulation.");

  // The title.
  if (files.size () == 1)
    qt_main.set_file_name (Path::nodir (files[0]).c_str ());
  else
    qt_main.set_file_name ("");
  qt_main.set_title ();
}

void 
UIRead::open_setup ()
{
  QString fileName 
    = QFileDialog::getOpenFileName (this,
				    "Open Daisy Setup", NULL, 
				    "Daisy Setup Files (*.dai)");

  if (!fileName.isEmpty())
    {
      qt_main.statusBar ()->showMessage ("Loading...");
      top_level->reset ();
      top_level->parse_file (fileName.toStdString ());
      reset ();
      qt_main.statusBar ()->showMessage ("Setup loaded", 2000);
    }
}

UIRead::UIRead (Block& al)
  : UIQt (al),
    qt_main (application_name ()),
    qt_name (new QLabel),
    qt_file (new QLabel),
    qt_description (new QLabel),
    top_level (NULL)
{ }

UIRead::~UIRead ()
{ }

static struct UIReadSyntax
{
  static Model& make (Block& al)
  { return *new UIRead (al); }

  UIReadSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
               "Allow the user to select a file to read.");
    Librarian::add_type (UI::component, "read", alist, syntax, &make);
  }
} UIRead_syntax;

// ui_Qt_read.C ends here.
