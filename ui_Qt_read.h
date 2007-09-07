// ui_Qt_read.h -- Qt based top level user interface for selecting a program.
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

#ifndef UI_QT_READ_H
#define UI_QT_READ_H

#include "ui_Qt.h"
#include "vis_Qt.h"

#include <QtCore/QPointer>

class Topelevel;
class Block;

class QLabel;

class UIRead : public QWidget, public UIQt
{
  Q_OBJECT

  // Subwidgets.
  VisQtMain qt_main;
  QPointer<QLabel> qt_name;
  QPointer<QLabel> qt_file;
  QPointer<QLabel> qt_description;               

  // Data.
  Toplevel* top_level;

  // Use.
public:
  void attach (Toplevel& toplevel);
  void run (Toplevel& toplevel);

  // Update.
private:
  void reset ();

  // Actions.
private slots:
  void open_setup ();

  // Create.
private:
  UIRead& operator= (const UIRead&); // Disable.
  UIRead (const UIRead&);         // Disable.
  explicit UIRead ();            // Disable.
public:
  explicit UIRead (Block& al);
private:
  ~UIRead ();
};

#endif // UI_QT_READ_H
