// ui_Qt_run.h -- Qt based top level user interface for running a program.
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

#ifndef UI_QT_RUN_H
#define UI_QT_RUN_H

#include "ui_Qt.h"
#include "vis_Qt.h"
#include "memutils.h"
#include <map>

#include <QtCore/QPointer>

class Topelevel;
class Block;
class RunQtMain;

class QPushButton;
class QLabel;

class UIRun : public QWidget, public UIQt
{
  Q_OBJECT

  // Widgets.
  VisQtMain qt_main;
  QPointer<QLabel> qt_name;
  QPointer<QLabel> qt_file;
  QPointer<QLabel> qt_description;               
  QPointer<QPushButton> qt_stop;
  QPointer<VisQtProgress> qt_progress;

  // Logs.
  bool has_loaded_log_file;
  auto_vector<Log*> all_logs;
  std::map<symbol, LogQt*> logs;
  void build_log (Block& block, const std::string& log);
  bool attach_log (const std::string& log, VisQtLog* object) const;

  // The simulation thread.
private:
  Toplevel* top_level;
  QPointer<RunQtMain> qt_run;
  void run_program ();
  void stop_program ();

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
  UIRun& operator= (const UIRun&); // Disable.
  UIRun (const UIRun&);         // Disable.
  explicit UIRun ();            // Disable.
public:
  explicit UIRun (Block& al);
private:
  ~UIRun ();
};

#endif // UI_QT_RUN_H

// ui_Qt_run.h ends here.
