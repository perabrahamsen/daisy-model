// vis_Qt.h -- Visualize data from other Qt objects.
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

#ifndef VIS_QT_H
#define VIS_QT_H

#include "toplevel.h"
#include "time.h"

#include <QtGui/QMainWindow>
#include <QtGui/QTextEdit>
#include <QtGui/QProgressBar>
#include <QtGui/QLabel>

#include <string>

class Time;
class LogQt;

class VisQtMain : public QMainWindow
{
  Q_OBJECT

  // Content.
private:
  const QString appName;
  QString fileName;
  Toplevel::state_t state;
  double progress;

public:
  void set_file_name (QString fileName);
  void set_title ();

public slots:
  void new_progress (double);
  void new_state (Toplevel::state_t);

protected:
  void closeEvent (QCloseEvent* event);

signals:
  void stop_program ();
  
  // Create and Destroy.
public:
  VisQtMain (QString appName, QWidget* parent = 0);
  ~VisQtMain ();
};

class VisQtText : public QTextEdit
{
  Q_OBJECT

  // Content.
  bool tracking;

  // Use.
private slots:
  void new_text (std::string);
  void track_tracking (int);

  // QWidget.
  QSize sizeHint () const;

  // Create and Destroy.
public:
  VisQtText (QWidget* parent = 0);
  ~VisQtText ();
};

class VisQtProgress : public QProgressBar
{
  Q_OBJECT

  // Content.
private:
  bool has_error;

  // Use.
public slots:
  void new_progress (double);
  void new_state (Toplevel::state_t);
  void found_error ();

  // Create and Destroy.
public:
  VisQtProgress (QWidget* parent = 0);
  ~VisQtProgress ();
};

class VisQtLog
{
  // Content.
protected:
  LogQt* log;

  // Use.
public:
  virtual void attach_log (LogQt*) = 0;

  // Create and destroy.
public:
  VisQtLog ();
  virtual ~VisQtLog ();
};

class VisQtTime : public QLabel, public VisQtLog
{
  Q_OBJECT

public slots:
  void scope_ready ();

private:
  Time time;
public:
  void set_time (const Time&);

  // Create and Destroy.
  void attach_log (LogQt*);
public:
  VisQtTime (QWidget* parent = 0);
  ~VisQtTime ();
};

#endif // VIS_QT_H
