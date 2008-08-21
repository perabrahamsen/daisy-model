// run_Qt.h -- Run a program in a seperate Qt thread.
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

#ifndef RUN_QT_H
#define RUN_QT_H

#include "run.h"
#include "toplevel.h"
#include "treelog_text.h"

#include <QtCore/QThread>
#include <QtCore/QMutex>

#include <vector>
#include <string>

class Toplevel;
class Log;

class VisQtText;
class VisQtProgress;

class RunQtMain : public QThread, public Run
{
  Q_OBJECT

  // Content.
private:
  Toplevel& toplevel;
  std::vector<Log*> logs;
  bool is_running;
  mutable QMutex mutex;

  // Use.
protected:
  void run();

  // Control.
private:
  bool running () const;
  void set_progress (double);
public slots:
  void stop ();

signals:
  void progress_changed (double) const;
  void progress_state (Toplevel::state_t) const;
  void is_now_running (bool) const;

  // Create and Destroy.
public:
  RunQtMain (Toplevel&, const std::vector<Log*>& l, QObject* parent = 0);
  ~RunQtMain ();
};

class RunQtText : public QObject
{
  Q_OBJECT

  // Use.
public:
  void send_text (const std::string&) const;
  void signal_error () const;

signals:
  void text_ready (std::string) const;
  void error_occured () const;

  // Create and Destroy.
public:
  RunQtText (QObject* parent = 0);
  ~RunQtText ();
};

class RunQtNumbers : public QObject
{
  Q_OBJECT

  // Use.
public:
  void send_numbers (const std::vector<double>&) const;

signals:
  void numbers_ready (std::vector<double>) const;

  // Create and Destroy.
public:
  RunQtNumbers (QObject* parent = 0);
  ~RunQtNumbers ();
};

class TreelogQtText : public TreelogText
{
  // Content.
private:
  RunQtText qt_text;
public:
  const QObject* tracker () const;

  // Use.
private:
  void write (const std::string& text);
  void touch ();
  void debug (const std::string&);
  void flush ();
  void error (const std::string&);
  
  // Create and Destroy.
public:
  TreelogQtText ();
private:
  ~TreelogQtText ();
};

#endif // RUN_QT_H
