// vis_Qt.C -- Visualize data from other Qt objects.
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

#include "vis_Qt.h"
#include "log_Qt.h"

#include "mathlib.h"

#include <QtGui/QPalette>
#include <QtGui/QStyle>
#include <QtGui/QCloseEvent>

void
VisQtMain::set_file_name (QString file)
{ fileName = file; }

void
VisQtMain::set_title ()
{
  switch (state)
    {
    case Toplevel::is_unloaded:
        setWindowTitle (appName);
	break;
    case Toplevel::is_uninitialized:
    case Toplevel::is_ready:
    case Toplevel::is_done:
      setWindowTitle (fileName + " - " + appName);
      break;
    case Toplevel::is_running:
      if (progress >= 0.0 && progress <= 1.0)
        setWindowTitle (QString ("%1% completed - %2")
                        .arg (double2int (progress * 100))
                        .arg (appName));
      else
        setWindowTitle ("Running... - " + appName);
        
      break;
    case Toplevel::is_error:
      setWindowTitle ("Failed! - " + appName);
      break;
    }
}

void
VisQtMain::new_progress (const double value)
{ 
  progress = value;
  set_title ();
}

void 
VisQtMain::new_state (Toplevel::state_t ns)
{
  state = ns;
  set_title ();
}

void 
VisQtMain::closeEvent (QCloseEvent* event)
{
  emit stop_program (); 
  event->accept ();
}

VisQtMain::VisQtMain (const QString app, QWidget *const parent)
  : QMainWindow (parent),
    appName (app),
    state (Toplevel::is_uninitialized),
    progress (0.0)
{ }

VisQtMain::~VisQtMain ()
{ }

void 
VisQtText::new_text (std::string text)
{
  QTextCursor cursor = textCursor ();
  cursor.movePosition (QTextCursor::End);
  cursor.insertText (text.c_str ()); 
  if (tracking)
    setTextCursor (cursor);
}

void 
VisQtText::track_tracking (int state)
{ tracking = state; }

QSize 
VisQtText::sizeHint () const
{ 
  QFontMetrics metric (document ()->defaultFont ());
  const int frame = (style()->pixelMetric (QStyle::PM_DefaultFrameWidth) + 2 ) * 2;
  const int x = metric.averageCharWidth () * 80 + frame;
  const int y = metric.lineSpacing () * 24 + frame;
  return QSize (x, y);
}

VisQtText::VisQtText (QWidget* parent)
  : QTextEdit (parent),
    tracking (true)
{ 
  setReadOnly (true); 
  setLineWrapMode (QTextEdit::NoWrap);
  QFont font = document ()->defaultFont ();
  font.setStyleHint (QFont::TypeWriter, QFont::PreferDefault);
  font.setFixedPitch (true);
  font.setFamily (font.defaultFamily()); 
  document ()->setDefaultFont (font);
}

VisQtText::~VisQtText ()
{ }

void 
VisQtProgress::new_progress (double value)
{
  if (value < 0)
    this->setMaximum (0);
  else
    {
      this->setMaximum (1000);
      this->setValue (double2int (value * 1000));
    }
}

void 
VisQtProgress::new_state (Toplevel::state_t state)
{
  QPalette pal = this->palette();

  switch (state)
    {
    case Toplevel::is_unloaded:
      new_progress (0.0);
      pal.setColor(QPalette::Highlight, QColor(Qt::black));
      break;
    case Toplevel::is_uninitialized:
      new_progress (0.0);
      pal.setColor(QPalette::Highlight, QColor(Qt::white));
      break;
    case Toplevel::is_ready:
      new_progress (0.0);
      pal.setColor(QPalette::Highlight, QColor(Qt::gray));
      break;
    case Toplevel::is_done:
      new_progress (1.0);
      if (has_error)
        pal.setColor(QPalette::Highlight, QColor(Qt::red));
      else 
        pal.setColor(QPalette::Highlight, QColor(Qt::green));
      break;
    case Toplevel::is_running:
      if (has_error)
        pal.setColor(QPalette::Highlight, QColor(Qt::yellow));
      else 
        pal.setColor(QPalette::Highlight, QColor(Qt::blue));
      break;
    case Toplevel::is_error:
      new_progress (1.0);
      pal.setColor(QPalette::Highlight, QColor(Qt::red));
      break;
    }
  this->setPalette(pal);
}

void
VisQtProgress::found_error ()
{ 
  has_error = true; 
  QPalette pal = this->palette();
  pal.setColor (QPalette::Highlight, QColor(Qt::black));
  this->setPalette(pal);
}

VisQtProgress::VisQtProgress (QWidget* parent)
  : has_error (false)
{ 
  this->setMinimum (0);
  this->setMaximum (1000);
  this->setValue (0);

  QPalette pal = this->palette();
  pal.setColor(QPalette::Highlight, QColor(Qt::magenta));
  this->setPalette(pal);
}

VisQtProgress::~VisQtProgress ()
{ }

VisQtLog::VisQtLog ()
  : log (NULL)
{ }

VisQtLog::~VisQtLog ()
{ }

void 
VisQtTime::scope_ready ()
{ 
  setText ("Hello!");
  daisy_assert (log);
  static const symbol year_symbol ("year");
  static const symbol month_symbol ("month");
  static const symbol mday_symbol ("mday");
  static const symbol hour_symbol ("hour");
  static const symbol minute_symbol ("minute");
  static const symbol second_symbol ("second");

  int year = time.year ();
  int month = time.month ();
  int mday = time.mday ();
  int hour = time.hour ();
  int minute = time.minute ();
  int second = time.second ();
  
  {
    QMutexLocker lock (&log->mutex);

    if (log->has_number (year_symbol))
      year = double2int (log->number (year_symbol));
    if (log->has_number (month_symbol))
      month = double2int (log->number (month_symbol));
    if (log->has_number (mday_symbol))
      mday = double2int (log->number (mday_symbol));
    if (log->has_number (hour_symbol))
      hour = double2int (log->number (hour_symbol));
    if (log->has_number (minute_symbol))
      minute = double2int (log->number (minute_symbol));
    if (log->has_number (second_symbol))
      second = double2int (log->number (second_symbol));
  }
  Time now (year, month, mday, hour, minute, second);
  set_time (now);
}

void
VisQtTime::set_time (const Time& now)
{
  time = now;
  setText (time.print ().c_str ());
}

void
VisQtTime::attach_log (LogQt* l)
{
  log = l; 
  QObject* obj = l;
  QObject::connect (obj, SIGNAL (ready()), this, SLOT (scope_ready ()));
}


VisQtTime::VisQtTime (QWidget* parent)
  : QLabel (parent),
    time (9999, 1, 1, 0)
{ }
  
VisQtTime::~VisQtTime ()
{ }

// vis_Qt.C ends here.
