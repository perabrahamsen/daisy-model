// ui_Qt_run.C -- Qt based top level user interface for running a program.
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
#include "run_Qt.h"
#include "vis_Qt.h"
#include "log_Qt.h"

#include "time.h"
#include "toplevel.h"
#include "program.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"
#include "memutils.h"

#include <QtGui/QStatusBar>
#include <QtGui/QGridLayout>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QCheckBox>
#include <QtCore/QPointer>

#include <sstream>
#include <map>

class UIRun : public UIQt
{
  VisQtMain qt_main;
  QPointer<VisQtProgress> qt_progress;
  QPointer<QPushButton> qt_stop;

  // Logs.
  auto_vector<Log*> all_logs;
  std::map<symbol, LogQt*> logs;
  void build_log (Block& block, const std::string& log);
  void attach_log (const std::string& log, VisQtLog* object);

  // Use.
public:
  void attach (Toplevel& toplevel);
  void run (Toplevel& toplevel);

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

void
UIRun::build_log (Block& block, const std::string& name)
{
  const symbol id (name);
  const std::map<symbol, LogQt*>::const_iterator i = logs.find (id);
  if (i != logs.end ())
    return;

  const Library& library = block.metalib ().library (Log::component);
  if (library.complete (block.metalib (), id))
    {
      const AttributeList& alist = library.lookup (id);
      std::auto_ptr<Log> log_raw 
        (Librarian::build_alist<Log> (block, alist, name));
      LogQt *const log = dynamic_cast<LogQt*> (log_raw.get ());
      if (log)
        {
          log->initialize_common (block.metalib (), block.msg ());
          all_logs.push_back (log);
          logs[id] = log;
          log_raw.release ();
          block.msg ().debug ("Qt log '" + name + "' attached.");
        }
    }
  else
    block.msg ().warning ("Log '" + name + "' not found"
                          ", user interface may be crippled");
}

void
UIRun::attach_log (const std::string& name, VisQtLog* vis)
{
  const symbol id (name);
  const std::map<symbol, LogQt*>::const_iterator i = logs.find (id);
  if (i == logs.end ())
    return;

  vis->attach_log ((*i).second);
}

void 
UIRun::attach (Toplevel& toplevel)
{
  // Fetch data.
  const AttributeList& alist = toplevel.program_alist ();
  const Syntax& syntax = toplevel.program_syntax ();
  
  // Use a special treelog to relay messages to window.
  TreelogQtText *const tlog = new TreelogQtText;                  
  toplevel.add_treelog (tlog); 

  // We organize items in a grid layout.
  QWidget *const center = new QWidget (&qt_main);
  QGridLayout *const layout = new QGridLayout (center);

  // The program name.
  QLabel *const qt_name = new QLabel;                      
  QFont font = qt_name->font ();
  font.setBold (true);
  qt_name->setFont (font);
  if (alist.check ("type"))
    qt_name->setText (alist.name ("type").c_str ());
  else
    qt_name->setText ("No program");
  layout->addWidget (qt_name, 0, 0, Qt::AlignLeft);

  // The simulation time.
  VisQtTime *const qt_time = new VisQtTime;                   
  if (alist.check ("time")
      && syntax.lookup ("time") == Syntax::AList
      && syntax.size ("time") == Syntax::Singleton)
    {
      Syntax time_syntax;
      AttributeList time_alist;
      Time::load_syntax (time_syntax, time_alist);

      const AttributeList& start_alist = alist.alist ("time");
      if (time_syntax.check (toplevel.metalib (),
                             start_alist, Treelog::null ()))
        {
          Time time (start_alist);
          qt_time->set_time (time);
        }
    }
  attach_log ("QtTime", qt_time);
  layout->addWidget (qt_time, 0, 1, Qt::AlignCenter);

  // The file name.
  QLabel *const qt_file = new QLabel;                      
  const std::vector<std::string> files = toplevel.files_found ();
  switch (files.size ())
    {
    case 0:
      qt_file->setText ("No file");
      break;
    case 1:
      qt_file->setText (files[0].c_str ());
      break;
    default:
      qt_file->setText ("Multiple files");
    }
  layout->addWidget (qt_file, 0, 2, Qt::AlignRight);

  // The program description.
  QLabel *const qt_description = new QLabel;               
  if (alist.check ("description")
      && alist.name ("description") != Toplevel::default_description)
    qt_description->setText (alist.name ("description").c_str ());
  else
    qt_description->hide (); // setText ("No description.");
  layout->addWidget (qt_description, 1, 0, 1, 3, Qt::AlignLeft);

  // A text window for simulation messages.
  VisQtText *const qt_vis = new VisQtText;                    
  QObject::connect(tlog->tracker (), SIGNAL(text_ready (std::string)),
                   qt_vis, SLOT(new_text (std::string))); 
  layout->addWidget (qt_vis, 2, 0, 1, 3);
  
  // Emergency stop.
  qt_stop = new QPushButton ("Stop");
  qt_stop->setDisabled (true);
  layout->addWidget (qt_stop, 3, 0);
                                                
  // Track newest messages.
  QCheckBox *const qt_track = new QCheckBox ("Track");
  QObject::connect(qt_track, SIGNAL(stateChanged (int)),
                   qt_vis, SLOT(track_tracking (int))); 
  qt_track->setCheckState (Qt::Checked);
  layout->addWidget (qt_track, 3, 1, Qt::AlignCenter);

  // Dismiss window.
  QPushButton *const qt_dismiss = new QPushButton ("Dismiss");
  QObject::connect(qt_dismiss, SIGNAL(clicked ()),
                   &qt_main, SLOT(close ())); 
  layout->addWidget (qt_dismiss, 3, 2);

  // All of this in our central widget.
  qt_main.setCentralWidget (center);

  // A progress bar in the bottom.
  qt_progress = new VisQtProgress;           
  QObject::connect(tlog->tracker (), SIGNAL(error_occured ()),
                   qt_progress, SLOT(found_error ())); 
  qt_progress->new_state (toplevel.state ());
  qt_main.statusBar ()->addPermanentWidget (qt_progress);

  // The title.
  if (files.size () == 1)
    qt_main.set_file_name (files[0].c_str ());
  else
    qt_main.set_file_name ("");
  qt_main.set_title ();

  // Show it all.
  qt_main.show ();
}

void 
UIRun::run (Toplevel& toplevel)
{ 
  // Start calculations.
  RunQtMain qt_run (toplevel, all_logs);
  daisy_assert (!qt_progress.isNull ());
  QObject::connect(&qt_run, SIGNAL(progress_changed (double)),
                   qt_progress, SLOT(new_progress (double))); 
  QObject::connect(&qt_run, SIGNAL(progress_changed (double)),
                   &qt_main, SLOT(new_progress (double))); 
  QObject::connect(&qt_run, SIGNAL(progress_state (Toplevel::state_t)),
                   qt_progress, SLOT(new_state (Toplevel::state_t))); 
  daisy_assert (!qt_stop.isNull ());
  QObject::connect(&qt_run, SIGNAL(is_now_running (bool)),
                   qt_stop, SLOT(setEnabled (bool))); 
  QObject::connect(qt_stop, SIGNAL(clicked ()),
                   &qt_run, SLOT(stop ())); 
  try 
    {
      qt_run.start ();

      // Start interface.
      run_user_interface ();       // Start the UI.
      qt_run.stop ();              // Tell program to stop.
      qt_run.wait ();              // Wait for it to happen.
      daisy_assert (qt_run.isFinished ());

      switch (toplevel.state ())
        {
        case Toplevel::is_done:
        case Toplevel::is_error:
          break;
        default:
          toplevel.error ("Top level did not finish properly");
          break;
        }
    }
  catch (...)
    {
      exit (107);
    }
}

UIRun::UIRun (Block& al)
  : UIQt (al),
    qt_main (application_name ())
{ build_log (al, "QtTime"); }

UIRun::~UIRun ()
{ }

static struct UIRunSyntax
{
  static Model& make (Block& al)
  { return *new UIRun (al); }

  UIRunSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Run the program in a window.");
    Librarian::add_type (UI::component, "run", alist, syntax, &make);
    Librarian::add_type (UI::component, "read", alist, syntax, &make);
  }
} UIRun_syntax;

// ui_Qt_run.C ends here.
