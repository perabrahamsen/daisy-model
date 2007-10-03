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

#include "ui_Qt_run.h"
#include "log_Qt.h"
#include "run_Qt.h"
#include "time.h"
#include "toplevel.h"
#include "program.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"
#include "path.h"
#include <sstream>

#include <QtGui/QMenuBar>
#include <QtGui/QFileDialog>
#include <QtGui/QStatusBar>
#include <QtGui/QHBoxLayout>
#include <QtGui/QVBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QCheckBox>

struct UIRun::WidgetState
{
  QPointer<QWidget> widget;
  const std::string active_tip;
  const std::string inactive_tip;
  const bool notify;
  std::vector<Toplevel::state_t> active_state;

  WidgetState (QWidget *const w, const std::string& a, const std::string& i,
	       const bool n)
    : widget (w),
      active_tip (a),
      inactive_tip (i),
      notify (n)
  { }
};

void 
UIRun::manage_widget (QWidget* widget, const std::string& active_tip, 
		      const std::string& inactive_tip, bool notify)
{ 
  for (size_t i = 0; i < widget_state.size (); i++)
    daisy_assert (widget != widget_state[i]->widget);

  widget_state.push_back (new WidgetState (widget, 
					   active_tip, inactive_tip, notify));
}

void 
UIRun::manage_widget_active (QWidget* widget, Toplevel::state_t state)
{ 
  for (size_t i = 0; i < widget_state.size (); i++)
    if (widget == widget_state[i]->widget)
      {
	widget_state[i]->active_state.push_back (state);
	return;
      }
  daisy_notreached ();
}

void
UIRun::build_log (Block& block, const std::string& name)
{
  const symbol id (name);
  const std::map<symbol, LogQt*>::const_iterator i = logs.find (id);
  if (i != logs.end ())
    return;

  const Library& library = block.metalib ().library (Log::component);
  if (!library.complete (block.metalib (), id))
    return;
  
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

bool
UIRun::attach_log (const std::string& name, VisQtLog *const vis) const
{
  const symbol id (name);
  const std::map<symbol, LogQt*>::const_iterator i = logs.find (id);
  if (i == logs.end ())
    return false;

  vis->attach_log ((*i).second);
  return true;
}

void 
UIRun::attach (Toplevel& toplevel)
{
  daisy_assert (top_level == NULL);
  top_level = &toplevel;

  // We start by loading special log used by UI.
  try
    {
      if (!has_loaded_log_file
          && toplevel.state () == Toplevel::is_uninitialized)
        {
          has_loaded_log_file = true;
          toplevel.parse_system_file ("ui-Qt.dai");
        }
    }
  catch (...)
    { toplevel.msg ().warning ("Problems loading utility file 'ui-Qt.dai'"); }

  // Build log.
  {
    Block block (toplevel.metalib (), toplevel.msg (), "UI logs");
    build_log (block, "QtTime");   
  }

  // Fetch data.
  const AttributeList& alist = toplevel.program_alist ();
  const Syntax& syntax = toplevel.program_syntax ();
  
  // Attach menubar.
  QMenuBar* menuBar = qt_main.menuBar ();

  // Create File menu.
  QMenu* fileMenu = menuBar->addMenu ("&File");

  // Open setup.
  QAction* openAction = new QAction ("&Open setup...", this);
  openAction->setToolTip ("Open a setup file");
  connect (openAction, SIGNAL(triggered()), this, SLOT(open_setup()));
  fileMenu->addAction (openAction);

  // Run setup.
  QAction* runAction = new QAction ("&Run", this);
  runAction->setToolTip ("Run the selected program.");
  connect (runAction, SIGNAL(triggered()), this, SLOT(run_program()));
  fileMenu->addAction (runAction);

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
  if (alist.check ("type"))
    qt_name->setText (alist.name ("type").c_str ());
  else
    qt_name->setText ("No program");
  top_layout->addWidget (qt_name /* , Qt::AlignLeft */);
  top_layout->addStretch ();

  // The simulation time.
  VisQtTime *const qt_time = new VisQtTime;
  qt_time->setToolTip ("The simulation time.");
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
  if (!attach_log ("QtTime", qt_time)
      && toplevel.state () == Toplevel::is_uninitialized)
    toplevel.msg ().warning ("Log 'QtTime' not found"
                             ", user interface may be crippled");
  top_layout->addWidget (qt_time /* , Qt::AlignCenter */);

  // The file name.
  const std::vector<std::string> files = toplevel.files_found ();
  switch (files.size ())
    {
    case 0:
      qt_file->setToolTip ("No file has been loaded.");
      qt_file->setText ("No file");
      break;
    case 1:
      qt_file->setToolTip ("This is the name of the loaded file.");
      qt_file->setText (files[0].c_str ());
      break;
    default:
      qt_file->setToolTip ("Multiple files have been loaded.");
      qt_file->setText ("Multiple files");
    }
  top_layout->addStretch ();
  top_layout->addWidget (qt_file /* , Qt::AlignRight */);

  // The program description.
  qt_description->setToolTip ("The description of the selected program.");
  if (alist.check ("description")
      && alist.name ("description") != Toplevel::default_description)
    qt_description->setText (alist.name ("description").c_str ());
  else
    qt_description->hide (); // setText ("No description.");
  layout->addWidget (qt_description /* , Qt::AlignLeft */);

  // A text window for simulation messages.
  // Use a special treelog to relay messages to window.
  TreelogQtText *const tlog = new TreelogQtText;                  
  VisQtText *const qt_vis = new VisQtText;
  qt_vis->setToolTip ("Textual feedback from the program.");
  QObject::connect(tlog->tracker (), SIGNAL(text_ready (std::string)),
                   qt_vis, SLOT(new_text (std::string))); 
  toplevel.add_treelog (tlog); 
  layout->addWidget (qt_vis);

  // The bottom line
  QWidget *const bottom = new QWidget (center);
  QHBoxLayout *const bottom_layout = new QHBoxLayout (bottom);
  layout->addWidget (bottom);

  // Emergency stop.
  qt_stop = new QPushButton ("Stop");
  qt_stop->setToolTip ("Press here to stop the program.");
  qt_stop->setDisabled (true);
  bottom_layout->addWidget (qt_stop);
  bottom_layout->addStretch ();
  daisy_assert (!qt_stop.isNull ());                                                
  // Track newest messages.
  QCheckBox *const qt_track = new QCheckBox ("Track");
  qt_track->setToolTip ("Check this box to always show new text.");
  QObject::connect(qt_track, SIGNAL(stateChanged (int)),
                   qt_vis, SLOT(track_tracking (int))); 
  qt_track->setCheckState (Qt::Checked);
  bottom_layout->addWidget (qt_track /* , Qt::AlignCenter */);
    
  // Dismiss window.
  QPushButton *const qt_dismiss = new QPushButton ("Dismiss");
  qt_dismiss->setToolTip ("Press here to end the application.");
  QObject::connect(qt_dismiss, SIGNAL(clicked ()),
                   &qt_main, SLOT(close ())); 
  bottom_layout->addStretch ();
  bottom_layout->addWidget (qt_dismiss);
    
  // All of this in our central widget.
  qt_main.setCentralWidget (center);

  // A progress bar in the bottom.
  qt_progress = new VisQtProgress;
  qt_progress->setToolTip ("Display progress of program.");
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
UIRun::run_program ()
{ 
  // New thread.
  daisy_assert (qt_run.isNull ());
  qt_run = new RunQtMain (*top_level, all_logs);
  daisy_assert (!qt_run.isNull ());

#if 0
  // Progress bar.
  daisy_assert (!qt_progress.isNull ());
  QObject::connect(qt_run, SIGNAL(progress_changed (double)),
		   qt_progress, SLOT(new_progress (double))); 
  QObject::connect(qt_run, SIGNAL(progress_state (Toplevel::state_t)),
		   qt_progress, SLOT(new_state (Toplevel::state_t))); 

  // Main window.
  QObject::connect(qt_run, SIGNAL(progress_changed (double)),
		   &qt_main, SLOT(new_progress (double))); 
  QObject::connect(qt_run, SIGNAL(progress_state (Toplevel::state_t)),
		   &qt_main, SLOT(new_state (Toplevel::state_t))); 

  // Stop button.
  daisy_assert (!qt_stop.isNull ());
  QObject::connect(qt_run, SIGNAL(is_now_running (bool)),
		   qt_stop, SLOT(setEnabled (bool))); 
  QObject::connect(qt_stop, SIGNAL(clicked ()),
		   qt_run, SLOT(stop ())); 
#endif

  // Start thread.
  qt_run->start ();
}

void
UIRun::stop_program ()
{
  if (!qt_run.isNull ())
    {
      qt_run->stop ();              // Tell program to stop.
      qt_run->wait ();              // Wait for it to happen.
      daisy_assert (qt_run->isFinished ());
      delete qt_run;
    }
  daisy_assert (qt_run.isNull ());
}

void 
UIRun::run (Toplevel& toplevel)
{ 

  run_user_interface ();	// Start the UI.
  stop_program ();		// Stop the simulation.
}

void
UIRun::reset ()
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
UIRun::open_setup ()
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

UIRun::UIRun (Block& al)
  : UIQt (al),
    qt_main (application_name ()),
    qt_name (new QLabel),
    qt_file (new QLabel),
    qt_description (new QLabel),
    has_loaded_log_file (false),
    top_level (NULL)
{ }

UIRun::~UIRun ()
{ stop_program (); }

static struct UIRunSyntax
{
  static Model& make (Block& al)
  { return *new UIRun (al); }

  UIRunSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Run the program in a window.");
    Librarian::add_type (UI::component, "GUI", alist, syntax, &make);
  }
} UIRun_syntax;

// ui_Qt_run.C ends here.
