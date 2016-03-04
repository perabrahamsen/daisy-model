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
#include "block_top.h"
#include "assertion.h"
#include "path.h"
#include "treelog.h"
#include "frame_submodel.h"
#include "uifilter.h"
#include "frame_model.h"
#include <sstream>

#include <QtGui/QMenuBar>
#include <QtGui/QFileDialog>
#include <QtGui/QStatusBar>
#include <QtGui/QHBoxLayout>
#include <QtGui/QVBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QCheckBox>
#include <QtGui/QTabWidget>
#include <QtGui/QGroupBox>
#include <QtGui/QComboBox>
#include <QtGui/QLineEdit>
#include <QtGui/QSplitter>
#include <QtGui/QSizePolicy>
#include <QtGui/QScrollArea>
#include <QtGui/QTextEdit>

void
UIRun::build_log (Metalib& metalib, Block& block, const std::string& name)
{
  const symbol id (name);
  const std::map<symbol, LogQt*>::const_iterator i = logs.find (id);
  if (i != logs.end ())
    return;

  Treelog& msg = block.msg ();
    
  const Library& library = metalib.library (Log::component);
  if (!library.complete (metalib, id))
    return;
  
  std::unique_ptr<Log> log_raw 
    (Librarian::build_stock<Log> (metalib, msg, id, __FUNCTION__));
  LogQt *const log = dynamic_cast<LogQt*> (log_raw.get ());
  if (log)
    {
      log->initialize_common (id, metalib, msg);
      all_logs.push_back (log);
      logs[id] = log;
      log_raw.release ();
      msg.debug ("Qt log '" + name + "' attached.");
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
    BlockTop block (toplevel.metalib (), toplevel.msg (),
                    toplevel.metalib ());
    build_log (toplevel.metalib (), block, "QtTime");   
  }

  // Organize tabs.
  QTabWidget *const tab_widget = new QTabWidget (&qt_main);
  attach_tab_file (toplevel, *tab_widget);
  attach_tab_edit (toplevel, *tab_widget);
  attach_tab_run (toplevel, *tab_widget);

  // All of this in our central widget.
  qt_main.setCentralWidget (tab_widget);

  // The title.
  const std::vector<std::string> files = toplevel.files_found ();
  if (files.size () == 1)
    qt_main.set_file_name (files[0].c_str ());
  else
    qt_main.set_file_name ("");
  qt_main.set_title ();

  // Show it all.
  qt_main.show ();
}

void 
UIRun::attach_tab_file (Toplevel& toplevel, QTabWidget& tab_widget)
{
  // We organize items in a boxes.
  QWidget *const file_center = new QWidget ();
  QVBoxLayout *const layout = new QVBoxLayout (file_center);
  
  // The Setup file.
  std::string file_label;
  const std::vector<std::string> files = toplevel.files_found ();
  switch (files.size ())
    { 
    case 0:
      file_label = "No file loaded";
      break;
    case 1:
      file_label = "Setup file: " + files[0];
      break;
    default:
      file_label = "Multiple files loaded";
    }

  QGroupBox *const file_box = new QGroupBox (file_label.c_str ());
  QHBoxLayout *const file_layout = new QHBoxLayout ();
  file_box->setLayout (file_layout);
  layout->addWidget (file_box);
 

  // Open setup.
  QPointer<QPushButton> qt_new = new QPushButton ("New");
  daisy_assert (!qt_new.isNull ());
  qt_new->setToolTip ("Start a new Daisy setup.");
  qt_new->setEnabled (false);
  file_layout->addWidget (qt_new);
  QObject::connect(qt_new, SIGNAL(clicked ()),
		   this, SLOT(new_setup ())); 

  QPointer<QPushButton> qt_open = new QPushButton ("Open...");
  daisy_assert (!qt_open.isNull ());
  qt_open->setToolTip ("Open a Daisy setup file.");
  file_layout->addWidget (qt_open);
  QObject::connect(qt_open, SIGNAL(clicked ()),
		   this, SLOT(open_setup ())); 


  QPointer<QPushButton> qt_save = new QPushButton ("Save");
  daisy_assert (!qt_save.isNull ());
  qt_save->setToolTip ("Save the Daisy setup to the currect file.");
  qt_save->setEnabled (false);
  file_layout->addWidget (qt_save);
  QObject::connect(qt_save, SIGNAL(clicked ()),
		   this, SLOT(save_setup ())); 

  QPointer<QPushButton> qt_save_as = new QPushButton ("Save as...");
  daisy_assert (!qt_save_as.isNull ());
  qt_save_as->setToolTip ("Save the Daisy setup to a new file");
  qt_save_as->setEnabled (false);
  file_layout->addWidget (qt_save_as);
  file_layout->addStretch ();
  QObject::connect(qt_save, SIGNAL(clicked ()),
		   this, SLOT(save_setup_as ())); 

  // The program.
  QGroupBox *const program_box = new QGroupBox ("Program");
  QHBoxLayout *const program_layout = new QHBoxLayout ();
  program_box->setLayout (program_layout);
  layout->addWidget (program_box);

  const Frame& frame = toplevel.program_frame ();
  const symbol program_name = frame.type_name ();
  QPointer<QComboBox> qt_select_program = new QComboBox ();
  daisy_assert (!qt_select_program.isNull ());
  qt_select_program->setToolTip ("Select program to run");
  const Metalib& metalib = toplevel.metalib ();
  const Library& proglib = metalib.library (Program::component);
  std::vector<symbol> progs;
  proglib.entries (progs);
  qt_select_program->addItem (Attribute::None ().name ().c_str ());
  size_t found = progs.size ();
  for (size_t i = 0; i < progs.size (); i++)
    {
      if (progs[i] == program_name)
        found = i;
      qt_select_program->addItem (progs[i].name ().c_str ());
    }
  if (found < progs.size ())
    qt_select_program->setCurrentIndex (found + 1);
  else
    qt_select_program->setCurrentIndex (0);
  program_layout->addWidget (qt_select_program);
  program_layout->addStretch ();

  // The working directory.
  QGroupBox *const directory_box = new QGroupBox ("Working directory");
  QHBoxLayout *const directory_layout = new QHBoxLayout ();
  directory_box->setLayout (directory_layout);
  layout->addWidget (directory_box);

  // The file path.
  QGroupBox *const path_box = new QGroupBox ("File path");
  QHBoxLayout *const path_layout = new QHBoxLayout ();
  path_box->setLayout (path_layout);
  layout->addWidget (path_box);

  // The inputs path.
  QGroupBox *const input_box = new QGroupBox ("Inputs");
  QHBoxLayout *const input_layout = new QHBoxLayout ();
  input_box->setLayout (input_layout);
  layout->addWidget (input_box);
  layout->addStretch ();

  // Organize tabs.
  tab_widget.addTab (file_center, "File");
}

void 
UIRun::attach_tab_edit (Toplevel& toplevel, QTabWidget& tab_widget)
{
  // Left pane
  const QPointer<QWidget> left_pane_widget = new QWidget;
  const QPointer<QVBoxLayout> left_pane = new QVBoxLayout;
  left_pane_widget->setLayout (left_pane);

  // Select filter.
  QPointer<QGroupBox> qt_select_filter_box = new QGroupBox ("Filter");
  left_pane->addWidget (qt_select_filter_box);
  daisy_assert (!qt_select_filter.isNull ());
  qt_select_filter->setToolTip ("Select user interface.");
  const Metalib& metalib = toplevel.metalib ();
  const Library& filterlib = metalib.library (UIFilter::component);
  std::vector<symbol> filters;
  filterlib.entries (filters);
  size_t index = 0;
  size_t found = filters.size ();
  for (size_t i = 0; i < filters.size (); i++)
    {
      const symbol filter = filters[i];
      if (!filterlib.complete (metalib, filter))
        continue;
      if (filter == selected_filter)
        found = index;
      index++;
      qt_select_filter->addItem (filter.name ().c_str ());
    }
  daisy_assert (found < index);
  qt_select_filter->setCurrentIndex (found);
  QPointer<QVBoxLayout> qt_select_filter_layout = new QVBoxLayout;
  qt_select_filter_box->setLayout (qt_select_filter_layout); 
  qt_select_filter_layout->addWidget (qt_select_filter);
  QObject::connect(qt_select_filter, SIGNAL(activated (const QString&)),
                   this, SLOT(select_filter (const QString&))); 

  // Select component and model.
  QPointer<QGroupBox> qt_select_model_box = new QGroupBox ("Edit model");
  left_pane->addWidget (qt_select_model_box);
  QPointer<QVBoxLayout> qt_select_model_layout = new QVBoxLayout;
  qt_select_model_box->setLayout (qt_select_model_layout); 
  daisy_assert (!qt_select_component.isNull ());
  qt_select_component->setToolTip ("Select component.");
  qt_select_model_layout->addWidget (qt_select_component);
  QObject::connect(qt_select_component, SIGNAL(activated (const QString&)),
                   this, SLOT(select_component (const QString&))); 
  daisy_assert (!qt_select_model.isNull ());
  qt_select_model->setToolTip ("Select model.");
  qt_select_model_layout->addWidget (qt_select_model);
  QObject::connect(qt_select_model, SIGNAL(activated (const QString&)),
                   this, SLOT(select_model (const QString&))); 
 
  // The Save button.
  QPointer<QPushButton> qt_save = new QPushButton ("Save");
  daisy_assert (!qt_save.isNull ());
  qt_save->setToolTip ("Save current model.");
  qt_save->setEnabled (false);
  QPointer<QHBoxLayout> qt_save_layout = new QHBoxLayout;
  qt_save_layout->addWidget (qt_save);
  // qt_save_layout->addStretch ();
  qt_select_model_layout->addLayout (qt_save_layout);

  // The Reset button.
  QPointer<QPushButton> qt_reset = new QPushButton ("Reset");
  daisy_assert (!qt_reset.isNull ());
  qt_reset->setToolTip ("Reset current model.");
  qt_reset->setEnabled (false);
  QPointer<QHBoxLayout> qt_reset_layout = new QHBoxLayout;
  qt_reset_layout->addWidget (qt_reset);
  // qt_reset_layout->addStretch ();
  qt_select_model_layout->addLayout (qt_reset_layout);

  // The Delete button.
  QPointer<QPushButton> qt_delete = new QPushButton ("Delete");
  daisy_assert (!qt_delete.isNull ());
  qt_delete->setToolTip ("Delete current model.");
  qt_delete->setEnabled (false);
  QPointer<QHBoxLayout> qt_delete_layout = new QHBoxLayout;
  qt_delete_layout->addWidget (qt_delete);
  // qt_delete_layout->addStretch ();
  qt_select_model_layout->addLayout (qt_delete_layout);
  
  // The New button.
  QPointer<QGroupBox> qt_derive_box = new QGroupBox ("New model");
  qt_derive_box->setToolTip ("Derive a new model from an existing model.");
  left_pane->addWidget (qt_derive_box);
  QPointer<QVBoxLayout> qt_derive_layout = new QVBoxLayout;
  qt_derive_box->setLayout (qt_derive_layout); 
  qt_new_component->setToolTip ("Choose a component to derive from.");
  qt_derive_layout->addWidget (qt_new_component);
  QObject::connect(qt_new_component, SIGNAL(activated (const QString&)),
                   this, SLOT(select_new_component (const QString&))); 
  qt_new_parent->setToolTip ("Choose an existing model to derive from.");
  qt_derive_layout->addWidget (qt_new_parent);
  QObject::connect(qt_new_parent, SIGNAL(activated (const QString&)),
                   this, SLOT(select_new_parent (const QString&))); 
  QPointer<QLineEdit> qt_derive_field = new QLineEdit ();
  qt_derive_layout->addWidget (qt_derive_field);
  qt_derive_field->setToolTip ("Enter name of new model here.");
  QPointer<QPushButton> qt_derive = new QPushButton ("Derive");
  daisy_assert (!qt_derive.isNull ());
  qt_derive->setToolTip ("Add a new model.");
  qt_derive->setEnabled (false);
  qt_derive_layout->addWidget (qt_derive);

  // Empty space at bottom.
  left_pane->addStretch ();

  // Right pane.
  const QPointer<QWidget> right_pane_widget = new QWidget;
  right_pane_widget->setLayout (qt_edit_pane);
  const QPointer<QScrollArea> right_pane_area = new QScrollArea;
  right_pane_area->setWidgetResizable (true);
  right_pane_area->setWidget (right_pane_widget);

  // Split between panes.
  QSizePolicy left_policy = left_pane_widget->sizePolicy ();
  left_policy.setHorizontalPolicy (QSizePolicy::Preferred);
  left_pane_widget->setSizePolicy (left_policy);
  QSizePolicy right_policy = right_pane_area->sizePolicy ();
  right_policy.setHorizontalPolicy (QSizePolicy::MinimumExpanding);
  right_policy.setHorizontalStretch (1);
  right_pane_area->setSizePolicy (right_policy);

  const QPointer<QSplitter> edit_center = new QSplitter;
  edit_center->addWidget (left_pane_widget);
  edit_center->addWidget (right_pane_area);

  // Organize tabs.
  tab_widget.addTab (edit_center, "Edit");

  // Select filter.
  reset_tab_edit ();
}

void 
UIRun::attach_tab_run (Toplevel& toplevel, QTabWidget& tab_widget)
{
  // We organize items in a boxes.
  QWidget *const run_center = new QWidget ();
  QVBoxLayout *const layout = new QVBoxLayout (run_center);
  
  // The top line.
  QHBoxLayout *const top_layout = new QHBoxLayout;
  layout->addLayout (top_layout);

  // The program name.
  qt_name->setToolTip ("The name of the program or simulation to run.");
  QFont font = qt_name->font ();
  font.setBold (true);
  qt_name->setFont (font);
  const Frame& frame = toplevel.program_frame ();
  const symbol type_name = frame.type_name ();
  qt_name->setText (type_name.name ().c_str ());
  top_layout->addWidget (qt_name /* , Qt::AlignLeft */);
  top_layout->addStretch ();

  // The simulation time.
  VisQtTime *const qt_time = new VisQtTime;
  qt_time->setToolTip ("The simulation time.");
  if (frame.check ("time")
      && frame.lookup ("time") == Attribute::Submodel
      && frame.type_size ("time") == Attribute::Singleton
      && frame.check (toplevel.metalib (), "time", Treelog::null ()))
    {
      Time time (frame.submodel ("time"));
      qt_time->set_time (time);
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
  if (frame.description () != Attribute::None ()
      && frame.description () != Toplevel::default_description)
    qt_description->setText (frame.description ().name ().c_str ());
  else
    qt_description->hide (); // setText ("No description.");
  layout->addWidget (qt_description /* , Qt::AlignLeft */);

  // A text window for simulation messages.
  // Use a special treelog to relay messages to window.
  TreelogQtText* tlog = new TreelogQtText;
  VisQtText *const qt_vis = new VisQtText;
  qt_vis->setToolTip ("Textual feedback from the program.");
  QObject::connect(tlog->tracker (), SIGNAL(text_ready (std::string)),
                   qt_vis, SLOT(new_text (std::string))); 
  boost::shared_ptr<Treelog> tlog_share (tlog);
  toplevel.add_treelog (tlog_share); 
  layout->addWidget (qt_vis);

  // The bottom line
  QWidget *const bottom = new QWidget (run_center);
  QHBoxLayout *const bottom_layout = new QHBoxLayout (bottom);
  layout->addWidget (bottom);

  // Start and stop program.
  qt_runstop = new QPushButton ("Run");
  daisy_assert (!qt_runstop.isNull ());
  qt_runstop->setToolTip ("Press here to run the program.");
  bottom_layout->addWidget (qt_runstop);
  // bottom_layout->addStretch ();
  QObject::connect(qt_runstop, SIGNAL(clicked ()),
		   this, SLOT(run_program ())); 

  // A progress bar in the bottom.
  qt_progress = new VisQtProgress;
  qt_progress->setToolTip ("Display progress of program.");
  QObject::connect(tlog->tracker (), SIGNAL(error_occured ()),
                   qt_progress, SLOT(found_error ())); 
  qt_progress->new_state (toplevel.state ());
  bottom_layout->addWidget (qt_progress);

  // Track newest messages.
  QCheckBox *const qt_track = new QCheckBox ("Track");
  qt_track->setToolTip ("Check this box to always show new text.");
  QObject::connect(qt_track, SIGNAL(stateChanged (int)),
                   qt_vis, SLOT(track_tracking (int))); 
  qt_track->setCheckState (Qt::Checked);
  bottom_layout->addWidget (qt_track /* , Qt::AlignCenter */);
    
  // Organize tabs.
  tab_widget.addTab (run_center, "Run");
}

void 
UIRun::run_program ()
{ 
  if (!qt_run.isNull ())
    // Thread already running. Stop it.
    {
      stop_program ();
      return;
    }

  // New thread.
  daisy_assert (qt_run.isNull ());
  qt_run = new RunQtMain (*top_level, all_logs);
  daisy_assert (!qt_run.isNull ());

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
  QObject::connect(qt_run, SIGNAL(progress_state (Toplevel::state_t)),
		   this, SLOT(new_state (Toplevel::state_t))); 
  QObject::connect(&qt_main, SIGNAL(stop_program ()),
		   this, SLOT(stop_program ())); 


  // Start thread.
  qt_run->start (QThread::QThread::IdlePriority);
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
      qt_run = NULL;
      daisy_assert (qt_run.isNull ());
    }
  if (!qt_run.isNull ())
    daisy_bug ("qt_run should be NULL after process has been stopped");
}

void 
UIRun::run (Toplevel& toplevel)
{ 

  run_user_interface ();	// Start the UI.
  if (!qt_run.isNull ())
    daisy_bug ("qt_run should be NULL after user interface has finished");
}

void 
UIRun::failure (Toplevel& toplevel)
{ 

  run_user_interface ();	// Start the UI.
  stop_program ();		// Stop the simulation.
}

void
UIRun::reset ()
{
  reset_tab_edit ();
  reset_tab_run ();
}

void
UIRun::reset_tab_edit ()
{
  // Fetch data.
  daisy_assert (top_level);
  const std::vector<std::string> files = top_level->files_found ();
  if (files.size () == 1)
    select_file (files[0]);
  else
    select_file (Attribute::None ());
}

void
UIRun::reset_tab_run ()
{
  // Fetch data.
  daisy_assert (top_level);
  const Frame& frame = top_level->program_frame ();
  const std::vector<std::string> files = top_level->files_found ();

  // The program name.
  const symbol type_name = frame.type_name ();
  if (type_name != Attribute::None ())
    qt_name->setText (type_name.name ().c_str ());
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
      qt_file->setText (Path::nodir (files[0]).name ().c_str ());
      break;
    default:
      qt_file->setToolTip ("Multiple setup files have been loaded.");
      qt_file->setText ("Multiple files");
    }

  // The program description.
  if (frame.description () != Attribute::None ()
      && frame.description () != Toplevel::default_description)
    qt_description->setText (frame.description ().name ().c_str ());
  else
    qt_description->setText ("No setup file has been loaded.\n\
You can drag a setup file to the Daisy icon to run a simulation.");

  // The title.
  if (files.size () == 1)
    qt_main.set_file_name (Path::nodir (files[0]).name ().c_str ());
  else
    qt_main.set_file_name ("");
  qt_main.set_title ();
}

void 
UIRun::new_setup ()
{ }

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
      try 
        {
          top_level->reset ();
          top_level->parse_file (fileName.toStdString ());
          reset ();
          qt_main.statusBar ()->showMessage ("Setup loaded", 2000);
        }
      catch (...)
        { 
          qt_main.statusBar ()->showMessage ("Failed!");
        }
    }
}

void 
UIRun::save_setup ()
{ }

void 
UIRun::save_setup_as ()
{ }

void 
UIRun::select_filter (const QString& name)
{ select_filter (name.toStdString ()); }

void 
UIRun::select_component (const QString& name)
{ select_component (name.toStdString ()); }

void 
UIRun::select_model (const QString& name)
{ select_model (name.toStdString ()); }

void 
UIRun::select_new_component (const QString& name)
{ select_new_component (name.toStdString ()); }

void 
UIRun::select_new_parent (const QString& name)
{ select_new_parent (name.toStdString ()); }

void 
UIRun::select_file (const symbol name)
{ 
  selected_file = name;
  select_filter (selected_filter);
}

void 
UIRun::select_filter (const symbol name)
{ 
  // Create filter.
  daisy_assert (top_level);
  const Metalib& metalib = top_level->metalib ();
  filter.reset (Librarian::build_stock<UIFilter> (metalib, Treelog::null (),
                                                  name, __FUNCTION__));
  daisy_assert (filter.get ());
  filter->reset (metalib, selected_file);

  // Remember filter.
  selected_filter = name;

  // Fill component box.
  const std::vector<symbol>& editable = filter->find_components_editable ();
  qt_select_component->clear ();
  for (size_t i = 0; i < editable.size (); i++)
    {
      qt_select_component->addItem (editable[i].name ().c_str ());
      daisy_assert (metalib.exist (editable[i]));
      const Library& library = metalib.library (editable[i]);
      qt_select_component->setItemData 
        (i, library.description ().name ().c_str (), Qt::ToolTipRole);
    }

  // Check if old component is still available.
  if (std::find (editable.begin (), editable.end (), selected_component) 
      == editable.end ())
    selected_component 
      = filter->default_component_editable ();

  // Update the component index (and model).
  select_component (selected_component);

  // Fill new component box.
  const std::vector<symbol>& all = filter->find_components_all ();
  qt_new_component->clear ();
  for (size_t i = 0; i < all.size (); i++)
    {
      qt_new_component->addItem (all[i].name ().c_str ());
      daisy_assert (metalib.exist (all[i]));
      const Library& library = metalib.library (all[i]);
      qt_new_component->setItemData 
        (i, library.description ().name ().c_str (), Qt::ToolTipRole);
    }

  // Check if old component is still available.
  if (std::find (all.begin (), all.end (), selected_new_component) 
      == all.end ())
    selected_new_component = filter->default_component_all ();

  // Update the component index (and model).
  select_new_component (selected_new_component);
}

void 
UIRun::select_component (const symbol name)
{ 
  // Update combobox.
  int index = qt_select_component->findText (name.name ().c_str ());
  qt_select_component->setCurrentIndex (index);

  // Remember choice.
  selected_component = name;

  // Check state.
  daisy_assert (top_level);
  const Metalib& metalib = top_level->metalib ();
  if (!metalib.exist (name))
    {
      qt_select_model->setEnabled (false);
      return;
    }
  qt_select_model->setEnabled (true);
  const Library& library = metalib.library (name);
  daisy_assert (filter.get ());


  // Fill model box.
  const std::vector<symbol>& all = filter->find_models_editable (name);
  qt_select_model->clear ();
  for (size_t i = 0; i < all.size (); i++)
    {
      qt_select_model->addItem (all[i].name ().c_str ());
      daisy_assert (library.check (all[i]));
      const FrameModel& frame = library.model (all[i]);
      qt_select_model->setItemData 
        (i, frame.description ().name ().c_str (), Qt::ToolTipRole);
    }

  // Check if old model is still available.
  if (std::find (all.begin (), all.end (), selected_model) == all.end ())
    selected_model = filter->default_model_editable (selected_component);

  // Update model index.
  select_model (selected_model);
}

void 
UIRun::select_model (const symbol name)
{ 
  // Update combobox.
  {
    int index = qt_select_model->findText (name.name ().c_str ());
    qt_select_model->setCurrentIndex (index);
  }

  // Remember choice.
  selected_model = name;

  // Get data.
  daisy_assert (top_level);
  const Metalib& metalib = top_level->metalib ();
  daisy_assert (metalib.exist (selected_component));

  // Get items.
  daisy_assert (filter.get ());
  const std::vector<const UIItem*>& items
    = filter->find_items (selected_component, selected_model);

  // Delete old content.
  QLayoutItem *child;
  while ((child = qt_edit_pane->takeAt(0)) != 0) 
    {
      delete child->widget();
      delete child;
    }

  // Indert new items.
  for (size_t i = 0; i < items.size (); i++)
    qt_edit_pane->addWidget (build_item (metalib, 
                                         selected_component, selected_model,
                                         *filter, *items[i]));

  qt_edit_pane->addStretch ();
}

QPointer<QWidget>
UIRun::build_item (const Metalib& metalib, 
                   const symbol component, const symbol model, 
                   const UIFilter& filter, const UIItem& item)
{
  // Data.
  const symbol name = item.name;
  const Library& library = metalib.library (component);
  const FrameModel& frame = library.model (model);
  const symbol super = frame.base_name ();
  bool is_changed = true;
  if (library.check (super))
    {
      const FrameModel& parent = library.model (super);
      is_changed = !frame.subset (metalib, parent, name);
    }

  // Widgets.
  QPointer<QGroupBox> group = new QGroupBox (name.name ().c_str ());
  group->setToolTip (frame.description (name).name ().c_str ());
  QPointer<QHBoxLayout> layout = new QHBoxLayout;
  group->setLayout (layout);
  QPointer<QCheckBox> check = new QCheckBox;
  check->setCheckState (is_changed ? Qt::Checked : Qt::Unchecked);
  check->setToolTip ("Overwrite parent value");
  layout->addWidget (check);

  if (frame.type_size (name) == Attribute::Singleton)
    {
      switch (frame.lookup (name))
        {
        case Attribute::String:
          if (frame.is_text (name))
            {
              QPointer<QTextEdit> edit = new QTextEdit;
              if (frame.check (name))
                edit->setText (frame.name (name).name ().c_str ());
              layout->addWidget (edit);
            }
          else
            {
              QPointer<QLineEdit> edit = new QLineEdit;
              if (frame.check (name))
                edit->setText (frame.name (name).name ().c_str ());
              layout->addWidget (edit);
            }
          break;
        default:
          {
            std::string type = Attribute::type_name (frame.lookup (name))
              + ": not yet supported";
            layout->addWidget (new QLabel (type.c_str ()));
            layout->addStretch ();
            break;
          }
        }
    }
  else
    {
      layout->addWidget (new QLabel ("Lists not yet implemented"));
      layout->addStretch ();
    }
  QPointer<QWidget> widget = group.data ();
  return widget;
}

void 
UIRun::select_new_component (const symbol name)
{ 
  // Update combobox.
  int index = qt_new_component->findText (name.name ().c_str ());
  qt_new_component->setCurrentIndex (index);

  // Remember choice.
  selected_new_component = name;

  daisy_assert (top_level);
  const Metalib& metalib = top_level->metalib ();
  if (!metalib.exist (name))
    {
      qt_new_parent->setEnabled (false);
      return;
    }
  qt_new_parent->setEnabled (true);
  const Library& library = metalib.library (name);
  daisy_assert (filter.get ());

  // Fill parent box.
  const std::vector<symbol>& all = filter->find_models_all (name);
  qt_new_parent->clear ();
  for (size_t i = 0; i < all.size (); i++)
    {
      qt_new_parent->addItem (all[i].name ().c_str ());
      daisy_assert (library.check (all[i]));
      const FrameModel& frame = library.model (all[i]);
      qt_new_parent->setItemData 
        (i, frame.description ().name ().c_str (), Qt::ToolTipRole);
    }

  // Check if old parent is still available.
  if (std::find (all.begin (), all.end (), selected_new_parent) == all.end ())
    selected_new_parent = filter->default_model_all (selected_new_component);

  // Update model index.
  select_new_parent (selected_new_parent);
}

void 
UIRun::select_new_parent (const symbol name)
{ selected_new_parent = name; }
  
void 
UIRun::new_state (Toplevel::state_t ns)
{
  // Stop button.
  if (ns == Toplevel::is_running)
    {
      qt_runstop->setText ("Stop");
      qt_runstop->setDisabled (false);
      qt_runstop->setToolTip ("Press here to stop the program.");
    }
  else
    {
      qt_runstop->setDisabled (true);
    }
}

UIRun::UIRun (const BlockModel& al)
  : UIQt (al),
    qt_main (application_name ()),
    qt_name (new QLabel),
    qt_file (new QLabel),
    qt_description (new QLabel),
    qt_edit_pane (new QVBoxLayout),
    qt_select_filter (new QComboBox),
    qt_select_component (new QComboBox),
    qt_select_model (new QComboBox),
    qt_new_component (new QComboBox),
    qt_new_parent (new QComboBox),
    selected_file (Attribute::None ()),
    selected_filter (UIFilter::default_filter ()),
    selected_component ("program"),
    selected_model ("Daisy"),
    selected_new_component ("program"),
    selected_new_parent ("Daisy"),
    has_loaded_log_file (false),
    top_level (NULL)
{ }

UIRun::~UIRun ()
{ stop_program (); }

static struct UIRunSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIRun (al); }

  UIRunSyntax ()
    : DeclareModel (UI::component, "GUI", "Run the program in a window.")
  { }
  void load_frame (Frame& frame) const
  {

  }
} UIRun_syntax;

// ui_Qt_run.C ends here.
