// qmain.C --- Qt interface to Daisy.

// Q Frontend includes.
#include "qmain.h"
#include "qmain_tree.h"
#include "qmain_populate.h"
#include "qmain_busy.h"

// Daisy backend includes.
#include "daisy.h"
#include "library.h"
#include "version.h"
#include "parser_file.h"
#include "printer_file.h"
#include "tmpstream.h"

// Q Toolkit includes.
#include <qapplication.h>
#include <qmenubar.h>
#include <qvbox.h>
#include <qlabel.h>
#include <qhgroupbox.h>
#include <qmessagebox.h>
#include <qfiledialog.h>
#include <qstatusbar.h>

int 
main (int argc, char** argv)
{
  // Application.
  QApplication app (argc, argv);
  Busy::set_global_app (&app);
  MainWindow main_window;
  
  // Initialize it.
  main_window.set_nofile ();
  main_window.populate_tree ();

  // View it.
  app.setMainWidget (&main_window);
  main_window.show ();
  return app.exec ();
}

MainWindow::MainWindow ()
  : QMainWindow (),
    view_logonly (true),
    view_parameters (true),
    view_filter (0),
    view_empty (false)
{
  // Daisy.
  Daisy::load_syntax (daisy_syntax, daisy_default_alist);
  daisy_alist = daisy_default_alist;

  // Arrange tree and description.
  QVBox* qmain = new QVBox (this);
  setCentralWidget (qmain);

  // The central tree.
  tree = new MainTree (qmain, this);
  tree->addColumn ("Parameter");
  tree->addColumn ("Type");
  tree->addColumn ("Value");
  tree->addColumn ("Category");
  tree->setRootIsDecorated (true);

  // The description.
  QHGroupBox* dbox = new QHGroupBox ("Description", qmain);
  description = new QLabel (dbox);

  // Menu bar.
  QMenuBar* menu = new QMenuBar (this);

  // - File menu.
  menu_file = new QPopupMenu (this);
  menu->insertItem ("&File", menu_file);
  menu_file->insertItem ("&New", this, SLOT (file_new ()));
  menu_file->insertItem ("&Open...", this, SLOT (file_open ()));
  menu_file_save_id 
    = menu_file->insertItem ("&Save", this, SLOT (file_save ()));
  menu_file->insertItem ("S&ave as...", this, SLOT (file_save_as ()));
  menu_file->insertSeparator ();
  int menu_file_run_id 
    = menu_file->insertItem ("&Run", this, SLOT (menu_action ()));
  menu_file->setItemEnabled (menu_file_run_id, false);
  int menu_file_check_id 
    = menu_file->insertItem ("&Check", this, SLOT (menu_action ()));
  menu_file->setItemEnabled (menu_file_check_id, false);
  menu_file->insertSeparator ();
  menu_file->insertItem ("E&xit",  qApp, SLOT (quit()));
  
  // - Edit menu.
  menu_edit = new QPopupMenu (this);
  menu->insertItem ("&Edit", menu_edit);
  menu_edit_edit_id 
    = menu_edit->insertItem ("&Edit...", this, SLOT (menu_action ()));
  menu_edit_raw_id 
    = menu_edit->insertItem ("&Raw...", this, SLOT (edit_raw ()));
  menu_edit_copy_id 
    = menu_edit->insertItem ("&Copy...", this, SLOT (menu_action ()));
  menu_edit_inherit_id 
    = menu_edit->insertItem ("In&herit...", this, SLOT (menu_action ()));
  menu_edit_delete_id 
    = menu_edit->insertItem ("&Delete...", this, SLOT (edit_delete ()));
  menu_edit->insertSeparator ();
  int menu_edit_simulation_id 
    = menu_edit->insertItem ("&Simulation...", this, SLOT (menu_action ()));
  menu_edit->setItemEnabled (menu_edit_simulation_id, false);
  int menu_edit_inputs_id 
    = menu_edit->insertItem ("&Inputs...", this, SLOT (menu_action ()));
  menu_edit->setItemEnabled (menu_edit_inputs_id, false);
  int menu_edit_preferences_id 
    = menu_edit->insertItem ("&Preferences...", this, SLOT (menu_action ()));
  menu_edit->setItemEnabled (menu_edit_preferences_id, false);
  
  // - View menu.
  menu_view = new QPopupMenu (this);
  menu->insertItem ("&View", menu_view);
  menu_view->setCheckable (true);
  menu_view_selected_id
    = menu_view->insertItem ("&Selected...", this, SLOT (menu_action ()));
  menu_view_check_id
    = menu_view->insertItem ("&Check model...", this, SLOT (view_check ()));
  menu_view_dependencies_id
    = menu_view->insertItem ("&Dependencies...", 
			     this, SLOT (view_dependencies ()));
  menu_view->setItemEnabled (menu_view_dependencies_id, false);
  menu_view->insertSeparator ();
  menu_view_logonly_id
    = menu_view->insertItem ("Include L&og variables",
			     this, SLOT (toggle_view_logonly ()));
  menu_view->setItemChecked (menu_view_logonly_id, view_logonly);
  menu_view_parameters_id
    = menu_view->insertItem ("Include &parameters", 
			     this, SLOT (toggle_view_parameters ()));
  menu_view->setItemChecked (menu_view_parameters_id, view_parameters);
  menu_view->insertSeparator ();
  for (unsigned int i = 0; i != Filter::filters.size (); i++)
    {
      Filter::filters[i]->menu_id
	= menu_view->insertItem (Filter::filters[i]->menu_entry (), 
				 Filter::filters[i],
				 SLOT (select_filter ()));
      Filter::filters[i]->view_filter = i;
      Filter::filters[i]->main = this;
    }
  menu_view->setItemChecked (Filter::filters[view_filter]->menu_id, true);
  menu_view->insertSeparator ();
  menu_view_empty_id 
    = menu_view->insertItem ("Include e&mpty libraries", 
			     this, SLOT (toggle_view_empty ()));
  menu_view->setItemChecked (menu_view_empty_id, view_empty);

  // - Help menu.
  menu->insertSeparator ();
  QPopupMenu* help = new QPopupMenu (this);
  menu->insertItem ("&Help", help);
  help->insertItem ("&About...", this, SLOT (help_about ()));
  help->insertItem ("&About Qt...", this, SLOT (help_aboutQt ()));
}

void
MainWindow::daisy_clear ()
{ 
  Busy busy (this, "Daisy cleanup...");
  // Clear simulation.
  daisy_alist = daisy_default_alist;
  
  // Clear libraries.
  Library::clear_all_parsed ();
}

void
MainWindow::set_nofile ()
{ 
  file_name = "";
  setCaption ("QDaisy: untitled");
  menu_file->setItemEnabled (menu_file_save_id, false);
}
  
void
MainWindow::set_filename (QString name)
{ 
  file_name = name;
  setCaption (QString ("QDaisy: " + name));
  menu_file->setItemEnabled (menu_file_save_id, true);
}

void
MainWindow::new_file ()
{ 
  // Delete old content.
  daisy_clear ();

  // Make it official.
  set_nofile ();
  populate_tree ();
}

void
MainWindow::open_file (QString name)
{ 
  // Delete old content.
  daisy_clear ();

  // Load new content.
  Busy busy (this, "Parsing file...");
  TmpStream errors;
  ParserFile parser (daisy_syntax, name.latin1 (), errors ());
  parser.load (daisy_alist);
  NotBusy notbusy;
  if (parser.error_count ())
    QMessageBox::critical (this, "QDaisy: Load errors", errors.str ());
  else if (strlen (errors.str ()) > 0)
    QMessageBox::warning (this, "QDaisy: Load warnings", errors.str ());

  // In any case:  Make it official.
  set_filename (name);
  populate_tree ();
}

void
MainWindow::save_file ()
{ 
  Busy busy (this, "Saving file...");
  // Open log file.
  PrinterFile printer (file_name.latin1 ());
  printer.print_comment ("Created by QDaisy");

  // Print input files.
  if (daisy_alist.check ("parser_inputs"))
    {
      const vector<AttributeList*> inputs 
	(daisy_alist.alist_sequence ("parser_inputs"));
      printer.print_comment ("Input files.");
      for (unsigned int i = 0; i < inputs.size (); i++)
	printer.print_input (*inputs[i]);
    }

  // Print included files.
  printer.print_comment ("Parameterizations");
  printer.print_library_file (file_name.latin1 ());

    // Print content.
  printer.print_comment ("Content");
  printer.print_alist (daisy_alist, daisy_syntax);
  if (!printer.good ())
    {
      NotBusy notbusy;
      QMessageBox::critical (this, "QDaisy: Save errors", "Save failed.");   
    }
}

void
MainWindow::set_selection_viewable (bool viewable)
{
  menu_view->setItemEnabled (menu_view_selected_id, viewable);
}

void
MainWindow::set_selection_checkable (bool checkable)
{
  menu_view->setItemEnabled (menu_view_check_id, checkable);
}

void
MainWindow::set_selection_editable (bool editable)
{
  menu_edit->setItemEnabled (menu_edit_edit_id, editable);
}

void
MainWindow::set_selection_raw_editable (bool editable)
{
  menu_edit->setItemEnabled (menu_edit_raw_id, editable);
  menu_edit->setItemEnabled (menu_edit_delete_id, editable);
}

void
MainWindow::set_selection_copyable (bool copyable)
{
  menu_edit->setItemEnabled (menu_edit_copy_id, copyable);
  menu_edit->setItemEnabled (menu_edit_inherit_id, copyable);
}

void
MainWindow::set_selection_depable (bool depable)
{
  menu_view->setItemEnabled (menu_view_dependencies_id, depable);
}

void 
MainWindow::set_view_filter (unsigned int filter)
{
  view_filter = filter;
  for (unsigned int i = 0; i < Filter::filters.size (); i++)
    menu_view->setItemChecked (Filter::filters[i]->menu_id, i == filter);
}

void
MainWindow::populate_tree ()
{
  Busy busy (this, "Populating tree...");
  ::populate_tree (this);
  clear_description ();
  set_selection_editable (false);
  set_selection_raw_editable (false);
  set_selection_copyable (false);
  set_selection_viewable (false);
  set_selection_checkable (false);
}

void 
MainWindow::set_description (const QString& s)
{ description->setText (s); }

void 
MainWindow::clear_description ()
{ description->setText (daisy_alist.name ("description").c_str ()); }

void 
MainWindow::menu_action ()
{ }

void 
MainWindow::file_new ()
{ 
  new_file ();
}

void 
MainWindow::file_open ()
{ 
  QString file (QFileDialog::getOpenFileName (QString::null,
					      "Daisy setup files (*.dai)", 
					      this));
  if (!file.isEmpty())
    {
      set_view_filter (1);
      open_file (file);
    }
}

void 
MainWindow::file_save ()
{
  save_file ();
}

void 
MainWindow::file_save_as ()
{
  QString file (QFileDialog::getSaveFileName (QString::null,
					      "Daisy setup files (*.dai)", 
					      this));
  if (!file.isEmpty())
    {
      if (file == file_name)
	save_file ();
      else
	{
	  Library::refile_parsed (file_name.latin1 (), file.latin1 ());
	  set_filename (file);
	  save_file ();
	  populate_tree ();
	}
    }
}

void
MainWindow::edit_raw ()
{ tree->edit_raw (); }

void
MainWindow::edit_delete ()
{ tree->edit_delete (); }


void 
MainWindow::view_check ()
{ tree->view_check (); }

void 
MainWindow::view_dependencies ()
{ tree->view_dependencies (); }

void 
MainWindow::toggle_view_logonly ()
{
  view_logonly = !view_logonly;
  menu_view->setItemChecked (menu_view_logonly_id, view_logonly);
  populate_tree ();
}

void 
MainWindow::toggle_view_parameters ()
{
  view_parameters = !view_parameters;
  menu_view->setItemChecked (menu_view_parameters_id, view_parameters);
  populate_tree ();
}

void 
MainWindow::select_view_filter (unsigned int filter)
{
  if (filter != view_filter)
    {
      set_view_filter (filter);
      populate_tree ();
    }
}

void 
MainWindow::toggle_view_empty ()
{
  view_empty = !view_empty;
  menu_view->setItemChecked (menu_view_empty_id, view_empty);
  populate_tree ();
}

void 
MainWindow::help_about ()
{ 
  static const QString about = QString ("QDaisy version ") + version + ".";
  QMessageBox::about (this, "QDaisy: About", about);
}

void 
MainWindow::help_aboutQt ()
{ 
  QMessageBox::aboutQt (this, "QDaisy: About Qt");
}

vector<Filter*> Filter::filters;

bool 
Filter::check (const string& /*comp*/) const
{ return true; }

bool 
Filter::check (const string& /*comp*/, const string& /*model*/) const
{ return true; }

Filter::~Filter ()
{ }

Filter::Filter ()
{ filters.push_back (this); }

void
Filter::select_filter ()
{ main->select_view_filter (view_filter); }

class FilterBuildin : public Filter
{
  const QString& menu_entry () const
  {
    static const QString name = "&Buildin models";
    return name;
  }
  bool check (const string& comp, const string& model) const
  {
    const Library& library = Library::find (comp);
    const AttributeList& alist = library.lookup (model);
    return !alist.check ("parsed_from_file");
  }
} filter_buildin;  

class FilterFile : public Filter
{
  const QString& menu_entry () const
  {
    static const QString name = "Current &file";
    return name;
  }
  bool check (const string& comp, const string& model) const
  {
    const Library& library = Library::find (comp);
    const AttributeList& alist = library.lookup (model);
    return alist.check ("parsed_from_file")
      && alist.name ("parsed_from_file") == main->file_name.latin1 ();
  }
} filter_current_file;  

class FilterLibraries : public Filter
{
  const QString& menu_entry () const
  {
    static const QString name = "&Libraries";
    return name;
  }
  bool check (const string& comp, const string& model) const
  {
    const Library& library = Library::find (comp);
    const AttributeList& alist = library.lookup (model);
    return alist.check ("parsed_from_file")
      && alist.name ("parsed_from_file") != main->file_name.latin1 ();
  }
} filter_libraries;  

class FilterEverything : public Filter
{
  const QString& menu_entry () const
  {
    static const QString name = "&Everything";
    return name;
  }
} filter_everything;  
