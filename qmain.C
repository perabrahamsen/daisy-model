// qmain.C --- Qt interface to Daisy.

#include "qmain.h"

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include "version.h"
#include "plf.h"
#include "parser_file.h"
#include "printer_file.h"

#include <qapplication.h>
#include <qmenubar.h>
#include <qvbox.h>
#include <qlabel.h>
#include <qhgroupbox.h>
#include <qmessagebox.h>
#include <qfiledialog.h>

int 
main (int argc, char** argv)
{
  // Application.
  QApplication app (argc, argv);
  MainWindow main_window;
  
  // Initialize it.
  main_window.set_nofile ();
  main_window.populate_tree ();
  main_window.set_description ("No selection", NULL);
  main_window.set_selection_editable (false);
  main_window.set_selection_copyable (false);
  main_window.set_selection_viewable (false);

  // View it.
  app.setMainWidget (&main_window);
  main_window.show ();
  return app.exec ();
}

MainWindow::MainWindow ()
  : QMainWindow (),
    view_logonly (true),
    view_parameters (true),
    errors (NULL)
{
  // Daisy.
  Daisy::load_syntax (daisy_syntax, daisy_alist);

  // Arrange tree and description.
  QVBox* qmain = new QVBox (this);
  setCentralWidget (qmain);

  // The central tree.
  tree = new QListView (qmain);
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
  menu_file->insertItem ("S&ave as...", this, SLOT (menu_action ()));
  menu_file->insertSeparator ();
  menu_file->insertItem ("&Run", this, SLOT (menu_action ()));
  menu_file->insertItem ("&Check", this, SLOT (menu_action ()));
  menu_file->insertSeparator ();
  menu_file->insertItem ("E&xit",  qApp, SLOT (quit()));
  
  // - Edit menu.
  menu_edit = new QPopupMenu (this);
  menu->insertItem ("&Edit", menu_edit);
  menu_edit_edit_id 
    = menu_edit->insertItem ("&Edit...", this, SLOT (menu_action ()));
  menu_edit_raw_id 
    = menu_edit->insertItem ("&Raw...", this, SLOT (menu_action ()));
  menu_edit_copy_id 
    = menu_edit->insertItem ("&Copy...", this, SLOT (menu_action ()));
  menu_edit_inherit_id 
    = menu_edit->insertItem ("In&herit...", this, SLOT (menu_action ()));
  menu_edit_delete_id 
    = menu_edit->insertItem ("&Delete...", this, SLOT (menu_action ()));
  menu_edit->insertSeparator ();
  menu_edit->insertItem ("&Simulation...", this, SLOT (menu_action ()));
  menu_edit->insertItem ("&Inputs...", this, SLOT (menu_action ()));
  menu_edit->insertItem ("&Preferences...", this, SLOT (menu_action ()));
  
  // - View menu.
  menu_view = new QPopupMenu (this);
  menu->insertItem ("&View", menu_view);
  menu_view_selected_id
    = menu_view->insertItem ("View &selected...", this, SLOT (menu_action ()));
  menu_view_check_id
    = menu_view->insertItem ("&Check model...", this, SLOT (view_check ()));
  menu_view->insertSeparator ();
  menu_view_logonly_id
    = menu_view->insertItem ("Include L&og variables",
			     this, SLOT (toggle_view_logonly ()));
  menu_view->setItemChecked (menu_view_logonly_id, view_logonly);
  menu_view_parameters_id
    = menu_view->insertItem ("Include &Parameters", 
			     this, SLOT (toggle_view_parameters ()));
  menu_view->setItemChecked (menu_view_parameters_id, view_parameters);
  menu_view->insertSeparator ();
  menu_view_current_id 
    = menu_view->insertItem ("C&urrent file", this, SLOT (menu_action ()));
  menu_view_buildin_id
    = menu_view->insertItem ("&Buildin models", this, SLOT (menu_action ()));
  menu_view_library_id 
    = menu_view->insertItem ("&Library parameterizations",
			     this, SLOT (menu_action ()));
  menu_view_all_id 
    = menu_view->insertItem ("&Everything", this, SLOT (menu_action ()));
  menu_view->setCheckable (true);
  menu_view->setItemChecked (menu_view_buildin_id, true);

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
  // Clear simulation.
  daisy_alist.clear ();
  Syntax dummy;
  Daisy::load_syntax (dummy, daisy_alist);
  
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
  tree->clear ();
  populate_tree ();
}

void
MainWindow::open_file (QString name)
{ 
  // Delete old content.
  daisy_clear ();

  // Load new content.
  ostrstream errors;
  ParserFile parser (daisy_syntax, name.ascii (), errors);
  parser.load (daisy_alist);
  const char* s = errors.str ();
  if (parser.error_count ())
    QMessageBox::critical (this, "QDaisy: Load errors", s);
  else if (strlen (s) > 0)
    QMessageBox::warning (this, "QDaisy: Load warnings", s);
  delete [] s;

  // In any case:  Make it official.
  set_filename (name);
  tree->clear ();
  populate_tree ();
}

void
MainWindow::save_file ()
{ 
  // Open log file.
  PrinterFile printer (file_name.ascii ());
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
  printer.print_library_file (file_name.ascii ());

  // Print content.
  printer.print_comment ("Content");
  printer.print_alist (daisy_alist, daisy_syntax);

  if (!printer.good ())
    QMessageBox::critical (this, "QDaisy: Save errors", "Save failed.");   
}

void
MainWindow::set_selection_viewable (bool viewable)
{
  menu_view->setItemEnabled (menu_view_selected_id, viewable);
}

void
MainWindow::set_selection_editable (bool editable)
{
  menu_edit->setItemEnabled (menu_edit_edit_id, editable);
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
MainWindow::populate_tree ()
{
  vector<string> components;
  Library::all (components);
  for (unsigned int i = 0; i < components.size (); i++)
    {
      const string& component = components[i];
      const Library& library = Library::find (component);
      MyListViewItem* qcomponent 
	= new MyListViewItem (this, library.description (), tree,
			      component.c_str ());

      vector<string> models;
      library.entries (models);
      for (unsigned int i = 0; i < models.size (); i++)
	{
	  const string& model = models[i];
	  const Syntax& syntax = library.syntax (model);
	  const AttributeList& alist = library.lookup (model);
	  ostrstream str;
	  const bool has_errors = !syntax.check (alist, str, model);
	  const char* errors = str.str ();
	  QString value =  has_errors ? "" : "Full";
	  QString description = "no description";
	  if (alist.check ("description"))
	    description = alist.name ("description").c_str ();
	  QString type;
	  if (alist.check ("parsed_from_file"))
	    type = QString ("\"")
	      + alist.name ("parsed_from_file").c_str () + "\"";
	  MyListViewItem* qmodel 
	    = new MyListViewItem (this, description,
				  has_errors ? new QString (errors) : NULL,
				  qcomponent, model.c_str (), type, value, "");
	  delete [] errors;

	  add_alist_children (qmodel, syntax, alist);
	}
    }
}

void 
MainWindow::add_alist_children (MyListViewItem* node, 
				const Syntax& syntax, 
				const AttributeList& alist)
{
  const vector<string>& order = syntax.order ();
  for (unsigned int i = 0; i < order.size (); i++)
    add_alist_entry (node, syntax, alist, order[i]);

  vector<string> parameters;
  syntax.entries (parameters);
  for (unsigned int i = 0; i < parameters.size (); i++)
    {
      const string& parameter = parameters[i];
      if (syntax.order (parameter) < 0)
	add_alist_entry (node, syntax, alist, parameter);
    }
}

void
MainWindow::add_alist_entry (MyListViewItem* node, 
			     const Syntax& syntax, const AttributeList& alist,
			     const string& entry)
{
  // Get the data.
  QString entry_name =  entry.c_str ();
  Syntax::type type = syntax.lookup (entry);
  QString type_name = Syntax::type_name (type);
  int size = syntax.size (entry);
  bool has_value = alist.check (entry);
  QString value_name = has_value ? "<has value>" : "";
  QString description = syntax.description (entry).c_str ();

  // Set category name and order.
  QString category_name;
  int order = syntax.order (entry);
  if (syntax.is_log (entry))
    {
      if (!view_logonly)
	return;
      category_name = "Log variable";
      if (order < 0)
	order = 9999;
    }
  else if (syntax.is_const (entry))
    {
      if (!view_parameters)
	return;
      category_name = "Parameter";
      if (order < 0)
	order = 7777;
    }
  else if (syntax.is_state (entry))
    {
      category_name = "State variable";
      if (order < 0)
	order = 8888;
    }
  else
    assert (false);

  if (syntax.is_optional (entry))
    category_name += " (optional)";

  // Value specific changes.
  if (has_value && size != Syntax::Singleton)
    {
      value_name = QString::number (alist.size (entry));
      value_name += " elements";
    }

  // Type specific changes.
  bool has_errors = false;
  QString errors;
  switch (type)
    {
    case Syntax::AList:
      {
	const AttributeList& child 
	  = (alist.check (entry) && size == Syntax::Singleton)
	  ? alist.alist (entry)
	  : syntax.default_alist (entry);
	if (child.check ("submodel"))
	  {
	    type_name = "`";
	    type_name += child.name ("submodel").c_str ();
	    type_name += "' submodel";
	  }
	else
	  type_name = "Submodel";

	if (child.check ("description"))
	  {
	    QString child_description = child.name ("description").c_str ();
	    if (description != child_description)
	      {
		description += "\n--\n";
		description += child_description;
	      }
	  }
	if (has_value && size == Syntax::Singleton)
	  {
	    ostrstream str;
	    has_errors = !syntax.syntax (entry).check (alist.alist (entry), 
						       str, entry);
	    const char* tmp = str.str ();
	    errors = tmp;
	    delete [] tmp;
	    if (has_errors)
	      value_name = "Partial";
	    else
	      value_name = "Full";
	  }
      }
      break;
    case Syntax::Object:
      type_name = "`";
      type_name += syntax.library (entry).name ().c_str ();
      type_name += "' component";
      if (has_value && size == Syntax::Singleton 
	  && alist.alist (entry).check ("type"))
	{
	  string type = alist.alist (entry).name ("type");
	  value_name = "`";
	  value_name += type.c_str ();
	  value_name += "'";
	  ostrstream str;
	  has_errors 
	    = !syntax.library (entry).syntax (type).check (alist.alist (entry),
							   str, entry);
	  const char* tmp = str.str ();
	  errors = tmp;
	  delete [] tmp;
	  if (has_errors)
	    value_name += " partial";
	  else
	    value_name += " full";
	}
      break;
    case Syntax::Number:
      type_name = syntax.dimension (entry).c_str ();
      if (size == Syntax::Singleton && has_value)
	value_name = QString::number (alist.number (entry));
      break;
    case Syntax::Integer:
      if (size == Syntax::Singleton && has_value)
	value_name = QString::number (alist.integer (entry));
      break;
    case Syntax::Boolean:
      if (size == Syntax::Singleton && has_value)
	value_name = alist.flag (entry) ? "true" : "false";
      break;
    case Syntax::PLF:
      type_name = "PLF: ";
      type_name += syntax.range (entry).c_str ();
      type_name += " -> ";
      type_name += syntax.domain (entry).c_str ();
      if (has_value && size == Syntax::Singleton)
	{
	  value_name = "<";
	  value_name += QString::number (alist.plf (entry).size ());
	  value_name += " points>";
	}
      break;
    case Syntax::String:
      if (has_value && size == Syntax::Singleton)
	{
	  value_name = "<";
	  value_name += QString::number (alist.name (entry).length ());
	  value_name += " characters>";
	}
      break;
    case Syntax::Date:
      if (has_value && size == Syntax::Singleton)
	{
	  Time time (alist.time (entry));
	  value_name.sprintf ("%04d-%02d-%02dT%02d",
			      time.year (), time.month (), time.mday (), 
			      time.hour ());
	}
      break;
    case Syntax::Library:
    case Syntax::Error:
    default:
      assert (false);
    }

  // Size specific changes.
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    type_name += " sequence";
  else
    {
      type_name += " array [";
      type_name += QString::number (size);
      type_name += "]";
    }

  // Create it.
  MyListViewItem* item = 
    new MyListViewItem (this, description, 
			has_errors ? new QString (errors) : NULL,
			node,
			entry_name, type_name, value_name, 
			category_name, order);

  // Children.
  switch (type)
    {
    case Syntax::AList:
      {
	const Syntax& entry_syntax = syntax.syntax (entry);
	const AttributeList& entry_alist
	  = (size == Syntax::Singleton && alist.check (entry))
	  ? alist.alist (entry)
	  : syntax.default_alist (entry);

	add_alist_children (item, entry_syntax, entry_alist);
      }
      break;
    case Syntax::Object:
      break;
    default:
      /* do nothing */
      break;
    }
}

void 
MainWindow::set_description (const QString& s, const QString* e)
{ 
  description->setText (s); 
  errors = e;
  menu_view->setItemEnabled (menu_view_check_id, e != NULL);
}

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
    open_file (file);
}

void 
MainWindow::file_save ()
{
  save_file ();
}
void 
MainWindow::view_check ()
{ 
  assert (errors);
  const QListViewItem* current = tree->currentItem ();
  assert (current);
  const MyListViewItem* mine = dynamic_cast<const MyListViewItem*> (current);
  assert (mine);
  QString title = QString ("QDaisy: Check ") + mine->entry;
  QMessageBox::information (this, title, *errors);
}

void 
MainWindow::toggle_view_logonly ()
{
  view_logonly = !view_logonly;
  menu_view->setItemChecked (menu_view_logonly_id, view_logonly);
  tree->clear ();
  populate_tree ();
}

void 
MainWindow::toggle_view_parameters ()
{
  view_parameters = !view_parameters;
  menu_view->setItemChecked (menu_view_parameters_id, view_parameters);
  tree->clear ();
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

QString
MyListViewItem::key (int column, bool ascending) const
{
  if (column == 3)
    {
      QString tmp;
      tmp.sprintf ("%04d", order);
      return tmp;
    }
  else
    return QListViewItem::key (column, ascending);
}

void
MyListViewItem::setSelected (bool s)
{
  main->set_selection_editable (false);
  main->set_selection_copyable (false);
  main->set_selection_viewable (false);

  if (s)
    main->set_description (description, errors);
  else
    main->set_description ("no selection", NULL);
  QListViewItem::setSelected (s);
}

MyListViewItem::MyListViewItem (MainWindow* m, const QString& d,
				const QString* err,
				MyListViewItem* i,
				const QString& e, const QString& t, 
				const QString& v, const QString& c, 
				int o)
  : QListViewItem (i, e, t, v, c),
    main (m),
    entry (e),
    description (d),
    errors (err),
    order (o)
{ }

MyListViewItem::MyListViewItem (MainWindow* m, const QString& d, QListView* i, 
				const QString& e)
  : QListViewItem (i, e),
    main (m),
    entry (e),
    description (d),
    errors (NULL),
    order (0)
{ }

MyListViewItem::~MyListViewItem ()
{
  if (errors)
    delete errors;
}
