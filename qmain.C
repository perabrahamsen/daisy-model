// qmain.C --- Qt interface to Daisy.

#include "qmain.h"

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"

#include <qapplication.h>
#include <qmenubar.h>
#include <qvbox.h>
#include <qlabel.h>

int 
main (int argc, char** argv)
{
  // Application.
  QApplication app (argc, argv);
  MainWindow main_window;
  
  // Initialize it.
  main_window.populate_tree ();
  main_window.set_selection_editable (false);
  main_window.set_selection_copyable (false);
  main_window.set_selection_viewable (false);

  // View it.
  app.setMainWidget (&main_window);
  main_window.show ();
  return app.exec ();
}

MainWindow::MainWindow (QWidget* parent, const char* name)
  : QMainWindow (parent, name),
    view_logonly (true),
    view_parameters (true)
{
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
  description = new QLabel (qmain);

  // Menu bar.
  QMenuBar* menu = new QMenuBar (this);

  // - File menu.
  QPopupMenu* menu_file = new QPopupMenu (this);
  menu->insertItem ("&File", menu_file);
  menu_file->insertItem ("&Open...", this, SLOT (menu_action ()));
  menu_file->insertItem ("&Save", this, SLOT (menu_action ()));
  menu_file->insertItem ("S&ave as...", this, SLOT (menu_action ()));
  menu_file->insertSeparator ();
  menu_file->insertItem ("Run", this, SLOT (menu_action ()));
  menu_file->insertItem ("Check", this, SLOT (menu_action ()));
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
    = menu_view->insertItem ("&Check model...", this, SLOT (menu_action ()));
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
  help->insertItem ("&About", this, SLOT (menu_action ()));
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
	  QString value = syntax.check (alist) ? "Full" : "";
	  QString description = "no description";
	  if (alist.check ("description"))
	    description = alist.name ("description").c_str ();
	  MyListViewItem* qmodel 
	    = new MyListViewItem (this, description, qcomponent,
				  model.c_str (), "", value, "");

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
	  if (syntax.syntax (entry).check (alist.alist (entry)))
	    value_name = "Full";
	  else
	    value_name = "Partial";
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
	  if (syntax.library (entry).syntax (type).check (alist.alist (entry)))
	    value_name += " full";
	  else
	    value_name += " partial";
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
    default:
      break;
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
    new MyListViewItem (this, description, node,
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
MainWindow::set_description (const QString& s)
{ description->setText (s); }

void 
MainWindow::menu_action ()
{ }

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
  main->set_selection_checkable (false);

  if (s)
    main->set_description (description);
  else
    main->set_description ("no selection");
  QListViewItem::setSelected (s);
}

MyListViewItem::MyListViewItem (MainWindow* m, const QString& d,
				MyListViewItem* i,
				const QString& e, const QString& t, 
				const QString& v, const QString& c, 
				int o)
  : QListViewItem (i, e, t, v, c),
    main (m),
    description (d),
    order (o)
{ }

MyListViewItem::MyListViewItem (MainWindow* m, const QString& d, QListView* i, 
				const QString& e)
  : QListViewItem (i, e),
    main (m),
    description (d),
    order (0)
{ }
