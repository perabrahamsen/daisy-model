// qmain_item.C -- Items in the parameter tree.

#include "qmain_item.h"
#include "qmain_tree.h"
#include "qmain_busy.h"
#include "qmain.h"

#include "tmpstream.h"
#include "treelog_stream.h"
#include "traverse_depend.h"
#include "syntax.h"
#include "alist.h"

#include <qmessagebox.h>

MainWindow* 
TreeItem::main () const
{
  QListView* list_view = listView ();
  assert (list_view);
  MainTree* main_tree = dynamic_cast<MainTree*> (list_view);
  assert (main_tree);
  return main_tree->main;
}

void
TreeItem::find_path (vector<string>& path) const
{
  if (parent ())
    {
      const TreeItem* myParent = dynamic_cast<const TreeItem*> (parent ());
      assert (myParent);

      myParent->find_path (path);
    }
  path.push_back (entry.latin1 ());
}

void
TreeItem::setSelected (bool s)
{
  main ()->set_selection_editable (false);
  main ()->set_selection_copyable (false);
  main ()->set_selection_viewable (false);

  if (s)
    {
      if (const AListItem* container
	  = dynamic_cast<const AListItem*> (parent ()))
	{
	  if (container->syntax.lookup (entry.latin1 ()) == Syntax::Number)
	    main ()->set_selection_raw_editable (true);
	  else
	    main ()->set_selection_raw_editable (false);
	}
      else
	main ()->set_selection_raw_editable (false);
      main ()->set_selection_checkable (!errors.isEmpty ());
      vector<string> path;
      find_path (path);
      main ()->set_selection_depable (path.size () == 2);
      main ()->set_description (description);
    }
  else
    {
      main ()->set_selection_raw_editable (false);
      main ()->set_selection_checkable (false);
      main ()->set_selection_depable (false);
      main ()->clear_description ();
    }
  QListViewItem::setSelected (s);
}

QString
TreeItem::key (int, bool) const
{
  QString tmp;
  tmp.sprintf ("%08d%s", order, entry.latin1 ());
  return tmp;
}

void 
TreeItem::edit_raw ()
{ }

void 
TreeItem::edit_delete ()
{ }

void 
TreeItem::view_check ()
{ 
  QString title = QString ("QDaisy: Check ") + entry;
  QMessageBox::information (main (), title, errors);
}

void 
TreeItem::view_dependencies ()
{ 
  vector<string> path;
  find_path (path);
  const string& component = path[0];
  const string& model = path[1];

  TmpStream errors;
  TreelogStream treelog (errors ());
  QString title = QString ("QDaisy: ") + entry + " dependencies";

  bool found = false;
  
  // Check Libraries.
  {
    Busy busy (main (), "Checking libraries...");
    Treelog::Open nest (treelog, "Libraries");
    if (check_dependencies (component, model, treelog))
      found = true;
  }
  // Check simulation.
  {
    Busy busy (main (), "Checking simulation...");
    if (check_dependencies (component, model, 
			    main ()->daisy_syntax, main ()->daisy_alist, 
			    "Daisy", treelog))
      found = true;
  }
  if (found)
    QMessageBox::information (main (), title, errors.str ());
  else
    QMessageBox::information (main (), title, "No dependencies found.");
}

TreeItem::TreeItem (const QString& d, const QString& err, TreeItem* i,
		    const QString& e, const QString& t, 
		    const QString& v, const QString& c, int o)
  : QListViewItem (i, e, t, v, c),
    entry (e),
    description (d),
    errors (err),
    order (o)
{ }

TreeItem::TreeItem (const QString& d, QListView* i, 
		    const QString& e, const QString& v)
  : QListViewItem (i, e, "", v, ""),
    entry (e),
    description (d),
    errors (""),
    order (0)
{ }

TreeItem::~TreeItem ()
{ }

void
AListItem::setSelected (bool s)
{
  TreeItem::setSelected (s);
}

AListItem::AListItem (const Syntax& syn, AttributeList& al,
		      const AttributeList& def_al,
		      const QString& d, const QString& err,
		      TreeItem* i,
		      const QString& e, const QString& t, const QString& v,
		      const QString& c, int o = 0)
  : TreeItem (d, err, i, e, t, v, c, o),
    syntax (syn),
    alist (al),
    default_alist (def_al)
{ }

AListItem::~AListItem ()
{ }
