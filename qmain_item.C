// qmain_item.C -- Items in the parameter tree.

#include "qmain_item.h"
#include "qmain_edit.h"
#include "qmain_tree.h"
#include "qmain_populate.h"
#include "qmain_busy.h"
#include "qmain.h"

#include "library.h"
#include "tmpstream.h"
#include "treelog_stream.h"
#include "traverse_depend.h"
#include "traverse_delete.h"

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
TreeItem::setSelected (bool s)
{
  main ()->clear_description ();
  main ()->clear_selection ();

  if (s)
    {
      main ()->set_selection_raw_editable (editable ());
      set_selected ();
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
TreeItem::edit_edit ()
{ assert (false); }

bool
TreeItem::edit_raw ()
{ assert (false); }

void
TreeItem::edit_after ()
{ assert (false); }

void
TreeItem::edit_child ()
{ assert (false); }

void
TreeItem::edit_copy ()
{ assert (false); }

void
TreeItem::edit_inherit ()
{ assert (false); }

bool
TreeItem::edit_delete ()
{ assert (false); }

void
TreeItem::view_selected ()
{ assert (false); }

void 
TreeItem::view_check ()
{ assert (false); }

bool 
TreeItem::toggle_view_defaults ()
{ assert (false); }

void 
TreeItem::view_dependencies ()
{ assert (false); }

TreeItem::TreeItem (TreeItem* i,
		    const QString& e, const QString& t, 
		    const QString& v, const QString& c, int o)
  : QListViewItem (i, e, t, v, c),
    entry (e),
    order (o)
{ }

TreeItem::TreeItem (QListView* i, const QString& e, const QString& v)
  : QListViewItem (i, e, "", v, ""),
    entry (e),
    order (0)
{ }

bool
LibraryItem::editable () const
{ return false; }

void
LibraryItem::set_selected ()
{
  const string component = entry.latin1 ();
  const Library& library = Library::find (component);
  main ()->set_description (library.description ());
}

QString 
LibraryItem::key (int, bool) const
{ return entry; }

LibraryItem::LibraryItem (QListView* i, const QString& e, const QString& v)
  : TreeItem (i, e, v)
{ }

bool
AtomItem::editable () const
{
  const TreeItem* c = dynamic_cast<const TreeItem*> (parent ());
  assert (c);
  return c->editable ();
}

bool
AtomItem::edit_raw ()
{
  AListItem* c = dynamic_cast<AListItem*> (parent ());
  assert (c); 
  return c->edit_item (this); 
}

bool
AtomItem::edit_delete ()
{ 
  AListItem* c = dynamic_cast<AListItem*> (parent ());
  assert (c); 
  return c->delete_item (this); 
}

void
AtomItem::edit_child ()
{ 
  const AListItem* c = dynamic_cast<const AListItem*> (parent ());
  assert (c); 
  const string parameter = entry.latin1 ();
  const Syntax::type type = c->syntax.lookup (parameter);
  assert (type == Syntax::AList || type == Syntax::Object);
  assert (c->syntax.size (parameter) != Syntax::Singleton);
}

void
AtomItem::set_selected ()
{
  const AListItem* c = dynamic_cast<const AListItem*> (parent ());
  assert (c);
  main ()->set_description (c->description (entry));
  const string parameter = entry.latin1 ();
  const Syntax::type type = c->syntax.lookup (parameter);
  assert (type != Syntax::Error);
  if (type == Syntax::AList || type == Syntax::Object)
    {
      assert (c->syntax.size (parameter) != Syntax::Singleton);
      main ()->set_selection_childable (true);
    }
}

AtomItem::AtomItem (TreeItem* i,
		    const QString& e, const QString& t, 
		    const QString& v, const QString& c, int o)
  : TreeItem (i, e, t, v, c, o)
{ assert (dynamic_cast<AListItem*> (i)); }

void
AListItem::set_selected ()
{
  main ()->set_selection_defaults_shown (view_defaults);
  main ()->set_selection_showable (true);
  main ()->set_selection_checkable (true);
}

void 
AListItem::view_check ()
{ 
  QString title = QString ("QDaisy: Check ") + entry;
  TmpStream errors;
  const bool ok = syntax.check (alist, errors (), entry.latin1 ());
  if (strlen (errors.str ()) > 0)
    QMessageBox::information (main (), title, errors.str ());
  else if (ok)
    QMessageBox::information (main (), title, "No errors found.");
  else
    QMessageBox::information (main (), title, "Strange errors found.");
}

bool
AListItem::toggle_view_defaults ()
{
  view_defaults = !view_defaults;
  populate_alist (this);
  return view_defaults;
}

bool
AListItem::edit_item (TreeItem* item)
{ 
  const string parameter = item->entry.latin1 ();
  ItemDialog edit_item (main (), syntax, alist, default_alist, parameter);

  switch (edit_item.exec ())
    {
    case QDialog::Rejected:
      return false;
    case QDialog::Accepted:
      populate_parameter (this, item);
      for (QListViewItem* i = firstChild (); i != NULL; i = i->nextSibling ())
	{
	  TreeItem* c = dynamic_cast<TreeItem*> (i);
	  assert (c);
	  if (c != item && c->entry == item->entry)
	    c->listView ()->setSelected (c, true);
	}
      return true;
    }
  assert (false);
}

bool
AListItem::delete_item (TreeItem*)
{ 
  return false;
}

QString 
AListItem::description (const QString& par) const
{ 
  const string parameter = par.latin1 ();
  if (syntax.lookup (parameter) != Syntax::Error)
    return syntax.description (parameter).c_str (); 
  return "Unknown item.";
}

AListItem::AListItem (const Syntax& syn, AttributeList& al,
		      const AttributeList& def_al,
		      TreeItem* i,
		      const QString& e, const QString& t, const QString& v,
		      const QString& c, int o = 0)
  : TreeItem (i, e, t, v, c, o),
    syntax (syn),
    alist (al),
    default_alist (def_al),
    view_defaults (false)
{ assert (&al != &def_al); }

bool 
ModelItem::editable () const
{ return editable_; }

void
ModelItem::set_selected ()
{
  AListItem::set_selected ();
  main ()->set_selection_depable (true);  
  main ()->set_selection_deletable (editable ());
  main ()->set_selection_copyable (true);
  if (alist.check ("description"))
    main ()->set_description (alist.name ("description").c_str ());
  else
    main ()->set_description ("Model with no description.");
}

bool
ModelItem::edit_raw ()
{ return false; }

void
ModelItem::edit_copy ()
{ }

void
ModelItem::edit_inherit ()
{ }

bool
ModelItem::edit_delete ()
{ 
  LibraryItem* par = dynamic_cast<LibraryItem*> (parent ());
  assert (par);
  const string& component = par->entry.latin1 ();
  const string& model = entry.latin1 ();
  
  TmpStream deps;
  TreelogStream treelog (deps ());
  QString title = QString ("QDaisy: Removing ") + entry;
  
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
    {
      switch (QMessageBox::warning (main (), "Deleting parameterization", "\
There are other object depending on this one.\n\
Really delete?",
			   "Yes", "No", 0, 1))
	{
	case 0:
	  break;
	case 1:
	  return false;
	default:
	  assert (false);
	}
    }
  remove_dependencies (component, model);
  remove_dependencies (component, model, 
		       main ()->daisy_syntax, main ()->daisy_alist);
  Library& library = Library::find (component);
  library.remove (model);
  return true;
}

void 
ModelItem::view_dependencies ()
{ 
  LibraryItem* par = dynamic_cast<LibraryItem*> (parent ());
  assert (par);
  const string& component = par->entry.latin1 ();
  const string& model = entry.latin1 ();
  
  TmpStream deps;
  TreelogStream treelog (deps ());
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
  if (strlen (deps.str ()) > 0)
    QMessageBox::information (main (), title, deps.str ());
  else if (found)
    QMessageBox::information (main (), title, "Strange dependencies found.");
  else
    QMessageBox::information (main (), title, "No dependencies found.");
}

ModelItem::ModelItem (const Syntax& syn, AttributeList& al, 
		      const AttributeList& al_def, 
		      TreeItem* i,
		      const QString& e, const QString& v, const QString& c, 
		      int o, bool editable)
  : AListItem (syn, al, al_def, i, e, "", v, c, o),
    editable_ (editable)
{ }

bool
SubmodelItem::editable () const
{
  const TreeItem* c = dynamic_cast<const TreeItem*> (parent ());
  assert (c);
  return c->editable ();
}

void
SubmodelItem::set_selected ()
{
  const AListItem* c = dynamic_cast<const AListItem*> (parent ());
  assert (c); 
  main ()->set_description (c->description (entry));
  const string parameter = entry.latin1 ();
  const Syntax::type type = c->syntax.lookup (parameter);
  assert (type == Syntax::AList || type == Syntax::Object);
  assert (c->syntax.size (parameter) == Syntax::Singleton);
  AListItem::set_selected ();
}

bool
SubmodelItem::edit_raw ()
{
  AListItem* c = dynamic_cast<AListItem*> (parent ());
  assert (c); 
  return c->edit_item (this); 
}

bool
SubmodelItem::edit_delete ()
{ 
  AListItem* c = dynamic_cast<AListItem*> (parent ());
  assert (c); 
  return c->delete_item (this); 
}

SubmodelItem::SubmodelItem (const Syntax& syn, AttributeList& al,
			    const AttributeList& al_def,
			    TreeItem* i,
			    const QString& e, const QString& t, 
			    const QString& v, const QString& c,
			    int o)
  : AListItem (syn, al, al_def, i, e, t, v, c, o)
{ assert (dynamic_cast<AListItem*> (i)); }

ObjectItem::ObjectItem (const Syntax& syn, AttributeList& al,
			const AttributeList& al_def,
			TreeItem* i,
			const QString& e, const QString& t, 
			const QString& v, const QString& c,
			int o)
  : SubmodelItem (syn, al, al_def, i, e, t, v, c, o)
{ }

bool
SequenceItem::editable () const
{
  const AtomItem* c = dynamic_cast<const AtomItem*> (parent ());
  assert (c);
  const AListItem* cc = dynamic_cast<const AListItem*> (c->parent ());
  assert (cc);
  return cc->editable ();
}

bool
SequenceItem::edit_raw ()
{ return false; }

void
SequenceItem::edit_after ()
{ }

bool
SequenceItem::edit_delete ()
{ return false; }

void
SequenceItem::set_selected ()
{
  const AtomItem* c = dynamic_cast<const AtomItem*> (parent ());
  assert (c);
  const AListItem* cc = dynamic_cast<const AListItem*> (c->parent ());
  assert (cc);
  const string parameter = c->entry.latin1 ();
  Syntax::type type = cc->syntax.lookup (parameter);
  assert (type == Syntax::AList || type == Syntax::Object);
  main ()->set_selection_deletable (true);
  main ()->set_selection_afterable (true);
  main ()->set_description ("Item in sequence.");
  AListItem::set_selected ();
}

SequenceItem::SequenceItem (const Syntax& syn, AttributeList& al,
			    const AttributeList& al_def,
			    TreeItem* i,
			    const QString& e, const QString& t, 
			    const QString& v, const QString& c,
			    int o)
  : AListItem (syn, al, al_def, i, e, t, v, c, o)
{ }

bool
DefaultItem::editable () const
{ return false; }

DefaultItem::DefaultItem (const Syntax& syn, AttributeList& al,
			  const AttributeList& al_def,
			  TreeItem* i,
			  const QString& e, const QString& t, 
			  const QString& v, const QString& c,
			  int o)
  : AListItem (syn, al, al_def, i, e, t, v, c, o)
{ }
