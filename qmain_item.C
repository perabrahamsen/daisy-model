// qmain_item.C -- Items in the parameter tree.
//
// Copyright 1996-2001 Per Abrahamsen.
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "qmain_item.h"
#include "qmain_edit.h"
#include "qmain_tree.h"
#include "qmain_populate.h"
#include "qmain_busy.h"
#include "qmain.h"

#include "library.h"
#include "tmpstream.h"
#include "treelog_stream.h"
#include "depend.h"
#include "traverse_delete.h"

#include <qmessagebox.h>
#include <qinputdialog.h>

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

bool
TreeItem::container (string& component, string& parameterization)
{
  TreeItem* p = dynamic_cast<TreeItem*> (parent ());
  assert (p);
  return p->container (component, parameterization);
};

void
TreeItem::edit_edit ()
{ assert (false); }

bool
TreeItem::edit_raw ()
{ assert (false); }

bool
TreeItem::edit_after ()
{ assert (false); }

bool
TreeItem::edit_child ()
{ assert (false); }

bool
TreeItem::edit_copy ()
{ assert (false); }

bool
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
{ return QString ("2") + entry; }

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

bool
AtomItem::edit_child ()
{ return insert_before (0); }
  
void
AtomItem::set_selected ()
{
  if (editable ())
    main ()->set_selection_deletable (true);

  const AListItem* c = dynamic_cast<const AListItem*> (parent ());
  assert (c);
  main ()->set_description (c->description (entry));
  const string parameter = entry.latin1 ();
  const Syntax::type type = c->syntax.lookup (parameter);
  assert (type != Syntax::Error);
  if ((type == Syntax::AList || type == Syntax::Object)
      && c->syntax.size (parameter) != Syntax::Singleton)
    {
      main ()->set_selection_childable (true);
    }
  else if (editable ())
    main ()->set_selection_raw_editable (true);
}

bool
AtomItem::insert_before (int where)
{ 
  const AListItem* c = dynamic_cast<const AListItem*> (parent ());
  assert (c); 
  const string parameter = entry.latin1 ();
  const Syntax::type type = c->syntax.lookup (parameter);
  assert (c->syntax.size (parameter) != Syntax::Singleton);
  vector<AttributeList*> alists = c->alist.alist_sequence (parameter);
  if (type == Syntax::AList)
    {
      AttributeList alist (c->syntax.default_alist (parameter));
      vector<AttributeList*> entry;
      assert (where <= alists.size ());
      entry.insert (entry.end (), alists.begin (), &alists[where]);
      entry.push_back (&alist);
      entry.insert (entry.end (), &alists[where], alists.end ());
      c->alist.add (parameter, entry);
    }
  else
    {
      assert (type == Syntax::Object);
      string component;
      string parameterization;
      dep_map dependencies;
      if (container (component, parameterization))
	{
	  assert (component.length () > 0);
	  find_dependencies (component, parameterization, dependencies);
	  resequence (component, parameterization, dependencies);
	}

      const Library& library = c->syntax.library (parameter);
      QStringList choices;
      vector<string> models;
      library.entries (models);
      assert (models.size () > 0);
      for (unsigned int i = 0; i < models.size (); i++)
	if (dependencies[component].find (models[i]) 
	    == dependencies[component].end ())
	  choices += models[i].c_str ();
      bool ok; 
      QString choice 
	= QInputDialog::getItem ("QDaisy: Insert child",
				 "Type:", choices, 0, false, &ok,
				 main ());
      const string name = choice.latin1 ();
      if (!ok || !library.check (name))
	return false;
      AttributeList alist (library.lookup (name));
      alist.add ("type", name);
      vector<AttributeList*> entry;
      assert (where <= alists.size ());
      entry.insert (entry.end (), alists.begin (), &alists[where]);
      entry.push_back (&alist);
      entry.insert (entry.end (), &alists[where], alists.end ());
      c->alist.add (parameter, entry);
    }
  return true;
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

void
AListItem::recreate_item (TreeItem* item)
{ 
  populate_parameter (this, item);
  for (QListViewItem* i = firstChild (); i != NULL; i = i->nextSibling ())
    {
      TreeItem* c = dynamic_cast<TreeItem*> (i);
      assert (c);
      if (c != item && c->entry == item->entry)
	{
	  if (SubmodelItem* cc = dynamic_cast<SubmodelItem*> (c))
	    {
	      if (SubmodelItem* ii = dynamic_cast<SubmodelItem*> (item))
		cc->view_defaults = ii->view_defaults;
	    }
	  c->listView ()->setSelected (c, true);
	  c->setOpen (item->isOpen ());
	  break;
	}
    }
}

bool
AListItem::edit_item (TreeItem* item)
{ 
  const string parameter = item->entry.latin1 ();
  string component;
  string parameterization;
  container (component, parameterization);
  ItemDialog edit_item (main (), syntax, alist, default_alist, parameter,
			component, parameterization);

  switch (edit_item.exec ())
    {
    case QDialog::Rejected:
      return false;
    case QDialog::Accepted:
      recreate_item (item);
      return true;
    }
  assert (false);
}

bool
AListItem::delete_item (TreeItem* item)
{ 
  const string parameter = item->entry.latin1 ();
  if (!alist.revert (parameter, default_alist, syntax))
    return false;
  
  recreate_item (item);
  return true;
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

AListItem::AListItem (const Syntax& syn, AttributeList& al,
		      const AttributeList& al_def,
		      QListView* tree, const QString& name)
  : TreeItem (tree, name, ""),
    syntax (syn),
    alist (al),
    default_alist (al_def),
    view_defaults (false)
{ assert (&al != &al_def); }

bool 
ModelItem::container (string& component, string& parameterization)
{
  LibraryItem* p = dynamic_cast<LibraryItem*> (parent ());
  assert (p);
  component = p->entry.latin1 ();
  parameterization = entry.latin1 ();
  return true;
}

bool 
ModelItem::editable () const
{ return editable_; }

void
ModelItem::set_selected ()
{
  AListItem::set_selected ();

  string component;
  string parameterization;
  container (component, parameterization);
  Library& library = Library::find (component);
  assert (library.check (parameterization));
  const AttributeList& alist = library.lookup (parameterization);

  if (editable ())
    main ()->set_selection_deletable (true);
  main ()->set_selection_depable (true);  
  main ()->set_selection_copyable (alist.check ("type"));
  main ()->set_selection_inheritable (true);
  
  if (alist.check ("description"))
    main ()->set_description (alist.name ("description").c_str ());
  else
    main ()->set_description ("Model with no description.");
}

bool
ModelItem::edit_copy ()
{ 
  // Where are we.
  string component;
  string parameterization;
  container (component, parameterization);
  
  // Get new name.
  bool ok;
  QString name = QInputDialog::getText ("QDaisy: Inherit model", 
					"Name of new parameterization",
					entry, &ok, main ());
  if (!ok || name.isEmpty ())
    return false;
  
  // Check that it doesn't already exists.
  string child = name.latin1 ();
  Library& library = Library::find (component);
  if (library.check (child))
    {
      QMessageBox::warning (main (), 
			    "QDaisy: Can't create new parameterization",
			    "The name is already in use.");
      return false;
    }

  // Find superclass.
  assert (library.check (parameterization));
  const AttributeList& alist = library.lookup (parameterization);
  assert (alist.check ("type"));
  const string& super = alist.name ("type");

  // Create new attribute derived from its superclass.
  const AttributeList& sl = library.lookup (parameterization);
  AttributeList& atts = *new AttributeList (sl);
  // Pretent we got this parameterization from the current file..
  atts.add ("parsed_from_file", main ()->file_name.latin1 ());
  atts.add ("parsed_sequence", Library::get_sequence ());
  // Add new object to library.
  library.add_derived (child, atts, super);
  return true;
}

bool
ModelItem::edit_inherit ()
{ 
  // Where are we.
  string component;
  string parameterization;
  container (component, parameterization);
  
  // Get new name.
  bool ok;
  QString name = QInputDialog::getText ("QDaisy: Inherit model", 
					"Name of new parameterization",
					entry, &ok, main ());
  if (!ok || name.isEmpty ())
    return false;
  
  // Check that it doesn't already exists.
  string child = name.latin1 ();
  Library& library = Library::find (component);
  if (library.check (child))
    {
      QMessageBox::warning (main (), 
			    "QDaisy: Can't create new parameterization",
			    "The name is already in use.");
      return false;
    }

  // Create new attribute derived from its superclass.
  const AttributeList& sl = library.lookup (parameterization);
  AttributeList& atts = *new AttributeList (sl);
  // Pretent we got this parameterization from the current file..
  atts.add ("parsed_from_file", main ()->file_name.latin1 ());
  atts.add ("parsed_sequence", Library::get_sequence ());
  // Add new object to library.
  library.add_derived (child, atts, parameterization);
  return true;
}

bool
ModelItem::edit_delete ()
{ 
  LibraryItem* par = dynamic_cast<LibraryItem*> (parent ());
  assert (par);
  const string& component = par->entry.latin1 ();
  const string& model = entry.latin1 ();
  
  QString title = QString ("QDaisy: Removing ") + entry;
  
  bool found = false;
  
  // Check Libraries.
  {
    Busy busy (main (), "Checking libraries...");
    if (has_dependencies (component, model))
      found = true;
  }
  // Check simulation.
  {
    Busy busy (main (), "Checking simulation...");
    if (has_dependencies (component, model, 
			  main ()->daisy_syntax, main ()->daisy_alist, 
			  "Daisy"))
      found = true;
  }
  if (found)
    {
      switch (QMessageBox::warning (main (), "Deleting parameterization", "\
There are other objects depending on this one.\n\
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
  AListItem::set_selected ();
  if (editable ())
    main ()->set_selection_deletable (true);
  const AListItem* c = dynamic_cast<const AListItem*> (parent ());
  assert (c); 
  main ()->set_description (c->description (entry));
  const string parameter = entry.latin1 ();
  const Syntax::type type = c->syntax.lookup (parameter);
  assert (type == Syntax::AList || type == Syntax::Object);
  assert (c->syntax.size (parameter) == Syntax::Singleton);
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

SubmodelItem::SubmodelItem (const Syntax& syn, AttributeList& al,
			    const AttributeList& al_def,
			    QListView* tree, const QString& name)
  : AListItem (syn, al, al_def, tree, name)
{ } 

void
ObjectItem::set_selected ()
{
  SubmodelItem::set_selected ();
  if (editable ())
    main ()->set_selection_raw_editable (true);
}
bool
ObjectItem::edit_raw ()
{
  AListItem* c = dynamic_cast<AListItem*> (parent ());
  assert (c); 
  return c->edit_item (this); 
}

ObjectItem::ObjectItem (const Syntax& syn, AttributeList& al,
			const AttributeList& al_def,
			TreeItem* i,
			const QString& e, const QString& t, 
			const QString& v, const QString& c,
			int o)
  : SubmodelItem (syn, al, al_def, i, e, t, v, c, o)
{ }

bool
SimulationItem::editable () const
{ return true; }

void
SimulationItem::set_selected ()
{
  AListItem::set_selected ();
  main ()->set_description (alist.name ("description").c_str ());
}

bool
SimulationItem::edit_delete ()
{ assert (false); }

QString 
SimulationItem::key (int, bool) const
{ return QString ("1") + entry; }

bool 
SimulationItem::container (string&, string&)
{ return false; }

SimulationItem::SimulationItem (const Syntax& syn, AttributeList& al,
				const AttributeList& al_def,
				QListView* tree, const QString& name)
  : SubmodelItem (syn, al, al_def, tree, name)
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
SequenceItem::edit_after ()
{ 
  assert (order > 0);
  AtomItem* c = dynamic_cast<AtomItem*> (parent ());
  assert (c);
  return c->insert_before (order);
}

bool
SequenceItem::edit_delete ()
{ 
  assert (order > 0);
  AtomItem* c = dynamic_cast<AtomItem*> (parent ());
  assert (c);
  const string parameter = c->entry.latin1 ();
  const AListItem* cc = dynamic_cast<const AListItem*> (c->parent ());
  assert (cc);
  assert (cc->syntax.size (parameter) != Syntax::Singleton);
  vector<AttributeList*> alists = cc->alist.alist_sequence (parameter);
  assert (order <= alists.size ());
  alists.erase (&alists[order-1]);
  cc->alist.add (parameter, alists);
  return true; 
}

void
SequenceItem::set_selected ()
{
  AListItem::set_selected ();
  const AtomItem* c = dynamic_cast<const AtomItem*> (parent ());
  assert (c);
  const AListItem* cc = dynamic_cast<const AListItem*> (c->parent ());
  assert (cc);
  const string parameter = c->entry.latin1 ();
  Syntax::type type = cc->syntax.lookup (parameter);
  assert (type == Syntax::AList || type == Syntax::Object);
  if (editable ())
    main ()->set_selection_deletable (true);
  main ()->set_selection_afterable (true);
  main ()->set_description ("Item in sequence.");
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

QString 
InputsItem::key (int, bool) const
{ return QString ("0") + entry; }

bool 
InputsItem::editable () const
{ return false; }

void 
InputsItem::set_selected ()
{
  main ()->set_description ("Externally defined parameterizations.");
  main ()->set_selection_childable (true);
}

bool
InputsItem::edit_child ()
{ return insert_before (0); }

bool
InputsItem::insert_before (int /* where */)
{ 
  // TODO.
  return false;
}

InputsItem::InputsItem (const vector<AttributeList*>& in, 
			QListView* i, const QString& e)
  : TreeItem (i, e, QString::number (in.size ()) + " entries"),
    inputs (in)
{ }

bool
InputItem::editable () const
{ return false; }

bool
InputItem::edit_after ()
{ 
  assert (order >= 0);
  InputsItem* c = dynamic_cast<InputsItem*> (parent ());
  assert (c);
  return c->insert_before (order+1);
}

bool
InputItem::edit_delete ()
{ 
  // TODO: Warn if anything depends on objects from this file.
  // TODO: Delete objects from this file.

  assert (order >= 0);
  InputsItem* c = dynamic_cast<InputsItem*> (parent ());
  assert (c);
  vector<AttributeList*>& alists = c->inputs;
  assert (order < alists.size ());
  alists.erase (&alists[order]);
  
  return true; 
}

void
InputItem::set_selected ()
{
  main ()->set_selection_deletable (true);
  main ()->set_selection_afterable (true);
  main ()->set_description ("Externally defined parameterization.");
}

InputItem::InputItem (const Syntax& syn, AttributeList& al,
		      const AttributeList& al_def,
		      InputsItem* i,
		      const QString& e, const QString& t, const QString& v,
		      const QString& c, int o)
  : AListItem (syn, al, al_def, i, e, t, v, c, o)
{ }
