// qmain_tree.C -- The parameter tree.
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

#include "qmain_tree.h"
#include "qmain_item.h"
#include "qmain_populate.h"

#include "common.h"

#include <map>

class TreeState
{
  // State.
private:
  QString selected;
  map<QString, TreeState*> children;

  // Recreate.
private:
  void recreate_list (QListViewItem* firstChild);
  void recreate (TreeItem*);
public:
  void recreate (MainTree*);

  // Create & Destroy.
private:
  void add_list (QListViewItem*);
  TreeState (TreeItem*);
public:
  TreeState (MainTree*);
  ~TreeState ();
};

void
TreeState::recreate_list (QListViewItem* firstChild)
{
  for (QListViewItem* i = firstChild; i != NULL; i = i->nextSibling ())
    {
      TreeItem* ti = dynamic_cast<TreeItem*> (i);
      daisy_assert (ti);
      if (selected == ti->entry)
	ti->listView ()->setSelected (ti, true);
      if (children.find (ti->entry) != children.end ())
	{
	  ti->setOpen (true);
	  children[ti->entry]->recreate (ti);
	}
    }
}

void
TreeState::recreate (TreeItem* item)
{ recreate_list (item->firstChild ()); }

void
TreeState::recreate (MainTree* item)
{ recreate_list (item->firstChild ()); }

void 
TreeState::add_list (QListViewItem* firstChild)
{
  for (QListViewItem* i = firstChild; i != NULL; i = i->nextSibling ())
    {
      TreeItem* ti = dynamic_cast<TreeItem*> (i);
      daisy_assert (ti);
      if (ti->isSelected ())
	selected = ti->entry;
      if (ti->isOpen ())
	children[ti->entry] = new TreeState (ti);
    }
}

TreeState::TreeState (TreeItem* item)
{ add_list (item->firstChild ()); }

TreeState::TreeState (MainTree* tree)
{ add_list (tree->firstChild ()); }

TreeState::~TreeState ()
{
  for (map<QString, TreeState*>::iterator i = children.begin ();
       i != children.end ();
       i++)
    {
      delete (*i).second;
      (*i).second = NULL;
    }
}

TreeItem*
MainTree::item () const
{
  QListViewItem* current = currentItem ();
  daisy_assert (current);
  TreeItem* mine = dynamic_cast<TreeItem*> (current);
  daisy_assert (mine);
  return mine;
}

void
MainTree::populate (bool check_composite, const Syntax& syntax, 
		    AttributeList& alist, const AttributeList& default_alist)
{
  TreeState state (this);
  ::populate_tree (main, check_composite, syntax, alist, default_alist);
  state.recreate (this);
}

void
MainTree::edit_edit ()
{ item ()->edit_edit (); }

void
MainTree::edit_raw ()
{ 
  TreeItem* old = item ();
  if (old->edit_raw ())
    delete old; 
}

bool
MainTree::edit_after ()
{ return item ()->edit_after (); }

bool
MainTree::edit_child ()
{ return item ()->edit_child (); }

bool
MainTree::edit_copy ()
{ return item ()->edit_copy (); }

bool
MainTree::edit_inherit ()
{ return item ()->edit_inherit (); }

bool
MainTree::edit_delete ()
{ 
  TreeItem* old = item ();
  if (old->edit_delete ())
    {
      const bool repopulate = ((dynamic_cast<ModelItem*> (old) != NULL)
			       || dynamic_cast<SequenceItem*> (old) != NULL);
      delete old;
      return repopulate;
    }
  return false;
}

void
MainTree::view_selected ()
{ item ()->view_selected (); }

void
MainTree::view_check ()
{ item ()->view_check (); }

bool
MainTree::toggle_view_defaults ()
{ return item ()->toggle_view_defaults (); }

void 
MainTree::view_dependencies ()
{ item ()->view_dependencies (); }

MainTree::MainTree (QWidget* w, MainWindow* m)
  : QListView (w),
    main (m)
{ }

MainTree::~MainTree ()
{ }
