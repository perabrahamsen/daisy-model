// qmain_tree.C -- The parameter tree.

#include "qmain_tree.h"
#include "qmain_item.h"

TreeItem*
MainTree::item () const
{
  QListViewItem* current = currentItem ();
  assert (current);
  TreeItem* mine = dynamic_cast<TreeItem*> (current);
  assert (mine);
  return mine;
}

void
MainTree::edit_edit ()
{ item ()->edit_edit (); }

void
MainTree::edit_raw ()
{ item ()->edit_raw (); }

void
MainTree::edit_after ()
{ item ()->edit_after (); }

void
MainTree::edit_child ()
{ item ()->edit_child (); }

void
MainTree::edit_copy ()
{ item ()->edit_copy (); }

void
MainTree::edit_inherit ()
{ item ()->edit_inherit (); }

void
MainTree::edit_delete ()
{ item ()->edit_delete (); }

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
