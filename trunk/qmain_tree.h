// qmain_tree.h -- The parameter tree.
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

#ifndef QMAIN_TREE_H
#define QMAIN_TREE_H

#include <qlistview.h>

#include <string>

struct AttributeList;
struct Syntax;

struct MainWindow;
struct TreeItem;

class MainTree : public QListView
{
  // Content.
public:
  MainWindow *const main;
private:
  TreeItem* item () const;
  
  // Utility.
public:
  void populate (bool check_composite, const Syntax& syntax, 
		 AttributeList& alist, const AttributeList& default_alist);

  // Use.
public:
  void edit_edit ();
  void edit_raw ();
  bool edit_after ();
  bool edit_child ();
  bool edit_copy ();
  bool edit_inherit ();
  bool edit_delete ();
  void view_check ();
  bool toggle_view_defaults ();
  void view_selected ();
  void view_dependencies ();

  // Create and Destroy.
public:
  MainTree (QWidget*, MainWindow*);
  ~MainTree ();
};

#endif // QMAIN_TREE_H
