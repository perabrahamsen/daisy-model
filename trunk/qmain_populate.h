// qmain_populate.h -- Populate the parameter tree.
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

#ifndef QMAIN_POPULATE_H
#define QMAIN_POPULATE_H

struct Syntax;
struct AttributeList;

struct MainWindow;
struct AListItem;
struct TreeItem;

void populate_tree (MainWindow* main, bool check_alists,
		    const Syntax& syntax, AttributeList& alist, 
		    const AttributeList& default_alist);
void populate_alist (AListItem* item);
void populate_parameter (AListItem* parent, TreeItem* child);

#endif // QMAIN_POPULATE_H
