// qmain.h --- Qt interface to Daisy.
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

#ifndef QMAIN_H
#define QMAIN_H

// Daisy
#include "syntax.h"
#include "alist.h"

// Qt
#include <qmainwindow.h>
struct QLabel;

// C++
#include <string>

// Qt Frondend
struct MainTree;

class MainWindow : public QMainWindow
{
  Q_OBJECT
public:
  MainWindow ();

  // Simulation.
public:
  Syntax daisy_syntax;
  AttributeList daisy_default_alist;
  AttributeList daisy_alist;
  void daisy_clear ();

  // Files.
public:
  QString file_name;
  void set_nofile ();
  void set_filename (QString file);
  void new_file ();
  void open_file (QString file);
  void save_file ();

  // Menu item ids.
private:
  QPopupMenu* menu_file;
  int menu_file_save_id;
  QPopupMenu* menu_edit;
  int menu_edit_edit_id;
  int menu_edit_raw_id;
  int menu_edit_after_id;
  int menu_edit_child_id;
  int menu_edit_copy_id;
  int menu_edit_inherit_id;
  int menu_edit_delete_id;
  QPopupMenu* menu_view;
  int menu_view_selected_id;
  int menu_view_check_id;
  int menu_view_defaults_id;
  int menu_view_dependencies_id;
  int menu_view_logonly_id;
  int menu_view_parameters_id;
  int menu_view_empty_id;
  int menu_view_check_composite_id;
public:
  void set_selection_editable (bool editable);
  void set_selection_raw_editable (bool editable);
  void set_selection_afterable (bool afterable);
  void set_selection_childable (bool childable);
  void set_selection_copyable (bool copyable);
  void set_selection_inheritable (bool inheritable);
  void set_selection_deletable (bool deletable);
  void set_selection_defaults_shown (bool shown);
  void set_selection_showable (bool showable);
  void set_selection_viewable (bool viewable);
  void set_selection_checkable (bool checkable);
  void set_selection_depable (bool cepable);
  void clear_selection ();
  void set_view_filter (unsigned int i); 

  // The tree.
public:
  MainTree* tree;
  bool view_logonly;
  bool view_parameters;
  unsigned int view_filter;
  bool view_empty;
  bool check_composite;
  void populate_tree ();

  // The description.
private:
  QLabel* description;
public:
  void set_description (const QString& des);
  void clear_description ();

  // Signals.
public slots:
  void menu_action ();
  void file_new ();
  void file_open ();
  void file_save ();
  void file_save_as ();
  void edit_edit ();
  void edit_raw ();
  void edit_after ();
  void edit_child ();
  void edit_copy ();
  void edit_inherit ();
  void edit_delete ();
  void view_selected ();
  void view_check ();
  void toggle_view_defaults ();
  void view_dependencies ();
  void toggle_view_logonly ();
  void toggle_view_parameters ();
  void toggle_view_empty ();
  void toggle_check_composite ();
  void select_view_filter (unsigned int i); 
  void help_about ();
  void help_aboutQt ();
};

class Filter : public QObject
{
  Q_OBJECT
public:
  Filter ();
  virtual ~Filter ();

  // Content
  static std::vector<Filter*> filters;
  int menu_id;
  MainWindow* main;
  unsigned int view_filter;

  // Subclass
  virtual const QString& menu_entry () const = 0;
  virtual bool check (const std::string& comp) const;
  virtual bool check (const std::string& comp, const std::string& model) const;

  // Signals.
public slots:
  void select_filter ();
};

#endif // QMAIN_H
