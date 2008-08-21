// qmain_edit.h -- Edit alist items in a dialog window.
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

#ifndef QMAIN_EDIT_H
#define QMAIN_EDIT_H

#include "alist.h"

struct Syntax;
struct PLF;

#include <qdialog.h>
#include <qhgroupbox.h>

struct QCheckBox;
struct QVBox;
struct QLineEdit;
struct QListView;
struct QPushButton;
struct QLabel;
struct QComboBox;
struct QListViewItem;

#include <string>

class EditEntry : public QHGroupBox
{ 
  Q_OBJECT

  // Content.
protected:
  QCheckBox* check;
  const Syntax& syntax;
  AttributeList& alist;
  const AttributeList& default_alist;
  const std::string& parameter;

  // Use.
public:
  virtual void reset () = 0;
  virtual void apply () = 0;
  virtual bool valid () = 0;

  // Create and Destroy.
public:
  EditEntry (QWidget* parent, 
	     const Syntax& syn, AttributeList& al, 
	     const AttributeList& def_al, 
	     const std::string& par);
  virtual ~EditEntry ();

  // Signals.
public slots:
  virtual void change () = 0;

};

class EditBoolean : public EditEntry
{ 
  Q_OBJECT
  
  // Children.
private:
  bool value;
  QPushButton* toggle;
  QLabel* state;

  // Utilities.
  void refresh ();

  // Use.
  void reset ();
  void apply ();
  bool valid ();
  void change ();

  // Create and Destroy.
public:
  EditBoolean (QWidget* parent, 
	       const Syntax& syn, AttributeList& al, 
	       const AttributeList& def_al, 
	       const std::string& par);

  // Signals.
public slots:
  void invert ();
};

class EditPLF : public EditEntry
{ 
  Q_OBJECT
  
  // Children.
private:
  QVBox* vbox;
  QLineEdit* x;
  QLineEdit* y;
  QListView* table;

  // Utilities.
  void refresh (const PLF& value);

  // Use.
  void reset ();
  void apply ();
  bool valid ();
  void change ();

  // Create and Destroy.
public:
  EditPLF (QWidget* parent, 
	   const Syntax& syn, AttributeList& al, 
	   const AttributeList& def_al, 
	   const std::string& par);

  // Signals.
public slots:
  void add ();
  void remove ();
  void view ();
};

class EditObject : public EditEntry
{ 
  Q_OBJECT
  
  // Children.
private:
  AttributeList value;
  QComboBox* choice;

  // Utilities.
  void refresh ();

  // Use.
  void reset ();
  void apply ();
  bool valid ();
  void change ();

  // Create and Destroy.
public:
  EditObject (QWidget* parent, 
	      const Syntax& syn, AttributeList& al, 
	      const AttributeList& def_al, 
	      const std::string& par,
	      const std::string& component, const std::string& parameterization);

  // Signals.
public slots:
  void activate (int);
};

class EditList : public EditEntry
{ 
  Q_OBJECT
  
  // Children.
protected:
  QVBox* vbox;
  QLineEdit* edit;
  QListView* table;

  // Use.
private:
  bool valid ();
  void insert (double order);

  // Create and Destroy.
public:
  EditList (QWidget* parent, 
	    const Syntax& syn, AttributeList& al, 
	    const AttributeList& def_al, 
	    const std::string& par, const std::string& dim = "");

  // Signals.
public slots:
  void before ();
  void at ();
  void after ();
  void remove ();
  void select (QListViewItem*);
};

class ItemDialog : public QDialog
{ 
  Q_OBJECT

  // Content.
  EditEntry* entry;

  // Create and Destroy.
public:
  ItemDialog (QWidget* parent, 
	      const Syntax& syntax, AttributeList& alist, 
	      const AttributeList& default_alist, 
	      const std::string& parameter,
	      const std::string& component, const std::string& parameterization);
  ~ItemDialog ();

  // Signals.
public slots:
  void apply ();
  void reset ();
  void cancel ();
};

#endif // QMAIN_EDIT_H
