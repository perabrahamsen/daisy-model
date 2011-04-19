// qmain_item.h -- Items in the parameter tree.
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

#ifndef QMAIN_ITEM_H
#define QMAIN_ITEM_H

struct MainWindow;

struct AttributeList;
struct Syntax;

#include <qlistview.h>
#include <qstring.h>

#include <string>
#include <vector>

struct AListItem;

class TreeItem : public QListViewItem
{ 
  // Content.
public:
  MainWindow* main () const;
  const QString entry;
  int order;

  // QListViewItem.
public:
  void setSelected (bool s);
  virtual bool editable () const = 0;
protected:
  virtual void set_selected () = 0;
  QString key (int column, bool) const;
  virtual bool container (std::string& component, std::string& parameterization);

  // Commands.
public:
  virtual void edit_edit ();
  virtual bool edit_raw ();
  virtual bool edit_after ();
  virtual bool edit_child ();
  virtual bool edit_copy ();
  virtual bool edit_inherit ();
  virtual bool edit_delete ();
  virtual void view_check ();
  virtual bool toggle_view_defaults ();
  virtual void view_selected ();
  virtual void view_dependencies ();

  // Create and Destroy.
protected:
  TreeItem (TreeItem* i,	// Nested.
	    const QString& e, const QString& t, const QString& v,
	    const QString& c, int o = 0);
  TreeItem (QListView* i, const QString& e, const QString& v); // Top level.
};

class LibraryItem : public TreeItem
{
  // QListViewItem.
protected:
  bool editable () const;
  void set_selected ();
  QString key (int column, bool) const;

  // Create and Destroy.
public:
  LibraryItem (QListView* i, const QString& e, const QString& v);
};

class AtomItem : public TreeItem
{
  // Overwrite.
private:
  bool editable () const;
  bool edit_raw ();
  bool edit_delete ();
  bool edit_child ();
  void set_selected ();

  // Utilities.
public:
  bool insert_before (int where);

  // Create and Destroy.
public:
  AtomItem (TreeItem* i,	// Nested.
	    const QString& e, const QString& t, const QString& v,
	    const QString& c, int o = 0);
};

class AListItem : public TreeItem
{ 
  // Content.
public:
  const Syntax& syntax;
  AttributeList& alist;
  const AttributeList& default_alist;
  bool view_defaults;

  // Overwrite.
protected:
  void set_selected ();
public:
  void view_check ();
  bool toggle_view_defaults ();

  // Container.
public:
  void recreate_item (TreeItem* item);
  bool edit_item (TreeItem* item);
  bool delete_item (TreeItem* item);
  QString description (const QString& par) const;

  // Create and Destroy.
public:
  AListItem (const Syntax& syn, AttributeList& al, const AttributeList& al_def,
	     TreeItem* i,
	     const QString& e, const QString& t, const QString& v,
	     const QString& c, int o = 0);
  AListItem (const Syntax& syn, AttributeList& al,
	     const AttributeList& al_def,
	     QListView* i, const QString& e);
};

class ModelItem : public AListItem
{
  // Content.
private:
  const bool editable_;

  // Overwrite.
private:
  bool container (std::string& component, std::string& parameterization);
  bool editable () const;
  void set_selected ();
  bool edit_copy ();
  bool edit_inherit ();
  bool edit_delete ();
  void view_dependencies ();

   // Create and Destroy.
public:
  ModelItem (const Syntax& syn, AttributeList& al, const AttributeList& al_def,
	     TreeItem* i,
	     const QString& e, const QString& v, const QString& c,
	     int o, bool editable);
}; 

class SubmodelItem : public AListItem
{
  // Overwrite.
public:
  bool editable () const;
private:
  bool edit_delete ();
protected:
  void set_selected ();

   // Create and Destroy.
public:
  SubmodelItem (const Syntax& syn, AttributeList& al,
		const AttributeList& al_def,
		TreeItem* i,
		const QString& e, const QString& t, const QString& v,
		const QString& c, int o);
  SubmodelItem (const Syntax& syn, AttributeList& al,
		const AttributeList& al_def,
		QListView* i, const QString& e);
}; 

class ObjectItem : public SubmodelItem
{
  // Overwrite.
public:
  bool edit_raw ();
  void set_selected ();

   // Create and Destroy.
public:
  ObjectItem (const Syntax& syn, AttributeList& al,
	      const AttributeList& al_def,
	      TreeItem* i,
	      const QString& e, const QString& t, const QString& v,
	      const QString& c, int o);
}; 

class SimulationItem : public SubmodelItem
{
  // Overwrite.
private:
  bool editable () const;
  bool edit_delete ();
  void set_selected ();
  QString key (int column, bool) const;
  bool container (std::string& component, std::string& parameterization);
  
   // Create and Destroy.
public:
  SimulationItem (const Syntax& syn, AttributeList& al,
		  const AttributeList& al_def,
		  QListView* i, const QString& e);
}; 

class SequenceItem : public AListItem
{
  // Overwrite.
private:
  bool editable () const;
  bool edit_after ();
  bool edit_delete ();
  void set_selected ();

   // Create and Destroy.
public:
  SequenceItem (const Syntax& syn, AttributeList& al,
		const AttributeList& al_def,
		TreeItem* i,
		const QString& e, const QString& t, const QString& v,
		const QString& c, int o);
}; 

class DefaultItem : public AListItem
{
  // Content.
private:
  bool editable () const;

   // Create and Destroy.
public:
  DefaultItem (const Syntax& syn, AttributeList& al,
	       const AttributeList& al_def,
	       TreeItem* i,
	       const QString& e, const QString& t, const QString& v,
	       const QString& c, int o);
}; 

class InputsItem : public TreeItem
{
  // Content.
public:
  std::vector<AttributeList*> inputs;

  // Overwrite.
  QString key (int column, bool) const;
  bool editable () const;
  bool edit_child ();
  void set_selected ();

  // Utilities.
public:
  bool insert_before (int where);

  // Create and Destroy.
public:
  InputsItem (const std::vector<AttributeList*>&
	      in, QListView* i, const QString& e);
};

class InputItem : public AListItem
{
  // Overwrite.
private:
  bool editable () const;
  bool edit_after ();
  bool edit_delete ();
  void set_selected ();

  // Create and Destroy
public:
  InputItem (const Syntax& syn, AttributeList& al,
	     const AttributeList& al_def,
	     InputsItem* i,
	     const QString& e, const QString& t, const QString& v,
	     const QString& c, int o);
};

#endif // QMAIN_ITEM_H
