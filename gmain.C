// gmain.C --- Gtk-- interface to Daisy.
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

// @ Includes.

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <gtk--.h>

// @ Class Definitions.

class Main_Window : public Gtk_Window
{
public:
  gint delete_event_impl (GdkEventAny*);
  Main_Window ();
};

class Main_MenuBar : public Gtk_MenuBar
{
public:
  Main_MenuBar ();
};

class Tip_Tree : public Gtk_Tree
{
  vector<Gtk_Tooltips*> tips;
public:
  void daisy_add_tip (Gtk_Widget* item, const string& text);
  Tip_Tree ();
  ~Tip_Tree ();
};

class Main_Tree : public Tip_Tree
{
public:
  Main_Tree ();
};

class Libraries_Tree : public Tip_Tree
{
public:
  Libraries_Tree ();
};

class Library_Tree : public Tip_Tree
{
  const Library& library;
public:
  Library_Tree (const Library&);
};


class AList_Tree : public Tip_Tree
{
  const Syntax& syntax;
  const AttributeList& alist;
  void daisy_add_entry (const string& entry);
public:
  AList_Tree (const Syntax&, const AttributeList&);
};

// @ Main_Window.

gint 
Main_Window::delete_event_impl (GdkEventAny*)
{
  Gtk_Main::instance()->quit(); 
  return 0;
} 

Main_Window::Main_Window ()
{
  // Create menubar.
  Main_MenuBar* menubar = new Main_MenuBar ();
  menubar->show ();

  // Create the main tree.
  Main_Tree* tree = new Main_Tree ();
  tree->set_selection_mode (GTK_SELECTION_MULTIPLE);
  tree->show ();

  Gtk_ScrolledWindow* scroll = new Gtk_ScrolledWindow ();
  scroll->set_policy (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  scroll->add (tree);
  scroll->show ();

  // Combine it.
  Gtk_VBox* top = new Gtk_VBox ();
  top->pack_start (menubar, false, false);
  top->pack_start (scroll);
  top->show ();

  // Hint the window manager.
  add (top); 
  set_title ("GTK Daisy");
  set_wmclass ("gdaisy", "GDaisy");
  set_usize (200, 200);
}

// @ Main_MenuBar

Main_MenuBar::Main_MenuBar ()
{
  // Create Quit button.
  Gtk_MenuItem* quit = new Gtk_MenuItem ("Quit");
  connect_to_method (quit->activate, Gtk_Main::instance (), &Gtk_Main::quit);
  quit->show ();

  // Create File menu.
  Gtk_Menu* file_menu = new Gtk_Menu ();
  file_menu->append (quit);

  // Create File item.
  Gtk_MenuItem* file_item = new Gtk_MenuItem ("File");
  file_item->set_submenu (file_menu);
  file_item->show ();

  // Add it.
  append (file_item);
}

// @ Tip_Tree

void 
Tip_Tree::daisy_add_tip (Gtk_Widget* item, const string& text)
{
  if (text != Syntax::Unknown ())
    {
      Gtk_Tooltips* tip = new Gtk_Tooltips ();
      tip->set_tip (item, text);
      tips.push_back (tip);
    }
}

Tip_Tree::Tip_Tree ()
{ }

Tip_Tree::~Tip_Tree ()
{ sequence_delete (tips.begin (), tips.end ()); }

// @ Main_Tree

Main_Tree::Main_Tree ()
{
  // Create the Daisy item and subtree.
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

  Daisy::load_syntax (syntax, alist);
  
  Gtk_TreeItem* simulation_item = new Gtk_TreeItem ("Simulation");
  static const string simulation_text 
    = "The simulation attributes.";
  daisy_add_tip (simulation_item, simulation_text);
  simulation_item->show ();
  add (simulation_item);

  AList_Tree* simulation_tree = new AList_Tree (syntax, alist);
  simulation_tree->show ();
  simulation_item->set_subtree (simulation_tree);
  
  // Create the library item and subtree.
  Gtk_TreeItem* libraries_item = new Gtk_TreeItem ("Libraries");
  static const string libraries_text 
    = "The process libraries.";
  daisy_add_tip (libraries_item, libraries_text);
  libraries_item->show ();
  add (libraries_item);
  
  Libraries_Tree* libraries_tree = new Libraries_Tree ();
  libraries_tree->show ();
  libraries_item->set_subtree (libraries_tree);
}

// @ Libraries_Tree

Libraries_Tree::Libraries_Tree ()
{
  // For all libraries...
  vector<string> entries;
  Library::all (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    {
      // Create item.
      const string& entry = *new string (entries[i]);
      Gtk_TreeItem* item = new Gtk_TreeItem (entry);
      item->show ();
      add (item);

      // Add subtree.
      const Library& library = Library::find (entry);
      Library_Tree* tree = new Library_Tree (library);
      tree->show ();
      item->set_subtree (tree);
    }
}

// @ Library_Tree

Library_Tree::Library_Tree (const Library& l)
  : library (l)
{
  // For all members...
  vector<string> entries;
  library.entries (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    {
      // Create item.
      const string& entry = *new string (entries[i]);
      Gtk_TreeItem* item = new Gtk_TreeItem (entry);
      item->show ();
      add (item);

      // Add subtree.
      const Syntax& syntax = library.syntax (entry);
      if (syntax.entries () > 0U)
	{
	  // ... unless empty.
	  const AttributeList& alist = library.lookup (entry);
	  AList_Tree* tree = new AList_Tree (syntax, alist);
	  tree->show ();
	  item->set_subtree (tree);
	}
    }
}

// Name_Value

class Name_Value : public Gtk_HBox 
{
public:
  Name_Value (const string& name, const string& value)
    {
      Gtk_Label* name_label = new Gtk_Label (name);
      name_label->show ();
      Gtk_Label* colon_label = new Gtk_Label (": ");
      colon_label->show ();
      Gtk_Label* value_label = new Gtk_Label (value);
      value_label->show ();
      
      pack_start (name_label, false, false);
      pack_start (colon_label, false, false);
      pack_start (value_label, false, false, true);
      show ();
    }
};


// @ AList_Window

class AList_Window : public Gtk_Window
{
  const string& entry;
  const Syntax& syntax;
  const AttributeList& alist;
public:
  AList_Window (const string& e, const Syntax& s, const AttributeList& a);
};

AList_Window::AList_Window (const string& e,
			    const Syntax& s,
			    const AttributeList& a)
  : entry (e),
    syntax (s),
    alist (a)
{
  Gtk_VBox* content = new Gtk_VBox ();

  // Header.
  strstream header;
  header << entry << ": " << Syntax::type_name (syntax.lookup (entry));
  if (syntax.is_const (entry))
    header << " constant";
  int size = syntax.size (entry);
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    header << " sequence ";
  else
    {
      header << " [" << size << "]";
    }
  header << "\0";
  Gtk_Label* header_label = new Gtk_Label (*new string (header.str ()));
  header_label->show ();
  content->pack_start (header_label);

  // Order.
  int order = syntax.order (entry);
  if (order >= 0)
    {
      ostrstream scratch;
      scratch << "Order: " << order;
      Gtk_Label* order_label = new Gtk_Label (scratch.str ());
      order_label->show ();
      content->pack_start (order_label);
    }

  // Done.
  content->show ();
  add (content);
  
  set_title (string ("GTK Daisy: ") + entry);
}

// @ AList_Item

class AList_Item : public Gtk_TreeItem 
{
  const string& entry;
  const Syntax& syntax;
  const AttributeList& alist;
  Gtk_Window* window;

  static const string& daisy_label (const string&,
				    const Syntax&, const AttributeList&);

  void daisy_select ();
  void daisy_deselect ();
  gint daisy_delete (GdkEventAny*);

public:
  AList_Item (const string&, const Syntax&, const AttributeList&);
  ~AList_Item ();
};

const string& 
AList_Item::daisy_label (const string& entry,
			 const Syntax& syntax, const AttributeList& alist)
{
  // Start.
  strstream label;

  // Order.
  int order = syntax.order (entry);
  if (order >= 0)
    label << "[" << order << "] ";

  // Name.
  label << entry << ": ";
  // Type.
  Syntax::type type = syntax.lookup (entry);
  switch (type)
    {
    case Syntax::Object:
      label << "[" << syntax.library (entry).name () << " object]";
      break;
    case Syntax::Number:
      {
	const string& dimension = syntax.dimension (entry);
	if (dimension == Syntax::Unknown ())
	  label << Syntax::type_name (type);
	else
	  label << dimension;
      }
      break;
    default:
      label << Syntax::type_name (type);
    }

  // Constant?
  if (syntax.is_const (entry))
    label << " constant";

  // Optional?
  if (syntax.is_optional (entry))
    label << " optional";

  // Size.
  int size = syntax.size (entry);
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    label << " sequence ";
  else
    label << " [" << size << "]";

  // Value.
  label << " (";
  if (alist.check (entry))
    if (size == Syntax::Sequence)
      {
	const int elements = alist.size (entry);
	if (type == Syntax::AList && elements == Syntax::Singleton)
	  label << "unset, has old style template";
	else if (elements == 1)
	  label << "one element";
	else
	  label << elements << " elements";
      }
    else switch (type)
      {
      case Syntax::Object:
	{
	  const AttributeList& object = alist.alist (entry);
	  if (object.check ("type"))
	    label << object.name ("type");
	  else
	    label << "<unknown>";
	}
	break;
      case Syntax::Boolean:
	label << (alist.flag (entry) ? "true" : "false");
	break;
      case Syntax::Integer:
	label << alist.integer (entry);
	break;
      case Syntax::Number:
	label << alist.number (entry);
	break;
      default:
	label << "set";
      }
  else
    label << "unset";
  label << ")";

  // End.
  label << "\0";
  return *new string (label.str ());
}

void 
AList_Item::daisy_select ()
{ 
  if (window)
    window->show ();
  else
    {
      window = new AList_Window (entry, syntax, alist);
      connect_to_method (window->delete_event, this, &daisy_delete);
      window->show ();
    }
}

void
AList_Item::daisy_deselect ()
{ 
  assert (window);
  window->hide ();
}

gint
AList_Item::daisy_delete (GdkEventAny*)
{
  deselect ();
#ifdef DELETE_EVENT_WORKS
  return false;
#else
  window = NULL;
  return true;
#endif
}

AList_Item::AList_Item (const string& e,
			const Syntax& s,
			const AttributeList& a)
  : Gtk_TreeItem (daisy_label (e, s, a)),
    entry (e),
    syntax (s),
    alist (a),
    window (NULL)
{ 
  connect_to_method (select, this, &daisy_select);
  connect_to_method (deselect, this, &daisy_deselect);
}

AList_Item::~AList_Item ()
{ delete window; }

// @ AList_Tree

void 
AList_Tree::daisy_add_entry (const string& entry)
{
  const Syntax::type type = syntax.lookup (entry);

  if (type == Syntax::Library)
    // We ignore libraries.
    return;

  // Create (and tip) the item.
  Gtk_TreeItem* item = new AList_Item (*new string (entry), syntax, alist);
  daisy_add_tip (item, syntax.description (entry));
  item->show ();
  add (item);

  // Add subtree.
  switch (type)
    {
    case Syntax::AList:
      if (syntax.size (entry) == Syntax::Singleton
	  || (alist.check (entry) && alist.size (entry) == Syntax::Singleton))
	{
	  const Syntax& child_syntax = syntax.syntax (entry);
	  if (child_syntax.entries () > 0U)
	    {
	      const AttributeList& child_alist
		= (alist.check (entry)
		   ? alist.alist (entry)
		   : syntax.default_alist (entry));
	      AList_Tree* tree = new AList_Tree (child_syntax, child_alist);
	      tree->show ();
	      item->set_subtree (tree);
	    }
	}
      break;
    case Syntax::Object:
      if (alist.check (entry) && alist.size (entry) == Syntax::Singleton)
	{
	  const AttributeList& child_alist = alist.alist (entry);
	  if (child_alist.check ("type"))
	    {
	      const string& child_type = child_alist.name ("type");
	      const Library& child_library = syntax.library (entry);
	      const Syntax& child_syntax = child_library.syntax (child_type);
	      if (child_syntax.entries () > 0U)
		{
		  AList_Tree* tree 
		    = new AList_Tree (child_syntax, child_alist);
		  tree->show ();
		  item->set_subtree (tree);
		}
	    }
	}
    default:
      /* do nothing */
      break;
    }
}

AList_Tree::AList_Tree (const Syntax& s, const AttributeList& a)
  : syntax (s),
    alist (a)
{
  // Ordered members first.
  const vector<string>& order = syntax.order ();
  for (unsigned int i = 0; i < order.size (); i++)
    daisy_add_entry (order[i]);
  
  // Then the remaining members.
  vector<string> entries;
  syntax.entries (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    if (syntax.order (entries[i]) < 0)
      daisy_add_entry (entries[i]);
}

// @ main

int main (int argc, char **argv)
{
  Gtk_Main main (&argc, &argv);

  Main_Window window;
  window.show ();

  main.run ();
  return 0;
}

