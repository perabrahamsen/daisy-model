// qmain_populate.C -- Build the parameter tree.
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

#include "qmain_populate.h"
#include "qmain_tree.h"
#include "qmain_item.h"
#include "qmain.h"

#include "traverse.h"
#include "tmpstream.h"
#include "treelog_stream.h"
#include "plf.h"
#include "time.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "parser.h"
#include <deque>

class TraverseQtTree : public Traverse
{
  // Content.
private:
  MainWindow* main;
  const bool check_alists;
  deque<TreeItem*> path;
  deque<bool> buildins;
  bool editable;
  const bool view_defaults;

  // Accessors.
public:
  void enter (TreeItem* item);
  void leave ();
private:
  bool buildin () const;
  TreeItem* item () const;

private:
  // Implementation.
  bool enter_library (Library& library, 
		      const string& component);
  void leave_library ();
  bool enter_model (const Syntax&, AttributeList&, 
		    const string& component, const string& model);
  void leave_model (const string& component, const string& name);
  bool enter_submodel (const Syntax& syntax, AttributeList& alist,
  		       const AttributeList& default_alist,
  		       const string& name);
  void leave_submodel ();
  bool enter_submodel_default (const Syntax& syntax, 
			       const AttributeList& default_alist,
			       const string& name);
  void leave_submodel_default ();
  bool enter_submodel_sequence (const Syntax& syntax,
  				AttributeList& alist,
  				const AttributeList& default_alist,
  				const string& name, unsigned index);
  void leave_submodel_sequence ();
  bool enter_submodel_sequence_default (const Syntax& syntax, 
  					const AttributeList& default_alist,
  					const string& name);
  void leave_submodel_sequence_default ();
  bool enter_object (const Library&, 
		     const Syntax& syntax, AttributeList& alist,
  		     const AttributeList& default_alist,
  		     const string& name);
  void leave_object ();
  bool enter_object_sequence (const Library&, const Syntax& syntax,
  			      AttributeList& alist,
  			      const AttributeList& default_alist,
  			      const string& name, 
  			      unsigned index);
  void leave_object_sequence ();
  bool enter_parameter (const Syntax&, AttributeList& alist, 
			const AttributeList& default_alist, 
			const string& name, const string& parameter);
  void leave_parameter ();

  // Create and destroy.
public:
  TraverseQtTree (MainWindow*, bool check_alists);
  TraverseQtTree (TreeItem* item, bool view_defaults);
  ~TraverseQtTree ();
};

void 
TraverseQtTree::enter (TreeItem* item)
{ path.push_back (item); }

void 
TraverseQtTree::leave ()
{ path.pop_back (); }

TreeItem*
TraverseQtTree::item () const
{ 
  assert (path.size () > 0);
  return path.back ();
}

bool 
TraverseQtTree::buildin () const
{ return (buildins.size () == 0) ? false : buildins.back (); }

bool
TraverseQtTree::enter_library (Library& library, const string& component)
{
  // Check whether to view this library.
  if (!Filter::filters[main->view_filter]->check (component))
    return false;

  // Find models.
  vector<string> models;
  library.entries (models);

  // Check whether this library is empty (filtered).
  if (!main->view_empty)
    {
      for (unsigned int i = 0; i < models.size (); i++)
	if (Filter::filters[main->view_filter]->check (component,
						       models[i]))
	  goto found;
      // None found, don't include this.
      return false;
    found:;
    }

  // Add it.
  TmpStream value;
  value () << models.size () << " entries";
  enter (new LibraryItem (main->tree, component.c_str (), value.str ()));
  // Recurse.
  return true;
}

void
TraverseQtTree::leave_library ()
{ leave (); }

bool
TraverseQtTree::enter_model (const Syntax& syntax, AttributeList& alist,
			     const string& component, const string& model)
{
  // Check whether to view this model.
  if (!Filter::filters[main->view_filter]->check (component, model))
    return false;

  // We use the base model as value.
  QString value;
  if (alist.check ("type"))
    value = QString ("'") + alist.name ("type").c_str () + "'";
  else 
    value = "buildin";

  // Check if it is fully defined.
  if (check_alists)
    {
      TmpStream errors;
      TreelogStream err (errors ());
      const bool has_errors = !syntax.check (alist, err);
      value +=  has_errors ? " partial" : " full";
    }
  
  // Find the model description.
  QString description = "no description";
  if (alist.check ("description"))
    description = alist.name ("description").c_str ();

  // Find file.
  QString from;
  string file;
  const bool from_file = alist.check ("parsed_from_file");
  if (from_file)
    {
      file = alist.name ("parsed_from_file");
      if (file.length () > 25)
	{
	  int target = 20;
	  for (unsigned int i = 20; i < file.length () - 10; i++)
	    if (file[i] == '\\' || file[i] == '/')
	      {
		target = i;
		break;
	      }
	  from = "...";
	  for (unsigned int i = target; i < file.length (); i++)
	    from += file[i];
	}
      else
	from = file.c_str ();
    }

  // Find sequence
  const int sequence = (alist.check ("parsed_sequence") 
			? alist.integer ("parsed_sequence")
			: 0);
  // Add it
  editable = (from_file && main->file_name == file.c_str ());
  if (alist.check ("type"))
    {
      const Library& library = library.find (component);
      const string& type = alist.name ("type");
      const AttributeList& default_alist = library.lookup (type);
      enter (new ModelItem (syntax, alist, default_alist,
			    item (), model.c_str (), value, from,
			    sequence, editable));
    }
  else
    {
      assert (!editable);
      static const AttributeList empty_alist;
      enter (new ModelItem (syntax, alist, empty_alist,
			    item (), model.c_str (), value, from,
			    sequence, editable));
    }
    
  // Remember whether this is buildin or not.
  buildins.push_back (!alist.check ("type"));

  // Recurse.
  return true;
}

void
TraverseQtTree::leave_model (const string&, const string&)
{ 
  editable = false;
  leave (); 
  buildins.pop_back ();
}

bool
TraverseQtTree::enter_submodel (const Syntax&, AttributeList&,
				const AttributeList&,
				const string&)
{ return true; }

void
TraverseQtTree::leave_submodel ()
{ }

bool
TraverseQtTree::enter_submodel_default (const Syntax&, const AttributeList&, 
					const string&)
{
  return buildin ();
}

void
TraverseQtTree::leave_submodel_default ()
{ }

bool
TraverseQtTree::enter_submodel_sequence (const Syntax& syntax,
					 AttributeList& alist,
					 const AttributeList& default_alist,
					 const string&, unsigned index)
{ 
  QString name;
  name.sprintf ("[%d]", index);
  QString value;
  if (check_alists)
    {
      TmpStream errors;
      TreelogStream err (errors ());
      const bool has_errors = !syntax.check (alist, err);
      value = has_errors ? "Partial" : "Full";
    }
  enter (new SequenceItem (syntax, alist, default_alist,
			   item (), name, "", value, "", index+1));
  buildins.push_back (false);
  return true; 
}

void
TraverseQtTree::leave_submodel_sequence ()
{ 
  leave (); 
  buildins.pop_back ();
}

bool
TraverseQtTree::enter_submodel_sequence_default (const Syntax& syntax, 
						 const AttributeList& 
						 /**/ default_alist,
						 const string&)
{
  if (!buildin ())
    return false;

  QString value;
  if (check_alists)
    {
      TmpStream errors;
      TreelogStream err (errors ());
      const bool has_errors = !syntax.check (default_alist, err);
      value = has_errors ? "Partial" : "Full";
    }
  static const AttributeList empty;
  enter (new DefaultItem (syntax, const_cast <AttributeList&> (default_alist),
			  empty, 
			  item (), "default", "", value, "", 0));
  return true; 
}

void
TraverseQtTree::leave_submodel_sequence_default ()
{ leave (); }

bool
TraverseQtTree::enter_object (const Library&, const Syntax&, AttributeList&,
			      const AttributeList&,
			      const string&)
{
  buildins.push_back (false);
  return true; 
}

void
TraverseQtTree::leave_object ()
{ buildins.pop_back (); }

bool
TraverseQtTree::enter_object_sequence (const Library&, const Syntax& syntax,
				       AttributeList& alist,
				       const AttributeList& default_alist,
				       const string&, 
				       unsigned index)
{ 
  QString name;
  name.sprintf ("[%d]", index);
  assert (alist.check ("type"));
  QString value = QString ("'") + alist.name ("type").c_str () + "'";
  if (check_alists)
    {
      TmpStream errors;
      TreelogStream err (errors ());
      const bool has_errors = !syntax.check (alist, err);
      value += (has_errors ? " partial" : " full");
    }
  enter (new SequenceItem (syntax, alist, default_alist, 
			   item (), name, "", value, "", index+1));
  buildins.push_back (false);
  return true; 
}

void
TraverseQtTree::leave_object_sequence ()
{ 
  leave (); 
  buildins.pop_back ();
}

bool
TraverseQtTree::enter_parameter (const Syntax& syntax, AttributeList& alist, 
				 const AttributeList& default_alist, 
				 const string&, const string& parameter)
{
  // Don't print default values.
  if (!view_defaults
      && !buildin () 
      && alist.subset (default_alist, syntax, parameter))
    return false;

  // Get the data.
  QString parameter_name =  parameter.c_str ();
  const Syntax::type type = syntax.lookup (parameter);
  QString type_name = Syntax::type_name (type);
  const int size = syntax.size (parameter);
  const bool has_value = alist.check (parameter);
  QString value_name = has_value ? "<has value>" : "";

  // Set category name and order.
  QString category_name;
  int order = syntax.order (parameter);
  if (syntax.is_log (parameter))
    {
      if (!main->view_logonly)
	return false;
      category_name = "Log";
      if (order < 0)
	order = 9999;
    }
  else if (syntax.is_const (parameter))
    {
      if (!main->view_parameters)
	return false;
      category_name = "Param.";
      if (order < 0)
	order = 7777;
    }
  else if (syntax.is_state (parameter))
    {
      category_name = "State";
      if (order < 0)
	order = 8888;
    }
  else
    assert (false);

  if (syntax.is_optional (parameter))
    category_name += " (opt)";

  // Value specific changes.
  if (has_value && size != Syntax::Singleton)
    {
      value_name = QString::number (alist.size (parameter));
      value_name += " elems";
    }

  // Type specific changes.
  QString errors;
  switch (type)
    {
    case Syntax::AList:
      {
	const AttributeList& child 
	  = (has_value && size == Syntax::Singleton)
	  ? alist.alist (parameter)
	  : syntax.default_alist (parameter);
	if (child.check ("submodel"))
	  {
	    type_name = "'";
	    type_name += child.name ("submodel").c_str ();
	    type_name += "' submodel";
	  }
	else
	  type_name = "Submodel";

	if (has_value && size == Syntax::Singleton && check_alists)
	  {
	    TmpStream str;
	    TreelogStream err (str ());
	    const bool has_errors = 
	      !syntax.syntax (parameter).check (alist.alist (parameter), err);
	    errors = str.str ();
	    if (has_errors)
	      value_name = "Partial";
	    else
	      value_name = "Full";
	  }
      }
      break;
    case Syntax::Object:
      type_name = "'";
      type_name += syntax.library (parameter).name ().c_str ();
      type_name += "' component";
      if (has_value && size == Syntax::Singleton 
	  && alist.alist (parameter).check ("type"))
	{
	  string type = alist.alist (parameter).name ("type");
	  value_name = "'";
	  value_name += type.c_str ();
	  value_name += "'";
	  if (check_alists)
	    {
	      TmpStream str;
	      TreelogStream err (str ());
	      const bool has_errors 
		= (!syntax.library (parameter)
		   .syntax (type).check (alist.alist (parameter), err));
	      errors = str.str ();
	      if (has_errors)
		value_name += " partial";
	      else
		value_name += " full";
	    }
	}
      break;
    case Syntax::Number:
      type_name = syntax.dimension (parameter).c_str ();
      if (size == Syntax::Singleton && has_value)
	value_name = QString::number (alist.number (parameter));
      break;
    case Syntax::Integer:
      if (size == Syntax::Singleton && has_value)
	value_name = QString::number (alist.integer (parameter));
      break;
    case Syntax::Boolean:
      if (size == Syntax::Singleton && has_value)
	value_name = alist.flag (parameter) ? "true" : "false";
      break;
    case Syntax::PLF:
      type_name = "PLF: ";
      type_name += syntax.range (parameter).c_str ();
      type_name += " -> ";
      type_name += syntax.domain (parameter).c_str ();
      if (has_value && size == Syntax::Singleton)
	{
	  value_name = "<";
	  value_name += QString::number (alist.plf (parameter).size ());
	  value_name += " points>";
	}
      break;
    case Syntax::String:
      if (has_value && size == Syntax::Singleton)
	{
	  value_name = "<";
	  value_name += QString::number (alist.name (parameter).length ());
	  value_name += " chars>";
	}
      break;
    case Syntax::Date:
      if (has_value && size == Syntax::Singleton)
	{
	  Time time (alist.time (parameter));
	  value_name.sprintf ("%04d-%02d-%02dT%02d",
			      time.year (), time.month (), time.mday (), 
			      time.hour ());
	}
      break;
    case Syntax::Library:
      return false;
    case Syntax::Error:
    default:
      assert (false);
    }

  // Size specific changes.
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    type_name += " seq";
  else
    {
      type_name += " array [";
      type_name += QString::number (size);
      type_name += "]";
    }

  // Be sure we have a non-shared alist for editing.
  if (editable
      && has_value 
      && (type == Syntax::AList || type == Syntax::Object))
    {
      if (size == Syntax::Singleton)
	alist.add (parameter, alist.alist (parameter));
      else
	alist.add (parameter, alist.alist_sequence (parameter));
    }	

  // Create it.
  if (size == Syntax::Singleton && type == Syntax::AList)
    {
      const Syntax& child_syntax = syntax.syntax (parameter);
      const AttributeList& child_default 
	= default_alist.check (parameter)
	? default_alist.alist (parameter)
	: syntax.default_alist (parameter);
      static AttributeList empty_alist;
      AttributeList& child_alist 
	= alist.check (parameter) 
	? alist.alist (parameter)
	: empty_alist;

      enter (new SubmodelItem (child_syntax, child_alist, child_default,
			       item (), 
			       parameter_name, type_name, value_name, 
			       category_name, order));
    }
  else if (size == Syntax::Singleton && type == Syntax::Object && has_value)
    {
      assert (type == Syntax::Object);
      const Library& library = syntax.library (parameter);
      AttributeList& child_alist = alist.alist (parameter);
      assert (child_alist.check ("type"));
      const string& type = child_alist.name ("type");
      const Syntax& child_syntax = library.syntax (type);
      const AttributeList& child_default = library.lookup (type);
      enter (new ObjectItem (child_syntax, child_alist, child_default,
			     item (),
			     parameter_name, type_name, value_name, 
			     category_name, order));
    }
  else
    enter (new AtomItem (item (), parameter_name, type_name, value_name, 
			 category_name, order));

  // Recurse.
  return true;
}

void 
TraverseQtTree::leave_parameter ()
{ leave (); }

TraverseQtTree::TraverseQtTree (MainWindow* m, bool ca)
  : main (m),
    check_alists (ca),
    editable (false),
    view_defaults (false)
{ }

TraverseQtTree::TraverseQtTree (TreeItem* item, bool view_defaults)
  : main (item->main ()),
    check_alists (false),
    editable (item->editable ()),
    view_defaults (view_defaults)
{ 
  path.push_back (item);
}

TraverseQtTree::~TraverseQtTree ()
{ }

void 
populate_tree (MainWindow* main, bool check_alists,
	       const Syntax& syntax, AttributeList& alist, 
	       const AttributeList& default_alist)
{
  main->tree->clear();
  
  TraverseQtTree build (main, check_alists);
  build.traverse_all_libraries ();

  // Add simulation.
  build.enter (new SimulationItem (syntax, alist, default_alist,
			     main->tree, "Daisy"));
  build.traverse_alist (syntax, alist, default_alist, "Daisy");
  build.leave ();

  // Add inputs
  if (!alist.check ("parser_inputs"))
    alist.add ("parser_inputs", vector<AttributeList*> ());
  const vector<AttributeList*>& inputs 
    = alist.alist_sequence ("parser_inputs");
  InputsItem* item = new InputsItem (inputs, main->tree, "Inputs");
  build.enter (item);
  const Library& library = Librarian<Parser>::library ();
  for (unsigned int i = 0; i < inputs.size (); i++)
    {
      QString name;
      name.sprintf ("[%d]", i);
      AttributeList& alist = *inputs[i];
      assert (alist.check ("type"));
      string type = alist.name ("type");
      assert (library.check (type));
      const Syntax& syntax = library.syntax (type);
      const AttributeList& default_alist = library.lookup (type);
      string where;
      if (alist.check ("where"))
	where = alist.name ("where");
      build.enter (new InputItem (syntax, alist, default_alist, 
				  item,
				  name, type.c_str (), where.c_str (), "",
				  i));
      build.traverse_alist (syntax, alist, default_alist, name.latin1 ());
      build.leave ();
    }
  build.leave ();
}

void 
populate_alist (AListItem* item)
{
  // Clear old content.
  while (item->firstChild ())
    delete item->firstChild ();
  
  // Build new content.
  TraverseQtTree build (item, item->view_defaults);
  build.traverse_alist (item->syntax, item->alist, item->default_alist, 
			item->entry.latin1 ());
}
void
populate_parameter (AListItem* parent, TreeItem* child)
{
  assert (parent);
  assert (child);
  string name = parent->entry.latin1 ();
  string parameter = child->entry.latin1 ();
  bool view_defaults = false;
  if (parent->view_defaults)
    view_defaults = true;
  else if (SubmodelItem* submodel = dynamic_cast<SubmodelItem*> (child))
    view_defaults = submodel->view_defaults;
  else
    assert (dynamic_cast<AtomItem*> (child));

  TraverseQtTree build (parent, view_defaults);
  build.traverse_parameter (parent->syntax, parent->alist, 
			    parent->default_alist, name, parameter);
}
