// qmain_populate.C -- Build the parameter tree.

#include "qmain_populate.h"
#include "qmain_tree.h"
#include "qmain_item.h"
#include "qmain.h"

#include "traverse.h"
#include "tmpstream.h"
#include "plf.h"
#include "time.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"

#include <deque>

class TraverseQtTree : public Traverse
{
  // Content.
private:
  MainWindow* main;
  deque<TreeItem*> path;
  deque<bool> buildins;
  bool editable;

  // Accessors.
private:
  void enter (TreeItem* item);
  void leave ();
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
  TraverseQtTree (MainWindow*);
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
  enter (new TreeItem (library.description (), main->tree,
		       component.c_str (), value.str ()));
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
    value = QString ("`") + alist.name ("type").c_str () + "'";
  else 
    value = "buildin";

  // Check if it is fully defined.
  TmpStream errors;
  const bool has_errors = !syntax.check (alist, errors (), model);
  value +=  has_errors ? " partial" : " full";
  
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
  if (from_file && main->file_name == file.c_str ())
    {
      editable = true;
      if (alist.check ("type"))
	{
	  const Library& library = library.find (component);
	  const string& type = alist.name ("type");
	  const AttributeList& default_alist = library.lookup (type);
	  enter (new AListItem (syntax, alist, default_alist,
				description, errors.str (),
				item (), model.c_str (), "", value, from,
				sequence));
	}
      else
	{
	  static const AttributeList empty_alist;
	  enter (new AListItem (syntax, alist, empty_alist,
				description, errors.str (),
				item (), model.c_str (), "", value, from,
				sequence));
	}
    }
  else
    enter (new TreeItem (description, errors.str (),
			 item (), model.c_str (), "", value, from, sequence));
    
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
  assert (buildin ());
  return true; 
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
  TmpStream errors;
  const bool has_errors = !syntax.check (alist, errors (), name.latin1 ());
  if (editable)
    enter (new AListItem (syntax, alist, default_alist,
			  "Value for item in sequence.", errors.str (),
			  item (), name, "", 
			  has_errors ? "Partial" : "Full",
			  "", index+1));
  else
    enter (new TreeItem ("Value for item in sequence.", errors.str (),
			 item (), name, "", 
			 has_errors ? "Partial" : "Full",
			 "", index+1));
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

  TmpStream errors;
  const bool has_errors = !syntax.check (default_alist, errors (), "default");
  enter (new TreeItem ("Default values for items in sequence.", errors.str (),
		       item (), "default", "", 
		       has_errors ? "Partial" : "Full",
		       "", 0));
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
  TmpStream errors;
  const bool has_errors = !syntax.check (alist, errors (), name.latin1 ());
  assert (alist.check ("type"));
  QString value = QString ("`") + alist.name ("type").c_str () + "'";
  value += (has_errors ? " partial" : " full");
  if (editable)
    enter (new AListItem (syntax, alist, default_alist, 
			  "Value for item in sequence.", errors.str (),
			 item (), name, "", value, "", index+1));
  else
    enter (new TreeItem ("Value for item in sequence.", errors.str (),
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
  if (!buildin () && alist.subset (default_alist, syntax, parameter))
    return false;

  // Get the data.
  QString parameter_name =  parameter.c_str ();
  const Syntax::type type = syntax.lookup (parameter);
  QString type_name = Syntax::type_name (type);
  const int size = syntax.size (parameter);
  const bool has_value = alist.check (parameter);
  QString value_name = has_value ? "<has value>" : "";
  QString description = syntax.description (parameter).c_str ();

  // Set category name and order.
  QString category_name;
  int order = syntax.order (parameter);
  if (syntax.is_log (parameter))
    {
      if (!main->view_logonly)
	return false;
      category_name = "Log variable";
      if (order < 0)
	order = 9999;
    }
  else if (syntax.is_const (parameter))
    {
      if (!main->view_parameters)
	return false;
      category_name = "Parameter";
      if (order < 0)
	order = 7777;
    }
  else if (syntax.is_state (parameter))
    {
      category_name = "State variable";
      if (order < 0)
	order = 8888;
    }
  else
    assert (false);

  if (syntax.is_optional (parameter))
    category_name += " (optional)";

  // Value specific changes.
  if (has_value && size != Syntax::Singleton)
    {
      value_name = QString::number (alist.size (parameter));
      value_name += " elements";
    }

  // Type specific changes.
  bool has_errors = false;
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
	    type_name = "`";
	    type_name += child.name ("submodel").c_str ();
	    type_name += "' submodel";
	  }
	else
	  type_name = "Submodel";

	if (child.check ("description"))
	  {
	    QString child_description = child.name ("description").c_str ();
	    if (description != child_description)
	      {
		description += "\n--\n";
		description += child_description;
	      }
	  }
	if (has_value && size == Syntax::Singleton)
	  {
	    TmpStream str;
	    has_errors = 
	      !syntax.syntax (parameter).check (alist.alist (parameter), 
						str (), parameter);
	    errors = str.str ();
	    if (has_errors)
	      value_name = "Partial";
	    else
	      value_name = "Full";
	  }
      }
      break;
    case Syntax::Object:
      type_name = "`";
      type_name += syntax.library (parameter).name ().c_str ();
      type_name += "' component";
      if (has_value && size == Syntax::Singleton 
	  && alist.alist (parameter).check ("type"))
	{
	  string type = alist.alist (parameter).name ("type");
	  value_name = "`";
	  value_name += type.c_str ();
	  value_name += "'";
	  TmpStream str;
	  has_errors 
	    = (!syntax.library (parameter)
	       .syntax (type).check (alist.alist (parameter),
				     str (), parameter));
	  errors = str.str ();
	  if (has_errors)
	    value_name += " partial";
	  else
	    value_name += " full";
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
	  value_name += " characters>";
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

  // Create it.
  if (editable 
      && size == Syntax::Singleton
      && has_value
      && (type == Syntax::AList || type == Syntax::Object))
    {
      if (type == Syntax::AList)
	{
	  const Syntax& child_syntax = syntax.syntax (parameter);
	  AttributeList& child_alist = alist.alist (parameter);
	  const AttributeList& child_default 
	    = syntax.default_alist (parameter);
	  enter (new AListItem (child_syntax, child_alist, child_default,
				description, errors, item (),
				parameter_name, type_name, value_name, 
				category_name, order));
	}
      else
	{
	  assert (type == Syntax::Object);
	  const Library& library = syntax.library (parameter);
	  AttributeList& child_alist = alist.alist (parameter);
	  assert (child_alist.check ("type"));
	  const string& type = child_alist.name ("type");
	  const Syntax& child_syntax = library.syntax (type);
	  const AttributeList& child_default = library.lookup (type);
	  enter (new AListItem (child_syntax, child_alist, child_default,
				description, errors, item (),
				parameter_name, type_name, value_name, 
				category_name, order));
	}
    }
  else
    enter (new TreeItem (description, errors,
			 item (), parameter_name, type_name, value_name, 
			 category_name, order));

  // Recurse.
  return true;
}

void 
TraverseQtTree::leave_parameter ()
{ leave (); }

TraverseQtTree::TraverseQtTree (MainWindow* m)
  : main (m),
    editable (false)
{ }

TraverseQtTree::~TraverseQtTree ()
{ 
  assert (path.size () == 0); 
  assert (buildins.size () == 0);
  assert (!editable);
}

void 
populate_tree (MainWindow* main)
{
  main->tree->clear();
  
  TraverseQtTree build (main);
  build.traverse_all_libraries ();
}
