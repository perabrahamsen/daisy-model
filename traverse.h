// traverse.h --- Base class to traverse the data structures.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef TRAVERSE_H
#define TRAVERSE_H

#include <string>
using namespace std;

class Library;
class Syntax;
class AttributeList;

class Traverse
{
  // Major calls.
public:
  void traverse_all_libraries ();
  void traverse_all_submodels ();
  void traverse_library (const string& component);
  void traverse_model (const string& component, const string& model);
  void traverse_submodel (const Syntax& syntax, AttributeList& alist,
			  const AttributeList& default_alist,
			  const string& name);
  
  // Minor calls.
  void traverse_submodel_default (const Syntax& syntax, 
				  const AttributeList& default_alist,
				  const string& name);
  void traverse_submodel_sequence (const Syntax& syntax,
				   AttributeList& alist,
				   const AttributeList& default_alist,
				   const string& name, unsigned index);
  void traverse_submodel_sequence_default (const Syntax& syntax, 
					   const AttributeList& default_alist,
					   const string& name);
  void traverse_object (const Library& library,
			const Syntax& syntax, AttributeList& alist,
			const AttributeList& default_alist,
			const string& name);
  void traverse_object_sequence (const Library& library,
				 const Syntax& syntax, AttributeList& alist,
				 const AttributeList& default_alist,
				 const string& name, unsigned index);
  void traverse_alist (const Syntax&, AttributeList& alist, 
		       const AttributeList& default_alist,
		       const string& name);
  void traverse_parameter (const Syntax&, AttributeList& alist, 
			   const AttributeList& default_alist, 
			   const string& name, const string& parameter);

  // Subclass Responsibility.
protected:
  virtual bool enter_library (Library&, const string& component) = 0;
  virtual void leave_library () = 0;
  virtual bool enter_model (const Syntax&, AttributeList&,
			    const string& component, const string& model) = 0;
  virtual void leave_model (const string& component, const string& name) = 0;
  virtual bool enter_submodel (const Syntax& syntax, AttributeList& alist,
			       const AttributeList& default_alist,
			       const string& name) = 0;
  virtual void leave_submodel () = 0;
  virtual bool enter_submodel_default (const Syntax& syntax, 
				       const AttributeList& default_alist,
				       const string& name) = 0;
  virtual void leave_submodel_default () = 0;
  virtual bool enter_submodel_sequence (const Syntax& syntax,
					AttributeList& alist,
					const AttributeList& default_alist,
					const string& name, 
					unsigned index) = 0;
  virtual void leave_submodel_sequence () = 0;
  virtual bool enter_submodel_sequence_default (const Syntax& syntax, 
						const AttributeList&
						/**/ default_alist,
						const string& name) = 0;
  virtual void leave_submodel_sequence_default () = 0;
  virtual bool enter_object (const Library&, 
			     const Syntax& syntax, AttributeList& alist,
			     const AttributeList& default_alist,
			     const string& name) = 0;
  virtual void leave_object () = 0;
  virtual bool enter_object_sequence (const Library&, const Syntax& syntax, 
				      AttributeList& alist,
				      const AttributeList& default_alist,
				      const string& name, 
				      unsigned index) = 0;
  virtual void leave_object_sequence () = 0;
  virtual bool enter_parameter (const Syntax&, AttributeList& alist, 
				const AttributeList& default_alist, 
				const string& name, 
				const string& parameter) = 0;
  virtual void leave_parameter () = 0;

  // Create and destroy.
protected:
  Traverse ();
  virtual ~Traverse ();
};

#endif // TRAVERSE_H
