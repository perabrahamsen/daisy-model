// traverse.h --- Base class to traverse the Daisy data structures.

#ifndef TRAVERSE_H
#define TRAVERSE_H

#include <string>
using namespace std;

struct Library;
struct Syntax;
struct AttributeList;

class Traverse
{
  // Major calls.
public:
  void traverse_all_libraries ();
  void traverse_library (const string& component);
  void traverse_model (const string& component, const string& model);
  void traverse_submodel (const Syntax& syntax, AttributeList& alist,
			  const AttributeList& default_alist,
			  const string& name);
  
  // Internal calls
private:
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

#endif TRAVERSE_H
