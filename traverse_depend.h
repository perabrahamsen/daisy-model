// traverse_depend.h -- Check dependencies in Daisy datastructures.

#ifndef TRAVERSE_DEPEND_H
#define TRAVERSE_DEPEND_H

#include <string>
using namespace std;

struct Treelog;
struct Syntax;
struct AttributeList;

bool
check_dependencies (const string& component, const string& parameterization, 
		    Treelog& treelog);

bool
check_dependencies (const string& component, const string& parameterization, 
		    const Syntax& syntax, AttributeList& alist,
		    const string& name, Treelog& treelog);


#endif TRAVERSE_DEPEND_H
