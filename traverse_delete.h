// traverse_delete.h -- Remove dependencies in datastructures.

#ifndef TRAVERSE_DELETE_H
#define TRAVERSE_DELETE_H

#include <string>
using namespace std;

class Syntax;
class AttributeList;

void
remove_dependencies (const string& component, const string& parameterization);

void
remove_dependencies (const string& component, const string& parameterization, 
		     const Syntax& syntax, AttributeList& alist);

#endif // TRAVERSE_DELETE_H
