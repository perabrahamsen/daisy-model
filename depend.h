// depend.h -- Check dependencies in datastructures.

#ifndef DEPEND_H
#define DEPEND_H

#include <string>
#include <set>
#include <map>
using namespace std;

class Treelog;
class Syntax;
class AttributeList;

typedef set<string, less<string>/**/> string_set;
typedef map<string, string_set, less<string>/**/> dep_map;

bool
has_dependencies (const string& component, const string& parameterization);

bool
has_dependencies (const string& component, const string& parameterization, 
		    const Syntax& syntax, AttributeList& alist,
		    const string& name);
bool
check_dependencies (const string& component, const string& parameterization, 
		    Treelog& treelog);

bool
check_dependencies (const string& component, const string& parameterization, 
		    const Syntax& syntax, AttributeList& alist,
		    const string& name, Treelog& treelog);
bool
find_dependencies (const string& component, const string& parameterization, 
		   dep_map& dependencies);

void
resequence (const string& component, const string& parameterization, 
	    const dep_map& dependencies);


#endif // DEPEND_H
