// submodel.h  --- A registry of submodels used in Daisy.

#ifndef SUBMODEL_H
#define SUBMODEL_H

#include <string>
#include <vector>

using namespace std;

struct Syntax;
struct AttributeList;

class Submodel
{
public:
  typedef void (*load_fun) (Syntax&, AttributeList&);
  
  static void all (vector<string>& entries);
  static void load_syntax (const string& model, Syntax&, AttributeList&);

  class Register
  {
  public:
      Register (const string& name, load_fun fun);
      ~Register ();
  };

  Submodel ();
  ~Submodel ();
};

#endif // SUBMODEL_H
