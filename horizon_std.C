// horizon_standard.C

#include "horizon.h"
#include "syntax.h"
#include "alist.h"

class HorizonStandard : public Horizon
{
  // Create and Destroy.
private:
  friend class HorizonStandardSyntax;
  static Horizon& make (const AttributeList& al)
  { return *new HorizonStandard (al); }
  HorizonStandard (const AttributeList& al)
    : Horizon (al)
  { }
public:
  ~HorizonStandard ()
  { }
};

static struct HorizonStandardSyntax
{
  HorizonStandardSyntax ();
} HorizonStandard_syntax;

HorizonStandardSyntax::HorizonStandardSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Horizon::load_syntax (syntax, alist);
  Horizon::add_type ("default", alist, syntax, &HorizonStandard::make);
}
