// hydraulic_old.C

#include "hydraulic.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "csmp.h"
#include <fstream.h>

class HydraulicOld : public Hydraulic
{
  // We cheat and use h_minus instead of h in all the CSMP except M_.
  CSMP Theta_;
  CSMP hm_;
  CSMP Cw2_;
  CSMP K_;
  CSMP M_;

public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;

  // Create and Destroy.
private:
  friend class HydraulicOldSyntax;
  static Hydraulic& make (AttributeList& al);
  HydraulicOld (const AttributeList&);
public:
  virtual ~HydraulicOld ();
};

double 
HydraulicOld::Theta (const double h) const
{
  return Theta_ (-h);
}

double 
HydraulicOld::K (const double h) const
{
  return K_ (-h);
}

double 
HydraulicOld::Cw2 (const double h) const
{
  return Cw2_ (-h);
}

double 
HydraulicOld::h (const double Theta) const
{
  return -hm_ (Theta);
}

double 
HydraulicOld::M (double h) const
{
  return M_ (h);
}

HydraulicOld::HydraulicOld (const AttributeList& al)
  : Hydraulic (al)
{ 
  const int M_intervals (al.integer ("M_intervals"));
  const string name (al.name ("file"));
  
  ifstream file (name.c_str ());
  if (!file.good ())
    {
      cerr << name << ": file open error";
      THROW ("read error");
    }
  while (file.good () && file.get () != '\n')
    ;

  int line = 0;
  double pF;
  double Theta;
  double Cw2;
  double K;
  while (file.good ())
    {
      file >> pF >> Theta >> Cw2 >> K;
      line++;

      if (Theta_sat < 0.0)
	const_cast<double&> (Theta_sat) = Theta;
      
      const double h_minus = exp (pF);

      Theta_.add (h_minus, Theta);
      Cw2_.add (h_minus, Cw2);
      K_.add (h_minus, K);
    }
  
  if (!file.eof ())
    {
      cerr << name << ":" << line << ": file error";
      THROW ("read error");
    }
  hm_ = Theta_.inverse ();
  K_to_M (M_, M_intervals);
}

HydraulicOld::~HydraulicOld ()
{ }

// Add the HydraulicOld syntax to the syntax table.

Hydraulic&
HydraulicOld::make (AttributeList& al)
{
  return *new HydraulicOld (al);
}

static struct HydraulicOldSyntax
{
  HydraulicOldSyntax ();
} hydraulicOld_syntax;

HydraulicOldSyntax::HydraulicOldSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("M_intervals", Syntax::Integer, Syntax::Const);
  alist.add ("M_intervals", 500);
  alist.add ("Theta_sat", -42.42e42);
  syntax.add ("file", Syntax::String, Syntax::Const);
  syntax.order ("file");
  Hydraulic::add_type ("old", alist, syntax, &HydraulicOld::make);
}
