// hydraulic_old2.C

#include "hydraulic.h"
#include "mathlib.h"
#include "csmp.h"
#include <fstream.h>

class HydraulicOld2 : public Hydraulic
{
  // We cheat and use h_minus instead of h in all the CSMP except M_.
  double Theta_[501];
  CSMP hm_;
  double Cw2_[501];
  double K_[501];
  double M_[501];

  inline double lookup (const double *const a, const double h) const
  {
    if (h >= -1e-10)
      return a[0];
    const double pos = h2pF (h) * 100;
    int min = (int) floor (pos);
    int max = (int) ceil (pos);

    if (min < 0)
      return a[0];
    if (max > 500)
      return a[500];
    return a[min] + (pos - min) * (a[max] - a[min]);
  }
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;

  // Create and Destroy.
private:
  friend class HydraulicOld2Syntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicOld2 (const AttributeList&);
public:
  virtual ~HydraulicOld2 ();
};

double 
HydraulicOld2::Theta (const double h) const
{
  return lookup (Theta_, h);
}

double 
HydraulicOld2::K (const double h) const
{
  return lookup (K_, h);
}

double 
HydraulicOld2::Cw2 (const double h) const
{
  return lookup (Cw2_, h);
}

double 
HydraulicOld2::h (const double Theta) const
{
  assert (Theta <= Theta_sat);
  return -hm_ (-Theta);
}

double 
HydraulicOld2::M (double h) const
{
  return lookup (M_, h);
}

HydraulicOld2::HydraulicOld2 (const AttributeList& al)
  : Hydraulic (al)
{ 
  const int M_intervals (al.integer ("M_intervals"));
  const string name (al.name ("file"));
  
  // CERR << "\n" << name << ": opening\n";

  ifstream file (Options::find_file (name));
  if (!file.good ())
    {
      CERR << "\n" << name << ": file open error";
      THROW ("read error");
    }
  while (file.good () && file.get () != '\n')
    ;

  int line = 0;
  double pF;
  double Theta;
  double Cw2;
  double K;

  CSMP Thetam_;

  for (int i = 0; i <= 500; i++)
    {
      if (!file.good ())
	CERR << "\n" << name << ":" << line << ": no good";

      file >> pF >> Theta >> Cw2 >> K;
      line++;

      if (Theta_sat < 0.0)
	const_cast<double&> (Theta_sat) = Theta;
      
      if (i != int (rint (pF * 100)))
	CERR << "\n" << name << ":" << line << ": i " << i << " != "
	     << pF * 100 << "(" << int (rint (pF * 100)) << ")";
      
      Theta_[i] = Theta;
      Cw2_[i] = Cw2 * 1.0e-2;
      K_[i] = K * 3.6e5;

      const double h_minus = (pF < 1.0e-10) ? 0.0 : - pF2h (pF);
      
      Thetam_.add (h_minus, -Theta);
    }
  
#if 0
  if (!file.eof ())
    {
      CERR << name << ":" << line << ": end of file expected\nGot:";

      while (file.good () && !file.eof ())
	CERR << " `" << file.get () << "'";
      CERR << "\n";
      // THROW ("read error");
    }
#endif
  hm_ = Thetam_.inverse ();

  CSMP myM;
  K_to_M (myM, M_intervals);

  for (int i = 0; i <= 500; i++)
    M_[i] = myM (pF2h (double (i) / 100.0));

  close (file.rdbuf ()->fd ());
}

HydraulicOld2::~HydraulicOld2 ()
{ }

// Add the HydraulicOld2 syntax to the syntax table.

Hydraulic&
HydraulicOld2::make (const AttributeList& al)
{
  return *new HydraulicOld2 (al);
}

static struct HydraulicOld2Syntax
{
  HydraulicOld2Syntax ();
} hydraulicOld2_syntax;

HydraulicOld2Syntax::HydraulicOld2Syntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("M_intervals", Syntax::Integer, Syntax::Const);
  alist.add ("M_intervals", 500);
  alist.add ("Theta_sat", -42.42e42);
  syntax.add ("file", Syntax::String, Syntax::Const);
  syntax.order ("file");
  Librarian<Hydraulic>::add_type ("old2", alist, syntax, &HydraulicOld2::make);
}
