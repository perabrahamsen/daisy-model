// groundwater_pipe.C

#include "groundwater.h"
#include "log.h"
#include "soil.h"
#include "mathlib.h"

class GroundwaterPipe : public Groundwater
{
  // Parameters
  const double L;		// Distance between pipes. [cm]
  const double x;		// Distance to nearest pipe. [cm]
  const double pipe_position;	// Height pipes are placed above surface. [cm]
  const double K_aquitard;	// Conductivity of the aquitard [cm h^-1]
  const double Z_aquitard;	// Vertical length of the aquitard [cm]
  const double h_aquifer;	// Pressure potential in the aquifer [cm]
  int i_bottom;			// Node, bottom layer
  int i_drain;			// Node, drain

  // Data.
  double height;		// Groundwater table height above surface. [cm]
  double GWT_new;		// Updated GWT
  double DrainFlow;		// Drain flow [cm/h]
  vector<double> S;		// Pipe drainage. [cm^3/cm^3/h]
  vector<double> Percolation;	// [cm^3/cm^2/h]


  // UZbottom.
public:
  bool flux_bottom () const
    { return false; }
  bool accept_bottom (double)
    { return true; }

  // Simulation.
public:
  void tick (const Time&)
    { }
  void update_water (const Soil&,
  		     vector<double>& S_sum,
		     vector<double>& h,
		     vector<double>& h_ice,
		     vector<double>& Theta,
		     vector<double>& q,
		     vector<double>& q_p);
  void output (Log& log) const;

private:
  double DeepPercolation (const Soil&);
  double EquilibriumDrainFlow (const Soil&);
  void RaisingGWT  (const Soil&,
                    vector<double>& h, vector<double>& h_ice,
                    vector<double>& Theta, vector<double>& q,
                    vector<double>& q_p);
  void FallingGWT1 (const Soil&,
                    vector<double>& h, vector<double>& h_ice,
                    vector<double>& Theta);
  void FallingGWT2 (const Soil&,
                    vector<double>& h, vector<double>& h_ice,
                    vector<double>& Theta);
  void Update_GWT  (const Soil&,
                    vector<double>& h, vector<double>& h_ice,
                    vector<double>& Theta, vector<double>& q,
                    vector<double>& q_p);
  double table () const
    { return height; }

  // Create and Destroy.
public:
  void initialize (const Time&, const Soil& soil)
    {
      unsigned int size = soil.size ();

      i_bottom = size - 1;
      i_drain = soil.interval_plus (pipe_position);

      S.insert (S.end (), size, 0.0);
      Percolation.insert (Percolation.end (), size+1u, 0.0);
    }
  GroundwaterPipe (const AttributeList& al)
    : Groundwater (al),
      L (al.number ("L")),
      x (al.check ("x") ? al.number ("x") : L / 2.0),
      pipe_position (al.number ("pipe_position")),
      K_aquitard (al.number ("K_aquitard")),
      Z_aquitard (al.number ("Z_aquitard")),
      h_aquifer (al.check ("h_aquifer") ? al.number ("h_aquifer") : Z_aquitard),
      height (al.check ("height") ? al.number ("height") : pipe_position)
    { }
  ~GroundwaterPipe ()
    { }
};

void
GroundwaterPipe::update_water (const Soil& soil,
			       vector<double>& S_sum,
			       vector<double>& h,
			       vector<double>& h_ice,
			       vector<double>& Theta,
			       vector<double>& q,
			       vector<double>& q_p)
{
  fill (S.begin (), S.end (), 0.0);
  for (unsigned int i = 1; i <= i_bottom+1; i++)
    {
       Percolation[i-1] = - q[i] - q_p[i];
    }
  Percolation[i_bottom+1] = DeepPercolation(soil);
  const double EquilibriumFlow = EquilibriumDrainFlow (soil);
  Update_GWT (soil, h, h_ice, Theta, q, q_p);
  DrainFlow = 0.0;
  for (unsigned int i = 0; i <= i_bottom; i++)
    {
       S_sum[i] += S[i];
       DrainFlow += S[i] * soil.dz (i);
    }
  for (unsigned int i = 1; i <= i_bottom+1; i++)
    {
       if (-q_p[i]>0)
         {
           if (-q[i]>0)
             {
               const double x_p = q_p[i]/(q[i] + q_p[i]);
               q_p[i] = - x_p * Percolation[i-1];
               q[i] = -Percolation[i-1] - q_p[i];
             }
           else
             {
               q[i] = -Percolation[i-1] - q_p[i];
             }
         }
       else
         {
           assert (q_p[i] == 0.0);
           q[i] = - Percolation[i-1];
         }
       assert (finite (q[i]));
    }
  height = GWT_new;
}

double
GroundwaterPipe::DeepPercolation(const Soil& soil)
{
  const double hb = height - soil.zplus(i_bottom);
  if (hb > 0)
    return K_aquitard * (1.0 + (hb - h_aquifer) / Z_aquitard);
  else
    return 0;
}

double
GroundwaterPipe::EquilibriumDrainFlow (const Soil& soil)
{
  const int i_GWT = soil.interval_plus (height) + 1;
  if (height >= soil.zplus(i_drain-1))
    {
      // GWT located above drain
      double Ha = 0;
      double Ka = 0;
      for (unsigned int i = i_GWT; i <= i_drain; i++)
	{
	  Ha += soil.dz (i);
	  Ka += soil.dz (i) * soil.K (i, 0.0, 0.0);
	}
      Ka /= Ha;

      // GWT located belove drain
      double Hb = 0;
      double Kb = 0;
      for (unsigned int i = i_drain+1; i <= i_bottom; i++)
	{
	  Hb += soil.dz (i);
	  Kb += soil.dz (i) * soil.K (i, 0.0, 0.0);
	}
      Kb /= Hb;
      const double Flow = (4*Ka*Ha*Ha + 2*Kb*Hb*Ha) / (L*x - x*x);

      // Distribution of drain flow among numeric soil layers
      const double a = Flow / (Ka*Ha + Kb*Hb);
      for (unsigned int i = 0; i < i_bottom; i++)
	{
	  if (i >= i_GWT)
	    S[i] = a * soil.K (i, 0.0, 0.0);
	}
      assert (finite (Flow));
      Percolation[i_bottom+1] = Flow;
      for (unsigned int i = i_bottom; i >= i_drain; i--)
	{
	  Percolation[i] = Percolation[i+1] + S[i] * soil.dz (i);
	}
      return Flow;
    }
  else
    {
      for (unsigned int i = i_bottom; i >= i_drain; i--)
	Percolation[i] = Percolation[i+1] + S[i] * soil.dz (i);
      return 0.0;
    }
}

void
GroundwaterPipe::Update_GWT (const Soil& soil,
                             vector<double>& h, vector<double>& h_ice,
                             vector<double>& Theta, vector<double>& q,
                             vector<double>& q_p)
{
  const double z_drain = soil.zplus (i_drain);
  const int i_GWT = soil.interval_plus (height) + 1;
  if (-(q[i_drain+1]+q_p[i_drain+1]) > Percolation[i_drain])
    RaisingGWT (soil, h, h_ice, Theta, q, q_p);
  else if (height<=z_drain)
    FallingGWT1 (soil, h, h_ice, Theta);
  else
    FallingGWT2 (soil, h, h_ice, Theta);
}

void
GroundwaterPipe::RaisingGWT (const Soil& soil,
                             vector<double>& h, vector<double>& h_ice,
                             vector<double>& Theta, vector<double>& q,
                             vector<double>& q_p)
{
  const int i_UZ = soil.interval_plus (height);
  GWT_new = height;
  for (int i = i_UZ; i >= 0; i--)
    {
       const double dq = -(q[i+1] + q_p[i+1]) - Percolation[i];
       const double dTheta = dq / soil.dz (i) * dt;
       const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
       if (Theta[i] + dTheta <= ThetaS)
         {
           Theta[i] += dTheta;
           h[i] = soil.h(i,Theta[i]);
           break;
         }
       else
         {
           if (i>0)
             GWT_new = soil.zplus (i-1);
           else
             throw ("Tile drain system inadequate. Soil profile saturated");
           Percolation[i-1] -= (ThetaS-Theta[i]) * soil.dz (i) / dt;
           Theta[i] = ThetaS;
        }
    }
  const int i_GWT = soil.interval_plus (GWT_new) + 1;
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    {
       h[i] = GWT_new - soil.z (i);
    }
}

void
GroundwaterPipe::FallingGWT1 (const Soil& soil,
                              vector<double>& h, vector<double>& h_ice,
                              vector<double>& Theta)
{
  GWT_new = height;
  const double z_drain = soil.zplus (i_drain);
  const int i_GWT = soil.interval_plus (height) + 1;
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    {
       const double qi = Percolation[i-1];
       const double qo = Percolation[i];
       if (i <= i_drain)
         {
           const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
           const double h1 = max ( z_drain - height, -100.0);
           const double Theta1 = soil.Theta(i,h1,0.0);
           const double h2 = max (-soil.dz (i) / 2.0, h1);
           const double Theta2 = soil.Theta(i,h2,0.0);
           const double W1 = (ThetaS - Theta1) * soil.dz (i) / dt;
           const double W2 = (ThetaS - Theta2) * soil.dz (i) / dt;
           if (qo-qi < W2)
             {
               h[i] = h2;
               Theta[i] = Theta2;
               const double W = (ThetaS - Theta2) * soil.dz (i) / dt + qi - qo;
               S[i] = W / soil.dz (i);
               GWT_new = soil.zplus (i);
               break;
             }
           else if (qo-qi < W1)
             {
               S[i] = 0.0;
               Theta[i] = ThetaS + (qi-qo) * dt / soil.dz (i);
               h[i] = soil.h(i,Theta[i]);
               GWT_new = soil.zplus (i);
               break;
             }
           else
             {
               S[i] = 0.0;
               Theta[i] = Theta2;
               h[i] = h2;
               Percolation[i] = qi + (ThetaS - Theta2) * soil.dz (i) / dt;
             }
         }
       else
         {
           // GWT has fallen to the depth of the drain pipes,
           // and flow to the drains ceases
           S[i] = 0.0;
           Percolation[i] = Percolation[i-1];
           GWT_new = z_drain;
         }
    }
}

void
GroundwaterPipe::FallingGWT2 (const Soil& soil,
                              vector<double>& h, vector<double>& h_ice,
                              vector<double>& Theta)
{
  GWT_new = height;
  const double z_bottom = soil.zplus(i_bottom);
  const int i_GWT = soil.interval_plus (height) + 1;
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    {
       const double qi = Percolation[i-1];
       const double qo = Percolation[i];
       const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
       const double h1 = max ( z_bottom - height, -100.0);
       const double Theta1 = soil.Theta(i,h1,h_ice[i]);
       const double h2 = max (-soil.dz (i) / 2.0, h1);
       const double Theta2 = soil.Theta(i,h2,h_ice[i]);
       const double W1 = (ThetaS - Theta1) * soil.dz (i) / dt;
       const double W2 = (ThetaS - Theta2) * soil.dz (i) / dt;
       if (qo-qi < W2)
         {
           h[i] = h2;
           Theta[i] = Theta2;
           GWT_new = soil.zplus (i);
           break;
         }
       else if (qo-qi < W1)
         {
           Theta[i] = ThetaS + (qi-qo) * dt / soil.dz (i);
           h[i] = soil.h(i,Theta[i]);
           GWT_new = soil.zplus (i);
           break;
         }
       else
         {
           Theta[i] = Theta2;
           h[i] = h2;
           Percolation[i] = qi + (ThetaS - Theta2) * soil.dz (i) / dt;
           GWT_new = z_bottom;
         }
    }
}

void
GroundwaterPipe::output (Log& log) const
{
  Groundwater::output (log);
  log.output ("DrainFlow", DrainFlow);
}

static struct GroundwaterPipeSyntax
{
  static Groundwater& make (const AttributeList& al)
    {
      return *new GroundwaterPipe (al);
    }
  GroundwaterPipeSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Groundwater for pipe (tile) drained soil.");
      Groundwater::load_syntax (syntax, alist);

      syntax.add ("L", "cm", Syntax::Const,
		  "Distance between pipes.");
      alist.add ("L", 1800.0);
      syntax.add ("x", "cm", Syntax::OptionalConst,
		  "Horizontal distance to nearest pipe.\n\
By default, this is 1/2 L.");
      syntax.add ("pipe_position", "cm", Syntax::Const,
		  "Height pipes are placed in the soil (a negative number).");
      alist.add ("pipe_position", -110.0);
      syntax.add ("K_aquitard", " cm/h", Syntax::Const,
		  "Conductivity of the aquitard.");
      alist.add ("K_aquitard", 1.0E-5);
      syntax.add ("Z_aquitard", " cm", Syntax::Const,
		  "Height of the aquitard.");
      alist.add ("Z_aquitard", 200.0);
      syntax.add ("h_aquifer", " cm ", Syntax::OptionalConst,
		  "Pressure potential in the aquifer below the aquitard.\n\
By default. this is Z_aquitard.");

      syntax.add ("height", "cm", Syntax::OptionalState,
		  "Current groundwater level (a negative number).");
      syntax.add ("DrainFlow", "cm/h", Syntax::LogOnly,
		  "Drain flow to pipes.");
      syntax.add ("S", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		  "Pipe drainage.");
      Librarian<Groundwater>::add_type ("pipe", alist, syntax, &make);
    }
} GroundwaterPipe_syntax;


