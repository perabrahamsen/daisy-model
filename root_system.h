// root_system.h -- Root development and uptake.

#include <vector>
using namespace std;

struct Geometry;
struct Soil;
struct SoilWater;
struct Solute;
struct SoilNO3;
struct SoilNH4;
struct SoilHeat;

struct Log;
struct AttributeList;
struct Syntax;

class RootSystem
{
  // Parameters.
public:
  double DptEmr;		// Penetration at emergence
private:
  double PenPar1;		// Penetration rate parameter, coefficient
  double PenPar2;		// Penetration rate parameter, threshold
  double MaxPen;		// Max penetration depth
  double SpRtLength;		// Specific root length [m/g]
  double DensRtTip;		// Root density at (pot) penetration depth
  double Rad;			// Root radius [cm]
  double h_wp;			// Matrix potential at wilting
  double MxNH4Up;		// Max NH4 uptake per unit root length
  double MxNO3Up;		// Max NO3 uptake per unit root length
  double Rxylem;		// Transport resistence in xyleme

  // State.
private:
  double PotRtDpt;	        // Potential Root Penetration Depth [cm]
public:
  double Depth;			// Rooting Depth [cm]
  vector<double> Density;	// Root density [cm/cm3] in soil layers
private:
  vector<double> H2OExtraction; // Extraction of H2O in soil [cm³/cm³/h]
  vector<double> NH4Extraction; // Extraction of NH4-N in soil [gN/cm³/h]
  vector<double> NO3Extraction; // Extraction of NH4-N in soil [gN/cm³/h]
  double h_x;			// Root extraction at surface.

  // Log.
public:
  double water_stress;		// Fraction of requested water we got.
  double nitrogen_stress;	// Fraction of requested nitrogen we got.

private:
  double Ept;			// Potential evapotranspiration.
  double H2OUpt;		// H2O uptake [mm/h]
  double NH4Upt;		// NH4-N uptake [g/m2/h]
  double NO3Upt;		// NO3-N uptake [g/m2/h]

  // Uptake.
private:
  double potential_water_uptake (double h_x,
				 const Soil& soil,
				 const SoilWater& soil_water);
public:
  double water_uptake (double Ept,
		       const Soil& soil, SoilWater& soil_water,
		       const double EvapInterception);
private:
  double solute_uptake (const Soil&, const SoilWater&, Solute&,
			double PotNUpt, vector<double>& uptake,
			double i_max);
public:
  double nitrogen_uptake (const Soil& soil,
			  const SoilWater& soil_water,
			  SoilNH4& soil_NH4,
			  SoilNO3& soil_NO3,
			  double PotNUpt);

  // Simulation.
private:
  static double density_distribution_parameter (double a);
public:
  void tick (const Soil&, const SoilHeat&, double WRoot, double IncWRoot);
  void set_density (const Geometry& geometry, const double WRoot);
  void full_grown (const Soil& soil, const double WRoot);
  void output (Log& log) const;

  // Create and Destroy
public:
  void initialize (unsigned int size);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  RootSystem (const AttributeList& al);
  ~RootSystem ();
};

