// crop_impl.h

#include "crop.h"
#include "ftable.h"

struct ValueCSMP;

typedef void (*CropFun)(const Crop::Parameters&, Crop::Variables&);

struct Crop::Parameters
{ 
    const struct DevelPar
    {
	static dFTable<CropFun> models;
	CropFun Model;		// Phenological development model ID
        double EmrTSum;		// Soil temp sum at emergence
        double DS_Emr;		// Development stage (DS) emergence
        double DSRate1;		// Development rate [C-1 or d-1],
				// the vegetative stage
        double DSRate2;		// Development rate [C-1 or d-1],
				// the reproductive stage
	const ValueCSMP* TempEff1; // Temperature effect, vegetative stage
	const ValueCSMP* TempEff2; // Temperature effect, reproductive stage
	const ValueCSMP* PhotEff1; // Ptotoperiode effect, vegetative stage
    private:
	friend class Crop::Parameters;
	DevelPar (const ValueList*);
    } Devel;
    const struct VernalPar {
        bool required;
        double DSLim1;		// DS at beginning of vernalization
        double DSLim2;		// DS at end of vernalization
        double TaLim;		// Vernalization temp threshold
        double TaSum;		// Vernalization T-sum requirement
    private:
	friend class Crop::Parameters;
	VernalPar (const ValueList*);
    } Vernal;
    const struct LeafPhotPar {
	static dFTable<CropFun> models;
	CropFun Model;		// exponential or parabolic
        double Qeff;		// Quantum efficiency at low light
        double Fm;		// Max assimilation rate
        double TLim1;		// Lowest temp for photosynthesis
        double TLim2;		// Lowest temp for unrestricted phot.
    private:
	friend class Crop::Parameters;
	LeafPhotPar (const ValueList*);
    } LeafPhot;
    const struct CanopyPar {
        double DSinit;		// DS at end of initial LAI-Development
        double WLfInit;		// WLeaf at end of initial LAI-Development
        double SpLAI;		// Specific leaf weight
	const ValueCSMP* HvsDS;	// Crop height as function of DS
	double LAIDist0[3];	// Relative LAI distribution at DS=0
	double LAIDist1[3];	// Relative LAI distribution at DS=1
        double LAIDista;        // LAI distribution transition value
        double PARref;		// PAR reflectance
        double PARext;		// PAR extinction coefficient
        double EPext;		// EP extinction coefficient
    private:
	friend class Crop::Parameters;
	CanopyPar (const ValueList*);
    } Canopy;
    const struct RootPar {
        double DptEmr;		// Penetration at emergence
        double PenPar1;		// Penetration rate parameter, coefficient
        double PenPar2;		// Penetration rate parameter, threshold
        double MaxPen;		// Max penetration depth
        double SpRtLength;	// Specific root length
        double DensRtTip;	// Root density at (pot) penetration depth
        double Rad;		// Root radius
        double h_wp;		// Matrix potential at wilting
        double MxNH4Up;		// Max NH4 uptake per unit root length
        double MxNO3Up;		// Max NO3 uptake per unit root length
    private:
	friend class Crop::Parameters;
	RootPar (const ValueList*);
    } Root;
    const struct PartitPar {
	const ValueCSMP* Root;	// Partitioning functions for root
	const ValueCSMP* Leaf;	//   leaf, and stem as function of DS
	const ValueCSMP* Stem;
	const ValueCSMP* LfDR;	// Death rate of Leafs
    private:
	friend class Crop::Parameters;
	PartitPar (const ValueList*);
    } Partit;
    struct RespPar {
        double E_Root;		// Conversion efficiency, root
        double E_Leaf;		// Conversion efficiency, leaf
        double E_Stem;		// Conversion efficiency, stem
        double E_SOrg;		// Conversion efficiency, stor. org.
        double r_Root;		// Maint. resp. coeff., root
        double r_Leaf;		// Maint. resp. coeff., leaf
        double r_Stem;		// Maint. resp. coeff., stem
        double r_SOrg;		// Maint. resp. coeff., stor. org.
        double Q10;		// Maint. resp. Q10-value
    private:
	friend class Crop::Parameters;
	RespPar (const ValueList*);
    } Resp;
    struct CrpNPar {
        double SeedN;		// N-content in seed
	const ValueCSMP* PtLeafCnc; // Upper limit for N-conc in leaves
	const ValueCSMP* CrLeafCnc; // Critical lim f. N-conc in leaves
	const ValueCSMP* PtStemCnc; // Upper limit for N-conc in stems
	const ValueCSMP* CrStemCnc; // Critical lim f. N-conc in stems
	const ValueCSMP* PtRootCnc; // Upper limit for N-conc in roots
	const ValueCSMP* CrRootCnc; // Critical lim f. N-conc in roots
	const ValueCSMP* PtSOrgCnc; // Upper limit for N-conc in stor org
	const ValueCSMP* CrSOrgCnc; // Critical lim f. N-conc in stor org
    private:
	friend class Crop::Parameters;
	CrpNPar (const ValueList*);
    } CrpN;
private:
    friend class Crop;
    Parameters (const ValueList*);
public:
    ~Parameters ();
};

struct Crop::Variables
{ 
private:
    friend class Crop;
    Variables ();
public:
    ~Variables ();
};
