// crop_impl.h

#ifndef CROP_IMPL_H
#define CROP_IMPL_H

#include "crop.h"
#include "ftable.h"

struct CSMP;

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
	const CSMP& TempEff1;   // Temperature effect, vegetative stage
	const CSMP& TempEff2;   // Temperature effect, reproductive stage
	const CSMP& PhotEff1;   // Ptotoperiode effect, vegetative stage
    private:
	friend struct Crop::Parameters;
	DevelPar (const AttributeList&);
    } Devel;
    const struct VernalPar {
        bool required;
        double DSLim1;		// DS at beginning of vernalization
        double DSLim2;		// DS at end of vernalization
        double TaLim;		// Vernalization temp threshold
        double TaSum;		// Vernalization T-sum requirement
    private:
	friend struct Crop::Parameters;
	VernalPar (const AttributeList&);
    } Vernal;
    const struct LeafPhotPar {
	static dFTable<CropFun> models;
	CropFun Model;		// exponential or parabolic
        double Qeff;		// Quantum efficiency at low light
        double Fm;		// Max assimilation rate
        double TLim1;		// Lowest temp for photosynthesis
        double TLim2;		// Lowest temp for unrestricted phot.
    private:
	friend struct Crop::Parameters;
	LeafPhotPar (const AttributeList&);
    } LeafPhot;
    const struct CanopyPar {
        double DSinit;		// DS at end of initial LAI-Development
        double WLfInit;		// WLeaf at end of initial LAI-Development
        double SpLAI;		// Specific leaf weight
	const CSMP& HvsDS;	// Crop height as function of DS
	const vector<double>& LAIDist0; // Relative LAI distribution at DS=0
	const vector<double>& LAIDist1; // Relative LAI distribution at DS=1
        double LAIDista;        // LAI distribution transition value
        double PARref;		// PAR reflectance
        double PARext;		// PAR extinction coefficient
        double EPext;		// EP extinction coefficient
    private:
	friend struct Crop::Parameters;
	CanopyPar (const AttributeList&);
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
	friend struct Crop::Parameters;
	RootPar (const AttributeList&);
    } Root;
    const struct PartitPar {
	const CSMP& Root;	// Partitioning functions for root
	const CSMP& Leaf;	//   leaf, and stem as function of DS
	const CSMP& Stem;
	const CSMP& LfDR;	// Death rate of Leafs
	const CSMP& RtDR;	// Death rate of Roots
    private:
	friend struct Crop::Parameters;
	PartitPar (const AttributeList&);
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
	friend struct Crop::Parameters;
	RespPar (const AttributeList&);
    } Resp;
    struct CrpNPar {
        double SeedN;		// N-content in seed
	const CSMP& PtLeafCnc;	// Upper limit for N-conc in leaves
	const CSMP& CrLeafCnc;	// Critical lim f. N-conc in leaves
	const CSMP& PtStemCnc;	// Upper limit for N-conc in stems
	const CSMP& CrStemCnc;	// Critical lim f. N-conc in stems
	const CSMP& PtRootCnc;	// Upper limit for N-conc in roots
	const CSMP& CrRootCnc;	// Critical lim f. N-conc in roots
	const CSMP& PtSOrgCnc;	// Upper limit for N-conc in stor org
	const CSMP& CrSOrgCnc;	// Critical lim f. N-conc in stor org
    private:
	friend struct Crop::Parameters;
	CrpNPar (const AttributeList&);
    } CrpN;
    static const Parameters& get (const string, const AttributeList&);
private:
    // BUG: We should have a STL map<string, Parameters> instead of
    // having each Parameters object know its own name.
    typedef list<const Parameters*> pList;
    const string name;
    static pList all;
    Parameters (const string, const AttributeList&);
public:
    ~Parameters ();
};

struct Crop::Variables
{ 
    struct RecPhenology
    {
	double DS;		// Development Stage
	double Vern;		// Vernalization criterium [C d]
    private:
	friend struct Crop::Variables;
	RecPhenology (const AttributeList&);
	RecPhenology (const Parameters&);
    } Phenology;
    struct RecCanopy
    {
	double Height;		// Crop height [cm]
	double LAI;		// Leaf Area Index
	double LADm;		// Max Leaf Area Density [cm2/cm3]
	vector<double> LADDist;	// LAD vs height
    private:
	friend struct Crop::Variables;
	RecCanopy (const AttributeList&);
	RecCanopy (const Parameters&);
    } Canopy;
    struct RecRootSys
    {
	double Depth;		// Rooting Depth [cm]
	vector<double> Density;	// Root density [cm/cm3] in soil layers
	vector<double> H2OExtraction; // Extraction of H2O in soil layers
				      // [cm/h]
	vector<double> NH4Extraction; // Extraction of NH4-N in soil layers
				      // [mg/m2/h]
	vector<double> NO3Extraction; // Extraction of NH4-N in soil layers
				      // [mg/m2/h]
    private:
	friend struct Crop::Variables;
	RecRootSys (const AttributeList&);
	RecRootSys (const Parameters&);
    } RootSys;
    struct RecProd
    {
	double WLeaf;		// Leaf dry matter weight [g/m2]
	double WStem;		// Stem dry matter weight [g/m2]
	double WRoot;		// Root dry matter weight [g/m2]
	double WSOrg;		// Storage organ dry matter weight [g/m2]
	double WLDrd;		// Inactive canopy dry matter weight [g/m2]
	double NCrop;		// Nitrogen stored in dry matter [g/m2]
    private:
	friend struct Crop::Variables;
	RecProd (const AttributeList&);
	RecProd (const Parameters&);
    } Prod;
    struct RecCrpAux
    {
	bool InitLAI;		// Initial LAI development ?
	double PotRtDpt;	// Potential Root Penetration Depth [cm]
	double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
	double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
	double PotTransp;	// Potential Transpiration [mm/h]
	double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2/h]
	double CanopyAss;	// Canopy Assimilation [g CH2O/m2/h]
	double IncWLeaf;	// Leaf growth [g DM/m2/d]
	double IncWStem;	// Stem growth [g DM/m2/d]
	double IncWSOrg;	// Storage organ growth [g DM/m2/d]
	double IncWRoot;	// Root growth [g DM/m2/d]
	double H2OUpt;		// H2O uptake [mm/h]
	double NH4Upt;		// NH4-N uptake [g/m2/h]
	double NO3Upt;		// NO3-N uptake [g/m2/h]
    private:
	friend struct Crop::Variables;
	RecCrpAux (const AttributeList&);
	RecCrpAux (const Parameters&);
    } CrpAux;
private:
    friend class Crop;
    Variables (const AttributeList&);
    Variables (const Parameters&);
public:
    ~Variables ();
};

#endif CROP_IMPL_H
