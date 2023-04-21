// helpers.h
//
/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */

// This work was supported by the Assistant Secretary for Energy Efficiency
// and Renewable Energy, Office of Building Technologies,
// Building Systems and Materials Division of the
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

/*
NOTICE: The Government is granted for itself and others acting on its behalf
a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce,
prepare derivative works, and perform publicly and display publicly.
Beginning five (5) years after (date permission to assert copyright was obtained),
subject to two possible five year renewals, the Government is granted for itself
and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
license in this data to reproduce, prepare derivative works, distribute copies to
the public, perform publicly and display publicly, and to permit others to do so.
NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
*/
Double			DegToRad(Double angle);

Double			RadToDeg(Double angle);

BGL::vector3	AnglesToDir3D(Double phi, Double theta);	//	angles input in radians

vector<Double>	Dir3DToAngles(BGL::vector3 vDir);			//	angles output in radians

//	directional luminance generators
struct LumParam
{
	LumParam();
	~LumParam();

	string	object;
	string	source;
	string	filename;
	string	type;
	Double	BFlux0;
	Double	dispersion;
	Double	phi0;
	Double	theta0;
	BGL::vector3	Dir0;
	Double	GndRefl;
	string	btdftype;

	Int		btdfHSResIn;
	Int		btdfHSResOut;

	Double	visTransNormal;
	Double	visTransExponent;

	string	EPlusType;
	Double	EPlusCoef[6];

	Double	LightShelfReflectance;

    // "SKY^GEN^CIECLEARSKY^SunAlt^SunAzm^ZenLum^GrndRefl"
	Double	dSunAltRadians;
	Double	dSunAzmRadians;
	Double	dZenithLum;
    // "SKY^GEN^CIECLEARSUN^SunAlt^SunAzm^Solic^TFac^AtmMoi^AtmTurb^BldgAlt^GrndRefl"
	Double	dMonthlyExtraTerrIllum;
	Double	dTurbidityFactor;
	Double	dBldgMonthlyAtmosMois;
	Double	dBldgMonthlyAtmosTurb;
	Double	dBldgAltitude;

	string	BadName;

	void	Dump();
};

Double		ConstLum(LumParam lp, BGL::vector3 Direction);
Double		CosThetaLum(LumParam lp, BGL::vector3 Direction);
Double		SimpleBeamLum(LumParam lp, BGL::vector3 Direction);
Double		SuperLambertianLum(LumParam lp, BGL::vector3 Direction);
Double		GaussLum(LumParam lp, BGL::vector3 Direction);

Double		CIEOvercastSkyLum(LumParam lp, BGL::vector3 Direction);
Double		CIEClearSkyLum(LumParam lp, BGL::vector3 Direction);
Double		CIEClearTurbidSkyLum(LumParam lp, BGL::vector3 Direction);
Double		CIEIntermediateSkyLum(LumParam lp, BGL::vector3 Direction);
Double		CIEClearSunLum(LumParam lp, BGL::vector3 Direction);

Double		GenDirLum(LumParam lp, BGL::vector3 Direction);

struct HemiSphiral;	//	forward declaration
HemiSphiral GenLuminanceMap(LumParam lp);

HemiSphiral	GenSky(LumParam lp);

struct btdfHS;	//	forward declaration
btdfHS*	GenBTDF(LumParam& lp);

struct btdf;	//	forward declaration
HemiSphiral	SkyBTDFIntegration(HemiSphiral& sky0, btdf* pbtdf0, BGL::RHCoordSys3 ics);

HemiSphiral	GenWindowMap(LumParam& lp, HemiSphiral& sky0, BGL::RHCoordSys3 ics);

bool		IsValidTypeName(string nametype, string inname);

bool		SecretDecoderRing(LumParam& lp, string InStr);

//***************************** subroutine vParseList *****************************
// Parses the given delimiter-separated string and passes back a vector of the
// parsed substrings
//****************************** subroutine vParseList *****************************
bool			charInList(const char c0, string delimList);
vector<string>	vParseList(string InStr, string delimList ="^");

struct FILE_FLG	//	used with writewndo
{
	bool zero, out, log, dbg, err, warn;
	FILE_FLG();
	FILE_FLG(string sfpflg);
};


struct RADdata
{
	int		ndim;	//	NOTE: DataArray only works for ndim == 2!
	double	beg1, end1;
	int		n1;
	double	beg2, end2;
	int		n2;
	vector< vector<double> >	DataArray;

	int		load(string filename);
	void	summary(ostream& outfile);
	void	dump(ostream& outfile);
	HemiSphiral	convertToHS();
};

struct IESNAdata
{
	vector<string>	headerlineList;
	int				LampToLumGeom;		//	"Line 06"
	int				nPairs;				//	"Line 07"
	vector<double>	angleList;			//	"Line 08"
	vector<double>	MultFacList;		//	"Line 09"
	int		nLamps;
	double	LampLumens;
	double	CandelaMult;
	int		nTheta;
	int		nPhi;
	int		PhotometricType;
	int		units;
	double	LumDimWidth, LumDimLength, LumDimHeight;
	double	BallastFactor;
	double	BallastLampPhotoFactor;
	double	InputWatts;
	vector<double>	theta;
	vector<double>	phi;
	vector< vector<double> >	DataArray;

	int		load(string filename);
	void	summary(ostream& outfile);
	void	dump(ostream& outfile);
	HemiSphiral	convertToHS();
};

