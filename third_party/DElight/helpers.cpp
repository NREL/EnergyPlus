// helpers.cpp
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

#pragma warning(disable:4786)

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <string>
#include <cstring>
#include <vector>
#include <map>
#include <limits>
#include <stdlib.h>
#include <string>
#include <algorithm> // for min/max
using namespace std;

// BGLincludes
#include "BGL.h"
namespace BGL = BldgGeomLib;

#include "helpers.h"
#include "hemisphiral.h"
#include "btdf.h"

#include "DElightManagerC.h"

#include "DEF.H"

#ifndef INFINITY
extern double INFINITY;
#endif
extern double NaN_QUIET;
extern double NaN_SIGNAL;
extern double MAXPointTol;

//	angle unit conversions
Double			DegToRad(Double angle)
{
	return angle*PI/180.;
}

Double			RadToDeg(Double angle)
{
	return angle*180./PI;
}

//	dir3D - direction angles conversions
BGL::vector3	AnglesToDir3D(Double phi, Double theta)
{
	return BGL::vector3(sin(theta)*cos(phi), sin(theta)*sin(phi), cos(theta));
}

vector<Double>	Dir3DToAngles(BGL::vector3 vDir)
//	XXX this doesn't agree with RHCoordSys3::RotAngles !!!
{
	vector<Double> RotAng(2);

	BGL::normalize(vDir);

	// special cases: tilt angle theta = 0, PI
	if ( vDir[2] >= 1.) RotAng[1] = 0.;
	else if ( vDir[2] <= -1.) RotAng[1] = PI;
	else {	// normal cases
		//	tilt
		RotAng[1] = acos(vDir[2]);
		// azimuth
		RotAng[0] = atan2(vDir[1],vDir[0]);
	}
	return RotAng;
}


LumParam::LumParam()
{
	object = "";
	source = "";
	filename = "";
	type = "";
	BFlux0 = 0;
	dispersion = 0;
	phi0 = 0;
	theta0 = 0;
	Dir0 = BGL::vector3(0,0,0);
	GndRefl = 0;

	btdftype = "";
	btdfHSResIn = BTDF_HSRES_IN;
	btdfHSResOut = BTDF_HSRES_OUT;

	visTransNormal = 0;
	visTransExponent = 0;

	EPlusType = "";
	EPlusCoef[0] = 0;
	EPlusCoef[1] = 0;
	EPlusCoef[2] = 0;
	EPlusCoef[3] = 0;
	EPlusCoef[4] = 0;
	EPlusCoef[5] = 0;

	LightShelfReflectance = 0;

    // "SKY^GEN^CIECLEARSKY^SunAlt^SunAzm^ZenLum^GrndRefl"
	dSunAltRadians = 0;
	dSunAzmRadians = 0;
	dZenithLum = 0;
    // "SKY^GEN^CIECLEARSUN^SunAlt^SunAzm^Solic^TFac^AtmMoi^AtmTurb^BldgAlt^GrndRefl"
	dMonthlyExtraTerrIllum = 0;
	dTurbidityFactor = 0;
	dBldgMonthlyAtmosMois = 0;
	dBldgMonthlyAtmosTurb = 0;
	dBldgAltitude = 0;

	BadName = "";

}

LumParam::~LumParam()
{ }

void	LumParam::Dump()
{
	cout << "object: " << object << "\n";
	cout << "source: " << source << "\n";
	cout << "filename: " << filename << "\n";
	cout << "type: " << type << "\n";
	cout << "BFlux0: " << BFlux0 << "\n";
	cout << "phi0: " << phi0 << "\n";
	cout << "theta0: " << theta0 << "\n";
	cout << "Dir0: " << Dir0 << "\n";
	cout << "dispersion: " << dispersion << "\n";
	cout << "GndRefl: " << GndRefl << "\n";
	cout << "BadName: " << BadName << "\n";
	cout << "btdftype: " << btdftype << "\n";
	cout << "btdfHSResIn: " << btdfHSResIn << "\n";
	cout << "btdfHSResOut: " << btdfHSResOut << "\n";
	cout << "visTransNormal: " << visTransNormal << "\n";
	cout << "visTransExponent: " << visTransExponent << "\n";
	cout << "EPlusType: " << EPlusType << "\n";
	cout << "EPlusCoef: ";
	for (int ii=0; ii<6; ii++) 	cout << EPlusCoef[ii] << " ";
	cout << "\n";
	cout << "LightShelfReflectance: " << LightShelfReflectance << "\n";

	cout << "dSunAltRadians: " << dSunAltRadians << "\n";
	cout << "dSunAzmRadians: " << dSunAzmRadians << "\n";
	cout << "dZenithLum: " << dZenithLum << "\n";

}

bool	IsValidTypeName(string nametype, string inname)
{
	if (nametype == "OBJECT") {
		if (inname == "SKY") return true;
		else if (inname == "BTDF") return true;
		else if (inname == "LUMMAP") return true;
		else if (inname == "WINDOW") return true;
		else {
	//		cerr << "GenLum: Bad type: " << inname << "\n";
			return false;	//	error value
		}
	}
	else if (nametype == "SOURCE") {
		if (inname == "FILE") return true;
		else if (inname == "GEN") return true;
		else {
	//		cerr << "GenLum: Bad type: " << inname << "\n";
			return false;	//	error value
		}
	}
	else if (nametype == "GENTYPE") {
		if (inname == "SUPERLAMBERTIAN") return true;
		else if (inname == "GAUSS") return true;
		else if (inname == "SIMPLEBEAM") return true;
		else if (inname == "CONST") return true;
		else if (inname == "CIEOVERCASTSKY") return true;
		else if (inname == "CIECLEARSKY") return true;
		else if (inname == "CIECLEARSUN") return true;
		else if (inname == "SINGLEPANE") return true;
		else if (inname == "EPLUS") return true;
		else if (inname == "WINDOW") return true;
		else if (inname == "LIGHTSHELF") return true;
		else {
	//		cerr << "GenLum: Bad type: " << inname << "\n";
			return false;	//	error value
		}
	}
	else {
//		cerr << "GenLum: Bad nametype: " << nametype << "\n";
		return false;	//	error value
	}
}

Double	GenDirLum(LumParam lp, BGL::vector3 Direction)
{
	Double (*fptr) (LumParam, BGL::vector3);
	if (lp.type == "SUPERLAMBERTIAN") fptr = &SuperLambertianLum;
	else if (lp.type == "GAUSS") fptr = &GaussLum;
	else if (lp.type == "SIMPLEBEAM") fptr = &SimpleBeamLum;
	else if (lp.type == "CONST") fptr = &ConstLum;
	else if (lp.type == "CIEOVERCASTSKY") fptr = &CIEOvercastSkyLum;
	else if (lp.type == "CIECLEARSKY") fptr = &CIEClearSkyLum;
	else if (lp.type == "CIECLEARSUN") fptr = &CIEClearSunLum;
	else {
//		cerr << "GenLum: Bad type: " << syst_type << "\n";
		return -1;	//	error value
	}
	return fptr(lp,Direction);
}

//HemiSphiral GenLuminanceMap(int size, LumParam lp)
HemiSphiral GenLuminanceMap(LumParam lp)
{
	int	zMin = -1;
	HemiSphiral	hs0(zMin,lp.btdfHSResOut);

	Double (*fptr) (LumParam,BGL::vector3);

	if (lp.type == "SUPERLAMBERTIAN") fptr = &SuperLambertianLum;
	else if (lp.type == "GAUSS") fptr = &GaussLum;
	else if (lp.type == "SIMPLEBEAM") fptr = &SimpleBeamLum;
	else if (lp.type == "CONST") fptr = &ConstLum;
	else if (lp.type == "CIEOVERCASTSKY") fptr = &CIEOvercastSkyLum;
	else if (lp.type == "CIECLEARSKY") fptr = &CIEClearSkyLum;
	else if (lp.type == "CIECLEARSUN") fptr = &CIEClearSunLum;
	else {
//		cerr << "GenLum: Bad type: " << syst_type << "\n";
		hs0.resize(0);	//	error value
		return hs0;
	}
//	for (int ii=0; ii < size; ii++) {
	for (int ii=0; ii < lp.btdfHSResOut; ii++) {
		hs0.valList[ii] = fptr(lp,hs0.dir(ii));
		//	 convert sun Illum to Lum based on skyMap omega
		if (lp.type == "CIECLEARSUN") hs0.valList[ii] /= hs0.omega;
	}
	return	hs0;
}



Double	ConstLum(LumParam lp, BGL::vector3 Direction)
{
        (void)Direction;
	//	factor 1/2 is 2PI/4PI:
	//	source BFlux0 per nit area is scattered into 2PI hemisphere
	//	LumMap assumes 4PI tot solid angle
//	return lp.BFlux0/2;
	return lp.BFlux0;
}

Double	CosThetaLum(LumParam lp)
{
	return lp.BFlux0;
}

Double	SimpleBeamLum(LumParam lp, BGL::vector3 Direction)
{
	//input Direction vec is UNIT LENGTH and in NATRUAL LOCAL COORD SYST for CFSSurf/Syst
	Double CosConeAngle = cos(lp.dispersion*PI/180.);
	//	hard cutoff if SURF node direction is outside Beam source light ConeAngle
	if (BGL::dot(lp.Dir0,Direction) < CosConeAngle) return (0.);
	//  Flux perp to CFS surface per unit of ConeSolidAngle
	Double	BFlux1 = lp.BFlux0;
	Double ConeSolidAngle = 2*PI*(1. - CosConeAngle);
	Double FluxRatio = BFlux1/ConeSolidAngle;	//	flux per unit solid angle of cone
	// otherwise return perp flux per unit ConeSolidAngle per unit CFS node area
	return(FluxRatio);
}

Double	SuperLambertianLum(LumParam lp, BGL::vector3 Direction)
{
	if (lp.dispersion <= 0) return 0;

	Double	ConeAngle = DegToRad(lp.dispersion);
//	Double Dot = Max(0.,BGL::dot(lp.Dir0,Direction));
	Double Dot = max(0.,BGL::dot(lp.Dir0,Direction));

	Double Power = PI/ConeAngle - 1.;
	Double SLbrt = lp.BFlux0*pow(Dot,Power);
//	cout << " " << Dir0 << " " << Power << " " << Dot << " " << SLbrt << "\n";
	return SLbrt;
}

Double	GaussLum(LumParam lp, BGL::vector3 Direction)
{
	lp.dispersion /= 100.;	//	sets roughly equal dispersion with same ConeAngle in other DirLum types
	Double Dot = max(min(BGL::dot(lp.Dir0,Direction),1.),-1.);	//	Dot is chord dist between Dir0, Direction unit vecs
	Double theta = acos(Dot);	//	angle theta is great circle arcdist between Dir0, Direction vecs on unit sphere surface
//	Double x = (1. - Dot) / (2*(1. + Dot));
	Double x;
	if ( fabs(1. - theta/PI) <= 1.e-10) x = theta/1.e-20;	//	avoid underflow
	else x = theta / (1. - theta/PI);
	Double sigmasq = lp.dispersion*lp.dispersion;
	Double exponent = (x*x)/(2.*sigmasq);
	if ( exponent > 50) return 0;	//	avoid too small argument for exp()
	Double Gbrt = lp.BFlux0*exp(-exponent);
	return Gbrt;
}

Double	CIEOvercastSkyLum(LumParam lp, BGL::vector3 Direction)
{
    // Calculates luminance (CD/FT**2) of CIE Standard Overcast skies in the given Direction.

    // Conventions:
        // Altitudes are 0 at horizon and 90 at zenith
        // Azimuths are 0 East and counter-clockwise is positive

    // Sun altitude (radians) for this overcast sky condition
    Double dSunAlt = lp.dSunAltRadians;
    Double dSinSunAlt = sin(dSunAlt);

    // Sine of sky patch altitude for this calculation
    Double dSinSkyAlt = Direction[2];

    // CIE Overcast Sky luminance (CD/ft2) for given sun and sky patch altitudes
    //  (0.123 + 8.6 * dSinSunAlt == ZENITH LUM IN KCD/M**2)
	//  (92.9 is conversion for zenith luminance from KCD/M**2 to CD/ft**2
	Double CIEOvercastSkyLuminance = 92.9 * (0.123 + 8.6 * dSinSunAlt) * (0.33333 + 0.66667 * dSinSkyAlt);

	return CIEOvercastSkyLuminance;
}

Double	CIEClearSkyLum(LumParam lp, BGL::vector3 Direction)
{
    // Calculates luminance (CD/FT**2) of CIE Standard Clear skies in the given Direction.

    // Conventions:
        // Altitudes are 0 at horizon and 90 at zenith
        // Azimuths are 0 East and counter-clockwise is positive
        // All angles are in radians

    // Sun altitude (radians) for this clear sky sun position
    Double dSunAlt = lp.dSunAltRadians;
    Double dSinSunAlt = sin(dSunAlt);

    // Sine of sky patch altitude
    Double dSinSkyAlt = Direction[2];
    // Sky patch altitude
    Double dSkyAlt = asin(dSinSkyAlt);

    // Sky patch azimuth
    Double dSkyAzm;
    if ((Direction[0]==0.0) && (Direction[1]==0.0)) dSkyAzm = 0.;
    else dSkyAzm = atan2(Direction[1], Direction[0]);

	// angle between sun and element of sky
	Double cangle = dSinSkyAlt * dSinSunAlt + cos(dSkyAlt) * cos(dSunAlt) * cos(dSkyAzm - lp.dSunAzmRadians);
	/* prevent cangle out of range due to roundoff */
	cangle = max(-1.0,min(cangle,1.0));
	Double angle = acos(cangle);

	/* various luminance factors */
	Double z1 = 0.91 + 10.0 * exp(-3.0 * angle) + 0.45 * cangle * cangle;
	Double z2 = 1.0 - exp(-0.32 / dSinSkyAlt);
	Double z3 = 0.27385 * (0.91 + 10.0 * exp(-3.0 * (1.5708 - dSunAlt)) + 0.45 * dSinSunAlt * dSinSunAlt);

	/* luminance of sky element */
	// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
	Double CIEClearSkyLuminance = 92.9 * lp.dZenithLum * z1 * z2 / z3;

//	return CIEClearSkyLuminance;
	return max(0.,CIEClearSkyLuminance);
}

Double	CIEClearTurbidSkyLum(LumParam lp, BGL::vector3 Direction)
{
    // Calculates luminance (CD/FT**2) of CIE Standard Clear Turbid skies in the given Direction.

    // Conventions:
        // Altitudes are 0 at horizon and 90 at zenith
        // Azimuths are 0 East and counter-clockwise is positive
        // All angles are in radians

    // Sun altitude (radians) for this clear sky sun position
    Double dSunAlt = lp.dSunAltRadians;
    Double dSinSunAlt = sin(dSunAlt);

    // Sine of sky patch altitude
    Double dSinSkyAlt = Direction[2];
    // Sky patch altitude
    Double dSkyAlt = asin(dSinSkyAlt);

    // Sky patch azimuth
    Double dSkyAzm;
    if ((Direction[0]==0.0) && (Direction[1]==0.0)) dSkyAzm = 0.;
    else dSkyAzm = atan2(Direction[1], Direction[0]);

	// angle between sun and element of sky
	Double cangle = dSinSkyAlt * dSinSunAlt + cos(dSkyAlt) * cos(dSunAlt) * cos(dSkyAzm - lp.dSunAzmRadians);
	/* prevent cangle out of range due to roundoff */
	cangle = max(-1.0,min(cangle,1.0));
	Double angle = acos(cangle);

	/* various luminance factors */
	Double z1 = 0.856 + 16.0 * exp(-3.0 * angle) + 0.3 * cangle * cangle;
	Double z2 = 1.0 - exp(-0.32 / dSinSkyAlt);
	Double z3 = 0.27385 * (0.856 + 16. * exp(-3. * (1.5708 - dSunAlt)) + 0.3 * dSinSunAlt * dSinSunAlt);

	/* luminance of sky element */
	// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
	Double CIEClearTurbidSkyLuminance = 92.9 * lp.dZenithLum * z1 * z2 / z3;

//	return CIEClearTurbidSkyLuminance;
	return max(0.,CIEClearTurbidSkyLuminance);
}

Double	CIEIntermediateSkyLum(LumParam lp, BGL::vector3 Direction)
{
    // Calculates luminance (CD/FT**2) of CIE Intermediate skies in the given Direction.

    // Conventions:
        // Altitudes are 0 at horizon and 90 at zenith
        // Azimuths are 0 East and counter-clockwise is positive
        // All angles are in radians

    // Sun altitude (radians) for this intermediate sky sun position
    Double dSunAlt = lp.dSunAltRadians;
    Double dSinSunAlt = sin(dSunAlt);

    // Sine of sky patch altitude
    Double dSinSkyAlt = Direction[2];
    // Sky patch altitude
    Double dSkyAlt = asin(dSinSkyAlt);

    // Sky patch azimuth
    Double dSkyAzm;
    if ((Direction[0]==0.0) && (Direction[1]==0.0)) dSkyAzm = 0.;
    else dSkyAzm = atan2(Direction[1], Direction[0]);

	// angle between sun and element of sky
	Double cangle = dSinSkyAlt * dSinSunAlt + cos(dSkyAlt) * cos(dSunAlt) * cos(dSkyAzm - lp.dSunAzmRadians);
	/* prevent cangle out of range due to roundoff */
	cangle = max(-1.0,min(cangle,1.0));
	Double angle = acos(cangle);

	/* various luminance factors */
	Double z1 = (1.35 * (sin(3.59 * dSkyAlt - 0.009) + 2.31) * sin(2.6 * dSunAlt + 0.316) + dSkyAlt + 4.799) / 2.326;
	Double z2 = exp(-angle * 0.563 * ((dSunAlt - 0.008) * (dSkyAlt + 1.059) + 0.812));
	Double z3 = 0.99224 * sin(2.6 * dSunAlt + 0.316) + 2.73852;
	Double z4 = exp(-(1.5708 - dSunAlt) * 0.563 * ((dSunAlt - 0.008) * 2.6298 + 0.812));

	/* luminance of sky element */
	// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
	Double CIEIntermediateSkyLuminance = 92.9 * lp.dZenithLum * z1 * z2 / (z3 * z4);

//	return CIEIntermediateSkyLuminance;
	return max(0.,CIEIntermediateSkyLuminance);
}

Double	CIEClearSunLum(LumParam lp, BGL::vector3 Direction)
{
    // Calculates and returns direct normal solar intensity
    // (lumens/ft2) for CIE Standard Clear Sky.
    // For a Direction that points at the sun

    // Check to see if the current Direction points at the sun
    // Direction altitude angle in radians
    Double dDirectionAlt = asin(Direction[2]);
    // Direction azimuth angle in radians
    // Sky patch azimuth
    Double dDirectionAzm;
    if ((Direction[0]==0.0) && (Direction[1]==0.0)) dDirectionAzm = 0.;
    else dDirectionAzm = atan2(Direction[1], Direction[0]);
    // Return 0.0 if the current Direction does not point at the sun
    // or at least within +/- the approximate diameter of the solar disk
    // solar disk = sun dia / sun dist = 9.291*10^-3 radians = 0.5323 deg
// RJH debug
//    if ((dDirectionAlt >= (lp.dSunAltRadians + 5*0.009291)) || (dDirectionAlt <= (lp.dSunAltRadians - 5*0.009291)))
    if ((dDirectionAlt >= (lp.dSunAltRadians + 7*0.009291)) || (dDirectionAlt <= (lp.dSunAltRadians - 7*0.009291)))
        return 0.0;
    if ((dDirectionAzm >= (lp.dSunAzmRadians + 5*0.009291)) || (dDirectionAzm <= (lp.dSunAzmRadians - 5*0.009291)))
        return 0.0;

    // Conventions:
        // Altitudes are 0 at horizon and 90 at zenith
        // Azimuths are 0 East and counter-clockwise is positive
        // All angles are in radians (except where converted to degrees below)

	Double lop, powlop;	/* exponentiation test and result holders */
	Double am;		/* corrected optical air mass */
	Double c1, c2, c3, s1, s2, s3;
	Double abars;
	Double bc;
	Double efflum;	/* luminous efficacy */

    // Sun altitude (radians) for this clear sky sun position
    Double dSunAlt = lp.dSunAltRadians;
    Double dSinSunAlt = sin(dSunAlt);
    // Sun altitude (degrees)
    Double dSunAltDegrees = RadToDeg(dSunAlt);

	/* optical air mass corrected for building altitude in kilometers */
	lop = dSunAltDegrees + 3.885;
	if (lop < 0.0) {
		return(0.0);
	}
	else powlop = pow(lop,1.253);
	am = (1.0 - 0.1 * lp.dBldgAltitude / 3281.0) / (dSinSunAlt + 0.15 / powlop);

	/* intermediate calculations for monster equation */
	c1 = 2.1099 * cos(dSunAlt);
	c2 = 0.6322 * cos(2.0 * dSunAlt);
	c3 = 0.0252 * cos(3.0 * dSunAlt);
	s1 = 1.0022 * dSinSunAlt;
	s2 = 1.0077 * sin(2.0 * dSunAlt);
	s3 = 0.2606 * sin(3.0 * dSunAlt);

	abars = 1.4899 - c1 + c2 + c3 - s1 + s2 - s3;

	/* lp.dBldgMonthlyAtmosTurb below is BETA in 21d code */
	bc = min(0.2,lp.dBldgMonthlyAtmosTurb);

	/* luminous efficacy */
	/* lp.dBldgMonthlyAtmosMois * 2.54 below is W in 21d code */
	efflum = (99.4+4.7*(lp.dBldgMonthlyAtmosMois*2.54)-52.4*bc)*(1.0-exp((24.0*bc-8.0)*dSunAlt));

	// 93.73 below is extraterrestrial lum eff (lm/w)
    // lp.dMonthlyExtraTerrIllum is extraterrestrial illum for 1st of month
    // lp.dTurbidityFactor is turbidity factor calculated with zenith luminance
	Double CIEClearSunLuminance = efflum * (lp.dMonthlyExtraTerrIllum / 93.73) * exp(-am * lp.dTurbidityFactor * abars);

	return CIEClearSunLuminance;
}

//	generate sky
//HemiSphiral	GenSky(int Nsize, LumParam lp)
HemiSphiral	GenSky(LumParam lp)
{
//	HemiSphiral	sky0(Nsize);
	HemiSphiral	sky0(lp.btdfHSResOut);
	if (lp.source == "GEN") {
		if (lp.type == "CIECLEARSUN") {
//			lp.Dump();
			HemiSphiral	sky1;
			sky1 = GenLuminanceMap(lp); //	original RJH CIECLEARSKY
//			sky1.plotview(60);
//			sky1.plotview(60,DegToRad(0),DegToRad(90),DegToRad(0));
			//	create fuzzed out sky using GAUSS and appropriate params
			lp.type = "GAUSS";
			lp.dispersion = 3.25;
			lp.phi0 = lp.dSunAzmRadians;
			lp.theta0 = PI/2 - lp.dSunAltRadians;
			lp.Dir0 = AnglesToDir3D(lp.phi0, lp.theta0);
            if (sky0.omega == 0.0) {
		        writewndo("GenSky: Divide by Zero trapped.\n","e");
            }
			lp.BFlux0 = sky1.TotIllum() / (sky0.omega);
//			lp.Dump();
			sky0 = GenLuminanceMap(lp); //	fuzzed out WLC CIECLEARSKY
//			sky0.plotview(60);
//			sky0.plotview(60,DegToRad(0),DegToRad(90),DegToRad(0));
		}
//		else sky0 = GenLuminanceMap(Nsize, lp);
		else sky0 = GenLuminanceMap(lp);
		//	uniform ground values replace bottom half of Sphiral
		Double	skyTotH = sky0.TotHorizIllum();
//		cout << "GenSky: Tot Horizontal Illum: " << skyTotH << " GndRefl: " << lp.GndRefl << "\n";
		for (int ii=sky0.size()/2+1; ii < sky0.size(); ii++) {
			sky0.valList[ii] = lp.GndRefl*skyTotH*-sky0.costheta(ii);
		}
	}
	else if (lp.source == "FILE") {
		//	open a file for reading ...
		ifstream	infile(lp.filename.c_str());
		if (infile)	sky0.load(infile);
	//	if (!infile) throw (string("Error: Can't open infile: \"") + infilename) + string("\"");
	//	cout << "infile: " << infilename << "\n";
	}
	return sky0;	//	need to test for sky0.size() != 0 for error return
}

//	generate btdf

//btdfHS*	GenBTDF(int Msize, int Nsize, LumParam& lp)
btdfHS*	GenBTDF(LumParam& lp)
{
	//	ONLY WORKS FOR WINDOW, LIGHTSHELF types
	//	directional generator for btdf data:  spiral input dirs are fixed by Msize
	//	make lp temporary copy
	LumParam	lptmp = lp;
	lptmp.type = "GAUSS";	//	hardwire gentype
	lptmp.BFlux0 = 1.00;
	//	hardwire
	BGL::vector3	dirwind, dirLS;
	HemiSphiral		lmTrans(lp.btdfHSResOut), lmRefl(lp.btdfHSResOut), lmTot(lp.btdfHSResOut);
	Double			tau;
	btdfHS*			pbtdf0 = new btdfHS(lp.btdfHSResIn,lp.btdfHSResOut);
	for (int ii=0; ii < lp.btdfHSResIn; ii++) {
		dirwind = pbtdf0->inDir(ii);	//	initial "outward" looking incident dir
		if (dirwind[2] < 0) break;	//	fill only top half of HSin

		//	WINDOW type: ray goes "straight through"
		dirwind *= BGL::vector3(-1,-1,-1);	//	reverse direction
		dirwind *= BGL::vector3(-1,+1,-1);	//	incident ("outside") LCS -> transmitted ("inside") LCS flip
//		dirwind *= BGL::vector3(1,-1,1);	//	combined transformation

		//	VisTrans in incident direction
		tau = lptmp.visTransNormal*pow(dirwind[2],lptmp.visTransExponent);
		pbtdf0->HSin[ii] = tau;	//	store here for later use

		if (lptmp.btdftype == "WINDOW") {
			lptmp.Dir0 = BGL::norm(dirwind);	//	renornalize
			lmTrans = GenLuminanceMap(lptmp);
			pbtdf0->HSoutList[ii]	= (lmTrans*tau)/lmTrans.TotHorizIllum();	//	normalize output for conservation of light
		}
		else if (lptmp.btdftype == "LIGHTSHELF") {
			lptmp.Dir0 = BGL::norm(dirwind);	//	renornalize
			lmTrans = GenLuminanceMap(lptmp);
		//	LIGHTSHELF type: same as WINDOW + one more step to ger reflected ray direction
		//	downward pointing rays are reflected upward: LCS +y -> -y
			dirLS = dirwind;
			if (dirwind[1] < 0) dirLS = dirwind*BGL::vector3(+1,-1,+1);
			lptmp.Dir0 = BGL::norm(dirLS);	//	renornalize
			lmRefl = GenLuminanceMap(lptmp);
			lmTot = lmTrans*(1. - lptmp.LightShelfReflectance) + lmRefl*lptmp.LightShelfReflectance;
			//lmTot.plotview(40);
			pbtdf0->HSoutList[ii]	= (lmTot*tau)/lmTot.TotHorizIllum();	//	normalize output for conservation of light
		}
//		cout << "GenBTDF: btdfHSoutList Tot Horizontal Illum: " << ii << " " << btdf0.HSoutList[ii].TotHorizIllum() << "\n";
	}
	return pbtdf0;
}

//	sky-btdf integration
HemiSphiral	SkyBTDFIntegration(HemiSphiral& sky0, btdf* pbtdf0, BGL::RHCoordSys3 ics)
{
	HemiSphiral	LumMap((int)pbtdf0->HSoutList[0].size());
	HemiSphiral lm((int)pbtdf0->HSoutList[0].size());
	Tregenza	Trgz0;
	//	integrate over btdf "natural" incident directions
	//	sky interpolation needed - even if Nsky = Mbtdf because of possible arbitrary relative orientations
	int	ii, jj;
	Double	skyLum, skyLumTot = 0;
	BGL::vector3	dir, dirsky;
	for (ii=0; ii<pbtdf0->size(); ii++) {	//	incident dir loop
		dir = pbtdf0->inDir(ii);	//	"natural" ii'th btdf incident direction in btdf outside LCS coords
		if (dir[2] < 0) break;	//	only integrate over top half of btdf incident Sphiral
		dirsky = BGL::dirLCStoWCS(dir, ics.RotateY());	//	converted to WCS dir for pointing at sky
		skyLum = sky0.interp(dirsky)*pbtdf0->inDirOmega(ii)*dir[2];
		for (jj=0; jj<pbtdf0->HSoutList[0].size(); jj++) {	//	out Dir loop
//			cout << '.';
			if (pbtdf0->outDir(jj)[2] < 0) break;	//	only integrate over top half of btdf transmitted Sphiral
			lm[jj] = pbtdf0->qexact(ii,jj)*skyLum;	//	uses qexact - NO BTDF interpolation
		}
//		cout << "\n";
		LumMap += lm;
		skyLumTot += skyLum;
//		cout << ii << " ";
//		cout << Trgz0.Theta(ii) << " " << Trgz0.Phi(ii) << " ";
//		cout << pbtdf0->iisym(ii) << " " << pbtdf0->iidata(ii) << " ";
//		cout << sky0.interp(dirsky) << " " << skyLum << " " << lm.TotIllum() << "\n";
//		pbtdf0->HSoutList[pbtdf0->iidata(ii)].plotview(25);
//		if (skyLum/skyTotIllum >= 0.01) {
//			cout << "skyInt: " << skyLum/skyTotIllum << "\n";
//			lm.plotview(25);
//			LumMap.plotview(30);
//		}
	}
//	cout << "skyLumTotRatio: " << skyLumTot/skyTotIllum << "\n";
	return LumMap;
}

/****************************** subroutine POLYF_WLC *****************************/
// PURPOSE OF THIS FUNCTION:
// Evaluates glazing beam transmittance or absorptance of the form
// A(1)*X + A(2)*X^2 + A(3)*X^3 + A(4)*X^4 + A(5)*X^5 + A(6)*X^6
// where X is the cosine of the angle of incidence (0.0 to 1.0)
/****************************************************************************/
/* AUTHOR         Fred Winkelmann */
/* DATE WRITTEN   February 1999 */
/* DATE MODIFIED  October 1999, FW: change to 6th order polynomial over */
/*					entire incidence angle range */
/****************************** subroutine POLYF_WLC *****************************/
double POLYF_WLC(
	double dCosI,		/* cosine of the angle of incidence */
	double EPCoef[6])	/* EnergyPlus coefs of angular transmission */
{
	double transmittance;	// transmittance at angle of incidence

	if(dCosI < 0.0 || dCosI > 1.0)
	  transmittance = 0.0;
	else
	  transmittance = dCosI*(EPCoef[0]+dCosI*(EPCoef[1]+dCosI*(EPCoef[2]+dCosI*(EPCoef[3]+dCosI*(EPCoef[4]+dCosI*EPCoef[5])))));

	return(transmittance);
}

//HemiSphiral	GenWindowMap(int Nsize, LumParam& lp, HemiSphiral& sky0, BGL::RHCoordSys3 ics)
HemiSphiral	GenWindowMap(LumParam& lp, HemiSphiral& sky0, BGL::RHCoordSys3 ics)
{
	HemiSphiral	LumMap(lp.btdfHSResOut);
	Double tau;

	int ii;
	BGL::vector3	dir;
//	int iimax = Nsize;
	int iimax = lp.btdfHSResOut;
	Double z;
	for (ii=0; ii<iimax; ii++) {
		z = LumMap.costheta(ii);
		if (z < 0) break;	//	only calcualte for top half of LumMap transmitted Sphiral
		dir = LumMap.dir(ii);	//	"natural" ii'th LumMap incident direction in inside LCS coords
		dir *= BGL::vector3(-1,-1,-1);	//	reverse direction - that's what a window does
		dir = BGL::dirLCStoWCS(dir, ics);	//	convert to WCS dir for pointing at sky
		if (lp.type == "SINGLEPANE") {
			tau = lp.visTransNormal*pow(z,lp.visTransExponent);
		}
		else if (lp.type == "EPLUS") {
			tau = POLYF_WLC(z,lp.EPlusCoef);
		}
		LumMap[ii] = sky0.interp(dir)*tau;
	}
	return LumMap;
}

//	parameter list string decoder
bool	SecretDecoderRing(LumParam& lp, string InStr)
{
	int	ii;
	vector<string> InStrList = vParseList(InStr);
//	for (ii=0; ii<InStrList.size(); ii++) cout << ii << " " << InStrList[ii] << "\n";

	if (InStrList.size() >= 1) {
		if (IsValidTypeName("OBJECT", InStrList[0])) lp.object = InStrList[0];
		else {
			lp.BadName = InStrList[0];
			return false;
		}
	}
	if (InStrList.size() >= 2) {
		if (IsValidTypeName("SOURCE", InStrList[1])) lp.source = InStrList[1];
		else {
			lp.BadName = InStrList[1];
			return false;
		}
	}
	//	chop off front of InStrList
	int	oldsize = InStrList.size();
	for (ii=2; ii<(int)InStrList.size(); ii++) InStrList[ii-2] = InStrList[ii];
	InStrList.resize(oldsize-2);
//	for (ii=0; ii<InStrList.size(); ii++) cout << ii << " " << InStrList[ii] << "\n";

	if (lp.source == "FILE") {
		if (InStrList.size() >= 1) 	lp.filename = InStrList[0];
		else {
			lp.BadName = "missing FILENAME";
			return false;
		}
	}

	if ( (lp.object == "BTDF") && (lp.source == "GEN") ) {
		if (IsValidTypeName("GENTYPE", InStrList[0])) lp.btdftype = InStrList[0];
		else {
			lp.BadName = InStrList[0];
			return false;
		}
		if (lp.btdftype == "WINDOW") {
			if (InStrList.size() < 3)	{
				lp.BadName = "missing BTDF:WINDOW Parameters";
				return false;
			}
			lp.visTransNormal = atof(InStrList[1].c_str());
			lp.dispersion = atof(InStrList[2].c_str());
			if (InStrList.size() >= 4) 	{
				lp.visTransExponent = atof(InStrList[3].c_str());
			}
			else {
				lp.visTransExponent = 2.00;	//	default value
			}
		}
		else if (lp.btdftype == "LIGHTSHELF") {
			if (InStrList.size() < 3)	{
				lp.BadName = "missing BTDF:LIGHTSHELF Parameters";
				return false;
			}
			lp.visTransNormal = atof(InStrList[1].c_str());
			lp.dispersion = atof(InStrList[2].c_str());
			if (InStrList.size() >= 4) 	{
				lp.LightShelfReflectance = atof(InStrList[3].c_str());
			}
			else {
				lp.LightShelfReflectance = 1.00;	//	default value
			}
			if (InStrList.size() >= 5) 	{
				lp.visTransExponent = atof(InStrList[4].c_str());
			}
			else {
				lp.visTransExponent = 2.00;	//	default value
			}
		}
		return true;
	}

	if ( (lp.object == "WINDOW") && (lp.source == "GEN") ) {
		if (IsValidTypeName("GENTYPE", InStrList[0])) lp.type = InStrList[0];
		else {
			lp.BadName = InStrList[0];
			return false;
		}
		if (lp.type == "SINGLEPANE") {
			if (InStrList.size() >= 3) 	{
				lp.visTransNormal = atof(InStrList[1].c_str());
				lp.visTransExponent = atof(InStrList[2].c_str());
			}
			else {
				lp.BadName = "missing WINDOW Parameters";
				return false;
			}
		}
		else if (lp.type == "EPLUS") {
			if (InStrList.size() >= 8) 	{
				lp.EPlusType = InStrList[1];
				lp.EPlusCoef[0] = atof(InStrList[2].c_str());
				lp.EPlusCoef[1] = atof(InStrList[3].c_str());
				lp.EPlusCoef[2] = atof(InStrList[4].c_str());
				lp.EPlusCoef[3] = atof(InStrList[5].c_str());
				lp.EPlusCoef[4] = atof(InStrList[6].c_str());
				lp.EPlusCoef[5] = atof(InStrList[7].c_str());
			}
			else {
				lp.BadName = "missing WINDOW Parameters";
				return false;
			}
		}
		return true;
	}

	if (lp.source == "GEN") {
		if (InStrList.size() >= 5) 	{
			if (IsValidTypeName("GENTYPE", InStrList[0])) lp.type = InStrList[0];
			else {
				lp.BadName = InStrList[0];
				return false;
			}
            if (lp.type == "CIECLEARSKY") {
                lp.dSunAltRadians = DegToRad(atof(InStrList[1].c_str()));   // Sun Altitude in radians
                lp.dSunAzmRadians = DegToRad(atof(InStrList[2].c_str()));	// Sun Azimuth in radians
                lp.dZenithLum = atof(InStrList[3].c_str()); // Zenith luminance
                lp.GndRefl = atof(InStrList[4].c_str());    // Ground reflectance
            }
            else if (lp.type == "CIECLEARSUN") {
                // "SKY^GEN^CIECLEARSUN^SunAlt^SunAzm^Solic^TFac^AtmMoi^AtmTurb^BldgAlt^GrndRefl"
                lp.dSunAltRadians = DegToRad(atof(InStrList[1].c_str()));   // Sun Altitude in radians
                lp.dSunAzmRadians = DegToRad(atof(InStrList[2].c_str()));	// Sun Azimuth in radians
                lp.dMonthlyExtraTerrIllum = atof(InStrList[3].c_str());     // extraterrestrial illum for 1st of month
                lp.dTurbidityFactor = atof(InStrList[4].c_str()); // turbidity factor calculated with zenith luminance
                lp.dBldgMonthlyAtmosMois = atof(InStrList[5].c_str()); // atmospheric moisture for reference month
                lp.dBldgMonthlyAtmosTurb = atof(InStrList[6].c_str()); // atmospheric turbidity for reference month
                lp.dBldgAltitude = atof(InStrList[7].c_str()); // building altitude
                lp.GndRefl = atof(InStrList[8].c_str());    // Ground reflectance
            }
            else {
			    lp.phi0 = atof(InStrList[1].c_str());	//Phi0:	Beam azimuth output direction, degrees CCW from CFS_x axis
			    lp.theta0 = atof(InStrList[2].c_str());//Theta0: Beam theta output direction, degrees from CFS normal
			    lp.Dir0 = AnglesToDir3D(DegToRad(lp.phi0),DegToRad(lp.theta0));
			    lp.dispersion = atof(InStrList[3].c_str());		// ConeAngle, deg
			    lp.BFlux0 = atof(InStrList[4].c_str());		// BFlux0
            }
		}
        else if (InStrList.size() >= 3) {
	        if (IsValidTypeName("GENTYPE", InStrList[0])) lp.type = InStrList[0];
	        else {
		        lp.BadName = InStrList[0];
		        return false;
	        }
	        lp.dSunAltRadians = DegToRad(atof(InStrList[1].c_str()));	// Sun Altitude in radians
        }
		else {
			lp.BadName = "missing Gen Parameters";
			return false;
		}
		if (lp.object == "SKY") {
			if ((InStrList.size() >= 6) && (lp.type != "CIECLEARSUN") && (lp.type != "CIECLEARSKY")) 	lp.GndRefl = atof(InStrList[5].c_str());
//			if (InStrList.size() >= 6) 	lp.GndRefl = atof(InStrList[5].c_str());
            else if (InStrList.size() >= 3) {
                if (lp.type == "CIEOVERCASTSKY") {
	                lp.GndRefl = atof(InStrList[2].c_str());    // Ground reflectance
                }
            }
			else {
				lp.BadName = "missing SKY GndRefl";
				return false;
			}
		}
		else if (lp.object == "BTDF") {
			if (InStrList.size() >= 6) 	{
				if (IsValidTypeName("BTDFTYPE", InStrList[5])) lp.btdftype = InStrList[5];
				else {
					lp.BadName = InStrList[5];
					return false;
				}
			}
			else {
				lp.BadName = "missing BTDF typename";
				return false;
			}
		}
	}


	//	lp.BadName = InStrList[0];
	return	true;
}



//***************************** subroutine vParseList *****************************
// Parses the given DELIMITER separated string and passes back a vector of the
// parsed substrings
//****************************** subroutine vParseList *****************************
bool	charInList(const char c0, string delimList)
{
	for(int ic=0; ic<(int)delimList.size(); ic++)	{
		if (c0 == delimList[ic]) return 1;
	}
	return 0;
}
vector<string> vParseList(string InStr, string delimList)
{
	string			strTmp;
	vector<string>	strList;
	for (int ii=0; ii<(int)InStr.size(); ii++) {
		if (charInList(InStr[ii], delimList)) {
			strList.push_back(strTmp);
			strTmp.erase();
			continue;
		}
		strTmp += InStr[ii];
	}
	strList.push_back(strTmp);

	return strList;
}

//	FILE_FLG stuff
FILE_FLG::FILE_FLG()
: zero(0), out(0), log(0), dbg(0), err(0), warn(0)
{ }

FILE_FLG::FILE_FLG(string sfpflg)
: zero(0), out(0), log(0), dbg(0), err(0), warn(0)
{
	if (sfpflg.size() == 0) {	//	default
		out = 1;
//		log = 1;
	}
	else for (int isfp=0; isfp<(int)sfpflg.size(); isfp++) {
//		fprintf(ofp,"sfpflg: %d %c\n", isfp, sfpflg[isfp]);
		if (sfpflg[isfp] == '0') zero = 1;
		else if (sfpflg[isfp] == 'o') out = 1;
		else if (sfpflg[isfp] == 'l') log = 1;
		else if (sfpflg[isfp] == 'd') dbg = 1;
		else if (sfpflg[isfp] == 'e') err = 1;
		else if (sfpflg[isfp] == 'w') warn = 1;
	}
}


//	RADdata
int	RADdata::load(string filename)
//	NOTE:  This only works for ndim==2!
{
	std::ostringstream osstream;

	//	open a file for reading ...
	ifstream	infile(filename.c_str());	//	XXXX NOTE:  This crashes if filename = "" !
	if (!infile) {
//		cerr << "Error: RADdata::load: Can't open infile: \"" << filename << "\"\n";
		osstream << "Error: RADdata::load: Can't open infile: \"" << filename << "\"\n";
	    writewndo(osstream.str(),"e");
		return 0;
	}

	infile >> ndim;
	if (ndim != 2) {
//		cerr << "Error: RADdata::load: ndim != 2: " << ndim << "\n";
		osstream << "Error: RADdata::load: ndim != 2: " << ndim << "\n";
	    writewndo(osstream.str(),"e");
		return 0;
	}
	infile >> beg1 >> end1 >> n1;
	infile >> beg2 >> end2 >> n2;
	DataArray.resize(n1);
	int		ndata = 0;
	for (int iin1=0; iin1<n1; iin1++) {
		DataArray[iin1].resize(n2);
		for (int iin2=0; iin2<n2; iin2++) {
			infile >> DataArray[iin1][iin2];
			ndata += 1;
		}
	}
	infile.clear();
	infile.close();
	return ndata;
}

void	RADdata::summary(ostream& outfile)
{
	outfile << ndim << "\n";
	outfile << beg1 << " " << end1 << " " << n1 << "\n";;
	outfile << beg2 << " " << end2 << " " << n2 << "\n" ;
}

void	RADdata::dump(ostream& outfile)
{
	summary(outfile);

	for (int iin1=0; iin1<n1; iin1++) {
		for (int iin2=0; iin2<n2; iin2++) outfile << DataArray[iin1][iin2] << " ";
		outfile << "\n";
	}
}

HemiSphiral	RADdata::convertToHS() {

	//	convert RADsky to Sphiral
//	int				Nsize = 1500;
	int				Nsize = 1*n1*n2;
	//	NOTE: Nsize assumes data is for upper hemisphere only, i.e. end1 = 90
	//	needs to be generalized for other end1 values, particularly end1 = 180
	HemiSphiral		hs0(Nsize);
	hs0.summary();

	int				nnin;
	vector<struct	nearestdata> nd;
	int				nninsizeMax=0, nninsizeMin=0;
	vector<int>		nninsizeDist(25,0);
	double			qdataMax = 0, qdataMin = 0, qdataSum = 0;
	vector<double>	inwgt;
	vector<int>		indxCount(Nsize,0);
	vector<int>		indxCountDist(100,0);
	int				zCount=0;
	double			phiRAD, thetaRAD, qdata;
	double			phiSPH, thetaSPH;
	double			deltathetaRAD = end1/(n1 - 1.);
	double			deltaphiRAD = end2/(n2 - 1.);
//	cout << "deltathetaRAD: " << deltathetaRAD << "; deltaphiRAD: " << deltaphiRAD << "\n";
	BGL::vector3	dir;
	int				iread = 0;
	//	limit logic skips redundant points
	int				limit1 = n1 - 1;		//	thetaRAD
	int				limit2 = n2 - 1;		//	phiRAD
	for (int iin1=0; iin1<=limit1; iin1++) {	//	thetaRAD loop
		if (iin1 == limit1) limit2 = 1;			//	only 1 zenith point
		for (int iin2=0; iin2<limit2; iin2++) {	//	phiRAD loop
			//	read data
			qdata = DataArray[iin1][iin2];
			qdataSum += qdata;
			iread += 1;
			//	convert to Sphiral dir3D
			thetaRAD = beg1 + deltathetaRAD*iin1;
			thetaSPH = 90. - thetaRAD;
			phiRAD = beg2 + deltaphiRAD*iin2;
			phiSPH = -(phiRAD + 90);  //	this is done to make the picture "look right" - why?
//			phiSPH = fmod(phiRAD + 90, 360);
			dir = AnglesToDir3D(DegToRad(phiSPH),DegToRad(thetaSPH));
//			cout << iin1 << " " << iin2 << " " << thetaSPH << " " << phiSPH << " " << dir << " " << qdata << "\n";
			if ( arcdist(dir,BGL::vector3(0,0,1)) < 0.90*hs0.DA ) zCount += 1;
			nnin = hs0.nearestc(0.90*hs0.DA,dir,nd);
			if (iread == 1) {
				nninsizeMax = nninsizeMin = nnin;
				qdataMax = qdataMin = qdata;
			}
			else {
				nninsizeMax = max(nninsizeMax,nnin);
				nninsizeMin = min(nninsizeMin,nnin);
				qdataMax = max(qdataMax,qdata);
				qdataMin = min(qdataMin,qdata);
			}
			nninsizeDist[nnin] += 1;

			//	assign qdata for each direction dir to single nearest sphiral direction
			hs0.valList[nd[0].indx] += qdata;
			indxCount[nd[0].indx] += 1;

//			inwgt = hs0.interpwgts(dir,nd);
//			for (ii=0; ii<inwgt.size(); ii++) {
//				indxCount[nd[ii].indx] += 1;
//				hs0.valList[nd[ii].indx] += qdata*inwgt[ii];
//			}

		}
	}
	int ii;
/*
	cout << "\n";
	cout << "nloopread: " << iread << "\n";
	cout << "qdataMax: " << qdataMax << " qdataMin: " << qdataMin << " qdataSum: " << qdataSum << "\n";

	//	nninsize stats
	cout << "nninsizeDist\n";
	cout << "nninsizeMin: " << nninsizeMin << " " << " nninsizeMax: " << nninsizeMax << "\n";
	for (ii=0; ii<nninsizeDist.size(); ii++) {
		cout << ii << " " << nninsizeDist[ii] << "\n";
	}
	cout << "\n";
	//	indxCount stats
	cout << "indxCountDist\n";
	cout << "zCount: " << zCount << "\n";
*/
	//	compute average for each Hemisphiral dir.
	for (ii=0; ii<Nsize; ii++) if (indxCount[ii]) hs0.valList[ii] /= indxCount[ii];

	return hs0;
}

int	IESNAdata::load(string filename)
{
	std::ostringstream osstream;

	//	open a file for reading ...
	ifstream	infile(filename.c_str());	//	XXXX NOTE:  This crashes if filename = "" !
	if (!infile) {
//		cerr << "Error: Can't open infile: \"" << filename << "\"\n";
		osstream << "Error: Can't open infile: \"" << filename << "\"\n";
	    writewndo(osstream.str(),"e");
		return 0;
	}

	//		read and parse input lines
	string			inlinestr;
	vector<string>	argList;
	while (1) {
		getline(infile,inlinestr);
		headerlineList.push_back(inlinestr);
		argList = vParseList(inlinestr,"=");
		if (argList.size() == 0) continue;
		if (argList[0] == "TILT") {
			if (argList[1] == "INCLUDE") {
				infile >> LampToLumGeom;		//	"Line 06"
				infile >> nPairs;				//	"Line 07"
				angleList.resize(nPairs);
				for (int iang=0; iang<nPairs; iang++) infile >> angleList[iang];	//	"Line 08"
				MultFacList.resize(nPairs);
				for (int imf=0; imf<nPairs; imf++) infile >> MultFacList[imf];		//	"Line 09"
			}
			else if (argList[1] == "NONE") break;
			else {
				break; //	<filename> - not implemented
			}
		}
	}

	infile >> nLamps >> LampLumens >> CandelaMult;	//	"Line 10"
	infile >> nTheta >> nPhi;						//	"Line 10"
	infile >> PhotometricType >> units;				//	"Line 10"
	infile >> LumDimWidth >> LumDimLength >> LumDimHeight;			//	"Line 10"
	infile >> BallastFactor >> BallastLampPhotoFactor >> InputWatts;	//	"Line 11"
	//	theta angles - "Line 12"
	theta.resize(nTheta);
	int iitheta;
	for (iitheta=0; iitheta<nTheta; iitheta++) infile >> theta[iitheta];
	//	phi angles - "Line 13"
	phi.resize(nPhi);
	int iiphi;
	for (iiphi=0; iiphi<nPhi; iiphi++) infile >> phi[iiphi];
	//	set DataArray sizes
	DataArray.resize(nTheta);
	for (iitheta=0; iitheta<nTheta; iitheta++) {
		DataArray[iitheta].resize(nPhi);
	}
	//	load data values - "Line 14++"
	//	NOTE:  DataArray indexes are in REVERSE order than input data load order
	int		ndata = 0;
	for (iiphi=0; iiphi<nPhi; iiphi++) {
		for (iitheta=0; iitheta<nTheta; iitheta++) {
			infile >> DataArray[iitheta][iiphi];
			ndata += 1;
		}
	}
	infile.clear();
	infile.close();
	return ndata;
}

void	IESNAdata::summary(ostream& outfile)
{
	for (int iihdr=0; iihdr<(int)headerlineList.size(); iihdr++) outfile << headerlineList[iihdr] << "\n";
	outfile << nLamps << " " << LampLumens << " " << CandelaMult << " ";	//	"Line 10"
	outfile << nTheta << " " << nPhi << " ";						//	"Line 10"
	outfile << PhotometricType << " " << units << " ";				//	"Line 10"
	outfile << LumDimWidth << " " << LumDimLength << " " << LumDimHeight << "\n";			//	"Line 10"
	outfile << BallastFactor << " " << BallastLampPhotoFactor << " " << InputWatts << "\n";	//	"Line 11"
	//	theta angles - "Line 12"
	for (int iitheta=0; iitheta<nTheta; iitheta++) outfile << theta[iitheta] << " ";
	outfile << "\n";
	//	phi angles - "Line 13"
	for (int iiphi=0; iiphi<nPhi; iiphi++) outfile << phi[iiphi] << " ";
	outfile << "\n";
}

void	IESNAdata::dump(ostream& outfile)
{
	summary(outfile);

	//	dump in phi / theta order to match input file order
	for (int iin1=0; iin1<nPhi; iin1++) {
		for (int iin2=0; iin2<nTheta; iin2++) outfile << DataArray[iin2][iin1] << " ";
		outfile << "\n";
	}
}

HemiSphiral	IESNAdata::convertToHS() {
	//	convert RADsky to Sphiral
//	int				Nsize = 1500;
	int				Nsize = 1*nTheta*nPhi;
	//	NOTE: Nsize assumes data is for upper hemisphere only, i.e. end1 = 90
	//	needs to be generalized for other end1 values, particularly end1 = 180
	HemiSphiral		hs0(Nsize);
	hs0.summary();

	int				nnin;
	vector<struct	nearestdata> nd;
	int				nninsizeMax=0, nninsizeMin=0;
	vector<int>		nninsizeDist(25,0);
	double			qdataMax = 0, qdataMin = 0, qdataSum = 0;
	vector<double>	inwgt;
	vector<int>		indxCount(Nsize,0);
	vector<int>		indxCountDist(100,0);
	int				zCount=0;
	double			qdata;
//	double			phiRAD, thetaRAD;
//	double			phiSPH, thetaSPH;
//	double			deltathetaRAD = theta[theta.size()-1]/(nTheta - 1.);
//	double			deltaphiRAD = phi[phi.size()-1]/(nPhi - 1.);
//	cout << "deltathetaRAD: " << deltathetaRAD << "; deltaphiRAD: " << deltaphiRAD << "\n";
	BGL::vector3	dir;
	int				iread = 0;
	for (int iin1=0; iin1<nTheta; iin1++) {	//	thetaRAD loop
		for (int iin2=0; iin2<nPhi-1; iin2++) {	//	phiRAD loop - read only nPhi-1 points
			if (iin1 == 0 && iin2 > 0) break;	//	read only 1 zenith point
			//	read data
			qdata = DataArray[iin1][iin2];
			qdataSum += qdata;
			iread += 1;
			//	convert to Sphiral dir3D
			dir = AnglesToDir3D(DegToRad(phi[iin2]),DegToRad(theta[iin1]));
//			cout << iin1 << " " << iin2 << " " << theta[iin1] << " " << phi[iin2] << " " << dir << " " << qdata << "\n";
			if ( arcdist(dir,BGL::vector3(0,0,1)) < 0.90*hs0.DA ) zCount += 1;
			nnin = hs0.nearestc(0.90*hs0.DA,dir,nd);
			if (iread == 1) {
				nninsizeMax = nninsizeMin = nnin;
				qdataMax = qdataMin = qdata;
			}
			else {
				nninsizeMax = max(nninsizeMax,nnin);
				nninsizeMin = min(nninsizeMin,nnin);
				qdataMax = max(qdataMax,qdata);
				qdataMin = min(qdataMin,qdata);
			}
			nninsizeDist[nnin] += 1;

			//	assign qdata for each direction dir to single nearest sphiral direction
			hs0.valList[nd[0].indx] += qdata;
			indxCount[nd[0].indx] += 1;

//			inwgt = hs0.interpwgts(dir,nd);
//			for (ii=0; ii<inwgt.size(); ii++) {
//				indxCount[nd[ii].indx] += 1;
//				hs0.valList[nd[ii].indx] += qdata*inwgt[ii];
//			}

		}
	}

//	cout << "\n";
//	cout << "nloopread: " << iread << "\n";
//	cout << "qdataMax: " << qdataMax << " qdataMin: " << qdataMin << " qdataSum: " << qdataSum << "\n";

	//	nninsize stats
//	cout << "nninsizeDist\n";
//	cout << "nninsizeMin: " << nninsizeMin << " " << " nninsizeMax: " << nninsizeMax << "\n";
//	for (int ii=0; ii<nninsizeDist.size(); ii++) {
//		cout << ii << " " << nninsizeDist[ii] << "\n";
//	}
//	cout << "\n";
	//	indxCount stats
//	cout << "indxCountDist\n";
//	cout << "zCount: " << zCount << "\n";

	//	compute average for each Hemisphiral dir.
	for (int ii=0; ii<Nsize; ii++) if (indxCount[ii]) hs0.valList[ii] /= indxCount[ii];

	return hs0;
}

