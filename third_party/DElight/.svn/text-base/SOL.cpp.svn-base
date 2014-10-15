/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 *           Email: RJHitchcock@lbl.gov and WLCarroll@lbl.gov
 */
/**************************************************************
 * C Language Implementation of DOE2.1d and Superlite 3.0
 * Daylighting Algorithms with new Complex Fenestration System
 * analysis algorithms.
 *
 * The original DOE2 daylighting algorithms and implementation
 * in FORTRAN were developed by F.C. Winkelmann at the
 * Lawrence Berkeley National Laboratory.
 *
 * The original Superlite algorithms and implementation in FORTRAN
 * were developed by Michael Modest and Jong-Jin Kim
 * under contract with Lawrence Berkeley National Laboratory.
 **************************************************************/

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

// Standard includes
#include <vector>
#include <map>
#include <fstream>
#include <cstring>
#include <limits>

using namespace std;

// BGL includes
#include "BGL.h"
namespace BGL = BldgGeomLib;

// includes
#include "CONST.H"
#include "DBCONST.H"
#include "DEF.H"

// WLC includes
#include "NodeMesh2.h"
#include "WLCSurface.h"
#include "helpers.h"
#include "hemisphiral.h"
#include "btdf.h"
#include "CFSSystem.h"
#include "CFSSurface.h"

// includes
#include "DOE2DL.H"
#include "SOL.H"
#include "geom.h"
#include "WxTMY2.h"

/****************************** subroutine dzenlm *****************************/
/* Calculates and returns clear sky zenith luminance (KCD/M**2) according to Liebelt. */
/* Liebelt expression is valid only for sun alt < 60 deg, so adjust as below. */
/* Calculates and returns turbidity factor according to Dogniaux. */
/* 12/17/98 - Note that atmospheric moisture and turbidity are both still used */
/* here even after mods to add Perez model to hourly calcs. */
/* Atmospheric moisture	is hardwired to the 0.7 DOE2 value hardwired in READSF. */
/* Atmospheric turbidity can be user input, but defaults to 0.12 DOE2 default. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dzenlm *****************************/
int	dzenlm(
	double *zenl,			/* clear sky zenith luminance */
	double *tfac,			/* turbidity factor */
	int mon,				/* month of calculation (0 to 11) */
	BLDG *bldg_ptr,			/* pointer to bldg structure (atmos turb & mois arrays )*/
	double phsun)			/* sun altitude (radians) */
{
	double sunaltd;	/* sun altitude (degrees) NOTE: var H in 21d code */
	double beta;
	double w;
	double tfacl;
	double phst;

	/* Set sun altitude in degrees. */
	sunaltd = phsun / DTOR;

	/* Set turbidity coef and atmos moisture for specified month. */
	beta = bldg_ptr->atmtur[mon];
	w = bldg_ptr->atmmoi[mon] * 2.54;

	*tfac = (sunaltd + 85.)/(39.5*exp(-w)+47.4)+0.1+(16.0+0.22*w)*beta;
	phst = min(phsun,1.0472);
	/* restrict to 3<TFAC<7.5 since Liebelt zenl invalid outside this range */
	tfacl = min(7.5,max(3.0,*tfac));
	*tfac = min(*tfac,7.5);
	*zenl = (1.34*tfacl-3.46)*tan(phst)+0.1*tfacl+0.9;
	/* If sun altitude is above 60 degrees require horiz illum proportional to */
	/* sin (solar altitude). */
	/* 3.7528 below is 3.25/sin(60), sunaltd below is H in 21d code */
	if (sunaltd > 60.0)
		*zenl = 3.7528*sin(phsun)*(*zenl)/(3.25+(sunaltd-60.0)*(-0.105+0.001*(sunaltd-60.0)));

	return(0);
}

/****************************** function dskylu ******************************/
/* Calculates luminance (CD/FT**2) of CIE Standard clear or overcast skies. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** function dskylu ******************************/
double dskylu(
	int skytype,		/* sky type (0=clear, 1=overcast) */
	double thsky,		/* azimuth of sky element (radians) */
	double phsky,		/* altitude of sky element (radians) */
	double thsun,		/* azimuth of sun (radians) */
	double phsun,		/* altitude of sun (radians) */
	double zenl)			/* clear sky zenith luminance */
{
	double sphsky, sphsun;
	double cangle, angle;
	double z1, z2, z3, z4;
	double skylum;

	sphsky = sin(phsky);
	if (sphsky <= 0.0) sphsky = 0.01;
	sphsun = sin(phsun);

	if (skytype == 0) {	/* clear sky */
		/* angle between sun and element of sky */
		cangle = sphsky * sphsun + cos(phsky) * cos(phsun) * cos(thsky - thsun);
		/* prevent cangle out of range due to roundoff */
		cangle = max(-1.0,min(cangle,1.0));
		angle = acos(cangle);

		/* various luminance factors */
		z1 = 0.91 + 10.0 * exp(-3.0 * angle) + 0.45 * cangle * cangle;
		z2 = 1.0 - exp(-0.32 / sphsky);
		z3 = 0.27385 * (0.91 + 10.0 * exp(-3.0 * (1.5708 - phsun)) + 0.45 * sphsun * sphsun);

		/* luminance of sky element */
		// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
		skylum = 92.9 * zenl * z1 * z2 / z3;

		return(skylum);
	}
	else if (skytype == 1) {	/* overcast sky */
		// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
		skylum = 92.9 * (0.123 + 8.6 * sphsun) * (0.33333 + 0.66667 * sphsky);

		return(skylum);
	}
	else if (skytype == 2) {	/* Clear turbid sky */
		/* angle between sun and element of sky */
		cangle = sphsky * sphsun + cos(phsky) * cos(phsun) * cos(thsky - thsun);
		/* prevent cangle out of range due to roundoff */
		cangle = max(-1.0,min(cangle,1.0));
		angle = acos(cangle);

		/* various luminance factors */
		z1 = 0.856 + 16.0 * exp(-3.0 * angle) + 0.3 * cangle * cangle;
		z2 = 1. - exp(-0.32 / sphsky);
		z3 = 0.27385 * (0.856 + 16. * exp(-3. * (1.5708 - phsun)) + 0.3 * sphsun * sphsun);

		/* luminance of sky element */
		// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
		skylum = 92.9 * zenl * z1 * z2 / z3;

		return(skylum);
	}
	else if (skytype == 3) {	/* Intermediate sky */
		/* angle between sun and element of sky */
		cangle = sphsky * sphsun + cos(phsky) * cos(phsun) * cos(thsky - thsun);
		/* prevent cangle out of range due to roundoff */
		cangle = max(-1.0,min(cangle,1.0));
		angle = acos(cangle);

		/* various luminance factors */
		z1 = (1.35 * (sin(3.59 * phsky - 0.009) + 2.31) * sin(2.6 * phsun + 0.316) + phsky + 4.799) / 2.326;
		z2 = exp(-angle * 0.563 * ((phsun - 0.008) * (phsky + 1.059) + 0.812));
		z3 = 0.99224 * sin(2.6 * phsun + 0.316) + 2.73852;
		z4 = exp(-(1.5708 - phsun) * 0.563 * ((phsun - 0.008) * 2.6298 + 0.812));

		/* luminance of sky element */
		// 92.9 is conversion for zenith luminance from KCD/m2 to CD/ft2
		skylum = 92.9 * zenl * z1 * z2 / (z3 * z4);

		return(skylum);
	}
	else return(-1.0);
}

/****************************** subroutine dsolic *****************************/
/* Calculates and returns extraterrestrial direct normal solar illuminance */
/* (lumens/ft2) for 1st of each month (0 to 11). */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dsolic *****************************/
int dsolic(
	double solic[MONTHS]) /* extraterrestrial illum for 1st of each month (0 to 11) */
{
	int imon;
	double omj;
	double c1, c2, c3, s1, s2, s3;

	for (imon=0; imon<MONTHS; imon++) {
		omj = (2.0 * PI / 366.0) * (1.0 + (double)imon * 30.5);
		/* intermediate calculations for monster equation */
		c1 = 4.248 * cos(omj);
		c2 = 0.0825 * cos(2.0 * omj);
		c3 = 0.00043 * cos(3.0 * omj);
		s1 = 0.1691 * sin(omj);
		s2 = 0.00914 * sin(2.0 * omj);
		s3 = 0.01726 * sin(3.0 * omj);

		/* 92.9 below is conversion from KLX to lumens/ft2 */
		solic[imon] = (126.82 + c1 + c2 - c3 + s1 + s2 + s3) * 92.9;
	}

	return(0);
}

/****************************** function dnsol *****************************/
/* Calculates and returns direct normal solar intensity */
/* (lumens/ft2) for CIE Standard Clear Sky. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** function dnsol *****************************/
double dnsol(
	double solic[MONTHS],/* extraterrestrial illum for 1st of every month */
	BLDG *bldg_ptr,		/* bldg pointer */
	int mon,			/* month of calc */
	double phsun,		/* sun altitude (rad) */
	double tfac,			/* turbidity factor calculated by dzenlm */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	double dnsolum;	/* returned value */
	double lop, powlop;	/* exponentiation test and result holders */
	double am;		/* corrected optical air mass */
	double phsun_deg;/* altitude of sun (degrees) */
	double sphsun;	/* sine of altitude of sun */
	double c1, c2, c3, s1, s2, s3;
	double abars;
	double bc;
	double efflum;	/* luminous efficacy */

	phsun_deg = phsun / DTOR;
	sphsun = sin(phsun);

	/* optical air mass corrected for building altitude in kilometers */
	lop = phsun_deg + 3.885;
	if (lop < 0.0) {
		*pofdmpfile << "ERROR: DElight Invalid sun altitude (" << phsun_deg << " passed to dnsol()\n";
		return(-1.0);
	}
	else powlop = pow(lop,1.253);
	am = (1.0 - 0.1 * bldg_ptr->alt / 3281.0) / (sphsun + 0.15 / powlop);

	/* intermediate calculations for monster equation */
	c1 = 2.1099 * cos(phsun);
	c2 = 0.6322 * cos(2.0 * phsun);
	c3 = 0.0252 * cos(3.0 * phsun);
	s1 = 1.0022 * sphsun;
	s2 = 1.0077 * sin(2.0 * phsun);
	s3 = 0.2606 * sin(3.0 * phsun);

	abars = 1.4899 - c1 + c2 + c3 - s1 + s2 - s3;

	/* bldg_ptr->atmtur[mon] below is BETA in 21d code */
	bc = min(0.2,bldg_ptr->atmtur[mon]);

	/* luminous efficacy */
	/* bldg_ptr->atmmoi[mon] * 2.54 below is W in 21d code */
	efflum = (99.4+4.7*(bldg_ptr->atmmoi[mon]*2.54)-52.4*bc)*(1.0-exp((24.0*bc-8.0)*phsun));

	/* 93.73 below is extraterrestrial lum eff (lm/w) */
	dnsolum = efflum * (solic[mon] / 93.73) * exp(-am * tfac * abars);

	return(dnsolum);
}

/****************************** subroutine dhill *****************************/
/* Calculates illuminance (lumens/ft2) on unobstructed horizontal surface */
/* for CIE Clear and Overcast skies. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dhill *****************************/
int dhill(
	double *hlskyc_ptr,	/* horiz illum sky component (clear sky) for given sun alt position */
	double *hlsunc_ptr,	/* horiz illum sun component (clear sky) for given sun alt position */
	double *hlskyo_ptr,	/* horiz illum sky component (overcast sky) for given sun alt position */
	BLDG *bldg_ptr,	/* bldg structure pointer */
	int mon,		/* month of calculation */
	double phsun,	/* sun altitude (rad) */
	double thsun,	/* sun azimuth (rad) */
	double zenl,		/* zenith luminance for this month */
	double tfac,		/* turbidity factor for this month */
	double solic[MONTHS],	/* extraterrestrial illum for each month */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	double dth, dph;	/* sky integration step size in azimuth and altitude */
	double zlum;		/* sky integration accumulator*/
	int iph, ith;	/* sky integration index vars */
	double phsky, thsky;	/* sky element altitude and azimuth (radians) */
	double sphcph;	/* sin(phsky) * cos(phsky) */
	double dnsolum;	/* dnsol test holder */


	/* Integrate to obtain illuminance from clear sky. */
	/* The contribution in lumens/ft2 from a patch of sky at */
	/* altitude ph and azimuth th is lum[th,ph]*sin[ph]*cos[ph]*dth*dph, */
	/* where lum[th,ph] is the luminance of the patch in cd/ft2 */

	/* sky integration step size in azimuth and altitude */
	dth = 2.0 * PI / (double)NTH;
	dph = PI / (2.0 * (double)NPH);
	zlum = 0.0;

	/* sky integration */
	for (iph=1; iph<=NPH; iph++) {
		phsky = ((double)iph - 0.5) * dph;
		sphcph = sin(phsky) * cos(phsky);
		for (ith=1; ith<=NTH; ith++) {
			thsky = ((double)ith - 0.5) * dth;
			zlum += dskylu(0,thsky,phsky,thsun,phsun,zenl) * sphcph;
		}
	}
	*hlskyc_ptr = zlum * dth * dph;

	/* direct solar illum */
	dnsolum = dnsol(solic,bldg_ptr,mon,phsun,tfac,pofdmpfile);
	if (dnsolum < 0.0) {
		*pofdmpfile << "ERROR: DElight Bad return from dnsol() = " << dnsolum << " return from dhill()\n";
		return(-1);
	}
	*hlsunc_ptr = sin(phsun) * dnsolum;

	/* Illuminance from overcast sky. There is no direct component. */
	/* The integration over the sky can be done in closed form, */
	/* giving (7.*PI/9.)(zenith luminance). */
	*hlskyo_ptr = 2.44346 * dskylu(1,0.0,PIOVR2,thsun,phsun,zenl);

	return(0);
}

/****************************** subroutine dshdlu *****************************/
/* Calculates luminance (cd/ft2) of building-shades, and exterior surfaces */
/* for CIE Clear and Overcast skies, for solar altitude (phsun) and azimuth (thsun). */
/* Building-shades and exterior surfaces are assumed to be unobstructed */
/* for this calculation. */
/* Only the side of the shade or surface from which the surface outward normal */
/* projects is considered to have luminance. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dshdlu *****************************/
int dshdlu(
	BLDG *bldg_ptr,			/* pointer to bldg structure */
	double phsun,			/* sun position altitude */
	double thsun,			/* sun position azimuth */
	int iphs,				/* sun position altitude index */
	int iths,				/* sun position azimuth index */
	double solic[MONTHS],	/* extraterrestrial illum for 1st of each month */
	double tfac,				/* turbidity factor calculated by dzenlm() */
	double zenl,				/* zenith luminance calculated by dzenlm() */
	double hlskyc,			/* horiz lum from clear sky calculated by dhill() */
	double hlskyo,			/* horiz lum from overcast sky calculated by dhill() */
	double hlsunc,			/* horiz lum from clear sun calculated by dhill() */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	int ish, icoord, iph, ith, izone, isurf;	/* indexes */
	double phshn, thshn;			/* temp shade angle vars */
	double sphshn, cphshn;		/* temp shade angle trig vars */
	double phsun_deg;			/* sun alt (deg) */
	double sphsun, cphsun;		/* temp sun angle trig vars */
	double dnsolum;				/* dnsol() test holder */
	double w21[NCOORDS], w23[NCOORDS], wsnorm[NCOORDS];	/* temp verts and vects */
	double dwnorm;	/* temp var */
	double z1sun, z1sky, z2sky;	/* temp vars clear sun & sky and overcast sky */
	double dasky;
	double cosbsh;				/* cos of angle of incidence */
	double ph, dph, sph, cph;
	double th, dth;
	double thmin, thmax; 		/* limits of integration of sky azimuth */

	/* set sun angle vars */
	phsun_deg = phsun / DTOR;
	sphsun = sin(phsun);
	cphsun = cos(phsun);

	/* loop over building-shades */
	for (ish=0; ish<bldg_ptr->nbshades; ish++) {
		/* init clear and overcast sky condition shade luminance to 0.0 */
		bldg_ptr->bshade[ish]->skylum[iphs][iths] = 0.;
		bldg_ptr->bshade[ish]->sunlum[iphs][iths] = 0.;
		/* init overcast sky luminance for a single sun position */
		if ((iphs == 0) && (iths == 0)) bldg_ptr->bshade[ish]->ovrlum = 0.;

		/* skip for shade with vis_refl of 0.0 */
		if (bldg_ptr->bshade[ish]->vis_refl == 0.0) continue;

		/* find azm and tilt of shade (in bldg_sys) from vertices 1, 2 and 3 */
		for (icoord=0; icoord<NCOORDS; icoord++) {
			w21[icoord] = bldg_ptr->bshade[ish]->vert[icoord][0] - bldg_ptr->bshade[ish]->vert[icoord][1];
			w23[icoord] = bldg_ptr->bshade[ish]->vert[icoord][2] - bldg_ptr->bshade[ish]->vert[icoord][1];
		}
		/* unit vector normal to shade */
		dcross(w23,w21,wsnorm);
		dwnorm = sqrt(ddot(wsnorm,wsnorm));
		for (icoord=0; icoord<NCOORDS; icoord++) {
			wsnorm[icoord] /= dwnorm;
		}
		/* alt and azm of shade normal */
		phshn = asin(wsnorm[2]);
		thshn = 0.;
		if ((wsnorm[0] == 0.0) && (wsnorm[1] == 0.0)) thshn = 0.;
		else thshn = atan2(wsnorm[1],wsnorm[0]);
		sphshn = wsnorm[2];
		cphshn = cos(phshn);

		/* direct illuminance from sun */
		z1sun = 0.;
		/* cos of angle of incidence */
		cosbsh = sphsun * sphshn + cphsun * cphshn * cos(thsun - thshn);
		/* only add direct contribution if sunlight falls on front of shade */
		if (cosbsh >= 0.0) {
			dnsolum = dnsol(solic,bldg_ptr,IMREF,phsun,tfac,pofdmpfile);
			if (dnsolum < 0.0) {
				*pofdmpfile << "ERROR: DElight Bad return from dnsol() = " << dnsolum << " exit dshdlu()\n";
				return(-1);
			}
			z1sun = dnsolum * cosbsh;
		}

		/* integrate to get contribution of diffuse light from sky */
		z1sky = 0.;
		z2sky = 0.;
		dph = PIOVR2 / (double)NPH;
		for (iph=0; iph<NPH; iph++) {
			ph = ((double)(iph+1) - 0.5) * dph;
			if (ph >= (phsun_deg + PIOVR2)) continue;
			sph = sin(ph);
			cph = cos(ph);
			/* find limits of integration of sky azimuth */
			dthlim(&thmin,&thmax,ph,thshn,phshn);
			dth = (thmax - thmin) / (double)NTH;
			dasky = cph * dth * dph;
			for (ith=0; ith<NTH; ith++) {
				th = thmin + ((double)(ith+1) - 0.5) * dth;
				/* cos of angle of incidence */
				cosbsh = sph * sphshn + cph * cphshn * cos(th - thshn);
				if (cosbsh < 0.0) continue;
				z1sky += dskylu(0,th,ph,thsun,phsun,zenl) * cosbsh * dasky;
				/* overcast sky luminance for a single sun position */
				if ((iphs == 0) && (iths == 0))
					z2sky += dskylu(1,th,ph,thsun,phsun,zenl) * cosbsh * dasky;
			}
		}

		/* add diffuse illuminance from ground */
		z1sky += hlskyc * bldg_ptr->bshade[ish]->gnd_refl * 0.5 * (1.0 - sphshn);
		z1sun += hlsunc * bldg_ptr->bshade[ish]->gnd_refl * 0.5 * (1.0 - sphshn);
		/* calculate overcast sky luminance for a single sun position */
		if ((iphs == 0) && (iths == 0))
			z2sky += hlskyo * bldg_ptr->bshade[ish]->gnd_refl * 0.5 * (1.0 - sphshn);

		/* overall luminance of shading surface (cd/ft2) */
		bldg_ptr->bshade[ish]->skylum[iphs][iths] = z1sky * bldg_ptr->bshade[ish]->vis_refl / PI;
		bldg_ptr->bshade[ish]->sunlum[iphs][iths] = z1sun * bldg_ptr->bshade[ish]->vis_refl / PI;
		/* calculate overcast sky luminance for a single sun position */
		if ((iphs == 0) && (iths == 0))
			bldg_ptr->bshade[ish]->ovrlum = z2sky * bldg_ptr->bshade[ish]->vis_refl / PI;
	}

	/* loop over zone surfaces */
	for (izone=0; izone<bldg_ptr->nzones; izone++) {
		for (isurf=0; isurf<bldg_ptr->zone[izone]->nsurfs; isurf++) {
			/* init clear and overcast sky condition surface luminance to 0.0 */
			bldg_ptr->zone[izone]->surf[isurf]->skylum[iphs][iths] = 0.;
			bldg_ptr->zone[izone]->surf[isurf]->sunlum[iphs][iths] = 0.;
			/* init overcast sky luminance for a single sun position */
			if ((iphs == 0) && (iths == 0)) bldg_ptr->zone[izone]->surf[isurf]->ovrlum = 0.;

			/* skip for surface with ext_vis_refl of 0.0 */
			if (bldg_ptr->zone[izone]->surf[isurf]->ext_vis_refl == 0.0) continue;

			/* find azm and tilt of surface (in bldg_sys) from vertices 1, 2 and 3 */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				w21[icoord] = bldg_ptr->zone[izone]->surf[isurf]->vert[icoord][0] - bldg_ptr->zone[izone]->surf[isurf]->vert[icoord][1];
				w23[icoord] = bldg_ptr->zone[izone]->surf[isurf]->vert[icoord][2] - bldg_ptr->zone[izone]->surf[isurf]->vert[icoord][1];
			}
			/* unit vector normal to shade */
			dcross(w23,w21,wsnorm);
			dwnorm = sqrt(ddot(wsnorm,wsnorm));
			for (icoord=0; icoord<NCOORDS; icoord++) {
				wsnorm[icoord] /= dwnorm;
			}
			/* alt and azm of shade normal */
			phshn = asin(wsnorm[2]);
			thshn = 0.;
			if ((wsnorm[0] == 0.0) && (wsnorm[1] == 0.0)) thshn = 0.;
			else thshn = atan2(wsnorm[1],wsnorm[0]);
			sphshn = wsnorm[2];
			cphshn = cos(phshn);

			/* direct illuminance from sun */
			z1sun = 0.;
			/* cos of angle of incidence */
			cosbsh = sphsun * sphshn + cphsun * cphshn * cos(thsun - thshn);
			/* only add direct contribution if sunlight falls on front of shade */
			if (cosbsh >= 0.0) {
				dnsolum = dnsol(solic,bldg_ptr,IMREF,phsun,tfac,pofdmpfile);
				if (dnsolum < 0.0) {
					*pofdmpfile << "ERROR: DElight Bad return from dnsol() = " << dnsolum << " exit dshdlu()\n";
					return(-1);
				}
				z1sun = dnsolum * cosbsh;
			}

			/* integrate to get contribution of diffuse light from sky */
			z1sky = 0.;
			z2sky = 0.;
			dph = PIOVR2 / (double)NPH;
			for (iph=0; iph<NPH; iph++) {
				ph = ((double)(iph+1) - 0.5) * dph;
				if (ph >= (phsun_deg + PIOVR2)) continue;
				sph = sin(ph);
				cph = cos(ph);
				/* find limits of integration of sky azimuth */
				dthlim(&thmin,&thmax,ph,thshn,phshn);
				dth = (thmax - thmin) / (double)NTH;
				dasky = cph * dth * dph;
				for (ith=0; ith<NTH; ith++) {
					th = thmin + ((double)(ith+1) - 0.5) * dth;
					/* cos of angle of incidence */
					cosbsh = sph * sphshn + cph * cphshn * cos(th - thshn);
					if (cosbsh < 0.0) continue;
					z1sky += dskylu(0,th,ph,thsun,phsun,zenl) * cosbsh * dasky;
					/* overcast sky luminance for a single sun position */
					if ((iphs == 0) && (iths == 0))
						z2sky += dskylu(1,th,ph,thsun,phsun,zenl) * cosbsh * dasky;
				}
			}

			/* add diffuse illuminance from ground */
			z1sky += hlskyc * bldg_ptr->zone[izone]->surf[isurf]->gnd_refl * 0.5 * (1.0 - sphshn);
			z1sun += hlsunc * bldg_ptr->zone[izone]->surf[isurf]->gnd_refl * 0.5 * (1.0 - sphshn);
			/* calculate overcast sky luminance for a single sun position */
			if ((iphs == 0) && (iths == 0))
				z2sky += hlskyo * bldg_ptr->zone[izone]->surf[isurf]->gnd_refl * 0.5 * (1.0 - sphshn);

			/* overall luminance of zone surface (cd/ft2) */
			bldg_ptr->zone[izone]->surf[isurf]->skylum[iphs][iths] = z1sky * bldg_ptr->zone[izone]->surf[isurf]->ext_vis_refl / PI;
			bldg_ptr->zone[izone]->surf[isurf]->sunlum[iphs][iths] = z1sun * bldg_ptr->zone[izone]->surf[isurf]->ext_vis_refl / PI;
			/* calculate overcast sky luminance for a single sun position */
			if ((iphs == 0) && (iths == 0))
				bldg_ptr->zone[izone]->surf[isurf]->ovrlum = z2sky * bldg_ptr->zone[izone]->surf[isurf]->ext_vis_refl / PI;
		}
	}

	return(0);
}

// turn off Improve Float Consistency optimization
#pragma optimize( "", off )

/****************************** subroutine sun1 *****************************/
/* Calculates daily solar angles. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine sun1 *****************************/
int sun1(
	int dayofyr,			/* sequential day of year */
	SUN1_DATA *sun1_ptr,	/* pointer to sun1_data structure */
	BLDG *bldg_ptr)			/* pointer to bldg structure */
{
	double c1, c2, c3, s1, s2, s3;	/* intermediate vars */
	double arg1, arg2, arg3, arg4, arg5, arg6;	/* intermediate vars */

	/* Get sin,cos of day of year/365 */
	c1 = cos(0.01721 * (double)dayofyr);
	s1 = sin(0.01721 * (double)dayofyr);
	s2 = 2.0 * s1 * c1;
	c2 = c1 * c1 - s1 * s1;
	c3 = c1 * c2 - s1 * s2;
	s3 = c1 * s2 + s1 * c2;

	/* Calc tangent of declination angle */
	sun1_ptr->tan_decl = 0.00527 - 0.4001 * c1 - 0.003996 * c2 - 0.00424 * c3 + 0.0672 * s1;

	/* Calc equation of time */
	arg1 = 0.00706 * c1;
	arg2 = 0.0533 * c2;
	arg3 = 0.00157 * c3;
	arg4 = 0.122 * s1;
	arg5 = 0.156 * s2;
	arg6 = 0.00556 * s3;

	sun1_ptr->eqtime = 0.0000696 + arg1 - arg2 - arg3 - arg4 - arg5 - arg6;

	/* Calc hour angle of sunrise */
	sun1_ptr->gundog = acos(-(tan(bldg_ptr->lat*DTOR)) * sun1_ptr->tan_decl);

	/* Calc angle of declination */
	sun1_ptr->decl = atan(sun1_ptr->tan_decl);

	/* Calc sin and cos of angle of declination */
	sun1_ptr->sin_decl = sin(sun1_ptr->decl);
	sun1_ptr->cos_decl = cos(sun1_ptr->decl);

	return(0);
}
// turn optimizations back on 
#pragma optimize( "", on )

/****************************** subroutine sun2 *****************************/
/* Calculates hourly solar angles. */
/* Reads hourly solar data from weather file if available. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine sun2 *****************************/
int sun2(
	int imon,				/* month of the year */
	int iday,				/* day of the month */
	int ihr,				/* hour of the day */
	SUN1_DATA *sun1_ptr,	/* pointer to sun1_data structure */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2_data structure */
	BLDG *bldg_ptr,			/* pointer to bldg structure */
	int wx_flag,			/* weather availability flag */
	FILE *wxfile_ptr)		/* TMY2 weather file pointer */
{
	double hour_ang;		/* hour angle */
	double test, diff;		/* test vars */
	double cstala, sstala;	/* cos and sin of site latitude */
	double cbazim, sbazim;	/* cos and sin of bldg azimuth */
	double chcd, shcd, clsd, chcdsc;	/* temp vars */

	/* Read weather data if available */
	if (wx_flag) {
		/* if TMY2 wx reader returns error value return from sun2() */
		if (read_wx_tmy2_hr(imon, iday, ihr, sun2_ptr, wxfile_ptr) < 0)
			return (-1);
	} 

	/* Calc hour angle */
	hour_ang = 0.2618 * ((double)(ihr - 11 + bldg_ptr->timezone) + sun1_ptr->eqtime - 0.5) - bldg_ptr->lon * DTOR;

	/* Set test to be the hour angle of the bin edge nearest noon (noon = hr 11) */
	if (ihr < 11) test = hour_ang + 0.1309;
	else test = hour_ang - 0.1309;

	/* Is sun up? */
	if (fabs(test) > fabs(sun1_ptr->gundog)) {
		sun2_ptr->isunup = 0;
		sun2_ptr->raycos[2] = 0;
		sun2_ptr->cldamt = 0;
		sun2_ptr->solrad = 0.;
		sun2_ptr->dirsol = 0.;
		return(0);
	}
	else {
		sun2_ptr->isunup = 1;
	}

	/* Test to see if this hour bin contains sunrise or sunset */
	sun2_ptr->fsunup = 1.;
	diff = fabs(sun1_ptr->gundog) - fabs(test);
	if ((diff >= 0.0) && (diff < 0.2618)) {
		/* Reset the hour angle half way between sunrise or sunset and the bin edge nearest noon */
		if (ihr < 11) hour_ang += 0.5 * (0.2618 - diff);
		else hour_ang -= 0.5 * (0.2618 - diff);
		/* Set fsunup to be the fraction of the hour the sun was up */
		sun2_ptr->fsunup = 3.8197 * diff;
	}

	/* Calc solar direction cosines */
	cstala = cos(bldg_ptr->lat * DTOR);
	sstala = sin(bldg_ptr->lat * DTOR);
	cbazim = cos(bldg_ptr->azm * DTOR);
	sbazim = sin(bldg_ptr->azm * DTOR);
	chcd = cos(hour_ang) * sun1_ptr->cos_decl;
	shcd = sin(hour_ang) * sun1_ptr->cos_decl;
	clsd = sun1_ptr->sin_decl * cstala;
	chcdsc = chcd * sstala - clsd;
	sun2_ptr->raycos[0] = chcdsc * sbazim - shcd * cbazim;
	sun2_ptr->raycos[1] = -chcdsc * cbazim - shcd * sbazim;
	sun2_ptr->raycos[2] = sstala * sun1_ptr->sin_decl + cstala * chcd;
	/* Is sun really up? */
	if (sun2_ptr->raycos[2] < 0.001) {
		sun2_ptr->isunup = 0;
		sun2_ptr->raycos[2] = 0;
		sun2_ptr->cldamt = 0;
		sun2_ptr->solrad = 0.;
		sun2_ptr->dirsol = 0.;
		return(0);
	}

	return(0);
}
