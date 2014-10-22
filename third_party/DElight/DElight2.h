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

#include "DElightAPI.h"

DllExport int DElight2(
	char sWxName[MAX_CHAR_LINE+1],		/* weather file name */
	char sInputName[MAX_CHAR_LINE+1],	/* input file name */
	char sOutputName[MAX_CHAR_LINE+1],	/* output file name */
	char sW4LibName[MAX_CHAR_LINE+1],	/* Window 4 Library file name */
	int iIterations,					/* Number of radiosity iterations */
	double dCloudFraction,				/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	int iSurfNodes,						/* Desired total number of surface nodes */
	int iWndoNodes,						/* Desired total number of window nodes */
	int iNumAlts,						/* Number of preprocessor sun altitude angles */
	double dMinAlt,						/* Minimum preprocessor sun altitude angle */
	int iNumAzms,						/* Number of preprocessor sun azimuth angles */
	double dMinAzm,						/* Minimum preprocessor sun azimuth angle */
	int iStrtMonth,						/* Beginning month of run period */
	int iStrtDay,						/* Beginning day of month of run period */
	int iEndMonth,						/* Ending month of run period */
	int iEndDay,						/* Ending month of run period */
	int iYear);							/* 4 digit year of run period */

DllExport int	DElightDaylightFactors4EPlus(
	char sInputName[MAX_CHAR_LINE+1],	/* input file name */
	char sOutputName[MAX_CHAR_LINE+1],	/* output file name */
	BLDG* bldg_ptr,							/* bldg data structure */
	LIB* lib_ptr,							/* library data structure */
	int iIterations,					/* Number of radiosity iterations */
	double dCloudFraction,				/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	int iSurfNodes,						/* Desired total number of surface nodes */
	int iWndoNodes,						/* Desired total number of window nodes */
	int iNumAlts,						/* Number of daylight factor sun altitude angles */
	double dMinAlt,						/* Minimum daylight factor sun altitude angle */
	int iNumAzms,						/* Number of daylight factor sun azimuth angles */
	double dMinAzm,						/* Minimum daylight factor sun azimuth angle */
    ofstream* pofdmpfile);              // ptr to Error message dump file

DllExport int DElightElecLtgCtrl4EPlus(
	BLDG* bldg_ptr,			/* pointer to DElight Bldg data structure */
	ZONE* zone_ptr,			/* pointer to DElight Zone data structure */
	double dHISKF,			/* Exterior horizontal illuminance from sky (lum/m^2) */
	double dHISUNF,			/* Exterior horizontal beam illuminance (lum/m^2) */
	double dCloudFraction,	/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	double dSOLCOS[3],		/* Direction cosines of current sun position */
	double dMinAlt,			/* Minimum daylight factor sun altitude angle */
	double dMinAzm,			/* Minimum daylight factor sun azimuth angle */
	double dMaxAlt,			/* Maximum daylight factor sun altitude angle */
	double dMaxAzm,			/* Maximum daylight factor sun azimuth angle */
	double dAltInc,			/* Increment of daylight factor sun altitude angles */
	double dAzmInc, 		/* Increment of daylight factor sun azimuth angles */
    ofstream* pofdmpfile);  // ptr to Error message dump file

DllExport int DElightFreeMemory4EPlus(
	BLDG* bldg_ptr,		/* bldg data structure */
	LIB* lib_ptr);		/* library data structure */
