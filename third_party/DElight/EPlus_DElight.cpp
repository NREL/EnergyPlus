/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
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
#include "DElight2.h"
#include "struct.h"
#include "EPlus_Loaddata.h"
#include "EPlus_Geom.h"
#include "DFcalcs.h"
#include "EPlus_ECM.H"
#include "ECM.H"
#include "savedata.h"
#include "TOOLS.H"
#include "WxTMY2.h"
#include "W4Lib.h"

/******************************** subroutine DElightDaylightFactors4EPlus *******************************/
// Called from DElightManagerC.cpp
/* Calls key daylighting simulation modules necessary for calculating a set of daylight factors for EnergyPlus. */
/******************************** subroutine DElightDaylightFactors4EPlus *******************************/
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
    ofstream* pofdmpfile)               // ptr to Error message dump file
{
	FILE *infile;						/* input file pointer */
	FILE *outfile;						/* output file pointer */
	SUN_DATA sun_data;	/* sun data structure */

    // Init return value
    int iReturnVal = 0;

	/* initialize BLDG and LIB structures */
	struct_init("BLDG",(char *)bldg_ptr);
	struct_init("LIB",(char *)lib_ptr);

	// Open the input file for loading
	if((infile = fopen(sInputName, "r" )) == NULL ) {
//	if ((infile=fopen(sInputName,"r")) == NULL) {
		*pofdmpfile << "ERROR: DElight cannot open input file [" <<sInputName << "]\n";
		return(-3);
	}

	// Read the first heading line in the input file to determine input version
	char cInputLine[MAX_CHAR_LINE+1];	/* Input line */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	char cInputVersion[MAX_CHAR_UNAME+1];
	sscanf(cInputLine,"%*s %s\n",cInputVersion); //,_countof(cInputVersion));

	// If the input version is EPlus then use LoadDataFromEPlus()
	if (strcmp(cInputVersion,"EPlus") == 0)
	{
		if (LoadDataFromEPlus(bldg_ptr,infile,pofdmpfile) < 0) {
			*pofdmpfile << "ERROR: DElight Bad Building data read from input file [" <<sInputName << "]\n";
			/* Close input file. */
			fclose(infile);
			return(-4);
		}

		/* load library data from input file */
		if (LoadLibDataFromEPlus(lib_ptr,infile,pofdmpfile) < 0) {
			*pofdmpfile << "ERROR: DElight Bad Library data read from input file [" <<sInputName << "]\n";
			/* Close input file. */
			fclose(infile);
			return(-4);
		}
	}
	else // return error
	{
		*pofdmpfile << "ERROR: DElight Incorrect DElight for EnergyPlus Input Format in input file [" <<sInputName << "]\n";
		/* Close input file. */
		fclose(infile);
		return(-4);
	}

	/* Close input file after successful read. */
	fclose(infile);

	/* Calculate geometrical values required for DF calcs. */
	if (iSurfNodes > MAX_SURF_NODES) iSurfNodes = MAX_SURF_NODES;
	if (iWndoNodes > MAX_WNDO_NODES) iWndoNodes = MAX_WNDO_NODES;
	if (CalcGeomFromEPlus(bldg_ptr) < 0) {
		*pofdmpfile << "ERROR: DElight Bad return from CalcGeomFromEPlus()\n";
		return(-4);
	}

	/* Fill SUN_DATA structure for sun position calculations. */
	if ((iNumAlts == 0) || (iNumAzms == 0)) {
		// Full set of sun positions
		sun_data.nphs = NPHS;	/* number of sun position altitudes */
		sun_data.nths = NTHS;	/* number of sun position azimuths */
		sun_data.phsmin = 10.;	/* minimum sun altitude (degrees) */
		sun_data.thsmin = -110.;	/* minimum sun azimuth (degrees: South=0.0, East=+90.0) */
	}
	else {
		if (iNumAlts > NPHS) iNumAlts = NPHS;
		sun_data.nphs = iNumAlts;	/* number of sun position altitudes */
		if (iNumAzms > NTHS) iNumAzms = NTHS;
		sun_data.nths = iNumAzms;	/* number of sun position azimuths */
		sun_data.phsmin = dMinAlt;	/* minimum sun altitude (degrees) */
		sun_data.thsmin = dMinAzm;	/* minimum sun azimuth (degrees: South=0.0, East=+90.0) */
	}

	/* Calculate daylight illuminances and daylight factors. */
	// NOTE: Use same version of CalcDFs as for standard DElight2 to avoid having to maintain separate versions.
    int iCalcDFsReturnVal = CalcDFs(&sun_data,bldg_ptr,lib_ptr,iIterations,pofdmpfile);
	if (iCalcDFsReturnVal < 0) {
        // If Errors have been detected then return now, else ignore Warnings (return value == -10) until return from DElight
	    if (iCalcDFsReturnVal != -10) {
		    *pofdmpfile << "ERROR: DElight Bad return from CalcDFs()\n";
		    return(-4);
        }
        else {
            iReturnVal = -10;
        }
	}

	/* Open output file. */
	if((outfile = fopen(sOutputName, "w" )) == NULL ) {
//	if ((outfile=fopen(sOutputName,"w")) == NULL) {
		*pofdmpfile << "ERROR: DElight Cannot open output file [" <<sOutputName << "]\n";
		return(-2);
	}

	/* Dump runtime data. */
	fprintf(outfile,"\n");
	fprintf(outfile,"RUNTIME DATA\n");
	fprintf(outfile,"Input_File_Name   %s\n", sInputName);
	fprintf(outfile,"Output_File_Name   %s\n", sOutputName);
	fprintf(outfile,"Cloud_Fraction %4.2lf\n", dCloudFraction);
	fprintf(outfile,"N_Surface_Nodes   %d\n", iSurfNodes);
	fprintf(outfile,"N_Window_Nodes   %d\n", iWndoNodes);
	fprintf(outfile,"N_Iterations   %d\n", iIterations);
	fprintf(outfile,"Min_Altitude      %5.2lf\n", dMinAlt);
	fprintf(outfile,"N_Altitude_Angles  %d\n", iNumAlts);
	fprintf(outfile,"Min_Azimuth       %5.2lf\n", dMinAzm);
	fprintf(outfile,"N_Azimuth_Angles   %d\n", iNumAzms);

	/* Dump bldg data. */
	dump_bldg(bldg_ptr,outfile);

	/* Dump lib data. */
	dump_lib(lib_ptr,outfile);

	/* Close output file. */
	fclose(outfile);

	return(iReturnVal);
}


/******************************** subroutine DElightElecLtgCtrl4EPlus *******************************/
// Called from DElightManagerC.cpp
/* Calls key daylighting simulation modules necessary for calculating
//	Interior Illuminace levels at each Reference Point within a Zone, and
//	the Power Reduction Factor for the Electric Lighting System defined in EnergyPlus. */
/******************************** subroutine DElightElecLtgCtrl4EPlus *******************************/
DllExport int DElightElecLtgCtrl4EPlus(
	BLDG* bldg_ptr,			/* pointer to DElight Bldg data structure */
	ZONE* zone_ptr,			/* pointer to DElight Zone data structure */
	double dHISKF,			/* Exterior horizontal illuminance from sky (lum/ft^2) */
	double dHISUNF,			/* Exterior horizontal beam illuminance (lum/ft^2) */
	double dCloudFraction,	/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	double dSOLCOS[3],		/* Direction cosines of current sun position */
	double dMinAlt,			/* Minimum daylight factor sun altitude angle */
	double dMinAzm,			/* Minimum daylight factor sun azimuth angle */
	double dMaxAlt,			/* Maximum daylight factor sun altitude angle */
	double dMaxAzm,			/* Maximum daylight factor sun azimuth angle */
	double dAltInc,			/* Increment of daylight factor sun altitude angles */
	double dAzmInc,			/* Increment of daylight factor sun azimuth angles */
    ofstream* pofdmpfile)   // ptr to Error message dump file
{
    // Init return value
    int iReturnVal = 0;

	// Calc interpolation indexes and ratios
	int iphs, iths;				/* sun position alt and azm interpolation indexes */
	double phratio, thratio;	/* sun position alt and azm interpolation displacement ratios */
	if (CalcInterpolationVars(bldg_ptr, dSOLCOS, dMinAlt, dMaxAlt, dAltInc, dMinAzm, dMaxAzm, dAzmInc, &iphs, &iths, &phratio, &thratio) < 0) {
		*pofdmpfile << "ERROR: DElight Bad return from CalcInterpolationVars()\n";
		return(-5);
	}

	// Calc interior daylight illuminance at each refpt
	if (CalcZoneInteriorIllum(zone_ptr, dHISKF, dHISUNF, dCloudFraction, iphs, iths, phratio, thratio) < 0) {
		*pofdmpfile << "ERROR: DElight Bad return from CalcZoneInteriorIllum()\n";
		return(-6);
	}

	// Calc electric lighting power reduction factor for the given zone
	// Create a new SUN2_DATA instance and
	// init the fsunup to 1.0 for no correction due to partial timestep sun up
	SUN2_DATA *sun2_ptr = new SUN2_DATA;
	sun2_ptr->fsunup = 1.0;
    int iDltsysRetVal;
	if ((iDltsysRetVal = dltsys(zone_ptr, sun2_ptr, pofdmpfile)) < 0) {
        // If errors were detected then write error and return, else write warning and return
        if (iDltsysRetVal != -10) {
			*pofdmpfile << "ERROR: DElight error return from dltsys()\n";
			return(-7);
        }
        else {
			*pofdmpfile << "WARNING: DElight warning return from dltsys()\n";
            iReturnVal = -10;
        }
    }

	// Return the calculated Power Reduction Factor for the given Zone, sun position, and exterior horizontal illuminance
	return(iReturnVal);
}

/******************************** subroutine DElightFreeMemory4EPlus *******************************/
// Not currently called from EnergyPlus
/* Calls key daylighting simulation modules necessary for freeing memory allocated by DElight for EnergyPlus. */
/******************************** subroutine DElightFreeMemory4EPlus *******************************/
DllExport int DElightFreeMemory4EPlus(
	BLDG* bldg_ptr,		/* bldg data structure */
	LIB* lib_ptr)		/* library data structure */
{

	/* Free bldg malloc-ed memory */
	// RJH 7/25/03 - malloc/free changed to new/delete
	free_bldg(bldg_ptr);

	/* Free lib malloc-ed memory */
	// RJH 7/25/03 - malloc/free changed to new/delete
	free_lib(lib_ptr);

	return(0);
}
