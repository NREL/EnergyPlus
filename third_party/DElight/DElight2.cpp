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
#include "Loaddata.h"
#include "geom.h"
#include "EPlus_Loaddata.h"
#include "EPlus_Geom.h"
#include "DFcalcs.h"
#include "ECM.H"
#include "savedata.h"
#include "TOOLS.H"
#include "WxTMY2.h"
#include "W4Lib.h"
#include <limits>

#ifndef INFINITY
double INFINITY = numeric_limits<double>::infinity();
#endif
double NaN_QUIET = numeric_limits<double>::quiet_NaN();
double NaN_SIGNAL = NaN_QUIET;
double MAXPointTol = 1.e-10;

/******************************** subroutine DElight2 *******************************/
/* Calls key daylighting simulation modules. */
/* This is the key exported function that defines the DElight2 API */
/* for standalone use, or integration with programs other than EnergyPlus. */
/* NOT USED in integration with EnergyPlus. */
/******************************** subroutine DElight2 *******************************/
DllExport int	DElight2(
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
	int iYear)							/* 4 digit year of run period */
{
	FILE *wxfile;						/* weather file pointer */
	FILE *infile;						/* input file pointer */
	FILE *outfile;						/* output file pointer */
	FILE *W4libfile;					/* Window 4 library file pointer */
	ofstream ofdmpfile("DElight2.DMP");	/* LBLDLL debug dump file */
	BLDG bldg;							/* bldg data structure */
	LIB lib;							/* library data structure */
	RUN_DATA run_data;					/* run data structure */

	SUN_DATA sun_data;	/* sun data structure */

	int wx_flag;		/* Wxfile flag */

    int iReturnVal = 0; // Return value

	// Check for successful opening of debug dump file.
	if(!ofdmpfile)
	{
		return (-1);
	}

	/* initialize BLDG and LIB structures */
	struct_init("BLDG",(char *)&bldg);
	struct_init("LIB",(char *)&lib);

	// Open the input file for loading
	if((infile = fopen(sInputName, "r" )) == NULL ) {
		ofdmpfile << "ERROR: DElight cannot open input file [" <<sInputName << "]\n";
		/* Close dump file. */
		ofdmpfile.close();
		return(-3);
	}

	// Read the first heading line in the input file to determine input version
	char cInputLine[MAX_CHAR_LINE+1];	/* Input line */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	char cInputVersion[MAX_CHAR_UNAME+1];
	sscanf(cInputLine,"%*s %s\n",cInputVersion);

    // If the input version is either EPlus or 2.3 then load data using LoadDataFromEPlus()
	if ((strcmp(cInputVersion,"EPlus") == 0) || (strcmp(cInputVersion,"2.3") == 0))
	{
		if (LoadDataFromEPlus(&bldg,infile,&ofdmpfile) < 0) {
			ofdmpfile << "ERROR: DElight Bad Building data read from input file [" <<sInputName << "]\n";
			/* Close dump file. */
			ofdmpfile.close();
			/* Close input file. */
			fclose(infile);
			return(-4);
		}

		/* load library data from input file */
		if (LoadLibDataFromEPlus(&lib,infile,&ofdmpfile) < 0) {
			ofdmpfile << "ERROR: DElight Bad Library data read from input file [" <<sInputName << "]\n";
			/* Close dump file. */
			ofdmpfile.close();
			/* Close input file. */
			fclose(infile);
			return(-4);
		}

		/* Close input file after successful read. */
		fclose(infile);

		/* Calculate geometrical values required for DF calcs. */
		if (iSurfNodes > MAX_SURF_NODES) iSurfNodes = MAX_SURF_NODES;
		if (iWndoNodes > MAX_WNDO_NODES) iWndoNodes = MAX_WNDO_NODES;
		if (CalcGeomFromEPlus(&bldg) < 0) {
			ofdmpfile << "ERROR: DElight Bad return from CalcGeomFromEPlus()\n";
			/* Close dump file. */
			ofdmpfile.close();
			return(-4);
		}
	}
	else // use old load_bldg()
	{
		if (load_bldg(&bldg,infile,&ofdmpfile) < 0) {
			ofdmpfile << "ERROR: DElight Bad Building data read from input file [" <<sInputName << "]\n";
			/* Close dump file. */
			ofdmpfile.close();
			/* Close input file. */
			fclose(infile);
			return(-4);
		}

		/* load library data from input file */
		if (load_lib(&lib,infile,&ofdmpfile) < 0) {
			ofdmpfile << "ERROR: DElight Bad Library data read from input file [" <<sInputName << "]\n";
			/* Close dump file. */
			ofdmpfile.close();
			/* Close input file. */
			fclose(infile);
			return(-4);
		}

		/* Close input file after successful read. */
		fclose(infile);

		/* Translate user oriented bldg geometry to DOE2 bldg coord system. */
		/* Also, calculate radiosity related geometrical values. */
		if (iSurfNodes > MAX_SURF_NODES) iSurfNodes = MAX_SURF_NODES;
		if (iWndoNodes > MAX_WNDO_NODES) iWndoNodes = MAX_WNDO_NODES;
		if (geometrans(&bldg,iSurfNodes,iWndoNodes,&ofdmpfile) < 0) {
			ofdmpfile << "ERROR: DElight Bad return from geometrans()\n";
			/* Close dump file. */
			ofdmpfile.close();
			return(-4);
		}
	}

	/* Open Window4 library file and read and process glazing types included in user input file. */
	if (strcmp(sW4LibName,"") != 0) {
		if((W4libfile = fopen(sW4LibName, "r" )) == NULL ) {
			ofdmpfile << "ERROR: DElight Cannot open Window4 library file [" <<sW4LibName << "]\n";
			/* Close dump file. */
			ofdmpfile.close();
			return(-5);
		}
		else {
			/* read and process glazing types included in user input file */
			if (process_W4glazing_types(&bldg,&lib,W4libfile,&ofdmpfile) < 0) {
				ofdmpfile << "ERROR: DElight Bad Window4 Library data read from file [" <<sW4LibName << "]\n";
				/* Close dump file. */
				ofdmpfile.close();
				return(-5);
			}
			/* Close Window4 library file. */
			fclose(W4libfile);
		}
	}

	/* Open TMY2 weather file and read header information. */
	// Note that header info includes site location used in CalcDFs.
	if (strcmp(sWxName,"") == 0) {
		wx_flag = 0;
	}
	else {
		if((wxfile = fopen(sWxName, "r" )) == NULL ) {
			ofdmpfile << "WARNING: DElight Cannot open weather file [" <<sWxName << "]\n";
			wx_flag = 0;
            // Set return value for warning
            iReturnVal = -10;
		}
		else {
			/* read header information */
			if(read_wx_tmy2_hdr(&bldg,wxfile) < 0) return(-1);
			wx_flag = 1;
		}
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
    int iCalcDFsReturnVal = CalcDFs(&sun_data,&bldg,&lib,iIterations,&ofdmpfile);
	if (iCalcDFsReturnVal < 0) {
        // If Errors have been detected then return now, else ignore Warnings (return value == -10) until return from DElight
	    if (iCalcDFsReturnVal != -10) {
		    ofdmpfile << "ERROR: DElight Bad return from CalcDFs()\n";
		    /* Close dump file. */
		    ofdmpfile.close();
		    /* Close wx file. */
		    if (wx_flag) fclose(wxfile);
		    return(-4);
        }
        else {
            iReturnVal = -10;
        }
	}

	/* Set hourly run period data. */
	/* Beginning month of run period. */
	run_data.mon_begin = iStrtMonth;
	/* Beginning day of month of run period */
	run_data.day_begin = iStrtDay;
	/* Ending month of run period */
	run_data.mon_end = iEndMonth;
	/* Ending day of month of run period */
	run_data.day_end = iEndDay;
	/* 4 digit year of run period */
	run_data.year = iYear;

	/* Check for no run period specified => do not perform hourly calcs. */
	if ((iStrtMonth != 0) && (iStrtDay != 0) && (iEndMonth != 0) && (iEndDay != 0)) {
		/* Calculate hourly illuminances, glare index and fractional electric light reductions due to daylight. */
        int iDillumReturnVal = dillum(dCloudFraction,&bldg,&sun_data,&run_data,wx_flag,wxfile,&ofdmpfile);
		if (iDillumReturnVal < 0) {
            // If Errors have been detected then return now, else ignore Warnings until return from DElight
	        if (iDillumReturnVal != -10) {
			    ofdmpfile << "ERROR: DElight Bad return from dillum()\n";
			    /* Close dump file. */
			    ofdmpfile.close();
			    /* Close wx file. */
			    if (wx_flag) fclose(wxfile);
			    return(-4);
            }
            else {
                iReturnVal = -10;
            }
		}
	}

	/* Open output file. */
	if((outfile = fopen(sOutputName, "w" )) == NULL ) {
		ofdmpfile << "ERROR: DElight Cannot open output file [" <<sOutputName << "]\n";
		/* Close dump file. */
		ofdmpfile.close();
		/* Close wx file. */
		if (wx_flag) fclose(wxfile);
		return(-2);
	}

	/* Dump runtime data. */
	fprintf(outfile,"RUNTIME DATA\n");
	fprintf(outfile,"Input_File_Name   %s\n", sInputName);
	fprintf(outfile,"Output_File_Name   %s\n", sOutputName);
	fprintf(outfile,"Weather_File_Name %s\n", sWxName);
	fprintf(outfile,"W4Lib_File_Name %s\n", sW4LibName);
	fprintf(outfile,"Cloud_Fraction %4.2lf\n", dCloudFraction);
	fprintf(outfile,"N_Surface_Nodes   %d\n", iSurfNodes);
	fprintf(outfile,"N_Window_Nodes   %d\n", iWndoNodes);
	fprintf(outfile,"N_Iterations   %d\n", iIterations);
	fprintf(outfile,"Min_Altitude      %5.2lf\n", dMinAlt);
	fprintf(outfile,"N_Altitude_Angles  %d\n", iNumAlts);
	fprintf(outfile,"Min_Azimuth       %5.2lf\n", dMinAzm);
	fprintf(outfile,"N_Azimuth_Angles   %d\n", iNumAzms);
	fprintf(outfile,"Start_Month %2d\n", iStrtMonth);
	fprintf(outfile,"Start_Day   %2d\n", iStrtDay);
	fprintf(outfile,"End_Month   %2d\n", iEndMonth);
	fprintf(outfile,"End_Day     %2d\n", iEndDay);
	fprintf(outfile,"Year %d\n", iYear);

	/* Dump bldg data. */
	dump_bldg(&bldg,outfile);

	/* Dump lib data. */
	dump_lib(&lib,outfile);

	/* Free bldg malloc-ed memory */
	free_bldg(&bldg);

	/* Free lib malloc-ed memory */
	free_lib(&lib);

	/* Close output file. */
	fclose(outfile);

	/* Close error output file. */
	ofdmpfile.close();

	/* Close wx file. */
	if (wx_flag) fclose(wxfile);

	return (iReturnVal);
}
