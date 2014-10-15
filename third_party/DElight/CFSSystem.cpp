// CFSSystem.cpp: implementation of the CSystem class(es).
//
//////////////////////////////////////////////////////////////////////
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

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <map>
#include <limits>
using namespace std;

// BGLincludes
#include "BGL.h"
namespace BGL = BldgGeomLib;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include	"CONST.H"
#include	"DBCONST.H"
#include	"DEF.H"
#include	"helpers.h"
#include	"hemisphiral.h"
#include	"NodeMesh2.h"
#include	"WLCSurface.h"
#include	"btdf.h"
#include	"CFSSystem.h"
#include	"CFSSurface.h"
#include	"DOE2DL.H"

extern double NaN_SIGNAL;

CFSSystem::CFSSystem()
{
}

CFSSystem::CFSSystem(string SysType, LumParam lpIn) :
syst_type(SysType), lp(lpIn), pbtdf0(0)
{
//	cout << "CFSSystem::CFSSystem\n";
	//	generate or load btdf if needed
	if (lp.object == "BTDF") {
		if (lp.source == "GEN") {
//			pbtdf0 = GenBTDF(2000,2000,lp);	//	UGH HARDWIRED RESOLUTIONS!!!!!
			pbtdf0 = GenBTDF(lp);
		}
		//	load btdf
		else if (lp.source == "FILE") {
			//	open a file for reading ...
			ifstream	infile(lp.filename.c_str());

            if (!(infile.rdbuf( )->is_open( )))	{
                writewndo("Cannot Open BTDF Data File - must be located in EnergyPlus EXE directory\n","e");
            }

            if (infile)	pbtdf0 = btdfLoad(infile);
        }
//		cout << "CFSSystem::CFSSystem: pbtdf0->summary(): \n";
//		pbtdf0->summary();
//		pbtdf0->plotview(0, 1, 30, 0, 0, 0);
	}
	else {
		pbtdf0 = 0;
	}
}

CFSSystem::~CFSSystem()
{}

string	CFSSystem::TypeName()
{
	return syst_type;
}

void CFSSystem::SetType(string type)
{
	syst_type = type;
//	lp.type = type;
//	ResetLumParam();
}

void	CFSSystem::ResetLumParam(LumParam lpIn)
{
	lp = lpIn;
}

HemiSphiral CFSSystem::CFSLuminanceMap(HemiSphiral& sky, BGL::RHCoordSys3 ics)
{
	// NOTE: LumParam	lp is member of CFSSystem
	HemiSphiral LumMap;

//	cout << "CFSSystem::CFSLuminanceMap: lp.object: " << lp.object << "\n";

	if (lp.object == "LUMMAP")	{
//		if (lp.source == "GEN") LumMap = GenLuminanceMap(Nsize,lp);
		if (lp.source == "GEN") LumMap = GenLuminanceMap(lp);
		else if (lp.source == "FILE") {
			//	open a file for reading ...
			ifstream	infile(lp.filename.c_str());
			if (infile)	LumMap.load(infile);
		//	if (!infile) throw (string("Error: Can't open infile: \"") + infilename) + string("\"");
		}
	}
	else if (lp.object == "BTDF")	{
		/*
		//	this code was moved to the CFSSystem::CFSSystem to avoid redundant calls
		btdf	btdf0(Msize, Nsize);
		//	generate btdf
		if (lp.source == "GEN") btdf0 = GenBTDF(Msize,Nsize,lp);
		//	load btdf
		else if (lp.source == "FILE") {
			//	open a file for reading ...
			ifstream	infile(lp.filename.c_str());
			if (infile)	btdf0.load(infile);
		//	if (!infile) throw (string("Error: Can't open infile: \"") + infilename) + string("\"");
		}
		*/
		//	do the sky-btdf integration
		if(pbtdf0->size() > 0) LumMap = SkyBTDFIntegration(sky, pbtdf0, ics);
	}
	else if (lp.object == "WINDOW")	{
//		if (lp.source == "GEN") LumMap = GenWindowMap(Nsize,lp,sky,ics);
		if (lp.source == "GEN") LumMap = GenWindowMap(lp,sky,ics);
		else if (lp.source == "FILE") {
			//	open a file for reading ...
			ifstream	infile(lp.filename.c_str());
			if (infile)	LumMap.load(infile);
		//	if (!infile) throw (string("Error: Can't open infile: \"") + infilename) + string("\"");
		}
	}
	return LumMap;
}

void CFSSystem::Dump()
{
	
	cout <<	"CFSSystem Params:\n";
	cout <<  "syst_type: " <<  syst_type << "\n";
//		<<  "sky_type: " <<  sky_type << "\n"
//		<<  "nparam: " <<  nParam()
//	cout << "\n";
	lp.Dump();
	if (pbtdf0) pbtdf0->summary();
//	cout << '\n';
}

