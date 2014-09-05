// CFSSurface.cpp: implementation of the CSurface class.
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

#include <string>
#include <map>
#include <vector>
using namespace std;

// BGLincludes
#include "BGL.h"
namespace BGL = BldgGeomLib;

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

extern double MAXPointTol;


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CFSSurface::CFSSurface()
{}

CFSSurface::CFSSurface(SURF *& Parent, string & CFSTypeName, HemiSphiral lm, Double & rotation, vector<BGL::point3> & p3List,Double & CFSmaxNodeArea)
: pParent(Parent), CFStype(CFSTypeName), rotangle(rotation), reveal(0), LumMap(lm)
{
	// names, etc...	
//	string name = pParent->name;					// ersatz auto-naming convention
	string name = "CFS_";					// ersatz auto-naming convention
//	char	buffer[100];
// 	name += _itoa(pParent->ncfs,buffer,10);

	//	rotation w.r.t. host SURF
	//	????
	
	//	initialize the WLCSurface part of CFSSurface
	WLCSurfInit(name, p3List, CFSmaxNodeArea);
}

CFSSurface::~CFSSurface()
{}

string	CFSSurface::TypeName()
{
	return CFStype;
}

Double	CFSSurface::fReveal(Double costheta)
{
	if (reveal == 0) return 1;
	if (costheta <= 0) return 0;
	if (costheta > 1) return 1;
	Double	tantheta = sqrt(1./(costheta*costheta) - 1.);	//	tan theta
	Double	fReveal = max(1. - reveal*tantheta/sqrt(Area()), 0.); 
//	cout << costheta << " " << fReveal << "\n";
	return	fReveal; 
}

HemiSphiral	CFSSurface::GetLumMap()
{
	return LumMap;
}

Int		CFSSurface::ResetLumMap(HemiSphiral lm)
{
	LumMap = lm;
	return LumMap.size();
}

Double	CFSSurface::CFSDirIllum(BGL::vector3 dir_CFSCS)
{
	return LumMap.interp(dir_CFSCS);
}

Double	CFSSurface::TotRefPtIllum(BGL::vector3 ExtPtNormal, BGL::point3 ExtPtPosition/*, Double ExtNodeArea*/)
{
	//	behind-plane, on-plane visibility culling
	if (plane3::Behind(ExtPtPosition)) return 0;

	Double	visDot;
	Double	TotExtPtIllum = 0.;
	for (Int ii=0; ii < MeshSize(); ii++) {	//CFS mesh loop
		BGL::vector3	VDir_wcs = ExtPtPosition - NodePosition3D(ii);
		//	CFS node to "in-front-of RefPt" visibility test
		visDot = BGL::dot(BGL::norm(VDir_wcs),ExtPtNormal);
		if (visDot >= 0.) continue;	//	behind or on edge
		//	convert VDir to LCS coords & BGL::normalize for call to CFSDirIllum
		BGL::vector3	VDir_CFS = BGL::dirWCStoLCS(BGL::norm(VDir_wcs),ics);
	//  N.B. line below:  no CosTheta factor yet because CFSDirIllum is per unit solid angle
//		Double	CFSNodeLum = CFSDirIllum(VDir_CFS)*NodeArea(ii);
	//  N.B. line below:  CosTheta factor = dot(VDir_CFS,[0,0,1]) = VDir_CFS[2]!
		Double	CFSNodeLum = max(0.,CFSDirIllum(VDir_CFS)*NodeArea(ii)*VDir_CFS[2]*fReveal(VDir_CFS[2]));
//		if (CFSNodeLum == 0) continue;
		// Ext Pt unit-area SolidAngle as seen from the CFS node
//		Double	ExtNodeSolidAngle = ExtNodeArea*BGL::dot(ExtNodeNormal,-BGL::norm(VDir_wcs))/BGL::sqrlen(VDir_wcs);
		Double	ExtPtSolidAngle = min(2*PI,-visDot/BGL::sqrlen(VDir_wcs));	//	sqrlen() = 0 is trapped by 1st visibility culling above
		TotExtPtIllum += CFSNodeLum*ExtPtSolidAngle;
		/*
		cout << 
				ii+1 << 
				" " << NodePosition3D(ii) << 
				" " << visDot << 
//				" " << VDir_wcs <<
				" " << VDir_CFS <<
//				" " << NodeArea(ii) << 
				" " << CFSDirIllum(VDir_CFS) <<
				" " << CFSNodeLum <<
//				" " << BGL::sqrlen(VDir_wcs) << 
				" " << ExtPtSolidAngle << 
				" " << CFSNodeLum*ExtPtSolidAngle <<
				" "	<< "\n";
		*/
	}
	return (TotExtPtIllum);
}
