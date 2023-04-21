// CFSSurface.h: interface for the CSurface class.
//
//////////////////////////////////////////////////////////////////////
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

//	forward declarations
class	CFSSystem;
class	NodeMesh;
struct	SURF;


class CFSSurface : public WLCSurface 
{
public:
	// -------------------- constructors -------------------- 
	CFSSurface();
//	CFSSurface(SURF *Parent, string CFStype, BGL::vector3 offset, Double width, Double height, Double rotation);
	CFSSurface(SURF *& Parent, string & CFStype, HemiSphiral LumMap, Double & rotation, vector<BGL::point3> & p3List,Double & CFSmaxNodeArea);
	~CFSSurface();


	// -------------------- methods -------------------- 
//	see also WLCSurface, surf3, plane3 methods
	string	TypeName();

	Double	Reveal() { return reveal;}				//	get...
	void	Reveal(Double rval) { reveal = rval;}	//	set...
	Double	fReveal(Double costheta);
	HemiSphiral		GetLumMap();
	Int		ResetLumMap(HemiSphiral lm);
	Double	CFSDirIllum(BGL::vector3 dir_CFSCS);	//	dir generator version
	Double	TotRefPtIllum(BGL::vector3 ExtPtNormal, BGL::point3 ExtPtPosition);

protected:
//	see also WLCSurface, surf3, plane3 members
	SURF	*pParent;	// pointer to parent SURF 
	string	CFStype;	// library CFS system type uname 
	Double	rotangle;			// in-plane rotation angle (radians) 
	Double	reveal;		//	aperture reveal depth
	HemiSphiral	LumMap;	//	transmitted CFSSurf LumMap

};

