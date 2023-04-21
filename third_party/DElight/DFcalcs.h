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

int	CalcDFs(
	SUN_DATA *sun_ptr,		/* pointer to sun data structure */
	BLDG *bldg_ptr,			/* pointer to bldg structure */
	LIB *lib_ptr,			/* pointer to library structure */
	int iIterations,		/* number of radiosity iterations */
	ofstream* pofdmpfile);	/* ptr to LBLDLL error dump file */

int	wndo_element_refpt_illum_contrib(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	LIB *lib_ptr,		/* pointer to lib structure */
	int izone,			/* current zone index */
	int iWndoSurf,		/* current surface index for Surface containing Window */
	int iNodeSurf,		/* current surface index for Surface containing Node (Not Applicable) */
	int iwndo,			/* current window index */
	int iWndoElement,	/* window element index */
	double thsun,		/* sun azm (radians) */
	double phsun,		/* sun alt (radians) */
	double thray,		/* sky element azm angle */
	double phray,		/* sky element alt angle */
	int iphs,			/* sun alt index */
	int iths,			/* sun azm index */
	double solic[MONTHS],	/* extraterrestrial irrad for 1st of each month (0 to 11) */
	double node[NCOORDS],	/* coords of refpt */
	double nodesurfnormal[NCOORDS],	/* INWARD normal unit vector from face of refpt virtual surface */
	double cosincidence,	/* cos(angle of incidence) of vector from refpt to wndo element */
	HIT *hit_ptr,			/* hit structure pointer for ray from refpt to wndo element */
	double domega,			/* solid angle subtended by window element wrt ref_pt */
	double vis_trans,		/* tvis of glass for normal incidence angle */
	double tvisincidence,	/* tvis of glass for current incidence angle */
	double wnorm[NCOORDS],	/* window outward normal vector */
	double tfac,			/* turbidity factor */
	double zenl,			/* zenith luminance */
	// return values stored in ref pt substructure
	double *pdirect_skycillum,	/* ptr to direct illuminance from sky - clear */
	double *pdirect_suncillum,	/* ptr to direct illuminance from sun - clear */
	double *pdirect_skyoillum,	/* ptr to direct illuminance from sky - overcast */
	ofstream* pofdmpfile);		/* ptr to LBLDLL error dump file */

int	wndo_element_surfnode_lum_contrib(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	LIB *lib_ptr,		/* pointer to lib structure */
	int izone,			/* current zone index */
	int iWndoSurf,		/* current surface index for Surface containing Window */
	int iNodeSurf,		/* current surface index for Surface containing Node */
	int iwndo,			/* current window index */
	int iWndoElement,	/* window element index */
	double thsun,		/* sun azm (radians) */
	double phsun,		/* sun alt (radians) */
	double thray,		/* sky element azm angle */
	double phray,		/* sky element alt angle */
	int iphs,			/* sun alt index */
	int iths,			/* sun azm index */
	double solic[MONTHS],	/* extraterrestrial irrad for 1st of each month (0 to 11) */
	double node[NCOORDS],	/* coords of surfnode */
	double nodesurfnormal[NCOORDS],	/* INWARD normal unit vector from face of surfnode */
	double dNodeSurfaceReflectance, // visible reflectance of node surface
	double cosincidence,	/* cos(angle of incidence) of vector from surfnode to wndo element */
	HIT *hit_ptr,			/* hit structure pointer for ray from surfnode to wndo element */
	double domega,			/* solid angle subtended by window element wrt surfnode */
	double vis_trans,		/* tvis of glass for normal incidence angle */
	double tvisincidence,	/* tvis of glass for current incidence angle */
	double wnorm[NCOORDS],	/* window outward normal vector */
	double tfac,			/* turbidity factor */
	double zenl,			/* zenith luminance */
	// return values stored in surf node, or wndo node substructure
	double *pdirect_skyclum,	/* ptr to direct luminance from sky - clear */
	double *pdirect_sunclum,	/* ptr to direct luminance from sun - clear */
	double *pdirect_skyolum,	/* ptr to direct luminance from sky - overcast */
	ofstream* pofdmpfile);		/* ptr to LBLDLL error dump file */
