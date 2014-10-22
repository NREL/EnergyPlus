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

int slite_interreflect(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	LIB *lib_ptr,		/* pointer to library structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int niterate,		/* number of radiosity iterations */
	ofstream* pofdmpfile);/* ptr to LBLDLL error dump file */

int surf_interreflect(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int iz,				/* current zone index */
	int isurf,			/* current surface index */
	double frac,		/* surface reflectance divided by PI */
	ofstream* pofdmpfile);/* ptr to LBLDLL error dump file */

int wndo_interreflect(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int iz,				/* current zone index */
	int is,				/* current surface index */
	int iw,				/* current window index */
	double frac,		/* surface reflectance divided by PI */
	ofstream* pofdmpfile);/* ptr to LBLDLL error dump file */

int refpt_total_illum(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int iz,				/* current zone index */
	ofstream* pofdmpfile);/* ptr to LBLDLL error dump file */
