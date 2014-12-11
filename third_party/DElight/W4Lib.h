/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *          Building Technologies Department
 *          Lawrence Berkeley National Laboratory
 */
/**************************************************************
 * C Language Implementation of DOE2 Daylighting Algorithms.
 *
 * The original DOE2 algorithms and implementation in FORTRAN
 * were developed by F.C. Winkelmann.
 * Simulation Research Group, Lawrence Berkeley Laboratory.
 *
 * Note that the routines in this module are not part of DOE2.
 **************************************************************/

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

int process_W4glazing_types(
	BLDG *bldg_ptr,			/* building structure pointer */
	LIB *lib_ptr,			/* library structure pointer */
	FILE *W4libfile,		/* pointer to Window4 library data file */
	ofstream* pofdmpfile);	/* ptr to dump file */

int IsGlassIDUnique(
	int iGlass_Type,			// current glass_type ID
	int iUniqueGlassIDs[200],	// array of encountered glass_type IDs					
	int* piUniqueIDCount);		// ptr to current count of unique glass_type ID

int ProcessW4GlassType(
	int iGlass_Type,	// W4 glass_type ID
	LIB *lib_ptr,		// library structure pointer
	FILE *W4libfile,	// pointer to Window4 library data file
	ofstream* pofdmpfile);	/* ptr to dump file */

int Qikfit4(
	int iData,			// number of data points
	double dYdat[10],	// normalized angular visible data
	double* pdTvFit1,	// pointer to coef1
	double* pdTvFit2);	// pointer to coef2
