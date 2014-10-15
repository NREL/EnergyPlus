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

/* Definition file for constants */

#define DTOR (3.14159265/180.0)	/* Degrees to Radians conversion */
#define IMREF 4					/* reference month for initial dcof() calcs (May=4) */
// #define PI 3.1415926	defined in Const.h
#define PIOVR2 1.570796327
#define PIOVR4 0.785398164

#define MIN_DAY_X_DIVISION 8	/* default min # of wndo x-axis elements */
#define MIN_DAY_Y_DIVISION 8	/* default min # of wndo y-axis elements */
#define MAX_DAY_X_DIVISION 40	/* default max # of wndo x-axis elements */
#define MAX_DAY_Y_DIVISION 40	/* default max # of wndo y-axis elements */
#define HOURS 24
#define MAX_BLDG_SHADES 10	/* max # of building shades */
#define MAX_BLDG_ZONES (NO_ZONES_PER_BLDG * LTGZONES_PER_ZONE)	/* max # of building lighting zones */
#define MAX_CHAR_FILE 12	/* max # of characters in input filename */
#define MAX_CHAR_LINE 250	/* max # of characters in input line */
#define MAX_CHAR_TOKEN 10	/* max # of characters in input token */
#define MAX_CHAR_UNAME (REC_NAME_LEN - 1)	/* max # of user name characters */
#define MAX_LIB_COMPS 100	/* max # of library entries for each component */
#define MAX_LT_SCHEDS 10	/* max # of lighting schedules */
#define MAX_PROJECT_BLDGS 2	/* max # of project buildings */
#define MAX_REF_PTS REFERENCES_PER_LIGHTING_ZONE		/* max # of lighting control reference points */
#define MAX_SURF_CFS 100	/* max # of CFS hosted by each surface */
#define MAX_SURF_NODES 1000	/* max # of interreflection nodes on each surface */
#define MAX_SURF_WNDOS (WINDOW_TYPES_PER_WALL * WINDOWS_PER_SURFACE)	/* max # of surface apertures */
#define MAX_WNDO_NODES 500	/* max # of interreflection nodes on each window */
#define MAX_ZONE_SHADES 60	/* max # of zone shades */
#define WPZ WALLS_PER_ZONE	/* max # of E10 walls per thermal zone */
#define RPZ ROOFS_PER_ZONE	/* max # of E10 roofs per thermal zone */
#define FPZ FLOORS_PER_ZONE	/* max # of E10 floors per thermal zone */
#define SPB SHARED_PARTITIONS_PER_BLDG	/* max # of E10 shared partitions per building */
#define PPZ PARTITIONS_PER_ZONE	/* max # of E10 partitions per thermal zone */
#define MAX_ZONE_SURFS (WPZ + RPZ + FPZ + SPB + PPZ)	/* max # of lighting zone surfaces */
#define MONTHS 12
#define NCOORDS 3		/* # of coordinates */
#define NDC 9			/* # of direction cosine values (slite) */
#define NITER 5			/* # of reflection iterations for cfs (slite) */
#define NSKYTYPE 2		/* # of sky conditions (0=clear, 1=overcast) */
#define NPH 4			/* # of sky integration altitude steps */
#define NPHMAX 16		/* # of dreflt integration altitude steps */
#define NPHS 4			/* # of sun position altitudes */
#define NSHDTYPE 2		/* # of wndo shade types (0=bare, 1=with shade) */
#define NTH 9			/* # of sky integration azimuth steps */
#define NTHMAX 16		/* # of dreflt integration azimuth steps */
#define NTHS 5			/* # of sun position azimuths */
#define NTILTS 3		/* # of surface orientation tilts (0=floor, 1=wall, 2=ceiling */
#define NVERTS 4		/* # of vertices */
#define NZSHADES 3		/* # of zone shades around each window (overhang/left/right) */
#define X 0				/* index for X coord of an origin or vertex */
#define Y 1				/* index for Y coord of an origin or vertex */
#define Z 2				/* index for Z coord of an origin or vertex */
#define LZFLOOR 1		/* floor surface type code */
#define LZWALL 2		/* wall surface type code */
#define LZCEILING 3		/* ceiling surface type code */
#define LZNEW 4			/* new (added by DL dlg) surface type code */

//#define max(A,B) ((A)>(B)?(A):(B))
//#define min(A,B) ((A)<(B)?(A):(B))

//	directional Luminance Generators
#define BTDF_HSRES_IN	1000
#define BTDF_HSRES_OUT	1000

// typedefs
typedef void            Void;
typedef float           Float;
typedef double          Double;
typedef char            Char;
typedef int             Short;
typedef int             Int;
typedef long            Long;
typedef unsigned char   Byte;
typedef unsigned int    UInt;
typedef bool			Bool;	//	WLC
#define SELF (*this)    // A syntactic convenience.
#ifndef _countof
#define _countof(arr) (sizeof(arr) / sizeof((arr)[0]))
#endif

