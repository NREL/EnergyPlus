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

/* Internal Data Structure Definitions */

class	CFSSurface;

/* function types */

double dskylu();
double dnsol();
double ddot();
double dpfac();
double ran0();

/* data structures */

typedef struct {	/* ecm runtime data for dillum() */
	int mon_begin;	/* beginning month (1 through 12) */
	int day_begin;	/* beginning day (1 through 28,30,or31) */
	int mon_end;	/* ending month (1 through 12) */
	int day_end;	/* ending day (1 through 28,30,or31) */
	int year;		/* 4-digit year of run */
} RUN_DATA;

typedef struct {	/* dcm sun data for dcof() */
	int nphs;		/* number of desired sun position altitudes */
	int nths;		/* number of desired sun position azimuths */
	double phsmin;	/* minimum sun altitude (degrees) */
	double thsmin;	/* minimum sun azimuth (degrees: South=0.0, East=+90.0) */
} SUN_DATA;

typedef struct {	/* ecm solar angle data for dillum()-sun1() */
	double decl;		/* angle of declination (radians) */
	double tan_decl;	/* tangent of angle of declination */
	double cos_decl;	/* cosine of angle of declination */
	double sin_decl;	/* sine of angle of declination */
	double eqtime;	/* equation of time */
	double gundog;	/* hour angle of sunrise */
} SUN1_DATA;

typedef struct {	/* ecm solar angle data for dillum()-sun2() */
	int isunup;		/* sun up flag (1=up) */
	double fsunup;	/* fraction of hour that the sun is up */
	double raycos[3];/* solar direction cosines */
	int cldamt;		/* cloud amount in tenths */
	double dirsol;	/* direct solar radiation */
	double solrad;	/* total horizontal solar radiation */
	double dewpt;	/* dewpoint temperature in degrees F */
} SUN2_DATA;

typedef struct {	/* shading surface hit structure */
	int ihit;		/* hit flag (0=no hit, 1=zshade hit, 2=exterior zone surface hit, 3=bshade hit from behind, 4=bshade hit from front) */
	int hitshade;	/* index of hit shade or surface */
	int hitzone;	/* index of zone of hit surface */
} HIT;

typedef struct {	/* zone reflectance structure */
	int nwtot;			/* # of windows in zone with area > 0.1 ft2 */
	double atot;			/* total inside surface area including windows */
	double arhtot;		/* sum of products of inside surf area * vis_refl */
	double ar[NTILTS];	/* areas for each surf orientation (0=floor, 1=wall, 2=ceiling) */
	double arh[NTILTS];	/* area * refl for each surf orientation */
	double avg_refl;		/* average inside surface reflectance of zone */
} ZONE_REFL;

typedef struct {	/* lighting schedule structure */
	char name[MAX_CHAR_UNAME+1];	/* lighting schedule uname */
	int mon_begin;		/* beginning month (1 through 12) */
	int day_begin;		/* beginning day (1 through 28,30,or31) */
	int mon_end;		/* ending month (1 through 12) */
	int day_end;		/* ending day (1 through 28,30,or31) */
	int dow_begin;		/* beginning day of week (1=Monday to 7=Sunday) */
	int dow_end;		/* ending day of week (1=Monday to 7=Sunday) */
	double frac[HOURS];	/* fraction of hour that lighting power is on */
	/* -------------------- derived quantities -------------------- */
	int doy_begin;	/* sequential beginning day of year */
	int doy_end;	/* sequential ending day of year */
} LTSCH;

struct WNDO : public WLCSurface
{	/* aperture data structure */
	char name[MAX_CHAR_UNAME+1];	/* window uname */
	double origin[NCOORDS];	/* origin coords (surface sys) (origin[2]>0 == z == setback) */
	double height;			/* height (ft) */
	double width;			/* width (ft) */
	char glass_type[MAX_CHAR_UNAME+1];	/* library glass type uname */
	int shade_flag;			/* shade flag (0=clear, 1=diffuse glazing) */
	char shade_type[MAX_CHAR_UNAME+1];	/* lib wndo shade type uname */
	double zshade_x[NZSHADES];		/* overhang/fin zone shade depth (ft) (viewed from inside looking outward (0=overhang, 1=right fin, 2=left fin)). */
	double zshade_y[NZSHADES];		/* overhang/fin zone shade distance from window (ft) */
	int nvertices;					/* # of vertices */
	/* -------------------- World Coordinate System vertex coordinates --------------------- */
	vector<BGL::point3> vPt3VerticesWCS_OCCW;	// Vertex list in WCS OUTSIDE-CounterClockWise order
	vector<BGL::point3> v3List_WLC;	// WLC Vertex list in WCS INSIDE-CounterClockWise order
	/* -------------------- derived quantities -------------------- */
	double vert[NCOORDS][NVERTS];	/* vertex coordinates (bldg sys) */
	/* ------------------ interreflection derived quantities ------------------- */
	double node_area;					/* area of window nodes */
	double node_areas[MAX_WNDO_NODES];	/* node areas */
	double node[MAX_WNDO_NODES][NCOORDS];	/* node coordinates (bldg sys) */
	int n_width;				/* number of nodes in window width direction */
	int n_height;				/* number of nodes in window height direction */
	int nnodes;					/* number of window nodes */
	/* ----------------- calculation variables ----------------- */
	double wlumsky[NPHS][NTHS];	/* center of window luminace from clear sky */
	double wlumsun[NPHS][NTHS];	/* center of window luminace from clear sun */
	double wlumskyo;	/* center of window luminace from overcast sky */
	double direct_skyclum[MAX_WNDO_NODES][NPHS][NTHS];	/* direct luminance from sky - clear */
	double direct_sunclum[MAX_WNDO_NODES][NPHS][NTHS];	/* direct luminance from sun - clear */
	double direct_skyolum[MAX_WNDO_NODES];				/* direct luminance from sky - overcast */
	double skyclum[MAX_WNDO_NODES][NPHS][NTHS];		/* total luminance from sky - clear */
	double sunclum[MAX_WNDO_NODES][NPHS][NTHS];		/* total luminance from sun - clear */
	double skyolum[MAX_WNDO_NODES];					/* total luminance from sky - overcast */
	/* ----------------- WLC data and methods ----------------- */
	void WLCWNDOInit(Double maxNodeArea);
} ;

struct SURF : public WLCSurface
{	/* zone surface data structure */
	char name[MAX_CHAR_UNAME+1];	/* surface uname */
	double origin[NCOORDS];	/* origin coordinates (zone sys) */
	double height;			/* height (ft) */
	double width;			/* width (ft) */
	double azm_zs;			/* azimuth (zone sys) */
	double tilt_zs;			/* tilt (0 to 180) (zone sys) */
	double vis_refl;		/* inside visible reflectance */
	double ext_vis_refl;	/* exterior visible reflectance */
	double gnd_refl;		/* ground reflectance */
	int type;				/* surface type (1=floor, 2=wall, 3=ceiling) */
	double area;			/* area (ft2) */
	int E10ndx;				/* E10 surface type list index */
	int nwndos;					/* # of apertures (windows or skylights) */
	WNDO *wndo[MAX_SURF_WNDOS];	/* aperture pointers */
	int ncfs;					/* # of CFS Surfaces */
	CFSSurface *cfs[MAX_SURF_CFS];	/* CFS Surface pointers */
	vector<CFSSystem*> vpCFSSystem;	// vector list of CFS Systems
	int nvertices;					/* # of vertices */
	/* -------------------- World Coordinate System vertex coordinates --------------------- */
	vector<BGL::point3> vPt3VerticesWCS_OCCW;	// Vertex list in WCS OUTSIDE-CounterClockWise order
	vector<BGL::point3> v3List_WLC;	// WLC Vertex list in WCS INSIDE-CounterClockWise order
	/* -------------------- derived quantities --------------------- */
	double vert[NCOORDS][NVERTS];	/* vertex coordinates (bldg sys) */
	double azm_bs;					/* azimuth (bldg sys) */
	double tilt_bs;					/* tilt (0 to 180) (bldg sys) */
	double skylum[NPHS][NTHS];		/* surface exterior lum (cd/ft2) from sky (each sun pos) */
	double sunlum[NPHS][NTHS];		/* surface exterior lum (cd/ft2) from sun (each sun pos) */
	double ovrlum;					/* surface exterior lum (cd/ft2) from overcast sky */
	double outward_uvect[NCOORDS];	/* outward pointing unit vector (DOE2) (bldg sys coords) */
	double inward_uvect[NCOORDS];	/* inward pointing unit vector (Superlite) (bldg sys coords) */
	/* ------------------ interreflection derived quantities ------------------- */
	double dircos[NDC];		/* surface direction cosine values (slite) */
	double node_area;					/* area of surface nodes */
	double node_areas[MAX_SURF_NODES];	/* node areas */
	double node[MAX_SURF_NODES][NCOORDS];	/* node coordinates (bldg sys) */
	int n_width;				/* number of nodes in surface width direction */
	int n_height;				/* number of nodes in surface height direction */
	int nnodes;					/* number of surface nodes */
	/* ----------------- calculation variables ----------------- */
	double TotDirectSkyCIllum[NPHS][NTHS];	/* total direct illuminance from sky - clear */
	double TotDirectSunCIllum[NPHS][NTHS];	/* total direct illuminance from sun - clear */
	double TotDirectOvercastIllum;			/* total direct illuminance from sky - overcast */
	double direct_skyclum[MAX_SURF_NODES][NPHS][NTHS];	/* direct luminance from sky - clear */
	double direct_sunclum[MAX_SURF_NODES][NPHS][NTHS];	/* direct luminance from sun - clear */
	double direct_skyolum[MAX_SURF_NODES];				/* direct luminance from sky - overcast */
	double skyclum[MAX_SURF_NODES][NPHS][NTHS];		/* total luminance from sky - clear */
	double sunclum[MAX_SURF_NODES][NPHS][NTHS];		/* total luminance from sun - clear */
	double skyolum[MAX_SURF_NODES];					/* total luminance from sky - overcast */
	/* ----------------- WLC data and methods ----------------- */
    SURF();
	void	WLCSURFInit(string Name, Double maxNodeArea);
	double	NetArea();
} ;

typedef struct {	/* window luminance factor structure */
	double sfsky[NPHS][NTHS];	/* window luminance sky - clear */
	double sfsun[NPHS][NTHS];	/* window luminance sun - clear */
	double sfskyo;				/* window luminance sky - overcast */
	double omega;				/* solid angle subtended by wndo from ref pt */
	double omegaw;				/* solid angle weighted by glare position factor */
} WLUM;

typedef struct {	/* lighting zone reference point structure */
	char name[MAX_CHAR_UNAME+1];	/* ref pt uname */
	double zs[NCOORDS];		/* ref_pt coords (zone sys) */
	double zone_frac;		/* fraction of floor area of zone controlled */
	double lt_set_pt;		/* lighting set point (fc) */
	int lt_ctrl_type;		/* lighting control type (0=none 1=cont 2=step 3=cont/off) */
	/* -------------------- derived quantities -------------------- */
	double bs[NCOORDS];	/* ref_pt coords (bldg sys) */
	/* --------------------- daylight illuminance --------------------- */
	double skycillum[NPHS][NTHS];		/* total illuminance from sky (fc) - clear */
	double suncillum[NPHS][NTHS];		/* total illuminance from sun (fc) - clear */
	double skyoillum;					/* total illuminance from sky (fc) - overcast */
	double day_illum[MONTHS][HOURS];	/* monthly avg hourly daylight illum (fc) */
	/* --------------- temporary calculation variables --------------- */
	double daylight;				/* temp current hour daylight illuminance (fc) */
	double glarendx;				/* temp current hour glare index */
	double frac_power;			/* temp current hour fractional lighting power on */
	/* --------------------- daylight factors --------------------- */
	double dfsky[NPHS][NTHS];	/* ref_pt illuminance daylight factor sky - clear */
	double dfsun[NPHS][NTHS];	/* ref_pt illuminance daylight factor sun - clear */
	double dfskyo;				/* ref_pt illuminance daylight factor sky - overcast */
	/* ------------------- background luminance factors ------------------- */
	double bfsky[NPHS][NTHS];	/* background luminance sky - clear */
	double bfsun[NPHS][NTHS];	/* background luminance sun - clear */
	double bfskyo;				/* background luminance sky - overcast */
	/* ----------- window luminance factor structure pointers ----------- */
	WLUM *wlum[MAX_ZONE_SURFS][MAX_SURF_WNDOS];	/* window lum pointers */
	/* ------------------------- glare index ------------------------- */
	double glare[MONTHS][HOURS];	/* monthly avg hourly glare index */
	double dcm_glare[NSKYTYPE];	/* dcm glare index for each sky condition */
	/* ------------------- calculation variables ------------------- */
	double delf_overcast;
	double delf_skyclear[NPHS][NTHS];
	double delf_sunclear[NPHS][NTHS];	/* tmp accumulators for reflected illum */
	double direct_skyoillum;				/* direct illuminance from sky - overcast */
	double direct_skycillum[NPHS][NTHS];	/* direct illuminance from sky - clear */
	double direct_suncillum[NPHS][NTHS];	/* direct illuminance from sun - clear */
} REFPT;

typedef struct {	/* zone shade data structure */
	char name[MAX_CHAR_UNAME+1];	/* zone shade uname */
	double origin[NCOORDS];	/* origin coordinates (zone sys) */
	double height;			/* height (ft) */
	double width;			/* width (ft) */
	double azm_zs;			/* azimuth (zone system) */
	double tilt_zs;			/* tilt (0 to 180) (zone system) */
	/* -------------------- derived quantities -------------------- */
	double vert[NCOORDS][NVERTS];	/* vertex coordinates (bldg sys) */
	double azm_bs;				/* azimuth (bldg system) */
	double tilt_bs;				/* tilt (0 to 180) (bldg system) */
} ZSHADE;

typedef struct {	/* building lighting zone data structure */
	char name[MAX_CHAR_UNAME+1];	/* zone uname */
	double origin[NCOORDS];		/* origin coordinates (bldg sys) */
	double azm;					/* azimuth */
	double mult;					/* zone multiplier */
	double flarea;				/* total zone floor area */
	double volume;				/* volume */
	double lighting;				/* installed lighting (W/ft2) */
	double min_power;			/* min input pwr fraction for cont dim system */
	double min_light;			/* light output fraction at min_power */
	int lt_ctrl_steps;			/* number of steps in stepped system */
	double lt_ctrl_prob;			/* stepped lighting control probability */
	double view_azm;				/* direction of occupant view (clockwise from zone y-axis) */
	double max_grid_node_area;		/* max node area for gridding all surfaces in zone */
	int nltsch;					/* # of electric lighting schedules */
	LTSCH *ltsch[MAX_LT_SCHEDS];/* lighting schedule pointers */
	int nsurfs;					/* # of surfaces */
	SURF *surf[MAX_ZONE_SURFS];	/* surface pointers */
	int nzshades;					/* # of zone shades */
	ZSHADE *zshade[MAX_ZONE_SHADES];/* zone shade struct pointers */
	int nrefpts;				/* # of lighting control reference points */
	REFPT *ref_pt[MAX_REF_PTS];	/* reference point pointers */
	char e10zonename[MAX_CHAR_UNAME+1];	/* E-10 thermal zone name */
	int eleclt_details;			/* are there electric ltg details? 0=No 1=Yes */
	/* --------------- temporary zone value place holders --------------- */
	double frac_power;		/* current hour fractional lighting power on */
	int ltsch_id;			/* current lighting schedule index */
	/* ----------------------- derived quantities ----------------------- */
	double lt_reduc[MONTHS][HOURS];	/* monthly avg fraction lighting energy reduction */
	double annual_reduc[HOURS];		/* annual avg fraction lighting energy reduction */
} ZONE;

typedef struct {	/* building shade data structure */
	char name[MAX_CHAR_UNAME+1];	/* building shade uname */
	double origin[NCOORDS];	/* origin coordinates (bldg sys) */
	double height;			/* height (ft) */
	double width;			/* width (ft) */
	double azm;				/* azimuth (bldg system) */
	double tilt;				/* tilt (0 to 180) (bldg system) */
	double vis_refl;			/* visible reflectance */
	double gnd_refl;			/* ground near shade reflectance */
	/* -------------------- derived quantities -------------------- */
	double vert[NCOORDS][NVERTS];	/* vertex coordinates (bldg sys) */
	double skylum[NPHS][NTHS];		/* shade lum (cd/ft2) from sky (each sun pos) */
	double sunlum[NPHS][NTHS];		/* shade lum (cd/ft2) from sun (each sun pos) */
	double ovrlum;					/* shade lum (cd/ft2) from overcast sky */
} BSHADE;

typedef struct {	/* building data structure */
	char name[MAX_CHAR_UNAME+1];	/* building uname */
	double lat;				/* bldg origin latitude */
	double lon;				/* bldg origin longitude */
	double alt;				/* bldg origin altitude (ft) */
	double azm;				/* bldg azimuth (deg from N) */
	double timezone;		/* site time zone (positive when WEST of prime meridian) */
	double atmtur[MONTHS];	/* monthly atmospheric turbidity */
	double atmmoi[MONTHS];	/* monthly atmospheric moisture */
	int nzones;						/* # of bldg lighting zones */
	ZONE *zone[MAX_BLDG_ZONES];		/* lighting zone struct pointers */
	int nbshades;					/* # of bldg shades */
	BSHADE *bshade[MAX_BLDG_SHADES];/* bldg shade struct pointers */
	/* -------------------- derived quantities -------------------- */
	double hillumskyc[NPHS];	/* exterior clear sky horiz illum (fc) sky component array */
	double hillumskyo[NPHS];	/* exterior overcast sky horiz illum (fc) sky component array */
	double hillumsunc[NPHS];	/* exterior clear sky horiz illum sun (fc) component array */
} BLDG;

typedef struct {	/* glass type library data structure */
	char name[MAX_CHAR_UNAME+1];	/* glass type uname */
	double vis_trans;	/* visible transmittance */
	double inside_refl;	/* inside visible reflectance */
	double cam1;			/* coef of transmission #1 */
	double cam2;			/* coef of transmission #2 */
	double cam3;			/* coef of transmission #3 */
	double cam4;			/* coef of transmission #4 */
	double cam9;			/* coef of transmission #9 */
	double E10hemi_trans;	/* Energy-10 hemispherical transmittance */
	double E10coef[4];		/* Energy-10 coefs of angular transmission */
	double W4hemi_trans;	/* Window 4 hemispherical transmittance */
	double W4vis_fit1;		/* Window 4 angular transmission curve fit coef #1 */
	double W4vis_fit2;		/* Window 4 angular transmission curve fit coef #2 */
	double EPlusDiffuse_Trans;	/* EnergyPlus diffuse visible transmittance */
	double EPlusCoef[6];		/* EnergyPlus coefs of angular transmission */
} GLASS;

typedef struct {	/* window shade type library data structure */
	char name[MAX_CHAR_UNAME+1];	/* window shade type uname */
	double vis_trans;	/* visible transmittance */
	double inside_refl;	/* inside visible reflectance */
} WSHADE;

typedef struct {	/* library data structure */
	char name[MAX_CHAR_UNAME+1];	/* library uname */
	int nglass;						/* # of library glass types */
	GLASS *glass[MAX_LIB_COMPS];	/* library glass struct pointers */
	int nwshades;					/* # of library wndo shade types */
	WSHADE *wshade[MAX_LIB_COMPS];	/* library wndo shade struct pointers */
} LIB;
