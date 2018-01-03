// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef PVWatts_hh_INCLUDED
#define PVWatts_hh_INCLUDED

// C++ Headers
#include <string>
#include <map>
#include <memory>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataSurfaces.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace PVWatts {

	const Real64 AOI_MIN(0.5);
	const Real64 AOI_MAX(89.5);
	const Real64 DTOR(DataGlobals::DegToRadians);
	enum RADMODE {DN_DF, DN_GH, GH_DF, POA_R, POA_P};
	const Real64 SMALL(1e-6);

	enum class ModuleType {
		STANDARD,
		PREMIUM,
		THIN_FILM,
	};

	enum class ArrayType {
		FIXED_OPEN_RACK,
		FIXED_ROOF_MOUNTED,
		ONE_AXIS,
		ONE_AXIS_BACKTRACKING,
		TWO_AXIS,
	};

	enum class GeometryType {
		TILT_AZIMUTH,
		SURFACE,
	};

	struct DCPowerOutput {
		Real64 poa; // Plane of array irradiance
		Real64 tpoa; // Transmitted plane of array irradiance
		Real64 pvt; // PV Cell temperature
		Real64 dc; // DC power output
	};

	struct IrradianceOutput {
		Real64 solazi;
		Real64 solzen;
		Real64 solalt;
		Real64 aoi;
		Real64 stilt;
		Real64 sazi;
		Real64 rot;
		Real64 btd;
		Real64 ibeam;
		Real64 iskydiff;
		Real64 ignddiff;
		int sunup;
	};

	// From here down is from the SAM Simulation core library
	
	class pvwatts_celltemp
	{
		/*
		 This class was converted from the original PVWatts subroutine
		 to store the previous irradiance and cell temp to make the model
		 give consistent results with the online PVWatts V1.  Previously
		 all calculations were done over the course of a day, so the
		 previous module/sun values were saved, but in the single time stamp
		 version, these changes were not tracked.  apd 2/24/2012

		 Defines function to calculate cell temperature, changed 8/22/2007 to
		 work with single time stamp data, also see pvsubs2.c

		 This function was converted from a PVFORM version 3.3 subroutine
		 c     this routine estimates the array temperature given the poa radiation,
		 c     ambient temperature, and wind speed.  it uses an advanced cell temp
		 c     model developed by m fuentes at snla.  if the poa insolation is eq
		 c     zero then set cell temp = 999.
		 c
		 passed variables:
		 inoct = installed nominal operating cell temperature (deg K)
		 height = average array height (meters)
		 poa2 = plane of array irradiances (W/m2)
		 ws2 = wind speeds (m/s)
		 ambt2 = ambient temperatures (deg C)

		 c  local variables :
		 c     absorb = absorbtivity
		 c     backrt = ratio of actual backside heat xfer to theoretical of rack mount
		 c     boltz = boltzmann's constant
		 c     cap = capacitance per unit area of module
		 c     capo = capacitance per unit area of rack mounted module
		 c     conair = conductivity of air
		 c     convrt = ratio of total convective heat xfer coef to topside hxc
		 c     denair = density of air
		 c     dtime = time step
		 c     eigen = product of eigen value and time step
		 c     emmis = emmisivity
		 ex = ?
		 c     grashf = grashoffs number
		 c     hconv = convective coeff of module (both sides)
		 c     hforce = forced convective coeff of top side
		 c     hfree = free convective coeff of top side
		 c     hgrnd = radiative heat xfer coeff from module to ground
		 hsky = ?
		 c     iflagc = flag to check if routine has been executed
		 c     reynld = reynolds number
		 c     suun = insolation at start of time step
		 c     suno = previous hours insolation
		 c     tamb = ambient temp
		 c     tave = average of amb and cell temp
		 c     tgrat = ratio of grnd temp above amb to cell temp above amb
		 c     tgrnd = temperature of ground
		 c     tmod = computed cell temp
		 c     tmodo = cell temp for previous time step
		 c     tsky = sky temp
		 c     visair = viscosity of air
		 c     windmd = wind speed at module height
		 c     xlen = hydrodynamic length of module              */

	private:
		int j;
		double height, inoct;
		double absorb,backrt,boltz,cap,capo,conair,convrt,denair;
		double eigen,emmis,grashf,hconv,hforce,hfree,hgrnd,reynld,suun;
		double suno,tamb,tave,tgrat,tgrnd,tmod,tmodo,tsky,visair,windmd,xlen;
		double hsky,ex;
		const double &dtime;
	public:
		pvwatts_celltemp( double _inoct, double _height, const double& _dTimeHrs);
		double operator() ( double poa2, double ws2, double ambt2, double fhconv = 1.0 );
		void set_last_values( double Tc, double poa );
	};

	struct poaDecompReq {
		poaDecompReq() : i(0), dayStart(0), stepSize(1), stepScale('h'), doy(-1) {}
		size_t i; // Current time index
		size_t dayStart; // time index corresponding to the start of the current day
		double stepSize;
		char stepScale; // indicates whether time steps are hours (h) or minutes (m)
		double* POA; // Pointer to entire POA array (will have size 8760 if time step is 1 hour)
		double* inc; // Pointer to angle of incident array (same size as POA)
		double* tilt; // Pointer to angle of incident array (same size as POA)
		double* zen; // Pointer to angle of incident array (same size as POA)
		double* exTer; // Pointer to angle of incident array (same size as POA)
		double tDew;
		int doy;
		double elev;
	};

	class irrad
	{
	private:
		int year, month, day, hour;
		double minute, delt;

		double lat, lon, tz;
		int radmode, skymodel, track;
		double gh, dn, df, wfpoa, alb;
		double tilt, sazm, rlim, gcr;
		bool en_backtrack;
		double sun[9], angle[5], poa[3], diffc[3];
		int tms[3];
		double ghi;

		poaDecompReq* poaAll;

	public:

		irrad();
		int check();

		// if delt_hr is less than zero, do not interpolate sunrise and sunset hours
#define IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET (-1.0)
		void set_time( int year, int month, int day, int hour, double minute, double delt_hr );
		void set_location( double lat, double lon, double tz);
		//skymodel: 0 is isotropic, 1 is hdkr, 2 is perez
		void set_sky_model( int skymodel, double albedo );
		void set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr );
		void set_beam_diffuse( double beam, double diffuse );
		void set_global_beam( double global, double beam );
		void set_global_diffuse(double global, double diffuse);
		void set_poa_reference( double poa, poaDecompReq* );
		void set_poa_pyranometer( double poa, poaDecompReq* );

		int calc();

		void get_sun( double *solazi,
					 double *solzen,
					 double *solelv,
					 double *soldec,
					 double *sunrise,
					 double *sunset,
					 int *sunup,
					 double *eccfac,
					 double *tst,
					 double *hextra );
		void get_angles( double *aoi,
						double *surftilt,
						double *surfazi,
						double *axisrot,
						double *btdiff );
		void get_poa( double *beam, double *skydiff, double *gnddiff,
					 double *isotrop, double *circum, double *horizon );
		void get_irrad (double *ghi, double *dni, double *dhi);
		double get_ghi();
		double get_sunpos_calc_hour();
	};

	double shade_fraction_1x( double solazi, double solzen,
							 double axis_tilt, double axis_azimuth,
							 double gcr, double rotation );

	void diffuse_reduce(double solzen,
						double stilt,
						double Gb_nor,
						double Gd_poa,
						double gcr,
						double phi0, // mask angle (degrees)
						double alb,
						double nrows,

						double &reduced_skydiff,
						double &Fskydiff,
						double &reduced_gnddiff,
						double &Fgnddiff );

	double iam( double theta_deg, bool ar_glass ); // incidence angle modifier factor relative to normal incidence

	// After this is E+ code again.

	class PVWattsGenerator
	{
	private:

		enum AlphaFields {
			NAME = 1,
			VERSION = 2,
			MODULE_TYPE = 3,
			ARRAY_TYPE = 4,
			GEOMETRY_TYPE = 5,
			SURFACE_NAME = 6,
		};

		enum NumFields {
			DC_SYSTEM_CAPACITY = 1,
			SYSTEM_LOSSES = 2,
			TILT_ANGLE = 3,
			AZIMUTH_ANGLE = 4,
			GROUND_COVERAGE_RATIO = 5,
		};

		std::string m_name;
		Real64 m_dcSystemCapacity;
		ModuleType m_moduleType;
		ArrayType m_arrayType;
		Real64 m_systemLosses;
		GeometryType m_geometryType;
		Real64 m_tilt;
		Real64 m_azimuth;
		int m_surfaceNum;
		Real64 m_groundCoverageRatio;
		std::unique_ptr<pvwatts_celltemp> m_tccalc;

		Real64 m_gamma;
		bool m_useARGlass;
		int m_trackMode;
		Real64 m_inoct;
		int m_shadeMode1x;

	public:
		static PVWattsGenerator createFromIdfObj(int objNum);

		PVWattsGenerator(const std::string &name, const Real64 dcSystemCapacity, ModuleType moduleType, ArrayType arrayType, Real64 systemLosses=0.14, GeometryType geometryType=GeometryType::TILT_AZIMUTH, Real64 tilt=20.0, Real64 azimuth=180.0, size_t surfaceNum=0, Real64 groundCoverageRatio=0.4);

		Real64 getDCSystemCapacity();
		ModuleType getModuleType();
		ArrayType getArrayType();
		Real64 getSystemLosses();
		GeometryType getGeometryType();
		Real64 getTilt();
		Real64 getAzimuth();
		DataSurfaces::SurfaceData& getSurface();
		Real64 getGroundCoverageRatio();

		void calc();

		IrradianceOutput processIrradiance(int year, int month, int day, int hour, Real64 minute, Real64 ts_hour, Real64 lat, Real64 lon, Real64 tz, Real64 dn, Real64 df, Real64 alb );

		DCPowerOutput powerout(Real64 &shad_beam, Real64 shad_diff, Real64 dni, Real64 alb, Real64 wspd, Real64 tdry, IrradianceOutput& irr_st);

	};

	extern std::map<int, PVWattsGenerator> PVWattsGenerators;

	PVWattsGenerator& GetOrCreatePVWattsGenerator(std::string const & GeneratorName);

	void SimPVWattsGenerator(std::string const & GeneratorName, bool const RunFlag);

	//static void get_vertices( double axis_tilt, double axis_azimuth, double gcr, double vertices[3][4][3], double rotation);

	//static double vec_dot(double a[3], double b[3]);

	//static void vec_cross(double a[3], double b[3], double result[3]);

	//static void vec_diff( double a[3], double b[3], double result[3] );

	void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);

	void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5]);

	void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

	void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );

	void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

	void poaDecomp( double wfPOA, double angle[], double sun[], double alb, poaDecompReq* pA, double &dn, double &df, double &gh, double poa[3], double diffc[3]);

	//static void sun_unit( double sazm, double szen, double sun[3] );

	double iam_nonorm( double theta_deg, bool ar_glass );  // non-normalized cover loss (typically use one above!)

	double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double &dn);

	void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double alt, int doy, double &dn);

	//static int julian(int yr,int month,int day);

	//static int day_of_year( int month, int day_of_month );

	double backtrack( double solazi, double solzen,
					 double axis_tilt, double axis_azimuth,
					 double rotlim, double gcr, double rotation_ideal);

	double GTI_DIRINT( const double poa[3], const double inc[3], double zen, double tilt, double ext, double alb, int doy, double tDew, double elev, double& dnOut, double& dfOut, double& ghOut, double poaCompOut[3]);

	double Min( double v1, double v2);

	double Max( double v1, double v2);

	double transmittance( double theta1_deg, /* incidence angle of incoming radiation (deg) */
						 double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
						 double n_incoming, /* refractive index of incoming material, typically n_air = 1.0 */
						 double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law (set to zero to skip bougeur's law */
						 double l_thick,  /* material thickness (set to zero to skip Bouguer's law */
						 double *_theta2_deg = 0 ); /* thickness of cover material (m), usually 2 mm for typical module */

}

}

#endif
