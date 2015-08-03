#ifndef GroundHeatExchangers_hh_INCLUDED
#define GroundHeatExchangers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GroundHeatExchangers {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE PARAMETER DEFINITIONS
	extern Real64 const hrsPerDay; // Number of hours in a day
	extern Real64 const hrsPerMonth; // Number of hours in month
	extern int const maxTSinHr; // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	//na

	// Types

	struct GLHEBase
	{
		// Members
		bool available; // need an array of logicals--load identifiers of available equipment
		bool on; // simulate the machine at it's operating part load ratio
		std::string Name; // user identifier
		int loopNum;
		int loopSideNum;
		int branchNum;
		int compNum;
		int inletNodeNum; // Node number on the inlet side of the plant
		int outletNodeNum; // Node number on the outlet side of the plant
		Real64 kGround; // Thermal conductivity of the ground		[W/(mK)]
		Real64 cpRhoGround; // Specific heat capacity of ground		[J/Kg/K]
		Real64 diffusivityGround; // Thermal diffisivity of the ground [m2/s]
		Real64 kPipe; // Thermal Conductivity of the U tube			[W/(mK)]
		Real64 cpPipe; // Specific heat of the U tube				[J/kg-K]
		Real64 rhoPipe; // Density of the U tube					[kg/m3]
		Real64 pipeOutDia; // Outer diameter of the Pipe			[m]
		Real64 pipeThick; // Thickness of the pipe wall				[m]
		Real64 designFlow; // Design volumetric flow rate			[m3/s]
		Real64 designMassFlow; // Design mass flow rate				[kg/s]
		Real64 tempGround; // The far feild temperature of the ground   [°C]
		Array1D< Real64 > QnMonthlyAgg; // Monthly aggregated normalized heat extraction/rejection rate [W/m]
		Array1D< Real64 > QnHr; // Hourly aggregated normalized heat extraction/rejection rate [W/m]
		Array1D< Real64 > QnSubHr; // Contains the subhourly heat extraction/rejection rate normalized
		// by the total active length of bore holes  [W/m]
		int prevHour;
		Real64 gReferenceRatio; // Reference ratio for developing g-functions [-]
		int NPairs; // Number of pairs of Lntts and Gfunc
		Array1D< Real64 > LNTTS; // natural log of Non Dimensional Time Ln(t/ts)
		Array1D< Real64 > GFNC; // G-function ( Non Dimensional temperature response factors)
		int AGG; // Minimum Hourly History required
		int SubAGG; // Minimum subhourly History
		Array1D_int LastHourN; // Stores the Previous hour's N for past hours
		// until the minimum subhourly history
		//loop topology variables
		Real64 boreholeTemp; // [°C]
		Real64 massFlowRate; // [kg/s]
		Real64 outletTemp; // [°C]
		Real64 inletTemp; // [°C]
		Real64 aveFluidTemp; // [°C]
		Real64 QGLHE; // [W] heat transfer rate
		bool myFlag;
		bool myEnvrnFlag;
		Real64 lastQnSubHr;
		Real64 HXResistance; // The thermal resistance of the GHX, (K per W/m)
		Real64 totalTubeLength; // The total length of pipe. NumBoreholes * BoreholeDepth OR Pi * Dcoil * NumCoils
		Real64 timeSS; // Steady state time
		Real64 timeSSFactor; // Steady state time factor for calculation

		// Default Constructor
		GLHEBase() :
			available( false ),
			on( false ),
			loopNum( 0 ),
			loopSideNum( 0 ),
			branchNum( 0 ),
			compNum( 0 ),
			inletNodeNum( 0 ),
			outletNodeNum( 0 ),
			kGround( 0.0 ),
			cpRhoGround( 0.0 ),
			kPipe( 0.0 ),
			cpPipe( 0.0 ),
			rhoPipe( 0.0 ),
			pipeOutDia( 0.0 ),
			pipeThick( 0.0 ),
			designFlow( 0.0 ),
			designMassFlow( 0.0 ),
			tempGround( 0.0 ),
			prevHour( 1 ),
			gReferenceRatio( 0.0 ),
			NPairs( 0 ),
			AGG( 0 ),
			SubAGG( 0 ),
			boreholeTemp( 0.0 ),
			massFlowRate( 0.0 ),
			outletTemp( 0.0 ),
			inletTemp( 0.0 ),
			aveFluidTemp( 0.0 ),
			QGLHE( 0.0 ),
			myFlag( true ),
			myEnvrnFlag( true ),
			lastQnSubHr( 0.0 ),
			HXResistance( 0.0 ),
			timeSS( 0.0 ),
			timeSSFactor( 0.0 )
		{}

		virtual void
		calcGFunctions()=0;

		void
		calcAggregateLoad();

		void
		updateGHX();

		void
		calcGroundHeatExchanger();

		virtual void
		initGLHESimVars()=0;

		virtual void
		calcHXResistance()=0;

		Real64
		interpGFunc( Real64 );

		virtual Real64
		getGFunc( Real64 )=0;

		virtual void
		getAnnualTimeConstant()=0;

		Real64
		getKAGrndTemp(
			Real64 const z,
			Real64 const dayOfYear,
			Real64 const aveGroundTemp,
			Real64 const aveGroundTempAmplitude,
			Real64 const phaseShift
		);

	};

	struct GLHEVert:GLHEBase
	{
		// Members
		Real64 maxFlowRate; // design nominal capacity of Pump
		int maxSimYears; // maximum length of simulation (years)
		int numBoreholes;
		Real64 boreholeLength;
		Real64 boreholeRadius;
		Real64 kGrout; // Grout thermal conductivity                [W/(mK)]
		Real64 UtubeDist; // Distance between the legs of the Utube    [m]
		bool runFlag;

		// Default Constructor
		GLHEVert() :
			maxFlowRate( 0.0 ),
			maxSimYears( 0 ),
			numBoreholes( 0 ),
			boreholeLength( 0.0 ),
			boreholeRadius( 0.0 ),
			kGrout( 0.0 ),
			UtubeDist( 0.0 ),
			runFlag( false )
		{}

		void
		calcGFunctions();

		void
		calcHXResistance();

		void
		initGLHESimVars();

		void
		getAnnualTimeConstant();

		Real64
		getGFunc(
			Real64 const time
		);

	};

	struct GLHESlinky:GLHEBase
	{
		// Members
		bool verticalConfig;	// HX Configuration Flag
		Real64 coilDiameter;	// Diameter of the slinky coils [m]
		Real64 coilPitch;		// Center-to-center slinky coil spacing [m]
		Real64 coilDepth;		// Average depth of the coil [m]
		Real64 trenchDepth;		// Trench depth from ground surface to trench bottom [m]
		Real64 trenchLength;	// Length of single trench [m]
		int numTrenches;		// Number of parallel trenches [m]
		Real64 trenchSpacing;	// Spacing between parallel trenches [m]
		int numCoils;			// Number of coils
		bool useGroundTempDataForKusuda; // Use Ground Temp Data Flag
		Real64 averageGroundTemp;
		Real64 averageGroundTempAmplitude;
		Real64 phaseShiftOfMinGroundTempDays;
		int monthOfMinSurfTemp;
		Real64 maxSimYears;
		Real64 minSurfTemp;
		Array1D< Real64 > X0;
		Array1D< Real64 > Y0;
		Real64 Z0;

		// Default Constructor
		GLHESlinky() :
			verticalConfig( false ),
			coilDiameter( 0.0 ),
			coilPitch( 0.0 ),
			coilDepth( 0.0 ),
			trenchDepth( 0.0 ),
			trenchLength( 0.0 ),
			numTrenches( 0 ),
			trenchSpacing( 0.0 ),
			numCoils( 0 ),
			useGroundTempDataForKusuda( false ),
			averageGroundTemp( 0.0 ),
			averageGroundTempAmplitude( 0.0 ),
			phaseShiftOfMinGroundTempDays( 0.0 ),
			monthOfMinSurfTemp( 0 ),
			maxSimYears( 0.0 ),
			minSurfTemp( 0.0 )
		{}

		void
		calcHXResistance();

		void
		calcGFunctions();

		void
		initGLHESimVars();

		void
		getAnnualTimeConstant();

		//Real64
		//interpGFunc(
		//Real64 const LnTTsVal // The value of LN(t/TimeSS) that a g-function
		//);

		Real64
		doubleIntegral(
			int const m,
			int const n,
			int const m1,
			int const n1,
			Real64 const t,
			int const I0,
			int const J0
		);

		Real64
		integral(
			int const m,
			int const n,
			int const m1,
			int const n1,
			Real64 const t,
			Real64 const eta,
			Real64 const J0
		);

		bool
		isEven(
			int const val
		);

		Real64
		distance(
			int const m,
			int const n,
			int const m1,
			int const n1,
			Real64 const eta,
			Real64 const theta
		);

		Real64
		distanceToFictRing(
			int const m,
			int const n,
			int const m1,
			int const n1,
			Real64 const eta,
			Real64 const theta
		);

		Real64
		distToCenter(
			int const m,
			int const n,
			int const m1,
			int const n1
		);

		Real64
		nearFieldResponseFunction(
			int const m,
			int const n,
			int const m1,
			int const n1,
			Real64 const eta,
			Real64 const theta,
			Real64 const t
		);

		Real64
		midFieldResponseFunction(
			int const m,
			int const n,
			int const m1,
			int const n1,
			Real64 const t
		);

		Real64
		getGFunc(
		Real64 const time
		);

	};

	// Object Data
	extern Array1D< GLHEVert > verticalGLHE; // Vertical GLHEs
	extern Array1D< GLHESlinky > slinkyGLHE; // Slinky GLHEs

	void
	SimGroundHeatExchangers(
		int const GLHETypeNum,
		std::string const & name,
		int & compIndex,
		bool const runFlag,
		bool const firstIteration,
		bool const initLoopEquip
	);

	void
	GetGroundHeatExchangerInput();

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // GroundHeatExchangers

} // EnergyPlus

#endif
