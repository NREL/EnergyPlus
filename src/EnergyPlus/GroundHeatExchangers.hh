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

#ifndef GroundHeatExchangers_hh_INCLUDED
#define GroundHeatExchangers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// JSON Headers
#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace GroundHeatExchangers {

	// Using/Aliasing
	using namespace GroundTemperatureManager;

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE PARAMETER DEFINITIONS
	extern Real64 const hrsPerDay; // Number of hours in a day
	extern Real64 const hrsPerMonth; // Number of hours in month
	extern int const maxTSinHr; // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	//na

	// Types

	struct  thermoPhysicialPropsStruct
	{
		// Destructor
		virtual ~thermoPhysicialPropsStruct(){}

		Real64 k;  // Thermal conductivity [W/m-K]
		Real64 rho; // Density [kg/m3]
		Real64 cp; // Specific heat [J/kg-K]
		Real64 rhoCp; // Specific heat capacity [J/kg-K]
		Real64 diffusivity; // Thermal diffusivity [m2/s]

		thermoPhysicialPropsStruct() :
			k( 0.0 ),
			rho( 0.0 ),
			cp( 0.0 ),
			rhoCp( 0.0 ),
			diffusivity( 0.0 )
		{}
	};

	struct pipePropsStruct : thermoPhysicialPropsStruct
	{
		// Destructor
		~pipePropsStruct(){}

		// Members
		Real64 outDia; // Outer diameter of the pipe [m]
		Real64 innerDia; // Inner diameter of the pipe [m]
		Real64 outRadius; // Outer radius of the pipe [m]
		Real64 innerRadius; // Inner radius of the pipe [m]
		Real64 thickness; // Thickness of the pipe wall [m]

		pipePropsStruct() :
			outDia( 0.0 ),
			thickness( 0.0 )
		{}
	};

	struct  GLHEVertPropsStruct
	{
		// Destructor
		~GLHEVertPropsStruct(){}

		// Members
		std::string name; // Name
		Real64 bhTopDepth; // Depth of top of borehole {m}
		Real64 bhLength; // Length of borehole from top of borehole {m}
		Real64 bhDiameter; // Diameter of borehole {m}
		thermoPhysicialPropsStruct grout; // Grout properties
		pipePropsStruct pipe; // Pipe properties
		Real64 bhUTubeDist; // U-tube, shank-to-shank spacking {m}

		GLHEVertPropsStruct () :
			bhTopDepth( 0.0 ),
			bhLength( 0.0 ),
			bhDiameter( 0.0 ),
			bhUTubeDist( 0.0 )
		{}
	};

	struct MyCartesian
	{
		// Destructor
		~MyCartesian(){}

		Real64 x;
		Real64 y;
		Real64 z;

		MyCartesian() :
			x( 0.0 ),
			y( 0.0 ),
			z( 0.0 )
		{}
	};

	struct GLHEVertSingleStruct
	{
		// Destructor
		~GLHEVertSingleStruct(){}

		// Members
		std::string name; // Name
		Real64 xLoc; // X-direction location {m}
		Real64 yLoc; // Y-direction location {m}
		Real64 dl_i; // Discretized bh length between points
		Real64 dl_ii;  // Discretized bh length between points
		Real64 dl_j;  // Discretized bh length between points
		std::shared_ptr < GLHEVertPropsStruct > props; // Properties
		std::vector < MyCartesian > pointLocations_i; // Discretized point locations for when computing temperature response of other boreholes on this bh
		std::vector < MyCartesian > pointLocations_ii; // Discretized point locations for when computing temperature response of this bh on itself
		std::vector < MyCartesian > pointLocations_j; // Discretized point locations for when other bh are computing the temperature response of this bh on themselves

		GLHEVertSingleStruct () :
			xLoc( 0.0 ),
			yLoc( 0.0 ),
			dl_i( 0.0 ),
			dl_ii( 0.0 ),
			dl_j( 0.0 )
		{}
	};

	struct GLHEVertArrayStruct
	{
		// Destructor
		~GLHEVertArrayStruct(){}

		// Members
		std::string name; // Name
		int numBHinXDirection; // Number of boreholes in X direction
		int numBHinYDirection; // Number of boreholes in Y direction
		Real64 bhSpacing; // Borehole center-to-center spacing {m}
		std::shared_ptr < GLHEVertPropsStruct > props; // Properties

		GLHEVertArrayStruct () :
			numBHinXDirection( 0 ),
			numBHinYDirection( 0 ),
			bhSpacing( 0.0 )
		{}
	};

	struct GLHEResponseFactorsStruct
	{
		// Destructor
		~GLHEResponseFactorsStruct(){}

		// Members
		std::string name; // Name
		int numBoreholes; // Number of boreholes
		int numGFuncPairs; // Number of g-function pairs
		Real64 gRefRatio; // Reference ratio of g-function set
		Real64 maxSimYears; // Maximum length of simulation in years
		Array1D< Real64 > time; // response time in seconds
		Array1D< Real64 > LNTTS; // natural log of Non Dimensional Time Ln(t/ts)
		Array1D< Real64 > GFNC; // G-function ( Non Dimensional temperature response factors)
		std::shared_ptr < GLHEVertPropsStruct > props; // Properties
		std::vector < std::shared_ptr < GLHEVertSingleStruct > > myBorholes; // Boreholes used by this response factors object

		GLHEResponseFactorsStruct() :
			numBoreholes( 0 ),
			numGFuncPairs( 0 ),
			gRefRatio( 0.0 ),
			maxSimYears( 0.0 )
		{}
	};

	struct GLHEBase : PlantComponent
	{
		// Destructor
		virtual
		~GLHEBase()
		{}

		// Members
		bool available; // need an array of logicals--load identifiers of available equipment
		bool on; // simulate the machine at it's operating part load ratio
		std::string name; // user identifier
		int loopNum;
		int loopSideNum;
		int branchNum;
		int compNum;
		int inletNodeNum; // Node number on the inlet side of the plant
		int outletNodeNum; // Node number on the outlet side of the plant
		thermoPhysicialPropsStruct soil;
		pipePropsStruct pipe;
		thermoPhysicialPropsStruct grout;
		std::shared_ptr < GLHEResponseFactorsStruct > myRespFactors;
		Real64 designFlow; // Design volumetric flow rate			[m3/s]
		Real64 designMassFlow; // Design mass flow rate				[kg/s]
		Real64 tempGround; // The far field temperature of the ground   [°C]
		Array1D< Real64 > QnMonthlyAgg; // Monthly aggregated normalized heat extraction/rejection rate [W/m]
		Array1D< Real64 > QnHr; // Hourly aggregated normalized heat extraction/rejection rate [W/m]
		Array1D< Real64 > QnSubHr; // Contains the sub-hourly heat extraction/rejection rate normalized by the total active length of bore holes  [W/m]
		int prevHour;
		int AGG; // Minimum Hourly History required
		int SubAGG; // Minimum sub-hourly History
		Array1D_int LastHourN; // Stores the Previous hour's N for past hours until the minimum sub-hourly history
		Real64 bhTemp; // [°C]
		Real64 massFlowRate; // [kg/s]
		Real64 outletTemp; // [°C]
		Real64 inletTemp; // [°C]
		Real64 aveFluidTemp; // [°C]
		Real64 QGLHE; // [W] heat transfer rate
		bool myFlag;
		bool myEnvrnFlag;
		bool gFunctionsExist;
		Real64 lastQnSubHr;
		Real64 HXResistance; // The thermal resistance of the GHX, (K per W/m)
		Real64 totalTubeLength; // The total length of pipe. NumBoreholes * BoreholeDepth OR Pi * Dcoil * NumCoils
		Real64 timeSS; // Steady state time
		Real64 timeSSFactor; // Steady state time factor for calculation
		std::shared_ptr< BaseGroundTempsModel > groundTempModel;

		GLHEBase() :
			available( false ),
			on( false ),
			loopNum( 0 ),
			loopSideNum( 0 ),
			branchNum( 0 ),
			compNum( 0 ),
			inletNodeNum( 0 ),
			outletNodeNum( 0 ),
			designFlow( 0.0 ),
			designMassFlow( 0.0 ),
			tempGround( 0.0 ),
			prevHour( 1 ),
			AGG( 0 ),
			SubAGG( 0 ),
			bhTemp( 0.0 ),
			massFlowRate( 0.0 ),
			outletTemp( 0.0 ),
			inletTemp( 0.0 ),
			aveFluidTemp( 0.0 ),
			QGLHE( 0.0 ),
			myFlag( true ),
			myEnvrnFlag( true ),
			gFunctionsExist( false ),
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

		inline bool
		isEven( int const val );

		Real64
		interpGFunc( Real64 );

		void onInitLoopEquip( const PlantLocation & calledFromLocation ) override;

		void simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad, bool const RunFlag ) override;

		static PlantComponent * factory( int const objectType, std::string objectName );

		virtual Real64
		getGFunc( Real64 )=0;

		virtual void
		initGLHESimVars()=0;

		virtual Real64
		calcHXResistance()=0;

		virtual void
		getAnnualTimeConstant()=0;

	};

	struct GLHEVert : GLHEBase
	{
		// Destructor
		~GLHEVert(){}

		// Members
		Real64 bhDiameter; // Diameter of borehole {m}
		Real64 bhRadius; // Radius of borehole {m}
		Real64 bhLength; // Length of borehole {m}
		Real64 bhUTubeDist; // Distance between u-tube legs {m}

		// Parameters for the multipole method
		Real64 theta_1;
		Real64 theta_2;
		Real64 theta_3;
		Real64 sigma;

		nlohmann::json myCacheData;

		std::vector< Real64 > GFNC_shortTimestep;
		std::vector< Real64 > LNTTS_shortTimestep;

		GLHEVert() :
			bhDiameter( 0.0 ),
			bhRadius( 0.0 ),
			bhLength( 0.0 ),
			bhUTubeDist( 0.0 ),
			theta_1( 0.0 ),
			theta_2( 0.0 ),
			theta_3( 0.0 ),
			sigma( 0.0 )
		{}

		std::vector< Real64 >
		distances(
			MyCartesian const & point_i,
			MyCartesian const & point_j
		);

		Real64
		calcResponse(
			std::vector< Real64 > const & dists,
			Real64 const & currTime
		);

		Real64
		integral(
			MyCartesian const & point_i,
			std::shared_ptr< GLHEVertSingleStruct > const & bh_j,
			Real64 const & currTime
		);

		Real64
		doubleIntegral(
			std::shared_ptr< GLHEVertSingleStruct > const & bh_i,
			std::shared_ptr< GLHEVertSingleStruct > const & bh_j,
			Real64 const & currTime
		);

		void
		calcShortTimestepGFunctions();

		void
		calcGFunctions();

		Real64
		calcHXResistance();

		void
		initGLHESimVars();

		void
		getAnnualTimeConstant();

		Real64
		getGFunc(
			Real64 const time
		);

		void
		makeCache();

		void
		readCache();

		void
		writeCache();

		Real64
		calcBHAverageResistance();

		Real64
		calcBHTotalInternalResistance();

		Real64
		calcBHGroutResistance();

		Real64
		calcPipeConductionResistance();

		Real64
		calcPipeConvectionResistance();

		Real64
		frictionFactor(
			Real64 const reynoldsNum
		);

		Real64
		calcPipeResistance();

		void
		combineShortAndLongTimestepGFunctions();

	};

	struct GLHESlinky : GLHEBase
	{

		// Destructor
		~GLHESlinky(){}

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
		int monthOfMinSurfTemp;
		Real64 maxSimYears;
		Real64 minSurfTemp;
		Array1D< Real64 > X0;
		Array1D< Real64 > Y0;
		Real64 Z0;

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
			monthOfMinSurfTemp( 0 ),
			maxSimYears( 0.0 ),
			minSurfTemp( 0.0 )
		{}

		Real64
		calcHXResistance();

		void
		calcGFunctions();

		void
		initGLHESimVars();

		void
		getAnnualTimeConstant();

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

	void
	clear_state();

	void
	GetGroundHeatExchangerInput();

	std::shared_ptr < GLHEResponseFactorsStruct >
	BuildAndGetResponseFactorObjectFromArray(
		std::shared_ptr < GLHEVertArrayStruct > const & arrayObjectPtr
	);

	std::shared_ptr < GLHEResponseFactorsStruct >
	BuildAndGetResponseFactorsObjectFromSingleBHs(
		std::vector < std::shared_ptr < GLHEVertSingleStruct > > const & singleBHsForRFVect
	);

	void
	SetupBHPointsForResponseFactorsObject(
		std::shared_ptr < GLHEResponseFactorsStruct > & thisRF
	);

	std::shared_ptr < GLHEResponseFactorsStruct >
	GetResponseFactor(
		std::string const & objectName
	);

	std::shared_ptr < GLHEVertSingleStruct >
	GetSingleBH(
		std::string const & objectName
	);

	std::shared_ptr < GLHEVertPropsStruct >
	GetVertProps(
		std::string const & objectName
	);

	std::shared_ptr < GLHEVertArrayStruct >
	GetVertArray(
		std::string const & objectName
	);

	std::vector< Real64 >
	TDMA(
		std::vector< Real64 > a,
		std::vector< Real64 > b,
		std::vector< Real64 > c,
		std::vector< Real64 > d
	);

	// Object Data
	extern std::vector < GLHEVert > verticalGLHE; // Vertical GLHEs
	extern std::vector < GLHESlinky > slinkyGLHE; // Slinky GLHEs
	extern std::vector < std::shared_ptr < GLHEVertArrayStruct > > vertArraysVector; // Vertical Arrays
	extern std::vector < std::shared_ptr < GLHEVertPropsStruct > > vertPropsVector; // Vertical Properties
	extern std::vector < std::shared_ptr < GLHEResponseFactorsStruct > > responseFactorsVector; // Vertical Response Factors
	extern std::vector < std::shared_ptr< GLHEVertSingleStruct > > singleBoreholesVector; // Vertical Single Boreholes

} // GroundHeatExchangers

} // EnergyPlus

#endif
