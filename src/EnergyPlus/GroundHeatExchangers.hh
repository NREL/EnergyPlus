// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef GroundHeatExchangers_hh_INCLUDED
#define GroundHeatExchangers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

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

	struct GLHEBase : PlantComponent
	{
		// Destructor
		virtual
		~GLHEBase()
		{}

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
		std::shared_ptr< BaseGroundTempsModel > groundTempModel;

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

		void onInitLoopEquip( const PlantLocation & calledFromLocation ) override;

		void simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad, bool const RunFlag ) override;

		static PlantComponent * factory( int const objectType, std::string objectName );

	};

	struct GLHEVert:GLHEBase
	{
		// Destructor
		~GLHEVert(){}

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
	clear_state();

	void
	GetGroundHeatExchangerInput();

} // GroundHeatExchangers

} // EnergyPlus

#endif
