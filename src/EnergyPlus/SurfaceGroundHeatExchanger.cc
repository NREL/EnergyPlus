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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <SurfaceGroundHeatExchanger.hh>
#include <BranchNodeConnections.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SurfaceGroundHeatExchanger {

	// Module containing the routines dealing with surface/panel ground heat exchangers

	// MODULE INFORMATION:
	//       AUTHOR         Simon Rees
	//       DATE WRITTEN   August 2002
	//       MODIFIED       Brent Griffith, Sept 2010, plant upgrades
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate hydronic Surface Ground Heat
	// Exchangers. This includes pavement surfaces with embedded pipes for snow-
	// melting or heat rejection from hybrid ground source heat pump systems.
	// The heat exchanger may be gound coupled or not. In the latter case the
	// bottom surface is exposed to the wind but not solar gains.

	// METHODOLOGY EMPLOYED:
	// This model is based on the QTF formulation of heat transfer through
	// building elements with embedded heat sources/sinks. The model uses
	// a heat exchanger analogy to relate the inlet fluid temperature to the
	// net heat transfer rate and consequently outlet temperature. The model
	// is entirely passive i.e. it does not set any flow rates or incorporate
	// any controls. In order to deal with the non-linear boundary conditions
	// at the top surface due to the presence of ice/snow fluxes have to be
	// calculated by the QTF model and temperature calculated from the surface
	// heat balance. This requires some iteration.
	// Note: top surface variables correspond to 'outside' variables in standard
	// CTF/QTF definition. Bottom surface variables correspond to 'inside' variables.

	// REFERENCES:
	// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
	//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
	//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
	//   Engineering.
	// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
	//   of Wisconsin-Madison.

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::KelvinConv;
	using namespace DataLoopNode;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	Real64 const SmallNum( 1.0e-30 ); // Very small number to avoid div0 errors
	Real64 const StefBoltzmann( 5.6697e-08 ); // Stefan-Boltzmann constant
	Real64 const SurfaceHXHeight( 0.0 ); // Surface Height above ground -- used in height dependent calcs.
	static std::string const BlankString;

	int const SurfCond_Ground( 1 );
	int const SurfCond_Exposed( 2 );

namespace loc {
	int const MaxCTFTerms( 19 ); // Maximum number of CTF terms allowed to still allow stability //Note Duplicate of DataHeatBalance::MaxCTFTerms to avoid static initialization order bug: Keep them in sync
} // loc

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// utility variables initialized once
	bool NoSurfaceGroundTempObjWarning( true ); // This will cause a warning to be issued if no "surface" ground
	// temperature object was input.
	// Utility variables - initialized for each instance of a surface GHE
	//int ConstructionNum( 0 ); // construction index number
	//int TopRoughness( 0 ); // roughness of top layer
	//int BtmRoughness( 0 ); // roughness of bottom layer
	Real64 nsvInletTemp( 0.0 ); // water inlet temperature
	Real64 nsvOutletTemp( 0.0 ); // water outlet temperature
	Real64 FlowRate( 0.0 ); // water mass flow rate
	Real64 TopSurfTemp( 0.0 ); // Top  surface temperature
	Real64 BtmSurfTemp( 0.0 ); // Bottom  surface temperature
	Real64 TopSurfFlux( 0.0 ); // Top  surface heat flux
	Real64 BtmSurfFlux( 0.0 ); // Bottom  surface heat flux
	Real64 SourceFlux( 0.0 ); // total heat transfer rate, Watts
	Real64 SourceTemp( 0.0 ); // total heat transfer rate, Watts
	Real64 nsvSurfaceArea( 0.0 ); // surface GHE surface area
	Real64 TopThermAbs( 0.0 ); // Thermal absortivity of top layer
	Real64 BtmThermAbs( 0.0 ); // Thermal absortivity of bottom layer
	Real64 TopSolarAbs( 0.0 ); // Solar absortivity of top layer
	Array1D_bool CheckEquipName;

	// weather data records updated every zone time step
	Real64 PastBeamSolarRad( 0.0 ); // Previous beam normal solar irradiance
	Real64 PastSolarDirCosVert( 0.0 ); // Previous vertical component of solar normal
	Real64 PastDifSolarRad( 0.0 ); // Previous sky diffuse solar horizontal irradiance
	Real64 PastGroundTemp( 0.0 ); // Previous ground temperature
	bool PastIsRain( false ); // Previous Surfaces are wet for this time interval
	bool PastIsSnow( false ); // Previous Snow on the ground for this time interval
	Real64 PastOutBaroPress( 0.0 ); // Previous outdoor air barometric pressure
	Real64 PastOutDryBulbTemp( 0.0 ); // Previous outdoor air dry bulb temperature
	Real64 PastOutHumRat( 0.0 ); // Previous outdoor air humidity ratio
	Real64 PastOutAirDensity( 0.0 ); // Previous outdoor air density
	Real64 PastOutWetBulbTemp( 0.0 ); // Previous outdoor air wet bulb temperature
	Real64 PastOutDewPointTemp( 0.0 ); // Previous outdoor dewpoint temperature
	Real64 PastSkyTemp( 0.0 ); // Previous sky temperature
	Real64 PastWindSpeed( 0.0 ); // Previous outdoor air wind speed
	Real64 PastCloudFraction( 0.0 ); // Previous Fraction of sky covered by clouds

	// getinput flag
	bool GetInputFlag( true );

	// time keeping variables used for keeping track of average flux over each time step
	Array1D< Real64 > QRadSysSrcAvg; // Average source over the time step
	Array1D< Real64 > LastSysTimeElapsed; // record of system time
	Array1D< Real64 > LastTimeStepSys; // previous time step size

	// SUBROUTINE SPECIFICATIONS FOR MODULE PlantSurfaceGroundHeatExchangers

	// Object Data
	Array1D< SurfaceGroundHeatExchangerData > SurfaceGHE;

	PlantComponent * SurfaceGroundHeatExchangerData::factory( int const EP_UNUSED(objectType), std::string const objectName ) {
		if ( GetInputFlag ) {
			GetSurfaceGroundHeatExchanger();
			GetInputFlag = false;
		}
		// Now look for this particular pipe in the list
		for ( auto & ghx : SurfaceGHE ) {
			if ( ghx.Name == objectName ) {
				return &ghx;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "Surface Ground Heat Exchanger: Error getting inputs for pipe named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}

	void SurfaceGroundHeatExchangerData::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ), bool const FirstHVACIteration, Real64 & EP_UNUSED( CurLoad ), bool const EP_UNUSED( RunFlag ) ) {
		this->InitSurfaceGroundHeatExchanger();
		this->CalcSurfaceGroundHeatExchanger( FirstHVACIteration );
		this->UpdateSurfaceGroundHeatExchngr();
		this->ReportSurfaceGroundHeatExchngr();
	}

	void
	GetSurfaceGroundHeatExchanger()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for hydronic Surface Ground Heat Exchangers
		// from the user input file.  This will contain all of the information
		// needed to define and simulate the surface.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Construct;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using DataEnvironment::GroundTemp_Surface;
		using DataEnvironment::GroundTemp_SurfaceObjInput;
		using General::RoundSigDigits;
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static bool ErrorsFound( false ); // Set to true if errors in input,
		// fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		//  INTEGER                        :: NumFluids            ! number of fluids in sim.

		// Initializations and allocations
		cCurrentModuleObject = "GroundHeatExchanger:Surface";
		int NumOfSurfaceGHEs = GetNumObjectsFound( cCurrentModuleObject );
		// allocate data structures
		if ( allocated( SurfaceGHE ) ) SurfaceGHE.deallocate();

		SurfaceGHE.allocate( NumOfSurfaceGHEs );
		CheckEquipName.dimension( NumOfSurfaceGHEs, true );

		// initialize data structures
		// surface data
		// Obtain all of the user data related to the surfaces...
		for ( Item = 1; Item <= NumOfSurfaceGHEs; ++Item ) {

			// get the input data
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );

			// General user input data
			SurfaceGHE( Item ).Name = cAlphaArgs( 1 );
			SurfaceGHE( Item ).ConstructionName = cAlphaArgs( 2 );
			SurfaceGHE( Item ).ConstructionNum = FindItemInList( cAlphaArgs( 2 ), Construct );

			if ( SurfaceGHE( Item ).ConstructionNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// Error checking for surfaces, zones, and construction information
			if ( ! Construct( SurfaceGHE( Item ).ConstructionNum ).SourceSinkPresent ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Construction must have internal source/sink and use Construction:InternalSource object" );
				ErrorsFound = true;
			}

			//get inlet node data
			SurfaceGHE( Item ).InletNode = cAlphaArgs( 3 );
			SurfaceGHE( Item ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( SurfaceGHE( Item ).InletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// get outlet node data
			SurfaceGHE( Item ).OutletNode = cAlphaArgs( 4 );
			SurfaceGHE( Item ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( SurfaceGHE( Item ).OutletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Condenser Water Nodes" );

			// tube data
			SurfaceGHE( Item ).TubeDiameter = rNumericArgs( 1 );
			SurfaceGHE( Item ).TubeCircuits = rNumericArgs( 2 );
			SurfaceGHE( Item ).TubeSpacing = rNumericArgs( 3 );

			if ( rNumericArgs( 2 ) == 0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 3 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 3 ) + '=' + RoundSigDigits( rNumericArgs( 3 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

			// surface geometry data
			SurfaceGHE( Item ).SurfaceLength = rNumericArgs( 4 );
			SurfaceGHE( Item ).SurfaceWidth = rNumericArgs( 5 );
			if ( rNumericArgs( 4 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( rNumericArgs( 4 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 5 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + '=' + RoundSigDigits( rNumericArgs( 5 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

			// get lower b.c. type
			if ( SameString( cAlphaArgs( 5 ), "GROUND" ) ) {
				SurfaceGHE( Item ).LowerSurfCond = SurfCond_Ground;
			} else if ( SameString( cAlphaArgs( 5 ), "EXPOSED" ) ) {
				SurfaceGHE( Item ).LowerSurfCond = SurfCond_Exposed;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Only \"Ground\" or \"Exposed\" is allowed." );
				ErrorsFound = true;
			}

		} // end of input loop

		// final error check
		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		// Set up the output variables
		for ( Item = 1; Item <= NumOfSurfaceGHEs; ++Item ) {
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", SurfaceGHE( Item ).HeatTransferRate, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Surface Heat Transfer Rate [W]", SurfaceGHE( Item ).SurfHeatTransferRate, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Energy [J]", SurfaceGHE( Item ).Energy, "Plant", "Sum", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", SurfaceGHE( Item ).MassFlowRate, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", SurfaceGHE( Item ).InletTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", SurfaceGHE( Item ).OutletTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Top Surface Temperature [C]", SurfaceGHE( Item ).TopSurfaceTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Bottom Surface Temperature [C]", SurfaceGHE( Item ).BtmSurfaceTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Top Surface Heat Transfer Energy per Area [J/m2]", SurfaceGHE( Item ).TopSurfaceFlux, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Bottom Surface Heat Transfer Energy per Area [J/m2]", SurfaceGHE( Item ).BtmSurfaceFlux, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Surface Heat Transfer Energy [J]", SurfaceGHE( Item ).SurfEnergy, "Plant", "Sum", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Source Temperature [C]", SurfaceGHE( Item ).SourceTemp, "Plant", "Average", SurfaceGHE( Item ).Name );

		}

		if ( NoSurfaceGroundTempObjWarning ) {
			if ( ! GroundTemp_SurfaceObjInput ) {
				ShowWarningError( "GetSurfaceGroundHeatExchanger: No \"Site:GroundTemperature:Shallow\" were input." );
				ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp_Surface, 1 ) + ") will be used." );
			}
			NoSurfaceGroundTempObjWarning = false;
		}

	}

	void
	SurfaceGroundHeatExchangerData::InitSurfaceGroundHeatExchanger()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine Resets the elements of the data structure as necessary
		// at the first HVAC iteration of each time step. The weather and QTF data
		// is initialized once only.

		// METHODOLOGY EMPLOYED:
		// Check flags and update data structure

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::BeginEnvrnFlag;
		using namespace DataEnvironment;
		using DataLoopNode::Node;
		using DataHeatBalance::TotConstructs;
		using DataHeatBalance::Construct;
		using DataHeatBalance::Material;
		using InputProcessor::SameString;
		using DataPlant::TypeOf_GrndHtExchgSurface;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DesignVelocity( 0.5 ); // Hypothetical design max pipe velocity [m/s]
		static std::string const RoutineName( "InitSurfaceGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 DesignFlow; // Hypothetical design flow rate
		int Cons; // construction counter
		int LayerNum; // material layer number for bottom
		Real64 OutDryBulb; // Height Dependent dry bulb.
		Real64 rho; // local fluid density
		bool errFlag;

		// Init more variables
		if ( this->MyFlag ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_GrndHtExchgSurface, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, _, _, _, _, _, errFlag );

			if ( errFlag ) {
				ShowFatalError( "InitSurfaceGroundHeatExchanger: Program terminated due to previous condition(s)." );
			}
			rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, constant_zero, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
			this->DesignMassFlowRate = Pi / 4.0 * pow_2( this->TubeDiameter ) * DesignVelocity * rho * this->TubeCircuits;
			InitComponentNodes( 0.0, this->DesignMassFlowRate, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );
			RegisterPlantCompDesignFlow( this->InletNodeNum, this->DesignMassFlowRate / rho );

			this->MyFlag = false;
		}

		// get QTF data - only once
		if ( this->InitQTF ) {
			for ( Cons = 1; Cons <= TotConstructs; ++Cons ) {
				if ( SameString( Construct( Cons ).Name, this->ConstructionName ) ) {
					// some error checking ??
					// CTF stuff
					LayerNum = Construct( Cons ).TotLayers;
					this->NumCTFTerms = Construct( Cons ).NumCTFTerms;
					this->CTFin = Construct( Cons ).CTFInside; // Z coefficents
					this->CTFout = Construct( Cons ).CTFOutside; // X coefficents
					this->CTFcross = Construct( Cons ).CTFCross; // Y coefficents
					this->CTFflux( {1,_} ) = Construct( Cons ).CTFFlux; // F & f coefficents
					// QTF stuff
					this->CTFSourceIn = Construct( Cons ).CTFSourceIn; // Wi coefficents
					this->CTFSourceOut = Construct( Cons ).CTFSourceOut; // Wo coefficents
					this->CTFTSourceOut = Construct( Cons ).CTFTSourceOut; // y coefficents
					this->CTFTSourceIn = Construct( Cons ).CTFTSourceIn; // x coefficents
					this->CTFTSourceQ = Construct( Cons ).CTFTSourceQ; // w coefficents
					this->ConstructionNum = Cons;
					// surface properties
					this->BtmRoughness = Material( Construct( Cons ).LayerPoint( LayerNum ) ).Roughness;
					this->TopThermAbs = Material( Construct( Cons ).LayerPoint( LayerNum ) ).AbsorpThermal;
					this->TopRoughness = Material( Construct( Cons ).LayerPoint( 1 ) ).Roughness;
					this->TopThermAbs = Material( Construct( Cons ).LayerPoint( 1 ) ).AbsorpThermal;
					this->TopSolarAbs = Material( Construct( Cons ).LayerPoint( 1 ) ).AbsorpSolar;
				}
			}
			// set one-time flag
			this->InitQTF = false;
		}

		if ( this->MyEnvrnFlag && BeginEnvrnFlag ) {
			OutDryBulb = OutDryBulbTempAt( SurfaceHXHeight );
			this->CTFflux( 0 ) = 0.0;
			this->TbtmHistory = OutDryBulb;
			this->TtopHistory = OutDryBulb;
			this->TsrcHistory = OutDryBulb;
			this->QbtmHistory = 0.0;
			this->QtopHistory = 0.0;
			this->QsrcHistory = 0.0;
			this->TsrcConstCoef = 0.0;
			this->TsrcVarCoef = 0.0;
			this->QbtmConstCoef = 0.0;
			this->QbtmVarCoef = 0.0;
			this->QtopConstCoef = 0.0;
			this->QtopVarCoef = 0.0;
			this->QSrc = 0.0;
			this->QSrcAvg = 0.0;
			this->LastQSrc = 0.0;
			this->LastSysTimeElapsed = 0.0;
			this->LastTimeStepSys = 0.0;
			// initialize past weather variables
			PastBeamSolarRad = BeamSolarRad;
			PastSolarDirCosVert = SOLCOS( 3 );
			PastDifSolarRad = DifSolarRad;
			PastGroundTemp = GroundTemp_Surface;
			PastIsRain = IsRain;
			PastIsSnow = IsSnow;
			PastOutBaroPress = OutBaroPress;
			PastOutDryBulbTemp = OutDryBulbTempAt( SurfaceHXHeight );
			PastOutHumRat = OutHumRat;
			PastOutAirDensity = OutAirDensity;
			PastOutWetBulbTemp = OutWetBulbTempAt( SurfaceHXHeight );
			PastOutDewPointTemp = OutDewPointTemp;
			PastSkyTemp = SkyTemp;
			PastWindSpeed = WindSpeedAt( SurfaceHXHeight );
			PastCloudFraction = CloudFraction;
			this->MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) this->MyEnvrnFlag = true;

		// always initialize - module variables
		this->SurfaceArea = this->SurfaceLength * this->SurfaceWidth;
		nsvInletTemp = Node( this->InletNodeNum ).Temp;
		nsvOutletTemp = Node( this->OutletNodeNum ).Temp;

		// If loop operation is controlled by an environmental variable (DBtemp, WBtemp, etc)
		// then shut branch down when equipment is not scheduled to run.
		DesignFlow = RegulateCondenserCompFlowReqOp( this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, this->DesignMassFlowRate );

		SetComponentFlowRate( DesignFlow, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );

		// get the current flow rate - module variable
		FlowRate = Node( this->InletNodeNum ).MassFlowRate;

	}

	void
	SurfaceGroundHeatExchangerData::CalcSurfaceGroundHeatExchanger(
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a surface ground heat exchanger.  Calls are made to appropriate subroutines
		// either in this module or outside of it.

		// METHODOLOGY EMPLOYED:
		// To update temperature and flux histories it is necessary to make a surface
		// flux/temperature calculation at the begining of each zone time step using the
		// weather data from the previous step, and using the average source flux.
		// Once this has been done a new source flux, and current surface temperatures,
		// are calculated using the current weather data. These surface temperatures and
		// fluxes are used for the rest of the system time steps. During subsequent system
		// time steps only the source flux is updated.

		// Surface fluxes are calculated from the QTF equations using assumed surface
		// temperatures. Surface fluxes are then dependant only on source flux. Constant
		// and terms and terms that multiply the source flux from the QTF equations, are
		// grouped together for convenience. These are calculated in "CalcBottomFluxCoefficents"
		// etc. It is necessary to iterate on these equations, updating the current surface
		// temperatures at each step.

		// REFERENCES:
		// See 'LowTempRadiantSystem' module
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.
		// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
		//   of Wisconsin-Madison.

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataGlobals::BeginTimeStepFlag;
		using namespace DataEnvironment;
		using DataPlant::PlantLoop;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// INTEGER, INTENT(IN) :: FlowLock             ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SurfFluxTol( 0.001 ); // tolerance on the surface fluxes
		Real64 const SrcFluxTol( 0.001 ); // tolerance on the source flux
		Real64 const RelaxT( 0.1 ); // temperature relaxation factor
		int const Maxiter( 100 );
		int const Maxiter1( 100 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// variables used with previous environmental conditions
		// not used  REAL(r64)    :: Concentration    ! set to 0.5 if glycol, 0.0 if water
		Real64 PastFluxTop; // top surface flux - past value
		Real64 PastFluxBtm; // bottom surface flux - past value
		Real64 PastTempBtm; // bottom surface temp - past value
		Real64 PastTempTop; // top surface temp - past value
		Real64 OldPastFluxTop; // top surface flux - past value used during iteration
		Real64 OldPastFluxBtm; // bottom surface flux - past value used during iteration
		// variables used with current environmental conditions
		static Real64 FluxTop; // top surface flux
		static Real64 FluxBtm; // bottom surface flux
		static Real64 TempBtm; // bottom surface temp
		static Real64 TempTop; // top surface temp
		Real64 TempT; // top surface temp - used in underrelaxation
		Real64 TempB; // bottom surface temp - used in underrelaxation
		Real64 OldFluxTop; // top surface flux - value used during iteration
		Real64 OldFluxBtm; // bottom surface flux - value used during iteration
		Real64 OldSourceFlux; // previous value of source flux - used during iteration
		int iter;
		int iter1;
		//  INTEGER, SAVE ::ErrCount1=0
		//  INTEGER, SAVE ::ErrCount2=0
		//  INTEGER, SAVE ::ErrCount3=0
		static bool InitializeTempTop( false );
		int LoopNum;
		int LoopSideNum;

		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;

		// check if we are in very first call for this zone time step
		if ( BeginTimeStepFlag && FirstHVACIteration && PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) { //DSU
			// calc temps and fluxes with past env. conditions and average source flux
			SourceFlux = this->QSrcAvg;
			// starting values for the surface temps
			PastTempBtm = this->TbtmHistory( 1 );
			PastTempTop = this->TtopHistory( 1 );
			OldPastFluxTop = 1.0e+30;
			OldPastFluxBtm = 1.0e+30;
			OldSourceFlux = 1.0e+30;
			TempB = 0.0;
			TempT = 0.0;
			iter = 0;
			while ( true ) { // iterate to find surface heat balances
				// update coefficients

				++iter;
				CalcTopFluxCoefficents( PastTempBtm, PastTempTop );
				// calc top surface flux
				PastFluxTop = this->QtopConstCoef + this->QtopVarCoef * SourceFlux;

				//calc new top surface temp
				CalcTopSurfTemp( -PastFluxTop, TempT, PastOutDryBulbTemp, PastOutWetBulbTemp, PastSkyTemp, PastBeamSolarRad, PastDifSolarRad, PastSolarDirCosVert, PastWindSpeed, PastIsRain, PastIsSnow );
				// under relax
				PastTempTop = PastTempTop * ( 1.0 - RelaxT ) + RelaxT * TempT;

				// update coefficients
				CalcBottomFluxCoefficents( PastTempBtm, PastTempTop );
				PastFluxBtm = this->QbtmConstCoef + this->QbtmVarCoef * SourceFlux;

				if ( std::abs( ( OldPastFluxTop - PastFluxTop ) / OldPastFluxTop ) <= SurfFluxTol && std::abs( ( OldPastFluxBtm - PastFluxBtm ) / OldPastFluxBtm ) <= SurfFluxTol ) break;

				//calc new surface temps
				CalcBottomSurfTemp( PastFluxBtm, TempB, PastOutDryBulbTemp, PastWindSpeed, PastGroundTemp );
				// underrelax
				PastTempBtm = PastTempBtm * ( 1.0 - RelaxT ) + RelaxT * TempB;
				// update flux record
				OldPastFluxTop = PastFluxTop;
				OldPastFluxBtm = PastFluxBtm;

				//Check for non-convergence
				if ( iter > Maxiter ) {
					if ( this->ConvErrIndex1 == 0 ) {
						ShowWarningMessage( "CalcSurfaceGroundHeatExchanger=\"" + this->Name + "\", Did not converge (part 1), Iterations=" + TrimSigDigits( Maxiter ) );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "CalcSurfaceGroundHeatExchanger=\"" + this->Name + "\", Did not converge (part 1)", this->ConvErrIndex1 );
					break;
				}
			}

			if ( ! InitializeTempTop ) {
				TempTop = TempT;
				TempBtm = TempB;
				FluxTop = PastFluxTop;
				FluxBtm = PastFluxBtm;
				InitializeTempTop = true;
			}

			// update module variables
			TopSurfTemp = TempTop;
			BtmSurfTemp = TempBtm;
			TopSurfFlux = -FluxTop;
			BtmSurfFlux = FluxBtm;

			// get source temp for output
			CalcSourceTempCoefficents( PastTempBtm, PastTempTop );
			this->SourceTemp = this->TsrcConstCoef + this->TsrcVarCoef * SourceFlux;
			// update histories
			UpdateHistories( PastFluxTop, PastFluxBtm, SourceFlux, SourceTemp );

			// At the beginning of a time step, reset to zero so average calculation can start again
			this->QSrcAvg = 0.0;
			this->LastSysTimeElapsed = 0.0;
			this->LastTimeStepSys = 0.0;

			// get current env. conditions
			PastBeamSolarRad = BeamSolarRad;
			PastSolarDirCosVert = SOLCOS( 3 );
			PastDifSolarRad = DifSolarRad;
			PastGroundTemp = GroundTemp_Surface;
			PastIsRain = IsRain;
			PastIsSnow = IsSnow;
			PastOutBaroPress = OutBaroPress;
			PastOutDryBulbTemp = OutDryBulbTempAt( SurfaceHXHeight );
			PastOutHumRat = OutHumRat;
			PastOutAirDensity = OutAirDensity;
			PastOutWetBulbTemp = OutWetBulbTempAt( SurfaceHXHeight );
			PastOutDewPointTemp = OutDewPointTempAt( SurfaceHXHeight );
			PastSkyTemp = SkyTemp;
			PastWindSpeed = WindSpeedAt( SurfaceHXHeight );
			PastCloudFraction = CloudFraction;

			TempBtm = this->TbtmHistory( 1 );
			TempTop = this->TtopHistory( 1 );
			OldFluxTop = 1.0e+30;
			OldFluxBtm = 1.0e+30;
			OldSourceFlux = 1.0e+30;
			SourceFlux = CalcSourceFlux();
			iter = 0;
			while ( true ) { // iterate to find source flux
				++iter;
				iter1 = 0;
				while ( true ) { // iterate to find surface heat balances
					++iter1;
					// update top coefficients
					CalcTopFluxCoefficents( TempBtm, TempTop );
					// calc top surface fluxe
					FluxTop = this->QtopConstCoef + this->QtopVarCoef * SourceFlux;
					//calc new surface temps
					CalcTopSurfTemp( -FluxTop, TempT, PastOutDryBulbTemp, PastOutWetBulbTemp, PastSkyTemp, PastBeamSolarRad, PastDifSolarRad, PastSolarDirCosVert, PastWindSpeed, PastIsRain, PastIsSnow );
					// under-relax
					TempTop = TempTop * ( 1.0 - RelaxT ) + RelaxT * TempT;
					// update bottom coefficients
					CalcBottomFluxCoefficents( TempBtm, TempTop );
					FluxBtm = this->QbtmConstCoef + this->QbtmVarCoef * SourceFlux;
					// convergence test on surface fluxes
					if ( std::abs( ( OldFluxTop - FluxTop ) / OldFluxTop ) <= SurfFluxTol && std::abs( ( OldFluxBtm - FluxBtm ) / OldFluxBtm ) <= SurfFluxTol ) break;

					//calc new surface temps
					CalcBottomSurfTemp( FluxBtm, TempB, PastOutDryBulbTemp, PastOutDryBulbTemp, GroundTemp_Surface );
					// under-relax
					TempBtm = TempBtm * ( 1.0 - RelaxT ) + RelaxT * TempB;
					// update flux record
					OldFluxBtm = FluxBtm;
					OldFluxTop = FluxTop;

					//Check for non-convergence
					if ( iter1 > Maxiter1 ) {
						if ( this->ConvErrIndex2 == 0 ) {
							ShowWarningMessage( "CalcSurfaceGroundHeatExchanger=\"" + this->Name + "\", Did not converge (part 2), Iterations=" + TrimSigDigits( Maxiter ) );
							ShowContinueErrorTimeStamp( "" );
						}
						ShowRecurringWarningErrorAtEnd( "CalcSurfaceGroundHeatExchanger=\"" + this->Name + "\", Did not converge (part 2)", this->ConvErrIndex2 );
						break;
					}
				}
				// update the source temp coefficients and update the source flux
				CalcSourceTempCoefficents( TempBtm, TempTop );
				SourceFlux = CalcSourceFlux();
				// check source flux convergence
				if ( std::abs( ( OldSourceFlux - SourceFlux ) / ( 1.0e-20 + OldSourceFlux ) ) <= SrcFluxTol ) break;
				OldSourceFlux = SourceFlux;

				//Check for non-convergence
				if ( iter > Maxiter ) {
					if ( this->ConvErrIndex3 == 0 ) {
						ShowWarningMessage( "CalcSurfaceGroundHeatExchanger=\"" + this->Name + "\", Did not converge (part 3), Iterations=" + TrimSigDigits( Maxiter ) );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "CalcSurfaceGroundHeatExchanger=\"" + this->Name + "\", Did not converge (part 3)", this->ConvErrIndex3 );
					break;
				}
			} // end surface heat balance iteration

		} else { // end source flux iteration

			// For the rest of the system time steps ...
			// update source flux from Twi
			SourceFlux = this->CalcSourceFlux();

		}

	}

	void
	SurfaceGroundHeatExchangerData::CalcBottomFluxCoefficents(
		Real64 const Tbottom, // current bottom (lower) surface temperature
		Real64 const Ttop // current top (upper) surface temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates current version of constant variable parts of QTF equations.

		// METHODOLOGY EMPLOYED:
		// For given current surface temperatures the terms of the QTF equations can be
		// grouped into constant terms, and those depending on the current source flux.
		// This routine calculates the current coefficient values for the bottom flux
		// equation.

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Term;

		// add current surface temperatures to history data
		this->TbtmHistory( 0 ) = Tbottom;
		this->TtopHistory( 0 ) = Ttop;

		// Bottom Surface Coefficients
		this->QbtmConstCoef = 0.0;
		for ( Term = 0; Term <= this->NumCTFTerms - 1; ++Term ) {

			this->QbtmConstCoef += ( -this->CTFin( Term ) * this->TbtmHistory( Term ) ) + ( this->CTFcross( Term ) * this->TtopHistory( Term ) ) + ( this->CTFflux( Term ) * this->QbtmHistory( Term ) ) + ( this->CTFSourceIn( Term ) * this->QsrcHistory( Term ) );

		}

		//     SurfaceGHE(SurfaceGHENum)%QbtmConstCoef =  SUM(-SurfaceGHE(SurfaceGHENum)%CTFin * &
		//                                                  SurfaceGHE(SurfaceGHENum)%TbtmHistory + &
		//                                                  SurfaceGHE(SurfaceGHENum)%CTFcross * &
		//                                                  SurfaceGHE(SurfaceGHENum)%TtopHistory + &
		//                                                  SurfaceGHE(SurfaceGHENum)%CTFflux * &
		//                                                  SurfaceGHE(SurfaceGHENum)%QbtmHistory + &
		//                                                  SurfaceGHE(SurfaceGHENum)%CTFSourceIn * &
		//                                                  SurfaceGHE(SurfaceGHENum)%QsrcHistory)
		// correct for extra bottom surface flux term
		this->QbtmConstCoef -= this->CTFSourceIn( 0 ) * this->QsrcHistory( 0 );
		// source flux current coefficient
		this->QbtmVarCoef = this->CTFSourceIn( 0 );

	}

	void
	SurfaceGroundHeatExchangerData::CalcTopFluxCoefficents(
		Real64 const Tbottom, // current bottom (lower) surface temperature
		Real64 const Ttop // current top (upper) surface temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates current version of constant variable parts of QTF equations.

		// METHODOLOGY EMPLOYED:
		// For given current surface temperatures the terms of the QTF equations can be
		// grouped into constant terms, and those depending on the current source flux.
		// This routine calculates the current coefficient values for the top flux
		// equation.

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// add current surface temperatures to history data
		this->TbtmHistory( 0 ) = Tbottom;
		this->TtopHistory( 0 ) = Ttop;

		// Top Surface Coefficients
		this->QtopConstCoef = 0.0;
		for ( int Term = 0; Term <= this->NumCTFTerms - 1; ++Term ) {

			this->QtopConstCoef += ( this->CTFout( Term ) * this->TtopHistory( Term ) ) - ( this->CTFcross( Term ) * this->TbtmHistory( Term ) ) + ( this->CTFflux( Term ) * this->QtopHistory( Term ) ) + ( this->CTFSourceOut( Term ) * this->QsrcHistory( Term ) );

		}

		//     ! Top Surface Coefficients
		//     SurfaceGHE(SurfaceGHENum)%QtopConstCoef = SUM(SurfaceGHE(SurfaceGHENum)%CTFout * &
		//                                              SurfaceGHE(SurfaceGHENum)%TtopHistory - &
		//                                              SurfaceGHE(SurfaceGHENum)%CTFcross * &
		//                                              SurfaceGHE(SurfaceGHENum)%TbtmHistory + &
		//                                              SurfaceGHE(SurfaceGHENum)%CTFflux * &
		//                                              SurfaceGHE(SurfaceGHENum)%QtopHistory + &
		//                                              SurfaceGHE(SurfaceGHENum)%CTFSourceOut * &
		//                                              SurfaceGHE(SurfaceGHENum)%QsrcHistory)

		// correct for extra top surface flux term
		this->QtopConstCoef -= ( this->CTFSourceOut( 0 ) * this->QsrcHistory( 0 ) );
		// surface flux current coefficient
		this->QtopVarCoef = this->CTFSourceOut( 0 );

	}

	void
	SurfaceGroundHeatExchangerData::CalcSourceTempCoefficents(
		Real64 const Tbottom, // current bottom (lower) surface temperature
		Real64 const Ttop // current top (upper) surface temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates current version of constant variable parts of QTF equations.

		// METHODOLOGY EMPLOYED:
		// For given current surface temperatures the terms of the QTF equations can be
		// grouped into constant terms, and those depending on the current source flux.
		// This routine calculates the current coefficient values for the source temperature
		// equation.

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Term;

		// add current surface temperatures to history data
		this->TbtmHistory( 0 ) = Tbottom;
		this->TtopHistory( 0 ) = Ttop;

		this->TsrcConstCoef = 0.0;
		for ( Term = 0; Term <= this->NumCTFTerms - 1; ++Term ) {

			this->TsrcConstCoef += ( this->CTFTSourceIn( Term ) * this->TbtmHistory( Term ) ) + ( this->CTFTSourceOut( Term ) * this->TtopHistory( Term ) ) + ( this->CTFflux( Term ) * this->TsrcHistory( Term ) ) + ( this->CTFTSourceQ( Term ) * this->QsrcHistory( Term ) );

		}

		// Source Temperature terms
		//     SurfaceGHE(SurfaceGHENum)%TsrcConstCoef = SUM(SurfaceGHE(SurfaceGHENum)%CTFTSourceIn * &
		//                                              SurfaceGHE(SurfaceGHENum)%TbtmHistory + &
		//                                              SurfaceGHE(SurfaceGHENum)%CTFTSourceOut * &
		//                                              SurfaceGHE(SurfaceGHENum)%TtopHistory + &
		//                                              SurfaceGHE(SurfaceGHENum)%CTFflux * &
		//                                              SurfaceGHE(SurfaceGHENum)%TsrcHistory + &
		//                                              SurfaceGHE(SurfaceGHENum)%CTFTSourceQ * &
		//                                              SurfaceGHE(SurfaceGHENum)%QsrcHistory)
		// correct for extra source flux term
		this->TsrcConstCoef -= this->CTFTSourceQ( 0 ) * this->QsrcHistory( 0 );
		// source flux current coefficient
		this->TsrcVarCoef = this->CTFTSourceQ( 0 );

	}

	Real64
	SurfaceGroundHeatExchangerData::CalcSourceFlux() // component number
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This calculates the source flux given the inlet fluid temperature. A
		// heat exchanger analogy is used, with the surface as a 'Fixed' fluid.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Return value
		Real64 CalcSourceFlux;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 EpsMdotCp; // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat

		// Effectiveness * Modot * specific heat
		if ( FlowRate > 0.0 ) {
			EpsMdotCp = CalcHXEffectTerm( InletTemp, FlowRate );
			// calc flux
			CalcSourceFlux = ( InletTemp - this->TsrcConstCoef ) / ( this->SurfaceArea / EpsMdotCp + this->TsrcVarCoef );
		} else {
			EpsMdotCp = 0.0;
			CalcSourceFlux = 0.0;
		}

		return CalcSourceFlux;
	}

	void
	SurfaceGroundHeatExchangerData::UpdateHistories(
		Real64 const TopFlux, // current top (top) surface flux
		Real64 const BottomFlux, // current bottom (bottom) surface flux
		Real64 const SourceFlux, // current source surface flux
		Real64 const SourceTemp // current source temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is used to update the temperature and flux records for the QTF
		// calculations. This is called at the start of each zone timestep.

		// METHODOLOGY EMPLOYED:
		// Just shift along and replace zero index element with current value.

		// REFERENCES:
		// n/a

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// update top surface temps
		this->TtopHistory = eoshift( this->TtopHistory, -1 );

		// update bottom surface temps
		this->TbtmHistory = eoshift( this->TbtmHistory, -1 );

		// update bottom surface temps
		this->TsrcHistory = eoshift( this->TsrcHistory, -1 );
		this->TsrcHistory( 1 ) = SourceTemp;

		// update bottom surface fluxes
		this->QbtmHistory = eoshift( this->QbtmHistory, -1 );
		this->QbtmHistory( 1 ) = BottomFlux;

		// update bottom surface fluxes
		this->QtopHistory = eoshift( this->QtopHistory, -1 );
		this->QtopHistory( 1 ) = TopFlux;

		// update bottom surface fluxes
		this->QsrcHistory = eoshift( this->QsrcHistory, -1 );
		this->QsrcHistory( 1 ) = SourceFlux;

	}

	Real64
	SurfaceGroundHeatExchangerData::CalcHXEffectTerm(
		Real64 const Temperature, // Temperature of water entering the surface, in C
		Real64 const WaterMassFlow // Mass flow rate, in kg/s
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   December 2000
		//       MODIFIED       Simon Rees, August 2002
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the "heat exchanger"
		// effectiveness term.  This is equal to the mass flow rate of water
		// times the specific heat of water times the effectiveness of
		// the surface heat exchanger. This routine is adapted from that in
		// the low temp radiant surface model.

		// METHODOLOGY EMPLOYED:
		// Assumes that the only REAL(r64) heat transfer term that we have to
		// deal with is the convection from the water to the tube.  The
		// other assumptions are that the tube bottom surface temperature
		// is equal to the "source location temperature" and that it is
		// a CONSTANT throughout the surface.

		// REFERENCES:
		// See RadiantSystemLowTemp module.
		// Property data for water shown below as parameters taken from
		//   Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
		// Heat exchanger information also from Incropera and DeWitt.
		// Code based loosely on code from IBLAST program (research version)

		// Using/Aliasing
		using DataGlobals::Pi;
		using General::RoundSigDigits;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Return value
		Real64 CalcHXEffectTerm;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		int const NumOfPropDivisions( 13 ); // intervals in property correlation
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { 1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.001652, 0.001422, 0.001225, 0.00108, 0.000959, 0.000855, 0.000769, 0.000695, 0.000631, 0.000577, 0.000528, 0.000489, 0.000453 } ); // Viscosity, in Ns/m2
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.574, 0.582, 0.590, 0.598, 0.606, 0.613, 0.620, 0.628, 0.634, 0.640, 0.645, 0.650, 0.656 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, { 12.22, 10.26, 8.81, 7.56, 6.62, 5.83, 5.20, 4.62, 4.16, 3.77, 3.42, 3.15, 2.88 } ); // Prandtl number (dimensionless)
		int const WaterIndex( 1 );
		static std::string const RoutineName( "SurfaceGroundHeatExchanger:CalcHXEffectTerm" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Index;
		Real64 InterpFrac;
		Real64 NuD;
		Real64 ReD;
		Real64 NTU;
		Real64 CpWater;
		Real64 Kactual;
		Real64 MUactual;
		Real64 PRactual;
		Real64 PipeLength;
		//  INTEGER, SAVE                             :: ErrCount

		// FLOW:
		// First find out where we are in the range of temperatures
		Index = 1;
		while ( Index <= NumOfPropDivisions ) {
			if ( Temperature < Temps( Index ) ) break; // DO loop
			++Index;
		}

		// Initialize thermal properties of water
		if ( Index == 1 ) {
			MUactual = Mu( Index );
			Kactual = Conductivity( Index );
			PRactual = Pr( Index );
		} else if ( Index > NumOfPropDivisions ) {
			Index = NumOfPropDivisions;
			MUactual = Mu( Index );
			Kactual = Conductivity( Index );
			PRactual = Pr( Index );
		} else {
			InterpFrac = ( Temperature - Temps( Index - 1 ) ) / ( Temps( Index ) - Temps( Index - 1 ) );
			MUactual = Mu( Index - 1 ) + InterpFrac * ( Mu( Index ) - Mu( Index - 1 ) );
			Kactual = Conductivity( Index - 1 ) + InterpFrac * ( Conductivity( Index ) - Conductivity( Index - 1 ) );
			PRactual = Pr( Index - 1 ) + InterpFrac * ( Pr( Index ) - Pr( Index - 1 ) );
		}
		// arguments are glycol name, temperature, and concentration
		if ( Temperature < 0.0 ) { // check if fluid is water and would be freezing
			if ( PlantLoop( this->LoopNum ).FluidIndex == WaterIndex ) {
				if ( this->FrozenErrIndex1 == 0 ) {
					ShowWarningMessage( "GroundHeatExchanger:Surface=\"" + this->Name + "\", water is frozen; Model not valid. Calculated Water Temperature=[" + RoundSigDigits( InletTemp, 2 ) + "] C" );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( "GroundHeatExchanger:Surface=\"" + this->Name + "\", water is frozen", this->FrozenErrIndex1, InletTemp, InletTemp, _, "[C]", "[C]" );
				InletTemp = max( InletTemp, 0.0 );
			}
		}
		CpWater = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, Temperature, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
		ReD = 4.0 * WaterMassFlow / ( Pi * MUactual * this->TubeDiameter * this->TubeCircuits );

		// Calculate the Nusselt number based on what flow regime one is in
		if ( ReD >= MaxLaminarRe ) { // Turbulent flow --> use Colburn equation
			NuD = 0.023 * std::pow( ReD, 0.8 ) * std::pow( PRactual, 1.0 / 3.0 );
		} else { // Laminar flow --> use constant surface temperature relation
			NuD = 3.66;
		}
		// Calculate the NTU parameter
		// NTU = UA/[(Mdot*Cp)min]
		// where: U = h (convection coefficient) and h = (k)(Nu)/D
		//        A = Pi*D*TubeLength
		//  NTU = PI * Kactual * NuD * SurfaceGHE(SurfaceGHENum)%TubeLength / (WaterMassFlow * CpWater)

		PipeLength = this->SurfaceLength * this->SurfaceWidth / this->TubeSpacing;

		NTU = Pi * Kactual * NuD * PipeLength / ( WaterMassFlow * CpWater );
		// Calculate Epsilon*MassFlowRate*Cp
		if ( -NTU >= EXP_LowerLimit ) {
			CalcHXEffectTerm = ( 1.0 - std::exp( -NTU ) ) * WaterMassFlow * CpWater;
		} else {
			CalcHXEffectTerm = 1.0 * WaterMassFlow * CpWater;
		}

		return CalcHXEffectTerm;

	}

	void
	SurfaceGroundHeatExchangerData::CalcTopSurfTemp(
		Real64 const FluxTop, // top surface flux
		Real64 & TempTop, // top surface temperature
		Real64 const ThisDryBulb, // dry bulb temperature
		Real64 const ThisWetBulb, // wet bulb temperature
		Real64 const ThisSkyTemp, // sky temperature
		Real64 const ThisBeamSolarRad, // beam solar radiation
		Real64 const ThisDifSolarRad, // diffuse solar radiation
		Real64 const ThisSolarDirCosVert, // vertical component of solar normal
		Real64 const ThisWindSpeed, // wind speed
		bool const ThisIsRain, // rain flag
		bool const ThisIsSnow // snow flag
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is used to calculate the top surface
		// temperature for the given surface flux.

		// METHODOLOGY EMPLOYED:
		// calc surface heat balance

		// REFERENCES:
		// USE STATEMENTS:

		// Using/Aliasing
		using ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// n/a
		// INTERFACE BLOCK SPECIFICATIONS
		// n/a

		// DERIVED TYPE DEFINITIONS
		// n/a

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ConvCoef; // convection coefficient
		Real64 RadCoef; // radiation coefficient
		Real64 ExternalTemp; // external environmental temp - drybulb or wetbulb
		Real64 OldSurfTemp; // previous surface temperature
		Real64 QSolAbsorbed; // absorbed solar flux
		Real64 SurfTempAbs; // absolute value of surface temp
		Real64 SkyTempAbs; // absolute value of sky temp

		// make a surface heat balance and solve for temperature

		// set appropriate external temp
		if ( ThisIsSnow ) {
			ExternalTemp = ThisWetBulb;
		} else if ( ThisIsRain ) {
			ExternalTemp = ThisWetBulb;
		} else { // normal dry conditions
			ExternalTemp = ThisDryBulb;
		}

		// set previous surface temp
		OldSurfTemp = this->TtopHistory( 1 );
		// absolute temperatures
		SurfTempAbs = OldSurfTemp + KelvinConv;
		SkyTempAbs = ThisSkyTemp + KelvinConv;

		// ASHRAE simple convection coefficient model for external surfaces.
		ConvCoef = CalcASHRAESimpExtConvectCoeff( TopRoughness, ThisWindSpeed );
		// radiation coefficient using surf temp from past time step
		if ( std::abs( SurfTempAbs - SkyTempAbs ) > SmallNum ) {
			RadCoef = StefBoltzmann * TopThermAbs * ( pow_4( SurfTempAbs ) - pow_4( SkyTempAbs ) ) / ( SurfTempAbs - SkyTempAbs );
		} else {
			RadCoef = 0.0;
		}

		// total absorbed solar - no ground solar
		QSolAbsorbed = TopSolarAbs * ( max( ThisSolarDirCosVert, 0.0 ) * ThisBeamSolarRad + ThisDifSolarRad );

		// solve for temperature
		TempTop = ( FluxTop + ConvCoef * ExternalTemp + RadCoef * ThisSkyTemp + QSolAbsorbed ) / ( ConvCoef + RadCoef );

	}

	void
	SurfaceGroundHeatExchangerData::CalcBottomSurfTemp(
		Real64 const FluxBtm, // bottom surface flux
		Real64 & TempBtm, // bottom surface temperature
		Real64 const ThisDryBulb, // dry bulb temperature
		Real64 const ThisWindSpeed, // wind speed
		Real64 const ThisGroundTemp // ground temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is used to calculate the bottom surface
		// temperature for the given surface flux.

		// METHODOLOGY EMPLOYED:
		// calc surface heat balances

		// REFERENCES:
		// USE STATEMENTS:

		// Using/Aliasing
		using ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 ConvCoef; // convection coefficient
		Real64 RadCoef; // radiation coefficient
		Real64 OldSurfTemp; // previous surface temperature
		Real64 SurfTempAbs; // absolute value of surface temp
		Real64 ExtTempAbs; // absolute value of sky temp

		if ( this->LowerSurfCond == SurfCond_Exposed ) {

			// make a surface heat balance and solve for temperature
			OldSurfTemp = this->TbtmHistory( 1 );
			// absolute temperatures
			SurfTempAbs = OldSurfTemp + KelvinConv;
			ExtTempAbs = ThisDryBulb + KelvinConv;

			// ASHRAE simple convection coefficient model for external surfaces.
			ConvCoef = CalcASHRAESimpExtConvectCoeff( TopRoughness, ThisWindSpeed );

			// radiation coefficient using surf temp from past time step
			if ( std::abs( SurfTempAbs - ExtTempAbs ) > SmallNum ) {
				RadCoef = StefBoltzmann * TopThermAbs * ( pow_4( SurfTempAbs ) - pow_4( ExtTempAbs ) ) / ( SurfTempAbs - ExtTempAbs );
			} else {
				RadCoef = 0.0;
			}

			// total absorbed solar - no ground solar
			TempBtm = ( FluxBtm + ConvCoef * ThisDryBulb + RadCoef * ThisDryBulb ) / ( ConvCoef + RadCoef );

		} else { // ground coupled
			// just use the supplied ground temperature
			TempBtm = ThisGroundTemp;
		}

	}

	void
	SurfaceGroundHeatExchangerData::UpdateSurfaceGroundHeatExchngr() // Index for the surface
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for surface
		// ground heat exchangers.  One of the most important functions of
		// this routine is to update the average heat source/sink for a
		// particular system over the various system time steps that make up
		// the zone time step. This routine must also set the outlet water conditions.

		// METHODOLOGY EMPLOYED:
		// For the source/sink average update, if the system time step elapsed
		// is still what it used to be, then either we are still iterating or
		// we had to go back and shorten the time step.  As a result, we have
		// to subtract out the previous value that we added.  If the system
		// time step elapsed is different, then we just need to add the new
		// values to the running average.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SurfaceGroundHeatExchanger:Update" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpFluid; // Specific heat of working fluid
		//  INTEGER,SAVE    :: ErrCount
		int LoopNum;
		int LoopSideNum;

		// update flux
		this->QSrc = SourceFlux;

		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock > 0 ) { // only update in normal mode !DSU
			if ( this->LastSysTimeElapsed == SysTimeElapsed ) {
				// Still iterating or reducing system time step, so subtract old values which were
				// not valid
				this->QSrcAvg -= this->LastQSrc * this->LastTimeStepSys / TimeStepZone;
			}

			// Update the running average and the "last" values with the current values of the appropriate variables
			this->QSrcAvg += this->QSrc * TimeStepSys / TimeStepZone;

			this->LastQSrc = SourceFlux;
			this->LastSysTimeElapsed = SysTimeElapsed;
			this->LastTimeStepSys = TimeStepSys;

		}

		// Calculate the water side outlet conditions and set the
		// appropriate conditions on the correct HVAC node.
		if ( PlantLoop( this->LoopNum ).FluidName == "WATER" ) {
			if ( InletTemp < 0.0 ) {
				ShowRecurringWarningErrorAtEnd( "UpdateSurfaceGroundHeatExchngr: Water is frozen in Surf HX=" + this->Name, this->FrozenErrIndex2, InletTemp, InletTemp );
			}
			InletTemp = max( InletTemp, 0.0 );
		}

		CpFluid = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, InletTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

		SafeCopyPlantNode( this->InletNodeNum, this->OutletNodeNum );
		// check for flow
		if ( ( CpFluid > 0.0 ) && ( FlowRate > 0.0 ) ) {
			Node( this->OutletNodeNum ).Temp = InletTemp - this->SurfaceArea * SourceFlux / ( FlowRate * CpFluid );
			Node( this->OutletNodeNum ).Enthalpy = Node( this->OutletNodeNum ).Temp * CpFluid;
		}

	}

	void
	SurfaceGroundHeatExchangerData::ReportSurfaceGroundHeatExchngr() // Index for the surface under consideration
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for Surface ground heat exchangers

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// update flows and temps from node data
		this->InletTemp = Node( this->InletNodeNum ).Temp;
		this->OutletTemp = Node( this->OutletNodeNum ).Temp;
		this->MassFlowRate = Node( this->InletNodeNum ).MassFlowRate;

		// update other variables from module variables
		this->HeatTransferRate = SourceFlux * this->SurfaceArea;
		this->SurfHeatTransferRate = this->SurfaceArea * ( TopSurfFlux + BtmSurfFlux );
		this->Energy = SourceFlux * this->SurfaceArea * TimeStepSys * SecInHour;
		this->TopSurfaceTemp = TopSurfTemp;
		this->BtmSurfaceTemp = BtmSurfTemp;
		this->TopSurfaceFlux = TopSurfFlux;
		this->BtmSurfaceFlux = BtmSurfFlux;
		this->SurfEnergy = SurfaceArea * ( TopSurfFlux + BtmSurfFlux ) * TimeStepSys * SecInHour;

	}

} // SurfaceGroundHeatExchanger

} // EnergyPlus
