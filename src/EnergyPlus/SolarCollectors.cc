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
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <SolarCollectors.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SolarCollectors {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   December 2003
	//       MODIFIED       B. Nigusse, FSEC/UCF, March 2012, added ICS Collector
	//       RE-ENGINEERED  Brent Griffith, for plant upgrade, general fluid props

	// PURPOSE OF THIS MODULE:
	// Simulates solar collectors as a component on the plant loop.  Currently only flat-plate collectors (glazed and
	// unglazed) are implemented.

	// METHODOLOGY EMPLOYED:
	// Solar collectors are called as non-zone equipment on the demand side of the plant loop.  The collector object
	// must be connected to a WATER HEATER object on the supply side of the plant loop.  Water is assumed to be
	// the heat transfer fluid.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataSurfaces::Surface;
	using DataSurfaces::SurfSunlitArea;
	using DataSurfaces::SurfSunlitFrac;
	using DataSurfaces::SurfaceClass_Detached_F;
	using DataSurfaces::SurfaceClass_Detached_B;
	using DataSurfaces::SurfaceClass_Shading;
	using PlantUtilities::SetComponentFlowRate;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Fluid Type Flags
	int const WATER( 1 );
	int const AIR( 2 );

	// Test Correlation Type Flags
	int const INLET( 1 );
	int const AVERAGE( 2 );
	int const OUTLET( 3 );

	// ICS Collector Type Flag
	int const ICSRectangularTank( 1 );
	//INTEGER, PARAMETER :: ICSProgressiveTube = 2

	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	Array1D_bool CheckEquipName;

	// MODULE VARIABLE DECLARATIONS:
	int NumOfParameters( 0 );
	int NumOfCollectors( 0 );

	Array1D< Real64 > TransSysSkyDiff; // transmittance of cover system for sky diffuse solar rad.
	Array1D< Real64 > TransSysGrnDiff; // transmittance of cover system for ground diffuse solar rad.
	Array1D< Real64 > RefSysSkyDiff; // reflectance of cover system for sky diffuse solar rad.
	Array1D< Real64 > RefSysGrnDiff; // reflectance of cover system for ground diffuse solar rad.

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< ParametersData > Parameters;
	Array1D< CollectorData > Collector;

	// MODULE SUBROUTINES:

	// Functions

	void
	SimSolarCollector(
		int const EP_UNUSED( EquipTypeNum ),
		std::string const & CompName,
		int & CompIndex,
		bool const EP_UNUSED( InitLoopEquip ),
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   December 2003
		//       MODIFIED       Brent Griffith, March 2010
		//                      Bereket Nigusse, March 2012 Added ICS collector
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates solar collector objects.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataPlant::TypeOf_SolarCollectorFlatPlate;
		using DataPlant::TypeOf_SolarCollectorICS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true );
		int CollectorNum;

		// FLOW:
		if ( GetInputFlag ) {
			GetSolarCollectorInput();
			GetInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			CollectorNum = FindItemInList( CompName, Collector );
			if ( CollectorNum == 0 ) {
				ShowFatalError( "SimSolarCollector: Specified solar collector not Valid =" + CompName );
			}
			CompIndex = CollectorNum;
		} else {
			CollectorNum = CompIndex;
			if ( CollectorNum > NumOfCollectors || CollectorNum < 1 ) {
				ShowFatalError( "SimSolarCollector: Invalid CompIndex passed=" + TrimSigDigits( CollectorNum ) + ", Number of Units=" + TrimSigDigits( NumOfCollectors ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( CollectorNum ) ) {
				if ( CompName != Collector( CollectorNum ).Name ) {
					ShowFatalError( "SimSolarCollector: Invalid CompIndex passed=" + TrimSigDigits( CollectorNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + Collector( CollectorNum ).Name );

				}
				CheckEquipName( CollectorNum ) = false;
			}

		}

		InitSolarCollector( CollectorNum );

		{ auto const SELECT_CASE_var( Collector( CollectorNum ).TypeNum );
		// Select and CALL models based on collector type
		if ( SELECT_CASE_var == TypeOf_SolarCollectorFlatPlate ) {

			CalcSolarCollector( CollectorNum );

		} else if ( SELECT_CASE_var == TypeOf_SolarCollectorICS ) {

			CalcICSSolarCollector( CollectorNum );

		} else {

		}}

		UpdateSolarCollector( CollectorNum );

		ReportSolarCollector( CollectorNum );

	}

	void
	GetSolarCollectorInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   December 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the solar collector input from the input file and sets up the parameters and collector objects.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::InitConvTemp;
		using namespace DataHeatBalance;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using Psychrometrics::RhoH2O;
		using namespace DataLoopNode;
		using namespace DataPlant; // DSU
		using General::RoundSigDigits;
		using DataSurfaces::Surface;
		using DataSurfaces::OSCM;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int CollectorNum; // Solar collector object number
		int CollectorNum2; // Second solar collector object number for looping
		int ParametersNum; // Solar collector parameters object number
		int SurfNum; // Collector surface object number
		std::string CurrentModuleObject; // for ease in renaming.
		std::string CurrentModuleParamObject; // for ease in renaming.

		int NumFields; // Total number of fields in object
		int MaxAlphas; // Maximum number of alpha fields in all objects
		int MaxNumbers; // Maximum number of numeric fields in all objects
		static int NumOfICSParam( 0 ); // number of parameter objects for ICS colectors
		static int NumOfICSUnits( 0 ); // number of ICS colector units
		static int NumOfFlatPlateParam( 0 ); // number of parameter objects for flat plate colectors
		static int NumFlatPlateUnits( 0 ); // number of plat plate solar colector units

		int FlatPlateParamNum; // plat plate solar colector parameters counter
		int ICSParamNum; // ICS collector parameters counter

		int FlatPlateUnitsNum; // plat plate solar colector parameters counter
		int ICSUnitsNum; // ICS collector parameters counter
		int Found; // index
		int VentCavIndex; // vent cavity index
		Real64 Perimeter; // perimeter of the absorber or collector

		Array1D< Real64 > Numbers; // Numeric data
		Array1D_string Alphas; // Alpha data
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// FLOW:
		MaxNumbers = 0;
		MaxAlphas = 0;

		CurrentModuleParamObject = "SolarCollectorPerformance:FlatPlate";
		NumOfFlatPlateParam = GetNumObjectsFound( CurrentModuleParamObject );
		GetObjectDefMaxArgs( CurrentModuleParamObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		CurrentModuleObject = "SolarCollector:FlatPlate:Water";
		NumFlatPlateUnits = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		CurrentModuleParamObject = "SolarCollectorPerformance:IntegralCollectorStorage";
		NumOfICSParam = GetNumObjectsFound( CurrentModuleParamObject );
		GetObjectDefMaxArgs( CurrentModuleParamObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		CurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
		NumOfICSUnits = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		Numbers.dimension( MaxNumbers, 0.0 );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNumbers );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNumbers, true );

		NumOfCollectors = NumFlatPlateUnits + NumOfICSUnits;
		NumOfParameters = NumOfFlatPlateParam + NumOfICSParam;

		if ( NumOfParameters > 0 ) {
			Parameters.allocate( NumOfParameters );

			CurrentModuleParamObject = "SolarCollectorPerformance:FlatPlate";

			for ( FlatPlateParamNum = 1; FlatPlateParamNum <= NumOfFlatPlateParam; ++FlatPlateParamNum ) {

				ParametersNum = FlatPlateParamNum;
				GetObjectItem( CurrentModuleParamObject, ParametersNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );

				// Collector module parameters name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Parameters, ParametersNum - 1, IsNotOK, IsBlank, CurrentModuleParamObject );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				Parameters( ParametersNum ).Name = cAlphaArgs( 1 );

				// NOTE:  This values serves mainly as a reference.  The area of the associated surface object is used in all calculations.
				Parameters( ParametersNum ).Area = rNumericArgs( 1 );

				{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );
				if ( SELECT_CASE_var == "WATER" ) {
					Parameters( ParametersNum ).TestFluid = WATER;
					//CASE('AIR')
					//  Parameters(ParametersNum)%TestFluid = AIR
				} else {
					ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) + ":  " + cAlphaArgs( 2 ) + " is an unsupported Test Fluid for " + cAlphaFieldNames( 2 ) );
					ErrorsFound = true;
				}}

				if ( rNumericArgs( 2 ) > 0.0 ) {
					Parameters( ParametersNum ).TestMassFlowRate = rNumericArgs( 2 ) * RhoH2O( InitConvTemp );
				} else {
					ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) + ":  flow rate must be greater than zero for " + cNumericFieldNames( 2 ) );
					ErrorsFound = true;
				}

				{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
				if ( SELECT_CASE_var == "INLET" ) {
					Parameters( ParametersNum ).TestType = INLET;
				} else if ( SELECT_CASE_var == "AVERAGE" ) {
					Parameters( ParametersNum ).TestType = AVERAGE;
				} else if ( SELECT_CASE_var == "OUTLET" ) {
					Parameters( ParametersNum ).TestType = OUTLET;
				} else {
					ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) + ":  " + cAlphaArgs( 3 ) + " is  not supported for " + cAlphaFieldNames( 3 ) );
					ErrorsFound = true;
				}}

				// Efficiency equation coefficients
				Parameters( ParametersNum ).eff0 = rNumericArgs( 3 );
				Parameters( ParametersNum ).eff1 = rNumericArgs( 4 );

				if ( NumNumbers > 4 ) {
					Parameters( ParametersNum ).eff2 = rNumericArgs( 5 );
				} else {
					Parameters( ParametersNum ).eff2 = 0.0;
				}

				// Incident angle modifier coefficients
				if ( NumNumbers > 5 ) {
					Parameters( ParametersNum ).iam1 = rNumericArgs( 6 );
				} else {
					Parameters( ParametersNum ).iam1 = 0.0;
				}

				if ( NumNumbers > 6 ) {
					Parameters( FlatPlateParamNum ).iam2 = rNumericArgs( 7 );
				} else {
					Parameters( ParametersNum ).iam2 = 0.0;
				}
			} // ParametersNum

			if ( ErrorsFound ) ShowFatalError( "Errors in " + CurrentModuleParamObject + " input." );
		}

		if ( NumOfCollectors > 0 ) {
			Collector.allocate( NumOfCollectors );

			CurrentModuleObject = "SolarCollector:FlatPlate:Water";

			for ( FlatPlateUnitsNum = 1; FlatPlateUnitsNum <= NumFlatPlateUnits; ++FlatPlateUnitsNum ) {

				CollectorNum = FlatPlateUnitsNum;

				GetObjectItem( CurrentModuleObject, CollectorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );

				// Collector name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Collector, CollectorNum - 1, IsNotOK, IsBlank, CurrentModuleObject );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				Collector( CollectorNum ).Name = cAlphaArgs( 1 );
				Collector( CollectorNum ).TypeNum = TypeOf_SolarCollectorFlatPlate; // parameter assigned in DataPlant !DSU

				// Get parameters object
				ParametersNum = FindItemInList( cAlphaArgs( 2 ), Parameters );

				if ( ParametersNum == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": " + CurrentModuleParamObject + " object called " + cAlphaArgs( 2 ) + " not found." );
					ErrorsFound = true;
				} else {
					Collector( CollectorNum ).Parameters = ParametersNum;
				}

				// Get surface object
				SurfNum = FindItemInList( cAlphaArgs( 3 ), Surface );

				if ( SurfNum == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Surface " + cAlphaArgs( 3 ) + " not found." );
					ErrorsFound = true;
					continue; // avoid hard crash
				} else {

					if ( ! Surface( SurfNum ).ExtSolar ) {
						ShowWarningError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Surface " + cAlphaArgs( 3 ) + " is not exposed to exterior radiation." );
					}

					// check surface orientation, warn if upside down
					if ( ( Surface( SurfNum ).Tilt < -95.0 ) || ( Surface( SurfNum ).Tilt > 95.0 ) ) {
						ShowWarningError( "Suspected input problem with " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Surface used for solar collector faces down" );
						ShowContinueError( "Surface tilt angle (degrees from ground outward normal) = " + RoundSigDigits( Surface( SurfNum ).Tilt, 2 ) );
					}

					// Check to make sure other solar collectors are not using the same surface
					// NOTE:  Must search over all solar collector types
					for ( CollectorNum2 = 1; CollectorNum2 <= NumFlatPlateUnits; ++CollectorNum2 ) {
						if ( Collector( CollectorNum2 ).Surface == SurfNum ) {
							ShowSevereError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Surface " + cAlphaArgs( 3 ) + " is referenced by more than one " + CurrentModuleObject );
							ErrorsFound = true;
							break;
						}
					} // CollectorNum2

					Collector( CollectorNum ).Surface = SurfNum;
				}

				// Give warning if surface area and gross area do not match within tolerance
				if ( SurfNum > 0 && ParametersNum > 0 && Parameters( ParametersNum ).Area > 0.0 && std::abs( Parameters( ParametersNum ).Area - Surface( SurfNum ).Area ) / Surface( SurfNum ).Area > 0.01 ) {

					ShowWarningError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Gross Area of solar collector parameters and surface object differ by more than 1%." );
					ShowContinueError( "Area of surface object will be used in all calculations." );
				}

				Collector( CollectorNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				Collector( CollectorNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

				if ( NumNumbers > 0 ) {
					Collector( CollectorNum ).VolFlowRateMax = rNumericArgs( 1 ); // Max volumetric flow rate used for plant sizing calculation
				} else {
					Collector( CollectorNum ).VolFlowRateMax = 0.0; // Max vol flow rate is not specified; no flow for plant sizing calculation
					Collector( CollectorNum ).MassFlowRateMax = 999999.9; // But...set a very high value so that it demands as much as possible
				}

				// Setup report variables
				SetupOutputVariable( "Solar Collector Incident Angle Modifier []", Collector( CollectorNum ).IncidentAngleModifier, "System", "Average", Collector( CollectorNum ).Name );

				SetupOutputVariable( "Solar Collector Efficiency []", Collector( CollectorNum ).Efficiency, "System", "Average", Collector( CollectorNum ).Name );

				SetupOutputVariable( "Solar Collector Heat Transfer Rate [W]", Collector( CollectorNum ).Power, "System", "Average", Collector( CollectorNum ).Name );

				SetupOutputVariable( "Solar Collector Heat Gain Rate [W]", Collector( CollectorNum ).HeatGain, "System", "Average", Collector( CollectorNum ).Name );

				SetupOutputVariable( "Solar Collector Heat Loss Rate [W]", Collector( CollectorNum ).HeatLoss, "System", "Average", Collector( FlatPlateUnitsNum ).Name );

				SetupOutputVariable( "Solar Collector Heat Transfer Energy [J]", Collector( CollectorNum ).Energy, "System", "Sum", Collector( FlatPlateUnitsNum ).Name, _, "SolarWater", "HeatProduced", _, "Plant" );

				TestCompSet( CurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Water Nodes" );

			} // FlatPlateUnitsNum

			// Get data for ICS collector
			CurrentModuleParamObject = "SolarCollectorPerformance:IntegralCollectorStorage";

			for ( ICSParamNum = 1; ICSParamNum <= NumOfICSParam; ++ICSParamNum ) {

				ParametersNum = ICSParamNum + NumOfFlatPlateParam;

				GetObjectItem( CurrentModuleParamObject, ICSParamNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );

				// Collector module parameters name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Parameters, ParametersNum - 1, IsNotOK, IsBlank, CurrentModuleParamObject );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				Parameters( ParametersNum ).Name = cAlphaArgs( 1 );
				// NOTE:  currently the only available choice is RectangularTank.  In the future progressive tube type will be
				//        added
				if ( SameString( cAlphaArgs( 2 ), "RectangularTank" ) ) {
					Parameters( ParametersNum ).ICSType_Num = ICSRectangularTank;
				} else {
					ShowSevereError( cAlphaFieldNames( 2 ) + " not found=" + cAlphaArgs( 2 ) + " in " + CurrentModuleParamObject + " =" + Parameters( ParametersNum ).Name );
					ErrorsFound = true;
				}
				// NOTE:  This collector gross area is used in all the calculations.
				Parameters( ParametersNum ).Area = rNumericArgs( 1 );
				if ( rNumericArgs( 1 ) <= 0.0 ) {
					ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Illegal " + cNumericFieldNames( 1 ) + " = " + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
					ShowContinueError( " Collector gross area must be always gretaer than zero." );
					ErrorsFound = true;
				}
				Parameters( ParametersNum ).Volume = rNumericArgs( 2 );
				if ( rNumericArgs( 2 ) <= 0.0 ) {
					ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Illegal " + cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
					ShowContinueError( " Collector water volume must be always gretaer than zero." );
					ErrorsFound = true;
				}
				// Note: this value is used to calculate the heat loss through the bottom and side of the collector
				//Parameters(ParametersNum)%ULoss = rNumericArgs(3)
				Parameters( ParametersNum ).ULossBottom = rNumericArgs( 3 );
				Parameters( ParametersNum ).ULossSide = rNumericArgs( 4 );
				Parameters( ParametersNum ).AspectRatio = rNumericArgs( 5 );
				Parameters( ParametersNum ).SideHeight = rNumericArgs( 6 );
				Parameters( ParametersNum ).ThermalMass = rNumericArgs( 7 );
				Parameters( ParametersNum ).NumOfCovers = rNumericArgs( 8 );
				Parameters( ParametersNum ).CoverSpacing = rNumericArgs( 9 );

				if ( Parameters( ParametersNum ).NumOfCovers == 2 ) {
					// Outer cover refractive index
					Parameters( ParametersNum ).RefractiveIndex( 1 ) = rNumericArgs( 10 );
					// Outer cover extinction coefficient times thickness of the cover
					Parameters( ParametersNum ).ExtCoefTimesThickness( 1 ) = rNumericArgs( 11 );
					// Outer cover Emissivity
					Parameters( ParametersNum ).EmissOfCover( 1 ) = rNumericArgs( 12 );

					if ( ! lNumericFieldBlanks( 13 ) || ! lNumericFieldBlanks( 14 ) || ! lNumericFieldBlanks( 15 ) ) {
						Parameters( ParametersNum ).RefractiveIndex( 2 ) = rNumericArgs( 13 );
						Parameters( ParametersNum ).ExtCoefTimesThickness( 2 ) = rNumericArgs( 14 );
						Parameters( ParametersNum ).EmissOfCover( 2 ) = rNumericArgs( 15 );
					} else {
						ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Illegal input for one of the three inputs of the inner cover optical properties" );
						ErrorsFound = true;
					}
				} else if ( Parameters( ParametersNum ).NumOfCovers == 1 ) {
					// Outer cover refractive index
					Parameters( ParametersNum ).RefractiveIndex( 1 ) = rNumericArgs( 10 );
					// Outer cover extinction coefficient times thickness of the cover
					Parameters( ParametersNum ).ExtCoefTimesThickness( 1 ) = rNumericArgs( 11 );
					// Outer cover emissivity
					Parameters( ParametersNum ).EmissOfCover( 1 ) = rNumericArgs( 12 );
				} else {
					ShowSevereError( CurrentModuleParamObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Illegal " + cNumericFieldNames( 8 ) + " = " + RoundSigDigits( rNumericArgs( 8 ), 2 ) );
					ErrorsFound = true;
				}
				// Solar absorptance of the absorber plate
				Parameters( ParametersNum ).AbsorOfAbsPlate = rNumericArgs( 16 );
				// thermal emmissivity of the absorber plate
				Parameters( ParametersNum ).EmissOfAbsPlate = rNumericArgs( 17 );

			} // end of ParametersNum

			if ( ErrorsFound ) ShowFatalError( "Errors in " + CurrentModuleParamObject + " input." );

			CurrentModuleObject = "SolarCollector:IntegralCollectorStorage";

			for ( ICSUnitsNum = 1; ICSUnitsNum <= NumOfICSUnits; ++ICSUnitsNum ) {

				CollectorNum = ICSUnitsNum + NumFlatPlateUnits;

				GetObjectItem( CurrentModuleObject, ICSUnitsNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				// Collector name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Collector, CollectorNum - 1, IsNotOK, IsBlank, CurrentModuleObject );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				Collector( CollectorNum ).Name = cAlphaArgs( 1 );
				Collector( CollectorNum ).TypeNum = TypeOf_SolarCollectorICS; // parameter assigned in DataPlant

				Collector( CollectorNum ).InitICS = true;

				// Get parameters object
				ParametersNum = FindItemInList( cAlphaArgs( 2 ), Parameters );

				if ( ParametersNum == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": " + CurrentModuleParamObject + " object called " + cAlphaArgs( 2 ) + " not found." );
					ErrorsFound = true;
				} else {
					Collector( CollectorNum ).Parameters = ParametersNum;
				}

				if ( ParametersNum > 0 ) {
					// Calculate constant collector parameters only once
					Perimeter = 2.0 * std::sqrt( Parameters( ParametersNum ).Area ) * ( std::sqrt( Parameters( ParametersNum ).AspectRatio ) + 1.0 / std::sqrt( Parameters( ParametersNum ).AspectRatio ) );
					Collector( CollectorNum ).Length = std::sqrt( Parameters( ParametersNum ).Area / Parameters( ParametersNum ).AspectRatio );

					// calculate the collector side heat transfer area and loss coefficient
					Collector( CollectorNum ).ICSType_Num = Parameters( ParametersNum ).ICSType_Num;
					Collector( CollectorNum ).Area = Parameters( ParametersNum ).Area;
					Collector( CollectorNum ).Volume = Parameters( ParametersNum ).Volume;
					Collector( CollectorNum ).SideArea = Perimeter * Parameters( ParametersNum ).SideHeight;
					Collector( CollectorNum ).AreaRatio = Collector( CollectorNum ).SideArea / Collector( CollectorNum ).Area;
				}
				// Get surface object
				SurfNum = FindItemInList( cAlphaArgs( 3 ), Surface );

				if ( SurfNum == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Surface " + cAlphaArgs( 3 ) + " not found." );
					ErrorsFound = true;
					continue; // avoid hard crash
				} else {

					if ( ! Surface( SurfNum ).ExtSolar ) {
						ShowWarningError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Surface " + cAlphaArgs( 3 ) + " is not exposed to exterior radiation." );
					}

					// check surface orientation, warn if upside down
					if ( ( Surface( SurfNum ).Tilt < -95.0 ) || ( Surface( SurfNum ).Tilt > 95.0 ) ) {
						ShowWarningError( "Suspected input problem with " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Surface used for solar collector faces down" );
						ShowContinueError( "Surface tilt angle (degrees from ground outward normal) = " + RoundSigDigits( Surface( SurfNum ).Tilt, 2 ) );
					}

					// Check to make sure other solar collectors are not using the same surface
					// NOTE:  Must search over all solar collector types
					for ( CollectorNum2 = 1; CollectorNum2 <= NumOfCollectors; ++CollectorNum2 ) {
						if ( Collector( CollectorNum2 ).Surface == SurfNum ) {
							ShowSevereError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Surface " + cAlphaArgs( 3 ) + " is referenced by more than one " + CurrentModuleObject );
							ErrorsFound = true;
							break;
						}
					} // ICSNum2

					Collector( CollectorNum ).Surface = SurfNum;
				}

				// Give warning if surface area and gross area do not match within tolerance
				if ( SurfNum > 0 && ParametersNum > 0 && Parameters( ParametersNum ).Area > 0.0 && std::abs( Parameters( ParametersNum ).Area - Surface( SurfNum ).Area ) / Surface( SurfNum ).Area > 0.01 ) {

					ShowWarningError( CurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": " );
					ShowContinueError( "Gross area of solar collector parameters and surface object differ by more than 1%." );
					ShowContinueError( "Gross collector area is always used in the calculation.  Modify the surface " );
					ShowContinueError( "coordinates to match its area with collector gross area. Otherwise, the underlying " );
					ShowContinueError( "surface is assumed to be fully shaded when it is not." );
				}

				Collector( CollectorNum ).BCType = cAlphaArgs( 4 );
				if ( SameString( cAlphaArgs( 4 ), "AmbientAir" ) ) {
					Collector( CollectorNum ).OSCMName = "";
				} else if ( SameString( cAlphaArgs( 4 ), "OtherSideConditionsModel" ) ) {
					Collector( CollectorNum ).OSCMName = cAlphaArgs( 5 );
					Collector( CollectorNum ).OSCM_ON = true;
					Found = FindItemInList( Collector( CollectorNum ).OSCMName, OSCM );
					if ( Found == 0 ) {
						ShowSevereError( cAlphaFieldNames( 5 ) + " not found=" + Collector( CollectorNum ).OSCMName + " in " + CurrentModuleObject + " =" + Collector( CollectorNum ).Name );
						ErrorsFound = true;
					}
					//Collector(CollectorNum)%OSCMPtr = Found
					//Surface(SurfNum)%IsICS = .TRUE.
				} else {
					ShowSevereError( cAlphaFieldNames( 5 ) + " not found=" + Collector( CollectorNum ).BCType + " in " + CurrentModuleObject + " =" + Collector( CollectorNum ).Name );
					ErrorsFound = true;
				}

				if ( Collector( CollectorNum ).OSCM_ON ) {
					// get index of ventilated cavity object
					GetExtVentedCavityIndex( SurfNum, VentCavIndex );
					Collector( CollectorNum ).VentCavIndex = VentCavIndex;
				}

				Collector( CollectorNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				Collector( CollectorNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

				if ( NumNumbers > 0 ) {
					Collector( CollectorNum ).VolFlowRateMax = rNumericArgs( 1 ); // Max volumetric flow rate used for plant sizing calculation
				} else {
					Collector( CollectorNum ).VolFlowRateMax = 0.0; // Max vol flow rate is not specified; no flow for plant sizing calculation
					Collector( CollectorNum ).MassFlowRateMax = 999999.9; // But...set a very high value so that it demands as much as possible
				}

				// Setup report variables
				SetupOutputVariable( "Solar Collector Transmittance Absorptance Product []", Collector( CollectorNum ).TauAlpha, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Overall Top Heat Loss Coefficient [W/m2-C]", Collector( CollectorNum ).UTopLoss, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Absorber Plate Temperature [C]", Collector( CollectorNum ).TempOfAbsPlate, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Storage Water Temperature [C]", Collector( CollectorNum ).TempOfWater, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Thermal Efficiency []", Collector( CollectorNum ).Efficiency, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Storage Heat Transfer Rate [W]", Collector( CollectorNum ).StoredHeatRate, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Storage Heat Transfer Energy [J]", Collector( CollectorNum ).StoredHeatEnergy, "System", "Sum", Collector( CollectorNum ).Name, _, "SolarWater", "HeatProduced", _, "Plant" );
				SetupOutputVariable( "Solar Collector Skin Heat Transfer Rate [W]", Collector( CollectorNum ).SkinHeatLossRate, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Skin Heat Transfer Energy [J]", Collector( CollectorNum ).CollHeatLossEnergy, "System", "Sum", Collector( CollectorNum ).Name, _, "SolarWater", "HeatProduced", _, "Plant" );
				SetupOutputVariable( "Solar Collector Heat Transfer Rate [W]", Collector( CollectorNum ).HeatRate, "System", "Average", Collector( CollectorNum ).Name );
				SetupOutputVariable( "Solar Collector Heat Transfer Energy [J]", Collector( CollectorNum ).HeatEnergy, "System", "Sum", Collector( CollectorNum ).Name, _, "SolarWater", "HeatProduced", _, "Plant" );

				TestCompSet( CurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Water Nodes" );

			} // ICSNum

			if ( ErrorsFound ) ShowFatalError( "Errors in " + CurrentModuleObject + " input." );

			if ( NumOfCollectors > 0 ) {
				CheckEquipName.dimension( NumOfCollectors, true );
			}

		}

	}

	void
	InitSolarCollector( int const CollectorNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the solar collector object during the plant simulation.

		// METHODOLOGY EMPLOYED:
		// Inlet and outlet nodes are initialized.  The maximum collector flow rate is requested.

		// Using/Aliasing
		using DataGlobals::SysSizingCalc;
		using DataGlobals::InitConvTemp;
		using DataGlobals::DegToRadians;
		using DataGlobals::TimeStepZone;
		using DataGlobals::TimeStep;
		using DataGlobals::HourOfDay;
		using DataLoopNode::Node;
		using namespace DataPlant;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using DataHVACGlobals::SysTimeElapsed;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "InitSolarCollector" );
		int InletNode;
		int OutletNode;

		Real64 const BigNumber( 9999.9 ); // Component desired mass flow rate

		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool SetLoopIndexFlag; // get loop number flag
		Real64 rho;
		//LOGICAL     :: errFlag
		//  REAL(r64)                                :: Density                ! density of fluid
		bool errFlag; // local error flag
		static Array1D_bool SetDiffRadFlag; // get diffuse radiation flag

		int SurfNum; // Surface object number for collector
		int ParamNum; // Collector parameters object number
		Real64 Tilt; // Surface tilt angle (degrees)
		Real64 Theta; // solar radiation incident angle (radians)
		Real64 TransSys; // cover system solar transmittance
		Real64 RefSys; // cover system solar reflectance
		Real64 AbsCover1; // Inner cover solar absorbtance
		Real64 AbsCover2; // Outer cover solar absorbtance
		Real64 RefSysDiffuse; // cover system solar reflectance
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			SetLoopIndexFlag.allocate( NumOfCollectors );
			SetDiffRadFlag.allocate( NumOfCollectors );
			SetLoopIndexFlag = true;
			SetDiffRadFlag = true;
			MyOneTimeFlag = false;
		}

		if ( SetLoopIndexFlag( CollectorNum ) ) {
			if ( allocated( PlantLoop ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( Collector( CollectorNum ).Name, Collector( CollectorNum ).TypeNum, Collector( CollectorNum ).WLoopNum, Collector( CollectorNum ).WLoopSideNum, Collector( CollectorNum ).WLoopBranchNum, Collector( CollectorNum ).WLoopCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitSolarCollector: Program terminated due to previous condition(s)." );
				}
				SetLoopIndexFlag( CollectorNum ) = false;
			}
		}
		// FLOW:
		InletNode = Collector( CollectorNum ).InletNode;
		OutletNode = Collector( CollectorNum ).OutletNode;

		if ( ! SysSizingCalc && Collector( CollectorNum ).InitSizing ) {
			RegisterPlantCompDesignFlow( InletNode, Collector( CollectorNum ).VolFlowRateMax );
			Collector( CollectorNum ).InitSizing = false;
		}

		if ( BeginEnvrnFlag && Collector( CollectorNum ).Init ) {
			// Clear node initial conditions
			if ( Collector( CollectorNum ).VolFlowRateMax > 0 ) { //CR7425
				rho = GetDensityGlycol( PlantLoop( Collector( CollectorNum ).WLoopNum ).FluidName, InitConvTemp, PlantLoop( Collector( CollectorNum ).WLoopNum ).FluidIndex, RoutineName );

				Collector( CollectorNum ).MassFlowRateMax = Collector( CollectorNum ).VolFlowRateMax * rho;
			} else { //CR7425
				Collector( CollectorNum ).MassFlowRateMax = BigNumber; //CR7425
			} //CR7425

			InitComponentNodes( 0.0, Collector( CollectorNum ).MassFlowRateMax, InletNode, OutletNode, Collector( CollectorNum ).WLoopNum, Collector( CollectorNum ).WLoopSideNum, Collector( CollectorNum ).WLoopBranchNum, Collector( CollectorNum ).WLoopCompNum );

			Collector( CollectorNum ).Init = false;

			if ( Collector( CollectorNum ).InitICS ) {
				Collector( CollectorNum ).TempOfWater = 20.0;
				Collector( CollectorNum ).SavedTempOfWater = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).SavedTempOfAbsPlate = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).TempOfAbsPlate = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).TempOfInnerCover = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).TempOfOuterCover = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).SavedTempOfInnerCover = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).SavedTempOfOuterCover = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).SavedTempCollectorOSCM = Collector( CollectorNum ).TempOfWater;
				//Collector(CollectorNum)%SavedTempOfOutdoorAir = Collector(CollectorNum)%TempOfWater
			}

		}

		if ( ! BeginEnvrnFlag ) Collector( CollectorNum ).Init = true;

		if ( SetDiffRadFlag( CollectorNum ) && Collector( CollectorNum ).InitICS ) {
			// calculates the sky and ground reflective diffuse radiation optical properties (only one time)
			SurfNum = Collector( CollectorNum ).Surface;
			ParamNum = Collector( CollectorNum ).Parameters;

			Tilt = Surface( SurfNum ).Tilt;
			Collector( CollectorNum ).Tilt = Tilt;
			Collector( CollectorNum ).TiltR2V = std::abs( 90.0 - Tilt );
			Collector( CollectorNum ).CosTilt = std::cos( Tilt * DegToRadians );
			Collector( CollectorNum ).SinTilt = std::sin( 1.8 * Tilt * DegToRadians );

			// Diffuse refelectance of the cover for solar radiation diffusely reflected back from the absober
			// plate to the cover.  The diffuse solar radiation reflected back from the absober plate to the
			// cover is represented by the 60 degree equivalent incident angle.  This diffuse reflectance is
			// used to calculate the transmittance - absorptance product (Duffie and Beckman, 1991)
			Theta = 60.0 * DegToRadians;
			CalcTransRefAbsOfCover( CollectorNum, Theta, TransSys, RefSys, AbsCover1, AbsCover2, true, RefSysDiffuse );
			Collector( CollectorNum ).RefDiffInnerCover = RefSysDiffuse;

			// transmittance-absorptance product normal incident:
			Theta = 0.0;
			CalcTransRefAbsOfCover( CollectorNum, Theta, TransSys, RefSys, AbsCover1, AbsCover2 );
			Collector( CollectorNum ).TauAlphaNormal = TransSys * Parameters( ParamNum ).AbsorOfAbsPlate / ( 1.0 - ( 1.0 - Parameters( ParamNum ).AbsorOfAbsPlate ) * Collector( CollectorNum ).RefDiffInnerCover );

			// transmittance-absorptance product for sky diffuse radiation.  Uses equivalent incident angle
			// of sky radiation (radians), and is calculated according to Brandemuehl and Beckman (1980):
			Theta = ( 59.68 - 0.1388 * Tilt + 0.001497 * pow_2( Tilt ) ) * DegToRadians;
			CalcTransRefAbsOfCover( CollectorNum, Theta, TransSys, RefSys, AbsCover1, AbsCover2 );
			Collector( CollectorNum ).TauAlphaSkyDiffuse = TransSys * Parameters( ParamNum ).AbsorOfAbsPlate / ( 1.0 - ( 1.0 - Parameters( ParamNum ).AbsorOfAbsPlate ) * Collector( CollectorNum ).RefDiffInnerCover );
			Collector( CollectorNum ).CoversAbsSkyDiffuse( 1 ) = AbsCover1;
			Collector( CollectorNum ).CoversAbsSkyDiffuse( 2 ) = AbsCover2;

			// transmittance-absorptance product for ground diffuse radiation.  Uses equivalent incident angle
			// of ground radiation (radians), and is calculated according to Brandemuehl and Beckman (1980):
			Theta = ( 90.0 - 0.5788 * Tilt + 0.002693 * pow_2( Tilt ) ) * DegToRadians;
			CalcTransRefAbsOfCover( CollectorNum, Theta, TransSys, RefSys, AbsCover1, AbsCover2 );
			Collector( CollectorNum ).TauAlphaGndDiffuse = TransSys * Parameters( ParamNum ).AbsorOfAbsPlate / ( 1.0 - ( 1.0 - Parameters( ParamNum ).AbsorOfAbsPlate ) * Collector( CollectorNum ).RefDiffInnerCover );
			Collector( CollectorNum ).CoversAbsGndDiffuse( 1 ) = AbsCover1;
			Collector( CollectorNum ).CoversAbsGndDiffuse( 2 ) = AbsCover2;

			SetDiffRadFlag( CollectorNum ) = false;

		}

		Collector( CollectorNum ).InletTemp = Node( InletNode ).Temp;

		Collector( CollectorNum ).MassFlowRate = Collector( CollectorNum ).MassFlowRateMax;

		// Request the mass flow rate from the plant component flow utility routine
		SetComponentFlowRate( Collector( CollectorNum ).MassFlowRate, InletNode, OutletNode, Collector( CollectorNum ).WLoopNum, Collector( CollectorNum ).WLoopSideNum, Collector( CollectorNum ).WLoopBranchNum, Collector( CollectorNum ).WLoopCompNum );

		if ( Collector( CollectorNum ).InitICS ) {

			TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

			SurfNum = Collector( CollectorNum ).Surface;

			if ( Collector( CollectorNum ).TimeElapsed != TimeElapsed ) {
				// The simulation has advanced to the next system timestep.  Save conditions from the end of the previous
				// system timestep for use as initial condition of each iteration that does not advance system timestep.
				Collector( CollectorNum ).SavedTempOfWater = Collector( CollectorNum ).TempOfWater;
				Collector( CollectorNum ).SavedTempOfAbsPlate = Collector( CollectorNum ).TempOfAbsPlate;
				Collector( CollectorNum ).SavedTempOfInnerCover = Collector( CollectorNum ).TempOfInnerCover;
				Collector( CollectorNum ).SavedTempOfOuterCover = Collector( CollectorNum ).TempOfOuterCover;
				if ( Collector( CollectorNum ).OSCM_ON ) {
					GetExtVentedCavityTsColl( Collector( CollectorNum ).VentCavIndex, Collector( CollectorNum ).SavedTempCollectorOSCM );
				}
				Collector( CollectorNum ).TimeElapsed = TimeElapsed;
			}
		}

	}

	void
	CalcSolarCollector( int const CollectorNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the heat gain (or loss), outlet temperature, and solar energy conversion efficiency for a flat-plate
		// solar collector when there is a fluid flow.  For the no flow condition, the fluid stagnation temperature is
		// calculated as the outlet temperature.  Glazed and unglazed collectors are both handled.

		// METHODOLOGY EMPLOYED:
		// Calculation is performed using the methodology described in the ASHRAE standards and references below.  Measured
		// collector performance coefficients (available from the Solar Rating & Certification Corporation, for example)
		// are modified from the test conditions to match the actual optical (incident angle modifier) and thermal (flow rate
		// modifier) conditions.  Water is assumed to be the heat transfer fluid.

		// REFERENCES:
		// ASHRAE Standard 93-1986 (RA 91), "Methods of Testing to Determine the Thermal Performance of Solar Collectors".
		// ASHRAE Standard 96-1980 (RA 89), "Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate
		//   Liquid-Type Solar Collectors".
		// Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.  Wiley-Interscience:
		//   New York (1991).

		// NOTES:
		// This subroutine has been validated against the TRNSYS Type 1 flat-plate solar collector module.  Results are
		// identical except for slight differences at extreme incident angles (>80 degrees) and extreme surface tilts (<20
		// degrees).  The differences are due to the fact that Type 1 does not prevent the *component* incident angle
		// modifiers from being less than zero.  There is an effect on the net incident angle modifier if one or more
		// components are less than zero but the net adds up to greater than zero.  The EnergyPlus subroutine, on the other
		// hand, requires each component incident angle modifier always to be greater than zero.

		// Using/Aliasing
		using DataGlobals::DegToRadians;
		using DataHeatBalance::CosIncidenceAngle;
		using DataHeatBalance::QRadSWOutIncident;
		using DataHeatBalance::QRadSWOutIncidentBeam;
		using DataHeatBalance::QRadSWOutIncidentSkyDiffuse;
		using DataHeatBalance::QRadSWOutIncidentGndDiffuse;
		using DataHeatBalance::TempConvergTol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::ccSimPlantEquipTypes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "CalcSolarCollector" );
		int SurfNum; // Surface object number for collector
		int ParamNum; // Collector parameters object number
		Real64 Tilt; // Surface tilt angle (degrees)
		Real64 IncidentAngleModifier; // Net incident angle modifier combining beam, sky, and ground radiation
		Real64 ThetaBeam; // Incident angle of beam radiation (radians)
		Real64 ThetaSky; // Equivalent incident angle of sky radiation (radians)
		Real64 ThetaGnd; // Equivalent incident angle of ground radiation (radians)
		Real64 InletTemp; // Inlet temperature from plant (C)
		Real64 OutletTemp; // Outlet temperature or stagnation temperature in the collector (C)
		Real64 OutletTempPrev; // Outlet temperature saved from previous iteration for convergence check (C)
		Real64 MassFlowRate; // Mass flow rate through collector (kg/s)
		Real64 Cp; // Specific heat of collector fluid (J/kg-K)
		Real64 Area; // Gross area of collector (m2)
		Real64 mCpATest; // = MassFlowRateTest * Cp / Area (tested area)
		Real64 mCpA; // = MassFlowRate * Cp / Area
		Real64 TestTypeMod; // Modifier for test correlation type:  INLET, AVERAGE, or OUTLET
		Real64 FlowMod; // Modifier for flow rate different from test flow rate
		Real64 FRULpTest; // FR * ULoss "prime" for test conditions = (eff1 + eff2 * deltaT)
		Real64 FpULTest; // F prime * ULoss for test conditions = collector efficiency factor * overall loss coefficient
		Real64 FRTAN; // FR * tau * alpha at normal incidence = Y-intercept of collector efficiency
		Real64 FRUL; // FR * ULoss = 1st order coefficient of collector efficiency
		Real64 FRULT; // FR * ULoss / T = 2nd order coefficent of collector efficiency
		Real64 Q; // Heat gain or loss to collector fluid (W)
		Real64 Efficiency; // Thermal efficiency of solar energy conversion
		Real64 A; // Coefficients for solving the quadratic equation
		Real64 B;
		Real64 C;
		Real64 qEquation; // test for negative value in quadratic equation
		int Iteration; // Counter of iterations until convergence

		// FLOW:
		SurfNum = Collector( CollectorNum ).Surface;
		ParamNum = Collector( CollectorNum ).Parameters;

		// Calculate incident angle modifier
		if ( QRadSWOutIncident( SurfNum ) > 0.0 ) {
			ThetaBeam = std::acos( CosIncidenceAngle( SurfNum ) );

			// Calculate equivalent incident angles for sky and ground radiation according to Brandemuehl and Beckman (1980)
			Tilt = Surface( SurfNum ).Tilt;
			ThetaSky = ( 59.68 - 0.1388 * Tilt + 0.001497 * pow_2( Tilt ) ) * DegToRadians;
			ThetaGnd = ( 90.0 - 0.5788 * Tilt + 0.002693 * pow_2( Tilt ) ) * DegToRadians;

			IncidentAngleModifier = ( QRadSWOutIncidentBeam( SurfNum ) * IAM( ParamNum, ThetaBeam ) + QRadSWOutIncidentSkyDiffuse( SurfNum ) * IAM( ParamNum, ThetaSky ) + QRadSWOutIncidentGndDiffuse( SurfNum ) * IAM( ParamNum, ThetaGnd ) ) / QRadSWOutIncident( SurfNum );
		} else {
			IncidentAngleModifier = 0.0;
		}

		InletTemp = Collector( CollectorNum ).InletTemp;

		MassFlowRate = Collector( CollectorNum ).MassFlowRate;

		Cp = GetSpecificHeatGlycol( PlantLoop( Collector( CollectorNum ).WLoopNum ).FluidName, InletTemp, PlantLoop( Collector( CollectorNum ).WLoopNum ).FluidIndex, RoutineName );

		Area = Surface( SurfNum ).Area;
		mCpA = MassFlowRate * Cp / Area;

		// CR 7920, changed next line to use tested area, not current surface area
		// mCpATest = Parameters(ParamNum)%TestMassFlowRate * Cp / Area
		mCpATest = Parameters( ParamNum ).TestMassFlowRate * Cp / Parameters( Collector( CollectorNum ).Parameters ).Area;

		Iteration = 1;
		OutletTemp = 0.0;
		OutletTempPrev = 999.9; // Set to a ridiculous number so that DO loop runs at least once
		Q = 0.0;

		while ( std::abs( OutletTemp - OutletTempPrev ) > TempConvergTol ) { // Check for temperature convergence

			OutletTempPrev = OutletTemp; // Save previous outlet temperature

			// Modify coefficients depending on test correlation type
			{ auto const SELECT_CASE_var( Parameters( ParamNum ).TestType );
			if ( SELECT_CASE_var == INLET ) {
				FRULpTest = Parameters( ParamNum ).eff1 + Parameters( ParamNum ).eff2 * ( InletTemp - Surface( SurfNum ).OutDryBulbTemp );
				TestTypeMod = 1.0;

			} else if ( SELECT_CASE_var == AVERAGE ) {
				FRULpTest = Parameters( ParamNum ).eff1 + Parameters( ParamNum ).eff2 * ( ( InletTemp + OutletTemp ) * 0.5 - Surface( SurfNum ).OutDryBulbTemp );
				TestTypeMod = 1.0 / ( 1.0 - FRULpTest / ( 2.0 * mCpATest ) );

			} else if ( SELECT_CASE_var == OUTLET ) {
				FRULpTest = Parameters( ParamNum ).eff1 + Parameters( ParamNum ).eff2 * ( OutletTemp - Surface( SurfNum ).OutDryBulbTemp );
				TestTypeMod = 1.0 / ( 1.0 - FRULpTest / mCpATest );
			}}

			FRTAN = Parameters( ParamNum ).eff0 * TestTypeMod;
			FRUL = Parameters( ParamNum ).eff1 * TestTypeMod;
			FRULT = Parameters( ParamNum ).eff2 * TestTypeMod;
			FRULpTest *= TestTypeMod;

			if ( MassFlowRate > 0.0 ) { // Calculate efficiency and heat transfer with flow

				if ( ( 1.0 + FRULpTest / mCpATest ) > 0.0 ) {
					FpULTest = -mCpATest * std::log( 1.0 + FRULpTest / mCpATest );
				} else {
					FpULTest = FRULpTest; // Avoid LOG( <0 )
				}

				if ( ( -FpULTest / mCpA ) < 700.0 ) {
					FlowMod = mCpA * ( 1.0 - std::exp( -FpULTest / mCpA ) );
				} else { // avoid EXP(too large #)
					//FlowMod = FlowMod; // Self-assignment commented out
				}
				if ( ( -FpULTest / mCpATest ) < 700.0 ) {
					FlowMod /= ( mCpATest * ( 1.0 - std::exp( -FpULTest / mCpATest ) ) );
				} else {
					//FlowMod = FlowMod; // Self-assignment commented out
				}

				// Calculate fluid heat gain (or loss)
				// Heat loss is possible if there is no incident radiation and fluid is still flowing.
				Q = ( FRTAN * IncidentAngleModifier * QRadSWOutIncident( SurfNum ) + FRULpTest * ( InletTemp - Surface( SurfNum ).OutDryBulbTemp ) ) * Area * FlowMod;

				OutletTemp = InletTemp + Q / ( MassFlowRate * Cp );

				// CR 7877 bound unreasonable result
				if ( OutletTemp < -100 ) {
					OutletTemp = -100.0;
					Q = MassFlowRate * Cp * ( OutletTemp - InletTemp );
				}
				if ( OutletTemp > 200 ) {
					OutletTemp = 200.0;
					Q = MassFlowRate * Cp * ( OutletTemp - InletTemp );
				}

				if ( QRadSWOutIncident( SurfNum ) > 0.0 ) { // Calculate thermal efficiency
					// NOTE: Efficiency can be > 1 if Q > QRadSWOutIncident because of favorable delta T, i.e. warm outdoor temperature
					Efficiency = Q / ( QRadSWOutIncident( SurfNum ) * Area ); // Q has units of W; QRadSWOutIncident has units of W/m2
				} else {
					Efficiency = 0.0;
				}

			} else { // Calculate stagnation temperature of fluid in collector (no flow)
				Q = 0.0;
				Efficiency = 0.0;

				// Calculate temperature of stagnant fluid in collector
				A = -FRULT;
				B = -FRUL + 2.0 * FRULT * Surface( SurfNum ).OutDryBulbTemp;
				C = -FRULT * pow_2( Surface( SurfNum ).OutDryBulbTemp ) + FRUL * Surface( SurfNum ).OutDryBulbTemp - FRTAN * IncidentAngleModifier * QRadSWOutIncident( SurfNum );
				qEquation = ( pow_2( B ) - 4.0 * A * C );
				if ( qEquation < 0.0 ) {
					if ( Collector( CollectorNum ).ErrIndex == 0 ) {
						ShowSevereMessage( "CalcSolarCollector: " + ccSimPlantEquipTypes( Collector( CollectorNum ).TypeNum ) + "=\"" + Collector( CollectorNum ).Name + "\", possible bad input coefficients." );
						ShowContinueError( "...coefficients cause negative quadratic equation part in calculating temperature of stagnant fluid." );
						ShowContinueError( "...examine input coefficients for accuracy. Calculation will be treated as linear." );
					}
					ShowRecurringSevereErrorAtEnd( "CalcSolarCollector: " + ccSimPlantEquipTypes( Collector( CollectorNum ).TypeNum ) + "=\"" + Collector( CollectorNum ).Name + "\", coefficient error continues.", Collector( CollectorNum ).ErrIndex, qEquation, qEquation );
				}
				if ( FRULT == 0.0 || qEquation < 0.0 ) { // Linear, 1st order solution
					OutletTemp = Surface( SurfNum ).OutDryBulbTemp - FRTAN * IncidentAngleModifier * QRadSWOutIncident( SurfNum ) / FRUL;
				} else { // Quadratic, 2nd order solution
					OutletTemp = ( -B + std::sqrt( qEquation ) ) / ( 2.0 * A );
				}
			}

			if ( Parameters( ParamNum ).TestType == INLET ) break; // Inlet temperature test correlations do not need to iterate

			if ( Iteration > 100 ) {
				if ( Collector( CollectorNum ).IterErrIndex == 0 ) {
					ShowWarningMessage( "CalcSolarCollector: " + ccSimPlantEquipTypes( Collector( CollectorNum ).TypeNum ) + "=\"" + Collector( CollectorNum ).Name + "\":  Solution did not converge." );
				}
				ShowRecurringWarningErrorAtEnd( "CalcSolarCollector: " + ccSimPlantEquipTypes( Collector( CollectorNum ).TypeNum ) + "=\"" + Collector( CollectorNum ).Name + "\", solution not converge error continues.", Collector( CollectorNum ).IterErrIndex );
				break;
			} else {
				++Iteration;
			}

		} // Check for temperature convergence

		Collector( CollectorNum ).IncidentAngleModifier = IncidentAngleModifier;
		Collector( CollectorNum ).Power = Q;
		Collector( CollectorNum ).HeatGain = max( Q, 0.0 );
		Collector( CollectorNum ).HeatLoss = min( Q, 0.0 );
		Collector( CollectorNum ).OutletTemp = OutletTemp;
		Collector( CollectorNum ).Efficiency = Efficiency;

	}

	Real64
	IAM(
		int const ParamNum, // Collector parameters object number
		Real64 const IncidentAngle // Angle of incidence (radians)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   December 2003
		//       MODIFIED       Sept 2008, BG cut off IAM beyond 60 degrees.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the incident angle modifier based on the solar collector parameters.  Both first and second order
		// correlations are allowed.

		// METHODOLOGY EMPLOYED:
		// A simple function.

		// REFERENCES:
		// ASHRAE Standard 93-1986 (RA 91), "Methods of Testing to Determine the Thermal Performance of Solar Collectors".
		// ASHRAE Standard 96-1980 (RA 89), "Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate
		//   Liquid-Type Solar Collectors".
		// Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.  Wiley-Interscience:
		//   New York (1991).
		// Using/Aliasing
		using DataGlobals::DegToRadians;

		// Return value
		Real64 IAM;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		static gio::Fmt fmtLD( "*" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 s; // Intermediate variable
		std::string String; // Dummy string for converting numbers to strings
		Real64 CutoffAngle;
		// FLOW:

		//cut off IAM for angles greater than 60 degrees. (CR 7534)
		CutoffAngle = 60.0 * DegToRadians;
		if ( std::abs( IncidentAngle ) > CutoffAngle ) { // cut off, model curves not robust beyond cutoff
			// curves from FSEC/SRCC testing are only certified to 60 degrees, larger angles can cause numerical problems in curves
			IAM = 0.0;
		} else {

			s = ( 1.0 / std::cos( IncidentAngle ) ) - 1.0;

			IAM = 1.0 + Parameters( ParamNum ).iam1 * s + Parameters( ParamNum ).iam2 * pow_2( s );
			IAM = max( IAM, 0.0 ); // Never allow to be less than zero, but greater than one is a possibility

			if ( IAM > 10.0 ) { // Greater than 10 is probably not a possibility
				ShowSevereError( "IAM Function: SolarCollectorPerformance:FlatPlate = " + Parameters( ParamNum ).Name + ":  Incident Angle Modifier is out of bounds due to bad coefficients." );
				gio::write( String, fmtLD ) << Parameters( ParamNum ).iam1;
				ShowContinueError( "Coefficient 2 of Incident Angle Modifier =" + String );
				gio::write( String, fmtLD ) << Parameters( ParamNum ).iam2;
				ShowContinueError( "Coefficient 3 of Incident Angle Modifier =" + String );
				gio::write( String, fmtLD ) << IAM;
				ShowContinueError( "Calculated Incident Angle Modifier =" + String );
				ShowContinueError( "Expected Incident Angle Modifier should be approximately 1.5 or less." );
				ShowFatalError( "Errors in SolarCollectorPerformance:FlatPlate input." );
			}

		} // not greater than cut off angle

		return IAM;

	}

	void
	CalcICSSolarCollector( int const ColleNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the heat transfered (gain or loss), energy stored, skin heat loss, outlet temperature, solar energy
		// conversion efficiency, and transmittance-absorptance product of an ICS solar collector.

		// METHODOLOGY EMPLOYED:
		// The governing equations for the absorber and collector water heat balance equations are solved simultaneously.
		// The two coupled first ODE are solved analytically.
		// The transmittance-absorptance product of the collector cover-absorber system is calcuated using ray tracing
		// method according to Duffie and Beckman(1991).
		// REFERENCES:
		// Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, 2nd. Edition.  Wiley-Interscience:
		// New York (1991).
		// NOTES:

		// Using/Aliasing
		using DataGlobals::DegToRadians;
		using DataGlobals::TimeStepZone;
		using DataGlobals::TimeStep;
		using DataGlobals::SecInHour;
		using DataGlobals::HourOfDay;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using DataHeatBalance::CosIncidenceAngle;
		using DataHeatBalance::QRadSWOutIncident;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcICSSolarCollector" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  REAL(r64) :: TimeElapsed             ! Fraction of the current hour that has elapsed (h)
		int SurfNum; // Surface object number for collector
		int ParamNum; // Collector parameters object number
		Real64 ThetaBeam; // Incident angle of beam radiation (radians)
		Real64 InletTemp; // Inlet temperature from plant (C)
		Real64 OutletTemp; // collector Outlet temperature (C)
		Real64 MassFlowRate; // Mass flow rate through collector (kg/s)
		Real64 TempAbsPlate; // absorber plate average temperature [C]
		Real64 TempAbsPlateOld; // absorber plate average temperature at previous time step [C]
		Real64 TempWater; // collector water average temperature [C]
		Real64 TempWaterOld; // collector water average temperature at previous time step  [C]
		Real64 TempOutdoorAir; // outdoor air temperature [C]
		Real64 TempOSCM; // Otherside condition model temperature [C]
		Real64 hConvCoefA2W; // convection coeff between absorber plate and water [W/m2K]
		Real64 Cpw; // Specific heat of collector fluid (J/kg-K)
		Real64 Rhow; // density of colloctor fluid (kg/m3)
		Real64 Area; // Gross area of collector (m2)
		Real64 aw; // thermal mass of the collector water [J/K]
		//  REAL(r64) :: bw                      ! coefficients of the ODE of water heat balance
		//  REAL(r64) :: cw                      ! coefficients of the ODE of water heat balance
		Real64 ap; // thermal mass of the absorber plate [J/K]
		//  REAL(r64) :: bp                      ! coefficients of the ODE of abs plate heat balance
		//  REAL(r64) :: cp                      ! coefficients of the ODE of abs plate heat balance
		Real64 SecInTimeStep; // Seconds in one timestep (s)

		//  REAL(r64) :: Q                       ! Heat gain or loss to collector fluid (W)
		Real64 Efficiency; // Thermal efficiency of solar energy conversion
		//  REAL(r64) :: StoredHeatRate          ! collector heat storage rate (-ve, or +ve) [W]
		//  REAL(r64) :: HeatLossRate            ! heat loss through the top, bottom and side of collector
		//  REAL(r64) :: CollectorHeatRate       ! collector net heat gain rate
		Real64 QHeatRate; // heat gain rate (W)
		Real64 a1; // coefficient of ODE for absorber temperature Tp
		Real64 a2; // coefficient of ODE for absorber temperature Tw
		Real64 a3; // conatant term of ODE for absorber temperature
		Real64 b1; // coefficient of ODE for water temperature Tp
		Real64 b2; // coefficient of ODE for water temperature Tw
		Real64 b3; // conatant term of ODE for water temperature
		bool AbsPlateMassFlag; // flag if the absober has thermal mass or not

		// FLOW:
		Efficiency = 0.0;
		QHeatRate = 0.0;

		SecInTimeStep = TimeStepSys * SecInHour;
		SurfNum = Collector( ColleNum ).Surface;
		ParamNum = Collector( ColleNum ).Parameters;
		Area = Parameters( ParamNum ).Area;
		TempWater = Collector( ColleNum ).SavedTempOfWater;
		TempAbsPlate = Collector( ColleNum ).SavedTempOfAbsPlate;
		TempOutdoorAir = Surface( SurfNum ).OutDryBulbTemp;
		if ( Collector( ColleNum ).OSCM_ON ) {
			TempOSCM = Collector( ColleNum ).SavedTempCollectorOSCM;
		} else {
			TempOSCM = TempOutdoorAir;
		}

		// Calculate transmittance-absorptance product of the system
		ThetaBeam = std::acos( CosIncidenceAngle( SurfNum ) );
		CalcTransAbsorProduct( ColleNum, ThetaBeam );

		InletTemp = Collector( ColleNum ).InletTemp;

		MassFlowRate = Collector( ColleNum ).MassFlowRate;

		Cpw = GetSpecificHeatGlycol( PlantLoop( Collector( ColleNum ).WLoopNum ).FluidName, InletTemp, PlantLoop( Collector( ColleNum ).WLoopNum ).FluidIndex, RoutineName );

		Rhow = GetDensityGlycol( PlantLoop( Collector( ColleNum ).WLoopNum ).FluidName, InletTemp, PlantLoop( Collector( ColleNum ).WLoopNum ).FluidIndex, RoutineName );

		// calculate heat transfer coefficients and covers temperature:
		CalcHeatTransCoeffAndCoverTemp( ColleNum );

		// Calc convection heat transfer coefficient between the absorber plate and water:
		hConvCoefA2W = CalcConvCoeffAbsPlateAndWater( TempAbsPlate, TempWater, Collector( ColleNum ).Length, Collector( ColleNum ).TiltR2V );
		TempWaterOld = TempWater;
		TempAbsPlateOld = TempAbsPlate;

		if ( Parameters( ParamNum ).ThermalMass > 0.0 ) {
			AbsPlateMassFlag = true;
			ap = Parameters( ParamNum ).ThermalMass * Area;
			a1 = -Area * ( hConvCoefA2W + Collector( ColleNum ).UTopLoss ) / ap;
			a2 = Area * hConvCoefA2W / ap;
			a3 = Area * ( Collector( ColleNum ).TauAlpha * QRadSWOutIncident( SurfNum ) + Collector( ColleNum ).UTopLoss * TempOutdoorAir ) / ap;
		} else {
			AbsPlateMassFlag = false;
			a1 = -Area * ( hConvCoefA2W + Collector( ColleNum ).UTopLoss );
			a2 = Area * hConvCoefA2W;
			a3 = Area * ( Collector( ColleNum ).TauAlpha * QRadSWOutIncident( SurfNum ) + Collector( ColleNum ).UTopLoss * TempOutdoorAir );
		}
		aw = Parameters( ParamNum ).Volume * Rhow * Cpw;
		b1 = Area * hConvCoefA2W / aw;
		b2 = -( Area * ( hConvCoefA2W + Collector( ColleNum ).UbLoss + Collector( ColleNum ).UsLoss ) + MassFlowRate * Cpw ) / aw;
		b3 = ( Area * ( Collector( ColleNum ).UbLoss * TempOSCM + Collector( ColleNum ).UsLoss * TempOutdoorAir ) + MassFlowRate * Cpw * InletTemp ) / aw;

		ICSCollectorAnalyticalSoluton( ColleNum, SecInTimeStep, a1, a2, a3, b1, b2, b3, TempAbsPlateOld, TempWaterOld, TempAbsPlate, TempWater, AbsPlateMassFlag );

		Collector( ColleNum ).SkinHeatLossRate = Area * ( Collector( ColleNum ).UTopLoss * ( TempOutdoorAir - TempAbsPlate ) + Collector( ColleNum ).UsLoss * ( TempOutdoorAir - TempWater ) + Collector( ColleNum ).UbLoss * ( TempOSCM - TempWater ) );
		Collector( ColleNum ).StoredHeatRate = aw * ( TempWater - TempWaterOld ) / SecInTimeStep;

		QHeatRate = MassFlowRate * Cpw * ( TempWater - InletTemp );
		Collector( ColleNum ).HeatRate = QHeatRate;
		Collector( ColleNum ).HeatGainRate = max( 0.0, QHeatRate );
		Collector( ColleNum ).HeatLossRate = min( 0.0, QHeatRate );

		OutletTemp = TempWater;
		Collector( ColleNum ).OutletTemp = OutletTemp;
		Collector( ColleNum ).TempOfWater = TempWater;
		Collector( ColleNum ).TempOfAbsPlate = TempAbsPlate;

		if ( QRadSWOutIncident( SurfNum ) > 0.0 ) {
			Efficiency = ( Collector( ColleNum ).HeatGainRate + Collector( ColleNum ).StoredHeatRate ) / ( QRadSWOutIncident( SurfNum ) * Area );
			if ( Efficiency < 0.0 ) Efficiency = 0.0;
		}
		Collector( ColleNum ).Efficiency = Efficiency;

	}

	void
	ICSCollectorAnalyticalSoluton(
		int const EP_UNUSED( ColleNum ), // solar collector index
		Real64 const SecInTimeStep, // seconds in a time step
		Real64 const a1, // coefficient of ODE for Tp
		Real64 const a2, // coefficient of ODE for Tp
		Real64 const a3, // coefficient of ODE for Tp
		Real64 const b1, // coefficient of ODE for TW
		Real64 const b2, // coefficient of ODE for TW
		Real64 const b3, // coefficient of ODE for TW
		Real64 const TempAbsPlateOld, // absorber plate temperature at previous time step [C]
		Real64 const TempWaterOld, // collector water temperature at previous time step [C]
		Real64 & TempAbsPlate, // absorber plate temperature at current time step [C]
		Real64 & TempWater, // collector water temperature at current time step [C]
		bool const AbsorberPlateHasMass // flag for absober thermal mass
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the absorber plate and collector water temperatures.
		// METHODOLOGY EMPLOYED:
		// Analytical method: Solves the coupled absorber plate and collector water energy balance
		// equations.  The two non-homogeneous ordinary differential equations of the form.
		//          Tp' = a1*Tp + a2*Tw + a3.
		//          Tw' = b1*Tp + b2*Tw + b3.
		// The general solution of these coupled equation with real routes has the following form:
		//          Tp = ConstantC1*exp(lamda1*t) + ConstantC2*exp(lamda2*t) + ConstOfTpSln
		//          Tw = r1*ConstantC2*exp(lamda1*t) + r2*ConstantC2*exp(lamda2*t) + ConstOfTwSln
		// REFERENCES:
		// NOTES:

		// Using/Aliasing
		using DataGlobals::DegToRadians;
		using DataGlobals::TimeStepZone;
		using DataGlobals::TimeStep;
		using DataGlobals::SecInHour;
		using DataGlobals::HourOfDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 BSquareM4TimesATimesC; // intermediate variable
		Real64 r1; // ratio of the ODE solution constant coefficients
		Real64 r2; // ratio of the ODE solution constant coefficients
		Real64 ConstantC1; // coefficient of the ODE solution
		Real64 ConstantC2; // coefficient of the ODE solution
		Real64 DetOfMatrix; // intermediate variable
		Real64 ConstOfTpSln; // the particular solution for the ODE
		Real64 ConstOfTwSln; // the particular solution for the ODE
		Real64 lamda1; // the real roots of the quadratic equation
		Real64 lamda2; // the real roots of the quadratic equation
		Real64 a; // coefficients of quadratic equation a*m2+b*m+c=0
		Real64 b; // coefficients of quadratic equation a*m2+b*m+c=0
		Real64 c; // coefficients of quadratic equation a*m2+b*m+c=0
		// FLOW:

		if ( AbsorberPlateHasMass ) {
			a = 1.0;
			b = -( a1 + b2 );
			c = a1 * b2 - a2 * b1;
			BSquareM4TimesATimesC = pow_2( b ) - 4.0 * a * c;
			if ( BSquareM4TimesATimesC > 0.0 ) {
				lamda1 = ( -b + std::sqrt( BSquareM4TimesATimesC ) ) / ( 2.0 * a );
				lamda2 = ( -b - std::sqrt( BSquareM4TimesATimesC ) ) / ( 2.0 * a );
				DetOfMatrix = c;
				ConstOfTpSln = ( -a3 * b2 + b3 * a2 ) / DetOfMatrix;
				ConstOfTwSln = ( -a1 * b3 + b1 * a3 ) / DetOfMatrix;
				r1 = ( lamda1 - a1 ) / a2;
				r2 = ( lamda2 - a1 ) / a2;
				ConstantC2 = ( TempWaterOld + r1 * ConstOfTpSln - r1 * TempAbsPlateOld - ConstOfTwSln ) / ( r2 - r1 );
				ConstantC1 = ( TempAbsPlateOld - ConstOfTpSln - ConstantC2 );
				TempAbsPlate = ConstantC1 * std::exp( lamda1 * SecInTimeStep ) + ConstantC2 * std::exp( lamda2 * SecInTimeStep ) + ConstOfTpSln;
				TempWater = r1 * ConstantC1 * std::exp( lamda1 * SecInTimeStep ) + r2 * ConstantC2 * std::exp( lamda2 * SecInTimeStep ) + ConstOfTwSln;

			} else { // this should never occur
				ShowSevereError( "ICSCollectorAnalyticalSoluton: Unanticipated differential equation coefficient - report to EnergyPlus Development Team" );
				ShowFatalError( "Program terminates due to above conditions." );
			}
		} else {
			// In the absence of absorber plate thermal mass, only the collector water heat balance has a
			// differential equation of the form: Tw' = b1*Tp + b2*Tw + b3. The absorber plate energy balance
			// equation in the absence of thermal mass is a steady state form:  b1*Tp + b2*Tw + b3 = 0
			b = b2 - b1 * ( a2 / a1 );
			c = b3 - b1 * ( a3 / a1 );
			TempWater = ( TempWaterOld + c / b ) * std::exp( b * SecInTimeStep ) - c / b;
			TempAbsPlate = -( a2 * TempWater + a3 ) / a1;

		}

	}

	void
	CalcTransAbsorProduct(
		int const ColleNum, // Collector object number
		Real64 const IncidAngle // Angle of incidence (radians)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket A Nigusse
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates transmittance-absorptance product and the fraction of total solar radiation
		// absorbed by each cover of a multicover ICS solar collector.

		// METHODOLOGY EMPLOYED:
		// Uses a ray tracing method.

		// REFERENCES:
		// Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
		// Wiley-Interscience: New York (1991).
		// Using/Aliasing
		using DataGlobals::DegToRadians;
		using DataHeatBalance::QRadSWOutIncident;
		using DataHeatBalance::QRadSWOutIncidentBeam;
		using DataHeatBalance::QRadSWOutIncidentSkyDiffuse;
		using DataHeatBalance::QRadSWOutIncidentGndDiffuse;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ParamNum; // Collector parameters object number
		int SurfNum; // Surface object number for collector
		int Num; // covers counter
		Real64 TuaAlphaBeam; // trans-abs product of beam radiation
		Real64 TuaAlpha; // weighted trans-abs product of system
		Real64 TransSys; // cover system solar transmittance
		Real64 ReflSys; // cover system solar reflectance
		Real64 AbsCover1; // Inner cover solar absorbtance
		Real64 AbsCover2; // Outer cover solar absorbtance
		Array1D< Real64 > CoversAbsBeam( 2 ); // Inner and Outer Cover absorptance
		// FLOW:

		// set
		TransSys = 1.0;
		ReflSys = 0.0;
		AbsCover1 = 0.0;
		AbsCover2 = 0.0;
		TuaAlpha = 0.0;
		TuaAlphaBeam = 0.0;
		Collector( ColleNum ).CoverAbs( 1 ) = 0.0;
		Collector( ColleNum ).CoverAbs( 2 ) = 0.0;

		SurfNum = Collector( ColleNum ).Surface;
		ParamNum = Collector( ColleNum ).Parameters;

		if ( QRadSWOutIncident( SurfNum ) > 0.0 ) {

			// cover system transmittance and reflectance from outer to inner cover
			CalcTransRefAbsOfCover( ColleNum, IncidAngle, TransSys, ReflSys, AbsCover1, AbsCover2 );

			TuaAlphaBeam = TransSys * Parameters( ParamNum ).AbsorOfAbsPlate / ( 1.0 - ( 1.0 - Parameters( ParamNum ).AbsorOfAbsPlate ) * Collector( ColleNum ).RefDiffInnerCover );

			Collector( ColleNum ).TauAlphaBeam = max( 0.0, TuaAlphaBeam );
			CoversAbsBeam( 1 ) = AbsCover1;
			CoversAbsBeam( 2 ) = AbsCover2;

			// calc total solar radiation weighted transmittance-absorptance product
			TuaAlpha = ( QRadSWOutIncidentBeam( SurfNum ) * Collector( ColleNum ).TauAlphaBeam + QRadSWOutIncidentSkyDiffuse( SurfNum ) * Collector( ColleNum ).TauAlphaSkyDiffuse + QRadSWOutIncidentGndDiffuse( SurfNum ) * Collector( ColleNum ).TauAlphaGndDiffuse ) / QRadSWOutIncident( SurfNum );

			if ( Parameters( ParamNum ).NumOfCovers == 1 ) {
				// calc total solar radiation weighted cover absorptance
				Collector( ColleNum ).CoverAbs( 1 ) = ( QRadSWOutIncidentBeam( SurfNum ) * CoversAbsBeam( 1 ) + QRadSWOutIncidentSkyDiffuse( SurfNum ) * Collector( ColleNum ).CoversAbsSkyDiffuse( 1 ) + QRadSWOutIncidentGndDiffuse( SurfNum ) * Collector( ColleNum ).CoversAbsGndDiffuse( 1 ) ) / QRadSWOutIncident( SurfNum );

			} else if ( Parameters( ParamNum ).NumOfCovers == 2 ) {
				// Num = 1 represents outer cover and Num = 2 represents inner cover
				for ( Num = 1; Num <= Parameters( ParamNum ).NumOfCovers; ++Num ) {
					Collector( ColleNum ).CoverAbs( Num ) = ( QRadSWOutIncidentBeam( SurfNum ) * CoversAbsBeam( Num ) + QRadSWOutIncidentSkyDiffuse( SurfNum ) * Collector( ColleNum ).CoversAbsSkyDiffuse( Num ) + QRadSWOutIncidentGndDiffuse( SurfNum ) * Collector( ColleNum ).CoversAbsGndDiffuse( Num ) ) / QRadSWOutIncident( SurfNum );
				}
			}

		} else {
			TuaAlpha = 0.0;
		}
		Collector( ColleNum ).TauAlpha = TuaAlpha;

	}

	void
	CalcTransRefAbsOfCover(
		int const ColleNum, // Collector object number
		Real64 const IncidentAngle, // Angle of incidence (radians)
		Real64 & TransSys, // cover system solar transmittance
		Real64 & ReflSys, // cover system solar reflectance
		Real64 & AbsCover1, // Inner cover solar absorbtance
		Real64 & AbsCover2, // Outer cover solar absorbtance
		Optional_bool_const InOUTFlag, // flag for calc. diffuse solar refl of cover from inside out
		Optional< Real64 > RefSysDiffuse // cover system solar reflectance from inner to outer cover
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket A Nigusse
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the transmitance, reflectance, and absorptance of the collector covers based on
		// solar collector optical parameters specified.

		// METHODOLOGY EMPLOYED:
		// Uses a ray tracing method.

		// REFERENCES:
		// Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
		// Wiley-Interscience: New York (1991).
		// Using/Aliasing
		using DataGlobals::DegToRadians;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const AirRefIndex( 1.0003 ); // refractive index of air

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  CHARACTER(len=MaxNameLength):: String       ! Dummy string for converting numbers to strings
		int nCover; // covers count
		int ParamNum; // Collector parameters object number
		Real64 IncAngle; // angle of incidence
		Real64 RefrAngle; // angle of refraction
		Real64 ParaRad; // parallel reflected component of unpolarized solar radiation
		Real64 PerpRad; // Perpendicular reflected component of unpolarized solar radiation
		Array1D< Real64 > TransPara( 2 ); // cover transmittance parallel component
		Array1D< Real64 > TransPerp( 2 ); // cover transmittance perpendicular component
		Array1D< Real64 > ReflPara( 2 ); // cover reflectance parallel component
		Array1D< Real64 > ReflPerp( 2 ); // cover reflectance Perpendicular component
		Array1D< Real64 > AbsorPara( 2 ); // cover absorbtance parallel component
		Array1D< Real64 > AbsorPerp( 2 ); // cover absorbtance Perpendicular component
		Array1D< Real64 > TransAbsOnly( 2 ); // cover transmittance with absorptance only considered
		Real64 CoverRefrIndex; // refractive index of collector cover
		Real64 TransSysDiff; // cover system solar transmittance from inner to outer cover
		bool DiffRefFlag; // flag for calc. diffuse refl of cover from inside to outside
		//  LOGICAL      :: OneTimeFlag                 ! allows to run only once
		// FLOW:

		// set
		TransPerp = 1.0;
		TransPara = 1.0;
		ReflPerp = 0.0;
		ReflPara = 0.0;
		AbsorPerp = 0.0;
		AbsorPara = 0.0;
		TransAbsOnly = 1.0;
		TransSys = 0.0;
		ReflSys = 0.0;
		AbsCover1 = 0.0;
		AbsCover2 = 0.0;

		if ( present( InOUTFlag ) ) {
			DiffRefFlag = InOUTFlag;
		} else {
			DiffRefFlag = false;
		}

		// get the incidence and refraction angles
		IncAngle = IncidentAngle;
		ParamNum = Collector( ColleNum ).Parameters;

		for ( nCover = 1; nCover <= Parameters( ParamNum ).NumOfCovers; ++nCover ) {

			CoverRefrIndex = Parameters( ParamNum ).RefractiveIndex( nCover );

			RefrAngle = std::asin( std::sin( IncAngle ) * AirRefIndex / CoverRefrIndex );

			// transmitted component with absorption only considered:
			TransAbsOnly( nCover ) = std::exp( -Parameters( ParamNum ).ExtCoefTimesThickness( nCover ) / std::cos( RefrAngle ) );

			// parallel and perpendicular reflection components:
			if ( IncAngle == 0.0 ) {
				ParaRad = pow_2( ( CoverRefrIndex - AirRefIndex ) / ( CoverRefrIndex + AirRefIndex ) );
				PerpRad = pow_2( ( CoverRefrIndex - AirRefIndex ) / ( CoverRefrIndex + AirRefIndex ) );
			} else {
				ParaRad = pow_2( std::tan( RefrAngle - IncAngle ) ) / pow_2( std::tan( RefrAngle + IncAngle ) );
				PerpRad = pow_2( std::sin( RefrAngle - IncAngle ) ) / pow_2( std::sin( RefrAngle + IncAngle ) );
			}

			// parallel and perpendicular transmitted components:
			TransPerp( nCover ) = TransAbsOnly( nCover ) * ( ( 1.0 - PerpRad ) / ( 1.0 + PerpRad ) ) * ( ( 1.0 - pow_2( PerpRad ) ) / ( 1.0 - pow_2( PerpRad * TransAbsOnly( nCover ) ) ) );
			TransPara( nCover ) = TransAbsOnly( nCover ) * ( ( 1.0 - ParaRad ) / ( 1.0 + ParaRad ) ) * ( ( 1.0 - pow_2( ParaRad ) ) / ( 1.0 - pow_2( ParaRad * TransAbsOnly( nCover ) ) ) );

			ReflPerp( nCover ) = ( PerpRad + ( pow_2( 1.0 - PerpRad ) * pow_2( TransAbsOnly( nCover ) ) * PerpRad ) / ( 1.0 - pow_2( PerpRad * TransAbsOnly( nCover ) ) ) );
			ReflPara( nCover ) = ( ParaRad + ( pow_2( 1.0 - ParaRad ) * pow_2( TransAbsOnly( nCover ) ) * ParaRad ) / ( 1.0 - pow_2( ParaRad * TransAbsOnly( nCover ) ) ) );

			AbsorPerp( nCover ) = 1.0 - TransPerp( nCover ) - ReflPerp( nCover );
			AbsorPara( nCover ) = 1.0 - TransPara( nCover ) - ReflPara( nCover );
		}

		// solar absorptance of the individual cover
		AbsCover1 = 0.5 * ( AbsorPerp( 1 ) + AbsorPara( 1 ) );
		if ( Parameters( ParamNum ).NumOfCovers == 2 ) AbsCover2 = 0.5 * ( AbsorPerp( 2 ) + AbsorPara( 2 ) );

		// calculate from outer to inner cover:
		TransSys = 0.5 * ( TransPerp( 1 ) * TransPerp( 2 ) / ( 1.0 - ReflPerp( 1 ) * ReflPerp( 2 ) ) + TransPara( 1 ) * TransPara( 2 ) / ( 1.0 - ReflPara( 1 ) * ReflPara( 2 ) ) );
		ReflSys = 0.5 * ( ReflPerp( 1 ) + TransSys * ReflPerp( 2 ) * TransPerp( 1 ) / TransPerp( 2 ) + ReflPara( 1 ) + TransSys * ReflPara( 2 ) * TransPara( 1 ) / TransPara( 2 ) );
		if ( DiffRefFlag ) {
			// calculate from inner to outer cover:
			TransSysDiff = 0.5 * ( TransPerp( 2 ) * TransPerp( 1 ) / ( 1.0 - ReflPerp( 2 ) * ReflPerp( 1 ) ) + TransPara( 2 ) * TransPara( 1 ) / ( 1.0 - ReflPara( 2 ) * ReflPara( 1 ) ) );
			RefSysDiffuse = 0.5 * ( ReflPerp( 2 ) + TransSysDiff * ReflPerp( 1 ) * TransPerp( 2 ) / TransPerp( 1 ) + ReflPara( 2 ) + TransSysDiff * ReflPara( 1 ) * TransPara( 2 ) / TransPara( 1 ) );
		}

	}

	void
	CalcHeatTransCoeffAndCoverTemp( int const ColleNum ) // Collector object number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket A Nigusse, FSEC/UCF
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the various heat transfer coefficients, and collector cover temperatures.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
		// Wiley-Interscience: New York (1991).
		// Using/Aliasing
		using DataGlobals::StefanBoltzmann;
		using DataGlobals::KelvinConv;
		using DataEnvironment::SkyTemp;
		using DataEnvironment::SkyTempKelvin;
		using DataEnvironment::GroundTemp;
		using DataEnvironment::GroundTempKelvin;
		using DataHeatBalance::QRadSWOutIncident;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  CHARACTER(len=MaxNameLength):: String        ! Dummy string for converting numbers to strings
		int ParamNum; // collector parameters object number
		int CoverNum; // counter for number of covers
		int NumCovers; // number of covers
		int SurfNum; // surface number
		int Num; // covers counter
		Real64 tempnom; // intermediate variable
		Real64 tempdenom; // intermediate variable
		Real64 AirGapDepth; // characteristic length [m]
		Real64 TempAbsPlate; // absorber plate average temperature [C]
		Real64 TempInnerCover; // inner cover average temperature [C]
		Real64 TempOuterCover; // outer cover average temperature [C]
		Real64 TempOutdoorAir; // outdoor air temperature [C]
		Real64 EmissOfAbsPlate; // emissivity of absorber plate
		Real64 EmissOfInnerCover; // emissivity of inner cover
		Real64 EmissOfOuterCover; // emissivity of outer cover
		Real64 UTopLoss; // over all top heat loss coefficient [W/m2C]
		Real64 hRadCoefC2Sky; // radiation coeff from collector to the sky [W/m2C]
		Real64 hRadCoefC2Gnd; // radiation coeff from collector to the ground [W/m2C]
		Real64 hRadConvOut; // combined convection-radiation coefficient [W/m2C]
		Real64 hConvCoefA2C; // convection coeff. between abs plate and cover [W/m2C]
		Real64 hConvCoefC2C; // convection coeff. between covers [W/m2C]
		Real64 hConvCoefC2O; // convection coeff. between outer cover and the ambient [W/m2C]
		Real64 hRadCoefA2C; // radiation coeff. between abs plate and cover [W/m2C]
		Real64 hRadCoefC2C; // radiation coeff. between covers [W/m2C]
		Real64 hRadCoefC2O; // radiation coeff. between outer covers and the ambient [W/m2C]

		// flow

		UTopLoss = 0.0;
		ParamNum = Collector( ColleNum ).Parameters;
		NumCovers = Parameters( ParamNum ).NumOfCovers;
		SurfNum = Collector( ColleNum ).Surface;

		TempAbsPlate = Collector( ColleNum ).SavedTempOfAbsPlate;
		TempInnerCover = Collector( ColleNum ).SavedTempOfInnerCover;
		TempOuterCover = Collector( ColleNum ).SavedTempOfOuterCover;
		TempOutdoorAir = Surface( SurfNum ).OutDryBulbTemp;

		EmissOfAbsPlate = Parameters( ParamNum ).EmissOfAbsPlate;
		EmissOfOuterCover = Parameters( ParamNum ).EmissOfCover( 1 );
		EmissOfInnerCover = Parameters( ParamNum ).EmissOfCover( 2 );
		AirGapDepth = Parameters( ParamNum ).CoverSpacing;

		{ auto const SELECT_CASE_var( NumCovers );
		if ( SELECT_CASE_var == 1 ) {
			// calc linearized radiation coefficient
			tempnom = StefanBoltzmann * ( ( TempAbsPlate + KelvinConv ) + ( TempOuterCover + KelvinConv ) ) * ( pow_2( TempAbsPlate + KelvinConv ) + pow_2( TempOuterCover + KelvinConv ) );
			tempdenom = 1.0 / EmissOfAbsPlate + 1.0 / EmissOfOuterCover - 1.0;
			hRadCoefA2C = tempnom / tempdenom;
			hRadCoefC2C = 0.0;
			hConvCoefC2C = 0.0;
			// Calc convection heat transfer coefficient:
			hConvCoefA2C = CalcConvCoeffBetweenPlates( TempAbsPlate, TempOuterCover, AirGapDepth, Collector( ColleNum ).CosTilt, Collector( ColleNum ).SinTilt );
		} else if ( SELECT_CASE_var == 2 ) {
			for ( CoverNum = 1; CoverNum <= NumCovers; ++CoverNum ) {
				if ( CoverNum == 1 ) {
					// calc linearized radiation coefficient
					tempnom = StefanBoltzmann * ( ( TempAbsPlate + KelvinConv ) + ( TempInnerCover + KelvinConv ) ) * ( pow_2( TempAbsPlate + KelvinConv ) + pow_2( TempInnerCover + KelvinConv ) );
					tempdenom = 1.0 / EmissOfAbsPlate + 1.0 / EmissOfInnerCover - 1.0;
					hRadCoefA2C = tempnom / tempdenom;
					// Calc convection heat transfer coefficient:
					hConvCoefA2C = CalcConvCoeffBetweenPlates( TempAbsPlate, TempOuterCover, AirGapDepth, Collector( ColleNum ).CosTilt, Collector( ColleNum ).SinTilt );
				} else {
					// calculate the linearized radiation coeff.
					tempnom = StefanBoltzmann * ( ( TempInnerCover + KelvinConv ) + ( TempOuterCover + KelvinConv ) ) * ( pow_2( TempInnerCover + KelvinConv ) + pow_2( TempOuterCover + KelvinConv ) );
					tempdenom = 1.0 / EmissOfInnerCover + 1.0 / EmissOfOuterCover - 1.0;
					hRadCoefC2C = tempnom / tempdenom;
					// Calc convection heat transfer coefficient:
					hConvCoefC2C = CalcConvCoeffBetweenPlates( TempInnerCover, TempOuterCover, AirGapDepth, Collector( ColleNum ).CosTilt, Collector( ColleNum ).SinTilt );
				}
			}
		}}

		// Calc collector outside surface convection heat transfer coefficient:
		hConvCoefC2O = 2.8 + 3.0 * Surface( SurfNum ).WindSpeed;

		// Calc linearized radiation coefficient between outer cover and the surrounding:
		tempnom = Surface( SurfNum ).ViewFactorSky * EmissOfOuterCover * StefanBoltzmann * ( ( TempOuterCover + KelvinConv ) + SkyTempKelvin ) * ( pow_2( TempOuterCover + KelvinConv ) + pow_2( SkyTempKelvin ) );
		tempdenom = ( TempOuterCover - TempOutdoorAir ) / ( TempOuterCover - SkyTemp );
		if ( tempdenom < 0.0 ) {
			// use approximate linearized radiation coefficient
			hRadCoefC2Sky = tempnom;
		} else if ( tempdenom == 0.0 ) {
			// if temperature difference is zero, no radiation exchange
			hRadCoefC2Sky = 0.0;
		} else {
			hRadCoefC2Sky = tempnom / tempdenom;
		}

		tempnom = Surface( SurfNum ).ViewFactorGround * EmissOfOuterCover * StefanBoltzmann * ( ( TempOuterCover + KelvinConv ) + GroundTempKelvin ) * ( pow_2( TempOuterCover + KelvinConv ) + pow_2( GroundTempKelvin ) );
		tempdenom = ( TempOuterCover - TempOutdoorAir ) / ( TempOuterCover - GroundTemp );
		if ( tempdenom < 0.0 ) {
			// use approximate linearized radiation coefficient
			hRadCoefC2Gnd = tempnom;
		} else if ( tempdenom == 0.0 ) {
			// if temperature difference is zero, no radiation exchange
			hRadCoefC2Gnd = 0.0;
		} else {
			hRadCoefC2Gnd = tempnom / tempdenom;
		}

		// combine the radiation coefficients
		hRadCoefC2O = hRadCoefC2Sky + hRadCoefC2Gnd;

		// calculate the overall top heat loss coefficient:
		if ( NumCovers == 1 ) {
			UTopLoss = 1.0 / ( 1.0 / ( hRadCoefA2C + hConvCoefA2C ) + 1.0 / ( hRadCoefC2O + hConvCoefC2O ) );
		} else {
			UTopLoss = 1.0 / ( 1.0 / ( hRadCoefA2C + hConvCoefA2C ) + 1.0 / ( hRadCoefC2C + hConvCoefC2C ) + 1.0 / ( hRadCoefC2O + hConvCoefC2O ) );
		}
		Collector( ColleNum ).UTopLoss = UTopLoss;

		// calculate the side loss coefficient.  Adds the insulation resistance and the combined
		// convection-radiation coefficients in series.
		hRadConvOut = 5.7 + 3.8 * Surface( SurfNum ).WindSpeed;
		Collector( ColleNum ).UsLoss = 1.0 / ( 1.0 / ( Parameters( ParamNum ).ULossSide * Collector( ColleNum ).AreaRatio ) + 1.0 / ( hRadConvOut * Collector( ColleNum ).AreaRatio ) );

		// the bottom loss coefficient calculation depends on the boundary condition
		if ( Collector( ColleNum ).OSCM_ON ) { // OtherSideConditionsModel
			Collector( ColleNum ).UbLoss = Parameters( ParamNum ).ULossBottom;
		} else { // AmbientAir
			Collector( ColleNum ).UbLoss = 1.0 / ( 1.0 / Parameters( ParamNum ).ULossBottom + 1.0 / hRadConvOut );
		}

		// Calculate current timestep covers temperature
		{ auto const SELECT_CASE_var( NumCovers );
		if ( SELECT_CASE_var == 1 ) {
			tempnom = Collector( ColleNum ).CoverAbs( 1 ) * QRadSWOutIncident( SurfNum ) + TempOutdoorAir * ( hConvCoefC2O + hRadCoefC2O ) + TempAbsPlate * ( hConvCoefA2C + hRadCoefA2C );
			tempdenom = ( hConvCoefC2O + hRadCoefC2O ) + ( hConvCoefA2C + hRadCoefA2C );
			TempOuterCover = tempnom / tempdenom;
		} else if ( SELECT_CASE_var == 2 ) {
			for ( Num = 1; Num <= NumCovers; ++Num ) {
				if ( Num == 1 ) {
					tempnom = Collector( ColleNum ).CoverAbs( Num ) * QRadSWOutIncident( SurfNum ) + TempOutdoorAir * ( hConvCoefC2O + hRadCoefC2O ) + TempInnerCover * ( hConvCoefC2C + hRadCoefC2C );
					tempdenom = ( hConvCoefC2O + hRadCoefC2O ) + ( hConvCoefC2C + hRadCoefC2C );
					TempOuterCover = tempnom / tempdenom;
				} else if ( Num == 2 ) {
					tempnom = Collector( ColleNum ).CoverAbs( Num ) * QRadSWOutIncident( SurfNum ) + TempAbsPlate * ( hConvCoefA2C + hRadCoefA2C ) + TempOuterCover * ( hConvCoefC2C + hRadCoefC2C );
					tempdenom = ( hConvCoefC2C + hRadCoefC2C + hConvCoefA2C + hRadCoefA2C );
					TempInnerCover = tempnom / tempdenom;
				}
			}
		}}
		Collector( ColleNum ).TempOfInnerCover = TempInnerCover;
		Collector( ColleNum ).TempOfOuterCover = TempOuterCover;

	}

	Real64
	CalcConvCoeffBetweenPlates(
		Real64 const TempSurf1, // temperature of surface 1
		Real64 const TempSurf2, // temperature of surface 1
		Real64 const AirGap, // characteristic length [m]
		Real64 const CosTilt, // cosine of surface tilt angle relative to the horizontal
		Real64 const SinTilt // sine of surface tilt angle relative to the horizontal
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   February 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//   Calculates the converction coefficient for an enclosure between two paralel surfaces
		//   at different temperatures.
		// METHODOLOGY EMPLOYED:
		//   Uses empirical correlation by Holands et al (1976) to determine free convection between
		//   inclined parallel plates at different temperature.
		// REFERENCES:
		//   Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, 2nd. Edition.
		//   Wiley-Interscience: New York (1991).
		//   Property data for air at atmospheric pressure were taken from Table A-11, Yunus A Cengel
		//   Heat Transfer: A Practical Approach, McGraw-Hill, Boston, MA, 1998.

		// Using/Aliasing
		using DataGlobals::StefanBoltzmann;
		using DataGlobals::KelvinConv;

		// Return value
		Real64 hConvCoef; // convection coefficient

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const gravity( 9.806 ); // gravitational constant [m/s^2]

		int const NumOfPropDivisions( 11 );
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { -23.15, 6.85, 16.85, 24.85, 26.85, 36.85, 46.85, 56.85, 66.85, 76.85, 126.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.0000161, 0.0000175, 0.000018, 0.0000184, 0.0000185, 0.000019, 0.0000194, 0.0000199, 0.0000203, 0.0000208, 0.0000229 } ); // Viscosity, in kg/(m.s)
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.0223, 0.0246, 0.0253, 0.0259, 0.0261, 0.0268, 0.0275, 0.0283, 0.0290, 0.0297, 0.0331 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, { 0.724, 0.717, 0.714, 0.712, 0.712, 0.711, 0.71, 0.708, 0.707, 0.706, 0.703 } ); // Prandtl number (dimensionless)
		static Array1D< Real64 > const Density( NumOfPropDivisions, { 1.413, 1.271, 1.224, 1.186, 1.177, 1.143, 1.110, 1.076, 1.043, 1.009, 0.883 } ); // Density, in kg/m3

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 CondOfAir; // thermal conductivity of air [W/mK]
		Real64 VisDOfAir; // dynamic viscosity of air [kg/m.s]
		Real64 DensOfAir; // density of air [W/mK]
		Real64 PrOfAir; // Prantle number of air [W/mK]
		Real64 VolExpAir; // volumetric expansion of air [1/K]
		Real64 RaNumCosTilt; // Rayleigh number of air times cosine of collector tilt []
		Real64 DeltaT; // temperature difference between absober plate and water
		Real64 Tref; // reference temperature for fluid properties [c]
		Real64 RaNum; // Rayleigh number
		//  REAL(r64)          :: GrNum                  ! Grashof number
		Real64 NuL; // Nusselt number
		int Index; // property range index
		Real64 InterpFrac; // fraction

		// Flow
		DeltaT = std::abs( TempSurf1 - TempSurf2 );
		Tref = 0.5 * ( TempSurf1 + TempSurf2 );
		Index = 1;
		while ( Index <= NumOfPropDivisions ) {
			if ( Tref < Temps( Index ) ) break; // DO loop
			++Index;
		}

		// Initialize thermal properties of air
		if ( Index == 1 ) {
			VisDOfAir = Mu( Index );
			CondOfAir = Conductivity( Index );
			PrOfAir = Pr( Index );
			DensOfAir = Density( Index );
		} else if ( Index > NumOfPropDivisions ) {
			Index = NumOfPropDivisions;
			VisDOfAir = Mu( Index );
			CondOfAir = Conductivity( Index );
			PrOfAir = Pr( Index );
			DensOfAir = Density( Index );
		} else {
			InterpFrac = ( Tref - Temps( Index - 1 ) ) / ( Temps( Index ) - Temps( Index - 1 ) );
			VisDOfAir = Mu( Index - 1 ) + InterpFrac * ( Mu( Index ) - Mu( Index - 1 ) );
			CondOfAir = Conductivity( Index - 1 ) + InterpFrac * ( Conductivity( Index ) - Conductivity( Index - 1 ) );
			PrOfAir = Pr( Index - 1 ) + InterpFrac * ( Pr( Index ) - Pr( Index - 1 ) );
			DensOfAir = Density( Index - 1 ) + InterpFrac * ( Density( Index ) - Density( Index - 1 ) );
		}

		VolExpAir = 1.0 / ( Tref + KelvinConv );

		RaNum = gravity * pow_2( DensOfAir ) * VolExpAir * PrOfAir * DeltaT * pow_3( AirGap ) / pow_2( VisDOfAir );
		RaNumCosTilt = RaNum * CosTilt;
		if ( RaNum == 0.0 ) {
			NuL = 0.0;
		} else {
			if ( RaNumCosTilt > 1708.0 ) {
				NuL = 1.44 * ( 1.0 - 1708.0 * std::pow( SinTilt, 1.6 ) / ( RaNum * CosTilt ) ) * ( 1.0 - 1708.0 / RaNumCosTilt );
			} else {
				NuL = 0.0;
			}
		}
		if ( RaNumCosTilt > 5830.0 ) {
			NuL += std::pow( RaNumCosTilt / 5830.0 - 1.0, 1.0 / 3.0 );
		}
		++NuL;
		hConvCoef = NuL * CondOfAir / AirGap;

		return hConvCoef;
	}

	Real64
	CalcConvCoeffAbsPlateAndWater(
		Real64 const TAbsorber, // temperature of absorber plate [C]
		Real64 const TWater, // temperature of water [C]
		Real64 const Lc, // characteristic length [m]
		Real64 const TiltR2V // collector tilt angle relative to the vertical [degree]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   February 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates the free converction coefficient between the absorber plate and water.
		// METHODOLOGY EMPLOYED:
		//  The convection coefficient calculation were based on the Fujii and Imura emperical correlations
		// REFERENCES:
		//  T.Fujii, and H.Imura,Natural convection heat transfer from aplate with arbitrary inclination.
		//  International Journal of Heat and Mass Transfer: 15(4), (1972), 755-764.
		// Using/Aliasing
		using DataGlobals::DegToRadians;
		using DataGlobals::StefanBoltzmann;
		using DataGlobals::KelvinConv;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetConductivityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using FluidProperties::GetDensityGlycol;

		// Return value
		Real64 hConvA2W; // convection coefficient, [W/m2K]

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const gravity( 9.806 ); // gravitational constant [m/s^2]
		static std::string const CalledFrom( "SolarCollectors:CalcConvCoeffAbsPlateAndWater" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DensOfWater; // density of water [kg/m3]
		Real64 CondOfWater; // thermal conductivity of water [W/mK]
		Real64 VolExpWater; // volumetric expansion of water, [1/K]
		Real64 VisOfWater; // dynamic viscosity of water [Ns/m2]
		Real64 WaterSpecHeat; // specific heat of water
		Real64 PrOfWater; // Prantle number of water
		//  REAL(r64)          :: RaNumCosTilt           ! Rayleigh number of air times cosine of collector tilt []
		Real64 CosTilt; // cosine of collector tilt angle []
		Real64 DeltaT; // temperature difference between absober plate and water
		Real64 TReference; // reference temperature for fluid properties [c]
		Real64 RaNum; // Rayleigh number
		Real64 GrNum; // Grashof number
		//  REAL(r64)          :: GrcPr                  ! critical Grashof number
		Real64 NuL; // Nusselt number
		int WaterIndex; // fluid type index

		// Flow
		DeltaT = std::abs( TAbsorber - TWater );
		TReference = TAbsorber - 0.25 * ( TAbsorber - TWater );
		// record fluid prop index for water
		WaterIndex = FindGlycol( fluidNameWater );
		// find properties of water - always assume water
		WaterSpecHeat = GetSpecificHeatGlycol( fluidNameWater, max( TReference, 0.0 ), WaterIndex, CalledFrom );
		CondOfWater = GetConductivityGlycol( fluidNameWater, max( TReference, 0.0 ), WaterIndex, CalledFrom );
		VisOfWater = GetViscosityGlycol( fluidNameWater, max( TReference, 0.0 ), WaterIndex, CalledFrom );
		DensOfWater = GetDensityGlycol( fluidNameWater, max( TReference, 0.0 ), WaterIndex, CalledFrom );
		PrOfWater = VisOfWater * WaterSpecHeat / CondOfWater;
		// Requires a different reference temperature for volumetric expansion coefficient
		TReference = TWater - 0.25 * ( TWater - TAbsorber );
		VolExpWater = -( GetDensityGlycol( fluidNameWater, max( TReference, 10.0 ) + 5.0, WaterIndex, CalledFrom ) - GetDensityGlycol( fluidNameWater, max( TReference, 10.0 ) - 5.0, WaterIndex, CalledFrom ) ) / ( 10.0 * DensOfWater );

		GrNum = gravity * VolExpWater * DensOfWater * DensOfWater * PrOfWater * DeltaT * pow_3( Lc ) / pow_2( VisOfWater );
		CosTilt = std::cos( TiltR2V * DegToRadians );
		if ( TAbsorber > TWater ) {
			// hot absorber plate facing down
			if ( std::abs( TiltR2V - 90.0 ) < 1.0 ) {
				// It is a horizontal surface
				RaNum = GrNum * PrOfWater;
				if ( RaNum <= 1708.0 ) {
					NuL = 1.0;
				} else {
					NuL = 0.58 * std::pow( RaNum, 0.20 );
				}
			} else {
				RaNum = GrNum * PrOfWater * CosTilt;
				if ( RaNum <= 1708.0 ) {
					NuL = 1.0;
				} else {
					NuL = 0.56 * root_4( RaNum );
				}
			}
		} else {
			// cold plate facing down or hot plate facing up
			RaNum = GrNum * PrOfWater;
			if ( RaNum > 5.0e8 ) {
				NuL = 0.13 * std::pow( RaNum, 1.0 / 3.0 );
			} else {
				NuL = 0.16 * std::pow( RaNum, 1.0 / 3.0 );
				if ( RaNum <= 1708.0 ) NuL = 1.0;
			}
		}
		hConvA2W = NuL * CondOfWater / Lc;

		return hConvA2W;
	}

	void
	UpdateSolarCollector( int const CollectorNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the node variables with local variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataLoopNode::Node;
		using PlantUtilities::SafeCopyPlantNode;
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "UpdateSolarCollector" );
		int InletNode;
		int OutletNode;
		Real64 Cp;

		// FLOW:
		InletNode = Collector( CollectorNum ).InletNode;
		OutletNode = Collector( CollectorNum ).OutletNode;

		SafeCopyPlantNode( InletNode, OutletNode );
		// Set outlet node variables that are possibly changed
		Node( OutletNode ).Temp = Collector( CollectorNum ).OutletTemp;
		Cp = GetSpecificHeatGlycol( PlantLoop( Collector( CollectorNum ).WLoopNum ).FluidName, Collector( CollectorNum ).OutletTemp, PlantLoop( Collector( CollectorNum ).WLoopNum ).FluidIndex, RoutineName );
		Node( OutletNode ).Enthalpy = Cp * Node( OutletNode ).Temp;

	}

	void
	ReportSolarCollector( int const CollectorNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TimeStepInSecond;

		// FLOW:
		TimeStepInSecond = TimeStepSys * SecInHour;

		Collector( CollectorNum ).Energy = Collector( CollectorNum ).Power * TimeStepInSecond;
		Collector( CollectorNum ).HeatEnergy = Collector( CollectorNum ).HeatRate * TimeStepInSecond;
		Collector( CollectorNum ).HeatGainEnergy = Collector( CollectorNum ).HeatGainRate * TimeStepInSecond;
		Collector( CollectorNum ).HeatLossEnergy = Collector( CollectorNum ).HeatLossRate * TimeStepInSecond;
		Collector( CollectorNum ).CollHeatLossEnergy = Collector( CollectorNum ).SkinHeatLossRate * TimeStepInSecond;
		Collector( CollectorNum ).StoredHeatEnergy = Collector( CollectorNum ).StoredHeatRate * TimeStepInSecond;

	}

	void
	GetExtVentedCavityIndex(
		int const SurfacePtr,
		int & VentCavIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC. Adopted from Photovoltaics module
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Get" routine for establishing correct integer index from outside this module

		// METHODOLOGY EMPLOYED:
		// mine Surface derived type for correct index/number of surface
		// mine  ExtVentedCavity derived type that has the surface.
		// Adapated from Photovoltaics module, originally developed by Brent G. (2004)

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSurfaces::Surface;
		using DataSurfaces::ExtVentedCavity;
		using DataSurfaces::TotExtVentCav;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CavNum; // temporary
		int ThisSurf; // temporary
		int thisCav;
		bool Found;

		if ( SurfacePtr == 0 ) {
			// should be trapped already
			ShowFatalError( "Invalid surface passed to GetExtVentedCavityIndex" );
		}

		CavNum = 0;
		Found = false;
		for ( thisCav = 1; thisCav <= TotExtVentCav; ++thisCav ) {
			for ( ThisSurf = 1; ThisSurf <= ExtVentedCavity( thisCav ).NumSurfs; ++ThisSurf ) {
				if ( SurfacePtr == ExtVentedCavity( thisCav ).SurfPtrs( ThisSurf ) ) {
					Found = true;
					CavNum = thisCav;
				}
			}
		}

		if ( ! Found ) {
			ShowFatalError( "Did not find surface in Exterior Vented Cavity description in GetExtVentedCavityIndex, Surface name = " + Surface( SurfacePtr ).Name );
		} else {

			VentCavIndex = CavNum;

		}

	}

	void
	GetExtVentedCavityTsColl(
		int const VentModNum,
		Real64 & TsColl
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>   Adopted from Photovoltaics module
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Get" routine for collector surface temperature.

		// METHODOLOGY EMPLOYED:
		// access derived type

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataSurfaces::ExtVentedCavity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		TsColl = ExtVentedCavity( VentModNum ).Tbaffle;

	}

} // SolarCollectors

} // EnergyPlus
