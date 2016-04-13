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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ChillerGasAbsorption.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerGasAbsorption {

	// MODULE INFORMATION:
	//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
	//                   for Gas Research Institute
	//    DATE WRITTEN   March 2001
	//    MODIFIED       Brent Griffith, Nov 2010 plant upgrades, generalize fluid properties
	//    RE-ENGINEERED  na
	// PURPOSE OF THIS MODULE:
	//    This module simulates the performance of the direct fired
	//    absorption chiller.
	// METHODOLOGY EMPLOYED:
	//    Once the PlantLoopManager determines that the absorber chiller
	//    is available to meet a loop cooling demand, it calls SimGasAbsorption
	//    which in turn calls the appropriate Absorption Chiller model.
	// REFERENCES:
	//    DOE-2.1e Supplement
	//    PG&E CoolTools GasMod
	// OTHER NOTES:
	//    The curves on this model follow the DOE-2 approach of using
	//    electric and heat input ratios.  In addition, the temperature
	//    correction curve has two independent variables for the
	//    chilled water temperature and either the entering or leaving
	//    condenser water temperature.
	//    The code was originally adopted from the ChillerAbsorption
	//    routine but has been extensively modified.
	//    Development of this module was funded by the Gas Research Institute.
	//    (Please see copyright and disclaimer information at end of module)

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BigNumber;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SecInHour;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallWaterVolFlow;

	using General::TrimSigDigits;
	using General::RoundSigDigits;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	int NumGasAbsorbers( 0 ); // number of Absorption Chillers specified in input

	// This type holds the output from the algorithm i.e., the Report Variables

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Object Data
	Array1D< GasAbsorberSpecs > GasAbsorber; // dimension to number of machines
	Array1D< ReportVars > GasAbsorberReport;

	// MODULE SUBROUTINES:

	// Beginning of Absorption Chiller Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimGasAbsorber(
		std::string const & EP_UNUSED( AbsorberType ), // type of Absorber
		std::string const & AbsorberName, // user specified name of Absorber
		int const EP_UNUSED( EquipFlowCtrl ), // Flow control mode for the equipment
		int & CompIndex, // Absorber number counter
		bool const RunFlag, // simulate Absorber when TRUE
		bool const FirstIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not false, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		int const BranchInletNodeNum, // node number of inlet to calling branch,
		Real64 & MaxCap, // W - maximum operating capacity of Absorber
		Real64 & MinCap, // W - minimum operating capacity of Absorber
		Real64 & OptCap, // W - optimal operating capacity of Absorber
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign,
		Real64 & TempEvapOutDesign
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using CurveManager::CurveValue;
		using DataPlant::TypeOf_Chiller_DFAbsorption;
		using PlantUtilities::UpdateChillerComponentCondenserSide;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// used to determine if heating side or cooling
		// side of chiller-heater is being called
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 HeatCap( 0.0 ); // W - nominal heating capacity
		static bool GetInput( true ); // then TRUE, calls subroutine to read input file.
		int ChillNum; // Absorber number counter

		//Get Absorber data from input file
		if ( GetInput ) {
			GetGasAbsorberInput();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			ChillNum = FindItemInList( AbsorberName, GasAbsorber );
			if ( ChillNum == 0 ) {
				ShowFatalError( "SimGasAbsorber: Unit not found=" + AbsorberName );
			}
			CompIndex = ChillNum;
		} else {
			ChillNum = CompIndex;
			if ( ChillNum > NumGasAbsorbers || ChillNum < 1 ) {
				ShowFatalError( "SimGasAbsorber:  Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Number of Units=" + TrimSigDigits( NumGasAbsorbers ) + ", Entered Unit name=" + AbsorberName );
			}
			if ( CheckEquipName( ChillNum ) ) {
				if ( AbsorberName != GasAbsorber( ChillNum ).Name ) {
					ShowFatalError( "SimGasAbsorber: Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Unit name=" + AbsorberName + ", stored Unit Name for that index=" + GasAbsorber( ChillNum ).Name );
				}
				CheckEquipName( ChillNum ) = false;
			}
		}

		// Check that this is a valid call
		if ( InitLoopEquip ) {
			TempEvapOutDesign = GasAbsorber( ChillNum ).TempDesCHWSupply;
			TempCondInDesign = GasAbsorber( ChillNum ).TempDesCondReturn;
			InitGasAbsorber( ChillNum, RunFlag );

			// Match inlet node name of calling branch to determine if this call is for heating or cooling
			if ( BranchInletNodeNum == GasAbsorber( ChillNum ).ChillReturnNodeNum ) { // Operate as chiller
				SizeGasAbsorber( ChillNum ); // only call from chilled water loop
				MinCap = GasAbsorber( ChillNum ).NomCoolingCap * GasAbsorber( ChillNum ).MinPartLoadRat;
				MaxCap = GasAbsorber( ChillNum ).NomCoolingCap * GasAbsorber( ChillNum ).MaxPartLoadRat;
				OptCap = GasAbsorber( ChillNum ).NomCoolingCap * GasAbsorber( ChillNum ).OptPartLoadRat;
			} else if ( BranchInletNodeNum == GasAbsorber( ChillNum ).HeatReturnNodeNum ) { // Operate as heater
				HeatCap = GasAbsorber( ChillNum ).NomCoolingCap * GasAbsorber( ChillNum ).NomHeatCoolRatio;
				MinCap = HeatCap * GasAbsorber( ChillNum ).MinPartLoadRat;
				MaxCap = HeatCap * GasAbsorber( ChillNum ).MaxPartLoadRat;
				OptCap = HeatCap * GasAbsorber( ChillNum ).OptPartLoadRat;
			} else if ( BranchInletNodeNum == GasAbsorber( ChillNum ).CondReturnNodeNum ) { // called from condenser loop
				HeatCap = 0.0;
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			} else { // Error, nodes do not match
				ShowSevereError( "SimGasAbsorber: Invalid call to Gas Absorbtion Chiller-Heater " + AbsorberName );
				ShowContinueError( "Node connections in branch are not consistent with object nodes." );
				ShowFatalError( "Preceding conditions cause termination." );
			} // Operate as Chiller or Heater
			if ( GetSizingFactor ) {
				SizingFactor = GasAbsorber( ChillNum ).SizFac;
			}
			return;
		}

		// Match inlet node name of calling branch to determine if this call is for heating or cooling
		if ( BranchInletNodeNum == GasAbsorber( ChillNum ).ChillReturnNodeNum ) { // Operate as chiller
			// Calculate Node Values
			// Calculate Equipment and Update Variables
			if ( RunFlag ) {
				GasAbsorber( ChillNum ).InCoolingMode = true;
			} else {
				GasAbsorber( ChillNum ).InCoolingMode = false;
			}
			InitGasAbsorber( ChillNum, RunFlag );
			CalcGasAbsorberChillerModel( ChillNum, MyLoad, RunFlag );
			UpdateGasAbsorberCoolRecords( MyLoad, RunFlag, ChillNum );
		} else if ( BranchInletNodeNum == GasAbsorber( ChillNum ).HeatReturnNodeNum ) { // Operate as heater
			// Calculate Node Values
			// Calculate Equipment and Update Variables
			if ( RunFlag ) {
				GasAbsorber( ChillNum ).InHeatingMode = true;
			} else {
				GasAbsorber( ChillNum ).InHeatingMode = false;
			}
			InitGasAbsorber( ChillNum, RunFlag );
			CalcGasAbsorberHeaterModel( ChillNum, MyLoad, RunFlag );
			UpdateGasAbsorberHeatRecords( MyLoad, RunFlag, ChillNum );
		} else if ( BranchInletNodeNum == GasAbsorber( ChillNum ).CondReturnNodeNum ) { // called from condenser loop
			UpdateChillerComponentCondenserSide( GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_DFAbsorption, GasAbsorber( ChillNum ).CondReturnNodeNum, GasAbsorber( ChillNum ).CondSupplyNodeNum, GasAbsorberReport( ChillNum ).TowerLoad, GasAbsorberReport( ChillNum ).CondReturnTemp, GasAbsorberReport( ChillNum ).CondSupplyTemp, GasAbsorberReport( ChillNum ).CondWaterFlowRate, FirstIteration );

		} else { // Error, nodes do not match
			ShowSevereError( "Invalid call to Gas Absorber Chiller " + AbsorberName );
			ShowContinueError( "Node connections in branch are not consistent with object nodes." );
			ShowFatalError( "Preceding conditions cause termination." );
		}

	}

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetGasAbsorberInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Jason Glazer
		//       DATE WRITTEN:    March 2001

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the Direct Fired Absorption chiller modelin the object ChillerHeater:Absorption:DirectFired

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveCheck;
		using GlobalNames::VerifyUniqueChillerName;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using DataSizing::AutoSize;

		// Locals
		// PARAMETERS

		//LOCAL VARIABLES
		int AbsorberNum; // Absorber counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string ChillerName;
		bool errFlag;
		bool Okay;

		//FLOW
		cCurrentModuleObject = "ChillerHeater:Absorption:DirectFired";
		NumGasAbsorbers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumGasAbsorbers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment found in input file" );
			ErrorsFound = true;
		}

		if ( allocated( GasAbsorber ) ) return;

		//ALLOCATE ARRAYS
		GasAbsorber.allocate( NumGasAbsorbers );

		GasAbsorberReport.allocate( NumGasAbsorbers );
		CheckEquipName.dimension( NumGasAbsorbers, true );

		//LOAD ARRAYS

		for ( AbsorberNum = 1; AbsorberNum <= NumGasAbsorbers; ++AbsorberNum ) {
			GetObjectItem( cCurrentModuleObject, AbsorberNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), GasAbsorber, AbsorberNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			GasAbsorber( AbsorberNum ).Name = cAlphaArgs( 1 );
			ChillerName = cCurrentModuleObject + " Named " + GasAbsorber( AbsorberNum ).Name;

			// Assign capacities
			GasAbsorber( AbsorberNum ).NomCoolingCap = rNumericArgs( 1 );
			if ( GasAbsorber( AbsorberNum ).NomCoolingCap == AutoSize ) {
				GasAbsorber( AbsorberNum ).NomCoolingCapWasAutoSized = true;
			}
			GasAbsorber( AbsorberNum ).NomHeatCoolRatio = rNumericArgs( 2 );
			// Assign efficiencies
			GasAbsorber( AbsorberNum ).FuelCoolRatio = rNumericArgs( 3 );
			GasAbsorber( AbsorberNum ).FuelHeatRatio = rNumericArgs( 4 );
			GasAbsorber( AbsorberNum ).ElecCoolRatio = rNumericArgs( 5 );
			GasAbsorber( AbsorberNum ).ElecHeatRatio = rNumericArgs( 6 );

			// Assign Node Numbers to specified nodes
			GasAbsorber( AbsorberNum ).ChillReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			GasAbsorber( AbsorberNum ).ChillSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Chilled Water Nodes" );
			// Condenser node processing depends on condenser type, see below
			GasAbsorber( AbsorberNum ).HeatReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
			GasAbsorber( AbsorberNum ).HeatSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Hot Water Nodes" );
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing node input for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = false;
			}

			// Assign Part Load Ratios
			GasAbsorber( AbsorberNum ).MinPartLoadRat = rNumericArgs( 7 );
			GasAbsorber( AbsorberNum ).MaxPartLoadRat = rNumericArgs( 8 );
			GasAbsorber( AbsorberNum ).OptPartLoadRat = rNumericArgs( 9 );
			// Assign Design Conditions
			GasAbsorber( AbsorberNum ).TempDesCondReturn = rNumericArgs( 10 );
			GasAbsorber( AbsorberNum ).TempDesCHWSupply = rNumericArgs( 11 );
			GasAbsorber( AbsorberNum ).EvapVolFlowRate = rNumericArgs( 12 );
			if ( GasAbsorber( AbsorberNum ).EvapVolFlowRate == AutoSize ) {
				GasAbsorber( AbsorberNum ).EvapVolFlowRateWasAutoSized = true;
			}
			if ( SameString( cAlphaArgs( 16 ), "AirCooled" ) ) {
				GasAbsorber( AbsorberNum ).CondVolFlowRate = 0.0011; // Condenser flow rate not used for this cond type
			} else {
				GasAbsorber( AbsorberNum ).CondVolFlowRate = rNumericArgs( 13 );
				if ( GasAbsorber( AbsorberNum ).CondVolFlowRate == AutoSize ) {
					GasAbsorber( AbsorberNum ).CondVolFlowRateWasAutoSized = true;
				}
			}
			GasAbsorber( AbsorberNum ).HeatVolFlowRate = rNumericArgs( 14 );
			if ( GasAbsorber( AbsorberNum ).HeatVolFlowRate == AutoSize ) {
				GasAbsorber( AbsorberNum ).HeatVolFlowRateWasAutoSized = true;
			}
			// Assign Curve Numbers
			GasAbsorber( AbsorberNum ).CoolCapFTCurve = GetCurveCheck( cAlphaArgs( 8 ), ErrorsFound, ChillerName );
			GasAbsorber( AbsorberNum ).FuelCoolFTCurve = GetCurveCheck( cAlphaArgs( 9 ), ErrorsFound, ChillerName );
			GasAbsorber( AbsorberNum ).FuelCoolFPLRCurve = GetCurveCheck( cAlphaArgs( 10 ), ErrorsFound, ChillerName );
			GasAbsorber( AbsorberNum ).ElecCoolFTCurve = GetCurveCheck( cAlphaArgs( 11 ), ErrorsFound, ChillerName );
			GasAbsorber( AbsorberNum ).ElecCoolFPLRCurve = GetCurveCheck( cAlphaArgs( 12 ), ErrorsFound, ChillerName );
			GasAbsorber( AbsorberNum ).HeatCapFCoolCurve = GetCurveCheck( cAlphaArgs( 13 ), ErrorsFound, ChillerName );
			GasAbsorber( AbsorberNum ).FuelHeatFHPLRCurve = GetCurveCheck( cAlphaArgs( 14 ), ErrorsFound, ChillerName );
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing curve input for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = false;
			}
			if ( SameString( cAlphaArgs( 15 ), "LeavingCondenser" ) ) {
				GasAbsorber( AbsorberNum ).isEnterCondensTemp = false;
			} else if ( SameString( cAlphaArgs( 15 ), "EnteringCondenser" ) ) {
				GasAbsorber( AbsorberNum ).isEnterCondensTemp = true;
			} else {
				GasAbsorber( AbsorberNum ).isEnterCondensTemp = true;
				ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid value" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 15 ) + "=\"" + cAlphaArgs( 15 ) + "\"" );
				ShowContinueError( "resetting to EnteringCondenser, simulation continues" );
			}
			// Assign Other Paramters
			if ( SameString( cAlphaArgs( 16 ), "AirCooled" ) ) {
				GasAbsorber( AbsorberNum ).isWaterCooled = false;
			} else if ( SameString( cAlphaArgs( 16 ), "WaterCooled" ) ) {
				GasAbsorber( AbsorberNum ).isWaterCooled = true;
			} else {
				GasAbsorber( AbsorberNum ).isWaterCooled = true;
				ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid value" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 16 ) + '=' + cAlphaArgs( 16 ) );
				ShowContinueError( "resetting to WaterCooled, simulation continues" );
			}
			if ( GasAbsorber( AbsorberNum ).isWaterCooled ) {
				GasAbsorber( AbsorberNum ).CondReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				GasAbsorber( AbsorberNum ).CondSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser Water Nodes" );
			} else {
				GasAbsorber( AbsorberNum ).CondReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				GasAbsorber( AbsorberNum ).CondSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				// Connection not required for air or evap cooled condenser so no call to TestCompSet here
				CheckAndAddAirNodeNumber( GasAbsorber( AbsorberNum ).CondReturnNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 4 ) );
				}
			}
			GasAbsorber( AbsorberNum ).CHWLowLimitTemp = rNumericArgs( 15 );
			GasAbsorber( AbsorberNum ).FuelHeatingValue = rNumericArgs( 16 );
			GasAbsorber( AbsorberNum ).SizFac = rNumericArgs( 17 );

			//Fuel Type Case Statement
			{ auto const SELECT_CASE_var( cAlphaArgs( 18 ) );
			if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				GasAbsorber( AbsorberNum ).FuelType = "Gas";

			} else if ( SELECT_CASE_var == "DIESEL" ) {
				GasAbsorber( AbsorberNum ).FuelType = "Diesel";

			} else if ( SELECT_CASE_var == "GASOLINE" ) {
				GasAbsorber( AbsorberNum ).FuelType = "Gasoline";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
				GasAbsorber( AbsorberNum ).FuelType = "FuelOil#1";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
				GasAbsorber( AbsorberNum ).FuelType = "FuelOil#2";

			} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				GasAbsorber( AbsorberNum ).FuelType = "Propane";

			} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
				GasAbsorber( AbsorberNum ).FuelType = "OtherFuel1";

			} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
				GasAbsorber( AbsorberNum ).FuelType = "OtherFuel2";

			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid value" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 18 ) + '=' + cAlphaArgs( 18 ) );
				ShowContinueError( "Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2,OtherFuel1 or OtherFuel2" );
				ErrorsFound = true;
			}}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( AbsorberNum = 1; AbsorberNum <= NumGasAbsorbers; ++AbsorberNum ) {
			ChillerName = GasAbsorber( AbsorberNum ).Name;

			SetupOutputVariable( "Chiller Heater Evaporator Cooling Rate [W]", GasAbsorberReport( AbsorberNum ).CoolingLoad, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Evaporator Cooling Energy [J]", GasAbsorberReport( AbsorberNum ).CoolingEnergy, "System", "Sum", ChillerName, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Heating Rate [W]", GasAbsorberReport( AbsorberNum ).HeatingLoad, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Energy [J]", GasAbsorberReport( AbsorberNum ).HeatingEnergy, "System", "Sum", ChillerName, _, "ENERGYTRANSFER", "BOILERS", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Condenser Heat Transfer Rate [W]", GasAbsorberReport( AbsorberNum ).TowerLoad, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Condenser Heat Transfer Energy [J]", GasAbsorberReport( AbsorberNum ).TowerEnergy, "System", "Sum", ChillerName, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			SetupOutputVariable( "Chiller Heater " + GasAbsorber( AbsorberNum ).FuelType + " Rate [W]", GasAbsorberReport( AbsorberNum ).FuelUseRate, "System", "Average", ChillerName );
			// Do not include this on meters, this would duplicate the cool fuel and heat fuel
			SetupOutputVariable( "Chiller Heater " + GasAbsorber( AbsorberNum ).FuelType + " Energy [J]", GasAbsorberReport( AbsorberNum ).FuelEnergy, "System", "Sum", ChillerName );

			SetupOutputVariable( "Chiller Heater Cooling " + GasAbsorber( AbsorberNum ).FuelType + " Rate [W]", GasAbsorberReport( AbsorberNum ).CoolFuelUseRate, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Cooling " + GasAbsorber( AbsorberNum ).FuelType + " Energy [J]", GasAbsorberReport( AbsorberNum ).CoolFuelEnergy, "System", "Sum", ChillerName, _, GasAbsorber( AbsorberNum ).FuelType, "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Cooling COP [W/W]", GasAbsorberReport( AbsorberNum ).FuelCOP, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Heating " + GasAbsorber( AbsorberNum ).FuelType + " Rate [W]", GasAbsorberReport( AbsorberNum ).HeatFuelUseRate, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating " + GasAbsorber( AbsorberNum ).FuelType + " Energy [J]", GasAbsorberReport( AbsorberNum ).HeatFuelEnergy, "System", "Sum", ChillerName, _, GasAbsorber( AbsorberNum ).FuelType, "Heating", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Electric Power [W]", GasAbsorberReport( AbsorberNum ).ElectricPower, "System", "Average", ChillerName );
			// Do not include this on meters, this would duplicate the cool electric and heat electric
			SetupOutputVariable( "Chiller Heater Electric Energy [J]", GasAbsorberReport( AbsorberNum ).ElectricEnergy, "System", "Sum", ChillerName );

			SetupOutputVariable( "Chiller Heater Cooling Electric Power [W]", GasAbsorberReport( AbsorberNum ).CoolElectricPower, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Cooling Electric Energy [J]", GasAbsorberReport( AbsorberNum ).CoolElectricEnergy, "System", "Sum", ChillerName, _, "Electricity", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Heating Electric Power [W]", GasAbsorberReport( AbsorberNum ).HeatElectricPower, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Electric Energy [J]", GasAbsorberReport( AbsorberNum ).HeatElectricEnergy, "System", "Sum", ChillerName, _, "Electricity", "Heating", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Evaporator Inlet Temperature [C]", GasAbsorberReport( AbsorberNum ).ChillReturnTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Evaporator Outlet Temperature [C]", GasAbsorberReport( AbsorberNum ).ChillSupplyTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Evaporator Mass Flow Rate [kg/s]", GasAbsorberReport( AbsorberNum ).ChillWaterFlowRate, "System", "Average", ChillerName );

			if ( GasAbsorber( AbsorberNum ).isWaterCooled ) {
				SetupOutputVariable( "Chiller Heater Condenser Inlet Temperature [C]", GasAbsorberReport( AbsorberNum ).CondReturnTemp, "System", "Average", ChillerName );
				SetupOutputVariable( "Chiller Heater Condenser Outlet Temperature [C]", GasAbsorberReport( AbsorberNum ).CondSupplyTemp, "System", "Average", ChillerName );
				SetupOutputVariable( "Chiller Heater Condenser Mass Flow Rate [kg/s]", GasAbsorberReport( AbsorberNum ).CondWaterFlowRate, "System", "Average", ChillerName );
			} else {
				SetupOutputVariable( "Chiller Heater Condenser Inlet Temperature [C]", GasAbsorberReport( AbsorberNum ).CondReturnTemp, "System", "Average", ChillerName );
			}

			SetupOutputVariable( "Chiller Heater Heating Inlet Temperature [C]", GasAbsorberReport( AbsorberNum ).HotWaterReturnTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Outlet Temperature [C]", GasAbsorberReport( AbsorberNum ).HotWaterSupplyTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Mass Flow Rate [kg/s]", GasAbsorberReport( AbsorberNum ).HotWaterFlowRate, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Cooling Part Load Ratio []", GasAbsorberReport( AbsorberNum ).CoolPartLoadRatio, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Maximum Cooling Rate [W]", GasAbsorberReport( AbsorberNum ).CoolingCapacity, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Part Load Ratio []", GasAbsorberReport( AbsorberNum ).HeatPartLoadRatio, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Maximum Heating Rate [W]", GasAbsorberReport( AbsorberNum ).HeatingCapacity, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Runtime Fraction []", GasAbsorberReport( AbsorberNum ).FractionOfPeriodRunning, "System", "Average", ChillerName );

		}
	}

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitGasAbsorber(
		int const ChillNum, // number of the current engine driven chiller being simulated
		bool const EP_UNUSED( RunFlag ) // TRUE when chiller operating
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of direct fired absorption chiller
		// components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::TypeOf_Chiller_DFAbsorption;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using Psychrometrics::RhoH2O;

		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// used to determine if heating side or cooling
		// side of chiller-heater is being called

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitGasAbsorber" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		int CondInletNode; // node number of water inlet node to the condenser
		int CondOutletNode; // node number of water outlet node from the condenser
		int HeatInletNode; // node number of hot water inlet node
		int HeatOutletNode; // node number of hot water outlet node
		bool errFlag;
		Real64 rho; // local fluid density
		Real64 mdot; // lcoal fluid mass flow rate

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyPlantScanFlag.allocate( NumGasAbsorbers );
			MyEnvrnFlag.dimension( NumGasAbsorbers, true );
			MyOneTimeFlag = false;
			MyPlantScanFlag = true;
		}

		// Init more variables
		if ( MyPlantScanFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( GasAbsorber( ChillNum ).Name, TypeOf_Chiller_DFAbsorption, GasAbsorber( ChillNum ).CWLoopNum, GasAbsorber( ChillNum ).CWLoopSideNum, GasAbsorber( ChillNum ).CWBranchNum, GasAbsorber( ChillNum ).CWCompNum, GasAbsorber( ChillNum ).CHWLowLimitTemp, _, _, GasAbsorber( ChillNum ).ChillReturnNodeNum, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitGasAbsorber: Program terminated due to previous condition(s)." );
			}

			ScanPlantLoopsForObject( GasAbsorber( ChillNum ).Name, TypeOf_Chiller_DFAbsorption, GasAbsorber( ChillNum ).HWLoopNum, GasAbsorber( ChillNum ).HWLoopSideNum, GasAbsorber( ChillNum ).HWBranchNum, GasAbsorber( ChillNum ).HWCompNum, _, _, _, GasAbsorber( ChillNum ).HeatReturnNodeNum, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitGasAbsorber: Program terminated due to previous condition(s)." );
			}

			if ( GasAbsorber( ChillNum ).isWaterCooled ) {
				ScanPlantLoopsForObject( GasAbsorber( ChillNum ).Name, TypeOf_Chiller_DFAbsorption, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum, _, _, _, GasAbsorber( ChillNum ).CondReturnNodeNum, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitGasAbsorber: Program terminated due to previous condition(s)." );
				}
				InterConnectTwoPlantLoopSides( GasAbsorber( ChillNum ).CWLoopNum, GasAbsorber( ChillNum ).CWLoopSideNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_DFAbsorption, true );
				InterConnectTwoPlantLoopSides( GasAbsorber( ChillNum ).HWLoopNum, GasAbsorber( ChillNum ).HWLoopSideNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_DFAbsorption, true );
			}

			InterConnectTwoPlantLoopSides( GasAbsorber( ChillNum ).CWLoopNum, GasAbsorber( ChillNum ).CWLoopSideNum, GasAbsorber( ChillNum ).HWLoopNum, GasAbsorber( ChillNum ).HWLoopSideNum, TypeOf_Chiller_DFAbsorption, true );

			// check if outlet node of chilled water side has a setpoint.
			if ( ( Node( GasAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( GasAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					if ( ! GasAbsorber( ChillNum ).ChillSetPointErrDone ) {
						ShowWarningError( "Missing temperature setpoint on cool side for chiller heater named " + GasAbsorber( ChillNum ).Name );
						ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager" );
						ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
						GasAbsorber( ChillNum ).ChillSetPointErrDone = true;
					}
				} else {
					// need call to EMS to check node
					errFlag = false; // but not really fatal yet, but should be.
					CheckIfNodeSetPointManagedByEMS( GasAbsorber( ChillNum ).ChillSupplyNodeNum, iTemperatureSetPoint, errFlag );
					if ( errFlag ) {
						if ( ! GasAbsorber( ChillNum ).ChillSetPointErrDone ) {
							ShowWarningError( "Missing temperature setpoint on cool side for chiller heater named " + GasAbsorber( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller evaporator " );
							ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
							ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							GasAbsorber( ChillNum ).ChillSetPointErrDone = true;

						}
					}

				}
				GasAbsorber( ChillNum ).ChillSetPointSetToLoop = true;
				Node( GasAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPoint = Node( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
				Node( GasAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPointHi = Node( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
			// check if outlet node of hot water side has a setpoint.
			if ( ( Node( GasAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( GasAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPointLo == SensedNodeFlagValue ) ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					if ( ! GasAbsorber( ChillNum ).HeatSetPointErrDone ) {
						ShowWarningError( "Missing temperature setpoint on heat side for chiller heater named " + GasAbsorber( ChillNum ).Name );
						ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager" );
						ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
						GasAbsorber( ChillNum ).HeatSetPointErrDone = true;
					}
				} else {
					// need call to EMS to check node
					errFlag = false; // but not really fatal yet, but should be.
					CheckIfNodeSetPointManagedByEMS( GasAbsorber( ChillNum ).HeatSupplyNodeNum, iTemperatureSetPoint, errFlag );
					if ( errFlag ) {
						if ( ! GasAbsorber( ChillNum ).HeatSetPointErrDone ) {
							ShowWarningError( "Missing temperature setpoint on heat side for chiller heater named " + GasAbsorber( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller heater " );
							ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the heater side outlet node " );
							ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
							ShowContinueError( "  The overall loop setpoint will be assumed for heater side. The simulation continues ... " );
							GasAbsorber( ChillNum ).HeatSetPointErrDone = true;
						}
					}
				}
				GasAbsorber( ChillNum ).HeatSetPointSetToLoop = true;
				Node( GasAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPoint = Node( PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
				Node( GasAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPointLo = Node( PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPointLo;
			}
			MyPlantScanFlag( ChillNum ) = false;
		}

		CondInletNode = GasAbsorber( ChillNum ).CondReturnNodeNum;
		CondOutletNode = GasAbsorber( ChillNum ).CondSupplyNodeNum;
		HeatInletNode = GasAbsorber( ChillNum ).HeatReturnNodeNum;
		HeatOutletNode = GasAbsorber( ChillNum ).HeatSupplyNodeNum;

		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			if ( GasAbsorber( ChillNum ).isWaterCooled ) {
				// init max available condenser water flow rate
				if ( GasAbsorber( ChillNum ).CDLoopNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				} else {
					rho = RhoH2O( InitConvTemp );

				}

				GasAbsorber( ChillNum ).DesCondMassFlowRate = rho * GasAbsorber( ChillNum ).CondVolFlowRate;
				InitComponentNodes( 0.0, GasAbsorber( ChillNum ).DesCondMassFlowRate, CondInletNode, CondOutletNode, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum );
			}

			if ( GasAbsorber( ChillNum ).HWLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).FluidName, InitConvTemp, PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = RhoH2O( InitConvTemp );
			}
			GasAbsorber( ChillNum ).DesHeatMassFlowRate = rho * GasAbsorber( ChillNum ).HeatVolFlowRate;
			//init available hot water flow rate
			InitComponentNodes( 0.0, GasAbsorber( ChillNum ).DesHeatMassFlowRate, HeatInletNode, HeatOutletNode, GasAbsorber( ChillNum ).HWLoopNum, GasAbsorber( ChillNum ).HWLoopSideNum, GasAbsorber( ChillNum ).HWBranchNum, GasAbsorber( ChillNum ).HWCompNum );

			if ( GasAbsorber( ChillNum ).CWLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = RhoH2O( InitConvTemp );
			}
			GasAbsorber( ChillNum ).DesEvapMassFlowRate = rho * GasAbsorber( ChillNum ).EvapVolFlowRate;
			//init available hot water flow rate
			InitComponentNodes( 0.0, GasAbsorber( ChillNum ).DesEvapMassFlowRate, GasAbsorber( ChillNum ).ChillReturnNodeNum, GasAbsorber( ChillNum ).ChillSupplyNodeNum, GasAbsorber( ChillNum ).CWLoopNum, GasAbsorber( ChillNum ).CWLoopSideNum, GasAbsorber( ChillNum ).CWBranchNum, GasAbsorber( ChillNum ).CWCompNum );

			MyEnvrnFlag( ChillNum ) = false;

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		//this component model works off setpoints on the leaving node
		// fill from plant if needed
		if ( GasAbsorber( ChillNum ).ChillSetPointSetToLoop ) {
			Node( GasAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPoint = Node( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( GasAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPointHi = Node( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( GasAbsorber( ChillNum ).HeatSetPointSetToLoop ) {
			Node( GasAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPoint = Node( PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( GasAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPointLo = Node( PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPointLo;
		}

		if ( ( GasAbsorber( ChillNum ).isWaterCooled ) && ( ( GasAbsorber( ChillNum ).InHeatingMode ) || ( GasAbsorber( ChillNum ).InCoolingMode ) ) && ( ! MyPlantScanFlag( ChillNum ) ) ) {
			mdot = GasAbsorber( ChillNum ).DesCondMassFlowRate;
			//DSU removed, this has to have been wrong (?)  Node(CondInletNode)%Temp  = GasAbsorber(ChillNum)%TempDesCondReturn

			SetComponentFlowRate( mdot, GasAbsorber( ChillNum ).CondReturnNodeNum, GasAbsorber( ChillNum ).CondSupplyNodeNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum );

		} else {
			mdot = 0.0;
			SetComponentFlowRate( mdot, GasAbsorber( ChillNum ).CondReturnNodeNum, GasAbsorber( ChillNum ).CondSupplyNodeNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum );
		}

	}

	void
	SizeGasAbsorber( int const ChillNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2003
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing direct fired gas absorption chiller components for which
		// capacities and flow rates have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
		// the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		// is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		//  USE BranchInputManager,  ONLY: MyPlantSizingIndex
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeGasAbsorber" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int PltSizCoolNum; // Plant Sizing index for cooling loop
		int PltSizHeatNum; // Plant Sizing index for heating loop
		int PltSizCondNum; // Plant Sizing index for condenser loop

		bool ErrorsFound; // If errors detected in input
		//  LOGICAL             :: LoopErrorsFound
		std::string equipName;
		Real64 Cp; // local fluid specific heat
		Real64 rho; // local fluid density
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 tmpHeatRecVolFlowRate; // local heat recovery design volume flow rate
		Real64 NomCapUser; // Hardsized nominal capacity for reporting
		Real64 EvapVolFlowRateUser; // Hardsized evaporator volume flow rate for reporting
		Real64 CondVolFlowRateUser; // Hardsized condenser flow rate for reporting
		Real64 HeatRecVolFlowRateUser; // Hardsized generator flow rate for reporting

		PltSizCoolNum = 0;
		PltSizCondNum = 0;
		PltSizHeatNum = 0;
		ErrorsFound = false;
		tmpNomCap = GasAbsorber( ChillNum ).NomCoolingCap;
		tmpEvapVolFlowRate = GasAbsorber( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = GasAbsorber( ChillNum ).CondVolFlowRate;
		tmpHeatRecVolFlowRate = GasAbsorber( ChillNum ).HeatVolFlowRate;
		NomCapUser = 0.0;
		EvapVolFlowRateUser = 0.0;
		CondVolFlowRateUser = 0.0;
		HeatRecVolFlowRateUser = 0.0;

		if ( GasAbsorber( ChillNum ).isWaterCooled ) PltSizCondNum = PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).PlantSizNum;
		PltSizHeatNum = PlantLoop( GasAbsorber( ChillNum ).HWLoopNum ).PlantSizNum;
		PltSizCoolNum = PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizCoolNum > 0 ) {
			if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				Cp = GetSpecificHeatGlycol( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				rho = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizCoolNum ).DeltaT * PlantSizData( PltSizCoolNum ).DesVolFlowRate * GasAbsorber( ChillNum ).SizFac;
				if ( ! GasAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) tmpNomCap = GasAbsorber( ChillNum ).NomCoolingCap;
			} else {
				if ( GasAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GasAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) {
					GasAbsorber( ChillNum ).NomCoolingCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Design Size Nominal Cooling Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Initial Design Size Nominal Cooling Capacity [W]", tmpNomCap );
					}
				} else {
					if ( GasAbsorber( ChillNum ).NomCoolingCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = GasAbsorber( ChillNum ).NomCoolingCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
								"Design Size Nominal Cooling Capacity [W]", tmpNomCap,
								"User-Specified Nominal Cooling Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerHeaterAbsorptionDirectFired: Potential issue with equipment sizing for " + GasAbsorber( ChillNum ).Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( GasAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + GasAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Direct Fired Absorption Chiller nominal cooling capacity requires" );
					ShowContinueError( "a cooling loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( GasAbsorber( ChillNum ).NomCoolingCap > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"User-Specified Nominal Capacity [W]", GasAbsorber( ChillNum ).NomCoolingCap );
					}
				}
			}
		}

		if ( PltSizCoolNum > 0 ) {
			if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizCoolNum ).DesVolFlowRate * GasAbsorber( ChillNum ).SizFac;
				if ( ! GasAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = GasAbsorber( ChillNum ).EvapVolFlowRate;
			} else {
				if ( GasAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GasAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					GasAbsorber( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( GasAbsorber( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = GasAbsorber( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorptionDirectFired: Potential issue with equipment sizing for " + GasAbsorber( ChillNum ).Name );
									ShowContinueError( "User-Specified Design Chilled Water Flow Rate of " + RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Chilled Water Flow Rate of " + RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpEvapVolFlowRate = EvapVolFlowRateUser;
					}
				}
			}
		} else {
			if ( GasAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + GasAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Direct Fired Absorption Chiller evap flow rate requires" );
					ShowContinueError( "a cooling loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( GasAbsorber( ChillNum ).EvapVolFlowRate > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"User-Specified Design Chilled Water Flow Rate [m3/s]", GasAbsorber( ChillNum ).EvapVolFlowRate );
					}
				}
			}
		}

		RegisterPlantCompDesignFlow( GasAbsorber( ChillNum ).ChillReturnNodeNum, tmpEvapVolFlowRate );

		if ( PltSizHeatNum > 0 ) {
			if ( PlantSizData( PltSizHeatNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpHeatRecVolFlowRate = PlantSizData( PltSizHeatNum ).DesVolFlowRate * GasAbsorber( ChillNum ).SizFac;
				if ( ! GasAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = GasAbsorber( ChillNum ).HeatVolFlowRate;

			} else {
				if ( GasAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GasAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) {
					GasAbsorber( ChillNum ).HeatVolFlowRate = tmpHeatRecVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Design Size Design Hot Water Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Initial Design Size Design Hot Water Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
				} else {
					if ( GasAbsorber( ChillNum ).HeatVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0 ) {
						HeatRecVolFlowRateUser = GasAbsorber( ChillNum ).HeatVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
								"Design Size Design Hot Water Flow Rate [m3/s]", tmpHeatRecVolFlowRate,
								"User-Specified Design Hot Water Flow Rate [m3/s]", HeatRecVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpHeatRecVolFlowRate - HeatRecVolFlowRateUser ) / HeatRecVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerHeaterAbsorptionDirectFired: Potential issue with equipment sizing for " + GasAbsorber( ChillNum ).Name );
									ShowContinueError( "User-Specified Design Hot Water Flow Rate of " + RoundSigDigits( HeatRecVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Hot Water Flow Rate of " + RoundSigDigits( tmpHeatRecVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpHeatRecVolFlowRate = HeatRecVolFlowRateUser;
					}
				}
			}
		} else {
			if ( GasAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + GasAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Direct Fired Absorption Chiller hot water flow rate requires" );
					ShowContinueError( "a heating loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( GasAbsorber( ChillNum ).HeatVolFlowRate > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"User-Specified Design Hot Water Flow Rate [m3/s]", GasAbsorber( ChillNum ).HeatVolFlowRate );
					}
				}
			}
		}

		RegisterPlantCompDesignFlow( GasAbsorber( ChillNum ).HeatReturnNodeNum, tmpHeatRecVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizCoolNum > 0 ) {
			if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {

				Cp = GetSpecificHeatGlycol( PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidName, GasAbsorber( ChillNum ).TempDesCondReturn, PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				rho = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidName, GasAbsorber( ChillNum ).TempDesCondReturn, PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + GasAbsorber( ChillNum ).FuelCoolRatio ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! GasAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = GasAbsorber( ChillNum ).CondVolFlowRate;
				//IF (PlantFirstSizesOkayToFinalize) GasAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
			} else {
				if ( GasAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
				//IF (PlantFirstSizesOkayToFinalize) GasAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GasAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
					GasAbsorber( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( GasAbsorber( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = GasAbsorber( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorptionDirectFired: Potential issue with equipment sizing for " + GasAbsorber( ChillNum ).Name );
									ShowContinueError( "User-Specified Design Condenser Water Flow Rate of " + RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Condenser Water Flow Rate of " + RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpCondVolFlowRate = CondVolFlowRateUser;
					}
				}
			}
		} else {
			if ( GasAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + GasAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Direct Fired Absorption Chiller condenser flow rate requires a condenser" );
					ShowContinueError( "loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( GasAbsorber( ChillNum ).CondVolFlowRate > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DirectFired", GasAbsorber( ChillNum ).Name,
							"User-Specified Design Condenser Water Flow Rate [m3/s]", GasAbsorber( ChillNum ).CondVolFlowRate );
					}
				}
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( GasAbsorber( ChillNum ).isWaterCooled ) RegisterPlantCompDesignFlow( GasAbsorber( ChillNum ).CondReturnNodeNum, tmpCondVolFlowRate );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = GasAbsorber( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "ChillerHeater:Absorption:DirectFired" );
			PreDefTableEntry( pdchMechNomEff, equipName, GasAbsorber( ChillNum ).FuelCoolRatio );
			PreDefTableEntry( pdchMechNomCap, equipName, GasAbsorber( ChillNum ).NomCoolingCap );
		}

	}

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcGasAbsorberChillerModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const EP_UNUSED( RunFlag ) // TRUE when Absorber operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a direct fired (gas consuming) absorption chiller using
		// curves and inputs similar to DOE-2.1e

		// METHODOLOGY EMPLOYED:
		// Curve fit of performance data

		// REFERENCES:
		// 1.  DOE-2.1e Supplement and source code
		// 2.  CoolTools GasMod work

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataPlant::DeltaTempTol;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FlowLock = 0  if mass flow rates may be changed by loop components
		// FlowLock = 1  if mass flow rates may not be changed by loop components

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcGasAbsorberChillerModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Local copies of GasAbsorberSpecs Type
		// all variables that are local copies of data structure
		// variables are prefaced with an "l" for local.
		Real64 lNomCoolingCap; // W - design nominal capacity of Absorber
		Real64 lFuelCoolRatio; // ratio of fuel input to cooling output
		Real64 lFuelHeatRatio; // ratio of fuel input to heating output
		Real64 lElecCoolRatio; // ratio of electricity input to cooling output
		int lChillReturnNodeNum; // Node number on the inlet side of the plant
		int lChillSupplyNodeNum; // Node number on the outlet side of the plant
		int lCondReturnNodeNum; // Node number on the inlet side of the condenser
		int lCondSupplyNodeNum; // Node number on the outlet side of the condenser
		Real64 lMinPartLoadRat; // min allowed operating frac full load
		Real64 lMaxPartLoadRat; // max allowed operating frac full load
		Real64 lOptPartLoadRat; // optimal operating frac full load
		Real64 lTempDesCondReturn; // design secondary loop fluid temperature at the Absorber condenser side inlet
		Real64 lTempDesCHWSupply; // design chilled water supply temperature
		Real64 lCondVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the condenser
		int lCoolCapFTCurve; // cooling capacity as a function of temperature curve
		int lFuelCoolFTCurve; // Fuel-Input-to cooling output Ratio Function of Temperature Curve
		int lFuelCoolFPLRCurve; // Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
		int lElecCoolFTCurve; // Electric-Input-to cooling output Ratio Function of Temperature Curve
		int lElecCoolFPLRCurve; // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
		bool lIsEnterCondensTemp; // if using entering conderser water temperature is TRUE, exiting is FALSE
		bool lIsWaterCooled; // if water cooled it is TRUE
		Real64 lCHWLowLimitTemp; // Chilled Water Lower Limit Temperature
		Real64 lFuelHeatingValue;
		// Local copies of GasAbsorberReportVars Type
		Real64 lCoolingLoad( 0.0 ); // cooling load on the chiller (previously called QEvap)
		// Real64 lCoolingEnergy( 0.0 ); // variable to track total cooling load for period (was EvapEnergy)
		Real64 lTowerLoad( 0.0 ); // load on the cooling tower/condenser (previously called QCond)
		// Real64 lTowerEnergy( 0.0 ); // variable to track total tower load for a period (was CondEnergy)
		// Real64 lFuelUseRate( 0.0 ); // instantaneous use of gas for period
		// Real64 lFuelEnergy( 0.0 ); // variable to track total fuel used for a period
		Real64 lCoolFuelUseRate( 0.0 ); // instantaneous use of gas for period for cooling
		// Real64 lCoolFuelEnergy( 0.0 ); // variable to track total fuel used for a period for cooling
		Real64 lHeatFuelUseRate( 0.0 ); // instantaneous use of gas for period for heating
		// Real64 lElectricPower( 0.0 ); // parasitic electric power used (was PumpingPower)
		// Real64 lElectricEnergy( 0.0 ); // track the total electricity used for a period (was PumpingEnergy)
		Real64 lCoolElectricPower( 0.0 ); // parasitic electric power used  for cooling
		// Real64 lCoolElectricEnergy( 0.0 ); // track the total electricity used for a period for cooling
		Real64 lHeatElectricPower( 0.0 ); // parasitic electric power used  for heating
		Real64 lChillReturnTemp( 0.0 ); // reporting: evaporator inlet temperature (was EvapInletTemp)
		Real64 lChillSupplyTemp( 0.0 ); // reporting: evaporator outlet temperature (was EvapOutletTemp)
		Real64 lChillWaterMassFlowRate( 0.0 ); // reporting: evaporator mass flow rate (was Evapmdot)
		Real64 lCondReturnTemp( 0.0 ); // reporting: condenser inlet temperature (was CondInletTemp)
		Real64 lCondSupplyTemp( 0.0 ); // reporting: condenser outlet temperature (was CondOutletTemp)
		Real64 lCondWaterMassFlowRate( 0.0 ); // reporting: condenser mass flow rate (was Condmdot)
		Real64 lCoolPartLoadRatio( 0.0 ); // operating part load ratio (load/capacity for cooling)
		Real64 lHeatPartLoadRatio( 0.0 ); // operating part load ratio (load/capacity for heating)
		Real64 lAvailableCoolingCapacity( 0.0 ); // current capacity after temperature adjustment
		Real64 lFractionOfPeriodRunning( 0.0 );
		Real64 PartLoadRat( 0.0 ); // actual operating part load ratio of unit (ranges from minplr to 1)
		Real64 lChillWaterMassflowratemax; // Maximum flow rate through the evaporator

		// other local variables
		Real64 ChillDeltaTemp; // chilled water temperature difference
		Real64 ChillSupplySetPointTemp( 0.0 );

		Real64 calcCondTemp; // the condenser temperature used for curve calculation
		// either return or supply depending on user input
		static Real64 oldCondSupplyTemp( 0.0 ); // save the last iteration value of leaving condenser water temperature
		Real64 revisedEstimateAvailCap; // final estimate of available capacity if using leaving
		// condenser water temperature
		Real64 errorAvailCap; // error fraction on final estimate of AvailableCoolingCapacity
		int LoopNum;
		int LoopSideNum;
		Real64 rhoCW; // local fluid density for chilled water
		Real64 Cp_CW; // local fluid specific heat for chilled water
		Real64 rhoCD; // local fluid density for condenser water
		Real64 Cp_CD; // local fluid specific heat for condenser water

		// set node values to data structure values for nodes

		lChillReturnNodeNum = GasAbsorber( ChillNum ).ChillReturnNodeNum;
		lChillSupplyNodeNum = GasAbsorber( ChillNum ).ChillSupplyNodeNum;
		lCondReturnNodeNum = GasAbsorber( ChillNum ).CondReturnNodeNum;
		lCondSupplyNodeNum = GasAbsorber( ChillNum ).CondSupplyNodeNum;

		// set local copies of data from rest of input structure

		lNomCoolingCap = GasAbsorber( ChillNum ).NomCoolingCap;
		lFuelCoolRatio = GasAbsorber( ChillNum ).FuelCoolRatio;
		lFuelHeatRatio = GasAbsorber( ChillNum ).FuelHeatRatio;
		lElecCoolRatio = GasAbsorber( ChillNum ).ElecCoolRatio;
		lMinPartLoadRat = GasAbsorber( ChillNum ).MinPartLoadRat;
		lMaxPartLoadRat = GasAbsorber( ChillNum ).MaxPartLoadRat;
		lOptPartLoadRat = GasAbsorber( ChillNum ).OptPartLoadRat;
		lTempDesCondReturn = GasAbsorber( ChillNum ).TempDesCondReturn;
		lTempDesCHWSupply = GasAbsorber( ChillNum ).TempDesCHWSupply;
		lCondVolFlowRate = GasAbsorber( ChillNum ).CondVolFlowRate;
		lCoolCapFTCurve = GasAbsorber( ChillNum ).CoolCapFTCurve;
		lFuelCoolFTCurve = GasAbsorber( ChillNum ).FuelCoolFTCurve;
		lFuelCoolFPLRCurve = GasAbsorber( ChillNum ).FuelCoolFPLRCurve;
		lElecCoolFTCurve = GasAbsorber( ChillNum ).ElecCoolFTCurve;
		lElecCoolFPLRCurve = GasAbsorber( ChillNum ).ElecCoolFPLRCurve;
		lIsEnterCondensTemp = GasAbsorber( ChillNum ).isEnterCondensTemp;
		lIsWaterCooled = GasAbsorber( ChillNum ).isWaterCooled;
		lCHWLowLimitTemp = GasAbsorber( ChillNum ).CHWLowLimitTemp;
		lFuelHeatingValue = GasAbsorber( ChillNum ).FuelHeatingValue;

		lHeatElectricPower = GasAbsorberReport( ChillNum ).HeatElectricPower;
		lHeatFuelUseRate = GasAbsorberReport( ChillNum ).HeatFuelUseRate;
		lHeatPartLoadRatio = GasAbsorberReport( ChillNum ).HeatPartLoadRatio;

		// initialize entering conditions
		lChillReturnTemp = Node( lChillReturnNodeNum ).Temp;
		lChillWaterMassFlowRate = Node( lChillReturnNodeNum ).MassFlowRate;
		lCondReturnTemp = Node( lCondReturnNodeNum ).Temp;
		lCondWaterMassFlowRate = Node( lCondReturnNodeNum ).MassFlowRate;
		{ auto const SELECT_CASE_var( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			ChillSupplySetPointTemp = Node( lChillSupplyNodeNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			ChillSupplySetPointTemp = Node( lChillSupplyNodeNum ).TempSetPointHi;
		} else {
			assert( false );
		}}
		ChillDeltaTemp = std::abs( lChillReturnTemp - ChillSupplySetPointTemp );

		rhoCW = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidName, lChillReturnTemp, PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
		Cp_CW = GetSpecificHeatGlycol( PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidName, lChillReturnTemp, PlantLoop( GasAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
		rhoCD = GetDensityGlycol( PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidName, lChillReturnTemp, PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
		Cp_CD = GetSpecificHeatGlycol( PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidName, lChillReturnTemp, PlantLoop( GasAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

		//If no loop demand or Absorber OFF, return
		// will need to modify when absorber can act as a boiler
		if ( MyLoad >= 0 || ! ( ( GasAbsorber( ChillNum ).InHeatingMode ) || ( GasAbsorber( ChillNum ).InCoolingMode ) ) ) {
			//set node temperatures
			lChillSupplyTemp = lChillReturnTemp;
			lCondSupplyTemp = lCondReturnTemp;
			lCondWaterMassFlowRate = 0.0;
			if ( lIsWaterCooled ) {
				SetComponentFlowRate( lCondWaterMassFlowRate, GasAbsorber( ChillNum ).CondReturnNodeNum, GasAbsorber( ChillNum ).CondSupplyNodeNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum );

			}
			ChillDeltaTemp = 0.0;
			lFractionOfPeriodRunning = min( 1.0, max( lHeatPartLoadRatio, lCoolPartLoadRatio ) / lMinPartLoadRat );
		} else {

			// if water cooled use the input node otherwise just use outside air temperature
			if ( lIsWaterCooled ) {
				// most manufacturers rate have tables of entering condenser water temperature
				// but a few use leaving condenser water temperature so we have a flag
				// when leaving is used it uses the previous iterations value of the value
				lCondReturnTemp = Node( lCondReturnNodeNum ).Temp;
				if ( lIsEnterCondensTemp ) {
					calcCondTemp = lCondReturnTemp;
				} else {
					if ( oldCondSupplyTemp == 0 ) {
						oldCondSupplyTemp = lCondReturnTemp + 8.0; // if not previously estimated assume 8C greater than return
					}
					calcCondTemp = oldCondSupplyTemp;
				}
				//Set mass flow rates
				lCondWaterMassFlowRate = GasAbsorber( ChillNum ).DesCondMassFlowRate;
				SetComponentFlowRate( lCondWaterMassFlowRate, GasAbsorber( ChillNum ).CondReturnNodeNum, GasAbsorber( ChillNum ).CondSupplyNodeNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum );
			} else {
				// air cooled
				Node( lCondReturnNodeNum ).Temp = Node( lCondReturnNodeNum ).OutAirDryBulb;
				lCondReturnTemp = Node( lCondReturnNodeNum ).Temp;
				lCondWaterMassFlowRate = 0.0;
				SetComponentFlowRate( lCondWaterMassFlowRate, GasAbsorber( ChillNum ).CondReturnNodeNum, GasAbsorber( ChillNum ).CondSupplyNodeNum, GasAbsorber( ChillNum ).CDLoopNum, GasAbsorber( ChillNum ).CDLoopSideNum, GasAbsorber( ChillNum ).CDBranchNum, GasAbsorber( ChillNum ).CDCompNum );
			}

			//Determine available cooling capacity using the setpoint temperature
			lAvailableCoolingCapacity = lNomCoolingCap * CurveValue( lCoolCapFTCurve, ChillSupplySetPointTemp, calcCondTemp );

			//Calculate current load for cooling
			MyLoad = sign( max( std::abs( MyLoad ), lAvailableCoolingCapacity * lMinPartLoadRat ), MyLoad );
			MyLoad = sign( min( std::abs( MyLoad ), lAvailableCoolingCapacity * lMaxPartLoadRat ), MyLoad );

			// Determine the following variables depending on if the flow has been set in
			// the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
			//    chilled water flow,
			//    cooling load taken by the chiller, and
			//    supply temperature
			lChillWaterMassflowratemax = GasAbsorber( ChillNum ).DesEvapMassFlowRate;

			LoopNum = GasAbsorber( ChillNum ).CWLoopNum;
			LoopSideNum = GasAbsorber( ChillNum ).CWLoopSideNum;
			{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock );
			if ( SELECT_CASE_var == 0 ) { // mass flow rates may be changed by loop components
				GasAbsorber( ChillNum ).PossibleSubcooling = false;
				lCoolingLoad = std::abs( MyLoad );
				if ( ChillDeltaTemp != 0.0 ) {
					lChillWaterMassFlowRate = std::abs( lCoolingLoad / ( Cp_CW * ChillDeltaTemp ) );
					if ( lChillWaterMassFlowRate - lChillWaterMassflowratemax > MassFlowTolerance ) GasAbsorber( ChillNum ).PossibleSubcooling = true;

					SetComponentFlowRate( lChillWaterMassFlowRate, GasAbsorber( ChillNum ).ChillReturnNodeNum, GasAbsorber( ChillNum ).ChillSupplyNodeNum, GasAbsorber( ChillNum ).CWLoopNum, GasAbsorber( ChillNum ).CWLoopSideNum, GasAbsorber( ChillNum ).CWBranchNum, GasAbsorber( ChillNum ).CWCompNum );
					lChillSupplyTemp = ChillSupplySetPointTemp;
				} else {
					lChillWaterMassFlowRate = 0.0;
					ShowRecurringWarningErrorAtEnd( "GasAbsorberChillerModel:Cooling\"" + GasAbsorber( ChillNum ).Name + "\", DeltaTemp = 0 in mass flow calculation", GasAbsorber( ChillNum ).DeltaTempCoolErrCount );
				}
				lChillSupplyTemp = ChillSupplySetPointTemp;
			} else if ( SELECT_CASE_var == 1 ) { // mass flow rates may not be changed by loop components
				lChillWaterMassFlowRate = Node( lChillReturnNodeNum ).MassFlowRate;
				if ( GasAbsorber( ChillNum ).PossibleSubcooling ) {
					lCoolingLoad = std::abs( MyLoad );

					ChillDeltaTemp = lCoolingLoad / lChillWaterMassFlowRate / Cp_CW;
					lChillSupplyTemp = Node( lChillReturnNodeNum ).Temp - ChillDeltaTemp;
				} else {
					ChillDeltaTemp = Node( lChillReturnNodeNum ).Temp - ChillSupplySetPointTemp;
					lCoolingLoad = std::abs( lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp );
					lChillSupplyTemp = ChillSupplySetPointTemp;
				}
				//Check that the Chiller Supply outlet temp honors both plant loop temp low limit and also the chiller low limit
				if ( lChillSupplyTemp < lCHWLowLimitTemp ) {
					if ( ( Node( lChillReturnNodeNum ).Temp - lCHWLowLimitTemp ) > DeltaTempTol ) {
						lChillSupplyTemp = lCHWLowLimitTemp;
						ChillDeltaTemp = Node( lChillReturnNodeNum ).Temp - lChillSupplyTemp;
						lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
					} else {
						lChillSupplyTemp = Node( lChillReturnNodeNum ).Temp;
						ChillDeltaTemp = Node( lChillReturnNodeNum ).Temp - lChillSupplyTemp;
						lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
					}
				}
				if ( lChillSupplyTemp < Node( lChillSupplyNodeNum ).TempMin ) {
					if ( ( Node( lChillReturnNodeNum ).Temp - Node( lChillSupplyNodeNum ).TempMin ) > DeltaTempTol ) {
						lChillSupplyTemp = Node( lChillSupplyNodeNum ).TempMin;
						ChillDeltaTemp = Node( lChillReturnNodeNum ).Temp - lChillSupplyTemp;
						lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
					} else {
						lChillSupplyTemp = Node( lChillReturnNodeNum ).Temp;
						ChillDeltaTemp = Node( lChillReturnNodeNum ).Temp - lChillSupplyTemp;
						lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
					}
				}

				// Checks Coolingload on the basis of the machine limits.
				if ( lCoolingLoad > std::abs( MyLoad ) ) {
					if ( lChillWaterMassFlowRate > MassFlowTolerance ) {
						lCoolingLoad = std::abs( MyLoad );
						ChillDeltaTemp = lCoolingLoad / lChillWaterMassFlowRate / Cp_CW;
						lChillSupplyTemp = Node( lChillReturnNodeNum ).Temp - ChillDeltaTemp;
					} else {
						lChillSupplyTemp = Node( lChillReturnNodeNum ).Temp;
						ChillDeltaTemp = Node( lChillReturnNodeNum ).Temp - lChillSupplyTemp;
						lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
					}
				}
			}}

			//Calculate operating part load ratio for cooling
			PartLoadRat = min( std::abs( MyLoad ) / lAvailableCoolingCapacity, lMaxPartLoadRat );
			PartLoadRat = max( lMinPartLoadRat, PartLoadRat );

			if ( lAvailableCoolingCapacity > 0.0 ) {
				if ( std::abs( MyLoad ) / lAvailableCoolingCapacity < lMinPartLoadRat ) {
					lCoolPartLoadRatio = MyLoad / lAvailableCoolingCapacity;
				} else {
					lCoolPartLoadRatio = PartLoadRat;
				}
			} else { //Else if AvailableCoolingCapacity < 0.0
				lCoolPartLoadRatio = 0.0;
			}

			// calculate the fraction of the time period that the chiller would be running
			// use maximum from heating and cooling sides
			if ( lCoolPartLoadRatio < lMinPartLoadRat || lHeatPartLoadRatio < lMinPartLoadRat ) {
				lFractionOfPeriodRunning = min( 1.0, max( lHeatPartLoadRatio, lCoolPartLoadRatio ) / lMinPartLoadRat );
			} else {
				lFractionOfPeriodRunning = 1.0;
			}

			//Calculate fuel consumption for cooling
			// fuel used for cooling availCap * HIR * HIR-FT * HIR-FPLR
			lCoolFuelUseRate = lAvailableCoolingCapacity * lFuelCoolRatio * CurveValue( lFuelCoolFTCurve, lChillSupplyTemp, calcCondTemp ) * CurveValue( lFuelCoolFPLRCurve, lCoolPartLoadRatio ) * lFractionOfPeriodRunning;

			//Calculate electric parasitics used
			// based on nominal capacity, not available capacity,
			// electric used for cooling nomCap * %OP * EIR * EIR-FT * EIR-FPLR
			lCoolElectricPower = lNomCoolingCap * lElecCoolRatio * lFractionOfPeriodRunning * CurveValue( lElecCoolFTCurve, lChillSupplyTemp, calcCondTemp ) * CurveValue( lElecCoolFPLRCurve, lCoolPartLoadRatio );

			// determine conderser load which is cooling load plus the
			// fuel used for cooling times the burner efficiency plus
			// the electricity used
			lTowerLoad = lCoolingLoad + lCoolFuelUseRate / lFuelHeatRatio + lCoolElectricPower;

			// for water cooled condenser make sure enough flow rate
			// for air cooled condenser just set supply to return temperature
			if ( lIsWaterCooled ) {
				if ( lCondWaterMassFlowRate > MassFlowTolerance ) {
					lCondSupplyTemp = lCondReturnTemp + lTowerLoad / ( lCondWaterMassFlowRate * Cp_CD );
				} else {
					ShowSevereError( "CalcGasAbsorberChillerModel: Condenser flow = 0, for Gas Absorber Chiller=" + GasAbsorber( ChillNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowFatalError( "Program Terminates due to previous error condition." );
				}
			} else {
				lCondSupplyTemp = lCondReturnTemp; //if air cooled condenser just set supply and return to same temperature
			}

			// save the condenser water supply temperature for next iteration if that is used in lookup
			// and if capacity is large enough error than report problem
			oldCondSupplyTemp = lCondSupplyTemp;
			if ( ! lIsEnterCondensTemp ) {
				// calculate the fraction of the estimated error between the capacity based on the previous
				// iteration's value of condenser supply temperature and the actual calculated condenser supply
				// temperature.  If this becomes too common then may need to iterate a solution instead of
				// relying on previous iteration method.
				revisedEstimateAvailCap = lNomCoolingCap * CurveValue( lCoolCapFTCurve, ChillSupplySetPointTemp, lCondSupplyTemp );
				if ( revisedEstimateAvailCap > 0.0 ) {
					errorAvailCap = std::abs( ( revisedEstimateAvailCap - lAvailableCoolingCapacity ) / revisedEstimateAvailCap );
					if ( errorAvailCap > 0.05 ) { // if more than 5% error in estimate
						ShowRecurringWarningErrorAtEnd( "GasAbsorberChillerModel:\"" + GasAbsorber( ChillNum ).Name + "\", poor Condenser Supply Estimate", GasAbsorber( ChillNum ).CondErrCount, errorAvailCap, errorAvailCap );
					}
				}
			}
		} // IF(MyLoad>=0 .OR. .NOT. RunFlag)
		// Write into the Report Variables except for nodes
		GasAbsorberReport( ChillNum ).CoolingLoad = lCoolingLoad;
		GasAbsorberReport( ChillNum ).TowerLoad = lTowerLoad;
		GasAbsorberReport( ChillNum ).CoolFuelUseRate = lCoolFuelUseRate;
		GasAbsorberReport( ChillNum ).CoolElectricPower = lCoolElectricPower;
		GasAbsorberReport( ChillNum ).CondReturnTemp = lCondReturnTemp;
		GasAbsorberReport( ChillNum ).ChillReturnTemp = lChillReturnTemp;
		GasAbsorberReport( ChillNum ).CondSupplyTemp = lCondSupplyTemp;
		GasAbsorberReport( ChillNum ).ChillSupplyTemp = lChillSupplyTemp;
		GasAbsorberReport( ChillNum ).ChillWaterFlowRate = lChillWaterMassFlowRate;
		GasAbsorberReport( ChillNum ).CondWaterFlowRate = lCondWaterMassFlowRate;
		GasAbsorberReport( ChillNum ).CoolPartLoadRatio = lCoolPartLoadRatio;
		GasAbsorberReport( ChillNum ).CoolingCapacity = lAvailableCoolingCapacity;
		GasAbsorberReport( ChillNum ).FractionOfPeriodRunning = lFractionOfPeriodRunning;

		// write the combined heating and cooling fuel used and electric used
		GasAbsorberReport( ChillNum ).FuelUseRate = lCoolFuelUseRate + lHeatFuelUseRate;
		GasAbsorberReport( ChillNum ).ElectricPower = lCoolElectricPower + lHeatElectricPower;

	}

	void
	CalcGasAbsorberHeaterModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const RunFlag // TRUE when Absorber operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer and Michael J. Witte
		//       DATE WRITTEN   March 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a direct fired (gas consuming) absorption chiller using
		// curves and inputs similar to DOE-2.1e

		// METHODOLOGY EMPLOYED:
		// Curve fit of performance data

		// REFERENCES:
		// 1.  DOE-2.1e Supplement and source code
		// 2.  CoolTools GasMod work

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// FlowLock = 0  if mass flow rates may be changed by loop components
		// FlowLock = 1  if mass flow rates may not be changed by loop components
		// FlowLock = 2  if overloaded and mass flow rates has changed to a small amount and Tout drops
		//                 below Setpoint

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcGasAbsorberHeaterModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Local copies of GasAbsorberSpecs Type
		// all variables that are local copies of data structure
		// variables are prefaced with an "l" for local.
		Real64 lNomCoolingCap; // W - design nominal capacity of Absorber
		Real64 lNomHeatCoolRatio; // ratio of heating to cooling capacity
		Real64 lFuelHeatRatio; // ratio of fuel input to heating output
		Real64 lElecHeatRatio; // ratio of electricity input to heating output
		int lHeatReturnNodeNum; // absorber steam inlet node number, water side
		int lHeatSupplyNodeNum; // absorber steam outlet node number, water side
		Real64 lMinPartLoadRat; // min allowed operating frac full load
		Real64 lMaxPartLoadRat; // max allowed operating frac full load
		Real64 lOptPartLoadRat; // optimal operating frac full load
		int lHeatCapFCoolCurve; // Heating Capacity Function of Cooling Capacity Curve
		int lFuelHeatFHPLRCurve; // Fuel Input to heat output ratio during heating only function
		Real64 lFuelHeatingValue( 0.0 );
		// Local copies of GasAbsorberReportVars Type
		Real64 lHeatingLoad( 0.0 ); // heating load on the chiller
		// Real64 lHeatingEnergy( 0.0 ); // heating energy
		// Real64 lFuelUseRate( 0.0 ); // instantaneous use of gas for period
		// Real64 lFuelEnergy( 0.0 ); // variable to track total fuel used for a period
		Real64 lCoolFuelUseRate( 0.0 ); // instantaneous use of gas for period for cooling
		Real64 lHeatFuelUseRate( 0.0 ); // instantaneous use of gas for period for heating
		// Real64 lHeatFuelEnergy( 0.0 ); // variable to track total fuel used for a period for heating
		// Real64 lElectricPower( 0.0 ); // parasitic electric power used (was PumpingPower)
		// Real64 lElectricEnergy( 0.0 ); // track the total electricity used for a period (was PumpingEnergy)
		Real64 lCoolElectricPower( 0.0 ); // parasitic electric power used  for cooling
		Real64 lHeatElectricPower( 0.0 ); // parasitic electric power used  for heating
		// Real64 lHeatElectricEnergy( 0.0 ); // track the total electricity used for a period for heating
		Real64 lHotWaterReturnTemp( 0.0 ); // reporting: hot water return (inlet) temperature
		Real64 lHotWaterSupplyTemp( 0.0 ); // reporting: hot water supply (outlet) temperature
		Real64 lHotWaterMassFlowRate( 0.0 ); // reporting: hot water mass flow rate
		Real64 lCoolPartLoadRatio( 0.0 ); // operating part load ratio (load/capacity for cooling)
		Real64 lHeatPartLoadRatio( 0.0 ); // operating part load ratio (load/capacity for heating)
		Real64 lAvailableHeatingCapacity( 0.0 ); // current heating capacity
		Real64 lFractionOfPeriodRunning( 0.0 );
		Real64 lHotWaterMassFlowRateMax( 0.0 ); // Maximum flow rate through the evaporator
		// other local variables
		Real64 HeatDeltaTemp( 0.0 ); // hot water temperature difference
		Real64 HeatSupplySetPointTemp( 0.0 );
		int LoopNum;
		int LoopSideNum;
		Real64 Cp_HW; // local fluid specific heat for hot water
		Real64 rhoHW; // local fluid density for hot water

		// set node values to data structure values for nodes

		lHeatReturnNodeNum = GasAbsorber( ChillNum ).HeatReturnNodeNum;
		lHeatSupplyNodeNum = GasAbsorber( ChillNum ).HeatSupplyNodeNum;

		// set local copies of data from rest of input structure

		lNomCoolingCap = GasAbsorber( ChillNum ).NomCoolingCap;
		lNomHeatCoolRatio = GasAbsorber( ChillNum ).NomHeatCoolRatio;
		lFuelHeatRatio = GasAbsorber( ChillNum ).FuelHeatRatio;
		lElecHeatRatio = GasAbsorber( ChillNum ).ElecHeatRatio;
		lMinPartLoadRat = GasAbsorber( ChillNum ).MinPartLoadRat;
		lMaxPartLoadRat = GasAbsorber( ChillNum ).MaxPartLoadRat;
		lOptPartLoadRat = GasAbsorber( ChillNum ).OptPartLoadRat;
		lHeatCapFCoolCurve = GasAbsorber( ChillNum ).HeatCapFCoolCurve;
		lFuelHeatFHPLRCurve = GasAbsorber( ChillNum ).FuelHeatFHPLRCurve;
		lFuelHeatingValue = GasAbsorber( ChillNum ).FuelHeatingValue;
		lHotWaterMassFlowRateMax = GasAbsorber( ChillNum ).DesHeatMassFlowRate;
		LoopNum = GasAbsorber( ChillNum ).HWLoopNum;
		LoopSideNum = GasAbsorber( ChillNum ).HWLoopSideNum;

		Cp_HW = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, lHotWaterReturnTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		rhoHW = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, lHotWaterReturnTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

		lCoolElectricPower = GasAbsorberReport( ChillNum ).CoolElectricPower;
		lCoolFuelUseRate = GasAbsorberReport( ChillNum ).CoolFuelUseRate;
		lCoolPartLoadRatio = GasAbsorberReport( ChillNum ).CoolPartLoadRatio;

		// initialize entering conditions
		lHotWaterReturnTemp = Node( lHeatReturnNodeNum ).Temp;
		lHotWaterMassFlowRate = Node( lHeatReturnNodeNum ).MassFlowRate;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			HeatSupplySetPointTemp = Node( lHeatSupplyNodeNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			HeatSupplySetPointTemp = Node( lHeatSupplyNodeNum ).TempSetPointLo;
		} else {
			assert( false );
		}}
		HeatDeltaTemp = std::abs( lHotWaterReturnTemp - HeatSupplySetPointTemp );

		//If no loop demand or Absorber OFF, return
		// will need to modify when absorber can act as a boiler
		if ( MyLoad <= 0 || ! RunFlag ) {
			//set node temperatures
			lHotWaterSupplyTemp = lHotWaterReturnTemp;
			HeatDeltaTemp = 0.0;
			lFractionOfPeriodRunning = min( 1.0, max( lHeatPartLoadRatio, lCoolPartLoadRatio ) / lMinPartLoadRat );
		} else {

			//Determine available heating capacity using the current cooling load
			lAvailableHeatingCapacity = GasAbsorber( ChillNum ).NomHeatCoolRatio * GasAbsorber( ChillNum ).NomCoolingCap * CurveValue( lHeatCapFCoolCurve, ( GasAbsorberReport( ChillNum ).CoolingLoad / GasAbsorber( ChillNum ).NomCoolingCap ) );

			//Calculate current load for heating
			MyLoad = sign( max( std::abs( MyLoad ), GasAbsorberReport( ChillNum ).HeatingCapacity * lMinPartLoadRat ), MyLoad );
			MyLoad = sign( min( std::abs( MyLoad ), GasAbsorberReport( ChillNum ).HeatingCapacity * lMaxPartLoadRat ), MyLoad );

			// Determine the following variables depending on if the flow has been set in
			// the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
			//    chilled water flow,
			//    cooling load taken by the chiller, and
			//    supply temperature
			{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock );
			if ( SELECT_CASE_var == 0 ) { // mass flow rates may be changed by loop components
				lHeatingLoad = std::abs( MyLoad );
				if ( HeatDeltaTemp != 0 ) {
					lHotWaterMassFlowRate = std::abs( lHeatingLoad / ( Cp_HW * HeatDeltaTemp ) );

					SetComponentFlowRate( lHotWaterMassFlowRate, GasAbsorber( ChillNum ).HeatReturnNodeNum, GasAbsorber( ChillNum ).HeatSupplyNodeNum, GasAbsorber( ChillNum ).HWLoopNum, GasAbsorber( ChillNum ).HWLoopSideNum, GasAbsorber( ChillNum ).HWBranchNum, GasAbsorber( ChillNum ).HWCompNum );

				} else {
					lHotWaterMassFlowRate = 0.0;
					ShowRecurringWarningErrorAtEnd( "GasAbsorberChillerModel:Heating\"" + GasAbsorber( ChillNum ).Name + "\", DeltaTemp = 0 in mass flow calculation", GasAbsorber( ChillNum ).DeltaTempHeatErrCount );
				}
				lHotWaterSupplyTemp = HeatSupplySetPointTemp;
			} else if ( SELECT_CASE_var == 1 ) { // mass flow rates may not be changed by loop components
				lHotWaterSupplyTemp = HeatSupplySetPointTemp;
				lHeatingLoad = std::abs( lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp );

				//DSU this "2" is not a real state for flowLock
			} else if ( SELECT_CASE_var == 2 ) { // chiller is underloaded and mass flow rates has changed to a small amount and Tout drops below Setpoint

				//DSU? this component model needs a lot of work, does not honor limits, incomplete ...

				// MJW Not sure what to do with this now
				// Must make adjustment to supply temperature since load is greater than available capacity
				// this also affects the available capacity itself since it is a function of supply temperature
				// Since these curves are generally fairly flat just use an estimate (done above) and correction
				// approach instead of iterating to a solution.
				// MJW 07MAR01 Logic seems wrong here, because of misunderstanding of what "overload" means
				//  "overload" means the chiller is overcooling the branch.  See SUBROUTINE DistributeLoad
				//      IF (lChillWaterMassFlowRate > MassFlowTol) THEN
				//        ChillDeltaTemp = MyLoad / (CPCW(lChillReturnTemp) * lChillWaterMassFlowRate)
				//        lChillSupplyTemp = lChillReturnTemp - ChillDeltaTemp
				//        lAvailableCoolingCapacity = lNomCoolingCap * CurveValue(lCoolCapFTCurve,lChillSupplyTemp,calcCondTemp)
				//      ELSE
				//        ErrCount = ErrCount + 1
				//        IF (ErrCount < 10) THEN
				//          CALL ShowWarningError('GasAbsorberModel:lChillWaterMassFlowRate near 0 in available capacity calculation')
				//        END IF
				//      END IF

				// MJW 07MAR01 Borrow logic from steam absorption module
				// The following conditional statements are made to avoid extremely small EvapMdot
				// & unreasonable EvapOutletTemp due to overloading.
				// Avoid 'divide by zero' due to small EvapMdot
				if ( lHotWaterMassFlowRate < MassFlowTolerance ) {
					HeatDeltaTemp = 0.0;
				} else {
					HeatDeltaTemp = std::abs( MyLoad ) / ( Cp_HW * lHotWaterMassFlowRate );
				}
				lHotWaterSupplyTemp = lHotWaterReturnTemp + HeatDeltaTemp;

				lHeatingLoad = std::abs( lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp );
			}}

			//Calculate operating part load ratio for cooling
			lHeatPartLoadRatio = lHeatingLoad / lAvailableHeatingCapacity;

			//Calculate fuel consumption for cooling
			// fuel used for cooling availCap * HIR * HIR-FT * HIR-FPLR

			lHeatFuelUseRate = lAvailableHeatingCapacity * lFuelHeatRatio * CurveValue( lFuelHeatFHPLRCurve, lHeatPartLoadRatio );

			// calculate the fraction of the time period that the chiller would be running
			// use maximum from heating and cooling sides
			lFractionOfPeriodRunning = min( 1.0, max( lHeatPartLoadRatio, lCoolPartLoadRatio ) / lMinPartLoadRat );

			//Calculate electric parasitics used
			// for heating based on nominal capacity not available capacity
			lHeatElectricPower = lNomCoolingCap * lNomHeatCoolRatio * lElecHeatRatio * lFractionOfPeriodRunning;
			// Coodinate electric parasitics for heating and cooling to avoid double counting
			// Total electric is the max of heating electric or cooling electric
			// If heating electric is greater, leave cooling electric and subtract if off of heating elec
			// If cooling electric is greater, set heating electric to zero
			if ( lHeatElectricPower <= lCoolElectricPower ) {
				lHeatElectricPower = 0.0;
			} else {
				lHeatElectricPower -= lCoolElectricPower;
			}

		} // IF(MyLoad==0 .OR. .NOT. RunFlag)
		// Write into the Report Variables except for nodes
		GasAbsorberReport( ChillNum ).HeatingLoad = lHeatingLoad;
		GasAbsorberReport( ChillNum ).HeatFuelUseRate = lHeatFuelUseRate;
		GasAbsorberReport( ChillNum ).HeatElectricPower = lHeatElectricPower;
		GasAbsorberReport( ChillNum ).HotWaterReturnTemp = lHotWaterReturnTemp;
		GasAbsorberReport( ChillNum ).HotWaterSupplyTemp = lHotWaterSupplyTemp;
		GasAbsorberReport( ChillNum ).HotWaterFlowRate = lHotWaterMassFlowRate;
		GasAbsorberReport( ChillNum ).HeatPartLoadRatio = lHeatPartLoadRatio;
		GasAbsorberReport( ChillNum ).HeatingCapacity = lAvailableHeatingCapacity;
		GasAbsorberReport( ChillNum ).FractionOfPeriodRunning = lFractionOfPeriodRunning;

		// write the combined heating and cooling fuel used and electric used
		GasAbsorberReport( ChillNum ).FuelUseRate = lCoolFuelUseRate + lHeatFuelUseRate;
		GasAbsorberReport( ChillNum ).ElectricPower = lCoolElectricPower + lHeatElectricPower;

	}

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateGasAbsorberCoolRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const ChillNum // Absorber number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int lChillReturnNodeNum; // Node number on the inlet side of the plant
		int lChillSupplyNodeNum; // Node number on the outlet side of the plant
		int lCondReturnNodeNum; // Node number on the inlet side of the condenser
		int lCondSupplyNodeNum; // Node number on the outlet side of the condenser

		// BEGIN ROUTINE

		lChillReturnNodeNum = GasAbsorber( ChillNum ).ChillReturnNodeNum;
		lChillSupplyNodeNum = GasAbsorber( ChillNum ).ChillSupplyNodeNum;
		lCondReturnNodeNum = GasAbsorber( ChillNum ).CondReturnNodeNum;
		lCondSupplyNodeNum = GasAbsorber( ChillNum ).CondSupplyNodeNum;

		if ( MyLoad == 0 || ! RunFlag ) {
			//set node temperatures

			Node( lChillSupplyNodeNum ).Temp = Node( lChillReturnNodeNum ).Temp;
			Node( lCondSupplyNodeNum ).Temp = Node( lCondReturnNodeNum ).Temp;

			//set node flow rates
			//Update Outlet Conditions so that same as Inlet, so component
			//can be bypassed if necessary
			//FlowResolver/EnforceSplitterContinuity will determine flow
			//received, whether component is running or not.
			//    Node(lChillReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lChillSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lCondReturnNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
			//    Node(lCondSupplyNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
		} else {
			//set node temperatures
			Node( lChillSupplyNodeNum ).Temp = GasAbsorberReport( ChillNum ).ChillSupplyTemp;
			Node( lCondSupplyNodeNum ).Temp = GasAbsorberReport( ChillNum ).CondSupplyTemp;
			//set node flow rates;  for these load based models
			//assume that the sufficient evaporator flow rate available
			//    Node(lChillReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lChillSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lCondReturnNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
			//    Node(lCondSupplyNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
		}

		// convert power to energy and instantaneous use to use over the time step
		GasAbsorberReport( ChillNum ).CoolingEnergy = GasAbsorberReport( ChillNum ).CoolingLoad * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).TowerEnergy = GasAbsorberReport( ChillNum ).TowerLoad * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).FuelEnergy = GasAbsorberReport( ChillNum ).FuelUseRate * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).CoolFuelEnergy = GasAbsorberReport( ChillNum ).CoolFuelUseRate * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).ElectricEnergy = GasAbsorberReport( ChillNum ).ElectricPower * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).CoolElectricEnergy = GasAbsorberReport( ChillNum ).CoolElectricPower * TimeStepSys * SecInHour;
		if ( GasAbsorberReport( ChillNum ).CoolFuelUseRate != 0.0 ) {
			GasAbsorberReport( ChillNum ).FuelCOP = GasAbsorberReport( ChillNum ).CoolingLoad / GasAbsorberReport( ChillNum ).CoolFuelUseRate;
		} else {
			GasAbsorberReport( ChillNum ).FuelCOP = 0.0;
		}
		//  Node(lChillSupplyNodeNum)%MassFlowRateMaxAvail = Node(lChillReturnNodeNum)%MassFlowRateMaxAvail
		//  Node(lChillSupplyNodeNum)%MassFlowRateMinAvail = Node(lChillReturnNodeNum)%MassFlowRateMinAvail
	}

	void
	UpdateGasAbsorberHeatRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const ChillNum // Absorber number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int lHeatReturnNodeNum; // absorber steam inlet node number, water side
		int lHeatSupplyNodeNum; // absorber steam outlet node number, water side

		// BEGIN ROUTINE

		lHeatReturnNodeNum = GasAbsorber( ChillNum ).HeatReturnNodeNum;
		lHeatSupplyNodeNum = GasAbsorber( ChillNum ).HeatSupplyNodeNum;

		if ( MyLoad == 0 || ! RunFlag ) {
			//set node temperatures
			Node( lHeatSupplyNodeNum ).Temp = Node( lHeatReturnNodeNum ).Temp;

			//set node flow rates
			//Update Outlet Conditions so that same as Inlet, so component
			//can be bypassed if necessary
			//FlowResolver/EnforceSplitterContinuity will determine flow
			//received, whether component is running or not.
			//    Node(lHeatReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
			//    Node(lHeatSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
		} else {
			//set node temperatures
			Node( lHeatSupplyNodeNum ).Temp = GasAbsorberReport( ChillNum ).HotWaterSupplyTemp;
			//          !set node flow rates;  for these load based models
			//          !assume that the sufficient evaporator flow rate available
			//    Node(lHeatReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
			//    Node(lHeatSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
		}

		// convert power to energy and instantaneous use to use over the time step
		GasAbsorberReport( ChillNum ).HeatingEnergy = GasAbsorberReport( ChillNum ).HeatingLoad * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).FuelEnergy = GasAbsorberReport( ChillNum ).FuelUseRate * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).HeatFuelEnergy = GasAbsorberReport( ChillNum ).HeatFuelUseRate * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).ElectricEnergy = GasAbsorberReport( ChillNum ).ElectricPower * TimeStepSys * SecInHour;
		GasAbsorberReport( ChillNum ).HeatElectricEnergy = GasAbsorberReport( ChillNum ).HeatElectricPower * TimeStepSys * SecInHour;
		//  Node(lHeatSupplyNodeNum)%MassFlowRateMaxAvail = Node(lHeatReturnNodeNum)%MassFlowRateMaxAvail
		//  Node(lHeatSupplyNodeNum)%MassFlowRateMinAvail = Node(lHeatReturnNodeNum)%MassFlowRateMinAvail
	}

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright (c) Gas Research Institute 2001.  All rights reserved.

	//     GRI LEGAL NOTICE
	//     Neither GRI, members of GRI nor any person or organization acting on behalf
	//     of either:

	//     A. Makes any warranty of representation, express or implied with respect to
	//        the accuracy, completness, or usefulness of the information contained in
	//        in this program, including any warranty of merchantability or fitness of
	//        any purpose with respoect to the program, or that the use of any
	//        information disclosed in this program may not infringe privately-owned
	//        rights, or

	//     B.  Assumes any liability with respoct to the use of, or for any and all
	//         damages resulting from the use of the program or any portion thereof or
	//         any information disclosed therein.

} // ChillerGasAbsorption

} // EnergyPlus
