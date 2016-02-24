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
#include <ChillerExhaustAbsorption.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
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
#include <MicroturbineElectricGenerator.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerExhaustAbsorption {

	// MODULE INFORMATION:
	//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
	//                   for Gas Research Institute (Original module GasAbsoptionChiller)
	//    DATE WRITTEN   March 2001
	//    MODIFIED       Brent Griffith, Nov 2010 plant upgrades, generalize fluid properties
	//                   Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Absorption Chiller
	//    RE-ENGINEERED  na
	// PURPOSE OF THIS MODULE:
	//    This module simulates the performance of the Exhaust fired double effect
	//    absorption chiller.
	// METHODOLOGY EMPLOYED:
	//    Once the PlantLoopManager determines that the exhasut fired absorber chiller
	//    is available to meet a loop cooling demand, it calls SimExhaustAbsorption
	//    which in turn calls the appropriate Exhaut Fired Absorption Chiller model.
	// REFERENCES:
	//    DOE-2.1e Supplement
	//    PG&E CoolToolsGas Mod
	//    Performnace curves obtained from manufcaturer
	// OTHER NOTES:
	//    The curves on this model follow the DOE-2 approach of using
	//    electric and heat input ratios.  In addition, the temperature
	//    correction curve has two independent variables for the
	//    chilled water temperature and either the entering or leaving
	//    condenser water temperature.
	//    The code was originally adopted from the ChillerAbsorption
	//    routine but has been extensively modified.
	//    Development of the original(GasAbsoptionChiller) module was funded by the Gas Research Institute.
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
	using Psychrometrics::RhoH2O;
	using Psychrometrics::CPCW;
	using Psychrometrics::CPHW;
	using DataGlobalConstants::iGeneratorMicroturbine;
	using MicroturbineElectricGenerator::SimMTGenerator;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	int NumExhaustAbsorbers( 0 ); // number of Absorption Chillers specified in input

	// This type holds the output from the algorithm i.e., the Report Variables

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Object Data
	Array1D< ExhaustAbsorberSpecs > ExhaustAbsorber; // dimension to number of machines
	Array1D< ReportVars > ExhaustAbsorberReport;

	// MODULE SUBROUTINES:

	// Beginning of Absorption Chiller Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimExhaustAbsorber(
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
		Real64 & SizingFactor // sizing factor
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001
		//       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Double Effect Absorption Chiller
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using CurveManager::CurveValue;
		using DataPlant::TypeOf_Chiller_ExhFiredAbsorption;
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
		//unused  INTEGER           :: CondReturnNodeNum !holds the node number for the condenser side return
		//unused  REAL(r64)         :: CondMassFlowRate !the rate of mass flow for the condenser (estimated)
		int ChillNum; // Absorber number counter

		//Get Absorber data from input file
		if ( GetInput ) {
			GetExhaustAbsorberInput();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			ChillNum = FindItemInList( AbsorberName, ExhaustAbsorber );
			if ( ChillNum == 0 ) {
				ShowFatalError( "SimExhaustAbsorber: Unit not found=" + AbsorberName );
			}
			CompIndex = ChillNum;
		} else {
			ChillNum = CompIndex;
			if ( ChillNum > NumExhaustAbsorbers || ChillNum < 1 ) {
				ShowFatalError( "SimExhaustAbsorber:  Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Number of Units=" + TrimSigDigits( NumExhaustAbsorbers ) + ", Entered Unit name=" + AbsorberName );
			}
			if ( CheckEquipName( ChillNum ) ) {
				if ( AbsorberName != ExhaustAbsorber( ChillNum ).Name ) {
					ShowFatalError( "SimExhaustAbsorber: Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Unit name=" + AbsorberName + ", stored Unit Name for that index=" + ExhaustAbsorber( ChillNum ).Name );
				}
				CheckEquipName( ChillNum ) = false;
			}
		}

		// Check that this is a valid call
		if ( InitLoopEquip ) {

			InitExhaustAbsorber( ChillNum, RunFlag );

			// Match inlet node name of calling branch to determine if this call is for heating or cooling
			if ( BranchInletNodeNum == ExhaustAbsorber( ChillNum ).ChillReturnNodeNum ) { // Operate as chiller
				SizeExhaustAbsorber( ChillNum ); // only call from chilled water loop
				MinCap = ExhaustAbsorber( ChillNum ).NomCoolingCap * ExhaustAbsorber( ChillNum ).MinPartLoadRat;
				MaxCap = ExhaustAbsorber( ChillNum ).NomCoolingCap * ExhaustAbsorber( ChillNum ).MaxPartLoadRat;
				OptCap = ExhaustAbsorber( ChillNum ).NomCoolingCap * ExhaustAbsorber( ChillNum ).OptPartLoadRat;
			} else if ( BranchInletNodeNum == ExhaustAbsorber( ChillNum ).HeatReturnNodeNum ) { // Operate as heater
				HeatCap = ExhaustAbsorber( ChillNum ).NomCoolingCap * ExhaustAbsorber( ChillNum ).NomHeatCoolRatio;
				MinCap = HeatCap * ExhaustAbsorber( ChillNum ).MinPartLoadRat;
				MaxCap = HeatCap * ExhaustAbsorber( ChillNum ).MaxPartLoadRat;
				OptCap = HeatCap * ExhaustAbsorber( ChillNum ).OptPartLoadRat;
			} else if ( BranchInletNodeNum == ExhaustAbsorber( ChillNum ).CondReturnNodeNum ) { // called from condenser loop
				HeatCap = 0.0;
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			} else { // Error, nodes do not match
				ShowSevereError( "SimExhaustAbsorber: Invalid call to Exhaust Absorbtion Chiller-Heater " + AbsorberName );
				ShowContinueError( "Node connections in branch are not consistent with object nodes." );
				ShowFatalError( "Preceding conditions cause termination." );
			} // Operate as Chiller or Heater
			if ( GetSizingFactor ) {
				SizingFactor = ExhaustAbsorber( ChillNum ).SizFac;
			}
			return;
		}

		// Match inlet node name of calling branch to determine if this call is for heating or cooling
		if ( BranchInletNodeNum == ExhaustAbsorber( ChillNum ).ChillReturnNodeNum ) { // Operate as chiller
			// Calculate Node Values
			// Calculate Equipment and Update Variables
			if ( RunFlag ) {
				ExhaustAbsorber( ChillNum ).InCoolingMode = true;
			} else {
				ExhaustAbsorber( ChillNum ).InCoolingMode = false;
			}
			InitExhaustAbsorber( ChillNum, RunFlag );
			CalcExhaustAbsorberChillerModel( ChillNum, MyLoad, RunFlag );
			UpdateExhaustAbsorberCoolRecords( MyLoad, RunFlag, ChillNum );
		} else if ( BranchInletNodeNum == ExhaustAbsorber( ChillNum ).HeatReturnNodeNum ) { // Operate as heater
			// Calculate Node Values
			// Calculate Equipment and Update Variables
			if ( RunFlag ) {
				ExhaustAbsorber( ChillNum ).InHeatingMode = true;
			} else {
				ExhaustAbsorber( ChillNum ).InHeatingMode = false;
			}
			InitExhaustAbsorber( ChillNum, RunFlag );
			CalcExhaustAbsorberHeaterModel( ChillNum, MyLoad, RunFlag );
			UpdateExhaustAbsorberHeatRecords( MyLoad, RunFlag, ChillNum );
		} else if ( BranchInletNodeNum == ExhaustAbsorber( ChillNum ).CondReturnNodeNum ) { // called from condenser loop
			UpdateChillerComponentCondenserSide( ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_ExhFiredAbsorption, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, ExhaustAbsorber( ChillNum ).CondSupplyNodeNum, ExhaustAbsorberReport( ChillNum ).TowerLoad, ExhaustAbsorberReport( ChillNum ).CondReturnTemp, ExhaustAbsorberReport( ChillNum ).CondSupplyTemp, ExhaustAbsorberReport( ChillNum ).CondWaterFlowRate, FirstIteration );

		} else { // Error, nodes do not match
			ShowSevereError( "Invalid call to Exhaust Absorber Chiller " + AbsorberName );
			ShowContinueError( "Node connections in branch are not consistent with object nodes." );
			ShowFatalError( "Preceding conditions cause termination." );
		}

	}

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetExhaustAbsorberInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Jason Glazer
		//       DATE WRITTEN:    March 2001
		//       MODIFIED         Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Double Effect Absorption Chiller
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the Exhaust Fired Absorption chiller model in the object ChillerHeater:Absorption:DoubleEffect

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
		using MicroturbineElectricGenerator::GetMTGeneratorExhaustNode;
		using DataSizing::AutoSize;

		// Locals
		// PARAMETERS

		//LOCAL VARIABLES
		int AbsorberNum; // Absorber counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		int MTExhaustNodeNum; // Exhaust node number passed from MicroTurbine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string ChillerName;
		bool errFlag;
		bool Okay;

		//FLOW
		cCurrentModuleObject = "ChillerHeater:Absorption:DoubleEffect";
		NumExhaustAbsorbers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumExhaustAbsorbers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment found in input file" );
			ErrorsFound = true;
		}

		if ( allocated( ExhaustAbsorber ) ) return;

		//ALLOCATE ARRAYS
		ExhaustAbsorber.allocate( NumExhaustAbsorbers );

		ExhaustAbsorberReport.allocate( NumExhaustAbsorbers );
		CheckEquipName.dimension( NumExhaustAbsorbers, true );

		//LOAD ARRAYS

		for ( AbsorberNum = 1; AbsorberNum <= NumExhaustAbsorbers; ++AbsorberNum ) {
			GetObjectItem( cCurrentModuleObject, AbsorberNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ExhaustAbsorber, AbsorberNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			ExhaustAbsorber( AbsorberNum ).Name = cAlphaArgs( 1 );
			ChillerName = cCurrentModuleObject + " Named " + ExhaustAbsorber( AbsorberNum ).Name;

			// Assign capacities
			ExhaustAbsorber( AbsorberNum ).NomCoolingCap = rNumericArgs( 1 );
			if ( ExhaustAbsorber( AbsorberNum ).NomCoolingCap == AutoSize ) {
				ExhaustAbsorber( AbsorberNum ).NomCoolingCapWasAutoSized = true;
			}
			ExhaustAbsorber( AbsorberNum ).NomHeatCoolRatio = rNumericArgs( 2 );
			// Assign efficiencies
			ExhaustAbsorber( AbsorberNum ).ThermalEnergyCoolRatio = rNumericArgs( 3 );
			ExhaustAbsorber( AbsorberNum ).ThermalEnergyHeatRatio = rNumericArgs( 4 );
			ExhaustAbsorber( AbsorberNum ).ElecCoolRatio = rNumericArgs( 5 );
			ExhaustAbsorber( AbsorberNum ).ElecHeatRatio = rNumericArgs( 6 );

			// Assign Node Numbers to specified nodes
			ExhaustAbsorber( AbsorberNum ).ChillReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ExhaustAbsorber( AbsorberNum ).ChillSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Chilled Water Nodes" );
			// Condenser node processing depends on condenser type, see below
			ExhaustAbsorber( AbsorberNum ).HeatReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
			ExhaustAbsorber( AbsorberNum ).HeatSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Hot Water Nodes" );
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing node input for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = false;
			}

			// Assign Part Load Ratios
			ExhaustAbsorber( AbsorberNum ).MinPartLoadRat = rNumericArgs( 7 );
			ExhaustAbsorber( AbsorberNum ).MaxPartLoadRat = rNumericArgs( 8 );
			ExhaustAbsorber( AbsorberNum ).OptPartLoadRat = rNumericArgs( 9 );
			// Assign Design Conditions
			ExhaustAbsorber( AbsorberNum ).TempDesCondReturn = rNumericArgs( 10 );
			ExhaustAbsorber( AbsorberNum ).TempDesCHWSupply = rNumericArgs( 11 );
			ExhaustAbsorber( AbsorberNum ).EvapVolFlowRate = rNumericArgs( 12 );
			if ( ExhaustAbsorber( AbsorberNum ).EvapVolFlowRate == AutoSize ) {
				ExhaustAbsorber( AbsorberNum ).EvapVolFlowRateWasAutoSized = true;
			}
			if ( SameString( cAlphaArgs( 16 ), "AirCooled" ) ) {
				ExhaustAbsorber( AbsorberNum ).CondVolFlowRate = 0.0011; // Condenser flow rate not used for this cond type
			} else {
				ExhaustAbsorber( AbsorberNum ).CondVolFlowRate = rNumericArgs( 13 );
				if ( ExhaustAbsorber( AbsorberNum ).CondVolFlowRate == AutoSize ) {
					ExhaustAbsorber( AbsorberNum ).CondVolFlowRateWasAutoSized = true;
				}
			}
			ExhaustAbsorber( AbsorberNum ).HeatVolFlowRate = rNumericArgs( 14 );
			if ( ExhaustAbsorber( AbsorberNum ).HeatVolFlowRate == AutoSize ) {
				ExhaustAbsorber( AbsorberNum ).HeatVolFlowRateWasAutoSized = true;
			}
			// Assign Curve Numbers
			ExhaustAbsorber( AbsorberNum ).CoolCapFTCurve = GetCurveCheck( cAlphaArgs( 8 ), ErrorsFound, ChillerName );
			ExhaustAbsorber( AbsorberNum ).ThermalEnergyCoolFTCurve = GetCurveCheck( cAlphaArgs( 9 ), ErrorsFound, ChillerName );
			ExhaustAbsorber( AbsorberNum ).ThermalEnergyCoolFPLRCurve = GetCurveCheck( cAlphaArgs( 10 ), ErrorsFound, ChillerName );
			ExhaustAbsorber( AbsorberNum ).ElecCoolFTCurve = GetCurveCheck( cAlphaArgs( 11 ), ErrorsFound, ChillerName );
			ExhaustAbsorber( AbsorberNum ).ElecCoolFPLRCurve = GetCurveCheck( cAlphaArgs( 12 ), ErrorsFound, ChillerName );
			ExhaustAbsorber( AbsorberNum ).HeatCapFCoolCurve = GetCurveCheck( cAlphaArgs( 13 ), ErrorsFound, ChillerName );
			ExhaustAbsorber( AbsorberNum ).ThermalEnergyHeatFHPLRCurve = GetCurveCheck( cAlphaArgs( 14 ), ErrorsFound, ChillerName );
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing curve input for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = false;
			}
			if ( SameString( cAlphaArgs( 15 ), "LeavingCondenser" ) ) {
				ExhaustAbsorber( AbsorberNum ).isEnterCondensTemp = false;
			} else if ( SameString( cAlphaArgs( 15 ), "EnteringCondenser" ) ) {
				ExhaustAbsorber( AbsorberNum ).isEnterCondensTemp = true;
			} else {
				ExhaustAbsorber( AbsorberNum ).isEnterCondensTemp = true;
				ShowWarningError( "Invalid " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "resetting to ENTERING-CONDENSER, simulation continues" );
			}
			// Assign Other Paramters
			if ( SameString( cAlphaArgs( 16 ), "AirCooled" ) ) {
				ExhaustAbsorber( AbsorberNum ).isWaterCooled = false;
			} else if ( SameString( cAlphaArgs( 16 ), "WaterCooled" ) ) {
				ExhaustAbsorber( AbsorberNum ).isWaterCooled = true;
			} else {
				ExhaustAbsorber( AbsorberNum ).isWaterCooled = true;
				ShowWarningError( "Invalid " + cAlphaFieldNames( 16 ) + '=' + cAlphaArgs( 16 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "resetting to WATER-COOLED, simulation continues" );
			}
			if ( ExhaustAbsorber( AbsorberNum ).isWaterCooled ) {
				ExhaustAbsorber( AbsorberNum ).CondReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				ExhaustAbsorber( AbsorberNum ).CondSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser Water Nodes" );
			} else {
				ExhaustAbsorber( AbsorberNum ).CondReturnNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				ExhaustAbsorber( AbsorberNum ).CondSupplyNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				// Connection not required for air or evap cooled condenser so no call to TestCompSet here
				CheckAndAddAirNodeNumber( ExhaustAbsorber( AbsorberNum ).CondReturnNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 4 ) );
				}
			}

			ExhaustAbsorber( AbsorberNum ).CHWLowLimitTemp = rNumericArgs( 15 );
			ExhaustAbsorber( AbsorberNum ).SizFac = rNumericArgs( 16 );
			ExhaustAbsorber( AbsorberNum ).TypeOf = cAlphaArgs( 17 );

			if ( SameString( cAlphaArgs( 17 ), "Generator:MicroTurbine" ) ) {
				ExhaustAbsorber( AbsorberNum ).CompType_Num = iGeneratorMicroturbine;
				ExhaustAbsorber( AbsorberNum ).ExhuastSourceName = cAlphaArgs( 18 );

				GetMTGeneratorExhaustNode( ExhaustAbsorber( AbsorberNum ).CompType_Num, ExhaustAbsorber( AbsorberNum ).ExhuastSourceName, MTExhaustNodeNum );
				ExhaustAbsorber( AbsorberNum ).ExhaustAirInletNodeNum = MTExhaustNodeNum;
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( AbsorberNum = 1; AbsorberNum <= NumExhaustAbsorbers; ++AbsorberNum ) {
			ChillerName = ExhaustAbsorber( AbsorberNum ).Name;

			SetupOutputVariable( "Chiller Heater Evaporator Cooling Rate [W]", ExhaustAbsorberReport( AbsorberNum ).CoolingLoad, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Evaporator Cooling Energy [J]", ExhaustAbsorberReport( AbsorberNum ).CoolingEnergy, "System", "Sum", ChillerName, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Heating Rate [W]", ExhaustAbsorberReport( AbsorberNum ).HeatingLoad, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Energy [J]", ExhaustAbsorberReport( AbsorberNum ).HeatingEnergy, "System", "Sum", ChillerName, _, "ENERGYTRANSFER", "BOILERS", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Condenser Heat Transfer Rate [W]", ExhaustAbsorberReport( AbsorberNum ).TowerLoad, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Condenser Heat Transfer Energy [J]", ExhaustAbsorberReport( AbsorberNum ).TowerEnergy, "System", "Sum", ChillerName, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Cooling Source Heat COP [W/W]", ExhaustAbsorberReport( AbsorberNum ).ThermalEnergyCOP, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Electric Power [W]", ExhaustAbsorberReport( AbsorberNum ).ElectricPower, "System", "Average", ChillerName );
			// Do not include this on meters, this would duplicate the cool electric and heat electric
			SetupOutputVariable( "Chiller Heater Electric Energy [J]", ExhaustAbsorberReport( AbsorberNum ).ElectricEnergy, "System", "Sum", ChillerName );

			SetupOutputVariable( "Chiller Heater Cooling Electric Power [W]", ExhaustAbsorberReport( AbsorberNum ).CoolElectricPower, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Cooling Electric Energy [J]", ExhaustAbsorberReport( AbsorberNum ).CoolElectricEnergy, "System", "Sum", ChillerName, _, "Electricity", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Heating Electric Power [W]", ExhaustAbsorberReport( AbsorberNum ).HeatElectricPower, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Electric Energy [J]", ExhaustAbsorberReport( AbsorberNum ).HeatElectricEnergy, "System", "Sum", ChillerName, _, "Electricity", "Heating", _, "Plant" );

			SetupOutputVariable( "Chiller Heater Evaporator Inlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).ChillReturnTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Evaporator Outlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).ChillSupplyTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Evaporator Mass Flow Rate [kg/s]", ExhaustAbsorberReport( AbsorberNum ).ChillWaterFlowRate, "System", "Average", ChillerName );

			if ( ExhaustAbsorber( AbsorberNum ).isWaterCooled ) {
				SetupOutputVariable( "Chiller Heater Condenser Inlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).CondReturnTemp, "System", "Average", ChillerName );
				SetupOutputVariable( "Chiller Heater Condenser Outlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).CondSupplyTemp, "System", "Average", ChillerName );
				SetupOutputVariable( "Chiller Heater Condenser Mass Flow Rate [kg/s]", ExhaustAbsorberReport( AbsorberNum ).CondWaterFlowRate, "System", "Average", ChillerName );
			} else {
				SetupOutputVariable( "Chiller Heater Condenser Inlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).CondReturnTemp, "System", "Average", ChillerName );
			}

			SetupOutputVariable( "Chiller Heater Heating Inlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).HotWaterReturnTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Outlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).HotWaterSupplyTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Mass Flow Rate [kg/s]", ExhaustAbsorberReport( AbsorberNum ).HotWaterFlowRate, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Cooling Part Load Ratio []", ExhaustAbsorberReport( AbsorberNum ).CoolPartLoadRatio, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Maximum Cooling Rate [W]", ExhaustAbsorberReport( AbsorberNum ).CoolingCapacity, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Part Load Ratio []", ExhaustAbsorberReport( AbsorberNum ).HeatPartLoadRatio, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Maximum Heating Rate [W]", ExhaustAbsorberReport( AbsorberNum ).HeatingCapacity, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Runtime Fraction []", ExhaustAbsorberReport( AbsorberNum ).FractionOfPeriodRunning, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Source Exhaust Inlet Temperature [C]", ExhaustAbsorberReport( AbsorberNum ).ExhaustInTemp, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Source Exhaust Inlet Mass Flow Rate [kg/s]", ExhaustAbsorberReport( AbsorberNum ).ExhaustInFlow, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Heating Heat Recovery Potential Rate [W]", ExhaustAbsorberReport( AbsorberNum ).ExhHeatRecPotentialHeat, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Cooling Heat Recovery Potential Rate [W]", ExhaustAbsorberReport( AbsorberNum ).ExhHeatRecPotentialCool, "System", "Average", ChillerName );

			SetupOutputVariable( "Chiller Heater Cooling Source Heat Transfer Rate [W]", ExhaustAbsorberReport( AbsorberNum ).CoolThermalEnergyUseRate, "System", "Average", ChillerName );
			SetupOutputVariable( "Chiller Heater Heating Source Heat Transfer Rate [W]", ExhaustAbsorberReport( AbsorberNum ).HeatThermalEnergyUseRate, "System", "Average", ChillerName );
		}
	}

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitExhaustAbsorber(
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
		// This subroutine is for initializations of Exhaust Fired absorption chiller
		// components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::TypeOf_Chiller_ExhFiredAbsorption;
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
		static std::string const RoutineName( "InitExhaustAbsorber" );

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
			MyPlantScanFlag.allocate( NumExhaustAbsorbers );
			MyEnvrnFlag.dimension( NumExhaustAbsorbers, true );
			MyOneTimeFlag = false;
			MyPlantScanFlag = true;
		}

		// Init more variables
		if ( MyPlantScanFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( ExhaustAbsorber( ChillNum ).Name, TypeOf_Chiller_ExhFiredAbsorption, ExhaustAbsorber( ChillNum ).CWLoopNum, ExhaustAbsorber( ChillNum ).CWLoopSideNum, ExhaustAbsorber( ChillNum ).CWBranchNum, ExhaustAbsorber( ChillNum ).CWCompNum, ExhaustAbsorber( ChillNum ).CHWLowLimitTemp, _, _,  ExhaustAbsorber( ChillNum ).ChillReturnNodeNum, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitExhaustAbsorber: Program terminated due to previous condition(s)." );
			}

			ScanPlantLoopsForObject( ExhaustAbsorber( ChillNum ).Name, TypeOf_Chiller_ExhFiredAbsorption, ExhaustAbsorber( ChillNum ).HWLoopNum, ExhaustAbsorber( ChillNum ).HWLoopSideNum, ExhaustAbsorber( ChillNum ).HWBranchNum, ExhaustAbsorber( ChillNum ).HWCompNum, _, _, _, ExhaustAbsorber( ChillNum ).HeatReturnNodeNum, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitExhaustAbsorber: Program terminated due to previous condition(s)." );
			}

			if ( ExhaustAbsorber( ChillNum ).isWaterCooled ) {
				ScanPlantLoopsForObject( ExhaustAbsorber( ChillNum ).Name, TypeOf_Chiller_ExhFiredAbsorption, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum, _, _, _, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitExhaustAbsorber: Program terminated due to previous condition(s)." );
				}
				InterConnectTwoPlantLoopSides( ExhaustAbsorber( ChillNum ).CWLoopNum, ExhaustAbsorber( ChillNum ).CWLoopSideNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_ExhFiredAbsorption, true );
				InterConnectTwoPlantLoopSides( ExhaustAbsorber( ChillNum ).HWLoopNum, ExhaustAbsorber( ChillNum ).HWLoopSideNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_ExhFiredAbsorption, true );
			}

			InterConnectTwoPlantLoopSides( ExhaustAbsorber( ChillNum ).CWLoopNum, ExhaustAbsorber( ChillNum ).CWLoopSideNum, ExhaustAbsorber( ChillNum ).HWLoopNum, ExhaustAbsorber( ChillNum ).HWLoopSideNum, TypeOf_Chiller_ExhFiredAbsorption, true );

			// check if outlet node of chilled water side has a setpoint.
			if ( ( Node( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					if ( ! ExhaustAbsorber( ChillNum ).ChillSetPointErrDone ) {
						ShowWarningError( "Missing temperature setpoint on cool side for chiller heater named " + ExhaustAbsorber( ChillNum ).Name );
						ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager" );
						ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
						ExhaustAbsorber( ChillNum ).ChillSetPointErrDone = true;
					}
				} else {
					// need call to EMS to check node
					errFlag = false; // but not really fatal yet, but should be.
					CheckIfNodeSetPointManagedByEMS( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum, iTemperatureSetPoint, errFlag );
					if ( errFlag ) {
						if ( ! ExhaustAbsorber( ChillNum ).ChillSetPointErrDone ) {
							ShowWarningError( "Missing temperature setpoint on cool side for chiller heater named " + ExhaustAbsorber( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller evaporator " );
							ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
							ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							ExhaustAbsorber( ChillNum ).ChillSetPointErrDone = true;

						}
					}

				}
				ExhaustAbsorber( ChillNum ).ChillSetPointSetToLoop = true;
				Node( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPoint = Node( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
				Node( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPointHi = Node( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
			// check if outlet node of hot water side has a setpoint.
			if ( ( Node( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPointLo == SensedNodeFlagValue ) ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					if ( ! ExhaustAbsorber( ChillNum ).HeatSetPointErrDone ) {
						ShowWarningError( "Missing temperature setpoint on heat side for chiller heater named " + ExhaustAbsorber( ChillNum ).Name );
						ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager" );
						ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
						ExhaustAbsorber( ChillNum ).HeatSetPointErrDone = true;
					}
				} else {
					// need call to EMS to check node
					errFlag = false; // but not really fatal yet, but should be.
					CheckIfNodeSetPointManagedByEMS( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum, iTemperatureSetPoint, errFlag );
					if ( errFlag ) {
						if ( ! ExhaustAbsorber( ChillNum ).HeatSetPointErrDone ) {
							ShowWarningError( "Missing temperature setpoint on heat side for chiller heater named " + ExhaustAbsorber( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of this chiller heater " );
							ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the heater side outlet node " );
							ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
							ShowContinueError( "  The overall loop setpoint will be assumed for heater side. The simulation continues ... " );
							ExhaustAbsorber( ChillNum ).HeatSetPointErrDone = true;
						}
					}
				}
				ExhaustAbsorber( ChillNum ).HeatSetPointSetToLoop = true;
				Node( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPoint = Node( PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
				Node( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPointLo = Node( PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPointLo;
			}
			MyPlantScanFlag( ChillNum ) = false;
		}

		CondInletNode = ExhaustAbsorber( ChillNum ).CondReturnNodeNum;
		CondOutletNode = ExhaustAbsorber( ChillNum ).CondSupplyNodeNum;
		HeatInletNode = ExhaustAbsorber( ChillNum ).HeatReturnNodeNum;
		HeatOutletNode = ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum;

		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			if ( ExhaustAbsorber( ChillNum ).isWaterCooled ) {
				// init max available condenser water flow rate
				if ( ExhaustAbsorber( ChillNum ).CDLoopNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				} else {
					rho = RhoH2O( InitConvTemp );

				}

				ExhaustAbsorber( ChillNum ).DesCondMassFlowRate = rho * ExhaustAbsorber( ChillNum ).CondVolFlowRate;
				InitComponentNodes( 0.0, ExhaustAbsorber( ChillNum ).DesCondMassFlowRate, CondInletNode, CondOutletNode, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum );
			}

			if ( ExhaustAbsorber( ChillNum ).HWLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).FluidName, InitConvTemp, PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = RhoH2O( InitConvTemp );
			}
			ExhaustAbsorber( ChillNum ).DesHeatMassFlowRate = rho * ExhaustAbsorber( ChillNum ).HeatVolFlowRate;
			//init available hot water flow rate
			InitComponentNodes( 0.0, ExhaustAbsorber( ChillNum ).DesHeatMassFlowRate, HeatInletNode, HeatOutletNode, ExhaustAbsorber( ChillNum ).HWLoopNum, ExhaustAbsorber( ChillNum ).HWLoopSideNum, ExhaustAbsorber( ChillNum ).HWBranchNum, ExhaustAbsorber( ChillNum ).HWCompNum );

			if ( ExhaustAbsorber( ChillNum ).CWLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = RhoH2O( InitConvTemp );
			}
			ExhaustAbsorber( ChillNum ).DesEvapMassFlowRate = rho * ExhaustAbsorber( ChillNum ).EvapVolFlowRate;
			//init available hot water flow rate
			InitComponentNodes( 0.0, ExhaustAbsorber( ChillNum ).DesEvapMassFlowRate, ExhaustAbsorber( ChillNum ).ChillReturnNodeNum, ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum, ExhaustAbsorber( ChillNum ).CWLoopNum, ExhaustAbsorber( ChillNum ).CWLoopSideNum, ExhaustAbsorber( ChillNum ).CWBranchNum, ExhaustAbsorber( ChillNum ).CWCompNum );

			MyEnvrnFlag( ChillNum ) = false;

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		//this component model works off setpoints on the leaving node
		// fill from plant if needed
		if ( ExhaustAbsorber( ChillNum ).ChillSetPointSetToLoop ) {
			Node( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPoint = Node( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum ).TempSetPointHi = Node( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ExhaustAbsorber( ChillNum ).HeatSetPointSetToLoop ) {
			Node( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPoint = Node( PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum ).TempSetPointLo = Node( PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).TempSetPointNodeNum ).TempSetPointLo;
		}

		if ( ( ExhaustAbsorber( ChillNum ).isWaterCooled ) && ( ( ExhaustAbsorber( ChillNum ).InHeatingMode ) || ( ExhaustAbsorber( ChillNum ).InCoolingMode ) ) && ( ! MyPlantScanFlag( ChillNum ) ) ) {
			mdot = ExhaustAbsorber( ChillNum ).DesCondMassFlowRate;

			SetComponentFlowRate( mdot, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, ExhaustAbsorber( ChillNum ).CondSupplyNodeNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum );

		} else {
			mdot = 0.0;
			SetComponentFlowRate( mdot, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, ExhaustAbsorber( ChillNum ).CondSupplyNodeNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum );
		}

	}

	void
	SizeExhaustAbsorber( int const ChillNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2003
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Exhaust Fired absorption chiller components for which
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
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeExhaustAbsorber" );

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
		tmpNomCap = ExhaustAbsorber( ChillNum ).NomCoolingCap;
		tmpEvapVolFlowRate = ExhaustAbsorber( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = ExhaustAbsorber( ChillNum ).CondVolFlowRate;
		tmpHeatRecVolFlowRate = ExhaustAbsorber( ChillNum ).HeatVolFlowRate;
		NomCapUser = 0.0;
		EvapVolFlowRateUser = 0.0;
		CondVolFlowRateUser = 0.0;
		HeatRecVolFlowRateUser = 0.0;

		if ( ExhaustAbsorber( ChillNum ).isWaterCooled ) PltSizCondNum = PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).PlantSizNum;

		PltSizHeatNum = PlantLoop( ExhaustAbsorber( ChillNum ).HWLoopNum ).PlantSizNum;
		PltSizCoolNum = PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizCoolNum > 0 ) {
			if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				Cp = GetSpecificHeatGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				rho = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizCoolNum ).DeltaT * PlantSizData( PltSizCoolNum ).DesVolFlowRate * ExhaustAbsorber( ChillNum ).SizFac;
				if ( ! ExhaustAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) tmpNomCap = ExhaustAbsorber( ChillNum ).NomCoolingCap;
			} else {
				if ( ExhaustAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ExhaustAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) {
					ExhaustAbsorber( ChillNum ).NomCoolingCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Design Size Nominal Cooling Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Initial Design Size Nominal Cooling Capacity [W]", tmpNomCap );
					}
				} else {
					if ( ExhaustAbsorber( ChillNum ).NomCoolingCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = ExhaustAbsorber( ChillNum ).NomCoolingCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
								"Design Size Nominal Cooling Capacity [W]", tmpNomCap,
								"User-Specified Nominal Cooling Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerHeaterAbsorptionDoubleEffect: Potential issue with equipment sizing for " + ExhaustAbsorber( ChillNum ).Name );
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
			if ( ExhaustAbsorber( ChillNum ).NomCoolingCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + ExhaustAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Exhaust Fired Absorption Chiller nominal cooling capacity requires" );
					ShowContinueError( "a cooling loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( ExhaustAbsorber( ChillNum ).NomCoolingCap > 0.0 ) {
						ReportSizingOutput( "Chiller:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"User-Specified Nominal Capacity [W]", ExhaustAbsorber( ChillNum ).NomCoolingCap );
					}
				}
			}
		}

		if ( PltSizCoolNum > 0 ) {
			if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizCoolNum ).DesVolFlowRate * ExhaustAbsorber( ChillNum ).SizFac;
				if ( ! ExhaustAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = ExhaustAbsorber( ChillNum ).EvapVolFlowRate;

			} else {
				if ( ExhaustAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ExhaustAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					ExhaustAbsorber( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( ExhaustAbsorber( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = ExhaustAbsorber( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate, "User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorptionDoubleEffect: Potential issue with equipment sizing for " + ExhaustAbsorber( ChillNum ).Name );
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
			if ( ExhaustAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + ExhaustAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Exhaust Fired Absorption Chiller evap flow rate requires" );
					ShowContinueError( "a cooling loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( ExhaustAbsorber( ChillNum ).EvapVolFlowRate > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"User-Specified Design Chilled Water Flow Rate [m3/s]", ExhaustAbsorber( ChillNum ).EvapVolFlowRate );
					}
				}
			}
		}

		RegisterPlantCompDesignFlow( ExhaustAbsorber( ChillNum ).ChillReturnNodeNum, tmpEvapVolFlowRate );

		if ( PltSizHeatNum > 0 ) {
			if ( PlantSizData( PltSizHeatNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpHeatRecVolFlowRate = PlantSizData( PltSizHeatNum ).DesVolFlowRate * ExhaustAbsorber( ChillNum ).SizFac;
				if ( ! ExhaustAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = ExhaustAbsorber( ChillNum ).HeatVolFlowRate;

			} else {
				if ( ExhaustAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ExhaustAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) {
					ExhaustAbsorber( ChillNum ).HeatVolFlowRate = tmpHeatRecVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Design Size Design Hot Water Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Initial Design Size Design Hot Water Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
				} else {
					if ( ExhaustAbsorber( ChillNum ).HeatVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0 ) {
						HeatRecVolFlowRateUser = ExhaustAbsorber( ChillNum ).HeatVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
								"Design Size Design Hot Water Flow Rate [m3/s]", tmpHeatRecVolFlowRate,
								"User-Specified Design Hot Water Flow Rate [m3/s]", HeatRecVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpHeatRecVolFlowRate - HeatRecVolFlowRateUser ) / HeatRecVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerHeaterAbsorptionDoubleEffect: Potential issue with equipment sizing for " + ExhaustAbsorber( ChillNum ).Name );
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
			if ( ExhaustAbsorber( ChillNum ).HeatVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + ExhaustAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowContinueError( "Autosizing of Exhaust Fired Absorption Chiller hot water flow rate requires" );
					ShowContinueError( "a heating loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( ExhaustAbsorber( ChillNum ).HeatVolFlowRate > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"User-Specified Design Hot Water Flow Rate [m3/s]", ExhaustAbsorber( ChillNum ).HeatVolFlowRate );
					}
				}
			}
		}

		RegisterPlantCompDesignFlow( ExhaustAbsorber( ChillNum ).HeatReturnNodeNum, tmpHeatRecVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizCoolNum > 0 ) {
			if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {

				Cp = GetSpecificHeatGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidName, ExhaustAbsorber( ChillNum ).TempDesCondReturn, PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				rho = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidName, ExhaustAbsorber( ChillNum ).TempDesCondReturn, PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + ExhaustAbsorber( ChillNum ).ThermalEnergyCoolRatio ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! ExhaustAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = ExhaustAbsorber( ChillNum ).CondVolFlowRate;

			} else {
				if ( ExhaustAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ExhaustAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
					ExhaustAbsorber( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( ExhaustAbsorber( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = ExhaustAbsorber( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorptionDoubleEffect: Potential issue with equipment sizing for " + ExhaustAbsorber( ChillNum ).Name );
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
			if ( ExhaustAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + ExhaustAbsorber( ChillNum ).Name + "\", autosize error." );
					ShowSevereError( "Autosizing of Exhaust Fired Absorption Chiller condenser flow rate requires a condenser" );
					ShowContinueError( "loop Sizing:Plant object." );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( ExhaustAbsorber( ChillNum ).CondVolFlowRate > 0.0 ) {
						ReportSizingOutput( "ChillerHeater:Absorption:DoubleEffect", ExhaustAbsorber( ChillNum ).Name,
							"User-Specified Design Condenser Water Flow Rate [m3/s]", ExhaustAbsorber( ChillNum ).CondVolFlowRate );
					}
				}
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( ExhaustAbsorber( ChillNum ).isWaterCooled ) RegisterPlantCompDesignFlow( ExhaustAbsorber( ChillNum ).CondReturnNodeNum, tmpCondVolFlowRate );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = ExhaustAbsorber( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "ChillerHeater:Absorption:DoubleEffect" );
			PreDefTableEntry( pdchMechNomEff, equipName, ExhaustAbsorber( ChillNum ).ThermalEnergyCoolRatio );
			PreDefTableEntry( pdchMechNomCap, equipName, ExhaustAbsorber( ChillNum ).NomCoolingCap );
		}

	}

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcExhaustAbsorberChillerModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const EP_UNUSED( RunFlag ) // TRUE when Absorber operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001
		//       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate exhaust fired chiller
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a Exhaust fired (Exhaust consuming) absorption chiller using
		// curves and inputs similar to DOE-2.1e

		// METHODOLOGY EMPLOYED:
		// Curve fit of performance data

		// REFERENCES:
		// 1.  DOE-2.1e Supplement and source code
		// 2.  CoolTools GasMod work

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataPlant::DeltaTempTol;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;
		using MicroturbineElectricGenerator::SimMTGenerator;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FlowLock = 0  if mass flow rates may be changed by loop components
		// FlowLock = 1  if mass flow rates may not be changed by loop components

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const AbsLeavingTemp( 176.667 ); // C - Minimum temperature leaving the Chiller absorber (350 F)
		static std::string const RoutineName( "CalcExhaustAbsorberChillerModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Local copies of ExhaustAbsorberSpecs Type
		// all variables that are local copies of data structure
		// variables are prefaced with an "l" for local.
		Real64 lNomCoolingCap; // W - design nominal capacity of Absorber
		Real64 lThermalEnergyCoolRatio; // ratio of ThermalEnergy input to cooling output
		Real64 lThermalEnergyHeatRatio; // ratio of ThermalEnergy input to heating output
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
		int lThermalEnergyCoolFTCurve; // ThermalEnergy-Input-to cooling output Ratio Function of Temperature Curve
		int lThermalEnergyCoolFPLRCurve; // ThermalEnergy-Input-to cooling output Ratio Function of Part Load Ratio Curve
		int lElecCoolFTCurve; // Electric-Input-to cooling output Ratio Function of Temperature Curve
		int lElecCoolFPLRCurve; // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
		bool lIsEnterCondensTemp; // if using entering conderser water temperature is TRUE, exiting is FALSE
		bool lIsWaterCooled; // if water cooled it is TRUE
		Real64 lCHWLowLimitTemp; // Chilled Water Lower Limit Temperature
		int lExhaustAirInletNodeNum; // Combustion Air Inlet Node number
		// Local copies of ExhaustAbsorberReportVars Type
		Real64 lCoolingLoad( 0.0 ); // cooling load on the chiller (previously called QEvap)
		// Real64 lCoolingEnergy( 0.0 ); // variable to track total cooling load for period (was EvapEnergy)
		Real64 lTowerLoad( 0.0 ); // load on the cooling tower/condenser (previously called QCond)
		// Real64 lTowerEnergy( 0.0 ); // variable to track total tower load for a period (was CondEnergy)
		// Real64 lThermalEnergyUseRate( 0.0 ); // instantaneous use of exhaust for period
		// Real64 lThermalEnergy( 0.0 ); // variable to track total ThermalEnergy used for a period
		Real64 lCoolThermalEnergyUseRate( 0.0 ); // instantaneous use of exhaust for period for cooling
		// Real64 lCoolThermalEnergy( 0.0 ); // variable to track total ThermalEnergy used for a period for cooling
		Real64 lHeatThermalEnergyUseRate( 0.0 ); // instantaneous use of exhaust for period for heating
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
		Real64 lChillWaterMassflowratemax( 0.0 ); // Maximum flow rate through the evaporator
		Real64 lExhaustInTemp( 0.0 ); // Exhaust inlet temperature
		Real64 lExhaustInFlow( 0.0 ); // Exhaust inlet flow rate
		Real64 lExhHeatRecPotentialCool( 0.0 ); // Exhaust heat recovery potential during cooling
		Real64 lExhaustAirHumRat( 0.0 );
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
		Real64 CpAir; // specific heat of exhaust air

		// define constant values

		// set node values to data structure values for nodes

		lChillReturnNodeNum = ExhaustAbsorber( ChillNum ).ChillReturnNodeNum;
		lChillSupplyNodeNum = ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum;
		lCondReturnNodeNum = ExhaustAbsorber( ChillNum ).CondReturnNodeNum;
		lCondSupplyNodeNum = ExhaustAbsorber( ChillNum ).CondSupplyNodeNum;
		lExhaustAirInletNodeNum = ExhaustAbsorber( ChillNum ).ExhaustAirInletNodeNum;

		// set local copies of data from rest of input structure
		lNomCoolingCap = ExhaustAbsorber( ChillNum ).NomCoolingCap;
		lThermalEnergyCoolRatio = ExhaustAbsorber( ChillNum ).ThermalEnergyCoolRatio;
		lThermalEnergyHeatRatio = ExhaustAbsorber( ChillNum ).ThermalEnergyHeatRatio;
		lElecCoolRatio = ExhaustAbsorber( ChillNum ).ElecCoolRatio;
		lMinPartLoadRat = ExhaustAbsorber( ChillNum ).MinPartLoadRat;
		lMaxPartLoadRat = ExhaustAbsorber( ChillNum ).MaxPartLoadRat;
		lOptPartLoadRat = ExhaustAbsorber( ChillNum ).OptPartLoadRat;
		lTempDesCondReturn = ExhaustAbsorber( ChillNum ).TempDesCondReturn;
		lTempDesCHWSupply = ExhaustAbsorber( ChillNum ).TempDesCHWSupply;
		lCondVolFlowRate = ExhaustAbsorber( ChillNum ).CondVolFlowRate;
		lCoolCapFTCurve = ExhaustAbsorber( ChillNum ).CoolCapFTCurve;
		lThermalEnergyCoolFTCurve = ExhaustAbsorber( ChillNum ).ThermalEnergyCoolFTCurve;
		lThermalEnergyCoolFPLRCurve = ExhaustAbsorber( ChillNum ).ThermalEnergyCoolFPLRCurve;
		lElecCoolFTCurve = ExhaustAbsorber( ChillNum ).ElecCoolFTCurve;
		lElecCoolFPLRCurve = ExhaustAbsorber( ChillNum ).ElecCoolFPLRCurve;
		lIsEnterCondensTemp = ExhaustAbsorber( ChillNum ).isEnterCondensTemp;
		lIsWaterCooled = ExhaustAbsorber( ChillNum ).isWaterCooled;
		lCHWLowLimitTemp = ExhaustAbsorber( ChillNum ).CHWLowLimitTemp;
		lHeatElectricPower = ExhaustAbsorberReport( ChillNum ).HeatElectricPower;
		lHeatThermalEnergyUseRate = ExhaustAbsorberReport( ChillNum ).HeatThermalEnergyUseRate;
		lHeatPartLoadRatio = ExhaustAbsorberReport( ChillNum ).HeatPartLoadRatio;

		// initialize entering conditions
		lChillReturnTemp = Node( lChillReturnNodeNum ).Temp;
		lChillWaterMassFlowRate = Node( lChillReturnNodeNum ).MassFlowRate;
		lCondReturnTemp = Node( lCondReturnNodeNum ).Temp;
		lCondWaterMassFlowRate = Node( lCondReturnNodeNum ).MassFlowRate;
		{ auto const SELECT_CASE_var( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			ChillSupplySetPointTemp = Node( lChillSupplyNodeNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			ChillSupplySetPointTemp = Node( lChillSupplyNodeNum ).TempSetPointHi;
		} else {
			assert( false );
		}}
		ChillDeltaTemp = std::abs( lChillReturnTemp - ChillSupplySetPointTemp );
		lExhaustInTemp = Node( lExhaustAirInletNodeNum ).Temp;
		lExhaustInFlow = Node( lExhaustAirInletNodeNum ).MassFlowRate;
		lExhaustAirHumRat = Node( lExhaustAirInletNodeNum ).HumRat;

		rhoCW = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidName, lChillReturnTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
		Cp_CW = GetSpecificHeatGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidName, lChillReturnTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
		rhoCD = GetDensityGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidName, lChillReturnTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
		Cp_CD = GetSpecificHeatGlycol( PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidName, lChillReturnTemp, PlantLoop( ExhaustAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

		//If no loop demand or Absorber OFF, return
		// will need to modify when absorber can act as a boiler
		if ( MyLoad >= 0 || ! ( ( ExhaustAbsorber( ChillNum ).InHeatingMode ) || ( ExhaustAbsorber( ChillNum ).InCoolingMode ) ) ) {
			//set node temperatures
			lChillSupplyTemp = lChillReturnTemp;
			lCondSupplyTemp = lCondReturnTemp;
			lCondWaterMassFlowRate = 0.0;
			if ( lIsWaterCooled ) {
				SetComponentFlowRate( lCondWaterMassFlowRate, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, ExhaustAbsorber( ChillNum ).CondSupplyNodeNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum );
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
				lCondWaterMassFlowRate = ExhaustAbsorber( ChillNum ).DesCondMassFlowRate;
				SetComponentFlowRate( lCondWaterMassFlowRate, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, ExhaustAbsorber( ChillNum ).CondSupplyNodeNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum );
			} else {
				// air cooled
				Node( lCondReturnNodeNum ).Temp = Node( lCondReturnNodeNum ).OutAirDryBulb;
				lCondReturnTemp = Node( lCondReturnNodeNum ).Temp;
				lCondWaterMassFlowRate = 0.0;
				SetComponentFlowRate( lCondWaterMassFlowRate, ExhaustAbsorber( ChillNum ).CondReturnNodeNum, ExhaustAbsorber( ChillNum ).CondSupplyNodeNum, ExhaustAbsorber( ChillNum ).CDLoopNum, ExhaustAbsorber( ChillNum ).CDLoopSideNum, ExhaustAbsorber( ChillNum ).CDBranchNum, ExhaustAbsorber( ChillNum ).CDCompNum );
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
			lChillWaterMassflowratemax = ExhaustAbsorber( ChillNum ).DesEvapMassFlowRate;

			LoopNum = ExhaustAbsorber( ChillNum ).CWLoopNum;
			LoopSideNum = ExhaustAbsorber( ChillNum ).CWLoopSideNum;
			{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock );
			if ( SELECT_CASE_var == 0 ) { // mass flow rates may be changed by loop components
				ExhaustAbsorber( ChillNum ).PossibleSubcooling = false;
				lCoolingLoad = std::abs( MyLoad );
				if ( ChillDeltaTemp != 0.0 ) {
					lChillWaterMassFlowRate = std::abs( lCoolingLoad / ( Cp_CW * ChillDeltaTemp ) );
					if ( lChillWaterMassFlowRate - lChillWaterMassflowratemax > MassFlowTolerance ) ExhaustAbsorber( ChillNum ).PossibleSubcooling = true;

					SetComponentFlowRate( lChillWaterMassFlowRate, ExhaustAbsorber( ChillNum ).ChillReturnNodeNum, ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum, ExhaustAbsorber( ChillNum ).CWLoopNum, ExhaustAbsorber( ChillNum ).CWLoopSideNum, ExhaustAbsorber( ChillNum ).CWBranchNum, ExhaustAbsorber( ChillNum ).CWCompNum );
					{ auto const SELECT_CASE_var1( PlantLoop( ExhaustAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var1 == SingleSetPoint ) {
						lChillSupplyTemp = Node( lChillSupplyNodeNum ).TempSetPoint;
					} else if ( SELECT_CASE_var1 == DualSetPointDeadBand ) {
						lChillSupplyTemp = Node( lChillSupplyNodeNum ).TempSetPointHi;
					}}
				} else {
					lChillWaterMassFlowRate = 0.0;
					ShowRecurringWarningErrorAtEnd( "ExhaustAbsorberChillerModel:Cooling\"" + ExhaustAbsorber( ChillNum ).Name + "\", DeltaTemp = 0 in mass flow calculation", ExhaustAbsorber( ChillNum ).DeltaTempCoolErrCount );
				}
				lChillSupplyTemp = ChillSupplySetPointTemp;
			} else if ( SELECT_CASE_var == 1 ) { // mass flow rates may not be changed by loop components
				lChillWaterMassFlowRate = Node( lChillReturnNodeNum ).MassFlowRate;
				if ( ExhaustAbsorber( ChillNum ).PossibleSubcooling ) {
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

			//Calculate thermal energy consumption for cooling
			// Thermal Energy used for cooling availCap * TeFIR * TeFIR-FT * TeFIR-FPLR
			lCoolThermalEnergyUseRate = lAvailableCoolingCapacity * lThermalEnergyCoolRatio * CurveValue( lThermalEnergyCoolFTCurve, lChillSupplyTemp, calcCondTemp ) * CurveValue( lThermalEnergyCoolFPLRCurve, lCoolPartLoadRatio ) * lFractionOfPeriodRunning;

			//Calculate electric parasitics used
			// based on nominal capacity, not available capacity,
			// electric used for cooling nomCap * %OP * EIR * EIR-FT * EIR-FPLR
			lCoolElectricPower = lNomCoolingCap * lElecCoolRatio * lFractionOfPeriodRunning * CurveValue( lElecCoolFTCurve, lChillSupplyTemp, calcCondTemp ) * CurveValue( lElecCoolFPLRCurve, lCoolPartLoadRatio );

			// determine conderser load which is cooling load plus the
			// ThermalEnergy used for cooling plus
			// the electricity used
			lTowerLoad = lCoolingLoad + lCoolThermalEnergyUseRate / lThermalEnergyHeatRatio + lCoolElectricPower;

			lExhaustInTemp = Node( lExhaustAirInletNodeNum ).Temp;
			lExhaustInFlow = Node( lExhaustAirInletNodeNum ).MassFlowRate;
			CpAir = PsyCpAirFnWTdb( lExhaustAirHumRat, lExhaustInTemp );
			lExhHeatRecPotentialCool = lExhaustInFlow * CpAir * ( lExhaustInTemp - AbsLeavingTemp );
			// If Microturbine Exhaust temperature and flow rate is not sufficient to run the chiller, then chiller will not run
			// lCoolThermalEnergyUseRate , lTowerLoad and  lCoolElectricPower will be set to 0.0

			if ( lExhHeatRecPotentialCool < lCoolThermalEnergyUseRate ) {
				if ( ExhaustAbsorber( ChillNum ).ExhTempLTAbsLeavingTempIndex == 0 ) {
					ShowWarningError( "ChillerHeater:Absorption:DoubleEffect \"" + ExhaustAbsorber( ChillNum ).Name + "\"" );
					ShowContinueError( "...Exhaust temperature and flow input from Micro Turbine is not sufficient during cooling to run the chiller " );
					ShowContinueError( "...Value of Exhaust air inlet temp =" + TrimSigDigits( lExhaustInTemp, 4 ) + " C." );
					ShowContinueError( "... and Exhaust air flow rate of " + TrimSigDigits( lExhaustInFlow, 2 ) + " kg/s." );
					ShowContinueError( "...Value of minimum absorber leaving temp =" + TrimSigDigits( AbsLeavingTemp, 4 ) + " C." );
					ShowContinueError( "...Either increase the Exhaust temperature (min required = 350 C )  or flow or both of Micro Turbine to meet the min available potential criteria." );
					ShowContinueErrorTimeStamp( "... Simulation will continue." );
				}
				ShowRecurringWarningErrorAtEnd( "ChillerHeater:Absorption:DoubleEffect \"" + ExhaustAbsorber( ChillNum ).Name + "\": Exhaust temperature from Micro Turbine is not sufficient to run the chiller during cooling warning continues...", ExhaustAbsorber( ChillNum ).ExhTempLTAbsLeavingTempIndex, lExhaustInTemp, AbsLeavingTemp );
				// If exhaust is not available, it means the avilable thermal energy is 0.0 and Chiller is not available
				lCoolThermalEnergyUseRate = 0.0;
				lTowerLoad = 0.0;
				lCoolElectricPower = 0.0;
				lChillSupplyTemp = lChillReturnTemp;
				lCondSupplyTemp = lCondReturnTemp;
				ChillDeltaTemp = 0.0;
				lFractionOfPeriodRunning = min( 1.0, max( lHeatPartLoadRatio, lCoolPartLoadRatio ) / lMinPartLoadRat );
			}
			// for water cooled condenser make sure enough flow rate
			// for air cooled condenser just set supply to return temperature
			if ( lIsWaterCooled ) {
				if ( lCondWaterMassFlowRate > MassFlowTolerance ) {
					lCondSupplyTemp = lCondReturnTemp + lTowerLoad / ( lCondWaterMassFlowRate * Cp_CD );
				} else {
					ShowSevereError( "CalcExhaustAbsorberChillerModel: Condenser flow = 0, for Exhaust Absorber Chiller=" + ExhaustAbsorber( ChillNum ).Name );
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
						ShowRecurringWarningErrorAtEnd( "ExhaustAbsorberChillerModel:\"" + ExhaustAbsorber( ChillNum ).Name + "\", poor Condenser Supply Estimate", ExhaustAbsorber( ChillNum ).CondErrCount, errorAvailCap, errorAvailCap );
					}
				}
			}
		} // IF(MyLoad>=0 .OR. .NOT. RunFlag)
		// Write into the Report Variables except for nodes
		ExhaustAbsorberReport( ChillNum ).CoolingLoad = lCoolingLoad;
		ExhaustAbsorberReport( ChillNum ).TowerLoad = lTowerLoad;
		ExhaustAbsorberReport( ChillNum ).CoolThermalEnergyUseRate = lCoolThermalEnergyUseRate;
		ExhaustAbsorberReport( ChillNum ).CoolElectricPower = lCoolElectricPower;
		ExhaustAbsorberReport( ChillNum ).CondReturnTemp = lCondReturnTemp;
		ExhaustAbsorberReport( ChillNum ).ChillReturnTemp = lChillReturnTemp;
		ExhaustAbsorberReport( ChillNum ).CondSupplyTemp = lCondSupplyTemp;
		ExhaustAbsorberReport( ChillNum ).ChillSupplyTemp = lChillSupplyTemp;
		ExhaustAbsorberReport( ChillNum ).ChillWaterFlowRate = lChillWaterMassFlowRate;
		ExhaustAbsorberReport( ChillNum ).CondWaterFlowRate = lCondWaterMassFlowRate;
		ExhaustAbsorberReport( ChillNum ).CoolPartLoadRatio = lCoolPartLoadRatio;
		ExhaustAbsorberReport( ChillNum ).CoolingCapacity = lAvailableCoolingCapacity;
		ExhaustAbsorberReport( ChillNum ).FractionOfPeriodRunning = lFractionOfPeriodRunning;
		ExhaustAbsorberReport( ChillNum ).ExhaustInTemp = lExhaustInTemp;
		ExhaustAbsorberReport( ChillNum ).ExhaustInFlow = lExhaustInFlow;
		ExhaustAbsorberReport( ChillNum ).ExhHeatRecPotentialCool = lExhHeatRecPotentialCool;

		// write the combined heating and cooling ThermalEnergy used and electric used
		ExhaustAbsorberReport( ChillNum ).ThermalEnergyUseRate = lCoolThermalEnergyUseRate + lHeatThermalEnergyUseRate;
		ExhaustAbsorberReport( ChillNum ).ElectricPower = lCoolElectricPower + lHeatElectricPower;

	}

	void
	CalcExhaustAbsorberHeaterModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const RunFlag // TRUE when Absorber operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer and Michael J. Witte
		//       DATE WRITTEN   March 2001
		//       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate exhaust fired double effect absorption chiller
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a Exhaust fired (Exhaust consuming) absorption chiller using
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
		using Psychrometrics::PsyCpAirFnWTdb;
		using MicroturbineElectricGenerator::SimMTGenerator;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// FlowLock = 0  if mass flow rates may be changed by loop components
		// FlowLock = 1  if mass flow rates may not be changed by loop components
		// FlowLock = 2  if overloaded and mass flow rates has changed to a small amount and Tout drops
		//                 below Setpoint

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const AbsLeavingTemp( 176.667 ); // C - Minimum temperature leaving the Chiller absorber (350 F)
		static std::string const RoutineName( "CalcExhaustAbsorberHeaterModel" );
		//INTEGER    :: ExhTempLTAbsLeavingTempCount      = 0        ! Counter for exhaust temp < absorber leaving air temp warning messages
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Local copies of ExhaustAbsorberSpecs Type
		// all variables that are local copies of data structure
		// variables are prefaced with an "l" for local.
		Real64 lNomCoolingCap; // W - design nominal capacity of Absorber
		Real64 lNomHeatCoolRatio; // ratio of heating to cooling capacity
		Real64 lThermalEnergyHeatRatio; // ratio of ThermalEnergy input to heating output
		Real64 lElecHeatRatio; // ratio of electricity input to heating output
		int lHeatReturnNodeNum; // absorber hot water inlet node number, water side
		int lHeatSupplyNodeNum; // absorber hot water outlet node number, water side
		Real64 lMinPartLoadRat; // min allowed operating frac full load
		Real64 lMaxPartLoadRat; // max allowed operating frac full load
		Real64 lOptPartLoadRat; // optimal operating frac full load
		int lHeatCapFCoolCurve; // Heating Capacity Function of Cooling Capacity Curve
		int lThermalEnergyHeatFHPLRCurve; // ThermalEnergy Input to heat output ratio during heating only function
		// Local copies of ExhaustAbsorberReportVars Type
		Real64 lHeatingLoad( 0.0 ); // heating load on the chiller
		// Real64 lHeatingEnergy( 0.0 ); // heating energy
		// Real64 lThermalEnergyUseRate( 0.0 ); // instantaneous use of Thermal Energy for period
		// Real64 lThermalEnergy( 0.0 ); // variable to track total Thermal Energy used for a period (reference only)
		Real64 lCoolThermalEnergyUseRate( 0.0 ); // instantaneous use of thermal energy for period for cooling
		Real64 lHeatThermalEnergyUseRate( 0.0 ); // instantaneous use of thermal energy for period for heating
		// Real64 lHeatThermalEnergy( 0.0 ); // variable to track total ThermalEnergy used for a period for heating
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
		Real64 lExhaustInTemp( 0.0 ); // Exhaust inlet temperature
		Real64 lExhaustInFlow( 0.0 ); // Exhaust inlet flow rate
		Real64 lExhHeatRecPotentialHeat( 0.0 ); // Exhaust heat recovery potential
		Real64 lExhaustAirHumRat( 0.0 );
		// other local variables
		Real64 HeatDeltaTemp( 0.0 ); // hot water temperature difference
		Real64 HeatSupplySetPointTemp( 0.0 );
		int LoopNum;
		int LoopSideNum;
		Real64 Cp_HW; // local fluid specific heat for hot water
		Real64 CpAir;
		Real64 rhoHW; // local fluid density for hot water
		int lExhaustAirInletNodeNum; // Combustion Air Inlet Node number

		// set node values to data structure values for nodes

		lHeatReturnNodeNum = ExhaustAbsorber( ChillNum ).HeatReturnNodeNum;
		lHeatSupplyNodeNum = ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum;
		lExhaustAirInletNodeNum = ExhaustAbsorber( ChillNum ).ExhaustAirInletNodeNum;

		// set local copies of data from rest of input structure

		lNomCoolingCap = ExhaustAbsorber( ChillNum ).NomCoolingCap;
		lNomHeatCoolRatio = ExhaustAbsorber( ChillNum ).NomHeatCoolRatio;
		lThermalEnergyHeatRatio = ExhaustAbsorber( ChillNum ).ThermalEnergyHeatRatio;
		lElecHeatRatio = ExhaustAbsorber( ChillNum ).ElecHeatRatio;
		lMinPartLoadRat = ExhaustAbsorber( ChillNum ).MinPartLoadRat;
		lMaxPartLoadRat = ExhaustAbsorber( ChillNum ).MaxPartLoadRat;
		lOptPartLoadRat = ExhaustAbsorber( ChillNum ).OptPartLoadRat;
		lHeatCapFCoolCurve = ExhaustAbsorber( ChillNum ).HeatCapFCoolCurve;
		lThermalEnergyHeatFHPLRCurve = ExhaustAbsorber( ChillNum ).ThermalEnergyHeatFHPLRCurve;
		lHotWaterMassFlowRateMax = ExhaustAbsorber( ChillNum ).DesHeatMassFlowRate;
		LoopNum = ExhaustAbsorber( ChillNum ).HWLoopNum;
		LoopSideNum = ExhaustAbsorber( ChillNum ).HWLoopSideNum;

		Cp_HW = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, lHotWaterReturnTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		rhoHW = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, lHotWaterReturnTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

		lCoolElectricPower = ExhaustAbsorberReport( ChillNum ).CoolElectricPower;
		lCoolThermalEnergyUseRate = ExhaustAbsorberReport( ChillNum ).CoolThermalEnergyUseRate;
		lCoolPartLoadRatio = ExhaustAbsorberReport( ChillNum ).CoolPartLoadRatio;

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
			lAvailableHeatingCapacity = ExhaustAbsorber( ChillNum ).NomHeatCoolRatio * ExhaustAbsorber( ChillNum ).NomCoolingCap * CurveValue( lHeatCapFCoolCurve, ( ExhaustAbsorberReport( ChillNum ).CoolingLoad / ExhaustAbsorber( ChillNum ).NomCoolingCap ) );

			//Calculate current load for heating
			MyLoad = sign( max( std::abs( MyLoad ), ExhaustAbsorberReport( ChillNum ).HeatingCapacity * lMinPartLoadRat ), MyLoad );
			MyLoad = sign( min( std::abs( MyLoad ), ExhaustAbsorberReport( ChillNum ).HeatingCapacity * lMaxPartLoadRat ), MyLoad );

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

					SetComponentFlowRate( lHotWaterMassFlowRate, ExhaustAbsorber( ChillNum ).HeatReturnNodeNum, ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum, ExhaustAbsorber( ChillNum ).HWLoopNum, ExhaustAbsorber( ChillNum ).HWLoopSideNum, ExhaustAbsorber( ChillNum ).HWBranchNum, ExhaustAbsorber( ChillNum ).HWCompNum );

				} else {
					lHotWaterMassFlowRate = 0.0;
					ShowRecurringWarningErrorAtEnd( "ExhaustAbsorberChillerModel:Heating\"" + ExhaustAbsorber( ChillNum ).Name + "\", DeltaTemp = 0 in mass flow calculation", ExhaustAbsorber( ChillNum ).DeltaTempHeatErrCount );
				}
				lHotWaterSupplyTemp = HeatSupplySetPointTemp;
			} else if ( SELECT_CASE_var == 1 ) { // mass flow rates may not be changed by loop components
				lHotWaterSupplyTemp = HeatSupplySetPointTemp;
				lHeatingLoad = std::abs( lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp );

				//DSU this "2" is not a real state for flowLock
			} else if ( SELECT_CASE_var == 2 ) { // chiller is underloaded and mass flow rates has changed to a small amount and Tout drops below Setpoint

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

			//Calculate ThermalEnergy consumption for heating
			// ThermalEnergy used for heating availCap * HIR * HIR-FT * HIR-FPLR

			lHeatThermalEnergyUseRate = lAvailableHeatingCapacity * lThermalEnergyHeatRatio * CurveValue( lThermalEnergyHeatFHPLRCurve, lHeatPartLoadRatio );

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

			lExhaustInTemp = Node( lExhaustAirInletNodeNum ).Temp;
			lExhaustInFlow = Node( lExhaustAirInletNodeNum ).MassFlowRate;
			CpAir = PsyCpAirFnWTdb( lExhaustAirHumRat, lExhaustInTemp );
			lExhHeatRecPotentialHeat = lExhaustInFlow * CpAir * ( lExhaustInTemp - AbsLeavingTemp );
			if ( lExhHeatRecPotentialHeat < lHeatThermalEnergyUseRate ) {
				if ( ExhaustAbsorber( ChillNum ).ExhTempLTAbsLeavingHeatingTempIndex == 0 ) {
					ShowWarningError( "ChillerHeater:Absorption:DoubleEffect \"" + ExhaustAbsorber( ChillNum ).Name + "\"" );
					ShowContinueError( "...Exhaust temperature and flow input from Micro Turbine is not sufficient to run the chiller during heating ." );
					ShowContinueError( "...Value of Exhaust air inlet temp =" + TrimSigDigits( lExhaustInTemp, 4 ) + " C." );
					ShowContinueError( "... and Exhaust air flow rate of " + TrimSigDigits( lExhaustInFlow, 2 ) + " kg/s." );
					ShowContinueError( "...Value of minimum absorber leaving temp =" + TrimSigDigits( AbsLeavingTemp, 4 ) + " C." );
					ShowContinueError( "...Either increase the Exhaust temperature (min required = 350 C  )  or flow or both of Micro Turbine to meet the min available potential criteria." );
					ShowContinueErrorTimeStamp( "... Simulation will continue." );
				}
				ShowRecurringWarningErrorAtEnd( "ChillerHeater:Absorption:DoubleEffect \"" + ExhaustAbsorber( ChillNum ).Name + "\": Exhaust temperature from Micro Turbine is not sufficient to run the chiller during heating warning continues...", ExhaustAbsorber( ChillNum ).ExhTempLTAbsLeavingHeatingTempIndex, lExhaustInTemp, AbsLeavingTemp );
				// If exhaust is not available, it means the avilable thermal energy is 0.0 and Chiller is not available
				lHeatThermalEnergyUseRate = 0.0;
				lHeatElectricPower = 0.0;
				lHotWaterSupplyTemp = lHotWaterReturnTemp;
				HeatDeltaTemp = 0.0;
				lFractionOfPeriodRunning = min( 1.0, max( lHeatPartLoadRatio, lCoolPartLoadRatio ) / lMinPartLoadRat );
			}

			if ( lHeatElectricPower <= lCoolElectricPower ) {
				lHeatElectricPower = 0.0;
			} else {
				lHeatElectricPower -= lCoolElectricPower;
			}

		} // IF(MyLoad==0 .OR. .NOT. RunFlag)
		// Write into the Report Variables except for nodes
		ExhaustAbsorberReport( ChillNum ).HeatingLoad = lHeatingLoad;
		ExhaustAbsorberReport( ChillNum ).HeatThermalEnergyUseRate = lHeatThermalEnergyUseRate;
		ExhaustAbsorberReport( ChillNum ).HeatElectricPower = lHeatElectricPower;
		ExhaustAbsorberReport( ChillNum ).HotWaterReturnTemp = lHotWaterReturnTemp;
		ExhaustAbsorberReport( ChillNum ).HotWaterSupplyTemp = lHotWaterSupplyTemp;
		ExhaustAbsorberReport( ChillNum ).HotWaterFlowRate = lHotWaterMassFlowRate;
		ExhaustAbsorberReport( ChillNum ).HeatPartLoadRatio = lHeatPartLoadRatio;
		ExhaustAbsorberReport( ChillNum ).HeatingCapacity = lAvailableHeatingCapacity;
		ExhaustAbsorberReport( ChillNum ).FractionOfPeriodRunning = lFractionOfPeriodRunning;

		// write the combined heating and cooling ThermalEnergy used and electric used
		ExhaustAbsorberReport( ChillNum ).ThermalEnergyUseRate = lCoolThermalEnergyUseRate + lHeatThermalEnergyUseRate;
		ExhaustAbsorberReport( ChillNum ).ElectricPower = lCoolElectricPower + lHeatElectricPower;
		ExhaustAbsorberReport( ChillNum ).ExhaustInTemp = lExhaustInTemp;
		ExhaustAbsorberReport( ChillNum ).ExhaustInFlow = lExhaustInFlow;
		ExhaustAbsorberReport( ChillNum ).ExhHeatRecPotentialHeat = lExhHeatRecPotentialHeat;

	}

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateExhaustAbsorberCoolRecords(
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
		// MBadded
		int lExhaustAirInletNodeNum; // Node number on the inlet side of the plant
		//  INTEGER           :: lExhaustAirOutletNodeNum ! Node number on the outlet side of the plant
		Real64 RptConstant;

		// BEGIN ROUTINE

		lChillReturnNodeNum = ExhaustAbsorber( ChillNum ).ChillReturnNodeNum;
		lChillSupplyNodeNum = ExhaustAbsorber( ChillNum ).ChillSupplyNodeNum;
		lCondReturnNodeNum = ExhaustAbsorber( ChillNum ).CondReturnNodeNum;
		lCondSupplyNodeNum = ExhaustAbsorber( ChillNum ).CondSupplyNodeNum;

		lExhaustAirInletNodeNum = ExhaustAbsorber( ChillNum ).ExhaustAirInletNodeNum;
		if ( MyLoad == 0 || ! RunFlag ) {
			//set node temperatures

			Node( lChillSupplyNodeNum ).Temp = Node( lChillReturnNodeNum ).Temp;
			Node( lCondSupplyNodeNum ).Temp = Node( lCondReturnNodeNum ).Temp;

			Node( lExhaustAirInletNodeNum ).Temp = Node( lExhaustAirInletNodeNum ).Temp;
			//set node flow rates
			//Update Outlet Conditions so that same as Inlet, so component
			//can be bypassed if necessary
			//FlowResolver/EnforceSplitterContinuity will determine flow
			//received, whether component is running or not.
			//    Node(lChillReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lChillSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lCondReturnNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
			//    Node(lCondSupplyNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
			Node( lExhaustAirInletNodeNum ).MassFlowRate = ExhaustAbsorberReport( ChillNum ).ExhaustInFlow;
		} else {
			//set node temperatures
			Node( lChillSupplyNodeNum ).Temp = ExhaustAbsorberReport( ChillNum ).ChillSupplyTemp;
			Node( lCondSupplyNodeNum ).Temp = ExhaustAbsorberReport( ChillNum ).CondSupplyTemp;
			//set node flow rates;  for these load based models
			//assume that the sufficient evaporator flow rate available
			//    Node(lChillReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lChillSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
			//    Node(lCondReturnNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
			//    Node(lCondSupplyNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
			Node( lExhaustAirInletNodeNum ).Temp = ExhaustAbsorberReport( ChillNum ).ExhaustInTemp;
			Node( lExhaustAirInletNodeNum ).MassFlowRate = ExhaustAbsorberReport( ChillNum ).ExhaustInFlow;
		}

		// convert power to energy and instantaneous use to use over the time step
		RptConstant = TimeStepSys * SecInHour;
		ExhaustAbsorberReport( ChillNum ).CoolingEnergy = ExhaustAbsorberReport( ChillNum ).CoolingLoad * RptConstant;
		ExhaustAbsorberReport( ChillNum ).TowerEnergy = ExhaustAbsorberReport( ChillNum ).TowerLoad * RptConstant;
		ExhaustAbsorberReport( ChillNum ).ThermalEnergy = ExhaustAbsorberReport( ChillNum ).ThermalEnergyUseRate * RptConstant;
		ExhaustAbsorberReport( ChillNum ).CoolThermalEnergy = ExhaustAbsorberReport( ChillNum ).CoolThermalEnergyUseRate * RptConstant;
		ExhaustAbsorberReport( ChillNum ).ElectricEnergy = ExhaustAbsorberReport( ChillNum ).ElectricPower * RptConstant;
		ExhaustAbsorberReport( ChillNum ).CoolElectricEnergy = ExhaustAbsorberReport( ChillNum ).CoolElectricPower * RptConstant;
		if ( ExhaustAbsorberReport( ChillNum ).CoolThermalEnergyUseRate != 0.0 ) {
			ExhaustAbsorberReport( ChillNum ).ThermalEnergyCOP = ExhaustAbsorberReport( ChillNum ).CoolingLoad / ExhaustAbsorberReport( ChillNum ).CoolThermalEnergyUseRate;
		} else {
			ExhaustAbsorberReport( ChillNum ).ThermalEnergyCOP = 0.0;
		}
		//  Node(lChillSupplyNodeNum)%MassFlowRateMaxAvail = Node(lChillReturnNodeNum)%MassFlowRateMaxAvail
		//  Node(lChillSupplyNodeNum)%MassFlowRateMinAvail = Node(lChillReturnNodeNum)%MassFlowRateMinAvail
	}

	void
	UpdateExhaustAbsorberHeatRecords(
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
		Real64 RptConstant;

		// BEGIN ROUTINE

		lHeatReturnNodeNum = ExhaustAbsorber( ChillNum ).HeatReturnNodeNum;
		lHeatSupplyNodeNum = ExhaustAbsorber( ChillNum ).HeatSupplyNodeNum;

		if ( MyLoad == 0 || ! RunFlag ) {
			//set node temperatures
			Node( lHeatSupplyNodeNum ).Temp = Node( lHeatReturnNodeNum ).Temp;

			//set node flow rates
			//Update Outlet Conditions so that same as Inlet, so component
			//can be bypassed if necessary
			//FlowResolver/EnforceSplitterContinuity will determine flow
			//received, whether component is running or not.
			//    Node(lHeatReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
			//    Node(lHeatSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
		} else {
			//set node temperatures
			Node( lHeatSupplyNodeNum ).Temp = ExhaustAbsorberReport( ChillNum ).HotWaterSupplyTemp;
			//          !set node flow rates;  for these load based models
			//          !assume that the sufficient evaporator flow rate available
			//    Node(lHeatReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
			//    Node(lHeatSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
		}

		// convert power to energy and instantaneous use to use over the time step
		RptConstant = TimeStepSys * SecInHour;
		ExhaustAbsorberReport( ChillNum ).HeatingEnergy = ExhaustAbsorberReport( ChillNum ).HeatingLoad * RptConstant;
		ExhaustAbsorberReport( ChillNum ).ThermalEnergy = ExhaustAbsorberReport( ChillNum ).ThermalEnergyUseRate * RptConstant;
		ExhaustAbsorberReport( ChillNum ).HeatThermalEnergy = ExhaustAbsorberReport( ChillNum ).HeatThermalEnergyUseRate * RptConstant;
		ExhaustAbsorberReport( ChillNum ).ElectricEnergy = ExhaustAbsorberReport( ChillNum ).ElectricPower * RptConstant;
		ExhaustAbsorberReport( ChillNum ).HeatElectricEnergy = ExhaustAbsorberReport( ChillNum ).HeatElectricPower * RptConstant;

	}

	// End of Record Keeping subroutines for the Exhasut Fired Absorption Chiller Module
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

} // ChillerExhaustAbsorption

} // EnergyPlus
