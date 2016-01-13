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
#include <MicroCHPElectricGenerator.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataGenerators.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneratorDynamicsManager.hh>
#include <GeneratorFuelSupply.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace MicroCHPElectricGenerator {

	//_______________________________________________________________________
	// IEA Annex 42 generators:
	// MicroCHPElectricGenerator
	// (small-scale/residential internal combustion and Stirling Engine for CHP, )

	// MODULE INFORMATION:
	//       AUTHOR         Brent Griffth
	//       DATE WRITTEN   June 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the operation of Internal Combustion and Stirling Engine
	//  residential-scale generators for combined heat and power.

	// METHODOLOGY EMPLOYED:
	// Once the ElectricPowerManager determines that the Combustion Generator
	// is available to meet an electric load demand, it calls SimCombustionGenerator
	// which in turn calls the Combustion model.
	// See DataFuelCells.cc for structures and variables

	// REFERENCES:
	// IEA/ECBCS Annex 42 model specification titled: " A Generic Model Specification for
	// Combustion-based Residential CHP Devices"  Alex Ferguson, Nick Kelly, IEA Annex 42.
	// Module developed from

	// OTHER NOTES:
	// N/A

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGenerators;
	using namespace DataLoopNode;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::NumOfZones;
	using DataGlobals::KelvinConv;
	using DataGlobals::HoursInDay;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobalConstants::iGeneratorFuelCell;
	using namespace GeneratorDynamicsManager;
	using namespace GeneratorFuelSupply;

	// Data
	//MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS
	bool GetMicroCHPInput( true ); // When TRUE, calls subroutine to read input file.
	Array1D_bool CheckEquipName;
	Array1D_bool MySizeFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Combustion ElectricGenerator

	// MODULE SUBROUTINES:

	// Beginning of Combustion Generator Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimMicroCHPGenerator(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlagElectCenter, // simulate Generator when TRUE
		bool const RunFlagPlant, // simulate generator when true.
		Real64 const MyElectricLoad, // demand on electric generator
		Real64 const MyThermalLoad, // thermal demand on cogenerator
		bool const FirstHVACIteration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   July 2006
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the main driver for the model for
		// internal combustion engine and
		// Stirling cycle engine model from IEA/ECBCS Annex 42 r.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataPlant::PlantFirstSizeCompleted;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int GenNum; // Generator number counter

		//Get Generator data from input file
		if ( GetMicroCHPInput ) {
			GetMicroCHPGeneratorInput();
			GetMicroCHPInput = false;
		}

		//  Call InitMicroCHPNoNormalizeGenerators

		if ( GeneratorIndex == 0 ) {
			GenNum = FindItemInList( GeneratorName, MicroCHP );
			if ( GenNum == 0 ) ShowFatalError( "SimMicroCHPGenerator: Specified Generator not one of Valid Micro CHP Generators " + GeneratorName );
			GeneratorIndex = GenNum;
		} else {
			GenNum = GeneratorIndex;
			if ( GenNum > NumMicroCHPs || GenNum < 1 ) {
				ShowFatalError( "SimMicroCHPGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Number of Micro CHP Generators=" + TrimSigDigits( NumMicroCHPs ) + ", Generator name=" + GeneratorName );
			}
			if ( CheckEquipName( GenNum ) ) {
				if ( GeneratorName != MicroCHP( GenNum ).Name ) {
					ShowFatalError( "SimMicroCHPNoNormalizeGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + MicroCHP( GenNum ).Name );
				}
				CheckEquipName( GenNum ) = false;
			}
		}

		if ( MicroCHP( GenNum ).ModelTypeAnnex42 ) { // call the non normalize calc routines (set for future extension to normalize ones)

			InitMicroCHPNoNormalizeGenerators( GenNum, FirstHVACIteration );
			if ( ! PlantFirstSizeCompleted ) return;
			CalcMicroCHPNoNormalizeGeneratorModel( GenNum, RunFlagElectCenter, RunFlagPlant, MyElectricLoad, MyThermalLoad, FirstHVACIteration );

			CalcUpdateHeatRecovery( GenNum, FirstHVACIteration );

			UpdateMicroCHPGeneratorRecords( GenNum );

		}

	}

	// End MicroCHPNoNormalize Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of Combustion Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetMicroCHPGeneratorInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Brent Griffith
		//       DATE WRITTEN:    July 2005

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the Micro CHP Generator models.

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using namespace DataGenerators;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using CurveManager::GetCurveCheck;
		using CurveManager::CurveValue;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataHeatBalance::Zone;
		using DataHeatBalance::IntGainTypeOf_GeneratorMicroCHP;
		using ScheduleManager::GetScheduleIndex;
		using General::RoundSigDigits;
		using namespace GeneratorFuelSupply;
		using namespace GeneratorDynamicsManager;

		// Locals
		int GeneratorNum; // Generator counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 25 ); // character string data
		Array1D< Real64 > NumArray( 200 ); // numeric data TODO deal with allocatable for extensible
		static bool ErrorsFound( false ); // error flag
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		//  INTEGER       :: thisMicroCHP  !temporary index
		//  INTEGER       :: otherMicroCHP !loop counter and temporary indexer
		//  INTEGER       :: I   ! loop counter
		static bool MyOneTimeFlag( true );
		int CHPParamNum; // loop count and temporary index
		std::string ObjMSGName; // string for error messages
		int thisParamID;

		// execution
		if ( MyOneTimeFlag ) {

			// call to Fuel supply module to set up data there.
			GetGeneratorFuelSupplyInput();

			// First get the Micro CHP Parameters so they can be nested in structure later
			cCurrentModuleObject = "Generator:MicroCHP:NonNormalizedParameters";
			NumMicroCHPParams = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumMicroCHPParams <= 0 ) {
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			MicroCHPParamInput.allocate( NumMicroCHPParams );
			CheckEquipName.dimension( NumMicroCHPParams, true );

			for ( CHPParamNum = 1; CHPParamNum <= NumMicroCHPParams; ++CHPParamNum ) {
				GetObjectItem( cCurrentModuleObject, CHPParamNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				//  Can't validate this name.
				//    IsNotOK=.FALSE.
				//    IsBlank=.FALSE.

				//    CALL VerifyName(AlphArray(1),MicroCHP%Name,CHPParamNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject))
				//    IF (IsNotOK) THEN
				//      ErrorsFound=.TRUE.
				//      IF (IsBlank) AlphArray(1)='xxxxx'
				//    ENDIF

				ObjMSGName = cCurrentModuleObject + " Named " + AlphArray( 1 );

				MicroCHPParamInput( CHPParamNum ).Name = AlphArray( 1 ); //A1 name
				MicroCHPParamInput( CHPParamNum ).MaxElecPower = NumArray( 1 ); //N1 Maximum Electric Power [W]
				MicroCHPParamInput( CHPParamNum ).MinElecPower = NumArray( 2 ); //N2 Minimum Electric Power [W]
				MicroCHPParamInput( CHPParamNum ).MinWaterMdot = NumArray( 3 ); //N3 Minimum Cooling Water Flow Rate [kg/s]
				MicroCHPParamInput( CHPParamNum ).MaxWaterTemp = NumArray( 4 ); //N3 Maximum Cooling Water Inlet Temp [C]
				MicroCHPParamInput( CHPParamNum ).ElecEffCurveID = GetCurveCheck( AlphArray( 2 ), ErrorsFound, ObjMSGName ); //Electrical Eff. ID
				MicroCHPParamInput( CHPParamNum ).ThermalEffCurveID = GetCurveCheck( AlphArray( 3 ), ErrorsFound, ObjMSGName ); //Thermal Efficiency

				if ( SameString( AlphArray( 4 ), "InternalControl" ) ) {
					MicroCHPParamInput( CHPParamNum ).InternalFlowControl = true; //  A4, \field Cooling Water Flow Rate Mode
					MicroCHPParamInput( CHPParamNum ).PlantFlowControl = false;
				}
				if ( ( ! ( SameString( AlphArray( 4 ), "InternalControl" ) ) ) && ( ! ( SameString( AlphArray( 4 ), "PlantControl" ) ) ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				if ( MicroCHPParamInput( CHPParamNum ).InternalFlowControl ) { // get the curve
					MicroCHPParamInput( CHPParamNum ).WaterFlowCurveID = GetCurveCheck( AlphArray( 5 ), ErrorsFound, ObjMSGName );
					//  Curve for Cooling Water Flow Rate
				}
				MicroCHPParamInput( CHPParamNum ).AirFlowCurveID = GetCurveCheck( AlphArray( 6 ), ErrorsFound, ObjMSGName );
				//  Name of Curve for Air Flow Rate
				MicroCHPParamInput( CHPParamNum ).DeltaPelMax = NumArray( 5 ); // N5 Maximum rate of change in net electrical power [W/s]
				MicroCHPParamInput( CHPParamNum ).DeltaFuelMdotMax = NumArray( 6 ); //N6 Maximum Rate of change in fuel flow rate [kg/s2]
				MicroCHPParamInput( CHPParamNum ).UAhx = NumArray( 7 ); // N7 Heat Exchanger UA_hx
				MicroCHPParamInput( CHPParamNum ).UAskin = NumArray( 8 ); //N8 Skin Loss UA_loss
				MicroCHPParamInput( CHPParamNum ).RadiativeFraction = NumArray( 9 ); //N9 radiative fraction for skin losses
				MicroCHPParamInput( CHPParamNum ).MCeng = NumArray( 10 ); // N10 Aggregated Thermal Mass of Generator MC_eng
				if ( MicroCHPParamInput( CHPParamNum ).MCeng <= 0.0 ) {
					ShowSevereError( "Invalid, " + cNumericFieldNames( 10 ) + " = " + RoundSigDigits( NumArray( 10 ), 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Thermal mass must be greater than zero" );
					ErrorsFound = true;
				}
				MicroCHPParamInput( CHPParamNum ).MCcw = NumArray( 11 ); // Aggregated Thermal Mass of Heat Recovery MC_cw
				if ( MicroCHPParamInput( CHPParamNum ).MCcw <= 0.0 ) {
					ShowSevereError( "Invalid, " + cNumericFieldNames( 11 ) + " = " + RoundSigDigits( NumArray( 11 ), 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Thermal mass must be greater than zero" );
					ErrorsFound = true;
				}
				MicroCHPParamInput( CHPParamNum ).Pstandby = NumArray( 12 ); // N12 Standby Power [W]

				if ( SameString( AlphArray( 7 ), "TimeDelay" ) ) {
					MicroCHPParamInput( CHPParamNum ).WarmUpByTimeDelay = true;
					MicroCHPParamInput( CHPParamNum ).WarmUpByEngineTemp = false;
				}
				if ( ( ! ( SameString( AlphArray( 7 ), "NominalEngineTemperature" ) ) ) && ( ! ( SameString( AlphArray( 7 ), "TimeDelay" ) ) ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 7 ) + " = " + AlphArray( 7 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				MicroCHPParamInput( CHPParamNum ).kf = NumArray( 13 ); // N13 Warmup Fuel Flow Rate Coefficient k_f
				MicroCHPParamInput( CHPParamNum ).TnomEngOp = NumArray( 14 ); // N14 Nominal Engine Operating Temperature [C]
				MicroCHPParamInput( CHPParamNum ).kp = NumArray( 15 ); // N15 Warmup Power Coefficient k_p
				MicroCHPParamInput( CHPParamNum ).Rfuelwarmup = NumArray( 16 ); // N16 Warm Up Fuel Flow Rate Limit Ratio
				MicroCHPParamInput( CHPParamNum ).WarmUpDelay = NumArray( 17 ); // N17 Warm Up Delay Time

				MicroCHPParamInput( CHPParamNum ).PcoolDown = NumArray( 18 ); // N18 Cool Down Power

				MicroCHPParamInput( CHPParamNum ).CoolDownDelay = NumArray( 19 ); // N19 Cool Down Delay Time in seconds

				if ( SameString( AlphArray( 8 ), "MandatoryCoolDown" ) ) {
					MicroCHPParamInput( CHPParamNum ).MandatoryFullCoolDown = true;
					MicroCHPParamInput( CHPParamNum ).WarmRestartOkay = false;
				}
				if ( ( ! ( SameString( AlphArray( 8 ), "MandatoryCoolDown" ) ) ) && ( ! ( SameString( AlphArray( 8 ), "OptionalCoolDown" ) ) ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 8 ) + " = " + AlphArray( 8 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}

			cCurrentModuleObject = "Generator:MicroCHP";
			NumMicroCHPs = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumMicroCHPs <= 0 ) {
				// shouldn't ever come here?
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			//ALLOCATE ARRAYS

			if ( ! ( allocated( MicroCHP ) ) ) {
				MicroCHP.allocate( NumMicroCHPs ); // inits handeled in derived type definitions
			}

			// load in Micro CHPs
			for ( GeneratorNum = 1; GeneratorNum <= NumMicroCHPs; ++GeneratorNum ) {
				GetObjectItem( cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), MicroCHP, GeneratorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				//GENERATOR:MICRO CHP,
				MicroCHP( GeneratorNum ).Name = AlphArray( 1 ); //  A1 Generator name
				ObjMSGName = cCurrentModuleObject + " Named " + AlphArray( 1 );
				MicroCHP( GeneratorNum ).ParamObjName = AlphArray( 2 ); //  A2 Micro CHP Parameter Object Name
				//find input structure
				thisParamID = FindItemInList( AlphArray( 2 ), MicroCHPParamInput );
				if ( thisParamID != 0 ) {
					MicroCHP( GeneratorNum ).A42Model = MicroCHPParamInput( thisParamID ); // entire structure of input data assigned here!
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

				if ( ! lAlphaFieldBlanks( 3 ) ) {
					MicroCHP( GeneratorNum ).ZoneName = AlphArray( 3 ); //  A3 Zone Name
					MicroCHP( GeneratorNum ).ZoneID = FindItemInList( MicroCHP( GeneratorNum ).ZoneName, Zone );
					if ( MicroCHP( GeneratorNum ).ZoneID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}
				} else {
					MicroCHP( GeneratorNum ).ZoneID = 0;
				}
				MicroCHP( GeneratorNum ).PlantInletNodeName = AlphArray( 4 ); //  A4 Cooling Water Inlet Node Name
				MicroCHP( GeneratorNum ).PlantOutletNodeName = AlphArray( 5 ); //  A5 Cooling Water Outlet Node Name
				//find node ids for water path
				MicroCHP( GeneratorNum ).PlantInletNodeID = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				MicroCHP( GeneratorNum ).PlantOutletNodeID = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Heat Recovery Nodes" );

				MicroCHP( GeneratorNum ).AirInletNodeName = AlphArray( 6 ); //  A6 Air Inlet Node Name
				// check the node connections
				MicroCHP( GeneratorNum ).AirInletNodeID = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

				MicroCHP( GeneratorNum ).AirOutletNodeName = AlphArray( 7 ); //  A7 Air Outlet Node Name
				MicroCHP( GeneratorNum ).AirOutletNodeID = GetOnlySingleNode( AlphArray( 7 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

				MicroCHP( GeneratorNum ).FuelSupplyID = FindItemInList( AlphArray( 8 ), FuelSupply ); // Fuel Supply ID
				if ( MicroCHP( GeneratorNum ).FuelSupplyID == 0 ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 8 ) + " = " + AlphArray( 8 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

				if ( lAlphaFieldBlanks( 9 ) ) {
					MicroCHP( GeneratorNum ).AvailabilitySchedID = ScheduleAlwaysOn;
				} else {
					MicroCHP( GeneratorNum ).AvailabilitySchedID = GetScheduleIndex( AlphArray( 9 ) );
					if ( MicroCHP( GeneratorNum ).AvailabilitySchedID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 9 ) + " = " + AlphArray( 9 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}
				}
				MicroCHP( GeneratorNum ).A42Model.TengLast = 20.0; // inits
				MicroCHP( GeneratorNum ).A42Model.TempCWOutLast = 20.0; // inits

			}

			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
			}

			//setup report variables
			for ( GeneratorNum = 1; GeneratorNum <= NumMicroCHPs; ++GeneratorNum ) {

				SetupOutputVariable( "Generator Off Mode Time [s]", MicroCHP( GeneratorNum ).Report.OffModeTime, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Standby Mode Time [s]", MicroCHP( GeneratorNum ).Report.StandyByModeTime, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Warm Up Mode Time [s]", MicroCHP( GeneratorNum ).Report.WarmUpModeTime, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Normal Operating Mode Time [s]", MicroCHP( GeneratorNum ).Report.NormalModeTime, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Cool Down Mode Time [s]", MicroCHP( GeneratorNum ).Report.CoolDownModeTime, "System", "Sum", MicroCHP( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Produced Electric Power [W]", MicroCHP( GeneratorNum ).Report.ACPowerGen, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Produced Electric Energy [J]", MicroCHP( GeneratorNum ).Report.ACEnergyGen, "System", "Sum", MicroCHP( GeneratorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );
				SetupOutputVariable( "Generator Produced Thermal Rate [W]", MicroCHP( GeneratorNum ).Report.QdotHR, "system", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Produced Thermal Energy [J]", MicroCHP( GeneratorNum ).Report.TotalHeatEnergyRec, "system", "Sum", MicroCHP( GeneratorNum ).Name, _, "ENERGYTRANSFER", "COGENERATION", _, "Plant" );

				SetupOutputVariable( "Generator Electric Efficiency []", MicroCHP( GeneratorNum ).Report.ElectEfficiency, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Thermal Efficiency []", MicroCHP( GeneratorNum ).Report.ThermalEfficiency, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Gross Input Heat Rate [W]", MicroCHP( GeneratorNum ).Report.QdotGross, "system", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Steady State Engine Heat Generation Rate [W]", MicroCHP( GeneratorNum ).Report.Qgenss, "system", "Average", MicroCHP( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Engine Heat Exchange Rate [W]", MicroCHP( GeneratorNum ).Report.QdotHX, "system", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Air Mass Flow Rate [kg/s]", MicroCHP( GeneratorNum ).Report.MdotAir, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Fuel Molar Flow Rate [kmol/s]", MicroCHP( GeneratorNum ).Report.NdotFuel, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Fuel Mass Flow Rate [kg/s]", MicroCHP( GeneratorNum ).Report.MdotFuel, "System", "Average", MicroCHP( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Engine Temperature [C]", MicroCHP( GeneratorNum ).Report.Tengine, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Coolant Inlet Temperature [C]", MicroCHP( GeneratorNum ).Report.HeatRecInletTemp, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Coolant Outlet Temperature [C]", MicroCHP( GeneratorNum ).Report.HeatRecOutletTemp, "System", "Average", MicroCHP( GeneratorNum ).Name );

				// this next one needs to be reconciled with non-gas fuel constituents.
				//   need custom resourceTypeKey or something for user defined fuel compositions.
				SetupOutputVariable( "Generator Fuel HHV Basis Energy [J]", MicroCHP( GeneratorNum ).Report.FuelEnergyHHV, "System", "Sum", MicroCHP( GeneratorNum ).Name, _, "Gas", "COGENERATION", _, "Plant" );

				SetupOutputVariable( "Generator Fuel HHV Basis Rate [W]", MicroCHP( GeneratorNum ).Report.FuelEnergyUseRateHHV, "System", "Average", MicroCHP( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Fuel LHV Basis Energy [J]", MicroCHP( GeneratorNum ).Report.FuelEnergyLHV, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Fuel LHV Basis Rate [W]", MicroCHP( GeneratorNum ).Report.FuelEnergyUseRateLHV, "System", "Average", MicroCHP( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Fuel Compressor Electric Power [W]", MicroCHP( GeneratorNum ).Report.FuelCompressPower, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Fuel Compressor Electric Energy [J]", MicroCHP( GeneratorNum ).Report.FuelCompressEnergy, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Fuel Compressor Skin Heat Loss Rate [W]", MicroCHP( GeneratorNum ).Report.FuelCompressSkinLoss, "System", "Average", MicroCHP( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Zone Sensible Heat Transfer Rate [W]", MicroCHP( GeneratorNum ).Report.SkinLossPower, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Zone Sensible Heat Transfer Energy [J]", MicroCHP( GeneratorNum ).Report.SkinLossEnergy, "System", "Sum", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Zone Convection Heat Transfer Rate [W]", MicroCHP( GeneratorNum ).Report.SkinLossConvect, "System", "Average", MicroCHP( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Zone Radiation Heat Transfer Rate [W]", MicroCHP( GeneratorNum ).Report.SkinLossRadiat, "System", "Average", MicroCHP( GeneratorNum ).Name );

				if ( MicroCHP( GeneratorNum ).ZoneID > 0 ) {
					SetupZoneInternalGain( MicroCHP( GeneratorNum ).ZoneID, "Generator:MicroCHP", MicroCHP( GeneratorNum ).Name, IntGainTypeOf_GeneratorMicroCHP, MicroCHP( GeneratorNum ).Report.SkinLossConvect, _, MicroCHP( GeneratorNum ).Report.SkinLossRadiat );
				}

			}

			MyOneTimeFlag = false;
		}

	}

	// PARAMETERS

	void
	InitMicroCHPNoNormalizeGenerators(
		int const GeneratorNum, // Generator number
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         BGriffith
		//       DATE WRITTEN   March 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// inits

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::HourOfDay;
		using DataGlobals::SysSizingCalc;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_Generator_MicroCHP;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizeCompleted;
		using DataPlant::SupplySide;
		using DataPlant::LoopFlowStatus_TakesWhatGets;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::DemandSide;
		using DataSizing::PlantSizData;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using CurveManager::GetCurveCheck;
		using CurveManager::CurveValue;
		using FluidProperties::GetDensityGlycol;
		using namespace GeneratorDynamicsManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitMicroCHPNoNormalizeGenerators" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int DynaCntrlNum( 0 );
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		static bool MyOneTimeFlag( true ); // Initialization flag
		static Array1D_bool MyEnvrnFlag; // Used for initializations each begin environment flag
		static Array1D_bool MyPlantScanFlag;

		bool errFlag;
		Real64 mdot; // local temporary for mass flow rate
		Real64 rho; // local temporary for fluid density

		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumMicroCHPs );
			MyPlantScanFlag.allocate( NumMicroCHPs );
			MySizeFlag.allocate( NumMicroCHPs );
			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MySizeFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyPlantScanFlag( GeneratorNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( MicroCHP( GeneratorNum ).Name, TypeOf_Generator_MicroCHP, MicroCHP( GeneratorNum ).CWLoopNum, MicroCHP( GeneratorNum ).CWLoopSideNum, MicroCHP( GeneratorNum ).CWBranchNum, MicroCHP( GeneratorNum ).CWCompNum, _, _, _, _, _, errFlag );

			if ( errFlag ) {
				ShowFatalError( "InitMicroCHPNoNormalizeGenerators: Program terminated for previous conditions." );
			}

			if ( ! MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {
				//IF this is on the supply side and not internal flow control then reset flow priority to lower
				if ( MicroCHP( GeneratorNum ).CWLoopSideNum == SupplySide ) {
					PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).LoopSide( MicroCHP( GeneratorNum ).CWLoopSideNum ).Branch( MicroCHP( GeneratorNum ).CWBranchNum ).Comp( MicroCHP( GeneratorNum ).CWCompNum ).FlowPriority = LoopFlowStatus_TakesWhatGets;

				}

			}

			MyPlantScanFlag( GeneratorNum ) = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( GeneratorNum ) && ! MyPlantScanFlag( GeneratorNum ) && ( PlantFirstSizesOkayToFinalize ) ) {
			rho = GetDensityGlycol( PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).FluidName, Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp, PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).FluidIndex, RoutineName );
			if ( MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) { // got a curve
				MicroCHP( GeneratorNum ).PlantMassFlowRateMax = 2.0 * CurveValue( MicroCHP( GeneratorNum ).A42Model.WaterFlowCurveID, MicroCHP( GeneratorNum ).A42Model.MaxElecPower, Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp );
			} else if ( MicroCHP( GeneratorNum ).CWLoopSideNum == SupplySide ) {
				if ( PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).MaxMassFlowRate > 0.0 ) {
					MicroCHP( GeneratorNum ).PlantMassFlowRateMax = PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).MaxMassFlowRate;
				} else if ( PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).PlantSizNum > 0 ) {
					MicroCHP( GeneratorNum ).PlantMassFlowRateMax = PlantSizData( MicroCHP( GeneratorNum ).CWLoopNum ).DesVolFlowRate * rho;
				} else {
					MicroCHP( GeneratorNum ).PlantMassFlowRateMax = 2.0;
				}

			} else if ( MicroCHP( GeneratorNum ).CWLoopSideNum == DemandSide ) {
				MicroCHP( GeneratorNum ).PlantMassFlowRateMax = 2.0; // would like to use plant loop max but not ready yet
			}

			RegisterPlantCompDesignFlow( MicroCHP( GeneratorNum ).PlantInletNodeID, MicroCHP( GeneratorNum ).PlantMassFlowRateMax / rho );

			MicroCHP( GeneratorNum ).A42Model.ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, MicroCHP( GeneratorNum ).A42Model.MaxElecPower, MicroCHP( GeneratorNum ).PlantMassFlowRateMax, Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp );

			MicroCHP( GeneratorNum ).A42Model.ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, MicroCHP( GeneratorNum ).A42Model.MaxElecPower, MicroCHP( GeneratorNum ).PlantMassFlowRateMax, Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp );

			SetupGeneratorControlStateManager( GeneratorNum );
			MySizeFlag( GeneratorNum ) = false;
		}

		if ( MySizeFlag( GeneratorNum ) ) return;

		DynaCntrlNum = MicroCHP( GeneratorNum ).DynamicsControlID;

		if ( BeginEnvrnFlag && MyEnvrnFlag( GeneratorNum ) ) {
			//reset to starting condition for different environment runperiods, design days
			MicroCHP( GeneratorNum ).A42Model.TengLast = 20.0;
			MicroCHP( GeneratorNum ).A42Model.TempCWOutLast = 20.0;
			MicroCHP( GeneratorNum ).A42Model.TimeElapsed = 0.0;
			MicroCHP( GeneratorNum ).A42Model.OpMode = 0;
			MicroCHP( GeneratorNum ).A42Model.OffModeTime = 0.0;
			MicroCHP( GeneratorNum ).A42Model.StandyByModeTime = 0.0;
			MicroCHP( GeneratorNum ).A42Model.WarmUpModeTime = 0.0;
			MicroCHP( GeneratorNum ).A42Model.NormalModeTime = 0.0;
			MicroCHP( GeneratorNum ).A42Model.CoolDownModeTime = 0.0;
			MicroCHP( GeneratorNum ).A42Model.Pnet = 0.0;
			MicroCHP( GeneratorNum ).A42Model.ElecEff = 0.0;
			MicroCHP( GeneratorNum ).A42Model.Qgross = 0.0;
			MicroCHP( GeneratorNum ).A42Model.ThermEff = 0.0;
			MicroCHP( GeneratorNum ).A42Model.Qgenss = 0.0;
			MicroCHP( GeneratorNum ).A42Model.NdotFuel = 0.0;
			MicroCHP( GeneratorNum ).A42Model.MdotFuel = 0.0;
			MicroCHP( GeneratorNum ).A42Model.Teng = 20.0;
			MicroCHP( GeneratorNum ).A42Model.TcwIn = 20.0;
			MicroCHP( GeneratorNum ).A42Model.TcwOut = 20.0;
			MicroCHP( GeneratorNum ).A42Model.MdotAir = 0.0;
			MicroCHP( GeneratorNum ).A42Model.QdotSkin = 0.0;
			MicroCHP( GeneratorNum ).A42Model.QdotConvZone = 0.0;
			MicroCHP( GeneratorNum ).A42Model.QdotRadZone = 0.0;
			GeneratorDynamics( DynaCntrlNum ).LastOpMode = OpModeOff;
			GeneratorDynamics( DynaCntrlNum ).CurrentOpMode = OpModeOff;
			GeneratorDynamics( DynaCntrlNum ).FractionalDayofLastShutDown = 0.0;
			GeneratorDynamics( DynaCntrlNum ).FractionalDayofLastStartUp = 0.0;
			GeneratorDynamics( DynaCntrlNum ).HasBeenOn = false;
			GeneratorDynamics( DynaCntrlNum ).DuringStartUp = false;
			GeneratorDynamics( DynaCntrlNum ).DuringShutDown = false;
			GeneratorDynamics( DynaCntrlNum ).FuelMdotLastTimestep = 0.0;
			GeneratorDynamics( DynaCntrlNum ).PelLastTimeStep = 0.0;
			GeneratorDynamics( DynaCntrlNum ).NumCycles = 0;

			FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).QskinLoss = 0.0;

			InitComponentNodes( 0.0, MicroCHP( GeneratorNum ).PlantMassFlowRateMax, MicroCHP( GeneratorNum ).PlantInletNodeID, MicroCHP( GeneratorNum ).PlantOutletNodeID, MicroCHP( GeneratorNum ).CWLoopNum, MicroCHP( GeneratorNum ).CWLoopSideNum, MicroCHP( GeneratorNum ).CWBranchNum, MicroCHP( GeneratorNum ).CWCompNum );

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( GeneratorNum ) = true;
		}

		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( MicroCHP( GeneratorNum ).A42Model.TimeElapsed != TimeElapsed ) {
			// The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
			// timestep for use as the initial conditions of each iteration that does not advance the system timestep.
			MicroCHP( GeneratorNum ).A42Model.TengLast = MicroCHP( GeneratorNum ).A42Model.Teng;
			MicroCHP( GeneratorNum ).A42Model.TempCWOutLast = MicroCHP( GeneratorNum ).A42Model.TcwOut;
			MicroCHP( GeneratorNum ).A42Model.TimeElapsed = TimeElapsed;
			GeneratorDynamics( DynaCntrlNum ).LastOpMode = GeneratorDynamics( DynaCntrlNum ).CurrentOpMode;
			GeneratorDynamics( DynaCntrlNum ).FuelMdotLastTimestep = MicroCHP( GeneratorNum ).A42Model.MdotFuel;
			GeneratorDynamics( DynaCntrlNum ).PelLastTimeStep = MicroCHP( GeneratorNum ).A42Model.Pnet;
		}

		if ( ! MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {

			mdot = MicroCHP( GeneratorNum ).PlantMassFlowRateMax;
			SetComponentFlowRate( mdot, MicroCHP( GeneratorNum ).PlantInletNodeID, MicroCHP( GeneratorNum ).PlantOutletNodeID, MicroCHP( GeneratorNum ).CWLoopNum, MicroCHP( GeneratorNum ).CWLoopSideNum, MicroCHP( GeneratorNum ).CWBranchNum, MicroCHP( GeneratorNum ).CWCompNum );
			MicroCHP( GeneratorNum ).PlantMassFlowRate = mdot;

		}

	}

	void
	CalcMicroCHPNoNormalizeGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlagElectCenter, // TRUE when Generator operating
		bool const RunFlagPlant,
		Real64 const MyElectricLoad, // Generator demand
		Real64 const MyThermalLoad,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR        B Griffith
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Main calculation subroutine for the IEA Annex 42 model

		// METHODOLOGY EMPLOYED:
		// curve fit, dynamic control limits,

		// REFERENCES:
		// IEA Annex 42 FC-COGEN-SIM "A Generic Model Specification for Combustion-based Residential CHP Devices"
		// Alex Ferguson, Nick Kelly, Version 3, June 26, 2006

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHeatBalFanSys::MAT;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using CurveManager::CurveValue;
		using namespace DataGlobalConstants;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SetComponentFlowRate;
		using DataEnvironment::OutDryBulbTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMicroCHPNoNormalizeGeneratorModel" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 AllowedLoad( 0.0 );
		static int CurrentOpMode( 0 );
		static Real64 PLRforSubtimestepStartUp( 1.0 );
		static Real64 PLRforSubtimestepShutDown( 0.0 );
		static bool RunFlag( false );
		static int DynaCntrlNum( 0 );
		static Real64 Pnetss( 0.0 );
		static Real64 Pstandby( 0.0 ); // power draw during standby, positive here means negative production
		static Real64 Pcooler( 0.0 ); // power draw during cool down, positive here means negative production
		//  REAL(r64)    :: Pnet   = 0.0d0
		static Real64 NdotFuel( 0.0 );

		static bool ConstrainedIncreasingNdot( false );
		static bool ConstrainedDecreasingNdot( false );
		static int i( 0 );
		static Real64 dt( 0.0 );
		static Real64 ElecEff( 0.0 );
		static Real64 MdotAir( 0.0 );
		static Real64 Qgenss( 0.0 );
		static Real64 MdotCW( 0.0 );
		static Real64 TcwIn( 0.0 );
		static Real64 TcwOut( 0.0 );
		static Real64 MdotFuel( 0.0 );
		static Real64 MdotFuelAllowed( 0.0 );
		static Real64 MdotFuelMax( 0.0 );
		static Real64 MdotFuelWarmup( 0.0 );
		static Real64 Pmax( 0.0 );
		static Real64 Qgross( 0.0 );
		static Real64 Teng( 0.0 );
		static Real64 ThermEff( 0.0 );
		static Real64 Cp( 0.0 ); // local fluid specific heat
		static Real64 thisAmbientTemp( 0.0 );

		bool EnergyBalOK; // check for balance to exit loop

		DynaCntrlNum = MicroCHP( GeneratorNum ).DynamicsControlID;

		ManageGeneratorControlState( iGeneratorMicroCHP, MicroCHP( GeneratorNum ).Name, GeneratorNum, RunFlagElectCenter, RunFlagPlant, MyElectricLoad, MyThermalLoad, AllowedLoad, CurrentOpMode, PLRforSubtimestepStartUp, PLRforSubtimestepShutDown, FirstHVACIteration );

		if ( RunFlagElectCenter || RunFlagPlant ) RunFlag = true;

		Teng = MicroCHP( GeneratorNum ).A42Model.Teng;
		TcwOut = MicroCHP( GeneratorNum ).A42Model.TcwOut;
		dt = TimeStepSys * SecInHour;

		if ( MicroCHP( GeneratorNum ).ZoneID > 0 ) {
			thisAmbientTemp = MAT( MicroCHP( GeneratorNum ).ZoneID );
		} else { // outdoor location, no zone
			thisAmbientTemp = OutDryBulbTemp;
		}

		{ auto const SELECT_CASE_var( CurrentOpMode );

		if ( SELECT_CASE_var == OpModeOff ) { // same as standby in model spec but no Pnet standby electicity losses.

			Qgenss = 0.0;
			MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
			TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
			Pnetss = 0.0;
			Pstandby = 0.0;
			Pcooler = MicroCHP( GeneratorNum ).A42Model.PcoolDown * PLRforSubtimestepShutDown;
			ElecEff = 0.0;
			ThermEff = 0.0;
			Qgross = 0.0;
			NdotFuel = 0.0;
			MdotFuel = 0.0;
			MdotAir = 0.0;

			MdotCW = 0.0;
			SetComponentFlowRate( MdotCW, MicroCHP( GeneratorNum ).PlantInletNodeID, MicroCHP( GeneratorNum ).PlantOutletNodeID, MicroCHP( GeneratorNum ).CWLoopNum, MicroCHP( GeneratorNum ).CWLoopSideNum, MicroCHP( GeneratorNum ).CWBranchNum, MicroCHP( GeneratorNum ).CWCompNum );
			MicroCHP( GeneratorNum ).PlantMassFlowRate = MdotCW;

		} else if ( SELECT_CASE_var == OpModeStandby ) {
			Qgenss = 0.0;
			MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
			TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
			Pnetss = 0.0;
			Pstandby = MicroCHP( GeneratorNum ).A42Model.Pstandby * ( 1.0 - PLRforSubtimestepShutDown );
			Pcooler = MicroCHP( GeneratorNum ).A42Model.PcoolDown * PLRforSubtimestepShutDown;
			ElecEff = 0.0;
			ThermEff = 0.0;
			Qgross = 0.0;
			NdotFuel = 0.0;
			MdotFuel = 0.0;
			MdotAir = 0.0;

			MdotCW = 0.0;
			SetComponentFlowRate( MdotCW, MicroCHP( GeneratorNum ).PlantInletNodeID, MicroCHP( GeneratorNum ).PlantOutletNodeID, MicroCHP( GeneratorNum ).CWLoopNum, MicroCHP( GeneratorNum ).CWLoopSideNum, MicroCHP( GeneratorNum ).CWBranchNum, MicroCHP( GeneratorNum ).CWCompNum );
			MicroCHP( GeneratorNum ).PlantMassFlowRate = MdotCW;

		} else if ( SELECT_CASE_var == OpModeWarmUp ) {

			if ( MicroCHP( GeneratorNum ).A42Model.WarmUpByTimeDelay ) {
				// Internal combustion engine.  This is just like normal  operation but no net power yet.
				Pnetss = MyElectricLoad; // W
				Pstandby = 0.0;
				Pcooler = MicroCHP( GeneratorNum ).A42Model.PcoolDown * PLRforSubtimestepShutDown;
				TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
				MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
				if ( MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {
					MdotCW = FuncDetermineCWMdotForInternalFlowControl( GeneratorNum, Pnetss, TcwIn );
				}
				ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn );
				ElecEff = max( 0.0, ElecEff ); //protect against bad curve result

				if ( ElecEff > 0.0 ) { // trap divide by bad thing
					Qgross = Pnetss / ElecEff; //W
				} else {
					Qgross = 0.0;
				}
				ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn );
				ThermEff = max( 0.0, ThermEff ); //protect against bad curve result

				Qgenss = ThermEff * Qgross; //W

				MdotFuel = Qgross / ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 ) * FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
				//  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)

				ManageGeneratorFuelFlow( iGeneratorMicroCHP, MicroCHP( GeneratorNum ).Name, GeneratorNum, RunFlag, MdotFuel, MdotFuelAllowed, ConstrainedIncreasingNdot, ConstrainedDecreasingNdot );

				if ( ConstrainedIncreasingNdot || ConstrainedDecreasingNdot ) { // recalculate Pnetss with new NdotFuel with iteration
					MdotFuel = MdotFuelAllowed;
					NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
					Qgross = NdotFuel * ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 );

					for ( i = 1; i <= 20; ++i ) { // iterating here  could add use of seach method
						Pnetss = Qgross * ElecEff;
						if ( MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {
							MdotCW = FuncDetermineCWMdotForInternalFlowControl( GeneratorNum, Pnetss, TcwIn );
						}
						ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn );
						ElecEff = max( 0.0, ElecEff ); //protect against bad curve result
					}

					ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn );
					ThermEff = max( 0.0, ThermEff ); //protect against bad curve result
					Qgenss = ThermEff * Qgross; //W

				}
				Pnetss = 0.0; // no actually power produced here.
				NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
				MdotAir = CurveValue( MicroCHP( GeneratorNum ).A42Model.AirFlowCurveID, MdotFuel );
				MdotAir = max( 0.0, MdotAir ); //protect against bad curve result

			} else if ( MicroCHP( GeneratorNum ).A42Model.WarmUpByEngineTemp ) {
				// Stirling engine mode warm up
				//   find MdotFuelMax
				Pmax = MicroCHP( GeneratorNum ).A42Model.MaxElecPower;
				Pnetss = 0.0;
				Pstandby = 0.0;
				Pcooler = MicroCHP( GeneratorNum ).A42Model.PcoolDown * PLRforSubtimestepShutDown; // could be here with part load in cool down
				TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
				MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
				ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, Pmax, MdotCW, TcwIn );
				ElecEff = max( 0.0, ElecEff ); //protect against bad curve result
				if ( ElecEff > 0.0 ) { // trap divide by bad thing
					Qgross = Pmax / ElecEff; //W
				} else {
					Qgross = 0.0;
				}
				NdotFuel = Qgross / ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 );
				//  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
				MdotFuelMax = NdotFuel * FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;

				if ( Teng > thisAmbientTemp ) {
					MdotFuelWarmup = MdotFuelMax + MicroCHP( GeneratorNum ).A42Model.kf * MdotFuelMax * ( ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp - thisAmbientTemp ) / ( Teng - thisAmbientTemp ) );
					// check that numerical answer didn't blow up beyond limit, and reset if it did
					if ( MdotFuelWarmup > MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax ) {
						MdotFuelWarmup = MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax;
					}
				} else if ( Teng < thisAmbientTemp ) {
					MdotFuelWarmup = MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax;
				} else { // equal would divide by zero
					MdotFuelWarmup = MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax;
				}

				if ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp > thisAmbientTemp ) {
					Pnetss = Pmax * MicroCHP( GeneratorNum ).A42Model.kp * ( ( Teng - thisAmbientTemp ) / ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp - thisAmbientTemp ) );
				} else if ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp < thisAmbientTemp ) {
					Pnetss = Pmax;
				} else { // equal would divide by zero
					Pnetss = Pmax;
				}

				//if ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp < thisAmbientTemp ) {
					//this case where zone is super hot and more than engine op. temp.
					//  never going to get here because E+ zones don't like to be over 50C. (and no cogen devices should operate below 50C)
				//}

				MdotFuel = MdotFuelWarmup;
				NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
				MdotAir = CurveValue( MicroCHP( GeneratorNum ).A42Model.AirFlowCurveID, MdotFuelWarmup );
				MdotAir = max( 0.0, MdotAir ); //protect against bad curve result
				Qgross = NdotFuel * ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 );
				ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, Pmax, MdotCW, TcwIn );
				Qgenss = ThermEff * Qgross; //W

			}
			NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;

		} else if ( SELECT_CASE_var == OpModeNormal ) {
			if ( PLRforSubtimestepStartUp < 1.0 ) {
				if ( RunFlagElectCenter ) Pnetss = MyElectricLoad; // W
				if ( RunFlagPlant ) Pnetss = AllowedLoad;
			} else {
				Pnetss = AllowedLoad;
			}
			Pstandby = 0.0;
			Pcooler = 0.0;
			TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
			MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
			if ( MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {
				MdotCW = FuncDetermineCWMdotForInternalFlowControl( GeneratorNum, Pnetss, TcwIn );
			}

			ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn );
			ElecEff = max( 0.0, ElecEff ); //protect against bad curve result

			if ( ElecEff > 0.0 ) { // trap divide by bad thing
				Qgross = Pnetss / ElecEff; //W
			} else {
				Qgross = 0.0;
			}

			ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn );
			ThermEff = max( 0.0, ThermEff ); //protect against bad curve result
			Qgenss = ThermEff * Qgross; //W
			MdotFuel = Qgross / ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 ) * FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
			//  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
			ManageGeneratorFuelFlow( iGeneratorMicroCHP, MicroCHP( GeneratorNum ).Name, GeneratorNum, RunFlag, MdotFuel, MdotFuelAllowed, ConstrainedIncreasingNdot, ConstrainedDecreasingNdot );

			if ( ConstrainedIncreasingNdot || ConstrainedDecreasingNdot ) { // recalculate Pnetss with new NdotFuel with iteration
				MdotFuel = MdotFuelAllowed;
				NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
				Qgross = NdotFuel * ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 );

				for ( i = 1; i <= 20; ++i ) { // iterating here,  could add use of seach method error signal
					Pnetss = Qgross * ElecEff;
					if ( MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {
						MdotCW = FuncDetermineCWMdotForInternalFlowControl( GeneratorNum, Pnetss, TcwIn );
					}
					ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn );
					ElecEff = max( 0.0, ElecEff ); //protect against bad curve result
				}

				ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn );
				ThermEff = max( 0.0, ThermEff ); //protect against bad curve result
				Qgenss = ThermEff * Qgross; //W

			}

			NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
			MdotAir = CurveValue( MicroCHP( GeneratorNum ).A42Model.AirFlowCurveID, MdotFuel );
			MdotAir = max( 0.0, MdotAir ); //protect against bad curve result
			if ( PLRforSubtimestepStartUp < 1.0 ) {
				Pnetss = AllowedLoad;
			}

		} else if ( SELECT_CASE_var == OpModeCoolDown ) {

			Pnetss = 0.0;
			Pstandby = 0.0;
			Pcooler = MicroCHP( GeneratorNum ).A42Model.PcoolDown;
			TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
			MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
			if ( MicroCHP( GeneratorNum ).A42Model.InternalFlowControl ) {
				MdotCW = FuncDetermineCWMdotForInternalFlowControl( GeneratorNum, Pnetss, TcwIn );
			}
			NdotFuel = 0.0;
			MdotFuel = 0.0;
			MdotAir = 0.0;
			ElecEff = 0.0;
			ThermEff = 0.0;
			Qgross = 0.0;
			Qgenss = 0.0;
		}}

		EnergyBalOK = false;
		for ( i = 1; i <= 20; ++i ) { // sequential search with exit criteria
			// calculate new value for engine temperature
			// for Stirling in warmup, need to include dependency of Qgness on Teng
			if ( ( MicroCHP( GeneratorNum ).A42Model.WarmUpByEngineTemp ) && ( CurrentOpMode == OpModeWarmUp ) ) {

				Pmax = MicroCHP( GeneratorNum ).A42Model.MaxElecPower;
				TcwIn = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).Temp; //C
				MdotCW = Node( MicroCHP( GeneratorNum ).PlantInletNodeID ).MassFlowRate; //kg/s
				ElecEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ElecEffCurveID, Pmax, MdotCW, TcwIn );
				ElecEff = max( 0.0, ElecEff ); //protect against bad curve result
				if ( ElecEff > 0.0 ) { // trap divide by bad thing
					Qgross = Pmax / ElecEff; //W
				} else {
					Qgross = 0.0;
				}
				NdotFuel = Qgross / ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 );
				//  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
				MdotFuelMax = NdotFuel * FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;

				if ( Teng > thisAmbientTemp ) {
					MdotFuelWarmup = MdotFuelMax + MicroCHP( GeneratorNum ).A42Model.kf * MdotFuelMax * ( ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp - thisAmbientTemp ) / ( Teng - thisAmbientTemp ) );

					// check that numerical answer didn't blow up beyond limit, and reset if it did
					if ( MdotFuelWarmup > MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax ) {
						MdotFuelWarmup = MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax;
					}
					if ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp > thisAmbientTemp ) {
						Pnetss = Pmax * MicroCHP( GeneratorNum ).A42Model.kp * ( ( Teng - thisAmbientTemp ) / ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp - thisAmbientTemp ) );
					} else if ( MicroCHP( GeneratorNum ).A42Model.TnomEngOp < thisAmbientTemp ) {
						Pnetss = Pmax;
					} else { // equal would divide by zero
						Pnetss = Pmax;
					}

				} else if ( Teng < thisAmbientTemp ) {
					MdotFuelWarmup = MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax;
				} else { // equal would divide by zero
					MdotFuelWarmup = MicroCHP( GeneratorNum ).A42Model.Rfuelwarmup * MdotFuelMax;
				}
				MdotFuel = MdotFuelWarmup;
				NdotFuel = MdotFuel / FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).KmolPerSecToKgPerSec;
				MdotAir = CurveValue( MicroCHP( GeneratorNum ).A42Model.AirFlowCurveID, MdotFuelWarmup );
				MdotAir = max( 0.0, MdotAir ); //protect against bad curve result
				Qgross = NdotFuel * ( FuelSupply( MicroCHP( GeneratorNum ).FuelSupplyID ).LHV * 1000.0 * 1000.0 );
				ThermEff = CurveValue( MicroCHP( GeneratorNum ).A42Model.ThermalEffCurveID, Pmax, MdotCW, TcwIn );
				ThermEff = max( 0.0, ThermEff ); //protect against bad curve result
				Qgenss = ThermEff * Qgross; //W
			}

			Teng = FuncDetermineEngineTemp( TcwOut, MicroCHP( GeneratorNum ).A42Model.MCeng, MicroCHP( GeneratorNum ).A42Model.UAhx, MicroCHP( GeneratorNum ).A42Model.UAskin, thisAmbientTemp, Qgenss, MicroCHP( GeneratorNum ).A42Model.TengLast, dt );

			Cp = GetSpecificHeatGlycol( PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).FluidName, TcwIn, PlantLoop( MicroCHP( GeneratorNum ).CWLoopNum ).FluidIndex, RoutineName );

			TcwOut = FuncDetermineCoolantWaterExitTemp( TcwIn, MicroCHP( GeneratorNum ).A42Model.MCcw, MicroCHP( GeneratorNum ).A42Model.UAhx, MdotCW * Cp, Teng, MicroCHP( GeneratorNum ).A42Model.TempCWOutLast, dt );

			// form balance and exit once met.
			EnergyBalOK = CheckMicroCHPThermalBalance( MicroCHP( GeneratorNum ).A42Model.MaxElecPower, TcwIn, TcwOut, Teng, thisAmbientTemp, MicroCHP( GeneratorNum ).A42Model.UAhx, MicroCHP( GeneratorNum ).A42Model.UAskin, Qgenss, MicroCHP( GeneratorNum ).A42Model.MCeng, MicroCHP( GeneratorNum ).A42Model.MCcw, MdotCW * Cp );

			if ( EnergyBalOK && ( i > 4 ) ) break;

		}

		MicroCHP( GeneratorNum ).PlantMassFlowRate = MdotCW;
		MicroCHP( GeneratorNum ).A42Model.Pnet = Pnetss - Pcooler - Pstandby;
		MicroCHP( GeneratorNum ).A42Model.ElecEff = ElecEff;
		MicroCHP( GeneratorNum ).A42Model.Qgross = Qgross;
		MicroCHP( GeneratorNum ).A42Model.ThermEff = ThermEff;
		MicroCHP( GeneratorNum ).A42Model.Qgenss = Qgenss;
		MicroCHP( GeneratorNum ).A42Model.NdotFuel = NdotFuel;
		MicroCHP( GeneratorNum ).A42Model.MdotFuel = MdotFuel;
		MicroCHP( GeneratorNum ).A42Model.Teng = Teng;
		MicroCHP( GeneratorNum ).A42Model.TcwOut = TcwOut;
		MicroCHP( GeneratorNum ).A42Model.TcwIn = TcwIn;
		MicroCHP( GeneratorNum ).A42Model.MdotAir = MdotAir;
		MicroCHP( GeneratorNum ).A42Model.QdotSkin = MicroCHP( GeneratorNum ).A42Model.UAskin * ( Teng - thisAmbientTemp );

		MicroCHP( GeneratorNum ).A42Model.OpMode = CurrentOpMode;

	}

	Real64
	FuncDetermineEngineTemp(
		Real64 const TcwOut, // hot water leaving temp
		Real64 const MCeng, // Fictitious mass and heat capacity of engine
		Real64 const UAHX, // Heat exchanger UA
		Real64 const UAskin, // Skin losses UA
		Real64 const Troom, // surrounding zone temperature C
		Real64 const Qgenss, // steady state generator heat generation
		Real64 const TengLast, // engine temp at previous time step
		Real64 const time // elapsed time since previous evaluation
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate engine temperaure,

		// METHODOLOGY EMPLOYED:
		// model is dynamic in that previous condition affects current timestep
		//  solve ode for engine temp using analytical solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 FuncDetermineEngineTemp;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 a;
		Real64 b;

		a = ( ( UAHX * TcwOut / MCeng ) + ( UAskin * Troom / MCeng ) + ( Qgenss / MCeng ) );
		b = ( ( -1.0 * UAHX / MCeng ) + ( -1.0 * UAskin / MCeng ) );

		FuncDetermineEngineTemp = ( TengLast + a / b ) * std::exp( b * time ) - a / b;

		return FuncDetermineEngineTemp;

	}

	Real64
	FuncDetermineCoolantWaterExitTemp(
		Real64 const TcwIn, // hot water inlet temp
		Real64 const MCcw, // Fictitious mass and heat capacity of coolant hx
		Real64 const UAHX, // Heat exchanger UA
		Real64 const MdotCpcw, // mass flow and specific heat of coolant water
		Real64 const Teng, // engine mass temperature C
		Real64 const TcwoutLast, // coolant water leaving temp at previous time step
		Real64 const time // elapsed time since previous evaluation
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate coolan water leaving temperaure,

		// METHODOLOGY EMPLOYED:
		// model is dynamic in that previous condition affects current timestep
		//  solve ode for coolant water outlet temp using analytical solution

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::MaxEXPArg;

		// Return value
		Real64 FuncDetermineCoolantWaterExitTemp;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 a;
		Real64 b;

		a = ( MdotCpcw * TcwIn / MCcw ) + ( UAHX * Teng / MCcw );
		b = ( ( -1.0 * MdotCpcw / MCcw ) + ( -1.0 * UAHX / MCcw ) );

		if ( b * time < ( -1.0 * MaxEXPArg ) ) {

			FuncDetermineCoolantWaterExitTemp = -a / b;
		} else {

			FuncDetermineCoolantWaterExitTemp = ( TcwoutLast + a / b ) * std::exp( b * time ) - a / b;
		}
		return FuncDetermineCoolantWaterExitTemp;

	}

	bool
	CheckMicroCHPThermalBalance(
		Real64 const NomHeatGen, // nominal heat generation rate for scaling
		Real64 const TcwIn, // hot water inlet temp
		Real64 const TcwOut, // hot water leaving temp
		Real64 const Teng, // engine mass temperature C
		Real64 const Troom, // surrounding zone temperature C
		Real64 const UAHX, // Heat exchanger UA
		Real64 const UAskin, // Skin losses UA
		Real64 const Qgenss, // steady state generator heat generation
		Real64 const MCeng, // Fictitious mass and heat capacity of engine
		Real64 const MCcw, // Fictitious mass and heat capacity of coolant hx
		Real64 const MdotCpcw // mass flow and specific heat of coolant water
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Check for energy balance to test if can exit iteration loop

		// METHODOLOGY EMPLOYED:
		// put all terms of dynamic energy balances on RHS and compute magnitude of imbalance
		//  compare imbalance to scalable thresholds and make a boolean conclusion.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckMicroCHPThermalBalance;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 a; // working variable, "a" term in generic ODE
		Real64 b; // working variable "b" term in generic ODE
		Real64 DTengDTime; // derivative of engine temp wrt time
		Real64 DCoolOutTDtime; // derivative of coolant exit temp wrt time
		Real64 magImbalEng; // energy imbalance for engine control volume
		Real64 magImbalCooling; // energy imbalance for coolant control volume
		Real64 threshold; // criteria for when to call energy balance okay
		//first compute derivatives using a + bT

		a = ( ( UAHX * TcwOut / MCeng ) + ( UAskin * Troom / MCeng ) + ( Qgenss / MCeng ) );
		b = ( ( -1.0 * UAHX / MCeng ) + ( -1.0 * UAskin / MCeng ) );
		DTengDTime = a + b * Teng;

		a = ( MdotCpcw * TcwIn / MCcw ) + ( UAHX * Teng / MCcw );
		b = ( ( -1.0 * MdotCpcw / MCcw ) + ( -1.0 * UAHX / MCcw ) );
		DCoolOutTDtime = a + b * TcwOut;

		magImbalEng = UAHX * ( TcwOut - Teng ) + UAskin * ( Troom - Teng ) + Qgenss - MCeng * DTengDTime;

		magImbalCooling = MdotCpcw * ( TcwIn - TcwOut ) + UAHX * ( Teng - TcwOut ) - MCcw * DCoolOutTDtime;

		threshold = NomHeatGen / 10000000.0;

		CheckMicroCHPThermalBalance = false;

		if ( ( threshold > magImbalEng ) && ( threshold > magImbalCooling ) ) {
			CheckMicroCHPThermalBalance = true;
		}

		return CheckMicroCHPThermalBalance;

	}

	void
	FigureMicroCHPZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Couple equiment skin losses to the Zone Heat Balance

		// METHODOLOGY EMPLOYED:
		// This routine adds up the various skin losses and then
		//  sets the values in the ZoneIntGain structure

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER :: thisZone ! index in Zone structure array
		Real64 TotalZoneHeatGain;
		int CHPnum;
		//  INTEGER :: ZoneNum
		static bool MyEnvrnFlag( true );

		if ( NumMicroCHPs == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( auto & e : FuelSupply ) e.QskinLoss = 0.0;
			for ( auto & e : MicroCHP ) {
				e.A42Model.QdotSkin = 0.0;
				e.Report.SkinLossConvect = 0.0;
				e.Report.SkinLossRadiat = 0.0;
			}
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		for ( CHPnum = 1; CHPnum <= NumMicroCHPs; ++CHPnum ) {
			TotalZoneHeatGain = FuelSupply( MicroCHP( CHPnum ).FuelSupplyID ).QskinLoss + MicroCHP( CHPnum ).A42Model.QdotSkin;

			MicroCHP( CHPnum ).A42Model.QdotConvZone = TotalZoneHeatGain * ( 1 - MicroCHP( CHPnum ).A42Model.RadiativeFraction );
			MicroCHP( CHPnum ).Report.SkinLossConvect = MicroCHP( CHPnum ).A42Model.QdotConvZone;
			MicroCHP( CHPnum ).A42Model.QdotRadZone = TotalZoneHeatGain * MicroCHP( CHPnum ).A42Model.RadiativeFraction;
			MicroCHP( CHPnum ).Report.SkinLossRadiat = MicroCHP( CHPnum ).A42Model.QdotRadZone;
		}

		// this routine needs to do something for zone gains during sizing
		//  IF(DoingSizing)THEN

		//  ENDIF

	}

	void
	CalcUpdateHeatRecovery(
		int const Num, // Generator number
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Aug 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update plant loop interactions, do any calcs needed

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcUpdateHeatRecovery" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNodeNum;
		int OutNodeNum;
		Real64 Cp; // local Specific heat of fluid
		//  REAL(r64) :: mdot !local mass flow rate

		// now update water outlet node Changing to Kg/s!
		OutNodeNum = MicroCHP( Num ).PlantOutletNodeID;
		InNodeNum = MicroCHP( Num ).PlantInletNodeID;

		SafeCopyPlantNode( InNodeNum, OutNodeNum );

		Node( OutNodeNum ).Temp = MicroCHP( Num ).A42Model.TcwOut;

		Cp = GetSpecificHeatGlycol( PlantLoop( MicroCHP( Num ).CWLoopNum ).FluidName, MicroCHP( Num ).A42Model.TcwIn, PlantLoop( MicroCHP( Num ).CWLoopNum ).FluidIndex, RoutineName );

		Node( OutNodeNum ).Enthalpy = MicroCHP( Num ).A42Model.TcwOut * Cp;

	}

	void
	SimMicroCHPPlantHeatRecovery(
		std::string const & EP_UNUSED( CompType ),
		std::string const & CompName,
		int & CompNum,
		bool const EP_UNUSED( RunFlag ),
		bool & InitLoopEquip,
		Real64 & EP_UNUSED( MyThermalLoad ),
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// makes sure input are gotten and setup from Plant loop perspective.
		// does not (re)simulate entire MicroCHP model

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using namespace DataGlobalConstants;
		using PlantUtilities::UpdateComponentHeatRecoverySide;
		using DataPlant::TypeOf_Generator_MicroCHP;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN)          :: FlowLock !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetMicroCHPInput ) {

			// Read input data.
			GetMicroCHPGeneratorInput();
			GetMicroCHPInput = false;
		}

		if ( InitLoopEquip ) {
			CompNum = FindItemInList( CompName, MicroCHP );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimMicroCHPPlantHeatRecovery: MicroCHP Generator Unit not found=" + CompName );
				return;
			}
			InitMicroCHPNoNormalizeGenerators( CompNum, FirstHVACIteration );
			if ( MySizeFlag( CompNum ) ) return;
			MinCap = GeneratorDynamics( MicroCHP( CompNum ).DynamicsControlID ).QdotHXMin;
			MaxCap = GeneratorDynamics( MicroCHP( CompNum ).DynamicsControlID ).QdotHXMax;
			OptCap = GeneratorDynamics( MicroCHP( CompNum ).DynamicsControlID ).QdotHXOpt;
			return;
		} // End Of InitLoopEquip

		UpdateComponentHeatRecoverySide( MicroCHP( CompNum ).CWLoopNum, MicroCHP( CompNum ).CWLoopSideNum, TypeOf_Generator_MicroCHP, MicroCHP( CompNum ).PlantInletNodeID, MicroCHP( CompNum ).PlantOutletNodeID, MicroCHP( CompNum ).Report.QdotHR, MicroCHP( CompNum ).Report.HeatRecInletTemp, MicroCHP( CompNum ).Report.HeatRecOutletTemp, MicroCHP( CompNum ).Report.HeatRecMdot, FirstHVACIteration );

	}

	void
	UpdateMicroCHPGeneratorRecords( int const Num ) // Generator number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update variables in structures linked to output reports

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UpdateMicroCHPGeneratorRecords" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Cp; // local fluid specific heat

		// na
		MicroCHP( Num ).Report.Mode = MicroCHP( Num ).A42Model.OpMode;
		MicroCHP( Num ).Report.OffModeTime = MicroCHP( Num ).A42Model.OffModeTime;
		MicroCHP( Num ).Report.StandyByModeTime = MicroCHP( Num ).A42Model.StandyByModeTime;
		MicroCHP( Num ).Report.WarmUpModeTime = MicroCHP( Num ).A42Model.WarmUpModeTime;
		MicroCHP( Num ).Report.NormalModeTime = MicroCHP( Num ).A42Model.NormalModeTime;
		MicroCHP( Num ).Report.CoolDownModeTime = MicroCHP( Num ).A42Model.CoolDownModeTime;

		MicroCHP( Num ).Report.ACPowerGen = MicroCHP( Num ).A42Model.Pnet; //electrical power produced [W]
		MicroCHP( Num ).Report.ACEnergyGen = MicroCHP( Num ).A42Model.Pnet * TimeStepSys * SecInHour; // energy produced (J)
		MicroCHP( Num ).Report.QdotGross = MicroCHP( Num ).A42Model.Qgross;
		MicroCHP( Num ).Report.Qgenss = MicroCHP( Num ).A42Model.Qgenss;
		MicroCHP( Num ).Report.QdotHX = MicroCHP( Num ).A42Model.UAhx * ( MicroCHP( Num ).A42Model.Teng - MicroCHP( Num ).A42Model.TcwOut ); //  heat recovered rate (W)

		Cp = GetSpecificHeatGlycol( PlantLoop( MicroCHP( Num ).CWLoopNum ).FluidName, MicroCHP( Num ).A42Model.TcwIn, PlantLoop( MicroCHP( Num ).CWLoopNum ).FluidIndex, RoutineName );

		MicroCHP( Num ).Report.QdotHR = MicroCHP( Num ).PlantMassFlowRate * Cp * ( MicroCHP( Num ).A42Model.TcwOut - MicroCHP( Num ).A42Model.TcwIn );
		MicroCHP( Num ).Report.TotalHeatEnergyRec = MicroCHP( Num ).Report.QdotHR * TimeStepSys * SecInHour; // heat recovered energy (J)

		MicroCHP( Num ).Report.HeatRecInletTemp = MicroCHP( Num ).A42Model.TcwIn; // Heat Recovery Loop Inlet Temperature (C)
		MicroCHP( Num ).Report.HeatRecOutletTemp = MicroCHP( Num ).A42Model.TcwOut; // Heat Recovery Loop Outlet Temperature (C)
		MicroCHP( Num ).Report.HeatRecMdot = MicroCHP( Num ).PlantMassFlowRate; // Heat Recovery Loop Mass flow rate (kg/s)
		MicroCHP( Num ).Report.Tengine = MicroCHP( Num ).A42Model.Teng;
		MicroCHP( Num ).Report.ElectEfficiency = MicroCHP( Num ).A42Model.ElecEff;
		MicroCHP( Num ).Report.ThermalEfficiency = MicroCHP( Num ).A42Model.ThermEff;

		MicroCHP( Num ).Report.OverallEfficiency = MicroCHP( Num ).A42Model.ElecEff + MicroCHP( Num ).A42Model.ThermEff;

		MicroCHP( Num ).Report.MdotAir = MicroCHP( Num ).A42Model.MdotAir; // air flow in kg/sec

		MicroCHP( Num ).Report.NdotFuel = MicroCHP( Num ).A42Model.NdotFuel; // fuel flow in kmol/sec
		MicroCHP( Num ).Report.MdotFuel = MicroCHP( Num ).A42Model.MdotFuel; // fuel flow in kg/sec

		MicroCHP( Num ).Report.FuelCompressPower = FuelSupply( MicroCHP( Num ).FuelSupplyID ).PfuelCompEl;
		// electrical power used by fuel supply compressor [W]
		MicroCHP( Num ).Report.FuelCompressEnergy = FuelSupply( MicroCHP( Num ).FuelSupplyID ).PfuelCompEl * TimeStepSys * SecInHour; // elect energy
		MicroCHP( Num ).Report.FuelCompressSkinLoss = FuelSupply( MicroCHP( Num ).FuelSupplyID ).QskinLoss;
		//heat rate of losses.by fuel supply compressor [W]
		MicroCHP( Num ).Report.FuelEnergyHHV = MicroCHP( Num ).A42Model.NdotFuel * FuelSupply( MicroCHP( Num ).FuelSupplyID ).HHV * FuelSupply( MicroCHP( Num ).FuelSupplyID ).KmolPerSecToKgPerSec * TimeStepSys * SecInHour;
		// reporting: Fuel Energy used (W)
		MicroCHP( Num ).Report.FuelEnergyUseRateHHV = MicroCHP( Num ).A42Model.NdotFuel * FuelSupply( MicroCHP( Num ).FuelSupplyID ).HHV * FuelSupply( MicroCHP( Num ).FuelSupplyID ).KmolPerSecToKgPerSec;
		// reporting: Fuel Energy used (J)
		MicroCHP( Num ).Report.FuelEnergyLHV = MicroCHP( Num ).A42Model.NdotFuel * FuelSupply( MicroCHP( Num ).FuelSupplyID ).LHV * 1000000.0 * TimeStepSys * SecInHour;
		// reporting: Fuel Energy used (W)
		MicroCHP( Num ).Report.FuelEnergyUseRateLHV = MicroCHP( Num ).A42Model.NdotFuel * FuelSupply( MicroCHP( Num ).FuelSupplyID ).LHV * 1000000.0;

		MicroCHP( Num ).Report.SkinLossPower = MicroCHP( Num ).A42Model.QdotConvZone + MicroCHP( Num ).A42Model.QdotRadZone;
		MicroCHP( Num ).Report.SkinLossEnergy = ( MicroCHP( Num ).A42Model.QdotConvZone + MicroCHP( Num ).A42Model.QdotRadZone ) * TimeStepSys * SecInHour;
		MicroCHP( Num ).Report.SkinLossConvect = MicroCHP( Num ).A42Model.QdotConvZone;
		MicroCHP( Num ).Report.SkinLossRadiat = MicroCHP( Num ).A42Model.QdotRadZone;

		// update node data for air inlet (and outlet)
		if ( MicroCHP( Num ).AirInletNodeID > 0 ) {
			Node( MicroCHP( Num ).AirInletNodeID ).MassFlowRate = MicroCHP( Num ).Report.MdotAir;
		}
		if ( MicroCHP( Num ).AirOutletNodeID > 0 ) {
			Node( MicroCHP( Num ).AirOutletNodeID ).MassFlowRate = MicroCHP( Num ).Report.MdotAir;
			Node( MicroCHP( Num ).AirOutletNodeID ).Temp = MicroCHP( Num ).A42Model.Teng;
		}

	}

	void
	GetMicroCHPGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// provide a get method to collect results at the load center level

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		GeneratorPower = MicroCHP( GeneratorIndex ).Report.ACPowerGen;
		GeneratorEnergy = MicroCHP( GeneratorIndex ).Report.ACEnergyGen;
		ThermalPower = MicroCHP( GeneratorIndex ).Report.QdotHR;
		ThermalEnergy = MicroCHP( GeneratorIndex ).Report.TotalHeatEnergyRec;

	}

} // MicroCHPElectricGenerator

} // EnergyPlus
