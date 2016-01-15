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
#include <OutdoorAirUnit.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <DataSurfaceLists.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DesiccantDehumidifiers.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HeatRecovery.hh>
#include <HVACDXHeatPumpSystem.hh>
#include <HVACDXSystem.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HVACUnitarySystem.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace OutdoorAirUnit {
	// Module containing the routines dealing with the outdoor air unit

	// MODULE INFORMATION:
	//       AUTHOR         Young Tae Chae, Rick Strand
	//       DATE WRITTEN   AUG. 2009
	//       MODIFIED
	//                      Feb 2013 Bereket Nigusse, FSEC
	//                        Added DX Coil Model For 100% OA systems
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Simulate zone outdooor air unit.

	// METHODOLOGY EMPLOYED:
	// Systems are modeled as a collection of components:
	// fan, heat recovery, dehumidifier, heating coil and/or cooling coil plus an integrated control
	// algorithm that adjusts the hot or cold water flow to meet the setpoint
	// condition.

	// REFERENCES:
	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::ZoneSizingCalc;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::DrawThru;
	using DataHVACGlobals::BlowThru;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using namespace Psychrometrics;
	using namespace FluidProperties;
	using General::TrimSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS

	// component types addressed by this module
	std::string const cMO_OutdoorAirUnit( "ZoneHVAC:OutdoorAirUnit" );

	int const WaterCoil_SimpleCool( 1 );
	int const WaterCoil_Cooling( 2 );
	int const WaterCoil_SimpleHeat( 3 );
	int const SteamCoil_AirHeat( 4 );
	int const WaterCoil_DetailedCool( 5 );
	int const WaterCoil_CoolingHXAsst( 6 );
	int const Coil_ElectricHeat( 7 );
	int const Coil_GasHeat( 8 );
	int const DXSystem( 9 );
	int const HeatXchngr( 10 );
	int const Desiccant( 11 );
	int const DXHeatPumpSystem( 12 );
	int const UnitarySystem( 13 );

	//  Control Types
	int const Neutral( 1 ); // Controls system using zone mean air temperature
	int const Unconditioned( 2 ); // Controls system when outdoor air temperature is identified with control temperature
	int const Temperature( 3 ); // Controls system using temperature band

	// Operating Options
	int const HeatingMode( 1 ); // normal heating coil operation
	int const CoolingMode( 2 ); // normal cooling coil operation
	int const NeutralMode( 3 ); // signal coil shouldn't run

	Array1D_string const CurrentModuleObjects( 2, { "ZoneHVAC:OutdoorAirUnit", "ZoneHVAC:OutdoorAirUnit:EquipmentList" } );

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );
	static std::string const BlankString;

	// Parameters below (CO - Current module Object.  used primarily in Get Inputs)
	// Multiple Get Input routines in this module or these would be in individual routines.
	int const CO_OAUnit( 1 );
	int const CO_OAEqList( 2 );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumOfOAUnits( 0 ); // Number of outdoor air unit in the input file
	Real64 OAMassFlowRate( 0.0 ); // Outside air mass flow rate for the zone outdoor air unit
	bool GetOutdoorAirUnitInputFlag( true ); // Flag set to make sure you get input once

	// Autosizing variables
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;
	Array1D_bool MyOneTimeErrorFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE OUTDOOR AIR UNIT
	//PRIVATE UpdateOutdoorAirUnit
	//PUBLIC GetOutAirCoilOutletTemp

	// Object Data
	Array1D< OAUnitData > OutAirUnit;

	namespace {
		bool MyOneTimeFlag( true );
		bool ZoneEquipmentListChecked( false );
	}

	// Functions

	void
	clear_state()
	{
		NumOfOAUnits = 0;
		OAMassFlowRate = 0.0;
		GetOutdoorAirUnitInputFlag = true;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		MyOneTimeErrorFlag.deallocate();
		OutAirUnit.deallocate();
		MyOneTimeFlag = true;
		ZoneEquipmentListChecked = false;
	}

	void
	SimOutdoorAirUnit(
		std::string const & CompName, // name of the outdoor air unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED
		// This is re-engineered by Rick Strand and Young T. Chae for OutdoorAirUnit (July, 2009)

		// PURPOSE OF THIS SUBROUTINE:
		// This is the main driver subroutine for the outdoor air control unit simulation.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OAUnitNum; // index of outdoor air unit being simulated

		// FLOW:
		if ( GetOutdoorAirUnitInputFlag ) {
			GetOutdoorAirUnitInputs();
			GetOutdoorAirUnitInputFlag = false;
		}

		// Find the correct Outdoor Air Unit

		if ( CompIndex == 0 ) {
			OAUnitNum = FindItemInList( CompName, OutAirUnit );
			if ( OAUnitNum == 0 ) {
				ShowFatalError( "ZoneHVAC:OutdoorAirUnit not found=" + CompName );
			}
			CompIndex = OAUnitNum;
		} else {
			OAUnitNum = CompIndex;
			if ( OAUnitNum > NumOfOAUnits || OAUnitNum < 1 ) {
				ShowFatalError( "SimOutdoorAirUnit:  Invalid CompIndex passed=" + TrimSigDigits( OAUnitNum ) + ", Number of Units=" + TrimSigDigits( NumOfOAUnits ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( OAUnitNum ) ) {
				if ( CompName != OutAirUnit( OAUnitNum ).Name ) {
					ShowFatalError( "SimOutdoorAirUnit: Invalid CompIndex passed=" + TrimSigDigits( OAUnitNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + OutAirUnit( OAUnitNum ).Name );
				}
				CheckEquipName( OAUnitNum ) = false;
			}
		}

		if ( ZoneSizingCalc || SysSizingCalc ) return;

		InitOutdoorAirUnit( OAUnitNum, ZoneNum, FirstHVACIteration );

		CalcOutdoorAirUnit( OAUnitNum, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided );

		// CALL UpdateOutdoorAirUnit(OAUnitNum, FirstHVACIteration)

		ReportOutdoorAirUnit( OAUnitNum );

	}

	void
	GetOutdoorAirUnitInputs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   July 2009
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine obtains the input for the outdoor air control unit and sets
		// up the appropriate derived type.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// Fred Buhl's fan coil module (FanCoilUnits.cc)
		// Kwang Ho Lee's Unit Ventilator Module (UnitVentilator.cc)
		// Young Tae Chae's Ventilated Slab System (VentilatedSlab.cc)
		// Mixed Air.cc

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		auto & GetWaterCoilMaxFlowRate( WaterCoils::GetCoilMaxWaterFlowRate );
		using WaterCoils::CheckWaterCoilSchedule;
		using SteamCoils::GetCoilAirInletNode;
		using SteamCoils::GetCoilAirOutletNode;
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilSteamInletNode;
		using FluidProperties::FindRefrigerant;
		using DataGlobals::ScheduleAlwaysOn;
		using DataHeatBalance::Zone;
		using DataSizing::AutoSize;
		using ScheduleManager::GetScheduleIndex;
		using namespace DataLoopNode;
		using namespace DataSurfaceLists;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetWaterCoilIndex;
		auto & GetWCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWCoilOutletNode( WaterCoils::GetCoilOutletNode );
		using WaterCoils::GetCoilWaterOutletNode;
		using HeatingCoils::GetCoilInletNode;
		using HeatingCoils::GetCoilOutletNode;
		auto & GetHeatingCoilIndex( HeatingCoils::GetCoilIndex );
		auto & GetElecCoilInletNode( HeatingCoils::GetCoilInletNode );
		auto & GetElecCoilOutletNode( HeatingCoils::GetCoilOutletNode );
		auto & GetHXAssistedCoilFlowRate( HVACHXAssistedCoolingCoil::GetCoilMaxWaterFlowRate );
		auto & GetWHXCoilInletNode( HVACHXAssistedCoolingCoil::GetCoilInletNode );
		auto & GetWHXCoilOutletNode( HVACHXAssistedCoolingCoil::GetCoilOutletNode );
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using Fans::GetFanIndex;
		using Fans::GetFanType;
		using Fans::GetFanAvailSchPtr;
		using Fans::GetFanDesignVolumeFlowRate;
		using DataHVACGlobals::cFanTypes;
		using HVACDXSystem::CheckDXCoolingCoilInOASysExists;
		using HVACUnitarySystem::CheckUnitarySysCoilInOASysExists;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOutdoorAirUnitInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int NumNums; // Number of real numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int IOStat;
		int OAUnitNum;
		int CompNum;
		std::string ComponentListName;
		int NumInList;
		int InListNum;
		int ListNum;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		bool IsValid; // Set for outside air node check
		Array1D_string cAlphaArgs; // Alpha input items for object
		std::string CurrentModuleObject; // Object type for getting and messages
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		Array1D< Real64 > NumArray;
		Array1D_string AlphArray;
		static bool errFlag( false );

		// FLOW:
		// Figure out how many outdoor air units there are in the input file

		if ( ! GetOutdoorAirUnitInputFlag ) return;

		GetObjectDefMaxArgs( CurrentModuleObjects( CO_OAUnit ), TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( CurrentModuleObjects( CO_OAEqList ), TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		AlphArray.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		NumArray.dimension( MaxNums, 0.0 );
		cNumericFields.allocate( MaxNums );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );
		cAlphaArgs.allocate( NumAlphas );

		CurrentModuleObject = CurrentModuleObjects( CO_OAUnit );
		NumOfOAUnits = GetNumObjectsFound( CurrentModuleObject );

		OutAirUnit.allocate( NumOfOAUnits );
		MyOneTimeErrorFlag.dimension( NumOfOAUnits, true );
		CheckEquipName.dimension( NumOfOAUnits, true );

		for ( OAUnitNum = 1; OAUnitNum <= NumOfOAUnits; ++OAUnitNum ) {

			GetObjectItem( CurrentModuleObject, OAUnitNum, cAlphaArgs, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), OutAirUnit, OAUnitNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			//A1
			OutAirUnit( OAUnitNum ).Name = cAlphaArgs( 1 );
			IsNotOK = false;
			IsBlank = false;

			//A2
			OutAirUnit( OAUnitNum ).SchedName = cAlphaArgs( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				OutAirUnit( OAUnitNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				OutAirUnit( OAUnitNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) ); // convert schedule name to pointer
				if ( OutAirUnit( OAUnitNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaArgs( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			//A3
			OutAirUnit( OAUnitNum ).ZoneName = cAlphaArgs( 3 );
			OutAirUnit( OAUnitNum ).ZonePtr = FindItemInList( cAlphaArgs( 3 ), Zone );

			if ( OutAirUnit( OAUnitNum ).ZonePtr == 0 ) {
				if ( lAlphaBlanks( 3 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaArgs( 3 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaArgs( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
				}
				ErrorsFound = true;
			}
			OutAirUnit( OAUnitNum ).ZoneNodeNum = Zone( OutAirUnit( OAUnitNum ).ZonePtr ).SystemZoneNodeNumber;
			// Outside air information:
			//N1
			OutAirUnit( OAUnitNum ).OutAirVolFlow = NumArray( 1 );
			//A4
			OutAirUnit( OAUnitNum ).OutAirSchedName = cAlphaArgs( 4 );
			// convert schedule name to pointer
			OutAirUnit( OAUnitNum ).OutAirSchedPtr = GetScheduleIndex( OutAirUnit( OAUnitNum ).OutAirSchedName );
			if ( OutAirUnit( OAUnitNum ).OutAirSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" not found." );
				ErrorsFound = true;
			}

			//A5
			OutAirUnit( OAUnitNum ).SFanName = cAlphaArgs( 5 );
			VerifyName( cAlphaArgs( 5 ), OutAirUnit, &OAUnitData::SFanName, OAUnitNum - 1, IsNotOK, IsBlank, "OA Unit Supply Fan Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 5 ) = "xxxxx";
			}
			errFlag = false;
			GetFanType( OutAirUnit( OAUnitNum ).SFanName, OutAirUnit( OAUnitNum ).SFanType, errFlag, CurrentModuleObject, OutAirUnit( OAUnitNum ).Name );
			OutAirUnit( OAUnitNum ).SFanMaxAirVolFlow = GetFanDesignVolumeFlowRate( cFanTypes( OutAirUnit( OAUnitNum ).SFanType ), OutAirUnit( OAUnitNum ).SFanName, errFlag );
			if( !errFlag ) {
				OutAirUnit( OAUnitNum ).SFanAvailSchedPtr = GetFanAvailSchPtr( cFanTypes( OutAirUnit( OAUnitNum ).SFanType ), OutAirUnit( OAUnitNum ).SFanName, errFlag );
				// get fan index
				GetFanIndex( OutAirUnit( OAUnitNum ).SFanName, OutAirUnit( OAUnitNum ).SFan_Index, ErrorsFound );
			} else {
				ErrorsFound = true;
			}
			//A6 :Fan Place
			if ( SameString( cAlphaArgs( 6 ), "BlowThrough" ) ) OutAirUnit( OAUnitNum ).FanPlace = BlowThru;
			if ( SameString( cAlphaArgs( 6 ), "DrawThrough" ) ) OutAirUnit( OAUnitNum ).FanPlace = DrawThru;
			if ( OutAirUnit( OAUnitNum ).FanPlace == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFields( 6 ) + " = " + cAlphaArgs( 6 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			//A7

			if ( lAlphaBlanks( 7 ) ) {
				OutAirUnit( OAUnitNum ).ExtFan = false;
			} else if ( ! lAlphaBlanks( 7 ) ) {
				OutAirUnit( OAUnitNum ).ExtFanName = cAlphaArgs( 7 );
				VerifyName( cAlphaArgs( 7 ), OutAirUnit, &OAUnitData::ExtFanName, OAUnitNum - 1, IsNotOK, IsBlank, "OA Unit Exhaust Fan Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 7 ) = "xxxxx";
				}
				errFlag = false;
				GetFanType( OutAirUnit( OAUnitNum ).ExtFanName, OutAirUnit( OAUnitNum ).ExtFanType, errFlag, CurrentModuleObject, OutAirUnit( OAUnitNum ).Name );
				OutAirUnit( OAUnitNum ).EFanMaxAirVolFlow = GetFanDesignVolumeFlowRate( cFanTypes( OutAirUnit( OAUnitNum ).ExtFanType ), OutAirUnit( OAUnitNum ).ExtFanName, errFlag );
				if( !errFlag ) {
					OutAirUnit( OAUnitNum ).ExtFanAvailSchedPtr = GetFanAvailSchPtr( cFanTypes( OutAirUnit( OAUnitNum ).ExtFanType ), OutAirUnit( OAUnitNum ).ExtFanName, errFlag );
					// get fan index
					GetFanIndex( OutAirUnit( OAUnitNum ).ExtFanName, OutAirUnit( OAUnitNum ).ExtFan_Index, ErrorsFound );
				} else {
					ErrorsFound = true;
				}
				OutAirUnit( OAUnitNum ).ExtFan = true;
			}

			//N2
			OutAirUnit( OAUnitNum ).ExtAirVolFlow = NumArray( 2 );
			//A8
			OutAirUnit( OAUnitNum ).ExtAirSchedName = cAlphaArgs( 8 );
			// convert schedule name to pointer
			OutAirUnit( OAUnitNum ).ExtOutAirSchedPtr = GetScheduleIndex( OutAirUnit( OAUnitNum ).ExtAirSchedName );
			if ( OutAirUnit( OAUnitNum ).ExtFan ) {
				if ( ( OutAirUnit( OAUnitNum ).ExtOutAirSchedPtr == 0 ) || ( lNumericBlanks( 2 ) ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + cAlphaArgs( 8 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			if ( OutAirUnit( OAUnitNum ).ExtFan ) {

				SetUpCompSets( CurrentModuleObject, OutAirUnit( OAUnitNum ).Name, "UNDEFINED", cAlphaArgs( 7 ), "UNDEFINED", "UNDEFINED" );
			}

			// Process the unit control type

			if ( ! lAlphaBlanks( 9 ) ) {
				{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
				if ( SELECT_CASE_var == "NEUTRALCONTROL" ) {
					OutAirUnit( OAUnitNum ).ControlType = Neutral;
				} else if ( SELECT_CASE_var == "TEMPERATURECONTROL" ) {
					OutAirUnit( OAUnitNum ).ControlType = Temperature;
				}}
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 9 ) + "=\"" + cAlphaArgs( 9 ) + "\"." );
				ShowContinueError( "Control reset to Unconditioned Control." );
				OutAirUnit( OAUnitNum ).ControlType = Neutral;
			}

			//A10:High Control Temp :
			OutAirUnit( OAUnitNum ).HiCtrlTempSched = cAlphaArgs( 10 );
			OutAirUnit( OAUnitNum ).HiCtrlTempSchedPtr = GetScheduleIndex( cAlphaArgs( 10 ) );
			if ( ( OutAirUnit( OAUnitNum ).HiCtrlTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 10 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 10 ) + "=\"" + cAlphaArgs( 9 ) + "\" not found." );
				ErrorsFound = true;
			}

			//A11:Low Control Temp :
			OutAirUnit( OAUnitNum ).LoCtrlTempSched = cAlphaArgs( 11 );
			OutAirUnit( OAUnitNum ).LoCtrlTempSchedPtr = GetScheduleIndex( cAlphaArgs( 11 ) );
			if ( ( OutAirUnit( OAUnitNum ).LoCtrlTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 11 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 11 ) + "=\"" + cAlphaArgs( 10 ) + "\" not found." );
				ErrorsFound = true;
			}

			OutAirUnit( OAUnitNum ).CompOutSetTemp = 0.0;

			//A12~A15 : Node Condition

			// Main air nodes (except outside air node):

			OutAirUnit( OAUnitNum ).AirOutletNode = GetOnlySingleNode( cAlphaArgs( 13 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );
			if ( ! lAlphaBlanks( 14 ) ) {
				OutAirUnit( OAUnitNum ).AirInletNode = GetOnlySingleNode( cAlphaArgs( 14 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			} else {
				if ( OutAirUnit( OAUnitNum ).ExtFan ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 14 ) + " cannot be blank when there is an exhaust fan." );
					ErrorsFound = true;
				}
			}

			OutAirUnit( OAUnitNum ).SFanOutletNode = GetOnlySingleNode( cAlphaArgs( 15 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );

			//  Set connection type to 'OutdoorAir', because this is hardwired to OA conditions
			OutAirUnit( OAUnitNum ).OutsideAirNode = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );

			if ( ! lAlphaBlanks( 12 ) ) {
				CheckAndAddAirNodeNumber( OutAirUnit( OAUnitNum ).OutsideAirNode, IsValid );
				if ( ! IsValid ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Adding OutdoorAir:Node=" + cAlphaArgs( 12 ) );
				}

			}

			// When the fan position is "BlowThru", Each node is set up

			if ( OutAirUnit( OAUnitNum ).FanPlace == BlowThru ) {
				SetUpCompSets( CurrentModuleObject, OutAirUnit( OAUnitNum ).Name, "UNDEFINED", cAlphaArgs( 5 ), cAlphaArgs( 12 ), cAlphaArgs( 15 ) );
			}

			//A16 : component list
			VerifyName( cAlphaArgs( 16 ), OutAirUnit, &OAUnitData::ComponentListName, OAUnitNum - 1, IsNotOK, IsBlank, CurrentModuleObjects( CO_OAEqList ) + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 16 ) = "xxxxx";
			}
			ComponentListName = cAlphaArgs( 16 );
			OutAirUnit( OAUnitNum ).ComponentListName = ComponentListName;
			if ( ! lAlphaBlanks( 16 ) ) {
				ListNum = GetObjectItemNum( CurrentModuleObjects( CO_OAEqList ), ComponentListName );
				if ( ListNum > 0 ) {
					GetObjectItem( CurrentModuleObjects( CO_OAEqList ), ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
					NumInList = ( NumAlphas - 1 ) / 2; // potential problem if puts in type but not name
					if ( mod( NumAlphas - 1, 2 ) != 0 ) ++NumInList;
					OutAirUnit( OAUnitNum ).NumComponents = NumInList;
					OutAirUnit( OAUnitNum ).OAEquip.allocate( NumInList );

					// Get information of component
					for ( InListNum = 1; InListNum <= NumInList; ++InListNum ) {
						OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName = AlphArray( InListNum * 2 + 1 );
						OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType = AlphArray( InListNum * 2 );
						CompNum = InListNum;
						{ auto const SELECT_CASE_var( MakeUPPERCase( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType ) );
						// Coil Types
						if ( SELECT_CASE_var == "COIL:COOLING:WATER" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = WaterCoil_Cooling;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum = TypeOf_CoilWaterCooling;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex = GetWaterCoilIndex( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetWCoilInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetWCoilOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterInletNode = GetCoilWaterInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterOutletNode = GetCoilWaterOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow = GetWaterCoilMaxFlowRate( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MinVolWaterFlow = 0.0;

						} else if ( SELECT_CASE_var == "COIL:HEATING:WATER" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = WaterCoil_SimpleHeat;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum = TypeOf_CoilWaterSimpleHeating;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex = GetWaterCoilIndex( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetWCoilInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetWCoilOutletNode( "Coil:Heating:Water", OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterInletNode = GetCoilWaterInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterOutletNode = GetCoilWaterOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Heating:Water", OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MinVolWaterFlow = 0.0;

						} else if ( SELECT_CASE_var == "COIL:HEATING:STEAM" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = SteamCoil_AirHeat;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum = TypeOf_CoilSteamAirHeating;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex = GetSteamCoilIndex( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetCoilAirInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetCoilAirOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterInletNode = GetCoilSteamInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );

							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MinVolWaterFlow = 0.0;
							// below: no extra error needed if steam properties not in input
							// file because getting the steam coil will have done that.
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).FluidIndex = FindRefrigerant( "Steam" );

						} else if ( SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = WaterCoil_DetailedCool;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex = GetWaterCoilIndex( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum = TypeOf_CoilWaterDetailedFlatCooling;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetWCoilInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetWCoilOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterInletNode = GetCoilWaterInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterOutletNode = GetCoilWaterOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow = GetWaterCoilMaxFlowRate( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MinVolWaterFlow = 0.0;

						} else if ( SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = WaterCoil_CoolingHXAsst;
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetWHXCoilInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetWHXCoilOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterInletNode = GetCoilWaterInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilWaterOutletNode = GetCoilWaterOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow = GetHXAssistedCoilFlowRate( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MinVolWaterFlow = 0.0;

						} else if ( SELECT_CASE_var == "COIL:HEATING:ELECTRIC" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = Coil_ElectricHeat;
							// Get OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, 2 types of mining functions to choose from
							GetHeatingCoilIndex( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetElecCoilInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetElecCoilOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );

						} else if ( SELECT_CASE_var == "COIL:HEATING:GAS" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = Coil_GasHeat;
							// Get OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, 2 types of mining functions to choose from
							GetHeatingCoilIndex( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirInletNode = GetCoilInletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilAirOutletNode = GetCoilOutletNode( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, ErrorsFound );

						} else if ( SELECT_CASE_var == "COILSYSTEM:COOLING:DX" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = DXSystem;
							// set the data for 100% DOAS DX cooling coil
							CheckDXCoolingCoilInOASysExists( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName );

						} else if ( SELECT_CASE_var == "COILSYSTEM:HEATING:DX" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = DXHeatPumpSystem;

						} else if ( SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = UnitarySystem;
							CheckUnitarySysCoilInOASysExists( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName );

							// Heat recovery
						} else if ( SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = HeatXchngr;

						} else if ( SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = HeatXchngr;
							//        CASE('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
							//          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr

							// Desiccant Dehumidifier
						} else if ( SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:NOFANS" ) {
							OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType_Num = Desiccant;
							// Futher Enhancement
							//        CASE('DEHUMIDIFIER:DESICCANT:SYSTEM')
							//          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Desiccant

						} else {
							ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid Outside Air Component=\"" + OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentType + "\"." );
							ErrorsFound = true;

						}}
						// Add equipment to component sets array
						// Node set up
						if ( OutAirUnit( OAUnitNum ).FanPlace == BlowThru ) {
							if ( InListNum == 1 ) { // the component is the first one
								SetUpCompSets( "ZoneHVAC:OutdoorAirUnit", OutAirUnit( OAUnitNum ).Name, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName, cAlphaArgs( 15 ), "UNDEFINED" );
							} else if ( InListNum != NumInList ) { // the component is placed in b/w components
								SetUpCompSets( "ZoneHVAC:OutdoorAirUnit", OutAirUnit( OAUnitNum ).Name, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName, "UNDEFINED", "UNDEFINED" );
							} else if ( InListNum == NumInList ) { // the component is the last one
								SetUpCompSets( "ZoneHVAC:OutdoorAirUnit", OutAirUnit( OAUnitNum ).Name, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName, "UNDEFINED", cAlphaArgs( 13 ) );
							}
							// If fan is on the end of equipment.
						} else if ( OutAirUnit( OAUnitNum ).FanPlace == DrawThru ) {
							if ( InListNum == 1 ) {
								SetUpCompSets( "ZoneHVAC:OutdoorAirUnit", OutAirUnit( OAUnitNum ).Name, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName, cAlphaArgs( 12 ), "UNDEFINED" );
							} else if ( InListNum != NumInList ) {
								SetUpCompSets( "ZoneHVAC:OutdoorAirUnit", OutAirUnit( OAUnitNum ).Name, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName, "UNDEFINED", "UNDEFINED" );
							} else if ( InListNum == NumInList ) {
								SetUpCompSets( "ZoneHVAC:OutdoorAirUnit", OutAirUnit( OAUnitNum ).Name, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( InListNum ).ComponentName, "UNDEFINED", "UNDEFINED" );
							}
						}
					} // End Inlist

					// In case of draw through, the last component is linked with the zone air supply node
					if ( OutAirUnit( OAUnitNum ).FanPlace == DrawThru ) {
						SetUpCompSets( CurrentModuleObject, OutAirUnit( OAUnitNum ).Name, "UNDEFINED", cAlphaArgs( 5 ), "UNDEFINED", cAlphaArgs( 13 ) );
					}

				} else { // when ListNum<0
					ShowSevereError( CurrentModuleObject + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 16 ) + "=\"" + cAlphaArgs( 16 ) + "\" not found." );
					ErrorsFound = true;
				}
			} else { // when Equipment list is left blanked
				ShowSevereError( CurrentModuleObject + " = \"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 16 ) + " is blank and must be entered." );
				ErrorsFound = true;
			}
			if ( ! lAlphaBlanks( 17 ) ) {
				OutAirUnit( OAUnitNum ).AvailManagerListName = cAlphaArgs( 17 );
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + '.' );
		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		NumArray.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		GetOutdoorAirUnitInputFlag = false;

		// Setup Report variables for the zone outdoor air unit CurrentModuleObject='ZoneHVAC:OutdoorAirUnit'
		for ( OAUnitNum = 1; OAUnitNum <= NumOfOAUnits; ++OAUnitNum ) {
			SetupOutputVariable( "Zone Outdoor Air Unit Total Heating Rate [W]", OutAirUnit( OAUnitNum ).TotHeatingRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Total Heating Energy [J]", OutAirUnit( OAUnitNum ).TotHeatingEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Sensible Heating Rate [W]", OutAirUnit( OAUnitNum ).SensHeatingRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Sensible Heating Energy [J]", OutAirUnit( OAUnitNum ).SensHeatingEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Latent Heating Rate [W]", OutAirUnit( OAUnitNum ).LatHeatingRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Latent Heating Energy [J]", OutAirUnit( OAUnitNum ).LatHeatingEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Total Cooling Rate [W]", OutAirUnit( OAUnitNum ).TotCoolingRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Total Cooling Energy [J]", OutAirUnit( OAUnitNum ).TotCoolingEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Sensible Cooling Rate [W]", OutAirUnit( OAUnitNum ).SensCoolingRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Sensible Cooling Energy [J]", OutAirUnit( OAUnitNum ).SensCoolingEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Latent Cooling Rate [W]", OutAirUnit( OAUnitNum ).LatCoolingRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Latent Cooling Energy [J]", OutAirUnit( OAUnitNum ).LatCoolingEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Air Mass Flow Rate [kg/s]", OutAirUnit( OAUnitNum ).AirMassFlow, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Fan Electric Power [W]", OutAirUnit( OAUnitNum ).ElecFanRate, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Fan Electric Energy [J]", OutAirUnit( OAUnitNum ).ElecFanEnergy, "System", "Sum", OutAirUnit( OAUnitNum ).Name );
			SetupOutputVariable( "Zone Outdoor Air Unit Fan Availability Status []", OutAirUnit( OAUnitNum ).AvailStatus, "System", "Average", OutAirUnit( OAUnitNum ).Name );
			//! Note that the outdoor air unit fan electric is NOT metered because this value is already metered through the fan component

		}

	}

	void
	InitOutdoorAirUnit(
		int const OAUnitNum, // index for the current outdoor air unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   July 2009
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes all of the data elements which are necessary
		// to simulate a zone outdoor air control unit.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::StdRhoAir;
		using DataGlobals::AnyPlantInModel;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::OutdoorAirUnit_Num;
		using DataHVACGlobals::ShortenTimeStepSys;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using PlantUtilities::InitComponentNodes;
		using FluidProperties::GetDensityGlycol;
		auto & GetWaterCoilMaxFlowRate( WaterCoils::GetCoilMaxWaterFlowRate );
		using WaterCoils::SimulateWaterCoilComponents;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "ZoneHVAC:OutdoorAirUnit" );
		static std::string const RoutineName( "SizeOutdoorAirUnit" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyOneTimeFlag( true );
		// static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		////////////////////////////////////////////////////////////////////////////////////
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		int InNode; // inlet node number in outdoor air unit
		int OutNode; // outlet node number in outdoor air unit
		int OutsideAirNode; // outside air node number outdoor air unit
		Real64 OAFrac; // possible outside air fraction
		Real64 EAFrac; // possible exhaust air fraction
		Real64 RhoAir; // air density at InNode
		int compLoop; // local do loop index
		Real64 rho;
		bool errFlag;

		// FLOW:
		// Do the one time initializations

		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumOfOAUnits );
			MySizeFlag.allocate( NumOfOAUnits );
			MyPlantScanFlag.allocate( NumOfOAUnits );
			MyZoneEqFlag.allocate ( NumOfOAUnits );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyZoneEqFlag = true;
			MyOneTimeFlag = false;

		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( OAUnitNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( OutdoorAirUnit_Num ).ZoneCompAvailMgrs( OAUnitNum ).AvailManagerListName = OutAirUnit( OAUnitNum ).AvailManagerListName;
				ZoneComp( OutdoorAirUnit_Num ).ZoneCompAvailMgrs( OAUnitNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( OAUnitNum ) = false;
			}
			OutAirUnit( OAUnitNum ).AvailStatus = ZoneComp( OutdoorAirUnit_Num ).ZoneCompAvailMgrs( OAUnitNum ).AvailStatus;
		}

		if ( MyPlantScanFlag( OAUnitNum ) && allocated( PlantLoop ) ) {
			for ( compLoop = 1; compLoop <= OutAirUnit( OAUnitNum ).NumComponents; ++compLoop ) {
				if ( ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilWaterCooling ) || ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling ) || ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilWaterSimpleHeating ) || ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilSteamAirHeating ) ) {

					errFlag = false;
					ScanPlantLoopsForObject( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentName, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).BranchNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CompNum, _, _, _, _, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitOutdoorAirUnit: Program terminated for previous conditions." );
					}
				}
			}

			MyPlantScanFlag( OAUnitNum ) = false;
		} else if ( MyPlantScanFlag( OAUnitNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( OAUnitNum ) = false;
		}

		// need to check all zone outdoor air control units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumOfOAUnits; ++Loop ) {
				if ( CheckZoneEquipmentList( CurrentModuleObject, OutAirUnit( Loop ).Name ) ) continue;
				ShowSevereError( "InitOutdoorAirUnit: Zone Outdoor Air Unit=[" + CurrentModuleObject + ',' + OutAirUnit( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( OAUnitNum ) && ! MyPlantScanFlag( OAUnitNum ) ) {

			SizeOutdoorAirUnit( OAUnitNum );

			MySizeFlag( OAUnitNum ) = false;

		}

		// Do the one time initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( OAUnitNum ) ) {
			// Node Conditions

			OutNode = OutAirUnit( OAUnitNum ).AirOutletNode;
			OutsideAirNode = OutAirUnit( OAUnitNum ).OutsideAirNode;
			//Outdoor Air flow rate conditions
			RhoAir = StdRhoAir;
			OAFrac = GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).OutAirSchedPtr );
			OutAirUnit( OAUnitNum ).OutAirMassFlow = RhoAir * OAFrac * OutAirUnit( OAUnitNum ).OutAirVolFlow;
			OutAirUnit( OAUnitNum ).SMaxAirMassFlow = RhoAir * OAFrac * OutAirUnit( OAUnitNum ).SFanMaxAirVolFlow;

			if ( OutAirUnit( OAUnitNum ).ExtFan ) {
				InNode = OutAirUnit( OAUnitNum ).AirInletNode;
				// set the exhaust air mass flow rate from input
				if ( OutAirUnit( OAUnitNum ).ExtFan ) {
					EAFrac = GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).ExtOutAirSchedPtr );
					OutAirUnit( OAUnitNum ).ExtAirMassFlow = RhoAir * EAFrac * OutAirUnit( OAUnitNum ).ExtAirVolFlow;
					OutAirUnit( OAUnitNum ).EMaxAirMassFlow = RhoAir * EAFrac * OutAirUnit( OAUnitNum ).EFanMaxAirVolFlow;
				} else if( !OutAirUnit( OAUnitNum ).ExtFan ) {
					OutAirUnit( OAUnitNum ).ExtAirMassFlow = OutAirUnit( OAUnitNum ).OutAirMassFlow;
					OutAirUnit( OAUnitNum ).EMaxAirMassFlow = OutAirUnit( OAUnitNum ).SMaxAirMassFlow;
				}
				Node( InNode ).MassFlowRateMax = OutAirUnit( OAUnitNum ).EMaxAirMassFlow;
				Node( InNode ).MassFlowRateMin = 0.0;
			}
			// set the node max and min mass flow rates
			Node( OutsideAirNode ).MassFlowRateMax = OutAirUnit( OAUnitNum ).SMaxAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMin = 0.0;
			Node( OutNode ).MassFlowRate = OutAirUnit( OAUnitNum ).EMaxAirMassFlow;

			if ( ! MyPlantScanFlag( OAUnitNum ) ) {
				for ( compLoop = 1; compLoop <= OutAirUnit( OAUnitNum ).NumComponents; ++compLoop ) {
					if ( ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilWaterCooling ) || ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling ) ) {
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow = GetWaterCoilMaxFlowRate( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentName, errFlag );
						rho = GetDensityGlycol( PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidName, 5.0, PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidIndex, RoutineName );
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow;
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinVolWaterFlow;
						InitComponentNodes( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterInletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterOutletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).BranchNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CompNum );

					}

					if ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilWaterSimpleHeating ) {
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow = GetWaterCoilMaxFlowRate( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentName, errFlag );
						rho = GetDensityGlycol( PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidName, 60.0, PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidIndex, RoutineName );
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow;
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinVolWaterFlow;
						InitComponentNodes( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterInletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterOutletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).BranchNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CompNum );
					}
					if ( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == TypeOf_CoilSteamAirHeating ) {
						//DSU deal with steam mass flow rate , currenlty just like hot water  DSU?
						rho = GetDensityGlycol( PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidName, 60.0, PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidIndex, RoutineName );
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow;
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinVolWaterFlow;
						InitComponentNodes( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterInletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterOutletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).BranchNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CompNum );
					}
					if( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilPlantTypeOfNum == WaterCoil_CoolingHXAsst ) {
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow = GetWaterCoilMaxFlowRate( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentType, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).ComponentName, errFlag );
						rho = GetDensityGlycol( PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidName, 60.0, PlantLoop( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum ).FluidIndex, RoutineName );
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxVolWaterFlow;
						OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow = rho * OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinVolWaterFlow;
						InitComponentNodes( OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MinWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).MaxWaterMassFlow, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterInletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CoilWaterOutletNode, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).BranchNum, OutAirUnit( OAUnitNum ).OAEquip( compLoop ).CompNum );
					}
				}
			}
			MyEnvrnFlag( OAUnitNum ) = false;

		} // ...end start of environment inits

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( OAUnitNum ) = true;

		// These initializations are done every iteration...
		// Set all the output variable
		OutAirUnit( OAUnitNum ).TotHeatingRate = 0.0;
		OutAirUnit( OAUnitNum ).SensHeatingRate = 0.0;
		OutAirUnit( OAUnitNum ).LatHeatingRate = 0.0;
		OutAirUnit( OAUnitNum ).TotCoolingRate = 0.0;
		OutAirUnit( OAUnitNum ).SensCoolingRate = 0.0;
		OutAirUnit( OAUnitNum ).LatCoolingRate = 0.0;
		OutAirUnit( OAUnitNum ).AirMassFlow = 0.0;
		OutAirUnit( OAUnitNum ).ElecFanRate = 0.0;
		// Node Set

		OutNode = OutAirUnit( OAUnitNum ).AirOutletNode;
		OutsideAirNode = OutAirUnit( OAUnitNum ).OutsideAirNode;
		RhoAir = StdRhoAir;
		OAFrac = GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).OutAirSchedPtr );

		// set the mass flow rates from the input volume flow rates
		if ( OAFrac > 0.0 || ( ZoneCompTurnFansOn && ! ZoneCompTurnFansOff ) ) { // fan is available
			OutAirUnit( OAUnitNum ).OutAirMassFlow = RhoAir * OAFrac * OutAirUnit( OAUnitNum ).OutAirVolFlow;
		} else {
			OutAirUnit( OAUnitNum ).OutAirMassFlow = 0.0;
		}

		// set the exhaust air mass flow rate from input
		if ( OutAirUnit( OAUnitNum ).ExtFan ) {
			InNode = OutAirUnit( OAUnitNum ).AirInletNode;
			EAFrac = GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).ExtOutAirSchedPtr );
			if ( OutAirUnit( OAUnitNum ).ExtFanAvailSchedPtr > 0.0 ) {
				OutAirUnit( OAUnitNum ).ExtAirMassFlow = RhoAir * EAFrac * OutAirUnit( OAUnitNum ).ExtAirVolFlow;
			} else {
				OutAirUnit( OAUnitNum ).ExtAirMassFlow = 0.0;
			}
			Node( InNode ).MassFlowRate = OutAirUnit( OAUnitNum ).ExtAirMassFlow;
			Node( InNode ).MassFlowRateMaxAvail = OutAirUnit( OAUnitNum ).ExtAirMassFlow;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
		} else if ( ! OutAirUnit( OAUnitNum ).ExtFan ) {
			OutAirUnit( OAUnitNum ).ExtAirMassFlow = 0.0;
		}

		// First, set the flow conditions up so that there is flow through the unit

		Node( OutNode ).MassFlowRate = OutAirUnit( OAUnitNum ).OutAirMassFlow;
		Node( OutNode ).MassFlowRateMaxAvail = OutAirUnit( OAUnitNum ).OutAirMassFlow;
		Node( OutNode ).MassFlowRateMinAvail = 0.0;
		Node( OutsideAirNode ).MassFlowRate = OutAirUnit( OAUnitNum ).OutAirMassFlow;
		Node( OutsideAirNode ).MassFlowRateMaxAvail = OutAirUnit( OAUnitNum ).OutAirMassFlow;
		Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;

		// Just in case the system is off and conditions do not get sent through
		// the system for some reason, set the outlet conditions equal to the inlet
		// conditions of the zone outdoor air control unit
		if ( OutAirUnit( OAUnitNum ).ExtFan ) {
			Node( OutNode ).Temp = Node( InNode ).Temp;
			Node( OutNode ).Press = Node( InNode ).Press;
			Node( OutNode ).HumRat = Node( InNode ).HumRat;
			Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;
		} else {
			Node( OutNode ).Temp = Node( OutsideAirNode ).Temp;
			Node( OutNode ).Press = Node( OutsideAirNode ).Press;
			Node( OutNode ).HumRat = Node( OutsideAirNode ).HumRat;
			Node( OutNode ).Enthalpy = Node( OutsideAirNode ).Enthalpy;
		}
		//These initializations only need to be done once at the start of the iterations...
		if ( FirstHVACIteration || ShortenTimeStepSys ) {
			// Initialize the outside air conditions...
			Node( OutsideAirNode ).Temp = Node( OutsideAirNode ).OutAirDryBulb;
			Node( OutsideAirNode ).HumRat = OutHumRat;
			Node( OutsideAirNode ).Press = OutBaroPress;

		}

	}

	void
	SizeOutdoorAirUnit( int const OAUnitNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   July 2009
		//       MODIFIED       Brent Griffith, March 2010, autosize OA flow rate
		//                      August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing zoen outdoor air control unit components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using DataEnvironment::StdRhoAir;
		using DataHVACGlobals::cFanTypes;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using ReportSizingManager::ReportSizingOutput;
		using Fans::SimulateFanComponents;
		using Fans::GetFanDesignVolumeFlowRate;
		using General::RoundSigDigits;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using SteamCoils::SimulateSteamCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeOutdoorAirUnit" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
		bool ErrorsFound;
		Real64 RhoAir;
		int CompNum;
		bool IsAutoSize; // Indicator to autosize
		Real64 OutAirVolFlowDes; // Autosized outdoor air flow for reporting
		Real64 OutAirVolFlowUser; // Hardsized outdoor air flow for reporting
		Real64 ExtAirVolFlowDes; // Autosized exhaust air flow for reporting
		Real64 ExtAirVolFlowUser; // Hardsized exhaust air flow for reporting
		Real64 MaxVolWaterFlowDes; // Autosized maximum water flow for reporting
		Real64 MaxVolWaterFlowUser; // Hardsized maximum water flow for reporting

		PltSizCoolNum = 0;
		PltSizHeatNum = 0;
		ErrorsFound = false;
		RhoAir = StdRhoAir;
		IsAutoSize = false;
		OutAirVolFlowDes = 0.0;
		OutAirVolFlowUser = 0.0;
		ExtAirVolFlowDes = 0.0;
		ExtAirVolFlowUser = 0.0;
		MaxVolWaterFlowDes = 0.0;
		MaxVolWaterFlowUser = 0.0;

		if ( OutAirUnit( OAUnitNum ).OutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}

		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( OutAirUnit( OAUnitNum ).OutAirVolFlow > 0.0 ) {
					ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "User-Specified Outdoor Air Flow Rate [m3/s]", OutAirUnit( OAUnitNum ).OutAirVolFlow );
				}
			} else {
				CheckZoneSizing( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name );
				OutAirVolFlowDes = FinalZoneSizing( CurZoneEqNum ).MinOA;
				if ( OutAirVolFlowDes < SmallAirVolFlow ) {
					OutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					OutAirUnit( OAUnitNum ).OutAirVolFlow = OutAirVolFlowDes;
					ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "Design Size Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes );
				} else {
					if ( OutAirUnit( OAUnitNum ).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0 ) {
						OutAirVolFlowUser = OutAirUnit( OAUnitNum ).OutAirVolFlow;
						ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "User-Specified Outdoor Air Flow Rate [m3/s]", OutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( OutAirVolFlowDes - OutAirVolFlowUser ) / OutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "Design Size Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes );
								ShowMessage( "SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit " + OutAirUnit( OAUnitNum ).Name );
								ShowContinueError( "User-Specified Outdoor Air Flow Rate of " + RoundSigDigits( OutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Outdoor Air Flow Rate of " + RoundSigDigits( OutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( OutAirUnit( OAUnitNum ).ExtAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( OutAirUnit( OAUnitNum ).ExtAirVolFlow > 0.0 ) {
					ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "User-Specified Exhaust Air Flow Rate [m3/s]", OutAirUnit( OAUnitNum ).ExtAirVolFlow );
				}
			} else {
				// set exhaust flow equal to the oa inlet flow
				ExtAirVolFlowDes = OutAirUnit( OAUnitNum ).OutAirVolFlow;
				if ( IsAutoSize ) {
					OutAirUnit( OAUnitNum ).ExtAirVolFlow = ExtAirVolFlowDes;
					ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "Design Size Exhaust Air Flow Rate [m3/s]", ExtAirVolFlowDes );
				} else {
					if ( OutAirUnit( OAUnitNum ).ExtAirVolFlow > 0.0 && ExtAirVolFlowDes > 0.0 ) {
						ExtAirVolFlowUser = OutAirUnit( OAUnitNum ).ExtAirVolFlow;
						ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "User-Specified Exhaust Air Flow Rate [m3/s]", ExtAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( ExtAirVolFlowDes - ExtAirVolFlowUser ) / ExtAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ReportSizingOutput( CurrentModuleObjects( 1 ), OutAirUnit( OAUnitNum ).Name, "Design Size Exhaust Air Flow Rate [m3/s]", ExtAirVolFlowDes );
								ShowMessage( "SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit " + OutAirUnit( OAUnitNum ).Name );
								ShowContinueError( "User-Specified Exhaust Air Flow Rate of " + RoundSigDigits( ExtAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Exhaust Air Flow Rate of " + RoundSigDigits( ExtAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow = true;
		ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow = true;
		ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow = OutAirUnit( OAUnitNum ).OutAirVolFlow;
		ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow = OutAirUnit( OAUnitNum ).OutAirVolFlow;

		if( OutAirUnit(OAUnitNum).SFanMaxAirVolFlow == AutoSize ) {
			SimulateFanComponents( OutAirUnit( OAUnitNum ).SFanName, true, OutAirUnit( OAUnitNum ).SFan_Index, _, false, false );
			OutAirUnit( OAUnitNum ).SFanMaxAirVolFlow = GetFanDesignVolumeFlowRate( cFanTypes( OutAirUnit( OAUnitNum ).SFanType ), OutAirUnit( OAUnitNum ).SFanName, ErrorsFound );
		}
		if( OutAirUnit( OAUnitNum ).ExtFan ) {
			if( OutAirUnit( OAUnitNum ).EFanMaxAirVolFlow == AutoSize ) {
				SimulateFanComponents( OutAirUnit( OAUnitNum ).ExtFanName, true, OutAirUnit( OAUnitNum ).ExtFan_Index );
				OutAirUnit( OAUnitNum ).EFanMaxAirVolFlow = GetFanDesignVolumeFlowRate( cFanTypes( OutAirUnit( OAUnitNum ).ExtFanType ), OutAirUnit( OAUnitNum ).ExtFanName, ErrorsFound );
			}
		}

		for( CompNum = 1; CompNum <= OutAirUnit( OAUnitNum ).NumComponents; ++CompNum ) {
			if( ( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum == TypeOf_CoilWaterCooling ) || ( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling ) ) {
				if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow == AutoSize ) {
					SimulateWaterCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, true, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, _, 1, 0.0 );
				}
			}
			if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum == TypeOf_CoilWaterSimpleHeating ) {
				if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow == AutoSize ) {
					SimulateWaterCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, true, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, _, 1, 0.0 );
				}
			}
			if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum == TypeOf_CoilSteamAirHeating ) {
				if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow == AutoSize ) {
					SimulateSteamCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, true, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex );
				}
			}
			if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).CoilPlantTypeOfNum == WaterCoil_CoolingHXAsst ) {
				if( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).MaxVolWaterFlow == AutoSize ) {
					SimHXAssistedCoolingCoil( OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentName, true, 1, 0.0, OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, ContFanCycCoil );
				}
			}
		}

		if( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcOutdoorAirUnit(
		int & OAUnitNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // power supplied
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine mainly controls the action of the outdoor air unit
		// (or more exactly, it controls the coil outlet temperature of the unit)
		// based on the user input for controls and the defined controls
		// algorithms.

		// METHODOLOGY EMPLOYED:
		// Outdoor air unit is controlled based on user input and what is happening in the
		// simulation.
		// Note: controls are strictly temperature based and do not factor
		// humidity into the equation (not an enthalpy economy cycle but rather
		// a simple return air cycle).

		// REFERENCES:
		// ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

		// USE STATEMENTS:

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using HeatingCoils::CheckHeatingCoilSchedule;
		using WaterCoils::CheckWaterCoilSchedule;
		using HVACHXAssistedCoolingCoil::CheckHXAssistedCoolingCoilSchedule;
		using SteamCoils::CheckSteamCoilSchedule;
		using Fans::SimulateFanComponents;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;

		// Locals

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const CurrentModuleObject( "ZoneHVAC:OutdoorAirUnit" );
		Real64 DesOATemp; // Design OA Temp degree C
		Real64 AirMassFlow; // air mass flow rate [kg/s]
		int ControlNode; // the hot water or cold water inlet node
		int InletNode; // Unit air inlet node
		int SFanOutletNode; // Unit supply fan outlet node
		int OutletNode; // air outlet node
		int OutsideAirNode; // outside air node
		Real64 QTotUnitOut; // total unit output [watts]
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 LatLoadMet; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 MinHumRat; // desired temperature after mixing inlet and outdoor air [degrees C]
		Real64 SetPointTemp; // temperature that will be used to control the radiant system [Celsius]
		Real64 HiCtrlTemp; // Current high point in setpoint temperature range
		Real64 LoCtrlTemp; // Current low point in setpoint temperature range
		Real64 AirInEnt; // RE-calcualte the Enthalpy of supply air
		Real64 AirOutletTemp;
		static int OperatingMode( 0 );
		static int UnitControlType( 0 );
		Real64 ZoneSupAirEnt; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		// Latent output
		Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		Real64 ZoneAirEnt; // zone air enthalphy J/kg

		// FLOW:

		FanElecPower = 0.0;
		// initialize local variables
		ControlNode = 0;
		QUnitOut = 0.0;
		if ( OutAirUnit( OAUnitNum ).ExtFan ) InletNode = OutAirUnit( OAUnitNum ).AirInletNode;
		SFanOutletNode = OutAirUnit( OAUnitNum ).SFanOutletNode;
		OutletNode = OutAirUnit( OAUnitNum ).AirOutletNode;
		OutsideAirNode = OutAirUnit( OAUnitNum ).OutsideAirNode;
		OperatingMode = OutAirUnit( OAUnitNum ).OperatingMode;
		UnitControlType = OutAirUnit( OAUnitNum ).ControlType;
		AirOutletTemp = 0.0;
		OutAirUnit( OAUnitNum ).CompOutSetTemp = 0.0;
		OutAirUnit( OAUnitNum ).FanEffect = false;

		if ( ( GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).SchedPtr ) <= 0 ) || ( GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).OutAirSchedPtr ) <= 0 ) || ( ( GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).SFanAvailSchedPtr ) <= 0 ) && ! ZoneCompTurnFansOn ) || ZoneCompTurnFansOff ) {
			// System is off or has no load upon the unit; set the flow rates to zero and then
			// simulate the components with the no flow conditions
			if ( OutAirUnit( OAUnitNum ).ExtFan ) Node( InletNode ).MassFlowRate = 0.0;
			if ( OutAirUnit( OAUnitNum ).ExtFan ) Node( InletNode ).MassFlowRateMaxAvail = 0.0;
			if ( OutAirUnit( OAUnitNum ).ExtFan ) Node( InletNode ).MassFlowRateMinAvail = 0.0;
			Node( SFanOutletNode ).MassFlowRate = 0.0;
			Node( SFanOutletNode ).MassFlowRateMaxAvail = 0.0;
			Node( SFanOutletNode ).MassFlowRateMinAvail = 0.0;
			Node( OutletNode ).MassFlowRate = 0.0;
			Node( OutletNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutletNode ).MassFlowRateMinAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRate = 0.0;
			Node( OutsideAirNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			AirMassFlow = Node( SFanOutletNode ).MassFlowRate;

			// Node condition
			if ( OutAirUnit( OAUnitNum ).ExtFan ) {
				Node( InletNode ).Temp = MAT( ZoneNum );
				Node( SFanOutletNode ).Temp = Node( InletNode ).Temp;
			} else {
				Node( SFanOutletNode ).Temp = MAT( ZoneNum );
			}
			Node( OutletNode ).Temp = Node( SFanOutletNode ).Temp;

			if ( OutAirUnit( OAUnitNum ).FanPlace == BlowThru ) {
				SimulateFanComponents( OutAirUnit( OAUnitNum ).SFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).SFan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				OutAirUnit( OAUnitNum ).ElecFanRate += FanElecPower;
				SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
				if ( OutAirUnit( OAUnitNum ).ExtFan ) SimulateFanComponents( OutAirUnit( OAUnitNum ).ExtFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).ExtFan_Index );
			} else if ( OutAirUnit( OAUnitNum ).FanPlace == DrawThru ) {
				SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
				SimulateFanComponents( OutAirUnit( OAUnitNum ).SFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).SFan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				if ( OutAirUnit( OAUnitNum ).ExtFan ) SimulateFanComponents( OutAirUnit( OAUnitNum ).ExtFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).ExtFan_Index );
			}

		} else { // System On

			//Flowrate Check
			if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
				Node( OutsideAirNode ).MassFlowRate = OutAirUnit( OAUnitNum ).OutAirMassFlow;
			}

			//Fan Positioning Check

			if ( OutAirUnit( OAUnitNum ).ExtFan ) {
				Node( InletNode ).MassFlowRate = OutAirUnit( OAUnitNum ).ExtAirMassFlow;
			}

			//Air mass balance check (removed because exhaust and supply can be imbalanced
			//    IF ((Node(InletNode)%MassFlowRate > Node(OutsideAirNode)%MassFlowRate) &
			//             .OR.(Node(InletNode)%MassFlowRate < Node(OutsideAirNode)%MassFlowRate)) THEN
			//      OutAirUnit(OAUnitNum)%UnBalancedErrCount = OutAirUnit(OAUnitNum)%UnBalancedErrCount + 1
			//      IF (OutAirUnit(OAUnitNum)%UnBalancedErrCount .EQ. 1) THEN
			//        CALL ShowWarningError('Air mass flow between zone supply and exhaust is not balanced')
			//        CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
			//                               //TRIM(OutAirUnit(OAUnitNum)%Name))
			//        CALL ShowContinueError('Air mass balance is required by other outdoor air units,'// &
			//                                  'ZoneMixing, ZoneCrossMixing, or other air flow control inputs.')
			//!
			//!  CALL ShowContinueErrorTimeStamp('Air volume flow rate ratio = '//TRIM(RoundSigDigits(HXAirVolFlowRatio,3))//'.')
			//!ELSE
			//! CALL ShowRecurringWarningErrorAtEnd(TRIM(OutAirUnit(OAUnitNum)%Name)//&
			//! ':  Air mass balance is required by other outdoor air units, ZoneMixing, ZoneCrossMixing, or other air flow control inputs.'&
			//!   , OutAirUnit(OAUnitNum)%UnBalancedErrIndex)
			//      END IF
			//    END IF

			if ( OutAirUnit( OAUnitNum ).FanPlace == BlowThru ) {
				SimulateFanComponents( OutAirUnit( OAUnitNum ).SFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).SFan_Index );
				DesOATemp = Node( SFanOutletNode ).Temp;
			} else if ( OutAirUnit( OAUnitNum ).FanPlace == DrawThru ) {
				DesOATemp = Node( OutsideAirNode ).Temp;
			}

			//Control type check
			{ auto const SELECT_CASE_var( UnitControlType );
			if ( SELECT_CASE_var == Neutral ) {
				SetPointTemp = MAT( ZoneNum );
				//Neutral Control Condition
				if ( DesOATemp == SetPointTemp ) {
					OutAirUnit( OAUnitNum ).OperatingMode = NeutralMode;
					AirOutletTemp = DesOATemp;
					OutAirUnit( OAUnitNum ).CompOutSetTemp = DesOATemp;
					SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
				} else {
					if ( DesOATemp < SetPointTemp ) { // Heating MODE
						OutAirUnit( OAUnitNum ).OperatingMode = HeatingMode;
						AirOutletTemp = SetPointTemp;
						OutAirUnit( OAUnitNum ).CompOutSetTemp = AirOutletTemp;
						SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
					} else if ( DesOATemp > SetPointTemp ) { //Cooling Mode
						OutAirUnit( OAUnitNum ).OperatingMode = CoolingMode;
						AirOutletTemp = SetPointTemp;
						OutAirUnit( OAUnitNum ).CompOutSetTemp = AirOutletTemp;
						SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
					}
				}
				//SetPoint Temperature Condition
			} else if ( SELECT_CASE_var == Temperature ) {
				SetPointTemp = DesOATemp;
				HiCtrlTemp = GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).HiCtrlTempSchedPtr );
				LoCtrlTemp = GetCurrentScheduleValue( OutAirUnit( OAUnitNum ).LoCtrlTempSchedPtr );
				if ( ( DesOATemp <= HiCtrlTemp ) && ( DesOATemp >= LoCtrlTemp ) ) {
					OutAirUnit( OAUnitNum ).OperatingMode = NeutralMode;
					AirOutletTemp = DesOATemp;
					OutAirUnit( OAUnitNum ).CompOutSetTemp = DesOATemp;
					SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
				} else {
					if ( SetPointTemp < LoCtrlTemp ) {
						OutAirUnit( OAUnitNum ).OperatingMode = HeatingMode;
						AirOutletTemp = LoCtrlTemp;
						OutAirUnit( OAUnitNum ).CompOutSetTemp = AirOutletTemp;
						SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
					} else if ( SetPointTemp > HiCtrlTemp ) {
						OutAirUnit( OAUnitNum ).OperatingMode = CoolingMode;
						AirOutletTemp = HiCtrlTemp;
						OutAirUnit( OAUnitNum ).CompOutSetTemp = AirOutletTemp;
						SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
					}
				}
			}}

			// Fan positioning
			if ( OutAirUnit( OAUnitNum ).FanPlace == DrawThru ) {
				SimulateFanComponents( OutAirUnit( OAUnitNum ).SFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).SFan_Index );
				OutAirUnit( OAUnitNum ).FanEffect = true; //RE-Simulation to take over the supply fan effect
				OutAirUnit( OAUnitNum ).FanCorTemp = ( Node( OutletNode ).Temp - OutAirUnit( OAUnitNum ).CompOutSetTemp );
				SimZoneOutAirUnitComps( OAUnitNum, FirstHVACIteration );
				SimulateFanComponents( OutAirUnit( OAUnitNum ).SFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).SFan_Index );

				OutAirUnit( OAUnitNum ).FanEffect = false;
			}
			if ( OutAirUnit( OAUnitNum ).ExtFan ) SimulateFanComponents( OutAirUnit( OAUnitNum ).ExtFanName, FirstHVACIteration, OutAirUnit( OAUnitNum ).ExtFan_Index );
		} // ...end of system ON/OFF IF-THEN block

		AirMassFlow = Node( OutletNode ).MassFlowRate;
		MinHumRat = min( Node( OutletNode ).HumRat, Node( OutAirUnit( OAUnitNum ).ZoneNodeNum ).HumRat );

		AirInEnt = PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ); // zone supply air node enthalpy
		ZoneAirEnt = PsyHFnTdbW( Node( OutAirUnit( OAUnitNum ).ZoneNodeNum ).Temp, MinHumRat ); // zone air enthalpy
		QUnitOut = AirMassFlow * ( AirInEnt - ZoneAirEnt ); // Senscooling

		// CR9155 Remove specific humidity calculations
		SpecHumOut = Node( OutletNode ).HumRat;
		SpecHumIn = Node( OutAirUnit( OAUnitNum ).ZoneNodeNum ).HumRat;
		LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative

		ZoneAirEnt = PsyHFnTdbW( Node( OutAirUnit( OAUnitNum ).ZoneNodeNum ).Temp, Node( OutAirUnit( OAUnitNum ).ZoneNodeNum ).HumRat );

		ZoneSupAirEnt = PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat );
		QTotUnitOut = AirMassFlow * ( ZoneSupAirEnt - ZoneAirEnt );
		LatLoadMet = QTotUnitOut - QUnitOut; // watts

		// Report variables...

		if ( QUnitOut < 0.0 ) {
			OutAirUnit( OAUnitNum ).SensCoolingRate = std::abs( QUnitOut );
			OutAirUnit( OAUnitNum ).SensHeatingRate = 0.0;
		} else {
			OutAirUnit( OAUnitNum ).SensCoolingRate = 0.0;
			OutAirUnit( OAUnitNum ).SensHeatingRate = QUnitOut;
		}

		if ( QTotUnitOut < 0.0 ) {
			OutAirUnit( OAUnitNum ).TotCoolingRate = std::abs( QTotUnitOut );
			OutAirUnit( OAUnitNum ).TotHeatingRate = 0.0;
		} else {
			OutAirUnit( OAUnitNum ).TotCoolingRate = 0.0;
			OutAirUnit( OAUnitNum ).TotHeatingRate = QTotUnitOut;
		}

		if ( LatLoadMet < 0.0 ) {
			OutAirUnit( OAUnitNum ).LatCoolingRate = std::abs( LatLoadMet );
			OutAirUnit( OAUnitNum ).LatHeatingRate = 0.0;
		} else {
			OutAirUnit( OAUnitNum ).LatCoolingRate = 0.0;
			OutAirUnit( OAUnitNum ).LatHeatingRate = LatLoadMet;
		}

		OutAirUnit( OAUnitNum ).ElecFanRate = FanElecPower;

		PowerMet = QUnitOut;
		LatOutputProvided = LatentOutput;

	}

	void
	SimZoneOutAirUnitComps(
		int const OAUnitNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Simulate the controllers and components in the outside air system.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
//		using InputProcessor::FindItemInList;
//		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EquipNum;
		int CurOAUnitNum;
		std::string EquipType;
		std::string EquipName;
		bool FatalErrorFlag;
		bool Sim;

		FatalErrorFlag = false;
		CurOAUnitNum = OAUnitNum;
		Sim = true;

		for ( EquipNum = 1; EquipNum <= OutAirUnit( OAUnitNum ).NumComponents; ++EquipNum ) {
			EquipType = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).ComponentType;
			EquipName = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).ComponentName;
			SimOutdoorAirEquipComps( OAUnitNum, EquipType, EquipName, EquipNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).ComponentType_Num, FirstHVACIteration, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).ComponentIndex, Sim );
		}

		CurOAUnitNum = 0;

	}

	void
	SimOutdoorAirEquipComps(
		int const OAUnitNum, // actual outdoor air unit num
		std::string const & EquipType, // the component type
		std::string const & EquipName, // the component Name
		int const EquipNum,
		int const EP_UNUSED( CompTypeNum ), // Component Type -- Integerized for this module
		bool const FirstHVACIteration,
		int & CompIndex,
		bool const Sim // if TRUE, simulate component
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Outdoor air unit has various coil options. This subroutine defines the coil loads and execute
		// to simulate each components
		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalance::MRT;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHVACGlobals::SmallLoad;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using General::TrimSigDigits;
		using NodeInputManager::GetOnlySingleNode;
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using HeatRecovery::SimHeatRecovery;
		using DesiccantDehumidifiers::SimDesiccantDehumidifier;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using HVACDXSystem::SimDXCoolingSystem;
		using HVACDXHeatPumpSystem::SimDXHeatPumpSystem;
//		using SteamCoils::SimulateSteamCoilComponents;
		using HVACUnitarySystem::SimUnitarySystem;
		//  Use TranspiredCollector, Only:SimTranspiredCollector
		//  Use EvaporativeCoolers, Only:SimEvapCooler
		//  USE PhotovoltaicThermalCollectors, ONLY:SimPVTcollectors, CalledFromOutsideAirSystem

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS: None

		// INTERFACE BLOCK DEFINITIONS:

		// DERIVED TYPE DEFINITIONS: None

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS
		Real64 OAMassFlow;
		Real64 QCompReq;
		int UnitNum;
		Real64 MaxWaterFlow;
		Real64 MinWaterFlow;
		int ControlNode;
		Real64 CpAirZn;
		int SimCompNum;
		int OpMode;
		int EquipTypeNum;
		int WCCoilInletNode;
		int WCCoilOutletNode;
		int WHCoilInletNode;
		int WHCoilOutletNode;
		Real64 QUnitOut;
		static int DXSystemIndex( 0 );
		Real64 CompAirOutTemp;
		Real64 FanEffect;
		bool DrawFan; // fan position If .True., the temperature increasing by fan operating is considered
		Real64 Dxsystemouttemp;
		static bool HeatActive( false );
		static bool CoolActive( false );

		// Flow!
		UnitNum = OAUnitNum;
		CompAirOutTemp = OutAirUnit( OAUnitNum ).CompOutSetTemp;
		OpMode = OutAirUnit( OAUnitNum ).OperatingMode;
		SimCompNum = EquipNum;
		EquipTypeNum = OutAirUnit( OAUnitNum ).OAEquip( SimCompNum ).ComponentType_Num;
		OAMassFlow = OutAirUnit( OAUnitNum ).OutAirMassFlow;
		DrawFan = OutAirUnit( OAUnitNum ).FanEffect;
		DXSystemIndex = 0;

		//check the fan positioning
		if ( DrawFan ) {
			FanEffect = OutAirUnit( OAUnitNum ).FanCorTemp; // Heat effect by fan
		} else {
			FanEffect = 0.0;
		}

		// checking equipment index

		{ auto const SELECT_CASE_var( EquipTypeNum );

		// Heat recovery
		if ( SELECT_CASE_var == HeatXchngr ) { // 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent',
			// 'HeatExchanger:Desiccant:BalancedFlow'
			if ( Sim ) {
				SimHeatRecovery( EquipName, FirstHVACIteration, CompIndex, ContFanCycCoil, _, _, _, _, false, false );
			}
			// Desiccant Dehumidifier
		} else if ( SELECT_CASE_var == Desiccant ) { // 'Dehumidifier:Desiccant:NoFans'
			if ( Sim ) {
				SimDesiccantDehumidifier( EquipName, FirstHVACIteration, CompIndex );
			}

		} else if ( SELECT_CASE_var == WaterCoil_SimpleHeat ) { // ('Coil:Heating:Water')

			if ( Sim ) {
				ControlNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilWaterInletNode;
				MaxWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MaxWaterMassFlow;
				MinWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MinWaterMassFlow;
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ( ! FirstHVACIteration ) && ( ControlNode > 0 ) ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				WHCoilInletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirInletNode;
				WHCoilOutletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirOutletNode;

				CpAirZn = PsyCpAirFnWTdb( Node( WHCoilInletNode ).HumRat, Node( WHCoilInletNode ).Temp );

				if ( ( OpMode == NeutralMode ) || ( OpMode == CoolingMode ) || ( Node( WHCoilInletNode ).Temp > CompAirOutTemp ) ) {
					QCompReq = 0.0;
				} else {
					QCompReq = CpAirZn * OAMassFlow * ( ( CompAirOutTemp - Node( WHCoilInletNode ).Temp ) - FanEffect );
					if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
					if ( QCompReq < 0.0 ) QCompReq = 0.0; // coil can heat only
				}

				ControlCompOutput( OutAirUnit( OAUnitNum ).Name, cMO_OutdoorAirUnit, UnitNum, FirstHVACIteration, QCompReq, ControlNode, MaxWaterFlow, MinWaterFlow, 0.0001, OutAirUnit( OAUnitNum ).ControlCompTypeNum, OutAirUnit( OAUnitNum ).CompErrIndex, _, _, _, 2, SimCompNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).BranchNum );
			}

		} else if ( SELECT_CASE_var == SteamCoil_AirHeat ) { // 'Coil:Heating:Steam'
			if ( Sim ) {
				CalcOAUnitCoilComps( UnitNum, FirstHVACIteration, SimCompNum, QUnitOut );
			}

		} else if ( SELECT_CASE_var == Coil_ElectricHeat ) { // 'Coil:Heating:Electric'
			if ( Sim ) {
				//     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
				CalcOAUnitCoilComps( UnitNum, FirstHVACIteration, SimCompNum, QUnitOut );
			}

		} else if ( SELECT_CASE_var == Coil_GasHeat ) { // 'Coil:Heating:Gas'
			if ( Sim ) {
				//     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
				CalcOAUnitCoilComps( UnitNum, FirstHVACIteration, SimCompNum, QUnitOut );
			}

			// water cooling coil Types

		} else if ( SELECT_CASE_var == WaterCoil_Cooling ) { // 'Coil:Cooling:Water'
			if ( Sim ) {
				ControlNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilWaterInletNode;
				MaxWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MaxWaterMassFlow;
				MinWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MinWaterMassFlow;
				// On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ( ! FirstHVACIteration ) && ( ControlNode > 0 ) ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				WCCoilInletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirInletNode;
				WCCoilOutletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirOutletNode;

				CpAirZn = PsyCpAirFnWTdb( Node( WCCoilInletNode ).HumRat, Node( WCCoilInletNode ).Temp );
				OAMassFlow = OutAirUnit( OAUnitNum ).OutAirMassFlow;
				if ( ( OpMode == NeutralMode ) || ( OpMode == HeatingMode ) || ( Node( WCCoilInletNode ).Temp < CompAirOutTemp ) ) {
					QCompReq = 0.0;
					Node( WCCoilOutletNode ).Temp = Node( WCCoilInletNode ).Temp;
					Node( WCCoilOutletNode ).HumRat = Node( WCCoilInletNode ).HumRat;
					Node( WCCoilOutletNode ).MassFlowRate = Node( WCCoilInletNode ).MassFlowRate;

				} else {

					QCompReq = CpAirZn * OAMassFlow * ( ( CompAirOutTemp - Node( WCCoilInletNode ).Temp ) - FanEffect );
					if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
					if ( QCompReq > 0.0 ) QCompReq = 0.0; // coil can cool only
				}

				ControlCompOutput( OutAirUnit( OAUnitNum ).Name, cMO_OutdoorAirUnit, UnitNum, FirstHVACIteration, QCompReq, ControlNode, MaxWaterFlow, MinWaterFlow, 0.001, OutAirUnit( OAUnitNum ).ControlCompTypeNum, OutAirUnit( OAUnitNum ).CompErrIndex, _, _, _, 1, SimCompNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).BranchNum );

			}

		} else if ( SELECT_CASE_var == WaterCoil_DetailedCool ) { // 'Coil:Cooling:Water:DetailedGeometry'
			if ( Sim ) {
				ControlNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilWaterInletNode;
				MaxWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MaxWaterMassFlow;
				MinWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MinWaterMassFlow;
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ( ! FirstHVACIteration ) && ( ControlNode > 0 ) ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				WCCoilInletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirInletNode;
				WCCoilOutletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirOutletNode;

				CpAirZn = PsyCpAirFnWTdb( Node( WCCoilInletNode ).HumRat, Node( WCCoilInletNode ).Temp );
				OAMassFlow = OutAirUnit( OAUnitNum ).OutAirMassFlow;

				if ( ( OpMode == NeutralMode ) || ( OpMode == HeatingMode ) || ( Node( WCCoilInletNode ).Temp < CompAirOutTemp ) ) {
					QCompReq = 0.0;
				} else {

					QCompReq = CpAirZn * OAMassFlow * ( ( CompAirOutTemp - Node( WCCoilInletNode ).Temp ) - FanEffect );
					if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
					if ( QCompReq > 0.0 ) QCompReq = 0.0; // coil can cool only
				}

				ControlCompOutput( OutAirUnit( OAUnitNum ).Name, "ZONEHVAC:OUTDOORAIRUNIT", UnitNum, FirstHVACIteration, QCompReq, ControlNode, MaxWaterFlow, MinWaterFlow, 0.001, OutAirUnit( OAUnitNum ).ControlCompTypeNum, OutAirUnit( OAUnitNum ).CompErrIndex, _, _, _, 1, SimCompNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).BranchNum );
			}

		} else if ( SELECT_CASE_var == WaterCoil_CoolingHXAsst ) { // 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
			if ( Sim ) {
				ControlNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilWaterInletNode;
				MaxWaterFlow = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).MaxWaterMassFlow;
				MinWaterFlow = 0.0;
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ( ! FirstHVACIteration ) && ( ControlNode > 0 ) ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				WCCoilInletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirInletNode;
				WCCoilOutletNode = OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).CoilAirOutletNode;
				CpAirZn = PsyCpAirFnWTdb( Node( WCCoilInletNode ).HumRat, Node( WCCoilInletNode ).Temp );
				OAMassFlow = OutAirUnit( OAUnitNum ).OutAirMassFlow;
				if ( ( OpMode == NeutralMode ) || ( OpMode == HeatingMode ) || ( Node( WCCoilInletNode ).Temp < CompAirOutTemp ) ) {
					QCompReq = 0.0;
				} else {
					QCompReq = CpAirZn * OAMassFlow * ( ( CompAirOutTemp - Node( WCCoilInletNode ).Temp ) - FanEffect );
					if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
					if ( QCompReq > 0.0 ) QCompReq = 0.0; // coil can cool only
				}
				ControlCompOutput( OutAirUnit( OAUnitNum ).Name, "ZONEHVAC:OUTDOORAIRUNIT", UnitNum, FirstHVACIteration, QCompReq, ControlNode, MaxWaterFlow, MinWaterFlow, 0.001, OutAirUnit( OAUnitNum ).ControlCompTypeNum, OutAirUnit( OAUnitNum ).CompErrIndex, _, _, _, 1, SimCompNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).LoopSideNum, OutAirUnit( OAUnitNum ).OAEquip( EquipNum ).BranchNum );
			}

		} else if ( SELECT_CASE_var == DXSystem ) { // CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
			if ( Sim ) {
				if ( ( ( OpMode == NeutralMode ) && ( OutAirUnit( OAUnitNum ).ControlType == Temperature ) ) || ( OpMode == HeatingMode ) ) {
					Dxsystemouttemp = 100.0; // There is no cooling demand for the DX system.
				} else {
					Dxsystemouttemp = CompAirOutTemp - FanEffect;
				}
				SimDXCoolingSystem( EquipName, FirstHVACIteration, -1, DXSystemIndex, UnitNum, Dxsystemouttemp );
			}

		} else if ( SELECT_CASE_var == DXHeatPumpSystem ) {
			if ( Sim ) {
				if ( ( ( OpMode == NeutralMode ) && ( OutAirUnit( OAUnitNum ).ControlType == Temperature ) ) || ( OpMode == CoolingMode ) ) {
					Dxsystemouttemp = -20.0; // There is no heating demand for the DX system.
				} else {
					Dxsystemouttemp = CompAirOutTemp - FanEffect;
				}
				SimDXHeatPumpSystem( EquipName, FirstHVACIteration, -1, DXSystemIndex, UnitNum, Dxsystemouttemp );
			}

		} else if ( SELECT_CASE_var == UnitarySystem ) { // 'AirLoopHVAC:UnitarySystem'
			if ( Sim ) {
				// This may have to be done in the unitary system object since there can be both cooling and heating
				if ( ( ( OpMode == NeutralMode ) && ( OutAirUnit( OAUnitNum ).ControlType == Temperature ) ) && ( OpMode == HeatingMode ) ) {
					Dxsystemouttemp = 100.0; // There is no cooling demand.
				} else if ( ( ( OpMode == NeutralMode ) && ( OutAirUnit( OAUnitNum ).ControlType == Temperature ) ) && ( OpMode == CoolingMode ) ) {
					Dxsystemouttemp = -20.0; // There is no heating demand.
				} else {
					Dxsystemouttemp = CompAirOutTemp - FanEffect;
				}
				SimUnitarySystem( EquipName, FirstHVACIteration, -1, DXSystemIndex, HeatActive, CoolActive, UnitNum, Dxsystemouttemp );
			}

		} else {
			ShowFatalError( "Invalid Outdoor Air Unit Component=" + EquipType ); // validate
		}}

	}

	void
	CalcOAUnitCoilComps(
		int const CompNum, // actual outdoor air unit num
		bool const FirstHVACIteration,
		int const EquipIndex, // Component Type -- Integerized for this module
		Real64 & LoadMet
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2009
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine mainly controls the action of water components in the unit

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataHVACGlobals::SmallLoad;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Locals
		// SUBROUTINE LOCAL VARIABLE DEFINITIONS
		int OAUnitNum;
		Real64 CpAirZn;
		int CoilIndex;
		int OpMode;
		Real64 AirMassFlow;
		Real64 FanEffect;
		bool DrawFan; // Fan Flag
		int InletNode;
		int OutletNode;
		Real64 QCompReq; // Actual equipment load
		int CoilTypeNum;
		Real64 CoilAirOutTemp;
		int CompoNum;

		// Flow
		CoilIndex = 0;
		OAUnitNum = CompNum;
		CompoNum = EquipIndex;
		CoilTypeNum = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentType_Num;
		OpMode = OutAirUnit( OAUnitNum ).OperatingMode;
		CoilAirOutTemp = OutAirUnit( OAUnitNum ).CompOutSetTemp;
		DrawFan = OutAirUnit( OAUnitNum ).FanEffect;
		if ( DrawFan ) {
			FanEffect = OutAirUnit( OAUnitNum ).FanCorTemp;
		} else {
			FanEffect = 0.0;
		}

		{ auto const SELECT_CASE_var( CoilTypeNum );
		if ( SELECT_CASE_var == Coil_ElectricHeat ) {
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			if ( ( OpMode == NeutralMode ) || ( OpMode == CoolingMode ) || ( Node( InletNode ).Temp > CoilAirOutTemp ) ) {
				QCompReq = 0.0;
			} else {
				CpAirZn = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );
				QCompReq = Node( InletNode ).MassFlowRate * CpAirZn * ( ( CoilAirOutTemp - Node( InletNode ).Temp ) - FanEffect );
				if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
			}

			if ( QCompReq <= 0.0 ) {
				QCompReq = 0.0; // a heating coil can only heat, not cool
				Node( OutletNode ).Temp = Node( InletNode ).Temp;
				Node( OutletNode ).HumRat = Node( InletNode ).HumRat;
				Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;

			}
			SimulateHeatingCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, QCompReq, CoilIndex );

			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} else if ( SELECT_CASE_var == Coil_GasHeat ) { // 'Coil:Heating:Steam'
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			if ( ( OpMode == NeutralMode ) || ( OpMode == CoolingMode ) || ( Node( InletNode ).Temp > CoilAirOutTemp ) ) {
				QCompReq = 0.0;
			} else {
				Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
				CpAirZn = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );
				QCompReq = Node( InletNode ).MassFlowRate * CpAirZn * ( ( CoilAirOutTemp - Node( InletNode ).Temp ) - FanEffect );
				if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
			}
			if ( QCompReq <= 0.0 ) {
				QCompReq = 0.0; // a heating coil can only heat, not cool
				Node( OutletNode ).Temp = Node( InletNode ).Temp;
				Node( OutletNode ).HumRat = Node( InletNode ).HumRat;
				Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
			}
			SimulateHeatingCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, QCompReq, CoilIndex );

			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} else if ( SELECT_CASE_var == SteamCoil_AirHeat ) { // 'Coil:Heating:Steam'
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			if ( ( OpMode == NeutralMode ) || ( OpMode == CoolingMode ) || ( Node( InletNode ).Temp > CoilAirOutTemp ) ) {
				QCompReq = 0.0;
			} else {
				CpAirZn = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );
				QCompReq = Node( InletNode ).MassFlowRate * CpAirZn * ( ( CoilAirOutTemp - Node( InletNode ).Temp ) - FanEffect );
				if ( std::abs( QCompReq ) < SmallLoad ) QCompReq = 0.0;
			}
			if ( QCompReq <= 0.0 ) {
				QCompReq = 0.0; // a heating coil can only heat, not cool
				Node( OutletNode ).Temp = Node( InletNode ).Temp;
				Node( OutletNode ).HumRat = Node( InletNode ).HumRat;
				Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
			}
			SimulateSteamCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, CoilIndex, QCompReq );
			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} else if ( SELECT_CASE_var == WaterCoil_SimpleHeat ) { // 'Coil:Heating:Water')
			SimulateWaterCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, CoilIndex );
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} else if ( SELECT_CASE_var == WaterCoil_Cooling ) { // 'Coil:Cooling:Water'
			SimulateWaterCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, CoilIndex );
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} else if ( SELECT_CASE_var == WaterCoil_DetailedCool ) {
			SimulateWaterCoilComponents( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, CoilIndex );
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} else if ( SELECT_CASE_var == WaterCoil_CoolingHXAsst ) {
			SimHXAssistedCoolingCoil( OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).ComponentName, FirstHVACIteration, 1, 0.0, CoilIndex, ContFanCycCoil );
			InletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirInletNode;
			OutletNode = OutAirUnit( OAUnitNum ).OAEquip( CompoNum ).CoilAirOutletNode;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		}}

	}

	//SUBROUTINE UpdateOutdoorAirUnit

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateOutdoorAirUnit

	void
	ReportOutdoorAirUnit( int const OAUnitNum ) // Index for the outdoor air unit under consideration within the derived types
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young T. Chae
		//       DATE WRITTEN   Oct. 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for the outdoor air unit.
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHeatBalance::Zone;
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

		OutAirUnit( OAUnitNum ).TotHeatingEnergy = OutAirUnit( OAUnitNum ).TotHeatingRate * TimeStepSys * SecInHour;
		OutAirUnit( OAUnitNum ).SensHeatingEnergy = OutAirUnit( OAUnitNum ).SensHeatingRate * TimeStepSys * SecInHour;
		OutAirUnit( OAUnitNum ).LatHeatingEnergy = OutAirUnit( OAUnitNum ).LatHeatingRate * TimeStepSys * SecInHour;
		OutAirUnit( OAUnitNum ).SensCoolingEnergy = OutAirUnit( OAUnitNum ).SensCoolingRate * TimeStepSys * SecInHour;
		OutAirUnit( OAUnitNum ).LatCoolingEnergy = OutAirUnit( OAUnitNum ).LatCoolingRate * TimeStepSys * SecInHour;
		OutAirUnit( OAUnitNum ).TotCoolingEnergy = OutAirUnit( OAUnitNum ).TotCoolingRate * TimeStepSys * SecInHour;
		OutAirUnit( OAUnitNum ).AirMassFlow = OutAirUnit( OAUnitNum ).OutAirMassFlow;
		OutAirUnit( OAUnitNum ).ElecFanEnergy = OutAirUnit( OAUnitNum ).ElecFanRate * TimeStepSys * SecInHour;

	}

	int
	GetOutdoorAirUnitOutAirNode( int const OAUnitNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetOutdoorAirUnitOutAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetOutdoorAirUnitInputFlag ) {
			GetOutdoorAirUnitInputs();
			GetOutdoorAirUnitInputFlag = false;
		}

		GetOutdoorAirUnitOutAirNode = 0;
		if ( OAUnitNum > 0 && OAUnitNum <= NumOfOAUnits ) {
			GetOutdoorAirUnitOutAirNode = OutAirUnit( OAUnitNum ).OutsideAirNode;
		}

		return GetOutdoorAirUnitOutAirNode;

	}

	int
	GetOutdoorAirUnitZoneInletNode( int const OAUnitNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetOutdoorAirUnitZoneInletNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetOutdoorAirUnitInputFlag ) {
			GetOutdoorAirUnitInputs();
			GetOutdoorAirUnitInputFlag = false;
		}

		GetOutdoorAirUnitZoneInletNode = 0;
		if ( OAUnitNum > 0 && OAUnitNum <= NumOfOAUnits ) {
			GetOutdoorAirUnitZoneInletNode = OutAirUnit( OAUnitNum ).AirOutletNode;
		}

		return GetOutdoorAirUnitZoneInletNode;

	}

	int
	GetOutdoorAirUnitReturnAirNode( int const OAUnitNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetOutdoorAirUnitReturnAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetOutdoorAirUnitInputFlag ) {
			GetOutdoorAirUnitInputs();
			GetOutdoorAirUnitInputFlag = false;
		}

		GetOutdoorAirUnitReturnAirNode = 0;
		if ( OAUnitNum > 0 && OAUnitNum <= NumOfOAUnits ) {
			GetOutdoorAirUnitReturnAirNode = OutAirUnit( OAUnitNum ).AirInletNode;
		}

		return GetOutdoorAirUnitReturnAirNode;

	}

	//*****************************************************************************************

} // OutdoorAirUnit

} // EnergyPlus
