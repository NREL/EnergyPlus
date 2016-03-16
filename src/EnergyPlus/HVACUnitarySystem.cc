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
#include <HVACUnitarySystem.hh>
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirflowNetwork.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACControllers.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HVACDXSystem.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <VariableSpeedCoils.hh>
#include <WaterCoils.hh>
#include <WaterToAirHeatPump.hh>
#include <WaterToAirHeatPumpSimple.hh>
#include <SimAirServingZones.hh>
#include <PackagedThermalStorageCoil.hh>
#include <UserDefinedComponents.hh>

namespace EnergyPlus {

namespace HVACUnitarySystem {
	// Module containing the Unitary System simulation routines
	// AirloopHVAC:UnitarySystem
	// Unitary System allows any coil type with fan and coils optional
	// Unitary System model can be placed anywhere in the simulation:
	//   (air loops, outside air systems, Outdoor air units, zone equipment)
	//   ( not fully tested for all configurations)
	// Routine calling order:
	//  SimUnitarySystem
	//    GetUnitarySystemInput
	//    InitUnitarySystems
	//    IF(SetPointBased Control)THEN
	//      ControlUnitarySystemToSP   ---->  SimFan (if exists and blowthru)
	//                                        UpdateUnitarySystemControl (cooling coil if exists)
	//                                        ControlCoolingSystem ---> Sim*CoolingCoil
	//                                        CalcUnitaryCoolingSystem
	//                                        UpdateUnitarySystemControl (heating coil if exists)
	//                                        ControlHeatingSystem ---> Sim*HeatingCoil
	//                                        CalcUnitaryHeatingSystem
	//                                        SimFan (if exists and drawthru)
	//                                        UpdateUnitarySystemControl (supp heating coil if exists)
	//                                        ControlSuppHeatingSystem ---> Sim*HeatingCoil
	//                                        CalcUnitarySuppSystemToSP
	//    ELSEIF(LoadBased Control)THEN
	//      ControlUnitarySystemToLoad ---->  UpdateUnitarySystemControl
	//                                        ControlUnitarySystemOutput ---> CalcUnitarySystemToLoad(PLR)
	//                                        CalcUnitarySystemToLoad(FinalPLR w/ supp heater operating)
	//    END IF
	//  ReportUnitarySystem(UnitarySysNum)
	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad, FSEC
	//       DATE WRITTEN   February 2013
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the Unitary System Component

	// METHODOLOGY EMPLOYED:
	// Calculates the part-load ratio of the HVAC system to meet the zone sensible load.
	// IF humidity control is specified and the latent capacity at the sensible PLR is insufficient to meet the latent load,
	// enable multimode operation and calculate a part-load ratio to meet the zone sensible load (MultiMode dehumidification control)
	// or the zone latent load (CoolReheat dehumidification control).
	// Subroutines:
	// SimUnitarySystem - Top level simulate routine CALLed by other modules. Each child object is simulated a final time after
	//                    the part-load ratio for child components has been determined.
	//  Note: A supplemental heater augments the heating capacity for both air-to-air and water-to-air heat pump systems.
	//        The supplemental heating coil may be present even if the system is not a heat pump.
	//        The supplemental heating coil is used in the unitarysystem to meet the sensible load when the
	//        primary heating coil is unable to meet the zone load.
	// Dehumidificaiton control options:
	// Dehumidification Control NONE:   If a HX assisted cooling coil is selected, the HX is always active (cooling).
	// Dehumidification Control COOLREHEAT: For cooling operation, the sensible capacity is calculated to
	//                                      meet the thermostat setpoint. If a HX assisted cooling coil is selected,
	//                                      the HX is always active. If the latent load is not met by operating the
	//                                      system at the sensible PLR, a new PLR is calculated to meet the humidistat
	//                                      setpoint. The supplemental heating coil load is then calculated to meet the
	//                                      HEATING setpoint temperature.
	// Dehumidification Control MULTIMODE: For cooling operation, the sensible capacity is calculated to
	//                                     meet the thermostat setpoint. If a HX assisted cooling coil is selected,
	//                                     the HX is off for this calculation. If the latent load is not met by operating
	//                                     the system at the sensible PLR, a new PLR is calculated with the HX operating
	//                                     and the target is the zone SENSIBLE load (thermostat setpoint). Humidity is not
	//                                     controlled in this mode. No reheat coil is needed in this configuration.
	// REFERENCES:

	// OTHER NOTES:  This module is intended to allow any configuration of coil types and location. All possible configurations
	//               have not been fully tested, however, the methodology is to treat all configurations the same. No special
	//               treatment is desired (e.g., IF coiltype == X THEN do this ELSE do something else). DX coils have not been
	//               included as supplemental heating coils, however, there is no reason other than the DX coil module may not
	//               allow a heating only DX system.
	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataAirLoop;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using namespace DataSizing;
	using namespace DataZoneEquipment;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::CurMnDy;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutWetBulbTemp;
	using DataEnvironment::StdRhoAir;

	// Use statements for access to subroutines in other modules
	using VariableSpeedCoils::MaxSpedLevels;
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS
	Real64 const MinAirMassFlow( 0.001 );

	// Last mode of operation
	int const CoolingMode( 1 ); // last compressor operating mode was in cooling
	int const HeatingMode( 2 ); // last compressor operating mode was in heating

	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	// Dehumidification control modes (DehumidControlMode)
	int const DehumidControl_None( 0 );
	int const DehumidControl_Multimode( 1 );
	int const DehumidControl_CoolReheat( 2 );

	// Coil type for SimWater and SimSteamCoil
	int const CoolingCoil( 0 );
	int const HeatingCoil( 1 );
	int const SuppHeatCoil( 2 );

	// Supply Air Sizing Option
	int const None( 1 );
	int const SupplyAirFlowRate( 2 );
	int const FlowPerFloorArea( 3 );
	int const FractionOfAutoSizedCoolingValue( 4 );
	int const FractionOfAutoSizedHeatingValue( 5 );
	int const FlowPerCoolingCapacity( 6 );
	int const FlowPerHeatingCapacity( 7 );

	// Airflow control for contant fan mode
	int const UseCompressorOnFlow( 1 ); // set compressor OFF air flow rate equal to compressor ON air flow rate
	int const UseCompressorOffFlow( 2 ); // set compressor OFF air flow rate equal to user defined value

	// System Control Type
	int const LoadBased( 1 ); // control system based on zone load
	int const SetPointBased( 2 ); // control system based on coil set point manager

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:

	namespace {
		bool InitUnitarySystemsOneTimeFlag( true ); // one time flag
		int NumUnitarySystemsSized( 0 ); // counter used to delete UnitarySystemNumericFields array after last system is sized
		bool SimUnitarySystemZoneEquipTestFlag( true );
		bool InitUnitarySystemsErrFlag( false );
		bool InitUnitarySystemsErrorsFound( false );
		bool InitLoadBasedControlOneTimeFlag( true );
		bool InitLoadBasedControlAirLoopPass( true );
		int AirLoopPassCounter( 0 );
		bool InitLoadBasedControlFlowFracFlagReady( true );
		Real64 InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax( 0.0 );
		Real64 InitUnitarySystemsQActual( 0.0 );
	}

	bool GetInputFlag( true ); // Flag to get input only once
	bool EconomizerFlag( false ); // holds air loop economizer status
	bool HeatingLoad( false ); // True when zone needs heating
	bool CoolingLoad( false ); // True when zone needs cooling
	Real64 MoistureLoad( 0.0 ); // Dehumidification Load (W)
	bool SuppHeatingCoilFlag( false ); // set to TRUE when simulating supplemental heating coil
	int NumUnitarySystem( 0 ); // The Number of Unitary Systems found in the Input
	int NumDesignSpecMultiSpeedHP( 0 ); // The number of design specification objects for MSHP
	Real64 CompOnMassFlow( 0.0 ); // Supply air mass flow rate w/ compressor ON [kg/s]
	Real64 CompOffMassFlow( 0.0 ); // Supply air mass flow rate w/ compressor OFF [kg/s]
	Real64 CompOnFlowRatio( 0.0 ); // fan flow ratio when coil on
	Real64 CompOffFlowRatio( 0.0 ); // fan flow ratio when coil off
	Real64 FanSpeedRatio( 0.0 ); // ratio of air flow ratio passed to fan object
	Real64 CoolHeatPLRRat( 1.0 ); // ratio of cooling to heating PLR, used for cycling fan RH control
	Real64 OnOffAirFlowRatioSave( 0.0 ); // Saves the OnOffAirFlowRatio calculated in RegulaFalsi calls.
	Real64 QToCoolSetPt( 0.0 ); // load to cooling set point {W}
	Real64 QToHeatSetPt( 0.0 ); // load to heating set point {W}
	Real64 TempSteamIn( 100.0 ); // steam coil steam inlet temperature

	// Allocatable types
	Array1D_bool CheckEquipName;
	Array1D_bool MyEnvrnFlag; // environment flag
	Array1D_bool MultiOrVarSpeedHeatCoil;
	Array1D_bool MultiOrVarSpeedCoolCoil;
	Array1D_bool MyPlantScanFlag; // used for finding on heat recovery plant loop
	Array1D_bool MySuppCoilPlantScanFlag; // used for finding on heat recovery plant loop
	Array1D_bool MySetPointCheckFlag; // tests for set point
	Array1D_bool MySizingCheckFlag; // tests for sizing

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Initialization routines

	// Get Input routines

	// Control routines to find PLR, check convergence and update nodes

	// Calc routines to simulate each child component in order

	// set point based calc routine
	// Load based calc routine

	// Airflow control routines

	// Verify set point exists for SetPointBased control
	// Heat recovery subroutine
	// Reporting routines for module

	// RegulaFalsi routines
	// ** RAR I'd rather see a SELECT CASE in 1 or 2 generic routines instead of one for each coil type

	// Object Data
	Array1D< DesignSpecMSHPData > DesignSpecMSHP;
	Array1D< UnitarySystemData > UnitarySystem;
	Array1D< UnitarySystemNumericFieldData > UnitarySystemNumericFields;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimUnitarySystem(
		std::string const & UnitarySystemName, // Name of Unitary System object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to Unitary System object
		Optional_bool HeatActive, // True if heat coil active
		Optional_bool CoolActive, // True if cool coil active
		Optional_int_const OAUnitNum, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
		Optional_bool_const ZoneEquipment // TRUE if called as zone equipment
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages unitary system component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using DataAirLoop::AirLoopControlInfo;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // Index to AirloopHVAC:UnitarySystem object
		bool HXUnitOn; // Flag to control HX for HXAssisted Cooling Coil
		int CompOn; // Determines if compressor is on or off
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyZoneEquipTestFlag( true ); // SimUnitarySystemZoneEquipTestFlag
		////////////////////////////////////////////////////////////////////////////////////

		CompOn = 0; //Autodesk:Init Was used uninitialized

		// Airloop inputs are filled after zone equipment has simulated. Wait for air loop equipment to simulate. Zone equipment will not simulate the first few times through.
		// This is only a problem is unitary systems are used as zone AND airloop equipment in the same input file.
		if ( SimUnitarySystemZoneEquipTestFlag ) {
			if ( present(ZoneEquipment) && FirstHVACIteration ) {
				return;
			} else {
				SimUnitarySystemZoneEquipTestFlag = false;
			}
		}
		// Obtains and Allocates unitary system related parameters from input file
		if ( GetInputFlag ) {
			// Get the unitary system input
			GetUnitarySystemInput();
			GetInputFlag = false;
		}

		// Find the correct unitary system Number
		if ( CompIndex == 0 ) {
			UnitarySysNum = FindItemInList( UnitarySystemName, UnitarySystem );
			if ( UnitarySysNum == 0 ) {
				ShowFatalError( "SimDXCoolingSystem: DXUnit not found=" + UnitarySystemName );
			}
			CompIndex = UnitarySysNum;
		} else {
			UnitarySysNum = CompIndex;
			if ( UnitarySysNum > NumUnitarySystem || UnitarySysNum < 1 ) {
				ShowFatalError( "SimUnitarySystem:  Invalid CompIndex passed=" + TrimSigDigits( UnitarySysNum ) + ", Number of Unit Systems=" + TrimSigDigits( NumUnitarySystem ) + ", Unitary System name=" + UnitarySystemName );
			}
			if ( CheckEquipName( UnitarySysNum ) ) {
				if ( UnitarySystemName != UnitarySystem( UnitarySysNum ).Name ) {
					ShowFatalError( "SimUnitarySystem: Invalid CompIndex passed=" + TrimSigDigits( UnitarySysNum ) + ", Unitary System name=" + UnitarySystemName + ", stored Unit Name for that index=" + UnitarySystem( UnitarySysNum ).Name );
				}
				CheckEquipName( UnitarySysNum ) = false;
			}
		}

		if ( present( HeatActive ) ) HeatActive = false;
		if ( present( CoolActive ) ) CoolActive = false;

		FanSpeedRatio = 1.0;
		if ( present( ZoneEquipment ) ) {
			InitUnitarySystems( UnitarySysNum, 0, FirstHVACIteration, OAUnitNum, OAUCoilOutTemp );
		} else {
			InitUnitarySystems( UnitarySysNum, AirLoopNum, FirstHVACIteration, OAUnitNum, OAUCoilOutTemp );
		}

		HXUnitOn = false;
		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).ControlType );
		if ( SELECT_CASE_var == SetPointBased ) {
			if ( present( ZoneEquipment ) ) {
				ControlUnitarySystemtoSP( UnitarySysNum, 0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn );
			} else {
				ControlUnitarySystemtoSP( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn );
			}
		} else if ( SELECT_CASE_var == LoadBased ) {
			if ( present( ZoneEquipment ) ) {
				ControlUnitarySystemtoLoad( UnitarySysNum, 0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn );
			} else {
				ControlUnitarySystemtoLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn );
			}
		}}

		// Report the current output
		if ( present( ZoneEquipment ) ) {
			ReportUnitarySystem( UnitarySysNum, 0 );
		} else {
			ReportUnitarySystem( UnitarySysNum, AirLoopNum );
		}

		if ( present( CoolActive ) ) {
			if ( UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac * double( CompOn ) > 0.0 ) CoolActive = true;
		}
		if ( present( HeatActive ) ) {
			if ( UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac * double( CompOn ) > 0.0 || UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac * double( CompOn ) > 0.0 ) HeatActive = true;
		}

		// set econo lockout flag
		// If the sysem is not an equipment of Outdoor air unit
		//  IF (AirLoopNum /=-1 .AND. ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment) THEN
		if ( AirLoopNum > 0 && allocated( AirLoopControlInfo ) && UnitarySystem( UnitarySysNum ).AirLoopEquipment ) {

			if ( ( UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio > 0.0 || UnitarySystem( UnitarySysNum ).SpeedRatio > 0.0 || UnitarySystem( UnitarySysNum ).CycRatio > 0.0 ) && AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor ) {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithCompressor = true;
			} else {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithCompressor = false;
			}

			if ( ( HeatActive ) && ( AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor || AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithHeating ) ) {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithHeating = true;
			} else {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithHeating = false;
			}

		}

		// Calculate heat recovery
		if ( UnitarySystem( UnitarySysNum ).HeatRecActive ) {
			UnitarySystemHeatRecovery( UnitarySysNum );
		}

		// Coils should have been sized by now. Set this flag to false in case other equipment is downstream of Unitary System.
		// can't do this since there are other checks that need this flag (e.g., HVACManager, line 3577)
		//  AirLoopControlInfo(AirLoopNum)%UnitarySys = .FALSE.

	}

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitUnitarySystems(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		bool const FirstHVACIteration, // True when first HVAC iteration
		Optional_int_const OAUnitNum, // number of the current Outdoor air unit being simulated
		Optional< Real64 const > OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the unitary systems.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirLoop::AirLoopControlInfo;
		using DataAirflowNetwork::AirflowNetworkUnitarySystem;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_UnitarySystemRecovery;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::SimulateWaterCoilComponents;
		using WaterCoils::SetCoilDesFlow;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilIndex;
		using HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum;
		using SteamCoils::GetCoilMaxSteamFlowRate;
		using SteamCoils::SimulateSteamCoilComponents;
		auto & GetSteamCoilCapacity( SteamCoils::GetCoilCapacity );
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineNames( "InitUnitarySystems" );
		static std::string const RoutineName( "InitUnitarySystem" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string CoolingCoilType; // Coil:Cooling:Water or Coil:Cooling:Water:DetailedGeometry
		static std::string CoolingCoilName; // Coil:Cooling:Water or Coil:Cooling:Water:DetailedGeometry
		static std::string HeatingCoilType; // Coil:Heating:Water or Coil:Heating:Steam
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool errFlag( false ); // error flag for mining functions // InitUnitarySystemsErrFlag
		// static bool ErrorsFound( false ); // error flag for mining functions // InitUnitarySystemsErrorsFound
		// static Real64 QActual( 0.0 ); // coil actual capacity [W] // InitUnitarySystemsQActual
		////////////////////////////////////////////////////////////////////////////////////
		int ControlNode; // control node number
		int OutdoorAirUnitNum; // "ONLY" for ZoneHVAC:OutdoorAirUnit
		int SteamIndex( 0 ); // index of steam quality for steam heating coil
		int TypeOfCoilWaterCooling( 0 ); // Used for simple water cool coil or detailed geometry
		int TypeOfCoilWaterHeating( 0 ); // Used for simple water heat coil or steam coil
		Real64 OAUCoilOutletTemp( 0.0 ); // "ONLY" for zoneHVAC:OutdoorAirUnit [C]
		Real64 mdot( 0.0 ); // local temporary for mass flow rate (kg/s)
		Real64 SteamDensity( 0.0 ); // density of steam at 100C, used for steam heating coils [kg/m3]
		Real64 CoilMaxVolFlowRate( 0.0 ); // coil fluid maximum volume flow rate [m3/s]
		Real64 rho( 0.0 ); // local fluid density [kg/m3]
		Real64 mdotHR( 0.0 ); // heat recovery mass flow rate [kg/s]
		//  REAL(r64)           :: SaveMassFlow            = 0.0d0     ! saves node flow rate when checking heat coil capacity [m3/s]

		if ( InitUnitarySystemsOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumUnitarySystem );
			MyPlantScanFlag.allocate( NumUnitarySystem );
			MySuppCoilPlantScanFlag.allocate( NumUnitarySystem );
			MySetPointCheckFlag.allocate( NumUnitarySystem );
			MySizingCheckFlag.allocate( NumUnitarySystem );

			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MySuppCoilPlantScanFlag = true;
			MySetPointCheckFlag = true;
			MySizingCheckFlag = true;

			InitUnitarySystemsOneTimeFlag = false;
			AirflowNetworkUnitarySystem = true;
		}

		if ( ! SysSizingCalc && MySizingCheckFlag( UnitarySysNum ) ) {
			if ( AirLoopNum > 0 ) {
				if ( UnitarySystem( UnitarySysNum ).FanExists && ( UnitarySystem( UnitarySysNum ).CoolCoilExists && ( UnitarySystem( UnitarySysNum ).HeatCoilExists || UnitarySystem( UnitarySysNum ).SuppCoilExists ) ) ) AirLoopControlInfo( AirLoopNum ).UnitarySys = true;
				AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = true;
			}
			SizeUnitarySystem( UnitarySysNum, FirstHVACIteration, AirLoopNum );
			MySizingCheckFlag( UnitarySysNum ) = false;
			if ( AirLoopNum > 0 ) {
				AirLoopControlInfo( AirLoopNum ).FanOpMode = UnitarySystem( UnitarySysNum ).FanOpMode;
				AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr = UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr;
			}
		}

		if ( AirLoopNum == -1 ) { // This DX system is component of ZoneHVAC:OutdoorAirUnit
			OutdoorAirUnitNum = OAUnitNum;
			OAUCoilOutletTemp = OAUCoilOutTemp;
		}

		// Scan hot water and steam heating coil plant components for one time initializations
		if ( MyPlantScanFlag( UnitarySysNum ) && allocated( PlantLoop ) ) {
			if ( UnitarySystem( UnitarySysNum ).HeatRecActive ) {
				InitUnitarySystemsErrFlag = false;
				ScanPlantLoopsForObject( UnitarySystem( UnitarySysNum ).Name, TypeOf_UnitarySystemRecovery, UnitarySystem( UnitarySysNum ).HRLoopNum, UnitarySystem( UnitarySysNum ).HRLoopSideNum, UnitarySystem( UnitarySysNum ).HRBranchNum, UnitarySystem( UnitarySysNum ).HRCompNum, _, _, _, _, _, InitUnitarySystemsErrFlag );
				if ( InitUnitarySystemsErrFlag ) {
					ShowFatalError( "InitUnitarySystems: Program terminated for previous conditions." );
				}
			}
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilWater_CoolingHXAssisted ) {
				if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater ) {
					TypeOfCoilWaterCooling = TypeOf_CoilWaterCooling;
					CoolingCoilType = "Coil:Cooling:Water";
					CoolingCoilName = UnitarySystem( UnitarySysNum ).CoolingCoilName;
				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {
					TypeOfCoilWaterCooling = TypeOf_CoilWaterDetailedFlatCooling;
					CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
					CoolingCoilName = UnitarySystem( UnitarySysNum ).CoolingCoilName;
				} else {
					TypeOfCoilWaterCooling = GetCoilObjectTypeNum( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, InitUnitarySystemsErrFlag, true );
					if ( TypeOfCoilWaterCooling == Coil_CoolingWater ) {
						TypeOfCoilWaterCooling = TypeOf_CoilWaterCooling;
						CoolingCoilType = "Coil:Cooling:Water";
					} else if ( TypeOfCoilWaterCooling == Coil_CoolingWaterDetailed ) {
						TypeOfCoilWaterCooling = TypeOf_CoilWaterDetailedFlatCooling;
						CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
					}
					CoolingCoilName = GetHXDXCoilName( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, InitUnitarySystemsErrFlag );
				}
				InitUnitarySystemsErrFlag = false;
				ScanPlantLoopsForObject( CoolingCoilName, TypeOfCoilWaterCooling, UnitarySystem( UnitarySysNum ).CoolCoilLoopNum, UnitarySystem( UnitarySysNum ).CoolCoilLoopSide, UnitarySystem( UnitarySysNum ).CoolCoilBranchNum, UnitarySystem( UnitarySysNum ).CoolCoilCompNum, _, _, _, _, _, InitUnitarySystemsErrFlag );
				if ( InitUnitarySystemsErrFlag ) {
					ShowFatalError( "InitUnitarySystem: Program terminated for previous conditions." );
				}
				UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow = GetCoilMaxWaterFlowRate( CoolingCoilType, CoolingCoilName, InitUnitarySystemsErrorsFound );

				if ( UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).CoolCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).CoolCoilLoopNum ).FluidIndex, RoutineName );
					UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow *= rho;
				}
				// fill outlet node for coil
				UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum = PlantLoop( UnitarySystem( UnitarySysNum ).CoolCoilLoopNum ).LoopSide( UnitarySystem( UnitarySysNum ).CoolCoilLoopSide ).Branch( UnitarySystem( UnitarySysNum ).CoolCoilBranchNum ).Comp( UnitarySystem( UnitarySysNum ).CoolCoilCompNum ).NodeNumOut;

			}
			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingSteam ) {
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
					TypeOfCoilWaterHeating = TypeOf_CoilWaterSimpleHeating;
					HeatingCoilType = "Coil:Heating:Water";
					SetCoilDesFlow( cAllCoilTypes( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num ), UnitarySystem( UnitarySysNum ).HeatingCoilName, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow, InitUnitarySystemsErrorsFound );
				} else {
					TypeOfCoilWaterHeating = TypeOf_CoilSteamAirHeating;
					HeatingCoilType = "Coil:Heating:Steam";
				}
				InitUnitarySystemsErrFlag = false;
				ScanPlantLoopsForObject( UnitarySystem( UnitarySysNum ).HeatingCoilName, TypeOfCoilWaterHeating, UnitarySystem( UnitarySysNum ).HeatCoilLoopNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopSide, UnitarySystem( UnitarySysNum ).HeatCoilBranchNum, UnitarySystem( UnitarySysNum ).HeatCoilCompNum, _, _, _, _, _, InitUnitarySystemsErrFlag );
				if ( InitUnitarySystemsErrFlag ) {
					ShowFatalError( "InitUnitarySystem: Program terminated for previous conditions." );
				}
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
					UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( HeatingCoilType, UnitarySystem( UnitarySysNum ).HeatingCoilName, InitUnitarySystemsErrorsFound );

					if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).FluidIndex, RoutineName );
						UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( HeatingCoilType, UnitarySystem( UnitarySysNum ).HeatingCoilName, InitUnitarySystemsErrorsFound ) * rho;
					}
				} else {
					UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, InitUnitarySystemsErrorsFound );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow *= SteamDensity;
					}
				}
				// fill outlet node for coil
				UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum = PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).LoopSide( UnitarySystem( UnitarySysNum ).HeatCoilLoopSide ).Branch( UnitarySystem( UnitarySysNum ).HeatCoilBranchNum ).Comp( UnitarySystem( UnitarySysNum ).HeatCoilCompNum ).NodeNumOut;
			}

			MyPlantScanFlag( UnitarySysNum ) = false;

		} else if ( MyPlantScanFlag( UnitarySysNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( UnitarySysNum ) = false;
		}

		// Scan Supplemental hot water and steam heating coil plant components for one time initializations
		if ( MySuppCoilPlantScanFlag( UnitarySysNum ) && allocated( PlantLoop ) ) {
			if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
				InitUnitarySystemsErrFlag = false;
				ScanPlantLoopsForObject( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, TypeOf_CoilWaterSimpleHeating, UnitarySystem( UnitarySysNum ).SuppCoilLoopNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopSide, UnitarySystem( UnitarySysNum ).SuppCoilBranchNum, UnitarySystem( UnitarySysNum ).SuppCoilCompNum, _, _, _, _, _, InitUnitarySystemsErrFlag );
				SetCoilDesFlow( cAllCoilTypes( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num ), UnitarySystem( UnitarySysNum ).SuppHeatCoilName, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow, InitUnitarySystemsErrorsFound );

				if ( InitUnitarySystemsErrFlag ) {
					ShowFatalError( "InitUnitarySystems: Program terminated for previous conditions." );
				}
				UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", UnitarySystem( UnitarySysNum ).SuppHeatCoilName, InitUnitarySystemsErrorsFound );

				if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).FluidIndex, RoutineNames );
					UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", UnitarySystem( UnitarySysNum ).SuppHeatCoilName, InitUnitarySystemsErrorsFound ) * rho;
				}
				// fill outlet node for coil
				UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum = PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).LoopSide( UnitarySystem( UnitarySysNum ).SuppCoilLoopSide ).Branch( UnitarySystem( UnitarySysNum ).SuppCoilBranchNum ).Comp( UnitarySystem( UnitarySysNum ).SuppCoilCompNum ).NodeNumOut;

			} else if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {
				InitUnitarySystemsErrFlag = false;
				ScanPlantLoopsForObject( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, TypeOf_CoilSteamAirHeating, UnitarySystem( UnitarySysNum ).SuppCoilLoopNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopSide, UnitarySystem( UnitarySysNum ).SuppCoilBranchNum, UnitarySystem( UnitarySysNum ).SuppCoilCompNum, _, _, _, _, _, InitUnitarySystemsErrFlag );
				if ( InitUnitarySystemsErrFlag ) {
					ShowFatalError( "InitUnitarySystems: Program terminated for previous conditions." );
				}
				UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, InitUnitarySystemsErrorsFound );
				if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow > 0.0 ) {
					SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNames );
					UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow *= SteamDensity;
				}

				// fill outlet node for coil
				UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum = PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).LoopSide( UnitarySystem( UnitarySysNum ).SuppCoilLoopSide ).Branch( UnitarySystem( UnitarySysNum ).SuppCoilBranchNum ).Comp( UnitarySystem( UnitarySysNum ).SuppCoilCompNum ).NodeNumOut;
			}

			MySuppCoilPlantScanFlag( UnitarySysNum ) = false;

		} else if ( MySuppCoilPlantScanFlag( UnitarySysNum ) && ! AnyPlantInModel ) {
			MySuppCoilPlantScanFlag( UnitarySysNum ) = false;
		}

		// do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( UnitarySysNum ) ) {
			UnitarySystem( UnitarySysNum ).DesignMassFlowRate = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate * StdRhoAir;
			UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow * StdRhoAir;
			UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow * StdRhoAir;
			UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow * StdRhoAir;
			UnitarySystem( UnitarySysNum ).SenLoadLoss = 0.0;
			if ( UnitarySystem( UnitarySysNum ).Humidistat ) {
				UnitarySystem( UnitarySysNum ).LatLoadLoss = 0.0;
			}

			if ( ( UnitarySystem( UnitarySysNum ).HeatRecActive ) && ( ! MyPlantScanFlag( UnitarySysNum ) ) ) {

				rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).HRLoopNum ).FluidName, 60.0, PlantLoop( UnitarySystem( UnitarySysNum ).HRLoopNum ).FluidIndex, RoutineNames );

				UnitarySystem( UnitarySysNum ).DesignHeatRecMassFlowRate = UnitarySystem( UnitarySysNum ).DesignHRWaterVolumeFlow * rho;

				InitComponentNodes( 0.0, UnitarySystem( UnitarySysNum ).DesignHeatRecMassFlowRate, UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum, UnitarySystem( UnitarySysNum ).HeatRecoveryOutletNodeNum, UnitarySystem( UnitarySysNum ).HRLoopNum, UnitarySystem( UnitarySysNum ).HRLoopSideNum, UnitarySystem( UnitarySysNum ).HRBranchNum, UnitarySystem( UnitarySysNum ).HRCompNum );
			}
			//   set fluid-side hardware limits
			if ( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode > 0 ) {

				if ( UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow == AutoSize ) {
					// If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
					if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater ) {
						CoolingCoilType = "Coil:Cooling:Water";
					} else {
						CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
					}
					SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).CoolingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
					CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( CoolingCoilType, UnitarySystem( UnitarySysNum ).CoolingCoilName, InitUnitarySystemsErrorsFound );
					if ( CoilMaxVolFlowRate != AutoSize ) {
						rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).CoolCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).CoolCoilLoopNum ).FluidIndex, RoutineName );
						UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow = CoilMaxVolFlowRate * rho;
					}
				}

				InitComponentNodes( 0.0, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow, UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode, UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).CoolCoilLoopNum, UnitarySystem( UnitarySysNum ).CoolCoilLoopSide, UnitarySystem( UnitarySysNum ).CoolCoilBranchNum, UnitarySystem( UnitarySysNum ).CoolCoilCompNum );
			}
			if ( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode > 0 ) {

				if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow == AutoSize ) {
					// IF water coil max water flow rate is autosized, simulate once in order to mine max flow rate
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
						SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex );
						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", UnitarySystem( UnitarySysNum ).HeatingCoilName, InitUnitarySystemsErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).FluidIndex, RoutineNames );
							UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
						}
					}
					// If steam coil max steam flow rate is autosized, simulate once in order to mine max flow rate
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingSteam ) {
						SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, 1.0, InitUnitarySystemsQActual ); //QCoilReq, simulate any load > 0 to get max capacity
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, InitUnitarySystemsErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNames );
							UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
						}
					}
				}

				InitComponentNodes( 0.0, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow, UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode, UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopSide, UnitarySystem( UnitarySysNum ).HeatCoilBranchNum, UnitarySystem( UnitarySysNum ).HeatCoilCompNum );
			}
			if ( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode > 0 ) {
				if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow == AutoSize ) {
					if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
						// If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
						SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex );
						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", UnitarySystem( UnitarySysNum ).SuppHeatCoilName, InitUnitarySystemsErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).FluidIndex, RoutineNames );
							UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
						}
					}
					if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {
						SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, 1.0, InitUnitarySystemsQActual ); //QCoilReq, simulate any load > 0 to get max capacity
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, InitUnitarySystemsErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNames );
							UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
						}
					}
					InitComponentNodes( 0.0, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow, UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode, UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopSide, UnitarySystem( UnitarySysNum ).SuppCoilBranchNum, UnitarySystem( UnitarySysNum ).SuppCoilCompNum );
				}
			}
			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric ) {
//				SimulateHeatingCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, 1.0, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, _, UnitarySystem( UnitarySysNum ).FanOpMode, 1.0 );
//				UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetHeatingCoilCapacity( cAllCoilTypes( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num ), UnitarySystem( UnitarySysNum ).HeatingCoilName, InitUnitarySystemsErrFlag );
			}
			MyEnvrnFlag( UnitarySysNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( UnitarySysNum ) = true;
		}

		//Init maximum available Heat Recovery flow rate
		if ( ( UnitarySystem( UnitarySysNum ).HeatRecActive ) && ( ! MyPlantScanFlag( UnitarySysNum ) ) ) {
			if ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr ) > 0.0 ) {
				if ( FirstHVACIteration ) {
					mdotHR = UnitarySystem( UnitarySysNum ).DesignHeatRecMassFlowRate;
				} else {
					if ( UnitarySystem( UnitarySysNum ).HeatRecoveryMassFlowRate > 0.0 ) {
						mdotHR = UnitarySystem( UnitarySysNum ).HeatRecoveryMassFlowRate;
					} else {
						mdotHR = UnitarySystem( UnitarySysNum ).DesignHeatRecMassFlowRate;
					}
				}
			} else {
				mdotHR = 0.0;
			}

			mdotHR = min( Node( UnitarySystem( UnitarySysNum ).HeatRecoveryOutletNodeNum ).MassFlowRateMaxAvail, mdotHR );
			Node( UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum ).MassFlowRate = mdotHR;
		}

		// get operating capacity of water and steam coil
		if ( FirstHVACIteration || UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat ) {
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {
				//     set air-side and steam-side mass flow rates
				mdot = min( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow );
				Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = mdot;
				//     simulate water coil to find operating capacity
				SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).CoolingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, InitUnitarySystemsQActual );
				UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = InitUnitarySystemsQActual;
			} // from IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. Coil_CoolingWaterDetailed
			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
				//     set air-side and steam-side mass flow rates
				mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow );
				Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;
				//     simulate water coil to find operating capacity
				//      SaveMassFlow = Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate
				//      Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
				SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, InitUnitarySystemsQActual );
				UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = InitUnitarySystemsQActual;
				//      Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate = SaveMassFlow
			} // from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow );
				Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, 1.0, InitUnitarySystemsQActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil

				UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetSteamCoilCapacity( cAllCoilTypes( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num ), UnitarySystem( UnitarySysNum ).HeatingCoilName, InitUnitarySystemsErrorsFound );
			} // from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
			if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
				//     set air-side and steam-side mass flow rates
				mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow );
				Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;
				//      SaveMassFlow = Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate
				//      Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
				//     simulate water coil to find operating capacity
				if ( mdot > 0.0 ) {
					//        Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate = &
					//          UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
					SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, InitUnitarySystemsQActual );
					UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity = InitUnitarySystemsQActual;
				} else {
					UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity = 0.0;
				}
				//      Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate = SaveMassFlow

			} // from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

			if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow );
				Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, 1.0, InitUnitarySystemsQActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
				UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity = GetSteamCoilCapacity( "Coil:Heating:Steam", UnitarySystem( UnitarySysNum ).SuppHeatCoilName, InitUnitarySystemsErrorsFound );

			} // from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
		} // from IF( FirstHVACIteration ) THEN

		if ( MySetPointCheckFlag( UnitarySysNum ) ) {
			if ( ! SysSizingCalc && DoSetPointTest ) {

				if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					ControlNode = UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum;
					if ( ControlNode > 0 ) {
						CheckNodeSetPoint( UnitarySysNum, AirLoopNum, ControlNode, CoolingCoil, OAUCoilOutTemp );
					}
				}

				if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
					ControlNode = UnitarySystem( UnitarySysNum ).SystemHeatControlNodeNum;
					if ( ControlNode > 0 ) {
						CheckNodeSetPoint( UnitarySysNum, AirLoopNum, ControlNode, HeatingCoil, OAUCoilOutTemp );
					}
				}

				if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
					ControlNode = UnitarySystem( UnitarySysNum ).SuppHeatControlNodeNum;
					if ( ControlNode > 0 ) {
						CheckNodeSetPoint( UnitarySysNum, AirLoopNum, ControlNode, SuppHeatCoil, OAUCoilOutTemp );
					}
				}

				MySetPointCheckFlag( UnitarySysNum ) = false;
			}
		}

		UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 0.0;
		UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = 0.0;
		UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = 0.0;
		UnitarySystem( UnitarySysNum ).CoolingCycRatio = 0.0;
		UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
		UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0.0;
		UnitarySystem( UnitarySysNum ).HeatingCycRatio = 0.0;
		UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
		UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0.0;
		UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand = 0.0;
		UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand = 0.0;
		UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand = 0.0;
		UnitarySystem( UnitarySysNum ).DehumidInducedHeatingDemandRate = 0.0;

		UnitarySystem( UnitarySysNum ).InitHeatPump = true;

	}

	void
	CheckNodeSetPoint(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		int const ControlNode, // Node to test for set point
		int const CoilType, // True if cooling coil, then test for HumRatMax set point
		Optional< Real64 const > OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   March 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks for proper set point at control node.

		// METHODOLOGY EMPLOYED:
		// Uses the control node to test for set point.

		// REFERENCES:
		// na

		// Using/Aliasing
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iHumidityRatioMaxSetPoint;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( AirLoopNum == -1 ) { // Outdoor Air Unit
			Node( ControlNode ).TempSetPoint = OAUCoilOutTemp; // Set the coil outlet temperature
			if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil ) {
				FrostControlSetPointLimit( UnitarySysNum, UnitarySystem( UnitarySysNum ).DesiredOutletTemp, Node( ControlNode ).HumRatMax, OutBaroPress, UnitarySystem( UnitarySysNum ).DOASDXCoolingCoilMinTout, 1 );
			}
		} else if ( AirLoopNum != -1 ) { // Not an Outdoor air unit

			if ( Node( ControlNode ).TempSetPoint == SensedNodeFlagValue && UnitarySystem( UnitarySysNum ).ControlType == SetPointBased ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					ShowSevereError( UnitarySystem( UnitarySysNum ).UnitarySystemType + ": Missing temperature setpoint for unitary system = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the coil control node." );
					SetPointErrorFlag = true;
				} else {
					CheckIfNodeSetPointManagedByEMS( ControlNode, iTemperatureSetPoint, SetPointErrorFlag );
					if ( SetPointErrorFlag ) {
						ShowSevereError( UnitarySystem( UnitarySysNum ).UnitarySystemType + ": Missing temperature setpoint for unitary system = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the coil control node." );
						ShowContinueError( "  or use an EMS actuator to establish a temperature setpoint at the coil control node." );
					}
				}
			}
			if ( ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_None ) && ( Node( ControlNode ).HumRatMax == SensedNodeFlagValue ) && UnitarySystem( UnitarySysNum ).ControlType == SetPointBased && CoilType == CoolingCoil ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					ShowSevereError( UnitarySystem( UnitarySysNum ).UnitarySystemType + ": Missing humidity ratio setpoint (HUMRATMAX) for unitary system = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the coil control node." );
					SetPointErrorFlag = true;
				} else {
					CheckIfNodeSetPointManagedByEMS( ControlNode, iHumidityRatioMaxSetPoint, SetPointErrorFlag );
					if ( SetPointErrorFlag ) {
						ShowSevereError( UnitarySystem( UnitarySysNum ).UnitarySystemType + ": Missing maximum humidity ratio setpoint (HUMRATMAX) for unitary system = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the coil control node." );
						ShowContinueError( "  or use an EMS actuator to establish a maximum humidity ratio setpoint." );
					}
				}

			}
		}

	}

	void
	UpdateUnitarySystemControl(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		int const OutNode, // coil outlet node number
		int const ControlNode, // control node number
		Real64 & OnOffAirFlowRatio,
		bool const FirstHVACIteration,
		Optional< Real64 const > OAUCoilOutletTemp, // "ONLY" for zoneHVAC:OutdoorAirUnit
		Optional< Real64 > ZoneLoad,
		Optional< Real64 const > MaxOutletTemp // limits heating coil outlet temp [C]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing unitary systems.

		// METHODOLOGY EMPLOYED:
		// Either CALL the coil model to get the size or size coil.
		// Current method is to use same methodology as is used in coil objects.
		// Future changes will include a common sizing algorithm and push the calculated
		// size to the coil object prior to first call (so the coil will not autosize).

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirLoop::AirLoopControlInfo;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using DataHeatBalFanSys::TempControlType;
		using namespace DataZoneEnergyDemands;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ControlType;
		Real64 H2OHtOfVap; // Heat of vaporization of air

		ControlType = UnitarySystem( UnitarySysNum ).ControlType;
		// These initializations are done every iteration

		{ auto const SELECT_CASE_var( ControlType );
		if ( SELECT_CASE_var == LoadBased ) {
			if ( AirLoopNum == -1 ) { // This IF-THEN routine is just for ZoneHVAC:OutdoorAirUnit
				ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\"" );
				ShowFatalError( "...Load based control is not allowed when used with ZoneHVAC:OutdoorAirUnit" );
			}

			// here we need to deal with sequenced zone equip
			HeatingLoad = false;
			CoolingLoad = false;
			if ( UnitarySystem( UnitarySysNum ).ZoneSequenceCoolingNum > 0 && UnitarySystem( UnitarySysNum ).ZoneSequenceHeatingNum > 0 ) {
				QToCoolSetPt = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).SequencedOutputRequiredToCoolingSP( UnitarySystem( UnitarySysNum ).ZoneSequenceCoolingNum );
				QToHeatSetPt = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).SequencedOutputRequiredToHeatingSP( UnitarySystem( UnitarySysNum ).ZoneSequenceHeatingNum );
				if ( QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 && TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) != SingleCoolingSetPoint ) {
					ZoneLoad = QToHeatSetPt;
					HeatingLoad = true;
				} else if ( QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 && TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) == SingleCoolingSetPoint ) {
					ZoneLoad = 0.0;
				} else if ( QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 && TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) != SingleHeatingSetPoint ) {
					ZoneLoad = QToCoolSetPt;
					CoolingLoad = true;
				} else if ( QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 && TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) == SingleHeatingSetPoint ) {
					ZoneLoad = 0.0;
				} else if ( QToHeatSetPt <= 0.0 && QToCoolSetPt >= 0.0 ) {
					ZoneLoad = 0.0;
				}
				MoistureLoad = ZoneSysMoistureDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).SequencedOutputRequiredToDehumidSP( UnitarySystem( UnitarySysNum ).ZoneSequenceCoolingNum );
			} else {
				ZoneLoad = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).RemainingOutputRequired;
				QToCoolSetPt = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).OutputRequiredToCoolingSP;
				QToHeatSetPt = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).OutputRequiredToHeatingSP;
				MoistureLoad = ZoneSysMoistureDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).OutputRequiredToDehumidifyingSP;
			}

			if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_None ) {
				H2OHtOfVap = PsyHfgAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).HumRat, Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp );

				// positive MoistureLoad means no dehumidification load
				MoistureLoad = min( 0.0, MoistureLoad * H2OHtOfVap );
			} else {
				MoistureLoad = 0.0;
			}

			InitLoadBasedControl( UnitarySysNum, AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad );

			// EMS override point
			if ( UnitarySystem( UnitarySysNum ).EMSOverrideSensZoneLoadRequest ) ZoneLoad = UnitarySystem( UnitarySysNum ).EMSSensibleZoneLoadValue;
			if ( UnitarySystem( UnitarySysNum ).EMSOverrideMoistZoneLoadRequest ) MoistureLoad = UnitarySystem( UnitarySysNum ).EMSMoistureZoneLoadValue;

		} else if ( SELECT_CASE_var == SetPointBased ) {
			if ( AirLoopNum == -1 ) { // This IF-THEN routine is just for ZoneHVAC:OutdoorAIRUNIT

				if ( ControlNode == 0 ) {
					UnitarySystem( UnitarySysNum ).DesiredOutletTemp = OAUCoilOutletTemp;
					UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = 1.0;
				} else if ( ControlNode == OutNode ) {
					UnitarySystem( UnitarySysNum ).DesiredOutletTemp = OAUCoilOutletTemp;
				}
				// If the unitary system is an equipment of Outdoor Air Unit, the desired coil outlet humidity level is set to zero
				UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = 1.0;

			} else { // Not Outdoor Air Unit or zone equipment
				if ( AirLoopNum > 0 ) EconomizerFlag = AirLoopControlInfo( AirLoopNum ).EconoActive;
				if ( ControlNode == 0 ) {
					UnitarySystem( UnitarySysNum ).DesiredOutletTemp = 0.0;
					UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = 1.0;
				} else if ( ControlNode == OutNode ) {
					if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil && UnitarySystem( UnitarySysNum ).RunOnSensibleLoad ) {
						FrostControlSetPointLimit( UnitarySysNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, UnitarySystem( UnitarySysNum ).DOASDXCoolingCoilMinTout, 1 );
					}
					UnitarySystem( UnitarySysNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint;
					//  IF HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
					if ( ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_None ) && ( Node( ControlNode ).HumRatMax > 0.0 ) ) {
						if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil && UnitarySystem( UnitarySysNum ).RunOnLatentLoad ) {
							FrostControlSetPointLimit( UnitarySysNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, UnitarySystem( UnitarySysNum ).DOASDXCoolingCoilMinTout, 2 );
						}
						UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = Node( ControlNode ).HumRatMax;
					} else {
						UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = 1.0;
					}
				} else {
					if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil && UnitarySystem( UnitarySysNum ).RunOnSensibleLoad ) {
						FrostControlSetPointLimit( UnitarySysNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, UnitarySystem( UnitarySysNum ).DOASDXCoolingCoilMinTout, 1 );
					}
					UnitarySystem( UnitarySysNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint - ( Node( ControlNode ).Temp - Node( OutNode ).Temp );
					if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_None ) {
						if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil && UnitarySystem( UnitarySysNum ).RunOnLatentLoad ) {
							FrostControlSetPointLimit( UnitarySysNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, UnitarySystem( UnitarySysNum ).DOASDXCoolingCoilMinTout, 2 );
						}
						UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = Node( ControlNode ).HumRatMax - ( Node( ControlNode ).HumRat - Node( OutNode ).HumRat );
					} else {
						UnitarySystem( UnitarySysNum ).DesiredOutletHumRat = 1.0;
					}
				}
			}
			if ( present( MaxOutletTemp ) ) UnitarySystem( UnitarySysNum ).DesiredOutletTemp = min( UnitarySystem( UnitarySysNum ).DesiredOutletTemp, MaxOutletTemp );

		} else {

		}}

	}

	void
	InitLoadBasedControl(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		bool const FirstHVACIteration,
		Real64 & OnOffAirFlowRatio,
		Real64 & ZoneLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the load controlled Unitary Systems.

		// METHODOLOGY EMPLOYED:
		// Initialize mass flow rates and speed ratios. Calculate loads and adjust if necessary when using constant fan.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirLoop::AirLoopControlInfo;
		using DataAirLoop::AirToZoneNodeInfo;
		using Fans::GetFanDesignVolumeFlowRate;
		using Fans::GetFanSpeedRatioCurveIndex;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::TempControlType;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using Psychrometrics::PsyHFnTdbW;
		using ReportSizingManager::ReportSizingOutput;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::GetCoilMaxSteamFlowRate;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataZoneEnergyDemands::Setback;
		using DataZoneControls::StageZoneLogic;
		using BranchInputManager::CheckSystemBranchFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Small5WLoad( 5.0 );
		static std::string const RoutineName( "InitUnitarySystems" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag; // environment flag
		static Array1D_bool MyFanFlag; // used for sizing fan inputs one time
		static Array1D_bool MyCheckFlag; // Used to obtain the zone inlet node number
		// in the controlled zone
		static Array1D_bool MyFlowFracFlag; // Used for calculatig flow fraction once
		static Array1D_bool MyStagedFlag; // used for finding on staged thermostat
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyOneTimeFlag( true ); // one time allocation flag // InitLoadBasedControlOneTimeFlag
		// static bool MyAirLoopPass( true ); // one time allocation flag // InitLoadBasedControlAirLoopPass
		// static int AirLoopPass( 0 ); // Number of air loop pass // AirLoopPassCounter
		// static bool FlowFracFlagReady( true ); // one time flag for calculating flow fraction // InitLoadBasedControlFlowFracFlagReady
		// static Real64 CntrlZoneTerminalUnitMassFlowRateMax( 0.0 ); // Maximum mass flow rate through controlled zone // InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax
		////////////////////////////////////////////////////////////////////////////////////
		std::string FanType; // used in warning messages
		std::string FanName; // used in warning messages
		bool errFlag; // error flag for mining functions
		bool ErrorsFound; // error flag for mining functions
		int ZoneInNode; // Zone inlet node number in the controlled zone
		Real64 MinHumRat; // Minimum humidity ratio for sensible capacity calculation (kg/kg)
		Real64 DeltaMassRate; // DIFference of mass flow rate between
		// inlet node and system outlet node
		int i; // index to get the zone inlet node
		int j;
		int k;
		Real64 MassFlowRate; // mass flow rate to calculate loss
		Real64 MaxTemp; // Maximum temperature used in latent loss calculation
		int EquipNum( 0 ); // local DO loop index for zone equipment
		int ZoneInSysIndex( 0 ); // number of zone inlet nodes counter in an airloop
		int NumAirLoopZones( 0 ); // number of zone inlet nodes in an air loop
		int ZoneInletNodeNum( 0 ); // zone inlet nodes node number
		Real64 SumOfMassFlowRateMax( 0.0 ); // the sum of zone inlet mass flow rates
		Real64 rho;
		Real64 QZnReq;
		Real64 QActual;
		Real64 CoilMaxVolFlowRate;
		int SteamIndex;
		Real64 SteamDensity;
		Real64 SensOutputOff;
		Real64 LatOutputOff;
		bool HXUnitOn;

		if ( InitLoadBasedControlOneTimeFlag ) {

			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumUnitarySystem );
			MyFanFlag.allocate( NumUnitarySystem );
			MyCheckFlag.allocate( NumUnitarySystem );
			MyFlowFracFlag.allocate( NumUnitarySystem );
			MyStagedFlag.allocate( NumUnitarySystem );

			MyEnvrnFlag = true;
			MyFanFlag = true;
			MyCheckFlag = true;
			MyFlowFracFlag = true;
			InitLoadBasedControlOneTimeFlag = false;
			MyStagedFlag = true;

		}

		// do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( UnitarySysNum ) ) {

			//   set fluid-side hardware limits
			if ( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode > 0 ) {

				if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow == AutoSize ) {
					// IF water coil max water flow rate is autosized, simulate once in order to mine max flow rate
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
						SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex );
						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", UnitarySystem( UnitarySysNum ).HeatingCoilName, ErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).HeatCoilLoopNum ).FluidIndex, RoutineName );
							UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
						}
					}
					// IF steam coil max steam flow rate is autosized, simulate once in order to mine max flow rate
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingSteam ) {
						SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, ErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
							UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
						}
					}
				}

				InitComponentNodes( 0.0, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow, UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode, UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopSide, UnitarySystem( UnitarySysNum ).HeatCoilBranchNum, UnitarySystem( UnitarySysNum ).HeatCoilCompNum );
			}
			if ( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode > 0 ) {
				if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow == AutoSize ) {
					if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
						// IF water coil max water flow rate is autosized, simulate once in order to mine max flow rate
						SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex );
						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", UnitarySystem( UnitarySysNum ).SuppHeatCoilName, ErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							rho = GetDensityGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).FluidName, InitConvTemp, PlantLoop( UnitarySystem( UnitarySysNum ).SuppCoilLoopNum ).FluidIndex, RoutineName );
							UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
						}
					}
					if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {
						SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, ErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
							UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
						}
					}
					InitComponentNodes( 0.0, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow, UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode, UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopSide, UnitarySystem( UnitarySysNum ).SuppCoilBranchNum, UnitarySystem( UnitarySysNum ).SuppCoilCompNum );
				}
			}
			MyEnvrnFlag( UnitarySysNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( UnitarySysNum ) = true;
		}

		if ( MyFanFlag( UnitarySysNum ) ) {
			if ( UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate != AutoSize ) {
				if ( UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate > 0.0 ) {
					UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow / UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate;
					UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow / UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate;
					UnitarySystem( UnitarySysNum ).NoHeatCoolSpeedRatio = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow / UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate;
					if ( UnitarySystem( UnitarySysNum ).FanExists ) {
						if ( GetFanSpeedRatioCurveIndex( FanType, FanName, UnitarySystem( UnitarySysNum ).FanIndex ) > 0 ) {
							if ( UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate == UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow && UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate == UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow && UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate == UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow ) {
								ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\"" );
								ShowContinueError( "...For fan type and name = " + FanType + " \"" + FanName + "\"" );
								ShowContinueError( "...Fan power ratio function of speed ratio curve has no impact IF fan volumetric flow rate is the same as the unitary system volumetric flow rate." );
								ShowContinueError( "...Fan volumetric flow rate            = " + RoundSigDigits( UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate, 5 ) + " m3/s." );
								ShowContinueError( "...Unitary system volumetric flow rate = " + RoundSigDigits( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow, 5 ) + " m3/s." );
							}
						}
					} else {
						CheckSystemBranchFlow( UnitarySystem( UnitarySysNum ).UnitarySystemType, UnitarySystem( UnitarySysNum ).Name, UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate, 0.0, errFlag );
					}
				}
				MyFanFlag( UnitarySysNum ) = false;
			} else {
				if ( UnitarySystem( UnitarySysNum ).FanExists ) {
					UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate = GetFanDesignVolumeFlowRate( BlankString, BlankString, errFlag, UnitarySystem( UnitarySysNum ).FanIndex );
				} else {
					CheckSystemBranchFlow( UnitarySystem( UnitarySysNum ).UnitarySystemType, UnitarySystem( UnitarySysNum ).Name, UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate, 0.0, errFlag );
				}
			}
		}

		// Get the zone inlet node
		if ( allocated( ZoneEquipConfig ) && MyCheckFlag( UnitarySysNum ) ) {
			for ( i = 1; i <= NumOfZones; ++i ) {
				if ( UnitarySystem( UnitarySysNum ).AirLoopEquipment ) {
					if ( AirLoopNum != ZoneEquipConfig( i ).AirLoopNum ) continue;
					if ( UnitarySystem( UnitarySysNum ).ControlZoneNum == ZoneEquipConfig( i ).ActualZoneNum ) {
						for ( j = 1; j <= ZoneEquipConfig( i ).NumInletNodes; ++j ) {
							if ( UnitarySystem( UnitarySysNum ).ZoneInletNode == 0 ) {
								for ( k = 1; k <= ZoneEquipConfig( i ).NumInletNodes; ++k ) {
									if ( ZoneEquipConfig( i ).InletNode( j ) == ZoneEquipConfig( i ).AirDistUnitCool( k ).OutNode ) {
										UnitarySystem( UnitarySysNum ).ZoneInletNode = ZoneEquipConfig( i ).InletNode( j );
										break;
									} else if ( ZoneEquipConfig( i ).InletNode( j ) == ZoneEquipConfig( i ).AirDistUnitHeat( k ).OutNode ) {
										UnitarySystem( UnitarySysNum ).ZoneInletNode = ZoneEquipConfig( i ).InletNode( j );
										break;
									}
								}
							}
						}
						//setup unitary system zone equipment sequence information based on finding an air terminal
						if ( ZoneEquipConfig( i ).EquipListIndex > 0 ) {
							for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
								if ( ( ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).EquipType_Num( EquipNum ) == AirDistUnit_Num ) || ( ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).EquipType_Num( EquipNum ) == DirectAir_Num ) ) {
									UnitarySystem( UnitarySysNum ).ZoneSequenceCoolingNum = ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).CoolingPriority( EquipNum );
									UnitarySystem( UnitarySysNum ).ZoneSequenceHeatingNum = ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).HeatingPriority( EquipNum );
								}
							}
						}
					}
				}
			}
			MyCheckFlag( UnitarySysNum ) = false;
			if ( UnitarySystem( UnitarySysNum ).ZoneInletNode == 0 ) {
				ShowSevereError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\": The zone inlet node in the controlled zone (" + Zone( UnitarySystem( UnitarySysNum ).ControlZoneNum ).Name + ") is not found." );
				ShowFatalError( "Subroutine InitLoadBasedControl: Errors found in getting " + UnitarySystem( UnitarySysNum ).UnitarySystemType + " input.  Preceding condition(s) causes termination." );
			}
		}

		// Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
		if ( MyFlowFracFlag( UnitarySysNum ) ) {
			if ( UnitarySystem( UnitarySysNum ).AirLoopEquipment ) {
				if ( allocated( AirToZoneNodeInfo ) ) NumAirLoopZones = AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled + AirToZoneNodeInfo( AirLoopNum ).NumZonesHeated;
				if ( allocated( AirToZoneNodeInfo ) && MyFlowFracFlag( UnitarySysNum ) ) {
					InitLoadBasedControlFlowFracFlagReady = true;
					for ( ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex ) {
						// zone inlet nodes for cooling
						if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled > 0 ) {
							if ( AirToZoneNodeInfo( AirLoopNum ).TermUnitCoolInletNodes( ZoneInSysIndex ) == -999 ) {
								// the data structure for the zones inlet nodes has not been filled
								InitLoadBasedControlFlowFracFlagReady = false;
							}
						}
						// zone inlet nodes for heating
						if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesHeated > 0 ) {
							if ( AirToZoneNodeInfo( AirLoopNum ).TermUnitHeatInletNodes( ZoneInSysIndex ) == -999 ) {
								// the data structure for the zones inlet nodes has not been filled
								InitLoadBasedControlFlowFracFlagReady = false;
							}
						}
					}
				}
				if ( allocated( AirToZoneNodeInfo ) && InitLoadBasedControlFlowFracFlagReady ) {
					SumOfMassFlowRateMax = 0.0; // initialize the sum of the maximum flows
					for ( ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex ) {
						ZoneInletNodeNum = AirToZoneNodeInfo( AirLoopNum ).TermUnitCoolInletNodes( ZoneInSysIndex );
						SumOfMassFlowRateMax += Node( ZoneInletNodeNum ).MassFlowRateMax;
						if ( AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZoneInSysIndex ) == UnitarySystem( UnitarySysNum ).ControlZoneNum ) {
							InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax = Node( ZoneInletNodeNum ).MassFlowRateMax;
						}
					}
					if ( SumOfMassFlowRateMax != 0.0 && MyFlowFracFlag( UnitarySysNum ) ) {
						if ( InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow ) {
							UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac = InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax / SumOfMassFlowRateMax;
						} else {
							ShowSevereError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( " The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1." );
						}
						ReportSizingOutput( UnitarySystem( UnitarySysNum ).UnitarySystemType, UnitarySystem( UnitarySysNum ).Name, "Fraction of Supply Air Flow That Goes Through the Controlling Zone", UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac );
						MyFlowFracFlag( UnitarySysNum ) = false;
					}
				}
			} else {
				UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac = 1.0;
			}
		} // IF(MyFlowFracFlag(UnitarySysNum))THEN

		// What type of logic is this? Is the point to go through the main IF once? or every other time?
		// RR: This was used with AirflowNetwork to calculate duct losses.
		// RR: AFN counts the number of passes through airloop equipment (same logic in Furnaces and other modules) and resets the counter to 0 on BeginEnvrnFlag.
		// RR: This has been changed in this module and AFN to use AirflowNetworkFanActivated if AirflowNetworkUnitarySystem is seen by AFN.
		// RR: Search for AirflowNetworkFanActivated in this module to see usage. The following lines of code can probably be removed although it would require a AFN input file to test.
		if ( BeginEnvrnFlag && InitLoadBasedControlAirLoopPass ) {
			AirLoopPassCounter = 0;
			InitLoadBasedControlAirLoopPass = false;
		}
		if ( ! BeginEnvrnFlag ) {
			InitLoadBasedControlAirLoopPass = true;
		}

		++AirLoopPassCounter;
		if ( AirLoopPassCounter > 2 ) AirLoopPassCounter = 1;

		// reset duct losses from previous iteration
		if ( FirstHVACIteration ) {
			UnitarySystem( UnitarySysNum ).SenLoadLoss = 0.0;
			UnitarySystem( UnitarySysNum ).LatLoadLoss = 0.0;
		}

		// Calcuate air distribution losses
		//  IF (.NOT. FirstHVACIteration .AND. AirLoopPass .EQ. 1 .AND. AirflowNetworkFanActivated) THEN
		if ( ! FirstHVACIteration && AirflowNetworkFanActivated ) {
			ZoneInNode = UnitarySystem( UnitarySysNum ).ZoneInletNode;
			MinHumRat = Node( ZoneInNode ).HumRat;
			MassFlowRate = Node( ZoneInNode ).MassFlowRate / UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
			if ( Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).Temp < Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp ) MinHumRat = Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).HumRat;
			if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone ) {
				DeltaMassRate = Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).MassFlowRate - Node( ZoneInNode ).MassFlowRate / UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
				if ( DeltaMassRate < 0.0 ) DeltaMassRate = 0.0;
			} else {
				MassFlowRate = Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).MassFlowRate;
				DeltaMassRate = 0.0;
			}
			UnitarySystem( UnitarySysNum ).SenLoadLoss = MassFlowRate * ( PsyHFnTdbW( Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( ZoneInNode ).Temp, MinHumRat ) ) + DeltaMassRate * ( PsyHFnTdbW( Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp, MinHumRat ) );
			if ( std::abs( UnitarySystem( UnitarySysNum ).SensibleLoadMet ) > 0.0 ) {
				if ( std::abs( UnitarySystem( UnitarySysNum ).SenLoadLoss / UnitarySystem( UnitarySysNum ).SensibleLoadMet ) < 0.001 ) UnitarySystem( UnitarySysNum ).SenLoadLoss = 0.0;
			}
			if ( UnitarySystem( UnitarySysNum ).Humidistat ) {
				MaxTemp = Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp;
				UnitarySystem( UnitarySysNum ).LatLoadLoss = MassFlowRate * ( PsyHFnTdbW( MaxTemp, Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).HumRat ) - PsyHFnTdbW( MaxTemp, Node( ZoneInNode ).HumRat ) ) + DeltaMassRate * ( PsyHFnTdbW( MaxTemp, Node( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ).HumRat ) - PsyHFnTdbW( MaxTemp, Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).HumRat ) );
				if ( std::abs( UnitarySystem( UnitarySysNum ).LatentLoadMet ) > 0.0 ) {
					if ( std::abs( UnitarySystem( UnitarySysNum ).LatLoadLoss / UnitarySystem( UnitarySysNum ).LatentLoadMet ) < 0.001 ) UnitarySystem( UnitarySysNum ).LatLoadLoss = 0.0;
				}
			}
		}

		if ( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr ) == 0.0 ) {
				UnitarySystem( UnitarySysNum ).FanOpMode = CycFanCycCoil;
			} else {
				UnitarySystem( UnitarySysNum ).FanOpMode = ContFanCycCoil;
				OnOffFanPartLoadFraction = 1.0;
			}
		}

		//  OpMode = UnitarySystem(UnitarySysNum)%FanOpMode
		if ( allocated( AirLoopControlInfo ) && UnitarySystem( UnitarySysNum ).AirLoopEquipment ) {
			EconomizerFlag = AirLoopControlInfo( AirLoopNum ).EconoActive;
		} else {
			EconomizerFlag = false;
		}

		// System load calculation for cycling fan systems
		if ( UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac > 0.0 ) {
			QZnReq = ZoneLoad / UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
			MoistureLoad /= UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
			QToCoolSetPt /= UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
			QToHeatSetPt /= UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
			ZoneLoad = QZnReq;
		} else {
			QZnReq = ZoneLoad;
			UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac = 1.0;
		}

		CoolingLoad = false;
		HeatingLoad = false;

		if ( QZnReq > Small5WLoad / UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac && ! CurDeadBandOrSetback( UnitarySystem( UnitarySysNum ).ControlZoneNum ) ) {
			if ( TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) != SingleCoolingSetPoint ) {
				HeatingLoad = true;
			}
		} else if ( QZnReq < - Small5WLoad / UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac && ! CurDeadBandOrSetback( UnitarySystem( UnitarySysNum ).ControlZoneNum ) ) {
			if ( TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) != SingleHeatingSetPoint ) {
				CoolingLoad = true;
			}
		}

		// System load calculation for constant fan systems
		if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
			HXUnitOn = false;
			CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, 0.0, 0.0, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn );
			{ auto const SELECT_CASE_var( TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) );
			if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
				CoolingLoad = false;
				// No heating load and constant fan pushes zone below heating set point
				if ( SensOutputOff < 0.0 && QToHeatSetPt < 0.0 && SensOutputOff - QToHeatSetPt < -SmallLoad ) {
					HeatingLoad = true;
					CoolingLoad = false;
					ZoneLoad = QToHeatSetPt;
				}
			} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
				HeatingLoad = false;
				// No heating load and constant fan pushes zone above cooling set point
				if ( SensOutputOff > 0.0 && QToCoolSetPt > 0.0 && SensOutputOff - QToCoolSetPt > SmallLoad ) {
					HeatingLoad = false;
					CoolingLoad = true;
					ZoneLoad = QToCoolSetPt;
				}
			} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
				// zone temp above cooling and heating set point temps
				if ( QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 ) {
					// zone pushed below heating set point
					if ( SensOutputOff < 0.0 && QToHeatSetPt - SensOutputOff > SmallLoad ) {
						HeatingLoad = true;
						CoolingLoad = false;
						ZoneLoad = QToHeatSetPt;
					}
					// zone temp below heating set point temp
				} else if ( QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 ) {
					// zone pushed above cooling set point
					if ( SensOutputOff > 0.0 && QToCoolSetPt - SensOutputOff > SmallLoad ) {
						HeatingLoad = false;
						CoolingLoad = true;
						ZoneLoad = QToCoolSetPt;
					}
				}
			} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
				// zone temp above cooling and heating set point temps
				if ( QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 ) {
					// zone pushed into deadband
					if ( SensOutputOff < 0.0 && QToCoolSetPt - SensOutputOff > SmallLoad ) {
						HeatingLoad = false;
						CoolingLoad = false;
						ZoneLoad = 0.0;
					}
					// zone pushed below heating set point
					if ( SensOutputOff < 0.0 && QToHeatSetPt - SensOutputOff > SmallLoad ) {
						HeatingLoad = true;
						CoolingLoad = false;
						ZoneLoad = QToHeatSetPt;
					}
					// zone temp below heating set point temp
				} else if ( QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 ) {
					// zone pushed into deadband
					if ( SensOutputOff > 0.0 && SensOutputOff - QToHeatSetPt > SmallLoad ) {
						HeatingLoad = false;
						CoolingLoad = false;
						ZoneLoad = 0.0;
					}
					// zone pushed above cooling set point
					if ( SensOutputOff > 0.0 && SensOutputOff - QToCoolSetPt > SmallLoad ) {
						HeatingLoad = false;
						CoolingLoad = true;
						ZoneLoad = QToCoolSetPt;
					}
					// zone temp between set point temps
				} else if ( QToHeatSetPt < 0.0 && QToCoolSetPt > 0.0 ) {
					// zone pushed below heating set point
					if ( SensOutputOff < 0.0 && SensOutputOff - QToHeatSetPt < -SmallLoad ) {
						HeatingLoad = true;
						CoolingLoad = false;
						ZoneLoad = QToHeatSetPt;
						// zone pushed above cooling set point
					} else if ( SensOutputOff > 0.0 && SensOutputOff - QToCoolSetPt > SmallLoad ) {
						HeatingLoad = false;
						CoolingLoad = true;
						ZoneLoad = QToCoolSetPt;
					}
				}
			} else {
			}}

			// IF small loads to meet, just shut down unit
			if ( std::abs( ZoneLoad ) < Small5WLoad ) {
				ZoneLoad = 0.0;
				CoolingLoad = false;
				HeatingLoad = false;
			}

		}

		// Determine the staged status
		if ( allocated( StageZoneLogic ) && UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex > 0 ) {
			if ( StageZoneLogic( UnitarySystem( UnitarySysNum ).ControlZoneNum ) ) {
				UnitarySystem( UnitarySysNum ).Staged = true;
				UnitarySystem( UnitarySysNum ).StageNum = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).StageNum;
			} else {
				if ( MyStagedFlag( UnitarySysNum ) ) {
					ShowWarningError( "ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to this AirLoopHVAC:UnitarySystem object with UnitarySystemPerformance:Multispeed type = " );
					ShowContinueError( UnitarySystem( UnitarySysNum ).Name + ". Please make correction. Simulation continues..." );
					MyStagedFlag( UnitarySysNum ) = false;
				}
			}
		}

		// Staged control
		if ( UnitarySystem( UnitarySysNum ).Staged ) {
			if ( UnitarySystem( UnitarySysNum ).StageNum == 0 ) {
				HeatingLoad = false;
				CoolingLoad = false;
				QZnReq = 0.0;
			} else {
				QZnReq = ZoneSysEnergyDemand( UnitarySystem( UnitarySysNum ).ControlZoneNum ).RemainingOutputRequired / UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac;
				if ( UnitarySystem( UnitarySysNum ).StageNum > 0 ) {
					HeatingLoad = true;
					CoolingLoad = false;
				} else {
					HeatingLoad = false;
					CoolingLoad = true;
				}
			}
		}

		if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) {
			if ( HeatingLoad ) MoistureLoad = 0.0;
		}

		// Check load control
		if ( UnitarySystem( UnitarySysNum ).RunOnLatentOnlyWithSensible && ZoneLoad == 0.0 ) MoistureLoad = 0.0;
		if ( ! UnitarySystem( UnitarySysNum ).RunOnSensibleLoad ) {
			ZoneLoad = 0.0;
			CoolingLoad = false;
			HeatingLoad = false;
		}
		if ( ! UnitarySystem( UnitarySysNum ).RunOnLatentLoad ) MoistureLoad = 0.0;

		// Testing heat pump air to air with RH control with CoolReheat dehumidifaction control showed that when there was heating
		// and moisture load, the cooling coil was turning on to meet the moisture load and reheat was then turning on to meet both
		// heating load and excess cooling load caused by cooling coil. Adding the logic below caused the zone temperature,
		// relative humidity, cooling/heating rate to line up for both the orignal and new file with unitary system object.

		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
			if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat ) {
				if ( MoistureLoad < 0.0 && UnitarySystem( UnitarySysNum ).HeatPump ) {
					HeatingLoad = false;
					CoolingLoad = true;
				}
			}
		}

	}

	// End of Initialization subroutines for the Module
	// *****************************************************************************

	void
	SizeUnitarySystem(
		int const UnitarySysNum,
		bool const FirstHVACIteration,
		int const AirLoopNum // does this need to be optional?
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing unitary system components for which nominal cpacities
		// and flow rates have not been specified in the input. Coil sizing is preformed in the coil module.
		// Future modifications will size coils here and "push" this info to the specific coil.

		// METHODOLOGY EMPLOYED:
		// Obtains heating capacities and flow rates from the zone or system sizing arrays.
		// NOTE: In UNITARYSYSTEM:HEATPUMP:AIRTOAIR we are sizing the heating capacity to be
		// equal to the cooling capacity.  Thus the cooling and
		// and heating capacities of a DX heat pump system will be identical. In real life the ARI
		// heating and cooling capacities are close but not identical.

		// REFERENCES:
		// na

		// Using/Aliasing
		using BranchInputManager::CheckSystemBranchFlow;
		using BranchInputManager::GetAirBranchIndex;
		using BranchInputManager::GetBranchFanTypeName;
		using BranchInputManager::GetBranchFlow;
		using DataAirSystems::PrimaryAirSystem;
		using CurveManager::CurveValue;
		using DXCoils::SimDXCoil; // , SetDXCoolingCoilData
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::GetCoilCapacityByIndexType;
		using DXCoils::GetDXCoilCapFTCurveIndex;
		using Fans::GetFanDesignVolumeFlowRate;
		using Fans::FanDesHeatGain;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using HVACHXAssistedCoolingCoil::GetCoilCapacity;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
		using WaterToAirHeatPump::SimWatertoAirHP;
		auto & GetWAHPCoilCapacity( WaterToAirHeatPump::GetCoilCapacity );
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		auto & GetSimpleCoilCapacity( WaterToAirHeatPumpSimple::GetCoilCapacity );
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using InputProcessor::MakeUPPERCase;
		using General::TrimSigDigits;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetWaterCoilCapacity;
		using WaterCoils::SimulateWaterCoilComponents;
		using namespace Psychrometrics;
		using HVACDXSystem::GetCoolingCoilTypeNameAndIndex;
		using EMSManager::ManageEMS;
		using DataGlobals::emsCallFromUnitarySystemSizing;
		using PackagedThermalStorageCoil::SimTESCoil;
		using PackagedThermalStorageCoil::GetTESCoilCoolingCapacity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeUnitarySystem" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Iter; // iteration count
		Real64 MulSpeedFlowScale; // variable speed air flow scaling factor
		int MSHPIndex; // Index to design Specification object
		int BranchNum; // Index to branch on air loop
		Real64 BranchFlow; // branch flow rate [m3/s]
		Real64 BranchFanFlow; // branch fan flow rate [m3/s]
		bool ErrFound; // logical error flag
		std::string FanType; // fan type
		std::string FanName; // fan name
		Real64 CoolCapAtPeak; // cooling capacity at peak [W]
		Real64 HeatCapAtPeak; // heating capacity at peak [W]
		std::string SystemType; // type of air loop equipment
		Real64 OnOffAirFlowRatio; // used to pass to cooling coil for sizing
		Real64 PartLoadRatio; // used to pass to cooling coil for sizing
		bool TempCoolingLoad; // size cooling coils with a cooling load, save actual load
		bool TempHeatingLoad; // save actual load
		Real64 SysCoolingFlow; // individually sized cooling flow rate [m3/s]
		Real64 SysHeatingFlow; // individually sized heating flow rate [m3/s]
		std::string HXCoilName; // cooling coil name in HXAssisted parent
		int ActualCoolCoilType; // cooling coil type in HXAssisted parent
		int SupFanNum; // supply fan index
		int SaveCurDuctType; // used during sizing to save the current duct type
		bool IsAutoSize; // used to catch autosized vs hardsized fields
		Real64 QActual; // water coil output [W]

		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		std::string CompName; // component name
		std::string	CompType; // component type
		Real64 TempSize; // autosized value of input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method (e.g., CoolingAirflowSizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CoolingSAFlowMethod; // Sizing type for UnitarySystem cooling coil
		int HeatingSAFlowMethod; // Sizing type for UnitarySystem Heating coil
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static int NumUnitarySystemsSized( 0 ); // counter used to delete UnitarySystemNumericFields array after last system is sized
		////////////////////////////////////////////////////////////////////////////////////
		// References
		ZoneEqSizingData * select_EqSizing( nullptr );

		//sweep specific data into one pointer to avoid if statements throughout this subroutine
		if ( CurOASysNum > 0 ) {
			select_EqSizing = &OASysEqSizing( CurOASysNum );
		} else if ( CurSysNum > 0 ) {
			select_EqSizing = &UnitarySysEqSizing( CurSysNum );
		} else if ( CurZoneEqNum > 0 ) {
			select_EqSizing = &ZoneEqSizing( CurZoneEqNum );
		} else {
			assert( false );
		}
		// Object Data, points to specific array
		ZoneEqSizingData & EqSizing( *select_EqSizing );

		// can't hurt to initialize these going in, problably redundant
		EqSizing.AirFlow = false;
		EqSizing.CoolingAirFlow = false;
		EqSizing.HeatingAirFlow = false;
		EqSizing.AirVolFlow = 0.0;
		EqSizing.CoolingAirVolFlow = 0.0;
		EqSizing.HeatingAirVolFlow = 0.0;
		EqSizing.Capacity = false;
		EqSizing.CoolingCapacity = false;
		EqSizing.HeatingCapacity = false;
		EqSizing.DesCoolingLoad = 0.0;
		EqSizing.DesHeatingLoad = 0.0;


		ManageEMS( emsCallFromUnitarySystemSizing ); // calling point

		CompName = UnitarySystem( UnitarySysNum ).Name;
		CompType = UnitarySystem( UnitarySysNum ).UnitarySystemType;
		CoolingSAFlowMethod = UnitarySystem( UnitarySysNum ).CoolingSAFMethod;
		HeatingSAFlowMethod = UnitarySystem( UnitarySysNum ).HeatingSAFMethod;
// can't reset this to 0 for systems where DX heating coil is in downstream unit and DX cooling coil is in upstream unit
//		DXCoolCap = 0.0;
		UnitaryHeatCap = 0.0;
		SuppHeatCap = 0.0;
		TempCoolingLoad = CoolingLoad;
		TempHeatingLoad = HeatingLoad;
		CoolingLoad = true;
		HeatingLoad = false;
		ZoneCoolingOnlyFan = false;
		ZoneHeatingOnlyFan = false;
		IsAutoSize = false;
		SysCoolingFlow = 0.0;
		SysHeatingFlow = 0.0;
		CoolCapAtPeak = 0.0;
		HeatCapAtPeak = 0.0;
		SupFanNum = 0;

		if ( CurSysNum > 0 && CurOASysNum == 0 && UnitarySystem( UnitarySysNum ).FanExists ) {
			PrimaryAirSystem( CurSysNum ).SupFanNum = UnitarySystem( UnitarySysNum ).FanIndex;
		}

		// STEP 1: find the autosized cooling air flow rate and capacity
		if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
			if ( ! UnitarySystem( UnitarySysNum ).HeatCoilExists ) ZoneCoolingOnlyFan = true;
			FieldNum = 3;	// N3 , \field Cooling Supply Air Flow Rate
			PrintFlag = false;
			SizingMethod = CoolingAirflowSizing;
			SizingString = UnitarySystemNumericFields( UnitarySysNum ).FieldNames( FieldNum ) + " [m3/s]";
			TempSize = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow;
			SaveCurDuctType = CurDuctType;
			CurDuctType = Cooling;
			if ( ( CoolingSAFlowMethod == SupplyAirFlowRate ) || ( CoolingSAFlowMethod == None ) ) {
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SysCoolingFlow = TempSize;
			} else if ( CoolingSAFlowMethod == FlowPerFloorArea ) {
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SysCoolingFlow = TempSize;
				UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
			} else if ( CoolingSAFlowMethod == FractionOfAutoSizedCoolingValue ) {
				TempSize = AutoSize;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SysCoolingFlow = TempSize * UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow;
				UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
			} else if ( CoolingSAFlowMethod == FlowPerCoolingCapacity ) {
				if ( UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) {
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					SizingMethod = CoolingCapacitySizing;
					DataFlowUsedForSizing = TempSize;
					TempSize = AutoSize;
					if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
						DataTotCapCurveIndex = GetDXCoilCapFTCurveIndex( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ErrFound );
						DataIsDXCoil = true;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					CoolCapAtPeak = TempSize;
					SysCoolingFlow = TempSize * UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow;
					DataTotCapCurveIndex = 0;
					EqSizing.CoolingCapacity = true;
					EqSizing.DesCoolingLoad = CoolCapAtPeak;
				} else {
					SysCoolingFlow = UnitarySystem( UnitarySysNum ).DesignCoolingCapacity * UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow;
					CoolCapAtPeak = UnitarySystem( UnitarySysNum ).DesignCoolingCapacity;
					DXCoolCap = CoolCapAtPeak;
				}
				UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
			} else {
				// should never happen
				ShowSevereError( RoutineName + ": " + CompType + " = " + CompName );
				ShowContinueError( "Illegal entry for Cooling Supply Air Flow Rate Method." );
			}

			CurDuctType = SaveCurDuctType;
			EqSizing.CoolingAirFlow = true;
			EqSizing.CoolingAirVolFlow = SysCoolingFlow;

// Cooling airflow should be known at this point. Now find autosized design cooling capacity.
			if ( CoolingSAFlowMethod != FlowPerCoolingCapacity && UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) {
				SizingMethod = CoolingCapacitySizing;
				DataFlowUsedForSizing = EqSizing.CoolingAirVolFlow;
				TempSize = AutoSize;
				if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					DataTotCapCurveIndex = GetDXCoilCapFTCurveIndex( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ErrFound );
					DataIsDXCoil = true;
				}
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				CoolCapAtPeak = TempSize;
				DXCoolCap = CoolCapAtPeak;
				EqSizing.CoolingCapacity = true;
				EqSizing.DesCoolingLoad = CoolCapAtPeak;
			} else {
				CoolCapAtPeak = UnitarySystem( UnitarySysNum ).DesignCoolingCapacity;
			}
			DataIsDXCoil = false;
			DataTotCapCurveIndex = 0;
			DataFlowUsedForSizing = 0.0;
		}

		// STEP 2: find the autosized heating air flow rate and capacity
		if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
			if ( ! UnitarySystem( UnitarySysNum ).CoolCoilExists ) ZoneHeatingOnlyFan = true;
			FieldNum = 7;	// N7 , \field Heating Supply Air Flow Rate
			PrintFlag = false;
			SizingMethod = HeatingAirflowSizing;
			SizingString = UnitarySystemNumericFields( UnitarySysNum ).FieldNames( FieldNum ) + " [m3/s]";
			TempSize = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow;
			SaveCurDuctType = CurDuctType;
			CurDuctType = Heating;
			if ( ( HeatingSAFlowMethod == SupplyAirFlowRate ) || ( HeatingSAFlowMethod == None ) ) {
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SysHeatingFlow = TempSize;
			} else if ( HeatingSAFlowMethod == FlowPerFloorArea ) {
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SysHeatingFlow = TempSize;
				UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
			} else if ( HeatingSAFlowMethod == FractionOfAutoSizedHeatingValue ) {
				TempSize = AutoSize;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SysHeatingFlow = TempSize * UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow;
				UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
			} else if ( HeatingSAFlowMethod == FlowPerHeatingCapacity ) {
				TempSize = AutoSize;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				SizingMethod = HeatingCapacitySizing;
				DataFlowUsedForSizing = TempSize;
				TempSize = AutoSize;
				DataFracOfAutosizedCoolingCapacity = 1.0;
				DataHeatSizeRatio = UnitarySystem( UnitarySysNum ).HeatingSizingRatio;
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
					DataTotCapCurveIndex = GetDXCoilCapFTCurveIndex( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, ErrFound );
					DataIsDXCoil = true;
				}
				if ( CurSysNum > 0 ) AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = false; // set to false to allow calculation of actual heating capacity
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				if ( CurSysNum > 0 ) AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = true;
				HeatCapAtPeak = TempSize;
				SysHeatingFlow = TempSize * UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow;
				UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
				EqSizing.HeatingCapacity = true;
				EqSizing.DesHeatingLoad = HeatCapAtPeak;
			} else {
				// should never happen
				ShowSevereError( RoutineName + ": " + CompType + " = " + CompName );
				ShowContinueError( "Illegal entry for Heating Supply Air Flow Rate Method." );
			}

			CurDuctType = SaveCurDuctType;
			EqSizing.HeatingAirFlow = true;
			EqSizing.HeatingAirVolFlow = SysHeatingFlow;

// Heating airflow should be known at this point. Now find autosized design heating capacity.
			if ( HeatingSAFlowMethod != FlowPerHeatingCapacity && UnitarySystem( UnitarySysNum ).DesignHeatingCapacity == AutoSize ) {
				SizingMethod = HeatingCapacitySizing;
				DataFlowUsedForSizing = EqSizing.HeatingAirVolFlow;
				TempSize = AutoSize;
				DataHeatSizeRatio = UnitarySystem( UnitarySysNum ).HeatingSizingRatio;
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating ) {
					DataTotCapCurveIndex = GetDXCoilCapFTCurveIndex( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, ErrFound );
					DataIsDXCoil = true;
				}
				if ( CurSysNum > 0 ) AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = false; // set to false to allow calculation of actual heating capacity
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				if ( CurSysNum > 0 ) AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = true;
				HeatCapAtPeak = TempSize;
				EqSizing.HeatingCapacity = true;
				EqSizing.DesHeatingLoad = HeatCapAtPeak;
			} else {
				HeatCapAtPeak = UnitarySystem( UnitarySysNum ).DesignHeatingCapacity;
			}
//			if ( ! UnitarySystem( UnitarySysNum ).CoolCoilExists )DXCoolCap = HeatCapAtPeak;
			DataIsDXCoil = false;
			DataTotCapCurveIndex = 0;
			DataFlowUsedForSizing = 0.0;
		}

		// STEP 3: use the greater of cooling and heating air flow rates for system flow
		// previous version of E+ used maximum flow rate for unitary systems. Keep this methodology for now.
		// Delete next 2 lines and uncomment 2 lines inside next if (HeatPump) statement to allow non-heat pump systems to operate at different flow rates (might require additional change to if block logic).
		EqSizing.CoolingAirVolFlow = max( EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow );
		EqSizing.HeatingAirVolFlow = EqSizing.CoolingAirVolFlow;

		// STEP 4: set heat pump coil capacities equal to greater of cooling or heating capacity
		if ( UnitarySystem( UnitarySysNum ).HeatPump ) { // if a heat pump, use maximum values and set main air flow and capacity variables
			EqSizing.AirFlow = true;
			EqSizing.AirVolFlow = max( EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow );
//			EqSizing.CoolingAirVolFlow = EqSizing.AirVolFlow;
//			EqSizing.HeatingAirVolFlow = EqSizing.AirVolFlow;
			EqSizing.Capacity = true;
			EqSizing.DesCoolingLoad = max( EqSizing.DesCoolingLoad, EqSizing.DesHeatingLoad );
			EqSizing.DesHeatingLoad = EqSizing.DesCoolingLoad;
			DXCoolCap = EqSizing.DesCoolingLoad;
		} else if( ! UnitarySystem( UnitarySysNum ).CoolCoilExists && CurZoneEqNum > 0 ) {
			DXCoolCap = EqSizing.DesHeatingLoad;
		}


		// STEP 5: report system parameters (e.g., air flow rates, capacities, etc.)
		if ( UnitarySystem( UnitarySysNum ).FanExists ) {

			SizingMethod = SystemAirflowSizing;
			if ( UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate <= 0.0 ) { // attempt to catch any missed logic in GetUnitarySystem
				UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = AutoSize;
			}
			PrintFlag = true;
			DataEMSOverrideON = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRateEMSOverrideOn;
			DataEMSOverride = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRateEMSOverrideValue;
			TempSize = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate;
			SizingString = "Supply Air Flow Rate [m3/s]";
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = TempSize;
			DataEMSOverrideON = false;

		}

		// not sure what to do if UnitarySystem has only 1 coil type and flow needs to occur when present coil is off
		// how does constant fan operating mode pertain here?
		if ( UnitarySystem( UnitarySysNum ).HeatCoilExists && ! UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
			UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = EqSizing.HeatingAirVolFlow;
		} else if ( UnitarySystem( UnitarySysNum ).CoolCoilExists && ! UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
			UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = EqSizing.CoolingAirVolFlow;
		}

		if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {

			SizingMethod = HeatingAirflowSizing;
			if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow <= 0.0 ) { // attempt to catch any missed logic in GetUnitarySystem
				UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
			}
			FieldNum = 7;	// N7 , \field Heating Supply Air Flow Rate
			PrintFlag = true;
			DataEMSOverrideON = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlowEMSOverrideOn;
			DataEMSOverride = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlowEMSOverrideValue;
			TempSize = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow;
			SizingString = UnitarySystemNumericFields( UnitarySysNum ).FieldNames( FieldNum ) + " [m3/s]";
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = TempSize;
			DataEMSOverrideON = false;
			DataConstantUsedForSizing = 0.0;

		}

		if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {

			SizingMethod = CoolingAirflowSizing;
			if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow <= 0.0 ) { // attempt to catch any missed logic in GetUnitarySystem
				UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
			}
			FieldNum = 3;	// N3 , \field Cooling Supply Air Flow Rate
			PrintFlag = true;
			DataEMSOverrideON = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlowEMSOverrideOn;
			DataEMSOverride = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlowEMSOverrideValue;
			TempSize = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow;
			SizingString = UnitarySystemNumericFields( UnitarySysNum ).FieldNames( FieldNum ) + " [m3/s]";
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = TempSize;
			DataEMSOverrideON = false;
			DataConstantUsedForSizing = 0.0;

		}

		if ( UnitarySystem( UnitarySysNum ).CoolCoilExists || UnitarySystem( UnitarySysNum ).HeatCoilExists || UnitarySystem( UnitarySysNum ).SuppCoilExists ) {

			SizingMethod = SystemAirflowSizing;

			if ( UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod == FractionOfAutoSizedCoolingValue ) {
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow *= EqSizing.CoolingAirVolFlow;
				DataConstantUsedForSizing = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
			} else if ( UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod == FractionOfAutoSizedHeatingValue ) {
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow *= EqSizing.HeatingAirVolFlow;
				DataConstantUsedForSizing = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
			} else if ( UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod == FlowPerCoolingCapacity ) {
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow *= EqSizing.DesCoolingLoad;
				DataConstantUsedForSizing = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
			} else if ( UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod == FlowPerHeatingCapacity ) {
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow *= EqSizing.DesHeatingLoad;
				DataConstantUsedForSizing = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = AutoSize;
			}

			FieldNum = 11;	// N11 , \field No Load Supply Air Flow Rate
			PrintFlag = true;
			DataEMSOverrideON = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlowEMSOverrideOn;
			DataEMSOverride = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlowEMSOverrideValue;
			TempSize = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow;
			SizingString = UnitarySystemNumericFields( UnitarySysNum ).FieldNames( FieldNum ) + " [m3/s]";
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = TempSize;
			DataEMSOverrideON = false;
			DataConstantUsedForSizing = 0.0;
			DataFractionUsedForSizing = 0.0;

		}

		// initialize multi-speed coils
		if ( ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) || ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed ) ) {

			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
				if ( ! allocated( UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate ) ) UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).CoolMassFlowRate ) ) UnitarySystem( UnitarySysNum ).CoolMassFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio ) ) UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
			}

			SimVariableSpeedCoils( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, 0, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 0, 0.0, 1, 0.0, 0.0, 0.0, 0.0 ); //conduct the sizing operation in the VS WSHP
			UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = VarSpeedCoil( UnitarySystem( UnitarySysNum ).CoolingCoilIndex ).NumOfSpeeds;
			DXCoolCap = GetCoilCapacityVariableSpeed( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, ErrFound );
			EqSizing.DesCoolingLoad = DXCoolCap;
			MulSpeedFlowScale = VarSpeedCoil( UnitarySystem( UnitarySysNum ).CoolingCoilIndex ).RatedAirVolFlowRate / VarSpeedCoil( UnitarySystem( UnitarySysNum ).CoolingCoilIndex ).MSRatedAirVolFlowRate( VarSpeedCoil( UnitarySystem( UnitarySysNum ).CoolingCoilIndex ).NormSpedLevel );
			for ( Iter = 1; Iter <= UnitarySystem( UnitarySysNum ).NumOfSpeedCooling; ++Iter ) {
				UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) = VarSpeedCoil( UnitarySystem( UnitarySysNum ).CoolingCoilIndex ).MSRatedAirVolFlowRate( Iter ) * MulSpeedFlowScale;
				UnitarySystem( UnitarySysNum ).CoolMassFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) * StdRhoAir;
				// it seems the ratio should reference the actual flow rates, not the fan flow ???
				if ( UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate > 0.0 && UnitarySystem( UnitarySysNum ).FanExists ) {
					//             UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling)
					UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate;
				} else {
					UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				}
			}

			UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( 1 );
			UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
			UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );

		} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling ) {

			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
				if ( ! allocated( UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate ) ) UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).CoolMassFlowRate ) ) UnitarySystem( UnitarySysNum ).CoolMassFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio ) ) UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
			}

			OnOffAirFlowRatio = 1.0;
			PartLoadRatio = 1.0;
			SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
			SimDXCoilMultiSpeed( BlankString, 1.0, 1.0, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, 0, 0, 0 );
			DXCoolCap = GetCoilCapacityByIndexType( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).CoolingCoilType_Num, ErrFound );
			EqSizing.DesCoolingLoad = DXCoolCap;
			MSHPIndex = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;

			if ( MSHPIndex > 0 ) {
				for ( Iter = DesignSpecMSHP( MSHPIndex ).NumOfSpeedCooling; Iter >= 1; --Iter ) { // use reverse order since we divide by CoolVolumeFlowRate(max)
					if ( DesignSpecMSHP( MSHPIndex ).CoolingVolFlowRatio( Iter ) == AutoSize ) DesignSpecMSHP( MSHPIndex ).CoolingVolFlowRatio( Iter ) = double( Iter ) / double( DesignSpecMSHP( MSHPIndex ).NumOfSpeedCooling );
					UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow * DesignSpecMSHP( MSHPIndex ).CoolingVolFlowRatio( Iter );
					UnitarySystem( UnitarySysNum ).CoolMassFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) * StdRhoAir;
					UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( DesignSpecMSHP( MSHPIndex ).NumOfSpeedCooling );
				}
				UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
			}

		}

		if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {

			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
				if ( ! allocated( UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate ) ) UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).HeatMassFlowRate ) ) UnitarySystem( UnitarySysNum ).HeatMassFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio ) ) UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
			}

			MSHPIndex = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;

			if ( MSHPIndex > 0 ) {
				for ( Iter = DesignSpecMSHP( MSHPIndex ).NumOfSpeedHeating; Iter >= 1; --Iter ) { // use reverse order since we divide by HeatVolumeFlowRate(max)
					if ( DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) == AutoSize ) {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {
							DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) = 1.0;
						} else {
							DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) = double( Iter ) / double( DesignSpecMSHP( MSHPIndex ).NumOfSpeedHeating );
						}
					} else {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {
							if ( DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) < 1.0 ) {
								ShowWarningError( RoutineName + ": " + CompType + " = " + CompName );
								ShowContinueError( "Design specification object = " + DesignSpecMSHP( MSHPIndex ).Name );
								ShowContinueError( "Speed " + TrimSigDigits( Iter ) + " Supply Air Flow Ratio During Heating Operation will be set = 1.0 and the simulation continues");
								DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) = 1.0;
							}
						}
					}
					UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow * DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter );
					UnitarySystem( UnitarySysNum ).HeatMassFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) * StdRhoAir;
					UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( DesignSpecMSHP( MSHPIndex ).NumOfSpeedHeating );
				}
				UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
			}
		} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) {
			SimVariableSpeedCoils( BlankString, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, 0, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 0, 0.0, 1, 0.0, 0.0, 0.0, 0.0 ); //conduct the sizing operation in the VS WSHP

			UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = VarSpeedCoil( UnitarySystem( UnitarySysNum ).HeatingCoilIndex ).NumOfSpeeds;

			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
				if ( ! allocated( UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate ) ) UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).HeatMassFlowRate ) ) UnitarySystem( UnitarySysNum ).HeatMassFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				if ( ! allocated( UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio ) ) UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
			}

			MulSpeedFlowScale = VarSpeedCoil( UnitarySystem( UnitarySysNum ).HeatingCoilIndex ).RatedAirVolFlowRate / VarSpeedCoil( UnitarySystem( UnitarySysNum ).HeatingCoilIndex ).MSRatedAirVolFlowRate( VarSpeedCoil( UnitarySystem( UnitarySysNum ).HeatingCoilIndex ).NormSpedLevel );
			for ( Iter = 1; Iter <= UnitarySystem( UnitarySysNum ).NumOfSpeedHeating; ++Iter ) {
				UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) = VarSpeedCoil( UnitarySystem( UnitarySysNum ).HeatingCoilIndex ).MSRatedAirVolFlowRate( Iter ) * MulSpeedFlowScale;
				UnitarySystem( UnitarySysNum ).HeatMassFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) * StdRhoAir;
				if ( UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate > 0.0 && UnitarySystem( UnitarySysNum ).FanExists ) {
					UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate;
				} else {
					UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				}

			}

			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				if ( allocated( UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate ) ) {
					UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = min( UnitarySystem( UnitarySysNum ).IdleVolumeAirRate, UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( 1 ) );
					UnitarySystem( UnitarySysNum ).IdleMassFlowRate = min( UnitarySystem( UnitarySysNum ).IdleMassFlowRate, UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 ) );
					UnitarySystem( UnitarySysNum ).IdleSpeedRatio = min( UnitarySystem( UnitarySysNum ).IdleSpeedRatio, UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 ) );
				} else {
					UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( 1 );
					UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
					UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
				}
			} else {
				UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
			}
		}

		if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {

			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
				if ( !allocated( UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate ) ) UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				if ( !allocated( UnitarySystem( UnitarySysNum ).CoolMassFlowRate ) ) UnitarySystem( UnitarySysNum ).CoolMassFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				if ( !allocated( UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio ) ) UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
			}
			MSHPIndex = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;

			if ( MSHPIndex > 0 ) {
				for ( Iter = DesignSpecMSHP( MSHPIndex ).NumOfSpeedCooling; Iter >= 1; --Iter ) { // use reverse order since we divide by CoolVolumeFlowRate(max)
					if ( DesignSpecMSHP( MSHPIndex ).CoolingVolFlowRatio( Iter ) == AutoSize ) DesignSpecMSHP( MSHPIndex ).CoolingVolFlowRatio( Iter ) = double( Iter ) / double( DesignSpecMSHP( MSHPIndex ).NumOfSpeedCooling );
					UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow * DesignSpecMSHP( MSHPIndex ).CoolingVolFlowRatio( Iter );
					UnitarySystem( UnitarySysNum ).CoolMassFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) * StdRhoAir;
					UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( DesignSpecMSHP( MSHPIndex ).NumOfSpeedCooling );
				}
				UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
			}

		}
		if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {

			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
				if ( !allocated( UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate ) ) UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				if ( !allocated( UnitarySystem( UnitarySysNum ).HeatMassFlowRate ) ) UnitarySystem( UnitarySysNum ).HeatMassFlowRate.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				if ( !allocated( UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio ) ) UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio.allocate( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
			}

			MSHPIndex = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;
			if ( MSHPIndex > 0 ) {
				for ( Iter = DesignSpecMSHP( MSHPIndex ).NumOfSpeedHeating; Iter >= 1; --Iter ) { // use reverse order since we divide by HeatVolumeFlowRate(max)
					if ( DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) == AutoSize ) {
						DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter ) = double( Iter ) / double( DesignSpecMSHP( MSHPIndex ).NumOfSpeedHeating );
					}
					UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow * DesignSpecMSHP( MSHPIndex ).HeatingVolFlowRatio( Iter );
					UnitarySystem( UnitarySysNum ).HeatMassFlowRate( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) * StdRhoAir;
					UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( Iter ) = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( Iter ) / UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( DesignSpecMSHP( MSHPIndex ).NumOfSpeedHeating );
				}
				UnitarySystem( UnitarySysNum ).IdleVolumeAirRate = UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleMassFlowRate = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
				UnitarySystem( UnitarySysNum ).IdleSpeedRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
			}
		}

// not sure this is still needed
		if ( UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow < 0.0 ) {
			if ( ! SysSizingRunDone ) {
				BranchNum = GetAirBranchIndex( "AirloopHVAC:UnitarySystem", UnitarySystem( UnitarySysNum ).Name );
				FanType = "";
				FanName = "";
				BranchFanFlow = 0.0;
				if ( BranchNum > 0.0 ) GetBranchFanTypeName( BranchNum, FanType, FanName, ErrFound );
				if ( ! ErrFound && BranchNum > 0 ) BranchFanFlow = GetFanDesignVolumeFlowRate( FanType, FanName, ErrFound );
				if ( BranchNum > 0.0 ) BranchFlow = GetBranchFlow( BranchNum );
				if ( BranchFanFlow > 0.0 ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = BranchFanFlow;
				} else if ( BranchFlow > 0.0 ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = BranchFlow;
				} else if ( BranchFlow == AutoSize ) {
					// what do I do?
				}
			}
		}

		//Change the Volume Flow Rates to Mass Flow Rates
		UnitarySystem( UnitarySysNum ).DesignMassFlowRate = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate * StdRhoAir;
		UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow * StdRhoAir;
		UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow = UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow * StdRhoAir;
		UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow * StdRhoAir;

		// why is this here?
		UnitarySystem( UnitarySysNum ).SenLoadLoss = 0.0;
		if ( UnitarySystem( UnitarySysNum ).Humidistat ) {
			UnitarySystem( UnitarySysNum ).LatLoadLoss = 0.0;
		}

		if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {

			SizingMethod = CoolingCapacitySizing;
			// water coils must report their size to parent objects (or split out sizing routines for water coils so they can be call from here)
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {
				SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).CoolingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, 1.0 );
				DataConstantUsedForSizing = GetWaterCoilCapacity( MakeUPPERCase( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ) ), UnitarySystem( UnitarySysNum ).CoolingCoilName, ErrFound );
				EqSizing.DesCoolingLoad = DataConstantUsedForSizing;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = AutoSize;
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilWater_CoolingHXAssisted ) {
				HXCoilName = GetHXDXCoilName( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, ErrFound );
				ActualCoolCoilType = GetCoilObjectTypeNum( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, ErrFound, true );
				SimHXAssistedCoolingCoil( BlankString, true, On, 1.0, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, 1, false, 1.0, false );
				DataConstantUsedForSizing = GetWaterCoilCapacity( MakeUPPERCase( cAllCoilTypes( ActualCoolCoilType ) ), HXCoilName, ErrFound );
				EqSizing.DesCoolingLoad = DataConstantUsedForSizing;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = AutoSize;
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple ) {
				SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand, UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand, 0, 0.0, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 0, 0.0, FirstHVACIteration );
				DataConstantUsedForSizing = GetSimpleCoilCapacity( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, ErrFound );
				EqSizing.DesCoolingLoad = DataConstantUsedForSizing;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = AutoSize;
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP ) EqSizing.DesHeatingLoad = DataConstantUsedForSizing;
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHP ) {
				SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow, UnitarySystem( UnitarySysNum ).FanOpMode, FirstHVACIteration, 0.0, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, 0.0, 0.0, 0, 0.0 );
				DataConstantUsedForSizing = GetWAHPCoilCapacity( cAllCoilTypes( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num ), UnitarySystem( UnitarySysNum ).CoolingCoilName, ErrFound );
				EqSizing.DesCoolingLoad = DataConstantUsedForSizing;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple ) EqSizing.DesHeatingLoad = DataConstantUsedForSizing;
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_PackagedThermalStorageCooling ) {
				SimTESCoil( UnitarySystem( UnitarySysNum ).CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).TESOpMode, 0.0 );
				GetTESCoilCoolingCapacity( UnitarySystem( UnitarySysNum ).CoolingCoilName, DataConstantUsedForSizing, ErrFound, CompType );
				EqSizing.DesCoolingLoad = DataConstantUsedForSizing;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
			}

			PrintFlag = true;
			TempSize = UnitarySystem( UnitarySysNum ).DesignCoolingCapacity;
			SizingString = "Nominal Cooling Capacity [W]";
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = TempSize;
			DataConstantUsedForSizing = 0.0;
			DataFractionUsedForSizing = 0.0;

		}

		if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {

			SizingMethod = HeatingCapacitySizing;

			// water coils must report their size to parent objects (or split out sizing routines for water coils so they can be call from here)
			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
				SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, 1.0 );
				DataConstantUsedForSizing = GetWaterCoilCapacity( MakeUPPERCase( cAllCoilTypes( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num ) ), UnitarySystem( UnitarySysNum ).HeatingCoilName, ErrFound );
				EqSizing.DesHeatingLoad = DataConstantUsedForSizing;
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = AutoSize;
			}

			PrintFlag = true;
			TempSize = UnitarySystem( UnitarySysNum ).DesignHeatingCapacity;
			SizingString = "Nominal Heating Capacity [W]";
			if ( CurSysNum > 0 ) AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = false; // set to false to allow calculation of parent object heating capacity
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple ) DXCoolCap = TempSize;
			if ( CurSysNum > 0 ) AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = true;
			UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = TempSize;
			DataConstantUsedForSizing = 0.0;
			DataFractionUsedForSizing = 0.0;
			DataHeatSizeRatio = 1.0;

		}

		UnitaryHeatCap = UnitarySystem( UnitarySysNum ).DesignHeatingCapacity;

		if ( UnitarySystem( UnitarySysNum ).HeatCoilExists || UnitarySystem( UnitarySysNum ).SuppCoilExists ) {

			SizingMethod = MaxHeaterOutletTempSizing;
			PrintFlag = true;
			TempSize = UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp;
			FieldNum = 17; // N17, \field Maximum Supply Air Temperature
			SizingString = UnitarySystemNumericFields( UnitarySysNum ).FieldNames( FieldNum ) + " [C]";
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp = TempSize;

		}

		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {

			SizingMethod = HeatingCapacitySizing;

			PrintFlag = false;
			TempSize = UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity;
			SizingString = "Supplemental Heating Coil Nominal Capacity [W]";
			if ( TempSize == AutoSize ) {
				IsAutoSize = true;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity = TempSize;
			}

			if ( UnitarySystem( UnitarySysNum ).Humidistat && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat && IsAutoSize) {
				DataConstantUsedForSizing = max( UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity, UnitarySystem( UnitarySysNum ).DesignCoolingCapacity );
				DataFractionUsedForSizing = 1.0;
				SizingMethod = AutoCalculateSizing;
				TempSize = AutoSize;
			}

			PrintFlag = true;
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity = TempSize;
			IsAutoSize = false;
			DataConstantUsedForSizing = 0.0;
			DataFractionUsedForSizing = 0.0;

			SuppHeatCap = UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity;
		}


		// register plant flow rate. Not sure this has ever been tested.
		if ( UnitarySystem( UnitarySysNum ).HeatRecActive ) {
			RegisterPlantCompDesignFlow( UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum, UnitarySystem( UnitarySysNum ).DesignHRWaterVolumeFlow );
		}

		// not sure if this is still needed
		if ( CurOASysNum == 0 && CurZoneEqNum == 0 && UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate <= 0.0 ) {
			BranchFlow = 0.0;
			SystemType = cFurnaceTypes( UnitarySystem( UnitarySysNum ).UnitarySystemType_Num );
			ErrFound = false;
			// check branch flow rate vs system flow rate. Branch must match system if OA system is present
			CheckSystemBranchFlow( SystemType, UnitarySystem( UnitarySysNum ).Name, BranchFlow, UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate, ErrFound );
			if ( ErrFound ) ShowContinueError( "...occurs in " + SystemType + " \"" + UnitarySystem( UnitarySysNum ).Name );
			if ( BranchFlow != AutoSize ) {
				UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = BranchFlow;
			} else {
				UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = max( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow );
			}
			UnitarySystem( UnitarySysNum ).DesignMassFlowRate = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate * StdRhoAir;
		}

		CoolingLoad = TempCoolingLoad;
		HeatingLoad = TempHeatingLoad;
		if ( ++NumUnitarySystemsSized == NumUnitarySystem ) UnitarySystemNumericFields.deallocate(); // remove temporary array for field names at end of sizing

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetUnitarySystemInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   September 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		//
		// PURPOSE OF THIS SUBROUTINE:
		// Manages GetInput processing and program termination

		// METHODOLOGY EMPLOYED:
		// Calls "Get" routines to read in data.

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetUnitarySystemInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ErrorFlag( false ); // true if errors detected in GetUnitarySystemInputData

		// Flow
		GetUnitarySystemInputData( ErrorFlag );

		if( ErrorFlag ) {
			ShowFatalError( RoutineName + "Errors found in getting AirLoopHVAC:UnitarySystem input. Preceding condition(s) causes termination." );
		}

	}

	void
	GetUnitarySystemInputData(
		bool & ErrorsFound // true if errors detected in input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   February 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for system and stores it in System data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using DataHeatBalance::Zone;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilType;
		using namespace DataIPShortCuts;
		using General::TrimSigDigits;
		using DataHVACControllers::ControllerTypes;
		using DataHVACControllers::ControllerSimple_Type;
		using DXCoils::SetCoilSystemCoolingData;
		using DXCoils::GetDXCoilAvailSchPtr;
		using DXCoils::SetDXCoolingCoilData;
		using DataAirSystems::PrimaryAirSystem;
		using DataZoneControls::TempControlledZone;
		using DataZoneControls::NumTempControlledZones;
		using DataZoneControls::HumidityControlZone;
		using DataZoneControls::NumHumidityControlZones;
		using DataZoneControls::ComfortControlledZone;
		using DataZoneControls::NumComfortControlledZones;
		auto & GetWtoAHPSimpleCoilCapacity( WaterToAirHeatPumpSimple::GetCoilCapacity );
		auto & GetWtoAHPSimpleCoilInletNode( WaterToAirHeatPumpSimple::GetCoilInletNode );
		auto & GetWtoAHPSimpleCoilOutletNode( WaterToAirHeatPumpSimple::GetCoilOutletNode );
		auto & GetWtoAHPSimpleCoilIndex( WaterToAirHeatPumpSimple::GetCoilIndex );
		auto & GetWtoAHPSimpleCoilAirFlowRate( WaterToAirHeatPumpSimple::GetCoilAirFlowRate );
		using WaterToAirHeatPumpSimple::SetSimpleWSHPData;
		using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
		using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
		using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;
		using VariableSpeedCoils::GetCoilIndexVariableSpeed;
		using VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed;
		using VariableSpeedCoils::SetVarSpeedCoilData;
		using VariableSpeedCoils::GetVSCoilCondenserInletNode;
		using VariableSpeedCoils::GetVSCoilMinOATCompressor;
		using VariableSpeedCoils::GetVSCoilNumOfSpeeds;
		auto & GetWtoAHPCoilCapacity( WaterToAirHeatPump::GetCoilCapacity );
		auto & GetWtoAHPCoilInletNode( WaterToAirHeatPump::GetCoilInletNode );
		auto & GetWtoAHPCoilOutletNode( WaterToAirHeatPump::GetCoilOutletNode );
		auto & GetWtoAHPCoilIndex( WaterToAirHeatPump::GetCoilIndex );
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );
		auto & GetHeatingCoilInletNode( HeatingCoils::GetCoilInletNode );
		auto & GetHeatingCoilOutletNode( HeatingCoils::GetCoilOutletNode );
		auto & GetHeatingCoilIndex( HeatingCoils::GetCoilIndex );
		using HeatingCoils::GetHeatingCoilTypeNum;
		using HeatingCoils::GetHeatingCoilPLFCurveIndex;
		using HeatingCoils::GetCoilAvailScheduleIndex;
		auto & GetDXCoilCapacity( DXCoils::GetCoilCapacity );
		auto & GetMinOATDXCoilCompressor( DXCoils::GetMinOATCompressor );
		auto & GetDXCoilInletNode( DXCoils::GetCoilInletNode );
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		auto & GetDXCoilCondenserInletNode( DXCoils::GetCoilCondenserInletNode );
		using DXCoils::GetDXCoilIndex;
		auto & GetDXCoilTypeNum( DXCoils::GetCoilTypeNum );
		using DXCoils::GetDXCoilAirFlow;
		using DXCoils::SetDXCoilTypeData;
		auto & GetDXHXAsstdCoilCapacity( HVACHXAssistedCoolingCoil::GetCoilCapacity );
		auto & GetDXHXAsstdCoilInletNode( HVACHXAssistedCoolingCoil::GetCoilInletNode );
		auto & GetDXHXAsstdCoilOutletNode( HVACHXAssistedCoolingCoil::GetCoilOutletNode );
		using HVACHXAssistedCoolingCoil::GetHXDXCoilIndex;
		auto & GetHXAssistedCoilTypeNum( HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum );
		using HVACHXAssistedCoolingCoil::GetActualDXCoilIndex;
		using HVACHXAssistedCoolingCoil::GetHXCoilAirFlowRate;
		using HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		auto & GetWaterCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWaterCoilOutletNode( WaterCoils::GetCoilOutletNode );
		using WaterCoils::GetWaterCoilAvailScheduleIndex;
		using WaterCoils::GetWaterCoilIndex;
		using WaterCoils::GetWaterCoilDesAirFlow;
		auto & GetSteamCoilAirInletNode( SteamCoils::GetCoilAirInletNode );
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilAirOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using SteamCoils::GetSteamCoilAvailScheduleIndex;
		using Fans::GetFanDesignVolumeFlowRate;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using Fans::GetFanIndex;
		using Fans::GetFanAvailSchPtr;
		using Fans::GetFanType;
		using FluidProperties::GetSatDensityRefrig;
		using EMSManager::ManageEMS;
		using SetPointManager::NodeHasSPMCtrlVarType;
		using SetPointManager::iCtrlVarType_Temp;
		using PackagedThermalStorageCoil::GetTESCoilIndex;
		using PackagedThermalStorageCoil::GetTESCoilAirInletNode;
		using PackagedThermalStorageCoil::GetTESCoilAirOutletNode;
		using PackagedThermalStorageCoil::GetTESCoilCoolingAirFlowRate;
		using UserDefinedComponents::GetUserDefinedCoilIndex;
		using UserDefinedComponents::GetUserDefinedCoilAirInletNode;
		using UserDefinedComponents::GetUserDefinedCoilAirOutletNode;
		using PackagedThermalStorageCoil::GetTESCoilCoolingCapacity;
		using DXCoils::SetMSHPDXCoilHeatRecoveryFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const getAirLoopHVACHeatCoolInput( "GetAirLoopHVACHeatCoolInput" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		std::string CurrentModuleObject; // for ease in getting objects
		std::string UnitarySysHeatPumpPerformanceObjectType; // Used for warning messages
		std::string CoolingCoilType; // Used in mining function calls
		std::string CoolingCoilName; // Used in mining function calls
		std::string ChildCoolingCoilType; // Used in mining function calls
		std::string ChildCoolingCoilName; // Used in mining function calls
		std::string HeatingCoilType; // Used in mining function calls
		std::string HeatingCoilName; // Used in mining function calls
		Real64 HeatingSizingRatio; // Used to size DX heating coil wrt DX cooling coil
		std::string SuppHeatCoilType; // Used in mining function calls
		std::string SuppHeatCoilName; // Used in mining function calls
		std::string FanType; // Used in mining function calls
		std::string FanName; // Used in mining function calls
		int FanIndex; // Used in mining function calls
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		Array1D< Real64 > Numbers; // Numeric input items for object
		Real64 FanVolFlowRate; // Fan Max Flow Rate from Fan object (for comparisons to validity)
		Real64 SteamDensity; // steam density
		Real64 TotalFloorAreaOnAirLoop; // AirloopHVAC total floor area served
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool AirNodeFound; // used in error checking
		bool AirLoopFound; // used in error checking
		bool OASysFound; // used in error checking
		bool ZoneEquipmentFound; // TRUE if Unitary System found connected to zone exhaust node
		bool ZoneInletNodeFound; // TRUE if Unitary System found node connection to zone inlet node
		bool errFlag; // Mining function error flag
		bool PrintMessage; // flag to print or not print message
		bool InletNodeNotControlled; // True if using controller on water coil
		int UnitarySysNum; // The Unitary System object currently loading
		int DesignSpecNum; // The design Specification object (multispeed coils) currently loading
		int NumAlphas; // The number of alpha arguments in each object
		int NumNumbers; // The number of numeric arguments in each object
		int IOStatus; // The status of the GetObjectItem call
		int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		// certain object in the input file
		int FanInletNode; // Used for node checking warning messages
		int FanOutletNode; // Used for node checking warning messages
		int CoolingCoilInletNode; // Used for node checking warning messages
		int CoolingCoilOutletNode; // Used for node checking warning messages
		int HeatingCoilInletNode; // Used for node checking warning messages
		int HeatingCoilOutletNode; // Used for node checking warning messages
		int SupHeatCoilInletNode; // Used for node checking warning messages
		int SupHeatCoilOutletNode; // Used for node checking warning messages
		int TotalZonesOnAirLoop; // number of zones connected to air loop
		int ActualCoolCoilType; // Coil type number for HX assisted coils
		int ControlledZoneNum; // loop counter
		int ZoneExhNum; // loop counter
		int EquipNum; // loop counter
		int ZoneInletNum; // loop counter
		int AirLoopNumber; // loop counter
		int BranchNum; // loop counter
		int CompNum; // loop counter
		int OACompNum; // loop counter
		int TstatZoneNum; // loop counter
		int HeatingCoilPLFCurveIndex; // index of heating coil PLF curve
		int SteamIndex; // steam index
		int DXCoilIndex; // index to DX coil
		int HStatZoneNum; // zone index where humidistat is located
		int TempAlphas; // temp alpha array
		int TempNumbers; // temp number array
		int Index; // index to multispeed coil speed data
		int SpeedNum; // multispeed coil speed
		int NumOfSpeedCooling; // number of cooling speeds for multispeed coil
		int NumOfSpeedHeating; // number of heating speeds for multispeed coil
		int StartingSpeedRatioInput; // field where speed ratio inputs start
		int MaxSpeedNum; // maximum number of numeric inputs in design specification object
		int OASysNum; // loop counter index to outside air system
		int AirLoopNum; // loop counter index to primary air system
		std::string HXCoilName; // Cooling coil name within HX assembly

		// local integer representation of input field numbers (i.e., alpha=A1 or numeric=N1)
		int iNameAlphaNum; // get input index to unitary system name
		int iControlTypeAlphaNum; // get input index to unitary system control type
		int iControlZoneAlphaNum; // get input index to unitary system control zone
		int iDehumidControlAlphaNum; // get input index to unitary system dehumidification control
		int iSysAvailSchedAlphaNum; // get input index to unitary system avail schedule
		int iFanTypeAlphaNum; // get input index to unitary system fan type
		int iFanNameAlphaNum; // get input index to unitary system fan name
		int iFanPlaceAlphaNum; // get input index to unitary system fan placement
		int iFanSchedAlphaNum; // get input index to unitary system fan mode op schedule
		int iHeatingCoilTypeAlphaNum; // get input index to unitary system heating coil type
		int iHeatingCoilNameAlphaNum; // get input index to unitary system heating coil name
		int iHeatingCoilSizeRatioNumericNum; // get input index to unitary system heating coil sizing ratio
		int iCoolingCoilTypeAlphaNum; // get input index to unitary system cooling coil type
		int iCoolingCoilNameAlphaNum; // get input index to unitary system cooling coil name
		int iDOASDXCoilAlphaNum; // get input index to unitary system DX coil DOAS specified
		int iRunOnLatentLoadAlphaNum; // get input index to unitary system run on latent load
		int iSuppHeatCoilTypeAlphaNum; // get input index to unitary system supp heat coil type
		int iSuppHeatCoilNameAlphaNum; // get input index to unitary system supp heat coil name
		int iHeatSAFMAlphaNum; // get input index to unitary system heat supp air flow method
		int iCoolSAFMAlphaNum; // get input index to unitary system cool supp air flow method
		int iMaxCoolAirVolFlowNumericNum; // get input index to unitary system cool supply air flow
		int iMaxHeatAirVolFlowNumericNum; // get input index to unitary system heat supply air flow
		int iNoCoolHeatSAFMAlphaNum; // get input index to unitary system no cool/heat supply air flow
		int iMaxNoCoolHeatAirVolFlowNumericNum; // get input index to unitary system no cool/heat supply air flow
		int iDesignSpecMSHPTypeAlphaNum; // get input index to unitary system MSHP coil type
		int iDesignSpecMSHPNameAlphaNum; // get input index to unitary system MSHP coil name
		int iAirInletNodeNameAlphaNum; // get input index to unitary system air inlet node
		int iAirOutletNodeNameAlphaNum; // get input index to unitary system air outlet node
		int iDOASDXMinTempNumericNum; // get input index to unitary system DOAS DX coil min outlet temp
		int iCoolFlowPerFloorAreaNumericNum; // get input index to unitary system cool flow per floor area
		int iCoolFlowPerFracCoolNumericNum; // get input index to unitary system cool flow per fraction cool
		int iCoolFlowPerCoolCapNumericNum; // get input index to unitary system cool flow per cooling cap
		int iHeatFlowPerFloorAreaNumericNum; // get input index to unitary system heat flow per floor area
		int iHeatFlowPerFracCoolNumericNum; // get input index to unitary system heat flow per fraction heat
		int iHeatFlowPerHeatCapNumericNum; // get input index to unitary system heat flow per heating cap
		int iNoCoolHeatFlowPerFloorAreaNumericNum; // get input index to unitary system no cool/heat FPA
		int iNoCoolHeatFlowPerFracCoolNumericNum; // get input index to unitary system no cool/heat FPFC
		int iNoCoolHeatFlowPerFracHeatNumericNum; // get input index to unitary system no cool/heat FPFH
		int iNoCoolHeatFlowPerCoolCapNumericNum; // get input index to unitary system no cool/heat FPCC
		int iNoCoolHeatFlowPerHeatCapNumericNum; // get input index to unitary system no cool/heat FPHC
		int iDesignMaxOutletTempNumericNum; // get input index to unitary system design max outlet temp
		int iMaxOATSuppHeatNumericNum; // get input index to unitary system maximum OAT for supp operation
		int iCondenserNodeAlphaNum; // get input index to unitary system condenser node
		int iMaxONOFFCycPerHourNumericNum; // get input index to unitary system WSHP max cycle per hour
		int iHPTimeConstantNumericNum; // get input index to unitary system WSHP time constant
		int iOnCyclePowerFracNumericNum; // get input index to unitary system WSHP on cycle power
		int iFanDelayTimeNumericNum; // get input index to unitary system WSHP off cycle power
		int iAncillaryOnPowerNumericNum; // get input index to unitary system ancillary on power
		int iAncillaryOffPowerNumericNum; // get input index to unitary system ancillary off power
		int iDesignHRWaterVolFlowNumericNum; // get input index to unitary system design HR water flow
		int iMaxHROutletWaterTempNumericNum; // get input index to unitary system max HR outlet temp
		int iHRWaterInletNodeAlphaNum; // get input index to unitary system HR water inlet node
		int iHRWaterOutletNodeAlphaNum; // get input index to unitary system HR water outlet node

		CurrentModuleObject = "AirloopHVAC:UnitarySystem";
		NumUnitarySystem = GetNumObjectsFound( CurrentModuleObject );

		UnitarySystem.allocate( NumUnitarySystem );
		UnitarySystemNumericFields.allocate( NumUnitarySystem );
		CheckEquipName.allocate( NumUnitarySystem );
		MultiOrVarSpeedHeatCoil.allocate( NumUnitarySystem );
		MultiOrVarSpeedCoolCoil.allocate( NumUnitarySystem );
		CheckEquipName = true;
		MultiOrVarSpeedHeatCoil = false;
		MultiOrVarSpeedCoolCoil = false;

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		TempAlphas = NumAlphas;
		TempNumbers = NumNumbers;

		CurrentModuleObject = "UnitarySystemPerformance:Multispeed";
		NumDesignSpecMultiSpeedHP = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		NumAlphas = max( TempAlphas, NumAlphas );
		NumNumbers = max( TempNumbers, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// Get the data for the DesignSpecification object
		CurrentModuleObject = "UnitarySystemPerformance:Multispeed";
		UnitarySysHeatPumpPerformanceObjectType = CurrentModuleObject;
		DesignSpecMSHP.allocate( NumDesignSpecMultiSpeedHP );

		for ( DesignSpecNum = 1; DesignSpecNum <= NumDesignSpecMultiSpeedHP; ++DesignSpecNum ) {

			GetObjectItem( CurrentModuleObject, DesignSpecNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DesignSpecMSHP, DesignSpecNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}

			DesignSpecMSHP( DesignSpecNum ).Name = Alphas( 1 );

			DesignSpecMSHP( DesignSpecNum ).NumOfSpeedHeating = Numbers( 1 );
			NumOfSpeedHeating = DesignSpecMSHP( DesignSpecNum ).NumOfSpeedHeating;

			DesignSpecMSHP( DesignSpecNum ).NumOfSpeedCooling = Numbers( 2 );
			NumOfSpeedCooling = DesignSpecMSHP( DesignSpecNum ).NumOfSpeedCooling;

			if ( !lAlphaBlanks( 2 ) ) {
				if ( SameString( Alphas( 2 ), "Yes" ) ) {
					DesignSpecMSHP( DesignSpecNum ).SingleModeFlag = true;
				}
				else if ( SameString( Alphas( 2 ), "No" ) ) {
					DesignSpecMSHP( DesignSpecNum ).SingleModeFlag = false;
				}
				else {
					ShowSevereError( CurrentModuleObject + "=\"" + Alphas( 1 ) + "\" invalid " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" illegal key." );
					ShowContinueError( "Valid keys are: Yes or No" );
					ErrorsFound = true;
				}
			}

			// initialize number of speeds (MIN = 4 or greater of cooling and heating speeds x 2)
			MaxSpeedNum = 0; // set to 0 so we know if the inputs are valid

			if ( mod( double( NumNumbers - 2 ), 2.0 ) > 0.0 ) {
				// check that the number of numeric speed input fields match the number of speeds
				ShowSevereError( CurrentModuleObject + " = " + DesignSpecMSHP( DesignSpecNum ).Name );
				ShowContinueError( "Illegal number of entries in Supply Air Flow Rate fields." );
				ShowContinueError( "..number of required Supply Air Flow Rate fields = " + TrimSigDigits( 2 * max( NumOfSpeedCooling, NumOfSpeedHeating ) ) );
				ShowContinueError( "..number of actual Supply Air Flow Rate fields   = " + TrimSigDigits( NumNumbers - 2 ) );
				ErrorsFound = true;
			} else {
				MaxSpeedNum = ( NumNumbers - 2 ) / 2; // Extensible fields included (>4) for cooling and heating
			}

			if ( MaxSpeedNum < max( NumOfSpeedHeating, NumOfSpeedCooling ) ) {
				ShowSevereError( CurrentModuleObject + " = " + DesignSpecMSHP( DesignSpecNum ).Name );
				ShowContinueError( "Illegal number of entries in Supply Air Flow Rate fields." );
				ShowContinueError( "..number of required Supply Air Flow Rate fields = " + TrimSigDigits( 2 * max( NumOfSpeedCooling, NumOfSpeedHeating ) ) );
				ShowContinueError( "..number of actual Supply Air Flow Rate fields   = " + TrimSigDigits( NumNumbers - 2 ) );
				ErrorsFound = true;
			}

			StartingSpeedRatioInput = 3; // start the index counter at the first input for heating (e.g. 3+0*2)
			if ( NumOfSpeedHeating > 0 && MaxSpeedNum > 0 ) {
				DesignSpecMSHP( DesignSpecNum ).HeatingVolFlowRatio.allocate( MaxSpeedNum );
				for ( SpeedNum = 1; SpeedNum <= NumOfSpeedHeating; ++SpeedNum ) {
					DesignSpecMSHP( DesignSpecNum ).HeatingVolFlowRatio( SpeedNum ) = Numbers( StartingSpeedRatioInput + ( ( SpeedNum - 1 ) * 2 ) );
				}
			}

			StartingSpeedRatioInput = 4; // start the index counter at the first input for heating (e.g. 4+0*2)
			if ( NumOfSpeedCooling > 0 && MaxSpeedNum > 0 ) {
				DesignSpecMSHP( DesignSpecNum ).CoolingVolFlowRatio.allocate( MaxSpeedNum );
				for ( SpeedNum = 1; SpeedNum <= NumOfSpeedCooling; ++SpeedNum ) {
					DesignSpecMSHP( DesignSpecNum ).CoolingVolFlowRatio( SpeedNum ) = Numbers( StartingSpeedRatioInput + ( ( SpeedNum - 1 ) * 2 ) );
				}
			}

		}

		// AirLoopHVAC:UnitarySystem,
		iNameAlphaNum = 1; // A1,  \field Name - \required-field
		iControlTypeAlphaNum = 2; // A2,  \field Control Type
		iControlZoneAlphaNum = 3; // A3,  \field Controlling Zone or Thermostat Location
		iDehumidControlAlphaNum = 4; // A4,  \field dehumidification Control Type
		iSysAvailSchedAlphaNum = 5; // A5,  \field Availability Schedule Name
		iAirInletNodeNameAlphaNum = 6; // A6,  \field Air Inlet Node Name - \required-field
		iAirOutletNodeNameAlphaNum = 7; // A7,  \field Air Outlet Node Name - \required-field
		iFanTypeAlphaNum = 8; // A8,  \field Supply Fan Object Type
		iFanNameAlphaNum = 9; // A9,  \field Supply Fan Name
		iFanPlaceAlphaNum = 10; // A10, \field Fan Placement
		iFanSchedAlphaNum = 11; // A11, \field Supply Air Fan Operating Mode Schedule Name
		iHeatingCoilTypeAlphaNum = 12; // A12, \field Heating Coil Object Type
		iHeatingCoilNameAlphaNum = 13; // A13, \field Heating Coil Name
		iHeatingCoilSizeRatioNumericNum = 1; // N1,  \field DX Heating Coil Sizing Ratio
		iCoolingCoilTypeAlphaNum = 14; // A14, \field Cooling Coil Object Type
		iCoolingCoilNameAlphaNum = 15; // A15, \field Cooling Coil Name
		iDOASDXCoilAlphaNum = 16; // A16, \field Use DOAS DX Cooling Coil
		iDOASDXMinTempNumericNum = 2; // N2 , \field DOAS DX Cooling Coil Leaving Minimum Air Temperature
		iRunOnLatentLoadAlphaNum = 17; // A17, \field Run on Latent Load
		iSuppHeatCoilTypeAlphaNum = 18; // A18, \field Supplemental Heating Coil Object Type
		iSuppHeatCoilNameAlphaNum = 19; // A19, \field Supplemental Heating Coil Name
		iCoolSAFMAlphaNum = 20; // A20, \field Supply air Flow Rate Method During Cooling Operation
		iMaxCoolAirVolFlowNumericNum = 3; // N3,  \field Supply Air Flow Rate During Cooling Operation
		iCoolFlowPerFloorAreaNumericNum = 4; // N4,  \field Supply Air Flow Rate Per Floor Area During Cooling Operation
		iCoolFlowPerFracCoolNumericNum = 5; // N5,  \field Fraction of Autosized Design Cooling Supply Air Flow Rate
		iCoolFlowPerCoolCapNumericNum = 6; // N6,  \field Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation
		iHeatSAFMAlphaNum = 21; // A21, \field Supply air Flow Rate Method During Heating Operation
		iMaxHeatAirVolFlowNumericNum = 7; // N7,  \field Supply Air Flow Rate During Heating Operation
		iHeatFlowPerFloorAreaNumericNum = 8; // N8,  \field Supply Air Flow Rate Per Floor Area during Heating Operation
		iHeatFlowPerFracCoolNumericNum = 9; // N9,  \field Fraction of Autosized Design Heating Supply Air Flow Rate
		iHeatFlowPerHeatCapNumericNum = 10; // N10, \field Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation
		iNoCoolHeatSAFMAlphaNum = 22; // A22, \field Supply air Flow Rate Method When No Cooling or Heating is Required
		iMaxNoCoolHeatAirVolFlowNumericNum = 11; // N11, \field Supply Air Flow Rate When No Cooling or Heating is Required
		iNoCoolHeatFlowPerFloorAreaNumericNum = 12; // N12, \field Supply Air Flow Rate Per Floor Area When No Cooling/Heating is Req
		iNoCoolHeatFlowPerFracCoolNumericNum = 13; // N13, \field Fraction of Autosized Design Cooling Supply Air Flow Rate
		iNoCoolHeatFlowPerFracHeatNumericNum = 14; // N14, \field Fraction of Autosized Design Heating Supply Air Flow Rate
		iNoCoolHeatFlowPerCoolCapNumericNum = 15; // N15, \field Design Supply Air Flow Rate Per Unit of Capacity During Cooling Op
		iNoCoolHeatFlowPerHeatCapNumericNum = 16; // N16, \field Design Supply Air Flow Rate Per Unit of Capacity During Heating Op
		iDesignMaxOutletTempNumericNum = 17; // N17, \field Maximum Supply Air Temperature
		iMaxOATSuppHeatNumericNum = 18; // N18, \field Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Op
		iCondenserNodeAlphaNum = 23; // A23, \field Outdoor Dry-Bulb Temperature Sensor Node Name
		iMaxONOFFCycPerHourNumericNum = 19; // N19, \field Maximum Cycling Rate
		iHPTimeConstantNumericNum = 20; // N20, \field Heat Pump Time Constant
		iOnCyclePowerFracNumericNum = 21; // N21, \field Fraction of On-Cycle Power Use
		iFanDelayTimeNumericNum = 22; // N22, \field Heat Pump Fan Delay Time
		iAncillaryOnPowerNumericNum = 23; // N23, \field Ancilliary On-Cycle Electric Power
		iAncillaryOffPowerNumericNum = 24; // N24, \field Ancilliary Off-Cycle Electric Power
		iDesignHRWaterVolFlowNumericNum = 25; // N25, \field Design Heat Recovery Water Flow Rate
		iMaxHROutletWaterTempNumericNum = 26; // N26, \field Maximum Temperature for Heat Recovery
		iHRWaterInletNodeAlphaNum = 24; // A24, \field Heat Recovery Water Inlet Node Name
		iHRWaterOutletNodeAlphaNum = 25; // A25, \field Heat Recovery Water Outlet Node Name

		iDesignSpecMSHPTypeAlphaNum = 26; // A26, \field design Specification Multispeed Heat Pump Object Type
		iDesignSpecMSHPNameAlphaNum = 27; // A27; \field design Specification Multispeed Heat Pump Object Name

		// Get the data for the Unitary System
		CurrentModuleObject = "AirloopHVAC:UnitarySystem";
		for ( UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum ) {

			FanInletNode = 0;
			FanOutletNode = 0;
			FanVolFlowRate = 0.0;
			CoolingCoilInletNode = 0;
			CoolingCoilOutletNode = 0;
			HeatingCoilInletNode = 0;
			HeatingCoilOutletNode = 0;
			SupHeatCoilInletNode = 0;
			SupHeatCoilOutletNode = 0;

			CurrentModuleObject = "AirLoopHVAC:UnitarySystem";
			UnitarySystem( UnitarySysNum ).UnitarySystemType = CurrentModuleObject;
			UnitarySystem( UnitarySysNum ).UnitarySystemType_Num = UnitarySystem_AnyCoilType;

			GetObjectItem( CurrentModuleObject, UnitarySysNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			UnitarySystemNumericFields( UnitarySysNum ).FieldNames.allocate( TempNumbers );
			UnitarySystemNumericFields( UnitarySysNum ).FieldNames = "";
			UnitarySystemNumericFields( UnitarySysNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( iNameAlphaNum ), UnitarySystem, UnitarySysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( iNameAlphaNum ) = "xxxxx";
			}

			UnitarySystem( UnitarySysNum ).Name = Alphas( iNameAlphaNum );

			if ( SameString( Alphas( iControlTypeAlphaNum ), "Load" ) ) {
				UnitarySystem( UnitarySysNum ).ControlType = LoadBased;
			} else if ( SameString( Alphas( iControlTypeAlphaNum ), "SetPoint" ) ) {
				UnitarySystem( UnitarySysNum ).ControlType = SetPointBased;
			} else {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iControlTypeAlphaNum ) + " = " + Alphas( iControlTypeAlphaNum ) );
				ErrorsFound = true;
			}

			//Get the Controlling Zone or Location of the Thermostat
			UnitarySystem( UnitarySysNum ).ControlZoneNum = FindItemInList( Alphas( iControlZoneAlphaNum ), Zone );

			if ( ! lAlphaBlanks( iDehumidControlAlphaNum ) ) {
				if ( SameString( Alphas( iDehumidControlAlphaNum ), "None" ) || SameString( Alphas( iDehumidControlAlphaNum ), "Multimode" ) || SameString( Alphas( iDehumidControlAlphaNum ), "CoolReheat" ) ) {
					AirNodeFound = false;
					if ( SameString( Alphas( iDehumidControlAlphaNum ), "Multimode" ) ) {
						UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DehumidControl_Multimode;
						UnitarySystem( UnitarySysNum ).Humidistat = true;
					}
					if ( SameString( Alphas( iDehumidControlAlphaNum ), "CoolReheat" ) ) {
						UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DehumidControl_CoolReheat;
						UnitarySystem( UnitarySysNum ).Humidistat = true;
						if ( lAlphaBlanks( iSuppHeatCoilNameAlphaNum ) ) {
						}
					}
					if ( SameString( Alphas( iDehumidControlAlphaNum ), "None" ) ) {
						UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DehumidControl_None;
						UnitarySystem( UnitarySysNum ).Humidistat = false;
					}
					if ( UnitarySystem( UnitarySysNum ).Humidistat && UnitarySystem( UnitarySysNum ).ControlType == LoadBased ) {
						for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
							if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != UnitarySystem( UnitarySysNum ).ControlZoneNum ) continue;
							AirNodeFound = true;
						}
						if ( ! AirNodeFound ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Did not find Air Node (Zone with Humidistat)." );
							ShowContinueError( "specified " + cAlphaFields( iControlZoneAlphaNum ) + " = " + Alphas( iControlZoneAlphaNum ) );
							ErrorsFound = true;
						}
					}
				} else { // invalid input
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iDehumidControlAlphaNum ) + " = " + Alphas( iDehumidControlAlphaNum ) );
					UnitarySystem( UnitarySysNum ).Humidistat = false;
					ErrorsFound = true;
				}
			}

			if ( lAlphaBlanks( iSysAvailSchedAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).SysAvailSchedPtr = ScheduleAlwaysOn;
			} else {
				UnitarySystem( UnitarySysNum ).SysAvailSchedPtr = GetScheduleIndex( Alphas( iSysAvailSchedAlphaNum ) );
				if ( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iSysAvailSchedAlphaNum ) + " = " + Alphas( iSysAvailSchedAlphaNum ) );
					ErrorsFound = true;
				}
			}

			UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum = GetOnlySingleNode( Alphas( iAirInletNodeNameAlphaNum ), ErrorsFound, CurrentModuleObject, Alphas( iNameAlphaNum ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum = GetOnlySingleNode( Alphas( iAirOutletNodeNameAlphaNum ), ErrorsFound, CurrentModuleObject, Alphas( iNameAlphaNum ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			//Get fan data
			FanType = Alphas( iFanTypeAlphaNum );
			FanName = Alphas( iFanNameAlphaNum );

			if ( ! lAlphaBlanks( iFanTypeAlphaNum ) ) {
				IsNotOK = false;
				GetFanType( FanName, UnitarySystem( UnitarySysNum ).FanType_Num, IsNotOK, CurrentModuleObject, Alphas( iNameAlphaNum ) );
				if ( IsNotOK ) {
					ErrorsFound = true;
				}
				UnitarySystem( UnitarySysNum ).FanExists = true;
			}

			if ( UnitarySystem( UnitarySysNum ).FanType_Num == FanType_SimpleOnOff || UnitarySystem( UnitarySysNum ).FanType_Num == FanType_SimpleConstVolume || UnitarySystem( UnitarySysNum ).FanType_Num == FanType_SimpleVAV || UnitarySystem( UnitarySysNum ).FanType_Num == FanType_ComponentModel ) {
				IsNotOK = false;
				ValidateComponent( FanType, FanName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;

				} else { // mine data from fan object

					// Get the fan index
					errFlag = false;
					GetFanIndex( FanName, UnitarySystem( UnitarySysNum ).FanIndex, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					// Get the Design Fan Volume Flow Rate
					errFlag = false;
					FanVolFlowRate = GetFanDesignVolumeFlowRate( FanType, FanName, errFlag );
					if ( FanVolFlowRate == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					UnitarySystem( UnitarySysNum ).ActualFanVolFlowRate = FanVolFlowRate;
					UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = FanVolFlowRate;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					// Get the Fan Inlet Node
					errFlag = false;
					FanInletNode = GetFanInletNode( FanType, FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					// Get the Fan Outlet Node
					errFlag = false;
					FanOutletNode = GetFanOutletNode( FanType, FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					// Get the fan's availability schedule
					errFlag = false;
					UnitarySystem( UnitarySysNum ).FanAvailSchedPtr = GetFanAvailSchPtr( FanType, FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

				} // IF (IsNotOK) THEN

			} else if ( UnitarySystem( UnitarySysNum ).FanExists ) {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iFanTypeAlphaNum ) + " = " + Alphas( iFanTypeAlphaNum ) );
				ErrorsFound = true;
			} // IF (UnitarySystem(UnitarySysNum)%FanType_Num...

			// Add fan to component sets array
			if ( UnitarySystem( UnitarySysNum ).FanExists ) SetUpCompSets( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iFanTypeAlphaNum ), Alphas( iFanNameAlphaNum ), NodeID( FanInletNode ), NodeID( FanOutletNode ) );

			if ( SameString( Alphas( iFanPlaceAlphaNum ), "BlowThrough" ) ) UnitarySystem( UnitarySysNum ).FanPlace = BlowThru;
			if ( SameString( Alphas( iFanPlaceAlphaNum ), "DrawThrough" ) ) UnitarySystem( UnitarySysNum ).FanPlace = DrawThru;
			if ( UnitarySystem( UnitarySysNum ).FanPlace == 0 && UnitarySystem( UnitarySysNum ).FanExists ) {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iFanPlaceAlphaNum ) + " = " + Alphas( iFanPlaceAlphaNum ) );
				ErrorsFound = true;
			}

			UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr = GetScheduleIndex( Alphas( iFanSchedAlphaNum ) );
			if ( ! lAlphaBlanks( iFanSchedAlphaNum ) && UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iFanSchedAlphaNum ) + " = " + Alphas( iFanSchedAlphaNum ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( iFanSchedAlphaNum ) ) {
				if ( UnitarySystem( UnitarySysNum ).ControlType == SetPointBased ) {
					// Fan operating mode must be constant fan so that the coil outlet temp is proportional to PLR
					// Cycling fan always outputs the full load outlet air temp so should not be used with set point based control
					UnitarySystem( UnitarySysNum ).FanOpMode = ContFanCycCoil;
				} else {
					UnitarySystem( UnitarySysNum ).FanOpMode = CycFanCycCoil;
					if ( UnitarySystem( UnitarySysNum ).FanType_Num != FanType_SimpleOnOff && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( cAlphaFields( iFanTypeAlphaNum ) + " = " + Alphas( iFanTypeAlphaNum ) );
						ShowContinueError( "Fan type must be Fan:OnOff when " + cAlphaFields( iFanSchedAlphaNum ) + " = Blank." );
						ErrorsFound = true;
					}
				}
			} else if ( ! lAlphaBlanks( iFanSchedAlphaNum ) && UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr > 0 && UnitarySystem( UnitarySysNum ).ControlType == SetPointBased ) {
				if ( ! CheckScheduleValueMinMax( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "For " + cAlphaFields( iFanTypeAlphaNum ) + " = " + Alphas( iFanTypeAlphaNum ) );
					ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0)." );
					ShowContinueError( "Error found in " + cAlphaFields( iFanSchedAlphaNum ) + " = " + Alphas( iFanSchedAlphaNum ) );
					ShowContinueError( "...schedule values must be (>0., <=1.)" );
					ErrorsFound = true;
				}
			}

			// Check fan's schedule for cycling fan operation IF constant volume fan is used
			if ( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr > 0 && UnitarySystem( UnitarySysNum ).FanType_Num == FanType_SimpleConstVolume ) {
				if ( ! CheckScheduleValueMinMax( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "For " + cAlphaFields( iFanTypeAlphaNum ) + " = " + Alphas( iFanTypeAlphaNum ) );
					ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0)." );
					ShowContinueError( "Error found in " + cAlphaFields( iFanSchedAlphaNum ) + " = " + Alphas( iFanSchedAlphaNum ) );
					ShowContinueError( "...schedule values must be (>0., <=1.)" );
					ErrorsFound = true;
				}
			}

			//Get coil data
			HeatingCoilType = Alphas( iHeatingCoilTypeAlphaNum );
			HeatingCoilName = Alphas( iHeatingCoilNameAlphaNum );
			if ( NumNumbers > ( iHeatingCoilSizeRatioNumericNum - 1 ) ) {
				HeatingSizingRatio = Numbers( iHeatingCoilSizeRatioNumericNum );
				UnitarySystem( UnitarySysNum ).HeatingSizingRatio = HeatingSizingRatio;
			} else {
				HeatingSizingRatio = 1.0;
			}
			HeatingCoilPLFCurveIndex = 0;
			UnitarySystem( UnitarySysNum ).HeatingCoilName = HeatingCoilName;
			if ( ! lAlphaBlanks( iHeatingCoilTypeAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).HeatCoilExists = true;
				PrintMessage = false;
			}

			if ( SameString( HeatingCoilType, "Coil:Heating:DX:VariableSpeed" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingAirToAirVariableSpeed;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:DX:MultiSpeed" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = CoilDX_MultiSpeedHeating;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:Water" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingWater;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:Steam" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingSteam;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:WaterToAirHeatPump:EquationFit" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingWaterToAirHPSimple;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingWaterToAirHP;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:Electric:MultiStage" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingElectric_MultiStage;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:Gas:MultiStage" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_HeatingGas_MultiStage;
			} else if ( SameString( HeatingCoilType, "Coil:Heating:Gas" ) || SameString( HeatingCoilType, "Coil:Heating:Electric" ) || SameString( HeatingCoilType, "Coil:Heating:Desuperheater" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = GetHeatingCoilTypeNum( HeatingCoilType, HeatingCoilName, errFlag );
			} else if ( SameString( HeatingCoilType, "Coil:UserDefined" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = Coil_UserDefined;
			} else if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				UnitarySystem( UnitarySysNum ).HeatingCoilType_Num = GetDXCoilTypeNum( HeatingCoilType, HeatingCoilName, errFlag, PrintMessage );
			}

			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {

				UnitarySystem( UnitarySysNum ).DXHeatingCoil = true;
				errFlag = false;

				ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;

				} else { // mine data from DX heating coil

					// Get DX heating coil index
					GetDXCoilIndex( HeatingCoilName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = GetDXCoilAvailSchPtr( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get DX heating coil capacity
					UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetDXCoilCapacity( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).DesignHeatingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get DX coil air flow rate.
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = GetDXCoilAirFlow( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Nodes
					HeatingCoilInletNode = GetDXCoilInletNode( HeatingCoilType, HeatingCoilName, errFlag );
					HeatingCoilOutletNode = GetDXCoilOutletNode( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					SetDXCoolingCoilData( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HeatingSizingRatio );
				}

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit ) {

				UnitarySystem( UnitarySysNum ).DXHeatingCoil = true;
				errFlag = false;

				ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
				} else {

					UnitarySystem( UnitarySysNum ).HeatingCoilIndex = GetCoilIndexVariableSpeed( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = GetVSCoilNumOfSpeeds( HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = ScheduleAlwaysOn;

					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = GetCoilAirFlowRateVariableSpeed( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					HeatingCoilInletNode = GetCoilInletNodeVariableSpeed( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					HeatingCoilOutletNode = GetCoilOutletNodeVariableSpeed( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}
					// Get DX heating coil capacity
					UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetCoilCapacityVariableSpeed( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).DesignHeatingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}
				}
			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating ) {
				UnitarySystem( UnitarySysNum ).DXHeatingCoil = true;
				errFlag = false;
				GetDXCoilIndex( HeatingCoilName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, errFlag, HeatingCoilType );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}

				UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = GetDXCoilAvailSchPtr( HeatingCoilType, HeatingCoilName, errFlag );

				// Get DX coil air flow rate.
				UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = GetDXCoilAirFlow( HeatingCoilType, HeatingCoilName, errFlag );
				if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}

					HeatingCoilInletNode = GetDXCoilInletNode( HeatingCoilType, HeatingCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}
				HeatingCoilOutletNode = GetDXCoilOutletNode( HeatingCoilType, HeatingCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {

				errFlag = false;
				GetHeatingCoilIndex( HeatingCoilName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}
				HeatingCoilInletNode = GetHeatingCoilInletNode( HeatingCoilType, HeatingCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}
				HeatingCoilOutletNode = GetHeatingCoilOutletNode( HeatingCoilType, HeatingCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				}

				UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetHeatingCoilCapacity( HeatingCoilType, HeatingCoilName, errFlag );

				if ( UnitarySystem( UnitarySysNum ).DesignHeatingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingDesuperheater ) {
				errFlag = false;
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
					errFlag = false;
				} else {

					ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;

					} else { // mine data from heating coil

						// Get heating coil index
						errFlag = false;
						GetHeatingCoilIndex( HeatingCoilName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the design heating capacity
						UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetHeatingCoilCapacity( HeatingCoilType, HeatingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).DesignHeatingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = GetCoilAvailScheduleIndex( HeatingCoilType, HeatingCoilName, errFlag );

						// Get the Heating Coil Inlet Node
						HeatingCoilInletNode = GetHeatingCoilInletNode( HeatingCoilType, HeatingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Heating Coil Outlet Node
						HeatingCoilOutletNode = GetHeatingCoilOutletNode( HeatingCoilType, HeatingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Heating Coil PLF Curve Index
						HeatingCoilPLFCurveIndex = GetHeatingCoilPLFCurveIndex( HeatingCoilType, HeatingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}
						// These heating coil types do not have an air flow input field
						if ( UnitarySystem( UnitarySysNum ).RequestAutoSize ) {
							UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
						}
					} // IF (IsNotOK) THEN

				}

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
				ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					errFlag = false;
					UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = GetWaterCoilAvailScheduleIndex( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).HeatingCoilIndex = GetWaterCoilIndex( "COIL:HEATING:WATER", HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cAlphaFields( iHeatingCoilNameAlphaNum ) + " = " + HeatingCoilName );
						ErrorsFound = true;
						errFlag = false;
					}

					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil water Inlet or control Node number
					UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil hot water max volume flow rate
					UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Inlet Node
					HeatingCoilInletNode = GetWaterCoilInletNode( "Coil:Heating:Water", HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Outlet Node
					HeatingCoilOutletNode = GetWaterCoilOutletNode( "Coil:Heating:Water", HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

				}

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingSteam ) {
				ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					errFlag = false;
					UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = GetSteamCoilAvailScheduleIndex( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).HeatingCoilIndex = GetSteamCoilIndex( "COIL:HEATING:STEAM", HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cAlphaFields( iHeatingCoilNameAlphaNum ) + " = " + HeatingCoilName );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil steam inlet node number
					errFlag = false;
					UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil steam max volume flow rate
					UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, errFlag );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

					if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getAirLoopHVACHeatCoolInput );
						UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow *= SteamDensity;
						errFlag = false;
					}

					// Get the Heating Coil Inlet Node
					errFlag = false;
					HeatingCoilInletNode = GetSteamCoilAirInletNode( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Outlet Node
					HeatingCoilOutletNode = GetCoilAirOutletNode( UnitarySystem( UnitarySysNum ).HeatingCoilIndex, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					if ( UnitarySystem( UnitarySysNum ).RequestAutoSize ) {
						UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
					}

				}

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple ) {
				UnitarySystem( UnitarySysNum ).DXHeatingCoil = true;
				ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					errFlag = false;
					UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = ScheduleAlwaysOn;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).HeatingCoilIndex = GetWtoAHPSimpleCoilIndex( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cAlphaFields( iHeatingCoilNameAlphaNum ) + " = " + HeatingCoilName );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetWtoAHPSimpleCoilCapacity( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get DX coil air flow rate. Later fields will overwrite this IF input field is present
					errFlag = false;
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = GetWtoAHPSimpleCoilAirFlowRate( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					// Get the Heating Coil Inlet Node
					errFlag = false;
					HeatingCoilInletNode = GetWtoAHPSimpleCoilInletNode( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Outlet Node
					HeatingCoilOutletNode = GetWtoAHPSimpleCoilOutletNode( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

				}

			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP ) {
				UnitarySystem( UnitarySysNum ).DXHeatingCoil = true;
				ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					errFlag = false;
					UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = ScheduleAlwaysOn;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).HeatingCoilIndex = GetWtoAHPCoilIndex( HeatingCoilType, HeatingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cAlphaFields( iHeatingCoilNameAlphaNum ) + " = " + HeatingCoilName );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetWtoAHPCoilCapacity( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Inlet Node
					errFlag = false;
					HeatingCoilInletNode = GetWtoAHPCoilInletNode( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					// Get the Heating Coil Outlet Node
					HeatingCoilOutletNode = GetWtoAHPCoilOutletNode( HeatingCoilType, HeatingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

				}

				} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_UserDefined ) {
					ValidateComponent( HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Heating coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = ScheduleAlwaysOn;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						GetUserDefinedCoilIndex( HeatingCoilName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, errFlag, CurrentModuleObject );
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( iHeatingCoilNameAlphaNum ) + " = " + HeatingCoilName );
							ErrorsFound = true;
							errFlag = false;
						}

// **** How to get this info ****
//						UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetWtoAHPCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
//						if ( errFlag ) {
//							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
//							ErrorsFound = true;
//							errFlag = false;
//						}

						// Get the Cooling Coil Inlet Node
						errFlag = false;
						GetUserDefinedCoilAirInletNode( HeatingCoilName, HeatingCoilInletNode, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Outlet Node
						GetUserDefinedCoilAirOutletNode( HeatingCoilName, HeatingCoilOutletNode, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

					}

			} else if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iHeatingCoilTypeAlphaNum ) + " = " + Alphas( iHeatingCoilTypeAlphaNum ) );
				ErrorsFound = true;
			} // IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &, etc.

			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {
				UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil = true;
			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) {
				UnitarySystem( UnitarySysNum ).VarSpeedHeatingCoil = true;
			}

			// coil outlet node set point has priority, IF not exist, then use system outlet node
			if ( NodeHasSPMCtrlVarType( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum, iCtrlVarType_Temp ) ) UnitarySystem( UnitarySysNum ).SystemHeatControlNodeNum = UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum;
			if ( NodeHasSPMCtrlVarType( HeatingCoilOutletNode, iCtrlVarType_Temp ) ) UnitarySystem( UnitarySysNum ).SystemHeatControlNodeNum = HeatingCoilOutletNode;

			UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum = HeatingCoilInletNode;
			UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum = HeatingCoilOutletNode;
			UnitarySystem( UnitarySysNum ).HeatingCoilName = HeatingCoilName;

			// Add heating coil to component sets array
			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != CoilDX_MultiSpeedHeating ) {
					SetUpCompSets( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iHeatingCoilTypeAlphaNum ), Alphas( iHeatingCoilNameAlphaNum ), NodeID( HeatingCoilInletNode ), NodeID( HeatingCoilOutletNode ) );
				} else {
					SetUpCompSets( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iHeatingCoilTypeAlphaNum ), Alphas( iHeatingCoilNameAlphaNum ), "UNDEFINED", "UNDEFINED" );
				}
			}
			// Get Cooling Coil Information IF available
			CoolingCoilType = Alphas( iCoolingCoilTypeAlphaNum );
			CoolingCoilName = Alphas( iCoolingCoilNameAlphaNum );
			if ( ! lAlphaBlanks( iCoolingCoilTypeAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).CoolCoilExists = true;

				//       Find the type of coil. do not print message since this may not be the correct coil type.
				errFlag = false;
				if ( SameString( CoolingCoilType, "Coil:Cooling:DX:VariableSpeed" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:DX:MultiSpeed" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = CoilDX_MultiSpeedCooling;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:Water" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingWater;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingWaterDetailed;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = CoilDX_CoolingTwoStageWHumControl;
				} else if ( SameString( CoolingCoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = GetHXAssistedCoilTypeNum( CoolingCoilType, CoolingCoilName, errFlag, PrintMessage );
				} else if ( SameString( CoolingCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = GetHXAssistedCoilTypeNum( CoolingCoilType, CoolingCoilName, errFlag, PrintMessage );
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:WaterToAirHeatPump:EquationFit" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingWaterToAirHPSimple;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingWaterToAirHP;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:DX:SingleSpeed" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = CoilDX_CoolingSingleSpeed;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:DX:TwoSpeed" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = CoilDX_CoolingTwoSpeed;
				} else if ( SameString( CoolingCoilType, "Coil:UserDefined" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = Coil_UserDefined;
				} else if ( SameString( CoolingCoilType, "Coil:Cooling:DX:SingleSpeed:ThermalStorage" ) ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilType_Num = CoilDX_PackagedThermalStorageCooling;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iCoolingCoilTypeAlphaNum ) + " = " + Alphas( iCoolingCoilTypeAlphaNum ) );
				}

				if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;

					} else { // mine data from DX cooling coil

						if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed ) UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = 2;

						// Get DX cooling coil index
						GetDXCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, IsNotOK );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr( CoolingCoilType, CoolingCoilName, errFlag );

						// Get DX cooling coil capacity
						errFlag = false;
						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetDXCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get DX coil air flow rate. Latter fields will overwrite this IF input field is present
						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetDXCoilAirFlow( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Cooling Coil Nodes
						errFlag = false;
						CoolingCoilInletNode = GetDXCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						CoolingCoilOutletNode = GetDXCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get Outdoor condenser node from DX coil object
						errFlag = false;
						UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						if ( UnitarySystem( UnitarySysNum ).FanExists ) {
							errFlag = false;
							GetFanIndex( FanName, FanIndex, errFlag, CurrentModuleObject );
							SetDXCoolingCoilData( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, FanName );
							SetDXCoolingCoilData( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, FanIndex );
							if ( errFlag ) {
								ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
								ErrorsFound = true;
							}
						}
						if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
							if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
								UnitarySystem( UnitarySysNum ).HeatPump = true;
							}
						}

					} // IF (IsNotOK) THEN

					// Push heating coil PLF curve index to DX coil
					if ( HeatingCoilPLFCurveIndex > 0 ) {
						SetDXCoolingCoilData( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ErrorsFound, HeatingCoilPLFCurveIndex );
					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;

					} else { // mine data from DX cooling coil

						// Get DX cooling coil index
						GetDXCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, IsNotOK );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr( CoolingCoilType, CoolingCoilName, errFlag );

						// Get DX cooling coil capacity
						errFlag = false;
						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetDXCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get DX coil air flow rate. Later fields will overwrite this IF input field is present
						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetDXCoilAirFlow( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Cooling Coil Nodes
						errFlag = false;
						CoolingCoilInletNode = GetDXCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						CoolingCoilOutletNode = GetDXCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get Outdoor condenser node from DX coil object
						errFlag = false;
						UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

					} // IF (IsNotOK) THEN

					// Push heating coil PLF curve index to DX coil
					if ( HeatingCoilPLFCurveIndex > 0 ) {
						SetDXCoolingCoilData( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ErrorsFound, HeatingCoilPLFCurveIndex );
					}

					if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
							UnitarySystem( UnitarySysNum ).HeatPump = true;
						}
					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingHXAssisted ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;

					} else { // mine data from heat exchanger assisted cooling coil

						// Get DX heat exchanger assisted cooling coil index
						errFlag = false;
						GetHXDXCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, IsNotOK );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						errFlag = false;
						ChildCoolingCoilName = GetHXDXCoilName( CoolingCoilType, CoolingCoilName, IsNotOK );
						ChildCoolingCoilType = GetHXDXCoilType( CoolingCoilType, CoolingCoilName, IsNotOK );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr( ChildCoolingCoilType, ChildCoolingCoilName, errFlag );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get DX cooling coil capacity
						errFlag = false;
						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetDXHXAsstdCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get DX coil air flow rate. Later fields will overwrite this IF input field is present
						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetDXCoilAirFlow( ChildCoolingCoilType, ChildCoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Cooling Coil Nodes
						errFlag = false;
						CoolingCoilInletNode = GetDXHXAsstdCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get Outdoor condenser node from heat exchanger assisted DX coil object
						errFlag = false;
						UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( "COIL:COOLING:DX:SINGLESPEED", GetHXDXCoilName( CoolingCoilType, CoolingCoilName, errFlag ), errFlag );

						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Push heating coil PLF curve index to DX coil
						if ( HeatingCoilPLFCurveIndex > 0 ) {
							// get the actual index to the DX cooling coil object
							DXCoilIndex = GetActualDXCoilIndex( CoolingCoilType, CoolingCoilName, ErrorsFound );
							UnitarySystem( UnitarySysNum ).ActualDXCoilIndexForHXAssisted = DXCoilIndex;
							SetDXCoolingCoilData( DXCoilIndex, ErrorsFound, HeatingCoilPLFCurveIndex );
						}

						if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
							if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
								UnitarySystem( UnitarySysNum ).HeatPump = true;
							}
						}

					} // IF (IsNotOK) THEN
				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilWater_CoolingHXAssisted ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;

					} else { // mine data from heat exchanger assisted cooling coil

						errFlag = false;
						ActualCoolCoilType = GetCoilObjectTypeNum( CoolingCoilType, CoolingCoilName, errFlag, true );
						HXCoilName = GetHXDXCoilName( CoolingCoilType, CoolingCoilName, errFlag );

						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get DX heat exchanger assisted cooling coil index
						errFlag = false;
						GetHXDXCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = GetWaterCoilAvailScheduleIndex( cAllCoilTypes( ActualCoolCoilType ), HXCoilName, errFlag );
						UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow = GetCoilMaxWaterFlowRate( cAllCoilTypes( ActualCoolCoilType ), HXCoilName, errFlag );
						// Get the Cooling Coil water Inlet Node number
						UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode = GetCoilWaterInletNode( cAllCoilTypes( ActualCoolCoilType ), HXCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Cooling Coil Nodes
						errFlag = false;
						CoolingCoilInletNode = GetDXHXAsstdCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetHXCoilAirFlowRate( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						UnitarySystem( UnitarySysNum ).CondenserNodeNum = 0;

						// Push heating coil PLF curve index to DX coil
						if ( HeatingCoilPLFCurveIndex > 0 ) {
							// get the actual index to the DX cooling coil object
							DXCoilIndex = GetActualDXCoilIndex( CoolingCoilType, CoolingCoilName, ErrorsFound );
							UnitarySystem( UnitarySysNum ).ActualDXCoilIndexForHXAssisted = DXCoilIndex;
							SetDXCoolingCoilData( DXCoilIndex, ErrorsFound, HeatingCoilPLFCurveIndex );
						}

					} // IF (IsNotOK) THEN
				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else {
						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilIndex = GetCoilIndexVariableSpeed( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						CoolingCoilInletNode = GetCoilInletNodeVariableSpeed( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						CoolingCoilOutletNode = GetCoilOutletNodeVariableSpeed( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetVSCoilCondenserInletNode( CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = ScheduleAlwaysOn;

						UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = GetVSCoilNumOfSpeeds( CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						errFlag = false;
						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetCoilCapacityVariableSpeed( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetCoilAirFlowRateVariableSpeed( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

					}

					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
							UnitarySystem( UnitarySysNum ).HeatPump = true;
						}
					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling ) {
					errFlag = false;
					GetDXCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, errFlag, CoolingCoilType );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr( CoolingCoilType, CoolingCoilName, errFlag );

					errFlag = false;
					CoolingCoilInletNode = GetDXCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					errFlag = false;
					CoolingCoilOutletNode = GetDXCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
						errFlag = false;
					}

					errFlag = false;
					UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetDXCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).DesignCoolingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					// Get DX coil air flow rate. Later fields will overwrite this IF input field is present
					errFlag = false;
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetDXCoilAirFlow( CoolingCoilType, CoolingCoilName, errFlag );
					if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}

					if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
							UnitarySystem( UnitarySysNum ).HeatPump = true;
						}
					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {

					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Cooling coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = GetWaterCoilAvailScheduleIndex( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).CoolingCoilIndex = GetWaterCoilIndex( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).CoolingCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( iCoolingCoilNameAlphaNum ) + " = " + HeatingCoilName );
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// call for air flow rate not valid for other water coil types
						if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater ) {
							UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetWaterCoilDesAirFlow( CoolingCoilType, CoolingCoilName, errFlag );
							if ( errFlag ) {
								ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
								ErrorsFound = true;
								errFlag = false;
							}
						}

						// Get the Cooling Coil water Inlet Node number
						UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode = GetCoilWaterInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						InletNodeNotControlled = true;
						//  CALL CheckCoilWaterInletNode(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode,InletNodeNotControlled)
						if ( ! InletNodeNotControlled ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( ControllerTypes( ControllerSimple_Type ) + " found for " + CoolingCoilType + " = \"" + CoolingCoilName + ".\"" );
							ShowContinueError( "...water coil controllers are not used with " + UnitarySystem( UnitarySysNum ).UnitarySystemType );
							ErrorsFound = true;
						}

						// Get the Cooling Coil chilled water max volume flow rate
						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow = GetCoilMaxWaterFlowRate( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Inlet Node
						CoolingCoilInletNode = GetWaterCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Outlet Node
						CoolingCoilOutletNode = GetWaterCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}
					}
				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Cooling coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = ScheduleAlwaysOn;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).CoolingCoilIndex = GetWtoAHPSimpleCoilIndex( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).CoolingCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( iCoolingCoilNameAlphaNum ) + " = " + CoolingCoilName );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetWtoAHPSimpleCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get DX coil air flow rate. Later fields will overwrite this IF input field is present
						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = GetWtoAHPSimpleCoilAirFlowRate( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Cooling Coil Inlet Node
						errFlag = false;
						CoolingCoilInletNode = GetWtoAHPSimpleCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Outlet Node
						CoolingCoilOutletNode = GetWtoAHPSimpleCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

					}

					if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
							UnitarySystem( UnitarySysNum ).HeatPump = true;
						}
					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHP ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Cooling coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = ScheduleAlwaysOn;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).CoolingCoilIndex = GetWtoAHPCoilIndex( CoolingCoilType, CoolingCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).CoolingCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( iCoolingCoilNameAlphaNum ) + " = " + CoolingCoilName );
							ErrorsFound = true;
							errFlag = false;
						}

						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetWtoAHPCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Inlet Node
						errFlag = false;
						CoolingCoilInletNode = GetWtoAHPCoilInletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Outlet Node
						CoolingCoilOutletNode = GetWtoAHPCoilOutletNode( CoolingCoilType, CoolingCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

					}

					if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical ) {
							UnitarySystem( UnitarySysNum ).HeatPump = true;
						}
					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_UserDefined ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Cooling coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = ScheduleAlwaysOn;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						GetUserDefinedCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, errFlag, CurrentModuleObject );
						if ( UnitarySystem( UnitarySysNum ).CoolingCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( iCoolingCoilNameAlphaNum ) + " = " + CoolingCoilName );
							ErrorsFound = true;
							errFlag = false;
						}

// **** How to get this info ****
//						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity = GetWtoAHPCoilCapacity( CoolingCoilType, CoolingCoilName, errFlag );
//						if ( errFlag ) {
//							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
//							ErrorsFound = true;
//							errFlag = false;
//						}

						// Get the Cooling Coil Inlet Node
						errFlag = false;
						GetUserDefinedCoilAirInletNode( CoolingCoilName, CoolingCoilInletNode, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Outlet Node
						GetUserDefinedCoilAirOutletNode( CoolingCoilName, CoolingCoilOutletNode, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

					}

				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_PackagedThermalStorageCooling ) {
					ValidateComponent( CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Cooling coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr = ScheduleAlwaysOn;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						GetTESCoilIndex( CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, errFlag, CurrentModuleObject );
						if ( UnitarySystem( UnitarySysNum ).CoolingCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( iCoolingCoilNameAlphaNum ) + " = " + CoolingCoilName );
							ErrorsFound = true;
							errFlag = false;
						}

						GetTESCoilCoolingAirFlowRate( CoolingCoilName, UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						GetTESCoilCoolingCapacity( CoolingCoilName, UnitarySystem( UnitarySysNum ).DesignCoolingCapacity, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Inlet Node
						errFlag = false;
						GetTESCoilAirInletNode( CoolingCoilName, CoolingCoilInletNode, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the Cooling Coil Outlet Node
						GetTESCoilAirOutletNode( CoolingCoilName, CoolingCoilOutletNode, errFlag, CurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

					}

				} else { // IF(.NOT. lAlphaBlanks(16))THEN
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iCoolingCoilTypeAlphaNum ) + " = " + Alphas( iCoolingCoilTypeAlphaNum ) );
					ErrorsFound = true;
				}

				if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling ) {
					UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil = true;
				} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed ) {
					UnitarySystem( UnitarySysNum ).VarSpeedCoolingCoil = true;
				}

				if ( NodeHasSPMCtrlVarType( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum, iCtrlVarType_Temp ) ) UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum = UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum;
				if ( NodeHasSPMCtrlVarType( CoolingCoilOutletNode, iCtrlVarType_Temp ) ) UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum = CoolingCoilOutletNode;

				UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum = CoolingCoilInletNode;
				UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum = CoolingCoilOutletNode;
				UnitarySystem( UnitarySysNum ).CoolingCoilName = CoolingCoilName;

			}

			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple && UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple ) {
				UnitarySystem( UnitarySysNum ).WaterCyclingMode = WaterCycling;
				SetSimpleWSHPData( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ErrorsFound, UnitarySystem( UnitarySysNum ).WaterCyclingMode, _, UnitarySystem( UnitarySysNum ).HeatingCoilIndex );
			}

			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit && UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) {
				SetVarSpeedCoilData( UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ErrorsFound, _, UnitarySystem( UnitarySysNum ).HeatingCoilIndex );
			}

			// Add cooling coil to component sets array
			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num != CoilDX_MultiSpeedCooling ) {
					SetUpCompSets( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iCoolingCoilTypeAlphaNum ), Alphas( iCoolingCoilNameAlphaNum ), NodeID( CoolingCoilInletNode ), NodeID( CoolingCoilOutletNode ) );
				} else {
					SetUpCompSets( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iCoolingCoilTypeAlphaNum ), Alphas( iCoolingCoilNameAlphaNum ), "UNDEFINED", "UNDEFINED" );
				}
			}
			// Run as 100% DOAS DX coil
			if ( lAlphaBlanks( iDOASDXCoilAlphaNum ) && NumAlphas < iDOASDXCoilAlphaNum ) {
				UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil = false;
			} else {
				if ( SameString( Alphas( iDOASDXCoilAlphaNum ), "Yes" ) ) {
					UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil = true;
				} else if ( SameString( Alphas( iDOASDXCoilAlphaNum ), "" ) ) {
					UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil = false;
				} else if ( SameString( Alphas( iDOASDXCoilAlphaNum ), "No" ) ) {
					UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil = false;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Invalid entry for " + cAlphaFields( iDOASDXCoilAlphaNum ) + " :" + Alphas( iDOASDXCoilAlphaNum ) );
					ShowContinueError( "Must be Yes or No." );
				}
			}

			// considered as as 100% DOAS DX cooling coil
			if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil ) {
				// set the system DX Coil application type to the child DX coil
				SetDXCoilTypeData( UnitarySystem( UnitarySysNum ).CoolingCoilName );
			}
			// DOAS DX Cooling Coil Leaving Minimum Air Temperature
			if ( NumNumbers > 0 ) {
				if ( ! lNumericBlanks( iDOASDXMinTempNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).DOASDXCoolingCoilMinTout = Numbers( iDOASDXMinTempNumericNum );
				}
			}

			//Get Latent Load Control flag
			if ( ! lAlphaBlanks( iRunOnLatentLoadAlphaNum ) ) {
				if ( SameString( Alphas( iRunOnLatentLoadAlphaNum ), "SensibleOnlyLoadControl" ) ) {
					UnitarySystem( UnitarySysNum ).RunOnSensibleLoad = true;
					UnitarySystem( UnitarySysNum ).RunOnLatentLoad = false;
				} else if ( SameString( Alphas( iRunOnLatentLoadAlphaNum ), "LatentOnlyLoadControl" ) ) {
					UnitarySystem( UnitarySysNum ).RunOnSensibleLoad = false;
					UnitarySystem( UnitarySysNum ).RunOnLatentLoad = true;
				} else if ( SameString( Alphas( iRunOnLatentLoadAlphaNum ), "LatentOrSensibleLoadControl" ) ) {
					UnitarySystem( UnitarySysNum ).RunOnSensibleLoad = true;
					UnitarySystem( UnitarySysNum ).RunOnLatentLoad = true;
				} else if ( SameString( Alphas( iRunOnLatentLoadAlphaNum ), "LatentWithSensibleLoadControl" ) ) {
					UnitarySystem( UnitarySysNum ).RunOnSensibleLoad = true;
					UnitarySystem( UnitarySysNum ).RunOnLatentLoad = true;
					UnitarySystem( UnitarySysNum ).RunOnLatentOnlyWithSensible = true;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Invalid entry for " + cAlphaFields( iRunOnLatentLoadAlphaNum ) + " :" + Alphas( iRunOnLatentLoadAlphaNum ) );
					ShowContinueError( "Must be SensibleOnlyLoadControl, LatentOnlyLoadControl, LatentOrSensibleLoadControl, or LatentWithSensibleLoadControl." );
				}
			}

			//Get reheat coil data if humidistat is used
			SuppHeatCoilType = Alphas( iSuppHeatCoilTypeAlphaNum );
			SuppHeatCoilName = Alphas( iSuppHeatCoilNameAlphaNum );
			UnitarySystem( UnitarySysNum ).SuppHeatCoilName = SuppHeatCoilName;
			errFlag = false;

			if ( SameString( SuppHeatCoilType, "Coil:Heating:Water" ) ) {
				UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num = Coil_HeatingWater;
			} else if ( SameString( SuppHeatCoilType, "Coil:Heating:Steam" ) ) {
				UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num = Coil_HeatingSteam;
			} else if ( SameString( SuppHeatCoilType, "Coil:Heating:Gas" ) || SameString( SuppHeatCoilType, "Coil:Heating:Electric" ) || SameString( SuppHeatCoilType, "Coil:Heating:DesuperHeater" ) ) {
				UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num = GetHeatingCoilTypeNum( SuppHeatCoilType, SuppHeatCoilName, errFlag );
			} else if ( SameString( SuppHeatCoilType, "Coil:UserDefined" ) ) {
				UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num = Coil_UserDefined;
			}

			if ( ! lAlphaBlanks( iSuppHeatCoilTypeAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).SuppCoilExists = true;

				if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingGas || UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingElectric || UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingDesuperheater ) {

					UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num = GetHeatingCoilTypeNum( SuppHeatCoilType, SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else {

						ValidateComponent( SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;

						} else { // mine data from reheat coil

							// Get the heating coil index
							GetHeatingCoilIndex( SuppHeatCoilName, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, IsNotOK );
							if ( IsNotOK ) {
								ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
								ErrorsFound = true;
							}

							// Get the design supplemental heating capacity
							errFlag = false;
							UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity = GetHeatingCoilCapacity( SuppHeatCoilType, SuppHeatCoilName, errFlag );
							if ( UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

							if ( errFlag ) {
								ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
								ErrorsFound = true;
							}

							// Get the Reheat Coil Inlet Node
							errFlag = false;
							SupHeatCoilInletNode = GetHeatingCoilInletNode( SuppHeatCoilType, SuppHeatCoilName, errFlag );
							if ( errFlag ) {
								ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
								ErrorsFound = true;
							}

							// Get the Reheat Coil Outlet Node
							errFlag = false;
							SupHeatCoilOutletNode = GetHeatingCoilOutletNode( SuppHeatCoilType, SuppHeatCoilName, errFlag );
							if ( errFlag ) {
								ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
								ErrorsFound = true;
							}

						} // IF (IsNotOK) THEN
					}

					UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode = SupHeatCoilInletNode;
					UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode = SupHeatCoilOutletNode;

				} else if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {

					ValidateComponent( SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from heating coil object

						// Get the Heating Coil water Inlet or control Node number
						errFlag = false;
						UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", SuppHeatCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the ReHeat Coil hot water max volume flow rate
						errFlag = false;
						UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", SuppHeatCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the ReHeat Coil Inlet Node
						errFlag = false;
						SupHeatCoilInletNode = GetWaterCoilInletNode( "Coil:Heating:Water", SuppHeatCoilName, errFlag );
						UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode = SupHeatCoilInletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the ReHeat Coil Outlet Node
						errFlag = false;
						SupHeatCoilOutletNode = GetWaterCoilOutletNode( "Coil:Heating:Water", SuppHeatCoilName, errFlag );
						UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

					}

				} else if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {

					ValidateComponent( SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from heating coil object

						errFlag = false;
						UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex = GetSteamCoilIndex( "COIL:HEATING:STEAM", SuppHeatCoilName, errFlag );
						if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowSevereError( "Illegal " + cAlphaFields( iSuppHeatCoilNameAlphaNum ) + " = " + SuppHeatCoilName );
							ErrorsFound = true;
						}

						// Get the Heating Coil steam inlet node number
						errFlag = false;
						UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", SuppHeatCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Heating Coil steam max volume flow rate
						UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, errFlag );
						if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

						if ( UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow > 0.0 ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getAirLoopHVACHeatCoolInput );
							UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, errFlag ) * SteamDensity;
						}

						// Get the Heating Coil Inlet Node
						errFlag = false;
						SupHeatCoilInletNode = GetSteamCoilAirInletNode( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, SuppHeatCoilName, errFlag );
						UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode = SupHeatCoilInletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

						// Get the Heating Coil Outlet Node
						errFlag = false;
						SupHeatCoilOutletNode = GetCoilAirOutletNode( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, SuppHeatCoilName, errFlag );
						UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
						}

					}

				} else if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_UserDefined ) {
					ValidateComponent( SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					} else { // mine data from Heating coil object

//						errFlag = false;
//						UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr = ScheduleAlwaysOn;
//						if ( errFlag ) {
//							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
//							ErrorsFound = true;
//							errFlag = false;
//						}

						errFlag = false;
						GetUserDefinedCoilIndex( SuppHeatCoilName, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, errFlag, CurrentModuleObject );
						if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( iSuppHeatCoilNameAlphaNum ) + " = " + SuppHeatCoilName );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the supplemental heating Coil Inlet Node
						errFlag = false;
						GetUserDefinedCoilAirInletNode( SuppHeatCoilName, SupHeatCoilInletNode, errFlag, CurrentModuleObject );
						UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode = SupHeatCoilInletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

						// Get the supplemenatal heating Coil Outlet Node
						GetUserDefinedCoilAirOutletNode( SuppHeatCoilName, SupHeatCoilOutletNode, errFlag, CurrentModuleObject );
						UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ErrorsFound = true;
							errFlag = false;
						}

					}

				} else { // Illegal reheating coil type
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iSuppHeatCoilTypeAlphaNum ) + " = " + Alphas( iSuppHeatCoilTypeAlphaNum ) );
					ErrorsFound = true;
				} // IF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingGas .OR. &, etc.

			} // IF(.NOT. lAlphaBlanks(iSuppHeatCoilTypeAlphaNum))THEN

			if ( NodeHasSPMCtrlVarType( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum, iCtrlVarType_Temp ) ) UnitarySystem( UnitarySysNum ).SuppHeatControlNodeNum = UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum;
			if ( NodeHasSPMCtrlVarType( SupHeatCoilOutletNode, iCtrlVarType_Temp ) ) UnitarySystem( UnitarySysNum ).SuppHeatControlNodeNum = SupHeatCoilOutletNode;

			// Add supplemental heating coil to component sets array
			if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) SetUpCompSets( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iSuppHeatCoilTypeAlphaNum ), Alphas( iSuppHeatCoilNameAlphaNum ), NodeID( SupHeatCoilInletNode ), NodeID( SupHeatCoilOutletNode ) );

			TotalZonesOnAirLoop = 0;
			TotalFloorAreaOnAirLoop = 0.0;
			AirLoopNumber = 0;

			AirNodeFound = false;
			AirLoopFound = false;
			OASysFound = false;
			ZoneEquipmentFound = false;
			ZoneInletNodeFound = false;

			// check if the UnitarySystem is connected to an air loop
			for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
				for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						if ( SameString( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name, Alphas( iNameAlphaNum ) ) && SameString( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf, CurrentModuleObject ) ) {
							AirLoopNumber = AirLoopNum;
							AirLoopFound = true;
							for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
								if ( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum != UnitarySystem( UnitarySysNum ).ControlZoneNum ) continue;
								//             Find the controlled zone number for the specified thermostat location
								UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
								UnitarySystem( UnitarySysNum ).ControlZoneNum = ControlledZoneNum;
								//             Determine if system is on air loop served by the thermostat location specified
								if ( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum == AirLoopNumber ) {
									++TotalZonesOnAirLoop;
									TotalFloorAreaOnAirLoop += Zone( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum ).FloorArea;
								}
								for ( TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum ) {
									if ( TempControlledZone( TstatZoneNum ).ActualZoneNum != UnitarySystem( UnitarySysNum ).ControlZoneNum ) continue;
									AirNodeFound = true;
								}
								for ( TstatZoneNum = 1; TstatZoneNum <= NumComfortControlledZones; ++TstatZoneNum ) {
									if ( ComfortControlledZone( TstatZoneNum ).ActualZoneNum != UnitarySystem( UnitarySysNum ).ControlZoneNum ) continue;
									AirNodeFound = true;
								}
							break;
							}
						}
					}
				}
			}

			// check if the UnitarySystem is connected to an outside air system
			if ( !AirLoopFound && CurOASysNum > 0 ) {
				for ( OASysNum = 1; OASysNum <= NumOASystems; ++OASysNum ) {
					for ( OACompNum = 1; OACompNum <= OutsideAirSys( OASysNum ).NumComponents; ++OACompNum ) {
						if ( ! SameString( OutsideAirSys( OASysNum ).ComponentName( OACompNum ), Alphas( iNameAlphaNum ) ) || ! SameString( OutsideAirSys( OASysNum ).ComponentType( OACompNum ), CurrentModuleObject ) ) continue;
						AirLoopNumber = OASysNum;
						OASysFound = true;
						break;
					}
				}
			}

			// check if the UnitarySystem is connected as zone equipment
			if ( !AirLoopFound && !OASysFound) {
				for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
					for ( ZoneExhNum = 1; ZoneExhNum <= ZoneEquipConfig( ControlledZoneNum ).NumExhaustNodes; ++ZoneExhNum ) {
						if ( ZoneEquipConfig( ControlledZoneNum ).ExhaustNode( ZoneExhNum ) != UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum ) continue;
						ZoneEquipmentFound = true;
						//               Find the controlled zone number for the specified thermostat location
						UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
						++TotalZonesOnAirLoop;
						TotalFloorAreaOnAirLoop = Zone( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum ).FloorArea;
						UnitarySystem( UnitarySysNum ).AirLoopEquipment = false;
						UnitarySystem( UnitarySysNum ).ZoneInletNode = ZoneEquipConfig( ControlledZoneNum ).ExhaustNode( ZoneExhNum );
						if ( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex > 0 ) {
							for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
								if ( ( ZoneEquipList( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex ).EquipType_Num( EquipNum ) != ZoneUnitarySystem_Num ) || ZoneEquipList( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex ).EquipName( EquipNum ) != UnitarySystem( UnitarySysNum ).Name ) continue;
								UnitarySystem( UnitarySysNum ).ZoneSequenceCoolingNum = ZoneEquipList( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
								UnitarySystem( UnitarySysNum ).ZoneSequenceHeatingNum = ZoneEquipList( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
							}
						}
						UnitarySystem( UnitarySysNum ).ControlZoneNum = ControlledZoneNum;
						break;
					}
					if ( ZoneEquipmentFound ) {
						for ( ZoneInletNum = 1; ZoneInletNum <= ZoneEquipConfig( ControlledZoneNum ).NumInletNodes; ++ZoneInletNum ) {
							if ( ZoneEquipConfig( ControlledZoneNum ).InletNode( ZoneInletNum ) != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) continue;
								ZoneInletNodeFound = true;
							break;
						}
					}
				}
				if ( ! ZoneInletNodeFound ) {
					for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
						for ( ZoneInletNum = 1; ZoneInletNum <= ZoneEquipConfig( ControlledZoneNum ).NumInletNodes; ++ZoneInletNum ) {
							if ( ZoneEquipConfig( ControlledZoneNum ).InletNode( ZoneInletNum ) != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) continue;
								ZoneInletNodeFound = true;
								ZoneEquipmentFound = true;
							break;
						}
					}
					if ( ! ZoneInletNodeFound ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirOutletNodeNameAlphaNum ) + " = " + Alphas( iAirOutletNodeNameAlphaNum ) );
						ShowContinueError( "Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections object inputs." );
						ErrorsFound = true;
					}
				}
				if ( ! ZoneEquipmentFound ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas( iAirInletNodeNameAlphaNum ) );
					ShowContinueError( "Node name does not match any controlled zone exhaust node name. Check ZoneHVAC:EquipmentConnections object inputs." );
					ErrorsFound = true;
				}
			}

			if ( AirLoopNumber == 0 && ! ZoneEquipmentFound && UnitarySystem( UnitarySysNum ).ControlType == LoadBased ) {
				ShowSevereError( CurrentModuleObject + " = " + Alphas( iNameAlphaNum ) );
				ShowContinueError( "Did not find proper connection for AirLoopHVAC or ZoneHVAC system." );
				ShowContinueError( "specified " + cAlphaFields( iControlZoneAlphaNum ) + " = " + Alphas( iControlZoneAlphaNum ) );
				if ( ! AirNodeFound && ! ZoneEquipmentFound ) {
					ShowSevereError( CurrentModuleObject + " = " + Alphas( iNameAlphaNum ) );
					ShowContinueError( "Did not find air node (zone with thermostat)." );
					ShowContinueError( "specified " + cAlphaFields( iControlZoneAlphaNum ) + " = " + Alphas( iControlZoneAlphaNum ) );
					ShowContinueError( "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone." );
				}
				ErrorsFound = true;
			}

			if ( ! ZoneEquipmentFound ) TestCompSet( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iAirInletNodeNameAlphaNum ), Alphas( iAirOutletNodeNameAlphaNum ), "Air Nodes" );

			// Users may not provide SA flow input fields (below) and leave them blank. Check of other coil is autosized first to alieviate input requirements.
			// check if coil has no air flow input (VolFlow = 0) and other coil is autosized. If so, use autosize for coil with 0 air flow rate.
			// This means that the coils MUST mine the air flow rate if it exists
			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == 0 && lAlphaBlanks( iHeatSAFMAlphaNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = AutoSize;
				} else if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == 0 && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize && lAlphaBlanks( iCoolSAFMAlphaNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = AutoSize;
				}
			}

			// Determine supply air flow rate sizing method for cooling mode
			if ( SameString( Alphas( iCoolSAFMAlphaNum ), "SupplyAirFlowRate" ) ) {
				UnitarySystem( UnitarySysNum ).CoolingSAFMethod = SupplyAirFlowRate;

				if ( ! lNumericBlanks( iMaxCoolAirVolFlowNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = Numbers( iMaxCoolAirVolFlowNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

					if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cNumericFields( iMaxCoolAirVolFlowNumericNum ) + " = " + TrimSigDigits( Numbers( iMaxCoolAirVolFlowNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iMaxCoolAirVolFlowNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "FlowPerFloorArea" ) ) {

				UnitarySystem( UnitarySysNum ).CoolingSAFMethod = FlowPerFloorArea;
				if ( ! lNumericBlanks( iCoolFlowPerFloorAreaNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = Numbers( iCoolFlowPerFloorAreaNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFlowPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow *= TotalFloorAreaOnAirLoop;
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFlowPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "FractionOfAutosizedCoolingValue" ) ) {

				UnitarySystem( UnitarySysNum ).CoolingSAFMethod = FractionOfAutoSizedCoolingValue;
				if ( ! lNumericBlanks( iCoolFlowPerFracCoolNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = Numbers( iCoolFlowPerFracCoolNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFracCoolNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFlowPerFracCoolNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFracCoolNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFlowPerFracCoolNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "FlowPerCoolingCapacity" ) ) {

				UnitarySystem( UnitarySysNum ).CoolingSAFMethod = FlowPerCoolingCapacity;
				if ( ! lNumericBlanks( iCoolFlowPerCoolCapNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = Numbers( iCoolFlowPerCoolCapNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerCoolCapNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFlowPerCoolCapNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerCoolCapNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFlowPerCoolCapNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "None" ) || lAlphaBlanks( iCoolSAFMAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).CoolingSAFMethod = None;
				//          UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE. ! ??
				if( UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " is blank and relates to " + CoolingCoilType + " = " + CoolingCoilName );
					if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						ShowContinueError( "Blank field not allowed for this coil type when heating coil air flow rate is not autosized." );
					} else {
						ShowContinueError( "Blank field not allowed for this type of cooling coil." );
					}
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
				ShowContinueError( "Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, FlowPerCoolingCapacity, or None " );
				ErrorsFound = true;
			}

			// Determine supply air flow rate sizing method for heating mode
			if ( SameString( Alphas( iHeatSAFMAlphaNum ), "SupplyAirFlowRate" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingSAFMethod = SupplyAirFlowRate;
				if ( ! lNumericBlanks( iMaxHeatAirVolFlowNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = Numbers( iMaxHeatAirVolFlowNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cNumericFields( iMaxHeatAirVolFlowNumericNum ) + " = " + TrimSigDigits( Numbers( iMaxHeatAirVolFlowNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iMaxHeatAirVolFlowNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "FlowPerFloorArea" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingSAFMethod = FlowPerFloorArea;
				if ( ! lNumericBlanks( iHeatFlowPerFloorAreaNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = Numbers( iHeatFlowPerFloorAreaNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFlowPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFlowPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "FractionOfAutosizedHeatingValue" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingSAFMethod = FractionOfAutoSizedHeatingValue;
				if ( ! lNumericBlanks( iHeatFlowPerFracCoolNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = Numbers( iHeatFlowPerFracCoolNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFracCoolNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFlowPerFracCoolNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFracCoolNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFlowPerFracCoolNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "FlowPerHeatingCapacity" ) ) {
				UnitarySystem( UnitarySysNum ).HeatingSAFMethod = FlowPerHeatingCapacity;
				if ( ! lNumericBlanks( iHeatFlowPerHeatCapNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = Numbers( iHeatFlowPerHeatCapNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow <= 0.0 && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerHeatCapNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFlowPerHeatCapNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerHeatCapNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFlowPerHeatCapNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "None" ) || lAlphaBlanks( iHeatSAFMAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).HeatingSAFMethod = None;
				//          UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE. ! ??
				if( UnitarySystem( UnitarySysNum ).HeatCoilExists && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " is blank and relates to " + HeatingCoilType + " = " + HeatingCoilName );
					if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
						ShowContinueError( "Blank field not allowed for this coil type when cooling coil air flow rate is not autosized." );
					} else {
						ShowContinueError( "Blank field not allowed for this type of heating coil." );
					}
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
				ShowContinueError( "Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedHeatingValue, FlowPerHeatingCapacity, or None " );
				ErrorsFound = true;
			}

			// Determine supply air flow rate sizing method when cooling or heating is not needed
			if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "SupplyAirFlowRate" ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = SupplyAirFlowRate;
				if ( ! lNumericBlanks( iMaxNoCoolHeatAirVolFlowNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = Numbers( iMaxNoCoolHeatAirVolFlowNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;

					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow < 0.0 && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Illegal " + cNumericFields( iMaxNoCoolHeatAirVolFlowNumericNum ) + " = " + TrimSigDigits( Numbers( iMaxNoCoolHeatAirVolFlowNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iMaxNoCoolHeatAirVolFlowNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FlowPerFloorArea" ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = FlowPerFloorArea;
				if ( ! lNumericBlanks( iNoCoolHeatFlowPerFloorAreaNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerFloorAreaNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow < 0.0 && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FractionOfAutosizedCoolingValue" ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = FractionOfAutoSizedCoolingValue;
				if ( ! lNumericBlanks( iNoCoolHeatFlowPerFracCoolNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerFracCoolNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow < 0.0 && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracCoolNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerFracCoolNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracCoolNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerFracCoolNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FractionOfAutosizedHeatingValue" ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = FractionOfAutoSizedHeatingValue;
				if ( ! lNumericBlanks( iNoCoolHeatFlowPerFracHeatNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerFracHeatNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow < 0.0 && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracHeatNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerFracHeatNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracHeatNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerFracHeatNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FlowPerCoolingCapacity" ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = FlowPerCoolingCapacity;
				if ( ! lNumericBlanks( iNoCoolHeatFlowPerCoolCapNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerCoolCapNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow < 0.0 && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerCoolCapNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerCoolCapNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerCoolCapNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerCoolCapNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FlowPerHeatingCapacity" ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = FlowPerHeatingCapacity;
				if ( ! lNumericBlanks( iNoCoolHeatFlowPerHeatCapNumericNum ) ) {
					UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerHeatCapNumericNum );
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow < 0.0 && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerHeatCapNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerHeatCapNumericNum ), 7 ) );
						ErrorsFound = true;
						// Autosized input is not allowed
					} else if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerHeatCapNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					} else {
						UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerHeatCapNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "None" ) || lAlphaBlanks( iNoCoolHeatSAFMAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).NoCoolHeatSAFMethod = None;
				//          UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE. ! ??
			} else {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
				ShowContinueError( "Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, FractionOfAutosizedHeatingValue, FlowPerCoolingCapacity, FlowPerHeatingCapacity, or None " );
				ErrorsFound = true;
			}

			//       Fan operating mode (cycling or constant) schedule. IF constant fan, then set AirFlowControl
			if ( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr > 0 ) {
				if ( ! CheckScheduleValueMinMax( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr, ">=", 0.0, "<=", 0.0 ) ) {
					//           set fan operating mode to continuous so sizing can set VS coil data
					UnitarySystem( UnitarySysNum ).FanOpMode = ContFanCycCoil;
					//           set air flow control mode:
					//             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
					//             UseCompressorOffFlow = operate at value specified by user
					//           AirFlowControl only valid if fan opmode = ContFanCycComp
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
						UnitarySystem( UnitarySysNum ).AirFlowControl = UseCompressorOnFlow;
					} else {
						UnitarySystem( UnitarySysNum ).AirFlowControl = UseCompressorOffFlow;
					}
				}
			}

			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num != CoilDX_CoolingHXAssisted && UnitarySystem( UnitarySysNum ).CoolingCoilType_Num != CoilDX_CoolingTwoStageWHumControl && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iDehumidControlAlphaNum ) + " = " + Alphas( iDehumidControlAlphaNum ) );
				ShowContinueError( "Multimode control must be used with a Heat Exchanger Assisted or Multimode Cooling Coil." );
				if ( lAlphaBlanks( iSuppHeatCoilNameAlphaNum ) ) {
				} else {
					if ( UnitarySystem(UnitarySysNum).CoolingCoilType_Num == Coil_UserDefined ) {
						ShowContinueError( "Dehumidification control type is assumed to be None and the simulation continues." );
						UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DehumidControl_None;
					} else {
						ShowContinueError( "Dehumidification control type is assumed to be CoolReheat and the simulation continues." );
						UnitarySystem( UnitarySysNum ).DehumidControlType_Num = DehumidControl_CoolReheat;
					}
				}
			}

			//       Check placement of cooling coil with respect to fan placement and dehumidification control type

			if ( UnitarySystem( UnitarySysNum ).FanExists ) {
				if ( UnitarySystem( UnitarySysNum ).FanPlace == BlowThru ) {
					if ( FanOutletNode == HeatingCoilInletNode && UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_CoolReheat ) {
						UnitarySystem( UnitarySysNum ).CoolingCoilUpstream = false;
					}
				} else if ( UnitarySystem( UnitarySysNum ).FanPlace == DrawThru ) {
					if ( HeatingCoilOutletNode == CoolingCoilInletNode && UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_CoolReheat ) {
						UnitarySystem( UnitarySysNum ).CoolingCoilUpstream = false;
					}
				}
			} else {
				if ( HeatingCoilOutletNode == CoolingCoilInletNode && UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_CoolReheat ) {
					UnitarySystem( UnitarySysNum ).CoolingCoilUpstream = false;
				}
				if ( ZoneEquipmentFound ) {
					ShowSevereError( CurrentModuleObject + " = " + Alphas( iNameAlphaNum ) );
					ShowContinueError( "ZoneHVAC equipment must contain a fan object." );
					ShowContinueError( "specified " + cAlphaFields( iFanTypeAlphaNum ) + " = " + Alphas( iFanTypeAlphaNum ) );
					ShowContinueError( "specified " + cAlphaFields( iFanNameAlphaNum ) + " = " + Alphas( iFanNameAlphaNum ) );
					ErrorsFound = true;
				}
			}

			// check node connections
			if ( UnitarySystem( UnitarySysNum ).FanPlace == BlowThru ) {

				if ( FanInletNode != UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "When a blow through fan is specified, the fan inlet node name must be the same as the unitary system inlet node name." );
					ShowContinueError( "...Fan inlet node name           = " + NodeID( FanInletNode ) );
					ShowContinueError( "...UnitarySystem inlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( UnitarySystem( UnitarySysNum ).CoolingCoilUpstream ) {
					if ( FanOutletNode != CoolingCoilInletNode && UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil inlet node name." );
						ShowContinueError( "...Fan outlet node name         = " + NodeID( FanOutletNode ) );
						ShowContinueError( "...Cooling coil inlet node name = " + NodeID( CoolingCoilInletNode ) );
						ErrorsFound = true;
					}
					if ( CoolingCoilOutletNode != HeatingCoilInletNode && UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "The cooling coil outlet node name must be the same as the heating coil inlet node name." );
						ShowContinueError( "...Cooling coil outlet node name = " + NodeID( CoolingCoilOutletNode ) );
						ShowContinueError( "...Heating coil inlet node name  = " + NodeID( HeatingCoilInletNode ) );
						ErrorsFound = true;
					}
					if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
						if ( SupHeatCoilOutletNode != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "The reheat coil outlet node name must be the same as the unitary system outlet node name." );
							ShowContinueError( "...Reheat coil outlet node name   = " + NodeID( SupHeatCoilOutletNode ) );
							ShowContinueError( "...UnitarySystem outlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) );
							//                ErrorsFound=.TRUE.
						}
					} else { // IF((UnitarySystem(UnitarySysNum)%Humidistat ...
						// Heating coil outlet node name must be the same as the Unitary system outlet node name
						if ( UnitarySystem( UnitarySysNum ).HeatCoilExists && HeatingCoilOutletNode != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "When a blow through fan is specified, the heating coil outlet node name must be the same as the unitary system outlet node name." );
							ShowContinueError( "...Heating coil outlet node name  = " + NodeID( HeatingCoilOutletNode ) );
							ShowContinueError( "...Unitary system outlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) );
							ErrorsFound = true;
						}
					}
				} else { // IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN
					if ( FanOutletNode != HeatingCoilInletNode && UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a blow through fan is specified, the fan outlet node name must be the same as the heating coil inlet node name." );
						ShowContinueError( "...Fan outlet node name         = " + NodeID( FanOutletNode ) );
						ShowContinueError( "...Heating coil inlet node name = " + NodeID( HeatingCoilInletNode ) );
						ErrorsFound = true;
					}
					if ( HeatingCoilOutletNode != CoolingCoilInletNode && UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "The heating coil outlet node name must be the same as the cooling coil inlet node name." );
						ShowContinueError( "...Heating coil outlet node name = " + NodeID( HeatingCoilOutletNode ) );
						ShowContinueError( "...Cooling coil inlet node name  = " + NodeID( CoolingCoilInletNode ) );
						ErrorsFound = true;
					}
					if ( CoolingCoilOutletNode != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum && UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a blow through fan is specified, the cooling coil outlet node name must be the same as the unitary system outlet node name." );
						ShowContinueError( "...Cooling coil outlet node name   = " + NodeID( CoolingCoilOutletNode ) );
						ShowContinueError( "...UnitarySystem outlet node name  = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) );
						ErrorsFound = true;
					}
				}

			} else { // ELSE from IF(UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru)THEN

				if ( UnitarySystem( UnitarySysNum ).CoolingCoilUpstream ) {
					if ( CoolingCoilInletNode != UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum && CoolingCoilInletNode != 0 && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a draw through fan is specified, the cooling coil inlet node name must be the same as the unitary system inlet node name." );
						ShowContinueError( "...Cooling coil inlet node name  = " + NodeID( CoolingCoilInletNode ) );
						ShowContinueError( "...UnitarySystem inlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum ) );
						ErrorsFound = true;
					}
					if ( CoolingCoilOutletNode != HeatingCoilInletNode && UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "The cooling coil outlet node name must be the same as the heating coil inlet node name." );
						ShowContinueError( "...Cooling coil outlet node name = " + NodeID( CoolingCoilOutletNode ) );
						ShowContinueError( "...Heating coil inlet node name  = " + NodeID( HeatingCoilInletNode ) );
						ErrorsFound = true;
					}
					if ( HeatingCoilOutletNode != FanInletNode && UnitarySystem( UnitarySysNum ).HeatCoilExists && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a draw through fan is specified, the heating coil outlet node name must be the same as the fan inlet node name." );
						ShowContinueError( "...Heating coil outlet node name = " + NodeID( HeatingCoilOutletNode ) );
						ShowContinueError( "...Fan inlet node name           = " + NodeID( FanInletNode ) );
						ErrorsFound = true;
					}
					if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
						if ( FanOutletNode != SupHeatCoilInletNode && UnitarySystem( UnitarySysNum ).FanExists ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil inlet node name." );
							ShowContinueError( "...Fan outlet node name        = " + NodeID( FanOutletNode ) );
							ShowContinueError( "...Reheat coil inlet node name = " + NodeID( SupHeatCoilInletNode ) );
							//                ErrorsFound=.TRUE.
						}
						if ( SupHeatCoilOutletNode != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "The reheat coil outlet node name must be the same as the unitary system outlet node name." );
							ShowContinueError( "...Reheat coil outlet node name   = " + NodeID( SupHeatCoilOutletNode ) );
							ShowContinueError( "...UnitarySystem outlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) );
						}
					} else {
						if ( FanOutletNode != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum && UnitarySystem( UnitarySysNum ).FanExists ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "When a draw through fan is specified, the fan outlet node name must be the same as the unitary system outlet node name." );
							ShowContinueError( "...Fan outlet node name        = " + NodeID( FanOutletNode ) );
							ShowContinueError( "...Unitary system outlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) );
							ErrorsFound = true;
						}
					}
				} else { // IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN
					if ( HeatingCoilInletNode != UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum && HeatingCoilInletNode != 0 && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a draw through fan is specified, the heating coil inlet node name must be the same as the unitary system inlet node name." );
						ShowContinueError( "...Heating coil inlet node name  = " + NodeID( HeatingCoilInletNode ) );
						ShowContinueError( "...UnitarySystem inlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum ) );
						ErrorsFound = true;
					}
					if ( HeatingCoilOutletNode != CoolingCoilInletNode && UnitarySystem( UnitarySysNum ).HeatCoilExists && UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "The heating coil outlet node name must be the same as the cooling coil inlet node name." );
						ShowContinueError( "...Heating coil outlet node name = " + NodeID( HeatingCoilOutletNode ) );
						ShowContinueError( "...Cooling coil inlet node name  = " + NodeID( CoolingCoilInletNode ) );
						ErrorsFound = true;
					}
					if ( CoolingCoilOutletNode != FanInletNode && UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a draw through fan is specified, the cooling coil outlet node name must be the same as the fan inlet node name." );
						ShowContinueError( "...Cooling coil outlet node name = " + NodeID( CoolingCoilOutletNode ) );
						ShowContinueError( "...Fan inlet node name           = " + NodeID( FanInletNode ) );
						ErrorsFound = true;
					}
					if ( FanOutletNode != UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum && UnitarySystem( UnitarySysNum ).FanExists ) {
						ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "When a draw through fan is specified, the fan outlet node name must be the same as the unitary system outlet node name." );
						ShowContinueError( "...Fan outlet node name           = " + NodeID( FanOutletNode ) );
						ShowContinueError( "...UnitarySystem outlet node name = " + NodeID( UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum ) );
						ErrorsFound = true;
					}
				}
			} // ELSE from IF(UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru)THEN

			//Set the unitary system supplemental heater max outlet temperature
			// this field will be 0 if the input is not specified (included) in the input file
			// someone may use a default other than what we intended, allow it to be used
			// so if this field is blank, and the input field is included, read the default, otherwise use 80
			if ( ! lNumericBlanks( iDesignMaxOutletTempNumericNum ) && NumNumbers > ( iDesignMaxOutletTempNumericNum - 1 ) ) {
				UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp = Numbers( iDesignMaxOutletTempNumericNum );
				if ( UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp == AutoSize ) UnitarySystem( UnitarySysNum ).RequestAutoSize = true;
			}

			//Set maximum Outdoor air temperature for supplemental heating coil operation
			// this field will be 0 if the input is not specified (included) in the input file
			// someone may use a default other than what we intended, allow it to be used
			// so if this field is blank, and the input field is included, read the default, otherwise use 9999
			if ( ! lNumericBlanks( iMaxOATSuppHeatNumericNum ) && NumNumbers > ( iMaxOATSuppHeatNumericNum - 1 ) ) {
				UnitarySystem( UnitarySysNum ).MaxOATSuppHeat = Numbers( iMaxOATSuppHeatNumericNum );
				// Can't let MaxOATSuppHeat default to 21C if using cool reheat since it would shut off supp heater when dehumidifying
				// this may also allow supplemental heater to operate when in heating mode when it should not
			} else if ( NumNumbers < iMaxOATSuppHeatNumericNum && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat ) {
				UnitarySystem( UnitarySysNum ).MaxOATSuppHeat = 999.0;
			}

			if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow != AutoSize && UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow != AutoSize && ! UnitarySystem( UnitarySysNum ).RequestAutoSize ) {
				//           (UnitarySystem(UnitarySysNum)%CoolingSAFMethod .LE. SupplyAirFlowRate .OR. &
				//            UnitarySystem(UnitarySysNum)%HeatingSAFMethod .LE. SupplyAirFlowRate))THEN
				UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = max( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow, UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow );
			} else {
				if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate == 0.0) {
					UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = AutoSize;
				}
				// need more of this type of warning when flow cannot be determined
				if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == 0.0 && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
					if ( UnitarySystem( UnitarySysNum ).FanExists ) {
						if ( UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize ) {
							if ( UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow == 0.0 ) {
								UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate;
							}
						}
					} else if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
						UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow;
					} else {
						if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != CoilDX_HeatingEmpirical && UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != CoilDX_MultiSpeedHeating && UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != Coil_HeatingAirToAirVariableSpeed ) {
							ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "When non-DX heating coils are specified, the heating air flow rate must be entered in " + cAlphaFields( iHeatSAFMAlphaNum ) );
							ErrorsFound = true;
						}
					}
				} else if ( UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow == 0.0 && ! UnitarySystem( UnitarySysNum ).FanExists && ! UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "When non-DX heating coils are specified, the heating air flow rate must be entered in " + cAlphaFields( iHeatSAFMAlphaNum ) );
				}
			}

			if ( FanVolFlowRate != AutoSize && UnitarySystem( UnitarySysNum ).FanExists ) {
				if ( FanVolFlowRate < UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow && UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow != AutoSize && UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "... air flow rate = " + TrimSigDigits( FanVolFlowRate, 7 ) + " in fan object " + FanName + " is less than the maximum HVAC system air flow rate in cooling mode." );
					ShowContinueError( " The " + cNumericFields( iMaxCoolAirVolFlowNumericNum ) + " is reset to the fan flow rate and the simulation continues." );
					UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlow = FanVolFlowRate;
					UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = FanVolFlowRate;
				}
				if ( FanVolFlowRate < UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow && UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow != AutoSize && UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "... air flow rate = " + TrimSigDigits( FanVolFlowRate, 7 ) + " in fan object " + FanName + " is less than the maximum HVAC system air flow rate in heating mode." );
					ShowContinueError( " The " + cNumericFields( 3 ) + " is reset to the fan flow rate and the simulation continues." );
					UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlow = FanVolFlowRate;
					UnitarySystem( UnitarySysNum ).DesignFanVolFlowRate = FanVolFlowRate;
				}
			}

			if ( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr > 0 ) {
				if ( ! CheckScheduleValueMinMax( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr, ">=", 0.0, "<=", 0.0 ) ) {
					//           set air flow control mode:
					//             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
					//             UseCompressorOffFlow = operate at value specified by user
					//           AirFlowControl only valid IF fan opmode = ContFanCycComp
					if ( UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
						UnitarySystem( UnitarySysNum ).AirFlowControl = UseCompressorOnFlow;
					} else {
						UnitarySystem( UnitarySysNum ).AirFlowControl = UseCompressorOffFlow;
					}
				}
			}

			//Set minimum OAT for heat pump compressor operation
			// get from coil module
			errFlag = false;
			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) {
				UnitarySystem( UnitarySysNum ).MinOATCompressor = GetVSCoilMinOATCompressor( HeatingCoilName, errFlag );
			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating ) {
				UnitarySystem( UnitarySysNum ).MinOATCompressor = GetMinOATDXCoilCompressor( HeatingCoilType, HeatingCoilName, errFlag );
				//       ELSEIF  ***... make sure we catch all possbile coil types here ...***
			} else {
				UnitarySystem( UnitarySysNum ).MinOATCompressor = -1000.0;
			}
			if ( errFlag ) {
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ErrorsFound = true;
			}

			//       Mine heatpump Outdoor condenser node from DX coil object
			errFlag = false;
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed ) {
				UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( CoolingCoilType, CoolingCoilName, errFlag );
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed ) {
				UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetVSCoilCondenserInletNode( CoolingCoilName, errFlag );
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingHXAssisted ) {
				UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed", GetHXDXCoilName( CoolingCoilType, CoolingCoilName, errFlag ), errFlag );
			} else {
				if ( ! lAlphaBlanks( iCondenserNodeAlphaNum ) ) {
					UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetOnlySingleNode( Alphas( iCondenserNodeAlphaNum ), errFlag, CurrentModuleObject, Alphas( iNameAlphaNum ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
				} else {
					// do nothing?
				}
			}
			if ( errFlag ) {
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ErrorsFound = true;
			}

			//Set the heatpump cycling rate
			UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour = Numbers( iMaxONOFFCycPerHourNumericNum );
			if ( NumNumbers < iMaxONOFFCycPerHourNumericNum ) {
				UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour = 2.5;
			}

			//Set the heat pump time constant
			UnitarySystem( UnitarySysNum ).HPTimeConstant = Numbers( iHPTimeConstantNumericNum );
			if ( NumNumbers < iHPTimeConstantNumericNum ) {
				UnitarySystem( UnitarySysNum ).HPTimeConstant = 60.0;
			}

			//Set the heat pump on-cycle power use fraction
			UnitarySystem( UnitarySysNum ).OnCyclePowerFraction = Numbers( iOnCyclePowerFracNumericNum );
			if ( NumNumbers < iOnCyclePowerFracNumericNum ) {
				UnitarySystem( UnitarySysNum ).OnCyclePowerFraction = 0.01;
			}

			//Set the heat pump fan delay time
			UnitarySystem( UnitarySysNum ).FanDelayTime = Numbers( iFanDelayTimeNumericNum );
			if ( NumNumbers < iFanDelayTimeNumericNum ) {
				UnitarySystem( UnitarySysNum ).FanDelayTime = 60.0;
			}

			UnitarySystem( UnitarySysNum ).AncillaryOnPower = Numbers( iAncillaryOnPowerNumericNum );
			UnitarySystem( UnitarySysNum ).AncillaryOffPower = Numbers( iAncillaryOffPowerNumericNum );

			UnitarySystem( UnitarySysNum ).DesignHRWaterVolumeFlow = Numbers( iDesignHRWaterVolFlowNumericNum );
			UnitarySystem( UnitarySysNum ).MaxHROutletWaterTemp = Numbers( iMaxHROutletWaterTempNumericNum );

			if ( UnitarySystem( UnitarySysNum ).DesignHRWaterVolumeFlow > 0.0 ) {
				UnitarySystem( UnitarySysNum ).HeatRecActive = true;
				errFlag = false;
				if ( ! lAlphaBlanks( iHRWaterInletNodeAlphaNum ) && ! lAlphaBlanks( iHRWaterOutletNodeAlphaNum ) ) {
					UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum = GetOnlySingleNode( Alphas( iHRWaterInletNodeAlphaNum ), errFlag, CurrentModuleObject, Alphas( iNameAlphaNum ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
					UnitarySystem( UnitarySysNum ).HeatRecoveryOutletNodeNum = GetOnlySingleNode( Alphas( iHRWaterOutletNodeAlphaNum ), errFlag, CurrentModuleObject, Alphas( iNameAlphaNum ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );

					TestCompSet( CurrentModuleObject, Alphas( iNameAlphaNum ), Alphas( iHRWaterInletNodeAlphaNum ), Alphas( iHRWaterOutletNodeAlphaNum ), "Unitary System Heat Recovery Nodes" );

					if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling ) {
						SetMSHPDXCoilHeatRecoveryFlag( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
					}
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating ) {
						SetMSHPDXCoilHeatRecoveryFlag( UnitarySystem( UnitarySysNum ).HeatingCoilIndex );
					}
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iHRWaterInletNodeAlphaNum ) + " = " + Alphas( iHRWaterInletNodeAlphaNum ) );
					ShowContinueError( "Illegal " + cAlphaFields( iHRWaterOutletNodeAlphaNum ) + " = " + Alphas( iHRWaterOutletNodeAlphaNum ) );
					ShowContinueError( "... heat recovery nodes must be specified when " + cNumericFields( iDesignHRWaterVolFlowNumericNum ) + " is greater than 0." );
					ShowContinueError( "... " + cNumericFields( iDesignHRWaterVolFlowNumericNum ) + " = " + TrimSigDigits( UnitarySystem( UnitarySysNum ).DesignHRWaterVolumeFlow, 7 ) );
					ErrorsFound = true;
				}
			}

			if ( ! lAlphaBlanks( iDesignSpecMSHPTypeAlphaNum ) && ! lAlphaBlanks( iDesignSpecMSHPNameAlphaNum ) ) {
				UnitarySystem( UnitarySysNum ).DesignSpecMultispeedHPType = Alphas( iDesignSpecMSHPTypeAlphaNum );
				UnitarySystem( UnitarySysNum ).DesignSpecMultispeedHPName = Alphas( iDesignSpecMSHPNameAlphaNum );

				UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex = FindItemInList( UnitarySystem( UnitarySysNum ).DesignSpecMultispeedHPName, DesignSpecMSHP );
				DesignSpecNum = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;

				if ( DesignSpecNum > 0 ) {

					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {
						UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = DesignSpecMSHP( DesignSpecNum ).NumOfSpeedHeating;
						UnitarySystem( UnitarySysNum ).HeatMassFlowRate.allocate( DesignSpecMSHP( DesignSpecNum ).NumOfSpeedHeating );
						UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate.allocate( DesignSpecMSHP( DesignSpecNum ).NumOfSpeedHeating );
						UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio.allocate( DesignSpecMSHP( DesignSpecNum ).NumOfSpeedHeating );
						UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio = 1.0;
					}

					if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling ) {
						UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = DesignSpecMSHP( DesignSpecNum ).NumOfSpeedCooling;
						UnitarySystem( UnitarySysNum ).CoolMassFlowRate.allocate( DesignSpecMSHP( DesignSpecNum ).NumOfSpeedCooling );
						UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate.allocate( DesignSpecMSHP( DesignSpecNum ).NumOfSpeedCooling );
						UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio.allocate( DesignSpecMSHP( DesignSpecNum ).NumOfSpeedCooling );
						UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio = 1.0;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "... one or both of the following inputs are invalid." );
					ShowContinueError( "Field " + cAlphaFields( iDesignSpecMSHPTypeAlphaNum ) + " = " + Alphas( iDesignSpecMSHPTypeAlphaNum ) );
					ShowContinueError( "Field " + cAlphaFields( iDesignSpecMSHPNameAlphaNum ) + " = " + Alphas( iDesignSpecMSHPNameAlphaNum ) );
					ErrorsFound = true;
				}
			} else if ( ( lAlphaBlanks( iDesignSpecMSHPTypeAlphaNum ) && ! lAlphaBlanks( iDesignSpecMSHPNameAlphaNum ) ) || ( ! lAlphaBlanks( iDesignSpecMSHPTypeAlphaNum ) && lAlphaBlanks( iDesignSpecMSHPNameAlphaNum ) ) ) {
				ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "... one or both of the following inputs are invalid." );
				ShowContinueError( "Field " + cAlphaFields( iDesignSpecMSHPTypeAlphaNum ) + " = " + Alphas( iDesignSpecMSHPTypeAlphaNum ) );
				ShowContinueError( "Field " + cAlphaFields( iDesignSpecMSHPNameAlphaNum ) + " = " + Alphas( iDesignSpecMSHPNameAlphaNum ) );
				ErrorsFound = true;
			} else if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
				NumOfSpeedHeating = UnitarySystem( UnitarySysNum ).NumOfSpeedHeating;

				UnitarySystem( UnitarySysNum ).HeatMassFlowRate.allocate( NumOfSpeedHeating );
				UnitarySystem( UnitarySysNum ).HeatVolumeFlowRate.allocate( NumOfSpeedHeating );
				UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio.allocate( NumOfSpeedHeating );
				UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio = 1.0;

			} else if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
				NumOfSpeedCooling = UnitarySystem( UnitarySysNum ).NumOfSpeedCooling;

				UnitarySystem( UnitarySysNum ).CoolMassFlowRate.allocate( NumOfSpeedCooling );
				UnitarySystem( UnitarySysNum ).CoolVolumeFlowRate.allocate( NumOfSpeedCooling );
				UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio.allocate( NumOfSpeedCooling );
				UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio = 1.0;

			}

			if ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil ) {

				Index = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;
				if ( Index > 0 ) UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = DesignSpecMSHP( Index ).NumOfSpeedCooling;

				if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "... Cooling coil object type requires valid " + UnitarySysHeatPumpPerformanceObjectType + " for cooling to be specified with number of speeds > 0" );
					ErrorsFound = true;
				}
			}
			if ( UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil ) {

				Index = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;
				if ( Index > 0 ) UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = DesignSpecMSHP( Index ).NumOfSpeedHeating;

				if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating == 0 ) {
					ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "... Heating coil object type requires valid " + UnitarySysHeatPumpPerformanceObjectType + " for heating to be specified with number of speeds > 0" );
					ErrorsFound = true;
				}
			}

			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating && UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling ) {
				Index = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;
				if ( DesignSpecMSHP( Index ).SingleModeFlag ) {
					UnitarySystem( UnitarySysNum ).SingleMode = 1;
				}
			}
			// Check fan operation mode to ensure cycling fan mode
			if ( UnitarySystem( UnitarySysNum ).SingleMode == 1 ) {
				if ( !CheckScheduleValueMinMax( UnitarySystem( UnitarySysNum ).FanOpModeSchedPtr, ">=", 0.0, "<=", 0.0 ) ) {
					ShowSevereError( CurrentModuleObject + ": " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "The schedule values in " + cAlphaFields( iFanSchedAlphaNum ) + " must be 0 when Single Mode Operation is applied." );
					ShowContinueError( "A value of 0 represents cycling fan mode, any other value up to 1 represents constant fan mode." );
					ErrorsFound = true;
				}
			}

			// set global logicals that denote coil type
			if ( UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil || UnitarySystem( UnitarySysNum ).VarSpeedHeatingCoil ) {
				MultiOrVarSpeedHeatCoil( UnitarySysNum ) = true;
			}
			if ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil || UnitarySystem( UnitarySysNum ).VarSpeedCoolingCoil ) {
				MultiOrVarSpeedCoolCoil( UnitarySysNum ) = true;
			}

			// set global variables for multi-stage chilled and hot water coils
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {
				Index = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;
				if ( Index > 0 ) {
					UnitarySystem( UnitarySysNum ).NumOfSpeedCooling = DesignSpecMSHP( Index ).NumOfSpeedCooling;
					if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 1 ) {
						UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil = true;
						MultiOrVarSpeedCoolCoil( UnitarySysNum ) = true;
					}
				}
			}
			if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater ) {
				Index = UnitarySystem( UnitarySysNum ).DesignSpecMSHPIndex;
				if ( Index > 0 ) {
					UnitarySystem( UnitarySysNum ).NumOfSpeedHeating = DesignSpecMSHP( Index ).NumOfSpeedHeating;
					if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 1 ) {
						UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil = true;
						MultiOrVarSpeedHeatCoil( UnitarySysNum ) = true;
					}
				}
			}

		} //End of the Unitary System Loop

		// Setup Report variables for the Unitary System that are not reported in the components themselves
		for ( UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum ) {
			SetupOutputVariable( "Unitary System Part Load Ratio []", UnitarySystem( UnitarySysNum ).PartLoadFrac, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Total Cooling Rate [W]", UnitarySystem( UnitarySysNum ).TotCoolEnergyRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Cooling Rate [W]", UnitarySystem( UnitarySysNum ).SensCoolEnergyRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Latent Cooling Rate [W]", UnitarySystem( UnitarySysNum ).LatCoolEnergyRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Total Heating Rate [W]", UnitarySystem( UnitarySysNum ).TotHeatEnergyRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Heating Rate [W]", UnitarySystem( UnitarySysNum ).SensHeatEnergyRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Latent Heating Rate [W]", UnitarySystem( UnitarySysNum ).LatHeatEnergyRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			SetupOutputVariable( "Unitary System Ancillary Electric Power [W]", UnitarySystem( UnitarySysNum ).TotalAuxElecPower, "System", "Average", UnitarySystem( UnitarySysNum ).Name );

			//        IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)THEN
			SetupOutputVariable( "Unitary System Dehumidification Induced Heating Demand Rate [W]", UnitarySystem( UnitarySysNum ).DehumidInducedHeatingDemandRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			//        END IF

			if ( UnitarySystem( UnitarySysNum ).FanExists ) {
				SetupOutputVariable( "Unitary System Fan Part Load Ratio []", UnitarySystem( UnitarySysNum ).FanPartLoadRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			}

			SetupOutputVariable( "Unitary System Compressor Part Load Ratio []", UnitarySystem( UnitarySysNum ).CompPartLoadRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );

			SetupOutputVariable( "Unitary System Frost Control Status []", UnitarySystem( UnitarySysNum ).FrostControlStatus, "System", "Average", UnitarySystem( UnitarySysNum ).Name );

			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				SetupOutputVariable( "Unitary System Heating Ancillary Electric Energy [J]", UnitarySystem( UnitarySysNum ).HeatingAuxElecConsumption, "System", "Sum", UnitarySystem( UnitarySysNum ).Name, _, "Electric", "Heating", _, "System" );
			}

			{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );
			if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {
				SetupOutputVariable( "Unitary System Cycling Ratio []", UnitarySystem( UnitarySysNum ).CycRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System Compressor Speed Ratio []", UnitarySystem( UnitarySysNum ).SpeedRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {
				SetupOutputVariable( "Unitary System Cooling Ancillary Electric Energy [J]", UnitarySystem( UnitarySysNum ).CoolingAuxElecConsumption, "System", "Sum", UnitarySystem( UnitarySysNum ).Name, _, "Electric", "Cooling", _, "System" );
				SetupOutputVariable( "Unitary System Electric Power [W]", UnitarySystem( UnitarySysNum ).ElecPower, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System Electric Energy [J]", UnitarySystem( UnitarySysNum ).ElecPowerConsumption, "System", "Sum", UnitarySystem( UnitarySysNum ).Name );
				if ( UnitarySystem( UnitarySysNum ).HeatRecActive ) {
					SetupOutputVariable( "Unitary System Heat Recovery Rate [W]", UnitarySystem( UnitarySysNum ).HeatRecoveryRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
					SetupOutputVariable( "Unitary System Heat Recovery Inlet Temperature [C]", UnitarySystem( UnitarySysNum ).HeatRecoveryInletTemp, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
					SetupOutputVariable( "Unitary System Heat Recovery Outlet Temperature [C]", UnitarySystem( UnitarySysNum ).HeatRecoveryOutletTemp, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
					SetupOutputVariable( "Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]", UnitarySystem( UnitarySysNum ).HeatRecoveryMassFlowRate, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
					SetupOutputVariable( "Unitary System Heat Recovery Energy [J]", UnitarySystem( UnitarySysNum ).HeatRecoveryEnergy, "System", "Sum", UnitarySystem( UnitarySysNum ).Name );
				}
			} else if ( ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPVSEquationFit ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPSimple ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHP ) ) {
				SetupOutputVariable( "Unitary System Requested Sensible Cooling Rate [W]", UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System Requested Latent Cooling Rate [W]", UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			} else {
			}}

			{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );
			if ( ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) || ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) ) {
			} else if ( ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPSimple ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHP ) ) {
				SetupOutputVariable( "Unitary System Requested Heating Rate [W]", UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			} else {
			}}

			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_MultiSpeedCooling || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_MultiSpeedHeating || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingElectric_MultiStage || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) {
				SetupOutputVariable( "Unitary System DX Coil Cycling Ratio []", UnitarySystem( UnitarySysNum ).CycRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System DX Coil Speed Ratio []", UnitarySystem( UnitarySysNum ).SpeedRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System DX Coil Speed Level []", UnitarySystem( UnitarySysNum ).SpeedNum, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			}

			if ( ( ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) && UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil ) || ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater &&  UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil ) ) {
				SetupOutputVariable( "Unitary System Water Coil Cycling Ratio []", UnitarySystem( UnitarySysNum ).CycRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System Water Coil Speed Ratio []", UnitarySystem( UnitarySysNum ).SpeedRatio, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
				SetupOutputVariable( "Unitary System Water Coil Speed Level []", UnitarySystem( UnitarySysNum ).SpeedNum, "System", "Average", UnitarySystem( UnitarySysNum ).Name );
			}

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "AirLoopHVAC:UnitarySystem", UnitarySystem( UnitarySysNum ).Name, "Autosized Supply Air Flow Rate", "[m3/s]", UnitarySystem( UnitarySysNum ).DesignFanVolFlowRateEMSOverrideOn, UnitarySystem( UnitarySysNum ).DesignFanVolFlowRateEMSOverrideValue );
				SetupEMSActuator( "AirLoopHVAC:UnitarySystem", UnitarySystem( UnitarySysNum ).Name, "Autosized Supply Air Flow Rate During Cooling Operation", "[m3/s]", UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlowEMSOverrideOn, UnitarySystem( UnitarySysNum ).MaxCoolAirVolFlowEMSOverrideValue );
				SetupEMSActuator( "AirLoopHVAC:UnitarySystem", UnitarySystem( UnitarySysNum ).Name, "Autosized Supply Air Flow Rate During Heating Operation", "[m3/s]", UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlowEMSOverrideOn, UnitarySystem( UnitarySysNum ).MaxHeatAirVolFlowEMSOverrideValue );
				SetupEMSActuator( "AirLoopHVAC:UnitarySystem", UnitarySystem( UnitarySysNum ).Name, "Autosized Supply Air Flow Rate During No Heating or Cooling Operation", "[m3/s]", UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlowEMSOverrideOn, UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirVolFlowEMSOverrideValue );

				SetupEMSInternalVariable( "Unitary System Control Zone Mass Flow Fraction", UnitarySystem( UnitarySysNum ).Name, "[]", UnitarySystem( UnitarySysNum ).ControlZoneMassFlowFrac );
			}

		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( AnyEnergyManagementSystemInModel ) {
			for ( UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum ) {
				SetupEMSInternalVariable( "Unitary HVAC Design Heating Capacity", UnitarySystem( UnitarySysNum ).Name, "[W]", UnitarySystem( UnitarySysNum ).DesignHeatingCapacity );
				SetupEMSInternalVariable( "Unitary HVAC Design Cooling Capacity", UnitarySystem( UnitarySysNum ).Name, "[W]", UnitarySystem( UnitarySysNum ).DesignCoolingCapacity );
				SetupEMSActuator( "Unitary HVAC", UnitarySystem( UnitarySysNum ).Name, "Sensible Load Request", "[W]", UnitarySystem( UnitarySysNum ).EMSOverrideSensZoneLoadRequest, UnitarySystem( UnitarySysNum ).EMSSensibleZoneLoadValue );
				SetupEMSActuator( "Unitary HVAC", UnitarySystem( UnitarySysNum ).Name, "Moisture Load Request", "[W]", UnitarySystem( UnitarySysNum ).EMSOverrideMoistZoneLoadRequest, UnitarySystem( UnitarySysNum ).EMSMoistureZoneLoadValue );

			}
		}

		ManageEMS( emsCallFromComponentGetInput );

	}

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

	void
	ControlUnitarySystemtoSP(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // Primary air loop number
		bool const FirstHVACIteration, // True when first HVAC iteration
		int & CompOn, // compressor on/off control
		Optional< Real64 const > OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
		Optional_bool HXUnitOn // Flag to control HX for HXAssisted Cooling Coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages DXCoolingSystem component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PartLoadRatio; // coil operating part-load ratio
		Real64 OnOffAirFlowRatio; // Setpoint based coil control does not use this variable
		Real64 CoilCoolHeatRat; // ratio of cooling to heating PLR for cycling fan RH control

		OnOffAirFlowRatio = 1.0;
		CoilCoolHeatRat = 1.0;

		//CALL the series of components that simulate a Unitary System
		if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );
		}

		if ( UnitarySystem( UnitarySysNum ).CoolingCoilUpstream ) {

			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				UpdateUnitarySystemControl( UnitarySysNum, AirLoopNum, UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum, UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp );
				ControlCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HXUnitOn, CompOn );
				PartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
			}
			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				UpdateUnitarySystemControl( UnitarySysNum, AirLoopNum, UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum, UnitarySystem( UnitarySysNum ).SystemHeatControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp, _, UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp );
				ControlHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn );
				PartLoadRatio = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio );
			}

		} else {

			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				UpdateUnitarySystemControl( UnitarySysNum, AirLoopNum, UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum, UnitarySystem( UnitarySysNum ).SystemHeatControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp, _, UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp );
				ControlHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn );
				PartLoadRatio = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio );
			}
			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				UpdateUnitarySystemControl( UnitarySysNum, AirLoopNum, UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum, UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp );
				ControlCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HXUnitOn, CompOn );
				PartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
			}

		}

		if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );
		}

		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
			SuppHeatingCoilFlag = true;
			UpdateUnitarySystemControl( UnitarySysNum, AirLoopNum, UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode, UnitarySystem( UnitarySysNum ).SuppHeatControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp, _, UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp );
			ControlSuppHeatSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
			CalcUnitarySuppSystemToSP( UnitarySysNum, FirstHVACIteration );
			SuppHeatingCoilFlag = false;
		}

		UnitarySystem( UnitarySysNum ).InitHeatPump = false;

	}

	void
	ControlUnitarySystemtoLoad(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // Primary air loop number
		bool const FirstHVACIteration, // True when first HVAC iteration
		int & CompOn, // Determines if compressor is on or off
		Optional< Real64 const > OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
		Optional_bool HXUnitOn // Flag to control HX for HXAssisted Cooling Coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages DXCoolingSystem component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirLoop::AirLoopControlInfo;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CoolPLR; // cooling part-load ratio
		Real64 HeatPLR; // heating part-load ratio
		Real64 SuppPLR; // supplemental heating part-load ratio
		Real64 SensOutput; // Sensible output of Unitary System (W)
		Real64 LatOutput; // Latent output of Unitary System (W)
		Real64 ZoneLoad; // zone load (W)
		Real64 OnOffAirFlowRatio; // ratio of on to off air flow
		Real64 QZnReq; // zone load (W)
		Real64 FullSensibleOutput; // sensible output of Unitary System (W)
		//  REAL(R64)                     :: FullLatentOutput   ! latent output of Unitary System (W)
		Real64 SupHeaterLoad; // additional heating required by supplemental heater (W)
		Real64 HeatCoilLoad; // load pass to heating coil (W)

		OnOffAirFlowRatio = 1.0;
		QZnReq = 0.0;
		UpdateUnitarySystemControl( UnitarySysNum, AirLoopNum, UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum, UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp, ZoneLoad );

		// will not be running supplemental heater on this CALL (simulate with supplemental heater off)
		FullSensibleOutput = 0.0;
		// using furnace module logic
		// first check to see if cycling fan with economizer can meet the load
		if ( AirLoopNum > 0 ) {
			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists && UnitarySystem( UnitarySysNum ).HeatCoilExists && UnitarySystem( UnitarySysNum ).CoolingCoilType_Num != Coil_CoolingAirToAirVariableSpeed && UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != Coil_HeatingAirToAirVariableSpeed && ! FirstHVACIteration && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil && CoolingLoad && AirLoopControlInfo( AirLoopNum ).EconoActive ) {
				CompOn = 0;
				ControlUnitarySystemOutput( UnitarySysNum, AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn );
				if ( UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac >= 1.0 || UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac >= 1.0 || ( UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac <= 0.0 && UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac <= 0.0 ) ) {
					CompOn = 1;
					ControlUnitarySystemOutput( UnitarySysNum, AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn );
				}
			} else {
				CompOn = 1;
				ControlUnitarySystemOutput( UnitarySysNum, AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn );
			}
		} else {
			CompOn = 1;
			ControlUnitarySystemOutput( UnitarySysNum, AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn );
		}
		CoolPLR = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
		HeatPLR = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
		HeatCoilLoad = HeatPLR * UnitarySystem( UnitarySysNum ).DesignHeatingCapacity;
		SensOutput = 0.0;
		LatOutput = 0.0;

		if ( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode > 0 ) {
			SetComponentFlowRate( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate, UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode, UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).CoolCoilLoopNum, UnitarySystem( UnitarySysNum ).CoolCoilLoopSide, UnitarySystem( UnitarySysNum ).CoolCoilBranchNum, UnitarySystem( UnitarySysNum ).CoolCoilCompNum );
		}
		if ( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode > 0 ) {
			SetComponentFlowRate( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate, UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode, UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopNum, UnitarySystem( UnitarySysNum ).HeatCoilLoopSide, UnitarySystem( UnitarySysNum ).HeatCoilBranchNum, UnitarySystem( UnitarySysNum ).HeatCoilCompNum );
		}

		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists && ( HeatingLoad || CoolingLoad || MoistureLoad < 0.0 ) ) {
			if ( ( FullSensibleOutput < ( QToHeatSetPt - SmallLoad ) ) && ! FirstHVACIteration ) {
				SupHeaterLoad = max( 0.0, QToHeatSetPt - FullSensibleOutput );
				UnitarySystem( UnitarySysNum ).SupHeaterLoad = 0.0;
				// what does this line even do? I know we want the supplemental heater on only if there is a dehum load,
				// but for HP's the supp heater should also run if the heating coil can't turn on
				// (i.e., this line calc's a supp heater load, then next line also calc's it?)
				if ( MoistureLoad < 0.0 ) UnitarySystem( UnitarySysNum ).SupHeaterLoad = SupHeaterLoad;
				// so it look's like this next line should only be valid for HP's.
				if ( UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity > 0.0 ) {
					UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = min( 1.0, SupHeaterLoad / UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity );
				}
			} else {
				SupHeaterLoad = 0.0;
				UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = 0.0;
			}
		} else {
			SupHeaterLoad = 0.0;
			UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = 0.0;
		}

		CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutput, LatOutput, HXUnitOn, HeatCoilLoad, SupHeaterLoad, CompOn );

		// check supplemental heating coil outlet temp based on maximum allowed
		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
			SuppPLR = UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac;
			// only need to test for high supply air temp if supplemental coil is operating
			if ( SuppPLR > 0.0 ) {
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutput, LatOutput, HXUnitOn, HeatCoilLoad, SupHeaterLoad, CompOn );
				if ( UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity > 0.0 ) {
					UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = SupHeaterLoad / UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity;
				} else {
					UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = 0.0;
				}
			}
		}

		if ( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode > 0 ) {
			SetComponentFlowRate( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate, UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode, UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopNum, UnitarySystem( UnitarySysNum ).SuppCoilLoopSide, UnitarySystem( UnitarySysNum ).SuppCoilBranchNum, UnitarySystem( UnitarySysNum ).SuppCoilCompNum );
		}

		if ( UnitarySystem( UnitarySysNum ).HeatRecActive ) {
			SetComponentFlowRate( Node( UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum ).MassFlowRate, UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum, UnitarySystem( UnitarySysNum ).HeatRecoveryOutletNodeNum, UnitarySystem( UnitarySysNum ).HRLoopNum, UnitarySystem( UnitarySysNum ).HRLoopSideNum, UnitarySystem( UnitarySysNum ).HRBranchNum, UnitarySystem( UnitarySysNum ).HRCompNum );
		}

	}

	void
	ControlUnitarySystemOutput(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // Index to air loop
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 & OnOffAirFlowRatio, // ratio of heating PLR to cooling PLR (is this correct?)
		Real64 const ZoneLoad,
		Real64 & FullSensibleOutput,
		Optional_bool HXUnitOn, // Flag to control HX for HXAssisted Cooling Coil
		Optional_int CompOn
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines operating PLR and calculates the load based system output.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using General::TrimSigDigits;
		using DataHeatBalFanSys::TempControlType;
		using Psychrometrics::PsyCpAirFnWTdb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 100 ); // maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		int InletNode; // DX System inlet node number
		int OutletNode; // DX System outlet node number
		int OpMode; // fan operating mode (cycling or constant)
		Real64 PartLoadRatio; // operating part-load ratio
		Real64 SensOutputOff; // sensible output at PLR = 0 [W]
		Real64 LatOutputOff; // latent output at PLR = 0 [W]
		Real64 SensOutputOn; // sensible output at PLR = 1 [W]
		Real64 LatOutputOn; // latent output at PLR = 1 [W]
		Real64 CoolPLR; // cooing part load ratio
		Real64 HeatPLR; // heating part load ratio
		Array1D< Real64 > Par( 11 ); // parameters passed to RegulaFalsi function
		int SolFlag; // return flag from RegulaFalsi for sensible load
		int SolFlagLat; // return flag from RegulaFalsi for latent load
		Real64 TempLoad; // represents either a sensible or latent load [W]
		Real64 TempSysOutput; // represents either a sensible or latent capacity [W]
		Real64 TempSensOutput; // iterative sensible capacity [W]
		Real64 TempLatOutput; // iterative latent capacity [W]
		Real64 TempMinPLR; // iterative minimum PLR
		Real64 TempMaxPLR; // iterative maximum PLR
		int SpeedNum; // multi-speed coil speed number
		int CompressorONFlag; // 0= compressor off, 1= compressor on
		Real64 CoolingOnlySensibleOutput; // use to calculate dehumidification induced heating [W]
		Real64 CpAir; // specific heat of air [J/kg_C]

		CompName = UnitarySystem( UnitarySysNum ).Name;
		InletNode = UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum;
		OutletNode = UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum;
		OpMode = UnitarySystem( UnitarySysNum ).FanOpMode;
//		// Pass full mass flow rate on FirstHVACIteration to set MassFlowRateMax
//		if ( FirstHVACIteration && UnitarySystem( UnitarySysNum ).AirLoopEquipment ) {
//			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingHXAssisted || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == CoilDX_HeatingEmpirical || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {
//				PartLoadRatio = 1.0;
//				if ( HeatingLoad && UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
//					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedHeating;
//					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
//				} else if ( ( CoolingLoad || MoistureLoad < 0.0 ) && UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
//					//        CoolingLoad = .TRUE.
//					UnitarySystem( UnitarySysNum ).CoolingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedCooling;
//					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
//				}
//			} else {
//				if ( HeatingLoad ) {
//					PartLoadRatio = 0.0;
//					if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
//						UnitarySystem( UnitarySysNum ).HeatingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedHeating;
//						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
//						if ( UnitarySystem( UnitarySysNum ).Staged && std::abs( UnitarySystem( UnitarySysNum ).StageNum ) < UnitarySystem( UnitarySysNum ).NumOfSpeedHeating ) {
//							UnitarySystem( UnitarySysNum ).HeatingSpeedNum = std::abs( UnitarySystem( UnitarySysNum ).StageNum );
//							if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum == 1 ) UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
//						}
//					}
//				} else if ( CoolingLoad || MoistureLoad < 0.0 ) {
//					//        CoolingLoad = .TRUE.
//					PartLoadRatio = 1.0;
//					if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
//						UnitarySystem( UnitarySysNum ).CoolingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedCooling;
//						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
//						if ( UnitarySystem( UnitarySysNum ).Staged && std::abs( UnitarySystem( UnitarySysNum ).StageNum ) < UnitarySystem( UnitarySysNum ).NumOfSpeedCooling ) {
//							UnitarySystem( UnitarySysNum ).CoolingSpeedNum = std::abs( UnitarySystem( UnitarySysNum ).StageNum );
//							if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum == 1 ) UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
//						}
//					}
//				} else {
//					PartLoadRatio = 0.0;
//				}
//			}
//			SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
//			if ( HeatingLoad ) {
//				UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = 1.0;
//			} else if ( CoolingLoad || MoistureLoad < 0.0 ) {
//				UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 1.0;
//			}
//			if ( present( CompOn ) ) CompOn = 0;
//			return;
//		}

		if ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr ) <= 0.0 ) {
			return;
		}

		PartLoadRatio = 0.0; // Get no load result
		SolFlag = 0; // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect
		SolFlagLat = 0; // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect

		SensOutputOff = 0.0;
		LatOutputOff = 0.0;
		CoolPLR = 0.0;
		HeatPLR = 0.0;
		CompressorONFlag = 0;

		UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = 0.0;

		SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );

		if ( !HeatingLoad && !CoolingLoad && MoistureLoad >= 0.0 ) return;

		CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
		FullSensibleOutput = SensOutputOff;

		if ( ! HeatingLoad && ! CoolingLoad ) {
			// no load
			if ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) return;
			// Dehumcontrol_Multimode only controls RH if there is a sensible load
			if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) return;
		}

		// determine if PLR=0 meets the load
		{ auto const SELECT_CASE_var( TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) );
		if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
			if ( HeatingLoad && SensOutputOff > ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			if ( ! HeatingLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
		} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
			if ( CoolingLoad && SensOutputOff < ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			if ( ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
		} else if ( ( SELECT_CASE_var == SingleHeatCoolSetPoint ) || ( SELECT_CASE_var == DualSetPointWithDeadBand ) ) {
			if ( HeatingLoad && SensOutputOff > ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			if ( CoolingLoad && SensOutputOff < ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			if ( ! HeatingLoad && ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
		} else {
			// should never get here
		}}

		// if a variable speed unit, the SensOutputOff at SpeedNum=1 must be checked to see if it exceeds the ZoneLoad
		// This is still no load but at the first speed above idle
		if ( ( HeatingLoad && UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) || ( CoolingLoad && UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) ) {
			if ( UnitarySystem( UnitarySysNum ).Staged ) {
				if ( HeatingLoad ) {
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = UnitarySystem( UnitarySysNum ).StageNum;
				} else {
					UnitarySystem( UnitarySysNum ).CoolingSpeedNum = std::abs( UnitarySystem( UnitarySysNum ).StageNum );
				}
			} else {
				if ( HeatingLoad ) {
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 1;
				} else {
					UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 1;
				}
			}
			SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
			CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
			FullSensibleOutput = SensOutputOff;

			{ auto const SELECT_CASE_var( TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) );
			if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
				if ( HeatingLoad && SensOutputOff > ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
				if ( ! HeatingLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
				if ( CoolingLoad && SensOutputOff < ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
				if ( ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			} else if ( ( SELECT_CASE_var == SingleHeatCoolSetPoint ) || ( SELECT_CASE_var == DualSetPointWithDeadBand ) ) {
				if ( HeatingLoad && SensOutputOff > ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
				if ( CoolingLoad && SensOutputOff < ZoneLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
				if ( ! HeatingLoad && ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff ) ) return;
			} else {
				// should never get here
			}}
		}

		PartLoadRatio = 1.0; // Get full load result
		if ( present( CompOn ) ) {
			CompressorONFlag = CompOn;
		} else {
			CompressorONFlag = 1;
		}

		if ( HeatingLoad ) {
			CoolPLR = 0.0;
			HeatPLR = 1.0;
			UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand = ZoneLoad;
			UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = HeatPLR;
			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
				UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
				UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
				UnitarySystem( UnitarySysNum ).HeatingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedHeating;
			}
			if ( UnitarySystem( UnitarySysNum ).Staged && UnitarySystem( UnitarySysNum ).StageNum > 0 ) {
				if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = min( UnitarySystem( UnitarySysNum ).StageNum, UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
				}
				SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
				if ( SensOutputOff > ZoneLoad ) return;
				if ( UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
			}
		} else if ( CoolingLoad || MoistureLoad < LatOutputOff ) {
			CoolPLR = 1.0;
			HeatPLR = 0.0;
			if ( CoolingLoad ) {
				UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand = std::abs( ZoneLoad );
			} else {
				UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand = 0.0;
			}
			UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand = std::abs( MoistureLoad );
			UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = CoolPLR;
			if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) {
				UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
				UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
				UnitarySystem( UnitarySysNum ).CoolingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedCooling;
			}
			if ( UnitarySystem( UnitarySysNum ).Staged && UnitarySystem( UnitarySysNum ).StageNum < 0 ) {
				if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) UnitarySystem( UnitarySysNum ).CoolingSpeedNum = min( std::abs( UnitarySystem( UnitarySysNum ).StageNum ), UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );
				UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
				if ( SensOutputOff < ZoneLoad ) return;
				if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 ) UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
			}
		} else {
			// will return here when no cooling or heating load and MoistureLoad > LatOutputOff (i.e., PLR=0)
			return;
		}

		SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio );

		CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOn, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
		FullSensibleOutput = SensOutputOn;

		// turn on HX if DehumidControl_Multimode
		if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode && MoistureLoad < 0.0 && MoistureLoad < LatOutputOn && CoolingLoad ) {
			HXUnitOn = true;
			CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOn, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
			FullSensibleOutput = SensOutputOn;
		}

		// test to see if full capacity is less than load, if so set to PLR=1 and RETURN if no moisture load
		if ( ( HeatingLoad && UnitarySystem( UnitarySysNum ).NumOfSpeedHeating <= 1 ) || ( CoolingLoad && UnitarySystem( UnitarySysNum ).NumOfSpeedCooling <= 1 ) ) {
			{ auto const SELECT_CASE_var( TempControlType( UnitarySystem( UnitarySysNum ).ControlZoneNum ) );
			if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
				if ( HeatingLoad && SensOutputOn < ZoneLoad ) {
					UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = 1.0;
					UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = 1.0;
					if ( MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn ) return;
				}
				if ( ! HeatingLoad && ( MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn ) ) return;
			} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
				if ( CoolingLoad && SensOutputOn > ZoneLoad ) {
					UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 1.0;
					UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = 1.0;
					if ( MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn ) return;
				}
				if ( ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn ) ) return;
			} else if ( ( SELECT_CASE_var == SingleHeatCoolSetPoint ) || ( SELECT_CASE_var == DualSetPointWithDeadBand ) ) {
				if ( HeatingLoad && SensOutputOn < ZoneLoad ) {
					UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = 1.0;
					UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = 1.0;
					if ( MoistureLoad >= 0.0 || MoistureLoad > LatOutputOn ) return;
				}
				if ( CoolingLoad && SensOutputOn > ZoneLoad ) {
					UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 1.0;
					UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = 1.0;
					return;
				}
				if ( ! HeatingLoad && ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn ) ) {
					return;
				}
			} else {
				// no other choices for thermostat control
			}}
		}
		// will find speed for multispeed coils here and then RegulaFalsi on PLR at a fixed speed

		// Do the non-variable or non-multispeed coils have a NumOfSpeed = 0 ? We don't need to do this for single speed coils.
		// Check to see which speed to meet the load
		UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
		UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
		if ( ! UnitarySystem( UnitarySysNum ).Staged ) {
			if ( HeatingLoad ) {
				for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedHeating; ++SpeedNum ) {
					CoolPLR = 0.0;
					HeatPLR = 1.0;
					if ( SpeedNum == 1 ) {
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
					} else {
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
					}
					UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOn, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
					if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != Coil_HeatingWaterToAirHPVSEquationFit && ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWater && !UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil ) ) {
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
						UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum - 1;
						if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum == 0 ) {
							UnitarySystem( UnitarySysNum ).HeatingCycRatio = 0.0;
							HeatPLR = 0.0;
						} else {
							UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
							HeatPLR = 1.0;
						}
						CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
						UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
					}
					if ( ZoneLoad <= SensOutputOn ) {
						break;
					}
				}
			} else { // Cooling or moisture load
				for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedCooling; ++SpeedNum ) {
					CoolPLR = 1.0;
					HeatPLR = 0.0;
					if ( SpeedNum == 1 ) {
						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
					} else {
						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
					}
					UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
					UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum;
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOn, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );

					if ( ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num != Coil_CoolingWaterToAirHPVSEquationFit ) && ( ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWater || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) && !UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil ) ) {
						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
						UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum - 1;
						if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum == 0 ) {
							UnitarySystem( UnitarySysNum ).CoolingCycRatio = 0.0;
							CoolPLR = 0.0;
						} else {
							UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
							UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
							if ( UnitarySystem( UnitarySysNum ).SingleMode == 1 ) {
								CoolPLR = 1.0;
							} 
						}

						CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
						UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum;
					}
					if ( ZoneLoad >= SensOutputOn ) {
						break;
					}
				}
			}
		} else { // IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN
			// Staged control
			if ( HeatingLoad ) {
				CoolPLR = 0.0;
				HeatPLR = 1.0;
				SpeedNum = UnitarySystem( UnitarySysNum ).StageNum;
				if ( SpeedNum == 1 ) {
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
				} else {
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
					SpeedNum = min( UnitarySystem( UnitarySysNum ).StageNum, UnitarySystem( UnitarySysNum ).NumOfSpeedHeating );
				}
				UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
				UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOn, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
				if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num != Coil_HeatingWaterToAirHPVSEquationFit ) {
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum - 1;
					if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum == 0 ) {
						UnitarySystem( UnitarySysNum ).HeatingCycRatio = 0.0;
						HeatPLR = 0.0;
					} else {
						UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
						HeatPLR = 1.0;
					}
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
				}
				if ( ZoneLoad <= SensOutputOn ) {
					//        EXIT ????????????
				}
			} else { // Cooling or moisture load
				CoolPLR = 1.0;
				HeatPLR = 0.0;
				SpeedNum = std::abs( UnitarySystem( UnitarySysNum ).StageNum );
				if ( SpeedNum == 1 ) {
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
				} else {
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
					SpeedNum = min( std::abs( UnitarySystem( UnitarySysNum ).StageNum ), UnitarySystem( UnitarySysNum ).NumOfSpeedCooling );
				}
				UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
				UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum;
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOn, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );

				if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num != Coil_CoolingWaterToAirHPVSEquationFit ) {
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
					UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum - 1;
					if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum == 0 ) {
						UnitarySystem( UnitarySysNum ).CoolingCycRatio = 0.0;
						CoolPLR = 0.0;
					} else {
						UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
						CoolPLR = 1.0;
					}
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn, _, _, CompressorONFlag );
					UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum;
				}
				if ( ZoneLoad >= SensOutputOn ) {
					//        EXIT ???????????
				}
			}
		} // IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN

		FullSensibleOutput = SensOutputOn;

		if ( ! HeatingLoad && ! CoolingLoad && ( MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn ) ) {
			// if no load, or only a moisture load which can't be met at PLR=1, RETURN
			return;
		}
		// must test to see if load is bounded by capacity before calling RegulaFalsi
		if ( ( HeatingLoad && ZoneLoad < SensOutputOn ) || ( CoolingLoad && ZoneLoad > SensOutputOn ) ) {
			if ( ( HeatingLoad && ZoneLoad > SensOutputOff ) || ( CoolingLoad && ZoneLoad < SensOutputOff ) ) {
				Par( 1 ) = double( UnitarySysNum );
				Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
				if ( FirstHVACIteration ) Par( 2 ) = 1.0;
				Par( 3 ) = double( UnitarySystem( UnitarySysNum ).FanOpMode );
				Par( 4 ) = CompressorONFlag; // CompOp
				Par( 5 ) = ZoneLoad;
				Par( 6 ) = 0.0; // FLAG, 0.0 if heating load, 1.0 IF cooling or moisture load
				if ( CoolingLoad ) Par( 6 ) = 1.0;
				Par( 7 ) = 1.0; // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
				Par( 8 ) = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
				Par( 9 ) = 0.0; // HXUnitOn is always false for HX
				Par( 10 ) = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				Par( 11 ) = double( AirLoopNum );

				//     Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
				SolveRegulaFalsi( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemLoadResidual, 0.0, 1.0, Par );

				if ( SolFlag == -1 ) {
					if ( HeatingLoad ) {
						// IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
						// This does cause a problem when coil cannot turn on when OAT < min allowed or scheduled off
						// If max iteration limit is exceeded, how do we know if the heating coil is operating?
						TempMaxPLR = -0.1;
						TempSensOutput = SensOutputOff;
						while ( ( TempSensOutput - ZoneLoad ) < 0.0 && TempMaxPLR < 1.0 ) {
							// find upper limit of HeatingPLR
							TempMaxPLR += 0.1;

							// SUBROUTINE SetSpeedVariables(UnitarySysNum, SensibleLoad, PartLoadRatio)
							SetSpeedVariables( UnitarySysNum, true, TempMaxPLR );
							CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, TempMaxPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
						}
						TempMinPLR = TempMaxPLR;
						while ( ( TempSensOutput - ZoneLoad ) > 0.0 && TempMinPLR > 0.0 ) {
							// pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
							TempMaxPLR = TempMinPLR;
							// find minimum limit of HeatingPLR
							TempMinPLR -= 0.01;
							SetSpeedVariables( UnitarySysNum, true, TempMinPLR );
							CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, TempMinPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
						}
						// Now solve again with tighter PLR limits
						SolveRegulaFalsi( 0.001, MaxIter, SolFlag, HeatPLR, CalcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par );
						CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					} else if ( CoolingLoad ) {
						// RegulaFalsi may not find cooling PLR when the latent degradation model is used.
						// IF iteration limit is exceeded (SolFlag = -1), find tighter boundary of solution and repeat RegulaFalsi
						TempMaxPLR = -0.1;
						TempSysOutput = SensOutputOff;
						TempLoad = ZoneLoad;
						while ( ( TempSysOutput - TempLoad ) > 0.0 && TempMaxPLR < 0.95 ) { // avoid PLR > 1 by limiting TempMaxPLR to 1 (i.e., TempMaxPLR += 0.1)
							// find upper limit of HeatingPLR
							TempMaxPLR += 0.1;
							if ( TempMaxPLR > 0.95 && TempMaxPLR < 1.05 ) {
								TempMaxPLR = 1.0; // enforce a perfect 1.0 at the top end
							}
							SetSpeedVariables( UnitarySysNum, true, TempMaxPLR );
							CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, TempMaxPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
							TempSysOutput = TempSensOutput;
						}
						TempMinPLR = TempMaxPLR;
						while ( ( TempSysOutput - TempLoad ) < 0.0 && TempMinPLR > 0.05 ) {
							// pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
							TempMaxPLR = TempMinPLR;
							// find minimum limit of HeatingPLR
							TempMinPLR -= 0.01;
							SetSpeedVariables( UnitarySysNum, true, TempMinPLR );
							CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, TempMinPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
							TempSysOutput = TempSensOutput;
						}
						// Now solve again with tighter PLR limits
						SolveRegulaFalsi( 0.001, MaxIter, SolFlag, CoolPLR, CalcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par );
						CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					} // IF(HeatingLoad)THEN
					if ( SolFlag == -1 ) {
						if ( std::abs( ZoneLoad - TempSensOutput ) > SmallLoad ) {
							if ( UnitarySystem( UnitarySysNum ).MaxIterIndex == 0 ) {
								ShowWarningMessage( "Coil control failed to converge for " + UnitarySystem( UnitarySysNum ).UnitarySystemType + ':' + UnitarySystem( UnitarySysNum ).Name );
								ShowContinueError( "  Iteration limit exceeded in calculating system sensible part-load ratio." );
								ShowContinueErrorTimeStamp( "Sensible load to be met = " + TrimSigDigits( ZoneLoad, 2 ) + " (watts), sensible output = " + TrimSigDigits( TempSensOutput, 2 ) + " (watts), and the simulation continues." );
							}
							ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded in calculating sensible part-load ratio error continues. Sensible load statistics:", UnitarySystem( UnitarySysNum ).MaxIterIndex, ZoneLoad, ZoneLoad );
						}
					} else if ( SolFlag == -2 ) {
						if ( UnitarySystem( UnitarySysNum ).RegulaFalsIFailedIndex == 0 ) {
							ShowWarningMessage( "Coil control failed for " + UnitarySystem( UnitarySysNum ).UnitarySystemType + ':' + UnitarySystem( UnitarySysNum ).Name );
							ShowContinueError( "  sensible part-load ratio determined to be outside the range of 0-1." );
							ShowContinueErrorTimeStamp( "Sensible load to be met = " + TrimSigDigits( ZoneLoad, 2 ) + " (watts), and the simulation continues." );
						}
						ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - sensible part-load ratio out of range error continues. Sensible load statistics:", UnitarySystem( UnitarySysNum ).RegulaFalsIFailedIndex, ZoneLoad, ZoneLoad );
					}
				} else if ( SolFlag == -2 ) {
					if ( UnitarySystem( UnitarySysNum ).RegulaFalsIFailedIndex == 0 ) {
						ShowWarningMessage( "Coil control failed for " + UnitarySystem( UnitarySysNum ).UnitarySystemType + ':' + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "  sensible part-load ratio determined to be outside the range of 0-1." );
						ShowContinueErrorTimeStamp( "Sensible load to be met = " + TrimSigDigits( ZoneLoad, 2 ) + " (watts), and the simulation continues." );
					}
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - sensible part-load ratio out of range error continues. Sensible load statistics:", UnitarySystem( UnitarySysNum ).RegulaFalsIFailedIndex, ZoneLoad, ZoneLoad );
				} // IF (SolFlag == -1) THEN
			} else { // load is not bounded by capacity. Leave PLR=1 or turn off unit?
				UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 0.0;
				UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = 0.0;
				CoolPLR = 0.0;
				HeatPLR = 0.0;
				PartLoadRatio = 0.0;
			} // IF((HeatingLoad .AND. ZoneLoad > SensOutputOff) .OR. (CoolingLoad .AND. ZoneLoad < SensOutputOff))THEN
		} // IF((HeatingLoad .AND. ZoneLoad < SensOutputOn) .OR. (CoolingLoad .AND. ZoneLoad > SensOutputOn))THEN

		if ( HeatingLoad && ( UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil || UnitarySystem( UnitarySysNum ).VarSpeedHeatingCoil ) ) {
			if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum == 1 ) {
				UnitarySystem( UnitarySysNum ).HeatingCycRatio = PartLoadRatio;
				UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
			} else {
				if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
					UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = PartLoadRatio;
				} else {
					UnitarySystem( UnitarySysNum ).HeatingCycRatio = PartLoadRatio;
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
				}
			}
			HeatPLR = PartLoadRatio;
			CoolPLR = 0.0;
			UnitarySystem( UnitarySysNum ).CoolingCycRatio = 0.0;
			UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
		} else if ( CoolingLoad && ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil || UnitarySystem( UnitarySysNum ).VarSpeedCoolingCoil ) ) {
			if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum == 1 ) {
				UnitarySystem( UnitarySysNum ).CoolingCycRatio = PartLoadRatio;
				UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
			} else {
				if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
					UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = PartLoadRatio;
				} else {
					UnitarySystem( UnitarySysNum ).CoolingCycRatio = PartLoadRatio;
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
				}
			}
			UnitarySystem( UnitarySysNum ).HeatingCycRatio = 0.0;
			UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
			HeatPLR = 0.0;
			CoolPLR = PartLoadRatio;
		} else {
			HeatPLR = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
			CoolPLR = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
		}

		CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );

		// FullSensibleOutput is used to set supplemental heater PLR in calling routine
		// OnOffAirFlowRatio is used to average air flow between ON and OFF state
		FullSensibleOutput = TempSensOutput;

		// RETURN if the moisture load is met
		if ( MoistureLoad >= 0.0 || MoistureLoad >= TempLatOutput ) return;
		// Multimode does not meet the latent load, only the sensible load with or without HX active
		if ( ! CoolingLoad && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) return;
		//  IF(HeatingLoad .AND. UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)RETURN

		if ( ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat || UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) ) {

			// find maximum latent output IF not already calculated
			if ( HeatingLoad ) {
				CoolPLR = 1.0;
				UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 1.0;
				UnitarySystem( UnitarySysNum ).CoolingSpeedNum = UnitarySystem( UnitarySysNum ).NumOfSpeedCooling;
				UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
				UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
				UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = CoolPLR;
				if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 0 ) {
					UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = 0.0;
					UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
					HeatPLR = 0.0;
					CoolingLoad = true;
					HeatingLoad = false;
					UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand = 0.0;
					UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand = MoistureLoad;
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, 0.0, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
				} else {
					UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand = 0.0;
					UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand = 0.0;
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, 0.0, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand = MoistureLoad;
					CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
				}
			}

			if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode && MoistureLoad < LatOutputOn ) {
				HXUnitOn = true;
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, LatOutputOn, HXUnitOn, _, _, CompressorONFlag );
				FullSensibleOutput = TempSensOutput;
			}

			//    IF ((HeatingLoad .AND. MoistureLoad < TempLatOutput) .OR. &
			//        (CoolingLoad .AND. MoistureLoad < TempLatOutput .AND. MoistureLoad > LatOutputOn) .OR. &
			//        ((.NOT. HeatingLoad) .AND. (.NOT. CoolingLoad) .AND. MoistureLoad > LatOutputOn)) THEN
			if ( ( MoistureLoad < TempLatOutput ) && ( MoistureLoad > LatOutputOn ) ) { // bounds check for RegulaFalsi

				// save heating PLR
				HeatPLR = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				Par( 1 ) = double( UnitarySysNum );
				Par( 2 ) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
				if ( FirstHVACIteration ) Par( 2 ) = 1.0;
				Par( 3 ) = double( UnitarySystem( UnitarySysNum ).FanOpMode );
				Par( 4 ) = CompressorONFlag; // CompOp
				if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) {
					Par( 5 ) = ZoneLoad;
					Par( 7 ) = 1.0; // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
				} else {
					Par( 5 ) = MoistureLoad;
					Par( 7 ) = 0.0; // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
				}
				Par( 6 ) = 1.0; // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
				//      IF(HeatingLoad)Par(6)  = 0.0d0
				Par( 8 ) = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
				if ( HXUnitOn ) {
					Par( 9 ) = 1.0;
				} else {
					Par( 9 ) = 0.0;
				}
				Par( 10 ) = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				Par( 11 ) = double( AirLoopNum );
				// Tolerance is fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
				SolveRegulaFalsi( 0.001, MaxIter, SolFlagLat, PartLoadRatio, CalcUnitarySystemLoadResidual, 0.0, 1.0, Par );
				//      IF (HeatingLoad) THEN
				//        UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = PartLoadRatio
				//      ELSE
				UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = PartLoadRatio;
				//      END IF
				UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = HeatPLR;
			} else if ( MoistureLoad < LatOutputOn && CoolingLoad ) {
				//     Logic below needs further look...what to do if the bounds check for RegulaFalsi fail?
				//     I'm not even sure if this should be done.
				//     It's wrong anyway, since there won't be a cooling load if multimode (see RETURN about 80 lines up).
				if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_Multimode ) {
					UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = 1.0;
				}
			}
		}

		CoolPLR = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
		HeatPLR = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;

		CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );

		if ( SolFlagLat == -1 ) {
			// RegulaFalsi may not find cooling PLR when the latent degradation model is used.
			// IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
			TempMaxPLR = -0.1;
			TempLatOutput = LatOutputOff;
			while ( ( TempLatOutput - MoistureLoad ) > 0.0 && TempMaxPLR < 1.0 ) {
				// find upper limit of HeatingPLR
				TempMaxPLR += 0.1;
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, TempMaxPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
			}
			TempMinPLR = TempMaxPLR;
			while ( ( TempLatOutput - MoistureLoad ) < 0.0 && TempMinPLR > 0.0 ) {
				// pull upper limit of HeatingPLR DOwn to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
				TempMaxPLR = TempMinPLR;
				// find minimum limit of HeatingPLR
				TempMinPLR -= 0.01;
				CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, TempMinPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
			}
			// Now solve again with tighter PLR limits
			SolveRegulaFalsi( 0.001, MaxIter, SolFlagLat, CoolPLR, CalcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par );
			CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
			if ( SolFlagLat == -1 ) {
				if ( std::abs( MoistureLoad - TempLatOutput ) > SmallLoad ) {
					if ( UnitarySystem( UnitarySysNum ).LatMaxIterIndex == 0 ) {
						ShowWarningMessage( "Coil control failed to converge for " + UnitarySystem( UnitarySysNum ).UnitarySystemType + ':' + UnitarySystem( UnitarySysNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating system Latent part-load ratio." );
						ShowContinueErrorTimeStamp( "Latent load to be met = " + TrimSigDigits( MoistureLoad, 2 ) + " (watts), Latent output = " + TrimSigDigits( TempLatOutput, 2 ) + " (watts), and the simulation continues." );
					}
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded in calculating Latent part-load ratio error continues. Latent load statistics:", UnitarySystem( UnitarySysNum ).LatMaxIterIndex, MoistureLoad, MoistureLoad );
				}
			} else if ( SolFlagLat == -2 ) {
				if ( UnitarySystem( UnitarySysNum ).LatRegulaFalsIFailedIndex == 0 ) {
					ShowWarningMessage( "Coil control failed for " + UnitarySystem( UnitarySysNum ).UnitarySystemType + ':' + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "  Latent part-load ratio determined to be outside the range of 0-1." );
					ShowContinueErrorTimeStamp( "Latent load to be met = " + TrimSigDigits( MoistureLoad, 2 ) + " (watts), and the simulation continues." );
				}
				ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Latent part-load ratio out of range error continues. Latent load statistics:", UnitarySystem( UnitarySysNum ).LatRegulaFalsIFailedIndex, MoistureLoad, MoistureLoad );
			}
		} else if ( SolFlagLat == -2 ) {
			if ( UnitarySystem( UnitarySysNum ).LatRegulaFalsIFailedIndex == 0 ) {
				ShowWarningMessage( "Coil control failed for " + UnitarySystem( UnitarySysNum ).UnitarySystemType + ':' + UnitarySystem( UnitarySysNum ).Name );
				ShowContinueError( "  Latent part-load ratio determined to be outside the range of 0-1." );
				ShowContinueErrorTimeStamp( "Latent load to be met = " + TrimSigDigits( MoistureLoad, 2 ) + " (watts), and the simulation continues." );
			}
			ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Latent part-load ratio out of range error continues. Latent load statistics:", UnitarySystem( UnitarySysNum ).LatRegulaFalsIFailedIndex, MoistureLoad, MoistureLoad );
		}

		FullSensibleOutput = TempSensOutput;

		CpAir = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum ).Temp );
		CoolingOnlySensibleOutput = Node( UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum ).MassFlowRate * CpAir * ( ( Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp - Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).Temp ) - ( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp - Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp ) );
		if ( QToHeatSetPt < 0.0 ) {
			//   Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
			//   the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
			UnitarySystem( UnitarySysNum ).DehumidInducedHeatingDemandRate = max( 0.0, ( CoolingOnlySensibleOutput + QToHeatSetPt ) );
			//   Heating mode and dehumidification is required
		} else if ( QToHeatSetPt >= 0.0 ) {
			//   Calculate the reheat coil load as the sensible capacity of the DX cooling coil only. Let
			//   the heating coil pick up the load due to outdoor air.
			UnitarySystem( UnitarySysNum ).DehumidInducedHeatingDemandRate = max( 0.0, CoolingOnlySensibleOutput );
		}

	}

	void
	SetSpeedVariables(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const SensibleLoad, // True when meeting a sensible load (not a moisture load)
		Real64 const PartLoadRatio // operating PLR
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines operating PLR and calculates the load based system output.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using DataHeatBalFanSys::TempControlType;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool errFlag; // error flag returned from subroutine
		Real64 RuntimeFrac; // heat pump runtime fraction
		Real64 OnOffAirFlowRatio; //compressor on to average flow rate

		if ( HeatingLoad && SensibleLoad ) {
			UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
			UnitarySystem( UnitarySysNum ).CoolingCycRatio = 0.0;
			if ( UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil || UnitarySystem( UnitarySysNum ).VarSpeedHeatingCoil ) {
				if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum <= 1 ) {
					UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
					UnitarySystem( UnitarySysNum ).HeatingCycRatio = PartLoadRatio;
					MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // #5518
				} else {
					if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = PartLoadRatio;
						UnitarySystem( UnitarySysNum ).HeatingCycRatio = 1.0;
					} else {
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 1.0;
						UnitarySystem( UnitarySysNum ).HeatingCycRatio = PartLoadRatio;
					}
				}
			} else if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHP ) {
				HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, RuntimeFrac );
				if ( RuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
					OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
				} else {
					OnOffFanPartLoadFraction = 1;
				}
				UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadRatio;
				UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = RuntimeFrac;
				UnitarySystem( UnitarySysNum ).HeatingSpeedNum = 0;
			}
		} else {
			UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = 0.0;
			UnitarySystem( UnitarySysNum ).HeatingCycRatio = 0.0;
			if ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil || UnitarySystem( UnitarySysNum ).VarSpeedCoolingCoil ) {
				if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum <= 1 ) {
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
					UnitarySystem( UnitarySysNum ).CoolingCycRatio = PartLoadRatio;
					MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // #5518
				} else {
					if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = PartLoadRatio;
						UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
					} else {
						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 1.0;
						UnitarySystem( UnitarySysNum ).CoolingCycRatio = PartLoadRatio;
					}

				}
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple || UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHP ) {
				HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, RuntimeFrac );
				if ( RuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
					OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
				} else {
					OnOffFanPartLoadFraction = 1.0;
				}
				UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadRatio;
				UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = RuntimeFrac;
				UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
			} else if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed ) {
				if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum == 1 ) {
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = 0.0;
					UnitarySystem( UnitarySysNum ).CoolingCycRatio = PartLoadRatio;
				} else {
					UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = PartLoadRatio;
					UnitarySystem( UnitarySysNum ).CoolingCycRatio = 1.0;
				}
			} else {
				UnitarySystem( UnitarySysNum ).CoolingSpeedNum = 0;
			}
		}
		OnOffAirFlowRatio = 1.0;
		SetAverageAirFlow( UnitarySysNum, PartLoadRatio, OnOffAirFlowRatio );
	}

	Real64
	CalcUnitarySystemLoadResidual(
		Real64 const PartLoadRatio, // DX cooling coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the unitary system

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (force to 0)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(UnitarySysNum,r64) ! Index to Unitary System
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(OpMode,r64)     ! Fan control, IF 1.0 then cycling fan, if 0.0 then continuous fan
		//       Par(4)  = REAL(CompOp,r64)     ! Compressor control, IF 1.0 then compressor ON, if 0.0 then compressor OFF
		//       Par(5)  = SensLoad or MoistureLoad   ! Sensible or Latent load to be met by unitary system
		//       Par(6)  = HeatingLoad or CoolingLoad ! Type of load FLAG, 0.0 IF heating load, 1.0 IF cooling or moisture load
		//       Par(7)  = 1.0                  ! Output calculation FLAG, 0.0 for latent capacity, 1.0 for sensible capacity
		//       Par(8)  = OnOffAirFlowRatio    ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
		//       Par(9)  = HXUnitOn             ! flag to enable HX, 1=ON and 2=OFF
		//       Par(10) = HeatingCoilPLR       ! used to calculate latent degradation for cycling fan RH control

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // Index to this unitary system
		int AirLoopNum; // Index to air loop
		bool FirstHVACIteration; // FirstHVACIteration flag
		int FanOpMode; // Cycling fan or constant fan
		int CompOp; // Compressor on/off; 1=on, 0=off
		Real64 LoadToBeMet; // Sensible or Latent load to be met
		Real64 OnOffAirFlowRatio; // Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
		bool HXUnitOn; // flag to enable HX based on zone moisture load
		Real64 HeatPLR; // heating coil part load ratio
		Real64 CoolPLR; // cooling coil part load ratio
		bool SensibleLoad; // sensible load
		Real64 SensOutput; // sensible output of system
		Real64 LatOutput; // latent output of system

		// Convert parameters to usable variables
		UnitarySysNum = int( Par( 1 ) );
		if ( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		FanOpMode = int( Par( 3 ) );
		CompOp = int( Par( 4 ) );
		LoadToBeMet = Par( 5 );
		OnOffAirFlowRatio = Par( 8 );

		if ( Par( 6 ) == 1.0 ) {
			CoolPLR = PartLoadRatio;
			HeatPLR = 0.0;
		} else {
			CoolPLR = 0.0;
			HeatPLR = PartLoadRatio;
		}

		SensibleLoad = false;
		if ( Par( 7 ) == 1.0 ) SensibleLoad = true;

		if ( Par( 9 ) == 1.0 ) {
			HXUnitOn = true;
		} else {
			HXUnitOn = false;
		}

		AirLoopNum = int( Par( 11 ) );

		SetSpeedVariables( UnitarySysNum, SensibleLoad, PartLoadRatio );

		CalcUnitarySystemToLoad( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, HeatPLR, OnOffAirFlowRatio, SensOutput, LatOutput, HXUnitOn, _, _, CompOp );

		// Calculate residual based on output calculation flag
		if ( SensibleLoad ) {
			if ( std::abs( LoadToBeMet ) == 0.0) {
				Residuum = ( SensOutput - LoadToBeMet ) / 100.0;
			} else {
				Residuum = ( SensOutput - LoadToBeMet ) / LoadToBeMet;
			}
		} else {
			if ( std::abs( LoadToBeMet ) == 0.0 ) {
				Residuum = ( LatOutput - LoadToBeMet ) / 100.0;
			} else {
				Residuum = ( LatOutput - LoadToBeMet ) / LoadToBeMet;
			}
		}

		return Residuum;
	}

	void
	CalcUnitarySystemToLoad(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const CoolPLR, // operating cooling part-load ratio []
		Real64 const HeatPLR, // operating cooling part-load ratio []
		Real64 & OnOffAirFlowRatio, // ratio of heating PLR to cooling PLR (is this correct?)
		Real64 & SensOutput, // sensible capacity (W)
		Real64 & LatOutput, // latent capacity (W)
		Optional_bool HXUnitOn, // Flag to control HX for HXAssisted Cooling Coil
		Optional< Real64 > HeatCoilLoad, // Adjusted load to heating coil when SAT exceeds max limit (W)
		Optional< Real64 > SuppCoilLoad, // Adjusted load to supp heating coil when SAT exceeds max limit (W)
		Optional_int_const CompOn // Determines if compressor is on or off
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the resulting performance of the unitary system given
		// the operating PLR. System output is calculated with respect to zone condition.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // DX System outlet node number
		Real64 SuppPLR; // supplemental heating coil operating part-load ratio
		Real64 MinHumRatio; // used to calculate delivered capacity
		Real64 AirMassFlow; // operating mass flow rate through unitary system (kg/s)
		Real64 ZoneTemp; // zone node temperature (C)
		Real64 ZoneHumRat; // zone node humidity ratio (kg-water/kg-dryair)
		Real64 CoilCoolHeatRat; // ratio of cooling to heating PLR for cycling fan RH control
		int CoolingCompOn; // Compressor control (0=off, 1=on1)
		int HeatingCompOn; // Compressor control (0=off, 1=on1)
		Real64 MDotAir; // inlet air mass flow rate [kg/s]
		Real64 CpAir; // average specific heat [J/kg-C]
		Real64 CpAirIn; // inlet air Cp  [J/kg-C]
		Real64 CpAirOut; // outlet air Cp [J/kg-C]
		Real64 HCDeltaT; // heating coil delta temperture [deltaC]
		Real64 MaxHeatCoilLoad; // maximum allowed coil load so max temp is not exceeded [W]

		CoolingCompOn = 0;
		if ( CoolPLR > 0 ) {
			CoolingCompOn = 1;
			// let logical override compressor status if present (tests if economizer can meet load without compressor)
			if ( present( CompOn ) ) CoolingCompOn = CompOn;
			// for multispeed coils, comp is on IF speed > 1
		} else if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 1 ) {
			CoolingCompOn = 1;
		}

		HeatingCompOn = 0;
		if ( HeatPLR > 0 ) {
			HeatingCompOn = 1;
			// let logical override compressor status if present (tests if economizer can meet load without compressor)
			// probably don't need this for heating mode
			if ( present( CompOn ) ) HeatingCompOn = CompOn;
			CoilCoolHeatRat = min( 1.0, CoolPLR / HeatPLR );
		} else {
			CoilCoolHeatRat = 1.0;
		}
		// for multispeed coils, comp is on at PLR=0 IF speed > 1
		if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum > 1 ) HeatingCompOn = 1;

		// set the operating flow rate
		if ( UnitarySystem( UnitarySysNum ).NumOfSpeedCooling > 0 || UnitarySystem( UnitarySysNum ).NumOfSpeedHeating > 0 ) {
			SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, max( CoolPLR, HeatPLR ) );
		} else {
			SetAverageAirFlow( UnitarySysNum, max( CoolPLR, HeatPLR ), OnOffAirFlowRatio );
		}

		// Call the series of components that simulate a Unitary System
		if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );
		}

		if ( UnitarySystem( UnitarySysNum ).CoolingCoilUpstream ) {

			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
			}
			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				// operate the heating coil without regard to coil outlet temperature
				CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad );
				if ( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp > UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp ) {
					MDotAir = Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).MassFlowRate;
					CpAirIn = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp );
					CpAirOut = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp );
					CpAir = ( CpAirIn + CpAirOut ) / 2;
					HCDeltaT = UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp - Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp;
					MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
					CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad );
					if ( present( HeatCoilLoad ) ) HeatCoilLoad = MaxHeatCoilLoad;
				}
			}

			// If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
			if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == BlowThru && OnOffFanPartLoadFraction < 1.0 ) {
				SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );

				if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
				}
				if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
					CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad );
					if ( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp > UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp ) {
						MDotAir = Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).MassFlowRate;
						CpAirIn = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp );
						CpAirOut = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp );
						CpAir = ( CpAirIn + CpAirOut ) / 2;
						HCDeltaT = UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp - Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp;
						MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
						CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad );
					}
				}
			}

		} else {

			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad );
				if ( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp > UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp ) {
					MDotAir = Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).MassFlowRate;
					CpAirIn = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp );
					CpAirOut = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp );
					CpAir = ( CpAirIn + CpAirOut ) / 2;
					HCDeltaT = UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp - Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp;
					MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
					CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad );
				}
			}
			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
			}

			// If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
			if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == BlowThru && OnOffFanPartLoadFraction < 1.0 ) {
				SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );

				if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
					CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad );
					if ( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp > UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp ) {
						MDotAir = Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).MassFlowRate;
						CpAirIn = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp );
						CpAirOut = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).HumRat, Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp );
						CpAir = ( CpAirIn + CpAirOut ) / 2;
						HCDeltaT = UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp - Node( UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum ).Temp;
						MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
						CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad );
					}
				}
				if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
				}
			}

		}

		if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );
		}

		SuppPLR = UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac;
		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
			CalcUnitarySuppHeatingSystem( UnitarySysNum, FirstHVACIteration, SuppPLR, SuppCoilLoad );
			if ( ( Node( UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode ).Temp > UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp ) && SuppPLR > 0.0 ) {
				MDotAir = Node( UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode ).MassFlowRate;
				CpAirIn = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode ).HumRat, Node( UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode ).Temp );
				CpAirOut = PsyCpAirFnWTdb( Node( UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode ).HumRat, Node( UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode ).Temp );
				CpAir = ( CpAirIn + CpAirOut ) / 2;
				HCDeltaT = max( 0.0, UnitarySystem( UnitarySysNum ).DesignMaxOutletTemp - Node( UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode ).Temp );
				MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
				CalcUnitarySuppHeatingSystem( UnitarySysNum, FirstHVACIteration, SuppPLR, MaxHeatCoilLoad );
				if ( present( SuppCoilLoad ) ) SuppCoilLoad = MaxHeatCoilLoad;
			}
		}

		// Check delta T (outlet to space), IF positive
		// use space HumRat (next line), ELSE outlet humrat (IF) so psyc routine gives good result
		MinHumRatio = Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).HumRat;
		OutletNode = UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum;
		AirMassFlow = Node( OutletNode ).MassFlowRate;
		ZoneTemp = Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp;
		ZoneHumRat = Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).HumRat;
		if ( Node( OutletNode ).Temp < ZoneTemp ) MinHumRatio = Node( OutletNode ).HumRat;

		// Calculate sensible load met (at constant humidity ratio)
		SensOutput = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) - UnitarySystem( UnitarySysNum ).SenLoadLoss;
		UnitarySystem( UnitarySysNum ).SensibleLoadMet = SensOutput;

		if ( UnitarySystem( UnitarySysNum ).Humidistat ) {

			//   Calculate latent load met (at constant temperature)
			LatOutput = AirMassFlow * ( PsyHFnTdbW( ZoneTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( ZoneTemp, ZoneHumRat ) ) - UnitarySystem( UnitarySysNum ).LatLoadLoss;
		} else {
			LatOutput = 0.0;
		}
		UnitarySystem( UnitarySysNum ).LatentLoadMet = LatOutput;

	}

	void
	CalcUnitaryCoolingSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadRatio, // coil operating part-load ratio
		int const CompOn, // compressor control (0=off, 1=on)
		Real64 const OnOffAirFlowRatio,
		Real64 const CoilCoolHeatRat, // ratio of cooling to heating PLR for cycling fan RH control
		Optional_bool HXUnitOn // Flag to control HX for HXAssisted Cooling Coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages unitary cooling system component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::SimDXCoilMultiMode;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using Fans::SimulateFanComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;
		using UserDefinedComponents::SimCoilUserDefined;
		using PackagedThermalStorageCoil::SimTESCoil;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		int CompIndex; // index to cooling coil
		Real64 OutsideDryBulbTemp; // outdoor temperature (C)
		Real64 mdot; // water side flow rate (kg/s)
		Real64 QActual; // actual coil output (W)
		Real64 CoilPLR; // variable speed coils run at PLR = 1 when SpeedNum > 1
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		bool errFlag; // returned flag from called routine
		bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil is wrapped by UnitarySystem.
		bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil is wrapped by UnitarySystem.

		// Simulate the coil component
		CompName = UnitarySystem( UnitarySysNum ).CoolingCoilName;
		CompIndex = UnitarySystem( UnitarySysNum ).CoolingCoilIndex;
		CoilPLR = 1.0;
		if ( UnitarySystem( UnitarySysNum ).CondenserNodeNum != 0 ) {
			OutdoorPressure = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).Press;
			// IF node is not connected to anything, pressure = default, use weather data
			if ( OutdoorPressure == DefaultNodeValues.Press ) {
				OutsideDryBulbTemp = OutDryBulbTemp;
				//      OutdoorHumRat   = OutHumRat
				//      OutdoorPressure = OutBaroPress
				//      OutdoorWetBulb  = OutWetBulbTemp
			} else {
				OutsideDryBulbTemp = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).Temp;
				//      OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
				//      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
			}
		} else {
			OutsideDryBulbTemp = OutDryBulbTemp;
			//    OutdoorHumRat   = OutHumRat
			//    OutdoorPressure = OutBaroPress
			//    OutdoorWetBulb  = OutWetBulbTemp
		}
		//  PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) { // Coil:Cooling:DX:SingleSpeed

			SimDXCoil( BlankString, CompOn, FirstHVACIteration, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio, OnOffAirFlowRatio, CoilCoolHeatRat );
			UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( ( SELECT_CASE_var == CoilDX_CoolingHXAssisted ) || ( SELECT_CASE_var == CoilWater_CoolingHXAssisted ) ) { // CoilSystem:Cooling:*:HeatExchangerAssisted

			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilWater_CoolingHXAssisted ) {
				mdot = min( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow * PartLoadRatio );
				Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = mdot;
			}
			SimHXAssistedCoolingCoil( BlankString, FirstHVACIteration, CompOn, PartLoadRatio, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, HXUnitOn, OnOffAirFlowRatio, EconomizerFlag );
			if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == CoilDX_CoolingHXAssisted ) UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) { // Coil:Cooling:DX:TwoSpeed
			// formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

			SimDXCoilMultiSpeed( BlankString, UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).CoolingCycRatio, CompIndex );
			if ( UnitarySystem( UnitarySysNum ).CoolingSpeedRatio > 0.0 ) {
				UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingSpeedRatio * double( CompOn );
			} else {
				UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingCycRatio * double( CompOn );
			}

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) { // Coil:Cooling:DX:Multispeed

			if ( OutsideDryBulbTemp > UnitarySystem( UnitarySysNum ).MinOATCompressor ) {
				SimDXCoilMultiSpeed( CompName, UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).CoolingCycRatio, CompIndex, UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).FanOpMode, CompOn, UnitarySystem( UnitarySysNum ).SingleMode );
				if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 1 ) {
					if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
						UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = double( CompOn );
					} else {
						UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingCycRatio * double( CompOn );
					}
				} else {
					UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingCycRatio * double( CompOn );
				}
			} else {
				SimDXCoilMultiSpeed( CompName, 0.0, 0.0, CompIndex, UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).FanOpMode, CompOn );
				UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = 0.0;
			}

		} else if ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
			// formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

			SimDXCoilMultiMode( CompName, CompOn, FirstHVACIteration, PartLoadRatio, UnitarySystem( UnitarySysNum ).DehumidificationMode, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode );
			UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( SELECT_CASE_var == Coil_UserDefined ) {

			HeatingActive = false; // set to arbitrary value on entry to function
			CoolingActive = false; // set to arbitrary value on entry to function

			SimCoilUserDefined( CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive );

		} else if ( ( SELECT_CASE_var == Coil_CoolingWater ) || ( SELECT_CASE_var == Coil_CoolingWaterDetailed ) ) {

			mdot = min( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow * PartLoadRatio );
			Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );

		} else if ( ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPVSEquationFit ) ) {
			if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 1 ) {
				CoilPLR = 1.0;
			} else {
				CoilPLR = PartLoadRatio;
			}
			SimVariableSpeedCoils( CompName, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, CoilPLR, UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand, UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand, OnOffAirFlowRatio );
			if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 1 ) {
				UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = 1.0;
			} else {
				UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = PartLoadRatio * double( CompOn );
			}

		} else if ( SELECT_CASE_var == Coil_CoolingWaterToAirHPSimple ) {

			if ( PartLoadRatio > 0.0 && UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
				OnOffFanPartLoadFraction = PartLoadRatio / UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac;
			}

			SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand, UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, PartLoadRatio, FirstHVACIteration );

			UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( SELECT_CASE_var == Coil_CoolingWaterToAirHP ) {

			HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac );

			if ( PartLoadRatio > 0.0 && UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
				OnOffFanPartLoadFraction = PartLoadRatio / UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac;
			}

			SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow, UnitarySystem( UnitarySysNum ).FanOpMode, FirstHVACIteration, UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand, UnitarySystem( UnitarySysNum ).CoolingCoilLatentDemand, CompOn, PartLoadRatio );

			UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( SELECT_CASE_var == CoilDX_PackagedThermalStorageCooling ) {

			SimTESCoil( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).TESOpMode, PartLoadRatio );

		}}

		UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = PartLoadRatio;

	}

	void
	CalcUnitaryHeatingSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadRatio, // coil operating part-load ratio
		int const CompOn, // comrpressor control (0=off, 1=on)
		Real64 const OnOffAirFlowRatio, // ratio of on to off flow rate
		Optional< Real64 const > HeatCoilLoad // adjusted heating coil load if outlet temp exceeds max (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages unitary heating system component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiSpeed;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;
		using UserDefinedComponents::SimCoilUserDefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		Real64 OutsideDryBulbTemp; // outdoor temperature (C)
		Real64 mdot; // water side flow rate (kg/s)
		Real64 QActual; // actual output of coil (W)
		Real64 HeatPLR;
		Real64 dummy; // used when sub argument is not needed
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		bool errFlag; // returned flag from called routine
		bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil is wrapped by UnitarySystem.
		bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil is wrapped by UnitarySystem.

		CompName = UnitarySystem( UnitarySysNum ).HeatingCoilName;
		dummy = 0.0;
		HeatPLR = 1.0;
		if ( UnitarySystem( UnitarySysNum ).CondenserNodeNum != 0 ) {
			OutdoorPressure = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).Press;
			// IF node is not connected to anything, pressure = default, use weather data
			if ( OutdoorPressure == DefaultNodeValues.Press ) {
				OutsideDryBulbTemp = OutDryBulbTemp;
				//      OutdoorHumRat   = OutHumRat
				//      OutdoorPressure = OutBaroPress
				//      OutdoorWetBulb  = OutWetBulbTemp
			} else {
				OutsideDryBulbTemp = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).Temp;
				//      OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
				//      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
			}
		} else {
			OutsideDryBulbTemp = OutDryBulbTemp;
			//    OutdoorHumRat   = OutHumRat
			//    OutdoorPressure = OutBaroPress
			//    OutdoorWetBulb  = OutWetBulbTemp
		}

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // COIL:HEATING:DX:SINGLESPEED

			SimDXCoil( CompName, CompOn, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio, OnOffAirFlowRatio );
			UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( SELECT_CASE_var == Coil_UserDefined ) {

			HeatingActive = false; // set to arbitrary value on entry to function
			CoolingActive = true; // set to arbitrary value on entry to function

			SimCoilUserDefined( CompName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, AirLoopNum, HeatingActive, CoolingActive );

		} else if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
			if ( present( HeatCoilLoad ) ) {
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, HeatCoilLoad, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, false, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			} else {
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).DesignHeatingCapacity * PartLoadRatio, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, false, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			}
		} else if ( SELECT_CASE_var == Coil_HeatingDesuperheater ) {
			if ( present( HeatCoilLoad ) ) {
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, HeatCoilLoad, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, false, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			} else {
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).DesignHeatingCapacity * PartLoadRatio, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, false, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			}

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {

			if ( OutsideDryBulbTemp > UnitarySystem( UnitarySysNum ).MinOATCompressor ) {
				SimDXCoilMultiSpeed( CompName, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingCycRatio, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).HeatingSpeedNum, UnitarySystem( UnitarySysNum ).FanOpMode, CompOn, UnitarySystem( UnitarySysNum ).SingleMode );
				UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = PartLoadRatio * double( CompOn );
			} else {
				SimDXCoilMultiSpeed( CompName, 0.0, 0.0, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).HeatingSpeedNum, UnitarySystem( UnitarySysNum ).FanOpMode, CompOn );
				UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = 0.0;
			}

		} else if ( ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) ) {
			SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, 0, _, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio, UnitarySystem( UnitarySysNum ).HeatingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio );
			UnitarySystem( UnitarySysNum ).HeatingCycRatio = PartLoadRatio;
		} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
			mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow * PartLoadRatio );
			Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
			// this same CALL is made in the steam coil calc routine
			mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow * PartLoadRatio );
			Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateSteamCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).DesignHeatingCapacity * PartLoadRatio, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );

		} else if ( ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) ) {

			if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum > 1 ) {
				HeatPLR = 1.0;
			} else {
				HeatPLR = PartLoadRatio;
			}
			SimVariableSpeedCoils( CompName, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, HeatPLR, UnitarySystem( UnitarySysNum ).HeatingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand, dummy, OnOffAirFlowRatio );
			if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum > 1 ) {
				UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = 1.0;
			} else {
				UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = PartLoadRatio * double( CompOn );
			}
		} else if ( SELECT_CASE_var == Coil_HeatingWaterToAirHPSimple ) {

			if ( PartLoadRatio > 0.0 && UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
				OnOffFanPartLoadFraction = PartLoadRatio / UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac;
			}

			SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand, dummy, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, PartLoadRatio, FirstHVACIteration );
			UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else if ( SELECT_CASE_var == Coil_HeatingWaterToAirHP ) {

			HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac );

			if ( PartLoadRatio > 0.0 && UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
				OnOffFanPartLoadFraction = PartLoadRatio / UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac;
			}

			SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow, UnitarySystem( UnitarySysNum ).FanOpMode, FirstHVACIteration, UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand, dummy, CompOn, PartLoadRatio );
			UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio = PartLoadRatio * double( CompOn );

		} else {
			ShowFatalError( "CalcUnitaryHeatingSystem: Invalid Unitary System coil type = " + cAllCoilTypes( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num ) );

		}}

		UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = PartLoadRatio;

	}

	void
	CalcUnitarySuppHeatingSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadRatio, // coil operating part-load ratio
		Optional< Real64 const > SuppCoilLoad // adjusted supp coil load when outlet temp exceeds max (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages supplemental heater simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using General::SolveRegulaFalsi;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.e-3 ); // Accuracy of solver result

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		Real64 SuppHeatCoilLoad; // load passed to supplemental heating coil (W)
		Real64 QActual; // actual coil output (W)
		Real64 mdot; // water coil water mass flow rate (kg/s)
		Array1D< Real64 > Par( 5 ); // Parameter array passed to solver
		int SolFla; // Flag of solver, num iterations if >0, else error index
		Real64 PartLoadFrac; // temporary PLR variable

		// work is needed to figure out how to adjust other coil types if outlet temp exceeds maximum
		// this works for gas and electric heating coils
		CompName = UnitarySystem( UnitarySysNum ).SuppHeatCoilName;
		if ( OutDryBulbTemp <= UnitarySystem( UnitarySysNum ).MaxOATSuppHeat || ( MoistureLoad < 0.0 && UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac > 0.0 ) ) {
			if ( present( SuppCoilLoad ) ) {
				SuppHeatCoilLoad = SuppCoilLoad;
			} else {
				SuppHeatCoilLoad = UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity * PartLoadRatio;
			}
		} else {
			SuppHeatCoilLoad = 0.0;
		}

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num );

		if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
			{ auto const SELECT_CASE_var1( UnitarySystem( UnitarySysNum ).ControlType );
			if ( SELECT_CASE_var1 == SetPointBased ) {
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, true, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			} else {
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, SuppHeatCoilLoad, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, true, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			}}
		} else if ( SELECT_CASE_var == Coil_HeatingDesuperheater ) {
			SimulateHeatingCoilComponents( CompName, FirstHVACIteration, SuppHeatCoilLoad, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, true, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );

		} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
			if ( present( SuppCoilLoad ) ) {
				if ( SuppHeatCoilLoad > 0.0 ) {
					// see if HW coil has enough capacity to meet the load
					mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow );
					Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;
					//     simulate water coil to find operating capacity
					SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
					if ( QActual > SuppHeatCoilLoad ) {
						Par( 1 ) = double( UnitarySysNum );
						if ( FirstHVACIteration ) {
							Par( 2 ) = 1.0;
						} else {
							Par( 2 ) = 0.0;
						}
						Par( 3 ) = SuppHeatCoilLoad;
						Par( 4 ) = 1.0; // SuppHeatingCoilFlag
						Par( 5 ) = 1.0; // Load based control
						SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HotWaterHeatingCoilResidual, 0.0, 1.0, Par );
						UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = PartLoadFrac;
					} else {
						UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = 1.0;
					}
				}
			} else {
				mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow * PartLoadRatio );
				Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;

				SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );
			}
		} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
			mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow * PartLoadRatio );
			Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateSteamCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, SuppHeatCoilLoad, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadRatio );

		} else {

		}}

		//  UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = PartLoadRatio

	}

	void
	CalcUnitarySuppSystemToSP(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration // True when first HVAC iteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages supplemental heater component simulation for setpoint based operation scheme.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		int CoilType_Num; // integer type of coil
		Real64 QActual;

		CompName = UnitarySystem( UnitarySysNum ).SuppHeatCoilName;
		CoilType_Num = UnitarySystem ( UnitarySysNum ).SuppHeatCoilType_Num;

		if ( ( CoilType_Num == Coil_HeatingGas ) || ( CoilType_Num == Coil_HeatingElectric ) ) {
			SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, _, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac );
			//                             QCoilReq=(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity*UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac)

		} else if ( CoilType_Num == Coil_HeatingDesuperheater ) {
			SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, _, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac );

		} else if ( CoilType_Num == Coil_HeatingWater ) {
			SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac );

		} else if ( CoilType_Num == Coil_HeatingSteam ) {
			SimulateSteamCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity * UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac, _, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac );

		} else {

		}

	}

	void
	ControlCoolingSystem(
		int const UnitarySysNum, // index to Unitary System
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration, // First HVAC iteration flag
		bool & HXUnitOn, // flag to enable heat exchanger heat recovery
		int & CompOn // compressor on/off control
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Simulate the coil object at the required PLR.

		// METHODOLOGY EMPLOYED:
		//  Calculate operating PLR and adjust speed when using multispeed coils.
		//  Meet moisture load if required to do so.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataAirLoop::LoopDXCoilRTF;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTdpFnWPb;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::SimDXCoilMultiMode;
		using DXCoils::DXCoilOutletHumRat;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp;
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat;
		using PlantUtilities::SetComponentFlowRate;
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using UserDefinedComponents::SimCoilUserDefined;
		using PackagedThermalStorageCoil::SimTESCoil;
		using PackagedThermalStorageCoil::OffMode;
		using PackagedThermalStorageCoil::ChargeOnlyMode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.e-3 ); // Accuracy of solver result
		Real64 const HumRatAcc( 1.e-6 ); // Accuracy of solver result

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of the DX cooling coil
		int CoilType_Num; // integer type of coil
		//  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
		Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
		Real64 ReqOutput; // Sensible capacity (outlet - inlet) required to meet load or setpoint temperature
		int InletNode; // Inlet node number of the DX cooling coil
		int OutletNode; // Outlet node number of the DX cooling coil
		int ControlNode; // The node number where a setpoint is placed to control the DX cooling coil
		Real64 PartLoadFrac; // The part-load fraction of the compressor
		Real64 SpeedRatio; // SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
		//              (CompressorSpeedMax - CompressorSpeedMin)
		// for variable speed or 2 speed compressors
		Real64 CycRatio; // Cycling part-load ratio for variable speed or 2 speed compressors
		Real64 DesOutTemp; // Desired outlet temperature of the DX cooling coil
		Real64 DesOutHumRat; // Desired outlet humidity ratio of the DX cooling coil
		Real64 OutletTempDXCoil; // Actual outlet temperature of the DX cooling coil
		Real64 OutletHumRatLS; // Actual outlet humrat of the variable speed DX cooling coil at low speed
		Real64 OutletHumRatHS; // Actual outlet humrat of the variable speed DX cooling coil at high speed
		Real64 OutletHumRatDXCoil; // Actual outlet humidity ratio of the DX cooling coil
		int SolFla; // Flag of solver, num iterations if >0, else error index
		int SolFlaLat; // Flag of solver for dehumid calculations
		Array1D< Real64 > Par( 8 ); // Parameter array passed to solver
		bool SensibleLoad; // True if there is a sensible cooling load on this system
		bool LatentLoad; // True if there is a latent   cooling load on this system
		int DehumidMode; // dehumidification mode (0=normal, 1=enhanced)
		int FanOpMode; // Supply air fan operating mode
		Real64 TempMinPLR; // Used to find latent PLR when max iterations exceeded
		Real64 TempMaxPLR; // Used to find latent PLR when max iterations exceeded
		Real64 TempOutletTempDXCoil; // Used to find latent PLR when max iterations exceeded
		Real64 TempOutletHumRatDXCoil; // Used to find latent PLR when max iterations exceeded
		Real64 NoLoadHumRatOut; // DX coil outlet air humidity ratio with comprssor off
		Real64 FullLoadHumRatOut; // DX coil outlet air humidity ratio with comprssor full on
		Real64 WSHPRuntimeFrac; // Run time fraction of water to air hp
		Real64 dummy; // dummy variable for heating latent demand
		Real64 SensLoad; // turns on coil
		Real64 OnOffAirFlowRatio;
		Real64 OutletTemp;
		int SpeedNum;
		Real64 LoopDXCoilMaxRTFSave; // Used to find RTF of DX heating coils without overwriting globabl variable
		Real64 NoLoadTempOut; // saves coil off outlet temp
		bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
		bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.

		// Set local variables
		// Retrieve the load on the controlled zone
		OutletNode = UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum;
		InletNode = UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum;
		ControlNode = UnitarySystem( UnitarySysNum ).SystemCoolControlNodeNum;
		DesOutTemp = UnitarySystem( UnitarySysNum ).DesiredOutletTemp;
		DesOutHumRat = UnitarySystem( UnitarySysNum ).DesiredOutletHumRat;
		CoilType_Num = UnitarySystem ( UnitarySysNum ).CoolingCoilType_Num;
		LoopDXCoilMaxRTFSave = LoopDXCoilRTF;
		LoopDXCoilRTF = 0.0;

		CompName = UnitarySystem( UnitarySysNum ).CoolingCoilName;
		FanOpMode = UnitarySystem( UnitarySysNum ).FanOpMode;
		SpeedRatio = 0.0;
		SpeedNum = 0;
		CycRatio = 0.0;
		PartLoadFrac = 0.0;
		DehumidMode = 0;
		SensibleLoad = false;
		LatentLoad = false;
		WSHPRuntimeFrac = 0.0;
		dummy = 0.0;
		SensLoad = 0.0;
		SolFla = 0.0;
		SolFlaLat = 0.0;
		NoLoadTempOut = 0.0;
		NoLoadHumRatOut = 0.0;
		OnOffAirFlowRatio = 0.0; //Autodesk:Init Patch to prevent use uninitialized in calls to SimVariableSpeedCoils

		// Check the dehumidification control type. IF it's multimode, turn off the HX to find the sensible PLR. Then check to
		// see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
		if ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num != DehumidControl_Multimode ) {
			HXUnitOn = true;
		} else {
			HXUnitOn = false;
		}

		// IF DXCoolingSystem is scheduled on and there is flow
		if ( ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr ) > 0.0 ) && GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).CoolingCoilAvailSchPtr ) > 0.0 && ( Node( InletNode ).MassFlowRate > MinAirMassFlow ) && UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac == 0.0 ) {

			// Determine if there is a sensible load on this system (aren't the first 2 tests redundant?)
			if ( ( Node( InletNode ).Temp > DesOutTemp ) && ( std::abs( Node( InletNode ).Temp - DesOutTemp ) > TempControlTol ) ) SensibleLoad = true;

			// Determine if there is a latent load on this system - for future use to serve latent-only loads
			if ( Node( InletNode ).HumRat > DesOutHumRat ) LatentLoad = true;

			// disable latent dehumidification if there is no sensible load and latent only is not allowed
			if ( UnitarySystem( UnitarySysNum ).RunOnLatentOnlyWithSensible && ! SensibleLoad ) LatentLoad = false;

			// IF DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
			// Multimode coil will switch to enhanced dehumidification IF available and needed, but it
			// still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
			if ( ( SensibleLoad && UnitarySystem( UnitarySysNum ).RunOnSensibleLoad ) || ( LatentLoad && UnitarySystem( UnitarySysNum ).RunOnLatentLoad ) ) {
				// calculate sensible PLR, don't care IF latent is true here but need to gaurd for
				// when LatentLoad=TRUE and SensibleLoad=FALSE
				ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DesOutTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

				PartLoadFrac = 0.0;
				CompOn = 0;

				if ( CoilType_Num == CoilDX_CoolingSingleSpeed ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
					UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					SimDXCoil( CompName, On, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, PartLoadFrac );

				} else if ( ( CoilType_Num == CoilDX_CoolingHXAssisted ) || ( CoilType_Num == CoilWater_CoolingHXAssisted ) ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

					if ( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode > 0 ) Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = 0.0;

					SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, PartLoadFrac, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
					if ( CoilType_Num == CoilDX_CoolingHXAssisted ) UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;
				} else if ( CoilType_Num == CoilDX_CoolingTwoSpeed ) {

					SimDXCoilMultiSpeed( CompName, 0.0, PartLoadFrac, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );

				} else if ( CoilType_Num == CoilDX_MultiSpeedCooling ) {

					SimMultiSpeedCoils( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, CoolingCoil );

				} else if ( ( CoilType_Num == Coil_CoolingAirToAirVariableSpeed ) || ( CoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

					UnitarySystem( UnitarySysNum ).CoolingCoilSensDemand = ReqOutput;
					SimVariableSpeedCoils( "", UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, CycRatio, SpeedNum, SpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

				} else if ( CoilType_Num == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode

					SimDXCoilMultiMode( CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode );
					UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;
				} else if ( ( CoilType_Num == Coil_CoolingWater ) || ( CoilType_Num == Coil_CoolingWaterDetailed ) ) { // COIL:COOLING:WATER

					SimWaterCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, CoolingCoil );

				} else if ( CoilType_Num == Coil_CoolingWaterToAirHPSimple ) {

					SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ReqOutput, dummy, FanOpMode, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 0, PartLoadFrac, FirstHVACIteration );
					UnitarySystem ( UnitarySysNum ).CoolingCoilSensDemand = 0.0;

				} else if ( CoilType_Num == Coil_CoolingWaterToAirHP ) {

					SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow, FanOpMode, FirstHVACIteration, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadFrac );

				} else if ( CoilType_Num == Coil_UserDefined ) {

					HeatingActive = false; // set to arbitrary value on entry to function
					CoolingActive = true; // set to arbitrary value on entry to function
					SimCoilUserDefined( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, AirLoopNum, HeatingActive, CoolingActive );
					if ( CoolingActive ) PartLoadFrac = 1.0;

				} else if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling ) {

					SimTESCoil( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).TESOpMode, PartLoadFrac );

				} else {

				}

				//      NoOutput = Node(InletNode)%MassFlowRate *  &
				//                   (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
				//                    - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
				NoLoadTempOut = Node( OutletNode ).Temp;
				NoLoadHumRatOut = Node( OutletNode ).HumRat;

				//     Changed logic to use temperature instead of load. The Psyc calcs can cause slight errors.
				//     For example it's possible that (NoOutput-ReqOutput) > Acc while (Node(OutletNode)%Temp-DesOutTemp) is not
				//     This can (and did) lead to RegulaFalsi errors

				//      IF ((NoOutput-ReqOutput) .LT. Acc) THEN
				//     IF outlet temp at no load is lower than DesOutTemp (set point), do not operate the coil
				if ( ( NoLoadTempOut - DesOutTemp ) < Acc ) {
					PartLoadFrac = 0.0;
				} else if ( SensibleLoad ) { // need to turn on compressor to see if load is met
					PartLoadFrac = 1.0;
					CompOn = 1;
					WSHPRuntimeFrac = 1.0;

					if ( CoilType_Num == CoilDX_CoolingSingleSpeed ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

						SimDXCoil( CompName, On, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, PartLoadFrac );
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else if ( ( CoilType_Num == CoilDX_CoolingHXAssisted ) || ( CoilType_Num == CoilWater_CoolingHXAssisted ) ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

						if ( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode > 0 ) Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = max( 0.0, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow );
						SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, PartLoadFrac, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );

						if ( CoilType_Num == CoilDX_CoolingHXAssisted ) UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else if ( CoilType_Num == CoilDX_CoolingTwoSpeed ) {

						CycRatio = 1.0;
						for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedCooling; ++SpeedNum ) {
							SpeedRatio = double( SpeedNum ) - 1.0;
							SimDXCoilMultiSpeed( CompName, SpeedRatio, CycRatio, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							OutletTemp = DXCoilOutletTemp( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							if ( OutletTemp < DesOutTemp && SensibleLoad ) break; // this isn't going to work IF dehumidIFying
						}

					} else if ( CoilType_Num == CoilDX_MultiSpeedCooling ) {

						CycRatio = 1.0;
						SpeedRatio = 0.0;
						for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedCooling; ++SpeedNum ) {
							if ( SpeedNum > 1 ) CycRatio = 0.0;
							if ( SpeedNum > 1 ) SpeedRatio = 1.0;
							UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum;
							SimMultiSpeedCoils( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, CoolingCoil, SpeedNum );
							OutletTemp = Node( OutletNode ).Temp;
							if ( OutletTemp < DesOutTemp && SensibleLoad ) break;
						}

					} else if ( ( CoilType_Num == Coil_CoolingAirToAirVariableSpeed ) || ( CoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

						CycRatio = 1.0;
						SpeedRatio = 1.0;
						SensLoad = -1.0; // turns on coil
						UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = SpeedRatio;
						UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = PartLoadFrac;
						for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedCooling; ++SpeedNum ) {
							UnitarySystem( UnitarySysNum ).CoolingSpeedNum = SpeedNum;
							SimVariableSpeedCoils( "", UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, CycRatio, SpeedNum, SpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );
							OutletTemp = Node( OutletNode ).Temp;
							if ( OutletTemp < DesOutTemp && SensibleLoad ) break;
						}

					} else if ( CoilType_Num == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode

						SimDXCoilMultiMode( CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode );
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else if ( ( CoilType_Num == Coil_CoolingWater ) || ( CoilType_Num == Coil_CoolingWaterDetailed ) ) { // COIL:COOLING:WATER

						SimWaterCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, CoolingCoil );

					} else if ( CoilType_Num == Coil_CoolingWaterToAirHPSimple ) {

						SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ReqOutput, dummy, FanOpMode, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, PartLoadFrac, FirstHVACIteration );
						UnitarySystem ( UnitarySysNum ).CoolingCoilSensDemand = ReqOutput;

					} else if ( CoilType_Num == Coil_CoolingWaterToAirHP ) {

						SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow, FanOpMode, FirstHVACIteration, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadFrac );

					} else if ( CoilType_Num == Coil_UserDefined ) {
						HeatingActive = false; // set to arbitrary value on entry to function
						CoolingActive = false; // set to arbitrary value on entry to function

						SimCoilUserDefined( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, AirLoopNum, HeatingActive, CoolingActive );
						if ( CoolingActive ) PartLoadFrac = 1.0;

					} else if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling ) {

						// TES coil simulated above with PLR=0. Operating mode is known here, no need to simulate again to determine operating mode.
						if ( UnitarySystem( UnitarySysNum ).TESOpMode == OffMode || UnitarySystem( UnitarySysNum ).TESOpMode == ChargeOnlyMode ) { // cannot cool
							PartLoadFrac = 0.0;
						} else {
							// Get full load result
							SimTESCoil( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).TESOpMode, PartLoadFrac );
						}

					} else {

					}

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					FullLoadHumRatOut = Node( OutletNode ).HumRat;

					//        IF ((FullOutput - ReqOutput) .GT. Acc) THEN ! old method
					//        IF ((Node(OutletNode)%Temp-DesOutTemp) .GT. Acc) THEN ! new method gets caught when temps are very close
					if ( Node( OutletNode ).Temp > DesOutTemp - Acc ) {
						PartLoadFrac = 1.0;
						if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling && ( UnitarySystem( UnitarySysNum ).TESOpMode == OffMode || UnitarySystem( UnitarySysNum ).TESOpMode == ChargeOnlyMode ) ) {
							PartLoadFrac = 0.0;
						}
					} else if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling && ( UnitarySystem( UnitarySysNum ).TESOpMode == OffMode || UnitarySystem( UnitarySysNum ).TESOpMode == ChargeOnlyMode ) ) {
							PartLoadFrac = 0.0;
					} else {

						if ( CoilType_Num == CoilDX_CoolingSingleSpeed ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0, 1.0, Par );
							UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( ( CoilType_Num == CoilDX_CoolingHXAssisted ) || ( CoilType_Num == CoilWater_CoolingHXAssisted ) ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
							if ( FirstHVACIteration ) {
								Par( 3 ) = 1.0;
							} else {
								Par( 3 ) = 0.0;
							}
							if ( HXUnitOn ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = double( FanOpMode );
							Par( 6 ) = double( UnitarySysNum );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {

								//                 RegulaFalsi may not find sensible PLR when the latent degradation model is used.
								//                 IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
								TempMaxPLR = -0.1;
								TempOutletTempDXCoil = Node( InletNode ).Temp;
								while ( ( TempOutletTempDXCoil - DesOutTemp ) > 0.0 && TempMaxPLR <= 1.0 ) {
									//                   find upper limit of PLR
									TempMaxPLR += 0.1;
									SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMaxPLR, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
									TempOutletTempDXCoil = HXAssistedCoilOutletTemp( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								}
								TempMinPLR = TempMaxPLR;
								while ( ( TempOutletTempDXCoil - DesOutTemp ) < 0.0 && TempMinPLR >= 0.0 ) {
									//                  pull upper limit of PLR DOwn to last valid limit (i.e. outlet temp still exceeds DesOutTemp)
									TempMaxPLR = TempMinPLR;
									//                   find minimum limit of PLR
									TempMinPLR -= 0.01;
									SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMinPLR, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
									TempOutletTempDXCoil = HXAssistedCoilOutletTemp( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								}
								//                 Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary may be
								//                 very near the desired result)
								TempMinPLR = max( 0.0, ( TempMinPLR - 0.01 ) );
								TempMaxPLR = min( 1.0, ( TempMaxPLR + 0.01 ) );
								//                 tighter boundary of solution has been found, CALL RegulaFalsi a second time
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, TempMinPLR, TempMaxPLR, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( UnitarySystem( UnitarySysNum ).HXAssistedSensPLRIter < 1 ) {
											++UnitarySystem( UnitarySysNum ).HXAssistedSensPLRIter;
											ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + UnitarySystem( UnitarySysNum ).Name );
											ShowContinueError( "Estimated part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).HXAssistedSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = ReqOutput / FullOutput;
									if ( ! WarmupFlag ) {
										if ( UnitarySystem( UnitarySysNum ).HXAssistedSensPLRFail < 1 ) {
											++UnitarySystem( UnitarySysNum ).HXAssistedSensPLRFail;
											ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - DX unit sensible part-load ratio calculation unexpectedly failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - DX unit sensible part-load ratio calculation unexpectedly failed error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).HXAssistedSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = ReqOutput / FullOutput;
								if ( ! WarmupFlag ) {
									if ( UnitarySystem( UnitarySysNum ).HXAssistedSensPLRFail2 < 1 ) {
										++UnitarySystem( UnitarySysNum ).HXAssistedSensPLRFail2;
										ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).HXAssistedSensPLRFailIndex2, PartLoadFrac, PartLoadFrac );
								}
							}
							if ( CoilType_Num == CoilDX_CoolingHXAssisted ) UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( CoilType_Num == CoilDX_CoolingTwoSpeed ) {
							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// Par(3) is only needed for variable speed coils (see DXCoilVarSpeedResidual and DXCoilCyclingResidual)
							Par( 3 ) = UnitarySysNum;
							if ( SpeedRatio == 1.0 ) {
								SolveRegulaFalsi( Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0, 1.0, Par );
								PartLoadFrac = SpeedRatio;
							} else {
								SolveRegulaFalsi( Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0, 1.0, Par );
								PartLoadFrac = CycRatio;
							}

						} else if ( CoilType_Num == CoilDX_MultiSpeedCooling ) {

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							Par( 3 ) = double( UnitarySysNum );
							// Par(4) = CycRatio or SpeedRatio
							Par( 5 ) = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
							Par( 6 ) = 1.0; // UnitarySystem(UnitarySysNum)%FanOpMode
							Par( 7 ) = 1.0; // CompOp
							Par( 8 ) = ReqOutput;

							if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 1.0 ) {
								Par( 4 ) = CycRatio;
								SolveRegulaFalsi( Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0, 1.0, Par );
								UnitarySystem( UnitarySysNum ).CoolingCycRatio = SpeedRatio;
								UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = SpeedRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								PartLoadFrac = SpeedRatio;
							} else {
								SpeedRatio = 0.0;
								UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = SpeedRatio;
								Par( 4 ) = SpeedRatio;

								SolveRegulaFalsi( Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0, 1.0, Par );
								UnitarySystem( UnitarySysNum ).CoolingCycRatio = CycRatio;
								UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = CycRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								PartLoadFrac = CycRatio;
							}

						} else if ( ( CoilType_Num == Coil_CoolingAirToAirVariableSpeed ) || ( CoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							Par( 3 ) = double( UnitarySysNum );
							// Par(4) = CycRatio or SpeedRatio
							Par( 5 ) = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
							Par( 6 ) = double( FanOpMode );
							Par( 7 ) = 1.0; // CompOp
							Par( 8 ) = ReqOutput;

							if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum > 1.0 ) {
								Par( 4 ) = CycRatio;
								SolveRegulaFalsi( Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0, 1.0, Par );
								UnitarySystem( UnitarySysNum ).CoolingCycRatio = CycRatio;
								UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = SpeedRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = SpeedRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								PartLoadFrac = SpeedRatio;
							} else {
								UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = SpeedRatio;
								Par( 4 ) = SpeedRatio;
								SolveRegulaFalsi( Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0, 1.0, Par );
								UnitarySystem( UnitarySysNum ).CoolingCycRatio = CycRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = CycRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								PartLoadFrac = CycRatio;
							}

						} else if ( CoilType_Num == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// dehumidification mode = 0 for normal mode, 1+ for enhanced mode
							Par( 3 ) = double( DehumidMode );
							Par( 4 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
							UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( ( CoilType_Num == Coil_CoolingWater ) || ( CoilType_Num == Coil_CoolingWaterDetailed ) ) { // COIL:COOLING:WATER

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, CoolWaterTempResidual, 0.0, 1.0, Par );

						} else if ( ( CoilType_Num == Coil_CoolingWaterToAirHPSimple ) || ( CoilType_Num == Coil_CoolingWaterToAirHP ) ) {
							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							Par( 4 ) = ReqOutput;
							UnitarySystem ( UnitarySysNum ).CoolingCoilSensDemand = ReqOutput;
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, CoolWatertoAirHPTempResidual, 0.0, 1.0, Par );

						} else if ( CoilType_Num == Coil_UserDefined ) {
							// do nothing, user defined coil cannot be controlled

						} else if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling ) {

							Par( 1 ) = double( UnitarySysNum );
							Par( 2 ) = DesOutTemp;
							Par( 3 ) = 0.0; // DesOutHumRat; set to 0 if temp controlled
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, TESIceStorageCoilOutletResidual, 0.0, 1.0, Par );

						} else {
							ShowMessage( " For :" + UnitarySystem( UnitarySysNum ).UnitarySystemType + "=\"" + UnitarySystem( UnitarySysNum ).Name + "\"" );
							ShowFatalError( "ControlCoolingSystem: Invalid cooling coil type = " + cAllCoilTypes( CoilType_Num ) );

						}

					}
				}

				//     IF system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
				//     ELSE use operating humidity ratio to test against humidity setpoint
				if ( PartLoadFrac == 0.0 ) {
					OutletHumRatDXCoil = NoLoadHumRatOut;
				} else {
					OutletHumRatDXCoil = Node( OutletNode ).HumRat;
				}

				// IF humidity setpoint is not satisfied and humidity control type is MultiMode,
				// then enable heat exchanger and run to meet sensible load

				if ( ( OutletHumRatDXCoil > ( DesOutHumRat + HumRatAcc ) ) && ( PartLoadFrac < 1.0 ) && ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_Multimode ) ) {

					if ( CoilType_Num == CoilDX_CoolingHXAssisted ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted
						// Determine required part load when heat exchanger is ON
						HXUnitOn = true;
						PartLoadFrac = 1.0;
						SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, PartLoadFrac, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );

						OutletTempDXCoil = HXAssistedCoilOutletTemp( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );

						//               FullOutput will be different than the FullOutput determined above during sensible PLR calculations
						FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

						//   Check to see if the system can meet the load with the compressor off
						//   If NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
						if ( ( NoLoadTempOut - DesOutTemp ) < Acc ) {
							PartLoadFrac = 0.0;
							//          OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
							//          if this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac = 1.
							//            ELSEIF ((OutletTempDXCoil > DesOutTemp) .OR. ABS(OutletTempDXCoil - DesOutTemp) .LE. (Acc*2.0d0)) THEN
						} else if ( OutletTempDXCoil > DesOutTemp - ( Acc * 2.0 ) ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1.0 and FALSE = 0.0
							if ( FirstHVACIteration ) {
								Par( 3 ) = 1.0;
							} else {
								Par( 3 ) = 0.0;
							}
							if ( HXUnitOn ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par );
						}
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else if ( CoilType_Num == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
						// formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL)

						// Get full load result
						PartLoadFrac = 1.0;
						DehumidMode = 1;
						UnitarySystem( UnitarySysNum ).DehumidificationMode = DehumidMode;
						SimDXCoilMultiMode( CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode );
						FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

						// Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
						// Check that this is the case; IF not set PartLoadFrac = 0.0 (off) and return
						// Calculate the part load fraction
						if ( FullOutput >= 0 ) {
							PartLoadFrac = 0.0;
						} else {
							OutletTempDXCoil = DXCoilOutletTemp( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							OutletHumRatDXCoil = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							// If sensible load and setpoint cannot be met, set PLR = 1. if no sensible load and
							// latent load exists and setpoint cannot be met, set PLR = 1.
							// why is our logic different? Did we figure something out that reduced the logic?
							//                  IF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad .AND. &
							//                       OutletHumRatDXCoil >= DesOutHumRat)) THEN
							if ( ( OutletTempDXCoil > ( DesOutTemp - ( Acc * 2.0 ) ) && SensibleLoad && UnitarySystem( UnitarySysNum ).RunOnSensibleLoad ) || ( OutletHumRatDXCoil > ( DesOutHumRat - ( HumRatAcc * 2.0 ) ) && ! SensibleLoad && LatentLoad && UnitarySystem( UnitarySysNum ).RunOnLatentLoad ) ) {
								PartLoadFrac = 1.0;
								//                  ELSEIF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad .AND. &
								//                       OutletHumRatDXCoil < DesOutHumRat)) THEN
							} else if ( ! SensibleLoad && ( OutletHumRatDXCoil < DesOutHumRat && LatentLoad && UnitarySystem( UnitarySysNum ).RunOnLatentLoad ) ) {
								PartLoadFrac = ReqOutput / FullOutput;
								Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								Par( 2 ) = DesOutHumRat;
								// dehumidification mode = 0 for normal mode, 1+ for enhanced mode
								Par( 3 ) = double( DehumidMode );
								Par( 4 ) = double( FanOpMode );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par );
							} else { // must be a sensible load so find PLR
								PartLoadFrac = ReqOutput / FullOutput;
								Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								Par( 2 ) = DesOutTemp;
								// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
								Par( 3 ) = double( DehumidMode );
								Par( 4 ) = double( FanOpMode );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
							}
						}
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else {

					}
				} // END IF humidity ratio setpoint not met - Multimode humidity control

				// IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
				// then overcool to meet moisture load

				if ( ( OutletHumRatDXCoil > DesOutHumRat ) && ( PartLoadFrac < 1.0 ) && LatentLoad && ( UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat ) ) {

					//           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
					//           do not run the compressor
					if ( ( NoLoadHumRatOut - DesOutHumRat ) < HumRatAcc ) {
						//PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
						//           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
						//           run the compressor at PartLoadFrac = 1.
						//        ELSEIF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc) THEN
					} else if ( FullLoadHumRatOut > ( DesOutHumRat - HumRatAcc ) ) {
						PartLoadFrac = 1.0;
						//           ELSE find the PLR to meet the load

					} else {

						if ( CoilType_Num == CoilDX_CoolingSingleSpeed ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutHumRat;
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, DOE2DXCoilHumRatResidual, 0.0, 1.0, Par );
							UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( CoilType_Num == CoilDX_CoolingHXAssisted ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

							//               IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
							//               do not run the compressor
							if ( ( NoLoadHumRatOut - DesOutHumRat ) < HumRatAcc * 2.0 ) {
								//PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
								//                If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
								//                run the compressor at PartLoadFrac = 1.
							} else if ( ( DesOutHumRat - FullLoadHumRatOut ) < HumRatAcc * 2.0 ) {
								PartLoadFrac = 1.0;
								//               ELSE find the PLR to meet the load
							} else {
								Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								Par( 2 ) = DesOutHumRat;
								// FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
								if ( FirstHVACIteration ) {
									Par( 3 ) = 1.0;
								} else {
									Par( 3 ) = 0.0;
								}
								if ( HXUnitOn ) {
									Par( 4 ) = 1.0;
								} else {
									Par( 4 ) = 0.0;
								}
								Par( 5 ) = double( FanOpMode );
								SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, 0.0, 1.0, Par );
								if ( SolFla == -1 ) {

									//                   RegulaFalsi may not find latent PLR when the latent degradation model is used.
									//                   IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
									TempMaxPLR = -0.1;
									TempOutletHumRatDXCoil = OutletHumRatDXCoil;
									while ( ( OutletHumRatDXCoil - TempOutletHumRatDXCoil ) >= 0.0 && TempMaxPLR <= 1.0 ) {
										//                     find upper limit of LatentPLR
										TempMaxPLR += 0.1;
										SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMaxPLR, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
										OutletHumRatDXCoil = HXAssistedCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									}
									TempMinPLR = TempMaxPLR;
									while ( ( OutletHumRatDXCoil - TempOutletHumRatDXCoil ) <= 0.0 && TempMinPLR >= 0.0 ) {
										//                     pull upper limit of LatentPLR DOwn to last valid limit (i.e. latent output still exceeds SystemMoisuterLoad)
										TempMaxPLR = TempMinPLR;
										//                     find minimum limit of Latent PLR
										TempMinPLR -= 0.01;
										SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMaxPLR, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
										OutletHumRatDXCoil = HXAssistedCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									}
									//                   tighter boundary of solution has been found, CALL RegulaFalsi a second time
									SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, TempMinPLR, TempMaxPLR, Par );
									if ( SolFla == -1 ) {
										if ( ! WarmupFlag ) {
											if ( UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRIter < 1 ) {
												++UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRIter;
												ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + UnitarySystem( UnitarySysNum ).Name );
												ShowContinueError( "Estimated latent part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
												ShowContinueError( "Calculated latent part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
												ShowContinueErrorTimeStamp( "The calculated latent part-load ratio will be used and the simulation continues. Occurrence info:" );
											}
											ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.", UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
										}

									} else if ( SolFla == -2 ) {

										PartLoadFrac = ReqOutput / FullOutput;
										if ( ! WarmupFlag ) {
											if ( UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRFail < 1 ) {
												++UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRFail;
												ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - DX unit latent part-load ratio calculation failed unexpectedly: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
												ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
												ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
											}
											ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - DX unit latent part-load ratio calculation failed unexpectedly error continues. Latent PLR statistics follow.", UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRFailIndex, PartLoadFrac, PartLoadFrac );
										}
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = ReqOutput / FullOutput;
									if ( ! WarmupFlag ) {
										if ( UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRFail2 < 1 ) {
											++UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRFail2;
											ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.", UnitarySystem( UnitarySysNum ).HXAssistedCRLatPLRFailIndex2, PartLoadFrac, PartLoadFrac );
									}
								}
							}
							UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( CoilType_Num == CoilDX_CoolingTwoSpeed ) {

							//               Simulate MultiSpeed DX coil at sensible result
							SimDXCoilMultiSpeed( CompName, SpeedRatio, CycRatio, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );

							OutletHumRatDXCoil = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							// IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
							// then overcool to meet moisture load

							if ( OutletHumRatDXCoil > DesOutHumRat ) {

								CycRatio = 0.0;
								SpeedRatio = 0.0;

								SimDXCoilMultiSpeed( CompName, 0.0, 1.0, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								OutletHumRatLS = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								if ( OutletHumRatLS > DesOutHumRat ) {
									CycRatio = 1.0;
									SimDXCoilMultiSpeed( CompName, 1.0, 1.0, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									OutletHumRatHS = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									if ( OutletHumRatHS < DesOutHumRat ) {
										Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
										Par( 2 ) = DesOutHumRat;
										SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par );
									} else {
										SpeedRatio = 1.0;
									}
								} else {
									SpeedRatio = 0.0;
									Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									Par( 2 ) = DesOutHumRat;
									SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0, 1.0, Par );
								}

							}

						} else if ( ( CoilType_Num == CoilDX_MultiSpeedCooling ) || ( CoilType_Num == Coil_CoolingAirToAirVariableSpeed ) || ( CoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

							if ( CoilType_Num == CoilDX_MultiSpeedCooling ) {
								SimDXCoilMultiSpeed( CompName, SpeedRatio, CycRatio, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							} else {
								SimVariableSpeedCoils( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, CycRatio, SpeedNum, SpeedRatio, ReqOutput, dummy, OnOffAirFlowRatio );
							}

							OutletHumRatDXCoil = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							// IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
							// then overcool to meet moisture load

							if ( OutletHumRatDXCoil > DesOutHumRat ) {

								CycRatio = 0.0;
								SpeedRatio = 0.0;

								if ( CoilType_Num == CoilDX_MultiSpeedCooling ) {
									SimDXCoilMultiSpeed( CompName, 0.0, 1.0, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
								} else {
									SimVariableSpeedCoils( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, 1.0, SpeedNum, 1.0, ReqOutput, dummy, OnOffAirFlowRatio );
								}

								OutletHumRatLS = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );

								if ( OutletHumRatLS > DesOutHumRat ) {
									CycRatio = 1.0;
									if ( CoilType_Num == CoilDX_MultiSpeedCooling ) {
										SimDXCoilMultiSpeed( CompName, 1.0, 1.0, UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									} else {
										SimVariableSpeedCoils( CompName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, 1.0, SpeedNum, 1.0, ReqOutput, dummy, OnOffAirFlowRatio );
									}
									OutletHumRatHS = DXCoilOutletHumRat( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									if ( OutletHumRatHS < DesOutHumRat ) {
										Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
										Par( 2 ) = DesOutHumRat;
										Par( 3 ) = ReqOutput;
										SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par );
									} else {
										SpeedRatio = 1.0;
									}
								} else {
									SpeedRatio = 0.0;
									Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
									Par( 2 ) = DesOutHumRat;
									Par( 3 ) = ReqOutput;
									SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0, 1.0, Par );
								}

							}

						} else if ( CoilType_Num == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutHumRat;
							// dehumidification mode = 0 for normal mode, 1+ for enhanced mode
							Par( 3 ) = double( DehumidMode );
							Par( 4 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFlaLat, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par );
							UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( ( CoilType_Num == Coil_CoolingWater ) || ( CoilType_Num == Coil_CoolingWaterDetailed ) ) { // COIL:COOLING:WATER

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutHumRat;

							SolveRegulaFalsi( HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, CoolWaterHumRatResidual, 0.0, 1.0, Par );

						} else if ( ( CoilType_Num == Coil_CoolingWaterToAirHPSimple ) || ( CoilType_Num == Coil_CoolingWaterToAirHP ) ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutHumRat;
							Par( 4 ) = ReqOutput;

							SolveRegulaFalsi( HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, CoolWatertoAirHPHumRatResidual, 0.0, 1.0, Par );

						} else if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling ) {

							if ( CoilType_Num == CoilDX_PackagedThermalStorageCooling && ( UnitarySystem( UnitarySysNum ).TESOpMode != OffMode && UnitarySystem( UnitarySysNum ).TESOpMode != ChargeOnlyMode ) ) {
								Par( 1 ) = double( UnitarySysNum );
								Par( 2 ) = 0.0; // DesOutTemp; set to 0 if humrat controlled
								Par( 3 ) = DesOutHumRat;
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, TESIceStorageCoilOutletResidual, 0.0, 1.0, Par );
							}

						} else {

						}
					}
				}
			}
		}

		if ( SolFla == -1 ) {
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).SensPLRIter < 1 ) {
					++UnitarySystem( UnitarySysNum ).SensPLRIter;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - Iteration limit exceeded calculating part-load ratio for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
					ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).SensPLRIterIndex, PartLoadFrac, PartLoadFrac );
				}
			}
		} else if ( SolFla == -2 ) {
			PartLoadFrac = ReqOutput / FullOutput;
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).SensPLRFail < 1 ) {
					++UnitarySystem( UnitarySysNum ).SensPLRFail;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).SensPLRFailIndex, PartLoadFrac, PartLoadFrac );
				}
			}
		}

		if ( SolFlaLat == -1 && SolFla != -1 ) {
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).LatPLRIter < 1 ) {
					++UnitarySystem( UnitarySysNum ).LatPLRIter;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - Iteration limit exceeded calculating latent part-load ratio for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
					ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
				}
				ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.", UnitarySystem( UnitarySysNum ).LatPLRIterIndex, PartLoadFrac, PartLoadFrac );
			}
		} else if ( SolFlaLat == -2 && SolFla != -2 ) {
			//               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
			if ( NoLoadHumRatOut - FullLoadHumRatOut != 0.0 ) {
				PartLoadFrac = ( NoLoadHumRatOut - DesOutHumRat ) / ( NoLoadHumRatOut - FullLoadHumRatOut );
			} else {
				PartLoadFrac = 1.0;
			}
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).LatPLRFail < 1 ) {
					++UnitarySystem( UnitarySysNum ).LatPLRFail;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
				}
				ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - latent part-load ratio calculation failed error continues. Latent PLR statistics follow.", UnitarySystem( UnitarySysNum ).LatPLRFailIndex, PartLoadFrac, PartLoadFrac );
			}
		}
		//Set the final results

		if ( PartLoadFrac > 1.0 ) {
			PartLoadFrac = 1.0;
		} else if ( PartLoadFrac < 0.0 ) {
			PartLoadFrac = 0.0;
		}

		UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac = PartLoadFrac;
		UnitarySystem( UnitarySysNum ).CoolingSpeedRatio = SpeedRatio;
		UnitarySystem( UnitarySysNum ).CoolingCycRatio = CycRatio;
		UnitarySystem( UnitarySysNum ).DehumidificationMode = DehumidMode;

		LoopDXCoilRTF = max( LoopDXCoilRTF, LoopDXCoilMaxRTFSave );

	}

	void
	ControlHeatingSystem(
		int const UnitarySysNum, // index to Unitary System
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration, // First HVAC iteration flag
		int & CompOn // compressor on/off control
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Simulate the coil object at the required PLR.

		// METHODOLOGY EMPLOYED:
		//  Calculate operating PLR and adjust speed when using multispeed coils.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTdpFnWPb;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::DXCoilOutletHumRat;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using DataAirLoop::LoopHeatingCoilMaxRTF;
		using DataAirLoop::LoopDXCoilRTF;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using UserDefinedComponents::SimCoilUserDefined;

		// Locals
		bool const SuppHeatingCoilFlag( false );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.0e-3 ); // Accuracy of solver result

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of the heating coil
		int CompIndex; // Index to the heating coil
		//  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
		Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
		Real64 ReqOutput; // Sensible capacity (outlet - inlet) required to meet load or set point temperature
		int InletNode; // Inlet node number of the DX cooling coil
		int OutletNode; // Outlet node number of the DX cooling coil
		int ControlNode; // The node number where a set point is placed to control the DX cooling coil
		Real64 PartLoadFrac; // The part-load fraction of the compressor

		Real64 SpeedRatio; // SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
		//              (CompressorSpeedMax - CompressorSpeedMin)
		// for variable speed or 2 speed compressors
		int SpeedNum;
		Real64 CycRatio; // Cycling part-load ratio for variable speed or 2 speed compressors
		Real64 DesOutTemp; // Desired outlet temperature of the DX cooling coil

		int SolFla; // Flag of solver, num iterations if >0, else error index
		Array1D< Real64 > Par( 8 ); // Parameter array passed to solver
		bool SensibleLoad; // True if there is a sensible cooling load on this system
		bool LatentLoad; // True if there is a latent   cooling load on this system
		Real64 SensLoad; // turns on coil
		int FanOpMode; // Supply air fan operating mode
		Real64 LoopHeatingCoilMaxRTFSave; // Used to find RTF of heating coils without overwriting globabl variable
		Real64 LoopDXCoilMaxRTFSave; // Used to find RTF of DX heating coils without overwriting globabl variable
		Real64 dummy; // dummy variable for heating latent demand
		Real64 WSHPRuntimeFrac; // Run time fraction of water to air hp
		Real64 OutdoorDryBulb; // local variable for OutDryBulbTemp
		Real64 OutdoorHumRat; // local variable for OutHumRat
		Real64 OutdoorPressure; // local variable for OutBaroPress
		Real64 OutdoorWetBulb; // local variable for OutWetBulbTemp
		Real64 OutletTemp;
		bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil is wrapped by UnitarySystem.
		bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil is wrapped by UnitarySystem.

		// Set local variables
		// Retrieve the load on the controlled zone
		InletNode = UnitarySystem( UnitarySysNum ).HeatCoilInletNodeNum;
		OutletNode = UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum;
		ControlNode = UnitarySystem( UnitarySysNum ).SystemHeatControlNodeNum;
		DesOutTemp = UnitarySystem( UnitarySysNum ).DesiredOutletTemp;
		LoopHeatingCoilMaxRTFSave = LoopHeatingCoilMaxRTF;
		LoopHeatingCoilMaxRTF = 0.0;
		LoopDXCoilMaxRTFSave = LoopDXCoilRTF;
		LoopDXCoilRTF = 0.0;

		CompName = UnitarySystem( UnitarySysNum ).HeatingCoilName;
		CompIndex = UnitarySystem( UnitarySysNum ).HeatingCoilIndex;
		FanOpMode = UnitarySystem( UnitarySysNum ).FanOpMode;

		PartLoadFrac = 0.0;
		SpeedRatio = 0.0;
		CycRatio = 0.0;
		SpeedNum = 0;
		dummy = 0.0;
		SolFla = 0.0;
		SensibleLoad = false;
		SensLoad = 0.0;
		LatentLoad = false;

		if ( UnitarySystem( UnitarySysNum ).CondenserNodeNum != 0 ) {
			OutdoorDryBulb = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).Temp;
			if ( UnitarySystem( UnitarySysNum ).CondenserType == WaterCooled ) {
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			} else {
				OutdoorPressure = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).Press;
				// IF node is not connected to anything, pressure = default, use weather data
				if ( OutdoorPressure == DefaultNodeValues.Press ) {
					OutdoorDryBulb = OutDryBulbTemp;
					OutdoorHumRat = OutHumRat;
					OutdoorPressure = OutBaroPress;
					OutdoorWetBulb = OutWetBulbTemp;
				} else {
					OutdoorHumRat = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).HumRat;
					//     this should use Node%WetBulbTemp or a PSYC function, not OAWB
					OutdoorWetBulb = Node( UnitarySystem( UnitarySysNum ).CondenserNodeNum ).OutAirWetBulb;
				}
			}
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		// IF DXHeatingSystem is scheduled on and there is flow
		if ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr ) > 0.0 && GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr ) > 0.0 && Node( InletNode ).MassFlowRate > MinAirMassFlow && UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac == 0.0 ) {

			// Determine if there is a sensible load on this system
			//    IF((Node(InletNode)%Temp < Node(ControlNode)%TempSetPoint) .AND. &
			if ( ( Node( InletNode ).Temp < DesOutTemp ) && ( std::abs( Node( InletNode ).Temp - DesOutTemp ) > TempControlTol ) ) SensibleLoad = true;

			// IF DXHeatingSystem runs with a heating load then set PartLoadFrac on Heating System
			if ( SensibleLoad ) {

				ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( UnitarySystem( UnitarySysNum ).DesiredOutletTemp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

				// Get no load result
				PartLoadFrac = 0.0;
				WSHPRuntimeFrac = 0.0;
				CompOn = 0;

				{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

				if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // Coil:Heating:DX:SingleSpeed

					SimDXCoil( CompName, On, FirstHVACIteration, CompIndex, FanOpMode, PartLoadFrac );
					UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

				} else if ( SELECT_CASE_var == Coil_UserDefined ) { // do nothing, user defined coil cannot be controlled

					HeatingActive = false; // set to arbitrary value on entry to function
					CoolingActive = false; // set to arbitrary value on entry to function
					SimCoilUserDefined( CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive );
					if ( HeatingActive ) PartLoadFrac = 1.0;

				} else if ( ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) ||  ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) ) {

					SimMultiSpeedCoils( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil );

				} else if ( ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) ) {

					UnitarySystem( UnitarySysNum ).HeatingCoilSensDemand = ReqOutput;
					SimVariableSpeedCoils( "", UnitarySystem( UnitarySysNum ).HeatingCoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, CycRatio, SpeedNum, SpeedRatio, SensLoad, dummy );

				} else if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) || ( SELECT_CASE_var == Coil_HeatingDesuperheater ) ) {

					SimulateHeatingCoilComponents( CompName, FirstHVACIteration, 0.0, CompIndex, _, _, FanOpMode );

				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {

					SimWaterCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil );

				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {

					SimSteamCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil );
					if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow > 0.0 ) PartLoadFrac = min( 1.0, Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRate / UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow );

				} else if ( SELECT_CASE_var == Coil_HeatingWaterToAirHPSimple ) {

					if ( FirstHVACIteration ) UnitarySystem ( UnitarySysNum ).CompPartLoadRatio = 1;
					SimWatertoAirHPSimple( BlankString, CompIndex, ReqOutput, dummy, FanOpMode, UnitarySystem( UnitarySysNum ).CompPartLoadRatio, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 0, PartLoadFrac, FirstHVACIteration );
					UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;
					UnitarySystem ( UnitarySysNum ).HeatingCoilSensDemand = 0.0;

				} else if ( SELECT_CASE_var == Coil_HeatingWaterToAirHP ) {

					SimWatertoAirHP( BlankString, CompIndex, UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow, FanOpMode, FirstHVACIteration, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadFrac );
					UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

				} else {

				}}

				//     IF outlet temp at no load is within ACC of set point, do not run the coil
				if ( std::abs( Node( OutletNode ).Temp - DesOutTemp ) < Acc  || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_UserDefined) {
					// do nothing, coil is at the set point.
				} else if ( ( Node( OutletNode ).Temp - DesOutTemp ) > Acc ) { // IF outlet temp is above set point turn off coil
					PartLoadFrac = 0.0;
				} else { // ELSE get full load result

					// Get full load result
					PartLoadFrac = 1.0;
					WSHPRuntimeFrac = 1.0;
					CompOn = 1;

					{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

					if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // Coil:Heating:DX:SingleSpeed

						SimDXCoil( CompName, On, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, FanOpMode, PartLoadFrac );
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else if ( SELECT_CASE_var == Coil_UserDefined ) {

						//  should never get here, coil cannot be controlled and has already been simulated

					} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {

						CycRatio = 1.0;
						SpeedRatio = 0.0;
						for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedHeating; ++SpeedNum ) {
							if ( SpeedNum > 1 ) CycRatio = 0.0;
							if ( SpeedNum > 1 ) SpeedRatio = 1.0;
							UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
							SimMultiSpeedCoils( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil, SpeedNum );
							OutletTemp = Node( OutletNode ).Temp;
							if ( OutletTemp > DesOutTemp && SensibleLoad ) break;
						}

					} else if ( ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) ) {

						CycRatio = 1.0;
						SpeedRatio = 1.0;
						SensLoad = 1.0; // turns on coil
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = SpeedRatio;
						UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = PartLoadFrac;
						for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedHeating; ++SpeedNum ) {
							UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
							SimMultiSpeedCoils( UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil, SpeedNum );
							OutletTemp = Node( OutletNode ).Temp;
							SpeedRatio = double( SpeedNum ) - 1.0;
							if ( OutletTemp > DesOutTemp && SensibleLoad ) break;
						}

					} else if ( ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) ) {

						CycRatio = 1.0;
						SpeedRatio = 1.0;
						SensLoad = 1.0; // turns on coil
						UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = SpeedRatio;
						UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = PartLoadFrac;
						for ( SpeedNum = 1; SpeedNum <= UnitarySystem( UnitarySysNum ).NumOfSpeedHeating; ++SpeedNum ) {
							UnitarySystem( UnitarySysNum ).HeatingSpeedNum = SpeedNum;
							SimVariableSpeedCoils( "", UnitarySystem( UnitarySysNum ).HeatingCoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, CycRatio, SpeedNum, SpeedRatio, SensLoad, dummy );
							OutletTemp = Node( OutletNode ).Temp;
							if ( OutletTemp > DesOutTemp && SensibleLoad ) break;
						}

					} else if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {

						SimulateHeatingCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).DesignHeatingCapacity, CompIndex, _, _, FanOpMode );

					} else if ( SELECT_CASE_var == Coil_HeatingDesuperheater ) {

						SimulateHeatingCoilComponents( CompName, FirstHVACIteration, ReqOutput, CompIndex, _, _, FanOpMode );

					} else if ( SELECT_CASE_var == Coil_HeatingWater ) {

						SimWaterCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil );

					} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {

						SimSteamCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil );
						if ( UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow > 0.0 ) PartLoadFrac = min( 1.0, Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRate / UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow );

					} else if ( SELECT_CASE_var == Coil_HeatingWaterToAirHPSimple ) {

						SimWatertoAirHPSimple( BlankString, CompIndex, ReqOutput, dummy, FanOpMode, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, PartLoadFrac, FirstHVACIteration );
						UnitarySystem ( UnitarySysNum ).HeatingCoilSensDemand = ReqOutput;
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else if ( SELECT_CASE_var == Coil_HeatingWaterToAirHP ) {
						SimWatertoAirHP( BlankString, CompIndex, UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow, FanOpMode, FirstHVACIteration, WSHPRuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadFrac );
						UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

					} else {

					}}

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

					//       If the outlet temp is within ACC of set point,
					if ( std::abs( Node( OutletNode ).Temp - DesOutTemp ) < Acc || UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_UserDefined) {
						// do nothing, coil is at set point
					} else if ( Node( OutletNode ).Temp < ( DesOutTemp - Acc ) ) { // IF outlet temp is below set point coil must be on
						PartLoadFrac = 1.0;
					} else { // ELSE find the PLR to meet the set point

						{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

						if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // Coil:Heating:DX:SingleSpeed

							Par( 1 ) = double( CompIndex );
							Par( 2 ) = DesOutTemp;
							Par( 3 ) = 1.0; //OnOffAirFlowFrac assume = 1.0 for continuous fan dx system
							Par( 5 ) = double( FanOpMode ); // this does nothing since set point based control requires constant fan
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, DXHeatingCoilResidual, 0.0, 1.0, Par );
							UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadFrac;

						} else if ( ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) || ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) || ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) ) {

							Par( 1 ) = double( UnitarySystem( UnitarySysNum ).HeatingCoilIndex );
							Par( 2 ) = DesOutTemp;
							Par( 3 ) = UnitarySysNum;
							// Par(4) = CycRatio or SpeedRatio
							Par( 5 ) = UnitarySystem( UnitarySysNum ).HeatingSpeedNum;
							Par( 6 ) = double( FanOpMode );
							Par( 7 ) = 1.0; // UnitarySystem(UnitarySysNum)%CompOp
							Par( 8 ) = ReqOutput;
							if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum > 1.0 ) {
								Par( 4 ) = CycRatio;
								SolveRegulaFalsi( Acc, MaxIte, SolFla, SpeedRatio, HeatingCoilVarSpeedResidual, 0.0, 1.0, Par );
								UnitarySystem( UnitarySysNum ).HeatingCycRatio = CycRatio;
								UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = SpeedRatio;
								UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = SpeedRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								PartLoadFrac = SpeedRatio;
							} else {
								SpeedRatio = 0.0;
								UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = SpeedRatio;
								Par( 4 ) = SpeedRatio;
								SolveRegulaFalsi( Acc, MaxIte, SolFla, CycRatio, HeatingCoilVarSpeedCycResidual, 0.0, 1.0, Par );
								UnitarySystem( UnitarySysNum ).HeatingCycRatio = CycRatio;
								UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = CycRatio;
								CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );
								PartLoadFrac = CycRatio;
							}

						} else if ( SELECT_CASE_var == Coil_HeatingGas ) {

							SimulateHeatingCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, ReqOutput, CompIndex, _, true, FanOpMode, PartLoadFrac );
							PartLoadFrac = ReqOutput / FullOutput;

						} else if ( ( SELECT_CASE_var == Coil_HeatingElectric ) || ( SELECT_CASE_var == Coil_HeatingDesuperheater ) ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							if ( SuppHeatingCoilFlag ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = FanOpMode;
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, GasElecHeatingCoilResidual, 0.0, 1.0, Par );

						} else if ( SELECT_CASE_var == Coil_HeatingWater ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							if ( SuppHeatingCoilFlag ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = 0.0;
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HotWaterHeatingCoilResidual, 0.0, 1.0, Par );

						} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							if ( SuppHeatingCoilFlag ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}

							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, SteamHeatingCoilResidual, 0.0, 1.0, Par );

						} else if ( ( SELECT_CASE_var == Coil_HeatingWaterToAirHPSimple ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHP ) ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							Par( 4 ) = ReqOutput;
							UnitarySystem ( UnitarySysNum ).HeatingCoilSensDemand = ReqOutput;

							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HeatWatertoAirHPTempResidual, 0.0, 1.0, Par );

						} else if ( SELECT_CASE_var == Coil_UserDefined ) {

							// should never get here, user defined coil cannot be controlled and has already been simulated

						} else {
							ShowMessage( " For :" + UnitarySystem( UnitarySysNum ).UnitarySystemType + "=\"" + UnitarySystem( UnitarySysNum ).Name + "\"" );
							ShowFatalError( "ControlHeatingSystem: Invalid heating coil type = " + cAllCoilTypes( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num ) );

						}}
					}
				}
			}
		}

		if ( PartLoadFrac > 1.0 ) {
			PartLoadFrac = 1.0;
		} else if ( PartLoadFrac < 0.0 ) {
			PartLoadFrac = 0.0;
		}

		if ( SolFla == -1 ) {
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).HeatCoilSensPLRIter < 1 ) {
					++UnitarySystem( UnitarySysNum ).HeatCoilSensPLRIter;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - Iteration limit exceeded calculating sensible part-load ratio for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
					ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).HeatCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
				}
			}
		} else if ( SolFla == -2 ) {
			PartLoadFrac = ReqOutput / FullOutput;
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).HeatCoilSensPLRFail < 1 ) {
					++UnitarySystem( UnitarySysNum ).HeatCoilSensPLRFail;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).HeatCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
				}
			}
		}

		//Set the final results
		UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac = PartLoadFrac;
		UnitarySystem( UnitarySysNum ).HeatingSpeedRatio = SpeedRatio;
		UnitarySystem( UnitarySysNum ).HeatingCycRatio = CycRatio;

		LoopHeatingCoilMaxRTF = max( LoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave );
		LoopDXCoilRTF = max( LoopDXCoilRTF, LoopDXCoilMaxRTFSave );

	}

	void
	ControlSuppHeatSystem(
		int const UnitarySysNum, // index to Unitary System
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration // First HVAC iteration flag
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine updates the System outlet nodes.

		// METHODOLOGY EMPLOYED:
		//  Data is moved from the System data structure to the System outlet nodes.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataAirLoop::LoopHeatingCoilMaxRTF;
		using DataAirLoop::LoopDXCoilRTF;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTdpFnWPb;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using UserDefinedComponents::SimCoilUserDefined;

		// Locals
		const bool SuppHeatingCoilFlag( true );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.0e-3 ); // Accuracy of solver result
		int const SolveMaxIter( 50 );

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of the heating coil
		int CompIndex; // Index to the heating coil
		//  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
		Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
		Real64 ReqOutput; // Sensible capacity (outlet - inlet) required to meet load or set point temperature
		int InletNode; // Inlet node number of the DX cooling coil
		int OutletNode; // Outlet node number of the DX cooling coil
		int ControlNode; // The node number where a set point is placed to control the DX cooling coil
		Real64 PartLoadFrac; // The part-load fraction of the compressor

		Real64 DesOutTemp; // Desired outlet temperature of the DX cooling coil
		Real64 QCoilActual; // Heating coil operating capacity [W]

		int SolFla; // Flag of solver, num iterations if >0, else error index
		Array1D< Real64 > Par( 5 ); // Parameter array passed to solver
		bool SensibleLoad; // True if there is a sensible cooling load on this system
		int FanOpMode; // Supply air fan operating mode
		Real64 LoopHeatingCoilMaxRTFSave; // Used to find RTF of heating coils without overwriting globabl variable
		Real64 LoopDXCoilMaxRTFSave; // Used to find RTF of DX heating coils without overwriting globabl variable
		Real64 NoLoadTempOut; // save outlet temp when coil is off (C)
		bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
		bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.

		// Set local variables
		// Retrieve the load on the controlled zone

		OutletNode = UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode;
		InletNode = UnitarySystem( UnitarySysNum ).SuppCoilAirInletNode;
		ControlNode = UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode;
		DesOutTemp = UnitarySystem( UnitarySysNum ).DesiredOutletTemp;
		CompName = UnitarySystem( UnitarySysNum ).SuppHeatCoilName;
		CompIndex = UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex;
		FanOpMode = UnitarySystem( UnitarySysNum ).FanOpMode;
		SolFla = 0.0;

		PartLoadFrac = 0.0;

		SensibleLoad = false;

		LoopHeatingCoilMaxRTFSave = LoopHeatingCoilMaxRTF;
		LoopHeatingCoilMaxRTF = 0.0;
		LoopDXCoilMaxRTFSave = LoopDXCoilRTF;
		LoopDXCoilRTF = 0.0;

		if ( ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr ) > 0.0 ) && ( Node( InletNode ).MassFlowRate > MinAirMassFlow ) ) {

			// Determine if there is a sensible load on this system
			if ( ( Node( InletNode ).Temp < DesOutTemp ) && ( std::abs( Node( InletNode ).Temp - DesOutTemp ) > TempControlTol ) ) SensibleLoad = true;

			if ( SensibleLoad ) {

				ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( UnitarySystem( UnitarySysNum ).DesiredOutletTemp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

				// Get no load result
				PartLoadFrac = 0.0;

				{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num );

				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) || ( SELECT_CASE_var == Coil_HeatingDesuperheater ) ) {
					SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, CompIndex, QCoilActual, SuppHeatingCoilFlag, FanOpMode, PartLoadFrac ); // QCoilReq= 0.0d0,  &
					PartLoadFrac = QCoilActual / UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity;

				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {

					SimWaterCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil );

				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {

					SimSteamCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil );

				} else if ( SELECT_CASE_var == Coil_UserDefined ) { // do nothing, user defined coil cannot be controlled
					HeatingActive = false; // set to arbitrary value on entry to function
					CoolingActive = false; // set to arbitrary value on entry to function
					SimCoilUserDefined( CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive );
					if ( HeatingActive ) PartLoadFrac = 1.0;

				} else {

				}}

				NoLoadTempOut = Node( OutletNode ).Temp;
				//      NoOutput = Node(InletNode)%MassFlowRate *  &
				//                       (PsyHFnTdbW(NoLoadTempOut,Node(OutletNode)%HumRat)  &
				//                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

				//     If OutletTemp is within ACC of set point, either coil operated or is not needed
				if ( std::abs( Node( OutletNode ).Temp - DesOutTemp ) < Acc || UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num == Coil_UserDefined ) {
					// do nothing, coil is at set point (i.e., gas/elec/steam/user coil will try to hit set point
				} else if ( PartLoadFrac > 0.0 ) {
					// do nothing, coil tried to hit set point (i.e., gas/elec/steam/user coil tried to hit set point but missed
				} else if ( NoLoadTempOut > ( DesOutTemp - Acc ) ) {
					PartLoadFrac = 0.0; // outlet temp > set point, coil is not needed
				} else { // outlet temp too low, turn on coil

					// Get full load result
					PartLoadFrac = 1.0;

					{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num );

					if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {

						//                                  CompIndex=CompIndex, QCoilReq= UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity,  &
						SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, CompIndex, QCoilActual, SuppHeatingCoilFlag, FanOpMode, PartLoadFrac );
						PartLoadFrac = QCoilActual / UnitarySystem( UnitarySysNum ).DesignSuppHeatingCapacity;

					} else if ( SELECT_CASE_var == Coil_HeatingDesuperheater ) {

						SimulateHeatingCoilComponents( CompName, FirstHVACIteration, ReqOutput, CompIndex, _, SuppHeatingCoilFlag, FanOpMode );

					} else if ( SELECT_CASE_var == Coil_HeatingWater ) {

						SimWaterCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil );

					} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {

						SimSteamCoils( UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil );

					} else if ( SELECT_CASE_var == Coil_UserDefined ) {

						//  should never get here, coil has already been simulated

					} else {

					}}

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

					//         If the FullOutput outlet temp is less than (insufficient heating) or very near set point,
					//         run the coil at PartLoadFrac = 1.
					if ( Node( OutletNode ).Temp < ( DesOutTemp + Acc ) ) {
						PartLoadFrac = 1.0;
					} else {

						{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).SuppHeatCoilType_Num );

						if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) || ( SELECT_CASE_var == Coil_HeatingDesuperheater ) ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							if ( SuppHeatingCoilFlag ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, GasElecHeatingCoilResidual, 0.0, 1.0, Par );

						} else if ( SELECT_CASE_var == Coil_HeatingWater ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							if ( SuppHeatingCoilFlag ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = 0.0;
							SolveRegulaFalsi( Acc, SolveMaxIter, SolFla, PartLoadFrac, HotWaterHeatingCoilResidual, 0.0, 1.0, Par );

						} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {

							Par( 1 ) = double( UnitarySysNum );
							if ( FirstHVACIteration ) {
								Par( 2 ) = 1.0;
							} else {
								Par( 2 ) = 0.0;
							}
							Par( 3 ) = DesOutTemp;
							if ( SuppHeatingCoilFlag ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}

							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, SteamHeatingCoilResidual, 0.0, 1.0, Par );

						} else if ( SELECT_CASE_var == Coil_UserDefined ) {

							//  do nothing, coil has already been simulated

						} else {

						}}

					} // IF ((FullOutput - ReqOutput) < Acc) THEN
				} // IF ((NoOutput-ReqOutput) > Acc) THEN
			} // IF (SensibleLoad ) THEN
		} // IF((GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) > 0.0d0) .AND. &

		if ( PartLoadFrac > 1.0 ) {
			PartLoadFrac = 1.0;
		} else if ( PartLoadFrac < 0.0 ) {
			PartLoadFrac = 0.0;
		}

		if ( SolFla == -1 ) {
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilSensPLRIter < 1 ) {
					++UnitarySystem( UnitarySysNum ).SuppHeatCoilSensPLRIter;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - Iteration limit exceeded calculating sensible part-load ratio for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
					ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).SuppHeatCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
				}
			} // IF(.NOT. WarmupFlag)THEN
		} else if ( SolFla == -2 ) {
			PartLoadFrac = ReqOutput / FullOutput;
			if ( ! WarmupFlag ) {
				if ( UnitarySystem( UnitarySysNum ).SuppHeatCoilSensPLRFail < 1 ) {
					++UnitarySystem( UnitarySysNum ).SuppHeatCoilSensPLRFail;
					ShowWarningError( UnitarySystem( UnitarySysNum ).UnitarySystemType + " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + UnitarySystem( UnitarySysNum ).Name );
					ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
					ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( UnitarySystem( UnitarySysNum ).UnitarySystemType + " \"" + UnitarySystem( UnitarySysNum ).Name + "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", UnitarySystem( UnitarySysNum ).SuppHeatCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
				}
			} // IF(.NOT. WarmupFlag)THEN
		} // IF (SolFla == -1) THEN

		UnitarySystem( UnitarySysNum ).SuppHeatPartLoadFrac = PartLoadFrac;

		LoopHeatingCoilMaxRTF = max( LoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave );
		LoopDXCoilRTF = max( LoopDXCoilRTF, LoopDXCoilMaxRTFSave );

	}

	void
	SimWaterCoils(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadFrac,
		int const CoilType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages water cooling/heating coil simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		Real64 mdot;

		if ( CoilType == CoolingCoil ) {
			CompName = UnitarySystem( UnitarySysNum ).CoolingCoilName;

			mdot = min( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = mdot;

			SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );

		} else if ( CoilType == HeatingCoil ) {
			CompName = UnitarySystem( UnitarySysNum ).HeatingCoilName;

			mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;

			SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );

		} else {
			CompName = UnitarySystem( UnitarySysNum ).SuppHeatCoilName;

			mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;

			SimulateWaterCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );
		}

	}

	void
	SimSteamCoils(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadFrac,
		int const CoilType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages steam heating coil simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using SteamCoils::SimulateSteamCoilComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		Real64 mdot;

		if ( CoilType == HeatingCoil ) {

			CompName = UnitarySystem( UnitarySysNum ).HeatingCoilName;

			mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;

			SimulateSteamCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, 1.0, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac ); //QCoilReq, simulate any load > 0 to get max capacity

		} else {

			CompName = UnitarySystem( UnitarySysNum ).SuppHeatCoilName;

			mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;

			SimulateSteamCoilComponents( CompName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, 1.0, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac ); //QCoilReq, simulate any load > 0 to get max capacity

		}

	}

	void
	SimMultiSpeedCoils(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // Index to air loop
		bool const FirstHVACIteration, // True when first HVAC iteration
		int & CompOn, // compresor on/off control
		bool const SensibleLoad,
		bool const LatentLoad,
		Real64 const PartLoadFrac,
		int const CoilType,
		Optional_int_const SpeedNumber
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages multispeed and variable speed cooling coil simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::DXCoilOutletTemp;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using HeatingCoils::SimulateHeatingCoilComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of Unitary System object
		Real64 dummy;
		Real64 SensLoad;
		Real64 LatLoad;
		Real64 OnOffAirFlowRatio;
		int CoilTypeNum;
		int SpeedNum;
		int CoilOutletNodeNum;
		int CompIndex;
		Real64 SpeedRatio;
		Real64 CycRatio;

		dummy = 0.0;

		if ( present( SpeedNumber ) ) {
			SpeedNum = SpeedNumber;
		} else {
			SpeedNum = 1;
		}

		if ( CoilType == CoolingCoil ) {

			CompName = UnitarySystem( UnitarySysNum ).CoolingCoilName;
			CompIndex = UnitarySystem( UnitarySysNum ).CoolingCoilIndex;
			CoilTypeNum = UnitarySystem( UnitarySysNum ).CoolingCoilType_Num;
			CoilOutletNodeNum = UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum;
			if ( SensibleLoad ) {
				SensLoad = -1.0;
				CoolingLoad = true;
				HeatingLoad = false;
			}
			if ( LatentLoad ) LatLoad = -1.0;

		} else {

			CompName = UnitarySystem( UnitarySysNum ).HeatingCoilName;
			CompIndex = UnitarySystem( UnitarySysNum ).HeatingCoilIndex;
			CoilTypeNum = UnitarySystem( UnitarySysNum ).HeatingCoilType_Num;
			CoilOutletNodeNum = UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum;

			if ( SensibleLoad ) {
				SensLoad = 1.0;
				CoolingLoad = false;
				HeatingLoad = true;
			} else {
				SensLoad = 0.0;
				HeatingLoad = false;
			}
			LatLoad = 0.0;
			UnitarySystem( UnitarySysNum ).FanOpMode = 1; // why is this here?

		}

		OnOffAirFlowRatio = 1.0;
		SetOnOffMassFlowRate( UnitarySysNum, OnOffAirFlowRatio, PartLoadFrac ); //1.0d0 = PartLoadRatio

		CalcPassiveSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration );

		if ( ( CoilTypeNum == CoilDX_MultiSpeedCooling ) || ( CoilTypeNum == CoilDX_MultiSpeedHeating ) ) {

			if ( CoilType == Cooling ) {
				if ( UnitarySystem( UnitarySysNum ).CoolingSpeedNum <= 1.0 ) {
					SpeedRatio = 0.0;
					CycRatio = PartLoadFrac;
				} else {
					if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
						SpeedRatio = PartLoadFrac;
						CycRatio = 0.0;
					} else {
						SpeedRatio = 1.0;
						CycRatio = PartLoadFrac;
					}
				}
			} else {
				if ( UnitarySystem( UnitarySysNum ).HeatingSpeedNum <= 1.0 ) {
					SpeedRatio = 0.0;
					CycRatio = PartLoadFrac;
				} else {
					if ( UnitarySystem( UnitarySysNum ).SingleMode == 0 ) {
						SpeedRatio = PartLoadFrac;
						CycRatio = 0.0;
					} else {
						SpeedRatio = 1.0;
						CycRatio = PartLoadFrac;
					}
				}
			}
			SimDXCoilMultiSpeed( CompName, 0.0, PartLoadFrac, CompIndex, SpeedNum, UnitarySystem( UnitarySysNum ).FanOpMode, 1, UnitarySystem( UnitarySysNum ).SingleMode );

		} else if ( CoilTypeNum == Coil_CoolingAirToAirVariableSpeed ) {

			SimVariableSpeedCoils( CompName, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, PartLoadFrac, SpeedNum, UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

		} else if ( CoilTypeNum == Coil_HeatingAirToAirVariableSpeed ) {

			SimVariableSpeedCoils( CompName, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, PartLoadFrac, SpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

		} else if ( CoilTypeNum == Coil_CoolingWaterToAirHPVSEquationFit ) {

			SimVariableSpeedCoils( CompName, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, PartLoadFrac, SpeedNum, UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

		} else if ( CoilTypeNum == Coil_HeatingWaterToAirHPVSEquationFit ) {

			SimVariableSpeedCoils( CompName, CompIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOn, PartLoadFrac, SpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

		} else if ( ( CoilTypeNum == Coil_HeatingElectric_MultiStage ) || ( CoilTypeNum == Coil_HeatingGas_MultiStage ) ) {

			SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, CompIndex, _, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac, SpeedNum, 0.0 );
		} else {

		}

	}

	void
	CalcPassiveSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		int const AirLoopNum, // index to air loop
		bool const FirstHVACIteration // True when first HVAC iteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the set point based output of the unitary system.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PartLoadRatio; // coil operating part-load ratio
		Real64 OnOffAirFlowRatio; // Setpoint based coil control does not use this variable
		Real64 CoilCoolHeatRat; // ratio of cooling to heating PLR for cycling fan RH control
		Real64 QZnReq;
		int CompOn; // compressor control (0=off, 1=on)
		bool HXUnitOn;

		OnOffAirFlowRatio = 1.0;
		CoilCoolHeatRat = 1.0;
		QZnReq = 0.0;
		//CALL the series of components that simulate a Unitary System
		if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );
		}

		if ( UnitarySystem( UnitarySysNum ).CoolingCoilUpstream ) {

			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				PartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				HXUnitOn = false;
				CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
			}
			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				PartLoadRatio = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio );
			}

		} else {

			if ( UnitarySystem( UnitarySysNum ).HeatCoilExists ) {
				PartLoadRatio = UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				CalcUnitaryHeatingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio );
			}
			if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
				PartLoadRatio = UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac;
				CompOn = 0;
				if ( PartLoadRatio > 0.0 ) CompOn = 1;
				HXUnitOn = false;
				CalcUnitaryCoolingSystem( UnitarySysNum, AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn );
			}

		}

		if ( UnitarySystem( UnitarySysNum ).FanExists && UnitarySystem( UnitarySysNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( BlankString, FirstHVACIteration, UnitarySystem( UnitarySysNum ).FanIndex, FanSpeedRatio );
		}

		// CALL reheat coils next
		if ( UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
			SuppHeatingCoilFlag = true;
			CalcUnitarySuppSystemToSP( UnitarySysNum, FirstHVACIteration );
			SuppHeatingCoilFlag = false;
		}

	}

	void
	SetOnOffMassFlowRate(
		int const UnitarySysNum, // index to unitary system
		Real64 & OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
		Real64 const PartLoadRatio // coil part-load ratio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the components.

		// METHODOLOGY EMPLOYED:
		// The unitarysystem may have alternate air flow rates
		// in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
		// air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
		// based on PLR.

		// REFERENCES:
		// Based on SetOnOffMassFlowRate by Richard Raustad

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatSpeedNum;
		int CoolSpeedNum;

		CompOffMassFlow = 0.0;
		CompOffFlowRatio = 0.0;

		// Set the compressor or coil ON mass flow rate
		if ( HeatingLoad ) {

			UnitarySystem( UnitarySysNum ).LastMode = HeatingMode;

			if ( MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) {

				HeatSpeedNum = UnitarySystem( UnitarySysNum ).HeatingSpeedNum;

				if ( HeatSpeedNum == 0 ) {
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
				} else if ( HeatSpeedNum == 1 ) {
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
				} else if ( HeatSpeedNum > 1 ) {
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum );
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( HeatSpeedNum );
				}
				// Set the compressor or coil OFF mass flow rate based on LOGICAL flag
				// UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
				if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
					if ( MoistureLoad < 0.0 && UnitarySystem( UnitarySysNum ).Humidistat && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat ) {
						if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
							CoolSpeedNum = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
							if ( CoolSpeedNum < 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
							} else if ( CoolSpeedNum == 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
							} else if ( CoolSpeedNum > 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum - 1 );
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum - 1 );
							}
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
						}
					} else {
						if ( HeatSpeedNum == 0 ) {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
						} else if ( HeatSpeedNum == 1 ) {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum );
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum );
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum - 1 );
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( HeatSpeedNum - 1 );
						}
					}
				} else {
					if ( HeatSpeedNum < 1 ) {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
					} else if ( HeatSpeedNum == 1 ) {
						if ( UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil ) {
							CompOffMassFlow = 0.0; // #5518
							CompOffFlowRatio = 0.0;
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum );
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum );
						}
					} else {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum - 1 );
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( HeatSpeedNum - 1 );
					}
				}
			} else { // IF(MultiOrVarSpeedHeatCoil) THEN
				//   If a heating and moisture load exists, operate at the cooling mass flow rate ELSE operate at the heating flow rate
				if ( MoistureLoad < 0.0 && UnitarySystem( UnitarySysNum ).Humidistat && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat && ! UnitarySystem( UnitarySysNum ).DXHeatingCoil ) {
					if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
						CoolSpeedNum = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
						if ( CoolSpeedNum < 1 ) {
							CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
							CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
						} else if ( CoolSpeedNum == 1 ) {
							CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
							CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
						} else {
							CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
							CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum );
						}
					} else { // IF (MultiOrVarSpeedCoolCoil) THEN
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
						CompOnFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
						if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
						}
					}
				} else { // Heating load but no moisture load
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow;
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio;
					if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
						if ( UnitarySystem( UnitarySysNum ).AirFlowControl == UseCompressorOnFlow ) {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio;
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio;
						}
					}
				}
			}

			// If a cooling load exists, operate at the cooling mass flow rate
		} else if ( CoolingLoad ) {

			UnitarySystem( UnitarySysNum ).LastMode = CoolingMode;

			if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {

				CoolSpeedNum = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;

				if ( CoolSpeedNum == 0 ) {
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
				} else if ( CoolSpeedNum == 1 ) {
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
				} else if ( CoolSpeedNum > 1 ) {
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum );
				}
				// Set the compressor or coil OFF mass flow rate based on LOGICAL flag
				// UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
				//    IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
				//      IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
				if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
					if ( CoolSpeedNum == 0 ) {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
					} else if ( CoolSpeedNum == 1 ) {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
					} else {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum - 1 );
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum - 1 );
					}
				} else {
					if ( CoolSpeedNum < 1 ) {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
					} else if ( CoolSpeedNum == 1 ) {
						if ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil ) {
							CompOffMassFlow = 0.0; // #5518
							CompOffFlowRatio = 0.0;
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
						}
					} else {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum - 1 );
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum - 1 );
					}
				}
			} else { // IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
				CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
				CompOnFlowRatio = UnitarySystem( UnitarySysNum ).CoolingSpeedRatio;
				if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
					if ( UnitarySystem( UnitarySysNum ).AirFlowControl == UseCompressorOnFlow ) {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
					} else {
						CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
						CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
					}
				}
			}

		} else { // No load
			// If no load exists, set the compressor on mass flow rate.
			// Set equal the mass flow rate when no heating or cooling is needed If no moisture load exists.
			// If the user has set the off mass flow rate to 0, set according to the last operating mode.

			if ( MoistureLoad < 0.0 && UnitarySystem( UnitarySysNum ).Humidistat && UnitarySystem( UnitarySysNum ).DehumidControlType_Num == DehumidControl_CoolReheat ) {
				if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
					CoolSpeedNum = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
					if ( CoolSpeedNum < 1 ) {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
						CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
					} else if ( CoolSpeedNum == 1 ) {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
						CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
					} else {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
						CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum );
					}

					if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
						if ( UnitarySystem( UnitarySysNum ).AirFlowControl == UseCompressorOnFlow ) {
							if ( CoolSpeedNum <= 1 ) {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
							} else {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum - 1 );
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum - 1 );
							}
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
						}
					}

				} else { // IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
					CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
					CompOnFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
					if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
						if ( UnitarySystem( UnitarySysNum ).AirFlowControl == UseCompressorOnFlow ) {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
						} else {
							CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
							CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
						}
					}
				}

			} else { // No Moisture Load

				if ( UnitarySystem( UnitarySysNum ).LastMode == HeatingMode ) {
					if ( MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
						CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
					} else {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
						CompOnFlowRatio = 1.0;
					}
				} else {
					if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
						CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
					} else {
						CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
						CompOnFlowRatio = 1.0;
					}
				}
				if ( CompOnMassFlow == 0.0 ) {
					if ( UnitarySystem( UnitarySysNum ).LastMode == HeatingMode ) {
						if ( MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) {
							HeatSpeedNum = UnitarySystem( UnitarySysNum ).HeatingSpeedNum;
							if ( HeatSpeedNum == 0 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
							} else if ( HeatSpeedNum == 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
								CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
							} else if ( HeatSpeedNum > 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum );
								CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( HeatSpeedNum );
							}
						} else { // IF(MultiOrVarSpeedHeatCoil) THEN
							CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow;
							CompOnFlowRatio = UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio;
						}
					} else { // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
						if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
							CoolSpeedNum = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
							if ( CoolSpeedNum == 0 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOnFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
							} else if ( CoolSpeedNum == 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
								CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
							} else if ( CoolSpeedNum > 1 ) {
								CompOnMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum );
								CompOnFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum );
							}
						} else { // IF(MultiOrVarSpeedCoolCoil) THEN
							CompOnMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
							CompOnFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
						} // IF(MultiOrVarSpeedCoolCoil) THEN
					} // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
				} // IF(CompOnMassFlow .EQ. 0.0d0)THEN

				if ( UnitarySystem( UnitarySysNum ).FanOpMode == ContFanCycCoil ) {
					if ( UnitarySystem( UnitarySysNum ).AirFlowControl == UseCompressorOnFlow ) {
						if ( UnitarySystem( UnitarySysNum ).LastMode == HeatingMode ) {
							if ( MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) {
								HeatSpeedNum = UnitarySystem( UnitarySysNum ).HeatingSpeedNum;
								if ( HeatSpeedNum < 1 ) {
									CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
									CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
								} else if ( HeatSpeedNum == 1 ) {
									CompOffMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( 1 );
									CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( 1 );
								} else {
									CompOffMassFlow = UnitarySystem( UnitarySysNum ).HeatMassFlowRate( HeatSpeedNum - 1 );
									CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSHeatingSpeedRatio( HeatSpeedNum - 1 );
								}
							} else {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio;
							}
						} else { // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
							if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
								CoolSpeedNum = UnitarySystem( UnitarySysNum ).CoolingSpeedNum;
								if ( CoolSpeedNum < 1 ) {
									CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
									CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
								} else if ( CoolSpeedNum == 1 ) {
									CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( 1 );
									CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( 1 );
								} else {
									CompOffMassFlow = UnitarySystem( UnitarySysNum ).CoolMassFlowRate( CoolSpeedNum - 1 );
									CompOffFlowRatio = UnitarySystem( UnitarySysNum ).MSCoolingSpeedRatio( CoolSpeedNum - 1 );
								}
							} else {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
							}
						} // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
					} else { // IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
						if ( UnitarySystem( UnitarySysNum ).LastMode == HeatingMode ) {
							if ( MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
							} else {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).HeatingFanSpeedRatio;
							}
						} else {
							if ( MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).IdleMassFlowRate;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).IdleSpeedRatio;
							} else {
								CompOffMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
								CompOffFlowRatio = UnitarySystem( UnitarySysNum ).CoolingFanSpeedRatio;
							}
						}
					} // IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
				} // IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
			} // ELSE ! No Moisture Load
		} // No Heating/Cooling Load

		if ( UnitarySystem( UnitarySysNum ).MultiSpeedHeatingCoil && ( HeatingLoad && HeatSpeedNum == 1 ) ) {
			MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // proportional to PLR when speed = 1,  #5518
		} else if ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil && ( CoolingLoad && CoolSpeedNum == 1 ) ) {
			MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // proportional to PLR when speed = 1,  #5518
		} else {
			MSHPMassFlowRateLow = CompOffMassFlow; // these need to be set for multi-speed coils
		}
		MSHPMassFlowRateHigh = CompOnMassFlow; // doesn't hurt to set these if multi-speed coils are not used

		// Set the system mass flow rates
		SetAverageAirFlow( UnitarySysNum, PartLoadRatio, OnOffAirFlowRatio );

	}

	void
	SetAverageAirFlow(
		int const UnitarySysNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the average air mass flow rates using the part-load fraction of the HVAC system for this time step
		// Set OnOffAirFlowRatio to be used by DX coils

		// METHODOLOGY EMPLOYED:
		// The air flow rate in cooling, heating, and no cooling or heating can be dIFferent.
		// Calculate the air flow rate based on initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // inlet node number
		Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step
		int SpeedNum; // speed for multi-speed or variable-speed coils
		bool FanOn;

		SpeedNum = max( UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedNum );
		InletNode = UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum;

		if ( SpeedNum > 1 ) {
			if ( ( CoolingLoad && MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) || ( HeatingLoad && MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) ) {
				AverageUnitMassFlow = PartLoadRatio * CompOnMassFlow + ( 1.0 - PartLoadRatio ) * CompOffMassFlow;
			} else {
				AverageUnitMassFlow = CompOnMassFlow;
			}
		} else {
			AverageUnitMassFlow = ( PartLoadRatio * CompOnMassFlow ) + ( ( 1.0 - PartLoadRatio ) * CompOffMassFlow );
		}

		if ( CompOffFlowRatio > 0.0 ) {
			if ( SpeedNum > 1 ) {
				if ( ( CoolingLoad && MultiOrVarSpeedCoolCoil( UnitarySysNum ) ) || ( HeatingLoad && MultiOrVarSpeedHeatCoil( UnitarySysNum ) ) ) {
					FanSpeedRatio = PartLoadRatio * CompOnFlowRatio + ( 1.0 - PartLoadRatio ) * CompOffFlowRatio;
				} else {
					FanSpeedRatio = CompOnFlowRatio;
				}
			} else {
				FanSpeedRatio = ( PartLoadRatio * CompOnFlowRatio ) + ( ( 1.0 - PartLoadRatio ) * CompOffFlowRatio );
			}
		} else {
			FanSpeedRatio = CompOnFlowRatio;
		}

		if ( UnitarySystem( UnitarySysNum ).SingleMode == 1 ) {
			AverageUnitMassFlow = PartLoadRatio * CompOnMassFlow;
			FanSpeedRatio = PartLoadRatio * CompOnFlowRatio;
			if ( !HeatingLoad && !CoolingLoad ) {
				AverageUnitMassFlow = UnitarySystem( UnitarySysNum ).MaxNoCoolHeatAirMassFlow;
			}
		}

		// If the unitary system is scheduled on or nightime cycle overrides fan schedule. Uses same logic as fan.
		if ( UnitarySystem( UnitarySysNum ).FanExists ) {
			FanOn = false;
			if ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).FanAvailSchedPtr ) > 0 ) FanOn = true;
		} else {
			FanOn = true;
		}
		if ( GetCurrentScheduleValue( UnitarySystem( UnitarySysNum ).SysAvailSchedPtr ) > 0.0 && ( ( FanOn || TurnFansOn ) && ! TurnFansOff ) ) {
			if ( UnitarySystem( UnitarySysNum ).ControlType == SetPointBased ) {
				// set point based equipment should use VAV terminal units to set the flow.
				// zone equipment needs to set flow since no other device regulates flow (ZoneHVAC /= AirLoopEquipment)
				if ( !UnitarySystem( UnitarySysNum ).AirLoopEquipment ) {
					Node( InletNode ).MassFlowRate = AverageUnitMassFlow;
					Node( InletNode ).MassFlowRateMaxAvail = AverageUnitMassFlow;
				}
				if ( AverageUnitMassFlow > 0.0 ) {
					OnOffAirFlowRatio = 1.0;
				} else {
					OnOffAirFlowRatio = 0.0;
				}
			} else {
				Node( InletNode ).MassFlowRate = AverageUnitMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AverageUnitMassFlow;
				if ( AverageUnitMassFlow > 0.0 ) {
					OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
				} else {
					OnOffAirFlowRatio = 0.0;
				}
			}
		} else {
			Node( InletNode ).MassFlowRate = 0.0;
			OnOffAirFlowRatio = 1.0;
		}

	}

	void
	ReportUnitarySystem(
		int const UnitarySysNum,
		int const AirLoopNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variable for the coils.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using DataAirLoop::LoopSystemOnMassFlowrate;
		using DataAirLoop::LoopSystemOffMassFlowrate;
		using DataAirLoop::LoopFanOperationMode;
		using DataAirLoop::LoopOnOffFanPartLoadRatio;
		using DataAirLoop::LoopCompCycRatio;
		using DataAirLoop::AirLoopFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;
		int InletNode;
		Real64 QSensUnitOut;
		Real64 QTotUnitOut;
		Real64 AirMassFlow;
		Real64 MinHumRatio;
		Real64 CompPartLoadFrac;
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;

		QTotUnitOut = 0.0;
		QSensUnitOut = 0.0;
		UnitarySystem( UnitarySysNum ).PartLoadFrac = 0.0;
		UnitarySystem( UnitarySysNum ).CompPartLoadRatio = 0.0;
		UnitarySystem( UnitarySysNum ).CycRatio = 0.0;
		UnitarySystem( UnitarySysNum ).SpeedRatio = 0.0;
		UnitarySystem( UnitarySysNum ).FanPartLoadRatio = 0.0;
		UnitarySystem( UnitarySysNum ).TotalAuxElecPower = 0.0;
		UnitarySystem( UnitarySysNum ).HeatingAuxElecConsumption = 0.0;
		UnitarySystem( UnitarySysNum ).CoolingAuxElecConsumption = 0.0;
		UnitarySystem( UnitarySysNum ).ElecPower = 0.0;
		UnitarySystem( UnitarySysNum ).ElecPowerConsumption = 0.0;

		OutletNode = UnitarySystem( UnitarySysNum ).UnitarySystemOutletNodeNum;
		AirMassFlow = Node( OutletNode ).MassFlowRate;
		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).ControlType );
		if ( SELECT_CASE_var == SetPointBased ) {
			InletNode = UnitarySystem( UnitarySysNum ).UnitarySystemInletNodeNum;
			MinHumRatio = Node( InletNode ).HumRat;
			QSensUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRatio ) ) - UnitarySystem( UnitarySysNum ).SenLoadLoss;
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );

		} else if ( SELECT_CASE_var == LoadBased ) {
			MinHumRatio = Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).HumRat;
			QSensUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Temp, MinHumRatio ) ) - UnitarySystem( UnitarySysNum ).SenLoadLoss;
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( UnitarySystem( UnitarySysNum ).NodeNumOfControlledZone ).Enthalpy );

		} else {

		}}

		// set the system part-load ratio report variable
		UnitarySystem( UnitarySysNum ).PartLoadFrac = max( UnitarySystem( UnitarySysNum ).CoolingPartLoadFrac, UnitarySystem( UnitarySysNum ).HeatingPartLoadFrac );
		// set the compressor part-load ratio report variable
		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );
		if ( ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) || ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHP ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPSimple ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) || ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) ) {
			// wasn't this already set in the calc routine?
			// they look wrong anyway since the compressor can be off when the fan is on
			//          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = UnitarySystem(UnitarySysNum)%PartLoadFrac
		} else {
			//          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
		}}
		UnitarySystem( UnitarySysNum ).CompPartLoadRatio = max( UnitarySystem( UnitarySysNum ).CoolCompPartLoadRatio, UnitarySystem( UnitarySysNum ).HeatCompPartLoadRatio );

		if ( HeatingLoad ) {
			if ( QTotUnitOut > 0.0 ) {  // heating
				UnitarySystem( UnitarySysNum ).TotCoolEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).SensCoolEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).LatCoolEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).TotHeatEnergyRate = std::abs( max( 0.0, QTotUnitOut ) );
				UnitarySystem( UnitarySysNum ).SensHeatEnergyRate = std::abs( max( 0.0, QSensUnitOut ) );
				UnitarySystem( UnitarySysNum ).LatHeatEnergyRate = std::abs( max( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
			} else {
				UnitarySystem( UnitarySysNum ).TotCoolEnergyRate = std::abs( min( 0.0, QTotUnitOut ) );
				UnitarySystem( UnitarySysNum ).SensCoolEnergyRate = std::abs( min( 0.0, QSensUnitOut ) );
				UnitarySystem( UnitarySysNum ).LatCoolEnergyRate = std::abs( min( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
				UnitarySystem( UnitarySysNum ).TotHeatEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).SensHeatEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).LatHeatEnergyRate = 0.0;
			}
		} else {
			if ( QTotUnitOut <= 0.0 ) {  // cooling
				UnitarySystem( UnitarySysNum ).TotCoolEnergyRate = std::abs( min( 0.0, QTotUnitOut ) );
				UnitarySystem( UnitarySysNum ).SensCoolEnergyRate = std::abs( min( 0.0, QSensUnitOut ) );
				UnitarySystem( UnitarySysNum ).LatCoolEnergyRate = std::abs( min( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
				UnitarySystem( UnitarySysNum ).TotHeatEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).SensHeatEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).LatHeatEnergyRate = 0.0;
			} else {
				UnitarySystem( UnitarySysNum ).TotCoolEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).SensCoolEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).LatCoolEnergyRate = 0.0;
				UnitarySystem( UnitarySysNum ).TotHeatEnergyRate = std::abs( max( 0.0, QTotUnitOut ) );
				UnitarySystem( UnitarySysNum ).SensHeatEnergyRate = std::abs( max( 0.0, QSensUnitOut ) );
				UnitarySystem( UnitarySysNum ).LatHeatEnergyRate = std::abs( max( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
			}
		} 

		if ( UnitarySystem( UnitarySysNum ).FanExists ) {
			if ( CompOnMassFlow > 0.0 ) {
				UnitarySystem( UnitarySysNum ).FanPartLoadRatio = Node( OutletNode ).MassFlowRate / CompOnMassFlow;
			} else {
				UnitarySystem( UnitarySysNum ).FanPartLoadRatio = 0.0;
			}
			if ( AirLoopNum > 0 ) {
				if ( UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
					AirLoopFlow( AirLoopNum ).FanPLR = UnitarySystem( UnitarySysNum ).FanPartLoadRatio;
				} else {
					AirLoopFlow( AirLoopNum ).FanPLR = 0.0;
				}
			}
		}

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {
			// need to make sure these are 0 for non-variable speed coils (or not report these variables)
			UnitarySystem( UnitarySysNum ).CycRatio = max( UnitarySystem( UnitarySysNum ).CoolingCycRatio, UnitarySystem( UnitarySysNum ).HeatingCycRatio );
			UnitarySystem( UnitarySysNum ).SpeedRatio = max( UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio );
			UnitarySystem( UnitarySysNum ).SpeedNum = max( UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedNum );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {
			UnitarySystem( UnitarySysNum ).CycRatio = max( UnitarySystem( UnitarySysNum ).CoolingCycRatio, UnitarySystem( UnitarySysNum ).HeatingCycRatio );
			UnitarySystem( UnitarySysNum ).SpeedRatio = max( UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio );
			UnitarySystem( UnitarySysNum ).SpeedNum = max( UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedNum );

			CompPartLoadFrac = UnitarySystem( UnitarySysNum ).CompPartLoadRatio;
			if ( CoolingLoad ) {

				UnitarySystem( UnitarySysNum ).TotalAuxElecPower = UnitarySystem( UnitarySysNum ).AncillaryOnPower * CompPartLoadFrac + UnitarySystem( UnitarySysNum ).AncillaryOffPower * ( 1.0 - CompPartLoadFrac );
				UnitarySystem( UnitarySysNum ).CoolingAuxElecConsumption = UnitarySystem( UnitarySysNum ).AncillaryOnPower * CompPartLoadFrac * ReportingConstant;
			}
			if ( UnitarySystem( UnitarySysNum ).LastMode == CoolingMode ) {
				UnitarySystem( UnitarySysNum ).CoolingAuxElecConsumption += UnitarySystem( UnitarySysNum ).AncillaryOffPower * ( 1.0 - CompPartLoadFrac ) * ReportingConstant;

			}
			UnitarySystem( UnitarySysNum ).ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + ElecHeatingCoilPower + UnitarySystem( UnitarySysNum ).TotalAuxElecPower;
			UnitarySystem( UnitarySysNum ).ElecPowerConsumption = UnitarySystem( UnitarySysNum ).ElecPower * ReportingConstant;

		} else if ( SELECT_CASE_var == Coil_CoolingWater || SELECT_CASE_var == Coil_CoolingWaterDetailed ) {

			if ( UnitarySystem( UnitarySysNum ).MultiSpeedCoolingCoil ) {
				UnitarySystem( UnitarySysNum ).CycRatio = max( UnitarySystem( UnitarySysNum ).CoolingCycRatio, UnitarySystem( UnitarySysNum ).HeatingCycRatio );
				UnitarySystem( UnitarySysNum ).SpeedRatio = max( UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio );
				UnitarySystem( UnitarySysNum ).SpeedNum = max( UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedNum );
			}
			UnitarySystem( UnitarySysNum ).ElecPower = FanElecPower;
			UnitarySystem( UnitarySysNum ).ElecPowerConsumption = UnitarySystem( UnitarySysNum ).ElecPower * ReportingConstant;

		} else {

		}}

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {
			UnitarySystem( UnitarySysNum ).CycRatio = max( UnitarySystem( UnitarySysNum ).CoolingCycRatio, UnitarySystem( UnitarySysNum ).HeatingCycRatio );
			UnitarySystem( UnitarySysNum ).SpeedRatio = max( UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio );
			UnitarySystem( UnitarySysNum ).SpeedNum = max( UnitarySystem( UnitarySysNum ).CoolingSpeedNum, UnitarySystem( UnitarySysNum ).HeatingSpeedNum );

			CompPartLoadFrac = UnitarySystem( UnitarySysNum ).CompPartLoadRatio;
			if ( HeatingLoad ) {

				UnitarySystem( UnitarySysNum ).TotalAuxElecPower = UnitarySystem( UnitarySysNum ).AncillaryOnPower * CompPartLoadFrac + UnitarySystem( UnitarySysNum ).AncillaryOffPower * ( 1.0 - CompPartLoadFrac );
				UnitarySystem( UnitarySysNum ).HeatingAuxElecConsumption = UnitarySystem( UnitarySysNum ).AncillaryOnPower * CompPartLoadFrac * ReportingConstant;
			}
			if ( UnitarySystem( UnitarySysNum ).LastMode == HeatingMode ) {
				UnitarySystem( UnitarySysNum ).HeatingAuxElecConsumption += UnitarySystem( UnitarySysNum ).AncillaryOffPower * ( 1.0 - CompPartLoadFrac ) * ReportingConstant;

			}
			UnitarySystem( UnitarySysNum ).ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + UnitarySystem( UnitarySysNum ).TotalAuxElecPower;
			UnitarySystem( UnitarySysNum ).ElecPowerConsumption = UnitarySystem( UnitarySysNum ).ElecPower * ReportingConstant;

		} else if ( ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) ) {
			UnitarySystem( UnitarySysNum ).CycRatio = max( UnitarySystem( UnitarySysNum ).CoolingCycRatio, UnitarySystem( UnitarySysNum ).HeatingCycRatio );
			UnitarySystem( UnitarySysNum ).SpeedRatio = max( UnitarySystem( UnitarySysNum ).CoolingSpeedRatio, UnitarySystem( UnitarySysNum ).HeatingSpeedRatio );

			UnitarySystem( UnitarySysNum ).ElecPower = FanElecPower + DXElecCoolingPower + ElecHeatingCoilPower + UnitarySystem( UnitarySysNum ).TotalAuxElecPower;
			UnitarySystem( UnitarySysNum ).ElecPowerConsumption = UnitarySystem( UnitarySysNum ).ElecPower * ReportingConstant;

		} else {

		}}

		LoopSystemOnMassFlowrate = CompOnMassFlow;
		LoopSystemOffMassFlowrate = CompOffMassFlow;
		LoopFanOperationMode = UnitarySystem( UnitarySysNum ).FanOpMode;
		LoopOnOffFanPartLoadRatio = UnitarySystem( UnitarySysNum ).FanPartLoadRatio;
		LoopCompCycRatio = UnitarySystem( UnitarySysNum ).CycRatio;

		if (  UnitarySystem(UnitarySysNum).FirstPass ) {

			if (  ! SysSizingCalc ) {

				if ( CurOASysNum > 0 ) {
					OASysEqSizing( CurOASysNum ).AirFlow = false;
					OASysEqSizing( CurOASysNum ).CoolingAirFlow = false;
					OASysEqSizing( CurOASysNum ).HeatingAirFlow = false;
					OASysEqSizing( CurOASysNum ).Capacity = false;
					OASysEqSizing( CurOASysNum ).CoolingCapacity = false;
					OASysEqSizing( CurOASysNum ).HeatingCapacity = false;
				} else if ( CurSysNum > 0 ) {
					UnitarySysEqSizing( CurSysNum ).AirFlow = false;
					UnitarySysEqSizing( CurSysNum ).CoolingAirFlow = false;
					UnitarySysEqSizing( CurSysNum ).HeatingAirFlow = false;
					UnitarySysEqSizing( CurSysNum ).Capacity = false;
					UnitarySysEqSizing( CurSysNum ).CoolingCapacity = false;
					UnitarySysEqSizing( CurSysNum ).HeatingCapacity = false;
					AirLoopControlInfo( CurSysNum ).UnitarySysSimulating = false;
				} else if ( CurZoneEqNum > 0 ) {
					ZoneEqSizing( CurZoneEqNum ).AirFlow = false;
					ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow = false;
					ZoneEqSizing( CurZoneEqNum ).HeatingAirFlow = false;
					ZoneEqSizing( CurZoneEqNum ).Capacity = false;
					ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = false;
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = false;
				}
				UnitarySystem( UnitarySysNum ).FirstPass = false;

			}

		}

	}

	void
	UnitarySystemHeatRecovery( int const UnitarySysNum ) // Number of the current electric UnitarySystem being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Chandan Sharma
		//       DATE WRITTEN:    May 2013
		//       MODIFIED:        na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		//  Calculate the heat recovered from UnitarySystem

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UnitarySystemHeatRecovery" );

		// DERIVMS TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecInNode; // Node number of heat recovery water inlet node
		int HeatRecOutNode; // Node number of heat recovery water outlet node
		Real64 QHeatRec; // Total heat recovered [W]
		Real64 HeatRecInletTemp; // Heat reclaim inlet temp [C]
		Real64 HeatRecOutletTemp; // Heat reclaim outlet temp [C]
		Real64 HeatRecMassFlowRate; // Heat reclaim mass flow rate [m3/s]
		Real64 CpHeatRec; // Heat reclaim water inlet specIFic heat [J/kg-K]
		Real64 HeatRecInletEnth; // Heat reclaim water inlet enthalpy [J/kg]
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;

		// Begin routine
		HeatRecInNode = UnitarySystem( UnitarySysNum ).HeatRecoveryInletNodeNum;
		HeatRecOutNode = UnitarySystem( UnitarySysNum ).HeatRecoveryOutletNodeNum;

		// Inlet node to the heat recovery heat exchanger
		HeatRecInletTemp = Node( HeatRecInNode ).Temp;
		HeatRecInletEnth = Node( HeatRecInNode ).Enthalpy;

		// Set heat recovery mass flow rates
		HeatRecMassFlowRate = Node( HeatRecInNode ).MassFlowRate;

		QHeatRec = MSHPWasteHeat;

		if ( HeatRecMassFlowRate > 0.0 ) {

			CpHeatRec = GetSpecificHeatGlycol( PlantLoop( UnitarySystem( UnitarySysNum ).HRLoopNum ).FluidName, HeatRecInletTemp, PlantLoop( UnitarySystem( UnitarySysNum ).HRLoopNum ).FluidIndex, RoutineName );

			HeatRecOutletTemp = QHeatRec / ( HeatRecMassFlowRate * CpHeatRec ) + HeatRecInletTemp;
			if ( HeatRecOutletTemp > UnitarySystem( UnitarySysNum ).MaxHROutletWaterTemp ) HeatRecOutletTemp = UnitarySystem( UnitarySysNum ).MaxHROutletWaterTemp;
		} else {
			HeatRecOutletTemp = HeatRecInletTemp;
		}

		SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
		// changed outputs
		Node( HeatRecOutNode ).Temp = HeatRecOutletTemp;

		UnitarySystem( UnitarySysNum ).HeatRecoveryRate = QHeatRec;
		UnitarySystem( UnitarySysNum ).HeatRecoveryEnergy = UnitarySystem( UnitarySysNum ).HeatRecoveryRate * ReportingConstant;
		UnitarySystem( UnitarySysNum ).HeatRecoveryInletTemp = HeatRecInletTemp;
		UnitarySystem( UnitarySysNum ).HeatRecoveryOutletTemp = HeatRecOutletTemp;
		UnitarySystem( UnitarySysNum ).HeatRecoveryMassFlowRate = HeatRecMassFlowRate;

	}

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2006
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDOe2DXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcDXHeatingCoil;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // Index of this coil
		Real64 OutletAirTemp; // Outlet air temperature [C]
		Real64 OnOffAirFlowFrac; // Ratio of compressor ON to compressor OFF air mass flow rate

		CoilIndex = int( Par( 1 ) );
		OnOffAirFlowFrac = Par( 3 );

		CalcDXHeatingCoil( CoilIndex, PartLoadFrac, ContFanCycCoil, OnOffAirFlowFrac );

		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DXCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp).
		// DX Coil output depends on the compressor speed which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet temperature at the given compressor speed
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcMultiSpeedDXCoil;
		using DXCoils::CalcMultiSpeedDXCoilCooling;
		using VariableSpeedCoils::SimVariableSpeedCoils;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		int UnitarySysNum; // index to Unitary System
		Real64 OutletAirTemp( 0.0 ); // outlet air temperature [C]
		Real64 CycRatio;
		int SpeedNum;
		int FanOpMode;
		int CompOp;
		Real64 ReqOutput;
		Real64 dummy;
		Real64 RuntimeFrac;
		Real64 OnOffAirFlowRatio;
		Real64 SensLoad;

		CoilIndex = int( Par( 1 ) );
		UnitarySysNum = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {

			CalcMultiSpeedDXCoil( CoilIndex, SpeedRatio, 1.0 );
			OutletAirTemp = DXCoilOutletTemp( CoilIndex );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			OnOffAirFlowRatio = 1.0;

			SetAverageAirFlow( UnitarySysNum, SpeedRatio, OnOffAirFlowRatio );
			CalcMultiSpeedDXCoilCooling( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0 );
			OutletAirTemp = DXCoilOutletTemp( CoilIndex );

		} else if ( ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			ReqOutput = Par( 8 );
			dummy = 0.0;
			SensLoad = -1.0;
			RuntimeFrac = 1.0;
			OnOffAirFlowRatio = 1.0;

			SimVariableSpeedCoils( "", CoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOp, CycRatio, SpeedNum, SpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).Temp;

		} else {
			assert( false );
		}}

		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	HeatingCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp).
		// DX Coil output depends on the compressor speed which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls calc routines of  multi Speed or variable Coil to get outlet temperature at the given compressor speed
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcMultiSpeedDXCoil;
		using DXCoils::CalcMultiSpeedDXCoilHeating;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using HeatingCoils::CalcMultiStageElectricHeatingCoil;
		using HeatingCoils::CalcMultiStageGasHeatingCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		int UnitarySysNum; // index to Unitary System
		Real64 OutletAirTemp( 0.0 ); // outlet air temperature [C]
		Real64 CycRatio;
		int SpeedNum;
		int FanOpMode;
		int CompOp;
		Real64 ReqOutput;
		Real64 OnOffAirFlowRatio;
		Real64 SensLoad;
		Real64 LatLoad;

		CoilIndex = int( Par( 1 ) );
		UnitarySysNum = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			OnOffAirFlowRatio = 1.0;

			SetAverageAirFlow( UnitarySysNum, SpeedRatio, OnOffAirFlowRatio );

			CalcMultiSpeedDXCoilHeating( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );

			OutletAirTemp = DXCoilOutletTemp( CoilIndex );

		} else if ( ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			ReqOutput = Par( 8 );
			OnOffAirFlowRatio = 1.0;
			SensLoad = 1.0;
			LatLoad = -1.0;

			// can't call only the calc routine with these coil types since Init sets air flow rate based on speed num and cycling ratio
			SimVariableSpeedCoils( "", CoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOp, CycRatio, SpeedNum, SpeedRatio, SensLoad, LatLoad, OnOffAirFlowRatio );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;

		} else if ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );

			CalcMultiStageElectricHeatingCoil( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;

		} else if ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );

			CalcMultiStageElectricHeatingCoil( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;

		} else {
			assert( false );
		}}

		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DXCoilVarSpeedHumRatResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat).
		// DX Coil output depends on the compressor speed which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls calc routine sof multi speed or variable speed coils to get outlet humidity ratio at the given compressor speed
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::CalcMultiSpeedDXCoil;
		using DXCoils::CalcMultiSpeedDXCoilCooling;
		using VariableSpeedCoils::SimVariableSpeedCoils;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		int UnitarySysNum; // index to Unitary System
		Real64 OutletAirHumRat( 0.0 ); // outlet air humidity ratio
		Real64 CycRatio;
		int SpeedNum;
		int FanOpMode;
		int CompOp;
		Real64 ReqOutput;
		Real64 SensLoad;
		Real64 LatLoad;
		Real64 RuntimeFrac;
		Real64 OnOffAirFlowRatio;

		CoilIndex = int( Par( 1 ) );
		UnitarySysNum = int( Par( 3 ) );
		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {

			CalcMultiSpeedDXCoil( CoilIndex, SpeedRatio, 1.0 );
			OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			OnOffAirFlowRatio = 1.0;

			SetAverageAirFlow( UnitarySysNum, SpeedRatio, OnOffAirFlowRatio );
			CalcMultiSpeedDXCoilCooling( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0 );
			OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );

		} else if ( ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

			CycRatio = Par( 4 );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			ReqOutput = Par( 8 );
			SensLoad = -1.0;
			LatLoad = 0.0;
			RuntimeFrac = 1.0;
			OnOffAirFlowRatio = 1.0;

			SimVariableSpeedCoils( "", CoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOp, CycRatio, SpeedNum, SpeedRatio, SensLoad, LatLoad, OnOffAirFlowRatio );

			OutletAirHumRat = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).HumRat;

		} else {
			assert( false );
		}}

		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	DXCoilCyclingResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the cycling ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls multi or variable speed coil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcMultiSpeedDXCoil;
		using DXCoils::CalcMultiSpeedDXCoilCooling;
		using VariableSpeedCoils::CalcVarSpeedCoilCooling;
		using VariableSpeedCoils::SimVariableSpeedCoils;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp( 0.0 ); // outlet air temperature [C]
		Real64 SpeedRatio;
		int SpeedNum;
		int FanOpMode;
		int CompOp;
		int UnitarySysNum;
		Real64 ReqOutput;
		Real64 dummy;
		Real64 SensLoad;
		Real64 OnOffAirFlowRatio;

		//            Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
		//            Par(2) = DesOutTemp
		//            Par(3) = UnitarySysNum
		//            Par(4) = SpeedRatio
		//            Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
		//            Par(6) = UnitarySystem(UnitarySysNum)%FanOpMode
		//            Par(7) = 1.0d0 ! CompOp

		CoilIndex = int( Par( 1 ) );
		UnitarySysNum = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {

			CalcMultiSpeedDXCoil( CoilIndex, 0.0, CycRatio );

			OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {

			SpeedRatio = int( Par( 4 ) );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			OnOffAirFlowRatio = 1.0;

			SetAverageAirFlow( UnitarySysNum, CycRatio, OnOffAirFlowRatio );
			CalcMultiSpeedDXCoilCooling( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0 );
			OutletAirTemp = DXCoilOutletTemp( CoilIndex );

		} else if ( ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

			SpeedRatio = Par( 4 ); //Autodesk:Init Added line to elim use uninitialized
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			if ( CycRatio == 0.0 ) CompOp = 0;
			ReqOutput = Par( 8 );
			dummy = 0.0;
			OnOffAirFlowRatio = 1.0;

			SensLoad = -1.0;
			SimVariableSpeedCoils( "", CoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOp, CycRatio, SpeedNum, SpeedRatio, SensLoad, dummy, OnOffAirFlowRatio );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).Temp;

		} else {
			assert( false );
		}}

		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	HeatingCoilVarSpeedCycResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the cycling ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls multi or variable speed coil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcMultiSpeedDXCoil;
		using DXCoils::CalcMultiSpeedDXCoilHeating;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using HeatingCoils::CalcMultiStageElectricHeatingCoil;
		using HeatingCoils::CalcMultiStageGasHeatingCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp( 0.0 ); // outlet air temperature [C]
		Real64 SpeedRatio;
		int SpeedNum;
		int FanOpMode;
		int CompOp;
		int UnitarySysNum;
		Real64 ReqOutput;
		Real64 SensLoad;
		Real64 LatLoad;
		Real64 OnOffAirFlowRatio;

		//            Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
		//            Par(2) = DesOutTemp
		//            Par(3) = UnitarySysNum
		//            Par(4) = SpeedRatio
		//            Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
		//            Par(6) = UnitarySystem(UnitarySysNum)%FanOpMode
		//            Par(7) = 1.0d0 ! CompOp

		CoilIndex = int( Par( 1 ) );
		UnitarySysNum = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {

			SpeedRatio = int( Par( 4 ) );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			OnOffAirFlowRatio = 1.0;

			SetAverageAirFlow( UnitarySysNum, CycRatio, OnOffAirFlowRatio );
			CalcMultiSpeedDXCoilHeating( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );
			OutletAirTemp = DXCoilOutletTemp( CoilIndex );

		} else if ( ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_HeatingWaterToAirHPVSEquationFit ) ) {

			SpeedRatio = int( Par( 4 ) ); //Autodesk:Init Added line to elim use uninitialized
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			if ( CycRatio == 0.0 ) CompOp = 0;
			ReqOutput = Par( 8 );
			SensLoad = -1.0;
			LatLoad = 0.0;
			OnOffAirFlowRatio = 1.0;

			SimVariableSpeedCoils( "", CoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOp, CycRatio, SpeedNum, SpeedRatio, SensLoad, LatLoad, OnOffAirFlowRatio );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;

		} else if ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) {

			SpeedRatio = int( Par( 4 ) );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );

			CalcMultiStageElectricHeatingCoil( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;

		} else if ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) {

			SpeedRatio = int( Par( 4 ) );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );

			CalcMultiStageElectricHeatingCoil( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode );

			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;

		} else {
			assert( false );
		}}

		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DXCoilCyclingHumRatResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the cycling ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::CalcMultiSpeedDXCoil;
		using DXCoils::CalcMultiSpeedDXCoilCooling;
		using VariableSpeedCoils::SimVariableSpeedCoils;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat( 0.0 ); // outlet air humidity ratio [kg/kg]
		Real64 SpeedRatio;
		int SpeedNum;
		int FanOpMode;
		int CompOp;
		int UnitarySysNum;
		Real64 ReqOutput;
		Real64 SensLoad;
		Real64 LatLoad;
		Real64 OnOffAirFlowRatio;

		CoilIndex = int( Par( 1 ) );
		UnitarySysNum = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {

			CalcMultiSpeedDXCoil( CoilIndex, 0.0, CycRatio );

			OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {

			SpeedRatio = int( Par( 4 ) );
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			OnOffAirFlowRatio = 1.0;

			SetAverageAirFlow( UnitarySysNum, CycRatio, OnOffAirFlowRatio );
			CalcMultiSpeedDXCoilCooling( CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0 );
			OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );

		} else if ( ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) || ( SELECT_CASE_var == Coil_CoolingWaterToAirHPVSEquationFit ) ) {

			SpeedRatio = int( Par( 4 ) ); //Autodesk:Init Added line to elim use uninitialized
			SpeedNum = int( Par( 5 ) );
			FanOpMode = int( Par( 6 ) );
			CompOp = int( Par( 7 ) );
			ReqOutput = Par( 8 );
			SensLoad = -1.0;
			LatLoad = 0.0;
			OnOffAirFlowRatio = 1.0;

			SimVariableSpeedCoils( "", CoilIndex, FanOpMode, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, CompOp, CycRatio, SpeedNum, SpeedRatio, SensLoad, LatLoad, OnOffAirFlowRatio );

			OutletAirHumRat = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).HumRat;

		} else {
			assert( false );
		}}

		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   November 2003
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDOe2DXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcDoe2DXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		int FanOpMode; // Supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		CalcDoe2DXCoil( CoilIndex, On, true, PartLoadRatio, FanOpMode );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DOE2DXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDOe2DXCoil to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::CalcDoe2DXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		int FanOpMode; // Supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		CalcDoe2DXCoil( CoilIndex, On, true, PartLoadRatio, FanOpMode );
		OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	CoolWaterHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWater coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// Cool water coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimulateWaterCoilComponents to get outlet temp at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		Real64 mdot;
		bool FirstHVACIteration;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );

		mdot = min( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow * PartLoadRatio );
		Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = mdot;
		SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).CoolingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, _, _, PartLoadRatio );

		OutletAirHumRat = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).HumRat;
		Residuum = Par( 3 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	CoolWaterTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWater coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// Cool water coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimulateWaterCoilComponents to get outlet temp at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // index of this coil
		Real64 OutletAirTemp; // outlet air humidity ratio [kg/kg]
		Real64 mdot;
		bool FirstHVACIteration;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );

		mdot = min( Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow * PartLoadRatio );
		Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = mdot;
		SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).CoolingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, _, _, PartLoadRatio );

		OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).Temp;
		Residuum = Par( 3 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	CoolWatertoAirHPHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWatertoAirHP coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// Cool water coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		Real64 ReqOutput;
		bool FirstHVACIteration;
		bool errFlag;
		Real64 RuntimeFrac; // heat pump runtime fraction
		Real64 dummy;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );
		ReqOutput = Par( 4 );

		HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, RuntimeFrac );

		UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadRatio;
		UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = RuntimeFrac;

		dummy = 0.0;
		if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple ) {
			SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ReqOutput, dummy, UnitarySystem( UnitarySysNum ).FanOpMode, RuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 0, PartLoadRatio, FirstHVACIteration );
		} else {
			SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow, UnitarySystem( UnitarySysNum ).FanOpMode, FirstHVACIteration, RuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadRatio );
		}

		OutletAirHumRat = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).HumRat;
		Residuum = Par( 3 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	CoolWatertoAirHPTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWatertoAirHP coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR        Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// Cool water coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // index of this coil
		Real64 OutletAirTemp; // outlet air humidity ratio [kg/kg]
		Real64 ReqOutput;
		bool FirstHVACIteration;
		bool errFlag;
		Real64 RuntimeFrac;
		Real64 dummy;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );
		ReqOutput = Par( 4 );

		HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, RuntimeFrac );

		if ( RuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
			OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
		} else {
			OnOffFanPartLoadFraction = 1;
		}

		UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadRatio;
		UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = RuntimeFrac;

		dummy = 0.0;
		if ( UnitarySystem( UnitarySysNum ).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple ) {
			SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, ReqOutput, dummy, UnitarySystem( UnitarySysNum ).FanOpMode, RuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, PartLoadRatio, FirstHVACIteration );
		} else {
			SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).MaxCoolAirMassFlow, UnitarySystem( UnitarySysNum ).FanOpMode, FirstHVACIteration, RuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadRatio );
		}

		OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).Temp;
		Residuum = Par( 3 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	TESIceStorageCoilOutletResidual(
		Real64 PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // data array
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR        Richard Raustad, FSEC
		//       DATE WRITTEN   August 2015
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using PackagedThermalStorageCoil::SimTESCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par( 1 ) = double( UnitarySysNum );
		// Par( 2 ) = DesOutTemp;
		// Par( 3 ) = 0.0; // DesOutHumRat; set to 0 if temp controlled

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // index of this coil
		Real64 DesiredOutletTemp; // temperature control point
		Real64 DesiredOutletHumRat; // humidity ratio control point
		Real64 OutletAirTemp; // outlet air temperature [C]
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]

		UnitarySysNum = int( Par( 1 ) );
		DesiredOutletTemp = Par( 2 );
		DesiredOutletHumRat = Par( 3 );

		SimTESCoil( UnitarySystem( UnitarySysNum ).CoolingCoilName, UnitarySystem( UnitarySysNum ).CoolingCoilIndex, UnitarySystem( UnitarySysNum ).FanOpMode, UnitarySystem( UnitarySysNum ).TESOpMode, PartLoadRatio );

		if ( DesiredOutletHumRat > 0.0 ) {
			OutletAirHumRat = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).HumRat;
			Residuum = OutletAirHumRat - DesiredOutletHumRat;
		} else {
			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).CoolCoilOutletNodeNum ).Temp;
			Residuum = OutletAirTemp - DesiredOutletTemp;
		}

		return Residuum;
	}

	Real64
	HeatWatertoAirHPTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// Heat water coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using WaterToAirHeatPump::SimWatertoAirHP;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum; // index of this coil
		Real64 OutletAirTemp; // outlet air humidity ratio [kg/kg]
		Real64 ReqOutput;
		bool FirstHVACIteration;
		bool errFlag;
		Real64 RuntimeFrac;
		Real64 dummy;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );
		ReqOutput = Par( 1 );

		HeatPumpRunFrac( UnitarySysNum, PartLoadRatio, errFlag, RuntimeFrac );

		if ( RuntimeFrac > 0.0 && UnitarySystem( UnitarySysNum ).FanOpMode == CycFanCycCoil ) {
			OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
		} else {
			OnOffFanPartLoadFraction = 1.0;
		}

		UnitarySystem( UnitarySysNum ).CompPartLoadRatio = PartLoadRatio;
		UnitarySystem( UnitarySysNum ).WSHPRuntimeFrac = RuntimeFrac;

		dummy = 0.0;
		if ( UnitarySystem( UnitarySysNum ).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple ) {
			SimWatertoAirHPSimple( BlankString, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, ReqOutput, dummy, UnitarySystem( UnitarySysNum ).FanOpMode, RuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, 1, PartLoadRatio, FirstHVACIteration );
		} else {
			SimWatertoAirHP( BlankString, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, UnitarySystem( UnitarySysNum ).MaxHeatAirMassFlow, UnitarySystem( UnitarySysNum ).FanOpMode, FirstHVACIteration, RuntimeFrac, UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour, UnitarySystem( UnitarySysNum ).HPTimeConstant, UnitarySystem( UnitarySysNum ).FanDelayTime, UnitarySystem( UnitarySysNum ).InitHeatPump, ReqOutput, dummy, 0, PartLoadRatio );
		}

		OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;
		Residuum = Par( 3 ) - OutletAirTemp;

		return Residuum;
	}

	void
	HeatPumpRunFrac(
		int const UnitarySysNum, // UnitarySystem Index Number
		Real64 const PLR, // part load ratio
		bool & errFlag, // part load factor out of range flag
		Real64 & RuntimeFrac // the required run time fraction to meet part load
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   Apr 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the PLF based on the PLR. Parameters required are
		// thermostat cycling rate (Nmax), heat pump time constant (tau), and the fraction
		// of on-cycle power use (pr)

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// (1) Henderson, H. I., K. Rengarajan.1996. A Model to predict the latent capacity
		// of air conditioners and heat pumps at part-load conditions with constant fan
		// operation. ASHRAE Transactions 102 (1): 266-274

		// (2) Henderson, H.I. Jr., Y.J. Huang and Danny Parker. 1999. Residential Equipment
		// Part Load Curves for Use in DOE-2.  Environmental Energy Technologies Division,
		// Ernest OrlanDO Lawrence Berkeley National Laboratory.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PartLoadFactor; // Part load factor
		Real64 Nmax; // Maximum cycling rate [cycles/hr]
		Real64 tau; // Heat pump time constant [s]
		Real64 pr; // On-cycle power use fraction [~]
		Real64 error; // Calculation error
		Real64 PLF1; // ith term of part load factor
		Real64 PLF2; // (i+1)th term of part load factor
		Real64 A; // Variable for simplIFy equation
		int NumIteration; // Iteration Counter

		Nmax = UnitarySystem( UnitarySysNum ).MaxONOFFCyclesperHour;
		tau = UnitarySystem( UnitarySysNum ).HPTimeConstant;
		pr = UnitarySystem( UnitarySysNum ).OnCyclePowerFraction;

		//Initialize
		errFlag = false;
		error = 1;
		NumIteration = 0;

		//Initial guess for part load fraction
		PLF1 = 1;

		//Calculate PLF using successive substitution until convergence
		//is achieved
		while ( true ) {
			++NumIteration;

			if ( PLR == 1 ) {
				// Set part load fraction, PLF1=1.0 IF PLR=1.0 and EXIT loop
				PLF1 = 1;
				break;
			}

			if ( NumIteration > 100 ) {
				// EXIT loop IF interation exceed 100
				errFlag = true;
				PLF1 = 1;
				break;
			}

			if ( error < 0.00001 ) {
				// EXIT loop IF convergence is achieved
				break;

			} else {
				// Calculate PLF
				A = 4.0 * tau * ( Nmax / 3600.0 ) * ( 1 - PLR / PLF1 );
				if ( A < 1.5e-3 ) {
					// A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
					// from "float underflow error". Occurs when PLR is very close to 1.0,
					// small A value, thus Exp(-1/A) = 0
					PLF2 = 1.0 - A;
				} else {
					PLF2 = 1.0 - A * ( 1.0 - std::exp( -1.0 / A ) );
				}
				error = std::abs( ( PLF2 - PLF1 ) / PLF1 );
				PLF1 = PLF2;
			}
		}

		//Adjust PLF for the off cycle power consumption IF
		//on-cycle power use is specified by the user
		if ( pr > 0.0 ) {
			PartLoadFactor = PLR / ( ( PLR / PLF1 ) + ( 1.0 - PLR / PLF1 ) * pr );
		} else {
			PartLoadFactor = PLF1;
		}

		if ( PartLoadFactor <= 0.0 ) {
			PartLoadFactor = 0;
			RuntimeFrac = 0;
			errFlag = true;
		} else {
			RuntimeFrac = PLR / PartLoadFactor;
		}

		if ( RuntimeFrac > 1.0 ) {
			RuntimeFrac = 1.0;
		}

	}

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         M. J. Witte, GARD Analytics, Inc.
		//       DATE WRITTEN   February 2005
		//                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::SimDXCoilMultiMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = dehumidification mode (0=normal, 1=enhanced)
		// par(4) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		int DehumidMode; // dehumidification mode (par3)
		int FanOpMode; // supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		DehumidMode = int( Par( 3 ) );
		FanOpMode = int( Par( 4 ) );
		SimDXCoilMultiMode( "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	MultiModeDXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimDXCoilMultiMode to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::SimDXCoilMultiMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(3) = dehumidification mode (0=normal, 1=enhanced)
		// par(4) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		int DehumidMode; // dehumidification mode (par3)
		int FanOpMode; // supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		DehumidMode = int( Par( 3 ) );
		FanOpMode = int( Par( 4 ) );
		SimDXCoilMultiMode( "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode );
		OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	HXAssistedCoolCoilTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   November 2003
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired outlet temp - actual outlet temp)
		//  DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcHXAssistedCoolingCoil to get outlet temperature at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp;
		using HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
		// par(4) = HX control (On/Off)
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		bool FirstHVACIteration; // FirstHVACIteration flag
		bool HXUnitOn; // flag to enable heat exchanger heat recovery
		int FanOpMode; // Supply air fan operating mode
		int UnitarySysNum; // index to unitary system

		CoilIndex = int( Par( 1 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		HXUnitOn = ( Par( 4 ) == 1.0 );
		FanOpMode = int( Par( 5 ) );
		UnitarySysNum = int( Par( 6 ) );
		if ( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode > 0 ) {
			Node( UnitarySystem( UnitarySysNum ).CoolCoilFluidInletNode ).MassFlowRate = UnitarySystem( UnitarySysNum ).MaxCoolCoilFluidFlow * PartLoadRatio;
		}
		CalcHXAssistedCoolingCoil( CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode );
		OutletAirTemp = HXAssistedCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;
		return Residuum;

	}

	Real64
	HXAssistedCoolCoilHRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired outlet humrat - actual outlet humrat)
		//  DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcHXAssistedCoolingCoil to get outlet humidity ratio at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat;
		using HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
		// par(4) = HX control (On/Off)
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		bool FirstHVACIteration; // FirstHVACIteration flag
		bool HXUnitOn; // flag to enable heat exchanger heat recovery
		int FanOpMode; // Supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		HXUnitOn = ( Par( 4 ) == 1.0 );
		FanOpMode = int( Par( 5 ) );
		CalcHXAssistedCoolingCoil( CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode, _, EconomizerFlag );
		OutletAirHumRat = HXAssistedCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;
		return Residuum;

	}

	Real64
	GasElecHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// hot water Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimulateHeatingCoilComponents to get outlet temperature at the given part load ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		Real64 CoilLoad; // heating coil load to be met [W]
		bool SuppHeatingCoilFlag; // true if heating coil is a supplemental heater in a parent object

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 OutletAirTemp; // Outlet air temperature [C]
		int UnitarySysNum;
		int FanOpMode( 0 ); // Fan operating mode (see parameter above)
		bool FirstHVACIteration;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );
		SuppHeatingCoilFlag = ( Par( 4 ) > 0.0 );
		FanOpMode = Par( 4 );
		CoilLoad = UnitarySystem( UnitarySysNum ).DesignHeatingCapacity * PartLoadFrac;
		if ( ! SuppHeatingCoilFlag ) {
			SimulateHeatingCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, CoilLoad, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, _, _, FanOpMode, PartLoadFrac );
			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;
		} else {
			SimulateHeatingCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, _, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, _, true, FanOpMode, PartLoadFrac );
			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode ).Temp;
		}
		Residuum = Par( 3 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	HotWaterHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// hot water Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimulateWaterCoilComponents to get outlet temperature at the given part load ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		bool LoadBased; // TRUE if controlling to load, else control to temperature
		bool SuppHeatingCoilFlag; // TRUE if supplemental heating coil

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 OutletAirTemp; // Outlet air temperature [C]
		Real64 mdot; // water-side flow rate of HW coil [kg/s]
		Real64 QActual; // heating capacity of HW coil [W]
		int UnitarySysNum; // index to unitary system
		bool FirstHVACIteration; // iteration flag

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );
		SuppHeatingCoilFlag = ( Par( 4 ) > 0.0 );
		LoadBased = ( Par( 5 ) > 0.0 );
		QActual = 0.0;
		if ( ! SuppHeatingCoilFlag ) {
			mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );
			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;
		} else {
			mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateWaterCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, QActual, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );
			OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).SuppCoilAirOutletNode ).Temp;
		}
		if ( LoadBased ) {
			Residuum = Par( 3 ) - QActual;
		} else {
			Residuum = Par( 3 ) - OutletAirTemp;
		}

		return Residuum;
	}

	Real64
	SteamHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// hot Steam Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimulateSteamCoilComponents to get outlet temperature at the given part load ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using SteamCoils::SimulateSteamCoilComponents;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		bool SuppHeatingCoilFlag;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 OutletAirTemp; // Outlet air temperature [C]
		Real64 mdot;
		int UnitarySysNum;
		bool FirstHVACIteration;

		UnitarySysNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) > 0.0 );
		SuppHeatingCoilFlag = ( Par( 4 ) > 0.0 );

		if ( ! SuppHeatingCoilFlag ) {
			mdot = min( Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxHeatCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).HeatCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).HeatingCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).HeatingCoilIndex, 1.0, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );
		} else {
			mdot = min( Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidOutletNodeNum ).MassFlowRateMaxAvail, UnitarySystem( UnitarySysNum ).MaxSuppCoilFluidFlow * PartLoadFrac );
			Node( UnitarySystem( UnitarySysNum ).SuppCoilFluidInletNode ).MassFlowRate = mdot;
			SimulateSteamCoilComponents( UnitarySystem( UnitarySysNum ).SuppHeatCoilName, FirstHVACIteration, UnitarySystem( UnitarySysNum ).SuppHeatCoilIndex, 1.0, _, UnitarySystem( UnitarySysNum ).FanOpMode, PartLoadFrac );
		}
		OutletAirTemp = Node( UnitarySystem( UnitarySysNum ).HeatCoilOutletNodeNum ).Temp;
		Residuum = Par( 3 ) - OutletAirTemp;

		return Residuum;
	}

	void
	FrostControlSetPointLimit(
		int const UnitarySysNum, // dx cooling coil system index
		Real64 & TempSetPoint, // temperature setpoint of the sensor node
		Real64 & HumRatSetPoint, // humidity ratio setpoint of the sensor node
		Real64 const BaroPress, // baromtric pressure, Pa [N/m^2]
		Real64 const TfrostControl, // minimum temperature limit for forst control
		int const ControlMode // temperature or humidity control mode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// Controls the forst formation condition based on user specified minimum DX coil outlet
		// air temperature. Resets the cooling setpoint based on the user specified limiting
		// temperature for frost control.
		// METHODOLOGY EMPLOYED:
		// Based on FrostControlSetPointLimit by Bereket Nigusse in HVACDXSystem
		// REFERENCES:
		//  na
		// Using/Aliasing
		using Psychrometrics::PsyWFnTdpPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const RunOnSensible( 1 ); // identifier for temperature (sensible load) control
		int const RunOnLatent( 2 ); // identifier for humidity (latent load) control
		static std::string const RoutineName( "FrostControlSetPointLimit" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HumRatioSat; // saturation humidity ratio at forst control temperature
		Real64 AirMassFlow; // air masss flow rate through the DX coil

		AirMassFlow = Node( UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum ).MassFlowRate;
		if ( ControlMode == RunOnSensible && AirMassFlow > MinAirMassFlow && TempSetPoint < Node( UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum ).Temp ) {
			if ( TempSetPoint < TfrostControl ) {
				TempSetPoint = TfrostControl;
				UnitarySystem( UnitarySysNum ).FrostControlStatus = 1;
			}
		} else if ( ControlMode == RunOnLatent && AirMassFlow > MinAirMassFlow && HumRatSetPoint < Node( UnitarySystem( UnitarySysNum ).CoolCoilInletNodeNum ).HumRat ) {
			HumRatioSat = PsyWFnTdpPb( TfrostControl, BaroPress, RoutineName );
			if ( HumRatioSat > HumRatSetPoint ) {
				HumRatSetPoint = HumRatioSat;
				UnitarySystem( UnitarySysNum ).FrostControlStatus = 2;
			}
		} else {
			UnitarySystem( UnitarySysNum ).FrostControlStatus = 0;
		}
	}

	void
	CheckUnitarySysCoilInOASysExists( std::string const & UnitarySysName )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// After making sure get input is done, checks if the Coil System DX coil is in the
		// OA System.  IF exists then the DX cooling coil is 100% DOAS DX coil.
		// METHODOLOGY EMPLOYED:
		// Based on CheckDXCoolingCoilInOASysExists by Bereket Nigusse in HVACDXSystem
		// REFERENCES:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DXCoils::SetDXCoilTypeData;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CheckUnitarySysCoilInOASysExists: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum;

		if ( GetInputFlag ) {
			GetUnitarySystemInput();
			GetInputFlag = false;
		}

		UnitarySysNum = 0;
		if ( NumUnitarySystem > 0 ) {
			UnitarySysNum = FindItemInList( UnitarySysName, UnitarySystem );
			if ( UnitarySysNum > 0 ) {
				if ( UnitarySystem( UnitarySysNum ).ISHundredPercentDOASDXCoil ) {
					SetDXCoilTypeData( UnitarySystem( UnitarySysNum ).CoolingCoilName );
				}
			} else {
				ShowSevereError( RoutineName + "System not found = AirloopHVAC:UnitarySystem \"" + UnitarySysName + "\"" );
			}
		} else {
			ShowSevereError( RoutineName + "System not found = AirloopHVAC:UnitarySystem \"" + UnitarySysName + "\"" );
		}

	}

	void
	GetUnitarySystemOAHeatCoolCoil(
		std::string const & UnitarySystemName, // Name of Unitary System object
		Optional_bool OACoolingCoil, // Cooling coil in OA stream
		Optional_bool OAHeatingCoil // Heating coil in OA stream
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Determined weather Unitary system in OA stream has heating or cooling coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum;

		if ( GetInputFlag ) { //First time subroutine has been entered
			GetUnitarySystemInput();
			GetInputFlag = false;
		}

		for ( UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum ) {
			if ( SameString( UnitarySystemName, UnitarySystem( UnitarySysNum ).Name ) ) {
				if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					OACoolingCoil = true;
				}
				if ( UnitarySystem( UnitarySysNum ).HeatCoilExists || UnitarySystem( UnitarySysNum ).SuppCoilExists ) {
					OAHeatingCoil = true;
				}
			}
		}

	}

	int
	GetUnitarySystemDXCoolingCoilIndex( std::string const & UnitarySystemName ) // Name of Unitary System object
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Find the DX cooling coil in this Unitary System

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Return value
		int GetUnitarySystemDXCoolingCoilIndex;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitarySysNum;

		if ( GetInputFlag ) { //First time subroutine has been entered
			GetUnitarySystemInput();
			GetInputFlag = false;
		}

		GetUnitarySystemDXCoolingCoilIndex = 0;
		for ( UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum ) {
			if ( SameString( UnitarySystemName, UnitarySystem( UnitarySysNum ).Name ) ) {
				if ( UnitarySystem( UnitarySysNum ).CoolCoilExists ) {
					GetUnitarySystemDXCoolingCoilIndex = UnitarySystem( UnitarySysNum ).CoolingCoilIndex;
				}
			}
		}

		return GetUnitarySystemDXCoolingCoilIndex;

	}

	// Clears the global data in HVACUnitarySystem.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		InitLoadBasedControlOneTimeFlag = true;
		InitLoadBasedControlAirLoopPass = true;
		AirLoopPassCounter = 0;
		InitLoadBasedControlFlowFracFlagReady = true;
		InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax = 0.0;
		InitUnitarySystemsQActual = 0.0;
		GetInputFlag = true;
		InitUnitarySystemsOneTimeFlag = true;
		SimUnitarySystemZoneEquipTestFlag = true;
		EconomizerFlag = false;
		HeatingLoad = false;
		CoolingLoad = false;
		MoistureLoad = 0.0;
		SuppHeatingCoilFlag = false;
		NumUnitarySystem = 0;
		NumDesignSpecMultiSpeedHP = 0;
		CompOnMassFlow = 0.0;
		CompOffMassFlow = 0.0;
		CompOnFlowRatio = 0.0;
		CompOffFlowRatio = 0.0;
		FanSpeedRatio = 0.0;
		CoolHeatPLRRat = 1.0;
		OnOffAirFlowRatioSave = 0.0;
		QToCoolSetPt = 0.0;
		QToHeatSetPt = 0.0;
		TempSteamIn = 100.0;
		NumUnitarySystemsSized = 0;

		// Allocatable types
		CheckEquipName.deallocate();
		MyEnvrnFlag.deallocate();
		MultiOrVarSpeedHeatCoil.deallocate();
		MultiOrVarSpeedCoolCoil.deallocate();
		DesignSpecMSHP.deallocate();
		UnitarySystem.deallocate();
		UnitarySystemNumericFields.deallocate();
		MyPlantScanFlag.deallocate();
		MySuppCoilPlantScanFlag.deallocate();
		MySetPointCheckFlag.deallocate();
		MySizingCheckFlag.deallocate();
	}
} // HVACUnitarySystem

} // EnergyPlus
