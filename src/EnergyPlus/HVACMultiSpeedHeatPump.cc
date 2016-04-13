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
#include <HVACMultiSpeedHeatPump.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataBranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DXCoils.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <HeatingCoils.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>
#include <ZoneTempPredictorCorrector.hh>


namespace EnergyPlus {

namespace HVACMultiSpeedHeatPump {

	// Module containing the Multi Speed Heat Pump simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Lixing Gu, Florida Solar Energy Center
	//       DATE WRITTEN   June 2007
	//       MODIFIED       Bereket Nigusse, FSEC, June 2010 - deprecated supply air flow fraction through controlled
	//                      zone from the furnace object input field. Now, the flow fraction is calculated internally
	//                      Brent Griffith, NREL, Dec 2010 -- upgrade to new plant for heat recovery, general fluid props.
	//                      Bereket Nigusse, FSEC, Jan. 2012 -- added hot water and steam heating coil

	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to simulate Multi Speed Heat Pump in
	// EnergyPlus.

	// Module currently models air-cooled or evap-cooled direct expansion systems
	// (split or packaged) with mulptiple speeds. Air-side performance is modeled to determine
	// coil discharge air conditions. The module also determines the DX unit's energy
	// usage. Neither the air-side performance nor the energy usage includes the effect
	// of supply air fan heat/energy usage. The supply air fan is modeled by other modules.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::CurMnDy;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::DayOfYear;
	using DataHVACGlobals::OnOffFanPartLoadFraction;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::DXElecCoolingPower;
	using DataHVACGlobals::DXElecHeatingPower;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::ElecHeatingCoilPower;
	using DataHVACGlobals::CycFanCycCoil;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::DrawThru;
	using DataHVACGlobals::BlowThru;
	using DataHVACGlobals::Coil_HeatingWater;
	using DataHVACGlobals::Coil_HeatingSteam;
	using DataHVACGlobals::Coil_HeatingGas;
	using DataHVACGlobals::Coil_HeatingElectric;
	using DataHVACGlobals::Coil_HeatingGas_MultiStage;
	using DataHVACGlobals::Coil_HeatingElectric_MultiStage;
	using DataZoneEnergyDemands::ZoneSysEnergyDemand;
	using namespace Psychrometrics;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS

	// Heating coil types
	int const MultiSpeedHeatingCoil( 1 ); // COIL:DX:MultiSpeed:Heating
	// Cooling coil types
	int const MultiSpeedCoolingCoil( 2 ); // COIL:DX:MultiSpeed:Cooling
	// Supplymental heating coil types
	int const SuppHeatingCoilGas( 1 ); // Supplymental heating coil type: COIL:GAS:HEATING
	int const SuppHeatingCoilElec( 2 ); // Supplymental heating coil type: COIL:ELECTRIC:HEATING
	int const SuppHeatingCoilRec( 3 ); // Supplymental heating coil type: COIL:ENGINEHEATRECOVERY:HEATING

	// Curve Types
	int const Linear( 1 ); // Linear curve type
	int const BiLinear( 2 ); // Bi-linear curve type
	int const Quadratic( 3 ); // Quadratic curve type
	int const BiQuadratic( 4 ); // Bi-quadratic curve type
	int const Cubic( 5 ); // Cubic curve type

	// Mode of operation
	int const CoolingMode( 1 ); // System operating mode is cooling
	int const HeatingMode( 2 ); // System operating mode is heating

	// Airflow control for contant fan mode
	int const UseCompressorOnFlow( 1 ); // set compressor OFF air flow rate equal to compressor ON air flow rate
	int const UseCompressorOffFlow( 2 ); // set compressor OFF air flow rate equal to user defined value
	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	static std::string const fluidNameSteam( "STEAM" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumMSHeatPumps( 0 ); // Number of multi speed heat pumps
	int AirLoopPass( 0 ); // Number of air loop pass
	Real64 TempSteamIn( 100.0 ); // steam coil steam inlet temperature

	std::string CurrentModuleObject; // Object type for getting and error messages
	Real64 CompOnMassFlow( 0.0 ); // System air mass flow rate w/ compressor ON
	Real64 CompOffMassFlow( 0.0 ); // System air mass flow rate w/ compressor OFF
	Real64 CompOnFlowRatio( 0.0 ); // fan flow ratio when coil on
	Real64 CompOffFlowRatio( 0.0 ); // fan flow ratio when coil off
	Real64 FanSpeedRatio( 0.0 ); // fan speed ratio passed to on/off fan object
	Real64 SupHeaterLoad( 0.0 ); // load to be met by supplemental heater [W]
	Real64 SaveLoadResidual( 0.0 ); // Saved load residual used to check convergence
	Real64 SaveCompressorPLR( 0.0 ); // holds compressor PLR from active DX coil
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< MSHeatPumpData > MSHeatPump;
	Array1D< MSHeatPumpReportData > MSHeatPumpReport;

	// Functions

	void
	SimMSHeatPump(
		std::string const & CompName, // Name of the unitary engine driven heat pump system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system time step
		int const AirLoopNum, // air loop index
		int & CompIndex // Index to changeover-bypass VAV system
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu, Florida Solar Energy Center
		//       DATE WRITTEN   June. 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of multispeed heat pump.

		// METHODOLOGY EMPLOYED:
		// NA

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
		int MSHeatPumpNum; // index of fan coil unit being simulated
		static bool GetInputFlag( true ); // Get input flag
		Real64 OnOffAirFlowRatio; // Ratio of compressor ON airflow to average airflow over timestep
		Real64 QZnLoad; // Zone load required by all zones served by this air loop system
		Real64 QSensUnitOut; // MSHP sensible capacity output [W]
		// FLOW

		// First time SimMSHeatPump is called, get the input
		if ( GetInputFlag ) {
			GetMSHeatPumpInput();
			GetInputFlag = false; // Set GetInputFlag false so you don't get coil inputs again
		}

		if ( CompIndex == 0 ) {
			MSHeatPumpNum = FindItemInList( CompName, MSHeatPump );
			if ( MSHeatPumpNum == 0 ) {
				ShowFatalError( "MultiSpeed Heat Pump is not found=" + CompName );
			}
			CompIndex = MSHeatPumpNum;
		} else {
			MSHeatPumpNum = CompIndex;
			if ( MSHeatPumpNum > NumMSHeatPumps || MSHeatPumpNum < 1 ) {
				ShowFatalError( "SimMSHeatPump: Invalid CompIndex passed=" + TrimSigDigits( MSHeatPumpNum ) + ", Number of MultiSpeed Heat Pumps=" + TrimSigDigits( NumMSHeatPumps ) + ", Heat Pump name=" + CompName );
			}
			if ( CheckEquipName( MSHeatPumpNum ) ) {
				if ( CompName != MSHeatPump( MSHeatPumpNum ).Name ) {
					ShowFatalError( "SimMSHeatPump: Invalid CompIndex passed=" + TrimSigDigits( MSHeatPumpNum ) + ", Heat Pump name=" + CompName + MSHeatPump( MSHeatPumpNum ).Name );
				}
				CheckEquipName( MSHeatPumpNum ) = false;
			}
		}

		OnOffAirFlowRatio = 0.0;

		// Initialize the engine driven heat pump
		InitMSHeatPump( MSHeatPumpNum, FirstHVACIteration, AirLoopNum, QZnLoad, OnOffAirFlowRatio );

		SimMSHP( MSHeatPumpNum, FirstHVACIteration, QSensUnitOut, QZnLoad, OnOffAirFlowRatio );

		// Update the unit outlet nodes
		UpdateMSHeatPump( MSHeatPumpNum );

		// Report the result of the simulation
		ReportMSHeatPump( MSHeatPumpNum );

	}

	//******************************************************************************

	void
	SimMSHP(
		int const MSHeatPumpNum, // number of the current engine driven Heat Pump being simulated
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QSensUnitOut, // cooling/heating deliveded to zones [W]
		Real64 const QZnReq, // required zone load
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  Revised based on SimPTHP

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a multispeed heat pump; adjust its output to match the
		// required system load.

		// METHODOLOGY EMPLOYED:
		// Calls ControlMSHPOutput to obtain the desired unit output

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataAirLoop::AirLoopControlInfo;

		// Locals
		Real64 SupHeaterLoad;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PartLoadFrac; // compressor part load fraction
		Real64 SpeedRatio; // compressor speed ratio
		bool UnitOn; // TRUE if unit is on
		int OutletNode; // MSHP air outlet node
		int InletNode; // MSHP air inlet node
		Real64 AirMassFlow; // air mass flow rate [kg/s]
		int OpMode; // operating mode (fan cycling or continious; DX coil always cycles)
		int ZoneNum; // Controlled zone number
		Real64 QTotUnitOut;
		int SpeedNum; // Speed number
		int CompOp; // compressor operation; 1=on, 0=off
		int AirLoopNumber; // Index to air loop
		Real64 SaveMassFlowRate; // saved inlet air mass flow rate [kg/s]

		// zero the fan, DX coils, and supplemental electric heater electricity consumption
		FanElecPower = 0.0;
		DXElecHeatingPower = 0.0;
		DXElecCoolingPower = 0.0;
		SaveCompressorPLR = 0.0;
		ElecHeatingCoilPower = 0.0;

		// initialize local variables
		UnitOn = true;
		OutletNode = MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum;
		InletNode = MSHeatPump( MSHeatPumpNum ).AirInletNodeNum;
		AirMassFlow = Node( InletNode ).MassFlowRate;
		OpMode = MSHeatPump( MSHeatPumpNum ).OpMode;
		ZoneNum = MSHeatPump( MSHeatPumpNum ).ControlZoneNum;
		CompOp = On;

		// set the on/off flags
		if ( MSHeatPump( MSHeatPumpNum ).OpMode == CycFanCycCoil ) {
			// cycling unit only runs if there is a cooling or heating load.
			if ( std::abs( QZnReq ) < SmallLoad || AirMassFlow < SmallMassFlow || CurDeadBandOrSetback( ZoneNum ) ) {
				UnitOn = false;
			}
		} else if ( MSHeatPump( MSHeatPumpNum ).OpMode == ContFanCycCoil ) {
			// continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
			if ( AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
			}
		}

		OnOffFanPartLoadFraction = 1.0;

		AirLoopNumber = ZoneEquipConfig( MSHeatPump( MSHeatPumpNum ).ControlZoneNum ).AirLoopNum;
		SaveMassFlowRate = Node( InletNode ).MassFlowRate;
		if ( ! FirstHVACIteration && MSHeatPump( MSHeatPumpNum ).OpMode == CycFanCycCoil && QZnReq < 0.0 && AirLoopControlInfo( AirLoopNumber ).EconoActive ) {
			// for cycling fan, cooling load, check whether furnace can meet load with compressor off
			CompOp = Off;
			ControlMSHPOutput( MSHeatPumpNum, FirstHVACIteration, CompOp, OpMode, QZnReq, ZoneNum, SpeedNum, SpeedRatio, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad );
			if ( SpeedNum == MSHeatPump( MSHeatPumpNum ).NumOfSpeedCooling && SpeedRatio == 1.0 ) {
				// compressor on (reset inlet air mass flow rate to starting value)
				Node( InletNode ).MassFlowRate = SaveMassFlowRate;
				CompOp = On;
				ControlMSHPOutput( MSHeatPumpNum, FirstHVACIteration, CompOp, OpMode, QZnReq, ZoneNum, SpeedNum, SpeedRatio, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad );
			}
		} else {
			// compressor on
			ControlMSHPOutput( MSHeatPumpNum, FirstHVACIteration, CompOp, OpMode, QZnReq, ZoneNum, SpeedNum, SpeedRatio, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad );
		}

		if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType != MultiSpeedHeatingCoil ) {
			SaveCompressorPLR = PartLoadFrac;
		} else {
			if ( SpeedNum > 1 ) {
				SaveCompressorPLR = 1.0;
			}

			if ( PartLoadFrac == 1.0 && SaveCompressorPLR < 1.0 && ( ! MSHeatPump( MSHeatPumpNum ).Staged ) ) {
				PartLoadFrac = SaveCompressorPLR;
			}
		}

		CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );

		// calculate delivered capacity
		AirMassFlow = Node( InletNode ).MassFlowRate;

		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( MSHeatPump( MSHeatPumpNum ).NodeNumOfControlledZone ).Enthalpy );

		// report variables
		MSHeatPump( MSHeatPumpNum ).CompPartLoadRatio = SaveCompressorPLR;
		if ( MSHeatPump( MSHeatPumpNum ).OpMode == CycFanCycCoil ) {
			if ( SupHeaterLoad > 0.0 ) {
				MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio = 1.0;
			} else {
				if ( SpeedNum < 2 ) {
					MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio = PartLoadFrac;
				} else {
					MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio = 1.0;
				}
			}
		} else {
			if ( UnitOn ) {
				MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio = 1.0;
			} else {
				if ( SpeedNum < 2 ) {
					MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio = PartLoadFrac;
				} else {
					MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio = 1.0;
				}
			}
		}

		if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == HeatingMode ) {
			MSHeatPump( MSHeatPumpNum ).TotHeatEnergyRate = std::abs( max( 0.0, QTotUnitOut ) );
			MSHeatPump( MSHeatPumpNum ).SensHeatEnergyRate = std::abs( max( 0.0, QSensUnitOut ) );
			MSHeatPump( MSHeatPumpNum ).LatHeatEnergyRate = std::abs( max( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
			MSHeatPump( MSHeatPumpNum ).TotCoolEnergyRate = 0.0;
			MSHeatPump( MSHeatPumpNum ).SensCoolEnergyRate = 0.0;
			MSHeatPump( MSHeatPumpNum ).LatCoolEnergyRate = 0.0;
		}
		if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == CoolingMode ) {
			MSHeatPump( MSHeatPumpNum ).TotCoolEnergyRate = std::abs( min( 0.0, QTotUnitOut ) );
			MSHeatPump( MSHeatPumpNum ).SensCoolEnergyRate = std::abs( min( 0.0, QSensUnitOut ) );
			MSHeatPump( MSHeatPumpNum ).LatCoolEnergyRate = std::abs( min( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
			MSHeatPump( MSHeatPumpNum ).TotHeatEnergyRate = 0.0;
			MSHeatPump( MSHeatPumpNum ).SensHeatEnergyRate = 0.0;
			MSHeatPump( MSHeatPumpNum ).LatHeatEnergyRate = 0.0;
		}

		MSHeatPump( MSHeatPumpNum ).AuxElecPower = MSHeatPump( MSHeatPumpNum ).AuxOnCyclePower * SaveCompressorPLR + MSHeatPump( MSHeatPumpNum ).AuxOffCyclePower * ( 1.0 - SaveCompressorPLR );
		if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType != MultiSpeedHeatingCoil ) {
			{ auto const SELECT_CASE_var( MSHeatPump( MSHeatPumpNum ).HeatCoilType );
			if ( ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) || ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) ) {
				MSHeatPump( MSHeatPumpNum ).ElecPower = FanElecPower + DXElecCoolingPower + ElecHeatingCoilPower;
			} else if ( ( SELECT_CASE_var == Coil_HeatingWater ) || ( SELECT_CASE_var == Coil_HeatingSteam ) ) {
				MSHeatPump( MSHeatPumpNum ).ElecPower = FanElecPower + DXElecCoolingPower;
			} else {
			}}
		} else {
			MSHeatPump( MSHeatPumpNum ).ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + ElecHeatingCoilPower + MSHeatPump( MSHeatPumpNum ).AuxElecPower;
		}

	}

	//******************************************************************************

	void
	GetMSHeatPumpInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    July 2007
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will get the input required by the multispeed heat pump model

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::GetObjectDefMaxArgs;
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using FluidProperties::FindGlycol;
		using CurveManager::CurveValue;
		using General::RoundSigDigits;
		using GlobalNames::VerifyUniqueChillerName;
		using DataSizing::AutoSize;
		using Fans::GetFanType;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using DataHeatBalance::Zone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataAirSystems::PrimaryAirSystem;
		using DataZoneControls::TempControlledZone;
		using DataZoneControls::NumTempControlledZones;
		using DataZoneControls::NumComfortControlledZones;
		using DataZoneControls::ComfortControlledZone;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataHVACGlobals::FanType_SimpleConstVolume;

		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using BranchNodeConnections::SetUpCompSets;
		using DXCoils::GetDXCoilIndex;
		auto & GetDXCoilInletNode( DXCoils::GetCoilInletNode );
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		using DXCoils::GetDXCoilNumberOfSpeeds;
		auto & GetHeatingCoilInletNode( HeatingCoils::GetCoilInletNode );
		auto & GetHeatingCoilOutletNode( HeatingCoils::GetCoilOutletNode );
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );
		using HeatingCoils::GetHeatingCoilIndex;
		using HeatingCoils::GetCoilIndex;
		using HeatingCoils::GetCoilInletNode;
		using HeatingCoils::GetCoilOutletNode;
		using HeatingCoils::GetHeatingCoilNumberOfStages;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		auto & GetWaterCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWaterCoilOutletNode( WaterCoils::GetCoilOutletNode );
		auto & GetSteamCoilAirInletNode( SteamCoils::GetCoilAirInletNode );
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilAirOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using SteamCoils::GetTypeOfCoil;
		using FluidProperties::GetSatDensityRefrig;
		using ZoneTempPredictorCorrector::NumStageCtrZone;
		using DataZoneControls::StageControlledZone;
		using DXCoils::SetMSHPDXCoilHeatRecoveryFlag;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetMSHeatPumpInput: " ); // include trailing blank space
		static std::string const RoutineNameNoColon( "GetMSHeatPumpInput" );

		// LOCAL VARIABLES
		int MSHPNum; // Engine driven heat pump count
		int NumAlphas; // Number of elements in the alpha array
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // True when input errors found
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool AllocatedFlag( false ); // True when arrays are allocated
		bool AirNodeFound; // True when an air node is found
		bool AirLoopFound; // True when an air loop is found
		int ControlledZoneNum; // Controlled zone number
		int AirLoopNumber; // Index to air loop
		int FanType; // Fan type
		int BranchNum; // Index to branch
		int CompNum; // Index to component
		int TstatZoneNum; // Used to determine if control zone has a thermostat object
		int i; // Index to speeds
		int j; // Index to speeds
		bool Found; // Flag to find autosize
		int HeatingCoilInletNode; // Heating coil inlet node number
		int HeatingCoilOutletNode; // Heating coil outlet node number
		int CoolingCoilInletNode; // Cooling coil inlet node number
		int CoolingCoilOutletNode; // Cooling coil outlet node number
		int SuppHeatCoilInletNode; // Supplemental heating coil inlet node number
		int SuppHeatCoilOutletNode; // Supplemental heating coil outlet node number
		bool LocalError; // Local error flag
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file
		bool errFlag;
		int SteamIndex; // steam coil steam inlet density
		Real64 SteamDensity; // density of steam at 100C

		// FLOW

		if ( AllocatedFlag ) return;

		CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed"; // Object type for getting and error messages

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		MaxNums = max( MaxNums, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		Numbers.dimension( MaxNums, 0.0 );
		cNumericFields.allocate( MaxNums );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		NumMSHeatPumps = GetNumObjectsFound( CurrentModuleObject );

		if ( NumMSHeatPumps <= 0 ) {
			ShowSevereError( "No " + CurrentModuleObject + " objects specified in input file." );
			ErrorsFound = true;
		}

		// ALLOCATE ARRAYS
		MSHeatPump.allocate( NumMSHeatPumps );
		MSHeatPumpReport.allocate( NumMSHeatPumps );
		CheckEquipName.dimension( NumMSHeatPumps, true );
		AllocatedFlag = true;

		// Load arrays with reformulated electric EIR chiller data
		for ( MSHPNum = 1; MSHPNum <= NumMSHeatPumps; ++MSHPNum ) {

			HeatingCoilInletNode = 0;
			HeatingCoilOutletNode = 0;
			CoolingCoilInletNode = 0;
			CoolingCoilOutletNode = 0;
			SuppHeatCoilInletNode = 0;
			SuppHeatCoilOutletNode = 0;

			GetObjectItem( CurrentModuleObject, MSHPNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), MSHeatPump, MSHPNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			MSHeatPump( MSHPNum ).Name = Alphas( 1 );
			if ( lAlphaBlanks( 2 ) ) {
				MSHeatPump( MSHPNum ).AvaiSchedPtr = ScheduleAlwaysOn;
			} else {
				MSHeatPump( MSHPNum ).AvaiSchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( MSHeatPump( MSHPNum ).AvaiSchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\" " + cAlphaFields( 2 ) + " not found: " + Alphas( 2 ) );
					ErrorsFound = true;
				}
			}

			MSHeatPump( MSHPNum ).AirInletNodeName = Alphas( 3 );
			MSHeatPump( MSHPNum ).AirOutletNodeName = Alphas( 4 );
			MSHeatPump( MSHPNum ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			MSHeatPump( MSHPNum ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			//Get the Controlling Zone or Location of the engine driven heat pump Thermostat
			MSHeatPump( MSHPNum ).ControlZoneNum = FindItemInList( Alphas( 5 ), Zone );
			MSHeatPump( MSHPNum ).ControlZoneName = Alphas( 5 );
			if ( MSHeatPump( MSHPNum ).ControlZoneNum == 0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\" " + cAlphaFields( 5 ) + " not found: " + MSHeatPump( MSHPNum ).ControlZoneName );
				ErrorsFound = true;
			}

			// Get the node number for the zone with the thermostat
			if ( MSHeatPump( MSHPNum ).ControlZoneNum > 0 ) {
				AirNodeFound = false;
				AirLoopFound = false;
				for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
					if ( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum != MSHeatPump( MSHPNum ).ControlZoneNum ) continue;
					// Find the controlled zone number for the specified thermostat location
					MSHeatPump( MSHPNum ).NodeNumOfControlledZone = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
					// Determine if furnace is on air loop served by the thermostat location specified
					AirLoopNumber = ZoneEquipConfig( ControlledZoneNum ).AirLoopNum;
					if ( AirLoopNumber > 0 ) {
						for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNumber ).NumBranches; ++BranchNum ) {
							for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNumber ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
								if ( ! SameString( PrimaryAirSystem( AirLoopNumber ).Branch( BranchNum ).Comp( CompNum ).Name, MSHeatPump( MSHPNum ).Name ) || ! SameString( PrimaryAirSystem( AirLoopNumber ).Branch( BranchNum ).Comp( CompNum ).TypeOf, CurrentModuleObject ) ) continue;
								AirLoopFound = true;
								break;
							}
							if ( AirLoopFound ) break;
						}
						for ( TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum ) {
							if ( TempControlledZone( TstatZoneNum ).ActualZoneNum != MSHeatPump( MSHPNum ).ControlZoneNum ) continue;
							AirNodeFound = true;
						}
						for ( TstatZoneNum = 1; TstatZoneNum <= NumComfortControlledZones; ++TstatZoneNum ) {
							if ( ComfortControlledZone( TstatZoneNum ).ActualZoneNum != MSHeatPump( MSHPNum ).ControlZoneNum ) continue;
							AirNodeFound = true;
						}
						for ( TstatZoneNum = 1; TstatZoneNum <= NumStageCtrZone; ++TstatZoneNum ) {
							if ( StageControlledZone( TstatZoneNum ).ActualZoneNum != MSHeatPump( MSHPNum ).ControlZoneNum ) continue;
							AirNodeFound = true;
						}
					} else {
						ShowSevereError( "Did not find a AirLoopHVAC for " + CurrentModuleObject + " = \"\"" + MSHeatPump( MSHPNum ).Name );
						ShowContinueError( "Specified " + cAlphaFields( 5 ) + " = " + Alphas( 5 ) );
						ErrorsFound = true;
					}
					break;
				}
				if ( ! AirNodeFound ) {
					ShowSevereError( "Did not find Air Node (" + cAlphaFields( 5 ) + "), " + CurrentModuleObject + " = \"\"" + MSHeatPump( MSHPNum ).Name );
					ShowContinueError( "Specified " + cAlphaFields( 5 ) + " = " + Alphas( 5 ) );
					ErrorsFound = true;
				}
				if ( ! AirLoopFound ) {
					ShowSevereError( "Did not find correct AirLoopHVAC for " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
					ShowContinueError( "The " + cAlphaFields( 5 ) + " = " + Alphas( 5 ) + " is not served by this Primary Air Loop equipment." );
					ErrorsFound = true;
				}
			}

			//    MSHeatPump(MSHPNum)%FlowFraction = Numbers(1)
			//    IF (MSHeatPump(MSHPNum)%FlowFraction .LE. 0.0 .AND. MSHeatPump(MSHPNum)%FlowFraction /= AutoSize) THEN
			//      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
			//                           '", '//TRIM(cNumericFields(1))//' must greater than zero.')
			//      ErrorsFound = .TRUE.
			//    END IF
			//    IF (MSHeatPump(MSHPNum)%FlowFraction .GT. 1.0 .AND. MSHeatPump(MSHPNum)%FlowFraction /= AutoSize) THEN
			//      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
			//                           '", '//TRIM(cNumericFields(1))//' cannot be greater than 1.0.')
			//      ErrorsFound = .TRUE.
			//    END IF

			//Get supply fan data
			MSHeatPump( MSHPNum ).FanName = Alphas( 7 );
			if ( SameString( Alphas( 6 ), "Fan:OnOff" ) || SameString( Alphas( 6 ), "Fan:ConstantVolume" ) ) {
				if ( SameString( Alphas( 6 ), "Fan:OnOff" ) ) {
					MSHeatPump( MSHPNum ).FanType = FanType_SimpleOnOff;
					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Fan:OnOff", MSHeatPump( MSHPNum ).FanName, "UNDEFINED", "UNDEFINED" );
					MSHeatPump( MSHPNum ).FanInletNode = GetFanInletNode( "Fan:OnOff", MSHeatPump( MSHPNum ).FanName, ErrorsFound );
					MSHeatPump( MSHPNum ).FanOutletNode = GetFanOutletNode( "Fan:OnOff", MSHeatPump( MSHPNum ).FanName, ErrorsFound );
				} else {
					MSHeatPump( MSHPNum ).FanType = FanType_SimpleConstVolume;
					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Fan:ConstantVolume", MSHeatPump( MSHPNum ).FanName, "UNDEFINED", "UNDEFINED" );
					MSHeatPump( MSHPNum ).FanInletNode = GetFanInletNode( "Fan:ConstantVolume", MSHeatPump( MSHPNum ).FanName, ErrorsFound );
					MSHeatPump( MSHPNum ).FanOutletNode = GetFanOutletNode( "Fan:ConstantVolume", MSHeatPump( MSHPNum ).FanName, ErrorsFound );
				}
				GetFanIndex( Alphas( 7 ), MSHeatPump( MSHPNum ).FanNum, ErrorsFound, CurrentModuleObject );
				GetFanType( Alphas( 7 ), FanType, ErrorsFound );
				if ( FanType != MSHeatPump( MSHPNum ).FanType ) {
					ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cAlphaFields( 6 ) + " and " + cAlphaFields( 7 ) + " do not match in Fan objects." );
					ShowContinueError( "The entered " + cAlphaFields( 7 ) + " = " + Alphas( 7 ) + " and " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cAlphaFields( 6 ) + " is not allowed = " + Alphas( 6 ) );
				ShowContinueError( "Valid choices are Fan:OnOff or Fan:ConstantVolume" );
				ErrorsFound = true;
			}

			//Get supply fan placement data
			if ( SameString( Alphas( 8 ), "BlowThrough" ) || SameString( Alphas( 8 ), "DrawThrough" ) ) {
				if ( SameString( Alphas( 8 ), "BlowThrough" ) ) {
					MSHeatPump( MSHPNum ).FanPlaceType = BlowThru;
				} else {
					MSHeatPump( MSHPNum ).FanPlaceType = DrawThru;
				}
			} else {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cAlphaFields( 8 ) + " is not allowed = " + Alphas( 8 ) );
				ShowContinueError( "Valid choices are BlowThrough or DrawThrough" );
				ErrorsFound = true;
			}

			MSHeatPump( MSHPNum ).FanSchedule = Alphas( 9 );
			MSHeatPump( MSHPNum ).FanSchedPtr = GetScheduleIndex( Alphas( 9 ) );
			if ( MSHeatPump( MSHPNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\" " + cAlphaFields( 9 ) + " not found: " + Alphas( 9 ) );
				ErrorsFound = true;
			}

			if ( MSHeatPump( MSHPNum ).FanSchedPtr > 0 && MSHeatPump( MSHPNum ).FanType == FanType_SimpleConstVolume ) {
				if ( ! CheckScheduleValueMinMax( MSHeatPump( MSHPNum ).FanSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( cAlphaFields( 9 ) + " must be continuous (fan operating mode schedule values > 0) for " + cAlphaFields( 6 ) + " = Fan:ConstantVolume." );
					ShowContinueError( "Error found in " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
					ShowContinueError( "schedule values must be (>0., <=1.)" );
					ErrorsFound = true;
				}
			}

			if ( SameString( Alphas( 10 ), "Coil:Heating:DX:MultiSpeed" ) ) {
				MSHeatPump( MSHPNum ).HeatCoilType = MultiSpeedHeatingCoil;
				MSHeatPump( MSHPNum ).HeatCoilNum = GetObjectItemNum( "Coil:Heating:DX:MultiSpeed", Alphas( 11 ) );
				MSHeatPump( MSHPNum ).DXHeatCoilName = Alphas( 11 );
				if ( MSHeatPump( MSHPNum ).HeatCoilNum <= 0 ) {
					ShowSevereError( "Configuration error in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ShowContinueError( cAlphaFields( 11 ) + " \"" + Alphas( 11 ) + "\" not found." );
					ShowContinueError( cAlphaFields( 10 ) + " must be Coil:Heating:DX:MultiSpeed " );
					ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
					ErrorsFound = true;
				}
				LocalError = false;
				GetDXCoilIndex( MSHeatPump( MSHPNum ).DXHeatCoilName, MSHeatPump( MSHPNum ).DXHeatCoilIndex, LocalError, "Coil:Heating:DX:MultiSpeed" );
				if ( LocalError ) {
					ShowSevereError( "The index of " + cAlphaFields( 11 ) + " is not found \"" + Alphas( 11 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				HeatingCoilInletNode = GetDXCoilInletNode( Alphas( 10 ), Alphas( 11 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The inlet node number of " + cAlphaFields( 11 ) + " is not found \"" + Alphas( 11 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				HeatingCoilOutletNode = GetDXCoilOutletNode( Alphas( 10 ), Alphas( 11 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The outlet node number of " + cAlphaFields( 11 ) + " is not found \"" + Alphas( 11 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:DX:MultiSpeed", MSHeatPump( MSHPNum ).DXHeatCoilName, "UNDEFINED", "UNDEFINED" );
			} else if ( SameString( Alphas( 10 ), "Coil:Heating:Electric:MultiStage" ) || SameString( Alphas( 10 ), "Coil:Heating:Gas:MultiStage" ) ) {

				if ( SameString( Alphas( 10 ), "Coil:Heating:Electric:MultiStage" ) ) {
					MSHeatPump( MSHPNum ).HeatCoilType = Coil_HeatingElectric_MultiStage;
					MSHeatPump( MSHPNum ).HeatCoilNum = GetObjectItemNum( "Coil:Heating:Electric:MultiStage", Alphas( 11 ) );
					if ( MSHeatPump( MSHPNum ).HeatCoilNum <= 0 ) {
						ShowSevereError( "Configuration error in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
						ShowContinueError( cAlphaFields( 11 ) + " \"" + Alphas( 11 ) + "\" not found." );
						ShowContinueError( cAlphaFields( 10 ) + " must be Coil:Heating:Electric:MultiStage " );
						ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
						ErrorsFound = true;
					}
				} else {
					MSHeatPump( MSHPNum ).HeatCoilType = Coil_HeatingGas_MultiStage;
					MSHeatPump( MSHPNum ).HeatCoilNum = GetObjectItemNum( "Coil:Heating:Gas:MultiStage", Alphas( 11 ) );
					if ( MSHeatPump( MSHPNum ).HeatCoilNum <= 0 ) {
						ShowSevereError( "Configuration error in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
						ShowContinueError( cAlphaFields( 11 ) + " \"" + Alphas( 11 ) + "\" not found." );
						ShowContinueError( cAlphaFields( 10 ) + " must be Coil:Heating:Gas:MultiStage " );
						ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
						ErrorsFound = true;
					}
				}
				MSHeatPump( MSHPNum ).HeatCoilName = Alphas( 11 );
				LocalError = false;
				if ( SameString( Alphas( 10 ), "Coil:Heating:Electric:MultiStage" ) ) {
					GetCoilIndex( MSHeatPump( MSHPNum ).HeatCoilName, MSHeatPump( MSHPNum ).HeatCoilIndex, LocalError );
				} else {
					GetCoilIndex( MSHeatPump( MSHPNum ).HeatCoilName, MSHeatPump( MSHPNum ).HeatCoilIndex, LocalError );
				}
				if ( LocalError ) {
					ShowSevereError( "The index of " + cAlphaFields( 11 ) + " is not found \"" + Alphas( 11 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				HeatingCoilInletNode = GetCoilInletNode( Alphas( 10 ), Alphas( 11 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The inlet node number of " + cAlphaFields( 11 ) + " is not found \"" + Alphas( 11 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				HeatingCoilOutletNode = GetCoilOutletNode( Alphas( 10 ), Alphas( 11 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The outlet node number of " + cAlphaFields( 11 ) + " is not found \"" + Alphas( 11 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				if ( SameString( Alphas( 10 ), "Coil:Heating:Electric:MultiStage" ) ) {
					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Electric:MultiStage", MSHeatPump( MSHPNum ).HeatCoilName, "UNDEFINED", "UNDEFINED" );
				} else {
					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Gas:MultiStage", MSHeatPump( MSHPNum ).HeatCoilName, "UNDEFINED", "UNDEFINED" );
				}
			} else if ( SameString( Alphas( 10 ), "Coil:Heating:Water" ) ) {
				MSHeatPump( MSHPNum ).HeatCoilType = Coil_HeatingWater;
				ValidateComponent( Alphas( 10 ), Alphas( 11 ), IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					MSHeatPump( MSHPNum ).HeatCoilName = Alphas( 11 );
					// Get the Heating Coil water Inlet or control Node number
					errFlag = false;
					MSHeatPump( MSHPNum ).CoilControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the ReHeat Coil hot water max volume flow rate
					errFlag = false;
					MSHeatPump( MSHPNum ).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the lemental Heating Coil Inlet Node
					errFlag = false;
					HeatingCoilInletNode = GetWaterCoilInletNode( "Coil:Heating:Water", MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					MSHeatPump( MSHPNum ).CoilAirInletNode = HeatingCoilInletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the lemental Heating Coil Outlet Node
					errFlag = false;
					HeatingCoilOutletNode = GetWaterCoilOutletNode( "Coil:Heating:Water", MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}
					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Water", MSHeatPump( MSHPNum ).HeatCoilName, NodeID( HeatingCoilInletNode ), NodeID( HeatingCoilOutletNode ) );
				}
			} else if ( SameString( Alphas( 10 ), "Coil:Heating:Steam" ) ) {
				MSHeatPump( MSHPNum ).HeatCoilType = Coil_HeatingSteam;
				ValidateComponent( Alphas( 10 ), Alphas( 11 ), IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					MSHeatPump( MSHPNum ).HeatCoilName = Alphas( 11 );
					errFlag = false;
					MSHeatPump( MSHPNum ).HeatCoilNum = GetSteamCoilIndex( Alphas( 10 ), MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					if ( MSHeatPump( MSHPNum ).HeatCoilNum == 0 ) {
						ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 10 ) + " = " + MSHeatPump( MSHPNum ).HeatCoilName );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the lemental Heating Coil steam inlet node number
					errFlag = false;
					MSHeatPump( MSHPNum ).CoilControlNode = GetCoilAirOutletNode( "Coil:Heating:Steam", MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the lemental Heating Coil steam max volume flow rate
					MSHeatPump( MSHPNum ).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate( MSHeatPump( MSHPNum ).HeatCoilNum, errFlag );
					if ( MSHeatPump( MSHPNum ).MaxCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNameNoColon );
						MSHeatPump( MSHPNum ).MaxCoilFluidFlow *= SteamDensity;
					}

					// Get the lemental Heating Coil Inlet Node
					errFlag = false;
					HeatingCoilInletNode = GetSteamCoilAirInletNode( MSHeatPump( MSHPNum ).HeatCoilNum, MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					MSHeatPump( MSHPNum ).CoilAirInletNode = HeatingCoilInletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the lemental Heating Coil Outlet Node
					errFlag = false;
					HeatingCoilOutletNode = GetCoilAirOutletNode( MSHeatPump( MSHPNum ).HeatCoilNum, MSHeatPump( MSHPNum ).HeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Steam", MSHeatPump( MSHPNum ).HeatCoilName, NodeID( HeatingCoilInletNode ), NodeID( HeatingCoilOutletNode ) );

				}
			} else {
				ShowSevereError( "The allowed " + cAlphaFields( 10 ) + " are Coil:Heating:DX:MultiSpeed, Coil:Heating:Electric:MultiStage, and Coil:Heating:Gas:MultiStage  in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "The entered " + cAlphaFields( 10 ) + " = \"" + Alphas( 10 ) + "\"." );
				ErrorsFound = true;
			}

			MSHeatPump( MSHPNum ).MinOATCompressor = Numbers( 1 );

			if ( SameString( Alphas( 12 ), "Coil:Cooling:DX:MultiSpeed" ) ) {
				MSHeatPump( MSHPNum ).CoolCoilType = MultiSpeedCoolingCoil;
				MSHeatPump( MSHPNum ).CoolCoilNum = GetObjectItemNum( "Coil:Cooling:DX:MultiSpeed", Alphas( 13 ) );
				MSHeatPump( MSHPNum ).DXCoolCoilName = Alphas( 13 );
				if ( MSHeatPump( MSHPNum ).CoolCoilNum <= 0 ) {
					ShowSevereError( "Configuration error in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ShowContinueError( cAlphaFields( 13 ) + " \"" + Alphas( 13 ) + "\" not found." );
					ShowContinueError( cAlphaFields( 12 ) + " must be Coil:Cooling:DX:MultiSpeed " );
					ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
					ErrorsFound = true;
				}
				LocalError = false;
				GetDXCoilIndex( MSHeatPump( MSHPNum ).DXCoolCoilName, MSHeatPump( MSHPNum ).DXCoolCoilIndex, LocalError, "Coil:Cooling:DX:MultiSpeed" );
				if ( LocalError ) {
					ShowSevereError( "The index of " + cAlphaFields( 13 ) + " is not found \"" + Alphas( 13 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				CoolingCoilInletNode = GetDXCoilInletNode( Alphas( 12 ), Alphas( 13 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The inlet node number of " + cAlphaFields( 13 ) + " is not found \"" + Alphas( 13 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				CoolingCoilOutletNode = GetDXCoilOutletNode( Alphas( 12 ), Alphas( 13 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The outlet node number of " + cAlphaFields( 13 ) + " is not found \"" + Alphas( 13 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
			} else {
				ShowSevereError( "The allowed " + cAlphaFields( 12 ) + " is Coil:Cooling:DX:MultiSpeed in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "The entered " + cAlphaFields( 12 ) + " = \"" + Alphas( 12 ) + "\"." );
				ErrorsFound = true;
			}
			SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Cooling:DX:MultiSpeed", MSHeatPump( MSHPNum ).DXCoolCoilName, "UNDEFINED", "UNDEFINED" );

			// Get supplemental heating coil data
			MSHeatPump( MSHPNum ).SuppHeatCoilName = Alphas( 15 );
			if ( SameString( Alphas( 14 ), "Coil:Heating:Gas" ) ) {
				MSHeatPump( MSHPNum ).SuppHeatCoilType = SuppHeatingCoilGas;
				errFlag = false;
				MSHeatPump( MSHPNum ).SuppHeatCoilNum = GetHeatingCoilIndex( "Coil:Heating:Gas", Alphas( 15 ), errFlag );
				if ( MSHeatPump( MSHPNum ).SuppHeatCoilNum <= 0 || errFlag ) {
					ShowContinueError( "Configuration error in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ShowContinueError( cAlphaFields( 15 ) + " of type Coil:Heating:Gas \"" + Alphas( 15 ) + "\" not found." );
					ErrorsFound = true;
				}

				// Get the Supplemental Heating Coil Node Numbers
				LocalError = false;
				SuppHeatCoilInletNode = GetHeatingCoilInletNode( Alphas( 14 ), Alphas( 15 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The inlet node number of " + cAlphaFields( 15 ) + " is not found \"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				SuppHeatCoilOutletNode = GetHeatingCoilOutletNode( Alphas( 14 ), Alphas( 15 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The outlet node number of " + cAlphaFields( 15 ) + " is not found \"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}

				// Get supplemental heating coil capacity to see if it is autosize
				MSHeatPump( MSHPNum ).DesignSuppHeatingCapacity = GetHeatingCoilCapacity( Alphas( 14 ), Alphas( 15 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The capacity " + cAlphaFields( 15 ) + " is not found \"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Gas", MSHeatPump( MSHPNum ).SuppHeatCoilName, "UNDEFINED", "UNDEFINED" );
			}
			if ( SameString( Alphas( 14 ), "Coil:Heating:Electric" ) ) {
				MSHeatPump( MSHPNum ).SuppHeatCoilType = SuppHeatingCoilElec;
				errFlag = false;
				MSHeatPump( MSHPNum ).SuppHeatCoilNum = GetHeatingCoilIndex( "Coil:Heating:Electric", Alphas( 15 ), errFlag );
				if ( MSHeatPump( MSHPNum ).SuppHeatCoilNum <= 0 || errFlag ) {
					ShowContinueError( "Configuration error in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ShowContinueError( cAlphaFields( 15 ) + " of type Coil:Heating:Electric \"" + Alphas( 15 ) + "\" not found." );
					ErrorsFound = true;
				}

				// Get the Supplemental Heating Coil Node Numbers
				LocalError = false;
				SuppHeatCoilInletNode = GetHeatingCoilInletNode( Alphas( 14 ), Alphas( 15 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The inlet node number of " + cAlphaFields( 15 ) + " is not found \"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}
				SuppHeatCoilOutletNode = GetHeatingCoilOutletNode( Alphas( 14 ), Alphas( 15 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The outlet node number of " + cAlphaFields( 15 ) + " is not found \"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}

				// Get supplemental heating coil capacity to see if it is autosize
				MSHeatPump( MSHPNum ).DesignSuppHeatingCapacity = GetHeatingCoilCapacity( Alphas( 14 ), Alphas( 15 ), LocalError );
				if ( LocalError ) {
					ShowSevereError( "The capacity " + cAlphaFields( 15 ) + " is not found \"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + Alphas( 1 ) + "\"" );
					ErrorsFound = true;
					LocalError = false;
				}

				SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Electric", MSHeatPump( MSHPNum ).SuppHeatCoilName, "UNDEFINED", "UNDEFINED" );
			}

			if ( SameString( Alphas( 14 ), "Coil:Heating:Water" ) ) {
				MSHeatPump( MSHPNum ).SuppHeatCoilType = Coil_HeatingWater;
				ValidateComponent( Alphas( 14 ), MSHeatPump( MSHPNum ).SuppHeatCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					// Get the Heating Coil water Inlet or control Node number
					errFlag = false;
					MSHeatPump( MSHPNum ).SuppCoilControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the ReHeat Coil hot water max volume flow rate
					errFlag = false;
					MSHeatPump( MSHPNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the Supplemental Heating Coil Inlet Node
					errFlag = false;
					SuppHeatCoilInletNode = GetWaterCoilInletNode( "Coil:Heating:Water", MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					MSHeatPump( MSHPNum ).SuppCoilAirInletNode = SuppHeatCoilInletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the Supplemental Heating Coil Outlet Node
					errFlag = false;
					SuppHeatCoilOutletNode = GetWaterCoilOutletNode( "Coil:Heating:Water", MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					MSHeatPump( MSHPNum ).SuppCoilAirOutletNode = SuppHeatCoilOutletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}
					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Water", MSHeatPump( MSHPNum ).SuppHeatCoilName, NodeID( SuppHeatCoilInletNode ), NodeID( SuppHeatCoilOutletNode ) );
				}
			}
			if ( SameString( Alphas( 14 ), "Coil:Heating:Steam" ) ) {
				MSHeatPump( MSHPNum ).SuppHeatCoilType = Coil_HeatingSteam;
				ValidateComponent( Alphas( 14 ), MSHeatPump( MSHPNum ).SuppHeatCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
					ErrorsFound = true;
				} else { // mine data from heating coil object

					errFlag = false;
					MSHeatPump( MSHPNum ).SuppHeatCoilNum = GetSteamCoilIndex( Alphas( 14 ), MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					if ( MSHeatPump( MSHPNum ).SuppHeatCoilNum == 0 ) {
						ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 14 ) + " = " + MSHeatPump( MSHPNum ).SuppHeatCoilName );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the Supplemental Heating Coil steam inlet node number
					errFlag = false;
					MSHeatPump( MSHPNum ).SuppCoilControlNode = GetCoilAirOutletNode( "Coil:Heating:Steam", MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the Supplemental Heating Coil steam max volume flow rate
					MSHeatPump( MSHPNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( MSHeatPump( MSHPNum ).SuppHeatCoilNum, errFlag );
					if ( MSHeatPump( MSHPNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNameNoColon );
						MSHeatPump( MSHPNum ).MaxSuppCoilFluidFlow *= SteamDensity;
					}

					// Get the Supplemental Heating Coil Inlet Node
					errFlag = false;
					SuppHeatCoilInletNode = GetSteamCoilAirInletNode( MSHeatPump( MSHPNum ).SuppHeatCoilNum, MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					MSHeatPump( MSHPNum ).SuppCoilAirInletNode = SuppHeatCoilInletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					// Get the Supplemental Heating Coil Outlet Node
					errFlag = false;
					SuppHeatCoilOutletNode = GetCoilAirOutletNode( MSHeatPump( MSHPNum ).SuppHeatCoilNum, MSHeatPump( MSHPNum ).SuppHeatCoilName, errFlag );
					MSHeatPump( MSHPNum ).SuppCoilAirOutletNode = SuppHeatCoilOutletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHPNum ).Name );
						ErrorsFound = true;
					}

					SetUpCompSets( CurrentModuleObject, MSHeatPump( MSHPNum ).Name, "Coil:Heating:Steam", MSHeatPump( MSHPNum ).SuppHeatCoilName, NodeID( SuppHeatCoilInletNode ), NodeID( SuppHeatCoilOutletNode ) );

				}
			}

			if ( MSHeatPump( MSHPNum ).SuppHeatCoilType == 0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cAlphaFields( 14 ) + " is not allowed = " + Alphas( 14 ) );
				ShowContinueError( "Valid choices are Coil:Heating:Gas,Coil:Heating:Electric,Coil:Heating:Steam,or Coil:Heating:Water" );
				ErrorsFound = true;
			}

			MSHeatPump( MSHPNum ).SuppMaxAirTemp = Numbers( 2 );
			MSHeatPump( MSHPNum ).SuppMaxOATemp = Numbers( 3 );
			if ( MSHeatPump( MSHPNum ).SuppMaxOATemp > 21.0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cNumericFields( 3 ) + " is greater than 21.0" );
				ShowContinueError( "The input value is " + RoundSigDigits( Numbers( 3 ), 2 ) );
				ErrorsFound = true;
			}

			MSHeatPump( MSHPNum ).AuxOnCyclePower = Numbers( 4 );
			MSHeatPump( MSHPNum ).AuxOffCyclePower = Numbers( 5 );
			if ( MSHeatPump( MSHPNum ).AuxOnCyclePower < 0.0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", A negative value for " + cNumericFields( 4 ) + " is not allowed " );
				ErrorsFound = true;
			}
			if ( MSHeatPump( MSHPNum ).AuxOffCyclePower < 0.0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", A negative value for " + cNumericFields( 5 ) + " is not allowed " );
				ErrorsFound = true;
			}

			// Heat recovery
			MSHeatPump( MSHPNum ).DesignHeatRecFlowRate = Numbers( 6 );
			if ( MSHeatPump( MSHPNum ).DesignHeatRecFlowRate > 0.0 ) {
				MSHeatPump( MSHPNum ).HeatRecActive = true;
				MSHeatPump( MSHPNum ).DesignHeatRecMassFlowRate = RhoH2O( InitConvTemp ) * MSHeatPump( MSHPNum ).DesignHeatRecFlowRate;
				MSHeatPump( MSHPNum ).HeatRecInletNodeNum = GetOnlySingleNode( Alphas( 16 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
				if ( MSHeatPump( MSHPNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", Missing " + cAlphaFields( 16 ) + '.' );
					ErrorsFound = true;
				}
				MSHeatPump( MSHPNum ).HeatRecOutletNodeNum = GetOnlySingleNode( Alphas( 17 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
				if ( MSHeatPump( MSHPNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", Missing " + cAlphaFields( 17 ) + '.' );
					ErrorsFound = true;
				}
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 16 ), Alphas( 17 ), "MSHP Heat receovery Nodes" );
				SetMSHPDXCoilHeatRecoveryFlag( MSHeatPump( MSHPNum ).DXCoolCoilIndex );
				if ( MSHeatPump( MSHPNum ).DXHeatCoilIndex > 0 ) {
					SetMSHPDXCoilHeatRecoveryFlag( MSHeatPump( MSHPNum ).DXHeatCoilIndex );
				}
			} else {
				MSHeatPump( MSHPNum ).HeatRecActive = false;
				MSHeatPump( MSHPNum ).DesignHeatRecMassFlowRate = 0.0;
				MSHeatPump( MSHPNum ).HeatRecInletNodeNum = 0;
				MSHeatPump( MSHPNum ).HeatRecOutletNodeNum = 0;
				if ( ! lAlphaBlanks( 16 ) || ! lAlphaBlanks( 17 ) ) {
					ShowWarningError( "Since " + cNumericFields( 6 ) + " = 0.0, heat recovery is inactive for " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "However, " + cAlphaFields( 16 ) + " or " + cAlphaFields( 17 ) + " was specified." );
				}
			}
			MSHeatPump( MSHPNum ).MaxHeatRecOutletTemp = Numbers( 7 );
			if ( MSHeatPump( MSHPNum ).MaxHeatRecOutletTemp < 0.0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", The value for " + cNumericFields( 7 ) + " is below 0.0" );
				ErrorsFound = true;
			}
			if ( MSHeatPump( MSHPNum ).MaxHeatRecOutletTemp > 100.0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", The value for " + cNumericFields( 7 ) + " is above 100.0" );
				ErrorsFound = true;
			}

			MSHeatPump( MSHPNum ).IdleVolumeAirRate = Numbers( 8 );
			if ( MSHeatPump( MSHPNum ).IdleVolumeAirRate < 0.0 && MSHeatPump( MSHPNum ).IdleVolumeAirRate != AutoSize ) {
				ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cNumericFields( 8 ) + " cannot be less than zero." );
				ErrorsFound = true;
			}

			//     AirFlowControl only valid if fan opmode = ContFanCycCoil
			if ( MSHeatPump( MSHPNum ).IdleVolumeAirRate == 0.0 ) {
				MSHeatPump( MSHPNum ).AirFlowControl = UseCompressorOnFlow;
			} else {
				MSHeatPump( MSHPNum ).AirFlowControl = UseCompressorOffFlow;
			}

			//   Initialize last mode of compressor operation
			MSHeatPump( MSHPNum ).LastMode = HeatingMode;

			MSHeatPump( MSHPNum ).NumOfSpeedHeating = Numbers( 9 );
			if ( MSHeatPump( MSHPNum ).NumOfSpeedHeating < 2 || MSHeatPump( MSHPNum ).NumOfSpeedHeating > 4 ) {
				if ( MSHeatPump( MSHPNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
					ShowSevereError( CurrentModuleObject + ", The maximum " + cNumericFields( 9 ) + " is 4, and the minimum number is 2" );
					ShowContinueError( "The input value is " + RoundSigDigits( Numbers( 9 ), 0 ) );
					ErrorsFound = true;
				}
			}
			MSHeatPump( MSHPNum ).NumOfSpeedCooling = Numbers( 10 );
			if ( MSHeatPump( MSHPNum ).NumOfSpeedCooling < 2 || MSHeatPump( MSHPNum ).NumOfSpeedCooling > 4 ) {
				ShowSevereError( CurrentModuleObject + ", The maximum " + cNumericFields( 10 ) + " is 4, and the minimum number is 2" );
				ShowContinueError( "The input value is " + RoundSigDigits( Numbers( 10 ), 0 ) );
				ErrorsFound = true;
			}

			// Generate a dynamic array for heating
			if ( MSHeatPump( MSHPNum ).NumOfSpeedHeating > 0 ) {
				MSHeatPump( MSHPNum ).HeatMassFlowRate.allocate( MSHeatPump( MSHPNum ).NumOfSpeedHeating );
				MSHeatPump( MSHPNum ).HeatVolumeFlowRate.allocate( MSHeatPump( MSHPNum ).NumOfSpeedHeating );
				MSHeatPump( MSHPNum ).HeatingSpeedRatio.allocate( MSHeatPump( MSHPNum ).NumOfSpeedHeating );
				MSHeatPump( MSHPNum ).HeatingSpeedRatio = 1.0;
				for ( i = 1; i <= MSHeatPump( MSHPNum ).NumOfSpeedHeating; ++i ) {
					MSHeatPump( MSHPNum ).HeatVolumeFlowRate( i ) = Numbers( 10 + i );
					if ( MSHeatPump( MSHPNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
						if ( MSHeatPump( MSHPNum ).HeatVolumeFlowRate( i ) <= 0.0 && MSHeatPump( MSHPNum ).HeatVolumeFlowRate( i ) != AutoSize ) {
							ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cNumericFields( 10 + i ) + " must be greater than zero." );
							ErrorsFound = true;
						}
					}
				}
				// Ensure flow rate at high speed should be greater or equal to the flow rate at low speed
				for ( i = 2; i <= MSHeatPump( MSHPNum ).NumOfSpeedHeating; ++i ) {
					if ( MSHeatPump( MSHPNum ).HeatVolumeFlowRate( i ) == AutoSize ) continue;
					Found = false;
					for ( j = i - 1; j >= 1; --j ) {
						if ( MSHeatPump( MSHPNum ).HeatVolumeFlowRate( i ) != AutoSize ) {
							Found = true;
							break;
						}
					}
					if ( Found ) {
						if ( MSHeatPump( MSHPNum ).HeatVolumeFlowRate( i ) < MSHeatPump( MSHPNum ).HeatVolumeFlowRate( j ) ) {
							ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cNumericFields( 10 + i ) );
							ShowContinueError( " cannot be less than " + cNumericFields( 10 + j ) );
							ErrorsFound = true;
						}
					}
				}
			}

			// Generate a dynamic array for cooling
			if ( MSHeatPump( MSHPNum ).NumOfSpeedCooling > 0 ) {
				MSHeatPump( MSHPNum ).CoolMassFlowRate.allocate( MSHeatPump( MSHPNum ).NumOfSpeedCooling );
				MSHeatPump( MSHPNum ).CoolVolumeFlowRate.allocate( MSHeatPump( MSHPNum ).NumOfSpeedCooling );
				MSHeatPump( MSHPNum ).CoolingSpeedRatio.allocate( MSHeatPump( MSHPNum ).NumOfSpeedCooling );
				MSHeatPump( MSHPNum ).CoolingSpeedRatio = 1.0;
				for ( i = 1; i <= MSHeatPump( MSHPNum ).NumOfSpeedCooling; ++i ) {
					MSHeatPump( MSHPNum ).CoolVolumeFlowRate( i ) = Numbers( 14 + i );
					if ( MSHeatPump( MSHPNum ).CoolVolumeFlowRate( i ) <= 0.0 && MSHeatPump( MSHPNum ).CoolVolumeFlowRate( i ) != AutoSize ) {
						ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cNumericFields( 14 + i ) + " must be greater than zero." );
						ErrorsFound = true;
					}
				}
				// Ensure flow rate at high speed should be greater or equal to the flow rate at low speed
				for ( i = 2; i <= MSHeatPump( MSHPNum ).NumOfSpeedCooling; ++i ) {
					if ( MSHeatPump( MSHPNum ).CoolVolumeFlowRate( i ) == AutoSize ) continue;
					Found = false;
					for ( j = i - 1; j >= 1; --j ) {
						if ( MSHeatPump( MSHPNum ).CoolVolumeFlowRate( i ) != AutoSize ) {
							Found = true;
							break;
						}
					}
					if ( Found ) {
						if ( MSHeatPump( MSHPNum ).CoolVolumeFlowRate( i ) < MSHeatPump( MSHPNum ).CoolVolumeFlowRate( j ) ) {
							ShowSevereError( CurrentModuleObject + ", \"" + MSHeatPump( MSHPNum ).Name + "\", " + cNumericFields( 14 + i ) );
							ShowContinueError( " cannot be less than " + cNumericFields( 14 + j ) );
							ErrorsFound = true;
						}
					}
				}
			}

			// Check node integrity
			if ( MSHeatPump( MSHPNum ).FanPlaceType == BlowThru ) {
				if ( MSHeatPump( MSHPNum ).FanInletNode != MSHeatPump( MSHPNum ).AirInletNodeNum ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "When a blow through fan is specified, the fan inlet node name must be the same as the " + cAlphaFields( 3 ) );
					ShowContinueError( "...Fan inlet node name           = " + NodeID( MSHeatPump( MSHPNum ).FanInletNode ) );
					ShowContinueError( "..." + cAlphaFields( 3 ) + " = " + NodeID( MSHeatPump( MSHPNum ).AirInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( MSHeatPump( MSHPNum ).FanOutletNode != CoolingCoilInletNode ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil inlet node name." );
					ShowContinueError( "...Fan outlet node name         = " + NodeID( MSHeatPump( MSHPNum ).FanOutletNode ) );
					ShowContinueError( "...Cooling coil inlet node name = " + NodeID( CoolingCoilInletNode ) );
					ErrorsFound = true;
				}
				if ( CoolingCoilOutletNode != HeatingCoilInletNode ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "The cooling coil outlet node name must be the same as the heating coil inlet node name." );
					ShowContinueError( "...Cooling coil outlet node name = " + NodeID( CoolingCoilOutletNode ) );
					ShowContinueError( "...Heating coil inlet node name  = " + NodeID( HeatingCoilInletNode ) );
					ErrorsFound = true;
				}
				if ( HeatingCoilOutletNode != SuppHeatCoilInletNode ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "When a blow through fan is specified, the heating coil outlet node name must be the same as the reheat coil inlet node name." );
					ShowContinueError( "...Heating coil outlet node name = " + NodeID( HeatingCoilOutletNode ) );
					ShowContinueError( "...Reheat coil inlet node name   = " + NodeID( SuppHeatCoilInletNode ) );
					ErrorsFound = true;
				}
				if ( SuppHeatCoilOutletNode != MSHeatPump( MSHPNum ).AirOutletNodeNum ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "The supplemental heating coil outlet node name must be the same as the " + cAlphaFields( 4 ) );
					ShowContinueError( "...Supplemental heating coil outlet node name   = " + NodeID( SuppHeatCoilOutletNode ) );
					ShowContinueError( "..." + cAlphaFields( 4 ) + " = " + NodeID( MSHeatPump( MSHPNum ).AirOutletNodeNum ) );
					ErrorsFound = true;
				}
			} else {
				if ( CoolingCoilInletNode != MSHeatPump( MSHPNum ).AirInletNodeNum ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "When a draw through fan is specified, the cooling coil inlet node name must be the same as the " + cAlphaFields( 3 ) );
					ShowContinueError( "...Cooling coil inlet node name  = " + NodeID( CoolingCoilInletNode ) );
					ShowContinueError( "..." + cAlphaFields( 3 ) + " = " + NodeID( MSHeatPump( MSHPNum ).AirInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( CoolingCoilOutletNode != HeatingCoilInletNode ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "The cooling coil outlet node name must be the same as the heating coil inlet node name." );
					ShowContinueError( "...Cooling coil outlet node name = " + NodeID( CoolingCoilOutletNode ) );
					ShowContinueError( "...Heating coil inlet node name  = " + NodeID( HeatingCoilInletNode ) );
					ErrorsFound = true;
				}
				if ( HeatingCoilOutletNode != MSHeatPump( MSHPNum ).FanInletNode ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "When a draw through fan is specified, the heating coil outlet node name must be the same as the fan inlet node name." );
					ShowContinueError( "...Heating coil outlet node name = " + NodeID( HeatingCoilOutletNode ) );
					ShowContinueError( "...Fan inlet node name           = " + NodeID( MSHeatPump( MSHPNum ).FanInletNode ) );
					ErrorsFound = true;
				}
				if ( MSHeatPump( MSHPNum ).FanOutletNode != SuppHeatCoilInletNode ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil inlet node name." );
					ShowContinueError( "...Fan outlet node name        = " + NodeID( MSHeatPump( MSHPNum ).FanOutletNode ) );
					ShowContinueError( "...Reheat coil inlet node name = " + NodeID( SuppHeatCoilInletNode ) );
					ErrorsFound = true;
				}
				if ( SuppHeatCoilOutletNode != MSHeatPump( MSHPNum ).AirOutletNodeNum ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "The reheat coil outlet node name must be the same as the " + cAlphaFields( 4 ) );
					ShowContinueError( "...Reheat coil outlet node name   = " + NodeID( SuppHeatCoilOutletNode ) );
					ShowContinueError( "..." + cAlphaFields( 4 ) + " = " + NodeID( MSHeatPump( MSHPNum ).AirOutletNodeNum ) );
					ErrorsFound = true;
				}
			}

			// Ensure the numbers of speeds defined in the parent object are equal to the numbers defined in coil objects
			if ( MSHeatPump( MSHPNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
				i = GetDXCoilNumberOfSpeeds( Alphas( 10 ), Alphas( 11 ), ErrorsFound );
				if ( MSHeatPump( MSHPNum ).NumOfSpeedHeating != i ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "The " + cNumericFields( 9 ) + " is not equal to the number defined in " + cAlphaFields( 11 ) + " = " + Alphas( 11 ) );
					ErrorsFound = true;
				}
			} else if ( MSHeatPump( MSHPNum ).HeatCoilType == Coil_HeatingElectric_MultiStage || MSHeatPump( MSHPNum ).HeatCoilType == Coil_HeatingGas_MultiStage ) {
				i = GetHeatingCoilNumberOfStages( Alphas( 10 ), Alphas( 11 ), ErrorsFound );
				if ( MSHeatPump( MSHPNum ).NumOfSpeedHeating != i ) {
					ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
					ShowContinueError( "The " + cNumericFields( 9 ) + " is not equal to the number defined in " + cAlphaFields( 11 ) + " = " + Alphas( 11 ) );
					ErrorsFound = true;
				}
			}
			i = GetDXCoilNumberOfSpeeds( Alphas( 12 ), Alphas( 13 ), ErrorsFound );
			if ( MSHeatPump( MSHPNum ).NumOfSpeedCooling != i ) {
				ShowSevereError( "For " + CurrentModuleObject + " \"" + MSHeatPump( MSHPNum ).Name + "\"" );
				ShowContinueError( "The " + cNumericFields( 10 ) + " is not equal to the number defined in " + cAlphaFields( 13 ) + " = " + Alphas( 13 ) );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination." );
		}
		// End of multispeed heat pump

		for ( MSHPNum = 1; MSHPNum <= NumMSHeatPumps; ++MSHPNum ) {
			// Setup Report Variables for MSHP Equipment
			SetupOutputVariable( "Unitary System Ancillary Electric Power [W]", MSHeatPump( MSHPNum ).AuxElecPower, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Cooling Ancillary Electric Energy [J]", MSHeatPumpReport( MSHPNum ).AuxElecCoolConsumption, "System", "Sum", MSHeatPump( MSHPNum ).Name, _, "Electric", "Cooling", _, "System" );
			SetupOutputVariable( "Unitary System Heating Ancillary Electric Energy [J]", MSHeatPumpReport( MSHPNum ).AuxElecHeatConsumption, "System", "Sum", MSHeatPump( MSHPNum ).Name, _, "Electric", "Heating", _, "System" );
			SetupOutputVariable( "Unitary System Fan Part Load Ratio []", MSHeatPump( MSHPNum ).FanPartLoadRatio, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Compressor Part Load Ratio []", MSHeatPump( MSHPNum ).CompPartLoadRatio, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Electric Power [W]", MSHeatPump( MSHPNum ).ElecPower, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Electric Energy [J]", MSHeatPumpReport( MSHPNum ).ElecPowerConsumption, "System", "Sum", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System DX Coil Cycling Ratio []", MSHeatPumpReport( MSHPNum ).CycRatio, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System DX Coil Speed Ratio []", MSHeatPumpReport( MSHPNum ).SpeedRatio, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System DX Coil Speed Level []", MSHeatPumpReport( MSHPNum ).SpeedNum, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Total Cooling Rate [W]", MSHeatPump( MSHPNum ).TotCoolEnergyRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Total Heating Rate [W]", MSHeatPump( MSHPNum ).TotHeatEnergyRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Cooling Rate [W]", MSHeatPump( MSHPNum ).SensCoolEnergyRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Heating Rate [W]", MSHeatPump( MSHPNum ).SensHeatEnergyRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Latent Cooling Rate [W]", MSHeatPump( MSHPNum ).LatCoolEnergyRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
			SetupOutputVariable( "Unitary System Latent Heating Rate [W]", MSHeatPump( MSHPNum ).LatHeatEnergyRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
			if ( MSHeatPump( MSHPNum ).HeatRecActive ) {
				SetupOutputVariable( "Unitary System Heat Recovery Rate [W]", MSHeatPump( MSHPNum ).HeatRecoveryRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
				SetupOutputVariable( "Unitary System Heat Recovery Inlet Temperature [C]", MSHeatPump( MSHPNum ).HeatRecoveryInletTemp, "System", "Average", MSHeatPump( MSHPNum ).Name );
				SetupOutputVariable( "Unitary System Heat Recovery Outlet Temperature [C]", MSHeatPump( MSHPNum ).HeatRecoveryOutletTemp, "System", "Average", MSHeatPump( MSHPNum ).Name );
				SetupOutputVariable( "Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]", MSHeatPump( MSHPNum ).HeatRecoveryMassFlowRate, "System", "Average", MSHeatPump( MSHPNum ).Name );
				SetupOutputVariable( "Unitary System Heat Recovery Energy [J]", MSHeatPumpReport( MSHPNum ).HeatRecoveryEnergy, "System", "Sum", MSHeatPump( MSHPNum ).Name );
			}
		}

	}

	//******************************************************************************

	void
	InitMSHeatPump(
		int const MSHeatPumpNum, // Engine driven heat pump number
		bool const FirstHVACIteration, // TRUE if first HVAC iteration
		int const AirLoopNum, // air loop index
		Real64 & QZnReq, // Heating/Cooling load for all served zones
		Real64 & OnOffAirFlowRatio // Ratio of compressor ON airflow to average airflow over timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    July 2007
		//       MODIFIED         Bereket Nigusse, June 2010 - added a procedure to calculate supply air flow fraction
		//                        through controlled zone
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the multispeed heat pump (MSHP) components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations. The MSHP system is simulated with no load (coils off) to
		// determine the outlet temperature. A setpoint temperature is calculated on FirstHVACIteration = TRUE.
		// Once the setpoint is calculated, the inlet mass flow rate on FirstHVACIteration = FALSE is used to
		// determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
		// temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

		// METHODOLOGY EMPLOYED:

		// REFERENCES: na

		// Using/Aliasing
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using DataSizing::AutoSize;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using InputProcessor::SameString;
		using DataAirLoop::AirLoopControlInfo;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::AirDistUnit_Num;
		using DataZoneEquipment::DirectAir_Num;
		using DataAirLoop::AirToZoneNodeInfo;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_MultiSpeedHeatPumpRecovery;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using DataGlobals::AnyPlantInModel;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;
		using SteamCoils::SimulateSteamCoilComponents;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		auto & GetSteamCoilCapacity( SteamCoils::GetCoilCapacity );
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::SimulateWaterCoilComponents;
		using DataZoneControls::StageZoneLogic;
		using DXCoils::GetDXCoilAvailSchPtr;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "InitMSHeatPump" );
		int InNode; // Inlet node number in MSHP loop
		int OutNode; // Outlet node number in MSHP loop
		int ZoneInNode; // Zone inlet node number in the controlled zone for MSHP
		Real64 RhoAir; // Air density at InNode
		static bool MyOneTimeFlag( true ); // Initialization flag
		static Array1D_bool MyEnvrnFlag; // Used for initializations each begin environment flag
		static Array1D_bool MySizeFlag; // Used for sizing MSHP inputs one time
		static Array1D_bool MyCheckFlag; // Used to obtain the zone inlet node number in the controlled zone
		static Array1D_bool MyFlowFracFlag; // Used for calculatig flow fraction once
		static Array1D_bool MyPlantScantFlag; // used for finding on heat recovery plant loop
		static Array1D_bool MyStagedFlag; // used for finding on staged thermostat

		Real64 QSensUnitOut; // Output of MSHP system with coils off
		Real64 PartLoadFrac; // Part-load ratio
		int ZoneNum;
		int i; // Index to speed
		int NumOfSpeedCooling; // Number of speeds for cooling
		int NumOfSpeedHeating; // Number of speeds for heating
		int j;
		int k;
		Real64 MinHumRat; // Minimum humidity ratio for sensible capacity calculation (kg/kg)
		Real64 DeltaMassRate; // Difference of mass flow rate between inlet node and system outlet node

		static int ZoneInSysIndex( 0 ); // number of zone inlet nodes counter in an airloop
		static int NumAirLoopZones( 0 ); // number of zone inlet nodes in an air loop
		static int ZoneInletNodeNum( 0 ); // zone inlet nodes node number
		static bool FlowFracFlagReady( true ); // one time flag for calculating flow fraction through controlled zone
		static Real64 SumOfMassFlowRateMax( 0.0 ); // the sum of mass flow rates at inlet to zones in an airloop
		static Real64 CntrlZoneTerminalUnitMassFlowRateMax( 0.0 ); // Maximum mass flow rate through controlled zone terminal unit
		bool errFlag;
		Real64 rho; // local fluid density
		Real64 MdotHR; // local temporary for heat recovery fluid mass flow rate (kg/s)
		Real64 ZoneLoadToCoolSPSequenced;
		Real64 ZoneLoadToHeatSPSequenced;
		static int EquipNum( 0 ); // local do loop index for equipment listed for a zone

		static bool ErrorsFound( false ); // flag returned from mining call
		static int SteamIndex( 0 ); // index of steam quality for steam heating coil
		static Real64 mdot( 0.0 ); // local temporary for mass flow rate (kg/s)
		static Real64 SteamDensity( 0.0 ); // density of steam at 100C, used for steam heating coils
		static Real64 CoilMaxVolFlowRate( 0.0 ); // coil fluid maximum volume flow rate
		static Real64 QActual( 0.0 ); // coil actual capacity
		static int CoilAvailSchPtr( 0 ); // DX coil availability schedule pointer

		// FLOW
		InNode = MSHeatPump( MSHeatPumpNum ).AirInletNodeNum;
		OutNode = MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum;
		NumOfSpeedCooling = MSHeatPump( MSHeatPumpNum ).NumOfSpeedCooling;
		NumOfSpeedHeating = MSHeatPump( MSHeatPumpNum ).NumOfSpeedHeating;

		++AirLoopPass;
		if ( AirLoopPass > 2 ) AirLoopPass = 1;
		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumMSHeatPumps );
			MySizeFlag.allocate( NumMSHeatPumps );
			MyCheckFlag.allocate( NumMSHeatPumps );
			MyFlowFracFlag.allocate( NumMSHeatPumps );
			MyPlantScantFlag.allocate( NumMSHeatPumps );
			MyStagedFlag.allocate( NumMSHeatPumps );

			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyOneTimeFlag = false;
			MyCheckFlag = true;
			MyFlowFracFlag = true;
			MyPlantScantFlag = true;
			MyStagedFlag = true;
		}

		if ( MyPlantScantFlag( MSHeatPumpNum ) && allocated( PlantLoop ) ) {
			if ( MSHeatPump( MSHeatPumpNum ).HeatRecActive ) {
				errFlag = false;
				ScanPlantLoopsForObject( MSHeatPump( MSHeatPumpNum ).Name, TypeOf_MultiSpeedHeatPumpRecovery, MSHeatPump( MSHeatPumpNum ).HRLoopNum, MSHeatPump( MSHeatPumpNum ).HRLoopSideNum, MSHeatPump( MSHeatPumpNum ).HRBranchNum, MSHeatPump( MSHeatPumpNum ).HRCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitMSHeatPump: Program terminated for previous conditions." );
				}

				MyPlantScantFlag( MSHeatPumpNum ) = false;
			} else {
				MyPlantScantFlag( MSHeatPumpNum ) = false;
			}
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingWater ) {
				errFlag = false;
				ScanPlantLoopsForObject( MSHeatPump( MSHeatPumpNum ).HeatCoilName, TypeOf_CoilWaterSimpleHeating, MSHeatPump( MSHeatPumpNum ).LoopNum, MSHeatPump( MSHeatPumpNum ).LoopSide, MSHeatPump( MSHeatPumpNum ).BranchNum, MSHeatPump( MSHeatPumpNum ).CompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitMSHeatPump: Program terminated for previous conditions." );
				}
				MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHeatPumpNum ).HeatCoilName, ErrorsFound );

				if ( MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( MSHeatPump( MSHeatPumpNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( MSHeatPump( MSHeatPumpNum ).LoopNum ).FluidIndex, RoutineName );
					MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHeatPumpNum ).HeatCoilName, ErrorsFound ) * rho;
				}
				// fill outlet node for coil
				MSHeatPump( MSHeatPumpNum ).CoilOutletNode = PlantLoop( MSHeatPump( MSHeatPumpNum ).LoopNum ).LoopSide( MSHeatPump( MSHeatPumpNum ).LoopSide ).Branch( MSHeatPump( MSHeatPumpNum ).BranchNum ).Comp( MSHeatPump( MSHeatPumpNum ).CompNum ).NodeNumOut;
				MyPlantScantFlag( MSHeatPumpNum ) = false;

			} else if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingSteam ) {
				errFlag = false;
				ScanPlantLoopsForObject( MSHeatPump( MSHeatPumpNum ).HeatCoilName, TypeOf_CoilSteamAirHeating, MSHeatPump( MSHeatPumpNum ).LoopNum, MSHeatPump( MSHeatPumpNum ).LoopSide, MSHeatPump( MSHeatPumpNum ).BranchNum, MSHeatPump( MSHeatPumpNum ).CompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitMSHeatPump: Program terminated for previous conditions." );
				}
				MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate( MSHeatPump( MSHeatPumpNum ).HeatCoilNum, ErrorsFound );
				if ( MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow > 0.0 ) {
					SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
					MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow *= SteamDensity;
				}

				// fill outlet node for coil
				MSHeatPump( MSHeatPumpNum ).CoilOutletNode = PlantLoop( MSHeatPump( MSHeatPumpNum ).LoopNum ).LoopSide( MSHeatPump( MSHeatPumpNum ).LoopSide ).Branch( MSHeatPump( MSHeatPumpNum ).BranchNum ).Comp( MSHeatPump( MSHeatPumpNum ).CompNum ).NodeNumOut;
				MyPlantScantFlag( MSHeatPumpNum ) = false;

			}
			if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == Coil_HeatingWater ) {
				errFlag = false;
				ScanPlantLoopsForObject( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, TypeOf_CoilWaterSimpleHeating, MSHeatPump( MSHeatPumpNum ).SuppLoopNum, MSHeatPump( MSHeatPumpNum ).SuppLoopSide, MSHeatPump( MSHeatPumpNum ).SuppBranchNum, MSHeatPump( MSHeatPumpNum ).SuppCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitMSHeatPump: Program terminated for previous conditions." );
				}
				MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, ErrorsFound );

				if ( MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( MSHeatPump( MSHeatPumpNum ).SuppLoopNum ).FluidName, InitConvTemp, PlantLoop( MSHeatPump( MSHeatPumpNum ).SuppLoopNum ).FluidIndex, RoutineName );
					MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, ErrorsFound ) * rho;
				}
				// fill outlet node for coil
				MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode = PlantLoop( MSHeatPump( MSHeatPumpNum ).SuppLoopNum ).LoopSide( MSHeatPump( MSHeatPumpNum ).SuppLoopSide ).Branch( MSHeatPump( MSHeatPumpNum ).SuppBranchNum ).Comp( MSHeatPump( MSHeatPumpNum ).SuppCompNum ).NodeNumOut;
				MyPlantScantFlag( MSHeatPumpNum ) = false;

			} else if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == Coil_HeatingSteam ) {
				errFlag = false;
				ScanPlantLoopsForObject( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, TypeOf_CoilSteamAirHeating, MSHeatPump( MSHeatPumpNum ).SuppLoopNum, MSHeatPump( MSHeatPumpNum ).SuppLoopSide, MSHeatPump( MSHeatPumpNum ).SuppBranchNum, MSHeatPump( MSHeatPumpNum ).SuppCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitMSHeatPump: Program terminated for previous conditions." );
				}
				MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum, ErrorsFound );
				if ( MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow > 0.0 ) {
					SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
					MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow *= SteamDensity;
				}

				// fill outlet node for coil
				MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode = PlantLoop( MSHeatPump( MSHeatPumpNum ).SuppLoopNum ).LoopSide( MSHeatPump( MSHeatPumpNum ).SuppLoopSide ).Branch( MSHeatPump( MSHeatPumpNum ).SuppBranchNum ).Comp( MSHeatPump( MSHeatPumpNum ).SuppCompNum ).NodeNumOut;
				MyPlantScantFlag( MSHeatPumpNum ) = false;
			}
		} else if ( MyPlantScantFlag( MSHeatPumpNum ) && ! AnyPlantInModel ) {
			MyPlantScantFlag( MSHeatPumpNum ) = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( MSHeatPumpNum ) ) {
			GetFanVolFlow( MSHeatPump( MSHeatPumpNum ).FanNum, MSHeatPump( MSHeatPumpNum ).FanVolFlow );
			SizeMSHeatPump( MSHeatPumpNum );
			MSHeatPump( MSHeatPumpNum ).FlowFraction = 1.0;
			MySizeFlag( MSHeatPumpNum ) = false;
			// Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
			AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr = MSHeatPump( MSHeatPumpNum ).FanSchedPtr;
			AirLoopControlInfo( AirLoopNum ).UnitarySys = true;
			AirLoopControlInfo( AirLoopNum ).UnitarySysSimulating = false; // affects child coil sizing by allowing coil to size itself instead of parent telling coil what size to use
			AirLoopControlInfo( AirLoopNum ).FanOpMode = MSHeatPump( MSHeatPumpNum ).OpMode;
		}

		if ( allocated( ZoneEquipConfig ) && MyCheckFlag( MSHeatPumpNum ) ) {
			for ( i = 1; i <= NumOfZones; ++i ) {
				if ( AirLoopNum != ZoneEquipConfig( i ).AirLoopNum ) continue;
				if ( MSHeatPump( MSHeatPumpNum ).ControlZoneNum == ZoneEquipConfig( i ).ActualZoneNum ) {
					for ( j = 1; j <= ZoneEquipConfig( i ).NumInletNodes; ++j ) {
						if ( MSHeatPump( MSHeatPumpNum ).ZoneInletNode == 0 ) {
							for ( k = 1; k <= ZoneEquipConfig( i ).NumInletNodes; ++k ) {
								if ( ZoneEquipConfig( i ).InletNode( j ) == ZoneEquipConfig( i ).AirDistUnitCool( k ).OutNode ) {
									MSHeatPump( MSHeatPumpNum ).ZoneInletNode = ZoneEquipConfig( i ).InletNode( j );
									break;
								}
							}
						}
						if ( MSHeatPump( MSHeatPumpNum ).ZoneInletNode == 0 ) {
							for ( k = 1; k <= ZoneEquipConfig( i ).NumInletNodes; ++k ) {
								if ( ZoneEquipConfig( i ).InletNode( j ) == ZoneEquipConfig( i ).AirDistUnitHeat( k ).OutNode ) {
									MSHeatPump( MSHeatPumpNum ).ZoneInletNode = ZoneEquipConfig( i ).InletNode( j );
									break;
								}
							}
						}
					}
					//setup furnace zone equipment sequence information based on finding an air terminal
					if ( ZoneEquipConfig( i ).EquipListIndex > 0 ) {
						for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
							if ( ( ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).EquipType_Num( EquipNum ) == AirDistUnit_Num ) || ( ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).EquipType_Num( EquipNum ) == DirectAir_Num ) ) {
								MSHeatPump( MSHeatPumpNum ).ZoneSequenceCoolingNum = ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).CoolingPriority( EquipNum );
								MSHeatPump( MSHeatPumpNum ).ZoneSequenceHeatingNum = ZoneEquipList( ZoneEquipConfig( i ).EquipListIndex ).HeatingPriority( EquipNum );
							}
						}
					}
				}
			}
			MyCheckFlag( MSHeatPumpNum ) = false;
			if ( MSHeatPump( MSHeatPumpNum ).ZoneInletNode == 0 ) {
				ShowSevereError( "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, \"" + MSHeatPump( MSHeatPumpNum ).Name + "\", The zone inlet node in the controlled zone (" + MSHeatPump( MSHeatPumpNum ).ControlZoneName + ") is not found." );
				ShowFatalError( "Subroutine InitMSHeatPump: Errors found in getting AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed input.  Preceding condition(s) causes termination." );
			}
		}

		// Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
		NumAirLoopZones = AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled + AirToZoneNodeInfo( AirLoopNum ).NumZonesHeated;
		if ( allocated( AirToZoneNodeInfo ) && MyFlowFracFlag( MSHeatPumpNum ) ) {
			FlowFracFlagReady = true;
			for ( ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex ) {
				// zone inlet nodes for cooling
				if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled > 0 ) {
					if ( AirToZoneNodeInfo( AirLoopNum ).TermUnitCoolInletNodes( ZoneInSysIndex ) == -999 ) {
						// the data structure for the zones inlet nodes has not been filled
						FlowFracFlagReady = false;
					}
				}
				// zone inlet nodes for heating
				if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesHeated > 0 ) {
					if ( AirToZoneNodeInfo( AirLoopNum ).TermUnitHeatInletNodes( ZoneInSysIndex ) == -999 ) {
						// the data structure for the zones inlet nodes has not been filled
						FlowFracFlagReady = false;
					}
				}
			}
		}
		if ( allocated( AirToZoneNodeInfo ) && FlowFracFlagReady ) {
			SumOfMassFlowRateMax = 0.0; // initialize the sum of the maximum flows
			for ( ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex ) {
				ZoneInletNodeNum = AirToZoneNodeInfo( AirLoopNum ).TermUnitCoolInletNodes( ZoneInSysIndex );
				SumOfMassFlowRateMax += Node( ZoneInletNodeNum ).MassFlowRateMax;
				if ( AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZoneInSysIndex ) == MSHeatPump( MSHeatPumpNum ).ControlZoneNum ) {
					CntrlZoneTerminalUnitMassFlowRateMax = Node( ZoneInletNodeNum ).MassFlowRateMax;
				}
			}
			if ( SumOfMassFlowRateMax != 0.0 && MyFlowFracFlag( MSHeatPumpNum ) ) {
				if ( CntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow ) {
					MSHeatPump( MSHeatPumpNum ).FlowFraction = CntrlZoneTerminalUnitMassFlowRateMax / SumOfMassFlowRateMax;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + MSHeatPump( MSHeatPumpNum ).Name );
					ShowContinueError( " The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1." );
				}
				ReportSizingOutput( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name, "Fraction of Supply Air Flow That Goes Through the Controlling Zone", MSHeatPump( MSHeatPumpNum ).FlowFraction );
				MyFlowFracFlag( MSHeatPumpNum ) = false;
			}
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( MSHeatPumpNum ) ) {
			RhoAir = StdRhoAir;
			// set the mass flow rates from the input volume flow rates
			for ( i = 1; i <= NumOfSpeedCooling; ++i ) {
				MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( i ) = RhoAir * MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i );
			}
			for ( i = 1; i <= NumOfSpeedHeating; ++i ) {
				MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( i ) = RhoAir * MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i );
			}
			MSHeatPump( MSHeatPumpNum ).IdleMassFlowRate = RhoAir * MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate;
			// set the node max and min mass flow rates
			Node( InNode ).MassFlowRateMax = max( MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( NumOfSpeedCooling ), MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( NumOfSpeedHeating ) );
			Node( InNode ).MassFlowRateMaxAvail = max( MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( NumOfSpeedCooling ), MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( NumOfSpeedHeating ) );
			Node( InNode ).MassFlowRateMin = 0.0;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
			Node( OutNode ) = Node( InNode );
			MSHeatPump( MSHeatPumpNum ).LoadLoss = 0.0;

			if ( ( MSHeatPump( MSHeatPumpNum ).HeatRecActive ) && ( ! MyPlantScantFlag( MSHeatPumpNum ) ) ) {

				rho = GetDensityGlycol( PlantLoop( MSHeatPump( MSHeatPumpNum ).HRLoopNum ).FluidName, 60.0, PlantLoop( MSHeatPump( MSHeatPumpNum ).HRLoopNum ).FluidIndex, RoutineName );

				MSHeatPump( MSHeatPumpNum ).DesignHeatRecMassFlowRate = MSHeatPump( MSHeatPumpNum ).DesignHeatRecFlowRate * rho;

				InitComponentNodes( 0.0, MSHeatPump( MSHeatPumpNum ).DesignHeatRecMassFlowRate, MSHeatPump( MSHeatPumpNum ).HeatRecInletNodeNum, MSHeatPump( MSHeatPumpNum ).HeatRecOutletNodeNum, MSHeatPump( MSHeatPumpNum ).HRLoopNum, MSHeatPump( MSHeatPumpNum ).HRLoopSideNum, MSHeatPump( MSHeatPumpNum ).HRBranchNum, MSHeatPump( MSHeatPumpNum ).HRCompNum );
			}
			if ( MSHeatPump( MSHeatPumpNum ).CoilControlNode > 0 ) {
				if ( MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow == AutoSize ) {
					if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingWater ) {
						SimulateWaterCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).HeatCoilNum );

						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHeatPumpNum ).HeatCoilName, ErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							rho = GetDensityGlycol( PlantLoop( MSHeatPump( MSHeatPumpNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( MSHeatPump( MSHeatPumpNum ).LoopNum ).FluidIndex, RoutineName );
							MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow = CoilMaxVolFlowRate * rho;
						}
						InitComponentNodes( 0.0, MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow, MSHeatPump( MSHeatPumpNum ).CoilControlNode, MSHeatPump( MSHeatPumpNum ).CoilOutletNode, MSHeatPump( MSHeatPumpNum ).LoopNum, MSHeatPump( MSHeatPumpNum ).LoopSide, MSHeatPump( MSHeatPumpNum ).BranchNum, MSHeatPump( MSHeatPumpNum ).CompNum );
					}
					if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingSteam ) {

						SimulateSteamCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).HeatCoilNum, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( MSHeatPump( MSHeatPumpNum ).HeatCoilNum, ErrorsFound );

						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
							MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
						}
						InitComponentNodes( 0.0, MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow, MSHeatPump( MSHeatPumpNum ).CoilControlNode, MSHeatPump( MSHeatPumpNum ).CoilOutletNode, MSHeatPump( MSHeatPumpNum ).LoopNum, MSHeatPump( MSHeatPumpNum ).LoopSide, MSHeatPump( MSHeatPumpNum ).BranchNum, MSHeatPump( MSHeatPumpNum ).CompNum );
					}
				}
			}
			if ( MSHeatPump( MSHeatPumpNum ).SuppCoilControlNode > 0 ) {
				if ( MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow == AutoSize ) {
					if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == Coil_HeatingWater ) {
						SimulateWaterCoilComponents( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum );

						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, ErrorsFound );
						if ( CoilMaxVolFlowRate != AutoSize ) {
							rho = GetDensityGlycol( PlantLoop( MSHeatPump( MSHeatPumpNum ).SuppLoopNum ).FluidName, InitConvTemp, PlantLoop( MSHeatPump( MSHeatPumpNum ).SuppLoopNum ).FluidIndex, RoutineName );
							MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
						}
						InitComponentNodes( 0.0, MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow, MSHeatPump( MSHeatPumpNum ).SuppCoilControlNode, MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode, MSHeatPump( MSHeatPumpNum ).SuppLoopNum, MSHeatPump( MSHeatPumpNum ).SuppLoopSide, MSHeatPump( MSHeatPumpNum ).SuppBranchNum, MSHeatPump( MSHeatPumpNum ).SuppCompNum );
					}
					if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == Coil_HeatingSteam ) {

						SimulateSteamCoilComponents( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum, ErrorsFound );

						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
							MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
						}
						InitComponentNodes( 0.0, MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow, MSHeatPump( MSHeatPumpNum ).SuppCoilControlNode, MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode, MSHeatPump( MSHeatPumpNum ).SuppLoopNum, MSHeatPump( MSHeatPumpNum ).SuppLoopSide, MSHeatPump( MSHeatPumpNum ).SuppBranchNum, MSHeatPump( MSHeatPumpNum ).SuppCompNum );
					}
				}
			}
			MyEnvrnFlag( MSHeatPumpNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( MSHeatPumpNum ) = true;
		}

		// IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
		if ( ! DoingSizing && MSHeatPump( MSHeatPumpNum ).CheckFanFlow ) {
			CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed";
			GetFanVolFlow( MSHeatPump( MSHeatPumpNum ).FanNum, MSHeatPump( MSHeatPumpNum ).FanVolFlow );
			if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow != AutoSize ) {
				//     Check fan versus system supply air flow rates
				if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow < MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( NumOfSpeedCooling ) ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( MSHeatPump( MSHeatPumpNum ).FanVolFlow, 7 ) + " in fan object " + MSHeatPump( MSHeatPumpNum ).FanName + " is less than the MSHP system air flow rate when cooling is required (" + TrimSigDigits( MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( NumOfSpeedCooling ), 7 ) + ")." );
					ShowContinueError( " The MSHP system flow rate when cooling is required is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHeatPumpNum ).Name );
					MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( NumOfSpeedCooling ) = MSHeatPump( MSHeatPumpNum ).FanVolFlow;
					// Check flow rates in other speeds and ensure flow rates are not above the max flow rate
					for ( i = NumOfSpeedCooling - 1; i >= 1; --i ) {
						if ( MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) > MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i + 1 ) ) {
							ShowContinueError( " The MSHP system flow rate when cooling is required is reset to the flow rate at higher speed and the simulation continues at Speed" + TrimSigDigits( i ) + '.' );
							ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHeatPumpNum ).Name );
							MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i + 1 );
						}
					}
				}
				if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow < MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( NumOfSpeedHeating ) ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( MSHeatPump( MSHeatPumpNum ).FanVolFlow, 7 ) + " in fan object " + MSHeatPump( MSHeatPumpNum ).FanName + " is less than the MSHP system air flow rate when heating is required (" + TrimSigDigits( MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( NumOfSpeedHeating ), 7 ) + ")." );
					ShowContinueError( " The MSHP system flow rate when heating is required is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHeatPumpNum ).Name );
					MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( NumOfSpeedHeating ) = MSHeatPump( MSHeatPumpNum ).FanVolFlow;
					for ( i = NumOfSpeedHeating - 1; i >= 1; --i ) {
						if ( MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) > MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i + 1 ) ) {
							ShowContinueError( " The MSHP system flow rate when heating is required is reset to the flow rate at higher speed and the simulation continues at Speed" + TrimSigDigits( i ) + '.' );
							ShowContinueError( " Occurs in " + CurrentModuleObject + " system = " + MSHeatPump( MSHeatPumpNum ).Name );
							MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i + 1 );
						}
					}
				}
				if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow < MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate && MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate != 0.0 ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( MSHeatPump( MSHeatPumpNum ).FanVolFlow, 7 ) + " in fan object " + MSHeatPump( MSHeatPumpNum ).FanName + " is less than the MSHP system air flow rate when no heating or cooling is needed (" + TrimSigDigits( MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate, 7 ) + ")." );
					ShowContinueError( " The MSHP system flow rate when no heating or cooling is needed is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + MSHeatPump( MSHeatPumpNum ).Name );
					MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate = MSHeatPump( MSHeatPumpNum ).FanVolFlow;
				}
				RhoAir = StdRhoAir;
				// set the mass flow rates from the reset volume flow rates
				for ( i = 1; i <= NumOfSpeedCooling; ++i ) {
					MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( i ) = RhoAir * MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i );
					if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow > 0.0 ) {
						MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( i ) = MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) / MSHeatPump( MSHeatPumpNum ).FanVolFlow;
					}
				}
				for ( i = 1; i <= NumOfSpeedHeating; ++i ) {
					MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( i ) = RhoAir * MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i );
					if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow > 0.0 ) {
						MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( i ) = MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) / MSHeatPump( MSHeatPumpNum ).FanVolFlow;
					}
				}
				MSHeatPump( MSHeatPumpNum ).IdleMassFlowRate = RhoAir * MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate;
				if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow > 0.0 ) {
					MSHeatPump( MSHeatPumpNum ).IdleSpeedRatio = MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate / MSHeatPump( MSHeatPumpNum ).FanVolFlow;
				}
				// set the node max and min mass flow rates based on reset volume flow rates
				Node( InNode ).MassFlowRateMax = max( MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( NumOfSpeedCooling ), MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( NumOfSpeedHeating ) );
				Node( InNode ).MassFlowRateMaxAvail = max( MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( NumOfSpeedCooling ), MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( NumOfSpeedHeating ) );
				Node( InNode ).MassFlowRateMin = 0.0;
				Node( InNode ).MassFlowRateMinAvail = 0.0;
				Node( OutNode ) = Node( InNode );
				MSHeatPump( MSHeatPumpNum ).CheckFanFlow = false;
			}
		}

		if ( MSHeatPump( MSHeatPumpNum ).FanSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( MSHeatPump( MSHeatPumpNum ).FanSchedPtr ) == 0.0 ) {
				MSHeatPump( MSHeatPumpNum ).OpMode = CycFanCycCoil;
			} else {
				MSHeatPump( MSHeatPumpNum ).OpMode = ContFanCycCoil;
			}
		}

		// Calcuate air distribution losses
		if ( ! FirstHVACIteration && AirLoopPass == 1 ) {
			ZoneInNode = MSHeatPump( MSHeatPumpNum ).ZoneInletNode;
			MinHumRat = Node( ZoneInNode ).HumRat;
			if ( Node( OutNode ).Temp < Node( MSHeatPump( MSHeatPumpNum ).NodeNumOfControlledZone ).Temp ) MinHumRat = Node( OutNode ).HumRat;
			DeltaMassRate = Node( OutNode ).MassFlowRate - Node( ZoneInNode ).MassFlowRate / MSHeatPump( MSHeatPumpNum ).FlowFraction;
			if ( DeltaMassRate < 0.0 ) DeltaMassRate = 0.0;
			MSHeatPump( MSHeatPumpNum ).LoadLoss = Node( ZoneInNode ).MassFlowRate / MSHeatPump( MSHeatPumpNum ).FlowFraction * ( PsyHFnTdbW( Node( OutNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( ZoneInNode ).Temp, MinHumRat ) ) + DeltaMassRate * ( PsyHFnTdbW( Node( OutNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( MSHeatPump( MSHeatPumpNum ).NodeNumOfControlledZone ).Temp, MinHumRat ) );
			if ( std::abs( MSHeatPump( MSHeatPumpNum ).LoadLoss ) < 1.0e-6 ) MSHeatPump( MSHeatPumpNum ).LoadLoss = 0.0;
		}

		// Returns load only for zones requesting cooling (heating). If in deadband, Qzoneload = 0.
		ZoneNum = MSHeatPump( MSHeatPumpNum ).ControlZoneNum;
		if ( ( MSHeatPump( MSHeatPumpNum ).ZoneSequenceCoolingNum > 0 ) && ( MSHeatPump( MSHeatPumpNum ).ZoneSequenceHeatingNum > 0 ) ) {
			ZoneLoadToCoolSPSequenced = ZoneSysEnergyDemand( MSHeatPump( MSHeatPumpNum ).ControlZoneNum ).SequencedOutputRequiredToCoolingSP( MSHeatPump( MSHeatPumpNum ).ZoneSequenceCoolingNum );
			ZoneLoadToHeatSPSequenced = ZoneSysEnergyDemand( MSHeatPump( MSHeatPumpNum ).ControlZoneNum ).SequencedOutputRequiredToHeatingSP( MSHeatPump( MSHeatPumpNum ).ZoneSequenceHeatingNum );
			if ( ZoneLoadToHeatSPSequenced > SmallLoad && ZoneLoadToCoolSPSequenced > SmallLoad ) {
				QZnReq = ZoneLoadToHeatSPSequenced;
			} else if ( ZoneLoadToHeatSPSequenced < ( -1.0 * SmallLoad ) && ZoneLoadToCoolSPSequenced < ( -1.0 * SmallLoad ) ) {
				QZnReq = ZoneLoadToCoolSPSequenced;
			} else if ( ZoneLoadToHeatSPSequenced <= ( -1.0 * SmallLoad ) && ZoneLoadToCoolSPSequenced >= SmallLoad ) {
				QZnReq = 0.0;
			} else {
				QZnReq = 0.0; //Autodesk:Init Case added to prevent use of uninitialized value (occurred in MultiSpeedACFurnace example)
			}
			QZnReq /= MSHeatPump( MSHeatPumpNum ).FlowFraction;
		} else {
			QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired / MSHeatPump( MSHeatPumpNum ).FlowFraction;
		}
		if ( CurDeadBandOrSetback( ZoneNum ) ) QZnReq = 0.0;

		if ( QZnReq > SmallLoad ) {
			MSHeatPump( MSHeatPumpNum ).HeatCoolMode = HeatingMode;
		} else if ( QZnReq < ( -1.0 * SmallLoad ) ) {
			MSHeatPump( MSHeatPumpNum ).HeatCoolMode = CoolingMode;
		} else {
			MSHeatPump( MSHeatPumpNum ).HeatCoolMode = 0;
		}

		// Determine the staged status
		if ( allocated( StageZoneLogic ) ) {
			if ( StageZoneLogic( ZoneNum ) ) {
				MSHeatPump( MSHeatPumpNum ).Staged = true;
				MSHeatPump( MSHeatPumpNum ).StageNum = ZoneSysEnergyDemand( ZoneNum ).StageNum;
			} else {
				if ( MyStagedFlag( MSHeatPumpNum ) ) {
					ShowWarningError( "ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to this AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed object = " );
					ShowContinueError( MSHeatPump( MSHeatPumpNum ).Name + ". Please make correction. Simulation continues..." );
					MyStagedFlag( MSHeatPumpNum ) = false;
				}
			}
		}
		// Set the inlet node mass flow rate
		if ( MSHeatPump( MSHeatPumpNum ).OpMode == ContFanCycCoil ) {
			// constant fan mode
			if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( 1 );
				CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( 1 );
				MSHeatPump( MSHeatPumpNum ).LastMode = HeatingMode;
			} else if ( QZnReq < ( -1.0 * SmallLoad ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( 1 );
				CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( 1 );
				MSHeatPump( MSHeatPumpNum ).LastMode = CoolingMode;
			} else {
				CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).IdleMassFlowRate;
				CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).IdleSpeedRatio;
			}
			CompOffMassFlow = MSHeatPump( MSHeatPumpNum ).IdleMassFlowRate;
			CompOffFlowRatio = MSHeatPump( MSHeatPumpNum ).IdleSpeedRatio;
		} else {
			// cycling fan mode
			if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( 1 );
				CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( 1 );
			} else if ( QZnReq < ( -1.0 * SmallLoad ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( 1 );
				CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( 1 );
			} else {
				CompOnMassFlow = 0.0;
				CompOnFlowRatio = 0.0;
			}
			CompOffMassFlow = 0.0;
			CompOffFlowRatio = 0.0;
		}

		// Set the inlet node mass flow rate
		if ( GetCurrentScheduleValue( MSHeatPump( MSHeatPumpNum ).AvaiSchedPtr ) > 0.0 && CompOnMassFlow != 0.0 ) {
			OnOffAirFlowRatio = 1.0;
			if ( FirstHVACIteration ) {
				Node( MSHeatPump( MSHeatPumpNum ).AirInletNodeNum ).MassFlowRate = CompOnMassFlow;
				PartLoadFrac = 0.0;
			} else {
				if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode != 0 ) {
					PartLoadFrac = 1.0;
				} else {
					PartLoadFrac = 0.0;
				}
			}
		} else {
			PartLoadFrac = 0.0;
			Node( InNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRateMaxAvail = 0.0;
			OnOffAirFlowRatio = 1.0;
		}

		// Check availability of DX coils
		if ( GetCurrentScheduleValue( MSHeatPump( MSHeatPumpNum ).AvaiSchedPtr ) > 0.0 ) {
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == CoolingMode ) {
				CoilAvailSchPtr = GetDXCoilAvailSchPtr( "Coil:Cooling:DX:MultiSpeed", MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, ErrorsFound, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex );
				if ( ErrorsFound ) {
					ShowFatalError( "InitMSHeatPump, The previous error causes termination." );
				}
				if ( GetCurrentScheduleValue( CoilAvailSchPtr ) == 0.0 ) {
					if ( MSHeatPump( MSHeatPumpNum ).CoolCountAvail == 0 ) {
						++MSHeatPump( MSHeatPumpNum ).CoolCountAvail;
						ShowWarningError( MSHeatPump( MSHeatPumpNum ).Name + " is ready to perform cooling, but its DX cooling coil = " + MSHeatPump( MSHeatPumpNum ).DXCoolCoilName + " is not available at Available Schedule = " + GetScheduleName( CoilAvailSchPtr ) + '.' );
						ShowContinueErrorTimeStamp( "Availability schedule returned=" + RoundSigDigits( GetCurrentScheduleValue( CoilAvailSchPtr ), 1 ) );
					} else {
						++MSHeatPump( MSHeatPumpNum ).CoolCountAvail;
						ShowRecurringWarningErrorAtEnd( MSHeatPump( MSHeatPumpNum ).Name + ": Cooling coil is still not available ...", MSHeatPump( MSHeatPumpNum ).CoolIndexAvail, GetCurrentScheduleValue( CoilAvailSchPtr ), GetCurrentScheduleValue( CoilAvailSchPtr ) );
					}
				}
			}
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == HeatingMode && MSHeatPump( MSHeatPumpNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
				CoilAvailSchPtr = GetDXCoilAvailSchPtr( "Coil:Heating:DX:MultiSpeed", MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, ErrorsFound, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex );
				if ( ErrorsFound ) {
					ShowFatalError( "InitMSHeatPump, The previous error causes termination." );
				}
				if ( GetCurrentScheduleValue( CoilAvailSchPtr ) == 0.0 ) {
					if ( MSHeatPump( MSHeatPumpNum ).HeatCountAvail == 0 ) {
						++MSHeatPump( MSHeatPumpNum ).HeatCountAvail;
						ShowWarningError( MSHeatPump( MSHeatPumpNum ).Name + " is ready to perform heating, but its DX heating coil = " + MSHeatPump( MSHeatPumpNum ).DXCoolCoilName + " is not available at Available Schedule = " + GetScheduleName( CoilAvailSchPtr ) + '.' );
						ShowContinueErrorTimeStamp( "Availability schedule returned=" + RoundSigDigits( GetCurrentScheduleValue( CoilAvailSchPtr ), 1 ) );
					} else {
						++MSHeatPump( MSHeatPumpNum ).HeatCountAvail;
						ShowRecurringWarningErrorAtEnd( MSHeatPump( MSHeatPumpNum ).Name + ": Heating coil is still not available ...", MSHeatPump( MSHeatPumpNum ).HeatIndexAvail, GetCurrentScheduleValue( CoilAvailSchPtr ), GetCurrentScheduleValue( CoilAvailSchPtr ) );
					}
				}
			}
		}

		MSHeatPumpReport( MSHeatPumpNum ).CycRatio = 0.0;
		MSHeatPumpReport( MSHeatPumpNum ).SpeedRatio = 0.0;
		MSHeatPumpReport( MSHeatPumpNum ).SpeedNum = 0;

		CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, On, 1, 0.0, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );

		auto & e = MSHeatPump( MSHeatPumpNum ); {
			e.TotHeatEnergyRate = 0.0;
			e.SensHeatEnergyRate = 0.0;
			e.LatHeatEnergyRate = 0.0;
			e.TotCoolEnergyRate = 0.0;
			e.SensCoolEnergyRate = 0.0;
			e.LatCoolEnergyRate = 0.0;
		}
		// If unit is scheduled OFF, setpoint is equal to inlet node temperature.
		//!!LKL Discrepancy with < 0
		if ( GetCurrentScheduleValue( MSHeatPump( MSHeatPumpNum ).AvaiSchedPtr ) == 0.0 ) {
			Node( OutNode ).Temp = Node( InNode ).Temp;
			return;
		}

		if ( ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == 0 && MSHeatPump( MSHeatPumpNum ).OpMode == CycFanCycCoil ) || CompOnMassFlow == 0.0 ) {
			QZnReq = 0.0;
			PartLoadFrac = 0.0;
			Node( InNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRateMaxAvail = 0.0;
		}
		MSHeatPump( MSHeatPumpNum ).LoadMet = 0.0;
		SetAverageAirFlow( MSHeatPumpNum, PartLoadFrac, OnOffAirFlowRatio );

		//Init maximum available Heat Recovery flow rate
		if ( ( MSHeatPump( MSHeatPumpNum ).HeatRecActive ) && ( ! MyPlantScantFlag( MSHeatPumpNum ) ) ) {
			if ( PartLoadFrac > 0.0 ) {
				if ( FirstHVACIteration ) {
					MdotHR = MSHeatPump( MSHeatPumpNum ).DesignHeatRecMassFlowRate;
				} else {
					if ( MSHeatPump( MSHeatPumpNum ).HeatRecoveryMassFlowRate > 0.0 ) {
						MdotHR = MSHeatPump( MSHeatPumpNum ).HeatRecoveryMassFlowRate;
					} else {
						MdotHR = MSHeatPump( MSHeatPumpNum ).DesignHeatRecMassFlowRate;
					}
				}
			} else {
				MdotHR = 0.0;
			}

			SetComponentFlowRate( MdotHR, MSHeatPump( MSHeatPumpNum ).HeatRecInletNodeNum, MSHeatPump( MSHeatPumpNum ).HeatRecOutletNodeNum, MSHeatPump( MSHeatPumpNum ).HRLoopNum, MSHeatPump( MSHeatPumpNum ).HRLoopSideNum, MSHeatPump( MSHeatPumpNum ).HRBranchNum, MSHeatPump( MSHeatPumpNum ).HRCompNum );
		}

		// get operating capacity of water and steam coil
		if ( FirstHVACIteration ) {
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingWater ) {
				//     set air-side and steam-side mass flow rates
				Node( MSHeatPump( MSHeatPumpNum ).CoilAirInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow;
				SetComponentFlowRate( mdot, MSHeatPump( MSHeatPumpNum ).CoilControlNode, MSHeatPump( MSHeatPumpNum ).CoilOutletNode, MSHeatPump( MSHeatPumpNum ).LoopNum, MSHeatPump( MSHeatPumpNum ).LoopSide, MSHeatPump( MSHeatPumpNum ).BranchNum, MSHeatPump( MSHeatPumpNum ).CompNum );
				//     simulate water coil to find operating capacity
				SimulateWaterCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).HeatCoilNum, QActual );
			} // from IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingWater) THEN

			if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				Node( MSHeatPump( MSHeatPumpNum ).CoilAirInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow;
				SetComponentFlowRate( mdot, MSHeatPump( MSHeatPumpNum ).CoilControlNode, MSHeatPump( MSHeatPumpNum ).CoilOutletNode, MSHeatPump( MSHeatPumpNum ).LoopNum, MSHeatPump( MSHeatPumpNum ).LoopSide, MSHeatPump( MSHeatPumpNum ).BranchNum, MSHeatPump( MSHeatPumpNum ).CompNum );

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).HeatCoilNum, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil

			} // from IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingSteam) THEN
			if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == Coil_HeatingWater ) {
				//     set air-side and steam-side mass flow rates
				Node( MSHeatPump( MSHeatPumpNum ).SuppCoilAirInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow;
				SetComponentFlowRate( mdot, MSHeatPump( MSHeatPumpNum ).SuppCoilControlNode, MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode, MSHeatPump( MSHeatPumpNum ).SuppLoopNum, MSHeatPump( MSHeatPumpNum ).SuppLoopSide, MSHeatPump( MSHeatPumpNum ).SuppBranchNum, MSHeatPump( MSHeatPumpNum ).SuppCompNum );
				//     simulate water coil to find operating capacity
				SimulateWaterCoilComponents( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum, QActual );
				MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity = QActual;

			} // from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN

			if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				Node( MSHeatPump( MSHeatPumpNum ).SuppCoilAirInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow;
				SetComponentFlowRate( mdot, MSHeatPump( MSHeatPumpNum ).SuppCoilControlNode, MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode, MSHeatPump( MSHeatPumpNum ).SuppLoopNum, MSHeatPump( MSHeatPumpNum ).SuppLoopSide, MSHeatPump( MSHeatPumpNum ).SuppBranchNum, MSHeatPump( MSHeatPumpNum ).SuppCompNum );

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
				MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity = GetSteamCoilCapacity( "Coil:Heating:Steam", MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName, ErrorsFound );

			} // from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingSteam) THEN
		} // from IF( FirstHVACIteration ) THEN

	}

	//******************************************************************************

	void
	SizeMSHeatPump( int const MSHeatPumpNum ) // Engine driven heat pump number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    June 2007
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing multispeed heat pump airflow rates and flow fraction.

		// METHODOLOGY EMPLOYED:

		// REFERENCES: na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using DataZoneEquipment::ZoneEquipConfig;
		using General::TrimSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumOfSpeedCooling; // Number of speeds for cooling
		int NumOfSpeedHeating; // Number of speeds for heating
		int i; // Index to speed

		// FLOW
		NumOfSpeedCooling = MSHeatPump( MSHeatPumpNum ).NumOfSpeedCooling;
		NumOfSpeedHeating = MSHeatPump( MSHeatPumpNum ).NumOfSpeedHeating;

		for ( i = NumOfSpeedCooling; i >= 1; --i ) {

			if ( MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) == AutoSize ) {
				if ( CurSysNum > 0 ) {
					if ( i == NumOfSpeedCooling ) {
						CheckSysSizing( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name );
						MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow < MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) && MSHeatPump( MSHeatPumpNum ).FanVolFlow != AutoSize ) {
							MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).FanVolFlow;
							ShowWarningError( CurrentModuleObject + " \"" + MSHeatPump( MSHeatPumpNum ).Name + "\"" );
							ShowContinueError( "The supply air flow rate at high speed is less than the autosized value for the supply air flow rate in cooling mode. Consider autosizing the fan for this simulation." );
							ShowContinueError( "The air flow rate at high speed in cooling mode is reset to the supply air flow rate and the simulation continues." );
						}
					} else {
						MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( NumOfSpeedCooling ) * i / NumOfSpeedCooling;
					}
					if ( MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) < SmallAirVolFlow ) {
						MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate = 0.0;
					}
					// Ensure the flow rate at lower speed has to be less or equal to the flow rate at higher speed
					if ( i != NumOfSpeedCooling ) {
						if ( MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) > MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i + 1 ) ) {
							MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i + 1 );
						}
					}
					ReportSizingOutput( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name, "Speed " + TrimSigDigits( i ) + " Supply Air Flow Rate During Cooling Operation [m3/s]", MSHeatPump( MSHeatPumpNum ).CoolVolumeFlowRate( i ) );
				}
			}

		}

		for ( i = NumOfSpeedHeating; i >= 1; --i ) {
			if ( MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) == AutoSize ) {
				if ( CurSysNum > 0 ) {
					if ( i == NumOfSpeedHeating ) {
						CheckSysSizing( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name );
						MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow < MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) && MSHeatPump( MSHeatPumpNum ).FanVolFlow != AutoSize ) {
							MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).FanVolFlow;
							ShowWarningError( CurrentModuleObject + " \"" + MSHeatPump( MSHeatPumpNum ).Name + "\"" );
							ShowContinueError( "The supply air flow rate at high speed is less than the autosized value for the maximum air flow rate in heating mode. Consider autosizing the fan for this simulation." );
							ShowContinueError( "The maximum air flow rate at high speed in heating mode is reset to the supply air flow rate and the simulation continues." );
						}
					} else {
						MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( NumOfSpeedHeating ) * i / NumOfSpeedHeating;
					}
					if ( MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) < SmallAirVolFlow ) {
						MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) = 0.0;
					}
					// Ensure the flow rate at lower speed has to be less or equal to the flow rate at higher speed
					if ( i != NumOfSpeedHeating ) {
						if ( MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) > MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i + 1 ) ) {
							MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) = MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i + 1 );
						}
					}
					ReportSizingOutput( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name, "Speed" + TrimSigDigits( i ) + "Supply Air Flow Rate During Heating Operation [m3/s]", MSHeatPump( MSHeatPumpNum ).HeatVolumeFlowRate( i ) );
				}
			}
		}

		if ( MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate == AutoSize ) {
			if ( CurSysNum > 0 ) {
				CheckSysSizing( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name );
				MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				if ( MSHeatPump( MSHeatPumpNum ).FanVolFlow < MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate && MSHeatPump( MSHeatPumpNum ).FanVolFlow != AutoSize ) {
					MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate = MSHeatPump( MSHeatPumpNum ).FanVolFlow;
					ShowWarningError( CurrentModuleObject + " \"" + MSHeatPump( MSHeatPumpNum ).Name + "\"" );
					ShowContinueError( "The supply air flow rate is less than the autosized value for the maximum air flow rate when no heating or cooling is needed. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The maximum air flow rate when no heating or cooling is needed is reset to the supply air flow rate and the simulation continues." );
				}
				if ( MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate < SmallAirVolFlow ) {
					MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate = 0.0;
				}

				ReportSizingOutput( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name, "Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]", MSHeatPump( MSHeatPumpNum ).IdleVolumeAirRate );
			}
		}

		if ( MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp == AutoSize ) {
			if ( CurSysNum > 0 ) {
				if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == 1 ) { // Gas
					CheckZoneSizing( "Coil:Heating:Gas", MSHeatPump( MSHeatPumpNum ).Name );
				} else {
					CheckZoneSizing( "Coil:Heating:Electric", MSHeatPump( MSHeatPumpNum ).Name );
				}
				MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp = FinalSysSizing( CurSysNum ).HeatSupTemp;
				ReportSizingOutput( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name, "Maximum Supply Air Temperature from Supplemental Heater [C]", MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp );
			}
		}

		if ( MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity == AutoSize ) {
			if ( CurSysNum > 0 ) {
				if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType == 1 ) { // Gas
					CheckSysSizing( "Coil:Heating:Gas", MSHeatPump( MSHeatPumpNum ).Name );
				} else {
					CheckSysSizing( "Coil:Heating:Electric", MSHeatPump( MSHeatPumpNum ).Name );
				}
				MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity = FinalSysSizing( CurSysNum ).HeatCap;
			} else {
				MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity = 0.0;
			}
			ReportSizingOutput( CurrentModuleObject, MSHeatPump( MSHeatPumpNum ).Name, "Supplemental Heating Coil Nominal Capacity [W]", MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity );
		}
		SuppHeatCap = MSHeatPump( MSHeatPumpNum ).DesignSuppHeatingCapacity;

		if ( MSHeatPump( MSHeatPumpNum ).HeatRecActive ) {
			RegisterPlantCompDesignFlow( MSHeatPump( MSHeatPumpNum ).HeatRecInletNodeNum, MSHeatPump( MSHeatPumpNum ).DesignHeatRecFlowRate );
		}

	}

	//******************************************************************************

	void
	ControlMSHPOutput(
		int const MSHeatPumpNum, // Unit index of engine driven heat pump
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		int const CompOp, // compressor operation; 1=on, 0=off
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const QZnReq, // cooling or heating output needed by zone [W]
		int const ZoneNum, // Index to zone number
		int & SpeedNum, // Speed number
		Real64 & SpeedRatio, // unit speed ratio for DX coils
		Real64 & PartLoadFrac, // unit part load fraction
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 & SupHeaterLoad // Supplemental heater load [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  Revised for multispeed heat pump use based on ControlPTHPOutput

		// PURPOSE OF THIS SUBROUTINE:
		// Determine the part load fraction at low speed, and speed ratio at high speed for this time step.

		// METHODOLOGY EMPLOYED:
		// Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using DataGlobals::WarmupFlag;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using Psychrometrics::PsyCpAirFnWTdb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FullOutput; // unit full output when compressor is operating [W]
		Real64 LowOutput; // unit full output at low speed [W]
		Real64 TempOutput; // unit output when iteration limit exceeded [W]
		Real64 NoCompOutput; // output when no active compressor [W]
		Real64 ErrorToler; // error tolerance
		int SolFla; // Flag of RegulaFalsi solver
		Array1D< Real64 > Par( 9 ); // Parameters passed to RegulaFalsi
		Real64 CpAir; // air specific heat
		Real64 OutsideDryBulbTemp; // Outside air temperature at external node height
		Real64 QCoilActual; // coil load actually delivered returned to calling component
		int i; // Speed index
		static int ErrCountCyc( 0 ); // Counter used to minimize the occurrence of output warnings
		static int ErrCountVar( 0 ); // Counter used to minimize the occurrence of output warnings

		// FLOW
		SupHeaterLoad = 0.0;
		PartLoadFrac = 0.0;
		SpeedRatio = 0.0;
		SpeedNum = 1;

		OutsideDryBulbTemp = OutDryBulbTemp;

		//!!LKL Discrepancy with < 0
		if ( GetCurrentScheduleValue( MSHeatPump( MSHeatPumpNum ).AvaiSchedPtr ) == 0.0 ) return;

		// Get result when DX coil is off
		CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );

		// If cooling and NoCompOutput < QZnReq, the coil needs to be off
		// If heating and NoCompOutput > QZnReq, the coil needs to be off
		if ( ( QZnReq < ( -1.0 * SmallLoad ) && NoCompOutput < QZnReq ) || ( QZnReq > SmallLoad && NoCompOutput > QZnReq ) || std::abs( QZnReq ) <= SmallLoad ) {
			return;
		}

		// Get full load result
		PartLoadFrac = 1.0;
		SpeedRatio = 1.0;
		if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == HeatingMode ) {
			SpeedNum = MSHeatPump( MSHeatPumpNum ).NumOfSpeedHeating;
			if ( MSHeatPump( MSHeatPumpNum ).Staged && std::abs( MSHeatPump( MSHeatPumpNum ).StageNum ) < SpeedNum ) {
				SpeedNum = std::abs( MSHeatPump( MSHeatPumpNum ).StageNum );
				if ( SpeedNum == 1 ) SpeedRatio = 0.0;
			}
		}
		if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == CoolingMode ) {
			SpeedNum = MSHeatPump( MSHeatPumpNum ).NumOfSpeedCooling;
			if ( MSHeatPump( MSHeatPumpNum ).Staged && std::abs( MSHeatPump( MSHeatPumpNum ).StageNum ) < SpeedNum ) {
				SpeedNum = std::abs( MSHeatPump( MSHeatPumpNum ).StageNum );
				if ( SpeedNum == 1 ) SpeedRatio = 0.0;
			}
		}

		CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );

		if ( QZnReq < ( -1.0 * SmallLoad ) ) {
			// Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
			if ( FullOutput >= 0.0 || FullOutput >= NoCompOutput ) {
				PartLoadFrac = 0.0;
				SpeedRatio = 0.0;
				SpeedNum = 0;
				return;
			}
			//  ! If the QZnReq <= FullOutput the unit needs to run full out
			if ( QZnReq <= FullOutput ) {
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				if ( MSHeatPump( MSHeatPumpNum ).Staged && SpeedNum == 1 ) SpeedRatio = 0.0;
				MSHeatPumpReport( MSHeatPumpNum ).CycRatio = PartLoadFrac;
				MSHeatPumpReport( MSHeatPumpNum ).SpeedRatio = SpeedRatio;
				MSHeatPumpReport( MSHeatPumpNum ).SpeedNum = SpeedNum;
				return;
			}
			ErrorToler = 0.001; //Error tolerance for convergence from input deck
		} else {
			// Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off)
			if ( FullOutput <= 0.0 || FullOutput <= NoCompOutput ) {
				PartLoadFrac = 0.0;
				SpeedRatio = 0.0;
				// may need supplemental heating so don't return in heating mode
			}
			if ( QZnReq >= FullOutput ) {
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				// may need supplemental heating so don't return in heating mode
			}
			ErrorToler = 0.001; //Error tolerance for convergence from input deck
		}

		// Calculate the part load fraction
		if ( ( ( QZnReq > SmallLoad && QZnReq < FullOutput ) || ( QZnReq < ( -1.0 * SmallLoad ) && QZnReq > FullOutput ) ) && ( ! MSHeatPump( MSHeatPumpNum ).Staged ) ) {

			Par( 1 ) = MSHeatPumpNum;
			Par( 2 ) = ZoneNum;
			if ( FirstHVACIteration ) {
				Par( 3 ) = 1.0;
			} else {
				Par( 3 ) = 0.0;
			}
			Par( 4 ) = OpMode;
			Par( 5 ) = QZnReq;
			Par( 6 ) = OnOffAirFlowRatio;
			Par( 7 ) = SupHeaterLoad;
			Par( 9 ) = CompOp;
			// Check whether the low speed coil can meet the load or not
			CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, 1, 0.0, 1.0, LowOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
			if ( ( QZnReq > 0.0 && QZnReq <= LowOutput ) || ( QZnReq < 0.0 && QZnReq >= LowOutput ) ) {
				SpeedRatio = 0.0;
				SpeedNum = 1;
				SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, PartLoadFrac, MSHPCyclingResidual, 0.0, 1.0, Par );
				if ( SolFla == -1 ) {
					if ( ! WarmupFlag ) {
						if ( ErrCountCyc == 0 ) {
							++ErrCountCyc;
							ShowWarningError( "Iteration limit exceeded calculating DX unit cycling ratio, for unit=" + MSHeatPump( MSHeatPumpNum ).Name );
							ShowContinueErrorTimeStamp( "Cycling ratio returned=" + RoundSigDigits( PartLoadFrac, 2 ) );
						} else {
							++ErrCountCyc;
							ShowRecurringWarningErrorAtEnd( MSHeatPump( MSHeatPumpNum ).Name + "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...", MSHeatPump( MSHeatPumpNum ).ErrIndexCyc, PartLoadFrac, PartLoadFrac );
						}
					}
				} else if ( SolFla == -2 ) {
					ShowFatalError( "DX unit cycling ratio calculation failed: cycling limits exceeded, for unit=" + MSHeatPump( MSHeatPumpNum ).DXCoolCoilName );
				}
			} else {
				// Check to see which speed to meet the load
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				if ( QZnReq < ( -1.0 * SmallLoad ) ) { // Cooling
					for ( i = 2; i <= MSHeatPump( MSHeatPumpNum ).NumOfSpeedCooling; ++i ) {
						CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, i, SpeedRatio, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
						if ( QZnReq >= TempOutput ) {
							SpeedNum = i;
							break;
						}
					}
				} else {
					for ( i = 2; i <= MSHeatPump( MSHeatPumpNum ).NumOfSpeedHeating; ++i ) {
						CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, i, SpeedRatio, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
						if ( QZnReq <= TempOutput ) {
							SpeedNum = i;
							break;
						}
					}
				}
				Par( 8 ) = SpeedNum;
				SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, SpeedRatio, MSHPVarSpeedResidual, 0.0, 1.0, Par );
				if ( SolFla == -1 ) {
					if ( ! WarmupFlag ) {
						if ( ErrCountVar == 0 ) {
							++ErrCountVar;
							ShowWarningError( "Iteration limit exceeded calculating DX unit speed ratio, for unit=" + MSHeatPump( MSHeatPumpNum ).Name );
							ShowContinueErrorTimeStamp( "Speed ratio returned=[" + RoundSigDigits( SpeedRatio, 2 ) + "], Speed number =" + RoundSigDigits( SpeedNum ) );
						} else {
							++ErrCountVar;
							ShowRecurringWarningErrorAtEnd( MSHeatPump( MSHeatPumpNum ).Name + "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...", MSHeatPump( MSHeatPumpNum ).ErrIndexVar, SpeedRatio, SpeedRatio );
						}
					}
				} else if ( SolFla == -2 ) {
					ShowFatalError( "DX unit compressor speed calculation failed: speed limits exceeded, for unit=" + MSHeatPump( MSHeatPumpNum ).DXCoolCoilName );
				}
			}
		} else {
			// Staged thermostat performance
			if ( MSHeatPump( MSHeatPumpNum ).StageNum != 0 ) {
				Par( 1 ) = MSHeatPumpNum;
				Par( 2 ) = ZoneNum;
				if ( FirstHVACIteration ) {
					Par( 3 ) = 1.0;
				} else {
					Par( 3 ) = 0.0;
				}
				Par( 4 ) = OpMode;
				Par( 5 ) = QZnReq;
				Par( 6 ) = OnOffAirFlowRatio;
				Par( 7 ) = SupHeaterLoad;
				Par( 9 ) = CompOp;
				SpeedNum = std::abs( MSHeatPump( MSHeatPumpNum ).StageNum );
				Par( 8 ) = SpeedNum;
				if ( SpeedNum == 1 ) {
					CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, 1, 0.0, 1.0, LowOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
					SpeedRatio = 0.0;
					if ( ( QZnReq > 0.0 && QZnReq <= LowOutput ) || ( QZnReq < 0.0 && QZnReq >= LowOutput ) ) {
						SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, PartLoadFrac, MSHPCyclingResidual, 0.0, 1.0, Par );
						if ( SolFla == -1 ) {
							if ( ! WarmupFlag ) {
								if ( ErrCountCyc == 0 ) {
									++ErrCountCyc;
									ShowWarningError( "Iteration limit exceeded calculating DX unit cycling ratio, for unit=" + MSHeatPump( MSHeatPumpNum ).Name );
									ShowContinueErrorTimeStamp( "Cycling ratio returned=" + RoundSigDigits( PartLoadFrac, 2 ) );
								} else {
									++ErrCountCyc;
									ShowRecurringWarningErrorAtEnd( MSHeatPump( MSHeatPumpNum ).Name + "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...", MSHeatPump( MSHeatPumpNum ).ErrIndexCyc, PartLoadFrac, PartLoadFrac );
								}
							}
						} else if ( SolFla == -2 ) {
							ShowFatalError( "DX unit cycling ratio calculation failed: cycling limits exceeded, for unit=" + MSHeatPump( MSHeatPumpNum ).DXCoolCoilName );
						}
					} else {
						FullOutput = LowOutput;
						PartLoadFrac = 1.0;
					}
				} else {
					if ( MSHeatPump( MSHeatPumpNum ).StageNum < 0 ) {
						SpeedNum = min( MSHeatPump( MSHeatPumpNum ).NumOfSpeedCooling, std::abs( MSHeatPump( MSHeatPumpNum ).StageNum ) );
					} else {
						SpeedNum = min( MSHeatPump( MSHeatPumpNum ).NumOfSpeedHeating, std::abs( MSHeatPump( MSHeatPumpNum ).StageNum ) );
					}
					CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, 0.0, 1.0, LowOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
					if ( ( QZnReq > 0.0 && QZnReq >= LowOutput ) || ( QZnReq < 0.0 && QZnReq <= LowOutput ) ) {
						CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, 1.0, 1.0, FullOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
						if ( ( QZnReq > 0.0 && QZnReq <= FullOutput ) || ( QZnReq < 0.0 && QZnReq >= FullOutput ) ) {
							Par( 8 ) = SpeedNum;
							SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, SpeedRatio, MSHPVarSpeedResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( ErrCountVar == 0 ) {
										++ErrCountVar;
										ShowWarningError( "Iteration limit exceeded calculating DX unit speed ratio, for unit=" + MSHeatPump( MSHeatPumpNum ).Name );
										ShowContinueErrorTimeStamp( "Speed ratio returned=[" + RoundSigDigits( SpeedRatio, 2 ) + "], Speed number =" + RoundSigDigits( SpeedNum ) );
									} else {
										++ErrCountVar;
										ShowRecurringWarningErrorAtEnd( MSHeatPump( MSHeatPumpNum ).Name + "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...", MSHeatPump( MSHeatPumpNum ).ErrIndexVar, SpeedRatio, SpeedRatio );
									}
								}
							} else if ( SolFla == -2 ) {
								ShowFatalError( "DX unit compressor speed calculation failed: speed limits exceeded, for unit=" + MSHeatPump( MSHeatPumpNum ).DXCoolCoilName );
							}
						} else {
							SpeedRatio = 1.0;
						}
					} else { // lowOutput provides a larger capacity than needed
						SpeedRatio = 0.0;
					}
				}
			}
		}

		// if the DX heating coil cannot meet the load, trim with supplemental heater
		// occurs with constant fan mode when compressor is on or off
		// occurs with cycling fan mode when compressor PLR is equal to 1
		if ( ( QZnReq > SmallLoad && QZnReq > FullOutput ) ) {
			PartLoadFrac = 1.0;
			SpeedRatio = 1.0;
			if ( MSHeatPump( MSHeatPumpNum ).Staged && SpeedNum == 1 ) SpeedRatio = 0.0;
			if ( OutsideDryBulbTemp <= MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp ) {
				SupHeaterLoad = QZnReq - FullOutput;
			} else {
				SupHeaterLoad = 0.0;
			}
			CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );
		}

		// check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
		if ( Node( MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum ).Temp > MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp && SupHeaterLoad > 0.0 ) {

			//   If the supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
			SupHeaterLoad = 0.0;
			CalcNonDXHeatingCoils( MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, OpMode, QCoilActual );

			//   If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
			//   the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
			//   use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
			//   of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
			if ( Node( MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum ).Temp < MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp ) {
				CpAir = PsyCpAirFnWTdb( Node( MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum ).HumRat, Node( MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum ).Temp );
				SupHeaterLoad = Node( MSHeatPump( MSHeatPumpNum ).AirInletNodeNum ).MassFlowRate * CpAir * ( MSHeatPump( MSHeatPumpNum ).SuppMaxAirTemp - Node( MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum ).Temp );

			} else {
				SupHeaterLoad = 0.0;
			}
		}

		MSHeatPumpReport( MSHeatPumpNum ).CycRatio = PartLoadFrac;
		MSHeatPumpReport( MSHeatPumpNum ).SpeedRatio = SpeedRatio;
		MSHeatPumpReport( MSHeatPumpNum ).SpeedNum = SpeedNum;

	}

	//******************************************************************************

	void
	CalcMSHeatPump(
		int const MSHeatPumpNum, // Engine driven heat pump number
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		int const CompOp, // Compressor on/off; 1=on, 0=off
		int const SpeedNum, // Speed number
		Real64 const SpeedRatio, // Compressor speed ratio
		Real64 const PartLoadFrac, // Compressor part load fraction
		Real64 & LoadMet, // Load met by unit (W)
		Real64 const QZnReq, // Zone load (W)
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 & SupHeaterLoad // supplemental heater load (W)
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    June 2007
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will calcultes MSHP performance based on given system load

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES: na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using DataEnvironment::OutDryBulbTemp;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::DXCoilPartLoadRatio;
		using DXCoils::DXCoil;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  INTEGER, PARAMETER  ::   On  = 1           ! Compressor on flag
		//  INTEGER, PARAMETER  ::   Off = 2           ! Compressor off flag

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVMS TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // MSHP air outlet node
		int InletNode; // MSHP air inlet node
		Real64 MinHumRat; // Minimum humidity ratio for sensible capacity calculation (kg/kg)
		Real64 OutsideDryBulbTemp; // Outdoor dry bulb temperature [C]
		Real64 AirMassFlow; // Air mass flow rate [kg/s]
		int FanInletNode; // MSHP air outlet node
		int FanOutletNode; // MSHP air inlet node
		Real64 SavePartloadRatio;
		Real64 SaveSpeedRatio;
		Real64 QCoilActual; // coil load actually delivered returned to calling component
		Real64 MinWaterFlow; // minimum water flow rate
		Real64 ErrorToler; // supplemental heating coil convergence tollerance

		// FLOW
		OutletNode = MSHeatPump( MSHeatPumpNum ).AirOutletNodeNum;
		InletNode = MSHeatPump( MSHeatPumpNum ).AirInletNodeNum;
		if ( MSHeatPump(MSHeatPumpNum).DXHeatCoilIndex > 0 ) {
			if ( DXCoil( MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex ).IsSecondaryDXCoilInZone ) {
				OutsideDryBulbTemp = ZT( DXCoil( MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex ).SecZonePtr );
			} else {
				OutsideDryBulbTemp = OutDryBulbTemp;
			}
		} else if ( MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex > 0) {
			if ( DXCoil( MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex ).IsSecondaryDXCoilInZone ) {
				OutsideDryBulbTemp = ZT( DXCoil( MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex ).SecZonePtr );
			} else {
				OutsideDryBulbTemp = OutDryBulbTemp;
			}
		} else {
			OutsideDryBulbTemp = OutDryBulbTemp;
		}
		FanOutletNode = MSHeatPump( MSHeatPumpNum ).FanOutletNode;
		FanInletNode = MSHeatPump( MSHeatPumpNum ).FanInletNode;

		SaveCompressorPLR = 0.0;
		SavePartloadRatio = 0.0;
		MinWaterFlow = 0.0;
		ErrorToler = 0.001;
		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		SetAverageAirFlow( MSHeatPumpNum, PartLoadFrac, OnOffAirFlowRatio, SpeedNum, SpeedRatio );

		AirMassFlow = Node( InletNode ).MassFlowRate;
		// if blow through, simulate fan then coils
		if ( MSHeatPump( MSHeatPumpNum ).FanPlaceType == BlowThru ) {
			SimulateFanComponents( MSHeatPump( MSHeatPumpNum ).FanName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).FanNum, FanSpeedRatio );
			if ( QZnReq < ( -1.0 * SmallLoad ) ) {
				if ( OutsideDryBulbTemp > MSHeatPump( MSHeatPumpNum ).MinOATCompressor ) {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, SpeedRatio, PartLoadFrac, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
					SavePartloadRatio = PartLoadFrac;
					SaveSpeedRatio = SpeedRatio;
				} else {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
				}
				SaveCompressorPLR = DXCoilPartLoadRatio( MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex );
			} else {
				SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
			}
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
				if ( QZnReq > SmallLoad ) {
					if ( OutsideDryBulbTemp > MSHeatPump( MSHeatPumpNum ).MinOATCompressor ) {
						SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, SpeedRatio, PartLoadFrac, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
						SavePartloadRatio = PartLoadFrac;
						SaveSpeedRatio = SpeedRatio;
					} else {
						SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
					}
					SaveCompressorPLR = DXCoilPartLoadRatio( MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex );
				} else {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
				}
			} else if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingElectric_MultiStage || MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingGas_MultiStage ) {
				if ( QZnReq > SmallLoad ) {
					SimulateHeatingCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, _, 0, _, _, MSHeatPump( MSHeatPumpNum ).OpMode, PartLoadFrac, SpeedNum, SpeedRatio );
				} else {
					SimulateHeatingCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, _, 0, _, _, MSHeatPump( MSHeatPumpNum ).OpMode, 0.0, SpeedNum, 0.0 );
				}
			} else {
				CalcNonDXHeatingCoils( MSHeatPumpNum, FirstHVACIteration, QZnReq, MSHeatPump( MSHeatPumpNum ).OpMode, QCoilActual, PartLoadFrac );
			}
			// Call twice to ensure the fan outlet conditions are updated
			SimulateFanComponents( MSHeatPump( MSHeatPumpNum ).FanName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).FanNum, FanSpeedRatio );
			if ( QZnReq < ( -1.0 * SmallLoad ) ) {
				if ( OutsideDryBulbTemp > MSHeatPump( MSHeatPumpNum ).MinOATCompressor ) {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, SpeedRatio, PartLoadFrac, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
					SavePartloadRatio = PartLoadFrac;
					SaveSpeedRatio = SpeedRatio;
				} else {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
				}
				SaveCompressorPLR = DXCoilPartLoadRatio( MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex );
			} else {
				SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
			}
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
				if ( QZnReq > SmallLoad ) {
					if ( OutsideDryBulbTemp > MSHeatPump( MSHeatPumpNum ).MinOATCompressor ) {
						SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, SpeedRatio, PartLoadFrac, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
						SavePartloadRatio = PartLoadFrac;
						SaveSpeedRatio = SpeedRatio;
					} else {
						SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
					}
					SaveCompressorPLR = DXCoilPartLoadRatio( MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex );
				} else {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
				}
			} else if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingElectric_MultiStage || MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingGas_MultiStage ) {
				if ( QZnReq > SmallLoad ) {
					SimulateHeatingCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, _, 0, _, _, MSHeatPump( MSHeatPumpNum ).OpMode, PartLoadFrac, SpeedNum, SpeedRatio );
				} else {
					SimulateHeatingCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, _, 0, _, _, MSHeatPump( MSHeatPumpNum ).OpMode, 0.0, SpeedNum, 0.0 );
				}
			} else {
				CalcNonDXHeatingCoils( MSHeatPumpNum, FirstHVACIteration, QZnReq, MSHeatPump( MSHeatPumpNum ).OpMode, QCoilActual, PartLoadFrac );
			}
			//  Simulate supplemental heating coil for blow through fan
			if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum > 0 ) {
				CalcNonDXHeatingCoils( MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, MSHeatPump( MSHeatPumpNum ).OpMode, QCoilActual );
			}
		} else { // otherwise simulate DX coils then fan then supplemental heater
			if ( QZnReq < ( -1.0 * SmallLoad ) ) {
				if ( OutsideDryBulbTemp > MSHeatPump( MSHeatPumpNum ).MinOATCompressor ) {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, SpeedRatio, PartLoadFrac, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
					SavePartloadRatio = PartLoadFrac;
					SaveSpeedRatio = SpeedRatio;
				} else {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
				}
				SaveCompressorPLR = DXCoilPartLoadRatio( MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex );
			} else {
				SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXCoolCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXCoolCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
			}
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == MultiSpeedHeatingCoil ) {
				if ( QZnReq > SmallLoad ) {
					if ( OutsideDryBulbTemp > MSHeatPump( MSHeatPumpNum ).MinOATCompressor ) {
						SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, SpeedRatio, PartLoadFrac, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
						SavePartloadRatio = PartLoadFrac;
						SaveSpeedRatio = SpeedRatio;
					} else {
						SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
					}
					SaveCompressorPLR = DXCoilPartLoadRatio( MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex );
				} else {
					SimDXCoilMultiSpeed( MSHeatPump( MSHeatPumpNum ).DXHeatCoilName, 0.0, 0.0, MSHeatPump( MSHeatPumpNum ).DXHeatCoilIndex, SpeedNum, MSHeatPump( MSHeatPumpNum ).OpMode, CompOp );
				}
			} else if ( MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingElectric_MultiStage || MSHeatPump( MSHeatPumpNum ).HeatCoilType == Coil_HeatingGas_MultiStage ) {
				if ( QZnReq > SmallLoad ) {
					SimulateHeatingCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, _, 0, _, _, MSHeatPump( MSHeatPumpNum ).OpMode, PartLoadFrac, SpeedNum, SpeedRatio );
				} else {
					SimulateHeatingCoilComponents( MSHeatPump( MSHeatPumpNum ).HeatCoilName, FirstHVACIteration, _, 0, _, _, MSHeatPump( MSHeatPumpNum ).OpMode, 0.0, SpeedNum, 0.0 );
				}
			} else {
				CalcNonDXHeatingCoils( MSHeatPumpNum, FirstHVACIteration, QZnReq, MSHeatPump( MSHeatPumpNum ).OpMode, QCoilActual, PartLoadFrac );
			}
			SimulateFanComponents( MSHeatPump( MSHeatPumpNum ).FanName, FirstHVACIteration, MSHeatPump( MSHeatPumpNum ).FanNum, FanSpeedRatio );
			//  Simulate supplemental heating coil for draw through fan
			if ( MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum > 0 ) {
				CalcNonDXHeatingCoils( MSHeatPumpNum, FirstHVACIteration, SupHeaterLoad, MSHeatPump( MSHeatPumpNum ).OpMode, QCoilActual );
			}
		}

		// calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
		MinHumRat = Node( MSHeatPump( MSHeatPumpNum ).NodeNumOfControlledZone ).HumRat;
		if ( Node( OutletNode ).Temp < Node( MSHeatPump( MSHeatPumpNum ).NodeNumOfControlledZone ).Temp ) MinHumRat = Node( OutletNode ).HumRat;
		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( MSHeatPump( MSHeatPumpNum ).NodeNumOfControlledZone ).Temp, MinHumRat ) ) - MSHeatPump( MSHeatPumpNum ).LoadLoss;

		MSHeatPump( MSHeatPumpNum ).LoadMet = LoadMet;

	}

	//******************************************************************************

	Real64
	MSHPCyclingResidual(
		Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = MSHPNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  Revised for multispeed heat pump use based on DXCoilCyclingResidual

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
		//  MSHP output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcMSHeatPump to get ActualOutput at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 MSHPCyclingResidual;

		// Argument array dimensioning

		// Locals
		Real64 SupHeaterLoad; // Supplemental heater load

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = Zone Num
		// par(3) = FirstHVACIteration
		// par(4) = OpMode
		// par(5) = QZnReq
		// par(6) = OnOffAirFlowRatio
		// par(7) = SupHeaterLoad
		// par(9) = CompOp

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int MSHeatPumpNum; // MSHP index
		int ZoneNum; // Zone index
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Compressor operating mode
		Real64 QZnReq; // zone load (W)
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 ActualOutput; // delivered capacity of MSHP
		int CompOp; // compressor operation; 1=on, 0=off

		MSHeatPumpNum = int( Par( 1 ) );
		ZoneNum = int( Par( 2 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		OpMode = int( Par( 4 ) );
		QZnReq = Par( 5 );
		OnOffAirFlowRatio = Par( 6 );
		SupHeaterLoad = Par( 7 );
		CompOp = int( Par( 9 ) );

		CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, 1, 0.0, PartLoadFrac, ActualOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );

		MSHPCyclingResidual = ( ActualOutput - QZnReq ) / QZnReq;
		return MSHPCyclingResidual;

	}

	//******************************************************************************

	Real64
	MSHPVarSpeedResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = MSHPNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na L. Gu, Oct. 2006, revised for multispeed heat pump use
		//       RE-ENGINEERED  Revised for multispeed heat pump use based on DXCoilVarSpeedResidual

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
		//  MSHP output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcMSHeatPump to get ActualOutput at the given speed ratio (partload ratio for high speed)
		//  and calculates the residual as defined above

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 MSHPVarSpeedResidual;

		// Argument array dimensioning

		// Locals
		Real64 SupHeaterLoad; // Supplemental heater load

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = Zone Num
		// par(3) = FirstHVACIteration
		// par(4) = OpMode
		// par(5) = QZnReq
		// par(6) = OnOffAirFlowRatio
		// par(7) = SupHeaterLoad
		// par(8) = SpeedNum
		// par(9) = CompOp

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int MSHeatPumpNum; // MSHP index
		int ZoneNum; // Zone index
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Compressor operating mode
		Real64 QZnReq; // zone load (W)
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 ActualOutput; // delivered capacity of MSHP
		int SpeedNum; // Speed number
		int CompOp; // compressor operation; 1=on, 0=off

		MSHeatPumpNum = int( Par( 1 ) );
		ZoneNum = int( Par( 2 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		OpMode = int( Par( 4 ) );
		QZnReq = Par( 5 );
		OnOffAirFlowRatio = Par( 6 );
		SupHeaterLoad = Par( 7 );
		SpeedNum = int( Par( 8 ) );
		CompOp = int( Par( 9 ) );

		CalcMSHeatPump( MSHeatPumpNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, 1.0, ActualOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad );

		MSHPVarSpeedResidual = ( ActualOutput - QZnReq ) / QZnReq;
		return MSHPVarSpeedResidual;

	}

	//******************************************************************************

	void
	UpdateMSHeatPump( int const MSHeatPumpNum ) // Engine driven heat pump number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    June 2007
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will update MSHP performance and calculate heat recovery rate and crankcase heater power

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES: na

		// Using/Aliasing
		using DataAirLoop::LoopSystemOnMassFlowrate;
		using DataAirLoop::LoopSystemOffMassFlowrate;
		using DataAirLoop::LoopFanOperationMode;
		using DataAirLoop::LoopOnOffFanPartLoadRatio;
		using DataAirLoop::LoopCompCycRatio;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Calculate heat recovery
		if ( MSHeatPump( MSHeatPumpNum ).HeatRecActive ) {
			MSHPHeatRecovery( MSHeatPumpNum );
		}

		LoopSystemOnMassFlowrate = CompOnMassFlow;
		LoopSystemOffMassFlowrate = CompOffMassFlow;
		LoopFanOperationMode = MSHeatPump( MSHeatPumpNum ).OpMode;
		LoopOnOffFanPartLoadRatio = MSHeatPump( MSHeatPumpNum ).FanPartLoadRatio;
		LoopCompCycRatio = MSHeatPumpReport( MSHeatPumpNum ).CycRatio;

	}

	//******************************************************************************

	void
	ReportMSHeatPump( int const MSHeatPumpNum ) // Engine driven heat pump number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    June 2007
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will write values to output variables in MSHP

		// METHODOLOGY EMPLOYED:

		// REFERENCES: na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;
		MSHeatPumpReport( MSHeatPumpNum ).ElecPowerConsumption = MSHeatPump( MSHeatPumpNum ).ElecPower * ReportingConstant; // + &
		MSHeatPumpReport( MSHeatPumpNum ).HeatRecoveryEnergy = MSHeatPump( MSHeatPumpNum ).HeatRecoveryRate * ReportingConstant;

		MSHeatPumpReport( MSHeatPumpNum ).AuxElecHeatConsumption = 0.0;
		MSHeatPumpReport( MSHeatPumpNum ).AuxElecCoolConsumption = 0.0;

		MSHeatPump( MSHeatPumpNum ).AuxElecPower = MSHeatPump( MSHeatPumpNum ).AuxOnCyclePower * SaveCompressorPLR + MSHeatPump( MSHeatPumpNum ).AuxOffCyclePower * ( 1.0 - SaveCompressorPLR );
		if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == CoolingMode ) {
			MSHeatPumpReport( MSHeatPumpNum ).AuxElecCoolConsumption = MSHeatPump( MSHeatPumpNum ).AuxOnCyclePower * SaveCompressorPLR * ReportingConstant;
		}
		if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == HeatingMode ) {
			MSHeatPumpReport( MSHeatPumpNum ).AuxElecHeatConsumption = MSHeatPump( MSHeatPumpNum ).AuxOnCyclePower * SaveCompressorPLR * ReportingConstant;
		}
		if ( MSHeatPump( MSHeatPumpNum ).LastMode == HeatingMode ) {
			MSHeatPumpReport( MSHeatPumpNum ).AuxElecHeatConsumption += MSHeatPump( MSHeatPumpNum ).AuxOffCyclePower * ( 1.0 - SaveCompressorPLR ) * ReportingConstant;
		} else {
			MSHeatPumpReport( MSHeatPumpNum ).AuxElecCoolConsumption += MSHeatPump( MSHeatPumpNum ).AuxOffCyclePower * ( 1.0 - SaveCompressorPLR ) * ReportingConstant;
		}

	}

	void
	MSHPHeatRecovery( int const MSHeatPumpNum ) // Number of the current electric MSHP being simulated
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu
		//       DATE WRITTEN:    June 2007
		//       MODIFIED:        na
		//       RE-ENGINEERED    Revised to calculate MSHP heat recovery rate based on EIR Chiller heat recovery subroutine
		// PURPOSE OF THIS SUBROUTINE:
		//  Calculate the heat recovered from MSHP

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataHVACGlobals::MSHPWasteHeat;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "MSHPHeatRecovery" );

		// DERIVMS TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecInNode; // Node number of heat recovery water inlet node
		int HeatRecOutNode; // Node number of heat recovery water outlet node
		Real64 QHeatRec; // Total heat recovered [W]
		Real64 HeatRecInletTemp; // Heat reclaim inlet temp [C]
		Real64 HeatRecOutletTemp; // Heat reclaim outlet temp [C]
		Real64 HeatRecMassFlowRate; // Heat reclaim mass flow rate [m3/s]
		Real64 CpHeatRec; // Heat reclaim water inlet specific heat [J/kg-K]
		Real64 HeatRecInletEnth; // Heat reclaim water inlet enthalpy [J/kg]

		// Begin routine
		HeatRecInNode = MSHeatPump( MSHeatPumpNum ).HeatRecInletNodeNum;
		HeatRecOutNode = MSHeatPump( MSHeatPumpNum ).HeatRecOutletNodeNum;

		// Inlet node to the heat recovery heat exchanger
		HeatRecInletTemp = Node( HeatRecInNode ).Temp;
		HeatRecInletEnth = Node( HeatRecInNode ).Enthalpy;

		// Set heat recovery mass flow rates
		HeatRecMassFlowRate = Node( HeatRecInNode ).MassFlowRate;

		QHeatRec = MSHPWasteHeat;

		if ( HeatRecMassFlowRate > 0.0 ) {

			CpHeatRec = GetSpecificHeatGlycol( PlantLoop( MSHeatPump( MSHeatPumpNum ).HRLoopNum ).FluidName, HeatRecInletTemp, PlantLoop( MSHeatPump( MSHeatPumpNum ).HRLoopNum ).FluidIndex, RoutineName );

			HeatRecOutletTemp = QHeatRec / ( HeatRecMassFlowRate * CpHeatRec ) + HeatRecInletTemp;
			if ( HeatRecOutletTemp > MSHeatPump( MSHeatPumpNum ).MaxHeatRecOutletTemp ) HeatRecOutletTemp = MSHeatPump( MSHeatPumpNum ).MaxHeatRecOutletTemp;
		} else {
			HeatRecOutletTemp = HeatRecInletTemp;
		}

		SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
		// changed outputs
		Node( HeatRecOutNode ).Temp = HeatRecOutletTemp;

		MSHeatPump( MSHeatPumpNum ).HeatRecoveryRate = QHeatRec;
		MSHeatPump( MSHeatPumpNum ).HeatRecoveryInletTemp = HeatRecInletTemp;
		MSHeatPump( MSHeatPumpNum ).HeatRecoveryOutletTemp = HeatRecOutletTemp;
		MSHeatPump( MSHeatPumpNum ).HeatRecoveryMassFlowRate = HeatRecMassFlowRate;

	}

	void
	SetAverageAirFlow(
		int const MSHeatPumpNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to average airflow over timestep
		Optional_int_const SpeedNum, // Speed number
		Optional< Real64 const > SpeedRatio // Speed ratio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  Resived to meet requirements of multispeed heat pump based on the same subroutine
		//                      in PTHP module

		// PURPOSE OF THIS SUBROUTINE:
		// Set the average air mass flow rates using the part load fraction of the heat pump for this time step
		// Set OnOffAirFlowRatio to be used by DX coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataHVACGlobals::MSHPMassFlowRateLow;
		using DataHVACGlobals::MSHPMassFlowRateHigh;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVMS TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // inlet node number for PTHPNum
		Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step

		MSHPMassFlowRateLow = 0.0; // Mass flow rate at low speed
		MSHPMassFlowRateHigh = 0.0; // Mass flow rate at high speed

		if ( ! CurDeadBandOrSetback( MSHeatPump( MSHeatPumpNum ).ControlZoneNum ) && present( SpeedNum ) ) {
			if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == HeatingMode ) {
				if ( SpeedNum == 1 ) {
					CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( SpeedNum );
					CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( SpeedNum );
					MSHPMassFlowRateLow = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( 1 );
					MSHPMassFlowRateHigh = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( 1 );
				} else if ( SpeedNum > 1 ) {
					CompOnMassFlow = SpeedRatio * MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( SpeedNum ) + ( 1.0 - SpeedRatio ) * MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( SpeedNum - 1 );
					CompOnFlowRatio = SpeedRatio * MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( SpeedNum ) + ( 1.0 - SpeedRatio ) * MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( SpeedNum - 1 );
					MSHPMassFlowRateLow = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( SpeedNum - 1 );
					MSHPMassFlowRateHigh = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( SpeedNum );
				}
			} else if ( MSHeatPump( MSHeatPumpNum ).HeatCoolMode == CoolingMode ) {
				if ( SpeedNum == 1 ) {
					CompOnMassFlow = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( SpeedNum );
					CompOnFlowRatio = MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( SpeedNum );
					MSHPMassFlowRateLow = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( 1 );
					MSHPMassFlowRateHigh = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( 1 );
				} else if ( SpeedNum > 1 ) {
					CompOnMassFlow = SpeedRatio * MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( SpeedNum ) + ( 1.0 - SpeedRatio ) * MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( SpeedNum - 1 );
					CompOnFlowRatio = SpeedRatio * MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( SpeedNum ) + ( 1.0 - SpeedRatio ) * MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( SpeedNum - 1 );
					MSHPMassFlowRateLow = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( SpeedNum - 1 );
					MSHPMassFlowRateHigh = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( SpeedNum );
				}
			}
		}
		InletNode = MSHeatPump( MSHeatPumpNum ).AirInletNodeNum;

		// Set up fan flow rate during compressor off time
		if ( MSHeatPump( MSHeatPumpNum ).OpMode == ContFanCycCoil && present( SpeedNum ) ) {
			if ( MSHeatPump( MSHeatPumpNum ).AirFlowControl == UseCompressorOnFlow && CompOnMassFlow > 0.0 ) {
				if ( MSHeatPump( MSHeatPumpNum ).LastMode == HeatingMode ) {
					CompOffMassFlow = MSHeatPump( MSHeatPumpNum ).HeatMassFlowRate( SpeedNum );
					CompOffFlowRatio = MSHeatPump( MSHeatPumpNum ).HeatingSpeedRatio( SpeedNum );
				} else {
					CompOffMassFlow = MSHeatPump( MSHeatPumpNum ).CoolMassFlowRate( SpeedNum );
					CompOffFlowRatio = MSHeatPump( MSHeatPumpNum ).CoolingSpeedRatio( SpeedNum );
				}
			}
		}

		if ( present( SpeedNum ) ) {
			if ( SpeedNum > 1 ) {
				AverageUnitMassFlow = CompOnMassFlow;
				FanSpeedRatio = CompOnFlowRatio;
			} else {
				AverageUnitMassFlow = ( PartLoadRatio * CompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * CompOffMassFlow );
				if ( CompOffFlowRatio > 0.0 ) {
					FanSpeedRatio = ( PartLoadRatio * CompOnFlowRatio ) + ( ( 1 - PartLoadRatio ) * CompOffFlowRatio );
				} else {
					FanSpeedRatio = CompOnFlowRatio;
				}
			}
		} else {
			AverageUnitMassFlow = ( PartLoadRatio * CompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * CompOffMassFlow );
			if ( CompOffFlowRatio > 0.0 ) {
				FanSpeedRatio = ( PartLoadRatio * CompOnFlowRatio ) + ( ( 1 - PartLoadRatio ) * CompOffFlowRatio );
			} else {
				FanSpeedRatio = CompOnFlowRatio;
			}
		}

		//!!LKL Discrepancy with > 0
		if ( GetCurrentScheduleValue( MSHeatPump( MSHeatPumpNum ).AvaiSchedPtr ) == 0.0 ) {
			Node( InletNode ).MassFlowRate = 0.0;
			OnOffAirFlowRatio = 0.0;
		} else {
			Node( InletNode ).MassFlowRate = AverageUnitMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = AverageUnitMassFlow;
			if ( AverageUnitMassFlow > 0.0 ) {
				OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
			} else {
				OnOffAirFlowRatio = 0.0;
			}
		}

	}

	void
	CalcNonDXHeatingCoils(
		int const MSHeatPumpNum, // multispeed heatpump index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 const HeatingLoad, // supplemental coil load to be met by unit (watts)
		int const FanMode, // fan operation mode
		Real64 & HeatCoilLoadmet, // Heating Load Met
		Optional< Real64 const > PartLoadFrac
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different heating coil component.  The hot water flow rate matching the coil load
		// is calculated iteratively.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DataHVACGlobals::SmallLoad;

		// Locals
		static std::string const CurrentModuleObject( "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed" );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrTolerance( 0.001 ); // convergence limit for hotwater coil
		int const SolveMaxIter( 50 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QCoilActual; // actual heating load met
		Real64 mdot; // heating coil steam or hot water mass flow rate
		Real64 MinWaterFlow; // coil minimum hot water mass flow rate, kg/s
		Real64 MaxHotWaterFlow; // coil maximum hot water mass flow rate, kg/s
		Real64 HotWaterMdot; // actual hot water mass flow rate
		Array1D< Real64 > Par( 3 );
		int SolFlag;

		static std::string HeatCoilName;
		int HeatCoilType;
		int HeatCoilNum;
		Real64 MaxCoilFluidFlow;
		Real64 SteamCoilHeatingLoad;
		int CoilControlNode;
		int CoilOutletNode;
		int LoopNum;
		int LoopSide;
		int BranchNum;
		int CompNum;

		QCoilActual = 0.0;

		if ( present( PartLoadFrac ) ) {
			HeatCoilType = MSHeatPump( MSHeatPumpNum ).HeatCoilType;
			HeatCoilName = MSHeatPump( MSHeatPumpNum ).HeatCoilName;
			HeatCoilNum = MSHeatPump( MSHeatPumpNum ).HeatCoilNum;
			MaxCoilFluidFlow = MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow;
			CoilControlNode = MSHeatPump( MSHeatPumpNum ).CoilControlNode;
			CoilOutletNode = MSHeatPump( MSHeatPumpNum ).CoilOutletNode;
			LoopNum = MSHeatPump( MSHeatPumpNum ).LoopNum;
			LoopSide = MSHeatPump( MSHeatPumpNum ).LoopSide;
			BranchNum = MSHeatPump( MSHeatPumpNum ).BranchNum;
			CompNum = MSHeatPump( MSHeatPumpNum ).CompNum;
		} else {
			HeatCoilType = MSHeatPump( MSHeatPumpNum ).SuppHeatCoilType;
			HeatCoilName = MSHeatPump( MSHeatPumpNum ).SuppHeatCoilName;
			HeatCoilNum = MSHeatPump( MSHeatPumpNum ).SuppHeatCoilNum;
			MaxCoilFluidFlow = MSHeatPump( MSHeatPumpNum ).MaxSuppCoilFluidFlow;
			CoilControlNode = MSHeatPump( MSHeatPumpNum ).SuppCoilControlNode;
			CoilOutletNode = MSHeatPump( MSHeatPumpNum ).SuppCoilOutletNode;
			LoopNum = MSHeatPump( MSHeatPumpNum ).SuppLoopNum;
			LoopSide = MSHeatPump( MSHeatPumpNum ).SuppLoopSide;
			BranchNum = MSHeatPump( MSHeatPumpNum ).SuppBranchNum;
			CompNum = MSHeatPump( MSHeatPumpNum ).SuppCompNum;
		}

		MSHeatPump( MSHeatPumpNum ).HotWaterLoopNum = LoopNum;
		MSHeatPump( MSHeatPumpNum ).HotWaterLoopSide = LoopSide;
		MSHeatPump( MSHeatPumpNum ).HotWaterBranchNum = BranchNum;
		MSHeatPump( MSHeatPumpNum ).HotWaterCompNum = CompNum;
		MSHeatPump( MSHeatPumpNum ).HotWaterCoilControlNode = CoilControlNode;
		MSHeatPump( MSHeatPumpNum ).HotWaterCoilOutletNode = CoilOutletNode;
		MSHeatPump( MSHeatPumpNum ).HotWaterCoilName = HeatCoilName;
		MSHeatPump( MSHeatPumpNum ).HotWaterCoilNum = HeatCoilNum;

		if ( HeatingLoad > SmallLoad ) {

			{ auto const SELECT_CASE_var( HeatCoilType );
			if ( ( SELECT_CASE_var == SuppHeatingCoilGas ) || ( SELECT_CASE_var == SuppHeatingCoilElec ) ) {
				SimulateHeatingCoilComponents( HeatCoilName, FirstHVACIteration, HeatingLoad, HeatCoilNum, QCoilActual, true, FanMode );
			} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
				if ( present( PartLoadFrac ) ) {
					MaxHotWaterFlow = MaxCoilFluidFlow * PartLoadFrac;
					SetComponentFlowRate( MaxHotWaterFlow, CoilControlNode, CoilOutletNode, LoopNum, LoopSide, BranchNum, CompNum );
					SimulateWaterCoilComponents( HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode );
				} else {
					MaxHotWaterFlow = MaxCoilFluidFlow;
					SetComponentFlowRate( MaxHotWaterFlow, CoilControlNode, CoilOutletNode, LoopNum, LoopSide, BranchNum, CompNum );
					SimulateWaterCoilComponents( HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode );
					if ( QCoilActual > ( HeatingLoad + SmallLoad ) ) {
						// control water flow to obtain output matching HeatingLoad
						SolFlag = 0;
						MinWaterFlow = 0.0;
						Par( 1 ) = double( MSHeatPumpNum );
						if ( FirstHVACIteration ) {
							Par( 2 ) = 1.0;
						} else {
							Par( 2 ) = 0.0;
						}
						Par( 3 ) = HeatingLoad;
						SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par );
						if ( SolFlag == -1 ) {
							if ( MSHeatPump( MSHeatPumpNum ).HotWaterCoilMaxIterIndex == 0 ) {
								ShowWarningMessage( "CalcNonDXHeatingCoils: Hot water coil control failed for " + CurrentModuleObject + "=\"" + MSHeatPump( MSHeatPumpNum ).Name + "\"" );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "  Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating hot water mass flow rate" );
							}
							ShowRecurringWarningErrorAtEnd( "CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + CurrentModuleObject + "=\"" + MSHeatPump( MSHeatPumpNum ).Name, MSHeatPump( MSHeatPumpNum ).HotWaterCoilMaxIterIndex );
						} else if ( SolFlag == -2 ) {
							if ( MSHeatPump( MSHeatPumpNum ).HotWaterCoilMaxIterIndex2 == 0 ) {
								ShowWarningMessage( "CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for " + CurrentModuleObject + "=\"" + MSHeatPump( MSHeatPumpNum ).Name + "\"" );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "...Bad hot water maximum flow rate limits" );
								ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinWaterFlow, 3 ) + " kg/s" );
								ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxHotWaterFlow, 3 ) + " kg/s" );
							}
							ShowRecurringWarningErrorAtEnd( "CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for " + CurrentModuleObject + "=\"" + MSHeatPump( MSHeatPumpNum ).Name + "\"", MSHeatPump( MSHeatPumpNum ).HotWaterCoilMaxIterIndex2, MaxHotWaterFlow, MinWaterFlow, _, "[kg/s]", "[kg/s]" );

						}
						// simulate hot water supplemental heating coil
						SimulateWaterCoilComponents( HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode );
					}
				}
			} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
				if ( present( PartLoadFrac ) ) {
					mdot = MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow * PartLoadFrac;
					SteamCoilHeatingLoad = HeatingLoad * PartLoadFrac;
				} else {
					mdot = MSHeatPump( MSHeatPumpNum ).MaxCoilFluidFlow;
					SteamCoilHeatingLoad = HeatingLoad;
				}
				SetComponentFlowRate( mdot, CoilControlNode, CoilOutletNode, LoopNum, LoopSide, BranchNum, CompNum );
				// simulate steam supplemental heating coil
				SimulateSteamCoilComponents( HeatCoilName, FirstHVACIteration, HeatCoilNum, SteamCoilHeatingLoad, QCoilActual, FanMode );
			}}

		} else { // end of IF (HeatingLoad > SmallLoad) THEN

			{ auto const SELECT_CASE_var( HeatCoilType );
			if ( ( SELECT_CASE_var == SuppHeatingCoilGas ) || ( SELECT_CASE_var == SuppHeatingCoilElec ) ) {
				SimulateHeatingCoilComponents( HeatCoilName, FirstHVACIteration, HeatingLoad, HeatCoilNum, QCoilActual, true, FanMode );
			} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, CoilControlNode, CoilOutletNode, LoopNum, LoopSide, BranchNum, CompNum );
				SimulateWaterCoilComponents( HeatCoilName, FirstHVACIteration, HeatCoilNum, QCoilActual, FanMode );
			} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, CoilControlNode, CoilOutletNode, LoopNum, LoopSide, BranchNum, CompNum );
				// simulate the steam supplemental heating coil
				SimulateSteamCoilComponents( HeatCoilName, FirstHVACIteration, HeatCoilNum, HeatingLoad, QCoilActual, FanMode );
			}}
		}
		HeatCoilLoadmet = QCoilActual;

	}

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   November 2011
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (QCoilActual - SupHeatCoilLoad) / SupHeatCoilLoad
		// coil actual output depends on the hot water flow rate which is varied to minimize the
		// residual.

		// METHODOLOGY EMPLOYED:
		// Calls HotWaterCoilResidual, and calculates the residual as defined above.

		// REFERENCES:

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;
		using PlantUtilities::SetComponentFlowRate;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int MSHeatPumpNum;
		bool FirstHVACSoln;
		Real64 QCoilActual; // delivered coild load, W
		Real64 HeatCoilLoad; // requested coild load, W
		Real64 mdot;

		MSHeatPumpNum = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		HeatCoilLoad = Par( 3 );
		QCoilActual = HeatCoilLoad;
		mdot = HWFlow;
		SetComponentFlowRate( mdot, MSHeatPump( MSHeatPumpNum ).HotWaterCoilControlNode, MSHeatPump( MSHeatPumpNum ).HotWaterCoilOutletNode, MSHeatPump( MSHeatPumpNum ).HotWaterLoopNum, MSHeatPump( MSHeatPumpNum ).HotWaterLoopSide, MSHeatPump( MSHeatPumpNum ).HotWaterBranchNum, MSHeatPump( MSHeatPumpNum ).HotWaterCompNum );
		// simulate the hot water supplemental heating coil
		SimulateWaterCoilComponents( MSHeatPump( MSHeatPumpNum ).HotWaterCoilName, FirstHVACSoln, MSHeatPump( MSHeatPumpNum ).HotWaterCoilNum, QCoilActual, MSHeatPump( MSHeatPumpNum ).OpMode );
		if ( HeatCoilLoad != 0.0 ) {
			Residuum = ( QCoilActual - HeatCoilLoad ) / HeatCoilLoad;
		} else { //Autodesk:Return Condition added to assure return value is set
			Residuum = 0.0;
		}
		return Residuum;
	}

} // HVACMultiSpeedHeatPump

} // EnergyPlus
