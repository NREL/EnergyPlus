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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <PackagedTerminalHeatPump.hh>
#include <BranchNodeConnections.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DXCoils.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SingleDuct.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>
#include <WaterToAirHeatPump.hh>
#include <WaterToAirHeatPumpSimple.hh>

namespace EnergyPlus {

namespace PackagedTerminalHeatPump {

	// Module containing the routines for modeling packaged terminal air conditioners and heat pumps

	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad
	//       DATE WRITTEN   July 2005
	//       MODIFIED       B. Griffith Dec. 2006 added Function call for OA node and moved get input flag up to Module
	//                      B. Griffith, Sept 2010, plant upgrades, fluid properties, for heating coils
	//                      B. Nigusse, Jan 2012, added hot water and steam heating coils to PTHP and WSHP
	//                      Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate packaged
	// terminal units, which are considered "Zone Equipment" in EnergyPlus

	// METHODOLOGY EMPLOYED:
	// Units are modeled as a collection of components: outside air mixer, supply air fan, DX cooling coils,
	// DX heating coil (PTHP) or gas/elec/water/steam heating coil (PTAC), and supplemental heater if necessary.
	// Control is by means of cycling: either continuous air flow with the DX compressor
	// cycling on/off or the entire unit - fan and compressor cycling on/off. Cycling behavior
	// is not explicitly modeled - instead cycling inefficiencies must be included in
	// the efficiency curves of the DX coil module.

	// REFERENCES: None

	// OTHER NOTES: None

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataSizing;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::SecInHour;
	using DataGlobals::InitConvTemp;
	using DataGlobals::NumOfZones;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using namespace DataHVACGlobals;
	using DXCoils::DXCoilPartLoadRatio;
	using VariableSpeedCoils::MaxSpedLevels;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	// Last mode of operation
	int const CoolingMode( 1 ); // last compressor operating mode was in cooling
	int const HeatingMode( 2 ); // last compressor operating mode was in heating

	// Airflow control for contant fan mode
	int const UseCompressorOnFlow( 1 ); // set compressor OFF air flow rate equal to compressor ON air flow rate
	int const UseCompressorOffFlow( 2 ); // set compressor OFF air flow rate equal to user defined value

	// Unit type
	int const PTHPUnit( 1 ); // equivalent to PackagedTerminal:HeatPump:AirToAir
	int const PTACUnit( 2 ); // equivalent to PackagedTerminal:AirConditioner
	int const PTWSHPUnit( 3 ); // equivalent to WaterToAirHeatPump

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	Array1D_bool CheckEquipName;

	Real64 SupHeaterLoad( 0.0 ); // load to be met by supplemental heater [W]
	int NumPTHP( 0 ); // total number of PTHP's
	int NumPTAC( 0 ); // total number of PTAC's
	int NumPTWSHP( 0 ); // total number of PTWSHP's
	int NumPTUs( 0 ); // total number of PTHP and PTAC units
	Real64 CompOnMassFlow( 0.0 ); // Supply air mass flow rate w/ compressor ON
	Real64 OACompOnMassFlow( 0.0 ); // OA mass flow rate w/ compressor ON
	Real64 CompOffMassFlow( 0.0 ); // Supply air mass flow rate w/ compressor OFF
	Real64 OACompOffMassFlow( 0.0 ); // OA mass flow rate w/ compressor OFF
	Real64 CompOnFlowRatio( 0.0 ); // fan flow ratio when coil on
	Real64 CompOffFlowRatio( 0.0 ); // fan flow ratio when coil off
	Real64 FanSpeedRatio( 0.0 ); // ratio of air flow ratio passed to fan object
	bool GetPTUnitInputFlag( true ); // First time, input is "gotten"
	Real64 SaveCompressorPLR( 0.0 ); // holds compressor PLR from active DX coil
	Real64 SteamDensity( 0.0 ); // density of steam at 100C, used for steam heating coils
	bool HeatingLoad( false ); // defines a heating load on PTUnit
	bool CoolingLoad( false ); // defines a cooling load on PTUnit
	Real64 MinWaterFlow( 0.0 ); // minimum water flow for heating [kg/s]
	Real64 TempSteamIn( 100.0 ); // steam coil steam inlet temperature

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// modules for variable speed heat pump

	// Object Data
	Array1D< PTUnitData > PTUnit;
	Array1D< PTUnitNumericFieldData > PTUnitUNumericFields; // holds VRF TU numeric input fields character field name

	// Functions

	void
	SimPackagedTerminalUnit(
		std::string const & CompName, // name of the packaged terminal heat pump
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QUnitOut, // sensible capacity delivered to zone
		Real64 & LatOutputProvided, // Latent add/removal by packaged terminal unit (kg/s), dehumid = negative
		int const PTUnitType, // indicates whether PTAC, PTHP or PTWSHP
		int & CompIndex // index to Packaged Terminal Heat Pump
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       D. Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a packaged terminal heat pump. Called from SimZoneEquipment.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;
		using namespace DataZoneEnergyDemands;
		using DataHeatBalFanSys::TempControlType;
		using DataZoneEquipment::PkgTermHPAirToAir_Num;
		using DataZoneEquipment::PkgTermHPWaterToAir_Num;
		using DataZoneEquipment::PkgTermACAirToAir_Num;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum( 0 ); // index of packaged terminal heat pump being simulated

		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 QZnReq; // load to be met by zone equipment
		Real64 RemainingOutputToHeatingSP; // remaining load to heating setpoint
		Real64 RemainingOutputToCoolingSP; // remaining load to cooling setpoint

		// FLOW

		// First time SimPackagedTerminalHeatPump is called, get the input for all the PTUnits
		if ( GetPTUnitInputFlag ) {
			GetPTUnit();
			GetPTUnitInputFlag = false;
		}

		// Find the correct packaged terminal heat pump
		if ( CompIndex == 0 ) {
			PTUnitNum = FindItemInList( CompName, PTUnit );
			if ( PTUnitNum == 0 ) {
				ShowFatalError( "SimPackagedTerminalUnit: Unit not found=" + CompName );
			}
			CompIndex = PTUnit( PTUnitNum ).PTObjectIndex;
		} else {
			{ auto const SELECT_CASE_var( PTUnitType );
			if ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) {
				PTUnitNum = CompIndex;
			} else if ( SELECT_CASE_var == PkgTermACAirToAir_Num ) {
				PTUnitNum = CompIndex + NumPTHP;
			} else if ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) {
				PTUnitNum = CompIndex + NumPTHP + NumPTAC;
			} else {
				assert( false );
			}}
			if ( PTUnitNum > NumPTUs || PTUnitNum < 1 ) {
				ShowFatalError( "SimPackagedTerminalUnit:  Invalid CompIndex passed=" + TrimSigDigits( PTUnitNum ) + ", Number of Units=" + TrimSigDigits( NumPTUs ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( PTUnitNum ) ) {
				if ( CompName != PTUnit( PTUnitNum ).Name ) {
					ShowFatalError( "SimPackagedTerminalUnit: Invalid CompIndex passed=" + TrimSigDigits( PTUnitNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + PTUnit( PTUnitNum ).Name );
				}
				CheckEquipName( PTUnitNum ) = false;
			}
		}

		OnOffAirFlowRatio = 0.0;

		RemainingOutputToHeatingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		RemainingOutputToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;

		if ( RemainingOutputToCoolingSP < 0.0 && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
			QZnReq = RemainingOutputToCoolingSP;
		} else if ( RemainingOutputToHeatingSP > 0.0 && TempControlType( ZoneNum ) != SingleCoolingSetPoint ) {
			QZnReq = RemainingOutputToHeatingSP;
		} else {
			QZnReq = 0.0;
		}

		ZoneEqDXCoil = true;

		// Initialize the packaged terminal heat pump
		InitPTUnit( PTUnitNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq );

		SimPTUnit( PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided );

		// Report the result of the simulation
		ReportPTUnit( PTUnitNum );

		ZoneEqDXCoil = false;

	}

	void
	SimPTUnit(
		int const PTUnitNum, // number of the current Packaged Terminal Heat Pump being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QSensUnitOut, // sensible delivered capacity [W]
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 const QZnReq, // cooling/heating needed by zone [W]
		Real64 & QLatUnitOut // Latent delivered capacity [kg/s], dehumidification = negative
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       D. Shirey, Aug 2009 (QLatUnitOut)
		//       MODIFIED       Bo Shen, March 2012, added switch to variable-speed water-source heat pump
		//       MODIFIED       Bo Shen, July 2012, added variable-speed air-source heat pump
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a packaged terminal heat pump; adjust its output to match the
		// remaining zone load.

		// METHODOLOGY EMPLOYED:
		// Calls ControlPTUnitOutput to obtain the desired unit output

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PartLoadFrac; // compressor part load fraction
		bool UnitOn; // TRUE if unit is on
		int OutletNode; // PTUnit air outlet node
		int InletNode; // PTUnit air inlet node
		Real64 QTotUnitOut; // total delivered capacity [W]
		Real64 AirMassFlow; // air mass flow rate [kg/s]
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		int OpMode; // operating mode (fan cycling or continious; DX coil always cycles)
		bool HXUnitOn; // flag to enable heat exchanger
		Real64 QLatReq; // latent cooling output needed by zone [W], now is zero
		Real64 QSensUnitOutMul; // sensible output for the variable speed HP
		Real64 QLatUnitOutMul; // latent output for the variable speed HP
		Real64 MinHumRat; // min humidity for calculating sensible capacity of VS WSHP
		Real64 QSensUnitOutNoATM; // sensible unit output excluding air added by supply side air terminal mixer

		// zero the fan, DX coils, and supplemental electric heater electricity consumption
		FanElecPower = 0.0;
		DXElecCoolingPower = 0.0;
		DXElecHeatingPower = 0.0;
		ElecHeatingCoilPower = 0.0;
		SaveCompressorPLR = 0.0;
		QLatReq = 0.0;

		// initialize local variables
		UnitOn = true;
		HXUnitOn = true;
		QSensUnitOut = 0.0;
		QLatUnitOut = 0.0;
		OutletNode = PTUnit( PTUnitNum ).AirOutNode;
		InletNode = PTUnit( PTUnitNum ).AirInNode;
		AirMassFlow = Node( InletNode ).MassFlowRate;
		OpMode = PTUnit( PTUnitNum ).OpMode;

		// reset operation flag if unit is off
		if ( PTUnit( PTUnitNum ).OpMode == CycFanCycCoil ) {
			// cycling unit: only runs if there is a cooling or heating load.
			if ( ( ! CoolingLoad && ! HeatingLoad ) || AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
			}
		} else if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil ) {
			// continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
			if ( AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
			}
		}

		OnOffFanPartLoadFraction = 1.0;

		if ( UnitOn ) {
			if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
				SimVariableSpeedHP( PTUnitNum, ZoneNum, FirstHVACIteration, QZnReq, QLatReq, OnOffAirFlowRatio, OpMode, HXUnitOn );
			} else {
				ControlPTUnitOutput( PTUnitNum, FirstHVACIteration, OpMode, QZnReq, ZoneNum, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
			}
		} else {
			PartLoadFrac = 0.0;
			OnOffAirFlowRatio = 1.0;
			SupHeaterLoad = 0.0;
			if ( PTUnit( PTUnitNum ).NumOfSpeedCooling > 0 ) {
				CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, 0, 1, 0.0, PartLoadFrac, QSensUnitOutMul, QLatUnitOutMul, 0.0, 0.0, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
			}
		}

		// calculate delivered capacity
		AirMassFlow = Node( InletNode ).MassFlowRate;

		if ( ! PTUnit( PTUnitNum ).useVSCoilModel ) {
			CalcPTUnit( PTUnitNum, FirstHVACIteration, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
		} else {
			// calculate delivered capacity
			MinHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );
			QSensUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );
		}

		// CR9155 Remove specific humidity calculations
		SpecHumOut = Node( OutletNode ).HumRat;
		SpecHumIn = Node( InletNode ).HumRat;
		QLatUnitOut = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate, kg/s (dehumid = negative)
		QSensUnitOutNoATM = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );

		if ( ! PTUnit( PTUnitNum ).useVSCoilModel ) {
			// report variables
			if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
				PTUnit( PTUnitNum ).CompPartLoadRatio = PartLoadFrac;
			} else {
				PTUnit( PTUnitNum ).CompPartLoadRatio = SaveCompressorPLR;
			}

			if ( PTUnit( PTUnitNum ).OpMode == CycFanCycCoil ) {
				PTUnit( PTUnitNum ).FanPartLoadRatio = PartLoadFrac;
			} else {
				if ( UnitOn ) {
					PTUnit( PTUnitNum ).FanPartLoadRatio = 1.0;
				} else {
					PTUnit( PTUnitNum ).FanPartLoadRatio = 0.0;
				}
			}
		}

		PTUnit( PTUnitNum ).TotCoolEnergyRate = std::abs( min( 0.0, QTotUnitOut ) );
		PTUnit( PTUnitNum ).TotHeatEnergyRate = std::abs( max( 0.0, QTotUnitOut ) );
		PTUnit( PTUnitNum ).SensCoolEnergyRate = std::abs( min( 0.0, QSensUnitOutNoATM ) );
		PTUnit( PTUnitNum ).SensHeatEnergyRate = std::abs( max( 0.0, QSensUnitOutNoATM ) );
		PTUnit( PTUnitNum ).LatCoolEnergyRate = std::abs( min( 0.0, ( QTotUnitOut - QSensUnitOutNoATM ) ) );
		PTUnit( PTUnitNum ).LatHeatEnergyRate = std::abs( max( 0.0, ( QTotUnitOut - QSensUnitOutNoATM ) ) );

		if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
			{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).ACHeatCoilType_Num );
			if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
				PTUnit( PTUnitNum ).ElecPower = FanElecPower + DXElecCoolingPower + ElecHeatingCoilPower;
			} else if ( ( SELECT_CASE_var == Coil_HeatingWater ) || ( SELECT_CASE_var == Coil_HeatingSteam ) ) {
				PTUnit( PTUnitNum ).ElecPower = FanElecPower + DXElecCoolingPower;
			} else {
			}}
		} else {
			PTUnit( PTUnitNum ).ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + ElecHeatingCoilPower;
		}

	}

	void
	GetPTUnit()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
		//                      Bereket Nigusse, FSEC, April 2011: added OA Mixer object type
		//       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
		//       MODIFIED       Bo Shen, July 2012, added variable-speed air-source heat pump
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for packaged terminal units and stores it in PTUnit data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::GetFanType;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using Fans::GetFanAvailSchPtr;
		using MixedAir::GetOAMixerNodeNumbers;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DXCoils::GetDXCoilIndex;
		auto & GetDXCoilInletNode( DXCoils::GetCoilInletNode );
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		using DXCoils::GetCoilCondenserInletNode;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		auto & GetHXDXCoilInletNode( HVACHXAssistedCoolingCoil::GetCoilInletNode );
		auto & GetHXDXCoilOutletNode( HVACHXAssistedCoolingCoil::GetCoilOutletNode );
		auto & GetHeatingCoilIndex( HeatingCoils::GetCoilIndex );
		using HeatingCoils::SimulateHeatingCoilComponents;
		auto & GetHeatingCoilInletNode( HeatingCoils::GetCoilInletNode );
		auto & GetHeatingCoilOutletNode( HeatingCoils::GetCoilOutletNode );
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );
		using HeatingCoils::GetHeatingCoilTypeNum;
		auto & GetSteamCoilAirInletNode( SteamCoils::GetCoilAirInletNode );
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilAirOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using SteamCoils::GetTypeOfCoil;
		using SteamCoils::ZoneLoadControl;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		auto & GetWaterCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWaterCoilOutletNode( WaterCoils::GetCoilOutletNode );
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::FindItemInList;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using FluidProperties::GetSatDensityRefrig;
		auto & GetWtoAHPCoilCapacity( WaterToAirHeatPump::GetCoilCapacity );
		auto & GetWtoAHPSimpleCoilCapacity( WaterToAirHeatPumpSimple::GetCoilCapacity );
		auto & GetWtoAHPSimpleCoilInletNode( WaterToAirHeatPumpSimple::GetCoilInletNode );
		auto & GetWtoAHPSimpleCoilOutletNode( WaterToAirHeatPumpSimple::GetCoilOutletNode );
		auto & GetWtoAHPSimpleCoilIndex( WaterToAirHeatPumpSimple::GetCoilIndex );
		using WaterToAirHeatPumpSimple::SetSimpleWSHPData;
		using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
		using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
		using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;
		using VariableSpeedCoils::GetCoilIndexVariableSpeed;
		using VariableSpeedCoils::SetVarSpeedCoilData;
		using VariableSpeedCoils::GetVSCoilCondenserInletNode;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::PkgTermHPAirToAir_Num;
		using DataZoneEquipment::PkgTermHPWaterToAir_Num;
		using DataZoneEquipment::PkgTermACAirToAir_Num;
		using DataHVACGlobals::WaterCycling;
		using DataHVACGlobals::WaterConstant;
		using DataHVACGlobals::WaterConstantOnDemand;
		using SingleDuct::GetATMixer;
		using DataSizing::ZoneHVACSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetPTUnit: " ); // include trailing blank space
		static std::string const RoutineNameFull( "GetPackagedTerminalHeatPumpInput" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PTUnitIndex; // loop index
		int PTUnitNum; // current packaged terminal unit number
		Array1D_string Alphas; // Alpha items for object
		Array1D< Real64 > Numbers; // Numeric items for object
		Array1D_int OANodeNums( 4 ); // Node numbers of OA mixer (OA, EA, RA, MA)
		int FanInletNodeNum; // Fan inlet node number
		int FanOutletNodeNum; // Fan outlet node number
		int SuppHeatInletNodeNum; // Supplemental heating coil inlet node number
		int SuppHeatOutletNodeNum; // Supplemental heating coil outlet node number
		int CoolCoilInletNodeNum; // cooling coil inlet node number
		int CoolCoilOutletNodeNum; // cooling coil outlet node number
		std::string ACHeatCoilName; // name of heating coil
		int HeatCoilInletNodeNum; // heating coil inlet node number
		int HeatCoilOutletNodeNum; // heating coil outlet node number
		int SuppHeatHWInletNodeNum; // Supplemental heating coil Hot Water inlet node number
		int SuppHeatHWOutletNodeNum; // Supplemental heating coil Hot Water outlet node number
		std::string CompSetFanInlet;
		std::string CompSetCoolInlet;
		std::string CompSetFanOutlet;
		std::string CompSetCoolOutlet;
		std::string CompSetHeatInlet;
		std::string CompSetHeatOutlet;
		std::string CompSetSupHeatInlet;
		std::string CompSetSupHeatOutlet;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int MaxAlphas; // Maximum number of alpha fields in all objects
		int MaxNumbers; // Maximum number of numeric fields in all objects
		int NumFields; // Total number of fields in object
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // Object type for getting and error messages
		static bool errFlag( false ); // Error flag returned during CALL to mining functions
		Real64 FanVolFlow; // maximum supply air volumetric flow rate of fan
		int TempNodeNum; // dummy variable to set up HW coil water inlet node
		int SteamIndex; // dummy variable to set up steam coil steam inlet density
		std::string SuppHeatCoilType; // type of supplemental heating coil
		std::string SuppHeatCoilName; // name of supplemental heating coil
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter
		bool ZoneNodeNotFound; // used in error checking

		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		MaxNumbers = 0;
		MaxAlphas = 0;

		// find the number of each type of packaged terminal unit
		CurrentModuleObject = "ZoneHVAC:PackagedTerminalHeatPump";
		NumPTHP = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		CurrentModuleObject = "ZoneHVAC:PackagedTerminalAirConditioner";
		NumPTAC = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		CurrentModuleObject = "ZoneHVAC:WaterToAirHeatPump";
		NumPTWSHP = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		Numbers.dimension( MaxNumbers, 0.0 );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNumbers );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNumbers, true );
		NumPTUs = NumPTHP + NumPTAC + NumPTWSHP;

		// allocate the data structures
		if ( NumPTUs > 0 ) {
			PTUnit.allocate( NumPTUs );
			CheckEquipName.allocate( NumPTUs );
			PTUnitUNumericFields.allocate( NumPTUs );
		}
		CheckEquipName = true;

		// loop over PTHP units; get and load the input data
		for ( PTUnitIndex = 1; PTUnitIndex <= NumPTHP; ++PTUnitIndex ) {

			FanInletNodeNum = 0;
			FanOutletNodeNum = 0;
			SuppHeatInletNodeNum = 0;
			SuppHeatOutletNodeNum = 0;
			CoolCoilInletNodeNum = 0;
			CoolCoilOutletNodeNum = 0;
			HeatCoilInletNodeNum = 0;
			HeatCoilOutletNodeNum = 0;
			SuppHeatHWInletNodeNum = 0;
			SuppHeatHWOutletNodeNum = 0;
			OANodeNums = 0;

			CurrentModuleObject = "ZoneHVAC:PackagedTerminalHeatPump";
			GetObjectItem( CurrentModuleObject, PTUnitIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			PTUnitNum = PTUnitIndex;

			PTUnitUNumericFields( PTUnitNum ).FieldNames.allocate( NumNumbers );
			PTUnitUNumericFields( PTUnitNum ).FieldNames = "";
			PTUnitUNumericFields( PTUnitNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PTUnit, PTUnitNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			PTUnit( PTUnitNum ).PTObjectIndex = PTUnitIndex;
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			PTUnit( PTUnitNum ).Name = Alphas( 1 );
			PTUnit( PTUnitNum ).UnitType = CurrentModuleObject;
			PTUnit( PTUnitNum ).UnitType_Num = PTHPUnit;
			PTUnit( PTUnitNum ).ZoneEquipType = PkgTermHPAirToAir_Num;
			if ( lAlphaBlanks( 2 ) ) {
				PTUnit( PTUnitNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				PTUnit( PTUnitNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer (index number)
				if ( PTUnit( PTUnitNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\" invalid data." );
					ShowContinueError( "invalid-not found " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			PTUnit( PTUnitNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			PTUnit( PTUnitNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			PTUnit( PTUnitNum ).OAMixType = Alphas( 5 );
			PTUnit( PTUnitNum ).OAMixName = Alphas( 6 );

			errFlag = false;
			ValidateComponent( PTUnit( PTUnitNum ).OAMixType, PTUnit( PTUnitNum ).OAMixName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				// OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
				OANodeNums = GetOAMixerNodeNumbers( PTUnit( PTUnitNum ).OAMixName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "that was specified in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ShowContinueError( "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name." );
					ErrorsFound = true;
				} else {
					//  Set connection type to 'Inlet', because this is not necessarily directly come from
					//  outside air.  Outside Air Inlet Node List will set the connection to outside air
					PTUnit( PTUnitNum ).OutsideAirNode = OANodeNums( 1 );
					PTUnit( PTUnitNum ).AirReliefNode = OANodeNums( 2 );
				}
			}

			PTUnit( PTUnitNum ).MaxCoolAirVolFlow = Numbers( 1 );
			if ( PTUnit( PTUnitNum ).MaxCoolAirVolFlow <= 0 && PTUnit( PTUnitNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 1 ) + " = " + TrimSigDigits( Numbers( 1 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxHeatAirVolFlow = Numbers( 2 );
			if ( PTUnit( PTUnitNum ).MaxHeatAirVolFlow <= 0 && PTUnit( PTUnitNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 2 ) + " = " + TrimSigDigits( Numbers( 2 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow = Numbers( 3 );
			if ( PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow < 0 && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 3 ) + " = " + TrimSigDigits( Numbers( 3 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).CoolOutAirVolFlow = Numbers( 4 );
			if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow < 0 && PTUnit( PTUnitNum ).CoolOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 4 ) + " = " + TrimSigDigits( Numbers( 4 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			//   only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
			if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow > PTUnit( PTUnitNum ).MaxCoolAirVolFlow && PTUnit( PTUnitNum ).CoolOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + ' ' + cNumericFields( 4 ) + " cannot be greater than " + cNumericFields( 1 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).HeatOutAirVolFlow = Numbers( 5 );
			if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow < 0 && PTUnit( PTUnitNum ).HeatOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 5 ) + " = " + TrimSigDigits( Numbers( 5 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			//   only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
			if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow > PTUnit( PTUnitNum ).MaxHeatAirVolFlow && PTUnit( PTUnitNum ).HeatOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + ' ' + cNumericFields( 5 ) + " cannot be greater than " + cNumericFields( 2 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow = Numbers( 6 );
			if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow < 0 && PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 6 ) + " = " + TrimSigDigits( Numbers( 6 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}
			//   only check that SA flow when compressor is OFF is >= OA flow when compressor is OFF after fan mode is read in

			PTUnit( PTUnitNum ).FanType = Alphas( 7 );
			PTUnit( PTUnitNum ).FanName = Alphas( 8 );
			errFlag = false;
			GetFanType( PTUnit( PTUnitNum ).FanName, PTUnit( PTUnitNum ).FanType_Num, errFlag, CurrentModuleObject, PTUnit( PTUnitNum ).Name );
			FanVolFlow = 0.0;
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			} else {
				GetFanIndex( PTUnit( PTUnitNum ).FanName, PTUnit( PTUnitNum ).FanIndex, errFlag, CurrentModuleObject );
				FanInletNodeNum = GetFanInletNode( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
				FanOutletNodeNum = GetFanOutletNode( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
				GetFanVolFlow( PTUnit( PTUnitNum ).FanIndex, FanVolFlow );
				PTUnit( PTUnitNum ).ActualFanVolFlowRate = FanVolFlow;
				// Get the fan's availability schedule
				PTUnit( PTUnitNum ).FanAvailSchedPtr = GetFanAvailSchPtr( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ErrorsFound = true;
				}
			}

			if ( FanVolFlow != AutoSize ) {
				if ( FanVolFlow < max( PTUnit( PTUnitNum ).MaxCoolAirVolFlow, PTUnit( PTUnitNum ).MaxHeatAirVolFlow, PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\", invalid air flow rate" );
					ShowContinueError( "air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in fan object " + PTUnit( PTUnitNum ).FanName + " is less than the maximum PTHP supply air flow rate." );
					ShowContinueError( " The fan flow rate must be greater than the PTHP maximum supply air flow rate." );
					ErrorsFound = true;
				}
			}

			PTUnit( PTUnitNum ).DXHeatCoilName = Alphas( 10 );
			if ( SameString( Alphas( 9 ), "Coil:Heating:DX:SingleSpeed" ) ) {
				PTUnit( PTUnitNum ).DXHeatCoilType = Alphas( 9 );
				//       PTUnit(PTUnitNum)%DXHeatCoilType_Num = CoilDX_HeatingEmpirical
				errFlag = false;
				GetDXCoilIndex( PTUnit( PTUnitNum ).DXHeatCoilName, PTUnit( PTUnitNum ).DXHeatCoilIndexNum, errFlag, PTUnit( PTUnitNum ).DXHeatCoilType );
				HeatCoilInletNodeNum = GetDXCoilInletNode( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				HeatCoilOutletNodeNum = GetDXCoilOutletNode( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				if ( errFlag ) ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
			} else if ( SameString( Alphas( 9 ), "COIL:HEATING:DX:VARIABLESPEED" ) ) {
				PTUnit( PTUnitNum ).DXHeatCoilType = Alphas( 9 );
				PTUnit( PTUnitNum ).DXHeatCoilType_Num = Coil_HeatingAirToAirVariableSpeed;
				PTUnit( PTUnitNum ).DXHeatCoilName = Alphas( 10 );
				ValidateComponent( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXHeatCoilIndex = GetCoilIndexVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					HeatCoilInletNodeNum = GetCoilInletNodeVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
					HeatCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				}
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\", invalid field" );
				ShowContinueError( " illegal " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).HeatConvergenceTol = Numbers( 7 );
			PTUnit( PTUnitNum ).MinOATCompressor = Numbers( 8 );
			PTUnit( PTUnitNum ).DXCoolCoilName = Alphas( 12 );
			PTUnit( PTUnitNum ).CoolConvergenceTol = Numbers( 9 );

			if ( SameString( Alphas( 11 ), "Coil:Cooling:DX:SingleSpeed" ) || SameString( Alphas( 11 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
				PTUnit( PTUnitNum ).DXCoolCoilType = Alphas( 11 );
				if ( SameString( Alphas( 11 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
					PTUnit( PTUnitNum ).DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed;
					errFlag = false;
					GetDXCoilIndex( PTUnit( PTUnitNum ).DXCoolCoilName, PTUnit( PTUnitNum ).DXCoolCoilIndexNum, errFlag, PTUnit( PTUnitNum ).DXCoolCoilType );
					CoolCoilInletNodeNum = GetDXCoilInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetDXCoilOutletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					PTUnit( PTUnitNum ).CondenserNodeNum = GetCoilCondenserInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					if ( errFlag ) ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
				} else if ( SameString( Alphas( 11 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
					PTUnit( PTUnitNum ).DXCoolCoilType_Num = CoilDX_CoolingHXAssisted;
					errFlag = false;
					GetDXCoilIndex( GetHXDXCoilName( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag ), PTUnit( PTUnitNum ).DXCoolCoilIndexNum, errFlag, "Coil:Cooling:DX:SingleSpeed" );
					CoolCoilInletNodeNum = GetHXDXCoilInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetHXDXCoilOutletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					PTUnit( PTUnitNum ).CondenserNodeNum = GetCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed", GetHXDXCoilName( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag ), errFlag );
					if ( errFlag ) ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
				}
			} else if ( SameString( Alphas( 11 ), "COIL:COOLING:DX:VARIABLESPEED" ) ) {
				PTUnit( PTUnitNum ).DXCoolCoilType = Alphas( 11 );
				PTUnit( PTUnitNum ).DXCoolCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
				PTUnit( PTUnitNum ).DXCoolCoilName = Alphas( 12 );
				ValidateComponent( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXCoolCoilIndexNum = GetCoilIndexVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					CoolCoilInletNodeNum = GetCoilInletNodeVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					PTUnit( PTUnitNum ).CondenserNodeNum = GetVSCoilCondenserInletNode( PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );

					if ( errFlag ) ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );

				}
			} else {
				ShowWarningError( CurrentModuleObject + " illegal " + cAlphaFields( 11 ) + " = " + Alphas( 11 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			if ( Alphas( 9 ) == "COIL:HEATING:DX:VARIABLESPEED" && Alphas( 11 ) == "COIL:COOLING:DX:VARIABLESPEED" ) {
				if ( PTUnit( PTUnitNum ).DXHeatCoilIndex > 0 && PTUnit( PTUnitNum ).DXCoolCoilIndexNum > 0 ) {
					SetVarSpeedCoilData( PTUnit( PTUnitNum ).DXCoolCoilIndexNum, ErrorsFound, _, PTUnit( PTUnitNum ).DXHeatCoilIndex );
					PTUnit( PTUnitNum ).useVSCoilModel = true;
				}
			}

			SuppHeatCoilType = Alphas( 13 );
			SuppHeatCoilName = Alphas( 14 );
			PTUnit( PTUnitNum ).SuppHeatCoilName = SuppHeatCoilName;
			if ( SameString( Alphas( 13 ), "Coil:Heating:Gas" ) || SameString( Alphas( 13 ), "Coil:Heating:Electric" ) || SameString( Alphas( 13 ), "Coil:Heating:Water" ) || SameString( Alphas( 13 ), "Coil:Heating:Steam" ) ) {
				PTUnit( PTUnitNum ).SuppHeatCoilType = SuppHeatCoilType;
				if ( SameString( Alphas( 13 ), "Coil:Heating:Gas" ) || SameString( Alphas( 13 ), "Coil:Heating:Electric" ) ) {
					if ( SameString( Alphas( 13 ), "Coil:Heating:Gas" ) ) {
						PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingGas;
					} else if ( SameString( Alphas( 13 ), "Coil:Heating:Electric" ) ) {
						PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingElectric;
					}
					errFlag = false;
					ValidateComponent( SuppHeatCoilType, SuppHeatCoilName, errFlag, CurrentModuleObject );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"." );
						ErrorsFound = true;
					} else {
						GetHeatingCoilIndex( SuppHeatCoilName, PTUnit( PTUnitNum ).SuppHeatCoilIndex, errFlag );
						// Get the Supplemental Heating Coil Node Numbers
						SuppHeatInletNodeNum = GetHeatingCoilInletNode( SuppHeatCoilType, SuppHeatCoilName, errFlag );
						SuppHeatOutletNodeNum = GetHeatingCoilOutletNode( SuppHeatCoilType, SuppHeatCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"." );
							ErrorsFound = true;
						}
					}
				} else if ( SameString( Alphas( 13 ), "Coil:Heating:Water" ) ) {
					PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingWater;
					errFlag = false;
					SuppHeatHWInletNodeNum = GetCoilWaterInletNode( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HotWaterControlNode = SuppHeatHWInletNodeNum;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
					PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					}
					errFlag = false;
					SuppHeatInletNodeNum = GetWaterCoilInletNode( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).SupCoilAirInletNode = SuppHeatInletNodeNum;
					SuppHeatOutletNodeNum = GetWaterCoilOutletNode( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}

				} else if ( SameString( Alphas( 13 ), "Coil:Heating:Steam" ) ) {
					PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingSteam;
					errFlag = false;
					PTUnit( PTUnitNum ).SuppHeatCoilIndex = GetSteamCoilIndex( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( PTUnit( PTUnitNum ).SuppHeatCoilIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 14 ) + " = " + PTUnit( PTUnitNum ).SuppHeatCoilName );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
					//IF (ErrFlag) CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
					errFlag = false;
					SuppHeatHWInletNodeNum = GetCoilSteamInletNode( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HWCoilSteamInletNode = SuppHeatHWInletNodeNum;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
					PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, errFlag );
					if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNameFull );
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, errFlag ) * SteamDensity;
					}
					errFlag = false;
					SuppHeatInletNodeNum = GetSteamCoilAirInletNode( PTUnit( PTUnitNum ).SuppHeatCoilIndex, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).SupCoilAirInletNode = SuppHeatInletNodeNum;
					SuppHeatOutletNodeNum = GetCoilAirOutletNode( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
				}

			} else {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 13 ) + " = " + Alphas( 13 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxSATSupHeat = Numbers( 10 );
			PTUnit( PTUnitNum ).MaxOATSupHeat = Numbers( 11 );
			if ( PTUnit( PTUnitNum ).MaxOATSupHeat > 21.0 ) {
				ShowWarningError( CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name + ": " + cNumericFields( 11 ) + " should be <= to 21." );
				ShowContinueError( "..." + cNumericFields( 11 ) + " = " + TrimSigDigits( Numbers( 11 ), 1 ) );
			}

			if ( SameString( Alphas( 15 ), "BlowThrough" ) ) PTUnit( PTUnitNum ).FanPlace = BlowThru;
			if ( SameString( Alphas( 15 ), "DrawThrough" ) ) PTUnit( PTUnitNum ).FanPlace = DrawThru;
			if ( PTUnit( PTUnitNum ).FanPlace == 0 ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 15 ) + " = " + Alphas( 15 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			// Check component placement
			if ( PTUnit( PTUnitNum ).FanPlace == BlowThru ) {
				// PTUnit inlet node must be the same as a zone exhaust node and the OA Mixer return node
				// check that PTUnit inlet node is a zone exhaust node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heat Pumps air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Heat pumps inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check OA Mixer return node
				if ( PTUnit( PTUnitNum ).AirInNode != OANodeNums( 3 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" PTUnit air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
					ShowContinueError( "..PTUnit air inlet node name            = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ShowContinueError( "..OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
					ErrorsFound = true;
				}
				// Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
				if ( OANodeNums( 4 ) != FanInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Fan inlet node name must be the same as the heat pumps" );
					ShowContinueError( "OutdoorAir:Mixer mixed air node name when blow through " + cAlphaFields( 15 ) + " is specified." );
					ShowContinueError( "..Fan inlet node name                   = " + NodeID( FanInletNodeNum ) );
					ShowContinueError( "..OutdoorAir:Mixer mixed air node name = " + NodeID( OANodeNums( 4 ) ) );
					ErrorsFound = true;
				}
				if ( CoolCoilInletNodeNum != FanOutletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Fan outlet node name must be the same as the cooling coil" );
					ShowContinueError( " inlet node name when blow through " + cAlphaFields( 15 ) + " is specified." );
					ShowContinueError( "..Fan outlet node name         = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "..Cooling coil inlet node name = " + NodeID( CoolCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( CoolCoilOutletNodeNum != HeatCoilInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Cooling coil outlet node name must be the same as the heating coil inlet node name." );
					ShowContinueError( "..Cooling coil outlet node name = " + NodeID( CoolCoilOutletNodeNum ) );
					ShowContinueError( "..Heating coil inlet node name  = " + NodeID( HeatCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( HeatCoilOutletNodeNum != SuppHeatInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heating coil outlet node name must be the same as the supplemental heating coil inlet" );
					ShowContinueError( " node name when blow through " + cAlphaFields( 14 ) + " is specified." );
					ShowContinueError( "..Heating coil outlet node name              = " + NodeID( HeatCoilOutletNodeNum ) );
					ShowContinueError( "..Supplemental heating coil inlet node name  = " + NodeID( SuppHeatInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( SuppHeatOutletNodeNum != PTUnit( PTUnitNum ).AirOutNode ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name." );
					ShowContinueError( "..Supplemental heating coil outlet node name = " + NodeID( SuppHeatOutletNodeNum ) );
					ShowContinueError( "..Heat pumps outlet node name                   = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
				// check that PTUnit outlet node is a zone inlet node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							PTUnit( PTUnitNum ).ZonePtr = CtrlZone;
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heat Pumps air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Heat pumps outlet node name = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
			} else { // draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
				// check that PTUnit inlet node is a zone exhaust node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heat Pumps air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Heat pumps inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check OA Mixer return node
				if ( PTUnit( PTUnitNum ).AirInNode != OANodeNums( 3 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" PTUnit air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
					ShowContinueError( "..PTUnit air inlet node name            = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ShowContinueError( "..OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
					ErrorsFound = true;
				}
				// Fan outlet node name must be the same as the supplemental heating coil inlet node name
				if ( CoolCoilInletNodeNum != OANodeNums( 4 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil" );
					ShowContinueError( " inlet node name when draw through " + cAlphaFields( 15 ) + " is specified." );
					ShowContinueError( "..OutdoorAir:Mixer mixed air name = " + NodeID( OANodeNums( 4 ) ) );
					ShowContinueError( "..Cooling coil inlet node name     = " + NodeID( CoolCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( CoolCoilOutletNodeNum != HeatCoilInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Cooling coil outlet node name must be the same as the heating coil inlet node name." );
					ShowContinueError( "..Cooling coil outlet node name = " + NodeID( CoolCoilOutletNodeNum ) );
					ShowContinueError( "..Heating coil inlet node name  = " + NodeID( HeatCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( HeatCoilOutletNodeNum != FanInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heating coil outlet node name must be the same as the fan inlet node name" );
					ShowContinueError( " when draw through " + cAlphaFields( 15 ) + " is specified." );
					ShowContinueError( "..Heating coil outlet node name = " + NodeID( HeatCoilOutletNodeNum ) );
					ShowContinueError( "..Fan inlet node name           = " + NodeID( FanInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( SuppHeatInletNodeNum != FanOutletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Fan outlet node name must be the same" );
					ShowContinueError( "as the supplemental heating coil inlet node name when draw through " + cAlphaFields( 15 ) + " is specified." );
					ShowContinueError( "..Fan outlet node = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "..Supplemental heating coil inlet node = " + NodeID( SuppHeatInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( SuppHeatOutletNodeNum != PTUnit( PTUnitNum ).AirOutNode ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name." );
					ShowContinueError( "..Supplemental heating coil outlet node name = " + NodeID( SuppHeatOutletNodeNum ) );
					ShowContinueError( "..Heat pumps outlet node name                = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
				// check that PTUnit outlet node is a zone inlet node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heat Pumps air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Heat pumps outlet node name = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
			} // IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

			PTUnit( PTUnitNum ).FanSchedPtr = GetScheduleIndex( Alphas( 16 ) );
			if ( ! lAlphaBlanks( 16 ) && PTUnit( PTUnitNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" " + cAlphaFields( 16 ) + " not found: " + Alphas( 16 ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( 16 ) ) {
				//     default to cycling fan if not specified in input
				PTUnit( PTUnitNum ).OpMode = CycFanCycCoil;
			}

			if ( ! lAlphaBlanks( 17 ) ) {
				PTUnit( PTUnitNum ).AvailManagerListName = Alphas( 17 );
			}

			PTUnit( PTUnitNum ).HVACSizingIndex = 0;
			if ( ! lAlphaBlanks( 18 )) {
				PTUnit( PTUnitNum ).HVACSizingIndex = FindItemInList( Alphas( 18 ), ZoneHVACSizing );
				if (PTUnit( PTUnitNum ).HVACSizingIndex == 0) {
					ShowSevereError( cAlphaFields( 18 ) + " = " + Alphas( 18 ) + " not found.");
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ErrorsFound = true;
				}
			}

			//   set air flow control mode, UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
			//                              UseCompressorOffFlow = operate at value specified by user
			//   AirFlowControl only valid if fan opmode = ContFanCycCoil
			if ( PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
				PTUnit( PTUnitNum ).AirFlowControl = UseCompressorOnFlow;
			} else {
				PTUnit( PTUnitNum ).AirFlowControl = UseCompressorOffFlow;
			}

			//   Initialize last mode of compressor operation
			PTUnit( PTUnitNum ).LastMode = HeatingMode;

			if ( SameString( PTUnit( PTUnitNum ).FanType, "Fan:OnOff" ) || SameString( PTUnit( PTUnitNum ).FanType, "Fan:ConstantVolume" ) ) {
				if ( PTUnit( PTUnitNum ).FanSchedPtr > 0 && SameString( PTUnit( PTUnitNum ).FanType, "Fan:ConstantVolume" ) ) {
					if ( ! CheckScheduleValueMinMax( PTUnit( PTUnitNum ).FanSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0) for supply fan type Fan:ConstantVolume." );
						ShowContinueError( "Error found in " + cAlphaFields( 16 ) + " = " + Alphas( 16 ) );
						ShowContinueError( "schedule values must be (>0., <=1.)" );
						ErrorsFound = true;
					} else if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow > PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow && PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != 0.0 ) {
						ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "Outdoor air flow rate when compressor is off cannot be greater than supply air flow rate when compressor is off" );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
				ShowContinueError( cAlphaFields( 8 ) + " \"" + PTUnit( PTUnitNum ).FanName + "\" must be type Fan:OnOff or Fan:ConstantVolume." );
				ErrorsFound = true;
			}

			if ( PTUnit( PTUnitNum ).DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignHeatingCapacity = GetCoilCapacityVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignCoolingCapacity = GetCoilCapacityVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			CompSetFanInlet = NodeID( FanInletNodeNum );
			CompSetFanOutlet = NodeID( FanOutletNodeNum );
			CompSetCoolInlet = NodeID( CoolCoilInletNodeNum );
			CompSetCoolOutlet = NodeID( CoolCoilOutletNodeNum );
			CompSetHeatInlet = NodeID( HeatCoilInletNodeNum );
			CompSetHeatOutlet = NodeID( HeatCoilOutletNodeNum );
			CompSetSupHeatInlet = NodeID( SuppHeatInletNodeNum );
			CompSetSupHeatOutlet = NodeID( SuppHeatOutletNodeNum );

			// Add fan to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, CompSetFanInlet, CompSetFanOutlet );

			// Add cooling coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, CompSetCoolInlet, CompSetCoolOutlet );

			// Add heating coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, CompSetHeatInlet, CompSetHeatOutlet );

			// Add supplemental heating coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, CompSetSupHeatInlet, CompSetSupHeatOutlet );

			if ( PTUnit( PTUnitNum ).UnitType_Num == PTHPUnit ) {
				if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
					// Add heating coil water inlet node as actuator node for coil
					TempNodeNum = GetOnlySingleNode( NodeID( PTUnit( PTUnitNum ).HotWaterControlNode ), ErrorsFound, PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsParent );
				} else if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {
					// Add heating coil steam inlet node as actualtor node for coil
					TempNodeNum = GetOnlySingleNode( NodeID( PTUnit( PTUnitNum ).HWCoilSteamInletNode ), ErrorsFound, PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, NodeType_Steam, NodeConnectionType_Actuator, 1, ObjectIsParent );
				}
			}
			// Set up component set for OA mixer - use OA node and Mixed air node
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).OAMixType, PTUnit( PTUnitNum ).OAMixName, NodeID( OANodeNums( 1 ) ), NodeID( OANodeNums( 4 ) ) );
		}

		// loop over PTAC units; get and load the input data
		for ( PTUnitIndex = 1; PTUnitIndex <= NumPTAC; ++PTUnitIndex ) {

			FanInletNodeNum = 0;
			FanOutletNodeNum = 0;
			CoolCoilInletNodeNum = 0;
			CoolCoilOutletNodeNum = 0;
			HeatCoilInletNodeNum = 0;
			HeatCoilOutletNodeNum = 0;
			SuppHeatInletNodeNum = 0;

			CurrentModuleObject = "ZoneHVAC:PackagedTerminalAirConditioner";
			GetObjectItem( CurrentModuleObject, PTUnitIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			PTUnitNum = PTUnitIndex + NumPTHP;

			PTUnitUNumericFields( PTUnitNum ).FieldNames.allocate( NumNumbers );
			PTUnitUNumericFields( PTUnitNum ).FieldNames = "";
			PTUnitUNumericFields( PTUnitNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PTUnit, PTUnitNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			PTUnit( PTUnitNum ).PTObjectIndex = PTUnitIndex;
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			PTUnit( PTUnitNum ).Name = Alphas( 1 );
			PTUnit( PTUnitNum ).UnitType = CurrentModuleObject;
			PTUnit( PTUnitNum ).UnitType_Num = PTACUnit;
			PTUnit( PTUnitNum ).ZoneEquipType = PkgTermACAirToAir_Num;
			if ( lAlphaBlanks( 2 ) ) {
				PTUnit( PTUnitNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				PTUnit( PTUnitNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer (index number)
				if ( PTUnit( PTUnitNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\" invalid data." );
					ShowContinueError( "invalid-not found " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			PTUnit( PTUnitNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			PTUnit( PTUnitNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			PTUnit( PTUnitNum ).OAMixType = Alphas( 5 );
			PTUnit( PTUnitNum ).OAMixName = Alphas( 6 );

			errFlag = false;
			ValidateComponent( PTUnit( PTUnitNum ).OAMixType, PTUnit( PTUnitNum ).OAMixName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				// OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
				OANodeNums = GetOAMixerNodeNumbers( PTUnit( PTUnitNum ).OAMixName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ShowContinueError( "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name." );
					ErrorsFound = true;
				} else {
					//  Set connection type to 'Inlet', because this is not necessarily directly come from
					//  outside air.  Outside Air Inlet Node List will set the connection to outside air
					PTUnit( PTUnitNum ).OutsideAirNode = OANodeNums( 1 );
					PTUnit( PTUnitNum ).AirReliefNode = OANodeNums( 2 );
				}
			}

			PTUnit( PTUnitNum ).MaxCoolAirVolFlow = Numbers( 1 );
			if ( PTUnit( PTUnitNum ).MaxCoolAirVolFlow <= 0 && PTUnit( PTUnitNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 1 ) + " = " + TrimSigDigits( Numbers( 1 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxHeatAirVolFlow = Numbers( 2 );
			if ( PTUnit( PTUnitNum ).MaxHeatAirVolFlow <= 0 && PTUnit( PTUnitNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 2 ) + " = " + TrimSigDigits( Numbers( 2 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow = Numbers( 3 );
			if ( PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow < 0 && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 3 ) + " = " + TrimSigDigits( Numbers( 3 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).CoolOutAirVolFlow = Numbers( 4 );
			if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow < 0 && PTUnit( PTUnitNum ).CoolOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 4 ) + " = " + TrimSigDigits( Numbers( 4 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			//   only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
			if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow > PTUnit( PTUnitNum ).MaxCoolAirVolFlow && PTUnit( PTUnitNum ).CoolOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + ' ' + cNumericFields( 4 ) + " cannot be greater than " + cNumericFields( 1 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).HeatOutAirVolFlow = Numbers( 5 );
			if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow < 0 && PTUnit( PTUnitNum ).HeatOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 5 ) + " = " + TrimSigDigits( Numbers( 5 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			//   only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
			if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow > PTUnit( PTUnitNum ).MaxHeatAirVolFlow && PTUnit( PTUnitNum ).HeatOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + ' ' + cNumericFields( 5 ) + " cannot be greater than " + cNumericFields( 2 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow = Numbers( 6 );
			if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow < 0 && PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 6 ) + " = " + TrimSigDigits( Numbers( 6 ), 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			//set minimum OA to something low because its not an input for PTACs
			PTUnit( PTUnitNum ).MinOATCompressor = -100.0;

			//   only check that SA flow when compressor is OFF is >= OA flow when compressor is OFF after fan mode is read in

			PTUnit( PTUnitNum ).FanType = Alphas( 7 );
			PTUnit( PTUnitNum ).FanName = Alphas( 8 );

			// Get the fan's availabitlity schedule
			errFlag = false;
			PTUnit( PTUnitNum ).FanAvailSchedPtr = GetFanAvailSchPtr( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
			if ( errFlag ) {
				ShowContinueError( "...specified in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			ValidateComponent( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, IsNotOK, CurrentModuleObject );
			if ( IsNotOK ) {
				ShowContinueError( "In " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}
			errFlag = false;
			GetFanType( PTUnit( PTUnitNum ).FanName, PTUnit( PTUnitNum ).FanType_Num, errFlag, CurrentModuleObject, PTUnit( PTUnitNum ).Name );
			FanVolFlow = 0.0;
			if ( errFlag ) {
				ShowContinueError( "...specified in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
				ErrorsFound = true;
			} else {
				GetFanIndex( PTUnit( PTUnitNum ).FanName, PTUnit( PTUnitNum ).FanIndex, errFlag, CurrentModuleObject );
				FanInletNodeNum = GetFanInletNode( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
				FanOutletNodeNum = GetFanOutletNode( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
				GetFanVolFlow( PTUnit( PTUnitNum ).FanIndex, FanVolFlow );
				PTUnit( PTUnitNum ).ActualFanVolFlowRate = FanVolFlow;
			}

			if ( FanVolFlow != AutoSize ) {
				if ( FanVolFlow < max( PTUnit( PTUnitNum ).MaxCoolAirVolFlow, PTUnit( PTUnitNum ).MaxHeatAirVolFlow, PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow ) ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in fan object " + PTUnit( PTUnitNum ).FanName + " is less than the maximum PTHP supply air flow rate." );
					ShowContinueError( " The fan flow rate must be greater than the PTHP maximum supply air flow rate." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ErrorsFound = true;
				}
			}

			//   Name is currently used in CALL to Sim routines, can't get rid of the character string at this time.
			PTUnit( PTUnitNum ).ACHeatCoilName = Alphas( 10 );
			ACHeatCoilName = Alphas( 10 );

			if ( SameString( Alphas( 9 ), "Coil:Heating:Gas" ) || SameString( Alphas( 9 ), "Coil:Heating:Electric" ) || SameString( Alphas( 9 ), "Coil:Heating:Water" ) || SameString( Alphas( 9 ), "Coil:Heating:Steam" ) ) {
				PTUnit( PTUnitNum ).ACHeatCoilType = Alphas( 9 );
				if ( SameString( Alphas( 9 ), "Coil:Heating:Gas" ) || SameString( Alphas( 9 ), "Coil:Heating:Electric" ) ) {
					if ( SameString( Alphas( 9 ), "Coil:Heating:Gas" ) ) PTUnit( PTUnitNum ).ACHeatCoilType_Num = Coil_HeatingGas;
					if ( SameString( Alphas( 9 ), "Coil:Heating:Electric" ) ) PTUnit( PTUnitNum ).ACHeatCoilType_Num = Coil_HeatingElectric;
					PTUnit( PTUnitNum ).ACHeatCoilCap = GetHeatingCoilCapacity( PTUnit( PTUnitNum ).ACHeatCoilType, ACHeatCoilName, ErrorsFound );
					errFlag = false;
					HeatCoilInletNodeNum = GetHeatingCoilInletNode( PTUnit( PTUnitNum ).ACHeatCoilType, ACHeatCoilName, errFlag );
					HeatCoilOutletNodeNum = GetHeatingCoilOutletNode( PTUnit( PTUnitNum ).ACHeatCoilType, ACHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 9 ), "Coil:Heating:Water" ) ) {
					PTUnit( PTUnitNum ).ACHeatCoilType_Num = Coil_HeatingWater;
					errFlag = false;
					PTUnit( PTUnitNum ).HotWaterControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", ACHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", ACHeatCoilName, errFlag );
					if ( PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", ACHeatCoilName, errFlag );
					}
					HeatCoilInletNodeNum = GetWaterCoilInletNode( "Coil:Heating:Water", ACHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HWCoilAirInletNode = HeatCoilInletNodeNum;
					HeatCoilOutletNodeNum = GetWaterCoilOutletNode( "Coil:Heating:Water", PTUnit( PTUnitNum ).ACHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 9 ), "Coil:Heating:Steam" ) ) {
					PTUnit( PTUnitNum ).ACHeatCoilType_Num = Coil_HeatingSteam;
					errFlag = false;
					PTUnit( PTUnitNum ).ACHeatCoilIndex = GetSteamCoilIndex( Alphas( 9 ), ACHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HWCoilAirInletNode = GetSteamCoilAirInletNode( PTUnit( PTUnitNum ).ACHeatCoilIndex, ACHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HWCoilSteamInletNode = GetCoilSteamInletNode( PTUnit( PTUnitNum ).ACHeatCoilIndex, ACHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).ACHeatCoilIndex, errFlag );
					SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNameFull );
					if ( PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).ACHeatCoilIndex, errFlag ) * SteamDensity;
					}
					HeatCoilInletNodeNum = PTUnit( PTUnitNum ).HWCoilAirInletNode;
					HeatCoilOutletNodeNum = GetCoilAirOutletNode( PTUnit( PTUnitNum ).ACHeatCoilIndex, ACHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ErrorsFound = true;
					}
					if ( GetTypeOfCoil( PTUnit( PTUnitNum ).ACHeatCoilIndex, ACHeatCoilName, errFlag ) != ZoneLoadControl ) {
						if ( errFlag ) {
							ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
							ErrorsFound = true;
						}
						ShowSevereError( CurrentModuleObject + PTUnit( PTUnitNum ).Name + "\" Steam coil type of control must be set to ZoneLoadControl in the heating coil = Coil:Heating:Steam \"" + ACHeatCoilName + "\"" );
						ErrorsFound = true;
					}
				}
			} else {
				ShowWarningError( CurrentModuleObject + " illegal " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).HeatConvergenceTol = 0.001;
			PTUnit( PTUnitNum ).DXCoolCoilName = Alphas( 12 );

			if ( SameString( Alphas( 11 ), "Coil:Cooling:DX:SingleSpeed" ) || SameString( Alphas( 11 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
				PTUnit( PTUnitNum ).DXCoolCoilType = Alphas( 11 );
				if ( SameString( Alphas( 11 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
					PTUnit( PTUnitNum ).DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed;
					errFlag = false;
					GetDXCoilIndex( PTUnit( PTUnitNum ).DXCoolCoilName, PTUnit( PTUnitNum ).DXCoolCoilIndexNum, errFlag, PTUnit( PTUnitNum ).DXCoolCoilType );
					CoolCoilInletNodeNum = GetDXCoilInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetDXCoilOutletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					PTUnit( PTUnitNum ).CondenserNodeNum = GetCoilCondenserInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 11 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
					PTUnit( PTUnitNum ).DXCoolCoilType_Num = CoilDX_CoolingHXAssisted;
					errFlag = false;
					GetDXCoilIndex( GetHXDXCoilName( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag ), PTUnit( PTUnitNum ).DXCoolCoilIndexNum, errFlag, "Coil:Cooling:DX:SingleSpeed" );
					CoolCoilInletNodeNum = GetHXDXCoilInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetHXDXCoilOutletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					PTUnit( PTUnitNum ).CondenserNodeNum = GetCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed", GetHXDXCoilName( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag ), errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ErrorsFound = true;
					}
				}
			} else if ( SameString( Alphas( 11 ), "COIL:COOLING:DX:VARIABLESPEED" ) ) {
				PTUnit( PTUnitNum ).DXCoolCoilType = Alphas( 11 );
				PTUnit( PTUnitNum ).DXCoolCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
				PTUnit( PTUnitNum ).DXCoolCoilName = Alphas( 12 );
				ValidateComponent( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXCoolCoilIndexNum = GetCoilIndexVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					CoolCoilInletNodeNum = GetCoilInletNodeVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					PTUnit( PTUnitNum ).CondenserNodeNum = GetVSCoilCondenserInletNode( PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );

					if ( errFlag ) ShowContinueError( "...occurs in " + PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
				}
			} else {
				ShowWarningError( CurrentModuleObject + " illegal " + cAlphaFields( 11 ) + " = " + Alphas( 11 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 13 ), "BlowThrough" ) ) PTUnit( PTUnitNum ).FanPlace = BlowThru;
			if ( SameString( Alphas( 13 ), "DrawThrough" ) ) PTUnit( PTUnitNum ).FanPlace = DrawThru;
			//   default to draw through if not specified in input
			if ( lAlphaBlanks( 13 ) ) PTUnit( PTUnitNum ).FanPlace = DrawThru;
			if ( PTUnit( PTUnitNum ).FanPlace == 0 ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 13 ) + " = " + Alphas( 13 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
				ErrorsFound = true;
			}

			// Check component placement
			if ( PTUnit( PTUnitNum ).FanPlace == BlowThru ) {
				// PTUnit inlet node must be the same as a zone exhaust node and the OA Mixer return node
				// check that PTUnit inlet node is a zone exhaust node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Air Conditioners air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Air Conditioners inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check OA Mixer return node
				if ( PTUnit( PTUnitNum ).AirInNode != OANodeNums( 3 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Air Conditioners air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
					ShowContinueError( "..PTUnit air inlet node name            = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ShowContinueError( "..OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
					ErrorsFound = true;
				}
				// Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
				if ( OANodeNums( 4 ) != FanInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Fan inlet node name must be the same as the air conditioners" );
					ShowContinueError( "OutdoorAir:Mixer mixed air node name when blow through " + cAlphaFields( 13 ) + " is specified." );
					ShowContinueError( "..Fan inlet node name                   = " + NodeID( FanInletNodeNum ) );
					ShowContinueError( "..OutdoorAir:Mixer mixed air node name = " + NodeID( OANodeNums( 4 ) ) );
					ErrorsFound = true;
				}
				if ( CoolCoilInletNodeNum != FanOutletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Fan outlet node name must be the same as the cooling coil" );
					ShowContinueError( " inlet node name when blow through " + cAlphaFields( 12 ) + " is specified." );
					ShowContinueError( "..Fan outlet node name         = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "..Cooling coil inlet node name = " + NodeID( CoolCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( CoolCoilOutletNodeNum != HeatCoilInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Cooling coil outlet node name must be the same as the heating coil inlet node name." );
					ShowContinueError( "..Cooling coil outlet node name = " + NodeID( CoolCoilOutletNodeNum ) );
					ShowContinueError( "..Heating coil inlet node name  = " + NodeID( HeatCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( HeatCoilOutletNodeNum != PTUnit( PTUnitNum ).AirOutNode ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heating coil outlet node name must be the same as the air conditioners outlet" );
					ShowContinueError( " node name when blow through " + cAlphaFields( 12 ) + " is specified." );
					ShowContinueError( "..Heating coil outlet node name      = " + NodeID( HeatCoilOutletNodeNum ) );
					ShowContinueError( "..Air conditioners outlet node name  = " + NodeID( SuppHeatInletNodeNum ) );
					ErrorsFound = true;
				}
				// check that PTUnit outlet node is a zone inlet node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							PTUnit( PTUnitNum ).ZonePtr = CtrlZone;
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Air Conditioners air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Air Conditioners outlet node name = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
			} else { // draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
				// PTUnit inlet node must be the same as a zone exhaust node and the OA Mixer return node
				// check that PTUnit inlet node is a zone exhaust node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Air Conditioners air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Air Conditioners inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check OA Mixer return node
				if ( PTUnit( PTUnitNum ).AirInNode != OANodeNums( 3 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Air Conditioners air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
					ShowContinueError( "..Air Conditioner air inlet node name   = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ShowContinueError( "..OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
					ErrorsFound = true;
				}
				// cooling coil inlet node name must be the same as the OA mixers mixed air node name
				if ( CoolCoilInletNodeNum != OANodeNums( 4 ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil" );
					ShowContinueError( " inlet node name when draw through " + cAlphaFields( 13 ) + " is specified." );
					ShowContinueError( "..OutdoorAir:Mixer mixed air name = " + NodeID( OANodeNums( 4 ) ) );
					ShowContinueError( "..Cooling coil inlet node name     = " + NodeID( CoolCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( CoolCoilOutletNodeNum != HeatCoilInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Cooling coil outlet node name must be the same as the heating coil inlet node name." );
					ShowContinueError( "..Cooling coil outlet node name = " + NodeID( CoolCoilOutletNodeNum ) );
					ShowContinueError( "..Heating coil inlet node name  = " + NodeID( HeatCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( HeatCoilOutletNodeNum != FanInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Heating coil outlet node name must be the same as the fan inlet node name" );
					ShowContinueError( " when blow through " + cAlphaFields( 13 ) + " is specified." );
					ShowContinueError( "..Heating coil outlet node name = " + NodeID( HeatCoilOutletNodeNum ) );
					ShowContinueError( "..Fan inlet node name           = " + NodeID( FanInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( FanOutletNodeNum != PTUnit( PTUnitNum ).AirOutNode ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Fan outlet node name must be the same" );
					ShowContinueError( "as the air conditioners outlet node name when draw through " + cAlphaFields( 13 ) + " is specified." );
					ShowContinueError( "..Fan outlet node  name             = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "..Air conditioners outlet node name = " + NodeID( SuppHeatInletNodeNum ) );
					ErrorsFound = true;
				}
				// check that PTUnit outlet node is a zone inlet node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							PTUnit(PTUnitNum).ZonePtr = CtrlZone;
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Air Conditionerss air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Air Conditioners outlet node name = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
			} // IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

			PTUnit( PTUnitNum ).FanSchedPtr = GetScheduleIndex( Alphas( 14 ) );
			if ( ! lAlphaBlanks( 14 ) && PTUnit( PTUnitNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" " + cAlphaFields( 14 ) + " not found: " + Alphas( 14 ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( 14 ) ) {
				//     default to cycling fan if not specified in input
				PTUnit( PTUnitNum ).OpMode = CycFanCycCoil;
			}

			if ( ! lAlphaBlanks( 15 ) ) {
				PTUnit( PTUnitNum ).AvailManagerListName = Alphas( 15 );
			}

			PTUnit( PTUnitNum ).HVACSizingIndex = 0;
			if ( ! lAlphaBlanks( 16 ) ) {
				PTUnit( PTUnitNum ).HVACSizingIndex = FindItemInList( Alphas( 16 ), ZoneHVACSizing );
				if ( PTUnit( PTUnitNum ).HVACSizingIndex == 0) {
					ShowSevereError( cAlphaFields( 16 ) + " = " + Alphas( 16 ) + " not found." );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ErrorsFound = true;
				}
			}

			//   set air flow control mode, UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
			//                              UseCompressorOffFlow = operate at value specified by user
			//   AirFlowControl only valid if fan opmode = ContFanCycCoil
			if ( PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
				PTUnit( PTUnitNum ).AirFlowControl = UseCompressorOnFlow;
			} else {
				PTUnit( PTUnitNum ).AirFlowControl = UseCompressorOffFlow;
			}

			//   Initialize last mode of compressor operation
			PTUnit( PTUnitNum ).LastMode = HeatingMode;

			if ( SameString( PTUnit( PTUnitNum ).FanType, "Fan:OnOff" ) || SameString( PTUnit( PTUnitNum ).FanType, "Fan:ConstantVolume" ) ) {
				if ( PTUnit( PTUnitNum ).FanSchedPtr > 0 && SameString( PTUnit( PTUnitNum ).FanType, "Fan:ConstantVolume" ) ) {
					if ( ! CheckScheduleValueMinMax( PTUnit( PTUnitNum ).FanSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0) for supply fan type Fan:ConstantVolume." );
						ShowContinueError( "Error found in " + cAlphaFields( 14 ) + " = " + Alphas( 14 ) );
						ShowContinueError( "schedule values must be (>0., <=1.)" );
						ErrorsFound = true;
					} else if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow > PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow && PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != 0.0 ) {
						ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "Outdoor air flow rate when compressor is off cannot be greater than supply air flow rate when compressor is off" );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
				ShowContinueError( cAlphaFields( 8 ) + " \"" + PTUnit( PTUnitNum ).FanName + "\" must be type Fan:OnOff or Fan:ConstantVolume." );
				ErrorsFound = true;
			}

			if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignCoolingCapacity = GetCoilCapacityVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				PTUnit( PTUnitNum ).useVSCoilModel = true;
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			CompSetFanInlet = NodeID( FanInletNodeNum );
			CompSetFanOutlet = NodeID( FanOutletNodeNum );
			CompSetCoolInlet = NodeID( CoolCoilInletNodeNum );
			CompSetCoolOutlet = NodeID( CoolCoilOutletNodeNum );
			CompSetHeatInlet = NodeID( HeatCoilInletNodeNum );
			CompSetHeatOutlet = NodeID( HeatCoilOutletNodeNum );

			// Add fan to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, NodeID( FanInletNodeNum ), NodeID( FanOutletNodeNum ) );

			// Add cooling coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, NodeID( CoolCoilInletNodeNum ), NodeID( CoolCoilOutletNodeNum ) );

			// Add heating coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).ACHeatCoilType, ACHeatCoilName, NodeID( HeatCoilInletNodeNum ), NodeID( HeatCoilOutletNodeNum ) );

			if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
				if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {
					// Add heating coil water inlet node as actuator node for coil
					TempNodeNum = GetOnlySingleNode( NodeID( PTUnit( PTUnitNum ).HotWaterControlNode ), ErrorsFound, PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsParent );
				} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {
					// Add heating coil steam inlet node as actualtor node for coil
					TempNodeNum = GetOnlySingleNode( NodeID( PTUnit( PTUnitNum ).HWCoilSteamInletNode ), ErrorsFound, PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, NodeType_Steam, NodeConnectionType_Actuator, 1, ObjectIsParent );
				}
			}

			// Set up component set for OA mixer - use OA node and Mixed air node
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).OAMixType, PTUnit( PTUnitNum ).OAMixName, NodeID( OANodeNums( 1 ) ), NodeID( OANodeNums( 4 ) ) );
		}

		//***********************************************************************************

		for ( PTUnitIndex = 1; PTUnitIndex <= NumPTWSHP; ++PTUnitIndex ) {

			FanInletNodeNum = 0;
			FanOutletNodeNum = 0;
			CoolCoilInletNodeNum = 0;
			CoolCoilOutletNodeNum = 0;
			HeatCoilInletNodeNum = 0;
			HeatCoilOutletNodeNum = 0;
			SuppHeatInletNodeNum = 0;
			SuppHeatOutletNodeNum = 0;
			SuppHeatHWInletNodeNum = 0;
			SuppHeatHWOutletNodeNum = 0;
			OANodeNums = 0;

			CurrentModuleObject = "ZoneHVAC:WaterToAirHeatPump";
			GetObjectItem( CurrentModuleObject, PTUnitIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			PTUnitNum = PTUnitIndex + NumPTHP + NumPTAC;

			PTUnitUNumericFields( PTUnitNum ).FieldNames.allocate( NumNumbers );
			PTUnitUNumericFields( PTUnitNum ).FieldNames = "";
			PTUnitUNumericFields( PTUnitNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PTUnit, PTUnitNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			PTUnit( PTUnitNum ).PTObjectIndex = PTUnitIndex;
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			PTUnit( PTUnitNum ).Name = Alphas( 1 );
			PTUnit( PTUnitNum ).UnitType = CurrentModuleObject;
			PTUnit( PTUnitNum ).UnitType_Num = PTWSHPUnit;
			PTUnit( PTUnitNum ).ZoneEquipType = PkgTermHPWaterToAir_Num;
			if ( lAlphaBlanks( 2 ) ) {
				PTUnit( PTUnitNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				PTUnit( PTUnitNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( PTUnit( PTUnitNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\" invalid data." );
					ShowContinueError( "invalid-not found " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			PTUnit( PTUnitNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			PTUnit( PTUnitNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			PTUnit( PTUnitNum ).OAMixType = Alphas( 5 );
			PTUnit( PTUnitNum ).OAMixName = Alphas( 6 );

			// check to see if local OA mixer specified
			if ( ! lAlphaBlanks( 6 ) ) {
				errFlag = false;
				ValidateComponent( PTUnit( PTUnitNum ).OAMixType, PTUnit( PTUnitNum ).OAMixName, errFlag, CurrentModuleObject );
				if ( errFlag ) {
					ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\"." );
					ErrorsFound = true;
				} else {
					// OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
					OANodeNums = GetOAMixerNodeNumbers( PTUnit( PTUnitNum ).OAMixName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "that was specified in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ShowContinueError( "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name." );
						ErrorsFound = true;
					} else {
						//  Set connection type to 'Inlet', because this is not necessarily directly come from
						//  outside air.  Outside Air Inlet Node List will set the connection to outside air
						PTUnit( PTUnitNum ).OutsideAirNode = OANodeNums( 1 );
						PTUnit( PTUnitNum ).AirReliefNode = OANodeNums( 2 );
					}
				}
			}

			//Get fan data
			PTUnit( PTUnitNum ).FanType = Alphas( 7 );
			PTUnit( PTUnitNum ).FanName = Alphas( 8 );
			errFlag = false;
			GetFanType( PTUnit( PTUnitNum ).FanName, PTUnit( PTUnitNum ).FanType_Num, errFlag, CurrentModuleObject, Alphas( 1 ) );
			FanVolFlow = 0.0;
			if ( errFlag ) {
				ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"." );
				ErrorsFound = true;
			}

			if ( PTUnit( PTUnitNum ).FanType_Num == FanType_SimpleOnOff ) {
				ValidateComponent( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					GetFanIndex( PTUnit( PTUnitNum ).FanName, PTUnit( PTUnitNum ).FanIndex, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					errFlag = false;
					FanInletNodeNum = GetFanInletNode( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					errFlag = false;
					FanOutletNodeNum = GetFanOutletNode( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					} else {
						GetFanVolFlow( PTUnit( PTUnitNum ).FanIndex, FanVolFlow );
						PTUnit( PTUnitNum ).ActualFanVolFlowRate = FanVolFlow;
					}
					errFlag = false;
					PTUnit( PTUnitNum ).FanAvailSchedPtr = GetFanAvailSchPtr( PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "Illegal " + cAlphaFields( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
				ErrorsFound = true;
			}

			//Get heating coil type and name data
			if ( Alphas( 9 ) == "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT" ) {
				PTUnit( PTUnitNum ).DXHeatCoilType = Alphas( 9 );
				PTUnit( PTUnitNum ).DXHeatCoilType_Num = Coil_HeatingWaterToAirHPSimple;
				PTUnit( PTUnitNum ).DXHeatCoilName = Alphas( 10 );
				ValidateComponent( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXHeatCoilIndex = GetWtoAHPSimpleCoilIndex( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					HeatCoilInletNodeNum = GetWtoAHPSimpleCoilInletNode( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
					HeatCoilOutletNodeNum = GetWtoAHPSimpleCoilOutletNode( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				}
			} else if ( Alphas( 9 ) == "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) {
				PTUnit( PTUnitNum ).DXHeatCoilType = Alphas( 9 );
				PTUnit( PTUnitNum ).DXHeatCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit;
				PTUnit( PTUnitNum ).DXHeatCoilName = Alphas( 10 );
				ValidateComponent( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXHeatCoilIndex = GetCoilIndexVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					HeatCoilInletNodeNum = GetCoilInletNodeVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
					HeatCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "Illegal " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
				ErrorsFound = true;
			}

			// Get Cooling Coil Information if available
			if ( Alphas( 11 ) == "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT" ) {
				PTUnit( PTUnitNum ).DXCoolCoilType = Alphas( 11 );
				PTUnit( PTUnitNum ).DXCoolCoilType_Num = Coil_CoolingWaterToAirHPSimple;
				PTUnit( PTUnitNum ).DXCoolCoilName = Alphas( 12 );
				ValidateComponent( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXCoolCoilIndexNum = GetWtoAHPSimpleCoilIndex( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					CoolCoilInletNodeNum = GetWtoAHPSimpleCoilInletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetWtoAHPSimpleCoilOutletNode( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				}
			} else if ( Alphas( 11 ) == "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) {
				PTUnit( PTUnitNum ).DXCoolCoilType = Alphas( 11 );
				PTUnit( PTUnitNum ).DXCoolCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit;
				PTUnit( PTUnitNum ).DXCoolCoilName = Alphas( 12 );
				ValidateComponent( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
					ErrorsFound = true;
				} else {
					errFlag = false;
					PTUnit( PTUnitNum ).DXCoolCoilIndexNum = GetCoilIndexVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"." );
						ErrorsFound = true;
					}
					CoolCoilInletNodeNum = GetCoilInletNodeVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
					CoolCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "Illegal " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 19 ) {
				// get water flow mode info before calling SetSimpleWSHPData
				if ( SameString( Alphas( 19 ), "Constant" ) ) PTUnit( PTUnitNum ).WaterCyclingMode = WaterConstant;
				if ( SameString( Alphas( 19 ), "Cycling" ) ) PTUnit( PTUnitNum ).WaterCyclingMode = WaterCycling;
				if ( SameString( Alphas( 19 ), "ConstantOnDemand" ) ) PTUnit( PTUnitNum ).WaterCyclingMode = WaterConstantOnDemand;
				//default to draw through if not specified in input
				if ( lAlphaBlanks( 19 ) ) PTUnit( PTUnitNum ).WaterCyclingMode = WaterCycling;
				if ( PTUnit( PTUnitNum ).WaterCyclingMode == 0 ) {
					ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 19 ) + " = " + Alphas( 19 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ErrorsFound = true;
				}
			} else {
				PTUnit( PTUnitNum ).WaterCyclingMode = WaterCycling;
			}

			// end get water flow mode info
			if ( Alphas( 9 ) == "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT" && Alphas( 11 ) == "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT" ) {
				if ( PTUnit( PTUnitNum ).DXHeatCoilIndex > 0 && PTUnit( PTUnitNum ).DXCoolCoilIndexNum > 0 ) {
					SetSimpleWSHPData( PTUnit( PTUnitNum ).DXCoolCoilIndexNum, ErrorsFound, PTUnit( PTUnitNum ).WaterCyclingMode, _, PTUnit( PTUnitNum ).DXHeatCoilIndex );
					//         CALL SetSimpleWSHPData(PTUnit(PTUnitNum)%WaterCyclingMode, PTUnit(PTUnitNum)%DXHeatCoilIndex,ErrorsFound, &
					//                                CompanionCoolingCoilNum=PTUnit(PTUnitNum)%DXCoolCoilIndexNum)
				}
			} else if ( Alphas( 9 ) == "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" && Alphas( 11 ) == "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) {
				if ( PTUnit( PTUnitNum ).DXHeatCoilIndex > 0 && PTUnit( PTUnitNum ).DXCoolCoilIndexNum > 0 ) {
					SetVarSpeedCoilData( PTUnit( PTUnitNum ).DXCoolCoilIndexNum, ErrorsFound, _, PTUnit( PTUnitNum ).DXHeatCoilIndex );
					PTUnit( PTUnitNum ).useVSCoilModel = true;
				}
			} else {
				ShowContinueError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "Cooling coil and heating coil should use the equation fit model and be of same general type" );
				ErrorsFound = true;
			}

			// Get supplemental heating coil information

			SuppHeatCoilType = Alphas( 13 );
			SuppHeatCoilName = Alphas( 14 );
			PTUnit( PTUnitNum ).SuppHeatCoilName = SuppHeatCoilName;
			if ( SameString( Alphas( 13 ), "Coil:Heating:Gas" ) || SameString( Alphas( 13 ), "Coil:Heating:Electric" ) || SameString( Alphas( 13 ), "Coil:Heating:Water" ) || SameString( Alphas( 13 ), "Coil:Heating:Steam" ) ) {
				PTUnit( PTUnitNum ).SuppHeatCoilType = SuppHeatCoilType;
				if ( SameString( Alphas( 13 ), "Coil:Heating:Gas" ) || SameString( Alphas( 13 ), "Coil:Heating:Electric" ) ) {
					if ( SameString( Alphas( 13 ), "Coil:Heating:Gas" ) ) {
						PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingGas;
					} else if ( SameString( Alphas( 13 ), "Coil:Heating:Electric" ) ) {
						PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingElectric;
					}
					errFlag = false;
					ValidateComponent( SuppHeatCoilType, SuppHeatCoilName, errFlag, CurrentModuleObject );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"." );
						ErrorsFound = true;
					} else {
						GetHeatingCoilIndex( SuppHeatCoilName, PTUnit( PTUnitNum ).SuppHeatCoilIndex, errFlag );
						// Get the Supplemental Heating Coil Node Numbers
						SuppHeatInletNodeNum = GetHeatingCoilInletNode( SuppHeatCoilType, SuppHeatCoilName, errFlag );
						SuppHeatOutletNodeNum = GetHeatingCoilOutletNode( SuppHeatCoilType, SuppHeatCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"." );
							ErrorsFound = true;
						}
					}
				} else if ( SameString( Alphas( 13 ), "Coil:Heating:Water" ) ) {
					PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingWater;
					errFlag = false;
					SuppHeatHWInletNodeNum = GetCoilWaterInletNode( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HotWaterControlNode = SuppHeatHWInletNodeNum;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
					PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					}
					errFlag = false;
					SuppHeatInletNodeNum = GetWaterCoilInletNode( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).SupCoilAirInletNode = SuppHeatInletNodeNum;
					SuppHeatOutletNodeNum = GetWaterCoilOutletNode( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}

				} else if ( SameString( Alphas( 13 ), "Coil:Heating:Steam" ) ) {
					PTUnit( PTUnitNum ).SuppHeatCoilType_Num = Coil_HeatingSteam;
					errFlag = false;
					PTUnit( PTUnitNum ).SuppHeatCoilIndex = GetSteamCoilIndex( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( PTUnit( PTUnitNum ).SuppHeatCoilIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 14 ) + " = " + PTUnit( PTUnitNum ).SuppHeatCoilName );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
					errFlag = false;
					SuppHeatHWInletNodeNum = GetCoilSteamInletNode( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).HWCoilSteamInletNode = SuppHeatHWInletNodeNum;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
					PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, errFlag );
					if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineNameFull );
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, errFlag ) * SteamDensity;
					}
					errFlag = false;
					SuppHeatInletNodeNum = GetSteamCoilAirInletNode( PTUnit( PTUnitNum ).SuppHeatCoilIndex, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					PTUnit( PTUnitNum ).SupCoilAirInletNode = SuppHeatInletNodeNum;
					SuppHeatOutletNodeNum = GetCoilAirOutletNode( SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = " + Alphas( 1 ) );
				ShowContinueError( "Illegal " + cAlphaFields( 13 ) + " = " + Alphas( 13 ) );
				ErrorsFound = true;
			}

			if ( lAlphaBlanks( 15 ) ) {
				PTUnit( PTUnitNum ).CondenserNodeNum = 0;
			} else {
				PTUnit( PTUnitNum ).CondenserNodeNum = GetOnlySingleNode( Alphas( 15 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				// need better verification.
				if ( ! CheckOutAirNodeNumber( PTUnit( PTUnitNum ).CondenserNodeNum ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
					ShowContinueError( " Node name of outdoor dry-bulb temperature sensor not valid outdoor air node=\"" + Alphas( 15 ) + "\"" );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

			if ( SameString( Alphas( 16 ), "BlowThrough" ) ) PTUnit( PTUnitNum ).FanPlace = BlowThru;
			if ( SameString( Alphas( 16 ), "DrawThrough" ) ) PTUnit( PTUnitNum ).FanPlace = DrawThru;
			if ( PTUnit( PTUnitNum ).FanPlace == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
				ShowContinueError( "Illegal " + cAlphaFields( 16 ) + "=\"" + Alphas( 16 ) + "\"." );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).FanSchedPtr = GetScheduleIndex( Alphas( 17 ) );
			if ( ! lAlphaBlanks( 17 ) && PTUnit( PTUnitNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " = " + Alphas( 1 ) );
				ShowContinueError( "Illegal " + cAlphaFields( 17 ) + " = " + Alphas( 17 ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( 17 ) ) {
				PTUnit( PTUnitNum ).OpMode = CycFanCycCoil;
			}

			if ( ! lAlphaBlanks( 18 ) ) {
				PTUnit( PTUnitNum ).AvailManagerListName = Alphas( 18 );
			}

			PTUnit( PTUnitNum ).HVACSizingIndex = 0;
			if ( ! lAlphaBlanks( 20 ) ) {
				PTUnit( PTUnitNum ).HVACSizingIndex = FindItemInList( Alphas( 20 ), ZoneHVACSizing );
				if (PTUnit( PTUnitNum ).HVACSizingIndex == 0 ) {
					ShowSevereError( cAlphaFields( 20 ) + " = " + Alphas( 20 ) + " not found." );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
					ErrorsFound = true;
				}
			}

			// Get AirTerminal mixer data
			GetATMixer( PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).ATMixerName, PTUnit( PTUnitNum ).ATMixerIndex, PTUnit( PTUnitNum ).ATMixerType, PTUnit( PTUnitNum ).ATMixerPriNode, PTUnit( PTUnitNum ).ATMixerSecNode, PTUnit( PTUnitNum ).ATMixerOutNode );
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide || PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				PTUnit( PTUnitNum ).ATMixerExists = true;
			}

			// check that heat pump doesn' have local outside air and DOA
			if ( PTUnit( PTUnitNum ).ATMixerExists && OANodeNums( 4 ) > 0 ) {
				ShowSevereError( CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\". heat pump unit has local as well as central outdoor air specified" );
				ErrorsFound = true;
			}

			// Check component placement and connectivity

			//Placement checks good for both blow-thru and draw-thru fan
			if ( CoolCoilOutletNodeNum != HeatCoilInletNodeNum ) { // cooling coil outlet must equal heating coil inlet
				ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" Cooling coil outlet node name must be the same as the heating coil inlet node name." );
				ShowContinueError( "..Cooling coil outlet node name = " + NodeID( CoolCoilOutletNodeNum ) );
				ShowContinueError( "..Heating coil inlet node name  = " + NodeID( HeatCoilInletNodeNum ) );
				ErrorsFound = true;
			}
			if ( SuppHeatOutletNodeNum != PTUnit( PTUnitNum ).AirOutNode ) { // check that supp HC out = heat pump air outlet
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
				ShowContinueError( "..Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name." );
				ShowContinueError( "..Supplemental heating coil outlet node name = " + NodeID( SuppHeatOutletNodeNum ) );
				ShowContinueError( "..Heat pumps outlet node name                   = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
				ErrorsFound = true;
			}
			// check that PTUnit inlet node is a zone exhaust node.
			if ( ! PTUnit( PTUnitNum ).ATMixerExists || PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "..Heat Pumps air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Heat pumps inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ErrorsFound = true;
				}
			}
			// check that PTUnit outlet node is a zone inlet node.
			if ( ! PTUnit( PTUnitNum ).ATMixerExists || PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide ) {
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( PTUnit( PTUnitNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							PTUnit( PTUnitNum ).ZonePtr = CtrlZone;
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "..Heat Pumps air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Heat pumps outlet node name = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
			}
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide ) {
				// check that the air teminal mixer out node is the heat pump inlet node
				if ( PTUnit( PTUnitNum ).AirInNode != PTUnit( PTUnitNum ).ATMixerOutNode ) {
					ShowSevereError( CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\". heat pump unit air inlet node name must be the same as the air terminal mixer outlet node name." );
					ShowContinueError( "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:InletSideMixer object." );
					ShowContinueError( "..heat pump unit air inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
					ErrorsFound = true;
				}
			}
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				// check that the air teminal mixer secondary air node is the heat pump outlet node
				if ( PTUnit( PTUnitNum ).AirOutNode != PTUnit( PTUnitNum ).ATMixerSecNode ) {
					ShowSevereError( CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\". heat pump unit air outlet node name must be the same as the air terminal mixer secondary node name." );
					ShowContinueError( "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:SupplySideMixer object." );
					ShowContinueError( "..heat pump unit air outlet node name = " + NodeID( PTUnit( PTUnitNum ).AirOutNode ) );
					ErrorsFound = true;
				}
				// check that the air teminal mixer secondary node is the supplemental heat coil air outlet node
				if ( PTUnit( PTUnitNum ).AirOutNode != SuppHeatOutletNodeNum ) {
					ShowSevereError( CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\". supplemental heating coil air outlet node name must be the same as an air terminal mixer secondary air node name." );
					ShowContinueError( "..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:SupplySideMixer object." );
					ShowContinueError( "..heat pump unit supp heater outlet node name = " + NodeID( SuppHeatOutletNodeNum ) );
					ErrorsFound = true;
				}
			}

			// check connectivity for blow through fan
			if ( PTUnit( PTUnitNum ).FanPlace == BlowThru ) {
				if ( CoolCoilInletNodeNum != FanOutletNodeNum ) { // check that fan outlet equals cooling coil inlet
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "..Fan outlet node name must be the same as the cooling coil inlet node name" );
					ShowContinueError( "..when blow through " + cAlphaFields( 16 ) + " is specified." );
					ShowContinueError( "..Fan outlet node name         = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "..Cooling coil inlet node name = " + NodeID( CoolCoilInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( HeatCoilOutletNodeNum != SuppHeatInletNodeNum ) { // check that heating coil outlet equals supp heating coil inlet
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "..Heating coil outlet node name must be the same as the supplemental heating coil inlet node name" );
					ShowContinueError( "..when blow through " + cAlphaFields( 16 ) + " is specified." );
					ShowContinueError( "..Heating coil outlet node name              = " + NodeID( HeatCoilOutletNodeNum ) );
					ShowContinueError( "..Supplemental heating coil inlet node name  = " + NodeID( SuppHeatInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( ! PTUnit( PTUnitNum ).ATMixerExists && OANodeNums( 4 ) > 0 ) {
					// Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
					if ( OANodeNums( 4 ) != FanInletNodeNum ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "..Fan inlet node name must be the same as the heat pumps OutdoorAir:Mixer mixed air node name" );
						ShowContinueError( "..when blow through " + cAlphaFields( 16 ) + " is specified." );
						ShowContinueError( "..Fan inlet node name                   = " + NodeID( FanInletNodeNum ) );
						ShowContinueError( "..OutdoorAir:Mixer mixed air node name = " + NodeID( OANodeNums( 4 ) ) );
						ErrorsFound = true;
					}
					// OA mixer return node must equal heat pump air inlet node
					if ( PTUnit( PTUnitNum ).AirInNode != OANodeNums( 3 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
						ShowContinueError( "..Heat Pump air inlet node name         = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
						ShowContinueError( "..OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
						ErrorsFound = true;
					}
				}
				if ( OANodeNums( 4 ) == 0 ) {
					// For no OA Mixer fan inlet node name must be the same as the heat pump's inlet air node name
					if ( PTUnit( PTUnitNum ).AirInNode != FanInletNodeNum ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "..Fan inlet node name must be the same as the heat pumps inlet air node name" );
						ShowContinueError( "..when blow through " + cAlphaFields( 16 ) + " is specified and an outdoor air mixer is not used." );
						ShowContinueError( "..Fan inlet node name           = " + NodeID( FanInletNodeNum ) );
						ShowContinueError( "..Heat pump air inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
						ErrorsFound = true;
					}
				}
				if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide ) {
					// check that the air teminal mixer out node is the fan inlet node
					if ( PTUnit( PTUnitNum ).AirInNode != FanInletNodeNum ) {
						ShowSevereError( CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\". fan inlet node name must be the same as an air terminal mixer outlet node name." );
						ShowContinueError( "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:InletSideMixer object." );
						ShowContinueError( "..fan inlet node name = " + NodeID( FanInletNodeNum ) );
						ErrorsFound = true;
					}
				}
			} // end blow through fan IF block

			// check connectivity for draw through fan
			if ( PTUnit( PTUnitNum ).FanPlace == DrawThru ) {
				if ( HeatCoilOutletNodeNum != FanInletNodeNum ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "..Heating coil outlet node name must be the same as the fan inlet node name" );
					ShowContinueError( "..when draw through " + cAlphaFields( 16 ) + " is specified." );
					ShowContinueError( "..Heating coil outlet node name = " + NodeID( HeatCoilOutletNodeNum ) );
					ShowContinueError( "..Fan inlet node name           = " + NodeID( FanInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( SuppHeatInletNodeNum != FanOutletNodeNum ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "..Fan outlet node name must be the same as the supplemental heating coil inlet node name " );
					ShowContinueError( "..when draw through " + cAlphaFields( 16 ) + " is specified." );
					ShowContinueError( "..Fan outlet node = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "..Supplemental heating coil inlet node = " + NodeID( SuppHeatInletNodeNum ) );
					ErrorsFound = true;
				}
				if ( OANodeNums( 4 ) == 0 ) {
					// For no OA mixer, cooling coil inlet node name must be the same as the heat pump's inlet air node name
					if ( CoolCoilInletNodeNum != PTUnit( PTUnitNum ).AirInNode ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "..Heat pump air inlet node name must be the same as the cooling coil inlet node name" );
						ShowContinueError( "..when draw through " + cAlphaFields( 16 ) + " is specified and an outdoor air mixer is not used." );
						ShowContinueError( "..Heat pump air inlet node name = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
						ShowContinueError( "..Cooling coil inlet node name  = " + NodeID( CoolCoilInletNodeNum ) );
						ErrorsFound = true;
					}
				}
				if ( ! PTUnit( PTUnitNum ).ATMixerExists && OANodeNums( 4 ) > 0 ) {
					// Cooling coil inlet node name must be the same as the OA mixers mixed air node name
					if ( CoolCoilInletNodeNum != OANodeNums( 4 ) ) {
						ShowSevereError( CurrentModuleObject + " \"" + PTUnit( PTUnitNum ).Name + "\" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil" );
						ShowContinueError( " inlet node name when draw through " + cAlphaFields( 16 ) + " is specified." );
						ShowContinueError( "..OutdoorAir:Mixer mixed air name = " + NodeID( OANodeNums( 4 ) ) );
						ShowContinueError( "..Cooling coil inlet node name    = " + NodeID( CoolCoilInletNodeNum ) );
						ErrorsFound = true;
					}
					// check OA Mixer return node
					if ( PTUnit( PTUnitNum ).AirInNode != OANodeNums( 3 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
						ShowContinueError( "..Heat Pump air inlet node name         = " + NodeID( PTUnit( PTUnitNum ).AirInNode ) );
						ShowContinueError( "..OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
						ErrorsFound = true;
					}
				}
				if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide ) {
					// check that the air teminal mixer out node is the cooling coil inlet node
					if ( PTUnit( PTUnitNum ).AirInNode != CoolCoilInletNodeNum ) {
						ShowSevereError( CurrentModuleObject + " = \"" + PTUnit( PTUnitNum ).Name + "\". cooling coil inlet node name must be the same as an air terminal mixer outlet node name." );
						ShowContinueError( "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:InletSideMixer object." );
						ShowContinueError( "..cooling coil inlet node name = " + NodeID( CoolCoilInletNodeNum ) );
						ErrorsFound = true;
					}
				}
			} // end draw through IF block

			CompSetFanInlet = NodeID( FanInletNodeNum );
			CompSetFanOutlet = NodeID( FanOutletNodeNum );
			CompSetCoolInlet = NodeID( CoolCoilInletNodeNum );
			CompSetCoolOutlet = NodeID( CoolCoilOutletNodeNum );
			CompSetHeatInlet = NodeID( HeatCoilInletNodeNum );
			CompSetHeatOutlet = NodeID( HeatCoilOutletNodeNum );
			CompSetSupHeatInlet = NodeID( SuppHeatInletNodeNum );
			CompSetSupHeatOutlet = NodeID( SuppHeatOutletNodeNum );

			// Add fan to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).FanType, PTUnit( PTUnitNum ).FanName, CompSetFanInlet, CompSetFanOutlet );

			// Add cooling coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, CompSetCoolInlet, CompSetCoolOutlet );

			// Add heating coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, CompSetHeatInlet, CompSetHeatOutlet );

			// Add supplemental heating coil to component sets array
			SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, SuppHeatCoilType, SuppHeatCoilName, CompSetSupHeatInlet, CompSetSupHeatOutlet );

			if ( PTUnit( PTUnitNum ).UnitType_Num == PTWSHPUnit ) {
				if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
					// Add heating coil water inlet node as actuator node for coil
					TempNodeNum = GetOnlySingleNode( NodeID( PTUnit( PTUnitNum ).HotWaterControlNode ), ErrorsFound, PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsParent );
				} else if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {
					// Add heating coil steam inlet node as actualtor node for coil
					TempNodeNum = GetOnlySingleNode( NodeID( PTUnit( PTUnitNum ).HWCoilSteamInletNode ), ErrorsFound, PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, NodeType_Steam, NodeConnectionType_Actuator, 1, ObjectIsParent );
				}
			}
			if ( OANodeNums( 1 ) > 0 ) {
				// Set up component set for OA mixer - use OA node and Mixed air node
				SetUpCompSets( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).OAMixType, PTUnit( PTUnitNum ).OAMixName, NodeID( OANodeNums( 1 ) ), NodeID( OANodeNums( 4 ) ) );
			}

			//Set the Design Fan Volume Flow Rate
			errFlag = false;
			GetFanVolFlow( PTUnit( PTUnitNum ).FanIndex, FanVolFlow );
			PTUnit( PTUnitNum ).ActualFanVolFlowRate = FanVolFlow;
			if ( errFlag ) {
				ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			//     PTUnit(PTUnitNum)%ActualFanVolFlowRate = MAX(Numbers(1),Numbers(2),Numbers(3))
			if ( FanVolFlow != AutoSize && PTUnit( PTUnitNum ).ActualFanVolFlowRate != AutoSize ) {
				if ( PTUnit( PTUnitNum ).ActualFanVolFlowRate > FanVolFlow ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "... has a Design Fan Flow Rate > Max Fan Volume Flow Rate, should be <=." );
					ShowContinueError( "... Entered value=" + RoundSigDigits( PTUnit( PTUnitNum ).ActualFanVolFlowRate, 2 ) + "... Fan [" + PTUnit( PTUnitNum ).FanType + ':' + PTUnit( PTUnitNum ).FanName + "] Max Value=" + RoundSigDigits( FanVolFlow, 2 ) );
				}
				if ( PTUnit( PTUnitNum ).ActualFanVolFlowRate <= 0.0 ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "... has a Design Fan Flow Rate <= 0.0, it must be >0.0" );
					ShowContinueError( "... Entered value=" + RoundSigDigits( PTUnit( PTUnitNum ).ActualFanVolFlowRate, 2 ) );
					ErrorsFound = true;
				}
			}

			PTUnit( PTUnitNum ).MaxCoolAirVolFlow = Numbers( 1 );
			if ( PTUnit( PTUnitNum ).MaxCoolAirVolFlow <= 0 && PTUnit( PTUnitNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
				ShowContinueError( " illegal value " + cNumericFields( 1 ) + " = " + TrimSigDigits( Numbers( 1 ), 7 ) );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxHeatAirVolFlow = Numbers( 2 );
			if ( PTUnit( PTUnitNum ).MaxHeatAirVolFlow <= 0 && PTUnit( PTUnitNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
				ShowContinueError( " illegal " + cNumericFields( 2 ) + " = " + TrimSigDigits( Numbers( 2 ), 7 ) );
				ErrorsFound = true;
			}

			PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow = Numbers( 3 );
			if ( PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow < 0 && PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
				ShowContinueError( " illegal " + cNumericFields( 3 ) + " = " + TrimSigDigits( Numbers( 3 ), 7 ) );
				ErrorsFound = true;
			}

			//   AirFlowControl only valid if fan opmode = ContFanCycCoil
			if ( PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
				PTUnit( PTUnitNum ).AirFlowControl = UseCompressorOnFlow;
			} else {
				PTUnit( PTUnitNum ).AirFlowControl = UseCompressorOffFlow;
			}

			if ( OANodeNums( 1 ) > 0 ) {
				PTUnit( PTUnitNum ).CoolOutAirVolFlow = Numbers( 4 );
				if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow < 0 && PTUnit( PTUnitNum ).CoolOutAirVolFlow != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( " illegal " + cNumericFields( 4 ) + " = " + TrimSigDigits( Numbers( 4 ), 7 ) );
					ErrorsFound = true;
				}

				//     only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
				if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow > PTUnit( PTUnitNum ).MaxCoolAirVolFlow && PTUnit( PTUnitNum ).CoolOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxCoolAirVolFlow != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( ".." + cNumericFields( 4 ) + " cannot be greater than " + cNumericFields( 1 ) );
					ShowContinueError( ".." + cNumericFields( 1 ) + " = " + TrimSigDigits( Numbers( 1 ), 7 ) );
					ShowContinueError( ".." + cNumericFields( 4 ) + " = " + TrimSigDigits( Numbers( 4 ), 7 ) );
					ErrorsFound = true;
				}

				PTUnit( PTUnitNum ).HeatOutAirVolFlow = Numbers( 5 );
				if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow < 0 && PTUnit( PTUnitNum ).HeatOutAirVolFlow != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( " illegal " + cNumericFields( 5 ) + " = " + TrimSigDigits( Numbers( 5 ), 7 ) );
					ErrorsFound = true;
				}

				//     only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
				if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow > PTUnit( PTUnitNum ).MaxHeatAirVolFlow && PTUnit( PTUnitNum ).HeatOutAirVolFlow != AutoSize && PTUnit( PTUnitNum ).MaxHeatAirVolFlow != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( ".." + cNumericFields( 5 ) + " cannot be greater than " + cNumericFields( 2 ) );
					ShowContinueError( ".." + cNumericFields( 2 ) + " = " + TrimSigDigits( Numbers( 2 ), 7 ) );
					ShowContinueError( ".." + cNumericFields( 5 ) + " = " + TrimSigDigits( Numbers( 5 ), 7 ) );
					ErrorsFound = true;
				}

				PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow = Numbers( 6 );
				if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow < 0 && PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( " illegal " + cNumericFields( 6 ) + " = " + TrimSigDigits( Numbers( 6 ), 7 ) );
					ErrorsFound = true;
				}
			} else {
				PTUnit( PTUnitNum ).CoolOutAirVolFlow = 0.0;
				PTUnit( PTUnitNum ).HeatOutAirVolFlow = 0.0;
				PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow = 0.0;
				if ( ! lNumericBlanks( 4 ) || ! lNumericBlanks( 5 ) || ! lNumericBlanks( 6 ) ) {
					// user entered values for OA with no outdoor air mixer name specified
					PTUnit( PTUnitNum ).CoolOutAirVolFlow = 0.0;
				}
			}

			//Set the heat pump heating coil capacity
			//  Get from coil module.
			if ( PTUnit( PTUnitNum ).DXHeatCoilType_Num == Coil_HeatingWaterToAirHP ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignHeatingCapacity = GetWtoAHPCoilCapacity( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );

				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			} else if ( PTUnit( PTUnitNum ).DXHeatCoilType_Num == Coil_HeatingWaterToAirHPSimple ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignHeatingCapacity = GetWtoAHPSimpleCoilCapacity( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			} else if ( PTUnit( PTUnitNum ).DXHeatCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignHeatingCapacity = GetCoilCapacityVariableSpeed( PTUnit( PTUnitNum ).DXHeatCoilType, PTUnit( PTUnitNum ).DXHeatCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			//Set the heat pump heating coil convergence
			PTUnit( PTUnitNum ).HeatConvergenceTol = 0.001;
			//Set the heat pump cooling coil capacity (Total capacity)
			//  Get from coil module.
			if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == Coil_CoolingWaterToAirHP ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignCoolingCapacity = GetWtoAHPCoilCapacity( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			} else if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == Coil_CoolingWaterToAirHPSimple ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignCoolingCapacity = GetWtoAHPSimpleCoilCapacity( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			} else if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignCoolingCapacity = GetCoilCapacityVariableSpeed( PTUnit( PTUnitNum ).DXCoolCoilType, PTUnit( PTUnitNum ).DXCoolCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			//Set the heat pump cooling coil convergence
			PTUnit( PTUnitNum ).CoolConvergenceTol = 0.001;
			//Set the heatpump cycling rate
			PTUnit( PTUnitNum ).MaxONOFFCyclesperHour = Numbers( 7 );

			//Set the heat pump time constant
			PTUnit( PTUnitNum ).HPTimeConstant = Numbers( 8 );

			//Set the heat pump on-cycle power use fraction
			PTUnit( PTUnitNum ).OnCyclePowerFraction = Numbers( 9 );

			//Set the heat pump fan delay time
			PTUnit( PTUnitNum ).FanDelayTime = Numbers( 10 );

			//Set the heatpump design supplemental heating capacity
			//  Get from coil module.
			if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingGas || PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingElectric ) {
				errFlag = false;
				PTUnit( PTUnitNum ).DesignSuppHeatingCapacity = GetHeatingCoilCapacity( SuppHeatCoilType, SuppHeatCoilName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			//Set the max outlet temperature for supplemental heating coil
			PTUnit( PTUnitNum ).MaxSATSupHeat = Numbers( 11 );

			//Set maximum supply air temperature for supplemental heating coil
			PTUnit( PTUnitNum ).MaxOATSupHeat = Numbers( 12 );

			//set minimum OA temp for WSHP compressor to large negative number (field not used for a WSHP)
			PTUnit( PTUnitNum ).MinOATCompressor = -99999.0;

		} //End of the WaterToAirHeatPump Loop

		//***********************************************************************************

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input." );
			ShowContinueError( "... Preceding condition causes termination." );
		}

		for ( PTUnitNum = 1; PTUnitNum <= NumPTHP; ++PTUnitNum ) {
			// Setup Report variables for the Packaged Terminal Heat Psmps,   CurrentModuleObject = 'ZoneHVAC:PackagedTerminalHeatPump'
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Total Heating Rate [W]", PTUnit( PTUnitNum ).TotHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Total Heating Energy [J]", PTUnit( PTUnitNum ).TotHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Total Cooling Rate [W]", PTUnit( PTUnitNum ).TotCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Total Cooling Energy [J]", PTUnit( PTUnitNum ).TotCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Sensible Heating Rate [W]", PTUnit( PTUnitNum ).SensHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Sensible Heating Energy [J]", PTUnit( PTUnitNum ).SensHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Sensible Cooling Rate [W]", PTUnit( PTUnitNum ).SensCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Sensible Cooling Energy [J]", PTUnit( PTUnitNum ).SensCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Latent Heating Rate [W]", PTUnit( PTUnitNum ).LatHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Latent Heating Energy [J]", PTUnit( PTUnitNum ).LatHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Latent Cooling Rate [W]", PTUnit( PTUnitNum ).LatCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Latent Cooling Energy [J]", PTUnit( PTUnitNum ).LatCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Electric Power [W]", PTUnit( PTUnitNum ).ElecPower, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Electric Energy [J]", PTUnit( PTUnitNum ).ElecConsumption, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Fan Part Load Ratio []", PTUnit( PTUnitNum ).FanPartLoadRatio, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Compressor Part Load Ratio []", PTUnit( PTUnitNum ).CompPartLoadRatio, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Heat Pump Fan Availability Status []", PTUnit( PTUnitNum ).AvailStatus, "System", "Average", PTUnit( PTUnitNum ).Name );
		}

		for ( PTUnitNum = 1 + NumPTHP; PTUnitNum <= NumPTHP + NumPTAC; ++PTUnitNum ) {
			// Setup Report variables for the Packaged Terminal Air Conditioners,
			// CurrentModuleObject = 'ZoneHVAC:PackagedTerminalAirConditioner'
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Total Heating Rate [W]", PTUnit( PTUnitNum ).TotHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Total Heating Energy [J]", PTUnit( PTUnitNum ).TotHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Total Cooling Rate [W]", PTUnit( PTUnitNum ).TotCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Total Cooling Energy [J]", PTUnit( PTUnitNum ).TotCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Sensible Heating Rate [W]", PTUnit( PTUnitNum ).SensHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Sensible Heating Energy [J]", PTUnit( PTUnitNum ).SensHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Sensible Cooling Rate [W]", PTUnit( PTUnitNum ).SensCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Sensible Cooling Energy [J]", PTUnit( PTUnitNum ).SensCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Latent Heating Rate [W]", PTUnit( PTUnitNum ).LatHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Latent Heating Energy [J]", PTUnit( PTUnitNum ).LatHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Latent Cooling Rate [W]", PTUnit( PTUnitNum ).LatCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Latent Cooling Energy [J]", PTUnit( PTUnitNum ).LatCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Electric Power [W]", PTUnit( PTUnitNum ).ElecPower, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Electric Energy [J]", PTUnit( PTUnitNum ).ElecConsumption, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Fan Part Load Ratio []", PTUnit( PTUnitNum ).FanPartLoadRatio, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Compressor Part Load Ratio []", PTUnit( PTUnitNum ).CompPartLoadRatio, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Packaged Terminal Air Conditioner Fan Availability Status []", PTUnit( PTUnitNum ).AvailStatus, "System", "Average", PTUnit( PTUnitNum ).Name );
		}

		for ( PTUnitNum = 1 + NumPTHP + NumPTAC; PTUnitNum <= NumPTUs; ++PTUnitNum ) {
			// Setup Report variables for the Zone Water Source Heat Pumps, CurrentModuleObject='ZoneHVAC:WaterToAirHeatPump'
			SetupOutputVariable( "Zone Water to Air Heat Pump Total Heating Rate [W]", PTUnit( PTUnitNum ).TotHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Total Heating Energy [J]", PTUnit( PTUnitNum ).TotHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Total Cooling Rate [W]", PTUnit( PTUnitNum ).TotCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Total Cooling Energy [J]", PTUnit( PTUnitNum ).TotCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Sensible Heating Rate [W]", PTUnit( PTUnitNum ).SensHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Sensible Heating Energy [J]", PTUnit( PTUnitNum ).SensHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Sensible Cooling Rate [W]", PTUnit( PTUnitNum ).SensCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Sensible Cooling Energy [J]", PTUnit( PTUnitNum ).SensCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Latent Heating Rate [W]", PTUnit( PTUnitNum ).LatHeatEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Latent Heating Energy [J]", PTUnit( PTUnitNum ).LatHeatEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Latent Cooling Rate [W]", PTUnit( PTUnitNum ).LatCoolEnergyRate, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Latent Cooling Energy [J]", PTUnit( PTUnitNum ).LatCoolEnergy, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Electric Power [W]", PTUnit( PTUnitNum ).ElecPower, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Electric Energy [J]", PTUnit( PTUnitNum ).ElecConsumption, "System", "Sum", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Fan Part Load Ratio []", PTUnit( PTUnitNum ).FanPartLoadRatio, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Compressor Part Load Ratio []", PTUnit( PTUnitNum ).CompPartLoadRatio, "System", "Average", PTUnit( PTUnitNum ).Name );
			SetupOutputVariable( "Zone Water to Air Heat Pump Fan Availability Status []", PTUnit( PTUnitNum ).AvailStatus, "System", "Average", PTUnit( PTUnitNum ).Name );
		}
	}

	void
	InitPTUnit(
		int const PTUnitNum, // number of the current PTHP unit being simulated
		int const ZoneNum, // zone number where the current PTHP unit is located
		bool const FirstHVACIteration, // TRUE on first HVAC iteration
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to average airflow over timestep
		Real64 & ZoneLoad // cooling or heating needed by zone [watts]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
		//       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
		//       MODIFIED       Bo Shen, ORNL, July 2012, added variable-speed air-source heat pump
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the packaged terminal heat pump components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataGlobals::InitConvTemp;
		using DataGlobals::AnyPlantInModel;
		using DataEnvironment::StdRhoAir;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );
		using SteamCoils::SimulateSteamCoilComponents;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		auto & GetSteamCoilCapacity( SteamCoils::GetCoilCapacity );
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::SimulateWaterCoilComponents;
		//unused-12/12/08  USE FluidProperties,      ONLY: GetSatDensityRefrig !, FindRefrigerant, FindGlycol
		using DataHeatBalFanSys::TempControlType;
		using Fans::GetFanVolFlow;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using General::TrimSigDigits;
		using DataZoneEquipment::ZoneEquipConfig;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;

		// Locals
		Real64 SupHeaterLoad;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitPTUnit" );
		static std::string const RoutineNameSpace( " InitPTUnit" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // inlet node number in PTHP loop
		int OutNode; // outlet node number in PTHP loop
		int OutsideAirNode; // outside air node number in PTHP loop
		Real64 QZnReq; // cooling or heating needed by zone [watts]
		Real64 RhoAir; // air density at InNode
		Real64 PartLoadFrac; // compressor part load fraction
		Real64 CoilMaxVolFlowRate; // water or steam max volumetric water flow rate
		static bool MyOneTimeFlag( true ); // initialization flag
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		static Array1D_bool MyEnvrnFlag; // used for initializations each begin environment flag
		static Array1D_bool MySizeFlag; // used for sizing PTHP inputs one time
		static Array1D_bool MyFanFlag; // used for sizing PTHP fan inputs one time
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		Real64 QActual; // actual PTAC steam heating coil load met (W)
		bool ErrorsFound; // flag returned from mining call
		Real64 QToCoolSetPt;
		Real64 QToHeatSetPt;
		Real64 NoCompOutput;
		Real64 mdot; // local temporary for mass flow rate (kg/s)
		Real64 rho; // local for fluid density
		int SteamIndex;
		bool errFlag;
		Real64 LatentOutput; // no load latent output (coils off) (W)
		int NumOfSpeedCooling; // Number of speeds for cooling
		int NumOfSpeedHeating; // Number of speeds for heating
		std::string CurrentModuleObject; // Object type for getting and error messages
		int i; // Loop index
		int Iter; // speed iteration count
		int PTObjectIndex;
		Real64 MulSpeedFlowScale; // variable speed air flow scaling factor
		int CtrlZoneNum; // the controlled zone index (index of ZoneEquipConfig)

		InNode = PTUnit( PTUnitNum ).AirInNode;
		OutNode = PTUnit( PTUnitNum ).AirOutNode;
		CtrlZoneNum = 0;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumPTUs );
			MySizeFlag.allocate( NumPTUs );
			MyFanFlag.allocate( NumPTUs );
			MyPlantScanFlag.allocate( NumPTUs );
			MyZoneEqFlag.allocate ( NumPTUs );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyFanFlag = true;
			MyPlantScanFlag = true;
			MyZoneEqFlag = true;
			MyOneTimeFlag = false;

		}

		if ( allocated( ZoneComp ) ) {
			PTObjectIndex = PTUnit( PTUnitNum ).PTObjectIndex;
			if ( MyZoneEqFlag( PTUnitNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( PTUnit( PTUnitNum ).ZoneEquipType ).ZoneCompAvailMgrs( PTObjectIndex ).AvailManagerListName = PTUnit( PTUnitNum ).AvailManagerListName;
				ZoneComp( PTUnit( PTUnitNum ).ZoneEquipType ).ZoneCompAvailMgrs( PTObjectIndex ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( PTUnitNum ) = false;
			}
			PTUnit( PTUnitNum ).AvailStatus = ZoneComp( PTUnit( PTUnitNum ).ZoneEquipType ).ZoneCompAvailMgrs( PTObjectIndex ).AvailStatus;
		}

		if ( MyPlantScanFlag( PTUnitNum ) && allocated( PlantLoop ) ) {
			if ( ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) || ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) ) {
				if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {

					errFlag = false;
					ScanPlantLoopsForObject( PTUnit( PTUnitNum ).ACHeatCoilName, TypeOf_CoilWaterSimpleHeating, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum, _, _, _, _, _, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Reference Unit=\"" + PTUnit( PTUnitNum ).Name + "\", type=" + PTUnit( PTUnitNum ).UnitType );
						ShowFatalError( "InitPTUnit: Program terminated for previous conditions." );
					}

					PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", PTUnit( PTUnitNum ).ACHeatCoilName, ErrorsFound );

					if ( PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						rho = GetDensityGlycol( PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidIndex, RoutineName );

						PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", PTUnit( PTUnitNum ).ACHeatCoilName, ErrorsFound ) * rho;
					}

				} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {

					errFlag = false;
					ScanPlantLoopsForObject( PTUnit( PTUnitNum ).ACHeatCoilName, TypeOf_CoilSteamAirHeating, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum, _, _, _, _, _, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Reference Unit=\"" + PTUnit( PTUnitNum ).Name + "\", type=" + PTUnit( PTUnitNum ).UnitType );
						ShowFatalError( "InitPTUnit: Program terminated for previous conditions." );
					}

					PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).ACHeatCoilIndex, ErrorsFound );

					if ( PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).ACHeatCoilIndex, ErrorsFound ) * SteamDensity;
					}

				}

				//fill outlet node for coil
				PTUnit( PTUnitNum ).PlantCoilOutletNode = PlantLoop( PTUnit( PTUnitNum ).LoopNum ).LoopSide( PTUnit( PTUnitNum ).LoopSide ).Branch( PTUnit( PTUnitNum ).BranchNum ).Comp( PTUnit( PTUnitNum ).CompNum ).NodeNumOut;
				MyPlantScanFlag( PTUnitNum ) = false;

			} else if ( ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) || ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) ) {
				if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {
					errFlag = false;
					ScanPlantLoopsForObject( PTUnit( PTUnitNum ).SuppHeatCoilName, TypeOf_CoilWaterSimpleHeating, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum, _, _, _, _, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitPTUnit: Program terminated for previous conditions." );
					}
					PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, ErrorsFound );

					if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						rho = GetDensityGlycol( PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidIndex, RoutineName );
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, ErrorsFound ) * rho;
					}
				} else if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {
					errFlag = false;
					ScanPlantLoopsForObject( PTUnit( PTUnitNum ).SuppHeatCoilName, TypeOf_CoilSteamAirHeating, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum, _, _, _, _, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitPTUnit: Program terminated for previous conditions." );
					}
					PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, ErrorsFound );
					if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, ErrorsFound ) * SteamDensity;
					}
				}
				//fill outlet node for coil
				PTUnit( PTUnitNum ).PlantCoilOutletNode = PlantLoop( PTUnit( PTUnitNum ).LoopNum ).LoopSide( PTUnit( PTUnitNum ).LoopSide ).Branch( PTUnit( PTUnitNum ).BranchNum ).Comp( PTUnit( PTUnitNum ).CompNum ).NodeNumOut;
				MyPlantScanFlag( PTUnitNum ) = false;
			} else { // pthp not connected to plant
				MyPlantScanFlag( PTUnitNum ) = false;
			}
		} else if ( MyPlantScanFlag( PTUnitNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( PTUnitNum ) = false;
		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumPTUs; ++Loop ) {
				if ( CheckZoneEquipmentList( PTUnit( Loop ).UnitType, PTUnit( Loop ).Name, CtrlZoneNum ) ) {
					// save the ZoneEquipConfig index for this unit
					PTUnit( Loop ).CtrlZoneNum = CtrlZoneNum;
				} else {
					ShowSevereError( "InitPTHP: Packaged Terminal Unit=[" + PTUnit( Loop ).UnitType + ',' + PTUnit( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
				}
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( PTUnitNum ) ) {
			SizePTUnit( PTUnitNum );
			MySizeFlag( PTUnitNum ) = false;
		}

		if ( PTUnit( PTUnitNum ).useVSCoilModel && PTUnit( PTUnitNum ).NumOfSpeedCooling == 0 && ! MySizeFlag( PTUnitNum ) ) {

			SimVariableSpeedCoils( "", PTUnit( PTUnitNum ).DXCoolCoilIndexNum, 0, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, 0, 0.0, 1, 0.0, 0.0, 0.0, 0.0 ); //conduct the sizing operation in the VS WSHP
			PTUnit( PTUnitNum ).NumOfSpeedCooling = VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).NumOfSpeeds;

			MulSpeedFlowScale = VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).RatedAirVolFlowRate / VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).MSRatedAirVolFlowRate( VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).NormSpedLevel );
			for ( Iter = 1; Iter <= PTUnit( PTUnitNum ).NumOfSpeedCooling; ++Iter ) {
				PTUnit( PTUnitNum ).CoolVolumeFlowRate( Iter ) = VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).MSRatedAirVolFlowRate( Iter ) * MulSpeedFlowScale;
				PTUnit( PTUnitNum ).CoolMassFlowRate( Iter ) = VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).MSRatedAirMassFlowRate( Iter ) * MulSpeedFlowScale;
				PTUnit( PTUnitNum ).MSCoolingSpeedRatio( Iter ) = VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).MSRatedAirVolFlowRate( Iter ) / VarSpeedCoil( PTUnit( PTUnitNum ).DXCoolCoilIndexNum ).MSRatedAirVolFlowRate( PTUnit( PTUnitNum ).NumOfSpeedCooling );
			}

			if ( PTUnit( PTUnitNum ).DXHeatCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit || PTUnit( PTUnitNum ).DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) {

				SimVariableSpeedCoils( "", PTUnit( PTUnitNum ).DXHeatCoilIndex, 0, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, 0, 0.0, 1, 0.0, 0.0, 0.0, 0.0 ); //conduct the sizing operation in the VS WSHP

				PTUnit( PTUnitNum ).NumOfSpeedHeating = VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).NumOfSpeeds;

				MulSpeedFlowScale = VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).RatedAirVolFlowRate / VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).MSRatedAirVolFlowRate( VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).NormSpedLevel );
				for ( Iter = 1; Iter <= PTUnit( PTUnitNum ).NumOfSpeedHeating; ++Iter ) {
					PTUnit( PTUnitNum ).HeatVolumeFlowRate( Iter ) = VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).MSRatedAirVolFlowRate( Iter ) * MulSpeedFlowScale;
					PTUnit( PTUnitNum ).HeatMassFlowRate( Iter ) = VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).MSRatedAirMassFlowRate( Iter ) * MulSpeedFlowScale;
					PTUnit( PTUnitNum ).MSHeatingSpeedRatio( Iter ) = VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).MSRatedAirVolFlowRate( Iter ) / VarSpeedCoil( PTUnit( PTUnitNum ).DXHeatCoilIndex ).MSRatedAirVolFlowRate( PTUnit( PTUnitNum ).NumOfSpeedHeating );
				}
			}
			// intialize idle flow

			if ( PTUnit( PTUnitNum ).NumOfSpeedHeating > 0 ) {
				PTUnit( PTUnitNum ).IdleMassFlowRate = min( PTUnit( PTUnitNum ).HeatMassFlowRate( 1 ), PTUnit( PTUnitNum ).CoolMassFlowRate( 1 ) );
				PTUnit( PTUnitNum ).IdleSpeedRatio = min( PTUnit( PTUnitNum ).MSHeatingSpeedRatio( 1 ), PTUnit( PTUnitNum ).MSCoolingSpeedRatio( 1 ) );
				PTUnit( PTUnitNum ).IdleVolumeAirRate = min( PTUnit( PTUnitNum ).HeatVolumeFlowRate( 1 ), PTUnit( PTUnitNum ).CoolVolumeFlowRate( 1 ) );
			} else {
				PTUnit( PTUnitNum ).IdleMassFlowRate = PTUnit( PTUnitNum ).CoolMassFlowRate( 1 );
				PTUnit( PTUnitNum ).IdleSpeedRatio = PTUnit( PTUnitNum ).MSCoolingSpeedRatio( 1 );
				PTUnit( PTUnitNum ).IdleVolumeAirRate = PTUnit( PTUnitNum ).CoolVolumeFlowRate( 1 );
			}

			if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil ) {
				PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow = PTUnit( PTUnitNum ).IdleVolumeAirRate;
				PTUnit( PTUnitNum ).MaxNoCoolHeatAirMassFlow = PTUnit( PTUnitNum ).IdleMassFlowRate;
				PTUnit( PTUnitNum ).NoHeatCoolSpeedRatio = PTUnit( PTUnitNum ).IdleSpeedRatio;
			}
		}

		if ( MyFanFlag( PTUnitNum ) ) {
			if ( PTUnit( PTUnitNum ).ActualFanVolFlowRate != AutoSize ) {
				if ( PTUnit( PTUnitNum ).ActualFanVolFlowRate > 0.0 ) {
					PTUnit( PTUnitNum ).HeatingSpeedRatio = PTUnit( PTUnitNum ).MaxHeatAirVolFlow / PTUnit( PTUnitNum ).ActualFanVolFlowRate;
					PTUnit( PTUnitNum ).CoolingSpeedRatio = PTUnit( PTUnitNum ).MaxCoolAirVolFlow / PTUnit( PTUnitNum ).ActualFanVolFlowRate;
					PTUnit( PTUnitNum ).NoHeatCoolSpeedRatio = PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow / PTUnit( PTUnitNum ).ActualFanVolFlowRate;
				}
				MyFanFlag( PTUnitNum ) = false;
			} else {
				GetFanVolFlow( PTUnit( PTUnitNum ).FanIndex, PTUnit( PTUnitNum ).ActualFanVolFlowRate );
			}
		}

		if ( PTUnit( PTUnitNum ).FanSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).FanSchedPtr ) == 0.0 ) {
				PTUnit( PTUnitNum ).OpMode = CycFanCycCoil;
			} else {
				PTUnit( PTUnitNum ).OpMode = ContFanCycCoil;
			}
		}

		QZnReq = ZoneLoad;

		// Original thermostat control logic
		// Sets initial control based on load - works only for cycling fan systems
		// Constant fan systems will further test the load including the impacts of OA
		// OA can change the load to be met by the PTUnit (this is done later in Init)
		if ( QZnReq > SmallLoad ) {
			HeatingLoad = true;
			CoolingLoad = false;
		} else if ( std::abs( QZnReq ) > SmallLoad ) {
			HeatingLoad = false;
			CoolingLoad = true;
		} else {
			HeatingLoad = false;
			CoolingLoad = false;
		}

		// Initialize the operating PLR (turn coils on if needed, otherwise turn coils off)
		if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).SchedPtr ) > 0.0 ) {
			if ( HeatingLoad || CoolingLoad ) {
				PartLoadFrac = 1.0;
			} else {
				PartLoadFrac = 0.0;
			}
		} else {
			PartLoadFrac = 0.0;
		}

		if ( PTUnit( PTUnitNum ).useVSCoilModel && PTUnit( PTUnitNum ).NumOfSpeedCooling > 0 && ! MySizeFlag( PTUnitNum ) ) { //BoS, variable-speed water source hp
			//PTUnit(PTUnitNum)%IdleMassFlowRate = RhoAir*PTUnit(PTUnitNum)%IdleVolumeAirRate
			NumOfSpeedCooling = PTUnit( PTUnitNum ).NumOfSpeedCooling;
			NumOfSpeedHeating = PTUnit( PTUnitNum ).NumOfSpeedHeating;
			// IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
			if ( PTUnit( PTUnitNum ).CheckFanFlow ) {
				CurrentModuleObject = "ZoneHVAC:PackagedTerminalHeatPump";
				GetFanVolFlow( PTUnit( PTUnitNum ).FanIndex, PTUnit( PTUnitNum ).FanVolFlow );
				if ( PTUnit( PTUnitNum ).FanVolFlow != AutoSize ) {
					//     Check fan versus system supply air flow rates
					if ( PTUnit( PTUnitNum ).FanVolFlow + 1e-10 < PTUnit( PTUnitNum ).CoolVolumeFlowRate( NumOfSpeedCooling ) ) {
						ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( PTUnit( PTUnitNum ).FanVolFlow, 7 ) + " in fan object is less than the MSHP system air flow rate when cooling is required (" + TrimSigDigits( PTUnit( PTUnitNum ).CoolVolumeFlowRate( NumOfSpeedCooling ), 7 ) + ")." );
						ShowContinueError( " The MSHP system flow rate when cooling is required is reset to the fan flow rate and the simulation continues." );
						ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						PTUnit( PTUnitNum ).CoolVolumeFlowRate( NumOfSpeedCooling ) = PTUnit( PTUnitNum ).FanVolFlow;
						// Check flow rates in other speeds and ensure flow rates are not above the max flow rate
						for ( i = NumOfSpeedCooling - 1; i >= 1; --i ) {
							if ( PTUnit( PTUnitNum ).CoolVolumeFlowRate( i ) > PTUnit( PTUnitNum ).CoolVolumeFlowRate( i + 1 ) ) {
								ShowContinueError( " The MSHP system flow rate when cooling is required is reset to the flow rate at higher speed and the simulation continues at Speed" + TrimSigDigits( i ) + '.' );
								ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
								PTUnit( PTUnitNum ).CoolVolumeFlowRate( i ) = PTUnit( PTUnitNum ).CoolVolumeFlowRate( i + 1 );
							}
						}
					}

					if ( PTUnit( PTUnitNum ).NumOfSpeedHeating > 0 ) {
						if ( PTUnit( PTUnitNum ).FanVolFlow + 1e-10 < PTUnit( PTUnitNum ).HeatVolumeFlowRate( NumOfSpeedHeating ) ) {
							ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( PTUnit( PTUnitNum ).FanVolFlow, 7 ) + " in fan object is less than the MSHP system air flow rate when heating is required (" + TrimSigDigits( PTUnit( PTUnitNum ).HeatVolumeFlowRate( NumOfSpeedHeating ), 7 ) + ")." );
							ShowContinueError( " The MSHP system flow rate when heating is required is reset to the fan flow rate and the simulation continues." );
							ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
							PTUnit( PTUnitNum ).HeatVolumeFlowRate( NumOfSpeedHeating ) = PTUnit( PTUnitNum ).FanVolFlow;
							for ( i = NumOfSpeedHeating - 1; i >= 1; --i ) {
								if ( PTUnit( PTUnitNum ).HeatVolumeFlowRate( i ) > PTUnit( PTUnitNum ).HeatVolumeFlowRate( i + 1 ) ) {
									ShowContinueError( " The MSHP system flow rate when heating is required is reset to the flow rate at higher speed and the simulation continues at Speed" + TrimSigDigits( i ) + '.' );
									ShowContinueError( " Occurs in " + CurrentModuleObject + " system = " + PTUnit( PTUnitNum ).Name );
									PTUnit( PTUnitNum ).HeatVolumeFlowRate( i ) = PTUnit( PTUnitNum ).HeatVolumeFlowRate( i + 1 );
								}
							}
						}
					}

					if ( PTUnit( PTUnitNum ).FanVolFlow < PTUnit( PTUnitNum ).IdleVolumeAirRate && PTUnit( PTUnitNum ).IdleVolumeAirRate != 0.0 ) {
						ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( PTUnit( PTUnitNum ).FanVolFlow, 7 ) + " in fan object is less than the MSHP system air flow rate when no heating or cooling is needed (" + TrimSigDigits( PTUnit( PTUnitNum ).IdleVolumeAirRate, 7 ) + ")." );
						ShowContinueError( " The MSHP system flow rate when no heating or cooling is needed is reset to the fan flow rate and the simulation continues." );
						ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + PTUnit( PTUnitNum ).Name );
						PTUnit( PTUnitNum ).IdleVolumeAirRate = PTUnit( PTUnitNum ).FanVolFlow;
					}
					RhoAir = StdRhoAir;
					// set the mass flow rates from the reset volume flow rates
					for ( i = 1; i <= NumOfSpeedCooling; ++i ) {
						PTUnit( PTUnitNum ).CoolMassFlowRate( i ) = RhoAir * PTUnit( PTUnitNum ).CoolVolumeFlowRate( i );
						if ( PTUnit( PTUnitNum ).FanVolFlow > 0.0 ) {
							PTUnit( PTUnitNum ).MSCoolingSpeedRatio( i ) = PTUnit( PTUnitNum ).CoolVolumeFlowRate( i ) / PTUnit( PTUnitNum ).FanVolFlow;
						}
					}
					for ( i = 1; i <= NumOfSpeedHeating; ++i ) {
						PTUnit( PTUnitNum ).HeatMassFlowRate( i ) = RhoAir * PTUnit( PTUnitNum ).HeatVolumeFlowRate( i );
						if ( PTUnit( PTUnitNum ).FanVolFlow > 0.0 ) {
							PTUnit( PTUnitNum ).MSHeatingSpeedRatio( i ) = PTUnit( PTUnitNum ).HeatVolumeFlowRate( i ) / PTUnit( PTUnitNum ).FanVolFlow;
						}
					}
					PTUnit( PTUnitNum ).IdleMassFlowRate = RhoAir * PTUnit( PTUnitNum ).IdleVolumeAirRate;
					if ( PTUnit( PTUnitNum ).FanVolFlow > 0.0 ) {
						PTUnit( PTUnitNum ).IdleSpeedRatio = PTUnit( PTUnitNum ).IdleVolumeAirRate / PTUnit( PTUnitNum ).FanVolFlow;
					}
					// set the node max and min mass flow rates based on reset volume flow rates
					if ( PTUnit( PTUnitNum ).NumOfSpeedHeating > 0 ) {
						Node( InNode ).MassFlowRateMax = max( PTUnit( PTUnitNum ).CoolMassFlowRate( NumOfSpeedCooling ), PTUnit( PTUnitNum ).HeatMassFlowRate( NumOfSpeedHeating ) );
						Node( InNode ).MassFlowRateMaxAvail = max( PTUnit( PTUnitNum ).CoolMassFlowRate( NumOfSpeedCooling ), PTUnit( PTUnitNum ).HeatMassFlowRate( NumOfSpeedHeating ) );
					} else {
						Node( InNode ).MassFlowRateMax = PTUnit( PTUnitNum ).CoolMassFlowRate( NumOfSpeedCooling );
						Node( InNode ).MassFlowRateMaxAvail = PTUnit( PTUnitNum ).CoolMassFlowRate( NumOfSpeedCooling );
					}

					Node( InNode ).MassFlowRateMin = 0.0;
					Node( InNode ).MassFlowRateMinAvail = 0.0;
					Node( OutNode ) = Node( InNode );
				}
			}

			PTUnit( PTUnitNum ).CheckFanFlow = false;

			SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );

			//CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, &
			//               ZoneEquipConfig(ZoneNum)%AirLoopNum, OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
		} else {
			SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( PTUnitNum ) ) {
			InNode = PTUnit( PTUnitNum ).AirInNode;
			OutNode = PTUnit( PTUnitNum ).AirOutNode;
			OutsideAirNode = PTUnit( PTUnitNum ).OutsideAirNode;
			RhoAir = StdRhoAir;
			// set the mass flow rates from the input volume flow rates
			PTUnit( PTUnitNum ).MaxCoolAirMassFlow = RhoAir * PTUnit( PTUnitNum ).MaxCoolAirVolFlow;
			PTUnit( PTUnitNum ).CoolOutAirMassFlow = RhoAir * PTUnit( PTUnitNum ).CoolOutAirVolFlow;
			PTUnit( PTUnitNum ).MaxHeatAirMassFlow = RhoAir * PTUnit( PTUnitNum ).MaxHeatAirVolFlow;
			PTUnit( PTUnitNum ).HeatOutAirMassFlow = RhoAir * PTUnit( PTUnitNum ).HeatOutAirVolFlow;
			PTUnit( PTUnitNum ).MaxNoCoolHeatAirMassFlow = RhoAir * PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow;
			PTUnit( PTUnitNum ).NoCoolHeatOutAirMassFlow = RhoAir * PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow;
			// set the node max and min mass flow rates
			// outside air mixer is optional, check that node num > 0
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRateMax = max( PTUnit( PTUnitNum ).CoolOutAirMassFlow, PTUnit( PTUnitNum ).HeatOutAirMassFlow );
				Node( OutsideAirNode ).MassFlowRateMin = 0.0;
				Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			}
			Node( OutNode ).MassFlowRateMax = max( PTUnit( PTUnitNum ).MaxCoolAirMassFlow, PTUnit( PTUnitNum ).MaxHeatAirMassFlow );
			Node( OutNode ).MassFlowRateMin = 0.0;
			Node( OutNode ).MassFlowRateMinAvail = 0.0;
			Node( InNode ).MassFlowRateMax = max( PTUnit( PTUnitNum ).MaxCoolAirMassFlow, PTUnit( PTUnitNum ).MaxHeatAirMassFlow );
			Node( InNode ).MassFlowRateMin = 0.0;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
			if ( PTUnit( PTUnitNum ).AirReliefNode > 0 ) {
				Node( PTUnit( PTUnitNum ).AirReliefNode ).MassFlowRateMinAvail = 0.0;
			}
			MyEnvrnFlag( PTUnitNum ) = false;
			PTUnit( PTUnitNum ).LastMode = HeatingMode;

			//   set fluid-side hardware limits
			if ( PTUnit( PTUnitNum ).HotWaterControlNode > 0 ) {
				// If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
				if ( PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow == AutoSize ) {
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex );
					CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", PTUnit( PTUnitNum ).ACHeatCoilName, ErrorsFound );
					if ( CoilMaxVolFlowRate != AutoSize ) {

						rho = GetDensityGlycol( PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidIndex, RoutineNameSpace );
						PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;

					}
				}

				InitComponentNodes( 0.0, PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

				if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow == AutoSize ) {
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex );
					CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", PTUnit( PTUnitNum ).SuppHeatCoilName, ErrorsFound );
					if ( CoilMaxVolFlowRate != AutoSize ) {
						rho = GetDensityGlycol( PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PTUnit( PTUnitNum ).LoopNum ).FluidIndex, RoutineNameSpace );
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
					}
				}
				InitComponentNodes( 0.0, PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

			}
			if ( PTUnit( PTUnitNum ).HWCoilSteamInletNode > 0 ) {
				//     If steam coil max steam flow rate is autosized, simulate once in order to mine max steam flow rate
				if ( PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow == AutoSize ) {
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
					CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).ACHeatCoilIndex, ErrorsFound );

					if ( CoilMaxVolFlowRate != AutoSize ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
					}
					InitComponentNodes( 0.0, PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
				}

				if ( PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow == AutoSize ) {
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
					CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( PTUnit( PTUnitNum ).SuppHeatCoilIndex, ErrorsFound );

					if ( CoilMaxVolFlowRate != AutoSize ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
					}
					InitComponentNodes( 0.0, PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
				}
			}
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( PTUnitNum ) = true;
		}

		if ( PTUnit( PTUnitNum ).ACHeatCoilCap == AutoSize ) {
			if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingGas ) {
				PTUnit( PTUnitNum ).ACHeatCoilCap = GetHeatingCoilCapacity( PTUnit( PTUnitNum ).ACHeatCoilType, PTUnit( PTUnitNum ).ACHeatCoilName, ErrorsFound );
			} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingElectric ) {
				PTUnit( PTUnitNum ).ACHeatCoilCap = GetHeatingCoilCapacity( PTUnit( PTUnitNum ).ACHeatCoilType, PTUnit( PTUnitNum ).ACHeatCoilName, ErrorsFound );
			}
		}

		// Constant fan systems are tested for ventilation load to determine if load to be met changes.

		if ( ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil || PTUnit( PTUnitNum ).ATMixerExists ) && GetCurrentScheduleValue( PTUnit( PTUnitNum ).SchedPtr ) > 0.0 && ( ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).FanAvailSchedPtr ) > 0.0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOff ) ) {

			SupHeaterLoad = 0.0;
			if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
				CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, Off, 1, 0.0, 0.0, NoCompOutput, LatentOutput, QZnReq, 0.0, OnOffAirFlowRatio, SupHeaterLoad, false );
			} else {
				CalcPTUnit( PTUnitNum, FirstHVACIteration, 0.0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, false );
			}

			QToCoolSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
			QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;

			//   If the PTUnit has a net cooling capacity (NoCompOutput < 0) and
			//   the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
			if ( NoCompOutput < 0.0 && QToHeatSetPt < 0.0 ) {
				if ( NoCompOutput < QToHeatSetPt ) {
					//       If the net cooling capacity overshoots the heating setpoint, change mode
					QZnReq = QToHeatSetPt;
					CoolingLoad = false;
					//       Don't set mode TRUE unless mode is allowed. Also check for floating zone.
					if ( TempControlType( ZoneNum ) == SingleCoolingSetPoint || TempControlType( ZoneNum ) == 0 ) {
						HeatingLoad = false;
					} else {
						HeatingLoad = true;
					}
					PartLoadFrac = 1.0;
					if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
						//CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, &
						//      ZoneEquipConfig(ZoneNum)%AirLoopNum, OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, Off, 1, 0.0, 0.0, NoCompOutput, LatentOutput, QZnReq, 0.0, OnOffAirFlowRatio, SupHeaterLoad, false );
					} else {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						CalcPTUnit( PTUnitNum, FirstHVACIteration, 0.0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, false );
					}
					if ( NoCompOutput > QToHeatSetPt ) {
						//         If changing operating mode (flow rates) does not overshoot heating setpoint, turn off coil
						QZnReq = 0.0;
						HeatingLoad = false;
						PartLoadFrac = 0.0;
						if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
							SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
							//CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
							//       OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
						} else {
							SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						}
					}
				} else if ( NoCompOutput < QZnReq ) {
					//       If the net cooling capacity meets the zone cooling load but does not overshoot heating set piont, turn off coil
					QZnReq = 0.0;
					CoolingLoad = false;
					PartLoadFrac = 0.0;
					if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						//CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
						//         OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
					} else {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
					}
				}
			}
			//   If the furnace has a net heating capacity and the zone temp is below the Tstat cooling setpoint
			if ( NoCompOutput > 0.0 && QToCoolSetPt > 0.0 ) {
				if ( NoCompOutput > QToCoolSetPt ) {
					QZnReq = QToCoolSetPt;
					//       Don't set mode TRUE unless mode is allowed. Also check for floating zone.
					if ( TempControlType( ZoneNum ) == SingleHeatingSetPoint || TempControlType( ZoneNum ) == 0 ) {
						CoolingLoad = false;
					} else {
						CoolingLoad = true;
					}
					HeatingLoad = false;
					PartLoadFrac = 1.0;
					if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						// CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
						//     OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
						CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, Off, 1, 0.0, 0.0, NoCompOutput, LatentOutput, QZnReq, 0.0, OnOffAirFlowRatio, SupHeaterLoad, false );
					} else {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						CalcPTUnit( PTUnitNum, FirstHVACIteration, 0.0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, false );
					}

					if ( NoCompOutput < QToCoolSetPt ) {
						//         If changing operating mode (flow rates) does not overshoot cooling setpoint, turn off coil
						QZnReq = 0.0;
						CoolingLoad = false;
						PartLoadFrac = 0.0;
						if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
							SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
							//CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
							//      OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
						} else {
							SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						}
					}
				} else if ( NoCompOutput > QZnReq ) {
					//       If the net heating capacity meets the zone heating load but does not overshoot, turn off coil
					QZnReq = 0.0;
					HeatingLoad = false;
					PartLoadFrac = 0.0;
					if ( PTUnit( PTUnitNum ).useVSCoilModel ) {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
						//CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
						//     OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
					} else {
						SetOnOffMassFlowRate( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );
					}
				}
			}
			ZoneLoad = QZnReq;
		}

		// get operating capacity of water and steam coil (dependent on entering water/steam temperature)
		if ( FirstHVACIteration && PartLoadFrac > 0.0 ) {

			if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {

				//     set water-side mass flow rates
				Node( PTUnit( PTUnitNum ).HWCoilAirInletNode ).MassFlowRate = CompOnMassFlow;

				mdot = PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow;

				SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

				//     simulate water coil to find operating capacity
				SimulateWaterCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QActual );
				PTUnit( PTUnitNum ).ACHeatCoilCap = QActual;

			} // from IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) THEN

			if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				Node( PTUnit( PTUnitNum ).HWCoilAirInletNode ).MassFlowRate = CompOnMassFlow;

				mdot = PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow;
				SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
				PTUnit( PTUnitNum ).ACHeatCoilCap = GetSteamCoilCapacity( PTUnit( PTUnitNum ).ACHeatCoilType, PTUnit( PTUnitNum ).ACHeatCoilName, ErrorsFound );

			} // from IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) THEN

			if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingWater ) {

				//     set air-side and steam-side mass flow rates
				Node( PTUnit( PTUnitNum ).SupCoilAirInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
				SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

				//     simulate water coil to find operating capacity
				SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual );
				PTUnit( PTUnitNum ).SupHeatCoilCap = QActual;

			} // from IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
			if ( PTUnit( PTUnitNum ).SuppHeatCoilType_Num == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				Node( PTUnit( PTUnitNum ).SupCoilAirInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
				SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, 1.0, QActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
				PTUnit( PTUnitNum ).SupHeatCoilCap = GetSteamCoilCapacity( PTUnit( PTUnitNum ).SuppHeatCoilType, PTUnit( PTUnitNum ).SuppHeatCoilName, ErrorsFound );

			} // from IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
		} // from IF(FirstHVACIteration .AND. PartLoadFrac > 0.0) THEN

		SetAverageAirFlow( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );

	}

	void
	SetOnOffMassFlowRate(
		int const PTUnitNum, // number of the current PTHP unit being simulated
		Real64 const PartLoadFrac, // coil operating part-load ratio
		Real64 & OnOffAirFlowRatio // ratio of coil on to coil off air flow rate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   November 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the operating flow rates.

		// METHODOLOGY EMPLOYED:
		// Set cooling or heating and no cooling or heating flow rate.
		// Set mass flow rate using PLR and call to Subroutine SetAverageAirFlow.

		// REFERENCES:
		// na

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
		// na

		// Set the operating air mass flow rate
		if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil ) {
			// constant fan mode
			if ( HeatingLoad ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).MaxHeatAirMassFlow;
				CompOnFlowRatio = PTUnit( PTUnitNum ).HeatingSpeedRatio;
				OACompOnMassFlow = PTUnit( PTUnitNum ).HeatOutAirMassFlow;
				PTUnit( PTUnitNum ).LastMode = HeatingMode;
			} else if ( CoolingLoad ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).MaxCoolAirMassFlow;
				CompOnFlowRatio = PTUnit( PTUnitNum ).CoolingSpeedRatio;
				OACompOnMassFlow = PTUnit( PTUnitNum ).CoolOutAirMassFlow;
				PTUnit( PTUnitNum ).LastMode = CoolingMode;
			} else {
				CompOnMassFlow = PTUnit( PTUnitNum ).MaxNoCoolHeatAirMassFlow;
				CompOnFlowRatio = PTUnit( PTUnitNum ).NoHeatCoolSpeedRatio;
				OACompOnMassFlow = PTUnit( PTUnitNum ).NoCoolHeatOutAirMassFlow;
			}

			if ( PTUnit( PTUnitNum ).AirFlowControl == UseCompressorOnFlow ) {
				if ( PTUnit( PTUnitNum ).LastMode == HeatingMode ) {
					CompOffMassFlow = PTUnit( PTUnitNum ).MaxHeatAirMassFlow;
					CompOffFlowRatio = PTUnit( PTUnitNum ).HeatingSpeedRatio;
					OACompOffMassFlow = PTUnit( PTUnitNum ).HeatOutAirMassFlow;
				} else {
					CompOffMassFlow = PTUnit( PTUnitNum ).MaxCoolAirMassFlow;
					CompOffFlowRatio = PTUnit( PTUnitNum ).CoolingSpeedRatio;
					OACompOffMassFlow = PTUnit( PTUnitNum ).CoolOutAirMassFlow;
				}
			} else {
				CompOffMassFlow = PTUnit( PTUnitNum ).MaxNoCoolHeatAirMassFlow;
				CompOffFlowRatio = PTUnit( PTUnitNum ).NoHeatCoolSpeedRatio;
				OACompOffMassFlow = PTUnit( PTUnitNum ).NoCoolHeatOutAirMassFlow;
			}
		} else {
			// cycling fan mode
			if ( HeatingLoad ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).MaxHeatAirMassFlow;
				CompOnFlowRatio = PTUnit( PTUnitNum ).HeatingSpeedRatio;
				OACompOnMassFlow = PTUnit( PTUnitNum ).HeatOutAirMassFlow;
			} else if ( CoolingLoad ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).MaxCoolAirMassFlow;
				CompOnFlowRatio = PTUnit( PTUnitNum ).CoolingSpeedRatio;
				OACompOnMassFlow = PTUnit( PTUnitNum ).CoolOutAirMassFlow;
			} else {
				CompOnMassFlow = 0.0;
				CompOnFlowRatio = 0.0;
				OACompOnMassFlow = 0.0;
			}
			CompOffMassFlow = 0.0;
			CompOffFlowRatio = 0.0;
			OACompOffMassFlow = 0.0;
		}

		SetAverageAirFlow( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );

	}

	void
	SizePTUnit( int const PTUnitNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
		//                      August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing packaged terminal heat pump components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays. ParentCoolAirFlowSizing and ParentHeatAirFlowSizing
		// arrays are used to pass volumetric flow rates to child objects when zone sizing array values are overridden.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using WaterCoils::SetCoilDesFlow;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		bool IsAutoSize; // Indicator to autosize
		Real64 MaxCoolAirVolFlowDes; // Autosized cooling air flow for reporting
		Real64 MaxCoolAirVolFlowUser; // Hardsizedcooling air flow for reporting
		Real64 MaxHeatAirVolFlowDes; // Autosized heating air flow for reporting
		Real64 MaxHeatAirVolFlowUser; // Hardsized heating air flow for reporting
		Real64 MaxNoCoolHeatAirVolFlowDes; // Autosized maximum air flow when unconditioned for reporting
		Real64 MaxNoCoolHeatAirVolFlowUser; // Hardsized maximum air flow when unconditioned for reporting
		Real64 CoolOutAirVolFlowDes; // Autosized cooling outdoor air flow for reporting
		Real64 CoolOutAirVolFlowUser; // Hardsized cooling outdoor air flow for reporting
		Real64 HeatOutAirVolFlowDes; // Autosized heating outdoor air flow for reporting
		Real64 HeatOutAirVolFlowUser; // Hardsized heating outdoor air flow for reporting
		Real64 NoCoolHeatOutAirVolFlowDes; // Autosized outdoor air flow when unconditioned for reporting
		Real64 NoCoolHeatOutAirVolFlowUser; // Hardsized outdoor air flow when unconditioned for reporting
		Real64 MaxSATSupHeatDes; // Autosized supply air temperature of supplemental heater for reporting
		Real64 MaxSATSupHeatUser; // Hardsized supply air temperature of supplemental heater for reporting

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("SizePTUnit: "); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ErrorsFound;
		bool SizingDesRunThisZone; // true if a particular zone had a Sizing:Zone object and zone sizing was done


		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		ErrorsFound = false;
		IsAutoSize = false;
		MaxCoolAirVolFlowDes = 0.0;
		MaxCoolAirVolFlowUser = 0.0;
		MaxHeatAirVolFlowDes = 0.0;
		MaxHeatAirVolFlowUser = 0.0;
		MaxNoCoolHeatAirVolFlowDes = 0.0;
		MaxNoCoolHeatAirVolFlowUser = 0.0;
		CoolOutAirVolFlowDes = 0.0;
		CoolOutAirVolFlowUser = 0.0;
		HeatOutAirVolFlowDes = 0.0;
		HeatOutAirVolFlowUser = 0.0;
		NoCoolHeatOutAirVolFlowDes = 0.0;
		NoCoolHeatOutAirVolFlowUser = 0.0;
		MaxSATSupHeatDes = 0.0;
		MaxSATSupHeatUser = 0.0;

		DataFracOfAutosizedCoolingAirflow = 1.0;
		DataFracOfAutosizedHeatingAirflow = 1.0;
		DataFracOfAutosizedCoolingCapacity = 1.0;
		DataFracOfAutosizedHeatingCapacity = 1.0;

		DataScalableSizingON = false;
		DataScalableCapSizingON = false;
		CompType = PTUnit( PTUnitNum ).UnitType;
		CompName = PTUnit( PTUnitNum ).Name;
		DataZoneNumber = PTUnit( PTUnitNum ).ZonePtr;

		if ( CurZoneEqNum > 0 ) {
			if ( PTUnit( PTUnitNum ).HVACSizingIndex > 0 ) {
				zoneHVACIndex = PTUnit( PTUnitNum ).HVACSizingIndex;
				SizingMethod = CoolingAirflowSizing;
				FieldNum = 1; // N1, \field Supply Air Flow Rate During Cooling Operation
				SizingString = PTUnitUNumericFields( PTUnitNum ).FieldNames( FieldNum ) + " [m3/s]";
				PrintFlag = true;
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					PTUnit( PTUnitNum ).MaxCoolAirVolFlow = TempSize;
				} else if ( SAFMethod == FlowPerCoolingCapacity ) {
					SizingMethod = CoolingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
					if ( ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod == FractionOfAutosizedCoolingCapacity ) {
						DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DataAutosizedCoolingCapacity = TempSize;
					DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					SizingMethod = CoolingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					PTUnit( PTUnitNum ).MaxCoolAirVolFlow = TempSize;
				}

				SizingMethod = HeatingAirflowSizing;
				FieldNum = 2; //N2, \field Supply Air Flow Rate During Heating Operation
				PrintFlag = true;
				SizingString = PTUnitUNumericFields( PTUnitNum ).FieldNames( FieldNum ) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					PTUnit( PTUnitNum ).MaxHeatAirVolFlow = TempSize;
				} else if ( SAFMethod == FlowPerHeatingCapacity ) {
					SizingMethod = HeatingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					if ( ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod == FractionOfAutosizedHeatingCapacity ) {
						DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
					}
					DataAutosizedHeatingCapacity = TempSize;
					DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					SizingMethod = HeatingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					PTUnit( PTUnitNum ).MaxHeatAirVolFlow = TempSize;
				}

				SizingMethod = SystemAirflowSizing;
				FieldNum = 3; //N3, \field Supply Air Flow Rate When No Cooling or Heating is Needed
				PrintFlag = true;
				SizingString = PTUnitUNumericFields( PTUnitNum ).FieldNames( FieldNum ) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).NoCoolHeatSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow = TempSize;
				}

				// initialize capacity sizing variables: cooling
				SizingMethod = CoolingCapacitySizing;
				CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
				if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
					if ( CapSizingMethod == CoolingDesignCapacity ) {
						if ( ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						}
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
						ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity * Zone( DataZoneNumber ).FloorArea;
						DataScalableCapSizingON = true;
					} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
						DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						DataScalableCapSizingON = true;
					}
				}

				// initialize capacity sizing variables: heating
				SizingMethod = HeatingCapacitySizing;
				CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
				if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
					if ( CapSizingMethod == HeatingDesignCapacity ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
						}
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
					} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
					}
				}
			} else {
				// no scalble sizing method has been specified. Sizing proceeds using the method
				// specified in the zoneHVAC object

				PrintFlag = true;
				SizingMethod = CoolingAirflowSizing;
				FieldNum = 1; // N1, \field Supply Air Flow Rate During Cooling Operation
				SizingString = PTUnitUNumericFields( PTUnitNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = PTUnit( PTUnitNum ).MaxCoolAirVolFlow;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				PTUnit( PTUnitNum ).MaxCoolAirVolFlow = TempSize;

				SizingMethod = HeatingAirflowSizing;
				FieldNum = 2; //N2, \field Supply Air Flow Rate During Heating Operation
				SizingString = PTUnitUNumericFields( PTUnitNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = PTUnit( PTUnitNum ).MaxHeatAirVolFlow;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				PTUnit( PTUnitNum ).MaxHeatAirVolFlow = TempSize;

				SizingMethod = SystemAirflowSizing;
				FieldNum = 3; //N3, \field Supply Air Flow Rate When No Cooling or Heating is Needed
				SizingString = PTUnitUNumericFields( PTUnitNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow = TempSize;
			}
		}

		IsAutoSize = false;
		if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
			if ( ! IsAutoSize && ! SizingDesRunThisZone ) { // Simulation continue
				if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]", PTUnit( PTUnitNum ).CoolOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name );
				CoolOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, PTUnit( PTUnitNum ).MaxCoolAirVolFlow );
				if ( CoolOutAirVolFlowDes < SmallAirVolFlow ) {
					CoolOutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					PTUnit( PTUnitNum ).CoolOutAirVolFlow = CoolOutAirVolFlowDes;
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]", CoolOutAirVolFlowDes );
				} else {
					if ( PTUnit( PTUnitNum ).CoolOutAirVolFlow > 0.0 && CoolOutAirVolFlowDes > 0.0 && SizingDesRunThisZone ) {
						CoolOutAirVolFlowUser = PTUnit( PTUnitNum ).CoolOutAirVolFlow;
						ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]", CoolOutAirVolFlowDes, "User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]", CoolOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( CoolOutAirVolFlowDes - CoolOutAirVolFlowUser ) / CoolOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePTUnit: Potential issue with equipment sizing for " + PTUnit( PTUnitNum ).UnitType + ' ' + PTUnit( PTUnitNum ).Name );
								ShowContinueError( "User-Specified Outdoor Air Flow Rate During Cooling Operation of " + RoundSigDigits( CoolOutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Outdoor Air Flow Rate During Cooling Operation of " + RoundSigDigits( CoolOutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
			if ( ! IsAutoSize && ! SizingDesRunThisZone ) { // Simulation continue
				if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "User-Specified Supply Air Flow Rate During Heating Operation [m3/s]", PTUnit( PTUnitNum ).HeatOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name );
				HeatOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, PTUnit( PTUnitNum ).MaxHeatAirVolFlow );
				if ( HeatOutAirVolFlowDes < SmallAirVolFlow ) {
					HeatOutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					PTUnit( PTUnitNum ).HeatOutAirVolFlow = HeatOutAirVolFlowDes;
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]", HeatOutAirVolFlowDes );
				} else {
					if ( PTUnit( PTUnitNum ).HeatOutAirVolFlow > 0.0 && HeatOutAirVolFlowDes > 0.0 && SizingDesRunThisZone ) {
						HeatOutAirVolFlowUser = PTUnit( PTUnitNum ).HeatOutAirVolFlow;
						ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]", HeatOutAirVolFlowDes, "User-Specified Outdoor Air Flow Rate During Heating Operation [m3/s]", HeatOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( HeatOutAirVolFlowDes - HeatOutAirVolFlowUser ) / HeatOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePTUnit: Potential issue with equipment sizing for " + PTUnit( PTUnitNum ).UnitType + ' ' + PTUnit( PTUnitNum ).Name );
								ShowContinueError( "User-Specified Outdoor Air Flow Rate During Heating Operation of " + RoundSigDigits( HeatOutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Outdoor Air Flow Rate During Heating Operation of " + RoundSigDigits( HeatOutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
			if ( ! IsAutoSize && ! SizingDesRunThisZone ) { // Simulation continue
				if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name );
				NoCoolHeatOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, PTUnit( PTUnitNum ).MaxNoCoolHeatAirVolFlow );
				if ( NoCoolHeatOutAirVolFlowDes < SmallAirVolFlow ) {
					NoCoolHeatOutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow = NoCoolHeatOutAirVolFlowDes;
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", NoCoolHeatOutAirVolFlowDes );
				} else {
					if ( PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow > 0.0 && NoCoolHeatOutAirVolFlowDes > 0.0 && SizingDesRunThisZone ) {
						NoCoolHeatOutAirVolFlowUser = PTUnit( PTUnitNum ).NoCoolHeatOutAirVolFlow;
						ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", NoCoolHeatOutAirVolFlowDes, "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", NoCoolHeatOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( NoCoolHeatOutAirVolFlowDes - NoCoolHeatOutAirVolFlowUser ) / NoCoolHeatOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePTUnit: Potential issue with equipment sizing for " + PTUnit( PTUnitNum ).UnitType + ' ' + PTUnit( PTUnitNum ).Name );
								ShowContinueError( "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed of " + RoundSigDigits( NoCoolHeatOutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed of " + RoundSigDigits( NoCoolHeatOutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PTUnit( PTUnitNum ).MaxSATSupHeat == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
			if ( ! IsAutoSize && ! SizingDesRunThisZone ) { // Simulation continue
				if ( PTUnit( PTUnitNum ).MaxSATSupHeat > 0.0 ) {
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "User-Specified Maximum Supply Air Temperature from Supplemental Heater [C]", PTUnit( PTUnitNum ).MaxSATSupHeat );
				}
			} else {
				CheckZoneSizing( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name );
				MaxSATSupHeatDes = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
				if ( IsAutoSize ) {
					PTUnit( PTUnitNum ).MaxSATSupHeat = MaxSATSupHeatDes;
					ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Maximum Supply Air Temperature from Supplemental Heater [C]", MaxSATSupHeatDes );
				} else {
					if ( PTUnit( PTUnitNum ).MaxSATSupHeat > 0.0 && MaxSATSupHeatDes > 0.0 && SizingDesRunThisZone ) {
						MaxSATSupHeatUser = PTUnit( PTUnitNum ).MaxSATSupHeat;
						ReportSizingOutput( PTUnit( PTUnitNum ).UnitType, PTUnit( PTUnitNum ).Name, "Design Size Maximum Supply Air Temperature from Supplemental Heater [C]", MaxSATSupHeatDes, "User-Specified Maximum Supply Air Temperature from Supplemental Heater [C]", MaxSATSupHeatUser );
						if ( DisplayExtraWarnings ) {
							if ( std::abs( MaxSATSupHeatDes - MaxSATSupHeatUser ) > ( 4.0 * AutoVsHardSizingDeltaTempThreshold ) ) {
								ShowMessage( "SizePTUnit: Potential issue with equipment sizing for " + PTUnit( PTUnitNum ).UnitType + ' ' + PTUnit( PTUnitNum ).Name );
								ShowContinueError( "User-Specified Maximum Supply Air Temperature from Supplemental Heater of " + RoundSigDigits( MaxSATSupHeatUser, 2 ) + " [C]" );
								ShowContinueError( "differs from Design Size Maximum Supply Air Temperature from Supplemental Heater of " + RoundSigDigits( MaxSATSupHeatDes, 2 ) + " [C]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		SetCoilDesFlow( PTUnit( PTUnitNum ).ACHeatCoilType, PTUnit( PTUnitNum ).ACHeatCoilName, PTUnit( PTUnitNum ).MaxHeatAirVolFlow, ErrorsFound );

		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).OAVolFlow = max( PTUnit( PTUnitNum ).CoolOutAirVolFlow, PTUnit( PTUnitNum ).HeatOutAirVolFlow );
			ZoneEqSizing( CurZoneEqNum ).AirVolFlow = max( PTUnit( PTUnitNum ).MaxCoolAirVolFlow, PTUnit( PTUnitNum ).MaxHeatAirVolFlow );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	ControlPTUnitOutput(
		int const PTUnitNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const QZnReq, // cooling or heating output needed by zone [W]
		int const ZoneNum, // Index to zone number
		Real64 & PartLoadFrac, // unit part load fraction
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 & SupHeaterLoad, // Supplemental heater load [W]
		bool & HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine the part load fraction of the heat pump for this time step.

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
		using DataEnvironment::OutDryBulbTemp;
		using SteamCoils::SimulateSteamCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		Real64 mdot; // coil fluid mass flow rate (kg/s)

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // maximum number of iterations
		Real64 const MinPLF( 0.0 ); // minimum part load factor allowed
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FullOutput; // unit full output when compressor is operating [W]
		Real64 TempOutput; // unit output when iteration limit exceeded [W]
		Real64 NoCompOutput; // output when no active compressor [W]
		Real64 ErrorToler; // error tolerance
		int SolFla; // Flag of RegulaFalsi solver
		static Array1D< Real64 > Par( 8 ); // Parameters passed to RegulaFalsi
		std::string IterNum; // Max number of iterations for warning message
		Real64 CpAir; // air specific heat
		Real64 OutsideDryBulbTemp; // Outside air temperature at external node height
		//unused1208  REAL(r64)          :: UpperLimitPLR ! used when RegulaFalsi exceeds iteration limit
		Real64 TempMinPLR;
		Real64 TempMaxPLR;
		bool ContinueIter;

		SupHeaterLoad = 0.0;
		PartLoadFrac = 0.0;

		if ( PTUnit( PTUnitNum ).CondenserNodeNum == 0 ) {
			OutsideDryBulbTemp = OutDryBulbTemp;
		} else {
			OutsideDryBulbTemp = Node( PTUnit( PTUnitNum ).CondenserNodeNum ).Temp;
		}

		if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).SchedPtr ) == 0.0 ) return;

		// If no heating or cooling required the coils needs to be off
		if ( ! HeatingLoad && ! CoolingLoad ) {
			return;
		}

		// Get result when DX coil is off
		CalcPTUnit( PTUnitNum, FirstHVACIteration, PartLoadFrac, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		// Get full load result
		PartLoadFrac = 1.0;
		CalcPTUnit( PTUnitNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		if ( CoolingLoad ) {
			// Since we are cooling, we expect FullOutput < NoCompOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
			if ( FullOutput >= NoCompOutput ) {
				PartLoadFrac = 0.0;
				return;
			}
			// If the QZnReq <= FullOutput the unit needs to run full out
			if ( QZnReq <= FullOutput ) {
				PartLoadFrac = 1.0;
				return;
			}
			if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
				ErrorToler = 0.001;
			} else {
				ErrorToler = PTUnit( PTUnitNum ).CoolConvergenceTol; //Error tolerance for convergence from input deck
			}
		} else {
			// Since we are heating, we expect FullOutput > NoCompOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off)
			if ( FullOutput <= NoCompOutput ) {
				PartLoadFrac = 0.0;
				// may need supplemental heating so don't return in heating mode
				//    RETURN
			}
			// If the QZnReq >= FullOutput the unit needs to run full out
			if ( QZnReq >= FullOutput && PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
				PartLoadFrac = 1.0;
				// may need supplemental heating so don't return in heating mode
				//    RETURN
			}
			ErrorToler = PTUnit( PTUnitNum ).HeatConvergenceTol; //Error tolerance for convergence from input deck
		}

		// Calculate the part load fraction

		if ( ( HeatingLoad && QZnReq < FullOutput ) || ( CoolingLoad && QZnReq > FullOutput ) ) {

			Par( 1 ) = PTUnitNum;
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
			if ( HXUnitOn ) {
				Par( 8 ) = 1.0;
			} else {
				Par( 8 ) = 0.0;
			}
			SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, PartLoadFrac, PLRResidual, 0.0, 1.0, Par );
			if ( SolFla == -1 ) {
				//     Very low loads may not converge quickly. Tighten PLR boundary and try again.
				TempMaxPLR = -0.1;
				ContinueIter = true;
				while ( ContinueIter && TempMaxPLR < 1.0 ) {
					TempMaxPLR += 0.1;
					CalcPTUnit( PTUnitNum, FirstHVACIteration, TempMaxPLR, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
					if ( HeatingLoad && TempOutput > QZnReq ) ContinueIter = false;
					if ( CoolingLoad && TempOutput < QZnReq ) ContinueIter = false;
				}
				TempMinPLR = TempMaxPLR;
				ContinueIter = true;
				while ( ContinueIter && TempMinPLR > 0.0 ) {
					TempMinPLR -= 0.01;
					CalcPTUnit( PTUnitNum, FirstHVACIteration, TempMinPLR, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
					if ( HeatingLoad && TempOutput < QZnReq ) ContinueIter = false;
					if ( CoolingLoad && TempOutput > QZnReq ) ContinueIter = false;
				}
				SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, PartLoadFrac, PLRResidual, TempMinPLR, TempMaxPLR, Par );
				if ( SolFla == -1 ) {
					if ( ! FirstHVACIteration && ! WarmupFlag ) {
						CalcPTUnit( PTUnitNum, FirstHVACIteration, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
						if ( PTUnit( PTUnitNum ).IterErrIndex == 0 ) {
							gio::write( IterNum, fmtLD ) << MaxIte;
							strip( IterNum );
							ShowWarningError( PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
							ShowContinueError( " Iteration limit exceeded calculating packaged terminal unit part-load ratio, maximum iterations = " + IterNum );
							ShowContinueErrorTimeStamp( " Part-load ratio returned = " + RoundSigDigits( PartLoadFrac, 3 ) );
							ShowContinueError( " Load requested = " + TrimSigDigits( QZnReq, 5 ) + ", Load delivered = " + TrimSigDigits( TempOutput, 5 ) );
						}
						ShowRecurringWarningErrorAtEnd( PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\" - Iteration limit exceeded error continues...", PTUnit( PTUnitNum ).IterErrIndex, TempOutput, TempOutput, _, "{W}", "{W}" );

					}
				} else if ( SolFla == -2 ) {
					if ( ! FirstHVACIteration ) {
						ShowWarningError( PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
						ShowContinueError( "Packaged terminal unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded" );
						ShowContinueError( "Please fill out a bug report and forward to the EnergyPlus support group." );
						ShowContinueErrorTimeStamp( "" );
						if ( WarmupFlag ) ShowContinueError( "Error occurred during warmup days." );
					}
					PartLoadFrac = max( MinPLF, std::abs( QZnReq - NoCompOutput ) / std::abs( FullOutput - NoCompOutput ) );
				}
			} else if ( SolFla == -2 ) {
				if ( ! FirstHVACIteration ) {
					ShowWarningError( PTUnit( PTUnitNum ).UnitType + " \"" + PTUnit( PTUnitNum ).Name + "\"" );
					ShowContinueError( "Packaged terminal unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded" );
					ShowContinueError( "Please fill out a bug report and forward to the EnergyPlus support group." );
					ShowContinueErrorTimeStamp( "" );
					if ( WarmupFlag ) ShowContinueError( "Error occurred during warmup days." );
				}
				PartLoadFrac = max( MinPLF, std::abs( QZnReq - NoCompOutput ) / std::abs( FullOutput - NoCompOutput ) );
			}

		}

		// if the DX heating coil cannot meet the load, trim with supplemental heater
		// occurs with constant fan mode when compressor is on or off
		// occurs with cycling fan mode when compressor PLR is equal to 1
		if ( HeatingLoad && QZnReq > FullOutput && PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
			PartLoadFrac = 1.0;
			if ( OutsideDryBulbTemp <= PTUnit( PTUnitNum ).MaxOATSupHeat ) {
				SupHeaterLoad = QZnReq - FullOutput;
			} else {
				SupHeaterLoad = 0.0;
			}
			CalcPTUnit( PTUnitNum, FirstHVACIteration, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
		}

		// check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
		if ( PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
			if ( Node( PTUnit( PTUnitNum ).AirOutNode ).Temp > PTUnit( PTUnitNum ).MaxSATSupHeat && SupHeaterLoad > 0.0 ) {

				// If supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
				SupHeaterLoad = 0.0;
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).SuppHeatCoilType_Num );
				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, SupHeaterLoad, PTUnit( PTUnitNum ).SuppHeatCoilIndex );
				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad );
				}}

				//     If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
				//     the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
				//     use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
				//     of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
				if ( Node( PTUnit( PTUnitNum ).AirOutNode ).Temp < PTUnit( PTUnitNum ).MaxSATSupHeat ) {
					CpAir = PsyCpAirFnWTdb( Node( PTUnit( PTUnitNum ).AirOutNode ).HumRat, Node( PTUnit( PTUnitNum ).AirOutNode ).Temp );
					SupHeaterLoad = Node( PTUnit( PTUnitNum ).AirInNode ).MassFlowRate * CpAir * ( PTUnit( PTUnitNum ).MaxSATSupHeat - Node( PTUnit( PTUnitNum ).AirOutNode ).Temp );

				} else {
					SupHeaterLoad = 0.0;
				}
			}
		}

	}

	void
	CalcPTUnit(
		int const PTUnitNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 const PartLoadFrac, // compressor part load fraction
		Real64 & LoadMet, // load met by unit (W)
		Real64 const QZnReq, // Zone load (W) unused1208
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 & SupHeaterLoad, // supplemental heater load (W)
		bool const HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED        B. Nigusse, Jan 2012, added hot water and steam heating coils to PTHP and WSHP
		//                       Chandan Sharma, July 2012 : Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the packaged terminal heat pump.

		// METHODOLOGY EMPLOYED:
		// Simulates the unit components sequentially in the air flow direction.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using DXCoils::SimDXCoil;
		using MixedAir::SimOAMixer;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using InputProcessor::SameString;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataEnvironment::OutDryBulbTemp;
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using SingleDuct::SimATMixer;
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		Real64 MinWaterFlow; // minimum water mass flow rate

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrTolerance( 0.001 ); // convergence limit for hotwater coil
		int const SolveMaxIter( 50 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // PTHP air outlet node
		int InletNode; // PTHP air inlet node
		int ZoneNode; // Zone node of zone PTHP is serving
		int ControlledZoneNum; // Index of ZoneEquipConfig that uses this heat pump
		Real64 AirMassFlow; // total supply air mass flow through the PTHP [m3/s]
		Real64 MinHumRat; // minimum humidity ratio for sensible capacity calculation (kg/kg)
		Real64 OutsideDryBulbTemp; // Outdoor air temperature at external node height
		Real64 QCoilReq; // load passed to heating coil (W)
		Real64 QActual; // actual heating coil output (W)
		int OpMode; // fan operating mode, CycFanCycCoil or ContFanCycCoil
		bool errFlag; // subroutine error flag
		Real64 WSHPRuntimeFrac; // RTF variable for WSHP's
		Real64 mdot; // local temporary for mass flow rate
		Real64 MaxHotWaterFlow; // coil maximum hot water mass flow rate, kg/s
		Real64 HotWaterMdot; // actual hot water mass flow rate
		static Array1D< Real64 > Par( 3 );
		int SolFlag;
		static int ATMixOutNode( 0 ); // outlet node of ATM Mixer

		//Tuned Named constants to avoid heap allocation when passed to Optional args
		bool const True( true );
		bool const False( false );
		int const iZero( 0 );
		int const iOne( 1 );
		Real64 const dZero( 0.0 );
		Real64 const dOne( 1.0 );

		// FLOW

		OutletNode = PTUnit( PTUnitNum ).AirOutNode;
		InletNode = PTUnit( PTUnitNum ).AirInNode;
		ControlledZoneNum = PTUnit( PTUnitNum ).CtrlZoneNum;
		ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
		OpMode = PTUnit( PTUnitNum ).OpMode;
		if ( PTUnit( PTUnitNum ).CondenserNodeNum == 0 ) {
			OutsideDryBulbTemp = OutDryBulbTemp;
		} else {
			OutsideDryBulbTemp = Node( PTUnit( PTUnitNum ).CondenserNodeNum ).Temp;
		}

		SaveCompressorPLR = 0.0;
		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		SetAverageAirFlow( PTUnitNum, PartLoadFrac, OnOffAirFlowRatio );

		AirMassFlow = Node( InletNode ).MassFlowRate;

		if ( PTUnit( PTUnitNum ).ATMixerExists ) {
			// There is an air terminal mixer
			ATMixOutNode = PTUnit( PTUnitNum ).ATMixerOutNode;
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide ) { // if there is an inlet side air terminal mixer
				// set the primary air inlet mass flow rate
				Node( PTUnit( PTUnitNum ).ATMixerPriNode ).MassFlowRate = min( Node( PTUnit( PTUnitNum ).ATMixerPriNode ).MassFlowRateMaxAvail, Node( InletNode ).MassFlowRate );
				// now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
				// the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
				SimATMixer( PTUnit( PTUnitNum ).ATMixerName, FirstHVACIteration, PTUnit( PTUnitNum ).ATMixerIndex );
			}
		} else {
			// No air terminal mixer; simulate the outside air mixer
			ATMixOutNode = 0;
			if ( PTUnit( PTUnitNum ).OutsideAirNode > 0 ) SimOAMixer( PTUnit( PTUnitNum ).OAMixName, FirstHVACIteration, PTUnit( PTUnitNum ).OAMixIndex );
		}

		// if blow through, simulate fan then coils
		if ( PTUnit( PTUnitNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( PTUnit( PTUnitNum ).FanName, FirstHVACIteration, PTUnit( PTUnitNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( CoolingLoad && OutsideDryBulbTemp > PTUnit( PTUnitNum ).MinOATCompressor ) {
			{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).UnitType_Num );
			if ( ( SELECT_CASE_var == PTACUnit ) || ( SELECT_CASE_var == PTHPUnit ) ) {
				if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted ) {
					SimHXAssistedCoolingCoil( PTUnit( PTUnitNum ).DXCoolCoilName, FirstHVACIteration, On, PartLoadFrac, PTUnit( PTUnitNum ).CoolCoilCompIndex, PTUnit( PTUnitNum ).OpMode, HXUnitOn );
				} else {
					SimDXCoil( PTUnit( PTUnitNum ).DXCoolCoilName, On, FirstHVACIteration, PTUnit( PTUnitNum ).CoolCoilCompIndex, PTUnit( PTUnitNum ).OpMode, PartLoadFrac, OnOffAirFlowRatio );
				}
				SaveCompressorPLR = DXCoilPartLoadRatio( PTUnit( PTUnitNum ).DXCoolCoilIndexNum );
			} else if ( SELECT_CASE_var == PTWSHPUnit ) {
				HeatPumpRunFrac( PTUnitNum, PartLoadFrac, errFlag, WSHPRuntimeFrac );
				SimWatertoAirHPSimple( BlankString, PTUnit( PTUnitNum ).DXCoolCoilIndexNum, QZnReq, dOne, OpMode, WSHPRuntimeFrac, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, iOne, PartLoadFrac, FirstHVACIteration, OnOffAirFlowRatio );
				SaveCompressorPLR = PartLoadFrac;
			} else {
			}}
		} else { // cooling coil is off
			{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).UnitType_Num );
			if ( ( SELECT_CASE_var == PTACUnit ) || ( SELECT_CASE_var == PTHPUnit ) ) {
				if ( PTUnit( PTUnitNum ).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted ) {
					SimHXAssistedCoolingCoil( PTUnit( PTUnitNum ).DXCoolCoilName, FirstHVACIteration, Off, dZero, PTUnit( PTUnitNum ).CoolCoilCompIndex, PTUnit( PTUnitNum ).OpMode, HXUnitOn );
				} else {
					SimDXCoil( PTUnit( PTUnitNum ).DXCoolCoilName, Off, FirstHVACIteration, PTUnit( PTUnitNum ).CoolCoilCompIndex, PTUnit( PTUnitNum ).OpMode, dZero, OnOffAirFlowRatio );
				}
			} else if ( SELECT_CASE_var == PTWSHPUnit ) {
				SimWatertoAirHPSimple( BlankString, PTUnit( PTUnitNum ).DXCoolCoilIndexNum, dZero, dZero, OpMode, dZero, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, iZero, dZero, FirstHVACIteration );
			} else {
			}}
		}
		if ( HeatingLoad ) {
			if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
				QCoilReq = PTUnit( PTUnitNum ).ACHeatCoilCap * PartLoadFrac;
				if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingGas || PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingElectric ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, QCoilReq, PTUnit( PTUnitNum ).ACHeatCoilIndex, QActual, False, OpMode, PartLoadFrac );
				} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {
					//       set water inlet node mass flow rate proportional to PLR. Limit water flow rate based on "available" upper limit.
					mdot = PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow * PartLoadFrac;

					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QActual, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
				} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {
					//       set steam inlet node mass flow rate proportional to PLR. Limit steam flow rate based on "available" upper limit.
					mdot = PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow * PartLoadFrac;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QCoilReq, QActual, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
				}
			} else {
				if ( OutsideDryBulbTemp > PTUnit( PTUnitNum ).MinOATCompressor ) {
					{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).UnitType_Num );
					if ( SELECT_CASE_var == PTHPUnit ) {
						SimDXCoil( PTUnit( PTUnitNum ).DXHeatCoilName, On, FirstHVACIteration, PTUnit( PTUnitNum ).DXHeatCoilIndex, PTUnit( PTUnitNum ).OpMode, PartLoadFrac, OnOffAirFlowRatio );
						SaveCompressorPLR = DXCoilPartLoadRatio( PTUnit( PTUnitNum ).DXHeatCoilIndexNum );
					} else if ( SELECT_CASE_var == PTWSHPUnit ) {
						HeatPumpRunFrac( PTUnitNum, PartLoadFrac, errFlag, WSHPRuntimeFrac );
						SimWatertoAirHPSimple( BlankString, PTUnit( PTUnitNum ).DXHeatCoilIndex, QZnReq, dZero, OpMode, WSHPRuntimeFrac, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, iOne, PartLoadFrac, FirstHVACIteration, OnOffAirFlowRatio );
						SaveCompressorPLR = PartLoadFrac;
					} else {
					}}
				} else {
					{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).UnitType_Num );
					if ( SELECT_CASE_var == PTHPUnit ) {
						SimDXCoil( PTUnit( PTUnitNum ).DXHeatCoilName, Off, FirstHVACIteration, PTUnit( PTUnitNum ).DXHeatCoilIndex, PTUnit( PTUnitNum ).OpMode, dZero, OnOffAirFlowRatio );
					} else if ( SELECT_CASE_var == PTWSHPUnit ) {
						SimWatertoAirHPSimple( BlankString, PTUnit( PTUnitNum ).DXHeatCoilIndex, dZero, dZero, OpMode, dZero, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, iZero, dZero, FirstHVACIteration );

					} else {
					}}
				}
			}
		} else {
			//   heating coil is off
			if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
				QCoilReq = 0.0;
				if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingGas || PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingElectric ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, QCoilReq, PTUnit( PTUnitNum ).ACHeatCoilIndex );
				} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex );
				} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QCoilReq, QActual, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
				}
			} else {
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).UnitType_Num );
				if ( SELECT_CASE_var == PTHPUnit ) {
					SimDXCoil( PTUnit( PTUnitNum ).DXHeatCoilName, Off, FirstHVACIteration, PTUnit( PTUnitNum ).DXHeatCoilIndex, PTUnit( PTUnitNum ).OpMode, dZero, OnOffAirFlowRatio );
				} else if ( SELECT_CASE_var == PTWSHPUnit ) {
					SimWatertoAirHPSimple( BlankString, PTUnit( PTUnitNum ).DXHeatCoilIndex, dZero, dZero, OpMode, dZero, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, iZero, dZero, FirstHVACIteration );

				} else {
				}}
			}
		}

		// if draw through, simulate coils then fan
		if ( PTUnit( PTUnitNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( PTUnit( PTUnitNum ).FanName, FirstHVACIteration, PTUnit( PTUnitNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}
		if ( PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
			if ( SupHeaterLoad < SmallLoad ) {
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).SuppHeatCoilType_Num );
				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, SupHeaterLoad, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, True, PTUnit( PTUnitNum ).OpMode );
				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, PTUnit( PTUnitNum ).OpMode );
				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, QActual, PTUnit( PTUnitNum ).OpMode );
				}}
			} else {
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).SuppHeatCoilType_Num );
				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, SupHeaterLoad, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, True, PTUnit( PTUnitNum ).OpMode );
				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
					MaxHotWaterFlow = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
					SetComponentFlowRate( MaxHotWaterFlow, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					QActual = SupHeaterLoad;
					// simulate the hot water supplemental heating coil
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, PTUnit( PTUnitNum ).OpMode );
					if ( QActual > ( SupHeaterLoad + SmallLoad ) ) {
						// control water flow to obtain output matching SupHeaterLoad
						SolFlag = 0;
						MinWaterFlow = 0.0;
						Par( 1 ) = double( PTUnitNum );
						if ( FirstHVACIteration ) {
							Par( 2 ) = 1.0;
						} else {
							Par( 2 ) = 0.0;
						}
						Par( 3 ) = SupHeaterLoad;
						MaxHotWaterFlow = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
						SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par );
						if ( SolFlag == -1 ) {
							if ( PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex == 0 ) {
								ShowWarningMessage( "CalcPTUnit: Hot water coil control failed for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "  Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating hot water mass flow rate" );
							}
							ShowRecurringWarningErrorAtEnd( "CalcPTUnit: Hot water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex );
						} else if ( SolFlag == -2 ) {
							if ( PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex2 == 0 ) {
								ShowWarningMessage( "CalcPTUnit: Hot water coil control failed (maximum flow limits) for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name + "\"" );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "...Bad hot water maximum flow rate limits" );
								ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinWaterFlow, 3 ) + " kg/s" );
								ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxHotWaterFlow, 3 ) + " kg/s" );
							}
							ShowRecurringWarningErrorAtEnd( "CalcPTUnit: Hot water coil control failed (flow limits) for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name + "\"", PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex2, MaxHotWaterFlow, MinWaterFlow, _, "[kg/s]", "[kg/s]" );
						}
						QActual = SupHeaterLoad;
						// simulate the hot water supplemental heating coil
						SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, PTUnit( PTUnitNum ).OpMode );
					}
				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
					mdot = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

					// simulate the steam supplemental heating coil
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, QActual, PTUnit( PTUnitNum ).OpMode );
				}}
			}
		}

		// If there is a supply side air terminal mixer, calculate its output
		if ( PTUnit( PTUnitNum ).ATMixerExists ) {
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				SimATMixer( PTUnit( PTUnitNum ).ATMixerName, FirstHVACIteration, PTUnit( PTUnitNum ).ATMixerIndex );
			}
		}

		// calculate sensible load met
		if ( PTUnit( PTUnitNum ).ATMixerExists ) {
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				// Air terminal supply side mixer
				LoadMet = Node( ATMixOutNode ).MassFlowRate * ( PsyHFnTdbW( Node( ATMixOutNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
			} else {
				// Air terminal inlet side mixer
				LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
			}
		} else {
			MinHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );
		}

	}

	void
	HeatPumpRunFrac(
		int const PTUnitNum, // PTAC Index Number
		Real64 const PLR, // part load ratio
		bool & errFlag, // part load factor out of range flag
		Real64 & RuntimeFrac // the required run time fraction to meet part load
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad (based on subroutine by Kenneth Tang)
		//       DATE WRITTEN   June 2009
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
		// Ernest Orlando Lawrence Berkeley National Laboratory.

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
		Real64 A; // Variable for simplify equation
		int NumIteration; // Iteration Counter

		Nmax = PTUnit( PTUnitNum ).MaxONOFFCyclesperHour;
		tau = PTUnit( PTUnitNum ).HPTimeConstant;
		pr = PTUnit( PTUnitNum ).OnCyclePowerFraction;

		// Initialize
		errFlag = false;
		error = 1.0;
		NumIteration = 0;

		// Initial guess for part load fraction
		PLF1 = 1.0;

		// Calculate PLF using successive substitution until convergence is achieved
		while ( true ) {
			++NumIteration;

			if ( PLR == 1 ) {
				// Set part load fraction, PLF1=1.0 if PLR=1.0 and exit loop
				PLF1 = 1.0;
				goto LOOPPLF_exit;
			}

			if ( NumIteration > 100 ) {
				// Exit loop if interation exceed 100
				errFlag = true;
				PLF1 = 1.0;
				goto LOOPPLF_exit;
			}

			if ( error < 0.00001 ) {
				// Exit loop if convergence is achieved
				goto LOOPPLF_exit;

			} else {
				// Calculate PLF
				A = 4.0 * tau * ( Nmax / 3600.0 ) * ( 1 - PLR / PLF1 );
				if ( A < 1.5e-3 ) {
					// A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
					// from "float underflow error". Occurs when PLR is very close to 1.0,
					// small A value, thus Exp(-1/A) = 0
					PLF2 = 1 - A;
				} else {
					PLF2 = 1.0 - A * ( 1 - std::exp( -1.0 / A ) );
				}
				error = std::abs( ( PLF2 - PLF1 ) / PLF1 );
				PLF1 = PLF2;
			}
		}
		LOOPPLF_exit: ;

		//Adjust PLF for the off cycle power consumption if
		//on-cycle power use is specified by the user
		if ( pr > 0.0 ) {
			PartLoadFactor = PLR / ( ( PLR / PLF1 ) + ( 1 - PLR / PLF1 ) * pr );
		} else {
			PartLoadFactor = PLF1;
		}

		if ( PartLoadFactor <= 0.0 ) {
			PartLoadFactor = 0.0;
			RuntimeFrac = 0.0;
			errFlag = true;
		} else {
			RuntimeFrac = PLR / PartLoadFactor;
		}

		if ( RuntimeFrac > 1.0 ) {
			RuntimeFrac = 1.0;
		}

	}

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   January 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Actual Coil Output - SupHeaterLoad) / SupHeaterLoad
		// the actual coil output depends on the hot water flow rate which is being varied to
		// minimize the residual to the tolerance limit specified.

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
		Real64 SupHeaterLoad; // requested coild load, W

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum;
		bool FirstHVACSoln;
		Real64 QCoilActual; // delivered coild load, W
		Real64 mdot;

		PTUnitNum = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		SupHeaterLoad = Par( 3 );
		QCoilActual = SupHeaterLoad;
		mdot = HWFlow;

		SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
		// simulate the hot water supplemental heating coil
		SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACSoln, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QCoilActual, PTUnit( PTUnitNum ).OpMode );

		if ( SupHeaterLoad != 0.0 ) {
			Residuum = ( QCoilActual - SupHeaterLoad ) / SupHeaterLoad;
		} else { //Autodesk:Return ELSE added to assure return value is set
			Residuum = 0.0;
		}
		return Residuum;
	}

	Real64
	SupSATResidual(
		Real64 & TempSupHeater, // supplemental heater load at maximum SAT
		Array1< Real64 > const & Par // par(1) = PTUnitNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (outlet temp - maximum supplemental heater SAT)
		//  Outlet temperature depends on the supplemental heater load which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls SimulateHeatingCoilComponents to get outlet temperature minus the maximum supplemental heater SAT
		//  at the given supplemental heater load and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;

		// Return value
		Real64 SupSATResidual;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = FirstHVACIteration

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum; // PTHP index
		bool FirstHVACIteration; // FirstHVACIteration flag

		PTUnitNum = int( Par( 1 ) );
		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 2 ) == 1.0 );
		SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, TempSupHeater, PTUnit( PTUnitNum ).SuppHeatCoilIndex );
		SupSATResidual = Node( PTUnit( PTUnitNum ).AirOutNode ).Temp - PTUnit( PTUnitNum ).MaxSATSupHeat;

		return SupSATResidual;
	}

	Real64
	PLRResidual(
		Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = PTUnitNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
		//  PTHP output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcPTHP to get ActualOutput at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 PLRResidual;

		// Argument array dimensioning

		// Locals
		Real64 SupHeaterLoad; // load passed to supplemental heater (W)

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = Zone Num
		// par(3) = FirstHVACIteration
		// par(4) = OpMode
		// par(5) = QZnReq
		// par(6) = OnOffAirFlowRatio
		// par(7) = SupHeaterLoad

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum; // PTHP index
		int ZoneNum; // Zone index
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Compressor operating mode
		Real64 QZnReq; // zone load (W)
		Real64 QZnReqTemp; // denominator representing zone load (W)
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 ActualOutput; // delivered capacity of PTHP
		bool HXUnitOn; // flag to enable heat exchanger

		PTUnitNum = int( Par( 1 ) );
		ZoneNum = int( Par( 2 ) );
		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		OpMode = int( Par( 4 ) );
		QZnReq = Par( 5 );
		QZnReqTemp = QZnReq;
		if ( std::abs( QZnReq ) < 100.0 ) QZnReqTemp = sign( 100.0, QZnReq );
		OnOffAirFlowRatio = Par( 6 );
		SupHeaterLoad = Par( 7 ) * PartLoadFrac;
		HXUnitOn = ( Par( 8 ) == 1.0 );

		CalcPTUnit( PTUnitNum, FirstHVACIteration, PartLoadFrac, ActualOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
		PLRResidual = ( ActualOutput - QZnReq ) / QZnReqTemp;

		return PLRResidual;
	}

	void
	SetAverageAirFlow(
		int const PTUnitNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to average airflow over timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the average air mass flow rates using the part load fraction of the heat pump for this time step
		// Set OnOffAirFlowRatio to be used by DX coils

		// METHODOLOGY EMPLOYED:
		// na

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
		int InletNode; // inlet node number for PTUnitNum
		int OutsideAirNode; // outside air node number in PTHP loop
		int AirRelNode; // relief air node number in PTHP loop
		Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step
		Real64 AverageOAMassFlow; // average outdoor air mass flow rate over time step

		InletNode = PTUnit( PTUnitNum ).AirInNode;
		OutsideAirNode = PTUnit( PTUnitNum ).OutsideAirNode;
		AirRelNode = PTUnit( PTUnitNum ).AirReliefNode;

		AverageUnitMassFlow = ( PartLoadRatio * CompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * CompOffMassFlow );
		AverageOAMassFlow = ( PartLoadRatio * OACompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * OACompOffMassFlow );
		if ( CompOffFlowRatio > 0.0 ) {
			FanSpeedRatio = ( PartLoadRatio * CompOnFlowRatio ) + ( ( 1 - PartLoadRatio ) * CompOffFlowRatio );
		} else {
			FanSpeedRatio = CompOnFlowRatio;
		}

		if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).SchedPtr ) > 0.0 && ( ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).FanAvailSchedPtr ) > 0.0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOff ) ) {

			Node( InletNode ).MassFlowRate = AverageUnitMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = AverageUnitMassFlow;
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = AverageOAMassFlow;
				Node( OutsideAirNode ).MassFlowRateMaxAvail = AverageOAMassFlow;
				Node( AirRelNode ).MassFlowRate = AverageOAMassFlow;
				Node( AirRelNode ).MassFlowRateMaxAvail = AverageOAMassFlow;
			}
			if ( AverageUnitMassFlow > 0.0 ) {
				OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
			} else {
				OnOffAirFlowRatio = 0.0;
			}

		} else {

			Node( InletNode ).MassFlowRate = 0.0;
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = 0.0;
				Node( AirRelNode ).MassFlowRate = 0.0;
			}
			OnOffAirFlowRatio = 0.0;

		}

	}

	void
	ReportPTUnit( int const PTUnitNum ) // number of the current AC unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the packaged terminal heat pump

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// NA

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ReportingConstant;

		// FLOW

		ReportingConstant = TimeStepSys * SecInHour;
		PTUnit( PTUnitNum ).TotCoolEnergy = PTUnit( PTUnitNum ).TotCoolEnergyRate * ReportingConstant;
		PTUnit( PTUnitNum ).TotHeatEnergy = PTUnit( PTUnitNum ).TotHeatEnergyRate * ReportingConstant;
		PTUnit( PTUnitNum ).SensCoolEnergy = PTUnit( PTUnitNum ).SensCoolEnergyRate * ReportingConstant;
		PTUnit( PTUnitNum ).SensHeatEnergy = PTUnit( PTUnitNum ).SensHeatEnergyRate * ReportingConstant;
		PTUnit( PTUnitNum ).LatCoolEnergy = PTUnit( PTUnitNum ).LatCoolEnergyRate * ReportingConstant;
		PTUnit( PTUnitNum ).LatHeatEnergy = PTUnit( PTUnitNum ).LatHeatEnergyRate * ReportingConstant;
		PTUnit( PTUnitNum ).ElecConsumption = PTUnit( PTUnitNum ).ElecPower * ReportingConstant;

	}

	int
	GetPTUnitZoneInletAirNode(
		int const PTUnitCompIndex,
		int const PTUnitType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for zone air inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::PkgTermHPAirToAir_Num;
		using DataZoneEquipment::PkgTermHPWaterToAir_Num;
		using DataZoneEquipment::PkgTermACAirToAir_Num;

		// Return value
		int GetPTUnitZoneInletAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum( 0 );

		if ( GetPTUnitInputFlag ) {
			GetPTUnit();
			GetPTUnitInputFlag = false;
		}

		GetPTUnitZoneInletAirNode = 0;

		// PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
		// objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
		// is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
		// objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
		// So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
		// which was recalculated for total number of all three object type for use in PT data structure.

		{ auto const SELECT_CASE_var( PTUnitType );
		if ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex;
		} else if ( SELECT_CASE_var == PkgTermACAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP;
		} else if ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC;
		} else {
			assert( false );
		}}

		if ( PTUnitNum > 0 && PTUnitNum <= NumPTUs ) {
			GetPTUnitZoneInletAirNode = PTUnit( PTUnitNum ).AirOutNode;
		}

		return GetPTUnitZoneInletAirNode;

	}

	int
	GetPTUnitOutAirNode(
		int const PTUnitCompIndex,
		int const PTUnitType
	)
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

		// Using/Aliasing
		using DataZoneEquipment::PkgTermHPAirToAir_Num;
		using DataZoneEquipment::PkgTermHPWaterToAir_Num;
		using DataZoneEquipment::PkgTermACAirToAir_Num;

		// Return value
		int GetPTUnitOutAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum( 0 );

		if ( GetPTUnitInputFlag ) {
			GetPTUnit();
			GetPTUnitInputFlag = false;
		}

		GetPTUnitOutAirNode = 0;

		// PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
		// objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
		// is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
		// objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
		// So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
		// which was recalculated for total number of all three object type for use in PT data structure.

		{ auto const SELECT_CASE_var( PTUnitType );
		if ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex;
		} else if ( SELECT_CASE_var == PkgTermACAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP;
		} else if ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC;
		} else {
			assert( false );
		}}

		if ( PTUnitNum > 0 && PTUnitNum <= NumPTUs ) {

			GetPTUnitOutAirNode = PTUnit( PTUnitNum ).OutsideAirNode;
		}

		return GetPTUnitOutAirNode;

	}

	int
	GetPTUnitReturnAirNode(
		int const PTUnitCompIndex,
		int const PTUnitType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixer return air node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::GetOAMixerReturnNodeNumber;
		using DataZoneEquipment::PkgTermHPAirToAir_Num;
		using DataZoneEquipment::PkgTermHPWaterToAir_Num;
		using DataZoneEquipment::PkgTermACAirToAir_Num;

		// Return value
		int GetPTUnitReturnAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum( 0 );

		if ( GetPTUnitInputFlag ) {
			GetPTUnit();
			GetPTUnitInputFlag = false;
		}

		// PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
		// objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
		// is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
		// objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
		// So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
		// which was recalculated for total number of all three object type for use in PT data structure.

		{ auto const SELECT_CASE_var( PTUnitType );
		if ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex;
		} else if ( SELECT_CASE_var == PkgTermACAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP;
		} else if ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC;
		} else {
			assert( false );
		}}

		if ( PTUnitNum > 0 && PTUnitNum <= NumPTUs ) {
			if ( PTUnit( PTUnitNum ).OAMixIndex > 0 ) {
				GetPTUnitReturnAirNode = GetOAMixerReturnNodeNumber( PTUnit( PTUnitNum ).OAMixIndex );
			} else {
				GetPTUnitReturnAirNode = 0;
			}
		} else {
			GetPTUnitReturnAirNode = 0;
		}

		return GetPTUnitReturnAirNode;

	}

	int
	GetPTUnitMixedAirNode(
		int const PTUnitCompIndex,
		int const PTUnitType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixed air node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::GetOAMixerMixedNodeNumber;
		using DataZoneEquipment::PkgTermHPAirToAir_Num;
		using DataZoneEquipment::PkgTermHPWaterToAir_Num;
		using DataZoneEquipment::PkgTermACAirToAir_Num;

		// Return value
		int GetPTUnitMixedAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PTUnitNum( 0 );

		if ( GetPTUnitInputFlag ) {
			GetPTUnit();
			GetPTUnitInputFlag = false;
		}

		// PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
		// objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
		// is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
		// objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
		// So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
		// which was recalculated for total number of all three object type for use in PT data structure.

		{ auto const SELECT_CASE_var( PTUnitType );
		if ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex;
		} else if ( SELECT_CASE_var == PkgTermACAirToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP;
		} else if ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) {
			PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC;
		} else {
			assert( false );
		}}

		if ( PTUnitNum > 0 && PTUnitNum <= NumPTUs ) {
			if ( PTUnit( PTUnitNum ).OAMixIndex > 0 ) {
				GetPTUnitMixedAirNode = GetOAMixerMixedNodeNumber( PTUnit( PTUnitNum ).OAMixIndex );
			} else {
				GetPTUnitMixedAirNode = 0;
			}
		} else {
			GetPTUnitMixedAirNode = 0;
		}

		return GetPTUnitMixedAirNode;

	}

	//******************************************************************************

	void
	SimVariableSpeedHP(
		int const PTUnitNum, // number of the current engine driven Heat Pump being simulated
		int const ZoneNum, // Controlled zone number
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 const QZnReq, // required zone load
		Real64 const QLatReq, // required latent load
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		bool const HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a multispeed heat pump; adjust its output to match the
		// required system load.

		// METHODOLOGY EMPLOYED:
		// Calls ControlMSHPOutput to obtain the desired unit output

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataAirLoop::AirLoopControlInfo;
		using DataAirLoop::AirToZoneNodeInfo;
		using DataAirSystems::PrimaryAirSystem;

		// Locals
		Real64 SupHeaterLoad; // supplement heater load

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 PartLoadFrac( 0.0 ); // compressor part load fraction
		static Real64 SpeedRatio( 0.0 ); // compressor speed ratio
		bool UnitOn; // TRUE if unit is on
		int OutletNode; // MSHP air outlet node
		int InletNode; // MSHP air inlet node
		Real64 AirMassFlow; // air mass flow rate [kg/s]
		Real64 QTotUnitOut; // capacity output
		static int SpeedNum( 1 ); // Speed number
		int AirLoopNumber; // Index to air loop
		Real64 SaveMassFlowRate; // saved inlet air mass flow rate [kg/s]
		Real64 QSensUnitOut; // sensible capacity output
		Real64 QLatUnitOut; // latent capacity output
		int CompOp; // compressor operation; 1=on, 0=off
		static Real64 TotalZoneLatentLoad; // Total ZONE heating load (not including outside air)
		int TotBranchNum; // total outlet branch number
		int ZoneSideNodeNum; // zone equip supply node
		bool EconoActive; // TRUE if Economizer is active

		// zero the fan, DX coils, and supplemental electric heater electricity consumption
		FanElecPower = 0.0;
		DXElecHeatingPower = 0.0;
		DXElecCoolingPower = 0.0;
		SaveCompressorPLR = 0.0;
		ElecHeatingCoilPower = 0.0;

		// initialize local variables
		UnitOn = true;
		CompOp = 1;
		OutletNode = PTUnit( PTUnitNum ).AirOutNode;
		InletNode = PTUnit( PTUnitNum ).AirInNode;
		AirMassFlow = PTUnit( PTUnitNum ).MaxCoolAirMassFlow;

		//Set latent load for heating
		if ( HeatingLoad ) {
			TotalZoneLatentLoad = 0.0;
			PTUnit( PTUnitNum ).HeatCoolMode = HeatingMode;
			//Set latent load for cooling and no sensible load condition
		} else {
			TotalZoneLatentLoad = QLatReq;
			PTUnit( PTUnitNum ).HeatCoolMode = CoolingMode;
		}

		if ( HeatingLoad ) {
			PTUnit( PTUnitNum ).HeatCoolMode = HeatingMode;
		} else if ( CoolingLoad ) {
			PTUnit( PTUnitNum ).HeatCoolMode = CoolingMode;
		} else {
			PTUnit( PTUnitNum ).HeatCoolMode = 0;
		}

		// set the on/off flags
		if ( PTUnit( PTUnitNum ).OpMode == CycFanCycCoil ) {
			// cycling unit only runs if there is a cooling or heating load.
			if ( std::abs( QZnReq ) < SmallLoad || AirMassFlow < SmallMassFlow || CurDeadBandOrSetback( ZoneNum ) ) {
				UnitOn = false;
			}
		} else if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil ) {
			// continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
			if ( AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
			}
		}

		OnOffFanPartLoadFraction = 1.0;

		AirLoopNumber = ZoneEquipConfig( ZoneNum ).AirLoopNum;

		if ( AirLoopNumber != 0 ) {
			EconoActive = AirLoopControlInfo( AirLoopNumber ).EconoActive;
		} else {
			EconoActive = false;
		}

		SaveMassFlowRate = Node( InletNode ).MassFlowRate;
		if ( ! FirstHVACIteration && PTUnit( PTUnitNum ).OpMode == CycFanCycCoil && ( QZnReq < ( -1.0 * SmallLoad ) || TotalZoneLatentLoad > SmallLoad ) && EconoActive ) {
			// for cycling fan, cooling load, check whether furnace can meet load with compressor off
			CompOp = Off;
			ControlVSHPOutput( PTUnitNum, FirstHVACIteration, CompOp, OpMode, QZnReq, TotalZoneLatentLoad, ZoneNum, SpeedNum, SpeedRatio, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

			if ( SpeedNum == PTUnit( PTUnitNum ).NumOfSpeedCooling && SpeedRatio == 1.0 ) {
				// compressor on (reset inlet air mass flow rate to starting value)
				Node( InletNode ).MassFlowRate = SaveMassFlowRate;
				CompOp = On;
				ControlVSHPOutput( PTUnitNum, FirstHVACIteration, CompOp, OpMode, QZnReq, TotalZoneLatentLoad, ZoneNum, SpeedNum, SpeedRatio, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
			}
		} else {
			// compressor on
			CompOp = On;
			ControlVSHPOutput( PTUnitNum, FirstHVACIteration, CompOp, OpMode, QZnReq, TotalZoneLatentLoad, ZoneNum, SpeedNum, SpeedRatio, PartLoadFrac, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
		}

		if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
			SaveCompressorPLR = PartLoadFrac;
		} else {
			if ( SpeedNum > 1 ) {
				SaveCompressorPLR = 1.0;
			}

			if ( PartLoadFrac == 1.0 && SaveCompressorPLR < 1.0 ) {
				PartLoadFrac = SaveCompressorPLR;
			}
		}

		CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, QSensUnitOut, QLatUnitOut, QZnReq, TotalZoneLatentLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		// calculate delivered capacity
		AirMassFlow = Node( InletNode ).MassFlowRate;

		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );

		Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
		Node( OutletNode ).MassFlowRateMaxAvail = AirMassFlow;

		if ( ! FirstHVACIteration && AirMassFlow > 0.0 && AirLoopNumber > 0 ) {
			TotBranchNum = PrimaryAirSystem( AirLoopNumber ).NumOutletBranches;
			if ( TotBranchNum == 1 ) {
				ZoneSideNodeNum = AirToZoneNodeInfo( AirLoopNumber ).ZoneEquipSupplyNodeNum( 1 );
				// THE MASS FLOW PRECISION of the system solver is not enough for some small air flow rate iterations , BY DEBUGGING
				// it may cause mass flow rate occilations between airloop and zoneequip
				// specify the air flow rate directly for one-to-one system, when the iteration deviation is closing the solver precision level
				// 0.02 is 2 * HVACFlowRateToler, in order to accomodate the system solver precision level
				if ( std::abs( AirMassFlow - Node( ZoneSideNodeNum ).MassFlowRate ) < 0.02 ) Node( ZoneSideNodeNum ).MassFlowRateMaxAvail = AirMassFlow;
				Node( ZoneSideNodeNum ).MassFlowRate = AirMassFlow;
			}

			// the below might be useful if more divergences occur
			// Node(PrimaryAirSystem(AirLoopNumber)%Branch(1)%NodeNumIn)%MassFlowRateMaxAvail = AirMassFlow
			// Node(PrimaryAirSystem(AirLoopNumber)%Branch(1)%NodeNumIn)%MassFlowRate = AirMassFlow
		}

		// report variables
		PTUnit( PTUnitNum ).CompPartLoadRatio = SaveCompressorPLR;
		if ( PTUnit( PTUnitNum ).OpMode == CycFanCycCoil ) {
			if ( SupHeaterLoad > 0.0 ) {
				PTUnit( PTUnitNum ).FanPartLoadRatio = 1.0;
			} else {
				if ( SpeedNum < 2 ) {
					PTUnit( PTUnitNum ).FanPartLoadRatio = PartLoadFrac;
				} else {
					PTUnit( PTUnitNum ).FanPartLoadRatio = 1.0;
				}
			}
		} else {
			if ( UnitOn ) {
				PTUnit( PTUnitNum ).FanPartLoadRatio = 1.0;
			} else {
				if ( SpeedNum < 2 ) {
					PTUnit( PTUnitNum ).FanPartLoadRatio = PartLoadFrac;
				} else {
					PTUnit( PTUnitNum ).FanPartLoadRatio = 1.0;
				}
			}
		}

	}

	//******************************************************************************
	//******************************************************************************

	void
	ControlVSHPOutput(
		int const PTUnitNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		int const CompOp, // compressor operation; 1=on, 0=off
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const QZnReq, // cooling or heating output needed by zone [W]
		Real64 const QLatReq, // latent cooling output needed by zone [W]
		int const ZoneNum, // Index to zone number
		int & SpeedNum, // Speed number
		Real64 & SpeedRatio, // unit speed ratio for DX coils
		Real64 & PartLoadFrac, // unit part load fraction
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 & SupHeaterLoad, // Supplemental heater load [W]
		bool const HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:ControlMSHPOutput
		//       DATE WRITTEN   March,  2012
		//       MODIFIED       na
		//       RE-ENGINEERED

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
		using SteamCoils::SimulateSteamCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using DataEnvironment::OutDryBulbTemp;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;

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
		Real64 LatOutput; // latent capacity output
		Real64 ErrorToler; // error tolerance
		int SolFla; // Flag of RegulaFalsi solver
		static Array1D< Real64 > Par( 11 ); // Parameters passed to RegulaFalsi
		Real64 CpAir; // air specific heat
		int i; // Speed index
		static int ErrCountCyc( 0 ); // Counter used to minimize the occurrence of output warnings
		static int ErrCountVar( 0 ); // Counter used to minimize the occurrence of output warnings
		Real64 mdot; // coil fluid mass flow rate (kg/s)

		// FLOW
		SupHeaterLoad = 0.0;
		PartLoadFrac = 0.0;
		SpeedRatio = 0.0;
		SpeedNum = 1;
		LatOutput = 0.0;
		ErrorToler = 0.001; //Error tolerance for convergence from input deck

		if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).SchedPtr ) == 0.0 ) return;

		// Get result when DX coil is off
		CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, NoCompOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		// If cooling and NoCompOutput < QZnReq, the coil needs to be off
		// If heating and NoCompOutput > QZnReq, the coil needs to be off
		if ( ( QZnReq < ( -1.0 * SmallLoad ) && NoCompOutput < QZnReq ) || ( QZnReq > SmallLoad && NoCompOutput > QZnReq ) || ( std::abs( QZnReq ) <= SmallLoad && std::abs( QLatReq ) <= SmallLoad ) || CurDeadBandOrSetback( ZoneNum ) ) {
			return;
		}

		// Get full load result
		PartLoadFrac = 1.0;
		SpeedRatio = 1.0;
		if ( PTUnit( PTUnitNum ).HeatCoolMode == HeatingMode ) {
			if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
				SpeedNum = PTUnit( PTUnitNum ).NumOfSpeedCooling;
			} else {
				SpeedNum = PTUnit( PTUnitNum ).NumOfSpeedHeating;
			}
		} else if ( PTUnit( PTUnitNum ).HeatCoolMode == CoolingMode ) {
			SpeedNum = PTUnit( PTUnitNum ).NumOfSpeedCooling;
		} else {
			SpeedNum = 1;
			PartLoadFrac = 0.0;
		}

		CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, FullOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		if ( QLatReq < 0.0 ) { //dehumidification mode
			//  ! If the QLatReq <= LatOutput the unit needs to run full out
			if ( QLatReq <= LatOutput ) {
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				PTUnit( PTUnitNum ).CompPartLoadRatio = PartLoadFrac;
				PTUnit( PTUnitNum ).CompSpeedRatio = SpeedRatio;
				PTUnit( PTUnitNum ).CompSpeedNum = SpeedNum;
				return;
			}
			ErrorToler = 0.001; //Error tolerance for convergence from input deck
		} else if ( QZnReq < ( -1.0 * SmallLoad ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
			// Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
			if ( FullOutput >= 0.0 || FullOutput >= NoCompOutput ) {
				PartLoadFrac = 0.0;
				SpeedRatio = 0.0;
				SpeedNum = 1;
				return;
			}
			//  ! If the QZnReq <= FullOutput the unit needs to run full out
			if ( QZnReq <= FullOutput ) {
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				PTUnit( PTUnitNum ).CompPartLoadRatio = PartLoadFrac;
				PTUnit( PTUnitNum ).CompSpeedRatio = SpeedRatio;
				PTUnit( PTUnitNum ).CompSpeedNum = SpeedNum;
				return;
			}
			ErrorToler = 0.001; //Error tolerance for convergence from input deck
		} else if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) ) {
			// Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off)
			if ( FullOutput <= 0.0 || FullOutput <= NoCompOutput ) {
				PartLoadFrac = 0.0;
				SpeedRatio = 0.0;
				SpeedNum = 1;
				// may need supplemental heating so don't return in heating mode
			}
			if ( QZnReq >= FullOutput ) {
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				// may need supplemental heating so don't return in heating mode
			}
			ErrorToler = 0.001; //Error tolerance for convergence from input deck
		} else { // no load
			PartLoadFrac = 0.0;
			SpeedRatio = 0.0;
			SpeedNum = 1;
		}

		// Calculate the part load fraction
		if ( ( ( QZnReq > SmallLoad && QZnReq < FullOutput ) || ( QZnReq < ( -1.0 * SmallLoad ) && QZnReq > FullOutput ) || ( QLatReq < ( -1.0 * SmallLoad ) ) ) && ! CurDeadBandOrSetback( ZoneNum ) ) {

			Par( 1 ) = PTUnitNum;
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
			Par( 10 ) = 1.0;
			if ( HXUnitOn ) {
				Par( 11 ) = 1.0;
			} else {
				Par( 11 ) = 0.0;
			}
			// Check whether the low speed coil can meet the load or not
			CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, 1, 0.0, 1.0, LowOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
			if ( ( ( QZnReq > SmallLoad && QZnReq <= LowOutput ) || ( QZnReq < ( -1.0 * SmallLoad ) && QZnReq >= LowOutput ) || ( QLatReq < ( -1.0 * SmallLoad ) && QLatReq > LatOutput ) ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				SpeedRatio = 0.0;
				SpeedNum = 1;

				if ( QLatReq < 0.0 ) { //calculate latent heat residual
					Par( 10 ) = 0.0;
					Par( 5 ) = QLatReq;
				}

				SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, PartLoadFrac, VSHPCyclingResidual, 0.0, 1.0, Par );
				if ( SolFla == -1 ) {
					if ( ! WarmupFlag ) {
						if ( ErrCountCyc == 0 ) {
							++ErrCountCyc;
							ShowWarningError( "Iteration limit exceeded calculating VS WSHP unit cycling ratio, for unit=" + PTUnit( PTUnitNum ).Name );
							ShowContinueErrorTimeStamp( "Cycling ratio returned=" + RoundSigDigits( PartLoadFrac, 2 ) );
						} else {
							++ErrCountCyc;
							ShowRecurringWarningErrorAtEnd( PTUnit( PTUnitNum ).Name + "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...", PTUnit( PTUnitNum ).ErrIndexCyc, PartLoadFrac, PartLoadFrac );
						}
					}
				} else if ( SolFla == -2 ) {
					ShowFatalError( "VS WSHP unit cycling ratio calculation failed: cycling limits exceeded, for unit=" + PTUnit( PTUnitNum ).Name );
				}
			} else {
				// Check to see which speed to meet the load
				PartLoadFrac = 1.0;
				SpeedRatio = 1.0;
				// Cooling
				if ( ( ( QZnReq < ( -1.0 * SmallLoad ) ) || ( QLatReq < ( -1.0 * SmallLoad ) ) ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
					for ( i = 2; i <= PTUnit( PTUnitNum ).NumOfSpeedCooling; ++i ) {
						CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, i, SpeedRatio, PartLoadFrac, TempOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

						if ( QLatReq < 0.0 ) {
							if ( QLatReq > LatOutput ) {
								SpeedNum = i;
								break;
							}
						} else if ( QZnReq >= TempOutput ) {
							SpeedNum = i;
							break;
						}

					}
				} else {
					for ( i = 2; i <= PTUnit( PTUnitNum ).NumOfSpeedHeating; ++i ) {
						CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, i, SpeedRatio, PartLoadFrac, TempOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
						if ( QZnReq <= TempOutput ) {
							SpeedNum = i;
							break;
						}
					}
				}
				Par( 8 ) = SpeedNum;

				if ( QLatReq < ( -1.0 * SmallLoad ) ) { //calculate latent heat residual
					Par( 10 ) = 0.0;
					Par( 5 ) = QLatReq;
				}

				SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, SpeedRatio, VSHPSpeedResidual, 1.0e-10, 1.0, Par );
				if ( SolFla == -1 ) {
					if ( ! WarmupFlag ) {
						if ( ErrCountVar == 0 ) {
							++ErrCountVar;
							ShowWarningError( "Iteration limit exceeded calculating VS WSHP unit speed ratio, for unit=" + PTUnit( PTUnitNum ).Name );
							ShowContinueErrorTimeStamp( "Speed ratio returned=[" + RoundSigDigits( SpeedRatio, 2 ) + "], Speed number =" + RoundSigDigits( SpeedNum ) );
						} else {
							++ErrCountVar;
							ShowRecurringWarningErrorAtEnd( PTUnit( PTUnitNum ).Name + "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...", PTUnit( PTUnitNum ).ErrIndexVar, SpeedRatio, SpeedRatio );
						}
					}
				} else if ( SolFla == -2 ) {
					ShowFatalError( "VS WSHP unit compressor speed calculation failed: speed limits exceeded, for unit=" + PTUnit( PTUnitNum ).Name );
				}
			}
		}

		// if the DX heating coil cannot meet the load, trim with supplemental heater
		// occurs with constant fan mode when compressor is on or off
		// occurs with cycling fan mode when compressor PLR is equal to 1
		if ( HeatingLoad && QZnReq > FullOutput && PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
			PartLoadFrac = 1.0;
			SpeedRatio = 1.0;

			if ( PTUnit( PTUnitNum ).NumOfSpeedHeating > 0 ) SpeedNum = PTUnit( PTUnitNum ).NumOfSpeedHeating; //maximum heating speed, avoid zero

			if ( OutDryBulbTemp <= PTUnit( PTUnitNum ).MaxOATSupHeat ) {
				SupHeaterLoad = QZnReq - FullOutput;
			} else {
				SupHeaterLoad = 0.0;
			}
			CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, PartLoadFrac, TempOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
		}

		// check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
		if ( PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
			if ( Node( PTUnit( PTUnitNum ).AirOutNode ).Temp > PTUnit( PTUnitNum ).MaxSATSupHeat && SupHeaterLoad > 0.0 ) {

				// If supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
				SupHeaterLoad = 0.0;
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).SuppHeatCoilType_Num );
				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, SupHeaterLoad, PTUnit( PTUnitNum ).SuppHeatCoilIndex );
				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad );
				}}

				//     If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
				//     the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
				//     use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
				//     of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
				if ( Node( PTUnit( PTUnitNum ).AirOutNode ).Temp < PTUnit( PTUnitNum ).MaxSATSupHeat ) {
					CpAir = PsyCpAirFnWTdb( Node( PTUnit( PTUnitNum ).AirOutNode ).HumRat, Node( PTUnit( PTUnitNum ).AirOutNode ).Temp );
					SupHeaterLoad = Node( PTUnit( PTUnitNum ).AirInNode ).MassFlowRate * CpAir * ( PTUnit( PTUnitNum ).MaxSATSupHeat - Node( PTUnit( PTUnitNum ).AirOutNode ).Temp );

				} else {
					SupHeaterLoad = 0.0;
				}
			}
		}

		PTUnit( PTUnitNum ).CompPartLoadRatio = PartLoadFrac;
		PTUnit( PTUnitNum ).CompSpeedRatio = SpeedRatio;
		PTUnit( PTUnitNum ).CompSpeedNum = SpeedNum;

	}

	//******************************************************************************

	//******************************************************************************

	Real64
	VSHPCyclingResidual(
		Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = FurnaceNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:MSHPCyclingResidual
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

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
		Real64 VSHPCyclingResidual;

		// Argument array dimensioning

		// Locals
		Real64 SupHeaterLoad; // Supplemental heater load

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = Zone Num
		// par(3) = FirstHVACIteration
		// par(4) = OpMode
		// par(5) = QZnReq, load to be met
		// par(6) = OnOffAirFlowRatio
		// par(7) = SupHeaterLoad

		// par(9) = CompOp
		// par(10) = 1.0 to meet sensible load

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool HXUnitOn; // flag to enable heat exchanger
		int PTUnitNum; // MSHP index
		int ZoneNum; // Zone index
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Compressor operating mode
		Real64 QZnReq; // zone sensible load (W)
		Real64 QZnLat; // zone latent load (W)
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 ZoneSensLoadMet; // delivered sensible capacity of MSHP
		Real64 ZoneLatLoadMet; // delivered latent capacity of MSHP
		Real64 LoadToBeMet; // sensible or latent load to be met
		Real64 ResScale; // Residual scale
		int CompOp; // compressor operation; 1=on, 0=off

		PTUnitNum = int( Par( 1 ) );
		ZoneNum = int( Par( 2 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration =  ( Par( 3 ) == 1.0 );
		OpMode = int( Par( 4 ) );

		QZnReq = 0.0;
		QZnLat = 0.0;

		LoadToBeMet = Par( 5 );
		if ( Par( 10 ) == 1.0 ) {
			QZnReq = Par( 5 );
		} else {
			QZnLat = Par( 5 );
		}

		OnOffAirFlowRatio = Par( 6 );
		SupHeaterLoad = Par( 7 );
		CompOp = int( Par( 9 ) );

		HXUnitOn = ( Par( 11 ) > 0.0 );

		CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, 1, 0.0, PartLoadFrac, ZoneSensLoadMet, ZoneLatLoadMet, QZnReq, QZnLat, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		ResScale = std::abs( LoadToBeMet );
		if ( ResScale < 100.0 ) {
			ResScale = 100.0;
		}

		// Calculate residual based on output calculation flag
		if ( Par( 10 ) == 1.0 ) {
			VSHPCyclingResidual = ( ZoneSensLoadMet - LoadToBeMet ) / ResScale;
		} else {
			VSHPCyclingResidual = ( ZoneLatLoadMet - LoadToBeMet ) / ResScale;
		}

		return VSHPCyclingResidual;

	}

	//******************************************************************************

	Real64
	VSHPSpeedResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = MSHPNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, , based on HVACMultiSpeedHeatPump:MSHPVarSpeedgResidual
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

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
		Real64 VSHPSpeedResidual;

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
		// par(10) = 1.0 to meet sensible load

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool HXUnitOn; // flag to enable heat exchanger
		int PTUnitNum; // MSHP index
		int ZoneNum; // Zone index
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Compressor operating mode
		Real64 QZnReq; // zone load (W)
		Real64 QZnLat; // zone latent load (W)
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 ZoneSensLoadMet; // delivered sensible capacity of MSHP
		Real64 ZoneLatLoadMet; // delivered latent capacity of MSHP
		Real64 LoadToBeMet; // sensible or latent load to be met
		Real64 ResScale; // Residual scale
		int SpeedNum; // Speed number
		int CompOp; // compressor operation; 1=on, 0=off

		PTUnitNum = int( Par( 1 ) );
		ZoneNum = int( Par( 2 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		OpMode = int( Par( 4 ) );

		QZnReq = 0.0;
		QZnLat = 0.0;

		LoadToBeMet = Par( 5 );
		if ( Par( 10 ) == 1.0 ) {
			QZnReq = Par( 5 );
		} else {
			QZnLat = Par( 5 );
		}

		OnOffAirFlowRatio = Par( 6 );
		SupHeaterLoad = Par( 7 );
		SpeedNum = int( Par( 8 ) );
		CompOp = int( Par( 9 ) );

		HXUnitOn = ( Par( 11 ) > 0.0 );

		CalcVarSpeedHeatPump( PTUnitNum, ZoneNum, FirstHVACIteration, CompOp, SpeedNum, SpeedRatio, 1.0, ZoneSensLoadMet, ZoneLatLoadMet, QZnReq, QZnLat, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		ResScale = std::abs( LoadToBeMet );
		if ( ResScale < 100.0 ) {
			ResScale = 100.0;
		}

		// Calculate residual based on output calculation flag
		if ( Par( 10 ) == 1.0 ) {
			VSHPSpeedResidual = ( ZoneSensLoadMet - LoadToBeMet ) / ResScale;
		} else {
			VSHPSpeedResidual = ( ZoneLatLoadMet - LoadToBeMet ) / ResScale;
		}

		return VSHPSpeedResidual;

	}

	//******************************************************************************

	void
	CalcVarSpeedHeatPump(
		int const PTUnitNum, // Unit index in fan coil array
		int const ZoneNum, // Zone index
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		int const CompOp, // Compressor on/off; 1=on, 0=off
		int const SpeedNum, // Speed number
		Real64 const SpeedRatio, // Compressor speed ratio
		Real64 const PartLoadFrac, // compressor part load fraction
		Real64 & LoadMet, // load met by unit (W)
		Real64 & LatentLoadMet, // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
		Real64 const QZnReq, // Zone load (W) unused1208
		Real64 const QLatReq, // Zone latent load []
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to AVERAGE airflow over timestep
		Real64 & SupHeaterLoad, // supplemental heater load (W)
		bool const EP_UNUSED( HXUnitOn ) // flag to enable heat exchanger
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
		//       DATE WRITTEN:    March 2012
		//       MODIFIED         July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will calcultes MSHP performance based on given system load

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES: na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using DXCoils::SimDXCoil;
		using MixedAir::SimOAMixer;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using InputProcessor::SameString;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataEnvironment::OutDryBulbTemp;
		using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using DataZoneEquipment::ZoneEquipConfig;
		using SingleDuct::SimATMixer;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcVarSpeedHeatPump: " ); // for error messages
		Real64 const ErrTolerance( 0.001 ); // convergence limit for hotwater coil
		int const SolveMaxIter( 50 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // PTHP air outlet node
		int InletNode; // PTHP air inlet node
		int ZoneNode; // Zone node of zone PTHP is serving
		int ControlledZoneNum; // Index of ZoneEquipConfig that uses this heat pump
		Real64 AirMassFlow; // total supply air mass flow through the PTHP [m3/s]
		Real64 MinHumRat; // minimum humidity ratio for sensible capacity calculation (kg/kg)
		Real64 OutsideDryBulbTemp; // Outdoor air temperature at external node height
		Real64 QCoilReq; // load passed to heating coil (W)
		Real64 QActual; // actual heating coil output (W)
		int OpMode; // fan operating mode, CycFanCycCoil or ContFanCycCoil
		Real64 mdot; // local temporary for mass flow rate
		Real64 MaxHotWaterFlow; // coil maximum hot water mass flow rate, kg/s
		Real64 HotWaterMdot; // actual hot water mass flow rate
		static Array1D< Real64 > Par( 3 );
		int SolFlag;
		static int ATMixOutNode( 0 ); // outlet node of ATM Mixer

		// FLOW

		OutletNode = PTUnit( PTUnitNum ).AirOutNode;
		InletNode = PTUnit( PTUnitNum ).AirInNode;
		ControlledZoneNum = PTUnit( PTUnitNum ).CtrlZoneNum;
		ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
		OpMode = PTUnit( PTUnitNum ).OpMode;
		//  IF(PTUnit(PTUnitNum)%CondenserNodeNum .EQ. 0)THEN
		//    OutsideDryBulbTemp = OutDryBulbTemp
		//  ELSE
		//    OutsideDryBulbTemp = Node(PTUnit(PTUnitNum)%CondenserNodeNum)%Temp
		//  END IF

		OutsideDryBulbTemp = OutDryBulbTemp;

		SaveCompressorPLR = 0.0;
		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		SetVSHPAirFlow( PTUnitNum, ZoneNum, PartLoadFrac, OnOffAirFlowRatio, SpeedNum, SpeedRatio );

		AirMassFlow = Node( InletNode ).MassFlowRate;

		if ( PTUnit( PTUnitNum ).ATMixerExists ) {
			// There is an air terminal mixer
			ATMixOutNode = PTUnit( PTUnitNum ).ATMixerOutNode;
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_InletSide ) { // if there is an inlet side air terminal mixer
				// set the primary air inlet mass flow rate
				Node( PTUnit( PTUnitNum ).ATMixerPriNode ).MassFlowRate = min( Node( PTUnit( PTUnitNum ).ATMixerPriNode ).MassFlowRateMaxAvail, Node( InletNode ).MassFlowRate );
				// now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
				// the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
				SimATMixer( PTUnit( PTUnitNum ).ATMixerName, FirstHVACIteration, PTUnit( PTUnitNum ).ATMixerIndex );
			}
		} else {
			// No air terminal mixer; simulate the outside air mixer
			ATMixOutNode = 0;
			if ( PTUnit( PTUnitNum ).OutsideAirNode > 0 ) SimOAMixer( PTUnit( PTUnitNum ).OAMixName, FirstHVACIteration, PTUnit( PTUnitNum ).OAMixIndex );
		}

		// if blow through, simulate fan then coils
		if ( PTUnit( PTUnitNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( PTUnit( PTUnitNum ).FanName, FirstHVACIteration, PTUnit( PTUnitNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( CoolingLoad && OutsideDryBulbTemp > PTUnit( PTUnitNum ).MinOATCompressor ) {
			SimVariableSpeedCoils( BlankString, PTUnit( PTUnitNum ).DXCoolCoilIndexNum, PTUnit( PTUnitNum ).OpMode, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

			SaveCompressorPLR = PartLoadFrac;
		} else { // cooling coil is off
			SimVariableSpeedCoils( BlankString, PTUnit( PTUnitNum ).DXCoolCoilIndexNum, PTUnit( PTUnitNum ).OpMode, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRatio );
		}

		if ( PTUnit( PTUnitNum ).UnitType_Num != PTACUnit ) { // PTHP
			if ( HeatingLoad ) {
				SimVariableSpeedCoils( BlankString, PTUnit( PTUnitNum ).DXHeatCoilIndex, PTUnit( PTUnitNum ).OpMode, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, CompOp, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

				SaveCompressorPLR = PartLoadFrac;
			} else {
				//   heating coil is off
				SimVariableSpeedCoils( BlankString, PTUnit( PTUnitNum ).DXHeatCoilIndex, PTUnit( PTUnitNum ).OpMode, PTUnit( PTUnitNum ).MaxONOFFCyclesperHour, PTUnit( PTUnitNum ).HPTimeConstant, PTUnit( PTUnitNum ).FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRatio );
			}
		} else { //PTAC
			if ( HeatingLoad ) {
				if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
					QCoilReq = PTUnit( PTUnitNum ).ACHeatCoilCap * PartLoadFrac;
					if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingGas || PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingElectric ) {
						SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, QCoilReq, PTUnit( PTUnitNum ).ACHeatCoilIndex, QActual, false, OpMode, PartLoadFrac );
					} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {
						//       set water inlet node mass flow rate proportional to PLR. Limit water flow rate based on "available" upper limit.
						mdot = PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow * PartLoadFrac;

						SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

						SimulateWaterCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QActual, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
					} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {
						//       set steam inlet node mass flow rate proportional to PLR. Limit steam flow rate based on "available" upper limit.
						mdot = PTUnit( PTUnitNum ).MaxHeatCoilFluidFlow * PartLoadFrac;
						SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

						SimulateSteamCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QCoilReq, QActual, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
					}
				}
			} else {
				//   heating coil is off
				if ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) {
					QCoilReq = 0.0;
					if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingGas || PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingElectric ) {
						SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, QCoilReq, PTUnit( PTUnitNum ).ACHeatCoilIndex );
					} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingWater ) {
						mdot = 0.0;
						SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

						SimulateWaterCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex );
					} else if ( PTUnit( PTUnitNum ).ACHeatCoilType_Num == Coil_HeatingSteam ) {
						mdot = 0.0;
						SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

						SimulateSteamCoilComponents( PTUnit( PTUnitNum ).ACHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).ACHeatCoilIndex, QCoilReq, QActual, PTUnit( PTUnitNum ).OpMode, PartLoadFrac );
					}
				}
			}
		}

		// if draw through, simulate coils then fan
		if ( PTUnit( PTUnitNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( PTUnit( PTUnitNum ).FanName, FirstHVACIteration, PTUnit( PTUnitNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( PTUnit( PTUnitNum ).SuppHeatCoilIndex > 0 ) {
			if ( SupHeaterLoad < SmallLoad ) {
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).SuppHeatCoilType_Num );
				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, SupHeaterLoad, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, true, PTUnit( PTUnitNum ).OpMode );
				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, PTUnit( PTUnitNum ).OpMode );
				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, QActual, PTUnit( PTUnitNum ).OpMode );
				}}
			} else {
				{ auto const SELECT_CASE_var( PTUnit( PTUnitNum ).SuppHeatCoilType_Num );
				if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
					SimulateHeatingCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, SupHeaterLoad, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, true, PTUnit( PTUnitNum ).OpMode );
				} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
					MaxHotWaterFlow = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
					SetComponentFlowRate( MaxHotWaterFlow, PTUnit( PTUnitNum ).HotWaterControlNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );
					QActual = SupHeaterLoad;
					// simulate the hot water supplemental heating coil
					SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, PTUnit( PTUnitNum ).OpMode );
					if ( QActual > ( SupHeaterLoad + SmallLoad ) ) {
						// control water flow to obtain output matching SupHeaterLoad
						SolFlag = 0;
						MinWaterFlow = 0.0;
						Par( 1 ) = double( PTUnitNum );
						if ( FirstHVACIteration ) {
							Par( 2 ) = 1.0;
						} else {
							Par( 2 ) = 0.0;
						}
						Par( 3 ) = SupHeaterLoad;
						MaxHotWaterFlow = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
						SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par );
						if ( SolFlag == -1 ) {
							if ( PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex == 0 ) {
								ShowWarningMessage( "RoutineName//Hot water coil control failed for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name + "\"" ); //Autodesk:Bug? Meant RoutineName + "Hot water...
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "  Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating hot water mass flow rate" );
							}
							ShowRecurringWarningErrorAtEnd( "RoutineName//Hot water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name, PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex ); //Autodesk:Bug? Meant RoutineName + "Hot water...
						} else if ( SolFlag == -2 ) {
							if ( PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex2 == 0 ) {
								ShowWarningMessage( "RoutineName//Hot water coil control failed (maximum flow limits) for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name + "\"" ); //Autodesk:Bug? Meant RoutineName + "Hot water...
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "...Bad hot water maximum flow rate limits" );
								ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinWaterFlow, 3 ) + " kg/s" );
								ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxHotWaterFlow, 3 ) + " kg/s" );
							}
							ShowRecurringWarningErrorAtEnd( "RoutineName//Hot water coil control failed (flow limits) for " + PTUnit( PTUnitNum ).UnitType + "=\"" + PTUnit( PTUnitNum ).Name + "\"", PTUnit( PTUnitNum ).HotWaterCoilMaxIterIndex2, MaxHotWaterFlow, MinWaterFlow, _, "[kg/s]", "[kg/s]" ); //Autodesk:Bug? Meant RoutineName + "Hot water...
						}
						QActual = SupHeaterLoad;
						// simulate the hot water supplemental heating coil
						SimulateWaterCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, QActual, PTUnit( PTUnitNum ).OpMode );
					}
				} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
					mdot = PTUnit( PTUnitNum ).MaxSuppCoilFluidFlow;
					SetComponentFlowRate( mdot, PTUnit( PTUnitNum ).HWCoilSteamInletNode, PTUnit( PTUnitNum ).PlantCoilOutletNode, PTUnit( PTUnitNum ).LoopNum, PTUnit( PTUnitNum ).LoopSide, PTUnit( PTUnitNum ).BranchNum, PTUnit( PTUnitNum ).CompNum );

					// simulate the steam supplemental heating coil
					SimulateSteamCoilComponents( PTUnit( PTUnitNum ).SuppHeatCoilName, FirstHVACIteration, PTUnit( PTUnitNum ).SuppHeatCoilIndex, SupHeaterLoad, QActual, PTUnit( PTUnitNum ).OpMode );
				}}
			}
		}

		// If there is a supply side air terminal mixer, calculate its output
		if ( PTUnit( PTUnitNum ).ATMixerExists ) {
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				SimATMixer( PTUnit( PTUnitNum ).ATMixerName, FirstHVACIteration, PTUnit( PTUnitNum ).ATMixerIndex );
			}
		}

		// calculate sensible load met
		if ( PTUnit( PTUnitNum ).ATMixerExists ) {
			if ( PTUnit( PTUnitNum ).ATMixerType == ATMixer_SupplySide ) {
				// Air terminal supply side mixer
				LoadMet = Node( ATMixOutNode ).MassFlowRate * ( PsyHFnTdbW( Node( ATMixOutNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
			} else {
				// Air terminal inlet side mixer
				LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
			}
		} else {
			MinHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );
		}

		LatentLoadMet = 0.0;

	}

	void
	SetVSHPAirFlow(
		int const PTUnitNum, // Unit index
		int const EP_UNUSED( ZoneNum ), // Zone index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to average airflow over timestep
		Optional_int_const SpeedNum, // Speed number
		Optional< Real64 const > SpeedRatio // Speed ratio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:SetAverageAirFlow
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// Set the average air mass flow rates using the part load fraction of the heat pump for this time step
		// Set OnOffAirFlowRatio to be used by DX coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::CurDeadBandOrSetback;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVMS TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // inlet node number for PTUnitNum
		int OutsideAirNode; // outside air node number in PTHP loop
		int AirRelNode; // relief air node number in PTHP loop
		static Real64 AverageUnitMassFlow( 0.0 ); // average supply air mass flow rate over time step
		static Real64 AverageOAMassFlow( 0.0 ); // average outdoor air mass flow rate over time step

		MSHPMassFlowRateLow = 0.0; // Mass flow rate at low speed
		MSHPMassFlowRateHigh = 0.0; // Mass flow rate at high speed

		InletNode = PTUnit( PTUnitNum ).AirInNode;
		OutsideAirNode = PTUnit( PTUnitNum ).OutsideAirNode;
		AirRelNode = PTUnit( PTUnitNum ).AirReliefNode;

		AverageOAMassFlow = ( PartLoadRatio * OACompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * OACompOffMassFlow );

		if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil ) {
			CompOffMassFlow = PTUnit( PTUnitNum ).IdleMassFlowRate;
			CompOffFlowRatio = PTUnit( PTUnitNum ).IdleSpeedRatio;
		} else {
			CompOffMassFlow = 0.0;
			CompOffFlowRatio = 0.0;
		}

		if ( HeatingLoad && ( PTUnit( PTUnitNum ).UnitType_Num == PTACUnit ) ) {
			CompOnMassFlow = PTUnit( PTUnitNum ).CoolMassFlowRate( PTUnit( PTUnitNum ).NumOfSpeedCooling );
			CompOnFlowRatio = PTUnit( PTUnitNum ).MSCoolingSpeedRatio( PTUnit( PTUnitNum ).NumOfSpeedCooling );
			MSHPMassFlowRateLow = PTUnit( PTUnitNum ).CoolMassFlowRate( PTUnit( PTUnitNum ).NumOfSpeedCooling );
			MSHPMassFlowRateHigh = PTUnit( PTUnitNum ).CoolMassFlowRate( PTUnit( PTUnitNum ).NumOfSpeedCooling );
			AverageUnitMassFlow = ( PartLoadRatio * CompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * CompOffMassFlow );
			if ( CompOffFlowRatio > 0.0 ) {
				FanSpeedRatio = ( PartLoadRatio * CompOnFlowRatio ) + ( ( 1 - PartLoadRatio ) * CompOffFlowRatio );
			} else {
				FanSpeedRatio = CompOnFlowRatio;
			}
		} else {
			if ( present( SpeedNum ) ) {
				if ( HeatingLoad ) {
					if ( SpeedNum == 1 ) {
						CompOnMassFlow = PTUnit( PTUnitNum ).HeatMassFlowRate( SpeedNum );
						CompOnFlowRatio = PTUnit( PTUnitNum ).MSHeatingSpeedRatio( SpeedNum );
						MSHPMassFlowRateLow = PTUnit( PTUnitNum ).HeatMassFlowRate( 1 );
						MSHPMassFlowRateHigh = PTUnit( PTUnitNum ).HeatMassFlowRate( 1 );
					} else if ( SpeedNum > 1 ) {
						CompOnMassFlow = SpeedRatio * PTUnit( PTUnitNum ).HeatMassFlowRate( SpeedNum ) + ( 1.0 - SpeedRatio ) * PTUnit( PTUnitNum ).HeatMassFlowRate( SpeedNum - 1 );
						CompOnFlowRatio = SpeedRatio * PTUnit( PTUnitNum ).MSHeatingSpeedRatio( SpeedNum ) + ( 1.0 - SpeedRatio ) * PTUnit( PTUnitNum ).MSHeatingSpeedRatio( SpeedNum - 1 );
						MSHPMassFlowRateLow = PTUnit( PTUnitNum ).HeatMassFlowRate( SpeedNum - 1 );
						MSHPMassFlowRateHigh = PTUnit( PTUnitNum ).HeatMassFlowRate( SpeedNum );
					}
				} else if ( PTUnit( PTUnitNum ).HeatCoolMode == CoolingMode ) {
					if ( SpeedNum == 1 ) {
						CompOnMassFlow = PTUnit( PTUnitNum ).CoolMassFlowRate( SpeedNum );
						CompOnFlowRatio = PTUnit( PTUnitNum ).MSCoolingSpeedRatio( SpeedNum );
						MSHPMassFlowRateLow = PTUnit( PTUnitNum ).CoolMassFlowRate( 1 );
						MSHPMassFlowRateHigh = PTUnit( PTUnitNum ).CoolMassFlowRate( 1 );
					} else if ( SpeedNum > 1 ) {
						CompOnMassFlow = SpeedRatio * PTUnit( PTUnitNum ).CoolMassFlowRate( SpeedNum ) + ( 1.0 - SpeedRatio ) * PTUnit( PTUnitNum ).CoolMassFlowRate( SpeedNum - 1 );
						CompOnFlowRatio = SpeedRatio * PTUnit( PTUnitNum ).MSCoolingSpeedRatio( SpeedNum ) + ( 1.0 - SpeedRatio ) * PTUnit( PTUnitNum ).MSCoolingSpeedRatio( SpeedNum - 1 );
						MSHPMassFlowRateLow = PTUnit( PTUnitNum ).CoolMassFlowRate( SpeedNum - 1 );
						MSHPMassFlowRateHigh = PTUnit( PTUnitNum ).CoolMassFlowRate( SpeedNum );
					}
				}
			}

			// Set up fan flow rate during compressor off time
			if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil && present( SpeedNum ) ) {
				if ( PTUnit( PTUnitNum ).AirFlowControl == UseCompressorOnFlow && CompOnMassFlow > 0.0 ) {
					if ( SpeedNum == 1 ) { //LOWEST SPEED USE IDLE FLOW
						CompOffMassFlow = PTUnit( PTUnitNum ).IdleMassFlowRate;
						CompOffFlowRatio = PTUnit( PTUnitNum ).IdleSpeedRatio;
					} else if ( PTUnit( PTUnitNum ).LastMode == HeatingMode ) {
						CompOffMassFlow = PTUnit( PTUnitNum ).HeatMassFlowRate( SpeedNum );
						CompOffFlowRatio = PTUnit( PTUnitNum ).MSHeatingSpeedRatio( SpeedNum );
					} else {
						CompOffMassFlow = PTUnit( PTUnitNum ).CoolMassFlowRate( SpeedNum );
						CompOffFlowRatio = PTUnit( PTUnitNum ).MSCoolingSpeedRatio( SpeedNum );
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
		}

		if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).SchedPtr ) > 0.0 && ( ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).FanAvailSchedPtr ) > 0.0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOff ) ) {

			Node( InletNode ).MassFlowRate = AverageUnitMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = AverageUnitMassFlow;
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = AverageOAMassFlow;
				Node( OutsideAirNode ).MassFlowRateMaxAvail = AverageOAMassFlow;
				Node( AirRelNode ).MassFlowRate = AverageOAMassFlow;
				Node( AirRelNode ).MassFlowRateMaxAvail = AverageOAMassFlow;
			}
			if ( AverageUnitMassFlow > 0.0 ) {
				OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
			} else {
				OnOffAirFlowRatio = 0.0;
			}

		} else {

			Node( InletNode ).MassFlowRate = 0.0;
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = 0.0;
				Node( AirRelNode ).MassFlowRate = 0.0;
			}
			OnOffAirFlowRatio = 0.0;

		}
	}

	void
	SetOnOffMassFlowRateVSCoil(
		int const PTUnitNum, // index to furnace
		int const ZoneNum, // index to zone
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		int const EP_UNUSED( AirLoopNum ), // index to air loop !unused1208
		Real64 & OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
		int const EP_UNUSED( OpMode ), // fan operating mode
		Real64 const EP_UNUSED( QZnReq ), // sensible load to be met (W) !unused1208
		Real64 const EP_UNUSED( MoistureLoad ), // moisture load to be met (W)
		Real64 & PartLoadRatio // coil part-load ratio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Furnace Components.

		// METHODOLOGY EMPLOYED:
		// The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
		// in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
		// air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
		// based on PLR.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // Inlet node number in MSHP loop
		int OutNode; // Outlet node number in MSHP loop

		InNode = PTUnit( PTUnitNum ).AirInNode;
		OutNode = PTUnit( PTUnitNum ).AirOutNode;

		SetOnOffMassFlowRate( PTUnitNum, PartLoadRatio, OnOffAirFlowRatio );
		//INTIALIZE FIXED SPEED FIRST, AND OVER-WRITE USING MUL-SPEED

		// FLOW:

		if ( CoolingLoad ) {
			PTUnit( PTUnitNum ).HeatCoolMode = CoolingMode;
		} else if ( HeatingLoad ) {
			PTUnit( PTUnitNum ).HeatCoolMode = HeatingMode;
		} else {
			PTUnit( PTUnitNum ).HeatCoolMode = 0;
		}

		// Set the inlet node mass flow rate
		if ( PTUnit( PTUnitNum ).OpMode == ContFanCycCoil ) {
			// constant fan mode
			if ( ( PTUnit( PTUnitNum ).HeatCoolMode == HeatingMode ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).HeatMassFlowRate( 1 );
				CompOnFlowRatio = PTUnit( PTUnitNum ).MSHeatingSpeedRatio( 1 );
				PTUnit( PTUnitNum ).LastMode = HeatingMode;
			} else if ( ( PTUnit( PTUnitNum ).HeatCoolMode == CoolingMode ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).CoolMassFlowRate( 1 );
				CompOnFlowRatio = PTUnit( PTUnitNum ).MSCoolingSpeedRatio( 1 );
				PTUnit( PTUnitNum ).LastMode = CoolingMode;
			} else {
				CompOnMassFlow = PTUnit( PTUnitNum ).IdleMassFlowRate;
				CompOnFlowRatio = PTUnit( PTUnitNum ).IdleSpeedRatio;
			}
			CompOffMassFlow = PTUnit( PTUnitNum ).IdleMassFlowRate;
			CompOffFlowRatio = PTUnit( PTUnitNum ).IdleSpeedRatio;
		} else {
			// cycling fan mode
			if ( ( PTUnit( PTUnitNum ).HeatCoolMode == HeatingMode ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).HeatMassFlowRate( 1 );
				CompOnFlowRatio = PTUnit( PTUnitNum ).MSHeatingSpeedRatio( 1 );
			} else if ( ( PTUnit( PTUnitNum ).HeatCoolMode == CoolingMode ) && ! CurDeadBandOrSetback( ZoneNum ) ) {
				CompOnMassFlow = PTUnit( PTUnitNum ).CoolMassFlowRate( 1 );
				CompOnFlowRatio = PTUnit( PTUnitNum ).MSCoolingSpeedRatio( 1 );
			} else {
				CompOnMassFlow = 0.0;
				CompOnFlowRatio = 0.0;
			}
			CompOffMassFlow = 0.0;
			CompOffFlowRatio = 0.0;
		}

		// Set the inlet node mass flow rate
		if ( GetCurrentScheduleValue( PTUnit( PTUnitNum ).FanAvailSchedPtr ) > 0.0 && CompOnMassFlow != 0.0 ) {
			OnOffAirFlowRatio = 1.0;
			if ( FirstHVACIteration ) {
				Node( InNode ).MassFlowRate = CompOnMassFlow;
				PartLoadRatio = 0.0;
			} else {
				if ( PTUnit( PTUnitNum ).HeatCoolMode != 0 ) {
					PartLoadRatio = 1.0;
				} else {
					PartLoadRatio = 0.0;
				}
			}
		} else {
			PartLoadRatio = 0.0;
			Node( InNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRateMaxAvail = 0.0;
			OnOffAirFlowRatio = 1.0;
		}

		// Set the system mass flow rates
		SetVSHPAirFlow( PTUnitNum, ZoneNum, PartLoadRatio, OnOffAirFlowRatio );

	}

} // PackagedTerminalHeatPump

} // EnergyPlus
