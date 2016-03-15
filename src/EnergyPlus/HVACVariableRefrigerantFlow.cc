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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HVACVariableRefrigerantFlow.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
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
#include <EMSManager.hh>
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
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace HVACVariableRefrigerantFlow {
	// Module containing the Variable Refrigerant Flow (VRF or VRV) simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad, FSEC
	//       DATE WRITTEN   August 2010
	//       MODIFIED       Apr 2012, R. Raustad, FSEC, Added Heat Recovery Operating Mode
	//                      Jul 2015, RP Zhang, XF Pang, LBNL, Added a new physics based VRF model applicable for Fluid Temperature Control
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the VRF System Component

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataGlobals;
	using namespace DataLoopNode;
	using namespace DataHVACGlobals;
	using namespace DataPrecisionGlobals;
	using namespace DataZoneEnergyDemands;
	using namespace Psychrometrics;
	using namespace DataPlant;

	// Use statements for access to subroutines in other modules
	// na

	// Data
	//MODULE PARAMETER DEFINITIONS
	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	//Heat Recovery System used
	int const No( 1 ); // Heat Pump mode only
	int const Yes( 2 ); // Heat Pump or Heat Recovery Mode (not available at this time)

	// Defrost strategy
	int const ReverseCycle( 1 ); // uses reverse cycle defrost strategy
	int const Resistive( 2 ); // uses electric resistance heater for defrost

	// Defrost control
	int const Timed( 1 ); // defrost cycle is timed
	int const OnDemand( 2 ); // defrost cycle occurs only when required

	// Thermostat Priority Control Type
	int const LoadPriority( 1 ); // total of zone loads dictate operation in cooling or heating
	int const ZonePriority( 2 ); // # of zones requireing cooling or heating dictate operation in cooling or heating
	int const ThermostatOffsetPriority( 3 ); // zone with largest deviation from setpoint dictates operation
	int const ScheduledPriority( 4 ); // cooling and heating modes are scheduled
	int const MasterThermostatPriority( 5 ); // Master zone thermostat dictates operation
	int const FirstOnPriority( 6 ); // first unit to respond dictates operation (not used at this time)

	//Water Systems
	int const CondensateDiscarded( 1001 ); // default mode where water is "lost"
	int const CondensateToTank( 1002 ); // collect coil condensate from air and store in water storage tank

	int const WaterSupplyFromMains( 101 ); // mains water line used as water source
	int const WaterSupplyFromTank( 102 ); // storage tank used as water source

	Real64 const MaxCap( 1.0e+20 ); // limit of zone terminal unit capacity

	// VRF System Types (strings used in integer conversions)
	int const NumVRFSystemTypes( 1 );
	int const VRF_HeatPump( 1 );
	Array1D_string const cVRFTypes( NumVRFSystemTypes, std::string( "AirConditioner:VariableRefrigerantFlow" ) );

	int const NumValidFuelTypes( 9 );
	Array1D_string const cValidFuelTypes( NumValidFuelTypes, { "Electric", "NaturalGas", "PropaneGas", "Diesel", "Gasoline", "FuelOil#1", "FuelOil#2", "OtherFuel1", "OtherFuel2" } );

	// VRF Algorithm Type
	int const AlgorithmTypeSysCurve( 1 ); // VRF model based on system curve
	int const AlgorithmTypeFluidTCtrl( 2 ); // VRF model based on physics, appliable for Fluid Temperature Control

	// Fuel Types
	int const FuelTypeElectric( 1 ); // Fuel type for electricity
	int const FuelTypeNaturalGas( 2 ); // Fuel type for natural gas
	int const FuelTypePropaneGas( 3 ); // Fuel type for propane gas
	int const FuelTypeDiesel( 4 ); // Fuel type for diesel
	int const FuelTypeGasoline( 5 ); // Fuel type for gasoline
	int const FuelTypeFuelOil1( 6 ); // Fuel type for fuel oil #1
	int const FuelTypeFuelOil2( 7 ); // Fuel type for fuel oil #2
	int const FuelTypeOtherFuel1( 8 ); // Fuel type for other fuel #1
	int const FuelTypeOtherFuel2( 9 ); // Fuel type for other fuel #2

	// curve type for equivalent piping losses (not necessarily the same value used in CurveManager)
	int const BiQuadratic( 4 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	bool GetVRFInputFlag( true ); // Flag set to make sure you get input once
	bool MyOneTimeFlag( true ); // One time flag used to allocate MyEnvrnFlag and MySizeFlag
	bool MyOneTimeSizeFlag( true ); // One time flag used to allocate MyEnvrnFlag and MySizeFlag
	bool ZoneEquipmentListNotChecked( true ); // False after the Zone Equipment List has been checked for items
	int NumVRFCond( 0 ); // total number of VRF condensers (All VRF Algorithm Types)
	int NumVRFCond_SysCurve( 0 ); // total number of VRF condensers with VRF Algorithm Type 1
	int NumVRFCond_FluidTCtrl( 0 ); // total number of VRF condensers with VRF Algorithm Type 2
	int NumVRFTU( 0 ); // total number of VRF terminal units
	int NumVRFTULists( 0 ); // The number of VRF TU lists
	Real64 CompOnMassFlow( 0.0 ); // Supply air mass flow rate w/ compressor ON
	Real64 OACompOnMassFlow( 0.0 ); // OA mass flow rate w/ compressor ON
	Real64 CompOffMassFlow( 0.0 ); // Supply air mass flow rate w/ compressor OFF
	Real64 OACompOffMassFlow( 0.0 ); // OA mass flow rate w/ compressor OFF
	Real64 CompOnFlowRatio( 0.0 ); // fan flow ratio when coil on
	Real64 CompOffFlowRatio( 0.0 ); // fan flow ratio when coil off
	Real64 FanSpeedRatio( 0.0 ); // ratio of air flow ratio passed to fan object
	Real64 LoopDXCoolCoilRTF( 0.0 ); // holds value of DX cooling coil RTF
	Real64 LoopDXHeatCoilRTF( 0.0 ); // holds value of DX heating coil RTF
	Real64 CondenserWaterMassFlowRate( 0.0 ); // VRF water-cooled condenser mass flow rate (kg/s)
	Array1D_bool HeatingLoad; // defines a heating load on VRFTerminalUnits
	Array1D_bool CoolingLoad; // defines a cooling load on VRFTerminalUnits
	Array1D_bool LastModeHeating; // defines last mode was heating mode
	Array1D_bool LastModeCooling; // defines last mode was cooling mode
	Array1D_bool CheckEquipName; // Flag set to check equipment connections once
	Array1D_bool MyEnvrnFlag; // Flag for initializing at beginning of each new environment
	Array1D_bool MySizeFlag; // False after TU has been sized
	Array1D_bool MyBeginTimeStepFlag; // Flag to sense beginning of time step
	Array1D_bool MyVRFFlag; // used for sizing VRF inputs one time
	Array1D_bool MyVRFCondFlag; // used to reset timer counter
	Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
	Array1D_int NumCoolingLoads; // number of TU's requesting cooling
	Array1D_int NumHeatingLoads; // number of TU's requesting heating
	Array1D< Real64 > MaxCoolingCapacity; // maximum capacity of any terminal unit
	Array1D< Real64 > MaxHeatingCapacity; // maximum capacity of any terminal unit
	Array1D< Real64 > CoolCombinationRatio; // ratio of terminal unit capacity to VRF condenser capacity
	Array1D< Real64 > HeatCombinationRatio; // ratio of terminal unit capacity to VRF condenser capacity
	Array1D< Real64 > MaxDeltaT; // maximum zone temperature difference from setpoint
	Array1D< Real64 > MinDeltaT; // minimum zone temperature difference from setpoint
	Array1D< Real64 > SumCoolingLoads; // sum of cooling loads
	Array1D< Real64 > SumHeatingLoads; // sum of heating loads

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes
	//Private UpdateVRF

	// Reporting routines for module

	// Object Data
	Array1D< VRFCondenserEquipment > VRF; // AirConditioner:VariableRefrigerantFlow object
	Array1D< VRFTerminalUnitEquipment > VRFTU; // ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object
	Array1D< TerminalUnitListData > TerminalUnitList; // zoneTerminalUnitList object
	Array1D< VRFTUNumericFieldData > VRFTUNumericFields; // holds VRF TU numeric input fields character field name

	// Utility routines for module
	// na

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimulateVRF(
		std::string const & CompName,
		int const ZoneNum,
		bool const FirstHVACIteration,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		// AUTHOR         Richard Raustad, FSEC
		// DATE WRITTEN   August 2010
		// MODIFIED       Jul 2015, RP Zhang (LBNL), XF Pang (LBNL), Y Yura (Daikin Inc). Add a physics based VRF model appliable for Fluid Temperature Co
		// RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages VRF terminal unit simulation.

		// METHODOLOGY EMPLOYED:
		// Simulate all terminal units
		// Once all terminal units have been simulated, simulate VRF condenser

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DXCoils::DXCoilTotalCooling;
		using DXCoils::DXCoilTotalHeating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int VRFTUNum; // current VRF system terminal unit index
		int VRFCondenser; // index to VRF AC system object - AirConditioner:VariableRefrigerantFlow
		int TUListNum; // index to VRF AC system terminal unit list
		int IndexToTUInTUList; // index to pointer in VRF AC system terminal unit list
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		int DXCoolingCoilIndex; // index to this terminal units DX cooling coil
		int DXHeatingCoilIndex; // index to this terminal units DX heating coil
		Real64 QZnReq;

		// FLOW:

		// Obtains and Allocates VRF system related parameters from input file
		if ( GetVRFInputFlag ) { //First time subroutine has been entered
			GetVRFInput();
			GetVRFInputFlag = false;
		}

		// CompIndex accounting
		if ( CompIndex == 0 ) {
			VRFTUNum = FindItemInList( CompName, VRFTU );
			if ( VRFTUNum == 0 ) {
				ShowFatalError( "SimulateVRF: VRF Terminal Unit not found=" + CompName );
			}
			CompIndex = VRFTUNum;

		} else {
			VRFTUNum = CompIndex;
			if ( VRFTUNum > NumVRFTU || VRFTUNum < 1 ) {
				ShowFatalError( "SimulateVRF: Invalid CompIndex passed=" + TrimSigDigits( VRFTUNum ) + ", Number of VRF Terminal Units = " + TrimSigDigits( NumVRFTU ) + ", VRF Terminal Unit name = " + CompName );
			}
			if ( CheckEquipName( VRFTUNum ) ) {
				if ( ! CompName.empty() && CompName != VRFTU( VRFTUNum ).Name ) {
					ShowFatalError( "SimulateVRF: Invalid CompIndex passed=" + TrimSigDigits( VRFTUNum ) + ", VRF Terminal Unit name=" + CompName + ", stored VRF TU Name for that index=" + VRFTU( VRFTUNum ).Name );
				}
				CheckEquipName( VRFTUNum ) = false;
			}
		}

		// the VRF condenser index
		VRFCondenser = VRFTU( VRFTUNum ).VRFSysNum;
		// the terminal unit list object index
		TUListNum = VRFTU( VRFTUNum ).TUListIndex;
		// the entry number in the terminal unit list (which item in the terminal unit list, e.g. second in list)
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		// index to cooling coil (coil is optional but at least one must be present)
		DXCoolingCoilIndex = VRFTU( VRFTUNum ).CoolCoilIndex;
		// index to heating coil (coil is optional but at least one must be present)
		DXHeatingCoilIndex = VRFTU( VRFTUNum ).HeatCoilIndex;
		QZnReq = 0.0;

		// Initialize terminal unit
		InitVRF( VRFTUNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq ); // Initialize all VRFTU related parameters

		// Simulate terminal unit
		SimVRF( VRFTUNum, FirstHVACIteration, OnOffAirFlowRatio, SysOutputProvided, LatOutputProvided, QZnReq );

		// mark this terminal unit as simulated
		TerminalUnitList( TUListNum ).IsSimulated( IndexToTUInTUList ) = true;

		// keep track of individual coil loads
		if ( DXCoolingCoilIndex > 0 ) {
			TerminalUnitList( TUListNum ).TotalCoolLoad( IndexToTUInTUList ) = DXCoilTotalCooling( DXCoolingCoilIndex );
		} else {
			TerminalUnitList( TUListNum ).TotalCoolLoad( IndexToTUInTUList ) = 0.0;
		}
		if ( DXHeatingCoilIndex > 0 ) {
			TerminalUnitList( TUListNum ).TotalHeatLoad( IndexToTUInTUList ) = DXCoilTotalHeating( DXHeatingCoilIndex );
		} else {
			TerminalUnitList( TUListNum ).TotalHeatLoad( IndexToTUInTUList ) = 0.0;
		}

		// Update the current VRF terminal unit to the outlet nodes
		//  CALL UpdateVRF(VRFTUNum)

		// Report the current VRF terminal unit
		ReportVRFTerminalUnit( VRFTUNum );

		// make sure all TU in a list are able to get simulated, otherwise condenser is never simulated **
		// either fatal on GetInput, or keep track of unused TU's and set their respective flag to TRUE **
		// after all VRF terminal units have been simulated, call the VRF condenser model
		if ( all( TerminalUnitList( TUListNum ).IsSimulated ) ) {

			if ( VRF( VRFCondenser ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
			// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
				CalcVRFCondenser_FluidTCtrl( VRFCondenser, FirstHVACIteration ); // Analyze the VRF OU operations
				CalcVRFIUTeTc_FluidTCtrl( VRFCondenser ); // Get the VRF IU Te/Tc
			} else {
			// Algorithm Type: VRF model based on system curve
				CalcVRFCondenser( VRFCondenser, FirstHVACIteration );
			}

			ReportVRFCondenser( VRFCondenser );

			if ( VRF( VRFCondenser ).CondenserType == WaterCooled ) UpdateVRFCondenser( VRFCondenser );
		}

	}

	void
	SimVRFCondenserPlant(
		std::string const & VRFType, // Type of VRF
		int const VRFTypeNum, // Type of VRF in Plant equipment
		std::string const & VRFName, // User Specified Name of VRF
		int & VRFNum, // Index of Equipment
		bool const FirstHVACIteration, // Flag for first time through HVAC simulation
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 const EP_UNUSED( MyLoad ), // Loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of GSHP [W]
		Real64 & MinCap, // Minimum operating capacity of GSHP [W]
		Real64 & OptCap, // Optimal operating capacity of GSHP [W]
		int const LoopNum // The calling loop number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   May 2012
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages water-source VRF condenser

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using namespace DataEnvironment;
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
		// na

		//Get input from VRF
		if ( GetVRFInputFlag ) { //First time subroutine has been entered
			GetVRFInput();
			GetVRFInputFlag = false;
		}

		if ( InitLoopEquip ) {
			VRFNum = FindItemInList( VRFName, VRF );
			if ( VRFNum != 0 ) { // if 0, fall through to next
				{ auto const SELECT_CASE_var( VRFTypeNum );
				if ( SELECT_CASE_var == TypeOf_HeatPumpVRF ) {
					MinCap = 0.0;
					MaxCap = max( VRF( VRFNum ).CoolingCapacity, VRF( VRFNum ).HeatingCapacity ); // greater of cooling and heating capacity
					OptCap = max( VRF( VRFNum ).CoolingCapacity, VRF( VRFNum ).HeatingCapacity ); // connects to single loop, need to switch between cooling/heating capacity?
				} else {
					ShowFatalError( "SimVRFCondenserPlant: Module called with incorrect VRFType=" + VRFType );
				}}
				SizeVRFCondenser( VRFNum );
				return;
			}
		}

		// Calculate Demand on heat pump
		{ auto const SELECT_CASE_var( VRFTypeNum );
		if ( SELECT_CASE_var == TypeOf_HeatPumpVRF ) {
			if ( VRFNum != 0 ) {
				if ( LoopNum == VRF( VRFNum ).SourceLoopNum ) { // condenser loop
					UpdateChillerComponentCondenserSide( VRF( VRFNum ).SourceLoopNum, VRF( VRFNum ).SourceLoopSideNum, TypeOf_HeatPumpVRF, VRF( VRFNum ).CondenserNodeNum, VRF( VRFNum ).CondenserOutletNodeNum, VRF( VRFNum ).QCondenser, VRF( VRFNum ).CondenserInletTemp, VRF( VRFNum ).CondenserSideOutletTemp, VRF( VRFNum ).WaterCondenserMassFlow, FirstHVACIteration );

				} else {
					ShowFatalError( "SimVRFCondenserPlant:: Invalid loop connection " + cVRFTypes( VRF_HeatPump ) + ", Requested Unit=" + VRFName );
				}
			} else {
				ShowFatalError( "SimVRFCondenserPlant:: Invalid " + cVRFTypes( VRF_HeatPump ) + ", Requested Unit=" + VRFName );
			}
		} else {
			ShowFatalError( "SimVRFCondenserPlant: Module called with incorrect VRFType=" + VRFType );
		}}

	}

	void
	CalcVRFCondenser(
		int const VRFCond, // index to VRF condenser
		bool const EP_UNUSED( FirstHVACIteration ) // flag for first time through HVAC system simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad, FSEC
		//       DATE WRITTEN   September 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Model the interactions of VRF terminal units with a single variable-speed condenser.
		// The terminal units are simulated first, and then the condenser is simulated.
		// If terminal units require more capacity than can be delivered by condenser, a limit is set.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using Psychrometrics::RhoH2O;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutWetBulbTemp;
		using DXCoils::DXCoilCoolInletAirWBTemp;
		using DXCoils::DXCoilHeatInletAirDBTemp;
		using DXCoils::DXCoilHeatInletAirWBTemp;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "VRFCondenser" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TUListNum; // index to TU List
		int NumTUInList; // number of terminal units is list
		int NumTU; // loop counter
		int TUIndex; // Index to terminal unit
		int CoolCoilIndex; // index to cooling coil in terminal unit
		int HeatCoilIndex; // index to heating coil in terminal unit
		int NumTUInCoolingMode; // number of terminal units actually cooling
		int NumTUInHeatingMode; // number of terminal units actually heating

		Real64 TUCoolingLoad; // DX cooling coil load to be met by condenser (W)
		Real64 TUHeatingLoad; // DX heating coil load to be met by condenser (W)
		Real64 TUParasiticPower; // total terminal unit parasitic power (W)
		Real64 TUFanPower; // total terminal unit fan power (W)
		Real64 TotCoolCapTempModFac; // cooling CAPFT curve output
		Real64 TotHeatCapTempModFac; // heating CAPFT curve output
		Real64 TotCoolEIRTempModFac; // cooling EIRFT curve output
		Real64 TotHeatEIRTempModFac; // heating EIRFT curve output
		Real64 InletAirWetBulbC; // coil inlet air wet-bulb temperature (C)
		Real64 InletAirDryBulbC; // coil inlet air dry-bulb temperature (C)
		Real64 CondInletTemp( 0.0 ); // condenser inlet air temperature (C)
		Real64 CondInletHumRat; // condenser inlet air humidity ratio (kg/kg)
		Real64 OutdoorDryBulb; // outdoor dry-bulb temperature (C)
		Real64 OutdoorHumRat; // outdoor humidity ratio (kg/kg)
		Real64 OutdoorPressure; // outdoor pressure (Pa)
		Real64 OutdoorWetBulb; // outdoor wet-bulb temperature (C)
		Real64 SumCoolInletWB; // sum of active TU's DX cooling coil inlet air wet-bulb temperature
		Real64 SumHeatInletDB; // sum of active TU's DX heating coil inlet air dry-bulb temperature
		Real64 SumHeatInletWB; // sum of active TU's DX heating coil inlet air wet-bulb temperature
		Real64 CoolOABoundary; // output of cooling boundary curve (outdoor temperature, C)
		Real64 HeatOABoundary; // output of heating boundary curve (outdoor temperature, C)
		Real64 TotalTUCoolingCapacity; // sum of TU's cooling capacity (W)
		Real64 TotalTUHeatingCapacity; // sum of TU's heating capacity (W)
		Real64 TotalCondCoolingCapacity; // total available condenser cooling capacity (W)
		Real64 TotalCondHeatingCapacity; // total available condenser heating capacity (W)
		Real64 CoolingPLR; // condenser cooling PLR
		Real64 HeatingPLR; // condenser heating PLR
		Real64 CyclingRatio; // cycling ratio of condenser's compressors
		Real64 EIRFPLRModFac; // EIRFPLR curve output
		int Stage; // used for crankcase heater power calculation
		Real64 UpperStageCompressorRatio; // used for crankcase heater power calculation
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 RhoWater; // Density of water [kg/m3]
		Real64 CpCond; // Specific Heat of water [J/kg-k]
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 CondWaterMassFlow; // Condenser water mass flow rate [kg/s]
		Real64 PartLoadFraction; // Part load fraction from PLFFPLR curve
		Real64 VRFRTF; // VRF runtime fraction when cycling below MINPLR
		Real64 OutdoorCoilT; // Outdoor coil temperature (C)
		Real64 OutdoorCoildw; // Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
		Real64 FractionalDefrostTime; // Fraction of time step system is in defrost
		Real64 HeatingCapacityMultiplier; // Multiplier for heating capacity when system is in defrost
		Real64 InputPowerMultiplier; // Multiplier for power when system is in defrost
		Real64 LoadDueToDefrost; // Additional load due to defrost
		Real64 DefrostEIRTempModFac; // EIR modifier for defrost (function of entering drybulb, outside wetbulb)
		int HRCAPFT; // index to heat recovery CAPFTCool curve
		Real64 HRCAPFTConst; // stead-state capacity fraction
		Real64 HRInitialCapFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
		Real64 HRCapTC; // Time constant used to recover from intial degratation in cooling heat recovery
		int HREIRFT; // Index to cool EIR as a function of temperature curve for heat recovery
		Real64 HREIRFTConst; // stead-state EIR fraction
		Real64 HRInitialEIRFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
		Real64 HREIRTC; // Time constant used to recover from intial degratation in cooling heat recovery
		static Real64 CurrentEndTime; // end time of current time step
		static Real64 CurrentEndTimeLast; // end time of last time step
		static Real64 TimeStepSysLast; // system time step on last time step
		Real64 SUMultiplier; // multiplier for simulating mode changes
		Real64 CondPower; // condenser power [W]
		Real64 CondCapacity; // condenser heat rejection [W]
		Real64 CondOutletTemp; // Outlet temperature from VRF condenser [C]
		Real64 QCondTmp; // temporary variable for condenser heat rejection [W]
		Real64 TotPower; // total condenser power use [W]
		bool HRHeatRequestFlag; // flag indicating VRF TU could operate in heating mode
		bool HRCoolRequestFlag; // flag indicating VRF TU could operate in cooling mode
		// FLOW

		// variable initializations
		TUListNum = VRF( VRFCond ).ZoneTUListPtr;
		NumTUInList = TerminalUnitList( TUListNum ).NumTUInList;
		TUCoolingLoad = 0.0;
		TUHeatingLoad = 0.0;
		TUParasiticPower = 0.0;
		TUFanPower = 0.0;
		CoolingPLR = 0.0;
		HeatingPLR = 0.0;
		CyclingRatio = 1.0;
		SumCoolInletWB = 0.0;
		SumHeatInletDB = 0.0;
		SumHeatInletWB = 0.0;
		TotalCondCoolingCapacity = 0.0;
		TotalCondHeatingCapacity = 0.0;
		TotalTUCoolingCapacity = 0.0;
		TotalTUHeatingCapacity = 0.0;
		NumTUInCoolingMode = 0;
		NumTUInHeatingMode = 0;
		VRF( VRFCond ).ElecCoolingPower = 0.0;
		VRF( VRFCond ).ElecHeatingPower = 0.0;
		VRF( VRFCond ).CrankCaseHeaterPower = 0.0;
		VRF( VRFCond ).EvapCondPumpElecPower = 0.0;
		VRF( VRFCond ).EvapWaterConsumpRate = 0.0;
		VRF( VRFCond ).DefrostPower = 0.0;
		VRF( VRFCond ).OperatingCoolingCOP = 0.0;
		VRF( VRFCond ).OperatingHeatingCOP = 0.0;
		VRF( VRFCond ).OperatingCOP = 0.0;
		VRF( VRFCond ).BasinHeaterPower = 0.0;

		// sum loads on TU coils
		for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
			TUCoolingLoad += TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU );
			TUHeatingLoad += TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU );
			TUParasiticPower += VRFTU( TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU ) ).ParasiticCoolElecPower + VRFTU( TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU ) ).ParasiticHeatElecPower;
			TUFanPower += VRFTU( TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU ) ).FanPower;
		}
		VRF( VRFCond ).TUCoolingLoad = TUCoolingLoad;
		VRF( VRFCond ).TUHeatingLoad = TUHeatingLoad;

		// loop through TU's and calculate average inlet conditions for active coils
		for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
			TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
			CoolCoilIndex = VRFTU( TUIndex ).CoolCoilIndex;
			HeatCoilIndex = VRFTU( TUIndex ).HeatCoilIndex;

			if ( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) > 0.0 ) {
				SumCoolInletWB += DXCoilCoolInletAirWBTemp( CoolCoilIndex ) * TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) / TUCoolingLoad;
				++NumTUInCoolingMode;
			}
			if ( TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) > 0.0 ) {
				SumHeatInletDB += DXCoilHeatInletAirDBTemp( HeatCoilIndex ) * TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) / TUHeatingLoad;
				SumHeatInletWB += DXCoilHeatInletAirWBTemp( HeatCoilIndex ) * TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) / TUHeatingLoad;
				++NumTUInHeatingMode;
			}
		}

		// set condenser entering air conditions
		if ( VRF( VRFCond ).CondenserNodeNum != 0 ) {
			OutdoorDryBulb = Node( VRF( VRFCond ).CondenserNodeNum ).Temp;
			if ( VRF( VRFCond ).CondenserType != WaterCooled ) {
				OutdoorHumRat = Node( VRF( VRFCond ).CondenserNodeNum ).HumRat;
				OutdoorPressure = Node( VRF( VRFCond ).CondenserNodeNum ).Press;
				OutdoorWetBulb = Node( VRF( VRFCond ).CondenserNodeNum ).OutAirWetBulb;
			} else {
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			}
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		if ( VRF( VRFCond ).CondenserType == AirCooled ) {
			CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp
		} else if ( VRF( VRFCond ).CondenserType == EvapCooled ) {
			RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat );
			CondAirMassFlow = RhoAir * VRF( VRFCond ).EvapCondAirVolFlowRate;
			// (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
			CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - VRF( VRFCond ).EvapCondEffectiveness );
			CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
		} else if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
			CondInletTemp = OutdoorDryBulb; // node inlet temp from above
			CondWaterMassFlow = VRF( VRFCond ).WaterCondenserDesignMassFlow;
		} else {
			assert( false );
		}
		VRF( VRFCond ).CondenserInletTemp = CondInletTemp;

		// calculate capacities and energy use
		if ( CoolingLoad( VRFCond ) && TerminalUnitList( TUListNum ).CoolingCoilPresent( NumTUInList ) ) {
			InletAirWetBulbC = SumCoolInletWB;
			TotCoolCapTempModFac = CurveValue( VRF( VRFCond ).CoolCapFT, InletAirWetBulbC, CondInletTemp );
			TotCoolEIRTempModFac = CurveValue( VRF( VRFCond ).CoolEIRFT, InletAirWetBulbC, CondInletTemp );

			// recalculate cooling Cap and EIR curve output if using boundary curve along with dual Cap and EIR curves.
			if ( VRF( VRFCond ).CoolBoundaryCurvePtr > 0 ) {
				CoolOABoundary = CurveValue( VRF( VRFCond ).CoolBoundaryCurvePtr, InletAirWetBulbC );
				if ( OutdoorDryBulb > CoolOABoundary ) {
					if ( VRF( VRFCond ).CoolCapFTHi > 0 ) TotCoolCapTempModFac = CurveValue( VRF( VRFCond ).CoolCapFTHi, InletAirWetBulbC, CondInletTemp );
					if ( VRF( VRFCond ).CoolEIRFTHi > 0 ) TotCoolEIRTempModFac = CurveValue( VRF( VRFCond ).CoolEIRFTHi, InletAirWetBulbC, CondInletTemp );
				}
			}

			TotalCondCoolingCapacity = VRF( VRFCond ).CoolingCapacity * CoolCombinationRatio( VRFCond ) * TotCoolCapTempModFac;
			TotalTUCoolingCapacity = TotalCondCoolingCapacity * VRF( VRFCond ).PipingCorrectionCooling;

			if ( TotalCondCoolingCapacity > 0.0 ) {
				CoolingPLR = ( TUCoolingLoad / VRF( VRFCond ).PipingCorrectionCooling ) / TotalCondCoolingCapacity;
			} else {
				CoolingPLR = 0.0;
			}

			//   Warn user if curve output goes negative
			if ( TotCoolEIRTempModFac < 0.0 ) {
				if ( ! WarmupFlag && NumTUInCoolingMode > 0 ) {
					if ( VRF( VRFCond ).EIRFTempCoolErrorIndex == 0 ) {
						ShowSevereMessage( cVRFTypes( VRF_HeatPump ) + " \"" + VRF( VRFCond ).Name + "\":" );
						ShowContinueError( " Cooling Energy Input Ratio Modifier curve (function of temperature) output is negative (" + TrimSigDigits( TotCoolEIRTempModFac, 3 ) + ")." );
						ShowContinueError( " Negative value occurs using an outdoor air temperature of " + TrimSigDigits( CondInletTemp, 1 ) + " C and an average indoor air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + " C." );
						ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( ccSimPlantEquipTypes( TypeOf_HeatPumpVRF ) + " \"" + VRF( VRFCond ).Name + "\": Cooling Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...", VRF( VRFCond ).EIRFTempCoolErrorIndex, TotCoolEIRTempModFac, TotCoolEIRTempModFac );
					TotCoolEIRTempModFac = 0.0;
				}
			}

		} else if ( HeatingLoad( VRFCond ) && TerminalUnitList( TUListNum ).HeatingCoilPresent( NumTUInList ) ) {
			InletAirDryBulbC = SumHeatInletDB;
			InletAirWetBulbC = SumHeatInletWB;
			{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
			if ( SELECT_CASE_var == DryBulbIndicator ) {
				TotHeatCapTempModFac = CurveValue( VRF( VRFCond ).HeatCapFT, InletAirDryBulbC, CondInletTemp );
				TotHeatEIRTempModFac = CurveValue( VRF( VRFCond ).HeatEIRFT, InletAirDryBulbC, CondInletTemp );
			} else if ( SELECT_CASE_var == WetBulbIndicator ) {
				TotHeatCapTempModFac = CurveValue( VRF( VRFCond ).HeatCapFT, InletAirDryBulbC, OutdoorWetBulb );
				TotHeatEIRTempModFac = CurveValue( VRF( VRFCond ).HeatEIRFT, InletAirDryBulbC, OutdoorWetBulb );
			} else {
				TotHeatCapTempModFac = 1.0;
				TotHeatEIRTempModFac = 1.0;
			}}
			// recalculate heating Cap and EIR curve output if using boundary curve along with dual Cap and EIR curves.
			if ( VRF( VRFCond ).HeatBoundaryCurvePtr > 0 ) {
				HeatOABoundary = CurveValue( VRF( VRFCond ).HeatBoundaryCurvePtr, InletAirDryBulbC );
				{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
				if ( SELECT_CASE_var == DryBulbIndicator ) {
					if ( OutdoorDryBulb > HeatOABoundary ) {
						if ( VRF( VRFCond ).HeatCapFTHi > 0 ) TotHeatCapTempModFac = CurveValue( VRF( VRFCond ).HeatCapFTHi, InletAirDryBulbC, CondInletTemp );
					}
				} else if ( SELECT_CASE_var == WetBulbIndicator ) {
					if ( OutdoorWetBulb > HeatOABoundary ) {
						if ( VRF( VRFCond ).HeatCapFTHi > 0 ) TotHeatCapTempModFac = CurveValue( VRF( VRFCond ).HeatCapFTHi, InletAirDryBulbC, OutdoorWetBulb );
					}
				} else {
					TotHeatCapTempModFac = 1.0;
				}}
			}
			if ( VRF( VRFCond ).EIRHeatBoundaryCurvePtr > 0 ) {
				HeatOABoundary = CurveValue( VRF( VRFCond ).EIRHeatBoundaryCurvePtr, InletAirDryBulbC );
				{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
				if ( SELECT_CASE_var == DryBulbIndicator ) {
					if ( OutdoorDryBulb > HeatOABoundary ) {
						if ( VRF( VRFCond ).HeatEIRFTHi > 0 ) TotHeatEIRTempModFac = CurveValue( VRF( VRFCond ).HeatEIRFTHi, InletAirDryBulbC, CondInletTemp );
					}
				} else if ( SELECT_CASE_var == WetBulbIndicator ) {
					if ( OutdoorWetBulb > HeatOABoundary ) {
						if ( VRF( VRFCond ).HeatEIRFTHi > 0 ) TotHeatEIRTempModFac = CurveValue( VRF( VRFCond ).HeatEIRFTHi, InletAirDryBulbC, OutdoorWetBulb );
					}
				} else {
					TotHeatEIRTempModFac = 1.0;
				}}
			}

			// Initializing defrost adjustment factors
			LoadDueToDefrost = 0.0;
			HeatingCapacityMultiplier = 1.0;
			FractionalDefrostTime = 0.0;
			InputPowerMultiplier = 1.0;

			// Check outdoor temperature to determine of defrost is active
			if ( OutdoorDryBulb <= VRF( VRFCond ).MaxOATDefrost && VRF( VRFCond ).CondenserType != WaterCooled) {

				// Calculating adjustment factors for defrost
				// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
				OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
				OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure ) ) );

				// Calculate defrost adjustment factors depending on defrost control type
				if ( VRF( VRFCond ).DefrostControl == Timed ) {
					FractionalDefrostTime = VRF( VRFCond ).DefrostFraction;
					if ( FractionalDefrostTime > 0.0 ) {
						HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
						InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
					}
				} else { //else defrost control is on-demand
					FractionalDefrostTime = 1.0 / ( 1.0 + 0.01446 / OutdoorCoildw );
					HeatingCapacityMultiplier = 0.875 * ( 1.0 - FractionalDefrostTime );
					InputPowerMultiplier = 0.954 * ( 1.0 - FractionalDefrostTime );
				}

				if ( FractionalDefrostTime > 0.0 ) {
					// Calculate defrost adjustment factors depending on defrost control strategy
					if ( VRF( VRFCond ).DefrostStrategy == ReverseCycle ) {
						LoadDueToDefrost = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( VRF( VRFCond ).HeatingCapacity / 1.01667 );
						DefrostEIRTempModFac = CurveValue( VRF( VRFCond ).DefrostEIRPtr, max( 15.555, InletAirWetBulbC ), max( 15.555, OutdoorDryBulb ) );

						//         Warn user if curve output goes negative
						if ( DefrostEIRTempModFac < 0.0 ) {
							if ( ! WarmupFlag ) {
								if ( VRF( VRFCond ).DefrostHeatErrorIndex == 0 ) {
									ShowSevereMessage( cVRFTypes( VRF_HeatPump ) + " \"" + VRF( VRFCond ).Name + "\":" );
									ShowContinueError( " Defrost Energy Input Ratio Modifier curve (function of temperature) output is negative (" + TrimSigDigits( DefrostEIRTempModFac, 3 ) + ")." );
									ShowContinueError( " Negative value occurs using an outdoor air dry-bulb temperature of " + TrimSigDigits( OutdoorDryBulb, 1 ) + " C and an average indoor air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + " C." );
									ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
								}
								ShowRecurringWarningErrorAtEnd( ccSimPlantEquipTypes( TypeOf_HeatPumpVRF ) + " \"" + VRF( VRFCond ).Name + "\": Defrost Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...", VRF( VRFCond ).DefrostHeatErrorIndex, DefrostEIRTempModFac, DefrostEIRTempModFac );
								DefrostEIRTempModFac = 0.0;
							}
						}

						VRF( VRFCond ).DefrostPower = DefrostEIRTempModFac * ( VRF( VRFCond ).HeatingCapacity / 1.01667 ) * FractionalDefrostTime;

					} else { // Defrost strategy is resistive
						VRF( VRFCond ).DefrostPower = VRF( VRFCond ).DefrostCapacity * FractionalDefrostTime;
					}
				} else { // Defrost is not active because FractionalDefrostTime = 0.0
					VRF( VRFCond ).DefrostPower = 0.0;
				}
			}

			TotalCondHeatingCapacity = VRF( VRFCond ).HeatingCapacity * HeatCombinationRatio( VRFCond ) * TotHeatCapTempModFac * HeatingCapacityMultiplier;
			TotalTUHeatingCapacity = TotalCondHeatingCapacity * VRF( VRFCond ).PipingCorrectionHeating;
			if ( TotalCondHeatingCapacity > 0.0 ) {
				HeatingPLR = ( TUHeatingLoad / VRF( VRFCond ).PipingCorrectionHeating ) / TotalCondHeatingCapacity;
				HeatingPLR += ( LoadDueToDefrost * HeatingPLR ) / TotalCondHeatingCapacity;
			} else {
				HeatingPLR = 0.0;
			}

			//   Warn user if curve output goes negative
			if ( TotHeatEIRTempModFac < 0.0 ) {
				if ( ! WarmupFlag && NumTUInHeatingMode > 0 ) {
					if ( VRF( VRFCond ).EIRFTempHeatErrorIndex == 0 ) {
						ShowSevereMessage( cVRFTypes( VRF_HeatPump ) + " \"" + VRF( VRFCond ).Name + "\":" );
						ShowContinueError( " Heating Energy Input Ratio Modifier curve (function of temperature) output is negative (" + TrimSigDigits( TotHeatEIRTempModFac, 3 ) + ")." );
						{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
						if ( SELECT_CASE_var == DryBulbIndicator ) {
							ShowContinueError( " Negative value occurs using an outdoor air dry-bulb temperature of " + TrimSigDigits( CondInletTemp, 1 ) + " C and an average indoor air dry-bulb temperature of " + TrimSigDigits( InletAirDryBulbC, 1 ) + " C." );
						} else if ( SELECT_CASE_var == WetBulbIndicator ) {
							ShowContinueError( " Negative value occurs using an outdoor air wet-bulb temperature of " + TrimSigDigits( OutdoorWetBulb, 1 ) + " C and an average indoor air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + " C." );
						} else {
						}}
						ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( ccSimPlantEquipTypes( TypeOf_HeatPumpVRF ) + " \"" + VRF( VRFCond ).Name + "\": Heating Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...", VRF( VRFCond ).EIRFTempHeatErrorIndex, TotHeatEIRTempModFac, TotHeatEIRTempModFac );
					TotHeatEIRTempModFac = 0.0;
				}
			}

		}

		VRF( VRFCond ).VRFCondPLR = max( CoolingPLR, HeatingPLR );

		HRHeatRequestFlag = any( TerminalUnitList( TUListNum ).HRHeatRequest );
		HRCoolRequestFlag = any( TerminalUnitList( TUListNum ).HRCoolRequest );

		if ( ! DoingSizing && ! WarmupFlag ) {
			if ( HRHeatRequestFlag && HRCoolRequestFlag ) {
				// determine operating mode change
				if ( ! VRF( VRFCond ).HRCoolingActive && ! VRF( VRFCond ).HRHeatingActive ) {
					VRF( VRFCond ).ModeChange = true;
				}
				if ( CoolingLoad( VRFCond ) ) {
					if ( VRF( VRFCond ).HRHeatingActive && ! VRF( VRFCond ).HRCoolingActive ) {
						VRF( VRFCond ).HRModeChange = true;
					}
					VRF( VRFCond ).HRCoolingActive = true;
					VRF( VRFCond ).HRHeatingActive = false;
					HRCAPFT = VRF( VRFCond ).HRCAPFTCool; // Index to cool capacity as a function of temperature\PLR curve for heat recovery
					if ( HRCAPFT > 0 ) {
						//         VRF(VRFCond)%HRCAPFTCoolConst = 0.9d0 ! initialized to 0.9
						if ( VRF( VRFCond ).HRCAPFTCoolType == BiQuadratic ) { // Curve type for HRCAPFTCool
							VRF( VRFCond ).HRCAPFTCoolConst = CurveValue( HRCAPFT, InletAirWetBulbC, CondInletTemp );
						} else {
							VRF( VRFCond ).HRCAPFTCoolConst = CurveValue( HRCAPFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HRCAPFTConst = VRF( VRFCond ).HRCAPFTCoolConst;
					HRInitialCapFrac = VRF( VRFCond ).HRInitialCoolCapFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
					HRCapTC = VRF( VRFCond ).HRCoolCapTC; // Time constant used to recover from intial degratation in cooling heat recovery

					HREIRFT = VRF( VRFCond ).HREIRFTCool; // Index to cool EIR as a function of temperature curve for heat recovery
					if ( HREIRFT > 0 ) {
						//         VRF(VRFCond)%HREIRFTCoolConst = 1.1d0 ! initialized to 1.1
						if ( VRF( VRFCond ).HREIRFTCoolType == BiQuadratic ) { // Curve type for HRCAPFTCool
							VRF( VRFCond ).HREIRFTCoolConst = CurveValue( HREIRFT, InletAirWetBulbC, CondInletTemp );
						} else {
							VRF( VRFCond ).HREIRFTCoolConst = CurveValue( HREIRFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HREIRFTConst = VRF( VRFCond ).HREIRFTCoolConst;
					HRInitialEIRFrac = VRF( VRFCond ).HRInitialCoolEIRFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
					HREIRTC = VRF( VRFCond ).HRCoolEIRTC; // Time constant used to recover from intial degratation in cooling heat recovery
				} else if ( HeatingLoad( VRFCond ) ) {
					if ( ! VRF( VRFCond ).HRHeatingActive && VRF( VRFCond ).HRCoolingActive ) {
						VRF( VRFCond ).HRModeChange = true;
					}
					VRF( VRFCond ).HRCoolingActive = false;
					VRF( VRFCond ).HRHeatingActive = true;
					HRCAPFT = VRF( VRFCond ).HRCAPFTHeat; // Index to heat capacity as a function of temperature\PLR curve for heat recovery
					if ( HRCAPFT > 0 ) {
						//         VRF(VRFCond)%HRCAPFTHeatConst = 1.1d0 ! initialized to 1.1
						if ( VRF( VRFCond ).HRCAPFTHeatType == BiQuadratic ) { // Curve type for HRCAPFTCool
							{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
							if ( SELECT_CASE_var == DryBulbIndicator ) {
								VRF( VRFCond ).HRCAPFTHeatConst = CurveValue( HRCAPFT, InletAirDryBulbC, CondInletTemp );
							} else if ( SELECT_CASE_var == WetBulbIndicator ) {
								VRF( VRFCond ).HRCAPFTHeatConst = CurveValue( HRCAPFT, InletAirDryBulbC, OutdoorWetBulb );
							} else {
								VRF( VRFCond ).HRCAPFTHeatConst = 1.0;
							}}
						} else {
							VRF( VRFCond ).HRCAPFTHeatConst = CurveValue( HRCAPFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HRCAPFTConst = VRF( VRFCond ).HRCAPFTHeatConst;
					HRInitialCapFrac = VRF( VRFCond ).HRInitialHeatCapFrac; // Fractional heating degradation at the start of heat recovery from cooling mode
					HRCapTC = VRF( VRFCond ).HRHeatCapTC; // Time constant used to recover from intial degratation in heating heat recovery

					HREIRFT = VRF( VRFCond ).HREIRFTHeat; // Index to cool EIR as a function of temperature curve for heat recovery
					if ( HREIRFT > 0 ) {
						//         VRF(VRFCond)%HREIRFTCoolConst = 1.1d0 ! initialized to 1.1
						if ( VRF( VRFCond ).HREIRFTHeatType == BiQuadratic ) { // Curve type for HRCAPFTHeat
							{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
							if ( SELECT_CASE_var == DryBulbIndicator ) {
								VRF( VRFCond ).HREIRFTHeatConst = CurveValue( HREIRFT, InletAirDryBulbC, CondInletTemp );
							} else if ( SELECT_CASE_var == WetBulbIndicator ) {
								VRF( VRFCond ).HREIRFTHeatConst = CurveValue( HREIRFT, InletAirDryBulbC, OutdoorWetBulb );
							} else {
								VRF( VRFCond ).HREIRFTHeatConst = 1.0;
							}}
						} else {
							VRF( VRFCond ).HREIRFTHeatConst = CurveValue( HREIRFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HREIRFTConst = VRF( VRFCond ).HRCAPFTHeatConst;
					HRInitialEIRFrac = VRF( VRFCond ).HRInitialHeatEIRFrac; // Fractional heating degradation at the start of heat recovery from heating mode
					HREIRTC = VRF( VRFCond ).HRHeatEIRTC; // Time constant used to recover from intial degratation in heating heat recovery
				} else {
					//   zone thermostats satisfied, condenser is off. Set values anyway
					HRCAPFTConst = 1.0;
					HRInitialCapFrac = 1.0;
					HRCapTC = 1.0;
					HREIRFTConst = 1.0;
					HRInitialEIRFrac = 1.0;
					HREIRTC = 1.0;
					if ( VRF( VRFCond ).HRHeatingActive || VRF( VRFCond ).HRCoolingActive ) {
						VRF( VRFCond ).HRModeChange = true;
					}
					VRF( VRFCond ).HRCoolingActive = false;
					VRF( VRFCond ).HRHeatingActive = false;
				}

			} else { // IF(HRHeatRequestFlag .AND. HRCoolRequestFlag)THEN -- Heat recovery turned off
				HRCAPFTConst = 1.0;
				HRInitialCapFrac = 1.0;
				HRCapTC = 0.0;
				HREIRFTConst = 1.0;
				HRInitialEIRFrac = 1.0;
				HREIRTC = 0.0;
				VRF( VRFCond ).HRModeChange = false;
				VRF( VRFCond ).HRCoolingActive = false;
				VRF( VRFCond ).HRHeatingActive = false;
			}

			// calculate end time of current time step to determine if max capacity reset is required
			CurrentEndTime = double( ( DayOfSim - 1 ) * 24 ) + CurrentTime - TimeStepZone + SysTimeElapsed;

			if ( VRF( VRFCond ).ModeChange || VRF( VRFCond ).HRModeChange ) {
				if ( VRF( VRFCond ).HRCoolingActive && VRF( VRFCond ).HRTimer == 0.0 ) {
					VRF( VRFCond ).HRTimer = CurrentEndTimeLast;
				} else if ( VRF( VRFCond ).HRHeatingActive && VRF( VRFCond ).HRTimer == 0.0 ) {
					VRF( VRFCond ).HRTimer = CurrentEndTimeLast;
				} else if ( ! VRF( VRFCond ).HRCoolingActive && ! VRF( VRFCond ).HRHeatingActive ) {
					VRF( VRFCond ).HRTimer = 0.0;
				}
			}

			VRF( VRFCond ).HRTime = max( 0.0, CurrentEndTime - VRF( VRFCond ).HRTimer );
			if ( VRF( VRFCond ).HRTime < ( HRCapTC * 5.0 ) ) {
				if ( HRCapTC > 0.0 ) {
					SUMultiplier = min( 1.0, 1.0 - std::exp( -VRF( VRFCond ).HRTime / HRCapTC ) );
				} else {
					SUMultiplier = 1.0;
				}
			} else {
				SUMultiplier = 1.0;
				VRF( VRFCond ).ModeChange = false;
				VRF( VRFCond ).HRModeChange = false;
			}
			VRF( VRFCond ).SUMultiplier = SUMultiplier;

			TimeStepSysLast = TimeStepSys;
			CurrentEndTimeLast = CurrentEndTime;

			if ( VRF( VRFCond ).HeatRecoveryUsed && VRF( VRFCond ).HRCoolingActive ) {
				TotalCondCoolingCapacity *= HRCAPFTConst;
				TotalCondCoolingCapacity = HRInitialCapFrac * TotalCondCoolingCapacity + ( 1.0 - HRInitialCapFrac ) * TotalCondCoolingCapacity * SUMultiplier;
				TotalTUCoolingCapacity = TotalCondCoolingCapacity * VRF( VRFCond ).PipingCorrectionCooling;
				if ( TotalCondCoolingCapacity > 0.0 ) {
					CoolingPLR = min( 1.0, ( TUCoolingLoad / VRF( VRFCond ).PipingCorrectionCooling ) / TotalCondCoolingCapacity );
				} else {
					CoolingPLR = 0.0;
				}
			} else if ( VRF( VRFCond ).HeatRecoveryUsed && VRF( VRFCond ).HRHeatingActive ) {
				TotalCondHeatingCapacity *= HRCAPFTConst;
				TotalCondHeatingCapacity = HRInitialCapFrac * TotalCondHeatingCapacity + ( 1.0 - HRInitialCapFrac ) * TotalCondHeatingCapacity * SUMultiplier;
				TotalTUHeatingCapacity = TotalCondHeatingCapacity * VRF( VRFCond ).PipingCorrectionHeating;
				if ( TotalCondHeatingCapacity > 0.0 ) {
					HeatingPLR = min( 1.0, ( TUHeatingLoad / VRF( VRFCond ).PipingCorrectionHeating ) / TotalCondHeatingCapacity );
				} else {
					HeatingPLR = 0.0;
				}
			}
			VRF( VRFCond ).VRFCondPLR = max( CoolingPLR, HeatingPLR );
		}

		VRF( VRFCond ).TotalCoolingCapacity = TotalCondCoolingCapacity * CoolingPLR;
		VRF( VRFCond ).TotalHeatingCapacity = TotalCondHeatingCapacity * HeatingPLR;

		if ( VRF( VRFCond ).MinPLR > 0.0 ) {
			CyclingRatio = min( 1.0, VRF( VRFCond ).VRFCondPLR / VRF( VRFCond ).MinPLR );
			if ( VRF( VRFCond ).VRFCondPLR < VRF( VRFCond ).MinPLR && VRF( VRFCond ).VRFCondPLR > 0.0 ) {
				VRF( VRFCond ).VRFCondPLR = VRF( VRFCond ).MinPLR;
			}
		}
		VRF( VRFCond ).VRFCondCyclingRatio = CyclingRatio; // report variable for cycling rate

		VRF( VRFCond ).OperatingMode = 0; // report variable for heating or cooling mode
		EIRFPLRModFac = 1.0;
		VRFRTF = 0.0;
		// cooling and heating is optional (only one may exist), if so then performance curve for missing coil are not required
		if ( CoolingLoad( VRFCond ) && CoolingPLR > 0.0 ) {
			VRF( VRFCond ).OperatingMode = 1;
			if ( CoolingPLR > 1.0 ) {
				if ( VRF( VRFCond ).CoolEIRFPLR2 > 0 ) EIRFPLRModFac = CurveValue( VRF( VRFCond ).CoolEIRFPLR2, max( VRF( VRFCond ).MinPLR, CoolingPLR ) );
			} else {
				if ( VRF( VRFCond ).CoolEIRFPLR1 > 0 ) EIRFPLRModFac = CurveValue( VRF( VRFCond ).CoolEIRFPLR1, max( VRF( VRFCond ).MinPLR, CoolingPLR ) );
			}
			// find part load fraction to calculate RTF
			if ( VRF( VRFCond ).CoolPLFFPLR > 0 ) {
				PartLoadFraction = max( 0.7, CurveValue( VRF( VRFCond ).CoolPLFFPLR, CyclingRatio ) );
			} else {
				PartLoadFraction = 1.0;
			}
			VRFRTF = min( 1.0, ( CyclingRatio / PartLoadFraction ) );

			VRF( VRFCond ).ElecCoolingPower = ( VRF( VRFCond ).RatedCoolingPower * TotCoolCapTempModFac ) * TotCoolEIRTempModFac * EIRFPLRModFac * VRFRTF;
		}
		if ( HeatingLoad( VRFCond ) && HeatingPLR > 0.0 ) {
			VRF( VRFCond ).OperatingMode = 2;
			if ( HeatingPLR > 1.0 ) {
				if ( VRF( VRFCond ).HeatEIRFPLR2 > 0 ) EIRFPLRModFac = CurveValue( VRF( VRFCond ).HeatEIRFPLR2, max( VRF( VRFCond ).MinPLR, HeatingPLR ) );
			} else {
				if ( VRF( VRFCond ).HeatEIRFPLR1 > 0 ) EIRFPLRModFac = CurveValue( VRF( VRFCond ).HeatEIRFPLR1, max( VRF( VRFCond ).MinPLR, HeatingPLR ) );
			}
			// find part load fraction to calculate RTF
			if ( VRF( VRFCond ).HeatPLFFPLR > 0 ) {
				PartLoadFraction = max( 0.7, CurveValue( VRF( VRFCond ).HeatPLFFPLR, CyclingRatio ) );
			} else {
				PartLoadFraction = 1.0;
			}
			VRFRTF = min( 1.0, ( CyclingRatio / PartLoadFraction ) );

			VRF( VRFCond ).ElecHeatingPower = ( VRF( VRFCond ).RatedHeatingPower * TotHeatCapTempModFac ) * TotHeatEIRTempModFac * EIRFPLRModFac * VRFRTF * InputPowerMultiplier;

			// adjust defrost power based on heating RTF
			VRF( VRFCond ).DefrostPower *= VRFRTF;
		}
		VRF( VRFCond ).VRFCondRTF = VRFRTF;

		// calculate crankcase heater power
		if ( VRF( VRFCond ).MaxOATCCHeater > OutdoorDryBulb ) {
			// calculate crankcase heater power
			VRF( VRFCond ).CrankCaseHeaterPower = VRF( VRFCond ).CCHeaterPower * ( 1.0 - VRFRTF );
			if ( VRF( VRFCond ).NumCompressors > 1 ) {
				UpperStageCompressorRatio = ( 1.0 - VRF( VRFCond ).CompressorSizeRatio ) / ( VRF( VRFCond ).NumCompressors - 1 );
				for ( Stage = 1; Stage <= VRF( VRFCond ).NumCompressors - 2; ++Stage ) {
					if ( VRF( VRFCond ).VRFCondPLR < ( VRF( VRFCond ).CompressorSizeRatio + Stage * UpperStageCompressorRatio ) ) {
						VRF( VRFCond ).CrankCaseHeaterPower += VRF( VRFCond ).CCHeaterPower;
					}
				}
			}
		} else {
			VRF( VRFCond ).CrankCaseHeaterPower = 0.0;
		}

		CondCapacity = max( VRF( VRFCond ).TotalCoolingCapacity, VRF( VRFCond ).TotalHeatingCapacity ) * VRFRTF;
		CondPower = max( VRF( VRFCond ).ElecCoolingPower, VRF( VRFCond ).ElecHeatingPower );
		if ( VRF( VRFCond ).ElecCoolingPower > 0.0 ) {
			VRF( VRFCond ).QCondenser = CondCapacity + CondPower - VRF( VRFCond ).TUHeatingLoad / VRF( VRFCond ).PipingCorrectionHeating;
		} else if ( VRF( VRFCond ).ElecHeatingPower > 0.0 ) {
			VRF( VRFCond ).QCondenser = -CondCapacity + CondPower + VRF( VRFCond ).TUCoolingLoad / VRF( VRFCond ).PipingCorrectionCooling;
		} else {
			VRF( VRFCond ).QCondenser = 0.0;
		}

		if ( VRF( VRFCond ).CondenserType == EvapCooled ) {
			// Calculate basin heater power
			CalcBasinHeaterPower( VRF( VRFCond ).BasinHeaterPowerFTempDiff, VRF( VRFCond ).BasinHeaterSchedulePtr, VRF( VRFCond ).BasinHeaterSetPointTemp, VRF( VRFCond ).BasinHeaterPower );
			VRF( VRFCond ).BasinHeaterPower *= ( 1.0 - VRFRTF );

			// calcualte evaporative condenser pump power and water consumption
			if ( CoolingLoad( VRFCond ) && CoolingPLR > 0.0 ) {
				//******************
				// WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
				// H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
				//                    /RhoWater [kg H2O/m3 H2O]
				//******************
				RhoWater = RhoH2O( OutdoorDryBulb );
				VRF( VRFCond ).EvapWaterConsumpRate = ( CondInletHumRat - OutdoorHumRat ) * CondAirMassFlow / RhoWater * VRF( VRFCond ).VRFCondPLR;
				VRF( VRFCond ).EvapCondPumpElecPower = VRF( VRFCond ).EvapCondPumpPower * VRFRTF;
			}
		} else if ( VRF( VRFCond ).CondenserType == WaterCooled ) {

			if ( CondCapacity > 0.0 ) {
				CondenserWaterMassFlowRate = CondWaterMassFlow;
			} else {
				CondenserWaterMassFlowRate = 0.0;
			}
			SetComponentFlowRate( CondenserWaterMassFlowRate, VRF( VRFCond ).CondenserNodeNum, VRF( VRFCond ).CondenserOutletNodeNum, VRF( VRFCond ).SourceLoopNum, VRF( VRFCond ).SourceLoopSideNum, VRF( VRFCond ).SourceBranchNum, VRF( VRFCond ).SourceCompNum );

			VRF( VRFCond ).CondenserInletTemp = Node( VRF( VRFCond ).CondenserNodeNum ).Temp;
			VRF( VRFCond ).WaterCondenserMassFlow = Node( VRF( VRFCond ).CondenserNodeNum ).MassFlowRate;

			CpCond = GetSpecificHeatGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, VRF( VRFCond ).CondenserInletTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
			if ( CondWaterMassFlow > 0.0 ) {
				CondOutletTemp = VRF( VRFCond ).QCondenser / ( CondWaterMassFlow * CpCond ) + CondInletTemp;
			} else {
				CondOutletTemp = CondInletTemp;
			}
			QCondTmp = CondWaterMassFlow * CpCond * ( CondOutletTemp - CondInletTemp );
			VRF( VRFCond ).CondenserSideOutletTemp = CondOutletTemp;

		}

		// calculate operating COP
		if ( CoolingLoad( VRFCond ) && CoolingPLR > 0.0 ) {
			if ( VRF( VRFCond ).ElecCoolingPower != 0.0 ) {
				// this calc should use delivered capacity, not condenser capacity, use VRF(VRFCond)%TUCoolingLoad
				VRF( VRFCond ).OperatingCoolingCOP = ( VRF( VRFCond ).TotalCoolingCapacity ) / ( VRF( VRFCond ).ElecCoolingPower + VRF( VRFCond ).CrankCaseHeaterPower + VRF( VRFCond ).EvapCondPumpElecPower + VRF( VRFCond ).DefrostPower );
			} else {
				VRF( VRFCond ).OperatingCoolingCOP = 0.0;
			}
		}
		if ( HeatingLoad( VRFCond ) && HeatingPLR > 0.0 ) {
			if ( VRF( VRFCond ).ElecHeatingPower != 0.0 ) {
				// this calc should use deleivered capacity, not condenser capacity, use VRF(VRFCond)%TUHeatingLoad
				VRF( VRFCond ).OperatingHeatingCOP = ( VRF( VRFCond ).TotalHeatingCapacity ) / ( VRF( VRFCond ).ElecHeatingPower + VRF( VRFCond ).CrankCaseHeaterPower + VRF( VRFCond ).EvapCondPumpElecPower + VRF( VRFCond ).DefrostPower );
			} else {
				VRF( VRFCond ).OperatingHeatingCOP = 0.0;
			}
		}

		TotPower = TUParasiticPower + TUFanPower + VRF( VRFCond ).ElecHeatingPower + VRF( VRFCond ).ElecCoolingPower + VRF( VRFCond ).CrankCaseHeaterPower + VRF( VRFCond ).EvapCondPumpElecPower + VRF( VRFCond ).DefrostPower;
		if ( TotPower > 0.0 ) VRF( VRFCond ).OperatingCOP = ( VRF( VRFCond ).TUCoolingLoad + VRF( VRFCond ).TUHeatingLoad ) / TotPower;

		// limit the TU capacity when the condenser is maxed out on capacity
		// I think this next line will make the max cap report variable match the coil objects, will probably change the answer though
		//  IF(CoolingLoad(VRFCond) .AND. NumTUInCoolingMode .GT. 0 .AND. MaxCoolingCapacity(VRFCond) == MaxCap)THEN
		if ( CoolingLoad( VRFCond ) && NumTUInCoolingMode > 0 ) {

			//   IF TU capacity is greater than condenser capacity find maximum allowed TU capacity (i.e., conserve energy)
			if ( TUCoolingLoad > TotalTUCoolingCapacity ) {
				LimitTUCapacity( VRFCond, NumTUInList, TotalTUCoolingCapacity, TerminalUnitList( TUListNum ).TotalCoolLoad, MaxCoolingCapacity( VRFCond ), TotalTUHeatingCapacity, TerminalUnitList( TUListNum ).TotalHeatLoad, MaxHeatingCapacity( VRFCond ) );
			}
		} else if ( HeatingLoad( VRFCond ) && NumTUInHeatingMode > 0 ) {
			//   IF TU capacity is greater than condenser capacity
			if ( TUHeatingLoad > TotalTUHeatingCapacity ) {
				LimitTUCapacity( VRFCond, NumTUInList, TotalTUHeatingCapacity, TerminalUnitList( TUListNum ).TotalHeatLoad, MaxHeatingCapacity( VRFCond ), TotalTUCoolingCapacity, TerminalUnitList( TUListNum ).TotalCoolLoad, MaxCoolingCapacity( VRFCond ) );
			}
		} else {
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetVRFInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages GetInput processing and program termination

		// METHODOLOGY EMPLOYED:
		// Calls "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetVRFInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ErrorsFound( false ); // If errors detected in input

		// Flow
		GetVRFInputData( ErrorsFound );

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting AirConditioner:VariableRefrigerantFlow system input. Preceding condition(s) causes termination." );
		}

	}

	void
	GetVRFInputData( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for VRF systems and stores it in data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace DataLoopNode;
		using General::TrimSigDigits;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using Fans::GetFanDesignVolumeFlowRate;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using Fans::GetFanIndex;
		using Fans::GetFanAvailSchPtr;
		using Fans::GetFanType;
		using MixedAir::GetOAMixerNodeNumbers;
		using DXCoils::GetDXCoilIndex;
		auto & GetDXCoilInletNode( DXCoils::GetCoilInletNode );
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		using DXCoils::GetCoilCondenserInletNode;
		using DXCoils::GetCoilTypeNum;
		using DXCoils::SetDXCoolingCoilData;
		using DXCoils::GetDXCoilAvailSchPtr;
		using DataHeatBalance::Zone;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using WaterManager::SetupTankDemandComponent;
		using WaterManager::SetupTankSupplyComponent;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataSizing::AutoSize;
		using DataSizing::ZoneHVACSizing;

		//    USE DataIPShortCuts

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetVRFInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumVRFCTU; // The number of VRF constant volume TUs (anticipating different types of TU's)
		int VRFTUNum; // Loop index to the total number of VRF terminal units
		int VRFNum; // Loop index to the total number of VRF terminal units
		int TUListNum; // Loop index to the total number of VRF terminal unit lists
		int NumAlphas; // Number of alpha arguments
		int NumNums; // Number of real arguments
		//    INTEGER :: checkNum
		int IOStat; // Status
		bool errFlag; // error flag for mining functions
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		int NumParams;
		int MaxAlphas;
		int MaxNumbers;
		std::string FanType; // Type of supply air fan
		std::string FanName; // Supply air fan name
		std::string OAMixerType; // Type of OA mixer
		std::string DXCoolingCoilType; // Type of VRF DX cooling coil
		std::string DXHeatingCoilType; // Type of VRF DX heating coil
		int FanType_Num; // Used in mining function CALLS
		Real64 FanVolFlowRate; // Fan Max Flow Rate from Fan object (for comparisons to validity)
		int FanInletNodeNum; // Used in TU configuration setup
		int FanOutletNodeNum; // Used in TU configuration setup
		Array1D_int OANodeNums( 4 ); // Node numbers of OA mixer (OA, EA, RA, MA)
		int CCoilInletNodeNum; // Used in TU configuration setup
		int CCoilOutletNodeNum; // Used in TU configuration setup
		int HCoilInletNodeNum; // Used in TU configuration setup
		int HCoilOutletNodeNum; // Used in TU configuration setup
		int ZoneTerminalUnitListNum; // Used to find connection between VRFTU, TUList and VRF condenser
		int NumCond; // loop counter
		int NumList; // loop counter
		bool ZoneNodeNotFound; // used in error checking
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter
		// Followings for VRF FluidTCtrl Only
		int NumCompSpd; // XP_loop counter
		int NumOfCompSpd; // XP_ number of compressor speed inputs

		// Flow
		MaxAlphas = 0;
		MaxNumbers = 0;

		NumVRFCTU = GetNumObjectsFound( "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow" );
		if ( NumVRFCTU > 0 ) {
			GetObjectDefMaxArgs( "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}

		NumVRFCond_SysCurve = GetNumObjectsFound( "AirConditioner:VariableRefrigerantFlow" );
		if ( NumVRFCond_SysCurve > 0 ) {
			GetObjectDefMaxArgs( "AirConditioner:VariableRefrigerantFlow", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}

		NumVRFCond_FluidTCtrl = GetNumObjectsFound( "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl" );
		if ( NumVRFCond_FluidTCtrl > 0 ) {
			GetObjectDefMaxArgs( "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}
		NumVRFCond = NumVRFCond_SysCurve + NumVRFCond_FluidTCtrl;

		NumVRFTULists = GetNumObjectsFound( "ZoneTerminalUnitList" );
		if ( NumVRFTULists > 0 ) {
			GetObjectDefMaxArgs( "ZoneTerminalUnitList", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}

		cAlphaArgs.allocate( MaxAlphas );
		cAlphaFieldNames.allocate( MaxAlphas );
		lAlphaFieldBlanks.dimension( MaxAlphas, false );
		cNumericFieldNames.allocate( MaxNumbers );
		lNumericFieldBlanks.dimension( MaxNumbers, false );
		rNumericArgs.dimension( MaxNumbers, 0.0 );

		NumVRFTU = NumVRFCTU;
		if ( NumVRFTU > 0 ) {
			VRFTU.allocate( NumVRFTU );
			CheckEquipName.dimension( NumVRFTU, true );
			VRFTUNumericFields.allocate( NumVRFTU );
		}

		if ( NumVRFCond > 0 ) {
			VRF.allocate( NumVRFCond );
			MaxCoolingCapacity.allocate( NumVRFCond );
			MaxHeatingCapacity.allocate( NumVRFCond );
			CoolCombinationRatio.allocate( NumVRFCond );
			HeatCombinationRatio.allocate( NumVRFCond );
			MaxCoolingCapacity = MaxCap;
			MaxHeatingCapacity = MaxCap;
			CoolCombinationRatio = 1.0;
			HeatCombinationRatio = 1.0;
		}

		if ( NumVRFTULists > 0 ) {
			TerminalUnitList.allocate( NumVRFTULists );
		}

		// read all terminal unit list objects
		cCurrentModuleObject = "ZoneTerminalUnitList";
		for ( VRFNum = 1; VRFNum <= NumVRFTULists; ++VRFNum ) {
			GetObjectItem( cCurrentModuleObject, VRFNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), TerminalUnitList, VRFNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			TerminalUnitList( VRFNum ).Name = cAlphaArgs( 1 );
			TerminalUnitList( VRFNum ).NumTUInList = NumAlphas - 1;
			TerminalUnitList( VRFNum ).ZoneTUPtr.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).ZoneTUName.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).IsSimulated.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).TotalCoolLoad.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).TotalHeatLoad.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).CoolingCoilPresent.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).HeatingCoilPresent.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).TerminalUnitNotSizedYet.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).HRHeatRequest.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).HRCoolRequest.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).CoolingCoilAvailable.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).HeatingCoilAvailable.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).CoolingCoilAvailSchPtr.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).HeatingCoilAvailSchPtr.allocate( TerminalUnitList( VRFNum ).NumTUInList );
			TerminalUnitList( VRFNum ).ZoneTUPtr = 0;
			TerminalUnitList( VRFNum ).IsSimulated = false;
			TerminalUnitList( VRFNum ).TotalCoolLoad = 0.0;
			TerminalUnitList( VRFNum ).TotalHeatLoad = 0.0;
			TerminalUnitList( VRFNum ).CoolingCoilPresent = true;
			TerminalUnitList( VRFNum ).HeatingCoilPresent = true;
			TerminalUnitList( VRFNum ).TerminalUnitNotSizedYet = true;
			TerminalUnitList( VRFNum ).HRHeatRequest = false;
			TerminalUnitList( VRFNum ).HRCoolRequest = false;
			TerminalUnitList( VRFNum ).CoolingCoilAvailable = false;
			TerminalUnitList( VRFNum ).HeatingCoilAvailable = false;
			TerminalUnitList( VRFNum ).CoolingCoilAvailSchPtr = -1;
			TerminalUnitList( VRFNum ).HeatingCoilAvailSchPtr = -1;

			for ( TUListNum = 1; TUListNum <= TerminalUnitList( VRFNum ).NumTUInList; ++TUListNum ) {
				TerminalUnitList( VRFNum ).ZoneTUName( TUListNum ) = cAlphaArgs( TUListNum + 1 );
			}
		}

		// read all VRF condenser objects: Algorithm Type 1_system curve based model
		cCurrentModuleObject = "AirConditioner:VariableRefrigerantFlow";
		for ( VRFNum = 1; VRFNum <= NumVRFCond_SysCurve; ++VRFNum ) {
			GetObjectItem( cCurrentModuleObject, VRFNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), VRF, VRFNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VRF( VRFNum ).Name = cAlphaArgs( 1 );
			VRF( VRFNum ).VRFSystemTypeNum = VRF_HeatPump;
			VRF( VRFNum ).VRFAlgorithmTypeNum = AlgorithmTypeSysCurve;
			if ( lAlphaFieldBlanks( 2 ) ) {
				VRF( VRFNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				VRF( VRFNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( VRF( VRFNum ).SchedPtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\" invalid data" );
					ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}
			//     CALL TestCompSet(TRIM(cCurrentModuleObject),VRF(VRFTUNum)%Name,cAlphaArgs(3),cAlphaArgs(4),'Air Nodes')

			VRF( VRFNum ).CoolingCapacity = rNumericArgs( 1 );
			VRF( VRFNum ).CoolingCOP = rNumericArgs( 2 );
			VRF( VRFNum ).MinOATCooling = rNumericArgs( 3 );
			VRF( VRFNum ).MaxOATCooling = rNumericArgs( 4 );

			VRF( VRFNum ).CoolCapFT = GetCurveIndex( cAlphaArgs( 3 ) );
			if ( VRF( VRFNum ).CoolCapFT > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolCapFT ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 3 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolCapFT ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
				//  only show error if cooling coil is present, since TU's have not yet been read, do this later in GetInput
				//      ELSE
				//        CALL ShowSevereError(TRIM(cCurrentModuleObject) //', "'//TRIM(VRF(VRFNum)%Name) // &
				//                     '" '//TRIM(cAlphaFieldNames(3)) //' not found.')
				//        ErrorsFound=.TRUE.
			}

			VRF( VRFNum ).CoolBoundaryCurvePtr = GetCurveIndex( cAlphaArgs( 4 ) );
			if ( VRF( VRFNum ).CoolBoundaryCurvePtr > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolBoundaryCurvePtr ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 4 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolBoundaryCurvePtr ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).CoolCapFTHi = GetCurveIndex( cAlphaArgs( 5 ) );
			if ( VRF( VRFNum ).CoolCapFTHi > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolCapFTHi ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 5 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolCapFTHi ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).CoolEIRFT = GetCurveIndex( cAlphaArgs( 6 ) );
			if ( VRF( VRFNum ).CoolEIRFT > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolEIRFT ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 6 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolEIRFT ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
				//  only show error if cooling coil is present, since TU's have not yet been read, do this later in GetInput
				//      ELSE
				//        CALL ShowSevereError(TRIM(cCurrentModuleObject) //', "'//TRIM(VRF(VRFNum)%Name) // &
				//                     '" '//TRIM(cAlphaFieldNames(6)) //' not found.')
				//        ErrorsFound=.TRUE.
			}

			VRF( VRFNum ).EIRCoolBoundaryCurvePtr = GetCurveIndex( cAlphaArgs( 7 ) );
			if ( VRF( VRFNum ).EIRCoolBoundaryCurvePtr > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).EIRCoolBoundaryCurvePtr ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 7 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).EIRCoolBoundaryCurvePtr ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).CoolEIRFTHi = GetCurveIndex( cAlphaArgs( 8 ) );
			if ( VRF( VRFNum ).CoolEIRFTHi > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolEIRFTHi ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 8 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolEIRFTHi ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).CoolEIRFPLR1 = GetCurveIndex( cAlphaArgs( 9 ) );
			if ( VRF( VRFNum ).CoolEIRFPLR1 > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolEIRFPLR1 ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 9 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolEIRFPLR1 ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
				//  only show error if cooling coil is present, since TU's have not yet been read, do this later in GetInput
				//      ELSE
				//        CALL ShowSevereError(TRIM(cCurrentModuleObject) //', "'//TRIM(VRF(VRFNum)%Name) // &
				//                     '" '//TRIM(cAlphaFieldNames(9)) //' not found.')
				//        ErrorsFound=.TRUE.
			}

			VRF( VRFNum ).CoolEIRFPLR2 = GetCurveIndex( cAlphaArgs( 10 ) );
			if ( VRF( VRFNum ).CoolEIRFPLR2 > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolEIRFPLR2 ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 10 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolEIRFPLR2 ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).CoolCombRatioPTR = GetCurveIndex( cAlphaArgs( 11 ) );
			if ( VRF( VRFNum ).CoolCombRatioPTR > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolCombRatioPTR ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 11 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolCombRatioPTR ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).CoolPLFFPLR = GetCurveIndex( cAlphaArgs( 12 ) );
			if ( VRF( VRFNum ).CoolPLFFPLR > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).CoolPLFFPLR ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 12 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).CoolPLFFPLR ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).HeatingCapacity = rNumericArgs( 5 );
			VRF( VRFNum ).HeatingCapacitySizeRatio = rNumericArgs( 6 );
			if ( ! lNumericFieldBlanks( 6 ) && VRF( VRFNum ).HeatingCapacity == AutoSize ) {
				VRF( VRFNum ).LockHeatingCapacity = true;
			}
			VRF( VRFNum ).HeatingCOP = rNumericArgs( 7 );
			VRF( VRFNum ).MinOATHeating = rNumericArgs( 8 );
			VRF( VRFNum ).MaxOATHeating = rNumericArgs( 9 );
			if ( VRF( VRFNum ).MinOATHeating >= VRF( VRFNum ).MaxOATHeating ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\"" );
				ShowContinueError( "... " + cNumericFieldNames( 8 ) + " (" + TrimSigDigits( VRF( VRFNum ).MinOATHeating, 3 ) + ") must be less than maximum (" + TrimSigDigits( VRF( VRFNum ).MaxOATHeating, 3 ) + ")." );
				ErrorsFound = true;
			}

			VRF( VRFNum ).HeatCapFT = GetCurveIndex( cAlphaArgs( 13 ) );
			if ( VRF( VRFNum ).HeatCapFT > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatCapFT ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 13 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatCapFT ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
				//  only show error if heating coil is present, since TU's have not yet been read, do this later in GetInput
				//      ELSE
				//        CALL ShowSevereError(TRIM(cCurrentModuleObject) //', "'//TRIM(VRF(VRFNum)%Name) // &
				//                     '" '//TRIM(cAlphaFieldNames(13)) //' not found.')
				//        ErrorsFound=.TRUE.
			}

			VRF( VRFNum ).HeatBoundaryCurvePtr = GetCurveIndex( cAlphaArgs( 14 ) );
			if ( VRF( VRFNum ).HeatBoundaryCurvePtr > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatBoundaryCurvePtr ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 14 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatBoundaryCurvePtr ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).HeatCapFTHi = GetCurveIndex( cAlphaArgs( 15 ) );
			if ( VRF( VRFNum ).HeatCapFTHi > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatCapFTHi ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 15 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatCapFTHi ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).HeatEIRFT = GetCurveIndex( cAlphaArgs( 16 ) );
			if ( VRF( VRFNum ).HeatEIRFT > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatEIRFT ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 16 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatEIRFT ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
				//  only show error if heating coil is present, since TU's have not yet been read, do this later in GetInput
				//      ELSE
				//        CALL ShowSevereError(TRIM(cCurrentModuleObject) //', "'//TRIM(VRF(VRFNum)%Name) // &
				//                     '" '//TRIM(cAlphaFieldNames(16)) //' not found.')
				//        ErrorsFound=.TRUE.
			}

			VRF( VRFNum ).EIRHeatBoundaryCurvePtr = GetCurveIndex( cAlphaArgs( 17 ) );
			if ( VRF( VRFNum ).EIRHeatBoundaryCurvePtr > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).EIRHeatBoundaryCurvePtr ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 17 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).EIRHeatBoundaryCurvePtr ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).HeatEIRFTHi = GetCurveIndex( cAlphaArgs( 18 ) );
			if ( VRF( VRFNum ).HeatEIRFTHi > 0 ) {
				// Verify Curve Object, only legal type is biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatEIRFTHi ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 18 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatEIRFTHi ) );
					ShowContinueError( "... curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			if ( SameString( cAlphaArgs( 19 ), "WETBULBTEMPERATURE" ) ) {
				VRF( VRFNum ).HeatingPerformanceOATType = WetBulbIndicator;
			} else if ( SameString( cAlphaArgs( 19 ), "DRYBULBTEMPERATURE" ) ) {
				VRF( VRFNum ).HeatingPerformanceOATType = DryBulbIndicator;
			} else {
				ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 19 ) + " input for this object = " + cAlphaArgs( 19 ) );
				ShowContinueError( "... input must be WETBULBTEMPERATURE or DRYBULBTEMPERATURE." );
				ErrorsFound = true;
			}

			VRF( VRFNum ).HeatEIRFPLR1 = GetCurveIndex( cAlphaArgs( 20 ) );
			if ( VRF( VRFNum ).HeatEIRFPLR1 > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatEIRFPLR1 ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 20 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatEIRFPLR1 ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).HeatEIRFPLR2 = GetCurveIndex( cAlphaArgs( 21 ) );
			if ( VRF( VRFNum ).HeatEIRFPLR2 > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatEIRFPLR2 ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 21 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatEIRFPLR2 ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).HeatCombRatioPTR = GetCurveIndex( cAlphaArgs( 22 ) );
			if ( VRF( VRFNum ).HeatCombRatioPTR > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatCombRatioPTR ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 22 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatCombRatioPTR ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}
			VRF( VRFNum ).HeatPLFFPLR = GetCurveIndex( cAlphaArgs( 23 ) );
			if ( VRF( VRFNum ).HeatPLFFPLR > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, or cubic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HeatPLFFPLR ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 23 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HeatPLFFPLR ) );
					ShowContinueError( "... curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			VRF( VRFNum ).MinPLR = rNumericArgs( 10 );

			VRF( VRFNum ).MasterZonePtr = FindItemInList( cAlphaArgs( 24 ), Zone );

			if ( SameString( cAlphaArgs( 25 ), "LoadPriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = LoadPriority;
			} else if ( SameString( cAlphaArgs( 25 ), "ZonePriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = ZonePriority;
			} else if ( SameString( cAlphaArgs( 25 ), "ThermostatOffsetPriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = ThermostatOffsetPriority;
			} else if ( SameString( cAlphaArgs( 25 ), "Scheduled" ) ) {
				VRF( VRFNum ).ThermostatPriority = ScheduledPriority;
			} else if ( SameString( cAlphaArgs( 25 ), "MasterThermostatPriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = MasterThermostatPriority;
				if ( VRF( VRFNum ).MasterZonePtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\"" );
					ShowContinueError( cAlphaFieldNames( 24 ) + " must be entered when " + cAlphaFieldNames( 25 ) + " = " + cAlphaArgs( 25 ) );
					ErrorsFound = true;
				}
				//      ELSE IF (SameString(cAlphaArgs(25),'FirstOnPriority') )THEN ! strategy not used
				//        VRF(VRFNum)%ThermostatPriority = FirstOnPriority
			} else {
				ShowSevereError( cCurrentModuleObject + " = " + VRF( VRFNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFieldNames( 25 ) + " = " + cAlphaArgs( 25 ) );
				ErrorsFound = true;
			}

			if ( VRF( VRFNum ).ThermostatPriority == ScheduledPriority ) {
				VRF( VRFNum ).SchedPriorityPtr = GetScheduleIndex( cAlphaArgs( 26 ) );
				if ( VRF( VRFNum ).SchedPriorityPtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = " + VRF( VRFNum ).Name );
					ShowContinueError( "..." + cAlphaFieldNames( 26 ) + " = " + cAlphaArgs( 26 ) + " not found." );
					ShowContinueError( "A schedule name is required when " + cAlphaFieldNames( 25 ) + " = " + cAlphaArgs( 25 ) );
					ErrorsFound = true;
				}
			}

			VRF( VRFNum ).ZoneTUListPtr = FindItemInList( cAlphaArgs( 27 ), TerminalUnitList );
			if ( VRF( VRFNum ).ZoneTUListPtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\"" );
				ShowContinueError( cAlphaFieldNames( 27 ) + " = " + cAlphaArgs( 27 ) + " not found." );
				ErrorsFound = true;
			}

			VRF( VRFNum ).HeatRecoveryUsed = false;
			if ( ! lAlphaFieldBlanks( 28 ) ) {
				if ( SameString( cAlphaArgs( 28 ), "No" ) ) {
					VRF( VRFNum ).HeatRecoveryUsed = false;
				} else if ( SameString( cAlphaArgs( 28 ), "Yes" ) ) {
					VRF( VRFNum ).HeatRecoveryUsed = true;
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + VRF( VRFNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFieldNames( 28 ) + " = " + cAlphaArgs( 28 ) );
					ErrorsFound = true;
				}
			}

			VRF( VRFNum ).EquivPipeLngthCool = rNumericArgs( 11 );
			VRF( VRFNum ).VertPipeLngth = rNumericArgs( 12 );
			VRF( VRFNum ).PCFLengthCoolPtr = GetCurveIndex( cAlphaArgs( 29 ) );
			if ( VRF( VRFNum ).PCFLengthCoolPtr > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, cubic, or biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).PCFLengthCoolPtr ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					VRF( VRFNum ).PCFLengthCoolPtrType = BiQuadratic;
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 29 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).PCFLengthCoolPtr ) );
					ShowContinueError( "... curve type must be Linear, Quadratic, Cubic or BiQuadratic." );
					ErrorsFound = true;
				}}
			}
			VRF( VRFNum ).PCFHeightCool = rNumericArgs( 13 );

			VRF( VRFNum ).EquivPipeLngthHeat = rNumericArgs( 14 );
			VRF( VRFNum ).PCFLengthHeatPtr = GetCurveIndex( cAlphaArgs( 30 ) );
			if ( VRF( VRFNum ).PCFLengthHeatPtr > 0 ) {
				// Verify Curve Object, only legal type is linear, quadratic, cubic, or biquadratic
				{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).PCFLengthHeatPtr ) );
				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					VRF( VRFNum ).PCFLengthHeatPtrType = BiQuadratic;
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 30 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).PCFLengthHeatPtr ) );
					ShowContinueError( "... curve type must be Linear, Quadratic, Cubic or BiQuadratic." );
					ErrorsFound = true;
				}}
			}
			VRF( VRFNum ).PCFHeightHeat = rNumericArgs( 15 );

			VRF( VRFNum ).CCHeaterPower = rNumericArgs( 16 );
			VRF( VRFNum ).NumCompressors = rNumericArgs( 17 );
			VRF( VRFNum ).CompressorSizeRatio = rNumericArgs( 18 );
			VRF( VRFNum ).MaxOATCCHeater = rNumericArgs( 19 );

			if ( ! lAlphaFieldBlanks( 31 ) ) {
				if ( SameString( cAlphaArgs( 31 ), "ReverseCycle" ) ) VRF( VRFNum ).DefrostStrategy = ReverseCycle;
				if ( SameString( cAlphaArgs( 31 ), "Resistive" ) ) VRF( VRFNum ).DefrostStrategy = Resistive;
				if ( VRF( VRFNum ).DefrostStrategy == 0 ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 31 ) + " not found: " + cAlphaArgs( 31 ) );
					ErrorsFound = true;
				}
			} else {
				VRF( VRFNum ).DefrostStrategy = ReverseCycle;
			}

			if ( ! lAlphaFieldBlanks( 32 ) ) {
				if ( SameString( cAlphaArgs( 32 ), "Timed" ) ) VRF( VRFNum ).DefrostControl = Timed;
				if ( SameString( cAlphaArgs( 32 ), "OnDemand" ) ) VRF( VRFNum ).DefrostControl = OnDemand;
				if ( VRF( VRFNum ).DefrostControl == 0 ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 32 ) + " not found: " + cAlphaArgs( 32 ) );
					ErrorsFound = true;
				}
			} else {
				VRF( VRFNum ).DefrostControl = Timed;
			}

			if ( ! lAlphaFieldBlanks( 33 ) ) {
				VRF( VRFNum ).DefrostEIRPtr = GetCurveIndex( cAlphaArgs( 33 ) );
				if ( VRF( VRFNum ).DefrostEIRPtr > 0 ) {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).DefrostEIRPtr ) );
					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 33 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).DefrostEIRPtr ) );
						ShowContinueError( "... curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				} else {
					if ( VRF( VRFNum ).DefrostStrategy == ReverseCycle ) {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 33 ) + " not found:" + cAlphaArgs( 33 ) );
						ErrorsFound = true;
					}
				}
			} else {
				if ( VRF( VRFNum ).DefrostStrategy == ReverseCycle ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 33 ) + " not found:" + cAlphaArgs( 33 ) );
					ErrorsFound = true;
				}
			}

			VRF( VRFNum ).DefrostFraction = rNumericArgs( 20 );
			VRF( VRFNum ).DefrostCapacity = rNumericArgs( 21 );
			if ( VRF( VRFNum ).DefrostCapacity == 0.0 && VRF( VRFNum ).DefrostStrategy == Resistive ) {
				ShowWarningError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cNumericFieldNames( 21 ) + " = 0.0 for defrost strategy = RESISTIVE." );
			}

			VRF( VRFNum ).MaxOATDefrost = rNumericArgs( 22 );

			if ( ! lAlphaFieldBlanks( 35 ) ) {
				if ( SameString( cAlphaArgs( 34 ), "AirCooled" ) ) VRF( VRFNum ).CondenserType = AirCooled;
				if ( SameString( cAlphaArgs( 34 ), "EvaporativelyCooled" ) ) VRF( VRFNum ).CondenserType = EvapCooled;
				if ( SameString( cAlphaArgs( 34 ), "WaterCooled" ) ) {
					VRF( VRFNum ).CondenserType = WaterCooled;
					VRF( VRFNum ).VRFPlantTypeOfNum = TypeOf_HeatPumpVRF;
				}
				if ( VRF( VRFNum ).CondenserType == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = " + VRF( VRFNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFieldNames( 34 ) + " = " + cAlphaArgs( 34 ) );
					ErrorsFound = true;
				}
			} else {
				VRF( VRFNum ).CondenserType = AirCooled;
			}

			// outdoor condenser node
			if ( lAlphaFieldBlanks( 35 ) ) {
				VRF( VRFNum ).CondenserNodeNum = 0;
			} else {
				{ auto const SELECT_CASE_var( VRF( VRFNum ).CondenserType );
				if ( ( SELECT_CASE_var == AirCooled ) || ( SELECT_CASE_var == EvapCooled ) ) {
					VRF( VRFNum ).CondenserNodeNum = GetOnlySingleNode( cAlphaArgs( 35 ), ErrorsFound, cCurrentModuleObject, VRF( VRFNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
					if ( ! CheckOutAirNodeNumber( VRF( VRFNum ).CondenserNodeNum ) ) {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 35 ) + " not a valid Outdoor Air Node = " + cAlphaArgs( 35 ) );
						ShowContinueError( "...node name does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == WaterCooled ) {
					VRF( VRFNum ).CondenserNodeNum = GetOnlySingleNode( cAlphaArgs( 35 ), ErrorsFound, cCurrentModuleObject, VRF( VRFNum ).Name, NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				} else {
				}}
			}

			if ( ! lAlphaFieldBlanks( 36 ) && VRF( VRFNum ).CondenserType == WaterCooled ) {
				VRF( VRFNum ).CondenserOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 36 ), ErrorsFound, cCurrentModuleObject, VRF( VRFNum ).Name, NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, VRF( VRFNum ).Name, cAlphaArgs( 35 ), cAlphaArgs( 36 ), "Condenser Water Nodes" );
			} else if ( lAlphaFieldBlanks( 36 ) && VRF( VRFNum ).CondenserType == WaterCooled ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 36 ) + " is blank." );
				ShowContinueError( "...node name must be entered when Condenser Type = WaterCooled." );
				ErrorsFound = true;
			}

			if ( lNumericFieldBlanks( 23 ) ) {
				if ( VRF( VRFNum ).CondenserType == WaterCooled ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cNumericFieldNames( 23 ) + " is blank." );
					ShowContinueError( "...input is required when " + cAlphaFieldNames( 34 ) + " = " + cAlphaArgs( 34 ) );
					ErrorsFound = true;
				}
			} else {
				VRF( VRFNum ).WaterCondVolFlowRate = rNumericArgs( 23 );
			}
			VRF( VRFNum ).EvapCondEffectiveness = rNumericArgs( 24 );
			VRF( VRFNum ).EvapCondAirVolFlowRate = rNumericArgs( 25 );
			VRF( VRFNum ).EvapCondPumpPower = rNumericArgs( 26 );

			// Get Water System tank connections
			// A37, \field Supply Water Storage Tank Name
			VRF( VRFNum ).EvapWaterSupplyName = cAlphaArgs( 37 );
			if ( lAlphaFieldBlanks( 37 ) ) {
				VRF( VRFNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				VRF( VRFNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( VRF( VRFNum ).Name, cCurrentModuleObject, VRF( VRFNum ).EvapWaterSupplyName, ErrorsFound, VRF( VRFNum ).EvapWaterSupTankID, VRF( VRFNum ).EvapWaterTankDemandARRID );
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			VRF( VRFNum ).BasinHeaterPowerFTempDiff = rNumericArgs( 27 );
			if ( rNumericArgs( 27 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\", " + cNumericFieldNames( 27 ) + " must be >= 0" );
				ErrorsFound = true;
			}

			VRF( VRFNum ).BasinHeaterSetPointTemp = rNumericArgs( 28 );
			if ( VRF( VRFNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 27 ) {
					VRF( VRFNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( VRF( VRFNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\", " + cNumericFieldNames( 28 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 38 ) ) {
				VRF( VRFNum ).BasinHeaterSchedulePtr = GetScheduleIndex( cAlphaArgs( 38 ) );
				if ( VRF( VRFNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\", " + cAlphaFieldNames( 38 ) + " = \"" + cAlphaArgs( 38 ) + "\" was not found." );
					ShowContinueError( "Basin heater will be available to operate throughout the simulation." );
				}
			}

			VRF( VRFNum ).FuelType = FuelTypeElectric;
			if ( ! lAlphaFieldBlanks( 39 ) ) {
				//A39; \field Fuel type
				if ( SameString( cAlphaArgs( 39 ), "ELECTRICITY" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeElectric;
				} else if ( SameString( cAlphaArgs( 39 ), "ELECTRIC" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeElectric;
				} else if ( SameString( cAlphaArgs( 39 ), "NATURALGAS" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeNaturalGas;
				} else if ( SameString( cAlphaArgs( 39 ), "PROPANEGAS" ) ) {
					VRF( VRFNum ).FuelType = FuelTypePropaneGas;
				} else if ( SameString( cAlphaArgs( 39 ), "DIESEL" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeDiesel;
				} else if ( SameString( cAlphaArgs( 39 ), "GASOLINE" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeGasoline;
				} else if ( SameString( cAlphaArgs( 39 ), "FUELOIL#1" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeFuelOil1;
				} else if ( SameString( cAlphaArgs( 39 ), "FUELOIL#2" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeFuelOil2;
				} else if ( SameString( cAlphaArgs( 39 ), "OtherFuel1" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeOtherFuel1;
				} else if ( SameString( cAlphaArgs( 39 ), "OtherFuel2" ) ) {
					VRF( VRFNum ).FuelType = FuelTypeOtherFuel2;
				} else {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\", " + cAlphaFieldNames( 39 ) + " not found = " + cAlphaArgs( 39 ) );
					ShowContinueError( "Valid choices are Electric, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2, OtherFuel1 or OtherFuel2" );
					ErrorsFound = true;
				}
			}

			//  REAL(r64)    :: MinOATHeatRecovery         =0.0d0 ! Minimum outdoor air temperature for heat recovery operation (C)
			//  REAL(r64)    :: MaxOATHeatRecovery         =0.0d0 ! Maximum outdoor air temperature for heat recovery operation (C)
			if ( VRF( VRFNum ).HeatRecoveryUsed ) {
				if ( lNumericFieldBlanks( 29 ) ) {
					VRF( VRFNum ).MinOATHeatRecovery = max( VRF( VRFNum ).MinOATCooling, VRF( VRFNum ).MinOATHeating );
				} else {
					VRF( VRFNum ).MinOATHeatRecovery = rNumericArgs( 29 );
					if ( VRF( VRFNum ).MinOATHeatRecovery < VRF( VRFNum ).MinOATCooling || VRF( VRFNum ).MinOATHeatRecovery < VRF( VRFNum ).MinOATHeating ) {
						ShowWarningError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\", " + cNumericFieldNames( 29 ) + " is less than the minimum temperature in heat pump mode." );
						ShowContinueError( "..." + cNumericFieldNames( 29 ) + " = " + TrimSigDigits( VRF( VRFNum ).MinOATHeatRecovery, 2 ) + " C" );
						ShowContinueError( "...Minimum Outdoor Temperature in Cooling Mode = " + TrimSigDigits( VRF( VRFNum ).MinOATCooling, 2 ) + " C" );
						ShowContinueError( "...Minimum Outdoor Temperature in Heating Mode = " + TrimSigDigits( VRF( VRFNum ).MinOATHeating, 2 ) + " C" );
						ShowContinueError( "...Minimum Outdoor Temperature in Heat Recovery Mode reset to greater of cooling or heating minimum temperature and simulation continues." );
						VRF( VRFNum ).MinOATHeatRecovery = max( VRF( VRFNum ).MinOATCooling, VRF( VRFNum ).MinOATHeating );
						ShowContinueError( "... adjusted " + cNumericFieldNames( 29 ) + " = " + TrimSigDigits( VRF( VRFNum ).MinOATHeatRecovery, 2 ) + " C" );
					}
				}
				if ( lNumericFieldBlanks( 30 ) ) {
					VRF( VRFNum ).MaxOATHeatRecovery = min( VRF( VRFNum ).MaxOATCooling, VRF( VRFNum ).MaxOATHeating );
				} else {
					VRF( VRFNum ).MaxOATHeatRecovery = rNumericArgs( 30 );
					if ( VRF( VRFNum ).MaxOATHeatRecovery > VRF( VRFNum ).MaxOATCooling || VRF( VRFNum ).MaxOATHeatRecovery > VRF( VRFNum ).MaxOATHeating ) {
						ShowWarningError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\", " + cNumericFieldNames( 30 ) + " is greater than the maximum temperature in heat pump mode." );
						ShowContinueError( "..." + cNumericFieldNames( 30 ) + " = " + TrimSigDigits( VRF( VRFNum ).MaxOATHeatRecovery, 2 ) + " C" );
						ShowContinueError( "...Maximum Outdoor Temperature in Cooling Mode = " + TrimSigDigits( VRF( VRFNum ).MaxOATCooling, 2 ) + " C" );
						ShowContinueError( "...Maximum Outdoor Temperature in Heating Mode = " + TrimSigDigits( VRF( VRFNum ).MaxOATHeating, 2 ) + " C" );
						ShowContinueError( "...Maximum Outdoor Temperature in Heat Recovery Mode reset to lesser of cooling or heating minimum temperature and simulation continues." );
						VRF( VRFNum ).MaxOATHeatRecovery = min( VRF( VRFNum ).MaxOATCooling, VRF( VRFNum ).MaxOATHeating );
						ShowContinueError( "... adjusted " + cNumericFieldNames( 30 ) + " = " + TrimSigDigits( VRF( VRFNum ).MaxOATHeatRecovery, 2 ) + " C" );
					}
				}

				//  INTEGER      :: HRCAPFTCool                =0   ! Index to cool capacity as a function of temperature curve for heat recovery
				//  REAL(r64)    :: HRInitialCoolCapFrac       =0.0d0 ! Fractional cooling degradation at the start of heat recovery from cooling mode
				//  REAL(r64)    :: HRCoolCapTC                =0.0d0 ! Time constant used to recover from intial degratation in cooling heat recovery
				VRF( VRFNum ).HRCAPFTCool = GetCurveIndex( cAlphaArgs( 40 ) );
				if ( VRF( VRFNum ).HRCAPFTCool > 0 ) {
					// Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
					{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HRCAPFTCool ) );
					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
					} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						VRF( VRFNum ).HRCAPFTCoolType = BiQuadratic;
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 40 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HRCAPFTCool ) );
						ShowContinueError( "... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}
				if ( ! lNumericFieldBlanks( 31 ) ) {
					VRF( VRFNum ).HRInitialCoolCapFrac = rNumericArgs( 31 );
				}
				VRF( VRFNum ).HRCoolCapTC = rNumericArgs( 32 );

				//  INTEGER      :: HREIRFTCool                =0   ! Index to cool EIR as a function of temperature curve for heat recovery
				//  REAL(r64)    :: HRInitialCoolEIRFrac       =0.0d0 ! Fractional EIR degradation at the start of heat recovery from cooling mode
				//  REAL(r64)    :: HRCoolEIRTC                =0.0d0 ! Time constant used to recover from intial degratation in cooling heat recovery
				VRF( VRFNum ).HREIRFTCool = GetCurveIndex( cAlphaArgs( 41 ) );
				if ( VRF( VRFNum ).HREIRFTCool > 0 ) {
					// Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
					{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HREIRFTCool ) );
					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
					} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						VRF( VRFNum ).HREIRFTCoolType = BiQuadratic;
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 41 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HREIRFTCool ) );
						ShowContinueError( "... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}
				VRF( VRFNum ).HRInitialCoolEIRFrac = rNumericArgs( 33 );
				VRF( VRFNum ).HRCoolEIRTC = rNumericArgs( 34 );

				//  INTEGER      :: HRCAPFTHeat                =0   ! Index to heat capacity as a function of temperature curve for heat recovery
				//  REAL(r64)    :: HRInitialHeatCapFrac       =0.0d0 ! Fractional heating degradation at the start of heat recovery from heating mode
				//  REAL(r64)    :: HRHeatCapTC                =0.0d0 ! Time constant used to recover from intial degratation in heating heat recovery
				VRF( VRFNum ).HRCAPFTHeat = GetCurveIndex( cAlphaArgs( 42 ) );
				if ( VRF( VRFNum ).HRCAPFTHeat > 0 ) {
					// Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
					{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HRCAPFTHeat ) );
					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
					} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						VRF( VRFNum ).HRCAPFTHeatType = BiQuadratic;
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 42 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HRCAPFTHeat ) );
						ShowContinueError( "... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}
				VRF( VRFNum ).HRInitialHeatCapFrac = rNumericArgs( 35 );
				VRF( VRFNum ).HRHeatCapTC = rNumericArgs( 36 );

				//  INTEGER      :: HREIRFTHeat                =0   ! Index to heat EIR as a function of temperature curve for heat recovery
				//  REAL(r64)    :: HRInitialHeatEIRFrac       =0.0d0 ! Fractional EIR degradation at the start of heat recovery from heating mode
				//  REAL(r64)    :: HRHeatEIRTC                =0.0d0 ! Time constant used to recover from intial degratation in heating heat recovery
				VRF( VRFNum ).HREIRFTHeat = GetCurveIndex( cAlphaArgs( 43 ) );
				if ( VRF( VRFNum ).HREIRFTHeat > 0 ) {
					// Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
					{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).HREIRFTHeat ) );
					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
					} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						VRF( VRFNum ).HREIRFTHeatType = BiQuadratic;
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 43 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).HREIRFTHeat ) );
						ShowContinueError( "... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}
				VRF( VRFNum ).HRInitialHeatEIRFrac = rNumericArgs( 37 );
				VRF( VRFNum ).HRHeatEIRTC = rNumericArgs( 38 );

			} else {
			}

			if ( VRF( VRFNum ).CondenserType == WaterCooled ) {

				//scan for loop connection data
				errFlag = false;
				ScanPlantLoopsForObject( VRF( VRFNum ).Name, VRF( VRFNum ).VRFPlantTypeOfNum, VRF( VRFNum ).SourceLoopNum, VRF( VRFNum ).SourceLoopSideNum, VRF( VRFNum ).SourceBranchNum, VRF( VRFNum ).SourceCompNum, _, _, _, VRF( VRFNum ).CondenserNodeNum, _, errFlag );

				if ( errFlag ) {
					ShowSevereError( "GetVRFInput: Error scanning for plant loop data" );
					ErrorsFound = true;
				}

			}

		}

		// Read all VRF condenser objects: Algorithm Type 2_physics based model (FluidTCtrl)_Aug. 2015, zrp
		cCurrentModuleObject = "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl";
		for ( VRFNum = NumVRFCond_SysCurve + 1; VRFNum <= NumVRFCond; ++VRFNum ) {
			GetObjectItem( cCurrentModuleObject, VRFNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), VRF, VRFNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VRF( VRFNum ).Name = cAlphaArgs( 1 );
			VRF( VRFNum ).VRFSystemTypeNum = VRF_HeatPump;
			VRF( VRFNum ).VRFAlgorithmTypeNum = AlgorithmTypeFluidTCtrl;
			VRF( VRFNum ).FuelType = FuelTypeElectric;

			if ( lAlphaFieldBlanks( 2 ) ) {
				VRF( VRFNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				VRF( VRFNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( VRF( VRFNum ).SchedPtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\" invalid data" );
					ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			VRF( VRFNum ).ZoneTUListPtr = FindItemInList( cAlphaArgs( 3 ), TerminalUnitList, NumVRFTULists );
			if ( VRF( VRFNum ).ZoneTUListPtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\"" );
				ShowContinueError( cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) + " not found." );
				ErrorsFound = true;
			}

			//Refrigerant type
			VRF( VRFNum ).RefrigerantName = cAlphaArgs( 4 );
			if ( EnergyPlus::FluidProperties::GetInput ) {
				EnergyPlus::FluidProperties::GetFluidPropertiesData();
				EnergyPlus::FluidProperties::GetInput = false;
			}
			if ( FindItemInList( VRF( VRFNum ).RefrigerantName, EnergyPlus::FluidProperties::RefrigData, EnergyPlus::FluidProperties::NumOfRefrigerants ) == 0 ) {
				ShowSevereError( cCurrentModuleObject + " = " + VRF( VRFNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFieldNames( 4 ) + " = " + cAlphaArgs( 4 ) );
				ErrorsFound = true;
			}

			VRF( VRFNum ).RatedEvapCapacity = rNumericArgs( 1 );
			VRF( VRFNum ).RatedCompPower = rNumericArgs( 2 ) * VRF( VRFNum ).RatedEvapCapacity;

			//Refrence system COP
			VRF( VRFNum ).CoolingCOP = VRF( VRFNum ).RatedEvapCapacity / VRF( VRFNum ).RatedCompPower;
			VRF( VRFNum ).HeatingCOP = VRF( VRFNum ).RatedEvapCapacity / VRF( VRFNum ).RatedCompPower + 1;

			//OA temperature range for VRF-HP operations
			VRF( VRFNum ).MinOATCooling = rNumericArgs( 3 );
			VRF( VRFNum ).MaxOATCooling = rNumericArgs( 4 );
			VRF( VRFNum ).MinOATHeating = rNumericArgs( 5 );
			VRF( VRFNum ).MaxOATHeating = rNumericArgs( 6 );
			if ( VRF( VRFNum ).MinOATCooling >= VRF( VRFNum ).MaxOATCooling ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\"" );
				ShowContinueError( "... " + cNumericFieldNames( 3 ) + " (" + TrimSigDigits( VRF( VRFNum ).MinOATCooling, 3 ) + ") must be less than maximum (" + TrimSigDigits( VRF( VRFNum ).MaxOATCooling, 3 ) + ")." );
				ErrorsFound = true;
			}
			if ( VRF( VRFNum ).MinOATHeating >= VRF( VRFNum ).MaxOATHeating ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\"" );
				ShowContinueError( "... " + cNumericFieldNames( 5 ) + " (" + TrimSigDigits( VRF( VRFNum ).MinOATHeating, 3 ) + ") must be less than maximum (" + TrimSigDigits( VRF( VRFNum ).MaxOATHeating, 3 ) + ")." );
				ErrorsFound = true;
			}

			//Refrence OU SH/SC
			VRF( VRFNum ).SH = rNumericArgs( 7 );
			VRF( VRFNum ).SC = rNumericArgs( 8 );

			if( SameString( cAlphaArgs( 5 ), "VariableTemp" ) ) {
				VRF(VRFNum).AlgorithmIUCtrl = 1;
			} else if ( SameString( cAlphaArgs( 5 ), "ConstantTemp" ) ) {
				VRF(VRFNum).AlgorithmIUCtrl = 2;
			} else {
				VRF(VRFNum).AlgorithmIUCtrl = 1;
			}

			//Refrence IU Te/Tc for IU Control Algorithm: ConstantTemp
			VRF( VRFNum ).EvapTempFixed  = rNumericArgs( 9 );
			VRF( VRFNum ).CondTempFixed  = rNumericArgs( 10 );

			//Bounds of Te/Tc for IU Control Algorithm: VariableTemp
			VRF( VRFNum ).IUEvapTempLow = rNumericArgs( 11 );
			VRF( VRFNum ).IUEvapTempHigh = rNumericArgs( 12 );
			VRF( VRFNum ).IUCondTempLow = rNumericArgs( 13 );
			VRF( VRFNum ).IUCondTempHigh = rNumericArgs( 14 );

			//Get OU fan data
			VRF( VRFNum ).RatedCondFanPower = rNumericArgs( 15 ) * VRF( VRFNum ).RatedEvapCapacity;
			VRF( VRFNum ).OUAirFlowRate = rNumericArgs( 16 ) * VRF( VRFNum ).RatedEvapCapacity;

			// OUEvapTempCurve
			int indexOUEvapTempCurve = GetCurveIndex( cAlphaArgs( 6 ) ); // convert curve name to index number
			// Verify curve name and type
			if ( indexOUEvapTempCurve == 0 ) {
				if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFieldNames( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				{ auto const SELECT_CASE_var( GetCurveType( indexOUEvapTempCurve ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						VRF( VRFNum ).C1Te = EnergyPlus::CurveManager::PerfCurve( indexOUEvapTempCurve ).Coeff1;
						VRF( VRFNum ).C2Te = EnergyPlus::CurveManager::PerfCurve( indexOUEvapTempCurve ).Coeff2;
						VRF( VRFNum ).C3Te = EnergyPlus::CurveManager::PerfCurve( indexOUEvapTempCurve ).Coeff3;

					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 6 ) + " type for this object = " + GetCurveType( indexOUEvapTempCurve ) );
						ShowContinueError( "... Curve type must be Quadratic." );
						ErrorsFound = true;
					}
				}
			}

			// OUCondTempCurve
			int indexOUCondTempCurve = GetCurveIndex( cAlphaArgs( 7 ) ); // convert curve name to index number
			// Verify curve name and type
			if ( indexOUCondTempCurve == 0 ) {
				if ( lAlphaFieldBlanks( 7 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFieldNames( 7 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				{ auto const SELECT_CASE_var( GetCurveType( indexOUCondTempCurve ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						VRF( VRFNum ).C1Tc = EnergyPlus::CurveManager::PerfCurve( indexOUCondTempCurve ).Coeff1;
						VRF( VRFNum ).C2Tc = EnergyPlus::CurveManager::PerfCurve( indexOUCondTempCurve ).Coeff2;
						VRF( VRFNum ).C3Tc = EnergyPlus::CurveManager::PerfCurve( indexOUCondTempCurve ).Coeff3;

					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 7 ) + " type for this object = " + GetCurveType( indexOUCondTempCurve ) );
						ShowContinueError( "... Curve type must be Quadratic." );
						ErrorsFound = true;
					}
				}
			}

			// Pipe parameters
			VRF( VRFNum ).RefPipDia      = rNumericArgs( 17 );
			VRF( VRFNum ).RefPipLen      = rNumericArgs( 18 );
			VRF( VRFNum ).RefPipEquLen   = rNumericArgs( 19 );
			VRF( VRFNum ).RefPipHei      = rNumericArgs( 20 );
			VRF( VRFNum ).RefPipInsThi   = rNumericArgs( 21 );
			VRF( VRFNum ).RefPipInsCon   = rNumericArgs( 22 );

			// Check the RefPipEquLen
			if ( lNumericFieldBlanks( 19 ) && !lNumericFieldBlanks( 28 ) ) {
				VRF( VRFNum ).RefPipEquLen = 1.2 * VRF( VRFNum ).RefPipLen;
				ShowWarningError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\", \" " + cNumericFieldNames( 19 ) + "\" is calculated based on" );
				ShowContinueError( "...the provided \"" + cNumericFieldNames( 18 ) + "\" value." );
			}
			if ( VRF( VRFNum ).RefPipEquLen < VRF( VRFNum ).RefPipLen ) {
				VRF( VRFNum ).RefPipEquLen = 1.2 * VRF( VRFNum ).RefPipLen;
				ShowWarningError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\", invalid \" " + cNumericFieldNames( 19 ) + "\" value." );
				ShowContinueError( "...Equivalent length of main pipe should be greater than or equal to the actural length." );
				ShowContinueError( "...The value is recalculated based on the provided \"" + cNumericFieldNames( 18 ) + "\" value." );
			}

			// Crank case
			VRF( VRFNum ).CCHeaterPower = rNumericArgs( 23 );
			VRF( VRFNum ).NumCompressors = rNumericArgs( 24 );
			VRF( VRFNum ).CompressorSizeRatio = rNumericArgs( 25 );
			VRF( VRFNum ).MaxOATCCHeater = rNumericArgs( 26 );

			//Defrost
			if ( ! lAlphaFieldBlanks( 8 ) ) {
				if ( SameString( cAlphaArgs( 8 ), "ReverseCycle" ) ) VRF( VRFNum ).DefrostStrategy = ReverseCycle;
				if ( SameString( cAlphaArgs( 8 ), "Resistive" ) ) VRF( VRFNum ).DefrostStrategy = Resistive;
				if ( VRF( VRFNum ).DefrostStrategy == 0 ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 8 ) + " not found: " + cAlphaArgs( 8 ) );
					ErrorsFound = true;
				}
			} else {
				VRF( VRFNum ).DefrostStrategy = ReverseCycle;
			}

			if ( ! lAlphaFieldBlanks( 9 ) ) {
				if ( SameString( cAlphaArgs( 9 ), "Timed" ) ) VRF( VRFNum ).DefrostControl = Timed;
				if ( SameString( cAlphaArgs( 9 ), "OnDemand" ) ) VRF( VRFNum ).DefrostControl = OnDemand;
				if ( VRF( VRFNum ).DefrostControl == 0 ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 9 ) + " not found: " + cAlphaArgs( 9 ) );
					ErrorsFound = true;
				}
			} else {
				VRF( VRFNum ).DefrostControl = Timed;
			}

			if ( ! lAlphaFieldBlanks( 10 ) ) {
				VRF( VRFNum ).DefrostEIRPtr = GetCurveIndex( cAlphaArgs( 10 ) );
				if ( VRF( VRFNum ).DefrostEIRPtr > 0 ) {
					// Verify Curve Object, only legal type is linear, quadratic, or cubic
					{ auto const SELECT_CASE_var( GetCurveType( VRF( VRFNum ).DefrostEIRPtr ) );
					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" illegal " + cAlphaFieldNames( 10 ) + " type for this object = " + GetCurveType( VRF( VRFNum ).DefrostEIRPtr ) );
						ShowContinueError( "... curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				} else {
					if ( VRF( VRFNum ).DefrostStrategy == ReverseCycle && VRF( VRFNum ).DefrostControl == OnDemand ) {
						ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 10 ) + " not found:" + cAlphaArgs( 10 ) );
						ErrorsFound = true;
					}
				}
			} else {
				if ( VRF( VRFNum ).DefrostStrategy == ReverseCycle && VRF( VRFNum ).DefrostControl == OnDemand ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cAlphaFieldNames( 10 ) + " not found:" + cAlphaArgs( 10 ) );
					ErrorsFound = true;
				}
			}

			VRF( VRFNum ).DefrostFraction = rNumericArgs( 27 );
			VRF( VRFNum ).DefrostCapacity = rNumericArgs( 28 );
			VRF( VRFNum ).MaxOATDefrost = rNumericArgs( 29 );
			if ( VRF( VRFNum ).DefrostCapacity == 0.0 && VRF( VRFNum ).DefrostStrategy == Resistive ) {
				ShowWarningError( cCurrentModuleObject + ", \"" + VRF( VRFNum ).Name + "\" " + cNumericFieldNames( 28 ) + " = 0.0 for defrost strategy = RESISTIVE." );
			}

			VRF( VRFNum ).CompMaxDeltaP  = rNumericArgs( 30 );

			//@@ The control type
			std::string ThermostatPriorityType = "LoadPriority"; // cAlphaArgs( 25 )
			if ( SameString( ThermostatPriorityType, "LoadPriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = LoadPriority;
			} else if ( SameString( ThermostatPriorityType, "ZonePriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = ZonePriority;
			} else if ( SameString( ThermostatPriorityType, "ThermostatOffsetPriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = ThermostatOffsetPriority;
			} else if ( SameString( ThermostatPriorityType, "Scheduled" ) ) {
				VRF( VRFNum ).ThermostatPriority = ScheduledPriority;
			} else if ( SameString( ThermostatPriorityType, "MasterThermostatPriority" ) ) {
				VRF( VRFNum ).ThermostatPriority = MasterThermostatPriority;
				if ( VRF( VRFNum ).MasterZonePtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + VRF( VRFNum ).Name + "\"" );
					//** ShowContinueError( cAlphaFieldNames( 24 ) + " must be entered when " + cAlphaFieldNames( 25 ) + " = " + cAlphaArgs( 25 ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + " = " + VRF( VRFNum ).Name );
				// ShowContinueError( "Illegal " + cAlphaFieldNames( 25 ) + " = " + cAlphaArgs( 25 ) );
				ErrorsFound = true;
			}

			// The new VRF model is Air cooled
			VRF( VRFNum ).CondenserType = AirCooled;
			VRF( VRFNum ).CondenserNodeNum = 0;

			// Evaporative Capacity & Compressor Power Curves corresponding to each Loading Index / compressor speed
			NumOfCompSpd = rNumericArgs( 31 );
			VRF( VRFNum ).CompressorSpeed.dimension( NumOfCompSpd );
			VRF( VRFNum ).OUCoolingCAPFT.dimension( NumOfCompSpd );
			VRF( VRFNum ).OUCoolingPWRFT.dimension( NumOfCompSpd );
			int Count1Index = 31; // the index of the last numeric field before compressor speed entries
			int Count2Index = 9; // the index of the last alpha field before capacity/power curves
			for ( NumCompSpd = 1; NumCompSpd <= NumOfCompSpd; NumCompSpd++ ) {
				VRF( VRFNum ).CompressorSpeed( NumCompSpd ) = rNumericArgs( Count1Index + NumCompSpd );

				// Evaporating Capacity Curve
				if ( ! lAlphaFieldBlanks( Count2Index + 2 * NumCompSpd ) ) {
					int indexOUEvapCapCurve = GetCurveIndex( cAlphaArgs( Count2Index + 2 * NumCompSpd ) ); // convert curve name to index number
					if ( indexOUEvapCapCurve == 0 ) {// Verify curve name and type
						if ( lAlphaFieldBlanks( Count2Index + 2 * NumCompSpd ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", missing" );
							ShowContinueError( "...required " + cAlphaFieldNames( Count2Index + 2 * NumCompSpd ) + " is blank." );
						} else {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
							ShowContinueError( "...not found " + cAlphaFieldNames( Count2Index + 2 * NumCompSpd ) + "=\"" + cAlphaArgs( Count2Index + 2 * NumCompSpd ) + "\"." );
						}
						ErrorsFound = true;
					} else {
						{ auto const SELECT_CASE_var( GetCurveType( indexOUEvapCapCurve ) );

							if ( SELECT_CASE_var == "BIQUADRATIC" ) {
								VRF( VRFNum ).OUCoolingCAPFT( NumCompSpd ) = indexOUEvapCapCurve;
							} else {
								ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
								ShowContinueError( "...illegal " + cAlphaFieldNames( Count2Index + 2 * NumCompSpd ) + " type for this object = " + GetCurveType( indexOUEvapCapCurve ) );
								ShowContinueError( "... Curve type must be BiQuadratic." );
								ErrorsFound = true;
							}
						}
					}
				}

				// Compressor Power Curve
				if ( ! lAlphaFieldBlanks( Count2Index + 2 * NumCompSpd + 1 ) ) {
					int indexOUCompPwrCurve = GetCurveIndex( cAlphaArgs( Count2Index + 2 * NumCompSpd + 1 ) ); // convert curve name to index number
					if ( indexOUCompPwrCurve == 0 ) {// Verify curve name and type
						if ( lAlphaFieldBlanks( Count2Index + 2 * NumCompSpd + 1 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", missing" );
							ShowContinueError( "...required " + cAlphaFieldNames( Count2Index + 2 * NumCompSpd + 1 ) + " is blank." );
						} else {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
							ShowContinueError( "...not found " + cAlphaFieldNames( Count2Index + 2 * NumCompSpd + 1 ) + "=\"" + cAlphaArgs( Count2Index + 2 * NumCompSpd + 1 ) + "\"." );
						}
						ErrorsFound = true;
					} else {
						{ auto const SELECT_CASE_var( GetCurveType( indexOUCompPwrCurve ) );

							if ( SELECT_CASE_var == "BIQUADRATIC" ) {
								VRF( VRFNum ).OUCoolingPWRFT( NumCompSpd ) = indexOUCompPwrCurve;
							} else {
								ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + VRF( VRFNum ).Name + "\", invalid" );
								ShowContinueError( "...illegal " + cAlphaFieldNames( Count2Index + 2 * NumCompSpd + 1 ) + " type for this object = " + GetCurveType( indexOUCompPwrCurve ) );
								ShowContinueError( "... Curve type must be BiQuadratic." );
								ErrorsFound = true;
							}
						}
					}
				}

			}
		}

		cCurrentModuleObject = "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow";
		for ( VRFNum = 1; VRFNum <= NumVRFTU; ++VRFNum ) {
			VRFTUNum = VRFNum;

			//     initialize local node number variables
			FanInletNodeNum = 0;
			FanOutletNodeNum = 0;
			CCoilInletNodeNum = 0;
			CCoilOutletNodeNum = 0;
			HCoilInletNodeNum = 0;
			HCoilOutletNodeNum = 0;

			GetObjectItem( cCurrentModuleObject, VRFTUNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			VRFTUNumericFields( VRFTUNum ).FieldNames.allocate(NumNums);
			VRFTUNumericFields( VRFTUNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), VRFTU, VRFTUNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VRFTU( VRFTUNum ).Name = cAlphaArgs( 1 );
			ZoneTerminalUnitListNum = 0;
			for ( NumList = 1; NumList <= NumVRFTULists; ++NumList ) {
				ZoneTerminalUnitListNum = FindItemInList( VRFTU( VRFTUNum ).Name, TerminalUnitList( NumList ).ZoneTUName, TerminalUnitList( NumList ).NumTUInList );
				if ( ZoneTerminalUnitListNum > 0 ) {
					VRFTU( VRFTUNum ).IndexToTUInTUList = ZoneTerminalUnitListNum;
					TerminalUnitList( NumList ).ZoneTUPtr( ZoneTerminalUnitListNum ) = VRFTUNum;
					VRFTU( VRFTUNum ).TUListIndex = NumList;
					break;
				}
			}
			if ( VRFTU( VRFTUNum ).TUListIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
				ShowContinueError( "Terminal unit not found on any ZoneTerminalUnitList." );
				ErrorsFound = true;
			}

			for ( NumCond = 1; NumCond <= NumVRFCond; ++NumCond ) {
				if ( VRF( NumCond ).ZoneTUListPtr != VRFTU( VRFTUNum ).TUListIndex ) continue;
				VRFTU( VRFTUNum ).VRFSysNum = NumCond;
				break;
			}
			VRFTU( VRFTUNum ).VRFTUType_Num = VRFTUType_ConstVolume;
			if ( lAlphaFieldBlanks( 2 ) ) {
				VRFTU( VRFTUNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				VRFTU( VRFTUNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( VRFTU( VRFTUNum ).SchedPtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + VRFTU( VRFTUNum ).Name + "\" invalid data" );
					ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			VRFTU( VRFTUNum ).VRFTUInletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, VRFTU( VRFTUNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			VRFTU( VRFTUNum ).VRFTUOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, VRFTU( VRFTUNum ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			VRFTU( VRFTUNum ).MaxCoolAirVolFlow = rNumericArgs( 1 );
			VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow = rNumericArgs( 2 );
			VRFTU( VRFTUNum ).MaxHeatAirVolFlow = rNumericArgs( 3 );
			VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow = rNumericArgs( 4 );
			VRFTU( VRFTUNum ).CoolOutAirVolFlow = rNumericArgs( 5 );
			VRFTU( VRFTUNum ).HeatOutAirVolFlow = rNumericArgs( 6 );
			VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow = rNumericArgs( 7 );

			VRFTU( VRFTUNum ).FanOpModeSchedPtr = GetScheduleIndex( cAlphaArgs( 5 ) );
			// default to constant fan operating mode
			if ( VRFTU( VRFTUNum ).FanOpModeSchedPtr == 0 ) {
				if ( ! lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
					ShowContinueError( "..." + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) + " not found." );
					ShowContinueError( "...Defaulting to constant fan operating mode and simulation continues." );
				}
				VRFTU( VRFTUNum ).OpMode = ContFanCycCoil;
			}

			if ( SameString( cAlphaArgs( 6 ), "BlowThrough" ) ) VRFTU( VRFTUNum ).FanPlace = BlowThru;
			if ( SameString( cAlphaArgs( 6 ), "DrawThrough" ) ) VRFTU( VRFTUNum ).FanPlace = DrawThru;
			if ( VRFTU( VRFTUNum ).FanPlace == 0 ) {
				ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
				ErrorsFound = true;
			}

			//Get fan data
			FanType = cAlphaArgs( 7 );
			FanName = cAlphaArgs( 8 );

			errFlag = false;
			GetFanType( FanName, FanType_Num, errFlag, cCurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "...occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
				ErrorsFound = true;
			}

			// Check the type of the fan is correct
			if ( ! SameString( cFanTypes( FanType_Num ), FanType ) ) {
				ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
				ShowContinueError( "Fan type specified = " + cAlphaArgs( 7 ) );
				ShowContinueError( "Based on the fan name the type of fan actually used = " + cFanTypes( FanType_Num ) );
				ErrorsFound = true;
			}

			if ( VRFTU(VRFTUNum).VRFSysNum > 0 ) {
				// VRFTU Supply Air Fan Object Type must be Fan:VariableVolume if VRF Algorithm Type is AlgorithmTypeFluidTCtrl
				if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl && FanType_Num != FanType_SimpleVAV ){
					ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
					ShowContinueError( "Fan type specified = " + cAlphaArgs( 7 ) );
					ShowContinueError( "Fan Object Type must be Fan:VariableVolume if VRF AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl" );
					ShowContinueError( "is used to model VRF outdoor unit." );
					ErrorsFound = true;
				}

				// VRFTU Supply Air Fan Object Type must be Fan:OnOff or Fan:ConstantVolume if VRF Algorithm Type is AlgorithmTypeSysCurve
				if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum == AlgorithmTypeSysCurve && !( FanType_Num == FanType_SimpleOnOff || FanType_Num == FanType_SimpleConstVolume ) ){
					ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
					ShowContinueError( "Fan type specified = " + cAlphaArgs( 7 ) );
					ShowContinueError( "Fan Object Type must be Fan:OnOff or Fan:ConstantVolume if VRF AirConditioner:VariableRefrigerantFlow" );
					ShowContinueError( "is used to model VRF outdoor unit." );
					ErrorsFound = true;
				}
			}

			if ( FanType_Num == FanType_SimpleOnOff || FanType_Num == FanType_SimpleConstVolume || FanType_Num == FanType_SimpleVAV ) {

				ValidateComponent( cFanTypes( FanType_Num ), FanName, IsNotOK, cCurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
					ErrorsFound = true;

				} else { // mine data from fan object

					// Get the fan index
					errFlag = false;
					GetFanIndex( FanName, VRFTU( VRFTUNum ).FanIndex, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
						ErrorsFound = true;
					}

					//Set the Design Fan Volume Flow Rate
					errFlag = false;
					FanVolFlowRate = GetFanDesignVolumeFlowRate( FanType, FanName, errFlag );
					VRFTU( VRFTUNum ).ActualFanVolFlowRate = FanVolFlowRate;

					if ( errFlag ) {
						ShowContinueError( "...occurs in " + cCurrentModuleObject + " =" + VRFTU( VRFTUNum ).Name );
						ErrorsFound = true;
					}

					// Get the Fan Inlet Node
					errFlag = false;
					FanInletNodeNum = GetFanInletNode( FanType, FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
						ErrorsFound = true;
					}

					// Get the Fan Outlet Node
					errFlag = false;
					FanOutletNodeNum = GetFanOutletNode( FanType, FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
						ErrorsFound = true;
					}

					// Get the fan's availabitlity schedule
					errFlag = false;
					VRFTU( VRFTUNum ).FanAvailSchedPtr = GetFanAvailSchPtr( FanType, FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
						ErrorsFound = true;
					}

					// Check fan's schedule for cycling fan operation if constant volume fan is used
					if ( VRFTU( VRFTUNum ).FanOpModeSchedPtr > 0 && FanType_Num == FanType_SimpleConstVolume ) {
						if ( ! CheckScheduleValueMinMax( VRFTU( VRFTUNum ).FanOpModeSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
							ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
							ShowContinueError( "For fan type = " + cFanTypes( FanType_SimpleConstVolume ) );
							ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0)." );
							ShowContinueError( "Error found in " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
							ShowContinueError( "...schedule values must be (>0., <=1.)" );
							ErrorsFound = true;
						}
					}

				} // IF (IsNotOK) THEN

			} else { // IF (FanType_Num == FanType_SimpleOnOff .OR. FanType_Num == FanType_SimpleConstVolume)THEN
				ShowSevereError( cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
				ErrorsFound = true;
			} // IF (FanType_Num == FanType_SimpleOnOff .OR. FanType_Num == FanType_SimpleConstVolume)THEN

			//Get OA mixer data
			OAMixerType = cAlphaArgs( 9 );

			if ( ! lAlphaFieldBlanks( 10 ) ) {
				VRFTU( VRFTUNum ).OAMixerName = cAlphaArgs( 10 );
				errFlag = false;
				OANodeNums = GetOAMixerNodeNumbers( VRFTU( VRFTUNum ).OAMixerName, errFlag );

				//       OANodeNums(1) = OAMixer(OAMixerNum)%InletNode
				//       OANodeNums(2) = OAMixer(OAMixerNum)%RelNode
				//       OANodeNums(3) = OAMixer(OAMixerNum)%RetNode
				//       OANodeNums(4) = OAMixer(OAMixerNum)%MixNode

				if ( errFlag ) {
					ShowContinueError( "Occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum ).Name );
					ErrorsFound = true;
				} else {
					VRFTU( VRFTUNum ).OAMixerUsed = true;
				}
				VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum = OANodeNums( 1 );
				VRFTU( VRFTUNum ).VRFTUOAMixerRelNodeNum = OANodeNums( 2 );
				VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum = OANodeNums( 3 );
			}

			//Get DX cooling coil data
			DXCoolingCoilType = cAlphaArgs( 11 );

			errFlag = false;
			VRFTU( VRFTUNum ).DXCoolCoilType_Num = GetCoilTypeNum( DXCoolingCoilType, cAlphaArgs( 12 ), errFlag, false );
			if ( VRFTU( VRFTUNum ).DXCoolCoilType_Num == 0 ) {
				VRFTU( VRFTUNum ).CoolingCoilPresent = false;
				if ( VRFTU( VRFTUNum ).TUListIndex > 0 && VRFTU( VRFTUNum ).IndexToTUInTUList > 0 ) {
					TerminalUnitList( VRFTU( VRFTUNum ).TUListIndex ).CoolingCoilPresent( VRFTU( VRFTUNum ).IndexToTUInTUList ) = false;
				}
			} else {
				if ( VRFTU(VRFTUNum).VRFSysNum > 0 ) {
					if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
					// Algorithm Type: VRF model based on physics, applicable for Fluid Temperature Control

						if ( SameString( cAllCoilTypes( VRFTU( VRFTUNum ).DXCoolCoilType_Num ), cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling ) ) ) {
							errFlag = false;
							if ( VRFTU( VRFTUNum ).TUListIndex > 0 && VRFTU( VRFTUNum ).IndexToTUInTUList > 0 ) {
								TerminalUnitList( VRFTU( VRFTUNum ).TUListIndex ).CoolingCoilAvailSchPtr( VRFTU( VRFTUNum ).IndexToTUInTUList ) = GetDXCoilAvailSchPtr( DXCoolingCoilType, cAlphaArgs( 12 ), errFlag );
							}
							GetDXCoilIndex( cAlphaArgs( 12 ), VRFTU( VRFTUNum ).CoolCoilIndex, errFlag, cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling ) );
							CCoilInletNodeNum = GetDXCoilInletNode( cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling ), cAlphaArgs( 12 ), errFlag );
							CCoilOutletNodeNum = GetDXCoilOutletNode( cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling ), cAlphaArgs( 12 ), errFlag );

							if ( errFlag ) ShowContinueError( "...occurs in " + cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );

							if ( VRFTU( VRFTUNum ).VRFSysNum > 0 ) {
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserType );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserNodeNum );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATCCHeater );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MinOATCooling );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATCooling );

								DXCoils::DXCoil( VRFTU( VRFTUNum ).CoolCoilIndex ).VRFIUPtr = VRFTUNum;
								DXCoils::DXCoil( VRFTU( VRFTUNum ).CoolCoilIndex ).VRFOUPtr = VRFTU( VRFTUNum ).VRFSysNum;
								DXCoils::DXCoil( VRFTU( VRFTUNum ).CoolCoilIndex ).SupplyFanIndex = VRFTU( VRFTUNum ).FanIndex;
								DXCoils::DXCoil( VRFTU( VRFTUNum ).CoolCoilIndex ).RatedAirVolFlowRate( 1 ) = EnergyPlus::Fans::Fan( VRFTU( VRFTUNum ).FanIndex ).MaxAirFlowRate;

							} else {
								ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
								ShowContinueError( "... when checking " + cAllCoilTypes( VRFTU( VRFTUNum ).DXCoolCoilType_Num ) + " \"" + cAlphaArgs( 12 ) + "\"" );
								ShowContinueError( "... terminal unit not connected to condenser." );
								ShowContinueError( "... check that terminal unit is specified in a terminal unit list object." );
								ShowContinueError( "... also check that the terminal unit list name is specified in an AirConditioner:VariableRefrigerantFlow object." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "... illegal " + cAlphaFieldNames( 12 ) + " = " + cAlphaArgs( 12 ) );
							ErrorsFound = true;
						}

					} else {
					// Algorithm Type: VRF model based on system curve

						if ( SameString( cAllCoilTypes( VRFTU( VRFTUNum ).DXCoolCoilType_Num ), cAllCoilTypes( CoilVRF_Cooling ) ) ) {
							if( VRFTU( VRFTUNum ).TUListIndex > 0 && VRFTU( VRFTUNum ).IndexToTUInTUList > 0 ) {
								TerminalUnitList( VRFTU( VRFTUNum ).TUListIndex ).CoolingCoilAvailSchPtr( VRFTU( VRFTUNum ).IndexToTUInTUList ) = GetDXCoilAvailSchPtr( DXCoolingCoilType, cAlphaArgs( 12 ), errFlag );
							} else {
								VRFTU( VRFTUNum ).CoolingCoilPresent = false;
							}
							errFlag = false;
							GetDXCoilIndex( cAlphaArgs( 12 ), VRFTU( VRFTUNum ).CoolCoilIndex, errFlag, cAllCoilTypes( CoilVRF_Cooling ) );
							CCoilInletNodeNum = GetDXCoilInletNode( cAllCoilTypes( CoilVRF_Cooling ), cAlphaArgs( 12 ), errFlag );
							CCoilOutletNodeNum = GetDXCoilOutletNode( cAllCoilTypes( CoilVRF_Cooling ), cAlphaArgs( 12 ), errFlag );

							if ( errFlag ) ShowContinueError( "...occurs in " + cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );

							SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserType );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserNodeNum );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATCCHeater );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MinOATCooling );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATCooling );

						} else {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "... illegal " + cAlphaFieldNames( 12 ) + " = " + cAlphaArgs( 12 ) );
							ErrorsFound = true;
						}
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
					ShowContinueError( "... when checking " + cAllCoilTypes( VRFTU( VRFTUNum ).DXCoolCoilType_Num ) + " \"" + cAlphaArgs( 12 ) + "\"" );
					ShowContinueError( "... terminal unit not connected to condenser." );
					ShowContinueError( "... check that terminal unit is specified in a terminal unit list object." );
					ShowContinueError( "... also check that the terminal unit list name is specified in an AirConditioner:VariableRefrigerantFlow object." );
					ErrorsFound = true;
				}
			}


			//Get DX heating coil data
			DXHeatingCoilType = cAlphaArgs( 13 );

			// Get the heating to cooling sizing ratio input before writing to DX heating coil data
			if ( ! lNumericFieldBlanks( 10 ) ) {
				VRFTU( VRFTUNum ).HeatingCapacitySizeRatio = rNumericArgs( 10 );
			}

			errFlag = false;
			VRFTU( VRFTUNum ).DXHeatCoilType_Num = GetCoilTypeNum( DXHeatingCoilType, cAlphaArgs( 14 ), errFlag, false );
			if ( VRFTU( VRFTUNum ).DXHeatCoilType_Num == 0 ) {
				VRFTU( VRFTUNum ).HeatingCoilPresent = false;
				if ( VRFTU( VRFTUNum ).TUListIndex > 0 && VRFTU( VRFTUNum ).IndexToTUInTUList > 0 ) {
					TerminalUnitList( VRFTU( VRFTUNum ).TUListIndex ).HeatingCoilPresent( VRFTU( VRFTUNum ).IndexToTUInTUList ) = false;
				}
			} else {
				if ( VRFTU( VRFTUNum ).VRFSysNum > 0 ) {
					if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
					// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control

						if ( SameString( cAllCoilTypes( VRFTU( VRFTUNum ).DXHeatCoilType_Num ), cAllCoilTypes( CoilVRF_FluidTCtrl_Heating ) ) ) {
							errFlag = false;
							if ( VRFTU( VRFTUNum ).TUListIndex > 0 && VRFTU( VRFTUNum ).IndexToTUInTUList > 0 ) {
								TerminalUnitList( VRFTU( VRFTUNum ).TUListIndex ).HeatingCoilAvailSchPtr( VRFTU( VRFTUNum ).IndexToTUInTUList ) = GetDXCoilAvailSchPtr( DXHeatingCoilType, cAlphaArgs( 14 ), errFlag );
							}
							GetDXCoilIndex( cAlphaArgs( 14 ), VRFTU( VRFTUNum ).HeatCoilIndex, errFlag, cAllCoilTypes( CoilVRF_FluidTCtrl_Heating ) );
							HCoilInletNodeNum = GetDXCoilInletNode( cAllCoilTypes( CoilVRF_FluidTCtrl_Heating ), cAlphaArgs( 14 ), errFlag );
							HCoilOutletNodeNum = GetDXCoilOutletNode( cAllCoilTypes( CoilVRF_FluidTCtrl_Heating ), cAlphaArgs( 14 ), errFlag );

							if ( errFlag ) ShowContinueError( "...occurs in " + cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );

							if ( VRFTU( VRFTUNum ).VRFSysNum > 0 ) {
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserType );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserNodeNum );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATCCHeater );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MinOATHeating );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATHeating );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingPerformanceOATType );
								// Set defrost controls in child object to trip child object defrost calculations
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostStrategy );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostControl );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostEIRPtr );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostFraction );
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATDefrost );
								// If defrost is disabled in the VRF condenser, it must be disabled in the DX coil
								// Defrost primarily handled in parent object, set defrost capacity to 1 to avoid autosizing.
								// Defrost capacity is used for nothing more than setting defrost power/consumption report
								// variables which are not reported. The coil's defrost algorythm IS used to derate the coil
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, 1.0 ); // DefrostCapacity=1.0

								DXCoils::DXCoil( VRFTU( VRFTUNum ).HeatCoilIndex ).VRFIUPtr = VRFTUNum;
								DXCoils::DXCoil( VRFTU( VRFTUNum ).HeatCoilIndex ).VRFOUPtr = VRFTU( VRFTUNum ).VRFSysNum;
								DXCoils::DXCoil( VRFTU( VRFTUNum ).HeatCoilIndex ).SupplyFanIndex = VRFTU( VRFTUNum ).FanIndex;
								DXCoils::DXCoil( VRFTU( VRFTUNum ).HeatCoilIndex ).RatedAirVolFlowRate( 1 ) = EnergyPlus::Fans::Fan( VRFTU( VRFTUNum ).FanIndex ).MaxAirFlowRate;

								// Terminal unit heating to cooling sizing ratio has precedence over VRF system sizing ratio
								if ( VRFTU( VRFTUNum ).HeatingCapacitySizeRatio > 1.0 ) {
									SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRFTU( VRFTUNum ).HeatingCapacitySizeRatio );
								} else if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingCapacitySizeRatio > 1.0 ) {
									SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingCapacitySizeRatio );
								}
							} else {
								ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
								ShowContinueError( "... when checking " + cAllCoilTypes( VRFTU( VRFTUNum ).DXHeatCoilType_Num ) + " \"" + cAlphaArgs( 14 ) + "\"" );
								ShowContinueError( "... terminal unit not connected to condenser." );
								ShowContinueError( "... check that terminal unit is specified in a terminal unit list object." );
								ShowContinueError( "... also check that the terminal unit list name is specified in an AirConditioner:VariableRefrigerantFlow object." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "... illegal " + cAlphaFieldNames( 14 ) + " = " + cAlphaArgs( 14 ) );
							ErrorsFound = true;
						}

					} else {
					// Algorithm Type: VRF model based on system curve
						if ( SameString( cAllCoilTypes( VRFTU( VRFTUNum ).DXHeatCoilType_Num ), cAllCoilTypes( CoilVRF_Heating ) ) ) {
							if( VRFTU( VRFTUNum ).TUListIndex > 0 && VRFTU( VRFTUNum ).IndexToTUInTUList > 0 ) {
								TerminalUnitList( VRFTU( VRFTUNum ).TUListIndex ).HeatingCoilAvailSchPtr( VRFTU( VRFTUNum ).IndexToTUInTUList ) = GetDXCoilAvailSchPtr( DXHeatingCoilType, cAlphaArgs( 14 ), errFlag );
							} else {
								VRFTU( VRFTUNum ).HeatingCoilPresent = false;
							}
							errFlag = false;
							GetDXCoilIndex( cAlphaArgs( 14 ), VRFTU( VRFTUNum ).HeatCoilIndex, errFlag, cAllCoilTypes( CoilVRF_Heating ) );
							HCoilInletNodeNum = GetDXCoilInletNode( cAllCoilTypes( CoilVRF_Heating ), cAlphaArgs( 14 ), errFlag );
							HCoilOutletNodeNum = GetDXCoilOutletNode( cAllCoilTypes( CoilVRF_Heating ), cAlphaArgs( 14 ), errFlag );

							if ( errFlag ) ShowContinueError( "...occurs in " + cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );

							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserType );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).CondenserNodeNum );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATCCHeater );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MinOATHeating );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATHeating );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingPerformanceOATType );
							// Set defrost controls in child object to trip child object defrost calculations
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostStrategy );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostControl );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostEIRPtr );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).DefrostFraction );
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).MaxOATDefrost );
							// If defrost is disabled in the VRF condenser, it must be disabled in the DX coil
							// Defrost primarily handled in parent object, set defrost capacity to 1 to avoid autosizing.
							// Defrost capacity is used for nothing more than setting defrost power/consumption report
							// variables which are not reported. The coil's defrost algorythm IS used to derate the coil
							SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, 1.0 ); // DefrostCapacity=1.0
							// Terminal unit heating to cooling sizing ratio has precedence over VRF system sizing ratio
							if ( VRFTU( VRFTUNum ).HeatingCapacitySizeRatio > 1.0 ) {
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRFTU( VRFTUNum ).HeatingCapacitySizeRatio );
							} else if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingCapacitySizeRatio > 1.0 ) {
								SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingCapacitySizeRatio );
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "... illegal " + cAlphaFieldNames( 14 ) + " = " + cAlphaArgs( 14 ) );
							ErrorsFound = true;
						}
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
					ShowContinueError( "... when checking " + cAllCoilTypes( VRFTU( VRFTUNum ).DXHeatCoilType_Num ) + " \"" + cAlphaArgs( 14 ) + "\"" );
					ShowContinueError( "... terminal unit not connected to condenser." );
					ShowContinueError( "... check that terminal unit is specified in a terminal unit list object." );
					ShowContinueError( "... also check that the terminal unit list name is specified in an AirConditioner:VariableRefrigerantFlow object." );
					ErrorsFound = true;
				}
			}

			if ( ! VRFTU( VRFTUNum ).CoolingCoilPresent && VRFTU( VRFTUNum ).DXCoolCoilType_Num == 0 && ! VRFTU( VRFTUNum ).HeatingCoilPresent && VRFTU( VRFTUNum ).DXHeatCoilType_Num == 0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
				ShowContinueError( "... no valid coils entered for this terminal unit. Simulation will not proceed." );
				ErrorsFound = true;
			}

			if ( ! lAlphaFieldBlanks( 15 ) ) {
				VRFTU( VRFTUNum ).AvailManagerListName = cAlphaArgs( 15 );
			}
			VRFTU( VRFTUNum ).ParasiticElec = rNumericArgs( 8 );
			VRFTU( VRFTUNum ).ParasiticOffElec = rNumericArgs( 9 );


			VRFTU( VRFTUNum ).HVACSizingIndex = 0;
			if ( ! lAlphaFieldBlanks( 16 ) ) {
				VRFTU( VRFTUNum ).HVACSizingIndex = FindItemInList( cAlphaArgs( 16 ), ZoneHVACSizing );
				if ( VRFTU( VRFTUNum ).HVACSizingIndex == 0 ) {
					ShowSevereError( cAlphaFieldNames( 16 ) + " = " + cAlphaArgs( 16 ) + " not found." );
					ShowContinueError( "Occurs in " + cCurrentModuleObject + " = " + VRFTU( VRFTUNum).Name );
					ErrorsFound = true;
				}
			}


			// Add TU to component sets array
			SetUpCompSets( cCurrentModuleObject, VRFTU( VRFTUNum ).Name, cFanTypes( FanType_Num ), FanName, NodeID( FanInletNodeNum ), NodeID( FanOutletNodeNum ) );

			// Add cooling coil to component sets array
			if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {

				SetUpCompSets( cCurrentModuleObject, VRFTU( VRFTUNum ).Name, cAllCoilTypes( VRFTU( VRFTUNum ).DXCoolCoilType_Num ), cAlphaArgs( 12 ), NodeID( CCoilInletNodeNum ), NodeID( CCoilOutletNodeNum ) );
				//     set heating coil present flag
				SetDXCoolingCoilData( VRFTU( VRFTUNum ).CoolCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRFTU( VRFTUNum ).HeatingCoilPresent );

				//   check that curve types are present in VRF Condenser if cooling coil is present in terminal unit (can be blank)
				//   all curves are checked for correct type if a curve name is entered in the VRF condenser object. Check that the
				//   curve is present if the corresponding coil is entered in the terminal unit.
				if ( VRFTU( VRFTUNum ).VRFSysNum > 0 ) {

					if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum != AlgorithmTypeFluidTCtrl ) {

						if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).CoolingCapacity <= 0 && VRF( VRFTU( VRFTUNum ).VRFSysNum ).CoolingCapacity != AutoSize ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "...This terminal unit contains a cooling coil and rated cooling capacity is also required in the associated condenser object." );
							ShowContinueError( "...Rated Cooling Capacity must also be specified for condenser = " + cVRFTypes( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFSystemTypeNum ) + " \"" + VRF( VRFTU( VRFTUNum ).VRFSysNum ).Name + "\"." );
							ErrorsFound = true;
						}

					}

				}

			}

			// Add heating coil to component sets array
			if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {

				SetUpCompSets( cCurrentModuleObject, VRFTU( VRFTUNum ).Name, cAllCoilTypes( VRFTU( VRFTUNum ).DXHeatCoilType_Num ), cAlphaArgs( 14 ), NodeID( HCoilInletNodeNum ), NodeID( HCoilOutletNodeNum ) );
				//     set cooling coil present flag
				SetDXCoolingCoilData( VRFTU( VRFTUNum ).HeatCoilIndex, ErrorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, VRFTU( VRFTUNum ).CoolingCoilPresent );

				if ( VRFTU( VRFTUNum ).VRFSysNum > 0 ) {

					if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum != AlgorithmTypeFluidTCtrl ) {

						if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingCapacity <= 0 && VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatingCapacity != AutoSize ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "...This terminal unit contains a heating coil and rated heating capacity is also required in the associated condenser object." );
							ShowContinueError( "...Rated Heating Capacity must also be specified for condenser = " + cVRFTypes( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFSystemTypeNum ) + " \"" + VRF( VRFTU( VRFTUNum ).VRFSysNum ).Name + "\"." );
							ErrorsFound = true;
						}

						if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatCapFT == 0 ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "...This terminal unit contains a heating coil and heating performance curves are also required in the associated condenser object." );
							ShowContinueError( "...Heating Capacity Ratio Modifier Function of Low Temperature Curve must also be specified for condenser = " + cVRFTypes( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFSystemTypeNum ) + " \"" + VRF( VRFTU( VRFTUNum ).VRFSysNum ).Name + "\"." );
							ErrorsFound = true;
						}

						if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatEIRFT == 0 ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "...This terminal unit contains a heating coil and heating performance curves are also required in the associated condenser object." );
							ShowContinueError( "...Heating Energy Input Ratio Modifier Function of Low Temperature Curve must also be specified for condenser = " + cVRFTypes( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFSystemTypeNum ) + " \"" + VRF( VRFTU( VRFTUNum ).VRFSysNum ).Name + "\"." );
							ErrorsFound = true;
						}

						if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).HeatEIRFPLR1 == 0 ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "...This terminal unit contains a heating coil and heating performance curves are also required in the associated condenser object." );
							ShowContinueError( "...Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve must also be specified for condenser = " + cVRFTypes( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFSystemTypeNum ) + " \"" + VRF( VRFTU( VRFTUNum ).VRFSysNum ).Name + "\"." );
						}

					}

				}

			}

			// Set up component set for OA mixer - use OA node and Mixed air node
			if ( VRFTU( VRFTUNum ).OAMixerUsed ) SetUpCompSets( cCurrentModuleObject, VRFTU( VRFTUNum ).Name, "UNDEFINED", VRFTU( VRFTUNum ).OAMixerName, NodeID( OANodeNums( 1 ) ), NodeID( OANodeNums( 4 ) ) );

			// TU inlet node must be the same as a zone exhaust node and the OA Mixer return node
			// check that TU inlet node is a zone exhaust node.
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
					if ( VRFTU( VRFTUNum ).VRFTUInletNodeNum == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
						ZoneNodeNotFound = false;
						break;
					}
				}
			}
			if ( ZoneNodeNotFound ) {
				ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Zone terminal unit air inlet node name must be the same as a zone exhaust node name." );
				ShowContinueError( "... Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "... Zone terminal unit inlet node name = " + NodeID( VRFTU( VRFTUNum ).VRFTUInletNodeNum ) );
				ErrorsFound = true;
			}
			// check OA Mixer return node
			if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
				if ( VRFTU( VRFTUNum ).VRFTUInletNodeNum != OANodeNums( 3 ) ) {
					ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Zone terminal unit air inlet node name must be the same as the OutdoorAir:Mixer return air node name." );
					ShowContinueError( "... Zone terminal unit air inlet node name = " + NodeID( VRFTU( VRFTUNum ).VRFTUInletNodeNum ) );
					ShowContinueError( "... OutdoorAir:Mixer return air node name = " + NodeID( OANodeNums( 3 ) ) );
					ErrorsFound = true;
				}
			}
			// check that TU outlet node is a zone inlet node.
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
					if ( VRFTU( VRFTUNum ).VRFTUOutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
						ZoneNodeNotFound = false;
						break;
					}
				}
			}
			if ( ZoneNodeNotFound ) {
				ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Zone terminal unit air outlet node name must be the same as a zone inlet node name." );
				ShowContinueError( "... Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "... Zone terminal unit outlet node name = " + NodeID( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) );
				ErrorsFound = true;
			}

			// check fan inlet and outlet nodes
			if ( VRFTU( VRFTUNum ).FanPlace == BlowThru ) {
				if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
					if ( FanInletNodeNum != OANodeNums( 4 ) ) {
						ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan inlet node name must be the same" );
						ShowContinueError( "as the outside air mixers mixed air node name when blow through fan is specified and an outside air mixer is present." );
						ShowContinueError( "... Fan inlet node = " + NodeID( FanInletNodeNum ) );
						ShowContinueError( "... OA mixers mixed air node = " + NodeID( OANodeNums( 4 ) ) );
						ErrorsFound = true;
					}
				} else {
					if ( FanInletNodeNum != VRFTU( VRFTUNum ).VRFTUInletNodeNum ) {
						ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan inlet node name must be the same" );
						ShowContinueError( "as the terminal unit air inlet node name when blow through fan is specified and an outside air mixer is not present." );
						ShowContinueError( "... Fan inlet node = " + NodeID( FanInletNodeNum ) );
						ShowContinueError( "... Terminal unit air inlet node = " + NodeID( VRFTU( VRFTUNum ).VRFTUInletNodeNum ) );
						ErrorsFound = true;
					}
				}
				if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {
					if ( FanOutletNodeNum != CCoilInletNodeNum ) {
						ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan outlet node name must be the same" );
						ShowContinueError( "as the DX cooling coil air inlet node name when blow through fan is specified." );
						ShowContinueError( "... Fan outlet node = " + NodeID( FanOutletNodeNum ) );
						ShowContinueError( "... DX cooling coil air inlet node = " + NodeID( CCoilInletNodeNum ) );
						ErrorsFound = true;
					}
					if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
						if ( HCoilOutletNodeNum != VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Heating coil outlet node name must be the same" );
							ShowContinueError( "as the terminal unit air outlet node name when blow through fan is specified." );
							ShowContinueError( "... Heating coil outlet node      = " + NodeID( HCoilOutletNodeNum ) );
							ShowContinueError( "... Terminal Unit air outlet node = " + NodeID( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) );
							ErrorsFound = true;
						}
					} else {
						if ( CCoilOutletNodeNum != VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Cooling coil outlet node name must be the same" );
							ShowContinueError( "as the terminal unit air outlet node name when blow through fan is specified and no DX heating coil is present." );
							ShowContinueError( "... Cooling coil outlet node      = " + NodeID( CCoilOutletNodeNum ) );
							ShowContinueError( "... Terminal Unit air outlet node = " + NodeID( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) );
							ErrorsFound = true;
						}
					}
				} else if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
					if ( FanOutletNodeNum != HCoilInletNodeNum ) {
						ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan outlet node name must be the same" );
						ShowContinueError( "as the DX heating coil air inlet node name when blow through fan is specified and a DX cooling coil is not present." );
						ShowContinueError( "... Fan outlet node = " + NodeID( FanOutletNodeNum ) );
						ShowContinueError( "... DX heating coil air inlet node = " + NodeID( HCoilInletNodeNum ) );
						ErrorsFound = true;
					}
					if ( HCoilOutletNodeNum != VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) {
						ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Heating coil outlet node name must be the same" );
						ShowContinueError( "as the terminal unit air outlet node name when blow through fan is specified." );
						ShowContinueError( "... Heating coil outlet node      = " + NodeID( HCoilOutletNodeNum ) );
						ShowContinueError( "... Terminal Unit air outlet node = " + NodeID( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) );
						ErrorsFound = true;
					}
				}
			} else if ( VRFTU( VRFTUNum ).FanPlace == DrawThru ) {
				if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {
					if ( !VRFTU( VRFTUNum ).OAMixerUsed ) {
						if ( VRFTU( VRFTUNum ).VRFTUInletNodeNum != CCoilInletNodeNum ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Cooling coil inlet node name must be the same" );
							ShowContinueError( "as the terminal unit air inlet node name when draw through fan is specified." );
							ShowContinueError( "... Terminal unit air inlet node = " + NodeID( VRFTU( VRFTUNum ).VRFTUInletNodeNum ) );
							ShowContinueError( "... DX cooling coil air inlet node = " + NodeID( CCoilInletNodeNum ) );
							ErrorsFound = true;
						}
					}
					if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
						if ( FanInletNodeNum != HCoilOutletNodeNum ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan inlet node name must be the same" );
							ShowContinueError( "as the DX heating coil air outlet node name when draw through fan is specifiedt." );
							ShowContinueError( "... Fan inlet node = " + NodeID( FanInletNodeNum ) );
							ShowContinueError( "... DX heating coil air outlet node = " + NodeID( HCoilOutletNodeNum ) );
							ErrorsFound = true;
						}
					} else {
						if ( FanInletNodeNum != CCoilOutletNodeNum ) {
							ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan inlet node name must be the same" );
							ShowContinueError( "as the DX cooling coil air outlet node name when draw through fan is specified and a DX heating coil is not present." );
							ShowContinueError( "... Fan inlet node = " + NodeID( FanInletNodeNum ) );
							ShowContinueError( "... DX cooling coil air outlet node = " + NodeID( CCoilOutletNodeNum ) );
							ErrorsFound = true;
						}
					}
				} else if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
					if ( FanInletNodeNum != HCoilOutletNodeNum ) {
						ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan inlet node name must be the same" );
						ShowContinueError( "as the DX heating coil air outlet node name when draw through fan is specified." );
						ShowContinueError( "... Fan inlet node = " + NodeID( FanInletNodeNum ) );
						ShowContinueError( "... DX heating coil air outlet node = " + NodeID( HCoilOutletNodeNum ) );
						ErrorsFound = true;
					}
				}
				if ( FanOutletNodeNum != VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) {
					ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" Fan outlet node name must be the same" );
					ShowContinueError( "as the terminal unit air outlet node name when draw through fan is specified." );
					ShowContinueError( "... Fan outlet node = " + NodeID( FanOutletNodeNum ) );
					ShowContinueError( "... Terminal unit air outlet node = " + NodeID( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ) );
					ErrorsFound = true;
				}
			}
			if ( VRFTU( VRFTUNum ).CoolingCoilPresent && VRFTU( VRFTUNum ).HeatingCoilPresent ) {
				if ( CCoilOutletNodeNum != HCoilInletNodeNum ) {
					ShowSevereError( cCurrentModuleObject + " \"" + VRFTU( VRFTUNum ).Name + "\" DX cooling coil air outlet node name must be the same" );
					ShowContinueError( " as the DX heating coil air inlet node name." );
					ShowContinueError( "... DX cooling coil air outlet node = " + NodeID( CCoilOutletNodeNum ) );
					ShowContinueError( "... DX heating coil air inlet node  = " + NodeID( HCoilInletNodeNum ) );
					ErrorsFound = true;
				}
			}

		} // end Number of VRF Terminal Unit Loop

		//   perform additional error checking
		for ( NumList = 1; NumList <= NumVRFTULists; ++NumList ) {
			for ( VRFNum = 1; VRFNum <= TerminalUnitList( NumList ).NumTUInList; ++VRFNum ) {
				if ( TerminalUnitList( NumList ).ZoneTUPtr( VRFNum ) > 0 ) continue;
				// TU name in zone terminal unit list not found
				ShowSevereError( "ZoneTerminalUnitList \"" + TerminalUnitList( NumList ).Name + "\"" );
				ShowContinueError( "...Zone Terminal Unit = " + TerminalUnitList( NumList ).ZoneTUName( VRFNum ) + " improperly connected to system." );
				ShowContinueError( "...either the ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object does not exist," );
				ShowContinueError( "...the ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object name is mispelled," );
				ShowContinueError( "...or the ZoneTerminalUnitList object is not named in an AirConditioner:VariableRefrigerantFlow object." );
				ErrorsFound = true;
			}
		}

		//   warn when number of ZoneTerminalUnitList different from number of AirConditioner:VariableRefrigerantFlow
		if ( NumVRFTULists != NumVRFCond ) {
			ShowSevereError( "The number of AirConditioner:VariableRefrigerantFlow objects (" + TrimSigDigits( NumVRFCond ) + ") does not match the number of ZoneTerminalUnitList objects (" + TrimSigDigits( NumVRFTULists ) + ")." );
			for ( NumCond = 1; NumCond <= NumVRFCond; ++NumCond ) {
				if ( VRF( NumCond ).ZoneTUListPtr > 0 ){
					ShowContinueError( "...AirConditioner:VariableRefrigerantFlow = " + VRF( NumCond ).Name + " specifies Zone Terminal Unit List Name = " + TerminalUnitList( VRF( NumCond ).ZoneTUListPtr ).Name );
				} else {
					ShowContinueError( "...AirConditioner:VariableRefrigerantFlow = " + VRF( NumCond ).Name + " Zone Terminal Unit List Name not found." );
				}
			}
			ShowContinueError( "...listing ZoneTerminalUnitList objects." );
			for ( NumList = 1; NumList <= NumVRFTULists; ++NumList ) {
				ShowContinueError( "...ZoneTerminalUnitList = " + TerminalUnitList( NumList ).Name );
			}
			ErrorsFound = true;
		}

		// Set up output variables
		for ( VRFNum = 1; VRFNum <= NumVRFTU; ++VRFNum ) {
			if ( VRFTU( VRFNum ).CoolingCoilPresent ) {
				SetupOutputVariable( "Zone VRF Air Terminal Cooling Electric Power [W]", VRFTU( VRFNum ).ParasiticCoolElecPower, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Cooling Electric Energy [J]", VRFTU( VRFNum ).ParasiticElecCoolConsumption, "System", "Sum", VRFTU( VRFNum ).Name, _, "Electric", "COOLING", _, "System" );
				SetupOutputVariable( "Zone VRF Air Terminal Total Cooling Rate [W]", VRFTU( VRFNum ).TotalCoolingRate, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Sensible Cooling Rate [W]", VRFTU( VRFNum ).SensibleCoolingRate, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Latent Cooling Rate [W]", VRFTU( VRFNum ).LatentCoolingRate, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Total Cooling Energy [J]", VRFTU( VRFNum ).TotalCoolingEnergy, "System", "Sum", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Sensible Cooling Energy [J]", VRFTU( VRFNum ).SensibleCoolingEnergy, "System", "Sum", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Latent Cooling Energy [J]", VRFTU( VRFNum ).LatentCoolingEnergy, "System", "Sum", VRFTU( VRFNum ).Name );
			}
			if ( VRFTU( VRFNum ).HeatingCoilPresent ) {
				SetupOutputVariable( "Zone VRF Air Terminal Heating Electric Power [W]", VRFTU( VRFNum ).ParasiticHeatElecPower, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Heating Electric Energy [J]", VRFTU( VRFNum ).ParasiticElecHeatConsumption, "System", "Sum", VRFTU( VRFNum ).Name, _, "Electric", "HEATING", _, "System" );
				SetupOutputVariable( "Zone VRF Air Terminal Total Heating Rate [W]", VRFTU( VRFNum ).TotalHeatingRate, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Sensible Heating Rate [W]", VRFTU( VRFNum ).SensibleHeatingRate, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Latent Heating Rate [W]", VRFTU( VRFNum ).LatentHeatingRate, "System", "Average", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Total Heating Energy [J]", VRFTU( VRFNum ).TotalHeatingEnergy, "System", "Sum", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Sensible Heating Energy [J]", VRFTU( VRFNum ).SensibleHeatingEnergy, "System", "Sum", VRFTU( VRFNum ).Name );
				SetupOutputVariable( "Zone VRF Air Terminal Latent Heating Energy [J]", VRFTU( VRFNum ).LatentHeatingEnergy, "System", "Sum", VRFTU( VRFNum ).Name );
			}
			SetupOutputVariable( "Zone VRF Air Terminal Fan Availability Status []", VRFTU( VRFNum ).AvailStatus, "System", "Average", VRFTU( VRFNum ).Name );
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "Variable Refrigerant Flow Terminal Unit", VRFTU( VRFNum ).Name, "Part Load Ratio", "[fraction]", VRFTU( VRFNum ).EMSOverridePartLoadFrac, VRFTU( VRFNum ).EMSValueForPartLoadFrac );
			}
		}

		for ( NumCond = 1; NumCond <= NumVRFCond; ++NumCond ) {
			SetupOutputVariable( "VRF Heat Pump Total Cooling Rate [W]", VRF( NumCond ).TotalCoolingCapacity, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Total Heating Rate [W]", VRF( NumCond ).TotalHeatingCapacity, "System", "Average", VRF( NumCond ).Name );
			if ( VRF( NumCond ).FuelType == FuelTypeElectric ) {
				SetupOutputVariable( "VRF Heat Pump Cooling Electric Power [W]", VRF( NumCond ).ElecCoolingPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Cooling Electric Energy [J]", VRF( NumCond ).CoolElecConsumption, "System", "Sum", VRF( NumCond ).Name, _, cValidFuelTypes( VRF( NumCond ).FuelType ), "COOLING", _, "System" );
			} else {
				SetupOutputVariable( "VRF Heat Pump Cooling " + cValidFuelTypes( VRF( NumCond ).FuelType ) + " Rate [W]", VRF( NumCond ).ElecCoolingPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Cooling " + cValidFuelTypes( VRF( NumCond ).FuelType ) + " Energy [J]", VRF( NumCond ).CoolElecConsumption, "System", "Sum", VRF( NumCond ).Name, _, cValidFuelTypes( VRF( NumCond ).FuelType ), "COOLING", _, "System" );
			}
			if ( VRF( NumCond ).FuelType == FuelTypeElectric ) {
				SetupOutputVariable( "VRF Heat Pump Heating Electric Power [W]", VRF( NumCond ).ElecHeatingPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Heating Electric Energy [J]", VRF( NumCond ).HeatElecConsumption, "System", "Sum", VRF( NumCond ).Name, _, cValidFuelTypes( VRF( NumCond ).FuelType ), "HEATING", _, "System" );
			} else {
				SetupOutputVariable( "VRF Heat Pump Heating " + cValidFuelTypes( VRF( NumCond ).FuelType ) + " Rate [W]", VRF( NumCond ).ElecHeatingPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Heating " + cValidFuelTypes( VRF( NumCond ).FuelType ) + " Energy [J]", VRF( NumCond ).HeatElecConsumption, "System", "Sum", VRF( NumCond ).Name, _, cValidFuelTypes( VRF( NumCond ).FuelType ), "HEATING", _, "System" );
			}

			SetupOutputVariable( "VRF Heat Pump Cooling COP []", VRF( NumCond ).OperatingCoolingCOP, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Heating COP []", VRF( NumCond ).OperatingHeatingCOP, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump COP []", VRF( NumCond ).OperatingCOP, "System", "Average", VRF( NumCond ).Name );

			if( VRF( NumCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ){
			// For VRF_FluidTCtrl Model
				SetupOutputVariable( "VRF Heat Pump Indoor Unit Evaporating Temperature at Cooling Mode [C]", VRF( NumCond ).IUEvaporatingTemp, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Outdoor Unit Condensing Temperature at Cooling Mode [C]", VRF( NumCond ).CondensingTemp, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Indoor Unit Condensing Temperature at Heating Mode [C]", VRF( NumCond ).IUCondensingTemp, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Outdoor Unit Evaporating Temperature at Heating Mode [C]", VRF( NumCond ).EvaporatingTemp, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Compressor Electric Power at Cooling Mode [W]", VRF( NumCond ).NcompCooling, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Compressor Electric Power at Heating Mode [W]", VRF( NumCond ).NcompHeating, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Compressor Rotating Speed [rev/min]", VRF( NumCond ).CompActSpeed, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Outdoor Unit Fan Power [W]", VRF( NumCond ).CondFanPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Maximum Capacity Cooling Rate [W]", VRF( NumCond ).CoolingCapacity, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Maximum Capacity Heating Rate [W]", VRF( NumCond ).HeatingCapacity, "System", "Average", VRF( NumCond ).Name );
			} else {
			// For VRF_SysCurve Model
				SetupOutputVariable( "VRF Heat Pump Maximum Capacity Cooling Rate [W]", MaxCoolingCapacity( NumCond ), "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Maximum Capacity Heating Rate [W]", MaxHeatingCapacity( NumCond ), "System", "Average", VRF( NumCond ).Name );
			}

			if ( VRF( NumCond ).DefrostStrategy == Resistive || ( VRF( NumCond ).DefrostStrategy == ReverseCycle && VRF( NumCond ).FuelType == FuelTypeElectric ) ) {
				SetupOutputVariable( "VRF Heat Pump Defrost Electric Power [W]", VRF( NumCond ).DefrostPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Defrost Electric Energy [J]", VRF( NumCond ).DefrostConsumption, "System", "Sum", VRF( NumCond ).Name, _, "Electric", "HEATING", _, "System" );
			} else { // defrost energy appied to fuel type
				SetupOutputVariable( "VRF Heat Pump Defrost " + cValidFuelTypes( VRF( NumCond ).FuelType ) + " Rate [W]", VRF( NumCond ).DefrostPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Defrost " + cValidFuelTypes( VRF( NumCond ).FuelType ) + " Energy [J]", VRF( NumCond ).DefrostConsumption, "System", "Sum", VRF( NumCond ).Name, _, cValidFuelTypes( VRF( NumCond ).FuelType ), "HEATING", _, "System" );
			}

			SetupOutputVariable( "VRF Heat Pump Part Load Ratio []", VRF( NumCond ).VRFCondPLR, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Runtime Fraction []", VRF( NumCond ).VRFCondRTF, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Cycling Ratio []", VRF( NumCond ).VRFCondCyclingRatio, "System", "Average", VRF( NumCond ).Name );

			SetupOutputVariable( "VRF Heat Pump Operating Mode []", VRF( NumCond ).OperatingMode, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Condenser Inlet Temperature [C]", VRF( NumCond ).CondenserInletTemp, "System", "Average", VRF( NumCond ).Name );

			SetupOutputVariable( "VRF Heat Pump Crankcase Heater Electric Power [W]", VRF( NumCond ).CrankCaseHeaterPower, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Crankcase Heater Electric Energy [J]", VRF( NumCond ).CrankCaseHeaterElecConsumption, "System", "Sum", VRF( NumCond ).Name, _, "Electric", "COOLING", _, "System" );
			SetupOutputVariable( "VRF Heat Pump Terminal Unit Cooling Load Rate [W]", VRF( NumCond ).TUCoolingLoad, "System", "Average", VRF( NumCond ).Name );
			SetupOutputVariable( "VRF Heat Pump Terminal Unit Heating Load Rate [W]", VRF( NumCond ).TUHeatingLoad, "System", "Average", VRF( NumCond ).Name );
			if ( VRF( NumCond ).HeatRecoveryUsed ) {
				SetupOutputVariable( "VRF Heat Pump Heat Recovery Status Change Multiplier []", VRF( NumCond ).SUMultiplier, "System", "Average", VRF( NumCond ).Name );
			}

			if ( VRF( NumCond ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "VRF Heat Pump Evaporative Condenser Water Use Volume [m3]", VRF( NumCond ).EvapWaterConsumpRate, "System", "Sum", VRF( NumCond ).Name, _, "Water", "Cooling", _, "System" );
				SetupOutputVariable( "VRF Heat Pump Evaporative Condenser Pump Electric Power [W]", VRF( NumCond ).EvapCondPumpElecPower, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Evaporative Condenser Pump Electric Energy [J]", VRF( NumCond ).EvapCondPumpElecConsumption, "System", "Sum", VRF( NumCond ).Name, _, "Electric", "COOLING", _, "System" );

				if ( VRF( NumCond ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "VRF Heat Pump Basin Heater Electric Power [W]", VRF( NumCond ).BasinHeaterPower, "System", "Average", VRF( NumCond ).Name );
					SetupOutputVariable( "VRF Heat Pump Basin Heater Electric Energy [J]", VRF( NumCond ).BasinHeaterConsumption, "System", "Sum", VRF( NumCond ).Name, _, "Electric", "COOLING", _, "System" );
				}

			} else if ( VRF( NumCond ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "VRF Heat Pump Condenser Outlet Temperature [C]", VRF( NumCond ).CondenserSideOutletTemp, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Condenser Mass Flow Rate [kg/s]", VRF( NumCond ).WaterCondenserMassFlow, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Condenser Heat Transfer Rate [W]", VRF( NumCond ).QCondenser, "System", "Average", VRF( NumCond ).Name );
				SetupOutputVariable( "VRF Heat Pump Condenser Heat Transfer Energy [J]", VRF( NumCond ).QCondEnergy, "System", "Sum", VRF( NumCond ).Name );
			}

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "Variable Refrigerant Flow Heat Pump", VRF( NumCond ).Name, "Operating Mode", "[integer]", VRF( NumCond ).EMSOverrideHPOperatingMode, VRF( NumCond ).EMSValueForHPOperatingMode );
			}

		}

	}

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitVRF(
		int const VRFTUNum,
		int const ZoneNum,
		bool const FirstHVACIteration,
		Real64 & OnOffAirFlowRatio,
		Real64 & QZnReq
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the VRF Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::VRFTerminalUnit_Num;
		using DataHeatBalFanSys::TempControlType;
		using DataHeatBalFanSys::ZT;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using InputProcessor::SameString;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataEnvironment::StdRhoAir;
		using DataEnvironment::OutDryBulbTemp;
		using MixedAir::SimOAMixer;
		using MixedAir::SimOAController;
		using DataZoneEquipment::ZoneEquipList;
		using DataSizing::AutoSize;
		using Fans::GetFanVolFlow;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitVRF" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // TU inlet node
		int OutNode; // TU outlet node
		int OutsideAirNode; // TU mixer outside air inlet node
		int NumTULoop; // loop counter, number of TU's in list
		int ELLoop; // loop counter, number of zone equipment lists
		int ListLoop; // loop counter, number of equipment is each list
		int VRFCond; // index to VRF condenser
		int TUIndex; // index to TU
		int TUListNum; // index to VRF AC system terminal unit list
		int TUListIndex; // pointer to TU list for this VRF system
		int IndexToTUInTUList; // index to TU in TerminalUnilList
		Real64 RhoAir; // air density at InNode
		static Real64 CurrentEndTime; // end time of current time step
		static Real64 CurrentEndTimeLast; // end time of last time step
		static Real64 TimeStepSysLast; // system time step on last time step
		Real64 TempOutput; // Sensible output of TU
		Real64 LoadToCoolingSP; // thermostat load to cooling setpoint (W)
		Real64 LoadToHeatingSP; // thermostat load to heating setpoint (W)
		bool EnableSystem; // use to turn on secondary operating mode if OA temp limits exceeded
		Real64 rho; // density of water (kg/m3)
		Real64 OutsideDryBulbTemp; // Outdoor air temperature at external node height

		// FLOW:

		// ALLOCATE and Initialize subroutine variables
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumVRFTU );
			MySizeFlag.allocate( NumVRFTU );
			MyVRFFlag.allocate( NumVRFTU );
			MyZoneEqFlag.allocate ( NumVRFTU );
			MyBeginTimeStepFlag.allocate( NumVRFCond );
			MaxDeltaT.allocate( NumVRFCond );
			MinDeltaT.allocate( NumVRFCond );
			LastModeCooling.allocate( NumVRFCond );
			LastModeHeating.allocate( NumVRFCond );
			HeatingLoad.allocate( NumVRFCond );
			CoolingLoad.allocate( NumVRFCond );
			NumCoolingLoads.allocate( NumVRFCond );
			SumCoolingLoads.allocate( NumVRFCond );
			NumHeatingLoads.allocate( NumVRFCond );
			SumHeatingLoads.allocate( NumVRFCond );
			MyVRFCondFlag.allocate( NumVRFCond );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyVRFFlag = true;
			MyZoneEqFlag = true;
			MyBeginTimeStepFlag = true;
			MaxDeltaT = 0.0;
			MinDeltaT = 0.0;
			LastModeCooling = false;
			LastModeHeating = true;
			NumCoolingLoads = 0;
			SumCoolingLoads = 0.0;
			NumHeatingLoads = 0;
			SumHeatingLoads = 0.0;

			MyOneTimeFlag = false;
			MyVRFCondFlag = true;

		} // IF (MyOneTimeFlag) THEN

		// identify VRF condenser connected to this TU
		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		InNode = VRFTU( VRFTUNum ).VRFTUInletNodeNum;
		OutNode = VRFTU( VRFTUNum ).VRFTUOutletNodeNum;
		OutsideAirNode = VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;

		// set condenser inlet temp, used as surrogate for OAT (used to check limits of operation)
		if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
			OutsideDryBulbTemp = Node( VRF( VRFCond ).CondenserNodeNum ).Temp;
		} else {
			if ( OutsideAirNode == 0 ) {
				OutsideDryBulbTemp = OutDryBulbTemp;
			} else {
				OutsideDryBulbTemp = Node( OutsideAirNode ).Temp;
			}
		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( VRFTUNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( VRFTerminalUnit_Num ).ZoneCompAvailMgrs( VRFTUNum ).AvailManagerListName = VRFTU( VRFTUNum ).AvailManagerListName;
				ZoneComp( VRFTerminalUnit_Num ).ZoneCompAvailMgrs( VRFTUNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( VRFTUNum ) = false;
			}
			VRFTU( VRFTUNum ).AvailStatus = ZoneComp( VRFTerminalUnit_Num ).ZoneCompAvailMgrs( VRFTUNum ).AvailStatus;
		}

		// If all VRF Terminal Units on this VRF AC System have been simulated, reset the IsSimulated flag
		// The condenser will be simulated after all terminal units have been simulated (see Sub SimulateVRF)
		if ( all( TerminalUnitList( TUListIndex ).IsSimulated ) ) {
			//   this should be the first time through on the next iteration. All TU's and condenser have been simulated.
			//   reset simulation flag for each terminal unit
			TerminalUnitList( TUListIndex ).IsSimulated = false;
			//     after all TU's have been simulated, reset operating mode flag if necessary
			if ( LastModeHeating( VRFCond ) && CoolingLoad( VRFCond ) ) {
				LastModeCooling( VRFCond ) = true;
				LastModeHeating( VRFCond ) = false;
				//        SwitchedMode(VRFCond)    = .TRUE.
			}
			if ( LastModeCooling( VRFCond ) && HeatingLoad( VRFCond ) ) {
				LastModeHeating( VRFCond ) = true;
				LastModeCooling( VRFCond ) = false;
				//        SwitchedMode(VRFCond)    = .TRUE.
			}
		} // IF(ALL(TerminalUnitList(VRFTU(VRFTUNum)%TUListIndex)%IsSimulated))THEN

		// one-time check to see if VRF TU's are on Zone Equipment List or issue warning
		if ( ZoneEquipmentListNotChecked ) {
			if ( ZoneEquipInputsFilled ) {
				ZoneEquipmentListNotChecked = false;
				for ( TUListNum = 1; TUListNum <= NumVRFTULists; ++TUListNum ) {
					for ( NumTULoop = 1; NumTULoop <= TerminalUnitList( TUListNum ).NumTUInList; ++NumTULoop ) {
						TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTULoop );
						for ( ELLoop = 1; ELLoop <= NumOfZones; ++ELLoop ) { // NumOfZoneEquipLists
							if ( ZoneEquipList( ELLoop ).Name == "" ) continue; // dimensioned by NumOfZones.  Only valid ones have names.
							for ( ListLoop = 1; ListLoop <= ZoneEquipList( ELLoop ).NumOfEquipTypes; ++ListLoop ) {
								if ( ! SameString( ZoneEquipList( ELLoop ).EquipType( ListLoop ), cVRFTUTypes( VRFTU( TUIndex ).VRFTUType_Num ) ) ) continue;
								if ( ! SameString( ZoneEquipList( ELLoop ).EquipName( ListLoop ), VRFTU( TUIndex ).Name ) ) continue;
								VRFTU( TUIndex ).ZoneNum = ELLoop;
								if ( VRF( VRFTU( TUIndex ).VRFSysNum ).MasterZonePtr == ELLoop ) {
									VRF( VRFTU( TUIndex ).VRFSysNum ).MasterZoneTUIndex = TUIndex;
								}
								goto EquipList_exit;
							}
						}
						EquipList_exit: ;
					}

					if ( CheckZoneEquipmentList( cVRFTUTypes( VRFTU( TUIndex ).VRFTUType_Num ), VRFTU( TUIndex ).Name ) ) continue;
					ShowSevereError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( TUIndex ).VRFTUType_Num ) + ',' + VRFTU( TUIndex ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
					ShowContinueError( "...The VRF AC System associated with this terminal unit may also not be simulated." );
				}
			} // IF(ZoneEquipInputsFilled) THEN
		} // IF(ZoneEquipmentListNotChecked)THEN

		// Size TU
		if ( MySizeFlag( VRFTUNum ) ) {
			if ( ! SysSizingCalc ) {
				SizeVRF( VRFTUNum );
				TerminalUnitList( TUListIndex ).TerminalUnitNotSizedYet( IndexToTUInTUList ) = false;
				MySizeFlag( VRFTUNum ) = false;
			} // IF ( .NOT. SysSizingCalc) THEN
		} // IF (MySizeFlag(VRFTUNum)) THEN

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( VRFTUNum ) ) {

			//Change the Volume Flow Rates to Mass Flow Rates

			RhoAir = StdRhoAir;
			// set the mass flow rates from the input volume flow rates
			VRFTU( VRFTUNum ).MaxCoolAirMassFlow = RhoAir * VRFTU( VRFTUNum ).MaxCoolAirVolFlow;
			VRFTU( VRFTUNum ).CoolOutAirMassFlow = RhoAir * VRFTU( VRFTUNum ).CoolOutAirVolFlow;
			VRFTU( VRFTUNum ).MaxHeatAirMassFlow = RhoAir * VRFTU( VRFTUNum ).MaxHeatAirVolFlow;
			VRFTU( VRFTUNum ).HeatOutAirMassFlow = RhoAir * VRFTU( VRFTUNum ).HeatOutAirVolFlow;
			VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow = RhoAir * VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow;
			VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow = RhoAir * VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow;
			VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow = RhoAir * VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow;
			// set the node max and min mass flow rates
			// outside air mixer is optional, check that node num > 0
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRateMax = max( VRFTU( VRFTUNum ).CoolOutAirMassFlow, VRFTU( VRFTUNum ).HeatOutAirMassFlow );
				Node( OutsideAirNode ).MassFlowRateMin = 0.0;
				Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			}
			Node( OutNode ).MassFlowRateMax = max( VRFTU( VRFTUNum ).MaxCoolAirMassFlow, VRFTU( VRFTUNum ).MaxHeatAirMassFlow );
			Node( OutNode ).MassFlowRateMin = 0.0;
			Node( OutNode ).MassFlowRateMinAvail = 0.0;
			Node( InNode ).MassFlowRateMax = max( VRFTU( VRFTUNum ).MaxCoolAirMassFlow, VRFTU( VRFTUNum ).MaxHeatAirMassFlow );
			Node( InNode ).MassFlowRateMin = 0.0;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
			if ( VRFTU( VRFTUNum ).VRFTUOAMixerRelNodeNum > 0 ) {
				Node( VRFTU( VRFTUNum ).VRFTUOAMixerRelNodeNum ).MassFlowRateMinAvail = 0.0;
			}

			MyEnvrnFlag( VRFTUNum ) = false;

			if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
				rho = GetDensityGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
				VRF( VRFCond ).WaterCondenserDesignMassFlow = VRF( VRFCond ).WaterCondVolFlowRate * rho;

				InitComponentNodes( 0.0, VRF( VRFCond ).WaterCondenserDesignMassFlow, VRF( VRFCond ).CondenserNodeNum, VRF( VRFCond ).CondenserOutletNodeNum, VRF( VRFCond ).SourceLoopNum, VRF( VRFCond ).SourceLoopSideNum, VRF( VRFCond ).SourceBranchNum, VRF( VRFCond ).SourceCompNum );
			}
			//    IF(MyVRFCondFlag(VRFCond))THEN
			VRF( VRFCond ).HRTimer = 0.0;
			VRF( VRFCond ).ModeChange = false;
			VRF( VRFCond ).HRModeChange = false;
			MyVRFCondFlag( VRFCond ) = false;
			//    END IF
		} // IF (BeginEnvrnFlag .and. MyEnvrnFlag(VRFTUNum)) THEN

		// reset environment flag for next environment
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( VRFTUNum ) = true;
			MyVRFCondFlag( VRFCond ) = true;
		}

		// one-time checks of flow rate vs fan flow rate
		if ( MyVRFFlag( VRFTUNum ) ) {
			if ( ! SysSizingCalc ) {
				if ( VRFTU( VRFTUNum ).ActualFanVolFlowRate != AutoSize ) {

					if ( VRFTU( VRFTUNum ).MaxCoolAirVolFlow > VRFTU( VRFTUNum ).ActualFanVolFlowRate ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "... has Supply Air Flow Rate During Cooling Operation > Max Fan Volume Flow Rate, should be <=" );
						ShowContinueError( "... Supply Air Flow Rate During Cooling Operation = " + RoundSigDigits( VRFTU( VRFTUNum ).MaxCoolAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Max Fan Volume Flow Rate                      = " + RoundSigDigits( VRFTU( VRFTUNum ).ActualFanVolFlowRate, 4 ) + " m3/s" );
						ShowContinueError( "...the supply air flow rate during cooling operation will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).MaxCoolAirVolFlow = VRFTU( VRFTUNum ).ActualFanVolFlowRate;
					}

					if ( VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow > VRFTU( VRFTUNum ).ActualFanVolFlowRate ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "... has Supply Air Flow Rate When No Cooling is Needed > Max Fan Volume Flow Rate, should be <=" );
						ShowContinueError( "... Supply Air Flow Rate When No Cooling is Needed = " + RoundSigDigits( VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Max Fan Volume Flow Rate                       = " + RoundSigDigits( VRFTU( VRFTUNum ).ActualFanVolFlowRate, 4 ) + " m3/s" );
						ShowContinueError( "...the supply air flow rate when no cooling is needed will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow = VRFTU( VRFTUNum ).ActualFanVolFlowRate;
					}

					if ( VRFTU( VRFTUNum ).CoolOutAirVolFlow > VRFTU( VRFTUNum ).MaxCoolAirVolFlow ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "...The Outdoor Air Flow Rate During Cooling Operation exceeds the Supply Air Flow Rate During Cooling Operation." );
						ShowContinueError( "...Outdoor Air Flow Rate During Cooling Operation = " + RoundSigDigits( VRFTU( VRFTUNum ).CoolOutAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Supply Air Flow Rate During Cooling Operation = " + RoundSigDigits( VRFTU( VRFTUNum ).MaxCoolAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "...the outdoor air flow rate will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).CoolOutAirVolFlow = VRFTU( VRFTUNum ).MaxCoolAirVolFlow;
					}

					if ( VRFTU( VRFTUNum ).MaxHeatAirVolFlow > VRFTU( VRFTUNum ).ActualFanVolFlowRate ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "... has Supply Air Flow Rate During Heating Operation > Max Fan Volume Flow Rate, should be <=" );
						ShowContinueError( "... Supply Air Flow Rate During Heating Operation = " + RoundSigDigits( VRFTU( VRFTUNum ).MaxHeatAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Max Fan Volume Flow Rate                      = " + RoundSigDigits( VRFTU( VRFTUNum ).ActualFanVolFlowRate, 4 ) + " m3/s" );
						ShowContinueError( "...the supply air flow rate during cooling operation will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).MaxHeatAirVolFlow = VRFTU( VRFTUNum ).ActualFanVolFlowRate;
					}

					if ( VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow > VRFTU( VRFTUNum ).ActualFanVolFlowRate ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "... has Supply Air Flow Rate When No Heating is Needed > Max Fan Volume Flow Rate, should be <=" );
						ShowContinueError( "... Supply Air Flow Rate When No Heating is Needed = " + RoundSigDigits( VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Max Fan Volume Flow Rate                       = " + RoundSigDigits( VRFTU( VRFTUNum ).ActualFanVolFlowRate, 4 ) + " m3/s" );
						ShowContinueError( "...the supply air flow rate when no cooling is needed will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow = VRFTU( VRFTUNum ).ActualFanVolFlowRate;
					}

					if ( VRFTU( VRFTUNum ).HeatOutAirVolFlow > VRFTU( VRFTUNum ).MaxHeatAirVolFlow ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "...The Outdoor Air Flow Rate During Heating Operation exceeds the Supply Air Flow Rate During Heating Operation." );
						ShowContinueError( "...Outdoor Air Flow Rate During Heating Operation = " + RoundSigDigits( VRFTU( VRFTUNum ).HeatOutAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Supply Air Flow Rate During Heating Operation = " + RoundSigDigits( VRFTU( VRFTUNum ).MaxHeatAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "...the outdoor air flow rate will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).HeatOutAirVolFlow = VRFTU( VRFTUNum ).MaxHeatAirVolFlow;
					}

					if ( VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow > VRFTU( VRFTUNum ).ActualFanVolFlowRate ) {
						ShowWarningError( "InitVRF: VRF Terminal Unit = [" + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ", \"" + VRFTU( VRFTUNum ).Name + "\"]" );
						ShowContinueError( "... has a Outdoor Air Flow Rate When No Cooling or Heating is Needed > Max Fan Volume Flow Rate, should be <=" );
						ShowContinueError( "... Outdoor Air Flow Rate When No Cooling or Heating is Needed = " + RoundSigDigits( VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow, 4 ) + " m3/s" );
						ShowContinueError( "... Max Fan Volume Flow Rate                                   = " + RoundSigDigits( VRFTU( VRFTUNum ).ActualFanVolFlowRate, 4 ) + " m3/s" );
						ShowContinueError( "...the outdoor air flow rate when no cooling or heating is needed will be reduced to match and the simulation continues." );
						VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow = VRFTU( VRFTUNum ).ActualFanVolFlowRate;
					}

					if ( VRFTU( VRFTUNum ).ActualFanVolFlowRate > 0.0 ) {
						VRFTU( VRFTUNum ).HeatingSpeedRatio = VRFTU( VRFTUNum ).MaxHeatAirVolFlow / VRFTU( VRFTUNum ).ActualFanVolFlowRate;
						VRFTU( VRFTUNum ).CoolingSpeedRatio = VRFTU( VRFTUNum ).MaxCoolAirVolFlow / VRFTU( VRFTUNum ).ActualFanVolFlowRate;
					}

					MyVRFFlag( VRFTUNum ) = false;
				} else {
					GetFanVolFlow( VRFTU( VRFTUNum ).FanIndex, VRFTU( VRFTUNum ).ActualFanVolFlowRate );
				}
			}
		} // IF(MyVRFFlag(VRFTUNum))THEN

		// calculate end time of current time step to determine if max capacity reset is required
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		// Initialize the maximum allowed terminal unit capacity. Total terminal unit capacity must not
		// exceed the available condenser capacity. This variable is used to limit the terminal units
		// providing more capacity than allowed. Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected
		// to a condenser having only 9-tons available. This variable will be set to 3-tons and the 4-ton
		// terminal unit will be limited to 3-tons (see SimVRFCondenser where this variable is calculated).
		if ( CurrentEndTime > CurrentEndTimeLast || TimeStepSysLast > TimeStepSys || ( FirstHVACIteration && MyBeginTimeStepFlag( VRFCond ) ) ) {
			MaxCoolingCapacity( VRFCond ) = MaxCap;
			MaxHeatingCapacity( VRFCond ) = MaxCap;
			MyBeginTimeStepFlag( VRFCond ) = false;
		}

		if ( ! FirstHVACIteration ) MyBeginTimeStepFlag( VRFCond ) = true;

		// Do the following initializations (every time step).

		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//  TUListNum = VRFTU(VRFTUNum)%TUListIndex

		if ( VRFTU( VRFTUNum ).FanOpModeSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( VRFTU( VRFTUNum ).FanOpModeSchedPtr ) == 0.0 ) {
				VRFTU( VRFTUNum ).OpMode = CycFanCycCoil;
			} else {
				VRFTU( VRFTUNum ).OpMode = ContFanCycCoil;
			}
		}

		// if condenser is off, all terminal unit coils are off
		//!!LKL Discrepancy < 0
		if ( GetCurrentScheduleValue( VRF( VRFCond ).SchedPtr ) == 0.0 ) {
			HeatingLoad( VRFCond ) = false;
			CoolingLoad( VRFCond ) = false;
		} else {

			//*** Operating Mode Initialization done at beginning of each iteration ***!
			//*** assumes all TU's and Condeser were simulated last iteration ***!
			//*** this code is done ONCE each iteration when all TU's IsSimulated flag is FALSE ***!
			// Determine operating mode prior to simulating any terminal units connected to a VRF condenser
			// this should happen at the beginning of a time step where all TU's are polled to see what
			// mode the heat pump condenser will operate in
			if ( ! any( TerminalUnitList( TUListIndex ).IsSimulated ) ) {
				InitializeOperatingMode( FirstHVACIteration, VRFCond, TUListIndex, OnOffAirFlowRatio );
			}
			//*** End of Operating Mode Initialization done at beginning of each iteration ***!

			//if ( ! any( TerminalUnitList( TUListIndex ).IsSimulated ) && VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
			//	CalcVRFIUTeTc_FluidTCtrl( VRFCond ); // Get the VRF IU Te/Tc for the timestep
			//}

			// disable VRF system when outside limits of operation based on OAT
			EnableSystem = false; // flag used to switch operating modes when OAT is outside operating limits
			if ( CoolingLoad( VRFCond ) ) {
				if ( ( OutsideDryBulbTemp < VRF( VRFCond ).MinOATCooling || OutsideDryBulbTemp > VRF( VRFCond ).MaxOATCooling ) && any( TerminalUnitList( TUListIndex ).CoolingCoilPresent ) ) {
					CoolingLoad( VRFCond ) = false;
					// test if heating load exists, account for thermostat control type
					{ auto const SELECT_CASE_var( VRF( VRFCond ).ThermostatPriority );
					if ( ( SELECT_CASE_var == LoadPriority ) || ( SELECT_CASE_var == ZonePriority ) ) {
						if ( SumHeatingLoads( VRFCond ) > 0.0 ) EnableSystem = true;
					} else if ( SELECT_CASE_var == ThermostatOffsetPriority ) {
						if ( MinDeltaT( VRFCond ) < 0.0 ) EnableSystem = true;
					} else if ( ( SELECT_CASE_var == ScheduledPriority ) || ( SELECT_CASE_var == MasterThermostatPriority ) ) {
						// can't switch modes if scheduled (i.e., would be switching to unscheduled mode)
						// or master TSTAT used (i.e., master zone only has a specific load - can't switch)
					} else {
					}}
					if ( EnableSystem ) {
						if ( ( OutsideDryBulbTemp >= VRF( VRFCond ).MinOATHeating && OutsideDryBulbTemp <= VRF( VRFCond ).MaxOATHeating ) && any( TerminalUnitList( TUListIndex ).HeatingCoilPresent ) ) {
							HeatingLoad( VRFCond ) = true;
						} else {
							if ( any( TerminalUnitList( TUListIndex ).CoolingCoilAvailable ) ) {
								if ( VRF( VRFCond ).CoolingMaxTempLimitIndex == 0 ) {
									ShowWarningMessage( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\"." );
									ShowContinueError( "...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Cooling Mode Limits have been exceeded and VRF system is disabled." );
									if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
										ShowContinueError( "... Outdoor Unit Inlet Water Temperature           = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
									} else {
										ShowContinueError( "... Outdoor Unit Inlet Air Temperature                 = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
									}
									ShowContinueError( "... Cooling Minimum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MinOATCooling, 3 ) );
									ShowContinueError( "... Cooling Maximum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MaxOATCooling, 3 ) );
									ShowContinueErrorTimeStamp( "... Check VRF Heat Pump Min/Max Outdoor Temperature in Cooling Mode limits." );
								}
								ShowRecurringWarningErrorAtEnd( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\" -- Exceeded VRF Heat Pump min/max cooling temperature limit error continues...", VRF( VRFCond ).CoolingMaxTempLimitIndex, OutsideDryBulbTemp, OutsideDryBulbTemp );
							}
						}
					} else {
						if ( any( TerminalUnitList( TUListIndex ).CoolingCoilAvailable ) ) {
							if ( VRF( VRFCond ).CoolingMaxTempLimitIndex == 0 ) {
								ShowWarningMessage( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\"." );
								ShowContinueError( "...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Cooling Mode Limits have been exceeded and VRF system is disabled." );
								if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
									ShowContinueError( "... Outdoor Unit Inlet Water Temperature           = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
								} else {
									ShowContinueError( "... Outdoor Unit Inlet Air Temperature                 = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
								}
								ShowContinueError( "... Cooling Minimum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MinOATCooling, 3 ) );
								ShowContinueError( "... Cooling Maximum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MaxOATCooling, 3 ) );
								ShowContinueErrorTimeStamp( "... Check VRF Heat Pump Min/Max Outdoor Temperature in Cooling Mode limits." );
							}
							ShowRecurringWarningErrorAtEnd( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\" -- Exceeded VRF Heat Pump min/max cooling temperature limit error continues...", VRF( VRFCond ).CoolingMaxTempLimitIndex, OutsideDryBulbTemp, OutsideDryBulbTemp );
						}
					}
				}
			} else if ( HeatingLoad( VRFCond ) ) {
				if ( ( OutsideDryBulbTemp < VRF( VRFCond ).MinOATHeating || OutsideDryBulbTemp > VRF( VRFCond ).MaxOATHeating ) && any( TerminalUnitList( TUListIndex ).HeatingCoilPresent ) ) {
					HeatingLoad( VRFCond ) = false;
					// test if heating load exists, account for thermostat control type
					{ auto const SELECT_CASE_var( VRF( VRFCond ).ThermostatPriority );
					if ( ( SELECT_CASE_var == LoadPriority ) || ( SELECT_CASE_var == ZonePriority ) ) {
						if ( SumCoolingLoads( VRFCond ) < 0.0 ) EnableSystem = true;
					} else if ( SELECT_CASE_var == ThermostatOffsetPriority ) {
						if ( MaxDeltaT( VRFCond ) > 0.0 ) EnableSystem = true;
					} else if ( ( SELECT_CASE_var == ScheduledPriority ) || ( SELECT_CASE_var == MasterThermostatPriority ) ) {
					} else {
					}}
					if ( EnableSystem ) {
						if ( ( OutsideDryBulbTemp >= VRF( VRFCond ).MinOATCooling && OutsideDryBulbTemp <= VRF( VRFCond ).MaxOATCooling ) && any( TerminalUnitList( TUListIndex ).CoolingCoilPresent ) ) {
							CoolingLoad( VRFCond ) = true;
						} else {
							if ( any( TerminalUnitList( TUListIndex ).HeatingCoilAvailable ) ) {
								if ( VRF( VRFCond ).HeatingMaxTempLimitIndex == 0 ) {
									ShowWarningMessage( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\"." );
									ShowContinueError( "...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Heating Mode Limits have been exceeded and VRF system is disabled." );
									if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
										ShowContinueError( "... Outdoor Unit Inlet Water Temperature           = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
									} else {
										ShowContinueError( "... Outdoor Unit Inlet Air Temperature             = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
									}
									ShowContinueError( "... Heating Minimum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MinOATHeating, 3 ) );
									ShowContinueError( "... Heating Maximum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MaxOATHeating, 3 ) );
									ShowContinueErrorTimeStamp( "... Check VRF Heat Pump Min/Max Outdoor Temperature in Heating Mode limits." );
								}
								ShowRecurringWarningErrorAtEnd( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\" -- Exceeded VRF Heat Pump min/max heating temperature limit error continues...", VRF( VRFCond ).HeatingMaxTempLimitIndex, OutsideDryBulbTemp, OutsideDryBulbTemp );
							}
						}
					} else {
						if ( any( TerminalUnitList( TUListIndex ).HeatingCoilAvailable ) ) {
							if ( VRF( VRFCond ).HeatingMaxTempLimitIndex == 0 ) {
								ShowWarningMessage( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\"." );
								ShowContinueError( "...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Heating Mode Limits have been exceeded and VRF system is disabled." );
								if ( VRF( VRFCond ).CondenserType == WaterCooled ) {
									ShowContinueError( "... Outdoor Unit Inlet Water Temperature           = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
								} else {
									ShowContinueError( "... Outdoor Unit Inlet Air Temperature             = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
								}
								ShowContinueError( "... Heating Minimum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MinOATHeating, 3 ) );
								ShowContinueError( "... Heating Maximum Outdoor Unit Inlet Temperature = " + TrimSigDigits( VRF( VRFCond ).MaxOATHeating, 3 ) );
								ShowContinueErrorTimeStamp( "... Check VRF Heat Pump Min/Max Outdoor Temperature in Heating Mode limits." );
							}
							ShowRecurringWarningErrorAtEnd( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\" -- Exceeded VRF Heat Pump min/max heating temperature limit error continues...", VRF( VRFCond ).HeatingMaxTempLimitIndex, OutsideDryBulbTemp, OutsideDryBulbTemp );
						}
					}
				}
			}

		} // IF (GetCurrentScheduleValue(VRF(VRFCond)%SchedPtr) .EQ. 0.0) THEN

		// initialize terminal unit flow rate
		if ( HeatingLoad( VRFCond ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) ) {
			if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
				Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
				Node( OutsideAirNode ).MassFlowRate = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
			} else {
				Node( InNode ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
			}
		} else if ( CoolingLoad( VRFCond ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) ) {
			if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
				Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
				Node( OutsideAirNode ).MassFlowRate = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
			} else {
				Node( InNode ).MassFlowRate = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
			}
		} else {
			if ( LastModeCooling( VRFCond ) ) {
				if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
					Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
					Node( OutsideAirNode ).MassFlowRate = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
				} else {
					Node( InNode ).MassFlowRate = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				}
			} else if ( LastModeHeating( VRFCond ) ) {
				if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
					Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
					Node( OutsideAirNode ).MassFlowRate = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
				} else {
					Node( InNode ).MassFlowRate = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
				}
			}
		}

		if ( VRFTU( VRFTUNum ).OAMixerUsed ) SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );

		OnOffAirFlowRatio = 1.0;

		// these flags are used in Subroutine CalcVRF to turn on the correct coil (heating or cooling)
		// valid operating modes
		// Heat Pump (heat recovery flags are set to FALSE):
		// CoolingLoad(VRFCond) - TU can only operate in this mode if heat recovery is not used and there is a cooling load
		// HeatingLoad(VRFCond) - TU can only operate in this mode if heat recovery is not used and there is a heating load
		// Heat Recovery (heat pump flags are set same as for Heat Pump operation):
		// TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) - TU will operate in this mode if heat recovery is used
		// TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) - TU will operate in this mode if heat recovery is used

		QZnReq = ZoneSysEnergyDemand( VRFTU( VRFTUNum ).ZoneNum ).RemainingOutputRequired;
		if ( std::abs( QZnReq ) < SmallLoad ) QZnReq = 0.0;
		LoadToCoolingSP = ZoneSysEnergyDemand( VRFTU( VRFTUNum ).ZoneNum ).RemainingOutputReqToCoolSP;
		// set initial terminal unit operating mode for heat recovery
		// operating mode for non-heat recovery set above using CoolingLoad(VRFCond) or HeatingLoad(VRFCond) variables
		// first turn off terminal unit
		TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
		TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
		// then set according to LoadToXXXXingSP variables
		if ( LoadToCoolingSP < -1.0 * SmallLoad ) {
			TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
			TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
		}
		LoadToHeatingSP = ZoneSysEnergyDemand( VRFTU( VRFTUNum ).ZoneNum ).RemainingOutputReqToHeatSP;
		if ( LoadToHeatingSP > SmallLoad ) {
			TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
			TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
		}
		if ( LoadToCoolingSP > 0.0 && LoadToHeatingSP < 0.0 ) QZnReq = 0.0;

		// next check for overshoot when constant fan mode is used
		// check operating load to see if OA will overshoot setpoint temperature when constant fan mode is used
		if ( VRFTU( VRFTUNum ).OpMode == ContFanCycCoil ) {
			SetCompFlowRate( VRFTUNum, VRFCond, true );

			if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
			// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
				CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
			} else {
			// Algorithm Type: VRF model based on system curve
				CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
			}

			// If the Terminal Unit has a net cooling capacity (TempOutput < 0) and
			// the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
			// see if the terminal unit operation will exceed the setpoint
			// 4 tests here to cover all possibilities:
			// IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .LT. 0.0d0)THEN
			// ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .GT. 0.0d0)THEN
			// ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .LT. 0.0d0)THEN
			// ELSE IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .GT. 0.0d0)THEN
			// END IF
			// could compress these to 2 complex IF's but logic inside each would get more complex
			if ( TempOutput < 0.0 && LoadToHeatingSP < 0.0 ) {
				// If the net cooling capacity overshoots the heating setpoint count as heating load
				if ( TempOutput < LoadToHeatingSP ) {
					// Don't count as heating load unless mode is allowed. Also check for floating zone.
					if ( TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != SingleCoolingSetPoint && TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != 0 ) {
						if ( ! LastModeHeating( VRFCond ) ) {
							// system last operated in cooling mode, change air flows and repeat coil off capacity test
							if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
								Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
								Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
								SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );
							} else {
								Node( InNode ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
							}

							if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
							// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
								CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
							} else {
							// Algorithm Type: VRF model based on system curve
								CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
							}

							// if zone temp will overshoot, pass the LoadToHeatingSP as the load to meet
							if ( TempOutput < LoadToHeatingSP ) {
								QZnReq = LoadToHeatingSP;
								TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
								TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
							}
						} else {
							// last mode was heating, zone temp will overshoot heating setpoint, reset QznReq to LoadtoHeatingSP
							QZnReq = LoadToHeatingSP;
							TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
							TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
						}
					}
				} else if ( TempOutput > LoadToCoolingSP && LoadToCoolingSP < 0.0 ) {
					//       If the net cooling capacity does not meet the zone cooling load enable cooling
					QZnReq = LoadToCoolingSP;
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
				} else if ( TempOutput < LoadToCoolingSP && LoadToCoolingSP < 0.0 ) {
					//       If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint
					QZnReq = 0.0;
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
				}
				//     If the terminal unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
				//     see if the terminal unit operation will exceed the setpoint
			} else if ( TempOutput > 0.0 && LoadToCoolingSP > 0.0 ) {
				//       If the net heating capacity overshoots the cooling setpoint count as cooling load
				if ( TempOutput > LoadToCoolingSP ) {
					//         Don't count as cooling load unless mode is allowed. Also check for floating zone.
					if ( TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != SingleHeatingSetPoint && TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != 0 ) {
						if ( ! LastModeCooling( VRFCond ) ) {
							if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
								Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
								Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
								SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );
							} else {
								Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
							}

							if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
							// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
								CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
							} else {
							// Algorithm Type: VRF model based on system curve
								CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
							}

							// if zone temp will overshoot, pass the LoadToCoolingSP as the load to meet
							if ( TempOutput > LoadToCoolingSP ) {
								QZnReq = LoadToCoolingSP;
								TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
								TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
							}
						} else {
							QZnReq = LoadToCoolingSP;
							TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
							TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
						}
					}
				} else if ( TempOutput < LoadToHeatingSP ) {
					//         Don't count as heating load unless mode is allowed. Also check for floating zone.
					if ( TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != SingleCoolingSetPoint && TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != 0 ) {
						if ( ! LastModeHeating( VRFCond ) ) {
							if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
								Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
								Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
								SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );
							} else {
								Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
							}

							if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
							// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
								CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
							} else {
							// Algorithm Type: VRF model based on system curve
								CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
							}

							// if zone temp will overshoot, pass the LoadToHeatingSP as the load to meet
							if ( TempOutput < LoadToHeatingSP ) {
								QZnReq = LoadToHeatingSP;
								TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
								TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
							}
						} else {
							QZnReq = LoadToHeatingSP;
							TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
							TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
						}
					}
				} else if ( TempOutput > LoadToHeatingSP && TempOutput < LoadToCoolingSP ) {
					//         If the net capacity does not overshoot either setpoint
					QZnReq = 0.0;
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
				} else {
					//         If the net heating capacity meets the zone heating load but does not overshoot cooling setpoint
					QZnReq = 0.0;
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
				}
				//     If the terminal unit has a net heating capacity and the zone temp is above the Tstat cooling setpoint
				//     see if the terminal unit operation will exceed the setpoint
			} else if ( TempOutput > 0.0 && LoadToCoolingSP < 0.0 ) {
				//       If the net heating capacity overshoots the cooling setpoint count as cooling load
				//       Don't count as cooling load unless mode is allowed. Also check for floating zone.
				if ( TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != SingleHeatingSetPoint && TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != 0 ) {
					if ( ! LastModeCooling( VRFCond ) ) {
						if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
							Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
							Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
							SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );
						} else {
							Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
						}

						if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
						// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
							CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
						} else {
						// Algorithm Type: VRF model based on system curve
							CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
						}

						// if zone temp will overshoot, pass the LoadToCoolingSP as the load to meet
						if ( TempOutput > LoadToCoolingSP ) {
							QZnReq = LoadToCoolingSP;
							TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
							TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
						}
						// last mode was cooling, zone temp will overshoot cooling setpoint, reset QznReq to LoadtoCoolingSP
					} else {
						QZnReq = LoadToCoolingSP;
						TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
						TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
					}
				}
				// If the Terminal Unit has a net cooling capacity (TempOutput < 0) and
				// the zone temp is below the Tstat heating setpoint (QToHeatSetPt > 0)
				// see if the terminal unit operation will exceed the setpoint
			} else if ( TempOutput < 0.0 && LoadToHeatingSP > 0.0 ) {
				// Don't count as heating load unless mode is allowed. Also check for floating zone.
				if ( TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != SingleCoolingSetPoint && TempControlType( VRFTU( VRFTUNum ).ZoneNum ) != 0 ) {
					if ( ! LastModeHeating( VRFCond ) ) {
						// system last operated in cooling mode, change air flows and repeat coil off capacity test
						if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
							Node( VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
							Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
							SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );
						} else {
							Node( InNode ).MassFlowRate = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
						}

						if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
						// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
							CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
						} else {
						// Algorithm Type: VRF model based on system curve
							CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
						}

						// if zone temp will overshoot, pass the LoadToHeatingSP as the load to meet
						if ( TempOutput < LoadToHeatingSP ) {
							QZnReq = LoadToHeatingSP;
							TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
							TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
						}
					} else {
						// last mode was heating, zone temp will overshoot heating setpoint, reset QznReq to LoadtoHeatingSP
						QZnReq = LoadToHeatingSP;
						TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
						TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
					}
				}
			}
		} // IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil)THEN

		if ( VRF( VRFCond ).HeatRecoveryUsed ) {
			if ( OutsideDryBulbTemp < VRF( VRFCond ).MinOATHeatRecovery || OutsideDryBulbTemp > VRF( VRFCond ).MaxOATHeatRecovery ) {
				if ( any( TerminalUnitList( TUListIndex ).HRCoolRequest ) || any( TerminalUnitList( TUListIndex ).HRHeatRequest ) ) {
					if ( VRF( VRFCond ).HRMaxTempLimitIndex == 0 ) {
						ShowWarningMessage( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\"." );
						ShowContinueError( "...InitVRF: VRF Heat Pump Min/Max Outdoor Temperature in Heat Recovery Mode Limits have been exceeded and VRF heat recovery is disabled." );
						ShowContinueError( "... Outdoor Dry-Bulb Temperature                       = " + TrimSigDigits( OutsideDryBulbTemp, 3 ) );
						ShowContinueError( "... Heat Recovery Minimum Outdoor Dry-Bulb Temperature = " + TrimSigDigits( VRF( VRFCond ).MinOATHeatRecovery, 3 ) );
						ShowContinueError( "... Heat Recovery Maximum Outdoor Dry-Bulb Temperature = " + TrimSigDigits( VRF( VRFCond ).MaxOATHeatRecovery, 3 ) );
						ShowContinueErrorTimeStamp( "... Check VRF Heat Pump Min/Max Outdoor Temperature in Heat Recovery Mode limits." );
						ShowContinueError( "...the system will operate in heat pump mode when applicable." );
					}
					ShowRecurringWarningErrorAtEnd( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\" -- Exceeded VRF Heat Recovery min/max outdoor temperature limit error continues...", VRF( VRFCond ).HRMaxTempLimitIndex, OutsideDryBulbTemp, OutsideDryBulbTemp );
				}
				// Allow heat pump mode to operate if within limits
				if ( OutsideDryBulbTemp < VRF( VRFCond ).MinOATCooling || OutsideDryBulbTemp > VRF( VRFCond ).MaxOATCooling ) {
					// Disable cooling mode only, heating model will still be allowed
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
				}
				if ( OutsideDryBulbTemp < VRF( VRFCond ).MinOATHeating || OutsideDryBulbTemp > VRF( VRFCond ).MaxOATHeating ) {
					// Disable heating mode only, cooling model will still be allowed
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
				}
			}
		} else {
			TerminalUnitList( TUListIndex ).HRHeatRequest = false;
			TerminalUnitList( TUListIndex ).HRCoolRequest = false;
		}

		// Override operating mode when using EMS
		// this logic seems suspect, uses a "just run it on" mentality. Nee to test using EMS.
		if ( VRF( VRFCond ).EMSOverrideHPOperatingMode ) {
			if ( VRF( VRFCond ).EMSValueForHPOperatingMode == 0.0 ) { // Off
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = false;
				TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
				TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
			} else if ( VRF( VRFCond ).EMSValueForHPOperatingMode == 1.0 ) { // Cooling
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = true;
				QZnReq = LoadToCoolingSP;
				if ( VRF( VRFCond ).HeatRecoveryUsed ) {
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = false;
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = true;
				}
			} else if ( VRF( VRFCond ).EMSValueForHPOperatingMode == 2.0 ) { // Heating
				HeatingLoad( VRFCond ) = true;
				CoolingLoad( VRFCond ) = false;
				QZnReq = LoadToHeatingSP;
				if ( VRF( VRFCond ).HeatRecoveryUsed ) {
					TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) = true;
					TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) = false;
				}
			} else {
				if ( VRF( VRFCond ).HPOperatingModeErrorIndex == 0 ) {
					ShowWarningMessage( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\"." );
					ShowContinueError( "...InitVRF: Illegal HP operating mode = " + TrimSigDigits( VRF( VRFCond ).EMSValueForHPOperatingMode, 0 ) );
					ShowContinueError( "...InitVRF: VRF HP operating mode will not be controlled by EMS." );

				}
				ShowRecurringWarningErrorAtEnd( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + " \"" + VRF( VRFCond ).Name + "\" -- Illegal HP operating mode error continues...", VRF( VRFCond ).HPOperatingModeErrorIndex, VRF( VRFCond ).EMSValueForHPOperatingMode, VRF( VRFCond ).EMSValueForHPOperatingMode );
			}
		}

		// set the TU flow rate. Check for heat recovery operation first, these will be FALSE if HR is not used.
		if ( TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) {
			CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
			CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
			OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
		} else if ( TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) {
			CompOnMassFlow = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
			CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
			OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
		} else if ( CoolingLoad( VRFCond ) && QZnReq != 0.0 ) {
			CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
			CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
			OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
		} else if ( HeatingLoad( VRFCond ) && QZnReq != 0.0 ) {
			CompOnMassFlow = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
			CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
			OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
		} else {
			if ( LastModeCooling( VRFCond ) ) {
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
			}
			if ( LastModeHeating( VRFCond ) ) {
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
			}
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
		}

		if ( VRFTU( VRFTUNum ).OpMode == CycFanCycCoil ) {
			CompOffMassFlow = 0.0;
			OACompOffMassFlow = 0.0;
		}

		SetAverageAirFlow( VRFTUNum, 0.0, OnOffAirFlowRatio );

	}

	void
	SetCompFlowRate(
		int const VRFTUNum,
		int const VRFCond,
		Optional_bool_const UseCurrentMode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for calling VRF terminal units during Init to initialize flow rate
		// while looping through all terminal units connected to a specific condenser.
		// This allows polling of capacities for all terminal units.
		// Since the heat pump can only heat or cool, a single operating mode is chosen for each condenser.

		// METHODOLOGY EMPLOYED:
		// Initializes flow rates for a specific terminal unit.

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
		bool CurrentMode; // - specifies whether current or previous operating mode is used
		int IndexToTUInTUList; // - index to TU in specific list for this VRF system
		int TUListIndex; // index to TU list for this VRF system

		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		TUListIndex = VRFTU( VRFTUNum ).TUListIndex;
		if ( present( UseCurrentMode ) ) {
			CurrentMode = UseCurrentMode;
		} else {
			CurrentMode = false;
		}

		// uses current operating mode to set flow rate (after mode is set)
		//  IF(VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList))THEN
		if ( TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) {
			CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
			CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
			OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			// uses current operating mode to set flow rate (after mode is set)
			//  ELSE IF(VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList))THEN
		} else if ( TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) {
			CompOnMassFlow = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
			CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
			OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
			OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
		} else if ( CurrentMode ) { // uses current operating mode to set flow rate (after mode is set)
			if ( CoolingLoad( VRFCond ) ) {
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			} else if ( HeatingLoad( VRFCond ) ) {
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			} else if ( LastModeCooling( VRFCond ) ) { // if NOT cooling or heating then use last mode
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			} else if ( LastModeHeating( VRFCond ) ) { // if NOT cooling or heating then use last mode
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			} else { // should not happen so just set to cooling flow rate
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			}
		} else { // uses previous operating mode to set flow rate (used for looping through each TU in Init before mode is set)
			if ( LastModeCooling( VRFCond ) ) {
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			} else if ( LastModeHeating( VRFCond ) ) {
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxHeatAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoHeatAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).HeatOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			} else { // should not happen so just set to cooling flow rate
				CompOnMassFlow = VRFTU( VRFTUNum ).MaxCoolAirMassFlow;
				CompOffMassFlow = VRFTU( VRFTUNum ).MaxNoCoolAirMassFlow;
				OACompOnMassFlow = VRFTU( VRFTUNum ).CoolOutAirMassFlow;
				OACompOffMassFlow = VRFTU( VRFTUNum ).NoCoolHeatOutAirMassFlow;
			}
		}

		if ( VRFTU( VRFTUNum ).OpMode == CycFanCycCoil ) {
			CompOffMassFlow = 0.0;
			OACompOffMassFlow = 0.0;
		}

	}

	void
	SizeVRF( int const VRFTUNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      B Nigusse, FSEC, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing VRF Components for which inputs have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using CurveManager::CurveValue;
		auto & GetDXCoilCap( DXCoils::GetCoilCapacityByIndexType );
		using ReportSizingManager::ReportSizingOutput;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::RequestSizing;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		static std::string const RoutineName("SizeVRF: "); // include trailing blank space

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool CheckVRFCombinationRatio;
		bool FoundAll; // temporary variable used to check all terminal units
		bool errFlag; // temporary variable used for error checking
		Real64 TUCoolingCapacity; // total terminal unit cooling capacity
		Real64 TUHeatingCapacity; // total terminal unit heating capacity
		int VRFCond; // index to VRF condenser
		int TUListNum; // index to terminal unit list
		int TUIndex; // index to terminal unit
		int NumTU; // DO Loop index counter
		static bool MyOneTimeEIOFlag( true ); // eio header flag reporting
		Real64 OnOffAirFlowRat; // temporary variable used when sizing coils
		Real64 DXCoilCap; // capacity of DX cooling coil (W)
		bool IsAutoSize; // Indicator to autosize
		Real64 MaxCoolAirVolFlowDes; // Autosized supply air during cooling for reporting
		Real64 MaxCoolAirVolFlowUser; // Hardsized supply air during cooling for reporting
		Real64 MaxHeatAirVolFlowDes; // Autosized supply air during heating for reporting
		Real64 MaxHeatAirVolFlowUser; // Hardsized supply air during heating for reporting
		Real64 MaxNoCoolAirVolFlowDes; // Autosized supply air flow when no cooling is needed for reporting
		Real64 MaxNoCoolAirVolFlowUser; // Hardsized supply air flow when no cooling is needed for reporting
		Real64 MaxNoHeatAirVolFlowDes; // Autosized supply air flow when no heating is needed for reporting
		Real64 MaxNoHeatAirVolFlowUser; // Hardsized supply air flow when no heating is needed for reporting
		Real64 CoolOutAirVolFlowDes; // Autosized outdoor air flow during cooling for reporting
		Real64 CoolOutAirVolFlowUser; // Hardsized outdoor air flow during cooling for reporting
		Real64 HeatOutAirVolFlowDes; // Autosized outdoor air flow during heating for reporting
		Real64 HeatOutAirVolFlowUser; // Hardsized outdoor air flow during heating for reporting
		Real64 NoCoolHeatOutAirVolFlowDes; // Autosized outdoor air when unconditioned for reporting
		Real64 NoCoolHeatOutAirVolFlowUser; // Hardsized outdoor air when unconditioned for reporting
		Real64 CoolingCapacityDes; // Autosized cooling capacity for reporting
		Real64 CoolingCapacityUser; // Hardsized cooling capacity for reporting
		Real64 HeatingCapacityDes; // Autosized heating capacity for reporting
		Real64 HeatingCapacityUser; // Hardsized heating capacity for reporting
		Real64 DefrostCapacityDes; // Autosized defrost heater capacity for reporting
		Real64 DefrostCapacityUser; // Hardsized defrost heater capacity for reporting
		Real64 EvapCondAirVolFlowRateDes; // Autosized evaporative condenser flow for reporting
		Real64 EvapCondAirVolFlowRateUser; // Hardsized evaporative condenser flow for reporting
		Real64 EvapCondPumpPowerDes; // Autosized evaporative condenser pump power for reporting
		Real64 EvapCondPumpPowerUser; // Hardsized evaporative condenser pump power for reporting

		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag = true; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		// Formats
		static gio::Fmt Format_990( "('! <VRF System Information>, VRF System Type, VRF System Name, ','VRF System Cooling Combination Ratio, VRF System Heating Combination Ratio, ','VRF System Cooling Piping Correction Factor, VRF System Heating Piping Correction Factor')" );
		static gio::Fmt Format_991( "(' VRF System Information',6(', ',A))" );

		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		IsAutoSize = false;
		MaxCoolAirVolFlowDes = 0.0;
		MaxCoolAirVolFlowUser = 0.0;
		MaxHeatAirVolFlowDes = 0.0;
		MaxHeatAirVolFlowUser = 0.0;
		MaxNoCoolAirVolFlowDes = 0.0;
		MaxNoCoolAirVolFlowUser = 0.0;
		MaxNoHeatAirVolFlowDes = 0.0;
		MaxNoHeatAirVolFlowUser = 0.0;
		CoolOutAirVolFlowDes = 0.0;
		CoolOutAirVolFlowUser = 0.0;
		HeatOutAirVolFlowDes = 0.0;
		HeatOutAirVolFlowUser = 0.0;
		NoCoolHeatOutAirVolFlowDes = 0.0;
		NoCoolHeatOutAirVolFlowUser = 0.0;
		CoolingCapacityDes = 0.0;
		CoolingCapacityUser = 0.0;
		HeatingCapacityDes = 0.0;
		HeatingCapacityUser = 0.0;
		DefrostCapacityDes = 0.0;
		DefrostCapacityUser = 0.0;
		EvapCondAirVolFlowRateDes = 0.0;
		EvapCondAirVolFlowRateUser = 0.0;
		EvapCondPumpPowerDes = 0.0;
		EvapCondPumpPowerUser = 0.0;

		DataScalableSizingON = false;
		DataScalableCapSizingON = false;
		DataFracOfAutosizedCoolingAirflow = 1.0;
		DataFracOfAutosizedHeatingAirflow = 1.0;
		DataFracOfAutosizedCoolingCapacity = 1.0;
		DataFracOfAutosizedHeatingCapacity = 1.0;

		if ( MyOneTimeSizeFlag ) {
			// initialize the environment and sizing flags
			CheckVRFCombinationRatio.dimension( NumVRFCond, true );
			MyOneTimeSizeFlag = false;
		}

		CompType = "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow";
		CompName = VRFTU( VRFTUNum ).Name;
		if ( CurZoneEqNum > 0 ) {
			if ( VRFTU( VRFTUNum ).HVACSizingIndex > 0 ) {
				zoneHVACIndex = VRFTU( VRFTUNum ).HVACSizingIndex;
				DataZoneNumber = VRFTU( VRFTUNum ).ZoneNum;

				SizingMethod = CoolingAirflowSizing;
				FieldNum = 1; // N1, \field Supply Air Flow Rate During Cooling Operation
				PrintFlag = true;
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames( FieldNum ) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
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
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					VRFTU( VRFTUNum ).MaxCoolAirVolFlow = TempSize;
				} else if ( SAFMethod == FlowPerCoolingCapacity ) {
					SizingMethod = CoolingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
					if ( ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod == FractionOfAutosizedCoolingCapacity ) {
						DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					DataAutosizedCoolingCapacity = TempSize;
					DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					SizingMethod = CoolingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					VRFTU( VRFTUNum ).MaxCoolAirVolFlow = TempSize;
				}

				SizingMethod = HeatingAirflowSizing;
				FieldNum = 3; //N3, \field Supply Air Flow Rate During Heating Operation
				PrintFlag = true;
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames( FieldNum ) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if (ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow > 0.0) {
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
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					VRFTU( VRFTUNum ).MaxHeatAirVolFlow = TempSize;
				} else if ( SAFMethod == FlowPerHeatingCapacity ) {
					SizingMethod = HeatingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
					if ( ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod == FractionOfAutosizedHeatingCapacity ) {
						DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					DataAutosizedHeatingCapacity = TempSize;
					DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					SizingMethod = HeatingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					VRFTU( VRFTUNum ).MaxHeatAirVolFlow = TempSize;
				}

				SizingMethod = CoolingAirflowSizing;
				FieldNum = 2; //N2, \field Supply Air Flow Rate When No Cooling is Needed
				PrintFlag = true;
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames( FieldNum ) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).NoCoolHeatSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( ( SAFMethod == SupplyAirFlowRate ) || ( SAFMethod == FlowPerFloorArea ) || ( SAFMethod == FractionOfAutosizedHeatingAirflow ) || ( SAFMethod == FractionOfAutosizedCoolingAirflow ) ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if (ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow > 0.0) {
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
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow = TempSize;
				}

				SizingMethod = HeatingAirflowSizing;
				FieldNum = 4; //N4, \field Supply Air Flow Rate When No Heating is Needed
				PrintFlag = true;
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames(FieldNum) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).NoCoolHeatSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod(SizingMethod) = SAFMethod;
				if ( ( SAFMethod == SupplyAirFlowRate ) || ( SAFMethod == FlowPerFloorArea ) || ( SAFMethod == FractionOfAutosizedHeatingAirflow ) || ( SAFMethod == FractionOfAutosizedCoolingAirflow ) ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if (ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow > 0.0) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if  (SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxNoCoolHeatAirVolFlow;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow = TempSize;
				}

				// initialize capacity sizing variables: cooling
				SizingMethod = CoolingCapacitySizing;
				CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
				if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
					if ( CapSizingMethod == HeatingDesignCapacity ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						}
					} else if (CapSizingMethod == CapacityPerFloorArea) {
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
						if (ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0) {
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
						}
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
						DataScalableCapSizingON = true;
					} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
						DataScalableCapSizingON = true;
					}
				}
			} else {
				// no scalble sizing method has been specified. Sizing proceeds using the method
				// specified in the zoneHVAC object

				PrintFlag = true;

				SizingMethod = CoolingAirflowSizing;
				FieldNum = 1; // N1, \field Supply Air Flow Rate During Cooling Operation
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames(FieldNum) + " [m3/s]";
				TempSize = VRFTU( VRFTUNum ).MaxCoolAirVolFlow;
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				VRFTU( VRFTUNum ).MaxCoolAirVolFlow = TempSize;

				FieldNum = 3; //N3, \field Supply Air Flow Rate During Heating Operation
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames( FieldNum ) + " [m3/s]";
				SizingMethod = HeatingAirflowSizing;
				TempSize = VRFTU( VRFTUNum ).MaxHeatAirVolFlow;
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				VRFTU( VRFTUNum ).MaxHeatAirVolFlow = TempSize;

				FieldNum = 2; //N2, \field Supply Air Flow Rate When No Cooling is Needed
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames( FieldNum ) + " [m3/s]";
				SizingMethod = SystemAirflowSizing;
				TempSize = VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow;
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				VRFTU( VRFTUNum ).MaxNoCoolAirVolFlow = TempSize;

				FieldNum = 4; //N4, \field Supply Air Flow Rate When No Heating is Needed
				SizingString = VRFTUNumericFields( VRFTUNum ).FieldNames( FieldNum ) + " [m3/s]";
				SizingMethod = SystemAirflowSizing;
				TempSize = VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow;
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				VRFTU( VRFTUNum ).MaxNoHeatAirVolFlow = TempSize;
			}
		}
		IsAutoSize = false;
		if ( VRFTU( VRFTUNum ).CoolOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( VRFTU( VRFTUNum ).CoolOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]", VRFTU( VRFTUNum ).CoolOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name );
				CoolOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, VRFTU( VRFTUNum ).MaxCoolAirVolFlow );
				if ( CoolOutAirVolFlowDes < SmallAirVolFlow ) {
					CoolOutAirVolFlowDes = 0.0;
				}

				if ( IsAutoSize ) {
					VRFTU( VRFTUNum ).CoolOutAirVolFlow = CoolOutAirVolFlowDes;
					ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]", CoolOutAirVolFlowDes );
				} else {
					if ( VRFTU( VRFTUNum ).CoolOutAirVolFlow > 0.0 && CoolOutAirVolFlowDes > 0.0 ) {
						CoolOutAirVolFlowUser = VRFTU( VRFTUNum ).CoolOutAirVolFlow;
						ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]", CoolOutAirVolFlowDes, "User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]", CoolOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( CoolOutAirVolFlowDes - CoolOutAirVolFlowUser ) / CoolOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ' ' + VRFTU( VRFTUNum ).Name );
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
		if ( VRFTU( VRFTUNum ).HeatOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( VRFTU( VRFTUNum ).CoolOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Outdoor Air Flow Rate During Heating Operation [m3/s]", VRFTU( VRFTUNum ).CoolOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name );
				HeatOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, VRFTU( VRFTUNum ).MaxHeatAirVolFlow );
				if ( HeatOutAirVolFlowDes < SmallAirVolFlow ) {
					HeatOutAirVolFlowDes = 0.0;
				}

				if ( IsAutoSize ) {
					VRFTU( VRFTUNum ).HeatOutAirVolFlow = HeatOutAirVolFlowDes;
					ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]", HeatOutAirVolFlowDes );
				} else {
					if ( VRFTU( VRFTUNum ).HeatOutAirVolFlow > 0.0 && HeatOutAirVolFlowDes > 0.0 ) {
						HeatOutAirVolFlowUser = VRFTU( VRFTUNum ).HeatOutAirVolFlow;
						ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]", HeatOutAirVolFlowDes, "User-Specified Outdoor Air Flow Rate During Heating Operation [m3/s]", HeatOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( HeatOutAirVolFlowDes - HeatOutAirVolFlowUser ) / HeatOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ' ' + VRFTU( VRFTUNum ).Name );
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
		if ( VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name );
				NoCoolHeatOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, VRFTU( VRFTUNum ).HeatOutAirVolFlow, VRFTU( VRFTUNum ).CoolOutAirVolFlow );
				if ( NoCoolHeatOutAirVolFlowDes < SmallAirVolFlow ) {
					NoCoolHeatOutAirVolFlowDes = 0.0;
				}

				if ( IsAutoSize ) {
					VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow = NoCoolHeatOutAirVolFlowDes;
					ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", NoCoolHeatOutAirVolFlowDes );
				} else {
					if ( VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow > 0.0 && NoCoolHeatOutAirVolFlowDes > 0.0 ) {
						NoCoolHeatOutAirVolFlowUser = VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow;
						ReportSizingOutput( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ), VRFTU( VRFTUNum ).Name, "Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", NoCoolHeatOutAirVolFlowDes, "User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]", NoCoolHeatOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( NoCoolHeatOutAirVolFlowDes - NoCoolHeatOutAirVolFlowUser ) / NoCoolHeatOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + ' ' + VRFTU( VRFTUNum ).Name );
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

		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).AirVolFlow = max( VRFTU( VRFTUNum ).MaxCoolAirVolFlow, VRFTU( VRFTUNum ).MaxHeatAirVolFlow );
		}

		if ( CheckVRFCombinationRatio( VRFCond ) ) {
			OnOffAirFlowRat = 1.0;
			// set up the outside air data for sizing the DX coils
			ZoneEqDXCoil = true;
			if ( CurZoneEqNum > 0 ) {
				if ( VRFTU( VRFTUNum ).CoolOutAirVolFlow > 0.0 || VRFTU( VRFTUNum ).HeatOutAirVolFlow > 0.0 ) {
					ZoneEqSizing( CurZoneEqNum ).OAVolFlow = max( VRFTU( VRFTUNum ).CoolOutAirVolFlow, VRFTU( VRFTUNum ).HeatOutAirVolFlow );
				} else {
					ZoneEqSizing( CurZoneEqNum ).OAVolFlow = 0.0;
				}
			} else {
				ZoneEqSizing( CurZoneEqNum ).OAVolFlow = 0.0;
			}

			// simulate the TU to size the coils
			if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
			// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
				CalcVRF_FluidTCtrl( VRFTUNum, true, 0.0, TUCoolingCapacity, OnOffAirFlowRat );
			} else {
			// Algorithm Type: VRF model based on system curve
				CalcVRF( VRFTUNum, true, 0.0, TUCoolingCapacity, OnOffAirFlowRat );
			}

			//    ZoneEqDXCoil = .FALSE.
			TUCoolingCapacity = 0.0;
			TUHeatingCapacity = 0.0;
			FoundAll = true;
			TUListNum = VRFTU( VRFTUNum ).TUListIndex;
			for ( NumTU = 1; NumTU <= TerminalUnitList( TUListNum ).NumTUInList; ++NumTU ) {
				TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
				if ( VRFTU( TUIndex ).CoolCoilIndex > 0 ) {
					DXCoilCap = GetDXCoilCap( VRFTU( TUIndex ).CoolCoilIndex, VRFTU( TUIndex ).DXCoolCoilType_Num, errFlag );
					TUCoolingCapacity += DXCoilCap;
					if ( DXCoilCap == AutoSize ) {
						FoundAll = false;
						break;
					}
				}
				if ( VRFTU( TUIndex ).HeatCoilIndex > 0 ) {
					DXCoilCap = GetDXCoilCap( VRFTU( TUIndex ).HeatCoilIndex, VRFTU( TUIndex ).DXHeatCoilType_Num, errFlag );
					TUHeatingCapacity += DXCoilCap;
					if ( DXCoilCap == AutoSize ) {
						FoundAll = false;
						break;
					}
				}
			}

			if ( FoundAll ) {
				IsAutoSize = false;
				if ( VRF( VRFCond ).CoolingCapacity == AutoSize ) {
					IsAutoSize = true;
				}
				CoolingCapacityDes = TUCoolingCapacity;
				if ( IsAutoSize ) {
					VRF( VRFCond ).CoolingCapacity = CoolingCapacityDes;
					ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Rated Total Cooling Capacity (gross) [W]", CoolingCapacityDes );
				} else {
					if ( VRF( VRFCond ).CoolingCapacity > 0.0 && CoolingCapacityDes > 0.0 ) {
						CoolingCapacityUser = VRF( VRFCond ).CoolingCapacity;
						ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Rated Total Cooling Capacity (gross) [W]", CoolingCapacityDes, "User-Specified Rated Total Cooling Capacity (gross) [W]", CoolingCapacityUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( CoolingCapacityDes - CoolingCapacityUser ) / CoolingCapacityUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + ' ' + VRFTU( VRFCond ).Name );
								ShowContinueError( "User-Specified Rated Total Cooling Capacity (gross) of " + RoundSigDigits( CoolingCapacityUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Rated Total Cooling Capacity (gross) of " + RoundSigDigits( CoolingCapacityDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}

				if ( VRF( VRFCond ).CoolingCapacity > 0.0 ) {
					VRF( VRFCond ).CoolingCombinationRatio = TUCoolingCapacity / VRF( VRFCond ).CoolingCapacity;
				}

				IsAutoSize = false;
				if ( VRF( VRFCond ).HeatingCapacity == AutoSize ) {
					IsAutoSize = true;
				}
				if ( VRF( VRFCond ).LockHeatingCapacity ) {
					HeatingCapacityDes = VRF( VRFCond ).CoolingCapacity * VRF( VRFCond ).HeatingCapacitySizeRatio;
				} else {
					HeatingCapacityDes = TUHeatingCapacity;
				}
				if ( IsAutoSize ) {
					VRF( VRFCond ).HeatingCapacity = HeatingCapacityDes;
					ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Rated Total Heating Capacity [W]", HeatingCapacityDes );
				} else {
					if ( VRF( VRFCond ).HeatingCapacity > 0.0 && HeatingCapacityDes > 0.0 ) {
						HeatingCapacityUser = VRF( VRFCond ).HeatingCapacity;
						ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Rated Total Heating Capacity [W]", HeatingCapacityDes, "User-Specified Rated Total Heating Capacity [W]", HeatingCapacityUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( HeatingCapacityDes - HeatingCapacityUser ) / HeatingCapacityUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + ' ' + VRFTU( VRFCond ).Name );
								ShowContinueError( "User-Specified Rated Total Heating Capacity of " + RoundSigDigits( HeatingCapacityUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Rated Total Heating Capacity of " + RoundSigDigits( HeatingCapacityDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}

				if ( VRF( VRFCond ).HeatingCapacity > 0.0 ) {
					VRF( VRFCond ).HeatingCombinationRatio = TUHeatingCapacity / VRF( VRFCond ).HeatingCapacity;
				}

				// calculate the piping correction factors only once
				if ( VRF( VRFCond ).PCFLengthCoolPtr > 0 ) {
					{ auto const SELECT_CASE_var( VRF( VRFCond ).PCFLengthCoolPtrType );
					if ( SELECT_CASE_var == BiQuadratic ) {
						VRF( VRFCond ).PipingCorrectionCooling = min( 1.0, max( 0.5, CurveValue( VRF( VRFCond ).PCFLengthCoolPtr, VRF( VRFCond ).EquivPipeLngthCool, VRF( VRFCond ).CoolingCombinationRatio ) + VRF( VRFCond ).VertPipeLngth * VRF( VRFCond ).PCFHeightCool ) );
					} else {
						VRF( VRFCond ).PipingCorrectionCooling = min( 1.0, max( 0.5, CurveValue( VRF( VRFCond ).PCFLengthCoolPtr, VRF( VRFCond ).EquivPipeLngthCool ) + VRF( VRFCond ).VertPipeLngth * VRF( VRFCond ).PCFHeightCool ) );
					}}
				} else {
					VRF( VRFCond ).PipingCorrectionCooling = min( 1.0, max( 0.5, ( 1.0 + VRF( VRFCond ).VertPipeLngth * VRF( VRFCond ).PCFHeightCool ) ) );
				}

				if ( VRF( VRFCond ).PCFLengthHeatPtr > 0 ) {
					{ auto const SELECT_CASE_var( VRF( VRFCond ).PCFLengthHeatPtrType );
					if ( SELECT_CASE_var == BiQuadratic ) {
						VRF( VRFCond ).PipingCorrectionHeating = min( 1.0, max( 0.5, CurveValue( VRF( VRFCond ).PCFLengthHeatPtr, VRF( VRFCond ).EquivPipeLngthHeat, VRF( VRFCond ).HeatingCombinationRatio ) + VRF( VRFCond ).VertPipeLngth * VRF( VRFCond ).PCFHeightHeat ) );
					} else {
						VRF( VRFCond ).PipingCorrectionHeating = min( 1.0, max( 0.5, CurveValue( VRF( VRFCond ).PCFLengthHeatPtr, VRF( VRFCond ).EquivPipeLngthHeat ) + VRF( VRFCond ).VertPipeLngth * VRF( VRFCond ).PCFHeightHeat ) );
					}}
				} else {
					VRF( VRFCond ).PipingCorrectionHeating = min( 1.0, max( 0.5, ( 1.0 + VRF( VRFCond ).VertPipeLngth * VRF( VRFCond ).PCFHeightHeat ) ) );
				}

				VRF( VRFCond ).RatedCoolingPower = VRF( VRFCond ).CoolingCapacity / VRF( VRFCond ).CoolingCOP;
				VRF( VRFCond ).RatedHeatingPower = VRF( VRFCond ).HeatingCapacity / VRF( VRFCond ).HeatingCOP;

				if ( VRF( VRFCond ).CoolCombRatioPTR > 0 ) {
					CoolCombinationRatio( VRFCond ) = CurveValue( VRF( VRFCond ).CoolCombRatioPTR, VRF( VRFCond ).CoolingCombinationRatio );
				} else {
					CoolCombinationRatio( VRFCond ) = 1.0;
				}

				if ( VRF( VRFCond ).HeatCombRatioPTR > 0 ) {
					HeatCombinationRatio( VRFCond ) = CurveValue( VRF( VRFCond ).HeatCombRatioPTR, VRF( VRFCond ).HeatingCombinationRatio );
				} else {
					HeatCombinationRatio( VRFCond ) = 1.0;
				}
				// autosize resistive defrost heater capacity
				IsAutoSize = false;
				if ( VRF( VRFCond ).DefrostCapacity == AutoSize ) {
					IsAutoSize = true;
				}
				if ( VRF( VRFCond ).DefrostStrategy == Resistive ) {
					DefrostCapacityDes = VRF( VRFCond ).CoolingCapacity;
				} else {
					DefrostCapacityDes = 0.0;
				}
				if ( IsAutoSize ) {
					VRF( VRFCond ).DefrostCapacity = DefrostCapacityDes;
					ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Resistive Defrost Heater Capacity", DefrostCapacityDes );
				} else {
					if ( VRF( VRFCond ).DefrostCapacity > 0.0 && DefrostCapacityDes > 0.0 ) {
						DefrostCapacityUser = VRF( VRFCond ).DefrostCapacity;
						ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Resistive Defrost Heater Capacity", DefrostCapacityDes, "User-Specified Resistive Defrost Heater Capacity", DefrostCapacityUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( DefrostCapacityDes - DefrostCapacityUser ) / DefrostCapacityUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + ' ' + VRFTU( VRFCond ).Name );
								ShowContinueError( "User-Specified Resistive Defrost Heater Capacity of " + RoundSigDigits( DefrostCapacityUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Resistive Defrost Heater Capacity of " + RoundSigDigits( DefrostCapacityDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}

				IsAutoSize = false;
				if ( VRF( VRFCond ).EvapCondAirVolFlowRate == AutoSize ) {
					IsAutoSize = true;
				}
				// Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
				EvapCondAirVolFlowRateDes = VRF( VRFCond ).CoolingCapacity * 0.000114;
				if ( IsAutoSize ) {
					VRF( VRFCond ).EvapCondAirVolFlowRate = EvapCondAirVolFlowRateDes;
					ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Evaporative Condenser Air Flow Rate [m3/s]", EvapCondAirVolFlowRateDes );
				} else {
					if ( VRF( VRFCond ).EvapCondAirVolFlowRate > 0.0 && EvapCondAirVolFlowRateDes > 0.0 ) {
						EvapCondAirVolFlowRateUser = VRF( VRFCond ).EvapCondAirVolFlowRate;
						ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Evaporative Condenser Air Flow Rate [m3/s]", EvapCondAirVolFlowRateDes, "User-Specified Evaporative Condenser Air Flow Rate [m3/s]", EvapCondAirVolFlowRateUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( EvapCondAirVolFlowRateDes - EvapCondAirVolFlowRateUser ) / EvapCondAirVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + ' ' + VRFTU( VRFCond ).Name );
								ShowContinueError( "User-Specified Evaporative Condenser Air Flow Rate of " + RoundSigDigits( EvapCondAirVolFlowRateUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Evaporative Condenser Air Flow Rate of " + RoundSigDigits( EvapCondAirVolFlowRateDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}

				IsAutoSize = false;
				if ( VRF( VRFCond ).EvapCondPumpPower == AutoSize ) {
					IsAutoSize = true;
				}
				// Auto size evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
				EvapCondPumpPowerDes = VRF( VRFCond ).CoolingCapacity * 0.004266;
				if ( IsAutoSize ) {
					VRF( VRFCond ).EvapCondPumpPower = EvapCondPumpPowerDes;
					ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Evaporative Condenser Pump Rated Power Consumption [W]", EvapCondPumpPowerDes );

				} else {
					if ( VRF( VRFCond ).EvapCondPumpPower > 0.0 && EvapCondPumpPowerDes > 0.0 ) {
						EvapCondPumpPowerUser = VRF( VRFCond ).EvapCondPumpPower;
						ReportSizingOutput( cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ), VRF( VRFCond ).Name, "Design Size Evaporative Condenser Pump Rated Power Consumption [W]", EvapCondPumpPowerDes, "User-Specified Evaporative Condenser Pump Rated Power Consumption [W]", EvapCondPumpPowerUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( EvapCondPumpPowerDes - EvapCondPumpPowerUser ) / EvapCondPumpPowerUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVRF: Potential issue with equipment sizing for " + cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) + ' ' + VRFTU( VRFCond ).Name );
								ShowContinueError( "User-Specified Evaporative Condenser Pump Rated Power Consumption of " + RoundSigDigits( EvapCondPumpPowerUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Evaporative Condenser Pump Rated Power Consumption of " + RoundSigDigits( EvapCondPumpPowerDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}

				// Report to eio other information not related to autosizing
				if ( MyOneTimeEIOFlag ) {
					gio::write( OutputFileInits, Format_990 );
					MyOneTimeEIOFlag = false;
				}
				gio::write( OutputFileInits, Format_991 ) << cVRFTypes( VRF( VRFCond ).VRFSystemTypeNum ) << VRF( VRFCond ).Name << RoundSigDigits( VRF( VRFCond ).CoolingCombinationRatio, 5 ) << RoundSigDigits( VRF( VRFCond ).HeatingCombinationRatio, 5 ) << RoundSigDigits( VRF( VRFCond ).PipingCorrectionCooling, 5 ) << RoundSigDigits( VRF( VRFCond ).PipingCorrectionHeating, 5 );

				CheckVRFCombinationRatio( VRFCond ) = false;
			}
		}

	}

	void
	SizeVRFCondenser( int const VRFCond )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing VRF Condenser.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the plant sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using ReportSizingManager::ReportSizingOutput;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeVRFCondenser" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizCondNum; // Plant Sizing index for condenser loop
		Real64 rho; // local fluid density [kg/m3]
		Real64 Cp; // local fluid specific heat [J/kg-k]
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate [m3/s]
		bool ErrorsFound; // indicates problem with sizing

		// save the design water flow rate for use by the water loop sizing algorithms
		if ( VRF( VRFCond ).CondenserType == WaterCooled ) {

			ErrorsFound = false;
			PltSizCondNum = 0;

			if ( VRF( VRFCond ).WaterCondVolFlowRate == AutoSize ) {
				if ( VRF( VRFCond ).SourceLoopNum > 0 ) PltSizCondNum = PlantLoop( VRF( VRFCond ).SourceLoopNum ).PlantSizNum;
				if ( PltSizCondNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );

					Cp = GetSpecificHeatGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
					tmpCondVolFlowRate = max( VRF( VRFCond ).CoolingCapacity, VRF( VRFCond ).HeatingCapacity ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
					if( VRF( VRFCond ).HeatingCapacity != AutoSize && VRF( VRFCond ).CoolingCapacity != AutoSize ) {
						VRF( VRFCond ).WaterCondVolFlowRate = tmpCondVolFlowRate;
						ReportSizingOutput( "AirConditioner:VariableRefrigerantFlow", VRF( VRFCond ).Name, "Design Condenser Water Flow Rate [m3/s]", VRF( VRFCond ).WaterCondVolFlowRate );
					}

					rho = GetDensityGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
					VRF( VRFCond ).WaterCondenserDesignMassFlow = VRF( VRFCond ).WaterCondVolFlowRate * rho;
					InitComponentNodes( 0.0, VRF( VRFCond ).WaterCondenserDesignMassFlow, VRF( VRFCond ).CondenserNodeNum, VRF( VRFCond ).CondenserOutletNodeNum, VRF( VRFCond ).SourceLoopNum, VRF( VRFCond ).SourceLoopSideNum, VRF( VRFCond ).SourceBranchNum, VRF( VRFCond ).SourceCompNum );

				} else {
					ShowSevereError( "Autosizing of condenser water flow rate requires a condenser loop Sizing:Plant object" );
					ShowContinueError( "... occurs in AirConditioner:VariableRefrigerantFlow object=" + VRF( VRFCond ).Name );
					ShowContinueError( "... plant loop name must be referenced in Sizing:Plant object" );
					ErrorsFound = true;
				}

			}

			if ( ErrorsFound ) {
				ShowFatalError( "Preceding sizing errors cause program termination" );
			}

			RegisterPlantCompDesignFlow( VRF( VRFCond ).CondenserNodeNum, VRF( VRFCond ).WaterCondVolFlowRate );

		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimVRF(
		int const VRFTUNum,
		bool const FirstHVACIteration,
		Real64 & OnOffAirFlowRatio,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided,
		Real64 const QZnReq
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the VRF TU's.

		// METHODOLOGY EMPLOYED:
		// Simulate terminal unit to meet zone load.

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
		Real64 PartLoadRatio( 1.0 );
		//REAL(r64) :: QZnReq        ! cooling or heating output needed by zone [W]
		//REAL(r64) :: LoadToCoolingSP
		//REAL(r64) :: LoadToHeatingSP

		//  QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
		//  LoadToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP
		//  LoadToHeatingSP = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP
		//  IF(QZnReq == 0.0d0 .AND. HeatingLoad(VRFTU(VRFTUNum)%VRFSysNum))QZnReq = LoadToHeatingSP
		//  IF(QZnReq == 0.0d0 .AND. CoolingLoad(VRFTU(VRFTUNum)%VRFSysNum))QZnReq = LoadToCoolingSP

		if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
		// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
			ControlVRF_FluidTCtrl( VRFTUNum, QZnReq, FirstHVACIteration, PartLoadRatio, OnOffAirFlowRatio );
			CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, PartLoadRatio, SysOutputProvided, OnOffAirFlowRatio, LatOutputProvided );
			//CalcVRF( VRFTUNum, FirstHVACIteration, PartLoadRatio, SysOutputProvided, OnOffAirFlowRatio, LatOutputProvided );
		} else {
		// Algorithm Type: VRF model based on system curve
			ControlVRF( VRFTUNum, QZnReq, FirstHVACIteration, PartLoadRatio, OnOffAirFlowRatio );
			CalcVRF( VRFTUNum, FirstHVACIteration, PartLoadRatio, SysOutputProvided, OnOffAirFlowRatio, LatOutputProvided );
		}

		VRFTU( VRFTUNum ).TerminalUnitSensibleRate = SysOutputProvided;
		VRFTU( VRFTUNum ).TerminalUnitLatentRate = LatOutputProvided;

	}

	void
	ControlVRF(
		int const VRFTUNum, // Index to VRF terminal unit
		Real64 const QZnReq, // Index to zone number
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 & PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
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
		using HeatingCoils::SimulateHeatingCoilComponents;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // maximum number of iterations
		Real64 const MinPLF( 0.0 ); // minimum part load factor allowed
		Real64 const ErrorTol( 0.001 ); // tolerance for RegulaFalsi iterations
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FullOutput; // unit full output when compressor is operating [W]
		Real64 TempOutput; // unit output when iteration limit exceeded [W]
		Real64 NoCompOutput; // output when no active compressor [W]
		int SolFla; // Flag of RegulaFalsi solver
		Array1D< Real64 > Par( 6 ); // Parameters passed to RegulaFalsi
		std::string IterNum; // Max number of iterations for warning message
		Real64 TempMinPLR; // min PLR used in Regula Falsi call
		Real64 TempMaxPLR; // max PLR used in Regula Falsi call
		bool ContinueIter; // used when convergence is an issue
		int VRFCond; // index to VRF condenser
		int IndexToTUInTUList; // index to TU in specific list for the VRF system
		int TUListIndex; // index to TU list for this VRF system
		bool VRFCoolingMode;
		bool VRFHeatingMode;
		bool HRCoolingMode;
		bool HRHeatingMode;

		PartLoadRatio = 0.0;
		LoopDXCoolCoilRTF = 0.0;
		LoopDXHeatCoilRTF = 0.0;
		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		VRFCoolingMode = CoolingLoad( VRFCond );
		VRFHeatingMode = HeatingLoad( VRFCond );
		HRCoolingMode = TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList );
		HRHeatingMode = TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList );

		// The RETURNS here will jump back to SimVRF where the CalcVRF routine will simulate with lastest PLR

		// do nothing else if TU is scheduled off
		//!!LKL Discrepancy < 0
		if ( GetCurrentScheduleValue( VRFTU( VRFTUNum ).SchedPtr ) == 0.0 ) return;

		// do nothing if TU has no load (TU will be modeled using PLR=0)
		if ( QZnReq == 0.0 ) return;

		// Set EMS value for PLR and return
		if ( VRFTU( VRFTUNum ).EMSOverridePartLoadFrac ) {
			PartLoadRatio = VRFTU( VRFTUNum ).EMSValueForPartLoadFrac;
			return;
		}

		// Get result when DX coil is off
		PartLoadRatio = 0.0;

		if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
		// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
			CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, NoCompOutput, OnOffAirFlowRatio );
		} else {
		// Algorithm Type: VRF model based on system curve
			CalcVRF( VRFTUNum, FirstHVACIteration, 0.0, NoCompOutput, OnOffAirFlowRatio );
		}

		if ( VRFCoolingMode && HRHeatingMode ) {
			// IF the system is in cooling mode, but the terminal unit requests heating (heat recovery)
			if ( NoCompOutput >= QZnReq ) return;
		} else if ( VRFHeatingMode && HRCoolingMode ) {
			// IF the system is in heating mode, but the terminal unit requests cooling (heat recovery)
			if ( NoCompOutput <= QZnReq ) return;
		} else if ( VRFCoolingMode || HRCoolingMode ) {
			// IF the system is in cooling mode and/or the terminal unit requests cooling
			if ( NoCompOutput <= QZnReq ) return;
		} else if ( VRFHeatingMode || HRHeatingMode ) {
			// IF the system is in heating mode and/or the terminal unit requests heating
			if ( NoCompOutput >= QZnReq ) return;
		}

		// Otherwise the coil needs to turn on. Get full load result
		PartLoadRatio = 1.0;
		if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
		// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
			CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, PartLoadRatio, FullOutput, OnOffAirFlowRatio );
		} else {
		// Algorithm Type: VRF model based on system curve
			CalcVRF( VRFTUNum, FirstHVACIteration, PartLoadRatio, FullOutput, OnOffAirFlowRatio );
		}
		PartLoadRatio = 0.0;

		if ( ( VRFCoolingMode && ! VRF( VRFCond ).HeatRecoveryUsed ) || ( VRF( VRFCond ).HeatRecoveryUsed && HRCoolingMode ) ) {
			// Since we are cooling, we expect FullOutput < NoCompOutput
			// If the QZnReq <= FullOutput the unit needs to run full out
			if ( QZnReq <= FullOutput ) {
				// if no coil present in terminal unit, no need to reset PLR?
				if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) PartLoadRatio = 1.0;
				return;
			}
		} else if ( ( VRFHeatingMode && ! VRF( VRFCond ).HeatRecoveryUsed ) || ( VRF( VRFCond ).HeatRecoveryUsed && HRHeatingMode ) ) {
			// Since we are heating, we expect FullOutput > NoCompOutput
			// If the QZnReq >= FullOutput the unit needs to run full out
			if ( QZnReq >= FullOutput ) {
				// if no coil present in terminal unit, no need reset PLR?
				if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) PartLoadRatio = 1.0;
				return;
			}
		} else {
			// VRF terminal unit is off, PLR already set to 0 above
			// shouldn't actually get here
			return;
		}

		// The coil will not operate at PLR=0 or PLR=1, calculate the operating part-load ratio

		if ( ( VRFHeatingMode || HRHeatingMode ) || ( VRFCoolingMode || HRCoolingMode ) ) {

			Par( 1 ) = VRFTUNum;
			Par( 2 ) = 0.0;
			Par( 4 ) = 0.0;
			if ( FirstHVACIteration ) {
				Par( 3 ) = 1.0;
			} else {
				Par( 3 ) = 0.0;
			}
			//    Par(4) = OpMode
			Par( 5 ) = QZnReq;
			Par( 6 ) = OnOffAirFlowRatio;
			SolveRegulaFalsi( ErrorTol, MaxIte, SolFla, PartLoadRatio, PLRResidual, 0.0, 1.0, Par );
			if ( SolFla == -1 ) {
				//     Very low loads may not converge quickly. Tighten PLR boundary and try again.
				TempMaxPLR = -0.1;
				ContinueIter = true;
				while ( ContinueIter && TempMaxPLR < 1.0 ) {
					TempMaxPLR += 0.1;

					if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
					// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
						CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, TempMaxPLR, TempOutput, OnOffAirFlowRatio );
					} else {
					// Algorithm Type: VRF model based on system curve
						CalcVRF( VRFTUNum, FirstHVACIteration, TempMaxPLR, TempOutput, OnOffAirFlowRatio );
					}

					if ( VRFHeatingMode && TempOutput > QZnReq ) ContinueIter = false;
					if ( VRFCoolingMode && TempOutput < QZnReq ) ContinueIter = false;
				}
				TempMinPLR = TempMaxPLR;
				ContinueIter = true;
				while ( ContinueIter && TempMinPLR > 0.0 ) {
					TempMaxPLR = TempMinPLR;
					TempMinPLR -= 0.01;

					if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
					// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
						CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, TempMaxPLR, TempOutput, OnOffAirFlowRatio );
					} else {
					// Algorithm Type: VRF model based on system curve
						CalcVRF( VRFTUNum, FirstHVACIteration, TempMaxPLR, TempOutput, OnOffAirFlowRatio );
					}

					if ( VRFHeatingMode && TempOutput < QZnReq ) ContinueIter = false;
					if ( VRFCoolingMode && TempOutput > QZnReq ) ContinueIter = false;
				}
				SolveRegulaFalsi( ErrorTol, MaxIte, SolFla, PartLoadRatio, PLRResidual, TempMinPLR, TempMaxPLR, Par );
				if ( SolFla == -1 ) {
					if ( ! FirstHVACIteration && ! WarmupFlag ) {
						if ( VRFTU( VRFTUNum ).IterLimitExceeded == 0 ) {
							gio::write( IterNum, fmtLD ) << MaxIte;
							strip( IterNum );
							ShowWarningMessage( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( " Iteration limit exceeded calculating terminal unit part-load ratio, maximum iterations = " + IterNum );
							ShowContinueErrorTimeStamp( " Part-load ratio returned = " + RoundSigDigits( PartLoadRatio, 3 ) );

							if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
							// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
								CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, TempMinPLR, TempOutput, OnOffAirFlowRatio );
							} else {
							// Algorithm Type: VRF model based on system curve
								CalcVRF( VRFTUNum, FirstHVACIteration, TempMinPLR, TempOutput, OnOffAirFlowRatio );
							}

							ShowContinueError( " Load requested = " + TrimSigDigits( QZnReq, 5 ) + ", Load delivered = " + TrimSigDigits( TempOutput, 5 ) );
							ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit Iteration limit exceeded error continues...", VRFTU( VRFTUNum ).IterLimitExceeded );
						} else {
							ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit Iteration limit exceeded error continues...", VRFTU( VRFTUNum ).IterLimitExceeded );
						}
					}
				} else if ( SolFla == -2 ) {
					if ( ! FirstHVACIteration && ! WarmupFlag ) {
						if ( VRFTU( VRFTUNum ).FirstIterfailed == 0 ) {
							ShowWarningMessage( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( "Terminal unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded" );
							ShowContinueError( "Please fill out a bug report and forward to the EnergyPlus support group." );
							ShowContinueErrorTimeStamp( "" );
							if ( WarmupFlag ) ShowContinueError( "Error occurred during warmup days." );
							ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...", VRFTU( VRFTUNum ).FirstIterfailed );
						} else {
							ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...", VRFTU( VRFTUNum ).FirstIterfailed );
						}
					}
					PartLoadRatio = max( MinPLF, std::abs( QZnReq - NoCompOutput ) / std::abs( FullOutput - NoCompOutput ) );
				}
			} else if ( SolFla == -2 ) {
				if ( ! FirstHVACIteration && ! WarmupFlag ) {
					if ( VRFTU( VRFTUNum ).FirstIterfailed == 0 ) {
						ShowWarningMessage( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
						ShowContinueError( "Terminal unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded" );
						ShowContinueError( "Please fill out a bug report and forward to the EnergyPlus support group." );
						ShowContinueErrorTimeStamp( "" );
						if ( WarmupFlag ) ShowContinueError( "Error occurred during warmup days." );
						ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...", VRFTU( VRFTUNum ).FirstIterfailed );
					} else {
						ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...", VRFTU( VRFTUNum ).FirstIterfailed );
					}
				}
				if ( FullOutput - NoCompOutput == 0.0 ) {
					PartLoadRatio = 0.0;
				} else {
					PartLoadRatio = min( 1.0, max( MinPLF, std::abs( QZnReq - NoCompOutput ) / std::abs( FullOutput - NoCompOutput ) ) );
				}
			}

		}

	}

	void
	CalcVRF(
		int const VRFTUNum, // Unit index in VRF terminal unit array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 const PartLoadRatio, // compressor part load fraction
		Real64 & LoadMet, // load met by unit (W)
		Real64 & OnOffAirFlowRatio, // ratio of ON air flow to average air flow
		Optional< Real64 > LatOutputProvided // delivered latent capacity (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the VRF terminal unit.

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
		using DataSizing::AutoSize;
		//  USE WaterToAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
		using DataAirLoop::LoopDXCoilRTF;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int VRFTUOutletNodeNum; // TU air outlet node
		int VRFTUInletNodeNum; // TU air inlet node
		Real64 AirMassFlow; // total supply air mass flow [m3/s]
		Real64 MinHumRat; // minimum humidity ratio for sensible capacity calculation (kg/kg)
		int OpMode; // fan operating mode, CycFanCycCoil or ContFanCycCoil
		int VRFCond; // index to VRF condenser
		Real64 SpecHumOut; // specific humidity ratio at outlet node
		Real64 SpecHumIn; // specific humidity ratio at inlet node
		int TUListIndex; // index to TU list for this VRF system
		int IndexToTUInTUList; // index to TU in specific list for the VRF system

		// FLOW

		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		VRFTUOutletNodeNum = VRFTU( VRFTUNum ).VRFTUOutletNodeNum;
		VRFTUInletNodeNum = VRFTU( VRFTUNum ).VRFTUInletNodeNum;
		OpMode = VRFTU( VRFTUNum ).OpMode;

		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		SetAverageAirFlow( VRFTUNum, PartLoadRatio, OnOffAirFlowRatio );

		AirMassFlow = Node( VRFTUInletNodeNum ).MassFlowRate;
		if ( VRFTU( VRFTUNum ).OAMixerUsed ) SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );

		// if blow through, simulate fan then coils
		if ( VRFTU( VRFTUNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( "", FirstHVACIteration, VRFTU( VRFTUNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {
			// above condition for heat pump mode, below condition for heat recovery mode
			if ( ( ! VRF( VRFCond ).HeatRecoveryUsed && CoolingLoad( VRFCond ) ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) ) {
				SimDXCoil( "", On, FirstHVACIteration, VRFTU( VRFTUNum ).CoolCoilIndex, OpMode, PartLoadRatio, OnOffAirFlowRatio, _, MaxCoolingCapacity( VRFCond ), VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFCondCyclingRatio );
			} else { // cooling coil is off
				SimDXCoil( "", Off, FirstHVACIteration, VRFTU( VRFTUNum ).CoolCoilIndex, OpMode, 0.0, OnOffAirFlowRatio );
			}
			LoopDXCoolCoilRTF = LoopDXCoilRTF;
		} else {
			LoopDXCoolCoilRTF = 0.0;
		}

		if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
			// above condition for heat pump mode, below condition for heat recovery mode
			if ( ( ! VRF( VRFCond ).HeatRecoveryUsed && HeatingLoad( VRFCond ) ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) ) {
				SimDXCoil( "", Off, FirstHVACIteration, VRFTU( VRFTUNum ).HeatCoilIndex, OpMode, PartLoadRatio, OnOffAirFlowRatio, _, MaxHeatingCapacity( VRFCond ) );
			} else {
				SimDXCoil( "", Off, FirstHVACIteration, VRFTU( VRFTUNum ).HeatCoilIndex, OpMode, 0.0, OnOffAirFlowRatio );
			}
			LoopDXHeatCoilRTF = LoopDXCoilRTF;
		} else {
			LoopDXHeatCoilRTF = 0.0;
		}

		// if draw through, simulate coils then fan
		if ( VRFTU( VRFTUNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( "", FirstHVACIteration, VRFTU( VRFTUNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		// track fan power per terminal unit for calculating COP
		VRFTU( VRFTUNum ).FanPower = FanElecPower;

		// calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio
		MinHumRat = min( Node( VRFTUInletNodeNum ).HumRat, Node( VRFTUOutletNodeNum ).HumRat );
		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( VRFTUOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( VRFTUInletNodeNum ).Temp, MinHumRat ) );

		if ( present( LatOutputProvided ) ) {
			//   CR9155 Remove specific humidity calculations
			SpecHumOut = Node( VRFTUOutletNodeNum ).HumRat;
			SpecHumIn = Node( VRFTUInletNodeNum ).HumRat;
			LatOutputProvided = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate, kg/s (dehumid = negative)
		}

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines
	// *****************************************************************************

	//SUBROUTINE UpdateVRF()

	//          ! SUBROUTINE INFORMATION:
	//          !       AUTHOR         Richard Raustad, FSEC
	//          !       DATE WRITTEN   August 2010
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS SUBROUTINE:
	//          ! This subroutine updates the fan outlet nodes.

	//          ! METHODOLOGY EMPLOYED:
	//          ! Data is moved from the fan data structure to the fan outlet nodes.

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//          ! na

	//  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

	//          ! SUBROUTINE ARGUMENT DEFINITIONS:
	//          ! na

	//          ! SUBROUTINE PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS
	//          ! na

	//          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	//          ! na

	//  RETURN
	//END Subroutine UpdateVRF

	//        End of Update subroutines for the Fan Module
	// *****************************************************************************

	// Beginning of Reporting subroutines
	// *****************************************************************************

	void
	ReportVRFTerminalUnit( int const VRFTUNum ) // index to VRF terminal unit
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variables for the VRF Terminal Units.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::DXCoilTotalCooling;
		using DXCoils::DXCoilTotalHeating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoolingCoilIndex; // - index to DX cooling coil
		int DXHeatingCoilIndex; // - index to DX heating coil
		Real64 TotalConditioning; // - sum of sensible and latent rates
		Real64 SensibleConditioning; // - sensible rate
		Real64 LatentConditioning; // - latent rate
		Real64 ReportingConstant; // - used to convert watts to joules
		int VRFCond; // - index to VRF condenser
		Real64 H2OHtOfVap; // - Heat of vaporization of air (J/kg)
		int TUListIndex; // - index to terminal unit list
		int IndexToTUInTUList; // - index to the TU in the list
		bool HRHeatRequestFlag; // - indicates TU could be in heat mode
		bool HRCoolRequestFlag; // - indicates TU could be in cool mode

		DXCoolingCoilIndex = VRFTU( VRFTUNum ).CoolCoilIndex;
		DXHeatingCoilIndex = VRFTU( VRFTUNum ).HeatCoilIndex;
		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		HRHeatRequestFlag = TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList );
		HRCoolRequestFlag = TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList );
		ReportingConstant = TimeStepSys * SecInHour;

		// account for terminal unit parasitic On/Off power use
		// account for heat recovery first since these flags will be FALSE otherwise, each TU may have different operating mode

		if ( HRCoolRequestFlag ) {
			if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = VRFTU( VRFTUNum ).ParasiticElec * LoopDXCoolCoilRTF + VRFTU( VRFTUNum ).ParasiticOffElec * ( 1.0 - LoopDXCoolCoilRTF );
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = VRFTU( VRFTUNum ).ParasiticCoolElecPower * ReportingConstant;
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = 0.0;
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = 0.0;
			} else {
				// cooling parasitic power report variable is not even available when there is no cooling coil, report for heating
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = VRFTU( VRFTUNum ).ParasiticOffElec;
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = VRFTU( VRFTUNum ).ParasiticHeatElecPower * ReportingConstant;
			}
		} else if ( HRHeatRequestFlag ) {
			if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = 0.0;
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = 0.0;
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = VRFTU( VRFTUNum ).ParasiticElec * LoopDXHeatCoilRTF + VRFTU( VRFTUNum ).ParasiticOffElec * ( 1.0 - LoopDXHeatCoilRTF );
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = VRFTU( VRFTUNum ).ParasiticHeatElecPower * ReportingConstant;
			} else {
				// heating parasitic power report variable is not even available when there is no heating coil, report for cooling
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = VRFTU( VRFTUNum ).ParasiticOffElec;
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = VRFTU( VRFTUNum ).ParasiticCoolElecPower * ReportingConstant;
			}
		} else if ( CoolingLoad( VRFCond ) || ( ! HeatingLoad( VRFCond ) && LastModeCooling( VRFTU( VRFTUNum ).VRFSysNum ) ) ) {
			if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = VRFTU( VRFTUNum ).ParasiticElec * LoopDXCoolCoilRTF + VRFTU( VRFTUNum ).ParasiticOffElec * ( 1.0 - LoopDXCoolCoilRTF );
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = VRFTU( VRFTUNum ).ParasiticCoolElecPower * ReportingConstant;
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = 0.0;
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = 0.0;
			} else {
				// cooling parasitic power report variable is not even available when there is no cooling coil, report for heating
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = VRFTU( VRFTUNum ).ParasiticOffElec;
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = VRFTU( VRFTUNum ).ParasiticHeatElecPower * ReportingConstant;
			}
		} else if ( HeatingLoad( VRFCond ) || ( ! CoolingLoad( VRFCond ) && LastModeHeating( VRFTU( VRFTUNum ).VRFSysNum ) ) ) {
			if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = 0.0;
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = 0.0;
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = VRFTU( VRFTUNum ).ParasiticElec * LoopDXHeatCoilRTF + VRFTU( VRFTUNum ).ParasiticOffElec * ( 1.0 - LoopDXHeatCoilRTF );
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = VRFTU( VRFTUNum ).ParasiticHeatElecPower * ReportingConstant;
			} else {
				// heating parasitic power report variable is not even available when there is no heating coil, report for cooling
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = VRFTU( VRFTUNum ).ParasiticOffElec;
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = VRFTU( VRFTUNum ).ParasiticCoolElecPower * ReportingConstant;
			}
		} else {
			// happens when there is no cooling or heating load
			if ( ! VRFTU( VRFTUNum ).CoolingCoilPresent ) {
				// report all for heating
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = VRFTU( VRFTUNum ).ParasiticOffElec;
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = VRFTU( VRFTUNum ).ParasiticHeatElecPower * ReportingConstant;
			} else if ( ! VRFTU( VRFTUNum ).HeatingCoilPresent ) {
				// report all for cooling
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = VRFTU( VRFTUNum ).ParasiticOffElec;
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = VRFTU( VRFTUNum ).ParasiticCoolElecPower * ReportingConstant;
			} else {
				// split parasitic between both reporting variables
				VRFTU( VRFTUNum ).ParasiticCoolElecPower = VRFTU( VRFTUNum ).ParasiticOffElec / 2.0;
				VRFTU( VRFTUNum ).ParasiticElecCoolConsumption = VRFTU( VRFTUNum ).ParasiticCoolElecPower * ReportingConstant;
				VRFTU( VRFTUNum ).ParasiticHeatElecPower = VRFTU( VRFTUNum ).ParasiticOffElec / 2.0;
				VRFTU( VRFTUNum ).ParasiticElecHeatConsumption = VRFTU( VRFTUNum ).ParasiticHeatElecPower * ReportingConstant;
			}
		}

		SensibleConditioning = VRFTU( VRFTUNum ).TerminalUnitSensibleRate;
		LatentConditioning = VRFTU( VRFTUNum ).TerminalUnitLatentRate;
		// convert latent in kg/s to watts
		H2OHtOfVap = PsyHfgAirFnWTdb( Node( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ).HumRat, Node( VRFTU( VRFTUNum ).VRFTUOutletNodeNum ).Temp );
		TotalConditioning = SensibleConditioning + ( LatentConditioning * H2OHtOfVap );

		if ( TotalConditioning <= 0.0 ) {
			VRFTU( VRFTUNum ).TotalCoolingRate = std::abs( TotalConditioning );
			VRFTU( VRFTUNum ).TotalHeatingRate = 0.0;
		} else {
			VRFTU( VRFTUNum ).TotalCoolingRate = 0.0;
			VRFTU( VRFTUNum ).TotalHeatingRate = TotalConditioning;
		}
		if ( SensibleConditioning <= 0.0 ) {
			VRFTU( VRFTUNum ).SensibleCoolingRate = std::abs( SensibleConditioning );
			VRFTU( VRFTUNum ).SensibleHeatingRate = 0.0;
		} else {
			VRFTU( VRFTUNum ).SensibleCoolingRate = 0.0;
			VRFTU( VRFTUNum ).SensibleHeatingRate = SensibleConditioning;
		}
		if ( LatentConditioning <= 0.0 ) {
			VRFTU( VRFTUNum ).LatentCoolingRate = std::abs( LatentConditioning ) * H2OHtOfVap;
			VRFTU( VRFTUNum ).LatentHeatingRate = 0.0;
		} else {
			VRFTU( VRFTUNum ).LatentCoolingRate = 0.0;
			VRFTU( VRFTUNum ).LatentHeatingRate = LatentConditioning * H2OHtOfVap;
		}
		VRFTU( VRFTUNum ).TotalCoolingEnergy = VRFTU( VRFTUNum ).TotalCoolingRate * ReportingConstant;
		VRFTU( VRFTUNum ).SensibleCoolingEnergy = VRFTU( VRFTUNum ).SensibleCoolingRate * ReportingConstant;
		VRFTU( VRFTUNum ).LatentCoolingEnergy = VRFTU( VRFTUNum ).LatentCoolingRate * ReportingConstant;
		VRFTU( VRFTUNum ).TotalHeatingEnergy = VRFTU( VRFTUNum ).TotalHeatingRate * ReportingConstant;
		VRFTU( VRFTUNum ).SensibleHeatingEnergy = VRFTU( VRFTUNum ).SensibleHeatingRate * ReportingConstant;
		VRFTU( VRFTUNum ).LatentHeatingEnergy = VRFTU( VRFTUNum ).LatentHeatingRate * ReportingConstant;

	}

	void
	ReportVRFCondenser( int const VRFCond ) // index to VRF condensing unit
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variables for the VRF Condenser.

		// METHODOLOGY EMPLOYED:
		// na

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
		Real64 ReportingConstant; // - conversion constant for energy

		ReportingConstant = TimeStepSys * SecInHour;

		//   calculate VRF condenser power/energy use
		VRF( VRFCond ).CoolElecConsumption = VRF( VRFCond ).ElecCoolingPower * ReportingConstant;
		VRF( VRFCond ).HeatElecConsumption = VRF( VRFCond ).ElecHeatingPower * ReportingConstant;

		VRF( VRFCond ).DefrostConsumption = VRF( VRFCond ).DefrostPower * ReportingConstant;
		VRF( VRFCond ).BasinHeaterConsumption = VRF( VRFCond ).BasinHeaterPower * ReportingConstant;

		VRF( VRFCond ).EvapCondPumpElecConsumption = VRF( VRFCond ).EvapCondPumpElecPower * ReportingConstant;
		VRF( VRFCond ).CrankCaseHeaterElecConsumption = VRF( VRFCond ).CrankCaseHeaterPower * ReportingConstant;

		VRF( VRFCond ).QCondEnergy = VRF( VRFCond ).QCondenser * ReportingConstant;

	}

	void
	UpdateVRFCondenser( int const VRFCond ) // index to VRF condensing unit
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   May 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the node data for the VRF Condenser.

		// METHODOLOGY EMPLOYED:
		// na

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
		//    INTEGER :: CondenserInletNode  !- inlet node for VRF water-cooled condenser
		int CondenserOutletNode; // - outlet node for VRF water-cooled condenser

		//    CondenserInletNode = VRF(VRFCond)%CondenserNodeNum
		CondenserOutletNode = VRF( VRFCond ).CondenserOutletNodeNum;

		Node( CondenserOutletNode ).Temp = VRF( VRFCond ).CondenserSideOutletTemp;

		//    Node(CondenserInletNode)%MassFlowRate = CondenserWaterMassFlowRate
		Node( CondenserOutletNode ).MassFlowRate = CondenserWaterMassFlowRate;

		Node( CondenserOutletNode ).MassFlowRateMaxAvail = Node( CondenserOutletNode ).MassFlowRateMaxAvail;
		Node( CondenserOutletNode ).MassFlowRateMinAvail = Node( CondenserOutletNode ).MassFlowRateMinAvail;

	}

	//        End of Reporting subroutines for the Module
	// *****************************************************************************

	// Utility subroutines for the Module

	Real64
	PLRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = VRFTUNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function ((ActualOutput - QZnReq) /QZnReq)
		//  VRF TU output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcVRF to get ActualOutput at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 PLRResidual;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = Not used
		// par(3) = FirstHVACIteration
		// par(4) = OpMode
		// par(5) = QZnReq
		// par(6) = OnOffAirFlowRatio

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int VRFTUNum; // TU index
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Compressor operating mode
		Real64 QZnReq; // zone load (W)
		Real64 QZnReqTemp; // denominator representing zone load (W)
		Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
		Real64 ActualOutput; // delivered capacity of VRF terminal unit

		VRFTUNum = int( Par( 1 ) );
		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		OpMode = int( Par( 4 ) );
		QZnReq = Par( 5 );
		QZnReqTemp = QZnReq;
		if ( std::abs( QZnReq ) < 100.0 ) QZnReqTemp = sign( 100.0, QZnReq );
		OnOffAirFlowRatio = Par( 6 );

		if ( VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
		// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
			CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, PartLoadRatio, ActualOutput, OnOffAirFlowRatio );
		} else {
		// Algorithm Type: VRF model based on system curve
			CalcVRF( VRFTUNum, FirstHVACIteration, PartLoadRatio, ActualOutput, OnOffAirFlowRatio );
		}

		PLRResidual = ( ActualOutput - QZnReq ) / QZnReqTemp;

		return PLRResidual;
	}

	void
	SetAverageAirFlow(
		int const VRFTUNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to average airflow over timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2010
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the average air mass flow rates using the part load fraction of the heat pump for this time step
		// Set OnOffAirFlowRatio to be used by DX coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEquipment::VRFTerminalUnit_Num;

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
		int OutsideAirNode; // outside air node number
		int AirRelNode; // relief air node number
		Real64 AverageUnitMassFlow( 0.0 ); // average supply air mass flow rate over time step
		Real64 AverageOAMassFlow( 0.0 ); // average outdoor air mass flow rate over time step

		InletNode = VRFTU( VRFTUNum ).VRFTUInletNodeNum;
		OutsideAirNode = VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum;
		AirRelNode = VRFTU( VRFTUNum ).VRFTUOAMixerRelNodeNum;

		if ( VRFTU( VRFTUNum ).OpMode == CycFanCycCoil ) {
			AverageUnitMassFlow = ( PartLoadRatio * CompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * CompOffMassFlow );
			AverageOAMassFlow = ( PartLoadRatio * OACompOnMassFlow ) + ( ( 1 - PartLoadRatio ) * OACompOffMassFlow );
		} else {
			if ( PartLoadRatio == 0.0 ) {
				// set the average OA air flow to off compressor values if the compressor PartLoadRatio is zero
				AverageUnitMassFlow = CompOffMassFlow;
				AverageOAMassFlow = OACompOffMassFlow;
			} else {
				AverageUnitMassFlow = CompOnMassFlow;
				AverageOAMassFlow = OACompOnMassFlow;
			}
		}
		if ( CompOffFlowRatio > 0.0 ) {
			FanSpeedRatio = ( PartLoadRatio * CompOnFlowRatio ) + ( ( 1 - PartLoadRatio ) * CompOffFlowRatio );
		} else {
			FanSpeedRatio = CompOnFlowRatio;
		}

		// if the terminal unit and fan are scheduled on then set flow rate
		if ( GetCurrentScheduleValue( VRFTU( VRFTUNum ).SchedPtr ) > 0.0 && ( GetCurrentScheduleValue( VRFTU( VRFTUNum ).FanAvailSchedPtr ) > 0.0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOff ) {

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

		} else { // terminal unit and/or fan is off

			Node( InletNode ).MassFlowRate = 0.0;
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = 0.0;
				Node( AirRelNode ).MassFlowRate = 0.0;
			}
			OnOffAirFlowRatio = 0.0;

		}
	}

	void
	InitializeOperatingMode(
		bool const FirstHVACIteration, // flag for first time through HVAC systems
		int const VRFCond, // Condenser Unit index
		int const TUListNum, // Condenser Unit terminal unit list
		Real64 & OnOffAirFlowRatio // ratio of on to off flow rate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2012 (Moved from InitVRF)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Scans each zone coil and determines the load based on control
		// Moved from Init to clean up and localize code segments

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::TempControlType;
		using DataHeatBalFanSys::ZT;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using ScheduleManager::GetCurrentScheduleValue;
		using MixedAir::SimOAMixer;
		using MixedAir::SimOAController;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneDeltaT; // zone temperature difference from setpoint
		Real64 SPTempHi; // thermostat setpoint high
		Real64 SPTempLo; // thermostat setpoint low
		int NumTU; // loop counter, number of TU's in list
		int TUIndex; // index to TU
		int ThisZoneNum; // index to zone number where TU is located
		Real64 ZoneLoad; // current zone load (W)
		Real64 LoadToCoolingSP; // thermostat load to cooling setpoint (W)
		Real64 LoadToHeatingSP; // thermostat load to heating setpoint (W)
		Real64 TempOutput; // terminal unit output [W]

		MaxDeltaT = 0.0;
		MinDeltaT = 0.0;
		NumCoolingLoads = 0;
		SumCoolingLoads = 0.0;
		NumHeatingLoads = 0;
		SumHeatingLoads = 0.0;

		NumCoolingLoads( VRFCond ) = 0;
		NumHeatingLoads( VRFCond ) = 0;
		SumCoolingLoads( VRFCond ) = 0.0;
		SumHeatingLoads( VRFCond ) = 0.0;
		MaxDeltaT( VRFCond ) = 0.0;
		MinDeltaT( VRFCond ) = 0.0;
		ZoneDeltaT = 0.0;
		HeatingLoad( VRFCond ) = false;
		CoolingLoad( VRFCond ) = false;
		TerminalUnitList( TUListNum ).CoolingCoilAvailable = false;
		TerminalUnitList( TUListNum ).HeatingCoilAvailable = false;
		// loop through all TU's to find operating mode. Be carefull not to mix loop counters with current TU/Cond index
		for ( NumTU = 1; NumTU <= TerminalUnitList( TUListNum ).NumTUInList; ++NumTU ) {
			// make sure TU's have been sized before looping through each one of them to determine operating mode
			// (which would size all coils based on the zone that called this specific VRF terminal unit)
			if ( any( TerminalUnitList( TUListNum ).TerminalUnitNotSizedYet ) ) break;
			TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
			ThisZoneNum = VRFTU( TUIndex ).ZoneNum;

			//       check to see if coil is present
			if ( TerminalUnitList( TUListNum ).CoolingCoilPresent( NumTU ) ) {
				//         now check to see if coil is scheduled off
				if ( GetCurrentScheduleValue( TerminalUnitList( TUListNum ).CoolingCoilAvailSchPtr( NumTU ) ) > 0.0 ) {
					TerminalUnitList( TUListNum ).CoolingCoilAvailable( NumTU ) = true;
				}
			}

			//       check to see if coil is present
			if ( TerminalUnitList( TUListNum ).HeatingCoilPresent( NumTU ) ) {
				//         now check to see if coil is scheduled off
				if ( GetCurrentScheduleValue( TerminalUnitList( TUListNum ).HeatingCoilAvailSchPtr( NumTU ) ) > 0.0 ) {
					TerminalUnitList( TUListNum ).HeatingCoilAvailable( NumTU ) = true;
				}
			}

			//     Constant fan systems are tested for ventilation load to determine if load to be met changes.
			//     more logic may be needed here, what is the OA flow rate, was last mode heating or cooling, what control is used, etc...
			ZoneLoad = ZoneSysEnergyDemand( ThisZoneNum ).RemainingOutputRequired;
			if ( VRF( VRFCond ).ThermostatPriority == ThermostatOffsetPriority ) {
				//         for TSTATPriority, just check difference between zone temp and thermostat setpoint
				if ( ThisZoneNum > 0 ) {
					SPTempHi = ZoneThermostatSetPointHi( ThisZoneNum );
					SPTempLo = ZoneThermostatSetPointLo( ThisZoneNum );
					{ auto const SELECT_CASE_var( TempControlType( ThisZoneNum ) );
					if ( SELECT_CASE_var == 0 ) { // Uncontrolled
					// MaxDeltaT denotes cooling, MinDeltaT denotes heating
					} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
						// if heating load, ZoneDeltaT will be negative
						ZoneDeltaT = min( 0.0, ZT( ThisZoneNum ) - SPTempLo );
						MinDeltaT( VRFCond ) = min( MinDeltaT( VRFCond ), ZoneDeltaT );
					} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
						// if cooling load, ZoneDeltaT will be positive
						ZoneDeltaT = max( 0.0, ZT( ThisZoneNum ) - SPTempHi );
						MaxDeltaT( VRFCond ) = max( MaxDeltaT( VRFCond ), ZoneDeltaT );
					} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
						ZoneDeltaT = ZT( ThisZoneNum ) - SPTempHi; //- SPTempHi and SPTempLo are same value
						if ( ZoneDeltaT > 0.0 ) {
							MaxDeltaT( VRFCond ) = max( MaxDeltaT( VRFCond ), ZoneDeltaT );
						} else {
							MinDeltaT( VRFCond ) = min( MinDeltaT( VRFCond ), ZoneDeltaT );
						}
					} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
						if ( ZT( ThisZoneNum ) - SPTempHi > 0.0 ) {
							ZoneDeltaT = max( 0.0, ZT( ThisZoneNum ) - SPTempHi );
							MaxDeltaT( VRFCond ) = max( MaxDeltaT( VRFCond ), ZoneDeltaT );
						} else if ( SPTempLo - ZT( ThisZoneNum ) > 0.0 ) {
							ZoneDeltaT = min( 0.0, ZT( ThisZoneNum ) - SPTempLo );
							MinDeltaT( VRFCond ) = min( MinDeltaT( VRFCond ), ZoneDeltaT );
						}
					} else {
					}}
				}
			} else if ( VRF( VRFCond ).ThermostatPriority == LoadPriority || VRF( VRFCond ).ThermostatPriority == ZonePriority ) {
				if ( VRFTU( TUIndex ).OpMode == ContFanCycCoil ) {
					SetCompFlowRate( TUIndex, VRFCond );

					if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
					// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
						CalcVRF_FluidTCtrl( TUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
					} else {
					// Algorithm Type: VRF model based on system curve
						CalcVRF( TUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
					}

					LoadToCoolingSP = ZoneSysEnergyDemand( ThisZoneNum ).OutputRequiredToCoolingSP;
					LoadToHeatingSP = ZoneSysEnergyDemand( ThisZoneNum ).OutputRequiredToHeatingSP;
					//           If the Terminal Unit has a net cooling capacity (NoCompOutput < 0) and
					//           the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
					if ( TempOutput < 0.0 && LoadToHeatingSP < 0.0 ) {
						//             If the net cooling capacity overshoots the heating setpoint count as heating load
						if ( TempOutput < LoadToHeatingSP ) {
							//               Don't count as heating load unless mode is allowed. Also check for floating zone.
							if ( TempControlType( ThisZoneNum ) != SingleCoolingSetPoint && TempControlType( ThisZoneNum ) != 0 ) {
								if ( ! LastModeHeating( VRFCond ) ) {
									// if last mode was cooling, make sure heating flow rate is used
									if ( VRFTU( TUIndex ).OAMixerUsed ) {
										Node( VRFTU( TUIndex ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( TUIndex ).MaxHeatAirMassFlow;
										Node( VRFTU( TUIndex ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( TUIndex ).HeatOutAirMassFlow;
										SimOAMixer( VRFTU( TUIndex ).OAMixerName, FirstHVACIteration, VRFTU( TUIndex ).OAMixerIndex );
									} else {
										Node( VRFTU( TUIndex ).VRFTUInletNodeNum ).MassFlowRate = VRFTU( TUIndex ).MaxHeatAirMassFlow;
									}

									// recalculate using correct flow rate
									if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
									// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
										CalcVRF_FluidTCtrl( TUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
									} else {
									// Algorithm Type: VRF model based on system curve
										CalcVRF( TUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
									}

									if ( TempOutput < LoadToHeatingSP ) {
										++NumHeatingLoads( VRFCond );
										// sum heating load on condenser, not total zone heating load
										SumHeatingLoads( VRFCond ) += ( LoadToHeatingSP - TempOutput );
									}
								} else {
									++NumHeatingLoads( VRFCond );
									// sum heating load on condenser, not total zone heating load
									SumHeatingLoads( VRFCond ) += ( LoadToHeatingSP - TempOutput );
								}
							}
						} else if ( TempOutput < ZoneLoad ) {
							//             If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint, turn off coil
							//             do nothing, the zone will float
						} else if ( ZoneLoad < 0.0 ) {
							//               still a cooling load
							++NumCoolingLoads( VRFCond );
							// sum cooling load on condenser, not total zone cooling load
							SumCoolingLoads( VRFCond ) += ( LoadToCoolingSP - TempOutput );
						}

						//           If the terminal unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
					} else if ( TempOutput > 0.0 && LoadToCoolingSP > 0.0 ) {
						//             If the net heating capacity overshoots the cooling setpoint count as cooling load
						if ( TempOutput > LoadToCoolingSP ) {
							//               Don't count as cooling load unless mode is allowed. Also check for floating zone.
							if ( TempControlType( ThisZoneNum ) != SingleHeatingSetPoint && TempControlType( ThisZoneNum ) != 0 ) {
								if ( ! LastModeCooling( VRFCond ) ) {
									if ( VRFTU( TUIndex ).OAMixerUsed ) {
										Node( VRFTU( TUIndex ).VRFTUOAMixerRetNodeNum ).MassFlowRate = VRFTU( TUIndex ).MaxCoolAirMassFlow;
										Node( VRFTU( TUIndex ).VRFTUOAMixerOANodeNum ).MassFlowRate = VRFTU( TUIndex ).CoolOutAirMassFlow;
										SimOAMixer( VRFTU( TUIndex ).OAMixerName, FirstHVACIteration, VRFTU( TUIndex ).OAMixerIndex );
									} else {
										Node( VRFTU( TUIndex ).VRFTUInletNodeNum ).MassFlowRate = VRFTU( TUIndex ).MaxCoolAirMassFlow;
									}

									if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
									// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
										CalcVRF_FluidTCtrl( TUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
									} else {
									// Algorithm Type: VRF model based on system curve
										CalcVRF( TUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
									}

									if ( TempOutput > LoadToCoolingSP ) {
										++NumCoolingLoads( VRFCond );
										SumCoolingLoads( VRFCond ) += ( LoadToCoolingSP - TempOutput );
									}
								} else {
									++NumCoolingLoads( VRFCond );
									SumCoolingLoads( VRFCond ) += ( LoadToCoolingSP - TempOutput );
								}
							}
						} else if ( TempOutput > ZoneLoad ) {
							// do nothing, zone will float
						} else if ( ZoneLoad > 0.0 ) {
							++NumHeatingLoads( VRFCond );
							SumHeatingLoads( VRFCond ) += ZoneLoad;
						}
						//           ELSE there is no overshoot and the zone has a valid cooling load
					} else if ( ZoneLoad < 0.0 ) {
						++NumCoolingLoads( VRFCond );
						SumCoolingLoads( VRFCond ) += ZoneLoad;
						// ELSE there is no overshoot and the zone has a valid heating load
					} else if ( ZoneLoad > 0.0 ) {
						++NumHeatingLoads( VRFCond );
						SumHeatingLoads( VRFCond ) += ZoneLoad;
					}
				} else { // is cycling fan
					if ( ZoneLoad > 0.0 ) {
						++NumHeatingLoads( VRFCond );
						SumHeatingLoads( VRFCond ) += ZoneLoad;
					} else if ( ZoneLoad < 0.0 ) {
						++NumCoolingLoads( VRFCond );
						SumCoolingLoads( VRFCond ) += ZoneLoad;
					}
				} // IF(VRFTU(TUIndex)%OpMode == ContFanCycCoil)THEN
			}
		}

		// Determine operating mode based on VRF type and thermostat control selection
		{ auto const SELECT_CASE_var( VRF( VRFCond ).ThermostatPriority );
		if ( SELECT_CASE_var == ThermostatOffsetPriority ) {
			if ( MaxDeltaT( VRFCond ) > std::abs( MinDeltaT( VRFCond ) ) && MaxDeltaT( VRFCond ) > 0.0 ) {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = true;
			} else if ( MaxDeltaT( VRFCond ) < std::abs( MinDeltaT( VRFCond ) ) && MinDeltaT( VRFCond ) < 0.0 ) {
				HeatingLoad( VRFCond ) = true;
				CoolingLoad( VRFCond ) = false;
			} else {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = false;
			}
		} else if ( SELECT_CASE_var == LoadPriority ) {
			if ( SumHeatingLoads( VRFCond ) > std::abs( SumCoolingLoads( VRFCond ) ) && SumHeatingLoads( VRFCond ) > 0.0 ) {
				HeatingLoad( VRFCond ) = true;
				CoolingLoad( VRFCond ) = false;
			} else if ( SumHeatingLoads( VRFCond ) <= std::abs( SumCoolingLoads( VRFCond ) ) && SumCoolingLoads( VRFCond ) < 0.0 ) {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = true;
			} else {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = false;
			}
		} else if ( SELECT_CASE_var == ZonePriority ) {
			if ( NumHeatingLoads( VRFCond ) > NumCoolingLoads( VRFCond ) && NumHeatingLoads( VRFCond ) > 0 ) {
				HeatingLoad( VRFCond ) = true;
				CoolingLoad( VRFCond ) = false;
			} else if ( NumHeatingLoads( VRFCond ) <= NumCoolingLoads( VRFCond ) && NumCoolingLoads( VRFCond ) > 0 ) {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = true;
			} else {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = false;
			}
		} else if ( SELECT_CASE_var == ScheduledPriority ) {
			if ( GetCurrentScheduleValue( VRF( VRFCond ).SchedPriorityPtr ) == 0 ) {
				HeatingLoad( VRFCond ) = true;
				CoolingLoad( VRFCond ) = false;
			} else if ( GetCurrentScheduleValue( VRF( VRFCond ).SchedPriorityPtr ) == 1 ) {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = true;
			} else {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = false;
			}
		} else if ( SELECT_CASE_var == MasterThermostatPriority ) {
			ZoneLoad = ZoneSysEnergyDemand( VRF( VRFCond ).MasterZonePtr ).RemainingOutputRequired;
			if ( VRFTU( VRF( VRFCond ).MasterZoneTUIndex ).OpMode == ContFanCycCoil ) {
				SetCompFlowRate( VRF( VRFCond ).MasterZoneTUIndex, VRFCond );

				if ( VRF( VRFCond ).VRFAlgorithmTypeNum == AlgorithmTypeFluidTCtrl ) {
				// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
					CalcVRF_FluidTCtrl( VRF( VRFCond ).MasterZoneTUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
				} else {
				// Algorithm Type: VRF model based on system curve
					CalcVRF( VRF( VRFCond ).MasterZoneTUIndex, FirstHVACIteration, 0.0, TempOutput, OnOffAirFlowRatio );
				}

				LoadToCoolingSP = ZoneSysEnergyDemand( VRF( VRFCond ).MasterZonePtr ).OutputRequiredToCoolingSP;
				LoadToHeatingSP = ZoneSysEnergyDemand( VRF( VRFCond ).MasterZonePtr ).OutputRequiredToHeatingSP;
				if ( TempOutput < LoadToHeatingSP ) {
					CoolingLoad( VRFCond ) = false;
					HeatingLoad( VRFCond ) = true;
				} else if ( TempOutput > LoadToCoolingSP ) {
					CoolingLoad( VRFCond ) = true;
					HeatingLoad( VRFCond ) = false;
				} else {
					CoolingLoad( VRFCond ) = false;
					HeatingLoad( VRFCond ) = false;
				}
			} else if ( ZoneLoad > 0.0 ) {
				HeatingLoad( VRFCond ) = true;
				CoolingLoad( VRFCond ) = false;
			} else if ( ZoneLoad < 0.0 ) {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = true;
			} else {
				HeatingLoad( VRFCond ) = false;
				CoolingLoad( VRFCond ) = false;
			}
		} else if ( SELECT_CASE_var == FirstOnPriority ) {
			// na
		} else {
		}}

		// limit to one possible mode
		if ( CoolingLoad( VRFCond ) && HeatingLoad( VRFCond ) ) HeatingLoad( VRFCond ) = false;

	}

	void
	LimitTUCapacity(
		int const VRFCond, // Condenser Unit index
		int const NumTUInList, // Number of terminal units in list
		Real64 const StartingCapacity, // temporary variable holding condenser capacity [W]
		Array1S< Real64 > const CapArray, // Array of coil capacities in either cooling or heating mode [W]
		Real64 & MaxLimit, // Maximum terminal unit capacity for coils in same operating mode [W]
		Real64 const AltCapacity, // temporary variable holding heat recovery capacity [W]
		Array1S< Real64 > const AltArray, // Array of coil capacities of heat recovery [W]
		Real64 & AltLimit // Maximum terminal unit capacity of heat recovery coils [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2012 (Moved from InitVRF)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the maximum allowed terminal unit capacity. Total terminal unit capacity must not
		// exceed the available condenser capacity. This variable, MaxCapacity (passed out to MaxCoolingCapacity
		// or MaxHeatingCapacity), is used to limit the terminal units providing more capacity than allowed.
		// Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected to a condenser having only 9-tons available.
		// This variable is will be set to 3-tons and the 4-ton terminal unit will be limited to 3-tons
		// (see InitVRF where this variable is reset and CalcVRF where the call to the DX coils passes this argument).

		// METHODOLOGY EMPLOYED:
		// The coils are simulated and summed. This value is compared to the available capacity. If the summed
		// TU capacity is greater than the available capacity, limit the TU's with the highest capacity so that
		// the TU capacity equals the available capacity. The report variable Variable Refrigerant Flow Heat Pump
		// Maximum Terminal Unit Cool/Heating Capacity holds the value for maximum TU capacity. This value may not
		// match the maximum individual coil capacity exactly since the available capaity uses a load weighted
		// average WB temperature to calculate available capacity. When the TU's are limited, this weighting changes.
		// The extra iterations required for these values to converge is considered excessive.
		// If the globabl flag SimZoneEquipment could be set for 1 additional iteration, these variables would
		// converge more closely (setting this globabl flag is not yet implemented).

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// these variables hold information on coil in opposite operating mode (i.e., heat recovery)
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RemainingCapacity; // decrement capacity counter to find limiting TU capacity [W]

		// limit TU coil capacity to be equal to the condenser capacity (piping losses already accounted for)
		LimitCoilCapacity( NumTUInList, StartingCapacity, CapArray, MaxLimit );

		// ** add in logic to limit coils operating opposite to mode when heat recovery is used
		// ** this is a hard one since we are here because the system is overloaded. That means
		// ** that we do not know at this point the actual operating capacity or compressor power.
		if ( VRF( VRFCond ).HeatRecoveryUsed ) {
			if ( CoolingLoad( VRFCond ) ) {
				RemainingCapacity = StartingCapacity * ( 1 + 1 / VRF( VRFCond ).CoolingCOP );
				if ( AltCapacity > RemainingCapacity ) {
					LimitCoilCapacity( NumTUInList, RemainingCapacity, AltArray, AltLimit );
				}
			}
			if ( HeatingLoad( VRFCond ) ) {
				RemainingCapacity = StartingCapacity / ( 1 + 1 / VRF( VRFCond ).HeatingCOP );
				if ( AltCapacity > RemainingCapacity ) {
					LimitCoilCapacity( NumTUInList, RemainingCapacity, AltArray, AltLimit );
				}
			}
		}

	}

	void
	LimitCoilCapacity(
		int const NumTUInList, // Number of terminal units in list
		Real64 const TotalCapacity, // temporary variable holding condenser capacity [W]
		Array1S< Real64 > const CapArray, // Array of coil capacities in either cooling or heating mode [W]
		Real64 & MaxLimit // Maximum terminal unit capacity for coils in same operating mode [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2012 (Moved from InitVRF)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the maximum allowed terminal unit capacity. Total terminal unit capacity must not
		// exceed the available condenser capacity. This variable, MaxCapacity (passed out to MaxCoolingCapacity
		// or MaxHeatingCapacity), is used to limit the terminal units providing more capacity than allowed.
		// Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected to a condenser having only 9-tons available.
		// This variable is will be set to 3-tons and the 4-ton terminal unit will be limited to 3-tons
		// (see InitVRF where this variable is reset and CalcVRF where the call to the DX coils passes this argument).

		// METHODOLOGY EMPLOYED:
		// The coils are simulated and summed. This value is compared to the available capacity. If the summed
		// TU capacity is greater than the available capacity, limit the TU's with the highest capacity so that
		// the TU capacity equals the available capacity. The report variable Variable Refrigerant Flow Heat Pump
		// Maximum Terminal Unit Cool/Heating Capacity holds the value for maximum TU capacity. This value may not
		// match the maximum individual coil capacity exactly since the available capaity uses a load weighted
		// average WB temperature to calculate available capacity. When the TU's are limited, this weighting changes.
		// The extra iterations required for these values to converge is considered excessive.
		// If the globabl flag SimZoneEquipment could be set for 1 additional iteration, these variables would
		// converge more closely (setting this globabl flag is not yet implemented).

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumTU; // loop counter
		int TempTUIndex; // temp variable used to find max terminal unit limit
		int MinOutputIndex; // index to TU with lowest load
		Real64 MinOutput; // used when finding TU "max" capacity limit
		Real64 RemainingCapacity; // decrement capacity counter to find limiting TU capacity [W]
		Array1D< Real64 > Temp( NumTUInList, CapArray ); // temporary array for processing terminal units
		Array1D< Real64 > Temp2( NumTUInList, Temp ); // temporary array for processing terminal units

		RemainingCapacity = TotalCapacity;

		// sort TU capacity from lowest to highest
		for ( TempTUIndex = 1; TempTUIndex <= NumTUInList; ++TempTUIndex ) {
			MinOutput = MaxCap;
			for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
				if ( Temp2( NumTU ) < MinOutput ) {
					MinOutput = Temp2( NumTU );
					Temp( TempTUIndex ) = MinOutput;
					MinOutputIndex = NumTU;
				}
			}
			Temp2( MinOutputIndex ) = MaxCap;
		}

		// find limit of "terminal unit" capacity so that sum of all TU's does not exceed condenser capacity
		// if the terminal unit capacity multiplied by number of remaining TU's does not exceed remaining available, subtract and cycle
		for ( TempTUIndex = 1; TempTUIndex <= NumTUInList; ++TempTUIndex ) {
			if ( ( Temp( TempTUIndex ) * ( NumTUInList - TempTUIndex + 1 ) ) < RemainingCapacity ) {
				RemainingCapacity -= Temp( TempTUIndex );
				continue;
			} else {
				// if it does exceed, limit is found
				MaxLimit = RemainingCapacity / ( NumTUInList - TempTUIndex + 1 );
				break;
			}
		}

	}

	int
	GetVRFTUOutAirNode( int const VRFTUNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad (copy of B Griffith routine)
		//       DATE WRITTEN   Jan  2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for VRF terminal unit OA inlet node

		if ( GetVRFInputFlag ) {
			GetVRFInput();
			GetVRFInputFlag = false;
		}

		if ( VRFTUNum > 0 && VRFTUNum <= NumVRFTU ) {
			return VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum;
		} else {
			return 0;
		}

	}

	int
	GetVRFTUZoneInletAirNode( int const VRFTUNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad (copy of B Griffith routine)
		//       DATE WRITTEN   Jan  2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for VRF terminal unit zone inlet node

		if ( GetVRFInputFlag ) {
			GetVRFInput();
			GetVRFInputFlag = false;
		}

		if ( VRFTUNum > 0 && VRFTUNum <= NumVRFTU ) {
			return VRFTU( VRFTUNum ).VRFTUOutletNodeNum;
		} else {
			return 0;
		}

	}

	int
	GetVRFTUMixedAirNode( int const VRFTUNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad (copy of B Griffith routine)
		//       DATE WRITTEN   Jan  2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for VRF terminal unit mixed air node

		if ( GetVRFInputFlag ) {
			GetVRFInput();
			GetVRFInputFlag = false;
		}

		if ( VRFTUNum > 0 && VRFTUNum <= NumVRFTU ) {
			return VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum;
		} else {
			return 0;
		}

	}

	int
	GetVRFTUReturnAirNode( int const VRFTUNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad (copy of B Griffith routine)
		//       DATE WRITTEN   Jan  2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for VRF terminal unit return air node

		if ( GetVRFInputFlag ) {
			GetVRFInput();
			GetVRFInputFlag = false;
		}

		if ( VRFTUNum > 0 && VRFTUNum <= NumVRFTU ) {
			return VRFTU( VRFTUNum ).VRFTUOAMixerRetNodeNum;
		} else {
			return 0;
		}

	}

	void
	CalcVRFIUTeTc_FluidTCtrl(
		int const IndexVRFCondenser // index to VRF OU
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         RP Zhang (LBNL), XF Pang (LBNL), Y Yura (Daikin Inc)
		//       DATE WRITTEN   June 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//       This subroutine is part of the new VRF model based on physics, appliable for Fluid Temperature Control.
		//       This subroutine determines the VRF evaporating temperature at cooling mode and the condensing temperature
		//       at heating mode. This is the indoor unit side analysis.

		// METHODOLOGY EMPLOYED:
		//       There are two options to calculate the IU Te/Tc: (1) HighSensible method analyzes the conditions of each IU
		//       and then decide and Te/Tc that can satisfy all the zones (2) TeTcConstant method uses fixed values provided
		//       by the user.

		// REFERENCES:
		// na

		// Using/Aliasing
		// na

		// Followings for FluidTCtrl Only
		Array1D< Real64 >  EvapTemp;
		Array1D< Real64 >  CondTemp;
		Real64 IUMinEvapTemp;
		Real64 IUMaxCondTemp;

		int TUListNum = VRF( IndexVRFCondenser ).ZoneTUListPtr;
		EvapTemp.allocate( TerminalUnitList( TUListNum ).NumTUInList );
		CondTemp.allocate( TerminalUnitList( TUListNum ).NumTUInList );
		IUMinEvapTemp = 100.0;
		IUMaxCondTemp = 0.0;

		if ( VRF( IndexVRFCondenser ).AlgorithmIUCtrl == 1) {
		// 1. HighSensible: analyze the conditions of each IU

			for ( int i = 1; i <= TerminalUnitList( TUListNum ).NumTUInList; i++ ) {
				int VRFTUNumi = TerminalUnitList( TUListNum ).ZoneTUPtr( i );
				// analyze the conditions of each IU
				CalcVRFIUVariableTeTc( VRFTUNumi, EvapTemp( i ), CondTemp( i ) );

				// select the Te/Tc that can satisfy all the zones
				IUMinEvapTemp = min( IUMinEvapTemp, EvapTemp( i ), 15.0);
				IUMaxCondTemp = max( IUMaxCondTemp, CondTemp( i ), 42.0);
			}

			VRF( IndexVRFCondenser ).IUEvaporatingTemp = max( IUMinEvapTemp, 4.0 );
			VRF( IndexVRFCondenser ).IUCondensingTemp = min( IUMaxCondTemp, 46.0 );

		} else {
		// 2. TeTcConstant: use fixed values provided by the user
			VRF( IndexVRFCondenser ).IUEvaporatingTemp = VRF( IndexVRFCondenser ).EvapTempFixed;
			VRF( IndexVRFCondenser ).IUCondensingTemp  = VRF( IndexVRFCondenser ).CondTempFixed;
		}

	}

	void
	CalcVRFIUVariableTeTc(
		int const VRFTUNum, // the number of the VRF TU to be simulated
		Real64 & EvapTemp, // evaporating temperature
		Real64 & CondTemp  // condensing temperature
	) {
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Xiufeng Pang, LBNL
		//       DATE WRITTEN   Feb 2014
		//       MODIFIED       Jul 2015, RP Zhang, LBNL, Modify the bounds of the Te/Tc
		//       MODIFIED       Nov 2015, RP Zhang, LBNL, take into account OA in Te/Tc determination
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//       Calculate the VRF IU Te (cooling mode) and Tc (heating mode), given zonal loads.

		// METHODOLOGY EMPLOYED:
		//       A new physics based VRF model appliable for Fluid Temperature Control.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using namespace DataZoneEnergyDemands;
		using DataEnvironment::OutBaroPress;
		using DXCoils::DXCoil;
		using Fans::Fan;
		using Fans::SimulateFanComponents;
		using InputProcessor::FindItemInList;
		using MixedAir::SimOAMixer;
		using MixedAir::OAMixer;
		using HVACVariableRefrigerantFlow::VRF;
		using HVACVariableRefrigerantFlow::VRFTU;
		using MixedAir::SimOAMixer;
		using Psychrometrics::PsyHFnTdbW;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int const Mode( 1 ); // Performance mode for MultiMode DX coil. Always 1 for other coil types
		int CoolCoilNum; // index to the VRF Cooling DX coil to be simulated
		int FanOutletNode; // index to the outlet node of the fan
		int HeatCoilNum; // index to the VRF Heating DX coil to be simulated
		int OAMixerNum; //OA mixer index
		int OAMixNode; // index to the mix node of OA mixer
		int IndexToTUInTUList; // index to TU in specific list for the VRF system
		int TUListIndex; // index to TU list for this VRF system
		int VRFNum; // index to VRF that the VRF Terminal Unit serves
		int VRFInletNode; // VRF inlet node number
		int ZoneIndex; // index to zone where the VRF Terminal Unit resides
		Real64 BFC; // Bypass factor at the cooling mode (-)
		Real64 BFH; // Bypass factor at the heating mode (-)
		Real64 C1Tevap; // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C2Tevap; // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C3Tevap; // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C1Tcond; // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 C2Tcond; // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 C3Tcond; // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 CondTempMin; // Min condensing temperature (C)
		Real64 CondTempMax; // Max condensing temperature, correspond to the maximum heating capacity (C)
		Real64 DeltaT; // Difference between evaporating/condensing temperature and coil surface temperature (C)
		Real64 EvapTempMax; // Max evaporating temperature (C)
		Real64 EvapTempMin; // Min evaporating temperature, correspond to the maximum cooling capacity (C)
		Real64 Garate; // Nominal air mass flow rate
		Real64 H_coil_in; // Air enthalpy at the coil inlet (kJ/kg)
		Real64 QZnReqSenCoolingLoad; // Zone required sensible cooling load (W)
		Real64 QZnReqSenHeatingLoad; // Zone required sensible heating load (W)
		Real64 RHsat; // Relative humidity of the air at saturated condition(-)
		Real64 SH; // Super heating degrees (C)
		Real64 SC; // Subcooling degrees (C)
		Real64 temp; // for temporary use
		Real64 T_coil_in; // Temperature of the air at the coil inlet, after absorbing the heat released by fan (C)
		Real64 T_TU_in; // Air temperature at the indoor unit inlet (C)
		Real64 Tout; // Air temperature at the indoor unit outlet (C)
		Real64 Th2; // Air temperature at the coil surface (C)
		Real64 W_coil_in; // coil inlet air humidity ratio [kg/kg]
		Real64 W_TU_in; // Air humidity ratio at the indoor unit inlet[kg/kg]

		// Get the equipment/zone index corresponding to the VRFTU
		CoolCoilNum = VRFTU( VRFTUNum ).CoolCoilIndex;
		HeatCoilNum = VRFTU( VRFTUNum ).HeatCoilIndex;
		ZoneIndex = VRFTU( VRFTUNum ).ZoneNum;
		VRFNum = VRFTU( VRFTUNum ).VRFSysNum;
		TUListIndex = VRF( VRFNum ).ZoneTUListPtr;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;

		// Bounds of Te/Tc for VRF IU Control Algorithm: VariableTemp
		EvapTempMin = VRF( VRFNum ).IUEvapTempLow;
		EvapTempMax = VRF( VRFNum ).IUEvapTempHigh;
		CondTempMin = VRF( VRFNum ).IUCondTempLow;
		CondTempMax = VRF( VRFNum ).IUCondTempHigh;

		// Coefficients describing coil performance
		SH = DXCoil( CoolCoilNum ).SH;
		SC = DXCoil( HeatCoilNum ).SC;
		C1Tevap = DXCoil( CoolCoilNum ).C1Te;
		C2Tevap = DXCoil( CoolCoilNum ).C2Te;
		C3Tevap = DXCoil( CoolCoilNum ).C3Te;
		C1Tcond = DXCoil( HeatCoilNum ).C1Tc;
		C2Tcond = DXCoil( HeatCoilNum ).C2Tc;
		C3Tcond = DXCoil( HeatCoilNum ).C3Tc;

		// Rated air flow rate for the coil
		if ( ( ! VRF( VRFNum ).HeatRecoveryUsed && CoolingLoad( VRFNum ) ) || ( VRF( VRFNum ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) ) {
			// VRF terminal unit is on cooling mode
			CompOnMassFlow = DXCoil( CoolCoilNum ).RatedAirMassFlowRate( Mode );
		} else if ( ( ! VRF( VRFNum ).HeatRecoveryUsed && HeatingLoad( VRFNum ) ) || ( VRF( VRFNum ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) ) {
			// VRF terminal unit is on heating mode
			CompOnMassFlow = DXCoil( HeatCoilNum ).RatedAirMassFlowRate( Mode );
		}

		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		SetAverageAirFlow( VRFTUNum, 1.0, temp );
		VRFInletNode = VRFTU( VRFTUNum ).VRFTUInletNodeNum;
		T_TU_in = Node( VRFInletNode ).Temp;
		W_TU_in = Node( VRFInletNode ).HumRat;
		T_coil_in = T_TU_in;
		W_coil_in = W_TU_in;

		// Simulation the OAMixer if there is any
		if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
			SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, false, VRFTU( VRFTUNum ).OAMixerIndex );

			OAMixerNum = FindItemInList( VRFTU( VRFTUNum ).OAMixerName, OAMixer );
			OAMixNode = OAMixer( OAMixerNum ).MixNode;
			T_coil_in = Node( OAMixNode ).Temp;
			W_coil_in = Node( OAMixNode ).HumRat;
		}

		// Simulate the blow-through fan if there is any
		if ( VRFTU( VRFTUNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( "", false, VRFTU( VRFTUNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

			FanOutletNode = Fan( VRFTU( VRFTUNum ).FanIndex ).OutletNodeNum;
			T_coil_in = Node( FanOutletNode ).Temp;
			W_coil_in = Node( FanOutletNode ).HumRat;
		}

		Garate = CompOnMassFlow;
		H_coil_in = PsyHFnTdbW( T_coil_in, W_coil_in );
		RHsat = 0.98;
		BFC = 0.0592;
		BFH = 0.136;

		//1. COOLING Mode
		if ( ( ! VRF( VRFNum ).HeatRecoveryUsed && CoolingLoad( VRFNum ) ) || ( VRF( VRFNum ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) ) {
		//1.1) Cooling coil is running
			QZnReqSenCoolingLoad = max( 0.0, - 1.0 * ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP );
			Tout = T_TU_in - QZnReqSenCoolingLoad / Garate / 1005;
			Th2 = T_coil_in - ( T_coil_in - Tout ) / ( 1 - BFC );
			DeltaT = C3Tevap * SH * SH + C2Tevap * SH + C1Tevap;
			EvapTemp = max( min( (Th2 - DeltaT ), EvapTempMax ), EvapTempMin );

		} else {
		//1.2) Cooling coil is not running
			EvapTemp = T_coil_in;
		}

		//2. HEATING Mode
		if ( ( ! VRF( VRFNum ).HeatRecoveryUsed && HeatingLoad( VRFNum ) ) || ( VRF( VRFNum ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) ) {
		//2.1) Heating coil is running
			QZnReqSenHeatingLoad = max( 0.0, ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP );
			Tout = T_TU_in + QZnReqSenHeatingLoad / Garate / 1005;
			Th2 = T_coil_in + ( Tout - T_coil_in ) / ( 1 - BFH );
			DeltaT = C3Tcond * SC * SC + C2Tcond * SC + C1Tcond;
			CondTemp = max( min( ( Th2 + DeltaT ), CondTempMax ), CondTempMin);
		} else {
		//2.2) Heating coil is not running
			CondTemp = T_coil_in;
		}
	}

	void
	CalcVRFCondenser_FluidTCtrl(
		int const VRFCond, // index to VRF condenser
		bool const EP_UNUSED( FirstHVACIteration ) // flag for first time through HVAC system simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         RP Zhang (LBNL), XF Pang (LBNL), Y Yura (Daikin Inc)
		//       DATE WRITTEN   June 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//       This subroutine is part of the new VRF model based on physics, appliable for Fluid Temperature Control.
		//       This is adapted from subroutine CalcVRFCondenser, which is part of the VRF model based on system curves.
		//       This subroutine models the interactions of VRF indoor units with the outdoor unit.
		//       The indoor terminal units are simulated first, and then the outdoor unit is simulated.

		// METHODOLOGY EMPLOYED:
		//       A new physics based VRF model appliable for Fluid Temperature Control.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using General::SolveRegulaFalsi;
		using Psychrometrics::RhoH2O;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutWetBulbTemp;
		using DXCoils::DXCoilCoolInletAirWBTemp;
		using DXCoils::DXCoilHeatInletAirDBTemp;
		using DXCoils::DXCoilHeatInletAirWBTemp;
		using DXCoils::DXCoilTotalHeating;
		using DXCoils::DXCoil;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetSatPressureRefrig;
		using FluidProperties::GetSatTemperatureRefrig;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSupHeatDensityRefrig;
		using FluidProperties::GetSupHeatEnthalpyRefrig;
		using FluidProperties::FindRefrigerant;
		using FluidProperties::RefrigData;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "VRFCondenser" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TUListNum; // index to TU List
		int NumTUInList; // number of terminal units is list
		int NumTU; // loop counter
		int TUIndex; // Index to terminal unit
		int CoolCoilIndex; // index to cooling coil in terminal unit
		int HeatCoilIndex; // index to heating coil in terminal unit
		int NumTUInCoolingMode; // number of terminal units actually cooling
		int NumTUInHeatingMode; // number of terminal units actually heating

		Real64 TUCoolingLoad; // DX cooling coil load to be met by condenser (W)
		Real64 TUHeatingLoad; // DX heating coil load to be met by condenser (W)
		Real64 TUParasiticPower; // total terminal unit parasitic power (W)
		Real64 TUFanPower; // total terminal unit fan power (W)
		Real64 InletAirWetBulbC; // coil inlet air wet-bulb temperature (C)
		Real64 InletAirDryBulbC; // coil inlet air dry-bulb temperature (C)
		Real64 CondInletTemp( 0.0 ); // condenser inlet air temperature (C)
		Real64 OutdoorDryBulb; // outdoor dry-bulb temperature (C)
		Real64 OutdoorHumRat; // outdoor humidity ratio (kg/kg)
		Real64 OutdoorPressure; // outdoor pressure (Pa)
		Real64 OutdoorWetBulb; // outdoor wet-bulb temperature (C)
		Real64 SumCoolInletWB; // sum of active TU's DX cooling coil inlet air wet-bulb temperature
		Real64 SumHeatInletDB; // sum of active TU's DX heating coil inlet air dry-bulb temperature
		Real64 SumHeatInletWB; // sum of active TU's DX heating coil inlet air wet-bulb temperature
		Real64 TotalTUCoolingCapacity; // sum of TU's cooling capacity (W)
		Real64 TotalTUHeatingCapacity; // sum of TU's heating capacity (W)
		Real64 TotalCondCoolingCapacity; // total available condenser cooling capacity (W)
		Real64 TotalCondHeatingCapacity; // total available condenser heating capacity (W)
		Real64 CoolingPLR; // condenser cooling PLR
		Real64 HeatingPLR; // condenser heating PLR
		Real64 CyclingRatio; // cycling ratio of condenser's compressors
		int Stage; // used for crankcase heater power calculation
		Real64 UpperStageCompressorRatio; // used for crankcase heater power calculation
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 PartLoadFraction; // Part load fraction from PLFFPLR curve
		Real64 VRFRTF; // VRF runtime fraction when cycling below MINPLR
		Real64 OutdoorCoilT; // Outdoor coil temperature (C)
		Real64 OutdoorCoildw; // Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
		Real64 FractionalDefrostTime; // Fraction of time step system is in defrost
		Real64 HeatingCapacityMultiplier; // Multiplier for heating capacity when system is in defrost
		Real64 InputPowerMultiplier; // Multiplier for power when system is in defrost
		Real64 LoadDueToDefrost; // Additional load due to defrost
		Real64 DefrostEIRTempModFac; // EIR modifier for defrost (function of entering drybulb, outside wetbulb)
		int HRCAPFT; // index to heat recovery CAPFTCool curve
		Real64 HRCAPFTConst; // stead-state capacity fraction
		Real64 HRInitialCapFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
		Real64 HRCapTC; // Time constant used to recover from intial degratation in cooling heat recovery
		int HREIRFT; // Index to cool EIR as a function of temperature curve for heat recovery
		Real64 HREIRFTConst; // stead-state EIR fraction
		Real64 HRInitialEIRFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
		Real64 HREIRTC; // Time constant used to recover from intial degratation in cooling heat recovery
		static Real64 CurrentEndTime; // end time of current time step
		static Real64 CurrentEndTimeLast; // end time of last time step
		static Real64 TimeStepSysLast; // system time step on last time step
		Real64 SUMultiplier; // multiplier for simulating mode changes
		Real64 CondPower; // condenser power [W]
		Real64 CondCapacity; // condenser heat rejection [W]
		Real64 TotPower; // total condenser power use [W]
		bool HRHeatRequestFlag; // flag indicating VRF TU could operate in heating mode
		bool HRCoolRequestFlag; // flag indicating VRF TU could operate in cooling mode

		// Followings for VRF FluidTCtrl Only
		Array1D< Real64 > CompEvaporatingPWRSpd; // Array for the compressor power at certain speed [W]
		Array1D< Real64 > CompEvaporatingCAPSpd; // Array for the evaporating capacity at certain speed [W]
		Array1D< Real64 > Par( 3 ); // Array for the parameters [-]
		int CompSpdLB; // index for Compressor speed low bound [-]
		int CompSpdUB; // index for Compressor speed up bound [-]
		int Counter; // counter for iterations [-]
		int CounterCompSpdTemp ; // counter for iterations for compressor calculations[-]
		int MaxIter = 500; // max iteration number allowed [-]
		int NumIUActivated; // number of the used indoor units [-]
		int NumIteTe; // counter for Te calculation iterations [-]
		int NumIteHIUIn; // counter for HIU calculation iterations [-]
		int NumIteCcap; // counter for Ccap calculation iterations [-]
		int NumOfCompSpdInput; // Number of compressor speed input by the user [-]
		int RefrigerantIndex; // Index of the refrigerant [-]
		int SolFla; // Slove flag for SolveRegulaFalsi [-]
		int VRFOperationSimPath ;   // Flag indicating the operation mode in the simulation [-]
		Real64 BFC; // VRF OU bypass factor in cooling mode [-]
		Real64 BFH; // VRF OU bypass factor in heating mode [-]
		Real64 Cap_Eva0; // Evaporating capacity calculated based on physics model, used in the iterations [W]
		Real64 Cap_Eva1; // Evaporating capacity calculated by curves, used in the iterations [W]
		Real64 CapDiff; // Evaporating capacity difference used in the iterations [W]
		Real64 C_cap_density = 1; // Compressor capacity modification algorithm_modified flow rate [-]
		Real64 C_cap_enthalpy = 1; // Compressor capacity modification algorithm_modified enthalpy difference [-]
		Real64 C_cap_operation = 1; // Compressor capacity modification algorithm_modified Cap [-]
		Real64 C_cap_operation0 = 1; // Compressor capacity modification algorithm_modified Cap, for temporary use [-]
		Real64 CompSpdActual = 0; // Actual compressor running speed [rps]
		Real64 CompEvaporatingCAPSpdMin = 0; // Evaporating capacity at the lowest compressor speed [W]
		Real64 CompEvaporatingPWRSpdMin = 0; // Compressor power at the lowest compressor speed [W]
		Real64 CompEvaporatingCAPSpdMax = 0; // Evaporating capacity at the highest compressor speed [W]
		Real64 CompEvaporatingPWRSpdMax = 0; // Compressor power at the highest compressor speed [W]
		Real64 OUCondHeatRelease = 0; // Condenser heat release (cooling mode) [W]
		Real64 OUEvapHeatExtract = 0; // Condenser heat extract (heating mode) [W]
		Real64 CondFlowRatio; // Outdoor unit fan air flow ratio [-]
		Real64 deltaT; // Difference between evaporating/condensing temperature and coil surface temperature [C]
		Real64 Pipe_Q0; // Compressor capacity modification algorithm_modified Pipe_Q, for temporary use [W]
		Real64 Houtdoor; // Enthalpy of the outdoor air [kJ/kg]
		Real64 Hfs; // Enthalpy of the air at the coil surface [kJ/kg]
		Real64 IUMinEvapTemp; // VRV IU evaporating temperature, min among all indoor units [C]
		Real64 IUMaxCondTemp; // VRV IU condensing temperature, max among all indoor units [C]
		Real64 Modifi_SH; // Compressor power modification algorithm_modified SH [C]
		Real64 Modifi_SHin; // Compressor power modification algorithm_modified SH for IDU [C]
		Real64 Modifi_Pe; // Compressor power modification algorithm_modified Pe [Pa]
		Real64 MaxNumIteTe; // Piping Loss Algorithm Parameter: max number of iterations for Te [-]
		Real64 MinOutdoorUnitTe; // The minimum temperature that Te can be at cooling mode (only used for calculating Min capacity)
		Real64 MinOutdoorUnitPe; // The minimum pressure that Pe can be at cooling mode (only used for calculating Min capacity)
		Real64 MaxOutdoorUnitTc; // The maximum temperature that Tc can be at heating mode [C]
		Real64 MaxOutdoorUnitPc; // The maximum temperature that Pc can be at heating mode [Pa]
		Real64 MinOutdoorUnitTc; // The minimum temperature that Tc can be at cooling mode [C]
		Real64 MinOutdoorUnitPc; // The minimum pressure that Pc can be at cooling mode [Pa]
		Real64 MinRefriPe; // Minimum refirgerant eveporating pressure [Pa]
		Real64 NcompPriCooling; // Compressor power in cooling mode, for temp use in iterations [W]
		Real64 NcompPriHeating; // Compressor power in heating mode, for temp use in iterations [W]
		Real64 NcompCooling; // Compressor power in cooling mode [W]
		Real64 NcompHeating; // Compressor power in heating mode [W]
		Real64 NcompDiff; // Compressor power difference, for use in iterations [W]
		Real64 Pcond; // VRF condensing pressure [Pa]
		Real64 Pdischarge; // VRF compressor discharge pressure [Pa]
		Real64 Pevap; // VRF evaporating pressure [Pa]
		Real64 Psuction; // VRF compressor suction pressure [Pa]
		Real64 Pipe_m_ref; // Piping Loss Algorithm Parameter: Refigerant mass flow rate [kg/s]
		Real64 Pipe_m_ref_i; // Piping Loss Algorithm Parameter: Refigerant mass flow rate for a individual IU[kg/s]
		Real64 Pipe_v_ref; // Piping Loss Algorithm Parameter: Refigerant velocity [m/s]
		Real64 Pipe_T_IU_in; // Piping Loss Algorithm Parameter: Average Refigerant Temperature [C]
		Real64 Pipe_T_room; // Piping Loss Algorithm Parameter: Average Room Temperature [C]
		Real64 Pipe_Num_Re; // Piping Loss Algorithm Parameter: refrigerant Re Number [-]
		Real64 Pipe_Num_Pr; // Piping Loss Algorithm Parameter: refrigerant Pr Number [-]
		Real64 Pipe_Num_Nu; // Piping Loss Algorithm Parameter: refrigerant Nu Number [-]
		Real64 Pipe_Num_St; // Piping Loss Algorithm Parameter: refrigerant St Number [-]
		Real64 Pipe_Coe_k1; // Piping Loss Algorithm Parameter: coefficients [-]
		Real64 Pipe_Coe_k2; // Piping Loss Algorithm Parameter: coefficients [-]
		Real64 Pipe_Coe_k3; // Piping Loss Algorithm Parameter: coefficients [-]
		Real64 Pipe_cp_ref; // Piping Loss Algorithm_[kJ/kg/K]
		Real64 Pipe_conductivity_ref; // Piping Loss Algorithm: refrigerant conductivity [W/m/K]
		Real64 Pipe_DeltP; // Piping Loss Algorithm Parameter: Pipe pressure drop [Pa]
		Real64 Pipe_viscosity_ref; // Piping Loss Algorithm Parameter: refrigerant viscosity [MuPa*s]
		Real64 Pipe_h_IU_in; // Piping Loss Algorithm Parameter: enthalpy of IU at inlet [kJ/kg]
		Real64 Pipe_h_IU_in_temp; // Piping Loss Algorithm Parameter: enthalpy of IU at inlet (temp) [kJ/kg]
		Real64 Pipe_h_IU_in_new; // Piping Loss Algorithm Parameter: enthalpy of IU at inlet (new) [kJ/kg]
		Real64 Pipe_h_IU_in_low; // Piping Loss Algorithm Parameter: enthalpy of IU at inlet (low) [kJ/kg]
		Real64 Pipe_h_IU_in_up; // Piping Loss Algorithm Parameter: enthalpy of IU at inlet (up) [kJ/kg]
		Real64 Pipe_h_IU_out; // Piping Loss Algorithm Parameter: enthalpy of IU at outlet [kJ/kg]
		Real64 Pipe_h_IU_out_i; // Piping Loss Algorithm Parameter: enthalpy of IU at outlet (individual) [kJ/kg]
		Real64 Pipe_h_comp_out; // Piping Loss Algorithm Parameter: enthalpy of Compressor at outlet [kJ/kg]
		Real64 Pipe_h_comp_out_new; // Piping Loss Algorithm Parameter: enthalpy of Compressor at outlet (new) [kJ/kg]
		Real64 Pipe_h_out_ave; // Average Enthalpy of the refrigerant leaving IUs [kJ/kg]
		Real64 Pipe_h_out_i; // Piping Loss Algorithm Parameter: Enthalpy of the refrigerant leaving a specific IU [kJ/kg]
		Real64 Pipe_h_comp_in; // Piping Loss Algorithm Parameter: Enthalpy after piping loss (comparessor inlet) [kJ/kg]
		Real64 Pipe_h_comp_in_assumed; // Piping Loss Algorithm Parameter: enthalpy of compressor at inlet (assumed) [kJ/kg]
		Real64 Pipe_p_IU_out; // Piping Loss Algorithm Parameter: pressure of IU at outlet [Pa]
		Real64 Pipe_Pe_assumed; // Piping Loss Algorithm Parameter: evaporating pressure assumed for iterations[Pa]
		Real64 Pipe_Q; // Piping Loss Algorithm Parameter: Heat loss [W]
		Real64 Pipe_Te_assumed; // Piping Loss Algorithm Parameter: evaporating temperature assumed for iterations[C]
		Real64 Pipe_T_comp_in; // Piping Loss Algorithm Parameter: refrigerant temperature at comparessor inlet (after piping loss) [C]
		Real64 Pipe_SH_merged; // Piping Loss Algorithm Parameter: average super heating degrees after the indoor units [C]
		Real64 Ref_Coe_v1; // Piping Loss Algorithm Parameter: coefficient to calculate Pipe_viscosity_ref [-]
		Real64 Ref_Coe_v2; // Piping Loss Algorithm Parameter: coefficient to calculate Pipe_viscosity_ref [-]
		Real64 Ref_Coe_v3; // Piping Loss Algorithm Parameter: coefficient to calculate Pipe_viscosity_ref [-]
		Real64 RefPipInsH; // Heat transfer coefficient for calculating piping loss [W/m2K]
		Real64 SC_ave; // Average subcooling degrees after the indoor units [C]
		Real64 SmallLoadTe; // Outdoor unit evaporating temperature at small indoor heating load [C]
		Real64 Tc0; // Condensing temperature, for temporary use in iterations [C]
		Real64 Tdischarge; // Compressor discharge refrigerant temperature [C]
		Real64 Tsuction; // VRF compressor suction refrigeranttemperature [Pa]
		Real64 TUHeatingLoad_temp; // Piping Loss Algorithm Parameter: TUHeatingLoad for temporary use [W]
		Real64 TUCoolingLoad_temp; // Piping Loss Algorithm Parameter: TUCoolingLoad for temporary use [W]
		Real64 Tolerance; // Tolerance for condensing temperature calculation [C}
		Real64 TcondOut; // Temperature of the air at the outlet of the VRF outdoor unit coil [C]
		Real64 Tcondh2; // Temperature of the air at the outlet of the VRF outdoor unit [C]
		Real64 TfsSat; // Temperature of the air at the coil surface at the saturation condition [C]
		Real64 Tfs; // Temperature of the air at the coil surface [C]
		Real64 WfsSat; // Humidity ratio of the air at the coil surface at the saturation condition [C]

		// FLOW

		// variable initializations
		TUListNum = VRF( VRFCond ).ZoneTUListPtr;
		Pipe_conductivity_ref = VRF( VRFCond ).RefPipInsCon;
		NumTUInList = TerminalUnitList( TUListNum ).NumTUInList;
		TUCoolingLoad = 0.0;
		TUHeatingLoad = 0.0;
		TUParasiticPower = 0.0;
		TUFanPower = 0.0;
		CoolingPLR = 0.0;
		HeatingPLR = 0.0;
		CyclingRatio = 1.0;
		SumCoolInletWB = 0.0;
		SumHeatInletDB = 0.0;
		SumHeatInletWB = 0.0;
		TotalCondCoolingCapacity = 0.0;
		TotalCondHeatingCapacity = 0.0;
		TotalTUCoolingCapacity = 0.0;
		TotalTUHeatingCapacity = 0.0;
		NumTUInCoolingMode = 0;
		NumTUInHeatingMode = 0;
		IUMinEvapTemp = 10.0;
		IUMaxCondTemp = 40.0;
		Tolerance = 0.05;
		RefrigerantIndex = -1;
		Counter = 1;
		CounterCompSpdTemp = 1;
		NumIUActivated = 1;
		NumIteTe = 1;
		NumIteHIUIn = 1;
		NumIteCcap = 1;
		VRFOperationSimPath = 0;
		BFC = 0.219;
		BFH = 0.45581;
		RefPipInsH = 9.3;
		Pipe_cp_ref = 1.6;
		VRF( VRFCond ).ElecCoolingPower = 0.0;
		VRF( VRFCond ).ElecHeatingPower = 0.0;
		VRF( VRFCond ).CrankCaseHeaterPower = 0.0;
		VRF( VRFCond ).EvapCondPumpElecPower = 0.0;
		VRF( VRFCond ).EvapWaterConsumpRate = 0.0;
		VRF( VRFCond ).DefrostPower = 0.0;
		VRF( VRFCond ).OperatingCoolingCOP = 0.0;
		VRF( VRFCond ).OperatingHeatingCOP = 0.0;
		VRF( VRFCond ).OperatingCOP = 0.0;
		VRF( VRFCond ).BasinHeaterPower = 0.0;
		VRF( VRFCond ).CondensingTemp = 60.0; //OutDryBulbTemp;

		// Refrigerant data
		int RefrigNum = FindRefrigerant( VRF( VRFCond ).RefrigerantName );
		// Real64 RefTLow = RefrigData( RefrigNum ).PsLowTempValue; // Low Temperature Value for Ps (>0.0)
		Real64 RefTHigh = RefrigData( RefrigNum ).PsHighTempValue; // High Temperature Value for Ps (max in tables)
		Real64 RefPLow = RefrigData( RefrigNum ).PsLowPresValue; // Low Pressure Value for Ps (>0.0)
		Real64 RefPHigh = RefrigData( RefrigNum ).PsHighPresValue; // High Pressure Value for Ps (max in tables)
		Real64 RefTSat; // Saturated temperature of the refrigerant. Used to check whether the refrigernat is in the superheat area.
		Real64 RefTSat1; // Saturated temperature of the refrigerant. Used to check whether the refrigernat is in the superheat area.

		// sum loads on TU coils
		for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
			TUCoolingLoad += TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU );
			TUHeatingLoad += TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU );
			TUParasiticPower += VRFTU( TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU ) ).ParasiticCoolElecPower + VRFTU( TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU ) ).ParasiticHeatElecPower;
			TUFanPower += VRFTU( TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU ) ).FanPower;
		}
		VRF( VRFCond ).TUCoolingLoad = TUCoolingLoad;
		VRF( VRFCond ).TUHeatingLoad = TUHeatingLoad;

		// loop through TU's and calculate average inlet conditions for active coils
		for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
			TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
			CoolCoilIndex = VRFTU( TUIndex ).CoolCoilIndex;
			HeatCoilIndex = VRFTU( TUIndex ).HeatCoilIndex;

			if ( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) > 0.0 ) {
				SumCoolInletWB += DXCoilCoolInletAirWBTemp( CoolCoilIndex ) * TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) / TUCoolingLoad;
				++NumTUInCoolingMode;
			}
			if ( TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) > 0.0 ) {
				SumHeatInletDB += DXCoilHeatInletAirDBTemp( HeatCoilIndex ) * TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) / TUHeatingLoad;
				SumHeatInletWB += DXCoilHeatInletAirWBTemp( HeatCoilIndex ) * TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) / TUHeatingLoad;
				++NumTUInHeatingMode;
			}
		}

		// set condenser entering air conditions (Outdoor air conditions)
		if ( VRF( VRFCond ).CondenserNodeNum != 0 ) {
			OutdoorDryBulb = Node( VRF( VRFCond ).CondenserNodeNum ).Temp;
			if ( VRF( VRFCond ).CondenserType != WaterCooled ) {
				OutdoorHumRat = Node( VRF( VRFCond ).CondenserNodeNum ).HumRat;
				OutdoorPressure = Node( VRF( VRFCond ).CondenserNodeNum ).Press;
				OutdoorWetBulb = Node( VRF( VRFCond ).CondenserNodeNum ).OutAirWetBulb;
			} else {
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			}
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}
		RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat); //zrp: Outdoor air density

		CondInletTemp = OutdoorDryBulb; // VRF( VRFCond ).CondenserType == AirCooled
		VRF( VRFCond ).CondenserInletTemp = CondInletTemp;

		// initialization
		IUMinEvapTemp = VRF( VRFCond ).IUEvaporatingTemp;
		Pevap = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, IUMinEvapTemp, RefrigerantIndex, RoutineName );
		Psuction = Pevap;
		Tsuction = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Psuction, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

		Pcond = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, IUMaxCondTemp, RefrigerantIndex, RoutineName );
		IUMaxCondTemp = VRF( VRFCond ).IUCondensingTemp;

		Modifi_SH = VRF( VRFCond ).SH;
		Modifi_Pe = Pevap;

		// Initilization for Ncomp iterations
		NcompPriCooling = TUCoolingLoad / VRF( VRFCond ).CoolingCOP;
		NcompPriHeating = TUHeatingLoad / VRF( VRFCond ).HeatingCOP;
		NumOfCompSpdInput = VRF( VRFCond ).CompressorSpeed.size();
		CompEvaporatingPWRSpd.dimension( NumOfCompSpdInput );
		CompEvaporatingCAPSpd.dimension( NumOfCompSpdInput );

		// THREE MODES: 1. COOLING MODE 2. HEATING MODE 3. No running

		// 1. COOLING MODE
		if( CoolingLoad( VRFCond ) && ( TUCoolingLoad > 0.0 ) ) {

			TUCoolingLoad_temp = TUCoolingLoad;
			CondFlowRatio = 1.0;
			MaxOutdoorUnitPc = min( Psuction + VRF( VRFCond ).CompMaxDeltaP, 4000000.0 );
			MaxOutdoorUnitTc = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MaxOutdoorUnitPc, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			MinOutdoorUnitTc = OutdoorDryBulb + VRF( VRFCond ).SC;
			MinOutdoorUnitPc = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, MinOutdoorUnitTc, RefrigerantIndex, RoutineName );

			MinRefriPe = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, -15, RefrigerantIndex, RoutineName );
			MinOutdoorUnitPe = max( MinOutdoorUnitPc - VRF( VRFCond ).CompMaxDeltaP, MinRefriPe );
			MinOutdoorUnitTe = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

			CompEvaporatingCAPSpdMin = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( 1 ), MinOutdoorUnitTc, MinOutdoorUnitTe );
			CompEvaporatingPWRSpdMin = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( 1 ), MinOutdoorUnitTc, MinOutdoorUnitTe );
			CompEvaporatingCAPSpdMax = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( NumOfCompSpdInput ), VRF( VRFCond ).CondensingTemp, VRF( VRFCond ).IUEvaporatingTemp );
			CompEvaporatingPWRSpdMax = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( NumOfCompSpdInput ), VRF( VRFCond ).CondensingTemp, VRF( VRFCond ).IUEvaporatingTemp );

			//Calculate Pipe_T_room
			Pipe_T_room = 0;
			NumIUActivated = 0;
			for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
				TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
				CoolCoilIndex = VRFTU( TUIndex ).CoolCoilIndex;

				if( DXCoil( CoolCoilIndex ).TotalCoolingEnergyRate > 0.0 ){
					Pipe_T_room = Pipe_T_room + DXCoil( CoolCoilIndex ).InletAirTemp;
					NumIUActivated = NumIUActivated + 1;
				}
			}
			if( NumIUActivated > 0 )
				Pipe_T_room = Pipe_T_room / NumIUActivated;
			else
				Pipe_T_room = 24;

			VRF( VRFCond ).EvaporatingTemp = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

			Pipe_h_IU_in_low = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, OutdoorDryBulb - VRF( VRFCond ).SC, 0.0, RefrigerantIndex, RoutineName ); // Tc = Tamb
			Pipe_h_IU_in_up = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, MaxOutdoorUnitTc - VRF( VRFCond ).SC, 0.0, RefrigerantIndex, RoutineName ); // Tc = MaxOutdoorUnitTc
			Pipe_h_IU_in = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, OutdoorDryBulb + 10 - VRF( VRFCond ).SC,  0.0, RefrigerantIndex, RoutineName ); // Tc = Tamb+10

			// Initilization for Pipe_h_IU_in iterations (Label12)
			NumIteHIUIn = 1;
			Label12: ;
			Pipe_m_ref = 0;
			Pipe_h_IU_out = 0;
			Pipe_h_IU_out_i = 0;
			Pipe_m_ref_i = 0;
			Pipe_SH_merged = 0;

			// Calculate total refrigerant flow rate
			if( TUCoolingLoad > CompEvaporatingCAPSpdMax ){
				// Required load is beyond the max system capacity

				TUCoolingLoad = CompEvaporatingCAPSpdMax;
				TUCoolingLoad_temp = CompEvaporatingCAPSpdMax;
				VRF( VRFCond ).TUCoolingLoad = TUCoolingLoad;
				RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				Pipe_h_IU_out = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, VRF( VRFCond ).EvaporatingTemp + 3 ), max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				Pipe_SH_merged = 3;
				Pipe_m_ref = TUCoolingLoad / ( Pipe_h_IU_out - Pipe_h_IU_in );

			} else {

				for ( NumTU = 1; NumTU <= NumTUInList; NumTU++ ){ // Calc total refrigerant flow rate
					if( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) > 0 ) {
						TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
						CoolCoilIndex = VRFTU( TUIndex ).CoolCoilIndex;

						RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
						Pipe_h_IU_out_i = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, VRF( VRFCond ).EvaporatingTemp + DXCoil( CoolCoilIndex ).ActualSH ), max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

						if( Pipe_h_IU_out_i > Pipe_h_IU_in  ) {
							Pipe_m_ref_i = ( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) <= 0.0 ) ? 0.0 : ( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) / ( Pipe_h_IU_out_i - Pipe_h_IU_in ) ); //Ref Flow Rate in the IU( kg/s )
							Pipe_m_ref  = Pipe_m_ref + Pipe_m_ref_i;
							Pipe_h_IU_out = Pipe_h_IU_out + Pipe_m_ref_i * Pipe_h_IU_out_i;
							Pipe_SH_merged = Pipe_SH_merged + Pipe_m_ref_i * DXCoil( CoolCoilIndex ).ActualSH;
						}
					}
				}
				if( Pipe_m_ref > 0 ) {
					Pipe_h_IU_out = Pipe_h_IU_out/Pipe_m_ref;
					Pipe_SH_merged = Pipe_SH_merged / Pipe_m_ref;
				} else {
					RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
					Pipe_h_IU_out = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, VRF( VRFCond ).EvaporatingTemp + 3 ), max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
					Pipe_SH_merged = 3;
				}
			}

			// Calculate piping loss
			if ( Pipe_m_ref > 0 ) {
				if( VRF( VRFCond ).RefPipDia <= 0 ) VRF( VRFCond ).RefPipDia = 0.025;
				Ref_Coe_v1 = Pevap/1000000/ 4.926;
				Ref_Coe_v2 = Pipe_h_IU_out / 383.5510343;
				Ref_Coe_v3 = ( VRF( VRFCond ).EvaporatingTemp + Pipe_SH_merged + 273.15 ) / 344.39;
				Pipe_viscosity_ref = 4.302 * Ref_Coe_v1 + 0.81622 * pow_2( Ref_Coe_v1 ) - 120.98 * Ref_Coe_v2 + 139.17 * pow_2( Ref_Coe_v2 ) + 118.76 * Ref_Coe_v3 + 81.04 * pow_2( Ref_Coe_v3 ) + 5.7858 * Ref_Coe_v1 * Ref_Coe_v2 - 8.3817 * Ref_Coe_v1 * Ref_Coe_v3 - 218.48 * Ref_Coe_v2 * Ref_Coe_v3 + 21.58;
				if ( Pipe_viscosity_ref <= 0 ) Pipe_viscosity_ref = 16.26; // default superheated vapor viscosity data (MuPa·s) at T=353.15 K, P=2MPa

				Pipe_v_ref  = Pipe_m_ref / ( 3.141593 * pow_2( VRF( VRFCond ).RefPipDia ) * 0.25 ) / GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp+Pipe_SH_merged, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				Pipe_Num_Re = Pipe_m_ref / ( 3.141593 * pow_2( VRF( VRFCond ).RefPipDia ) * 0.25 ) * VRF( VRFCond ).RefPipDia / Pipe_viscosity_ref * 1000000;
				Pipe_Num_Pr = Pipe_viscosity_ref * Pipe_cp_ref * 0.001 / Pipe_conductivity_ref;
				Pipe_Num_Nu = 0.023 * std::pow( Pipe_Num_Re, 0.8) * std::pow( Pipe_Num_Pr, 0.3 );
				Pipe_Num_St = Pipe_Num_Nu / Pipe_Num_Re / Pipe_Num_Pr;

				Pipe_DeltP = max( 0.0, 8 * Pipe_Num_St * std::pow( Pipe_Num_Pr, 0.6667 ) * VRF( VRFCond ).RefPipEquLen / VRF( VRFCond ).RefPipDia * GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp + Pipe_SH_merged, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) * pow_2( Pipe_v_ref ) / 2 - VRF( VRFCond ).RefPipHei*GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp + Pipe_SH_merged, max( min( Pevap, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) *9.80665 );

				Pipe_Coe_k1 = Pipe_Num_Nu * Pipe_viscosity_ref;
				Pipe_Coe_k3 = RefPipInsH *( VRF( VRFCond ).RefPipDia + 2 * VRF( VRFCond ).RefPipInsThi );
				if ( VRF( VRFCond ).RefPipInsThi >= 0.0 ) {
					Pipe_Coe_k2 = 2 * VRF( VRFCond ).RefPipInsCon / std::log(  1.0 + 2 * VRF( VRFCond ).RefPipInsThi / VRF( VRFCond ).RefPipDia  );
				} else {
					Pipe_Coe_k2 = 9999.9; // 1/k2 is close to 0
				}

				Pipe_Q = max( 0.0, ( 3.141593 * VRF( VRFCond ).RefPipLen ) * ( OutdoorDryBulb / 2 + Pipe_T_room / 2 - VRF( VRFCond ).EvaporatingTemp - Pipe_SH_merged ) / ( 1 / Pipe_Coe_k1 + 1 / Pipe_Coe_k2 + 1 / Pipe_Coe_k3 ) );

				Pipe_h_comp_in = Pipe_h_IU_out + Pipe_Q / Pipe_m_ref;

			} else {
				Pipe_DeltP = 0;
				Pipe_Q = 0;
				Pipe_h_comp_in = Pipe_h_IU_out;
			}

			Tsuction = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pevap - Pipe_DeltP, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

			// Perform iteration to calculate Pipe_T_Suction
			for ( Pipe_T_comp_in = Tsuction + 3; Pipe_T_comp_in <= Tsuction + 30; Pipe_T_comp_in++ ){
				RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pevap - Pipe_DeltP, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				Pipe_h_comp_in_assumed = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Pipe_T_comp_in ), max( min( Pevap - Pipe_DeltP, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

				if( Pipe_h_comp_in_assumed > Pipe_h_comp_in )  break;
			}
			if( Pipe_T_comp_in > ( Tsuction +30 ) ) Pipe_T_comp_in = Tsuction + 3;

			Modifi_SH = Pipe_T_comp_in - Tsuction; //This Modifi_SH is used for rps > min; will be updated for rps = min
			Modifi_Pe = Pevap - Pipe_DeltP; //This Modifi_Pe is used for rps > min; will be updated for rps = min

			TUCoolingLoad = TUCoolingLoad_temp + Pipe_Q;
			OUCondHeatRelease = TUCoolingLoad + CompEvaporatingPWRSpdMin;

			// Calculate capacity modification factor
			C_cap_density = GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Tsuction + 8, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
							/ GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Tsuction + Modifi_SH, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			C_cap_enthalpy = abs( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Tsuction + 8 ), max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
							- GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, MinOutdoorUnitTc - 5, 0.0, RefrigerantIndex, RoutineName ) ) / abs( Pipe_h_comp_in - Pipe_h_IU_in );
			C_cap_operation = C_cap_density * C_cap_enthalpy;

			if( TUCoolingLoad * C_cap_operation <= CompEvaporatingCAPSpdMin ) {
			// Required cooling load is less than the min cooling capacity, on-off strategy
				VRFOperationSimPath = 1;

				CyclingRatio = TUCoolingLoad * C_cap_operation / CompEvaporatingCAPSpdMin;
				double CyclingRatioFrac = 0.85 + 0.15 * CyclingRatio;
				double HPRTF = CyclingRatio / CyclingRatioFrac;
				NcompCooling = CompEvaporatingPWRSpdMin * HPRTF;
				CompSpdActual = VRF( VRFCond ).CompressorSpeed( 1 );
				VRF( VRFCond ).CondensingTemp = MinOutdoorUnitTc;

			} else {
			// Required cooling load is greater than or equal to the min cooling capacity

				Label10: ;
				OUCondHeatRelease = TUCoolingLoad + NcompPriCooling;

				// VRF OU air side calculations
				TcondOut = OutdoorDryBulb + OUCondHeatRelease / 1005.0 / VRF( VRFCond ).OUAirFlowRate / RhoAir;
				Tcondh2  = OutdoorDryBulb + ( TcondOut - OutdoorDryBulb ) / ( 1 - BFC );
				deltaT = VRF( VRFCond ).C3Tc * pow_2( VRF( VRFCond ).SC ) + VRF( VRFCond ).C2Tc * VRF( VRFCond ).SC + VRF( VRFCond ).C1Tc;
				VRF( VRFCond ).CondensingTemp = Tcondh2 + deltaT;

				// Iteration to find the VRF speed that can meet the required load
				for ( CounterCompSpdTemp = 1; CounterCompSpdTemp <= NumOfCompSpdInput; CounterCompSpdTemp++ ){

					CompEvaporatingPWRSpd( CounterCompSpdTemp ) = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( CounterCompSpdTemp ), VRF( VRFCond ).CondensingTemp, Tsuction );
					CompEvaporatingCAPSpd( CounterCompSpdTemp ) = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( CounterCompSpdTemp ), VRF( VRFCond ).CondensingTemp, Tsuction );

					C_cap_density = GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Tsuction + 8, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
								  / GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Tsuction + Modifi_SH, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
					RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
					C_cap_enthalpy = ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Tsuction + 8 ), max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
									- GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).CondensingTemp - 5 ,  0.0, RefrigerantIndex, RoutineName ) ) / ( Pipe_h_comp_in - Pipe_h_IU_in );
					C_cap_operation = C_cap_density * C_cap_enthalpy;

					if( TUCoolingLoad * C_cap_operation <= CompEvaporatingCAPSpd( CounterCompSpdTemp ) ) {
						// Compressor speed stage CounterCompSpdTemp need not to be increased
						VRFOperationSimPath = 2; //0924

						if( CounterCompSpdTemp <= 1 ) {
						// Compressor runs at the min speed

							//Initialization of NumIteCcap iterations (Label13)
							Pipe_Q0 = Pipe_Q;
							C_cap_operation0 = C_cap_operation;
							Tc0 = VRF( VRFCond ).CondensingTemp;
							NumIteCcap = 1;

							//Update the C_cap_operation
							Label13: ;
							TUCoolingLoad = TUCoolingLoad_temp + Pipe_Q0; //Pipe_Q0 is updated during the iteration
							Pipe_h_IU_in  = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, Tc0 - VRF( VRFCond ).SC,  0.0, RefrigerantIndex, RoutineName );
							CompSpdActual = VRF( VRFCond ).CompressorSpeed( 1 );
							Par( 1 ) = Tc0;
							Par( 2 ) = TUCoolingLoad * C_cap_operation0 / VRF( VRFCond ).RatedEvapCapacity;  // 150130 To be confirmed
							Par( 3 ) = VRF( VRFCond ).OUCoolingCAPFT( CounterCompSpdTemp );

							// Update Te'( MinOutdoorUnitTe ) to meet the required evaporator capacity
							Pdischarge = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).CondensingTemp, RefrigerantIndex, RoutineName );

							MinRefriPe = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, -15, RefrigerantIndex, RoutineName );
							MinOutdoorUnitPe = max( Pdischarge - VRF( VRFCond ).CompMaxDeltaP, MinRefriPe );
							MinOutdoorUnitTe = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

							SolveRegulaFalsi( 1.0e-3, MaxIter, SolFla, SmallLoadTe, CompResidual_FluidTCtrl, MinOutdoorUnitTe, Tsuction, Par ); // SmallLoadTe is the updated Te'
							if( SolFla < 0 ) SmallLoadTe = 6; //MinOutdoorUnitTe; //SmallLoadTe( Te'_new ) is constant during iterations

							//Initialization of Te iterations (Label11)
							NumIteTe = 1;
							Pipe_Te_assumed = VRF( VRFCond ).EvaporatingTemp - 0.1;
							Label11: ;
							Pipe_m_ref = 0; // Total Ref Flow Rate( kg/s )

							// Re-calculate Piping loss due to the Te and SH updates
							Pipe_h_IU_out = 0;
							Pipe_h_IU_out_i = 0;
							Pipe_m_ref_i = 0;
							Pipe_SH_merged = 0;
							Pipe_Pe_assumed = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, Pipe_Te_assumed, RefrigerantIndex, RoutineName );

							// Re-calculate total refrigerant flow rate
							for ( NumTU = 1; NumTU <= NumTUInList; NumTU++ ){
								if( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) > 0 ) {
									TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
									CoolCoilIndex = VRFTU( TUIndex ).CoolCoilIndex;

								  	Tfs = VRF( VRFCond ).EvaporatingTemp +( VRF( VRFCond ).C3Te * pow_2( DXCoil( CoolCoilIndex ).ActualSH ) + VRF( VRFCond ).C2Te * DXCoil( CoolCoilIndex ).ActualSH + VRF( VRFCond ).C1Te );

									// Modifi_SH is the updated SH for a specific IU
									if( VRF( VRFCond ).C3Te == 0 )
										Modifi_SHin = -( VRF( VRFCond ).C1Te - Tfs + Pipe_Te_assumed ) /VRF( VRFCond ).C2Te; //150130 Modifi_SH>Modifi_SHin
									else
										Modifi_SHin = ( -VRF( VRFCond ).C2Te + std::pow( ( pow_2( VRF( VRFCond ).C2Te ) - 4 * ( VRF( VRFCond ).C1Te - Tfs + Pipe_Te_assumed ) * VRF( VRFCond ).C3Te) , 0.5 ) ) / ( 2*VRF( VRFCond ).C3Te );

									RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
									Pipe_h_IU_out_i = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Pipe_Te_assumed + Modifi_SHin ), max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ); // hB_i for the IU

									if( Pipe_h_IU_out_i > Pipe_h_IU_in ) {
										Pipe_m_ref_i = ( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) <= 0.0 ) ? 0.0 : ( TerminalUnitList( TUListNum ).TotalCoolLoad( NumTU ) / ( Pipe_h_IU_out_i - Pipe_h_IU_in ) );
										Pipe_m_ref = Pipe_m_ref + Pipe_m_ref_i;
										Pipe_SH_merged = Pipe_SH_merged + Pipe_m_ref_i * Modifi_SHin;
										Pipe_h_IU_out = Pipe_h_IU_out + Pipe_m_ref_i * Pipe_h_IU_out_i;
									}
							    }
							}
							if( Pipe_m_ref > 0 ) {
								Pipe_h_IU_out = Pipe_h_IU_out/Pipe_m_ref;
								Pipe_SH_merged = Pipe_SH_merged /Pipe_m_ref;
							} else {
								Pipe_SH_merged = VRF( VRFCond ).SH; //SH_merged_new  150130 Nedds to be confirmed
								RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
								Pipe_h_IU_out = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Pipe_Te_assumed + Pipe_SH_merged ), max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
							}

							// Re-calculate piping loss
							if ( Pipe_m_ref > 0 ) {
								if( VRF( VRFCond ).RefPipDia <= 0 ) VRF( VRFCond ).RefPipDia = 0.025; // Default value, [m]
								Ref_Coe_v1 = Pipe_Pe_assumed / 1000000 / 4.926;
								Ref_Coe_v2 = Pipe_h_IU_out / 383.5510343;
								Ref_Coe_v3 = ( Pipe_Te_assumed + Pipe_SH_merged + 273.15 ) / 344.39;
								Pipe_viscosity_ref = 4.302 * Ref_Coe_v1 + 0.81622 * pow_2( Ref_Coe_v1 ) - 120.98 * Ref_Coe_v2+ 139.17 * pow_2( Ref_Coe_v2 ) + 118.76 * Ref_Coe_v3 + 81.04 * pow_2( Ref_Coe_v3 ) + 5.7858 * Ref_Coe_v1 * Ref_Coe_v2- 8.3817 * Ref_Coe_v1 * Ref_Coe_v3 - 218.48 * Ref_Coe_v2* Ref_Coe_v3 + 21.58;
								if ( Pipe_viscosity_ref <= 0 ) Pipe_viscosity_ref = 16.26; // default superheated vapor viscosity data (MuPa·s) at T=353.15 K, P=2MPa

								Pipe_v_ref  = Pipe_m_ref / ( 3.141593 * pow_2( VRF( VRFCond ).RefPipDia ) * 0.25 ) / GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Pipe_Te_assumed + Pipe_SH_merged, max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
								Pipe_Num_Re = Pipe_m_ref / ( 3.141593 * pow_2( VRF( VRFCond ).RefPipDia ) * 0.25 ) * VRF( VRFCond ).RefPipDia / Pipe_viscosity_ref * 1000000;
								Pipe_Num_Pr = Pipe_viscosity_ref * Pipe_cp_ref * 0.001 / Pipe_conductivity_ref;
								Pipe_Num_Nu = 0.023 * std::pow( Pipe_Num_Re, 0.8 ) * std::pow( Pipe_Num_Pr, 0.3 );
								Pipe_Num_St = Pipe_Num_Nu / Pipe_Num_Re/Pipe_Num_Pr;

								Pipe_DeltP = max( 0.0, 8 * Pipe_Num_St * std::pow( Pipe_Num_Pr, 0.6667 ) * VRF( VRFCond ).RefPipEquLen
											/ VRF( VRFCond ).RefPipDia * GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Pipe_Te_assumed+Pipe_SH_merged, max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) * pow_2( Pipe_v_ref ) / 2 - VRF( VRFCond ).RefPipHei*GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Pipe_Te_assumed+Pipe_SH_merged, max( min( Pipe_Pe_assumed, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) *9.80665 );

								Pipe_Coe_k1 = Pipe_Num_Nu * Pipe_viscosity_ref;
								Pipe_Coe_k3 = RefPipInsH *( VRF( VRFCond ).RefPipDia + 2*VRF( VRFCond ).RefPipInsThi );
								if( VRF( VRFCond ).RefPipInsThi >= 0.0 )
									Pipe_Coe_k2 = 2 * VRF( VRFCond ).RefPipInsCon / std::log( 1.0 + 2 * VRF( VRFCond ).RefPipInsThi / VRF( VRFCond ).RefPipDia );
								else
									Pipe_Coe_k2 = 9999.9; //1/k2 is close to 0

								Pipe_Q = max( 0.0, ( 3.141593 * VRF( VRFCond ).RefPipLen ) * ( OutdoorDryBulb / 2 + Pipe_T_room / 2 - Pipe_Te_assumed - Pipe_SH_merged ) / ( 1 / Pipe_Coe_k1 + 1 / Pipe_Coe_k2 + 1 / Pipe_Coe_k3 ) );

								Pipe_h_comp_in = Pipe_h_IU_out + Pipe_Q / Pipe_m_ref;

							} else {
								Pipe_DeltP = 0;
								Pipe_Q = 0;
								Pipe_h_comp_in = Pipe_h_IU_out;
							}

							Tsuction = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pipe_Pe_assumed - Pipe_DeltP, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

							MaxNumIteTe = ( VRF( VRFCond ).EvaporatingTemp - SmallLoadTe ) / 0.1 + 1;
							if( ( abs( Tsuction - SmallLoadTe ) > 0.5 ) && ( Pipe_Te_assumed < VRF( VRFCond ).EvaporatingTemp ) && ( Pipe_Te_assumed > SmallLoadTe ) && ( NumIteTe < MaxNumIteTe ) ){
								Pipe_Te_assumed = Pipe_Te_assumed - 0.1;
							    NumIteTe = NumIteTe + 1;
							    goto Label11;
							}

							if( abs( Tsuction - SmallLoadTe ) > 0.5 ) {
								NumIteTe = 999;
								Tsuction = SmallLoadTe;
								Pipe_SH_merged = 3.0;
								Pipe_Te_assumed = SmallLoadTe + 1;
							}
							//Iteration_Te End

							//Post-process with new Te( Pipe_Te_assumed ) and Tsuction( Te'_new2 ), Pipe_h_IU_out, Pipe_Q, Pipe_m_ref
							//SH'_Start: suction SH'( Pipe_T_comp_in - Tsuction ) is calculated, then  Modifi_SH will be updated
							// Pipe_h_comp_in = Pipe_h_IU_out + Pipe_Q / Pipe_m_ref; //Pipe_h_comp_in is the enthalpy at the inlet of compressor

							//Perform iteration to calculate Pipe_T_comp_in( Te'+SH' )
							for ( Pipe_T_comp_in = Tsuction +3; Pipe_T_comp_in <= Tsuction +30; Pipe_T_comp_in++ ) {
								RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pipe_Pe_assumed - Pipe_DeltP, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
								Pipe_h_comp_in_assumed = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Pipe_T_comp_in ), max( min( Pipe_Pe_assumed - Pipe_DeltP, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
								if( Pipe_h_comp_in_assumed > Pipe_h_comp_in ) break;
							}

							if( Pipe_T_comp_in >( Tsuction +30 ) ) Pipe_T_comp_in = Tsuction + 3;

							Modifi_SH = Pipe_T_comp_in - Tsuction;
							Modifi_Pe = Pipe_Pe_assumed - Pipe_DeltP;
							OUCondHeatRelease = TUCoolingLoad_temp + Pipe_Q + NcompPriCooling; //Pipe_Q is changed when Tsuction is changed ->Tc is also changed
							TcondOut = OutdoorDryBulb + OUCondHeatRelease / 1005.0 / VRF( VRFCond ).OUAirFlowRate / RhoAir;
							Tcondh2  = OutdoorDryBulb + ( TcondOut-OutdoorDryBulb ) / ( 1 - BFC );
							deltaT = VRF( VRFCond ).C3Tc * pow_2( VRF( VRFCond ).SC ) + VRF( VRFCond ).C2Tc * VRF( VRFCond ).SC + VRF( VRFCond ).C1Tc;
							VRF( VRFCond ).CondensingTemp = min( Tcondh2 + deltaT, MaxOutdoorUnitTc );

							C_cap_density = GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Tsuction + 8,max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
											/ GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Tsuction + Modifi_SH, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
							RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
							C_cap_enthalpy = ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Tsuction + 8 ), max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
											  - GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).CondensingTemp - 5 ,  0.0, RefrigerantIndex, RoutineName ) )
											  / ( Pipe_h_comp_in - Pipe_h_IU_in );
							C_cap_operation = C_cap_density * C_cap_enthalpy;

							Cap_Eva0 = ( TUCoolingLoad_temp + Pipe_Q ) * C_cap_operation; //New Pipe_Q & C_cap_operation
							Cap_Eva1 = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( CounterCompSpdTemp ), VRF( VRFCond ).CondensingTemp, Tsuction );  //New Tc
							CapDiff = abs( Cap_Eva1 - Cap_Eva0 );

							if( ( CapDiff > ( Tolerance*Cap_Eva0 ) ) && ( NumIteCcap < 30 ) ) {
								Pipe_Q0 = Pipe_Q;
								C_cap_operation0 = C_cap_operation;
								Tc0 = VRF( VRFCond ).CondensingTemp;
								NumIteCcap = NumIteCcap + 1;
								goto Label13;
							}

							if( CapDiff >( Tolerance*Cap_Eva0 ) ) NumIteCcap = 999;

							NcompCooling = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( CounterCompSpdTemp ), VRF( VRFCond ).CondensingTemp, Tsuction );

							break; //EXIT DoName1

						} else {// Since: if( CounterCompSpdTemp <= 1 )
							//Compressor speed > min
							VRFOperationSimPath = 3;//0924

							CompSpdLB = CounterCompSpdTemp - 1;
							CompSpdUB = CounterCompSpdTemp;

							CompSpdActual = VRF( VRFCond ).CompressorSpeed( CompSpdLB ) +( VRF( VRFCond ).CompressorSpeed( CompSpdUB ) - VRF( VRFCond ).CompressorSpeed( CompSpdLB ) )
											  / ( CompEvaporatingCAPSpd( CompSpdUB ) - CompEvaporatingCAPSpd( CompSpdLB ) ) * ( TUCoolingLoad * C_cap_operation - CompEvaporatingCAPSpd( CompSpdLB ) );

							NcompCooling = CompEvaporatingPWRSpd( CompSpdLB ) + ( CompEvaporatingPWRSpd( CompSpdUB ) - CompEvaporatingPWRSpd( CompSpdLB ) ) /
										  ( VRF( VRFCond ).CompressorSpeed( CompSpdUB ) - VRF( VRFCond ).CompressorSpeed( CompSpdLB ) ) *
										  ( CompSpdActual - VRF( VRFCond ).CompressorSpeed( CompSpdLB ) );
							break; //EXIT DoName1

						} // End: if( CounterCompSpdTemp <= 1 )

					}// End: if( TUCoolingLoad <= CompEvaporatingCAPSpd( CounterCompSpdTemp ) )

				} // END DO DoName1

				CompEvaporatingCAPSpd( NumOfCompSpdInput ) = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( NumOfCompSpdInput ), VRF( VRFCond ).CondensingTemp, Tsuction );
				if( CounterCompSpdTemp > NumOfCompSpdInput ) {
				// Required load is beyond the maximum system capacity
				// TUCoolingLoad * C_cap_operation > CompEvaporatingCAPSpd( NumOfCompSpdInput )
				// Required cooling load is beyond the maximum system capacity
					NcompCooling = CompEvaporatingPWRSpd( NumOfCompSpdInput );
					CompSpdActual = VRF( VRFCond ).CompressorSpeed( NumOfCompSpdInput );
					OUCondHeatRelease = NcompCooling + CompEvaporatingCAPSpd( NumOfCompSpdInput );
				}

				NcompDiff = ( NcompCooling - NcompPriCooling ) / NcompPriCooling;
				if( ( abs( NcompDiff ) > Tolerance ) && ( Counter < 30 ) ) {
					NcompPriCooling = NcompCooling;
					Counter = Counter + 1;
					goto Label10;
				}

			} //if( TUCoolingLoad <= CompEvaporatingCAPSpdMin )

			Pipe_h_IU_in_new  = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).CondensingTemp-VRF( VRFCond ).SC,  0.0, RefrigerantIndex, RoutineName ); // Tc

			if( ( abs( Pipe_h_IU_in - Pipe_h_IU_in_new ) > 0.05*Pipe_h_IU_in ) &&( Pipe_h_IU_in < Pipe_h_IU_in_up ) &&( Pipe_h_IU_in > Pipe_h_IU_in_low ) ) {
				Pipe_h_IU_in = Pipe_h_IU_in_new;
				NumIteHIUIn = NumIteHIUIn + 1;
				goto Label12;
			}

			if( ( abs( Pipe_h_IU_in - Pipe_h_IU_in_new ) > 0.05*Pipe_h_IU_in ) )  {
				Pipe_h_IU_in = 0.5*( Pipe_h_IU_in_low + Pipe_h_IU_in_up );
				NumIteHIUIn = 100;
			} else {
				Pipe_h_IU_in = ( Pipe_h_IU_in + Pipe_h_IU_in_new ) / 2;
				NumIteHIUIn = 200;
			}

			if( Pipe_h_IU_in > Pipe_h_IU_in_up ) {
				Pipe_h_IU_in = Pipe_h_IU_in_up ;
				NumIteHIUIn = 300;
			} else if( Pipe_h_IU_in < Pipe_h_IU_in_low ) {
				Pipe_h_IU_in = Pipe_h_IU_in_low;
				NumIteHIUIn = 400;
			}

			VRF( VRFCond ).CompActSpeed = max( CompSpdActual,0.0 );
			VRF( VRFCond ).NcompCooling = max( NcompCooling,0.0 ) / 0.95; // 0.95 is the efficiency of the compressor inverter, coming from IDF input
			VRF( VRFCond ).CondFanPower = VRF( VRFCond ).RatedCondFanPower * pow_3( CondFlowRatio ); //@@
			VRF( VRFCond ).VRFCondCyclingRatio = CyclingRatio; // report variable for cycling rate

			// VRF( VRFCond ).IUEvaporatingTemp = Tsuction;
			VRF( VRFCond ).CoolingCapacity = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( NumOfCompSpdInput ), VRF( VRFCond ).CondensingTemp, Tsuction ); // Include the piping loss
			VRF( VRFCond ).PipingCorrectionCooling = TUCoolingLoad_temp / ( TUCoolingLoad_temp + Pipe_Q );
			MaxCoolingCapacity( VRFCond ) = VRF( VRFCond ).CoolingCapacity; // for report

		// 2. HEATING MODE
		} else if ( HeatingLoad( VRFCond ) && ( TUHeatingLoad > 0.0 ) ) {

			TUHeatingLoad_temp = TUHeatingLoad;

			MinOutdoorUnitTe = OutdoorDryBulb - VRF( VRFCond ).SH;
			CompEvaporatingCAPSpdMax = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( NumOfCompSpdInput ), VRF( VRFCond ).IUCondensingTemp, MinOutdoorUnitTe );
			CompEvaporatingPWRSpdMax = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( NumOfCompSpdInput ), VRF( VRFCond ).IUCondensingTemp, MinOutdoorUnitTe );

			// Calculate avrage room temperature
			Pipe_T_room = 0;
			NumIUActivated = 0;
			for ( NumTU = 1; NumTU <= NumTUInList; ++NumTU ) {
				TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
				HeatCoilIndex = VRFTU( TUIndex ).HeatCoilIndex;

				if( DXCoil( HeatCoilIndex ).TotalHeatingEnergyRate > 0.0 ){
					Pipe_T_room = Pipe_T_room + DXCoil( HeatCoilIndex ).InletAirTemp;
					NumIUActivated = NumIUActivated + 1;
				}
			}
			if( NumIUActivated > 0 )
				Pipe_T_room = Pipe_T_room / NumIUActivated;
			else
				Pipe_T_room = 18;

			// Initialization of Pipe_h_comp_out iterations (Label23)
			Pipe_h_IU_in_low = GetSatEnthalpyRefrig ( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).IUCondensingTemp, 1.0,  RefrigerantIndex, RoutineName ); // Quality=1
			RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			Pipe_h_IU_in_up = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, min( VRF( VRFCond ).IUCondensingTemp + 50, RefTHigh )), max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			Pipe_h_IU_in = Pipe_h_IU_in_low;

			Label23: ;
			Pipe_m_ref = 0;
			Pipe_h_out_ave = 0;
			SC_ave = 0;

			// Calculate total refrigerant flow rate
			if( TUHeatingLoad > CompEvaporatingCAPSpdMax + CompEvaporatingPWRSpdMax ){
				// Required load is beyond the max system capacity

				TUHeatingLoad = CompEvaporatingCAPSpdMax;
				TUHeatingLoad_temp = CompEvaporatingCAPSpdMax;
				VRF( VRFCond ).TUHeatingLoad = TUHeatingLoad;
				Pipe_h_IU_out = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) - 5.0, 0.0, RefrigerantIndex, RoutineName ); //Quality=0
				Pipe_h_out_ave = Pipe_h_IU_out;
				SC_ave = 5;
				Pipe_m_ref = TUCoolingLoad / ( Pipe_h_IU_in - Pipe_h_IU_out );

			} else {
				for ( NumTU = 1; NumTU <= NumTUInList; NumTU++ ) {
					if( TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) > 0 ) {
						TUIndex = TerminalUnitList( TUListNum ).ZoneTUPtr( NumTU );
						HeatCoilIndex = VRFTU( TUIndex ).HeatCoilIndex;
						Pipe_h_out_i = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) - DXCoil( HeatCoilIndex ).ActualSC, 0.0, RefrigerantIndex, RoutineName ); //Quality=0
						Pipe_m_ref_i = ( TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) <= 0.0 ) ? 0.0 : ( TerminalUnitList( TUListNum ).TotalHeatLoad( NumTU ) / ( Pipe_h_IU_in - Pipe_h_out_i ) );
						Pipe_m_ref = Pipe_m_ref + Pipe_m_ref_i;
						Pipe_h_out_ave = Pipe_h_out_ave + Pipe_m_ref_i * Pipe_h_out_i;
						SC_ave = SC_ave + Pipe_m_ref_i * DXCoil( HeatCoilIndex ).ActualSC;
					}
				}
				if( Pipe_m_ref > 0 ) {
					Pipe_h_out_ave = Pipe_h_out_ave / Pipe_m_ref; //h_merge
					SC_ave = SC_ave / Pipe_m_ref; //SC_merged  0923: theoreticaly, it is not correct. It should be calculated from Pipe_h_IU_out & Pe
				} else {
					Pipe_h_out_ave = GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) - 5.0, 0.0, RefrigerantIndex, RoutineName );  //Quality=0
					SC_ave = 5;
				}
			}

			//Perform iteration to calculate Pipe_T_IU_in
			for ( Pipe_T_IU_in = VRF( VRFCond ).IUCondensingTemp; Pipe_T_IU_in <= min( VRF( VRFCond ).IUCondensingTemp + 50, RefTHigh ); Pipe_T_IU_in++ ) {
				RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				Pipe_h_IU_in_temp = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Pipe_T_IU_in ), max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				if( Pipe_h_IU_in_temp > Pipe_h_IU_in ) break; //EXIT Do_Pipe_T_IU_in
			}
			if ( Pipe_T_IU_in > min( VRF( VRFCond ).IUCondensingTemp + 50, RefTHigh ) ) {
				Pipe_T_IU_in = VRF( VRFCond ).IUCondensingTemp;
			}

			// Calculate piping loss
			if ( Pipe_m_ref > 0 ) {
				Ref_Coe_v1 = Pcond / 1000000 / 4.926;
				Ref_Coe_v2 = Pipe_h_IU_in / 383.5510343;
				Ref_Coe_v3 = ( Pipe_T_IU_in + 273.15 ) / 344.39;
				Pipe_viscosity_ref = 4.302 * Ref_Coe_v1 + 0.81622 * pow_2( Ref_Coe_v1 ) - 120.98 * Ref_Coe_v2+ 139.17 * pow_2( Ref_Coe_v2 ) + 118.76 * Ref_Coe_v3 + 81.04 * pow_2( Ref_Coe_v3 ) + 5.7858 * Ref_Coe_v1 * Ref_Coe_v2- 8.3817 * Ref_Coe_v1 * Ref_Coe_v3 - 218.48 * Ref_Coe_v2* Ref_Coe_v3 + 21.58;
				if ( Pipe_viscosity_ref <= 0 ) Pipe_viscosity_ref = 16.26; // default superheated vapor viscosity data (MuPa·s) at T=353.15 K, P=2MPa

				Pipe_v_ref = Pipe_m_ref / ( 3.141593 * pow_2( VRF( VRFCond ).RefPipDia ) * 0.25 ) / GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Pipe_T_IU_in, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				Pipe_Num_Re = Pipe_m_ref / ( 3.141593 * pow_2( VRF( VRFCond ).RefPipDia ) * 0.25 ) * VRF( VRFCond ).RefPipDia / Pipe_viscosity_ref * 1000000;
				Pipe_Num_Pr = Pipe_viscosity_ref * Pipe_cp_ref * 0.001 / Pipe_conductivity_ref;
				Pipe_Num_Nu = 0.023 * std::pow( Pipe_Num_Re, 0.8) * std::pow( Pipe_Num_Pr, 0.4);
				Pipe_Num_St = Pipe_Num_Nu / Pipe_Num_Re / Pipe_Num_Pr;

				Pipe_Coe_k1 = Pipe_Num_Nu * Pipe_viscosity_ref;
				Pipe_Coe_k2 = VRF( VRFCond ).RefPipInsCon * ( VRF( VRFCond ).RefPipDia + VRF( VRFCond ).RefPipInsThi ) /VRF( VRFCond ).RefPipInsThi;
				Pipe_Coe_k3 = RefPipInsH   * ( VRF( VRFCond ).RefPipDia + 2 * VRF( VRFCond ).RefPipInsThi );

				Pipe_Q = max( 0.0, ( 3.141593 * VRF( VRFCond ).RefPipLen ) * ( Pipe_T_IU_in - OutdoorDryBulb / 2 - Pipe_T_room / 2 ) / ( 1 / Pipe_Coe_k1 + 1 / Pipe_Coe_k2 + 1 / Pipe_Coe_k3 ) ); // [W]
				Pipe_DeltP = max( 0.0, 8 * Pipe_Num_St * std::pow( Pipe_Num_Pr, 0.6667) * VRF( VRFCond ).RefPipEquLen / VRF( VRFCond ).RefPipDia * GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName,
						Pipe_T_IU_in, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
						* pow_2( Pipe_v_ref ) / 2 - VRF( VRFCond ).RefPipHei * GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, Pipe_T_IU_in, max( min( Pcond, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) * 9.80665 );

				Pipe_h_comp_out = Pipe_h_IU_in + Pipe_Q / Pipe_m_ref;

			} else {
				Pipe_DeltP = 0;
				Pipe_Q = 0;
				Pipe_h_comp_out = Pipe_h_IU_in;
			}

			Pdischarge = max( Pcond + Pipe_DeltP, Pcond );
			Tdischarge = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pdischarge, RefPHigh ), RefPLow), RefrigerantIndex, RoutineName );

			CondFlowRatio = 1.0;
			MinRefriPe = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, -15, RefrigerantIndex, RoutineName );
			MinOutdoorUnitPe = min( Pdischarge - VRF( VRFCond ).CompMaxDeltaP, MinRefriPe );
			MinOutdoorUnitTe = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			CompEvaporatingCAPSpdMin = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( 1 ), Tdischarge, MinOutdoorUnitTe );
			CompEvaporatingPWRSpdMin = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( 1 ), Tdischarge, MinOutdoorUnitTe );

			TUHeatingLoad = TUHeatingLoad_temp + Pipe_Q;
			OUEvapHeatExtract = max( 0.0, TUHeatingLoad - CompEvaporatingPWRSpdMin);

			C_cap_density = GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, MinOutdoorUnitTe + 8,   max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
							/ GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, MinOutdoorUnitTe + VRF( VRFCond ).SH, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
			C_cap_enthalpy = ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, MinOutdoorUnitTe + 8 ), max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName )
							- GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, IUMaxCondTemp - 5 , 0.0, RefrigerantIndex, RoutineName ) )
							/ ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, MinOutdoorUnitTe + VRF( VRFCond ).SH ), max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) - Pipe_h_out_ave );
			C_cap_operation = C_cap_density * C_cap_enthalpy;

			if( ( OUEvapHeatExtract * C_cap_operation ) <= CompEvaporatingCAPSpdMin ) {
			//Reuired heating load is smaller than the min heating capacity

				if( OUEvapHeatExtract == 0) {
				// TUHeatingLoad is less than or equal to CompEvaporatingPWRSpdMin
					CyclingRatio = TUHeatingLoad / CompEvaporatingPWRSpdMin;
				} else {
				// TUHeatingLoad is greater than CompEvaporatingPWRSpdMin
					CyclingRatio = OUEvapHeatExtract * C_cap_operation / CompEvaporatingCAPSpdMin;
				}

				double CyclingRatioFrac = 0.85 + 0.15 * CyclingRatio;
				double HPRTF = CyclingRatio / CyclingRatioFrac;
				NcompHeating = CompEvaporatingPWRSpdMin * HPRTF;
				CompSpdActual = VRF( VRFCond ).CompressorSpeed( 1 );
				VRF( VRFCond ).EvaporatingTemp = MinOutdoorUnitTe;

			} else {
			//Reuired heating load is greater than or equal to the min heating capacity

				// Perform iterations to calculate NcompHeating (Label20)
				Label20: ;
				// VRF OU coil analysis
				OUEvapHeatExtract = max( 0.0, TUHeatingLoad - NcompPriHeating);
				Houtdoor = PsyHFnTdbW( OutdoorDryBulb, OutdoorHumRat );
				Hfs = Houtdoor - OUEvapHeatExtract / ( VRF( VRFCond ).OUAirFlowRate * RhoAir ) / ( 1 - BFH );
				if( Hfs < 0.01 ) Hfs = 0.01;
				TfsSat = PsyTsatFnHPb( Hfs, OutdoorPressure, "CalcVRFCondenser_FluidTCtrl" );
				WfsSat = PsyWFnTdbH( TfsSat, Hfs, "CalcVRFCondenser_FluidTCtrl" );
				if( WfsSat < OutdoorHumRat )
				   Tfs = TfsSat;
				else
				   Tfs = PsyTdbFnHW( Hfs, OutdoorHumRat );
				deltaT = VRF( VRFCond ).C3Te * pow_2( VRF( VRFCond ).SH ) + VRF( VRFCond ).C2Te * VRF( VRFCond ).SH + VRF( VRFCond ).C1Te;
				VRF( VRFCond ).EvaporatingTemp = Tfs - deltaT;

				// Perform iterations to find the compressor speed that can meet the required heating load
				for ( CounterCompSpdTemp = 1; CounterCompSpdTemp <= NumOfCompSpdInput; CounterCompSpdTemp++ ) {

					CompEvaporatingPWRSpd( CounterCompSpdTemp ) = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( CounterCompSpdTemp ), Tdischarge, VRF( VRFCond ).EvaporatingTemp );
					CompEvaporatingCAPSpd( CounterCompSpdTemp ) = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( CounterCompSpdTemp ), Tdischarge, VRF( VRFCond ).EvaporatingTemp );

					MinOutdoorUnitPe = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp, RefrigerantIndex, RoutineName );

					C_cap_density = GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp + 8, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex,
									RoutineName ) / GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp + VRF( VRFCond ).SH, max( min( MinOutdoorUnitPe, RefPHigh ),
									RefPLow ), RefrigerantIndex, RoutineName );
					RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
					C_cap_enthalpy = ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, VRF( VRFCond ).EvaporatingTemp + 8 ), max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ),
									RefrigerantIndex, RoutineName ) - GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName,  IUMaxCondTemp - 5 , 0.0, RefrigerantIndex, RoutineName ) )
									/ ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, VRF( VRFCond ).EvaporatingTemp + VRF( VRFCond ).SH ), max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) - Pipe_h_out_ave );
					C_cap_operation = C_cap_density * C_cap_enthalpy;

					if( ( OUEvapHeatExtract * C_cap_operation ) <= CompEvaporatingCAPSpd( CounterCompSpdTemp ) ) {
					//Compressor Capacity is greater than the required

						if( CounterCompSpdTemp <= 1 ) {
						//Compressor runs at the min speed

							NumIteCcap = 1;

							Label19: ;
							OUEvapHeatExtract = max( 0.0, TUHeatingLoad - NcompPriHeating );

							CompSpdActual = VRF( VRFCond ).CompressorSpeed( 1 );
							Par( 1 ) = Tdischarge;
							Par( 2 ) = OUEvapHeatExtract * C_cap_operation / VRF( VRFCond ).RatedEvapCapacity;
							Par( 3 ) = VRF( VRFCond ).OUCoolingCAPFT( CounterCompSpdTemp );

							SolveRegulaFalsi( 1.0e-3, MaxIter, SolFla, SmallLoadTe, CompResidual_FluidTCtrl, MinOutdoorUnitTe, VRF( VRFCond ).EvaporatingTemp, Par );

							if( SolFla < 0 ) SmallLoadTe = MinOutdoorUnitTe;

							VRF( VRFCond ).EvaporatingTemp = SmallLoadTe;

							//Update SH and Pe to calculate Modification Factor, which is used to update rps to for N_comp calculations
							if( VRF( VRFCond ).C3Te == 0 )
								Modifi_SH = -( VRF( VRFCond ).C1Te - Tfs + VRF( VRFCond ).EvaporatingTemp ) / VRF( VRFCond ).C2Te;
							else
								Modifi_SH = ( -VRF( VRFCond ).C2Te + std::pow( ( pow_2( VRF( VRFCond ).C2Te ) - 4 * ( VRF( VRFCond ).C1Te - Tfs + VRF( VRFCond ).EvaporatingTemp ) * VRF( VRFCond ).C3Te ), 0.5 ) ) / ( 2*VRF( VRFCond ).C3Te );

							Modifi_Pe = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp, RefrigerantIndex, RoutineName );

							C_cap_density = GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp + 8, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex,
											RoutineName ) / GetSupHeatDensityRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp + Modifi_SH, max( min( Modifi_Pe, RefPHigh ),
											RefPLow ), RefrigerantIndex, RoutineName );
							RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
							RefTSat1 = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
							C_cap_enthalpy = ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, VRF( VRFCond ).EvaporatingTemp + 8 ), max( min( MinOutdoorUnitPe, RefPHigh ), RefPLow ),
											RefrigerantIndex, RoutineName ) - GetSatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, IUMaxCondTemp - 5  , 0.0, RefrigerantIndex, RoutineName ) )
											/ ( GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat1, VRF( VRFCond ).EvaporatingTemp + Modifi_SH ), max( min( Modifi_Pe, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName ) - Pipe_h_out_ave );
							C_cap_operation = C_cap_density * C_cap_enthalpy;

							Cap_Eva0 = ( TUHeatingLoad - NcompPriHeating ) * C_cap_operation;
							Cap_Eva1 = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( CounterCompSpdTemp ), Tdischarge, VRF( VRFCond ).EvaporatingTemp );
							CapDiff = abs( Cap_Eva1 - Cap_Eva0 );

							if( ( CapDiff > ( Tolerance * Cap_Eva0 ) ) && ( NumIteCcap < 30 ) ) {
								NumIteCcap = NumIteCcap + 1;
								goto Label19;
							}
							if( CapDiff >( Tolerance*Cap_Eva0 ) ) NumIteCcap = 999;

							NcompHeating = VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( CounterCompSpdTemp ), Tdischarge, VRF( VRFCond ).EvaporatingTemp );

							break; // EXIT DoName2

						} else {
						//Compressor runs at higher speed than min speed
							CompSpdLB = CounterCompSpdTemp - 1;
							CompSpdUB = CounterCompSpdTemp;

							CompSpdActual = VRF( VRFCond ).CompressorSpeed( CompSpdLB ) +( VRF( VRFCond ).CompressorSpeed( CompSpdUB ) - VRF( VRFCond ).CompressorSpeed( CompSpdLB ) )
											/ ( CompEvaporatingCAPSpd( CompSpdUB ) - CompEvaporatingCAPSpd( CompSpdLB ) ) *( OUEvapHeatExtract*C_cap_operation - CompEvaporatingCAPSpd( CompSpdLB ) );
							Modifi_SH = VRF( VRFCond ).SH;
							NcompHeating = CompEvaporatingPWRSpd( CompSpdLB ) +( CompEvaporatingPWRSpd( CompSpdUB ) - CompEvaporatingPWRSpd( CompSpdLB ) ) /
										  ( VRF( VRFCond ).CompressorSpeed( CompSpdUB ) - VRF( VRFCond ).CompressorSpeed( CompSpdLB ) ) *
										  ( CompSpdActual - VRF( VRFCond ).CompressorSpeed( CompSpdLB ) );

							break; // EXIT DoName2

						} //Since: if( CounterCompSpdTemp <= 1 )

					} //Since: if( OUEvapHeatExtract <= CompEvaporatingCAPSpd( CounterCompSpdTemp ) )

				} // END DO DoName2

				CompEvaporatingCAPSpd( NumOfCompSpdInput ) = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( NumOfCompSpdInput ), Tdischarge, VRF( VRFCond ).EvaporatingTemp );
				if ( CounterCompSpdTemp > NumOfCompSpdInput ) {
				// CounterCompSpdTemp > NumOfCompSpdInput
				// OUEvapHeatExtract * C_cap_operation > CompEvaporatingCAPSpd( NumOfCompSpdInput )
				// Required heating load is beyond the maximum system capacity
					NcompHeating = CompEvaporatingPWRSpd( NumOfCompSpdInput );
					CompSpdActual = VRF( VRFCond ).CompressorSpeed( NumOfCompSpdInput );
					OUEvapHeatExtract = CompEvaporatingCAPSpd( NumOfCompSpdInput );
				}

				NcompDiff = ( NcompHeating - NcompPriHeating ) / NcompPriHeating;
				if ( ( abs( NcompDiff ) > Tolerance ) && ( Counter < 30 ) ) {
					NcompPriHeating = NcompHeating;
					Counter = Counter + 1;
					goto Label20;
				}

				Pipe_p_IU_out = GetSatPressureRefrig( VRF( VRFCond ).RefrigerantName, VRF( VRFCond ).EvaporatingTemp, RefrigerantIndex, RoutineName );

				RefTSat = GetSatTemperatureRefrig( VRF( VRFCond ).RefrigerantName, max( min( Pipe_p_IU_out, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );

				if ( Pipe_m_ref > 0 ) {
					Pipe_h_comp_out_new = NcompHeating / Pipe_m_ref + GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Modifi_SH + VRF( VRFCond ).EvaporatingTemp ), max( min( Pipe_p_IU_out, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				} else {
					Pipe_h_comp_out_new = GetSupHeatEnthalpyRefrig( VRF( VRFCond ).RefrigerantName, max( RefTSat, Modifi_SH + VRF( VRFCond ).EvaporatingTemp ), max( min( Pipe_p_IU_out, RefPHigh ), RefPLow ), RefrigerantIndex, RoutineName );
				}

				if ( ( abs( Pipe_h_comp_out - Pipe_h_comp_out_new ) > 0.05 * Pipe_h_comp_out ) && ( Pipe_h_IU_in < Pipe_h_IU_in_up ) ) {
					Pipe_h_IU_in = Pipe_h_IU_in + 0.1 * ( Pipe_h_IU_in_up - Pipe_h_IU_in_low );
					goto Label23;
				}

				if ( Pipe_h_IU_in > Pipe_h_IU_in_up ) {
					Pipe_h_IU_in = 0.5 * ( Pipe_h_IU_in_up + Pipe_h_IU_in_low );
				}

			}

			VRF( VRFCond ).CompActSpeed = max( CompSpdActual, 0.0 );
			VRF( VRFCond ).NcompHeating = max( NcompHeating, 0.0 ) / 0.95;
			VRF( VRFCond ).CondFanPower = VRF( VRFCond ).RatedCondFanPower * pow_3( CondFlowRatio );
			VRF( VRFCond ).VRFCondCyclingRatio = CyclingRatio ;// report variable for cycling rate

			VRF( VRFCond ).HeatingCapacity = VRF( VRFCond ).RatedEvapCapacity * CurveValue( VRF( VRFCond ).OUCoolingCAPFT( NumOfCompSpdInput ), Tdischarge, VRF( VRFCond ).EvaporatingTemp ) + VRF( VRFCond ).RatedCompPower * CurveValue( VRF( VRFCond ).OUCoolingPWRFT( NumOfCompSpdInput ), Tdischarge, VRF( VRFCond ).EvaporatingTemp ); // Include the piping loss
			VRF( VRFCond ).PipingCorrectionCooling = TUHeatingLoad_temp / ( TUHeatingLoad_temp + Pipe_Q );
			MaxHeatingCapacity( VRFCond ) = VRF( VRFCond ).HeatingCapacity; // for report

		// 3. Stop running
		} else { // Since: if( CoolingLoad( VRFCond ) &&( TUCoolingLoad > 0.0 ) )

			VRFOperationSimPath = 4;
			NcompCooling = 0.0;
			NcompHeating = 0.0;
			VRF( VRFCond ).CompActSpeed = 0.0;
			VRF( VRFCond ).CondFanPower = 0.0;
			VRF( VRFCond ).VRFCondCyclingRatio = 0.0;
			VRF( VRFCond ).NcompCooling = NcompCooling;
			VRF( VRFCond ).NcompHeating = NcompHeating;
			VRF( VRFCond ).CondensingTemp  = OutDryBulbTemp;
			VRF( VRFCond ).EvaporatingTemp = OutDryBulbTemp;

		}

		VRF( VRFCond ).VRFOperationSimPath = VRFOperationSimPath;

		// calculate capacities and energy use
		if ( CoolingLoad( VRFCond ) && TerminalUnitList( TUListNum ).CoolingCoilPresent( NumTUInList ) ) {
			InletAirWetBulbC = SumCoolInletWB;

			// From the VRF_FluidTCtrl model
			TotalCondCoolingCapacity = VRF( VRFCond ).CoolingCapacity; //Include piping loss
			TotalTUCoolingCapacity = TotalCondCoolingCapacity * VRF( VRFCond ).PipingCorrectionCooling;

			if ( TotalCondCoolingCapacity > 0.0 ) {
				CoolingPLR = min( 1.0, ( VRF( VRFCond ).TUCoolingLoad / VRF( VRFCond ).PipingCorrectionCooling ) / TotalCondCoolingCapacity );
			} else {
				CoolingPLR = 0.0;
			}

		} else if ( HeatingLoad( VRFCond ) && TerminalUnitList( TUListNum ).HeatingCoilPresent( NumTUInList ) ) {
			InletAirDryBulbC = SumHeatInletDB;
			InletAirWetBulbC = SumHeatInletWB;

			// Initializing defrost adjustment factors
			LoadDueToDefrost = 0.0;
			HeatingCapacityMultiplier = 1.0;
			FractionalDefrostTime = 0.0;
			InputPowerMultiplier = 1.0;

			// Check outdoor temperature to determine of defrost is active
			if ( OutdoorDryBulb <= VRF( VRFCond ).MaxOATDefrost && VRF( VRFCond ).CondenserType != WaterCooled ) {

				// Calculating adjustment factors for defrost
				// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
				OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
				OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure ) ) );

				// Calculate defrost adjustment factors depending on defrost control type
				if ( VRF( VRFCond ).DefrostControl == Timed ) {
					FractionalDefrostTime = VRF( VRFCond ).DefrostFraction;
					if ( FractionalDefrostTime > 0.0 ) {
						HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
						InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
					}
				} else { //else defrost control is on-demand
					FractionalDefrostTime = 1.0 / ( 1.0 + 0.01446 / OutdoorCoildw );
					HeatingCapacityMultiplier = 0.875 * ( 1.0 - FractionalDefrostTime );
					InputPowerMultiplier = 0.954 * ( 1.0 - FractionalDefrostTime );
				}

				if ( FractionalDefrostTime > 0.0 ) {
					// Calculate defrost adjustment factors depending on defrost control strategy
					if ( VRF( VRFCond ).DefrostStrategy == ReverseCycle && VRF( VRFCond ).DefrostControl == OnDemand ) {
						LoadDueToDefrost = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( VRF( VRFCond ).HeatingCapacity / 1.01667 );
						DefrostEIRTempModFac = CurveValue( VRF( VRFCond ).DefrostEIRPtr, max( 15.555, InletAirWetBulbC ), max( 15.555, OutdoorDryBulb ) );

						//         Warn user if curve output goes negative
						if ( DefrostEIRTempModFac < 0.0 ) {
							if ( ! WarmupFlag ) {
								if ( VRF( VRFCond ).DefrostHeatErrorIndex == 0 ) {
									ShowSevereMessage( cVRFTypes( VRF_HeatPump ) + " \"" + VRF( VRFCond ).Name + "\":" );
									ShowContinueError( " Defrost Energy Input Ratio Modifier curve (function of temperature) output is negative (" + TrimSigDigits( DefrostEIRTempModFac, 3 ) + ")." );
									ShowContinueError( " Negative value occurs using an outdoor air dry-bulb temperature of " + TrimSigDigits( OutdoorDryBulb, 1 ) + " C and an average indoor air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + " C." );
									ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
								}
								ShowRecurringWarningErrorAtEnd( ccSimPlantEquipTypes( TypeOf_HeatPumpVRF ) + " \"" + VRF( VRFCond ).Name + "\": Defrost Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...", VRF( VRFCond ).DefrostHeatErrorIndex, DefrostEIRTempModFac, DefrostEIRTempModFac );
								DefrostEIRTempModFac = 0.0;
							}
						}

						VRF( VRFCond ).DefrostPower = DefrostEIRTempModFac * ( VRF( VRFCond ).HeatingCapacity / 1.01667 ) * FractionalDefrostTime;

					} else { // Defrost strategy is resistive
						VRF( VRFCond ).DefrostPower = VRF( VRFCond ).DefrostCapacity * FractionalDefrostTime;
					}
				} else { // Defrost is not active because FractionalDefrostTime = 0.0
					VRF( VRFCond ).DefrostPower = 0.0;
				}
			}

			// From the VRF_FluidTCtrl model
			TotalCondHeatingCapacity = VRF( VRFCond ).HeatingCapacity; //Include piping loss;
			TotalTUHeatingCapacity = TotalCondHeatingCapacity * VRF( VRFCond ).PipingCorrectionHeating;

			if ( TotalCondHeatingCapacity > 0.0 ) {
				HeatingPLR = min( 1.0, ( VRF( VRFCond ).TUHeatingLoad / VRF( VRFCond ).PipingCorrectionHeating ) / TotalCondHeatingCapacity );
				HeatingPLR += ( LoadDueToDefrost * HeatingPLR ) / TotalCondHeatingCapacity;
			} else {
				HeatingPLR = 0.0;
			}

		}

		VRF( VRFCond ).VRFCondPLR = max( CoolingPLR, HeatingPLR );

		// For HR Operations
		HRHeatRequestFlag = any( TerminalUnitList( TUListNum ).HRHeatRequest );
		HRCoolRequestFlag = any( TerminalUnitList( TUListNum ).HRCoolRequest );

		if ( ! DoingSizing && ! WarmupFlag ) {
			if ( HRHeatRequestFlag && HRCoolRequestFlag ) {
				// determine operating mode change
				if ( ! VRF( VRFCond ).HRCoolingActive && ! VRF( VRFCond ).HRHeatingActive ) {
					VRF( VRFCond ).ModeChange = true;
				}
				if ( CoolingLoad( VRFCond ) ) {
					if ( VRF( VRFCond ).HRHeatingActive && ! VRF( VRFCond ).HRCoolingActive ) {
						VRF( VRFCond ).HRModeChange = true;
					}
					VRF( VRFCond ).HRCoolingActive = true;
					VRF( VRFCond ).HRHeatingActive = false;
					HRCAPFT = VRF( VRFCond ).HRCAPFTCool; // Index to cool capacity as a function of temperature\PLR curve for heat recovery
					if ( HRCAPFT > 0 ) {
						//         VRF(VRFCond)%HRCAPFTCoolConst = 0.9d0 ! initialized to 0.9
						if ( VRF( VRFCond ).HRCAPFTCoolType == BiQuadratic ) { // Curve type for HRCAPFTCool
							VRF( VRFCond ).HRCAPFTCoolConst = CurveValue( HRCAPFT, InletAirWetBulbC, CondInletTemp );
						} else {
							VRF( VRFCond ).HRCAPFTCoolConst = CurveValue( HRCAPFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HRCAPFTConst = VRF( VRFCond ).HRCAPFTCoolConst;
					HRInitialCapFrac = VRF( VRFCond ).HRInitialCoolCapFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
					HRCapTC = VRF( VRFCond ).HRCoolCapTC; // Time constant used to recover from intial degratation in cooling heat recovery

					HREIRFT = VRF( VRFCond ).HREIRFTCool; // Index to cool EIR as a function of temperature curve for heat recovery
					if ( HREIRFT > 0 ) {
						//         VRF(VRFCond)%HREIRFTCoolConst = 1.1d0 ! initialized to 1.1
						if ( VRF( VRFCond ).HREIRFTCoolType == BiQuadratic ) { // Curve type for HRCAPFTCool
							VRF( VRFCond ).HREIRFTCoolConst = CurveValue( HREIRFT, InletAirWetBulbC, CondInletTemp );
						} else {
							VRF( VRFCond ).HREIRFTCoolConst = CurveValue( HREIRFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HREIRFTConst = VRF( VRFCond ).HREIRFTCoolConst;
					HRInitialEIRFrac = VRF( VRFCond ).HRInitialCoolEIRFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
					HREIRTC = VRF( VRFCond ).HRCoolEIRTC; // Time constant used to recover from intial degratation in cooling heat recovery
				} else if ( HeatingLoad( VRFCond ) ) {
					if ( ! VRF( VRFCond ).HRHeatingActive && VRF( VRFCond ).HRCoolingActive ) {
						VRF( VRFCond ).HRModeChange = true;
					}
					VRF( VRFCond ).HRCoolingActive = false;
					VRF( VRFCond ).HRHeatingActive = true;
					HRCAPFT = VRF( VRFCond ).HRCAPFTHeat; // Index to heat capacity as a function of temperature\PLR curve for heat recovery
					if ( HRCAPFT > 0 ) {
						//         VRF(VRFCond)%HRCAPFTHeatConst = 1.1d0 ! initialized to 1.1
						if ( VRF( VRFCond ).HRCAPFTHeatType == BiQuadratic ) { // Curve type for HRCAPFTCool
							{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
							if ( SELECT_CASE_var == DryBulbIndicator ) {
								VRF( VRFCond ).HRCAPFTHeatConst = CurveValue( HRCAPFT, InletAirDryBulbC, CondInletTemp );
							} else if ( SELECT_CASE_var == WetBulbIndicator ) {
								VRF( VRFCond ).HRCAPFTHeatConst = CurveValue( HRCAPFT, InletAirDryBulbC, OutdoorWetBulb );
							} else {
								VRF( VRFCond ).HRCAPFTHeatConst = 1.0;
							}}
						} else {
							VRF( VRFCond ).HRCAPFTHeatConst = CurveValue( HRCAPFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HRCAPFTConst = VRF( VRFCond ).HRCAPFTHeatConst;
					HRInitialCapFrac = VRF( VRFCond ).HRInitialHeatCapFrac; // Fractional heating degradation at the start of heat recovery from cooling mode
					HRCapTC = VRF( VRFCond ).HRHeatCapTC; // Time constant used to recover from intial degratation in heating heat recovery

					HREIRFT = VRF( VRFCond ).HREIRFTHeat; // Index to cool EIR as a function of temperature curve for heat recovery
					if ( HREIRFT > 0 ) {
						//         VRF(VRFCond)%HREIRFTCoolConst = 1.1d0 ! initialized to 1.1
						if ( VRF( VRFCond ).HREIRFTHeatType == BiQuadratic ) { // Curve type for HRCAPFTHeat
							{ auto const SELECT_CASE_var( VRF( VRFCond ).HeatingPerformanceOATType );
							if ( SELECT_CASE_var == DryBulbIndicator ) {
								VRF( VRFCond ).HREIRFTHeatConst = CurveValue( HREIRFT, InletAirDryBulbC, CondInletTemp );
							} else if ( SELECT_CASE_var == WetBulbIndicator ) {
								VRF( VRFCond ).HREIRFTHeatConst = CurveValue( HREIRFT, InletAirDryBulbC, OutdoorWetBulb );
							} else {
								VRF( VRFCond ).HREIRFTHeatConst = 1.0;
							}}
						} else {
							VRF( VRFCond ).HREIRFTHeatConst = CurveValue( HREIRFT, VRF( VRFCond ).VRFCondPLR );
						}
					}
					HREIRFTConst = VRF( VRFCond ).HRCAPFTHeatConst;
					HRInitialEIRFrac = VRF( VRFCond ).HRInitialHeatEIRFrac; // Fractional heating degradation at the start of heat recovery from heating mode
					HREIRTC = VRF( VRFCond ).HRHeatEIRTC; // Time constant used to recover from intial degratation in heating heat recovery
				} else {
					//   zone thermostats satisfied, condenser is off. Set values anyway
					HRCAPFTConst = 1.0;
					HRInitialCapFrac = 1.0;
					HRCapTC = 1.0;
					HREIRFTConst = 1.0;
					HRInitialEIRFrac = 1.0;
					HREIRTC = 1.0;
					if ( VRF( VRFCond ).HRHeatingActive || VRF( VRFCond ).HRCoolingActive ) {
						VRF( VRFCond ).HRModeChange = true;
					}
					VRF( VRFCond ).HRCoolingActive = false;
					VRF( VRFCond ).HRHeatingActive = false;
				}

			} else { // IF(HRHeatRequestFlag .AND. HRCoolRequestFlag)THEN -- Heat recovery turned off
				HRCAPFTConst = 1.0;
				HRInitialCapFrac = 1.0;
				HRCapTC = 0.0;
				HREIRFTConst = 1.0;
				HRInitialEIRFrac = 1.0;
				HREIRTC = 0.0;
				VRF( VRFCond ).HRModeChange = false;
				VRF( VRFCond ).HRCoolingActive = false;
				VRF( VRFCond ).HRHeatingActive = false;
			}

			// calculate end time of current time step to determine if max capacity reset is required
			CurrentEndTime = double( ( DayOfSim - 1 ) * 24 ) + CurrentTime - TimeStepZone + SysTimeElapsed;

			if ( VRF( VRFCond ).ModeChange || VRF( VRFCond ).HRModeChange ) {
				if ( VRF( VRFCond ).HRCoolingActive && VRF( VRFCond ).HRTimer == 0.0 ) {
					VRF( VRFCond ).HRTimer = CurrentEndTimeLast;
				} else if ( VRF( VRFCond ).HRHeatingActive && VRF( VRFCond ).HRTimer == 0.0 ) {
					VRF( VRFCond ).HRTimer = CurrentEndTimeLast;
				} else if ( ! VRF( VRFCond ).HRCoolingActive && ! VRF( VRFCond ).HRHeatingActive ) {
					VRF( VRFCond ).HRTimer = 0.0;
				}
			}

			VRF( VRFCond ).HRTime = max( 0.0, CurrentEndTime - VRF( VRFCond ).HRTimer );
			if ( VRF( VRFCond ).HRTime < ( HRCapTC * 5.0 ) ) {
				if ( HRCapTC > 0.0 ) {
					SUMultiplier = min( 1.0, 1.0 - std::exp( -VRF( VRFCond ).HRTime / HRCapTC ) );
				} else {
					SUMultiplier = 1.0;
				}
			} else {
				SUMultiplier = 1.0;
				VRF( VRFCond ).ModeChange = false;
				VRF( VRFCond ).HRModeChange = false;
			}
			VRF( VRFCond ).SUMultiplier = SUMultiplier;

			TimeStepSysLast = TimeStepSys;
			CurrentEndTimeLast = CurrentEndTime;

			if ( VRF( VRFCond ).HeatRecoveryUsed && VRF( VRFCond ).HRCoolingActive ) {
				TotalCondCoolingCapacity *= HRCAPFTConst;
				TotalCondCoolingCapacity = HRInitialCapFrac * TotalCondCoolingCapacity + ( 1.0 - HRInitialCapFrac ) * TotalCondCoolingCapacity * SUMultiplier;
				TotalTUCoolingCapacity = TotalCondCoolingCapacity * VRF( VRFCond ).PipingCorrectionCooling;
				if ( TotalCondCoolingCapacity > 0.0 ) {
					CoolingPLR = min( 1.0, ( VRF( VRFCond ).TUCoolingLoad / VRF( VRFCond ).PipingCorrectionCooling ) / TotalCondCoolingCapacity );
				} else {
					CoolingPLR = 0.0;
				}
			} else if ( VRF( VRFCond ).HeatRecoveryUsed && VRF( VRFCond ).HRHeatingActive ) {
				TotalCondHeatingCapacity *= HRCAPFTConst;
				TotalCondHeatingCapacity = HRInitialCapFrac * TotalCondHeatingCapacity + ( 1.0 - HRInitialCapFrac ) * TotalCondHeatingCapacity * SUMultiplier;
				TotalTUHeatingCapacity = TotalCondHeatingCapacity * VRF( VRFCond ).PipingCorrectionHeating;
				if ( TotalCondHeatingCapacity > 0.0 ) {
					HeatingPLR = min( 1.0, ( VRF( VRFCond ).TUHeatingLoad / VRF( VRFCond ).PipingCorrectionHeating ) / TotalCondHeatingCapacity );
				} else {
					HeatingPLR = 0.0;
				}
			}
			VRF( VRFCond ).VRFCondPLR = max( CoolingPLR, HeatingPLR );
		}

		VRF( VRFCond ).TotalCoolingCapacity = TotalCondCoolingCapacity * CoolingPLR;
		VRF( VRFCond ).TotalHeatingCapacity = TotalCondHeatingCapacity * HeatingPLR;

		if ( VRF( VRFCond ).MinPLR > 0.0 ) {
			// CyclingRatio = min( 1.0, VRF( VRFCond ).VRFCondPLR / VRF( VRFCond ).MinPLR );
			if ( VRF( VRFCond ).VRFCondPLR < VRF( VRFCond ).MinPLR && VRF( VRFCond ).VRFCondPLR > 0.0 ) {
				VRF( VRFCond ).VRFCondPLR = VRF( VRFCond ).MinPLR;
			}
		}
		// VRF( VRFCond ).VRFCondCyclingRatio = CyclingRatio; // obtained above

		VRF( VRFCond ).OperatingMode = 0; // report variable for heating or cooling mode
		VRFRTF = 0.0;
		// cooling and heating is optional (only one may exist), if so then performance curve for missing coil are not required
		if ( CoolingLoad( VRFCond ) && CoolingPLR > 0.0 ) {
			PartLoadFraction = 1.0;
			VRFRTF = min( 1.0, ( CyclingRatio / PartLoadFraction ) );

			VRF( VRFCond ).ElecCoolingPower = VRF(VRFCond).NcompCooling;
		}
		if ( HeatingLoad( VRFCond ) && HeatingPLR > 0.0 ) {
			PartLoadFraction = 1.0;
			VRFRTF = min( 1.0, ( CyclingRatio / PartLoadFraction ) );

			VRF( VRFCond ).ElecHeatingPower = VRF( VRFCond ).NcompHeating;
		}
		VRF( VRFCond ).VRFCondRTF = VRFRTF;

		// Calculate CrankCaseHeaterPower: VRF Heat Pump Crankcase Heater Electric Power [W]
		if ( VRF( VRFCond ).MaxOATCCHeater > OutdoorDryBulb ) {
			// calculate crankcase heater power
			VRF( VRFCond ).CrankCaseHeaterPower = VRF( VRFCond ).CCHeaterPower * ( 1.0 - VRFRTF );
			if ( VRF( VRFCond ).NumCompressors > 1 ) {
				UpperStageCompressorRatio = ( 1.0 - VRF( VRFCond ).CompressorSizeRatio ) / ( VRF( VRFCond ).NumCompressors - 1 );
				for ( Stage = 1; Stage <= VRF( VRFCond ).NumCompressors - 2; ++Stage ) {
					if ( VRF( VRFCond ).VRFCondPLR < ( VRF( VRFCond ).CompressorSizeRatio + Stage * UpperStageCompressorRatio ) ) {
						VRF( VRFCond ).CrankCaseHeaterPower += VRF( VRFCond ).CCHeaterPower;
					}
				}
			}
		} else {
			VRF( VRFCond ).CrankCaseHeaterPower = 0.0;
		}

		// Calculate QCondenser: VRF Heat Pump Condenser Heat Transfer Rate [W]
		CondCapacity = max( VRF( VRFCond ).TotalCoolingCapacity, VRF( VRFCond ).TotalHeatingCapacity ) * VRFRTF;
		CondPower = max( VRF( VRFCond ).ElecCoolingPower, VRF( VRFCond ).ElecHeatingPower );
		if ( VRF( VRFCond ).ElecHeatingPower > 0.0 ) {
			VRF( VRFCond ).QCondenser = CondCapacity + CondPower - VRF( VRFCond ).TUHeatingLoad / VRF( VRFCond ).PipingCorrectionHeating;
		} else if ( VRF( VRFCond ).ElecCoolingPower > 0.0 ) {
			VRF( VRFCond ).QCondenser = - CondCapacity + CondPower + VRF( VRFCond ).TUCoolingLoad / VRF( VRFCond ).PipingCorrectionCooling;
		} else {
			VRF( VRFCond ).QCondenser = 0.0;
		}

		// Calculate OperatingHeatingCOP & OperatingCoolingCOP: VRF Heat Pump Operating COP []
		if ( CoolingLoad( VRFCond ) && CoolingPLR > 0.0 ) {
			if ( VRF( VRFCond ).ElecCoolingPower != 0.0 ) {
				// this calc should use delivered capacity, not condenser capacity, use VRF(VRFCond)%TUCoolingLoad
				VRF( VRFCond ).OperatingCoolingCOP = ( VRF( VRFCond ).TotalCoolingCapacity ) / ( VRF( VRFCond ).ElecCoolingPower + VRF( VRFCond ).CrankCaseHeaterPower + VRF( VRFCond ).EvapCondPumpElecPower + VRF( VRFCond ).DefrostPower );
			} else {
				VRF( VRFCond ).OperatingCoolingCOP = 0.0;
			}
		}
		if ( HeatingLoad( VRFCond ) && HeatingPLR > 0.0 ) {
			if ( VRF( VRFCond ).ElecHeatingPower != 0.0 ) {
				// this calc should use deleivered capacity, not condenser capacity, use VRF(VRFCond)%TUHeatingLoad
				VRF( VRFCond ).OperatingHeatingCOP = ( VRF( VRFCond ).TotalHeatingCapacity ) / ( VRF( VRFCond ).ElecHeatingPower + VRF( VRFCond ).CrankCaseHeaterPower + VRF( VRFCond ).EvapCondPumpElecPower + VRF( VRFCond ).DefrostPower );
			} else {
				VRF( VRFCond ).OperatingHeatingCOP = 0.0;
			}
		}

		TotPower = TUParasiticPower + TUFanPower + VRF( VRFCond ).ElecHeatingPower + VRF( VRFCond ).ElecCoolingPower + VRF( VRFCond ).CrankCaseHeaterPower + VRF( VRFCond ).EvapCondPumpElecPower + VRF( VRFCond ).DefrostPower;
		if ( TotPower > 0.0 ) VRF( VRFCond ).OperatingCOP = ( VRF( VRFCond ).TUCoolingLoad + VRF( VRFCond ).TUHeatingLoad ) / TotPower;

		// limit the TU capacity when the condenser is maxed out on capacity
		// I think this next line will make the max cap report variable match the coil objects, will probably change the answer though
		//  IF(CoolingLoad(VRFCond) .AND. NumTUInCoolingMode .GT. 0 .AND. MaxCoolingCapacity(VRFCond) == MaxCap)THEN
		if ( CoolingLoad( VRFCond ) && NumTUInCoolingMode > 0 ) {

			//   IF TU capacity is greater than condenser capacity find maximum allowed TU capacity (i.e., conserve energy)
			if ( TUCoolingLoad > TotalTUCoolingCapacity ) {
				LimitTUCapacity( VRFCond, NumTUInList, TotalTUCoolingCapacity, TerminalUnitList( TUListNum ).TotalCoolLoad, MaxCoolingCapacity( VRFCond ), TotalTUHeatingCapacity, TerminalUnitList( TUListNum ).TotalHeatLoad, MaxHeatingCapacity( VRFCond ) );
			}
		} else if ( HeatingLoad( VRFCond ) && NumTUInHeatingMode > 0 ) {
			//   IF TU capacity is greater than condenser capacity
			if ( TUHeatingLoad > TotalTUHeatingCapacity ) {
				LimitTUCapacity( VRFCond, NumTUInList, TotalTUHeatingCapacity, TerminalUnitList( TUListNum ).TotalHeatLoad, MaxHeatingCapacity( VRFCond ), TotalTUCoolingCapacity, TerminalUnitList( TUListNum ).TotalCoolLoad, MaxCoolingCapacity( VRFCond ) );
			}
		} else {
		}

	}

	void
	ControlVRF_FluidTCtrl(
		int const VRFTUNum, // Index to VRF terminal unit
		Real64 const QZnReq, // Index to zone number
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 & PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rongpeng Zhang
		//       DATE WRITTEN   Nov 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine the coil load and part load ratio, given the zone load
		// Determine the air mass flow rate corresponding to the coil load of the heat pump for this time step

		// METHODOLOGY EMPLOYED:
		// Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using DataEnvironment::OutDryBulbTemp;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // maximum number of iterations
		Real64 const MinPLF( 0.0 ); // minimum part load factor allowed
		Real64 const ErrorTol( 0.001 ); // tolerance for RegulaFalsi iterations
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FullOutput; // unit full output when compressor is operating [W]
		Real64 TempOutput; // unit output when iteration limit exceeded [W]
		Real64 NoCompOutput; // output when no active compressor [W]
		int SolFla; // Flag of RegulaFalsi solver
		Array1D< Real64 > Par( 6 ); // Parameters passed to RegulaFalsi
		std::string IterNum; // Max number of iterations for warning message
		Real64 TempMinPLR; // min PLR used in Regula Falsi call
		Real64 TempMaxPLR; // max PLR used in Regula Falsi call
		bool ContinueIter; // used when convergence is an issue
		int VRFCond; // index to VRF condenser
		int IndexToTUInTUList; // index to TU in specific list for the VRF system
		int TUListIndex; // index to TU list for this VRF system
		bool VRFCoolingMode;
		bool VRFHeatingMode;
		bool HRCoolingMode;
		bool HRHeatingMode;

		PartLoadRatio = 0.0;
		LoopDXCoolCoilRTF = 0.0;
		LoopDXHeatCoilRTF = 0.0;
		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		VRFCoolingMode = CoolingLoad( VRFCond );
		VRFHeatingMode = HeatingLoad( VRFCond );
		HRCoolingMode = TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList );
		HRHeatingMode = TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList );

		// The RETURNS here will jump back to SimVRF where the CalcVRF routine will simulate with lastest PLR

		// do nothing else if TU is scheduled off
		//!!LKL Discrepancy < 0
		if ( GetCurrentScheduleValue( VRFTU( VRFTUNum ).SchedPtr ) == 0.0 ) return;

		// Block the following statement: QZnReq==0 doesn't mean QCoilReq==0 due to possible OA mixer operation. zrp_201511
		// do nothing if TU has no load (TU will be modeled using PLR=0)
		// if ( QZnReq == 0.0 ) return;

		// Set EMS value for PLR and return
		if ( VRFTU( VRFTUNum ).EMSOverridePartLoadFrac ) {
			PartLoadRatio = VRFTU( VRFTUNum ).EMSValueForPartLoadFrac;
			return;
		}

		// Get result when DX coil is off
		PartLoadRatio = 0.0;

		// Algorithm Type: VRF model based on physics, appliable for Fluid Temperature Control
		CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, 0.0, NoCompOutput, OnOffAirFlowRatio );

		if ( VRFCoolingMode && HRHeatingMode ) {
			// IF the system is in cooling mode, but the terminal unit requests heating (heat recovery)
			if ( NoCompOutput >= QZnReq ) return;
		} else if ( VRFHeatingMode && HRCoolingMode ) {
			// IF the system is in heating mode, but the terminal unit requests cooling (heat recovery)
			if ( NoCompOutput <= QZnReq ) return;
		} else if ( VRFCoolingMode || HRCoolingMode ) {
			// IF the system is in cooling mode and/or the terminal unit requests cooling
			if ( NoCompOutput <= QZnReq ) return;
		} else if ( VRFHeatingMode || HRHeatingMode ) {
			// IF the system is in heating mode and/or the terminal unit requests heating
			if ( NoCompOutput >= QZnReq ) return;
		}

		// Otherwise the coil needs to turn on. Get full load result
		PartLoadRatio = 1.0;
		CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, PartLoadRatio, FullOutput, OnOffAirFlowRatio );

		PartLoadRatio = 0.0;

		if ( ( VRFCoolingMode && ! VRF( VRFCond ).HeatRecoveryUsed ) || ( VRF( VRFCond ).HeatRecoveryUsed && HRCoolingMode ) ) {
			// Since we are cooling, we expect FullOutput < NoCompOutput
			// If the QZnReq <= FullOutput the unit needs to run full out
			if ( QZnReq <= FullOutput ) {
				// if no coil present in terminal unit, no need to reset PLR?
				if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) PartLoadRatio = 1.0;
				return;
			}
		} else if ( ( VRFHeatingMode && ! VRF( VRFCond ).HeatRecoveryUsed ) || ( VRF( VRFCond ).HeatRecoveryUsed && HRHeatingMode ) ) {
			// Since we are heating, we expect FullOutput > NoCompOutput
			// If the QZnReq >= FullOutput the unit needs to run full out
			if ( QZnReq >= FullOutput ) {
				// if no coil present in terminal unit, no need reset PLR?
				if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) PartLoadRatio = 1.0;
				return;
			}
		} else {
			// VRF terminal unit is off, PLR already set to 0 above
			// shouldn't actually get here
			return;
		}

		// The coil will not operate at PLR=0 or PLR=1, calculate the operating part-load ratio

		if ( ( VRFHeatingMode || HRHeatingMode ) || ( VRFCoolingMode || HRCoolingMode ) ) {

			Par( 1 ) = VRFTUNum;
			Par( 2 ) = 0.0;
			Par( 4 ) = 0.0;
			if ( FirstHVACIteration ) {
				Par( 3 ) = 1.0;
			} else {
				Par( 3 ) = 0.0;
			}
			//    Par(4) = OpMode
			Par( 5 ) = QZnReq;
			Par( 6 ) = OnOffAirFlowRatio;
			SolveRegulaFalsi( ErrorTol, MaxIte, SolFla, PartLoadRatio, PLRResidual, 0.0, 1.0, Par );
			if ( SolFla == -1 ) {
				//     Very low loads may not converge quickly. Tighten PLR boundary and try again.
				TempMaxPLR = -0.1;
				ContinueIter = true;
				while ( ContinueIter && TempMaxPLR < 1.0 ) {
					TempMaxPLR += 0.1;

					CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, TempMaxPLR, TempOutput, OnOffAirFlowRatio );

					if ( VRFHeatingMode && TempOutput > QZnReq ) ContinueIter = false;
					if ( VRFCoolingMode && TempOutput < QZnReq ) ContinueIter = false;
				}
				TempMinPLR = TempMaxPLR;
				ContinueIter = true;
				while ( ContinueIter && TempMinPLR > 0.0 ) {
					TempMaxPLR = TempMinPLR;
					TempMinPLR -= 0.01;

					CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, TempMaxPLR, TempOutput, OnOffAirFlowRatio );

					if ( VRFHeatingMode && TempOutput < QZnReq ) ContinueIter = false;
					if ( VRFCoolingMode && TempOutput > QZnReq ) ContinueIter = false;
				}
				SolveRegulaFalsi( ErrorTol, MaxIte, SolFla, PartLoadRatio, PLRResidual, TempMinPLR, TempMaxPLR, Par );
				if ( SolFla == -1 ) {
					if ( ! FirstHVACIteration && ! WarmupFlag ) {
						if ( VRFTU( VRFTUNum ).IterLimitExceeded == 0 ) {
							gio::write( IterNum, fmtLD ) << MaxIte;
							strip( IterNum );
							ShowWarningMessage( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\"" );
							ShowContinueError( " Iteration limit exceeded calculating terminal unit part-load ratio, maximum iterations = " + IterNum );
							ShowContinueErrorTimeStamp( " Part-load ratio returned = " + RoundSigDigits( PartLoadRatio, 3 ) );

							CalcVRF_FluidTCtrl( VRFTUNum, FirstHVACIteration, TempMinPLR, TempOutput, OnOffAirFlowRatio );

							ShowContinueError( " Load requested = " + TrimSigDigits( QZnReq, 5 ) + ", Load delivered = " + TrimSigDigits( TempOutput, 5 ) );
							ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit Iteration limit exceeded error continues...", VRFTU( VRFTUNum ).IterLimitExceeded );
						} else {
							ShowRecurringWarningErrorAtEnd( cVRFTUTypes( VRFTU( VRFTUNum ).VRFTUType_Num ) + " \"" + VRFTU( VRFTUNum ).Name + "\" -- Terminal unit Iteration limit exceeded error continues...", VRFTU( VRFTUNum ).IterLimitExceeded );
						}
					}
				} else if ( SolFla == -2 ) {
					PartLoadRatio = max( MinPLF, std::abs( QZnReq - NoCompOutput ) / std::abs( FullOutput - NoCompOutput ) );
				}
			} else if ( SolFla == -2 ) {
				if ( FullOutput - NoCompOutput == 0.0 ) {
					PartLoadRatio = 0.0;
				} else {
					PartLoadRatio = min( 1.0, max( MinPLF, std::abs( QZnReq - NoCompOutput ) / std::abs( FullOutput - NoCompOutput ) ) );
				}
			}

		}

	}

	void
	CalcVRF_FluidTCtrl(
		int const VRFTUNum, // Unit index in VRF terminal unit array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 const PartLoadRatio, // compressor part load fraction
		Real64 & LoadMet, // load met by unit (W)
		Real64 & OnOffAirFlowRatio, // ratio of ON air flow to average air flow
		Optional< Real64 > LatOutputProvided // delivered latent capacity (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         RP Zhang (LBNL), XF Pang (LBNL), Y Yura (Daikin Inc)
		//       DATE WRITTEN   June 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// 		This subroutine is part of the new VRF model based on physics, appliable for Fluid Temperature Control.
		// 		This is adapted from subroutine CalcVRF, which is part of the VRF model based on system curves.
		// 		This subroutine simulates the components making up the VRF indoor terminal unit.

		// METHODOLOGY EMPLOYED:
		//		A new physics based VRF model appliable for Fluid Temperature Control.

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
		using DataSizing::AutoSize;
		//  USE WaterToAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
		using DataAirLoop::LoopDXCoilRTF;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int VRFTUOutletNodeNum; // TU air outlet node
		int VRFTUInletNodeNum; // TU air inlet node
		Real64 AirMassFlow; // total supply air mass flow [m3/s]
		Real64 MinHumRat; // minimum humidity ratio for sensible capacity calculation (kg/kg)
		int OpMode; // fan operating mode, CycFanCycCoil or ContFanCycCoil
		int VRFCond; // index to VRF condenser
		Real64 SpecHumOut; // specific humidity ratio at outlet node
		Real64 SpecHumIn; // specific humidity ratio at inlet node
		int TUListIndex; // index to TU list for this VRF system
		int IndexToTUInTUList; // index to TU in specific list for the VRF system
		Real64 EvapTemp; //evaporating temperature
		Real64 CondTemp; //condensing temperature

		// FLOW

		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;
		VRFTUOutletNodeNum = VRFTU( VRFTUNum ).VRFTUOutletNodeNum;
		VRFTUInletNodeNum = VRFTU( VRFTUNum ).VRFTUInletNodeNum;
		OpMode = VRFTU( VRFTUNum ).OpMode;
		EvapTemp = VRF(VRFCond).IUEvaporatingTemp;
		CondTemp = VRF(VRFCond).IUCondensingTemp;

		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		if( PartLoadRatio == 0 ) {
			// only provide required OA when coil is off
			CompOnMassFlow = OACompOnMassFlow;
			CompOffMassFlow = OACompOffMassFlow;
		} else {
			// identify the air flow rate corresponding to the coil load
			CompOnMassFlow = CalVRFTUAirFlowRate_FluidTCtrl( VRFTUNum, PartLoadRatio, FirstHVACIteration );
		}
		SetAverageAirFlow( VRFTUNum, PartLoadRatio, OnOffAirFlowRatio );
		AirMassFlow = Node( VRFTUInletNodeNum ).MassFlowRate;

		// simulate OA Mixer
		if ( VRFTU( VRFTUNum ).OAMixerUsed ) SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );

		// if blow through, simulate fan then coils
		if ( VRFTU( VRFTUNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( "", FirstHVACIteration, VRFTU( VRFTUNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( VRFTU( VRFTUNum ).CoolingCoilPresent ) {
			// above condition for heat pump mode, below condition for heat recovery mode
			if ( ( ! VRF( VRFCond ).HeatRecoveryUsed && CoolingLoad( VRFCond ) ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) ) {
				SimDXCoil( "", On, FirstHVACIteration, VRFTU( VRFTUNum ).CoolCoilIndex, OpMode, PartLoadRatio, _, _, MaxCoolingCapacity( VRFCond ), VRF( VRFTU( VRFTUNum ).VRFSysNum ).VRFCondCyclingRatio );
			} else { // cooling coil is off
				SimDXCoil( "", Off, FirstHVACIteration, VRFTU( VRFTUNum ).CoolCoilIndex, OpMode, 0.0, _ );
			}
			LoopDXCoolCoilRTF = LoopDXCoilRTF;
		} else {
			LoopDXCoolCoilRTF = 0.0;
		}

		if ( VRFTU( VRFTUNum ).HeatingCoilPresent ) {
			// above condition for heat pump mode, below condition for heat recovery mode
			if ( ( ! VRF( VRFCond ).HeatRecoveryUsed && HeatingLoad( VRFCond ) ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) ) {
				SimDXCoil( "", On, FirstHVACIteration, VRFTU( VRFTUNum ).HeatCoilIndex, OpMode, PartLoadRatio, _, _, MaxHeatingCapacity( VRFCond ) );
			} else {
				SimDXCoil( "", Off, FirstHVACIteration, VRFTU( VRFTUNum ).HeatCoilIndex, OpMode, 0.0, _ );
			}
			LoopDXHeatCoilRTF = LoopDXCoilRTF;
		} else {
			LoopDXHeatCoilRTF = 0.0;
		}

		// if draw through, simulate coils then fan
		if ( VRFTU( VRFTUNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( "", FirstHVACIteration, VRFTU( VRFTUNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		// track fan power per terminal unit for calculating COP
		VRFTU( VRFTUNum ).FanPower = FanElecPower;

		// calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio
		MinHumRat = min( Node( VRFTUInletNodeNum ).HumRat, Node( VRFTUOutletNodeNum ).HumRat );
		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( VRFTUOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( VRFTUInletNodeNum ).Temp, MinHumRat ) ); // sensible load met by TU

		if ( present( LatOutputProvided ) ) {
			//   CR9155 Remove specific humidity calculations
			SpecHumOut = Node( VRFTUOutletNodeNum ).HumRat;
			SpecHumIn = Node( VRFTUInletNodeNum ).HumRat;
			LatOutputProvided = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate, kg/s (dehumid = negative)
		}

	}

	Real64
	CalVRFTUAirFlowRate_FluidTCtrl(
		int VRFTUNum, // TU index
		Real64 PartLoadRatio, // part load ratio of the coil
		bool FirstHVACIteration // FirstHVACIteration flag
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rongpeng Zhang, LBNL
		//       DATE WRITTEN   Nov 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  This function determines the TU airflow rate corresponding to the coil load.
		//  This is used to address the coupling between OA mixer simulation and VRF-FluidTCtrl coil simulation.

		// METHODOLOGY EMPLOYED:
		//  VRF-FluidTCtrl TU airflow rate is determined by the control logic of VRF-FluidTCtrl coil to match the
		//  coil load. This is affected by the coil inlet conditions. However, the airflow rate will affect the
		//  OA mixer simulation, which leads to different coil inlet conditions. So, there is a coupling issue here.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using DXCoils::DXCoil;
		using General::SolveRegulaFalsi;

		// Return value
		Real64 AirMassFlowRate; // air mass flow rate of the coil (kg/s)

		// Argument array dimensioning

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > Par( 7 ); // Parameters passed to RegulaFalsi
		int const Mode( 1 ); // Performance mode for MultiMode DX coil. Always 1 for other coil types
		int const MaxIte( 500 ); // maximum number of iterations
		int DXCoilNum; // index to DX Coil
		int IndexToTUInTUList; // index to TU in specific list for the VRF system
		int SolFla; // Flag of RegulaFalsi solver
		int TUListIndex; // index to TU list for this VRF system
		int VRFCond; // index to VRF condenser
		Real64 const ErrorTol( 0.01 ); // tolerance for RegulaFalsi iterations
		Real64 FanSpdRatio; // ratio of required and rated air flow rate
		Real64 FanSpdRatioMin; // min fan speed ratio
		Real64 FanSpdRatioMax; // min fan speed ratio
		Real64 QCoilReq; // required coil load (W)
		Real64 QCoilAct; // actural coil load (W)
		Real64 TeTc; // evaporating temperature or condensing temperature for VRF indoor unit(C)


		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		TUListIndex = VRF( VRFCond ).ZoneTUListPtr;
		IndexToTUInTUList = VRFTU( VRFTUNum ).IndexToTUInTUList;

		if ( ( ! VRF( VRFCond ).HeatRecoveryUsed && CoolingLoad( VRFCond ) ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRCoolRequest( IndexToTUInTUList ) ) ) {
			// VRF terminal unit is on cooling mode
			DXCoilNum = VRFTU( VRFTUNum ).CoolCoilIndex;
			QCoilReq = - PartLoadRatio * DXCoil( DXCoilNum ).RatedTotCap( Mode ); // positive for heating; negative for cooling
			TeTc = VRF( VRFCond ).IUEvaporatingTemp;

		} else if ( ( ! VRF( VRFCond ).HeatRecoveryUsed && HeatingLoad( VRFCond ) ) || ( VRF( VRFCond ).HeatRecoveryUsed && TerminalUnitList( TUListIndex ).HRHeatRequest( IndexToTUInTUList ) ) ) {
			// VRF terminal unit is on heating mode
			DXCoilNum = VRFTU( VRFTUNum ).HeatCoilIndex;
			QCoilReq = PartLoadRatio * DXCoil( DXCoilNum ).RatedTotCap( Mode ); // positive for heating; negative for cooling
			TeTc = VRF( VRFCond ).IUCondensingTemp;

		} else {
			// VRF terminal unit is off
			QCoilAct = 0.0;
			AirMassFlowRate = max( OACompOnMassFlow, 0.0 );
			return AirMassFlowRate;
		}

		// minimum airflow rate
		FanSpdRatioMin = min( OACompOnMassFlow / DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ), 1.0 );

		if ( FirstHVACIteration ) {
			Par( 1 ) = 1.0;
		} else {
			Par( 1 ) = 0.0;
		}
		Par( 2 ) = VRFTUNum;
		Par( 3 ) = DXCoilNum;
		Par( 4 ) = QCoilReq;
		Par( 5 ) = TeTc;
		Par( 6 ) = PartLoadRatio;
		Par( 7 ) = OACompOnMassFlow;

		FanSpdRatioMax = 1.0;
		SolveRegulaFalsi( ErrorTol, MaxIte, SolFla, FanSpdRatio, VRFTUAirFlowResidual_FluidTCtrl, FanSpdRatioMin, FanSpdRatioMax, Par );
		if( SolFla < 0) FanSpdRatio = FanSpdRatioMax; //over capacity

		AirMassFlowRate = FanSpdRatio * DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );

		return AirMassFlowRate;

	}

	Real64
	VRFTUAirFlowResidual_FluidTCtrl(
		Real64 const FanSpdRatio, // fan speed ratio of VRF VAV TU
		Array1< Real64 > const & Par // par(1) = VRFTUNum
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Rongpeng Zhang, LBNL
		//       DATE WRITTEN   Nov 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// 		Calculates residual function ( FanSpdRatioAct - FanSpdRatio ) / FanSpdRatio
		// 		This is used to address the coupling between OA mixer simulation and VRF-FluidTCtrl coil simulation.

		// METHODOLOGY EMPLOYED:
		// 		VRF-FluidTCtrl TU airflow rate is determined by the control logic of VRF-FluidTCtrl coil to match the
		// 		coil load. This is affected by the coil inlet conditions. However, the airflow rate will affect the
		// 		OA mixer simulation, which leads to different coil inlet conditions. So, there is a coupling issue here.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::ControlVRFIUCoil;
		using DXCoils::DXCoil;
		using Fans::Fan;
		using Fans::SimulateFanComponents;
		using InputProcessor::FindItemInList;
		using MixedAir::SimOAMixer;
		using MixedAir::OAMixer;
		using Psychrometrics::PsyHFnTdbW;

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 AirFlowRateResidual;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// 	Par( 1 ) = FirstHVACIteration;
		// 	Par( 2 ) = VRFTUNum;
		// 	Par( 3 ) = DXCoilNum;
		// 	Par( 4 ) = QCoilReq;
		// 	Par( 5 ) = TeTc;
		// 	Par( 6 ) = PartLoadRatio;
		// 	Par( 7 ) = OACompOnMassFlow;

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int const Mode( 1 ); // Performance mode for MultiMode DX coil. Always 1 for other coil types
		int CoilIndex; // index to coil
		int FanOutletNode; // index to the outlet node of the fan
		int OAMixerNum; //OA mixer index
		int OAMixNode; // index to the mix node of OA mixer
		int VRFCond; // index to VRF condenser
		int VRFTUNum; // Unit index in VRF terminal unit array
		int VRFInletNode; // VRF inlet node number
		bool FirstHVACIteration; // flag for 1st HVAC iteration in the time step
		Real64 FanSpdRatioBase; // baseline FanSpdRatio for VRFTUAirFlowResidual
		Real64 FanSpdRatioAct; // calculated FanSpdRatio for VRFTUAirFlowResidual
		Real64 PartLoadRatio; //Part load ratio
		Real64 QCoilReq; // required coil load [W]
		Real64 QCoilAct; // actual coil load [W]
		Real64 temp; // for temporary use
		Real64 TeTc; //evaporating/condensing temperature [C]
		Real64 Tin; // coil inlet air temperature [C]
		Real64 Win; // coil inlet air humidity ratio [kg/kg]
		Real64 Hin; // coil inlet air enthalpy
		Real64 Wout; // coil outlet air humidity ratio
		Real64 Tout; // coil outlet air temperature
		Real64 Hout; // coil outlet air enthalpy
		Real64 SHact;// coil actual SH
		Real64 SCact;// coil actual SC

		// FLOW

		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 1 ) == 1.0 );
		VRFTUNum = int( Par( 2 ) );
		CoilIndex = int( Par( 3 ) );
		QCoilReq = Par( 4 ) ;
		TeTc = Par( 5 ) ;
		PartLoadRatio = Par( 6 ) ;
		OACompOnMassFlow = Par( 7 ) ;

		VRFCond = VRFTU( VRFTUNum ).VRFSysNum;
		VRFInletNode = VRFTU( VRFTUNum ).VRFTUInletNodeNum;

		if ( std::abs( FanSpdRatio ) < 0.01 )
			FanSpdRatioBase = sign( 0.01, FanSpdRatio );
		else
			FanSpdRatioBase = FanSpdRatio;

		// Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
		CompOnMassFlow = FanSpdRatio * DXCoil( CoilIndex ).RatedAirMassFlowRate( Mode );
		SetAverageAirFlow( VRFTUNum, PartLoadRatio, temp );
		Tin = Node( VRFInletNode ).Temp;
		Win = Node( VRFInletNode ).HumRat;

		// Simulation the OAMixer if there is any
		if ( VRFTU( VRFTUNum ).OAMixerUsed ) {
			SimOAMixer( VRFTU( VRFTUNum ).OAMixerName, FirstHVACIteration, VRFTU( VRFTUNum ).OAMixerIndex );

			OAMixerNum = FindItemInList( VRFTU( VRFTUNum ).OAMixerName, OAMixer );
			OAMixNode = OAMixer( OAMixerNum ).MixNode;
			Tin = Node( OAMixNode ).Temp;
			Win = Node( OAMixNode ).HumRat;
		}

		// Simulate the blow-through fan if there is any
		if ( VRFTU( VRFTUNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( "", FirstHVACIteration, VRFTU( VRFTUNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

			FanOutletNode = Fan( VRFTU( VRFTUNum ).FanIndex ).OutletNodeNum;
			Tin = Node( FanOutletNode ).Temp;
			Win = Node( FanOutletNode ).HumRat;
		}

		// Call the coil control logic to determine the air flow rate to match the given coil load
		ControlVRFIUCoil( CoilIndex, QCoilReq, Tin, Win, TeTc, OACompOnMassFlow, FanSpdRatioAct, Wout, Tout, Hout, SHact, SCact );

		Hin = PsyHFnTdbW( Tin, Win );
		QCoilAct = FanSpdRatioAct * DXCoil( CoilIndex ).RatedAirMassFlowRate( Mode ) * ( Hout - Hin ); // positive for heating, negative for cooling

		AirFlowRateResidual = ( FanSpdRatioAct - FanSpdRatio ); //@@ / FanSpdRatioBase;

		return AirFlowRateResidual;

	}

	Real64
	CompResidual_FluidTCtrl(
		Real64 const Te, // Outdoor unit evaporating temperature
		Array1< Real64 > const & Par        // parameters
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Xiufeng Pang (XP)
		//       DATE WRITTEN   Mar 2013
		//       MODIFIED       Jul 2015, RP Zhang, LBNL
		//       RE-ENGINEERED
		//
		// PURPOSE OF THIS FUNCTION:
		//  	 Calculates residual function ((VRV terminal unit cooling output - Zone sensible cooling load)
		//
		// METHODOLOGY EMPLOYED:
		//
		// REFERENCES:
		// na
		//
		// USE STATEMENTS:
		using CurveManager::CurveValue;

		Real64 Tdis;
		Real64 CondHeat;
		Real64 CAPSpd;
		Real64 CompResidual;
		int CAPFT;

		Tdis     = Par( 1 );
		CondHeat = Par( 2 );
		CAPFT    = Par( 3 );

		CAPSpd = CurveValue( CAPFT, Tdis, Te );
		CompResidual = ( CondHeat - CAPSpd ) / CAPSpd;

		return CompResidual;
	}

	// Clears the global data in HVACVariableRefrigerantFlow.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumVRFCond = 0;
		NumVRFTU = 0;
		NumVRFTULists = 0;
		CompOnMassFlow = 0.0;
		OACompOnMassFlow = 0.0;
		CompOffMassFlow = 0.0;
		OACompOffMassFlow = 0.0;
		CompOnFlowRatio = 0.0;
		CompOffFlowRatio = 0.0;
		FanSpeedRatio = 0.0;
		LoopDXCoolCoilRTF = 0.0;
		LoopDXHeatCoilRTF = 0.0;
		CondenserWaterMassFlowRate = 0.0;

		GetVRFInputFlag = true;
		MyOneTimeFlag = true;
		MyOneTimeSizeFlag = true;
		ZoneEquipmentListNotChecked = true;

		VRF.deallocate();
		VRFTU.deallocate();
		TerminalUnitList.deallocate();
		VRFTUNumericFields.deallocate();
		MaxCoolingCapacity.deallocate();
		MaxHeatingCapacity.deallocate();
		CoolCombinationRatio.deallocate();
		HeatCombinationRatio.deallocate();
		MaxDeltaT.deallocate();
		MinDeltaT.deallocate();
		LastModeCooling.deallocate();
		LastModeHeating.deallocate();
		HeatingLoad.deallocate();
		CoolingLoad.deallocate();
		NumCoolingLoads.deallocate();
		SumCoolingLoads.deallocate();
		NumHeatingLoads.deallocate();
		SumHeatingLoads.deallocate();
		CheckEquipName.deallocate();
		MyEnvrnFlag.deallocate();
		MySizeFlag.deallocate();
		MyBeginTimeStepFlag.deallocate();
		MyVRFFlag.deallocate();
		MyVRFCondFlag.deallocate();
		MyZoneEqFlag.deallocate();
	}

	// End of Utility subroutines for the Module
	// *****************************************************************************

} // HVACVariableRefrigerantFlow

} // EnergyPlus
