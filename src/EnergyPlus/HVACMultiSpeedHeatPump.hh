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

#ifndef HVACMultiSpeedHeatPump_hh_INCLUDED
#define HVACMultiSpeedHeatPump_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACMultiSpeedHeatPump {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS

	// Heating coil types
	extern int const MultiSpeedHeatingCoil; // COIL:DX:MultiSpeed:Heating
	// Cooling coil types
	extern int const MultiSpeedCoolingCoil; // COIL:DX:MultiSpeed:Cooling
	// Supplymental heating coil types
	extern int const SuppHeatingCoilGas; // Supplymental heating coil type: COIL:GAS:HEATING
	extern int const SuppHeatingCoilElec; // Supplymental heating coil type: COIL:ELECTRIC:HEATING
	extern int const SuppHeatingCoilRec; // Supplymental heating coil type: COIL:ENGINEHEATRECOVERY:HEATING

	// Curve Types
	extern int const Linear; // Linear curve type
	extern int const BiLinear; // Bi-linear curve type
	extern int const Quadratic; // Quadratic curve type
	extern int const BiQuadratic; // Bi-quadratic curve type
	extern int const Cubic; // Cubic curve type

	// Mode of operation
	extern int const CoolingMode; // System operating mode is cooling
	extern int const HeatingMode; // System operating mode is heating

	// Airflow control for contant fan mode
	extern int const UseCompressorOnFlow; // set compressor OFF air flow rate equal to compressor ON air flow rate
	extern int const UseCompressorOffFlow; // set compressor OFF air flow rate equal to user defined value
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumMSHeatPumps; // Number of multi speed heat pumps
	extern int AirLoopPass; // Number of air loop pass
	extern Real64 TempSteamIn; // steam coil steam inlet temperature

	extern std::string CurrentModuleObject; // Object type for getting and error messages
	extern Real64 CompOnMassFlow; // System air mass flow rate w/ compressor ON
	extern Real64 CompOffMassFlow; // System air mass flow rate w/ compressor OFF
	extern Real64 CompOnFlowRatio; // fan flow ratio when coil on
	extern Real64 CompOffFlowRatio; // fan flow ratio when coil off
	extern Real64 FanSpeedRatio; // fan speed ratio passed to on/off fan object
	extern Real64 SupHeaterLoad; // load to be met by supplemental heater [W]
	extern Real64 SaveLoadResidual; // Saved load residual used to check convergence
	extern Real64 SaveCompressorPLR; // holds compressor PLR from active DX coil
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct MSHeatPumpData
	{
		// Members
		//          Some variables in this type are arrays (dimension=MaxSpeed) to support the number of speeds
		std::string Name; // Name of the engine driven heat pump
		std::string AvaiSchedule; // Availability Schedule name
		int AvaiSchedPtr; // Pointer to the correct schedule
		int AirInletNodeNum; // Node number of the heat pump air inlet
		int AirOutletNodeNum; // Node number of the heat pump air inlet
		std::string AirInletNodeName; // Node name of the heat pump air inlet
		std::string AirOutletNodeName; // Node name of the heat pump air outlet
		int ControlZoneNum; // Controlling zone or thermostat location
		int ZoneSequenceCoolingNum; // Index to cooling sequence/priority for this zone
		int ZoneSequenceHeatingNum; // Index to heating sequence/priority for this zone
		std::string ControlZoneName; // Controlled zone name
		int NodeNumOfControlledZone; // Controlled zone node number
		Real64 FlowFraction; // Fraction of the total volume flow that goes through the controlling zone
		std::string FanName; // Name of supply air fan
		int FanType; // Supply fan type
		int FanNum; // Supply fan number
		int FanPlaceType; // Supply air fan placement: 1 Blow through; 2 Draw through
		int FanInletNode; // Fan Inlet node
		int FanOutletNode; // Fan Outlet node
		Real64 FanVolFlow; // Supply fan volumetric flow rate
		std::string FanSchedule; // Supply air fan operating mode schedule name
		int FanSchedPtr; // Pointer to the Supply air fan operating mode schedule
		int OpMode; // mode of operation; 1=cycling fan, cycling compressor; 2=continuous fan, cycling compresor
		std::string DXHeatCoilName; // COIL:DX:MultiSpeed:Heating name
		int HeatCoilType; // Heating coil type: 1 COIL:DX:MultiSpeed:Heating only
		int HeatCoilNum; // Heating coil number
		int DXHeatCoilIndex; // DX heating coil index number
		Real64 MinOATCompressor; // Minimum outdoor dry-bulb temperature for compressor operation
		std::string HeatCoilName; // Coil:Electric:MultiSpeed:Heating OR Coil:Gas:MultiSpeed:Heating name
		int HeatCoilIndex; // heating coil index number (Coil:Electric:MultiSpeed:Heating OR Coil:Gas:MultiSpeed:Heating)
		std::string DXCoolCoilName; // COIL:DX:MultiSpeed:Cooling name
		int CoolCoilType; // Cooling coil type: 1 COIL:DX:MultiSpeed:Cooling only
		int CoolCoilNum; // Cooling coil number
		int DXCoolCoilIndex; // DX cooling coil index number
		std::string SuppHeatCoilName; // Supplymental heating coil name
		int SuppHeatCoilType; // Supplymental heating coil type: 1 Gas; 2 Electric; 3 Recovery
		int SuppHeatCoilNum; // Supplymental heating coil number
		Real64 DesignSuppHeatingCapacity; // Supplemental heating coil design capacity
		Real64 SuppMaxAirTemp; // Maximum supply air temperature from supplemental heater
		Real64 SuppMaxOATemp; // Maximum outdoor dry-bulb temperature for supplemental heater operation
		Real64 AuxOnCyclePower; // Auxiliary On-Cycle Electric Power
		Real64 AuxOffCyclePower; // Auxiliary Off-Cycle Electric Power
		Real64 DesignHeatRecFlowRate; // Design water volume flow rate through heat recovery loop [m3/s]
		bool HeatRecActive; // True when entered Heat Rec Vol Flow Rate > 0
		std::string HeatRecName; // heat recovery water inlet name
		int HeatRecInletNodeNum; // Node number on heat recovery water inlet
		int HeatRecOutletNodeNum; // Node number on heat recovery water outlet
		Real64 MaxHeatRecOutletTemp; // Maximum outlet water temperature for heat recovery
		Real64 DesignHeatRecMassFlowRate; // Design water mass flow rate through heat recovery loop [kg/s]
		int HRLoopNum; // plant loop number for heat recovery
		int HRLoopSideNum; // Plant loop side for heat recovery
		int HRBranchNum; // plant loop branch for heat recovery
		int HRCompNum; // plant loop component for heat recovery
		Real64 AuxElecPower; // Auxiliary Electric Power
		Real64 IdleVolumeAirRate; // Supply air volumetric flow rate when no cooling or heating is needed
		Real64 IdleMassFlowRate; // Supply air mass flow rate when no cooling or heating is needed
		Real64 IdleSpeedRatio; // Fan speed ratio in idle mode
		int NumOfSpeedCooling; // The number of speeds for cooling
		int NumOfSpeedHeating; // The number of speeds for heating
		Array1D< Real64 > HeatVolumeFlowRate; // Supply air volume flow rate during heating operation
		Array1D< Real64 > HeatMassFlowRate; // Supply air mass flow rate during heating operation
		Array1D< Real64 > CoolVolumeFlowRate; // Supply air volume flow rate during cooling operation
		Array1D< Real64 > CoolMassFlowRate; // Supply air mass flow rate during cooling operation
		Array1D< Real64 > HeatingSpeedRatio; // Fan speed ratio in heating mode
		Array1D< Real64 > CoolingSpeedRatio; // Fan speed ratio in cooling mode
		bool CheckFanFlow; // Supply airflow check
		int LastMode; // MSHP operation mode
		int HeatCoolMode; // System operating mode (0 = floating, 1 = cooling, 2 = heating)
		int AirLoopNumber; // Air loop served by the engine driven heat pump system
		int NumControlledZones; // Number of controlled zones for this system
		int ZoneInletNode; // Zone inlet node number in the controlled zone
		Real64 CompPartLoadRatio; // Compressor part load ratio
		Real64 FanPartLoadRatio; // Fan part load ratio
		Real64 TotCoolEnergyRate; // Total cooling enertgy rate
		Real64 TotHeatEnergyRate; // Total heating enertgy rate
		Real64 SensCoolEnergyRate; // Sensible cooling enertgy rate
		Real64 SensHeatEnergyRate; // Sensible heating enertgy rate
		Real64 LatCoolEnergyRate; // Latent cooling enertgy rate
		Real64 LatHeatEnergyRate; // Latent heating enertgy rate
		Real64 ElecPower; // Electric power (fan + supplemental electric coil)
		Real64 LoadMet; // met system load
		Real64 HeatRecoveryRate; // Heat recovery rate [W]
		Real64 HeatRecoveryInletTemp; // Inlet temperature for heat recovery rate [C]
		Real64 HeatRecoveryOutletTemp; // Outlet temperature for heat recovery rate [C]
		Real64 HeatRecoveryMassFlowRate; // Mass flow rate for heat recovery rate [kg/s]
		int AirFlowControl; // fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
		int ErrIndexCyc; // Error index at low speed
		int ErrIndexVar; // Error index at high speed
		Real64 LoadLoss; // Air distribution system loss
		int SuppCoilAirInletNode; // air inlet node number of supplemental heating coil
		int SuppCoilAirOutletNode; // air outlet node number of supplemental heating coil
		int SuppHeatCoilType_Num; // Numeric Equivalent for Supplemental Heat Coil Type
		int SuppHeatCoilIndex; // Index to supplemental heater
		int SuppCoilControlNode; // control node for simple water and steam heating coil
		Real64 MaxSuppCoilFluidFlow; // water or steam mass flow rate for supplemental heating coil [kg/s]
		int SuppCoilOutletNode; // outlet node for hot water and steam supplemental heating coil
		int CoilAirInletNode; // air inlet node number of supplemental heating coil
		int CoilControlNode; // control node for simple water and steam heating coil
		Real64 MaxCoilFluidFlow; // water or steam mass flow rate for supplemental heating coil [kg/s]
		int CoilOutletNode; // outlet node for hot water and steam supplemental heating coil
		int HotWaterCoilControlNode;
		int HotWaterCoilOutletNode;
		std::string HotWaterCoilName;
		int HotWaterCoilNum;
		int LoopNum; // plant loop index for hot water and steam heating coil
		int LoopSide; // plant loop side  index for hot water and steam heating coil
		int BranchNum; // plant loop branch index for water and steam heating coil
		int CompNum; // plant loop component index for hot water and steam heating coil
		int SuppLoopNum; // plant loop index for hot water and steam supplemental heating coil
		int SuppLoopSide; // plant loop side  index for hot water and steam supplemental heating coil
		int SuppBranchNum; // plant loop branch index for water and steam supplemental heating coil
		int SuppCompNum; // plant loop component index for hot water and steam supplemental heating coil
		int HotWaterLoopNum; // plant loop index for hot water and steam heating coil
		int HotWaterLoopSide; // plant loop side  index for hot water and steam heating coil
		int HotWaterBranchNum; // plant loop branch index for water and steam heating coil
		int HotWaterCompNum; // plant loop component index for hot water and steam heating coil
		int HotWaterCoilMaxIterIndex; // Index to recurring warning message
		int HotWaterCoilMaxIterIndex2; // Index to recurring warning message
		int StageNum; // Stage number specified by staged thermostat
		bool Staged; // Using Staged thermostat
		int CoolCountAvail; // Counter used to minimize the occurrence of output warnings
		int CoolIndexAvail; // Index used to minimize the occurrence of output warnings
		int HeatCountAvail; // Counter used to minimize the occurrence of output warnings
		int HeatIndexAvail; // Index used to minimize the occurrence of output warnings

		// Default Constructor
		MSHeatPumpData() :
			AvaiSchedPtr( 0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			ControlZoneNum( 0 ),
			ZoneSequenceCoolingNum( 0 ),
			ZoneSequenceHeatingNum( 0 ),
			NodeNumOfControlledZone( 0 ),
			FlowFraction( 0.0 ),
			FanType( 0 ),
			FanNum( 0 ),
			FanPlaceType( 0 ),
			FanInletNode( 0 ),
			FanOutletNode( 0 ),
			FanVolFlow( 0.0 ),
			FanSchedPtr( 0 ),
			OpMode( 0 ),
			HeatCoilType( 0 ),
			HeatCoilNum( 0 ),
			DXHeatCoilIndex( 0 ),
			MinOATCompressor( 0.0 ),
			HeatCoilIndex( 0 ),
			CoolCoilType( 0 ),
			CoolCoilNum( 0 ),
			DXCoolCoilIndex( 0 ),
			SuppHeatCoilType( 0 ),
			SuppHeatCoilNum( 0 ),
			DesignSuppHeatingCapacity( 0.0 ),
			SuppMaxAirTemp( 0.0 ),
			SuppMaxOATemp( 0.0 ),
			AuxOnCyclePower( 0.0 ),
			AuxOffCyclePower( 0.0 ),
			DesignHeatRecFlowRate( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			MaxHeatRecOutletTemp( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			AuxElecPower( 0.0 ),
			IdleVolumeAirRate( 0.0 ),
			IdleMassFlowRate( 0.0 ),
			IdleSpeedRatio( 0.0 ),
			NumOfSpeedCooling( 0 ),
			NumOfSpeedHeating( 0 ),
			CheckFanFlow( true ),
			LastMode( 0 ),
			HeatCoolMode( 0 ),
			AirLoopNumber( 0 ),
			NumControlledZones( 0 ),
			ZoneInletNode( 0 ),
			CompPartLoadRatio( 0.0 ),
			FanPartLoadRatio( 0.0 ),
			TotCoolEnergyRate( 0.0 ),
			TotHeatEnergyRate( 0.0 ),
			SensCoolEnergyRate( 0.0 ),
			SensHeatEnergyRate( 0.0 ),
			LatCoolEnergyRate( 0.0 ),
			LatHeatEnergyRate( 0.0 ),
			ElecPower( 0.0 ),
			LoadMet( 0.0 ),
			HeatRecoveryRate( 0.0 ),
			HeatRecoveryInletTemp( 0.0 ),
			HeatRecoveryOutletTemp( 0.0 ),
			HeatRecoveryMassFlowRate( 0.0 ),
			AirFlowControl( 0 ),
			ErrIndexCyc( 0 ),
			ErrIndexVar( 0 ),
			LoadLoss( 0.0 ),
			SuppCoilAirInletNode( 0 ),
			SuppCoilAirOutletNode( 0 ),
			SuppHeatCoilType_Num( 0 ),
			SuppHeatCoilIndex( 0 ),
			SuppCoilControlNode( 0 ),
			MaxSuppCoilFluidFlow( 0.0 ),
			SuppCoilOutletNode( 0 ),
			CoilAirInletNode( 0 ),
			CoilControlNode( 0 ),
			MaxCoilFluidFlow( 0.0 ),
			CoilOutletNode( 0 ),
			HotWaterCoilControlNode( 0 ),
			HotWaterCoilOutletNode( 0 ),
			HotWaterCoilNum( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			SuppLoopNum( 0 ),
			SuppLoopSide( 0 ),
			SuppBranchNum( 0 ),
			SuppCompNum( 0 ),
			HotWaterLoopNum( 0 ),
			HotWaterLoopSide( 0 ),
			HotWaterBranchNum( 0 ),
			HotWaterCompNum( 0 ),
			HotWaterCoilMaxIterIndex( 0 ),
			HotWaterCoilMaxIterIndex2( 0 ),
			StageNum( 0 ),
			Staged( false ),
			CoolCountAvail( 0 ),
			CoolIndexAvail( 0 ),
			HeatCountAvail( 0 ),
			HeatIndexAvail( 0 )
		{}

	};

	struct MSHeatPumpReportData
	{
		// Members
		Real64 ElecPowerConsumption; // Electricity power comsumption: CondenserFan+CrankcaseHeater+Defroster+aux
		Real64 HeatRecoveryEnergy; // Heat recovery rate [J]
		Real64 CycRatio; // Cycle ratio
		Real64 SpeedRatio; // Speed ratio between two stages
		int SpeedNum; // Speed number
		Real64 AuxElecCoolConsumption; // Auxiliary electricity power consumption during cooling
		Real64 AuxElecHeatConsumption; // Auxiliary electricity power consumption during heating

		// Default Constructor
		MSHeatPumpReportData() :
			ElecPowerConsumption( 0.0 ),
			HeatRecoveryEnergy( 0.0 ),
			CycRatio( 0.0 ),
			SpeedRatio( 0.0 ),
			SpeedNum( 0 ),
			AuxElecCoolConsumption( 0.0 ),
			AuxElecHeatConsumption( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< MSHeatPumpData > MSHeatPump;
	extern Array1D< MSHeatPumpReportData > MSHeatPumpReport;

	// Functions

	void
	SimMSHeatPump(
		std::string const & CompName, // Name of the unitary engine driven heat pump system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system time step
		int const AirLoopNum, // air loop index
		int & CompIndex // Index to changeover-bypass VAV system
	);

	//******************************************************************************

	void
	SimMSHP(
		int const MSHeatPumpNum, // number of the current engine driven Heat Pump being simulated
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QSensUnitOut, // cooling/heating deliveded to zones [W]
		Real64 const QZnReq, // required zone load
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	);

	//******************************************************************************

	void
	GetMSHeatPumpInput();

	//******************************************************************************

	void
	InitMSHeatPump(
		int const MSHeatPumpNum, // Engine driven heat pump number
		bool const FirstHVACIteration, // TRUE if first HVAC iteration
		int const AirLoopNum, // air loop index
		Real64 & QZnReq, // Heating/Cooling load for all served zones
		Real64 & OnOffAirFlowRatio // Ratio of compressor ON airflow to average airflow over timestep
	);

	//******************************************************************************

	void
	SizeMSHeatPump( int const MSHeatPumpNum ); // Engine driven heat pump number

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
	);

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
	);

	//******************************************************************************

	Real64
	MSHPCyclingResidual(
		Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = MSHPNum
	);

	//******************************************************************************

	Real64
	MSHPVarSpeedResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = MSHPNum
	);

	//******************************************************************************

	void
	UpdateMSHeatPump( int const MSHeatPumpNum ); // Engine driven heat pump number

	//******************************************************************************

	void
	ReportMSHeatPump( int const MSHeatPumpNum ); // Engine driven heat pump number

	void
	MSHPHeatRecovery( int const MSHeatPumpNum ); // Number of the current electric MSHP being simulated

	void
	SetAverageAirFlow(
		int const MSHeatPumpNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio, // ratio of compressor ON airflow to average airflow over timestep
		Optional_int_const SpeedNum = _, // Speed number
		Optional< Real64 const > SpeedRatio = _ // Speed ratio
	);

	void
	CalcNonDXHeatingCoils(
		int const MSHeatPumpNum, // multispeed heatpump index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 const HeatingLoad, // supplemental coil load to be met by unit (watts)
		int const FanMode, // fan operation mode
		Real64 & HeatCoilLoadmet, // Heating Load Met
		Optional< Real64 const > PartLoadFrac = _
	);

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	);

} // HVACMultiSpeedHeatPump

} // EnergyPlus

#endif
