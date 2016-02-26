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

#ifndef FanCoilUnits_hh_INCLUDED
#define FanCoilUnits_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace FanCoilUnits {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	extern std::string const cMO_FanCoil;

	// coil operation
	extern int const On; // normal coil operation
	extern int const Off; // signal coil shouldn't run

	// coil type units supported in this module
	extern int const FanCoilUnit_4Pipe;

	extern int const CCoil_Water;
	extern int const CCoil_Detailed;
	extern int const CCoil_HXAssist;

	extern int const HCoil_Water;
	extern int const HCoil_Electric;

	//capacity control method supported in this module
	extern int const CCM_ConsFanVarFlow;
	extern int const CCM_CycFan;
	extern int const CCM_VarFanVarFlow;
	extern int const CCM_VarFanConsFlow;
	extern int const CCM_MultiSpeedFan;
	extern int const CCM_ASHRAE;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumFanCoils;
	extern int Num4PipeFanCoils;
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern bool GetFanCoilInputFlag; // First time, input is "gotten"
	extern Real64 FanFlowRatio;
	extern bool HeatingLoad; // True when zone needs heating
	extern bool CoolingLoad; // True when zone needs cooling
	extern Real64 const Small5WLoad; // load threshold 5.0 W

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// look up functions for node numbers

	// Types

	struct FanCoilData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		std::string UnitType; // type of unit
		int UnitType_Num;
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		std::string SchedOutAir; // outside air schedule, multipliy maximum outdoor air flow rate
		int SchedOutAirPtr; // index to outside air schedule
		int FanType_Num; // index to fan type
		std::string CapCtrlMeth; // type of capacity control method
		// 'ConstantFanVariableFlow' or
		// 'CyclingFan' or
		// 'VariableFanVariableFlow'
		int SpeedFanSel; // Speed fan selected
		int CapCtrlMeth_Num;
		Real64 PLR; // Part Load Ratio, fraction of time step fancoil is on
		int MaxIterIndexH; // Maximum iterations exceeded for heating
		int MaxIterIndexC; // Maximum iterations exceeded for cooling
		Real64 FanAirVolFlow; // m3/s
		Real64 MaxAirVolFlow; // m3/s
		Real64 MaxAirMassFlow; // kg/s
		Real64 LowSpeedRatio; // Low speed fan supply air flow ratio
		Real64 MedSpeedRatio; // Medium speed fan supply air flow ratio
		Real64 SpeedFanRatSel; // Speed fan ratio determined by fan speed selection at each timestep
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		int AirInNode; // inlet air node number
		int AirOutNode; // outlet air node number
		int OutsideAirNode; // outside air node number
		int AirReliefNode; // relief air node number
		int MixedAirNode; // Mixed Air Node number
		int ColdControlNode; // chilled water control node
		int ColdPlantOutletNode; // chilled water coil outlet plant node
		int HotControlNode; // hot water control node
		int HotPlantOutletNode; // hot water coil outlet plant node
		std::string OAMixName; // name of outside air mixer
		std::string OAMixType; // type of outside air mixer
		int OAMixIndex;
		std::string FanName; // name of fan
		std::string FanType; // type of fan
		int FanIndex; // index for fan
		std::string CCoilName; // name of cooling coil
		int CCoilName_Index; // Index for this Cooling Coil in SimWaterComp
		std::string CCoilType; // type of cooling coil:
		// 'Coil:Cooling:Water' or
		// 'Coil:Cooling:Water:DetailedGeometry' or
		// 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
		int CCoilType_Num; // Numeric equivalent for type of cooling coil
		std::string CCoilPlantName; // name of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
		std::string CCoilPlantType; // type of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
		int CCoilPlantTypeOfNum;
		int CWLoopNum; // index for plant loop with chilled water coil
		int CWLoopSide; // index for plant loop side for chilled water coil
		int CWBranchNum; // index for plant branch for chilled water coil
		int CWCompNum; // index for plant component for chilled water coil
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 MaxColdWaterVolFlow; // m3/s
		Real64 MaxColdWaterFlow; // kg/s
		Real64 MinColdWaterVolFlow; // m3/s
		Real64 MinColdWaterFlow; // kg/s
		Real64 ColdControlOffset; // control tolerance
		std::string HCoilName; // name of heating coil
		int HCoilName_Index;
		std::string HCoilType; // type of heating coil:
		// 'Coil:Heating:Water' or
		int HCoilType_Num; // Numeric equivalent for type of cooling coil
		int HCoilPlantTypeOfNum;
		int HWLoopNum; // index for plant loop with hot water coil
		int HWLoopSide; // index for plant loop side for hot water coil
		int HWBranchNum; // index for plant branch for hot water coil
		int HWCompNum; // index for plant component for hot water coil
		Real64 MaxHotWaterVolFlow; // m3/s
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MinHotWaterVolFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		Real64 HotControlOffset; // control tolerance
		Real64 DesignHeatingCapacity; // size of electric heating coil [W]
		int AvailStatus;
		std::string AvailManagerListName; // Name of an availability manager list object
		// addition for OA to Zone Units
		bool ATMixerExists; // True if there is an ATMixer
		std::string ATMixerName; // name of air terminal mixer
		int ATMixerIndex; // index to the air terminal mixer
		int ATMixerType; // 1 = inlet side mixer, 2 = supply side mixer
		int ATMixerPriNode; // primary inlet air node number for the air terminal mixer
		int ATMixerSecNode; // secondary air inlet node number for the air terminal mixer
		int ATMixerOutNode; // outlet air node number for the air terminal mixer
		int ZonePtr; // pointer to a zone served by a fancoil unit
		int HVACSizingIndex; // index of a HVACSizing object for a fancoil unit
		Real64 SpeedRatio; // speed ratio when the fan is cycling between stages
		int FanOpModeSchedPtr; // pointer to supply air fan operating mode schedule
		int FanOpMode; // 1=cycling fan cycling coil; 2=constant fan cycling coil
		Real64 MinSATempCooling; // ASHRAE90.1 maximum supply air temperature in Cooling mode
		Real64 MaxSATempHeating; // ASHRAE90.1 maximum supply air temperature in Heating mode
		bool ASHRAETempControl; // ASHRAE90.1 control to temperature set point when true

		// Report data
		Real64 HeatPower; // unit heating output in watts
		Real64 HeatEnergy; // unit heating output in J
		Real64 TotCoolPower; // unit total cooling power output in watts
		Real64 TotCoolEnergy; // unit total cooling energy output in joules
		Real64 SensCoolPower; // unit sensible cooling power output in watts
		Real64 SensCoolEnergy; // unit sensible cooling energy output in joules
		Real64 ElecPower; // unit electric power consumption in watts
		Real64 ElecEnergy; // unit electiric energy consumption in joules
		Real64 DesCoolingLoad; // used for reporting in watts
		Real64 DesHeatingLoad; // used for reporting in watts
		Real64 DesZoneCoolingLoad; // used for reporting in watts
		Real64 DesZoneHeatingLoad; // used for reporting in watts
		int DSOAPtr; // design specification outdoor air object index

		// Default Constructor
		FanCoilData() :
			UnitType_Num( 0 ),
			SchedPtr( 0 ),
			SchedOutAirPtr( 0 ),
			FanType_Num( 0 ),
			SpeedFanSel( 0 ),
			CapCtrlMeth_Num( 0 ),
			PLR( 0.0 ),
			MaxIterIndexH( 0 ),
			MaxIterIndexC( 0 ),
			FanAirVolFlow( 0.0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			LowSpeedRatio( 0.0 ),
			MedSpeedRatio( 0.0 ),
			SpeedFanRatSel( 0.0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			OutsideAirNode( 0 ),
			AirReliefNode( 0 ),
			MixedAirNode( 0 ),
			ColdControlNode( 0 ),
			ColdPlantOutletNode( 0 ),
			HotControlNode( 0 ),
			HotPlantOutletNode( 0 ),
			OAMixIndex( 0 ),
			FanIndex( 0 ),
			CCoilName_Index( 0 ),
			CCoilType_Num( 0 ),
			CCoilPlantTypeOfNum( 0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			MaxColdWaterVolFlow( 0.0 ),
			MaxColdWaterFlow( 0.0 ),
			MinColdWaterVolFlow( 0.0 ),
			MinColdWaterFlow( 0.0 ),
			ColdControlOffset( 0.0 ),
			HCoilName_Index( 0 ),
			HCoilType_Num( 0 ),
			HCoilPlantTypeOfNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			MaxHotWaterVolFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MinHotWaterVolFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			HotControlOffset( 0.0 ),
			DesignHeatingCapacity( 0.0 ),
			AvailStatus( 0 ),
			ATMixerExists( false ),
			ATMixerIndex( 0 ),
			ATMixerType( 0 ),
			ATMixerPriNode( 0 ),
			ATMixerSecNode( 0 ),
			ATMixerOutNode( 0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 ),
			SpeedRatio( 0.0 ),
			FanOpModeSchedPtr( 0 ),
			FanOpMode( 1 ),
			MinSATempCooling( 0.0 ),
			MaxSATempHeating( 0.0 ),
			ASHRAETempControl( false ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			TotCoolPower( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensCoolPower( 0.0 ),
			SensCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			DesCoolingLoad( 0.0 ),
			DesHeatingLoad( 0.0 ),
			DesZoneCoolingLoad( 0.0 ),
			DesZoneHeatingLoad( 0.0 ),
			DSOAPtr( 0 )
		{}

	};

	struct FanCoilNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		FanCoilNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< FanCoilData > FanCoil;
	extern Array1D< FanCoilNumericFieldData > FanCoilNumericFields;

	// Functions

	void
	clear_state();

	void
	SimFanCoilUnit(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		int const ControlledZoneNum, // index into ZoneEquipConfig array; may not be equal to ZoneNum
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetFanCoilUnits();

	void
	InitFanCoilUnits(
		int const FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum // number of zone being served
	);

	void
	SizeFanCoilUnit( int const FanCoilNum );

	void
		SizeCoilWaterFlowRate(
		std::string const & WaterCoilType,
		std::string const & WaterCoilName,
		int const WaterCoilType_Num,
		int const WLoopNum,
		Real64 & MaxWaterVolFlowDes,
		Real64 & DesignLoad,
		bool & ErrorsFound
	);

	void
	Sim4PipeFanCoil(
		int & FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		int const ControlledZoneNum, // index into ZoneEqupConfig
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	Calc4PipeFanCoil(
		int const FanCoilNum, // Unit index in fan coil array
		int const ControlledZoneNum, // ZoneEquipConfig index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional< Real64 > PLR = _ // Part Load Ratio, fraction of time step fancoil is on
	);

	void
	SimMultiStage4PipeFanCoil(
		int & FanCoilNum, // number of the current fan coil unit being simulated
		int const ControlledZoneNum, // index into ZoneEqupConfig
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet // Sensible power supplied (W)
	);

	void
	CalcMultiStage4PipeFanCoil(
		int & FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 const QZnReq, // current zone cooling or heating load
		Real64 & SpeedRatio, // fan coil speed ratio
		Real64 & PartLoadRatio, // fan coil part load ratio
		Real64 & PowerMet // Sensible power supplied (W)
	);

	void
	ReportFanCoilUnit( int const FanCoilNum ); // number of the current fan coil unit being simulated

	int
	GetFanCoilZoneInletAirNode( int const FanCoilNum );

	int
	GetFanCoilOutAirNode( int const FanCoilNum );

	int
	GetFanCoilReturnAirNode( int const FanCoilNum );

	int
	GetFanCoilMixedAirNode( int const FanCoilNum );

	int
	GetFanCoilInletAirNode( int const FanCoilNum );

	void
	GetFanCoilIndex(
		std::string const & FanCoilName,
		int & FanCoilIndex
	);

	Real64
	CalcFanCoilLoadResidual(
		Real64 const PartLoadRatio, // DX cooling coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	);

	Real64
	CalcFanCoilWaterFlowTempResidual(
		Real64 const WaterFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
	);
	
	Real64
	CalcFanCoilWaterFlowResidual(
		Real64 const WaterFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
	);
	
	Real64
	CalcFanCoilAirFlowResidual(
		Real64 const WaterFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
	);

	Real64
	CalcFanCoilAirAndWaterFlowResidual(
		Real64 const WaterFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
	);

	Real64
	CalcFanCoilAirAndWaterInStepResidual(
		Real64 const PLR, // air and water mass flow rate ratio
		Array1< Real64 > const & Par // Function parameters
	);

	Real64
	CalcFanCoilBothFlowResidual(
		Real64 const PLR, // air and water mass flow rate ratio
		Array1< Real64 > const & Par // Function parameters
	);

	Real64
	CalcFanCoilElecHeatResidual(
		Real64 const PLR, // electric heating coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	);

	Real64
	CalcFanCoilElecHeatTempResidual(
		Real64 const PLR, // electric heating coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	);

	//     NOTICE

	//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // FanCoilUnits

} // EnergyPlus

#endif
