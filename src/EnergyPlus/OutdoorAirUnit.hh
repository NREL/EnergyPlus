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

#ifndef OutdoorAirUnit_hh_INCLUDED
#define OutdoorAirUnit_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace OutdoorAirUnit {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// component types addressed by this module
	extern std::string const cMO_OutdoorAirUnit;

	extern int const WaterCoil_SimpleCool;
	extern int const WaterCoil_Cooling;
	extern int const WaterCoil_SimpleHeat;
	extern int const SteamCoil_AirHeat;
	extern int const WaterCoil_DetailedCool;
	extern int const WaterCoil_CoolingHXAsst;
	extern int const Coil_ElectricHeat;
	extern int const Coil_GasHeat;
	extern int const DXSystem;
	extern int const HeatXchngr;
	extern int const Desiccant;
	extern int const DXHeatPumpSystem;
	extern int const UnitarySystem;

	//  Control Types
	extern int const Neutral; // Controls system using zone mean air temperature
	extern int const Unconditioned; // Controls system when outdoor air temperature is identified with control temperature
	extern int const Temperature; // Controls system using temperature band

	// Operating Options
	extern int const HeatingMode; // normal heating coil operation
	extern int const CoolingMode; // normal cooling coil operation
	extern int const NeutralMode; // signal coil shouldn't run

	extern Array1D_string const CurrentModuleObjects;

	// Parameters below (CO - Current module Object.  used primarily in Get Inputs)
	// Multiple Get Input routines in this module or these would be in individual routines.
	extern int const CO_OAUnit;
	extern int const CO_OAEqList;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfOAUnits; // Number of outdoor air unit in the input file
	extern Real64 OAMassFlowRate; // Outside air mass flow rate for the zone outdoor air unit
	extern Array1D_bool MyOneTimeErrorFlag;
	extern bool GetOutdoorAirUnitInputFlag; // Flag set to make sure you get input once

	// Autosizing variables
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE OUTDOOR AIR UNIT
	//PRIVATE UpdateOutdoorAirUnit
	//PUBLIC GetOutAirCoilOutletTemp

	// Types

	struct OAEquipList
	{
		// Members
		// Equipment List Data
		std::string ComponentName;
		std::string ComponentType;
		int ComponentType_Num; // Parameterized Component Types this module can address
		int ComponentIndex; // Which one in list -- updated by routines called from here
		int CoilAirInletNode;
		int CoilAirOutletNode;
		int CoilWaterInletNode;
		int CoilWaterOutletNode;
		int CoilPlantTypeOfNum;
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		int FluidIndex; // used in Steam...
		Real64 MaxVolWaterFlow;
		Real64 MaxWaterMassFlow;
		Real64 MinVolWaterFlow;
		Real64 MinWaterMassFlow;
		// End Of Equipment list data

		// Default Constructor
		OAEquipList() :
			ComponentType_Num( 0 ),
			ComponentIndex( 0 ),
			CoilAirInletNode( 0 ),
			CoilAirOutletNode( 0 ),
			CoilWaterInletNode( 0 ),
			CoilWaterOutletNode( 0 ),
			CoilPlantTypeOfNum( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			FluidIndex( 0 ),
			MaxVolWaterFlow( 0.0 ),
			MaxWaterMassFlow( 0.0 ),
			MinVolWaterFlow( 0.0 ),
			MinWaterMassFlow( 0.0 )
		{}

	};

	struct OAUnitData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		int ZoneNodeNum; // index of zone air node in node structure
		std::string UnitControlType; // Control type for the system
		// (Neutral and setpoint temperatrue)
		int ControlType; // Unit Control type indicator
		int AirInletNode; // inlet air node number
		int AirOutletNode; // outlet air node number
		std::string SFanName; // name of supply fan
		int SFan_Index; // index in fan structure
		int SFanType; // type of fan in cFanTypes
		int SFanAvailSchedPtr; // supply fan availability sched from fan object
		int FanPlace; // fan placement; blow through and draw through
		Real64 FanCorTemp; // correction temperature
		bool FanEffect; // .TRUE. if unit has a fan type of draw through
		int SFanOutletNode; // supply fan outlet node number
		std::string ExtFanName; // name of exhaust fan
		int ExtFan_Index; // index in fan structure
		int ExtFanType; // type of fan in cFanTypes
		int ExtFanAvailSchedPtr; // exhaust fan availability sched from fan object
		bool ExtFan; // true if there is an exhaust fan
		std::string OutAirSchedName; // schedule of fraction for outside air (all controls)
		int OutAirSchedPtr; // index to schedule
		int OutsideAirNode; // outside air node number
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		Real64 ExtAirVolFlow; // m3/s
		Real64 ExtAirMassFlow; // kg/s
		std::string ExtAirSchedName; // schedule of fraction for exhaust air
		int ExtOutAirSchedPtr; // index to schedule
		Real64 SMaxAirMassFlow; // kg/s
		Real64 EMaxAirMassFlow; // kg/s
		Real64 SFanMaxAirVolFlow; // m3/s
		Real64 EFanMaxAirVolFlow; // m3/s
		std::string HiCtrlTempSched; // Schedule name for the High Control Air temperature
		int HiCtrlTempSchedPtr; // Schedule index for the High Control Air temperature
		std::string LoCtrlTempSched; // Schedule name for the Low Control Air temperature
		int LoCtrlTempSchedPtr; // Schedule index for the Low Control Air temperature
		int OperatingMode; // operating condition( NeutralMode, HeatingMode, CoolingMode)
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 AirMassFlow; // kg/s
		int UnBalancedErrCount; // Counter for recurring warning message
		int UnBalancedErrIndex; // Index to recurring warning message
		int NumComponents;
		std::string ComponentListName;
		Real64 CompOutSetTemp; // component outlet setpoint temperature
		int AvailStatus;
		std::string AvailManagerListName; // Name of an availability manager list object
		Array1D< OAEquipList > OAEquip;
		// Report data
		Real64 TotCoolingRate; // Rate of total cooling delivered to the zone [W]
		Real64 TotCoolingEnergy; // Total cooling energy delivered by the OAU supply air to the zone [J]
		Real64 SensCoolingRate; // Rate of sensible cooling delivered to the zone [W]
		Real64 SensCoolingEnergy; // Sensible cooling energy delivered by the OAU supply air to the zone [J]
		Real64 LatCoolingRate; // Rate of latent cooling delivered to the zone [W]
		Real64 LatCoolingEnergy; // Latent cooling energy delivered by the OAU supply air to the zone [J]
		Real64 ElecFanRate; // Total electric use rate (power) for supply/exhaust fans [W]
		Real64 ElecFanEnergy; // Electric energy use for supply fan and exhaust fan [J]
		Real64 SensHeatingEnergy; // sensible heating energy delivered by the ERV supply air to the zone [J]
		Real64 SensHeatingRate; // rate of sensible heating delivered to the zone [W]
		Real64 LatHeatingEnergy; // latent heating energy delivered by the ERV supply air to the zone [J]
		Real64 LatHeatingRate; // rate of latent heating delivered to the zone [W]
		Real64 TotHeatingEnergy; // total heating energy delivered by the ERV supply air to the zone [J]
		Real64 TotHeatingRate; // rate of total heating delivered to the zone [W]

		// Default Constructor
		OAUnitData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			ZoneNodeNum( 0 ),
			ControlType( 0 ),
			AirInletNode( 0 ),
			AirOutletNode( 0 ),
			SFan_Index( 0 ),
			SFanType( 0 ),
			SFanAvailSchedPtr( 0 ),
			FanPlace( 0 ),
			FanCorTemp( 0.0 ),
			FanEffect( false ),
			SFanOutletNode( 0 ),
			ExtFan_Index( 0 ),
			ExtFanType( 0 ),
			ExtFanAvailSchedPtr( 0 ),
			ExtFan( false ),
			OutAirSchedPtr( 0 ),
			OutsideAirNode( 0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			ExtAirVolFlow( 0.0 ),
			ExtAirMassFlow( 0.0 ),
			ExtOutAirSchedPtr( 0 ),
			SMaxAirMassFlow( 0.0 ),
			EMaxAirMassFlow( 0.0 ),
			SFanMaxAirVolFlow( 0.0 ),
			EFanMaxAirVolFlow( 0.0 ),
			HiCtrlTempSchedPtr( 0 ),
			LoCtrlTempSchedPtr( 0 ),
			OperatingMode( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			AirMassFlow( 0.0 ),
			UnBalancedErrCount( 0 ),
			UnBalancedErrIndex( 0 ),
			NumComponents( 0 ),
			CompOutSetTemp( 0.0 ),
			AvailStatus( 0 ),
			TotCoolingRate( 0.0 ),
			TotCoolingEnergy( 0.0 ),
			SensCoolingRate( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			LatCoolingRate( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			ElecFanRate( 0.0 ),
			ElecFanEnergy( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			SensHeatingRate( 0.0 ),
			LatHeatingEnergy( 0.0 ),
			LatHeatingRate( 0.0 ),
			TotHeatingEnergy( 0.0 ),
			TotHeatingRate( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< OAUnitData > OutAirUnit;

	// Functions

	void
	clear_state();

	void
	SimOutdoorAirUnit(
		std::string const & CompName, // name of the outdoor air unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetOutdoorAirUnitInputs();

	void
	InitOutdoorAirUnit(
		int const OAUnitNum, // index for the current outdoor air unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	SizeOutdoorAirUnit( int const OAUnitNum );

	void
	CalcOutdoorAirUnit(
		int & OAUnitNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // power supplied
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	SimZoneOutAirUnitComps(
		int const OAUnitNum,
		bool const FirstHVACIteration
	);

	void
	SimOutdoorAirEquipComps(
		int const OAUnitNum, // actual outdoor air unit num
		std::string const & EquipType, // the component type
		std::string const & EquipName, // the component Name
		int const EquipNum,
		int const CompTypeNum, // Component Type -- Integerized for this module
		bool const FirstHVACIteration,
		int & CompIndex,
		bool const Sim // if TRUE, simulate component
	);

	void
	CalcOAUnitCoilComps(
		int const CompNum, // actual outdoor air unit num
		bool const FirstHVACIteration,
		int const EquipIndex, // Component Type -- Integerized for this module
		Real64 & LoadMet
	);

	//SUBROUTINE UpdateOutdoorAirUnit

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateOutdoorAirUnit

	void
	ReportOutdoorAirUnit( int const OAUnitNum ); // Index for the outdoor air unit under consideration within the derived types

	int
	GetOutdoorAirUnitOutAirNode( int const OAUnitNum );

	int
	GetOutdoorAirUnitZoneInletNode( int const OAUnitNum );

	int
	GetOutdoorAirUnitReturnAirNode( int const OAUnitNum );

	//*****************************************************************************************

} // OutdoorAirUnit

} // EnergyPlus

#endif
