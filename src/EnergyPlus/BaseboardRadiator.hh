#ifndef BaseboardRadiator_hh_INCLUDED
#define BaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// Note: This file contains two modules:
// Module BaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:Convective:Water)
// Module BaseboardElectric -- (ref: Object: ZoneHVAC:Baseboard:Convective:Electric)

namespace BaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const SimpConvAirFlowSpeed; // m/s

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumBaseboards;
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct BaseboardParams
	{
		// Members
		std::string EquipID;
		std::string Schedule;
		int SchedPtr;
		int EquipType;
		int ZonePtr;
		int WaterInletNode;
		int WaterOutletNode;
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 UA;
		Real64 WaterMassFlowRate;
		Real64 WaterVolFlowRateMax; // m3/s
		Real64 WaterMassFlowRateMax; // kg/s
		Real64 Offset;
		Real64 AirMassFlowRate; // kg/s
		Real64 DesAirMassFlowRate; // kg/s
		Real64 WaterInletTemp;
		Real64 WaterOutletTemp;
		Real64 WaterInletEnthalpy;
		Real64 WaterOutletEnthalpy;
		Real64 AirInletTemp;
		Real64 AirInletHumRat;
		Real64 AirOutletTemp;
		Real64 Power;
		Real64 Energy;
		int LoopNum; // plant loop index
		int LoopSideNum; // plant loop side index
		int BranchNum; // plant loop branch index
		int CompNum; // plant loop component index
		int BBLoadReSimIndex;
		int BBMassFlowReSimIndex;
		int BBInletTempFlowReSimIndex;
		int HeatingCapMethod; // - Method for water baseboard Radiator system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // -  water baseboard Radiator system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		BaseboardParams() :
			SchedPtr( 0 ),
			EquipType( 0 ),
			ZonePtr( 0 ),
			WaterInletNode( 0 ),
			WaterOutletNode( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			UA( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			WaterVolFlowRateMax( 0.0 ),
			WaterMassFlowRateMax( 0.0 ),
			Offset( 0.0 ),
			AirMassFlowRate( 0.0 ),
			DesAirMassFlowRate( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterInletEnthalpy( 0.0 ),
			WaterOutletEnthalpy( 0.0 ),
			AirInletTemp( 0.0 ),
			AirInletHumRat( 0.0 ),
			AirOutletTemp( 0.0 ),
			Power( 0.0 ),
			Energy( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			BBLoadReSimIndex( 0 ),
			BBMassFlowReSimIndex( 0 ),
			BBInletTempFlowReSimIndex( 0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		BaseboardParams(
			std::string const & EquipID,
			std::string const & Schedule,
			int const SchedPtr,
			int const EquipType,
			int const ZonePtr,
			int const WaterInletNode,
			int const WaterOutletNode,
			int const ControlCompTypeNum,
			int const CompErrIndex,
			Real64 const UA,
			Real64 const WaterMassFlowRate,
			Real64 const WaterVolFlowRateMax, // m3/s
			Real64 const WaterMassFlowRateMax, // kg/s
			Real64 const Offset,
			Real64 const AirMassFlowRate, // kg/s
			Real64 const DesAirMassFlowRate, // kg/s
			Real64 const WaterInletTemp,
			Real64 const WaterOutletTemp,
			Real64 const WaterInletEnthalpy,
			Real64 const WaterOutletEnthalpy,
			Real64 const AirInletTemp,
			Real64 const AirInletHumRat,
			Real64 const AirOutletTemp,
			Real64 const Power,
			Real64 const Energy,
			int const LoopNum, // plant loop index
			int const LoopSideNum, // plant loop side index
			int const BranchNum, // plant loop branch index
			int const CompNum, // plant loop component index
			int const BBLoadReSimIndex,
			int const BBMassFlowReSimIndex,
			int const BBInletTempFlowReSimIndex,
			int const HeatingCapMethod, // - Method for steam baseboard Radiator system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledHeatingCapacity // -  steam baseboard Radiator system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		) :
			EquipID( EquipID ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			EquipType( EquipType ),
			ZonePtr( ZonePtr ),
			WaterInletNode( WaterInletNode ),
			WaterOutletNode( WaterOutletNode ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			UA( UA ),
			WaterMassFlowRate( WaterMassFlowRate ),
			WaterVolFlowRateMax( WaterVolFlowRateMax ),
			WaterMassFlowRateMax( WaterMassFlowRateMax ),
			Offset( Offset ),
			AirMassFlowRate( AirMassFlowRate ),
			DesAirMassFlowRate( DesAirMassFlowRate ),
			WaterInletTemp( WaterInletTemp ),
			WaterOutletTemp( WaterOutletTemp ),
			WaterInletEnthalpy( WaterInletEnthalpy ),
			WaterOutletEnthalpy( WaterOutletEnthalpy ),
			AirInletTemp( AirInletTemp ),
			AirInletHumRat( AirInletHumRat ),
			AirOutletTemp( AirOutletTemp ),
			Power( Power ),
			Energy( Energy ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			BBLoadReSimIndex( BBLoadReSimIndex ),
			BBMassFlowReSimIndex( BBMassFlowReSimIndex ),
			BBInletTempFlowReSimIndex( BBInletTempFlowReSimIndex ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity )
		{}

	};

	struct BaseboardParamsNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		BaseboardParamsNumericFieldData()
		{}

		// Member Constructor
		BaseboardParamsNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};
	// Object Data
	extern Array1D< BaseboardParams > Baseboard;
	extern Array1D< BaseboardParamsNumericFieldData > BaseboardParamsNumericFields;


	// Functions

	void
	SimBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetBaseboardInput();

	void
	InitBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub
	);

	void
	SizeBaseboard( int const BaseboardNum );

	void
	SimHWConvective(
		int & BaseboardNum,
		Real64 & LoadMet
	);

	void
	UpdateBaseboard( int & BaseboardNum );

	void
	ReportBaseboard( int const BaseboardNum );

	Real64
	HWBaseboardUAResidual(
		Real64 const UA, // UA of coil
		Array1< Real64 > const & Par // par(1) = design coil load [W]
	);

	void
	UpdateBaseboardPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const FirstHVACIteration,
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	);

} // BaseboardRadiator

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//******************************************************************************************************
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//******************************************************************************************************
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	//     NOTICE
	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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


} // EnergyPlus

#endif
