#ifndef SteamBaseboardRadiator_hh_INCLUDED
#define SteamBaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SteamBaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern std::string const cCMO_BBRadiator_Steam;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumSteamBaseboards;
	extern int SteamIndex;

	extern Array1D< Real64 > QBBSteamRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QBBSteamRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone
	// with no source

	// Record keeping variables used to calculate QBBRadSrcAvg locally
	extern Array1D< Real64 > LastQBBSteamRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct SteamBaseboardParams
	{
		// Members
		std::string EquipID;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr; // Pointer to the correct schedule
		int SteamInletNode; // Inlet steam baseboard node
		int SteamOutletNode; // Outlet steam baseboard node
		int TotSurfToDistrib; // Total numbers of the surfaces that the radiant heat gets distributed
		int FluidIndex; // Fluid index for FluidProperties (Steam)
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 DegOfSubcooling; // Temperature differences due to subcooling of the condensate [C]
		Real64 Offset; // Control accuracy
		Real64 SteamMassFlowRate; // Mass flow rate of steam passing through the heater [kg/s]
		Real64 SteamMassFlowRateMax; // Maximum mass flow rate of steam [kg/s]
		Real64 SteamVolFlowRateMax; // Maximum volumetric flow rate of steam [m3/s]
		Real64 SteamOutletTemp; // Outlet steam temperature from the heater [C]
		Real64 SteamInletTemp; // Inlet steam temperature [C]
		Real64 SteamInletEnthalpy; // Enthalpy of the steam delivered from the boiler [J/kg]
		Real64 SteamOutletEnthalpy; // Enthalpy of the steam leaving the heater [J/kg]
		Real64 SteamInletPress; // Pressure of steam at the inlet of the heater [Pa]
		Real64 SteamOutletPress; // Pressure of steam at the outlet of the heater [Pa]
		Real64 SteamInletQuality; // Quality of steam at the inlet of the heater [Pa]
		Real64 SteamOutletQuality; // Quality of steam at the outlet of the heater [Pa]
		Real64 FracRadiant; // User defined fraction for radiant heat addition
		Real64 FracConvect; // Fraction for convective heat addition
		Real64 FracDistribPerson; // Fraction for radiant heat incident on people
		Array1D< Real64 > FracDistribToSurf;
		Real64 TotPower; // Convective system impact rate that the heater actually meets [W]
		Real64 Power; // Maximum heating rate [W]
		Real64 ConvPower; // Convective heating rate [W]
		Real64 RadPower; // Radiant heating rate [W]
		Real64 TotEnergy; // Convective system impact energy [J]
		Real64 Energy; // Maximum heating energy [J]
		Real64 ConvEnergy; // Convective heating energy [J]
		Real64 RadEnergy; // Radiant heating energy [J]
		int LoopNum; // plant loop index
		int LoopSideNum; // plant loop side index
		int BranchNum; // plant loop branch index
		int CompNum; // plant loop component index
		int BBLoadReSimIndex;
		int BBMassFlowReSimIndex;
		int BBInletTempFlowReSimIndex;
		int HeatingCapMethod; // - Method for steam baseboard Radiator system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // -  steam baseboard Radiator system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		SteamBaseboardParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			SteamInletNode( 0 ),
			SteamOutletNode( 0 ),
			TotSurfToDistrib( 0 ),
			FluidIndex( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			DegOfSubcooling( 0.0 ),
			Offset( 0.0 ),
			SteamMassFlowRate( 0.0 ),
			SteamMassFlowRateMax( 0.0 ),
			SteamVolFlowRateMax( 0.0 ),
			SteamOutletTemp( 0.0 ),
			SteamInletTemp( 0.0 ),
			SteamInletEnthalpy( 0.0 ),
			SteamOutletEnthalpy( 0.0 ),
			SteamInletPress( 0.0 ),
			SteamOutletPress( 0.0 ),
			SteamInletQuality( 0.0 ),
			SteamOutletQuality( 0.0 ),
			FracRadiant( 0.0 ),
			FracConvect( 0.0 ),
			FracDistribPerson( 0.0 ),
			TotPower( 0.0 ),
			Power( 0.0 ),
			ConvPower( 0.0 ),
			RadPower( 0.0 ),
			TotEnergy( 0.0 ),
			Energy( 0.0 ),
			ConvEnergy( 0.0 ),
			RadEnergy( 0.0 ),
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
		SteamBaseboardParams(
			std::string const & EquipID,
			int const EquipType,
			std::string const & Schedule,
			Array1_string const & SurfaceName,
			Array1_int const & SurfacePtr,
			int const ZonePtr,
			int const SchedPtr, // Pointer to the correct schedule
			int const SteamInletNode, // Inlet steam baseboard node
			int const SteamOutletNode, // Outlet steam baseboard node
			int const TotSurfToDistrib, // Total numbers of the surfaces that the radiant heat gets distributed
			int const FluidIndex, // Fluid index for FluidProperties (Steam)
			int const ControlCompTypeNum,
			int const CompErrIndex,
			Real64 const DegOfSubcooling, // Temperature differences due to subcooling of the condensate [C]
			Real64 const Offset, // Control accuracy
			Real64 const SteamMassFlowRate, // Mass flow rate of steam passing through the heater [kg/s]
			Real64 const SteamMassFlowRateMax, // Maximum mass flow rate of steam [kg/s]
			Real64 const SteamVolFlowRateMax, // Maximum volumetric flow rate of steam [m3/s]
			Real64 const SteamOutletTemp, // Outlet steam temperature from the heater [C]
			Real64 const SteamInletTemp, // Inlet steam temperature [C]
			Real64 const SteamInletEnthalpy, // Enthalpy of the steam delivered from the boiler [J/kg]
			Real64 const SteamOutletEnthalpy, // Enthalpy of the steam leaving the heater [J/kg]
			Real64 const SteamInletPress, // Pressure of steam at the inlet of the heater [Pa]
			Real64 const SteamOutletPress, // Pressure of steam at the outlet of the heater [Pa]
			Real64 const SteamInletQuality, // Quality of steam at the inlet of the heater [Pa]
			Real64 const SteamOutletQuality, // Quality of steam at the outlet of the heater [Pa]
			Real64 const FracRadiant, // User defined fraction for radiant heat addition
			Real64 const FracConvect, // Fraction for convective heat addition
			Real64 const FracDistribPerson, // Fraction for radiant heat incident on people
			Array1< Real64 > const & FracDistribToSurf,
			Real64 const TotPower, // Convective system impact rate that the heater actually meets [W]
			Real64 const Power, // Maximum heating rate [W]
			Real64 const ConvPower, // Convective heating rate [W]
			Real64 const RadPower, // Radiant heating rate [W]
			Real64 const TotEnergy, // Convective system impact energy [J]
			Real64 const Energy, // Maximum heating energy [J]
			Real64 const ConvEnergy, // Convective heating energy [J]
			Real64 const RadEnergy, // Radiant heating energy [J]
			int const LoopNum, // plant loop index
			int const LoopSideNum, // plant loop side index
			int const BranchNum, // plant loop branch index
			int const CompNum, // plant loop component index
			int const BBLoadReSimIndex,
			int const BBMassFlowReSimIndex,
			int const BBInletTempFlowReSimIndex,
			int const HeatingCapMethod,   // - Method for steam baseboard Radiator system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledHeatingCapacity   // -  steam baseboard Radiator system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		) :
			EquipID( EquipID ),
			EquipType( EquipType ),
			Schedule( Schedule ),
			SurfaceName( SurfaceName ),
			SurfacePtr( SurfacePtr ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			SteamInletNode( SteamInletNode ),
			SteamOutletNode( SteamOutletNode ),
			TotSurfToDistrib( TotSurfToDistrib ),
			FluidIndex( FluidIndex ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			DegOfSubcooling( DegOfSubcooling ),
			Offset( Offset ),
			SteamMassFlowRate( SteamMassFlowRate ),
			SteamMassFlowRateMax( SteamMassFlowRateMax ),
			SteamVolFlowRateMax( SteamVolFlowRateMax ),
			SteamOutletTemp( SteamOutletTemp ),
			SteamInletTemp( SteamInletTemp ),
			SteamInletEnthalpy( SteamInletEnthalpy ),
			SteamOutletEnthalpy( SteamOutletEnthalpy ),
			SteamInletPress( SteamInletPress ),
			SteamOutletPress( SteamOutletPress ),
			SteamInletQuality( SteamInletQuality ),
			SteamOutletQuality( SteamOutletQuality ),
			FracRadiant( FracRadiant ),
			FracConvect( FracConvect ),
			FracDistribPerson( FracDistribPerson ),
			FracDistribToSurf( FracDistribToSurf ),
			TotPower( TotPower ),
			Power( Power ),
			ConvPower( ConvPower ),
			RadPower( RadPower ),
			TotEnergy( TotEnergy ),
			Energy( Energy ),
			ConvEnergy( ConvEnergy ),
			RadEnergy( RadEnergy ),
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

	struct SteamBaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		SteamBaseboardNumericFieldData()
		{}

		// Member Constructor
		SteamBaseboardNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< SteamBaseboardParams > SteamBaseboard;
	extern Array1D< SteamBaseboardNumericFieldData > SteamBaseboardNumericFields;

	// Functions

	void
	SimSteamBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetSteamBaseboardInput();

	void
	InitSteamBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	SizeSteamBaseboard( int const BaseboardNum );

	void
	CalcSteamBaseboard(
		int & BaseboardNum,
		Real64 & LoadMet
	);

	void
	UpdateSteamBaseboard( int const BaseboardNum );

	void
	UpdateBBSteamRadSourceValAvg( bool & SteamBaseboardSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeBBSteamRadGains();

	void
	ReportSteamBaseboard( int const BaseboardNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	UpdateSteamBaseboardPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const FirstHVACIteration,
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // SteamBaseboardRadiator

} // EnergyPlus

#endif
