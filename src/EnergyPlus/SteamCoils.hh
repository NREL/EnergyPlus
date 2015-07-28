#ifndef SteamCoils_hh_INCLUDED
#define SteamCoils_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SteamCoils {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const SteamCoil_AirHeating;
	extern int const TemperatureSetPointControl;
	extern int const ZoneLoadControl;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE DEFINITIONS
	// MODULE VARIABLE DECLARATIONS:
	extern int SteamIndex;
	extern int NumSteamCoils; // The Number of SteamCoils found in the Input
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CoilWarningOnceFlag;
	extern Array1D_bool CheckEquipName;
	extern bool GetSteamCoilsInputFlag; // Flag set to make sure you get input once

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Types

	struct SteamCoilEquipConditions
	{
		// Members
		std::string Name; // Name of the SteamCoil
		std::string SteamCoilTypeA; // Type of SteamCoil ie. Heating or Cooling
		int SteamCoilType; // Type of SteamCoil ie. Heating or Cooling
		int SteamCoilModel; // Type of SteamCoil ie. Simple, Detailed, etc.
		int SteamCoilType_Num;
		std::string Schedule; // SteamCoil Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		Real64 InletAirMassFlowRate; // MassFlow through the SteamCoil being Simulated [kg/s]
		Real64 OutletAirMassFlowRate; // MassFlow throught the SteamCoil being Simulated[kg/s]
		Real64 InletAirTemp; // Inlet Air Temperature Operating Condition [C]
		Real64 OutletAirTemp; // Outlet Air Temperature Operating Condition [C]
		Real64 InletAirHumRat; // Inlet Air Humidity Ratio Operating Condition
		Real64 OutletAirHumRat; // Outlet Air Humidity Ratio Calculated Condition
		Real64 InletAirEnthalpy; // Inlet Air enthalpy [J/kg]
		Real64 OutletAirEnthalpy; // Outlet Air enthalpy [J/kg]
		Real64 TotSteamCoilLoad; // Total Load on the Coil [W]
		Real64 SenSteamCoilLoad; // Sensible Load on the Coil [W]
		Real64 TotSteamHeatingCoilEnergy; // Total Heating Coil energy of the Coil [J]
		Real64 TotSteamCoolingCoilEnergy; // Total Cooling Coil energy of the Coil [J]
		Real64 SenSteamCoolingCoilEnergy; // Sensible Cooling Coil energy of the Coil [J]
		Real64 TotSteamHeatingCoilRate; // Total Heating Coil Rate on the Coil [W]
		Real64 LoopLoss; // Loss in loop due to cond return to atm pressure
		Real64 TotSteamCoolingCoilRate; // Total Cooling Coil Rate on the Coil [W]
		Real64 SenSteamCoolingCoilRate; // Sensible Cooling Coil Rate on the Coil [W]
		Real64 LeavingRelHum; // Simple Coil Latent Model requires User input for leaving RH
		Real64 DesiredOutletTemp; // Temp desired at the outlet (C)
		Real64 DesiredOutletHumRat; // Humudity Ratio desired at outlet (C)
		Real64 InletSteamTemp; // Inlet Steam Temperature [C]
		Real64 OutletSteamTemp; // Outlet Steam Temperature [C]
		Real64 InletSteamMassFlowRate; // Inlet Steam Mass Flow Rate [Kg/s]
		Real64 OutletSteamMassFlowRate; // Outlet Steam Mass Flow Rate [Kg/s]
		Real64 MaxSteamVolFlowRate; // Maximum water Volume flow rate [m3/s]
		Real64 MaxSteamMassFlowRate; // Maximum water mass flow rate [Kg/s]
		Real64 InletSteamEnthalpy; // Inlet Water Enthalpy (J/Kg)
		Real64 OutletWaterEnthalpy; // Outlet Water Enthalpy (J/kg)
		Real64 InletSteamPress; // Pressure at steam inlet (Pa)
		Real64 InletSteamQuality; // Quality of steam at inlet
		Real64 OutletSteamQuality; // Quality of steam at outlet
		Real64 DegOfSubcooling;
		Real64 LoopSubcoolReturn;
		int AirInletNodeNum; // Inlet node number at air side
		int AirOutletNodeNum; // Outlet node number at air side
		int SteamInletNodeNum; // SteamInletNodeNum
		int SteamOutletNodeNum; // SteamOutletNodeNum
		int TempSetPointNodeNum; // If applicable : node number that the temp setpoint exists.
		int TypeOfCoil; // Control of Coil , temperature or Zone load
		int FluidIndex; // Fluid index for FluidProperties (Steam)
		int LoopNum; // index for plant loop with steam coil
		int LoopSide; // index for plant loop side for steam coil
		int BranchNum; // index for plant branch for steam coil
		int CompNum; // index for plant component for steam coil
		int Coil_PlantTypeNum; // plant level index for coil type
		Real64 OperatingCapacity; // capacity of steam coil at operating conditions (W)

		// Default Constructor
		SteamCoilEquipConditions() :
			SteamCoilType( 0 ),
			SteamCoilModel( 0 ),
			SteamCoilType_Num( 0 ),
			SchedPtr( 0 ),
			InletAirMassFlowRate( 0.0 ),
			OutletAirMassFlowRate( 0.0 ),
			InletAirTemp( 0.0 ),
			OutletAirTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			OutletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			TotSteamCoilLoad( 0.0 ),
			SenSteamCoilLoad( 0.0 ),
			TotSteamHeatingCoilEnergy( 0.0 ),
			TotSteamCoolingCoilEnergy( 0.0 ),
			SenSteamCoolingCoilEnergy( 0.0 ),
			TotSteamHeatingCoilRate( 0.0 ),
			LoopLoss( 0.0 ),
			TotSteamCoolingCoilRate( 0.0 ),
			SenSteamCoolingCoilRate( 0.0 ),
			LeavingRelHum( 0.0 ),
			DesiredOutletTemp( 0.0 ),
			DesiredOutletHumRat( 0.0 ),
			InletSteamTemp( 0.0 ),
			OutletSteamTemp( 0.0 ),
			InletSteamMassFlowRate( 0.0 ),
			OutletSteamMassFlowRate( 0.0 ),
			MaxSteamVolFlowRate( 0.0 ),
			MaxSteamMassFlowRate( 0.0 ),
			InletSteamEnthalpy( 0.0 ),
			OutletWaterEnthalpy( 0.0 ),
			InletSteamPress( 0.0 ),
			InletSteamQuality( 0.0 ),
			OutletSteamQuality( 0.0 ),
			DegOfSubcooling( 0.0 ),
			LoopSubcoolReturn( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			SteamInletNodeNum( 0 ),
			SteamOutletNodeNum( 0 ),
			TempSetPointNodeNum( 0 ),
			TypeOfCoil( 0 ),
			FluidIndex( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			Coil_PlantTypeNum( 0 ),
			OperatingCapacity( 0.0 )
		{}

		// Member Constructor
		SteamCoilEquipConditions(
			std::string const & Name, // Name of the SteamCoil
			std::string const & SteamCoilTypeA, // Type of SteamCoil ie. Heating or Cooling
			int const SteamCoilType, // Type of SteamCoil ie. Heating or Cooling
			int const SteamCoilModel, // Type of SteamCoil ie. Simple, Detailed, etc.
			int const SteamCoilType_Num,
			std::string const & Schedule, // SteamCoil Operation Schedule
			int const SchedPtr, // Pointer to the correct schedule
			Real64 const InletAirMassFlowRate, // MassFlow through the SteamCoil being Simulated [kg/s]
			Real64 const OutletAirMassFlowRate, // MassFlow throught the SteamCoil being Simulated[kg/s]
			Real64 const InletAirTemp, // Inlet Air Temperature Operating Condition [C]
			Real64 const OutletAirTemp, // Outlet Air Temperature Operating Condition [C]
			Real64 const InletAirHumRat, // Inlet Air Humidity Ratio Operating Condition
			Real64 const OutletAirHumRat, // Outlet Air Humidity Ratio Calculated Condition
			Real64 const InletAirEnthalpy, // Inlet Air enthalpy [J/kg]
			Real64 const OutletAirEnthalpy, // Outlet Air enthalpy [J/kg]
			Real64 const TotSteamCoilLoad, // Total Load on the Coil [W]
			Real64 const SenSteamCoilLoad, // Sensible Load on the Coil [W]
			Real64 const TotSteamHeatingCoilEnergy, // Total Heating Coil energy of the Coil [J]
			Real64 const TotSteamCoolingCoilEnergy, // Total Cooling Coil energy of the Coil [J]
			Real64 const SenSteamCoolingCoilEnergy, // Sensible Cooling Coil energy of the Coil [J]
			Real64 const TotSteamHeatingCoilRate, // Total Heating Coil Rate on the Coil [W]
			Real64 const LoopLoss, // Loss in loop due to cond return to atm pressure
			Real64 const TotSteamCoolingCoilRate, // Total Cooling Coil Rate on the Coil [W]
			Real64 const SenSteamCoolingCoilRate, // Sensible Cooling Coil Rate on the Coil [W]
			Real64 const LeavingRelHum, // Simple Coil Latent Model requires User input for leaving RH
			Real64 const DesiredOutletTemp, // Temp desired at the outlet (C)
			Real64 const DesiredOutletHumRat, // Humudity Ratio desired at outlet (C)
			Real64 const InletSteamTemp, // Inlet Steam Temperature [C]
			Real64 const OutletSteamTemp, // Outlet Steam Temperature [C]
			Real64 const InletSteamMassFlowRate, // Inlet Steam Mass Flow Rate [Kg/s]
			Real64 const OutletSteamMassFlowRate, // Outlet Steam Mass Flow Rate [Kg/s]
			Real64 const MaxSteamVolFlowRate, // Maximum water Volume flow rate [m3/s]
			Real64 const MaxSteamMassFlowRate, // Maximum water mass flow rate [Kg/s]
			Real64 const InletSteamEnthalpy, // Inlet Water Enthalpy (J/Kg)
			Real64 const OutletWaterEnthalpy, // Outlet Water Enthalpy (J/kg)
			Real64 const InletSteamPress, // Pressure at steam inlet (Pa)
			Real64 const InletSteamQuality, // Quality of steam at inlet
			Real64 const OutletSteamQuality, // Quality of steam at outlet
			Real64 const DegOfSubcooling,
			Real64 const LoopSubcoolReturn,
			int const AirInletNodeNum, // Inlet node number at air side
			int const AirOutletNodeNum, // Outlet node number at air side
			int const SteamInletNodeNum, // SteamInletNodeNum
			int const SteamOutletNodeNum, // SteamOutletNodeNum
			int const TempSetPointNodeNum, // If applicable : node number that the temp setpoint exists.
			int const TypeOfCoil, // Control of Coil , temperature or Zone load
			int const FluidIndex, // Fluid index for FluidProperties (Steam)
			int const LoopNum, // index for plant loop with steam coil
			int const LoopSide, // index for plant loop side for steam coil
			int const BranchNum, // index for plant branch for steam coil
			int const CompNum, // index for plant component for steam coil
			int const Coil_PlantTypeNum, // plant level index for coil type
			Real64 const OperatingCapacity // capacity of steam coil at operating conditions (W)
		) :
			Name( Name ),
			SteamCoilTypeA( SteamCoilTypeA ),
			SteamCoilType( SteamCoilType ),
			SteamCoilModel( SteamCoilModel ),
			SteamCoilType_Num( SteamCoilType_Num ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			InletAirMassFlowRate( InletAirMassFlowRate ),
			OutletAirMassFlowRate( OutletAirMassFlowRate ),
			InletAirTemp( InletAirTemp ),
			OutletAirTemp( OutletAirTemp ),
			InletAirHumRat( InletAirHumRat ),
			OutletAirHumRat( OutletAirHumRat ),
			InletAirEnthalpy( InletAirEnthalpy ),
			OutletAirEnthalpy( OutletAirEnthalpy ),
			TotSteamCoilLoad( TotSteamCoilLoad ),
			SenSteamCoilLoad( SenSteamCoilLoad ),
			TotSteamHeatingCoilEnergy( TotSteamHeatingCoilEnergy ),
			TotSteamCoolingCoilEnergy( TotSteamCoolingCoilEnergy ),
			SenSteamCoolingCoilEnergy( SenSteamCoolingCoilEnergy ),
			TotSteamHeatingCoilRate( TotSteamHeatingCoilRate ),
			LoopLoss( LoopLoss ),
			TotSteamCoolingCoilRate( TotSteamCoolingCoilRate ),
			SenSteamCoolingCoilRate( SenSteamCoolingCoilRate ),
			LeavingRelHum( LeavingRelHum ),
			DesiredOutletTemp( DesiredOutletTemp ),
			DesiredOutletHumRat( DesiredOutletHumRat ),
			InletSteamTemp( InletSteamTemp ),
			OutletSteamTemp( OutletSteamTemp ),
			InletSteamMassFlowRate( InletSteamMassFlowRate ),
			OutletSteamMassFlowRate( OutletSteamMassFlowRate ),
			MaxSteamVolFlowRate( MaxSteamVolFlowRate ),
			MaxSteamMassFlowRate( MaxSteamMassFlowRate ),
			InletSteamEnthalpy( InletSteamEnthalpy ),
			OutletWaterEnthalpy( OutletWaterEnthalpy ),
			InletSteamPress( InletSteamPress ),
			InletSteamQuality( InletSteamQuality ),
			OutletSteamQuality( OutletSteamQuality ),
			DegOfSubcooling( DegOfSubcooling ),
			LoopSubcoolReturn( LoopSubcoolReturn ),
			AirInletNodeNum( AirInletNodeNum ),
			AirOutletNodeNum( AirOutletNodeNum ),
			SteamInletNodeNum( SteamInletNodeNum ),
			SteamOutletNodeNum( SteamOutletNodeNum ),
			TempSetPointNodeNum( TempSetPointNodeNum ),
			TypeOfCoil( TypeOfCoil ),
			FluidIndex( FluidIndex ),
			LoopNum( LoopNum ),
			LoopSide( LoopSide ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			Coil_PlantTypeNum( Coil_PlantTypeNum ),
			OperatingCapacity( OperatingCapacity )
		{}

	};

	// Object Data
	extern Array1D< SteamCoilEquipConditions > SteamCoil;

	// Functions

	void
	SimulateSteamCoilComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex,
		Optional< Real64 const > QCoilReq = _, // coil load to be met
		Optional< Real64 > QCoilActual = _, // coil load actually delivered returned to calling component
		Optional_int_const FanOpMode = _,
		Optional< Real64 const > PartLoadRatio = _
	);

	// Get Input Section of the Module

	void
	GetSteamCoilInput();

	// End of Get Input subroutines for the HB Module

	// Beginning Initialization Section of the Module

	void
	InitSteamCoil(
		int const CoilNum,
		bool const FirstHVACIteration
	);

	void
	SizeSteamCoil( int const CoilNum );

	// End Initialization Section of the Module

	// Begin Algorithm Section of the Module

	void
	CalcSteamAirCoil(
		int const CoilNum,
		Real64 const QCoilRequested, // requested coil load
		Real64 & QCoilActual, // coil load actually delivered
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	);

	// Beginning of Update subroutines for the SteamCoil Module

	void
	UpdateSteamCoil( int const CoilNum );

	// End of Update subroutines for the SteamCoil Module

	// Beginning of Reporting subroutines for the SteamCoil Module

	void
	ReportSteamCoil( int const CoilNum );

	// End of Reporting subroutines for the SteamCoil Module

	// Utility subroutines for the SteamCoil Module

	int
	GetSteamCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	void
	CheckSteamCoilSchedule(
		std::string const & CompType,
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	);

	Real64
	GetCoilMaxWaterFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilMaxSteamFlowRate(
		int const CoilIndex, // must match coil types in this module
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAirInletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAirOutletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAirOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamInletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamOutletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetTypeOfCoil(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSteamCoilControlNodeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorFlag // set to true if problem
	);

	int
	GetSteamCoilAvailScheduleIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	// End of Utility subroutines for the SteamCoil Module

	// *****************************************************************************
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

} // SteamCoils

} // EnergyPlus

#endif
