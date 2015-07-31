#ifndef ZoneDehumidifier_hh_INCLUDED
#define ZoneDehumidifier_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ZoneDehumidifier {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// Unit type index
	extern int const ZoneDehumidUnit; // 1 is the index for ZoneHVAC:Dehumidifier:DX

	// Water Systems
	extern int const CondensateDiscarded; // Default mode where water is "lost"
	extern int const CondensateToTank; // Collect coil condensate from air and store in water storage tank

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumDehumidifiers; // Number of zone dehumidifier objects in the input file

	extern bool GetInputFlag; // Set to FALSE after first time input is "gotten"
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms/Calculation routines for the module

	// Update routine to update node information

	// Reporting routines for module

	// Get either inlet or outlet node number

	// Types

	struct ZoneDehumidifierData
	{
		// Members
		// input data and others required during calculations
		std::string Name; // Name of unit
		std::string UnitType; // Type of unit
		int UnitType_Num; // Parameter equivalent to type of unit
		int SchedPtr; // Index number to availability schedule
		Real64 RatedWaterRemoval; // Rated water removal [liters/day]
		Real64 RatedEnergyFactor; // Rated energy factor [liters/kWh]
		Real64 RatedAirVolFlow; // Rated air flow rate through the dehumidifier [m3/s]
		Real64 RatedAirMassFlow; // Rated air mass flow rate through the dehumidifier [kg/s]
		Real64 MinInletAirTemp; // Minimum dry-bulb temperature for dehumidifier operation [C]
		Real64 MaxInletAirTemp; // Maximum dry-bulb temperature for dehumidifier operation [C]
		Real64 InletAirMassFlow; // Inlet air mass flow rate for the time step being simulated [kg/s]
		Real64 OutletAirEnthalpy; // Dehumidifier outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // Dehumidifier outlet air humidity ratio [kg/kg]
		Real64 OffCycleParasiticLoad; // Off Cycle Parasitic Load, user input [W]
		int AirInletNodeNum; // Inlet air node number
		int AirOutletNodeNum; // Outlet air node number
		int WaterRemovalCurveIndex; // Index for water removal curve
		int WaterRemovalCurveType; // Water removal curve type. 2 = biquadratic
		int WaterRemovalCurveErrorCount; // Count number of times water removal curve returns a negative value
		int WaterRemovalCurveErrorIndex; // Index for negative value water removal factor recurring messages
		int EnergyFactorCurveIndex; // Index for energy factor curve
		int EnergyFactorCurveType; // Energy factor curve type. 2 = biquadratic
		int EnergyFactorCurveErrorCount; // Count number of times energy factor curve returns negative value
		int EnergyFactorCurveErrorIndex; // Index for negative value energy factor recurring messages
		int PartLoadCurveIndex; // Index for part load curve
		int PartLoadCurveType; // Part load curve type. 1 = quadratic, cubic = 3
		int LowPLFErrorCount; // Count number of times PLF < 0.7
		int LowPLFErrorIndex; // Index for PLF < 0.7 recurring warning messages
		int HighPLFErrorCount; // Count number of times PLF > 1.0
		int HighPLFErrorIndex; // Index for PLF > 1.0 recurring warning messages
		int HighRTFErrorCount; // Count number of times RTF > 1.0
		int HighRTFErrorIndex; // Index for RTF > 1.0 recurring warning messages
		int PLFPLRErrorCount; // Count number of times PLF < PLR
		int PLFPLRErrorIndex; // Index for PLF < PLR recurring warning messages
		int CondensateCollectMode; // Where does water come from
		std::string CondensateCollectName; // Name of water storage (collection) tank
		int CondensateTankID; // Condensate collection tank ID number
		int CondensateTankSupplyARRID; // Condensate collection tank supply ID number
		// Report data
		Real64 SensHeatingRate; // Zone Dehumidifier Sensible Heating Rate [W]
		Real64 SensHeatingEnergy; // Zone Dehumidifier Sensible Heating Energy [J]
		Real64 WaterRemovalRate; // Zone Dehumidifier Water Removal Rate [kg/s]
		Real64 WaterRemoved; // Zone Dehumidifier Water Removed [kg]
		Real64 ElecPower; // Zone Dehumidifier Electric Power [W]
		Real64 ElecConsumption; // Zone Dehumidifier Electric Consumption [J]
		Real64 DehumidPLR; // Zone Dehumidifier Part-Load Ratio [-]
		Real64 DehumidRTF; // Zone Dehumidifier Runtime Fraction [-]
		Real64 DehumidCondVolFlowRate; // Zone Dehumidifier Condensate Volumetric Flow Rate [m3/s]
		Real64 DehumidCondVol; // Zone Dehumidifier Condensate Volume [m3]
		Real64 OutletAirTemp; // Zone Dehumidifier Outlet Air Temperature [C]
		Real64 OffCycleParasiticElecPower; // Zone Dehumidifier Off-Cycle Parasitic Electric Power [W]
		Real64 OffCycleParasiticElecCons; // Zone Dehumidifier Off-Cycle Parasitic Electric Consumption [J]

		// Default Constructor
		ZoneDehumidifierData() :
			UnitType_Num( 0 ),
			SchedPtr( 0 ),
			RatedWaterRemoval( 0.0 ),
			RatedEnergyFactor( 0.0 ),
			RatedAirVolFlow( 0.0 ),
			RatedAirMassFlow( 0.0 ),
			MinInletAirTemp( 0.0 ),
			MaxInletAirTemp( 0.0 ),
			InletAirMassFlow( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			OutletAirHumRat( 0.0 ),
			OffCycleParasiticLoad( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			WaterRemovalCurveIndex( 0 ),
			WaterRemovalCurveType( 0 ),
			WaterRemovalCurveErrorCount( 0 ),
			WaterRemovalCurveErrorIndex( 0 ),
			EnergyFactorCurveIndex( 0 ),
			EnergyFactorCurveType( 0 ),
			EnergyFactorCurveErrorCount( 0 ),
			EnergyFactorCurveErrorIndex( 0 ),
			PartLoadCurveIndex( 0 ),
			PartLoadCurveType( 0 ),
			LowPLFErrorCount( 0 ),
			LowPLFErrorIndex( 0 ),
			HighPLFErrorCount( 0 ),
			HighPLFErrorIndex( 0 ),
			HighRTFErrorCount( 0 ),
			HighRTFErrorIndex( 0 ),
			PLFPLRErrorCount( 0 ),
			PLFPLRErrorIndex( 0 ),
			CondensateCollectMode( CondensateDiscarded ),
			CondensateTankID( 0 ),
			CondensateTankSupplyARRID( 0 ),
			SensHeatingRate( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			WaterRemovalRate( 0.0 ),
			WaterRemoved( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsumption( 0.0 ),
			DehumidPLR( 0.0 ),
			DehumidRTF( 0.0 ),
			DehumidCondVolFlowRate( 0.0 ),
			DehumidCondVol( 0.0 ),
			OutletAirTemp( 0.0 ),
			OffCycleParasiticElecPower( 0.0 ),
			OffCycleParasiticElecCons( 0.0 )
		{}

		// Member Constructor
		ZoneDehumidifierData(
			std::string const & Name, // Name of unit
			std::string const & UnitType, // Type of unit
			int const UnitType_Num, // Parameter equivalent to type of unit
			int const SchedPtr, // Index number to availability schedule
			Real64 const RatedWaterRemoval, // Rated water removal [liters/day]
			Real64 const RatedEnergyFactor, // Rated energy factor [liters/kWh]
			Real64 const RatedAirVolFlow, // Rated air flow rate through the dehumidifier [m3/s]
			Real64 const RatedAirMassFlow, // Rated air mass flow rate through the dehumidifier [kg/s]
			Real64 const MinInletAirTemp, // Minimum dry-bulb temperature for dehumidifier operation [C]
			Real64 const MaxInletAirTemp, // Maximum dry-bulb temperature for dehumidifier operation [C]
			Real64 const InletAirMassFlow, // Inlet air mass flow rate for the time step being simulated [kg/s]
			Real64 const OutletAirEnthalpy, // Dehumidifier outlet air enthalpy [J/kg]
			Real64 const OutletAirHumRat, // Dehumidifier outlet air humidity ratio [kg/kg]
			Real64 const OffCycleParasiticLoad, // Off Cycle Parasitic Load, user input [W]
			int const AirInletNodeNum, // Inlet air node number
			int const AirOutletNodeNum, // Outlet air node number
			int const WaterRemovalCurveIndex, // Index for water removal curve
			int const WaterRemovalCurveType, // Water removal curve type. 2 = biquadratic
			int const WaterRemovalCurveErrorCount, // Count number of times water removal curve returns a negative value
			int const WaterRemovalCurveErrorIndex, // Index for negative value water removal factor recurring messages
			int const EnergyFactorCurveIndex, // Index for energy factor curve
			int const EnergyFactorCurveType, // Energy factor curve type. 2 = biquadratic
			int const EnergyFactorCurveErrorCount, // Count number of times energy factor curve returns negative value
			int const EnergyFactorCurveErrorIndex, // Index for negative value energy factor recurring messages
			int const PartLoadCurveIndex, // Index for part load curve
			int const PartLoadCurveType, // Part load curve type. 1 = quadratic, cubic = 3
			int const LowPLFErrorCount, // Count number of times PLF < 0.7
			int const LowPLFErrorIndex, // Index for PLF < 0.7 recurring warning messages
			int const HighPLFErrorCount, // Count number of times PLF > 1.0
			int const HighPLFErrorIndex, // Index for PLF > 1.0 recurring warning messages
			int const HighRTFErrorCount, // Count number of times RTF > 1.0
			int const HighRTFErrorIndex, // Index for RTF > 1.0 recurring warning messages
			int const PLFPLRErrorCount, // Count number of times PLF < PLR
			int const PLFPLRErrorIndex, // Index for PLF < PLR recurring warning messages
			int const CondensateCollectMode, // Where does water come from
			std::string const & CondensateCollectName, // Name of water storage (collection) tank
			int const CondensateTankID, // Condensate collection tank ID number
			int const CondensateTankSupplyARRID, // Condensate collection tank supply ID number
			Real64 const SensHeatingRate, // Zone Dehumidifier Sensible Heating Rate [W]
			Real64 const SensHeatingEnergy, // Zone Dehumidifier Sensible Heating Energy [J]
			Real64 const WaterRemovalRate, // Zone Dehumidifier Water Removal Rate [kg/s]
			Real64 const WaterRemoved, // Zone Dehumidifier Water Removed [kg]
			Real64 const ElecPower, // Zone Dehumidifier Electric Power [W]
			Real64 const ElecConsumption, // Zone Dehumidifier Electric Consumption [J]
			Real64 const DehumidPLR, // Zone Dehumidifier Part-Load Ratio [-]
			Real64 const DehumidRTF, // Zone Dehumidifier Runtime Fraction [-]
			Real64 const DehumidCondVolFlowRate, // Zone Dehumidifier Condensate Volumetric Flow Rate [m3/s]
			Real64 const DehumidCondVol, // Zone Dehumidifier Condensate Volume [m3]
			Real64 const OutletAirTemp, // Zone Dehumidifier Outlet Air Temperature [C]
			Real64 const OffCycleParasiticElecPower, // Zone Dehumidifier Off-Cycle Parasitic Electric Power [W]
			Real64 const OffCycleParasiticElecCons // Zone Dehumidifier Off-Cycle Parasitic Electric Consumption [J]
		) :
			Name( Name ),
			UnitType( UnitType ),
			UnitType_Num( UnitType_Num ),
			SchedPtr( SchedPtr ),
			RatedWaterRemoval( RatedWaterRemoval ),
			RatedEnergyFactor( RatedEnergyFactor ),
			RatedAirVolFlow( RatedAirVolFlow ),
			RatedAirMassFlow( RatedAirMassFlow ),
			MinInletAirTemp( MinInletAirTemp ),
			MaxInletAirTemp( MaxInletAirTemp ),
			InletAirMassFlow( InletAirMassFlow ),
			OutletAirEnthalpy( OutletAirEnthalpy ),
			OutletAirHumRat( OutletAirHumRat ),
			OffCycleParasiticLoad( OffCycleParasiticLoad ),
			AirInletNodeNum( AirInletNodeNum ),
			AirOutletNodeNum( AirOutletNodeNum ),
			WaterRemovalCurveIndex( WaterRemovalCurveIndex ),
			WaterRemovalCurveType( WaterRemovalCurveType ),
			WaterRemovalCurveErrorCount( WaterRemovalCurveErrorCount ),
			WaterRemovalCurveErrorIndex( WaterRemovalCurveErrorIndex ),
			EnergyFactorCurveIndex( EnergyFactorCurveIndex ),
			EnergyFactorCurveType( EnergyFactorCurveType ),
			EnergyFactorCurveErrorCount( EnergyFactorCurveErrorCount ),
			EnergyFactorCurveErrorIndex( EnergyFactorCurveErrorIndex ),
			PartLoadCurveIndex( PartLoadCurveIndex ),
			PartLoadCurveType( PartLoadCurveType ),
			LowPLFErrorCount( LowPLFErrorCount ),
			LowPLFErrorIndex( LowPLFErrorIndex ),
			HighPLFErrorCount( HighPLFErrorCount ),
			HighPLFErrorIndex( HighPLFErrorIndex ),
			HighRTFErrorCount( HighRTFErrorCount ),
			HighRTFErrorIndex( HighRTFErrorIndex ),
			PLFPLRErrorCount( PLFPLRErrorCount ),
			PLFPLRErrorIndex( PLFPLRErrorIndex ),
			CondensateCollectMode( CondensateCollectMode ),
			CondensateCollectName( CondensateCollectName ),
			CondensateTankID( CondensateTankID ),
			CondensateTankSupplyARRID( CondensateTankSupplyARRID ),
			SensHeatingRate( SensHeatingRate ),
			SensHeatingEnergy( SensHeatingEnergy ),
			WaterRemovalRate( WaterRemovalRate ),
			WaterRemoved( WaterRemoved ),
			ElecPower( ElecPower ),
			ElecConsumption( ElecConsumption ),
			DehumidPLR( DehumidPLR ),
			DehumidRTF( DehumidRTF ),
			DehumidCondVolFlowRate( DehumidCondVolFlowRate ),
			DehumidCondVol( DehumidCondVol ),
			OutletAirTemp( OutletAirTemp ),
			OffCycleParasiticElecPower( OffCycleParasiticElecPower ),
			OffCycleParasiticElecCons( OffCycleParasiticElecCons )
		{}

	};

	// Object Data
	extern Array1D< ZoneDehumidifierData > ZoneDehumid;

	// Functions

	void
	SimZoneDehumidifier(
		std::string const & CompName, // Name of the zone dehumidifier
		int const ZoneNum, // Number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QSensOut, // Sensible capacity delivered to zone (W)
		Real64 & QLatOut, // Latent capacity delivered to zone (kg/s), dehumidify = negative
		int & CompIndex // Index to the zone dehumidifier
	);

	void
	GetZoneDehumidifierInput();

	void
	InitZoneDehumidifier( int const ZoneDehumNum ); // Number of the current zone dehumidifier being simulated

	void
	SizeZoneDehumidifier();

	void
	CalcZoneDehumidifier(
		int const ZoneDehumNum, // Index number of the current zone dehumidifier being simulated
		Real64 const QZnDehumidReq, // Dehumidification load to be met (kg/s), negative value means dehumidification load
		Real64 & SensibleOutput, // Sensible (heating) output (W), sent to load predictor for next simulation time step
		Real64 & LatentOutput // Latent (dehumidification) output provided (kg/s)
	);

	void
	UpdateZoneDehumidifier( int const ZoneDehumNum ); // Number of the current zone dehumidifier being simulated

	void
	ReportZoneDehumidifier( int const DehumidNum ); // Index of the current zone dehumidifier being simulated

	bool
	GetZoneDehumidifierNodeNumber( int const NodeNumber ); // Node being tested

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

} // ZoneDehumidifier

} // EnergyPlus

#endif
