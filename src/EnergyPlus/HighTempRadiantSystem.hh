#ifndef HighTempRadiantSystem_hh_INCLUDED
#define HighTempRadiantSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HighTempRadiantSystem {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern std::string const cGas;
	extern std::string const cNaturalGas;
	extern std::string const cElectric;
	extern std::string const cElectricity;
	extern int const Gas;
	extern int const Electric;
	extern std::string const cMATControl; // Control for using mean air temperature
	extern std::string const cMRTControl; // Control for using mean radiant temperature
	extern std::string const cOperativeControl; // Control for using operative temperature
	extern std::string const cMATSPControl; // Control for to MAT setpoint
	extern std::string const cMRTSPControl; // Control for to MRT setpoint
	extern std::string const cOperativeSPControl; // Control for operative temperature setpoint
	extern int const MATControl;
	extern int const MRTControl;
	extern int const OperativeControl;
	extern int const MATSPControl;
	extern int const MRTSPControl;
	extern int const OperativeSPControl;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	extern int NumOfHighTempRadSys; // Number of hydronic low tempererature radiant systems
	extern Array1D< Real64 > QHTRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QHTRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QHTRadSrcAvg locally
	extern Array1D< Real64 > LastQHTRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HighTempRadiantSystem

	// Types

	struct HighTempRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		int HeaterType; // Type of heater (gas or electric)
		Real64 MaxPowerCapac; // Maximum capacity of the radiant heater in Watts
		Real64 CombustionEffic; // Combustion efficiency (only valid for a gas heater)
		Real64 FracRadiant; // Fraction of heater power that is given off as radiant heat
		Real64 FracLatent; // Fraction of heater power that is given off as latent heat
		Real64 FracLost; // Fraction of heater power that is lost to the outside environment
		Real64 FracConvect; // Fraction of heater power that is given off as convective heat
		// (by definition this is 1 minus the sum of all other fractions)
		int ControlType; // Control type for the system (MAT, MRT, or op temp)
		Real64 ThrottlRange; // Throttling range for heating [C]
		std::string SetptSched; // Schedule name for the zone setpoint temperature
		int SetptSchedPtr; // Schedule index for the zone setpoint temperature
		Real64 FracDistribPerson; // Fraction of fraction radiant incident on a "person" in the space
		int TotSurfToDistrib; // Total number of surfaces the heater sends radiation to
		Array1D_string SurfaceName; // Surface name in the list of surfaces heater sends radiation to
		Array1D_int SurfacePtr; // Surface number in the list of surfaces heater sends radiation to
		Array1D< Real64 > FracDistribToSurf; // Fraction of fraction radiant incident on the surface
		// Other parameters
		// Report data
		Real64 ElecPower; // system electric consumption in Watts
		Real64 ElecEnergy; // system electric consumption in Joules
		Real64 GasPower; // system gas consumption in Watts
		Real64 GasEnergy; // system gas consumption in Joules
		Real64 HeatPower; // actual heating sent to zone (convective and radiative) in Watts
		Real64 HeatEnergy; // actual heating sent to zone (convective and radiative) in Joules
		int HeatingCapMethod; // - Method for High Temperature Radiant heating capacity scalable sizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // - High Temperature Radiant scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}

		// Default Constructor
		HighTempRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			HeaterType( 0 ),
			MaxPowerCapac( 0.0 ),
			CombustionEffic( 0.0 ),
			FracRadiant( 0.0 ),
			FracLatent( 0.0 ),
			FracLost( 0.0 ),
			FracConvect( 0.0 ),
			ControlType( 0 ),
			ThrottlRange( 0.0 ),
			SetptSchedPtr( 0 ),
			FracDistribPerson( 0.0 ),
			TotSurfToDistrib( 0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			GasPower( 0.0 ),
			GasEnergy( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		HighTempRadiantSystemData(
			std::string const & Name, // name of hydronic radiant system
			std::string const & SchedName, // availability schedule
			int const SchedPtr, // index to schedule
			std::string const & ZoneName, // Name of zone the system is serving
			int const ZonePtr, // Point to this zone in the Zone derived type
			int const HeaterType, // Type of heater (gas or electric)
			Real64 const MaxPowerCapac, // Maximum capacity of the radiant heater in Watts
			Real64 const CombustionEffic, // Combustion efficiency (only valid for a gas heater)
			Real64 const FracRadiant, // Fraction of heater power that is given off as radiant heat
			Real64 const FracLatent, // Fraction of heater power that is given off as latent heat
			Real64 const FracLost, // Fraction of heater power that is lost to the outside environment
			Real64 const FracConvect, // Fraction of heater power that is given off as convective heat
			int const ControlType, // Control type for the system (MAT, MRT, or op temp)
			Real64 const ThrottlRange, // Throttling range for heating [C]
			std::string const & SetptSched, // Schedule name for the zone setpoint temperature
			int const SetptSchedPtr, // Schedule index for the zone setpoint temperature
			Real64 const FracDistribPerson, // Fraction of fraction radiant incident on a "person" in the space
			int const TotSurfToDistrib, // Total number of surfaces the heater sends radiation to
			Array1_string const & SurfaceName, // Surface name in the list of surfaces heater sends radiation to
			Array1_int const & SurfacePtr, // Surface number in the list of surfaces heater sends radiation to
			Array1< Real64 > const & FracDistribToSurf, // Fraction of fraction radiant incident on the surface
			Real64 const ElecPower, // system electric consumption in Watts
			Real64 const ElecEnergy, // system electric consumption in Joules
			Real64 const GasPower, // system gas consumption in Watts
			Real64 const GasEnergy, // system gas consumption in Joules
			Real64 const HeatPower, // actual heating sent to zone (convective and radiative) in Watts
			Real64 const HeatEnergy, // actual heating sent to zone (convective and radiative) in Joules
			int const HeatingCapMethod, // - Method for High Temperature Radiant heating capacity scalable sizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledHeatingCapacity // - High Temperature Radiant scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}
		) :
			Name( Name ),
			SchedName( SchedName ),
			SchedPtr( SchedPtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			HeaterType( HeaterType ),
			MaxPowerCapac( MaxPowerCapac ),
			CombustionEffic( CombustionEffic ),
			FracRadiant( FracRadiant ),
			FracLatent( FracLatent ),
			FracLost( FracLost ),
			FracConvect( FracConvect ),
			ControlType( ControlType ),
			ThrottlRange( ThrottlRange ),
			SetptSched( SetptSched ),
			SetptSchedPtr( SetptSchedPtr ),
			FracDistribPerson( FracDistribPerson ),
			TotSurfToDistrib( TotSurfToDistrib ),
			SurfaceName( SurfaceName ),
			SurfacePtr( SurfacePtr ),
			FracDistribToSurf( FracDistribToSurf ),
			ElecPower( ElecPower ),
			ElecEnergy( ElecEnergy ),
			GasPower( GasPower ),
			GasEnergy( GasEnergy ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity )
		{}
	};

	struct HighTempRadSysNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HighTempRadSysNumericFieldData()
		{}

		// Member Constructor
		HighTempRadSysNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< HighTempRadiantSystemData > HighTempRadSys;
	extern Array1D< HighTempRadSysNumericFieldData > HighTempRadSysNumericFields;

	// Functions

	void
	SimHighTempRadiantSystem(
		std::string const & CompName, // name of the low temperature radiant system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & LoadMet, // load met by the radiant system, in Watts
		int & CompIndex
	);

	void
	GetHighTempRadiantSystem();

	void
	InitHighTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum // Index for the low temperature radiant system under consideration within the derived types
	);

	void
	SizeHighTempRadiantSystem( int const RadSysNum );

	void
	CalcHighTempRadiantSystem( int const RadSysNum ); // name of the low temperature radiant system

	void
	CalcHighTempRadiantSystemSP(
		bool const FirstHVACIteration, // true if this is the first HVAC iteration at this system time step !unused1208
		int const RadSysNum // name of the low temperature radiant system
	);

	void
	UpdateHighTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	UpdateHTRadSourceValAvg( bool & HighTempRadSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeHTRadGains();

	void
	ReportHighTempRadiantSystem( int const RadSysNum ); // Index for the low temperature radiant system under consideration within the derived types

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

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

} // HighTempRadiantSystem

} // EnergyPlus

#endif
