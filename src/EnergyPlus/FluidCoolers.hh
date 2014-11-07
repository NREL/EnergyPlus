#ifndef FluidCoolers_hh_INCLUDED
#define FluidCoolers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace FluidCoolers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern std::string const cFluidCooler_SingleSpeed;
	extern std::string const cFluidCooler_TwoSpeed;

	extern int const PIM_NominalCapacity;
	extern int const PIM_UFactor;

	extern int const FluidCooler_SingleSpeed;
	extern int const FluidCooler_TwoSpeed;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumSimpleFluidCoolers; // Number of similar fluid coolers

	// The following block of variables are used to carry model results for a fluid cooler instance
	// across sim, update, and report routines.  Simulation manager must be careful
	// in models with multiple fluid coolers.

	extern Real64 InletWaterTemp; // CW temperature at fluid cooler inlet
	extern Real64 OutletWaterTemp; // CW temperature at fluid cooler outlet
	extern int WaterInletNode; // Node number at fluid cooler inlet
	extern int WaterOutletNode; // Node number at fluid cooler outlet
	extern Real64 WaterMassFlowRate; // WaterMassFlowRate through fluid cooler
	//DSU this is plant level stuff now  :: FluidCoolerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
	//DSU this is plant level stuff now  :: FluidCoolerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
	//DSU this is plant level stuff now  :: LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
	//DSU this is plant level stuff now  :: LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
	extern Real64 Qactual; // Fluid cooler heat transfer
	extern Real64 FanPower; // Fluid cooler fan power used

	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopFluidCoolers

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module
	// also, calculates UA based on nominal capacity input(s)

	// Update routines to check convergence and update nodes

	// Types

	struct FluidCoolerspecs
	{
		// Members
		std::string Name; // User identifier
		std::string FluidCoolerType; // Type of fluid cooler
		int FluidCoolerType_Num;
		int PerformanceInputMethod_Num;
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // Simulate the machine at it's operating part load ratio
		Real64 DesignWaterFlowRate; // Design water flow rate through the fluid cooler [m3/s]
		Real64 DesWaterMassFlowRate; // Design water flow rate through the fluid cooler [kg/s]
		Real64 HighSpeedAirFlowRate; // Air flow rate through the fluid cooler at high speed [m3/s]
		Real64 HighSpeedFanPower; // Fan power at high fan speed [W]
		Real64 HighSpeedFluidCoolerUA; // UA of fluid cooler at high fan speed [W/C]
		Real64 LowSpeedAirFlowRate; // Air flow rate through fluid cooler at low speed [m3/s]
		Real64 LowSpeedAirFlowRateSizingFactor; // sizing factor for low speed air flow rate []
		Real64 LowSpeedFanPower; // Fan power at low fan speed [W]
		Real64 LowSpeedFanPowerSizingFactor; // sizing factor for low speed fan power []
		Real64 LowSpeedFluidCoolerUA; // UA of fluid cooler at low fan speed [W/C]
		Real64 LowSpeedFluidCoolerUASizingFactor; // sizing factor for low speed UA []
		Real64 DesignEnteringWaterTemp; // Entering water temperature at design conditions
		Real64 DesignLeavingWaterTemp; // Entering water temperature at design conditions
		Real64 DesignEnteringAirTemp; // Entering water temperature at design conditions
		Real64 DesignEnteringAirWetBulbTemp; // Entering water temperature at design condition
		Real64 FluidCoolerMassFlowRateMultiplier; // Maximum fluid cooler flow rate is this multiplier * design flow rate
		Real64 FluidCoolerNominalCapacity; // Nominal capacity of the fluid cooler [W] at high speed
		Real64 FluidCoolerLowSpeedNomCap; // Nominal capacity of the fluid cooler [W] at low speed
		Real64 FluidCoolerLowSpeedNomCapSizingFactor; // sizing factor for low speed capacity []
		int WaterInletNodeNum; // Node number on the water inlet side of the fluid cooler
		int WaterOutletNodeNum; // Node number on the water outlet side of the fluid cooler
		int OutdoorAirInletNodeNum; // Node number of outdoor air inlet for the fluid cooler
		int HighMassFlowErrorCount; // Counter when mass flow rate is > Design*FluidCoolerMassFlowRateMultiplier
		int HighMassFlowErrorIndex; // Index for high mass flow recurring error message
		int OutletWaterTempErrorCount; // Counter when outlet water temperature is < minimum allowed temperature
		int OutletWaterTempErrorIndex; // Index for outlet water temperature recurring error message
		int SmallWaterMassFlowErrorCount; // Counter when water mass flow rate is very small
		int SmallWaterMassFlowErrorIndex; // Index for very small water mass flow rate recurring error message
		int WMFRLessThanMinAvailErrCount; // Counter when water mass flow rate is less than minimum available
		int WMFRLessThanMinAvailErrIndex; // Index for water mass flow rate less than minavail recurring message
		int WMFRGreaterThanMaxAvailErrCount; // Counter when water mass flow rate is greater than minimum available
		int WMFRGreaterThanMaxAvailErrIndex; // Index for water mass flow rate > minavail recurring message
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		FluidCoolerspecs() :
			FluidCoolerType_Num( 0 ),
			PerformanceInputMethod_Num( 0 ),
			Available( true ),
			ON( true ),
			DesignWaterFlowRate( 0.0 ),
			DesWaterMassFlowRate( 0.0 ),
			HighSpeedAirFlowRate( 0.0 ),
			HighSpeedFanPower( 0.0 ),
			HighSpeedFluidCoolerUA( 0.0 ),
			LowSpeedAirFlowRate( 0.0 ),
			LowSpeedAirFlowRateSizingFactor( 0.0 ),
			LowSpeedFanPower( 0.0 ),
			LowSpeedFanPowerSizingFactor( 0.0 ),
			LowSpeedFluidCoolerUA( 0.0 ),
			LowSpeedFluidCoolerUASizingFactor( 0.0 ),
			DesignEnteringWaterTemp( 0.0 ),
			DesignLeavingWaterTemp( 0.0 ),
			DesignEnteringAirTemp( 0.0 ),
			DesignEnteringAirWetBulbTemp( 0.0 ),
			FluidCoolerMassFlowRateMultiplier( 0.0 ),
			FluidCoolerNominalCapacity( 0.0 ),
			FluidCoolerLowSpeedNomCap( 0.0 ),
			FluidCoolerLowSpeedNomCapSizingFactor( 0.0 ),
			WaterInletNodeNum( 0 ),
			WaterOutletNodeNum( 0 ),
			OutdoorAirInletNodeNum( 0 ),
			HighMassFlowErrorCount( 0 ),
			HighMassFlowErrorIndex( 0 ),
			OutletWaterTempErrorCount( 0 ),
			OutletWaterTempErrorIndex( 0 ),
			SmallWaterMassFlowErrorCount( 0 ),
			SmallWaterMassFlowErrorIndex( 0 ),
			WMFRLessThanMinAvailErrCount( 0 ),
			WMFRLessThanMinAvailErrIndex( 0 ),
			WMFRGreaterThanMaxAvailErrCount( 0 ),
			WMFRGreaterThanMaxAvailErrIndex( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		FluidCoolerspecs(
			std::string const & Name, // User identifier
			std::string const & FluidCoolerType, // Type of fluid cooler
			int const FluidCoolerType_Num,
			int const PerformanceInputMethod_Num,
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // Simulate the machine at it's operating part load ratio
			Real64 const DesignWaterFlowRate, // Design water flow rate through the fluid cooler [m3/s]
			Real64 const DesWaterMassFlowRate, // Design water flow rate through the fluid cooler [kg/s]
			Real64 const HighSpeedAirFlowRate, // Air flow rate through the fluid cooler at high speed [m3/s]
			Real64 const HighSpeedFanPower, // Fan power at high fan speed [W]
			Real64 const HighSpeedFluidCoolerUA, // UA of fluid cooler at high fan speed [W/C]
			Real64 const LowSpeedAirFlowRate, // Air flow rate through fluid cooler at low speed [m3/s]
			Real64 const LowSpeedAirFlowRateSizingFactor, // sizing factor for low speed air flow rate []
			Real64 const LowSpeedFanPower, // Fan power at low fan speed [W]
			Real64 const LowSpeedFanPowerSizingFactor, // sizing factor for low speed fan power []
			Real64 const LowSpeedFluidCoolerUA, // UA of fluid cooler at low fan speed [W/C]
			Real64 const LowSpeedFluidCoolerUASizingFactor, // sizing factor for low speed UA []
			Real64 const DesignEnteringWaterTemp, // Entering water temperature at design conditions
			Real64 const DesignLeavingWaterTemp, // Entering water temperature at design conditions
			Real64 const DesignEnteringAirTemp, // Entering water temperature at design conditions
			Real64 const DesignEnteringAirWetBulbTemp, // Entering water temperature at design condition
			Real64 const FluidCoolerMassFlowRateMultiplier, // Maximum fluid cooler flow rate is this multiplier * design flow rate
			Real64 const FluidCoolerNominalCapacity, // Nominal capacity of the fluid cooler [W] at high speed
			Real64 const FluidCoolerLowSpeedNomCap, // Nominal capacity of the fluid cooler [W] at low speed
			Real64 const FluidCoolerLowSpeedNomCapSizingFactor, // sizing factor for low speed capacity []
			int const WaterInletNodeNum, // Node number on the water inlet side of the fluid cooler
			int const WaterOutletNodeNum, // Node number on the water outlet side of the fluid cooler
			int const OutdoorAirInletNodeNum, // Node number of outdoor air inlet for the fluid cooler
			int const HighMassFlowErrorCount, // Counter when mass flow rate is > Design*FluidCoolerMassFlowRateMultiplier
			int const HighMassFlowErrorIndex, // Index for high mass flow recurring error message
			int const OutletWaterTempErrorCount, // Counter when outlet water temperature is < minimum allowed temperature
			int const OutletWaterTempErrorIndex, // Index for outlet water temperature recurring error message
			int const SmallWaterMassFlowErrorCount, // Counter when water mass flow rate is very small
			int const SmallWaterMassFlowErrorIndex, // Index for very small water mass flow rate recurring error message
			int const WMFRLessThanMinAvailErrCount, // Counter when water mass flow rate is less than minimum available
			int const WMFRLessThanMinAvailErrIndex, // Index for water mass flow rate less than minavail recurring message
			int const WMFRGreaterThanMaxAvailErrCount, // Counter when water mass flow rate is greater than minimum available
			int const WMFRGreaterThanMaxAvailErrIndex, // Index for water mass flow rate > minavail recurring message
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum
		) :
			Name( Name ),
			FluidCoolerType( FluidCoolerType ),
			FluidCoolerType_Num( FluidCoolerType_Num ),
			PerformanceInputMethod_Num( PerformanceInputMethod_Num ),
			Available( Available ),
			ON( ON ),
			DesignWaterFlowRate( DesignWaterFlowRate ),
			DesWaterMassFlowRate( DesWaterMassFlowRate ),
			HighSpeedAirFlowRate( HighSpeedAirFlowRate ),
			HighSpeedFanPower( HighSpeedFanPower ),
			HighSpeedFluidCoolerUA( HighSpeedFluidCoolerUA ),
			LowSpeedAirFlowRate( LowSpeedAirFlowRate ),
			LowSpeedAirFlowRateSizingFactor( LowSpeedAirFlowRateSizingFactor ),
			LowSpeedFanPower( LowSpeedFanPower ),
			LowSpeedFanPowerSizingFactor( LowSpeedFanPowerSizingFactor ),
			LowSpeedFluidCoolerUA( LowSpeedFluidCoolerUA ),
			LowSpeedFluidCoolerUASizingFactor( LowSpeedFluidCoolerUASizingFactor ),
			DesignEnteringWaterTemp( DesignEnteringWaterTemp ),
			DesignLeavingWaterTemp( DesignLeavingWaterTemp ),
			DesignEnteringAirTemp( DesignEnteringAirTemp ),
			DesignEnteringAirWetBulbTemp( DesignEnteringAirWetBulbTemp ),
			FluidCoolerMassFlowRateMultiplier( FluidCoolerMassFlowRateMultiplier ),
			FluidCoolerNominalCapacity( FluidCoolerNominalCapacity ),
			FluidCoolerLowSpeedNomCap( FluidCoolerLowSpeedNomCap ),
			FluidCoolerLowSpeedNomCapSizingFactor( FluidCoolerLowSpeedNomCapSizingFactor ),
			WaterInletNodeNum( WaterInletNodeNum ),
			WaterOutletNodeNum( WaterOutletNodeNum ),
			OutdoorAirInletNodeNum( OutdoorAirInletNodeNum ),
			HighMassFlowErrorCount( HighMassFlowErrorCount ),
			HighMassFlowErrorIndex( HighMassFlowErrorIndex ),
			OutletWaterTempErrorCount( OutletWaterTempErrorCount ),
			OutletWaterTempErrorIndex( OutletWaterTempErrorIndex ),
			SmallWaterMassFlowErrorCount( SmallWaterMassFlowErrorCount ),
			SmallWaterMassFlowErrorIndex( SmallWaterMassFlowErrorIndex ),
			WMFRLessThanMinAvailErrCount( WMFRLessThanMinAvailErrCount ),
			WMFRLessThanMinAvailErrIndex( WMFRLessThanMinAvailErrIndex ),
			WMFRGreaterThanMaxAvailErrCount( WMFRGreaterThanMaxAvailErrCount ),
			WMFRGreaterThanMaxAvailErrIndex( WMFRGreaterThanMaxAvailErrIndex ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct FluidCoolerInletConds
	{
		// Members
		Real64 WaterTemp; // Fluid cooler water inlet temperature (C)
		Real64 AirTemp; // Fluid cooler air inlet dry-bulb temperature (C)
		Real64 AirWetBulb; // Fluid cooler air inlet wet-bulb temperature (C)
		Real64 AirPress; // Fluid cooler air barometric pressure
		Real64 AirHumRat; // Fluid cooler air inlet humidity ratio (kg/kg)

		// Default Constructor
		FluidCoolerInletConds() :
			WaterTemp( 0.0 ),
			AirTemp( 0.0 ),
			AirWetBulb( 0.0 ),
			AirPress( 0.0 ),
			AirHumRat( 0.0 )
		{}

		// Member Constructor
		FluidCoolerInletConds(
			Real64 const WaterTemp, // Fluid cooler water inlet temperature (C)
			Real64 const AirTemp, // Fluid cooler air inlet dry-bulb temperature (C)
			Real64 const AirWetBulb, // Fluid cooler air inlet wet-bulb temperature (C)
			Real64 const AirPress, // Fluid cooler air barometric pressure
			Real64 const AirHumRat // Fluid cooler air inlet humidity ratio (kg/kg)
		) :
			WaterTemp( WaterTemp ),
			AirTemp( AirTemp ),
			AirWetBulb( AirWetBulb ),
			AirPress( AirPress ),
			AirHumRat( AirHumRat )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 InletWaterTemp; // Fluid cooler inlet water temperature (C)
		Real64 OutletWaterTemp; // Fluid cooler outlet water temperature (C)
		Real64 WaterMassFlowRate; // Fluid cooler water mass flow rate (m3/s)
		Real64 Qactual; // Fluid cooler heat rejection rate (W)
		Real64 FanPower; // Fluid cooler fan power (W)
		Real64 FanEnergy; // Fluid cooler fan energy consumption (J)

		// Default Constructor
		ReportVars() :
			InletWaterTemp( 0.0 ),
			OutletWaterTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			Qactual( 0.0 ),
			FanPower( 0.0 ),
			FanEnergy( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const InletWaterTemp, // Fluid cooler inlet water temperature (C)
			Real64 const OutletWaterTemp, // Fluid cooler outlet water temperature (C)
			Real64 const WaterMassFlowRate, // Fluid cooler water mass flow rate (m3/s)
			Real64 const Qactual, // Fluid cooler heat rejection rate (W)
			Real64 const FanPower, // Fluid cooler fan power (W)
			Real64 const FanEnergy // Fluid cooler fan energy consumption (J)
		) :
			InletWaterTemp( InletWaterTemp ),
			OutletWaterTemp( OutletWaterTemp ),
			WaterMassFlowRate( WaterMassFlowRate ),
			Qactual( Qactual ),
			FanPower( FanPower ),
			FanEnergy( FanEnergy )
		{}

	};

	// Object Data
	extern FArray1D< FluidCoolerspecs > SimpleFluidCooler; // dimension to number of machines
	extern FArray1D< FluidCoolerInletConds > SimpleFluidCoolerInlet; // inlet conditions
	extern FArray1D< ReportVars > SimpleFluidCoolerReport; // report variables

	// Functions

	void
	SimFluidCoolers(
		std::string & FluidCoolerType,
		std::string & FluidCoolerName,
		int & CompIndex,
		bool & RunFlag,
		bool const InitLoopEquip,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap
	);

	// End CondenserLoopFluidCoolers Module Driver Subroutines
	//******************************************************************************

	// Beginning of CondenserLoopFluidCoolers Module Get Input subroutines
	//******************************************************************************

	void
	GetFluidCoolerInput();

	// End of Get Input subroutines for the CondenserLoopFluidCoolers Module
	//******************************************************************************

	// Beginning Initialization Section for the CondenserLoopFluidCoolers Module
	//******************************************************************************

	void
	InitSimVars();

	void
	InitFluidCooler(
		int const FluidCoolerNum, // Number of the current fluid cooler being simulated
		bool const RunFlag // TRUE if fluid cooler is ON
	);

	void
	SizeFluidCooler( int const FluidCoolerNum );

	// End Initialization Section for the CondenserLoopFluidCoolers Module
	//******************************************************************************

	// Beginning of the CondenserLoopFluidCoolers Module Simulation Subroutines
	// *****************************************************************************

	void
	SingleSpeedFluidCooler( int & FluidCoolerNum );

	void
	TwoSpeedFluidCooler( int & FluidCoolerNum );

	void
	SimSimpleFluidCooler(
		int const FluidCoolerNum,
		Real64 const WaterMassFlowRate,
		Real64 const AirFlowRate,
		Real64 const UAdesign,
		Real64 & OutletWaterTemp
	);

	Real64
	SimpleFluidCoolerUAResidual(
		Real64 const UA, // UA of fluid cooler
		Optional< FArray1S< Real64 > const > Par = _ // par(1) = design fluid cooler load [W]
	);

	// End of the CondenserLoopFluidCoolers Module Simulation Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the FluidCooler Module
	// *****************************************************************************

	void
	UpdateFluidCooler( int const FluidCoolerNum );

	// End of Record Keeping subroutines for the FluidCooler Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the FluidCooler Module
	// *****************************************************************************

	void
	ReportFluidCooler(
		bool const RunFlag,
		int const FluidCoolerNum
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

} // FluidCoolers

} // EnergyPlus

#endif
