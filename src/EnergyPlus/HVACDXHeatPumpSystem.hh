#ifndef HVACDXHeatPumpSystem_hh_INCLUDED
#define HVACDXHeatPumpSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACDXHeatPumpSystem {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const MinAirMassFlow;
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDXHeatPumpSystems; // The Number of DXHeatPumpSystems found in the Input
	extern bool EconomizerFlag; // holds air loop economizer status

	// Make this type allocatable
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Update routine to check convergence and update nodes

	// Types

	struct DXHeatPumpSystemStruct
	{
		// Members
		std::string DXHeatPumpSystemType; // Type of DXHeatingSystem
		std::string Name; // Name of the DXHeatingSystem
		int SchedPtr;
		std::string HeatPumpCoilType;
		int HeatPumpCoilType_Num;
		std::string HeatPumpCoilName;
		int HeatPumpCoilIndex;
		int DXHeatPumpCoilInletNodeNum;
		int DXHeatPumpCoilOutletNodeNum;
		int DXSystemControlNodeNum; // the node number of the node with the set point
		Real64 DesiredOutletTemp; // the temperature at the unit outlet node needed
		// to meet the supply air set point.
		Real64 PartLoadFrac; // part load fraction for current time step (single speed)
		Real64 SpeedRatio; // current compressor speed ratio (variable speed)
		Real64 CycRatio; // cycling part load ratio (variable speed)
		int FanOpMode; // Fan operating mode (see parameter above)
		// Warning message variables
		int DXCoilSensPLRIter; // used in DXCoil calculations
		int DXCoilSensPLRIterIndex; // used in DXCoil calculations
		int DXCoilSensPLRFail; // used in DXCoil calculations
		int DXCoilSensPLRFailIndex; // used in DXCoil calculations
		// When the Dx system is a part of Outdoor Air Unit
		Real64 OAUnitSetTemp; // set
		// variable-speed coil
		int SpeedNum; // select speed number for variable-speed coil

		// Default Constructor
		DXHeatPumpSystemStruct() :
			SchedPtr( 0 ),
			HeatPumpCoilType_Num( 0 ),
			HeatPumpCoilIndex( 0 ),
			DXHeatPumpCoilInletNodeNum( 0 ),
			DXHeatPumpCoilOutletNodeNum( 0 ),
			DXSystemControlNodeNum( 0 ),
			DesiredOutletTemp( 0.0 ),
			PartLoadFrac( 0.0 ),
			SpeedRatio( 0.0 ),
			CycRatio( 0.0 ),
			FanOpMode( 0 ),
			DXCoilSensPLRIter( 0 ),
			DXCoilSensPLRIterIndex( 0 ),
			DXCoilSensPLRFail( 0 ),
			DXCoilSensPLRFailIndex( 0 ),
			OAUnitSetTemp( 0.0 ),
			SpeedNum( 0 )
		{}

		// Member Constructor
		DXHeatPumpSystemStruct(
			std::string const & DXHeatPumpSystemType, // Type of DXHeatingSystem
			std::string const & Name, // Name of the DXHeatingSystem
			int const SchedPtr,
			std::string const & HeatPumpCoilType,
			int const HeatPumpCoilType_Num,
			std::string const & HeatPumpCoilName,
			int const HeatPumpCoilIndex,
			int const DXHeatPumpCoilInletNodeNum,
			int const DXHeatPumpCoilOutletNodeNum,
			int const DXSystemControlNodeNum, // the node number of the node with the set point
			Real64 const DesiredOutletTemp, // the temperature at the unit outlet node needed
			Real64 const PartLoadFrac, // part load fraction for current time step (single speed)
			Real64 const SpeedRatio, // current compressor speed ratio (variable speed)
			Real64 const CycRatio, // cycling part load ratio (variable speed)
			int const FanOpMode, // Fan operating mode (see parameter above)
			int const DXCoilSensPLRIter, // used in DXCoil calculations
			int const DXCoilSensPLRIterIndex, // used in DXCoil calculations
			int const DXCoilSensPLRFail, // used in DXCoil calculations
			int const DXCoilSensPLRFailIndex, // used in DXCoil calculations
			Real64 const OAUnitSetTemp, // set
			int const SpeedNum // select speed number for variable-speed coil
		) :
			DXHeatPumpSystemType( DXHeatPumpSystemType ),
			Name( Name ),
			SchedPtr( SchedPtr ),
			HeatPumpCoilType( HeatPumpCoilType ),
			HeatPumpCoilType_Num( HeatPumpCoilType_Num ),
			HeatPumpCoilName( HeatPumpCoilName ),
			HeatPumpCoilIndex( HeatPumpCoilIndex ),
			DXHeatPumpCoilInletNodeNum( DXHeatPumpCoilInletNodeNum ),
			DXHeatPumpCoilOutletNodeNum( DXHeatPumpCoilOutletNodeNum ),
			DXSystemControlNodeNum( DXSystemControlNodeNum ),
			DesiredOutletTemp( DesiredOutletTemp ),
			PartLoadFrac( PartLoadFrac ),
			SpeedRatio( SpeedRatio ),
			CycRatio( CycRatio ),
			FanOpMode( FanOpMode ),
			DXCoilSensPLRIter( DXCoilSensPLRIter ),
			DXCoilSensPLRIterIndex( DXCoilSensPLRIterIndex ),
			DXCoilSensPLRFail( DXCoilSensPLRFail ),
			DXCoilSensPLRFailIndex( DXCoilSensPLRFailIndex ),
			OAUnitSetTemp( OAUnitSetTemp ),
			SpeedNum( SpeedNum )
		{}

	};

	// Object Data
	extern Array1D< DXHeatPumpSystemStruct > DXHeatPumpSystem;

	// Functions

	void
	SimDXHeatPumpSystem(
		std::string const & DXHeatPumpSystemName, // Name of DXSystem:Airloop object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to CoilSystem:Heating:DX object
		Optional_int_const OAUnitNum = _, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional< Real64 > QTotOut = _ // the total cooling output of unit
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDXHeatPumpSystemInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitDXHeatPumpSystem(
		int const DXSystemNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		Optional_int_const OAUnitNum = _, // number of the current outdoor air unit being simulated
		Optional< Real64 const > OAUCoilOutTemp = _ // the coil inlet temperature of OutdoorAirUnit
	);

	// End of Initialization subroutines for the Module
	// *****************************************************************************

	// Beginning of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

	void
	ControlDXHeatingSystem(
		int const DXSystemNum, // index to DXSystem
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//******************************************************************************

	Real64
	VSCoilCyclingResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//******************************************************************************

	Real64
	VSCoilSpeedResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

} // HVACDXHeatPumpSystem

} // EnergyPlus

#endif
