#ifndef HVACDXSystem_hh_INCLUDED
#define HVACDXSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// note that there are two modules in this file

//  HVACDXSystem is for cooling DX coils

//  HVACDXHeatPumpSystem is for heating DX coils

namespace HVACDXSystem {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const MinAirMassFlow;
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run
	// Dehumidification control modes (DehumidControlMode)
	extern int const DehumidControl_None;
	extern int const DehumidControl_Multimode;
	extern int const DehumidControl_CoolReheat;
	extern bool GetInputFlag; // Flag to get input only once

	//packaged TES modes
	extern int const OffMode;
	extern int const CoolingOnlyMode;
	extern int const CoolingAndChargeMode;
	extern int const CoolingAndDischargeMode;
	extern int const ChargeOnlyMode;
	extern int const DischargeOnlyMode;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDXSystem; // The Number of DXCoolingSystems found in the Input
	extern bool EconomizerFlag; // holds air loop economizer status

	// Make this type allocatable
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Update routine to check convergence and update nodes

	// Types

	struct DXCoolingConditions
	{
		// Members
		std::string DXCoolingSystemType; // Type of DXCoolingSystem
		std::string Name; // Name of the DXCoolingSystem
		int SchedPtr;
		std::string CoolingCoilType;
		int CoolingCoilType_Num;
		std::string CoolingCoilName;
		int CoolingCoilIndex;
		int DXCoolingCoilInletNodeNum;
		int DXCoolingCoilOutletNodeNum;
		int DXSystemControlNodeNum; // the node number of the node with the setpoint
		Real64 DesiredOutletTemp; // the temperature at the unit outlet node needed
		// to meet the supply air setpoint.
		Real64 DesiredOutletHumRat; // the humidity ratio at the unit outlet node needed
		// to meet the supply air setpoint.
		Real64 PartLoadFrac; // part load fraction for current time step (single speed)
		Real64 SpeedRatio; // current compressor speed ratio (variable speed)
		Real64 CycRatio; // cycling part load ratio (variable speed)
		bool RunOnSensibleLoad; // logical determines if this system will run to
		// meet a sensible load - for future use
		bool RunOnLatentLoad; // logical determines if this system will run to
		// meet a latent-only load - for future use
		int DehumidControlType; // Dehumidification control type (currently only for multimode coil)
		int DehumidificationMode; // Dehumidification mode for multimode coil,
		// 0=normal, 1+=enhanced dehumidification mode
		int FanOpMode; // Fan operating mode (see parameter above)
		// Warning message variables
		int HXAssistedSensPLRIter; // used in HX Assisted calculations
		int HXAssistedSensPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedSensPLRFail; // used in HX Assisted calculations
		int HXAssistedSensPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedSensPLRFail2; // used in HX Assisted calculations
		int HXAssistedSensPLRFailIndex2; // used in HX Assisted calculations
		int HXAssistedLatPLRIter; // used in HX Assisted calculations
		int HXAssistedLatPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedLatPLRFail; // used in HX Assisted calculations
		int HXAssistedLatPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRIter; // used in HX Assisted calculations
		int HXAssistedCRLatPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFail; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFail2; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFailIndex2; // used in HX Assisted calculations
		int DXCoilSensPLRIter; // used in DXCoil calculations
		int DXCoilSensPLRIterIndex; // used in DXCoil calculations
		int DXCoilSensPLRFail; // used in DXCoil calculations
		int DXCoilSensPLRFailIndex; // used in DXCoil calculations
		int DXCoilLatPLRIter; // used in DXCoil calculations
		int DXCoilLatPLRIterIndex; // used in DXCoil calculations
		int DXCoilLatPLRFail; // used in DXCoil calculations
		int DXCoilLatPLRFailIndex; // used in DXCoil calculations
		int MSpdSensPLRIter; // used in MultiSpeed calculations
		int MSpdSensPLRIterIndex; // used in MultiSpeed calculations
		int MSpdCycSensPLRIter; // used in MultiSpeed calculations
		int MSpdCycSensPLRIterIndex; // used in MultiSpeed calculations
		int MSpdLatPLRIter; // used in MultiSpeed calculations
		int MSpdLatPLRIterIndex; // used in MultiSpeed calculations
		int MSpdCycLatPLRIter; // used in MultiSpeed calculations
		int MSpdCycLatPLRIterIndex; // used in MultiSpeed calculations
		int MModeSensPLRIter; // used in MultiMode calculations
		int MModeSensPLRIterIndex; // used in MultiMode calculations
		int MModeLatPLRIter; // used in MultiMode calculations
		int MModeLatPLRIterIndex; // used in MultiMode calculations
		int MModeLatPLRIter2; // used in MultiMode calculations
		int MModeLatPLRIterIndex2; // used in MultiMode calculations
		// When the Dx system is a part of Outdoor Air Unit
		Real64 OAUnitSetTemp; // set
		// DOAS DX Cooling coil
		bool ISHundredPercentDOASDXCoil; // logical determines if this system will run as 100% DOAS
		// DX Coil, false is regular DX coil
		Real64 DOASDXCoolingCoilMinTout; // DOAS DX Cooling coil outlet air minimum temperature
		int FrostControlStatus; // DOAS coil system frost control status
		// variable-speed coil
		int SpeedNum; // select speed number for variable-speed coil
		// Packaged thermal energy storage coil
		int TESOpMode;

		// Default Constructor
		DXCoolingConditions() :
			SchedPtr( 0 ),
			CoolingCoilType_Num( 0 ),
			CoolingCoilIndex( 0 ),
			DXCoolingCoilInletNodeNum( 0 ),
			DXCoolingCoilOutletNodeNum( 0 ),
			DXSystemControlNodeNum( 0 ),
			DesiredOutletTemp( 0.0 ),
			DesiredOutletHumRat( 1.0 ),
			PartLoadFrac( 0.0 ),
			SpeedRatio( 0.0 ),
			CycRatio( 0.0 ),
			RunOnSensibleLoad( true ),
			RunOnLatentLoad( false ),
			DehumidControlType( 0 ),
			DehumidificationMode( 0 ),
			FanOpMode( 0 ),
			HXAssistedSensPLRIter( 0 ),
			HXAssistedSensPLRIterIndex( 0 ),
			HXAssistedSensPLRFail( 0 ),
			HXAssistedSensPLRFailIndex( 0 ),
			HXAssistedSensPLRFail2( 0 ),
			HXAssistedSensPLRFailIndex2( 0 ),
			HXAssistedLatPLRIter( 0 ),
			HXAssistedLatPLRIterIndex( 0 ),
			HXAssistedLatPLRFail( 0 ),
			HXAssistedLatPLRFailIndex( 0 ),
			HXAssistedCRLatPLRIter( 0 ),
			HXAssistedCRLatPLRIterIndex( 0 ),
			HXAssistedCRLatPLRFail( 0 ),
			HXAssistedCRLatPLRFailIndex( 0 ),
			HXAssistedCRLatPLRFail2( 0 ),
			HXAssistedCRLatPLRFailIndex2( 0 ),
			DXCoilSensPLRIter( 0 ),
			DXCoilSensPLRIterIndex( 0 ),
			DXCoilSensPLRFail( 0 ),
			DXCoilSensPLRFailIndex( 0 ),
			DXCoilLatPLRIter( 0 ),
			DXCoilLatPLRIterIndex( 0 ),
			DXCoilLatPLRFail( 0 ),
			DXCoilLatPLRFailIndex( 0 ),
			MSpdSensPLRIter( 0 ),
			MSpdSensPLRIterIndex( 0 ),
			MSpdCycSensPLRIter( 0 ),
			MSpdCycSensPLRIterIndex( 0 ),
			MSpdLatPLRIter( 0 ),
			MSpdLatPLRIterIndex( 0 ),
			MSpdCycLatPLRIter( 0 ),
			MSpdCycLatPLRIterIndex( 0 ),
			MModeSensPLRIter( 0 ),
			MModeSensPLRIterIndex( 0 ),
			MModeLatPLRIter( 0 ),
			MModeLatPLRIterIndex( 0 ),
			MModeLatPLRIter2( 0 ),
			MModeLatPLRIterIndex2( 0 ),
			OAUnitSetTemp( 0.0 ),
			ISHundredPercentDOASDXCoil( false ),
			DOASDXCoolingCoilMinTout( 0.0 ),
			FrostControlStatus( 0 ),
			SpeedNum( 0 ),
			TESOpMode( 0 )
		{}

		// Member Constructor
		DXCoolingConditions(
			std::string const & DXCoolingSystemType, // Type of DXCoolingSystem
			std::string const & Name, // Name of the DXCoolingSystem
			int const SchedPtr,
			std::string const & CoolingCoilType,
			int const CoolingCoilType_Num,
			std::string const & CoolingCoilName,
			int const CoolingCoilIndex,
			int const DXCoolingCoilInletNodeNum,
			int const DXCoolingCoilOutletNodeNum,
			int const DXSystemControlNodeNum, // the node number of the node with the setpoint
			Real64 const DesiredOutletTemp, // the temperature at the unit outlet node needed
			Real64 const DesiredOutletHumRat, // the humidity ratio at the unit outlet node needed
			Real64 const PartLoadFrac, // part load fraction for current time step (single speed)
			Real64 const SpeedRatio, // current compressor speed ratio (variable speed)
			Real64 const CycRatio, // cycling part load ratio (variable speed)
			bool const RunOnSensibleLoad, // logical determines if this system will run to
			bool const RunOnLatentLoad, // logical determines if this system will run to
			int const DehumidControlType, // Dehumidification control type (currently only for multimode coil)
			int const DehumidificationMode, // Dehumidification mode for multimode coil,
			int const FanOpMode, // Fan operating mode (see parameter above)
			int const HXAssistedSensPLRIter, // used in HX Assisted calculations
			int const HXAssistedSensPLRIterIndex, // used in HX Assisted calculations
			int const HXAssistedSensPLRFail, // used in HX Assisted calculations
			int const HXAssistedSensPLRFailIndex, // used in HX Assisted calculations
			int const HXAssistedSensPLRFail2, // used in HX Assisted calculations
			int const HXAssistedSensPLRFailIndex2, // used in HX Assisted calculations
			int const HXAssistedLatPLRIter, // used in HX Assisted calculations
			int const HXAssistedLatPLRIterIndex, // used in HX Assisted calculations
			int const HXAssistedLatPLRFail, // used in HX Assisted calculations
			int const HXAssistedLatPLRFailIndex, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRIter, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRIterIndex, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFail, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFailIndex, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFail2, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFailIndex2, // used in HX Assisted calculations
			int const DXCoilSensPLRIter, // used in DXCoil calculations
			int const DXCoilSensPLRIterIndex, // used in DXCoil calculations
			int const DXCoilSensPLRFail, // used in DXCoil calculations
			int const DXCoilSensPLRFailIndex, // used in DXCoil calculations
			int const DXCoilLatPLRIter, // used in DXCoil calculations
			int const DXCoilLatPLRIterIndex, // used in DXCoil calculations
			int const DXCoilLatPLRFail, // used in DXCoil calculations
			int const DXCoilLatPLRFailIndex, // used in DXCoil calculations
			int const MSpdSensPLRIter, // used in MultiSpeed calculations
			int const MSpdSensPLRIterIndex, // used in MultiSpeed calculations
			int const MSpdCycSensPLRIter, // used in MultiSpeed calculations
			int const MSpdCycSensPLRIterIndex, // used in MultiSpeed calculations
			int const MSpdLatPLRIter, // used in MultiSpeed calculations
			int const MSpdLatPLRIterIndex, // used in MultiSpeed calculations
			int const MSpdCycLatPLRIter, // used in MultiSpeed calculations
			int const MSpdCycLatPLRIterIndex, // used in MultiSpeed calculations
			int const MModeSensPLRIter, // used in MultiMode calculations
			int const MModeSensPLRIterIndex, // used in MultiMode calculations
			int const MModeLatPLRIter, // used in MultiMode calculations
			int const MModeLatPLRIterIndex, // used in MultiMode calculations
			int const MModeLatPLRIter2, // used in MultiMode calculations
			int const MModeLatPLRIterIndex2, // used in MultiMode calculations
			Real64 const OAUnitSetTemp, // set
			bool const ISHundredPercentDOASDXCoil, // logical determines if this system will run as 100% DOAS
			Real64 const DOASDXCoolingCoilMinTout, // DOAS DX Cooling coil outlet air minimum temperature
			int const FrostControlStatus, // DOAS coil system frost control status
			int const SpeedNum, // select speed number for variable-speed coil
			int const TESOpMode
		) :
			DXCoolingSystemType( DXCoolingSystemType ),
			Name( Name ),
			SchedPtr( SchedPtr ),
			CoolingCoilType( CoolingCoilType ),
			CoolingCoilType_Num( CoolingCoilType_Num ),
			CoolingCoilName( CoolingCoilName ),
			CoolingCoilIndex( CoolingCoilIndex ),
			DXCoolingCoilInletNodeNum( DXCoolingCoilInletNodeNum ),
			DXCoolingCoilOutletNodeNum( DXCoolingCoilOutletNodeNum ),
			DXSystemControlNodeNum( DXSystemControlNodeNum ),
			DesiredOutletTemp( DesiredOutletTemp ),
			DesiredOutletHumRat( DesiredOutletHumRat ),
			PartLoadFrac( PartLoadFrac ),
			SpeedRatio( SpeedRatio ),
			CycRatio( CycRatio ),
			RunOnSensibleLoad( RunOnSensibleLoad ),
			RunOnLatentLoad( RunOnLatentLoad ),
			DehumidControlType( DehumidControlType ),
			DehumidificationMode( DehumidificationMode ),
			FanOpMode( FanOpMode ),
			HXAssistedSensPLRIter( HXAssistedSensPLRIter ),
			HXAssistedSensPLRIterIndex( HXAssistedSensPLRIterIndex ),
			HXAssistedSensPLRFail( HXAssistedSensPLRFail ),
			HXAssistedSensPLRFailIndex( HXAssistedSensPLRFailIndex ),
			HXAssistedSensPLRFail2( HXAssistedSensPLRFail2 ),
			HXAssistedSensPLRFailIndex2( HXAssistedSensPLRFailIndex2 ),
			HXAssistedLatPLRIter( HXAssistedLatPLRIter ),
			HXAssistedLatPLRIterIndex( HXAssistedLatPLRIterIndex ),
			HXAssistedLatPLRFail( HXAssistedLatPLRFail ),
			HXAssistedLatPLRFailIndex( HXAssistedLatPLRFailIndex ),
			HXAssistedCRLatPLRIter( HXAssistedCRLatPLRIter ),
			HXAssistedCRLatPLRIterIndex( HXAssistedCRLatPLRIterIndex ),
			HXAssistedCRLatPLRFail( HXAssistedCRLatPLRFail ),
			HXAssistedCRLatPLRFailIndex( HXAssistedCRLatPLRFailIndex ),
			HXAssistedCRLatPLRFail2( HXAssistedCRLatPLRFail2 ),
			HXAssistedCRLatPLRFailIndex2( HXAssistedCRLatPLRFailIndex2 ),
			DXCoilSensPLRIter( DXCoilSensPLRIter ),
			DXCoilSensPLRIterIndex( DXCoilSensPLRIterIndex ),
			DXCoilSensPLRFail( DXCoilSensPLRFail ),
			DXCoilSensPLRFailIndex( DXCoilSensPLRFailIndex ),
			DXCoilLatPLRIter( DXCoilLatPLRIter ),
			DXCoilLatPLRIterIndex( DXCoilLatPLRIterIndex ),
			DXCoilLatPLRFail( DXCoilLatPLRFail ),
			DXCoilLatPLRFailIndex( DXCoilLatPLRFailIndex ),
			MSpdSensPLRIter( MSpdSensPLRIter ),
			MSpdSensPLRIterIndex( MSpdSensPLRIterIndex ),
			MSpdCycSensPLRIter( MSpdCycSensPLRIter ),
			MSpdCycSensPLRIterIndex( MSpdCycSensPLRIterIndex ),
			MSpdLatPLRIter( MSpdLatPLRIter ),
			MSpdLatPLRIterIndex( MSpdLatPLRIterIndex ),
			MSpdCycLatPLRIter( MSpdCycLatPLRIter ),
			MSpdCycLatPLRIterIndex( MSpdCycLatPLRIterIndex ),
			MModeSensPLRIter( MModeSensPLRIter ),
			MModeSensPLRIterIndex( MModeSensPLRIterIndex ),
			MModeLatPLRIter( MModeLatPLRIter ),
			MModeLatPLRIterIndex( MModeLatPLRIterIndex ),
			MModeLatPLRIter2( MModeLatPLRIter2 ),
			MModeLatPLRIterIndex2( MModeLatPLRIterIndex2 ),
			OAUnitSetTemp( OAUnitSetTemp ),
			ISHundredPercentDOASDXCoil( ISHundredPercentDOASDXCoil ),
			DOASDXCoolingCoilMinTout( DOASDXCoolingCoilMinTout ),
			FrostControlStatus( FrostControlStatus ),
			SpeedNum( SpeedNum ),
			TESOpMode( TESOpMode )
		{}

	};

	// Object Data
	extern Array1D< DXCoolingConditions > DXCoolingSystem;

	// Functions

	void
	SimDXCoolingSystem(
		std::string const & DXCoolingSystemName, // Name of DXSystem:Airloop object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to DXSystem:Airloop object
		Optional_int_const OAUnitNum = _, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional< Real64 > QTotOut = _ // the total cooling output of unit
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDXCoolingSystemInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitDXCoolingSystem(
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
	ControlDXSystem(
		int const DXSystemNum, // index to DXSystem
		bool const FirstHVACIteration, // First HVAC iteration flag
		bool & HXUnitOn // flag to enable heat exchanger heat recovery
	);

	Real64
	DXCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilVarSpeedHumRatResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilCyclingResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilCyclingHumRatResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DOE2DXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HXAssistedCoolCoilTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HXAssistedCoolCoilHRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	TESCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	TESCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	void
	FrostControlSetPointLimit(
		int const DXSystemNum, // dx cooling coil system index
		Real64 & TempSetPoint, // temperature setpoint of the sensor node
		Real64 & HumRatSetPoint, // humidity ratio setpoint of the sensor node
		Real64 const BaroPress, // baromtric pressure, Pa [N/m^2]
		Real64 const TfrostControl, // minimum temperature limit for forst control
		int const ControlMode // temperature or humidity control mode
	);

	void
	CheckDXCoolingCoilInOASysExists( std::string const & DXCoilSysName );

	void
	GetCoolingCoilTypeNameAndIndex(
		std::string const & DXCoilSysName,
		int & CoolCoilType,
		int & CoolCoilIndex,
		std::string & CoolCoilName,
		bool & ErrFound
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

	Real64
	VSCoilCyclingHumResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//******************************************************************************

	Real64
	VSCoilSpeedHumResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//        End of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

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

} // HVACDXSystem

//******************************************************************************************************

//        End of Calculation subroutines for the DXCoolingSystem Module
// *****************************************************************************

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


} // EnergyPlus

#endif
