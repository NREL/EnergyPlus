#ifndef Pumps_hh_INCLUDED
#define Pumps_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace Pumps {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:
	extern int const Continuous; // Pump control type (pump always running)
	extern int const Intermittent; // Pump control type (pump runs only when there is a demand)

	extern int const VFDManual; // VFD control type (Scheduled RPM)
	extern int const VFDAutomatic; // VFD control type (Variable RPM according to flow request)

	extern int const OptimalScheme; // Control sequencing for pump bank
	extern int const SequentialScheme; // Control sequencing for pump bank
	extern int const UserDefined; // Control sequencing for pump bank

	extern std::string const cPump_VarSpeed;
	extern int const Pump_VarSpeed;
	extern std::string const cPump_ConSpeed;
	extern int const Pump_ConSpeed;
	extern std::string const cPump_Cond;
	extern int const Pump_Cond;
	extern std::string const cPumpBank_VarSpeed;
	extern int const PumpBank_VarSpeed;
	extern std::string const cPumpBank_ConSpeed;
	extern int const PumpBank_ConSpeed;
	extern Array1D_string const cPumpTypes;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumPumps; // Num Pumps (used in pump bank)
	extern int NumPumpsRunning; // Num of pumps ON (used in pump bank)
	extern int NumPumpsFullLoad; // Num pumps running at full load (used in pump bank)
	//  INTEGER       :: NumPumpsPartLoad                   = 0    !Num pumps running at part load (used in pump bank)

	//simulation and reporting variables
	//  REAL(r64)           :: OutletTemp                   = 0.0d0  ! pump outlet temperature
	extern Real64 PumpMassFlowRate; // mass flow rate at pump inlet node
	//  REAL(r64)           :: PumpPress                    = 0.0d0  ! For Passing around the steam loops
	//  REAL(r64)           :: PumpQuality                  = 0.0d0  ! For Passing around the steam loops=0.0 here
	extern Real64 PumpHeattoFluid; // Pump Power dissipated in fluid stream
	extern Real64 Power; // Pump Electric power
	extern Real64 ShaftPower; // Power passing through pump shaft

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Types

	struct PumpVFDControlData
	{
		// Members
		std::string Name;
		std::string ManualRPMSchedName;
		int ManualRPMSchedIndex;
		std::string LowerPsetSchedName;
		int LowerPsetSchedIndex;
		std::string UpperPsetSchedName;
		int UpperPsetSchedIndex;
		std::string MinRPMSchedName;
		int MinRPMSchedIndex;
		std::string MaxRPMSchedName;
		int MaxRPMSchedIndex;
		int VFDControlType; // Integer equivalent of VFDControlType
		Real64 MaxRPM; // Maximum RPM range value - schedule limit
		Real64 MinRPM; // Minimum RPM range value - schedule limit
		Real64 PumpActualRPM; // RPM recalculated from final flow through the loop

		// Default Constructor
		PumpVFDControlData() :
			ManualRPMSchedIndex( 0 ),
			LowerPsetSchedIndex( 0 ),
			UpperPsetSchedIndex( 0 ),
			MinRPMSchedIndex( 0 ),
			MaxRPMSchedIndex( 0 ),
			VFDControlType( 0 ),
			MaxRPM( 0.0 ),
			MinRPM( 0.0 ),
			PumpActualRPM( 0.0 )
		{}

		// Member Constructor
		PumpVFDControlData(
			std::string const & Name,
			std::string const & ManualRPMSchedName,
			int const ManualRPMSchedIndex,
			std::string const & LowerPsetSchedName,
			int const LowerPsetSchedIndex,
			std::string const & UpperPsetSchedName,
			int const UpperPsetSchedIndex,
			std::string const & MinRPMSchedName,
			int const MinRPMSchedIndex,
			std::string const & MaxRPMSchedName,
			int const MaxRPMSchedIndex,
			int const VFDControlType, // Integer equivalent of VFDControlType
			Real64 const MaxRPM, // Maximum RPM range value - schedule limit
			Real64 const MinRPM, // Minimum RPM range value - schedule limit
			Real64 const PumpActualRPM // RPM recalculated from final flow through the loop
		) :
			Name( Name ),
			ManualRPMSchedName( ManualRPMSchedName ),
			ManualRPMSchedIndex( ManualRPMSchedIndex ),
			LowerPsetSchedName( LowerPsetSchedName ),
			LowerPsetSchedIndex( LowerPsetSchedIndex ),
			UpperPsetSchedName( UpperPsetSchedName ),
			UpperPsetSchedIndex( UpperPsetSchedIndex ),
			MinRPMSchedName( MinRPMSchedName ),
			MinRPMSchedIndex( MinRPMSchedIndex ),
			MaxRPMSchedName( MaxRPMSchedName ),
			MaxRPMSchedIndex( MaxRPMSchedIndex ),
			VFDControlType( VFDControlType ),
			MaxRPM( MaxRPM ),
			MinRPM( MinRPM ),
			PumpActualRPM( PumpActualRPM )
		{}

	};

	struct PumpSpecs
	{
		// Members
		std::string Name; // user identifier
		std::string PumpSchedule; // Schedule to modify the design nominal capacity of the pump
		std::string PressureCurve_Name; // - placeholder for pump curve name
		int PumpType; // pump type integer, based on local parameter values, used to identify
		// index in the cPumpTypes string array to do error reporting
		int TypeOf_Num; // pump type of number in reference to the dataplant values
		int LoopNum; // loop where pump is located
		int LoopSideNum; // LoopSide index on loop where pump is located
		int BranchNum; // branch index on LoopSide where pump is located
		int CompNum; // component index on branch where pump is located
		int PumpControl; // Integer equivalent of PumpControlType
		int PumpScheduleIndex; // Schedule Pointer
		int InletNodeNum; // Node number on the inlet side of the plant
		int OutletNodeNum; // Node number on the outlet side of the plant
		int SequencingScheme; // Optimal, Sequential, User-Defined
		int FluidIndex; // Index for Fluid Properties
		int NumPumpsInBank; // Node number on the inlet side of the plant
		int PowerErrIndex1; // for recurring errors
		int PowerErrIndex2; // for recurring errors
		Real64 MinVolFlowRateFrac; // minimum schedule value fraction modifier
		Real64 NomVolFlowRate; // design nominal capacity of Pump
		bool NomVolFlowRateWasAutoSized; // true if previous was autosize on input
		Real64 MassFlowRateMax; // design nominal capacity of Pump
		bool EMSMassFlowOverrideOn; // if true, then EMS is calling to override flow requests.
		Real64 EMSMassFlowValue; // EMS value to use for mass flow rate [kg/s]
		Real64 NomSteamVolFlowRate; // For Steam Pump
		bool NomSteamVolFlowRateWasAutoSized; // true if steam volume flow rate was autosize on input
		Real64 MinVolFlowRate; // For a Variable Flow Pump this is the minimum capacity during operation.
		Real64 MassFlowRateMin; // For a Variable Flow Pump this is the minimum capacity during operation.
		Real64 NomPumpHead; // design nominal head pressure of Pump, [Pa]
		bool EMSPressureOverrideOn; // if true, EMS is calling to override pump pressure
		Real64 EMSPressureOverrideValue; // EMS value to use for pressure [Pa]
		Real64 NomPowerUse; // design nominal capacity of Pump
		bool NomPowerUseWasAutoSized; // true if power was autosize on input
		Real64 MotorEffic; // efficiency of the motor
		Real64 PumpEffic; // efficiency of the pump
		Real64 FracMotorLossToFluid; // ?????
		Real64 Energy; // Energy consumed
		Real64 Power; // Power used
		Array1D< Real64 > PartLoadCoef; // Pump Curve Coefficients
		int PressureCurve_Index; // Pointer to a pump coefficient curve
		Real64 PumpMassFlowRateMaxRPM; // Mass flow rate calculated from maximum rpm
		Real64 PumpMassFlowRateMinRPM; // Mass flow rate calculated from minimum rpm
		Real64 MinPhiValue; // Minimum value of Phi (from CurveManager)
		Real64 MaxPhiValue; // Maximum value of Phi (from CurveManager)
		Real64 ImpellerDiameter; // Pump Impeller Diameter [m]
		Real64 RotSpeed_RPM; // Rotational speed used for input in revs/min
		Real64 RotSpeed; // Rotational speed for calculations in revs/sec
		bool PumpInitFlag;
		bool PumpOneTimeFlag;
		bool CheckEquipName;
		bool HasVFD;
		PumpVFDControlData VFD;
		bool OneTimePressureWarning;
		bool HeatLossesToZone; // if true then pump losses added to surrounding zone
		int ZoneNum; // index for zone surrounding pump
		Real64 SkinLossRadFraction; // radiative split for skin losses to zone
		bool LoopSolverOverwriteFlag; // loop solver overwrite for determining pump minimum flow rate

		// Default Constructor
		PumpSpecs() :
			PumpType( 0 ),
			TypeOf_Num( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			PumpControl( 0 ),
			PumpScheduleIndex( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			SequencingScheme( 0 ),
			FluidIndex( 0 ),
			NumPumpsInBank( 0 ),
			PowerErrIndex1( 0 ),
			PowerErrIndex2( 0 ),
			MinVolFlowRateFrac( 0.0 ),
			NomVolFlowRate( 0.0 ),
			NomVolFlowRateWasAutoSized( false ),
			MassFlowRateMax( 0.0 ),
			EMSMassFlowOverrideOn( false ),
			EMSMassFlowValue( 0.0 ),
			NomSteamVolFlowRate( 0.0 ),
			NomSteamVolFlowRateWasAutoSized( false ),
			MinVolFlowRate( 0.0 ),
			MassFlowRateMin( 0.0 ),
			NomPumpHead( 0.0 ),
			EMSPressureOverrideOn( false ),
			EMSPressureOverrideValue( 0.0 ),
			NomPowerUse( 0.0 ),
			NomPowerUseWasAutoSized( false ),
			MotorEffic( 0.0 ),
			PumpEffic( 0.0 ),
			FracMotorLossToFluid( 0.0 ),
			Energy( 0.0 ),
			Power( 0.0 ),
			PartLoadCoef( 4, 0.0 ),
			PressureCurve_Index( 0 ),
			PumpMassFlowRateMaxRPM( 0.0 ),
			PumpMassFlowRateMinRPM( 0.0 ),
			MinPhiValue( 0.0 ),
			MaxPhiValue( 0.0 ),
			ImpellerDiameter( 0.0 ),
			RotSpeed_RPM( 0.0 ),
			RotSpeed( 0.0 ),
			PumpInitFlag( true ),
			PumpOneTimeFlag( true ),
			CheckEquipName( true ),
			HasVFD( false ),
			OneTimePressureWarning( true ),
			HeatLossesToZone( false ),
			ZoneNum( 0 ),
			SkinLossRadFraction( 0.0 ),
			LoopSolverOverwriteFlag( false )
		{}

		// Member Constructor
		PumpSpecs(
			std::string const & Name, // user identifier
			std::string const & PumpSchedule, // Schedule to modify the design nominal capacity of the pump
			std::string const & PressureCurve_Name, // - placeholder for pump curve name
			int const PumpType, // pump type integer, based on local parameter values, used to identify
			int const TypeOf_Num, // pump type of number in reference to the dataplant values
			int const LoopNum, // loop where pump is located
			int const LoopSideNum, // LoopSide index on loop where pump is located
			int const BranchNum, // branch index on LoopSide where pump is located
			int const CompNum, // component index on branch where pump is located
			int const PumpControl, // Integer equivalent of PumpControlType
			int const PumpScheduleIndex, // Schedule Pointer
			int const InletNodeNum, // Node number on the inlet side of the plant
			int const OutletNodeNum, // Node number on the outlet side of the plant
			int const SequencingScheme, // Optimal, Sequential, User-Defined
			int const FluidIndex, // Index for Fluid Properties
			int const NumPumpsInBank, // Node number on the inlet side of the plant
			int const PowerErrIndex1, // for recurring errors
			int const PowerErrIndex2, // for recurring errors
			Real64 const MinVolFlowRateFrac, // minimum schedule value fraction modifier
			Real64 const NomVolFlowRate, // design nominal capacity of Pump
			bool const NomVolFlowRateWasAutoSized, // true if nom vol flow rate was autosize
			Real64 const MassFlowRateMax, // design nominal capacity of Pump
			bool const EMSMassFlowOverrideOn, // if true, then EMS is calling to override flow requests.
			Real64 const EMSMassFlowValue, // EMS value to use for mass flow rate [kg/s]
			Real64 const NomSteamVolFlowRate, // For Steam Pump
			Real64 const MinVolFlowRate, // For a Variable Flow Pump this is the minimum capacity during operation.
			Real64 const MassFlowRateMin, // For a Variable Flow Pump this is the minimum capacity during operation.
			Real64 const NomPumpHead, // design nominal head pressure of Pump, [Pa]
			bool const EMSPressureOverrideOn, // if true, EMS is calling to override pump pressure
			Real64 const EMSPressureOverrideValue, // EMS value to use for pressure [Pa]
			Real64 const NomPowerUse, // design nominal capacity of Pump
			bool const NomPowerUseWasAutoSized, // true if previous is autosize on input
			Real64 const MotorEffic, // efficiency of the motor
			Real64 const PumpEffic, // efficiency of the pump
			Real64 const FracMotorLossToFluid, // ?????
			Real64 const Energy, // Energy consumed
			Real64 const Power, // Power used
			Array1< Real64 > const & PartLoadCoef, // Pump Curve Coefficients
			int const PressureCurve_Index, // Pointer to a pump coefficient curve
			Real64 const PumpMassFlowRateMaxRPM, // Mass flow rate calculated from maximum rpm
			Real64 const PumpMassFlowRateMinRPM, // Mass flow rate calculated from minimum rpm
			Real64 const MinPhiValue, // Minimum value of Phi (from CurveManager)
			Real64 const MaxPhiValue, // Maximum value of Phi (from CurveManager)
			Real64 const ImpellerDiameter, // Pump Impeller Diameter [m]
			Real64 const RotSpeed_RPM, // Rotational speed used for input in revs/min
			Real64 const RotSpeed, // Rotational speed for calculations in revs/sec
			bool const PumpInitFlag,
			bool const PumpOneTimeFlag,
			bool const CheckEquipName,
			bool const HasVFD,
			PumpVFDControlData const & VFD,
			bool const OneTimePressureWarning,
			bool const HeatLossesToZone, // if true then pump losses added to surrounding zone
			int const ZoneNum, // index for zone surrounding pump
			Real64 const SkinLossRadFraction, // radiative split for skin losses to zone
			bool const LoopSolverOverwriteFlag
		) :
			Name( Name ),
			PumpSchedule( PumpSchedule ),
			PressureCurve_Name( PressureCurve_Name ),
			PumpType( PumpType ),
			TypeOf_Num( TypeOf_Num ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			PumpControl( PumpControl ),
			PumpScheduleIndex( PumpScheduleIndex ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			SequencingScheme( SequencingScheme ),
			FluidIndex( FluidIndex ),
			NumPumpsInBank( NumPumpsInBank ),
			PowerErrIndex1( PowerErrIndex1 ),
			PowerErrIndex2( PowerErrIndex2 ),
			MinVolFlowRateFrac( MinVolFlowRateFrac ),
			NomVolFlowRate( NomVolFlowRate ),
			NomVolFlowRateWasAutoSized( NomVolFlowRateWasAutoSized ),
			MassFlowRateMax( MassFlowRateMax ),
			EMSMassFlowOverrideOn( EMSMassFlowOverrideOn ),
			EMSMassFlowValue( EMSMassFlowValue ),
			NomSteamVolFlowRate( NomSteamVolFlowRate ),
			MinVolFlowRate( MinVolFlowRate ),
			MassFlowRateMin( MassFlowRateMin ),
			NomPumpHead( NomPumpHead ),
			EMSPressureOverrideOn( EMSPressureOverrideOn ),
			EMSPressureOverrideValue( EMSPressureOverrideValue ),
			NomPowerUse( NomPowerUse ),
			NomPowerUseWasAutoSized( NomPowerUseWasAutoSized ),
			MotorEffic( MotorEffic ),
			PumpEffic( PumpEffic ),
			FracMotorLossToFluid( FracMotorLossToFluid ),
			Energy( Energy ),
			Power( Power ),
			PartLoadCoef( 4, PartLoadCoef ),
			PressureCurve_Index( PressureCurve_Index ),
			PumpMassFlowRateMaxRPM( PumpMassFlowRateMaxRPM ),
			PumpMassFlowRateMinRPM( PumpMassFlowRateMinRPM ),
			MinPhiValue( MinPhiValue ),
			MaxPhiValue( MaxPhiValue ),
			ImpellerDiameter( ImpellerDiameter ),
			RotSpeed_RPM( RotSpeed_RPM ),
			RotSpeed( RotSpeed ),
			PumpInitFlag( PumpInitFlag ),
			PumpOneTimeFlag( PumpOneTimeFlag ),
			CheckEquipName( CheckEquipName ),
			HasVFD( HasVFD ),
			VFD( VFD ),
			OneTimePressureWarning( OneTimePressureWarning ),
			HeatLossesToZone( HeatLossesToZone ),
			ZoneNum( ZoneNum ),
			SkinLossRadFraction( SkinLossRadFraction ),
			LoopSolverOverwriteFlag( LoopSolverOverwriteFlag )
		{}

	};

	struct ReportVars
	{
		// Members
		int NumPumpsOperating; // Used in pump bank. reports how many pumps are ON
		Real64 PumpMassFlowRate; // Mass flow rate of the pump
		Real64 PumpHeattoFluid; // Heat transfer from pump to fluid (W)
		Real64 PumpHeattoFluidEnergy; // Pump Energy dissipated into fluid stream
		Real64 OutletTemp; // Pump outlet temperature
		Real64 ShaftPower; // Power input at the shaft
		Real64 ZoneTotalGainRate; // total pump skin losses to zone (W)
		Real64 ZoneTotalGainEnergy; // total pump skin losses to zone energy (J)
		Real64 ZoneConvGainRate; // pump skin losses convecting to zone air (W)
		Real64 ZoneRadGainRate; // pump skin losses radiating to inside of zone (W)

		// Default Constructor
		ReportVars() :
			NumPumpsOperating( 0 ),
			PumpMassFlowRate( 0.0 ),
			PumpHeattoFluid( 0.0 ),
			PumpHeattoFluidEnergy( 0.0 ),
			OutletTemp( 0.0 ),
			ShaftPower( 0.0 ),
			ZoneTotalGainRate( 0.0 ),
			ZoneTotalGainEnergy( 0.0 ),
			ZoneConvGainRate( 0.0 ),
			ZoneRadGainRate( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			int const NumPumpsOperating, // Used in pump bank. reports how many pumps are ON
			Real64 const PumpMassFlowRate, // Mass flow rate of the pump
			Real64 const PumpHeattoFluid, // Heat transfer from pump to fluid (W)
			Real64 const PumpHeattoFluidEnergy, // Pump Energy dissipated into fluid stream
			Real64 const OutletTemp, // Pump outlet temperature
			Real64 const ShaftPower, // Power input at the shaft
			Real64 const ZoneTotalGainRate, // total pump skin losses to zone (W)
			Real64 const ZoneTotalGainEnergy, // total pump skin losses to zone energy (J)
			Real64 const ZoneConvGainRate, // pump skin losses convecting to zone air (W)
			Real64 const ZoneRadGainRate // pump skin losses radiating to inside of zone (W)
		) :
			NumPumpsOperating( NumPumpsOperating ),
			PumpMassFlowRate( PumpMassFlowRate ),
			PumpHeattoFluid( PumpHeattoFluid ),
			PumpHeattoFluidEnergy( PumpHeattoFluidEnergy ),
			OutletTemp( OutletTemp ),
			ShaftPower( ShaftPower ),
			ZoneTotalGainRate( ZoneTotalGainRate ),
			ZoneTotalGainEnergy( ZoneTotalGainEnergy ),
			ZoneConvGainRate( ZoneConvGainRate ),
			ZoneRadGainRate( ZoneRadGainRate )
		{}

	};

	// Object Data
	extern Array1D< PumpSpecs > PumpEquip;
	extern Array1D< ReportVars > PumpEquipReport;

	// Functions

	void
	SimPumps(
		std::string const & PumpName, // Name of pump to be managed
		int const LoopNum, // Plant loop number
		Real64 const FlowRequest, // requested flow from adjacent demand side
		bool & PumpRunning, // .TRUE. if the loop pump is actually operating
		int & PumpIndex,
		Real64 & PumpHeat
	);

	//*************************************************************************!

	//*************************************************************************!

	void
	GetPumpInput();

	//*************************************************************************!

	//*************************************************************************!

	void
	InitializePumps( int const PumpNum );

	//*************************************************************************!

	//*************************************************************************!

	void
	SetupPumpMinMaxFlows(
		int const LoopNum,
		int const PumpNum
	);

	//*************************************************************************!

	//*************************************************************************!

	void
	CalcPumps(
		int const PumpNum,
		Real64 const FlowRequest,
		bool & PumpRunning
	);

	//*************************************************************************!

	//*************************************************************************!

	void
	SizePump( int const PumpNum );

	//*************************************************************************!

	//*************************************************************************!

	void
	ReportPumps( int const PumpNum );

	//*************************************************************************!

	//*************************************************************************!

	void
	PumpDataForTable( int const NumPump );

	//*************************************************************************!

	void
	GetRequiredMassFlowRate(
		int const LoopNum,
		int const PumpNum,
		Real64 const InletNodeMassFlowRate,
		Real64 & ActualFlowRate,
		Real64 & PumpMinMassFlowRateVFDRange,
		Real64 & PumpMaxMassFlowRateVFDRange
	);

	//=================================================================================================!

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

} // Pumps

} // EnergyPlus

#endif
