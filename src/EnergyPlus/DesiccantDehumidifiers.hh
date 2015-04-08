#ifndef DesiccantDehumidifiers_hh_INCLUDED
#define DesiccantDehumidifiers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DesiccantDehumidifiers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Desiccant dehumidifier type
	extern int const Solid; // DESICCANT DEHUMIDIFIER:SOLID = 1
	extern int const Generic; // DESICCANT DEHUMIDIFIER = 2
	//  Desiccant heat exchanger type
	extern int const BalancedHX; // HeatExchanger:Desiccant:BalancedFlow = 1
	// Desiccant control type
	extern int const FixedHumratBypass; // FIXED LEAVING HUMRAT SETPOINT:BYPASS = 1
	extern int const NodeHumratBypass; // NODE LEAVING HUMRAT SETPOINT:BYPASS  = 2
	// Preheat selection
	extern int const No; // Condenser waste heat NOT reclaimed for desiccant regeneration
	extern int const Yes; // Condenser waste heat reclaimed for desiccant regeneration
	// Performance Model
	extern int const PM_Default; // Performance Model = default
	extern int const PM_UserCurves; // Performance Model = user curve

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumDesicDehums; // number of desiccant dehumidifiers of all types
	extern int NumSolidDesicDehums; // number of solid desiccant dehumidifiers
	extern int NumGenericDesicDehums; // number of generic desiccant dehumidifiers
	extern Real64 TempSteamIn; // steam coil steam inlet temperature

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

	// Name Public routines, optionally name Private routines within this module

	// Types

	struct DesiccantDehumidifierData
	{
		// Members
		// User Input data
		std::string Name; // unique name of component
		std::string Sched; // name of availability schedule
		std::string RegenCoilType; // type of regen coil
		std::string RegenCoilName; // name of regen coil
		std::string RegenFanType; // type of regen fan
		std::string RegenFanName; // name of regen fan
		int PerformanceModel_Num; // type of performance model, default or user curves
		int ProcAirInNode; // process air inlet node of dehumidifier
		int ProcAirOutNode; // process air outlet node of dehumidifier
		int RegenAirInNode; // regen air inlet node of dehumidifier
		// (initially set to conditions entering regen heating coil)
		int RegenAirOutNode; // regen air outlet node of dehumidifier
		int RegenFanInNode; // regen fan inlet node
		int ControlType; // type of controls
		Real64 HumRatSet; // humidity ratio setpoint [kg water / kg air]
		Real64 NomProcAirVolFlow; // nominal process air flow rate [m3/s]
		Real64 NomProcAirVel; // nominal process air velocity [m/s]
		Real64 NomRotorPower; // rotor power consumption at full output [W]
		int RegenCoilIndex; // Index for regen coil
		int RegenFanIndex; // Index for regen fan
		int ProcDryBulbCurvefTW; // number of process leaving dry bulb f(edb,ew) curve
		int ProcDryBulbCurvefV; // number of process leaving dry bulb f(v) curve
		int ProcHumRatCurvefTW; // number of process leaving humidity ratio f(edb,ew) curve
		int ProcHumRatCurvefV; // number of process leaving humidity ratio f(v) curve
		int RegenEnergyCurvefTW; // number of regen energy f(edb,ew) curve
		int RegenEnergyCurvefV; // number of regen energy f(v) curve
		int RegenVelCurvefTW; // number of regen velocity f(edb,ew) curve
		int RegenVelCurvefV; // number of regen velocity f(v) curve
		Real64 NomRegenTemp; // nominal regen temperature for regen energy curve [C]
		// Possible future inputs, hardwired for now depending on which performance model is in use, unit off if out of bounds
		Real64 MinProcAirInTemp; // min allowable process inlet air temperature [C]
		Real64 MaxProcAirInTemp; // max allowable process inlet air temperature [C]
		Real64 MinProcAirInHumRat; // min allowable process inlet air humidity ratio [kg water / kg air]
		Real64 MaxProcAirInHumRat; // max allowable process inlet air humidity ratio [kg water / kg air]
		// Internal Data
		int SchedPtr; // index of availability schedule
		Real64 NomProcAirMassFlow; // nominal process air mass flow rate [kg/s]
		Real64 NomRegenAirMassFlow; // nominal regeneration air mass flow rate [kg/s]
		Real64 ProcAirInTemp; // process inlet air temperature [C]
		Real64 ProcAirInHumRat; // process inlet air humidity ratio [kg water / kg air]
		Real64 ProcAirInEnthalpy; // process inlet air specific enthalpy [J/kg]
		Real64 ProcAirInMassFlowRate; // process inlet air mass flow rate [kg/s]
		Real64 ProcAirOutTemp; // process outlet air temperature [C]
		Real64 ProcAirOutHumRat; // process outlet air humidity ratio [kg water / kg air]
		Real64 ProcAirOutEnthalpy; // process outlet air specific enthalpy [J/kg]
		Real64 ProcAirOutMassFlowRate; // process outlet air mass flow rate [kg/s]
		Real64 RegenAirInTemp; // regen inlet air temperature [C]
		Real64 RegenAirInHumRat; // regen inlet air humidity ratio [kg water / kg air]
		Real64 RegenAirInEnthalpy; // regen inlet air specific enthalpy [J/kg]
		Real64 RegenAirInMassFlowRate; // regen inlet air mass flow rate [kg/s]
		Real64 RegenAirVel; // regen air velocity [m/s]
		std::string DehumType; // Type of desiccant dehumidifier
		int DehumTypeCode; // Type of desiccant dehumidifier, integer code
		Real64 WaterRemove; // water removed [kg]
		Real64 WaterRemoveRate; // water removal rate [kg/s]
		Real64 SpecRegenEnergy; // specific regen energy [J/kg of water removed]
		Real64 QRegen; // regen energy rate requested from regen coil [W]
		Real64 RegenEnergy; // regen energy requested from regen coil [J]
		Real64 ElecUseEnergy; // electricity consumption [J]
		Real64 ElecUseRate; // electricity consumption rate [W]
		Real64 PartLoad; // fraction of dehumidification capacity required to meet setpoint
		int RegenCapErrorIndex1; // recurring error message index for insufficient regen coil capacity
		int RegenCapErrorIndex2; // recurring error message index for insufficient regen coil capacity
		int RegenCapErrorIndex3; // recurring error message index for insufficient regen coil capacity
		int RegenCapErrorIndex4; // recurring error message index for insufficient regen coil capacity
		int RegenFanErrorIndex1; // recurring error message index for incorrect regen fan flow
		int RegenFanErrorIndex2; // recurring error message index for incorrect regen fan flow
		int RegenFanErrorIndex3; // recurring error message index for incorrect regen fan flow
		int RegenFanErrorIndex4; // recurring error message index for incorrect regen fan flow
		// structure elements unique to generic desiccant dehumidifier
		std::string HXType; // type of desiccant heat exchanger
		std::string HXName; // name of desiccant heat exchanger
		int HXTypeNum; // parameter number of desiccant heat exchanger
		std::string ExhaustFanCurveObject; // exhaust fan curve object
		std::string CoolingCoilType; // type of cooling coil used with desiccant heat exchanger
		std::string CoolingCoilName; // name of cooling coil used with desiccant heat exchanger
		int Preheat; // determine condenser waste heat usage for pre heating regen air
		Real64 RegenSetPointTemp; // heating set-point for regeneration air [C]
		Real64 ExhaustFanMaxVolFlowRate; // exhaust fan maximum allowable air flow rate [m3/s]
		Real64 ExhaustFanMaxMassFlowRate; // exhaust fan maximum allowable air mass flow rate [kg/s]
		Real64 ExhaustFanMaxPower; // exhaust fan maximum allowable power [W]
		Real64 ExhaustFanPower; // exhaust fan power for reporting [W]
		Real64 ExhaustFanElecConsumption; // exhaust fan electric consumption for reporting [J]
		Real64 CompanionCoilCapacity; // DX coil capacity for dehumidifier companion cooling coil [W]
		int RegenFanPlacement; // placement of the fan used for regeneration air flow
		int ControlNodeNum; // node number of control node
		int ExhaustFanCurveIndex; // exhaust fan curve object index
		int CompIndex; // index of HX component to call simheatrecovery
		int CoolingCoilOutletNode; // node number of cooling coil outlet node
		int RegenFanOutNode; // fan outlet node number mined from regen fan object
		int RegenCoilInletNode; // regen heating coil inlet node number mined from regen heater object
		int RegenCoilOutletNode; // regen heating coil outlet node number mined from regen heater object
		int HXProcInNode; // process inlet node num mined from desiccant heat exchanger object
		int HXProcOutNode; // process outlet node num mined from desiccant heat exchanger object
		int HXRegenInNode; // regen inlet node number mined from desiccant heat exchanger object
		int HXRegenOutNode; // regen outlet node number mined from desiccant heat exchanger object
		int CondenserInletNode; // regen outlet node number mined from desiccant heat exchanger object
		int DXCoilIndex; // DX Coil index mined from coil object
		int ErrCount; // error count
		int ErrIndex1; // error index
		int CoilUpstreamOfProcessSide; // used to determine if process inlet is pre-cooled
		bool RegenInletIsOutsideAirNode; // regen inlet is connected to an outside air node
		int RegenCoilType_Num; // type number of regen coil
		int CoilControlNode; // heating coil hot water or steam inlet node
		int CoilOutletNode; // outlet node for water coil
		int LoopNum; // plant loop index for water heating coil
		int LoopSide; // plant loop side  index for water heating coil
		int BranchNum; // plant loop branch index for water heating coil
		int CompNum; // plant loop component index for water heating coil
		int HotWaterCoilMaxIterIndex; // Index to recurring warning message
		int HotWaterCoilMaxIterIndex2; // Index to recurring warning message
		Real64 MaxCoilFluidFlow; // hot water or steam mass flow rate regen. heating coil [kg/s]
		Real64 RegenCoilCapacity; // hot water or steam coil operating capacity [W]

		// Default Constructor
		DesiccantDehumidifierData() :
			PerformanceModel_Num( 0 ),
			ProcAirInNode( 0 ),
			ProcAirOutNode( 0 ),
			RegenAirInNode( 0 ),
			RegenAirOutNode( 0 ),
			RegenFanInNode( 0 ),
			ControlType( 0 ),
			HumRatSet( 0.0 ),
			NomProcAirVolFlow( 0.0 ),
			NomProcAirVel( 0.0 ),
			NomRotorPower( 0.0 ),
			RegenCoilIndex( 0 ),
			RegenFanIndex( 0 ),
			ProcDryBulbCurvefTW( 0 ),
			ProcDryBulbCurvefV( 0 ),
			ProcHumRatCurvefTW( 0 ),
			ProcHumRatCurvefV( 0 ),
			RegenEnergyCurvefTW( 0 ),
			RegenEnergyCurvefV( 0 ),
			RegenVelCurvefTW( 0 ),
			RegenVelCurvefV( 0 ),
			NomRegenTemp( 121.0 ),
			MinProcAirInTemp( -73.3 ),
			MaxProcAirInTemp( 65.6 ),
			MinProcAirInHumRat( 0.0 ),
			MaxProcAirInHumRat( 0.21273 ),
			SchedPtr( 0 ),
			NomProcAirMassFlow( 0.0 ),
			NomRegenAirMassFlow( 0.0 ),
			ProcAirInTemp( 0.0 ),
			ProcAirInHumRat( 0.0 ),
			ProcAirInEnthalpy( 0.0 ),
			ProcAirInMassFlowRate( 0.0 ),
			ProcAirOutTemp( 0.0 ),
			ProcAirOutHumRat( 0.0 ),
			ProcAirOutEnthalpy( 0.0 ),
			ProcAirOutMassFlowRate( 0.0 ),
			RegenAirInTemp( 0.0 ),
			RegenAirInHumRat( 0.0 ),
			RegenAirInEnthalpy( 0.0 ),
			RegenAirInMassFlowRate( 0.0 ),
			RegenAirVel( 0.0 ),
			DehumTypeCode( 0 ),
			WaterRemove( 0.0 ),
			WaterRemoveRate( 0.0 ),
			SpecRegenEnergy( 0.0 ),
			QRegen( 0.0 ),
			RegenEnergy( 0.0 ),
			ElecUseEnergy( 0.0 ),
			ElecUseRate( 0.0 ),
			PartLoad( 0.0 ),
			RegenCapErrorIndex1( 0 ),
			RegenCapErrorIndex2( 0 ),
			RegenCapErrorIndex3( 0 ),
			RegenCapErrorIndex4( 0 ),
			RegenFanErrorIndex1( 0 ),
			RegenFanErrorIndex2( 0 ),
			RegenFanErrorIndex3( 0 ),
			RegenFanErrorIndex4( 0 ),
			HXTypeNum( 0 ),
			Preheat( 0 ),
			RegenSetPointTemp( 0.0 ),
			ExhaustFanMaxVolFlowRate( 0.0 ),
			ExhaustFanMaxMassFlowRate( 0.0 ),
			ExhaustFanMaxPower( 0.0 ),
			ExhaustFanPower( 0.0 ),
			ExhaustFanElecConsumption( 0.0 ),
			CompanionCoilCapacity( 0.0 ),
			RegenFanPlacement( 0 ),
			ControlNodeNum( 0 ),
			ExhaustFanCurveIndex( 0 ),
			CompIndex( 0 ),
			CoolingCoilOutletNode( 0 ),
			RegenFanOutNode( 0 ),
			RegenCoilInletNode( 0 ),
			RegenCoilOutletNode( 0 ),
			HXProcInNode( 0 ),
			HXProcOutNode( 0 ),
			HXRegenInNode( 0 ),
			HXRegenOutNode( 0 ),
			CondenserInletNode( 0 ),
			DXCoilIndex( 0 ),
			ErrCount( 0 ),
			ErrIndex1( 0 ),
			CoilUpstreamOfProcessSide( 0 ),
			RegenInletIsOutsideAirNode( false ),
			RegenCoilType_Num( 0 ),
			CoilControlNode( 0 ),
			CoilOutletNode( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			HotWaterCoilMaxIterIndex( 0 ),
			HotWaterCoilMaxIterIndex2( 0 ),
			MaxCoilFluidFlow( 0.0 ),
			RegenCoilCapacity( 0.0 )
		{}

		// Member Constructor
		DesiccantDehumidifierData(
			std::string const & Name, // unique name of component
			std::string const & Sched, // name of availability schedule
			std::string const & RegenCoilType, // type of regen coil
			std::string const & RegenCoilName, // name of regen coil
			std::string const & RegenFanType, // type of regen fan
			std::string const & RegenFanName, // name of regen fan
			int const PerformanceModel_Num, // type of performance model, default or user curves
			int const ProcAirInNode, // process air inlet node of dehumidifier
			int const ProcAirOutNode, // process air outlet node of dehumidifier
			int const RegenAirInNode, // regen air inlet node of dehumidifier
			int const RegenAirOutNode, // regen air outlet node of dehumidifier
			int const RegenFanInNode, // regen fan inlet node
			int const ControlType, // type of controls
			Real64 const HumRatSet, // humidity ratio setpoint [kg water / kg air]
			Real64 const NomProcAirVolFlow, // nominal process air flow rate [m3/s]
			Real64 const NomProcAirVel, // nominal process air velocity [m/s]
			Real64 const NomRotorPower, // rotor power consumption at full output [W]
			int const RegenCoilIndex, // Index for regen coil
			int const RegenFanIndex, // Index for regen fan
			int const ProcDryBulbCurvefTW, // number of process leaving dry bulb f(edb,ew) curve
			int const ProcDryBulbCurvefV, // number of process leaving dry bulb f(v) curve
			int const ProcHumRatCurvefTW, // number of process leaving humidity ratio f(edb,ew) curve
			int const ProcHumRatCurvefV, // number of process leaving humidity ratio f(v) curve
			int const RegenEnergyCurvefTW, // number of regen energy f(edb,ew) curve
			int const RegenEnergyCurvefV, // number of regen energy f(v) curve
			int const RegenVelCurvefTW, // number of regen velocity f(edb,ew) curve
			int const RegenVelCurvefV, // number of regen velocity f(v) curve
			Real64 const NomRegenTemp, // nominal regen temperature for regen energy curve [C]
			Real64 const MinProcAirInTemp, // min allowable process inlet air temperature [C]
			Real64 const MaxProcAirInTemp, // max allowable process inlet air temperature [C]
			Real64 const MinProcAirInHumRat, // min allowable process inlet air humidity ratio [kg water / kg air]
			Real64 const MaxProcAirInHumRat, // max allowable process inlet air humidity ratio [kg water / kg air]
			int const SchedPtr, // index of availability schedule
			Real64 const NomProcAirMassFlow, // nominal process air mass flow rate [kg/s]
			Real64 const NomRegenAirMassFlow, // nominal regeneration air mass flow rate [kg/s]
			Real64 const ProcAirInTemp, // process inlet air temperature [C]
			Real64 const ProcAirInHumRat, // process inlet air humidity ratio [kg water / kg air]
			Real64 const ProcAirInEnthalpy, // process inlet air specific enthalpy [J/kg]
			Real64 const ProcAirInMassFlowRate, // process inlet air mass flow rate [kg/s]
			Real64 const ProcAirOutTemp, // process outlet air temperature [C]
			Real64 const ProcAirOutHumRat, // process outlet air humidity ratio [kg water / kg air]
			Real64 const ProcAirOutEnthalpy, // process outlet air specific enthalpy [J/kg]
			Real64 const ProcAirOutMassFlowRate, // process outlet air mass flow rate [kg/s]
			Real64 const RegenAirInTemp, // regen inlet air temperature [C]
			Real64 const RegenAirInHumRat, // regen inlet air humidity ratio [kg water / kg air]
			Real64 const RegenAirInEnthalpy, // regen inlet air specific enthalpy [J/kg]
			Real64 const RegenAirInMassFlowRate, // regen inlet air mass flow rate [kg/s]
			Real64 const RegenAirVel, // regen air velocity [m/s]
			std::string const & DehumType, // Type of desiccant dehumidifier
			int const DehumTypeCode, // Type of desiccant dehumidifier, integer code
			Real64 const WaterRemove, // water removed [kg]
			Real64 const WaterRemoveRate, // water removal rate [kg/s]
			Real64 const SpecRegenEnergy, // specific regen energy [J/kg of water removed]
			Real64 const QRegen, // regen energy rate requested from regen coil [W]
			Real64 const RegenEnergy, // regen energy requested from regen coil [J]
			Real64 const ElecUseEnergy, // electricity consumption [J]
			Real64 const ElecUseRate, // electricity consumption rate [W]
			Real64 const PartLoad, // fraction of dehumidification capacity required to meet setpoint
			int const RegenCapErrorIndex1, // recurring error message index for insufficient regen coil capacity
			int const RegenCapErrorIndex2, // recurring error message index for insufficient regen coil capacity
			int const RegenCapErrorIndex3, // recurring error message index for insufficient regen coil capacity
			int const RegenCapErrorIndex4, // recurring error message index for insufficient regen coil capacity
			int const RegenFanErrorIndex1, // recurring error message index for incorrect regen fan flow
			int const RegenFanErrorIndex2, // recurring error message index for incorrect regen fan flow
			int const RegenFanErrorIndex3, // recurring error message index for incorrect regen fan flow
			int const RegenFanErrorIndex4, // recurring error message index for incorrect regen fan flow
			std::string const & HXType, // type of desiccant heat exchanger
			std::string const & HXName, // name of desiccant heat exchanger
			int const HXTypeNum, // parameter number of desiccant heat exchanger
			std::string const & ExhaustFanCurveObject, // exhaust fan curve object
			std::string const & CoolingCoilType, // type of cooling coil used with desiccant heat exchanger
			std::string const & CoolingCoilName, // name of cooling coil used with desiccant heat exchanger
			int const Preheat, // determine condenser waste heat usage for pre heating regen air
			Real64 const RegenSetPointTemp, // heating set-point for regeneration air [C]
			Real64 const ExhaustFanMaxVolFlowRate, // exhaust fan maximum allowable air flow rate [m3/s]
			Real64 const ExhaustFanMaxMassFlowRate, // exhaust fan maximum allowable air mass flow rate [kg/s]
			Real64 const ExhaustFanMaxPower, // exhaust fan maximum allowable power [W]
			Real64 const ExhaustFanPower, // exhaust fan power for reporting [W]
			Real64 const ExhaustFanElecConsumption, // exhaust fan electric consumption for reporting [J]
			Real64 const CompanionCoilCapacity, // DX coil capacity for dehumidifier companion cooling coil [W]
			int const RegenFanPlacement, // placement of the fan used for regeneration air flow
			int const ControlNodeNum, // node number of control node
			int const ExhaustFanCurveIndex, // exhaust fan curve object index
			int const CompIndex, // index of HX component to call simheatrecovery
			int const CoolingCoilOutletNode, // node number of cooling coil outlet node
			int const RegenFanOutNode, // fan outlet node number mined from regen fan object
			int const RegenCoilInletNode, // regen heating coil inlet node number mined from regen heater object
			int const RegenCoilOutletNode, // regen heating coil outlet node number mined from regen heater object
			int const HXProcInNode, // process inlet node num mined from desiccant heat exchanger object
			int const HXProcOutNode, // process outlet node num mined from desiccant heat exchanger object
			int const HXRegenInNode, // regen inlet node number mined from desiccant heat exchanger object
			int const HXRegenOutNode, // regen outlet node number mined from desiccant heat exchanger object
			int const CondenserInletNode, // regen outlet node number mined from desiccant heat exchanger object
			int const DXCoilIndex, // DX Coil index mined from coil object
			int const ErrCount, // error count
			int const ErrIndex1, // error index
			int const CoilUpstreamOfProcessSide, // used to determine if process inlet is pre-cooled
			bool const RegenInletIsOutsideAirNode, // regen inlet is connected to an outside air node
			int const RegenCoilType_Num, // type number of regen coil
			int const CoilControlNode, // heating coil hot water or steam inlet node
			int const CoilOutletNode, // outlet node for water coil
			int const LoopNum, // plant loop index for water heating coil
			int const LoopSide, // plant loop side  index for water heating coil
			int const BranchNum, // plant loop branch index for water heating coil
			int const CompNum, // plant loop component index for water heating coil
			int const HotWaterCoilMaxIterIndex, // Index to recurring warning message
			int const HotWaterCoilMaxIterIndex2, // Index to recurring warning message
			Real64 const MaxCoilFluidFlow, // hot water or steam mass flow rate regen. heating coil [kg/s]
			Real64 const RegenCoilCapacity // hot water or steam coil operating capacity [W]
		) :
			Name( Name ),
			Sched( Sched ),
			RegenCoilType( RegenCoilType ),
			RegenCoilName( RegenCoilName ),
			RegenFanType( RegenFanType ),
			RegenFanName( RegenFanName ),
			PerformanceModel_Num( PerformanceModel_Num ),
			ProcAirInNode( ProcAirInNode ),
			ProcAirOutNode( ProcAirOutNode ),
			RegenAirInNode( RegenAirInNode ),
			RegenAirOutNode( RegenAirOutNode ),
			RegenFanInNode( RegenFanInNode ),
			ControlType( ControlType ),
			HumRatSet( HumRatSet ),
			NomProcAirVolFlow( NomProcAirVolFlow ),
			NomProcAirVel( NomProcAirVel ),
			NomRotorPower( NomRotorPower ),
			RegenCoilIndex( RegenCoilIndex ),
			RegenFanIndex( RegenFanIndex ),
			ProcDryBulbCurvefTW( ProcDryBulbCurvefTW ),
			ProcDryBulbCurvefV( ProcDryBulbCurvefV ),
			ProcHumRatCurvefTW( ProcHumRatCurvefTW ),
			ProcHumRatCurvefV( ProcHumRatCurvefV ),
			RegenEnergyCurvefTW( RegenEnergyCurvefTW ),
			RegenEnergyCurvefV( RegenEnergyCurvefV ),
			RegenVelCurvefTW( RegenVelCurvefTW ),
			RegenVelCurvefV( RegenVelCurvefV ),
			NomRegenTemp( NomRegenTemp ),
			MinProcAirInTemp( MinProcAirInTemp ),
			MaxProcAirInTemp( MaxProcAirInTemp ),
			MinProcAirInHumRat( MinProcAirInHumRat ),
			MaxProcAirInHumRat( MaxProcAirInHumRat ),
			SchedPtr( SchedPtr ),
			NomProcAirMassFlow( NomProcAirMassFlow ),
			NomRegenAirMassFlow( NomRegenAirMassFlow ),
			ProcAirInTemp( ProcAirInTemp ),
			ProcAirInHumRat( ProcAirInHumRat ),
			ProcAirInEnthalpy( ProcAirInEnthalpy ),
			ProcAirInMassFlowRate( ProcAirInMassFlowRate ),
			ProcAirOutTemp( ProcAirOutTemp ),
			ProcAirOutHumRat( ProcAirOutHumRat ),
			ProcAirOutEnthalpy( ProcAirOutEnthalpy ),
			ProcAirOutMassFlowRate( ProcAirOutMassFlowRate ),
			RegenAirInTemp( RegenAirInTemp ),
			RegenAirInHumRat( RegenAirInHumRat ),
			RegenAirInEnthalpy( RegenAirInEnthalpy ),
			RegenAirInMassFlowRate( RegenAirInMassFlowRate ),
			RegenAirVel( RegenAirVel ),
			DehumType( DehumType ),
			DehumTypeCode( DehumTypeCode ),
			WaterRemove( WaterRemove ),
			WaterRemoveRate( WaterRemoveRate ),
			SpecRegenEnergy( SpecRegenEnergy ),
			QRegen( QRegen ),
			RegenEnergy( RegenEnergy ),
			ElecUseEnergy( ElecUseEnergy ),
			ElecUseRate( ElecUseRate ),
			PartLoad( PartLoad ),
			RegenCapErrorIndex1( RegenCapErrorIndex1 ),
			RegenCapErrorIndex2( RegenCapErrorIndex2 ),
			RegenCapErrorIndex3( RegenCapErrorIndex3 ),
			RegenCapErrorIndex4( RegenCapErrorIndex4 ),
			RegenFanErrorIndex1( RegenFanErrorIndex1 ),
			RegenFanErrorIndex2( RegenFanErrorIndex2 ),
			RegenFanErrorIndex3( RegenFanErrorIndex3 ),
			RegenFanErrorIndex4( RegenFanErrorIndex4 ),
			HXType( HXType ),
			HXName( HXName ),
			HXTypeNum( HXTypeNum ),
			ExhaustFanCurveObject( ExhaustFanCurveObject ),
			CoolingCoilType( CoolingCoilType ),
			CoolingCoilName( CoolingCoilName ),
			Preheat( Preheat ),
			RegenSetPointTemp( RegenSetPointTemp ),
			ExhaustFanMaxVolFlowRate( ExhaustFanMaxVolFlowRate ),
			ExhaustFanMaxMassFlowRate( ExhaustFanMaxMassFlowRate ),
			ExhaustFanMaxPower( ExhaustFanMaxPower ),
			ExhaustFanPower( ExhaustFanPower ),
			ExhaustFanElecConsumption( ExhaustFanElecConsumption ),
			CompanionCoilCapacity( CompanionCoilCapacity ),
			RegenFanPlacement( RegenFanPlacement ),
			ControlNodeNum( ControlNodeNum ),
			ExhaustFanCurveIndex( ExhaustFanCurveIndex ),
			CompIndex( CompIndex ),
			CoolingCoilOutletNode( CoolingCoilOutletNode ),
			RegenFanOutNode( RegenFanOutNode ),
			RegenCoilInletNode( RegenCoilInletNode ),
			RegenCoilOutletNode( RegenCoilOutletNode ),
			HXProcInNode( HXProcInNode ),
			HXProcOutNode( HXProcOutNode ),
			HXRegenInNode( HXRegenInNode ),
			HXRegenOutNode( HXRegenOutNode ),
			CondenserInletNode( CondenserInletNode ),
			DXCoilIndex( DXCoilIndex ),
			ErrCount( ErrCount ),
			ErrIndex1( ErrIndex1 ),
			CoilUpstreamOfProcessSide( CoilUpstreamOfProcessSide ),
			RegenInletIsOutsideAirNode( RegenInletIsOutsideAirNode ),
			RegenCoilType_Num( RegenCoilType_Num ),
			CoilControlNode( CoilControlNode ),
			CoilOutletNode( CoilOutletNode ),
			LoopNum( LoopNum ),
			LoopSide( LoopSide ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			HotWaterCoilMaxIterIndex( HotWaterCoilMaxIterIndex ),
			HotWaterCoilMaxIterIndex2( HotWaterCoilMaxIterIndex2 ),
			MaxCoilFluidFlow( MaxCoilFluidFlow ),
			RegenCoilCapacity( RegenCoilCapacity )
		{}

	};

	// Object Data
	extern Array1D< DesiccantDehumidifierData > DesicDehum;

	// Functions

	void
	SimDesiccantDehumidifier(
		std::string const & CompName, // name of the dehumidifier unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex
	);

	void
	GetDesiccantDehumidifierInput();

	void
	InitDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	ControlDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 & HumRatNeeded, // process air leaving humidity ratio set by controller [kg water/kg air]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep !unused1208
	);

	void
	CalcSolidDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 const HumRatNeeded, // process air leaving humidity ratio set by controller [kgWater/kgDryAir]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	CalcGenericDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 const HumRatNeeded, // process air leaving humidity ratio set by controller [kg water/kg air]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	UpdateDesiccantDehumidifier( int const DesicDehumNum ); // number of the current dehumidifier being simulated

	void
	ReportDesiccantDehumidifier( int const DesicDehumNum ); // number of the current dehumidifier being simulated

	void
	CalcNonDXHeatingCoils(
		int const DesicDehumNum, // Desiccant dehumidifier unit index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 const RegenCoilLoad, // heating coil load to be met (Watts)
		Optional< Real64 > RegenCoilLoadmet = _ // heating load met
	);

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	);

	//        End of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright © Gas Research Institute 2001.  All rights reserved.

	//     GRI LEGAL NOTICE
	//     Neither GRI, members of GRI nor any person or organization acting on behalf
	//     of either:

	//     A. Makes any warranty of representation, express or implied with respect to
	//        the accuracy, completness, or usefulness of the information contained in
	//        in this program, including any warranty of merchantability or fitness of
	//        any purpose with respoect to the program, or that the use of any
	//        information disclosed in this program may not infringe privately-owned
	//        rights, or

	//     B.  Assumes any liability with respoct to the use of, or for any and all
	//         damages resulting from the use of the program or any portion thereof or
	//         any information disclosed therein.

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

} // DesiccantDehumidifiers

} // EnergyPlus

#endif
