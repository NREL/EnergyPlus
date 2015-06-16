#ifndef Fans_hh_INCLUDED
#define Fans_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataHVACGlobals.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace Fans {

	// Using/Aliasing
	using DataHVACGlobals::MinFrac;
	using DataHVACGlobals::SystemAirflowSizing;

	// Data
	//MODULE PARAMETER DEFINITIONS
	// parameters describing fan types are contained in DataHVACGlobals (see USE statement above)

	extern int const ExhaustFanCoupledToAvailManagers;
	extern int const ExhaustFanDecoupledFromAvailManagers;

	//na

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumFans; // The Number of Fans found in the Input
	extern int NumNightVentPerf; // number of FAN:NIGHT VENT PERFORMANCE objects found in the input
	extern bool GetFanInputFlag; // Flag set to make sure you get input once
	extern Array1D_bool CheckEquipName;
	extern bool LocalTurnFansOn; // If True, overrides fan schedule and cycles ZoneHVAC component fans on
	extern bool LocalTurnFansOff; // If True, overrides fan schedule and LocalTurnFansOn and
	// forces ZoneHVAC comp fans off

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Types

	struct FanEquipConditions
	{
		// Members
		std::string FanName; // Name of the fan
		std::string FanType; // Type of Fan ie. Simple, Vane axial, Centrifugal, etc.
		std::string AvailSchedName; // Fan Operation Schedule
		int FanType_Num; // DataHVACGlobals fan type
		int AvailSchedPtrNum; // Pointer to the availability schedule
		Real64 InletAirMassFlowRate; // MassFlow through the Fan being Simulated [kg/Sec]
		Real64 OutletAirMassFlowRate;
		Real64 MaxAirFlowRate; // Max Specified Volume Flow Rate of Fan [m3/sec]
		bool MaxAirFlowRateIsAutosizable; // if true, then this type of fan could be autosize
		bool MaxAirFlowRateEMSOverrideOn; // if true, EMS wants to override fan size for Max Volume Flow Rate
		Real64 MaxAirFlowRateEMSOverrideValue; // EMS value to use for override of  Max Volume Flow Rate
		Real64 MinAirFlowRate; // Min Specified Volume Flow Rate of Fan [m3/sec]
		Real64 MaxAirMassFlowRate; // Max flow rate of fan in kg/sec
		Real64 MinAirMassFlowRate; // Min flow rate of fan in kg/sec
		int FanMinAirFracMethod; // parameter for what method is used for min flow fraction
		Real64 FanMinFrac; // Minimum fan air flow fraction
		Real64 FanFixedMin; // Absolute minimum fan air flow [m3/s]
		bool EMSMaxMassFlowOverrideOn; // if true, then EMS is calling to override mass flow
		Real64 EMSAirMassFlowValue; // value EMS is directing to use [kg/s]
		Real64 InletAirTemp;
		Real64 OutletAirTemp;
		Real64 InletAirHumRat;
		Real64 OutletAirHumRat;
		Real64 InletAirEnthalpy;
		Real64 OutletAirEnthalpy;
		Real64 FanPower; // Power of the Fan being Simulated [kW]
		Real64 FanEnergy; // Fan energy in [kJ]
		Real64 FanRuntimeFraction; // Fraction of the timestep that the fan operates
		Real64 DeltaTemp; // Temp Rise across the Fan [C]
		Real64 DeltaPress; // Delta Pressure Across the Fan [N/m2]
		bool EMSFanPressureOverrideOn; // if true, then EMS is calling to override
		Real64 EMSFanPressureValue; // EMS value for Delta Pressure Across the Fan [Pa]
		// cpw22Aug2010 Clarify meaning of "fan efficiency"
		//  REAL(r64)    :: FanEff                   =0.0d0  !Fan total efficiency; motor and mechanical
		Real64 FanEff; // Fan total system efficiency (fan*belt*motor*VFD)
		bool EMSFanEffOverrideOn; // if true, then EMS is calling to override
		Real64 EMSFanEffValue; // EMS value for total efficiency of the Fan, fraction on 0..1
		bool FaultyFilterFlag; // Indicate whether there is a fouling air filter corresponding to the fan
		int FaultyFilterIndex;  // Index of the fouling air filter corresponding to the fan
		Real64 MotEff; // Fan motor efficiency
		Real64 MotInAirFrac; // Fraction of motor heat entering air stream
		Array1D< Real64 > FanCoeff; // Fan Part Load Coefficients to match fan type
		// Mass Flow Rate Control Variables
		Real64 MassFlowRateMaxAvail;
		Real64 MassFlowRateMinAvail;
		Real64 RhoAirStdInit;
		int InletNodeNum;
		int OutletNodeNum;
		int NVPerfNum;
		int FanPowerRatAtSpeedRatCurveIndex;
		int FanEffRatioCurveIndex;
		std::string EndUseSubcategoryName;
		bool OneTimePowerRatioCheck; // one time flag used for error message
		bool OneTimeEffRatioCheck; // one time flag used for error message
		//cpw22Aug2010 Following added to support Fan Component Model input
		Real64 FanWheelDia; // Fan wheel outer diameter [m]
		Real64 FanOutletArea; // Fan outlet area [m2]
		Real64 FanMaxEff; // Fan maximum static efficiency [-]
		Real64 EuMaxEff; // Euler number at fan maximum static efficiency [-]
		Real64 FanMaxDimFlow; // Fan maximum dimensionless airflow [-]
		Real64 FanShaftPwrMax; // Fan shaft maximum input power [W]
		Real64 FanSizingFactor; // Fan sizing factor [-] cpw31Aug2010
		Real64 PulleyDiaRatio; // Motor/fan pulley diameter ratio [-]
		Real64 BeltMaxTorque; // Belt maximum torque [N-m]
		Real64 BeltSizingFactor; // Belt sizing factor [-]
		Real64 BeltTorqueTrans; // Belt fractional torque transition Region 1-2 [-]
		Real64 MotorMaxSpd; // Motor maximum speed [rpm]
		Real64 MotorMaxOutPwr; // Motor maximum output power [W]
		Real64 MotorSizingFactor; // Motor sizing factor [-]
		std::string VFDEffType; // VFD efficiency type [Speed or Power]
		Real64 VFDMaxOutPwr; // VFD maximum output power [W]
		Real64 VFDSizingFactor; // VFD sizing factor [-] cpw31Aug2010
		int PressRiseCurveIndex; // Fan pressure rise curve index
		int PressResetCurveIndex; // Duct static pressure reset curve index
		int PLFanEffNormCurveIndex; // Fan part-load efficiency (normal) curve index
		int PLFanEffStallCurveIndex; // Fan part-load efficiency (stall) curve index
		int DimFlowNormCurveIndex; // Fan dimensionless airflow (normal) curve index
		int DimFlowStallCurveIndex; // Fan dimensionless airflow (stall) curve index
		int BeltMaxEffCurveIndex; // Belt maximum efficiency curve index
		int PLBeltEffReg1CurveIndex; // Belt part-load efficiency (Region 1) curve index
		int PLBeltEffReg2CurveIndex; // Belt part-load efficiency (Region 2) curve index
		int PLBeltEffReg3CurveIndex; // Belt part-load efficiency (Region 3) curve index
		int MotorMaxEffCurveIndex; // Motor maximum efficiency curve index
		int PLMotorEffCurveIndex; // Motor part-load efficiency curve index
		int VFDEffCurveIndex; // VFD efficiency curve index
		//cpw22Aug2010 Following added to support Fan Component Model calculated values
		Real64 DeltaPressTot; // Total pressure rise across fan [N/m2]
		Real64 FanAirPower; // Air power for fan being Simulated [W]
		Real64 FanSpd; // Fan shaft rotational speed [rpm]
		Real64 FanTrq; // Fan shaft torque [N-m]
		Real64 FanWheelEff; // Fan efficiency (mechanical)
		Real64 FanShaftPower; // Shaft input power for fan being Simulated [W]
		Real64 BeltMaxEff; // Belt maximum efficiency (mechanical) cpw31Aug2010
		Real64 BeltEff; // Belt efficiency (mechanical)
		Real64 BeltInputPower; // Belt input power for fan being Simulated [W]
		Real64 MotorMaxEff; // Motor maximum efficiency (electrical) cpw31Aug2010
		Real64 MotorInputPower; // Motor input power for fan being Simulated [W]
		Real64 VFDEff; // VFD efficiency (electrical)
		Real64 VFDInputPower; // VFD input power for fan being Simulated [W]
		Real64 MaxFanPowerEncountered; // Maximum VFD input power encountered [W]
		//zone exhaust fan
		int FlowFractSchedNum; // schedule index flow rate modifier schedule
		int AvailManagerMode; // mode for how exhaust fan should react to availability managers
		int MinTempLimitSchedNum; // schedule index minimum temperature limit
		int BalancedFractSchedNum; // schedule index portion recirculated
		Real64 UnbalancedOutletMassFlowRate;
		Real64 BalancedOutletMassFlowRate;

		// Default Constructor
		FanEquipConditions() :
			FanType_Num( 0 ),
			AvailSchedPtrNum( 0 ),
			InletAirMassFlowRate( 0.0 ),
			OutletAirMassFlowRate( 0.0 ),
			MaxAirFlowRate( 0.0 ),
			MaxAirFlowRateIsAutosizable( false ),
			MaxAirFlowRateEMSOverrideOn( false ),
			MaxAirFlowRateEMSOverrideValue( 0.0 ),
			MinAirFlowRate( 0.0 ),
			MaxAirMassFlowRate( 0.0 ),
			MinAirMassFlowRate( 0.0 ),
			FanMinAirFracMethod( MinFrac ),
			FanMinFrac( 0.0 ),
			FanFixedMin( 0.0 ),
			EMSMaxMassFlowOverrideOn( false ),
			EMSAirMassFlowValue( 0.0 ),
			InletAirTemp( 0.0 ),
			OutletAirTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			OutletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			FanPower( 0.0 ),
			FanEnergy( 0.0 ),
			FanRuntimeFraction( 0.0 ),
			DeltaTemp( 0.0 ),
			DeltaPress( 0.0 ),
			EMSFanPressureOverrideOn( false ),
			EMSFanPressureValue( 0.0 ),
			FanEff( 0.0 ),
			EMSFanEffOverrideOn( false ),
			EMSFanEffValue( 0.0 ),
		    FaultyFilterFlag( false ),
		    FaultyFilterIndex( 0 ),
			MotEff( 0.0 ),
			MotInAirFrac( 0.0 ),
			FanCoeff( 5, 0.0 ),
			MassFlowRateMaxAvail( 0.0 ),
			MassFlowRateMinAvail( 0.0 ),
			RhoAirStdInit( 0.0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			NVPerfNum( 0 ),
			FanPowerRatAtSpeedRatCurveIndex( 0 ),
			FanEffRatioCurveIndex( 0 ),
			OneTimePowerRatioCheck( true ),
			OneTimeEffRatioCheck( true ),
			FanWheelDia( 0.0 ),
			FanOutletArea( 0.0 ),
			FanMaxEff( 0.0 ),
			EuMaxEff( 0.0 ),
			FanMaxDimFlow( 0.0 ),
			FanShaftPwrMax( 0.0 ),
			FanSizingFactor( 0.0 ),
			PulleyDiaRatio( 0.0 ),
			BeltMaxTorque( 0.0 ),
			BeltSizingFactor( 0.0 ),
			BeltTorqueTrans( 0.0 ),
			MotorMaxSpd( 0.0 ),
			MotorMaxOutPwr( 0.0 ),
			MotorSizingFactor( 0.0 ),
			VFDMaxOutPwr( 0.0 ),
			VFDSizingFactor( 0.0 ),
			PressRiseCurveIndex( 0 ),
			PressResetCurveIndex( 0 ),
			PLFanEffNormCurveIndex( 0 ),
			PLFanEffStallCurveIndex( 0 ),
			DimFlowNormCurveIndex( 0 ),
			DimFlowStallCurveIndex( 0 ),
			BeltMaxEffCurveIndex( 0 ),
			PLBeltEffReg1CurveIndex( 0 ),
			PLBeltEffReg2CurveIndex( 0 ),
			PLBeltEffReg3CurveIndex( 0 ),
			MotorMaxEffCurveIndex( 0 ),
			PLMotorEffCurveIndex( 0 ),
			VFDEffCurveIndex( 0 ),
			DeltaPressTot( 0.0 ),
			FanAirPower( 0.0 ),
			FanSpd( 0.0 ),
			FanTrq( 0.0 ),
			FanWheelEff( 0.0 ),
			FanShaftPower( 0.0 ),
			BeltMaxEff( 0.0 ),
			BeltEff( 0.0 ),
			BeltInputPower( 0.0 ),
			MotorMaxEff( 0.0 ),
			MotorInputPower( 0.0 ),
			VFDEff( 0.0 ),
			VFDInputPower( 0.0 ),
			MaxFanPowerEncountered( 0.0 ),
			FlowFractSchedNum( 0 ),
			AvailManagerMode( 0 ),
			MinTempLimitSchedNum( 0 ),
			BalancedFractSchedNum( 0 ),
			UnbalancedOutletMassFlowRate( 0.0 ),
			BalancedOutletMassFlowRate( 0.0 )
		{}

		// Member Constructor
		FanEquipConditions(
			std::string const & FanName, // Name of the fan
			std::string const & FanType, // Type of Fan ie. Simple, Vane axial, Centrifugal, etc.
			std::string const & AvailSchedName, // Fan Operation Schedule
			int const FanType_Num, // DataHVACGlobals fan type
			int const AvailSchedPtrNum, // Pointer to the availability schedule
			Real64 const InletAirMassFlowRate, // MassFlow through the Fan being Simulated [kg/Sec]
			Real64 const OutletAirMassFlowRate,
			Real64 const MaxAirFlowRate, // Max Specified Volume Flow Rate of Fan [m3/sec]
			bool const MaxAirFlowRateIsAutosizable, // if true, then this type of fan could be autosize
			bool const MaxAirFlowRateEMSOverrideOn, // if true, EMS wants to override fan size for Max Volume Flow Rate
			Real64 const MaxAirFlowRateEMSOverrideValue, // EMS value to use for override of  Max Volume Flow Rate
			Real64 const MinAirFlowRate, // Min Specified Volume Flow Rate of Fan [m3/sec]
			Real64 const MaxAirMassFlowRate, // Max flow rate of fan in kg/sec
			Real64 const MinAirMassFlowRate, // Min flow rate of fan in kg/sec
			int const FanMinAirFracMethod, // parameter for what method is used for min flow fraction
			Real64 const FanMinFrac, // Minimum fan air flow fraction
			Real64 const FanFixedMin, // Absolute minimum fan air flow [m3/s]
			bool const EMSMaxMassFlowOverrideOn, // if true, then EMS is calling to override mass flow
			Real64 const EMSAirMassFlowValue, // value EMS is directing to use [kg/s]
			Real64 const InletAirTemp,
			Real64 const OutletAirTemp,
			Real64 const InletAirHumRat,
			Real64 const OutletAirHumRat,
			Real64 const InletAirEnthalpy,
			Real64 const OutletAirEnthalpy,
			Real64 const FanPower, // Power of the Fan being Simulated [kW]
			Real64 const FanEnergy, // Fan energy in [kJ]
			Real64 const FanRuntimeFraction, // Fraction of the timestep that the fan operates
			Real64 const DeltaTemp, // Temp Rise across the Fan [C]
			Real64 const DeltaPress, // Delta Pressure Across the Fan [N/m2]
			bool const EMSFanPressureOverrideOn, // if true, then EMS is calling to override
			Real64 const EMSFanPressureValue, // EMS value for Delta Pressure Across the Fan [Pa]
			Real64 const FanEff, // Fan total system efficiency (fan*belt*motor*VFD)
			bool const EMSFanEffOverrideOn, // if true, then EMS is calling to override
			Real64 const EMSFanEffValue, // EMS value for total efficiency of the Fan, fraction on 0..1
		    bool FaultyFilterFlag, // Indicate whether there is a fouling air filter corresponding to the fan
		    int FaultyFilterIndex,  // Index of the fouling air filter corresponding to the fan
			Real64 const MotEff, // Fan motor efficiency
			Real64 const MotInAirFrac, // Fraction of motor heat entering air stream
			Array1< Real64 > const & FanCoeff, // Fan Part Load Coefficients to match fan type
			Real64 const MassFlowRateMaxAvail,
			Real64 const MassFlowRateMinAvail,
			Real64 const RhoAirStdInit,
			int const InletNodeNum,
			int const OutletNodeNum,
			int const NVPerfNum,
			int const FanPowerRatAtSpeedRatCurveIndex,
			int const FanEffRatioCurveIndex,
			std::string const & EndUseSubcategoryName,
			bool const OneTimePowerRatioCheck, // one time flag used for error message
			bool const OneTimeEffRatioCheck, // one time flag used for error message
			Real64 const FanWheelDia, // Fan wheel outer diameter [m]
			Real64 const FanOutletArea, // Fan outlet area [m2]
			Real64 const FanMaxEff, // Fan maximum static efficiency [-]
			Real64 const EuMaxEff, // Euler number at fan maximum static efficiency [-]
			Real64 const FanMaxDimFlow, // Fan maximum dimensionless airflow [-]
			Real64 const FanShaftPwrMax, // Fan shaft maximum input power [W]
			Real64 const FanSizingFactor, // Fan sizing factor [-] cpw31Aug2010
			Real64 const PulleyDiaRatio, // Motor/fan pulley diameter ratio [-]
			Real64 const BeltMaxTorque, // Belt maximum torque [N-m]
			Real64 const BeltSizingFactor, // Belt sizing factor [-]
			Real64 const BeltTorqueTrans, // Belt fractional torque transition Region 1-2 [-]
			Real64 const MotorMaxSpd, // Motor maximum speed [rpm]
			Real64 const MotorMaxOutPwr, // Motor maximum output power [W]
			Real64 const MotorSizingFactor, // Motor sizing factor [-]
			std::string const & VFDEffType, // VFD efficiency type [Speed or Power]
			Real64 const VFDMaxOutPwr, // VFD maximum output power [W]
			Real64 const VFDSizingFactor, // VFD sizing factor [-] cpw31Aug2010
			int const PressRiseCurveIndex, // Fan pressure rise curve index
			int const PressResetCurveIndex, // Duct static pressure reset curve index
			int const PLFanEffNormCurveIndex, // Fan part-load efficiency (normal) curve index
			int const PLFanEffStallCurveIndex, // Fan part-load efficiency (stall) curve index
			int const DimFlowNormCurveIndex, // Fan dimensionless airflow (normal) curve index
			int const DimFlowStallCurveIndex, // Fan dimensionless airflow (stall) curve index
			int const BeltMaxEffCurveIndex, // Belt maximum efficiency curve index
			int const PLBeltEffReg1CurveIndex, // Belt part-load efficiency (Region 1) curve index
			int const PLBeltEffReg2CurveIndex, // Belt part-load efficiency (Region 2) curve index
			int const PLBeltEffReg3CurveIndex, // Belt part-load efficiency (Region 3) curve index
			int const MotorMaxEffCurveIndex, // Motor maximum efficiency curve index
			int const PLMotorEffCurveIndex, // Motor part-load efficiency curve index
			int const VFDEffCurveIndex, // VFD efficiency curve index
			Real64 const DeltaPressTot, // Total pressure rise across fan [N/m2]
			Real64 const FanAirPower, // Air power for fan being Simulated [W]
			Real64 const FanSpd, // Fan shaft rotational speed [rpm]
			Real64 const FanTrq, // Fan shaft torque [N-m]
			Real64 const FanWheelEff, // Fan efficiency (mechanical)
			Real64 const FanShaftPower, // Shaft input power for fan being Simulated [W]
			Real64 const BeltMaxEff, // Belt maximum efficiency (mechanical) cpw31Aug2010
			Real64 const BeltEff, // Belt efficiency (mechanical)
			Real64 const BeltInputPower, // Belt input power for fan being Simulated [W]
			Real64 const MotorMaxEff, // Motor maximum efficiency (electrical) cpw31Aug2010
			Real64 const MotorInputPower, // Motor input power for fan being Simulated [W]
			Real64 const VFDEff, // VFD efficiency (electrical)
			Real64 const VFDInputPower, // VFD input power for fan being Simulated [W]
			Real64 const MaxFanPowerEncountered, // Maximum VFD input power encountered [W]
			int const FlowFractSchedNum, // schedule index flow rate modifier schedule
			int const AvailManagerMode, // mode for how exhaust fan should react to availability managers
			int const MinTempLimitSchedNum, // schedule index minimum temperature limit
			int const BalancedFractSchedNum, // schedule index portion recirculated
			Real64 const UnbalancedOutletMassFlowRate,
			Real64 const BalancedOutletMassFlowRate
		) :
			FanName( FanName ),
			FanType( FanType ),
			AvailSchedName( AvailSchedName ),
			FanType_Num( FanType_Num ),
			AvailSchedPtrNum( AvailSchedPtrNum ),
			InletAirMassFlowRate( InletAirMassFlowRate ),
			OutletAirMassFlowRate( OutletAirMassFlowRate ),
			MaxAirFlowRate( MaxAirFlowRate ),
			MaxAirFlowRateIsAutosizable( MaxAirFlowRateIsAutosizable ),
			MaxAirFlowRateEMSOverrideOn( MaxAirFlowRateEMSOverrideOn ),
			MaxAirFlowRateEMSOverrideValue( MaxAirFlowRateEMSOverrideValue ),
			MinAirFlowRate( MinAirFlowRate ),
			MaxAirMassFlowRate( MaxAirMassFlowRate ),
			MinAirMassFlowRate( MinAirMassFlowRate ),
			FanMinAirFracMethod( FanMinAirFracMethod ),
			FanMinFrac( FanMinFrac ),
			FanFixedMin( FanFixedMin ),
			EMSMaxMassFlowOverrideOn( EMSMaxMassFlowOverrideOn ),
			EMSAirMassFlowValue( EMSAirMassFlowValue ),
			InletAirTemp( InletAirTemp ),
			OutletAirTemp( OutletAirTemp ),
			InletAirHumRat( InletAirHumRat ),
			OutletAirHumRat( OutletAirHumRat ),
			InletAirEnthalpy( InletAirEnthalpy ),
			OutletAirEnthalpy( OutletAirEnthalpy ),
			FanPower( FanPower ),
			FanEnergy( FanEnergy ),
			FanRuntimeFraction( FanRuntimeFraction ),
			DeltaTemp( DeltaTemp ),
			DeltaPress( DeltaPress ),
			EMSFanPressureOverrideOn( EMSFanPressureOverrideOn ),
			EMSFanPressureValue( EMSFanPressureValue ),
			FanEff( FanEff ),
			EMSFanEffOverrideOn( EMSFanEffOverrideOn ),
			EMSFanEffValue( EMSFanEffValue ),
		    FaultyFilterFlag( FaultyFilterFlag ),
		    FaultyFilterIndex( FaultyFilterIndex ),
			MotEff( MotEff ),
			MotInAirFrac( MotInAirFrac ),
			FanCoeff( 5, FanCoeff ),
			MassFlowRateMaxAvail( MassFlowRateMaxAvail ),
			MassFlowRateMinAvail( MassFlowRateMinAvail ),
			RhoAirStdInit( RhoAirStdInit ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			NVPerfNum( NVPerfNum ),
			FanPowerRatAtSpeedRatCurveIndex( FanPowerRatAtSpeedRatCurveIndex ),
			FanEffRatioCurveIndex( FanEffRatioCurveIndex ),
			EndUseSubcategoryName( EndUseSubcategoryName ),
			OneTimePowerRatioCheck( OneTimePowerRatioCheck ),
			OneTimeEffRatioCheck( OneTimeEffRatioCheck ),
			FanWheelDia( FanWheelDia ),
			FanOutletArea( FanOutletArea ),
			FanMaxEff( FanMaxEff ),
			EuMaxEff( EuMaxEff ),
			FanMaxDimFlow( FanMaxDimFlow ),
			FanShaftPwrMax( FanShaftPwrMax ),
			FanSizingFactor( FanSizingFactor ),
			PulleyDiaRatio( PulleyDiaRatio ),
			BeltMaxTorque( BeltMaxTorque ),
			BeltSizingFactor( BeltSizingFactor ),
			BeltTorqueTrans( BeltTorqueTrans ),
			MotorMaxSpd( MotorMaxSpd ),
			MotorMaxOutPwr( MotorMaxOutPwr ),
			MotorSizingFactor( MotorSizingFactor ),
			VFDEffType( VFDEffType ),
			VFDMaxOutPwr( VFDMaxOutPwr ),
			VFDSizingFactor( VFDSizingFactor ),
			PressRiseCurveIndex( PressRiseCurveIndex ),
			PressResetCurveIndex( PressResetCurveIndex ),
			PLFanEffNormCurveIndex( PLFanEffNormCurveIndex ),
			PLFanEffStallCurveIndex( PLFanEffStallCurveIndex ),
			DimFlowNormCurveIndex( DimFlowNormCurveIndex ),
			DimFlowStallCurveIndex( DimFlowStallCurveIndex ),
			BeltMaxEffCurveIndex( BeltMaxEffCurveIndex ),
			PLBeltEffReg1CurveIndex( PLBeltEffReg1CurveIndex ),
			PLBeltEffReg2CurveIndex( PLBeltEffReg2CurveIndex ),
			PLBeltEffReg3CurveIndex( PLBeltEffReg3CurveIndex ),
			MotorMaxEffCurveIndex( MotorMaxEffCurveIndex ),
			PLMotorEffCurveIndex( PLMotorEffCurveIndex ),
			VFDEffCurveIndex( VFDEffCurveIndex ),
			DeltaPressTot( DeltaPressTot ),
			FanAirPower( FanAirPower ),
			FanSpd( FanSpd ),
			FanTrq( FanTrq ),
			FanWheelEff( FanWheelEff ),
			FanShaftPower( FanShaftPower ),
			BeltMaxEff( BeltMaxEff ),
			BeltEff( BeltEff ),
			BeltInputPower( BeltInputPower ),
			MotorMaxEff( MotorMaxEff ),
			MotorInputPower( MotorInputPower ),
			VFDEff( VFDEff ),
			VFDInputPower( VFDInputPower ),
			MaxFanPowerEncountered( MaxFanPowerEncountered ),
			FlowFractSchedNum( FlowFractSchedNum ),
			AvailManagerMode( AvailManagerMode ),
			MinTempLimitSchedNum( MinTempLimitSchedNum ),
			BalancedFractSchedNum( BalancedFractSchedNum ),
			UnbalancedOutletMassFlowRate( UnbalancedOutletMassFlowRate ),
			BalancedOutletMassFlowRate( BalancedOutletMassFlowRate )
		{}

	};

	struct NightVentPerfData
	{
		// Members
		std::string FanName; // Name of the fan that will use this data
		Real64 FanEff; // Fan total efficiency; motor and mechanical
		Real64 DeltaPress; // Delta Pressure Across the Fan [N/m2]
		Real64 MaxAirFlowRate; // Max Specified Volume Flow Rate of Fan [m3/s]
		Real64 MaxAirMassFlowRate; // Max flow rate of fan in kg/sec
		Real64 MotEff; // Fan motor efficiency
		Real64 MotInAirFrac; // Fraction of motor heat entering air stream

		// Default Constructor
		NightVentPerfData() :
			FanEff( 0.0 ),
			DeltaPress( 0.0 ),
			MaxAirFlowRate( 0.0 ),
			MaxAirMassFlowRate( 0.0 ),
			MotEff( 0.0 ),
			MotInAirFrac( 0.0 )
		{}

		// Member Constructor
		NightVentPerfData(
			std::string const & FanName, // Name of the fan that will use this data
			Real64 const FanEff, // Fan total efficiency; motor and mechanical
			Real64 const DeltaPress, // Delta Pressure Across the Fan [N/m2]
			Real64 const MaxAirFlowRate, // Max Specified Volume Flow Rate of Fan [m3/s]
			Real64 const MaxAirMassFlowRate, // Max flow rate of fan in kg/sec
			Real64 const MotEff, // Fan motor efficiency
			Real64 const MotInAirFrac // Fraction of motor heat entering air stream
		) :
			FanName( FanName ),
			FanEff( FanEff ),
			DeltaPress( DeltaPress ),
			MaxAirFlowRate( MaxAirFlowRate ),
			MaxAirMassFlowRate( MaxAirMassFlowRate ),
			MotEff( MotEff ),
			MotInAirFrac( MotInAirFrac )
		{}

	};

	struct FanNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		FanNumericFieldData()
		{}

		// Member Constructor
		FanNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
		) :
			FieldNames( FieldNames )
		{}
	};

	// Object Data
	extern Array1D< FanEquipConditions > Fan;
	extern Array1D< NightVentPerfData > NightVentPerf;
	extern Array1D< FanNumericFieldData > FanNumericFields;

	// Functions

	void
	SimulateFanComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex,
		Optional< Real64 const > SpeedRatio = _,
		Optional_bool_const ZoneCompTurnFansOn = _, // Turn fans ON signal from ZoneHVAC component
		Optional_bool_const ZoneCompTurnFansOff = _, // Turn Fans OFF signal from ZoneHVAC component
		Optional< Real64 const > PressureRise = _ // Pressure difference to use for DeltaPress
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetFanInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitFan(
		int const FanNum,
		bool const FirstHVACIteration // unused1208
	);

	void
	SizeFan( int const FanNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimSimpleFan( int const FanNum );

	void
	SimVariableVolumeFan(
		int const FanNum,
		Optional< Real64 const > PressureRise = _
	);

	void
	SimOnOffFan(
		int const FanNum,
		Optional< Real64 const > SpeedRatio = _
	);

	void
	SimZoneExhaustFan( int const FanNum );

	//cpw22Aug2010 Added Component Model fan algorithm

	void
	SimComponentModelFan( int const FanNum );

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Fan Module
	// *****************************************************************************

	void
	UpdateFan( int const FanNum );

	//        End of Update subroutines for the Fan Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Fan Module
	// *****************************************************************************

	void
	ReportFan( int const FanNum );

	//        End of Reporting subroutines for the Fan Module
	// *****************************************************************************

	// Beginning of Utility subroutines for the Fan Module
	// *****************************************************************************

	void
	GetFanIndex(
		std::string const & FanName,
		int & FanIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType = _
	);

	void
	GetFanVolFlow(
		int const FanIndex,
		Real64 & FanVolFlow
	);

	void
	GetFanPower(
		int const FanIndex,
		Real64 & FanPower
	);

	void
	GetFanType(
		std::string const & FanName, // Fan name
		int & FanType, // returned fantype number
		bool & ErrorsFound, // error indicator
		Optional_string_const ThisObjectType = _, // parent object type (for error message)
		Optional_string_const ThisObjectName = _ // parent object name (for error message)
	);

	Real64
	GetFanDesignVolumeFlowRate(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound, // set to true if problem
		Optional_int_const FanIndex = _ // index to fan
	);

	int
	GetFanInletNode(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetFanOutletNode(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetFanAvailSchPtr(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetFanSpeedRatioCurveIndex(
		std::string & FanType, // must match fan types in this module (set if nonzero index passed)
		std::string & FanName, // must match fan names for the fan type (set if nonzero index passed)
		Optional_int IndexIn = _ // optional fan index if fan type and name are unknown or index needs setting
	);

	void
	SetFanData(
		int const FanNum, // Index of fan
		bool & ErrorsFound, // Set to true if certain errors found
		std::string const & FanName, // Name of fan
		Optional< Real64 const > MaxAirVolFlow = _, // Fan air volumetric flow rate    [m3/s]
		Optional< Real64 const > MinAirVolFlow = _ // Fan air volumetric flow rate    [m3/s]
	);

	Real64
	FanDesDT(
		int const FanNum, // index of fan in Fan array
		Real64 const FanVolFlow // fan volumetric flow rate [m3/s]
	);

	Real64 
	CalFaultyFanAirFlowReduction(
		std::string const FanName,            // Name of the Fan 
		Real64 const FanDesignAirFlowRate,    // Fan Design Volume Flow Rate [m3/s]
		Real64 const FanDesignDeltaPress,     // Fan Design Delta Pressure [Pa]
		Real64 const FanFaultyDeltaPressInc,  // Increase of Fan Delta Pressure in the Faulty Case [Pa]
		int const FanCurvePtr                 // Fan Curve Pointer
	);

	Real64
	FanDesHeatGain(
		int const FanNum, // index of fan in Fan array
		Real64 const FanVolFlow // fan volumetric flow rate [m3/s]
	);

	// End of Utility subroutines for the Fan Module
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

} // Fans

} // EnergyPlus

#endif
