#ifndef CondenserLoopTowers_hh_INCLUDED
#define CondenserLoopTowers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace CondenserLoopTowers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Empirical Model Type
	extern int const CoolToolsXFModel;
	// CoolTools counterflow model does not work properly. The empirical model seems flawed since the tower
	// operates in the free convection regime on the design day.
	// INTEGER, PARAMETER             :: CoolToolsCFModel     = 2
	extern int const CoolToolsUserDefined;
	extern int const YorkCalcModel;
	extern int const YorkCalcUserDefined;

	extern int const EvapLossByUserFactor;
	extern int const EvapLossByMoistTheory;

	extern int const BlowdownByConcentration;
	extern int const BlowdownBySchedule;

	extern std::string const cCoolingTower_SingleSpeed;
	extern std::string const cCoolingTower_TwoSpeed;
	extern std::string const cCoolingTower_VariableSpeed;
	extern std::string const cCoolingTower_VariableSpeedMerkel;

	extern int const PIM_NominalCapacity;
	extern int const PIM_UFactor;

	extern int const CoolingTower_SingleSpeed;
	extern int const CoolingTower_TwoSpeed;
	extern int const CoolingTower_VariableSpeed;
	extern int const CoolingTower_VariableSpeedMerkel;

	extern int const CapacityControl_FanCycling;
	extern int const CapacityControl_FluidBypass;

	extern int const CellCtrl_MinCell;
	extern int const CellCtrl_MaxCell;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumSimpleTowers; // Number of similar towers

	//? The following block of variables are used to carry model results for a tower instance
	//   across sim, update, and report routines.  Simulation manager must be careful
	//   in models with multiple towers.

	extern Real64 InletWaterTemp; // CW temperature at tower inlet
	extern Real64 OutletWaterTemp; // CW temperature at tower outlet
	extern int WaterInletNode; // Node number at tower inlet
	extern int WaterOutletNode; // Node number at tower outlet
	extern Real64 WaterMassFlowRate; // WaterMassFlowRate through tower
	//DSU this is plant level stuff now REAL(r64)         :: TowerMassFlowRateMax     = 0.0d0    ! Max Hardware Mass Flow Rate
	//DSU this is plant level stuff now REAL(r64)         :: TowerMassFlowRateMin     = 0.0d0    ! Min Hardware Mass Flow Rate
	//DSU this is plant level stuff now REAL(r64)         :: LoopMassFlowRateMaxAvail = 0.0d0    ! Max Loop Mass Flow Rate available
	//DSU this is plant level stuff now REAL(r64)         :: LoopMassFlowRateMinAvail = 0.0d0    ! Min Loop Mass Flow Rate available
	extern Real64 Qactual; // Tower heat transfer
	extern Real64 CTFanPower; // Tower fan power used
	extern Real64 AirFlowRateRatio; // Ratio of air flow rate through VS cooling tower to design air flow rate
	extern Real64 BasinHeaterPower; // Basin heater power use (W)
	extern Real64 WaterUsage; // Tower water usage (m3/s)
	extern Real64 FanCyclingRatio; // cycling ratio of tower fan when min fan speed provide to much capacity

	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopTowers

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Types

	struct Towerspecs
	{
		// Members
		std::string Name; // User identifier
		std::string TowerType; // Type of cooling tower
		int TowerType_Num;
		int PerformanceInputMethod_Num; // Method of entering tower performance: UA and Design Water
		//  Flow Rate, or Nominal Capacity
		std::string ModelCoeffObjectName; // Cooling Tower:Variable Speed Model Coefficient Object name
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // Simulate the machine at it's operating part load ratio
		Real64 DesignWaterFlowRate; // Design water flow rate through the tower [m3/s]
		Real64 DesignWaterFlowPerUnitNomCap; // scalable sizing factor for water flow per capacity [m3/s/W]
		Real64 DesWaterMassFlowRate; // Design water flow rate through the entire tower [kg/s]
		Real64 DesWaterMassFlowRatePerCell; // Design water flow rate per cell [Kg/s]
		Real64 HighSpeedAirFlowRate; // Air flow rate through tower at high speed [m3/s]
		Real64 DesignAirFlowPerUnitNomCap; // scalable sizing factor for air flow per capacity [m3/s/W]
		bool DefaultedDesignAirFlowScalingFactor; // true if user left input field blank for DesignAirFlowPerUnitNomCap
		Real64 HighSpeedFanPower; // Fan power at high fan speed [W]
		Real64 DesignFanPowerPerUnitNomCap; // scalable sizing factor for fan power per capacity [W/W]
		bool UAvaluesCompleted;
		Real64 HighSpeedTowerUA; // UA of tower at high fan speed [W/C]
		Real64 LowSpeedAirFlowRate; // Air flow rate through tower at low speed [m3/s]
		Real64 LowSpeedAirFlowRateSizingFactor; // sizing factor for low speed flow rate [ ]
		Real64 LowSpeedFanPower; // Fan power at low fan speed [W]
		Real64 LowSpeedFanPowerSizingFactor; // sizing factor for low speed fan power []
		Real64 LowSpeedTowerUA; // UA of tower at low fan speed [W/C]
		Real64 LowSpeedTowerUASizingFactor; // sizing factor for UA at low fan speed []
		Real64 FreeConvAirFlowRate; // Air flow rate through tower with fan off [m3/s]
		Real64 FreeConvAirFlowRateSizingFactor; // sizing factor for air flow at free conv []
		Real64 FreeConvTowerUA; // UA of tower with fan off [W/C]
		Real64 FreeConvTowerUASizingFactor; // sizing factor for UA at fre convection []
		Real64 DesignInletWB; // Design inlet air wet-bulb temperature (C)
		Real64 DesignApproach; // Design approach (outlet water temp minus inlet air wet-bulb temp (C)
		Real64 DesignRange; // Design range temperature (inlet water temp minus outlet water temp (C)
		Real64 MinimumVSAirFlowFrac; // Min air flow ratio (used for VS tower only, point where free conv occurs)
		Real64 CalibratedWaterFlowRate; // Water flow ratio required for model calibration
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree C below setpoint (W/C)
		Real64 BasinHeaterSetPointTemp; // setpoint temperature for basin heater operation (C)
		Real64 MakeupWaterDrift; // Makeup water flow rate fraction due to drift
		Real64 FreeConvectionCapacityFraction; // Percentage of tower capacity in free convection regime
		Real64 TowerMassFlowRateMultiplier; // Maximum tower flow rate is this multiplier times design flow rate
		Real64 HeatRejectCapNomCapSizingRatio; // ratio of actual cap to nominal capacity []
		Real64 TowerNominalCapacity; // Nominal capacity of the tower [W] with entering water at 35C (95F),
		//  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
		//  temp and 35C (95F) dry-bulb temp, and water flow
		//  rate of 5.382E-8 m3/s per watt (3 gpm/ton)
		Real64 TowerLowSpeedNomCap; // Nominal capacity of the tower [W] with entering water at 35C (95F),
		//  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
		//  temp and 35C (95F) dry-bulb temp, and water flow
		//  rate of 5.382E-8 m3/s per nominal capacity watt (3 gpm/ton)
		Real64 TowerLowSpeedNomCapSizingFactor; // sizing factor for low speed capacity []
		Real64 TowerFreeConvNomCap; // Nominal capacity of the tower [W] with entering water at 35C (95F),
		//  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
		//  temp and 35C (95F) dry-bulb temp, and water flow
		//  rate of 5.382E-8 m3/s per nominal capacity watt (3 gpm/ton)
		Real64 TowerFreeConvNomCapSizingFactor; // sizing factor for free conv capacity []
		Real64 SizFac; // sizing factor
		int WaterInletNodeNum; // Node number on the water inlet side of the tower
		int WaterOutletNodeNum; // Node number on the water outlet side of the tower
		int OutdoorAirInletNodeNum; // Node number of outdoor air inlet for the tower
		int TowerModelType; // Type of empirical model (1=CoolTools)
		int VSTower; // Index to a variable speed tower (otherwise = 0)
		int FanPowerfAirFlowCurve; // Index to fan power correlation curve for VS Towers
		int BlowDownSchedulePtr; // Pointer to blow down schedule
		int BasinHeaterSchedulePtr; // Pointer to basin heater schedule
		int HighMassFlowErrorCount; // Counter when mass flow rate is > Design*TowerMassFlowRateMultiplier
		int HighMassFlowErrorIndex; // Index for high mass flow recurring error message
		int OutletWaterTempErrorCount; // Counter when outlet water temperature is < minimum allowed temperature
		int OutletWaterTempErrorIndex; // Index for outlet water temperature recurring error message
		int SmallWaterMassFlowErrorCount; // Counter when water mass flow rate is very small
		int SmallWaterMassFlowErrorIndex; // Index for very small water mass flow rate recurring error message
		int WMFRLessThanMinAvailErrCount; // Counter when water mass flow rate is less than minimum available
		int WMFRLessThanMinAvailErrIndex; // Index for water mass flow rate less than minavail recurring message
		int WMFRGreaterThanMaxAvailErrCount; // Counter when water mass flow rate is greater than minimum available
		int WMFRGreaterThanMaxAvailErrIndex; // Index for water mass flow rate > minavail recurring message
		int CoolingTowerAFRRFailedCount; // Counter for air flow rate ratio out of bounds error
		int CoolingTowerAFRRFailedIndex; // Index for air flow rate ratio out of bounds error
		int SpeedSelected; // speed of the two-speed fan selected (0:ON;1:LOW;2:HIGH)
		//fluid bypass
		int CapacityControl; // Type of capacity control for single speed cooling tower:
		//  0 - FanCycling, 1 - FluidBypass
		Real64 BypassFraction; // Fraction of fluid bypass as a ratio of total fluid flow
		//  through the tower sump
		//multi cell tower
		int NumCell; // Number of cells in the cooling tower
		std::string CellCtrl; // Cell control type : either MaxCell or MinCell
		int CellCtrl_Num;
		int NumCellOn; // number of cells working
		Real64 MinFracFlowRate; // Minimal fraction of design flow/cell allowable
		Real64 MaxFracFlowRate; // Maximal ratio of design flow/cell allowable
		//begin water system interactions
		int EvapLossMode; // sets how tower water evaporation is modeled
		Real64 UserEvapLossFactor; // simple model [%/Delt C]
		Real64 DriftLossFraction;
		int BlowdownMode; // sets how tower water blowdown is modeled
		Real64 ConcentrationRatio; // ratio of solids in blowdown vs make up water
		int SchedIDBlowdown; // index "pointer" to schedule of blowdown in [m3/s]
		bool SuppliedByWaterSystem;
		int WaterTankID; // index "pointer" to WaterStorage structure
		int WaterTankDemandARRID; // index "pointer" to demand array inside WaterStorage structure
		//end water system variables
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		//Merkel VS model curves
		int UAModFuncAirFlowRatioCurvePtr; // curve index for UA modifier as a function of air flow ratio
		int UAModFuncWetBulbDiffCurvePtr; // curve index for UA modifier as a function of local wetbulb
		int UAModFuncWaterFlowRatioCurvePtr; // curve index for UA modifier as a function of water flow ratio
		bool SetpointIsOnOutlet; // if true look to outlet node of tower, if flase look to overall loop setpoint
		int VSMerkelAFRErrorIter; // error counter for regula falsi failed with max iterations, vs merkel model
		int VSMerkelAFRErrorFail; // error counter for regula falsi failed with limits exceeded, vs merkel model

		// Default Constructor
		Towerspecs() :
			TowerType_Num( 0 ),
			PerformanceInputMethod_Num( 0 ),
			Available( true ),
			ON( true ),
			DesignWaterFlowRate( 0.0 ),
			DesignWaterFlowPerUnitNomCap( 0.0 ),
			DesWaterMassFlowRate( 0.0 ),
			DesWaterMassFlowRatePerCell( 0.0 ),
			HighSpeedAirFlowRate( 0.0 ),
			DesignAirFlowPerUnitNomCap( 0.0 ),
			DefaultedDesignAirFlowScalingFactor( false ),
			HighSpeedFanPower( 0.0 ),
			DesignFanPowerPerUnitNomCap( 0.0 ),
			UAvaluesCompleted( false ),
			HighSpeedTowerUA( 0.0 ),
			LowSpeedAirFlowRate( 0.0 ),
			LowSpeedAirFlowRateSizingFactor( 0.0 ),
			LowSpeedFanPower( 0.0 ),
			LowSpeedFanPowerSizingFactor( 0.0 ),
			LowSpeedTowerUA( 0.0 ),
			LowSpeedTowerUASizingFactor( 0.0 ),
			FreeConvAirFlowRate( 0.0 ),
			FreeConvAirFlowRateSizingFactor( 0.0 ),
			FreeConvTowerUA( 0.0 ),
			FreeConvTowerUASizingFactor( 0.0 ),
			DesignInletWB( 0.0 ),
			DesignApproach( 0.0 ),
			DesignRange( 0.0 ),
			MinimumVSAirFlowFrac( 0.0 ),
			CalibratedWaterFlowRate( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 0.0 ),
			MakeupWaterDrift( 0.0 ),
			FreeConvectionCapacityFraction( 0.0 ),
			TowerMassFlowRateMultiplier( 0.0 ),
			HeatRejectCapNomCapSizingRatio( 1.25 ),
			TowerNominalCapacity( 0.0 ),
			TowerLowSpeedNomCap( 0.0 ),
			TowerLowSpeedNomCapSizingFactor( 0.0 ),
			TowerFreeConvNomCap( 0.0 ),
			TowerFreeConvNomCapSizingFactor( 0.0 ),
			SizFac( 0.0 ),
			WaterInletNodeNum( 0 ),
			WaterOutletNodeNum( 0 ),
			OutdoorAirInletNodeNum( 0 ),
			TowerModelType( 0 ),
			VSTower( 0 ),
			FanPowerfAirFlowCurve( 0 ),
			BlowDownSchedulePtr( 0 ),
			BasinHeaterSchedulePtr( 0 ),
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
			CoolingTowerAFRRFailedCount( 0 ),
			CoolingTowerAFRRFailedIndex( 0 ),
			SpeedSelected( 0 ),
			CapacityControl( 0 ),
			BypassFraction( 0.0 ),
			NumCell( 0 ),
			CellCtrl_Num( 0 ),
			NumCellOn( 0 ),
			MinFracFlowRate( 0.0 ),
			MaxFracFlowRate( 0.0 ),
			EvapLossMode( EvapLossByMoistTheory ),
			UserEvapLossFactor( 0.0 ),
			DriftLossFraction( 0.0 ),
			BlowdownMode( BlowdownByConcentration ),
			ConcentrationRatio( 0.0 ),
			SchedIDBlowdown( 0 ),
			SuppliedByWaterSystem( false ),
			WaterTankID( 0 ),
			WaterTankDemandARRID( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			UAModFuncAirFlowRatioCurvePtr( 0 ),
			UAModFuncWetBulbDiffCurvePtr( 0 ),
			UAModFuncWaterFlowRatioCurvePtr( 0 ),
			SetpointIsOnOutlet( false ),
			VSMerkelAFRErrorIter( 0 ),
			VSMerkelAFRErrorFail( 0 )
		{}

		// Member Constructor
		Towerspecs(
			std::string const & Name, // User identifier
			std::string const & TowerType, // Type of cooling tower
			int const TowerType_Num,
			int const PerformanceInputMethod_Num, // Method of entering tower performance: UA and Design Water
			std::string const & ModelCoeffObjectName, // Cooling Tower:Variable Speed Model Coefficient Object name
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // Simulate the machine at it's operating part load ratio
			Real64 const DesignWaterFlowRate, // Design water flow rate through the tower [m3/s]
			Real64 const DesignWaterFlowPerUnitNomCap, // scalable sizing factor for water flow per capacity [m3/s/W]
			Real64 const DesWaterMassFlowRate, // Design water flow rate through the entire tower [kg/s]
			Real64 const DesWaterMassFlowRatePerCell, // Design water flow rate per cell [Kg/s]
			Real64 const HighSpeedAirFlowRate, // Air flow rate through tower at high speed [m3/s]
			Real64 const DesignAirFlowPerUnitNomCap, // scalable sizing factor for air flow per capacity [m3/s/W]
			bool const DefaultedDesignAirFlowScalingFactor, // true if user left input field blank for DesignAirFlowPerUnitNomCap
			Real64 const HighSpeedFanPower, // Fan power at high fan speed [W]
			Real64 const DesignFanPowerPerUnitNomCap, // scalable sizing factor for fan power per capacity [W/W]
			bool const UAvaluesCompleted,
			Real64 const HighSpeedTowerUA, // UA of tower at high fan speed [W/C]
			Real64 const LowSpeedAirFlowRate, // Air flow rate through tower at low speed [m3/s]
			Real64 const LowSpeedAirFlowRateSizingFactor, // sizing factor for low speed flow rate [ ]
			Real64 const LowSpeedFanPower, // Fan power at low fan speed [W]
			Real64 const LowSpeedFanPowerSizingFactor, // sizing factor for low speed fan power []
			Real64 const LowSpeedTowerUA, // UA of tower at low fan speed [W/C]
			Real64 const LowSpeedTowerUASizingFactor, // sizing factor for UA at low fan speed []
			Real64 const FreeConvAirFlowRate, // Air flow rate through tower with fan off [m3/s]
			Real64 const FreeConvAirFlowRateSizingFactor, // sizing factor for air flow at free conv []
			Real64 const FreeConvTowerUA, // UA of tower with fan off [W/C]
			Real64 const FreeConvTowerUASizingFactor, // sizing factor for UA at fre convection []
			Real64 const DesignInletWB, // Design inlet air wet-bulb temperature (C)
			Real64 const DesignApproach, // Design approach (outlet water temp minus inlet air wet-bulb temp (C)
			Real64 const DesignRange, // Design range temperature (inlet water temp minus outlet water temp (C)
			Real64 const MinimumVSAirFlowFrac, // Min air flow ratio (used for VS tower only, point where free conv occurs)
			Real64 const CalibratedWaterFlowRate, // Water flow ratio required for model calibration
			Real64 const BasinHeaterPowerFTempDiff, // Basin heater capacity per degree C below setpoint (W/C)
			Real64 const BasinHeaterSetPointTemp, // setpoint temperature for basin heater operation (C)
			Real64 const MakeupWaterDrift, // Makeup water flow rate fraction due to drift
			Real64 const FreeConvectionCapacityFraction, // Percentage of tower capacity in free convection regime
			Real64 const TowerMassFlowRateMultiplier, // Maximum tower flow rate is this multiplier times design flow rate
			Real64 const HeatRejectCapNomCapSizingRatio, // ratio of actual cap to nominal capacity []
			Real64 const TowerNominalCapacity, // Nominal capacity of the tower [W] with entering water at 35C (95F),
			Real64 const TowerLowSpeedNomCap, // Nominal capacity of the tower [W] with entering water at 35C (95F),
			Real64 const TowerLowSpeedNomCapSizingFactor, // sizing factor for low speed capacity []
			Real64 const TowerFreeConvNomCap, // Nominal capacity of the tower [W] with entering water at 35C (95F),
			Real64 const TowerFreeConvNomCapSizingFactor, // sizing factor for free conv capacity []
			Real64 const SizFac, // sizing factor
			int const WaterInletNodeNum, // Node number on the water inlet side of the tower
			int const WaterOutletNodeNum, // Node number on the water outlet side of the tower
			int const OutdoorAirInletNodeNum, // Node number of outdoor air inlet for the tower
			int const TowerModelType, // Type of empirical model (1=CoolTools)
			int const VSTower, // Index to a variable speed tower (otherwise = 0)
			int const FanPowerfAirFlowCurve, // Index to fan power correlation curve for VS Towers
			int const BlowDownSchedulePtr, // Pointer to blow down schedule
			int const BasinHeaterSchedulePtr, // Pointer to basin heater schedule
			int const HighMassFlowErrorCount, // Counter when mass flow rate is > Design*TowerMassFlowRateMultiplier
			int const HighMassFlowErrorIndex, // Index for high mass flow recurring error message
			int const OutletWaterTempErrorCount, // Counter when outlet water temperature is < minimum allowed temperature
			int const OutletWaterTempErrorIndex, // Index for outlet water temperature recurring error message
			int const SmallWaterMassFlowErrorCount, // Counter when water mass flow rate is very small
			int const SmallWaterMassFlowErrorIndex, // Index for very small water mass flow rate recurring error message
			int const WMFRLessThanMinAvailErrCount, // Counter when water mass flow rate is less than minimum available
			int const WMFRLessThanMinAvailErrIndex, // Index for water mass flow rate less than minavail recurring message
			int const WMFRGreaterThanMaxAvailErrCount, // Counter when water mass flow rate is greater than minimum available
			int const WMFRGreaterThanMaxAvailErrIndex, // Index for water mass flow rate > minavail recurring message
			int const CoolingTowerAFRRFailedCount, // Counter for air flow rate ratio out of bounds error
			int const CoolingTowerAFRRFailedIndex, // Index for air flow rate ratio out of bounds error
			int const SpeedSelected, // speed of the two-speed fan selected (0:ON;1:LOW;2:HIGH)
			int const CapacityControl, // Type of capacity control for single speed cooling tower:
			Real64 const BypassFraction, // Fraction of fluid bypass as a ratio of total fluid flow
			int const NumCell, // Number of cells in the cooling tower
			std::string const & CellCtrl, // Cell control type : either MaxCell or MinCell
			int const CellCtrl_Num,
			int const NumCellOn, // number of cells working
			Real64 const MinFracFlowRate, // Minimal fraction of design flow/cell allowable
			Real64 const MaxFracFlowRate, // Maximal ratio of design flow/cell allowable
			int const EvapLossMode, // sets how tower water evaporation is modeled
			Real64 const UserEvapLossFactor, // simple model [%/Delt C]
			Real64 const DriftLossFraction,
			int const BlowdownMode, // sets how tower water blowdown is modeled
			Real64 const ConcentrationRatio, // ratio of solids in blowdown vs make up water
			int const SchedIDBlowdown, // index "pointer" to schedule of blowdown in [m3/s]
			bool const SuppliedByWaterSystem,
			int const WaterTankID, // index "pointer" to WaterStorage structure
			int const WaterTankDemandARRID, // index "pointer" to demand array inside WaterStorage structure
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum,
			int const UAModFuncAirFlowRatioCurvePtr, // curve index for UA modifier as a function of air flow ratio
			int const UAModFuncWetBulbDiffCurvePtr, // curve index for UA modifier as a function of local wetbulb
			int const UAModFuncWaterFlowRatioCurvePtr, // curve index for UA modifier as a function of water flow ratio
			bool const SetpointIsOnOutlet, // if true look to outlet node of tower, if flase look to overall loop setpoint
			int const VSMerkelAFRErrorIter, // error counter for regula falsi failed with max iterations, vs merkel model
			int const VSMerkelAFRErrorFail // error counter for regula falsi failed with limits exceeded, vs merkel model
		) :
			Name( Name ),
			TowerType( TowerType ),
			TowerType_Num( TowerType_Num ),
			PerformanceInputMethod_Num( PerformanceInputMethod_Num ),
			ModelCoeffObjectName( ModelCoeffObjectName ),
			Available( Available ),
			ON( ON ),
			DesignWaterFlowRate( DesignWaterFlowRate ),
			DesignWaterFlowPerUnitNomCap( DesignWaterFlowPerUnitNomCap ),
			DesWaterMassFlowRate( DesWaterMassFlowRate ),
			DesWaterMassFlowRatePerCell( DesWaterMassFlowRatePerCell ),
			HighSpeedAirFlowRate( HighSpeedAirFlowRate ),
			DesignAirFlowPerUnitNomCap( DesignAirFlowPerUnitNomCap ),
			DefaultedDesignAirFlowScalingFactor( DefaultedDesignAirFlowScalingFactor ),
			HighSpeedFanPower( HighSpeedFanPower ),
			DesignFanPowerPerUnitNomCap( DesignFanPowerPerUnitNomCap ),
			UAvaluesCompleted( UAvaluesCompleted ),
			HighSpeedTowerUA( HighSpeedTowerUA ),
			LowSpeedAirFlowRate( LowSpeedAirFlowRate ),
			LowSpeedAirFlowRateSizingFactor( LowSpeedAirFlowRateSizingFactor ),
			LowSpeedFanPower( LowSpeedFanPower ),
			LowSpeedFanPowerSizingFactor( LowSpeedFanPowerSizingFactor ),
			LowSpeedTowerUA( LowSpeedTowerUA ),
			LowSpeedTowerUASizingFactor( LowSpeedTowerUASizingFactor ),
			FreeConvAirFlowRate( FreeConvAirFlowRate ),
			FreeConvAirFlowRateSizingFactor( FreeConvAirFlowRateSizingFactor ),
			FreeConvTowerUA( FreeConvTowerUA ),
			FreeConvTowerUASizingFactor( FreeConvTowerUASizingFactor ),
			DesignInletWB( DesignInletWB ),
			DesignApproach( DesignApproach ),
			DesignRange( DesignRange ),
			MinimumVSAirFlowFrac( MinimumVSAirFlowFrac ),
			CalibratedWaterFlowRate( CalibratedWaterFlowRate ),
			BasinHeaterPowerFTempDiff( BasinHeaterPowerFTempDiff ),
			BasinHeaterSetPointTemp( BasinHeaterSetPointTemp ),
			MakeupWaterDrift( MakeupWaterDrift ),
			FreeConvectionCapacityFraction( FreeConvectionCapacityFraction ),
			TowerMassFlowRateMultiplier( TowerMassFlowRateMultiplier ),
			HeatRejectCapNomCapSizingRatio( HeatRejectCapNomCapSizingRatio ),
			TowerNominalCapacity( TowerNominalCapacity ),
			TowerLowSpeedNomCap( TowerLowSpeedNomCap ),
			TowerLowSpeedNomCapSizingFactor( TowerLowSpeedNomCapSizingFactor ),
			TowerFreeConvNomCap( TowerFreeConvNomCap ),
			TowerFreeConvNomCapSizingFactor( TowerFreeConvNomCapSizingFactor ),
			SizFac( SizFac ),
			WaterInletNodeNum( WaterInletNodeNum ),
			WaterOutletNodeNum( WaterOutletNodeNum ),
			OutdoorAirInletNodeNum( OutdoorAirInletNodeNum ),
			TowerModelType( TowerModelType ),
			VSTower( VSTower ),
			FanPowerfAirFlowCurve( FanPowerfAirFlowCurve ),
			BlowDownSchedulePtr( BlowDownSchedulePtr ),
			BasinHeaterSchedulePtr( BasinHeaterSchedulePtr ),
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
			CoolingTowerAFRRFailedCount( CoolingTowerAFRRFailedCount ),
			CoolingTowerAFRRFailedIndex( CoolingTowerAFRRFailedIndex ),
			SpeedSelected( SpeedSelected ),
			CapacityControl( CapacityControl ),
			BypassFraction( BypassFraction ),
			NumCell( NumCell ),
			CellCtrl( CellCtrl ),
			CellCtrl_Num( CellCtrl_Num ),
			NumCellOn( NumCellOn ),
			MinFracFlowRate( MinFracFlowRate ),
			MaxFracFlowRate( MaxFracFlowRate ),
			EvapLossMode( EvapLossMode ),
			UserEvapLossFactor( UserEvapLossFactor ),
			DriftLossFraction( DriftLossFraction ),
			BlowdownMode( BlowdownMode ),
			ConcentrationRatio( ConcentrationRatio ),
			SchedIDBlowdown( SchedIDBlowdown ),
			SuppliedByWaterSystem( SuppliedByWaterSystem ),
			WaterTankID( WaterTankID ),
			WaterTankDemandARRID( WaterTankDemandARRID ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			UAModFuncAirFlowRatioCurvePtr( UAModFuncAirFlowRatioCurvePtr ),
			UAModFuncWetBulbDiffCurvePtr( UAModFuncWetBulbDiffCurvePtr ),
			UAModFuncWaterFlowRatioCurvePtr( UAModFuncWaterFlowRatioCurvePtr ),
			SetpointIsOnOutlet( SetpointIsOnOutlet ),
			VSMerkelAFRErrorIter( VSMerkelAFRErrorIter ),
			VSMerkelAFRErrorFail( VSMerkelAFRErrorFail )
		{}

	};

	struct TowerInletConds
	{
		// Members
		Real64 WaterTemp; // Tower water inlet temperature (C)
		Real64 AirTemp; // Tower air inlet dry-bulb temperature (C)
		Real64 AirWetBulb; // Tower air inlet wet-bulb temperature (C)
		Real64 AirPress; // Tower air barometric pressure
		Real64 AirHumRat; // Tower air inlet humidity ratio (kg/kg)

		// Default Constructor
		TowerInletConds() :
			WaterTemp( 0.0 ),
			AirTemp( 0.0 ),
			AirWetBulb( 0.0 ),
			AirPress( 0.0 ),
			AirHumRat( 0.0 )
		{}

		// Member Constructor
		TowerInletConds(
			Real64 const WaterTemp, // Tower water inlet temperature (C)
			Real64 const AirTemp, // Tower air inlet dry-bulb temperature (C)
			Real64 const AirWetBulb, // Tower air inlet wet-bulb temperature (C)
			Real64 const AirPress, // Tower air barometric pressure
			Real64 const AirHumRat // Tower air inlet humidity ratio (kg/kg)
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
		Real64 InletWaterTemp; // Tower inlet water temperature (C)
		Real64 OutletWaterTemp; // Tower outlet water temperature (C)
		Real64 WaterMassFlowRate; // Tower water mass flow rate (m3/s)
		Real64 Qactual; // Tower heat rejection rate (W)
		Real64 FanPower; // Tower fan power (W)
		Real64 FanEnergy; // Tower fan energy consumption (J)
		Real64 AirFlowRatio; // Air flow ratio through variable speed cooling tower
		Real64 BasinHeaterPower; // Basin heater power (W)
		Real64 BasinHeaterConsumption; // Basin heater energy consumption (J)
		Real64 WaterAmountUsed; // Tower make up water usage (m3)
		Real64 FanCyclingRatio; // cycling ratio of tower fan when min fan speed provide too much capacity (for VFD)
		Real64 EvaporationVdot;
		Real64 EvaporationVol;
		Real64 DriftVdot;
		Real64 DriftVol;
		Real64 BlowdownVdot;
		Real64 BlowdownVol;
		Real64 MakeUpVdot;
		Real64 MakeUpVol;
		Real64 TankSupplyVdot;
		Real64 TankSupplyVol;
		Real64 StarvedMakeUpVdot;
		Real64 StarvedMakeUpVol;
		Real64 BypassFraction; // Added for fluid bypass
		int NumCellOn; // for multi-cell tower
		int SpeedSelected; // Speed selected for the two speed tower

		// Default Constructor
		ReportVars() :
			InletWaterTemp( 0.0 ),
			OutletWaterTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			Qactual( 0.0 ),
			FanPower( 0.0 ),
			FanEnergy( 0.0 ),
			AirFlowRatio( 0.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 ),
			WaterAmountUsed( 0.0 ),
			FanCyclingRatio( 0.0 ),
			EvaporationVdot( 0.0 ),
			EvaporationVol( 0.0 ),
			DriftVdot( 0.0 ),
			DriftVol( 0.0 ),
			BlowdownVdot( 0.0 ),
			BlowdownVol( 0.0 ),
			MakeUpVdot( 0.0 ),
			MakeUpVol( 0.0 ),
			TankSupplyVdot( 0.0 ),
			TankSupplyVol( 0.0 ),
			StarvedMakeUpVdot( 0.0 ),
			StarvedMakeUpVol( 0.0 ),
			BypassFraction( 0.0 ),
			NumCellOn( 0 ),
			SpeedSelected( 0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const InletWaterTemp, // Tower inlet water temperature (C)
			Real64 const OutletWaterTemp, // Tower outlet water temperature (C)
			Real64 const WaterMassFlowRate, // Tower water mass flow rate (m3/s)
			Real64 const Qactual, // Tower heat rejection rate (W)
			Real64 const FanPower, // Tower fan power (W)
			Real64 const FanEnergy, // Tower fan energy consumption (J)
			Real64 const AirFlowRatio, // Air flow ratio through variable speed cooling tower
			Real64 const BasinHeaterPower, // Basin heater power (W)
			Real64 const BasinHeaterConsumption, // Basin heater energy consumption (J)
			Real64 const WaterAmountUsed, // Tower make up water usage (m3)
			Real64 const FanCyclingRatio, // cycling ratio of tower fan when min fan speed provide too much capacity (for VFD)
			Real64 const EvaporationVdot,
			Real64 const EvaporationVol,
			Real64 const DriftVdot,
			Real64 const DriftVol,
			Real64 const BlowdownVdot,
			Real64 const BlowdownVol,
			Real64 const MakeUpVdot,
			Real64 const MakeUpVol,
			Real64 const TankSupplyVdot,
			Real64 const TankSupplyVol,
			Real64 const StarvedMakeUpVdot,
			Real64 const StarvedMakeUpVol,
			Real64 const BypassFraction, // Added for fluid bypass
			int const NumCellOn, // for multi-cell tower
			int const SpeedSelected // Speed selected for the two speed tower
		) :
			InletWaterTemp( InletWaterTemp ),
			OutletWaterTemp( OutletWaterTemp ),
			WaterMassFlowRate( WaterMassFlowRate ),
			Qactual( Qactual ),
			FanPower( FanPower ),
			FanEnergy( FanEnergy ),
			AirFlowRatio( AirFlowRatio ),
			BasinHeaterPower( BasinHeaterPower ),
			BasinHeaterConsumption( BasinHeaterConsumption ),
			WaterAmountUsed( WaterAmountUsed ),
			FanCyclingRatio( FanCyclingRatio ),
			EvaporationVdot( EvaporationVdot ),
			EvaporationVol( EvaporationVol ),
			DriftVdot( DriftVdot ),
			DriftVol( DriftVol ),
			BlowdownVdot( BlowdownVdot ),
			BlowdownVol( BlowdownVol ),
			MakeUpVdot( MakeUpVdot ),
			MakeUpVol( MakeUpVol ),
			TankSupplyVdot( TankSupplyVdot ),
			TankSupplyVol( TankSupplyVol ),
			StarvedMakeUpVdot( StarvedMakeUpVdot ),
			StarvedMakeUpVol( StarvedMakeUpVol ),
			BypassFraction( BypassFraction ),
			NumCellOn( NumCellOn ),
			SpeedSelected( SpeedSelected )
		{}

	};

	struct VSTowerData
	{
		// Members
		// variables specific to variable-speed towers
		FArray1D< Real64 > Coeff; // - model coefficients
		bool FoundModelCoeff; // - TRUE if model is calibratable
		Real64 MinInletAirWBTemp; // - model limit for min inlet air WB temp
		Real64 MaxInletAirWBTemp; // - model limit for max inlet air WB temp
		Real64 MinRangeTemp; // - model limit for min range temp
		Real64 MaxRangeTemp; // - model limit for max range temp
		Real64 MinApproachTemp; // - model limit for min approach temp
		Real64 MaxApproachTemp; // - model limit for max approach temp
		Real64 MinWaterFlowRatio; // - model limit for min water flow rate ratio
		Real64 MaxWaterFlowRatio; // - model limit for max water flow rate ratio
		Real64 MaxLiquidToGasRatio; // - model limit for max liquid to gas ratio
		int VSErrorCountFlowFrac; // - counter if water flow rate ratio limits are exceeded
		int VSErrorCountWFRR; // - counter if water flow rate ratio limits are exceeded
		int VSErrorCountIAWB; // - counter if inlet air wet-bulb temperature limits are exceeded
		int VSErrorCountTR; // - counter if tower range temperature limits are exceeded
		int VSErrorCountTA; // - counter if tower approach temperature limits are exceeded
		int ErrIndexFlowFrac; // - index to recurring error structure for liquid to gas ratio
		int ErrIndexWFRR; // - index to recurring error structure for water flow rate ratio
		int ErrIndexIAWB; // - index to recurring error structure for inlet air WB
		int ErrIndexTR; // - index to recurring error structure for tower range
		int ErrIndexTA; // - index to recurring error structure for tower approach
		int ErrIndexLG; // - index to recurring error structure for tower liquid/gas ratio
		//- Tr = Range temperature
		std::string TrBuffer1; // - buffer to print Tr warning messages on following time step
		std::string TrBuffer2; // - buffer to print Tr warning messages on following time step
		std::string TrBuffer3; // - buffer to print Tr warning messages on following time step
		//- Twb = Wet-bulb temperature
		std::string TwbBuffer1; // - buffer to print Twb warning messages on following time step
		std::string TwbBuffer2; // - buffer to print Twb warning messages on following time step
		std::string TwbBuffer3; // - buffer to print Twb warning messages on following time step
		//- Ta = Approach temperature
		std::string TaBuffer1; // - buffer to print Ta warning messages on following time step
		std::string TaBuffer2; // - buffer to print Ta warning messages on following time step
		std::string TaBuffer3; // - buffer to print Ta warning messages on following time step
		//- WFRR = Water flow rate ratio
		std::string WFRRBuffer1; // - buffer to print WFRR warning messages on following time step
		std::string WFRRBuffer2; // - buffer to print WFRR warning messages on following time step
		std::string WFRRBuffer3; // - buffer to print WFRR warning messages on following time step
		//- LG = Liquid to gas ratio
		std::string LGBuffer1; // - buffer to print LG warning messages on following time step
		std::string LGBuffer2; // - buffer to print LG warning messages on following time step
		std::string LGBuffer3; // - buffer to print LG warning messages on following time step
		bool PrintTrMessage; // - flag to print Tr error message
		bool PrintTwbMessage; // - flag to print Twb error message
		bool PrintTaMessage; // - flag to print Ta error message
		bool PrintWFRRMessage; // - flag to print WFRR error message
		bool PrintLGMessage; // - flag to print liquid-gas ratio error message
		Real64 TrLast; // value of Tr when warning occurred (passed to Recurring Warning)
		Real64 TwbLast; // value of Twb when warning occurred (passed to Recurring Warning)
		Real64 TaLast; // value of Ta when warning occurred (passed to Recurring Warning)
		Real64 WaterFlowRateRatioLast; // value of WFRR when warning occurred (passed to Recurring Warn)
		Real64 LGLast; // value of LG when warning occurred (passed to Recurring Warn)

		// Default Constructor
		VSTowerData() :
			FoundModelCoeff( false ),
			MinInletAirWBTemp( 0.0 ),
			MaxInletAirWBTemp( 0.0 ),
			MinRangeTemp( 0.0 ),
			MaxRangeTemp( 0.0 ),
			MinApproachTemp( 0.0 ),
			MaxApproachTemp( 0.0 ),
			MinWaterFlowRatio( 0.0 ),
			MaxWaterFlowRatio( 0.0 ),
			MaxLiquidToGasRatio( 0.0 ),
			VSErrorCountFlowFrac( 0 ),
			VSErrorCountWFRR( 0 ),
			VSErrorCountIAWB( 0 ),
			VSErrorCountTR( 0 ),
			VSErrorCountTA( 0 ),
			ErrIndexFlowFrac( 0 ),
			ErrIndexWFRR( 0 ),
			ErrIndexIAWB( 0 ),
			ErrIndexTR( 0 ),
			ErrIndexTA( 0 ),
			ErrIndexLG( 0 ),
			PrintTrMessage( false ),
			PrintTwbMessage( false ),
			PrintTaMessage( false ),
			PrintWFRRMessage( false ),
			PrintLGMessage( false ),
			TrLast( 0.0 ),
			TwbLast( 0.0 ),
			TaLast( 0.0 ),
			WaterFlowRateRatioLast( 0.0 ),
			LGLast( 0.0 )
		{}

		// Member Constructor
		VSTowerData(
			FArray1< Real64 > const & Coeff, // - model coefficients
			bool const FoundModelCoeff, // - TRUE if model is calibratable
			Real64 const MinInletAirWBTemp, // - model limit for min inlet air WB temp
			Real64 const MaxInletAirWBTemp, // - model limit for max inlet air WB temp
			Real64 const MinRangeTemp, // - model limit for min range temp
			Real64 const MaxRangeTemp, // - model limit for max range temp
			Real64 const MinApproachTemp, // - model limit for min approach temp
			Real64 const MaxApproachTemp, // - model limit for max approach temp
			Real64 const MinWaterFlowRatio, // - model limit for min water flow rate ratio
			Real64 const MaxWaterFlowRatio, // - model limit for max water flow rate ratio
			Real64 const MaxLiquidToGasRatio, // - model limit for max liquid to gas ratio
			int const VSErrorCountFlowFrac, // - counter if water flow rate ratio limits are exceeded
			int const VSErrorCountWFRR, // - counter if water flow rate ratio limits are exceeded
			int const VSErrorCountIAWB, // - counter if inlet air wet-bulb temperature limits are exceeded
			int const VSErrorCountTR, // - counter if tower range temperature limits are exceeded
			int const VSErrorCountTA, // - counter if tower approach temperature limits are exceeded
			int const ErrIndexFlowFrac, // - index to recurring error structure for liquid to gas ratio
			int const ErrIndexWFRR, // - index to recurring error structure for water flow rate ratio
			int const ErrIndexIAWB, // - index to recurring error structure for inlet air WB
			int const ErrIndexTR, // - index to recurring error structure for tower range
			int const ErrIndexTA, // - index to recurring error structure for tower approach
			int const ErrIndexLG, // - index to recurring error structure for tower liquid/gas ratio
			std::string const & TrBuffer1, // - buffer to print Tr warning messages on following time step
			std::string const & TrBuffer2, // - buffer to print Tr warning messages on following time step
			std::string const & TrBuffer3, // - buffer to print Tr warning messages on following time step
			std::string const & TwbBuffer1, // - buffer to print Twb warning messages on following time step
			std::string const & TwbBuffer2, // - buffer to print Twb warning messages on following time step
			std::string const & TwbBuffer3, // - buffer to print Twb warning messages on following time step
			std::string const & TaBuffer1, // - buffer to print Ta warning messages on following time step
			std::string const & TaBuffer2, // - buffer to print Ta warning messages on following time step
			std::string const & TaBuffer3, // - buffer to print Ta warning messages on following time step
			std::string const & WFRRBuffer1, // - buffer to print WFRR warning messages on following time step
			std::string const & WFRRBuffer2, // - buffer to print WFRR warning messages on following time step
			std::string const & WFRRBuffer3, // - buffer to print WFRR warning messages on following time step
			std::string const & LGBuffer1, // - buffer to print LG warning messages on following time step
			std::string const & LGBuffer2, // - buffer to print LG warning messages on following time step
			std::string const & LGBuffer3, // - buffer to print LG warning messages on following time step
			bool const PrintTrMessage, // - flag to print Tr error message
			bool const PrintTwbMessage, // - flag to print Twb error message
			bool const PrintTaMessage, // - flag to print Ta error message
			bool const PrintWFRRMessage, // - flag to print WFRR error message
			bool const PrintLGMessage, // - flag to print liquid-gas ratio error message
			Real64 const TrLast, // value of Tr when warning occurred (passed to Recurring Warning)
			Real64 const TwbLast, // value of Twb when warning occurred (passed to Recurring Warning)
			Real64 const TaLast, // value of Ta when warning occurred (passed to Recurring Warning)
			Real64 const WaterFlowRateRatioLast, // value of WFRR when warning occurred (passed to Recurring Warn)
			Real64 const LGLast // value of LG when warning occurred (passed to Recurring Warn)
		) :
			Coeff( Coeff ),
			FoundModelCoeff( FoundModelCoeff ),
			MinInletAirWBTemp( MinInletAirWBTemp ),
			MaxInletAirWBTemp( MaxInletAirWBTemp ),
			MinRangeTemp( MinRangeTemp ),
			MaxRangeTemp( MaxRangeTemp ),
			MinApproachTemp( MinApproachTemp ),
			MaxApproachTemp( MaxApproachTemp ),
			MinWaterFlowRatio( MinWaterFlowRatio ),
			MaxWaterFlowRatio( MaxWaterFlowRatio ),
			MaxLiquidToGasRatio( MaxLiquidToGasRatio ),
			VSErrorCountFlowFrac( VSErrorCountFlowFrac ),
			VSErrorCountWFRR( VSErrorCountWFRR ),
			VSErrorCountIAWB( VSErrorCountIAWB ),
			VSErrorCountTR( VSErrorCountTR ),
			VSErrorCountTA( VSErrorCountTA ),
			ErrIndexFlowFrac( ErrIndexFlowFrac ),
			ErrIndexWFRR( ErrIndexWFRR ),
			ErrIndexIAWB( ErrIndexIAWB ),
			ErrIndexTR( ErrIndexTR ),
			ErrIndexTA( ErrIndexTA ),
			ErrIndexLG( ErrIndexLG ),
			TrBuffer1( TrBuffer1 ),
			TrBuffer2( TrBuffer2 ),
			TrBuffer3( TrBuffer3 ),
			TwbBuffer1( TwbBuffer1 ),
			TwbBuffer2( TwbBuffer2 ),
			TwbBuffer3( TwbBuffer3 ),
			TaBuffer1( TaBuffer1 ),
			TaBuffer2( TaBuffer2 ),
			TaBuffer3( TaBuffer3 ),
			WFRRBuffer1( WFRRBuffer1 ),
			WFRRBuffer2( WFRRBuffer2 ),
			WFRRBuffer3( WFRRBuffer3 ),
			LGBuffer1( LGBuffer1 ),
			LGBuffer2( LGBuffer2 ),
			LGBuffer3( LGBuffer3 ),
			PrintTrMessage( PrintTrMessage ),
			PrintTwbMessage( PrintTwbMessage ),
			PrintTaMessage( PrintTaMessage ),
			PrintWFRRMessage( PrintWFRRMessage ),
			PrintLGMessage( PrintLGMessage ),
			TrLast( TrLast ),
			TwbLast( TwbLast ),
			TaLast( TaLast ),
			WaterFlowRateRatioLast( WaterFlowRateRatioLast ),
			LGLast( LGLast )
		{}

	};

	// Object Data
	extern FArray1D< Towerspecs > SimpleTower; // dimension to number of machines
	extern FArray1D< TowerInletConds > SimpleTowerInlet; // inlet conditions
	extern FArray1D< ReportVars > SimpleTowerReport; // report variables
	extern FArray1D< VSTowerData > VSTower; // model coefficients and specific variables for VS tower

	// Functions

	void
	SimTowers(
		std::string const & TowerType,
		std::string const & TowerName,
		int & CompIndex,
		bool & RunFlag,
		bool const InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	);

	// End CondenserLoopTowers Module Driver Subroutines
	//******************************************************************************

	// Beginning of CondenserLoopTowers Module Get Input subroutines
	//******************************************************************************

	void
	GetTowerInput();

	// End of Get Input subroutines for the CondenserLoopTowers Module
	//******************************************************************************

	// Beginning Initialization Section for the CondenserLoopTowers Module
	//******************************************************************************

	void
	InitSimVars();

	void
	InitTower(
		int const TowerNum, // Number of the current cooling tower being simulated
		bool const RunFlag // Indication of
	);

	void
	SizeTower( int const TowerNum );

	void
	SizeVSMerkelTower( int const TowerNum );

	// End Initialization Section for the CondenserLoopTowers Module
	//******************************************************************************

	// Beginning of the CondenserLoopTowers Module Simulation Subroutines
	// *****************************************************************************

	void
	CalcSingleSpeedTower( int & TowerNum );

	void
	CalcTwoSpeedTower( int & TowerNum );

	void
	CalcMerkelVariableSpeedTower(
		int const TowerNum,
		Real64 & MyLoad
	);

	Real64
	VSMerkelResidual(
		Real64 const AirFlowRateRatio, // fan speed ratio (1.0 is continuous, 0.0 is off)
		Optional< FArray1S< Real64 > const > Par = _ // par(1) = Tower number
	);

	void
	CalcVariableSpeedTower( int const TowerNum );

	void
	SimSimpleTower(
		int const TowerNum,
		Real64 const WaterMassFlowRate,
		Real64 const AirFlowRate,
		Real64 const UAdesign,
		Real64 & OutletWaterTemp
	);

	void
	SimVariableTower(
		int const TowerNum, // variable speed tower index
		Real64 const WaterFlowRateRatio, // current water flow rate ratio (capped if applicable)
		Real64 const AirFlowRateRatio, // current air flow rate ratio
		Real64 const Twb, // current inlet air wet-bulb temperature (C, capped if applicable)
		Real64 & OutletWaterTemp // calculated tower outlet water temperature (C)
	);

	void
	CalcVSTowerApproach(
		int const TowerNum, // Index to cooling tower
		Real64 const PctWaterFlow, // Water flow ratio of cooling tower
		Real64 const AirFlowRatio, // Air flow ratio of cooling tower
		Real64 const Twb, // Inlet air wet-bulb temperature [C]
		Real64 const Tr, // Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
		Real64 & Approach // Calculated approach temperature [C]
	);

	void
	CheckModelBounds(
		int const TowerNum, // index to tower
		Real64 const Twb, // current inlet air wet-bulb temperature (C)
		Real64 const Tr, // requested range temperature for current time step (C)
		Real64 const Ta, // requested approach temperature for current time step (C)
		Real64 const WaterFlowRateRatio, // current water flow rate ratio at water inlet node
		Real64 & TwbCapped, // bounded value of inlet air wet-bulb temperature (C)
		Real64 & TrCapped, // bounded value of range temperature (C)
		Real64 & TaCapped, // bounded value of approach temperature (C)
		Real64 & WaterFlowRateRatioCapped // bounded value of water flow rate ratio
	);

	Real64
	SimpleTowerUAResidual(
		Real64 const UA, // UA of cooling tower
		Optional< FArray1S< Real64 > const > Par = _ // par(1) = design tower load [W]
	);

	Real64
	SimpleTowerTrResidual(
		Real64 const Trange, // cooling tower range temperature [C]
		Optional< FArray1S< Real64 > const > Par = _ // par(1) = tower number
	);

	Real64
	SimpleTowerApproachResidual(
		Real64 const FlowRatio, // water or air flow ratio of cooling tower
		Optional< FArray1S< Real64 > const > Par = _ // par(1) = tower number
	);

	// End of the CondenserLoopTowers Module Simulation Subroutines

	// *****************************************************************************

	void
	CalculateWaterUseage( int const TowerNum );

	// Beginning of Record Keeping subroutines for the Tower Module
	// *****************************************************************************

	void
	UpdateTowers( int const TowerNum );

	// End of Record Keeping subroutines for the Tower Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Tower Module
	// *****************************************************************************

	void
	ReportTowers(
		bool const RunFlag,
		int const TowerNum
	);

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // CondenserLoopTowers

} // EnergyPlus

#endif
