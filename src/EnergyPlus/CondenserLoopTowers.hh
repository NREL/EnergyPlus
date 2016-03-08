// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef CondenserLoopTowers_hh_INCLUDED
#define CondenserLoopTowers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

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

	extern Array1D_bool CheckEquipName;

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
		bool DesignWaterFlowRateWasAutoSized; //true if previous was autosize on input
		Real64 DesignWaterFlowPerUnitNomCap; // scalable sizing factor for water flow per capacity [m3/s/W]
		Real64 DesWaterMassFlowRate; // Design water flow rate through the entire tower [kg/s]
		Real64 DesWaterMassFlowRatePerCell; // Design water flow rate per cell [Kg/s]
		Real64 HighSpeedAirFlowRate; // Air flow rate through tower at high speed [m3/s]
		bool HighSpeedAirFlowRateWasAutoSized; //true if previous was autosize on input
		Real64 DesignAirFlowPerUnitNomCap; // scalable sizing factor for air flow per capacity [m3/s/W]
		bool DefaultedDesignAirFlowScalingFactor; // true if user left input field blank for DesignAirFlowPerUnitNomCap
		Real64 HighSpeedFanPower; // Fan power at high fan speed [W]
		bool HighSpeedFanPowerWasAutoSized; //true if fan power was autosize on input
		Real64 DesignFanPowerPerUnitNomCap; // scalable sizing factor for fan power per capacity [W/W]

		Real64 HighSpeedTowerUA; // UA of tower at high fan speed [W/C]
		bool HighSpeedTowerUAWasAutoSized; // true if previous was autosize on input
		Real64 LowSpeedAirFlowRate; // Air flow rate through tower at low speed [m3/s]
		bool LowSpeedAirFlowRateWasAutoSized; // true if previous was autosize on input
		Real64 LowSpeedAirFlowRateSizingFactor; // sizing factor for low speed flow rate [ ]
		Real64 LowSpeedFanPower; // Fan power at low fan speed [W]
		bool LowSpeedFanPowerWasAutoSized;// true if low speed fan power was autosized on input
		Real64 LowSpeedFanPowerSizingFactor; // sizing factor for low speed fan power []
		Real64 LowSpeedTowerUA; // UA of tower at low fan speed [W/C]
		bool LowSpeedTowerUAWasAutoSized; //ture if low speed UA was autosize on input
		Real64 LowSpeedTowerUASizingFactor; // sizing factor for UA at low fan speed []
		Real64 FreeConvAirFlowRate; // Air flow rate through tower with fan off [m3/s]
		bool FreeConvAirFlowRateWasAutoSized; //true if previous was autosize on input
		Real64 FreeConvAirFlowRateSizingFactor; // sizing factor for air flow at free conv []
		Real64 FreeConvTowerUA; // UA of tower with fan off [W/C]
		bool FreeConvTowerUAWasAutoSized; // true if previous was autosize on input
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
		bool TowerNominalCapacityWasAutoSized; //true if tower nominal capacity was autosized on input
		Real64 TowerLowSpeedNomCap; // Nominal capacity of the tower [W] with entering water at 35C (95F),
		//  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
		//  temp and 35C (95F) dry-bulb temp, and water flow
		//  rate of 5.382E-8 m3/s per nominal capacity watt (3 gpm/ton)
		bool TowerLowSpeedNomCapWasAutoSized; // true if previous was autosize on input
		Real64 TowerLowSpeedNomCapSizingFactor; // sizing factor for low speed capacity []
		Real64 TowerFreeConvNomCap; // Nominal capacity of the tower [W] with entering water at 35C (95F),
		//  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
		//  temp and 35C (95F) dry-bulb temp, and water flow
		//  rate of 5.382E-8 m3/s per nominal capacity watt (3 gpm/ton)
		bool TowerFreeConvNomCapWasAutoSized; // true if previous was autosize on Input
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
			DesignWaterFlowRateWasAutoSized( false ),
			DesignWaterFlowPerUnitNomCap( 0.0 ),
			DesWaterMassFlowRate( 0.0 ),
			DesWaterMassFlowRatePerCell( 0.0 ),
			HighSpeedAirFlowRate( 0.0 ),
			HighSpeedAirFlowRateWasAutoSized( false ),
			DesignAirFlowPerUnitNomCap( 0.0 ),
			DefaultedDesignAirFlowScalingFactor( false ),
			HighSpeedFanPower( 0.0 ),
			HighSpeedFanPowerWasAutoSized( false ),
			DesignFanPowerPerUnitNomCap( 0.0 ),
			HighSpeedTowerUA( 0.0 ),
			HighSpeedTowerUAWasAutoSized( false ),
			LowSpeedAirFlowRate( 0.0 ),
			LowSpeedAirFlowRateWasAutoSized( false ),
			LowSpeedAirFlowRateSizingFactor( 0.0 ),
			LowSpeedFanPower( 0.0 ),
			LowSpeedFanPowerWasAutoSized( false ),
			LowSpeedFanPowerSizingFactor( 0.0 ),
			LowSpeedTowerUA( 0.0 ),
			LowSpeedTowerUAWasAutoSized( false ),
			LowSpeedTowerUASizingFactor( 0.0 ),
			FreeConvAirFlowRate( 0.0 ),
			FreeConvAirFlowRateWasAutoSized( false ),
			FreeConvAirFlowRateSizingFactor( 0.0 ),
			FreeConvTowerUA( 0.0 ),
			FreeConvTowerUAWasAutoSized( false ),
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
			TowerNominalCapacityWasAutoSized( false ),
			TowerLowSpeedNomCap( 0.0 ),
			TowerLowSpeedNomCapWasAutoSized( false ),
			TowerLowSpeedNomCapSizingFactor( 0.0 ),
			TowerFreeConvNomCap( 0.0 ),
			TowerFreeConvNomCapWasAutoSized( false ),
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

	};

	struct VSTowerData
	{
		// Members
		// variables specific to variable-speed towers
		Array1D< Real64 > Coeff; // - model coefficients
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

	};

	// Object Data
	extern Array1D< Towerspecs > SimpleTower; // dimension to number of machines
	extern Array1D< TowerInletConds > SimpleTowerInlet; // inlet conditions
	extern Array1D< ReportVars > SimpleTowerReport; // report variables
	extern Array1D< VSTowerData > VSTower; // model coefficients and specific variables for VS tower

	// Functions
	void
	clear_state();

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
		Array1< Real64 > const & Par // par(1) = Tower number
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
		Array1< Real64 > const & Par // par(1) = design tower load [W]
	);

	Real64
	SimpleTowerTrResidual(
		Real64 const Trange, // cooling tower range temperature [C]
		Array1< Real64 > const & Par // par(1) = tower number
	);

	Real64
	SimpleTowerApproachResidual(
		Real64 const FlowRatio, // water or air flow ratio of cooling tower
		Array1< Real64 > const & Par // par(1) = tower number
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

} // CondenserLoopTowers

} // EnergyPlus

#endif
