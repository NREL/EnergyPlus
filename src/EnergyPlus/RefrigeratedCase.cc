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

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <RefrigeratedCase.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace RefrigeratedCase {

	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad, FSEC
	//       DATE WRITTEN   Oct/Nov 2004
	//       MODIFIED       Shirey, FSEC Dec 2004; Hudson, ORNL Feb 2007, July 2007
	//       MODIFIED       Stovall, ORNL, April 2008 added detailed refrigerations systems
	//       MODIFIED       Stovall, ORNL, Fall 2009 added cascade condensers, secondary loops, and walk-ins.
	//       MODIFIED       Griffith, NREL, 2010, Plant upgrade, generalize plant fluid properties.
	//       MODIFIED       Fricke, ORNL, Fall 2011, added detailed transcritical CO2 refrigeration system.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To simulate refrigerated cases,walk-in coolers, secondary loops,
	//             compressor racks, and detailed refrigeration systems.
	// Case performance is varied based on specific operating conditions within the zone.

	// METHODOLOGY EMPLOYED:
	// Refrigerated cases are simulated based on performance information available from manufacturers
	// and knowing the latent heat ratio of the case cooling load which can be obtained from ASHRAE or
	// others who have completed research to quantify this value. The sensible case credits
	// (sensible cooling provided to the surrounding zone air) are calculated during input
	// processing by subtracting the fan, lighting, and anti-sweat heater energy from the
	// design sensible capacity (TotCap * (1-LHR) * RTF).  The actual latent cooling provided
	// by the case (at the actual operating conditions) is calculated based on the latent performance
	// curve supplied by the user.  The actual sensible cooling provided by the case (at the actual
	// operating conditions) is calculated by summing all sensible components (fan + light + anti-sweat +
	// sensible case credit). The case (evaporator) fans are assumed to be disabled during hot-gas or
	// electric defrost modes. A stocking schedule (W/m) is available for added load if desired.
	// Walk-In coolers are simulated based on the specified sum of the conductance*area of each wall and door facing
	// up to three (extendible) zones.  Two types of doors are specified, stock doors and glass doors.  For each category
	// of doors, schedules for door openings are used with ASHRAE models for infiltration
	// which are a function of the height of the door.  That
	// infiltration is used to calculate the latent load on the cooler and the latent case credits for each zone.
	// The sensible case credit for each zone is the sum of the conductive and sensible infiltration load
	// for each surface/door facing that zone.  Note that the latent portion of the infiltration is unused
	// during defrost to be consistent with decision for refrigerated cases.
	// Compressor racks are simulated by summing the attached refrigerated case and walk-in cooler
	// capacities.  The energy use of the
	// compressor rack is then calculated with a simplified model for COP variation with temperature. Condenser fan power
	// is based on a user-supplied curve object. Racks are not scheduled.
	// Compressor racks may have indoor (in-zone) or outdoor condensers.  If outdoors, the condensers may be cooled
	// by dry air cooling or evaporative cooling. A water-cooled condenser is also available by specifying water-cooling
	// as the Condenser Type.  If a water-cooled condenser is utilized, a second entry to this module will
	// occur during the HVAC simulation as called from the NonZoneEquipmentManager.
	// Detailed refrigeration systems model each compressor individually using the manufacturer's rating curves.
	// A collection of such curves has been added in the datasets library. The curves produce the refrigeration
	// capacity and power consumption. The capacity needed is based on the sum of the case and walk-in loads (as is done
	// for the compressor racks).  The compressors are dispatched to meet this load according to the order
	// prescribed in the compressor list input object. The condenser for each system can be air-cooled,
	// evaporative-cooled, or water cooled.  For air and evap-cooled condensers, manufacturer's rating data
	// is input to describe the performance and to determine the required air flow rate, which is used to
	// calculate the fan power requirements.  The fans can be described as single-speed, two-speed, or variable
	// speed. The condenser performance data also is used to calculate the condensing temperature, which is a function
	// of the heat sink temperature and the load on the condenser.  This must be solved iteratively, checking on
	// the calculated refrigerant mass flow through the compressors.  The solution usually requires less than 5 iterations.
	// The refrigerant state exiting the compressor group is known so the amount of heat available for
	// desuperheat reclaim is explicitly known.
	// The detailed refrigeration model allows the use of subcoolers,secondary loops, and cascade condensers
	// to transfer load from one suction group to another. This introduces the need for further iterations among
	// the systems.  Three loops through the
	// systems are adequate to model these interactions.  The detailed model will also calculate a variable suction
	// pressure for systems with controls that allow the suction temperature/pressure to float
	// up when the case loads are less than their design capacity.

	// Secondary Systems include case and walk-in refrigeration loads.  However, the balance of the system is
	// made up of a heat exchanger and circulating pump(s) instead of a condenser and compressors.
	// The total load on the heat exchanger is the sum of the refrigeration loads, any pipe heat gains,
	// and the portion of the pump power that is absorbed by the circulating fluid. If the total load is
	// greater than the rated capacity of the secondary loop, the unmet load for any time step is carried over
	// to the next time step.  Each secondary system appears as a load on a detailed refrigeration system. If
	// any of the cases or walk-ins served by a secondary are defrosted using hot brine, the primary system
	// serving the secondary loop receives the defrost energy credits (i.e., heat reclaim used to generate defrost
	// energy).

	// Cascade Condensers allow the use of a higher temperature refrigeration system (primary system) to serve as a
	// heat rejection sink for a lower temperature refrigeration system (secondary system). The condensing
	// temperature can be fixed or can be allowed to float according to the minimum required evaporating temperature
	// for other loads upon the primary system. For cases and walk-ins served by cascade condensers, energy associated
	// with hot gas defrost is reclaimed from the primary system.  The refrigeration load the cascade condenser
	// places upon the primary system is the sum of all case and walk-in loads served by the secondary system plus
	// the sum of the secondary loop's compressor power. The same name used to identify the condenser in the
	// secondary loop is used to identify the cascade load on the primary system.

	// Detailed transcritical CO2 refrigeration systems model each compressor individually using the manufacturer's
	// performance data. A collection of CO2 compressor performance curves has been added in the datasets library.
	// The curves produce the refrigeration capacity and power consumption. The capacity required is based on the sum
	// of the case and walk-in loads and the compressors are dispatched to meet this load according to the order
	// prescribed in the compressor list input object. Currently, an air-cooled gas cooler is modeled, and
	// manufacturer's rating data is input to describe the performance and to determine the required fan power
	// requirements. The gas cooler fans can be described as single-speed, two-speed, or variable speed. During
	// transcritical operation, the optimal gas cooler pressure, which maximizes the system's COP, is determined as
	// a function of the ambient air temperature. During subcritical operation, the condensing pressure is allowed to
	// float with ambient temperature in order to acheive maximum performance.

	//This module was designed to be accessed once for each time step.  It uses several accumulating variables
	//  to carry unmet loads from one time step to the next (cases/walk-ins and compressors.  Also, it meets
	//  heat reclaim needs with the loads from the previous time step (because they are unknown for the current
	//  zone time step).  However, the loads time step may be repeated, such as when a demand manager is used.
	//  For that purpose, the values for these accumulating variables are saved at the start of each time step
	//  and reset whenever the time step is repeated.  (see the init subroutine.)
	//This correction is also applied when working on the system time step for coil-type loads by setting the saved values
	//  at the start of each system time step to the value at the end of the previous time step. They are reset
	//  to that value at each sys time step iteration. (see InitRefrigeration)

	// REFERENCES:
	// Specific references are provided for the equipment simulation subroutines below.

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHeatBalance::RefrigCaseCredit;
	using DataHeatBalance::HeatReclaimRefrigeratedRack;
	using DataHeatBalance::NumRefrigeratedRacks;
	using DataHeatBalance::NumRefrigSystems;
	using DataHeatBalance::HeatReclaimRefrigCondenser;
	using DataHeatBalance::NumRefrigCondensers;
	using DataHeatBalance::NumRefrigChillerSets;
	using DataHeatBalance::RefrigSystemTypeDetailed;
	using DataHeatBalance::RefrigSystemTypeRack;
	using DataHeatBalance::RefrigCondenserTypeAir;
	using DataHeatBalance::RefrigCondenserTypeEvap;
	using DataHeatBalance::RefrigCondenserTypeWater;
	using DataHeatBalance::RefrigCondenserTypeCascade;

	using DataHVACGlobals::TimeStepSys; // used when operating for warehouse coil
	using namespace DataGlobals; // includes LOGICAL :: BeginTimeStepFlag =.FALSE.
	// True at the start of each time step, False after first subtime step of time step
	// includes CurrentTime, in fractional hours, from start of day. Uses Loads time step.
	// includes NumOfZones
	using namespace ScheduleManager;
	using namespace DataLoopNode;
	using namespace DataEnvironment;
	using namespace FluidProperties;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// Anti-sweat heater control type
	int const ASNone( 0 );
	int const ASConstant( 1 );
	int const ASLinear( 2 );
	int const ASDewPoint( 3 );
	int const ASHeatBalance( 4 );
	// Refrigerated display case defrost type
	int const DefNone( 0 );
	int const DefOffCycle( 1 );
	int const DefHotFluid( 2 );
	int const DefHotFluidOnDemand( 3 );
	int const DefHotFluidTerm( 4 );
	int const DefElectric( 5 );
	int const DefElectricOnDemand( 6 );
	int const DefElectricTerm( 7 );

	// Refrigerated display case rack heat rejection location
	int const LocationOutdoors( 1 );
	int const LocationZone( 2 );
	// Condenser cooling type -- See DataHeatBalance - RefrigxxxTypexxx
	//INTEGER, PARAMETER :: CondenserCoolingAir   = 1
	//INTEGER, PARAMETER :: CondenserCoolingEvap  = 2
	//INTEGER, PARAMETER :: CondenserCoolingWater = 3
	//INTEGER, PARAMETER :: CondenserCascade      = 4
	// Air- and evap-cooled condenser fan speed control types
	int const FanVariableSpeed( 1 );
	int const FanConstantSpeedLinear( 2 );
	int const FanTwoSpeed( 3 );
	int const FanConstantSpeed( 4 );
	// Water-cooled condenser loop flow type
	int const VariableFlow( 1 );
	int const ConstantFlow( 2 );
	// Condenser evap cooling water supply
	int const WaterSupplyFromMains( 101 );
	int const WaterSupplyFromTank( 102 );
	// Cascade condenser temperature control types
	int const CascadeTempSet( 1 );
	int const CascadeTempFloat( 2 );
	// Refrigerated display case energy equation form
	int const None( 0 );
	int const CaseTemperatureMethod( 1 );
	int const RHCubic( 2 );
	int const DPCubic( 3 );
	// Secondary loop parameters
	int const SecFluidTypeAlwaysLiquid( 1 );
	int const SecFluidTypePhaseChange( 2 );
	int const SecPumpControlConstant( 1 );
	int const SecPumpControlVariable( 2 );
	// Walk In Cooler Defrost type
	int const WalkInDefrostFluid( 1 );
	int const WalkInDefrostElec( 2 );
	int const WalkInDefrostNone( 3 );
	int const WalkInDefrostOffCycle( 4 );
	// Walk In Cooler Defrost Control type
	int const DefrostControlSched( 1 );
	int const DefrostContTempTerm( 2 );
	// Walk In Cooler Stock Door Protection types
	int const WIStockDoorNone( 1 );
	int const WIStockDoorAirCurtain( 2 );
	int const WIStockDoorStripCurtain( 3 );
	// Subcooler type
	int const LiquidSuction( 1 );
	int const Mechanical( 2 );
	// Compressor suction pressure control
	int const FloatSuctionTemperature( 1 );
	int const ConstantSuctionTemperature( 2 );
	//Compressor rating types
	int const RatedSuperheat( 1 );
	int const RatedReturnGasTemperature( 2 );
	int const RatedSubcooling( 1 );
	int const RatedLiquidTemperature( 2 );
	// System service types (applies to system, cascade condenser, and secondary loops)
	//INTEGER, PARAMETER :: SupermarketService =1
	//INTEGER, PARAMETER :: WarehouseService   =2
	// Warehouse coil Defrost type
	int const DefrostFluid( 1 );
	int const DefrostElec( 2 );
	int const DefrostNone( 3 );
	int const DefrostOffCycle( 4 );

	int const RatedCapacityTotal( 1 );
	int const EuropeanSC1Std( 2 );
	int const EuropeanSC1Nom( 3 );
	int const EuropeanSC2Std( 4 );
	int const EuropeanSC2Nom( 5 );
	int const EuropeanSC3Std( 6 );
	int const EuropeanSC3Nom( 7 );
	int const EuropeanSC4Std( 8 );
	int const EuropeanSC4Nom( 9 );
	int const EuropeanSC5Std( 10 );
	int const EuropeanSC5Nom( 11 );
	int const UnitLoadFactorSens( 12 );
	int const SHR60( 1 );
	int const QuadraticSHR( 2 );
	int const European( 3 );
	int const TabularRH_DT1_TRoom( 4 );
	int const Ceiling( 1 );
	int const Middle( 2 );
	int const Floor( 3 );
	int const DetailedSystem( 1 );
	int const SecondarySystem( 2 );

	// Following constant approp for R22, future may make f(refrigerant)
	Real64 const CaseSuperheat( 4.0 ); // case superheat used to control thermal expansion valve, ASHRAE 2006 p 44.6 (C)
	Real64 const TransCaseSuperheat( 10.0 ); // case superheat for transcritical CO2 systems (C)
	// Next two constants used to autosize evap condenser
	Real64 const CondPumpRatePower( 0.004266 ); // evap condenser pump rated, Wpump/Wcapacity (15 W/ton)
	Real64 const AirVolRateEvapCond( 0.000144 ); // evap cond air flow rate for autosize, equiv 850 cfm/ton (m3/W-s)
	Real64 const EvapCutOutTdb( 4.0 ); // shut off evap water flow if outdoor drybulb < evapcutOutTdb (C)
	// Miscellaneous constants
	Real64 const MyLargeNumber( 1.0e9 );
	Real64 const MySmallNumber( 1.0e-9 );
	Real64 const Rair( 0.3169 ); // Air resistance used with Heat Balance anti-sweat (AS) heater
	Real64 const IceMeltEnthalpy( 335000.0 ); // heat of fusion of water J/kg
	Real64 const TempTooHotToFrost( 5.0 ); // C, used to check for frosting conditions on evaporator coils
	Real64 const IcetoVaporEnthalpy( 2833000.0 ); // J/kg to freeze water vapor to ice
	Real64 const WatertoVaporEnthalpy( 2.5e6 ); // at 0C
	Real64 const SpecificHeatIce( 2000.0 ); // in the likely range (2040 at 0C and 1950 at -20C) (J/kg-C)
	Real64 const CondAirVolExponentDry( 1.58 ); // exponent for forced air over a cylinder, = 1/.633
	//per ASHRAE 2005 (page 3.15)
	Real64 const CondAirVolExponentEvap( 1.32 ); // exponent for evap condenser air vol flow, = 1/.76
	//per Manske, 1999
	Real64 const EvaporatorAirVolExponent( 1.54 ); // exponent for evapaporator air vol flow, = 1/.65
	//per Manske, 1999, page 35

	Real64 const FanHalfSpeedRatio( 0.1768 ); // = 1/(2**2.5) for power step for two speed fan
	Real64 const CapFac60Percent( 0.60 ); // = 60%, load served by half power 2-speed fan

	Array1D< Real64 > const EuropeanWetCoilFactor( 5, { 1.35, 1.15, 1.05, 1.01, 1.0 } );
	Array1D< Real64 > const EuropeanAirInletTemp( 5, { 10.0, 0.0, -18.0, -25.0, -34.0 } );
	Array1D< Real64 > const EuropeanEvapTemp( 5, { 0.0, -8.0, -25.0, -31.0, -40.0 } );
	Array1D< Real64 > const EuropeanDT1( 5, { 10.0, 8.0, 7.0, 7.0, 6.0 } );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	int NumSimulationCondAir( 0 ); // Number of air-cooled condensers in simulation
	int NumSimulationCondEvap( 0 ); // Number of evaporative condensers in simulation
	int NumSimulationCondWater( 0 ); // Number of water-cooled condensers in simulation
	int NumSimulationCascadeCondensers( 0 ); // Total number of Cascade condensers in IDF
	int NumSimulationGasCooler( 0 ); // Number of gas coolers in simulation
	int NumSimulationSharedGasCoolers( 0 ); // Total number of gas coolers that serve more than one system
	int NumTransRefrigSystems( 0 ); // Total number of transcritical CO2 refrigeration systems
	int NumSimulationSharedCondensers( 0 ); // Total number of condensers that serve more than one system
	int NumSimulationCases( 0 ); // Number of refrigerated cases in simulation
	int NumSimulationCaseAndWalkInLists( 0 ); // Total number of CaseAndWalkIn Lists in IDF
	int NumSimulationWalkIns( 0 ); // Number of walk in coolers in simulation
	int NumSimulationCompressors( 0 ); // Number of refrigeration compressors in simulation
	int NumSimulationSubcoolers( 0 ); // Number of refrigeration subcoolers in simulation
	int NumSimulationMechSubcoolers( 0 ); // Number of mechanical subcoolers in simulation
	int NumSimulationRefrigAirChillers( 0 ); // Number of individual Air Chillers/coils in simulation
	int NumSimulationSecondarySystems( 0 ); // Number of Secondary loops in simulation
	int NumSimulationTransferLoadLists( 0 ); // Number of Secondary Lists in simulation
	int NumUnusedRefrigCases( 0 ); // Number of refrigerated cases not connected to a rack or system
	int NumUnusedCoils( 0 ); // Number of refrigeration air coils not connected to a rack or system
	int NumUnusedCondensers( 0 ); // Number of refrigeration condensors not connected to a system
	int NumUnusedGasCoolers( 0 ); // Number of refrigeration gas coolers not connected to a system
	int NumUnusedCompressors( 0 ); // Number of refrigeration compressors not connected to a system
	int NumUnusedSecondarys( 0 ); // Number of refrigeration secondarys not connected to a system
	int NumUnusedWalkIns( 0 ); // Number of refrigeration compressors not connected to a system
	bool MyReferPlantScanFlag( true );

	// Refrigerated case variables
	Real64 CaseRAFactor( 0.0 ); // Factor determining case credit allocation (e.g. % to zone or HVAC)
	Array1D_bool ShowStockingWarning; // Used for one-time warning message for possible case
	// input error regarding stocking
	Array1D_bool ShowFrostWarning; // Used for one-time warning message for possible case
	// input error regarding frost
	Array1D_bool ShowStoreEnergyWarning; // Used for one-time warning message for possible case
	// input error regarding defrost or stocking
	//  Walk In variables
	Array1D_bool ShowUnmetWIEnergyWarning; // Used for one-time warning message
	Array1D_bool ShowWIFrostWarning; // Used for one-time warning message

	// Refrigeration compressor rack variables
	Real64 TotalRackDeliveredCapacity( 0.0 ); // Total capacity of all refrigerated cases attached to rack (W)
	Real64 TotalCompressorPower( 0.0 ); // Total compressor electric power (W)
	Real64 CompressorCOPactual( 0.0 ); // Compressor coefficient of performance at specific operating conditions (W/W)
	Real64 RackSenCreditToZone( 0.0 ); // Amount of condenser heat applied to zone load (W)
	Real64 RackSenCreditToHVAC( 0.0 ); // Amount of condenser heat applied to HVAC RA duct (W)
	int InletNode( 0 ); // Water-cooled condenser inlet node number
	int OutletNode( 0 ); // Water-cooled condenser outlet node number
	Array1D_bool ShowCOPWarning; // Used for one-time warning message for possible rack
	// input error regarding COP
	// Refrigeration condenser variables (used for both racks and detailed systems)
	Real64 TotalCondenserFanPower( 0.0 ); // Total condenser fan electric power (W)
	Real64 TotalCondenserPumpPower( 0.0 ); // Total condenser pump electric power (W)
	Real64 TotalCondenserHeat( 0.0 ); // Total condenser heat from compressor rack (W)
	Real64 TotalBasinHeatPower( 0.0 ); // Total condenser basin water heater power (W)
	Real64 TotalEvapWaterUseRate( 0.0 ); // Total condenser water use rate (m3/s)

	// Refrigeration system variables
	Array1D_bool ShowUnmetEnergyWarning; // Used for one-time warning message for possible
	//compressor input error regarding total system compressor capacity
	Array1D_bool ShowHiStageUnmetEnergyWarning; // Used for one-time warning message for possible
	//high-stage compressor input error regarding high-stage compressor capacity

	// Transcritical refrigeration system variables
	bool TransCritSysFlag( false ); // Used to indicate whether or not a transcritical refrigeration system has been defined.
	Array1D_bool ShowUnmetEnergyWarningTrans; // Used for one-time warning message for possible
	//compressor input error regarding total system compressor capacity

	// Refrigeration Secondary Loop variables
	Array1D_bool ShowUnmetSecondEnergyWarning; // Used for one-time warning message for possible
	//compressor input error regarding secondary loop heat exchanger capacity
	//Refrigerated warehouse coil variables
	Array1D_bool CheckChillerName; // used when simrefrigcoil called for a zone
	//LOGICAL, ALLOCATABLE,DIMENSION(:) :: CheckZoneNum  !used when simrefrigcoil called for a zone
	Array1D_bool ShowCoilFrostWarning; // Used for one-time warning message if defrost cycles insufficient to melt ice

	// Refrigeration Plant connections checks
	Array1D_bool CheckEquipNameRackWaterCondenser;
	Array1D_bool CheckEquipNameWaterCondenser;

	//Control variables
	Array1D_bool RefrigPresentInZone; // Used when translating rate to energy for reporting
	//  total refrigeration impact on a zone
	Array1D_bool CheckChillerSetName; // used when sim chiller set called form zone equip manager

	bool GetRefrigerationInputFlag( true ); // Flag to show case input should be read
	bool HaveRefrigRacks( true ); // Is initialized as TRUE and remains true when
	// refrigerated racks exist in the input deck
	bool HaveDetailedRefrig( true ); // Is initialized as TRUE and remains true when
	// detailed refrigeration systems exist in the input deck
	bool HaveDetailedTransRefrig( true ); // Is initialized as TRUE and remains true when
	// detailed transcritical CO2 refrigeration systems exist in the input deck
	bool ManageRefrigeration( true ); // Is initialized as TRUE and remains true when
	// refrigerated racks or detailed systems exist in the input deck
	bool UseSysTimeStep( false ); // Flag is true IF working on a system that includes a coil cooling a controlled zone on the system time step,
	// All other refrigeration calculations for case and walkin systems done on the load time step
	bool HaveCasesOrWalkins( true ); // Is initialized as TRUE and remains true when
	// refrigerated cases or walkins exist in the input deck
	bool HaveChillers( true ); // Is initialized as TRUE and remains true when
	// chillers exist in the input deck
	// SUBROUTINE SPECIFICATIONS FOR MODULE RefrigeratedCase:

	// Object Data
	Array1D< RefrigCaseData > RefrigCase;
	Array1D< RefrigRackData > RefrigRack;
	Array1D< CaseRAFractionData > CaseRAFraction;
	Array1D< RefrigSystemData > System;
	Array1D< TransRefrigSystemData > TransSystem;
	Array1D< RefrigCondenserData > Condenser;
	Array1D< RefrigCompressorData > Compressor;
	Array1D< RefrigGasCoolerData > GasCooler;
	Array1D< SubcoolerData > Subcooler;
	Array1D< CaseAndWalkInListDef > CaseAndWalkInList;
	Array1D< CompressorListDef > CompressorLists;
	Array1D< SecondaryLoopData > Secondary;
	Array1D< TransferLoadListDef > TransferLoadList;
	Array1D< WalkInData > WalkIn;
	Array1D< WarehouseCoilData > WarehouseCoil;
	Array1D< AirChillerSetData > AirChillerSet;
	Array1D< CoilCreditData > CoilSysCredit;
	Array1D< CaseWIZoneReportData > CaseWIZoneReport;

	// Functions

	void
	ManageRefrigeratedCaseRacks()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Shirey, FSEC Dec 2004, Stovall, ORNL, May 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called from HVACManager.cc, subroutine ManageHVAC to
		// manage refrigerated cases and associated compressor racks on zone time step
		// OR from SimAirChillerSet in this module on sys time step (Stovall 2011)

		// METHODOLOGY EMPLOYED:
		// Each compressor rack is modeled by first simulating the attached refrigeration loads. The
		// loads can include refrigerated cases, walk-in coolers, and secondary fluid chillers.  The sum
		// of the total heat transfer for all attached loads determines the load on the compressor rack.
		// For the refrigeration rack approach, a simple model for variation of COP with
		// condensing temperature is used to determine rack power and energy consumption.
		// For the detailed system approach, the compressors and condensers are modeled individually
		// using manufacturer's data and rated performance curves.
		// Inter-system heat transfer via subcoolers and cascade condensers can be accomodated.
		// Secondary refrigeration cycles are also available.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int RackNum; // Index to the refrigerated compressor rack being modeled
		static bool MyOneTimeFlag( true ); // flag to skip first pass on next begin environment flag

		if ( ! ManageRefrigeration ) return;

		CheckRefrigerationInput();

		InitRefrigeration();

		//ManageRefrigeratedCaseRacks is called on each zone time step.
		//  However, if have chillers, ManageRefrigeration will be .TRUE. and will
		//  need to bounce back. (InitRefrig has to be called anyway to zero values at zone time step.)
		//  Therefore...
		if ( ( ! HaveCasesOrWalkins ) && ( ! UseSysTimeStep ) ) {
			//Zero requests for cooling water from plant or tank
			ZeroHVACValues();
			return;
		}
		//Following case should never occur, but just for completeness:
		if ( ( ! HaveChillers ) && ( UseSysTimeStep ) ) return;

		// Refrigerated cases are not simulated the first time through, replicate this on beginning of next environment
		if ( BeginEnvrnFlag && MyOneTimeFlag ) {
			MyOneTimeFlag = false;
			return;
		}
		if ( ! BeginEnvrnFlag ) MyOneTimeFlag = true;

		if ( HaveRefrigRacks ) {
			for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {
				CalcRackSystem( RackNum );
				ReportRackSystem( RackNum );
			}
		}

		if ( HaveDetailedRefrig ) SimulateDetailedRefrigerationSystems();
		if ( HaveDetailedTransRefrig ) SimulateDetailedTransRefrigSystems();

	}

	//***************************************************************************************************

	void
	GetRefrigerationInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Shirey, FSEC Dec 2004; Hudson, ORNL Feb 2007, July 2007
		//       MODIFIED       Stovall, ORNL April 2008, Assisted by Hugh Henderson
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// GetObjectItem is called to read refrigerated case, rack, compressor, and condenser information
		// Lists of cases and compressors are then correlated to the appropriate system.
		// The nominal ratings of all components are then compared and a warning is printed if the system is not balanced

		// METHODOLOGY EMPLOYED:
		// GetObjectItem is called to read refrigerated case information

		// REFERENCES:
		// na

		// Using/Aliasing
		using BranchNodeConnections::TestCompSet;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::CurveValue;
		using DataHeatBalance::Zone; // , &
		using DataHeatBalance::NumRefrigeratedRacks;
		using DataHeatBalance::NumRefrigSystems;
		//unused                               IntGainTypeOf_RefrigerationCompressorRack, &
		//unused                               IntGainTypeOf_RefrigerationCase
		using DataZoneEquipment::GetSystemNodeNumberForZone;
		using DataZoneEquipment::GetReturnAirNodeForZone;
		using DataEnvironment::StdBaroPress;
		using General::RoundSigDigits;
		using FluidProperties::GetSupHeatEnthalpyRefrig;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyTdpFnWPb;
		// USE ScheduleManager,   ONLY: CheckScheduleValueMinMax
		using WaterManager::SetupTankDemandComponent;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		//USE FluidProperties,   ONLY: GetDensityGlycol, GetSpecificHeatGlycol

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const TrackMessage( "from refrigerated case" );
		static std::string const RoutineName( "GetRefrigerationInput: " );
		static std::string const TrackMessageAlt( "GetInput in RefrigeratedCase" );
		static std::string const RoutineNameNoColon( "GetRefrigerationInput" );

		int const AlwaysOn( -1 ); // -1 pointer sent to schedule manager returns a value of 1.0
		//unused INTEGER, Parameter   ::  InputTypeSecond = 1  ! Indicator that flow used to specify capacity of secondary heat exchanger
		//unused INTEGER, Parameter   ::  InputTypeFirst = 2   ! Indicator that capacity of secondary heat exchanger is input directly
		//unused INTEGER, Parameter   ::  InputTypeBoth = 3    ! Indicator that capacity of secondary heat exchanger is
		//     input in both watts and flow rate
		int const NumWIAlphaFieldsBeforeZoneInput( 9 ); // Used to cycle through zones on input for walk in coolers
		int const NumWIAlphaFieldsPerZone( 4 ); // Used to cycle through zones on input for walk in coolers
		int const NumWINumberFieldsBeforeZoneInput( 12 ); // Used to cycle through zones on input for walk in coolers
		int const NumWINumberFieldsPerZone( 8 ); // Used to cycle through zones on input for walk in coolers
		Real64 const CondARI460DelT( 16.7 ); // Rated sat cond temp - dry bulb air T for air-cooled Condensers, ARI460
		Real64 const CondARI460Tcond( 51.7 ); // Rated sat cond temp for air-cooled cond, ARI 460
		Real64 const CondARI490DelT( 15.0 ); // Rated sat cond temp - wet bulb air T for evap-cooled Cond w R22, ARI490
		Real64 const CondARI490Tcond( 40.6 ); // Rated sat cond temp for evap-cooled cond with R22, ARI 490
		Real64 const DelEvapTDefault( 5.0 ); // default difference between case T and evap T (C)
		Real64 const HoursPerDay( 24.0 );
		Real64 const SecondsPerHour( 3600.0 );
		Real64 const DefaultCascadeCondApproach( 3.0 ); // Cascade condenser approach temperature difference (deltaC)
		Real64 const DefaultCircRate( 2.5 ); // Phase change liquid overfeed circulating rate (ASHRAE definition)
		//unused REAL(r64), PARAMETER ::  DefaultVarSpdCoeffA    =  0.9d0     !Variable speed pump power curve coefficients based
		//unused REAL(r64), PARAMETER ::  DefaultVarSpdCoeffB    = -0.1d0     !      upon paper by John Bittner of Hill Phoenix
		//unused REAL(r64), PARAMETER ::  DefaultVarSpdCoeffC    =  0.2d0     !      A is cube term, B square term, C linear term
		Real64 const DefaultWISurfaceUValue( 0.3154 ); // equiv R18 in Archaic American units (W/m2-delta T)
		Real64 const DefaultWIUValueGlassDr( 1.136 ); // equiv R5 in Archaic American units (W/m2-delta T)
		Real64 const DefaultWIUValueStockDr( 0.3785 ); // equiv R15 in Archaic American units (W/m2-delta T)
		Real64 const DefaultWIHeightGlassDr( 1.5 ); // glass door height in walk-in cooler (m)
		Real64 const DefaultWIHeightStockDr( 3.0 ); // stock door height in walk-in cooler (m)
		Real64 const PumpImpellerEfficiency( 0.78 ); // same as used in pump auto-sizing, dimensionless
		Real64 const PumpMotorEfficiency( 0.85 ); // suggested as average value in ITT/Gould pump references,
		//     dimensionless
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string Alphas; // Alpha items for object
		Array1D_string cAlphaFieldNames; // Alpha field names (from input processor)
		Array1D_string cNumericFieldNames; // Numeric field names (from input processor)
		static std::string CurrentModuleObject; // Object type for getting and error messages

		Array1D_bool lAlphaBlanks; // Logic array, alpha input blank = .TRUE.
		Array1D_bool lNumericBlanks; // Logic array, numeric input blank = .TRUE.
		static bool CaseLoads( false ); // Flag to help verify load type with loads served by systems cooled by cascade condensers
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static bool IsNotOK( false ); // Flag to verify name
		static bool IsBlank( false ); // Flag for blank name
		static bool StartCycle( false ); // Flag for counting defrost cycles

		static int AlphaListNum( 0 ); // Index of Names in Case, Compressor et al Lists
		static int AlphaNum( 0 ); // Used to cycle through input
		static int AlphaStartList( 0 );
		static int AStart( 0 ); // Used to cycle through zones on input for walk in coolers
		//INTEGER    :: CascadeCondenserID= 0       ! Used to match load on system to Condenser absolute index
		static int CascadeLoadNum( 0 ); // counters while associating cascade loads with systems
		static int CascadeLoadIndex( 0 ); // Counters while inputting cascade loads
		static int CaseID( 0 ); // ID of refrigerated case in rack
		static int CaseIndex( 0 ); // Index of refrigerated case attached to a system
		static int CaseNum( 0 ); // Index of refrigerated case
		static int CaseAndWalkInListNum( 0 ); // ID of refrigerated CaseAndWalkInList
		static int ChillerIndex( 0 );
		static int CoilID( 0 ); // Index of warehouse coil attached to a system
		static int CoilIndex( 0 ); // Index of warehouse coil attached to a system
		static int CoilNum( 0 ); // Index of warehouse coil
		static int CompIndex( 0 ); // Index  of refrigeration compressor attached to a system
		static int CompNum( 0 ); // Index of refrigeration compressor
		static int CondID( 0 ); // Condenser ID used when associating condenser as a cascade load
		static int CondIndex( 0 ); // Index  of refrigeration condenser attached to a system
		static int CondNum( 0 ); // Index of refrigeration condenser
		static int DefType( 0 ); // Local value for case defrost type
		//INTEGER    :: FlowIndex         = 0       ! Index of pump flow numeric field
		static int GCNum( 0 ); // Index of refrigeration gas cooler
		static int HRNum( 0 ); // Counter for hours in day
		static int IOStatus( 0 ); // Used in GetObjectItem
		static int ListNum( 0 ); // Index of Lists of cases, compressors, and subcoolers
		static int LoadCascadeNum( 0 ); // Used to read transfer load list
		static int LoadCount( 0 ); // check for blank case and walkin names in caseand alkinlist
		static int LoadSecondaryNum( 0 ); // Used to read transfer load list
		static int LoadWalkInNum( 0 ); // Used to read CaseAndWalkInList
		static int LoadCaseNum( 0 ); // Used to read CaseAndWalkInList
		static int LoadCoilNum( 0 ); // Used to read CaseAndWalkInList
		static int MaxNumAlphasRack( 0 ); // Maximum number of alphas for rack object
		static int MaxNumAlphasAirChiller( 0 ); // Maximum number of alphas for air chiller
		static int MaxNumAlphasAll( 0 ); // Maximum number of alphas for all objects
		static int MaxNumAlphasSys( 0 ); // Maximum number of alphas for system object
		static int MaxNumAlphasTransSys( 0 ); // Maximum number of alphas for transcritical system object
		static int MaxNumAlphasChillerSet( 0 ); // Maximum number of alphas for chiller set
		static int MaxNumAlphasConda( 0 ); // Maximum number of alphas for air-cooled condenser object
		static int MaxNumAlphasConde( 0 ); // Maximum number of alphas for evap-cooled condenser object
		static int MaxNumAlphasCondw( 0 ); // Maximum number of alphas for water-cooled condenser object
		static int MaxNumAlphasGasCoolera( 0 ); // Maximum number of alphas for air-cooled gas cooler object
		static int MaxNumAlphasComp( 0 ); // Maximum number of alphas for compressor object
		static int MaxNumAlphasCompressorList( 0 ); // Maximum number of alphas for compressor list objects
		static int MaxNumAlphasCase( 0 ); // Maximum number of alphas for case object
		static int MaxNumAlphasCaseAndWalkInList( 0 ); // Maximum number of alphas in CaseAndWalkInList
		static int MaxNumAlphasWalkIn( 0 ); // Maximum number of alphas for walkin object
		//INTEGER    :: MaxNumAlphasWalkInList  = 0 ! Maximum number of alphas for walkin list object
		static int MaxNumAlphasSecond( 0 ); // Maximum number of alphas for air chiller object
		static int MaxNumNumbersAirChiller( 0 ); // Maximum number of numbers for air chiller object
		static int MaxNumNumbersSecond( 0 ); // Maximum number of numbers for secondary system object
		static int MaxNumNumbersWalkIn( 0 ); // Maximum number of numbers for walkin object
		static int MaxNumNumbersCase( 0 ); // Maximum number of numbers for case object
		static int MaxNumNumbersCaseAndWalkInList( 0 ); // Maximum number of numbers in CaseAndWalkInList
		static int MaxNumNumbersRack( 0 ); // Maximum number of numbers for rack object
		static int MaxNumNumbersAll( 0 ); // Maximum number of numeric inputs for all objects
		static int MaxNumNumbersSys( 0 ); // Maximum number of numbers for system object
		static int MaxNumNumbersTransSys( 0 ); // Maximum number of numbers for transcritical system object
		static int MaxNumNumbersChillerSet( 0 ); // Maximum number of numbers for chiller set object
		static int MaxNumNumbersConda( 0 ); // Maximum number of numbers for air-cooled condenser object
		static int MaxNumNumbersConde( 0 ); // Maximum number of numbers for evap-cooled condenser object
		static int MaxNumNumbersCondw( 0 ); // Maximum number of numbers for water-cooled condenser object
		static int MaxNumNumbersGasCoolera( 0 ); // Maximum number of numbers for air-cooled gas cooler object
		static int MaxNumNumbersComp( 0 ); // Maximum number of numbers for compressor object
		static int MaxNumNumbersCompressorList( 0 ); // Maximum number of numbers
		static int MaxNumArgs( 0 ); // Max number of alphas and numbers (arguments) for rack object
		static int NStart( 0 ); // Used to cycle through zones on input for walk in coolers
		static int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		static int NumCascadeLoad( 0 ); // Number of Cascade Loads on current system
		static int NumCompressorsSys( 0 ); // Number of compressors on current system
		static int NumHiStageCompressorsSys( 0 ); // Number of high-stage compressors on current system
		static int NumCondensers( 0 ); // Counter for condensers in GETInput do loop
		static int NumGasCoolers( 0 ); // Counter for gas coolers in GetInput do loop
		static int NumDefCycles( 0 ); // Number of defrost cycles per day
		static int NumPumps( 0 ); // Number of pumps on a secondary loop
		static int NumCases( 0 ); // Number of refrigerated cases for single system
		static int NumCasesMT( 0 ); // Number of medium temperature cases on a single transcritical system
		static int NumCasesLT( 0 ); // Number of low temperature cases on a single transcritical system
		static int NumCoils( 0 ); // Number of warehouse coils for single system
		static int NumSubcooler( 0 ); // Number of subcoolers on current system
		static int NumNameMatches( 0 ); // Used to check for uniqueness of name for transfer loads
		static int NumNum( 0 ); // Used to cycle through input
		static int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call
		static int NumCompressorLists( 0 ); // Total number of Compressor Lists in IDF
		static int NumDisplayCases( 0 ); // Counter for refrigerated cases in GetInput do loop
		static int NumSecondary( 0 ); // Number of secondary loops
		static int NumWalkIns( 0 ); // Number of walk ins
		static int NumWalkInsMT( 0 ); // Number of medium temperature walk-ins on a single transcritical system
		static int NumWalkInsLT( 0 ); // Number of low temperature walk-ins on a single transcritical system
		static int NumWIFieldsPerZone( 0 ); // Used to calculate number of zones exposed to each walkin
		static int NumWIFieldsTotal( 0 ); // Used to calculate number of zones exposed to each walkin
		static int NumZones( 0 ); // Used to cycle through zones on input for walk in coolers
		static int NumTotalLoadsOnList( 0 ); // Used to read transfer load and caseandWalkIn lists
		static int NumSecondarysOnList( 0 ); // Used to read transfer load lists
		static int NumCascadeLoadsChecked( 0 ); // Used when checking for consistency of coil loads/time steps
		static int NumCascadeLoadsOnList( 0 ); // Used to read transfer load lists
		static int NumLoad( 0 ); // Used to read transfer loadand caseandWalkIn lists
		static int NumCasesOnList( 0 ); // Used to read caseandWalkIn lists
		static int NumChillersInSet( 0 );
		static int NumCoilsOnList( 0 ); // Used to read caseandWalkIn lists
		static int NumWalkInsOnList( 0 ); // Used to read caseandWalkIn lists
		static int RackNum( 0 ); // Index of refrigerated display case compressor rack
		static int RefrigIndex( 0 ); // Index used in fluid property routines
		static int RefrigSysNum( 0 ); // Index of refrigeration system
		static int TransRefrigSysNum( 0 ); // Index of transcritical CO2 refrigeration system
		static int SecondaryIndex( 0 ); // Index of secondary loops
		static int SecondaryID( 0 ); // Index of secondary loops
		static int SetID( 0 ); // Index of refrigerated chilller SETS
		static int SecondaryNum( 0 ); // Index of secondary loops
		//INTEGER    :: TransferLoadListIndex = 0      ! Index of TransferLoad lists
		//INTEGER    :: TransferLoadListID    = 0      ! Index of TransferLoad lists
		static int TransferLoadListNum( 0 ); // Index of TransferLoad lists
		//  INTEGER    :: InputType         = 0       ! Type of inlet, capcity in W or brine flow rate in m3/s
		static int SubcoolerNum( 0 ); // Index of subcooler
		static int TSNum( 0 ); // Counter for time steps in hour
		static int WalkInIndex( 0 ); // Index of walk ins
		static int WalkInID( 0 ); // Index of walk ins
		static int WalkInNum( 0 ); // Index of walk ins
		static int TotFields( 0 ); // Used to calc number of zones on input for walk in coolers
		static int ZoneID( 0 ); // Index to zone
		static int ZoneIndex( 0 ); // Index to zone
		static int ZoneNum( 0 ); // Index to zone
		static Real64 CalcCircRate( 0.0 ); // Calculted circ rate in secondary phase change loop, dimensionless
		static Real64 CalcTotFlowVol( 0.0 ); // Secondary loop flow in phase change liquid overfeed system (m3/s)
		static Real64 CaseHeatGain( 0.0 ); // Case sensible heat gain used for error messages
		static Real64 CapacityAtMaxVolFlow( 0.0 ); // Secondary loop capacity (W)
		static Real64 CpBrineRated( 0.0 ); // specific heat of circ fluid in secondary loop
		static Real64 Capmin( 0.0 ); // min heat rej for heat rej curve for air cooled condenser (W)
		static Real64 Capmax( 0.0 ); // max heat rej for heat rej curve for air cooled condenser (W)
		static Real64 DeltaCap1( 0.0 ); // fraction dif in capacity for input error check
		static Real64 DeltaCap2( 0.0 ); // fraction dif in capacity for input error check
		static Real64 DeltaHPhaseChange( 0.0 ); // Secondary loop enthalpy change in condenser w overfeed system (J/g)
		static Real64 DelTempMin( 0.0 ); // min temperature for heat rej curve for air cooled condenser (C)
		static Real64 DelTempMax( 0.0 ); // max temperature for heat rej curve for air cooled condenser (C)
		static Real64 DensityBrineRated( 0.0 ); // density of circ fluid in secondary loop
		static Real64 DensityPhaseChange( 0.0 ); // Secondary loop density at condensing temperature w overfeed system (g/m3)
		static Real64 DesignSensibleCap( 0.0 ); // Case sensible capacity used for error messages
		static Real64 DiffCircRates( 0.0 ); // Difference between calculated and specified circ rates, fraction
		static Real64 ErrSecondPumpPower( 0.0 ); // Used to check consistency when both head and power input
		static Real64 FlowMassRated( 0.0 ); // Design mass flow rate of circ fluid in secondary loop(kg/s)
		static Real64 GCOutletH( 0.0 ); // Gas cooler outlet enthalpy (J/kg)
		static Real64 NominalSecondaryCapacity( 0.0 ); // Rated Capacity from input data, W
		static Real64 NominalSecondaryRefLoad( 0.0 ); // Load from all connected cases and walkins, W
		static Real64 NominalTotalCascadeLoad( 0.0 ); // Load from all connected cascade condensers, W
		static Real64 NominalTotalCaseCap( 0.0 ); // Total of nominal case capacities, used for rough input check (W)
		static Real64 NominalTotalCoilCap( 0.0 ); // Total of nominal case capacities, used for rough input check (W)
		static Real64 NominalTotalWalkInCap( 0.0 ); // Total of nominal walk-in capacities, used for rough input check (W)
		static Real64 NominalTotalSecondaryCap( 0.0 ); // Total of nominal secondary capacities, used for rough input check (W)
		static Real64 NominalTotalCaseCapMT( 0.0 ); // Total of nominal medium temperature case capacities, used for rough input check (W) (Transcritical CO2)
		static Real64 NominalTotalCaseCapLT( 0.0 ); // Total of nominal low temperature case capacities, used for rough input check (W) (Transcritical CO2)
		static Real64 NominalTotalWalkInCapMT( 0.0 ); // Total of nominal medium temperature walk-in capacities, used for rough input check (W) (Transcritical CO2)
		static Real64 NominalTotalWalkInCapLT( 0.0 ); // Total of nominal low temperature walk-in capacities, used for rough input check (W) (Transcritical CO2)
		static Real64 NominalTotalCoolingCap( 0.0 ); // Total of nominal load capacities, used for rough input check (W)
		static Real64 NominalTotalCompCap( 0.0 ); // Total of nominal compressor capacities, used for rough input check (W)
		static Real64 NominalTotalHiStageCompCap( 0.0 ); // Total of nominal high-stage compressor capacities, used for rough input check (W)
		static Real64 NominalTotalCompCapHP( 0.0 ); // Total of nominal high pressure compressor capacities, used for rough input check (W) (Transcritical CO2)
		static Real64 NominalTotalCompCapLP( 0.0 ); // Total of nominal low pressure compressor capacities, used for rough input check (W) (Transcritical CO2)
		static Real64 NominalCondCap( 0.0 ); // Nominal Condenser capacity, used for rough input check (W)
		static Real64 PCond( 0.0 ); // Condensing Pressure (Pa)
		static Real64 PEvap( 0.0 ); // Evaporating Pressure (Pa)
		static Real64 PumpTotRatedHead( 0.0 ); // Total pump rated head on secondary loop (Pa)
		static Real64 PumpTotRatedFlowVol( 0.0 ); // Rated flow from input pump data, m3/s
		static Real64 Rcase( 0.0 ); // Case thermal resistance used with anti-sweat heater control
		static Real64 RcaseDenom( 0.0 ); // Denominator of case thermal resistance calculation for anti-sweat
		static Real64 SecondaryFlowVolRated( 0.0 ); // Rated flow of secondary fluid, used to calculate capacity (m3/s)
		static Real64 TBrineOutRated( 0.0 ); // Rated temperature of circ fluid LEAVING heat exchanger,C
		static Real64 TBrineInRated( 0.0 ); // Rated temperature of circ fluid going INTO heat exchanger, C
		static Real64 TBrineAverage( 0.0 ); // Rated average of inlet and outlet temps, used for property look up, C
		static Real64 TempRAFraction( 0.0 ); // Temporary sum of Return Air fraction per zone for reporting
		static Real64 TestDelta( 0.0 ); // Used to compare secondary loop rated capacity to calculated capacity, fraction
		Array1D< Real64 > Numbers; // Numeric items for object
		Array2D< Real64 > DayValues; // Array of schedule values

		NumSimulationCascadeCondensers = GetNumObjectsFound( "Refrigeration:Condenser:Cascade" );
		NumSimulationCases = GetNumObjectsFound( "Refrigeration:Case" );
		NumSimulationCaseAndWalkInLists = GetNumObjectsFound( "Refrigeration:CaseAndWalkInList" );
		NumRefrigeratedRacks = GetNumObjectsFound( "Refrigeration:CompressorRack" );
		NumSimulationSecondarySystems = GetNumObjectsFound( "Refrigeration:SecondarySystem" );
		NumSimulationTransferLoadLists = GetNumObjectsFound( "Refrigeration:TransferLoadList" );
		NumSimulationWalkIns = GetNumObjectsFound( "Refrigeration:WalkIn" );
		NumRefrigSystems = GetNumObjectsFound( "Refrigeration:System" );
		NumTransRefrigSystems = GetNumObjectsFound( "Refrigeration:TranscriticalSystem" );
		NumSimulationCondAir = GetNumObjectsFound( "Refrigeration:Condenser:AirCooled" );
		NumSimulationCondEvap = GetNumObjectsFound( "Refrigeration:Condenser:EvaporativeCooled" );
		NumSimulationCondWater = GetNumObjectsFound( "Refrigeration:Condenser:WaterCooled" );
		NumSimulationGasCooler = GetNumObjectsFound( "Refrigeration:GasCooler:AirCooled" );
		NumRefrigCondensers = NumSimulationCondAir + NumSimulationCondEvap + NumSimulationCondWater + NumSimulationCascadeCondensers;
		NumSimulationCompressors = GetNumObjectsFound( "Refrigeration:Compressor" );
		NumSimulationSubcoolers = GetNumObjectsFound( "Refrigeration:Subcooler" );
		NumCompressorLists = GetNumObjectsFound( "Refrigeration:CompressorList" );
		NumRefrigChillerSets = GetNumObjectsFound( "ZoneHVAC:RefrigerationChillerSet" );
		NumSimulationRefrigAirChillers = GetNumObjectsFound( "Refrigeration:AirChiller" );

		// Set flags used later to avoid unnecessary steps.
		if ( NumRefrigeratedRacks == 0 ) HaveRefrigRacks = false;
		if ( NumRefrigSystems == 0 ) HaveDetailedRefrig = false;
		if ( NumTransRefrigSystems == 0 ) HaveDetailedTransRefrig = false;
		if ( NumSimulationCases == 0 && NumSimulationWalkIns == 0 ) HaveCasesOrWalkins = false;
		if ( NumSimulationRefrigAirChillers == 0 ) HaveChillers = false;

		if ( NumRefrigeratedRacks > 0 ) {
			RefrigRack.allocate( NumRefrigeratedRacks );
			HeatReclaimRefrigeratedRack.allocate( NumRefrigeratedRacks );
			ShowCOPWarning.dimension( NumRefrigeratedRacks, true );
		}
		if ( NumRefrigSystems > 0 ) {
			System.allocate( NumRefrigSystems );
			ShowUnmetEnergyWarning.allocate( NumRefrigSystems );
			ShowHiStageUnmetEnergyWarning.allocate( NumRefrigSystems );
			ShowUnmetEnergyWarning = true;
			ShowHiStageUnmetEnergyWarning = true;
		}
		if ( NumTransRefrigSystems > 0 ) {
			TransSystem.allocate( NumTransRefrigSystems );
			ShowUnmetEnergyWarningTrans.dimension( NumTransRefrigSystems, true );
		}
		if ( NumRefrigChillerSets > 0 ) AirChillerSet.allocate( NumRefrigChillerSets );
		if ( NumRefrigCondensers > 0 ) {
			HeatReclaimRefrigCondenser.allocate( NumRefrigCondensers );
			Condenser.allocate( NumRefrigCondensers );
		}
		if ( NumSimulationGasCooler > 0 ) {
			GasCooler.allocate( NumSimulationGasCooler );
		}
		if ( NumSimulationCases > 0 ) {
			CaseRAFraction.allocate( NumOfZones );
			RefrigCase.allocate( NumSimulationCases );
			ShowStockingWarning.dimension( NumSimulationCases, true );
			ShowFrostWarning.dimension( NumSimulationCases, true );
			ShowStoreEnergyWarning.dimension( NumSimulationCases, true );
		}
		if ( NumSimulationWalkIns > 0 ) {
			WalkIn.allocate( NumSimulationWalkIns );
			ShowUnmetWIEnergyWarning.dimension( NumSimulationWalkIns, true );
			ShowWIFrostWarning.dimension( NumSimulationWalkIns, true );
		}
		if ( ( NumSimulationWalkIns > 0 ) || ( NumSimulationCases > 0 ) ) {
			CaseWIZoneReport.allocate( NumOfZones );
		} else {
			UseSysTimeStep = true;
			//needed to avoid accessing unallocated caseWIZoneReport on early call to SumZones
		}
		if ( NumSimulationSecondarySystems > 0 ) {
			Secondary.allocate( NumSimulationSecondarySystems );
			ShowUnmetSecondEnergyWarning.dimension( NumSimulationSecondarySystems, true );
		}
		if ( NumSimulationRefrigAirChillers > 0 ) {
			WarehouseCoil.allocate( NumSimulationRefrigAirChillers );
			ShowCoilFrostWarning.allocate( NumSimulationRefrigAirChillers );
			CoilSysCredit.allocate( NumOfZones );
			ShowCoilFrostWarning = true;
		}
		if ( NumSimulationCompressors > 0 ) Compressor.allocate( NumSimulationCompressors );
		if ( NumSimulationSubcoolers > 0 ) Subcooler.allocate( NumSimulationSubcoolers );
		if ( NumSimulationCaseAndWalkInLists > 0 ) CaseAndWalkInList.allocate( NumSimulationCaseAndWalkInLists );
		if ( NumCompressorLists > 0 ) CompressorLists.allocate( NumCompressorLists );
		if ( NumSimulationTransferLoadLists > 0 ) TransferLoadList.allocate( NumSimulationTransferLoadLists );

		DayValues.allocate( NumOfTimeStepInHour, 24 );
		RefrigPresentInZone.dimension( NumOfZones, false );

		GetObjectDefMaxArgs( "Refrigeration:Case", MaxNumArgs, MaxNumAlphasCase, MaxNumNumbersCase );
		GetObjectDefMaxArgs( "Refrigeration:CaseAndWalkInList", MaxNumArgs, MaxNumAlphasCaseAndWalkInList, MaxNumNumbersCaseAndWalkInList );
		GetObjectDefMaxArgs( "Refrigeration:CompressorRack", MaxNumArgs, MaxNumAlphasRack, MaxNumNumbersRack );
		GetObjectDefMaxArgs( "Refrigeration:System", MaxNumArgs, MaxNumAlphasSys, MaxNumNumbersSys );
		GetObjectDefMaxArgs( "Refrigeration:TranscriticalSystem", MaxNumArgs, MaxNumAlphasTransSys, MaxNumNumbersTransSys );
		GetObjectDefMaxArgs( "Refrigeration:Condenser:AirCooled", MaxNumArgs, MaxNumAlphasConda, MaxNumNumbersConda );
		GetObjectDefMaxArgs( "Refrigeration:Condenser:EvaporativeCooled", MaxNumArgs, MaxNumAlphasConde, MaxNumNumbersConde );
		GetObjectDefMaxArgs( "Refrigeration:Condenser:WaterCooled", MaxNumArgs, MaxNumAlphasCondw, MaxNumNumbersCondw );
		GetObjectDefMaxArgs( "Refrigeration:GasCooler:AirCooled", MaxNumArgs, MaxNumAlphasGasCoolera, MaxNumNumbersGasCoolera );
		GetObjectDefMaxArgs( "Refrigeration:Compressor", MaxNumArgs, MaxNumAlphasComp, MaxNumNumbersComp );
		GetObjectDefMaxArgs( "Refrigeration:CompressorList", MaxNumArgs, MaxNumAlphasCompressorList, MaxNumNumbersCompressorList );
		GetObjectDefMaxArgs( "Refrigeration:WalkIn", MaxNumArgs, MaxNumAlphasWalkIn, MaxNumNumbersWalkIn );
		GetObjectDefMaxArgs( "Refrigeration:SecondarySystem", MaxNumArgs, MaxNumAlphasSecond, MaxNumNumbersSecond );
		GetObjectDefMaxArgs( "ZoneHVAC:RefrigerationChillerSet", MaxNumArgs, MaxNumAlphasChillerSet, MaxNumNumbersChillerSet );
		GetObjectDefMaxArgs( "Refrigeration:AirChiller", MaxNumArgs, MaxNumAlphasAirChiller, MaxNumNumbersAirChiller );

		MaxNumAlphasAll = max( MaxNumAlphasCase, MaxNumAlphasCaseAndWalkInList, MaxNumAlphasRack, MaxNumAlphasSys, MaxNumAlphasTransSys, MaxNumAlphasConda, MaxNumAlphasConde, MaxNumAlphasCondw, MaxNumAlphasGasCoolera, MaxNumAlphasComp, MaxNumAlphasCompressorList, MaxNumAlphasSecond, MaxNumAlphasWalkIn, MaxNumAlphasChillerSet, MaxNumAlphasAirChiller );
		MaxNumNumbersAll = max( MaxNumNumbersCase, MaxNumNumbersCaseAndWalkInList, MaxNumNumbersRack, MaxNumNumbersSys, MaxNumNumbersTransSys, MaxNumNumbersConda, MaxNumNumbersConde, MaxNumNumbersCondw, MaxNumNumbersGasCoolera, MaxNumNumbersComp, MaxNumNumbersCompressorList, MaxNumNumbersSecond, MaxNumNumbersWalkIn, MaxNumNumbersChillerSet, MaxNumNumbersAirChiller );

		Alphas.allocate( MaxNumAlphasAll );
		Numbers.dimension( MaxNumNumbersAll, 0.0 );
		cAlphaFieldNames.allocate( MaxNumAlphasAll );
		cNumericFieldNames.allocate( MaxNumNumbersAll );
		lAlphaBlanks.dimension( MaxNumAlphasAll, true );
		lNumericBlanks.dimension( MaxNumNumbersAll, true );
		//bbb stovall note for future - for all curve entries, see if need fail on type or if can allow table input
		if ( NumSimulationCases > 0 ) {
			CurrentModuleObject = "Refrigeration:Case";
			for ( CaseNum = 1; CaseNum <= NumSimulationCases; ++CaseNum ) {
				GetObjectItem( CurrentModuleObject, CaseNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				++NumDisplayCases;
				IsNotOK = false;
				IsBlank = false;
				AlphaNum = 1;
				VerifyName( Alphas( AlphaNum ), RefrigCase, CaseNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( AlphaNum ) + "\", invalid " + cAlphaFieldNames( AlphaNum ) + "+\"" + Alphas( AlphaNum ) );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				RefrigCase( CaseNum ).Name = Alphas( AlphaNum );

				AlphaNum = 2;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					RefrigCase( CaseNum ).SchedPtr = GetScheduleIndex( Alphas( AlphaNum ) ); // convert schedule name to pointer
					if ( RefrigCase( CaseNum ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					RefrigCase( CaseNum ).SchedPtr = AlwaysOn;
				} // not blank

				//   check availability schedule for values between 0 and 1
				if ( RefrigCase( CaseNum ).SchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( RefrigCase( CaseNum ).SchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				//Get the Zone node number from the zone name entered by the user
				RefrigCase( CaseNum ).ZoneName = Alphas( 3 );
				RefrigCase( CaseNum ).ActualZoneNum = FindItemInList( Alphas( 3 ), Zone );

				if ( RefrigCase( CaseNum ).ActualZoneNum == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 3 ) + " not valid: " + Alphas( 3 ) );
					ErrorsFound = true;
				} else {
					RefrigPresentInZone( RefrigCase( CaseNum ).ActualZoneNum ) = true;
				}

				RefrigCase( CaseNum ).ZoneNodeNum = GetSystemNodeNumberForZone( RefrigCase( CaseNum ).ZoneName );
				RefrigCase( CaseNum ).ZoneRANode = GetReturnAirNodeForZone( RefrigCase( CaseNum ).ZoneName );

				if ( RefrigCase( CaseNum ).ActualZoneNum >= 0 ) {
					if ( RefrigCase( CaseNum ).ZoneNodeNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", System Node Number not found for " + cAlphaFieldNames( 3 ) + " = " + Alphas( 3 ) );
						ShowContinueError( "..Refrigerated cases must reference a controlled Zone (appear in a ZoneHVAC:EquipmentConnections object)." );
						ErrorsFound = true;
					}
				}

				RefrigCase( CaseNum ).RatedAmbientTemp = Numbers( 1 );
				if ( Numbers( 1 ) <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 1 ) + " must be greater than 0 C" );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).RatedAmbientRH = Numbers( 2 );
				if ( Numbers( 2 ) <= 0.0 || Numbers( 2 ) >= 100.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 2 ) + " must be greater than 0% and less than 100%" );
					ErrorsFound = true;
				}
				RefrigCase( CaseNum ).RatedAmbientDewPoint = PsyTdpFnWPb( PsyWFnTdbRhPb( RefrigCase( CaseNum ).RatedAmbientTemp, ( RefrigCase( CaseNum ).RatedAmbientRH / 100.0 ), StdBaroPress ), StdBaroPress );

				RefrigCase( CaseNum ).RateTotCapPerLength = Numbers( 3 );
				if ( Numbers( 3 ) <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 3 ) + " must be greater than 0 W/m" );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).RatedLHR = Numbers( 4 );
				if ( Numbers( 4 ) < 0.0 || Numbers( 4 ) > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 4 ) + " must be >= 0 and <= 1" );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).RatedRTF = Numbers( 5 );
				if ( Numbers( 5 ) <= 0.0 || Numbers( 5 ) > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 5 ) + " must be > 0 and <= to 1" );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).Length = Numbers( 6 );
				if ( Numbers( 6 ) <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 6 ) + " must be greater than 0 m" );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).Temperature = Numbers( 7 );
				if ( RefrigCase( CaseNum ).Temperature >= RefrigCase( CaseNum ).RatedAmbientTemp ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 7 ) + " must be below " + cNumericFieldNames( 1 ) );
					ErrorsFound = true;
				}

				if ( SameString( Alphas( 4 ), "CaseTemperatureMethod" ) ) {
					RefrigCase( CaseNum ).LatentEnergyCurveType = CaseTemperatureMethod;
				} else if ( SameString( Alphas( 4 ), "RelativeHumidityMethod" ) ) {
					RefrigCase( CaseNum ).LatentEnergyCurveType = RHCubic;
				} else if ( SameString( Alphas( 4 ), "DewpointMethod" ) ) {
					RefrigCase( CaseNum ).LatentEnergyCurveType = DPCubic;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 4 ) + "=\"" + Alphas( 4 ) + "\"." );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).LatCapCurvePtr = GetCurveIndex( Alphas( 5 ) ); // convert curve name to number
				if ( RefrigCase( CaseNum ).LatCapCurvePtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 5 ) + " not found:" + Alphas( 5 ) );
					ErrorsFound = true;
				}

				if ( ! SameString( GetCurveType( RefrigCase( CaseNum ).LatCapCurvePtr ), "CUBIC" ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 5 ) + " object must be of type Cubic." );
					ErrorsFound = true;
				}

				NumNum = 8;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).STDFanPower = Numbers( NumNum );
					if ( Numbers( NumNum ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
						ErrorsFound = true;
					}
				} else { //blank use default of 75 W/m
					RefrigCase( CaseNum ).STDFanPower = 75.0;
				} // blank input

				NumNum = 9;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).OperatingFanPower = Numbers( NumNum );
					if ( Numbers( NumNum ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
						ErrorsFound = true;
					}
				} else { // if blank set = to std fan power
					RefrigCase( CaseNum ).OperatingFanPower = RefrigCase( CaseNum ).STDFanPower;
				} // if blank

				NumNum = 10;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).RatedLightingPower = Numbers( NumNum );
					if ( Numbers( NumNum ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
						ErrorsFound = true;
					}
				} else { //blank input - use default of 90 W/m
					RefrigCase( CaseNum ).RatedLightingPower = 90.0;
				} // blank input

				NumNum = 11;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).LightingPower = Numbers( NumNum );
					if ( Numbers( NumNum ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
						ErrorsFound = true;
					}
				} else { // blank input so set lighting power equal to rated/std lighting power
					RefrigCase( CaseNum ).LightingPower = RefrigCase( CaseNum ).RatedLightingPower;
				} // blank input

				if ( ! lAlphaBlanks( 6 ) ) {
					RefrigCase( CaseNum ).LightingSchedPtr = GetScheduleIndex( Alphas( 6 ) ); // convert schedule name to pointer
					if ( RefrigCase( CaseNum ).LightingSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 6 ) + " not found: " + Alphas( 6 ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					RefrigCase( CaseNum ).LightingSchedPtr = AlwaysOn;
				} // not blank

				//   check lighting schedule for values between 0 and 1
				if ( RefrigCase( CaseNum ).LightingSchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( RefrigCase( CaseNum ).LightingSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 6 ) + " = " + Alphas( 6 ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				NumNum = 12;
				RefrigCase( CaseNum ).LightingFractionToCase = 1.0; //default value
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).LightingFractionToCase = Numbers( NumNum );
				} // blank input lighting fraction to case
				//   check lighting fraction to case input
				if ( RefrigCase( CaseNum ).LightingFractionToCase < 0.0 || RefrigCase( CaseNum ).LightingFractionToCase > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " has a value outside the valid range" );
					ShowContinueError( "  Minimum should be >= 0.0 and Maximum should be <= 1.0" );
					ErrorsFound = true;
				}

				NumNum = 13;
				RefrigCase( CaseNum ).AntiSweatPower = Numbers( NumNum );
				if ( Numbers( NumNum ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
					ErrorsFound = true;
				}

				NumNum = 14;
				RefrigCase( CaseNum ).MinimumASPower = Numbers( NumNum );
				if ( Numbers( NumNum ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
					ErrorsFound = true;
				}

				if ( SameString( Alphas( 7 ), "None" ) ) {
					RefrigCase( CaseNum ).AntiSweatControlType = ASNone;
					RefrigCase( CaseNum ).AntiSweatPower = 0.0;
				} else if ( SameString( Alphas( 7 ), "Constant" ) ) {
					RefrigCase( CaseNum ).AntiSweatControlType = ASConstant;
				} else if ( SameString( Alphas( 7 ), "Linear" ) ) {
					RefrigCase( CaseNum ).AntiSweatControlType = ASLinear;
				} else if ( SameString( Alphas( 7 ), "DewpointMethod" ) ) {
					RefrigCase( CaseNum ).AntiSweatControlType = ASDewPoint;
				} else if ( SameString( Alphas( 7 ), "HeatBalanceMethod" ) ) {
					RefrigCase( CaseNum ).AntiSweatControlType = ASHeatBalance;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
					ErrorsFound = true;
				}

				//   Assure that case temperature is below the rated dew point when anti-sweat heater control type is dew point method
				if ( RefrigCase( CaseNum ).Temperature >= RefrigCase( CaseNum ).RatedAmbientDewPoint && RefrigCase( CaseNum ).AntiSweatControlType == ASDewPoint ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 7 ) + " must be below the Rated Ambient Dew Point when " + cAlphaFieldNames( 7 ) + " is Dew Point Method" );
					ErrorsFound = true;
				}

				NumNum = 15;
				//  negative values for minimum humidity are allowed
				RefrigCase( CaseNum ).HumAtZeroAS = Numbers( NumNum );

				//   check minimum humidity when linear AS control type is used
				if ( RefrigCase( CaseNum ).HumAtZeroAS >= RefrigCase( CaseNum ).RatedAmbientRH && RefrigCase( CaseNum ).AntiSweatControlType == ASLinear ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be less than " + cNumericFieldNames( 2 ) );
					ShowContinueError( " for Linear " + cAlphaFieldNames( 7 ) + '.' );
					ErrorsFound = true;
				}

				NumNum = 16;
				RefrigCase( CaseNum ).Height = Numbers( NumNum );
				if ( Numbers( NumNum ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 m" );
					ErrorsFound = true;
				}

				if ( RefrigCase( CaseNum ).Height <= 0.0 && RefrigCase( CaseNum ).AntiSweatControlType == ASHeatBalance ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than 0 when " + cAlphaFieldNames( 7 ) + " is Heat Balance Method." );
					ShowContinueError( "..given " + cNumericFieldNames( NumNum ) + " was: " + RoundSigDigits( RefrigCase( CaseNum ).Height, 3 ) );
					ErrorsFound = true;
				}

				//   initialize case resistance for anti-sweat heater control type = Heat Balance Method
				if ( RefrigCase( CaseNum ).AntiSweatControlType == ASHeatBalance ) {
					if ( RefrigCase( CaseNum ).Height == 0.0 ) {
						Rcase = 0.0;
					} else {
						RcaseDenom = ( ( RefrigCase( CaseNum ).AntiSweatPower / RefrigCase( CaseNum ).Height ) - ( RefrigCase( CaseNum ).RatedAmbientDewPoint - RefrigCase( CaseNum ).RatedAmbientTemp ) / Rair );
						Rcase = ( RefrigCase( CaseNum ).RatedAmbientDewPoint - RefrigCase( CaseNum ).Temperature ) / RcaseDenom;
					}
					RefrigCase( CaseNum ).Rcase = max( 0.0, Rcase );
					if ( RefrigCase( CaseNum ).Rcase == 0.0 ) {
						ShowWarningError( CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\" A case thermal resistance of 0 was calculated for anti-sweat heater performance using the" );
						ShowContinueError( " Heat Balance Method control type. Anti-sweat heater performance cannot be calculated and " + cAlphaFieldNames( 7 ) + " will be set to None and simulation continues." );
						ShowContinueError( " See Engineering Documentation for anti-sweat heater control of refrigerated cases." );
					}
				}

				NumNum = 17;
				RefrigCase( CaseNum ).ASHeaterFractionToCase = Numbers( NumNum );
				if ( Numbers( NumNum ) < 0.0 || Numbers( NumNum ) > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be >= 0 and <= 1" );
					ErrorsFound = true;
				}

				if ( SameString( Alphas( 8 ), "None" ) ) {
					RefrigCase( CaseNum ).DefrostType = DefNone;
				} else if ( SameString( Alphas( 8 ), "OffCycle" ) ) {
					RefrigCase( CaseNum ).DefrostType = DefOffCycle;
				} else if ( ( SameString( Alphas( 8 ), "HotFluid" ) ) || ( SameString( Alphas( 8 ), "HotGas" ) ) ) {
					RefrigCase( CaseNum ).DefrostType = DefHotFluid;
				} else if ( ( SameString( Alphas( 8 ), "HotFluidWithTemperatureTermination" ) ) || ( SameString( Alphas( 8 ), "HotGasWithTemperatureTermination" ) ) ) {
					RefrigCase( CaseNum ).DefrostType = DefHotFluidTerm;
					//   ELSEIF (SameString(Alphas(8),'Hot-Fluid On Demand')) THEN
					//     RefrigCase(CaseNum)%DefrostType = DefHotFluidOnDemand
				} else if ( SameString( Alphas( 8 ), "Electric" ) ) {
					RefrigCase( CaseNum ).DefrostType = DefElectric;
				} else if ( SameString( Alphas( 8 ), "ElectricWithTemperatureTermination" ) ) {
					RefrigCase( CaseNum ).DefrostType = DefElectricTerm;
					//   ELSEIF (SameString(Alphas(8),'Electric On Demand')) THEN
					//     RefrigCase(CaseNum)%DefrostType = DefElectricOnDemand
				} else {
					ShowWarningError( CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 8 ) + "=\"" + Alphas( 8 ) + "\"." );
					ShowContinueError( "Simulation will default to " + cAlphaFieldNames( 8 ) + "=\"None\" and continue." );
					RefrigCase( CaseNum ).DefrostType = DefNone;
				}

				DefType = RefrigCase( CaseNum ).DefrostType;
				NumNum = 18;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).DefrostPower = Numbers( NumNum );
					if ( Numbers( NumNum ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than or equal to 0 W/m" );
						ErrorsFound = true;
					}
					//   disregard defrost power for Off-Cycle or None defrost types
					if ( ( DefType == DefOffCycle || DefType == DefNone ) && ( RefrigCase( CaseNum ).DefrostPower > 0.0 ) ) {
						RefrigCase( CaseNum ).DefrostPower = 0.0;
						ShowWarningError( CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " for " + cAlphaFieldNames( 8 ) + " None or Off-Cycle will be set to 0 and simulation continues." );
					}
				} else {
					RefrigCase( CaseNum ).DefrostPower = 0.0;
				}

				//defrost power needed to calculate heat gain to case even if not needed for electric consumption
				if ( ( DefType == DefHotFluid || DefType == DefHotFluidTerm || DefType == DefElectric || DefType == DefElectricTerm ) && RefrigCase( CaseNum ).DefrostPower <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than 0 W/m for " + cAlphaFieldNames( 8 ) + ' ' + Alphas( 8 ) );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).DefrostSchedPtr = GetScheduleIndex( Alphas( 9 ) ); // convert schedule name to pointer
				if ( RefrigCase( CaseNum ).DefrostSchedPtr == 0 && RefrigCase( CaseNum ).DefrostType != DefNone ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 9 ) + " not found: " + Alphas( 9 ) );
					ShowContinueError( "required when " + cAlphaFieldNames( 8 ) + "=\"" + Alphas( 8 ) + "\"." );
					ErrorsFound = true;
				}

				//   check defrost schedule for values between 0 and 1
				if ( RefrigCase( CaseNum ).DefrostSchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( RefrigCase( CaseNum ).DefrostSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\"." );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 9 ) + " = " + Alphas( 9 ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}
				//   Note that next section counting number cycles and setting maxkgfrost not used now, but may be in the future.
				//   count the number of defrost cycles
				StartCycle = false;
				NumDefCycles = 0;
				DayValues = 0.0;
				GetScheduleValuesForDay( RefrigCase( CaseNum ).DefrostSchedPtr, DayValues, 1 );
				for ( HRNum = 1; HRNum <= 24; ++HRNum ) {
					for ( TSNum = 1; TSNum <= NumOfTimeStepInHour; ++TSNum ) {
						if ( DayValues( TSNum, HRNum ) > 0.0 ) {
							if ( ! StartCycle ) {
								++NumDefCycles;
								StartCycle = true;
							}
						} else {
							StartCycle = false;
						}
					}
				}

				if ( NumDefCycles > 0 ) {
					//     calculate maximum frost formation based on defrost schedule, heat of vaporization+fusion for water = 2833.0 kJ/kg
					RefrigCase( CaseNum ).MaxKgFrost = ( RefrigCase( CaseNum ).RateTotCapPerLength * RefrigCase( CaseNum ).RatedLHR * RefrigCase( CaseNum ).RatedRTF * SecondsPerHour * HoursPerDay / 1000.0 / 2833.0 ) / ( NumDefCycles );
				} else {
					RefrigCase( CaseNum ).MaxKgFrost = 9999999.9;
				}

				//   some defrost types do not use drip-down schedules, use same defrost schedule pointer in that case
				if ( ! lAlphaBlanks( 10 ) ) {
					RefrigCase( CaseNum ).DefrostDripDownSchedPtr = GetScheduleIndex( Alphas( 10 ) ); // convert schedule name to pointer
					if ( RefrigCase( CaseNum ).DefrostDripDownSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 10 ) + " not found: " + Alphas( 10 ) );
						ErrorsFound = true;
					}
				} else {
					RefrigCase( CaseNum ).DefrostDripDownSchedPtr = RefrigCase( CaseNum ).DefrostSchedPtr;
				}

				//   check defrost drip-down schedule for values between 0 and 1
				if ( RefrigCase( CaseNum ).DefrostDripDownSchedPtr > 0 && ( ! lAlphaBlanks( 10 ) ) ) {
					if ( ! CheckScheduleValueMinMax( RefrigCase( CaseNum ).DefrostDripDownSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\"." );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 10 ) + " = " + Alphas( 10 ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				if ( SameString( Alphas( 11 ), "CaseTemperatureMethod" ) ) {
					RefrigCase( CaseNum ).DefrostEnergyCurveType = CaseTemperatureMethod;
				} else if ( SameString( Alphas( 11 ), "RelativeHumidityMethod" ) ) {
					RefrigCase( CaseNum ).DefrostEnergyCurveType = RHCubic;
				} else if ( SameString( Alphas( 11 ), "DewpointMethod" ) ) {
					RefrigCase( CaseNum ).DefrostEnergyCurveType = DPCubic;
				} else if ( SameString( Alphas( 11 ), "None" ) ) {
					RefrigCase( CaseNum ).DefrostEnergyCurveType = None;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).DefCapCurvePtr = GetCurveIndex( Alphas( 12 ) ); // convert curve name to number
				if ( ( RefrigCase( CaseNum ).DefrostType == DefElectricTerm || RefrigCase( CaseNum ).DefrostType == DefHotFluidTerm ) && ( RefrigCase( CaseNum ).DefCapCurvePtr == 0 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 12 ) + " not found:" + Alphas( 12 ) );
					ErrorsFound = true;
				}

				if ( RefrigCase( CaseNum ).DefCapCurvePtr > 0 ) {
					if ( ! SameString( GetCurveType( RefrigCase( CaseNum ).DefCapCurvePtr ), "CUBIC" ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 12 ) + " must be of type Cubic." );
						ErrorsFound = true;
					}
				}

				//  warn user if defrost energy curve is entered that it is only used for temperature termination types
				if ( RefrigCase( CaseNum ).DefCapCurvePtr > 0 ) {
					if ( RefrigCase( CaseNum ).DefrostType != DefElectricTerm && RefrigCase( CaseNum ).DefrostType != DefHotFluidTerm ) {
						ShowWarningError( CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 12 ) + " is only applicable to Defrost Temperature Termination types." );
						ShowContinueError( cAlphaFieldNames( 12 ) + " will be disregarded and simulation continues." );
					}
				}

				NumNum = 19;
				RefrigCase( CaseNum ).RAFrac = Numbers( NumNum );
				if ( Numbers( NumNum ) < 0.0 || Numbers( NumNum ) > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be >= 0 or <= 1 " );
					ErrorsFound = true;
				}

				// set flag in Zone Data if RAFrac > 0
				if ( RefrigCase( CaseNum ).RAFrac > 0.0 ) {
					Zone( RefrigCase( CaseNum ).ActualZoneNum ).RefrigCaseRA = true;
				}

				//   Make sure RA node exists for display cases with under case HVAC returns
				if ( RefrigCase( CaseNum ).ZoneRANode == 0 && RefrigCase( CaseNum ).RAFrac > 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", " + cNumericFieldNames( 18 ) + " not applicable to zones without return air systems." );
					ErrorsFound = true;
				}

				if ( RefrigCase( CaseNum ).ActualZoneNum != 0 ) {
					CaseRAFraction( RefrigCase( CaseNum ).ActualZoneNum ).TotalCaseRAFraction += RefrigCase( CaseNum ).RAFrac;
					CaseRAFraction( RefrigCase( CaseNum ).ActualZoneNum ).ZoneName = RefrigCase( CaseNum ).ZoneName;
				}

				RefrigCase( CaseNum ).StockingSchedPtr = GetScheduleIndex( Alphas( 13 ) ); // convert schedule name to pointer
				if ( ! lAlphaBlanks( 13 ) ) {
					if ( RefrigCase( CaseNum ).StockingSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 13 ) + " not found: " + Alphas( 13 ) );
						ErrorsFound = true;
					}
				} else {
					RefrigCase( CaseNum ).StockingSchedPtr = 0;
				}

				//   calculate sensible case load at design conditions
				DesignSensibleCap = RefrigCase( CaseNum ).RateTotCapPerLength * ( 1.0 - RefrigCase( CaseNum ).RatedLHR ) * RefrigCase( CaseNum ).RatedRTF * RefrigCase( CaseNum ).Length;

				//   calculate case heat gain = lights + fans + anti-sweat
				CaseHeatGain = ( ( RefrigCase( CaseNum ).RatedLightingPower * RefrigCase( CaseNum ).LightingFractionToCase ) + ( RefrigCase( CaseNum ).AntiSweatPower * RefrigCase( CaseNum ).ASHeaterFractionToCase ) + RefrigCase( CaseNum ).STDFanPower ) * RefrigCase( CaseNum ).Length;

				//   sensible case credits are calculated as the difference between the design sensible capacity and the case heat gain
				RefrigCase( CaseNum ).DesignSensCaseCredit = DesignSensibleCap - CaseHeatGain;

				//   compare case loads to design capacity
				if ( DesignSensibleCap < CaseHeatGain ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", the sum of lighting, fan, and anti-sweat heater energy is greater than refrigerated case sensible capacity" );
					ErrorsFound = true;
				}

				RefrigCase( CaseNum ).CaseCreditFracSchedPtr = GetScheduleIndex( Alphas( 14 ) ); // convert schedule name to pointer
				if ( ! lAlphaBlanks( 14 ) ) {
					if ( RefrigCase( CaseNum ).CaseCreditFracSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\", invalid  " + cAlphaFieldNames( 14 ) + " not found: " + Alphas( 14 ) );
						ErrorsFound = true;
					}
				} else {
					RefrigCase( CaseNum ).CaseCreditFracSchedPtr = 0;
				}

				//   check case credit fraction schedule for values between 0 and 1
				if ( RefrigCase( CaseNum ).CaseCreditFracSchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( RefrigCase( CaseNum ).CaseCreditFracSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\"." );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 14 ) + " = " + Alphas( 14 ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				RefrigCase( CaseNum ).DesignRatedCap = RefrigCase( CaseNum ).RateTotCapPerLength * RefrigCase( CaseNum ).Length;
				RefrigCase( CaseNum ).DesignLatentCap = RefrigCase( CaseNum ).DesignRatedCap * RefrigCase( CaseNum ).RatedLHR * RefrigCase( CaseNum ).RatedRTF;
				RefrigCase( CaseNum ).DesignDefrostCap = RefrigCase( CaseNum ).DefrostPower * RefrigCase( CaseNum ).Length;
				RefrigCase( CaseNum ).DesignLighting = RefrigCase( CaseNum ).LightingPower * RefrigCase( CaseNum ).Length;
				RefrigCase( CaseNum ).DesignFanPower = RefrigCase( CaseNum ).OperatingFanPower * RefrigCase( CaseNum ).Length;

				//Design evaporating temperature:  for a DX system, saturated temperature for pressure leaving case
				//                              :  for a liquid system, liquid temperature entering case
				NumNum = 20;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).EvapTempDesign = Numbers( NumNum );
					if ( RefrigCase( CaseNum ).EvapTempDesign >= RefrigCase( CaseNum ).Temperature ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\" " + cNumericFieldNames( NumNum ) + " must be below " + cNumericFieldNames( 7 ) );
						ErrorsFound = true;
					}
				} else {
					RefrigCase( CaseNum ).EvapTempDesign = RefrigCase( CaseNum ).Temperature - DelEvapTDefault;
					//    default 5C less than case operating temperature
				}

				NumNum = 21;
				if ( ! lNumericBlanks( NumNum ) ) {
					RefrigCase( CaseNum ).RefrigInventory = Numbers( NumNum );
					RefrigCase( CaseNum ).DesignRefrigInventory = RefrigCase( CaseNum ).RefrigInventory * RefrigCase( CaseNum ).Length;
					if ( RefrigCase( CaseNum ).RefrigInventory < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigCase( CaseNum ).Name + "\" " + cNumericFieldNames( NumNum ) + " must be a positive number." );
						ErrorsFound = true;
					}
				} else {
					RefrigCase( CaseNum ).RefrigInventory = 0.0;
				}

			} //Individual refrigerated cases
		} //(NumSimulationCases > 0 )

		//************ START WALK IN COOLER INPUT **************

		if ( NumSimulationWalkIns > 0 ) {
			CurrentModuleObject = "Refrigeration:WalkIn";
			for ( WalkInID = 1; WalkInID <= NumSimulationWalkIns; ++WalkInID ) {
				GetObjectItem( CurrentModuleObject, WalkInID, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), WalkIn, WalkInID - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );

				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined name=\"" + Alphas( 1 ) + "\"." );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				WalkIn( WalkInID ).Name = Alphas( 1 );

				if ( ! lAlphaBlanks( 2 ) ) {
					WalkIn( WalkInID ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
					if ( WalkIn( WalkInID ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( 2 ) + " not found: " + Alphas( 2 ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					WalkIn( WalkInID ).SchedPtr = AlwaysOn;
				} // not blank

				//   check availability schedule for values between 0 and 1
				if ( WalkIn( WalkInID ).SchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).SchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 2 ) + " = " + Alphas( 2 ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				WalkIn( WalkInID ).DesignRatedCap = Numbers( 1 );
				if ( Numbers( 1 ) <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 1 ) + " must be greater than 0 W" );
					ErrorsFound = true;
				}

				if ( ! lNumericBlanks( 2 ) ) {
					WalkIn( WalkInID ).Temperature = Numbers( 2 );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 2 ) + " must be input " );
					ErrorsFound = true;
				}

				if ( ! lNumericBlanks( 3 ) ) {
					WalkIn( WalkInID ).TEvapDesign = Numbers( 3 );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 3 ) + " must be input" );
					ErrorsFound = true;
				}

				if ( ! lNumericBlanks( 4 ) ) {
					WalkIn( WalkInID ).HeaterPower = Numbers( 4 );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 4 ) + " must be input " );
					ErrorsFound = true;
				}

				AlphaNum = 3;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					WalkIn( WalkInID ).HeaterSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) ); // convert heater schedule name to pointer
					if ( WalkIn( WalkInID ).HeaterSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					WalkIn( WalkInID ).HeaterSchedPtr = AlwaysOn;
				} // not blank

				//   check heater schedule for values between 0 and 1
				if ( WalkIn( WalkInID ).HeaterSchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).HeaterSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				if ( ! lNumericBlanks( 5 ) && Numbers( 5 ) > 0.0 ) {
					WalkIn( WalkInID ).CoilFanPower = Numbers( 5 );
				} else {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 5 ) + " was not input or was less than 0 and default of 375.0 W will be used " );
					WalkIn( WalkInID ).CoilFanPower = 375.0; //default value = 1/2 hp
				}

				if ( lNumericBlanks( 6 ) ) {
					WalkIn( WalkInID ).CircFanPower = 0.0;
				} else {
					WalkIn( WalkInID ).CircFanPower = Numbers( 6 );
					if ( Numbers( 7 ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 6 ) + " must be greater than >= 0 W" );
						ErrorsFound = true;
					}
				}

				if ( ! lNumericBlanks( 7 ) ) {
					WalkIn( WalkInID ).DesignLighting = Numbers( 7 );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\" " + cNumericFieldNames( 7 ) + " must be input " );
					ErrorsFound = true;
				}

				AlphaNum = 4;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					WalkIn( WalkInID ).LightingSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) ); // convert lighting schedule name to pointer
					if ( WalkIn( WalkInID ).LightingSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					WalkIn( WalkInID ).LightingSchedPtr = AlwaysOn;
				} // schedule name not blank
				//   check Lighting schedule for values between 0 and 1
				if ( WalkIn( WalkInID ).LightingSchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).LightingSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				//Input walk-in cooler defrost information
				AlphaNum = 5;
				if ( lAlphaBlanks( AlphaNum ) ) {
					WalkIn( WalkInID ).DefrostType = WalkInDefrostElec;
				} else if ( SameString( Alphas( AlphaNum ), "Electric" ) ) {
					WalkIn( WalkInID ).DefrostType = WalkInDefrostElec;
				} else if ( SameString( Alphas( AlphaNum ), "HotFluid" ) ) {
					WalkIn( WalkInID ).DefrostType = WalkInDefrostFluid;
				} else if ( SameString( Alphas( AlphaNum ), "None" ) ) {
					WalkIn( WalkInID ).DefrostType = WalkInDefrostNone;
				} else if ( SameString( Alphas( AlphaNum ), "OffCycle" ) ) {
					WalkIn( WalkInID ).DefrostType = WalkInDefrostOffCycle;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
					ErrorsFound = true;
				}

				AlphaNum = 6;
				if ( lAlphaBlanks( AlphaNum ) ) {
					WalkIn( WalkInID ).DefrostControlType = DefrostControlSched;
				} else if ( SameString( Alphas( AlphaNum ), "TimeSchedule" ) ) {
					WalkIn( WalkInID ).DefrostControlType = DefrostControlSched;
				} else if ( SameString( Alphas( AlphaNum ), "TemperatureTermination" ) ) {
					WalkIn( WalkInID ).DefrostControlType = DefrostContTempTerm;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				} // defrost control type

				// convert defrost schedule name to pointer
				AlphaNum = 7;
				WalkIn( WalkInID ).DefrostSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) );
				if ( WalkIn( WalkInID ).DefrostSchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				}
				//   check defrost schedule for values between 0 and 1
				if ( WalkIn( WalkInID ).DefrostSchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).DefrostSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + WalkIn( WalkInID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + '=' + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				// convert defrost drip-down schedule name to pointer
				// some defrost types do not use drip-down schedules, use same defrost schedule pointer in that case
				AlphaNum = 8;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					WalkIn( WalkInID ).DefrostDripDownSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) );
					if ( WalkIn( WalkInID ).DefrostDripDownSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					}
					// check schedule for values between 0 and 1
					if ( WalkIn( WalkInID ).DefrostDripDownSchedPtr > 0 ) {
						if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).DefrostDripDownSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\"" );
							ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
							ShowContinueError( "schedule values must be (>=0., <=1.)" );
							ErrorsFound = true;
						}
					}
				} else { //blank input so use drip down schedule for defrost
					WalkIn( WalkInID ).DefrostDripDownSchedPtr = WalkIn( WalkInID ).DefrostSchedPtr;
				}

				if ( WalkIn( WalkInID ).DefrostType == WalkInDefrostOffCycle || WalkIn( WalkInID ).DefrostType == WalkInDefrostNone ) {
					WalkIn( WalkInID ).DefrostCapacity = 0.0;
					//Don't even need to read N8 or N9 for those two defrost types.
				} else { //have electric or hot gas/brine defrost
					if ( ( lNumericBlanks( 8 ) ) || ( Numbers( 8 ) <= 0.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 8 ) + " must be input and greater than or equal to 0 W for " + cAlphaFieldNames( 5 ) + ' ' + Alphas( 5 ) );
						ErrorsFound = true;
					} else {
						WalkIn( WalkInID ).DefrostCapacity = Numbers( 8 );
					} //Blank  or negative N8

					//defaults for defrost energy fraction are 0.7 for elec defrost and 0.3 for warm fluid
					//note this value is only used for temperature terminated defrost control type
					if ( WalkIn( WalkInID ).DefrostType == WalkInDefrostElec ) WalkIn( WalkInID ).DefEnergyFraction = 0.7;
					if ( WalkIn( WalkInID ).DefrostType == WalkInDefrostFluid ) WalkIn( WalkInID ).DefEnergyFraction = 0.3;
					if ( ! lNumericBlanks( 9 ) ) {
						if ( ( Numbers( 9 ) > 1.0 ) || ( Numbers( 9 ) < 0.0 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 9 ) + " must be between 0 and 1, default values will be used." );
						} else {
							WalkIn( WalkInID ).DefEnergyFraction = Numbers( 9 );
						} // number out of range
					} //lnumericblanks
				} // defrost type

				// convert restocking schedule name to pointer, default of 0.1 is assigned inside walkin subroutine if blank
				AlphaNum = 9;
				if ( lAlphaBlanks( AlphaNum ) ) {
					WalkIn( WalkInID ).StockingSchedPtr = 0;
				} else {
					WalkIn( WalkInID ).StockingSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) );
					if ( WalkIn( WalkInID ).StockingSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					}
				} //blank

				WalkIn( WalkInID ).DesignRefrigInventory = 0.0;
				if ( ! lNumericBlanks( 10 ) ) WalkIn( WalkInID ).DesignRefrigInventory = Numbers( 10 );

				if ( ! lNumericBlanks( 11 ) ) {
					WalkIn( WalkInID ).FloorArea = Numbers( 11 );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 11 ) + " must be input" );
					ErrorsFound = true;
				}

				if ( lNumericBlanks( 12 ) ) {
					WalkIn( WalkInID ).FloorUValue = DefaultWISurfaceUValue;
				} else {
					WalkIn( WalkInID ).FloorUValue = Numbers( 12 );
					if ( Numbers( 12 ) <= 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( 12 ) + " must be > 0." );
						ErrorsFound = true;
					}
				}

				//Calculate the number of zones exposed to walk-in based on number of input fields, all integer math,
				// This approach used because last zone could have less than NumWIFieldsPerZone due to optional values
				TotFields = NumNumbers + NumAlphas;
				NumWIFieldsPerZone = NumWIAlphaFieldsPerZone + NumWINumberFieldsPerZone;
				NumWIFieldsTotal = TotFields - NumWIAlphaFieldsBeforeZoneInput - NumWINumberFieldsBeforeZoneInput;
				NumZones = 1;
				if ( NumWIFieldsTotal > NumWIFieldsPerZone ) NumZones = 2;
				if ( NumWIFieldsTotal > ( 2 * NumWIFieldsPerZone ) ) NumZones = 3;
				if ( NumWIFieldsTotal > ( 3 * NumWIFieldsPerZone ) ) NumZones = 4;
				if ( NumWIFieldsTotal > ( 4 * NumWIFieldsPerZone ) ) NumZones = 5;
				if ( NumWIFieldsTotal > ( 5 * NumWIFieldsPerZone ) ) NumZones = 6;
				WalkIn( WalkInID ).NumZones = NumZones;

				// All variables for walk-in/zone interactions need to be allocated after know number of zones
				//Autodesk Missing initialization added below: At least SensZoneCreditRate was used uninitialized
				if ( ! allocated( WalkIn( WalkInID ).ZoneName ) ) WalkIn( WalkInID ).ZoneName.allocate( NumZones );
				if ( ! allocated( WalkIn( WalkInID ).ZoneNum ) ) WalkIn( WalkInID ).ZoneNum.allocate( NumZones ) = 0;
				if ( ! allocated( WalkIn( WalkInID ).ZoneNodeNum ) ) WalkIn( WalkInID ).ZoneNodeNum.allocate( NumZones ) = 0;
				if ( ! allocated( WalkIn( WalkInID ).SurfaceArea ) ) WalkIn( WalkInID ).SurfaceArea.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).UValue ) ) WalkIn( WalkInID ).UValue.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).UValueGlassDr ) ) WalkIn( WalkInID ).UValueGlassDr.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).GlassDoorOpenSchedPtr ) ) WalkIn( WalkInID ).GlassDoorOpenSchedPtr.allocate( NumZones ) = 0;
				if ( ! allocated( WalkIn( WalkInID ).AreaGlassDr ) ) WalkIn( WalkInID ).AreaGlassDr.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).HeightGlassDr ) ) WalkIn( WalkInID ).HeightGlassDr.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).UValueStockDr ) ) WalkIn( WalkInID ).UValueStockDr.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).StockDoorOpenSchedPtr ) ) WalkIn( WalkInID ).StockDoorOpenSchedPtr.allocate( NumZones ) = 0;
				if ( ! allocated( WalkIn( WalkInID ).StockDoorProtectType ) ) WalkIn( WalkInID ).StockDoorProtectType.allocate( NumZones ) = 0;
				if ( ! allocated( WalkIn( WalkInID ).AreaStockDr ) ) WalkIn( WalkInID ).AreaStockDr.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).HeightStockDr ) ) WalkIn( WalkInID ).HeightStockDr.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).SensZoneCreditRate ) ) WalkIn( WalkInID ).SensZoneCreditRate.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).SensZoneCreditCoolRate ) ) WalkIn( WalkInID ).SensZoneCreditCoolRate.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).SensZoneCreditCool ) ) WalkIn( WalkInID ).SensZoneCreditCool.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).SensZoneCreditHeatRate ) ) WalkIn( WalkInID ).SensZoneCreditHeatRate.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).SensZoneCreditHeat ) ) WalkIn( WalkInID ).SensZoneCreditHeat.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).LatZoneCreditRate ) ) WalkIn( WalkInID ).LatZoneCreditRate.allocate( NumZones ) = 0.0;
				if ( ! allocated( WalkIn( WalkInID ).LatZoneCredit ) ) WalkIn( WalkInID ).LatZoneCredit.allocate( NumZones ) = 0.0;

				AStart = NumWIAlphaFieldsBeforeZoneInput + 1;
				NStart = NumWINumberFieldsBeforeZoneInput + 1;
				for ( ZoneID = 1; ZoneID <= NumZones; ++ZoneID ) {
					//Get the Zone node number from the zone name
					//The Zone Node is needed to get the zone's ambient conditions, NumOfZones from dataglobals
					WalkIn( WalkInID ).ZoneName( ZoneID ) = Alphas( AStart );
					WalkIn( WalkInID ).ZoneNum( ZoneID ) = FindItemInList( Alphas( AStart ), Zone );

					if ( WalkIn( WalkInID ).ZoneNum( ZoneID ) == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AStart ) + " not valid: " + Alphas( AStart ) );
						ErrorsFound = true;
					} else {
						RefrigPresentInZone( WalkIn( WalkInID ).ZoneNum( ZoneID ) ) = true;
					}
					WalkIn( WalkInID ).ZoneNodeNum( ZoneID ) = GetSystemNodeNumberForZone( WalkIn( WalkInID ).ZoneName( ZoneID ) );
					if ( WalkIn( WalkInID ).ZoneNum( ZoneID ) >= 0 ) {
						if ( WalkIn( WalkInID ).ZoneNodeNum( ZoneID ) == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\" System Node Number not found for " + cAlphaFieldNames( AStart ) + " = " + Alphas( AStart ) );
							ShowContinueError( ".. Walk Ins must reference a controlled Zone (appear in a ZoneHVAC:EquipmentConnections object." );
							ErrorsFound = true;
						}
					}

					if ( ! lNumericBlanks( NStart ) ) {
						WalkIn( WalkInID ).SurfaceArea( ZoneID ) = Numbers( NStart );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", " + cNumericFieldNames( NStart ) + " must be input for Zone: " + WalkIn( WalkInID ).ZoneName( ZoneID ) );
						ErrorsFound = true;
					}

					if ( lNumericBlanks( NStart + 1 ) ) {
						WalkIn( WalkInID ).UValue( ZoneID ) = DefaultWISurfaceUValue;
					} else {
						WalkIn( WalkInID ).UValue( ZoneID ) = Numbers( NStart + 1 );
						if ( Numbers( NStart + 1 ) <= 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", Zone=\"" + WalkIn( WalkInID ).ZoneName( ZoneID ) + "\", " + cNumericFieldNames( NStart + 1 ) + " must be > 0." );
							ErrorsFound = true;
						}
					}

					//start IF set for glass doors in this zone
					WalkIn( WalkInID ).AreaGlassDr( ZoneID ) = 0.0;
					WalkIn( WalkInID ).HeightGlassDr( ZoneID ) = 0.0;
					WalkIn( WalkInID ).UValueGlassDr( ZoneID ) = 0.0;
					if ( ! lNumericBlanks( NStart + 2 ) ) {
						WalkIn( WalkInID ).AreaGlassDr( ZoneID ) = Numbers( NStart + 2 );

						WalkIn( WalkInID ).HeightGlassDr( ZoneID ) = DefaultWIHeightGlassDr;
						if ( ! lNumericBlanks( NStart + 3 ) ) WalkIn( WalkInID ).HeightGlassDr( ZoneID ) = Numbers( NStart + 3 );

						WalkIn( WalkInID ).UValueGlassDr( ZoneID ) = DefaultWIUValueGlassDr;
						if ( ! lNumericBlanks( NStart + 4 ) ) WalkIn( WalkInID ).UValueGlassDr( ZoneID ) = Numbers( NStart + 4 );

						// convert door opening schedule name to pointer, default of 0.1 is assigned inside walkin subroutine if blank
						if ( lAlphaBlanks( AStart + 1 ) ) {
							WalkIn( WalkInID ).GlassDoorOpenSchedPtr( ZoneID ) = 0;
						} else {
							WalkIn( WalkInID ).GlassDoorOpenSchedPtr( ZoneID ) = GetScheduleIndex( Alphas( AStart + 1 ) );
							if ( WalkIn( WalkInID ).GlassDoorOpenSchedPtr( ZoneID ) == 0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", Zone=\"" + WalkIn( WalkInID ).ZoneName( ZoneID ) + "\", invalid  " + cAlphaFieldNames( AStart + 1 ) + " not found: " + Alphas( AStart + 1 ) );
								ErrorsFound = true;
							} else {
								//       check schedule for values between 0 and 1
								if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).GlassDoorOpenSchedPtr( ZoneID ), ">=", 0.0, "<=", 1.0 ) ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", Zone=\"" + WalkIn( WalkInID ).ZoneName( ZoneID ) + "\"" );
									ShowContinueError( "Error found in " + cAlphaFieldNames( AStart + 1 ) + " = " + Alphas( AStart + 1 ) );
									ShowContinueError( "schedule values must be (>=0., <=1.)" );
									ErrorsFound = true;
								} //schedule values outside range
							} // have schedule pointer
						} //blank on door opening schedule (AStart + 1)
					} // have glassdoor area facing zone (blank on lNumericBlanks(NStart+2))

					//start IF set for stock doors in this zone
					WalkIn( WalkInID ).AreaStockDr( ZoneID ) = 0.0;
					WalkIn( WalkInID ).HeightStockDr( ZoneID ) = 0.0;
					WalkIn( WalkInID ).UValueStockDr( ZoneID ) = 0.0;
					if ( ! lNumericBlanks( NStart + 5 ) ) {
						WalkIn( WalkInID ).AreaStockDr( ZoneID ) = Numbers( NStart + 5 );

						WalkIn( WalkInID ).HeightStockDr( ZoneID ) = DefaultWIHeightStockDr;
						if ( ! lNumericBlanks( NStart + 6 ) ) WalkIn( WalkInID ).HeightStockDr( ZoneID ) = Numbers( NStart + 6 );

						WalkIn( WalkInID ).UValueStockDr( ZoneID ) = DefaultWIUValueStockDr;
						if ( ! lNumericBlanks( NStart + 7 ) ) WalkIn( WalkInID ).UValueStockDr( ZoneID ) = Numbers( NStart + 7 );

						// convert door opening schedule name to pointer, default of 0.1 is assigned inside walkin subroutine if blank
						if ( lAlphaBlanks( AStart + 2 ) ) {
							WalkIn( WalkInID ).StockDoorOpenSchedPtr( ZoneID ) = 0;
						} else {
							WalkIn( WalkInID ).StockDoorOpenSchedPtr( ZoneID ) = GetScheduleIndex( Alphas( AStart + 2 ) );
							if ( WalkIn( WalkInID ).StockDoorOpenSchedPtr( ZoneID ) == 0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", Zone=\"" + WalkIn( WalkInID ).ZoneName( ZoneID ) + "\", invalid  " + cAlphaFieldNames( AStart + 2 ) + " not found: " + Alphas( AStart + 2 ) );
								ErrorsFound = true;
							} else {
								//       check schedule for values between 0 and 1
								if ( ! CheckScheduleValueMinMax( WalkIn( WalkInID ).StockDoorOpenSchedPtr( ZoneID ), ">=", 0.0, "<=", 1.0 ) ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", Zone=\"" + WalkIn( WalkInID ).ZoneName( ZoneID ) + "\"" );
									ShowContinueError( "Error found in " + cAlphaFieldNames( AStart + 2 ) + " = " + Alphas( AStart + 2 ) );
									ShowContinueError( "schedule values must be (>=0., <=1.)" );
									ErrorsFound = true;
								} //schedule values outside range
							} // have schedule pointer
						} //blank on door opening schedule (AStart + 2)

						if ( lAlphaBlanks( AStart + 3 ) ) {
							//default air curtain
							WalkIn( WalkInID ).StockDoorProtectType( ZoneID ) = WIStockDoorAirCurtain;
						} else if ( SameString( Alphas( AStart + 3 ), "None" ) ) {
							WalkIn( WalkInID ).StockDoorProtectType( ZoneID ) = WIStockDoorNone;
						} else if ( SameString( Alphas( AStart + 3 ), "AirCurtain" ) ) {
							WalkIn( WalkInID ).StockDoorProtectType( ZoneID ) = WIStockDoorAirCurtain;
						} else if ( SameString( Alphas( AStart + 3 ), "StripCurtain" ) ) {
							WalkIn( WalkInID ).StockDoorProtectType( ZoneID ) = WIStockDoorStripCurtain;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WalkIn( WalkInID ).Name + "\", invalid  " + cAlphaFieldNames( AStart + 3 ) + "=\"" + Alphas( AStart + 3 ) + "\"." );
							ErrorsFound = true;
						} //stock door protection (AStart + 3) blank
					} // have Stockdoor area facing zone

					AStart += NumWIAlphaFieldsPerZone;
					NStart += NumWINumberFieldsPerZone;
				} //Zones for Walk Ins
			} //Individual Walk Ins
		} //(NumSimulationWalkIns > 0 )

		//************* Start Indiv Refrig Air Chillers

		if ( NumSimulationRefrigAirChillers > 0 ) {
			CurrentModuleObject = "Refrigeration:AirChiller";
			for ( CoilID = 1; CoilID <= NumSimulationRefrigAirChillers; ++CoilID ) {
				//A1
				AlphaNum = 1;
				GetObjectItem( CurrentModuleObject, CoilID, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( AlphaNum ), WarehouseCoil, CoilID - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );

				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined name=\"" + Alphas( AlphaNum ) + "\"." );
					if ( IsBlank ) Alphas( AlphaNum ) = "xxxxx";
					ErrorsFound = true;
				}
				WarehouseCoil( CoilID ).Name = Alphas( AlphaNum );

				//A2
				++AlphaNum;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).SchedPtr = GetScheduleIndex( Alphas( AlphaNum ) ); // convert schedule name to pointer
					if ( WarehouseCoil( CoilID ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					WarehouseCoil( CoilID ).SchedPtr = AlwaysOn;
				} // not blank

				//   check availability schedule for values between 0 and 1
				if ( WarehouseCoil( CoilID ).SchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( WarehouseCoil( CoilID ).SchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				//Input capacity rating type
				//bbbbb input values (DT1 or DTM type)translate DT1 to DTm here because node will give avg temp?
				//      ask whether ceiling or floor mounted? - effects translation from DT1 to DTM
				//      input coil condition, wet or dry OR input rating basis, European SC1, SC2 etc., have to combine with refrigerant factor)
				//      rated capacity, BAC give W/C, European gives W
				//      fin material factor, default 1
				//      refrigerant factor (factor of both refrigerant and Tevap)

				//A3
				++AlphaNum;
				if ( lAlphaBlanks( AlphaNum ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + cAlphaFieldNames( AlphaNum ) + " is required and not found." );
					ErrorsFound = true;
				} else if ( SameString( Alphas( AlphaNum ), "UnitLoadFactorSensibleOnly" ) ) {
					WarehouseCoil( CoilID ).RatingType = UnitLoadFactorSens;
				} else if ( SameString( Alphas( AlphaNum ), "CapacityTotalSpecificConditions" ) ) {
					WarehouseCoil( CoilID ).RatingType = RatedCapacityTotal;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC1Standard" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC1Std;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC1NominalWet" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC1Nom;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC2Standard" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC2Std;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC2NominalWet" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC2Nom;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC3Standard" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC3Std;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC3NominalWet" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC3Nom;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC4Standard" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC4Std;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC4NominalWet" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC4Nom;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC5Standard" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC5Std;
				} else if ( SameString( Alphas( AlphaNum ), "EuropeanSC5NominalWet" ) ) {
					WarehouseCoil( CoilID ).RatingType = EuropeanSC5Nom;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
					ErrorsFound = true;
				}

				//Here have to do select case with one numeric field with units of W and the second with units of W/deltaC,
				//  (RatedRH field only used for RatedCapacityTotal type)
				{ auto const SELECT_CASE_var( WarehouseCoil( CoilID ).RatingType );
				if ( SELECT_CASE_var == UnitLoadFactorSens ) {
					//N1
					NumNum = 1;
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).UnitLoadFactorSens = Numbers( NumNum );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W/C" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == RatedCapacityTotal ) {
					//N2
					NumNum = 2; //advance past rating in W/C to N2 with rating in W
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedCapTotal = Numbers( NumNum );
						//N3
						NumNum = 3; //read rated RH only for this type of rating at N3
						if ( lNumericBlanks( NumNum ) ) {
							WarehouseCoil( CoilID ).RatedRH = 0.85;
						} else {
							if ( Numbers( NumNum ) <= 0.0 || Numbers( NumNum ) >= 100.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be greater than 0% and less than 100%" );
								ErrorsFound = true;
							}
							WarehouseCoil( CoilID ).RatedRH = Numbers( NumNum ) / 100.0;
						}
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC1Std ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum );
						WarehouseCoil( CoilID ).SCIndex = 1;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC1Nom ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedCapTotal = Numbers( NumNum );
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum ) / EuropeanWetCoilFactor( 1 );
						WarehouseCoil( CoilID ).SCIndex = 1;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC2Std ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum );
						WarehouseCoil( CoilID ).SCIndex = 2;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC2Nom ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedCapTotal = Numbers( NumNum );
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum ) / EuropeanWetCoilFactor( 2 );
						WarehouseCoil( CoilID ).SCIndex = 2;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC3Std ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum );
						WarehouseCoil( CoilID ).SCIndex = 3;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC3Nom ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedCapTotal = Numbers( NumNum );
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum ) / EuropeanWetCoilFactor( 3 );
						WarehouseCoil( CoilID ).SCIndex = 3;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC4Std ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum );
						WarehouseCoil( CoilID ).SCIndex = 4;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC4Nom ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedCapTotal = Numbers( NumNum );
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum ) / EuropeanWetCoilFactor( 4 );
						WarehouseCoil( CoilID ).SCIndex = 4;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC5Std ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum );
						WarehouseCoil( CoilID ).SCIndex = 5;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == EuropeanSC5Nom ) {
					//N2
					NumNum = 2; //advance past rating in W/C to rating in W at N2
					if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
						WarehouseCoil( CoilID ).RatedCapTotal = Numbers( NumNum );
						WarehouseCoil( CoilID ).RatedSensibleCap = Numbers( NumNum ) / EuropeanWetCoilFactor( 5 );
						WarehouseCoil( CoilID ).SCIndex = 5;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and be greater than 0 W" );
						ErrorsFound = true;
					}
				}} //WarehouseCoil(CoilID)%RatingType

				//N4
				NumNum = 4;
				if ( ! lNumericBlanks( NumNum ) ) {
					WarehouseCoil( CoilID ).TEvapDesign = Numbers( NumNum ); //also used to rep inlet brine T later when add that option
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input" );
					ErrorsFound = true;
				}

				++NumNum; //N5
				if ( ! lNumericBlanks( NumNum ) ) {
					WarehouseCoil( CoilID ).RatedTemperatureDif = Numbers( NumNum );
					// INLET temperature - evaporating temperature, NOT room temp - evap temp
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input" );
					ErrorsFound = true;
				}

				++NumNum; //N6
				if ( ! lNumericBlanks( NumNum ) ) {
					WarehouseCoil( CoilID ).MaxTemperatureDif = Numbers( NumNum );
					// Maximum difference between INLET temperature - evaporating temperature, NOT room temp - evap temp
					// Important when cooling down space at start of environment or if large stocking loads imposed.
				} else {
					WarehouseCoil( CoilID ).MaxTemperatureDif = 1.3 * WarehouseCoil( CoilID ).RatedTemperatureDif;
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " not entered, default 1.3 times rated temperature difference will be used." );
				}

				// Correction factor from manufacturer's rating for coil material, default 1.0
				++NumNum; //N7
				WarehouseCoil( CoilID ).CorrMaterial = 1.0; //default value
				if ( ! lNumericBlanks( NumNum ) ) WarehouseCoil( CoilID ).CorrMaterial = Numbers( NumNum );

				// Correction factor from manufacturer's rating for refrigerant, default 1.0
				++NumNum; //N8
				WarehouseCoil( CoilID ).CorrRefrigerant = 1.0; //default value
				if ( ! lNumericBlanks( NumNum ) ) WarehouseCoil( CoilID ).CorrRefrigerant = Numbers( NumNum );
				//ONLY used if the Capacity Rating Type is CapacityTotalSpecificConditions

				//Convert all European sensible capacities to sensible load factors
				if ( ( WarehouseCoil( CoilID ).RatingType != UnitLoadFactorSens ) && ( WarehouseCoil( CoilID ).RatingType != RatedCapacityTotal ) ) WarehouseCoil( CoilID ).UnitLoadFactorSens = WarehouseCoil( CoilID ).RatedSensibleCap / WarehouseCoil( CoilID ).RatedTemperatureDif;
				//Now have UnitLoadFactorSens for all except RatingType == RatedCapacityTotal

				//Apply material and refrigerant correction factors to sensible load factors
				if ( ( WarehouseCoil( CoilID ).RatingType != RatedCapacityTotal ) ) WarehouseCoil( CoilID ).UnitLoadFactorSens *= WarehouseCoil( CoilID ).CorrMaterial * WarehouseCoil( CoilID ).CorrRefrigerant;
				//First calc of ratedsensiblecap for type type unitloadfactorsens
				WarehouseCoil( CoilID ).RatedSensibleCap = WarehouseCoil( CoilID ).UnitLoadFactorSens * WarehouseCoil( CoilID ).RatedTemperatureDif;
				//A4    Enter capacity correction curve type
				AlphaNum = 4;
				if ( ( lAlphaBlanks( AlphaNum ) ) && ( WarehouseCoil( CoilID ).RatingType != RatedCapacityTotal ) ) {
					// For all except RatedCapacityTotal - default to linear capacity factor approximating Nelson August 2010 ASHRAE journal
					WarehouseCoil( CoilID ).SHRCorrectionType = SHR60;
				} else if ( WarehouseCoil( CoilID ).RatingType == RatedCapacityTotal ) {
					// For RatedCapacityTotal, the manufacturer's coil performance map is required
					// Specify the performance map with TabularRHxDT1xTRoom
					WarehouseCoil( CoilID ).SHRCorrectionType = TabularRH_DT1_TRoom;
					if ( ! ( SameString( Alphas( AlphaNum ), "TabularRHxDT1xTRoom" ) ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
						ShowContinueError( "The \"CapacityTotalSpecificConditions\" Capacity Rating Type has been specified for this air chiller.  This rating type requires " );
						ShowContinueError( "the \"TabularRHxDT1xTRoom\" correction curve.  Verify that a valid \"TabularRHxDT1xTRoom\" curve is specified in \"" + cAlphaFieldNames( AlphaNum + 1 ) + "\"." );
					}
				} else if ( SameString( Alphas( AlphaNum ), "LinearSHR60" ) ) {
					WarehouseCoil( CoilID ).SHRCorrectionType = SHR60;
				} else if ( SameString( Alphas( AlphaNum ), "QuadraticSHR" ) ) {
					WarehouseCoil( CoilID ).SHRCorrectionType = QuadraticSHR;
				} else if ( SameString( Alphas( AlphaNum ), "European" ) ) {
					WarehouseCoil( CoilID ).SHRCorrectionType = European;
				} else if ( SameString( Alphas( AlphaNum ), "TabularRHxDT1xTRoom" ) ) {
					WarehouseCoil( CoilID ).SHRCorrectionType = TabularRH_DT1_TRoom;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
					ErrorsFound = true;
				}

				++AlphaNum; //A5
				++NumNum; //N9
				{ auto const SELECT_CASE_var( WarehouseCoil( CoilID ).SHRCorrectionType );
				if ( SELECT_CASE_var == SHR60 ) {
					WarehouseCoil( CoilID ).SHRCorrection60 = 1.48; // reference Nelson, ASHRAE journal August 2010 Fig 2
					if ( ! lNumericBlanks( NumNum ) ) WarehouseCoil( CoilID ).SHRCorrection60 = Numbers( NumNum );
					//(1.66667 would be a perfect effectiveness, 1.0 would be artificial coil that does only sensible)
					if ( WarehouseCoil( CoilID ).SHRCorrection60 > 1.67 ) {
						WarehouseCoil( CoilID ).SHRCorrection60 = 1.67;
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be between 1 and 1.67, 1.67 will be used." );
					}
					if ( WarehouseCoil( CoilID ).SHRCorrection60 < 1.0 ) {
						WarehouseCoil( CoilID ).SHRCorrection60 = 1.0;
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be between 1 and 1.67, 1.00 will be used." );
					}
				} else if ( SELECT_CASE_var == European ) {
					//WarehouseCoil(CoilID)%SHRCorrectionCurvePtr = GetCurveIndex('ChillerEuropeanWetCoilFactor')
					// This is a place holder, currently use embedded constants for European ratings, future may want a curve
				} else if ( SELECT_CASE_var == QuadraticSHR ) {
					WarehouseCoil( CoilID ).SHRCorrectionCurvePtr = GetCurveIndex( Alphas( AlphaNum ) ); // convert curve name to number
					if ( lAlphaBlanks( AlphaNum ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " is blank, required." );
						ErrorsFound = true;
					} else if ( WarehouseCoil( CoilID ).SHRCorrectionCurvePtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " );
						ShowContinueError( "...invalid curve " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					}
					//error checks for curve type entered and curve name
					if ( ! SameString( GetCurveType( WarehouseCoil( CoilID ).SHRCorrectionCurvePtr ), "QUADRATIC" ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " must be of type Quadratic." );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == TabularRH_DT1_TRoom ) {
					WarehouseCoil( CoilID ).SHRCorrectionCurvePtr = GetCurveIndex( Alphas( AlphaNum ) ); // convert curve name to number
					if ( lAlphaBlanks( AlphaNum ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " is blank, required." );
						ErrorsFound = true;
					} else if ( WarehouseCoil( CoilID ).SHRCorrectionCurvePtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " );
						ShowContinueError( "...invalid curve " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					}
					//        IF(WarehouseCoil(CoilID)%SHRCorrectionCurvePtr == 0) THEN
					//          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
					//                           '", not found  '//TRIM(cAlphaFieldNames(AlphaNum)))
					//          ErrorsFound = .TRUE.
					//        END IF !valid table name
				}} //SHRCorrectionType

				++NumNum; //N10
				if ( ! lNumericBlanks( NumNum ) ) {
					WarehouseCoil( CoilID ).HeaterPower = Numbers( NumNum );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input " );
					ErrorsFound = true;
				}

				++AlphaNum; //A6
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).HeaterSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) ); // convert heater schedule name to pointer
					if ( WarehouseCoil( CoilID ).HeaterSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} else { //   check heater schedule for values between 0 and 1
						if ( ! CheckScheduleValueMinMax( WarehouseCoil( CoilID ).HeaterSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\"" );
							ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
							ShowContinueError( "schedule values must be (>=0., <=1.)" );
							ErrorsFound = true;
						} //heater schedule ptr == 0
					} //htr sched == 0
				} else { // lalphaBlanks, no schedule specified
					WarehouseCoil( CoilID ).HeaterSchedPtr = AlwaysOn;
				} // not blank

				//Input fan control type
				++AlphaNum; //A7
				if ( lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).FanType = FanConstantSpeed;
				} else if ( SameString( Alphas( AlphaNum ), "Fixed" ) ) {
					WarehouseCoil( CoilID ).FanType = FanConstantSpeed;
				} else if ( SameString( Alphas( AlphaNum ), "FixedLinear" ) ) {
					WarehouseCoil( CoilID ).FanType = FanConstantSpeedLinear;
				} else if ( SameString( Alphas( AlphaNum ), "VariableSpeed" ) ) {
					WarehouseCoil( CoilID ).FanType = FanVariableSpeed;
				} else if ( SameString( Alphas( AlphaNum ), "TwoSpeed" ) ) {
					WarehouseCoil( CoilID ).FanType = FanTwoSpeed;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
					ErrorsFound = true;
				} //fan control type

				++NumNum; //N11
				if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
					WarehouseCoil( CoilID ).RatedFanPower = Numbers( NumNum );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " was not input or was less than 0 " );
					ErrorsFound = true;
				} //coil fan power

				++NumNum; //N12
				if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) {
					WarehouseCoil( CoilID ).RatedAirVolumeFlow = Numbers( NumNum );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " is required and was not input or was less than 0  " );
				} //air volume flow

				++NumNum; //N13
				WarehouseCoil( CoilID ).FanMinAirFlowRatio = 0.2; //default value
				if ( ! lNumericBlanks( NumNum ) && Numbers( NumNum ) > 0.0 ) WarehouseCoil( CoilID ).FanMinAirFlowRatio = Numbers( NumNum );

				//Input defrost type
				++AlphaNum; //A8
				if ( lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).DefrostType = DefrostElec;
				} else if ( SameString( Alphas( AlphaNum ), "Electric" ) ) {
					WarehouseCoil( CoilID ).DefrostType = DefrostElec;
				} else if ( SameString( Alphas( AlphaNum ), "HotFluid" ) ) {
					WarehouseCoil( CoilID ).DefrostType = DefrostFluid;
				} else if ( SameString( Alphas( AlphaNum ), "None" ) ) {
					WarehouseCoil( CoilID ).DefrostType = DefrostNone;
				} else if ( SameString( Alphas( AlphaNum ), "OffCycle" ) ) {
					WarehouseCoil( CoilID ).DefrostType = DefrostOffCycle;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + "=\"" + Alphas( AlphaNum ) + "\"." );
					ErrorsFound = true;
				} //defrost type

				++AlphaNum; //A9
				if ( lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).DefrostControlType = DefrostControlSched;
				} else if ( SameString( Alphas( AlphaNum ), "TimeSchedule" ) ) {
					WarehouseCoil( CoilID ).DefrostControlType = DefrostControlSched;
				} else if ( SameString( Alphas( AlphaNum ), "TemperatureTermination" ) ) {
					WarehouseCoil( CoilID ).DefrostControlType = DefrostContTempTerm;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				} // defrost control type

				// convert defrost schedule name to pointer
				++AlphaNum; //A10
				WarehouseCoil( CoilID ).DefrostSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) );
				if ( WarehouseCoil( CoilID ).DefrostSchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				} else { //   check defrost schedule for values between 0 and 1
					if ( ! CheckScheduleValueMinMax( WarehouseCoil( CoilID ).DefrostSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + WarehouseCoil( CoilID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + '=' + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					} //checkschedulevalueMinMax
				} //check for valid schedule name

				// convert defrost drip-down schedule name to pointer
				// some defrost types do not use drip-down schedules, use same defrost schedule pointer in that case
				++AlphaNum; //A11
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).DefrostDripDownSchedPtr = GetScheduleIndex( Alphas( AlphaNum ) );
					if ( WarehouseCoil( CoilID ).DefrostDripDownSchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} else { // check schedule for values between 0 and 1
						if ( ! CheckScheduleValueMinMax( WarehouseCoil( CoilID ).DefrostDripDownSchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\"" );
							ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
							ShowContinueError( "schedule values must be (>=0., <=1.)" );
							ErrorsFound = true;
						} //Check schedule value between 0 and 1
					} // Check if drip down schedule name is valid
				} else { // .not. lAlphaBlanks  so use drip down schedule for defrost
					WarehouseCoil( CoilID ).DefrostDripDownSchedPtr = WarehouseCoil( CoilID ).DefrostSchedPtr;
				} // .not. lAlphaBlanks

				++NumNum; //N14
				if ( WarehouseCoil( CoilID ).DefrostType == DefrostOffCycle || WarehouseCoil( CoilID ).DefrostType == DefrostNone ) {
					WarehouseCoil( CoilID ).DefrostCapacity = 0.0;
					//Don't even need to read Defrost capacity for those two defrost types.
				} else { //have electric or hot gas/brine defrost
					if ( ( lNumericBlanks( NumNum ) ) || ( Numbers( NumNum ) <= 0.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be input and greater than or equal to 0 W for " + cAlphaFieldNames( AlphaNum ) + ' ' + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} else {
						WarehouseCoil( CoilID ).DefrostCapacity = Numbers( NumNum );
					} //Blank  or negative Defrost Capacity

					//defaults for defrost energy fraction are 0.7 for elec defrost and 0.3 for warm fluid
					//note this value is only used for temperature terminated defrost control type
					if ( WarehouseCoil( CoilID ).DefrostType == DefrostElec ) WarehouseCoil( CoilID ).DefEnergyFraction = 0.7;
					if ( WarehouseCoil( CoilID ).DefrostType == DefrostFluid ) WarehouseCoil( CoilID ).DefEnergyFraction = 0.3;

					++NumNum; //N15
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( ( Numbers( NumNum ) > 1.0 ) || ( Numbers( NumNum ) < 0.0 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be between 0 and 1, default values will be used." );
						} else {
							WarehouseCoil( CoilID ).DefEnergyFraction = Numbers( NumNum );
						} // number out of range
					} //lnumericblanks
				} // defrost type

				++AlphaNum; //A12
				if ( lAlphaBlanks( AlphaNum ) ) {
					WarehouseCoil( CoilID ).VerticalLocation = Middle; //default position
				} else if ( SameString( Alphas( AlphaNum ), "Ceiling" ) ) {
					WarehouseCoil( CoilID ).VerticalLocation = Ceiling;
				} else if ( SameString( Alphas( AlphaNum ), "Middle" ) ) {
					WarehouseCoil( CoilID ).VerticalLocation = Middle;
				} else if ( SameString( Alphas( AlphaNum ), "Floor" ) ) {
					WarehouseCoil( CoilID ).VerticalLocation = Floor;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + WarehouseCoil( CoilID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				} // Vertical location class

				++NumNum; //N16
				WarehouseCoil( CoilID ).DesignRefrigInventory = 0.0;
				if ( ! lNumericBlanks( NumNum ) ) WarehouseCoil( CoilID ).DesignRefrigInventory = Numbers( NumNum );
			} //NumRefrigAirChillers
		} //NumRefrigerationAirChillers > 0

		//************ START Warehouse Coil SET INPUT **************
		// One Set allowed per zone, but indiv coils within zone can be served by different compressor/condenser systems

		if ( NumRefrigChillerSets > 0 ) {

			CheckChillerSetName.dimension( NumRefrigChillerSets, true );

			CurrentModuleObject = "ZoneHVAC:RefrigerationChillerSet";
			for ( SetID = 1; SetID <= NumRefrigChillerSets; ++SetID ) {
				GetObjectItem( CurrentModuleObject, SetID, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				AlphaNum = 1;
				VerifyName( Alphas( AlphaNum ), AirChillerSet, SetID - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );

				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined name=\"" + Alphas( AlphaNum ) + "\"." );
					if ( IsBlank ) Alphas( AlphaNum ) = "xxxxx";
					ErrorsFound = true;
				}
				AirChillerSet( SetID ).Name = Alphas( AlphaNum );

				AlphaNum = 2;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					AirChillerSet( SetID ).SchedPtr = GetScheduleIndex( Alphas( AlphaNum ) ); // convert schedule name to pointer
					if ( AirChillerSet( SetID ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found: " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					} // ptr == 0
				} else { // no schedule specified
					AirChillerSet( SetID ).SchedPtr = AlwaysOn;
				} // not blank

				//   check availability schedule for values between 0 and 1
				if ( AirChillerSet( SetID ).SchedPtr > 0 ) {
					if ( ! CheckScheduleValueMinMax( AirChillerSet( SetID ).SchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\"" );
						ShowContinueError( "Error found in " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
						ShowContinueError( "schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}

				++AlphaNum;
				AirChillerSet( SetID ).ZoneName = Alphas( AlphaNum );
				AirChillerSet( SetID ).ZoneNum = FindItemInList( Alphas( AlphaNum ), Zone );

				if ( AirChillerSet( SetID ).ZoneNum == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not valid: " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				}
				AirChillerSet( SetID ).ZoneNodeNum = GetSystemNodeNumberForZone( AirChillerSet( SetID ).ZoneName );
				if ( AirChillerSet( SetID ).ZoneNodeNum == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\" System Node Number not found for " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) );
					ShowContinueError( ".. Refrigeration chillers must reference a controlled Zone (appear in a ZoneHVAC:EquipmentConnections object." );
					ErrorsFound = true;
				}
				RefrigPresentInZone( AirChillerSet( SetID ).ZoneNum ) = true;

				++AlphaNum;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					ShowMessage( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " is not used. This is not an error.  Energy is exchanged directly with the zone independent of any air system. " );
					// Node identification reserved for future use.  Currently exchange energy directly with zone outside any air system
					//AirChillerSet(SetID)%NodeNumInlet = &
					//       GetOnlySingleNode(Alphas(AlphaNum),ErrorsFound,TRIM(CurrentModuleObject), &
					//                    AirChillerSet(SetID)%Name,NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
				}

				++AlphaNum;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					ShowMessage( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " is not used. This is not an error.  Energy is exchanged directly with the zone independent of any air system. " );
					// Node identification reserved for future use.  Currently exchange energy directly with zone outside any air system
					//AirChillerSet(SetID)%NodeNumOutlet = &
					//         GetOnlySingleNode(Alphas(AlphaNum),ErrorsFound,TRIM(CurrentModuleObject), &
					//                      AirChillerSet(SetID)%Name,NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
				}

				//An extensible list is used to enter the individual names of each chiller in the set.
				//These chillers will be dispatched in this list order to meet the required zone load
				NumChillersInSet = NumAlphas - AlphaNum;
				AlphaStartList = AlphaNum; //+ 1
				AirChillerSet( SetID ).NumCoils = NumChillersInSet;
				if ( ! allocated( AirChillerSet( SetID ).CoilNum ) ) AirChillerSet( SetID ).CoilNum.allocate( NumChillersInSet );
				for ( ChillerIndex = 1; ChillerIndex <= NumChillersInSet; ++ChillerIndex ) {
					AlphaListNum = AlphaStartList + ChillerIndex;
					if ( ! lAlphaBlanks( AlphaListNum ) ) {
						CoilNum = FindItemInList( Alphas( AlphaListNum ), WarehouseCoil );
						if ( CoilNum == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AirChillerSet( SetID ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaListNum ) + " defined as " + Alphas( AlphaListNum ) );
							ErrorsFound = true;
						} // == 0
						AirChillerSet( SetID ).CoilNum( ChillerIndex ) = CoilNum;
						WarehouseCoil( CoilNum ).ZoneName = AirChillerSet( SetID ).ZoneName;
						WarehouseCoil( CoilNum ).ZoneNum = AirChillerSet( SetID ).ZoneNum;
						WarehouseCoil( CoilNum ).ZoneNodeNum = AirChillerSet( SetID ).ZoneNodeNum;
					} // ! = alphablanks
				} //CoilID over NumChillersInSet
			} // NumChillerSets
		} // NumChillerSets > 0
		//************* End Air Chiller Sets

		//**** Read CaseAndWalkIn Lists **********************************************************
		if ( NumSimulationCaseAndWalkInLists > 0 ) {
			CurrentModuleObject = "Refrigeration:CaseAndWalkInList";
			for ( ListNum = 1; ListNum <= NumSimulationCaseAndWalkInLists; ++ListNum ) {
				GetObjectItem( CurrentModuleObject, ListNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), CaseAndWalkInList, ListNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}

				CaseAndWalkInList( ListNum ).Name = Alphas( 1 );

				// CaseAndWalkInList alphas include CaseAndWalkInList name and one name for each Case or WalkIn in list
				// below allocates larger than needed (each allocated to sum of both), but avoids two loops through input fields
				NumTotalLoadsOnList = NumAlphas - 1;
				if ( ! allocated( CaseAndWalkInList( ListNum ).WalkInItemNum ) ) CaseAndWalkInList( ListNum ).WalkInItemNum.allocate( NumTotalLoadsOnList );
				if ( ! allocated( CaseAndWalkInList( ListNum ).CaseItemNum ) ) CaseAndWalkInList( ListNum ).CaseItemNum.allocate( NumTotalLoadsOnList );
				if ( ! allocated( CaseAndWalkInList( ListNum ).CoilItemNum ) ) CaseAndWalkInList( ListNum ).CoilItemNum.allocate( NumTotalLoadsOnList );

				NumCasesOnList = 0;
				NumCoilsOnList = 0;
				NumWalkInsOnList = 0;
				LoadCount = 0;
				for ( NumLoad = 1; NumLoad <= NumTotalLoadsOnList; ++NumLoad ) {
					AlphaListNum = 1 + NumLoad;
					if ( ! lAlphaBlanks( AlphaListNum ) ) {
						++LoadCount;
						LoadWalkInNum = 0;
						LoadCaseNum = 0;
						LoadCoilNum = 0;
						if ( NumSimulationWalkIns > 0 ) LoadWalkInNum = FindItemInList( Alphas( AlphaListNum ), WalkIn );
						if ( NumSimulationCases > 0 ) LoadCaseNum = FindItemInList( Alphas( AlphaListNum ), RefrigCase );
						if ( NumSimulationRefrigAirChillers > 0 ) LoadCoilNum = FindItemInList( Alphas( AlphaListNum ), WarehouseCoil );
						if ( ( LoadWalkInNum == 0 ) && ( LoadCaseNum == 0 ) && ( LoadCoilNum == 0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaFieldNames( AlphaListNum ) + "\", has an invalid value of " + Alphas( AlphaListNum ) );
							ErrorsFound = true;
						} else if ( ( LoadWalkInNum != 0 ) && ( LoadCaseNum != 0 ) && ( LoadCoilNum != 0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaFieldNames( AlphaListNum ) + "\", " + Alphas( AlphaListNum ) + " Case and WalkIns and Refrigerated Coils cannot have the same name." );
							ErrorsFound = true;
						} else if ( LoadWalkInNum != 0 ) {
							++NumWalkInsOnList;
							CaseAndWalkInList( ListNum ).WalkInItemNum( NumWalkInsOnList ) = LoadWalkInNum;
						} else if ( LoadCaseNum != 0 ) {
							++NumCasesOnList;
							CaseAndWalkInList( ListNum ).CaseItemNum( NumCasesOnList ) = LoadCaseNum;
						} else if ( LoadCoilNum != 0 ) {
							++NumCoilsOnList;
							CaseAndWalkInList( ListNum ).CoilItemNum( NumCoilsOnList ) = LoadCoilNum;
						}
					} //lAlphaBlanks
				} //Num Total Loads on List
				if ( LoadCount == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", \"" + CaseAndWalkInList( ListNum ).Name + "\" : degenerate list All entries were blank." );
					ErrorsFound = true;
				} //loadcount == 0
				CaseAndWalkInList( ListNum ).NumCases = NumCasesOnList;
				CaseAndWalkInList( ListNum ).NumCoils = NumCoilsOnList;
				CaseAndWalkInList( ListNum ).NumWalkIns = NumWalkInsOnList;
			} //ListNum=1,NumSimulationCaseAndWalkInLists
		} //(NumSimulationCaseAndWalkInLists > 0)

		//**** End read CaseAndWalkIn Lists **********************************************************

		//************** Start RefrigerationRacks

		if ( NumRefrigeratedRacks > 0 ) {

			CurrentModuleObject = "Refrigeration:CompressorRack";

			for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {

				GetObjectItem( CurrentModuleObject, RackNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), RefrigRack, RackNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				RefrigRack( RackNum ).Name = Alphas( 1 );
				HeatReclaimRefrigeratedRack( RackNum ).Name = Alphas( 1 );
				HeatReclaimRefrigeratedRack( RackNum ).SourceType = CurrentModuleObject;
				if ( SameString( Alphas( 2 ), "Outdoors" ) ) {
					RefrigRack( RackNum ).HeatRejectionLocation = LocationOutdoors;
				} else if ( SameString( Alphas( 2 ), "Zone" ) ) {
					RefrigRack( RackNum ).HeatRejectionLocation = LocationZone;
					// don't need to set RefrigPresentInZone to .TRUE. here because only allowed to reject heat to zone
					// holding all served cases,  so already set when case read in
				} else {
					RefrigRack( RackNum ).HeatRejectionLocation = LocationOutdoors;
					ShowWarningError( CurrentModuleObject + ", " + cAlphaFieldNames( 1 ) + " = \"" + RefrigRack( RackNum ).Name + "\": " + cAlphaFieldNames( 2 ) + " defined as " + Alphas( 2 ) + " not found. Will assume " + cAlphaFieldNames( 2 ) + " is OUTDOORS and simulation continues." );
				}

				RefrigRack( RackNum ).RatedCOP = Numbers( 1 );

				if ( RefrigRack( RackNum ).RatedCOP <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" " + cNumericFieldNames( 1 ) + " must be greater than 0.0" );
					ErrorsFound = true;
				}

				RefrigRack( RackNum ).COPFTempPtr = GetCurveIndex( Alphas( 3 ) ); // convert curve name to number
				if ( RefrigRack( RackNum ).COPFTempPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 3 ) + " not found:" + Alphas( 3 ) );
					ErrorsFound = true;
				}

				if ( ! SameString( GetCurveType( RefrigRack( RackNum ).COPFTempPtr ), "CUBIC" ) ) {
					if ( ! SameString( GetCurveType( RefrigRack( RackNum ).COPFTempPtr ), "QUADRATIC" ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 3 ) + " object must be of type cubic or quadratic." );
						ErrorsFound = true;
					}
				}

				RefrigRack( RackNum ).CondenserFanPower = Numbers( 2 );
				if ( Numbers( 2 ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" " + cNumericFieldNames( 2 ) + " must be greater than or equal to 0 Watts." );
					ErrorsFound = true;
				}

				RefrigRack( RackNum ).TotCondFTempPtr = GetCurveIndex( Alphas( 4 ) ); // convert curve name to number
				if ( ( ! lAlphaBlanks( 4 ) ) && RefrigRack( RackNum ).TotCondFTempPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 4 ) + " not found:" + Alphas( 4 ) );
					ErrorsFound = true;
				}

				if ( ! lAlphaBlanks( 4 ) ) {
					if ( ! SameString( GetCurveType( RefrigRack( RackNum ).TotCondFTempPtr ), "CUBIC" ) ) {
						if ( ! SameString( GetCurveType( RefrigRack( RackNum ).TotCondFTempPtr ), "QUADRATIC" ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 4 ) + " object must be of type cubic or quadratic." );
							ErrorsFound = true;
						}
					}
				}

				if ( SameString( Alphas( 5 ), "EvaporativelyCooled" ) ) {
					RefrigRack( RackNum ).CondenserType = RefrigCondenserTypeEvap;
					if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
						ShowWarningError( CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" Evap cooled " + cAlphaFieldNames( 5 ) + " not available with " + cAlphaFieldNames( 2 ) + " = Zone." );
						ShowContinueError( cAlphaFieldNames( 5 ) + " reset to Air Cooled and simulation continues." );
						RefrigRack( RackNum ).CondenserType = RefrigCondenserTypeAir;
					}
				} else if ( SameString( Alphas( 5 ), "WaterCooled" ) ) {
					RefrigRack( RackNum ).CondenserType = RefrigCondenserTypeWater;
					if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
						ShowWarningError( CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" Water cooled " + cAlphaFieldNames( 5 ) + " not available with " + cAlphaFieldNames( 2 ) + " = Zone." );
						ShowContinueError( cAlphaFieldNames( 5 ) + " reset to Air Cooled and simulation continues." );
						RefrigRack( RackNum ).CondenserType = RefrigCondenserTypeAir;
					}
				} else {
					RefrigRack( RackNum ).CondenserType = RefrigCondenserTypeAir;
				}
				// Get water-cooled condenser input, if applicable
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeWater ) {
					RefrigRack( RackNum ).InletNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					RefrigRack( RackNum ).OutletNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
					// Check node connections
					TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 6 ), Alphas( 7 ), "RefrigRack Nodes" );
					// Get loop flow type
					if ( SameString( Alphas( 8 ), "VariableFlow" ) ) {
						RefrigRack( RackNum ).FlowType = VariableFlow;
					} else if ( SameString( Alphas( 8 ), "ConstantFlow" ) ) {
						RefrigRack( RackNum ).FlowType = ConstantFlow;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 8 ) + " not recognized: " + Alphas( 8 ) );
						ShowContinueError( "Check input value choices." );
						ErrorsFound = true;
					}
					// Get outlet temperature schedule for variable flow case
					if ( RefrigRack( RackNum ).FlowType == VariableFlow ) {
						if ( lAlphaBlanks( 9 ) ) {
							RefrigRack( RackNum ).OutletTempSchedPtr = 0;
						} else {
							RefrigRack( RackNum ).OutletTempSchedPtr = GetScheduleIndex( Alphas( 9 ) ); // convert schedule name to pointer
						}
						if ( RefrigRack( RackNum ).OutletTempSchedPtr == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 9 ) + " : " + Alphas( 9 ) );
							ShowContinueError( "A schedule with this name is not defined in this input data file." );
							ErrorsFound = true;
						}
					}
					// Get volumetric flow rate if applicable
					if ( RefrigRack( RackNum ).FlowType == ConstantFlow ) {
						RefrigRack( RackNum ).DesVolFlowRate = Numbers( 3 );
						RefrigRack( RackNum ).VolFlowRate = Numbers( 3 );
					}
					// Get maximum flow rates
					RefrigRack( RackNum ).VolFlowRateMax = Numbers( 4 );

					// Check constant flow for max violation, if applicable
					if ( RefrigRack( RackNum ).FlowType == ConstantFlow && RefrigRack( RackNum ).VolFlowRate > Numbers( 4 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" " + cNumericFieldNames( 3 ) + " > " + cNumericFieldNames( 4 ) + '.' );
						ShowContinueError( "Revise flow rates." );
						ErrorsFound = true;
					}
					// Get max/min allowed water temps
					RefrigRack( RackNum ).OutletTempMax = Numbers( 5 );
					RefrigRack( RackNum ).InletTempMin = Numbers( 6 );
					// set hardware limits on Node data structure for plant interactions
					//Node(RefrigRack(RackNum)%InletNode)%MassFlowRateMax = RefrigRack(RackNum)%MassFlowRateMax      !CR7425
					//Node(RefrigRack(RackNum)%InletNode)%MassFlowRateMin = 0.0D0                                    !CR7435
					// set flow request for plant sizing.
					RegisterPlantCompDesignFlow( RefrigRack( RackNum ).InletNode, RefrigRack( RackNum ).VolFlowRateMax );
				} //Water cooled condenser data

				// Get evaporative cooled condenser input
				if ( lAlphaBlanks( 10 ) ) {
					RefrigRack( RackNum ).EvapSchedPtr = 0;
				} else {
					RefrigRack( RackNum ).EvapSchedPtr = GetScheduleIndex( Alphas( 10 ) ); // convert schedule name to pointer
					//   check availability schedule for values >= 0
					if ( RefrigRack( RackNum ).EvapSchedPtr > 0 ) {
						if ( ! CheckScheduleValueMinMax( RefrigRack( RackNum ).EvapSchedPtr, ">=", 0.0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" ." );
							ShowContinueError( "Error found in " + cAlphaFieldNames( 10 ) + " = " + Alphas( 10 ) );
							ShowContinueError( "schedule values must be (>=0.)." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 10 ) + " = " + Alphas( 10 ) );
						ShowContinueError( "A schedule with this name is not defined in this input data file." );
						ErrorsFound = true;
					}
				}

				RefrigRack( RackNum ).EvapEffect = Numbers( 7 );
				if ( RefrigRack( RackNum ).EvapEffect < 0.0 || RefrigRack( RackNum ).EvapEffect > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" " + cNumericFieldNames( 7 ) + " cannot be less than zero or greater than 1.0." );
					ErrorsFound = true;
				}

				RefrigRack( RackNum ).CondenserAirFlowRate = Numbers( 8 );
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && RefrigRack( RackNum ).CondenserAirFlowRate <= 0.0 && RefrigRack( RackNum ).CondenserAirFlowRate != AutoCalculate ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", " + cNumericFieldNames( 8 ) + " cannot be less than or equal to zero." );
					ErrorsFound = true;
				}

				//   Basin heater power as a function of temperature must be greater than or equal to 0
				RefrigRack( RackNum ).BasinHeaterPowerFTempDiff = Numbers( 9 );
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && Numbers( 9 ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", " + cNumericFieldNames( 9 ) + " must be >= 0" );
					ErrorsFound = true;
				}

				RefrigRack( RackNum ).BasinHeaterSetPointTemp = Numbers( 10 );
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && RefrigRack( RackNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", " + cNumericFieldNames( 10 ) + " is less than 2 deg C. Freezing could occur." );
				}

				RefrigRack( RackNum ).EvapPumpPower = Numbers( 11 );
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && RefrigRack( RackNum ).EvapPumpPower < 0.0 && RefrigRack( RackNum ).EvapPumpPower != AutoCalculate ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", " + cNumericFieldNames( 11 ) + " cannot be less than zero." );
					ErrorsFound = true;
				}

				// Get Water System tank connections
				RefrigRack( RackNum ).SupplyTankName = Alphas( 11 );
				if ( lAlphaBlanks( 11 ) ) {
					RefrigRack( RackNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
				} else {
					RefrigRack( RackNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
					SetupTankDemandComponent( RefrigRack( RackNum ).Name, CurrentModuleObject, RefrigRack( RackNum ).SupplyTankName, ErrorsFound, RefrigRack( RackNum ).EvapWaterSupTankID, RefrigRack( RackNum ).EvapWaterTankDemandARRID );
				}

				// Check condenser air inlet node connection
				if ( lAlphaBlanks( 12 ) ) {
					RefrigRack( RackNum ).OutsideAirNodeNum = 0;
				} else {
					RefrigRack( RackNum ).OutsideAirNodeNum = GetOnlySingleNode( Alphas( 12 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsParent );
					if ( ! CheckOutAirNodeNumber( RefrigRack( RackNum ).OutsideAirNodeNum ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", " + cAlphaFieldNames( 12 ) + " not found: " + Alphas( 12 ) );
						ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaBlanks( 13 ) ) RefrigRack( RackNum ).EndUseSubcategory = Alphas( 13 );

				//Read all loads on this rack: cases and walk-ins and coils
				NumCases = 0;
				NumCoils = 0;
				NumWalkIns = 0;
				RefrigRack( RackNum ).NumCases = 0;
				RefrigRack( RackNum ).NumCoils = 0;
				RefrigRack( RackNum ).NumWalkIns = 0;
				RefrigRack( RackNum ).TotalRackLoad = 0.0;

				//   Read display case and walkin assignments for this rack
				AlphaNum = 14;
				if ( lAlphaBlanks( AlphaNum ) ) {
					//No cases or walkins or coils specified, ie, rack has no load
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" : has no loads, must have at least one of: " + cAlphaFieldNames( 14 ) );
					ErrorsFound = true;
				} else { // (.NOT. lAlphaBlanks(AlphaNum))
					// Entry for Alphas(AlphaNum) can be either a Case, WalkIn, Coil, or CaseAndWalkInList name
					CaseAndWalkInListNum = 0;
					CaseNum = 0;
					WalkInNum = 0;
					CoilNum = 0;
					if ( NumSimulationCaseAndWalkInLists > 0 ) CaseAndWalkInListNum = FindItemInList( Alphas( AlphaNum ), CaseAndWalkInList );
					if ( NumSimulationCases > 0 ) CaseNum = FindItemInList( Alphas( AlphaNum ), RefrigCase );
					if ( NumSimulationWalkIns > 0 ) WalkInNum = FindItemInList( Alphas( AlphaNum ), WalkIn );
					if ( NumSimulationRefrigAirChillers > 0 ) CoilNum = FindItemInList( Alphas( AlphaNum ), WarehouseCoil );
					NumNameMatches = 0;
					if ( CaseAndWalkInListNum != 0 ) ++NumNameMatches;
					if ( CaseNum != 0 ) ++NumNameMatches;
					if ( WalkInNum != 0 ) ++NumNameMatches;
					if ( CoilNum != 0 ) ++NumNameMatches;

					if ( NumNameMatches != 1 ) { //name must uniquely point to a list or a single case or walkin
						ErrorsFound = true;
						if ( NumNameMatches == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" : has an invalid " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} else if ( NumNameMatches > 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" : has a non-unique name that could be either a " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} //num matches = 0 or > 1
					} else if ( CaseAndWalkInListNum != 0 ) { //Name points to a CaseAndWalkInList
						NumCoils = CaseAndWalkInList( CaseAndWalkInListNum ).NumCoils;
						NumCases = CaseAndWalkInList( CaseAndWalkInListNum ).NumCases;
						NumWalkIns = CaseAndWalkInList( CaseAndWalkInListNum ).NumWalkIns;
						RefrigRack( RackNum ).NumCoils = NumCoils;
						RefrigRack( RackNum ).NumCases = NumCases;
						RefrigRack( RackNum ).NumWalkIns = NumWalkIns;
						if ( ! allocated( RefrigRack( RackNum ).CoilNum ) ) RefrigRack( RackNum ).CoilNum.allocate( NumCoils );
						RefrigRack( RackNum ).CoilNum( {1,NumCoils} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CoilItemNum( {1,NumCoils} );
						if ( ! allocated( RefrigRack( RackNum ).CaseNum ) ) RefrigRack( RackNum ).CaseNum.allocate( NumCases );
						RefrigRack( RackNum ).CaseNum( {1,NumCases} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CaseItemNum( {1,NumCases} );
						if ( ! allocated( RefrigRack( RackNum ).WalkInNum ) ) RefrigRack( RackNum ).WalkInNum.allocate( NumWalkIns );
						RefrigRack( RackNum ).WalkInNum( {1,NumWalkIns} ) = CaseAndWalkInList( CaseAndWalkInListNum ).WalkInItemNum( {1,NumWalkIns} );
					} else if ( CoilNum != 0 ) { //Name points to a coil
						NumCoils = 1;
						RefrigRack( RackNum ).NumCoils = 1;
						if ( ! allocated( RefrigRack( RackNum ).CoilNum ) ) RefrigRack( RackNum ).CoilNum.allocate( NumCoils );
						RefrigRack( RackNum ).CoilNum( NumCoils ) = CoilNum;
					} else if ( CaseNum != 0 ) { //Name points to a case
						NumCases = 1;
						RefrigRack( RackNum ).NumCases = 1;
						if ( ! allocated( RefrigRack( RackNum ).CaseNum ) ) RefrigRack( RackNum ).CaseNum.allocate( NumCases );
						RefrigRack( RackNum ).CaseNum( NumCases ) = CaseNum;
					} else if ( WalkInNum != 0 ) { //Name points to a walkin
						NumWalkIns = 1;
						RefrigRack( RackNum ).NumWalkIns = 1;
						if ( ! allocated( RefrigRack( RackNum ).WalkInNum ) ) RefrigRack( RackNum ).WalkInNum.allocate( NumWalkIns );
						RefrigRack( RackNum ).WalkInNum( NumWalkIns ) = WalkInNum;
					} //NumNameMatches /= 1
				} //blank input for loads on rack

				if ( NumCases > 0 ) {
					for ( CaseIndex = 1; CaseIndex <= NumCases; ++CaseIndex ) {
						CaseID = RefrigRack( RackNum ).CaseNum( CaseIndex );
						//mark all cases on rack as used by this system (checking for unused or non-unique cases)
						++RefrigCase( CaseID ).NumSysAttach;
						//determine total capacity on rack
						RefrigRack( RackNum ).TotalRackLoad += RefrigCase( CaseID ).DesignRatedCap;
					} //CaseIndex=1,NumCases
					//     check that all refrigerated cases attached to a rack are to the same zone if heat rejection location is "Zone"
					//     however, won't matter if walk-in specified
					if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone && RefrigRack( RackNum ).NumCases > 1 && RefrigCase( RefrigRack( RackNum ).CaseNum( 1 ) ).ActualZoneNum != 0 && NumWalkIns < 1 && NumCoils < 1 ) {
						ZoneNum = RefrigCase( RefrigRack( RackNum ).CaseNum( 1 ) ).ActualZoneNum;
						for ( CaseIndex = 2; CaseIndex <= RefrigRack( RackNum ).NumCases; ++CaseIndex ) {
							if ( RefrigCase( RefrigRack( RackNum ).CaseNum( CaseIndex ) ).ActualZoneNum == ZoneNum ) continue;
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\" : All cases attached to a rack must be in the same zone when " + cAlphaFieldNames( 2 ) + " equals \"Zone\"." );
							ErrorsFound = true;
							break;
						}
					} // heat rejection location is zone
				} // numcases > 0

				if ( NumCoils > 0 ) {
					RefrigRack( RackNum ).CoilFlag = true;
					for ( CoilIndex = 1; CoilIndex <= NumCoils; ++CoilIndex ) {
						CoilNum = RefrigRack( RackNum ).CoilNum( CoilIndex );
						//mark all Coils on rack as used by this system (checking for unused or non-unique Coils)
						++WarehouseCoil( CoilNum ).NumSysAttach;
						//determine total capacity on rack
						RefrigRack( RackNum ).TotalRackLoad += WarehouseCoil( CoilNum ).RatedSensibleCap;
					} //CoilIndex=1,NumCoils
				} //numcoils > 0

				if ( NumWalkIns > 0 ) {
					for ( WalkInIndex = 1; WalkInIndex <= NumWalkIns; ++WalkInIndex ) {
						WalkInID = RefrigRack( RackNum ).WalkInNum( WalkInIndex );
						//mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
						++WalkIn( WalkInID ).NumSysAttach;
						//determine total capacity on rack
						RefrigRack( RackNum ).TotalRackLoad += WalkIn( WalkInID ).DesignRatedCap;
					} //WalkInIndex=1,NumWalkIns
				} //NumWalkins

				if ( NumWalkIns > 0 || NumCoils > 0 ) {
					//Get the heat rejection Zone node number from the zone name entered by the user (if heatrej location = zone)
					if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
						if ( lAlphaBlanks( 15 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + cAlphaFieldNames( 15 ) + " must be input if walkins or AirChillers connected to rack and heat rejection location = zone." );
							ErrorsFound = true;
						} else { // alpha (15) not blank
							RefrigRack( RackNum ).HeatRejectionZoneNum = FindItemInList( Alphas( 15 ), Zone );
							RefrigRack( RackNum ).HeatRejectionZoneNodeNum = GetSystemNodeNumberForZone( Alphas( 15 ) );
							if ( RefrigRack( RackNum ).HeatRejectionZoneNum == 0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + RefrigRack( RackNum ).Name + "\", invalid  " + cAlphaFieldNames( 15 ) + " not valid: " + Alphas( 15 ) );
								ErrorsFound = true;
							} else {
								RefrigPresentInZone( RefrigRack( RackNum ).HeatRejectionZoneNum ) = true;
							} //zonenum == 0
						} // alpha 15 blank
					} // zone heat rej and walk-ins or coils present, must input heat rejection zone
				} //numwalkins or coils > 0

				// set condenser air flow and evap water pump power if autocalculated
				// autocalculate condenser evap water pump if needed
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && RefrigRack( RackNum ).EvapPumpPower == AutoCalculate ) {
					RefrigRack( RackNum ).EvapPumpPower = CondPumpRatePower * RefrigRack( RackNum ).TotalRackLoad;
				}
				// autocalculate evap condenser air volume flow rate if needed
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && RefrigRack( RackNum ).CondenserAirFlowRate == AutoCalculate ) {
					RefrigRack( RackNum ).CondenserAirFlowRate = AirVolRateEvapCond * RefrigRack( RackNum ).TotalRackLoad;
				}

			} //RackNum=1,NumRefrigeratedRacks

			CheckEquipNameRackWaterCondenser.dimension( NumRefrigeratedRacks, true );
		} //(NumRefrigeratedRacks > 0)

		if ( NumRefrigSystems > 0 || NumTransRefrigSystems > 0 ) {

			if ( NumRefrigSystems > 0 && NumRefrigCondensers == 0 ) {
				ShowSevereError( "Refrigeration:System objects were found during input processing, however no Rrefrigeration condenser objects (which may be either: " );
				ShowContinueError( " Refrigeration:Condenser:AirCooled, Refrigeration:Condenser:WaterCooled, Refrigeration:Condenser:EvaporativeCooled,or Refrigeration:Condenser:CascadeCooled) were found." );
				ErrorsFound = true;
			}
			if ( NumTransRefrigSystems > 0 && NumSimulationGasCooler == 0 ) {
				ShowSevereError( "Refrigeration:TranscriticalSystem objects were found during input processing, however no Refrigeration gas cooler objects (Refrigeration:GasCooler:AirCooled) were found." );
				ErrorsFound = true;
			}
			if ( NumSimulationCompressors == 0 ) {
				ShowSevereError( "Refrigeration:System objects were found during input processing, however no Refrigeration:Compressor objects were found." );
				ErrorsFound = true;
			}

			//************ START CONDENSER INPUT  **************

			if ( NumSimulationCondAir > 0 ) {
				CurrentModuleObject = "Refrigeration:Condenser:AirCooled";
				for ( CondNum = 1; CondNum <= NumSimulationCondAir; ++CondNum ) {
					GetObjectItem( CurrentModuleObject, CondNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Condenser, CondNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + " = " + Alphas( 1 ) );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					} //IsNotOK on Verify Name
					Condenser( CondNum ).Name = Alphas( 1 );
					HeatReclaimRefrigCondenser( CondNum ).Name = Alphas( 1 );
					Condenser( CondNum ).CapCurvePtr = GetCurveIndex( Alphas( 2 ) ); // convert curve name to number
					if ( Condenser( CondNum ).CapCurvePtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", invalid  " + cAlphaFieldNames( 2 ) + " not found:" + Alphas( 2 ) );
						ErrorsFound = true;
					}

					//set start of count for number of systems attached to this condenser
					Condenser( CondNum ).NumSysAttach = 0;
					if ( ! allocated( Condenser( CondNum ).SysNum ) ) Condenser( CondNum ).SysNum.allocate( NumRefrigSystems );

					//set CondenserType and rated temperature difference (51.7 - 35)C per ARI 460
					Condenser( CondNum ).CondenserType = RefrigCondenserTypeAir;
					HeatReclaimRefrigCondenser( CondNum ).SourceType = RefrigCondenserTypeAir;
					Condenser( CondNum ).RatedDelT = CondARI460DelT; //= 16.7d0 ,Rated sat cond temp - dry bulb air T for air-cooled Condensers, ARI460
					Condenser( CondNum ).RatedTCondense = CondARI460Tcond;
					if ( Condenser( CondNum ).CapCurvePtr > 0 ) {
						Condenser( CondNum ).RatedCapacity = CurveValue( Condenser( CondNum ).CapCurvePtr, CondARI460DelT );
					}
					//elevation capacity correction on air-cooled condensers, Carrier correlation more conservative than Trane
					Condenser( CondNum ).RatedCapacity *= ( 1.0 - 7.17e-5 * Elevation );
					if ( Condenser( CondNum ).RatedCapacity > 0.0 ) {
						GetCurveMinMaxValues( Condenser( CondNum ).CapCurvePtr, DelTempMin, DelTempMax );
						Capmin = CurveValue( Condenser( CondNum ).CapCurvePtr, DelTempMin ) * ( 1.0 - 7.17e-5 * Elevation ); //Mar 2011 bug fix
						Capmax = CurveValue( Condenser( CondNum ).CapCurvePtr, DelTempMax ) * ( 1.0 - 7.17e-5 * Elevation ); //Mar 2011 bug
						Condenser( CondNum ).TempSlope = ( DelTempMax - DelTempMin ) / ( ( Capmax - Capmin ) ); // * ( 1.0 - 7.17e-5 * Elevation ) ) //Mar 2011 bug fix
						Condenser( CondNum ).MinCondLoad = Capmax - DelTempMax / Condenser( CondNum ).TempSlope;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" Condenser capacity curve per ARI 460 must be input and must be greater than 0 Watts at 16.7C temperature difference." );
						ErrorsFound = true;
					}

					Condenser( CondNum ).RatedSubcool = 0.0; //default value
					if ( ! lNumericBlanks( 1 ) ) Condenser( CondNum ).RatedSubcool = Numbers( 1 );

					// Get fan control type
					if ( SameString( Alphas( 3 ), "Fixed" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanConstantSpeed;
					} else if ( SameString( Alphas( 3 ), "FixedLinear" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanConstantSpeedLinear;
					} else if ( SameString( Alphas( 3 ), "VariableSpeed" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanVariableSpeed;
					} else if ( SameString( Alphas( 3 ), "TwoSpeed" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanTwoSpeed;
					} else {
						Condenser( CondNum ).FanSpeedControlType = FanConstantSpeed; //default
					} //Set fan control type

					if ( ! lNumericBlanks( 2 ) ) Condenser( CondNum ).RatedFanPower = Numbers( 2 );
					if ( ( lNumericBlanks( 2 ) ) || ( Numbers( 2 ) < 0.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 2 ) + " must be input greater than or equal to 0 Watts." );
						ErrorsFound = true;
					}

					Condenser( CondNum ).FanMinAirFlowRatio = 0.2; //default value
					if ( ! lNumericBlanks( 3 ) ) Condenser( CondNum ).FanMinAirFlowRatio = Numbers( 3 );

					// Check condenser air inlet node connection
					// Jan 2011 - added ability to reject heat to a zone from air-cooled condenser
					Condenser( CondNum ).CondenserRejectHeatToZone = false;
					if ( lAlphaBlanks( 4 ) ) {
						Condenser( CondNum ).InletAirNodeNum = 0;
					} else { //see if it's an outside air node name or an indoor zone name,
						//have to check inside first because outside check automatically generates an error message
						Condenser( CondNum ).InletAirZoneNum = FindItemInList( Alphas( 4 ), Zone );
						//need to clearly id node number for air inlet conditions and zone number for casecredit assignment
						if ( Condenser( CondNum ).InletAirZoneNum != 0 ) {
							//set condenser flag (later used to set system flag) and zone flag
							Condenser( CondNum ).InletAirNodeNum = GetSystemNodeNumberForZone( Alphas( 4 ) );
							Condenser( CondNum ).CondenserRejectHeatToZone = true;
							RefrigPresentInZone( Condenser( CondNum ).InletAirZoneNum ) = true;
						} else { // not in a conditioned zone, so see if it's outside
							Condenser( CondNum ).InletAirNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsParent );
							if ( ! CheckOutAirNodeNumber( Condenser( CondNum ).InletAirNodeNum ) ) {
								// not outside and not a zone
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cAlphaFieldNames( 4 ) + " not found: " + Alphas( 4 ) );
								ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node or as a Zone." );
								ErrorsFound = true;
							} //checkoutairnodenumber
						} //InletAirZoneNum \=0
					} // Condenser air inlet node connection

					Condenser( CondNum ).EndUseSubcategory = "";
					if ( ! lAlphaBlanks( 5 ) ) Condenser( CondNum ).EndUseSubcategory = Alphas( 5 );

					Condenser( CondNum ).RefOpCharge = 0.0;
					Condenser( CondNum ).RefReceiverInventory = 0.0;
					Condenser( CondNum ).RefPipingInventory = 0.0;
					if ( ! lNumericBlanks( 4 ) ) Condenser( CondNum ).RefOpCharge = Numbers( 4 );
					if ( ! lNumericBlanks( 5 ) ) Condenser( CondNum ).RefReceiverInventory = Numbers( 5 );
					if ( ! lNumericBlanks( 6 ) ) Condenser( CondNum ).RefPipingInventory = Numbers( 6 );

				} // Read input for REFRIGERATION:Condenser:AirCooled
			} // NumSimulationCondAir > 0

			if ( NumSimulationCondEvap > 0 ) {
				CurrentModuleObject = "Refrigeration:Condenser:EvaporativeCooled";
				for ( CondIndex = 1; CondIndex <= NumSimulationCondEvap; ++CondIndex ) {
					CondNum = CondIndex + NumSimulationCondAir;
					GetObjectItem( CurrentModuleObject, CondIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Condenser, CondNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					} //IsNotOK on Verify Name
					Condenser( CondNum ).Name = Alphas( 1 );
					HeatReclaimRefrigCondenser( CondNum ).Name = Alphas( 1 );

					//set start of count for number of systems attached to this condenser
					Condenser( CondNum ).NumSysAttach = 0;
					if ( ! allocated( Condenser( CondNum ).SysNum ) ) Condenser( CondNum ).SysNum.allocate( NumRefrigSystems );

					//set CondenserType and rated Heat Rejection per ARI 490 rating
					Condenser( CondNum ).CondenserType = RefrigCondenserTypeEvap;
					HeatReclaimRefrigCondenser( CondNum ).SourceType = RefrigCondenserTypeEvap;
					Condenser( CondNum ).RatedTCondense = CondARI490Tcond;
					Condenser( CondNum ).RatedDelT = CondARI490DelT;

					if ( ( ! lNumericBlanks( 1 ) ) && ( Numbers( 1 ) > 0.0 ) ) {
						Condenser( CondNum ).RatedCapacity = Numbers( 1 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 1 ) + " per ARI 490 must be input and must be greater than 0 Watts." );
						ErrorsFound = true;
					}
					//Calculate capacity elevation derate factor per ARI 490 barometric pressure correction factor
					Condenser( CondNum ).EvapElevFact = 1.0 - 3.074e-5 * Elevation;

					Condenser( CondNum ).RatedSubcool = 0.0; //default value
					if ( ( ! lNumericBlanks( 2 ) ) && ( Numbers( 2 ) > 0.0 ) ) Condenser( CondNum ).RatedSubcool = Numbers( 2 );

					// Get fan control type
					if ( SameString( Alphas( 2 ), "Fixed" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanConstantSpeed;
					} else if ( SameString( Alphas( 3 ), "FixedLinear" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanConstantSpeedLinear;
					} else if ( SameString( Alphas( 2 ), "VariableSpeed" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanVariableSpeed;
					} else if ( SameString( Alphas( 2 ), "TwoSpeed" ) ) {
						Condenser( CondNum ).FanSpeedControlType = FanTwoSpeed;
					} else {
						Condenser( CondNum ).FanSpeedControlType = FanConstantSpeed; //default
					} //Set fan control type

					Condenser( CondNum ).RatedFanPower = Numbers( 3 );
					if ( Numbers( 3 ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 3 ) + " must be greater than or equal to 0 Watts." );
						ErrorsFound = true;
					}

					Condenser( CondNum ).FanMinAirFlowRatio = 0.2; //default value
					if ( ! lNumericBlanks( 4 ) ) Condenser( CondNum ).FanMinAirFlowRatio = Numbers( 4 );

					//Enter min and max and default coefficients for evap condenser HRCF correlation
					//Defaults taken from 2008 BAC equipment for R22, R134a, series CXV
					//Correlation coefficients for other manufacturers are very similar per Hugh Henderson's work
					Condenser( CondNum ).EvapCoeff1 = 6.63;
					Condenser( CondNum ).EvapCoeff2 = 0.468;
					Condenser( CondNum ).EvapCoeff3 = 17.93;
					Condenser( CondNum ).EvapCoeff4 = -0.322;
					Condenser( CondNum ).MinCapFacEvap = 0.5;
					Condenser( CondNum ).MaxCapFacEvap = 5.0;
					NumNum = 5; //added warnings if below not blank but unused due to limits
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( Numbers( NumNum ) >= 0.0 ) {
							Condenser( CondNum ).EvapCoeff1 = Numbers( NumNum );
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than 0 and was not used. Default was used." );
						}
					}
					NumNum = 6; // EvapCoeff2 can't be equal to 0 because used in a denominator
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( Numbers( NumNum ) > 0.0 ) {
							Condenser( CondNum ).EvapCoeff2 = Numbers( NumNum );
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than or equal to 0 and was not used. Default was used." );
						}
					}
					NumNum = 7;
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( Numbers( NumNum ) >= 0.0 ) {
							Condenser( CondNum ).EvapCoeff3 = Numbers( NumNum );
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than 0 and was not used. Default was used." );
						}
					}
					NumNum = 8;
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( Numbers( NumNum ) >= -20.0 ) {
							Condenser( CondNum ).EvapCoeff4 = Numbers( NumNum );
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than -20 and was not used. Default was used." );
						}
					}
					NumNum = 9;
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( Numbers( NumNum ) >= 0.0 ) {
							Condenser( CondNum ).MinCapFacEvap = Numbers( NumNum );
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than 0 and was not used. Default was used." );
						}
					}
					NumNum = 10;
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( Numbers( NumNum ) >= 0.0 ) {
							Condenser( CondNum ).MaxCapFacEvap = Numbers( NumNum );
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than 0 and was not used. Default was used." );
						}
					}

					// Check condenser air inlet node connection
					if ( lAlphaBlanks( 3 ) ) {
						Condenser( CondNum ).InletAirNodeNum = 0;
					} else {
						Condenser( CondNum ).InletAirNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsParent );
						if ( ! CheckOutAirNodeNumber( Condenser( CondNum ).InletAirNodeNum ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cAlphaFieldNames( 3 ) + " not found: " + Alphas( 3 ) );
							ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
							ErrorsFound = true;
						}
					} // Condenser air inlet node connection

					NumNum = 11;
					Condenser( CondNum ).RatedAirFlowRate = Numbers( NumNum );
					// Note the autocalculate feature for this value takes place in the system section because
					//  it is a function of the total cooling capacity of the cases served by the condenser

					// Evaporative condensers basin heater power as a function of temperature must be greater than or equal to 0
					NumNum = 12;
					Condenser( CondNum ).BasinHeaterPowerFTempDiff = Numbers( NumNum );
					if ( Numbers( NumNum ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " must be >= 0" );
						ErrorsFound = true;
					}

					NumNum = 13;
					Condenser( CondNum ).BasinHeaterSetPointTemp = 2.0; //default
					if ( ! lNumericBlanks( NumNum ) ) Condenser( CondNum ).BasinHeaterSetPointTemp = Numbers( NumNum );
					if ( Condenser( CondNum ).BasinHeaterSetPointTemp < 2.0 ) {
						ShowWarningError( CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", " + cNumericFieldNames( NumNum ) + " is less than 2 deg C. Freezing could occur." );
					}

					NumNum = 14;
					Condenser( CondNum ).EvapPumpPower = 1000.0; //default
					if ( ! lNumericBlanks( NumNum ) ) Condenser( CondNum ).EvapPumpPower = Numbers( NumNum );
					// Note the autocalculate feature for this value takes place in the system section because
					//  it is a function of the total cooling capacity of the cases served by the condenser

					// Get Evaporative Water System tank connections
					Condenser( CondNum ).SupplyTankName = Alphas( 4 );
					if ( lAlphaBlanks( 4 ) ) {
						Condenser( CondNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
					} else {
						Condenser( CondNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
						SetupTankDemandComponent( Condenser( CondNum ).Name, CurrentModuleObject, Condenser( CondNum ).SupplyTankName, ErrorsFound, Condenser( CondNum ).EvapWaterSupTankID, Condenser( CondNum ).EvapWaterTankDemandARRID );
					}

					if ( lAlphaBlanks( 5 ) ) {
						Condenser( CondNum ).EvapSchedPtr = 0;
					} else {
						Condenser( CondNum ).EvapSchedPtr = GetScheduleIndex( Alphas( 5 ) ); // convert schedule name to pointer
						//   check availability schedule for values >= 0
						if ( Condenser( CondNum ).EvapSchedPtr > 0 ) {
							if ( ! CheckScheduleValueMinMax( Condenser( CondNum ).EvapSchedPtr, ">=", 0.0 ) ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" ." );
								ShowContinueError( "Error found in " + cAlphaFieldNames( 5 ) + " = " + Alphas( 5 ) );
								ShowContinueError( "schedule values must be (>=0.)." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", invalid  " + cAlphaFieldNames( 5 ) + " = " + Alphas( 5 ) );
							ShowContinueError( "A schedule with this name is not defined in this input data file." );
							ErrorsFound = true;
						}
					} // Set Evap Schedule Pointer

					Condenser( CondNum ).EndUseSubcategory = "";
					if ( ! lAlphaBlanks( 6 ) ) Condenser( CondNum ).EndUseSubcategory = Alphas( 6 );

					Condenser( CondNum ).RefOpCharge = 0.0;
					Condenser( CondNum ).RefReceiverInventory = 0.0;
					Condenser( CondNum ).RefPipingInventory = 0.0;
					NumNum = 15;
					if ( ! lNumericBlanks( NumNum ) ) Condenser( CondNum ).RefOpCharge = Numbers( NumNum );
					NumNum = 16;
					if ( ! lNumericBlanks( NumNum ) ) Condenser( CondNum ).RefReceiverInventory = Numbers( NumNum );
					NumNum = 17;
					if ( ! lNumericBlanks( NumNum ) ) Condenser( CondNum ).RefPipingInventory = Numbers( NumNum );
				} // Read input for CONDENSER:REFRIGERATION:EVAPorativeCooled
			} // If NumSimulationCondEvap > 0

			if ( NumSimulationCondWater > 0 ) {
				CurrentModuleObject = "Refrigeration:Condenser:WaterCooled";
				for ( CondIndex = 1; CondIndex <= NumSimulationCondWater; ++CondIndex ) {
					CondNum = CondIndex + NumSimulationCondAir + NumSimulationCondEvap;
					GetObjectItem( CurrentModuleObject, CondIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Condenser, CondNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					} //IsNotOK on Verify Name
					Condenser( CondNum ).Name = Alphas( 1 );
					HeatReclaimRefrigCondenser( CondNum ).Name = Alphas( 1 );

					//set start of count for number of systems attached to this condenser
					Condenser( CondNum ).NumSysAttach = 0;
					if ( ! allocated( Condenser( CondNum ).SysNum ) ) Condenser( CondNum ).SysNum.allocate( NumRefrigSystems );

					//set CondenserType and rated Heat Rejection per ARI 450 rating
					Condenser( CondNum ).CondenserType = RefrigCondenserTypeWater;
					HeatReclaimRefrigCondenser( CondNum ).SourceType = RefrigCondenserTypeWater;
					if ( ( ! lNumericBlanks( 1 ) ) && ( Numbers( 1 ) > 0.0 ) ) {
						Condenser( CondNum ).RatedCapacity = Numbers( 1 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 1 ) + " per ARI 450 must be input and must be greater than 0 Watts." );
						ErrorsFound = true;
					}

					if ( ( ! lNumericBlanks( 2 ) ) && ( Numbers( 2 ) > 0.0 ) ) {
						Condenser( CondNum ).RatedTCondense = Numbers( 2 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 2 ) + " per ARI 450 must be input and must be greater than 0 C." );
						ErrorsFound = true;
					}

					if ( ! lNumericBlanks( 3 ) ) {
						if ( Numbers( 3 ) >= 0.0 ) {
							Condenser( CondNum ).RatedSubcool = Numbers( 3 );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 3 ) + " must be greater than or equal to zero." );
							ErrorsFound = true;
						}
					} else {
						Condenser( CondNum ).RatedSubcool = 0.0; //default value
					}

					if ( ( ! lNumericBlanks( 4 ) ) && ( Numbers( 4 ) > 0.0 ) ) {
						Condenser( CondNum ).RatedWaterInletT = Numbers( 4 );
						Condenser( CondNum ).RatedApproachT = Condenser( CondNum ).RatedTCondense - Numbers( 4 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 4 ) + " must be input and greater than zero." );
						ErrorsFound = true;
					}

					Condenser( CondNum ).InletNode = GetOnlySingleNode( Alphas( 2 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					Condenser( CondNum ).OutletNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
					// Check node connections
					TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 2 ), Alphas( 3 ), "Water Cooled Condenser Nodes" );
					// Get loop flow type
					if ( SameString( Alphas( 4 ), "VariableFlow" ) ) { //set FlowType
						Condenser( CondNum ).FlowType = VariableFlow;
					} else if ( SameString( Alphas( 4 ), "ConstantFlow" ) ) {
						Condenser( CondNum ).FlowType = ConstantFlow;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", invalid  " + cAlphaFieldNames( 4 ) + " not recognized: " + Alphas( 4 ) );
						ShowContinueError( "Check input value choices." );
						ErrorsFound = true;
					} //Set FlowType

					// Get outlet temperature schedule for variable flow case
					if ( Condenser( CondNum ).FlowType == VariableFlow ) {
						if ( lAlphaBlanks( 5 ) ) {
							Condenser( CondNum ).OutletTempSchedPtr = 0;
						} else {
							Condenser( CondNum ).OutletTempSchedPtr = GetScheduleIndex( Alphas( 5 ) ); // convert schedule name to pointer
						}
						if ( Condenser( CondNum ).OutletTempSchedPtr == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", invalid  " + cAlphaFieldNames( 5 ) + " = " + Alphas( 5 ) );
							ShowContinueError( "A schedule with this name is not defined in this input data file." );
							ErrorsFound = true;
						}
					} // Outlet temperature schedule

					// Get volumetric flow rate if applicable
					if ( Condenser( CondNum ).FlowType == ConstantFlow ) {
						if ( ( !lNumericBlanks( 5 ) ) && ( Numbers( 5 ) > 0.0 ) ) {
							Condenser( CondNum ).DesVolFlowRate = Numbers( 5 );
							Condenser( CondNum ).VolFlowRate = Numbers( 5 );
						}
						else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 5 ) + " must be greater than zero." );
							ShowContinueError( "Revise flow rates." );
							ErrorsFound = true;
						}
						RegisterPlantCompDesignFlow( Condenser( CondNum ).InletNode, Condenser( CondNum ).DesVolFlowRate );
					}


					// Get maximum flow rates
					if ( Numbers( 6 ) > 0.0 ) {
						Condenser( CondNum ).VolFlowRateMax = Numbers( 6 );
						// Check constant flow for max violation, if applicable
						if ( Condenser( CondNum ).FlowType == ConstantFlow && Condenser( CondNum ).VolFlowRate > Numbers( 6 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 5 ) + " > " + cNumericFieldNames( 6 ) + " ." );
							ShowContinueError( "Revise flow rates." );
							ErrorsFound = true;
						} //Error check on max flow rate
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 6 ) + " must be greater than zero." );
						ErrorsFound = true;
					}

					// Get max/min allowed water temps
					Condenser( CondNum ).OutletTempMax = Numbers( 7 );
					Condenser( CondNum ).InletTempMin = Numbers( 8 );

					Condenser( CondNum ).EndUseSubcategory = "";
					if ( ! lAlphaBlanks( 6 ) ) Condenser( CondNum ).EndUseSubcategory = Alphas( 6 );

					Condenser( CondNum ).RefOpCharge = 0.0;
					Condenser( CondNum ).RefReceiverInventory = 0.0;
					Condenser( CondNum ).RefPipingInventory = 0.0;
					if ( ! lNumericBlanks( 9 ) ) Condenser( CondNum ).RefOpCharge = Numbers( 9 );
					if ( ! lNumericBlanks( 10 ) ) Condenser( CondNum ).RefReceiverInventory = Numbers( 10 );
					if ( ! lNumericBlanks( 11 ) ) Condenser( CondNum ).RefPipingInventory = Numbers( 11 );

				} // Read input for CONDENSER:REFRIGERATION:WaterCooled

				CheckEquipNameWaterCondenser.dimension( NumRefrigCondensers, true );
			} // NumSimulationCondWater > 0

			//cascade condensers assumed to provide zero subcooling
			if ( NumSimulationCascadeCondensers > 0 ) {
				CurrentModuleObject = "Refrigeration:Condenser:Cascade";
				for ( CondIndex = 1; CondIndex <= NumSimulationCascadeCondensers; ++CondIndex ) {
					CondNum = CondIndex + NumSimulationCondAir + NumSimulationCondEvap + NumSimulationCondWater;
					GetObjectItem( CurrentModuleObject, CondIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Condenser, CondNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					} //IsNotOK on Verify Name
					Condenser( CondNum ).Name = Alphas( 1 );
					HeatReclaimRefrigCondenser( CondNum ).Name = Alphas( 1 );

					//set start of count for number of systems attached to this condenser
					Condenser( CondNum ).NumSysAttach = 0;
					if ( ! allocated( Condenser( CondNum ).SysNum ) ) Condenser( CondNum ).SysNum.allocate( NumRefrigSystems );

					//set CondenserType
					Condenser( CondNum ).CondenserType = RefrigCondenserTypeCascade;

					if ( ! lNumericBlanks( 1 ) ) {
						Condenser( CondNum ).RatedTCondense = Numbers( 1 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 1 ) + " must be input." );
						ErrorsFound = true;
					}

					if ( ! lNumericBlanks( 2 ) ) {
						if ( Numbers( 2 ) >= 0.0 ) {
							Condenser( CondNum ).RatedApproachT = Numbers( 2 );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 2 ) + " must be greater than or equal to zero." );
							ErrorsFound = true;
						}
					} else {
						Condenser( CondNum ).RatedApproachT = DefaultCascadeCondApproach;
					}

					if ( ( ! lNumericBlanks( 3 ) ) && ( Numbers( 3 ) > 0.0 ) ) {
						Condenser( CondNum ).RatedCapacity = Numbers( 3 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\" " + cNumericFieldNames( 3 ) + " must be in put and must be greater than or equal to zero." );
						ErrorsFound = true;
					}

					// Get condensing temperature type, either fixed by design or allowed to float to match other loads on supply system
					if ( ! lAlphaBlanks( 2 ) ) {
						if ( SameString( Alphas( 2 ), "Fixed" ) ) { //set Condenser Temperature Control Type
							Condenser( CondNum ).CascadeTempControl = CascadeTempSet;
						} else if ( SameString( Alphas( 2 ), "Float" ) ) {
							Condenser( CondNum ).CascadeTempControl = CascadeTempFloat;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", invalid  " + cAlphaFieldNames( 2 ) + " not recognized: " + Alphas( 2 ) );
							ShowContinueError( "Check input value choices." );
							ErrorsFound = true;
						} //string comparison to key choices
					} else { // default is fixed/cascadetempset
						Condenser( CondNum ).CascadeTempControl = CascadeTempSet;
					} // not blank

					Condenser( CondNum ).CascadeRatedEvapTemp = Condenser( CondNum ).RatedTCondense - Condenser( CondNum ).RatedApproachT;

					//future - add refrigerant inventory on system side accepting reject heat (as was done for secondary)
					Condenser( CondNum ).RefOpCharge = 0.0;
					Condenser( CondNum ).RefReceiverInventory = 0.0;
					Condenser( CondNum ).RefPipingInventory = 0.0;
					if ( ! lNumericBlanks( 4 ) ) Condenser( CondNum ).RefOpCharge = Numbers( 4 );
					if ( ! lNumericBlanks( 5 ) ) Condenser( CondNum ).RefReceiverInventory = Numbers( 5 );
					if ( ! lNumericBlanks( 6 ) ) Condenser( CondNum ).RefPipingInventory = Numbers( 6 );

				} // Read input for CONDENSER:REFRIGERATION:Cascade
			} // NumSimulationCascadeCondensers > 0

			//************ END CONDENSER INPUT   **************

			//**********  START GAS COOLER INPUT  **********

			if ( NumSimulationGasCooler > 0 ) {
				CurrentModuleObject = "Refrigeration:GasCooler:AirCooled";
				for ( GCNum = 1; GCNum <= NumSimulationGasCooler; ++GCNum ) {
					GetObjectItem( CurrentModuleObject, GCNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), GasCooler, GCNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + " = " + Alphas( 1 ) );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					} //IsNotOK on Verify Name
					GasCooler( GCNum ).Name = Alphas( 1 );

					GasCooler( GCNum ).CapCurvePtr = GetCurveIndex( Alphas( 2 ) ); // convert curve name to number
					if ( GasCooler( GCNum ).CapCurvePtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + " not found:" + Alphas( 2 ) );
						ErrorsFound = true;
					}

					//set start of count for number of systems attached to this gas cooler
					GasCooler( GCNum ).NumSysAttach = 0;
					if ( ! allocated( GasCooler( GCNum ).SysNum ) ) GasCooler( GCNum ).SysNum.allocate( NumTransRefrigSystems );

					GasCooler( GCNum ).RatedApproachT = 3.0; // rated CO2 gas cooler approach temperature
					if ( GasCooler( GCNum ).CapCurvePtr > 0 ) {
						GasCooler( GCNum ).RatedCapacity = CurveValue( GasCooler( GCNum ).CapCurvePtr, GasCooler( GCNum ).RatedApproachT );
					}
					// elevation capacity correction on air-cooled condensers, Carrier correlation more conservative than Trane
					GasCooler( GCNum ).RatedCapacity *= ( 1.0 - 7.17e-5 * Elevation );
					if ( GasCooler( GCNum ).RatedCapacity > 0.0 ) {
						GetCurveMinMaxValues( GasCooler( GCNum ).CapCurvePtr, DelTempMin, DelTempMax );
						Capmin = CurveValue( GasCooler( GCNum ).CapCurvePtr, DelTempMin ) * ( 1.0 - 7.17e-5 * Elevation );
						Capmax = CurveValue( GasCooler( GCNum ).CapCurvePtr, DelTempMax ) * ( 1.0 - 7.17e-5 * Elevation );
						GasCooler( GCNum ).TempSlope = ( DelTempMax - DelTempMin ) / ( ( Capmax - Capmin ) );
						GasCooler( GCNum ).MinCondLoad = Capmax - DelTempMax / GasCooler( GCNum ).TempSlope;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" Gas Cooler capacity curve must be input and must be greater than 0 Watts at 3C temperature difference." );
						ErrorsFound = true;
					}

					// Get fan control type
					if ( SameString( Alphas( 3 ), "Fixed" ) ) {
						GasCooler( GCNum ).FanSpeedControlType = FanConstantSpeed;
					} else if ( SameString( Alphas( 3 ), "FixedLinear" ) ) {
						GasCooler( GCNum ).FanSpeedControlType = FanConstantSpeedLinear;
					} else if ( SameString( Alphas( 3 ), "VariableSpeed" ) ) {
						GasCooler( GCNum ).FanSpeedControlType = FanVariableSpeed;
					} else if ( SameString( Alphas( 3 ), "TwoSpeed" ) ) {
						GasCooler( GCNum ).FanSpeedControlType = FanTwoSpeed;
					} else {
						GasCooler( GCNum ).FanSpeedControlType = FanConstantSpeed; //default
					} //Set fan control type

					// Gas cooler fan power
					GasCooler( GCNum ).RatedFanPower = 5000.0; // default value
					if ( ! lNumericBlanks( 1 ) ) GasCooler( GCNum ).RatedFanPower = Numbers( 1 );
					if ( Numbers( 1 ) < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 1 ) + " must be input greater than or equal to 0 Watts." );
						ErrorsFound = true;
					}

					// Gas cooler minimum fan air flow ratio
					GasCooler( GCNum ).FanMinAirFlowRatio = 0.2; //default value
					if ( ! lNumericBlanks( 2 ) ) GasCooler( GCNum ).FanMinAirFlowRatio = Numbers( 2 );
					if ( ( GasCooler( GCNum ).FanMinAirFlowRatio < 0.0 ) || ( GasCooler( GCNum ).FanMinAirFlowRatio > 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 2 ) + " must be a value between zero and one.  The default value (0.2) will be used." );
						GasCooler( GCNum ).FanMinAirFlowRatio = 0.2;
					}

					// Gas cooler transition temperature
					GasCooler( GCNum ).TransitionTemperature = 2.7e1; // default value
					if ( ! lNumericBlanks( 3 ) ) GasCooler( GCNum ).TransitionTemperature = Numbers( 3 );
					if ( GasCooler( GCNum ).TransitionTemperature < 2.5e1 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 3 ) + " is low (less than 25C).  Consider raising the transition temperature to operate for longer periods of time in the subcritical region." );
					}
					if ( GasCooler( GCNum ).TransitionTemperature > 30.978 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 3 ) + " is greater than the critical temperature of carbon dioxide.  The default value (27C) will be used." );
						GasCooler( GCNum ).TransitionTemperature = 2.7e1;
					}

					// Gas cooler approach temperature for transcritical operation
					GasCooler( GCNum ).GasCoolerApproachT = 3.0; // default value
					if ( ! lNumericBlanks( 4 ) ) GasCooler( GCNum ).GasCoolerApproachT = Numbers( 4 );
					if ( GasCooler( GCNum ).GasCoolerApproachT < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 4 ) + " must be greater than 0C." );
						ErrorsFound = true;
					}

					// Gas cooler temperature difference for subcritical operation
					GasCooler( GCNum ).SubcriticalTempDiff = 1.0e1; // default value
					if ( ! lNumericBlanks( 5 ) ) GasCooler( GCNum ).SubcriticalTempDiff = Numbers( 5 );
					if ( GasCooler( GCNum ).SubcriticalTempDiff < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 5 ) + " must be greater than 0C." );
						ErrorsFound = true;
					}

					// Gas cooler minimum condensing temperature for subcritical operation
					GasCooler( GCNum ).MinCondTemp = 1.0e1; // default value
					if ( ! lNumericBlanks( 6 ) ) GasCooler( GCNum ).MinCondTemp = Numbers( 6 );
					if ( GasCooler( GCNum ).MinCondTemp > 30.9 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\" " + cNumericFieldNames( 6 ) + " must be less than the critical temperature of carbon dioxide (31C)." );
						ErrorsFound = true;
					}

					// Check GasCooler air inlet node connection
					GasCooler( GCNum ).GasCoolerRejectHeatToZone = false;
					if ( lAlphaBlanks( 4 ) ) {
						GasCooler( GCNum ).InletAirNodeNum = 0;
					} else { //see if it's an outside air node name or an indoor zone name,
						//have to check inside first because outside check automatically generates an error message
						GasCooler( GCNum ).InletAirZoneNum = FindItemInList( Alphas( 4 ), Zone );
						//need to clearly id node number for air inlet conditions and zone number for casecredit assignment
						if ( GasCooler( GCNum ).InletAirZoneNum != 0 ) {
							//set condenser flag (later used to set system flag) and zone flag
							GasCooler( GCNum ).InletAirNodeNum = GetSystemNodeNumberForZone( Alphas( 4 ) );
							GasCooler( GCNum ).GasCoolerRejectHeatToZone = true;
							RefrigPresentInZone( GasCooler( GCNum ).InletAirZoneNum ) = true;
						} else { // not in a conditioned zone, so see if it's outside
							GasCooler( GCNum ).InletAirNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsParent );
							if ( ! CheckOutAirNodeNumber( GasCooler( GCNum ).InletAirNodeNum ) ) {
								// not outside and not a zone
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + GasCooler( GCNum ).Name + "\", " + cAlphaFieldNames( 4 ) + " not found: " + Alphas( 4 ) );
								ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node or as a Zone." );
								ErrorsFound = true;
							} //checkoutairnodenumber
						} //InletAirZoneNum \=0
					} // Gas cooler air inlet node connection

					GasCooler( GCNum ).EndUseSubcategory = "";
					if ( ! lAlphaBlanks( 5 ) ) GasCooler( GCNum ).EndUseSubcategory = Alphas( 5 );

					GasCooler( GCNum ).RefOpCharge = 0.0;
					GasCooler( GCNum ).RefReceiverInventory = 0.0;
					GasCooler( GCNum ).RefPipingInventory = 0.0;
					if ( ! lNumericBlanks( 7 ) ) GasCooler( GCNum ).RefOpCharge = Numbers( 7 );
					if ( ! lNumericBlanks( 8 ) ) GasCooler( GCNum ).RefReceiverInventory = Numbers( 8 );
					if ( ! lNumericBlanks( 9 ) ) GasCooler( GCNum ).RefPipingInventory = Numbers( 9 );

				} // Read input for REFRIGERATION:GasCooler:AirCooled
			} // NumSimulationGasCooler > 0

			//**********  END GAS COOLER INPUT  **********

			//************ START SECONDARY LOOP INPUT (before system input) **************
			if ( NumSimulationSecondarySystems > 0 ) {
				CurrentModuleObject = "Refrigeration:SecondarySystem";
				for ( SecondaryNum = 1; SecondaryNum <= NumSimulationSecondarySystems; ++SecondaryNum ) {
					GetObjectItem( CurrentModuleObject, SecondaryNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Secondary, SecondaryNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					}
					Secondary( SecondaryNum ).Name = Alphas( 1 );

					//   Find the loads on the secondary loop: can be input in form of case or walkin or CaseAndWalkInList names
					NominalTotalCaseCap = 0.0;
					NumCases = 0;
					NominalTotalCoilCap = 0.0;
					NumCoils = 0;
					NumWalkIns = 0;
					NominalTotalWalkInCap = 0.0;
					Secondary( SecondaryNum ).RefInventory = 0.0;

					//   Read display case and walkin assignments for this secondary
					AlphaNum = 2;
					if ( lAlphaBlanks( AlphaNum ) ) {
						//No cases or walkins specified, ie, secondary has no load
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", has no loads, must have at least one of: " + cAlphaFieldNames( AlphaNum ) );
						ErrorsFound = true;
					} else { // (.NOT. lAlphaBlanks(AlphaNum))

						// Entry for Alphas(AlphaNum) can be either a Case, WalkIn Coil, or CaseAndWalkInList name
						CaseAndWalkInListNum = 0;
						CaseNum = 0;
						WalkInNum = 0;
						CoilNum = 0;
						if ( NumSimulationCaseAndWalkInLists > 0 ) CaseAndWalkInListNum = FindItemInList( Alphas( AlphaNum ), CaseAndWalkInList );
						if ( NumSimulationCases > 0 ) CaseNum = FindItemInList( Alphas( AlphaNum ), RefrigCase );
						if ( NumSimulationWalkIns > 0 ) WalkInNum = FindItemInList( Alphas( AlphaNum ), WalkIn );
						if ( NumSimulationRefrigAirChillers > 0 ) CoilNum = FindItemInList( Alphas( AlphaNum ), WarehouseCoil );
						NumNameMatches = 0;
						if ( CaseAndWalkInListNum != 0 ) ++NumNameMatches;
						if ( CaseNum != 0 ) ++NumNameMatches;
						if ( WalkInNum != 0 ) ++NumNameMatches;
						if ( CoilNum != 0 ) ++NumNameMatches;

						if ( NumNameMatches != 1 ) { //name must uniquely point to a list or a single case or walkin or coil
							ErrorsFound = true;
							if ( NumNameMatches == 0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
							} else if ( NumNameMatches > 1 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", has a non-unique name that could be either a " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
							} //num matches = 0 or > 1
						} else if ( CaseAndWalkInListNum != 0 ) { //Name points to a CaseAndWalkInList
							NumCoils = CaseAndWalkInList( CaseAndWalkInListNum ).NumCoils;
							NumCases = CaseAndWalkInList( CaseAndWalkInListNum ).NumCases;
							NumWalkIns = CaseAndWalkInList( CaseAndWalkInListNum ).NumWalkIns;
							Secondary( SecondaryNum ).NumCases = NumCases;
							Secondary( SecondaryNum ).NumCoils = NumCoils;
							Secondary( SecondaryNum ).NumWalkIns = NumWalkIns;
							if ( ! allocated( Secondary( SecondaryNum ).CaseNum ) ) Secondary( SecondaryNum ).CaseNum.allocate( NumCases );
							Secondary( SecondaryNum ).CaseNum( {1,NumCases} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CaseItemNum( {1,NumCases} );
							if ( ! allocated( Secondary( SecondaryNum ).CoilNum ) ) Secondary( SecondaryNum ).CoilNum.allocate( NumCoils );
							Secondary( SecondaryNum ).CoilNum( {1,NumCoils} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CoilItemNum( {1,NumCoils} );
							if ( ! allocated( Secondary( SecondaryNum ).WalkInNum ) ) Secondary( SecondaryNum ).WalkInNum.allocate( NumWalkIns );
							Secondary( SecondaryNum ).WalkInNum( {1,NumWalkIns} ) = CaseAndWalkInList( CaseAndWalkInListNum ).WalkInItemNum( {1,NumWalkIns} );
						} else if ( CaseNum != 0 ) { //Name points to a case
							NumCases = 1;
							Secondary( SecondaryNum ).NumCases = 1;
							if ( ! allocated( Secondary( SecondaryNum ).CaseNum ) ) Secondary( SecondaryNum ).CaseNum.allocate( NumCases );
							Secondary( SecondaryNum ).CaseNum( NumCases ) = CaseNum;
						} else if ( CoilNum != 0 ) { //Name points to a coil
							NumCoils = 1;
							Secondary( SecondaryNum ).NumCoils = 1;
							if ( ! allocated( Secondary( SecondaryNum ).CoilNum ) ) Secondary( SecondaryNum ).CoilNum.allocate( NumCoils );
							Secondary( SecondaryNum ).CoilNum( NumCoils ) = CoilNum;
						} else if ( WalkInNum != 0 ) { //Name points to a walkin
							NumWalkIns = 1;
							Secondary( SecondaryNum ).NumWalkIns = 1;
							if ( ! allocated( Secondary( SecondaryNum ).WalkInNum ) ) Secondary( SecondaryNum ).WalkInNum.allocate( NumWalkIns );
							Secondary( SecondaryNum ).WalkInNum( NumWalkIns ) = WalkInNum;
						} //NumNameMatches /= 1
					} //blank input for loads on secondary

					if ( NumCases > 0 ) {
						// Find lowest design T loop fluid out of secondary chiller
						// Sum rated capacity of all cases on Secondary
						for ( CaseIndex = 1; CaseIndex <= NumCases; ++CaseIndex ) {
							//mark all cases on Secondary as used by this Secondary - checking for unused or non-unique cases
							CaseNum = Secondary( SecondaryNum ).CaseNum( CaseIndex );
							++RefrigCase( CaseNum ).NumSysAttach;
							NominalTotalCaseCap += RefrigCase( CaseNum ).DesignRatedCap * RefrigCase( CaseNum ).RatedRTF;
							Secondary( SecondaryNum ).RefInventory += RefrigCase( CaseNum ).DesignRefrigInventory;
							if ( CaseIndex == 1 ) { //look for lowest case design evap T for Secondary
								Secondary( SecondaryNum ).TMinNeeded = RefrigCase( CaseNum ).EvapTempDesign;
							} else {
								Secondary( SecondaryNum ).TMinNeeded = min( RefrigCase( CaseNum ).EvapTempDesign, Secondary( SecondaryNum ).TMinNeeded );
							}
						} //CaseIndex=1,NumCases
					} //Numcases > 0

					if ( NumCoils > 0 ) {
						// Find lowest design T loop fluid out of secondary chiller
						// Sum rated capacity of all Coils on Secondary
						for ( CoilIndex = 1; CoilIndex <= NumCoils; ++CoilIndex ) {
							//mark all Coils on Secondary as used by this Secondary - checking for unused or non-unique Coils
							CoilNum = Secondary( SecondaryNum ).CoilNum( CoilIndex );
							++WarehouseCoil( CoilNum ).NumSysAttach;
							NominalTotalCoilCap += WarehouseCoil( CoilNum ).RatedSensibleCap;
							Secondary( SecondaryNum ).RefInventory += WarehouseCoil( CoilNum ).DesignRefrigInventory;
							if ( ( CoilIndex == 1 ) && ( NumCases == 0 ) ) { //look for lowest Coil design evap T for Secondary
								Secondary( SecondaryNum ).TMinNeeded = WarehouseCoil( CoilNum ).TEvapDesign;
							} else {
								Secondary( SecondaryNum ).TMinNeeded = min( WarehouseCoil( CoilNum ).TEvapDesign, Secondary( SecondaryNum ).TMinNeeded );
							}
						} //CoilIndex=1,NumCoils
					} //NumCoils > 0

					if ( NumWalkIns > 0 ) {
						// Find lowest design T loop fluid out of secondary chiller
						// Sum rated capacity of all WalkIns on Secondary
						for ( WalkInIndex = 1; WalkInIndex <= NumWalkIns; ++WalkInIndex ) {
							//mark all WalkIns on Secondary as used by this Secondary - checking for unused or non-unique WalkIns
							WalkInID = Secondary( SecondaryNum ).WalkInNum( WalkInIndex );
							++WalkIn( WalkInID ).NumSysAttach;
							NominalTotalWalkInCap += WalkIn( WalkInID ).DesignRatedCap;
							Secondary( SecondaryNum ).RefInventory += WalkIn( WalkInID ).DesignRefrigInventory;
							if ( ( WalkInIndex == 1 ) && ( NumCases == 0 ) && ( NumCoils == 0 ) ) { //look for lowest load design evap T for Secondary
								Secondary( SecondaryNum ).TMinNeeded = WalkIn( WalkInID ).TEvapDesign;
							} else {
								Secondary( SecondaryNum ).TMinNeeded = min( Secondary( SecondaryNum ).TMinNeeded, WalkIn( WalkInID ).TEvapDesign );
							}
						} //WalkInIndex=1,NumWalkIns
					} // Numwalkins > 0

					// Get circulating fluid type
					AlphaNum = 3;
					if ( ! lAlphaBlanks( AlphaNum ) ) {
						if ( SameString( Alphas( AlphaNum ), "FluidAlwaysLiquid" ) ) {
							Secondary( SecondaryNum ).FluidType = SecFluidTypeAlwaysLiquid;
						} else if ( SameString( Alphas( AlphaNum ), "FluidPhaseChange" ) ) {
							Secondary( SecondaryNum ).FluidType = SecFluidTypePhaseChange;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\"  " + cAlphaFieldNames( AlphaNum ) + " not recognized = " + Alphas( AlphaNum ) );
							ShowContinueError( "Input value choices should be FluidAlwaysLiquid or FluidPhaseChange." );
							ErrorsFound = true;
						} //Set FluidType
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " must be specified." );
						ErrorsFound = true;
					} // blank on cir fluid type

					AlphaNum = 4;
					Secondary( SecondaryNum ).FluidName = Alphas( AlphaNum );
					// Error messages for refrigerants and glycols already found in fluidproperties

					// Note remainder of inputs for secondary don't follow IDD input order because of different interpretations
					//   and intermediate calculations used to assign default values for brine type vs. liquid overfeed/phase change loops

					if ( ! lNumericBlanks( 3 ) ) {
						Secondary( SecondaryNum ).TEvapDesign = Numbers( 3 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" " + cNumericFieldNames( 3 ) + " must be specified." );
						ErrorsFound = true;
					} // blank on N3

					if ( ! lNumericBlanks( 4 ) ) {
						Secondary( SecondaryNum ).TApproachDifRated = Numbers( 4 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" " + cNumericFieldNames( 4 ) + " must be specified." );
						ErrorsFound = true;
					} // blank on N4

					//^^^^^^^Now look at input and once-only calculations required only for liquid/brine secondary loops^^^^^^^^^^^^^^^^^^^^^^
					//   Ensure that required input data is not missing prior to performing the following once-only calculations
					if ( ErrorsFound ) {
						ShowFatalError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", Program terminated due to previous condition(s)." );
					} // ErrorsFound

					if ( Secondary( SecondaryNum ).FluidType == SecFluidTypeAlwaysLiquid ) {
						if ( ! lNumericBlanks( 5 ) ) {
							Secondary( SecondaryNum ).TRangeDifRated = Numbers( 5 );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", " + cNumericFieldNames( 5 ) + " must be specified." );
							ShowContinueError( "...when " + cAlphaFieldNames( 3 ) + "=\"FluidAlwaysLiquid\"." );
							ErrorsFound = true;
						} // blank on N5

						// Get fluid properties at rated conditions, will be used to calculate ht exchgr effectiveness
						TBrineOutRated = Secondary( SecondaryNum ).TEvapDesign + Secondary( SecondaryNum ).TApproachDifRated;
						TBrineInRated = TBrineOutRated + Secondary( SecondaryNum ).TRangeDifRated;
						TBrineAverage = ( TBrineOutRated + TBrineInRated ) / 2.0;
						Secondary( SecondaryNum ).TBrineAverage = TBrineAverage;
						DensityBrineRated = GetDensityGlycol( Secondary( SecondaryNum ).FluidName, TBrineAverage, Secondary( SecondaryNum ).FluidID, TrackMessage );
						Secondary( SecondaryNum ).DensityBrineRated = DensityBrineRated;
						CpBrineRated = GetSpecificHeatGlycol( Secondary( SecondaryNum ).FluidName, TBrineAverage, Secondary( SecondaryNum ).FluidID, TrackMessage );
						Secondary( SecondaryNum ).CpBrineRated = CpBrineRated;

						//Users can input either design brine flow (m3/s), or capacity in W, or both.  Now have
						//  temperatures needed to calculate either the loop cooling capacity or fluid flow rate, if one was not input
						//  Don't need to save as a flow vol as a permanent var because calc whichever is missing here
						if ( ( ! lNumericBlanks( 1 ) ) && ( ! lNumericBlanks( 2 ) ) ) {
							//Both values input, check for approximate agreement
							Secondary( SecondaryNum ).CoolingLoadRated = Numbers( 1 );
							SecondaryFlowVolRated = Numbers( 2 );
							FlowMassRated = SecondaryFlowVolRated * DensityBrineRated;
							NominalSecondaryCapacity = FlowMassRated * CpBrineRated * Secondary( SecondaryNum ).TRangeDifRated;
							TestDelta = ( NominalSecondaryCapacity - Secondary( SecondaryNum ).CoolingLoadRated ) / NominalSecondaryCapacity;
							if ( std::abs( TestDelta ) > 0.2 ) {
								ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + " You may wish to check the system definition. Based upon the design flow rate and range temperature difference,  The nominal secondary loop heat exchanger capacity is, " + RoundSigDigits( NominalSecondaryCapacity, 0 ) + " but the specified design capacity is,  " + RoundSigDigits( Secondary( SecondaryNum ).CoolingLoadRated, 0 ) );
							}
						} else if ( ! lNumericBlanks( 1 ) ) {
							Secondary( SecondaryNum ).CoolingLoadRated = Numbers( 1 );
							//Calc flow vol rated
							FlowMassRated = Secondary( SecondaryNum ).CoolingLoadRated / ( CpBrineRated * Secondary( SecondaryNum ).TRangeDifRated );
							SecondaryFlowVolRated = FlowMassRated / DensityBrineRated;
						} else if ( ! lNumericBlanks( 2 ) ) {
							SecondaryFlowVolRated = Numbers( 2 );
							//Calc rated load
							FlowMassRated = SecondaryFlowVolRated * DensityBrineRated;
							Secondary( SecondaryNum ).CoolingLoadRated = FlowMassRated * CpBrineRated * Secondary( SecondaryNum ).TRangeDifRated;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", Either \"" + cNumericFieldNames( 1 ) + "\" OR \"" + cNumericFieldNames( 2 ) + "\" must be input." );
							ErrorsFound = true;
						} // Capacity Input via either or both options

						if ( ! ErrorsFound ) {
							//Calculate heat exchanger effectiveness based on rated flow and temperature differences
							Secondary( SecondaryNum ).HeatExchangeEta = Secondary( SecondaryNum ).CoolingLoadRated / ( FlowMassRated * CpBrineRated * ( TBrineInRated - Secondary( SecondaryNum ).TEvapDesign ) );
							Secondary( SecondaryNum ).TBrineInRated = TBrineInRated;
							if ( Secondary( SecondaryNum ).HeatExchangeEta > 0.99 ) {
								ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + " You may wish to check the system definition.  The heat exchanger effectiveness is, " + RoundSigDigits( Secondary( SecondaryNum ).HeatExchangeEta, 2 ) );
								Secondary( SecondaryNum ).HeatExchangeEta = 0.99;
							}
						} else {
							ShowContinueError( "...remainder of this object input skipped due to previous errors" );
							continue;
						}

						PumpTotRatedFlowVol = SecondaryFlowVolRated;
						if ( ! lNumericBlanks( 7 ) ) PumpTotRatedFlowVol = Numbers( 7 );

					} else { //FluidType = FluidTypePhaseChange     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
						if ( ! lNumericBlanks( 1 ) ) {
							Secondary( SecondaryNum ).CoolingLoadRated = Numbers( 1 );
						} else {
							Secondary( SecondaryNum ).CoolingLoadRated = NominalTotalCaseCap + NominalTotalWalkInCap;
							// first estimate, will later be adjusted to include pump power
						} //input capacity

						Secondary( SecondaryNum ).TCondense = Secondary( SecondaryNum ).TEvapDesign + Secondary( SecondaryNum ).TApproachDifRated;
						Secondary( SecondaryNum ).CircRate = DefaultCircRate;
						if ( ! lNumericBlanks( 10 ) ) Secondary( SecondaryNum ).CircRate = Numbers( 10 );

						DensityPhaseChange = GetSatDensityRefrig( Secondary( SecondaryNum ).FluidName, Secondary( SecondaryNum ).TCondense, 0.0, Secondary( SecondaryNum ).FluidID, TrackMessageAlt );
						DeltaHPhaseChange = GetSatEnthalpyRefrig( Secondary( SecondaryNum ).FluidName, Secondary( SecondaryNum ).TCondense, 1.0, Secondary( SecondaryNum ).FluidID, TrackMessageAlt ) - GetSatEnthalpyRefrig( Secondary( SecondaryNum ).FluidName, Secondary( SecondaryNum ).TCondense, 0.0, Secondary( SecondaryNum ).FluidID, TrackMessageAlt );

						//TotRatedFlowVol= capacity*circrate/deltahphasechange/density
						CalcTotFlowVol = Secondary( SecondaryNum ).CoolingLoadRated * Secondary( SecondaryNum ).CircRate / ( DensityPhaseChange * DeltaHPhaseChange );
						PumpTotRatedFlowVol = CalcTotFlowVol;
						if ( ! lNumericBlanks( 7 ) ) {
							PumpTotRatedFlowVol = Numbers( 7 );
							CalcCircRate = DensityPhaseChange * DeltaHPhaseChange * PumpTotRatedFlowVol / Secondary( SecondaryNum ).CoolingLoadRated;
							DiffCircRates = ( CalcCircRate - Secondary( SecondaryNum ).CircRate ) / Secondary( SecondaryNum ).CircRate;
							if ( std::abs( DiffCircRates ) > 0.3 ) {
								ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + ' ' + cNumericFieldNames( 7 ) + " Produces a circulating rate of " + RoundSigDigits( CalcCircRate, 2 ) + " ;  A circulating rate of " + RoundSigDigits( Secondary( SecondaryNum ).CircRate, 2 ) + " would need a " + cNumericFieldNames( 7 ) + " of " + RoundSigDigits( CalcTotFlowVol, 2 ) + " m3/s" );
							} // warning check on pump flow rate vs circ rate input
						} //blank pump flow rate
						SecondaryFlowVolRated = PumpTotRatedFlowVol;

					} //fluid type AlwaysLiquid or PhaseChange ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

					//Read number of pumps (or pump stages) in secondary loop
					NumPumps = 1; //default value
					if ( ( ! lNumericBlanks( 6 ) ) && ( Numbers( 6 ) >= 1 ) ) NumPumps = Numbers( 6 );
					Secondary( SecondaryNum ).NumPumps = NumPumps;
					// Get pump power (users can input either power in W or head in Pa or both)
					// Assume pump impeller efficiency is 0.78 (consistent with E+ Pump auto sizing assumption)
					// Assume pump motor efficiency is 0.85 (Goulds Pumps motor data sheet)
					// It is important that tot rated head must be for specific fluid
					if ( ( ! lNumericBlanks( 8 ) ) && ( ! lNumericBlanks( 9 ) ) ) {
						Secondary( SecondaryNum ).PumpTotRatedPower = Numbers( 8 );
						PumpTotRatedHead = Numbers( 9 );
						ErrSecondPumpPower = ( Secondary( SecondaryNum ).PumpTotRatedPower - PumpTotRatedFlowVol * PumpTotRatedHead / ( PumpImpellerEfficiency * PumpMotorEfficiency ) ) / Secondary( SecondaryNum ).PumpTotRatedPower;
						if ( std::abs( ErrSecondPumpPower ) > 0.35 ) ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + " Input value for " + cNumericFieldNames( 9 ) + " not consistent with input value for " + cNumericFieldNames( 8 ) + ". " + cNumericFieldNames( 8 ) + " will be used" ); //generous diff allowed because comparing to my assumed impeller and motor effs
					} else if ( ! lNumericBlanks( 8 ) ) {
						Secondary( SecondaryNum ).PumpTotRatedPower = Numbers( 8 );
					} else if ( ! lNumericBlanks( 9 ) ) {
						PumpTotRatedHead = Numbers( 9 );
						Secondary( SecondaryNum ).PumpTotRatedPower = PumpTotRatedFlowVol * PumpTotRatedHead / ( PumpImpellerEfficiency * PumpMotorEfficiency );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", Either \"" + cNumericFieldNames( 8 ) + "\" OR \"" + cNumericFieldNames( 9 ) + "\" must be input." );
						ErrorsFound = true;
					} //Either or pump power Input variations (head or power)

					// Get pump drive type
					AlphaNum = 5;
					Secondary( SecondaryNum ).PumpControlType = SecPumpControlConstant; //default
					if ( ! lAlphaBlanks( AlphaNum ) ) {
						if ( SameString( Alphas( AlphaNum ), "Constant" ) ) {
							Secondary( SecondaryNum ).PumpControlType = SecPumpControlConstant;
						} else if ( SameString( Alphas( AlphaNum ), "Variable" ) ) {
							Secondary( SecondaryNum ).PumpControlType = SecPumpControlVariable;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\"  " + cAlphaFieldNames( AlphaNum ) + " not recognized = " + Alphas( AlphaNum ) );
							ShowContinueError( "Check input value choices." );
							ErrorsFound = true;
						} //Set PumpControlType
					} // blank on pump drive control type

					//  Print warning if Pump Control = Constant and Variable Speed Curve is specified.
					if ( ( Secondary( SecondaryNum ).PumpControlType == SecPumpControlConstant ) && ( ! lAlphaBlanks( AlphaNum + 1 ) ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", A " + cAlphaFieldNames( AlphaNum + 1 ) + " is specified even though " + cAlphaFieldNames( AlphaNum ) + " is \"CONSTANT\"." );
						ShowContinueError( "The secondary loop pump(s) will be modeled as constant speed and the " + cAlphaFieldNames( AlphaNum + 1 ) + " will be ignored." );
					}

					if ( Secondary( SecondaryNum ).PumpControlType == SecPumpControlConstant ) {
						//Set incremental flow and power amounts for pump dispatch
						Secondary( SecondaryNum ).PumpIncrementFlowVol = PumpTotRatedFlowVol / NumPumps;
						Secondary( SecondaryNum ).PumpIncrementPower = Secondary( SecondaryNum ).PumpTotRatedPower / NumPumps;
					} else { //Variable speed drive need to read in power curve
						AlphaNum = 6;
						Secondary( SecondaryNum ).VarSpeedCurvePtr = GetCurveIndex( Alphas( AlphaNum ) ); // convert curve name to number
						if ( Secondary( SecondaryNum ).VarSpeedCurvePtr == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found:" + Alphas( AlphaNum ) );
							ErrorsFound = true;
						}
						if ( ! SameString( GetCurveType( Secondary( SecondaryNum ).VarSpeedCurvePtr ), "CUBIC" ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " object must be of type cubic." );
							ErrorsFound = true;
						}
					} // input power conditions/levels for constant or variable speed pump drives

					//Default non-hermetic motor eff at 85% and all shaft power goes to heat in fluid
					// In a semi-hermetic motor, assume all power to motor goes to heat in fluid
					Secondary( SecondaryNum ).PumpPowerToHeat = PumpMotorEfficiency;
					NumNum = 11;
					if ( ! lNumericBlanks( NumNum ) ) {
						if ( ( 0.5 <= Numbers( NumNum ) ) && ( 1.0 >= Numbers( NumNum ) ) ) {
							Secondary( SecondaryNum ).PumpPowerToHeat = Numbers( NumNum );
						} else {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" " + cNumericFieldNames( NumNum ) + " must be between 0.5 and 1.0. Default value of : " + RoundSigDigits( PumpMotorEfficiency, 3 ) + " will be used" );
						} //range of pump moter heat to fluid
					} //blank input for pumppowertoheat

					//Distribution piping heat gain - optional
					//  Input UA and Zone containing the bulk of the secondary coolant distribution piping
					//  This Zone ID will be used to determine the temperature used for distribution piping heat gain.
					//  Zone Id is only required if Sum UA Distribution Piping >0.0
					//  Get the Zone node number from the zone name entered by the user
					Secondary( SecondaryNum ).SumUADistPiping = 0.0;
					AlphaNum = 7;
					NumNum = 12;
					if ( ! lNumericBlanks( NumNum ) && ! lAlphaBlanks( AlphaNum ) ) {
						Secondary( SecondaryNum ).SumUADistPiping = Numbers( NumNum );
						Secondary( SecondaryNum ).DistPipeZoneNum = FindItemInList( Alphas( AlphaNum ), Zone );
						Secondary( SecondaryNum ).DistPipeZoneNodeNum = GetSystemNodeNumberForZone( Alphas( AlphaNum ) );

						if ( Secondary( SecondaryNum ).DistPipeZoneNum == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not valid: " + Alphas( AlphaNum ) );
							ErrorsFound = true;
						} else {
							RefrigPresentInZone( Secondary( SecondaryNum ).DistPipeZoneNum ) = true;
						}

						if ( Secondary( SecondaryNum ).DistPipeZoneNodeNum == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" System Node Number not found for " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) + " even though " + cNumericFieldNames( NumNum ) + " is greater than zero. Distribution piping heat gain cannot be calculated unless a controlled Zone (appear in a ZoneHVAC:EquipmentConnections object.) is defined to determine the environmental temperature surrounding the piping." );
							ErrorsFound = true;
						}
					} else if ( ! lNumericBlanks( NumNum ) && lAlphaBlanks( AlphaNum ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " not found even though " + cNumericFieldNames( NumNum ) + " is greater than zero. Distribution piping heat gain will not be calculated unless a Zone is defined to deterimine the environmental temperature surrounding the piping." );
					} else if ( lNumericBlanks( NumNum ) && ! lAlphaBlanks( AlphaNum ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " will not be used and distribution piping heat gain will not be calculated because " + cNumericFieldNames( NumNum ) + " was blank." );
					} //distribution piping

					//Separator/receiver heat gain - optional
					//  Input UA and Zone containing the Separator/receiver
					//  This Zone ID will be used to determine the temperature used for Separator/receiver heat gain.
					//  Zone Id is only required if Sum UA Separator/receiver >0.0
					//  Get the Zone node number from the zone name entered by the user
					Secondary( SecondaryNum ).SumUAReceiver = 0.0;
					AlphaNum = 8;
					NumNum = 13;
					if ( ! lNumericBlanks( NumNum ) && ! lAlphaBlanks( AlphaNum ) ) {
						Secondary( SecondaryNum ).SumUAReceiver = Numbers( NumNum );
						Secondary( SecondaryNum ).ReceiverZoneNum = FindItemInList( Alphas( AlphaNum ), Zone );
						Secondary( SecondaryNum ).ReceiverZoneNodeNum = GetSystemNodeNumberForZone( Alphas( AlphaNum ) );

						if ( Secondary( SecondaryNum ).ReceiverZoneNum == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not valid: " + Alphas( AlphaNum ) );
							ErrorsFound = true;
						} else {
							RefrigPresentInZone( Secondary( SecondaryNum ).ReceiverZoneNum ) = true;
						}
						if ( Secondary( SecondaryNum ).ReceiverZoneNodeNum == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" System Node Number not found for " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) + " even though " + cNumericFieldNames( NumNum ) + " is greater than zero. Receiver heat gain cannot be calculated unless a controlled Zone (appear in a ZoneHVAC:EquipmentConnections object.) is defined to determine the environmental temperature surrounding the Receiver." );
							ErrorsFound = true;
						}
					} else if ( ! lNumericBlanks( NumNum ) && lAlphaBlanks( AlphaNum ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " not found even though " + cNumericFieldNames( NumNum ) + " is greater than zero. Receiver heat gain will not be calculated unless a Zone is defined to deterimine the environmental temperature surrounding the Receiver." );
					} else if ( lNumericBlanks( NumNum ) && ! lAlphaBlanks( AlphaNum ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " will not be used and Receiver heat gain will not be calculated because " + cNumericFieldNames( NumNum ) + " was blank." );
					} //Receiver

					NumNum = 14;
					Secondary( SecondaryNum ).ChillerRefInventory = 0.0;
					if ( ! lNumericBlanks( NumNum ) ) Secondary( SecondaryNum ).ChillerRefInventory = Numbers( NumNum );
					if ( Secondary( SecondaryNum ).ChillerRefInventory < 0.0 ) {
						Secondary( SecondaryNum ).ChillerRefInventory = 0.0;
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\", The value specified for " + cNumericFieldNames( NumNum ) + " is less than zero. The default value of zero will be used." );
					}

					AlphaNum = 9;
					if ( ! lAlphaBlanks( AlphaNum ) ) Secondary( SecondaryNum ).EndUseSubcategory = Alphas( AlphaNum );

					//Error checks on secondary loop:
					// Note, rated capacities can be far off from operating capacities, but rough checks here
					//       (don't include dist piping or receiver heat gains).
					// Load limit logic here (maxvolflow and maxload used in calcs later)
					Secondary( SecondaryNum ).MaxVolFlow = min( SecondaryFlowVolRated, PumpTotRatedFlowVol );
					NominalSecondaryRefLoad = NominalTotalCaseCap + NominalTotalWalkInCap + Secondary( SecondaryNum ).PumpTotRatedPower;

					if ( Secondary( SecondaryNum ).FluidType == SecFluidTypeAlwaysLiquid ) {
						if ( TBrineOutRated > ( Secondary( SecondaryNum ).TMinNeeded + 0.5 ) ) {
							ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + " The design brine temperature to the refrigeration loads: " + RoundSigDigits( TBrineOutRated, 1 ) + " ;" );
							ShowContinueError( " is greater than the design inlet temperature for at least one of the cases or walkins: " + RoundSigDigits( Secondary( SecondaryNum ).TMinNeeded, 1 ) );
							ShowContinueError( " Compare your Approach and Evaporating Temperature to the design inlet temperatures needed for the loads." );
							//ErrorsFound = .TRUE.
						} //Tbrine out warning
						CapacityAtMaxVolFlow = Secondary( SecondaryNum ).MaxVolFlow * Secondary( SecondaryNum ).HeatExchangeEta * ( CpBrineRated * DensityBrineRated ) * ( TBrineInRated - Secondary( SecondaryNum ).TEvapDesign );
						Secondary( SecondaryNum ).MaxLoad = min( Secondary( SecondaryNum ).CoolingLoadRated, CapacityAtMaxVolFlow );
						DeltaCap1 = std::abs( ( Secondary( SecondaryNum ).CoolingLoadRated - CapacityAtMaxVolFlow ) / Secondary( SecondaryNum ).CoolingLoadRated );
						if ( DeltaCap1 > ( 0.3 ) ) { //diff between chiller rating and capacity at max flow > 30%
							ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" You may wish to check the system sizing.  The nominal secondary loop heat exchanger capacity is " + RoundSigDigits( Secondary( SecondaryNum ).CoolingLoadRated, 0 ) + " But the capacity based upon the maximum flow rate is " + RoundSigDigits( CapacityAtMaxVolFlow, 0 ) );
						} // DeltaCap1 > .3
					} else { // Fluid type phase change                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
						if ( lNumericBlanks( 1 ) ) { // Chiller/evaporator capacity was not specified
							if ( lNumericBlanks( 7 ) ) { // Pump power was not input, calc based on flow and head
								//need to refine because capacity calculated, but needs to include pump power (which was prev
								//   estimated based upon capacity which had been estimated as sum of case and walk-in capacities)
								PumpTotRatedFlowVol = NominalSecondaryRefLoad * Secondary( SecondaryNum ).CircRate / ( DensityPhaseChange * DeltaHPhaseChange );
								Secondary( SecondaryNum ).PumpTotRatedPower = PumpTotRatedFlowVol * PumpTotRatedHead / ( PumpImpellerEfficiency * PumpMotorEfficiency );
								//need to recalc nominal load with new pump power value
								NominalSecondaryRefLoad = NominalTotalCaseCap + NominalTotalWalkInCap + Secondary( SecondaryNum ).PumpTotRatedPower;
								if ( Secondary( SecondaryNum ).PumpControlType == SecPumpControlConstant ) {
									//Set incremental flow and power amounts for pump dispatch
									Secondary( SecondaryNum ).PumpIncrementFlowVol = PumpTotRatedFlowVol / NumPumps;
									Secondary( SecondaryNum ).PumpIncrementPower = Secondary( SecondaryNum ).PumpTotRatedPower / NumPumps;
								} // constant speed pump
							} // Pump power was not specified
							Secondary( SecondaryNum ).CoolingLoadRated = NominalSecondaryRefLoad;
						} // Chiller/evap capacity was not specified
						Secondary( SecondaryNum ).MaxLoad = Secondary( SecondaryNum ).CoolingLoadRated;
					} // SecFluidType

					DeltaCap2 = std::abs( ( Secondary( SecondaryNum ).CoolingLoadRated - NominalSecondaryRefLoad ) / Secondary( SecondaryNum ).CoolingLoadRated );
					if ( DeltaCap2 > ( 0.3 ) ) { //diff between chiller rating and sum of nominal loads > 30%
						ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" You may wish to check the system sizing. Total nominal refrigerating load is " + RoundSigDigits( NominalSecondaryRefLoad, 0 ) + " (Including cases, walk-ins, and pump heat).  The nominal secondary loop heat exchanger capacity is " + RoundSigDigits( Secondary( SecondaryNum ).CoolingLoadRated, 0 ) );
					}
					//compare rated xt xchanger brine flow to the total rated pump flow
					if ( SecondaryFlowVolRated > ( 1.1 * PumpTotRatedFlowVol ) ) {
						ShowWarningError( CurrentModuleObject + "=\"" + Secondary( SecondaryNum ).Name + "\" You may wish to check the pump sizing. Total nominal brine flow is " + RoundSigDigits( SecondaryFlowVolRated, 0 ) + " m3/s, but the total nominal pump flow rate is:  " + RoundSigDigits( PumpTotRatedFlowVol, 0 ) + " m3/s. " );
					}

				} // Secondary Loops
			} //(  IF (NumSimulationSecondarySystems > 0)

			//************ END SECONDARY SYSTEM INPUT  **************

			//************ START Compressor INPUT  **************

			CurrentModuleObject = "Refrigeration:Compressor";
			for ( CompNum = 1; CompNum <= NumSimulationCompressors; ++CompNum ) {
				GetObjectItem( CurrentModuleObject, CompNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), Compressor, CompNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				Compressor( CompNum ).Name = Alphas( 1 );

				Compressor( CompNum ).ElecPowerCurvePtr = GetCurveIndex( Alphas( 2 ) ); // convert curve name to number
				if ( ( ! lAlphaBlanks( 2 ) ) && Compressor( CompNum ).ElecPowerCurvePtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Compressor( CompNum ).Name + "\", invalid  " + cAlphaFieldNames( 2 ) + " not found = " + Alphas( 2 ) );
					ErrorsFound = true;
				}

				Compressor( CompNum ).CapacityCurvePtr = GetCurveIndex( Alphas( 3 ) ); // convert curve name to number
				if ( ( ! lAlphaBlanks( 3 ) ) && Compressor( CompNum ).CapacityCurvePtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Compressor( CompNum ).Name + "\", invalid  " + cAlphaFieldNames( 3 ) + " not found = " + Alphas( 3 ) );
					ErrorsFound = true;
				}

				// Get superheat rating type (Either N1 or N2 Must be input)
				if ( ( ( ! lNumericBlanks( 1 ) ) && ( ! lNumericBlanks( 2 ) ) ) || ( lNumericBlanks( 1 ) && lNumericBlanks( 2 ) ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Compressor( CompNum ).Name + "\"One, and Only One of " + cNumericFieldNames( 1 ) + " or " + cNumericFieldNames( 2 ) );
					ShowContinueError( "Must Be Entered. Check input value choices." );
					ErrorsFound = true;
				} else if ( ! lNumericBlanks( 1 ) ) {
					Compressor( CompNum ).SuperheatRatingType = RatedSuperheat;
					Compressor( CompNum ).RatedSuperheat = Numbers( 1 );
				} else if ( ! lNumericBlanks( 2 ) ) {
					Compressor( CompNum ).SuperheatRatingType = RatedReturnGasTemperature;
					Compressor( CompNum ).RatedSuperheat = Numbers( 2 );
				} //Set SuperheatRatingType

				// Get subcool rating type (Either N3 or N4 Must be input)
				if ( ( ( ! lNumericBlanks( 3 ) ) && ( ! lNumericBlanks( 4 ) ) ) || ( lNumericBlanks( 3 ) && lNumericBlanks( 4 ) ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Compressor( CompNum ).Name + "\" One, and Only One of " + cNumericFieldNames( 3 ) + " or " + cNumericFieldNames( 4 ) );
					ShowContinueError( "Must Be Entered. Check input value choices." );
					ErrorsFound = true;
				} else if ( ! lNumericBlanks( 3 ) ) {
					Compressor( CompNum ).SubcoolRatingType = RatedLiquidTemperature;
					Compressor( CompNum ).RatedSubcool = Numbers( 3 );
				} else if ( ! lNumericBlanks( 4 ) ) {
					Compressor( CompNum ).SubcoolRatingType = RatedSubcooling;
					Compressor( CompNum ).RatedSubcool = Numbers( 4 );
				} //Set SubcoolRatingType

				Compressor( CompNum ).EndUseSubcategory = "General";
				if ( ! lAlphaBlanks( 4 ) ) Compressor( CompNum ).EndUseSubcategory = Alphas( 4 );

				//  If the compressor is a transcritical CO compressor, get transcritical power and capacity curves
				if ( SameString( Alphas( 5 ), "Transcritical" ) ) { // Mode of Operation = Transcritical
					Compressor( CompNum ).TransFlag = true;
					Compressor( CompNum ).TransElecPowerCurvePtr = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number
					if ( lAlphaBlanks( 6 ) && Compressor( CompNum ).TransElecPowerCurvePtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + '=' + Compressor( CompNum ).Name + ": " + cAlphaFieldNames( 6 ) + " not found." );
						ErrorsFound = true;
					}
					Compressor( CompNum ).TransCapacityCurvePtr = GetCurveIndex( Alphas( 7 ) ); // convert curve name to number
					if ( lAlphaBlanks( 7 ) && Compressor( CompNum ).TransCapacityCurvePtr == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + '=' + Compressor( CompNum ).Name + ": " + cAlphaFieldNames( 7 ) + " not found." );
						ErrorsFound = true;
					}
				} else if ( ( SameString( Alphas( 5 ), "Subcritical" ) ) || ( lAlphaBlanks( 5 ) ) ) { // Mode of Operation = Subcritical
					Compressor( CompNum ).TransFlag = false;
					if ( ( ! lAlphaBlanks( 6 ) ) || ( ! lAlphaBlanks( 7 ) ) ) { // Transcritical compressor curves specified for subcritical compressor
						ShowWarningError( RoutineName + CurrentModuleObject + '=' + Compressor( CompNum ).Name + " is specified to be a subcritical compressor, however transcritical compressor curve(s) are given." );
						ShowContinueError( "The compressor will be modeled as a subcritical compressor and the transcritical compressor curve(s) will be ignored." );
					}
				} else { // Invalid Mode of Operation
					ShowSevereError( RoutineName + CurrentModuleObject + ": " + cAlphaFieldNames( 5 ) + " for " + Compressor( CompNum ).Name + '=' + Alphas( 5 ) + " is invalid. Valid choices are \"Subcritical\" or \"Transcritical\"." );
					ErrorsFound = true;
				}

			} // RefrigCompressor

			//************ END Compressor INPUT         **************

			//************ START Subcooler INPUT        **************
			if ( NumSimulationSubcoolers > 0 ) {
				CurrentModuleObject = "Refrigeration:Subcooler";
				NumSimulationMechSubcoolers = 0;
				for ( SubcoolerNum = 1; SubcoolerNum <= NumSimulationSubcoolers; ++SubcoolerNum ) {
					GetObjectItem( CurrentModuleObject, SubcoolerNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Subcooler, SubcoolerNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					}
					Subcooler( SubcoolerNum ).Name = Alphas( 1 );

					// Get subcooler type
					Subcooler( SubcoolerNum ).SubcoolerType = LiquidSuction; //default subcooler type
					if ( SameString( Alphas( 2 ), "Mechanical" ) ) { //set subcooler type
						Subcooler( SubcoolerNum ).SubcoolerType = Mechanical;
						++NumSimulationMechSubcoolers;
					} else if ( SameString( Alphas( 2 ), "LiquidSuction" ) ) {
						Subcooler( SubcoolerNum ).SubcoolerType = LiquidSuction;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\", " + cAlphaFieldNames( 2 ) + " not recognized = " + Alphas( 2 ) );
						ShowContinueError( "Check input value choices." );
						ErrorsFound = true;
					} //Set Subcooler Type

					{ auto const SELECT_CASE_var( Subcooler( SubcoolerNum ).SubcoolerType );

					if ( SELECT_CASE_var == LiquidSuction ) {
						Subcooler( SubcoolerNum ).LiqSuctDesignDelT = 10.0; //default value
						if ( ! lNumericBlanks( 1 ) ) Subcooler( SubcoolerNum ).LiqSuctDesignDelT = Numbers( 1 );
						if ( Subcooler( SubcoolerNum ).LiqSuctDesignDelT < 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\" " + cNumericFieldNames( 1 ) + " cannot be less than zero." );
							ErrorsFound = true;
						}

						if ( ! lNumericBlanks( 2 ) ) {
							Subcooler( SubcoolerNum ).LiqSuctDesignTliqIn = Numbers( 2 );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\" " + cNumericFieldNames( 2 ) + " must be specified." );
							ErrorsFound = true;
						}

						if ( ! lNumericBlanks( 3 ) ) {
							Subcooler( SubcoolerNum ).LiqSuctDesignTvapIn = Numbers( 3 );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\" " + cNumericFieldNames( 3 ) + " must be specified." );
							ErrorsFound = true;
						}
						if ( Subcooler( SubcoolerNum ).LiqSuctDesignTvapIn > Subcooler( SubcoolerNum ).LiqSuctDesignTliqIn ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\" " + cNumericFieldNames( 3 ) + " cannot be greater than " + cNumericFieldNames( 2 ) + '.' );
							ErrorsFound = true;
						} //error check

					} else if ( SELECT_CASE_var == Mechanical ) {
						Subcooler( SubcoolerNum ).MechSourceSys = Alphas( 3 );
						//Error check on system name comes later after systems have been read

						if ( ! lNumericBlanks( 4 ) ) {
							Subcooler( SubcoolerNum ).MechControlTliqOut = Numbers( 4 );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\" " + cNumericFieldNames( 4 ) + " must be specified." );
							ErrorsFound = true;
						} //error check

					}}

				} // Subcooler Input
			} // If there are subcoolers

			// ********END SUBCOOLER INPUTS ************

			//**** Read TransferLoad Lists **********************************************************
			if ( NumSimulationTransferLoadLists > 0 ) {
				CurrentModuleObject = "Refrigeration:TransferLoadList";
				for ( ListNum = 1; ListNum <= NumSimulationTransferLoadLists; ++ListNum ) {
					GetObjectItem( CurrentModuleObject, ListNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), TransferLoadList, ListNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
						if ( IsBlank ) Alphas( 1 ) = "xxxxx";
						ErrorsFound = true;
					}

					TransferLoadList( ListNum ).Name = Alphas( 1 );

					// Transfer load list alphas include TransferLoadList name and one name for each Secondary or Cascade Condenser in list
					// below allocates larger than needed (each allocated to sum of both), but avoids two loops through input fields
					NumTotalLoadsOnList = NumAlphas - 1;
					if ( ! allocated( TransferLoadList( ListNum ).CascadeLoadItemNum ) ) TransferLoadList( ListNum ).CascadeLoadItemNum.allocate( NumTotalLoadsOnList );
					if ( ! allocated( TransferLoadList( ListNum ).SecondaryItemNum ) ) TransferLoadList( ListNum ).SecondaryItemNum.allocate( NumTotalLoadsOnList );

					NumSecondarysOnList = 0;
					NumCascadeLoadsOnList = 0;
					for ( NumLoad = 1; NumLoad <= NumTotalLoadsOnList; ++NumLoad ) {
						AlphaListNum = 1 + NumLoad;
						LoadCascadeNum = 0;
						LoadSecondaryNum = 0;
						if ( NumRefrigCondensers > 0 ) LoadCascadeNum = FindItemInList( Alphas( AlphaListNum ), Condenser );
						if ( NumSimulationSecondarySystems > 0 ) LoadSecondaryNum = FindItemInList( Alphas( AlphaListNum ), Secondary );
						if ( ( LoadCascadeNum == 0 ) && ( LoadSecondaryNum == 0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaFieldNames( AlphaListNum ) + "\" : has an invalid value of " + Alphas( AlphaListNum ) );
							ErrorsFound = true;
						} else if ( ( LoadCascadeNum != 0 ) && ( LoadSecondaryNum != 0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaFieldNames( AlphaListNum ) + "\" : has a non-unique name : " + Alphas( AlphaListNum ) );
							ErrorsFound = true;
						} else if ( LoadCascadeNum != 0 ) {
							if ( Condenser( LoadCascadeNum ).CondenserType != RefrigCondenserTypeCascade ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\" : has a condenser listed as a transfer load that is not a cascade condenser: " + Alphas( AlphaListNum ) );
								ErrorsFound = true;
							} else {
								++NumCascadeLoadsOnList;
								TransferLoadList( ListNum ).CascadeLoadItemNum( NumCascadeLoadsOnList ) = LoadCascadeNum;
							} // /= condenser cascade type
						} else if ( LoadSecondaryNum != 0 ) {
							++NumSecondarysOnList;
							TransferLoadList( ListNum ).SecondaryItemNum( NumSecondarysOnList ) = LoadSecondaryNum;
						}
						TransferLoadList( ListNum ).NumSecondarys = NumSecondarysOnList;
						TransferLoadList( ListNum ).NumCascadeLoads = NumCascadeLoadsOnList;
					} //Num Total Loads on List
				} //ListNum=1,NumSimulationTransferLoadLists
			} //(NumSimulationTransferLoadLists > 0)

			//**** End read transfer load Lists **********************************************************

			//**** Read Compressor Lists **********************************************************
			CurrentModuleObject = "Refrigeration:CompressorList";
			for ( ListNum = 1; ListNum <= NumCompressorLists; ++ListNum ) {
				GetObjectItem( CurrentModuleObject, ListNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				CompressorLists( ListNum ).NumCompressors = NumAlphas - 1;
				VerifyName( Alphas( 1 ), CompressorLists, ListNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				CompressorLists( ListNum ).Name = Alphas( 1 );
				if ( ! allocated( CompressorLists( ListNum ).CompItemNum ) ) CompressorLists( ListNum ).CompItemNum.allocate( CompressorLists( ListNum ).NumCompressors );

				for ( CompIndex = 1; CompIndex <= CompressorLists( ListNum ).NumCompressors; ++CompIndex ) {
					AlphaListNum = CompIndex + 1; //same as do loop from 2 to end of list
					if ( ! lAlphaBlanks( AlphaListNum ) ) {
						CompressorLists( ListNum ).CompItemNum( CompIndex ) = FindItemInList( Alphas( AlphaListNum ), Compressor );
						if ( CompressorLists( ListNum ).CompItemNum( CompIndex ) == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + CompressorLists( ListNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaListNum ) + " defined as " + Alphas( AlphaListNum ) );
							ErrorsFound = true;
						}
					}
				} //NumCompressors in CompressorList

			} //NumCompressorLists

			// ********READ REFRIGERATION SYSTEMS  ***********

			CurrentModuleObject = "Refrigeration:System";
			for ( RefrigSysNum = 1; RefrigSysNum <= NumRefrigSystems; ++RefrigSysNum ) {

				GetObjectItem( CurrentModuleObject, RefrigSysNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;

				VerifyName( Alphas( 1 ), System, RefrigSysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				System( RefrigSysNum ).Name = Alphas( 1 );

				//Read all loads on this System: cases, walk-ins, cascade loads, and secondary loops
				if ( lAlphaBlanks( 2 ) && lAlphaBlanks( 3 ) ) {
					//No cases, walkins, cascade loads, or secondary loops specified, ie, System has no load
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has no loads, must have at least one of: " + cAlphaFieldNames( 2 ) + " or " + cAlphaFieldNames( 3 ) + " objects attached." );
					ErrorsFound = true;
				}
				NumCases = 0;
				System( RefrigSysNum ).NumCases = 0;
				NumCoils = 0;
				System( RefrigSysNum ).NumCoils = 0;
				NumWalkIns = 0;
				System( RefrigSysNum ).NumWalkIns = 0;
				NumSecondary = 0;
				System( RefrigSysNum ).NumSecondarys = 0;
				NumCascadeLoad = 0;
				System( RefrigSysNum ).NumCascadeLoads = 0;
				System( RefrigSysNum ).NumNonCascadeLoads = 0;
				NominalTotalCaseCap = 0.0;
				NominalTotalCoilCap = 0.0;
				NominalTotalWalkInCap = 0.0;
				NominalTotalSecondaryCap = 0.0;
				NominalTotalCoolingCap = 0.0;
				NominalTotalCascadeLoad = 0.0;
				System( RefrigSysNum ).RefInventory = 0.0;

				//   Check for case or walkin or CaseAndWalkInList names
				AlphaNum = 2;
				if ( ! lAlphaBlanks( AlphaNum ) ) {

					// Entry for Alphas(AlphaNum) can be either a Case, WalkIn or CaseAndWalkInList name
					CaseAndWalkInListNum = 0;
					CaseNum = 0;
					WalkInNum = 0;
					CoilNum = 0;
					if ( NumSimulationCaseAndWalkInLists > 0 ) CaseAndWalkInListNum = FindItemInList( Alphas( AlphaNum ), CaseAndWalkInList );
					if ( NumSimulationCases > 0 ) CaseNum = FindItemInList( Alphas( AlphaNum ), RefrigCase );
					if ( NumSimulationWalkIns > 0 ) WalkInNum = FindItemInList( Alphas( AlphaNum ), WalkIn );
					if ( NumSimulationRefrigAirChillers > 0 ) CoilNum = FindItemInList( Alphas( AlphaNum ), WarehouseCoil );
					NumNameMatches = 0;
					if ( CaseAndWalkInListNum != 0 ) ++NumNameMatches;
					if ( CaseNum != 0 ) ++NumNameMatches;
					if ( WalkInNum != 0 ) ++NumNameMatches;
					if ( CoilNum != 0 ) ++NumNameMatches;

					if ( NumNameMatches != 1 ) { //name must uniquely point to a list or a single case or walkin or coil
						ErrorsFound = true;
						if ( NumNameMatches == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} else if ( NumNameMatches > 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\",  has a non-unique name that could be either a " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} //num matches = 0 or > 1
					} else if ( CaseAndWalkInListNum != 0 ) { //Name points to a CaseAndWalkInList
						NumCases = CaseAndWalkInList( CaseAndWalkInListNum ).NumCases;
						NumWalkIns = CaseAndWalkInList( CaseAndWalkInListNum ).NumWalkIns;
						NumCoils = CaseAndWalkInList( CaseAndWalkInListNum ).NumCoils;
						System( RefrigSysNum ).NumCases = NumCases;
						System( RefrigSysNum ).NumWalkIns = NumWalkIns;
						System( RefrigSysNum ).NumCoils = NumCoils;
						if ( NumCases > 0 ) {
							if ( ! allocated( System( RefrigSysNum ).CaseNum ) ) System( RefrigSysNum ).CaseNum.allocate( NumCases );
							System( RefrigSysNum ).CaseNum( {1,NumCases} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CaseItemNum( {1,NumCases} );
						}
						if ( NumCoils > 0 ) {
							if ( ! allocated( System( RefrigSysNum ).CoilNum ) ) System( RefrigSysNum ).CoilNum.allocate( NumCoils );
							System( RefrigSysNum ).CoilNum( {1,NumCoils} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CoilItemNum( {1,NumCoils} );
						}
						if ( NumWalkIns > 0 ) {
							if ( ! allocated( System( RefrigSysNum ).WalkInNum ) ) System( RefrigSysNum ).WalkInNum.allocate( NumWalkIns );
							System( RefrigSysNum ).WalkInNum( {1,NumWalkIns} ) = CaseAndWalkInList( CaseAndWalkInListNum ).WalkInItemNum( {1,NumWalkIns} );
						}
					} else if ( CaseNum != 0 ) { //Name points to a case
						NumCases = 1;
						System( RefrigSysNum ).NumCases = 1;
						if ( ! allocated( System( RefrigSysNum ).CaseNum ) ) System( RefrigSysNum ).CaseNum.allocate( NumCases );
						System( RefrigSysNum ).CaseNum( NumCases ) = CaseNum;
					} else if ( CoilNum != 0 ) { //Name points to a coil
						NumCoils = 1;
						System( RefrigSysNum ).NumCoils = 1;
						if ( ! allocated( System( RefrigSysNum ).CoilNum ) ) System( RefrigSysNum ).CoilNum.allocate( NumCoils );
						System( RefrigSysNum ).CoilNum( NumCoils ) = CoilNum;
					} else if ( WalkInNum != 0 ) { //Name points to a walkin
						NumWalkIns = 1;
						System( RefrigSysNum ).NumWalkIns = 1;
						if ( ! allocated( System( RefrigSysNum ).WalkInNum ) ) System( RefrigSysNum ).WalkInNum.allocate( NumWalkIns );
						System( RefrigSysNum ).WalkInNum( NumWalkIns ) = WalkInNum;
					} //NumNameMatches /= 1
				} //blank input for cases, walkins, or caseandwalkinlist

				if ( NumCases > 0 ) {
					// Find lowest design evap T
					// Sum rated capacity of all cases on system
					for ( CaseIndex = 1; CaseIndex <= NumCases; ++CaseIndex ) {
						//mark all cases on system as used by this system - checking for unused or non-unique cases
						CaseNum = System( RefrigSysNum ).CaseNum( CaseIndex );
						++RefrigCase( CaseNum ).NumSysAttach;
						NominalTotalCaseCap += RefrigCase( CaseNum ).DesignRatedCap;
						System( RefrigSysNum ).RefInventory += RefrigCase( CaseNum ).DesignRefrigInventory;
						if ( CaseIndex == 1 ) { //look for lowest case design evap T for system
							System( RefrigSysNum ).TEvapDesign = RefrigCase( CaseNum ).EvapTempDesign;
						} else {
							System( RefrigSysNum ).TEvapDesign = min( RefrigCase( CaseNum ).EvapTempDesign, System( RefrigSysNum ).TEvapDesign );
						}
					} //CaseIndex=1,NumCases
					System( RefrigSysNum ).NumNonCascadeLoads += System( RefrigSysNum ).NumCases;
				} //Numcases > 0

				if ( NumCoils > 0 ) {
					// Find lowest design evap T
					// Sum rated capacity of all Coils on system
					for ( CoilIndex = 1; CoilIndex <= NumCoils; ++CoilIndex ) {
						//mark all Coils on system as used by this system - checking for unused or non-unique Coils
						CoilNum = System( RefrigSysNum ).CoilNum( CoilIndex );
						++WarehouseCoil( CoilNum ).NumSysAttach;
						NominalTotalCoilCap += WarehouseCoil( CoilNum ).RatedSensibleCap;
						System( RefrigSysNum ).RefInventory += WarehouseCoil( CoilNum ).DesignRefrigInventory;
						if ( ( CoilIndex == 1 ) && ( System( RefrigSysNum ).NumCases == 0 ) ) { //look for lowest Coil design evap T for system
							System( RefrigSysNum ).TEvapDesign = WarehouseCoil( CoilNum ).TEvapDesign;
						} else {
							System( RefrigSysNum ).TEvapDesign = min( WarehouseCoil( CoilNum ).TEvapDesign, System( RefrigSysNum ).TEvapDesign );
						}
					} //CoilIndex=1,NumCoils
					System( RefrigSysNum ).NumNonCascadeLoads += System( RefrigSysNum ).NumCoils;
				} //NumCoils > 0

				if ( NumWalkIns > 0 ) {
					for ( WalkInIndex = 1; WalkInIndex <= NumWalkIns; ++WalkInIndex ) {
						WalkInID = System( RefrigSysNum ).WalkInNum( WalkInIndex );
						//mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
						++WalkIn( WalkInID ).NumSysAttach;
						NominalTotalWalkInCap += WalkIn( WalkInID ).DesignRatedCap;
						System( RefrigSysNum ).RefInventory += WalkIn( WalkInID ).DesignRefrigInventory;
						//Defrost capacity is treated differently by compressor racks and detailed systems,
						//  so this value may be adjusted (or warnings issued) after the walkin is assigned
						//  to either the rack or system.
						//for walkins served by detailed system, need capacity for both fluid and electric types.
						if ( WalkIn( WalkInID ).DefrostCapacity <= -98.0 ) {
							// - 99 used as a flag for blank input error message for detailed systems
							ShowSevereError( RoutineName + "Refrigeration:WalkIn=\"" + WalkIn( WalkInID ).Name + "\", Defrost capacity must be greater than or equal to 0 W for electric and hotfluid defrost types" );
							ErrorsFound = true;
						}
						// Find design evaporating temperature for system by getting min design evap for ALL loads
						if ( ( WalkInIndex == 1 ) && ( System( RefrigSysNum ).NumCases == 0 ) && ( System( RefrigSysNum ).NumCoils == 0 ) ) {
							//note use walk in index, not walkinid here to get
							//first walkin on this suction group/system
							System( RefrigSysNum ).TEvapDesign = WalkIn( WalkInID ).TEvapDesign;
						} else {
							System( RefrigSysNum ).TEvapDesign = min( WalkIn( WalkInID ).TEvapDesign, System( RefrigSysNum ).TEvapDesign );
						}
					} //WalkInIndex=1,NumWalkIns
					System( RefrigSysNum ).NumNonCascadeLoads += System( RefrigSysNum ).NumWalkIns;
				} //numwalkins > 0

				AlphaNum = 3;
				// Read Transfer Loads (Secondary and Cascade) assignments for this System ,
				//     already allow more than one mech subcooler to load onto a system so they don't need to go in list
				if ( ! lAlphaBlanks( AlphaNum ) ) {

					// Entry for Alphas(AlphaNum) can be either a Secondary, CascadeLoad name or a TransferLoadList name
					TransferLoadListNum = 0;
					SecondaryNum = 0;
					CascadeLoadNum = 0;
					if ( NumSimulationTransferLoadLists > 0 ) TransferLoadListNum = FindItemInList( Alphas( AlphaNum ), TransferLoadList );
					if ( NumSimulationSecondarySystems > 0 ) SecondaryNum = FindItemInList( Alphas( AlphaNum ), Secondary );
					if ( NumRefrigCondensers > 0 ) CascadeLoadNum = FindItemInList( Alphas( AlphaNum ), Condenser );
					NumNameMatches = 0;
					if ( TransferLoadListNum != 0 ) ++NumNameMatches;
					if ( SecondaryNum != 0 ) ++NumNameMatches;
					if ( CascadeLoadNum != 0 ) ++NumNameMatches;

					if ( NumNameMatches != 1 ) { //name must uniquely point to a list or a single transfer load
						ErrorsFound = true;
						if ( NumNameMatches == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} else if ( NumNameMatches > 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has a non-unique name that could be either a " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} //num matches = 0 or > 1
					} else if ( TransferLoadListNum != 0 ) { //Name points to a transferLoad list
						NumSecondary = TransferLoadList( TransferLoadListNum ).NumSecondarys;
						NumCascadeLoad = TransferLoadList( TransferLoadListNum ).NumCascadeLoads;
						System( RefrigSysNum ).NumSecondarys = NumSecondary;
						System( RefrigSysNum ).NumCascadeLoads = NumCascadeLoad;
						if ( ! allocated( System( RefrigSysNum ).SecondaryNum ) ) System( RefrigSysNum ).SecondaryNum.allocate( NumSecondary );
						System( RefrigSysNum ).SecondaryNum( {1,NumSecondary} ) = TransferLoadList( TransferLoadListNum ).SecondaryItemNum( {1,NumSecondary} );
						if ( ! allocated( System( RefrigSysNum ).CascadeLoadNum ) ) System( RefrigSysNum ).CascadeLoadNum.allocate( NumCascadeLoad );
						System( RefrigSysNum ).CascadeLoadNum( {1,NumCascadeLoad} ) = TransferLoadList( TransferLoadListNum ).CascadeLoadItemNum( {1,NumCascadeLoad} );
					} else if ( SecondaryNum != 0 ) { //Name points to a secondary loop load
						NumSecondary = 1;
						System( RefrigSysNum ).NumSecondarys = 1;
						if ( ! allocated( System( RefrigSysNum ).SecondaryNum ) ) System( RefrigSysNum ).SecondaryNum.allocate( NumSecondary );
						System( RefrigSysNum ).SecondaryNum( NumSecondary ) = SecondaryNum;
					} else if ( CascadeLoadNum != 0 ) { //Name points to a cascade condenser load
						NumCascadeLoad = 1;
						System( RefrigSysNum ).NumCascadeLoads = 1;
						if ( ! allocated( System( RefrigSysNum ).CascadeLoadNum ) ) System( RefrigSysNum ).CascadeLoadNum.allocate( NumCascadeLoad );
						System( RefrigSysNum ).CascadeLoadNum( NumCascadeLoad ) = CascadeLoadNum;
					} //NumNameMatches /= 1

					System( RefrigSysNum ).CoilFlag = false;
					// Now need to loop through all transfer loads to see if they change the minimum required system evaporating temperature
					if ( NumSecondary > 0 ) {
						for ( SecondaryIndex = 1; SecondaryIndex <= NumSecondary; ++SecondaryIndex ) {
							SecondaryID = System( RefrigSysNum ).SecondaryNum( SecondaryIndex );
							if ( SecondaryIndex == 1 ) { // check for consistency of loads (coils calc on sys time step, all others on zone time step)
								if ( Secondary( SecondaryID ).CoilFlag ) System( RefrigSysNum ).CoilFlag = true;
							} else if ( Secondary( SecondaryID ).CoilFlag != System( RefrigSysNum ).CoilFlag ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Serves an inconsistent mixture of loads. Coil-type loads are served on a different time step than case or walkin loads. Compare loads on system served by secondary loop \"" + Secondary( SecondaryID ).Name );
								ErrorsFound = true;
							} // check for consistency of loads (coils calc on sys time step, all others on zone time step)
							//mark all Secondarys on system as used by this system (checking for unused or non-unique Secondarys)
							++Secondary( SecondaryID ).NumSysAttach;
							NominalTotalSecondaryCap += Secondary( SecondaryID ).CoolingLoadRated;
							System( RefrigSysNum ).RefInventory += Secondary( SecondaryID ).ChillerRefInventory;
							// Find design evaporating temperature for system by getting min design evap for ALL loads
							if ( ( SecondaryIndex == 1 ) && ( System( RefrigSysNum ).NumCases == 0 ) && ( System( RefrigSysNum ).NumCoils == 0 ) && ( System( RefrigSysNum ).NumWalkIns == 0 ) ) {
								//note use secondary index above, not secondaryid here to get
								//first secondary on this suction group/system
								//note - TMinNeeded on secondary defined by cases and walkins served by secondary, not by
								//       the secondary's rated evaporating temperature (which is used to calc secondary heat
								//       exchanger effectiveness with other rated values)
								System( RefrigSysNum ).TEvapDesign = Secondary( SecondaryID ).TMinNeeded;
							} else {
								System( RefrigSysNum ).TEvapDesign = min( Secondary( SecondaryID ).TMinNeeded, System( RefrigSysNum ).TEvapDesign );
							}
						} //SecondaryIndex=1,NumSecondary
						System( RefrigSysNum ).NumNonCascadeLoads += System( RefrigSysNum ).NumSecondarys;
					} //numsecondary > 0

					if ( NumCascadeLoad > 0 ) {
						for ( CascadeLoadIndex = 1; CascadeLoadIndex <= NumCascadeLoad; ++CascadeLoadIndex ) {
							CondID = System( RefrigSysNum ).CascadeLoadNum( CascadeLoadIndex );
							if ( Condenser( CondID ).CondenserType != RefrigCondenserTypeCascade ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has a  " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) + " cascade load that is not a cascade condenser." );
								ErrorsFound = true;
							}
							// For a cascade condenser, need to identify the system absorbing the heat
							Condenser( CondID ).CascadeSinkSystemID = RefrigSysNum;
							NominalTotalCascadeLoad += Condenser( CondID ).RatedCapacity;
							// Find design evaporating temperature for system by getting min design evap for ALL loads
							if ( System( RefrigSysNum ).NumNonCascadeLoads == 0 ) {
								if ( CascadeLoadIndex == 1 ) {
									//note use cascadeload index above, not condid here to get
									//first cascade condenser served by this suction group/system
									System( RefrigSysNum ).TEvapDesign = Condenser( CondID ).CascadeRatedEvapTemp;
								} else {
									System( RefrigSysNum ).TEvapDesign = min( Condenser( CondID ).CascadeRatedEvapTemp, System( RefrigSysNum ).TEvapDesign );
								} // CascadeLoadIndex == 1
							} else { // (NumNonCascadeLoads > 0 so initial TEvapDesign set above with those other loads)
								if ( Condenser( CondID ).CascadeTempControl == CascadeTempSet ) System( RefrigSysNum ).TEvapDesign = min( Condenser( CondID ).CascadeRatedEvapTemp, System( RefrigSysNum ).TEvapDesign ); // other wise TEvapDesign set by other loads
							}
						} //CascadeLoadIndex=1,NumCascadeLoad
					} //CascadeLoadNum > 0
				} //yes/no blank input for transfer loads

				// check for consistency of loads (coils calc on sys time step, all others on zone time step, so can't mix on one system)
				if ( System( RefrigSysNum ).CoilFlag ) { //could already be true if serving secondary that serves coils
					if ( ( System( RefrigSysNum ).NumCases > 0 ) || ( System( RefrigSysNum ).NumWalkIns > 0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Serves an inconsistent mixture of loads. Coil-type loads are served on a different time step than case or walkin loads." );
						ErrorsFound = true;
					}
				} else { //no coils on secondary or no secondary
					if ( System( RefrigSysNum ).NumCoils > 0 ) { //(note, coilflag set to .FALSE. for all systems as default above
						System( RefrigSysNum ).CoilFlag = true;
						if ( ( System( RefrigSysNum ).NumCases > 0 ) || ( System( RefrigSysNum ).NumWalkIns > 0 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Serves an inconsistent mixture of loads. Coil-type loads are served on a different time step than case or walkin loads." );
							ErrorsFound = true;
						}
					} // NumCoils > 0
				} //Coil flag already true due to secondary coil loads

				NominalTotalCoolingCap = NominalTotalCaseCap + NominalTotalWalkInCap + NominalTotalSecondaryCap + NominalTotalCascadeLoad;

				// read condenser
				// currently assumes one condenser per refrigeration system and but multiple systems allowed per condenser
				AlphaNum = 4;
				NumCondensers = 1;
				if ( ! allocated( System( RefrigSysNum ).CondenserNum ) ) System( RefrigSysNum ).CondenserNum.allocate( NumCondensers );
				System( RefrigSysNum ).NumCondensers = 1;
				//Find condenser number, note condensers were read in one of four objects, but all read into same list
				CondNum = FindItemInList( Alphas( AlphaNum ), Condenser );
				if ( CondNum == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + " defined as " + Alphas( AlphaNum ) );
					ErrorsFound = true;
				} else {
					System( RefrigSysNum ).CondenserNum( NumCondensers ) = CondNum;
					//Now take care of case where multiple systems share a condenser
					++Condenser( CondNum ).NumSysAttach;
					Condenser( CondNum ).SysNum( Condenser( CondNum ).NumSysAttach ) = RefrigSysNum;
				}

				System( RefrigSysNum ).RefInventory += Condenser( CondNum ).RefReceiverInventory + Condenser( CondNum ).RefPipingInventory + Condenser( CondNum ).RefOpCharge;
				if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeCascade ) Condenser( CondNum ).CascadeSysID = RefrigSysNum;
				if ( ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeAir ) && ( Condenser( CondNum ).CondenserRejectHeatToZone ) ) System( RefrigSysNum ).SystemRejectHeatToZone = true;

				//Now do evaporative condenser auto sizing because it is a function of the system's cooling load
				if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeEvap ) {
					if ( Condenser( CondNum ).RatedAirFlowRate == AutoCalculate ) {
						Condenser( CondNum ).RatedAirFlowRate = AirVolRateEvapCond * Condenser( CondNum ).RatedCapacity;
					}
					if ( Condenser( CondNum ).RatedAirFlowRate <= 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", Evaporative Condenser Air Volume Flow Rate cannot be less than or equal to zero." );
						ErrorsFound = true;
					}
					if ( Condenser( CondNum ).EvapPumpPower == AutoCalculate ) {
						Condenser( CondNum ).EvapPumpPower = CondPumpRatePower * Condenser( CondNum ).RatedCapacity;
					}
					if ( Condenser( CondNum ).EvapPumpPower < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Condenser( CondNum ).Name + "\", Design Evaporative Condenser Water Pump Power cannot be less than zero." );
						ErrorsFound = true;
					}
				}

				// Read the compressor data.
				// If the system consists of two stages of compression, these compressors will be the low-stage compressors.
				AlphaNum = 5;
				NumCompressorsSys = 0;
				if ( lAlphaBlanks( AlphaNum ) ) {
					//blank input where must have compressor or compressor list input.
					ShowSevereError( RoutineName + CurrentModuleObject + ' ' + cAlphaFieldNames( AlphaNum ) + "\" : must be input." );
					ErrorsFound = true;
				} else { //     Entry for Alphas(AlphaNum) can be either a compressor name or a compressorlist name
					if ( NumCompressorLists > 0 ) {
						ListNum = FindItemInList( Alphas( AlphaNum ), CompressorLists );
					} else {
						ListNum = 0;
					}
					if ( NumSimulationCompressors > 0 ) {
						CompNum = FindItemInList( Alphas( AlphaNum ), Compressor );
					} else {
						CompNum = 0;
					}
					if ( ( ListNum == 0 ) && ( CompNum == 0 ) ) { // name doesn't match either a compressor or a compressor list
						ShowSevereError( RoutineName + CurrentModuleObject + ' ' + cAlphaFieldNames( AlphaNum ) + ", has an invalid or undefined value=\"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					} else if ( ( ListNum != 0 ) && ( CompNum != 0 ) ) { //have compressor list and compressor with same name
						ShowSevereError( RoutineName + CurrentModuleObject + ' ' + cAlphaFieldNames( AlphaNum ) + ", has a non-unique name used for both Compressor and CompressorList name: \"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					} else if ( ListNum != 0 ) {
						NumCompressorsSys = CompressorLists( ListNum ).NumCompressors;
						System( RefrigSysNum ).NumCompressors = NumCompressorsSys;
						if ( ! allocated( System( RefrigSysNum ).CompressorNum ) ) System( RefrigSysNum ).CompressorNum.allocate( NumCompressorsSys );
						System( RefrigSysNum ).CompressorNum( {1,NumCompressorsSys} ) = CompressorLists( ListNum ).CompItemNum( {1,NumCompressorsSys} );
					} else if ( CompNum != 0 ) {
						NumCompressorsSys = 1;
						System( RefrigSysNum ).NumCompressors = 1;
						if ( ! allocated( System( RefrigSysNum ).CompressorNum ) ) System( RefrigSysNum ).CompressorNum.allocate( NumCompressorsSys );
						System( RefrigSysNum ).CompressorNum( NumCompressorsSys ) = CompNum;
					}
				}

				if ( ! lNumericBlanks( 1 ) ) {
					System( RefrigSysNum ).TCondenseMin = Numbers( 1 );
					System( RefrigSysNum ).TCondenseMinInput = System( RefrigSysNum ).TCondenseMin;
					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "Refrigeration:System", System( RefrigSysNum ).Name, "Minimum Condensing Temperature", "[C]", System( RefrigSysNum ).EMSOverrideOnTCondenseMin, System( RefrigSysNum ).EMSOverrideValueTCondenseMin );
					}
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", " + cNumericFieldNames( 1 ) + " must be defined." );
					ErrorsFound = true;
				}
				if ( ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeCascade ) && ( System( RefrigSysNum ).TCondenseMin > Condenser( CondNum ).RatedTCondense ) ) ShowWarningError( CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", The system specified minimum condensing temperature is greater than the rated condensing temperature for the cascade condenser. " );

				AlphaNum = 6;
				System( RefrigSysNum ).RefrigerantName = Alphas( AlphaNum );
				//error messages for refrigerants already found in fluidproperties

				AlphaNum = 7;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					if ( SameString( Alphas( AlphaNum ), "ConstantSuctionTemperature" ) ) {
						System( RefrigSysNum ).CompSuctControl = ConstantSuctionTemperature;
					} else if ( SameString( Alphas( AlphaNum ), "FloatSuctionTemperature" ) ) {
						System( RefrigSysNum ).CompSuctControl = FloatSuctionTemperature;
						if ( System( RefrigSysNum ).CoilFlag ) {
							ShowWarningError( CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", The system specified a FloatSuctionTemperature, but that is not available with air chiller loads so ConstantSuctionTemperature will be used. " );
						} //coilflag
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", invalid  " + cAlphaFieldNames( AlphaNum ) + " not found = " + Alphas( AlphaNum ) );
						ErrorsFound = true;
					}
				} else {
					System( RefrigSysNum ).CompSuctControl = ConstantSuctionTemperature; //Default for blank
				}

				//Count subcoolers on system and allocate
				AlphaNum = 8;
				System( RefrigSysNum ).NumSubcoolers = 0;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					++System( RefrigSysNum ).NumSubcoolers;
				}
				if ( ! lAlphaBlanks( AlphaNum + 1 ) ) {
					++System( RefrigSysNum ).NumSubcoolers;
				}

				if ( System( RefrigSysNum ).NumSubcoolers > 0 ) {
					if ( ! allocated( System( RefrigSysNum ).SubcoolerNum ) ) System( RefrigSysNum ).SubcoolerNum.allocate( System( RefrigSysNum ).NumSubcoolers );
					NumSubcooler = 1;
					if ( ! lAlphaBlanks( AlphaNum ) ) {
						System( RefrigSysNum ).SubcoolerNum( NumSubcooler ) = GetObjectItemNum( "Refrigeration:Subcooler", Alphas( AlphaNum ) );
						if ( System( RefrigSysNum ).SubcoolerNum( NumSubcooler ) <= 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + " defined as \"" + Alphas( AlphaNum ) + "\"." );
							ErrorsFound = true;
						} else {
							Subcooler( System( RefrigSysNum ).SubcoolerNum( NumSubcooler ) ).CoilFlag = System( RefrigSysNum ).CoilFlag;
						}
						++NumSubcooler;
					}
					if ( ! lAlphaBlanks( AlphaNum + 1 ) ) {
						System( RefrigSysNum ).SubcoolerNum( NumSubcooler ) = GetObjectItemNum( "Refrigeration:Subcooler", Alphas( AlphaNum + 1 ) );
						if ( System( RefrigSysNum ).SubcoolerNum( NumSubcooler ) <= 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum + 1 ) + " defined as \"" + Alphas( AlphaNum + 1 ) + "\"." );
							ErrorsFound = true;
						} else {
							Subcooler( System( RefrigSysNum ).SubcoolerNum( NumSubcooler ) ).CoilFlag = System( RefrigSysNum ).CoilFlag;
						}
					}
				}

				//Suction piping heat gain - optional
				//  Input UA and identify the Zone containing the bulk of the suction piping
				//  This Zone ID will be used to determine the temperature used for suction piping heat gain.
				//  The pipe heat gains are also counted as cooling credit for the zone.
				//  Zone Id is only required if Sum UA Suction Piping >0.0
				//  Get the Zone and zone node numbers from the zone name entered by the user
				AlphaNum = 10;
				System( RefrigSysNum ).SumUASuctionPiping = 0.0;
				if ( ! lNumericBlanks( 2 ) && ! lAlphaBlanks( AlphaNum ) ) {
					System( RefrigSysNum ).SumUASuctionPiping = Numbers( 2 );
					System( RefrigSysNum ).SuctionPipeActualZoneNum = FindItemInList( Alphas( AlphaNum ), Zone );
					System( RefrigSysNum ).SuctionPipeZoneNodeNum = GetSystemNodeNumberForZone( Alphas( AlphaNum ) );
					if ( System( RefrigSysNum ).SuctionPipeZoneNodeNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", System Node Number not found for " + cAlphaFieldNames( AlphaNum ) + " = " + Alphas( AlphaNum ) + " even though " + cNumericFieldNames( 2 ) + " is greater than zero. Suction piping heat gain cannot be calculated unless a Zone is defined to deterimine the environmental temperature surrounding the piping." );
						ErrorsFound = true;
					} else {
						RefrigPresentInZone( System( RefrigSysNum ).SuctionPipeActualZoneNum ) = true;
					}
				} else if ( ! lNumericBlanks( 2 ) && lAlphaBlanks( AlphaNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " not found even though " + cNumericFieldNames( 2 ) + " is greater than zero. Suction piping heat gain will not be calculated unless a Zone is defined to determine the environmental temperature surrounding the piping." );
				} else if ( lNumericBlanks( 2 ) && ! lAlphaBlanks( AlphaNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " will not be used and suction piping heat gain will not be calculated because " + cNumericFieldNames( 2 ) + " was blank." );
				} //suction piping heat gains

				AlphaNum = 11;
				if ( ! lAlphaBlanks( AlphaNum ) ) System( RefrigSysNum ).EndUseSubcategory = Alphas( AlphaNum );

				// Single-stage or two-stage compression system
				if ( ! lNumericBlanks( 3 ) ) {
					System( RefrigSysNum ).NumStages = Numbers( 3 );
					if ( System( RefrigSysNum ).NumStages < 1 || System( RefrigSysNum ).NumStages > 2 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", " + cNumericFieldNames( 3 ) + " has an invalid value.  Only \"1\" or \"2\" compressor stages are allowed." );
						ErrorsFound = true;
					}
				} else {
					System( RefrigSysNum ).NumStages = 1; //Default for blank
				}

				// Intercooler type
				// None (0) for single-stage compression systems
				// Flash intercooler (1) or coil-and-shell intercooler (2) for two-stage compression systems
				AlphaNum = 12;
				if ( ! lAlphaBlanks( AlphaNum ) ) {
					if ( SameString( Alphas( AlphaNum ), "None" ) ) {
						System( RefrigSysNum ).IntercoolerType = 0;
					} else if ( SameString( Alphas( AlphaNum ), "Flash Intercooler" ) ) {
						System( RefrigSysNum ).IntercoolerType = 1;
					} else if ( SameString( Alphas( AlphaNum ), "Shell-and-Coil Intercooler" ) ) {
						System( RefrigSysNum ).IntercoolerType = 2;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Invalid " + cAlphaFieldNames( AlphaNum ) + " specified." );
						ShowContinueError( "\"" + Alphas( AlphaNum ) + "\" is not a recognized intercooler type." );
						ErrorsFound = true;
					}
				} else {
					System( RefrigSysNum ).IntercoolerType = 0; //Default for blank
				}

				if ( System( RefrigSysNum ).NumStages == 1 && ( System( RefrigSysNum ).IntercoolerType == 1 || System( RefrigSysNum ).IntercoolerType == 2 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", A single-stage compression system" );
					ShowContinueError( "has been specified with an intercooler.  Verify that the number of compressor stages" );
					ShowContinueError( "and the intercooler type are consistent." );
					ErrorsFound = true;
				} else if ( System( RefrigSysNum ).NumStages == 2 && System( RefrigSysNum ).IntercoolerType == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", A two-stage compression system" );
					ShowContinueError( "has been specified without an intercooler.  Verify that the number of compressor stages" );
					ShowContinueError( "and the intercooler type are consistent." );
					ErrorsFound = true;
				}

				// Shell-and-coil intercooler effectiveness
				if ( ! lNumericBlanks( 4 ) ) {
					System( RefrigSysNum ).IntercoolerEffectiveness = Numbers( 4 );
					if ( System( RefrigSysNum ).IntercoolerEffectiveness < 0.0 || System( RefrigSysNum ).IntercoolerEffectiveness > 1.0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", The specified value for the" );
						ShowContinueError( cNumericFieldNames( 4 ) + " = " + RoundSigDigits( System( RefrigSysNum ).IntercoolerEffectiveness, 2 ) + " is invalid.  This value must be" );
						ShowContinueError( "between 0.0 and 1.0.  The default value of 0.8 will be used." );
						System( RefrigSysNum ).IntercoolerEffectiveness = 0.8;
					}
				} else {
					System( RefrigSysNum ).IntercoolerEffectiveness = 0.8;
				}

				// Read the high-stage compressor info, if two-stage compression has been specified.
				AlphaNum = 13;
				NumHiStageCompressorsSys = 0;
				if ( System( RefrigSysNum ).NumStages == 2 ) {
					if ( lAlphaBlanks( AlphaNum ) ) {
						//blank input where must have high-stage compressor or compressor list input.
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " must be input for two-stage compression systems." );
						ErrorsFound = true;
					} else { //     Entry for Alphas(AlphaNum) can be either a compressor name or a compressorlist name
						ListNum = FindItemInList( Alphas( AlphaNum ), CompressorLists );
						CompNum = FindItemInList( Alphas( AlphaNum ), Compressor );
						if ( ( ListNum == 0 ) && ( CompNum == 0 ) ) { // name doesn't match either a compressor or a compressor list
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " has an invalid or undefined value=\"" + Alphas( AlphaNum ) + "\"." );
							ErrorsFound = true;
						} else if ( ( ListNum != 0 ) && ( CompNum != 0 ) ) { //have compressor list and compressor with same name
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", " + cAlphaFieldNames( AlphaNum ) + " has a non-unique name used for both Compressor and CompressorList name: \"" + Alphas( AlphaNum ) + "\"." );
							ErrorsFound = true;
						} else if ( ListNum != 0 ) {
							NumHiStageCompressorsSys = CompressorLists( ListNum ).NumCompressors;
							System( RefrigSysNum ).NumHiStageCompressors = NumHiStageCompressorsSys;
							if ( ! allocated( System( RefrigSysNum ).HiStageCompressorNum ) ) System( RefrigSysNum ).HiStageCompressorNum.allocate( NumHiStageCompressorsSys );
							System( RefrigSysNum ).HiStageCompressorNum( {1,NumHiStageCompressorsSys} ) = CompressorLists( ListNum ).CompItemNum( {1,NumHiStageCompressorsSys} );
						} else if ( CompNum != 0 ) {
							NumHiStageCompressorsSys = 1;
							System( RefrigSysNum ).NumHiStageCompressors = 1;
							if ( ! allocated( System( RefrigSysNum ).HiStageCompressorNum ) ) System( RefrigSysNum ).HiStageCompressorNum.allocate( NumHiStageCompressorsSys );
							System( RefrigSysNum ).HiStageCompressorNum( NumHiStageCompressorsSys ) = CompNum;
						}
					}
				}

				// Determine intercooler pressure and temperature at design conditions
				if ( System( RefrigSysNum ).NumStages == 2 ) {
					PCond = GetSatPressureRefrig( System( RefrigSysNum ).RefrigerantName, Condenser( System( RefrigSysNum ).CondenserNum( 1 ) ).RatedTCondense, System( RefrigSysNum ).RefIndex, RoutineName );
					PEvap = GetSatPressureRefrig( System( RefrigSysNum ).RefrigerantName, System( RefrigSysNum ).TEvapDesign, System( RefrigSysNum ).RefIndex, RoutineName );
					System( RefrigSysNum ).PIntercooler = std::sqrt( PCond * PEvap );
					System( RefrigSysNum ).TIntercooler = GetSatTemperatureRefrig( System( RefrigSysNum ).RefrigerantName, System( RefrigSysNum ).PIntercooler, System( RefrigSysNum ).RefIndex, RoutineName );
				} // NumStages

				// Sum capacity of single-stage compressors or low-stage compressors if two-stage system
				NominalTotalCompCap = 0.0;
				for ( CompIndex = 1; CompIndex <= NumCompressorsSys; ++CompIndex ) {
					CompNum = System( RefrigSysNum ).CompressorNum( CompIndex );
					if ( ! Compressor( CompNum ).TransFlag ) { //  Subcritical Compressor
						if ( System( RefrigSysNum ).NumStages == 1 ) { //  Single-stage compression
							Compressor( CompNum ).NomCap = CurveValue( Compressor( CompNum ).CapacityCurvePtr, System( RefrigSysNum ).TEvapDesign, Condenser( System( RefrigSysNum ).CondenserNum( 1 ) ).RatedTCondense );
							NominalTotalCompCap += Compressor( CompNum ).NomCap;
							++Compressor( CompNum ).NumSysAttach;
						} else { //  Two-stage compression, low-stage compressors
							Compressor( CompNum ).NomCap = CurveValue( Compressor( CompNum ).CapacityCurvePtr, System( RefrigSysNum ).TEvapDesign, System( RefrigSysNum ).TIntercooler );
							NominalTotalCompCap += Compressor( CompNum ).NomCap;
							++Compressor( CompNum ).NumSysAttach;
						} // NumStages
					} else { //  Transcritical compressor attached to subcritical refigeration cycle
						ShowSevereError( RoutineName + CurrentModuleObject + ". A transcritical compressor is attached to a subcritical refrigeration system." );
						ShowContinueError( "Check input to ensure that subcritical compressors are connected only to subcritical systems and transcritical compressors are connected only to transcritical systems." );
						ErrorsFound = true;
					} // .NOT. Compressor(CompNum)%TransFlag
				}

				// Sum capacity of high-stage compressors if two stage system
				if ( System( RefrigSysNum ).NumStages == 2 ) {
					for ( CompIndex = 1; CompIndex <= NumHiStageCompressorsSys; ++CompIndex ) {
						CompNum = System( RefrigSysNum ).HiStageCompressorNum( CompIndex );
						if ( ! Compressor( CompNum ).TransFlag ) { //  Subcritical Compressor
							Compressor( CompNum ).NomCap = CurveValue( Compressor( CompNum ).CapacityCurvePtr, System( RefrigSysNum ).TIntercooler, Condenser( System( RefrigSysNum ).CondenserNum( 1 ) ).RatedTCondense );
							NominalTotalHiStageCompCap += Compressor( CompNum ).NomCap;
							++Compressor( CompNum ).NumSysAttach;
						} else { //  Transcritical compressor attached to subcritical refigeration cycle
							ShowSevereError( RoutineName + CurrentModuleObject + ". A transcritical compressor is attached to a subcritical refrigeration system." );
							ShowContinueError( "Check input to ensure that subcritical compressors are connected only to subcritical systems and transcritical compressors are connected only to transcritical systems." );
							ErrorsFound = true;
						}
					}
				} // NumStages

				//Compare the rated capacity of compressor, condenser, and cases.
				// Note, rated capacities can be far off from operating capacities, but rough check.
				NominalCondCap = Condenser( System( RefrigSysNum ).CondenserNum( 1 ) ).RatedCapacity;
				if ( System( RefrigSysNum ).SystemRejectHeatToZone ) NominalCondCap *= 2.0;
				if ( System( RefrigSysNum ).NumStages == 1 ) { // Single-stage system
					if ( ( NominalTotalCompCap < ( 0.7 * NominalTotalCoolingCap ) ) || ( NominalCondCap < ( 1.3 * NominalTotalCoolingCap ) ) ) {
						ShowWarningError( CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", You may wish to check the system sizing. Total nominal cooling capacity is " + RoundSigDigits( NominalTotalCoolingCap, 0 ) + "W. Condenser capacity is " + RoundSigDigits( NominalCondCap, 0 ) + "W. Nominal compressor capacity is " + RoundSigDigits( NominalTotalCompCap, 0 ) + "W." );
					}
				} else if ( System( RefrigSysNum ).NumStages == 2 ) { // Two-stage system
					if ( ( NominalTotalHiStageCompCap < ( 0.7 * NominalTotalCoolingCap ) ) || ( NominalCondCap < ( 1.3 * NominalTotalCoolingCap ) ) ) {
						ShowWarningError( CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", You may wish to check the system sizing. Total nominal cooling capacity is " + RoundSigDigits( NominalTotalCoolingCap, 0 ) + "W. Condenser capacity is " + RoundSigDigits( NominalCondCap, 0 ) + "W. Nominal compressor capacity is " + RoundSigDigits( NominalTotalCompCap, 0 ) + "W." );
					}
				} // NumStages

			} // Refrigeration systems

			// Assign coilflags to compressors, condensers, and subcoolers (coils calc on sys time step, all other refrig loads on zone time step, so can't mix on one system)
			// need to do here once again after all cascade condensers and cascade sink systems have been identified
			for ( RefrigSysNum = 1; RefrigSysNum <= NumRefrigSystems; ++RefrigSysNum ) {
				//assign flags to all condensers to match system below condenser (system rejecting heat to cascade condenser)
				CondNum = System( RefrigSysNum ).CondenserNum( 1 ); // right now only have one condenser per system
				Condenser( CondNum ).CoilFlag = System( RefrigSysNum ).CoilFlag;
				for ( CompIndex = 1; CompIndex <= System( RefrigSysNum ).NumCompressors; ++CompIndex ) {
					CompNum = System( RefrigSysNum ).CompressorNum( CompIndex );
					Compressor( CompNum ).CoilFlag = System( RefrigSysNum ).CoilFlag;
				}

			} //assign coil flags to all condensers

			//Finished setting cascade condenser coilflags to match system rejecting heat to the cascade condenser
			// Now have to see if there's a mismatch in the coilflag with the system absorbing heat from the cascade condenser
			// Note a system can cool multiple cascade condensers.  If so, need to be sure all are consistent - all coil or all non-coil(called case here)
			// check for consistency of loads (coils calc on sys time step, all others on zone time step, so can't mix on one system)
			for ( RefrigSysNum = 1; RefrigSysNum <= NumRefrigSystems; ++RefrigSysNum ) { //check flags for systems reflect all cascade loads
				if ( System( RefrigSysNum ).NumCascadeLoads == 0 ) continue;
				if ( System( RefrigSysNum ).CoilFlag ) { //system already identified as serving coils
					for ( CondID = 1; CondID <= NumRefrigCondensers; ++CondID ) {
						if ( Condenser( CondID ).CondenserType != RefrigCondenserTypeCascade ) continue;
						if ( RefrigSysNum != Condenser( CondID ).CascadeSinkSystemID ) continue; //this condenser is not a cascade load on this system
						if ( ! Condenser( CondID ).CoilFlag ) {
							//would mean system already serving coil loads and this condenser cooling system with case-type loads
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Serves an inconsistent mixture of loads. Coil-type loads are served on a different time step than case or walkin loads. Compare loads on system served by cascade condenser \"" + Condenser( CondID ).Name );
							ErrorsFound = true;
						}
					} //CondID
				} else { // %coilflag == false, so no coil loads prev identified directly or through secondary loop
					CaseLoads = false;
					NumCascadeLoadsChecked = 0;
					for ( CondID = 1; CondID <= NumRefrigCondensers; ++CondID ) { //look at All cascade condenser loads on system
						if ( Condenser( CondID ).CondenserType != RefrigCondenserTypeCascade ) continue;
						if ( RefrigSysNum != Condenser( CondID ).CascadeSinkSystemID ) continue; //this condenser is not a cascade load on this system
						++NumCascadeLoadsChecked;
						if ( ( CaseLoads ) && ( ! Condenser( CondID ).CoilFlag ) && ( ! System( RefrigSysNum ).CoilFlag ) ) continue;
						//all loads to date are case-type and properly flagged with consistent coilflags
						//(note caseloads could be true if prev cascade load checked is serving a case-type system)
						if ( NumCascadeLoadsChecked == 1 ) {
							if ( Condenser( CondID ).CoilFlag ) {
								System( RefrigSysNum ).CoilFlag = true;
								//setting system coilflag if 1st cascade condenser served has coils (system has no case-type loads up to this point)
							} else { //condenser is not serving coils, but case-type loads
								CaseLoads = true;
								//system coilflag already set to false
							} //Condenser%CoilFlag
						} else { //numcascadeloadschecked > 1
							if ( System( RefrigSysNum ).CoilFlag != Condenser( CondID ).CoilFlag ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Serves an inconsistent mixture of loads. Coil-type loads are served on a different time step than case or walkin loads. Compare loads on system served by cascade condenser \"" + Condenser( CondID ).Name );
								ErrorsFound = true;
							}
						} //numcascadeloadschecked > 1
					} // CondID
				} //(System%coilflag)
			} // Refrigeration systems checking coilflag consistency with cascade condenser loads

		} //(NumRefrigSystems > 0)

		//after the systems have been read, can finish the mechanical subcooler/system interactions
		//System%NumMechSCServed=0
		if ( NumSimulationSubcoolers > 0 ) {
			for ( SubcoolerNum = 1; SubcoolerNum <= NumSimulationSubcoolers; ++SubcoolerNum ) {
				if ( Subcooler( SubcoolerNum ).SubcoolerType == LiquidSuction ) continue;
				Subcooler( SubcoolerNum ).MechSourceSysID = GetObjectItemNum( "Refrigeration:System", Subcooler( SubcoolerNum ).MechSourceSys );
				if ( Subcooler( SubcoolerNum ).MechSourceSysID == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Subcooler( SubcoolerNum ).Name + "\", Mechanical Subcooler has an invalid Source Refrigeration:System=\"" + Subcooler( SubcoolerNum ).MechSourceSys + "\"." );
					ErrorsFound = true;
				} else {
					if ( System( Subcooler( SubcoolerNum ).MechSourceSysID ).CoilFlag != Subcooler( SubcoolerNum ).CoilFlag ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + System( RefrigSysNum ).Name + "\", Serves an inconsistent mixture of loads. Coil-type loads are served on a different time step than case or walkin loads. Compare loads on system served by mechanical subcooler \"" + Subcooler( SubcoolerNum ).Name );
						ErrorsFound = true;
					}
				} //error check
			} // numsubcoolers

			for ( RefrigSysNum = 1; RefrigSysNum <= NumRefrigSystems; ++RefrigSysNum ) {
				for ( SubcoolerNum = 1; SubcoolerNum <= NumSimulationSubcoolers; ++SubcoolerNum ) {
					if ( Subcooler( SubcoolerNum ).SubcoolerType == LiquidSuction ) continue;
					if ( Subcooler( SubcoolerNum ).MechSourceSysID == RefrigSysNum ) {
						++System( RefrigSysNum ).NumMechSCServed;
					}
				}
				if ( System( RefrigSysNum ).NumMechSCServed > 0 ) {
					if ( ! allocated( System( RefrigSysNum ).MechSCLoad ) ) System( RefrigSysNum ).MechSCLoad.allocate( NumSimulationSubcoolers );
				}
			}
		} // NumSimulationSubcoolers > 0

		// **********  READ TRANSCRITICAL REFRIGERATION SYSTEMS  **********

		if ( NumTransRefrigSystems > 0 ) {
			CurrentModuleObject = "Refrigeration:TranscriticalSystem";
			for ( TransRefrigSysNum = 1; TransRefrigSysNum <= NumTransRefrigSystems; ++TransRefrigSysNum ) {
				GetObjectItem( CurrentModuleObject, TransRefrigSysNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), TransSystem, RefrigSysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ", has an invalid or undefined " + cAlphaFieldNames( 1 ) + "=\"" + Alphas( 1 ) + "\"." );
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
					ErrorsFound = true;
				}
				TransSystem( TransRefrigSysNum ).Name = Alphas( 1 );

				// Read refrigerant for this system
				AlphaNum = 8;
				TransSystem( TransRefrigSysNum ).RefrigerantName = Alphas( AlphaNum );
				//error messages for refrigerants already found in fluidproperties

				// Read Transcritical System Type:  SingleStage or TwoStage
				if ( lAlphaBlanks( 2 ) ) {
					// No system type specified
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", has no system type specified." );
					ShowContinueError( "  System type must be specified as \"SingleStage\" or \"TwoStage\"." );
					ErrorsFound = true;
				}
				if ( SameString( Alphas( 2 ), "SingleStage" ) ) {
					TransSystem( TransRefrigSysNum ).TransSysType = 1;
				} else if ( SameString( Alphas( 2 ), "TwoStage" ) ) {
					TransSystem( TransRefrigSysNum ).TransSysType = 2;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", has an incorrect System Type specified as \"" + Alphas( 2 ) + "\"." );
					ShowContinueError( "  System type must be specified as \"SingleStage\" or \"TwoStage\"." );
					ErrorsFound = true;
				}

				// Read all loads (display cases and walk-ins) on this Transcritical System
				if ( lAlphaBlanks( 3 ) && lAlphaBlanks( 4 ) ) {
					// No loads specified - display error
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", has no loads." );
					ShowContinueError( "  The system must have at least one of: " + cAlphaFieldNames( 3 ) + " or " + cAlphaFieldNames( 4 ) + " objects attached." );
					ErrorsFound = true;
				} else if ( lAlphaBlanks( 3 ) && TransSystem( TransRefrigSysNum ).TransSysType == 1 ) {
					// No medium temperature loads specified for a SingleStage system - display error
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", is a \"SingleStage\" system but no medium temperature loads are specified." );
					ShowContinueError( "  The system must have at least one " + cAlphaFieldNames( 3 ) + " object attached." );
					ErrorsFound = true;
				} else if ( lAlphaBlanks( 4 ) && TransSystem( TransRefrigSysNum ).TransSysType == 2 ) {
					// No low temperature loads specified for a TwoStage system - display error
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", is a \"TwoStage\" system but no low temperature loads are specified." );
					ShowContinueError( "  The system must have at least one " + cAlphaFieldNames( 4 ) + " object attached." );
					ErrorsFound = true;
				}

				NumCasesMT = 0;
				TransSystem( TransRefrigSysNum ).NumCasesMT = 0;
				NumCasesLT = 0;
				TransSystem( TransRefrigSysNum ).NumCasesLT = 0;
				NumWalkInsMT = 0;
				TransSystem( TransRefrigSysNum ).NumWalkInsMT = 0;
				NumWalkInsLT = 0;
				TransSystem( TransRefrigSysNum ).NumWalkInsLT = 0;
				NominalTotalCaseCapMT = 0.0;
				NominalTotalCaseCapLT = 0.0;
				NominalTotalWalkInCapMT = 0.0;
				NominalTotalWalkInCapLT = 0.0;
				NominalTotalCoolingCap = 0.0;
				TransSystem( TransRefrigSysNum ).RefInventory = 0.0;

				//   Check for Medium Temperature Case or Walk-In or CaseAndWalkInList names
				AlphaNum = 3;

				if ( ! lAlphaBlanks( AlphaNum ) ) {

					// Entry for Alphas(AlphaNum) can be either a Case, WalkIn or CaseAndWalkInList name
					CaseAndWalkInListNum = 0;
					CaseNum = 0;
					WalkInNum = 0;
					if ( NumSimulationCaseAndWalkInLists > 0 ) CaseAndWalkInListNum = FindItemInList( Alphas( AlphaNum ), CaseAndWalkInList );
					if ( NumSimulationCases > 0 ) CaseNum = FindItemInList( Alphas( AlphaNum ), RefrigCase );
					if ( NumSimulationWalkIns > 0 ) WalkInNum = FindItemInList( Alphas( AlphaNum ), WalkIn );
					NumNameMatches = 0;
					if ( CaseAndWalkInListNum != 0 ) ++NumNameMatches;
					if ( CaseNum != 0 ) ++NumNameMatches;
					if ( WalkInNum != 0 ) ++NumNameMatches;

					if ( NumNameMatches != 1 ) { //name must uniquely point to a list or a single case or walkin or coil
						ErrorsFound = true;
						if ( NumNameMatches == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} else if ( NumNameMatches > 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\",  has a non-unique name that could be either a " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} //num matches = 0 or > 1
					} else if ( CaseAndWalkInListNum != 0 ) { //Name points to a CaseAndWalkInList
						NumCasesMT = CaseAndWalkInList( CaseAndWalkInListNum ).NumCases;
						NumWalkInsMT = CaseAndWalkInList( CaseAndWalkInListNum ).NumWalkIns;
						TransSystem( TransRefrigSysNum ).NumCasesMT = NumCasesMT;
						TransSystem( TransRefrigSysNum ).NumWalkInsMT = NumWalkInsMT;
						if ( NumCasesMT > 0 ) {
							if ( ! allocated( TransSystem( TransRefrigSysNum ).CaseNumMT ) ) TransSystem( TransRefrigSysNum ).CaseNumMT.allocate( NumCasesMT );
							TransSystem( TransRefrigSysNum ).CaseNumMT( {1,NumCasesMT} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CaseItemNum( {1,NumCasesMT} );
						}
						if ( NumWalkInsMT > 0 ) {
							if ( ! allocated( TransSystem( TransRefrigSysNum ).WalkInNumMT ) ) TransSystem( TransRefrigSysNum ).WalkInNumMT.allocate( NumWalkInsMT );
							TransSystem( TransRefrigSysNum ).WalkInNumMT( {1,NumWalkInsMT} ) = CaseAndWalkInList( CaseAndWalkInListNum ).WalkInItemNum( {1,NumWalkInsMT} );
						}
					} else if ( CaseNum != 0 ) { //Name points to a case
						NumCasesMT = 1;
						TransSystem( TransRefrigSysNum ).NumCasesMT = 1;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).CaseNumMT ) ) TransSystem( TransRefrigSysNum ).CaseNumMT.allocate( NumCasesMT );
						TransSystem( TransRefrigSysNum ).CaseNumMT( NumCases ) = CaseNum;
					} else if ( WalkInNum != 0 ) { //Name points to a walkin
						NumWalkInsMT = 1;
						TransSystem( TransRefrigSysNum ).NumWalkInsMT = 1;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).WalkInNumMT ) ) TransSystem( TransRefrigSysNum ).WalkInNumMT.allocate( NumWalkInsMT );
						TransSystem( TransRefrigSysNum ).WalkInNumMT( NumWalkIns ) = WalkInNum;
					} //NumNameMatches /= 1
				} //blank input for cases, walkins, or caseandwalkinlist

				if ( NumCasesMT > 0 ) {
					// Find lowest design evap T
					// Sum rated capacity of all MT cases on system
					for ( CaseIndex = 1; CaseIndex <= NumCasesMT; ++CaseIndex ) {
						//mark all cases on system as used by this system - checking for unused or non-unique cases
						CaseNum = TransSystem( TransRefrigSysNum ).CaseNumMT( CaseIndex );
						++RefrigCase( CaseNum ).NumSysAttach;
						NominalTotalCaseCapMT += RefrigCase( CaseNum ).DesignRatedCap;
						TransSystem( TransRefrigSysNum ).RefInventory += RefrigCase( CaseNum ).DesignRefrigInventory;
						if ( CaseIndex == 1 ) { //look for lowest case design evap T for system
							TransSystem( TransRefrigSysNum ).TEvapDesignMT = RefrigCase( CaseNum ).EvapTempDesign;
						} else {
							TransSystem( TransRefrigSysNum ).TEvapDesignMT = min( RefrigCase( CaseNum ).EvapTempDesign, TransSystem( TransRefrigSysNum ).TEvapDesignMT );
						}
					} //CaseIndex=1,NumCases
				} //NumcasesMT > 0

				if ( NumWalkInsMT > 0 ) {
					for ( WalkInIndex = 1; WalkInIndex <= NumWalkInsMT; ++WalkInIndex ) {
						WalkInID = TransSystem( TransRefrigSysNum ).WalkInNumMT( WalkInIndex );
						//mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
						++WalkIn( WalkInID ).NumSysAttach;
						NominalTotalWalkInCapMT += WalkIn( WalkInID ).DesignRatedCap;
						TransSystem( TransRefrigSysNum ).RefInventory += WalkIn( WalkInID ).DesignRefrigInventory;
						//Defrost capacity is treated differently by compressor racks and detailed systems,
						//  so this value may be adjusted (or warnings issued) after the walkin is assigned
						//  to either the rack or system.
						//for walkins served by detailed system, need capacity for both fluid and electric types.
						if ( WalkIn( WalkInID ).DefrostCapacity <= -98.0 ) {
							// - 99 used as a flag for blank input error message for detailed systems
							ShowSevereError( RoutineName + "Refrigeration:WalkIn=\"" + WalkIn( WalkInID ).Name + "\", Defrost capacity must be greater than or equal to 0 W for electric and hotfluid defrost types" );
							ErrorsFound = true;
						}
						// Find design evaporating temperature for system by getting min design evap for ALL loads
						if ( ( WalkInIndex == 1 ) && ( TransSystem( TransRefrigSysNum ).NumCasesMT == 0 ) ) {
							//note use walk in index, not walkinid here to get
							//first walkin on this suction group/system
							TransSystem( TransRefrigSysNum ).TEvapDesignMT = WalkIn( WalkInID ).TEvapDesign;
						} else {
							TransSystem( TransRefrigSysNum ).TEvapDesignMT = min( WalkIn( WalkInID ).TEvapDesign, TransSystem( TransRefrigSysNum ).TEvapDesignMT );
						}
					} //WalkInIndex=1,NumWalkIns
				} //NumWalkInsMT > 0

				//   Check for Low Temperature Case or Walk-In or CaseAndWalkInList names
				AlphaNum = 4;
				if ( ! lAlphaBlanks( AlphaNum ) ) {

					// Entry for Alphas(AlphaNum) can be either a Case, WalkIn or CaseAndWalkInList name
					CaseAndWalkInListNum = 0;
					CaseNum = 0;
					WalkInNum = 0;
					if ( NumSimulationCaseAndWalkInLists > 0 ) CaseAndWalkInListNum = FindItemInList( Alphas( AlphaNum ), CaseAndWalkInList );
					if ( NumSimulationCases > 0 ) CaseNum = FindItemInList( Alphas( AlphaNum ), RefrigCase );
					if ( NumSimulationWalkIns > 0 ) WalkInNum = FindItemInList( Alphas( AlphaNum ), WalkIn );
					NumNameMatches = 0;
					if ( CaseAndWalkInListNum != 0 ) ++NumNameMatches;
					if ( CaseNum != 0 ) ++NumNameMatches;
					if ( WalkInNum != 0 ) ++NumNameMatches;

					if ( NumNameMatches != 1 ) { //name must uniquely point to a list or a single case or walkin or coil
						ErrorsFound = true;
						if ( NumNameMatches == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} else if ( NumNameMatches > 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\",  has a non-unique name that could be either a " + cAlphaFieldNames( AlphaNum ) + ": " + Alphas( AlphaNum ) );
						} //num matches = 0 or > 1
					} else if ( CaseAndWalkInListNum != 0 ) { //Name points to a CaseAndWalkInList
						NumCasesLT = CaseAndWalkInList( CaseAndWalkInListNum ).NumCases;
						NumWalkInsLT = CaseAndWalkInList( CaseAndWalkInListNum ).NumWalkIns;
						TransSystem( TransRefrigSysNum ).NumCasesLT = NumCasesLT;
						TransSystem( TransRefrigSysNum ).NumWalkInsLT = NumWalkInsLT;
						if ( NumCasesLT > 0 ) {
							if ( ! allocated( TransSystem( TransRefrigSysNum ).CaseNumLT ) ) TransSystem( TransRefrigSysNum ).CaseNumLT.allocate( NumCasesLT );
							TransSystem( TransRefrigSysNum ).CaseNumLT( {1,NumCasesLT} ) = CaseAndWalkInList( CaseAndWalkInListNum ).CaseItemNum( {1,NumCasesLT} );
						}
						if ( NumWalkInsLT > 0 ) {
							if ( ! allocated( TransSystem( TransRefrigSysNum ).WalkInNumLT ) ) TransSystem( TransRefrigSysNum ).WalkInNumLT.allocate( NumWalkInsLT );
							TransSystem( TransRefrigSysNum ).WalkInNumLT( {1,NumWalkInsLT} ) = CaseAndWalkInList( CaseAndWalkInListNum ).WalkInItemNum( {1,NumWalkInsLT} );
						}
					} else if ( CaseNum != 0 ) { //Name points to a case
						NumCasesLT = 1;
						TransSystem( TransRefrigSysNum ).NumCasesLT = 1;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).CaseNumLT ) ) TransSystem( TransRefrigSysNum ).CaseNumLT.allocate( NumCasesLT );
						TransSystem( TransRefrigSysNum ).CaseNumLT( NumCases ) = CaseNum;
					} else if ( WalkInNum != 0 ) { //Name points to a walkin
						NumWalkInsLT = 1;
						TransSystem( TransRefrigSysNum ).NumWalkInsLT = 1;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).WalkInNumLT ) ) TransSystem( TransRefrigSysNum ).WalkInNumLT.allocate( NumWalkInsLT );
						TransSystem( TransRefrigSysNum ).WalkInNumLT( NumWalkIns ) = WalkInNum;
					} //NumNameMatches /= 1
				} //blank input for cases, walkins, or caseandwalkinlist

				if ( NumCasesLT > 0 ) {
					// Find lowest design evap T
					// Sum rated capacity of all LT cases on system
					for ( CaseIndex = 1; CaseIndex <= NumCasesLT; ++CaseIndex ) {
						//mark all cases on system as used by this system - checking for unused or non-unique cases
						CaseNum = TransSystem( TransRefrigSysNum ).CaseNumLT( CaseIndex );
						++RefrigCase( CaseNum ).NumSysAttach;
						NominalTotalCaseCapLT += RefrigCase( CaseNum ).DesignRatedCap;
						TransSystem( TransRefrigSysNum ).RefInventory += RefrigCase( CaseNum ).DesignRefrigInventory;
						if ( CaseIndex == 1 ) { //look for lowest case design evap T for system
							TransSystem( TransRefrigSysNum ).TEvapDesignLT = RefrigCase( CaseNum ).EvapTempDesign;
						} else {
							TransSystem( TransRefrigSysNum ).TEvapDesignLT = min( RefrigCase( CaseNum ).EvapTempDesign, TransSystem( TransRefrigSysNum ).TEvapDesignLT );
						}
					} //CaseIndex=1,NumCases
				} //NumcasesLT > 0

				if ( NumWalkInsLT > 0 ) {
					for ( WalkInIndex = 1; WalkInIndex <= NumWalkInsLT; ++WalkInIndex ) {
						WalkInID = TransSystem( TransRefrigSysNum ).WalkInNumLT( WalkInIndex );
						//mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
						++WalkIn( WalkInID ).NumSysAttach;
						NominalTotalWalkInCapLT += WalkIn( WalkInID ).DesignRatedCap;
						TransSystem( TransRefrigSysNum ).RefInventory += WalkIn( WalkInID ).DesignRefrigInventory;
						//Defrost capacity is treated differently by compressor racks and detailed systems,
						//  so this value may be adjusted (or warnings issued) after the walkin is assigned
						//  to either the rack or system.
						//for walkins served by detailed system, need capacity for both fluid and electric types.
						if ( WalkIn( WalkInID ).DefrostCapacity <= -98.0 ) {
							// - 99 used as a flag for blank input error message for detailed systems
							ShowSevereError( RoutineName + "Refrigeration:WalkIn=\"" + WalkIn( WalkInID ).Name + "\", Defrost capacity must be greater than or equal to 0 W for electric and hotfluid defrost types" );
							ErrorsFound = true;
						}
						// Find design evaporating temperature for system by getting min design evap for ALL loads
						if ( ( WalkInIndex == 1 ) && ( TransSystem( TransRefrigSysNum ).NumCasesLT == 0 ) ) {
							//note use walk in index, not walkinid here to get
							//first walkin on this suction group/system
							TransSystem( TransRefrigSysNum ).TEvapDesignLT = WalkIn( WalkInID ).TEvapDesign;
						} else {
							TransSystem( TransRefrigSysNum ).TEvapDesignLT = min( WalkIn( WalkInID ).TEvapDesign, TransSystem( TransRefrigSysNum ).TEvapDesignLT );
						}
					} //WalkInIndex=1,NumWalkIns
				} //NumWalkInsMT > 0

				NominalTotalCoolingCap = NominalTotalCaseCapMT + NominalTotalCaseCapLT + NominalTotalWalkInCapMT + NominalTotalWalkInCapLT;

				// Read Gas Cooler
				// currently assumes one gas cooler per refrigeration system and but multiple systems allowed per gas cooler
				AlphaNum = 5;
				NumGasCoolers = 1;
				if ( ! allocated( TransSystem( TransRefrigSysNum ).GasCoolerNum ) ) TransSystem( TransRefrigSysNum ).GasCoolerNum.allocate( NumGasCoolers );
				TransSystem( TransRefrigSysNum ).NumGasCoolers = 1;
				//Find gascooler number
				GCNum = FindItemInList( Alphas( AlphaNum ), GasCooler );

				if ( GCNum == 0 ) { //  Invalid Gas Cooler attached to Transcritical Refrigeration System
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", has an invalid " + cAlphaFieldNames( AlphaNum ) + " defined as \"" + Alphas( AlphaNum ) + "\"." );
					ErrorsFound = true;
				} else if ( GCNum != 0 ) { //  Gas Cooler attached to Transcritical Refrigeration System
					TransSystem( TransRefrigSysNum ).GasCoolerNum( NumGasCoolers ) = GCNum;
					TransSystem( TransRefrigSysNum ).NumGasCoolers = 1;
					//Now take care of case where multiple systems share a gas cooler
					++GasCooler( GCNum ).NumSysAttach;
					GasCooler( GCNum ).SysNum( GasCooler( GCNum ).NumSysAttach ) = TransRefrigSysNum;
					TransSystem( TransRefrigSysNum ).RefInventory += GasCooler( GCNum ).RefReceiverInventory + GasCooler( GCNum ).RefPipingInventory + GasCooler( GCNum ).RefOpCharge;
					if ( GasCooler( GCNum ).GasCoolerRejectHeatToZone ) TransSystem( TransRefrigSysNum ).SystemRejectHeatToZone = true;
				}

				// Read High Pressure Compressor
				AlphaNum = 6;
				NumCompressorsSys = 0;
				if ( lAlphaBlanks( AlphaNum ) ) {
					//blank input where must have compressor or compressor list input.
					ShowSevereError( RoutineName + CurrentModuleObject + ' ' + cAlphaFieldNames( AlphaNum ) + "\" : must be input." );
					ErrorsFound = true;
				} else { //     Entry for Alphas(AlphaNum) can be either a compressor name or a compressorlist name
					ListNum = FindItemInList( Alphas( AlphaNum ), CompressorLists );
					CompNum = FindItemInList( Alphas( AlphaNum ), Compressor );
					if ( ( ListNum == 0 ) && ( CompNum == 0 ) ) { // name doesn't match either a compressor or a compressor list
						ShowSevereError( RoutineName + CurrentModuleObject + ", \"" + cAlphaFieldNames( AlphaNum ) + "\", has an invalid or undefined value=\"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					} else if ( ( ListNum != 0 ) && ( CompNum != 0 ) ) { //have compressor list and compressor with same name
						ShowSevereError( RoutineName + CurrentModuleObject + ' ' + cAlphaFieldNames( AlphaNum ) + ", has a non-unique name used for both Compressor and CompressorList name: \"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					} else if ( ListNum != 0 ) {
						NumCompressorsSys = CompressorLists( ListNum ).NumCompressors;
						TransSystem( TransRefrigSysNum ).NumCompressorsHP = NumCompressorsSys;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).CompressorNumHP ) ) TransSystem( TransRefrigSysNum ).CompressorNumHP.allocate( NumCompressorsSys );
						TransSystem( TransRefrigSysNum ).CompressorNumHP( {1,NumCompressorsSys} ) = CompressorLists( ListNum ).CompItemNum( {1,NumCompressorsSys} );
					} else if ( CompNum != 0 ) {
						NumCompressorsSys = 1;
						TransSystem( TransRefrigSysNum ).NumCompressorsHP = 1;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).CompressorNumHP ) ) TransSystem( TransRefrigSysNum ).CompressorNumHP.allocate( NumCompressorsSys );
						TransSystem( TransRefrigSysNum ).CompressorNumHP( NumCompressorsSys ) = CompNum;
					}
					// Sum rated capacity of all HP compressors on system
					NominalTotalCompCapHP = 0.0;
					for ( CompIndex = 1; CompIndex <= NumCompressorsSys; ++CompIndex ) {
						CompNum = TransSystem( TransRefrigSysNum ).CompressorNumHP( CompIndex );

						if ( Compressor( CompNum ).TransFlag ) { //  Calculate nominal capacity of transcritical Compressor
							GCOutletH = GetSupHeatEnthalpyRefrig( TransSystem( TransRefrigSysNum ).RefrigerantName, GasCooler( TransSystem( TransRefrigSysNum ).GasCoolerNum( 1 ) ).RatedOutletT, GasCooler( TransSystem( TransRefrigSysNum ).GasCoolerNum( 1 ) ).RatedOutletP, RefrigIndex, RoutineNameNoColon );
							Compressor( CompNum ).NomCap = CurveValue( Compressor( CompNum ).TransCapacityCurvePtr, TransSystem( TransRefrigSysNum ).TEvapDesignMT, GCOutletH );
							NominalTotalCompCapHP += Compressor( CompNum ).NomCap;
							++Compressor( CompNum ).NumSysAttach;
						} else { //  Subcritical compressor attached to transcritical system - show error
							ShowSevereError( RoutineName + CurrentModuleObject + ", No transcritical CO2 compressors are attached to the transcritical refrigeration system, \"" + TransSystem( TransRefrigSysNum ).Name + "\"." );
							ErrorsFound = true;
						}
					}
				}

				// Read Low Pressure Compressor
				AlphaNum = 7;
				NumCompressorsSys = 0;
				if ( ( lAlphaBlanks( AlphaNum ) ) && ( TransSystem( TransRefrigSysNum ).TransSysType == 2 ) ) {
					// TwoStage system type is specified but low pressure compressor input is blank
					ShowSevereError( RoutineName + CurrentModuleObject + ", The transcritical refrigeration system, \"" + TransSystem( TransRefrigSysNum ).Name + "\", is specified to be \"TwoStage\", however, the \"" + cAlphaFieldNames( AlphaNum ) + "\" is not given." );
					ErrorsFound = true;
				} else if ( ( ! ( lAlphaBlanks( AlphaNum ) ) ) && ( TransSystem( TransRefrigSysNum ).TransSysType == 1 ) ) {
					// SingleStage system type with low pressure compressors specified. Ignore low pressure compressors
					ShowWarningError( RoutineName + CurrentModuleObject + ", The transcritical refrigeration system, \"" + TransSystem( TransRefrigSysNum ).Name + "\", is specified to be \"SingleStage\", however, a\"" + cAlphaFieldNames( AlphaNum ) + "\" was found.  The low pressure compressors will be ignored and will not simulated." );
				} else if ( ( ! ( lAlphaBlanks( AlphaNum ) ) ) && ( TransSystem( TransRefrigSysNum ).TransSysType == 2 ) ) {
					// TwoStage system with low pressure compressors specified
					ListNum = FindItemInList( Alphas( AlphaNum ), CompressorLists );
					CompNum = FindItemInList( Alphas( AlphaNum ), Compressor );
					if ( ( ListNum == 0 ) && ( CompNum == 0 ) ) { // name doesn't match either a compressor or a compressor list
						ShowSevereError( RoutineName + CurrentModuleObject + ", \"" + cAlphaFieldNames( AlphaNum ) + "\", has an invalid or undefined value=\"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					} else if ( ( ListNum != 0 ) && ( CompNum != 0 ) ) { //have compressor list and compressor with same name
						ShowSevereError( RoutineName + CurrentModuleObject + ' ' + cAlphaFieldNames( AlphaNum ) + ", has a non-unique name used for both Compressor and CompressorList name: \"" + Alphas( AlphaNum ) + "\"." );
						ErrorsFound = true;
					} else if ( ListNum != 0 ) {
						NumCompressorsSys = CompressorLists( ListNum ).NumCompressors;
						TransSystem( TransRefrigSysNum ).NumCompressorsLP = NumCompressorsSys;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).CompressorNumLP ) ) TransSystem( TransRefrigSysNum ).CompressorNumLP.allocate( NumCompressorsSys );
						TransSystem( TransRefrigSysNum ).CompressorNumLP( {1,NumCompressorsSys} ) = CompressorLists( ListNum ).CompItemNum( {1,NumCompressorsSys} );
					} else if ( CompNum != 0 ) {
						NumCompressorsSys = 1;
						TransSystem( TransRefrigSysNum ).NumCompressorsLP = 1;
						if ( ! allocated( TransSystem( TransRefrigSysNum ).CompressorNumLP ) ) TransSystem( TransRefrigSysNum ).CompressorNumLP.allocate( NumCompressorsSys );
						TransSystem( TransRefrigSysNum ).CompressorNumLP( NumCompressorsSys ) = CompNum;
					}
					// Sum rated capacity of all LP compressors on system
					NominalTotalCompCapLP = 0.0;
					for ( CompIndex = 1; CompIndex <= NumCompressorsSys; ++CompIndex ) {
						CompNum = TransSystem( TransRefrigSysNum ).CompressorNumLP( CompIndex );
						if ( TransSystem( TransRefrigSysNum ).TransSysType == 2 ) { //  Calculate capacity of LP compressors
							Compressor( CompNum ).NomCap = CurveValue( Compressor( CompNum ).CapacityCurvePtr, TransSystem( TransRefrigSysNum ).TEvapDesignLT, TransSystem( TransRefrigSysNum ).TEvapDesignMT );
							NominalTotalCompCapLP += Compressor( CompNum ).NomCap;
							++Compressor( CompNum ).NumSysAttach;
						}
					}
				}

				// Read Receiver Pressure
				if ( ! lNumericBlanks( 1 ) ) {
					TransSystem( TransRefrigSysNum ).PReceiver = Numbers( 1 );
				} else { // Default value receiver pressure = 4000000 Pa
					TransSystem( TransRefrigSysNum ).PReceiver = 4.0e6;
				}

				// Check receiver temperature against minimum condensing temperature (from gas cooler input) and design evaporator temperatures
				TransSystem( TransRefrigSysNum ).TReceiver = GetSatTemperatureRefrig( TransSystem( TransRefrigSysNum ).RefrigerantName, TransSystem( TransRefrigSysNum ).PReceiver, RefrigIndex, RoutineNameNoColon );
				if ( TransSystem( TransRefrigSysNum ).TReceiver > GasCooler( TransSystem( TransRefrigSysNum ).GasCoolerNum( NumGasCoolers ) ).MinCondTemp ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + ": The receiver temperature (" + RoundSigDigits( TransSystem( TransRefrigSysNum ).TReceiver, 2 ) + "C) is greater than the minimum condensing temperature specified for subcritical operation (" + RoundSigDigits( GasCooler( TransSystem( TransRefrigSysNum ).GasCoolerNum( NumGasCoolers ) ).MinCondTemp, 2 ) + "C)." );
					ShowContinueError( "  The minimum condensing temperature will be set at 5C greater than the receiver temperature." );
					GasCooler( TransSystem( TransRefrigSysNum ).GasCoolerNum( NumGasCoolers ) ).MinCondTemp = TransSystem( TransRefrigSysNum ).TReceiver + 5.0;
				}
				if ( NominalTotalCompCapLP > 0.0 ) {
					if ( TransSystem( TransRefrigSysNum ).TReceiver <= TransSystem( TransRefrigSysNum ).TEvapDesignLT ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + ": The receiver temperature (" + RoundSigDigits( TransSystem( TransRefrigSysNum ).TReceiver, 2 ) + "C) is less than the design evaporator temperature for the low temperature loads (" + RoundSigDigits( TransSystem( TransRefrigSysNum ).TEvapDesignLT, 2 ) + "C)." );
						ShowContinueError( "  Ensure that the receiver temperature is sufficiently greater than the design evaporator temperature for the low temperature loads." );
						ShowContinueError( "  A receiver pressure between 3.0 MPa to 4.0 MPa will typically result in an adequate receiver temperature." );
						ErrorsFound = true;
					}
				}
				if ( NominalTotalCompCapHP > 0.0 ) {
					if ( TransSystem( TransRefrigSysNum ).TReceiver <= TransSystem( TransRefrigSysNum ).TEvapDesignMT ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + ": The receiver temperature (" + RoundSigDigits( TransSystem( TransRefrigSysNum ).TReceiver, 2 ) + "C) is less than the design evaporator temperature for the medium temperature loads (" + RoundSigDigits( TransSystem( TransRefrigSysNum ).TEvapDesignMT, 2 ) + "C)." );
						ShowContinueError( "  Ensure that the receiver temperature is sufficiently greater than the design evaporator temperature for the medium temperature loads." );
						ShowContinueError( "  A receiver pressure between 3.0 MPa to 4.0 MPa will typically result in an adequate receiver temperature." );
						ErrorsFound = true;
					}
				}

				// Read subcooler effectiveness
				if ( ! lNumericBlanks( 2 ) ) {
					TransSystem( TransRefrigSysNum ).SCEffectiveness = Numbers( 2 );
				} else { // Default value effectiveness = 0.4
					TransSystem( TransRefrigSysNum ).PReceiver = 0.4;
				}
				// Check subcooler effectiveness value, must be value between 0 and 1
				if ( ( TransSystem( TransRefrigSysNum ).SCEffectiveness < 0 ) || ( TransSystem( TransRefrigSysNum ).SCEffectiveness > 1 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + ": The value for subcooler effectivness is invalid.  The subcooler effectivenss must be a value greater than or equal to zero and less than or equal to one." );
					ErrorsFound = true;
				}

				//Suction piping heat gain - optional
				//  Input UA and identify the Zone containing the bulk of the suction piping
				//  This Zone ID will be used to determine the temperature used for suction piping heat gain.
				//  The pipe heat gains are also counted as cooling credit for the zone.
				//  Zone Id is only required if Sum UA Suction Piping >0.0
				//  Get the Zone and zone node numbers from the zone name entered by the user
				AlphaNum = 9; // Medium temperature suction piping
				TransSystem( TransRefrigSysNum ).SumUASuctionPipingMT = 0.0;
				if ( ! lNumericBlanks( 3 ) && ! lAlphaBlanks( AlphaNum ) ) {
					TransSystem( TransRefrigSysNum ).SumUASuctionPipingMT = Numbers( 3 );
					TransSystem( TransRefrigSysNum ).SuctionPipeActualZoneNumMT = FindItemInList( Alphas( AlphaNum ), Zone );
					TransSystem( TransRefrigSysNum ).SuctionPipeZoneNodeNumMT = GetSystemNodeNumberForZone( Alphas( AlphaNum ) );
					if ( TransSystem( TransRefrigSysNum ).SuctionPipeZoneNodeNumMT == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", System Node Number not found for " + cAlphaFieldNames( AlphaNum ) + " = \"" + Alphas( AlphaNum ) + "\" even though " + cNumericFieldNames( 3 ) + " is greater than zero." );
						ShowContinueError( "  The medium temperature suction piping heat gain cannot be calculated unless a Zone is defined to deterimine the environmental temperature surrounding the piping." );
						ErrorsFound = true;
					} else {
						RefrigPresentInZone( TransSystem( TransRefrigSysNum ).SuctionPipeActualZoneNumMT ) = true;
					}
				} else if ( ! lNumericBlanks( 3 ) && lAlphaBlanks( AlphaNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " not found even though " + cNumericFieldNames( 3 ) + " is greater than zero." );
					ShowContinueError( "  The medium temperature suction piping heat gain will not be calculated unless a Zone is defined to determine the environmental temperature surrounding the piping." );
				} else if ( lNumericBlanks( 3 ) && ! lAlphaBlanks( AlphaNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " will not be used and suction piping heat gain will not be calculated because " + cNumericFieldNames( 3 ) + " was blank." );
				} // Medium temperature suction piping heat gains

				AlphaNum = 10; // Low temperature suction piping
				TransSystem( TransRefrigSysNum ).SumUASuctionPipingLT = 0.0;
				if ( ! lNumericBlanks( 4 ) && ! lAlphaBlanks( AlphaNum ) ) {
					TransSystem( TransRefrigSysNum ).SumUASuctionPipingLT = Numbers( 4 );
					TransSystem( TransRefrigSysNum ).SuctionPipeActualZoneNumLT = FindItemInList( Alphas( AlphaNum ), Zone );
					TransSystem( TransRefrigSysNum ).SuctionPipeZoneNodeNumLT = GetSystemNodeNumberForZone( Alphas( AlphaNum ) );
					if ( TransSystem( TransRefrigSysNum ).SuctionPipeZoneNodeNumLT == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", System Node Number not found for " + cAlphaFieldNames( AlphaNum ) + " = \"" + Alphas( AlphaNum ) + "\" even though " + cNumericFieldNames( 4 ) + " is greater than zero." );
						ShowContinueError( "  The low temperature suction piping heat gain cannot be calculated unless a Zone is defined to deterimine the environmental temperature surrounding the piping." );
						ErrorsFound = true;
					} else {
						RefrigPresentInZone( TransSystem( TransRefrigSysNum ).SuctionPipeActualZoneNumLT ) = true;
					}
				} else if ( ! lNumericBlanks( 4 ) && lAlphaBlanks( AlphaNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " not found even though " + cNumericFieldNames( 4 ) + " is greater than zero." );
					ShowContinueError( "  The low temperature suction piping heat gain will not be calculated unless a Zone is defined to determine the environmental temperature surrounding the piping." );
				} else if ( lNumericBlanks( 4 ) && ! lAlphaBlanks( AlphaNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\" " + cAlphaFieldNames( AlphaNum ) + " will not be used and suction piping heat gain will not be calculated because " + cNumericFieldNames( 4 ) + " was blank." );
				} // Low temperature suction piping heat gains

				AlphaNum = 11;
				if ( ! lAlphaBlanks( AlphaNum ) ) TransSystem( TransRefrigSysNum ).EndUseSubcategory = Alphas( AlphaNum );

				//Compare the rated capacity of compressor, condenser, and cases.
				// Note, rated capacities can be far off from operating capacities, but rough check.
				NominalCondCap = GasCooler( TransSystem( TransRefrigSysNum ).GasCoolerNum( 1 ) ).RatedCapacity;
				NominalTotalCompCap = NominalTotalCompCapHP + NominalTotalCompCapLP;
				if ( ( NominalTotalCompCap < ( 0.7 * NominalTotalCoolingCap ) ) || ( NominalCondCap < ( 1.3 * NominalTotalCoolingCap ) ) ) {
					ShowWarningError( CurrentModuleObject + "=\"" + TransSystem( TransRefrigSysNum ).Name + "\", You may wish to check the system sizing." );
					ShowContinueError( "Total nominal cooling capacity is " + RoundSigDigits( NominalTotalCoolingCap, 0 ) + "W. Condenser capacity is " + RoundSigDigits( NominalCondCap, 0 ) + "W. Nominal compressor capacity is " + RoundSigDigits( NominalTotalCompCap, 0 ) + "W." );

				}

			} // Transcritical refrigeration systems

		} //(NumTransRefrigSystems > 0)

		DayValues.deallocate();
		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFieldNames.deallocate();
		cNumericFieldNames.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( NumSimulationCases > 0 ) {
			// Find unused and non-unique display case objects to report in eio and err file and sum
			//    all HVAC RA fractions and write error message if greater than 1 for any zone
			for ( ZoneIndex = 1; ZoneIndex <= NumOfZones; ++ZoneIndex ) { //numofzones from dataglobals
				TempRAFraction = CaseRAFraction( ZoneIndex ).TotalCaseRAFraction;
				for ( CaseNum = 1; CaseNum <= NumSimulationCases; ++CaseNum ) {
					// TempRaFraction already includes contributions from ALL cases in zone
					// Want to delete portion from unused cases (numsysattach = 0)that will never be simulated
					if ( RefrigCase( CaseNum ).ActualZoneNum != ZoneIndex || RefrigCase( CaseNum ).NumSysAttach > 0 ) continue;
					TempRAFraction -= RefrigCase( CaseNum ).RAFrac;
				} //NumSimulationCases
				if ( TempRAFraction > 1.0 ) {
					ShowSevereError( RoutineName + ": Refrigeration:Case, Refrigerated case return air fraction for all cases in zone=\"" + CaseRAFraction( ZoneIndex ).ZoneName + "\" is greater than 1.0." );
					//check in comment, can't use "currentModuleObject" because not in get input subroutine where that is known
					ErrorsFound = true;
				}
			} //ZoneIndex=1,NumOfZones

			CaseRAFraction.deallocate(); //only used for input check just completed
			//check for cases not connected to systems and cases connected
			//more than once (twice in a system or to more than one system)

			NumUnusedRefrigCases = 0;
			for ( CaseNum = 1; CaseNum <= NumSimulationCases; ++CaseNum ) {
				if ( RefrigCase( CaseNum ).NumSysAttach == 1 ) continue;
				if ( RefrigCase( CaseNum ).NumSysAttach < 1 ) {
					++NumUnusedRefrigCases;
					if ( DisplayExtraWarnings ) {
						//  individual case names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:Case=\"" + RefrigCase( CaseNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused cases
				} //unused case
				if ( RefrigCase( CaseNum ).NumSysAttach > 1 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + ": Refrigeration:Case=\"" + RefrigCase( CaseNum ).Name + "\", Same refrigerated case name referenced " );
					ShowContinueError( " by more than one refrigeration system and/or compressor rack." );
				} // if looking for same case attached to multiple systems/racks
			} //NumSimulationCases

			if ( ( NumUnusedRefrigCases > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused cases given if DisplayExtraWarnings option not selected
				ShowWarningError( "Refrigeration:Case -> " + RoundSigDigits( NumUnusedRefrigCases ) + " unused refrigerated case(s) found during input processing." );
				ShowContinueError( "  These refrigerated cases are in the input file but are not connected to a " );
				ShowContinueError( "  Refrigeration:CompressorRack, Refrigeration:System, or Refrigeration:SecondarySystem object." );
				ShowContinueError( "  These unused refrigeration cases will not be simulated." );
				ShowContinueError( "  Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedRefrigCases
		} //numsimulation cases > 0

		if ( NumSimulationCompressors > 0 ) {
			//check for compressors not connected to systems and compressors connected more than once
			// (twice in a system or to more than one system)
			NumUnusedCompressors = 0;
			for ( CompNum = 1; CompNum <= NumSimulationCompressors; ++CompNum ) {
				if ( Compressor( CompNum ).NumSysAttach == 1 ) continue;
				if ( Compressor( CompNum ).NumSysAttach < 1 ) {
					++NumUnusedCompressors;
					if ( DisplayExtraWarnings ) {
						//  individual compressor names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:Compressor=\"" + Compressor( CompNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused compressors
				} //unused compressor
				if ( Compressor( CompNum ).NumSysAttach > 1 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + ": Refrigeration:Compressor=\"" + Compressor( CompNum ).Name + "\", Same refrigeration compressor name referenced" );
					ShowContinueError( " by more than one refrigeration system." );
				} // looking for same compressor attached to multiple systems/racks
			} //NumSimulationCompressors

			if ( ( NumUnusedCompressors > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused compressors given if DisplayExtraWarnings option not selected
				ShowWarningError( "Refrigeration:Compressor -> " + RoundSigDigits( NumUnusedCompressors ) + " unused refrigeration compressor(s) found during input processing." );
				ShowContinueError( "  Those refrigeration compressors are in the input file but are not connected to a Refrigeration:System object." );
				ShowContinueError( "   These unused refrigeration compressors will not be simulated." );
				ShowContinueError( "   Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedCompressors
		} //NumSimulationCompressors > 0

		if ( NumSimulationWalkIns > 0 ) {
			//check for refrigeration WalkIns not connected to any systems and
			//  refrigeration WalkIns connected more than once
			NumUnusedWalkIns = 0;
			for ( WalkInNum = 1; WalkInNum <= NumSimulationWalkIns; ++WalkInNum ) {
				if ( WalkIn( WalkInNum ).NumSysAttach == 1 ) continue;
				if ( WalkIn( WalkInNum ).NumSysAttach < 1 ) {
					++NumUnusedWalkIns;
					if ( DisplayExtraWarnings ) {
						//  individual walkin names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:WalkIn=\"" + WalkIn( WalkInNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused WalkIns
				} //unused walkin
				if ( WalkIn( WalkInNum ).NumSysAttach > 1 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + ": Refrigeration:WalkIn=\"" + WalkIn( WalkInNum ).Name + "\", Same Refrigeration WalkIn name referenced" );
					ShowContinueError( " by more than one refrigeration system and/or compressor rack." );
				} // if looking for same walk in attached to multiple systems/racks
			} //NumSimulationWalkIns

			if ( ( NumUnusedWalkIns > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused walkins given if DisplayExtraWarnings option not selected
				ShowWarningError( RoutineName + "Refrigeration:WalkIn -> " + RoundSigDigits( NumUnusedWalkIns ) + " unused refrigeration WalkIns found during input processing." );
				ShowContinueError( "   Those refrigeration WalkIns are in the input file but are not connected to a " );
				ShowContinueError( "   Refrigeration:CompressorRack, Refrigeration:System or Refrigeration:SecondarySystem object." );
				ShowContinueError( "   These unused refrigeration WalkIns will not be simulated." );
				ShowContinueError( "   Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedWalkIns
		} //NumSimulationWalkIns > 0

		if ( NumSimulationRefrigAirChillers > 0 ) {
			//check for air chillers not connected to any systems and
			//  air chillers connected more than once
			NumUnusedCoils = 0;
			for ( CoilNum = 1; CoilNum <= NumSimulationRefrigAirChillers; ++CoilNum ) {
				if ( WarehouseCoil( CoilNum ).NumSysAttach == 1 ) continue;
				if ( WarehouseCoil( CoilNum ).NumSysAttach < 1 ) {
					++NumUnusedWalkIns;
					if ( DisplayExtraWarnings ) {
						//  individual walkin names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:AirChiller=\"" + WarehouseCoil( CoilNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused chillers
				} //unused chiller
				if ( WarehouseCoil( CoilNum ).NumSysAttach > 1 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + ": Refrigeration:AirChiller=\"" + WarehouseCoil( CoilNum ).Name + "\", Same Refrigeration Air Chiller name referenced" );
					ShowContinueError( " by more than one refrigeration system and/or compressor rack." );
				} // if looking for same walk in attached to multiple systems/racks
			} //NumSimulationRefrigAirchillers

			if ( ( NumUnusedCoils > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused air chillers given if DisplayExtraWarnings option not selected
				ShowWarningError( RoutineName + "Refrigeration:AirChiller -> " + RoundSigDigits( NumUnusedCoils ) + " unused refrigeration air chillers found during input processing." );
				ShowContinueError( "   Those refrigeration air chillers are in the input file but are not connected to a " );
				ShowContinueError( "   Refrigeration:CompressorRack, Refrigeration:System or Refrigeration:SecondarySystem object." );
				ShowContinueError( "   These unused refrigeration air chillers will not be simulated." );
				ShowContinueError( "   Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedAirChllerss
		} //NumSimulationAirChillers > 0

		if ( NumSimulationSecondarySystems > 0 ) {
			//check for refrigeration Secondarys not connected to detailed systems and
			//  refrigeration Secondarys connected more than once
			NumUnusedSecondarys = 0;
			for ( SecondaryNum = 1; SecondaryNum <= NumSimulationSecondarySystems; ++SecondaryNum ) {
				if ( Secondary( SecondaryNum ).NumSysAttach == 1 ) continue;
				if ( Secondary( SecondaryNum ).NumSysAttach < 1 ) {
					++NumUnusedSecondarys;
					if ( DisplayExtraWarnings ) {
						//  individual secondary names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:Secondary=\"" + Secondary( SecondaryNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused Secondaries
				} //unused secondary
				if ( Secondary( SecondaryNum ).NumSysAttach > 1 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + ": Refrigeration:Secondary=\"" + Secondary( SecondaryNum ).Name + "\", Same Refrigeration Secondary name referenced" );
					ShowContinueError( "   by more than one refrigeration system" );
				} // looking for same secondary loop attached to multiple systems/racks
			} //NumSimulationSecondarys

			if ( ( NumUnusedSecondarys > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused secondaries given if DisplayExtraWarnings option not selected
				ShowWarningError( RoutineName + "Refrigeration:Secondary -> " + RoundSigDigits( NumUnusedSecondarys ) + " unused refrigeration Secondary Loops found during input processing." );
				ShowContinueError( "  Those refrigeration Secondary Loops are in the input file but are not connected to a refrigeration system." );
				ShowContinueError( "   These unused refrigeration secondaries will not be simulated." );
				ShowContinueError( "   Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedSecondarys
		} //NumSimulationSecondarySystems > 0

		if ( NumRefrigCondensers > 0 ) {
			//Check for presence of shared condensers and for unused condensers
			//     - determines number of loops through refrigeration simulation
			//       because of dependence of performance on total condenser load
			NumSimulationSharedCondensers = 0;
			NumUnusedCondensers = 0;
			for ( CondNum = 1; CondNum <= NumRefrigCondensers; ++CondNum ) {
				if ( Condenser( CondNum ).NumSysAttach == 1 ) continue;
				if ( Condenser( CondNum ).NumSysAttach < 1 ) {
					++NumUnusedCondensers;
					if ( DisplayExtraWarnings ) {
						//  individual condenser names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:Condenser=\"" + Condenser( CondNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused condensers
				} //unused condenser
				if ( Condenser( CondNum ).NumSysAttach > 1 ) {
					++NumSimulationSharedCondensers;
				} // looking for shared condensers
			} //CondNum

			if ( ( NumUnusedCondensers > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused condensers given if DisplayExtraWarnings option not selected
				ShowWarningError( RoutineName + "Refrigeration condenser -> " + RoundSigDigits( NumUnusedCondensers ) + " unused refrigeration condensers found during input processing." );
				ShowContinueError( "  Those refrigeration condensers are in the input file but are not connected to a refrigeration system." );
				ShowContinueError( "   These unused refrigeration condensers will not be simulated." );
				ShowContinueError( "   Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedCondensers and displayextra warnings
		} //NumRefrigCondensers > 0

		if ( NumSimulationGasCooler > 0 ) {
			//Check for presence of shared gas coolers and for unused gas coolers
			NumSimulationSharedGasCoolers = 0;
			NumUnusedGasCoolers = 0;
			for ( GCNum = 1; GCNum <= NumSimulationGasCooler; ++GCNum ) {
				if ( GasCooler( GCNum ).NumSysAttach == 1 ) continue;
				if ( GasCooler( GCNum ).NumSysAttach < 1 ) {
					++NumUnusedGasCoolers;
					if ( DisplayExtraWarnings ) {
						//  individual gas cooler names listed if DisplayExtraWarnings option selected
						ShowWarningError( RoutineName + ": Refrigeration:GasCooler=\"" + GasCooler( GCNum ).Name + "\" unused. " );
					} //display extra warnings - give a list of unused gas coolers
				} //unused gas cooler
				if ( GasCooler( GCNum ).NumSysAttach > 1 ) {
					++NumSimulationSharedGasCoolers;
				} // looking for shared gas coolers
			} //GCNum

			if ( ( NumUnusedGasCoolers > 0 ) && ( ! DisplayExtraWarnings ) ) {
				//  write to error file,
				//  summary number of unused gas coolers given if DisplayExtraWarnings option not selected
				ShowWarningError( RoutineName + "Refrigeration gas cooler -> " + RoundSigDigits( NumUnusedGasCoolers ) + " unused refrigeration gas cooler(s) found during input processing." );
				ShowContinueError( "  These refrigeration gas coolers are in the input file but are not connected to a refrigeration system." );
				ShowContinueError( "  These unused refrigeration gas coolers will not be simulated." );
				ShowContinueError( "  Use Output:Diagnostics,DisplayUnusedObjects; to see them. " );
			} //NumUnusedGasCoolers and displayextra warnings
		} //NumSimulationGasCooler > 0

		//echo input to eio file.
		ReportRefrigerationComponents();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + " Previous errors cause program termination" );
		}

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SetupReportInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Hudson, ORNL July 2007, Stovall, ORNL, 2008 and 09
		//       MODIFIED       Fricke, ORNL, Fall 2011, added transcritical CO2 refrigeration system variables
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set up the report variables.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataHeatBalance::IntGainTypeOf_RefrigerationCase;
		using DataHeatBalance::IntGainTypeOf_RefrigerationSystemSuctionPipe;
		using DataHeatBalance::IntGainTypeOf_RefrigerationCompressorRack;
		using DataHeatBalance::IntGainTypeOf_RefrigerationSystemAirCooledCondenser;
		using DataHeatBalance::IntGainTypeOf_RefrigerationSecondaryReceiver;
		using DataHeatBalance::IntGainTypeOf_RefrigerationSecondaryPipe;
		using DataHeatBalance::IntGainTypeOf_RefrigerationWalkIn;
		using DataHeatBalance::IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler;
		using DataHeatBalance::IntGainTypeOf_RefrigerationTransSysSuctionPipeMT;
		using DataHeatBalance::IntGainTypeOf_RefrigerationTransSysSuctionPipeLT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// LOGICAL, SAVE :: MyBeginEnvrnFlag = .TRUE.
		// INTEGER       :: SystemID
		static int CaseNum( 0 );
		static int CoilNum( 0 );
		static int SecondNum( 0 );
		static int WalkInNum( 0 );
		static int RackNum( 0 );
		static int RefrigSysNum( 0 );
		static int CompNum( 0 );
		static int CompIndex( 0 );
		static int CondNum( 0 );
		static int GCNum( 0 );
		static int SubcoolNum( 0 );
		static int ZoneID( 0 );
		static std::string Walkin_and_zone_name; // concat name for walk-in/zone credit reporting

		if ( NumSimulationCases > 0 ) {
			// Setup Report Variables for simulated Refrigerated Case (do not report unused cases)
			// CurrentModuleObject='Refrigeration:Case'
			for ( CaseNum = 1; CaseNum <= NumSimulationCases; ++CaseNum ) {
				if ( RefrigCase( CaseNum ).NumSysAttach == 1 ) {
					SetupOutputVariable( "Refrigeration Case Evaporator Total Cooling Rate [W]", RefrigCase( CaseNum ).TotalCoolingLoad, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Evaporator Total Cooling Energy [J]", RefrigCase( CaseNum ).TotalCoolingEnergy, "Zone", "Sum", RefrigCase( CaseNum ).Name, _, "ENERGYTRANSFER", "REFRIGERATION", _, "Building", RefrigCase( CaseNum ).ZoneName );
					SetupOutputVariable( "Refrigeration Case Evaporator Sensible Cooling Rate [W]", RefrigCase( CaseNum ).SensCoolingEnergyRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Evaporator Sensible Cooling Energy [J]", RefrigCase( CaseNum ).SensCoolingEnergy, "Zone", "Sum", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Evaporator Latent Cooling Rate [W]", RefrigCase( CaseNum ).LatCoolingEnergyRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Evaporator Latent Cooling Energy [J]", RefrigCase( CaseNum ).LatCoolingEnergy, "Zone", "Sum", RefrigCase( CaseNum ).Name );

					SetupOutputVariable( "Refrigeration Case Zone Sensible Cooling Rate [W]", RefrigCase( CaseNum ).SensZoneCreditCoolRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Zone Sensible Cooling Energy [J]", RefrigCase( CaseNum ).SensZoneCreditCool, "Zone", "Sum", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Zone Sensible Heating Rate [W]", RefrigCase( CaseNum ).SensZoneCreditHeatRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Zone Sensible Heating Energy [J]", RefrigCase( CaseNum ).SensZoneCreditHeat, "Zone", "Sum", RefrigCase( CaseNum ).Name );

					SetupOutputVariable( "Refrigeration Case Zone Latent Rate [W]", RefrigCase( CaseNum ).LatZoneCreditRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Zone Latent Energy [J]", RefrigCase( CaseNum ).LatZoneCredit, "Zone", "Sum", RefrigCase( CaseNum ).Name );

					SetupOutputVariable( "Refrigeration Case Return Air Sensible Cooling Rate [W]", RefrigCase( CaseNum ).SensHVACCreditCoolRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Return Air Sensible Cooling Energy [J]", RefrigCase( CaseNum ).SensHVACCreditCool, "Zone", "Sum", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Return Air Sensible Heating Rate [W]", RefrigCase( CaseNum ).SensHVACCreditHeatRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Return Air Sensible Heating Energy [J]", RefrigCase( CaseNum ).SensHVACCreditHeat, "Zone", "Sum", RefrigCase( CaseNum ).Name );

					SetupOutputVariable( "Refrigeration Case Return Air Latent Rate [W]", RefrigCase( CaseNum ).LatHVACCreditRate, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Return Air Latent Energy [J]", RefrigCase( CaseNum ).LatHVACCredit, "Zone", "Sum", RefrigCase( CaseNum ).Name );

					SetupOutputVariable( "Refrigeration Case Evaporator Fan Electric Power [W]", RefrigCase( CaseNum ).ElecFanPower, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Evaporator Fan Electric Energy [J]", RefrigCase( CaseNum ).ElecFanConsumption, "Zone", "Sum", RefrigCase( CaseNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building", RefrigCase( CaseNum ).ZoneName );
					SetupOutputVariable( "Refrigeration Case Lighting Electric Power [W]", RefrigCase( CaseNum ).ElecLightingPower, "Zone", "Average", RefrigCase( CaseNum ).Name );
					SetupOutputVariable( "Refrigeration Case Lighting Electric Energy [J]", RefrigCase( CaseNum ).ElecLightingConsumption, "Zone", "Sum", RefrigCase( CaseNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building", RefrigCase( CaseNum ).ZoneName );

					// Report defrost energy curve value only for cases having electric or hot-gas defrost with temperature termination
					if ( RefrigCase( CaseNum ).DefrostType == DefElectricTerm || RefrigCase( CaseNum ).DefrostType == DefHotFluidTerm ) {
						SetupOutputVariable( "Refrigeration Case Defrost Energy Correction Curve Value []", RefrigCase( CaseNum ).DefEnergyCurveValue, "Zone", "Average", RefrigCase( CaseNum ).Name );
					}

					SetupOutputVariable( "Refrigeration Case Latent Credit Curve Value []", RefrigCase( CaseNum ).LatEnergyCurveValue, "Zone", "Average", RefrigCase( CaseNum ).Name );

					// Report only for cases having anti-sweat heaters
					if ( RefrigCase( CaseNum ).AntiSweatControlType > ASNone ) {
						SetupOutputVariable( "Refrigeration Case Anti Sweat Electric Power [W]", RefrigCase( CaseNum ).ElecAntiSweatPower, "Zone", "Average", RefrigCase( CaseNum ).Name );
						SetupOutputVariable( "Refrigeration Case Anti Sweat Electric Energy [J]", RefrigCase( CaseNum ).ElecAntiSweatConsumption, "Zone", "Sum", RefrigCase( CaseNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building", RefrigCase( CaseNum ).ZoneName );
					}

					// Report only for cases using electric defrost

					if ( RefrigCase( CaseNum ).DefrostType == DefElectric || RefrigCase( CaseNum ).DefrostType == DefElectricOnDemand || RefrigCase( CaseNum ).DefrostType == DefElectricTerm ) {
						SetupOutputVariable( "Refrigeration Case Defrost Electric Power [W]", RefrigCase( CaseNum ).ElecDefrostPower, "Zone", "Average", RefrigCase( CaseNum ).Name );
						SetupOutputVariable( "Refrigeration Case Defrost Electric Energy [J]", RefrigCase( CaseNum ).ElecDefrostConsumption, "Zone", "Sum", RefrigCase( CaseNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building", RefrigCase( CaseNum ).ZoneName );
					}

					//register refrigeration case credits as internal gains
					if ( RefrigCase( CaseNum ).ActualZoneNum > 0 ) {
						SetupZoneInternalGain( RefrigCase( CaseNum ).ActualZoneNum, "Refrigeration:Case", RefrigCase( CaseNum ).Name, IntGainTypeOf_RefrigerationCase, RefrigCase( CaseNum ).SensZoneCreditRate, RefrigCase( CaseNum ).SensHVACCreditRate, _, RefrigCase( CaseNum ).LatZoneCreditRate, RefrigCase( CaseNum ).LatHVACCreditRate );
					}
				} //END IF (.NOT. RefrigCase(CaseNum)%unusedCase)
			}
		} //NumSimulationCases > 0

		if ( NumSimulationWalkIns > 0 ) {
			// Setup Report Variables for simulated  Walk In (do not report unused WalkIns)
			// CurrentModuleObject='Refrigeration:WalkIn'
			for ( WalkInNum = 1; WalkInNum <= NumSimulationWalkIns; ++WalkInNum ) {
				if ( WalkIn( WalkInNum ).NumSysAttach == 1 ) { //ensure no unuseds reported
					SetupOutputVariable( "Refrigeration Walk In Evaporator Total Cooling Rate [W]", WalkIn( WalkInNum ).TotalCoolingLoad, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Evaporator Total Cooling Energy [J]", WalkIn( WalkInNum ).TotalCoolingEnergy, "Zone", "Sum", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Evaporator Sensible Cooling Rate [W]", WalkIn( WalkInNum ).TotSensCoolingEnergyRate, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Evaporator Sensible Cooling Energy [J]", WalkIn( WalkInNum ).TotSensCoolingEnergy, "Zone", "Sum", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Evaporator Latent Cooling Rate [W]", WalkIn( WalkInNum ).TotLatCoolingEnergyRate, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Evaporator Latent Cooling Energy [J]", WalkIn( WalkInNum ).TotLatCoolingEnergy, "Zone", "Sum", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Ancillary Electric Power [W]", WalkIn( WalkInNum ).TotalElecPower, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Ancillary Electric Energy [J]", WalkIn( WalkInNum ).TotalElecConsumption, "Zone", "Sum", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Fan Electric Power [W]", WalkIn( WalkInNum ).ElecFanPower, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Fan Electric Energy [J]", WalkIn( WalkInNum ).ElecFanConsumption, "Zone", "Sum", WalkIn( WalkInNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );
					SetupOutputVariable( "Refrigeration Walk In Lighting Electric Power [W]", WalkIn( WalkInNum ).ElecLightingPower, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Lighting Electric Energy [J]", WalkIn( WalkInNum ).ElecLightingConsumption, "Zone", "Sum", WalkIn( WalkInNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );
					SetupOutputVariable( "Refrigeration Walk In Heater Electric Power [W]", WalkIn( WalkInNum ).ElecHeaterPower, "Zone", "Average", WalkIn( WalkInNum ).Name );
					SetupOutputVariable( "Refrigeration Walk In Heater Electric Energy [J]", WalkIn( WalkInNum ).ElecHeaterConsumption, "Zone", "Sum", WalkIn( WalkInNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );

					// Report only for WalkIns using electric defrost
					if ( WalkIn( WalkInNum ).DefrostType == WalkInDefrostElec ) {
						SetupOutputVariable( "Refrigeration Walk In Defrost Electric Power [W]", WalkIn( WalkInNum ).ElecDefrostPower, "Zone", "Average", WalkIn( WalkInNum ).Name );
						SetupOutputVariable( "Refrigeration Walk In Defrost Electric Energy [J]", WalkIn( WalkInNum ).ElecDefrostConsumption, "Zone", "Sum", WalkIn( WalkInNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );
					}

					// Report walkin variables that are specified for each zone exposed to the walkin
					// For "IDOut" variable in SetupOutputVariable, need to create a single name that includes
					//    both the walk-in name and the zone name - see "Walkin_and_zone_name" concatination
					//    This new variable name is important if using an rvi file!
					for ( ZoneID = 1; ZoneID <= WalkIn( WalkInNum ).NumZones; ++ZoneID ) {

						Walkin_and_zone_name = WalkIn( WalkInNum ).Name + "InZone" + WalkIn( WalkInNum ).ZoneName( ZoneID );

						SetupOutputVariable( "Refrigeration Walk In Zone Sensible Cooling Rate [W]", WalkIn( WalkInNum ).SensZoneCreditCoolRate( ZoneID ), "Zone", "Average", Walkin_and_zone_name );
						SetupOutputVariable( "Refrigeration Walk In Zone Sensible Cooling Energy [J]", WalkIn( WalkInNum ).SensZoneCreditCool( ZoneID ), "Zone", "Sum", Walkin_and_zone_name );
						SetupOutputVariable( "Refrigeration Walk In Zone Sensible Heating Rate [W]", WalkIn( WalkInNum ).SensZoneCreditHeatRate( ZoneID ), "Zone", "Average", Walkin_and_zone_name );
						SetupOutputVariable( "Refrigeration Walk In Zone Sensible Heating Energy [J]", WalkIn( WalkInNum ).SensZoneCreditHeat( ZoneID ), "Zone", "Sum", Walkin_and_zone_name );
						SetupOutputVariable( "Refrigeration Walk In Zone Latent Rate [W]", WalkIn( WalkInNum ).LatZoneCreditRate( ZoneID ), "Zone", "Average", Walkin_and_zone_name );
						SetupOutputVariable( "Refrigeration Walk In Zone Latent Energy [J]", WalkIn( WalkInNum ).LatZoneCredit( ZoneID ), "Zone", "Sum", Walkin_and_zone_name );

						if ( WalkIn( WalkInNum ).ZoneNum( ZoneID ) > 0 ) SetupZoneInternalGain( WalkIn( WalkInNum ).ZoneNum( ZoneID ), "Refrigeration:WalkIn", Walkin_and_zone_name, IntGainTypeOf_RefrigerationWalkIn, WalkIn( WalkInNum ).SensZoneCreditRate( ZoneID ), _, _, WalkIn( WalkInNum ).LatZoneCreditRate( ZoneID ) );

					} // ZoneID
				} //(.NOT.  WalkIn( WalkInNum)%unusedWalkIn)
			} // NumSimulationWalkIns
		} // NumSimulationWalkIns > 0

		if ( NumSimulationRefrigAirChillers > 0 ) {
			// Setup Report Variables for simulated Warehouse coils (do not report unused warehouse coils)
			// CurrentModuleObject='Refrigeration:AirChiller'
			for ( CoilNum = 1; CoilNum <= NumSimulationRefrigAirChillers; ++CoilNum ) {
				if ( WarehouseCoil( CoilNum ).NumSysAttach == 1 ) { //ensure no unuseds reported
					SetupOutputVariable( "Refrigeration Zone Air Chiller Total Cooling Rate [W]", WarehouseCoil( CoilNum ).TotalCoolingLoad, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Total Cooling Energy [J]", WarehouseCoil( CoilNum ).TotalCoolingEnergy, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Cooling Rate [W]", WarehouseCoil( CoilNum ).SensCoolingEnergyRate, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Cooling Energy [J]", WarehouseCoil( CoilNum ).SensCoolingEnergy, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Latent Cooling Rate [W]", WarehouseCoil( CoilNum ).LatCreditRate, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Latent Cooling Energy [J]", WarehouseCoil( CoilNum ).LatCreditEnergy, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Water Removed Mass Flow Rate [kg/s]", WarehouseCoil( CoilNum ).LatKgPerS_ToZone, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Total Electric Power [W]", WarehouseCoil( CoilNum ).TotalElecPower, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Total Electric Energy [J]", WarehouseCoil( CoilNum ).TotalElecConsumption, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name ); //components are metered seperately
					SetupOutputVariable( "Refrigeration Zone Air Chiller Fan Electric Power [W]", WarehouseCoil( CoilNum ).ElecFanPower, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Fan Electric Energy [J]", WarehouseCoil( CoilNum ).ElecFanConsumption, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Heater Electric Power [W]", WarehouseCoil( CoilNum ).ElecHeaterPower, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Heater Electric Energy [J]", WarehouseCoil( CoilNum ).ElecHeaterConsumption, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Heat Ratio []", WarehouseCoil( CoilNum ).SensHeatRatio, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Frost Accumulation Mass [Kg]", WarehouseCoil( CoilNum ).KgFrost, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Zone Total Cooling Rate [W]", WarehouseCoil( CoilNum ).ReportTotalCoolCreditRate, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Zone Total Cooling Energy [J]", WarehouseCoil( CoilNum ).ReportTotalCoolCreditEnergy, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Zone Sensible Cooling Rate [W]", WarehouseCoil( CoilNum ).ReportSensCoolCreditRate, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Zone Sensible Cooling Energy [J]", WarehouseCoil( CoilNum ).ReportSensCoolCreditEnergy, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Zone Heating Rate [W]", WarehouseCoil( CoilNum ).ReportHeatingCreditRate, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Zone Heating Energy [J]", WarehouseCoil( CoilNum ).ReportHeatingCreditEnergy, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name );

					// Report only for Warehouse coils using electric defrost
					if ( WarehouseCoil( CoilNum ).DefrostType == DefrostElec ) {
						SetupOutputVariable( "Refrigeration Zone Air Chiller Defrost Electric Power [W]", WarehouseCoil( CoilNum ).ElecDefrostPower, "HVAC", "Average", WarehouseCoil( CoilNum ).Name );
						SetupOutputVariable( "Refrigeration Zone Air Chiller Defrost Electric Energy [J]", WarehouseCoil( CoilNum ).ElecDefrostConsumption, "HVAC", "Sum", WarehouseCoil( CoilNum ).Name, _, "ELECTRICITY", "REFRIGERATION", "General", "Building" );
					} // electric defrost coil
				} //(.NOT.  WarehouseCoil(CoilNum)%unusedWarehouseCoil)
			} // NumSimulationWarehouseCoils
		} // NumSimulationRefrigAirChillers > 0

		//There are no report variables for Chiller sets because they are
		// used to pass the demand to the coils, but are NOT used to provide the
		// cooling energy to the zone (because more than one set may cool a zone)

		// Report sum of all refrigeration interactions with each zone

		for ( ZoneID = 1; ZoneID <= NumOfZones; ++ZoneID ) {
			if ( RefrigPresentInZone( ZoneID ) ) {
				if ( HaveCasesOrWalkins ) {
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Sensible Cooling Rate [W]", RefrigCaseCredit( ZoneID ).SenCaseCreditToZone, "Zone", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Sensible Cooling Energy [J]", CaseWIZoneReport( ZoneID ).SenCaseCreditToZoneEnergy, "Zone", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Heating Rate [W]", CaseWIZoneReport( ZoneID ).HeatingToZoneRate, "Zone", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Heating Energy [J]", CaseWIZoneReport( ZoneID ).HeatingToZoneEnergy, "Zone", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Sensible Cooling Rate [W]", CaseWIZoneReport( ZoneID ).SenCoolingToZoneRate, "Zone", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Sensible Cooling Energy [J]", CaseWIZoneReport( ZoneID ).SenCoolingToZoneEnergy, "Zone", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Latent Cooling Rate [W]", CaseWIZoneReport( ZoneID ).LatCoolingToZoneRate, "Zone", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Latent Cooling Energy [J]", CaseWIZoneReport( ZoneID ).LatCoolingToZoneEnergy, "Zone", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Cooling Rate [W]", CaseWIZoneReport( ZoneID ).TotCoolingToZoneRate, "Zone", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Cooling Energy [J]", CaseWIZoneReport( ZoneID ).TotCoolingToZoneEnergy, "Zone", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Heat Transfer Rate [W]", CaseWIZoneReport( ZoneID ).TotHtXferToZoneRate, "Zone", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Case and Walk In Total Heat Transfer Energy [J]", CaseWIZoneReport( ZoneID ).TotHtXferToZoneEnergy, "Zone", "Sum", Zone( ZoneID ).Name );
				} //HaveCasesOrWalkIns

				if ( HaveChillers ) {
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Heat Transfer Rate [W]", CoilSysCredit( ZoneID ).SenCreditToZoneRate, "HVAC", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Heat Transfer Energy [J]", CoilSysCredit( ZoneID ).SenCreditToZoneEnergy, "HVAC", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Cooling Rate [W]", CoilSysCredit( ZoneID ).ReportSenCoolingToZoneRate, "HVAC", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Sensible Cooling Energy [J]", CoilSysCredit( ZoneID ).ReportSenCoolingToZoneEnergy, "HVAC", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Latent Cooling Rate [W]", CoilSysCredit( ZoneID ).ReportLatCreditToZoneRate, "HVAC", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Latent Cooling Energy [J]", CoilSysCredit( ZoneID ).ReportLatCreditToZoneEnergy, "HVAC", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Water Removed Mass Flow Rate [Kg/s]", CoilSysCredit( ZoneID ).ReportH20RemovedKgPerS_FromZoneRate, "HVAC", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Total Cooling Rate [W]", CoilSysCredit( ZoneID ).ReportTotCoolingToZoneRate, "HVAC", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Total Cooling Energy [J]", CoilSysCredit( ZoneID ).ReportTotCoolingToZoneEnergy, "HVAC", "Sum", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Heating Rate [W]", CoilSysCredit( ZoneID ).ReportHeatingToZoneRate, "HVAC", "Average", Zone( ZoneID ).Name );
					SetupOutputVariable( "Refrigeration Zone Air Chiller Heating Energy [J]", CoilSysCredit( ZoneID ).ReportHeatingToZoneEnergy, "HVAC", "Sum", Zone( ZoneID ).Name );
				} //HaveChillers
			} //RefrigPresentInZone(ZoneID)
		} // ZoneID

		if ( NumSimulationSecondarySystems > 0 ) {
			// CurrentModuleObject='Refrigeration:SecondarySystem'
			for ( SecondNum = 1; SecondNum <= NumSimulationSecondarySystems; ++SecondNum ) {
				if ( Secondary( SecondNum ).NumSysAttach == 1 ) {
					if ( Secondary( SecondNum ).CoilFlag ) { //secondary system serves chillers and is solved on HVAC time step
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Pump Electric Power [W]", Secondary( SecondNum ).PumpPowerTotal, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Pump Electric Energy [J]", Secondary( SecondNum ).PumpElecEnergyTotal, "HVAC", "Sum", Secondary( SecondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Secondary( SecondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Load Heat Transfer Rate [W]", Secondary( SecondNum ).TotalRefrigLoad, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Load Heat Transfer Energy [J]", Secondary( SecondNum ).TotalRefrigEnergy, "HVAC", "Sum", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Total Heat Transfer Rate [W]", Secondary( SecondNum ).TotalCoolingLoad, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Total Heat Transfer Energy [J]", Secondary( SecondNum ).TotalCoolingEnergy, "HVAC", "Sum", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Estimated Refrigerant Inventory Mass [kg]", Secondary( SecondNum ).RefInventory, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Volume Flow Rate [m3/s]", Secondary( SecondNum ).FlowVolActual, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Rate [W]", Secondary( SecondNum ).DistPipeHeatGain, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Energy [J]", Secondary( SecondNum ).DistPipeHeatGainEnergy, "HVAC", "Sum", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Rate [W]", Secondary( SecondNum ).ReceiverHeatGain, "HVAC", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Energy [J]", Secondary( SecondNum ).ReceiverHeatGainEnergy, "HVAC", "Sum", Secondary( SecondNum ).Name );
					} else { //Secondary loop serves cases and walk-ins on zone(load) time step
						SetupOutputVariable( "Refrigeration Secondary Loop Pump Electric Power [W]", Secondary( SecondNum ).PumpPowerTotal, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Pump Electric Energy [J]", Secondary( SecondNum ).PumpElecEnergyTotal, "Zone", "Sum", Secondary( SecondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Secondary( SecondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Secondary Loop Load Heat Transfer Rate [W]", Secondary( SecondNum ).TotalRefrigLoad, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Load Heat Transfer Energy [J]", Secondary( SecondNum ).TotalRefrigEnergy, "Zone", "Sum", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Total Heat Transfer Rate [W]", Secondary( SecondNum ).TotalCoolingLoad, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Total Heat Transfer Energy [J]", Secondary( SecondNum ).TotalCoolingEnergy, "Zone", "Sum", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Estimated Refrigerant Inventory Mass [kg]", Secondary( SecondNum ).RefInventory, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Volume Flow Rate [m3/s]", Secondary( SecondNum ).FlowVolActual, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Pipe Heat Gain Rate [W]", Secondary( SecondNum ).DistPipeHeatGain, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Pipe Heat Gain Energy [J]", Secondary( SecondNum ).DistPipeHeatGainEnergy, "Zone", "Sum", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Receiver Heat Gain Rate [W]", Secondary( SecondNum ).ReceiverHeatGain, "Zone", "Average", Secondary( SecondNum ).Name );
						SetupOutputVariable( "Refrigeration Secondary Loop Receiver Heat Gain Energy [J]", Secondary( SecondNum ).ReceiverHeatGainEnergy, "Zone", "Sum", Secondary( SecondNum ).Name );
					} //NOT coilflag so on Zone timestep
					if ( Secondary( SecondNum ).ReceiverZoneNum > 0 ) {
						SetupZoneInternalGain( Secondary( SecondNum ).ReceiverZoneNum, "Refrigeration:SecondarySystem:Receiver", Secondary( SecondNum ).Name, IntGainTypeOf_RefrigerationSecondaryReceiver, Secondary( SecondNum ).ReceiverZoneHeatGain );
					}
					if ( Secondary( SecondNum ).DistPipeZoneNum > 0 ) {
						SetupZoneInternalGain( Secondary( SecondNum ).DistPipeZoneNum, "Refrigeration:SecondarySystem:Pipe", Secondary( SecondNum ).Name, IntGainTypeOf_RefrigerationSecondaryPipe, Secondary( SecondNum ).DistPipeZoneHeatGain );
					}
				} // not an unused
			} // NumSimulationSecondarySystems
		} // NumSimulationSecondarySystems > 0

		// Setup Report Variables for Refrigeration Compressor Rack
		if ( NumRefrigeratedRacks > 0 ) {
			// CurrentModuleObject='Refrigeration:CompressorRack'
			for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {
				if ( RefrigRack( RackNum ).CoilFlag ) { //rack serves chillers and is solved on HVAC time step
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Electric Power [W]", RefrigRack( RackNum ).RackCompressorPower, "HVAC", "Average", RefrigRack( RackNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Electric Energy [J]", RefrigRack( RackNum ).RackElecConsumption, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Power [W]", RefrigRack( RackNum ).ActualCondenserFanPower, "HVAC", "Average", RefrigRack( RackNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Energy [J]", RefrigRack( RackNum ).CondenserFanConsumption, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Total Heat Transfer Rate [W]", RefrigRack( RackNum ).RackCapacity, "HVAC", "Average", RefrigRack( RackNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Total Heat Transfer Energy [J]", RefrigRack( RackNum ).RackCoolingEnergy, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "ENERGYTRANSFER", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack COP [W/W]", RefrigRack( RackNum ).RackCompressorCOP, "HVAC", "Average", RefrigRack( RackNum ).Name );

					if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap ) {
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Power [W]", RefrigRack( RackNum ).ActualEvapPumpPower, "HVAC", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Energy [J]", RefrigRack( RackNum ).EvapPumpConsumption, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]", RefrigRack( RackNum ).BasinHeaterPower, "HVAC", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]", RefrigRack( RackNum ).BasinHeaterConsumption, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume Flow Rate [m3/s]", RefrigRack( RackNum ).EvapWaterConsumpRate, "HVAC", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume [m3]", RefrigRack( RackNum ).EvapWaterConsumption, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "Water", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					} //Evap condenser

					if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Rate [W]", RefrigRack( RackNum ).SensZoneCreditHeatRate, "HVAC", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Energy [J]", RefrigRack( RackNum ).SensZoneCreditHeat, "HVAC", "Sum", RefrigRack( RackNum ).Name );

						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Rate [W]", RefrigRack( RackNum ).SensHVACCreditHeatRate, "HVAC", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Energy [J]", RefrigRack( RackNum ).SensHVACCreditHeat, "HVAC", "Sum", RefrigRack( RackNum ).Name );

						SetupZoneInternalGain( RefrigCase( RefrigRack( RackNum ).CaseNum( 1 ) ).ActualZoneNum, "Refrigeration:CompressorRack", RefrigRack( RackNum ).Name, IntGainTypeOf_RefrigerationCompressorRack, RefrigRack( RackNum ).SensZoneCreditHeatRate, RefrigRack( RackNum ).SensHVACCreditHeatRate );

					} //LocationZone

				} else { // Rack serves cases and walkins on zone (load) time step

					SetupOutputVariable( "Refrigeration Compressor Rack Electric Power [W]", RefrigRack( RackNum ).RackCompressorPower, "Zone", "Average", RefrigRack( RackNum ).Name );
					SetupOutputVariable( "Refrigeration Compressor Rack Electric Energy [J]", RefrigRack( RackNum ).RackElecConsumption, "Zone", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					SetupOutputVariable( "Refrigeration Compressor Rack Condenser Fan Electric Power [W]", RefrigRack( RackNum ).ActualCondenserFanPower, "Zone", "Average", RefrigRack( RackNum ).Name );
					SetupOutputVariable( "Refrigeration Compressor Rack Condenser Fan Electric Energy [J]", RefrigRack( RackNum ).CondenserFanConsumption, "Zone", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					SetupOutputVariable( "Refrigeration Compressor Rack Total Heat Transfer Rate [W]", RefrigRack( RackNum ).RackCapacity, "Zone", "Average", RefrigRack( RackNum ).Name );
					SetupOutputVariable( "Refrigeration Compressor Rack Total Heat Transfer Energy [J]", RefrigRack( RackNum ).RackCoolingEnergy, "Zone", "Sum", RefrigRack( RackNum ).Name, _, "ENERGYTRANSFER", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					SetupOutputVariable( "Refrigeration Compressor Rack COP [W/W]", RefrigRack( RackNum ).RackCompressorCOP, "Zone", "Average", RefrigRack( RackNum ).Name );

					if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap ) {
						SetupOutputVariable( "Refrigeration Compressor Rack Evaporative Condenser Pump Electric Power [W]", RefrigRack( RackNum ).ActualEvapPumpPower, "Zone", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Rack Evaporative Condenser Pump Electric Energy [J]", RefrigRack( RackNum ).EvapPumpConsumption, "Zone", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]", RefrigRack( RackNum ).BasinHeaterPower, "Zone", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]", RefrigRack( RackNum ).BasinHeaterConsumption, "Zone", "Sum", RefrigRack( RackNum ).Name, _, "ELECTRICITY", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Compressor Rack Evaporative Condenser Water Volume Flow Rate [m3/s]", RefrigRack( RackNum ).EvapWaterConsumpRate, "Zone", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Rack Evaporative Condenser Water Volume [m3]", RefrigRack( RackNum ).EvapWaterConsumption, "Zone", "Sum", RefrigRack( RackNum ).Name, _, "Water", "REFRIGERATION", RefrigRack( RackNum ).EndUseSubcategory, "Plant" );
					} //condenser evap

					if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
						SetupOutputVariable( "Refrigeration Compressor Rack Zone Sensible Heating Rate [W]", RefrigRack( RackNum ).SensZoneCreditHeatRate, "Zone", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Rack Zone Sensible Heating Energy [J]", RefrigRack( RackNum ).SensZoneCreditHeat, "Zone", "Sum", RefrigRack( RackNum ).Name );

						SetupOutputVariable( "Refrigeration Compressor Rack Return Air Sensible Heating Rate [W]", RefrigRack( RackNum ).SensHVACCreditHeatRate, "Zone", "Average", RefrigRack( RackNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Rack Return Air Sensible Heating Energy [J]", RefrigRack( RackNum ).SensHVACCreditHeat, "Zone", "Sum", RefrigRack( RackNum ).Name );
						SetupZoneInternalGain( RefrigCase( RefrigRack( RackNum ).CaseNum( 1 ) ).ActualZoneNum, "Refrigeration:CompressorRack", RefrigRack( RackNum ).Name, IntGainTypeOf_RefrigerationCompressorRack, RefrigRack( RackNum ).SensZoneCreditHeatRate, RefrigRack( RackNum ).SensHVACCreditHeatRate );

					} //location zone
				} // Serves coils or case/walkin loads

				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeWater ) { //on HVAC time step no matter what
					SetupOutputVariable( "Refrigeration Compressor Rack Condenser Mass Flow Rate [kg/s]", RefrigRack( RackNum ).MassFlowRate, "HVAC", "Average", RefrigRack( RackNum ).Name );

					SetupOutputVariable( "Refrigeration Compressor Rack Condenser Heat Transfer Rate [W]", RefrigRack( RackNum ).CondLoad, "HVAC", "Average", RefrigRack( RackNum ).Name );

					SetupOutputVariable( "Refrigeration Compressor Rack Condenser Heat Transfer Energy [J]", RefrigRack( RackNum ).CondEnergy, "HVAC", "Sum", RefrigRack( RackNum ).Name, _, "ENERGYTRANSFER", "Heating", _, "Plant" );

				} //Condenser cooling water
			} //Refrigerated Racks
		} //NumRefrigeratedRacks > 0

		if ( NumRefrigSystems > 0 ) {
			// CurrentModuleObject='Refrigeration:System'
			for ( RefrigSysNum = 1; RefrigSysNum <= NumRefrigSystems; ++RefrigSysNum ) {
				if ( System( RefrigSysNum ).CoilFlag ) { //system serves chillers and is solved on HVAC time step
					if ( System( RefrigSysNum ).NumStages == 1 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Total Compressor Electric Power [W]", System( RefrigSysNum ).TotCompPower, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total Compressor Electric Energy [J]", System( RefrigSysNum ).TotCompElecConsump, "HVAC", "Sum", System( RefrigSysNum ).Name );
					} else if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Total Low Stage Compressor Electric Power [W]", System( RefrigSysNum ).TotCompPower, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total Low Stage Compressor Electric Energy [J]", System( RefrigSysNum ).TotCompElecConsump, "HVAC", "Sum", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total High Stage Compressor Electric Power [W]", System( RefrigSysNum ).TotHiStageCompPower, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total High Stage Compressor Electric Energy [J]", System( RefrigSysNum ).TotHiStageCompElecConsump, "HVAC", "Sum", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total Low and High Stage Compressor Electric Energy [J]", System( RefrigSysNum ).TotCompElecConsumpTwoStage, "HVAC", "Sum", System( RefrigSysNum ).Name );
					} // NumStages
					SetupOutputVariable( "Refrigeration Air Chiller System Average Compressor COP [W/W]", System( RefrigSysNum ).AverageCompressorCOP, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Total Air Chiller Heat Transfer Rate [W]", System( RefrigSysNum ).TotalCoolingLoad, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Total Case and Walk In Heat Transfer Energy [J]", System( RefrigSysNum ).TotalCoolingEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Total Transferred Load Heat Transfer Rate [W]", System( RefrigSysNum ).TotTransferLoad, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Total Transferred Load Heat Transfer Energy [J]", System( RefrigSysNum ).TotTransferEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Total Suction Pipe Heat Gain Rate [W]", System( RefrigSysNum ).PipeHeatLoad, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Total Suction Pipe Heat Gain Energy [J]", System( RefrigSysNum ).PipeHeatEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name );
					if ( System( RefrigSysNum ).NumStages == 1 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Total Compressor Heat Transfer Rate [W]", System( RefrigSysNum ).TotCompCapacity, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total Compressor Heat Transfer Energy [J]", System( RefrigSysNum ).TotCompCoolingEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
					} else if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Rate [W]", System( RefrigSysNum ).TotCompCapacity, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Energy [J]", System( RefrigSysNum ).TotCompCoolingEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
						SetupOutputVariable( "Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Rate [W]", System( RefrigSysNum ).TotHiStageCompCapacity, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Energy [J]", System( RefrigSysNum ).TotHiStageCompCoolingEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
					} // NumStages
					SetupOutputVariable( "Refrigeration Air Chiller System Net Rejected Heat Transfer Rate [W]", System( RefrigSysNum ).NetHeatRejectLoad, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Net Rejected Heat Transfer Energy [J]", System( RefrigSysNum ).NetHeatRejectEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Estimated Refrigerant Inventory Mass [kg]", System( RefrigSysNum ).RefInventory, "HVAC", "Average", System( RefrigSysNum ).Name );
					if ( System( RefrigSysNum ).NumStages == 1 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Estimated Refrigerant Mass Flow Rate [kg/s]", System( RefrigSysNum ).RefMassFlowComps, "HVAC", "Average", System( RefrigSysNum ).Name );
					} else if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]", System( RefrigSysNum ).RefMassFlowComps, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]", System( RefrigSysNum ).RefMassFlowHiStageComps, "HVAC", "Average", System( RefrigSysNum ).Name );
					} // NumStages
					if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Intercooler Temperature [C]", System( RefrigSysNum ).TIntercooler, "HVAC", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Intercooler Pressure [Pa]", System( RefrigSysNum ).PIntercooler, "HVAC", "Average", System( RefrigSysNum ).Name );
					}
					SetupOutputVariable( "Refrigeration Air Chiller System Condensing Temperature [C]", System( RefrigSysNum ).TCondense, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Evaporating Temperature [C]", System( RefrigSysNum ).TEvapNeeded, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Suction Temperature [C]", System( RefrigSysNum ).TCompIn, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System TXV Liquid Temperature [C]", System( RefrigSysNum ).TLiqInActual, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Rate [W]", System( RefrigSysNum ).LSHXTrans, "HVAC", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Energy [J]", System( RefrigSysNum ).LSHXTransEnergy, "HVAC", "Sum", System( RefrigSysNum ).Name );
				} else { // NOT System(SysNum)%CoilFlag, so serving loads on zone timestep
					if ( System( RefrigSysNum ).NumStages == 1 ) {
						SetupOutputVariable( "Refrigeration System Total Compressor Electric Power [W]", System( RefrigSysNum ).TotCompPower, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total Compressor Electric Energy [J]", System( RefrigSysNum ).TotCompElecConsump, "Zone", "Sum", System( RefrigSysNum ).Name );
					} else if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration System Total Low Stage Compressor Electric Power [W]", System( RefrigSysNum ).TotCompPower, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total Low Stage Compressor Electric Energy [J]", System( RefrigSysNum ).TotCompElecConsump, "Zone", "Sum", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total High Stage Compressor Electric Power [W]", System( RefrigSysNum ).TotHiStageCompPower, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total High Stage Compressor Electric Energy [J]", System( RefrigSysNum ).TotHiStageCompElecConsump, "Zone", "Sum", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total Low and High Stage Compressor Electric Energy [J]", System( RefrigSysNum ).TotCompElecConsumpTwoStage, "Zone", "Sum", System( RefrigSysNum ).Name );
					} // NumStages
					SetupOutputVariable( "Refrigeration System Average Compressor COP [W/W]", System( RefrigSysNum ).AverageCompressorCOP, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Total Cases and Walk Ins Heat Transfer Rate [W]", System( RefrigSysNum ).TotalCoolingLoad, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Total Cases and Walk Ins Heat Transfer Energy [J]", System( RefrigSysNum ).TotalCoolingEnergy, "Zone", "Sum", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Total Transferred Load Heat Transfer Rate [W]", System( RefrigSysNum ).TotTransferLoad, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Total Transferred Load Heat Transfer Energy [J]", System( RefrigSysNum ).TotTransferEnergy, "Zone", "Sum", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Total Suction Pipe Heat Gain Rate [W]", System( RefrigSysNum ).PipeHeatLoad, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Total Suction Pipe Heat Gain Energy [J]", System( RefrigSysNum ).PipeHeatEnergy, "Zone", "Sum", System( RefrigSysNum ).Name );
					if ( System( RefrigSysNum ).NumStages == 1 ) {
						SetupOutputVariable( "Refrigeration System Total Compressor Heat Transfer Rate [W]", System( RefrigSysNum ).TotCompCapacity, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total Compressor Heat Transfer Energy [J]", System( RefrigSysNum ).TotCompCoolingEnergy, "Zone", "Sum", System( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
					} else if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration System Total Low Stage Compressor Heat Transfer Rate [W]", System( RefrigSysNum ).TotCompCapacity, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total Low Stage Compressor Heat Transfer Energy [J]", System( RefrigSysNum ).TotCompCoolingEnergy, "Zone", "Sum", System( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
						SetupOutputVariable( "Refrigeration System Total High Stage Compressor Heat Transfer Rate [W]", System( RefrigSysNum ).TotHiStageCompCapacity, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Total High Stage Compressor Heat Transfer Energy [J]", System( RefrigSysNum ).TotHiStageCompCoolingEnergy, "Zone", "Sum", System( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
					} // NumStages
					SetupOutputVariable( "Refrigeration System Net Rejected Heat Transfer Rate [W]", System( RefrigSysNum ).NetHeatRejectLoad, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Net Rejected Heat Transfer Energy [J]", System( RefrigSysNum ).NetHeatRejectEnergy, "Zone", "Sum", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Estimated Refrigerant Inventory Mass [kg]", System( RefrigSysNum ).RefInventory, "Zone", "Average", System( RefrigSysNum ).Name );
					if ( System( RefrigSysNum ).NumStages == 1 ) {
						SetupOutputVariable( "Refrigeration System Estimated Refrigerant Mass Flow Rate [kg/s]", System( RefrigSysNum ).RefMassFlowComps, "Zone", "Average", System( RefrigSysNum ).Name );
					} else if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]", System( RefrigSysNum ).RefMassFlowComps, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]", System( RefrigSysNum ).RefMassFlowHiStageComps, "Zone", "Average", System( RefrigSysNum ).Name );
					} // NumStages
					if ( System( RefrigSysNum ).NumStages == 2 ) {
						SetupOutputVariable( "Refrigeration System Intercooler Temperature [C]", System( RefrigSysNum ).TIntercooler, "Zone", "Average", System( RefrigSysNum ).Name );
						SetupOutputVariable( "Refrigeration System Intercooler Pressure [Pa]", System( RefrigSysNum ).PIntercooler, "Zone", "Average", System( RefrigSysNum ).Name );
					}
					SetupOutputVariable( "Refrigeration System Condensing Temperature [C]", System( RefrigSysNum ).TCondense, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Evaporating Temperature [C]", System( RefrigSysNum ).TEvapNeeded, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Suction Pipe Suction Temperature [C]", System( RefrigSysNum ).TCompIn, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Thermostatic Expansion Valve Liquid Temperature [C]", System( RefrigSysNum ).TLiqInActual, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Liquid Suction Subcooler Heat Transfer Rate [W]", System( RefrigSysNum ).LSHXTrans, "Zone", "Average", System( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration System Liquid Suction Subcooler Heat Transfer Energy [J]", System( RefrigSysNum ).LSHXTransEnergy, "Zone", "Sum", System( RefrigSysNum ).Name );
				} //System(coilflag)

				if ( System( RefrigSysNum ).SystemRejectHeatToZone ) {
					if ( Condenser( System( RefrigSysNum ).CondenserNum( 1 ) ).InletAirZoneNum > 0 ) SetupZoneInternalGain( Condenser( System( RefrigSysNum ).CondenserNum( 1 ) ).InletAirZoneNum, "Refrigeration:System:Condenser:AirCooled", System( RefrigSysNum ).Name, IntGainTypeOf_RefrigerationSystemAirCooledCondenser, System( RefrigSysNum ).NetHeatRejectLoad );

					if ( System( RefrigSysNum ).SuctionPipeActualZoneNum > 0 ) SetupZoneInternalGain( System( RefrigSysNum ).SuctionPipeActualZoneNum, "Refrigeration:System:SuctionPipe", System( RefrigSysNum ).Name, IntGainTypeOf_RefrigerationSystemSuctionPipe, System( RefrigSysNum ).PipeHeatLoad );

				}
			} // numrefrigsystems

			//Report Compressor ENERGY here, not on system level for meters.
			for ( CompNum = 1; CompNum <= NumSimulationCompressors; ++CompNum ) {
				// CurrentModuleObject='Refrigeration:Compressor'
				if ( Compressor( CompNum ).NumSysAttach == 1 ) { //only set up reports for compressors that are used once and only once
					if ( Compressor( CompNum ).CoilFlag ) { //Compressor serving system with chillers on HVAC time step
						SetupOutputVariable( "Refrigeration Air Chiller System Compressor Electric Power [W]", Compressor( CompNum ).Power, "HVAC", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Compressor Electric Energy [J]", Compressor( CompNum ).ElecConsumption, "HVAC", "Sum", Compressor( CompNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Compressor( CompNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller System Compressor Heat Transfer Rate [W]", Compressor( CompNum ).Capacity, "HVAC", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Compressor Heat Transfer Energy [J]", Compressor( CompNum ).CoolingEnergy, "HVAC", "Sum", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Compressor Runtime Fraction []", Compressor( CompNum ).LoadFactor, "HVAC", "Average", Compressor( CompNum ).Name );
					} else { // serve cases/walkins on zone time step
						SetupOutputVariable( "Refrigeration Compressor Electric Power [W]", Compressor( CompNum ).Power, "Zone", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Electric Energy [J]", Compressor( CompNum ).ElecConsumption, "Zone", "Sum", Compressor( CompNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Compressor( CompNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Compressor Heat Transfer Rate [W]", Compressor( CompNum ).Capacity, "Zone", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Heat Transfer Energy [J]", Compressor( CompNum ).CoolingEnergy, "Zone", "Sum", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Runtime Fraction []", Compressor( CompNum ).LoadFactor, "Zone", "Average", Compressor( CompNum ).Name );
					} // Serve coils on HVAC time step or cases/walkins on Zone time step
				} // NumSysAttach
			} //CompNum on NumSimulationCompressors

			// Report Variables for Refrigeration Condensers
			for ( CondNum = 1; CondNum <= NumRefrigCondensers; ++CondNum ) {
				// CurrentModuleObject='Refrigeration:Condenser:*'
				if ( Condenser( CondNum ).CoilFlag ) { //Condenser serving system with chillers on HVAC time step
					SetupOutputVariable( "Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]", Condenser( CondNum ).CondLoad, "HVAC", "Average", Condenser( CondNum ).Name );
					SetupOutputVariable( "Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]", Condenser( CondNum ).CondEnergy, "HVAC", "Sum", Condenser( CondNum ).Name );

					if ( Condenser( CondNum ).CondenserType != RefrigCondenserTypeCascade ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate [W]", Condenser( CondNum ).TotalHeatRecoveredLoad, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]", Condenser( CondNum ).TotalHeatRecoveredEnergy, "HVAC", "Sum", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]", Condenser( CondNum ).ExternalHeatRecoveredLoad, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]", Condenser( CondNum ).ExternalEnergyRecovered, "HVAC", "Sum", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]", Condenser( CondNum ).InternalHeatRecoveredLoad, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]", Condenser( CondNum ).InternalEnergyRecovered, "HVAC", "Sum", Condenser( CondNum ).Name );
					} //not cascade because recovered energy on cascade systems passed up to higher temperature system

					if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeAir ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Fan Electric Power [W]", Condenser( CondNum ).ActualFanPower, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Fan Electric Energy [J]", Condenser( CondNum ).FanElecEnergy, "HVAC", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
					} //Air cooled

					if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeEvap ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Fan Electric Power [W]", Condenser( CondNum ).ActualFanPower, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Fan Electric Energy [J]", Condenser( CondNum ).FanElecEnergy, "HVAC", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Pump Electric Power [W]", Condenser( CondNum ).ActualEvapPumpPower, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Pump Electric Energy [J]", Condenser( CondNum ).EvapPumpConsumption, "HVAC", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Basin Heater Electric Power [W]", Condenser( CondNum ).BasinHeaterPower, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Basin Heater Electric Energy [J]", Condenser( CondNum ).BasinHeaterConsumption, "HVAC", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Evaporated Water Volume Flow Rate [m3/s]", Condenser( CondNum ).EvapWaterConsumpRate, "HVAC", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Evaporated Water Volume [m3]", Condenser( CondNum ).EvapWaterConsumption, "HVAC", "Sum", Condenser( CondNum ).Name, _, "Water", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
					} //Evaporative Condenser Variables

					if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeWater ) {
						SetupOutputVariable( "Refrigeration Air Chiller System Condenser Fluid Mass Flow Rate [kg/s]", Condenser( CondNum ).MassFlowRate, "HVAC", "Average", Condenser( CondNum ).Name );

					} //Water-cooled Condenser variables

				} else { //Serving loads/systems with cases and walkins on zone time step

					SetupOutputVariable( "Refrigeration System Condenser Heat Transfer Rate [W]", Condenser( CondNum ).CondLoad, "Zone", "Average", Condenser( CondNum ).Name );
					SetupOutputVariable( "Refrigeration System Condenser Heat Transfer Energy [J]", Condenser( CondNum ).CondEnergy, "Zone", "Sum", Condenser( CondNum ).Name );

					if ( Condenser( CondNum ).CondenserType != RefrigCondenserTypeCascade ) {
						SetupOutputVariable( "Refrigeration System Condenser Total Recovered Heat Transfer Rate [W]", Condenser( CondNum ).TotalHeatRecoveredLoad, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]", Condenser( CondNum ).TotalHeatRecoveredEnergy, "Zone", "Sum", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]", Condenser( CondNum ).ExternalHeatRecoveredLoad, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]", Condenser( CondNum ).ExternalEnergyRecovered, "Zone", "Sum", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]", Condenser( CondNum ).InternalHeatRecoveredLoad, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]", Condenser( CondNum ).InternalEnergyRecovered, "Zone", "Sum", Condenser( CondNum ).Name );
					} //not cascade because recovered energy on cascade systems passed up to higher temperature system

					if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeAir ) {
						SetupOutputVariable( "Refrigeration System Condenser Fan Electric Power [W]", Condenser( CondNum ).ActualFanPower, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Fan Electric Energy [J]", Condenser( CondNum ).FanElecEnergy, "Zone", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
					} //Air cooled

					if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeEvap ) {
						SetupOutputVariable( "Refrigeration System Condenser Fan Electric Power [W]", Condenser( CondNum ).ActualFanPower, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Fan Electric Energy [J]", Condenser( CondNum ).FanElecEnergy, "Zone", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration System Condenser Pump Electric Power [W]", Condenser( CondNum ).ActualEvapPumpPower, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Pump Electric Energy [J]", Condenser( CondNum ).EvapPumpConsumption, "Zone", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration System Condenser Basin Heater Electric Power [W]", Condenser( CondNum ).BasinHeaterPower, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Basin Heater Electric Energy [J]", Condenser( CondNum ).BasinHeaterConsumption, "Zone", "Sum", Condenser( CondNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration System Condenser Evaporated Water Volume Flow Rate [m3/s]", Condenser( CondNum ).EvapWaterConsumpRate, "Zone", "Average", Condenser( CondNum ).Name );
						SetupOutputVariable( "Refrigeration System Condenser Evaporated Water Volume [m3]", Condenser( CondNum ).EvapWaterConsumption, "Zone", "Sum", Condenser( CondNum ).Name, _, "Water", "REFRIGERATION", Condenser( CondNum ).EndUseSubcategory, "Plant" );
					} //Evaporative Condenser Variables

					if ( Condenser( CondNum ).CondenserType == RefrigCondenserTypeWater ) {
						SetupOutputVariable( "Refrigeration System Condenser Water Mass Flow Rate [kg/s]", Condenser( CondNum ).MassFlowRate, "HVAC", "Average", Condenser( CondNum ).Name );

					} //Water-cooled Condenser variables
				} // Condenser%CoilFlag to distinguish HVAC vs Zone time steps
			} // CondNum on NumRefrigCondensers

			if ( NumSimulationSubcoolers > 0 ) {
				for ( SubcoolNum = 1; SubcoolNum <= NumSimulationSubcoolers; ++SubcoolNum ) {
					// CurrentModuleObject='Refrigeration:Subcooler'
					if ( Subcooler( SubcoolNum ).CoilFlag ) { //Subcooler serving system with chillers on HVAC time step
						if ( Subcooler( SubcoolNum ).SubcoolerType == Mechanical ) {
							SetupOutputVariable( "Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Rate [W]", Subcooler( SubcoolNum ).MechSCTransLoad, "Zone", "Average", Subcooler( SubcoolNum ).Name );
							SetupOutputVariable( "Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Energy [J]", Subcooler( SubcoolNum ).MechSCTransEnergy, "Zone", "Sum", Subcooler( SubcoolNum ).Name );
						}
					} else { // Subcooler on system serving cases and/or walkins
						if ( Subcooler( SubcoolNum ).SubcoolerType == Mechanical ) {
							SetupOutputVariable( "Refrigeration System Mechanical Subcooler Heat Transfer Rate [W]", Subcooler( SubcoolNum ).MechSCTransLoad, "HVAC", "Average", Subcooler( SubcoolNum ).Name );
							SetupOutputVariable( "Refrigeration System Mechanical Subcooler Heat Transfer Energy [J]", Subcooler( SubcoolNum ).MechSCTransEnergy, "HVAC", "Sum", Subcooler( SubcoolNum ).Name );
						}
					} //Subcoolers on system serving chillers
				} //Subcoolnum on NumSimulationSubcoolers
			} // NumSimulationSubcoolers > 0

		} //NumRefrigSystems > 0

		if ( NumTransRefrigSystems > 0 ) {
			// CurrentModuleObject='Refrigeration:TranscriticalSystem'
			for ( RefrigSysNum = 1; RefrigSysNum <= NumTransRefrigSystems; ++RefrigSysNum ) {
				// for both SingleStage and TwoStage systems (medium temperature loads present)
				SetupOutputVariable( "Refrigeration Transcritical System Total High Pressure Compressor Electric Power [W]", TransSystem( RefrigSysNum ).TotCompPowerHP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Total High Pressure Compressor Electric Energy [J]", TransSystem( RefrigSysNum ).TotCompElecConsumpHP, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Total Compressor Electric Energy [J]", TransSystem( RefrigSysNum ).TotCompElecConsump, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Average COP [W/W]", TransSystem( RefrigSysNum ).AverageCompressorCOP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).TotalCoolingLoadMT, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).TotalCoolingEnergyMT, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Total Cases and Walk Ins Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).TotalCoolingEnergy, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).PipeHeatLoadMT, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).PipeHeatEnergyMT, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System High Pressure Compressor Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).TotCompCapacityHP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System High Pressure Compressor Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).TotCompCoolingEnergyHP, "Zone", "Sum", TransSystem( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
				SetupOutputVariable( "Refrigeration Transcritical System Net Rejected Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).NetHeatRejectLoad, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Net Rejected Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).NetHeatRejectEnergy, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Estimated Refrigerant Inventory Mass [kg]", TransSystem( RefrigSysNum ).RefInventory, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Refrigerant Mass Flow Rate [kg/s]", TransSystem( RefrigSysNum ).RefMassFlowComps, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Medium Temperature Evaporating Temperature [C]", TransSystem( RefrigSysNum ).TEvapNeededMT, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Medium Temperature Suction Temperature [C]", TransSystem( RefrigSysNum ).TCompInHP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				if ( TransSystem( RefrigSysNum ).TransSysType == 2 ) { // for TwoStage system only (low temperature loads present)
					SetupOutputVariable( "Refrigeration Transcritical System Low Pressure Compressor Electric Power [W]", TransSystem( RefrigSysNum ).TotCompPowerLP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Pressure Compressor Electric Energy [J]", TransSystem( RefrigSysNum ).TotCompElecConsumpLP, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).TotalCoolingLoadLT, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).TotalCoolingEnergyLT, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).PipeHeatLoadLT, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).PipeHeatEnergyLT, "Zone", "Sum", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Rate [W]", TransSystem( RefrigSysNum ).TotCompCapacityLP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Energy [J]", TransSystem( RefrigSysNum ).TotCompCoolingEnergyLP, "Zone", "Sum", TransSystem( RefrigSysNum ).Name ); //indiv compressors go to meter, not system sum
					SetupOutputVariable( "Refrigeration Transcritical System Low Temperature Evaporating Temperature [C]", TransSystem( RefrigSysNum ).TEvapNeededLT, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
					SetupOutputVariable( "Refrigeration Transcritical System Low Temperature Suction Temperature [C]", TransSystem( RefrigSysNum ).TCompInLP, "Zone", "Average", TransSystem( RefrigSysNum ).Name );
				} // (TransSystem(RefrigSysNum)%TransSysType == 2)

				if ( TransSystem( RefrigSysNum ).SystemRejectHeatToZone ) {
					if ( GasCooler( TransSystem( RefrigSysNum ).GasCoolerNum( 1 ) ).InletAirZoneNum > 0 ) SetupZoneInternalGain( GasCooler( TransSystem( RefrigSysNum ).GasCoolerNum( 1 ) ).InletAirZoneNum, "Refrigeration:TranscriticalSystem:GasCooler:AirCooled", TransSystem( RefrigSysNum ).Name, IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler, TransSystem( RefrigSysNum ).NetHeatRejectLoad );
				} // (TransSystem(RefrigSysNum)%SystemRejectHeatToZone)
				if ( TransSystem( RefrigSysNum ).SuctionPipeActualZoneNumMT > 0 ) {
					SetupZoneInternalGain( TransSystem( RefrigSysNum ).SuctionPipeActualZoneNumMT, "Refrigeration:TranscriticalSystem:SuctionPipeMT", TransSystem( RefrigSysNum ).Name, IntGainTypeOf_RefrigerationTransSysSuctionPipeMT, TransSystem( RefrigSysNum ).PipeHeatLoadMT );
				} // TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumMT > 0
				if ( TransSystem( RefrigSysNum ).SuctionPipeActualZoneNumLT > 0 ) {
					SetupZoneInternalGain( TransSystem( RefrigSysNum ).SuctionPipeActualZoneNumLT, "Refrigeration:TranscriticalSystem:SuctionPipeLT", TransSystem( RefrigSysNum ).Name, IntGainTypeOf_RefrigerationTransSysSuctionPipeLT, TransSystem( RefrigSysNum ).PipeHeatLoadLT );
				} // TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumLT > 0

				//Report Compressor ENERGY here, not on system level for meters.
				// LP compressors
				for ( CompIndex = 1; CompIndex <= TransSystem( RefrigSysNum ).NumCompressorsLP; ++CompIndex ) {
					CompNum = TransSystem( RefrigSysNum ).CompressorNumLP( CompIndex );
					// CurrentModuleObject='Refrigeration:Compressor'
					if ( Compressor( CompNum ).NumSysAttach == 1 ) { //only set up reports for compressors that are used once and only once
						SetupOutputVariable( "Refrigeration Compressor Electric Power [W]", Compressor( CompNum ).Power, "Zone", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Electric Energy [J]", Compressor( CompNum ).ElecConsumption, "Zone", "Sum", Compressor( CompNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Compressor( CompNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Compressor Heat Transfer Rate [W]", Compressor( CompNum ).Capacity, "Zone", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Heat Transfer Energy [J]", Compressor( CompNum ).CoolingEnergy, "Zone", "Sum", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Runtime Fraction []", Compressor( CompNum ).LoadFactor, "Zone", "Average", Compressor( CompNum ).Name );
					} // NumSysAttach
				} //TransSystem(RefrigSysNum)%NumCompressorsLP

				// HP compressors
				for ( CompIndex = 1; CompIndex <= TransSystem( RefrigSysNum ).NumCompressorsHP; ++CompIndex ) {
					CompNum = TransSystem( RefrigSysNum ).CompressorNumHP( CompIndex );
					// CurrentModuleObject='Refrigeration:Compressor'
					if ( Compressor( CompNum ).NumSysAttach == 1 ) { //only set up reports for compressors that are used once and only once
						SetupOutputVariable( "Refrigeration Compressor Electric Power [W]", Compressor( CompNum ).Power, "Zone", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Electric Energy [J]", Compressor( CompNum ).ElecConsumption, "Zone", "Sum", Compressor( CompNum ).Name, _, "ELECTRICITY", "REFRIGERATION", Compressor( CompNum ).EndUseSubcategory, "Plant" );
						SetupOutputVariable( "Refrigeration Compressor Heat Transfer Rate [W]", Compressor( CompNum ).Capacity, "Zone", "Average", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Heat Transfer Energy [J]", Compressor( CompNum ).CoolingEnergy, "Zone", "Sum", Compressor( CompNum ).Name );
						SetupOutputVariable( "Refrigeration Compressor Runtime Fraction []", Compressor( CompNum ).LoadFactor, "Zone", "Average", Compressor( CompNum ).Name );
					} // NumSysAttach
				} //TransSystem(RefrigSysNum)%NumCompressorsHP

			} // NumTransRefrigSystems
		} // (NumTransRefrigSystems > 0)

		if ( NumSimulationGasCooler > 0 ) {
			for ( GCNum = 1; GCNum <= NumSimulationGasCooler; ++GCNum ) {
				// CurrentModuleObject='Refrigeration:GasCooler:AirCooled'
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Heat Transfer Rate [W]", GasCooler( GCNum ).GasCoolerLoad, "Zone", "Average", GasCooler( GCNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Heat Transfer Energy [J]", GasCooler( GCNum ).GasCoolerEnergy, "Zone", "Sum", GasCooler( GCNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Fan Electric Power [W]", GasCooler( GCNum ).ActualFanPower, "Zone", "Average", GasCooler( GCNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Fan Electric Energy [J]", GasCooler( GCNum ).FanElecEnergy, "Zone", "Sum", GasCooler( GCNum ).Name, _, "ELECTRICITY", "REFRIGERATION", GasCooler( GCNum ).EndUseSubcategory, "Plant" );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Outlet Temperature [C]", GasCooler( GCNum ).TGasCoolerOut, "Zone", "Average", GasCooler( GCNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Outlet Pressure [Pa]", GasCooler( GCNum ).PGasCoolerOut, "Zone", "Average", GasCooler( GCNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Rate [W]", GasCooler( GCNum ).InternalHeatRecoveredLoad, "Zone", "Average", GasCooler( GCNum ).Name );
				SetupOutputVariable( "Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Energy [J]", GasCooler( GCNum ).InternalEnergyRecovered, "Zone", "Sum", GasCooler( GCNum ).Name );
			} // GCNum on NumSimulationGasCooler
		} // (NumSimulationGasCooler >0)

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	InitRefrigeration()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Hudson, ORNL July 2007, Stovall, ORNL, 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize (zero) global variables before simulating compressor racks and refrigerated cases
		// Several variables in this module are accumulative.  For example, unmet compressor loads are carried over
		// to the next time step. Ice loads are accumulated until melted by a defrost.  Because this module can be
		// called multiple times during any single time step, these summations need to be saved ONLY on the last time
		// through any given time step.

		// It is necessary to decrease the condenser load by the amount of heat used elsewhere
		//   via desuperheating water heaters and heating coils.
		//   Because the refrigeration system is solved before the HVAC time step loops, the
		//   refrigeration system must use the values lagged from the previous time step. In
		//   terms of energy, this should balance out and is preferable to not making the correction,
		//   in which case the condenser cooling water/air/fan energy are charged with energy
		//   loads that have been accounted elsewhere.  For consistency, the lagged value must be used,
		//   even if the Zone time step is repeated.  Therefore, the lagged variables are saved
		//   here for use during successive iterations of same zone/load time step.

		// METHODOLOGY EMPLOYED:
		// Global variables for Case Credit are located in DataHeatBalance. To Zone variables are used in the Air Heat
		// Balance in ZoneTempPredictorCorrector to calculate the zone load. To HVAC variables are used in
		// ZoneEquipmentManager to add the portion of case credits attributed to the HVAC system to the zone return air node.

		// Because we can't know apriori whether or not the time step will be repeated, we save the most recent
		// addition/subtraction to/from each accumulating variable.  If the time step is repeated,
		// this most recent addition/subtraction is reversed before the rest of the refrigeration simulation begins.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyBeginEnvrnFlag( true );
		static int SystemID( 0 );
		static int CaseID( 0 );
		static int WalkInID( 0 );
		static int CoilID( 0 );
		static int ICond( 0 );
		static int IRack( 0 );
		static int SecondID( 0 );
		//Used to adjust accumulative variables when time step is repeated
		static Real64 MyCurrentTime( 0.0 ); // Used to determine whether the zone time step is a repetition
		static Real64 MyCurrentTimeSaved( 0.0 ); // Used to determine whether the zone time step is a repetition
		static Real64 MyStepStartTime( 0.0 ); // Used to determine whether the system time step is a repetition
		static Real64 MyStepStartTimeSaved( 0.0 ); // Used to determine whether the system time step is a repetition
		static Real64 TimeStepFraction( 0.0 ); // Used to calculate my current time

		// Zero display case, air-coil, and walk-in cooler credits (summed by zone)
		// to 0 each zone or sys time step
		// These 'casecredit' variables are also used to transfer energy from zone-located
		// compressor-rack condenser heat rejection, heat absorption by distribution piping,
		// suction piping, and receiver shells to zone
		if ( NumOfZones > 0 ) {
			if ( UseSysTimeStep ) {
				for ( int i = CoilSysCredit.l(), e = CoilSysCredit.u(); i <= e; ++i ) {
					CoilSysCredit( i ).reset();
				}
			} // UseSysTimeStep = true

			// Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is false
			if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
				for ( int i = RefrigCaseCredit.l(), e = RefrigCaseCredit.u(); i <= e; ++i ) {
					RefrigCaseCredit( i ).reset();
				}
				for ( int i = CaseWIZoneReport.l(), e = CaseWIZoneReport.u(); i <= e; ++i ) {
					CaseWIZoneReport( i ).reset();
				}
			}
		}

		if ( NumSimulationCases > 0 ) {
			//RefrigCase ALLOCATED to NumSimulationCases
			for ( int i = RefrigCase.l(), e = RefrigCase.u(); i <= e; ++i ) {
				RefrigCase( i ).reset_init();
			}
		} // NumSimulationCases

		if ( NumSimulationWalkIns > 0 ) {
			//WalkIn ALLOCATED to NumSimulationWalkIns
			for ( int i = WalkIn.l(), e = WalkIn.u(); i <= e; ++i ) {
				WalkIn( i ).reset_init();
			}
		}

		if ( HaveChillers ) {
			//HaveChillers is TRUE when NumSimulationRefrigAirChillers > 0
			//WarehouseCoil ALLOCATED to NumSimulationRefrigAirChillers
			for ( int i = WarehouseCoil.l(), e = WarehouseCoil.u(); i <= e; ++i ) {
				WarehouseCoil( i ).reset_init();
			}
		}

		if ( HaveRefrigRacks ) {
			//HaveRefrigRacks TRUE when NumRefrigeratedRacks > 0
			//RefrigRack ALLOCATED to NumRefrigeratedRacks
			for ( int i = RefrigRack.l(), e = RefrigRack.u(); i <= e; ++i ) {
				RefrigRack( i ).reset_init();
			}
			for ( auto & e : HeatReclaimRefrigeratedRack ) e.AvailCapacity = 0.0;
			//Note don't reset basin heat to zero when no load because heater would remain on
			//RefrigRack.BasinHeaterPower = 0.0;
			//RefrigRack.BasinHeaterConsumption = 0.0;
		}

		if ( NumRefrigCondensers > 0 ) {
			//Condenser ALLOCATED to NumRefrigCondensers
			for ( int i = Condenser.l(), e = Condenser.u(); i <= e; ++i ) {
				Condenser( i ).reset_init();
			}
			//N don't reset basin heat to zero when no load because heater would remain on
			for ( auto & e : HeatReclaimRefrigCondenser ) {
				e.AvailCapacity = 0.0;
				e.AvailTemperature = 0.0;
			}
		}

		if ( NumSimulationGasCooler > 0 ) {
			//GasCooler ALLOCATED to NumSimulationGasCooler
			for ( int i = GasCooler.l(), e = GasCooler.u(); i <= e; ++i ) {
				GasCooler( i ).reset_init();
			}
		}

		if ( NumSimulationCompressors > 0 ) {
			//Compressor ALLOCATED to NumSimulationCompressors
			for ( int i = Compressor.l(), e = Compressor.u(); i <= e; ++i ) {
				Compressor( i ).reset_init();
			}
		}

		if ( HaveDetailedRefrig ) {
			//HaveDetailedRefrig is TRUE when NumRefrigSystems > 0
			//System is ALLOCATED to NumRefrigSystems
			for ( int i = System.l(), e = System.u(); i <= e; ++i ) {
				System( i ).reset_init();
			}
		}

		if ( HaveDetailedTransRefrig ) {
			//HaveDetailedTransRefrig is TRUE when NumTransRefrigSystems > 0
			//TransSystem is ALLOCATED to NumTransRefrigSystems
			for ( int i = TransSystem.l(), e = TransSystem.u(); i <= e; ++i ) {
				TransSystem( i ).reset_init();
			}
		}

		if ( NumSimulationSecondarySystems > 0 ) {
			//Secondary is ALLOCATED to NumSimulationSecondarySystems
			for ( int i = Secondary.l(), e = Secondary.u(); i <= e; ++i ) {
				Secondary( i ).reset_init();
			}
		}

		// Accumulative and carry-over variables are not zeroed at start of each time step, only at begining of environment
		if ( BeginEnvrnFlag && MyBeginEnvrnFlag ) {
			if ( NumSimulationCases > 0 ) {
				for ( int i = RefrigCase.l(), e = RefrigCase.u(); i <= e; ++i ) {
					RefrigCase( i ).reset_init_accum();
				}
			}
			if ( NumRefrigSystems > 0 ) {
				for ( auto & e : System ) e.UnmetEnergy = 0.0;
			}
			if ( NumSimulationWalkIns > 0 ) {
				for ( auto & e : WalkIn ) {
					e.KgFrost = 0.0;
					e.StoredEnergy = 0.0;
				}
				for ( WalkInID = 1; WalkInID <= NumSimulationWalkIns; ++WalkInID ) {
					WalkIn( WalkInID ).IceTemp = WalkIn( WalkInID ).TEvapDesign;
				}
			}
			if ( NumSimulationRefrigAirChillers > 0 ) {
				for ( auto & e : WarehouseCoil ) {
					e.KgFrost = 0.0;
					e.KgFrostSaved = 0.0;
				}
				for ( CoilID = 1; CoilID <= NumSimulationRefrigAirChillers; ++CoilID ) {
					WarehouseCoil( CoilID ).IceTemp = WarehouseCoil( CoilID ).TEvapDesign;
					WarehouseCoil( CoilID ).IceTempSaved = WarehouseCoil( CoilID ).TEvapDesign;
				}
			}
			if ( NumSimulationSecondarySystems > 0 ) {
				for ( auto & e : Secondary ) e.UnmetEnergy = 0.0;
			}
			if ( NumRefrigeratedRacks > 0 ) {
				for ( auto & e : HeatReclaimRefrigeratedRack ) {
					e.UsedHVACCoil = 0.0;
					e.UsedWaterHeater = 0.0;
				}
				for ( auto & e : RefrigRack ) {
					e.LaggedUsedWaterHeater = 0.0;
					e.LaggedUsedHVACCoil = 0.0;
				}
			}
			if ( NumRefrigCondensers > 0 ) {
				for ( auto & e : HeatReclaimRefrigCondenser ) {
					e.UsedHVACCoil = 0.0;
					e.UsedWaterHeater = 0.0;
				}
				for ( auto & e : Condenser ) {
					e.LaggedUsedWaterHeater = 0.0;
					e.LaggedUsedHVACCoil = 0.0;
				}
			}
			for ( SystemID = 1; SystemID <= NumRefrigSystems; ++SystemID ) {
				if ( allocated( System( SystemID ).MechSCLoad ) ) System( SystemID ).MechSCLoad = 0.0;
				System( SystemID ).LSHXTrans = 0.0;
				System( SystemID ).LSHXTransEnergy = 0.0;
			}

			if ( NumOfTimeStepInHour > 0.0 ) TimeStepFraction = 1.0 / double( NumOfTimeStepInHour );
			MyBeginEnvrnFlag = false;

		} // ( BeginEnvrnFlag && MyBeginEnvrnFlag )

		if ( ! BeginEnvrnFlag ) MyBeginEnvrnFlag = true;

		// Avoid multiplying accumulation if go through zone/load time step more than once.
		if ( ! WarmupFlag ) { // because no accumulation is done during warm up
			// Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
			if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
				MyCurrentTime = ( HourOfDay - 1 ) + TimeStep * TimeStepFraction;
				if ( std::abs( MyCurrentTime - MyCurrentTimeSaved ) < MySmallNumber ) {
					// If the time step is repeated, need to return to correct values at start of time step
					if ( NumSimulationCases > 0 ) {
						for ( CaseID = 1; CaseID <= NumSimulationCases; ++CaseID ) {
							RefrigCase( CaseID ).DefrostEnergy = RefrigCase( CaseID ).DefrostEnergySaved;
							RefrigCase( CaseID ).StockingEnergy = RefrigCase( CaseID ).StockingEnergySaved;
							RefrigCase( CaseID ).WarmEnvEnergy = RefrigCase( CaseID ).WarmEnvEnergySaved;
							RefrigCase( CaseID ).KgFrost = RefrigCase( CaseID ).KgFrostSaved;
							RefrigCase( CaseID ).StoredEnergy = RefrigCase( CaseID ).StoredEnergySaved;
						} // CaseID
					} // NumSimulationCases
					if ( NumSimulationWalkIns > 0 ) {
						for ( WalkInID = 1; WalkInID <= NumSimulationWalkIns; ++WalkInID ) {
							WalkIn( WalkInID ).KgFrost = WalkIn( WalkInID ).KgFrostSaved;
							WalkIn( WalkInID ).StoredEnergy = WalkIn( WalkInID ).StoredEnergySaved;
							WalkIn( WalkInID ).IceTemp = WalkIn( WalkInID ).IceTempSaved;
						}
					}
					if ( NumRefrigSystems > 0 ) {
						for ( SystemID = 1; SystemID <= NumRefrigSystems; ++SystemID ) {
							if ( System( SystemID ).CoilFlag ) continue;
							System( SystemID ).UnmetEnergy = System( SystemID ).UnmetEnergySaved;
						}
					}
					if ( NumTransRefrigSystems > 0 ) {
						for ( SystemID = 1; SystemID <= NumTransRefrigSystems; ++SystemID ) {
							TransSystem( SystemID ).UnmetEnergyMT = TransSystem( SystemID ).UnmetEnergySavedMT;
							TransSystem( SystemID ).UnmetEnergyLT = TransSystem( SystemID ).UnmetEnergySavedLT;
						}
					}
					if ( NumSimulationSecondarySystems > 0 ) {
						for ( SecondID = 1; SecondID <= NumSimulationSecondarySystems; ++SecondID ) {
							if ( Secondary( SecondID ).CoilFlag ) continue;
							Secondary( SecondID ).UnmetEnergy = Secondary( SecondID ).UnmetEnergySaved;
						}
					}

				} else {
					// First time through this Zone time step, so set saved values to those in place at start of this time step
					MyCurrentTimeSaved = MyCurrentTime;
					if ( NumSimulationCases > 0 ) {
						for ( CaseID = 1; CaseID <= NumSimulationCases; ++CaseID ) {
							RefrigCase( CaseID ).DefrostEnergySaved = RefrigCase( CaseID ).DefrostEnergy;
							RefrigCase( CaseID ).StockingEnergySaved = RefrigCase( CaseID ).StockingEnergy;
							RefrigCase( CaseID ).WarmEnvEnergySaved = RefrigCase( CaseID ).WarmEnvEnergy;
							RefrigCase( CaseID ).KgFrostSaved = RefrigCase( CaseID ).KgFrost;
							RefrigCase( CaseID ).StoredEnergySaved = RefrigCase( CaseID ).StoredEnergy;
						} //caseid
					} //numsimulationcases
					if ( NumSimulationWalkIns > 0 ) {
						for ( WalkInID = 1; WalkInID <= NumSimulationWalkIns; ++WalkInID ) {
							WalkIn( WalkInID ).KgFrostSaved = WalkIn( WalkInID ).KgFrost;
							WalkIn( WalkInID ).StoredEnergySaved = WalkIn( WalkInID ).StoredEnergy;
							WalkIn( WalkInID ).IceTempSaved = WalkIn( WalkInID ).IceTemp;
						}
					}
					if ( NumRefrigSystems > 0 ) {
						for ( SystemID = 1; SystemID <= NumRefrigSystems; ++SystemID ) {
							if ( System( SystemID ).CoilFlag ) continue;
							System( SystemID ).UnmetEnergySaved = System( SystemID ).UnmetEnergy;
						}
					}
					if ( NumTransRefrigSystems > 0 ) {
						for ( SystemID = 1; SystemID <= NumTransRefrigSystems; ++SystemID ) {
							TransSystem( SystemID ).UnmetEnergySavedMT = TransSystem( SystemID ).UnmetEnergyMT;
							TransSystem( SystemID ).UnmetEnergySavedLT = TransSystem( SystemID ).UnmetEnergyLT;
						}
					}
					if ( NumSimulationSecondarySystems > 0 ) {
						for ( SecondID = 1; SecondID <= NumSimulationSecondarySystems; ++SecondID ) {
							if ( Secondary( SecondID ).CoilFlag ) continue;
							Secondary( SecondID ).UnmetEnergySaved = Secondary( SecondID ).UnmetEnergy;
						}
					}
					//Following lagged variables set for consistency to value calculated prev time through HVAC time step loops
					if ( allocated( HeatReclaimRefrigeratedRack ) ) {
						for ( IRack = 1; IRack <= NumRefrigeratedRacks; ++IRack ) {
							RefrigRack( IRack ).LaggedUsedHVACCoil = HeatReclaimRefrigeratedRack( IRack ).UsedHVACCoil;
							RefrigRack( IRack ).LaggedUsedWaterHeater = HeatReclaimRefrigeratedRack( IRack ).UsedWaterHeater;
						}
					}
					if ( allocated( HeatReclaimRefrigCondenser ) ) {
						for ( ICond = 1; ICond <= NumRefrigCondensers; ++ICond ) {
							Condenser( ICond ).LaggedUsedHVACCoil = HeatReclaimRefrigCondenser( ICond ).UsedHVACCoil;
							Condenser( ICond ).LaggedUsedWaterHeater = HeatReclaimRefrigCondenser( ICond ).UsedWaterHeater;
						}
					}
				} //repeating same time step

			} else { // using UseSysTimeStep as a flag for a chiller system

				MyStepStartTime = CurrentTime - TimeStepZone + SysTimeElapsed;
				if ( std::abs( MyStepStartTime - MyStepStartTimeSaved ) < MySmallNumber ) {
					// If the time step is repeated, need to return to correct values at start of time step
					if ( NumSimulationRefrigAirChillers > 0 ) {
						for ( CoilID = 1; CoilID <= NumSimulationRefrigAirChillers; ++CoilID ) {
							WarehouseCoil( CoilID ).KgFrost = WarehouseCoil( CoilID ).KgFrostSaved;
							WarehouseCoil( CoilID ).IceTemp = WarehouseCoil( CoilID ).IceTempSaved;
						}
					}
				} else { // First time through this system time step or hvac loop,
					// so set saved values to those in place at start of this time step
					MyStepStartTimeSaved = MyStepStartTime;
					if ( NumSimulationRefrigAirChillers > 0 ) {
						for ( CoilID = 1; CoilID <= NumSimulationRefrigAirChillers; ++CoilID ) {
							WarehouseCoil( CoilID ).KgFrostSaved = WarehouseCoil( CoilID ).KgFrost;
							WarehouseCoil( CoilID ).IceTempSaved = WarehouseCoil( CoilID ).IceTemp;
						}
					}
					//Following lagged variables set for consistency to value calculated prev time through HVAC time step loops
					if ( allocated( HeatReclaimRefrigeratedRack ) ) {
						for ( IRack = 1; IRack <= NumRefrigeratedRacks; ++IRack ) {
							RefrigRack( IRack ).LaggedUsedHVACCoil = HeatReclaimRefrigeratedRack( IRack ).UsedHVACCoil;
							RefrigRack( IRack ).LaggedUsedWaterHeater = HeatReclaimRefrigeratedRack( IRack ).UsedWaterHeater;
						}
					}
					if ( allocated( HeatReclaimRefrigCondenser ) ) {
						for ( ICond = 1; ICond <= NumRefrigCondensers; ++ICond ) {
							Condenser( ICond ).LaggedUsedHVACCoil = HeatReclaimRefrigCondenser( ICond ).UsedHVACCoil;
							Condenser( ICond ).LaggedUsedWaterHeater = HeatReclaimRefrigCondenser( ICond ).UsedWaterHeater;
						}
					}
				} // if first time
			} //(.NOT. UseSysTimeStep)

		} //warm up flag

		if ( AnyEnergyManagementSystemInModel ) {
			if ( NumRefrigSystems > 0 ) {
				for ( SystemID = 1; SystemID <= NumRefrigSystems; ++SystemID ) {
					if ( System( SystemID ).EMSOverrideOnTCondenseMin ) {
						System( SystemID ).TCondenseMin = System( SystemID ).EMSOverrideValueTCondenseMin;
					} else {
						System( SystemID ).TCondenseMin = System( SystemID ).TCondenseMinInput;
					}
				}
			}
		}

	}

	void
	InitRefrigerationPlantConnections()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// do inits that should only occur when component model routines
		// are entered from plant, for water cooled Condensers and Refrigeration Racks

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_RefrigSystemWaterCondenser;
		using DataPlant::TypeOf_RefrigerationWaterCoolRack;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "InitRefrigerationPlantConnections" );
		static bool MyBeginEnvrnFlag( true );
		static bool errFlag( false );
		static int RefCondLoop( 0 ); // loop over Condenser
		static int RefCompRackLoop( 0 ); // loop over RefrigRack
		Real64 rho; // local fluid property for cooling water

		//initialize plant topology information, if applicable
		if ( MyReferPlantScanFlag && allocated( PlantLoop ) ) {
			for ( RefCondLoop = 1; RefCondLoop <= NumRefrigCondensers; ++RefCondLoop ) {
				if ( Condenser( RefCondLoop ).CondenserType != RefrigCondenserTypeWater ) continue;

				errFlag = false;
				ScanPlantLoopsForObject( Condenser( RefCondLoop ).Name, TypeOf_RefrigSystemWaterCondenser, Condenser( RefCondLoop ).PlantLoopNum, Condenser( RefCondLoop ).PlantLoopSideNum, Condenser( RefCondLoop ).PlantBranchNum, Condenser( RefCondLoop ).PlantCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitRefrigerationPlantConnections: Program terminated due to previous condition(s)." );
				}

				rho = GetDensityGlycol( PlantLoop( Condenser( RefCondLoop ).PlantLoopNum ).FluidName, 20.0, PlantLoop( Condenser( RefCondLoop ).PlantLoopNum ).FluidIndex, RoutineName );

				if ( Condenser( RefCondLoop ).FlowType == ConstantFlow ) {
					Condenser( RefCondLoop ).MassFlowRateMax = Condenser( RefCondLoop ).DesVolFlowRate * rho;
				} else if ( Condenser( RefCondLoop ).FlowType == VariableFlow ) {
					Condenser( RefCondLoop ).MassFlowRateMax = Condenser( RefCondLoop ).VolFlowRateMax * rho;
				}

			}

			for ( RefCompRackLoop = 1; RefCompRackLoop <= NumRefrigeratedRacks; ++RefCompRackLoop ) {
				if ( RefrigRack( RefCompRackLoop ).CondenserType != RefrigCondenserTypeWater ) continue;

				errFlag = false;
				ScanPlantLoopsForObject( RefrigRack( RefCompRackLoop ).Name, TypeOf_RefrigerationWaterCoolRack, RefrigRack( RefCompRackLoop ).PlantLoopNum, RefrigRack( RefCompRackLoop ).PlantLoopSideNum, RefrigRack( RefCompRackLoop ).PlantBranchNum, RefrigRack( RefCompRackLoop ).PlantCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitRefrigerationPlantConnections: Program terminated due to previous condition(s)." );
				}

				rho = GetDensityGlycol( PlantLoop( RefrigRack( RefCompRackLoop ).PlantLoopNum ).FluidName, 20.0, PlantLoop( RefrigRack( RefCompRackLoop ).PlantLoopNum ).FluidIndex, RoutineName );

				if ( RefrigRack( RefCompRackLoop ).FlowType == ConstantFlow ) {
					RefrigRack( RefCompRackLoop ).MassFlowRateMax = RefrigRack( RefCompRackLoop ).DesVolFlowRate * rho;
				} else if ( RefrigRack( RefCompRackLoop ).FlowType == VariableFlow ) {
					RefrigRack( RefCompRackLoop ).MassFlowRateMax = RefrigRack( RefCompRackLoop ).VolFlowRateMax * rho;
				}

			}

			MyReferPlantScanFlag = false;
		} else if ( MyReferPlantScanFlag && ! AnyPlantInModel ) {
			MyReferPlantScanFlag = false;
		}

		if ( BeginEnvrnFlag && MyBeginEnvrnFlag ) {

			// do plant inits, if applicable
			if ( ! MyReferPlantScanFlag ) {
				for ( RefCondLoop = 1; RefCondLoop <= NumRefrigCondensers; ++RefCondLoop ) {
					if ( Condenser( RefCondLoop ).CondenserType != RefrigCondenserTypeWater ) continue;

					rho = GetDensityGlycol( PlantLoop( Condenser( RefCondLoop ).PlantLoopNum ).FluidName, 20.0, PlantLoop( Condenser( RefCondLoop ).PlantLoopNum ).FluidIndex, RoutineName );

					if ( Condenser( RefCondLoop ).FlowType == ConstantFlow ) {
						Condenser( RefCondLoop ).MassFlowRateMax = Condenser( RefCondLoop ).DesVolFlowRate * rho;
					} else if ( Condenser( RefCondLoop ).FlowType == VariableFlow ) {
						Condenser( RefCondLoop ).MassFlowRateMax = Condenser( RefCondLoop ).VolFlowRateMax * rho;
					}

					InitComponentNodes( 0.0, Condenser( RefCondLoop ).MassFlowRateMax, Condenser( RefCondLoop ).InletNode, Condenser( RefCondLoop ).OutletNode, Condenser( RefCondLoop ).PlantLoopNum, Condenser( RefCondLoop ).PlantLoopSideNum, Condenser( RefCondLoop ).PlantBranchNum, Condenser( RefCondLoop ).PlantCompNum );
				}
				for ( RefCompRackLoop = 1; RefCompRackLoop <= NumRefrigeratedRacks; ++RefCompRackLoop ) {
					if ( RefrigRack( RefCompRackLoop ).CondenserType != RefrigCondenserTypeWater ) continue;

					rho = GetDensityGlycol( PlantLoop( RefrigRack( RefCompRackLoop ).PlantLoopNum ).FluidName, 20.0, PlantLoop( RefrigRack( RefCompRackLoop ).PlantLoopNum ).FluidIndex, RoutineName );

					if ( RefrigRack( RefCompRackLoop ).FlowType == ConstantFlow ) {
						RefrigRack( RefCompRackLoop ).MassFlowRateMax = RefrigRack( RefCompRackLoop ).DesVolFlowRate * rho;
					} else if ( RefrigRack( RefCompRackLoop ).FlowType == VariableFlow ) {
						RefrigRack( RefCompRackLoop ).MassFlowRateMax = RefrigRack( RefCompRackLoop ).VolFlowRateMax * rho;
					}

					InitComponentNodes( 0.0, RefrigRack( RefCompRackLoop ).MassFlowRateMax, RefrigRack( RefCompRackLoop ).InletNode, RefrigRack( RefCompRackLoop ).OutletNode, RefrigRack( RefCompRackLoop ).PlantLoopNum, RefrigRack( RefCompRackLoop ).PlantLoopSideNum, RefrigRack( RefCompRackLoop ).PlantBranchNum, RefrigRack( RefCompRackLoop ).PlantCompNum );
				}

			}
			MyBeginEnvrnFlag = false;

		} //(BeginEnvrnFlag .AND. MyBeginEnvrnFlag)

		if ( ! BeginEnvrnFlag ) MyBeginEnvrnFlag = true;

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcRackSystem( int const RackNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Shirey, FSEC Dec 2004; Hudson, ORNL Feb 2007, July 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate compressor rack load, power, energy consumption, and condenser fan/pump power and consumption

		// METHODOLOGY EMPLOYED:
		// Loop through cases attached to each rack and determine total load on compressor rack

		// REFERENCES:
		// "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
		//  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutDryBulbTemp;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CaseID; // Index to absolute case ID
		int CaseNum; // Index to refrigerated case attached to rack
		//INTEGER     :: SecondID                    ! Index to absolute secondary loop ID
		//INTEGER     :: SecondIndex                 ! Index to secondary loop attached to rack
		int WalkInID; // Index to absolute walk-in ID
		int WalkInIndex; // Index to walk-in attached to rack
		int NumCases; // Total number of refrigerated cases attached to rack
		Real64 COPFTempOutput; // Curve value for COPFTemp curve object
		Real64 CondenserFrac; // Fraction of condenser power as a function of outdoor temperature
		Real64 TotalHeatRejectedToZone; // Total compressor and condenser fan heat rejected to zone (based on CaseRAFactor)
		int HeatRejectZoneNum; // Index to zone where heat is rejected
		int HeatRejectZoneNodeNum; // Index to zone where heat is rejected
		Real64 OutWbTemp; // Outdoor wet bulb temp at condenser air inlet node [C]
		Real64 OutDbTemp; // Outdoor dry bulb temp at condenser air inlet node [C]
		Real64 EffectTemp; // Effective outdoor temp when using evap condenser cooling [C]
		Real64 HumRatIn; // Humidity ratio of inlet air to condenser [kg/kg]
		Real64 HumRatOut; // Humidity ratio of outlet air from condenser (assumed saturated) [kg/kg]
		Real64 BPress; // Barometric pressure at condenser air inlet node [Pa]
		bool EvapAvail; // Control for evap condenser availability
		static Real64 LocalTimeStep( 0.0 ); // TimeStepZone for case/walkin systems, TimeStepSys for coil systems
		static int CoilSetIndex( 0 ); // Index to set of coils in a single zone
		static int CoilSetID( 0 ); // Index to set of coils in a single zone (shared inlet and outlet nodes)
		static int CoilIndex( 0 ); // Index to a single air chiller/coil
		static int CoilID( 0 ); // Index to a single air chiller/coil

		NumCases = RefrigRack( RackNum ).NumCases;
		TotalRackDeliveredCapacity = 0.0;
		CompressorCOPactual = 0.0;
		TotalCompressorPower = 0.0;
		TotalCondenserFanPower = 0.0;
		TotalCondenserPumpPower = 0.0;
		TotalBasinHeatPower = 0.0;
		TotalCondenserHeat = 0.0;
		TotalHeatRejectedToZone = 0.0;
		TotalEvapWaterUseRate = 0.0;
		RackSenCreditToZone = 0.0;
		RackSenCreditToHVAC = 0.0;
		CondenserFrac = 0.0;
		EvapAvail = true;
		HeatRejectZoneNum = 0;
		HeatRejectZoneNodeNum = 0;

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		//Loads for chiller sets are set in call to zone equipment element "SimAirChillerSet"
		// (all chiller coils within a set are located in the same zone)
		// (note non-zone, such as refrigeration, and zone equip, such as airchillersets, called at diff times)
		// Loads are then defined for each chiller coil within the set in "CalculateAirChillerSet"
		// In that subroutine, dispatch coils within each set in order specified for each zone
		//  Below will assign loads to refrigeration system or secondary loop
		//Note that this routine will go through all refrigeration systems, but loads for multiple systems
		// with interactions will not be known for the intitial calls with first HVAC time step. They will,
		// however, be repeated when the last chiller set is called from ZoneEquipmentManager
		// that's why important where init goes, don't want to zero out data should keep
		if ( UseSysTimeStep ) {
			for ( CoilSetIndex = 1; CoilSetIndex <= NumRefrigChillerSets; ++CoilSetIndex ) {
				CoilSetID = CoilSetIndex;
				CalculateAirChillerSets( CoilSetID );
			}
		}

		if ( RefrigRack( RackNum ).NumCoils > 0 ) {
			for ( CoilIndex = 1; CoilIndex <= RefrigRack( RackNum ).NumCoils; ++CoilIndex ) {
				CoilID = RefrigRack( RackNum ).CoilNum( CoilIndex );
				// already CALLed CalculateCoil(CoilID) in CoilSet specified order
				// increment TotalCoolingLoad for Compressors/condenser on each system
				TotalRackDeliveredCapacity += WarehouseCoil( CoilID ).TotalCoolingLoad;
				//      System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + WarehouseCoil(CoilID)%HotDefrostCondCredit
			} //NumCoils systems
		} //System(SysNum)%NumCoils > 0

		if ( NumCases > 0 ) {
			for ( CaseNum = 1; CaseNum <= NumCases; ++CaseNum ) {
				CaseID = RefrigRack( RackNum ).CaseNum( CaseNum );
				CalculateCase( CaseID );

				//   add evaporator load for all cases connected to rack
				TotalRackDeliveredCapacity += RefrigCase( CaseID ).TotalCoolingLoad;

				//   sensible and latent case credits already calculated in "CalculateCase"
				//   Now need to calculate amount of condenser heat rejection that should be applied to zone
				//                                     (used when HeatRejectionLocation = LocationZone)
				//   if walk-ins are served by rack, user must specify heat rejection zone and 100% of heat
				//   rejection goes to that zone - that is, no heat rejection goes to the HVAC return air
				if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
					if ( RefrigRack( RackNum ).NumWalkIns == 0 ) {
						TotalHeatRejectedToZone += RefrigCase( CaseID ).TotalCoolingLoad * ( 1.0 - CaseRAFactor );
						//  CaseRAFactor is a module variable calculated in CalculateCase
						//   find zone number of first case on rack (all cases are in the same zone
						//  if HeatRejectionLocation = LocationZone and no walk-ins)
						HeatRejectZoneNum = RefrigCase( RefrigRack( RackNum ).CaseNum( 1 ) ).ActualZoneNum;
						HeatRejectZoneNodeNum = RefrigCase( RefrigRack( RackNum ).CaseNum( 1 ) ).ZoneNodeNum;
					} else { // have walk ins so no reduction in condenser heat rejection for caseRA factor
						TotalHeatRejectedToZone += RefrigCase( CaseID ).TotalCoolingLoad;
					} // no walk ins
				}
			} //NumCases
		} //Numcases on rack > 0

		if ( RefrigRack( RackNum ).NumWalkIns > 0 ) {
			for ( WalkInIndex = 1; WalkInIndex <= RefrigRack( RackNum ).NumWalkIns; ++WalkInIndex ) {
				WalkInID = RefrigRack( RackNum ).WalkInNum( WalkInIndex );
				CalculateWalkIn( WalkInID );
				TotalRackDeliveredCapacity += WalkIn( WalkInID ).TotalCoolingLoad;
				if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
					TotalHeatRejectedToZone += WalkIn( WalkInID ).TotalCoolingLoad;
					HeatRejectZoneNum = RefrigRack( RackNum ).HeatRejectionZoneNum;
					HeatRejectZoneNodeNum = RefrigRack( RackNum ).HeatRejectionZoneNodeNum;
				} //reject heat to zone
			} //WalkInIndex
		} //NumWalkIns>0

		if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
			COPFTempOutput = CurveValue( RefrigRack( RackNum ).COPFTempPtr, Node( HeatRejectZoneNodeNum ).Temp );
			EvapAvail = false;
		} else {
			if ( RefrigRack( RackNum ).OutsideAirNodeNum != 0 ) {
				OutDbTemp = Node( RefrigRack( RackNum ).OutsideAirNodeNum ).Temp;
				BPress = Node( RefrigRack( RackNum ).OutsideAirNodeNum ).Press;
			} else {
				OutDbTemp = OutDryBulbTemp;
				BPress = OutBaroPress;
			}
			EffectTemp = OutDbTemp;

			// IF schedule exists, evap condenser can be scheduled OFF
			// Check schedule to determine evap condenser availability
			if ( RefrigRack( RackNum ).EvapSchedPtr > 0 && GetCurrentScheduleValue( RefrigRack( RackNum ).EvapSchedPtr ) == 0 ) EvapAvail = false;

			// Evaporative condensers will have their water flow shut off in cold months to avoid
			//  'spectacular' icing problems.  Ideally, the user will use the evaporative schedule input
			//  to set such a schedule.  However, sometimes, users will use a single input deck to model
			//  one building in multiple climates, and may not think to put in such a schedule in the colder
			//  climates.  To accomodate such applications, the variable EvapCutOutTdb is used as an extra
			//  check.
			if ( OutDbTemp < EvapCutOutTdb ) EvapAvail = false;

			if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && EvapAvail ) {
				// determine temps for evap cooling
				if ( RefrigRack( RackNum ).OutsideAirNodeNum != 0 ) {
					HumRatIn = Node( RefrigRack( RackNum ).OutsideAirNodeNum ).HumRat;
				} else {
					HumRatIn = OutHumRat;
				} //outsideairnode
				OutWbTemp = PsyTwbFnTdbWPb( OutDbTemp, HumRatIn, BPress );
				EffectTemp = OutWbTemp + ( 1.0 - RefrigRack( RackNum ).EvapEffect ) * ( OutDbTemp - OutWbTemp );
			} //evapAvail

			// Obtain water-cooled condenser inlet/outlet temps
			if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeWater ) {
				InletNode = RefrigRack( RackNum ).InletNode;
				OutletNode = RefrigRack( RackNum ).OutletNode;
				RefrigRack( RackNum ).InletTemp = Node( InletNode ).Temp;
				EffectTemp = Node( InletNode ).Temp + 5.0; // includes approach temp
				if ( RefrigRack( RackNum ).InletTemp < RefrigRack( RackNum ).InletTempMin ) {
					//   RefrigRack(RackNum)%LowTempWarn = RefrigRack(RackNum)%LowTempWarn +1
					if ( RefrigRack( RackNum ).LowTempWarnIndex == 0 ) {
						ShowWarningMessage( "Refrigeration:CompressorRack: " + RefrigRack( RackNum ).Name );
						ShowContinueError( "Water-cooled condenser inlet temp lower than minimum allowed temp. Check returning water temperature and/or minimum temperature setpoints." );
					} //LowTempWarnIndex
					ShowRecurringWarningErrorAtEnd( "Refrigeration Compressor Rack " + RefrigRack( RackNum ).Name + " - Condenser inlet temp lower than minimum allowed ... continues", RefrigRack( RackNum ).LowTempWarnIndex );
					//END IF  !LowTempWarn
				} //InletTempMin
			} //RefrigCondenserTypeWater

			COPFTempOutput = CurveValue( RefrigRack( RackNum ).COPFTempPtr, EffectTemp );
		} //Location Zone

		CompressorCOPactual = RefrigRack( RackNum ).RatedCOP * COPFTempOutput;

		if ( CompressorCOPactual > 0.0 ) {
			TotalCompressorPower = TotalRackDeliveredCapacity / CompressorCOPactual;
			TotalCondenserHeat = TotalCompressorPower + TotalRackDeliveredCapacity;
		} else {
			if ( ShowCOPWarning( RackNum ) ) {
				ShowWarningError( "Refrigeration:CompressorRack: " + RefrigRack( RackNum ).Name );
				ShowContinueError( " The calculated COP has a value of zero or is negative. Refer to Engineering Documentation for" );
				ShowContinueError( " further explanation of Compressor Rack COP as a Function of Temperature Curve." );
				ShowCOPWarning( RackNum ) = false;
			}
		}

		//calculate condenser fan usage here if not water-cooled; if water-cooled, fan is in separate tower object
		// fan loads > 0 only if the connected cases are operating
		if ( TotalRackDeliveredCapacity > 0.0 && RefrigRack( RackNum ).CondenserType != RefrigCondenserTypeWater ) {
			if ( RefrigRack( RackNum ).TotCondFTempPtr != 0 ) {
				if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
					CondenserFrac = max( 0.0, min( 1.0, CurveValue( RefrigRack( RackNum ).TotCondFTempPtr, Node( HeatRejectZoneNodeNum ).Temp ) ) );
					TotalCondenserFanPower = RefrigRack( RackNum ).CondenserFanPower * CondenserFrac;
					RefrigCaseCredit( HeatRejectZoneNum ).SenCaseCreditToZone += RefrigRack( RackNum ).CondenserFanPower * CondenserFrac;
				} else {
					CondenserFrac = max( 0.0, min( 1.0, CurveValue( RefrigRack( RackNum ).TotCondFTempPtr, EffectTemp ) ) );
					TotalCondenserFanPower = RefrigRack( RackNum ).CondenserFanPower * CondenserFrac;
				} //location zone
			} else {
				CondenserFrac = 1.0;
				TotalCondenserFanPower = RefrigRack( RackNum ).CondenserFanPower * CondenserFrac;
			} //TotCondFTempPtr
		} //Cooling Water type

		// calculate evap water use and water pump power, if applicable
		// assumes pump runs whenever evap cooling is available to minimize scaling
		if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap && EvapAvail ) {
			TotalCondenserPumpPower = RefrigRack( RackNum ).EvapPumpPower;
			HumRatOut = PsyWFnTdbTwbPb( EffectTemp, OutWbTemp, BPress );
			TotalEvapWaterUseRate = RefrigRack( RackNum ).CondenserAirFlowRate * CondenserFrac * PsyRhoAirFnPbTdbW( BPress, OutDbTemp, HumRatIn ) * ( HumRatOut - HumRatIn ) / RhoH2O( EffectTemp );
		} //evapAvail
		// calculate basin water heater load
		if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap ) {
			if ( ( TotalRackDeliveredCapacity == 0.0 ) && ( EvapAvail ) && ( OutDbTemp < RefrigRack( RackNum ).BasinHeaterSetPointTemp ) ) {
				TotalBasinHeatPower = max( 0.0, RefrigRack( RackNum ).BasinHeaterPowerFTempDiff * ( RefrigRack( RackNum ).BasinHeaterSetPointTemp - OutDbTemp ) );
				// provide warning if no heater power exists
				if ( TotalBasinHeatPower == 0.0 ) {
					//RefrigRack(RackNum)%EvapFreezeWarn = RefrigRack(RackNum)%EvapFreezeWarn + 1
					if ( RefrigRack( RackNum ).EvapFreezeWarnIndex == 0 ) {
						ShowWarningMessage( "Refrigeration Compressor Rack " + RefrigRack( RackNum ).Name + " - Evap cooling of condenser underway with no basin heater power" );
						ShowContinueError( "and condenser inlet air dry-bulb temp at or below the basin heater setpoint temperature." );
						ShowContinueErrorTimeStamp( "Continuing simulation." );
					} //EvapFreezeWarnIndex == 0
					ShowRecurringWarningErrorAtEnd( "Refrigeration Compressor Rack " + RefrigRack( RackNum ).Name + " - Evap cooling of condenser underway with no basin heater power ... continues", RefrigRack( RackNum ).EvapFreezeWarnIndex );
					//END IF
				} // TotalBasinHeatPower == 0 when at outdoor freezing conditions
			} // cap
		} //evap condenser type

		// add in compressor and condenser fan power to rack heat rejection variables if the heat rejection location is to the zone
		//   if walk-ins are served by rack, user must specify heat rejection zone and 100% of heat
		//   rejection goes to that zone - that is, no condenser heat rejection goes to the HVAC return air
		if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
			TotalCondenserHeat = TotalRackDeliveredCapacity + TotalCompressorPower + TotalCondenserFanPower;
			if ( HeatRejectZoneNum > 0 && TotalRackDeliveredCapacity > 0.0 ) {
				if ( RefrigRack( RackNum ).NumWalkIns == 0 ) {
					//       rack report variables for condenser heat to Zone and/or HVAC
					//       The difference between TotalHeatRejectedToZone and TotalRackDeliveredCapacity is the heat sent to return air
					RackSenCreditToZone = TotalCondenserHeat * ( TotalHeatRejectedToZone / TotalRackDeliveredCapacity );
					RackSenCreditToHVAC = TotalCondenserHeat - RackSenCreditToZone;
				} else { // walkins present and no rack heat rejection goes to return air
					RackSenCreditToZone = TotalCondenserHeat;
					RackSenCreditToHVAC = 0.0;
				} //walkins present
				//     Update globals for use in Air Heat Balance and Zone Equipment Manager
				RefrigCaseCredit( HeatRejectZoneNum ).SenCaseCreditToZone += RackSenCreditToZone;

				RefrigCaseCredit( HeatRejectZoneNum ).SenCaseCreditToHVAC += RackSenCreditToHVAC;
			} //zone # > 0 and tot del cap > 0
		} //rack heat rejection to zone

	}

	//***************************************************************************************************

	void
	ReportRackSystem( int const RackNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Hudson, ORNL Feb 2007, July 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To report compressor rack variables

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataWater::WaterStorage;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 LocalTimeStep( 0.0 ); // TimeStepZone for case/walkin systems, TimeStepSys for coil systems
		static int DemandARRID( 0 ); // Index to water tank Demand used for evap condenser on rack
		static int RackTankID( 0 ); // Index to water tank used for evap condenser on rack

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		RefrigRack( RackNum ).RackCompressorPower = TotalCompressorPower;
		RefrigRack( RackNum ).RackElecConsumption = TotalCompressorPower * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).ActualCondenserFanPower = TotalCondenserFanPower;
		RefrigRack( RackNum ).CondenserFanConsumption = TotalCondenserFanPower * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).RackCapacity = TotalRackDeliveredCapacity;
		RefrigRack( RackNum ).RackCoolingEnergy = TotalRackDeliveredCapacity * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).RackCompressorCOP = CompressorCOPactual;
		RefrigRack( RackNum ).SensHVACCreditHeatRate = RackSenCreditToHVAC;
		RefrigRack( RackNum ).SensHVACCreditHeat = RackSenCreditToHVAC * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).SensZoneCreditHeatRate = RackSenCreditToZone;
		RefrigRack( RackNum ).SensZoneCreditHeat = RackSenCreditToZone * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).EvapWaterConsumpRate = TotalEvapWaterUseRate;
		RefrigRack( RackNum ).EvapWaterConsumption = TotalEvapWaterUseRate * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).ActualEvapPumpPower = TotalCondenserPumpPower;
		RefrigRack( RackNum ).EvapPumpConsumption = TotalCondenserPumpPower * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).BasinHeaterPower = TotalBasinHeatPower;
		RefrigRack( RackNum ).BasinHeaterConsumption = TotalBasinHeatPower * LocalTimeStep * SecInHour;
		RefrigRack( RackNum ).CondLoad = TotalCondenserHeat;
		RefrigRack( RackNum ).CondEnergy = TotalCondenserHeat * LocalTimeStep * SecInHour;
		// Set total rack heat rejection used for heat reclaim. Do not allow heat reclaim on stand alone (indoor) display cases.
		if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationZone ) {
			HeatReclaimRefrigeratedRack( RackNum ).AvailCapacity = 0.0;
		} else {
			HeatReclaimRefrigeratedRack( RackNum ).AvailCapacity = TotalRackDeliveredCapacity * ( 1.0 + 1.0 / CompressorCOPactual );
		}

		//set water system demand request (if needed)
		if ( RefrigRack( RackNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
			DemandARRID = RefrigRack( RackNum ).EvapWaterTankDemandARRID;
			RackTankID = RefrigRack( RackNum ).EvapWaterSupTankID;
			WaterStorage( RackTankID ).VdotRequestDemand( DemandARRID ) = RefrigRack( RackNum ).EvapWaterConsumpRate;
		}

		SumZoneImpacts();

	}

	//***************************************************************************************************

	void
	CalculateCase( int const CaseID ) // Absolute pointer to refrigerated case
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad and Don Shirey, FSEC
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       Therese Stovall, ORNL, May 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To model refrigerated cases.

		// METHODOLOGY EMPLOYED:
		// Case performance is based on a latent component calculated using a user input curve object. The sensible
		// component is made up of all equipment loads (fan, light, anti-sweat) and the sensible case credit
		// calculated during initialization. A master schedule is used for the refrigerated case operation and
		// additional schedules control the lights and defrost operation.
		// The fan is assumed to be off for Hot-Gas and Electric defrost.

		// Unmet loads are accumulated to be met the following time step.  This usually occurs only during the
		// defrost period, so the case calls for full capacity at the end of defrost to make up for the sensible
		// case gains during the defrost period. This feature is also used if needed for restocking loads.

		// REFERENCES:

		// "Calculation of Humidity Effects on Energy Requirements of Refrigerated Display Cases",
		//  R. H. Howell, Ph. D., P.E., ASHRAE Paper, 3687 (CH-93-16-4) (RP-596)

		// "Effects of Store Relative Humidity on Refrigerated Display Case Performance",
		//  R. H. Howell, Ph. D., P.E., ASHRAE Paper, 3686 (CH-93-16-1) (RP-596)

		// "Analysis of Supermarket Dehumidification Alternatives",
		//  Electric Power Research Institute, EPRI TR-100352, Project 2891-03 Final Report, Nov. 1992.

		// "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
		//  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

		// Using/Aliasing
		using CurveManager::CurveValue;
		using namespace DataLoopNode;
		using DataEnvironment::OutBaroPress; // , Month
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyTdpFnWPb;

		// Locals
		static Real64 CaseRAFraction( 0.0 ); // Fraction of case credits applied to return air

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int ActualZoneNum( 0 ); // Index to zone
		static int DefCapCurvePtr( 0 );
		static int DefrostEnergyCurveType( 0 );
		static int DefrostType( 0 );
		static int ZoneNodeNum( 0 ); // Zone node number
		static Real64 CapAvail( 0.0 ); // capacity available to meet current and stored load (W)
		static Real64 CaseCreditFraction( 0.0 ); // Reduction in case credits due to e.g., reduced door openings at night
		static Real64 CaseSenCreditToZone( 0.0 ); // Amount of sensible case credit applied to zone load (W)
		static Real64 CaseLatCreditToZone( 0.0 ); // Amount of latent case credit applied to zone load (W)
		static Real64 CaseSenCreditToHVAC( 0.0 ); // Amount of sensible case credit applied to HVAC RA duct (W)
		static Real64 CaseLatCreditToHVAC( 0.0 ); // Amount of latent case credit applied to HVAC RA duct (W)
		static Real64 CaseSchedule( 0.0 ); // Current value of case operating (availability) schedule
		static Real64 DefrostEnergy( 0.0 ); // Energy form of defrost capacity (J)
		static Real64 DefrostSchedule( 0.0 ); // Display case defrost schedule
		static Real64 DefrostDripDownSchedule( 0.0 ); // Display case drip-down schedule (allows coil to drain after defrost)
		static Real64 DefCapModFrac( 0.0 ); // Defrost capacity modifier curve based on case operating temperature
		static Real64 DefrostRatio( 0.0 ); // ratio of defrost energy at current zone temp/humrat to defrost
		//    capacity at design condition
		static Real64 DefrostLoad_Actual( 0.0 ); // heat load on case due to defrost (W)
		static Real64 DefrostCap_Actual( 0.0 ); // power used to defrost (W)
		static Real64 DeltaFreezeKgFrost( 0.0 ); // change in frost on coils (kg)
		static Real64 DeltaStockingEnergy( 0.0 ); // Used to keep track of problems if sizing not consistent (J)
		static Real64 DeltaWarmEnvEnergy( 0.0 ); // Used to keep track of problems if sizing not consistent (J)
		static Real64 DesignRatedCap( 0.0 ); // Design rated capacity of display case (W)
		static Real64 DesignDefrostCap( 0.0 ); // Design defrost capacity of display case (W)
		static Real64 DesignLatentCap( 0.0 ); // Design latent capacity of display case (W)
		static Real64 DesignLighting( 0.0 ); // Total design display case lighting power (W)
		static Real64 FrostMeltedKg( 0.0 ); // Frost melted by defrost during a time step (kg)
		static Real64 LatentLoad( 0.0 ); // Latent load placed on case at actual zone conditions (W)
		static Real64 LatentRatio( 0.0 ); // ratio of latent capacity at current zone temp/humrat to
		//    latent capacity at design condition
		static Real64 LatentCap_Actual( 0.0 ); // Refrigerated case latent capacity at specific operating conditions
		static Real64 LatentCaseCredit( 0.0 ); // Latent case credit delivered to zone (W)
		static Real64 LatCapModFrac( 0.0 ); // Latent capacity modifier curve based on case operating temperature
		static Real64 LightingSchedule( 0.0 ); // Display case lighting schedule
		static Real64 Length( 0.0 ); // Length of display case (m)
		static Real64 LoadRequested( 0.0 ); // TotalLoad_Actual  + StoredEnergyRate
		static Real64 RatedAmbientRH( 0.0 ); // Local variable for the RH corresponding to case rating conditions
		static Real64 SensibleCaseCredit( 0.0 ); // Sensible case credit delivered to zone (W)
		static Real64 SensibleCap_Actual( 0.0 ); // Refrigerated case sensible capacity at specific operating conditions
		//REAL(r64)    :: SensibleFraction        =0.0d0 ! Portion of total load due to sensible load
		static Real64 SensibleLoadPrime( 0.0 ); // Sensible load due to cond, conv, rad, infil (W)
		static Real64 SensibleLoadAux( 0.0 ); // Sensible load due to heaters, lighting (W)
		static Real64 SensibleLoadTotal( 0.0 ); // Total sensible load on case, may not = capacity applied (W)
		static Real64 StockingSchedule( 0.0 ); // Current value of product stocking schedule (W/m)
		static Real64 StockingLoad( 0.0 ); // Total load due to stocking case product (W)
		static Real64 StoredEnergyRate( 0.0 ); // Rate needed to serve all stored energy during single time step (W)
		static Real64 TotalLoad_Actual( 0.0 ); // total load on case at zone conditions (W)
		static Real64 StartFrostKg( 0.0 ); // frost load at start of time step (kg of ice)
		static Real64 TotalCap_Actual( 0.0 ); // Refrigerated case total capacity at specific operating conditions
		static Real64 TotalLightingLoad( 0.0 ); // Total lighting energy rate (W)
		static Real64 TotalFan( 0.0 ); // Total fan energy rate (W)
		static Real64 TotalAntiSweat( 0.0 ); // Total anti-sweat heater energy rate (W)
		static Real64 TotalLightToCase( 0.0 ); // Lighting energy to case
		static Real64 TotalASHeaterToCase( 0.0 ); // Anti-sweat heater energy to case
		static Real64 TotalLightToZone( 0.0 ); // Lighting energy to zone
		static Real64 TotalASHeaterToZone( 0.0 ); // Anti-sweat heater energy to zone
		static Real64 TCase( 0.0 ); // Display case operating temperature
		static Real64 ZoneRHPercent( 0.0 ); // Zone relative humidity (%)
		static Real64 ZoneDewPoint( 0.0 ); // Zone dew point (C)
		static Real64 ZoneTempFactor( 0.0 ); // used to look at extra sensible load due to excursions in zone T

		// Refrigerated display case defrost type (parameters)
		// DefNone             = 0
		// DefOffCycle         = 1
		// DefHotFluid           = 2
		// DefHotFluidOnDemand   = 3 (not available)
		// DefHotFluidTerm       = 4
		// DefElectric         = 5
		// DefElectricOnDemand = 6 (not available)
		// DefElectricTerm     = 7

		//Initialize this case for this time step (
		//     All report variables prev set to zero for case when schedule for case is 'off')
		TotalCap_Actual = 0.0;
		LatentCap_Actual = 0.0;
		SensibleCap_Actual = 0.0;
		SensibleLoadTotal = 0.0;
		SensibleLoadPrime = 0.0;
		SensibleLoadAux = 0.0;
		DefrostLoad_Actual = 0.0;
		DefrostCap_Actual = 0.0;

		DefrostRatio = 0.0;
		LatentRatio = 0.0;
		StartFrostKg = 0.0;

		SensibleCaseCredit = 0.0;
		LatentCaseCredit = 0.0;

		CaseSenCreditToZone = 0.0;
		CaseLatCreditToZone = 0.0;
		CaseSenCreditToHVAC = 0.0;
		CaseLatCreditToHVAC = 0.0;
		CaseRAFactor = 0.0;

		TotalLightingLoad = 0.0;
		TotalAntiSweat = 0.0;
		TotalFan = 0.0;

		//Set local subroutine variables for convenience
		ActualZoneNum = RefrigCase( CaseID ).ActualZoneNum;
		ZoneNodeNum = RefrigCase( CaseID ).ZoneNodeNum;
		ZoneRHPercent = PsyRhFnTdbWPb( Node( ZoneNodeNum ).Temp, Node( ZoneNodeNum ).HumRat, OutBaroPress ) * 100.0;
		ZoneDewPoint = PsyTdpFnWPb( Node( ZoneNodeNum ).HumRat, OutBaroPress );
		Length = RefrigCase( CaseID ).Length;
		TCase = RefrigCase( CaseID ).Temperature;
		DesignRatedCap = RefrigCase( CaseID ).DesignRatedCap;
		DesignLatentCap = RefrigCase( CaseID ).DesignLatentCap;
		DesignDefrostCap = RefrigCase( CaseID ).DesignDefrostCap;
		DesignLighting = RefrigCase( CaseID ).DesignLighting;
		DefCapCurvePtr = RefrigCase( CaseID ).DefCapCurvePtr;
		DefrostEnergyCurveType = RefrigCase( CaseID ).DefrostEnergyCurveType;
		DefrostType = RefrigCase( CaseID ).DefrostType;
		RatedAmbientRH = RefrigCase( CaseID ).RatedAmbientRH;

		// GET ALL SCHEDULES (note all schedules can be fractions if on/off a portion of time step)
		// case schedule should be coincident with the zone time step otherwise the simulation proceeds
		CaseSchedule = GetCurrentScheduleValue( RefrigCase( CaseID ).SchedPtr );
		if ( CaseSchedule <= 0 ) return;
		// get defrost schedule
		if ( DefrostType > DefNone ) {
			DefrostSchedule = GetCurrentScheduleValue( RefrigCase( CaseID ).DefrostSchedPtr );
			DefrostDripDownSchedule = GetCurrentScheduleValue( RefrigCase( CaseID ).DefrostDripDownSchedPtr );
			//next statement In case user doesn't understand concept of drip down schedule
			DefrostDripDownSchedule = max( DefrostDripDownSchedule, DefrostSchedule );
		} else {
			DefrostSchedule = 0.0;
			DefrostDripDownSchedule = 0.0;
		}
		// get product stocking schedule and load due to product stocking, if no schedule exists load is 0
		if ( RefrigCase( CaseID ).StockingSchedPtr > 0 ) {
			StockingSchedule = GetCurrentScheduleValue( RefrigCase( CaseID ).StockingSchedPtr );
		} else {
			StockingSchedule = 0.0;
		}
		// get lighting schedule and total load due to lighting
		LightingSchedule = GetCurrentScheduleValue( RefrigCase( CaseID ).LightingSchedPtr );

		// if case credit reduction fraction schedule exists, modify both sensible and latent case credits
		// according to schedule - used to account for variable case envelope, such as night covers.
		if ( RefrigCase( CaseID ).CaseCreditFracSchedPtr != 0 ) {
			CaseCreditFraction = GetCurrentScheduleValue( RefrigCase( CaseID ).CaseCreditFracSchedPtr );
		} else {
			CaseCreditFraction = 1.0;
		}

		// CALCULATE AUX LOADS DUE TO LIGHTS, FAN AND STOCKING
		TotalLightingLoad = DesignLighting * LightingSchedule;
		TotalLightToCase = TotalLightingLoad * RefrigCase( CaseID ).LightingFractionToCase;
		TotalLightToZone = TotalLightingLoad - TotalLightToCase;
		// cycle fan according to defrost schedule
		// turn fan on for none or off-cycle defrost types
		if ( DefrostType == DefNone || DefrostType == DefOffCycle ) {
			TotalFan = RefrigCase( CaseID ).DesignFanPower;
		} else {
			TotalFan = RefrigCase( CaseID ).DesignFanPower * ( 1.0 - DefrostDripDownSchedule );
		}
		// get  load due to product stocking
		// accumulate stocking loads for reporting to help evaluate any cumulative unmet loads problems
		// only accumulate energy during actual simulation (so same if DD's are switched)
		StockingLoad = StockingSchedule * Length;
		if ( ! WarmupFlag ) {
			DeltaStockingEnergy = ( StockingLoad * TimeStepZoneSec );
			RefrigCase( CaseID ).StockingEnergy += DeltaStockingEnergy;
		} //warm up
		// CALCULTE ALL LOADS INFLUENCED BY ZONE TEMPERATURE AND RH
		// Anti-sweat heater capacity
		{ auto const SELECT_CASE_var( RefrigCase( CaseID ).AntiSweatControlType );
		if ( SELECT_CASE_var == ASNone ) {
			TotalAntiSweat = 0.0;
		} else if ( SELECT_CASE_var == ASConstant ) {
			TotalAntiSweat = RefrigCase( CaseID ).AntiSweatPower;
		} else if ( SELECT_CASE_var == ASLinear ) {
			TotalAntiSweat = RefrigCase( CaseID ).AntiSweatPower * min( 1.0, max( 0.0, 1.0 - ( RatedAmbientRH - ZoneRHPercent ) / ( RatedAmbientRH - RefrigCase( CaseID ).HumAtZeroAS ) ) );
			TotalAntiSweat = max( RefrigCase( CaseID ).MinimumASPower, TotalAntiSweat );
		} else if ( SELECT_CASE_var == ASDewPoint ) {
			TotalAntiSweat = RefrigCase( CaseID ).AntiSweatPower * min( 1.0, max( 0.0, ( ZoneDewPoint - TCase ) / ( RefrigCase( CaseID ).RatedAmbientDewPoint - TCase ) ) );
			TotalAntiSweat = max( RefrigCase( CaseID ).MinimumASPower, TotalAntiSweat );
		} else if ( SELECT_CASE_var == ASHeatBalance ) {
			if ( RefrigCase( CaseID ).Rcase > 0.0 ) {
				TotalAntiSweat = ( ( ( ZoneDewPoint - Node( ZoneNodeNum ).Temp ) * RefrigCase( CaseID ).Height / Rair ) + ( ( ZoneDewPoint - TCase ) * RefrigCase( CaseID ).Height / RefrigCase( CaseID ).Rcase ) );
				TotalAntiSweat = min( RefrigCase( CaseID ).AntiSweatPower, max( RefrigCase( CaseID ).MinimumASPower, TotalAntiSweat ) );
			} else {
				TotalAntiSweat = 0.0;
			}
		} else {
			// should never execute this CASE statement
			TotalAntiSweat = 0.0;
		}}
		TotalAntiSweat *= Length;
		TotalASHeaterToCase = RefrigCase( CaseID ).ASHeaterFractionToCase * TotalAntiSweat;
		TotalASHeaterToZone = TotalAntiSweat - TotalASHeaterToCase;

		// latent capacity correction term at off-design conditions
		{ auto const SELECT_CASE_var( RefrigCase( CaseID ).LatentEnergyCurveType );
		if ( SELECT_CASE_var == CaseTemperatureMethod ) {
			LatCapModFrac = CurveValue( RefrigCase( CaseID ).LatCapCurvePtr, TCase );
			LatentRatio = max( 0.0, ( 1.0 - ( RatedAmbientRH - ZoneRHPercent ) * LatCapModFrac ) );
		} else if ( SELECT_CASE_var == RHCubic ) {
			LatentRatio = max( 0.0, CurveValue( RefrigCase( CaseID ).LatCapCurvePtr, ZoneRHPercent ) );
		} else if ( SELECT_CASE_var == DPCubic ) {
			LatentRatio = max( 0.0, CurveValue( RefrigCase( CaseID ).LatCapCurvePtr, ZoneDewPoint ) );
		}}

		// calculate latent case load (assumes no moisture load due to stocking)
		// assume sensible case credits continue to accumulate in case during defrost/dripdown,
		//    but latent credits/load and capacity only applied outside dripdownschedule
		LatentLoad = DesignLatentCap * LatentRatio * CaseCreditFraction * ( 1.0 - DefrostDripDownSchedule );
		LatentCaseCredit = -LatentLoad;
		// adjust sensible loads and case credit for actual zone temperature
		// If zone temp rises above rated ambient temperature, total load can exceed case design capacity,
		// so unmet cooling loads are accumulated to meet in the next time step. (Case credit fraction allows
		//  extra insulation, e.g. night covers, or imitating a better insulated walk-in cooler)
		ZoneTempFactor = ( Node( ZoneNodeNum ).Temp - TCase ) / ( RefrigCase( CaseID ).RatedAmbientTemp - TCase );
		SensibleLoadPrime = RefrigCase( CaseID ).DesignSensCaseCredit * ZoneTempFactor * CaseCreditFraction;
		SensibleLoadAux = TotalLightToCase + TotalASHeaterToCase + TotalFan + StockingLoad;
		SensibleLoadTotal = SensibleLoadPrime + SensibleLoadAux;
		// include lighting and anti-sweat power not attributed to case load to sensible case credit
		SensibleCaseCredit = TotalLightToZone + TotalASHeaterToZone - SensibleLoadPrime;

		// FROST:  keep track of frost build up on evaporator coil
		//avoid accumulation during warm-up to avoid reverse dd test problem
		if ( ! WarmupFlag ) {
			DeltaFreezeKgFrost = LatentLoad * TimeStepZoneSec / IcetoVaporEnthalpy;
			RefrigCase( CaseID ).KgFrost += DeltaFreezeKgFrost;
		}

		if ( TCase > TempTooHotToFrost ) RefrigCase( CaseID ).KgFrost = 0.0;

		//DEFROST CALCULATIONS
		if ( DefrostSchedule > 0.0 ) {
			if ( DefrostType != DefNone && DefrostType != DefOffCycle ) {
				DefrostCap_Actual = DesignDefrostCap * DefrostSchedule;
				if ( DefrostType == DefElectricTerm || DefrostType == DefHotFluidTerm ) {
					// calculate correction term for temperature termination defrost control
					{ auto const SELECT_CASE_var( DefrostEnergyCurveType );
					if ( SELECT_CASE_var == CaseTemperatureMethod ) {
						DefCapModFrac = CurveValue( DefCapCurvePtr, TCase );
						DefrostRatio = max( 0.0, ( 1.0 - ( RatedAmbientRH - ZoneRHPercent ) * DefCapModFrac ) );
					} else if ( SELECT_CASE_var == RHCubic ) {
						DefrostRatio = max( 0.0, CurveValue( DefCapCurvePtr, ZoneRHPercent ) );
					} else if ( SELECT_CASE_var == DPCubic ) {
						DefrostRatio = max( 0.0, CurveValue( DefCapCurvePtr, ZoneDewPoint ) );
					} else if ( SELECT_CASE_var == None ) {
						DefrostRatio = 1.0;
					}}
					DefrostCap_Actual *= DefrostRatio;
				}
				StartFrostKg = RefrigCase( CaseID ).KgFrost;
				DefrostEnergy = DefrostCap_Actual * TimeStepZoneSec;
				FrostMeltedKg = min( DefrostEnergy / IceMeltEnthalpy, StartFrostKg );
				RefrigCase( CaseID ).KgFrost -= FrostMeltedKg;

				//Reduce defrost heat load on case by amount of ice melted during time step
				//However, don't reduce the defrost capacity applied

				DefrostLoad_Actual = DefrostCap_Actual - FrostMeltedKg * IceMeltEnthalpy / TimeStepZone / SecInHour;

				if ( ! WarmupFlag ) { //avoid reverse dd test problems
					// keep running total of defrost energy above that needed to melt frost for use in evaluating
					//      problems of excessive unmet loads
					RefrigCase( CaseID ).DeltaDefrostEnergy = max( 0.0, ( DefrostEnergy - ( FrostMeltedKg * IceMeltEnthalpy ) ) );
					RefrigCase( CaseID ).DefrostEnergy += RefrigCase( CaseID ).DeltaDefrostEnergy;
				}
				// If hot brine or hot gas is used for defrost, need to reduce condenser load
				// Note this condenser credit is not applied in compressor-rack systems.
				if ( DefrostType != DefElectric && DefrostType != DefElectricOnDemand && DefrostType != DefElectricTerm ) RefrigCase( CaseID ).HotDefrostCondCredit = DefrostCap_Actual * DefrostSchedule;
			} else { //no defrost or off-cycle defrost
				DefrostCap_Actual = 0.0;
				DefrostLoad_Actual = 0.0;
				RefrigCase( CaseID ).KgFrost = 0.0;
				// Off-Cycle defrost is assumed to melt all the ice
			} // defrost type

		} else { //DefrostSchedule = 0, so no defrost load or capacity
			DefrostLoad_Actual = 0.0;
			DefrostCap_Actual = 0.0;
		} //Defrost calculations

		//*** See if capacity meets load and manage accumulated stored energy ***********************************
		TotalLoad_Actual = SensibleLoadTotal + LatentLoad + DefrostLoad_Actual;
		StoredEnergyRate = RefrigCase( CaseID ).StoredEnergy / TimeStepZone / SecInHour;
		LoadRequested = TotalLoad_Actual + StoredEnergyRate;

		// prorate available cooling capacity for portion of time off due to drip down.
		CapAvail = DesignRatedCap * ( 1.0 - DefrostDripDownSchedule );
		if ( CapAvail >= LoadRequested ) {
			//Have more at least as much capacity available as needed, even counting stored energy
			TotalCap_Actual = LoadRequested;
			SensibleCap_Actual = SensibleLoadTotal + StoredEnergyRate;
			LatentCap_Actual = LatentLoad;
			RefrigCase( CaseID ).StoredEnergy = 0.0;
		} else {
			//Don't have as much capacity as needed (during dripdown or period following dripdown)
			TotalCap_Actual = CapAvail;
			LatentCap_Actual = min( LatentLoad, CapAvail ); //Latent load should never be > capavail, but just in case...
			SensibleCap_Actual = TotalCap_Actual - LatentCap_Actual;
			if ( ! WarmupFlag ) RefrigCase( CaseID ).StoredEnergy += ( TotalLoad_Actual - CapAvail ) * TimeStepZoneSec;
		} //CapAvail vs Load requested

		// Reset DefrostLoad_Actual to zero for non-electric defrost types, for reporting purposes
		if ( DefrostType != DefElectric && DefrostType != DefElectricOnDemand && DefrostType != DefElectricTerm ) DefrostCap_Actual = 0.0;

		CaseRAFraction = min( 0.8, RefrigCase( CaseID ).RAFrac );
		CaseRAFactor = ( 1.0 - ( ( 0.8 - CaseRAFraction ) / 0.8 ) ) * 0.5;

		// Update globals for use in ZoneTemperaturePredictorCorrector (Air Heat Balance) and
		//   Zone Equipment Manager. Sum case credits to zone and case credits to HVAC

		//** this needs a moisture variable NonAirSystemMoistureResponse (equivalent of NonAirSystemResponse) to properly
		//** allocate moisture to the zone when the HVAC system is off.

		CaseSenCreditToZone = SensibleCaseCredit * ( 1.0 - CaseRAFactor );
		CaseLatCreditToZone = LatentCaseCredit * ( 1.0 - CaseRAFactor );
		CaseSenCreditToHVAC = SensibleCaseCredit * CaseRAFactor;
		CaseLatCreditToHVAC = LatentCaseCredit * CaseRAFactor;

		RefrigCaseCredit( ActualZoneNum ).SenCaseCreditToZone += CaseSenCreditToZone;
		RefrigCaseCredit( ActualZoneNum ).LatCaseCreditToZone += CaseLatCreditToZone;
		RefrigCaseCredit( ActualZoneNum ).SenCaseCreditToHVAC += CaseSenCreditToHVAC;
		RefrigCaseCredit( ActualZoneNum ).LatCaseCreditToHVAC += CaseLatCreditToHVAC;

		// ReportRefrigeratedCase(CaseID)
		RefrigCase( CaseID ).TotalCoolingLoad = TotalCap_Actual;
		RefrigCase( CaseID ).TotalCoolingEnergy = TotalCap_Actual * TimeStepZoneSec;
		RefrigCase( CaseID ).SensCoolingEnergyRate = SensibleCap_Actual;
		RefrigCase( CaseID ).SensCoolingEnergy = SensibleCap_Actual * TimeStepZoneSec;
		RefrigCase( CaseID ).LatCoolingEnergyRate = LatentCap_Actual;
		RefrigCase( CaseID ).LatCoolingEnergy = LatentCap_Actual * TimeStepZoneSec;

		RefrigCase( CaseID ).SensZoneCreditRate = CaseSenCreditToZone; // both positive or negative
		// This rate can be positive or negative, split into separate output variables and always report positive value
		if ( CaseSenCreditToZone <= 0.0 ) {
			RefrigCase( CaseID ).SensZoneCreditCoolRate = -CaseSenCreditToZone;
			RefrigCase( CaseID ).SensZoneCreditCool = -CaseSenCreditToZone * TimeStepZoneSec;
			RefrigCase( CaseID ).SensZoneCreditHeatRate = 0.0;
			RefrigCase( CaseID ).SensZoneCreditHeat = 0.0;
		} else {
			RefrigCase( CaseID ).SensZoneCreditHeatRate = CaseSenCreditToZone;
			RefrigCase( CaseID ).SensZoneCreditHeat = CaseSenCreditToZone * TimeStepZoneSec;
			RefrigCase( CaseID ).SensZoneCreditCoolRate = 0.0;
			RefrigCase( CaseID ).SensZoneCreditCool = 0.0;
		}

		// This rate should always be negative
		RefrigCase( CaseID ).LatZoneCreditRate = CaseLatCreditToZone;
		RefrigCase( CaseID ).LatZoneCredit = CaseLatCreditToZone * TimeStepZoneSec;

		RefrigCase( CaseID ).SensHVACCreditRate = CaseSenCreditToHVAC;
		// This rate can be positive or negative, split into separate output variables and always report positive value
		if ( CaseSenCreditToHVAC <= 0.0 ) {
			RefrigCase( CaseID ).SensHVACCreditCoolRate = -CaseSenCreditToHVAC;
			RefrigCase( CaseID ).SensHVACCreditCool = -CaseSenCreditToHVAC * TimeStepZoneSec;
			RefrigCase( CaseID ).SensHVACCreditHeatRate = 0.0;
			RefrigCase( CaseID ).SensHVACCreditHeat = 0.0;
		} else {
			RefrigCase( CaseID ).SensHVACCreditHeatRate = CaseSenCreditToHVAC;
			RefrigCase( CaseID ).SensHVACCreditHeat = CaseSenCreditToHVAC * TimeStepZoneSec;
			RefrigCase( CaseID ).SensHVACCreditCoolRate = 0.0;
			RefrigCase( CaseID ).SensHVACCreditCool = 0.0;
		}

		// This rate should always be negative
		RefrigCase( CaseID ).LatHVACCreditRate = CaseLatCreditToHVAC;
		RefrigCase( CaseID ).LatHVACCredit = CaseLatCreditToHVAC * TimeStepZoneSec;

		RefrigCase( CaseID ).ElecFanPower = TotalFan;
		RefrigCase( CaseID ).ElecFanConsumption = TotalFan * TimeStepZoneSec;
		RefrigCase( CaseID ).ElecAntiSweatPower = TotalAntiSweat;
		RefrigCase( CaseID ).ElecAntiSweatConsumption = TotalAntiSweat * TimeStepZoneSec;
		RefrigCase( CaseID ).ElecLightingPower = TotalLightingLoad;
		RefrigCase( CaseID ).ElecLightingConsumption = TotalLightingLoad * TimeStepZoneSec;
		RefrigCase( CaseID ).ElecDefrostPower = DefrostCap_Actual;
		RefrigCase( CaseID ).ElecDefrostConsumption = DefrostCap_Actual * TimeStepZoneSec;

		RefrigCase( CaseID ).DefEnergyCurveValue = DefrostRatio;
		RefrigCase( CaseID ).LatEnergyCurveValue = LatentRatio;

		//**************************************************************************************************
		// Cap Energy and Kg Frost to avoid floating overflow errors
		// 1-time warning is issued. It should be rare but could happen with unrealistic inputs.

		//Collect extra sensible load above design for possible warning if that is determining factor in
		// excessively large stored energy
		if ( ( ZoneTempFactor * CaseCreditFraction ) > 1.0 ) {
			if ( ! WarmupFlag ) {
				DeltaWarmEnvEnergy = ( SensibleLoadPrime - RefrigCase( CaseID ).DesignSensCaseCredit ) * TimeStepZoneSec;
				RefrigCase( CaseID ).WarmEnvEnergy += DeltaWarmEnvEnergy;
			}
		}

		if ( RefrigCase( CaseID ).DefrostEnergy > MyLargeNumber ) RefrigCase( CaseID ).DefrostEnergy = MyLargeNumber;
		if ( RefrigCase( CaseID ).WarmEnvEnergy > MyLargeNumber ) RefrigCase( CaseID ).WarmEnvEnergy = MyLargeNumber;
		if ( RefrigCase( CaseID ).StockingEnergy > MyLargeNumber ) RefrigCase( CaseID ).StockingEnergy = MyLargeNumber;
		if ( RefrigCase( CaseID ).StoredEnergy > MyLargeNumber ) {
			RefrigCase( CaseID ).StoredEnergy = MyLargeNumber;
			if ( ShowStoreEnergyWarning( CaseID ) ) {
				ShowWarningError( "Refrigeration:Case: " + RefrigCase( CaseID ).Name );
				if ( RefrigCase( CaseID ).StockingEnergy >= RefrigCase( CaseID ).DefrostEnergy ) {
					if ( RefrigCase( CaseID ).StockingEnergy >= RefrigCase( CaseID ).WarmEnvEnergy ) {
						ShowContinueError( " This case has insufficient capacity to meet excess energy associated with stocking." );
						ShowContinueError( " Refer to documentation for further explanation of product stocking requirements and" );
						ShowContinueError( " Total Cooling Capacity." );
					} else {
						ShowContinueError( " This case has insufficient capacity to meet excess energy associated with a zone enviroment temperature greater than the design ambient for the case." );
						ShowContinueError( " Refer to documentation for further explanation of " );
						ShowContinueError( " Total Cooling Capacity." );
					} // Stocking energy > warm environment energy
				} else {
					if ( RefrigCase( CaseID ).DefrostEnergy >= RefrigCase( CaseID ).WarmEnvEnergy ) {
						ShowContinueError( " This case has insufficient capacity to meet excess energy associated with defrost." );
						ShowContinueError( " Refer to documentation for further explanation of defrost control requirements and" );
						ShowContinueError( " recommendations regarding Total Cooling Capacity, Sensible Heat Ratio, and Defrost Capacity." );
					} else {
						ShowContinueError( " This case has insufficient capacity to meet excess energy associated with a zone enviroment temperature greater than the design ambient for the case." );
						ShowContinueError( " Refer to documentation for further explanation of " );
						ShowContinueError( " Total Cooling Capacity." );
					} // defrost energy > warm environment energy
				} // stock > defrost ELSE
				ShowStoreEnergyWarning( CaseID ) = false; // only give this warning once for any one case
			} //showstoreenergy warning true
		} // stored energy > large number

		if ( RefrigCase( CaseID ).KgFrost > MyLargeNumber ) {
			RefrigCase( CaseID ).KgFrost = MyLargeNumber;
			if ( ShowFrostWarning( CaseID ) ) {
				ShowWarningError( "Refrigeration:Case: " + RefrigCase( CaseID ).Name );
				ShowContinueError( " This case has insufficient defrost capacity to remove the excess frost accumulation." );
				ShowContinueError( " Refer to documentation for further explanation of product stocking requirements and" );
				ShowContinueError( " recommendations regarding Total Cooling Capacity, Sensible Heat Ratio, and Latent Heat Ratio." );
				ShowFrostWarning( CaseID ) = false;
			}
		}

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimRefrigCondenser(
		int const SysType,
		std::string const & CompName,
		int & CompIndex,
		bool const FirstHVACIteration,
		bool const InitLoopEquip
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Randy Hudson, ORNL
		//       DATE WRITTEN   July 2007
		//       MODIFIED       Therese Stovall, ORNL May 2008
		//                      Brent Griffith, NREL Oct 2010, generalize fluid properties
		//                        plant upgrades, moved where called from to SimPlantEquip from ManageNonZoneEquipment
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates the water-cooled refrigeration condenser object.
		// Modified to add condensers for detailed refrigeration systems and to
		// avoid double-counting heat rejection that has been used in desuperheater
		// hvac coils or water heaters.

		// METHODOLOGY EMPLOYED:
		// Called from SimPlantEquip in PlantLoopEquipment , previously was called from Non-Zone Equipment Manager
		// Flow is requested and the actual available flow is set.  The outlet temperature is calculated.

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataPlant::TypeOf_RefrigSystemWaterCondenser;
		using DataPlant::TypeOf_RefrigerationWaterCoolRack;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "SimRefrigCondenser" );
		static Real64 DeltaT( 0.0 );
		static Real64 InletTemp( 0.0 );
		static Real64 DesVolFlowRate( 0.0 );
		static Real64 MassFlowRate( 0.0 );
		static Real64 MassFlowRateMax( 0.0 );
		static Real64 OutletTempMax( 0.0 );
		static Real64 VolFlowRate( 0.0 );
		static Real64 OutletTemp( 0.0 );
		static int FlowType( 0 );
		// INTEGER   :: HighFlowWarn = 0
		// INTEGER   :: HighTempWarn = 0
		static int NoFlowWarnIndex( 0 );
		static int HighFlowWarnIndex( 0 );
		static int HighInletWarnIndex( 0 );
		static int HighTempWarnIndex( 0 );
		static std::string Name;
		static std::string TypeName;
		static std::string ErrIntro;
		int PlantInletNode( 0 ); //Autodesk:Init
		int PlantOutletNode( 0 );
		int PlantLoopIndex( 0 ); //Autodesk:Init
		int PlantLoopSideIndex( 0 );
		int PlantBranchIndex( 0 );
		int PlantCompIndex( 0 );
		int Num( 0 ); // local index //Autodesk:Init
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat

		if ( CompIndex == 0 ) {
			{ auto const SELECT_CASE_var( SysType );
			if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
				Num = FindItemInList( CompName, RefrigRack );

			} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
				Num = FindItemInList( CompName, Condenser );
			} else {
				ShowFatalError( "SimRefrigCondenser: invalid system type passed" );
			}}

			if ( Num == 0 ) {
				ShowFatalError( "SimRefrigCondenser: Specified refrigeration condenser not Valid =" + CompName );
			}
			CompIndex = Num;
		} else {
			Num = CompIndex;

			{ auto const SELECT_CASE_var( SysType );
			if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
				if ( Num > NumRefrigeratedRacks || Num < 1 ) {
					ShowFatalError( "SimRefrigCondenser: Invalid CompIndex passed=" + TrimSigDigits( Num ) + ", Number of Units=" + TrimSigDigits( NumRefrigeratedRacks ) + ", Entered Unit name=" + CompName );
				}
				if ( CheckEquipNameRackWaterCondenser( Num ) ) {
					if ( CompName != RefrigRack( Num ).Name ) {
						ShowFatalError( "SimRefrigCondenser: Invalid CompIndex passed=" + TrimSigDigits( Num ) + ", Entered Unit name=" + CompName + ", stored Unit name for that index=" + RefrigRack( Num ).Name );
					}
					CheckEquipNameRackWaterCondenser( Num ) = false;
				}

			} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
				if ( Num > NumRefrigCondensers || Num < 1 ) {
					ShowFatalError( "SimRefrigCondenser: Invalid CompIndex passed=" + TrimSigDigits( Num ) + ", Number of Units=" + TrimSigDigits( NumRefrigCondensers ) + ", Entered Unit name=" + CompName );
				}
				if ( CheckEquipNameWaterCondenser( Num ) ) {
					if ( CompName != Condenser( Num ).Name ) {
						ShowFatalError( "SimRefrigCondenser: Invalid CompIndex passed=" + TrimSigDigits( Num ) + ", Entered Unit name=" + CompName + ", stored Unit name for that index=" + Condenser( Num ).Name );
					}
					CheckEquipNameWaterCondenser( Num ) = false;
				}

			}}
		}

		// this next block may not be necessary, should only get called from plant now.
		//  SELECT CASE (SysType)
		//   CASE (TypeOf_RefrigerationWaterCoolRack)
		//       IF(RefrigRack(Num)%CondenserType/=RefrigCondenserTypeWater) RETURN
		//   CASE (TypeOf_RefrigSystemWaterCondenser)
		//       IF(Condenser(Num)%CondenserType/=RefrigCondenserTypeWater) RETURN
		//  END SELECT
		// Return if not water cooled condenser

		if ( InitLoopEquip ) {
			InitRefrigeration();
			InitRefrigerationPlantConnections();
			return;
		}

		InitRefrigerationPlantConnections();

		//set variables depending upon system type
		{ auto const SELECT_CASE_var( SysType );
		if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
			PlantInletNode = RefrigRack( Num ).InletNode;
			PlantOutletNode = RefrigRack( Num ).OutletNode;
			PlantLoopIndex = RefrigRack( Num ).PlantLoopNum;
			PlantLoopSideIndex = RefrigRack( Num ).PlantLoopSideNum;
			PlantBranchIndex = RefrigRack( Num ).PlantBranchNum;
			PlantCompIndex = RefrigRack( Num ).PlantCompNum;

			TotalCondenserHeat = HeatReclaimRefrigeratedRack( Num ).AvailCapacity - RefrigRack( Num ).LaggedUsedWaterHeater - RefrigRack( Num ).LaggedUsedHVACCoil;
			FlowType = RefrigRack( Num ).FlowType;
			InletTemp = RefrigRack( Num ).InletTemp;
			// HighFlowWarn = RefrigRack(Num)%HighFlowWarn
			// HighTempWarn = RefrigRack(Num)%HighTempWarn
			DesVolFlowRate = RefrigRack( Num ).DesVolFlowRate;

			//DSU? init mass flow here?
			//     MassFlowRate = RefrigRack(Num)%MassFlowRate
			MassFlowRateMax = RefrigRack( Num ).MassFlowRateMax;
			OutletTempMax = RefrigRack( Num ).OutletTempMax;
			Name = RefrigRack( Num ).Name;
			TypeName = "Refrigeration:CompressorRack:";
			ErrIntro = "Condenser for refrigeration rack ";
			NoFlowWarnIndex = RefrigRack( Num ).NoFlowWarnIndex;
			HighFlowWarnIndex = RefrigRack( Num ).HighFlowWarnIndex;
			HighTempWarnIndex = RefrigRack( Num ).HighTempWarnIndex;
			HighInletWarnIndex = RefrigRack( Num ).HighInletWarnIndex;
		} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
			//InletNode = Condenser(Num)%InletNode
			PlantInletNode = Condenser( Num ).InletNode;
			PlantOutletNode = Condenser( Num ).OutletNode;
			PlantLoopIndex = Condenser( Num ).PlantLoopNum;
			PlantLoopSideIndex = Condenser( Num ).PlantLoopSideNum;
			PlantBranchIndex = Condenser( Num ).PlantBranchNum;
			PlantCompIndex = Condenser( Num ).PlantCompNum;

			TotalCondenserHeat = Condenser( Num ).CondLoad;
			FlowType = Condenser( Num ).FlowType;
			InletTemp = Condenser( Num ).InletTemp;
			// HighFlowWarn = Condenser(Num)%HighFlowWarn
			// HighTempWarn = Condenser(Num)%HighTempWarn
			DesVolFlowRate = Condenser( Num ).DesVolFlowRate;
			//     MassFlowRate = Condenser(Num)%MassFlowRate
			MassFlowRateMax = Condenser( Num ).MassFlowRateMax;
			OutletTempMax = Condenser( Num ).OutletTempMax;
			Name = Condenser( Num ).Name;
			TypeName = "Refrigeration:Condenser:WaterCooled";
			ErrIntro = "Condenser for refrigeration system ";
			NoFlowWarnIndex = Condenser( Num ).NoFlowWarnIndex;
			HighFlowWarnIndex = Condenser( Num ).HighFlowWarnIndex;
			HighTempWarnIndex = Condenser( Num ).HighTempWarnIndex;
			HighInletWarnIndex = Condenser( Num ).HighInletWarnIndex;
		} else {
			assert( false );
		}}

		// Current condenser is water cooled
		// Make demand request on first HVAC iteration

		//get cooling fluid properties
		rho = GetDensityGlycol( PlantLoop( PlantLoopIndex ).FluidName, InletTemp, PlantLoop( PlantLoopIndex ).FluidIndex, RoutineName );
		Cp = GetSpecificHeatGlycol( PlantLoop( PlantLoopIndex ).FluidName, InletTemp, PlantLoop( PlantLoopIndex ).FluidIndex, RoutineName );

		// first determine desired flow
		if ( FlowType == VariableFlow && TotalCondenserHeat > 0.0 ) {
			if ( SysType == TypeOf_RefrigerationWaterCoolRack ) {
				OutletTemp = GetCurrentScheduleValue( RefrigRack( Num ).OutletTempSchedPtr );
			} else if ( SysType == TypeOf_RefrigSystemWaterCondenser ) {
				OutletTemp = GetCurrentScheduleValue( Condenser( Num ).OutletTempSchedPtr );
			}
			if ( OutletTemp == InletTemp ) {

				if ( HighInletWarnIndex == 0 ) {
					ShowSevereError( ErrIntro + ", \"" + Name + "\" : has inlet water temp equal to desired outlet temp. Excessive flow resulting. " );
					ShowContinueError( "cooling water is not cold enough to reach desired outlet temperature" );

				}
				ShowRecurringWarningErrorAtEnd( ErrIntro + ", \"" + Name + "\" : has inlet water temp equal to desired outlet temp.... continues. ", HighInletWarnIndex );
				VolFlowRate = 9999.0;
				MassFlowRate = VolFlowRate * rho;
			} else {
				DeltaT = OutletTemp - InletTemp;
				MassFlowRate = TotalCondenserHeat / Cp / DeltaT;
				// Check for maximum flow in the component
				if ( MassFlowRate > MassFlowRateMax ) {
					//HighFlowWarn = HighFlowWarn +1
					if ( HighFlowWarnIndex == 0 ) {
						ShowWarningMessage( TypeName + Name );
						ShowContinueError( "Requested condenser water mass flow rate greater than maximum allowed value. " );
						ShowContinueError( "Flow reset to maximum value." );
					} //HighFlowWarnIndex
					ShowRecurringWarningErrorAtEnd( ErrIntro + Name + " - Flow rate higher than maximum allowed ... continues", HighFlowWarnIndex );
					//END IF
					MassFlowRate = MassFlowRateMax;
				}
			} //compare outlet T to inlet T

		} else if ( FlowType == ConstantFlow && TotalCondenserHeat > 0.0 ) {
			// this part for constant flow condition
			VolFlowRate = DesVolFlowRate;
			MassFlowRate = VolFlowRate * rho;

		} else if ( TotalCondenserHeat == 0.0 ) {
			MassFlowRate = 0.0;

		} //on flow type
		// check against plant, might get changed.
		SetComponentFlowRate( MassFlowRate, PlantInletNode, PlantOutletNode, PlantLoopIndex, PlantLoopSideIndex, PlantBranchIndex, PlantCompIndex );

		VolFlowRate = MassFlowRate / rho;

		if ( MassFlowRate > 0 ) {
			OutletTemp = TotalCondenserHeat / ( MassFlowRate * Cp ) + Node( PlantInletNode ).Temp;
		} else {
			OutletTemp = InletTemp;
			if ( ( TotalCondenserHeat > 0.0 ) && ( ! FirstHVACIteration ) ) {

				ShowRecurringWarningErrorAtEnd( TypeName + Name + "Water-cooled condenser has no cooling water flow. Heat is not being rejected from compressor rack condenser.", NoFlowWarnIndex );
			}
		}
		// Check outlet water temp for max value
		if ( OutletTemp > OutletTempMax ) {
			// HighTempWarn = HighTempWarn +1
			if ( HighTempWarnIndex == 0 ) {
				ShowWarningMessage( TypeName + Name );
				ShowContinueError( "Water-cooled condenser outlet temp higher than maximum allowed temp. Check flow rates and/or temperature setpoints." );
			}
			ShowRecurringWarningErrorAtEnd( ErrIntro + Name + " - Condenser outlet temp higher than maximum allowed ... continues", HighTempWarnIndex );
		}

		//set up output variables
		{ auto const SELECT_CASE_var( SysType );
		if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
			//RefrigRack(Num)%HighFlowWarn = HighFlowWarn
			//RefrigRack(Num)%HighTempWarn = HighTempWarn
			RefrigRack( Num ).MassFlowRate = MassFlowRate;
			RefrigRack( Num ).VolFlowRate = VolFlowRate;
			RefrigRack( Num ).OutletTemp = OutletTemp;
			RefrigRack( Num ).HighFlowWarnIndex = HighFlowWarnIndex;
			RefrigRack( Num ).HighTempWarnIndex = HighTempWarnIndex;
			RefrigRack( Num ).HighInletWarnIndex = HighInletWarnIndex;
			RefrigRack( Num ).NoFlowWarnIndex = NoFlowWarnIndex;
		} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
			//Condenser(Num)%HighFlowWarn = HighFlowWarn
			//Condenser(Num)%HighTempWarn = HighTempWarn
			Condenser( Num ).MassFlowRate = MassFlowRate;
			Condenser( Num ).VolFlowRate = VolFlowRate;
			Condenser( Num ).OutletTemp = OutletTemp;
			Condenser( Num ).HighFlowWarnIndex = HighFlowWarnIndex;
			Condenser( Num ).HighTempWarnIndex = HighTempWarnIndex;
			Condenser( Num ).NoFlowWarnIndex = NoFlowWarnIndex;
			Condenser( Num ).HighInletWarnIndex = HighInletWarnIndex;
		}}

		UpdateRefrigCondenser( Num, SysType );

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	UpdateRefrigCondenser(
		int const Num,
		int const SysType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Randy Hudson, ORNL
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the node variables with local variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataLoopNode::Node;
		using PlantUtilities::SafeCopyPlantNode;
		using DataPlant::TypeOf_RefrigerationWaterCoolRack;
		using DataPlant::TypeOf_RefrigSystemWaterCondenser;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:
		{ auto const SELECT_CASE_var( SysType );
		if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
			InletNode = RefrigRack( Num ).InletNode;
			OutletNode = RefrigRack( Num ).OutletNode;
		} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
			InletNode = Condenser( Num ).InletNode;
			OutletNode = Condenser( Num ).OutletNode;
		}}

		// Pass all variables from inlet to outlet node
		SafeCopyPlantNode( InletNode, OutletNode ); //  Node(OutletNode) = Node(InletNode)

		// Set outlet node variables that are possibly changed
		{ auto const SELECT_CASE_var( SysType );
		if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
			Node( OutletNode ).Temp = RefrigRack( Num ).OutletTemp;
		} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
			Node( OutletNode ).Temp = Condenser( Num ).OutletTemp;
		}}

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimulateDetailedRefrigerationSystems()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
		//       DATE WRITTEN   Spring 2008
		//       Based upon ManageRefrigeratedCaseRacks by Richard Raustad, FSEC
		//          Oct/Nov 2004, and MODIFIED by Shirey, FSEC Dec 2004
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called to simulate detailed refrigeration systems

		// METHODOLOGY EMPLOYED:
		// Each refrigeration system is modeled by first simulating the attached refrigerated cases.  The sum
		// of the total heat transfer for all attached cases determines the load on the compressor rack.
		// Iterations are used here to account for load transfer between independent refrigeration systems
		// via mechanical subcoolers.
		// The logical variable, UseSysTimeStep, determines whether we are evaluating only systems driven by
		// ZoneEquipmentManager on the system time step, or only system driven by HVACManager on the zone time step.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "SimulateDetailedRefrigerationSystems" );
		int SysNum; // Index to the detailed refrigerated system being modeled
		static bool DeRate( false ); // If true, need to derate aircoils because load can't be met by system
		static bool FirstSCLoop( true ); // Flag first time through multi-system loop used when mech subcoolers present

		static int StartMechSubcoolLoop( 3 ); // if no mechanical subcoolers transfer energy between system,
		//    don't loop
		static int LoopNum( 0 ); // Index to overall repeat necessary for mechanical subcoolers
		static int SubcoolID( 0 ); // Subcooler ID number
		static int SubcoolerIndex( 0 ); // Subcooler ID number
		static int CaseID( 0 ); // Absolute reference to case
		static int CaseIndex( 0 ); // Index to case
		static int CoilID( 0 ); // Index to a single air chiller/coil
		static int CoilIndex( 0 ); // Index to a single air chiller/coil
		static int CoilSetID( 0 ); // Index to set of coils in a single zone (shared inlet and outlet nodes)
		static int CoilSetIndex( 0 ); // Index to set of coils in a single zone
		static int CondInletAirZoneNum( 0 ); // Index used to assign zone credits
		static int SecondID( 0 ); // Absolute reference to Secondary Loop
		static int SecondIndex( 0 ); // Index to Secondary Loop
		static int SuctionPipeActualZoneNum( 0 ); // Index to zone exchanging heat with suction pipes
		static int WalkInID( 0 ); // Absolute reference to WalkIn
		static int WalkInIndex( 0 ); // Index to WalkIn
		static int ZoneNum( 0 ); // counter when assigning zone case credits
		static int CascadeLoadIndex( 0 ); // Index to Cascade Condenser Load
		static int CascadeLoadID( 0 ); // Absolute reference to Cascade Condenser
		static Real64 LoadFrac( 1.0 ); // case load/design case load
		static Real64 LocalTimeStep( 0.0 ); // Set equal to either TimeStepSys or TimeStepZone
		static Real64 CurrentLoads( 0.0 ); // current loads on compressor, exclusive of unmet loads from prev time steps
		static Real64 CurrentHiStageLoads( 0.0 ); // Current loads on high-stage compressor, exclusive of unmet loads from
		// prev time steps (two-stage systems only)
		static Real64 MaxTEvap( 0.0 ); // Maximum evaporating temperature that can still meet load
		static Real64 MaxDelTFloatFrac( 0.5 ); // max fraction allowed for difference between case and evaporator temperature
		//    relative to design temperature difference
		Real64 SuctionPipeZoneTemp; // Temperature for zone identified as environment for suction pipe heat gains, C

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		//Cascade condenser assumes a constant approach delta T (Tcond - Tevap), not f(load)

		//Loads for chiller sets are set in call to zone equipment element "SimAirChillerSet"
		// (all chiller coils within a set are located in the same zone)
		// (note non-zone, such as refrigeration, and zone equip, such as airchillersets, called at diff times)
		// Loads are then defined for each chiller coil within the set in "CalculateAirChillerSet"
		// In that subroutine, dispatch coils within each set in order specified for each zone
		//  Below will assign loads to refrigeration system or secondary loop
		//Note that this routine will go through all refrigeration systems, but loads for multiple systems
		// with interactions will not be known for the intitial calls with first HVAC time step. They will,
		// however, be repeated when the last chiller set is called from ZoneEquipmentManager
		// that's why important where init goes, don't want to zero out data should keep
		if ( UseSysTimeStep ) {
			for ( CoilSetIndex = 1; CoilSetIndex <= NumRefrigChillerSets; ++CoilSetIndex ) {
				CoilSetID = CoilSetIndex;
				CalculateAirChillerSets( CoilSetID );
			}
		}

		//Do refrigeration system loop outside of iterative solution to initialize time step and
		//  calculate case, walk-in, and secondary loop loads (that won't change during balance
		//  of refrigeration system iterations) and prepare initial estimates for the iterative system solution
		for ( SysNum = 1; SysNum <= NumRefrigSystems; ++SysNum ) {
			//Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
			if ( ( ( ! UseSysTimeStep ) && ( ! System( SysNum ).CoilFlag ) ) || ( ( UseSysTimeStep ) && ( System( SysNum ).CoilFlag ) ) ) {
				if ( System( SysNum ).NumCases > 0 ) {
					for ( CaseIndex = 1; CaseIndex <= System( SysNum ).NumCases; ++CaseIndex ) {
						CaseID = System( SysNum ).CaseNum( CaseIndex );
						CalculateCase( CaseID );
						//  TevapDesign calc in Get Input to meet lowest evap temp of any load on the system.
						//  Tevap needed is either fixed at this design value,
						//  or allowed to float to meet lowest T needed among all loads served by the system
						//  (Floating Tevap = Design Tevap unless load <= Design cap)
						if ( System( SysNum ).CompSuctControl == ConstantSuctionTemperature ) {
							System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
						} else { // calculate floating T evap
							LoadFrac = min( 1.0, ( RefrigCase( CaseID ).TotalCoolingLoad / RefrigCase( CaseID ).DesignRatedCap ) );
							MaxTEvap = RefrigCase( CaseID ).Temperature - ( RefrigCase( CaseID ).Temperature - RefrigCase( CaseID ).EvapTempDesign ) * max( LoadFrac, MaxDelTFloatFrac );
							//Compare Tevap for this case to max allowed for all previous cases on this suction group and set at the MINIMUM of the two
							if ( CaseIndex == 1 ) { //note use case index, not caseid here to get first case on this suction group/system
								System( SysNum ).TEvapNeeded = MaxTEvap;
							} else {
								System( SysNum ).TEvapNeeded = min( MaxTEvap, System( SysNum ).TEvapNeeded );
							}
						} //floating or constant evap temperature
						// increment TotalCoolingLoad for Compressors/condenser on each system and defrost condenser credits for heat recovery
						System( SysNum ).TotalCoolingLoad += RefrigCase( CaseID ).TotalCoolingLoad;
						System( SysNum ).TotalCondDefrostCredit += RefrigCase( CaseID ).HotDefrostCondCredit;
					} //NumCases
				} //Num of cases > 0

				if ( System( SysNum ).NumWalkIns > 0 ) {
					for ( WalkInIndex = 1; WalkInIndex <= System( SysNum ).NumWalkIns; ++WalkInIndex ) {
						WalkInID = System( SysNum ).WalkInNum( WalkInIndex );
						CalculateWalkIn( WalkInID );
						if ( System( SysNum ).CompSuctControl == ConstantSuctionTemperature ) {
							System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
						} else { // calculate floating T evap
							LoadFrac = min( 1.0, ( WalkIn( WalkInID ).TotalCoolingLoad / WalkIn( WalkInID ).DesignRatedCap ) );
							MaxTEvap = WalkIn( WalkInID ).Temperature - ( WalkIn( WalkInID ).Temperature - WalkIn( WalkInID ).TEvapDesign ) * max( LoadFrac, MaxDelTFloatFrac );
							//  Compare maxTevap for this walk in to max allowed for cases and for all
							//  previous walk ins on this suction group and set at the MINIMUM of the two
							if ( WalkInIndex == 1 && System( SysNum ).NumCases == 0 ) {
								System( SysNum ).TEvapNeeded = MaxTEvap;
							} else {
								System( SysNum ).TEvapNeeded = min( MaxTEvap, System( SysNum ).TEvapNeeded );
							}
						} //floating or constant evap temperature
						// increment TotalCoolingLoad for Compressors/condenser on each system
						System( SysNum ).TotalCoolingLoad += WalkIn( WalkInID ).TotalCoolingLoad;
						System( SysNum ).TotalCondDefrostCredit += WalkIn( WalkInID ).HotDefrostCondCredit;
					} //NumWalkIns systems
				} //System(SysNum)%NumWalkIns > 0

				if ( System( SysNum ).NumCoils > 0 ) {
					for ( CoilIndex = 1; CoilIndex <= System( SysNum ).NumCoils; ++CoilIndex ) {
						CoilID = System( SysNum ).CoilNum( CoilIndex );
						// already CALLed CalculateCoil(CoilID) in CoilSet specified order
						if ( System( SysNum ).CompSuctControl == ConstantSuctionTemperature ) {
							System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
						} else { // calculate floating T evap
							// for now, override floating Tevap if coils on system, warning was printed in input to let user know
							System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
						} //floating or constant evap temperature
						// increment TotalCoolingLoad for Compressors/condenser on each system
						System( SysNum ).TotalCoolingLoad += WarehouseCoil( CoilID ).TotalCoolingLoad;
						System( SysNum ).TotalCondDefrostCredit += WarehouseCoil( CoilID ).HotDefrostCondCredit;
					} //NumCoils systems
				} //System(SysNum)%NumCoils > 0

				if ( System( SysNum ).NumSecondarys > 0 ) {
					for ( SecondIndex = 1; SecondIndex <= System( SysNum ).NumSecondarys; ++SecondIndex ) {
						SecondID = System( SysNum ).SecondaryNum( SecondIndex );
						CalculateSecondary( SecondID );
						if ( System( SysNum ).CompSuctControl == ConstantSuctionTemperature ) {
							System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
						} else { // check for lowest T evap design among the secondary systems and
							//  Compare Tevap for this second to max allowed for cases, walk ins, and
							//  for all previous secondary loops on this suction group and set
							//  at the MINIMUM (note secondary loops control capacity with
							//  brine flow rate, so don't float above their design evap temperature)
							if ( SecondIndex == 1 && System( SysNum ).NumNonCascadeLoads == 0 ) {
								System( SysNum ).TEvapNeeded = Secondary( SecondID ).TEvapDesign;
							} else {
								System( SysNum ).TEvapNeeded = min( Secondary( SecondID ).TEvapDesign, System( SysNum ).TEvapNeeded );
							}
						} //floating or constant evap temperature
						// increment TotalCoolingLoad for Compressors/condenser on each system
						System( SysNum ).SumSecondaryLoopLoad += Secondary( SecondID ).TotalCoolingLoad;
						System( SysNum ).TotalCondDefrostCredit += Secondary( SecondID ).HotDefrostCondCredit;
					} //NumSecondarys systems
				} //System(SysNum)%NumSecondarys > 0

				//add suction pipe heat gains (W) if input by user
				//Suction pipe heat gains aren't included in the reported total system load, but are heat gains that must be met in
				//  condenser and compressor loads. However, secondary dist piping and receiver gains are included
				//  in the total secondary system loads.
				System( SysNum ).PipeHeatLoad = 0.0;
				if ( System( SysNum ).SumUASuctionPiping > MySmallNumber ) {
					SuctionPipeZoneTemp = Node( System( SysNum ).SuctionPipeZoneNodeNum ).Temp;
					System( SysNum ).PipeHeatLoad = System( SysNum ).SumUASuctionPiping * ( SuctionPipeZoneTemp - System( SysNum ).TEvapNeeded );
					// pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
					//     from refrigcasecredit (- for cooling zone, + for heating zone)
					SuctionPipeActualZoneNum = System( SysNum ).SuctionPipeActualZoneNum;
					if ( UseSysTimeStep ) {
						CoilSysCredit( SuctionPipeActualZoneNum ).SenCreditToZoneRate -= System( SysNum ).PipeHeatLoad;
						CoilSysCredit( SuctionPipeActualZoneNum ).ReportSenCoolingToZoneRate = -CoilSysCredit( SuctionPipeActualZoneNum ).SenCreditToZoneRate;
					}
					//Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
					if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
						RefrigCaseCredit( SuctionPipeActualZoneNum ).SenCaseCreditToZone -= System( SysNum ).PipeHeatLoad;
					} //UseSysTimeStep
				}
			} //(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag)))
		} // SysNum

		// Need to know if mechanical subcoolers or cascade condensers or shared condensers
		//    are present. If so, energy transfer between
		//    detailed refrigeration systems requires additional iteration at this level.

		StartMechSubcoolLoop = 3;
		if ( ( NumSimulationMechSubcoolers > 0 ) || ( NumSimulationCascadeCondensers > 0 ) || ( NumSimulationSharedCondensers > 0 ) || ( NumSimulationRefrigAirChillers > 0 ) ) StartMechSubcoolLoop = 1;

		FirstSCLoop = true;
		for ( LoopNum = StartMechSubcoolLoop; LoopNum <= 3; ++LoopNum ) { //Note, for cascade cond loads compared requiring 5 iterations to 3, no difference.

			for ( SysNum = 1; SysNum <= NumRefrigSystems; ++SysNum ) {
				//Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
				if ( ( ( ! UseSysTimeStep ) && ( ! System( SysNum ).CoilFlag ) ) || ( ( UseSysTimeStep ) && ( System( SysNum ).CoilFlag ) ) ) {
					System( SysNum ).SumMechSCLoad = 0.0;
					System( SysNum ).SumCascadeLoad = 0.0;
					System( SysNum ).SumCascadeCondCredit = 0.0;
					System( SysNum ).SumMechSCBenefit = 0.0;

					if ( ( NumSimulationMechSubcoolers > 0 ) && ( ! FirstSCLoop ) ) {
						//This loop places load on system providing mechanical subcooling
						for ( SubcoolID = 1; SubcoolID <= NumSimulationSubcoolers; ++SubcoolID ) {
							if ( Subcooler( SubcoolID ).SubcoolerType == LiquidSuction ) continue;
							if ( Subcooler( SubcoolID ).MechSourceSysID != SysNum ) continue;
							//don't have summechscload until second subcooler pass, set to zero on first pass
							System( SysNum ).SumMechSCLoad += System( SysNum ).MechSCLoad( SubcoolID );
							//subcooler should not drive Tevap for supplying system,
							//    but check to see if T controlled can be met or if Tevap is at a higher temperature
							if ( Subcooler( SubcoolID ).MechControlTliqOut < System( SysNum ).TEvapNeeded ) {
								ShowWarningError( "Refrigeration:System: " + System( SysNum ).Name );
								ShowContinueError( " Evaporating temperature greater than the controlled " );
								ShowContinueError( " liquid outlet temperature for SUBCOOLER:" + Subcooler( SubcoolID ).Name );
							}
						} //SubcoolId

						if ( System( SysNum ).NumSubcoolers > 0 ) {
							for ( SubcoolerIndex = 1; SubcoolerIndex <= System( SysNum ).NumSubcoolers; ++SubcoolerIndex ) {
								SubcoolID = System( SysNum ).SubcoolerNum( SubcoolerIndex );
								if ( Subcooler( SubcoolID ).SubcoolerType == LiquidSuction ) continue;
								System( SysNum ).SumMechSCBenefit = Subcooler( SubcoolID ).MechSCTransLoad;
							} //subcoolerindex
						} // System(sysid)%numsubcoolers > 0
					} //NumSimulationMechSubcoolers > 0 and not first loop

					//This loop places load on system absorbing heat from cascade condenser and &
					//     condenser heat reclaim credits from hot gas/brine defrosts
					if ( ( System( SysNum ).NumCascadeLoads > 0 ) && ( ! FirstSCLoop ) ) {
						for ( CascadeLoadIndex = 1; CascadeLoadIndex <= System( SysNum ).NumCascadeLoads; ++CascadeLoadIndex ) {
							CascadeLoadID = System( SysNum ).CascadeLoadNum( CascadeLoadIndex );
							if ( System( SysNum ).CompSuctControl == ConstantSuctionTemperature ) {
								System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
							} else { // check for lowest T evap design among the CascadeLoad systems and
								//  Compare Tevap for this Cascade to max allowed for cases, walk ins, and
								//  for all previous CascadeLoad loops on this suction group and set
								//  at the MINIMUM
								if ( Condenser( CascadeLoadID ).CascadeTempControl == CascadeTempSet ) {
									//if float then set tevap based upon other loads
									if ( CascadeLoadIndex == 1 && System( SysNum ).NumNonCascadeLoads == 0 ) {
										System( SysNum ).TEvapNeeded = Condenser( CascadeLoadID ).CascadeRatedEvapTemp;
									} else {
										System( SysNum ).TEvapNeeded = min( Condenser( CascadeLoadID ).CascadeRatedEvapTemp, System( SysNum ).TEvapNeeded );
									}
								}
							} //floating or constant system evap temperature
							// increment Cascade condenser Loads for Compressors/condenser on each system
							// place any defrost credits on the same system absorbing the cascade condenser load
							// (CascadeSysID identifies the condenser producing the defrost credits, that is, the lower temp system)
							System( SysNum ).SumCascadeLoad += Condenser( CascadeLoadID ).CondLoad;
							System( SysNum ).SumCascadeCondCredit += System( Condenser( CascadeLoadID ).CascadeSysID ).TotalCondDefrostCredit;

						} //NumCascadeLoads
					} //System(SysNum)%NumCascadeLoads > 0

					//only calc detailed system if have load (could be zero first time through if only load is cascade condenser)
					System( SysNum ).TotalSystemLoad = System( SysNum ).TotalCoolingLoad + System( SysNum ).SumSecondaryLoopLoad + System( SysNum ).SumMechSCLoad + System( SysNum ).SumCascadeLoad;
					if ( System( SysNum ).TotalSystemLoad > 0.0 ) {
						System( SysNum ).CpSatVapEvap = GetSatSpecificHeatRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TEvapNeeded, 1.0, System( SysNum ).RefIndex, RoutineName );
						System( SysNum ).HCaseOut = GetSatEnthalpyRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TEvapNeeded, 1.0, System( SysNum ).RefIndex, RoutineName ) + System( SysNum ).CpSatVapEvap * CaseSuperheat;
						//Establish estimates to start solution loop
						{ auto const SELECT_CASE_var( Condenser( System( SysNum ).CondenserNum( 1 ) ).CondenserType ); //only one condenser allowed now
						if ( SELECT_CASE_var == RefrigCondenserTypeAir ) {
							System( SysNum ).TCondense = OutDryBulbTemp + 16.7;
							//16.7C is delta T at rating point for air-cooled condensers, just estimate, so ok for zone-located condensers
						} else if ( SELECT_CASE_var == RefrigCondenserTypeEvap ) {
							System( SysNum ).TCondense = OutDryBulbTemp + 15.0;
							//15C is delta T at rating point for evap-cooled condensers
						} else if ( SELECT_CASE_var == RefrigCondenserTypeWater ) {
							//define starting estimate at temperature of water exiting condenser
							System( SysNum ).TCondense = Node( Condenser( System( SysNum ).CondenserNum( 1 ) ).OutletNode ).Temp;
						} else if ( SELECT_CASE_var == RefrigCondenserTypeCascade ) {
							//?Don't need estimate for cascade condenser because it doesn't iterate?
						}}

						//Produce first time step estimates, assume no subcoolers
						System( SysNum ).HSatLiqCond = GetSatEnthalpyRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
						System( SysNum ).CpSatLiqCond = GetSatSpecificHeatRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
						System( SysNum ).HCaseIn = System( SysNum ).HSatLiqCond - System( SysNum ).CpSatLiqCond * Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool;
						System( SysNum ).RefMassFlowtoLoads = System( SysNum ).TotalSystemLoad / ( System( SysNum ).HCaseOut - System( SysNum ).HCaseIn );
						System( SysNum ).RefMassFlowComps = System( SysNum ).RefMassFlowtoLoads;

						if ( System( SysNum ).NumStages == 2 ) { // Two-stage compression system
							// Initial guess for high-stage mass flow rate in two-stage compression systems
							System( SysNum ).RefMassFlowHiStageComps = System( SysNum ).RefMassFlowComps / 0.65;
						}

						CalcDetailedSystem( SysNum );
						DeRate = false;

						//With air chiller coils, don't use unmet energy, instead reduce capacity on coils to match avail compressor/cond capacity
						CurrentLoads = System( SysNum ).TotalSystemLoad + System( SysNum ).LSHXTrans; //because compressor capacity rated from txv to comp inlet
						if ( ( System( SysNum ).CoilFlag ) && ( CurrentLoads > ( System( SysNum ).TotCompCapacity * 1.001 ) ) ) {
							DeRate = true;
							FinalRateCoils( DeRate, DetailedSystem, SysNum, CurrentLoads, System( SysNum ).TotCompCapacity );
							System( SysNum ).TotalCoolingLoad = 0.0;
							System( SysNum ).TotalCondDefrostCredit = 0.0;
							for ( CoilIndex = 1; CoilIndex <= System( SysNum ).NumCoils; ++CoilIndex ) {
								CoilID = System( SysNum ).CoilNum( CoilIndex );
								// already CALLed CalculateCoil(CoilID) in CoilSet specified order
								if ( System( SysNum ).CompSuctControl == ConstantSuctionTemperature ) {
									System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
								} else { // calculate floating T evap
									System( SysNum ).TEvapNeeded = System( SysNum ).TEvapDesign;
									ShowWarningError( "Refrigeration:System: " + System( SysNum ).Name );
									ShowContinueError( " Floating evaporator temperature model not yet available for warehouse coil systems. " );
								} //floating or constant evap temperature
								// increment TotalCoolingLoad for Compressors/condenser on each system
								System( SysNum ).TotalCoolingLoad += WarehouseCoil( CoilID ).TotalCoolingLoad;
								System( SysNum ).TotalCondDefrostCredit += WarehouseCoil( CoilID ).HotDefrostCondCredit;
							} //NumCoils systems
							if ( System( SysNum ).NumStages == 2 && System( SysNum ).TotHiStageCompCapacity < ( System( SysNum ).TotalCoolingLoad + System( SysNum ).LSHXTrans + System( SysNum ).TotCompPower ) ) {
								ShowRecurringWarningErrorAtEnd( "Refrigeration:System: " + System( SysNum ).Name + ":The specified high-stage compressors for this system are unable to meet the sum of the refrigeration loads, ", System( SysNum ).HiStageWarnIndex1 );
								ShowRecurringContinueErrorAtEnd( " subcooler loads (if any), and low-stage compressor loads for this sytem.", System( SysNum ).HiStageWarnIndex2 );
							} //Hi-stage capacity<(load+LSHX load + lo-stage compressor load)
						} //CoilFlag (Numcoils > 0) and load > capacity

					} //System(SysNum)%TotalSystemLoad > 0
				} //(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag)))
			} //SysNum over NumRefrigSystems
			FirstSCLoop = false;
		} // LoopNum, three times for buildings with multiple detailed systems connected with mechanical subcoolers
		// or cascade condensers or shared condensers or warehouse coils that might need to be de-rated

		// Dealing with unmet load has to be done outside iterative loop
		for ( SysNum = 1; SysNum <= NumRefrigSystems; ++SysNum ) {
			//Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
			if ( ( ( ( ! UseSysTimeStep ) && ( ! System( SysNum ).CoilFlag ) ) || ( ( UseSysTimeStep ) && ( System( SysNum ).CoilFlag ) ) ) && ( ! WarmupFlag ) ) {
				CurrentLoads = System( SysNum ).TotalSystemLoad + System( SysNum ).LSHXTrans; //because compressor capacity rated from txv to comp inlet
				if ( System( SysNum ).NumStages == 2 ) {
					CurrentHiStageLoads = CurrentLoads + System( SysNum ).TotCompPower;
				} // NumStages==2
				if ( System( SysNum ).CoilFlag ) {
					// don't use 'unmet energy' with air chillers, see 'derate'
					System( SysNum ).UnmetEnergy = 0.0;
					System( SysNum ).UnmetHiStageEnergy = 0.0;
				} else {
					// Meeting current and possibly some portion of the previously unmet energy
					// perhaps future interest in reporting percent of installed capacity used(or number of compressors) ?
					// If the system compressors were unable to meet the current loads, save energy to be met in succeeding time step
					// Note the unmet energy is turned into a rate and applied to the system load at the start of calccompressor
					System( SysNum ).UnmetEnergy += ( CurrentLoads - System( SysNum ).TotCompCapacity ) * TimeStepZoneSec;
					if ( System( SysNum ).NumStages == 2 ) {
						System( SysNum ).UnmetHiStageEnergy += ( CurrentHiStageLoads - System( SysNum ).TotHiStageCompCapacity ) * TimeStepZoneSec;
					}
					if ( System( SysNum ).UnmetEnergy > MyLargeNumber ) {
						System( SysNum ).UnmetEnergy = MyLargeNumber;
						if ( ShowUnmetEnergyWarning( SysNum ) ) {
							ShowWarningError( "Refrigeration:System: " + System( SysNum ).Name );
							ShowContinueError( " The specified compressors for this system are unable to meet " );
							ShowContinueError( " the sum of the refrigerated case loads and subcooler loads (if any) for this sytem." );
							ShowUnmetEnergyWarning( SysNum ) = false;
						} //show warning
					} // > mylarge number
					if ( System( SysNum ).UnmetHiStageEnergy > MyLargeNumber ) {
						System( SysNum ).UnmetHiStageEnergy = MyLargeNumber;
						if ( ShowHiStageUnmetEnergyWarning( SysNum ) ) {
							ShowWarningError( "Refrigeration:System: " + System( SysNum ).Name );
							ShowContinueError( " The specified high-stage compressors for this system are unable to meet " );
							ShowContinueError( " the sum of the refrigerated case loads, subcooler loads (if any) and " );
							ShowContinueError( " low-stage compressor loads for this sytem." );
							ShowHiStageUnmetEnergyWarning( SysNum ) = false;
						} //show warning
					} // > mylarge number
				} // numcoils > 0

				//Zone-located air-cooled condenser reject heat also has to be outside iterative loop
				if ( System( SysNum ).SystemRejectHeatToZone ) {
					CondInletAirZoneNum = Condenser( System( SysNum ).CondenserNum( 1 ) ).InletAirZoneNum;
					if ( UseSysTimeStep ) {
						CoilSysCredit( CondInletAirZoneNum ).SenCreditToZoneRate += System( SysNum ).NetHeatRejectLoad; //Adding heat is positive
						CoilSysCredit( CondInletAirZoneNum ).ReportSenCoolingToZoneRate = -CoilSysCredit( CondInletAirZoneNum ).SenCreditToZoneRate;
					}
					//Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
					if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
						RefrigCaseCredit( CondInletAirZoneNum ).SenCaseCreditToZone += System( SysNum ).NetHeatRejectLoad; //Adding heat is positive
					} //UseSystimestep
				} //Reject heat to zone

				// Report variables
				System( SysNum ).TotTransferLoad = System( SysNum ).SumMechSCLoad - System( SysNum ).SumMechSCBenefit + System( SysNum ).SumSecondaryLoopLoad + System( SysNum ).SumCascadeLoad;
				System( SysNum ).TotTransferEnergy = System( SysNum ).TotTransferLoad * LocalTimeStep * SecInHour;
				System( SysNum ).PipeHeatEnergy = System( SysNum ).PipeHeatLoad * LocalTimeStep * SecInHour;
				System( SysNum ).TotalCoolingEnergy = System( SysNum ).TotalCoolingLoad * LocalTimeStep * SecInHour;
			} //(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag))).and.not WarmupFlag
		} // SysNum = 1,NumRefrigSystems

		// Update for sending to zone equipment manager. (note report variables are summed elsewhere)
		//   LatOutputProvided = CoilSysCredit(ZoneNum)%LatKgPerS_ToZoneRate
		//   SysOutputProvided = CoilSysCredit(ZoneNum)%SenCreditToZoneRate
		// Note that case credit is negative for cooling, thus subtract positive value calculated for coil
		//   Note this is done whether or not the coils are derated.
		if ( UseSysTimeStep ) {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				for ( CoilID = 1; CoilID <= NumSimulationRefrigAirChillers; ++CoilID ) {
					if ( WarehouseCoil( CoilID ).ZoneNum != ZoneNum ) continue;
					CoilSysCredit( ZoneNum ).SenCreditToZoneRate -= WarehouseCoil( CoilID ).SensCreditRate;
					CoilSysCredit( ZoneNum ).SenCreditToZoneEnergy = CoilSysCredit( ZoneNum ).SenCreditToZoneRate * LocalTimeStep * SecInHour;
					CoilSysCredit( ZoneNum ).LatKgPerS_ToZoneRate -= WarehouseCoil( CoilID ).LatKgPerS_ToZone;
					CoilSysCredit( ZoneNum ).LatCreditToZoneRate -= WarehouseCoil( CoilID ).LatCreditRate;
					CoilSysCredit( ZoneNum ).LatCreditToZoneEnergy -= WarehouseCoil( CoilID ).LatCreditEnergy;
				}
			}
		}

		SumZoneImpacts();

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimulateDetailedTransRefrigSystems()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brian A. Fricke, ORNL
		//       DATE WRITTEN   Fall 2011
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called to simulate detailed transcritical CO2 refrigeration systems

		// METHODOLOGY EMPLOYED:
		// Each refrigeration system is modeled by first simulating the attached refrigerated cases and
		// walk-ins. The sum of the total heat transfer for all attached cases and walk-ins determines
		// the load on the compressors. Iterations are used here to account for sharing of gas coolers
		// between independent refrigeration systems.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "SimulateDetailedTransRefrigSystems" );
		int SysNum; // Index to the detailed transcritical refrigeration system being modeled
		static bool FirstSCLoop( true ); // Flag first time through multi-system loop used when mech subcoolers present
		static int StartMechSubcoolLoop( 3 ); // if no mechanical subcoolers transfer energy between system, don't loop
		static int LoopNum( 0 ); // Index to overall repeat necessary for mechanical subcoolers
		static int CaseID( 0 ); // Absolute reference to case
		static int CaseIndex( 0 ); // Index to case
		static int CondInletAirZoneNum( 0 ); // Index used to assign zone credits
		static int SuctionPipeActualZoneNum( 0 ); // Index to zone exchanging heat with suction pipes
		static int WalkInID( 0 ); // Absolute reference to WalkIn
		static int WalkInIndex( 0 ); // Index to WalkIn
		static Real64 LocalTimeStep( 0.0 ); // Set equal to either TimeStepSys or TimeStepZone
		static Real64 CurrentLoads( 0.0 ); // current loads on compressor, exclusive of unmet loads from prev time steps
		Real64 SuctionPipeZoneTemp; // Temperature for zone identified as environment for suction pipe heat gains, C

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		//  Do transcritical CO2 refrigeration system loop outside of iterative solution to initialize time step and
		//  calculate case and and walk-ins (that won't change during balance of refrigeration system iterations)
		//  and prepare initial estimates for the iterative system solution

		//  TransCritSysFlag = .TRUE.
		for ( SysNum = 1; SysNum <= NumTransRefrigSystems; ++SysNum ) {
			//Only do those systems appropriate for this analysis, supermarket type on load time step
			if ( TransSystem( SysNum ).NumCasesMT > 0 ) {
				for ( CaseIndex = 1; CaseIndex <= TransSystem( SysNum ).NumCasesMT; ++CaseIndex ) {
					CaseID = TransSystem( SysNum ).CaseNumMT( CaseIndex );
					CalculateCase( CaseID );
					//  TEvapDesignMT calc in Get Input to meet lowest evap temp of any MT load on the system.
					//  TEvapNeededMT is fixed at this design value.
					TransSystem( SysNum ).TEvapNeededMT = TransSystem( SysNum ).TEvapDesignMT;
					// increment TotalCoolingLoad for Compressors/gas cooler on each system and defrost gas cooler credits for heat recovery
					TransSystem( SysNum ).TotalCoolingLoadMT += RefrigCase( CaseID ).TotalCoolingLoad;
					TransSystem( SysNum ).TotalCondDefrostCredit += RefrigCase( CaseID ).HotDefrostCondCredit;
				} //NumCasesMT
			} //Num of MT cases > 0

			if ( TransSystem( SysNum ).NumCasesLT > 0 ) {
				for ( CaseIndex = 1; CaseIndex <= TransSystem( SysNum ).NumCasesLT; ++CaseIndex ) {
					CaseID = TransSystem( SysNum ).CaseNumLT( CaseIndex );
					CalculateCase( CaseID );
					//  TEvapDesignLT calc in Get Input to meet lowest evap temp of any LT load on the system.
					//  TEvapNeededLT is fixed at this design value.
					TransSystem( SysNum ).TEvapNeededLT = TransSystem( SysNum ).TEvapDesignLT;
					// increment TotalCoolingLoad for Compressors/gas cooler on each system and defrost gas cooler credits for heat recovery
					TransSystem( SysNum ).TotalCoolingLoadLT += RefrigCase( CaseID ).TotalCoolingLoad;
					TransSystem( SysNum ).TotalCondDefrostCredit += RefrigCase( CaseID ).HotDefrostCondCredit;
				} //NumCasesLT
			} //Num of LT cases > 0

			if ( TransSystem( SysNum ).NumWalkInsMT > 0 ) {
				for ( WalkInIndex = 1; WalkInIndex <= TransSystem( SysNum ).NumWalkInsMT; ++WalkInIndex ) {
					WalkInID = TransSystem( SysNum ).WalkInNumMT( WalkInIndex );
					CalculateWalkIn( WalkInID );
					//  TEvapDesignMT calc in Get Input to meet lowest evap temp of any MT load on the system.
					//  TEvapNeededMT is fixed at this design value.
					TransSystem( SysNum ).TEvapNeededMT = TransSystem( SysNum ).TEvapDesignMT;
					// increment TotalCoolingLoad for Compressors/gas cooler on each system
					TransSystem( SysNum ).TotalCoolingLoadMT += WalkIn( WalkInID ).TotalCoolingLoad;
					TransSystem( SysNum ).TotalCondDefrostCredit += WalkIn( WalkInID ).HotDefrostCondCredit;
				} //NumWalkInsMT systems
			} //TransSystem(SysNum)%NumWalkInsMT > 0

			if ( TransSystem( SysNum ).NumWalkInsLT > 0 ) {
				for ( WalkInIndex = 1; WalkInIndex <= TransSystem( SysNum ).NumWalkInsLT; ++WalkInIndex ) {
					WalkInID = TransSystem( SysNum ).WalkInNumLT( WalkInIndex );
					CalculateWalkIn( WalkInID );
					//  TEvapDesignLT calc in Get Input to meet lowest evap temp of any LT load on the system.
					//  TEvapNeeded is fixed at this design value.
					TransSystem( SysNum ).TEvapNeededLT = TransSystem( SysNum ).TEvapDesignLT;
					// increment TotalCoolingLoad for Compressors/gas cooler on each system
					TransSystem( SysNum ).TotalCoolingLoadLT += WalkIn( WalkInID ).TotalCoolingLoad;
					TransSystem( SysNum ).TotalCondDefrostCredit += WalkIn( WalkInID ).HotDefrostCondCredit;
				} //NumWalkInsLT systems
			} //TransSystem(SysNum)%NumWalkInsLT > 0

			//add suction pipe heat gains (W) if input by user
			//Suction pipe heat gains aren't included in the reported total system load, but are heat gains that must be met in
			//  gas cooler and compressor loads.
			TransSystem( SysNum ).PipeHeatLoadMT = 0.0;
			if ( TransSystem( SysNum ).SumUASuctionPipingMT > MySmallNumber ) {
				SuctionPipeZoneTemp = Node( TransSystem( SysNum ).SuctionPipeZoneNodeNumMT ).Temp;
				TransSystem( SysNum ).PipeHeatLoadMT = TransSystem( SysNum ).SumUASuctionPipingMT * ( SuctionPipeZoneTemp - TransSystem( SysNum ).TEvapNeededMT );
				// pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
				//   from refrigcasecredit (- for cooling zone, + for heating zone)
				SuctionPipeActualZoneNum = TransSystem( SysNum ).SuctionPipeActualZoneNumMT;
				//Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
				if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
					RefrigCaseCredit( SuctionPipeActualZoneNum ).SenCaseCreditToZone -= TransSystem( SysNum ).PipeHeatLoadMT;
				} //UseSysTimeStep
			}

			TransSystem( SysNum ).PipeHeatLoadLT = 0.0;
			if ( TransSystem( SysNum ).SumUASuctionPipingLT > MySmallNumber ) {
				SuctionPipeZoneTemp = Node( TransSystem( SysNum ).SuctionPipeZoneNodeNumLT ).Temp;
				TransSystem( SysNum ).PipeHeatLoadLT = TransSystem( SysNum ).SumUASuctionPipingLT * ( SuctionPipeZoneTemp - TransSystem( SysNum ).TEvapNeededLT );
				// pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
				//   from refrigcasecredit (- for cooling zone, + for heating zone)
				SuctionPipeActualZoneNum = TransSystem( SysNum ).SuctionPipeActualZoneNumLT;
				//Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
				if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
					RefrigCaseCredit( SuctionPipeActualZoneNum ).SenCaseCreditToZone -= TransSystem( SysNum ).PipeHeatLoadLT;
				} //UseSysTimeStep
			}

		} // SysNum

		// Need to know if shared gas coolers are present. If so, energy
		// transfer between detailed transcritical refrigeration systems
		// requires additional iteration at this level.

		StartMechSubcoolLoop = 3;
		if ( NumSimulationSharedGasCoolers > 0 ) StartMechSubcoolLoop = 1;

		FirstSCLoop = true;
		for ( LoopNum = StartMechSubcoolLoop; LoopNum <= 3; ++LoopNum ) {

			for ( SysNum = 1; SysNum <= NumTransRefrigSystems; ++SysNum ) {
				//Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
				//only calc detailed system if have load
				TransSystem( SysNum ).TotalSystemLoadMT = TransSystem( SysNum ).TotalCoolingLoadMT;
				if ( TransSystem( SysNum ).TransSysType == 2 ) {
					TransSystem( SysNum ).TotalSystemLoadLT = TransSystem( SysNum ).TotalCoolingLoadLT;
				}
				TransSystem( SysNum ).TotalSystemLoad = TransSystem( SysNum ).TotalSystemLoadLT + TransSystem( SysNum ).TotalSystemLoadMT;
				if ( TransSystem( SysNum ).TotalSystemLoad > 0.0 ) {
					if ( TransSystem( SysNum ).TransSysType == 2 ) {
						TransSystem( SysNum ).CpSatVapEvapLT = GetSatSpecificHeatRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededLT, 1.0, TransSystem( SysNum ).RefIndex, RoutineName );
						TransSystem( SysNum ).HCaseOutLT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededLT, 1.0, TransSystem( SysNum ).RefIndex, RoutineName ) + TransSystem( SysNum ).CpSatVapEvapLT * TransCaseSuperheat;
					}
					TransSystem( SysNum ).CpSatVapEvapMT = GetSatSpecificHeatRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededMT, 1.0, TransSystem( SysNum ).RefIndex, RoutineName );
					TransSystem( SysNum ).HCaseOutMT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededMT, 1.0, TransSystem( SysNum ).RefIndex, RoutineName ) + TransSystem( SysNum ).CpSatVapEvapMT * TransCaseSuperheat;

					//Produce first time step estimates.
					//Assume no subcoolers and neglect flow through bypass.
					TransSystem( SysNum ).TReceiver = GetSatTemperatureRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).PReceiver, TransSystem( SysNum ).RefIndex, RoutineName );
					TransSystem( SysNum ).HSatLiqReceiver = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TReceiver, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
					TransSystem( SysNum ).CpSatLiqReceiver = GetSatSpecificHeatRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TReceiver, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
					TransSystem( SysNum ).HCaseInMT = TransSystem( SysNum ).HSatLiqReceiver;
					TransSystem( SysNum ).HCaseInLT = TransSystem( SysNum ).HSatLiqReceiver;
					TransSystem( SysNum ).RefMassFlowtoLTLoads = 0.0;
					TransSystem( SysNum ).RefMassFlowCompsLP = 0.0;
					TransSystem( SysNum ).DelHSubcoolerDis = 0.0;
					TransSystem( SysNum ).DelHSubcoolerSuc = 0.0;
					if ( TransSystem( SysNum ).TransSysType == 2 ) {
						TransSystem( SysNum ).RefMassFlowtoLTLoads = TransSystem( SysNum ).TotalSystemLoadLT / ( TransSystem( SysNum ).HCaseOutLT - TransSystem( SysNum ).HCaseInLT );
						TransSystem( SysNum ).RefMassFlowCompsLP = TransSystem( SysNum ).RefMassFlowtoLTLoads;
					} // (TransSystem(SysNum)%TransSysType == 2)
					TransSystem( SysNum ).RefMassFlowtoMTLoads = TransSystem( SysNum ).TotalSystemLoadMT / ( TransSystem( SysNum ).HCaseOutMT - TransSystem( SysNum ).HCaseInMT );
					TransSystem( SysNum ).RefMassFlowCompsHP = TransSystem( SysNum ).RefMassFlowtoLTLoads + TransSystem( SysNum ).RefMassFlowtoMTLoads;

					CalcDetailedTransSystem( SysNum );
					//       TransCritSysFlag = .FALSE.

				} //TransSystem(SysNum)%TotalSystemLoad > 0
			} //SysNum over NumRefrigSystems
			FirstSCLoop = false;
		} // LoopNum, three times for buildings with multiple detailed systems connected with shared gas coolers

		// Unmet load is done outside iterative loop
		for ( SysNum = 1; SysNum <= NumTransRefrigSystems; ++SysNum ) {
			//Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
			if ( ( ! UseSysTimeStep ) && ( ! WarmupFlag ) ) {
				CurrentLoads = TransSystem( SysNum ).TotalSystemLoad;
				// Meeting current and possibly some portion of the previously unmet energy
				// perhaps future interest in reporting percent of installed capacity used(or number of compressors) ?
				// If the system compressors were unable to meet the current loads, save energy to be met in succeeding time step
				// Note the unmet energy is turned into a rate and applied to the system load at the start of calccompressor
				TransSystem( SysNum ).UnmetEnergy += ( CurrentLoads - TransSystem( SysNum ).TotCompCapacity ) * TimeStepZoneSec;

				if ( TransSystem( SysNum ).UnmetEnergy > MyLargeNumber ) {
					TransSystem( SysNum ).UnmetEnergy = MyLargeNumber;
					if ( ShowUnmetEnergyWarningTrans( SysNum ) ) {
						ShowWarningError( "Refrigeration:TranscriticalSystem: " + TransSystem( SysNum ).Name );
						ShowContinueError( " The specified compressors for this system are unable to meet " );
						ShowContinueError( " the sum of the refrigerated case loads and subcooler loads (if any) for this sytem." );
						ShowUnmetEnergyWarningTrans( SysNum ) = false;
					} //show warning
				} // > mylarge number

				//Zone-located air-cooled gas cooler reject heat also has to be outside iterative loop
				if ( TransSystem( SysNum ).SystemRejectHeatToZone ) {
					CondInletAirZoneNum = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).InletAirZoneNum;
					//Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
					if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
						RefrigCaseCredit( CondInletAirZoneNum ).SenCaseCreditToZone += TransSystem( SysNum ).NetHeatRejectLoad; //Adding heat is positive
					} //UseSystimestep
				} //Reject heat to zone

				// Report variables
				TransSystem( SysNum ).PipeHeatEnergy = ( TransSystem( SysNum ).PipeHeatLoadMT + TransSystem( SysNum ).PipeHeatLoadLT ) * LocalTimeStep * SecInHour;
				TransSystem( SysNum ).TotalCoolingEnergy = ( TransSystem( SysNum ).TotalCoolingLoadMT + TransSystem( SysNum ).TotalCoolingLoadMT ) * LocalTimeStep * SecInHour;
			} //(.NOT. UseSysTimeStep).AND. (.not. WarmupFlag)
		} // SysNum = 1,NumTransRefrigSystems

		// Update for sending to zone equipment manager. (note report variables are summed elsewhere)

		SumZoneImpacts();

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcDetailedSystem( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
		//       DATE WRITTEN   Spring 2008
		//       Using condenser solution algorithms written by Richard Raustad, FSEC
		//          Oct/Nov 2004, and MODIFIED by Shirey, FSEC Dec 2004, and Hudson, ORNL in 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the power and energy needed to meet the refrigeration loads for a particular detailed
		// refrigeration system comprised of multiple cases, one condenser, and multiple compressors.

		// METHODOLOGY EMPLOYED:
		// Sum the refrigeration loads on the system and determine the required evaporating temperature.
		// Using the initial estimate for condensing temperature, dispatch the compressors to
		// determine the needed power, energy consumption, and refrigerant mass flow.
		// Calculate the condenser fan/pump power and consumption.
		// Calculate the condensing temperature as a function of environment and load.
		// Resolve the impact of subcooler heat transfer between and among systems
		// Iterate until the calculated refrigerant mass flow through the compressors converges, which
		// typically requires less than 5 iterations. This was found to be more sensitive than converging
		// upon the calculated condensing temperature.

		// REFERENCES:
		// "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
		//  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

		// Kyle A. Manske, Performance Optimization of Industrial Refrigeration Systems,
		//  A thesis submitted in partial fulfillment of the requirements for the degree of
		//  Master of Science, University of Wisconsin-Madison, 1999

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrorTol( 0.001 ); // Iterative solution tolerance

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumIter;
		bool NotBalanced;
		Real64 TCondStart;
		static Real64 MassFlowCompsStart( 0.0 ); // Mass flow through (low-stage) compressors (single- or two-stage systems)
		static Real64 MassFlowHiStageCompsStart( 0.0 ); // Mass flow through high-stage comrpessors (two-stage systems only)
		static Real64 ErrorMassFlowComps( 0.0 ); // Error in calculated (low stage) compressor mass flow (single- or two-stage systems)
		static Real64 ErrorMassFlowHiStageComps( 0.0 ); // Error in calculated high-stage compressor mass flow (two-stage systems only)

		//Balance This Refrigeration System using calculated refrigerant flow
		NotBalanced = true;
		NumIter = 0;

		auto & refrig_system( System( SysNum ) );
		while ( NotBalanced ) {
			//Set values for iteration convergence tolerance check
			++NumIter;
			TCondStart = refrig_system.TCondense;
			MassFlowCompsStart = refrig_system.RefMassFlowComps;
			if ( refrig_system.NumStages == 2 ) { // Two-stage systems
				MassFlowHiStageCompsStart = refrig_system.RefMassFlowHiStageComps;
			}

			if ( refrig_system.NumSubcoolers > 0 ) CalculateSubcoolers( SysNum );
			CalculateCompressors( SysNum );
			CalculateCondensers( SysNum );
			refrig_system.RefMassFlowtoLoads = refrig_system.TotalSystemLoad / ( refrig_system.HCaseOut - refrig_system.HCaseIn );
			if ( NumIter < 2 ) continue;
			//Previously did error check on calculated Tcondense, but not sensitive enough
			if ( ( refrig_system.RefMassFlowtoLoads == 0.0 ) || ( MassFlowCompsStart == 0.0 ) ) { //.OR. (MassFlowCasesStart == 0.0)
				ShowWarningError( "Refrigeration:System: " + refrig_system.Name + " showing zero refrigeration flow." );
			} else {
				ErrorMassFlowComps = std::abs( MassFlowCompsStart - refrig_system.RefMassFlowComps ) / MassFlowCompsStart;
				if ( refrig_system.NumStages == 2 ) { // Two-stage systems
					ErrorMassFlowHiStageComps = std::abs( MassFlowHiStageCompsStart - refrig_system.RefMassFlowHiStageComps ) / MassFlowCompsStart;
				}
			} //denominator zero check
			if ( NumIter > 20 ) break;
			if ( ErrorMassFlowComps < ErrorTol ) {
				if ( refrig_system.NumStages == 1 ) {
					NotBalanced = false;
				} else if ( refrig_system.NumStages == 2 && ErrorMassFlowHiStageComps < ErrorTol ) {
					NotBalanced = false;
				}
			}
		} //error check

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcDetailedTransSystem( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brian A. Fricke, ORNL
		//       DATE WRITTEN   Fall 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the power and energy needed to meet the refrigeration loads for a detailed transcritical
		// CO2 refrigeration system comprised of multiple cases and walk-ins, one gas cooler, and
		// multiple compressors.

		// METHODOLOGY EMPLOYED:
		// Sum the refrigeration loads on the system and determine the required evaporating temperature.
		// Dispatch the compressors to determine the needed power, energy consumption, and refrigerant
		// mass flow. Calculate the gas cooler fan power and consumption. Calculate the gas cooler
		// outlet temperature and pressure as a function of ambient temperature. Iterate until the
		// calculated refrigerant mass flow through the receiver bypass converges, which typically
		// requires less than 5 iterations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrorTol( 0.001 ); // Iterative solution tolerance

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumIter; // Iteration counter
		bool NotBalanced; // Flag to indicate convergence, based on system balance
		Real64 MassFlowStart; // Initial refrigerant mass flow through receiver bypass
		Real64 ErrorMassFlow; // Error in calculated refrigerant mass flow trhough receiver bypass

		//Balance this refrigeration system using calculated refrigerant flow
		NotBalanced = true;
		NumIter = 0;

		// Set initial guess for receiver bypass refrigerant flow rate
		MassFlowStart = 0.5;
		while ( NotBalanced ) {
			++NumIter;

			if ( TransSystem( SysNum ).NumGasCoolers >= 1 ) CalcGasCooler( SysNum );
			CalculateTransCompressors( SysNum );
			if ( NumIter < 2 ) continue;
			if ( ( TransSystem( SysNum ).RefMassFlowReceiverBypass == 0.0 ) || ( MassFlowStart == 0.0 ) ) {
				ShowSevereError( "Refrigeration:TranscriticalSystem: " + TransSystem( SysNum ).Name + " showing zero refrigerant flow through receiver bypass." );
				ShowContinueError( "Receiver Bypass Flow = " + RoundSigDigits( TransSystem( SysNum ).RefMassFlowReceiverBypass, 6 ) );
				ShowContinueError( "Check input file to ensure that refrigeration loads on this system are not zero." );
			} else {
				ErrorMassFlow = std::abs( MassFlowStart - TransSystem( SysNum ).RefMassFlowReceiverBypass ) / MassFlowStart;
				MassFlowStart = TransSystem( SysNum ).RefMassFlowReceiverBypass;
			} //denominator zero check
			if ( NumIter > 20 ) break;
			if ( ErrorMassFlow < ErrorTol ) NotBalanced = false;
		} //error check

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateCondensers( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall and C. R. Hudson, ORNL, Assisted by Hugh Henderson
		//       DATE WRITTEN   Spring 2008
		//       Using condenser solution algorithms written by Richard Raustad, FSEC
		//          Oct/Nov 2004, and MODIFIED by Shirey, FSEC Dec 2004, and Hudson, ORNL in 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the condenser heat rejection for a particular detailed
		// refrigeration system and condensing temperature (part of iterative soln for cond temp).

		// METHODOLOGY EMPLOYED:
		// Calculate the condenser fan/pump power and consumption
		// using manufacturer's rating data and fan power correlations
		// from ASHRAE and evaporative effectiveness based on enthalpy
		// similar to work done by Manske.

		// From Heejin Cho, Re variable frequency drive fans,
		// "From HVAC forums, I learned that it is common practice to set a
		// minimum frequency at 15 or 20 Hz to protect motors from overheating. The
		// full speed is at 60 Hz. The ratio of minimum and maximum frequencies
		// will correspond to the ratio of minimum and maximum flow rates."

		// REFERENCES:
		// "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
		//  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

		// Kyle A. Manske, Performance Optimization of Industrial Refrigeration Systems,
		//  A thesis submitted in partial fulfillment of the requirements for the degree of
		//  Master of Science, University of Wisconsin-Madison, 1999

		// Lawrence Berkeley Laboratory and Resource Dynamics, Improving Fan Systrem Performance,
		//   A Sourcebook for Industry, DOE/GO-102003-1294, April 2003

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTsatFnHPb;
		using Psychrometrics::PsyWFnTdpPb;
		using Psychrometrics::PsyHFnTdbRhPb;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutDryBulbTemp;
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const BleedRateConstant( 5.0E-10 ); // water purge rate for evaporative
		//  condensers (m3/W-s) equal to 3 GPM per 100 tons (BAC Engineering Reference)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondID; // Condenser Number
		int CondCreditWarnIndex1; // Used to sum up warning count
		int CondCreditWarnIndex2; // Used to sum up warning count
		int CondCreditWarnIndex3; // Used to sum up warning count
		int CondCreditWarnIndex4; // Used to sum up warning count
		int CondCreditWarnIndex5; // Used to sum up warning count
		int CondCreditWarnIndex6; // Used to sum up warning count
		int CondCreditWarnIndex7; // Used to sum up warning count
		int Sysloop; // counter over number of systems attached to this condenser
		int SystemID; // System number rejecting heat to this condenser
		bool EvapAvail; // Control for evap condenser availability

		Real64 AirVolRatio; // Ratio of air volume needed to remove load relative to design load
		Real64 AirDensity; // Density of air at condenser inlet [kg/m3]
		Real64 AirDensityDry; // Density of dry air at condenser inlet temperature [kg/m3]
		Real64 ActualFanPower; // Fan power after adjustments for partially loaded condenser [W]
		Real64 BPress; // Barometric pressure at condenser air inlet node [Pa]
		Real64 CapFac; // Capacity Factor
		Real64 Effectiveness; // for evap condenser, =capacity/max cap, where max cap is cap if Tairout equal Tcondense
		Real64 EnthalpyAtTcond; // enthalpy of saturated air at Tcondense
		Real64 EnthalpyAirIn; // Enthalpy of air entering condenser [J/kg]
		Real64 EnthalpyAirOut; // Enthalpy of air leaving condenser [J/kg]
		Real64 FanMinAirFlowRatio; // Minimum fan air flow ratio
		Real64 FanPowerRatio; // Calculated fan power ratio
		Real64 HRCF( 0.0 ); // Heat Rejection Capacity Factor (convention for evap condensers)
		Real64 HRCFFullFlow; // Heat Rejection Capacity Factor at full air flow
		Real64 HumRatIn; // Humidity ratio of inlet air to condenser [kg/kg]
		Real64 HumRatOut; // Humidity ratio of outlet air from condenser (assumed saturated) [kg/kg]
		Real64 LocalTimeStep; // Set equal to either TimeStepSys or TimeStepZone
		Real64 OutWbTemp( 0.0 ); // Outdoor wet bulb temp at condenser air inlet node [C]
		Real64 OutDbTemp; // Outdoor dry bulb temp at condenser air inlet node [C]
		Real64 PurgeRate; // Rate of water blow-down/bleed/purge in evap condenseer (m3/s)
		Real64 RatedFanPower; // local variable equal to input condenser value
		Real64 RatedAirFlowRate; // local variable equal to input condenser value
		Real64 QuadBterm; // calculation step in solving quadratic equation for hrcf
		Real64 Sqrtterm; // calculation step in solving quadratic equation for hrcf
		Real64 SinkTemp; // Heat sink temperature used to derate fan power at reduced loads [C]
		Real64 TAirOut; // Temperature of air leaving condenser [C]
		Real64 TCondCalc; // Calculated Condensing temperature
		Real64 TotalCondDefrostCredit; // total cond credit from hot gas/brine defr for cases etc. served
		//     directly by all systems served by this condenser [W]
		Real64 TotalCondDefCredfromSysID; // cond credit for single system [W]
		Real64 TotalLoadFromSysID; // total heat rejection load from a single detailed system [W]
		Real64 TotalLoadFromThisSystem( 0.0 ); // total heat rejection load from the detailed system id'd in subroutine call [W]
		Real64 TotalLoadFromSystems; // total heat rejection load from all systems served by this condenser [W]
		Real64 CurMaxCapacity; // current maximum condenser capacity at delta T present for minimum condensing temperature [W]

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		//Initialize this condenser for this time step
		TotalCondenserPumpPower = 0.0;
		TotalBasinHeatPower = 0.0;
		TotalCondenserHeat = 0.0;
		TotalEvapWaterUseRate = 0.0;
		AirVolRatio = 1.0;
		ActualFanPower = 0.0;
		TotalCondDefrostCredit = 0.0;
		TotalLoadFromSystems = 0.0;
		EvapAvail = true;
		CondID = System( SysNum ).CondenserNum( 1 );
		auto & condenser( Condenser( CondID ) );
		RatedFanPower = condenser.RatedFanPower;
		RatedAirFlowRate = condenser.RatedAirFlowRate;
		FanMinAirFlowRatio = condenser.FanMinAirFlowRatio;
		CondCreditWarnIndex1 = condenser.CondCreditWarnIndex1;
		CondCreditWarnIndex2 = condenser.CondCreditWarnIndex2;
		CondCreditWarnIndex3 = condenser.CondCreditWarnIndex3;
		CondCreditWarnIndex4 = condenser.CondCreditWarnIndex4;
		CondCreditWarnIndex5 = condenser.CondCreditWarnIndex5;
		CondCreditWarnIndex6 = condenser.CondCreditWarnIndex6;
		CondCreditWarnIndex7 = condenser.CondCreditWarnIndex7;

		//Sum total condenser load and defrost credits for all systems connected to this condenser
		//  The system values will match the last time that system was solved, so some of the values may be
		//  from the previous overall solution iteration.  However, solution goes through 3 iterations if
		//  there are any shared condensers, so that's ok.
		for ( Sysloop = 1; Sysloop <= condenser.NumSysAttach; ++Sysloop ) {
			SystemID = condenser.SysNum( Sysloop );
			TotalCondDefCredfromSysID = System( SystemID ).TotalCondDefrostCredit + System( SystemID ).SumCascadeCondCredit;
			TotalCondDefrostCredit += TotalCondDefCredfromSysID;
			TotalLoadFromSysID = System( SystemID ).TotalSystemLoad + System( SystemID ).TotCompPower + System( SystemID ).TotHiStageCompPower + System( SystemID ).PipeHeatLoad;
			TotalLoadFromSystems += TotalLoadFromSysID;
			if ( SystemID == SysNum ) TotalLoadFromThisSystem = TotalLoadFromSysID;
		} // Sysloop over every system connected to this condenser

		// for cascade condensers, condenser defrost credit gets passed on to the primary system condenser
		if ( condenser.CondenserType == RefrigCondenserTypeCascade ) TotalCondDefrostCredit = 0.0;

		// Calculate Total Heat rejection needed.  Assume hermetic compressors - conservative assumption
		// Note that heat rejection load carried by desuperheater hvac coils or water heaters is the
		// lagged variable from the previous time step because these are calculated after the refrigeration
		// system is solved.
		condenser.ExternalHeatRecoveredLoad = condenser.LaggedUsedWaterHeater + condenser.LaggedUsedHVACCoil;
		condenser.InternalHeatRecoveredLoad = TotalCondDefrostCredit;
		condenser.TotalHeatRecoveredLoad = condenser.ExternalHeatRecoveredLoad + TotalCondDefrostCredit;

		TotalCondenserHeat = TotalLoadFromSystems - TotalCondDefrostCredit - condenser.ExternalHeatRecoveredLoad;
		if ( TotalCondenserHeat < 0.0 ) {

			TotalCondenserHeat = 0.0;
			if ( ! WarmupFlag ) {
				ShowRecurringWarningErrorAtEnd( "Refrigeration:System: " + System( SysNum ).Name + ":heat reclaimed(defrost,other purposes) >current condenser load. ", CondCreditWarnIndex1 );
				ShowRecurringContinueErrorAtEnd( "For heat recovered for defrost: ASHRAE rule of thumb: <= 25% of the load on a rack ", CondCreditWarnIndex2 );
				ShowRecurringContinueErrorAtEnd( "should be in defrost at the same time. Consider diversifying defrost schedules.", CondCreditWarnIndex3 );
				ShowRecurringContinueErrorAtEnd( "For heat recovered for other purposes: this warning may be an artifact of refrigeration calculation at the load", CondCreditWarnIndex4 );
				ShowRecurringContinueErrorAtEnd( "time step and heat recovery at the system time step. In that case, and ONLY if it occurs a large number of times", CondCreditWarnIndex5 );
				ShowRecurringContinueErrorAtEnd( "(relative to the number of time steps in the simulation), there may be a mis-match between the", CondCreditWarnIndex6 );
				ShowRecurringContinueErrorAtEnd( "operating schedules of the refrigeration system and the heat recovery load.", CondCreditWarnIndex7 );
			} //not warmup
		} //total condenser heat < 0

		// Water side of water-cooled condensers simulated in SimRefrigCondenser,
		//   Here, we just need load and condensing temperatures.
		//   Condensing temperature a fixed delta (the rated approach temperature) from inlet water temp so long as above minimum.
		//   Note, if condensing temperature falls below minimum, get warning and reset but no change in water-side calculations.
		if ( condenser.CondenserType == RefrigCondenserTypeWater ) {
			// Obtain water-cooled condenser inlet/outlet temps
			InletNode = condenser.InletNode;
			condenser.InletTemp = Node( InletNode ).Temp;
			TCondCalc = Node( InletNode ).Temp + condenser.RatedApproachT;
			if ( ( condenser.InletTemp < condenser.InletTempMin ) || ( TCondCalc < System( SysNum ).TCondenseMin ) ) {
				System( SysNum ).TCondense = System( SysNum ).TCondenseMin;
				// condenser.LowTempWarn += 1;
				if ( condenser.LowTempWarnIndex == 0 ) {
					ShowWarningMessage( "Refrigeration:Condenser:WaterCooled " + condenser.Name );
					ShowContinueError( "Water-cooled condenser inlet temp lower than minimum allowed temp. Check returning water temperature and/or minimum temperature setpoints relative to minimum allowed condensing temperature." );
				}
				ShowRecurringWarningErrorAtEnd( "Refrigeration:Condenser:WaterCooled " + condenser.Name + " - Condenser inlet temp lower than minimum allowed ... continues", condenser.LowTempWarnIndex );
				//END IF
			} else {
				System( SysNum ).TCondense = TCondCalc;
			}

		} else if ( ( condenser.CondenserType == RefrigCondenserTypeAir ) || ( condenser.CondenserType == RefrigCondenserTypeEvap ) ) {
			//Condensing Temp, fan and other aux loads for air-cooled or evap-cooled

			//The rated capacity of air-cooled condenser was adjusted for elevation in get input step
			CapFac = TotalCondenserHeat / condenser.RatedCapacity;
			// See whether condenser is at ground level or if other air conditions(ie node) have been specified.
			//    Note that air-cooled condensers can draw air from, and reject heat to, a conditioned zone
			//    But evaporative condensers cannot.
			// Provides effective condensing temperature for air-cooled condenser (or evap if evap is scheduled off)
			if ( condenser.InletAirNodeNum != 0 ) {
				OutDbTemp = Node( condenser.InletAirNodeNum ).Temp;
				BPress = Node( condenser.InletAirNodeNum ).Press;
				HumRatIn = Node( condenser.InletAirNodeNum ).HumRat;
			} else {
				OutDbTemp = OutDryBulbTemp;
				BPress = OutBaroPress;
				HumRatIn = OutHumRat;
			}
			AirDensity = PsyRhoAirFnPbTdbW( BPress, OutDbTemp, HumRatIn );
			AirDensityDry = PsyRhoAirFnPbTdbW( BPress, OutDbTemp, 0.0 );
			// Evaporative condensers will have their water flow shut off in cold months to avoid
			//  'spectacular' icing problems.  Ideally, the user will use the evaporative schedule input
			//  to set such a schedule.  However, sometimes, users will use a single input deck to model
			//  one building in multiple climates, and may not think to put in such a schedule in the colder
			//  climates.  To accomodate such applications, the variable EvapCutOutTdb is used as an extra
			//  check.

			if ( OutDbTemp < EvapCutOutTdb ) EvapAvail = false;

			// Check schedule to determine evap condenser availability
			// IF schedule exists, evap condenser can be scheduled OFF
			if ( ( condenser.CondenserType == RefrigCondenserTypeEvap ) && ( condenser.EvapSchedPtr > 0 ) && ( GetCurrentScheduleValue( condenser.EvapSchedPtr ) == 0 ) ) EvapAvail = false;

			//Calculate condensing temperatures for air-cooled and evap-cooled
			if ( condenser.CondenserType == RefrigCondenserTypeEvap ) {
				//Manufacturer's HRCF regressed to produce a function of the form:
				// (Tcondense-Twb)=A1 + A2*hrcf + A3/hrcf + A4*Twb
				// HRCF defined as rated capacity divided by load
				// Apply ARI490 elevation correction factor here for evap condenser, then apply hrcf limits
				if ( CapFac > 0.0 ) {
					HRCF = condenser.EvapElevFact / CapFac;
					//Condenser(CondNum)%EvapElevFact=1.0d0-3.074D-5*Elevation
				} else {
					HRCF = MyLargeNumber;
				}
				HRCF = min( HRCF, condenser.MaxCapFacEvap );
				HRCF = max( HRCF, condenser.MinCapFacEvap );
				if ( EvapAvail ) {
					OutWbTemp = PsyTwbFnTdbWPb( OutDbTemp, HumRatIn, BPress );
					SinkTemp = OutWbTemp;
				} else { //evaporative condenser with water spray scheduled off so use Tdb
					HRCF /= 3.0; //reference Menske, cap of evap cond operating dry about 1/3 of rated cap
					HRCF = max( HRCF, condenser.MinCapFacEvap );
					SinkTemp = OutDbTemp;
				} //evap avail, still in evap condenser
				TCondCalc = condenser.EvapCoeff1 + condenser.EvapCoeff2 * HRCF + condenser.EvapCoeff3 / HRCF + ( 1.0 + condenser.EvapCoeff4 ) * SinkTemp;
			} else { //air-cooled condenser
				// MinCondLoad and TempSlope came from condenser capacity curve, using curve backwards
				TCondCalc = OutDbTemp + ( TotalCondenserHeat - condenser.MinCondLoad ) * condenser.TempSlope;
				SinkTemp = OutDbTemp;
			} // if evap-cooled condenser

			// Fan energy calculations apply to both air- and evap-cooled condensers
			// Compare calculated condensing temps to minimum allowed to determine fan power/operating mode
			if ( TCondCalc >= System( SysNum ).TCondenseMin ) {
				System( SysNum ).TCondense = TCondCalc;
				ActualFanPower = RatedFanPower;
				AirVolRatio = 1.0;

			} else { //need to reduce fan speed to reduce air flow and keep Tcond at or above Tcond min
				System( SysNum ).TCondense = System( SysNum ).TCondenseMin;
				TCondCalc = System( SysNum ).TCondenseMin;
				// recalculate CapFac at current delta T
				if ( condenser.CondenserType == RefrigCondenserTypeAir ) {
					CurMaxCapacity = CurveValue( condenser.CapCurvePtr, ( System( SysNum ).TCondenseMin - OutDbTemp ) );
					CapFac = TotalCondenserHeat / CurMaxCapacity;
					AirVolRatio = max( FanMinAirFlowRatio, std::pow( CapFac, CondAirVolExponentDry ) ); //Fans limited by minimum air flow ratio
					AirVolRatio = min( AirVolRatio, 1.0 );
				} else { // condenser.CondenserType == RefrigCondenserTypeEvap
					HRCFFullFlow = HRCF;
					//if evap condenser need to back calculate the operating capacity using HRCF relationship, given known Tcond
					QuadBterm = condenser.EvapCoeff1 - ( System( SysNum ).TCondense - SinkTemp ) + condenser.EvapCoeff4 * SinkTemp;
					Sqrtterm = pow_2( QuadBterm ) - 4.0 * condenser.EvapCoeff2 * condenser.EvapCoeff3;
					if ( Sqrtterm < 0.0 ) { // only happens for very high wet bulb temps
						HRCF = condenser.EvapElevFact * condenser.MaxCapFacEvap;
						if ( ! EvapAvail ) HRCF /= 3.0;
						HRCF = max( HRCF, condenser.MinCapFacEvap );
					} else {
						HRCF = condenser.EvapElevFact * ( -QuadBterm - std::sqrt( Sqrtterm ) ) / ( 2.0 * condenser.EvapCoeff2 );
						if ( ! EvapAvail ) HRCF /= 3.0;
						HRCF = min( HRCF, condenser.MaxCapFacEvap );
						HRCF = max( HRCF, condenser.MinCapFacEvap );
					} //sqrtterm
					CapFac = HRCF / HRCFFullFlow; //note, HRCFFullFlow previously limited between min and max,so can't be zero
					if ( EvapAvail ) {
						AirVolRatio = max( FanMinAirFlowRatio, std::pow( CapFac, CondAirVolExponentEvap ) ); //Fans limited by minimum air flow ratio
					} else { //evap not available
						AirVolRatio = max( FanMinAirFlowRatio, std::pow( CapFac, CondAirVolExponentDry ) ); //Fans limited by minimum air flow ratio
					} //evap available
					AirVolRatio = min( AirVolRatio, 1.0 );
				} // condenser type = RefrigCondenserTypeAir with else for evap

				{ auto const SELECT_CASE_var( condenser.FanSpeedControlType );
				if ( SELECT_CASE_var == FanVariableSpeed ) { //fan power law, adjusted for reality, applies
					FanPowerRatio = std::pow( AirVolRatio, 2.5 );
					ActualFanPower = FanPowerRatio * RatedFanPower;
				} else if ( SELECT_CASE_var == FanConstantSpeed ) {
					ActualFanPower = AirVolRatio * std::exp( 1.0 - AirVolRatio ) * RatedFanPower;
				} else if ( SELECT_CASE_var == FanConstantSpeedLinear ) {
					ActualFanPower = AirVolRatio * RatedFanPower;
				} else if ( SELECT_CASE_var == FanTwoSpeed ) {
					//low speed setting of 1/2 fan speed can give up to 60% of capacity.
					//1/2 speed corresonds to ~1/8 power consumption (FanHalfSpeedRatio = 1/(2**2.5) = 0.1768)
					//dampers are used to control flow within those two ranges as in FanConstantSpeed
					Real64 const air_vol_fan_power_fac( std::exp( 1.0 - AirVolRatio ) * RatedFanPower );
					ActualFanPower = AirVolRatio * air_vol_fan_power_fac;
					if ( CapFac < CapFac60Percent ) ActualFanPower = ( ( AirVolRatio + 0.4 ) * FanHalfSpeedRatio ) * air_vol_fan_power_fac;
				}} // fan speed control type
			} //Tcondense >= Tcondense minimum

			if ( ( condenser.CondenserType == RefrigCondenserTypeEvap ) && ( EvapAvail ) ) {
				// calculate evap water use,  need to include bleed down/purge water as well as water
				// actually evaporated.  Use BAC Engineering Reference value of 3 gpm/100 tons because it's more
				// conservative than the ASHRAE value.
				//  Also, based on experience, running the evap water when outdoor T near freezing
				//  leads to 'spectacular' ice, so schedule evap off when Tdb <=4 C.
				//Calculate bleed/purge rate of water loss as a function of capacity, 3 gpm/100 tons refrigeration
				PurgeRate = TotalCondenserHeat * BleedRateConstant;
				EnthalpyAirIn = PsyHFnTdbW( OutDbTemp, HumRatIn );
				//calculate effectiveness at rated conditions, so use Tcondcalc)
				EnthalpyAtTcond = PsyHFnTdbRhPb( TCondCalc, 1.0, BPress );
				Effectiveness = TotalCondenserHeat / ( RatedAirFlowRate * AirDensity * ( EnthalpyAtTcond - EnthalpyAirIn ) );
				//need to limit max effectiveness for errors due to working beyond limits of HRCF in manuf data
				Effectiveness = min( Effectiveness, 0.9 );
				EnthalpyAirOut = EnthalpyAirIn + Effectiveness * ( EnthalpyAtTcond - EnthalpyAirIn );
				//Air leaving the evaporative condenser is saturated
				TAirOut = PsyTsatFnHPb( EnthalpyAirOut, BPress );
				HumRatOut = PsyWFnTdpPb( TAirOut, BPress );
				TotalEvapWaterUseRate = PurgeRate + RatedAirFlowRate * AirVolRatio * AirDensityDry * ( HumRatOut - HumRatIn ) / RhoH2O( OutWbTemp );
				// assumes evap water pump runs whenever evap cooling is available to minimize scaling
				TotalCondenserPumpPower = condenser.EvapPumpPower;
				// calculate basin water heater load
				if ( TotalCondenserHeat == 0.0 && OutDbTemp < condenser.BasinHeaterSetPointTemp ) {
					TotalBasinHeatPower = max( 0.0, condenser.BasinHeaterPowerFTempDiff * ( condenser.BasinHeaterSetPointTemp - OutDbTemp ) );
					// provide warning if no heater power exists
					if ( TotalBasinHeatPower == 0.0 ) {
						// condenser.EvapFreezeWarn = condenser.EvapFreezeWarn + 1;
						if ( condenser.EvapFreezeWarnIndex == 0 ) {
							ShowWarningMessage( "Refrigeration Condenser " + condenser.Name + " - Evap cooling of condenser underway with no basin heater power" );
							ShowContinueError( "and condenser inlet air dry-bulb temp at or below the basin heater setpoint temperature." );
							ShowContinueErrorTimeStamp( "Continuing simulation." );
						}
						ShowRecurringWarningErrorAtEnd( "Refrigeration Condenser " + condenser.Name + " - Evap cooling of condenser underway with no basin heater power ... continues", condenser.EvapFreezeWarnIndex );
						//END IF  !freeze warnings <= 5
					} // basin power == 0
				} // no load and cold outside
			} //EvapAvail

		} else if ( condenser.CondenserType == RefrigCondenserTypeCascade ) { // continuing Condenser type = water, (evap or air), or cascade
			//Cascade condenser does not iterate.  Condensing temperature specified as a load on higher temp system
			//    or floats to meet other loads on that system
			//therese ** future - here and for new phase change heat exchanger - need to handle unmet loads!

			System( SysNum ).TCondense = condenser.RatedTCondense;

			if ( ( System( SysNum ).NumNonCascadeLoads > 0 ) && ( condenser.CascadeTempControl == CascadeTempFloat ) ) {
				System( SysNum ).TCondense = System( condenser.CascadeSinkSystemID ).TEvapNeeded + condenser.RatedApproachT;
				if ( System( SysNum ).TCondense < System( SysNum ).TCondenseMin ) {
					System( SysNum ).TCondense = System( SysNum ).TCondenseMin;
					ShowRecurringWarningErrorAtEnd( "Refrigeration Condenser " + condenser.Name + " - Cascade condenser floating condensing temperature less than specified minimum condensing temperature. Minimum specified temperature used for system below cascade condenser. No correction made for system absorbing heat rejected by the cascade condenser.", condenser.EvapFreezeWarnIndex );
				} //floating condensing temperature less than specified min for system
			} //floating temperature
		} // Condenser type = water, (evap or air), or cascade

		condenser.ActualFanPower = ActualFanPower;
		condenser.FanElecEnergy = ActualFanPower * LocalTimeStep * SecInHour;
		condenser.EvapWaterConsumpRate = TotalEvapWaterUseRate;
		condenser.EvapWaterConsumption = TotalEvapWaterUseRate * LocalTimeStep * SecInHour;
		condenser.ActualEvapPumpPower = TotalCondenserPumpPower;
		condenser.EvapPumpConsumption = TotalCondenserPumpPower * LocalTimeStep * SecInHour;
		condenser.BasinHeaterPower = TotalBasinHeatPower;
		condenser.BasinHeaterConsumption = TotalBasinHeatPower * LocalTimeStep * SecInHour;
		condenser.CondLoad = TotalCondenserHeat;
		condenser.CondEnergy = TotalCondenserHeat * LocalTimeStep * SecInHour;
		condenser.CondCreditWarnIndex1 = CondCreditWarnIndex1;
		condenser.CondCreditWarnIndex2 = CondCreditWarnIndex2;
		condenser.CondCreditWarnIndex3 = CondCreditWarnIndex3;
		condenser.CondCreditWarnIndex4 = CondCreditWarnIndex4;
		condenser.CondCreditWarnIndex5 = CondCreditWarnIndex5;
		condenser.CondCreditWarnIndex6 = CondCreditWarnIndex6;
		condenser.CondCreditWarnIndex7 = CondCreditWarnIndex7;
		condenser.ExternalEnergyRecovered = condenser.ExternalHeatRecoveredLoad * LocalTimeStep * SecInHour;
		condenser.InternalEnergyRecovered = condenser.InternalHeatRecoveredLoad * LocalTimeStep * SecInHour;
		condenser.TotalHeatRecoveredEnergy = condenser.TotalHeatRecoveredLoad * LocalTimeStep * SecInHour;
		System( SysNum ).NetHeatRejectLoad = TotalCondenserHeat * TotalLoadFromThisSystem / TotalLoadFromSystems;
		System( SysNum ).NetHeatRejectEnergy = System( SysNum ).NetHeatRejectLoad * LocalTimeStep * SecInHour;

		//set water system demand request (if needed)
		if ( condenser.EvapWaterSupplyMode == WaterSupplyFromTank ) {
			WaterStorage( condenser.EvapWaterSupTankID ).VdotRequestDemand( condenser.EvapWaterTankDemandARRID ) = condenser.EvapWaterConsumpRate;
		}

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcGasCooler( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brian A. Fricke, ORNL
		//       DATE WRITTEN   Fall 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the gas cooler outlet temperature, the optimum gas cooler pressure, heat rejection,
		// fan power, and fan energy for a detailed transcritical CO2 refrigeration system.

		// METHODOLOGY EMPLOYED:
		// For a specified gas cooler outlet temperature in transcritical operation, there is an optimal gas cooler
		// pressure which produces the highest COP. A curve-fit equation similar to that presented by Ge and Tassou
		// (2011) and Sawalha (2008) is used to determine the optimum gas cooler pressure for a given gas cooler
		// outlet temperature. In subcritical operation, the condensing temperature and pressure are allowed to
		// float with ambient conditions, above the minimum condensing temperature.

		// REFERENCES:
		// Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
		//     systems with supermarket model "SuperSim", Part I: Model description and validation. International
		//     Journal of Refrigeration 34: 527-539.
		// Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
		//     systems with supermarket model "SuperSim", Part II: Model applications. International Journal of
		//     Refrigeration 34: 540-549.
		// Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
		//     Part I: Modeling, simulation and optimization of two system solutions. International Journal of
		//     Refrigeration 31: 516-524.
		// Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
		//     Part II: System modifications and comparisons of different solutions. International Journal of
		//     Refrigeration 31: 525-534.

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "RefrigeratedCase:CalcGasCooler" );

		int GasCoolerCreditWarnIndex; // Warning counter
		int GasCoolerID; // Gas cooler number
		int Sysloop; // Counter over number of systems attached to this gas cooler
		int SystemID; // System number rejecting heat to this gas cooler
		Real64 ActualFanPower; // Fan power after adjustments for partially loaded gas cooler [W]
		Real64 AirVolRatio; // Ratio of air volume needed to remove load relative to design load
		Real64 CapFac; // Capacity factor
		Real64 FanMinAirFlowRatio; // Minimum fan air flow ratio
		Real64 FanPowerRatio; // Calculated fan power ratio
		Real64 LocalTimeStep; // Set equal to either TimeStepSys or TimeStepZone
		Real64 OutDbTemp; // Outdoor dry bulb temperature at gas cooler air inlet node [C]
		Real64 RatedFanPower; // Rated fan power for this gas cooler [W]
		Real64 TotalCondDefCredfromSysID; // Gas cooler defrost credit for single system [W]
		Real64 TotalCondDefrostCredit; // Total gas cooler credit from hot gas/brine defrost for cases etc. served
		//     directly by all systems served by this gas cooler [W]
		Real64 TotalGasCoolerHeat; // Total gas cooler heat from system [W]
		Real64 TotalLoadFromSysID; // Total heat rejection load from a single detailed system [W]
		Real64 TotalLoadFromSystems; // Total heat rejection load from all systems served by this condenser [W]
		Real64 TotalLoadFromThisSystem( 0.0 ); // Total heat rejection load from the detailed system identified in subroutine call [W]

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		//!Initialize this gas cooler for this time step
		TotalGasCoolerHeat = 0.0;
		AirVolRatio = 1.0;
		ActualFanPower = 0.0;
		TotalCondDefrostCredit = 0.0;
		TotalLoadFromSystems = 0.0;
		GasCoolerID = TransSystem( SysNum ).GasCoolerNum( 1 );
		RatedFanPower = GasCooler( GasCoolerID ).RatedFanPower;
		FanMinAirFlowRatio = GasCooler( GasCoolerID ).FanMinAirFlowRatio;
		GasCoolerCreditWarnIndex = GasCooler( GasCoolerID ).GasCoolerCreditWarnIndex;

		for ( Sysloop = 1; Sysloop <= GasCooler( GasCoolerID ).NumSysAttach; ++Sysloop ) {
			SystemID = GasCooler( GasCoolerID ).SysNum( Sysloop );
			TotalCondDefCredfromSysID = TransSystem( SystemID ).TotalCondDefrostCredit;
			TotalCondDefrostCredit += TotalCondDefCredfromSysID;
			TotalLoadFromSysID = TransSystem( SystemID ).TotalSystemLoadLT + TransSystem( SystemID ).TotalSystemLoadMT + TransSystem( SystemID ).TotCompPowerLP + TransSystem( SystemID ).TotCompPowerHP + TransSystem( SystemID ).PipeHeatLoadLT + TransSystem( SystemID ).PipeHeatLoadMT;
			TotalLoadFromSystems += TotalLoadFromSysID;
			if ( SystemID == SysNum ) TotalLoadFromThisSystem = TotalLoadFromSysID;
		} // Sysloop over every system connected to this gas cooler

		// Calculate Total Heat rejection needed.
		GasCooler( GasCoolerID ).InternalHeatRecoveredLoad = TotalCondDefrostCredit;
		GasCooler( GasCoolerID ).TotalHeatRecoveredLoad = TotalCondDefrostCredit;
		TotalGasCoolerHeat = TotalLoadFromSystems - TotalCondDefrostCredit;

		if ( TotalGasCoolerHeat < 0.0 ) {
			TotalGasCoolerHeat = 0.0;
			if ( ! WarmupFlag ) ShowRecurringWarningErrorAtEnd( "Refrigeration:TranscriticalSystem: " + TransSystem( SysNum ).Name + ":heat reclaimed (defrost,other purposes) is greater than current gas cooler load. ASHRAE rule of thumb: <= 25% of the load on a system should be in defrost at the same time. Consider diversifying defrost schedules.", GasCoolerCreditWarnIndex );
		} //total gas cooler heat < 0

		//The rated capacity of air-cooled gas cooler was adjusted for elevation in get input step
		CapFac = TotalGasCoolerHeat / GasCooler( GasCoolerID ).RatedCapacity;
		// See whether gas cooler is at ground level or if other air conditions (ie node) have been specified.
		// Note that air-cooled gas coolers can draw air from, and reject heat to, a conditioned zone.
		if ( GasCooler( GasCoolerID ).InletAirNodeNum != 0 ) {
			OutDbTemp = Node( GasCooler( GasCoolerID ).InletAirNodeNum ).Temp;
		} else {
			OutDbTemp = OutDryBulbTemp;
		}
		// Determine gas cooler outlet temperature and pressure
		// Transcritical:  Gas cooler outlet temperature based on ambient temperature and approach temperature.
		//                 Determine optimum gas cooler pressure to maximize COP.
		// Subcritical:  Allow condensing temperature and pressure to float between minimum condensing temperature and
		//               transition temperature.
		if ( OutDbTemp > GasCooler( GasCoolerID ).TransitionTemperature ) { // Gas cooler in transcritical operation
			GasCooler( GasCoolerID ).TGasCoolerOut = OutDbTemp + GasCooler( GasCoolerID ).GasCoolerApproachT;
			GasCooler( GasCoolerID ).PGasCoolerOut = 1.0e5 * ( 2.3083 * OutDryBulbTemp + 11.9 );
			if ( GasCooler( GasCoolerID ).PGasCoolerOut < 7.5e6 ) { // Ensure gas cooler pressure is at least 7.5 MPa for transcritical operation
				GasCooler( GasCoolerID ).PGasCoolerOut = 7.5e6;
			}
			GasCooler( GasCoolerID ).HGasCoolerOut = GetSupHeatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( GasCoolerID ).TGasCoolerOut, GasCooler( GasCoolerID ).PGasCoolerOut, TransSystem( SysNum ).RefIndex, RoutineName );
			GasCooler( GasCoolerID ).TransOpFlag = true;
		} else { // Gas cooler in subcritical operation
			GasCooler( GasCoolerID ).TGasCoolerOut = OutDbTemp + GasCooler( GasCoolerID ).SubcriticalTempDiff;
			if ( GasCooler( GasCoolerID ).TGasCoolerOut > 30.978 ) { //  Gas temperature should be less than critical temperature
				GasCooler( GasCoolerID ).PGasCoolerOut = 7.2e6; //  Fix the pressure to be subcritical
				GasCooler( GasCoolerID ).TGasCoolerOut = GetSatTemperatureRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( GasCoolerID ).PGasCoolerOut, TransSystem( SysNum ).RefIndex, RoutineName );
			} else if ( GasCooler( GasCoolerID ).TGasCoolerOut > GasCooler( GasCoolerID ).MinCondTemp ) { //  Allow condensing temperature to float above the minimum
				GasCooler( GasCoolerID ).PGasCoolerOut = GetSatPressureRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( GasCoolerID ).TGasCoolerOut, TransSystem( SysNum ).RefIndex, RoutineName );
			} else { //  Don't allow condensing temperature to drop below minimum
				GasCooler( GasCoolerID ).TGasCoolerOut = GasCooler( GasCoolerID ).MinCondTemp;
				GasCooler( GasCoolerID ).PGasCoolerOut = GetSatPressureRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( GasCoolerID ).TGasCoolerOut, TransSystem( SysNum ).RefIndex, RoutineName );
			}
			GasCooler( GasCoolerID ).HGasCoolerOut = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( GasCoolerID ).TGasCoolerOut, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
			GasCooler( GasCoolerID ).TransOpFlag = false;
		} // (OutDbTemp > TransitionTemperature)

		if ( GasCooler( GasCoolerID ).TGasCoolerOut < 30.978 ) {
			GasCooler( GasCoolerID ).CpGasCoolerOut = GetSatSpecificHeatRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( GasCoolerID ).TGasCoolerOut, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
		} else {
			GasCooler( GasCoolerID ).CpGasCoolerOut = 0.0;
		}

		// Gas cooler fan energy calculations
		AirVolRatio = max( FanMinAirFlowRatio, std::pow( CapFac, CondAirVolExponentDry ) ); //Fans limited by minimum air flow ratio

		{ auto const SELECT_CASE_var( GasCooler( GasCoolerID ).FanSpeedControlType );
		if ( SELECT_CASE_var == FanVariableSpeed ) { //fan power law, adjusted for reality, applies
			FanPowerRatio = std::pow( AirVolRatio, 2.5 );
			ActualFanPower = FanPowerRatio * RatedFanPower;
		} else if ( SELECT_CASE_var == FanConstantSpeed ) {
			ActualFanPower = AirVolRatio * std::exp( 1.0 - AirVolRatio ) * RatedFanPower;
		} else if ( SELECT_CASE_var == FanConstantSpeedLinear ) {
			ActualFanPower = AirVolRatio * RatedFanPower;
		} else if ( SELECT_CASE_var == FanTwoSpeed ) {
			//low speed setting of 1/2 fan speed can give up to 60% of capacity.
			//1/2 speed corresonds to ~1/8 power consumption (FanHalfSpeedRatio = 1/(2**2.5) = 0.1768)
			//dampers are used to control flow within those two ranges as in FanConstantSpeed
			ActualFanPower = AirVolRatio * std::exp( 1.0 - AirVolRatio ) * RatedFanPower;
			if ( CapFac < CapFac60Percent ) ActualFanPower = ( ( AirVolRatio + 0.4 ) * ( FanHalfSpeedRatio ) ) * std::exp( 1.0 - AirVolRatio ) * RatedFanPower;
		}} // fan speed control type

		GasCooler( GasCoolerID ).ActualFanPower = ActualFanPower;
		GasCooler( GasCoolerID ).FanElecEnergy = ActualFanPower * LocalTimeStep * SecInHour;
		GasCooler( GasCoolerID ).GasCoolerLoad = TotalGasCoolerHeat;
		GasCooler( GasCoolerID ).GasCoolerEnergy = TotalGasCoolerHeat * LocalTimeStep * SecInHour;
		GasCooler( GasCoolerID ).GasCoolerCreditWarnIndex = GasCoolerCreditWarnIndex;
		GasCooler( GasCoolerID ).InternalEnergyRecovered = GasCooler( GasCoolerID ).InternalHeatRecoveredLoad * LocalTimeStep * SecInHour;
		GasCooler( GasCoolerID ).TotalHeatRecoveredEnergy = GasCooler( GasCoolerID ).TotalHeatRecoveredLoad * LocalTimeStep * SecInHour;
		TransSystem( SysNum ).NetHeatRejectLoad = TotalGasCoolerHeat * TotalLoadFromThisSystem / TotalLoadFromSystems;
		TransSystem( SysNum ).NetHeatRejectEnergy = TransSystem( SysNum ).NetHeatRejectLoad * LocalTimeStep * SecInHour;

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateCompressors( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
		//       DATE WRITTEN   Spring 2008
		//       MODIFIED       Brian Fricke, ORNL, March 2012, added two-stage compression
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the Compressor power, energy, capacity, and efficiency for a particular detailed
		// refrigeration system.  Routine is capable of modeling single-stage and two-stage
		// compression refrigeration systems.

		// METHODOLOGY EMPLOYED:
		// USe ARI compressor performance curves, the evaporating temperature and condensing temperature

		// REFERENCES:
		// "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
		//  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

		// ARI Standard 540, 2004, Standard for Performance Rating of Positive Displacement Refrigerant
		//  Comprssors and Compressor Units, Air-Conditionig & Refrigeration Institute,Arlington VA

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// Following constants approp for R22, R134a, R404a, R507, R410a, R407c, future allow input?
		//   May want to allow input to reflect larger pipes selected to reduce delta P and increase compressor efficiency.
		//NOTE, these DelT...Pipes reflect the decrease in Pressure in the pipes, NOT thermal transfer through the pipe walls.
		Real64 const DelTSuctPipes( 1.0 ); // Tsat drop corresponding to P drop in suction pipes, ASHRAE 2006 p 2.4 (C)
		Real64 const DelTDischPipes( 0.5 ); // Tsat drop corresponding to P drop in discharge pipes, ASHRAE 2006 p 2.5 (C)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "RefrigeratedCase:CalculateCompressors" );
		int CompIndex; // Compressor index within system
		int CompID; // Compressor index within all compressors
		int CondID; // Condenser index for this refrigeration system
		Real64 CaseEnthalpyChangeRated; // Enthalpy change in cases at compressor rated cond, J/kg
		Real64 CapacityCorrection; // Capacity at existing subcool/superheat over cap at rated conditions
		Real64 CpSatVapCondense; // Specific heat of vapor at cond temp J/kg-C
		Real64 DensityRated; // Density of inlet gas at rated superheat, m3/kg
		Real64 DensityActual; // Density of superheated gas at compressor inlet, m3/kg
		Real64 HCompInRated( 0.0 ); // Enthalpy entering compressor at rated superheat, J/kg //Autodesk:Init
		Real64 HCaseInRated( 0.0 ); // Enthalpy entering cases at rated subcooling, J/kg //Autodesk:Init
		Real64 HSatVapCondense; // Enthalpy of saturated vapor at T condense, J/kg
		Real64 HsatVaporforTevapneeded; // Enthalpy saturated vapor at temperature needed at evaporator
		Real64 LFLastComp; // Load factor for last compressor dispatched
		static Real64 LocalTimeStep( 0.0 ); // TimeStepZone for case/walkin systems, TimeStepSys for coil systems
		Real64 MassCorrection; // Mass flow at existing subcool/superheat over cap at rated conditions
		Real64 NeededCapacity; // Sum of case loads and mech subcooler loads on suction group
		Real64 PSuction; // Suction Pressure
		Real64 PCond; // Condensing pressure
		Real64 PEvap; // Evaporating pressure
		Real64 TCompOutEstimate; // Estimated temperature out of the compressor, used to flag whether heat reclaim is reasonable, C
		Real64 TempInRated( 0.0 ); // Temperature entering compressor at rated superheat, C //Autodesk:Init
		Real64 TotalEnthalpyChangeActual; // Actual enthalpy change in cases and cold side of LSHX, J/kg
		Real64 TsatforPsuct; // Tsat for PSuction, C
		Real64 TsatforPdisch; // Tsat for Pdischarge, c
		int StageIndex; // Compression stage index
		int NumComps; // Number of low-stage or high-stage compressors in system
		Real64 HHiStageCompIn; // Enthalpy at inlet of high-stage compressor (J/kg)
		Real64 HCaseInRated_base( 0.0 ), HCompInRated_base( 0.0 );  //Autodesk:Tuned Intermediate values for performance tuning

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;
		Real64 const LocalTimeStepSec( LocalTimeStep * SecInHour );

		auto & System_SysNum( System( SysNum ) );
		auto const NumStages( System_SysNum.NumStages );
		auto const & RefrigerantName( System_SysNum.RefrigerantName );
		auto const TEvapNeeded( System_SysNum.TEvapNeeded );
		auto const TCondense( System_SysNum.TCondense );
		auto & RefIndex( System_SysNum.RefIndex );
		auto & TIntercooler( System_SysNum.TIntercooler );
		auto & HSatLiqCond( System_SysNum.HSatLiqCond );
		auto & CpSatLiqCond( System_SysNum.CpSatLiqCond );
		CondID = System_SysNum.CondenserNum( 1 );
		auto const & Condenser1( Condenser( CondID ) );
		Real64 const AccumLoad = max( 0.0, ( System_SysNum.UnmetEnergy / LocalTimeStepSec ) ); // Load due to previously unmet compressor loads
		Real64 const NeededCapacity_base( System_SysNum.TotalSystemLoad + AccumLoad + System_SysNum.PipeHeatLoad + System_SysNum.LSHXTrans );

		//Before dispatching compressors, zero sum of compressor outputs and zero each compressor
		System_SysNum.TotCompCapacity = 0.0;
		System_SysNum.RefMassFlowComps = 0.0;
		System_SysNum.TotCompPower = 0.0;
		if ( NumStages == 2 ) {
			System_SysNum.TotHiStageCompCapacity = 0.0;
			System_SysNum.RefMassFlowHiStageComps = 0.0;
			System_SysNum.TotHiStageCompPower = 0.0;
		}

		for ( CompIndex = 1; CompIndex <= System_SysNum.NumCompressors; ++CompIndex ) {
			CompID = System_SysNum.CompressorNum( CompIndex );
			auto & Compressor_CompID( Compressor( CompID ) );
			Compressor_CompID.Power = 0.0;
			Compressor_CompID.MassFlow = 0.0;
			Compressor_CompID.Capacity = 0.0;
			Compressor_CompID.ElecConsumption = 0.0;
			Compressor_CompID.CoolingEnergy = 0.0;
			Compressor_CompID.LoadFactor = 0.0;
		}
		if ( NumStages == 2 ) {
			for ( CompIndex = 1; CompIndex <= System_SysNum.NumHiStageCompressors; ++CompIndex ) {
				CompID = System_SysNum.HiStageCompressorNum( CompIndex );
				auto & Compressor_CompID( Compressor( CompID ) );
				Compressor_CompID.Power = 0.0;
				Compressor_CompID.MassFlow = 0.0;
				Compressor_CompID.Capacity = 0.0;
				Compressor_CompID.ElecConsumption = 0.0;
				Compressor_CompID.CoolingEnergy = 0.0;
				Compressor_CompID.LoadFactor = 0.0;
			}
		}

		// Determine properties at case inlet and compressor inlet
		for ( StageIndex = 1; StageIndex <= min( 2, NumStages ); ++StageIndex ) {
			if ( StageIndex == 1 ) { // Do single-stage or low-stage calculations
				if ( NumStages == 1 ) { // Single-stage system
					NeededCapacity = NeededCapacity_base; //because compressor capacity rated from txv to comp inlet
					TsatforPdisch = TCondense + DelTDischPipes; //need (Psat of (Tcond + delT corresponding to delP disch Pipes))
					TsatforPsuct = TEvapNeeded - DelTSuctPipes; //need (Psat of (Tevap - delT corresponding to del P suct Pipes))
					HsatVaporforTevapneeded = GetSatEnthalpyRefrig( RefrigerantName, TEvapNeeded, 1.0, RefIndex, RoutineName );
					HSatLiqCond = GetSatEnthalpyRefrig( RefrigerantName, TCondense, 0.0, RefIndex, RoutineName );
					CpSatLiqCond = GetSatSpecificHeatRefrig( RefrigerantName, TCondense, 0.0, RefIndex, RoutineName );
					//HCaseIn is a function of the condenser rated subcooling, not the compressor rated subcooling
					//TCompIn needs to include case superheat as well as Temp change from lshx subcoolers
					//Calculate both here unless set previously by subcooler subroutine
					//HCaseOut corresponds to (tevapneeded + case superheat)
					//future - visit how parameter 'casesuperheat' applies when using walk-ins or transfer loads
					if ( System_SysNum.NumSubcoolers == 0 ) { // No subcooler on this system
						System_SysNum.HCaseIn = HSatLiqCond - CpSatLiqCond * Condenser1.RatedSubcool;
						System_SysNum.TCompIn = TEvapNeeded + CaseSuperheat; //+
						System_SysNum.TLiqInActual = TCondense - Condenser1.RatedSubcool;
						System_SysNum.HCompIn = System_SysNum.HCaseOut;
					} else { //subcooler subroutine has been called to calc TCompIn and HCaseIn
						System_SysNum.HCompIn = System_SysNum.HCaseOut + System_SysNum.CpSatVapEvap * ( System_SysNum.TCompIn - ( TEvapNeeded + CaseSuperheat ) );
					} // whether or not subcooler routine used
					PSuction = GetSatPressureRefrig( RefrigerantName, TsatforPsuct, RefIndex, RoutineName );
					NumComps = System_SysNum.NumCompressors;
				} else { // Low-stage side of two-stage system
					PCond = GetSatPressureRefrig( RefrigerantName, TCondense, RefIndex, RoutineName );
					PEvap = GetSatPressureRefrig( RefrigerantName, TEvapNeeded, RefIndex, RoutineName );
					System_SysNum.PIntercooler = std::sqrt( PCond * PEvap );
					TIntercooler = GetSatTemperatureRefrig( RefrigerantName, System_SysNum.PIntercooler, RefIndex, RoutineName );
					NeededCapacity = NeededCapacity_base; //because compressor capacity rated from txv to comp inlet
					TsatforPdisch = TIntercooler + DelTDischPipes; //need (Psat of (Tinter + delT corresponding to delP disch Pipes))
					TsatforPsuct = TEvapNeeded - DelTSuctPipes; //need (Psat of (Tevap - delT corresponding to del P suct Pipes))
					HsatVaporforTevapneeded = GetSatEnthalpyRefrig( RefrigerantName, TEvapNeeded, 1.0, RefIndex, RoutineName );
					HSatLiqCond = GetSatEnthalpyRefrig( RefrigerantName, TCondense, 0.0, RefIndex, RoutineName );
					CpSatLiqCond = GetSatSpecificHeatRefrig( RefrigerantName, TCondense, 0.0, RefIndex, RoutineName );
					//HCaseIn is a function of the condenser rated subcooling, not the compressor rated subcooling
					//TCompIn needs to include case superheat as well as Temp change from lshx subcoolers
					//Calculate both here unless set previously by subcooler subroutine
					//HCaseOut corresponds to (tevapneeded + case superheat)
					if ( System_SysNum.NumSubcoolers == 0 ) { // No subcooler on this system
						if ( System_SysNum.IntercoolerType == 1 ) { // Flash Intercooler
							System_SysNum.HCaseIn = GetSatEnthalpyRefrig( RefrigerantName, TIntercooler, 0.0, RefIndex, RoutineName );
							System_SysNum.TLiqInActual = TIntercooler;
						} else if ( System_SysNum.IntercoolerType == 2 ) { // Shell-and-Coil Intercooler
							System_SysNum.TLiqInActual = TCondense - Condenser1.RatedSubcool - System_SysNum.IntercoolerEffectiveness * ( TCondense - Condenser1.RatedSubcool - TIntercooler );
							System_SysNum.HCaseIn = HSatLiqCond - CpSatLiqCond * ( TCondense - System_SysNum.TLiqInActual );
						} // IntercoolerType
						System_SysNum.TCompIn = TEvapNeeded + CaseSuperheat; //+
						System_SysNum.HCompIn = System_SysNum.HCaseOut;
					} else { //subcooler subroutine has been called to calc TCompIn and HCaseIn
						System_SysNum.HCompIn = System_SysNum.HCaseOut + System_SysNum.CpSatVapEvap * ( System_SysNum.TCompIn - ( TEvapNeeded + CaseSuperheat ) );
					} // whether or not subcooler routine used
					PSuction = GetSatPressureRefrig( RefrigerantName, TsatforPsuct, RefIndex, RoutineName );
					NumComps = System_SysNum.NumCompressors;
				} // NumStages
			} else { // Two-stage system, high-stage side
				NeededCapacity = NeededCapacity_base + System_SysNum.TotCompPower;
				TsatforPdisch = TCondense + DelTDischPipes;
				TsatforPsuct = TIntercooler;
				HsatVaporforTevapneeded = GetSatEnthalpyRefrig( RefrigerantName, TIntercooler, 1.0, RefIndex, RoutineName );
//				HSatLiqCond = GetSatEnthalpyRefrig( RefrigerantName, TCondense, 0.0, RefIndex, RoutineName ); 	  //Autodesk:Tuned These don't change for 2nd stage
//				CpSatLiqCond = GetSatSpecificHeatRefrig( RefrigerantName, TCondense, 0.0, RefIndex, RoutineName ); //Autodesk:Tuned These don't change for 2nd stage
				System_SysNum.HCaseIn = HSatLiqCond - CpSatLiqCond * Condenser1.RatedSubcool;
				System_SysNum.TCompIn = TIntercooler;
				//      System(SysNum)%TLiqInActual = System(SysNum)%TCondense-Condenser(System(SysNum)%CondenserNum(1))%RatedSubcool
				System_SysNum.HCompIn = HsatVaporforTevapneeded;
				PSuction = GetSatPressureRefrig( RefrigerantName, TsatforPsuct, RefIndex, RoutineName );
				NumComps = System_SysNum.NumHiStageCompressors;
			} // StageIndex

			//dispatch compressors to meet load, note they were listed in compressor list in dispatch order
			DensityActual = GetSupHeatDensityRefrig( RefrigerantName, System_SysNum.TCompIn, PSuction, RefIndex, RoutineName ); //Autodesk:Tuned Hoisted out of CompIndex loop
			TotalEnthalpyChangeActual = System_SysNum.HCompIn - System_SysNum.HCaseIn; //Autodesk:Tuned Hoisted out of CompIndex loop
			if ( NumStages == 2 ) { //Autodesk:Tuned Hoisted out of CompIndex loop
				if ( StageIndex == 1 ) {
					HCaseInRated_base = GetSatEnthalpyRefrig( RefrigerantName, TIntercooler, 0.0, RefIndex, RoutineName );
				} else if ( StageIndex == 2 ) {
					HCompInRated_base = GetSatEnthalpyRefrig( RefrigerantName, TIntercooler, 1.0, RefIndex, RoutineName );
				}
			}
			for ( CompIndex = 1; CompIndex <= NumComps; ++CompIndex ) {
				if ( StageIndex == 1 ) {
					CompID = System_SysNum.CompressorNum( CompIndex );
				} else {
					CompID = System_SysNum.HiStageCompressorNum( CompIndex );
				} // StageIndex
				auto & Compressor_CompID( Compressor( CompID ) );

				//need to use indiv compressor's rated subcool and superheat to adjust capacity to actual conditions
				{ auto const SELECT_CASE_var( Compressor_CompID.SubcoolRatingType );
				if ( SELECT_CASE_var == RatedSubcooling ) {
					if ( NumStages == 1 ) { // Single-stage system
						HCaseInRated = HSatLiqCond - CpSatLiqCond * Compressor_CompID.RatedSubcool;
					} else if ( NumStages == 2 && StageIndex == 1 ) { // Two-stage system, low-stage side
						HCaseInRated = HCaseInRated_base - CpSatLiqCond * Compressor_CompID.RatedSubcool;
					} else if ( NumStages == 2 && StageIndex == 2 ) { // Two-stage system, high-stage side
						HCaseInRated = HSatLiqCond - CpSatLiqCond * Compressor_CompID.RatedSubcool;
					} // NumStages
				} else if ( SELECT_CASE_var == RatedLiquidTemperature ) { //have rated liquid temperature stored in "RatedSubcool"
					if ( NumStages == 1 ) { // Single-stage system
						HCaseInRated = HSatLiqCond - CpSatLiqCond * ( TCondense - Compressor_CompID.RatedSubcool );
					} else if ( NumStages == 2 && StageIndex == 1 ) { // Two-stage system, low-stage side
						HCaseInRated = HCaseInRated_base - CpSatLiqCond * ( TIntercooler - Compressor_CompID.RatedSubcool );
					} else if ( NumStages == 2 && StageIndex == 2 ) { // Two-stage system, high-stage side
						HCaseInRated = HSatLiqCond - CpSatLiqCond * ( TCondense - Compressor_CompID.RatedSubcool );
					} // NumStages
				}} // Compressor SubcoolRatingType
				{ auto const SELECT_CASE_var( Compressor_CompID.SuperheatRatingType );
				if ( SELECT_CASE_var == RatedSuperheat ) {
					if ( NumStages == 1 ) { // Single-stage system
						HCompInRated = HsatVaporforTevapneeded + System_SysNum.CpSatVapEvap * Compressor_CompID.RatedSuperheat;
						TempInRated = TEvapNeeded + Compressor_CompID.RatedSuperheat;
					} else if ( NumStages == 2 && StageIndex == 1 ) { // Two-stage system, low-stage side
						HCompInRated = HsatVaporforTevapneeded + System_SysNum.CpSatVapEvap * Compressor_CompID.RatedSuperheat;
						TempInRated = TEvapNeeded + Compressor_CompID.RatedSuperheat;
					} else if ( NumStages == 2 && StageIndex == 2 ) { // Two-stage system, high-stage side
						HCompInRated = HCompInRated_base + System_SysNum.CpSatVapEvap * Compressor_CompID.RatedSuperheat;
						TempInRated = TIntercooler + Compressor_CompID.RatedSuperheat;
					} // NumStages
				} else if ( SELECT_CASE_var == RatedReturnGasTemperature ) { //have rated compressor inlet temperature stored in "RatedSuperheat"
					if ( NumStages == 1 ) { // Single-stage system
						TempInRated = Compressor_CompID.RatedSuperheat;
						HCompInRated = HsatVaporforTevapneeded + System_SysNum.CpSatVapEvap * ( TempInRated - TEvapNeeded );
					} else if ( NumStages == 2 && StageIndex == 1 ) { // Two-stage system, low-stage side
						TempInRated = Compressor_CompID.RatedSuperheat;
						HCompInRated = HsatVaporforTevapneeded + System_SysNum.CpSatVapEvap * ( TempInRated - TEvapNeeded );
					} else if ( NumStages == 2 && StageIndex == 2 ) { // Two-stage system, high-stage side
						TempInRated = Compressor_CompID.RatedSuperheat;
						HCompInRated = HsatVaporforTevapneeded + System_SysNum.CpSatVapEvap * ( TempInRated - TIntercooler );
					} // NumStages
				}} // Compressor SuperheatRatingType

				CaseEnthalpyChangeRated = HCompInRated - HCaseInRated;
				DensityRated = GetSupHeatDensityRefrig( RefrigerantName, TempInRated, PSuction, RefIndex, RoutineName );
				//  Adjust capacity and mass flow to reflect the specific volume change due to superheating and
				//  the increase in capacity due to extra subcooling
				MassCorrection = DensityActual / DensityRated;
				CapacityCorrection = MassCorrection * TotalEnthalpyChangeActual / CaseEnthalpyChangeRated;
				Compressor_CompID.Power = CurveValue( Compressor_CompID.ElecPowerCurvePtr, TsatforPsuct, TsatforPdisch );
				Compressor_CompID.Capacity = CapacityCorrection * CurveValue( Compressor_CompID.CapacityCurvePtr, TsatforPsuct, TsatforPdisch );
				Compressor_CompID.MassFlow = Compressor_CompID.Capacity / TotalEnthalpyChangeActual;

				// calculate load factor for last compressor addded
				// assumes either cycling or part load eff = full load eff for last compressor
				if ( StageIndex == 1 ) { // Single-stage or low-stage compressors
					if ( ( System_SysNum.TotCompCapacity + Compressor_CompID.Capacity ) >= NeededCapacity ) {
						LFLastComp = ( NeededCapacity - System_SysNum.TotCompCapacity ) / Compressor_CompID.Capacity;
						Compressor_CompID.Power *= LFLastComp;
						Compressor_CompID.MassFlow *= LFLastComp;
						Compressor_CompID.Capacity *= LFLastComp;
						System_SysNum.TotCompCapacity += Compressor_CompID.Capacity;
						System_SysNum.RefMassFlowComps += Compressor_CompID.MassFlow;
						System_SysNum.TotCompPower += Compressor_CompID.Power;
						Compressor_CompID.ElecConsumption = Compressor_CompID.Power * LocalTimeStepSec;
						Compressor_CompID.CoolingEnergy = Compressor_CompID.Capacity * LocalTimeStepSec;
						Compressor_CompID.LoadFactor = LFLastComp;
						break; //numcomps do
					} else { //>= needed capacity
						System_SysNum.TotCompCapacity += Compressor_CompID.Capacity;
						System_SysNum.RefMassFlowComps += Compressor_CompID.MassFlow;
						System_SysNum.TotCompPower += Compressor_CompID.Power;
					} //>= needed capacity
				} else { // high-stage compressors (for two-stage systems only)
					if ( ( System_SysNum.TotHiStageCompCapacity + Compressor_CompID.Capacity ) >= NeededCapacity ) {
						LFLastComp = ( NeededCapacity - System_SysNum.TotHiStageCompCapacity ) / Compressor_CompID.Capacity;
						Compressor_CompID.Power *= LFLastComp;
						Compressor_CompID.MassFlow *= LFLastComp;
						Compressor_CompID.Capacity *= LFLastComp;
						System_SysNum.TotHiStageCompCapacity += Compressor_CompID.Capacity;
						System_SysNum.RefMassFlowHiStageComps += Compressor_CompID.MassFlow;
						System_SysNum.TotHiStageCompPower += Compressor_CompID.Power;
						System_SysNum.FlowRatioIntercooler = System_SysNum.RefMassFlowComps / System_SysNum.RefMassFlowHiStageComps;
						Compressor_CompID.ElecConsumption = Compressor_CompID.Power * LocalTimeStepSec;
						Compressor_CompID.CoolingEnergy = Compressor_CompID.Capacity * LocalTimeStepSec;
						Compressor_CompID.LoadFactor = LFLastComp;
						break; //numcomps do
					} else { //>= needed capacity
						System_SysNum.TotHiStageCompCapacity += Compressor_CompID.Capacity;
						System_SysNum.RefMassFlowHiStageComps += Compressor_CompID.MassFlow;
						System_SysNum.TotHiStageCompPower += Compressor_CompID.Power;
					} //>= needed capacity
				} // StageIndex
				Compressor_CompID.ElecConsumption = Compressor_CompID.Power * LocalTimeStepSec;
				Compressor_CompID.CoolingEnergy = Compressor_CompID.Capacity * LocalTimeStepSec;
				Compressor_CompID.LoadFactor = 1.0;
			} // NumComps
		}

		//Calculate enthalpy at compressor discharge
		if ( NumStages == 1 ) { // Single-stage or low-stage compressors
			System_SysNum.HCompOut = System_SysNum.HCompIn + System_SysNum.TotCompPower / System_SysNum.RefMassFlowComps;
			//error found 9/19/2011, was System(SysNum)%TotCompPower*LocalTimeStep*SecInHour/System(SysNum)%RefMassFlowComps
		} else { // High-stage compressors (only for two-stage systems)
			HHiStageCompIn = GetSatEnthalpyRefrig( RefrigerantName, TIntercooler, 1.0, RefIndex, RoutineName );
			System_SysNum.HCompOut = HHiStageCompIn + System_SysNum.TotHiStageCompPower / System_SysNum.RefMassFlowHiStageComps;
		}

		//Calculate superheat energy available for desuperheaters
		HSatVapCondense = GetSatEnthalpyRefrig( RefrigerantName, System_SysNum.TCondense, 1.0, RefIndex, RoutineName );
		CpSatVapCondense = GetSatSpecificHeatRefrig( RefrigerantName, System_SysNum.TCondense, 1.0, RefIndex, RoutineName );
		if ( NumStages == 1 ) { // Single-stage systems
			HeatReclaimRefrigCondenser( CondID ).AvailCapacity = System_SysNum.RefMassFlowComps * ( System_SysNum.HCompOut - HSatVapCondense );
		} else { // Two-stage systems
			HeatReclaimRefrigCondenser( CondID ).AvailCapacity = System_SysNum.RefMassFlowHiStageComps * ( System_SysNum.HCompOut - HSatVapCondense );
		} // NumStages

		//No function available to get Tout as f(Pout, Hout), so use estimate based on constant cp in superheat range...
		//  Use average of Tcondense and Tout of condenser as check for whether heat reclaim is reasonable.
		TCompOutEstimate = System_SysNum.TCondense + ( System_SysNum.HCompOut - HSatVapCondense ) / CpSatVapCondense;

		HeatReclaimRefrigCondenser( CondID ).AvailTemperature = ( TsatforPdisch + TCompOutEstimate ) / 2.0;
		System_SysNum.AverageCompressorCOP = System_SysNum.TotCompCapacity / ( System_SysNum.TotCompPower + System_SysNum.TotHiStageCompPower );
		System_SysNum.TotCompElecConsump = System_SysNum.TotCompPower * LocalTimeStepSec;
		if ( NumStages == 2 ) {
			System_SysNum.TotHiStageCompElecConsump = System_SysNum.TotHiStageCompPower * LocalTimeStepSec;
			System_SysNum.TotCompElecConsumpTwoStage = System_SysNum.TotCompElecConsump + System_SysNum.TotHiStageCompElecConsump;
		}
		System_SysNum.TotCompCoolingEnergy = System_SysNum.TotCompCapacity * LocalTimeStepSec;
		System_SysNum.TotHiStageCompCoolingEnergy = System_SysNum.TotHiStageCompCapacity * LocalTimeStepSec;

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateTransCompressors( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brian A. Fricke, ORNL
		//       DATE WRITTEN   Fall 2011
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the compressor power, energy, capacity, and efficiency for a detailed transcritical CO2
		// refrigeration system.

		// METHODOLOGY EMPLOYED:
		// Use AHRI compressor performance curves for subcritical compressor operation, AHRI-style compressor
		// performance curves for transcritical compressor operation, the evaporating temperature of the
		// medium- and low-temperature loads, and the gas cooler outlet conditions (temperature, pressure
		// and enthalpy).

		// REFERENCES:
		// ANSI/AHRI. 2004. Standard 540, Standard for Performance Rating of Positive Displacement Refrigerant
		//     Comprssors and Compressor Units. Arlington, VA: Air-Conditioning, Heating, and Refrigeration
		//     Institute.
		// Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
		//     systems with supermarket model "SuperSim", Part I: Model description and validation. International
		//     Journal of Refrigeration 34: 527-539.
		// Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
		//     systems with supermarket model "SuperSim", Part II: Model applications. International Journal of
		//     Refrigeration 34: 540-549.
		// Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
		//     Part I: Modeling, simulation and optimization of two system solutions. International Journal of
		//     Refrigeration 31: 516-524.
		// Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
		//     Part II: System modifications and comparisons of different solutions. International Journal of
		//     Refrigeration 31: 525-534.

		// Using/Aliasing
		using CurveManager::CurveValue;
		//unused  USE DataEnvironment,   ONLY : OutDryBulbTemp

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// Following constants approp for R22, R134a, R404a, R507, R410a, R407c.
		// For the same pressure drop, CO2 has a corresponding temperature penalty 5 to 10 times smaller than
		// ammonia and R-134a (ASHRAE Handbook of Refrigeration, 2010, p. 3.7).  Ignore pressure drop for CO2 calculations.
		// NOTE, these DelT...Pipes reflect the decrease in Pressure in the pipes, NOT thermal transfer through the pipe walls.
		//REAL(r64), PARAMETER ::DelTSuctPipes  = 1.0d0 ! Tsat drop corresponding to P drop in suction pipes, ASHRAE 2006 p 2.4 (C)
		//REAL(r64), PARAMETER ::DelTDischPipes = 0.5d0 ! Tsat drop corresponding to P drop in discharge pipes, ASHRAE 2006 p 2.5 (C)
		Real64 const ErrorTol( 0.001 ); // Iterative solution tolerance

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "RefrigeratedCase:CalculateTransCompressors" );
		int CompIndex; // Compressor index within system
		int CompID; // Compressor index within all compressors
		int GasCoolerID; // Gas cooler index for this refrigeration system
		int Iter; // Iteration counter
		static Real64 AccumLoadMT( 0.0 ); // Load due to previously unmet medium temperature compressor loads (transcritical system)
		static Real64 AccumLoadLT( 0.0 ); // Load due to previously unmet low temperature compressor loads (transcritical system)
		Real64 CapacityCorrectionLT; // Capacity at existing subcool/superheat over cap at rated conditions for LT loads
		Real64 CapacityCorrectionMT; // Capacity at existing subcool/superheat over cap at rated conditions for MT loads
		Real64 CaseEnthalpyChangeRatedMT; // Enthalpy change in medium temperature cases at compressor rated cond, J/kg
		Real64 CaseEnthalpyChangeRatedLT; // Enthalpy change in low temperature cases at compressor rated cond, J/kg
		Real64 DensityActualLT; // Density of superheated gas at LP compressor inlet, m3/kg
		Real64 DensityActualMT; // Density of superheated gas at HP compressor inlet, m3/kg
		Real64 DensityRatedHP; // Density of high pressure compressor inlet gas at rated superheat, m3/kg
		Real64 DensityRatedLP; // Density of low pressure compressor inlet gas at rated superheat, m3/kg
		Real64 HCaseInRatedLT; // Enthalpy entering low temperature cases at rated subcooling, J/kg
		Real64 HCaseInRatedMT; // Enthalpy entering medium temperature cases at rated subcooling, J/kg
		static Real64 HCaseOutLTMT( 0.0 ); // Combined enthalpy from the outlets of the LP compressor and MT loads, J/kg
		Real64 HCompInRatedHP; // Enthalpy entering high pressure compressor at rated superheat, J/kg
		Real64 HCompInRatedLP; // Enthalpy entering low pressure compressor at rated superheat, J/kg
		Real64 HGCOutlet; // Enthalpy at gas cooler outlet, J/kg
		Real64 HIdeal; // Ideal enthalpy at subcooler (for 100% effectiveness)
		Real64 Hnew; // Calucalted enthalpy, J/kg
		static Real64 HReceiverBypass( 0.0 ); // Enthalpy at the receiver bypass, J/kg
		Real64 HsatLiqforTevapNeededMT; // Enthalpy of saturated liquid at MT evaporator, J/kg
		Real64 HsatVaporforTevapneededMT; // Enthlapy of saturated vapor at MT evaporator (transcritical cycle), J/kg
		Real64 HsatVaporforTevapneededLT; // Enthlapy of saturated vapor at LT evaporator (transcritical cycle), J/kg
		Real64 LFLastComp; // Load factor for last compressor dispatched
		static Real64 LocalTimeStep( 0.0 ); // TimeStepZone for case/walkin systems, TimeStepSys for coil systems
		Real64 MassCorrectionLT; // Mass flow at existing subcool/superheat over cap at rated conditions for LT loads
		Real64 MassCorrectionMT; // Mass flow at existing subcool/superheat over cap at rated conditions for MT loads
		Real64 NeededCapacityLT; // Sum of LT case loads and mech subcooler loads (transcritical cycle), W
		Real64 NeededCapacityMT; // Sum of MT case loads and mech subcooler loads (transcritical cycle), W
		Real64 PSuctionLT; // Suction pressure in low temperature cases, Pa
		Real64 PSuctionMT; // Suction pressure in medium temperature cases, Pa
		Real64 PGCOutlet; // Gas cooler outlet pressure, Pa
		Real64 QualityReceiver; // Refrigerant quality in the receiver
		Real64 SubcoolEffect; // Heat exchanger effectiveness of the subcooler
		Real64 TempInRatedHP; // Temperature entering high pressure compressor at rated superheat, C
		Real64 TempInRatedLP; // Temperature entering low pressure compressor at rated superheat, C
		Real64 TsatforPdisLT; // Low temperature saturated discharge temperature (transcritical cycle), C
		Real64 TsatforPdisMT; // Medium temperature saturated discharge temperature (transcritical cycle), C
		Real64 TsatforPsucLT; // Low temperature saturated suction temperature (transcritical cycle), C
		Real64 TsatforPsucMT; // Medium temperature saturated suction temperature (transcritical cycle), C
		Real64 TSubcoolerColdIn; // Suction gas temperature at the inlet of the subcooler, C
		Real64 TotalEnthalpyChangeActualLT; // Actual enthalpy change in LT cases, J/kg
		Real64 TotalEnthalpyChangeActualMT; // Actual enthalpy change in MT cases, J/kg
		Real64 TotalRefMassFlow; // Total mass flow through high pressure side of system, kg/s
		Real64 Xu; // Initial upper guess for iterative search
		Real64 Xl; // Initial lower guess for iterative search
		Real64 Xnew; // New guess for iterative search

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		GasCoolerID = TransSystem( SysNum ).GasCoolerNum( 1 );

		// Determine refrigerating capacity needed
		AccumLoadLT = 0.0;
		NeededCapacityLT = 0.0;
		if ( TransSystem( SysNum ).TransSysType == 2 ) {
			AccumLoadLT = max( 0.0, ( TransSystem( SysNum ).UnmetEnergyLT / LocalTimeStep / SecInHour ) );
			NeededCapacityLT = TransSystem( SysNum ).TotalSystemLoadLT + AccumLoadLT + TransSystem( SysNum ).PipeHeatLoadLT;
		} // (TransSystem(SysNum)%TransSysType == 2)
		AccumLoadMT = max( 0.0, ( TransSystem( SysNum ).UnmetEnergyMT / LocalTimeStep / SecInHour ) );
		NeededCapacityMT = TransSystem( SysNum ).TotalSystemLoadMT + AccumLoadMT + TransSystem( SysNum ).PipeHeatLoadMT;

		// Determine refrigerant properties at receiver
		TransSystem( SysNum ).CpSatLiqReceiver = GetSatSpecificHeatRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TReceiver, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
		HReceiverBypass = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TReceiver, 1.0, TransSystem( SysNum ).RefIndex, RoutineName );

		// Determine refrigerant properties at low temperature (LT) loads (if present)
		// Dispatch low pressure (LP) compressors as necessary
		if ( TransSystem( SysNum ).TransSysType == 2 ) { // LT side of TwoStage transcritical system
			TransSystem( SysNum ).HCaseInLT = TransSystem( SysNum ).HSatLiqReceiver;
			// TCompInLP and HCompInLP include case superheat plus effect of suction line heat gain
			TransSystem( SysNum ).TCompInLP = TransSystem( SysNum ).TEvapNeededLT + TransCaseSuperheat + TransSystem( SysNum ).PipeHeatLoadLT / ( TransSystem( SysNum ).CpSatVapEvapLT * TransSystem( SysNum ).RefMassFlowtoLTLoads );
			TransSystem( SysNum ).HCompInLP = TransSystem( SysNum ).HCaseOutLT + TransSystem( SysNum ).PipeHeatLoadLT / TransSystem( SysNum ).RefMassFlowtoLTLoads;
			TsatforPsucLT = TransSystem( SysNum ).TEvapNeededLT;
			TsatforPdisLT = TransSystem( SysNum ).TEvapNeededMT;
			HsatVaporforTevapneededLT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededLT, 1.0, TransSystem( SysNum ).RefIndex, RoutineName );
			HsatLiqforTevapNeededMT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededMT, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
			PSuctionLT = GetSatPressureRefrig( TransSystem( SysNum ).RefrigerantName, TsatforPsucLT, TransSystem( SysNum ).RefIndex, RoutineName );
			DensityActualLT = GetSupHeatDensityRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TCompInLP, PSuctionLT, TransSystem( SysNum ).RefIndex, RoutineName );
			TotalEnthalpyChangeActualLT = TransSystem( SysNum ).HCompInLP - TransSystem( SysNum ).HCaseInLT;

			//Dispatch low pressure (LP) compressors
			//Before dispatching LP compressors, zero sum of compressor outputs and zero each compressor
			TransSystem( SysNum ).TotCompCapacityLP = 0.0;
			TransSystem( SysNum ).RefMassFlowCompsLP = 0.0;
			TransSystem( SysNum ).TotCompPowerLP = 0.0;

			for ( CompIndex = 1; CompIndex <= TransSystem( SysNum ).NumCompressorsLP; ++CompIndex ) {
				CompID = TransSystem( SysNum ).CompressorNumLP( CompIndex );
				Compressor( CompID ).Power = 0.0;
				Compressor( CompID ).MassFlow = 0.0;
				Compressor( CompID ).Capacity = 0.0;
				Compressor( CompID ).ElecConsumption = 0.0;
				Compressor( CompID ).CoolingEnergy = 0.0;
				Compressor( CompID ).LoadFactor = 0.0;
			}

			for ( CompIndex = 1; CompIndex <= TransSystem( SysNum ).NumCompressorsLP; ++CompIndex ) {
				CompID = TransSystem( SysNum ).CompressorNumLP( CompIndex );
				//need to use indiv compressor's rated subcool and superheat to adjust capacity to actual conditions
				{ auto const SELECT_CASE_var( Compressor( CompID ).SubcoolRatingType );
				if ( SELECT_CASE_var == RatedSubcooling ) {
					HCaseInRatedLT = HsatLiqforTevapNeededMT - TransSystem( SysNum ).CpSatLiqReceiver * Compressor( CompID ).RatedSubcool;
				} else if ( SELECT_CASE_var == RatedLiquidTemperature ) { //have rated liquid temperature stored in "RatedSubcool"
					HCaseInRatedLT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, Compressor( CompID ).RatedSubcool, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
				}}
				{ auto const SELECT_CASE_var( Compressor( CompID ).SuperheatRatingType );
				if ( SELECT_CASE_var == RatedSuperheat ) {
					HCompInRatedLP = HsatVaporforTevapneededLT + TransSystem( SysNum ).CpSatVapEvapLT * Compressor( CompID ).RatedSuperheat;
					TempInRatedLP = TransSystem( SysNum ).TEvapNeededLT + Compressor( CompID ).RatedSuperheat;
				} else if ( SELECT_CASE_var == RatedReturnGasTemperature ) { //have rated compressor inlet temperature stored in "RatedSuperheat"
					TempInRatedLP = Compressor( CompID ).RatedSuperheat;
					HCompInRatedLP = GetSupHeatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, Compressor( CompID ).RatedSuperheat, PSuctionLT, TransSystem( SysNum ).RefIndex, RoutineName );
				}}

				CaseEnthalpyChangeRatedLT = HCompInRatedLP - HCaseInRatedLT;
				DensityRatedLP = GetSupHeatDensityRefrig( TransSystem( SysNum ).RefrigerantName, TempInRatedLP, PSuctionLT, TransSystem( SysNum ).RefIndex, RoutineName );

				//  Adjust capacity and mass flow to reflect the specific volume change due to superheating and
				//  the increase in capacity due to extra subcooling
				MassCorrectionLT = DensityActualLT / DensityRatedLP;
				CapacityCorrectionLT = MassCorrectionLT * TotalEnthalpyChangeActualLT / CaseEnthalpyChangeRatedLT;
				Compressor( CompID ).Power = CurveValue( Compressor( CompID ).ElecPowerCurvePtr, TsatforPsucLT, TsatforPdisLT );
				Compressor( CompID ).Capacity = CapacityCorrectionLT * CurveValue( Compressor( CompID ).CapacityCurvePtr, TsatforPsucLT, TsatforPdisLT );
				Compressor( CompID ).MassFlow = Compressor( CompID ).Capacity / TotalEnthalpyChangeActualLT;
				Compressor( CompID ).ElecConsumption = Compressor( CompID ).Power * LocalTimeStep * SecInHour;
				Compressor( CompID ).CoolingEnergy = Compressor( CompID ).Capacity * LocalTimeStep * SecInHour;
				Compressor( CompID ).LoadFactor = 1.0;
				if ( ( TransSystem( SysNum ).TotCompCapacityLP + Compressor( CompID ).Capacity ) >= NeededCapacityLT ) {
					LFLastComp = ( NeededCapacityLT - TransSystem( SysNum ).TotCompCapacityLP ) / Compressor( CompID ).Capacity;
					Compressor( CompID ).Power *= LFLastComp;
					Compressor( CompID ).MassFlow *= LFLastComp;
					Compressor( CompID ).Capacity *= LFLastComp;
					TransSystem( SysNum ).TotCompCapacityLP += Compressor( CompID ).Capacity;
					TransSystem( SysNum ).RefMassFlowCompsLP += Compressor( CompID ).MassFlow;
					TransSystem( SysNum ).TotCompPowerLP += Compressor( CompID ).Power;
					Compressor( CompID ).ElecConsumption = Compressor( CompID ).Power * LocalTimeStep * SecInHour;
					Compressor( CompID ).CoolingEnergy = Compressor( CompID ).Capacity * LocalTimeStep * SecInHour;
					Compressor( CompID ).LoadFactor = LFLastComp;
					break;
				} else {
					TransSystem( SysNum ).TotCompCapacityLP += Compressor( CompID ).Capacity;
					TransSystem( SysNum ).RefMassFlowCompsLP += Compressor( CompID ).MassFlow;
					TransSystem( SysNum ).TotCompPowerLP += Compressor( CompID ).Power;
				}
			} // NumCompressorsLP
			TransSystem( SysNum ).HCompOutLP = TransSystem( SysNum ).HCompInLP + TransSystem( SysNum ).TotCompPowerLP / TransSystem( SysNum ).RefMassFlowCompsLP;
		} // (TransSystem(SysNum)%TransSysType == 2)

		// Determine refrigerant properties at medium temperature (MT) loads
		// Dispatch high pressure (HP) compressors as necessary
		TsatforPsucMT = TransSystem( SysNum ).TEvapNeededMT;
		if ( GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TransOpFlag ) { // Transcritical system is operating in transcritical region
			HGCOutlet = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut;
		} else { // Transcritical system is operating in subcritical region
			TsatforPdisMT = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TGasCoolerOut;
		}
		PSuctionMT = GetSatPressureRefrig( TransSystem( SysNum ).RefrigerantName, TsatforPsucMT, TransSystem( SysNum ).RefIndex, RoutineName );
		PGCOutlet = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).PGasCoolerOut;
		HsatVaporforTevapneededMT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TEvapNeededMT, 1.0, TransSystem( SysNum ).RefIndex, RoutineName );
		TransSystem( SysNum ).HCaseInMT = TransSystem( SysNum ).HSatLiqReceiver;
		// Enthalpy of refrigerant after leaving medium temperature loads and low pressure compressors
		HCaseOutLTMT = ( TransSystem( SysNum ).RefMassFlowtoLTLoads * TransSystem( SysNum ).HCompOutLP + TransSystem( SysNum ).RefMassFlowtoMTLoads * TransSystem( SysNum ).HCaseOutMT + TransSystem( SysNum ).PipeHeatLoadMT ) / ( TransSystem( SysNum ).RefMassFlowtoLTLoads + TransSystem( SysNum ).RefMassFlowtoMTLoads );

		// Total refrigerant flow rate is total flow from LT and MT loads divided by (1-x) where x is the quality of the
		// refrigerant entering the receiver.  The receiver bypass flow rate is (x)*(Total Flow).
		// Iterate to find the quality of the refrigerant entering the receiver.
		Xu = 1.0; // upper bound on quality
		Xl = 0.0; // lower bound on quality
		if ( ( GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut + TransSystem( SysNum ).DelHSubcoolerDis ) > TransSystem( SysNum ).HSatLiqReceiver ) {
			for ( Iter = 1; Iter <= 15; ++Iter ) { // Maximum of 15 iterations to find receiver quality
				QualityReceiver = ( Xu + Xl ) / 2.0;
				Hnew = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TReceiver, QualityReceiver, TransSystem( SysNum ).RefIndex, RoutineName );

				// estimated QualityReceiver is too high
				if ( Hnew > ( GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut + TransSystem( SysNum ).DelHSubcoolerDis ) ) {
					Xu = QualityReceiver;
				} else { // estimated QualityReceiver is too low
					Xl = QualityReceiver;
				}
				if ( std::abs( ( Hnew - ( GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut + TransSystem( SysNum ).DelHSubcoolerDis ) ) / Hnew ) < ErrorTol ) break;
			}
			TotalRefMassFlow = ( TransSystem( SysNum ).RefMassFlowtoLTLoads + TransSystem( SysNum ).RefMassFlowtoMTLoads ) / ( 1.0 - QualityReceiver );
			TransSystem( SysNum ).RefMassFlowReceiverBypass = QualityReceiver * TotalRefMassFlow;
		} else {
			TransSystem( SysNum ).RefMassFlowReceiverBypass = 0.0;
			TotalRefMassFlow = ( TransSystem( SysNum ).RefMassFlowtoLTLoads + TransSystem( SysNum ).RefMassFlowtoMTLoads );
		} // %HGasCoolerOut > TransSystem(SysNum)%HSatLiqReceiver)

		TransSystem( SysNum ).HCompInHP = ( HCaseOutLTMT * ( TransSystem( SysNum ).RefMassFlowtoLTLoads + TransSystem( SysNum ).RefMassFlowtoMTLoads ) + HReceiverBypass * TransSystem( SysNum ).RefMassFlowReceiverBypass ) / ( TransSystem( SysNum ).RefMassFlowtoLTLoads + TransSystem( SysNum ).RefMassFlowtoMTLoads + TransSystem( SysNum ).RefMassFlowReceiverBypass );

		// Iterate to find the suction temperature entering subcooler
		Xl = GetSatTemperatureRefrig( TransSystem( SysNum ).RefrigerantName, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
		Xu = Xl + 50.0;
		for ( Iter = 1; Iter <= 15; ++Iter ) { // Maximum of 15 iterations
			Xnew = ( Xu + Xl ) / 2.0;
			Hnew = GetSupHeatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, Xnew, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
			if ( Hnew > TransSystem( SysNum ).HCompInHP ) { // xnew is too high
				Xu = Xnew;
			} else { // xnew is too low
				Xl = Xnew;
			}
			if ( std::abs( ( Hnew - TransSystem( SysNum ).HCompInHP ) / Hnew ) < ErrorTol ) break;
		}
		TSubcoolerColdIn = Xnew;

		// Modify receiver inlet enthlapy and HP compressor inlet enthalpy to account for subcooler
		HIdeal = GetSupHeatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TGasCoolerOut, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
		// Only use subcooler if suction gas inlet temperature less than gas cooler outlet temperature
		if ( TSubcoolerColdIn < GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TGasCoolerOut ) {
			SubcoolEffect = TransSystem( SysNum ).SCEffectiveness;
		} else {
			SubcoolEffect = 0.0;
		} // (TSubcoolerColdIn < GasCooler(SysNum)%TGasCoolerOut)
		TransSystem( SysNum ).DelHSubcoolerSuc = SubcoolEffect * ( HIdeal - TransSystem( SysNum ).HCompInHP );
		TransSystem( SysNum ).HCompInHP += TransSystem( SysNum ).DelHSubcoolerSuc;
		TransSystem( SysNum ).DelHSubcoolerDis = -TransSystem( SysNum ).DelHSubcoolerSuc;

		// Iterate to find the temperature at the inlet of the high pressure (HP) compressors
		Xl = GetSatTemperatureRefrig( TransSystem( SysNum ).RefrigerantName, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
		Xu = Xl + 50.0;
		for ( Iter = 1; Iter <= 15; ++Iter ) { // Maximum of 15 iterations
			Xnew = ( Xu + Xl ) / 2.0;
			Hnew = GetSupHeatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, Xnew, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
			if ( Hnew > TransSystem( SysNum ).HCompInHP ) { // xnew is too high
				Xu = Xnew;
			} else { // xnew is too low
				Xl = Xnew;
			}
			if ( std::abs( ( Hnew - TransSystem( SysNum ).HCompInHP ) / Hnew ) < ErrorTol ) break;
		}
		TransSystem( SysNum ).TCompInHP = Xnew;

		//  For capacity correction of HP compressors, consider subcooler, receiver, MT loads, LT loads and LP compressors
		//  to constitute the "load".  The actual and rated conditions at the exit of the gas cooler and the inlet of the
		//  HP compressors are used for capacity correction calculations.
		DensityActualMT = GetSupHeatDensityRefrig( TransSystem( SysNum ).RefrigerantName, TransSystem( SysNum ).TCompInHP, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
		TotalEnthalpyChangeActualMT = TransSystem( SysNum ).HCompInHP - GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut;

		//Dispatch HP compressors
		//Before dispatching HP compressors, zero sum of compressor outputs and zero each compressor
		TransSystem( SysNum ).TotCompCapacityHP = 0.0;
		TransSystem( SysNum ).RefMassFlowCompsHP = 0.0;
		TransSystem( SysNum ).TotCompPowerHP = 0.0;

		for ( CompIndex = 1; CompIndex <= TransSystem( SysNum ).NumCompressorsHP; ++CompIndex ) {
			CompID = TransSystem( SysNum ).CompressorNumHP( CompIndex );
			Compressor( CompID ).Power = 0.0;
			Compressor( CompID ).MassFlow = 0.0;
			Compressor( CompID ).Capacity = 0.0;
			Compressor( CompID ).ElecConsumption = 0.0;
			Compressor( CompID ).CoolingEnergy = 0.0;
			Compressor( CompID ).LoadFactor = 0.0;
		}

		// Dispatch High Pressure compressors to meet load, note they were listed in compressor list in dispatch order
		for ( CompIndex = 1; CompIndex <= TransSystem( SysNum ).NumCompressorsHP; ++CompIndex ) {
			CompID = TransSystem( SysNum ).CompressorNumHP( CompIndex );

			// Need to use indiv compressor's rated subcool and superheat to adjust capacity to actual conditions
			// Transcritical operation requires rated superheat
			// Subcritical operation requires rated subcool and rated superheat
			{ auto const SELECT_CASE_var( Compressor( CompID ).SubcoolRatingType );
			if ( SELECT_CASE_var == RatedSubcooling ) {
				if ( ! GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TransOpFlag ) { // Subcritical operation
					HCaseInRatedMT = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut - GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).CpGasCoolerOut * Compressor( CompID ).RatedSubcool;
				} else { // Transcritical operation
					HCaseInRatedMT = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut;
				} // (.NOT.GasCooler(SysNum)%TransOpFlag)
			} else if ( SELECT_CASE_var == RatedLiquidTemperature ) { //have rated liquid temperature stored in "RatedSubcool"
				if ( ! GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TransOpFlag ) { // Subcritical operation
					HCaseInRatedMT = GetSatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, Compressor( CompID ).RatedSubcool, 0.0, TransSystem( SysNum ).RefIndex, RoutineName );
				} else { // Transcritical operation
					HCaseInRatedMT = GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).HGasCoolerOut;
				} // (.NOT.GasCooler(SysNum)%TransOpFlag)
			}}
			{ auto const SELECT_CASE_var( Compressor( CompID ).SuperheatRatingType );
			if ( SELECT_CASE_var == RatedSuperheat ) {
				HCompInRatedHP = HsatVaporforTevapneededMT + TransSystem( SysNum ).CpSatVapEvapMT * Compressor( CompID ).RatedSuperheat;
				TempInRatedHP = TransSystem( SysNum ).TEvapNeededMT + Compressor( CompID ).RatedSuperheat;
			} else if ( SELECT_CASE_var == RatedReturnGasTemperature ) { //have rated compressor inlet temperature stored in "RatedSuperheat"
				TempInRatedHP = Compressor( CompID ).RatedSuperheat;
				HCompInRatedHP = GetSupHeatEnthalpyRefrig( TransSystem( SysNum ).RefrigerantName, Compressor( CompID ).RatedSuperheat, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
			}}

			CaseEnthalpyChangeRatedMT = HCompInRatedHP - HCaseInRatedMT;
			DensityRatedHP = GetSupHeatDensityRefrig( TransSystem( SysNum ).RefrigerantName, TempInRatedHP, PSuctionMT, TransSystem( SysNum ).RefIndex, RoutineName );
			//  Adjust capacity and mass flow to reflect the specific volume change due to superheating and
			//  the increase in capacity due to extra subcooling
			MassCorrectionMT = DensityActualMT / DensityRatedHP;
			CapacityCorrectionMT = MassCorrectionMT * TotalEnthalpyChangeActualMT / CaseEnthalpyChangeRatedMT;

			if ( GasCooler( TransSystem( SysNum ).GasCoolerNum( 1 ) ).TransOpFlag ) { // System is operating in transcritical region
				Compressor( CompID ).Power = CurveValue( Compressor( CompID ).TransElecPowerCurvePtr, TsatforPsucMT, PGCOutlet );
				Compressor( CompID ).Capacity = CapacityCorrectionMT * CurveValue( Compressor( CompID ).TransCapacityCurvePtr, TsatforPsucMT, HGCOutlet );
			} else { // System is operating in subcritical region
				Compressor( CompID ).Power = CurveValue( Compressor( CompID ).ElecPowerCurvePtr, TsatforPsucMT, TsatforPdisMT );
				Compressor( CompID ).Capacity = CapacityCorrectionMT * CurveValue( Compressor( CompID ).CapacityCurvePtr, TsatforPsucMT, TsatforPdisMT );
			} // (GasCooler(SysNum)%TransOpFlag)
			//  Mass flow through HP compressors is HP compressor refrigerating capacity divided by MT load, LT load and LP compressor power
			Compressor( CompID ).MassFlow = TotalRefMassFlow * Compressor( CompID ).Capacity / ( NeededCapacityMT + NeededCapacityLT + TransSystem( SysNum ).TotCompPowerLP );
			Compressor( CompID ).ElecConsumption = Compressor( CompID ).Power * LocalTimeStep * SecInHour;
			Compressor( CompID ).CoolingEnergy = Compressor( CompID ).Capacity * LocalTimeStep * SecInHour;
			Compressor( CompID ).LoadFactor = 1.0;
			// calculate load factor for last compressor addded
			// assumes either cycling or part load eff = full load eff for last compressor
			if ( ( TransSystem( SysNum ).TotCompCapacityHP + Compressor( CompID ).Capacity ) >= ( NeededCapacityMT + NeededCapacityLT + TransSystem( SysNum ).TotCompPowerLP ) ) {
				LFLastComp = ( ( NeededCapacityMT + NeededCapacityLT + TransSystem( SysNum ).TotCompPowerLP ) - TransSystem( SysNum ).TotCompCapacityHP ) / Compressor( CompID ).Capacity;
				Compressor( CompID ).Power *= LFLastComp;
				Compressor( CompID ).MassFlow *= LFLastComp;
				Compressor( CompID ).Capacity *= LFLastComp;
				TransSystem( SysNum ).TotCompCapacityHP += Compressor( CompID ).Capacity;
				TransSystem( SysNum ).RefMassFlowCompsHP += Compressor( CompID ).MassFlow;
				TransSystem( SysNum ).TotCompPowerHP += Compressor( CompID ).Power;
				Compressor( CompID ).ElecConsumption = Compressor( CompID ).Power * LocalTimeStep * SecInHour;
				Compressor( CompID ).CoolingEnergy = Compressor( CompID ).Capacity * LocalTimeStep * SecInHour;
				Compressor( CompID ).LoadFactor = LFLastComp;
				break;
			} else {
				TransSystem( SysNum ).TotCompCapacityHP += Compressor( CompID ).Capacity;
				TransSystem( SysNum ).RefMassFlowCompsHP += Compressor( CompID ).MassFlow;
				TransSystem( SysNum ).TotCompPowerHP += Compressor( CompID ).Power;
			}

		} // NumCompressorsHP

		TransSystem( SysNum ).HCompOutHP = TransSystem( SysNum ).HCompInHP + TransSystem( SysNum ).TotCompPowerHP / TransSystem( SysNum ).RefMassFlowCompsHP;
		TransSystem( SysNum ).RefMassFlowComps = TransSystem( SysNum ).RefMassFlowCompsLP + TransSystem( SysNum ).RefMassFlowCompsHP;
		TransSystem( SysNum ).TotCompCapacity = TransSystem( SysNum ).TotCompCapacityHP + TransSystem( SysNum ).TotCompCapacityLP;
		TransSystem( SysNum ).AverageCompressorCOP = ( TransSystem( SysNum ).TotCompCapacityHP - TransSystem( SysNum ).TotCompPowerLP ) / ( TransSystem( SysNum ).TotCompPowerLP + TransSystem( SysNum ).TotCompPowerHP );
		TransSystem( SysNum ).TotCompElecConsump = ( TransSystem( SysNum ).TotCompPowerLP + TransSystem( SysNum ).TotCompPowerHP ) * LocalTimeStep * SecInHour;
		TransSystem( SysNum ).TotCompCoolingEnergy = ( TransSystem( SysNum ).TotCompCapacityLP + TransSystem( SysNum ).TotCompCapacityHP ) * LocalTimeStep * SecInHour;

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateSubcoolers( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
		//       DATE WRITTEN   Spring 2008
		//       MODIFIED       Brian Fricke, ORNL, March 2012, added two-stage compression
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the subcooler energy exchange and refrigerant states for a particular detailed
		// refrigeration system. Use the refrigerant property subroutines in FluidProperties.cc

		// METHODOLOGY EMPLOYED:
		// Use refrigerant properties and heat exchanger correlations.  NOTE:  Assumes any Mech subcooler
		// immediately follows condenser outlet (after pipe loss) and comes before any LSHX

		// REFERENCES:
		// ASHRAE 1006 Section 2: Refrigeration Accessories

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "CalculateSubcoolers" );
		int SubcoolerIndex; // Counter through number of subcoolers on this system
		int SubcoolerID; // ID number for each unique subcooler
		int SysProvideID; // ID number of system providing refrigeration effect (ie compressor work) for mech sc
		Real64 CpLiquid; // specific heat at liquid Tsat, best avail in Fluid Properties
		Real64 CpVapor; // specific heat at vapor Tsat, best avail in Fluid Properties
		Real64 ControlTLiqOut; // Controlled temperature of liquid leaving Mechanical subcooler
		Real64 DelTempActual; // Actual subcooling, T liquid in - T liquid out
		Real64 DelTLiqDes; // Design Temperature Change on liquid side of LSHX
		static Real64 LocalTimeStep( 0.0 ); // TimeStepZone for case/walkin systems, TimeStepSys for coil systems
		Real64 LSHXeffectiveness; // EFfectiveness of liquid suction heat exchanger (LSHX)
		Real64 MechSCLoad; // Mechanical subcooler load
		Real64 SubcoolLoad; // energy transferred from warmer liquid to cool vapor
		Real64 SubcoolerSupHeat; // additional superheat added to vapor going to compressor from LSHX
		Real64 TVapInDes; // Design Vapor Inlet temperature for LSHX
		Real64 TLiqInDes; // Design Liquid Inlet temperature for LSHX
		Real64 TLiqInActual; // Liquid T in, after condenser, before any mechanical subcooler
		Real64 TVapInActual; // Vapor T in, after any superheat added by case control

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		//HCaseIn has to be recalculated as the starting point for the subcoolers here because
		//  of the multiple number of iterations through this subroutine and because Tcondense is evolving.
		if ( System( SysNum ).NumStages == 1 ) { // Single-stage compression system
			System( SysNum ).HSatLiqCond = GetSatEnthalpyRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
			System( SysNum ).CpSatLiqCond = GetSatSpecificHeatRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
			System( SysNum ).HCaseIn = System( SysNum ).HSatLiqCond - System( SysNum ).CpSatLiqCond * Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool;

			// Two-stage compression with flash intercooler
		} else if ( System( SysNum ).NumStages == 2 && System( SysNum ).IntercoolerType == 1 ) {
			System( SysNum ).HSatLiqCond = GetSatEnthalpyRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
			System( SysNum ).CpSatLiqCond = GetSatSpecificHeatRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
			System( SysNum ).HCaseIn = GetSatEnthalpyRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TIntercooler, 0.0, System( SysNum ).RefIndex, RoutineName );

			// Two-stage compression with shell-and-coil intercooler
		} else if ( System( SysNum ).NumStages == 2 && System( SysNum ).IntercoolerType == 2 ) {
			TLiqInActual = System( SysNum ).TCondense - Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool - System( SysNum ).IntercoolerEffectiveness * ( System( SysNum ).TCondense - Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool - System( SysNum ).TIntercooler );
			System( SysNum ).HSatLiqCond = GetSatEnthalpyRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
			System( SysNum ).CpSatLiqCond = GetSatSpecificHeatRefrig( System( SysNum ).RefrigerantName, System( SysNum ).TCondense, 0.0, System( SysNum ).RefIndex, RoutineName );
			System( SysNum ).HCaseIn = System( SysNum ).HSatLiqCond - System( SysNum ).CpSatLiqCond * ( System( SysNum ).TCondense - TLiqInActual );
		} // NumStages and IntercoolerType

		for ( SubcoolerIndex = 1; SubcoolerIndex <= System( SysNum ).NumSubcoolers; ++SubcoolerIndex ) {
			SubcoolerID = System( SysNum ).SubcoolerNum( SubcoolerIndex );
			//set up local variables for convenience
			DelTLiqDes = Subcooler( SubcoolerID ).LiqSuctDesignDelT;
			TVapInDes = Subcooler( SubcoolerID ).LiqSuctDesignTvapIn;
			TLiqInDes = Subcooler( SubcoolerID ).LiqSuctDesignTliqIn;
			ControlTLiqOut = Subcooler( SubcoolerID ).MechControlTliqOut;
			CpLiquid = System( SysNum ).CpSatLiqCond;
			CpVapor = System( SysNum ).CpSatVapEvap;
			if ( System( SysNum ).NumStages == 1 ) { // Single-stage compression system
				TLiqInActual = System( SysNum ).TCondense - Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool;

				// Two-stage compression with flash intercooler
			} else if ( System( SysNum ).NumStages == 2 && System( SysNum ).IntercoolerType == 1 ) {
				TLiqInActual = System( SysNum ).TIntercooler;

				// Two-stage compression with shell-and-coil intercooler
			} else if ( System( SysNum ).NumStages == 2 && System( SysNum ).IntercoolerType == 2 ) {
				TLiqInActual = System( SysNum ).TCondense - Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool - System( SysNum ).IntercoolerEffectiveness * ( System( SysNum ).TCondense - Condenser( System( SysNum ).CondenserNum( 1 ) ).RatedSubcool - System( SysNum ).TIntercooler );
			} // NumStages and IntercoolerType

			{ auto const SELECT_CASE_var( Subcooler( SubcoolerID ).SubcoolerType );
			//Mechanical subcoolers required to come first in order to take advantage of delT
			//  from lshx. taken care of because subcooler ID assigned in that order in input.

			if ( SELECT_CASE_var == Mechanical ) {
				MechSCLoad = System( SysNum ).RefMassFlowtoLoads * CpLiquid * ( TLiqInActual - ControlTLiqOut );
				System( SysNum ).HCaseIn -= CpLiquid * ( TLiqInActual - ControlTLiqOut );
				//refrigeration benefit to System(sysnum)
				//refrigeration load must be assigned properly according to input
				SysProvideID = Subcooler( SubcoolerID ).MechSourceSysID;
				System( SysProvideID ).MechSCLoad( SubcoolerID ) = MechSCLoad;
				Subcooler( SubcoolerID ).MechSCTransLoad = MechSCLoad;
				Subcooler( SubcoolerID ).MechSCTransEnergy = MechSCLoad * LocalTimeStep * SecInHour;
				// Reset inlet temperature for any LSHX that follows this mech subcooler
				TLiqInActual = ControlTLiqOut;
				System( SysNum ).TCompIn = System( SysNum ).TEvapNeeded + CaseSuperheat;

			} else if ( SELECT_CASE_var == LiquidSuction ) {
				LSHXeffectiveness = DelTLiqDes / ( TLiqInDes - TVapInDes );
				TVapInActual = System( SysNum ).TEvapNeeded + CaseSuperheat;
				DelTempActual = LSHXeffectiveness * ( TLiqInActual - TVapInActual );
				TLiqInActual -= DelTempActual;
				SubcoolLoad = System( SysNum ).RefMassFlowtoLoads * CpLiquid * DelTempActual;
				SubcoolerSupHeat = SubcoolLoad / CpVapor / System( SysNum ).RefMassFlowComps;
				System( SysNum ).TCompIn = TVapInActual + SubcoolerSupHeat;
				System( SysNum ).HCaseIn -= SubcoolLoad / System( SysNum ).RefMassFlowtoLoads;
				System( SysNum ).LSHXTrans = SubcoolLoad;
				System( SysNum ).LSHXTransEnergy = SubcoolLoad * LocalTimeStep * SecInHour;
			}}

			System( SysNum ).TLiqInActual = TLiqInActual;
		}

	}

	//***************************************************************************************************

	//***************************************************************************************************

	void
	GetRefrigeratedRackIndex(
		std::string const & Name,
		int & IndexPtr,
		int const SysType,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType,
		Optional_bool_const SuppressWarning
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   June 2007
		//       MODIFIED       Therese Stovall May 2008
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given refrigerated rack or refrigeration condenser
		//  -- issues error message if the rack or condenser is not found.
		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		//USE DataGlobals,    ONLY: ShowSevereError

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		CheckRefrigerationInput();

		{ auto const SELECT_CASE_var( SysType );
		if ( SELECT_CASE_var == RefrigSystemTypeRack ) {
			IndexPtr = FindItemInList( Name, RefrigRack );
			if ( IndexPtr == 0 ) {
				if ( present( SuppressWarning ) ) {
					//     No warning printed if only searching for the existence of a refrigerated rack
				} else {
					if ( present( ThisObjectType ) ) {
						ShowSevereError( ThisObjectType + ", GetRefrigeratedRackIndex: Rack not found=" + Name );
					} else {
						ShowSevereError( "GetRefrigeratedRackIndex: Rack not found=" + Name );
					}
				}
				ErrorsFound = true;
			}
		} else if ( SELECT_CASE_var == RefrigSystemTypeDetailed ) {
			IndexPtr = FindItemInList( Name, Condenser );
			if ( IndexPtr == 0 ) {
				if ( present( SuppressWarning ) ) {
					//     No warning printed if only searching for the existence of a refrigeration Condenser
				} else {
					if ( present( ThisObjectType ) ) {
						ShowSevereError( ThisObjectType + ", GetRefrigeratedRackIndex: Condenser not found=" + Name );
					} else {
						ShowSevereError( "GetRefrigeratedRackIndex: Condenser not found=" + Name );
					}
				}
				ErrorsFound = true;
			}
		}}

	}

	void
	ReportRefrigerationComponents()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   October 2004
		//       MODIFIED       Shirey, FSEC Dec 2004; Lawrie, Mar 2008 - Node names, not numbers.
		//       MODIFIED       Stovall - 2008 to 2010, new refrig variables and move orphan reporting to input.
		//       MODIFIED       Fricke, ORNL, Fall 2011, added transcritical CO2 refrigeration system variables
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To report information from the input deck for refrigerated cases and racks to the eio and err file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::NodeID;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CascadeLoadID;
		int CascadeLoadNum;
		int CaseID;
		int CaseNum;
		int ChillerSetNum;
		int CoilID;
		int CoilNum;
		int CompID;
		int CompressorNum;
		int CondID;
		int CountSecPhase;
		int CountSecBrine;
		int GasCoolerID;
		int RackNum;
		int SecondaryID;
		int SecondaryNum;
		int SubcoolerNum;
		int SubcoolerID;
		int SystemNum;
		int TransSystemNum;
		int WalkInID;
		int WalkInNum;
		int ZoneID;
		std::string ChrOut;
		std::string ChrOut2;

		// Formats
		static gio::Fmt Format_101( "(A)" );
		static gio::Fmt Format_102( "(4X,A)" );
		static gio::Fmt Format_103( "(2X,A)" );
		static gio::Fmt Format_104( "('! <Refrigeration Compressor Rack>,Compressor Rack Name,',' # Refrigerated Cases Connected,# WalkIn Coolers Connected, Heat Rejection Location, ','Condenser Type, COP')" );
		static gio::Fmt Format_105( "('!',2x,'<Refrigeration Case>,Refrigeration Case Number, Refrigeration Case Name,Zone Name,','Zone Node #,Zone Node Name,Capacity (W/m),LHR,Temperature (C),Length (m),Fan (W/m),','Installed Lighting (W/m),Anti-Sweat (W/m),Defrost (W/m)')" );
		static gio::Fmt Format_108( "('!',2x,'<Refrigeration Compressor>,Compressor Number,Compressor Name,Nominal Capacity (W)')" );
		static gio::Fmt Format_109( "('! <#Refrigeration Compressor Racks>,Number of Refrigeration Compressor Racks')" );
		static gio::Fmt Format_114( "(',',1X,F7.1)" );
		static gio::Fmt Format_117( "('! <#Detailed Refrigeration Systems>,Number of Detailed Refrigeration Systems')" );
		static gio::Fmt Format_118( "('! <Detailed Refrigeration System>,Refrigeration System Name,Refrigerant Used',', # Refrigerated Cases Connected, # WalkInCoolers Connected, #Air Chillers Connected',', # Secondary Loops Served, # Cascade Condensers Served',', # Mechanical Subcoolers Served, # Compressors Connected',', # Compression Stages, Intercooler Type, Intercooler Effectiveness',', # Subcoolers Connected, Minimum Condensing Temperature (C)')" );
		static gio::Fmt Format_119( "('!',2x,'<Refrigeration Walk In Cooler>, Walk In Number, Walk In Name,','Capacity (W),Temperature (C),Coil Fan (W), Circulating Fan (W), ','Lighting (W),Heaters (W),Defrost (W), # Zones')" );
		static gio::Fmt Format_120( "('! <#Detailed Transcritical Refrigeration Systems>,Number of Detailed Transcritical Refrigeration Systems')" );
		static gio::Fmt Format_121( "('! <Detailed Transcritical Refrigeration System>,Transcritical Refrigeration System Name,Refrigerant Used',', # Medium Temperature Refrigerated Cases Connected, # Low Temperature Refrigerated Cases Connected',', # Medium Temperature WalkInCoolers Connected, # Low Temperature WalkInCoolers Connected',', # High Pressure Compressors Connected, # Low Pressure Compressors Connected',', Minimum Condensing Temperature (C)')" );
		static gio::Fmt Format_123( "('!',2x,'<Secondary Load>, Secondary System Served Name, Secondary Number')" );
		static gio::Fmt Format_126( "('!',2x,'<Refrigeration Mechanical Subcooler>, Subcooler Number, Subcooler Name, ','Name of System Providing Cooling, Design Outlet Temperature (C)')" );
		static gio::Fmt Format_127( "('!',2x,'<Refrigeration Liquid Suction Subcooler>, Subcooler Number, Subcooler Name, ','Design Subcooling (DeltaC),','Design liquid inlet temperature (C), Design vapor inlet temperature (C)')" );
		static gio::Fmt Format_128( "('!',2x,'<Cascade Load>, System Name Connected, Condenser Number, Condenser Name')" );
		static gio::Fmt Format_129( "('!',2x,'<Refrigeration Condenser:Air-Cooled>,Condenser Number,Condenser Name,Rated Condensing Temperature (C),','Rated Capacity (W), Rated Fan Power (W)')" );
		static gio::Fmt Format_130( "('!',2x,'<Refrigeration Condenser:Water-Cooled>,Condenser Number,Condenser Name,Rated Condensing Temperature (C),','Rated Capacity (W), Rated Water Inlet Temperature (C), Rated Water Flow Rate (m3/s)')" );
		static gio::Fmt Format_131( "('!',2x,'<Refrigeration Condenser:Evaporative-Cooled>,Condenser Number,Condenser Name,','Rated Capacity (W), Rated Fan Power (W)')" );
		static gio::Fmt Format_132( "('!',2x,'<Refrigeration Condenser:Cascade>, Condenser Number, Condenser Name,',' Condensing Temperature Control Type, Rated Condensing Temperature (C),',' Capacity (W), Approach Temperature Difference (DeltaC)')" );
		static gio::Fmt Format_133( "('! <Secondary Refrigeration System: Fluid Always Liquid>, Secondary Number, Secondary Name,',' # Refrigerated Cases Connected, # WalkIn Coolers Connected,',' Fluid Name, Capacity (W),Evap Temperature in Secondary Evaporator (C),',' Approach Temperature Difference (DeltaC), Temperature Range (DeltaC), TotalPumpPower (W)')" );
		static gio::Fmt Format_134( "('!',6x,'<Walk-In Surfaces Facing Zone>, ZoneName,',' Wall/Ceiling Area (m2), UValue (W/m2-C), AreaStockDoors (m2), HeightStockDoors,',' UValueStockDoors (W/m2-C), AreaGlassDoors (m2), HeightGlassDoors (m), ',' UValueGlassDoors (W/m2-C)')" );
		static gio::Fmt Format_141( "('!',2x,'<Mechanical Subcooler Load>, Subcooler Number, Subcooler Name')" );
		static gio::Fmt Format_142( "('! <#Secondary Refrigeration Systems>,Number of Secondary Refrigeration Systems')" );
		static gio::Fmt Format_146( "('! <Secondary Refrigeration System: Liquid Overfeed>, Secondary Number, Secondary Name,',' # Refrigerated Cases Connected, # WalkIn Coolers Connected, #Air Coils Connected',' Fluid Name, Capacity (W),Evap Temperature in Secondary Evaporator (C),',' Approach Temperature Difference (DeltaC), Circulating Rate, TotalPumpPower (W)')" );
		static gio::Fmt Format_148( "('! <#ZoneHVAC/Refrigeration Air Chiller Sets>,Number of ZoneHVAC/Refrigeration Air Chiller Sets')" );
		static gio::Fmt Format_149( "('! <ZoneHVAC/Refrigeration Air Chiller Set>,Chiller Set Name,',' # Air Chillers Connected, Zone Location')" );
		static gio::Fmt Format_151( "('!',2x,'<Refrigeration Air Chiller>,Refrigeration Chiller Number, Refrigeration Chiller Name,Zone Name,','Zone Node #,Zone Node Name,Sensible Capacity (W/C),Sensible Capacity (W),Evaporating Temperature (C),DT1 (C),','Fan Power (W),Heater (W),Defrost (W), Air Flow Rate (m3/s)')" );
		static gio::Fmt Format_152( "('!',2x,'<Air Chiller Load>, Air Chiller Name, Air Chiller Number, Zone Name,')" );
		static gio::Fmt Format_160( "('!',2x,'<Refrigeration GasCooler:Air-Cooled>,Gas Cooler Number, Gas Cooler Name, Rated Outlet Pressure (Pa),','Rated Outlet Temperature (C), Rated Approach Temperature (C), Rated Capacity (W), Rated Fan Power (W)')" );

		// 111 FORMAT(',',1X,F6.3) ! compressor rack output line
		// 112 FORMAT(',',1X,F16.0)! compressor output line
		// 113 FORMAT(',',1X,F7.1,',',1X,F6.2,6(',',1X,F6.1)) !refrigerated case output line
		// 135 FORMAT (6x,6(',',1X,F16.1),',',2x,I5)
		// 136 FORMAT (6x,8(',',1X,F16.1))
		// 137 FORMAT (2x, 2(',',1X,F12.1))!condenser output
		// 138 FORMAT (2x, 3(',',1X,F12.1))!condenser output
		// 139 FORMAT (2x, 4(',',1X,F12.1))!condenser output
		// 140 FORMAT(7(',',1X,F8.1),1X,',',I5) !walkin output line
		// 143 FORMAT(',',1X,F8.1,',',1X,F8.4,2(1X,',',F8.2,',',1x,F8.2,',',1x,F8.4)) !walkin/zone output line
		// 144 FORMAT(',',1X,F7.1) !mech subcooler output line
		// 145 FORMAT(3(',',1X,F7.1)) !lshx output line
		// 147 FORMAT(',',1X,F7.1,1X,2(',',F6.2),2(',',1X,F9.3)) !secondary system output line
		//150 FORMAT('! <#Refrigeration Air Chiller>,Number of Refrigeration Air Chillers')

		//write all headers applicable to this simulation
		if ( NumRefrigeratedRacks > 0 ) {
			gio::write( OutputFileInits, Format_109 ); // Intro to refrigeration case racks
			gio::write( OutputFileInits, Format_104 ); // Refrigeration Rack header
		} //(NumRefrigeratedRacks > 0)
		if ( NumRefrigSystems > 0 ) {
			gio::write( OutputFileInits, Format_117 ); // Intro to detailed systems
			gio::write( OutputFileInits, Format_118 ); // Detailed system header
			gio::write( OutputFileInits, Format_108 ); // Compressor header (Always have compressor if have detailed system)
		} //(NumRefrigSystems > 0)
		if ( NumSimulationSecondarySystems > 0 ) {
			gio::write( OutputFileInits, Format_142 ); // Intro to Secondary systems
			CountSecPhase = 0;
			CountSecBrine = 0;
			for ( SecondaryID = 1; SecondaryID <= NumSimulationSecondarySystems; ++SecondaryID ) {
				if ( ( Secondary( SecondaryID ).FluidType == SecFluidTypeAlwaysLiquid ) && ( CountSecBrine == 0 ) ) {
					gio::write( OutputFileInits, Format_133 ); // Secondary system header for brine type systems
					++CountSecBrine;
				}
				if ( ( Secondary( SecondaryID ).FluidType == SecFluidTypePhaseChange ) && ( CountSecPhase == 0 ) ) {
					gio::write( OutputFileInits, Format_146 ); // Secondary system header for liquid overfeed/phase change systems
					++CountSecPhase;
				}
			}
			gio::write( OutputFileInits, Format_123 ); //  Secondary system load header
		} //(NumSimulationSecondarySystems > 0)
		if ( NumRefrigChillerSets > 0 ) {
			gio::write( OutputFileInits, Format_148 ); // Intro to Chiller set
			gio::write( OutputFileInits, Format_149 ); // Chiller set header
			gio::write( OutputFileInits, Format_151 ); // Intro to Air Chiller
			gio::write( OutputFileInits, Format_152 ); // Air chiller header
		} //(NumRefrigSystems > 0)
		if ( NumSimulationCases > 0 ) {
			gio::write( OutputFileInits, Format_105 ); //  Case header
		} //(NumSimulationCases > 0)
		if ( NumSimulationWalkIns > 0 ) {
			gio::write( OutputFileInits, Format_119 ); //  Walk-in header
			gio::write( OutputFileInits, Format_134 ); //  Walk-in zone-specific header
		} //(NumSimulationWalkIns > 0)
		if ( NumSimulationCondAir > 0 ) {
			gio::write( OutputFileInits, Format_129 ); //  Condenser, Air-Cooled header
		} //(NumSimulationCondAir > 0)
		if ( NumSimulationCondEvap > 0 ) {
			gio::write( OutputFileInits, Format_131 ); //  Condenser, Evaporative-Cooled header
		} //(NumSimulationCondEvap > 0)
		if ( NumSimulationCondWater > 0 ) {
			gio::write( OutputFileInits, Format_130 ); //  Condenser, Water-Cooled header
		} //(NumSimulationCondWater > 0)
		if ( NumSimulationCascadeCondensers > 0 ) {
			gio::write( OutputFileInits, Format_132 ); //  Condenser, Cascade header
			gio::write( OutputFileInits, Format_128 ); //  Cascade Load header
		} //(NumSimulationCascadeCondensers > 0)
		if ( NumSimulationMechSubcoolers > 0 ) {
			gio::write( OutputFileInits, Format_141 ); //  Mech subcooler loads served header
			gio::write( OutputFileInits, Format_126 ); //  Mechanical Subcooler header
		} //(NumSimulationMechSubcoolers > 0)
		if ( ( NumSimulationSubcoolers - NumSimulationMechSubcoolers ) > 0 ) {
			gio::write( OutputFileInits, Format_127 ); //  LSHX Subcooler header
		} //((NumSimulationSubcoolers - NumSimulationMechSubcoolers) > 0)

		if ( NumTransRefrigSystems > 0 ) {
			gio::write( OutputFileInits, Format_120 ); // Intro to detailed transcriticial refrigeration system
			gio::write( OutputFileInits, Format_121 ); // Detailed system header
			if ( NumSimulationCases > 0 ) {
				gio::write( OutputFileInits, Format_105 ); //  Case header
			} //(NumSimulationCases > 0)
			if ( NumSimulationWalkIns > 0 ) {
				gio::write( OutputFileInits, Format_119 ); //  Walk-in header
				gio::write( OutputFileInits, Format_134 ); //  Walk-in zone-specific header
			} //(NumSimulationWalkIns > 0)
			gio::write( OutputFileInits, Format_108 ); // Compressor header (Always have compressor if have detailed system)
			if ( NumSimulationGasCooler > 0 ) {
				gio::write( OutputFileInits, Format_160 ); //  Gas Cooler, Air-Cooled header
			} //(NumSimulationGasCooler > 0)
		} //(NumTransRefrigSystems > 0)

		if ( NumRefrigeratedRacks > 0 ) {
			gio::write( OutputFileInits, Format_101 ) << "#Refrigeration Compressor Racks, " + RoundSigDigits( NumRefrigeratedRacks );
			for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {
				if ( RefrigRack( RackNum ).HeatRejectionLocation == LocationOutdoors ) {
					ChrOut = "Outdoors";
				} else {
					ChrOut = "Zone";
				}
				{ auto const SELECT_CASE_var( RefrigRack( RackNum ).CondenserType );
				if ( SELECT_CASE_var == RefrigCondenserTypeAir ) {
					ChrOut2 = "Air-Cooled";
				} else if ( SELECT_CASE_var == RefrigCondenserTypeEvap ) {
					ChrOut2 = "Evap-Cooled";
				} else if ( SELECT_CASE_var == RefrigCondenserTypeWater ) {
					ChrOut2 = "Water-Cooled";
				}}
				gio::write( OutputFileInits, Format_101 ) << " Refrigeration Compressor Rack," + RefrigRack( RackNum ).Name + ',' + RoundSigDigits( RefrigRack( RackNum ).NumCases ) + ',' + RoundSigDigits( RefrigRack( RackNum ).NumWalkIns ) + ',' + ChrOut + ',' + ChrOut2 + ',' + RoundSigDigits( RefrigRack( RackNum ).RatedCOP, 3 );
				for ( CaseNum = 1; CaseNum <= RefrigRack( RackNum ).NumCases; ++CaseNum ) {
					CaseID = RefrigRack( RackNum ).CaseNum( CaseNum );
					if ( RefrigCase( CaseID ).ZoneNodeNum > 0 ) {
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration Case," + RoundSigDigits( CaseID ) + ',' + RefrigCase( CaseID ).Name + ',' + RefrigCase( CaseID ).ZoneName + ',' + RoundSigDigits( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + NodeID( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RateTotCapPerLength, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RatedLHR, 2 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Temperature, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Length, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).OperatingFanPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).LightingPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).AntiSweatPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).DefrostPower, 1 ); //Installed lighting power, may not be rated power
					}
				} //numcases

				for ( WalkInNum = 1; WalkInNum <= RefrigRack( RackNum ).NumWalkIns; ++WalkInNum ) {
					WalkInID = RefrigRack( RackNum ).WalkInNum( WalkInNum );
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Walk In Cooler,  " + RoundSigDigits( WalkInID ) + ',' + WalkIn( WalkInID ).Name + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignRatedCap, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).Temperature, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CoilFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CircFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).ElecFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignLighting, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeaterPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DefrostCapacity, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).NumZones );
					for ( ZoneID = 1; ZoneID <= WalkIn( WalkInID ).NumZones; ++ZoneID ) {
						gio::write( OutputFileInits, Format_102 ) << "  Walk-In Surfaces Facing Zone, " + WalkIn( WalkInID ).ZoneName( ZoneID ) + ',' + RoundSigDigits( WalkIn( WalkInID ).SurfaceArea( ZoneID ), 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValue( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueStockDr( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueGlassDr( ZoneID ), 4 );
					} //zones for walk ins on rack
				} // walk ins on rack

				for ( CoilNum = 1; CoilNum <= RefrigRack( RackNum ).NumCoils; ++CoilNum ) {
					CoilID = RefrigRack( RackNum ).CoilNum( CoilNum );
					gio::write( OutputFileInits, Format_103 ) << " Air Chiller Load," + WarehouseCoil( CoilID ).Name + ',' + RoundSigDigits( CoilID ) + ',' + WarehouseCoil( CoilID ).ZoneName;
				} //numairchillers
			} //numracks
		} //(NumRefrigeratedRacks > 0)

		if ( NumRefrigSystems > 0 ) {
			gio::write( OutputFileInits, Format_101 ) << "#Detailed Refrigeration Systems," + RoundSigDigits( NumRefrigSystems );
			for ( SystemNum = 1; SystemNum <= NumRefrigSystems; ++SystemNum ) {
				gio::write( OutputFileInits, Format_101 ) << " Detailed Refrigeration System," + System( SystemNum ).Name + ',' + System( SystemNum ).RefrigerantName + ',' + RoundSigDigits( System( SystemNum ).NumCases ) + ',' + RoundSigDigits( System( SystemNum ).NumWalkIns ) + ',' + RoundSigDigits( System( SystemNum ).NumCoils ) + ',' + RoundSigDigits( System( SystemNum ).NumSecondarys ) + ',' + RoundSigDigits( System( SystemNum ).NumCascadeLoads ) + ',' + RoundSigDigits( System( SystemNum ).NumMechSCServed ) + ',' + RoundSigDigits( System( SystemNum ).NumCompressors + System( SystemNum ).NumHiStageCompressors ) + ',' + RoundSigDigits( System( SystemNum ).NumStages ) + ',' + RoundSigDigits( System( SystemNum ).IntercoolerType ) + ',' + RoundSigDigits( System( SystemNum ).IntercoolerEffectiveness, 2 ) + ',' + RoundSigDigits( System( SystemNum ).NumSubcoolers ) + ',' + RoundSigDigits( System( SystemNum ).TCondenseMin, 1 );

				for ( CaseNum = 1; CaseNum <= System( SystemNum ).NumCases; ++CaseNum ) {
					CaseID = System( SystemNum ).CaseNum( CaseNum );
					if ( RefrigCase( CaseID ).ZoneNodeNum > 0 ) {
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration Case," + RoundSigDigits( CaseID ) + ',' + RefrigCase( CaseID ).Name + ',' + RefrigCase( CaseID ).ZoneName + ',' + RoundSigDigits( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + NodeID( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RateTotCapPerLength, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RatedLHR, 2 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Temperature, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Length, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).OperatingFanPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).LightingPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).AntiSweatPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).DefrostPower, 1 );
					}
				} //NumCases on system
				for ( WalkInNum = 1; WalkInNum <= System( SystemNum ).NumWalkIns; ++WalkInNum ) {
					WalkInID = System( SystemNum ).WalkInNum( WalkInNum );
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Walk In Cooler," + RoundSigDigits( WalkInID ) + ',' + WalkIn( WalkInID ).Name + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignRatedCap, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).Temperature, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CoilFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CircFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignLighting, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeaterPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DefrostCapacity, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).NumZones );
					for ( ZoneID = 1; ZoneID <= WalkIn( WalkInID ).NumZones; ++ZoneID ) {
						gio::write( OutputFileInits, Format_102 ) << "  Walk-In Surfaces Facing Zone, " + WalkIn( WalkInID ).ZoneName( ZoneID ) + ',' + RoundSigDigits( WalkIn( WalkInID ).SurfaceArea( ZoneID ), 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValue( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueStockDr( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueGlassDr( ZoneID ), 4 );
					} //Num zones for each walk in on system
				} //NumWalkIns on system

				for ( CoilNum = 1; CoilNum <= System( SystemNum ).NumCoils; ++CoilNum ) {
					CoilID = System( SystemNum ).CoilNum( CoilNum );
					gio::write( OutputFileInits, Format_103 ) << " Air Chiller Load," + WarehouseCoil( CoilID ).Name + ',' + RoundSigDigits( CoilID ) + ',' + WarehouseCoil( CoilID ).ZoneName;
				} //numairchillers

				for ( CascadeLoadNum = 1; CascadeLoadNum <= System( SystemNum ).NumCascadeLoads; ++CascadeLoadNum ) {
					CascadeLoadID = System( SystemNum ).CascadeLoadNum( CascadeLoadNum );
					gio::write( OutputFileInits, Format_103 ) << " Cascade Load," + System( Condenser( CascadeLoadID ).CascadeSysID ).Name + ',' + RoundSigDigits( CascadeLoadID ) + ',' + Condenser( CascadeLoadID ).Name;
				} //cascade load on detailed system

				for ( SecondaryNum = 1; SecondaryNum <= System( SystemNum ).NumSecondarys; ++SecondaryNum ) {
					SecondaryID = System( SystemNum ).SecondaryNum( SecondaryNum );
					gio::write( OutputFileInits, Format_103 ) << " Secondary Load," + Secondary( SecondaryID ).Name + ',' + RoundSigDigits( SecondaryID );
				} //secondary load on detailed system

				for ( SubcoolerNum = 1; SubcoolerNum <= NumSimulationSubcoolers; ++SubcoolerNum ) {
					if ( Subcooler( SubcoolerNum ).MechSourceSysID != SystemNum ) continue;
					gio::write( OutputFileInits, Format_103 ) << " Mechanical Subcooler Load, " + RoundSigDigits( SubcoolerNum ) + ',' + Subcooler( SubcoolerNum ).Name;
				} //Num sim subcoolers, looking only for NumSMech Subcoolers served by this system

				if ( System( SystemNum ).NumStages == 1 ) { // Single-stage compression system
					for ( CompressorNum = 1; CompressorNum <= System( SystemNum ).NumCompressors; ++CompressorNum ) {
						CompID = System( SystemNum ).CompressorNum( CompressorNum );
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration Compressor," + RoundSigDigits( CompID ) + ',' + Compressor( CompID ).Name + ',' + RoundSigDigits( Compressor( CompID ).NomCap, 0 );
					} //NumCompressors
				} else if ( System( SystemNum ).NumStages == 2 ) { // Two-stage compression system
					// Low-stage compressors
					for ( CompressorNum = 1; CompressorNum <= System( SystemNum ).NumCompressors; ++CompressorNum ) {
						CompID = System( SystemNum ).CompressorNum( CompressorNum );
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration Low-Stage Compressor," + RoundSigDigits( CompID ) + ',' + Compressor( CompID ).Name + ',' + RoundSigDigits( Compressor( CompID ).NomCap, 0 );
					} //NumCompressors
					// High-stage compressors
					for ( CompressorNum = 1; CompressorNum <= System( SystemNum ).NumHiStageCompressors; ++CompressorNum ) {
						CompID = System( SystemNum ).HiStageCompressorNum( CompressorNum );
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration High-Stage Compressor," + RoundSigDigits( CompID ) + ',' + Compressor( CompID ).Name + ',' + RoundSigDigits( Compressor( CompID ).NomCap, 0 );
					} //NumHiStageCompressors
				} //NumStages

				CondID = System( SystemNum ).CondenserNum( 1 );
				{ auto const SELECT_CASE_var( Condenser( CondID ).CondenserType );
				if ( SELECT_CASE_var == RefrigCondenserTypeAir ) {
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Condenser:Air-Cooled," + RoundSigDigits( CondID ) + ',' + Condenser( CondID ).Name + ',' + RoundSigDigits( Condenser( CondID ).RatedTCondense, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).RatedCapacity, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).RatedFanPower, 1 );
				} else if ( SELECT_CASE_var == RefrigCondenserTypeEvap ) {
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Condenser:Evaporative-Cooled," + RoundSigDigits( CondID ) + ',' + Condenser( CondID ).Name + ',' + RoundSigDigits( Condenser( CondID ).RatedCapacity, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).RatedFanPower, 1 );
				} else if ( SELECT_CASE_var == RefrigCondenserTypeWater ) {
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Condenser:Water-Cooled," + RoundSigDigits( CondID ) + ',' + Condenser( CondID ).Name + ',' + RoundSigDigits( Condenser( CondID ).RatedTCondense, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).RatedCapacity, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).InletTemp, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).DesVolFlowRate, 1 );
				} else if ( SELECT_CASE_var == RefrigCondenserTypeCascade ) {

					{ auto const SELECT_CASE_var1( Condenser( CondID ).CascadeTempControl );
					if ( SELECT_CASE_var1 == CascadeTempSet ) {
						ChrOut = "Fixed";
					} else if ( SELECT_CASE_var1 == CascadeTempFloat ) {
						ChrOut = "Floating";
					}} // cascade temperature control
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Condenser:Cascade," + RoundSigDigits( CondID ) + ',' + Condenser( CondID ).Name + ',' + ChrOut + ',' + RoundSigDigits( Condenser( CondID ).RatedTCondense, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).RatedCapacity, 1 ) + ',' + RoundSigDigits( Condenser( CondID ).RatedApproachT, 1 );
				}} //condenser type

				for ( SubcoolerNum = 1; SubcoolerNum <= System( SystemNum ).NumSubcoolers; ++SubcoolerNum ) {
					SubcoolerID = System( SystemNum ).SubcoolerNum( SubcoolerNum );
					{ auto const SELECT_CASE_var( Subcooler( SubcoolerID ).SubcoolerType );
					if ( SELECT_CASE_var == LiquidSuction ) {
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration Liquid Suction Subcooler," + RoundSigDigits( SubcoolerID ) + ',' + Subcooler( SubcoolerID ).Name + ',' + RoundSigDigits( Subcooler( SubcoolerID ).LiqSuctDesignDelT, 1 ) + ',' + RoundSigDigits( Subcooler( SubcoolerID ).LiqSuctDesignTliqIn, 1 ) + ',' + RoundSigDigits( Subcooler( SubcoolerID ).LiqSuctDesignTvapIn, 1 );
					} else if ( SELECT_CASE_var == Mechanical ) {
						gio::write( OutputFileInits, Format_103 ) << " Refrigeration Mechanical Subcooler," + RoundSigDigits( SubcoolerID ) + ',' + Subcooler( SubcoolerID ).Name + ',' + Subcooler( SubcoolerID ).MechSourceSys + ',' + RoundSigDigits( Subcooler( SubcoolerID ).MechControlTliqOut, 1 );
					}}
				} //NumSubcoolers

			} //NumRefrigSystems
		} //(NumRefrigSystems > 0)

		if ( NumTransRefrigSystems > 0 ) {
			gio::write( OutputFileInits, Format_101 ) << "#Detailed Transcritical Refrigeration Systems," + RoundSigDigits( NumTransRefrigSystems );
			for ( TransSystemNum = 1; TransSystemNum <= NumTransRefrigSystems; ++TransSystemNum ) {
				gio::write( OutputFileInits, Format_101 ) << " Detailed Transcritical Refrigeration System," + TransSystem( TransSystemNum ).Name + ',' + TransSystem( TransSystemNum ).RefrigerantName + ',' + RoundSigDigits( TransSystem( TransSystemNum ).NumCasesMT ) + ',' + RoundSigDigits( TransSystem( TransSystemNum ).NumCasesLT ) + ',' + RoundSigDigits( TransSystem( TransSystemNum ).NumWalkInsMT ) + ',' + RoundSigDigits( TransSystem( TransSystemNum ).NumWalkInsLT ) + ',' + RoundSigDigits( TransSystem( TransSystemNum ).NumCompressorsHP ) + ',' + RoundSigDigits( TransSystem( TransSystemNum ).NumCompressorsLP ) + ',' + RoundSigDigits( GasCooler( TransSystem( TransSystemNum ).GasCoolerNum( 1 ) ).MinCondTemp, 1 );

				for ( CaseNum = 1; CaseNum <= TransSystem( TransSystemNum ).NumCasesMT; ++CaseNum ) {
					CaseID = TransSystem( TransSystemNum ).CaseNumMT( CaseNum );
					if ( RefrigCase( CaseID ).ZoneNodeNum > 0 ) {
						gio::write( OutputFileInits, Format_103 ) << " Medium Temperature Refrigeration Case," + RoundSigDigits( CaseID ) + ',' + RefrigCase( CaseID ).Name + ',' + RefrigCase( CaseID ).ZoneName + ',' + RoundSigDigits( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + NodeID( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RateTotCapPerLength, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RatedLHR, 2 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Temperature, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Length, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).OperatingFanPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).LightingPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).AntiSweatPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).DefrostPower, 1 );
					}
				} //NumCasesMT on system
				for ( CaseNum = 1; CaseNum <= TransSystem( TransSystemNum ).NumCasesLT; ++CaseNum ) {
					CaseID = TransSystem( TransSystemNum ).CaseNumLT( CaseNum );
					if ( RefrigCase( CaseID ).ZoneNodeNum > 0 ) {
						gio::write( OutputFileInits, Format_103 ) << " Low Temperature Refrigeration Case," + RoundSigDigits( CaseID ) + ',' + RefrigCase( CaseID ).Name + ',' + RefrigCase( CaseID ).ZoneName + ',' + RoundSigDigits( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + NodeID( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RateTotCapPerLength, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RatedLHR, 2 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Temperature, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Length, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).OperatingFanPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).LightingPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).AntiSweatPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).DefrostPower, 1 );
					}
				} //NumCasesLT on system
				for ( WalkInNum = 1; WalkInNum <= TransSystem( TransSystemNum ).NumWalkInsMT; ++WalkInNum ) {
					WalkInID = TransSystem( TransSystemNum ).WalkInNumMT( WalkInNum );
					gio::write( OutputFileInits, Format_103 ) << " Medium Temperature Refrigeration Walk In Cooler," + RoundSigDigits( WalkInID ) + ',' + WalkIn( WalkInID ).Name + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignRatedCap, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).Temperature, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CoilFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CircFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignLighting, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeaterPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DefrostCapacity, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).NumZones );
					for ( ZoneID = 1; ZoneID <= WalkIn( WalkInID ).NumZones; ++ZoneID ) {
						gio::write( OutputFileInits, Format_102 ) << "   Walk-In Surfaces Facing Zone," + WalkIn( WalkInID ).ZoneName( ZoneID ) + ',' + RoundSigDigits( WalkIn( WalkInID ).SurfaceArea( ZoneID ), 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValue( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueStockDr( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueGlassDr( ZoneID ), 4 );
					} //Num zones for each walk in on system
				} //NumWalkInsMT on system
				for ( WalkInNum = 1; WalkInNum <= TransSystem( TransSystemNum ).NumWalkInsLT; ++WalkInNum ) {
					WalkInID = TransSystem( TransSystemNum ).WalkInNumLT( WalkInNum );
					gio::write( OutputFileInits, Format_103 ) << " Low Temperature Refrigeration Walk In Cooler," + RoundSigDigits( WalkInID ) + ',' + WalkIn( WalkInID ).Name + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignRatedCap, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).Temperature, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CoilFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CircFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignLighting, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeaterPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DefrostCapacity, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).NumZones );
					for ( ZoneID = 1; ZoneID <= WalkIn( WalkInID ).NumZones; ++ZoneID ) {
						gio::write( OutputFileInits, Format_102 ) << "   Walk-In Surfaces Facing Zone," + WalkIn( WalkInID ).ZoneName( ZoneID ) + ',' + RoundSigDigits( WalkIn( WalkInID ).SurfaceArea( ZoneID ), 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValue( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueStockDr( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueGlassDr( ZoneID ), 4 );
					} //Num zones for each walk in on system
				} //NumWalkInsLT on system

				for ( CompressorNum = 1; CompressorNum <= TransSystem( TransSystemNum ).NumCompressorsHP; ++CompressorNum ) {
					CompID = TransSystem( TransSystemNum ).CompressorNumHP( CompressorNum );
					gio::write( OutputFileInits, Format_103 ) << " High Pressure Refrigeration Compressor," + RoundSigDigits( CompID ) + ',' + Compressor( CompID ).Name + ',' + RoundSigDigits( Compressor( CompID ).NomCap, 0 );
				} //NumCompressorsHP
				for ( CompressorNum = 1; CompressorNum <= TransSystem( TransSystemNum ).NumCompressorsLP; ++CompressorNum ) {
					CompID = TransSystem( TransSystemNum ).CompressorNumLP( CompressorNum );
					gio::write( OutputFileInits, Format_103 ) << " Low Pressure Refrigeration Compressor," + RoundSigDigits( CompID ) + ',' + Compressor( CompID ).Name + ',' + RoundSigDigits( Compressor( CompID ).NomCap, 0 );
				} //NumCompressorsLP

				if ( TransSystem( TransSystemNum ).NumGasCoolers >= 1 ) {
					GasCoolerID = TransSystem( TransSystemNum ).GasCoolerNum( 1 );
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration GasCooler:Air-Cooled," + RoundSigDigits( GasCoolerID ) + ',' + GasCooler( GasCoolerID ).Name + ',' + RoundSigDigits( GasCooler( GasCoolerID ).RatedOutletP, 1 ) + ',' + RoundSigDigits( GasCooler( GasCoolerID ).RatedOutletT, 1 ) + ',' + RoundSigDigits( GasCooler( GasCoolerID ).RatedApproachT, 1 ) + ',' + RoundSigDigits( GasCooler( GasCoolerID ).RatedCapacity, 1 ) + ',' + RoundSigDigits( GasCooler( GasCoolerID ).RatedFanPower, 1 );
				} // System(SystemNum)%NumGasCoolers >= 1

			} //NumTransRefrigSystems
		} //(NumTransRefrigSystems > 0)

		if ( NumSimulationSecondarySystems > 0 ) {
			gio::write( OutputFileInits, Format_101 ) << "#Secondary Refrigeration Systems," + RoundSigDigits( NumSimulationSecondarySystems );
			for ( SecondaryID = 1; SecondaryID <= NumSimulationSecondarySystems; ++SecondaryID ) {
				{ auto const SELECT_CASE_var( Secondary( SecondaryID ).FluidType );
				if ( SELECT_CASE_var == SecFluidTypeAlwaysLiquid ) {
					gio::write( OutputFileInits, Format_101 ) << "Secondary Refrigeration System: Fluid Always Liquid," + RoundSigDigits( SecondaryID ) + ',' + Secondary( SecondaryID ).Name + ',' + RoundSigDigits( Secondary( SecondaryID ).NumCases ) + ',' + RoundSigDigits( Secondary( SecondaryID ).NumWalkIns ) + ',' + Secondary( SecondaryID ).FluidName + ',' + RoundSigDigits( Secondary( SecondaryID ).CoolingLoadRated, 1 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).TEvapDesign, 2 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).TApproachDifRated, 2 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).TRangeDifRated, 3 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).PumpTotRatedPower, 3 );
				} else if ( SELECT_CASE_var == SecFluidTypePhaseChange ) {
					gio::write( OutputFileInits, Format_101 ) << "Secondary Refrigeration System: Liquid Overfeed," + RoundSigDigits( SecondaryID ) + ',' + Secondary( SecondaryID ).Name + ',' + RoundSigDigits( Secondary( SecondaryID ).NumCases ) + ',' + RoundSigDigits( Secondary( SecondaryID ).NumWalkIns ) + ',' + Secondary( SecondaryID ).FluidName + ',' + RoundSigDigits( Secondary( SecondaryID ).CoolingLoadRated, 1 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).TEvapDesign, 2 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).TApproachDifRated, 2 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).CircRate, 3 ) + ',' + RoundSigDigits( Secondary( SecondaryID ).PumpTotRatedPower, 3 );
				}}
				for ( CaseNum = 1; CaseNum <= Secondary( SecondaryID ).NumCases; ++CaseNum ) {
					CaseID = Secondary( SecondaryID ).CaseNum( CaseNum );
					if ( RefrigCase( CaseID ).ZoneNodeNum > 0 ) {
						gio::write( OutputFileInits, Format_103 ) << "Refrigeration Case," + RoundSigDigits( CaseID ) + ',' + RefrigCase( CaseID ).Name + ',' + RefrigCase( CaseID ).ZoneName + ',' + RoundSigDigits( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + NodeID( RefrigCase( CaseID ).ZoneNodeNum ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RateTotCapPerLength, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).RatedLHR, 2 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Temperature, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).Length, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).OperatingFanPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).LightingPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).AntiSweatPower, 1 ) + ',' + RoundSigDigits( RefrigCase( CaseID ).DefrostPower, 1 );
					}
				} //NumCases on secondary on secondary system

				for ( WalkInNum = 1; WalkInNum <= Secondary( SecondaryID ).NumWalkIns; ++WalkInNum ) {
					WalkInID = Secondary( SecondaryID ).WalkInNum( WalkInNum );
					gio::write( OutputFileInits, Format_103 ) << "Walk In," + RoundSigDigits( WalkInID ) + ',' + WalkIn( WalkInID ).Name + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignRatedCap, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).Temperature, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CoilFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).CircFanPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DesignLighting, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeaterPower, 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).DefrostCapacity, 1 );
					for ( ZoneID = 1; ZoneID <= WalkIn( WalkInID ).NumZones; ++ZoneID ) {
						gio::write( OutputFileInits, Format_102 ) << "Walk In Surfaces Facing Zone," + WalkIn( WalkInID ).ZoneName( ZoneID ) + ',' + RoundSigDigits( WalkIn( WalkInID ).SurfaceArea( ZoneID ), 1 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValue( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightStockDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueStockDr( ZoneID ), 4 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).AreaGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).HeightGlassDr( ZoneID ), 2 ) + ',' + RoundSigDigits( WalkIn( WalkInID ).UValueGlassDr( ZoneID ), 4 );
					} //zones for walk ins on secondary
				} // walk ins on secondary

				for ( CoilNum = 1; CoilNum <= Secondary( SecondaryID ).NumCoils; ++CoilNum ) {
					CoilID = Secondary( SecondaryID ).CoilNum( CoilNum );
					gio::write( OutputFileInits, Format_103 ) << " Air Chiller Load," + WarehouseCoil( CoilID ).Name + ',' + RoundSigDigits( CoilID ) + ',' + WarehouseCoil( CoilID ).ZoneName;
				} //numairchillers
			} //secondary
		} //numsimulationsecondarys

		if ( NumRefrigChillerSets > 0 ) {
			gio::write( OutputFileInits, Format_101 ) << "#ZoneHVAC/Refrigeration Air Chiller Sets," + RoundSigDigits( NumRefrigChillerSets );
			for ( ChillerSetNum = 1; ChillerSetNum <= NumRefrigChillerSets; ++ChillerSetNum ) {
				gio::write( OutputFileInits, Format_101 ) << "ZoneHVAC/Refrigeration Air Chiller Set," + AirChillerSet( ChillerSetNum ).Name + ',' + RoundSigDigits( ChillerSetNum ) + ',' + RoundSigDigits( AirChillerSet( ChillerSetNum ).NumCoils ) + ',' + AirChillerSet( ChillerSetNum ).ZoneName;

				for ( CoilNum = 1; CoilNum <= AirChillerSet( ChillerSetNum ).NumCoils; ++CoilNum ) {
					CoilID = AirChillerSet( ChillerSetNum ).CoilNum( CoilNum );
					gio::write( OutputFileInits, Format_103 ) << " Refrigeration Air Chiller," + RoundSigDigits( CoilID ) + ',' + WarehouseCoil( CoilID ).Name + ',' + WarehouseCoil( CoilID ).ZoneName + ',' + RoundSigDigits( WarehouseCoil( CoilID ).ZoneNodeNum ) + ',' + NodeID( WarehouseCoil( CoilID ).ZoneNodeNum ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).UnitLoadFactorSens, 1 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).RatedSensibleCap, 2 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).TEvapDesign, 1 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).RatedTemperatureDif, 1 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).RatedFanPower, 1 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).HeaterPower, 1 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).DefrostCapacity, 1 ) + ',' + RoundSigDigits( WarehouseCoil( CoilID ).RatedAirVolumeFlow, 1 );
				} //numairchillers
			} //numrefrigchillersets
		} //numrefrigchillersets

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateWalkIn( int const WalkInID ) // Absolute pointer to  Walk In
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL, May 2009
		//       DATE WRITTEN   Oct/Nov 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To model Walk In Coolers.

		// METHODOLOGY EMPLOYED:
		// Walk-in cooler performance is based on the ASHRAE load model, which includes
		// infiltration through door openings and sensible loss through walls/ceilings identified
		// by the user as sum of UA for each zone. A sub-floor heater is assumed to be sized so that
		// the temperature of the slab beneath the floor insulation is the same as the ground
		// temperature (to avoid ground freezing and heaving).
		// All equipment loads (fan, light, heaters) are modeled as well.  Sensible and latent
		// exchange with multiple adjoining zones is included. A master schedule is used for the Walk In operation and
		// additional schedules control the lights, defrost, and heater operation.

		// The fan is assumed to be off for Hot-Gas, Hot-Brine, and Electric defrost. The user can choose
		// to include the load due to bringing the coil mass up from the evaporating temperature to the melting temperature
		//  if they choose.  Otherwise this factor is set to zero.

		// Unmet loads are accumulated to be met the following time step.  This usually occurs during defrost and
		// restocking.

		// REFERENCES:
		// ASHRAE 2006 Handbook, chapters 13 and 14.
		// Gosney, W.B., Olama, G.A.-L., Heat and Enthalpy Gains through Cold Room Doorways,
		//     Proceedings of the Institute of Refrigeration, vol. 72, pp 31-41, 1975

		// Using/Aliasing
		using CurveManager::CurveValue;
		using namespace DataLoopNode;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTsatFnHPb;
		using Psychrometrics::PsyWFnTdpPb;
		using Psychrometrics::PsyHFnTdbRhPb;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyTdpFnWPb;
		using Psychrometrics::PsyWFnTdbH;
		//  USE DataEnvironment, ONLY:   OutBaroPress, OutDryBulbTemp
		using DataEnvironment::OutBaroPress;
		using General::CreateSysTimeIntervalString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DefaultWalkInDoorOpenFactor( 0.05 ); // walk in door open factor (fraction time open)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "CalculateWalkIn" );
		static int ZoneNodeNum( 0 ); // Zone node number
		static int ZoneNum( 0 ); // Index to zone
		static int ZoneID( 0 ); // Index to zone
		static Real64 CapApplied( 0.0 ); // Walk In total capacity at specific operating conditions
		static Real64 CircFanSchedule( 0.0 );
		static Real64 Conv( 0.0 ); // conversion factor in gravity equation
		static Real64 DefrostCap( 0.0 ); // Design defrost capacity of WalkIn (W)
		static Real64 DefrostEnergy( 0.0 ); // (J)
		static Real64 DefEnergyFraction( 0.0 ); // dimensionless
		static Real64 AvailDefrostEnergy( 0.0 ); // available to melt ice with temp term control (J)
		static Real64 DefrostLoad( 0.0 ); // (W)
		static Real64 DefrostSchedule( 0.0 ); // WalkIn defrost schedule, between 0 and 1
		static Real64 DefrostDripDownSchedule( 0.0 ); // WalkIn drip-down schedule (allows coil to drain after defrost)
		static Real64 DefrostEnergyNeeded( 0.0 ); // Energy needed to melt all ice, used with temperature termination (J)
		static Real64 DensityAirWalkIn( 0.0 ); // Density at Walk in temperature and 90% assumed RH
		static Real64 DensityZoneAir( 0.0 ); // Density of the air in a particular zone (kg/m3)
		static Real64 DensityFactorFm( 0.0 ); // called Fm in ASHRAE 2010 eq 13 page 24.5 for door infiltration
		static Real64 DensitySqRtFactor( 0.0 ); // from ASHRAE 2010 eq 12 page 24.4 for door infiltration
		static Real64 DelTemp( 0.0 ); // Difference between zone and walk in temperatures (C)
		static Real64 DesignLighting( 0.0 ); // Total design display WalkIn lighting power (W)
		static Real64 DesignRatedCap( 0.0 ); // Rated capacity of walk in cooler coil (W)
		static Real64 DoorFlowFactor( 0.0 ); // Derate compared to fully developed flow through 100% open door
		static Real64 DoorOpenFactor( 0.0 ); // Derate based upon fraction time door opened
		static Real64 DoorProtectEff( 0.0 ); // Door protection effectiveness
		static Real64 DrHeight( 0.0 ); // Door height (m)
		static Real64 DrArea( 0.0 ); // Door area (m2)
		static Real64 EnthalpyAirWalkIn( 0.0 ); // Enthalpy of air corresponding to walk in temperatuer and 90% assumed RH (J/kg)
		static Real64 EnthalpyZoneAir( 0.0 ); // Enthalpy of the air in a particular zone (J/kg)
		static Real64 FanLoad( 0.0 ); // Total fan energy rate (W)
		static Real64 FloorLoad( 0.0 ); // Total floor energy rate (W)
		static Real64 FrostChangekg( 0.0 ); // Amount of frost added or melted  (kg)
		static Real64 FullFlowInfLoad( 0.0 ); // Total load (lat + sens) due to 100% open doors w/ fully developed flow (W)
		static Real64 GlassDoorInfLoad( 0.0 ); // infiltration through glass reach-in doors in a particular zone (W)
		static Real64 GlassDoorSensHeat( 0.0 ); // sensible heat gain through glass reach-in doors (UA*delta T) (W)
		static Real64 GlassDoorArea( 0.0 ); // facing a particular zone (m2)
		static Real64 Gravity( 0.0 );
		static Real64 HeaterSchedule( 0.0 ); // zero to one
		static Real64 HeaterLoad( 0.0 ); // Total heater (except defrost) energy rate (W)
		static Real64 HumRatioAirWalkIn( 0.0 ); // corresponds to walk in temp and 90% assumed RH(kg water/kg dry air)
		static Real64 HumRatioZoneAir( 0.0 ); // Humidity Ratio of the air in a particular zone (kg water/kg dry air)
		static Real64 IceSensHeatNeeded( 0.0 ); // Energy to raise frost temperature to 0C, used w/ temp termination (J)
		static Real64 LatentCapApplied( 0.0 ); // Walk In latent capacity at specific operating conditions
		static Real64 LatentLoadTotal( 0.0 ); // total latent load on WalkIn over all zones (W)
		static Real64 LightLoad( 0.0 ); // Total lighting energy rate (W)
		static Real64 LightingSchedule( 0.0 ); // WalkIn lighting schedule
		static Real64 LoadRequested( 0.0 ); // Load necessary to meet current and all stored energy needs (W)
		static Real64 LoadTotal( 0.0 ); // total load in time step (W)
		static Real64 MassDryAirRate( 0.0 ); // Mass dry air infiltrating into/out-of walkin through doors (kg/s)
		static Real64 MaxCap( 0.0 ); // Design chilling capacity reduced according to drip-down schedule (W)
		static Real64 SensibleCapApplied( 0.0 ); // Walk In sensible capacity at specific operating conditions
		static Real64 SensibleLoadTotal( 0.0 ); // Total sensible load on WalkIn over all zones (W)
		static Real64 StoredEnergyRate( 0.0 ); // Rate needed to serve all stored energy during single time step (W)
		static Real64 StartIceTemp( 0.0 ); // Frost temperature at start of time step [C]
		static Real64 StartFrostKg( 0.0 ); // frost load at start of time step (kg of ice)
		static Real64 StockDoorInfLoad( 0.0 ); // infiltration through stock doors in a particular zone (W)
		static Real64 StockDoorSensHeat( 0.0 ); // sensible heat gain through stock doors (UA*delta T) (W)
		static Real64 StockDoorArea( 0.0 ); // (m2)
		static Real64 StockingLoad( 0.0 ); // Total load due to stocking WalkIn product (W)
		static Real64 TWalkIn( 0.0 ); // WalkIn operating temperature (C)
		static Real64 UAOtherSurfaces( 0.0 ); // UA for non-door surfaces facing a certain zone (W/C)
		static Real64 WalkInSchedule( 0.0 ); // Current value of WalkIn operating (availability) schedule
		static Real64 WalkInSensLoad( 0.0 ); // Walk in cooler sensible load facing particular zone (W)
		static Real64 WalkInLatLoad( 0.0 ); // Walk in cooler latent load facing particular zone (W)
		static Real64 WaterRemovRate( 0.0 ); // Walk in cooler removes water at this rate in this zone (kg/s)
		static Real64 ZoneDryBulb( 0.0 ); // Dry Bulb Temperature of adjacent zone
		static Real64 ZoneSensLoad( 0.0 ); // Sensible WalkIn credit delivered to a particular zone (W)
		static Real64 ZoneLatentLoad( 0.0 ); // Latent WalkIn credit delivered to zone (W)
		static Real64 ZoneRHFrac( 0.0 ); // Zone relative humidity fraction (decimal)
		static Real64 ZoneInfilLoad( 0.0 ); // Walk in cooler infiltration load (sens + latent) in certain zone (W)
		static Real64 ZInfilSensLoad( 0.0 ); // Sensible load due to infiltration in one zone
		static Real64 ZdoorSensLoad( 0.0 ); // Sensible load due to UA delta T through closed door in one zone

		WalkInSchedule = GetCurrentScheduleValue( WalkIn( WalkInID ).SchedPtr );
		if ( WalkInSchedule <= 0 ) return;
		// GET OTHER SCHEDULES
		DefrostSchedule = GetCurrentScheduleValue( WalkIn( WalkInID ).DefrostSchedPtr );
		DefrostDripDownSchedule = GetCurrentScheduleValue( WalkIn( WalkInID ).DefrostDripDownSchedPtr );
		//next statement In case user doesn't understand concept of drip down schedule
		DefrostDripDownSchedule = max( DefrostDripDownSchedule, DefrostSchedule );

		//next four values optional, so set to default before checking for schedule
		StockingLoad = 0.0;
		LightingSchedule = 1.0;
		HeaterSchedule = 1.0;
		CircFanSchedule = 1.0;
		if ( WalkIn( WalkInID ).StockingSchedPtr > 0 ) StockingLoad = GetCurrentScheduleValue( WalkIn( WalkInID ).StockingSchedPtr );
		if ( WalkIn( WalkInID ).LightingSchedPtr > 0 ) LightingSchedule = GetCurrentScheduleValue( WalkIn( WalkInID ).LightingSchedPtr );
		if ( WalkIn( WalkInID ).HeaterSchedPtr > 0 ) HeaterSchedule = GetCurrentScheduleValue( WalkIn( WalkInID ).HeaterSchedPtr );
		if ( WalkIn( WalkInID ).CircFanSchedPtr > 0 ) CircFanSchedule = GetCurrentScheduleValue( WalkIn( WalkInID ).CircFanSchedPtr );

		//Set local subroutine variables for convenience
		TWalkIn = WalkIn( WalkInID ).Temperature;
		DesignRatedCap = WalkIn( WalkInID ).DesignRatedCap;
		DefrostCap = WalkIn( WalkInID ).DefrostCapacity;
		// %DefrostCapacity already set to zero for WalkInDefrostNone , WalkInDefrostOffCycle
		DesignLighting = WalkIn( WalkInID ).DesignLighting;
		EnthalpyAirWalkIn = PsyHFnTdbRhPb( TWalkIn, 0.9, OutBaroPress ); //assume 90%RH in cooler
		HumRatioAirWalkIn = PsyWFnTdbH( TWalkIn, EnthalpyAirWalkIn );
		DensityAirWalkIn = PsyRhoAirFnPbTdbW( OutBaroPress, TWalkIn, HumRatioAirWalkIn );
		Conv = Latitude * 2.0 * Pi / 360.0; //Convert Latitude to radians
		Gravity = 9.780373 * ( 1.0 + 0.0052891 * pow_2( std::sin( Conv ) ) - 0.0000059 * pow_2( std::sin( 2.0 * Conv ) ) );

		// CALCULATE ALL LOADS INFLUENCED BY ZONE TEMPERATURE AND RH
		//set to zero before summing over zones
		SensibleLoadTotal = 0.0;
		LatentLoadTotal = 0.0;
		WalkIn( WalkInID ).SensZoneCreditRate = 0.0;
		WalkIn( WalkInID ).SensZoneCreditCoolRate = 0.0;
		WalkIn( WalkInID ).SensZoneCreditCool = 0.0;
		WalkIn( WalkInID ).SensZoneCreditHeatRate = 0.0;
		WalkIn( WalkInID ).SensZoneCreditHeat = 0.0;
		WalkIn( WalkInID ).LatZoneCreditRate = 0.0;

		//Start zone loop:
		for ( ZoneID = 1; ZoneID <= WalkIn( WalkInID ).NumZones; ++ZoneID ) {
			ZoneSensLoad = 0.0;
			GlassDoorSensHeat = 0.0;
			StockDoorSensHeat = 0.0;
			ZoneNum = WalkIn( WalkInID ).ZoneNum( ZoneID );
			ZoneNodeNum = WalkIn( WalkInID ).ZoneNodeNum( ZoneID );
			ZoneDryBulb = Node( ZoneNodeNum ).Temp;
			DelTemp = ZoneDryBulb - TWalkIn;
			StockDoorArea = WalkIn( WalkInID ).AreaStockDr( ZoneID );
			GlassDoorArea = WalkIn( WalkInID ).AreaGlassDr( ZoneID );
			UAOtherSurfaces = WalkIn( WalkInID ).SurfaceArea( ZoneID ) * WalkIn( WalkInID ).UValue( ZoneID );
			DoorFlowFactor = 0.8; //see ASHRAE Refrigeration, p13.5, 2006
			if ( DelTemp <= 11.0 ) DoorFlowFactor = 1.1; // from ASHRAE Refrigeration Loads

			//Get infiltration loads if either type of door is present in this zone
			if ( StockDoorArea > 0.0 || GlassDoorArea > 0.0 ) {
				ZoneRHFrac = PsyRhFnTdbWPb( Node( ZoneNodeNum ).Temp, Node( ZoneNodeNum ).HumRat, OutBaroPress, RoutineName );
				EnthalpyZoneAir = PsyHFnTdbRhPb( ZoneDryBulb, ZoneRHFrac, OutBaroPress, RoutineName );
				HumRatioZoneAir = PsyWFnTdbH( ZoneDryBulb, EnthalpyZoneAir, RoutineName );
				DensityZoneAir = PsyRhoAirFnPbTdbW( OutBaroPress, ZoneDryBulb, HumRatioZoneAir, RoutineName );
				if ( DensityZoneAir < DensityAirWalkIn ) { //usual case when walk in is colder than zone
					DensitySqRtFactor = std::sqrt( 1.0 - DensityZoneAir / DensityAirWalkIn );
					DensityFactorFm = std::pow( 2.0 / ( 1.0 + std::pow( DensityAirWalkIn / DensityZoneAir, 0.333 ) ), 1.5 );
				} else { //temperature inversion with zone colder and/or drier than walk-in, infiltration in reverse direction
					//The enthalpy difference will show whether the energy transport is reversed
					//(same air mass exchange in either direction )
					//That is, these factors establish the magnitude of the exchange air flow, not direction
					DensitySqRtFactor = std::sqrt( 1.0 - DensityAirWalkIn / DensityZoneAir );
					DensityFactorFm = std::pow( 2.0 / ( 1.0 + std::pow( DensityZoneAir / DensityAirWalkIn, 0.333 ) ), 1.5 );
				} // check for density in zone and in walk-in to avoid taking sqrt of neg number
				GlassDoorInfLoad = 0.0;
				StockDoorInfLoad = 0.0;
				StockDoorSensHeat = 0.0;
				GlassDoorSensHeat = 0.0;
				if ( StockDoorArea > 0.0 ) {
					{ auto const SELECT_CASE_var( WalkIn( WalkInID ).StockDoorProtectType( ZoneID ) );
					//Values from ASHRAE Ref p 13.6
					if ( SELECT_CASE_var == WIStockDoorNone ) {
						DoorProtectEff = 0.0;
					} else if ( SELECT_CASE_var == WIStockDoorAirCurtain ) {
						DoorProtectEff = 0.5;
					} else if ( SELECT_CASE_var == WIStockDoorStripCurtain ) {
						DoorProtectEff = 0.9;
					}}
					DrHeight = WalkIn( WalkInID ).HeightStockDr( ZoneID );
					DrArea = StockDoorArea;
					// if exists, get Stock Door Zone schedule
					DoorOpenFactor = DefaultWalkInDoorOpenFactor;
					if ( WalkIn( WalkInID ).StockDoorOpenSchedPtr( ZoneID ) > 0 ) DoorOpenFactor = GetCurrentScheduleValue( WalkIn( WalkInID ).StockDoorOpenSchedPtr( ZoneID ) );

					FullFlowInfLoad = 0.221 * DrArea * ( EnthalpyZoneAir - EnthalpyAirWalkIn ) * DensityAirWalkIn * DensitySqRtFactor * std::sqrt( Gravity * DrHeight ) * DensityFactorFm;
					StockDoorInfLoad = FullFlowInfLoad * DoorOpenFactor * DoorFlowFactor * ( 1.0 - DoorProtectEff );
					StockDoorSensHeat = DrArea * WalkIn( WalkInID ).UValueStockDr( ZoneID ) * DelTemp;
				} //have stock doors

				if ( GlassDoorArea > 0.0 ) {
					DoorProtectEff = 0.5; // Assume glass doors have air curtain
					DrHeight = WalkIn( WalkInID ).HeightGlassDr( ZoneID );
					DrArea = GlassDoorArea;
					// get Glass Door Zone schedule
					DoorOpenFactor = DefaultWalkInDoorOpenFactor; //default value
					if ( WalkIn( WalkInID ).GlassDoorOpenSchedPtr( ZoneID ) > 0 ) DoorOpenFactor = GetCurrentScheduleValue( WalkIn( WalkInID ).GlassDoorOpenSchedPtr( ZoneID ) );

					FullFlowInfLoad = 0.221 * DrArea * ( EnthalpyZoneAir - EnthalpyAirWalkIn ) * DensityAirWalkIn * DensitySqRtFactor * std::sqrt( Gravity * DrHeight ) * DensityFactorFm;
					GlassDoorInfLoad = FullFlowInfLoad * DoorOpenFactor * DoorFlowFactor * ( 1.0 - DoorProtectEff );
					GlassDoorSensHeat = DrArea * WalkIn( WalkInID ).UValueGlassDr( ZoneID ) * DelTemp;
				} //have Glass doors

				//assume mass dry air infiltrating into walk-in == mass out into zone,
				//                       that is, equal air exchange (ASHRAE 2006 Refrigeration)
				ZoneInfilLoad = -StockDoorInfLoad - GlassDoorInfLoad;
				MassDryAirRate = -ZoneInfilLoad / ( EnthalpyZoneAir - EnthalpyAirWalkIn );
				WaterRemovRate = MassDryAirRate * ( HumRatioZoneAir - HumRatioAirWalkIn );
				// Just as with cases,  we assume no latent credit (water removal = 0) to zone or load on cooler during dripdown
				// To be consistent with the treatment of refrigerated cases, latent load
				//  and latent credit are bothbased on reducing the infiltrating vapor to ice.  (This is
				//  slightly greater than if the latent credit were based upon condensing out the water as liquid.)
				//  then it would be: ZoneLatentLoad = -WaterRemovRate * WaterToVaporEnthalpy * (1.0d0-DefrostDripDownSchedule)
				ZoneLatentLoad = -WaterRemovRate * IcetoVaporEnthalpy * ( 1.0 - DefrostDripDownSchedule );
				ZInfilSensLoad = ZoneInfilLoad - ( -WaterRemovRate * IcetoVaporEnthalpy ); //done to avoid moving latent to sens during dripdown
				ZdoorSensLoad = -GlassDoorSensHeat - StockDoorSensHeat;
				WalkInLatLoad = -ZoneLatentLoad;
				if ( WalkIn( WalkInID ).TEvapDesign <= 0.0 ) { // water turned to ice on coil
					WalkInLatLoad = WaterRemovRate * IcetoVaporEnthalpy * ( 1.0 - DefrostDripDownSchedule );
					// FROST:  keep track of frost build up on evaporator coil
					//         avoid accumulation during warm-up to avoid reverse dd test problem
					if ( ! WarmupFlag ) {
						FrostChangekg = ( WaterRemovRate * TimeStepZoneSec ) * ( 1.0 - DefrostDripDownSchedule );
						WalkIn( WalkInID ).KgFrost += FrostChangekg;
					}
				} //water to ice
			} //No doors

			ZoneSensLoad = ZInfilSensLoad + ZdoorSensLoad - UAOtherSurfaces * DelTemp;
			WalkInSensLoad = -ZoneSensLoad;

			// Update globals for use in ZoneTemperaturePredictorCorrector (Air Heat Balance) and
			//   Zone Equipment Manager. Sum walk-in credits to zone using existing 'casecredit' variable
			//   No return air fractions are applied to walk-ins, and no latent in stocking -

			RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone += ZoneSensLoad;
			RefrigCaseCredit( ZoneNum ).LatCaseCreditToZone += ZoneLatentLoad;

			// Set up report variables for each zone for this walk-in
			// Sensible heat exchange can be positive or negative, split into separate output variables and always report positive value
			WalkIn( WalkInID ).SensZoneCreditRate( ZoneID ) = ZoneSensLoad;
			if ( ZoneSensLoad <= 0.0 ) {
				WalkIn( WalkInID ).SensZoneCreditCoolRate( ZoneID ) = -ZoneSensLoad;
				WalkIn( WalkInID ).SensZoneCreditCool( ZoneID ) = -ZoneSensLoad * TimeStepZoneSec;
				WalkIn( WalkInID ).SensZoneCreditHeatRate( ZoneID ) = 0.0;
				WalkIn( WalkInID ).SensZoneCreditHeat( ZoneID ) = 0.0;
			} else {
				WalkIn( WalkInID ).SensZoneCreditHeatRate( ZoneID ) = ZoneSensLoad;
				WalkIn( WalkInID ).SensZoneCreditHeat( ZoneID ) = ZoneSensLoad * TimeStepZoneSec;
				WalkIn( WalkInID ).SensZoneCreditCoolRate( ZoneID ) = 0.0;
				WalkIn( WalkInID ).SensZoneCreditCool( ZoneID ) = 0.0;
			}
			// This rate should always be negative
			WalkIn( WalkInID ).LatZoneCreditRate( ZoneID ) = ZoneLatentLoad;
			WalkIn( WalkInID ).LatZoneCredit( ZoneID ) = ZoneLatentLoad * TimeStepZoneSec;

			//Running total over all zones, use later to dispatch capacity
			SensibleLoadTotal += WalkInSensLoad;
			LatentLoadTotal += WalkInLatLoad;

		} //Do loop over zones for zone-condition-related sensible and latent loads

		//cooling coil fan power default is 375W, = 1/2 HP (Tyler showed 1/3 to 3/4 hp)

		// CALCULATE AUX LOADS DUE TO LIGHTS, FANS AND HEATERS
		LightLoad = DesignLighting * LightingSchedule;
		// turn coil fan off during defrost/drip - down period
		FanLoad = WalkIn( WalkInID ).CircFanPower * CircFanSchedule + WalkIn( WalkInID ).CoilFanPower * ( 1.0 - DefrostDripDownSchedule );
		HeaterLoad = WalkIn( WalkInID ).HeaterPower * HeaterSchedule;
		// Calculate floor load - using 'GroundTemp' assigned in weather manager (can be entered by user if desired)
		//    Default value is 18C.
		FloorLoad = WalkIn( WalkInID ).FloorArea * WalkIn( WalkInID ).FloorUValue * ( GroundTemp - TWalkIn );

		//DEFROST CALCULATIONS
		if ( ( DefrostSchedule > 0.0 ) && ( WalkIn( WalkInID ).DefrostType != WalkInDefrostNone ) && ( WalkIn( WalkInID ).DefrostType != WalkInDefrostOffCycle ) ) {
			DefrostLoad = DefrostCap * DefrostSchedule; //W
			StartFrostKg = WalkIn( WalkInID ).KgFrost;
			DefrostEnergy = DefrostLoad * TimeStepZoneSec; //Joules
			if ( WalkIn( WalkInID ).DefrostControlType == DefrostContTempTerm ) {
				//  Need to turn defrost system off early if controlled by temperature and all ice melted
				//  For temperature termination, need to recognize not all defrost heat goes to melt ice
				//  Some goes to misc losses (for fluid defrost, some coil areas bare earlier than
				//  others and xfer heat to environment)
				//  Assume full ice melting satisfies temperature control.
				//      (defaults for DefEnergyFraction are :=0.7 for elec, =0.3 for fluids)
				DefEnergyFraction = WalkIn( WalkInID ).DefEnergyFraction;
				AvailDefrostEnergy = DefEnergyFraction * DefrostEnergy; //Joules avail to melt ice
				IceSensHeatNeeded = 0.0;
				if ( StartFrostKg > 0.0 ) {
					if ( WalkIn( WalkInID ).IceTemp < 0.0 ) {
						StartIceTemp = WalkIn( WalkInID ).IceTemp;
						IceSensHeatNeeded = StartFrostKg * SpecificHeatIce * ( 0.0 - StartIceTemp ); //Joules
						if ( AvailDefrostEnergy >= IceSensHeatNeeded ) {
							WalkIn( WalkInID ).IceTemp = 0.0;
							AvailDefrostEnergy -= IceSensHeatNeeded; //Joules
						} else { //DefrostEnergy < IceSensHeatNeeded
							WalkIn( WalkInID ).IceTemp = StartIceTemp + AvailDefrostEnergy / ( SpecificHeatIce * StartFrostKg );
							AvailDefrostEnergy = 0.0;
						} // AvailDefrostEnergy >= IceSensHeatNeeded
					} // IceTemp < 0,  need to raise temperature of ice
					//Reduce defrost heat load on walkin by amount of ice melted during time step
					FrostChangekg = min( AvailDefrostEnergy / IceMeltEnthalpy, StartFrostKg );
					if ( FrostChangekg < StartFrostKg ) {
						DefrostLoad -= FrostChangekg * IceMeltEnthalpy / TimeStepZone / SecInHour;
						if ( ! WarmupFlag ) WalkIn( WalkInID ).KgFrost = StartFrostKg - FrostChangekg;
						//DefrostSchedule not changed
					} else { // all frost melted during time step, so need to terminate defrost
						//  see Aug 8 page 3 notes
						WalkIn( WalkInID ).KgFrost = 0.0;
						DefrostEnergyNeeded = ( IceSensHeatNeeded + ( FrostChangekg * IceMeltEnthalpy ) ) / DefEnergyFraction; //Joules - energy needed including E unavail to melt ice
						DefrostSchedule = min( DefrostSchedule, ( DefrostEnergyNeeded / ( DefrostCap * TimeStepZoneSec ) ) );
						// reduce load on walkin by energy put into ice melting
						DefrostLoad = max( 0.0, ( DefrostSchedule * DefrostCap - ( IceSensHeatNeeded + ( FrostChangekg * IceMeltEnthalpy ) ) / TimeStepZoneSec ) );
						WalkIn( WalkInID ).IceTemp = WalkIn( WalkInID ).TEvapDesign;

					} // frost melted during time step less than amount of ice at start
				} else { // no frost present so terminate defrost and reset ice temperature for start of next defrost
					DefrostLoad = 0.0;
					DefrostSchedule = 0.0;
					WalkIn( WalkInID ).IceTemp = WalkIn( WalkInID ).TEvapDesign;
				} // have frost present

			} else { //Not temperature control type
				FrostChangekg = min( DefrostEnergy / IceMeltEnthalpy, StartFrostKg );
				//Reduce defrost heat load on walkin by amount of ice melted during time step
				DefrostLoad -= FrostChangekg * IceMeltEnthalpy / TimeStepZone / SecInHour;
				if ( ! WarmupFlag ) WalkIn( WalkInID ).KgFrost = StartFrostKg - FrostChangekg;
				//DefrostSchedule not changed
			} //Temperature termination control type

		} else { //DefrostSchedule <= 0 or have None or OffCycle
			DefrostLoad = 0.0;
		} //Defrost calculations

		if ( WalkIn( WalkInID ).DefrostType == WalkInDefrostElec ) {
			WalkIn( WalkInID ).ElecDefrostConsumption = DefrostCap * DefrostSchedule * TimeStepZoneSec;
			WalkIn( WalkInID ).ElecDefrostPower = DefrostCap * DefrostSchedule;
		} else {
			WalkIn( WalkInID ).ElecDefrostConsumption = 0.0;
			WalkIn( WalkInID ).ElecDefrostPower = 0.0;
		}

		// If hot brine or hot gas is used for defrost, need to reduce condenser load by heat reclaimed for defrost
		if ( WalkIn( WalkInID ).DefrostType == WalkInDefrostFluid ) WalkIn( WalkInID ).HotDefrostCondCredit = DefrostCap * DefrostSchedule;

		// loads reflects that walk ins continue to accumulate loads, even during defrost
		// but cap is used to report portion met by active system while operating

		//*** See if capacity meets load and manage accumulated stored energy ***********************************
		SensibleLoadTotal += LightLoad + HeaterLoad + FanLoad + StockingLoad + DefrostLoad + FloorLoad;
		LoadTotal = SensibleLoadTotal + LatentLoadTotal;

		//Account for difference between load and capacity. Assume rack or system able to provide
		// rated capacity.  If it can't, that unmet energy will be stored and discharged at the system level.
		//  Here we are calculating the load the walk-in cooler places on the refrigeration compressor systems.
		//  Meet current load to the extent possible.  If extra capacity available,
		//  apply it to previously unmet/stored loads.  If capacity less than current load,
		//  (e.g. as it is during defrost cycles) save the unmet/stored load to be met in
		//  succeeding time steps. This is an artificial way of recognizing that the internal
		//  temperature will increase by a small amount during defrost and the system will have to
		//  run full out until the temperature is brought back down.

		StoredEnergyRate = WalkIn( WalkInID ).StoredEnergy / TimeStepZone / SecInHour;
		LoadRequested = LoadTotal + StoredEnergyRate;

		// prorate available cooling capacity for portion of time off due to drip down.
		MaxCap = DesignRatedCap * ( 1.0 - DefrostDripDownSchedule );
		if ( MaxCap >= LoadRequested ) {
			//Have more at least as much capacity available as needed, even counting stored energy
			CapApplied = LoadRequested;
			SensibleCapApplied = SensibleLoadTotal + StoredEnergyRate;
			LatentCapApplied = LatentLoadTotal;
			WalkIn( WalkInID ).StoredEnergy = 0.0;
		} else {
			//Don't have as much capacity as needed (during dripdown or period following dripdown)
			CapApplied = MaxCap;
			LatentCapApplied = min( LatentLoadTotal, MaxCap ); //Latent load should never be > capavail, but just in case...
			SensibleCapApplied = CapApplied - LatentCapApplied;
			if ( ! WarmupFlag ) WalkIn( WalkInID ).StoredEnergy += ( LoadTotal - MaxCap ) * TimeStepZoneSec;
		} //CapAvail vs Load requested

		// ReportWalkIn( WalkInID)
		WalkIn( WalkInID ).TotalCoolingLoad = CapApplied;
		WalkIn( WalkInID ).TotalCoolingEnergy = CapApplied * TimeStepZoneSec;
		WalkIn( WalkInID ).TotSensCoolingEnergyRate = SensibleCapApplied;
		WalkIn( WalkInID ).TotSensCoolingEnergy = SensibleCapApplied * TimeStepZoneSec;
		WalkIn( WalkInID ).TotLatCoolingEnergyRate = LatentCapApplied;
		WalkIn( WalkInID ).TotLatCoolingEnergy = LatentCapApplied * TimeStepZoneSec;

		WalkIn( WalkInID ).ElecFanPower = FanLoad;
		WalkIn( WalkInID ).ElecFanConsumption = FanLoad * TimeStepZoneSec;
		WalkIn( WalkInID ).ElecHeaterPower = HeaterLoad;
		WalkIn( WalkInID ).ElecHeaterConsumption = HeaterLoad * TimeStepZoneSec;
		WalkIn( WalkInID ).ElecLightingPower = LightLoad;
		WalkIn( WalkInID ).ElecLightingConsumption = LightLoad * TimeStepZoneSec;
		WalkIn( WalkInID ).TotalElecPower = FanLoad + HeaterLoad + LightLoad + WalkIn( WalkInID ).ElecDefrostPower;
		WalkIn( WalkInID ).TotalElecConsumption = WalkIn( WalkInID ).TotalElecPower * TimeStepZoneSec;

		//**************************************************************************************************
		// Cap Energy and Kg Frost to avoid floating overflow errors
		// 1-time warning is issued. It should be rare but could happen with unrealistic inputs.
		if ( WalkIn( WalkInID ).StoredEnergy > MyLargeNumber ) {
			WalkIn( WalkInID ).StoredEnergy = MyLargeNumber;
			if ( ShowUnmetWIEnergyWarning( WalkInID ) ) {
				ShowWarningError( "Refrigeration:WalkIn: " + WalkIn( WalkInID ).Name );
				ShowContinueError( " This walk-in cooler has insufficient capacity to meet the loads" );
				ShowContinueError( "... Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString() );
				ShowContinueError( " Refer to documentation for further explanation of Total Cooling Capacity." );
				ShowUnmetWIEnergyWarning( WalkInID ) = false;
			} // ShowStoreEnergyWarning
		} // stored energy > large number
		if ( WalkIn( WalkInID ).KgFrost > MyLargeNumber ) {
			WalkIn( WalkInID ).KgFrost = MyLargeNumber;
			if ( ShowWIFrostWarning( WalkInID ) ) {
				ShowWarningError( "Refrigeration:WalkIn: " + WalkIn( WalkInID ).Name );
				ShowContinueError( " This walkin cooler has insufficient defrost capacity to remove the excess frost accumulation." );
				ShowContinueError( " Check the defrost schedule or defrost capacity. " );
				ShowContinueError( "... Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString() );
				ShowWIFrostWarning( WalkInID ) = false;
			}
		}

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateSecondary( int const SecondaryNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL
		//       DATE WRITTEN   Spring 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the total cooling load, pump power, and needed primary refrigerant supply temperature
		// for a secondary system.

		// METHODOLOGY EMPLOYED:
		// Sum the loads for the cases and walk-ins supplied by a secondary loop.
		// Calculate the pumping power.
		// Assume that the fluid supply and return temperatures are fixed and the
		// fluid flow rate is varied to meed the variable load.
		// User has been told in IO and Eng ref: for secondary systems/pumps: pump energy is f(viscosity),
		//        but since specifying Tcircfluid as steady
		//        state in loop, specify power for fluid and system head/resistance at that temp
		//ashrae 2006 p4.1 supports 78% eff for pump impellers
		//  all power into heat because it would otherwise not be counted in zone
		//  if use semihermetic motor, also need to add motor ineff as heat

		// REFERENCES:
		// SCE report
		//  others

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTsatFnHPb;
		using Psychrometrics::PsyWFnTdpPb;
		using Psychrometrics::PsyHFnTdbRhPb;
		//unused  USE DataWater,         ONLY: WaterStorage

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrorTol( 0.001 ); // Iterative solution tolerance

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool AtPartLoad; // Whether or not need to iterate on pump power
		bool DeRate; // If true, need to derate aircoils because don't carry over unmet energy
		int CaseID; // used in summing case loads on loop
		int CaseNum; // used in summing case loads on loop
		int CoilID; // used in summing coil loads on loop
		int CoilIndex; // used in summing coil loads on loop
		int DistPipeZoneNum; // used to assign case credit to zone
		int Iter; // loop counter
		int NumPumps; // number of pumps (or stages if used to approx var speed) on loop
		int PumpID; // loop counter
		int ReceiverZoneNum; // used to assign case credit it zone
		int WalkInID; // used in summing walk-in loads on loop
		int WalkInIndex; // used in summing walk-in loads on loop
		int ZoneNodeNum; // used to establish environmental temperature for dist piping heat gains
		Real64 CpBrine; // Specific heat (W/kg)
		Real64 CircRatio; // Per ASHRAE definition = mass flow at pump/mass flow to condenser
		Real64 DensityBrine; // Density (kg/m3)
		Real64 DiffTemp; // (C)
		Real64 DistPipeHeatGain; // Optional (W)
		Real64 Error; // Used in iterative soln for pumps needed to meet load (that has to include pump energy)
		Real64 Eta; // Secondary loop heat exchanger eta, dimensionless
		Real64 FlowVolNeeded; // Flow rate needed to meet load (m3/s)
		static Real64 LoadRequested( 0.0 ); // Load necessary to meet current and all stored energy needs (W)
		static Real64 LocalTimeStep( 0.0 ); // TimeStepZone for case/walkin systems, TimeStepSys for coil systems
		Real64 MaxLoad; // Secondary loop capacity can be limited by heat exchanger or pumps (W)
		Real64 MaxVolFlow; // Flow can be limited by either total pump capacity or heat exchanger design (m3/s)
		Real64 PartLdFrac; // Used to ratio pump power
		Real64 PartPumpFrac; // Used to see if part pumps dispatched meets part pump load
		//REAL(r64)   :: PartPower         ! Used to ratio power for last pump added to loop
		Real64 PrevTotalLoad; // Used in pump energy convergence test
		Real64 ReceiverHeatGain; // Optional (W)
		Real64 RefrigerationLoad; // Load for cases and walk-ins served by loop, does not include pump energy (W)
		Real64 StoredEnergyRate; // Used to meet loads unmet in previous time step (related to defrost cycles
		//     on cases/walk-ins served)(W)
		Real64 TBrineAverage; // (C)
		Real64 TBrineIn; // Brine temperature going to heat exchanger, C
		Real64 TCondense; // Condensing temperature for a phase change secondary loop, C
		Real64 TEvap; // Evaporating temperature in secondary loop heat exchanger (C)
		Real64 TotalCoolingLoad( 0 ); // Cooling load reported back to compressor rack or detailed system (W)
		Real64 TotalHotDefrostCondCredit; // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
		Real64 TotalPumpPower; // Total Pumping power for loop, W
		Real64 TotalLoad; // Total Cooling Load on secondary loop, W
		Real64 TPipesReceiver; // Temperature used for contents of pipes and/or receiver in calculating shell losses (C)
		Real64 VarFrac; // Pump power fraction for variable speed pump, dimensionless
		Real64 VolFlowRate; // Used in dispatching pumps to meet load (m3/s)
		Real64 UnmetEnergy; // Cumulative, grows and shrinks with defrost cycles on loads served by loop (J)

		LocalTimeStep = TimeStepZone;
		if ( UseSysTimeStep ) LocalTimeStep = TimeStepSys;

		NumPumps = Secondary( SecondaryNum ).NumPumps;
		TEvap = Secondary( SecondaryNum ).TEvapDesign;
		MaxVolFlow = Secondary( SecondaryNum ).MaxVolFlow;
		MaxLoad = Secondary( SecondaryNum ).MaxLoad;
		UnmetEnergy = Secondary( SecondaryNum ).UnmetEnergy;
		{ auto const SELECT_CASE_var( Secondary( SecondaryNum ).FluidType );
		if ( SELECT_CASE_var == SecFluidTypeAlwaysLiquid ) {
			CpBrine = Secondary( SecondaryNum ).CpBrineRated;
			DensityBrine = Secondary( SecondaryNum ).DensityBrineRated;
			Eta = Secondary( SecondaryNum ).HeatExchangeEta;
			TBrineAverage = Secondary( SecondaryNum ).TBrineAverage;
			TBrineIn = Secondary( SecondaryNum ).TBrineInRated;
			TPipesReceiver = TBrineAverage;
		} else if ( SELECT_CASE_var == SecFluidTypePhaseChange ) {
			CircRatio = Secondary( SecondaryNum ).CircRate;
			TCondense = Secondary( SecondaryNum ).TCondense;
			TPipesReceiver = TCondense;
		}} // Fluid type

		//Initialize this secondary for this time step
		TotalLoad = 0.0;
		TotalPumpPower = 0.0;
		RefrigerationLoad = 0.0;
		TotalHotDefrostCondCredit = 0.0;
		FlowVolNeeded = 0.0;
		DeRate = false;

		//SCE page 28 gives a delta T for pipe heat gains
		//         (.25F each for supply and discharge) for use with mdot*cp.
		//          However, another author shows this as a major diff between dx and secondary
		//          So - allow the user to include this in his total load, even though he has to do
		//          most of the calculations before the input (to get to SumUADistPiping)).
		DistPipeHeatGain = 0.0;
		if ( Secondary( SecondaryNum ).SumUADistPiping > MySmallNumber ) {
			ZoneNodeNum = Secondary( SecondaryNum ).DistPipeZoneNodeNum;
			DiffTemp = Node( ZoneNodeNum ).Temp - TPipesReceiver;
			DistPipeHeatGain = DiffTemp * Secondary( SecondaryNum ).SumUADistPiping;
			DistPipeZoneNum = Secondary( SecondaryNum ).DistPipeZoneNum;
			// pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
			//     from refrigcasecredit (- for cooling zone, + for heating zone)
			Secondary( SecondaryNum ).DistPipeZoneHeatGain = -DistPipeHeatGain;
			RefrigCaseCredit( DistPipeZoneNum ).SenCaseCreditToZone -= DistPipeHeatGain;
		} //calc distribution piping heat gains

		ReceiverHeatGain = 0.0;
		if ( Secondary( SecondaryNum ).SumUAReceiver > MySmallNumber ) {
			ZoneNodeNum = Secondary( SecondaryNum ).ReceiverZoneNodeNum;
			DiffTemp = Node( ZoneNodeNum ).Temp - TPipesReceiver;
			ReceiverHeatGain = DiffTemp * Secondary( SecondaryNum ).SumUAReceiver;
			ReceiverZoneNum = Secondary( SecondaryNum ).ReceiverZoneNum;
			// receiver heat load is a positive number (ie. heat absorbed by receiver, so needs to be subtracted
			//     from refrigcasecredit (- for cooling zone, + for heating zone)
			Secondary( SecondaryNum ).ReceiverZoneHeatGain = -ReceiverHeatGain;
			RefrigCaseCredit( ReceiverZoneNum ).SenCaseCreditToZone -= ReceiverHeatGain;
		} //calc receiver heat gains

		//Sum up all the case and walk-in loads served by the secondary loop
		if ( Secondary( SecondaryNum ).NumCases > 0 ) {
			for ( CaseNum = 1; CaseNum <= Secondary( SecondaryNum ).NumCases; ++CaseNum ) {
				CaseID = Secondary( SecondaryNum ).CaseNum( CaseNum );
				CalculateCase( CaseID );
				// increment TotalCoolingLoad Hot gas/brine defrost credits for each secondary loop
				RefrigerationLoad += RefrigCase( CaseID ).TotalCoolingLoad;
				TotalHotDefrostCondCredit += RefrigCase( CaseID ).HotDefrostCondCredit;
			} //CaseNum
		} //NumCases > 0
		if ( Secondary( SecondaryNum ).NumWalkIns > 0 ) {
			for ( WalkInIndex = 1; WalkInIndex <= Secondary( SecondaryNum ).NumWalkIns; ++WalkInIndex ) {
				WalkInID = Secondary( SecondaryNum ).WalkInNum( WalkInIndex );
				CalculateWalkIn( WalkInID );
				// increment TotalCoolingLoad for  each system
				RefrigerationLoad += WalkIn( WalkInID ).TotalCoolingLoad;
				TotalHotDefrostCondCredit += WalkIn( WalkInID ).HotDefrostCondCredit;
			} //NumWalkIns systems
		} //Secondary(SecondaryNum)%NumWalkIns > 0

		if ( Secondary( SecondaryNum ).NumCoils > 0 ) {
			for ( CoilIndex = 1; CoilIndex <= Secondary( SecondaryNum ).NumCoils; ++CoilIndex ) {
				CoilID = Secondary( SecondaryNum ).CoilNum( CoilIndex );
				// already CALL CalculateCoil(CoilID) for each coil, dispatched in coilset order for each zone
				// increment TotalCoolingLoad for each system
				//  here will find out if secondary can serve total load, if not will derate coil outout/case credits
				RefrigerationLoad += WarehouseCoil( CoilID ).TotalCoolingLoad;
				TotalHotDefrostCondCredit += WarehouseCoil( CoilID ).HotDefrostCondCredit;
			} //NumCoils on seocndary system
		} //Secondary(SecondaryNum)%NumCoils > 0

		TotalLoad = RefrigerationLoad + DistPipeHeatGain + ReceiverHeatGain;
		AtPartLoad = true;
		//Check to see if load is already >+ maxload without pump heat
		if ( Secondary( SecondaryNum ).FluidType == SecFluidTypeAlwaysLiquid ) { //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
			FlowVolNeeded = TotalLoad / Eta / ( CpBrine * DensityBrine * ( TBrineIn - TEvap ) );
			// For brine/glycol systems, find flow volume needed to meet load
			// Per ashrae 2006, p4.1, eval mass flow rate to pump at brine return (to chiller) temp
			//   because pumps located in return piping
			if ( FlowVolNeeded >= MaxVolFlow ) {
				//Don't need to iterate on pumps, just set to max.  Will have unmet load this time step (unless coils present)
				VolFlowRate = MaxVolFlow;
				TotalPumpPower = Secondary( SecondaryNum ).PumpTotRatedPower;
				TotalLoad += TotalPumpPower * Secondary( SecondaryNum ).PumpPowerToHeat;
				AtPartLoad = false;
				if ( Secondary( SecondaryNum ).NumCoils > 0 ) DeRate = true;
			} //flowvolneeded >= maxvolflow
		} else { // have SecFluidTypePhaseChange !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
			if ( TotalLoad >= MaxLoad ) {
				TotalPumpPower = Secondary( SecondaryNum ).PumpTotRatedPower;
				TotalLoad += TotalPumpPower * Secondary( SecondaryNum ).PumpPowerToHeat;
				VolFlowRate = MaxVolFlow;
				AtPartLoad = false;
				if ( Secondary( SecondaryNum ).NumCoils > 0 ) DeRate = true;
			}
		} //fluid type check for max load or max flow       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

		// If totalLoad < maxload, then need to calculate partial pump load
		// Need an iterative soln for pump energy needed to meet total load
		//  because that total load has to include pump energy
		if ( AtPartLoad ) {
			for ( Iter = 1; Iter <= 10; ++Iter ) {
				if ( TotalLoad <= 0.0 ) {
					// Load on secondary loop is zero (or negative).
					// Set volumetric flow rate and pump power to be zero.
					VolFlowRate = 0.0;
					TotalPumpPower = 0.0;
					break;
				}
				PrevTotalLoad = TotalLoad;
				if ( Secondary( SecondaryNum ).FluidType == SecFluidTypeAlwaysLiquid ) {
					FlowVolNeeded = TotalLoad / Eta / ( CpBrine * DensityBrine * ( TBrineIn - TEvap ) );
					PartLdFrac = FlowVolNeeded / MaxVolFlow;
				} else {
					PartLdFrac = TotalLoad / MaxLoad;
				}
				if ( Secondary( SecondaryNum ).PumpControlType == SecPumpControlConstant ) {
					VolFlowRate = 0.0;
					TotalPumpPower = 0.0;
					for ( PumpID = 1; PumpID <= NumPumps; ++PumpID ) { //dispatch pumps to meet needed flow rate
						if ( Secondary( SecondaryNum ).FluidType == SecFluidTypeAlwaysLiquid ) { //>>>>>>>>>>>>>>>>>>>>>
							VolFlowRate += Secondary( SecondaryNum ).PumpIncrementFlowVol;
							TotalPumpPower += Secondary( SecondaryNum ).PumpIncrementPower;
							if ( VolFlowRate >= FlowVolNeeded ) break;
						} else { // fluid type phase change >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
							VolFlowRate += Secondary( SecondaryNum ).PumpIncrementFlowVol;
							TotalPumpPower += Secondary( SecondaryNum ).PumpIncrementPower;
							PartPumpFrac = TotalPumpPower / Secondary( SecondaryNum ).PumpTotRatedPower;
							if ( PartPumpFrac >= PartLdFrac ) break;
						} //fluid type              >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
					} //Dispatching pumps until fluid flow need is met
				} else { // pump type variable
					VarFrac = max( 0.1, CurveValue( Secondary( SecondaryNum ).VarSpeedCurvePtr, PartLdFrac ) );
					TotalPumpPower = Secondary( SecondaryNum ).PumpTotRatedPower * VarFrac;
					VolFlowRate = MaxVolFlow * PartLdFrac;
				} // pump type

				TotalLoad = RefrigerationLoad + DistPipeHeatGain + ReceiverHeatGain + TotalPumpPower * Secondary( SecondaryNum ).PumpPowerToHeat;
				Error = std::abs( ( TotalLoad - PrevTotalLoad ) / PrevTotalLoad );
				if ( Error < ErrorTol ) break;
			} //end iteration on pump energy convergence

			//   IF (Iter >=10 .AND. .NOT. WarmupFlag)THEN
			//     If( .not. WarmupFlag) Then
			//      Write(OutputFileDebug,707)Month, CurrentTime, Iter, TotalLoad, TotalPumpPower
			//     End If
			//707 format(' in iter loop at 707: ',1x,I2,1x,F5.2,1x,I5,7(F10.5,1x))
			//    END IF  !didn't converge
		} //(AtPartLoad)

		//If only loads are cases and walk-ins, that is, no air coils:
		//  Account for difference between load and capacity on secondary loop. Assume system able to provide
		//  rated capacity.  If it can't, that unmet energy will be stored and discharged at the system level.
		//  Meet current load to the extent possible.  If extra capacity available,
		//  apply it to previously unmet/stored loads.  If capacity less than current load,
		//  (e.g. as it may be following defrost cycles on cases or walk-ins served by secondary loop)
		//  save the unmet/stored load to be met in succeeding time steps.
		if ( Secondary( SecondaryNum ).NumCoils == 0 ) {
			StoredEnergyRate = max( 0.0, ( UnmetEnergy / TimeStepZone / SecInHour ) );
			LoadRequested = TotalLoad + StoredEnergyRate;
			if ( MaxLoad > LoadRequested ) {
				// Have at least as much capacity avail as needed, even counting stored energy
				TotalCoolingLoad = LoadRequested;
				RefrigerationLoad += StoredEnergyRate;
				UnmetEnergy = 0.0;
			} else {
				//Don't have as much capacity as needed (likely following defrost periods)
				TotalCoolingLoad = MaxLoad;
				RefrigerationLoad -= ( TotalLoad - MaxLoad );
				if ( ! WarmupFlag ) UnmetEnergy += ( ( TotalLoad - MaxLoad ) * TimeStepZoneSec );
			} // load requested greater than MaxLoad
			if ( Secondary( SecondaryNum ).UnmetEnergy > MyLargeNumber ) {
				Secondary( SecondaryNum ).UnmetEnergy = MyLargeNumber;
				if ( ShowUnmetSecondEnergyWarning( SecondaryNum ) ) {
					ShowWarningError( "Secondary Refrigeration Loop: " + Secondary( SecondaryNum ).Name );
					ShowContinueError( " This secondary system has insufficient capacity to meet the refrigeration loads." );
					ShowUnmetSecondEnergyWarning( SecondaryNum ) = false;
				}
			} //>my large number

			Secondary( SecondaryNum ).UnmetEnergy = UnmetEnergy;
		} else { // air coils on secondary loop, no "unmet" energy accounting, just reduce amount of cooling provided to zone by coils
			DeRate = false;
			if ( TotalLoad > MaxLoad ) DeRate = true;
			//  TotalLoad = RefrigerationLoad + DistPipeHeatGain  + ReceiverHeatGain &
			//           + TotalPumpPower*Secondary(SecondaryNum)%PumpPowertoHeat
			FinalRateCoils( DeRate, SecondarySystem, SecondaryNum, TotalLoad, MaxLoad ); //assign case credits for coils on this loop
			//Bug TotalCoolingLoad not set but used below
		} // no air coils on secondary loop
		Secondary( SecondaryNum ).PumpPowerTotal = TotalPumpPower;
		Secondary( SecondaryNum ).PumpElecEnergyTotal = TotalPumpPower * LocalTimeStep * SecInHour;
		Secondary( SecondaryNum ).TotalRefrigLoad = RefrigerationLoad;
		Secondary( SecondaryNum ).TotalRefrigEnergy = RefrigerationLoad * LocalTimeStep * SecInHour;
		Secondary( SecondaryNum ).TotalCoolingLoad = TotalCoolingLoad;
		Secondary( SecondaryNum ).TotalCoolingEnergy = TotalCoolingLoad * LocalTimeStep * SecInHour;
		Secondary( SecondaryNum ).FlowVolActual = VolFlowRate;
		Secondary( SecondaryNum ).HotDefrostCondCredit = TotalHotDefrostCondCredit;
		Secondary( SecondaryNum ).DistPipeHeatGain = DistPipeHeatGain;
		Secondary( SecondaryNum ).DistPipeHeatGainEnergy = DistPipeHeatGain * LocalTimeStep * SecInHour;
		Secondary( SecondaryNum ).ReceiverHeatGain = ReceiverHeatGain;
		Secondary( SecondaryNum ).ReceiverHeatGainEnergy = ReceiverHeatGain * LocalTimeStep * SecInHour;

	}

	void
	SumZoneImpacts()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL
		//       DATE WRITTEN   Spring 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the total impact of all refrigeration systems on each zone.

		// METHODOLOGY EMPLOYED:
		// Calculate the energy from refrigerated case credits arising from interaction between the zone and:
		//   refrigerated cases and walk-ins
		//   heat rejection from zone-located compressor-racks and zone-located air-cooled condensers
		//   heat absorbed by suction piping, secondary loop distribution piping, and
		//   secondary receiver shells

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int ZoneNum( 0 ); // used calculating total refrigeration interactions for zone

		if ( UseSysTimeStep ) { // air chillers
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				CoilSysCredit( ZoneNum ).ReportH20RemovedKgPerS_FromZoneRate = -CoilSysCredit( ZoneNum ).LatKgPerS_ToZoneRate;
				CoilSysCredit( ZoneNum ).ReportLatCreditToZoneRate = -CoilSysCredit( ZoneNum ).LatCreditToZoneRate;
				CoilSysCredit( ZoneNum ).ReportLatCreditToZoneEnergy = -CoilSysCredit( ZoneNum ).LatCreditToZoneEnergy;
				// Sensible rate can be positive or negative, split into separate output variables and
				//   always report positive value
				if ( CoilSysCredit( ZoneNum ).SenCreditToZoneRate <= 0.0 ) {
					CoilSysCredit( ZoneNum ).ReportSenCoolingToZoneRate = -CoilSysCredit( ZoneNum ).SenCreditToZoneRate;
					CoilSysCredit( ZoneNum ).ReportSenCoolingToZoneEnergy = -CoilSysCredit( ZoneNum ).SenCreditToZoneEnergy;
					CoilSysCredit( ZoneNum ).ReportHeatingToZoneRate = 0.0;
					CoilSysCredit( ZoneNum ).ReportHeatingToZoneEnergy = 0.0;
				} else {
					CoilSysCredit( ZoneNum ).ReportSenCoolingToZoneRate = 0.0;
					CoilSysCredit( ZoneNum ).ReportSenCoolingToZoneEnergy = 0.0;
					CoilSysCredit( ZoneNum ).ReportHeatingToZoneRate = CoilSysCredit( ZoneNum ).SenCreditToZoneRate;
					CoilSysCredit( ZoneNum ).ReportHeatingToZoneEnergy = -CoilSysCredit( ZoneNum ).SenCreditToZoneEnergy;
				}
				CoilSysCredit( ZoneNum ).ReportTotCoolingToZoneRate = CoilSysCredit( ZoneNum ).ReportLatCreditToZoneRate + CoilSysCredit( ZoneNum ).ReportSenCoolingToZoneRate;
				CoilSysCredit( ZoneNum ).ReportTotCoolingToZoneEnergy = CoilSysCredit( ZoneNum ).ReportLatCreditToZoneEnergy + CoilSysCredit( ZoneNum ).ReportSenCoolingToZoneEnergy;
			}
		} //UseSysTimeStep signals run for air chillers

		//Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .FALSE.
		if ( ( ! UseSysTimeStep ) && ( ( NumSimulationCases > 0 ) || ( NumSimulationWalkIns > 0 ) ) ) {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				CaseWIZoneReport( ZoneNum ).SenCaseCreditToZoneEnergy = RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone * TimeStepZoneSec;
				// Latent always negative
				CaseWIZoneReport( ZoneNum ).LatCoolingToZoneRate = -RefrigCaseCredit( ZoneNum ).LatCaseCreditToZone;
				CaseWIZoneReport( ZoneNum ).LatCoolingToZoneEnergy = CaseWIZoneReport( ZoneNum ).LatCoolingToZoneRate * TimeStepZoneSec;
				// Sensible rate can be positive or negative, split into separate output variables and
				//   always report positive value
				if ( RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone <= 0.0 ) {
					CaseWIZoneReport( ZoneNum ).SenCoolingToZoneRate = -RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone;
					CaseWIZoneReport( ZoneNum ).SenCoolingToZoneEnergy = -RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone * TimeStepZoneSec;
					CaseWIZoneReport( ZoneNum ).HeatingToZoneRate = 0.0;
					CaseWIZoneReport( ZoneNum ).HeatingToZoneEnergy = 0.0;
				} else {
					CaseWIZoneReport( ZoneNum ).SenCoolingToZoneRate = 0.0;
					CaseWIZoneReport( ZoneNum ).SenCoolingToZoneEnergy = 0.0;
					CaseWIZoneReport( ZoneNum ).HeatingToZoneRate = RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone;
					CaseWIZoneReport( ZoneNum ).HeatingToZoneEnergy = RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone * TimeStepZoneSec;
				}
				CaseWIZoneReport( ZoneNum ).TotCoolingToZoneRate = CaseWIZoneReport( ZoneNum ).SenCoolingToZoneRate + CaseWIZoneReport( ZoneNum ).LatCoolingToZoneRate;
				CaseWIZoneReport( ZoneNum ).TotCoolingToZoneEnergy = CaseWIZoneReport( ZoneNum ).SenCoolingToZoneEnergy + CaseWIZoneReport( ZoneNum ).LatCoolingToZoneEnergy;
				CaseWIZoneReport( ZoneNum ).TotHtXferToZoneRate = RefrigCaseCredit( ZoneNum ).SenCaseCreditToZone + RefrigCaseCredit( ZoneNum ).LatCaseCreditToZone;
				CaseWIZoneReport( ZoneNum ).TotHtXferToZoneEnergy = CaseWIZoneReport( ZoneNum ).TotHtXferToZoneRate * TimeStepZoneSec;
			} // over zones for cases and walkins
		}

	}

	void
	CheckRefrigerationInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Sep 2010 - mining function
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provides the structure to get Refrigeration input so that
		// it can be called from internally or outside the module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetRefrigerationInputFlag ) {

			GetRefrigerationInput();
			SetupReportInput();
			GetRefrigerationInputFlag = false;

			if ( ( ! HaveCasesOrWalkins ) && ( ! HaveChillers ) ) {
				ManageRefrigeration = false;
				return;
			}
			if ( ( ! HaveDetailedRefrig ) && ( ! HaveRefrigRacks ) && ( ! HaveDetailedTransRefrig ) ) {
				ManageRefrigeration = false;
				return;
			}
		} //GetRefrigerationInputFlag

	}

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimAirChillerSet(
		std::string const & AirChillerSetName,
		int const ZoneNum,
		bool const FirstHVACIteration,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided,
		int & AirChillerSetPtr // from ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL
		//       DATE WRITTEN   January 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Transfers the load requested from the zone to the refrigeration module.
		// The load is met, partially met, or not met in the call to the detailed system solution
		// METHODOLOGY EMPLOYED:
		// Called from Zone Equipment Manager.

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using InputProcessor::FindItemInList;
		using DataHeatBalFanSys::TempControlType;
		using DataHVACGlobals::SingleHeatingSetPoint;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ChillerSetID;
		Real64 RemainingOutputToCoolingSP; // Remaining requested load in zone

		CheckRefrigerationInput();

		// Find the correct Chiller set
		if ( AirChillerSetPtr == 0 ) {
			ChillerSetID = FindItemInList( AirChillerSetName, AirChillerSet );
			if ( ChillerSetID == 0 ) {
				ShowFatalError( "SimAirChillerSet: Unit not found=" + AirChillerSetName );
			} // chillersetid ==0 because not in list
			AirChillerSetPtr = ChillerSetID;
		} else { //airchllersetpointer passed in call to subroutine not ==0
			ChillerSetID = AirChillerSetPtr;
			if ( ChillerSetID > NumRefrigChillerSets || ChillerSetID < 1 ) {
				ShowFatalError( "SimAirChillerSet:  Invalid AirChillerSetPtr passed=" + TrimSigDigits( ChillerSetID ) + ", Number of Units=" + TrimSigDigits( NumRefrigChillerSets ) + ", Entered Unit name=" + AirChillerSetName );
			} //ChillerSetID makes no sense
			if ( CheckChillerSetName( ChillerSetID ) ) {
				if ( AirChillerSetName != AirChillerSet( ChillerSetID ).Name ) {
					ShowFatalError( "SimAirChillerSet:  Invalid AirChillerSetPtr passed=" + TrimSigDigits( ChillerSetID ) + ", Unit name=" + AirChillerSetName + ", stored Unit Name for that index=" + AirChillerSet( ChillerSetID ).Name );
				} //name not equal correct name
				CheckChillerSetName( ChillerSetID ) = false;
			} //CheckChillerSetName logical test
		} //(AirChillerSetPtr == 0 or else not == 0

		if ( FirstHVACIteration ) {
			for ( ChillerSetID = 1; ChillerSetID <= NumRefrigChillerSets; ++ChillerSetID ) { //bbb what point of do loop, only set one (airchillersetptr) to zero
				AirChillerSet( AirChillerSetPtr ).QZnReqSens = 0.0;
			}
		} //FirstHVACIteration

		RemainingOutputToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		//RemainingOutputToCoolingSP in Watts, < 0 for cooling demand

		if ( RemainingOutputToCoolingSP < 0.0 && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
			AirChillerSet( AirChillerSetPtr ).QZnReqSens = RemainingOutputToCoolingSP;
		} else {
			AirChillerSet( AirChillerSetPtr ).QZnReqSens = 0.0;
		}

		UseSysTimeStep = true;

		ManageRefrigeratedCaseRacks();

		UseSysTimeStep = false;

		// Return values to Zone Equipment Manager.
		LatOutputProvided = CoilSysCredit( ZoneNum ).LatKgPerS_ToZoneRate;
		SysOutputProvided = CoilSysCredit( ZoneNum ).SenCreditToZoneRate;

	}

	//***************************************************************************************************

	void
	CalculateAirChillerSets( int const AirChillerSetID )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL
		//       DATE WRITTEN   January 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Transfers the load requested from the zone to the refrigeration module.
		// The load is met, partially met, or not met in the next time step when the refrigerated case
		// module is called via case credits. Therefore, by definition, the sensible and latent
		// output provided are zero.
		// METHODOLOGY EMPLOYED:
		// Called from Zone Equipment Manager.
		//       have however done the variable definitions for in and out.

		// USE STATEMENTS:
		//unused  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
		//unused  USE InputProcessor,    ONLY: FindItemInList

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int CoilID( 0 ); // Index to coil
		static int CoilIndex( 0 ); // rank of coils within system
		static Real64 AirChillerSetSchedule( 0.0 ); // Schedule value for air chiller SET
		static Real64 QZNReqSens( 0.0 ); // Amount of sensible heat needed by the zone, NEGATIVE when cooling needed [W]
		static Real64 RemainQZNReqSens( 0.0 ); // Remaiing amount of sensible heat needed by the zone [W]

		//Note, all coils in a coil set are in the same zone
		// the coils may be served by different detailed systems
		// The coils are dispatched to meet the load specified in the previous time step in order listed in coilset object
		AirChillerSetSchedule = GetCurrentScheduleValue( AirChillerSet( AirChillerSetID ).SchedPtr );

		if ( AirChillerSetSchedule <= 0.0 ) return;
		QZNReqSens = AirChillerSet( AirChillerSetID ).QZnReqSens;
		RemainQZNReqSens = QZNReqSens;

		for ( CoilIndex = 1; CoilIndex <= AirChillerSet( AirChillerSetID ).NumCoils; ++CoilIndex ) {
			CoilID = AirChillerSet( AirChillerSetID ).CoilNum( CoilIndex );

			CalculateCoil( CoilID, RemainQZNReqSens );
			RemainQZNReqSens += WarehouseCoil( CoilID ).SensCreditRate;
			//should be a negative minus a negative, so a smaller negative, that is, going toward zero, but senscoolingenergyrate expressed as positive
			//Need to go over all the coils so that the defrosts occur on schedule, even when the chiller isn't called for at that particular time step
			//IF(RemainQZNReqSens >=0.0d0)EXIT  !shouldn't be > 0 because limited by request in calculatecoil
			if ( RemainQZNReqSens > 0.0 ) RemainQZNReqSens = 0.0;
		} // CoilIndex

	}

	//***************************************************************************************************

	void
	FinalRateCoils(
		bool const DeRate, // True if compressor rack or secondary ht exchanger unable to provide capacity
		int const SystemSourceType, // Secondarysystem or DetailedSystem
		int const SystemID, // ID for Secondary loop or detailed system calling for derate
		Real64 const InitialTotalLoad, // Load on system or secondary loop as initially calculated [W]
		Real64 const AvailableTotalLoad // Load that system or secondary loop is able to serve [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL
		//       DATE WRITTEN   January 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// When compressor system, or secondary loop capacity is insufficient to meet coil loads
		//   Come back here and derate the coil case credits to show unmet load impact
		//   Note that the coil fan, heater, and defrost would be unaffected because they
		//   would still be running at level calculated previously

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using CurveManager::CurveValue;
		using namespace DataLoopNode;
		using General::CreateSysTimeIntervalString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int NumCoils( 0 ); // Number of coils on this system or secondary loop
		static int CoilID( 0 ); // Index to coil
		static int CoilIndex( 0 ); // rank of coils within system
		static Real64 DeRateFactor( 0.0 ); // Ratio of energy available from system or secondary loop
		static Real64 InitLatCreditEnergy( 0.0 ); // Latent credit energy before derate [W]
		static Real64 InitKgFrost( 0.0 ); // Initial amount of frost on coils based on latent load before derate [kg]
		static Real64 FrostReduction( 0.0 ); // Change in frost on coils based on derated latent load [kg]

		{ auto const SELECT_CASE_var( SystemSourceType );
		if ( SELECT_CASE_var == DetailedSystem ) {
			NumCoils = System( SystemID ).NumCoils;
		} else if ( SELECT_CASE_var == SecondarySystem ) {
			NumCoils = Secondary( SystemID ).NumCoils;
		}} // DeRateCoils

		if ( DeRate ) {
			ShowRecurringWarningErrorAtEnd( "Refrigeration:System chilling WarehouseCoils " + System( SystemID ).Name + " - Refrigeration system unable to meet load of warehouse coils chilled by system ... continues by derating coil load", System( SystemID ).InsuffCapWarn );

			DeRateFactor = AvailableTotalLoad / InitialTotalLoad;
			Real64 const time_step_sec( TimeStepSys * SecInHour );
			for ( CoilIndex = 1; CoilIndex <= NumCoils; ++CoilIndex ) {
				CoilID = System( SystemID ).CoilNum( CoilIndex );
				auto & warehouse_coil( WarehouseCoil( CoilID ) );

				// need to adjust ice on coil due to reduction in latent load met by coil
				InitLatCreditEnergy = warehouse_coil.LatCreditEnergy;
				InitKgFrost = warehouse_coil.KgFrost;

				warehouse_coil.TotalCoolingLoad *= DeRateFactor;
				warehouse_coil.TotalCoolingEnergy *= DeRateFactor;
				warehouse_coil.SensCoolingEnergyRate *= DeRateFactor;
				warehouse_coil.SensCoolingEnergy *= DeRateFactor;
				warehouse_coil.LatCreditRate *= DeRateFactor;
				warehouse_coil.LatCreditEnergy *= DeRateFactor;
				warehouse_coil.LatKgPerS_ToZone *= DeRateFactor;
				warehouse_coil.SensCreditRate = warehouse_coil.SensCoolingEnergyRate - warehouse_coil.ElecFanPower - warehouse_coil.ElecHeaterPower - warehouse_coil.ThermalDefrostPower;
				warehouse_coil.SensCreditEnergy = warehouse_coil.SensCreditRate * time_step_sec;

				FrostReduction = ( InitLatCreditEnergy - warehouse_coil.LatCreditEnergy ) / IcetoVaporEnthalpy;
				warehouse_coil.KgFrost = max( 0.0, warehouse_coil.KgFrost - FrostReduction );

				if ( warehouse_coil.SensCreditRate >= 0.0 ) {
					warehouse_coil.ReportSensCoolCreditRate = warehouse_coil.SensCreditRate;
					warehouse_coil.ReportHeatingCreditRate = 0.0;
				} else {
					warehouse_coil.ReportSensCoolCreditRate = 0.0;
					warehouse_coil.ReportHeatingCreditRate = -warehouse_coil.SensCreditRate;
				}
				warehouse_coil.ReportSensCoolCreditEnergy = warehouse_coil.ReportSensCoolCreditRate * time_step_sec;
				warehouse_coil.ReportHeatingCreditEnergy = warehouse_coil.ReportHeatingCreditRate * time_step_sec;
				warehouse_coil.ReportTotalCoolCreditRate = warehouse_coil.ReportSensCoolCreditRate + warehouse_coil.LatCreditRate;
				warehouse_coil.ReportTotalCoolCreditEnergy = warehouse_coil.ReportSensCoolCreditEnergy + warehouse_coil.LatCreditEnergy;
			}
		} // DeRate == true

	}

	//***************************************************************************************************

	//***************************************************************************************************

	void
	CalculateCoil(
		int const CoilID,
		Real64 const QZnReq // sensible load required
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Therese Stovall, ORNL
		//       DATE WRITTEN   January 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates the refrigerated warehouse coil object.
		// Note QZnReq < 0 corresponds to cooling needed

		// METHODOLOGY EMPLOYED:
		// Called from Calculate Air Chiller Set.
		// Air chillers are used to model the type of equipment typically used in
		// refrigerated warehouses. For that reason, there is a major difference
		// between the air chiller model and those for refrigerated cases or walk-ins.
		// For cases and walk-ins, a portion of the model is directed toward
		// calculating the amount of refrigeration needed to maintain the refrigerated
		// volume at the desired temperature due to heat exchange with the surrounding
		// zone, and that zone is conditioned to a nearly constant temperature.
		// In a refrigerated warehouse, the refrigeration load is caused by heat exchange
		// with a variable external environment.  For that reason, the loads for these
		// zones are calculated by the usual EnergyPlus zone heat balance.
		// The amount of refrigeration needed to maintain the specified temperature
		// setpoints is then passed to the air chiller model, in a similar fashion
		// to the load passed to a window air conditioner model. The air chillers
		// are therefore solved using the system time step, not the zone time step
		// used for cases and walk-ins.
		// The air chiller performance is based on three types of manufacturers ratings,
		// Unit Load Factor, Total Capacity Map, or a set of European standards.
		// Correction factors for material and refrigerant are applied to all of these ratings.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using namespace DataLoopNode;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTsatFnHPb;
		using Psychrometrics::PsyWFnTdpPb;
		using Psychrometrics::PsyHFnTdbRhPb;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyTdpFnWPb;
		using Psychrometrics::PsyWFnTdbH;
		using Psychrometrics::PsyCpAirFnWTdb;
		using General::CreateSysTimeIntervalString;

		// Locals
		static Real64 UnitLoadFactorSens( 0.0 ); // Rated capacity divided by rated DT1 (T air in - Tevap) (W/delta C)

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//unused  REAL(r64), PARAMETER ::ErrorTol       = 0.001d0 !Iterative solution tolerance
		static std::string const TrackMessage( "from RefrigeratedCase:CalculateCoil" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//INTEGER      :: Iter                    =0   ! counter for loop to solve for total coil capacity as a function of inlet air conditions
		static int FanSpeedControlType( 0 ); // from input
		static int SHRCorrectionCurvePtr( 0 ); // Points to curve entered by user to specify total/sensible capacity as a function of SHR
		static int SHRCorrectionType( 0 ); // SHR60, QuadraticSHR, European, or TabularRH_DT1_TRoom
		static int ZoneNodeNum( 0 ); // Zone node number
		static Real64 AirVolRatio( 0.0 ); // used when operating at part load
		static Real64 AirVolumeFlowMax( 0.0 ); // Coil air flow limited by drip down schedule (m3/s)
		static Real64 AirVolumeFlowRated( 0.0 ); // Coil rated air flow (m3/s)
		static Real64 AvailDefrostEnergy( 0.0 ); // available to melt ice with temp term control (J)
		static Real64 CapFac( 0.0 ); // used to reduce fan power when don't need full coil capacity
		static Real64 CoilCapTotal( 0.0 ); // Sensible plus latent load (W)
		static Real64 CoilCapTotEstimate( 0.0 ); // Part of loop to solve for total coil capacity as a function of inlet air conditions (W)
		static Real64 CoilInletCp( 0.0 ); // Coil air inlet specific heat (J/kg-deltaC)
		static Real64 CoilInletDensity( 0.0 ); // Coil air inlet density (kg/m3)
		static Real64 CoilInletDryAirCp( 0.0 ); // Dry air specific heat at coil inlet temperature (J/kg-C)
		static Real64 CoilInletDryAirDensity( 0.0 ); // Dry Air density at coil inlet temperature (kg/m3)
		static Real64 CoilInletHumRatio( 0.0 ); // Coil air inlet humidity ratio (kg water/kg air)
		static Real64 CoilInletTemp( 0.0 ); // Inlet temperature of air to coil, not mixed zone temperature unless "middle" location selected (C)
		static Real64 CoilInletEnthalpy( 0.0 ); // Coil inlet air enthalpy (J/kg)
		static Real64 CoilInletRHFrac( 0.0 ); // Coil inlet air relative humidity expressed as a fraction (0 to 1)
		static Real64 CoilSchedule( 0.0 ); // Current value of Coil operating (availability) schedule
		static Real64 CoolingLoadNet( 0.0 ); // Cooling capacity of the coil minus fan, heater, and defrost loads (W)
		static Real64 DefrostCap( 0.0 ); // Design defrost capacity of Coil (W)
		static Real64 DefrostEnergy( 0.0 ); // (J)
		static Real64 DefEnergyFraction( 0.0 ); // dimensionless
		static Real64 DefrostLoad( 0.0 ); // Part of the defrost that is a heat load on the zone (W)
		static Real64 DefrostSchedule( 0.0 ); // Coil defrost schedule, between 0 and 1
		static Real64 DefrostDripDownSchedule( 0.0 ); // Coil drip-down schedule (allows coil to drain after defrost)
		static Real64 DefrostEnergyNeeded( 0.0 ); // Energy needed to melt all ice, used with temperature termination (J)
		static Real64 DefrostRateNeeded( 0.0 ); // Defrost load that actually goes to melting ice (W)
		static Real64 DryAirMassFlowMax( 0.0 ); // Rated volume flow rate times dry air density adjusted for schedules (kg/s)
		static Real64 DryAirMassFlowRated( 0.0 ); // Rated volume flow rate times dry air density
		//REAL(r64)    :: Error                   =0.0d0 ! Used in iterative solution for sensible heat ratio
		static Real64 ExitHumRatio( 0.0 ); // kg water/kg air
		static Real64 ExitTemperature( 0.0 ); // Air temperature leaving the coil (C)
		static Real64 ExitTemperatureEstimate( 0.0 ); // Estimated Air temperature leaving the coil (C)
		static Real64 ExitEnthalpy( 0.0 ); // Air enthalpy leaving the coil (J/kg)
		static Real64 ExitEnthalpyEstimate( 0.0 ); // Estimated Air enthalpy leaving the coil (J/kg)
		static Real64 FanMinAirFlowRatio( 0.0 ); // From input
		static Real64 FanPowerActual( 0.0 ); // (W)
		static Real64 FanPowerRated( 0.0 ); // (W)
		static Real64 FanPowerMax( 0.0 ); // Total fan energy rate, limited by dripdown period (W)
		static Real64 FanPowerRatio( 0.0 ); // Used for variable speed fans, dimensionless
		static Real64 FrostChangekg( 0.0 ); // Amount of frost added or melted  (kg)
		static Real64 HeaterSchedule( 0.0 ); // zero to one
		static Real64 HeaterLoad( 0.0 ); // Total heater (except defrost) energy rate (W)
		static Real64 IceSensHeatNeeded( 0.0 ); // Energy to raise frost temperature to 0C, used w/ temp termination (J)
		static Real64 LatLoadServed( 0.0 ); // Energy rate used to remove water from zone air (W)
		static Real64 MaxTemperatureDif( 0.0 ); // Used to limit capacity during initial pulldown (deltaC)
		static Real64 SensibleCapacityMax( 0.0 ); // Sensible capacity adjusted for any time in dripdown state (W)
		//REAL(r64)    :: SensibleLoad            =0.0d0 ! Sensible load provided by coil (W)
		static Real64 SensLoadRequested( 0.0 ); // Sensible load requested by zone balance (W)
		static Real64 SensLoadFromZone( 0.0 ); // Net sensible load removed from zone after accounting for heaters, fans, defrost [W]
		static Real64 SensLoadRequestedGross( 0.0 ); // Gross sensible load removed by coil
		static Real64 SensLoadGross( 0.0 ); // Sensible load met by coil (W)
		static Real64 SHR( 0.0 ); // Sensible heat ratio, sensible load/total load
		static Real64 SHRCorrection( 0.0 ); // Actual total/sensible load, NOT = Inverse SHR (unless coil efficiency = 1.0),
		// but function of SHR, which is why iteration needed
		static Real64 SHRCorrection60( 0.0 ); // Total capacity as a fraction of sensible capacity at a SHR of 0.6, entered by user
		static Real64 Slope( 0.0 ); // Part of linear SHR60 correction factor, dimensionless
		static Real64 StartIceTemp( 0.0 ); // Frost temperature at start of time step [C]
		static Real64 StartFrostKg( 0.0 ); // frost load at start of time step (kg of ice)
		static Real64 TemperatureDif( 0.0 ); // difference between inlet air and evaporating temperature (deltaC)
		static Real64 TEvap( 0.0 ); // Evaporating temperature in the coil (C)
		static Real64 WaterRemovRate( 0.0 ); // Walk in cooler removes water at this rate in this zone (kg/s)
		static Real64 Yint( 0.0 ); // Part of linear SHR60 correction factor, dimensionless
		static Real64 ZoneDryAirDensity( 0.0 ); // Dry air density at mixed zone conditions
		static Real64 ZoneMixedAirCp( 0.0 ); // J/kg-deltaC
		static Real64 ZoneMixedAirDensity( 0.0 ); // kg/m3
		static Real64 ZoneMixedAirDryBulb( 0.0 ); // (C)
		static Real64 ZoneMixedAirRHFrac( 0.0 ); // relative humidity of mixed air in the zone expressed as a fraction from 0 to 1
		static Real64 ZoneMixedAirEnthalpy( 0.0 ); // J/kg
		static Real64 ZoneMixedAirHumRatio( 0.0 ); // kg water/kg air in the zone mixed air

		// GET SCHEDULES
		auto & warehouse_coil( WarehouseCoil( CoilID ) );
		CoilSchedule = GetCurrentScheduleValue( warehouse_coil.SchedPtr );

		if ( CoilSchedule <= 0.0 ) return;

		DefrostSchedule = GetCurrentScheduleValue( warehouse_coil.DefrostSchedPtr );
		DefrostDripDownSchedule = GetCurrentScheduleValue( warehouse_coil.DefrostDripDownSchedPtr );
		//next statement In case user doesn't understand concept of drip down schedule
		DefrostDripDownSchedule = max( DefrostDripDownSchedule, DefrostSchedule );
		//next value optional, so set to default before checking for schedule
		HeaterSchedule = 1.0;
		if ( warehouse_coil.HeaterSchedPtr > 0 ) HeaterSchedule = GetCurrentScheduleValue( warehouse_coil.HeaterSchedPtr );

		AirVolRatio = 0.0;
		AirVolumeFlowMax = 0.0;
		CapFac = 0.0;
		CoilCapTotal = 0.0;
		CoilCapTotEstimate = 0.0;
		CoolingLoadNet = 0.0;
		DefrostLoad = 0.0;
		DryAirMassFlowMax = 0.0;
		ExitEnthalpyEstimate = 0.0;
		ExitEnthalpy = 0.0;
		ExitTemperature = 0.0;
		ExitHumRatio = 0.0;
		FanPowerActual = 0.0;
		HeaterLoad = 0.0;
		LatLoadServed = 0.0;
		FanPowerRatio = 0.0;
		FrostChangekg = 0.0;
		SensLoadFromZone = 0.0;
		SensLoadGross = 0.0;
		SensibleCapacityMax = 0.0;
		SHR = 0.0;
		WaterRemovRate = 0.0;

		//Set local subroutine variables for convenience
		ZoneNodeNum = warehouse_coil.ZoneNodeNum;
		AirVolumeFlowRated = warehouse_coil.RatedAirVolumeFlow;
		FanPowerRated = warehouse_coil.RatedFanPower;
		HeaterLoad = warehouse_coil.HeaterPower * HeaterSchedule;
		UnitLoadFactorSens = warehouse_coil.UnitLoadFactorSens;
		DefrostCap = warehouse_coil.DefrostCapacity;
		TEvap = warehouse_coil.TEvapDesign;
		SHRCorrectionType = warehouse_coil.SHRCorrectionType;
		SHRCorrection60 = warehouse_coil.SHRCorrection60;
		SHRCorrectionCurvePtr = warehouse_coil.SHRCorrectionCurvePtr;
		FanMinAirFlowRatio = warehouse_coil.FanMinAirFlowRatio;
		FanSpeedControlType = warehouse_coil.FanType;
		MaxTemperatureDif = warehouse_coil.MaxTemperatureDif;

		if ( DefrostDripDownSchedule == 1.0 ) {
			AirVolumeFlowMax = 0.0;
			DryAirMassFlowMax = 0.0;
		} else { // DefrostDripDownSchedule < 1.0d0, cooling will occur at least part of the time step
			SensLoadRequested = -QZnReq; //here let cooling demand be positive within subroutine
			if ( SensLoadRequested <= 0.0 ) { //No load so assume control keeps off, except that scheduled defrost still occurs
				AirVolumeFlowMax = 0.0;
				DryAirMassFlowMax = 0.0;
			} else {
				SensLoadRequestedGross = SensLoadRequested + HeaterLoad + FanPowerRated;
				ZoneMixedAirDryBulb = Node( ZoneNodeNum ).Temp;
				ZoneMixedAirHumRatio = Node( ZoneNodeNum ).HumRat;
				ZoneMixedAirRHFrac = PsyRhFnTdbWPb( ZoneMixedAirDryBulb, ZoneMixedAirHumRatio, OutBaroPress, TrackMessage );
				ZoneMixedAirEnthalpy = PsyHFnTdbRhPb( ZoneMixedAirDryBulb, ZoneMixedAirRHFrac, OutBaroPress, TrackMessage );
				ZoneMixedAirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ZoneMixedAirDryBulb, ZoneMixedAirHumRatio, TrackMessage );
				ZoneDryAirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ZoneMixedAirDryBulb, 0.0, TrackMessage );
				ZoneMixedAirCp = PsyCpAirFnWTdb( ZoneMixedAirHumRatio, ZoneMixedAirDryBulb );
				DryAirMassFlowRated = AirVolumeFlowRated * ZoneDryAirDensity;
				//calc t inlet to coil assuming at middle/mixed point in room  bbb -
				//    later need to do for hottest/coolest in room where Tin /= Tzonemixed
				//calc RH inlet to coil assuming at middle/mixed point in room
				//calc coilcap, sens and latent, available as f(inlet T,RH)
				switch( WarehouseCoil( CoilID ).VerticalLocation ) {
					case Floor:
						// purposely fall through
					case Ceiling:
						// purposely fall through
					case Middle:
						CoilInletTemp = ZoneMixedAirDryBulb;
						CoilInletEnthalpy = ZoneMixedAirEnthalpy;
						CoilInletRHFrac = ZoneMixedAirRHFrac;
						CoilInletDensity = ZoneMixedAirDensity;
						CoilInletCp = ZoneMixedAirCp;
						CoilInletHumRatio = ZoneMixedAirHumRatio;
						CoilInletDryAirDensity = ZoneDryAirDensity;
						CoilInletDryAirCp = PsyCpAirFnWTdb( 0.0, CoilInletTemp );
						break;
				}
				AirVolumeFlowMax = AirVolumeFlowRated * ( 1.0 - DefrostDripDownSchedule ) * CoilSchedule;
				DryAirMassFlowMax = DryAirMassFlowRated * ( 1.0 - DefrostDripDownSchedule ) * CoilSchedule;

			} //Sens load requested is non-zero
		} // DefrostDripDownSchedule == 1.0d0

		if ( AirVolumeFlowMax > 0.0 ) {

			TemperatureDif = min( MaxTemperatureDif, ( CoilInletTemp - TEvap ) );

			if ( warehouse_coil.RatingType == RatedCapacityTotal ) {
				// RatingType = CapacityTotalSpecificConditions, will be doing a table lookup
				//    based upon RHInlet, DT1, CoilInletTemperature - see excel files from B. Nelson, CoilCom
				//    In the table, X1== inlet air dry bulb temperature
				//                  X2== Difference between inlet T and evap T
				//                  X3== RH expressed as decimal
				CoilCapTotEstimate = CurveValue( SHRCorrectionCurvePtr, CoilInletTemp, TemperatureDif, CoilInletRHFrac ) * warehouse_coil.RatedCapTotal * ( 1.0 - DefrostDripDownSchedule ) * CoilSchedule;

			} else { //work with unit load factor (sensible only), function of DT1 (Tair in drybulb-Tevap)
				SensibleCapacityMax = warehouse_coil.UnitLoadFactorSens * TemperatureDif * ( 1.0 - DefrostDripDownSchedule ) * CoilSchedule;

				if ( SensibleCapacityMax > 0.0 ) {
					ExitTemperatureEstimate = CoilInletTemp - ( SensibleCapacityMax / ( DryAirMassFlowMax * CoilInletDryAirCp ) );
					if ( ExitTemperatureEstimate <= TEvap ) {
						ShowWarningError( TrackMessage + "Refrigeration:AirCoil: " + warehouse_coil.Name );
						ShowContinueError( " The estimated air outlet temperature is less than the evaporating temperature." );
					}
					ExitEnthalpyEstimate = PsyHFnTdbRhPb( ExitTemperatureEstimate, 1.0, OutBaroPress, TrackMessage );
					if ( ExitEnthalpyEstimate <= CoilInletEnthalpy ) {
						CoilCapTotEstimate = ( CoilInletEnthalpy - ExitEnthalpyEstimate ) * AirVolumeFlowMax * CoilInletDensity;
					} else {
						// Assume no water is extracted from flow
						ExitEnthalpyEstimate = PsyHFnTdbW( ExitTemperatureEstimate, CoilInletHumRatio );
						CoilCapTotEstimate = ( CoilInletEnthalpy - ExitEnthalpyEstimate ) * AirVolumeFlowMax * CoilInletDensity;
					}
					if ( SensibleCapacityMax > CoilCapTotEstimate ) SensibleCapacityMax = CoilCapTotEstimate;
					if ( std::abs( CoilCapTotEstimate ) > 0.0 ) {
						SHR = SensibleCapacityMax / ( CoilCapTotEstimate );
					} else {
						// will occur whenever defrost or dripdown
						SHR = 0.0;
					}

					{ auto const SELECT_CASE_var( SHRCorrectionType );
					if ( SELECT_CASE_var == SHR60 ) {
						//line from y = SHRCorrection60 value to 1. as x(SHR) goes from .6 to 1, from B. Nelson, ASHRAE August 2010
						Slope = ( SHRCorrection60 - 1.0 ) / ( 0.6 - 1.0 );
						Yint = SHRCorrection60 - ( Slope * 0.6 );
						SHRCorrection = Slope * SHR + Yint;
					} else if ( SELECT_CASE_var == QuadraticSHR ) {
						SHRCorrection = CurveValue( SHRCorrectionCurvePtr, SHR );
					} else if ( SELECT_CASE_var == European ) {
						//With European ratings, either start with rated total sensible capacity or rated total capacity
						//    If rated total capacity is used, 'get input'
						//    translated it to rated total sensible capacity using
						//    PARAMETER ::EuropeanWetCoilFactor = (/1.35D0, 1.15D0,  1.05D0,  1.01D0,   1.0D0/)
						//    That sensible capacity rating was then turned to a rated UnitLoadFactor using
						//    the rated temperature difference. That sensible rating was also corrected
						//    for refrigerant and fin material in 'get input' and is given as UnitLoadFactor
						//  The total (sens + latent) capacity is equal to that * DT1 * WetCoilFactor(TcoilIn)
						//    Sensible capacity max already has DT1, just need WetCoilFactor(TcoilIn)
						//PARAMETER ::EuropeanWetCoilFactor = (/1.35D0, 1.15D0,  1.05D0,  1.01D0,   1.0D0/)
						//PARAMETER ::EuropeanAirInletTemp  = (/10.0D0,  0.0D0, -18.0D0, -25.0D0, -34.0D0/)
						//PARAMETER ::EuropeanEvapTemp      = (/ 0.0D0, -8.0D0, -25.0D0, -31.0D0, -40.0D0/)
						//PARAMETER ::EuropeanDT1           = (/10.0D0,  8.0D0,   7.0D0,   7.0D0,   6.0D0/)
						if ( CoilInletTemp <= -25.0 ) {
							SHRCorrection = 1.0;
						} else if ( CoilInletTemp > -25.0 && CoilInletTemp <= 0.0 ) {
							SHRCorrection = ( EuropeanWetCoilFactor( 2 ) - EuropeanWetCoilFactor( 4 ) ) / ( EuropeanAirInletTemp( 2 ) - EuropeanAirInletTemp( 4 ) ) * ( EuropeanAirInletTemp( 2 ) - CoilInletTemp ) + EuropeanWetCoilFactor( 4 );
						} else if ( CoilInletTemp > 0.0 && CoilInletTemp <= 5.0 ) {
							SHRCorrection = ( EuropeanWetCoilFactor( 1 ) - EuropeanWetCoilFactor( 2 ) ) / ( EuropeanAirInletTemp( 1 ) - EuropeanAirInletTemp( 2 ) ) * ( EuropeanAirInletTemp( 1 ) - CoilInletTemp ) + EuropeanWetCoilFactor( 2 );
						} else if ( CoilInletTemp > 5.0 ) {
							SHRCorrection = EuropeanWetCoilFactor( 1 );
						} // calc correction as a function of coil inlet temperature
					}}
					CoilCapTotEstimate = SHRCorrection * SensibleCapacityMax;
				} else { // NOT (SensibleCapacityMax > 0.0d0)
					CoilCapTotEstimate = 0.0;
				} //  (SensibleCapacityMax > 0.0d0)
			} // Rating type : CapacityTotalSpecificConditions or Sensible Unit Load Factor

			if ( CoilCapTotEstimate > 0.0 ) {
				ExitEnthalpy = CoilInletEnthalpy - ( CoilCapTotEstimate / ( AirVolumeFlowMax * CoilInletDensity ) );
				ExitTemperature = PsyTsatFnHPb( ExitEnthalpy, OutBaroPress, TrackMessage ); //RH =1.0 at Tsat
				ExitHumRatio = PsyWFnTdbH( ExitTemperature, ExitEnthalpy, TrackMessage );
				if ( ExitHumRatio > CoilInletHumRatio ) ExitHumRatio = CoilInletHumRatio;
				WaterRemovRate = DryAirMassFlowMax * ( CoilInletHumRatio - ExitHumRatio );
				LatLoadServed = WaterRemovRate * IcetoVaporEnthalpy;
				SensLoadGross = CoilCapTotEstimate - LatLoadServed;
				FanPowerActual = FanPowerRated;
				if ( SensLoadGross < 0.0 ) {
					// Could rarely happen during initial cooldown of a warm environment
					SensLoadGross = 0.0;
					LatLoadServed = CoilCapTotEstimate;
					WaterRemovRate = LatLoadServed / IcetoVaporEnthalpy;
				} //SensLoadGross < 0
			} else { // NOT (SensibleCapacityMax > 0.0d0)
				WaterRemovRate = 0.0;
				LatLoadServed = 0.0;
				SensLoadGross = 0.0;
				FanPowerActual = 0.0;
			} //(CoilCapTotEstimate > 0.0d0)

			FanPowerMax = FanPowerRated * ( 1.0 - DefrostDripDownSchedule );
			if ( SensLoadGross > SensLoadRequestedGross ) { //part load operation
				//don't need full chiller power, reduce fan speed to reduce air flow
				// move fan to part power if need to
				CapFac = SensLoadRequestedGross / SensLoadGross;
				AirVolRatio = max( FanMinAirFlowRatio, std::pow( CapFac, EvaporatorAirVolExponent ) );
				//Fans limited by minimum air flow ratio

				{ auto const SELECT_CASE_var( FanSpeedControlType );
				if ( SELECT_CASE_var == FanVariableSpeed ) { //fan power law, adjusted for reality, applies
					FanPowerRatio = std::pow( AirVolRatio, 2.5 );
					FanPowerActual = FanPowerRatio * FanPowerMax;
				} else if ( SELECT_CASE_var == FanConstantSpeed ) {
					FanPowerActual = AirVolRatio * std::exp( 1.0 - AirVolRatio ) * FanPowerMax;
				} else if ( SELECT_CASE_var == FanConstantSpeedLinear ) { //e.g., on-off control
					FanPowerActual = AirVolRatio * FanPowerMax;
				} else if ( SELECT_CASE_var == FanTwoSpeed ) {
					//low speed setting of 1/2 fan speed can give up to 60% of capacity.
					//1/2 speed corresonds to ~1/8 power consumption (FanHalfSpeedRatio = 1/(2**2.5) = 0.1768)
					//dampers are used to control flow within those two ranges as in FanConstantSpeed
					if ( CapFac < CapFac60Percent ) {
						FanPowerActual = ( ( AirVolRatio + 0.4 ) * ( FanHalfSpeedRatio ) ) * std::exp( 1.0 - AirVolRatio ) * FanPowerMax;
					} else {
						FanPowerActual = AirVolRatio * std::exp( 1.0 - AirVolRatio ) * FanPowerMax;
					} //capfac60percent
				}} // fan speed control type

				//reduce latent capacity according to value called for for sensible  - recalc latent.
				//   recalc coilcaptotal
				WaterRemovRate *= AirVolRatio;
				LatLoadServed = WaterRemovRate * IcetoVaporEnthalpy;
				SensLoadGross = SensLoadRequestedGross;
			} else { // at full load
				FanPowerActual = FanPowerMax;
			} // part load and sensload served > 0.

			CoilCapTotal = SensLoadGross + LatLoadServed;
			if ( CoilCapTotal > 0.0 ) {
				SHR = SensLoadGross / CoilCapTotal;
			} else {
				SHR = 0.0;
			} //(CoilCapTotal > 0.0d0)

			//now handle ice on coil and defrost because defrost energy not into melting ice goes into sensible load
			// FROST:  keep track of frost build up on evaporator coil
			//         avoid accumulation during warm-up to avoid reverse dd test problem
			if ( ! WarmupFlag ) {
				FrostChangekg = ( WaterRemovRate * TimeStepSys * SecInHour );
				warehouse_coil.KgFrost += FrostChangekg;
			}

		} else { // NOT (AirVolumeFlowMax > 0.0d0)
			CoilCapTotEstimate = 0.0;
			WaterRemovRate = 0.0;
			LatLoadServed = 0.0;
			FrostChangekg = 0.0;
			SensLoadGross = 0.0;
			FanPowerActual = 0.0;
		} //(AirVolumeFlowMax > 0.0d0)

		//DEFROST CALCULATIONS   ***** need to reduce sensible heat to zone from
		//                     defrost by amount used to melt ice. Last two elements
		//                     in starting IF are there to mimic temperature override
		//                     on the coils that stops defrost if the coils get above
		//                     a certain temperature (such as when there's no load and no ice)
		if ( ( DefrostSchedule > 0.0 ) && ( warehouse_coil.DefrostType != DefrostNone ) && ( warehouse_coil.DefrostType != DefrostOffCycle ) ) {
			DefrostLoad = DefrostCap * DefrostSchedule; //W
			DefrostEnergy = DefrostLoad * TimeStepSys * SecInHour; //Joules
			StartFrostKg = warehouse_coil.KgFrost;

			if ( warehouse_coil.DefrostControlType == DefrostContTempTerm ) {
				//  Need to turn defrost system off early if controlled by temperature and all ice melted
				//  For temperature termination, need to recognize not all defrost heat goes to melt ice
				//  Some goes to misc losses (for fluid defrost, some coil areas bare earlier than
				//  others and xfer heat to environment)
				//  Assume full ice melting satisfies temperature control.
				//      (defaults for DefEnergyFraction are :=0.7 for elec, =0.3 for fluids)
				DefEnergyFraction = warehouse_coil.DefEnergyFraction;
				AvailDefrostEnergy = DefEnergyFraction * DefrostEnergy; //Joules avail to melt ice
				IceSensHeatNeeded = 0.0;
				if ( StartFrostKg > 0.0 ) {
					if ( warehouse_coil.IceTemp < 0.0 ) {
						StartIceTemp = warehouse_coil.IceTemp;
						IceSensHeatNeeded = StartFrostKg * SpecificHeatIce * ( 0.0 - StartIceTemp ); //Joules
						if ( AvailDefrostEnergy >= IceSensHeatNeeded ) {
							warehouse_coil.IceTemp = 0.0;
							AvailDefrostEnergy -= IceSensHeatNeeded; //Joules
						} else { //DefrostEnergy < IceSensHeatNeeded
							warehouse_coil.IceTemp = StartIceTemp + AvailDefrostEnergy / ( SpecificHeatIce * StartFrostKg );
							AvailDefrostEnergy = 0.0;
						} // AvailDefrostEnergy >= IceSensHeatNeeded
					} // IceTemp < 0,  need to raise temperature of ice
					//Reduce defrost heat load on walkin by amount of ice melted during time step
					FrostChangekg = min( AvailDefrostEnergy / IceMeltEnthalpy, StartFrostKg );
					if ( FrostChangekg < StartFrostKg ) {
						DefrostLoad -= FrostChangekg * IceMeltEnthalpy / TimeStepSys / SecInHour;
						if ( ! WarmupFlag ) warehouse_coil.KgFrost = StartFrostKg - FrostChangekg;
						//DefrostSchedule not changed because ice not all melted, temp term not triggered
					} else { // all frost melted during time step, so need to terminate defrost
						//  see Aug 8 2010 page 3 notes
						warehouse_coil.KgFrost = 0.0;
						DefrostEnergyNeeded = ( IceSensHeatNeeded + ( FrostChangekg * IceMeltEnthalpy ) ) / DefEnergyFraction; //Joules - energy needed including E unavail to melt ice
						DefrostSchedule = min( DefrostSchedule, ( DefrostEnergyNeeded / ( DefrostCap * TimeStepSys * SecInHour ) ) );
						// reduce heat load on warehouse by energy put into ice melting
						DefrostRateNeeded = ( IceSensHeatNeeded + ( FrostChangekg * IceMeltEnthalpy ) ) / ( TimeStepSys * SecInHour );
						DefrostLoad = max( 0.0, ( DefrostSchedule * DefrostCap - DefrostRateNeeded ) );
						warehouse_coil.IceTemp = warehouse_coil.TEvapDesign;
					} // frost melted during time step less than amount of ice at start
				} else {
					// no frost present so terminate defrost and reset ice temperature for start of next defrost
					// However, dripdown schedule still prevents/limits cooling capacity during time step
					DefrostLoad = 0.0;
					DefrostSchedule = 0.0;
					warehouse_coil.IceTemp = warehouse_coil.TEvapDesign;
				} // have frost present

			} else {
				//Not temperature control type, controlled only by schedule
				//Reduce defrost heat load on the zone by amount of ice melted during time step
				//But DefrostSchedule not changed
				FrostChangekg = max( 0.0, min( ( DefrostEnergy / IceMeltEnthalpy ), StartFrostKg ) );
				DefrostLoad -= FrostChangekg * IceMeltEnthalpy / TimeStepSys / SecInHour;
				if ( ! WarmupFlag ) warehouse_coil.KgFrost = StartFrostKg - FrostChangekg;
			} //Temperature termination vs. time-clock control type

		} else { //DefrostSchedule <= 0 or have None or OffCycle
			DefrostLoad = 0.0;
		} //Defrost calculations

		SensLoadFromZone = SensLoadGross - HeaterLoad - DefrostLoad - FanPowerActual;
		CoolingLoadNet = SensLoadFromZone + LatLoadServed;

		// ReportWarehouseCoil(CoilID)
		warehouse_coil.ThermalDefrostPower = DefrostLoad;
		if ( warehouse_coil.DefrostType == DefrostElec ) {
			warehouse_coil.ElecDefrostConsumption = DefrostCap * DefrostSchedule * TimeStepSys * SecInHour;
			warehouse_coil.ElecDefrostPower = DefrostCap * DefrostSchedule;
		} else {
			warehouse_coil.ElecDefrostConsumption = 0.0;
			warehouse_coil.ElecDefrostPower = 0.0;
		}

		// If hot brine or hot gas is used for defrost, need to reduce condenser load by heat reclaimed for defrost
		if ( warehouse_coil.DefrostType == DefrostFluid ) warehouse_coil.HotDefrostCondCredit = DefrostCap * DefrostSchedule;
		// LatentLoadServed is positive for latent heat removed from zone
		// SensLoadFromZone positive for heat REMOVED from zone, switch when do credit to zone
		warehouse_coil.SensCreditRate = SensLoadFromZone;
		warehouse_coil.SensCreditEnergy = SensLoadFromZone * TimeStepSys * SecInHour;
		warehouse_coil.LatCreditRate = LatLoadServed;
		warehouse_coil.LatCreditEnergy = LatLoadServed * TimeStepSys * SecInHour;
		warehouse_coil.LatKgPerS_ToZone = WaterRemovRate;
		warehouse_coil.TotalCoolingLoad = CoilCapTotal;
		warehouse_coil.TotalCoolingEnergy = CoilCapTotal * TimeStepSys * SecInHour;
		warehouse_coil.SensCoolingEnergyRate = SensLoadGross;
		warehouse_coil.SensCoolingEnergy = SensLoadGross * TimeStepSys * SecInHour;
		warehouse_coil.SensHeatRatio = SHR;
		warehouse_coil.ElecFanPower = FanPowerActual;
		warehouse_coil.ElecFanConsumption = FanPowerActual * TimeStepSys * SecInHour;
		warehouse_coil.ElecHeaterPower = HeaterLoad;
		warehouse_coil.ElecHeaterConsumption = HeaterLoad * TimeStepSys * SecInHour;

		warehouse_coil.TotalElecPower = FanPowerActual + HeaterLoad + warehouse_coil.ElecDefrostPower;
		warehouse_coil.TotalElecConsumption = warehouse_coil.TotalElecPower * TimeStepSys * SecInHour;

		if ( warehouse_coil.SensCreditRate >= 0.0 ) {
			warehouse_coil.ReportSensCoolCreditRate = warehouse_coil.SensCreditRate;
			warehouse_coil.ReportHeatingCreditRate = 0.0;
		} else {
			warehouse_coil.ReportSensCoolCreditRate = 0.0;
			warehouse_coil.ReportHeatingCreditRate = -warehouse_coil.SensCreditRate;
		}
		warehouse_coil.ReportSensCoolCreditEnergy = warehouse_coil.ReportSensCoolCreditRate * TimeStepSys * SecInHour;
		warehouse_coil.ReportHeatingCreditEnergy = warehouse_coil.ReportHeatingCreditRate * TimeStepSys * SecInHour;
		warehouse_coil.ReportTotalCoolCreditRate = warehouse_coil.ReportSensCoolCreditRate + warehouse_coil.LatCreditRate;
		warehouse_coil.ReportTotalCoolCreditEnergy = warehouse_coil.ReportSensCoolCreditEnergy + warehouse_coil.LatCreditEnergy;

		//**************************************************************************************************
		// Cap Kg Frost to avoid floating overflow errors
		// 1-time warning is issued. It should be rare but could happen with unrealistic inputs.

		if ( warehouse_coil.KgFrost > MyLargeNumber ) {
			warehouse_coil.KgFrost = MyLargeNumber;
			if ( ShowCoilFrostWarning( CoilID ) ) {
				ShowWarningError( "Refrigeration:AirCoil: " + warehouse_coil.Name );
				ShowContinueError( " This refrigerated air coil has insufficient defrost capacity to remove the excess frost accumulation." );
				ShowContinueError( " Check the defrost schedule or defrost capacity. " );
				ShowContinueErrorTimeStamp( "... Occurrence info" );
				ShowCoilFrostWarning( CoilID ) = false;
			}
		}

	}

	//***************************************************************************************************

	void
	FigureRefrigerationZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// initialize zone gain terms at begin environment

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyEnvrnFlag( true );
		int loop;

		CheckRefrigerationInput();

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {

			if ( NumRefrigSystems > 0 ) {
				for ( auto & e : System ) {
					e.PipeHeatLoad = 0.0;
					e.NetHeatRejectLoad = 0.0;
				}
			}

			if ( NumTransRefrigSystems > 0 ) {
				for ( auto & e : TransSystem ) {
					e.PipeHeatLoadMT = 0.0;
					e.PipeHeatLoadLT = 0.0;
					e.NetHeatRejectLoad = 0.0;
				}
			}

			if ( NumRefrigeratedRacks > 0 ) {
				for ( auto & e : RefrigRack ) {
					e.SensZoneCreditHeatRate = 0.0;
					e.SensHVACCreditHeatRate = 0.0;
				}
			}

			if ( NumSimulationSecondarySystems > 0 ) {
				for ( auto & e : Secondary ) {
					e.DistPipeZoneHeatGain = 0.0;
					e.ReceiverZoneHeatGain = 0.0;
				}
			}

			if ( NumSimulationWalkIns > 0 ) {
				for ( loop = 1; loop <= NumSimulationWalkIns; ++loop ) {
					WalkIn( loop ).SensZoneCreditRate = 0.0;
					WalkIn( loop ).LatZoneCreditRate = 0.0;
				}
			}
			if ( NumSimulationCases > 0 ) {
				for ( auto & e : RefrigCase ) {
					e.SensZoneCreditRate = 0.0;
					e.SensHVACCreditRate = 0.0;
					e.LatZoneCreditRate = 0.0;
					e.LatHVACCreditRate = 0.0;
				}
			}
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		return;

		// should model above terms for use during sizing here
		//  IF(DoingSizing)THEN

		//  ENDIF

	}

	//***************************************************************************************************

	void
	ZeroHVACValues()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         T. Stovall
		//       DATE WRITTEN   Aug 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reset all values that communicate outside module for HVAC steps
		// to zero when called on zone timestep. Otherwise, values may be held over when
		// no HVAC load calls module during that zone time step.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataWater::WaterStorage;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static int DemandARRID( 0 ); // Index to water tank Demand used for evap condenser
		static int TankID( 0 ); // Index to water tank used for evap condenser
		static int RackNum( 0 ); // Index to refrigerated rack
		static int CondID( 0 ); // Index to condenser
		static int PlantInletNode( 0 ); // Used to zero request for cooling water for condenser
		static int PlantOutletNode( 0 ); // Used to zero request for cooling water for condenser
		static int PlantLoopIndex( 0 ); // Used to zero request for cooling water for condenser
		static int PlantLoopSideIndex( 0 ); // Used to zero request for cooling water for condenser
		static int PlantBranchIndex( 0 ); // Used to zero request for cooling water for condenser
		static int PlantCompIndex( 0 ); // Used to zero request for cooling water for condenser
		static Real64 MassFlowRate( 0.0 ); // Used to zero request for cooling water for condenser

		if ( HaveRefrigRacks ) {
			//HaveRefrigRacks is TRUE when NumRefrigeratedRAcks > 0
			//RefrigRack ALLOCATED to NumRefrigeratedRacks
			for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeWater ) {
					PlantInletNode = RefrigRack( RackNum ).InletNode;
					PlantOutletNode = RefrigRack( RackNum ).OutletNode;
					PlantLoopIndex = RefrigRack( RackNum ).PlantLoopNum;
					PlantLoopSideIndex = RefrigRack( RackNum ).PlantLoopSideNum;
					PlantBranchIndex = RefrigRack( RackNum ).PlantBranchNum;
					PlantCompIndex = RefrigRack( RackNum ).PlantCompNum;
					MassFlowRate = 0.0;
					SetComponentFlowRate( MassFlowRate, PlantInletNode, PlantOutletNode, PlantLoopIndex, PlantLoopSideIndex, PlantBranchIndex, PlantCompIndex );
				}
				if ( RefrigRack( RackNum ).CondenserType == RefrigCondenserTypeEvap ) {
					if ( RefrigRack( RackNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
						DemandARRID = RefrigRack( RackNum ).EvapWaterTankDemandARRID;
						TankID = RefrigRack( RackNum ).EvapWaterSupTankID;
						WaterStorage( TankID ).VdotRequestDemand( DemandARRID ) = 0.0;
					}
				}
			} // RackNum
		} //HaveRefrigRacks

		if ( NumRefrigCondensers > 0 ) {
			//Condenser ALLOCATED to NumRefrigCondensers
			for ( CondID = 1; CondID <= NumRefrigCondensers; ++CondID ) {
				if ( Condenser( CondID ).CondenserType == RefrigCondenserTypeWater ) {
					PlantInletNode = Condenser( CondID ).InletNode;
					PlantOutletNode = Condenser( CondID ).OutletNode;
					PlantLoopIndex = Condenser( CondID ).PlantLoopNum;
					PlantLoopSideIndex = Condenser( CondID ).PlantLoopSideNum;
					PlantBranchIndex = Condenser( CondID ).PlantBranchNum;
					PlantCompIndex = Condenser( CondID ).PlantCompNum;
					MassFlowRate = 0.0;
					SetComponentFlowRate( MassFlowRate, PlantInletNode, PlantOutletNode, PlantLoopIndex, PlantLoopSideIndex, PlantBranchIndex, PlantCompIndex );
				}
				if ( Condenser( CondID ).CondenserType == RefrigCondenserTypeEvap ) {
					if ( Condenser( CondID ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
						DemandARRID = Condenser( CondID ).EvapWaterTankDemandARRID;
						TankID = Condenser( CondID ).EvapWaterSupTankID;
						WaterStorage( TankID ).VdotRequestDemand( DemandARRID ) = 0.0;
					}
				}
			} // ICond
		} // NumRefrigCondensers>0

	}

} // RefrigeratedCase

} // EnergyPlus
