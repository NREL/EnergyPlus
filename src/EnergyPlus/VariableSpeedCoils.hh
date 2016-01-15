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

#ifndef VariableSpeedCoils_hh_INCLUDED
#define VariableSpeedCoils_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <VariableSpeedCoils.hh>
#include <DataSizing.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

namespace VariableSpeedCoils {

	// Using/Aliasing
	using namespace DataHVACGlobals;
	using DataSizing::AutoSize;

	// Data
	//MODULE PARAMETER DEFINITIONS

	extern Real64 const RatedInletAirTemp; // 26.6667C or 80F
	extern Real64 const RatedInletWetBulbTemp; // 19.44 or 67F, cooling mode
	extern Real64 const RatedInletAirHumRat; // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
	extern Real64 const RatedInletWaterTemp; // 85 F cooling mode
	extern Real64 const RatedAmbAirTemp; // 95 F cooling mode
	extern Real64 const RatedInletAirTempHeat; // 21.11C or 70F, heating mode
	extern Real64 const RatedInletWaterTempHeat; // 21.11C or 70F, heating mode
	extern Real64 const RatedAmbAirTempHeat; // 8.33 or 47F, heating mode
	extern Real64 const RatedAmbAirWBHeat; // 8.33 or 43F, heating mode, rated wet bulb temperature

	// Airflow per total capacity range
	extern Real64 const MaxRatedVolFlowPerRatedTotCap; // m3/s per watt = 450 cfm/ton
	extern Real64 const MinRatedVolFlowPerRatedTotCap; // m3/s per watt = 300 cfm/ton
	extern Real64 const MaxHeatVolFlowPerRatedTotCap; // m3/s per watt = 600 cfm/ton
	extern Real64 const MaxCoolVolFlowPerRatedTotCap; // m3/s per watt = 500 cfm/ton
	extern Real64 const MinOperVolFlowPerRatedTotCap; // m3/s per watt = 200 cfm/ton

	//Water Systems
	extern int const CondensateDiscarded; // default mode where water is "lost"
	extern int const CondensateToTank; // collect coil condensate from air and store in water storage tank

	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;

	// Curve Types
	extern int const Linear;
	extern int const BiLinear;
	extern int const Quadratic;
	extern int const BiQuadratic;
	extern int const Cubic;

	// Defrost strategy (heat pump only)
	extern int const ReverseCycle; // uses reverse cycle defrost strategy
	extern int const Resistive; // uses electric resistance heater for defrost
	// Defrost control  (heat pump only)
	extern int const Timed; // defrost cycle is timed
	extern int const OnDemand; // defrost cycle occurs only when required

	extern int const MaxSpedLevels; // Maximum number of speed that supports

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// Identifier is VarSpeedCoil
	extern int NumWatertoAirHPs; // The Number of Water to Air Heat Pumps found in the Input

	extern bool GetCoilsInputFlag; // Flag set to make sure you get input once
	// LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag

	extern Real64 SourceSideMassFlowRate; // Source Side Mass flow rate [Kg/s]
	extern Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
	extern Real64 SourceSideInletEnth; // Source Side Inlet Enthalpy [J/kg]
	extern Real64 LoadSideMassFlowRate; // Load Side Mass flow rate [Kg/s]
	extern Real64 LoadSideInletDBTemp; // Load Side Inlet Dry Bulb Temp [C]
	extern Real64 LoadSideInletWBTemp; // Load Side Inlet Wet Bulb Temp [C]
	extern Real64 LoadSideInletHumRat; // Load Side Outlet Humidity ratio
	extern Real64 LoadSideInletEnth; // Load Side Inlet Enthalpy [J/kg]
	extern Real64 LoadSideOutletDBTemp; // Load Side Outlet Dry Bulb Temp [C]
	extern Real64 LoadSideOutletHumRat; // Load Side Outlet Humidity ratio
	extern Real64 LoadSideOutletEnth; // Load Side Outlet Enthalpy [J/kg]
	extern Real64 QSensible; // Load side sensible heat transfer rate [W]
	extern Real64 QLoadTotal; // Load side total heat transfer rate [W]
	extern Real64 QLatRated; // Latent Capacity [W] rated at entering air conditions [Tdb=26.7C Twb=19.4C]
	extern Real64 QLatActual; // Actual Latent Capacity [W]
	extern Real64 QSource; // Source side heat transfer rate [W]
	extern Real64 Winput; // Power Consumption [W]
	extern Real64 PLRCorrLoadSideMdot; // Load Side Mdot corrected for Part Load Ratio of the unit

	extern Real64 VSHPWHHeatingCapacity; // Used by Heat Pump:Water Heater object as total water heating capacity [W]
	extern Real64 VSHPWHHeatingCOP; // Used by Heat Pump:Water Heater object as water heating COP [W/W]

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Update routine

	// Utility routines
	//SHR, bypass factor routines

	// Types

	struct VariableSpeedCoilData // variable speed coil
	{
		// Members
		std::string Name; // Name of the  Coil
		std::string VarSpeedCoilType; // type of coil
		int NumOfSpeeds; // Number of speeds
		int NormSpedLevel; // Nominal speed level
		Real64 RatedWaterVolFlowRate; // Rated/Ref Water Volumetric Flow Rate [m3/s]
		Real64 RatedWaterMassFlowRate; // Rated/Ref Water Volumetric Flow Rate [m3/s]
		Real64 RatedAirVolFlowRate; // Rated/Ref Air Volumetric Flow Rate [m3/s]
		Real64 RatedCapHeat; // Rated/Ref Heating Capacity [W]
		Real64 RatedCapCoolTotal; // Rated/Ref Total Cooling Capacity [W]
		Real64 MaxONOFFCyclesperHour; // Maximum ON/OFF cycles per hour for the compressor (cycles/hour)
		Real64 Twet_Rated; // Nominal time for condensate to begin leaving the coil's
		// condensate drain line (sec)
		Real64 Gamma_Rated; // Initial moisture evaporation rate divided by steady-state
		// AC latent capacity (dimensionless)
		int HOTGASREHEATFLG; // whether to use hot gas reheat
		Real64 HPTimeConstant; // Heat pump time constant [s]
		int PLFFPLR; // index of part load curve as a function of part load ratio
		std::string CoolHeatType; // Type of WatertoAirHP ie. Heating or Cooling
		int VSCoilTypeOfNum; // type of component in plant
		bool SimFlag; // Heat Pump Simulation Flag
		Real64 DesignWaterMassFlowRate; // design water mass flow rate [kg/s]
		Real64 DesignWaterVolFlowRate; // design water volumetric flow rate [m3/s]
		Real64 DesignAirMassFlowRate; // Design Air Mass Flow Rate [kg/s]
		Real64 DesignAirVolFlowRate; // Design Air Volumetric Flow Rate [m3/s]
		Real64 AirVolFlowRate; // Air Volumetric Flow Rate[m3/s], real time
		Real64 AirMassFlowRate; // Air Mass Flow Rate[kg/s], real time
		Real64 InletAirPressure; // air inlet pressure [pa]
		Real64 InletAirDBTemp; // Inlet Air Dry Bulb Temperature [C], real time
		Real64 InletAirHumRat; // Inlet Air Humidity Ratio [kg/kg], real time
		Real64 InletAirEnthalpy; // Inlet Air Enthalpy [J/kg], real time
		Real64 OutletAirDBTemp; // Outlet Air Dry Bulb Temperature [C], real time
		Real64 OutletAirHumRat; // Outlet Air Humidity Ratio [kg/kg], real time
		Real64 OutletAirEnthalpy; // Outlet Air Enthalpy [J/kg], real time
		Real64 WaterVolFlowRate; // Water Volumetric Flow Rate [m3/s], real time
		Real64 WaterMassFlowRate; // Water Mass Flow Rate [kg/s], real time
		Real64 InletWaterTemp; // Inlet Water Temperature [C]
		Real64 InletWaterEnthalpy; // Inlet Water Enthalpy [J/kg]
		Real64 OutletWaterTemp; // Outlet Water Temperature [C]
		Real64 OutletWaterEnthalpy; // Outlet Water Enthalpy [J/kg]
		Real64 Power; // Power Consumption [W]
		Real64 QLoadTotal; // Load Side Total Heat Transfer Rate [W]
		Real64 QSensible; // Sensible Load Side Heat Transfer Rate [W]
		Real64 QLatent; // Latent Load Side Heat Transfer Rate [W]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 QWasteHeat; // Recoverable waste Heat Transfer Rate [W]
		Real64 Energy; // Energy Consumption [J]
		Real64 EnergyLoadTotal; // Load Side Total Heat Transferred [J]
		Real64 EnergySensible; // Sensible Load Side Heat Transferred [J]
		Real64 EnergyLatent; // Latent Load Side Heat Transferred [J]
		Real64 EnergySource; // Source Side Heat Transferred [J]
		Real64 COP; // Heat Pump Coefficient of Performance [-]
		Real64 RunFrac; // Duty Factor
		Real64 PartLoadRatio; // Part Load Ratio
		Real64 RatedPowerHeat; // Rated/Ref Heating Power Consumption[W]
		Real64 RatedCOPHeat; // Rated/Ref Heating COP [W/W]
		Real64 RatedCapCoolSens; // Rated/Ref Sensible Cooling Capacity [W]
		Real64 RatedPowerCool; // Rated/Ref Cooling Power Consumption[W]
		Real64 RatedCOPCool; // Rated/Ref Cooling COP [W/W]
		int AirInletNodeNum; // Node Number of the Air Inlet
		int AirOutletNodeNum; // Node Number of the Air Outlet
		int WaterInletNodeNum; // Node Number of the Water Onlet
		int WaterOutletNodeNum; // Node Number of the Water Outlet
		int LoopNum; // plant loop index for water side
		int LoopSide; // plant loop side index
		int BranchNum; // plant branch index
		int CompNum; // plant component index
		// set by parent object and "pushed" to this structure in SetVSWSHPData subroutine
		bool FindCompanionUpStreamCoil; // Flag to get the companion coil in Init
		bool IsDXCoilInZone; // true means dx coil is in zone instead of outside
		int CompanionCoolingCoilNum; // Heating coil companion cooling coil index
		int CompanionHeatingCoilNum; // Cooling coil companion heating coil index
		Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to
		// beginning for multispeed coil type
		Array1D_int MSErrIndex; // index flag for num speeds/recurring messages
		Array1D< Real64 > MSRatedPercentTotCap; // Percentage to the total cooling capacity for MS heat pump at the highest speed [dimensionless]
		Array1D< Real64 > MSRatedTotCap; // Rated cooling capacity for MS heat pump [W]
		Array1D< Real64 > MSRatedSHR; // Rated SHR for MS heat pump [dimensionless]
		Array1D< Real64 > MSRatedCOP; // Rated COP for MS heat pump [dimensionless]
		Array1D< Real64 > MSRatedAirVolFlowPerRatedTotCap;
		// Rated Air volume flow rate per total capacity through unit at rated conditions [m^3/w]
		Array1D< Real64 > MSRatedAirVolFlowRate;
		// Air volume flow rate through unit at rated conditions [m3/s]
		Array1D< Real64 > MSRatedAirMassFlowRate;
		// Air mass flow rate through unit at rated conditions [kg/s]
		Array1D< Real64 > MSRatedWaterVolFlowPerRatedTotCap;
		// Rated water volume flow rate per total  capacity through unit at rated conditions [m^3/w]
		Array1D< Real64 > MSRatedWaterVolFlowRate;
		// Water volume flow rate through unit at rated conditions [m3/s]
		Array1D< Real64 > MSRatedWaterMassFlowRate;
		// Water mass flow rate through unit at rated conditions [kg/s]
		Array1D< Real64 > MSRatedCBF;
		// rated coil bypass factor
		Array1D< Real64 > MSEffectiveAo;
		// effective heat transfer surface at each speed
		Array1D_int MSCCapFTemp;
		// index of total capacity modifier curve
		Array1D_int MSCCapAirFFlow;
		// index of total capacity modifier curve as a function of air flow
		Array1D_int MSCCapWaterFFlow;
		// index of total capacity modifier curve as a function of water flow
		Array1D_int MSEIRFTemp;
		// index of energy input ratio modifier curve as a function of temperature
		Array1D_int MSEIRAirFFlow;
		// index of energy input ratio modifier curve as a function of air flow fraction
		Array1D_int MSEIRWaterFFlow;
		// index of energy input ratio modifier curve as a function of water flow fraction
		Array1D_int MSWasteHeat;
		// index of waste heat as a function of temperature
		Array1D< Real64 > MSWasteHeatFrac;
		// water heating coil pump power at various speeds
		Array1D< Real64 > MSWHPumpPower;
		Array1D< Real64 > MSWHPumpPowerPerRatedTotCap;
		// Waste heat fraction
		Real64 SpeedNumReport;
		//speed number for output
		Real64 SpeedRatioReport;
		//speed ratio for output between two neighboring speeds
		// End of multispeed water source coil input
		//----------------------------------------------------------------
		//added variables and arrays for variable speed air-source heat pump
		//defrosting
		int DefrostStrategy; // defrost strategy; 1=reverse-cycle, 2=resistive
		int DefrostControl; // defrost control; 1=timed, 2=on-demand
		int EIRFPLR; // index of energy input ratio vs part-load ratio curve
		int DefrostEIRFT; // index of defrost mode total cooling capacity for reverse cycle heat pump
		Real64 MinOATCompressor; // Minimum OAT for heat pump compressor operation
		Real64 OATempCompressorOn; // The outdoor tempearture when the compressor is automatically turned back on,
		// if applicable, following automatic shut off. This field is used only for
		// HSPF calculation.
		Real64 MaxOATDefrost; // Maximum OAT for defrost operation
		Real64 DefrostTime; // Defrost time period in hours
		Real64 DefrostCapacity; // Resistive defrost to nominal capacity (at 21.11C/8.33C) ratio
		Real64 HPCompressorRuntime; // keep track of compressor runtime
		Real64 HPCompressorRuntimeLast; // keep track of last time step compressor runtime (if simulation downshifts)
		Real64 TimeLeftToDefrost; // keep track of time left to defrost heat pump
		Real64 DefrostPower; // power used during defrost
		Real64 DefrostConsumption; // energy used during defrost
		//crankcase heater
		bool ReportCoolingCoilCrankcasePower; // logical determines if the cooling coil crankcase heater power is reported
		Real64 CrankcaseHeaterCapacity; // total crankcase heater capacity [W]
		Real64 CrankcaseHeaterPower; // report variable for average crankcase heater power [W]
		Real64 MaxOATCrankcaseHeater; // maximum OAT for crankcase heater operation [C]
		Real64 CrankcaseHeaterConsumption; // report variable for total crankcase heater energy consumption [J]
		//condenser evaporative precooling
		int CondenserInletNodeNum; // Node number of outdoor condenser
		int CondenserType; // Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
		bool ReportEvapCondVars; // true if any performance mode includes an evap condenser
		Real64 EvapCondPumpElecNomPower; // Nominal power input to the evap condenser water circulation pump [W]
		Real64 EvapCondPumpElecPower; // Average power consumed by the evap condenser water circulation pump over
		// the time step [W]
		Real64 EvapWaterConsumpRate; // Evap condenser water consumption rate [m3/s]
		Real64 EvapCondPumpElecConsumption; // Electric energy consumed by the evap condenser water circulation pump [J]
		Real64 EvapWaterConsump; // Evap condenser water consumption [m3]
		Real64 BasinHeaterConsumption; // Basin heater energy consumption (J)
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree C below setpoint (W/C)
		Real64 BasinHeaterSetPointTemp; // setpoint temperature for basin heater operation (C)
		Real64 BasinHeaterPower; // Basin heater power (W)
		int BasinHeaterSchedulePtr; // Pointer to basin heater schedule
		Array1D< Real64 > EvapCondAirFlow; // Air flow rate through the evap condenser at high speed, volumetric flow rate
		// for water use calcs [m3/s]
		Array1D< Real64 > EvapCondEffect; // effectiveness of the evaporatively cooled condenser
		// [high speed for multi-speed unit] (-)
		Array1D< Real64 > MSRatedEvapCondVolFlowPerRatedTotCap; // evap condenser air flow ratio to capacity
		//begin variables for Water System interactions
		int EvapWaterSupplyMode; // where does water come from
		std::string EvapWaterSupplyName; // name of water source e.g. water storage tank
		int EvapWaterSupTankID;
		int EvapWaterTankDemandARRID;
		int CondensateCollectMode; // where does water come from
		std::string CondensateCollectName; // name of water source e.g. water storage tank
		int CondensateTankID;
		int CondensateTankSupplyARRID;
		Real64 CondensateVdot; // rate of water condensation from air stream [m3/s]
		Real64 CondensateVol; // amount of water condensed from air stream [m3]
		Real64 CondInletTemp; // Evap condenser inlet temperature [C], report variable
		Real64 SourceAirMassFlowRate; // source air mass flow rate [kg/s]
		Real64 InletSourceAirTemp; // source air temperature entering the outdoor coil [C]
		Real64 InletSourceAirEnthalpy; // source air enthalpy entering the outdoor coil [J/kg]
		//end variables for water system interactions

		//begin varibles for HPWH
		Real64 RatedCapWH; // Rated water heating Capacity [W]
		int InletAirTemperatureType; // Specifies to use either air wet-bulb or dry-bulb temp for curve objects
		Real64 WHRatedInletDBTemp; // Rated inlet air dry-bulb temperature [C]
		Real64 WHRatedInletWBTemp; // Rated inlet air wet-bulb temperature [C]
		Real64 WHRatedInletWaterTemp; // Rated condenser water inlet temperature [C]
		Real64 HPWHCondPumpElecNomPower; // Nominal power input to the condenser water circulation pump [W]
		Real64 HPWHCondPumpFracToWater; // Nominal power fraction to water for the condenser water circulation pump
		Real64 RatedHPWHCondWaterFlow; // Rated water flow rate through the condenser of the HPWH DX coil [m3/s]
		Real64 ElecWaterHeatingPower; // Total electric power consumed by compressor and condenser pump [W]
		Real64 ElecWaterHeatingConsumption; // Total electric consumption by compressor and condenser pump [J]
		bool FanPowerIncludedInCOP; // Indicates that fan heat is included in heating capacity and COP
		bool CondPumpHeatInCapacity; // Indicates that condenser pump heat is included in heating capacity
		bool CondPumpPowerInCOP; // Indicates that condenser pump power is included in heating COP
		bool AirVolFlowAutoSized; // Used to report autosizing info for the HPWH DX coil
		bool WaterVolFlowAutoSized; // Used to report autosizing info for the HPWH DX coil
		Real64 TotalHeatingEnergy; //total water heating energy
		Real64 TotalHeatingEnergyRate;//total WH energy rate
		//end variables for HPWH

		// Default Constructor
		VariableSpeedCoilData() :
			NumOfSpeeds( 2 ),
			NormSpedLevel( MaxSpedLevels ),
			RatedWaterVolFlowRate( AutoSize ),
			RatedWaterMassFlowRate( AutoSize ),
			RatedAirVolFlowRate( AutoSize ),
			RatedCapHeat( AutoSize ),
			RatedCapCoolTotal( AutoSize ),
			MaxONOFFCyclesperHour( 0.0 ),
			Twet_Rated( 0.0 ),
			Gamma_Rated( 0.0 ),
			HOTGASREHEATFLG( 0 ),
			HPTimeConstant( 0.0 ),
			PLFFPLR( 0 ),
			VSCoilTypeOfNum( 0 ),
			SimFlag( false ),
			DesignWaterMassFlowRate( 0.0 ),
			DesignWaterVolFlowRate( 0.0 ),
			DesignAirMassFlowRate( 0.0 ),
			DesignAirVolFlowRate( 0.0 ),
			AirVolFlowRate( 0.0 ),
			AirMassFlowRate( 0.0 ),
			InletAirPressure( 0.0 ),
			InletAirDBTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirDBTemp( 0.0 ),
			OutletAirHumRat( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			WaterVolFlowRate( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			InletWaterTemp( 0.0 ),
			InletWaterEnthalpy( 0.0 ),
			OutletWaterTemp( 0.0 ),
			OutletWaterEnthalpy( 0.0 ),
			Power( 0.0 ),
			QLoadTotal( 0.0 ),
			QSensible( 0.0 ),
			QLatent( 0.0 ),
			QSource( 0.0 ),
			QWasteHeat( 0.0 ),
			Energy( 0.0 ),
			EnergyLoadTotal( 0.0 ),
			EnergySensible( 0.0 ),
			EnergyLatent( 0.0 ),
			EnergySource( 0.0 ),
			COP( 0.0 ),
			RunFrac( 0.0 ),
			PartLoadRatio( 0.0 ),
			RatedPowerHeat( 0.0 ),
			RatedCOPHeat( 0.0 ),
			RatedCapCoolSens( 0.0 ),
			RatedPowerCool( 0.0 ),
			RatedCOPCool( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			WaterInletNodeNum( 0 ),
			WaterOutletNodeNum( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			FindCompanionUpStreamCoil( true ),
			IsDXCoilInZone( false ),
			CompanionCoolingCoilNum( 0 ),
			CompanionHeatingCoilNum( 0 ),
			FanDelayTime( 0.0 ),
			MSErrIndex( MaxSpedLevels, 0 ),
			MSRatedPercentTotCap( MaxSpedLevels, 0.0 ),
			MSRatedTotCap( MaxSpedLevels, 0.0 ),
			MSRatedSHR( MaxSpedLevels, 0.0 ),
			MSRatedCOP( MaxSpedLevels, 0.0 ),
			MSRatedAirVolFlowPerRatedTotCap( MaxSpedLevels, 0.0 ),
			MSRatedAirVolFlowRate( MaxSpedLevels, 0.0 ),
			MSRatedAirMassFlowRate( MaxSpedLevels, 0.0 ),
			MSRatedWaterVolFlowPerRatedTotCap( MaxSpedLevels, 0.0 ),
			MSRatedWaterVolFlowRate( MaxSpedLevels, 0.0 ),
			MSRatedWaterMassFlowRate( MaxSpedLevels, 0.0 ),
			MSRatedCBF( MaxSpedLevels, 0.0 ),
			MSEffectiveAo( MaxSpedLevels, 0.0 ),
			MSCCapFTemp( MaxSpedLevels, 0 ),
			MSCCapAirFFlow( MaxSpedLevels, 0 ),
			MSCCapWaterFFlow( MaxSpedLevels, 0 ),
			MSEIRFTemp( MaxSpedLevels, 0 ),
			MSEIRAirFFlow( MaxSpedLevels, 0 ),
			MSEIRWaterFFlow( MaxSpedLevels, 0 ),
			MSWasteHeat( MaxSpedLevels, 0 ),
			MSWasteHeatFrac( MaxSpedLevels, 0.0 ),
			MSWHPumpPower( MaxSpedLevels, 0.0 ),
			MSWHPumpPowerPerRatedTotCap( MaxSpedLevels, 0.0 ),
			SpeedNumReport( 0.0 ),
			SpeedRatioReport( 0.0 ),
			DefrostStrategy( 0 ),
			DefrostControl( 0 ),
			EIRFPLR( 0 ),
			DefrostEIRFT( 0 ),
			MinOATCompressor( 0.0 ),
			OATempCompressorOn( 0.0 ),
			MaxOATDefrost( 0.0 ),
			DefrostTime( 0.0 ),
			DefrostCapacity( 0.0 ),
			HPCompressorRuntime( 0.0 ),
			HPCompressorRuntimeLast( 0.0 ),
			TimeLeftToDefrost( 0.0 ),
			DefrostPower( 0.0 ),
			DefrostConsumption( 0.0 ),
			ReportCoolingCoilCrankcasePower( true ),
			CrankcaseHeaterCapacity( 0.0 ),
			CrankcaseHeaterPower( 0.0 ),
			MaxOATCrankcaseHeater( 0.0 ),
			CrankcaseHeaterConsumption( 0.0 ),
			CondenserInletNodeNum( 0 ),
			CondenserType( AirCooled ),
			ReportEvapCondVars( false ),
			EvapCondPumpElecNomPower( 0.0 ),
			EvapCondPumpElecPower( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			EvapCondPumpElecConsumption( 0.0 ),
			EvapWaterConsump( 0.0 ),
			BasinHeaterConsumption( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 0.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterSchedulePtr( 0 ),
			EvapCondAirFlow( MaxSpedLevels, 0.0 ),
			EvapCondEffect( MaxSpedLevels, 0.0 ),
			MSRatedEvapCondVolFlowPerRatedTotCap( MaxSpedLevels, 0.0 ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			CondensateCollectMode( CondensateDiscarded ),
			CondensateTankID( 0 ),
			CondensateTankSupplyARRID( 0 ),
			CondensateVdot( 0.0 ),
			CondensateVol( 0.0 ),
			CondInletTemp( 0.0 ),
			SourceAirMassFlowRate( 0.0 ),
			InletSourceAirTemp( 0.0 ),
			InletSourceAirEnthalpy( 0.0 ),
			//begin varibles for HPWH
			RatedCapWH( 0.0 ), // Rated water heating Capacity [W]
			InletAirTemperatureType( 0 ), // Specifies to use either air wet-bulb or dry-bulb temp for curve objects
			WHRatedInletDBTemp( 0.0 ), // Rated inlet air dry-bulb temperature [C]
			WHRatedInletWBTemp( 0.0 ),  // Rated inlet air wet-bulb temperature [C]
			WHRatedInletWaterTemp( 0.0 ),  // Rated condenser water inlet temperature [C]
			HPWHCondPumpElecNomPower( 0.0 ),  // Nominal power input to the condenser water circulation pump [W]
			HPWHCondPumpFracToWater( 1.0 ),  // Nominal power fraction to water for the condenser water circulation pump
			RatedHPWHCondWaterFlow( 0.0 ), // Rated water flow rate through the condenser of the HPWH DX coil [m3/s]
			ElecWaterHeatingPower( 0.0 ),  // Total electric power consumed by compressor and condenser pump [W]
			ElecWaterHeatingConsumption( 0.0 ),  // Total electric consumption by compressor and condenser pump [J]
			FanPowerIncludedInCOP( false ), // Indicates that fan heat is included in heating capacity and COP
			CondPumpHeatInCapacity( false ), // Indicates that condenser pump heat is included in heating capacity
			CondPumpPowerInCOP( false ), // Indicates that condenser pump power is included in heating COP
			AirVolFlowAutoSized( false ), // Used to report autosizing info for the HPWH DX coil
			WaterVolFlowAutoSized( false ), // Used to report autosizing info for the HPWH DX coil
			TotalHeatingEnergy( 0.0 ),  //total water heating energy
			TotalHeatingEnergyRate( 0.0 ) //total WH energy rate
			//end variables for HPWH
		{}

	};

	// Object Data
	extern Array1D< VariableSpeedCoilData > VarSpeedCoil;

	// Functions
	void
	clear_state();

	void
	SimVariableSpeedCoils(
		std::string const & CompName, // Coil Name
		int & CompIndex, // Index for Component name
		int const CyclingScheme, // Continuous fan OR cycling compressor
		Real64 & MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 & HPTimeConstant, // Heat pump time constant [s]
		Real64 & FanDelayTime, // Fan delay time, time delay for the HP's fan to
		int const CompOp, // compressor on/off. 0 = off; 1= on
		Real64 const PartLoadFrac,
		int const SpeedNum, // compressor speed number
		Real64 const SpeedRatio, // compressor speed ratio
		Real64 const SensLoad, // Sensible demand load [W]
		Real64 const LatentLoad, // Latent demand load [W]
		Optional< Real64 const > OnOffAirFlowRat = _ // ratio of comp on to comp off air flow rate
	);

	void
	GetVarSpeedCoilInput();

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitVarSpeedCoil(
		int const DXCoilNum, // Current DXCoilNum under simulation
		Real64 const MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 const HPTimeConstant, // Heat pump time constant [s]
		Real64 const FanDelayTime, // Fan delay time, time delay for the HP's fan to
		Real64 const SensLoad, // Control zone sensible load[W]
		Real64 const LatentLoad, // Control zone latent load[W]
		int const CyclingScheme, // fan operating mode
		Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
		Real64 const SpeedRatio, // compressor speed ratio
		int const SpeedNum // compressor speed number
	);

	void
	SizeVarSpeedCoil( int const DXCoilNum );

	void
	CalcVarSpeedCoilCooling(
		int const DXCoilNum, // Heat Pump Number
		int const CyclingScheme, // Fan/Compressor cycling scheme indicator
		Real64 & RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const SensDemand, // Cooling Sensible Demand [W] !unused1208
		Real64 const LatentDemand, // Cooling Latent Demand [W]
		int const CompOp, // compressor operation flag
		Real64 const PartLoadRatio, // compressor part load ratio
		Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
		int const SpeedNum // Speed number, high bound
	);

	void
	CalcVarSpeedCoilHeating(
		int const DXCoilNum, // Heat Pump Number
		int const CyclingScheme, // Fan/Compressor cycling scheme indicator
		Real64 & RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const SensDemand, // Cooling Sensible Demand [W] !unused1208
		int const CompOp, // compressor operation flag
		Real64 const PartLoadRatio, // compressor part load ratio
		Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
		int const SpeedNum // Speed number, high bound, i.e. SpeedNum - 1 is the other side
	);

	Real64
	GetCoilCapacityVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilIndexVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilAirFlowRateVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilInletNodeVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilOutletNodeVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetVSCoilCondenserInletNode(
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetVSCoilPLFFPLR(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetVSCoilMinOATCompressor(
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetVSCoilNumOfSpeeds(
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	void
	SetVarSpeedCoilData(
		int const WSHPNum, // Number of OA Controller
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_int CompanionCoolingCoilNum = _, // Index to cooling coil for heating coil = SimpleWSHPNum
		Optional_int CompanionHeatingCoilNum = _ // Index to heating coil for cooling coil = SimpleWSHPNum
	);

	void
	UpdateVarSpeedCoil( int const DXCoilNum );

	Real64
	CalcEffectiveSHR(
		int const DXCoilNum, // Index number for cooling coil
		Real64 const SHRss, // Steady-state sensible heat ratio
		int const CyclingScheme, // Fan/compressor cycling scheme indicator
		Real64 const RTF, // Compressor run-time fraction
		Real64 const QLatRated, // Rated latent capacity
		Real64 const QLatActual, // Actual latent capacity
		Real64 const EnteringDB, // Entering air dry-bulb temperature
		Real64 const EnteringWB // Entering air wet-bulb temperature
	);

	void
	CalcTotCapSHR_VSWSHP(
		Real64 const InletDryBulb, // inlet air dry bulb temperature [C]
		Real64 const InletHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const InletEnthalpy, // inlet air specific enthalpy [J/kg]
		Real64 const InletWetBulb, // inlet air wet bulb temperature [C]
		Real64 const AirMassFlowRatio, // Ratio of actual air mass flow to nominal air mass flow
		Real64 const WaterMassFlowRatio, // Ratio of actual water mass flow to nominal water mass flow
		Real64 const AirMassFlow, // actual mass flow for capacity and SHR calculation
		Real64 const CBF, // coil bypass factor
		Real64 const TotCapNom1, // nominal total capacity at low speed [W]
		int const CCapFTemp1, // capacity modifier curve index, function of entering wetbulb at low speed
		int const CCapAirFFlow1, // capacity modifier curve, function of actual air flow vs rated flow at low speed
		int const CCapWaterFFlow1, // capacity modifier curve, function of actual water flow vs rated flow at low speed
		Real64 const TotCapNom2, // nominal total capacity at high speed [W]
		int const CCapFTemp2, // capacity modifier curve index, function of entering wetbulb at high speed
		int const CCapAirFFlow2, // capacity modifier curve, function of actual air flow vs rated flow at high speed
		int const CCapWaterFFlow2, // capacity modifier curve, function of actual water flow vs rated flow at high speed
		Real64 & TotCap1, // total capacity at the given conditions [W] at low speed
		Real64 & TotCap2, // total capacity at the given conditions [W] at high speed
		Real64 & TotCapSpeed, // integrated total capacity corresponding to the speed ratio
		Real64 & SHR, // sensible heat ratio at the given conditions
		Real64 const CondInletTemp, // Condenser inlet temperature [C]
		Real64 const Pressure, // air pressure [Pa]
		Real64 const SpeedRatio, // from 0.0 to 1.0
		int const NumSpeeds // number of speeds for input
	);

	Real64
	AdjustCBF(
		Real64 const CBFNom, // nominal coil bypass factor
		Real64 const AirMassFlowRateNom, // nominal air mass flow rate [kg/s]
		Real64 const AirMassFlowRate // actual air mass flow rate [kg/s]
	);

	Real64
	CalcCBF(
		std::string const & UnitType,
		std::string const & UnitName,
		Real64 const InletAirTemp, // inlet air temperature [C]
		Real64 const InletAirHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const TotCap, // total cooling  capacity [Watts]
		Real64 const AirMassFlowRate, // the air mass flow rate at the given capacity [kg/s]
		Real64 const SHR // sensible heat ratio at the given capacity and flow rate
	);

	void
	CalcVarSpeedHPWH(
		int const DXCoilNum, // the number of the DX coil to be simulated
		Real64 & RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const PartLoadRatio, // sensible water heating load / full load sensible water heating capacity
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
		int const SpeedNum, // Speed number, high bound capacity
		int const CyclingScheme // Continuous fan OR cycling compressor
	);

} // VariableSpeedCoils

} // EnergyPlus

#endif
