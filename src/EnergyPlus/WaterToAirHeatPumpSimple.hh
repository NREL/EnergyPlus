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

#ifndef WaterToAirHeatPumpSimple_hh_INCLUDED
#define WaterToAirHeatPumpSimple_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataHVACGlobals.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace WaterToAirHeatPumpSimple {

	// Using/Aliasing
	using DataHVACGlobals::WaterCycling;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const CelsiustoKelvin; // Conversion from Celsius to Kelvin

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumWatertoAirHPs; // The Number of Water to Air Heat Pumps found in the Input
	//INTEGER        :: WaterIndex = 0                   ! Water index
	//INTEGER        :: Count = 0
	extern bool GetCoilsInputFlag; // Flag set to make sure you get input once
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool SimpleHPTimeStepFlag; // determines whether the previous operating mode for the coil and it's partner has been initialized

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

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine

	// Utility routines

	// Types

	struct SimpleWatertoAirHPConditions
	{
		// Members
		std::string Name; // Name of the Water to Air Heat pump
		std::string WatertoAirHPType; // Type of WatertoAirHP ie. Heating or Cooling
		int WAHPPlantTypeOfNum; // type of component in plant
		bool SimFlag; // Heat Pump Simulation Flag
		Real64 AirVolFlowRate; // Air Volumetric Flow Rate[m3/s]
		Real64 AirMassFlowRate; // Air Mass Flow Rate[kg/s]
		Real64 InletAirDBTemp; // Inlet Air Dry Bulb Temperature [C]
		Real64 InletAirHumRat; // Inlet Air Humidity Ratio [kg/kg]
		Real64 InletAirEnthalpy; // Inlet Air Enthalpy [J/kg]
		Real64 OutletAirDBTemp; // Outlet Air Dry Bulb Temperature [C]
		Real64 OutletAirHumRat; // Outlet Air Humidity Ratio [kg/kg]
		Real64 OutletAirEnthalpy; // Outlet Air Enthalpy [J/kg]
		Real64 WaterVolFlowRate; // Water Volumetric Flow Rate [m3/s]
		Real64 WaterMassFlowRate; // Water Mass Flow Rate [kg/s]
		Real64 DesignWaterMassFlowRate;
		Real64 InletWaterTemp; // Inlet Water Temperature [C]
		Real64 InletWaterEnthalpy; // Inlet Water Enthalpy [J/kg]
		Real64 OutletWaterTemp; // Outlet Water Temperature [C]
		Real64 OutletWaterEnthalpy; // Outlet Water Enthalpy [J/kg]
		Real64 Power; // Power Consumption [W]
		Real64 QLoadTotal; // Load Side Total Heat Transfer Rate [W]
		Real64 QSensible; // Sensible Load Side Heat Transfer Rate [W]
		Real64 QLatent; // Latent Load Side Heat Transfer Rate [W]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 Energy; // Energy Consumption [J]
		Real64 EnergyLoadTotal; // Load Side Total Heat Transferred [J]
		Real64 EnergySensible; // Sensible Load Side Heat Transferred [J]
		Real64 EnergyLatent; // Latent Load Side Heat Transferred [J]
		Real64 EnergySource; // Source Side Heat Transferred [J]
		Real64 COP; // Heat Pump Coefficient of Performance [-]
		Real64 RunFrac; // Duty Factor
		Real64 PartLoadRatio; // Part Load Ratio
		Real64 RatedWaterVolFlowRate; // Rated/Ref Water Volumetric Flow Rate [m3/s]
		Real64 RatedAirVolFlowRate; // Rated/Ref Air Volumetric Flow Rate [m3/s]
		Real64 RatedCapHeat; // Rated/Ref Heating Capacity [W]
		Real64 RatedPowerHeat; // Rated/Ref Heating Power Consumption[W]
		Real64 RatedCOPHeat; // Rated/Ref Heating COP [W/W]
		Real64 RatedCapCoolTotal; // Rated/Ref Total Cooling Capacity [W]
		Real64 RatedCapCoolSens; // Rated/Ref Sensible Cooling Capacity [W]
		Real64 RatedPowerCool; // Rated/Ref Cooling Power Consumption[W]
		Real64 RatedCOPCool; // Rated/Ref Cooling COP [W/W]
		Real64 HeatCap1; // 1st coefficient of the Heating capacity performance curve
		Real64 HeatCap2; // 2nd coefficient of the Heating capacity performance curve
		Real64 HeatCap3; // 3rd coefficient of the Heating capacity performance curve
		Real64 HeatCap4; // 4th coefficient of the Heating capacity performance curve
		Real64 HeatCap5; // 5th coefficient of the Heating capacity performance curve
		Real64 HeatPower1; // 1st coefficient of the Heating power consumption curve
		Real64 HeatPower2; // 2nd coefficient of the Heating power consumption curve
		Real64 HeatPower3; // 3rd coefficient of the Heating power consumption curve
		Real64 HeatPower4; // 4th coefficient of the Heating power consumption curve
		Real64 HeatPower5; // 5th coefficient of the Heating power consumption curve
		Real64 TotalCoolCap1; // 1st coefficient of the Total Cooling capacity performance curve
		Real64 TotalCoolCap2; // 2nd coefficient of the Total Cooling capacity performance curve
		Real64 TotalCoolCap3; // 3rd coefficient of the Total Cooling capacity performance curve
		Real64 TotalCoolCap4; // 4th coefficient of the Total Cooling capacity performance curve
		Real64 TotalCoolCap5; // 5th coefficient of the Total Cooling capacity performance curve
		Real64 SensCoolCap1; // 1st coefficient of the Sensible Cooling capacity performance curve
		Real64 SensCoolCap2; // 2nd coefficient of the Sensible Cooling capacity performance curve
		Real64 SensCoolCap3; // 3rd coefficient of the Sensible Cooling capacity performance curve
		Real64 SensCoolCap4; // 4th coefficient of the Sensible Cooling capacity performance curve
		Real64 SensCoolCap5; // 5th coefficient of the Sensible Cooling capacity performance curve
		Real64 SensCoolCap6; // 6th coefficient of the Sensible Cooling capacity performance curve
		Real64 CoolPower1; // 1st coefficient of the Cooling power consumption curve
		Real64 CoolPower2; // 2nd coefficient of the Cooling power consumption curve
		Real64 CoolPower3; // 3rd coefficient of the Cooling power consumption curve
		Real64 CoolPower4; // 4th coefficient of the Cooling power consumption curve
		Real64 CoolPower5; // 5th coefficient of the Cooling power consumption curve
		int AirInletNodeNum; // Node Number of the Air Inlet
		int AirOutletNodeNum; // Node Number of the Air Outlet
		int WaterInletNodeNum; // Node Number of the Water Onlet
		int WaterOutletNodeNum; // Node Number of the Water Outlet
		int LoopNum; // plant loop index for water side
		int LoopSide; // plant loop side index
		int BranchNum; // plant branch index
		int CompNum; // plant component index
		int WaterCyclingMode; // Heat Pump Coil water flow mode; See definitions in DataHVACGlobals,
		// 1=water cycling, 2=water constant, 3=water constant on demand (old mode)
		int LastOperatingMode; // type of coil calling for water flow, either heating or cooling,
		// start it at 1 so there will be water flow from the start,
		// even if there is no load.
		// Gets updated only during the first iteration of each timestep
		bool WaterFlowMode; // whether the water flow through the coil is called
		// because there is a load on the coil, or not.
		// Gets updated each iteration
		// set by parent object and "pushed" to this structure in SetSimpleWSHPData subroutine
		int CompanionCoolingCoilNum; // Heating coil companion cooling coil index
		int CompanionHeatingCoilNum; // Cooling coil companion heating coil index
		Real64 Twet_Rated; // Nominal Time for Condensate Removal to Begin [s]
		Real64 Gamma_Rated; // Ratio of Initial Moisture Evaporation Rate
		// and Steady-state Latent Capacity
		Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of heat pump [cycles/hr]
		Real64 HPTimeConstant; // Heat pump time constant [s]
		Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to

		// Default Constructor
		SimpleWatertoAirHPConditions() :
			WAHPPlantTypeOfNum( 0 ),
			SimFlag( false ),
			AirVolFlowRate( 0.0 ),
			AirMassFlowRate( 0.0 ),
			InletAirDBTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirDBTemp( 0.0 ),
			OutletAirHumRat( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			WaterVolFlowRate( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			DesignWaterMassFlowRate( 0.0 ),
			InletWaterTemp( 0.0 ),
			InletWaterEnthalpy( 0.0 ),
			OutletWaterTemp( 0.0 ),
			OutletWaterEnthalpy( 0.0 ),
			Power( 0.0 ),
			QLoadTotal( 0.0 ),
			QSensible( 0.0 ),
			QLatent( 0.0 ),
			QSource( 0.0 ),
			Energy( 0.0 ),
			EnergyLoadTotal( 0.0 ),
			EnergySensible( 0.0 ),
			EnergyLatent( 0.0 ),
			EnergySource( 0.0 ),
			COP( 0.0 ),
			RunFrac( 0.0 ),
			PartLoadRatio( 0.0 ),
			RatedWaterVolFlowRate( 0.0 ),
			RatedAirVolFlowRate( 0.0 ),
			RatedCapHeat( 0.0 ),
			RatedPowerHeat( 0.0 ),
			RatedCOPHeat( 0.0 ),
			RatedCapCoolTotal( 0.0 ),
			RatedCapCoolSens( 0.0 ),
			RatedPowerCool( 0.0 ),
			RatedCOPCool( 0.0 ),
			HeatCap1( 0.0 ),
			HeatCap2( 0.0 ),
			HeatCap3( 0.0 ),
			HeatCap4( 0.0 ),
			HeatCap5( 0.0 ),
			HeatPower1( 0.0 ),
			HeatPower2( 0.0 ),
			HeatPower3( 0.0 ),
			HeatPower4( 0.0 ),
			HeatPower5( 0.0 ),
			TotalCoolCap1( 0.0 ),
			TotalCoolCap2( 0.0 ),
			TotalCoolCap3( 0.0 ),
			TotalCoolCap4( 0.0 ),
			TotalCoolCap5( 0.0 ),
			SensCoolCap1( 0.0 ),
			SensCoolCap2( 0.0 ),
			SensCoolCap3( 0.0 ),
			SensCoolCap4( 0.0 ),
			SensCoolCap5( 0.0 ),
			SensCoolCap6( 0.0 ),
			CoolPower1( 0.0 ),
			CoolPower2( 0.0 ),
			CoolPower3( 0.0 ),
			CoolPower4( 0.0 ),
			CoolPower5( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			WaterInletNodeNum( 0 ),
			WaterOutletNodeNum( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			WaterCyclingMode( 0 ),
			LastOperatingMode( WaterCycling ),
			WaterFlowMode( false ),
			CompanionCoolingCoilNum( 0 ),
			CompanionHeatingCoilNum( 0 ),
			Twet_Rated( 0.0 ),
			Gamma_Rated( 0.0 ),
			MaxONOFFCyclesperHour( 0.0 ),
			HPTimeConstant( 0.0 ),
			FanDelayTime( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< SimpleWatertoAirHPConditions > SimpleWatertoAirHP;

	// Functions

	void
	SimWatertoAirHPSimple(
		std::string const & CompName, // Coil Name
		int & CompIndex, // Index for Component name
		Real64 const SensLoad, // Sensible demand load [W]
		Real64 const LatentLoad, // Latent demand load [W]
		int const CyclingScheme, // Continuous fan OR cycling compressor
		Real64 const RuntimeFrac, // Compressor run time fraction  or
		Real64 & MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 & HPTimeConstant, // Heat pump time constant [s]
		Real64 & FanDelayTime, // Fan delay time, time delay for the HP's fan to
		int const CompOp,
		Real64 const PartLoadRatio,
		bool const FirstHVACIteration,
		Optional< Real64 const > OnOffAirFlowRat = _ // ratio of comp on to comp off air flow rate
	);

	// MODULE SUBROUTINES:
	//*************************************************************************

	void
	GetSimpleWatertoAirHPInput();

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSimpleWatertoAirHP(
		int const HPNum, // Current HPNum under simulation
		Real64 const MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 const HPTimeConstant, // Heat pump time constant [s]
		Real64 const FanDelayTime, // Fan delay time, time delay for the HP's fan to
		Real64 const SensLoad, // Control zone sensible load[W]
		Real64 const LatentLoad, // Control zone latent load[W]
		int const CyclingScheme, // fan operating mode
		Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
		Real64 const WaterPartLoad,
		bool const FirstHVACIteration // Iteration flag
	);

	void
	SizeHVACWaterToAir( int const HPNum );

	void
	CalcHPCoolingSimple(
		int const HPNum, // Heat Pump Number
		int const CyclingScheme, // Fan/Compressor cycling scheme indicator
		Real64 const RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const SensDemand, // Cooling Sensible Demand [W] !unused1208
		Real64 const LatentDemand, // Cooling Latent Demand [W]
		int const CompOp, // compressor operation flag
		Real64 const PartLoadRatio, // compressor part load ratio
		Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
		Real64 const WaterPartLoad // water part load ratio
	);

	void
	CalcHPHeatingSimple(
		int const HPNum, // Heat Pump Number
		int const CyclingScheme, // Fan/Compressor cycling scheme indicator
		Real64 const RuntimeFrac, // Runtime Fraction of compressor
		Real64 const SensDemand, // Cooling Sensible Demand [W] !unused1208
		int const CompOp, // compressor operation flag
		Real64 const PartLoadRatio, // compressor part load ratio
		Real64 const OnOffAirFlowRatio, // ratio of compressor on flow to average flow over time step
		Real64 const WaterPartLoad // water part load ratio
	);

	void
	UpdateSimpleWatertoAirHP( int const HPNum );

	//        End of Update subroutines for the WatertoAirHP Module
	// *****************************************************************************

	Real64
	CalcEffectiveSHR(
		int const HPNum, // Index number for cooling coil
		Real64 const SHRss, // Steady-state sensible heat ratio
		int const CyclingScheme, // Fan/compressor cycling scheme indicator
		Real64 const RTF, // Compressor run-time fraction
		Real64 const QLatRated, // Rated latent capacity
		Real64 const QLatActual, // Actual latent capacity
		Real64 const EnteringDB, // Entering air dry-bulb temperature
		Real64 const EnteringWB // Entering air wet-bulb temperature
	);

	int
	GetCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilAirFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	void
	SetSimpleWSHPData(
		int const SimpleWSHPNum, // Number of OA Controller
		bool & ErrorsFound, // Set to true if certain errors found
		int const WaterCyclingMode, // the coil water flow mode (cycling, constant or constantondemand)
		Optional_int CompanionCoolingCoilNum = _, // Index to cooling coil for heating coil = SimpleWSHPNum
		Optional_int CompanionHeatingCoilNum = _ // Index to heating coil for cooling coil = SimpleWSHPNum
	);

} // WaterToAirHeatPumpSimple

} // EnergyPlus

#endif
