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

#ifndef ManageElectricPower_hh_INCLUDED
#define ManageElectricPower_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ManageElectricPower {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:

	extern int const iOpSchemeBaseLoad; // Electric load center dispatch mode
	extern int const iOpSchemeDemandLimit; // Electric load center dispatch mode
	extern int const iOpSchemeTrackElectrical; // Electric load center dispatch mode
	extern int const iOpSchemeTrackSchedule; // Electric load center dispatch mode
	extern int const iOpSchemeTrackMeter; // Electric load center dispatch mode
	extern int const iOpSchemeThermalFollow; // Electric load center dispatch mode
	extern int const iOpSchemeThermalFollowLimitElectrical; // Electric load center dispatch mode

	extern int const ACBuss; // Electic load center buss and power conditioning mode
	extern int const ACBussStorage; // Electic load center buss and power conditioning mode
	extern int const DCBussInverter; // Electic load center buss and power conditioning mode
	extern int const DCBussInverterDCStorage; // Electic load center buss and power conditioning mode
	extern int const DCBussInverterACStorage; // Electic load center buss and power conditioning mode

	extern int const CECLookUpTableModel; // inverter model mode
	extern int const CurveFuncOfPower; // inverter model mode
	extern int const SimpleConstantEff; // inverter model mode

	extern int const ZoneGains; // power conditioning equipment thermal loss destination
	extern int const LostToOutside; // power conditioning equipment thermal loss destination

	extern int const SimpleBucketStorage; // storage model mode (1 of 2)
	extern int const KiBaMBattery; // storage model mode (2 of 2)

	extern int const PowerInFromGrid; // Transformer usage: power in from grid
	extern int const PowerOutFromBldg; // Transformer usage: power out from onsite generation
	extern int const LossesMethod; // Transformer performance input methos: RatedLosses
	extern int const EfficiencyMethod; // Transformer performance input methos: NominalEfficiency

	extern int const Battery_LifeCalculation_Yes;
	extern int const Battery_LifeCalculation_No;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetInput; // When TRUE, calls subroutine to read input file.
	extern int NumLoadCenters;
	extern int NumInverters;
	extern int NumElecStorageDevices;
	extern int NumTransformers;

	extern int ElecProducedCoGenIndex;
	extern int ElecProducedPVIndex;
	extern int ElecProducedWTIndex;

	extern int MaxRainflowArrayBounds;
	extern int MaxRainflowArrayInc;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Types

	struct GenData
	{
		// Members
		std::string Name; // user identifier
		std::string TypeOf; // equipment type
		int CompType_Num; // Numeric designator for CompType (TypeOf)
		int GeneratorIndex;
		Real64 MaxPowerOut; // Maximum Power Output (W)
		std::string AvailSched; // Operation Schedule.
		int AvailSchedPtr; // pointer to operation schedule
		Real64 PowerRequestThisTimestep; // Current Demand on Equipment (W)
		bool ONThisTimestep; // Indicator whether Generator on
		Real64 EMSPowerRequest; // EMS actuator for current demand on equipment (W)
		bool EMSRequestOn; // EMS actuating On if true.
		bool PlantInfoFound;
		int PlantLoopNum; // Cogen: pointer to plant loop data structure
		int LoopSideNum; // Cogen: pointer to plant loop data structure
		int BranchNum; // Cogen: pointer to plant loop data structure
		int CompNum; // Cogen: pointer to plant loop data structure
		Real64 NominalThermElectRatio; // Cogen: nominal ratio of thermal to elect production
		//results of component models for load center reporting
		Real64 DCElectricityProd; // Current DC Electric Produced from Equipment (J)
		Real64 DCElectProdRate; // Current DC Electric Production Rate from Equipment (W)
		Real64 ElectricityProd; // Current AC Electric Produced from Equipment (J)
		Real64 ElectProdRate; // Current AC Electric Production Rate from Equipment (W)
		Real64 ThermalProd; // Current Thermal energy Produced from Equipment (J)
		Real64 ThermalProdRate; // Current Thermal energy Production Rate from Equipment (W)

		// Default Constructor
		GenData() :
			CompType_Num( 0 ),
			GeneratorIndex( 0 ),
			MaxPowerOut( 0.0 ),
			AvailSchedPtr( 0 ),
			PowerRequestThisTimestep( 0.0 ),
			ONThisTimestep( false ),
			EMSPowerRequest( 0.0 ),
			EMSRequestOn( false ),
			PlantInfoFound( false ),
			PlantLoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			NominalThermElectRatio( 0.0 ),
			DCElectricityProd( 0.0 ),
			DCElectProdRate( 0.0 ),
			ElectricityProd( 0.0 ),
			ElectProdRate( 0.0 ),
			ThermalProd( 0.0 ),
			ThermalProdRate( 0.0 )
		{}

	};

	struct ElectricPowerLoadCenter
	{
		// Members
		std::string Name; // user identifier
		std::string GeneratorList; // List name of available generators
		int OperationScheme; // Name of Operation Scheme
		std::string DemandMeterName; // Name of Demand Energy Meter for "on demand" operation
		int DemandMeterPtr; // "pointer" to Meter for electrical Demand to meet
		std::string GenerationMeterName; // Name of Generated Energy Meter for "on demand" operation
		int NumGenerators; // Number of Generators
		Array1D< GenData > ElecGen; // pointer to generator
		Real64 DemandLimit; // Demand Limit in Watts(W) which the generator will operate above
		int TrackSchedPtr; // "pointer" to schedule for electrical demand to meet.
		int BussType; // is this load center powered by AC or DC generators
		bool InverterPresent;
		std::string InverterName; // hold name for verificaton and error messages
		int InverterModelNum; // simulation model parameter type
		Real64 DCElectricityProd; // Current DC Elect produced (J) (if buss type DCbussInverter)
		Real64 DCElectProdRate; // Current DC Elect power produced (W) (if buss type DCbussInverter)
		Real64 DCpowerConditionLosses; // current DC to AC inverter losses (W) (if DCbussInverter)
		bool StoragePresent;
		std::string StorageName; // hold name for verificaton and error messages
		int StorageModelNum; // simulation model parameter type
		bool TransformerPresent;
		std::string TransformerName; // hold name for verificaton and error messages
		int TransformerModelNum; // simulation model parameter type
		Real64 ElectricityProd; // Current AC Electric Produced from Equipment (J)
		Real64 ElectProdRate; // Current Electric Production Rate from Equipment (W)
		Real64 ThermalProd; // Current Thermal energy Produced from Equipment (J)
		Real64 ThermalProdRate; // Current Thermal energy Production Rate from Equipment (W)
		Real64 TotalPowerRequest; // Total electric power request from the load center (W)
		Real64 TotalThermalPowerRequest; // Total thermal power request from the load center (W)
		Real64 ElectDemand; // Current electric power demand on the load center (W)

		// Default Constructor
		ElectricPowerLoadCenter() :
			OperationScheme( 0 ),
			DemandMeterPtr( 0 ),
			NumGenerators( 0 ),
			DemandLimit( 0.0 ),
			TrackSchedPtr( 0 ),
			BussType( 0 ),
			InverterPresent( false ),
			InverterModelNum( 0 ),
			DCElectricityProd( 0.0 ),
			DCElectProdRate( 0.0 ),
			DCpowerConditionLosses( 0.0 ),
			StoragePresent( false ),
			StorageModelNum( 0 ),
			TransformerPresent( false ),
			TransformerModelNum( 0 ),
			ElectricityProd( 0.0 ),
			ElectProdRate( 0.0 ),
			ThermalProd( 0.0 ),
			ThermalProdRate( 0.0 ),
			TotalPowerRequest( 0.0 ),
			TotalThermalPowerRequest( 0.0 ),
			ElectDemand( 0.0 )
		{}

	};

	struct CECInverterLookUpTableData
	{
		// Members
		Real64 NightTareLossPower;
		Real64 NominalVoltage;
		Array1D< Real64 > NomVoltEfficiencyARR; // eff at 10, 20, 30, 50, 75, & 100% power and Nominal voltage

		// Default Constructor
		CECInverterLookUpTableData() :
			NightTareLossPower( 0.0 ),
			NominalVoltage( 0.0 ),
			NomVoltEfficiencyARR( 6, 0.0 )
		{}

	};

	struct DCtoACInverterStruct
	{
		// Members
		std::string Name; // user identifier
		int ModelType; // type of inverter model used
		int AvailSchedPtr; // number for availability schedule.
		int HeatLossesDestination;
		int ZoneNum; // destination zone for heat losses from inverter.
		Real64 ZoneRadFract; // radiative fraction for thermal losses to zone
		CECInverterLookUpTableData LUTable;
		int CurveNum; // curve index for eff as func of power
		Real64 RatedPower; // rated, max continuous power output level for inverter
		Real64 MinPower;
		Real64 MaxPower;
		Real64 MinEfficiency;
		Real64 MaxEfficiency;
		Real64 StandbyPower;
		//results and reporting
		Real64 Efficiency;
		Real64 DCPowerIn;
		Real64 ACPowerOut;
		Real64 DCEnergyIn;
		Real64 ACEnergyOut;
		Real64 ThermLossRate;
		Real64 ThermLossEnergy;
		Real64 QdotConvZone;
		Real64 QdotRadZone;
		Real64 AncillACuseRate;
		Real64 AncillACuseEnergy;

		// Default Constructor
		DCtoACInverterStruct() :
			ModelType( 0 ),
			AvailSchedPtr( 0 ),
			HeatLossesDestination( 0 ),
			ZoneNum( 0 ),
			ZoneRadFract( 0.0 ),
			CurveNum( 0 ),
			RatedPower( 0.0 ),
			MinPower( 0.0 ),
			MaxPower( 0.0 ),
			MinEfficiency( 0.0 ),
			MaxEfficiency( 0.0 ),
			StandbyPower( 0.0 ),
			Efficiency( 0.0 ),
			DCPowerIn( 0.0 ),
			ACPowerOut( 0.0 ),
			DCEnergyIn( 0.0 ),
			ACEnergyOut( 0.0 ),
			ThermLossRate( 0.0 ),
			ThermLossEnergy( 0.0 ),
			QdotConvZone( 0.0 ),
			QdotRadZone( 0.0 ),
			AncillACuseRate( 0.0 ),
			AncillACuseEnergy( 0.0 )
		{}

	};

	struct ElecStorageDataStruct
	{
		// Members
		//user defined variables
		std::string Name; // name of this electrical storage module
		int StorageModelMode; // type of model parameter, SimpleBucketStorage
		int AvailSchedPtr; // availability schedule index.
		int HeatLossesDestination; // mode for where thermal losses go
		int ZoneNum; // destination zone for heat losses from inverter.
		Real64 ZoneRadFract; // radiative fraction for thermal losses to zone
		Real64 StartingEnergyStored; // [J] joules inside at beginning of environment period
		Real64 EnergeticEfficCharge; // [ ] efficiency of charging
		Real64 EnergeticEfficDischarge; // [ ] efficiency of discharging
		Real64 MaxPowerDraw; // [W] max rate of discharge
		Real64 MaxPowerStore; // [W] max rate of charge
		Real64 MaxEnergyCapacity; // [J] max storage capacity
		int ParallelNum; // [ ] number of battery modules in parallel
		int SeriesNum; // [ ] number of battery modules in series
		int ChargeCurveNum; // [ ] voltage change curve index number for charging
		int DischargeCurveNum; // [ ] voltage change curve index number for discharging
		int CycleBinNum; // [ ] number of cycle bins
		Real64 StartingSOC; // [ ] initial fractional state of charge
		Real64 MaxAhCapacity; // [Ah]maximum capacity
		Real64 AvailableFrac; // [ ] fraction of available charge capacity
		Real64 ChargeConversionRate; // [1/h]change rate from bound charge energy to available charge
		Real64 ChargedOCV; // [V] fully charged oppen circuit voltage
		Real64 DischargedOCV; // [V] fully discharged open circuit voltage
		Real64 InternalR; // [ohm]internal electric resistance
		Real64 MaxDischargeI; // [A] maximum discharging current
		Real64 CutoffV; // [V] cut-off voltage
		Real64 MaxChargeRate; // [1/h]charge rate limit
		int LifeCalculation; // [ ]battery life calculation: Yes or No
		int LifeCurveNum; // [ ]battery life curve name index number
		//calculated and from elsewhere vars
		Real64 ThisTimeStepStateOfCharge; // [J]
		Real64 LastTimeStepStateOfCharge; // [J]
		Real64 PelNeedFromStorage; // [W]
		Real64 PelFromStorage; // [W]
		bool EMSOverridePelFromStorage; // if true, EMS calling for override
		Real64 EMSValuePelFromStorage; // value EMS is directing to use, power from storage [W]
		Real64 PelIntoStorage; // [W]
		bool EMSOverridePelIntoStorage; // if true, EMS calling for override
		Real64 EMSValuePelIntoStorage; // value EMS is directing to use, power into storage [W]
		Real64 QdotConvZone; // [W]
		Real64 QdotRadZone; // [W]
		Real64 TimeElapsed; // [h]
		Real64 ThisTimeStepAvailable; // [Ah] available charge at the current timestep
		Real64 ThisTimeStepBound; // [Ah] bound charge at the current timestep
		Real64 LastTimeStepAvailable; // [Ah] available charge at the previous timestep
		Real64 LastTimeStepBound; // [Ah] bound charge at the previous timestep
		Real64 LastTwoTimeStepAvailable; // [Ah] available charge at the previous two timesteps
		Real64 LastTwoTimeStepBound; // [Ah] bound charge at the previous two timesteps
		//battery life calculation variables
		int count0;
		Array1D< Real64 > B10;
		Array1D< Real64 > X0;
		Array1D< Real64 > Nmb0;
		Array1D< Real64 > OneNmb0;
		//report
		Real64 ElectEnergyinStorage; // [J] state of charge
		Real64 StoredPower; // [W]
		Real64 StoredEnergy; // [J]
		Real64 DecrementedEnergyStored; // [J] this is the negative of StoredEnergy
		Real64 DrawnPower; // [W]
		Real64 DrawnEnergy; // [J]
		Real64 ThermLossRate; // [W]
		Real64 ThermLossEnergy; // [J]
		int StorageMode; // [ ] mode of operation 0 for idle, 1 for discharging, 2 for charging
		Real64 AbsoluteSOC; // [Ah] total state of charge
		Real64 FractionSOC; // [ ] fractional state of charge
		Real64 BatteryCurrent; // [A] total current
		Real64 BatteryVoltage; // [V] total voltage
		Real64 BatteryDamage; // [ ] fractional battery damage

		// Default Constructor
		ElecStorageDataStruct() :
			StorageModelMode( 0 ),
			AvailSchedPtr( 0 ),
			HeatLossesDestination( 0 ),
			ZoneNum( 0 ),
			ZoneRadFract( 0.0 ),
			StartingEnergyStored( 0.0 ),
			EnergeticEfficCharge( 0.0 ),
			EnergeticEfficDischarge( 0.0 ),
			MaxPowerDraw( 0.0 ),
			MaxPowerStore( 0.0 ),
			MaxEnergyCapacity( 0.0 ),
			ParallelNum( 0 ),
			SeriesNum( 0 ),
			ChargeCurveNum( 0 ),
			DischargeCurveNum( 0 ),
			CycleBinNum( 0 ),
			StartingSOC( 0.0 ),
			MaxAhCapacity( 0.0 ),
			AvailableFrac( 0.0 ),
			ChargeConversionRate( 0.0 ),
			ChargedOCV( 0.0 ),
			DischargedOCV( 0.0 ),
			InternalR( 0.0 ),
			MaxDischargeI( 0.0 ),
			CutoffV( 0.0 ),
			MaxChargeRate( 0.0 ),
			LifeCalculation( 0 ),
			LifeCurveNum( 0 ),
			ThisTimeStepStateOfCharge( 0.0 ),
			LastTimeStepStateOfCharge( 0.0 ),
			PelNeedFromStorage( 0.0 ),
			PelFromStorage( 0.0 ),
			EMSOverridePelFromStorage( false ),
			EMSValuePelFromStorage( 0.0 ),
			PelIntoStorage( 0.0 ),
			EMSOverridePelIntoStorage( false ),
			EMSValuePelIntoStorage( 0.0 ),
			QdotConvZone( 0.0 ),
			QdotRadZone( 0.0 ),
			TimeElapsed( 0.0 ),
			ThisTimeStepAvailable( 0.0 ),
			ThisTimeStepBound( 0.0 ),
			LastTimeStepAvailable( 0.0 ),
			LastTimeStepBound( 0.0 ),
			LastTwoTimeStepAvailable( 0.0 ),
			LastTwoTimeStepBound( 0.0 ),
			count0( 0 ),
			ElectEnergyinStorage( 0.0 ),
			StoredPower( 0.0 ),
			StoredEnergy( 0.0 ),
			DecrementedEnergyStored( 0.0 ),
			DrawnPower( 0.0 ),
			DrawnEnergy( 0.0 ),
			ThermLossRate( 0.0 ),
			ThermLossEnergy( 0.0 ),
			StorageMode( 0 ),
			AbsoluteSOC( 0.0 ),
			FractionSOC( 0.0 ),
			BatteryCurrent( 0.0 ),
			BatteryVoltage( 0.0 ),
			BatteryDamage( 0.0 )
		{}

	};

	struct ElectricTransformer
	{
		// Members
		// user defined variables
		std::string Name; // user identifier
		int AvailSchedPtr; // availability schedule index.
		int UsageMode; // mode for transformer usage
		int HeatLossesDestination; // mode for where thermal losses go
		int ZoneNum; // destination zone for heat losses from inverter.
		Real64 ZoneRadFrac; // radiative fraction for thermal losses to zone
		Real64 RatedCapacity; // rated capacity [VA]
		int Phase; // phase
		Real64 FactorTempCoeff; // thermal coefficient of resistance for winding material
		Real64 TempRise; // full load temperature rise [C]
		Real64 EddyFrac; // fraction of eddy current losses []
		int PerformanceInputMode; // performance input method
		Real64 RatedEfficiency; // nameplate efficiency []
		Real64 RatedPUL; // per unit load for nameplate efficiency []
		Real64 RatedTemp; // reference temperature for nameplate efficiency [C]
		Real64 MaxPUL; // per unit load for maximum efficiency []
		bool ConsiderLosses; // if true, consider transformer lossses in metering
		Array1D_string WiredMeterNames; // names of the meters wired to transformer
		Array1D_int WiredMeterPtrs; // array of "pointers" to meters wired to transformer
		Array1D_bool SpecialMeter; // indicates whether a meter needs special consideration
		// Electricity:Facility and Electricity:HVAC are two special
		// meters because tranformer loss is part of them
		//calculated and from elsewhere vars
		Real64 RatedNL; // rated no load losses, user input or calculated [W]
		Real64 RatedLL; // rated load losses, user input or calculated [W]
		int LoadCenterNum; // number of load centers served by the transformer
		Array1D_int LoadCenterIndexes; // index array of load centers served by the transformer
		int OverloadErrorIndex; // used for warning message when transformer is overloaded
		//results and reporting
		Real64 Efficiency; // transformer efficiency
		Real64 PowerIn; // [W]
		Real64 EnergyIn; // [J]
		Real64 PowerOut; // [W]
		Real64 EnergyOut; // [J]
		Real64 NoLoadLossRate; // [W]
		Real64 NoLoadLossEnergy; // [J]
		Real64 LoadLossRate; // [W]
		Real64 LoadLossEnergy; // [J]
		Real64 ThermalLossRate; // [W]
		Real64 ThermalLossEnergy; // [J]
		Real64 ElecUseUtility; // [J] Energy consumption for a utility transformer (power in)
		// Positive values
		Real64 ElecProducedCoGen; // [J] Energy consumption for a cogeneration transformer (power out)
		// Negative values
		Real64 QdotConvZone; // [W]
		Real64 QdotRadZone; // [W]

		// Default Constructor
		ElectricTransformer() :
			AvailSchedPtr( 0 ),
			UsageMode( 0 ),
			HeatLossesDestination( 0 ),
			ZoneNum( 0 ),
			ZoneRadFrac( 0.0 ),
			RatedCapacity( 0.0 ),
			Phase( 0 ),
			FactorTempCoeff( 0.0 ),
			TempRise( 0.0 ),
			EddyFrac( 0.0 ),
			PerformanceInputMode( 0 ),
			RatedEfficiency( 0.0 ),
			RatedPUL( 0.0 ),
			RatedTemp( 0.0 ),
			MaxPUL( 0.0 ),
			ConsiderLosses( true ),
			RatedNL( 0.0 ),
			RatedLL( 0.0 ),
			LoadCenterNum( 0 ),
			OverloadErrorIndex( 0 ),
			Efficiency( 0.0 ),
			PowerIn( 0.0 ),
			EnergyIn( 0.0 ),
			PowerOut( 0.0 ),
			EnergyOut( 0.0 ),
			NoLoadLossRate( 0.0 ),
			NoLoadLossEnergy( 0.0 ),
			LoadLossRate( 0.0 ),
			LoadLossEnergy( 0.0 ),
			ThermalLossRate( 0.0 ),
			ThermalLossEnergy( 0.0 ),
			ElecUseUtility( 0.0 ),
			ElecProducedCoGen( 0.0 ),
			QdotConvZone( 0.0 ),
			QdotRadZone( 0.0 )
		{}

	};

	struct WholeBuildingElectricPowerSummary
	{
		// Members
		std::string Name;
		Real64 ElectricityProd; // Current Electric Produced from Equipment (J)
		Real64 ElectProdRate; // Current Electric Production Rate from Equipment (W)
		Real64 ElectricityPurch; // Current Purchased Electric (J)
		Real64 ElectPurchRate; // Current Electric Purhcased Rate (W)
		Real64 ElectSurplusRate; // Current excess power (W)
		Real64 ElectricitySurplus; // Current excess energy (J)
		Real64 ElectricityNetRate; // Net elect rate, + is Purchased, - is Surplus (W)
		Real64 ElectricityNet; // Net energy, + is Purchased, - is Surplus (J)
		Real64 TotalBldgElecDemand; // Current Total Building Electric Demand (W)
		Real64 TotalHVACElecDemand; // Current Total HVAC Electric Demand (W)
		Real64 TotalElectricDemand; // Current Total Electric Demand (W)
		Real64 ElecProducedPVRate; // Current Rate of PV Produced from the Arrays (W)
		Real64 ElecProducedWTRate; // Current Rate of Wind Turbine Produced (W)
		Real64 ElecProducedStorageRate; // Current Rate of power to(-)/from(+) storage (W)

		// Default Constructor
		WholeBuildingElectricPowerSummary() :
			Name( "Whole Building" ),
			ElectricityProd( 0.0 ),
			ElectProdRate( 0.0 ),
			ElectricityPurch( 0.0 ),
			ElectPurchRate( 0.0 ),
			ElectSurplusRate( 0.0 ),
			ElectricitySurplus( 0.0 ),
			ElectricityNetRate( 0.0 ),
			ElectricityNet( 0.0 ),
			TotalBldgElecDemand( 0.0 ),
			TotalHVACElecDemand( 0.0 ),
			TotalElectricDemand( 0.0 ),
			ElecProducedPVRate( 0.0 ),
			ElecProducedWTRate( 0.0 ),
			ElecProducedStorageRate( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< ElecStorageDataStruct > ElecStorage;
	extern Array1D< DCtoACInverterStruct > Inverter;
	extern Array1D< ElectricPowerLoadCenter > ElecLoadCenter; // dimension to number of machines
	extern Array1D< ElectricTransformer > Transformer;
	extern WholeBuildingElectricPowerSummary WholeBldgElectSummary;

	// Functions

	void
	clear_state();

	void
	ManageElectricLoadCenters(
		bool const FirstHVACIteration,
		bool & SimElecCircuits, // simulation convergence flag
		bool const UpdateMetersOnly // if true then don't resimulate generators, just update meters.
	);

	void
	GetPowerManagerInput();

	void
	GeneratorPowerOutput(
		int const LoadCenterNum, // Load Center number counter
		int const GenNum, // Generator number counter
		bool const FirstHVACIteration, // Unused 2010 JANUARY
		Real64 & ElectricPowerOutput, // Actual generator electric power output
		Real64 & ThermalPowerOutput // Actual generator thermal power output
	);

	void
	CalcLoadCenterThermalLoad(
		bool const FirstHVACIteration, // unused1208
		int const LoadCenterNum, // Load Center number counter
		Real64 & ThermalLoad // heat rate called for from cogenerator(watts)
	);

	void
	VerifyCustomMetersElecPowerMgr();

	void
	ManageInverter( int const LoadCenterNum ); // Load Center number counter

	void
	UpdateLoadCenterRecords( int const LoadCenterNum ); // Load Center index

	void
	UpdateWholeBuildingRecords();

	void
	FigureInverterZoneGains();

	//***********************************************************************************************************************************

	void
	ManageElectCenterStorageInteractions(
		int const LoadCenterNum, // load center number, index for structure
		Real64 & StorageDrawnPower, // Electric Power Draw Rate from storage units
		Real64 & StorageStoredPower // Electric Power Store Rate from storage units
	);

	//*****************************************************************************************************************

	bool
	determineCurrentForBatteryDischarge(
		Real64& curI0,
		Real64& curT0,
		Real64& curVolt,
		Real64 const Pw,
		Real64 const q0,
		int const CurveNum,
		Real64 const k,
		Real64 const c,
		Real64 const qmax,
		Real64 const E0c,
		Real64 const InternalR
	);

	void
	FigureElectricalStorageZoneGains();

	void
	ManageTransformers();

	void
	FigureTransformerZoneGains();

	void
	Rainflow(
		int const numbin, // numbin = constant value
		Real64 const input, // input = input value from other object (battery model)
		Array1A< Real64 > B1, // stores values of points, calculated here - stored for next timestep
		Array1A< Real64 > X, // stores values of two data point difference, calculated here - stored for next timestep
		int & count, // calculated here - stored for next timestep in main loop
		Array1A< Real64 > Nmb, // calculated here - stored for next timestep in main loop
		Array1A< Real64 > OneNmb, // calculated here - stored for next timestep in main loop
		int const dim // end dimension of array
	);

	void
	shift(
		Array1A< Real64 > A,
		int const m,
		int const n,
		Array1A< Real64 > B,
		int const dim // end dimension of arrays
	);

	//******************************************************************************************************
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

} // ManageElectricPower

} // EnergyPlus

#endif
