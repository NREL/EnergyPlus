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

#ifndef ElectricPowerServiceManager_hh_INCLUDED
#define ElectricPowerServiceManager_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>
#include <memory>

// ObjexxFCL Headers
//#include <ObjexxFCL/Array1.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <PlantLocation.hh>
#include <OutputProcessor.hh>
#include <DataHeatBalance.hh>
#include <EMSManager.hh>

namespace EnergyPlus {

namespace ElectricPowerService {

	enum thermalLossDestinationEnum {
		heatLossNotDetermined,
		zoneGains,
		lostToOutside
	};

	void
	initializeElectricPowerServiceZoneGains();



class DCtoACInverter
{
private: // Creation
	// Default Constructor
		DCtoACInverter() :
			aCPowerOut( 0.0 ),
			aCEnergyOut( 0.0 ),
			efficiency( 0.0 ),
			dCPowerIn( 0.0 ),
			dCEnergyIn( 0.0 ),
			thermLossRate( 0.0 ),
			thermLossEnergy( 0.0 ),
			qdotConvZone( 0.0 ),
			qdotRadZone( 0.0 ),
			ancillACuseRate( 0.0 ),
			ancillACuseEnergy( 0.0 ),
			name( ""),
			modelType( notYetSet ),
			availSchedPtr( 0 ),
			heatLossesDestination( heatLossNotDetermined ),
			zoneNum( 0 ),
			zoneRadFract( 0.0 ),
			nightTareLossPower( 0.0 ),
			nominalVoltage( 0.0 ),
			nomVoltEfficiencyARR( 6, 0.0 ),
			curveNum( 0 ),
			ratedPower( 0.0 ),
			minPower( 0.0 ),
			maxPower( 0.0 ),
			minEfficiency( 0.0 ),
			maxEfficiency( 0.0 ),
			standbyPower( 0.0 )

		{}

	// Copy Constructor
	DCtoACInverter( DCtoACInverter const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	DCtoACInverter( DCtoACInverter && ) = default;
#endif

public: // Methods

	// Destructor
	~DCtoACInverter()
	{}

	// Constructor
	DCtoACInverter(
		std::string objectName
	);

	void
	manageInverter( 
		Real64 const powerDCElectProductionRate,
		Real64 const powerDCElectStorageDrawRate
	); // Load Center number counter

	void
	reinitAtBeginEnvironment();

	void
	reinitZoneGainsAtBeginEnvironment();

	Real64
	getThermLossRate();

	Real64
	getACPowerOut();

	Real64
	getACEnergyOut();


private: //Methods

public: // data public for unit test
		Real64 aCPowerOut;
		Real64 aCEnergyOut;
		//results and reporting
		Real64 efficiency;
		Real64 dCPowerIn;

		Real64 dCEnergyIn;

		Real64 thermLossRate;
		Real64 thermLossEnergy;
		Real64 qdotConvZone;
		Real64 qdotRadZone;
		Real64 ancillACuseRate;
		Real64 ancillACuseEnergy;

private: // data
		enum inverterModelTypeEnum {
			notYetSet,
			cECLookUpTableModel,
			curveFuncOfPower,
			simpleConstantEff
		};

		std::string name; // user identifier
		inverterModelTypeEnum modelType; // type of inverter model used
		int availSchedPtr; // number for availability schedule.
		thermalLossDestinationEnum heatLossesDestination;
		int zoneNum; // destination zone for heat losses from inverter.
		Real64 zoneRadFract; // radiative fraction for thermal losses to zone
		Real64 nightTareLossPower; // CEC lookup table model
		Real64 nominalVoltage; // CEC lookup table model
		std::vector < Real64 > nomVoltEfficiencyARR; // eff at 10, 20, 30, 50, 75, & 100% CEC lookup table model
		int curveNum; // curve index for eff as func of power
		Real64 ratedPower; // rated, max continuous power output level for inverter
		Real64 minPower;
		Real64 maxPower;
		Real64 minEfficiency;
		Real64 maxEfficiency;
		Real64 standbyPower;


}; //DCtoACInverter

class ElectricStorage
{
private: // Creation
	// Default Constructor
	ElectricStorage() :
			storedPower( 0.0 ),
			storedEnergy( 0.0 ),
			drawnPower( 0.0 ),
			drawnEnergy( 0.0 ),
			decrementedEnergyStored( 0.0 ),
			maxRainflowArrayBounds( 100 ),
			maxRainflowArrayInc( 100 ),
			myWarmUpFlag( false ),
			name( "" ),
			storageModelMode( storageTypeNotSet ),
			availSchedPtr( 0 ),
			heatLossesDestination( heatLossNotDetermined ),
			zoneNum( 0 ),
			zoneRadFract( 0.0 ),
			startingEnergyStored( 0.0 ),
			energeticEfficCharge( 0.0 ),
			energeticEfficDischarge( 0.0 ),
			maxPowerDraw( 0.0 ),
			maxPowerStore( 0.0 ),
			maxEnergyCapacity( 0.0 ),
			parallelNum( 0 ),
			seriesNum( 0 ),
			numBattery( 0 ),
			chargeCurveNum( 0 ),
			dischargeCurveNum( 0 ),
			cycleBinNum( 0 ),
			startingSOC( 0.0 ),
			maxAhCapacity( 0.0 ),
			availableFrac( 0.0 ),
			chargeConversionRate( 0.0 ),
			chargedOCV( 0.0 ),
			dischargedOCV( 0.0 ),
			internalR( 0.0 ),
			maxDischargeI( 0.0 ),
			cutoffV( 0.0 ),
			maxChargeRate( 0.0 ),
			lifeCalculation( degredationNotSet ),
			lifeCurveNum( 0 ),
			thisTimeStepStateOfCharge( 0.0 ),
			lastTimeStepStateOfCharge( 0.0 ),
			pelNeedFromStorage( 0.0 ),
			pelFromStorage( 0.0 ),
			eMSOverridePelFromStorage( false ),
			eMSValuePelFromStorage( 0.0 ),
			pelIntoStorage( 0.0 ),
			eMSOverridePelIntoStorage( false ),
			eMSValuePelIntoStorage( 0.0 ),
			qdotConvZone( 0.0 ),
			qdotRadZone( 0.0 ),
			timeElapsed( 0.0 ),
			thisTimeStepAvailable( 0.0 ),
			thisTimeStepBound( 0.0 ),
			lastTimeStepAvailable( 0.0 ),
			lastTimeStepBound( 0.0 ),
			lastTwoTimeStepAvailable( 0.0 ),
			lastTwoTimeStepBound( 0.0 ),
			count0( 0 ),
			electEnergyinStorage( 0.0 ),

			thermLossRate( 0.0 ),
			thermLossEnergy( 0.0 ),
			storageMode( 0 ),
			absoluteSOC( 0.0 ),
			fractionSOC( 0.0 ),
			batteryCurrent( 0.0 ),
			batteryVoltage( 0.0 ),
			batteryDamage( 0.0 )
		{}


	// Copy Constructor
	ElectricStorage( ElectricStorage const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	ElectricStorage( ElectricStorage && ) = default;
#endif

public: //methods

	// Destructor
	~ElectricStorage()
	{}
	
	// Constructor
	ElectricStorage(
		std::string objectName
		// need object type
	);

	void
	manageElectCenterStorageInteractions(
		Real64 const powerDemand, // load center power demand minus any inverter losses that need to be applied
		Real64 const powerGenSupply, // sum of load center generator production
		Real64 & StorageDrawnPower, // Electric Power Draw Rate from storage units
		Real64 & StorageStoredPower // Electric Power Store Rate from storage units
	);

	void
	reinitAtBeginEnvironment();

	void
	reinitZoneGainsAtBeginEnvironment();

	Real64
	getDrawnPower();

	Real64
	getStoredPower();

	Real64
	getDrawnEnergy();

	Real64
	getStoredEnergy();

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

private: //methods

	void
	rainflow(
		int const numbin, // numbin = constant value
		Real64 const input, // input = input value from other object (battery model)
		std::vector < Real64 > B1, // stores values of points, calculated here - stored for next timestep
		std::vector < Real64 > X, // stores values of two data point difference, calculated here - stored for next timestep
		int & count, // calculated here - stored for next timestep in main loop
		std::vector < Real64 > Nmb, // calculated here - stored for next timestep in main loop
		std::vector < Real64 > OneNmb // calculated here - stored for next timestep in main loop
	//	int const dim // end dimension of array
	);


	void
	shift(
		std::vector < Real64 > A,
		int const m,
		int const n,
		std::vector < Real64 > B
	//	int const dim // end dimension of arrays
	);

public: //data public for unit tests
	Real64 storedPower; // [W]
	Real64 storedEnergy; // [J]
	Real64 drawnPower; // [W]
	Real64 drawnEnergy; // [J]
	Real64 decrementedEnergyStored; // [J] this is the negative of StoredEnergy

private: //data

	enum storageModelTypeEnum {
		storageTypeNotSet,
		simpleBucketStorage,
		kiBaMBattery
	};

	enum degredationModelTypeEnum {
		degredationNotSet,
		batteryLifeCalculationYes,
		batteryLifeCalculationNo
	};
	int maxRainflowArrayBounds;
	int const maxRainflowArrayInc = 100;
	bool myWarmUpFlag;
	std::string name; // name of this electrical storage module
	storageModelTypeEnum storageModelMode; // type of model parameter, SimpleBucketStorage
	int availSchedPtr; // availability schedule index.
	thermalLossDestinationEnum heatLossesDestination; // mode for where thermal losses go
	int zoneNum; // destination zone for heat losses from inverter.
	Real64 zoneRadFract; // radiative fraction for thermal losses to zone
	Real64 startingEnergyStored; // [J] joules inside at beginning of environment period
	Real64 energeticEfficCharge; // [ ] efficiency of charging
	Real64 energeticEfficDischarge; // [ ] efficiency of discharging
	Real64 maxPowerDraw; // [W] max rate of discharge
	Real64 maxPowerStore; // [W] max rate of charge
	Real64 maxEnergyCapacity; // [J] max storage capacity
	int parallelNum; // [ ] number of battery modules in parallel
	int seriesNum; // [ ] number of battery modules in series
	int numBattery; // total number of batteries all together
	int chargeCurveNum; // [ ] voltage change curve index number for charging
	int dischargeCurveNum; // [ ] voltage change curve index number for discharging
	int cycleBinNum; // [ ] number of cycle bins
	Real64 startingSOC; // [ ] initial fractional state of charge
	Real64 maxAhCapacity; // [Ah]maximum capacity
	Real64 availableFrac; // [ ] fraction of available charge capacity
	Real64 chargeConversionRate; // [1/h]change rate from bound charge energy to available charge
	Real64 chargedOCV; // [V] fully charged oppen circuit voltage
	Real64 dischargedOCV; // [V] fully discharged open circuit voltage
	Real64 internalR; // [ohm]internal electric resistance
	Real64 maxDischargeI; // [A] maximum discharging current
	Real64 cutoffV; // [V] cut-off voltage
	Real64 maxChargeRate; // [1/h]charge rate limit
	degredationModelTypeEnum lifeCalculation; // [ ]battery life calculation: Yes or No
	int lifeCurveNum; // [ ]battery life curve name index number
	//calculated and from elsewhere vars
	Real64 thisTimeStepStateOfCharge; // [J]
	Real64 lastTimeStepStateOfCharge; // [J]
	Real64 pelNeedFromStorage; // [W]
	Real64 pelFromStorage; // [W]
	bool eMSOverridePelFromStorage; // if true, EMS calling for override
	Real64 eMSValuePelFromStorage; // value EMS is directing to use, power from storage [W]
	Real64 pelIntoStorage; // [W]
	bool eMSOverridePelIntoStorage; // if true, EMS calling for override
	Real64 eMSValuePelIntoStorage; // value EMS is directing to use, power into storage [W]
	Real64 qdotConvZone; // [W]
	Real64 qdotRadZone; // [W]
	Real64 timeElapsed; // [h]
	Real64 thisTimeStepAvailable; // [Ah] available charge at the current timestep
	Real64 thisTimeStepBound; // [Ah] bound charge at the current timestep
	Real64 lastTimeStepAvailable; // [Ah] available charge at the previous timestep
	Real64 lastTimeStepBound; // [Ah] bound charge at the previous timestep
	Real64 lastTwoTimeStepAvailable; // [Ah] available charge at the previous two timesteps
	Real64 lastTwoTimeStepBound; // [Ah] bound charge at the previous two timesteps
	//battery life calculation variables
	int count0;
	std::vector < Real64 > b10;
	std::vector < Real64 > x0;
	std::vector < Real64 > nmb0;
	std::vector < Real64 > oneNmb0;
	//report
	Real64 electEnergyinStorage; // [J] state of charge


	Real64 thermLossRate; // [W]
	Real64 thermLossEnergy; // [J]
	int storageMode; // [ ] mode of operation 0 for idle, 1 for discharging, 2 for charging
	Real64 absoluteSOC; // [Ah] total state of charge
	Real64 fractionSOC; // [ ] fractional state of charge
	Real64 batteryCurrent; // [A] total current
	Real64 batteryVoltage; // [V] total voltage
	Real64 batteryDamage; // [ ] fractional battery damage

}; //ElectricStorage

class ElectricTransformer
{
private: // Creation
	// Default Constructor
		ElectricTransformer() :
			numLoadCenters( 0 ),
			name( " "),
			myOneTimeFlag( true ),
			availSchedPtr( 0 ),
			usageMode( useNotYetSet ),
			heatLossesDestination( heatLossNotDetermined ),
			zoneNum( 0 ),
			zoneRadFrac( 0.0 ),
			ratedCapacity( 0.0 ),
			phase( 0 ),
			factorTempCoeff( 0.0 ),
			tempRise( 0.0 ),
			eddyFrac( 0.0 ),
			performanceInputMode( perfInputMethodNotSet ),
			ratedEfficiency( 0.0 ),
			ratedPUL( 0.0 ),
			ratedTemp( 0.0 ),
			maxPUL( 0.0 ),
			considerLosses( true ),
			ratedNL( 0.0 ),
			ratedLL( 0.0 ),

			overloadErrorIndex( 0 ),
			efficiency( 0.0 ),
			powerIn( 0.0 ),
			energyIn( 0.0 ),
			powerOut( 0.0 ),
			energyOut( 0.0 ),
			noLoadLossRate( 0.0 ),
			noLoadLossEnergy( 0.0 ),
			loadLossRate( 0.0 ),
			loadLossEnergy( 0.0 ),
			thermalLossRate( 0.0 ),
			thermalLossEnergy( 0.0 ),
			elecUseUtility( 0.0 ),
			elecProducedCoGen( 0.0 ),
			qdotConvZone( 0.0 ),
			qdotRadZone( 0.0 )
		{}

	// Copy Constructor
	ElectricTransformer( ElectricTransformer const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	ElectricTransformer( ElectricTransformer && ) = default;
#endif



public: //methods

	// Destructor
	~ElectricTransformer()
	{}

	// Constructor
	ElectricTransformer(
		std::string objectName
	);

	void
	manageTransformers(
		Real64 const surplusPowerOutFromLoadCenters
	);

	void
	setupMeterIndices();

	void
	reinitAtBeginEnvironment();

	void
	reinitZoneGainsAtBeginEnvironment();

	void
	addLoadCenterIndex( 
		int const objectIndex
	);

	std::vector< int >
	getLoadCenterObjIndices();


private: //methods

public: 
	int numLoadCenters; // number of load centers served by the transformer
	std::vector < int > loadCenterObjIndexes; // index array of load centers served by the transformer
private: //data

	enum transformerUseEnum {
		useNotYetSet,
		powerInFromGrid, // condition power from grid going into building buss
		powerOutFromBldgToGrid, // condition power from building buss going out to grid
		powerFromLoadCenterToBldg // condition power from a load center going into building buss
	};
	enum transformerPerformanceInputEnum {
		perfInputMethodNotSet,
		lossesMethod,
		efficiencyMethod
	
	};

	std::string name; // user identifier
	bool myOneTimeFlag;
	int availSchedPtr; // availability schedule index.
	transformerUseEnum usageMode; // mode for transformer usage
	thermalLossDestinationEnum heatLossesDestination; // mode for where thermal losses go
	int zoneNum; // destination zone for heat losses from inverter.
	Real64 zoneRadFrac; // radiative fraction for thermal losses to zone
	Real64 ratedCapacity; // rated capacity [VA]
	int phase; // phase
	Real64 factorTempCoeff; // thermal coefficient of resistance for winding material
	Real64 tempRise; // full load temperature rise [C]
	Real64 eddyFrac; // fraction of eddy current losses []
	transformerPerformanceInputEnum performanceInputMode; // performance input method
	Real64 ratedEfficiency; // nameplate efficiency []
	Real64 ratedPUL; // per unit load for nameplate efficiency []
	Real64 ratedTemp; // reference temperature for nameplate efficiency [C]
	Real64 maxPUL; // per unit load for maximum efficiency []
	bool considerLosses; // if true, consider transformer lossses in metering
	std::vector < std::string > wiredMeterNames; // names of the meters wired to transformer
	std::vector < int > wiredMeterPtrs; // array of "pointers" to meters wired to transformer
	std::vector < bool > specialMeter; // indicates whether a meter needs special consideration
	// Electricity:Facility and Electricity:HVAC are two special
	// meters because tranformer loss is part of them
	//calculated and from elsewhere vars
	Real64 ratedNL; // rated no load losses, user input or calculated [W]
	Real64 ratedLL; // rated load losses, user input or calculated [W]


	int overloadErrorIndex; // used for warning message when transformer is overloaded
	//results and reporting
	Real64 efficiency; // transformer efficiency
	Real64 powerIn; // [W]
	Real64 energyIn; // [J]
	Real64 powerOut; // [W]
	Real64 energyOut; // [J]
	Real64 noLoadLossRate; // [W]
	Real64 noLoadLossEnergy; // [J]
	Real64 loadLossRate; // [W]
	Real64 loadLossEnergy; // [J]
	Real64 thermalLossRate; // [W]
	Real64 thermalLossEnergy; // [J]
	Real64 elecUseUtility; // [J] Energy consumption for a utility transformer (power in)
	// Positive values
	Real64 elecProducedCoGen; // [J] Energy consumption for a cogeneration transformer (power out)
	// Negative values
	Real64 qdotConvZone; // [W]
	Real64 qdotRadZone; // [W]


}; //ElectricTransformer

class GeneratorController
{
private: // Creation
	// Default Constructor
	GeneratorController() :
		name( "" ),
		typeOfName( "" ),
		compGenTypeOf_Num( 0 ),
		compPlantTypeOf_Num( 0 ),
		generatorType( generatorNotYetSet ),
		generatorIndex( 0 ),
		maxPowerOut( 0.0 ),
		availSched( " " ),
		availSchedPtr( 0 ),
		powerRequestThisTimestep( 0.0 ),
		onThisTimestep( false ),
		eMSPowerRequest( 0.0 ),
		eMSRequestOn( false ),
		plantInfoFound( false ),
		cogenLocation( PlantLocation( 0, 0, 0, 0 ) ),
		nominalThermElectRatio( 0.0 ),
		dCElectricityProd( 0.0 ),
		dCElectProdRate( 0.0 ),
		electricityProd( 0.0 ),
		electProdRate( 0.0 ),
		thermalProd( 0.0 ),
		thermalProdRate( 0.0 )
	{}

	// Copy Constructor
	GeneratorController( GeneratorController const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	GeneratorController( GeneratorController && ) = default;
#endif

public: // Methods
	// Destructor
	~GeneratorController()
	{}

	// Constructor
	GeneratorController(
		std::string objectName,
		std::string objectType,
		Real64 ratedElecPowerOutput,
		std::string availSchedName,
		Real64 thermalToElectRatio
	);

	void
	simGeneratorGetPowerOutput(
		bool const runFlag, //true if generator is on
		Real64 const myElecLoadRequest, //target electric power production request
		bool const FirstHVACIteration, // 
		Real64 & electricPowerOutput, // Actual generator electric power output
		Real64 & thermalPowerOutput // Actual generator thermal power output
	);

	void
	reinitAtBeginEnvironment();


private: //Methods


public: // data // might make this class a friend of ElectPowerLoadCenter?
	enum generatorTypeEnum {
		generatorNotYetSet,
		generatorICEngine,
		generatorCombTurbine,
		generatorPV,
		generatorFuelCell,
		generatorMicroCHP,
		generatorMicroturbine,
		generatorWindTurbine
	};


	std::string name; // user identifier
	std::string typeOfName; // equipment type
	int compGenTypeOf_Num; // Numeric designator for generator CompType (TypeOf), in DataGlobalConstants
	int compPlantTypeOf_Num; // numeric designator for plant component, in DataPlant
	generatorTypeEnum generatorType;
	int generatorIndex; // index in generator model data struct
	Real64 maxPowerOut; // Maximum Power Output (W)
	std::string availSched; // Operation Schedule.
	int availSchedPtr; // pointer to operation schedule
	Real64 powerRequestThisTimestep; // Current Demand on Equipment (W)
	bool onThisTimestep; // Indicator whether Generator on
	Real64 eMSPowerRequest; // EMS actuator for current demand on equipment (W)
	bool eMSRequestOn; // EMS actuating On if true.
	bool plantInfoFound;
	PlantLocation cogenLocation;
	Real64 nominalThermElectRatio; // Cogen: nominal ratio of thermal to elect production
	//results of component models for load center reporting
	Real64 dCElectricityProd; // Current DC Electric Produced from Equipment (J)
	Real64 dCElectProdRate; // Current DC Electric Production Rate from Equipment (W)
	Real64 electricityProd; // Current AC Electric Produced from Equipment (J)
	Real64 electProdRate; // Current AC Electric Production Rate from Equipment (W)
	Real64 thermalProd; // Current Thermal energy Produced from Equipment (J)
	Real64 thermalProdRate; // Current Thermal energy Production Rate from Equipment (W)
}; //class GeneratorController

class ElectPowerLoadCenter
{

private: // Creation
	// Default Constructor
	ElectPowerLoadCenter() :
		numGenerators( 0 ),
		bussType( bussNotYetSet ),
		electricityProd( 0.0 ),
		electProdRate( 0.0 ),
		thermalProd( 0.0 ),
		thermalProdRate( 0.0 ),
		inverterPresent( false ),
		inverterName( " "),
		electDemand( 0.0 ),
		name( ""),
		generatorListName( ""),
		genOperationScheme( genOpSchemeNotYetSet ),
		demandMeterPtr( 0 ),
		generatorsPresent( false ),

		myCoGenSetupFlag( true ),
		demandLimit( 0.0 ),
		trackSchedPtr( 0 ),


		dCElectricityProd( 0.0 ),
		dCElectProdRate( 0.0 ),
		dCpowerConditionLosses( 0.0 ),
		storagePresent( false ),
		storageName ( "" ),
		transformerPresent( false ),
		transformerName( "" ),


		totalPowerRequest( 0.0 ),
		totalThermalPowerRequest( 0.0 )

	{}

	// Copy Constructor
	ElectPowerLoadCenter( ElectPowerLoadCenter const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	ElectPowerLoadCenter( ElectPowerLoadCenter && ) = default;
#endif


public: // Methods

	// Destructor
	~ElectPowerLoadCenter()
	{}

	// Constructor
	ElectPowerLoadCenter(
		int const objectNum
	);

	void
	manageElecLoadCenter(
		bool const firstHVACIteration,
		Real64 & remainingPowerDemand
	);

	void
	setupLoadCenterMeterIndices();

	void
	reinitAtBeginEnvironment();

	void
	reinitZoneGainsAtBeginEnvironment();

	std::string
	getTransformerName();

	void
	updateLoadCenterRecords();

private: //Methods



	void
	calcLoadCenterThermalLoad(
		Real64 & thermalLoad // heat rate called for from cogenerator(watts)
	);

public: // data public for unit test
	enum electricBussTypeEnum {
		bussNotYetSet,
		aCBuss,
		dCBussInverter,
		aCBussStorage,
		dCBussInverterDCStorage,
		dCBussInverterACStorage
	};
	std::unique_ptr < ElectricStorage > storageObj;  
//	int storageModelNum; // simulation model parameter type
	int numGenerators; // Number of Generators
	std::vector < std::unique_ptr <GeneratorController> > elecGenCntrlObj; // generator controller objects
	electricBussTypeEnum bussType; // is this load center powered by AC or DC generators
	Real64 electricityProd; // Current AC Electric Produced from Equipment (J)
	Real64 electProdRate; // Current Electric Production Rate from Equipment (W)
	Real64 thermalProd; // Current Thermal energy Produced from Equipment (J)
	Real64 thermalProdRate; // Current Thermal energy Production Rate from Equipment (W)
	bool inverterPresent;
	std::string inverterName; // hold name for verificaton and error messages
	std::unique_ptr < DCtoACInverter > inverterObj;
	Real64 electDemand; // Current electric power demand on the load center (W)

private: // data
	enum generatorOpSchemeEnum {
		genOpSchemeNotYetSet,
		genOpSchemeBaseLoad,
		genOpSchemeDemandLimit,
		genOpSchemeTrackElectrical,
		genOpSchemeTrackSchedule,
		genOpSchemeTrackMeter,
		genOpSchemeThermalFollow,
		genOpSchemeThermalFollowLimitElectrical
	};



	std::string name; // user identifier
	std::string generatorListName; // List name of available generators
	generatorOpSchemeEnum genOperationScheme; // Name of Operation Scheme
	std::string demandMeterName; // Name of Demand Energy Meter for "on demand" operation
	int demandMeterPtr; // "pointer" to Meter for electrical Demand to meet
	std::string generationMeterName; // Name of Generated Energy Meter for "on demand" operation
	bool generatorsPresent; // true if any generators
//	int numGenerators; // Number of Generators
//	std::vector < std::unique_ptr <GeneratorController> > elecGenCntrlObj; // generator controller objects
	bool myCoGenSetupFlag;
	Real64 demandLimit; // Demand Limit in Watts(W) which the generator will operate above
	int trackSchedPtr; // "pointer" to schedule for electrical demand to meet.


//	int inverterModelNum; // simulation model parameter type
	Real64 dCElectricityProd; // Current DC Elect produced (J) (if buss type DCbussInverter)
	Real64 dCElectProdRate; // Current DC Elect power produced (W) (if buss type DCbussInverter)
	Real64 dCpowerConditionLosses; // current DC to AC inverter losses (W) (if DCbussInverter)
	bool storagePresent;
	std::string storageName; // hold name for verificaton and error messages


	bool transformerPresent; // should only be transformers for on-site load center, not facility service 
	std::string transformerName; // hold name for verificaton and error messages
	std::unique_ptr < ElectricTransformer > transformerObj;


	Real64 totalPowerRequest; // Total electric power request from the load center (W)
	Real64 totalThermalPowerRequest; // Total thermal power request from the load center (W)


}; //class ElectPowerLoadCenter

class ElectricPowerServiceManager // 
{

public: // Creation

	// Default Constructor
	ElectricPowerServiceManager() :
			newEnvironmentInternalGainsFlag( true ),
			numElecStorageDevices( 0 ),
			getInputFlag( true ),
			newEnvironmentFlag( true ),
			numLoadCenters( 0 ),
			numTransformers( 0 ),
			setupMeterIndexFlag( true ),
			elecFacilityIndex( 0 ),
			elecProducedCoGenIndex( 0 ),
			elecProducedPVIndex( 0 ),
			elecProducedWTIndex( 0 ),
			elecProducedStorageIndex( 0 ),
			name( "Whole Building" ),
			facilityPowerInTransformerPresent( false ),
			facilityPowerInTransformerName( "" ),
			numPowerOutTransformers( 0 ),
			wholeBldgRemainingLoad( 0.0 ),
			electricityProd( 0.0 ),
			electProdRate( 0.0 ),
			electricityPurch( 0.0 ),
			electPurchRate( 0.0 ),
			electSurplusRate( 0.0 ),
			electricitySurplus( 0.0 ),
			electricityNetRate( 0.0 ),
			electricityNet( 0.0 ),
			totalBldgElecDemand( 0.0 ),
			totalHVACElecDemand( 0.0 ),
			totalElectricDemand( 0.0 ),
			elecProducedPVRate( 0.0 ),
			elecProducedWTRate( 0.0 ),
			elecProducedStorageRate( 0.0 )
		{}
	// Copy Constructor
	ElectricPowerServiceManager( ElectricPowerServiceManager const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	ElectricPowerServiceManager( ElectricPowerServiceManager && ) = default;
#endif

public: // Methods

	// Destructor
	~ElectricPowerServiceManager()
	{}

	void
	manageElectricPowerService(
		bool const FirstHVACIteration,
		bool & SimElecCircuits, // simulation convergence flag
		bool const UpdateMetersOnly // if true then don't resimulate generators, just update meters.
	);

	void
	reinitZoneGainsAtBeginEnvironment();

	void
	verifyCustomMetersElecPowerMgr();

private: //Methods
	void
	getPowerManagerInput();

	void
	setupMeterIndices();

	void
	reinitAtBeginEnvironment();

	void
	updateWholeBuildingRecords();

	void
	reportPVandWindCapacity();

	void
	sumUpNumberOfStorageDevices();

public: // data
	bool newEnvironmentInternalGainsFlag;
	int numElecStorageDevices;
	std::vector< std::unique_ptr < ElectPowerLoadCenter > > elecLoadCenterObjs;

private: // data
	bool getInputFlag; // control if object needs to get input and call factory methods
	bool newEnvironmentFlag; //control if object needs to reinit at beginning of a new environment period
	int numLoadCenters;

	int numTransformers;
	bool setupMeterIndexFlag;  // control if object needs to make calls to GetMeterIndex
	int elecFacilityIndex;
	int elecProducedCoGenIndex;
	int elecProducedPVIndex;
	int elecProducedWTIndex;
	int elecProducedStorageIndex;
	std::string name;

	bool facilityPowerInTransformerPresent;
	std::string facilityPowerInTransformerName; // hold name for verificaton and error messages
	std::unique_ptr < ElectricTransformer > facilityPowerInTransformerObj;
	int numPowerOutTransformers;
	std::vector< std::string > powerOutTransformerNames;
	std::vector< std::unique_ptr < ElectricTransformer > > powerOutTransformerObjs;

	Real64 wholeBldgRemainingLoad;
	Real64 electricityProd; // Current Electric Produced from Equipment (J)
	Real64 electProdRate; // Current Electric Production Rate from Equipment (W)
	Real64 electricityPurch; // Current Purchased Electric (J)
	Real64 electPurchRate; // Current Electric Purhcased Rate (W)
	Real64 electSurplusRate; // Current excess power (W)
	Real64 electricitySurplus; // Current excess energy (J)
	Real64 electricityNetRate; // Net elect rate, + is Purchased, - is Surplus (W)
	Real64 electricityNet; // Net energy, + is Purchased, - is Surplus (J)
	Real64 totalBldgElecDemand; // Current Total Building Electric Demand (W)
	Real64 totalHVACElecDemand; // Current Total HVAC Electric Demand (W)
	Real64 totalElectricDemand; // Current Total Electric Demand (W)
	Real64 elecProducedPVRate; // Current Rate of PV Produced from the Arrays (W)
	Real64 elecProducedWTRate; // Current Rate of Wind Turbine Produced (W)
	Real64 elecProducedStorageRate; // Current Rate of power to(-)/from(+) storage

	Real64 pvTotalCapacity; // for LEED report, total installed PV capacity
	Real64 windTotalCapacity; // for LEED report, total installed wind capacity

}; // class ElectricPowerServiceManager

	extern std::unique_ptr< ElectricPowerService::ElectricPowerServiceManager > facilityElectricServiceObj;

	void
	createFacilityElectricPowerServiceObject();

	void
	clear_state();

} // ElectricPowerService namespace


} // EnergyPlus namespace
#endif //ElectricPowerServiceManager_hh_INCLUDED
