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
#include <vector>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1.hh>

// EnergyPlus Headers
#include <ElectricPowerServiceManager.hh>
#include <PlantLocation.hh>
#include <OutputProcessor.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <UtilityRoutines.hh>
#include <InputProcessor.hh>
#include <DataIPShortCuts.hh>
#include <ScheduleManager.hh>
#include <CurveManager.hh>
#include <DataHeatBalance.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <DataGlobalConstants.hh>
#include <ICEngineElectricGenerator.hh>
#include <CTElectricGenerator.hh>
#include <Photovoltaics.hh>
#include <FuelCellElectricGenerator.hh>
#include <MicroCHPElectricGenerator.hh>
#include <MicroturbineElectricGenerator.hh>
#include <WindTurbine.hh>
#include <DataPlant.hh>
#include <OutputReportPredefined.hh>

namespace EnergyPlus {



namespace ElectricPowerService {

	std::unique_ptr< ElectricPowerService::ElectricPowerServiceManager > facilityElectricServiceObj;

	void
	clear_state()
	{
		facilityElectricServiceObj.release();
	}

	void
	createFacilityElectricPowerServiceObject()
	{
		facilityElectricServiceObj = std::unique_ptr< ElectricPowerServiceManager >( new ElectricPowerServiceManager() );
	}

	void
	initializeElectricPowerServiceZoneGains() // namespace routine for handling call from InternalHeatGains
	{
		//internal zone gains need to be re initialized for begin new environment earlier than the main call into manage electric power service
		if ( facilityElectricServiceObj->newEnvironmentInternalGainsFlag && DataGlobals::BeginEnvrnFlag ) {
			facilityElectricServiceObj->reinitZoneGainsAtBeginEnvironment();
			facilityElectricServiceObj->newEnvironmentInternalGainsFlag =  false;
		}
		if ( ! DataGlobals::BeginEnvrnFlag ) {
			facilityElectricServiceObj->newEnvironmentInternalGainsFlag = true;
		}

	}

	void
	ElectricPowerServiceManager::manageElectricPowerService(
		bool const firstHVACIteration,
		bool & SimElecCircuits, // simulation convergence flag
		bool const UpdateMetersOnly // if true then don't resimulate generators, just update meters.
	)
	{
		if ( this->getInputFlag ) {
			this->getPowerManagerInput();
			this->getInputFlag = false;
		}

		if ( DataGlobals::MetersHaveBeenInitialized && this-> setupMeterIndexFlag ) {
			this->setupMeterIndices();
			this->setupMeterIndexFlag = false;
		}

		if ( DataGlobals::BeginEnvrnFlag && this->newEnvironmentFlag ) {
			this->reinitAtBeginEnvironment();
			this->newEnvironmentFlag = false;
		}
		if ( ! DataGlobals::BeginEnvrnFlag ) this->newEnvironmentFlag = true;

		// retrieve data from meters for demand and production
		this->totalBldgElecDemand = GetInstantMeterValue( this->elecFacilityIndex, 1 ) / DataGlobals::TimeStepZoneSec;
		this->totalHVACElecDemand = GetInstantMeterValue( this->elecFacilityIndex, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->totalElectricDemand = this->totalBldgElecDemand + this->totalHVACElecDemand;
		this->elecProducedPVRate = GetInstantMeterValue( this->elecProducedPVIndex, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedWTRate = GetInstantMeterValue( this->elecProducedWTIndex, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedStorageRate = GetInstantMeterValue( this->elecProducedStorageIndex, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

		this->wholeBldgRemainingLoad = this->totalElectricDemand;

		if ( UpdateMetersOnly ) { // just update record keeping, don't resimulate load centers
			if ( this->facilityPowerInTransformerPresent ) {
				this->facilityPowerInTransformerObj->manageTransformers( 0.0 );
			}

			if ( this->numPowerOutTransformers > 0 ) {
				for (auto loopPowerOutTransformers = 0; loopPowerOutTransformers < this->numPowerOutTransformers; ++loopPowerOutTransformers) {
					//determine surplus production from the load centers connected to this transformer
//					auto loadCenters = this->powerOutTransformerObjs[ loopPowerOutTransformers ]->getLoadCenterObjIndices;
//					for (std::size_t loopLoadCenters = 0; loopLoadCenters < loadCenters.size; ++loopLoadCenters){
	// this needs to be from takin the total surplus out to utility grid and partitioning out what fraction of that is from the load centers that point to this transformer.  TODO
	// legacy code is not correct. defer for now. 

	//					this->elecLoadCenterObjs[ loadCenters[ loopLoadCenters ] ]->
//					}

				}
			}

			this->updateWholeBuildingRecords();
			return;
		}

		for ( auto loopElecLoadCenter= 0; loopElecLoadCenter< this->numLoadCenters; ++loopElecLoadCenter ){
			this->elecLoadCenterObjs[ loopElecLoadCenter ]->manageElecLoadCenter( firstHVACIteration, this->wholeBldgRemainingLoad );
		}

		// The transformer call should be put outside of the "Load Center" loop because
		// 1) A transformer may be for utility, not for load center
		// 2) A tansformer may be shared by multiple load centers
		if ( this->facilityPowerInTransformerPresent ) {
			this->facilityPowerInTransformerObj->manageTransformers( 0.0 );
		}
		if ( this->numPowerOutTransformers >0 ){

			for (auto loopPowerOutTransformers = 0; loopPowerOutTransformers < this->numPowerOutTransformers; ++loopPowerOutTransformers) {
				Real64 surplusPower = 0.0;
				if ( this->powerOutTransformerObjs[ loopPowerOutTransformers ]->numLoadCenters > 0 ) {
					for (auto loopLoadCenters = 0; loopLoadCenters < this->powerOutTransformerObjs[ loopPowerOutTransformers ]->numLoadCenters; ++loopLoadCenters) {
						int thisLoadCenterIndex = this->powerOutTransformerObjs[ loopPowerOutTransformers ]->loadCenterObjIndexes[ loopLoadCenters ];
						surplusPower += max( this->elecLoadCenterObjs[ thisLoadCenterIndex ]->electProdRate - this->elecLoadCenterObjs[ thisLoadCenterIndex ]->electDemand, 0.0);

					}
				
				}

				this->powerOutTransformerObjs[ loopPowerOutTransformers ]->manageTransformers( surplusPower );

			}
		}

		this->updateWholeBuildingRecords();

		// Need to simulate through the Elec Manager at least twice to ensure that Heat Recovery information is included.
		// recheck this, may not be needed now that load centers are called more often.
		//  Does the IF condition also need to check if any thermal following strategies have been specified?
		//  That is, if only electrical following schemes, don't need to resimulate?
		if ( firstHVACIteration ) {
			SimElecCircuits = true;
		} else {
			SimElecCircuits = false;
		}
	}

	void
	ElectricPowerServiceManager::reinitZoneGainsAtBeginEnvironment()
	{
		if ( this->facilityPowerInTransformerPresent ) {
			this->facilityPowerInTransformerObj->reinitZoneGainsAtBeginEnvironment();
		}
		if ( this->numPowerOutTransformers > 0 ) {
			for ( auto loop = 0; loop < this->numPowerOutTransformers; ++loop ) {
				this->powerOutTransformerObjs[ loop ]->reinitZoneGainsAtBeginEnvironment();
			}
		}
		if ( this->numLoadCenters > 0 ) {
			for ( auto loop = 0; loop < this->numLoadCenters; ++loop ) {
				this->elecLoadCenterObjs[ loop ]->reinitZoneGainsAtBeginEnvironment();
			}
		}
	}

	void
	ElectricPowerServiceManager::getPowerManagerInput()
	{
	std::string const routineName = "ElectricPowerServiceManager  getPowerManagerInput ";

	this->numLoadCenters = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Distribution" );

	if ( this->numLoadCenters > 0 ){
		for ( auto iLoadCenterNum = 1; iLoadCenterNum <= this->numLoadCenters; ++iLoadCenterNum ){
			// call Electric Power Load Center constructor, in place
			this->elecLoadCenterObjs.emplace_back( new ElectPowerLoadCenter ( iLoadCenterNum) );
		}
	
	} else {
// TODO check that the distribution object isn't just missing but is needed. #issue 4639
	

			// if user input did not include an Electric Load center, create a simple default one here for reporting purposes
		//   but only if there are any other electricity components set up (yet) for metering
		int anyElectricityPresent = GetMeterIndex( "ELECTRICITY:FACILITY" );
		if ( anyElectricityPresent > 0 ) {
			this->elecLoadCenterObjs.emplace_back( new ElectPowerLoadCenter ( 0 ) );
			this->numLoadCenters = 1;
		}
	}

	// see if there is a transformer of the type powerInFromGrid
	this->numTransformers = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Transformer" );

	if ( this->numTransformers > 0 ) {
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array
		int iOStat; // IO Status when calling get input subroutine
		int facilityPowerInTransformerIDFObjNum = 0;
		bool foundInFromGridTransformer = false;
//		bool foundPowerOutFromOnsiteTransformer = false;

		DataIPShortCuts::cCurrentModuleObject =  "ElectricLoadCenter:Transformer";
		for ( auto loopTransformer = 1; loopTransformer <= this->numTransformers; ++loopTransformer) {
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, loopTransformer, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, iOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "PowerInFromGrid" ) ) {
				if ( ! foundInFromGridTransformer ) {
					foundInFromGridTransformer = true;
					facilityPowerInTransformerIDFObjNum = loopTransformer;
					this->facilityPowerInTransformerName = DataIPShortCuts::cAlphaArgs( 1 );
					this->facilityPowerInTransformerPresent = true;
				} else {
					// should only have one transformer in input that is PowerInFromGrid
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					ShowContinueError("Only one transformer with Usage PowerInFromGrid can be used, first one in input file will be used and the simulation continues...");
				}
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "PowerOutFromOnsiteGeneration" ) ) {
				++this->numPowerOutTransformers;
				this->powerOutTransformerNames.push_back( DataIPShortCuts::cAlphaArgs( 1 ) ) ;
				this->powerOutTransformerObjs.emplace_back( new ElectricTransformer ( DataIPShortCuts::cAlphaArgs( 1 ) ) );
			}
		}
		if ( foundInFromGridTransformer ) {
			//call transformer constructor
			facilityPowerInTransformerObj = std::unique_ptr < ElectricTransformer > ( new ElectricTransformer ( this->facilityPowerInTransformerName  ) );
		}
	} // if transformers

	// loop over power out transformers and electric load centers and register load center object index
	for (auto loopPowerOutTransformers = 0; loopPowerOutTransformers < this->numPowerOutTransformers; ++loopPowerOutTransformers ) {

		for ( auto loopElectLoadCenters = 0; loopElectLoadCenters < this->numLoadCenters; ++loopElectLoadCenters ) {
			if (InputProcessor::SameString( this->powerOutTransformerNames[ loopPowerOutTransformers ], this->elecLoadCenterObjs[ loopElectLoadCenters ]->getTransformerName() ) ){
				this->powerOutTransformerObjs[ loopPowerOutTransformers ]->addLoadCenterIndex( loopElectLoadCenters );
			}
		}
	}

	if ( this->numLoadCenters > 0 ) { 
		SetupOutputVariable( "Facility Total Purchased Electric Power [W]", this->electPurchRate, "System", "Average", this->name );
		SetupOutputVariable( "Facility Total Purchased Electric Energy [J]", this->electricityPurch, "System", "Sum", this->name, _, "ElectricityPurchased", "COGENERATION", _, "Plant" );

		SetupOutputVariable( "Facility Total Surplus Electric Energy [J]", this->electricitySurplus, "System", "Sum", this->name, _, "ElectricitySurplusSold", "COGENERATION", _, "Plant" );

		SetupOutputVariable( "Facility Net Purchased Electric Power [W]", this->electricityNetRate, "System", "Average", this->name );
		SetupOutputVariable( "Facility Net Purchased Electric Energy [J]", this->electricityNet, "System", "Sum", this->name, _, "ElectricityNet", "COGENERATION", _, "Plant" );

		SetupOutputVariable( "Facility Total Building Electric Demand Power [W]", this->totalBldgElecDemand, "System", "Average", this->name );
		SetupOutputVariable( "Facility Total HVAC Electric Demand Power [W]", this->totalHVACElecDemand, "System", "Average", this->name );
		SetupOutputVariable( "Facility Total Electric Demand Power [W]", this->totalElectricDemand, "System", "Average", this->name );

		SetupOutputVariable( "Facility Total Produced Electric Power [W]", this->electProdRate, "System", "Average", this->name );
		SetupOutputVariable( "Facility Total Produced Electric Energy [J]", this->electricityProd, "System", "Sum", this->name );

		this->reportPVandWindCapacity();
	}


	}


	void
	ElectricPowerServiceManager::setupMeterIndices()
	{
		this->elecFacilityIndex        = EnergyPlus::GetMeterIndex( "Electricity:Facility" );
		this->elecProducedCoGenIndex   = EnergyPlus::GetMeterIndex( "Cogeneration:ElectricityProduced" );
		this->elecProducedPVIndex      = EnergyPlus::GetMeterIndex( "Photovoltaic:ElectricityProduced" );
		this->elecProducedWTIndex      = EnergyPlus::GetMeterIndex( "WindTurbine:ElectricityProduced" );
		this->elecProducedStorageIndex = EnergyPlus::GetMeterIndex( "ElectricStorage:ElectricityProduced" );

		if ( this->numLoadCenters > 0 ){
			for( auto loopLoadCenters = 0; loopLoadCenters < this->numLoadCenters; ++loopLoadCenters ){
				elecLoadCenterObjs[ loopLoadCenters ]->setupLoadCenterMeterIndices();
			}
		}
		if (this->facilityPowerInTransformerPresent ) {
			facilityPowerInTransformerObj->setupMeterIndices();
		}
	}

	void
	ElectricPowerServiceManager::reinitAtBeginEnvironment()
	{
		this->wholeBldgRemainingLoad  = 0.0;
		this->electricityProd         = 0.0;
		this->electProdRate           = 0.0;
		this->electricityPurch        = 0.0;
		this->electPurchRate          = 0.0;
		this->electSurplusRate        = 0.0;
		this->electricitySurplus      = 0.0;
		this->electricityNetRate      = 0.0;
		this->electricityNet          = 0.0;
		this->totalBldgElecDemand     = 0.0;
		this->totalHVACElecDemand     = 0.0;
		this->totalElectricDemand     = 0.0;
		this->elecProducedPVRate      = 0.0;
		this->elecProducedWTRate      = 0.0;
		this->elecProducedStorageRate = 0.0;

		if ( this->numLoadCenters > 0 ){
			for( auto loopLoadCenters = 0; loopLoadCenters < this->numLoadCenters; ++loopLoadCenters ){
				elecLoadCenterObjs[ loopLoadCenters ]->reinitAtBeginEnvironment();
			}
		}
		if ( this->facilityPowerInTransformerPresent ) {
			facilityPowerInTransformerObj->reinitAtBeginEnvironment();
		}
		if ( this->numPowerOutTransformers > 0 ) {
			for (auto loopOutTransformers = 0; loopOutTransformers < this->numPowerOutTransformers ; ++loopOutTransformers ) {
				this->powerOutTransformerObjs[ loopOutTransformers ]->reinitAtBeginEnvironment();
			}
		}
	}

	void
	ElectricPowerServiceManager::verifyCustomMetersElecPowerMgr()
	{
		for ( std::size_t loop = 0; loop < this->elecLoadCenterObjs.size(); ++loop ) {
			this->elecLoadCenterObjs[ loop ]->setupLoadCenterMeterIndices();
		}
	}

	void
	ElectricPowerServiceManager::updateWholeBuildingRecords()
	{
		this->electricityProd = GetInstantMeterValue( this->elecProducedCoGenIndex, 2 ) + ( this->elecProducedPVRate + this->elecProducedWTRate + this->elecProducedStorageRate ) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; //whole building
		this->electProdRate = this->electricityProd / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour ); //whole building

		//Report the Total Electric Power Purchased [W], If negative then there is extra power to be sold or stored.
		this->electPurchRate = this->totalElectricDemand - this->electProdRate;
		//Check this value against a tolerance to aid in reporting.
		if ( std::abs( this->electPurchRate ) < 0.0001 ) this->electPurchRate = 0.0;
		if ( this->electPurchRate < 0.0 ) this->electPurchRate = 0.0; // don't want negative purchased...

		//Report the Total Electric Energy Purchased [J]
		this->electricityPurch = this->electPurchRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//report the total electric surplus....
		this->electSurplusRate = this->electProdRate - this->totalElectricDemand;
		if ( std::abs( this->electSurplusRate ) < 0.0001 ) this->electSurplusRate = 0.0;
		if ( this->electSurplusRate < 0.0 ) this->electSurplusRate = 0.0; // don't want negative surplus

		this->electricitySurplus = this->electSurplusRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//report the net electricity , + is purchased, - is surplus
		this->electricityNetRate = this->totalElectricDemand - this->electProdRate;

		this->electricityNet = this->electricityNetRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
	}

	void
	ElectricPowerServiceManager::reportPVandWindCapacity()
	{
			// LEED report
		this->pvTotalCapacity = 0.0;
		this->windTotalCapacity = 0.0;
		for ( auto count = 0; count < this->numLoadCenters; ++count ) {
			if ( this->elecLoadCenterObjs[ count ]->numGenerators > 0 ) {
				for ( auto genCount = 0; genCount < this->elecLoadCenterObjs[ count ]->numGenerators; ++genCount ) {
					if ( this->elecLoadCenterObjs[ count ]->elecGenCntrlObj[ genCount ]->compTypeOf_Num == DataGlobalConstants::iGeneratorPV ) {
						pvTotalCapacity += this->elecLoadCenterObjs[ count ]->elecGenCntrlObj[ genCount ]->maxPowerOut;
					}
					if ( this->elecLoadCenterObjs[ count ]->elecGenCntrlObj[ genCount ]->compTypeOf_Num  == DataGlobalConstants::iGeneratorWindTurbine ) {
						windTotalCapacity += this->elecLoadCenterObjs[ count ]->elecGenCntrlObj[ genCount ]->maxPowerOut;
					}
				}
			}
		}
		//put in total capacity for PV and Wind for LEED report
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchLeedRenRatCap, "Photovoltaic", this->pvTotalCapacity / 1000, 2 );
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchLeedRenRatCap, "Wind", this->windTotalCapacity / 1000, 2 );

		//TODO, this approach is relying on the correct power output to have been placed in the Generator list.  There could be a difference between this control input and the actual size of the systems as defined as generators.

	}

	ElectPowerLoadCenter::ElectPowerLoadCenter( // constructor
		int const objectNum
	)
	{
		// initialize 
		this->name="";
		this->generatorListName="";
		this->genOperationScheme = genOpSchemeNotYetSet ;
		this->demandMeterPtr = 0;
		this->generatorsPresent = false;
		this->numGenerators = 0;
		this->myCoGenSetupFlag = true;
		this->demandLimit = 0.0;
		this->trackSchedPtr = 0;
		this->bussType = bussNotYetSet;
		this->inverterPresent = false;
		this->dCElectricityProd = 0.0;
		this->dCElectProdRate = 0.0;
		this->dCpowerConditionLosses = 0.0;
		this->storagePresent = false;
		this->transformerPresent = false;
		this->electricityProd = 0.0;
		this->electProdRate = 0.0;
		this->thermalProd = 0.0;
		this->thermalProdRate = 0.0;
		this->totalPowerRequest = 0.0;
		this->totalThermalPowerRequest = 0.0;
		this->electDemand = 0.0;

		std::string const routineName = "ElectPowerLoadCenter constructor ";
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool errorsFound;

		DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Distribution";
		errorsFound = false;
		if ( objectNum > 0 ) {
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, objectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name          = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?

			this->generatorListName = DataIPShortCuts::cAlphaArgs( 2 );

			//Load the Generator Control Operation Scheme
			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "Baseload" ) ) {
				this->genOperationScheme = genOpSchemeBaseLoad;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "DemandLimit" ) ) {
				this->genOperationScheme = genOpSchemeDemandLimit;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "TrackElectrical" ) ) {
				this->genOperationScheme = genOpSchemeTrackElectrical;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "TrackSchedule" ) ) {
				this->genOperationScheme = genOpSchemeTrackSchedule;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "TrackMeter" ) ) {
				this->genOperationScheme =  genOpSchemeTrackMeter;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "FollowThermal" ) ) {
				this->genOperationScheme = genOpSchemeThermalFollow;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "FollowThermalLimitElectrical" ) ) {
				this->genOperationScheme =  genOpSchemeThermalFollowLimitElectrical;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
				errorsFound = true;
			}

			this->demandLimit = DataIPShortCuts::rNumericArgs( 1 );

			this->trackSchedPtr = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 4 ) );
			if ( ( this->trackSchedPtr == 0 ) && ( this->genOperationScheme == genOpSchemeTrackSchedule ) ) {
				if ( ! DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = " + DataIPShortCuts::cAlphaArgs( 4 ) );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = blank field." );
				}
				ShowContinueError( "Schedule not found; Must be entered and valid when Generator Operation Scheme=TrackSchedule" );
				errorsFound = true;
			}

			this->demandMeterName = InputProcessor::MakeUPPERCase( DataIPShortCuts::cAlphaArgs( 5 ) );
				// meters may not be "loaded" yet, defered check to later subroutine

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "AlternatingCurrent" ) ) {
				this->bussType = aCBuss;
				DataIPShortCuts::cAlphaArgs( 6 ) = "AlternatingCurrent";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "DirectCurrentWithInverter" ) ) {
				this->bussType  = dCBussInverter;
				this->inverterPresent = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "DirectCurrentWithInverter";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "AlternatingCurrentWithStorage" ) ) {
				this->bussType  = aCBussStorage;
				this->storagePresent = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "AlternatingCurrentWithStorage";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "DirectCurrentWithInverterDCStorage" ) ) {
				this->bussType  = dCBussInverterDCStorage;
				this->inverterPresent = true;
				this->storagePresent = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "DirectCurrentWithInverterDCStorage";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "DirectCurrentWithInverterACStorage" ) ) {
				this->bussType  = dCBussInverterACStorage;
				this->inverterPresent = true;
				this->storagePresent = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "DirectCurrentWithInverterACStorage";
			} else if ( DataIPShortCuts::cAlphaArgs( 6 ).empty() ) {
				this->bussType  = aCBuss;
				DataIPShortCuts::cAlphaArgs( 6 ) = "AlternatingCurrent (field was blank)";
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 6 ) + " = " + DataIPShortCuts::cAlphaArgs( 6 ) );
				errorsFound = true;
			}

			if ( this->inverterPresent ) {
				if ( !  DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
					this->inverterName = DataIPShortCuts::cAlphaArgs( 7 );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( DataIPShortCuts::cAlphaFieldNames( 7 ) + " is blank, but buss type requires inverter.");
					errorsFound = true;
				}
			}

			if ( this->storagePresent ) {
				if ( ! DataIPShortCuts::lAlphaFieldBlanks( 8 ) ) {
					this->storageName = DataIPShortCuts::cAlphaArgs( 8 );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( DataIPShortCuts::cAlphaFieldNames( 8 ) + " is blank, but buss type requires storage.");
					errorsFound = true;
				}
			}

			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 9 ) ) {
				// process transformer
				this->transformerName = DataIPShortCuts::cAlphaArgs( 9 );
				// only transformers of use type powerFromLoadCenterToBldg are really held in a load center, The legacy applications for transformers are held at the higher Electric service level
		//TODO add handling of Load center to building buss transfers
				this->transformerPresent =  true;
			}
		}

		// now that we are done with processing get input for ElectricLoadCenter:Distribution we can call child input objects without IP shortcut problems
		DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Generators";
		int genListObjectNum = InputProcessor::GetObjectItemNum( DataIPShortCuts::cCurrentModuleObject, this->generatorListName );
		if ( genListObjectNum > 0 ){
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, objectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			//Calculate the number of generators in list
			this->numGenerators = numNums / 2; // note IDD needs Min Fields = 6  
			if ( mod( ( numAlphas - 1 + numNums ), 5 ) != 0 ) ++this->numGenerators;
			int alphaCount = 2;
			for ( auto genCount = 1; genCount <= this->numGenerators; ++genCount) {
				// call constructor in place
				this->generatorsPresent = true;
				this->elecGenCntrlObj.emplace_back( new GeneratorController ( DataIPShortCuts::cAlphaArgs( alphaCount ), DataIPShortCuts::cAlphaArgs( alphaCount + 1 ), DataIPShortCuts::rNumericArgs( 2 * genCount - 1 ), DataIPShortCuts::cAlphaArgs( alphaCount + 2 ), DataIPShortCuts::rNumericArgs( 2 * genCount) ) );
				++alphaCount;
				++alphaCount;
				++alphaCount;
			}
		}

		if ( ! errorsFound && this->inverterPresent ) {
			// call inverter constructor
			this->inverterObj = std::unique_ptr < DCtoACInverter >  ( new DCtoACInverter( this->inverterName ) ) ;
		}

		if ( ! errorsFound && this->storagePresent ) {
			// call storage constructor 
			this->storageObj =  std::unique_ptr < ElectricStorage > ( new ElectricStorage( this->storageName ) );
		}

		if ( ! errorsFound && this->transformerPresent ) {
// TODO figure out if the transformer is of usage type LoadCenterProductionConditioning, new for v8.5
			
			//call transformer constructor 

			//this->transformerObj = std::unique_ptr < ElectricTransformer >( new ElectricTransformer (this->transformerName ) );
		}

		//Setup general output variables for reporting in the electric load center


		SetupOutputVariable( "Electric Load Center Produced Electric Power [W]", this->electProdRate, "System", "Average", this->name );

		SetupOutputVariable( "Electric Load Center Produced Electric Energy [J]", this->electricityProd, "System", "Sum", this->name );

		SetupOutputVariable( "Electric Load Center Produced Thermal Rate [W]", this->thermalProdRate, "System", "Average", this->name );

		SetupOutputVariable( "Electric Load Center Produced Thermal Energy [J]", this->thermalProd, "System", "Sum", this->name );

		SetupOutputVariable( "Electric Load Center Requested Electric Power [W]", this->totalPowerRequest, "System", "Average", this->name );


	}

	void
	ElectPowerLoadCenter::manageElecLoadCenter(
		bool const firstHVACIteration,
		Real64 & remainingWholePowerDemand
	)
	{
		this->totalPowerRequest = 0.0;
		this->totalThermalPowerRequest = 0.0;

			// Check Operation Scheme and assign power generation load
			// Both the Demand Limit and Track Electrical schemes will sequentially load the available generators.  All demand
			// not met by available generator capacity will be met by purchased electrical.
			// If a generator is needed in the simulation for a small load and it is less than the minimum part load ratio
			// the generator will operate at the minimum part load ratio and the excess will either reduce demand or
			// be available for storage or sell back to the power company.
		Real64 loadCenterElectricLoad = 0.0;
		Real64 remainingLoad = 0.0;
		Real64 electricProdRate = 0.0;
		Real64 thermalProdRate = 0.0;
		Real64 customMeterDemand = 0.0;

		if( this->generatorsPresent ) {

		switch ( this->genOperationScheme ) 
		{

		case genOpSchemeBaseLoad: {

			loadCenterElectricLoad = remainingWholePowerDemand;

			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {

				if ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 ) {
					// Set the Operation Flag
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
					// Set the electric generator load request
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;
				}

				// now handle EMS override
				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
					if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
					} else {
						this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					}
				}

				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep;
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining load
			}
			break;
		}
		case genOpSchemeDemandLimit: {
			// The Demand Limit scheme tries to have the generators meet all of the demand above the purchased Electric
			//  limit set by the user.
			remainingLoad = remainingWholePowerDemand - this->demandLimit;
			loadCenterElectricLoad = remainingLoad;

			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {

				if ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;

					// Set the electric generator load
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = min( this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->totalPowerRequest += max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
				} else {
					if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
						this->totalPowerRequest = min( loadCenterElectricLoad, this->totalPowerRequest );
					}
				}
				remainingLoad -= electricProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining load
			}
			break;
		}
		case genOpSchemeTrackElectrical: {
				//The Track Electrical scheme tries to have the generators meet all of the electrical demand for the building.
			remainingLoad = remainingWholePowerDemand;
			loadCenterElectricLoad = remainingLoad;

			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {

				if ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;

					// Set the electric generator load
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = min( this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;
					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->totalPowerRequest += max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
				} else {
					if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
						this->totalPowerRequest = min( loadCenterElectricLoad, this->totalPowerRequest );
					}
				}
				remainingLoad -= electricProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining load
			}
			break;
		}
		case genOpSchemeTrackSchedule: {
				// The Track Schedule scheme tries to have the generators meet the electrical demand determined from a schedule.
				//  Code is very similar to 'Track Electrical' except for initial RemainingLoad is replaced by SchedElecDemand
				//  and PV production is ignored.
			remainingLoad = ScheduleManager::GetCurrentScheduleValue( this->trackSchedPtr );
			loadCenterElectricLoad = remainingLoad;

			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {

				if ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;

					// Set the electric generator load
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = min( this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->totalPowerRequest += max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
				} else {
					if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
						this->totalPowerRequest = min( loadCenterElectricLoad, this->totalPowerRequest );
					}
				}
				remainingLoad -= electricProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining load
			}
			break;
		}
		case genOpSchemeTrackMeter: {
				// The TRACK CUSTOM METER scheme tries to have the generators meet all of the
				//   electrical demand from a meter, it can also be a user-defined Custom Meter
				//   and PV is ignored.
			customMeterDemand = GetInstantMeterValue( this->demandMeterPtr, 1 ) / DataGlobals::TimeStepZoneSec + GetInstantMeterValue( this->demandMeterPtr, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

			remainingLoad = customMeterDemand;
			loadCenterElectricLoad = remainingLoad;

			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {
				if ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
					// Set the electric generator load
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = min( this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;

					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->totalPowerRequest += max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
				} else {
					if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
						this->totalPowerRequest = min( loadCenterElectricLoad, this->totalPowerRequest );
					}
				}
				remainingLoad -= electricProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining load
			break;
		}
		case genOpSchemeThermalFollow: {
				// Turn thermal load into an electrical load for cogenerators controlled to follow heat loads
			Real64 remainingThermalLoad = 0.0;

			this->calcLoadCenterThermalLoad( remainingThermalLoad );
			Real64 loadCenterThermalLoad = remainingThermalLoad;
			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {

				if ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 && remainingThermalLoad > 0.0 ) {

					if ( this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio > 0.0 ) {
						remainingLoad = remainingThermalLoad / this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio;
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = min( this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut, remainingLoad );
						this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						// now handle EMS override
						if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
							this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
							if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
								this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
							} else {
								this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
							}
						}
					}
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;
					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->totalThermalPowerRequest += ( max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 ) ) * this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio;
					this->totalPowerRequest += ( max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 ) );
				} else {
					if ( this->totalThermalPowerRequest < loadCenterThermalLoad && this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						Real64 excessThermalPowerRequest = this->totalThermalPowerRequest + this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut * this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio - loadCenterThermalLoad;
						if ( excessThermalPowerRequest < 0.0 ) {
							this->totalThermalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut * this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio;
							this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
						} else {
							this->totalThermalPowerRequest = loadCenterThermalLoad;
							if ( this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio > 0.0 ) {
								this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut - ( excessThermalPowerRequest / this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio );
							}
						}
					}
				}
				remainingThermalLoad -= thermalProdRate; // Update remaining load to be met
				// by this load center
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining load
			}
			break;
		}
		case genOpSchemeThermalFollowLimitElectrical: {
			//  Turn a thermal load into an electrical load for cogenerators controlled to follow heat loads.
			//  Add intitialization of RemainingThermalLoad as in the ThermalFollow operating scheme above.
			Real64 remainingThermalLoad = 0.0;
			this->calcLoadCenterThermalLoad( remainingThermalLoad );
			// Total current electrical demand for the building is a secondary limit.
			remainingLoad = remainingWholePowerDemand;
			loadCenterElectricLoad = remainingWholePowerDemand;
			Real64 loadCenterThermalLoad = remainingThermalLoad;
			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {
				if ( ( ScheduleManager::GetCurrentScheduleValue( this->elecGenCntrlObj[ loopGenNum ]->availSchedPtr ) > 0.0 ) && ( remainingThermalLoad > 0.0 ) && ( remainingLoad > 0.0 ) ) {
					if ( this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio > 0.0 ) {
						remainingLoad = min( remainingWholePowerDemand, remainingThermalLoad / this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio );
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = min( this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut, remainingLoad );
						this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						// now handle EMS override
						if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
							this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
							if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
								this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
							} else {
								this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
							}
						}
					}
				} else {
					this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
					this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = 0.0;
					// now handle EMS override
					if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
						this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep = max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 );
						if ( this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = true;
						} else {
							this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep = false;
						}
					}
				}
				// Get generator's actual electrical and thermal power outputs
				this->elecGenCntrlObj[ loopGenNum ]->simGeneratorGetPowerOutput ( this->elecGenCntrlObj[ loopGenNum ]->onThisTimestep, this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep, firstHVACIteration, electricProdRate, thermalProdRate );

				if ( this->elecGenCntrlObj[ loopGenNum ]->eMSRequestOn ) {
					this->totalThermalPowerRequest += ( max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 ) ) * this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio;
					this->totalPowerRequest += ( max( this->elecGenCntrlObj[ loopGenNum ]->eMSPowerRequest, 0.0 ) );
				} else {
					if ( this->totalThermalPowerRequest < loadCenterThermalLoad && this->elecGenCntrlObj[ loopGenNum ]->powerRequestThisTimestep > 0.0 ) {
						Real64 excessThermalPowerRequest = this->totalThermalPowerRequest + this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut * this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio - loadCenterThermalLoad;
						if ( excessThermalPowerRequest < 0.0 ) {
							this->totalThermalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut * this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio;
							this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut;
						} else {
							this->totalThermalPowerRequest = loadCenterThermalLoad;
							if ( this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio > 0.0 ) {
								this->totalPowerRequest += this->elecGenCntrlObj[ loopGenNum ]->maxPowerOut - ( excessThermalPowerRequest / this->elecGenCntrlObj[ loopGenNum ]->nominalThermElectRatio );
							}
						}
						this->totalPowerRequest = min( loadCenterElectricLoad, this->totalPowerRequest );
					}
				}
				remainingThermalLoad -= thermalProdRate; // Update remaining thermal load to
				// be met by this load center
				remainingWholePowerDemand -= electricProdRate; // Update whole building remaining
				// electric load
			}
			break;
		}
		case genOpSchemeNotYetSet: {
			// This case allows for the reporting to be done without any generators specified.
		}
		} // end switch

		} // generators present

		this->electDemand = loadCenterElectricLoad; //To obtain the load for transformer

		Real64 storageDrawnPower = 0.0;
		Real64 storageStoredPower = 0.0;
		if ( ( this->storagePresent ) && ( this->bussType == dCBussInverterDCStorage ) ) {
			Real64 pcuLosses = 0.0;
			if ( this->inverterPresent  ) {
				// this will be lagged from last calc.
				pcuLosses = this->inverterObj->getThermLossRate();
			}
			Real64 powerDemand = this->totalPowerRequest + pcuLosses; 
	//TODO this legacy may be wrong, double counting ancillaries maybe?

			Real64 powerGenSupply = 0.0;
			for ( auto loopGen = 0; loopGen < this->numGenerators; ++loopGen ) {
				powerGenSupply += this->elecGenCntrlObj[ loopGen ]->dCElectProdRate;
			}
			this->storageObj->manageElectCenterStorageInteractions( powerDemand, powerGenSupply, storageDrawnPower, storageStoredPower );
			//     Adjust whole building electric demand based on storage inputs and outputs
			remainingWholePowerDemand = remainingWholePowerDemand - storageDrawnPower + storageStoredPower;
		}

		if ( this->inverterPresent ){
			this->dCElectProdRate = 0.0;
			for ( auto loopGen = 0; loopGen < this->numGenerators; ++loopGen ) {
				this->dCElectProdRate += this->elecGenCntrlObj[ loopGen ]->dCElectProdRate;
			}

	//TODO should be		this->inverterObj->manageInverter( this->dCElectProdRate, storageDrawnPower - storageStoredPower ); found doing this seemed to double count power from inverter and storage discharge

			this->inverterObj->manageInverter( this->dCElectProdRate, 0.0 ); //legacy I think is not right
		}
		if ( ( this->storagePresent ) && ( ( this->bussType == dCBussInverterACStorage ) || ( this->bussType == aCBussStorage ) ) ) {
			Real64 pcuLosses = 0.0;
			if ( this->inverterPresent  ) {
				// this will be lagged from last calc.
				pcuLosses = this->inverterObj->getThermLossRate();
			}
			Real64 powerDemand = this->totalPowerRequest + pcuLosses; // this legacy may be wrong, double counting ancillaries maybe?
			Real64 powerGenSupply = 0.0;
			if ( this->bussType == aCBussStorage ) {
				for ( auto loopGen = 0; loopGen < this->numGenerators; ++loopGen ) {
					powerGenSupply += this->elecGenCntrlObj[ loopGen ]->electProdRate;
				}
			} else if ( this->bussType == dCBussInverterACStorage ) {
				powerGenSupply = this->inverterObj->getACPowerOut();
			}
			Real64 storageDrawnPower = 0.0;
			Real64 storageStoredPower = 0.0;
			this->storageObj->manageElectCenterStorageInteractions( powerDemand, powerGenSupply, storageDrawnPower, storageStoredPower );
			remainingWholePowerDemand = remainingWholePowerDemand - storageDrawnPower + storageStoredPower;
		}
		this->updateLoadCenterRecords();
		} // if generators present
	}

	void
	ElectPowerLoadCenter::setupLoadCenterMeterIndices()
	{
		this->demandMeterPtr = EnergyPlus::GetMeterIndex( this->demandMeterName );
		if ( ( this->demandMeterPtr == 0 ) && ( this->genOperationScheme == genOpSchemeTrackMeter ) ) { // throw error
				ShowFatalError( "Did not find Meter named: " + this->demandMeterName + " in ElectricLoadCenter:Distribution named " + this->name );
			}
	}

	void
	ElectPowerLoadCenter::reinitAtBeginEnvironment()
	{
		this->dCElectricityProd        = 0.0;
		this->dCElectProdRate          = 0.0;
		this->dCpowerConditionLosses   = 0.0;
		this->electricityProd          = 0.0;
		this->electProdRate            = 0.0;
		this->thermalProd              = 0.0;
		this->thermalProdRate          = 0.0;
		this->totalPowerRequest        = 0.0;
		this->totalThermalPowerRequest = 0.0;
		this->electDemand              = 0.0;

		if ( this->generatorsPresent && this->numGenerators > 0 ) {
			for ( auto genLoop=0; genLoop < this->numGenerators; ++genLoop ) {
				this->elecGenCntrlObj[ genLoop ]->reinitAtBeginEnvironment();
			}
		}

		if (this->transformerPresent && this->transformerObj != nullptr ){
			this->transformerObj->reinitAtBeginEnvironment();
		}

		if ( this->storagePresent && this->storageObj != nullptr ) {
			this->storageObj->reinitAtBeginEnvironment();
		}

		if ( this->inverterPresent &&  this->inverterObj != nullptr ) {
			this->inverterObj->reinitAtBeginEnvironment();
		}
	}

	void 
	ElectPowerLoadCenter::reinitZoneGainsAtBeginEnvironment()
	{
		if (this->transformerPresent && this->transformerObj != nullptr ){
			this->transformerObj->reinitZoneGainsAtBeginEnvironment();
		}

		if ( this->storagePresent ) {
			this->storageObj->reinitZoneGainsAtBeginEnvironment();
		}

		if ( this->inverterPresent ) {
			this->inverterObj->reinitZoneGainsAtBeginEnvironment();
		}
	}

	std::string
	ElectPowerLoadCenter::getTransformerName()
	{
		if ( this->transformerPresent ) {
			return this->transformerName;		
		} else {
			return "";
		}
	}

	void
	ElectPowerLoadCenter::updateLoadCenterRecords()
	{
		if ( this->generatorsPresent) { //TODO revise for no generators storage 

			switch ( this->bussType )
			{
			case aCBuss: {
				this->electProdRate = 0.0;
				this->electricityProd = 0.0;
				for ( auto loop=0; loop < this->numGenerators ; ++loop ) {
					this->electProdRate += this->elecGenCntrlObj[ loop ]->electProdRate; 
					this->electricityProd += this->elecGenCntrlObj[ loop ]->electricityProd;
				}
				break;
			}
			case aCBussStorage: {
				this->electProdRate = 0.0;
				this->electricityProd = 0.0;
				for ( auto loop=0; loop < this->numGenerators ; ++loop ) {
					this->electProdRate += this->elecGenCntrlObj[ loop ]->electProdRate; 
					this->electricityProd += this->elecGenCntrlObj[ loop ]->electricityProd;
				}
				if ( this->storagePresent ) {
					this->electProdRate += ( this->storageObj->getDrawnPower() -  this->storageObj->getStoredPower() );
					this->electricityProd += ( this->storageObj->getDrawnEnergy() -  this->storageObj->getStoredEnergy() );
				}
				break;
			}
			case dCBussInverter: {
				if ( this->inverterPresent ) {
					this->electProdRate = this->inverterObj->getACPowerOut();
					this->electricityProd = this->inverterObj->getACEnergyOut();
				}
				break;
			}

			case dCBussInverterDCStorage: {
				if ( this->inverterPresent ) {
					this->electProdRate = this->inverterObj->getACPowerOut();
					this->electricityProd = this->inverterObj->getACEnergyOut();
				}
				break;
			}
			case dCBussInverterACStorage: {
				if ( this->inverterPresent && this->storagePresent  ) {
					this->electProdRate = this->inverterObj->getACPowerOut() +  ( this->storageObj->getDrawnPower() -  this->storageObj->getStoredPower() );
					this->electricityProd = this->inverterObj->getACEnergyOut() + ( this->storageObj->getDrawnEnergy() -  this->storageObj->getStoredEnergy() );
				}
				break;
			}
			case bussNotYetSet: {
				// do nothing
			}

			} // end switch
			this->thermalProdRate = 0.0;
			this->thermalProd = 0.0;
			for ( auto loop=0; loop < this->numGenerators ; ++loop ) { 
				this->thermalProdRate += this->elecGenCntrlObj[ loop ]->thermalProdRate;
				this->thermalProd += this->elecGenCntrlObj[ loop ]->thermalProd;
			}
		}
	}

	void
	ElectPowerLoadCenter::calcLoadCenterThermalLoad(
		Real64 & thermalLoad // heat rate called for from cogenerator(watts)
	)
	{
		if ( this->myCoGenSetupFlag ) {
			bool plantNotFound = false;
			for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {
				plantNotFound = false;
				DataPlant::ScanPlantLoopsForObject( this->elecGenCntrlObj[ loopGenNum ]->name, this->elecGenCntrlObj[ loopGenNum ]->compTypeOf_Num, this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.loopNum, this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.loopSideNum, this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.branchNum, this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.compNum, _, _, _, _, _, plantNotFound );
				if ( ! plantNotFound ) this->elecGenCntrlObj[ loopGenNum ]->plantInfoFound = true;
			}
		} // cogen setup

		// sum up "MyLoad" for all generators on this load center from plant structure
		thermalLoad = 0.0;
		for ( auto loopGenNum = 0; loopGenNum < this->numGenerators; ++loopGenNum ) {
			if ( this->elecGenCntrlObj[ loopGenNum ]->plantInfoFound  ) {
				thermalLoad += DataPlant::PlantLoop( this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.loopNum ).LoopSide( this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.loopSideNum ).Branch( this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.branchNum ).Comp( this->elecGenCntrlObj[ loopGenNum ]->cogenLocation.compNum ).MyLoad;
			}
		}
	}



	GeneratorController::GeneratorController(
		std::string objectName,
		std::string objectType,
		Real64 ratedElecPowerOutput,
		std::string availSchedName,
		Real64 thermalToElectRatio
	)
	{
		//initialization
		this->name = "";
		this->typeOfName= "";
		this->compTypeOf_Num = 0;
		this->generatorType = generatorNotYetSet;
		this->generatorIndex = 0;
		this->maxPowerOut = 0.0;
		this->availSchedPtr = 0;
		this->powerRequestThisTimestep= 0.0;
		this->onThisTimestep = false;
		this->eMSPowerRequest= 0.0;
		this->eMSRequestOn = false;
		this->plantInfoFound = false;
		this->cogenLocation =  PlantLocation( 0, 0, 0, 0 );
		this->nominalThermElectRatio= 0.0;
		this->dCElectricityProd= 0.0;
		this->dCElectProdRate= 0.0;
		this->electricityProd= 0.0;
		this->electProdRate= 0.0;
		this->thermalProd= 0.0;
		this->thermalProdRate= 0.0;

		std::string const routineName = "GeneratorController constructor ";
		bool errorsFound = false;

		this->name                   = objectName;
		this->typeOfName             = objectType;
		if ( InputProcessor::SameString( objectType, "Generator:InternalCombustionEngine" ) ) {
			this->generatorType = generatorICEngine;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorICEngine;
		} else if ( InputProcessor::SameString( objectType, "Generator:CombustionTurbine" ) ) {
			this->generatorType = generatorCombTurbine;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorCombTurbine;
		} else if ( InputProcessor::SameString( objectType, "Generator:MicroTurbine" ) ) {
			this->generatorType = generatorMicroturbine;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorMicroturbine;
		} else if ( InputProcessor::SameString( objectType, "Generator:Photovoltaic" ) ) {
			this->generatorType = generatorPV;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorPV;
		} else if ( InputProcessor::SameString( objectType, "Generator:FuelCell" ) ) {
			this->generatorType = generatorFuelCell;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorFuelCell;
		} else if ( InputProcessor::SameString( objectType, "Generator:MicroCHP" ) ) {
			this->generatorType = generatorMicroCHP;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorMicroCHP;
		} else if ( InputProcessor::SameString( objectType, "Generator:WindTurbine" ) ) {
			this->generatorType = generatorWindTurbine;
			this->compTypeOf_Num = DataGlobalConstants::iGeneratorWindTurbine;
		} else {
			ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + " invalid entry." );
			ShowContinueError( "Invalid " + objectType + " associated with generator = " + objectName ) ;
			errorsFound = true;
		}

		this->availSched             = availSchedName;
		if ( availSched.empty() ) {
			this->availSchedPtr = DataGlobals::ScheduleAlwaysOn;
		} else {
			this->availSchedPtr =  ScheduleManager::GetScheduleIndex( availSchedName );
			if ( this->availSchedPtr <= 0 ) {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + ", invalid entry." );
				ShowContinueError( "Invalid availability schedule = " + availSchedName );
				ShowContinueError( "Schedule was not found " );
				errorsFound = true;
			}
		}

		this->maxPowerOut            = ratedElecPowerOutput,
		this->nominalThermElectRatio = thermalToElectRatio;

		SetupOutputVariable( "Generator Requested Electric Power [W]", this->powerRequestThisTimestep, "System", "Average", objectName );
		if ( DataGlobals::AnyEnergyManagementSystemInModel ) {
			SetupEMSInternalVariable( "Generator Nominal Maximum Power", objectName, "[W]", this->maxPowerOut );
			SetupEMSInternalVariable( "Generator Nominal Thermal To Electric Ratio", objectName, "[ratio]", this->nominalThermElectRatio );
			SetupEMSActuator( "On-Site Generator Control", objectName, "Requested Power", "[W]", this->eMSRequestOn, this->eMSPowerRequest );
		}
	}

	void
	GeneratorController::reinitAtBeginEnvironment()
	{
		this->onThisTimestep    = false;
		this->dCElectricityProd = 0.0;
		this->dCElectProdRate   = 0.0;
		this->electricityProd   = 0.0;
		this->electProdRate     = 0.0;
		this->thermalProd       = 0.0;
		this->thermalProdRate   = 0.0;
	}

	void
	GeneratorController::simGeneratorGetPowerOutput(
		bool const runFlag,
		Real64 const myElecLoadRequest,
		bool const FirstHVACIteration, // Unused 2010 JANUARY
		Real64 & electricPowerOutput, // Actual generator electric power output
		Real64 & thermalPowerOutput // Actual generator thermal power output
	)
	{
		// Select and call models and also collect results for load center power conditioning and reporting
		switch ( this->generatorType )
		{
		case generatorICEngine: {
			ICEngineElectricGenerator::SimICEngineGenerator( DataGlobalConstants::iGeneratorICEngine, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			ICEngineElectricGenerator::GetICEGeneratorResults( DataGlobalConstants::iGeneratorICEngine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorCombTurbine: {
			CTElectricGenerator::SimCTGenerator( DataGlobalConstants::iGeneratorCombTurbine, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			CTElectricGenerator::GetCTGeneratorResults( DataGlobalConstants::iGeneratorCombTurbine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorPV: {
			Photovoltaics::SimPVGenerator( DataGlobalConstants::iGeneratorPV, this->name, this->generatorIndex, runFlag, myElecLoadRequest );
			Photovoltaics::GetPVGeneratorResults( DataGlobalConstants::iGeneratorPV, this->generatorIndex, this->dCElectProdRate, this->dCElectricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->dCElectProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorFuelCell: {
			FuelCellElectricGenerator::SimFuelCellGenerator( DataGlobalConstants::iGeneratorFuelCell, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			FuelCellElectricGenerator::GetFuelCellGeneratorResults( DataGlobalConstants::iGeneratorFuelCell, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorMicroCHP: {
			MicroCHPElectricGenerator::SimMicroCHPGenerator( DataGlobalConstants::iGeneratorMicroCHP, this->name, this->generatorIndex, runFlag, false, myElecLoadRequest, DataPrecisionGlobals::constant_zero, FirstHVACIteration );
			MicroCHPElectricGenerator::GetMicroCHPGeneratorResults( DataGlobalConstants::iGeneratorMicroCHP, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorMicroturbine: {
			MicroturbineElectricGenerator::SimMTGenerator( DataGlobalConstants::iGeneratorMicroturbine, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			MicroturbineElectricGenerator::GetMTGeneratorResults( DataGlobalConstants::iGeneratorMicroturbine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorWindTurbine: {
			WindTurbine::SimWindTurbine( DataGlobalConstants::iGeneratorWindTurbine, this->name, this->generatorIndex, runFlag, myElecLoadRequest );
			WindTurbine::GetWTGeneratorResults( DataGlobalConstants::iGeneratorWindTurbine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermalProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermalProdRate;
			break;
		}
		case generatorNotYetSet: {
			// do nothing
			break;
		}
		} // end switch
	}

	DCtoACInverter::DCtoACInverter(
		std::string objectName
	)
	{
		//initialize
		this->modelType = notYetSet;
		this->availSchedPtr = 0;
		this->heatLossesDestination = heatLossNotDetermined;
		this->zoneNum = 0;
		this->zoneRadFract = 0.0;
		this->nightTareLossPower = 0.0;
		this->nominalVoltage = 0.0;
		this->nomVoltEfficiencyARR.resize( 6, 0.0 );
		this->curveNum = 0;
		this->ratedPower = 0.0;
		this->minPower = 0.0;
		this->maxPower = 0.0;
		this->minEfficiency = 0.0;
		this->maxEfficiency = 0.0;
		this->standbyPower = 0.0;
		this->efficiency = 0.0;
		this->dCPowerIn = 0.0;
		this->aCPowerOut = 0.0;
		this->dCEnergyIn = 0.0;
		this->aCEnergyOut = 0.0;
		this->thermLossRate = 0.0;
		this->thermLossEnergy = 0.0;
		this->qdotConvZone = 0.0;
		this->qdotRadZone = 0.0;
		this->ancillACuseRate = 0.0;
		this->ancillACuseEnergy = 0.0;

		std::string const routineName = "DCtoACInverter constructor ";
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool errorsFound = false;
		// if/when add object class name to input object this can be simplified. for now search all possible types 
		bool foundInverter = false;
		int testInvertIndex = 0;
		int invertIDFObjectNum = 0;

		testInvertIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Inverter:LookUpTable",  objectName );
		if ( testInvertIndex > 0) {
			foundInverter = true;
			invertIDFObjectNum = testInvertIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:LookUpTable";
			this->modelType = cECLookUpTableModel;
		}
		testInvertIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Inverter:FunctionOfPower",  objectName );
		if ( testInvertIndex > 0) {
			foundInverter = true;
			invertIDFObjectNum = testInvertIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:FunctionOfPower";
			this->modelType = curveFuncOfPower;
		}
		testInvertIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Inverter:Simple",  objectName );
		if ( testInvertIndex > 0) {
			foundInverter = true;
			invertIDFObjectNum = testInvertIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:Simple";
			this->modelType = simpleConstantEff;
		}

		if ( foundInverter ){

			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, invertIDFObjectNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name          = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?

			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			this->zoneNum = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 3 ), DataHeatBalance::Zone );
			if ( this->zoneNum > 0 ) this->heatLossesDestination = zoneGains;
			if ( this->zoneNum == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 3 ) ) {
					this->heatLossesDestination = lostToOutside;
				} else {
					this->heatLossesDestination = lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					ShowContinueError( "Zone name not found. Inverter heat losses will not be added to a zone" );
					// continue with simulation but inverter losses not sent to a zone.
				}
			}
			this->zoneRadFract = DataIPShortCuts::rNumericArgs( 1 );

			// now the input objects differ depending on class type
			switch ( this->modelType )
			{
			case cECLookUpTableModel: {
				this->ratedPower                = DataIPShortCuts::rNumericArgs( 2 );
				this->standbyPower              = DataIPShortCuts::rNumericArgs( 3 );
				this->nightTareLossPower        = DataIPShortCuts::rNumericArgs( 3 );
				this->nominalVoltage            = DataIPShortCuts::rNumericArgs( 4 );
				this->nomVoltEfficiencyARR[ 0 ] = DataIPShortCuts::rNumericArgs( 5 );
				this->nomVoltEfficiencyARR[ 1 ] = DataIPShortCuts::rNumericArgs( 6 );
				this->nomVoltEfficiencyARR[ 2 ] = DataIPShortCuts::rNumericArgs( 7 );
				this->nomVoltEfficiencyARR[ 3 ] = DataIPShortCuts::rNumericArgs( 8 );
				this->nomVoltEfficiencyARR[ 4 ] = DataIPShortCuts::rNumericArgs( 9 );
				this->nomVoltEfficiencyARR[ 5 ] = DataIPShortCuts::rNumericArgs( 10 );
				break;
			}
			case curveFuncOfPower: {
				this->curveNum = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 4 ) );
				if ( this->curveNum == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = " + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Curve was not found" );
					errorsFound = true;
				}

				this->ratedPower    = DataIPShortCuts::rNumericArgs( 2 );
				this->minEfficiency = DataIPShortCuts::rNumericArgs( 3 );
				this->maxEfficiency = DataIPShortCuts::rNumericArgs( 4 );
				this->minPower      = DataIPShortCuts::rNumericArgs( 5 );
				this->maxPower      = DataIPShortCuts::rNumericArgs( 6 );
				this->standbyPower  = DataIPShortCuts::rNumericArgs( 7 );
				break;
			}
			case simpleConstantEff: {
				this->efficiency = DataIPShortCuts::rNumericArgs( 2 );
				break;
			}
			case notYetSet: {
				// do nothing
				break;
			}

			} // end switch modelType
		
			SetupOutputVariable( "Inverter DC to AC Efficiency []", this->efficiency, "System", "Average", this->name );
			SetupOutputVariable( "Inverter DC Input Electric Power [W]", this->dCPowerIn, "System", "Average", this->name );
			SetupOutputVariable( "Inverter DC Input Electric Energy [J]", this->dCEnergyIn, "System", "Sum", this->name );
			SetupOutputVariable( "Inverter AC Output Electric Power [W]", this->aCPowerOut, "System", "Average", this->name );
			SetupOutputVariable( "Inverter AC Output Electric Energy [J]", this->aCEnergyOut, "System", "Sum", this->name, _, "ElectricityProduced", "Photovoltaics", _, "Plant" ); // right now PV is the only DC source
			SetupOutputVariable( "Inverter Thermal Loss Rate [W]", this->thermLossRate, "System", "Average", this->name );
			SetupOutputVariable( "Inverter Thermal Loss Energy [J]", this->thermLossEnergy, "System", "Sum", this->name );
			SetupOutputVariable( "Inverter Ancillary AC Electric Power [W]", this->ancillACuseRate, "System", "Average", this->name );
			SetupOutputVariable( "Inverter Ancillary AC Electric Energy [J]", this->ancillACuseEnergy, "System", "Sum", this->name, _, "Electricity", "Cogeneration", _, "Plant" ); // called cogeneration for end use table
			if ( this->zoneNum > 0 ) {
				switch (this->modelType )
				{
				case simpleConstantEff: {
					SetupZoneInternalGain( this->zoneNum, "ElectricLoadCenter:Inverter:Simple", this->name, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterSimple, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case curveFuncOfPower: {
					SetupZoneInternalGain( this->zoneNum, "ElectricLoadCenter:Inverter:FunctionOfPower", this->name, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case cECLookUpTableModel: {
					SetupZoneInternalGain( this->zoneNum, "ElectricLoadCenter:Inverter:LookUpTable", this->name, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case notYetSet: {
					// do nothing
					break;
				}
				} // end switch modelType
			}
		} else {
			ShowSevereError( routineName + " did not find inverter name = " + objectName);
			errorsFound = true;
		}

		if ( errorsFound ) {
			ShowFatalError( routineName + "Preceding errors terminate program." );
		}
	}

	void
	DCtoACInverter::reinitAtBeginEnvironment()
	{
		this->ancillACuseRate   = 0.0;
		this->ancillACuseEnergy = 0.0;
		this->qdotConvZone      = 0.0;
		this->qdotRadZone       = 0.0;
	}

	void
	DCtoACInverter::reinitZoneGainsAtBeginEnvironment()
	{
		this->qdotConvZone            = 0.0;
		this->qdotRadZone             = 0.0;
	}

	Real64
	DCtoACInverter::getThermLossRate()
	{
		return this->thermLossRate;
	}

	Real64
	DCtoACInverter::getACPowerOut()
	{
		return this->aCPowerOut;
	}

	Real64
	DCtoACInverter::getACEnergyOut()
	{
		return this->aCEnergyOut;
	}

	void
	DCtoACInverter::manageInverter(
		Real64 const powerDCElectProductionRate,
		Real64 const powerDCElectNetStorageDrawRate
	)
	{
		this->dCPowerIn = powerDCElectProductionRate + powerDCElectNetStorageDrawRate;
		this->dCEnergyIn = this->dCPowerIn * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
			// check availability schedule
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr ) > 0.0 ) {
			Real64 tempACPower = 0.0;
			Real64 tmpEffic = 0.0;
			// now calculate Inverter based on model type
			switch ( this->modelType )
			{
			case cECLookUpTableModel: {
				// we don't model voltage, so use nominal voltage
				Real64 normalizedPower = this->dCPowerIn / this->ratedPower;

				// get efficiency
				if ( normalizedPower <= 0.1 ) {
					// extrapolate or fix at 10% value? fix it for now
					tmpEffic = this->nomVoltEfficiencyARR[ 0 ];
				} else if ( ( normalizedPower > 0.1 ) && ( normalizedPower < 0.20 ) ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 0 ] + ( ( normalizedPower - 0.1 ) / ( 0.2 - 0.1 ) ) * ( this->nomVoltEfficiencyARR[ 1 ] - this->nomVoltEfficiencyARR[ 0 ] );
				} else if ( normalizedPower == 0.2 ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 1 ];
				} else if ( ( normalizedPower > 0.2 ) && ( normalizedPower < 0.30 ) ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 1 ] + ( ( normalizedPower - 0.2 ) / ( 0.3 - 0.2 ) ) * ( this->nomVoltEfficiencyARR[ 2 ] - this->nomVoltEfficiencyARR[ 1 ] );
				} else if ( normalizedPower == 0.3 ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 2 ];
				} else if ( ( normalizedPower > 0.3 ) && ( normalizedPower < 0.50 ) ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 2 ] + ( ( normalizedPower - 0.3 ) / ( 0.5 - 0.3 ) ) * ( this->nomVoltEfficiencyARR[ 3 ] - this->nomVoltEfficiencyARR[ 2 ] );
				} else if ( normalizedPower == 0.5 ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 3 ];
				} else if ( ( normalizedPower > 0.5 ) && ( normalizedPower < 0.75 ) ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 3 ] + ( ( normalizedPower - 0.5 ) / ( 0.75 - 0.5 ) ) * ( this->nomVoltEfficiencyARR[ 4 ] - this->nomVoltEfficiencyARR[ 3 ] );
				} else if ( normalizedPower == 0.75 ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 4 ];
				} else if ( ( normalizedPower > 0.75 ) && ( normalizedPower < 1.0 ) ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 4 ] + ( ( normalizedPower - 0.75 ) / ( 1.0 - 0.75 ) ) * ( this->nomVoltEfficiencyARR[ 5 ] - this->nomVoltEfficiencyARR[ 4 ] );
				} else if ( normalizedPower >= 1.0 ) {
					tmpEffic = this->nomVoltEfficiencyARR[ 5 ];
				} else {
					assert( false );
				}

				this->efficiency = max( tmpEffic, 0.0 );
				this->efficiency = min( this->efficiency, 1.0 );

				tempACPower = this->efficiency * this->dCPowerIn;
				break;
			}
			case curveFuncOfPower: {
				Real64 normalizedPower = this->dCPowerIn / this->ratedPower;

				tmpEffic = CurveManager::CurveValue( this->curveNum, normalizedPower );

				this->efficiency = max( tmpEffic, this->minEfficiency );
				this->efficiency = min( this->efficiency, this->maxEfficiency );

				tempACPower = this->efficiency * this->dCPowerIn;
				if ( tempACPower < this->minPower ) { // not enough to produce any AC power.  all lost. also standby mode
					tempACPower = 0.0;

				} else if ( tempACPower > this->maxPower ) { // too much DC for inverter to handle, excess is lost
					tempACPower = this->maxPower;
				}
				break;
			}
			case simpleConstantEff: {
				tempACPower = this->efficiency * this->dCPowerIn;
				break;
			}
			case notYetSet: {
				// do nothing
				tempACPower = 0.0;
				break;
			}

			} // end switch

			this->aCPowerOut = tempACPower;
			this->aCEnergyOut = tempACPower * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

			if ( tempACPower == 0.0 ) {
				this->ancillACuseEnergy = this->standbyPower * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
				this->ancillACuseRate = this->standbyPower;
			}
		} else { // not available per schedule, inverter is dead.
			//  assume thermal shunt for DC in, but no standby electricity
			this->aCPowerOut = 0.0;
			this->aCEnergyOut = 0.0;
			this->ancillACuseRate = 0.0;
			this->ancillACuseEnergy = 0.0;

		}
		//update report variables
		this->thermLossRate = this->dCPowerIn - this->aCPowerOut + this->standbyPower;
		this->thermLossEnergy = this->thermLossRate * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->qdotConvZone = this->thermLossRate * ( 1.0 - this->zoneRadFract );
		this->qdotRadZone = this->thermLossRate * this->zoneRadFract;
	}

	// main constructor
	ElectricStorage::ElectricStorage(
		std::string objectName
	)
	{
		//initialize
		this->maxRainflowArrayBounds = 100;
		this->myWarmUpFlag =  false;
		this->storageModelMode = storageTypeNotSet;
		this->availSchedPtr = 0;
		this->heatLossesDestination = heatLossNotDetermined;
		this->zoneNum = 0;
		this->zoneRadFract = 0.0;
		this->startingEnergyStored = 0.0;
		this->energeticEfficCharge = 0.0;
		this->energeticEfficDischarge = 0.0;
		this->maxPowerDraw = 0.0;
		this->maxPowerStore = 0.0;
		this->maxEnergyCapacity = 0.0;
		this->parallelNum = 0;
		this->seriesNum = 0;
		this->numBattery = 0;
		this->chargeCurveNum = 0;
		this->dischargeCurveNum = 0;
		this->cycleBinNum = 0;
		this->startingSOC = 0.0;
		this->maxAhCapacity = 0.0;
		this->availableFrac = 0.0;
		this->chargeConversionRate = 0.0;
		this->chargedOCV = 0.0;
		this->dischargedOCV = 0.0;
		this->internalR = 0.0;
		this->maxDischargeI = 0.0;
		this->cutoffV = 0.0;
		this->maxChargeRate = 0.0;
		this->lifeCalculation = degredationNotSet;
		this->lifeCurveNum = 0;
		this->thisTimeStepStateOfCharge = 0.0;
		this->lastTimeStepStateOfCharge = 0.0;
		this->pelNeedFromStorage = 0.0;
		this->pelFromStorage = 0.0;
		this->eMSOverridePelFromStorage = false;
		this->eMSValuePelFromStorage = 0.0;
		this->pelIntoStorage = 0.0;
		this->eMSOverridePelIntoStorage = false;
		this->eMSValuePelIntoStorage = 0.0;
		this->qdotConvZone = 0.0;
		this->qdotRadZone = 0.0;
		this->timeElapsed = 0.0;
		this->thisTimeStepAvailable = 0.0;
		this->thisTimeStepBound = 0.0;
		this->lastTimeStepAvailable = 0.0;
		this->lastTimeStepBound = 0.0;
		this->lastTwoTimeStepAvailable = 0.0;
		this->lastTwoTimeStepBound = 0.0;
		this->count0 = 0;
		this->electEnergyinStorage = 0.0;
		this->storedPower = 0.0;
		this->storedEnergy = 0.0;
		this->decrementedEnergyStored = 0.0;
		this->drawnPower = 0.0;
		this->drawnEnergy = 0.0;
		this->thermLossRate = 0.0;
		this->thermLossEnergy = 0.0;
		this->storageMode = 0;
		this->absoluteSOC = 0.0;
		this->fractionSOC = 0.0;
		this->batteryCurrent = 0.0;
		this->batteryVoltage = 0.0;
		this->batteryDamage = 0.0;

		std::string const routineName = "ElectricStorage constructor ";
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array
		int iOStat; // IO Status when calling get input subroutine
		bool errorsFound = false;
		// if/when add object class name to input object this can be simplified. for now search all possible types 
		bool foundStorage = false;
		int testStorageIndex = 0;
		int storageIDFObjectNum = 0;

		testStorageIndex = InputProcessor::GetObjectItemNum("ElectricLoadCenter:Storage:Simple", objectName );
		if ( testStorageIndex > 0 ) {
			foundStorage = true;
			storageIDFObjectNum = testStorageIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Simple";
			this->storageModelMode = simpleBucketStorage;
		}

		testStorageIndex = InputProcessor::GetObjectItemNum("ElectricLoadCenter:Storage:Battery", objectName );
		if ( testStorageIndex > 0 ) {
			foundStorage = true;
			storageIDFObjectNum = testStorageIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Battery";
			this->storageModelMode = kiBaMBattery;
		}

		if ( foundStorage ) {
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, storageIDFObjectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, iOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name          = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?

			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			this->zoneNum = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 3 ), DataHeatBalance::Zone );
			if ( this->zoneNum > 0 ) this->heatLossesDestination = zoneGains;
			if ( this->zoneNum == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 3 ) ) {
					this->heatLossesDestination = lostToOutside;
				} else {
					this->heatLossesDestination = lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					ShowContinueError( "Zone name not found. Storage heat losses will not be added to a zone" );
					// continue with simulation but storage losses not sent to a zone.
				}
			}
			this->zoneRadFract = DataIPShortCuts::rNumericArgs( 1 );

			switch ( this->storageModelMode )
			{
			
			case simpleBucketStorage: {
				this->energeticEfficCharge    = DataIPShortCuts::rNumericArgs( 2 );
				this->energeticEfficDischarge = DataIPShortCuts::rNumericArgs( 3 );
				this->maxEnergyCapacity       = DataIPShortCuts::rNumericArgs( 4 );
				this->maxPowerDraw            = DataIPShortCuts::rNumericArgs( 5 );
				this->maxPowerStore           = DataIPShortCuts::rNumericArgs( 6 );
				this->startingEnergyStored    = DataIPShortCuts::rNumericArgs( 7 );
				SetupOutputVariable( "Electric Storage Charge State [J]", this->electEnergyinStorage, "System", "Average", this->name );
				break;
			}
			
			case kiBaMBattery: {
				this->chargeCurveNum = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 4 ) ); //voltage calculation for charging
				if ( this->chargeCurveNum == 0 && ! DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + '=' + DataIPShortCuts::cAlphaArgs( 4 ) );
					errorsFound = true;
				} else if ( DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " cannot be blank. But no entry found." );
					errorsFound = true;
				} else if ( ! InputProcessor::SameString( CurveManager::GetCurveType( this->chargeCurveNum ), "RectangularHyperbola2" ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + '=' + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Curve Type must be RectangularHyperbola2 but was " + CurveManager::GetCurveType( this->chargeCurveNum ) );
					errorsFound = true;
				}
				this->dischargeCurveNum = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 5 ) ); // voltage calculation for discharging
				if ( this->dischargeCurveNum == 0 && ! DataIPShortCuts::lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + '=' + DataIPShortCuts::cAlphaArgs( 5 ) );
					errorsFound = true;
				} else if ( DataIPShortCuts::lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + " cannot be blank. But no entry found." );
					errorsFound = true;
				} else if ( ! InputProcessor::SameString( CurveManager::GetCurveType( this->dischargeCurveNum ), "RectangularHyperbola2" ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + '=' + DataIPShortCuts::cAlphaArgs( 5 ) );
					ShowContinueError( "Curve Type must be RectangularHyperbola2 but was " + CurveManager::GetCurveType( this->dischargeCurveNum ) );
					errorsFound = true;
				}

				if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "Yes" ) ) {
					this->lifeCalculation = batteryLifeCalculationYes;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "No" ) ) {
					this->lifeCalculation = batteryLifeCalculationNo;
				} else {
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 6 ) + " = " + DataIPShortCuts::cAlphaArgs( 6 ) );
					ShowContinueError( "Yes or No should be selected. Default value No is used to continue simulation" );
					this->lifeCalculation = batteryLifeCalculationNo;
				}

				if ( this->lifeCalculation == batteryLifeCalculationYes ) {
					this->lifeCurveNum = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 7 ) ); //Battery life calculation
					if ( this->lifeCurveNum == 0 && ! DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
						ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + '=' + DataIPShortCuts::cAlphaArgs( 7 ) );
						errorsFound = true;
					} else if ( DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
						ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + " cannot be blank when " + DataIPShortCuts::cAlphaArgs( 6 ) + " = Yes. But no entry found." );
						errorsFound = true;
					} else if ( ! InputProcessor::SameString( CurveManager::GetCurveType( this->lifeCurveNum ), "DoubleExponentialDecay" ) ) {
						ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + '=' + DataIPShortCuts::cAlphaArgs( 7 ) );
						ShowContinueError( "Curve Type must be DoubleExponentialDecay but was " + CurveManager::GetCurveType( this->lifeCurveNum ) );
						errorsFound = true;
					}

					this->cycleBinNum = DataIPShortCuts::rNumericArgs( 14 );

					if ( ! errorsFound ) { // life cycle calculation for this battery, allocate arrays for degradation calculation
					//std::vector is zero base instead of 1, so first index is now 0. 
						this->b10.resize( this->maxRainflowArrayBounds + 1, 0.0 );
						this->x0.resize( this->maxRainflowArrayBounds + 1, 0 );
						this->nmb0.resize( this->cycleBinNum , 0.0 );
						this->oneNmb0.resize( this->cycleBinNum, 0.0 );
					}
				}

				this->parallelNum          = DataIPShortCuts::rNumericArgs( 2 );
				this->seriesNum            = DataIPShortCuts::rNumericArgs( 3 );
				this->numBattery           = this->parallelNum * this->seriesNum ;
				this->maxAhCapacity        = DataIPShortCuts::rNumericArgs( 4 );
				this->startingSOC          = DataIPShortCuts::rNumericArgs( 5 );
				this->availableFrac        = DataIPShortCuts::rNumericArgs( 6 );
				this->chargeConversionRate = DataIPShortCuts::rNumericArgs( 7 );
				this->chargedOCV           = DataIPShortCuts::rNumericArgs( 8 );
				this->dischargedOCV        = DataIPShortCuts::rNumericArgs( 9 );
				this->internalR            = DataIPShortCuts::rNumericArgs( 10 );
				this->maxDischargeI        = DataIPShortCuts::rNumericArgs( 11 );
				this->cutoffV              = DataIPShortCuts::rNumericArgs( 12 );
				this->maxChargeRate        = DataIPShortCuts::rNumericArgs( 13 );

				SetupOutputVariable( "Electric Storage Operating Mode Index []", this->storageMode, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Charge State [Ah]", this->absoluteSOC, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Charge Fraction []", this->fractionSOC, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Total Current [A]", this->batteryCurrent, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Total Voltage [V]", this->batteryVoltage, "System", "Average", this->name );

				if ( this->lifeCalculation == batteryLifeCalculationYes ) {
					SetupOutputVariable( "Electric Storage Degradation Fraction []", this->batteryDamage, "System", "Average", this->name );
				}
				break;
			}
			case storageTypeNotSet: {
				// do nothing
				break;
			}

			} // switch storage model type

			SetupOutputVariable( "Electric Storage Charge Power [W]", this->storedPower, "System", "Average", this->name  );
			SetupOutputVariable( "Electric Storage Charge Energy [J]", this->storedEnergy, "System", "Sum", this->name  );
			SetupOutputVariable( "Electric Storage Production Decrement Energy [J]", this->decrementedEnergyStored, "System", "Sum", this->name , _, "ElectricityProduced", "ELECTRICSTORAGE", _, "Plant" );
			SetupOutputVariable( "Electric Storage Discharge Power [W]", this->drawnPower, "System", "Average", this->name  );
			SetupOutputVariable( "Electric Storage Discharge Energy [J]", this->drawnEnergy, "System", "Sum", this->name , _, "ElectricityProduced", "ELECTRICSTORAGE", _, "Plant" );
			SetupOutputVariable( "Electric Storage Thermal Loss Rate [W]", this->thermLossRate, "System", "Average", this->name  );
			SetupOutputVariable( "Electric Storage Thermal Loss Energy [J]", this->thermLossEnergy, "System", "Sum", this->name  );
			if ( DataGlobals::AnyEnergyManagementSystemInModel ) {
				if ( this->storageModelMode == simpleBucketStorage ) {
					SetupEMSInternalVariable( "Electrical Storage Maximum Capacity", this->name , "[J]", this->maxEnergyCapacity );
				} else if ( this->storageModelMode == kiBaMBattery ) {
					SetupEMSInternalVariable( "Electrical Storage Maximum Capacity", this->name , "[Ah]", this->maxAhCapacity );
				}
				SetupEMSActuator( "Electrical Storage", this->name , "Power Draw Rate", "[W]", this->eMSOverridePelFromStorage, this->eMSValuePelFromStorage );
				SetupEMSActuator( "Electrical Storage", this->name , "Power Charge Rate", "[W]", this->eMSOverridePelIntoStorage, this->eMSValuePelIntoStorage );
			}

			if ( this->zoneNum > 0 ) {
				switch ( this->storageModelMode )
				{
				case simpleBucketStorage: {
					SetupZoneInternalGain( this->zoneNum, "ElectricLoadCenter:Storage:Simple", this->name , DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageSimple, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case kiBaMBattery: {
					SetupZoneInternalGain( this->zoneNum, "ElectricLoadCenter:Storage:Battery", this->name , DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageBattery, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case storageTypeNotSet: {
					// do nothing
					break;
				}

				} // switch storage model type
			}
		} else { // storage not found
			ShowSevereError( routineName + " did not find storage name = " + objectName);
			errorsFound = true;
		}
		if ( errorsFound ) {
			ShowFatalError( routineName + "Preceding errors terminate program." );
		}
	}

	void
	ElectricStorage::reinitAtBeginEnvironment()
	{
		this->pelNeedFromStorage      = 0.0;
		this->pelFromStorage          = 0.0;
		this->pelIntoStorage          = 0.0;
		this->qdotConvZone            = 0.0;
		this->qdotRadZone             = 0.0;
		this->timeElapsed             = 0.0;
		this->electEnergyinStorage    = 0.0;
		this->storedPower             = 0.0;
		this->storedEnergy            = 0.0;
		this->decrementedEnergyStored = 0.0;
		this->drawnPower              = 0.0;
		this->drawnEnergy             = 0.0;
		this->thermLossRate           = 0.0;
		this->thermLossEnergy         = 0.0;
		this->lastTimeStepStateOfCharge = this->startingEnergyStored;
		this->thisTimeStepStateOfCharge = this->startingEnergyStored;

		if ( this->storageModelMode == kiBaMBattery ) {
			Real64 initialCharge = this->maxAhCapacity * this->startingSOC;
			this->lastTwoTimeStepAvailable = initialCharge * this->availableFrac;
			this->lastTwoTimeStepBound = initialCharge * ( 1.0 - this->availableFrac );
			this->lastTimeStepAvailable = initialCharge * this->availableFrac;
			this->lastTimeStepBound = initialCharge * ( 1.0 - this->availableFrac );
			this->thisTimeStepAvailable = initialCharge * this->availableFrac;
			this->thisTimeStepBound = initialCharge * ( 1.0 - this->availableFrac );
			if ( this->lifeCalculation == batteryLifeCalculationYes ) {
				this->count0 = 1; // Index 0 is for initial SOC, so new input starts from index 2.
				this->b10[ 0 ] = this->startingSOC; // the initial fractional SOC is stored as the reference
				this->x0[ 0 ] = 0;
				for (auto loop = 1; loop < this->maxRainflowArrayBounds + 1; ++loop) {
					this->b10[ loop ] = 0.0;
					this->x0[ loop ] = 0;
				}
				for (auto loop = 0; loop < this->cycleBinNum; ++loop) {
					this->oneNmb0[ loop ] = 0.0;
					this->nmb0[ loop ] = 0.0;
				}
				this->batteryDamage = 0.0;
			}
		}
		this->myWarmUpFlag = true;
	}

	void
	ElectricStorage::reinitZoneGainsAtBeginEnvironment()
	{
		this->qdotConvZone            = 0.0;
		this->qdotRadZone             = 0.0;
	}

	void
	ElectricStorage::manageElectCenterStorageInteractions(
		Real64 const powerDemand, // load center power demand minus any inverter losses that need to be applied
		Real64 const powerGenSupply, // sum of load center generator production
		Real64 & StorageDrawnPower, // Electric Power Draw Rate from storage units
		Real64 & StorageStoredPower // Electric Power Store Rate from storage units
	)
	{
		if ( this->myWarmUpFlag && ( ! DataGlobals::WarmupFlag ) ) {
			this->reinitAtBeginEnvironment();
			this->myWarmUpFlag = false;
		}

		Real64 timeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
		if ( this->timeElapsed != timeElapsed ) { //time changed, update last with "current" result from previous time
			if ( this->storageModelMode == kiBaMBattery && this->lifeCalculation == batteryLifeCalculationYes ) {
				//    At this point, the current values, last time step values and last two time step values have not been updated, hence:
				//    "ThisTimeStep*" actually points to the previous one time step
				//    "LastTimeStep*" actually points to the previous two time steps
				//    "LastTwoTimeStep" actually points to the previous three time steps

				//      Calculate the fractional SOC change between the "current" time step and the "previous one" time step
				Real64 deltaSOC1 = this->thisTimeStepAvailable +this->thisTimeStepBound - this->lastTimeStepAvailable - this->lastTimeStepBound;
				deltaSOC1 /= this->maxAhCapacity;

				//      Calculate the fractional SOC change between the "previous one" time step and the "previous two" time steps
				Real64 deltaSOC2 = this->lastTimeStepAvailable + this->lastTimeStepBound - this->lastTwoTimeStepAvailable - this->lastTwoTimeStepBound;
				deltaSOC2 /= this->maxAhCapacity;

				//     DeltaSOC2 = 0 may occur at the begining of each simulation environment.
				//     DeltaSOC1 * DeltaSOC2 means that the SOC from "LastTimeStep" is a peak or valley. Only peak or valley needs
				//     to call the rain flow algorithm
				if ( ( deltaSOC2 == 0 ) || ( ( deltaSOC1 * deltaSOC2 ) < 0 ) ) {
					//     Because we cannot determine whehter "ThisTimeStep" is a peak or valley (next time step is unknown yet), we
					//     use the "LastTimeStep" value for battery life calculation.
					Real64 input0 = ( this->lastTimeStepAvailable + this->lastTimeStepBound ) / this->maxAhCapacity;
					this->b10[ this->count0 ] = input0;

					//        The arrary size needs to be increased when count = MaxRainflowArrayBounds. Please note that (MaxRainflowArrayBounds +1)
					//        is the index used in the subroutine RainFlow. So we cannot reallocate array size until count = MaxRainflowArrayBounds +1.
					if ( this->count0 == this->maxRainflowArrayBounds ) {
						this->b10.resize( this->maxRainflowArrayBounds + 1 + this->maxRainflowArrayInc, 0.0 );
						this->x0.resize( this->maxRainflowArrayBounds + 1 + this->maxRainflowArrayInc, 0.0 );
						this->maxRainflowArrayBounds += this->maxRainflowArrayInc;
					}

					this->rainflow( this->cycleBinNum, input0, this->b10, this->x0, this->count0, this->nmb0, this->oneNmb0);

					this->batteryDamage = 0.0;

					for ( auto binNum = 0; binNum < this->cycleBinNum; ++binNum ) {
						//       Battery damage is calculated by accumulating the impact from each cycle.
						this->batteryDamage += this->oneNmb0[ binNum ] / CurveManager::CurveValue( this->lifeCurveNum, ( double( binNum ) / double( this->cycleBinNum ) ) );
					}
				}
			}

			this->lastTimeStepStateOfCharge = this->thisTimeStepStateOfCharge;
			this->lastTwoTimeStepAvailable = this->lastTimeStepAvailable;
			this->lastTwoTimeStepBound = this->lastTimeStepBound;
			this->lastTimeStepAvailable = this->thisTimeStepAvailable;
			this->lastTimeStepBound = this->thisTimeStepBound;
			this->timeElapsed = timeElapsed;

		} // end if time changed
		// end Inits


		//initialize locals
		Real64 tmpPdraw = 0.0;
		Real64 tmpPcharge = 0.0;
		bool drawing = false;
		bool charging = false;
		Real64 Pstorage = 0.0;
		Real64 I0 = 0.0;
		Real64 Volt = 0.0;

		Real64 T0 = 0.0;
		Real64 E0c = 0.0;
		Real64 k = 0.0;
		Real64 c = 0.0;
		Real64 qmaxf = 0.0;
		Real64 Ef = 0.0;
		Real64 qmax = 0.0;
		Real64 Pactual = 0.0;
		Real64 q0 = 0.0;

		// step 1 figure out what is desired of electrical storage system

		if ( powerGenSupply < ( powerDemand ) ) {
			//draw from storage
			tmpPdraw = powerDemand - powerGenSupply;
			tmpPcharge = 0.0;
			drawing = true;
			charging = false;

		} else if ( powerGenSupply > ( powerDemand ) ) {
			//add to storage
			tmpPcharge = powerGenSupply - powerDemand;
			tmpPdraw = 0.0;
			charging = true;
			drawing = false;

		} else if ( powerGenSupply == ( powerDemand ) ) {
			//do nothing
			tmpPcharge = 0.0;
			tmpPdraw = 0.0;
			charging = false;
			drawing = false;
		}

		// EMS override -- intent to draw, charge or hold?
		if ( ( this->eMSOverridePelFromStorage ) && ( ! this->eMSOverridePelIntoStorage ) ) {
			// EMS is calling for specific discharge rate
			tmpPdraw = max( this->eMSValuePelFromStorage, 0.0 );
			tmpPcharge = 0.0;
			drawing = true;
			charging = false;
		} else if ( ( ! this->eMSOverridePelFromStorage ) && ( this->eMSOverridePelIntoStorage ) ) {
			// EMS is calling for specific charge rate
			tmpPcharge = max( this->eMSValuePelIntoStorage, 0.0 );
			tmpPdraw = 0.0;
			drawing = false;
			charging = true;
		} else if ( ( this->eMSOverridePelFromStorage ) && ( this->eMSOverridePelIntoStorage ) ) {
			// EMS is asking to override both
			if ( this->eMSValuePelIntoStorage > this->eMSValuePelFromStorage ) {
				tmpPcharge = this->eMSValuePelIntoStorage - this->eMSValuePelFromStorage;
				tmpPdraw = 0.0;
				drawing = false;
				charging = true;
			} else if ( this->eMSValuePelIntoStorage < this->eMSValuePelFromStorage ) {
				tmpPdraw = this->eMSValuePelFromStorage - this->eMSValuePelIntoStorage;
				tmpPcharge = 0.0;
				drawing = true;
				charging = false;
			} else { //they equal just hold
				tmpPdraw = 0.0;
				tmpPcharge = 0.0;
				drawing = false;
				charging = false;
			}
		}

		//*****************************************************************************************************************

		//  step 2, figure out what is possible for electrical storage draws/charges

		if ( charging ) {

			if ( this->storageModelMode == simpleBucketStorage ) {

				if ( this->lastTimeStepStateOfCharge >= this->maxEnergyCapacity ) {
					// storage full!  no more allowed!
					tmpPcharge = 0.0;
					//       Constrained = .TRUE.
					charging = false;
				}
				if ( tmpPcharge > this->maxPowerStore ) {
					tmpPcharge = this->maxPowerStore;
					//        Constrained = .TRUE.
				}

				//now add energy to storage from charging
				if ( ( this->lastTimeStepStateOfCharge + tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge ) < this->maxEnergyCapacity ) {

					this->thisTimeStepStateOfCharge = this->lastTimeStepStateOfCharge + tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge;
				} else { // would over charge this time step

					tmpPcharge = ( this->maxEnergyCapacity - this->lastTimeStepStateOfCharge ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge );
					//     Constrained = .TRUE.
					this->thisTimeStepStateOfCharge = this->lastTimeStepStateOfCharge + tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge;
				}
				Pstorage = tmpPcharge;

			}

			if ( this->storageModelMode == kiBaMBattery ) {

			//	InternalR = this->internalR;
				qmax = this->maxAhCapacity;
				E0c = this->chargedOCV;
				Real64 E0d = this->dischargedOCV;
				k = this->chargeConversionRate;
				c = this->availableFrac;
				//*************************************************
				//The sign of power and current is negative in charging
				//*************************************************
				Real64 Pw = -tmpPcharge / this->numBattery;
				Real64 q0 = this->lastTimeStepAvailable + this->lastTimeStepBound;

				I0 = 1.0; // Initial assumption
				T0 = std::abs( qmax / I0 ); // Initial Assumption
				qmaxf = qmax * k * c * T0 / ( 1.0 - std::exp( -k * T0 ) + c * ( k * T0 - 1.0 + std::exp( -k * T0 ) ) ); //Initial calculation of a function qmax(I)
				Real64 Xf = q0 / qmaxf;
				Ef = E0d + CurveManager::CurveValue( this->chargeCurveNum, Xf ); //E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
				Volt = Ef - I0 * this->internalR;
				Real64 Inew = Pw / Volt;
				Real64 Tnew = qmaxf / std::abs( Inew );
				Real64 error = 1.0;

				while ( error > 0.0001 ) { //Iteration process to get converged current(I)
					I0 = Inew;
					T0 = Tnew;
					qmaxf = qmax * k * c * T0 / ( 1.0 - std::exp( -k * T0 ) + c * ( k * T0 - 1.0 + std::exp( -k * T0 ) ) );
					Xf = q0 / qmaxf;
					Ef = E0d + CurveManager::CurveValue( this->chargeCurveNum, Xf ); //E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
					Volt = Ef - I0 * this->internalR;
					Inew = Pw / Volt;
					Tnew = std::abs( qmaxf / Inew ); // ***Always positive here
					error = std::abs( Inew - I0 );
				}

				Real64 dividend = -k * c * qmax + k * this->lastTimeStepAvailable * std::exp( -k * DataHVACGlobals::TimeStepSys ) + q0 * k * c * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) );
				Real64 divisor = 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) + c * ( k * DataHVACGlobals::TimeStepSys - 1 + std::exp( -k * DataHVACGlobals::TimeStepSys ) );
				Real64 Imax = dividend / divisor;
				// Below: This is the limit of charging current from Charge Rate Limit (input)
				Imax = max( Imax, - ( qmax - q0 ) * this->maxChargeRate );
				
				if ( std::abs( I0 ) <= std::abs( Imax ) ) {
					I0 = Pw / Volt;
					Pactual = I0 * Volt;
				} else {
					I0 = Imax;
					qmaxf = 80.0; //Initial assumption to solve the equation using iterative method
					error = 10.0; //Initial assumption ...
					while ( error > 0.001 ) {
						// *** I0(current) should be positive for this calculation
						Real64 RHS = ( qmax * k * c * qmaxf / std::abs( I0 ) ) / ( 1.0 - std::exp( -k * qmaxf / std::abs( I0 ) ) + c * ( k * qmaxf / std::abs( I0 ) - 1.0 + std::exp( -k * qmaxf / std::abs( I0 ) ) ) );
						error = std::abs( qmaxf - RHS );
						qmaxf = RHS;
					}
				}

			} // end KiBaM charging

		} //charging

		if ( drawing ) {
			if ( this->storageModelMode == simpleBucketStorage ) {

				if ( this->lastTimeStepStateOfCharge <= 0.0 ) {
					// storage empty  no more allowed!
					tmpPdraw = 0.0;
					drawing = false;
				}
				if ( tmpPdraw > this->maxPowerDraw ) {
					tmpPdraw = this->maxPowerDraw;
				}

				//now take energy from storage by drawing  (amplified by energetic effic)
				if ( ( this->lastTimeStepStateOfCharge - tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->energeticEfficDischarge ) > 0.0 ) {

					this->thisTimeStepStateOfCharge = this->lastTimeStepStateOfCharge - tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->energeticEfficDischarge;
				} else { //would over drain storage this timestep so reduce tmpPdraw
					tmpPdraw = this->lastTimeStepStateOfCharge * this->energeticEfficDischarge / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
					this->thisTimeStepStateOfCharge = this->lastTimeStepStateOfCharge - tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->energeticEfficDischarge;
					this->thisTimeStepStateOfCharge = max( this->thisTimeStepStateOfCharge, 0.0 );
				}

				this->thermLossRate = tmpPdraw * ( 1.0 / this->energeticEfficDischarge - 1.0 );
				Pstorage = -tmpPdraw;
			} // simple discharging

			if ( this->storageModelMode == kiBaMBattery ) {

				//**********************************************
				//The sign of power and current is positive in discharging
				//**********************************************

				Real64 Pw = tmpPdraw / this->numBattery;
				Real64 q0 = this->lastTimeStepAvailable + this->lastTimeStepBound;
				bool const ok = this->determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, this->dischargeCurveNum, k, c, qmax, E0c, this->internalR );
				if ( !ok ) {
					ShowFatalError( "ElectricLoadCenter:Storage:Battery named=\"" + this->name + "\". Battery discharge current could not be estimated due to iteration limit reached. " );
					//issue #5301, need more diagnostics for this. 
				}

				Real64 dividend = k * this->lastTimeStepAvailable * std::exp( -k * DataHVACGlobals::TimeStepSys ) + q0 * k * c * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) );
				Real64 divisor = 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) + c * ( k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp( -k * DataHVACGlobals::TimeStepSys ) );
				Real64 Imax = dividend / divisor;
				Imax = min( Imax, this->maxDischargeI );
				if ( std::abs( I0 ) <= Imax ) {
					I0 = Pw / Volt;
					Pactual = I0 * Volt;
				} else {
					I0 = Imax;
					qmaxf = 10.0; //Initial assumption to solve the equation using iterative method
					Real64 error = 10.0; //Initial assumption ...
					while ( error > 0.001 ) {
						Real64 RHS = ( qmax * k * c * qmaxf / I0 ) / ( 1.0 - std::exp( -k * qmaxf / I0 ) + c * ( k * qmaxf / I0 - 1 + std::exp( -k * qmaxf / I0 ) ) );
						error = std::abs( qmaxf - RHS );
						qmaxf = RHS;
					}
					Real64 Xf = ( qmax - q0 ) / qmaxf;
					Ef = E0c + CurveManager::CurveValue( this->dischargeCurveNum, Xf );
					Volt = Ef - I0 * this->internalR;
				}

				if ( Volt < this->cutoffV ) {
					I0 = 0.0;
				}
			} // end KiBaM discharging

		} //drawing

		// correct if not available.
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr ) == 0.0 ) {
			if ( ( ! this->eMSOverridePelFromStorage ) && ( ! this->eMSOverridePelIntoStorage ) ) {
				charging = false;
				drawing = false;
	//TODO this legacy looks wrong, separate for charging and discharging
			}
		}

		if ( this->storageModelMode == simpleBucketStorage ) {
			if ( ( ! charging ) && ( ! drawing ) ) {

				this->thisTimeStepStateOfCharge = this->lastTimeStepStateOfCharge;
				this->pelIntoStorage = 0.0;
				this->pelFromStorage = 0.0;
				Pstorage = 0.0;
			}

			if ( Pstorage >= 0.0 ) {

				this->pelIntoStorage = Pstorage;
				this->pelFromStorage = 0.0;
			}

			if ( Pstorage < 0.0 ) {

				this->pelIntoStorage = 0.0;
				this->pelFromStorage = -Pstorage;

			}

			this->electEnergyinStorage = this->thisTimeStepStateOfCharge;
			this->storedPower = this->pelIntoStorage;
			this->storedEnergy = this->pelIntoStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
			this->decrementedEnergyStored = -1.0 * this->storedEnergy;
			this->drawnPower = this->pelFromStorage;
			this->drawnEnergy = this->pelFromStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
			this->thermLossRate = max( this->storedPower * ( 1.0 - this->energeticEfficCharge ), this->drawnPower * ( 1.0 - this->energeticEfficDischarge ) );
			this->thermLossEnergy = this->thermLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

			if ( this->zoneNum > 0 ) { // set values for zone heat gains
				this->qdotConvZone = ( 1.0 - this->zoneRadFract ) * this->thermLossRate;
				this->qdotRadZone = ( this->zoneRadFract ) * this->thermLossRate;
			}

			StorageStoredPower = this->storedPower;
			StorageDrawnPower = this->drawnPower;
		} // Outputs for simple battery model

		if ( this->storageModelMode == kiBaMBattery ) {

			if ( ( ! charging ) && ( ! drawing ) ) {
				this->thisTimeStepAvailable = this->lastTimeStepAvailable;
				this->thisTimeStepBound = this->lastTimeStepBound;
				I0 = 0.0;
				Volt = 0.0;
				q0 = this->lastTimeStepAvailable + this->lastTimeStepBound;
			} else {
				Real64 newAvailable = this->lastTimeStepAvailable * std::exp( -k * DataHVACGlobals::TimeStepSys ) + ( q0 * k * c - I0 ) * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) ) / k - I0 * c * ( k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp( -k * DataHVACGlobals::TimeStepSys ) ) / k;
				Real64 newBound = this->lastTimeStepBound * std::exp( -k * DataHVACGlobals::TimeStepSys ) + q0 * ( 1.0 - c ) * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) ) - I0 * ( 1.0 - c ) * ( k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp( -k * DataHVACGlobals::TimeStepSys ) ) / k;
				this->thisTimeStepAvailable = max( 0.0, newAvailable );
				this->thisTimeStepBound = max( 0.0, newBound );
			}

			Pactual = I0 * Volt;
			Real64 TotalSOC = this->thisTimeStepAvailable + this->thisTimeStepBound;

			//output1
			if ( TotalSOC > q0 ) {
				this->storageMode = 2;
//TODO change sign convention here		this->storedPower = -1.0 * Volt * I0 * numbattery;
//TODO change sign convention here				this->storedEnergy = -1.0 * Volt * I0 * numbattery * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
				this->storedPower = Volt * I0 * this->numBattery;
				this->storedEnergy =  Volt * I0 * this->numBattery * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
				this->decrementedEnergyStored = this->storedEnergy; 
				this->drawnPower = 0.0;
				this->drawnEnergy = 0.0;

			} else if ( TotalSOC < q0 ) {
				this->storageMode = 1;
				this->storedPower = 0.0;
				this->storedEnergy = 0.0;
				this->decrementedEnergyStored = 0.0;
				this->drawnPower = Volt * I0 * this->numBattery;
				this->drawnEnergy = Volt * I0 * this->numBattery * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

			} else {
				this->storageMode = 0;
				this->storedPower = 0.0;
				this->storedEnergy = 0.0;
				this->decrementedEnergyStored = 0.0;
				this->drawnPower = 0.0;
				this->drawnEnergy = 0.0;
			}

			this->absoluteSOC = TotalSOC * this->numBattery;
			this->fractionSOC = TotalSOC / qmax;
			this->batteryCurrent = I0 * this->parallelNum;
			this->batteryVoltage = Volt * this->seriesNum;
			this->thermLossRate = this->internalR * pow_2( I0 ) * this->numBattery;
			this->thermLossEnergy = this->internalR * pow_2( I0 ) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->numBattery;

			if ( this->zoneNum > 0 ) { // set values for zone heat gains
				this->qdotConvZone = ( ( 1.0 - this->zoneRadFract ) * this->thermLossRate ) * this->numBattery;
				this->qdotRadZone = ( ( this->zoneRadFract ) * this->thermLossRate ) * this->numBattery;
			}

			StorageStoredPower = this->storedPower;
			StorageDrawnPower = this->drawnPower;

		} // Outputs for kibam model


	}

	Real64
	ElectricStorage::getDrawnPower()
	{
		return this->drawnPower;
	}

	Real64
	ElectricStorage::getStoredPower()
	{
		return this->storedPower;
	}

	Real64
	ElectricStorage::getDrawnEnergy()
	{
		return this->drawnEnergy;
	}

	Real64
	ElectricStorage::getStoredEnergy()
	{
		return this->storedEnergy;
	}


	bool
	ElectricStorage::determineCurrentForBatteryDischarge(
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
	)
	{
		curI0 = 10.0; // Initial assumption
		curT0 = qmax / curI0; // Initial Assumption
		Real64 qmaxf = qmax * k * c * curT0 / ( 1.0 - std::exp( -k * curT0 ) + c * ( k * curT0 - 1.0 + std::exp( -k * curT0 ) ) ); //Initial calculation of a function qmax(I)
		Real64 Xf = ( qmax - q0 ) / qmaxf;
		Real64 Ef = E0c + CurveManager::CurveValue( CurveNum, Xf ); //E0d+Ac*Xf+Cc*X/(Dc-Xf)
		curVolt = Ef - curI0 * InternalR;
		Real64 Inew = Pw / curVolt;
		Real64 Tnew = qmaxf / Inew;
		Real64 error = 1.0;
		int countForIteration = 0;
		bool exceedIterationLimit = false;

		while ( error > 0.0001 ) { // Iteration process to get converged current(I)
			curI0 = Inew;
			curT0 = Tnew;
			qmaxf = qmax * k * c * curT0 / ( 1.0 - std::exp( -k * curT0 ) + c * ( k * curT0 - 1.0 + std::exp( -k * curT0 ) ) );
		//TODO add div by zero protection #5301
			Xf = ( qmax - q0 ) / qmaxf;
			Ef = E0c + CurveManager::CurveValue( CurveNum, Xf ); //E0c+Ad*Xf+Cd*X/(Dd-Xf)
			curVolt = Ef - curI0 * InternalR;
		//TODO add div by zero protection #5301
			Inew = Pw / curVolt;
		//TODO add div by zero protection #5301
			Tnew = qmaxf / Inew;
			error = std::abs( Inew - curI0 );
			++countForIteration;
			if ( countForIteration > 1000 ) {
				exceedIterationLimit = true;
				//Issue #5301 need more diagnostics for this case
				break;
			}
		}
		return (!exceedIterationLimit);

	}

	void
	ElectricStorage::rainflow(
		int const numbin, // numbin = constant value
		Real64 const input, // input = input value from other object (battery model)
		std::vector < Real64 > B1, // stores values of points, calculated here - stored for next timestep
		std::vector < Real64 > X, // stores values of two data point difference, calculated here - stored for next timestep
		int & count, // calculated here - stored for next timestep in main loop
		std::vector < Real64 > Nmb, // calculated here - stored for next timestep in main loop
		std::vector < Real64 > OneNmb // calculated here - stored for next timestep in main loop
	//	int const dim // end dimension of array
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Y. KyungTae & W. Wang
		//       DATE WRITTEN   July-August, 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Rainflow cycle counting for battery life calculation

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Ariduru S. 2004. Fatigue life calculation by rainflow cycle counting method.
		//                  Master Thesis, Middle East Technical University.

		// Argument array dimensioning
	//	B1.dim( {1,dim} );
	//	X.dim( {1,dim} );
	//	Nmb.dim( {1,numbin} );
	//	OneNmb.dim( {1,numbin} );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//Array B1 stores the value of points
		//Array X stores the value of two data points' difference.

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int num;

		X[ count ] = input - B1[ count - 1 ]; // calculate the difference between two data (current and previous)

		// Get rid of the data if it is not peak nor valley
		// The value of count means the number of peak or valley points added to the arrary B10/B1, not including the
		// first point B10(0)/B1(0). Therefore, even if count =2, B1(count-2) is still valid.
		if ( count >= 3 ) {
			//  The following check on peak or valley may be not necessary in most times because the same check is made in the
			//  upper-level subroutine. However, it does not hurt to leave it here.
			if ( X[ count ] * X[ count - 1 ] >= 0 ) {
				X[ count - 1 ] = B1[ count ] - B1[ count - 2 ];
				this->shift( B1, count - 1, count, B1 ); // Get rid of (count-1) row in B1
				this->shift( X, count, count, X );
				--count; // If the value keep increasing or decreasing, get rid of the middle point.
			} // Only valley and peak will be stored in the matrix, B1

			if ( ( count == 3 ) && ( std::abs( X[ 2 ] ) <= std::abs( X[ 3 ] ) ) ) {
				//  This means the starting point is included in X(2), a half cycle is counted according to the rain flow
				//  algorithm specified in the reference (Ariduru S. 2004)
				num = nint( ( std::abs( X[ 2 ] ) * numbin * 10 + 5 ) / 10 ); // Count half cycle
				Nmb[ num ] += 0.5;
				//B1 = eoshift( B1, 1 ); // Once counting a half cycle, get rid of the value.
				B1.erase( B1.begin() );
				B1.push_back( 0.0 );
				//X = eoshift( X, 1 );
				X.erase( X.begin() );
				X.push_back( 0.0 );
				--count; // The number of matrix, B1 and X1 decrease.
			}
		} // Counting cyle end
		//*** Note: The value of "count" changes in the upper "IF LOOP"

		if ( count >= 4 ) { //count 1 cycle
			while ( std::abs( X[ count ] ) > std::abs( X[ count - 1 ] ) ) {
				//  This means that the starting point is not included in X(count-1). a cycle is counted according to the rain flow
				//  algorithm specified in the reference (Ariduru S. 2004)
				num = nint( ( std::abs( X[ count - 1 ] ) * numbin * 10 + 5 ) / 10 );
				++Nmb[ num ];

				//     X(count-2) = ABS(X(count))-ABS(X(count-1))+ABS(X(count-2))
				X[ count - 2 ] = B1[ count ] - B1[ count - 3 ]; // Updating X needs to be done before shift operation below

				this->shift( B1, count - 1, count, B1 ); // Get rid of two data points one by one
				this->shift( B1, count - 2, count, B1 ); // Delete one point

				this->shift( X, count, count, X ); // Get rid of two data points one by one
				this->shift( X, count - 1, count, X ); // Delete one point

				count -= 2; // If one cycle is counted, two data points are deleted.
				if ( count < 4 ) break; // When only three data points exists, one cycle cannot be counted.
			}
		}

		++count;

		// Check the rest of the half cycles every time step
		OneNmb = Nmb; // Array Nmb (Bins) will be used for the next time step later.
		// OneNmb is used to show the current output only.
		// Ideally, the following clean-up counting is needed at the last system time step in each simulation environemnt.
		// Because of the difficulty in knowing the above information, the clean-up counting is skipped. Skipping this has
		// little impact on the simulation results.
		//   DO k = 1, count-1
		//     num = NINT((ABS(X(k))*numbin*10+5)/10) !Bin number
		//     OneNmb(num) = OneNmb(num)+0.5d0
		//   ENDDO

	}

	void
	ElectricStorage::shift(
		std::vector < Real64 > A,
		int const m,
		int const n,
		std::vector < Real64 > B
	//	int const dim // end dimension of arrays
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Y. KyungTae & W. Wang
		//       DATE WRITTEN   July-August, 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Utility subroutine for rainflow cycle counting


		// Argument array dimensioning
	//	A.dim( {1,dim} );
	//	B.dim( {1,dim} );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ShiftNum; // Loop variable

		for ( ShiftNum = 1; ShiftNum <= m - 1; ++ShiftNum ) {
			B[ ShiftNum ] = A[ ShiftNum ];
		}

		for ( ShiftNum = m; ShiftNum <= n; ++ShiftNum ) {
			B[ ShiftNum ] = A[ ShiftNum + 1 ];
		}
	}

	// constructor
	ElectricTransformer::ElectricTransformer(
		std::string objectName
	)
	{
		this->myOneTimeFlag = true;
		this->availSchedPtr = 0;
		this->usageMode = useNotYetSet;
		this->heatLossesDestination = heatLossNotDetermined;
		this->zoneNum = 0;
		this->zoneRadFrac = 0.0;
		this->ratedCapacity = 0.0;
		this->phase = 0;
		this->factorTempCoeff = 0.0;
		this->tempRise = 0.0;
		this->eddyFrac = 0.0;
		this->performanceInputMode = perfInputMethodNotSet;
		this->ratedEfficiency = 0.0;
		this->ratedPUL = 0.0;
		this->ratedTemp = 0.0;
		this->maxPUL = 0.0;
		this->considerLosses = true;
		this->ratedNL = 0.0;
		this->ratedLL = 0.0;
		this->numLoadCenters = 0;
		this->overloadErrorIndex = 0;
		this->efficiency = 0.0;
		this->powerIn = 0.0;
		this->energyIn = 0.0;
		this->powerOut = 0.0;
		this->energyOut = 0.0;
		this->noLoadLossRate = 0.0;
		this->noLoadLossEnergy = 0.0;
		this->loadLossRate = 0.0;
		this->loadLossEnergy = 0.0;
		this->thermalLossRate = 0.0;
		this->thermalLossEnergy = 0.0;
		this->elecUseUtility = 0.0;
		this->elecProducedCoGen = 0.0;
		this->qdotConvZone = 0.0;
		this->qdotRadZone = 0.0;
				
		std::string const routineName = "ElectricTransformer constructor ";
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool errorsFound = false;
		int transformerIDFObjectNum = 0;
		DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Transformer";

		transformerIDFObjectNum = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Transformer",  objectName );
		if ( transformerIDFObjectNum > 0 ) {
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, transformerIDFObjectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );
			this->name  = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?
			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			if ( DataIPShortCuts::lAlphaFieldBlanks( 3 ) ) {
				this->usageMode = powerInFromGrid; //default
			} else if ( InputProcessor::SameString(DataIPShortCuts::cAlphaArgs( 3 ), "PowerInFromGrid" ) ) {
				this->usageMode = powerInFromGrid;
			} else if ( InputProcessor::SameString(DataIPShortCuts::cAlphaArgs( 3 ), "PowerOutFromOnsiteGeneration" ) ) {
				this->usageMode = powerOutFromBldgToGrid;
			} else if ( InputProcessor::SameString(DataIPShortCuts::cAlphaArgs( 3 ), "LoadCenterProductionConditioning" ) ) {
				this->usageMode = powerFromLoadCenterToBldg;

			} else {
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					errorsFound = true;
			}

			this->zoneNum = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 4 ), DataHeatBalance::Zone );
			if ( this->zoneNum > 0 ) this->heatLossesDestination = zoneGains;
			if ( this->zoneNum == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					this->heatLossesDestination = lostToOutside;
				} else {
					this->heatLossesDestination = lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = " + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Zone name not found. Transformer heat losses will not be added to a zone" );
					// continue with simulation but storage losses not sent to a zone.
				}
			}
			this->zoneRadFrac   = DataIPShortCuts::rNumericArgs( 1 );
			this->ratedCapacity = DataIPShortCuts::rNumericArgs( 2 );
			this->phase         = DataIPShortCuts::rNumericArgs( 3 );

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 5 ), "Copper" ) ) {
				this->factorTempCoeff = 234.5;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 5 ), "Aluminum" ) ) {
				this->factorTempCoeff = 225.0;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + " = " + DataIPShortCuts::cAlphaArgs( 5 ) );
				errorsFound = true;
			}
			this->tempRise = DataIPShortCuts::rNumericArgs( 4 );
			this->eddyFrac = DataIPShortCuts::rNumericArgs( 5 );

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "RatedLosses" ) ) {
				this->performanceInputMode = lossesMethod;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "NominalEfficiency" ) ) {
				this->performanceInputMode = efficiencyMethod;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" +  DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 6 ) + " = " + DataIPShortCuts::cAlphaArgs( 6 ) );
				errorsFound = true;
			}

			this->ratedNL         = DataIPShortCuts::rNumericArgs( 6 );
			this->ratedLL         = DataIPShortCuts::rNumericArgs( 7 );
			this->ratedEfficiency = DataIPShortCuts::rNumericArgs( 8 );
			this->ratedPUL        = DataIPShortCuts::rNumericArgs( 9 );
			this->ratedTemp       = DataIPShortCuts::rNumericArgs( 10 );
			this->maxPUL          = DataIPShortCuts::rNumericArgs( 11 );
			//Check the input for MaxPUL if the performance input method is EfficiencyMethod
			if ( this->performanceInputMode == efficiencyMethod ) {
				if ( DataIPShortCuts::lNumericFieldBlanks( 11 ) ) {
					this->maxPUL = this->ratedPUL;
				} else if ( this->maxPUL <= 0 || this->maxPUL > 1 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cNumericFieldNames( 11 ) + "=[" + General::RoundSigDigits( DataIPShortCuts::rNumericArgs( 11 ), 3 ) + "]." );
					ShowContinueError( "Entered value must be > 0 and <= 1." );
					errorsFound = true;
				}
			}
			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 7 ), "Yes" ) ) {
				this->considerLosses = true;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 7 ), "No" ) ) {
				this->considerLosses = false;
			} else {
				if ( this->usageMode == powerInFromGrid ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + " = " + DataIPShortCuts::cAlphaArgs( 7 ) );
					errorsFound = true;
				}
			}

			int numAlphaBeforeMeter = 7;
			int numWiredMeters = numAlphas - numAlphaBeforeMeter;

			if ( this->usageMode == powerInFromGrid ) {

				//Provide warning if no meter is wired to a transformer used to get power from the grid
				if ( numWiredMeters <= 0 ) {
					ShowWarningError( routineName + "ElectricLoadCenter:Transformer=\"" + this->name + "\":" );
					ShowContinueError( "ISOLATED Transformer: No meter wired to a transformer used to input power from grid" );
				}

				this->wiredMeterNames.resize( numWiredMeters, "" );
				this->wiredMeterPtrs.resize( numWiredMeters, 0 );
				this->specialMeter.resize( numWiredMeters, false );

				//Meter check deferred because they may have not been "loaded" yet,
				for ( auto loopCount = 0; loopCount < numWiredMeters; ++loopCount ) {
					this->wiredMeterNames[ loopCount ] = InputProcessor::MakeUPPERCase( DataIPShortCuts::cAlphaArgs( loopCount + numAlphaBeforeMeter + 1 ) );
					//Assign SpecialMeter as TRUE if the meter name is Electricity:Facility or Electricity:HVAC
					if ( InputProcessor::SameString( this->wiredMeterNames[ loopCount ], "Electricity:Facility" ) || InputProcessor::SameString( this->wiredMeterNames[ loopCount ], "Electricity:HVAC" ) ) {
						this->specialMeter[ loopCount ] = true;
					} else {
						this->specialMeter[ loopCount ] = false;
					}
				}
			}
			SetupOutputVariable( "Transformer Efficiency []", this->efficiency, "System", "Average", this->name );
			SetupOutputVariable( "Transformer Input Electric Power [W]", this->powerIn, "System", "Average", this->name );
			SetupOutputVariable( "Transformer Input Electric Energy [J]", this->energyIn, "System", "Sum", this->name );
			SetupOutputVariable( "Transformer Output Electric Power [W]", this->powerOut, "System", "Average", this->name );
			SetupOutputVariable( "Transformer Output Electric Energy [J]", this->energyOut, "System", "Sum", this->name );
			SetupOutputVariable( "Transformer No Load Loss Rate [W]", this->noLoadLossRate, "System", "Average", this->name );
			SetupOutputVariable( "Transformer No Load Loss Energy [J]", this->noLoadLossEnergy, "System", "Sum", this->name );
			SetupOutputVariable( "Transformer Load Loss Rate [W]", this->loadLossRate, "System", "Average", this->name );
			SetupOutputVariable( "Transformer Load Loss Energy [J]", this->loadLossEnergy, "System", "Sum", this->name );
			SetupOutputVariable( "Transformer Thermal Loss Rate [W]", this->thermalLossRate, "System", "Average", this->name );
			SetupOutputVariable( "Transformer Thermal Loss Energy [J]", this->thermalLossEnergy, "System", "Sum", this->name );
			SetupOutputVariable( "Transformer Distribution Electric Loss Energy [J]", this->elecUseUtility, "System", "Sum", this->name, _, "Electricity", "ExteriorEquipment", "Transformer", "System" );
			SetupOutputVariable( "Transformer Cogeneration Electric Loss Energy [J]", this->elecProducedCoGen, "System", "Sum", this->name, _, "ElectricityProduced", "COGENERATION", _, "System" );

			if ( this->zoneNum > 0 ) {
				SetupZoneInternalGain( this->zoneNum, "ElectricLoadCenter:Transformer", this->name, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterTransformer, this->qdotConvZone, _, this->qdotRadZone );
			}

		} else {
			ShowSevereError( routineName + " did not find transformer name = " + objectName);
			errorsFound = true;
		}

		if ( errorsFound ) {
			ShowFatalError( routineName + "Preceding errors terminate program." );
		}
	}

	void
	ElectricTransformer::addLoadCenterIndex( 
		int const objectIndex
	)
	{
		++this->numLoadCenters;
		this->loadCenterObjIndexes.push_back( objectIndex );
	}

	std::vector< int >
	ElectricTransformer::getLoadCenterObjIndices()
	{
		return this->loadCenterObjIndexes;

	}

	void
	ElectricTransformer::manageTransformers(
		Real64 const surplusPowerOutFromLoadCenters
	)
	{
		Real64 const ambTempRef = 20.0; // reference ambient temperature (C)
		if ( this->myOneTimeFlag ) {
			// calculate rated no load losses and rated load losses if the performance input method is based on
			// nominal efficiency. This calculation is done only once

			if ( this->performanceInputMode == efficiencyMethod ) {

				Real64 resRef = this->factorTempCoeff + this->tempRise + ambTempRef;
				Real64 resSpecified = this->factorTempCoeff + this->ratedTemp;
				Real64 resRatio = resSpecified / resRef;
				Real64 factorTempCorr = ( 1.0 - this->eddyFrac ) * resRatio + this->eddyFrac * ( 1.0 / resRatio );
				Real64 numerator = this->ratedCapacity * this->ratedPUL * ( 1.0 - this->ratedEfficiency );
				Real64 denominator = this->ratedEfficiency * ( 1.0 + pow_2( this->ratedPUL / this->maxPUL ) );

				this->ratedNL = numerator / denominator;
				this->ratedLL = this->ratedNL / ( factorTempCorr * pow_2( this->maxPUL ) );
			}
			this->myOneTimeFlag = false;
		}

		Real64 elecLoad = 0.0; // transformer load which may be power in or out depending on the usage mode
		Real64 pastElecLoad = 0.0; // transformer load at the previous timestep
		switch ( this->usageMode )
		{
		case powerInFromGrid: {
			for ( std::size_t meterNum = 0; meterNum < this->wiredMeterPtrs.size(); ++meterNum ) {

				if ( DataGlobals::MetersHaveBeenInitialized ) {

					elecLoad += GetInstantMeterValue( this->wiredMeterPtrs[ meterNum ], 1 ) / DataGlobals::TimeStepZoneSec + GetInstantMeterValue( this->wiredMeterPtrs[ meterNum ], 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
					// PastElecLoad store the metered value in the previous time step. This value will be used to check whether
					// a transformer is overloaded or not.
					pastElecLoad += GetCurrentMeterValue( this->wiredMeterPtrs[ meterNum ] ) / DataGlobals::TimeStepZoneSec;
				} else {
					elecLoad = 0.0;
					pastElecLoad = 0.0;
				}

				// Because transformer loss has been accounted for by Electricity:Facility and Electricity:HVAC, the transformer
				// loss needs to be deducted from the metered value. Otherwise, double counting (circular relationship) occurs.
				if ( this->specialMeter[ meterNum ] ) {
					elecLoad = elecLoad - this->loadLossRate - this->noLoadLossRate;

					if ( elecLoad < 0 ) elecLoad = 0.0; //Essential check.
				}

			}

			this->powerOut = elecLoad; //the metered value is transformer's output in PowerInFromGrid mode
			break;
		}
		case powerOutFromBldgToGrid : {
			this->powerIn = surplusPowerOutFromLoadCenters;
			elecLoad = surplusPowerOutFromLoadCenters; // TODO this is input but should be output with the losses, but we don't have them yet. 
			break;
		}
		case powerFromLoadCenterToBldg : {
			//TODO, new configuration for transformer, really part of the specific load center and connects it to the main building bus
			break;
		}
		case useNotYetSet : {
			// do nothing
			break;
		}
		} // switch usage mode


		// check availability schedule
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr ) > 0.0 ) {
			Real64 capacity = this->ratedCapacity;
			Real64 pUL = elecLoad / capacity;

			if ( pUL > 1.0 ) {
				pUL = 1.0;
			}

			//Originally, PUL was used to check whether a transformer is overloaded (PUL > 1.0 or not). However, it was
			//found that ElecLoad obtained from GetInstantMeterVlaue() might refer to intermideiate values before
			//convergence. The intermediate values may issue false warning. This the reason why PastElecLoad obtained
			//by GetCurrentMeterValue() is used here to check overload issue.
			if ( ( pastElecLoad / capacity ) > 1.0 ) {
				if ( this->overloadErrorIndex == 0 ) {
					ShowSevereError( "Transformer Overloaded" );
					ShowContinueError( "Entered in ElectricLoadCenter:Transformer =" + this->name );
				}
				ShowRecurringSevereErrorAtEnd( "Transformer Overloaded: Entered in ElectricLoadCenter:Transformer =" + this->name, this->overloadErrorIndex );
			}

			Real64 tempChange = std::pow( pUL, 1.6 ) * this->tempRise;
			Real64 ambTemp = 20.0;
			if ( this->heatLossesDestination == zoneGains ) {

				ambTemp = DataHeatBalance::ZnAirRpt( this->zoneNum ).MeanAirTemp;
			} else {
				ambTemp = 20.0;
			}

			Real64 resRef = this->factorTempCoeff + this->tempRise + ambTempRef;
			Real64 resSpecified = this->factorTempCoeff + tempChange + ambTemp;
			Real64 resRatio = resSpecified / resRef;
			Real64 factorTempCorr = ( 1.0 - this->eddyFrac ) * resRatio + this->eddyFrac * ( 1.0 / resRatio );

			this->loadLossRate = this->ratedLL * pow_2( pUL ) * factorTempCorr;
			this->noLoadLossRate = this->ratedNL;
		} else { //Transformer is not available.
			this->loadLossRate = 0.0;
			this->noLoadLossRate = 0.0;
		}

		Real64 totalLossRate = this->loadLossRate + this->noLoadLossRate;

		switch ( this->usageMode )
		{
		case powerInFromGrid: {
			this->powerIn = elecLoad + totalLossRate;

			//Transformer losses are wired to the meter via the variable "%ElecUseUtility" only if transformer losses
			//are considered in utility cost. If transformer losses are not considered in utility cost, 0 is assigned
			//to the variable "%ElecUseUtility".
			if ( this->considerLosses ) {
				this->elecUseUtility = totalLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
			} else {
				this->elecUseUtility = 0.0;
			}

			//Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
			//is assigned 0
			this->elecProducedCoGen = 0.0;

			break;
		}

		case powerOutFromBldgToGrid: {
			this->powerOut = elecLoad - totalLossRate;

			if ( this->powerOut < 0 ) this->powerOut = 0.0;

			this->elecProducedCoGen = -1.0 * totalLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

			//Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
			//is assigned 0
			this->elecUseUtility = 0.0;
			break;
		}
		case powerFromLoadCenterToBldg : {
			//TODO, new configuration for transformer, really part of the specific load center and connects it to the main building bus
			break;
		}
		case useNotYetSet : {
			// do nothing
			break;
		}
		} // switch

		if ( this->powerIn <= 0 ) {
			this->efficiency = 0.0;
		} else {
			this->efficiency = this->powerOut / this->powerIn;
		}
		this->noLoadLossEnergy = this->noLoadLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->loadLossEnergy = this->loadLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		this->energyIn = this->powerIn * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->energyOut = this->powerOut * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//   Thermal loss rate may not be equal to Total loss rate. This is the case when surplus power is less than the
		//    calculated total loss rate for a cogeneration transformer. That is why "PowerIn - PowerOut" is used below.
		this->thermalLossRate = this->powerIn - this->powerOut;
		this->thermalLossEnergy = totalLossRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		if ( this->zoneNum > 0 ) { // set values for zone heat gains
			this->qdotConvZone = ( 1.0 - this->zoneRadFrac ) * this->thermalLossRate;
			this->qdotRadZone = ( this->zoneRadFrac ) * this->thermalLossRate;
		}

	}

	void
	ElectricTransformer::setupMeterIndices()
	{
		if (this->usageMode == powerInFromGrid ) {
			for ( std::size_t meterNum = 0; meterNum < this->wiredMeterNames.size(); ++meterNum ) {

				this->wiredMeterPtrs[ meterNum ] = GetMeterIndex( this->wiredMeterNames[ meterNum ] );

				//Check whether the meter is an electricity meter
				//Index function is used here because some resource types are not Electricity but strings containing
				// Electricity such as ElectricityPurchased and ElectricityProduced.
				//It is not proper to have this check in GetInput routine because the meter index may have not been defined
				if ( ! has( GetMeterResourceType( this->wiredMeterPtrs[ meterNum ] ), "Electricity" ) ) {
					EnergyPlus::ShowFatalError( "Non-electricity meter used for " + this->name );
				}
			}
		}
	}

	void
	ElectricTransformer::reinitAtBeginEnvironment()
	{
		this->efficiency        = 0.0;
		this->powerIn           = 0.0;
		this->energyIn          = 0.0;
		this->powerOut          = 0.0;
		this->energyOut         = 0.0;
		this->noLoadLossRate    = 0.0;
		this->noLoadLossEnergy  = 0.0;
		this->loadLossRate      = 0.0;
		this->loadLossEnergy    = 0.0;
		this->thermalLossRate   = 0.0;
		this->thermalLossEnergy = 0.0;
		this->elecUseUtility    = 0.0;
		this->elecProducedCoGen = 0.0;
		this->qdotConvZone      = 0.0;
		this->qdotRadZone       = 0.0;
	}

	void
	ElectricTransformer::reinitZoneGainsAtBeginEnvironment()
	{
		this->qdotConvZone      = 0.0;
		this->qdotRadZone       = 0.0;
	}

} // ElectricPowerService namespace

} // EnergyPlus namespace
