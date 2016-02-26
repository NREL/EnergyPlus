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

	std::unique_ptr< ElectricPowerServiceManager > facilityElectricServiceObj;

	void
	clearFacilityElectricPowerServiceObject()
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
		if ( this->getInputFlag_ ) {
			this->getPowerManagerInput();
			this->getInputFlag_ = false;
		}

		if ( DataGlobals::MetersHaveBeenInitialized && this->setupMeterIndexFlag_ ) {
			this->setupMeterIndices();
			this->setupMeterIndexFlag_ = false;
		}

		if ( DataGlobals::BeginEnvrnFlag && this->newEnvironmentFlag_ ) {
			this->reinitAtBeginEnvironment();
			this->newEnvironmentFlag_ = false;
		}
		if ( ! DataGlobals::BeginEnvrnFlag ) this->newEnvironmentFlag_ = true;

		// retrieve data from meters for demand and production
		this->totalBldgElecDemand_ = GetInstantMeterValue( this->elecFacilityIndex_, 1 ) / DataGlobals::TimeStepZoneSec;
		this->totalHVACElecDemand_ = GetInstantMeterValue( this->elecFacilityIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->totalElectricDemand_ = this->totalBldgElecDemand_ + this->totalHVACElecDemand_;
		this->elecProducedPVRate_ = GetInstantMeterValue( this->elecProducedPVIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedWTRate_ = GetInstantMeterValue( this->elecProducedWTIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedStorageRate_ = GetInstantMeterValue( this->elecProducedStorageIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedCoGenRate_ = GetInstantMeterValue( this->elecProducedCoGenIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedPowerConversionRate_ = GetInstantMeterValue( this->elecProducedPowerConversionIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

		this->wholeBldgRemainingLoad_ = this->totalElectricDemand_;

		if ( UpdateMetersOnly ) { // just update record keeping, don't resimulate load centers
			if ( this->facilityPowerInTransformerPresent_ ) {
				this->facilityPowerInTransformerObj_->manageTransformers( 0.0 );
			}

			this->updateWholeBuildingRecords();
			return;
		}

		for ( auto & e : this->elecLoadCenterObjs ) {
			e->manageElecLoadCenter( firstHVACIteration, this->wholeBldgRemainingLoad_ );
		}

		this->updateWholeBuildingRecords();
		// The transformer call should be put outside of the "Load Center" loop because
		// 1) A transformer may be for utility, not for load center
		// 2) A tansformer may be shared by multiple load centers
		if ( this->facilityPowerInTransformerPresent_ ) {
			this->facilityPowerInTransformerObj_->manageTransformers( 0.0 );
		}

		this->updateWholeBuildingRecords();
		if ( this->powerOutTransformerObj_ != nullptr ){
			this->powerOutTransformerObj_->manageTransformers( this->electSurplusRate_ );
		}

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
		if ( this->facilityPowerInTransformerPresent_ ) {
			this->facilityPowerInTransformerObj_->reinitZoneGainsAtBeginEnvironment();
		}
		if ( this->powerOutTransformerObj_ != nullptr ) {
				this->powerOutTransformerObj_->reinitZoneGainsAtBeginEnvironment();
		}
		if ( this->numLoadCenters_ > 0 ) {
			for ( auto loop = 0; loop < this->numLoadCenters_; ++loop ) {
				this->elecLoadCenterObjs[ loop ]->reinitZoneGainsAtBeginEnvironment();
			}
		}
	}

	void
	ElectricPowerServiceManager::getPowerManagerInput()
	{
		std::string const routineName = "ElectricPowerServiceManager  getPowerManagerInput ";

		this->numLoadCenters_ = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Distribution" );

		if ( this->numLoadCenters_ > 0 ){
			for ( auto iLoadCenterNum = 1; iLoadCenterNum <= this->numLoadCenters_; ++iLoadCenterNum ){
				// call Electric Power Load Center constructor, in place
				this->elecLoadCenterObjs.emplace_back( new ElectPowerLoadCenter ( iLoadCenterNum) );
			}
	
		} else {
			// issue #4639. see if there are any generators, inverters, converters, or storage devcies, that really need a ElectricLoadCenter:Distribution
			int numGenLists   = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Generators" );
			if ( numGenLists > 0 ) {
				ShowWarningError( "ElectricLoadCenter:Generators input object requires an ElectricLoadCenterDistribution input object." );
			}
			int numInverters  = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Inverter:Simple" );
				numInverters += InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Inverter:FunctionOfPower" );
				numInverters += InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Inverter:LookUpTable" );
			if ( numInverters > 0 ) {
				ShowWarningError( "ElectricLoadCenter:Inverter:* input objects require an ElectricLoadCenter:Distribution input object." );
			}
			int numStorage    = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Storage:Simple" );
				numStorage   += InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Storage:Battery" );
			if ( numStorage > 0 ) {
				ShowWarningError( "ElectricLoadCenter:Storage:* input objects require an ElectricLoadCenter:Distribution input object." );
			}
			int numGenerators  = InputProcessor::GetNumObjectsFound( "Generator:InternalCombustionEngine" );
				numGenerators += InputProcessor::GetNumObjectsFound( "Generator:CombustionTurbine" );
				numGenerators += InputProcessor::GetNumObjectsFound( "Generator:MicroCHP" );
				numGenerators += InputProcessor::GetNumObjectsFound( "Generator:FuelCell" );
				numGenerators += InputProcessor::GetNumObjectsFound( "Generator:Photovoltaic" );
				numGenerators += InputProcessor::GetNumObjectsFound( "Generator:WindTurbine" );
			if ( numGenerators > 0 ) {
				ShowWarningError( "Electric generator input objects require and ElectricLoadCenter:Distribution input object." );
			}
				// if user input did not include an Electric Load center, create a simple default one here for reporting purposes
			//   but only if there are any other electricity components set up (yet) for metering
			int anyElectricityPresent = GetMeterIndex( "ELECTRICITY:FACILITY" );
			if ( anyElectricityPresent > 0 ) {
				this->elecLoadCenterObjs.emplace_back( new ElectPowerLoadCenter ( 0 ) );
				this->numLoadCenters_ = 1;
			}
		}

		// see if there are any transformers of the type powerInFromGrid
		this->numTransformers_ = InputProcessor::GetNumObjectsFound( "ElectricLoadCenter:Transformer" );

		if ( this->numTransformers_ > 0 ) {
			int numAlphas; // Number of elements in the alpha array
			int numNums; // Number of elements in the numeric array
			int iOStat; // IO Status when calling get input subroutine
			int facilityPowerInTransformerIDFObjNum = 0;
			bool foundInFromGridTransformer = false;


			DataIPShortCuts::cCurrentModuleObject =  "ElectricLoadCenter:Transformer";
			for ( auto loopTransformer = 1; loopTransformer <= this->numTransformers_; ++loopTransformer) {
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, loopTransformer, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, iOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

				if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "PowerInFromGrid" ) ) {
					if ( ! foundInFromGridTransformer ) {
						foundInFromGridTransformer = true;
						facilityPowerInTransformerIDFObjNum = loopTransformer;
						this->facilityPowerInTransformerName_ = DataIPShortCuts::cAlphaArgs( 1 );
						this->facilityPowerInTransformerPresent_ = true;
					} else {
						// should only have one transformer in input that is PowerInFromGrid
						ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
						ShowContinueError("Only one transformer with Usage PowerInFromGrid can be used, first one in input file will be used and the simulation continues...");
					}
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "PowerOutToGrid" ) ) {
					if ( this->powerOutTransformerObj_ == nullptr ) {
						++this->numPowerOutTransformers_;
						this->powerOutTransformerName_ = DataIPShortCuts::cAlphaArgs( 1 );
						this->powerOutTransformerObj_ = std::unique_ptr < ElectricTransformer > ( new ElectricTransformer ( this->powerOutTransformerName_ ) );
					
					} else {
						ShowWarningError( "Found more than one transformer set to PowerOutFromOnsiteGeneration, however only the first one will be used." );
					}

				}
			}
			if ( foundInFromGridTransformer ) {
				//call transformer constructor
				facilityPowerInTransformerObj_ = std::unique_ptr < ElectricTransformer > ( new ElectricTransformer ( this->facilityPowerInTransformerName_ ) );
			}
		} // if transformers

		if ( this->numLoadCenters_ > 0 ) { 
			SetupOutputVariable( "Facility Total Purchased Electric Power [W]", this->electPurchRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Facility Total Purchased Electric Energy [J]", this->electricityPurch_, "System", "Sum", this->name_, _, "ElectricityPurchased", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Facility Total Surplus Electric Power [W]", this->electSurplusRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Facility Total Surplus Electric Energy [J]", this->electricitySurplus_, "System", "Sum", this->name_, _, "ElectricitySurplusSold", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Facility Net Purchased Electric Power [W]", this->electricityNetRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Facility Net Purchased Electric Energy [J]", this->electricityNet_, "System", "Sum", this->name_, _, "ElectricityNet", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Facility Total Building Electric Demand Power [W]", this->totalBldgElecDemand_, "System", "Average", this->name_ );
			SetupOutputVariable( "Facility Total HVAC Electric Demand Power [W]", this->totalHVACElecDemand_, "System", "Average", this->name_ );
			SetupOutputVariable( "Facility Total Electric Demand Power [W]", this->totalElectricDemand_, "System", "Average", this->name_ );

			SetupOutputVariable( "Facility Total Produced Electric Power [W]", this->electProdRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Facility Total Produced Electric Energy [J]", this->electricityProd_, "System", "Sum", this->name_ );

			this->reportPVandWindCapacity();

			this->sumUpNumberOfStorageDevices();

			this->checkLoadCenters(); // for issue #5302.  
		}
	}

	void
	ElectricPowerServiceManager::setupMeterIndices()
	{
		this->elecFacilityIndex_        = EnergyPlus::GetMeterIndex( "Electricity:Facility" );
		this->elecProducedCoGenIndex_   = EnergyPlus::GetMeterIndex( "Cogeneration:ElectricityProduced" );
		this->elecProducedPVIndex_      = EnergyPlus::GetMeterIndex( "Photovoltaic:ElectricityProduced" );
		this->elecProducedWTIndex_      = EnergyPlus::GetMeterIndex( "WindTurbine:ElectricityProduced" );
		this->elecProducedStorageIndex_ = EnergyPlus::GetMeterIndex( "ElectricStorage:ElectricityProduced" );
		this->elecProducedPowerConversionIndex_ =  EnergyPlus::GetMeterIndex( "PowerConversion:ElectricityProduced" );

		if ( this->numLoadCenters_ > 0 ){
			for ( auto & e : this->elecLoadCenterObjs ) {
				e->setupLoadCenterMeterIndices();
			}
		}
		if (this->facilityPowerInTransformerPresent_ ) {
			facilityPowerInTransformerObj_->setupMeterIndices();
		}

	}

	void
	ElectricPowerServiceManager::reinitAtBeginEnvironment()
	{
		this->wholeBldgRemainingLoad_  = 0.0;
		this->electricityProd_         = 0.0;
		this->electProdRate_           = 0.0;
		this->electricityPurch_        = 0.0;
		this->electPurchRate_          = 0.0;
		this->electSurplusRate_        = 0.0;
		this->electricitySurplus_      = 0.0;
		this->electricityNetRate_      = 0.0;
		this->electricityNet_          = 0.0;
		this->totalBldgElecDemand_     = 0.0;
		this->totalHVACElecDemand_     = 0.0;
		this->totalElectricDemand_     = 0.0;
		this->elecProducedPVRate_      = 0.0;
		this->elecProducedWTRate_      = 0.0;
		this->elecProducedStorageRate_ = 0.0;
		this->elecProducedCoGenRate_   = 0.0;

		if ( this->numLoadCenters_ > 0 ){
			for ( auto & e : this->elecLoadCenterObjs ) {
				e->reinitAtBeginEnvironment();
			}
		}
		if ( this->facilityPowerInTransformerPresent_ ) {
			facilityPowerInTransformerObj_->reinitAtBeginEnvironment();
		}
		if ( this->powerOutTransformerObj_ != nullptr ) {
			this->powerOutTransformerObj_->reinitAtBeginEnvironment();
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

		// main panel clearing house.

		this->totalBldgElecDemand_ = GetInstantMeterValue( this->elecFacilityIndex_, 1 ) / DataGlobals::TimeStepZoneSec;
		this->totalHVACElecDemand_ = GetInstantMeterValue( this->elecFacilityIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->totalElectricDemand_ = this->totalBldgElecDemand_ + this->totalHVACElecDemand_;
		this->elecProducedPVRate_ = GetInstantMeterValue( this->elecProducedPVIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedWTRate_ = GetInstantMeterValue( this->elecProducedWTIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedStorageRate_ = GetInstantMeterValue( this->elecProducedStorageIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedCoGenRate_ = GetInstantMeterValue( this->elecProducedCoGenIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->elecProducedPowerConversionRate_ = GetInstantMeterValue( this->elecProducedPowerConversionIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

		this->electProdRate_ = this->elecProducedCoGenRate_ +  this->elecProducedPVRate_ + this->elecProducedWTRate_ + this->elecProducedStorageRate_ + this->elecProducedPowerConversionRate_;

		this->electricityProd_ = this->electProdRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; //whole building


		//Report the Total Electric Power Purchased [W], If negative then there is extra power to be sold or stored.
		this->electPurchRate_ = this->totalElectricDemand_ - this->electProdRate_;
		//Check this value against a tolerance to aid in reporting.
		if ( std::abs( this->electPurchRate_ ) < 0.0001 ) this->electPurchRate_ = 0.0;
		if ( this->electPurchRate_ < 0.0 ) this->electPurchRate_ = 0.0; // don't want negative purchased...

		//Report the Total Electric Energy Purchased [J]
		this->electricityPurch_ = this->electPurchRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//report the total electric surplus....
		this->electSurplusRate_ = this->electProdRate_ - this->totalElectricDemand_;
		if ( std::abs( this->electSurplusRate_ ) < 0.0001 ) this->electSurplusRate_ = 0.0;
		if ( this->electSurplusRate_ < 0.0 ) this->electSurplusRate_ = 0.0; // don't want negative surplus

		this->electricitySurplus_ = this->electSurplusRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//report the net electricity , + is purchased, - is surplus
		this->electricityNetRate_ = this->totalElectricDemand_ - this->electProdRate_;

		this->electricityNet_ = this->electricityNetRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
	}

	void
	ElectricPowerServiceManager::reportPVandWindCapacity()
	{
			// LEED report
		this->pvTotalCapacity_ = 0.0;
		this->windTotalCapacity_ = 0.0;
		for ( auto & lc : this->elecLoadCenterObjs ) {
			if ( lc->numGenerators > 0 ) {
				for ( auto & g : lc->elecGenCntrlObj ) {
					if ( g->compGenTypeOf_Num == DataGlobalConstants::iGeneratorPV ) {
						pvTotalCapacity_ += g->maxPowerOut;
					}
					if ( g->compGenTypeOf_Num  == DataGlobalConstants::iGeneratorWindTurbine ) {
						windTotalCapacity_ += g->maxPowerOut;
					}
				}
			}
		}
		//put in total capacity for PV and Wind for LEED report
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchLeedRenRatCap, "Photovoltaic", this->pvTotalCapacity_ / 1000, 2 );
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchLeedRenRatCap, "Wind", this->windTotalCapacity_ / 1000, 2 );

		//future work: this legacy approach is relying on the correct power output to have been placed in the Generator list.  There could be a difference between this control input and the actual size of the systems as defined in the generator objects.

	}

	void
	ElectricPowerServiceManager::sumUpNumberOfStorageDevices(){
		this->numElecStorageDevices = 0;
		for ( auto & e : this->elecLoadCenterObjs ) {
			if ( e->storageObj != nullptr ) {
				++this->numElecStorageDevices;
			}
		}
	}

	void
	ElectricPowerServiceManager::checkLoadCenters() {
	
		//issue #5302, detect if storage used on more than one load center. This is really a kind of GlobalNames issue, 

		//first fill in a vector of names
		std::vector < std::string > storageNames;
		std::vector < std::string > genListNames;
		for ( auto & e : elecLoadCenterObjs ) {
			if ( e->storageObj != nullptr ) {
				storageNames.emplace_back( e->storageObj->name );
			}
			if ( ! e->elecGenCntrlObj.empty()  ) {
				genListNames.emplace_back( e->generatorListName() );
			}
		}

		//then check the vector for duplicates. 
		for ( std::size_t i = 0; i < storageNames.size(); ++i ) {
			for ( std::size_t j = 0; j < storageNames.size(); ++j ) {
				if ( storageNames[ i ] == storageNames[ j ] && i != j ) {
					ShowWarningError( "ElectricPowerServiceManager::checkLoadCenters, the electrical storage device named = " + storageNames[ i ] + " is used on more than one ElectricLoadCenter:Distribution input object." );
					ShowContinueError( "Electric Load Centers cannot share the same storage device." );
				}
			}
		}

		for ( std::size_t i = 0; i < genListNames.size(); ++i ) {
			for ( std::size_t j = 0; j < genListNames.size(); ++j ) {
				if ( genListNames[ i ] == genListNames[ j ] && i != j ) {
					ShowWarningError( "ElectricPowerServiceManager::checkLoadCenters, the generator list named = " + genListNames[ i ] + " is used on more than one ElectricLoadCenter:Distribution input object." );
					ShowContinueError( "Electric Load Centers cannot share the same generator list (ElectricLoadCenter:Generators)." );
				}
			}
		}
	}

	ElectPowerLoadCenter::ElectPowerLoadCenter( // constructor
		int const objectNum
	):
		numGenerators( 0 ),
		bussType( ElectricBussType::notYetSet ),
		thermalProd( 0.0 ),
		thermalProdRate( 0.0 ),
		inverterPresent( false ),
		subpanelFeedInRequest( 0.0 ),
		subpanelFeedInRate( 0.0 ),
		subpanelDrawRate( 0.0 ),
		genElectricProd( 0.0 ),
		genElectProdRate( 0.0 ),
		storOpCVDrawRate( 0.0 ),
		storOpCVFeedInRate( 0.0 ),
		storOpCVChargeRate( 0.0 ),
		storOpCVDischargeRate( 0.0 ),
		storOpIsCharging( false ),
		storOpIsDischarging( false ),
		genOperationScheme_( GeneratorOpScheme::notYetSet ),
		demandMeterPtr_( 0 ),
		generatorsPresent_( false ),
		myCoGenSetupFlag_( true ),
		demandLimit_( 0.0 ),
		trackSchedPtr_( 0 ),
		dCElectricityProd_( 0.0 ),
		dCElectProdRate_( 0.0 ),
		dCpowerConditionLosses_( 0.0 ),
		storagePresent_( false ),
		transformerPresent_( false ),
		totalPowerRequest_( 0.0 ),
		totalThermalPowerRequest_( 0.0 ),
		storageScheme_( StorageOpScheme::notYetSet ),
		trackStorageOpMeterIndex_( 0 ),
		converterPresent_( false ),
		maxStorageSOCFraction_( 1.0 ),
		minStorageSOCFraction_( 0.0 ),
		designStorageChargePower_( 0.0 ),
		designStorageChargePowerWasSet_( false ),
		designStorageDischargePower_( 0.0 ),
		designStorageDischargePowerWasSet_( false ),
		storageChargeModSchedIndex_( 0 ),
		storageDischargeModSchedIndex_( 0 ),
		facilityDemandTarget_( 0.0 ),
		facilityDemandTargetModSchedIndex_( 0 ),
		eMSOverridePelFromStorage_( false ), // if true, EMS calling for override
		eMSValuePelFromStorage_( 0.0 ), // value EMS is directing to use, power from storage [W]
		eMSOverridePelIntoStorage_( false ), // if true, EMS calling for override
		eMSValuePelIntoStorage_( 0.0 ) // value EMS is directing to use, power into storage [W]
	{

		std::string const routineName = "ElectPowerLoadCenter constructor ";
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool errorsFound;

		DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Distribution";
		errorsFound = false;
		if ( objectNum > 0 ) {
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, objectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name_          = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?

			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 2 ) )  {
				this->generatorListName_ = DataIPShortCuts::cAlphaArgs( 2 );
				// check that 

				int testIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Generators", this->generatorListName_ );
				if ( testIndex == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 3 ) )  {
				//Load the Generator Control Operation Scheme
				if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "Baseload" ) ) {
					this->genOperationScheme_ = GeneratorOpScheme::baseLoad;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "DemandLimit" ) ) {
					this->genOperationScheme_ = GeneratorOpScheme::demandLimit;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "TrackElectrical" ) ) {
					this->genOperationScheme_ = GeneratorOpScheme::trackElectrical;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "TrackSchedule" ) ) {
					this->genOperationScheme_ = GeneratorOpScheme::trackSchedule;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "TrackMeter" ) ) {
					this->genOperationScheme_ =  GeneratorOpScheme::trackMeter;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "FollowThermal" ) ) {
					this->genOperationScheme_ = GeneratorOpScheme::thermalFollow;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "FollowThermalLimitElectrical" ) ) {
					this->genOperationScheme_ =  GeneratorOpScheme::thermalFollowLimitElectrical;
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					errorsFound = true;
				}
			}

			this->demandLimit_ = DataIPShortCuts::rNumericArgs( 1 );

			this->trackSchedPtr_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 4 ) );
			if ( ( this->trackSchedPtr_ == 0 ) && ( this->genOperationScheme_ == GeneratorOpScheme::trackSchedule ) ) {
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

			this->demandMeterName_ = InputProcessor::MakeUPPERCase( DataIPShortCuts::cAlphaArgs( 5 ) );
				// meters may not be "loaded" yet, defered check to later subroutine

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "AlternatingCurrent" ) ) {
				this->bussType = ElectricBussType::aCBuss;
				DataIPShortCuts::cAlphaArgs( 6 ) = "AlternatingCurrent";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "DirectCurrentWithInverter" ) ) {
				this->bussType  = ElectricBussType::dCBussInverter;
				this->inverterPresent = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "DirectCurrentWithInverter";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "AlternatingCurrentWithStorage" ) ) {
				this->bussType  = ElectricBussType::aCBussStorage;
				this->storagePresent_ = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "AlternatingCurrentWithStorage";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "DirectCurrentWithInverterDCStorage" ) ) {
				this->bussType  = ElectricBussType::dCBussInverterDCStorage;
				this->inverterPresent = true;
				this->storagePresent_ = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "DirectCurrentWithInverterDCStorage";
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "DirectCurrentWithInverterACStorage" ) ) {
				this->bussType  = ElectricBussType::dCBussInverterACStorage;
				this->inverterPresent = true;
				this->storagePresent_ = true;
				DataIPShortCuts::cAlphaArgs( 6 ) = "DirectCurrentWithInverterACStorage";
			} else if ( DataIPShortCuts::cAlphaArgs( 6 ).empty() ) {
				this->bussType  = ElectricBussType::aCBuss;
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

			if ( this->storagePresent_ ) {
				if ( ! DataIPShortCuts::lAlphaFieldBlanks( 8 ) ) {
					this->storageName_ = DataIPShortCuts::cAlphaArgs( 8 );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( DataIPShortCuts::cAlphaFieldNames( 8 ) + " is blank, but buss type requires storage.");
					errorsFound = true;
				}
			}

			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 9 ) ) {
				// process transformer
				this->transformerName_ = DataIPShortCuts::cAlphaArgs( 9 );
				// only transformers of use type powerFromLoadCenterToBldg are really held in a load center, The legacy applications for transformers are held at the higher Electric service level
				this->transformerPresent_ =  true;
			}

			// Begin new content for grid supply and more control over storage
			// user selected storage operation scheme
			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 10 ) ) {
				if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 10 ), "TrackFacilityElectricDemandStoreExcessOnSite" ) ) {
					this->storageScheme_ = StorageOpScheme::facilityDemandStoreExcessOnSite;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 10 ), "TrackMeterDemandStoreExcessOnSite" )  ) {
					this->storageScheme_ = StorageOpScheme::meterDemandStoreExcessOnSite;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 10 ), "TrackChargeDischargeSchedules" )  ) {
					this->storageScheme_ = StorageOpScheme::chargeDischargeSchedules;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 10 ), "FacilityDemandLeveling" )  ) {
					this->storageScheme_ = StorageOpScheme::facilityDemandLeveling;
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 10 ) + " = " + DataIPShortCuts::cAlphaArgs( 10 ) );
					errorsFound = true;
				}
			} else { // blank (preserve legacy behavior for short files)
				this->storageScheme_ = StorageOpScheme::facilityDemandStoreExcessOnSite;
			}

			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 11 ) ) {
				this->demandMeterName_ = DataIPShortCuts::cAlphaArgs( 11 );

			} else {
				if ( this->storageScheme_ == StorageOpScheme::meterDemandStoreExcessOnSite ) { // throw error
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 11 ) + ", cannot be blank when storage operation scheme is TrackMeterDemandStoreExcessOnSite" );
					errorsFound = true;
				}
			}

			if ( ! DataIPShortCuts::lAlphaFieldBlanks( 12 ) ) {
				this->converterName_ = DataIPShortCuts::cAlphaArgs( 12 );
				this->converterPresent_ = true;
			} else {
				if ( this->storageScheme_ == StorageOpScheme::chargeDischargeSchedules || this->storageScheme_ == StorageOpScheme::facilityDemandLeveling ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 12 ) + ", cannot be blank when storage scheme is " + DataIPShortCuts::cAlphaArgs( 10 ) );
					errorsFound = true;
				}
			}

			if ( DataIPShortCuts::lNumericFieldBlanks( 2 ) ) {
				this->maxStorageSOCFraction_  = 1.0;
			} else {
				this->maxStorageSOCFraction_        = DataIPShortCuts::rNumericArgs( 2 );
			}
			if ( DataIPShortCuts::lNumericFieldBlanks( 3 ) ) {
				this->minStorageSOCFraction_        = 0.0;
			} else {
				this->minStorageSOCFraction_        = DataIPShortCuts::rNumericArgs( 3 );
			}
			if ( DataIPShortCuts::lNumericFieldBlanks( 4 ) ) {
				this->designStorageChargePowerWasSet_ = false;
			} else {
				this->designStorageChargePowerWasSet_ = true;
				this->designStorageChargePower_     = DataIPShortCuts::rNumericArgs( 4 );
			}
			if ( DataIPShortCuts::lNumericFieldBlanks( 5 ) ) {
				this->designStorageDischargePowerWasSet_ = false;
			} else {
				this->designStorageDischargePowerWasSet_ = true;
				this->designStorageDischargePower_ = DataIPShortCuts::rNumericArgs( 5 );
			}

			if ( DataIPShortCuts::lNumericFieldBlanks( 6 ) ) {
				if ( this->storageScheme_ == StorageOpScheme::facilityDemandLeveling ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cNumericFieldNames( 6 ) + " = blank field." );
					errorsFound = true;
				}
			} else {
				this->facilityDemandTarget_     = DataIPShortCuts::rNumericArgs( 6 );
			}
			this->storageChargeModSchedIndex_   = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 13 ) );
			if ( this->storageChargeModSchedIndex_ == 0 && this->storageScheme_ == StorageOpScheme::chargeDischargeSchedules ) {
				if ( ! DataIPShortCuts::lAlphaFieldBlanks( 13 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 13 ) + " = " + DataIPShortCuts::cAlphaArgs( 13 ) );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 13 ) + " = blank field." );
				}
				ShowContinueError( "Schedule not found; Must be entered and valid when Storage Operation Scheme = TrackChargeDischargeSchedules" );
				errorsFound = true;
			}

			this->storageDischargeModSchedIndex_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 14 ) );
			if ( this->storageDischargeModSchedIndex_ == 0 && this->storageScheme_ == StorageOpScheme::chargeDischargeSchedules ) {
				if ( ! DataIPShortCuts::lAlphaFieldBlanks( 14 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 14 ) + " = " + DataIPShortCuts::cAlphaArgs( 14 ) );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 14 ) + " = blank field." );
				}
				ShowContinueError( "Schedule not found; Must be entered and valid when Storage Operation Scheme = TrackChargeDischargeSchedules" );
				errorsFound = true;
			}

			this->facilityDemandTargetModSchedIndex_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 15 ) );
			if ( this->facilityDemandTargetModSchedIndex_ == 0 && this->storageScheme_ == StorageOpScheme::facilityDemandLeveling ) {
				if ( ! DataIPShortCuts::lAlphaFieldBlanks( 15 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 15 ) + " = " + DataIPShortCuts::cAlphaArgs( 15 ) );
				} else {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 15 ) + " = blank field." );
				}
				ShowContinueError( "Schedule not found; Must be entered and valid when Storage Operation Scheme = FacilityDemandLeveling" );
				errorsFound = true;
			}
		} else { // object num == 0
			// just construct an empty object and return
			return;
		}

		// now that we are done with processing get input for ElectricLoadCenter:Distribution we can call child input objects without IP shortcut problems
		DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Generators";
		int genListObjectNum = InputProcessor::GetObjectItemNum( DataIPShortCuts::cCurrentModuleObject, this->generatorListName_ );
		if ( genListObjectNum > 0 ){
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, genListObjectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			//Calculate the number of generators in list
			this->numGenerators = numNums / 2; // note IDD needs Min Fields = 6  
			if ( mod( ( numAlphas - 1 + numNums ), 5 ) != 0 ) ++this->numGenerators;
			int alphaCount = 2;
			for ( auto genCount = 1; genCount <= this->numGenerators; ++genCount) {
				// call constructor in place
				this->generatorsPresent_ = true;
				this->elecGenCntrlObj.emplace_back( new GeneratorController ( DataIPShortCuts::cAlphaArgs( alphaCount ), DataIPShortCuts::cAlphaArgs( alphaCount + 1 ), DataIPShortCuts::rNumericArgs( 2 * genCount - 1 ), DataIPShortCuts::cAlphaArgs( alphaCount + 2 ), DataIPShortCuts::rNumericArgs( 2 * genCount) ) );
				++alphaCount;
				++alphaCount;
				++alphaCount;
			}

			//issue #5299 check for non-zero values in thermal electric ratio if gen op scheme is thermalFollow*
			if ( this->genOperationScheme_ == GeneratorOpScheme::thermalFollow || this->genOperationScheme_ == GeneratorOpScheme::thermalFollowLimitElectrical ) {
				// check to make sure the user didn't input zeros for thermalToElectricControlRatio
				for ( auto & g : elecGenCntrlObj ) {
					if ( g->nominalThermElectRatio <= 0.0 ) {
						ShowErrorMessage( "Generator operation need to be based on following thermal loads and needs values for Rated Thermal to Electrical Power Ratio in " + DataIPShortCuts::cCurrentModuleObject + " named " + DataIPShortCuts::cAlphaArgs( 1 ) );
					}
				}
			}
		}

		if ( ! errorsFound && this->inverterPresent ) {
			// call inverter constructor
			this->inverterObj = std::unique_ptr < DCtoACInverter >  ( new DCtoACInverter( this->inverterName ) ) ;
		}

		if ( ! errorsFound && this->storagePresent_ ) {
			// call storage constructor 
			this->storageObj =  std::unique_ptr < ElectricStorage > ( new ElectricStorage( this->storageName_ ) );
		}

		if ( ! errorsFound && this->transformerPresent_ ) {

			DataIPShortCuts::cCurrentModuleObject =  "ElectricLoadCenter:Transformer";
			int transformerItemNum = InputProcessor::GetObjectItemNum( DataIPShortCuts::cCurrentModuleObject, this->transformerName_ );
			int iOStat;
			if ( transformerItemNum > 0 ) {
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, transformerItemNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, iOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );
				if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 3 ), "LoadCenterPowerConditioning" ) ) { // this is the right kind of transformer
					this->transformerObj_ = std::unique_ptr < ElectricTransformer >( new ElectricTransformer (this->transformerName_ ) );
				} else {
					ShowWarningError( "Transformer named " + this->transformerName_ + " associated with the load center named " + this->name_ + " should have " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " set to LoadCenterPowerConditioning." );
				}
			} else {
				ShowSevereError( "Transformer named " + this->transformerName_ + ", was not found for the load center named " + this->name_ );
				errorsFound =  true;
			}

		}

		if ( ! errorsFound && this->converterPresent_ ) {
			// call AC to DC converter constructor
			this->converterObj_ = std::unique_ptr< ACtoDCConverter >( new ACtoDCConverter( this->converterName_ ) );
		}

		//Setup general output variables for reporting in the electric load center

		SetupOutputVariable( "Electric Load Center Produced Electric Power [W]",this->genElectProdRate, "System", "Average", this->name_ );
		SetupOutputVariable( "Electric Load Center Produced Electric Energy [J]",this->genElectricProd, "System", "Sum", this->name_ );
		SetupOutputVariable( "Electric Load Center Supplied Electric Power [W]", this->subpanelFeedInRate, "System", "Average", this->name_ );
		SetupOutputVariable( "Electric Load Center Drawn Electric Power [W]", this->subpanelDrawRate, "System", "Average", this->name_ );
		SetupOutputVariable( "Electric Load Center Produced Thermal Rate [W]", this->thermalProdRate, "System", "Average", this->name_ );
		SetupOutputVariable( "Electric Load Center Produced Thermal Energy [J]", this->thermalProd, "System", "Sum", this->name_ );
		SetupOutputVariable( "Electric Load Center Requested Electric Power [W]", this->totalPowerRequest_, "System", "Average", this->name_ );

		if ( DataGlobals::AnyEnergyManagementSystemInModel && this->storagePresent_ ) {
				SetupEMSActuator( "Electrical Storage", this->name_ , "Power Draw Rate", "[W]", this->eMSOverridePelFromStorage_, this->eMSValuePelFromStorage_ );
				SetupEMSActuator( "Electrical Storage", this->name_ , "Power Charge Rate", "[W]", this->eMSOverridePelIntoStorage_, this->eMSValuePelIntoStorage_ );
		}

		if ( errorsFound ) {
			ShowFatalError( routineName + "Preceding errors terminate program." );
		}
	}

	void
	ElectPowerLoadCenter::manageElecLoadCenter(
		bool const firstHVACIteration,
		Real64 & remainingWholePowerDemand
	)
	{
		//
		this->subpanelFeedInRequest = remainingWholePowerDemand;

		if( this->generatorsPresent_ ) {
			this->dispatchGenerators(
				firstHVACIteration,
				remainingWholePowerDemand
			);

		} // if generators present
		this->updateLoadCenterGeneratorRecords();
		if ( this->bussType == ElectricBussType::dCBussInverter || this->bussType == ElectricBussType::dCBussInverterACStorage) {
			this->inverterObj->simulate( this->genElectProdRate ); 
		}

		if ( this->storagePresent_ ) {
			this->storageObj->timeCheckAndUpdate();
			this->dispatchStorage( this->subpanelFeedInRequest );
		}

		if ( this->bussType == ElectricBussType::dCBussInverterDCStorage ) {
			if ( this->inverterObj != nullptr ) {
				this->inverterObj->simulate( this->storOpCVFeedInRate );
			}
		}

		if ( this->converterObj_ != nullptr ) {
			this->converterObj_->simulate( this->storOpCVDrawRate );
		}

		if ( this->transformerObj_ != nullptr ) {
			if ( this->storOpCVFeedInRate > 0.0 ) {
				this->transformerObj_->manageTransformers( this->storOpCVFeedInRate );
			} else if ( this->storOpCVDrawRate > 0.0 ) {
				this->transformerObj_->manageTransformers( this->subpanelDrawRate );
			}
		}
		this->updateLoadCenterGeneratorRecords();
	}

	void
	ElectPowerLoadCenter::dispatchGenerators(
		bool const firstHVACIteration,
		Real64 & remainingWholePowerDemand // power request in, remaining unmet request out
	)
	{

		// This funciton checks generator operation scheme and assigns requests to power generators
		// the generators are called to simulate from here and passed some control data
		// the actual production from each generator is recorded and accounting tracks how much of the load is met

		// If a generator is needed in the simulation for a small load and it is less than the minimum part load ratio
		// the generator will operate at the minimum part load ratio and the excess will either reduce demand or
		// be available for storage or sell back to the power company.

		// Both the Demand Limit and Track Electrical schemes will sequentially load the available generators.  All demand
		Real64 loadCenterElectricLoad = 0.0;
		Real64 remainingLoad          = 0.0;

//		Real64 thermalProdRate        = 0.0;
		Real64 customMeterDemand      = 0.0;

		switch ( this->genOperationScheme_ ) 
		{

		case GeneratorOpScheme::baseLoad: {

			loadCenterElectricLoad = remainingWholePowerDemand;

			for ( auto & g : this->elecGenCntrlObj ) {

				if ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 ) {
					// Set the Operation Flag
					g->onThisTimestep = true;
					// Set the electric generator load request
					g->powerRequestThisTimestep = g->maxPowerOut;
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;
				}

				// now handle EMS override
				if ( g->eMSRequestOn ) {
					g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
					if ( g->powerRequestThisTimestep > 0.0 ) {
						g->onThisTimestep = true;
					} else {
						g->onThisTimestep = false;
					}
				}

				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				this->totalPowerRequest_ += g->powerRequestThisTimestep;
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
			}
			break;
		}
		case GeneratorOpScheme::demandLimit: {
			// The Demand Limit scheme tries to have the generators meet all of the demand above the purchased Electric
			//  limit set by the user.
			remainingLoad = remainingWholePowerDemand - this->demandLimit_;
			loadCenterElectricLoad = remainingLoad;

			for ( auto & g : this->elecGenCntrlObj ) {

				if ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					g->onThisTimestep = true;

					// Set the electric generator load
					g->powerRequestThisTimestep = min( g->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				if ( g->eMSRequestOn ) {
					this->totalPowerRequest_ += max( g->eMSPowerRequest, 0.0 );
				} else {
					if ( g->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest_ += g->maxPowerOut;
						this->totalPowerRequest_ = min( loadCenterElectricLoad, this->totalPowerRequest_ );
					}
				}
				remainingLoad -= g->electProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
			}
			break;
		}
		case GeneratorOpScheme::trackElectrical: {
				//The Track Electrical scheme tries to have the generators meet all of the electrical demand for the building.
			remainingLoad = remainingWholePowerDemand;
			loadCenterElectricLoad = remainingLoad;

			for ( auto & g : this->elecGenCntrlObj ) {

				if ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					g->onThisTimestep = true;

					// Set the electric generator load
					g->powerRequestThisTimestep = min( g->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;
					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				if ( g->eMSRequestOn ) {
					this->totalPowerRequest_ += max( g->eMSPowerRequest, 0.0 );
				} else {
					if ( g->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest_ += g->maxPowerOut;
						this->totalPowerRequest_ = min( loadCenterElectricLoad, this->totalPowerRequest_ );
					}
				}
				remainingLoad -= g->electProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
			}
			break;
		}
		case GeneratorOpScheme::trackSchedule: {
				// The Track Schedule scheme tries to have the generators meet the electrical demand determined from a schedule.
				//  Code is very similar to 'Track Electrical' except for initial RemainingLoad is replaced by SchedElecDemand
				//  and PV production is ignored.
			remainingLoad = ScheduleManager::GetCurrentScheduleValue( this->trackSchedPtr_ );
			loadCenterElectricLoad = remainingLoad;

			for ( auto & g : this->elecGenCntrlObj ) {

				if ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					g->onThisTimestep = true;

					// Set the electric generator load
					g->powerRequestThisTimestep = min( g->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				if ( g->eMSRequestOn ) {
					this->totalPowerRequest_ += max( g->eMSPowerRequest, 0.0 );
				} else {
					if ( g->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest_ += g->maxPowerOut;
						this->totalPowerRequest_ = min( loadCenterElectricLoad, this->totalPowerRequest_ );
					}
				}
				remainingLoad -= g->electProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
			}
			break;
		}
		case GeneratorOpScheme::trackMeter: {
				// The TRACK CUSTOM METER scheme tries to have the generators meet all of the
				//   electrical demand from a meter, it can also be a user-defined Custom Meter
				//   and PV is ignored.
			customMeterDemand = GetInstantMeterValue( this->demandMeterPtr_, 1 ) / DataGlobals::TimeStepZoneSec + GetInstantMeterValue( this->demandMeterPtr_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

			remainingLoad = customMeterDemand;
			loadCenterElectricLoad = remainingLoad;

			for ( auto & g : this->elecGenCntrlObj ) {
				if ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 && remainingLoad > 0.0 ) {
					// Set the Operation Flag
					g->onThisTimestep = true;
					// Set the electric generator load
					g->powerRequestThisTimestep = min( g->maxPowerOut, remainingLoad );

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;

					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				if ( g->eMSRequestOn ) {
					this->totalPowerRequest_ += max( g->eMSPowerRequest, 0.0 );
				} else {
					if ( g->powerRequestThisTimestep > 0.0 ) {
						this->totalPowerRequest_ += g->maxPowerOut;
						this->totalPowerRequest_ = min( loadCenterElectricLoad, this->totalPowerRequest_ );
					}
				}
				remainingLoad -= g->electProdRate; // Update remaining load to be met by this load center
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
			} // end for
			break;
		}
		case GeneratorOpScheme::thermalFollow: {
				// Turn thermal load into an electrical load for cogenerators controlled to follow heat loads
			Real64 remainingThermalLoad = 0.0;

			this->calcLoadCenterThermalLoad( remainingThermalLoad );
			Real64 loadCenterThermalLoad = remainingThermalLoad;
			for ( auto & g : this->elecGenCntrlObj ) {

				if ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 && remainingThermalLoad > 0.0 ) {

					if ( g->nominalThermElectRatio > 0.0 ) {
						remainingLoad = remainingThermalLoad / g->nominalThermElectRatio;
						g->powerRequestThisTimestep = min( g->maxPowerOut, remainingLoad );
						g->onThisTimestep = true;
						// now handle EMS override
						if ( g->eMSRequestOn ) {
							g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
							if ( g->powerRequestThisTimestep > 0.0 ) {
								g->onThisTimestep = true;
							} else {
								g->onThisTimestep = false;
							}
						}
					}
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;
					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				}

				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				if ( g->eMSRequestOn ) {
					this->totalThermalPowerRequest_ += ( max( g->eMSPowerRequest, 0.0 ) ) * g->nominalThermElectRatio;
					this->totalPowerRequest_ += ( max( g->eMSPowerRequest, 0.0 ) );
				} else {
					if ( this->totalThermalPowerRequest_ < loadCenterThermalLoad && g->powerRequestThisTimestep > 0.0 ) {
						Real64 excessThermalPowerRequest = this->totalThermalPowerRequest_ + g->maxPowerOut * g->nominalThermElectRatio - loadCenterThermalLoad;
						if ( excessThermalPowerRequest < 0.0 ) {
							this->totalThermalPowerRequest_ += g->maxPowerOut * g->nominalThermElectRatio;
							this->totalPowerRequest_ += g->maxPowerOut;
						} else {
							this->totalThermalPowerRequest_ = loadCenterThermalLoad;
							if ( g->nominalThermElectRatio > 0.0 ) {
								this->totalPowerRequest_ += g->maxPowerOut - ( excessThermalPowerRequest / g->nominalThermElectRatio );
							}
						}
					}
				}
				remainingThermalLoad -= g->thermProdRate; // Update remaining load to be met
				// by this load center
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
			}
			break;
		}
		case GeneratorOpScheme::thermalFollowLimitElectrical: {
			//  Turn a thermal load into an electrical load for cogenerators controlled to follow heat loads.
			//  Add intitialization of RemainingThermalLoad as in the ThermalFollow operating scheme above.
			Real64 remainingThermalLoad = 0.0;
			this->calcLoadCenterThermalLoad( remainingThermalLoad );
			// Total current electrical demand for the building is a secondary limit.
			remainingLoad = remainingWholePowerDemand;
			loadCenterElectricLoad = remainingWholePowerDemand;
			Real64 loadCenterThermalLoad = remainingThermalLoad;
			for ( auto & g : this->elecGenCntrlObj ) {
				if ( ( ScheduleManager::GetCurrentScheduleValue( g->availSchedPtr ) > 0.0 ) && ( remainingThermalLoad > 0.0 ) && ( remainingLoad > 0.0 ) ) {
					if ( g->nominalThermElectRatio > 0.0 ) {
						remainingLoad = min( remainingWholePowerDemand, remainingThermalLoad / g->nominalThermElectRatio );
						g->powerRequestThisTimestep = min( g->maxPowerOut, remainingLoad );
						g->onThisTimestep = true;
						// now handle EMS override
						if ( g->eMSRequestOn ) {
							g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
							if ( g->powerRequestThisTimestep > 0.0 ) {
								g->onThisTimestep = true;
							} else {
								g->onThisTimestep = false;
							}
						}
					}
				} else {
					g->onThisTimestep = false;
					g->powerRequestThisTimestep = 0.0;
					// now handle EMS override
					if ( g->eMSRequestOn ) {
						g->powerRequestThisTimestep = max( g->eMSPowerRequest, 0.0 );
						if ( g->powerRequestThisTimestep > 0.0 ) {
							g->onThisTimestep = true;
						} else {
							g->onThisTimestep = false;
						}
					}
				}
				// Get generator's actual electrical and thermal power outputs
				g->simGeneratorGetPowerOutput ( g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate );

				if ( g->eMSRequestOn ) {
					this->totalThermalPowerRequest_ += ( max( g->eMSPowerRequest, 0.0 ) ) * g->nominalThermElectRatio;
					this->totalPowerRequest_ += ( max( g->eMSPowerRequest, 0.0 ) );
				} else {
					if ( this->totalThermalPowerRequest_ < loadCenterThermalLoad && g->powerRequestThisTimestep > 0.0 ) {
						Real64 excessThermalPowerRequest = this->totalThermalPowerRequest_ + g->maxPowerOut * g->nominalThermElectRatio - loadCenterThermalLoad;
						if ( excessThermalPowerRequest < 0.0 ) {
							this->totalThermalPowerRequest_ += g->maxPowerOut * g->nominalThermElectRatio;
							this->totalPowerRequest_ += g->maxPowerOut;
						} else {
							this->totalThermalPowerRequest_ = loadCenterThermalLoad;
							if ( g->nominalThermElectRatio > 0.0 ) {
								this->totalPowerRequest_ += g->maxPowerOut - ( excessThermalPowerRequest / g->nominalThermElectRatio );
							}
						}
						this->totalPowerRequest_ = min( loadCenterElectricLoad, this->totalPowerRequest_ );
					}
				}
				remainingThermalLoad -= g->thermProdRate; // Update remaining thermal load to
				// be met by this load center
				remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining
				// electric load
			}
			break;
		}
		case GeneratorOpScheme::notYetSet: {
			// do nothing
		}
		} // end switch

		// sum up generator production
		this->genElectProdRate = 0.0;
		this->genElectricProd = 0.0;
		for ( auto & g : this->elecGenCntrlObj ) {
			this->genElectProdRate += g->electProdRate; 
			g->electricityProd = g->electProdRate * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
			this->genElectricProd += g->electricityProd;
		}
	}

	void
	ElectPowerLoadCenter::dispatchStorage( 
		Real64 const originalFeedInRequest // whole building remaining electric demand for this load center
	 )
	{

		//1. resolve generator power rate into storage operation control volume, by buss type
		switch ( this->bussType )
		{
			case ElectricBussType::notYetSet :
			case ElectricBussType::aCBuss : 
			case ElectricBussType::dCBussInverter : {
				// do nothing, no storage to manage
				break;
			}
			case ElectricBussType::aCBussStorage : {
				this->storOpCVGenRate = this->genElectProdRate;
				break;
			}
			case ElectricBussType::dCBussInverterDCStorage : {
				this->storOpCVGenRate = this->genElectProdRate;
				break;
			}
			case ElectricBussType::dCBussInverterACStorage : {
				//TODO call inverter model here?
				this->storOpCVGenRate = this->inverterObj->getACPowerOut();
				break;
			}
		} // end switch buss type

		//2.  determine subpanel feed in and draw requests based on storage operation control scheme
		Real64 subpanelFeedInRequest = 0.0;
		Real64 subpanelDrawRequest   = 0.0;
		switch ( this->storageScheme_ )
		{
			case StorageOpScheme::notYetSet : {
				// do nothing
				break;
			}
			case StorageOpScheme::facilityDemandStoreExcessOnSite : {
				subpanelFeedInRequest = originalFeedInRequest; //legacy behavior, storage dispatched to meet building load
				subpanelDrawRequest  = 0.0;
				break;
			}
			case StorageOpScheme::meterDemandStoreExcessOnSite : {
				// Get meter rate
				subpanelFeedInRequest = GetInstantMeterValue( this->trackStorageOpMeterIndex_, 1 ) / DataGlobals::TimeStepZoneSec + GetInstantMeterValue( this->trackStorageOpMeterIndex_, 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
				subpanelDrawRequest  = 0.0;
				break;
			}
			case StorageOpScheme::chargeDischargeSchedules : {
				// do not need to deal with subpanel rates here, charge or discharge is known from schedules and filled in below
				break;
			}
			case StorageOpScheme::facilityDemandLeveling : {
				Real64 demandTarget = this->facilityDemandTarget_ * ScheduleManager::GetCurrentScheduleValue( facilityDemandTargetModSchedIndex_ );
				//compare target to 
				Real64 deltaLoad = originalFeedInRequest - demandTarget;
				if ( deltaLoad >= 0.0 ) {
					//subpanel should feed main panel
					subpanelFeedInRequest = deltaLoad;
					subpanelDrawRequest   = 0.0;
				} else { 
					// subpanel should draw from main panel 
					subpanelFeedInRequest = 0.0;
					subpanelDrawRequest   = std::abs( deltaLoad );
				}
				break;
			}
		}

		// 3. adjust feed in and draw rates from subpanel to storage operation control volume
		Real64 adjustedFeedInRequest = 0.0; // account for any inverter or transformer losses
		Real64 adjustedDrawRequest   = 0.0; // account for any converer or transformer losses

		switch ( this->bussType )
		{
			case ElectricBussType::notYetSet :
			case ElectricBussType::aCBuss : 
			case ElectricBussType::dCBussInverter : {
				// do nothing, no storage to manage
				break;
			}
			case ElectricBussType::aCBussStorage :
			case ElectricBussType::dCBussInverterACStorage : {
				if ( this->transformerObj_ == nullptr ) {
					adjustedFeedInRequest = subpanelFeedInRequest;
					adjustedDrawRequest   = subpanelDrawRequest;
				} else {
					adjustedFeedInRequest = subpanelFeedInRequest + this->transformerObj_->getLossRateForOutputPower( subpanelFeedInRequest );
					adjustedDrawRequest   = subpanelDrawRequest - this->transformerObj_->getLossRateForInputPower( subpanelDrawRequest );
				}
				break;
			}
			case ElectricBussType::dCBussInverterDCStorage : {
				// can we get updated power conditioning losses here?
				if ( this->transformerObj_ == nullptr ) {
					adjustedFeedInRequest = subpanelFeedInRequest + this->inverterObj->getLossRateForOutputPower( subpanelFeedInRequest );
					if ( this->converterObj_ == nullptr ) { // some operation schemes will never need a converter
						adjustedDrawRequest   = subpanelDrawRequest;
					} else {
						adjustedDrawRequest   = subpanelDrawRequest - this->converterObj_->getLossRateForInputPower( subpanelDrawRequest );
					}
				} else {
					adjustedFeedInRequest = subpanelFeedInRequest + this->inverterObj->getLossRateForOutputPower( subpanelFeedInRequest ) + this->transformerObj_->getLossRateForOutputPower( subpanelFeedInRequest );
					if ( this->converterObj_ == nullptr ) {
						adjustedDrawRequest   = subpanelDrawRequest - this->transformerObj_->getLossRateForInputPower( subpanelDrawRequest );
					} else {
						adjustedDrawRequest   = subpanelDrawRequest - this->converterObj_->getLossRateForInputPower( subpanelDrawRequest ) - this->transformerObj_->getLossRateForInputPower( subpanelDrawRequest );
					}
				}
				break;
			}
		} // end switch buss type

		switch ( this->storageScheme_ )
		{
			case StorageOpScheme::notYetSet : {
				// do nothing
				break;
			}
			case StorageOpScheme::facilityDemandStoreExcessOnSite : // these are both the same because adjusted feed in request has already accounted for the difference
			case StorageOpScheme::meterDemandStoreExcessOnSite :{
				// this is the legacy behavior but with more limits from storage control operation information

				// no draws from main panel
				this->storOpCVDrawRate = 0.0;

				if ( this->storOpCVGenRate < ( adjustedFeedInRequest ) ) {
					//draw from storage
					this->storOpCVDischargeRate = adjustedFeedInRequest - this->storOpCVGenRate;
					this->storOpCVChargeRate    = 0.0;
					this->storOpIsDischarging   = true;
					this->storOpIsCharging      = false;

				} else if ( this->storOpCVGenRate > ( adjustedFeedInRequest ) ) {
					//add to storage
					this->storOpCVDischargeRate = 0.0;
					this->storOpCVChargeRate    = this->storOpCVGenRate - adjustedFeedInRequest;
					this->storOpIsCharging      = true;
					this->storOpIsDischarging   = false;

				} else if ( this->storOpCVGenRate == ( adjustedFeedInRequest ) ) {
					//do nothing
					this->storOpCVDischargeRate = 0.0;
					this->storOpCVChargeRate    = 0.0;
					this->storOpIsCharging      = false;
					this->storOpIsDischarging   = false;
				}
				break;
			}

			case StorageOpScheme::chargeDischargeSchedules : {
				this->storOpCVChargeRate    = this->designStorageChargePower_ * ScheduleManager::GetCurrentScheduleValue( this->storageChargeModSchedIndex_ );
				this->storOpCVDischargeRate = this->designStorageDischargePower_ * ScheduleManager::GetCurrentScheduleValue( this->storageDischargeModSchedIndex_ );
				Real64 genAndStorSum = this->storOpCVGenRate + this->storOpCVDischargeRate - this->storOpCVChargeRate;
				if ( genAndStorSum >= 0.0 ) { // power to feed toward main panel
					this->storOpCVDrawRate  = 0.0;
					this->storOpCVFeedInRate = genAndStorSum;
				} else { // shortfall, will need to draw from main panel (e.g. for grid charging)
					this->storOpCVFeedInRate = 0.0;
					this->storOpCVDrawRate   = std::abs( genAndStorSum );
				}
				if ( this->storOpCVChargeRate > 0.0 ) {
					this->storOpIsCharging = true;
				} else {
					this->storOpIsCharging = false;
				}
				if ( this->storOpCVDischargeRate > 0.0 ) {
					this->storOpIsDischarging = true;
				} else {
					this->storOpIsDischarging = false;
				}
				break;
			}
			case StorageOpScheme::facilityDemandLeveling : {

				if ( adjustedDrawRequest > 0.0 ) { // the only reason to draw instead of feed is to charge storage
					this->storOpCVFeedInRate    = 0.0;
					this->storOpCVDrawRate      = adjustedDrawRequest;
					this->storOpCVChargeRate    = this->storOpCVDrawRate + this->storOpCVGenRate;
					this->storOpCVDischargeRate = 0.0;
					this->storOpIsCharging      = true;
					this->storOpIsDischarging   = false;
				}
				if ( adjustedFeedInRequest > 0.0 ) {
					this->storOpCVDrawRate      = 0.0;
					this->storOpCVFeedInRate    = adjustedFeedInRequest;
					if ( this->storOpCVGenRate < ( adjustedFeedInRequest ) ) {
						//draw from storage
						this->storOpCVDischargeRate = adjustedFeedInRequest - this->storOpCVGenRate;
						this->storOpCVChargeRate    = 0.0;
						this->storOpIsDischarging   = true;
						this->storOpIsCharging      = false;

					} else if ( this->storOpCVGenRate > ( adjustedFeedInRequest ) ) {
						//add to storage
						this->storOpCVDischargeRate = 0.0;
						this->storOpCVChargeRate    = this->storOpCVGenRate - adjustedFeedInRequest; 
						this->storOpIsCharging      = true;
						this->storOpIsDischarging   = false;

					} else if ( this->storOpCVGenRate == ( adjustedFeedInRequest ) ) {
						//do nothing
						this->storOpCVDischargeRate = 0.0;
						this->storOpCVChargeRate    = 0.0;
						this->storOpIsCharging      = false;
						this->storOpIsDischarging   = false;
					}
				}
				break;
			}
		}

				//handle EMS overrides
		if ( this->eMSOverridePelFromStorage_ || this->eMSOverridePelIntoStorage_ ) {
			if ( ( this->eMSOverridePelFromStorage_ ) && ( ! this->eMSOverridePelIntoStorage_ ) ) {
				// EMS is calling for specific discharge rate
				this->storOpCVDischargeRate     = max( this->eMSValuePelFromStorage_, 0.0 );
				this->storOpCVChargeRate        = 0.0;
				this->storOpIsDischarging       = true;
				this->storOpIsCharging          = false;
			} else if ( ( ! this->eMSOverridePelFromStorage_ ) && ( this->eMSOverridePelIntoStorage_ ) ) {
				// EMS is calling for specific charge rate
				this->storOpCVChargeRate        = max( this->eMSValuePelIntoStorage_, 0.0 );
				this->storOpCVDischargeRate     = 0.0;
				this->storOpIsDischarging       = false;
				this->storOpIsCharging          = true;
			} else if ( ( this->eMSOverridePelFromStorage_ ) && ( this->eMSOverridePelIntoStorage_ ) ) {
				// EMS is asking to override both
				if ( this->eMSValuePelIntoStorage_ > this->eMSValuePelFromStorage_ ) {
					this->storOpCVChargeRate    = this->eMSValuePelIntoStorage_ - this->eMSValuePelFromStorage_;
					this->storOpCVDischargeRate = 0.0;
					this->storOpIsDischarging   = false;
					this->storOpIsCharging      = true;
				} else if ( this->eMSValuePelIntoStorage_ < this->eMSValuePelFromStorage_ ) {
					this->storOpCVDischargeRate = this->eMSValuePelFromStorage_ - this->eMSValuePelIntoStorage_;
					this->storOpCVChargeRate    = 0.0;
					this->storOpIsDischarging   = true;
					this->storOpIsCharging      = false;
				} else { //they equal just hold
					this->storOpCVDischargeRate = 0.0;
					this->storOpCVChargeRate    = 0.0;
					this->storOpIsDischarging   = false;
					this->storOpIsCharging      = false;
				}
			}

		}

		//check against the controller limits
		if ( this->designStorageChargePowerWasSet_ ) {
			this->storOpCVChargeRate    = min( this->storOpCVChargeRate,    this->designStorageChargePower_ );
		}
		if ( this->designStorageDischargePowerWasSet_ ) {
			this->storOpCVDischargeRate = min( this->storOpCVDischargeRate, this->designStorageDischargePower_ );
		}

		//dispatch final request to storage device, calculate, update, and report storage device, passing what controller wants for SOC limits

		this->storageObj->simulate( this->storOpCVChargeRate, this->storOpCVDischargeRate,this->storOpIsCharging,this->storOpIsDischarging, this->maxStorageSOCFraction_, this->minStorageSOCFraction_ );

		// rebalance with final charge and discharge rates
		Real64 genAndStorSum = this->storOpCVGenRate + this->storOpCVDischargeRate - this->storOpCVChargeRate;
		if ( genAndStorSum >= 0.0 ) { // power to feed toward main panel
			this->storOpCVDrawRate  = 0.0;
			this->storOpCVFeedInRate = genAndStorSum;
		} else { // shortfall, will need to draw from main panel (e.g. for grid charging)
			this->storOpCVFeedInRate = 0.0;
			this->storOpCVDrawRate   = std::abs( genAndStorSum );
		}
	}

	void
	ElectPowerLoadCenter::setupLoadCenterMeterIndices()
	{
		this->demandMeterPtr_ = EnergyPlus::GetMeterIndex( this->demandMeterName_ );
		if ( ( this->demandMeterPtr_ == 0 ) && ( this->genOperationScheme_ == GeneratorOpScheme::trackMeter ) ) { // throw error
				ShowFatalError( "ElectPowerLoadCenter::setupLoadCenterMeterIndices  Did not find Meter named: " + this->demandMeterName_ + " in ElectricLoadCenter:Distribution named " + this->name_ );
		}

		if ( this->storageScheme_ == StorageOpScheme::meterDemandStoreExcessOnSite ) {
			this->trackStorageOpMeterIndex_ = EnergyPlus::GetMeterIndex( this->trackSorageOpMeterName_ );
			if ( this->trackStorageOpMeterIndex_ == 0 ) { // 
				ShowFatalError( "ElectPowerLoadCenter::setupLoadCenterMeterIndices  Did not find Meter named: " + this->trackSorageOpMeterName_ + " in ElectricLoadCenter:Distribution named " + this->name_ );
			}
		}
	}

	void
	ElectPowerLoadCenter::reinitAtBeginEnvironment()
	{
		this->dCElectricityProd_        = 0.0;
		this->dCElectProdRate_          = 0.0;
		this->dCpowerConditionLosses_   = 0.0;
		this->genElectricProd          = 0.0;
		this->genElectProdRate            = 0.0;
		this->thermalProd              = 0.0;
		this->thermalProdRate          = 0.0;
		this->totalPowerRequest_        = 0.0;
		this->totalThermalPowerRequest_ = 0.0;
		this->subpanelFeedInRate       = 0.0;
		this->subpanelDrawRate         = 0.0;
		this->storOpCVDrawRate         = 0.0;
		this->storOpCVFeedInRate       = 0.0;
		this->storOpCVChargeRate       = 0.0;
		this->storOpCVDischargeRate    = 0.0;

		if ( this->generatorsPresent_ && this->numGenerators > 0 ) {
			for ( auto & g : this->elecGenCntrlObj ) {
				g->reinitAtBeginEnvironment();
			}
		}

		if ( this->transformerObj_ != nullptr ){
			this->transformerObj_->reinitAtBeginEnvironment();
		}

		if ( this->storageObj != nullptr ) {
			this->storageObj->reinitAtBeginEnvironment();
		}

		if ( this->inverterObj != nullptr ) {
			this->inverterObj->reinitAtBeginEnvironment();
		}

		if ( this->converterObj_ != nullptr ) {
			this->converterObj_->reinitAtBeginEnvironment();
		}
	}

	void 
	ElectPowerLoadCenter::reinitZoneGainsAtBeginEnvironment()
	{
		if (this->transformerObj_ != nullptr ){
			this->transformerObj_->reinitZoneGainsAtBeginEnvironment();
		}

		if ( this->storageObj != nullptr ) {
			this->storageObj->reinitZoneGainsAtBeginEnvironment();
		}

		if ( this->inverterPresent && this->inverterObj != nullptr ) {
			this->inverterObj->reinitZoneGainsAtBeginEnvironment();
		}

		if ( this->converterObj_ != nullptr ) {
			this->converterObj_->reinitZoneGainsAtBeginEnvironment();
		}
	}

	std::string
	ElectPowerLoadCenter::transformerName()
	{
		if ( this->transformerPresent_ ) {
			return this->transformerName_;		
		} else {
			return "";
		}
	}

	std::string
	ElectPowerLoadCenter::generatorListName()
	{
		return generatorListName_;
	}

	void
	ElectPowerLoadCenter::updateLoadCenterGeneratorRecords()
	{


		switch ( this->bussType )
		{
		case ElectricBussType::aCBuss: {
			this->genElectProdRate = 0.0;
			this->genElectricProd = 0.0;
			for ( auto & gc : this->elecGenCntrlObj ) {
				this->genElectProdRate += gc->electProdRate; 
				this->genElectricProd  += gc->electricityProd;
			}
			// no inverter, no storage, so generator production equals subpanel feed in
			this->subpanelFeedInRate = this->genElectProdRate;
			if ( this->transformerObj_ != nullptr ) {
				this->subpanelFeedInRate -= this->transformerObj_->getLossRateForInputPower( this->genElectProdRate );
			}
			this->subpanelDrawRate   = 0.0;

			break;
		}
		case ElectricBussType::aCBussStorage: {
			this->genElectProdRate = 0.0;
			this->genElectricProd = 0.0;
			for ( auto & gc : this->elecGenCntrlObj ) {
				this->genElectProdRate += gc->electProdRate; 
				this->genElectricProd  += gc->electricityProd;
			}
			if ( this->storagePresent_ ) {
				this->subpanelFeedInRate = this->genElectProdRate + this->storOpCVDischargeRate - this->storOpCVChargeRate;
			} else {
				this->subpanelFeedInRate = this->genElectProdRate;
			}
			if ( this->transformerObj_ != nullptr ) {
				this->subpanelFeedInRate -= this->transformerObj_->getLossRateForInputPower( this->subpanelFeedInRate );
			}
			this->subpanelDrawRate   = 0.0;
			break;
		}
		case ElectricBussType::dCBussInverter: {
			this->genElectProdRate = 0.0;
			this->genElectricProd = 0.0;
			for ( auto & gc : this->elecGenCntrlObj ) {
				this->genElectProdRate += gc->electProdRate; 
				this->genElectricProd  += gc->electricityProd;
			}

			if ( this->inverterObj != nullptr ) {
				this->subpanelFeedInRate = this->inverterObj->getACPowerOut();
			}
			if ( this->transformerObj_ != nullptr ) {
				this->subpanelFeedInRate -= this->transformerObj_->getLossRateForInputPower( this->subpanelFeedInRate );
			}
			this->subpanelDrawRate   = 0.0;
			break;
		}

		case ElectricBussType::dCBussInverterDCStorage: {
			this->genElectProdRate = 0.0;
			this->genElectricProd = 0.0;
			for ( auto & gc : this->elecGenCntrlObj ) {
				this->genElectProdRate += gc->electProdRate; 
				this->genElectricProd  += gc->electricityProd;
			}
			if ( this->inverterObj != nullptr ) {
				this->subpanelFeedInRate = this->inverterObj->getACPowerOut();
			}

			if ( this->converterObj_ != nullptr ) {
				this->subpanelDrawRate   = this->converterObj_->getACPowerIn();
			}
			if ( this->transformerObj_ != nullptr ) {
				this->subpanelFeedInRate -= this->transformerObj_->getLossRateForInputPower( this->subpanelFeedInRate );
				this->subpanelDrawRate   += this->transformerObj_->getLossRateForOutputPower( this->subpanelDrawRate );
			}
			break;
		}
		case ElectricBussType::dCBussInverterACStorage: {
			this->genElectProdRate = 0.0;
			this->genElectricProd = 0.0;
			for ( auto & gc : this->elecGenCntrlObj ) {
				this->genElectProdRate += gc->electProdRate; 
				this->genElectricProd  += gc->electricityProd;
			}
			if ( this->inverterObj != nullptr && this->storagePresent_  ) {
				this->subpanelFeedInRate = this->inverterObj->getACPowerOut() +  this->storOpCVDischargeRate - this->storOpCVChargeRate;
			}

			this->subpanelDrawRate   = this->storOpCVDrawRate; // no converter for AC storage
			if ( this->transformerObj_ != nullptr ) {
				this->subpanelFeedInRate -= this->transformerObj_->getLossRateForInputPower( this->subpanelFeedInRate );
				this->subpanelDrawRate   += this->transformerObj_->getLossRateForOutputPower( this->subpanelDrawRate );
			}
			break;
		}
		case ElectricBussType::notYetSet: {
			// do nothing
		}

		} // end switch
		this->thermalProdRate = 0.0;
		this->thermalProd = 0.0;
		for ( auto & gc : this->elecGenCntrlObj ) { 
			this->thermalProdRate += gc->thermProdRate;
			this->thermalProd     += gc->thermalProd;
		}

	}

	void
	ElectPowerLoadCenter::calcLoadCenterThermalLoad(
		Real64 & thermalLoad // heat rate called for from cogenerator(watts)
	)
	{
		if ( this->myCoGenSetupFlag_ ) {
			bool plantNotFound = false;
			for ( auto & g : this->elecGenCntrlObj ) {
				plantNotFound = false;
				DataPlant::ScanPlantLoopsForObject( g->name, g->compPlantTypeOf_Num, g->cogenLocation.loopNum, g->cogenLocation.loopSideNum, g->cogenLocation.branchNum, g->cogenLocation.compNum, _, _, _, _, _, plantNotFound );
				if ( ! plantNotFound ) g->plantInfoFound = true;
			}
		} // cogen setup

		// sum up "MyLoad" for all generators on this load center from plant structure
		thermalLoad = 0.0;
		for ( auto & g : this->elecGenCntrlObj ) {
			if ( g->plantInfoFound  ) {
				thermalLoad += DataPlant::PlantLoop( g->cogenLocation.loopNum ).LoopSide( g->cogenLocation.loopSideNum ).Branch( g->cogenLocation.branchNum ).Comp( g->cogenLocation.compNum ).MyLoad;
			}
		}
	}

	GeneratorController::GeneratorController(
		std::string objectName,
		std::string objectType,
		Real64 ratedElecPowerOutput,
		std::string availSchedName,
		Real64 thermalToElectRatio
	):
		compGenTypeOf_Num( 0 ),
		compPlantTypeOf_Num( 0 ),
		generatorType( GeneratorType::notYetSet ),
		generatorIndex( 0 ),
		maxPowerOut( 0.0 ),
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
		thermProdRate( 0.0 )
	{

		std::string const routineName = "GeneratorController constructor ";
		bool errorsFound = false;

		this->name                   = objectName;
		this->typeOfName             = objectType;
		if ( InputProcessor::SameString( objectType, "Generator:InternalCombustionEngine" ) ) {
			this->generatorType = GeneratorType::iCEngine;
			this->compGenTypeOf_Num   = DataGlobalConstants::iGeneratorICEngine;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_Generator_ICEngine;
		} else if ( InputProcessor::SameString( objectType, "Generator:CombustionTurbine" ) ) {
			this->generatorType = GeneratorType::combTurbine;
			this->compGenTypeOf_Num = DataGlobalConstants::iGeneratorCombTurbine;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_Generator_CTurbine;
		} else if ( InputProcessor::SameString( objectType, "Generator:MicroTurbine" ) ) {
			this->generatorType = GeneratorType::microturbine;
			this->compGenTypeOf_Num = DataGlobalConstants::iGeneratorMicroturbine;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_Generator_MicroTurbine;
		} else if ( InputProcessor::SameString( objectType, "Generator:Photovoltaic" ) ) {
			this->generatorType = GeneratorType::pV;
			this->compGenTypeOf_Num = DataGlobalConstants::iGeneratorPV;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_PVTSolarCollectorFlatPlate;
		} else if ( InputProcessor::SameString( objectType, "Generator:FuelCell" ) ) {
			this->generatorType = GeneratorType::fuelCell;
			this->compGenTypeOf_Num = DataGlobalConstants::iGeneratorFuelCell;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_Generator_FCStackCooler;
		} else if ( InputProcessor::SameString( objectType, "Generator:MicroCHP" ) ) {
			this->generatorType = GeneratorType::microCHP;
			this->compGenTypeOf_Num = DataGlobalConstants::iGeneratorMicroCHP;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_Generator_MicroCHP;
		} else if ( InputProcessor::SameString( objectType, "Generator:WindTurbine" ) ) {
			this->generatorType = GeneratorType::windTurbine;
			this->compGenTypeOf_Num = DataGlobalConstants::iGeneratorWindTurbine;
			this->compPlantTypeOf_Num = DataPlant::TypeOf_Other;
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
		this->thermProdRate   = 0.0;
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
		case GeneratorType::iCEngine: {
			ICEngineElectricGenerator::SimICEngineGenerator( DataGlobalConstants::iGeneratorICEngine, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			ICEngineElectricGenerator::GetICEGeneratorResults( DataGlobalConstants::iGeneratorICEngine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::combTurbine: {
			CTElectricGenerator::SimCTGenerator( DataGlobalConstants::iGeneratorCombTurbine, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			CTElectricGenerator::GetCTGeneratorResults( DataGlobalConstants::iGeneratorCombTurbine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::pV: {
			Photovoltaics::SimPVGenerator( DataGlobalConstants::iGeneratorPV, this->name, this->generatorIndex, runFlag, myElecLoadRequest );
			Photovoltaics::GetPVGeneratorResults( DataGlobalConstants::iGeneratorPV, this->generatorIndex, this->dCElectProdRate, this->dCElectricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->dCElectProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::fuelCell: {
			FuelCellElectricGenerator::SimFuelCellGenerator( DataGlobalConstants::iGeneratorFuelCell, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			FuelCellElectricGenerator::GetFuelCellGeneratorResults( DataGlobalConstants::iGeneratorFuelCell, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::microCHP: {
			MicroCHPElectricGenerator::SimMicroCHPGenerator( DataGlobalConstants::iGeneratorMicroCHP, this->name, this->generatorIndex, runFlag, false, myElecLoadRequest, DataPrecisionGlobals::constant_zero, FirstHVACIteration );
			MicroCHPElectricGenerator::GetMicroCHPGeneratorResults( DataGlobalConstants::iGeneratorMicroCHP, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::microturbine: {
			MicroturbineElectricGenerator::SimMTGenerator( DataGlobalConstants::iGeneratorMicroturbine, this->name, this->generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration );
			MicroturbineElectricGenerator::GetMTGeneratorResults( DataGlobalConstants::iGeneratorMicroturbine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::windTurbine: {
			WindTurbine::SimWindTurbine( DataGlobalConstants::iGeneratorWindTurbine, this->name, this->generatorIndex, runFlag, myElecLoadRequest );
			WindTurbine::GetWTGeneratorResults( DataGlobalConstants::iGeneratorWindTurbine, this->generatorIndex, this->electProdRate, this->electricityProd, this->thermProdRate, this->thermalProd );
			electricPowerOutput = this->electProdRate;
			thermalPowerOutput = this->thermProdRate;
			break;
		}
		case GeneratorType::notYetSet: {
			// do nothing
			break;
		}
		} // end switch
	}

	DCtoACInverter::DCtoACInverter(
		std::string const objectName
	):
		aCPowerOut( 0.0 ),
		aCEnergyOut( 0.0 ),
		efficiency( 0.0 ),
		dCPowerIn( 0.0 ),
		dCEnergyIn( 0.0 ),
		conversionLossPower( 0.0 ),
		conversionLossEnergy( 0.0 ),
		conversionLossEnergyDecrement( 0.0 ),
		thermLossRate( 0.0 ),
		thermLossEnergy( 0.0 ),
		qdotConvZone( 0.0 ),
		qdotRadZone( 0.0 ),
		ancillACuseRate( 0.0 ),
		ancillACuseEnergy( 0.0 ),
		modelType_( InverterModelType::notYetSet ),
		availSchedPtr_( 0 ),
		heatLossesDestination_( ThermalLossDestination::heatLossNotDetermined ),
		zoneNum_( 0 ),
		zoneRadFract_( 0.0 ),
		nominalVoltage_( 0.0 ),
		nomVoltEfficiencyARR_( 6, 0.0 ),
		curveNum_( 0 ),
		ratedPower_( 0.0 ),
		minPower_( 0.0 ),
		maxPower_( 0.0 ),
		minEfficiency_( 0.0 ),
		maxEfficiency_( 0.0 ),
		standbyPower_( 0.0 )
	{
		//initialize
		this->nomVoltEfficiencyARR_.resize( 6, 0.0 );

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
			this->modelType_ = InverterModelType::cECLookUpTableModel;
		}
		testInvertIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Inverter:FunctionOfPower",  objectName );
		if ( testInvertIndex > 0) {
			foundInverter = true;
			invertIDFObjectNum = testInvertIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:FunctionOfPower";
			this->modelType_ = InverterModelType::curveFuncOfPower;
		}
		testInvertIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Inverter:Simple",  objectName );
		if ( testInvertIndex > 0) {
			foundInverter = true;
			invertIDFObjectNum = testInvertIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:Simple";
			this->modelType_ = InverterModelType::simpleConstantEff;
		}

		if ( foundInverter ){

			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, invertIDFObjectNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name_          = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?

			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr_ == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			this->zoneNum_ = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 3 ), DataHeatBalance::Zone );
			if ( this->zoneNum_ > 0 ) this->heatLossesDestination_ = ThermalLossDestination::zoneGains;
			if ( this->zoneNum_ == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 3 ) ) {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
				} else {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					ShowContinueError( "Zone name not found. Inverter heat losses will not be added to a zone" );
					// continue with simulation but inverter losses not sent to a zone.
				}
			}
			this->zoneRadFract_ = DataIPShortCuts::rNumericArgs( 1 );

			// now the input objects differ depending on class type
			switch ( this->modelType_ )
			{
			case InverterModelType::cECLookUpTableModel: {
				this->ratedPower_                = DataIPShortCuts::rNumericArgs( 2 );
				this->standbyPower_              = DataIPShortCuts::rNumericArgs( 3 );

				this->nominalVoltage_            = DataIPShortCuts::rNumericArgs( 4 );
				this->nomVoltEfficiencyARR_[ 0 ] = DataIPShortCuts::rNumericArgs( 5 );
				this->nomVoltEfficiencyARR_[ 1 ] = DataIPShortCuts::rNumericArgs( 6 );
				this->nomVoltEfficiencyARR_[ 2 ] = DataIPShortCuts::rNumericArgs( 7 );
				this->nomVoltEfficiencyARR_[ 3 ] = DataIPShortCuts::rNumericArgs( 8 );
				this->nomVoltEfficiencyARR_[ 4 ] = DataIPShortCuts::rNumericArgs( 9 );
				this->nomVoltEfficiencyARR_[ 5 ] = DataIPShortCuts::rNumericArgs( 10 );
				break;
			}
			case InverterModelType::curveFuncOfPower: {
				this->curveNum_ = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 4 ) );
				if ( this->curveNum_ == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = " + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Curve was not found" );
					errorsFound = true;
				}

				this->ratedPower_    = DataIPShortCuts::rNumericArgs( 2 );
				this->minEfficiency_ = DataIPShortCuts::rNumericArgs( 3 );
				this->maxEfficiency_ = DataIPShortCuts::rNumericArgs( 4 );
				this->minPower_      = DataIPShortCuts::rNumericArgs( 5 );
				this->maxPower_      = DataIPShortCuts::rNumericArgs( 6 );
				this->standbyPower_  = DataIPShortCuts::rNumericArgs( 7 );
				break;
			}
			case InverterModelType::simpleConstantEff: {
				this->efficiency = DataIPShortCuts::rNumericArgs( 2 );
				break;
			}
			case InverterModelType::notYetSet: {
				// do nothing
				break;
			}

			} // end switch modelType
		
			SetupOutputVariable( "Inverter DC to AC Efficiency []", this->efficiency, "System", "Average", this->name_ );
			SetupOutputVariable( "Inverter DC Input Electric Power [W]", this->dCPowerIn, "System", "Average", this->name_ );
			SetupOutputVariable( "Inverter DC Input Electric Energy [J]", this->dCEnergyIn, "System", "Sum", this->name_ );
			SetupOutputVariable( "Inverter AC Output Electric Power [W]", this->aCPowerOut, "System", "Average", this->name_ );
			SetupOutputVariable( "Inverter AC Output Electric Energy [J]", this->aCEnergyOut, "System", "Sum", this->name_ ); 
			SetupOutputVariable( "Inverter Conversion Loss Power [W]", this->conversionLossPower, "System", "Average", this->name_ );
			SetupOutputVariable( "Inverter Conversion Loss Energy [J]", this->conversionLossEnergy, "System", "Sum", this->name_ );
			SetupOutputVariable( "Inverter Conversion Loss Decrement Energy [J]", this->conversionLossEnergyDecrement, "System", "Sum", this->name_, _, "ElectricityProduced", "POWERCONVERSION", _, "Plant" );
			SetupOutputVariable( "Inverter Thermal Loss Rate [W]", this->thermLossRate, "System", "Average", this->name_ );
			SetupOutputVariable( "Inverter Thermal Loss Energy [J]", this->thermLossEnergy, "System", "Sum", this->name_ );
			SetupOutputVariable( "Inverter Ancillary AC Electric Power [W]", this->ancillACuseRate, "System", "Average", this->name_ );
			SetupOutputVariable( "Inverter Ancillary AC Electric Energy [J]", this->ancillACuseEnergy, "System", "Sum", this->name_, _, "Electricity", "Cogeneration", "DCtoACInverter Ancillary", "Plant" ); // called cogeneration for end use table
			if ( this->zoneNum_ > 0 ) {
				switch (this->modelType_ )
				{
				case InverterModelType::simpleConstantEff: {
					SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Inverter:Simple", this->name_, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterSimple, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case InverterModelType::curveFuncOfPower: {
					SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Inverter:FunctionOfPower", this->name_, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case InverterModelType::cECLookUpTableModel: {
					SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Inverter:LookUpTable", this->name_, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, this->qdotConvZone, _, this->qdotRadZone );
					break;
				}
				case InverterModelType::notYetSet: {
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

	Real64
	DCtoACInverter::getLossRateForOutputPower( 
		Real64 const powerOutOfInverter
	)
	{

	//need to invert, find a dCPowerIn that produces the desired AC power out
	// use last efficiency for initial guess
		if ( this->efficiency > 0.0 ) {
			this->dCPowerIn = powerOutOfInverter / this->efficiency;		
		} else {
			this->dCPowerIn = powerOutOfInverter;
			this->calcEfficiency();
			this->dCPowerIn = powerOutOfInverter / this->efficiency;

		}

		this->calcEfficiency();
		// one more update is close enough.
		if ( this->efficiency > 0.0 ) {
			this->dCPowerIn = powerOutOfInverter / this->efficiency;
		}
		this->calcEfficiency();
		return ( 1.0 - this->efficiency ) * this->dCPowerIn;

	}

	void
	DCtoACInverter::calcEfficiency()
	{

		switch ( this->modelType_ )
		{
			case InverterModelType::cECLookUpTableModel: {
				// we don't model voltage, so use nominal voltage
				Real64 normalizedPower = this->dCPowerIn / this->ratedPower_;

				// get efficiency
				if ( normalizedPower <= 0.1 ) {
					// extrapolate or fix at 10% value? fix it for now
					this->efficiency = this->nomVoltEfficiencyARR_[ 0 ];
				} else if ( ( normalizedPower > 0.1 ) && ( normalizedPower < 0.20 ) ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 0 ] + ( ( normalizedPower - 0.1 ) / ( 0.2 - 0.1 ) ) * ( this->nomVoltEfficiencyARR_[ 1 ] - this->nomVoltEfficiencyARR_[ 0 ] );
				} else if ( normalizedPower == 0.2 ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 1 ];
				} else if ( ( normalizedPower > 0.2 ) && ( normalizedPower < 0.30 ) ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 1 ] + ( ( normalizedPower - 0.2 ) / ( 0.3 - 0.2 ) ) * ( this->nomVoltEfficiencyARR_[ 2 ] - this->nomVoltEfficiencyARR_[ 1 ] );
				} else if ( normalizedPower == 0.3 ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 2 ];
				} else if ( ( normalizedPower > 0.3 ) && ( normalizedPower < 0.50 ) ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 2 ] + ( ( normalizedPower - 0.3 ) / ( 0.5 - 0.3 ) ) * ( this->nomVoltEfficiencyARR_[ 3 ] - this->nomVoltEfficiencyARR_[ 2 ] );
				} else if ( normalizedPower == 0.5 ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 3 ];
				} else if ( ( normalizedPower > 0.5 ) && ( normalizedPower < 0.75 ) ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 3 ] + ( ( normalizedPower - 0.5 ) / ( 0.75 - 0.5 ) ) * ( this->nomVoltEfficiencyARR_[ 4 ] - this->nomVoltEfficiencyARR_[ 3 ] );
				} else if ( normalizedPower == 0.75 ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 4 ];
				} else if ( ( normalizedPower > 0.75 ) && ( normalizedPower < 1.0 ) ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 4 ] + ( ( normalizedPower - 0.75 ) / ( 1.0 - 0.75 ) ) * ( this->nomVoltEfficiencyARR_[ 5 ] - this->nomVoltEfficiencyARR_[ 4 ] );
				} else if ( normalizedPower >= 1.0 ) {
					this->efficiency = this->nomVoltEfficiencyARR_[ 5 ];
				} else {
					assert( false );
				}

				this->efficiency = max( this->efficiency, 0.0 );
				this->efficiency = min( this->efficiency, 1.0 );

				break;
			}
			case InverterModelType::curveFuncOfPower: {

				Real64 normalizedPower = this->dCPowerIn / this->ratedPower_;

				this->efficiency = CurveManager::CurveValue( this->curveNum_, normalizedPower );

				this->efficiency = max( this->efficiency, this->minEfficiency_ );
				this->efficiency = min( this->efficiency, this->maxEfficiency_ );


				break;
			}
			case InverterModelType::simpleConstantEff: 
			case InverterModelType::notYetSet: {
				// do nothing

				break;
			}
		} // end switch
	}

	void
	DCtoACInverter::simulate(
		Real64 const powerIntoInverter
	)
	{
		this->dCPowerIn = powerIntoInverter;
		this->dCEnergyIn = this->dCPowerIn * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
			// check availability schedule
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr_ ) > 0.0 ) {

			// now calculate Inverter based on model type
			this->calcEfficiency();
			this->aCPowerOut  = this->efficiency * this->dCPowerIn;
			this->aCEnergyOut = this->aCPowerOut * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );

			if ( this->aCPowerOut == 0.0 ) {
				this->ancillACuseEnergy = this->standbyPower_ * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
				this->ancillACuseRate = this->standbyPower_;
			} else {
				this->ancillACuseRate   = 0.0;
				this->ancillACuseEnergy = 0.0;
			}
		} else { // not available per schedule, inverter is dead.
			//  assume thermal shunt for DC in, but no standby electricity
			this->aCPowerOut = 0.0;
			this->aCEnergyOut = 0.0;
			this->ancillACuseRate = 0.0;
			this->ancillACuseEnergy = 0.0;

		}
		//update report variables
		this->conversionLossPower = this->dCPowerIn - this->aCPowerOut;
		this->conversionLossEnergy = this->conversionLossPower * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->conversionLossEnergyDecrement = -1.0 * this->conversionLossEnergy;
		this->thermLossRate    = this->dCPowerIn - this->aCPowerOut + this->ancillACuseRate;
		this->thermLossEnergy = this->thermLossRate * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->qdotConvZone    = this->thermLossRate * ( 1.0 - this->zoneRadFract_ );
		this->qdotRadZone     = this->thermLossRate * this->zoneRadFract_;
	}

	ACtoDCConverter::ACtoDCConverter(
		std::string const objectName
	):
		efficiency( 0.0 ),
		aCPowerIn( 0.0 ),
		aCEnergyIn( 0.0 ),
		dCPowerOut( 0.0 ),
		dCEnergyOut( 0.0 ),
		conversionLossPower( 0.0 ),
		conversionLossEnergy( 0.0 ),
		conversionLossEnergyDecrement( 0.0 ),
		thermLossRate( 0.0 ),
		thermLossEnergy( 0.0 ),
		qdotConvZone( 0.0 ),
		qdotRadZone( 0.0 ),
		ancillACuseRate( 0.0 ),
		ancillACuseEnergy( 0.0 ),
		availSchedPtr_( 0 ),
		modelType_( ConverterModelType::notYetSet ),
		heatLossesDestination_( ThermalLossDestination::heatLossNotDetermined ),
		zoneNum_( 0 ),
		zoneRadFract_( 0.0 ), // radiative fraction for thermal losses to zone
		standbyPower_( 0.0 ), 
		maxPower_( 0.0 )
	{

		std::string const routineName = "ACtoDCConverter constructor ";
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool errorsFound = false;
		// if/when add object class name to input object this can be simplified. for now search all possible types 

		int testConvertIndex = InputProcessor::GetObjectItemNum( "ElectricLoadCenter:Storage:Converter",  objectName );

		if ( testConvertIndex > 0 ) {
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Converter";

			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, testConvertIndex, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name_          = DataIPShortCuts::cAlphaArgs( 1 );
			// need a new general approach for verify names are unique across objects,  next gen GlobalNames

			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr_ == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			if ( InputProcessor::SameString(  DataIPShortCuts::cAlphaArgs( 3 ), "SimpleFixed" ) ) {
				this->modelType_ = ConverterModelType::simpleConstantEff;

			} else if ( InputProcessor::SameString(  DataIPShortCuts::cAlphaArgs( 3 ), "FunctionOfPower" ) ) {
				this->modelType_ = ConverterModelType::curveFuncOfPower;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
				errorsFound = true;
			}
			
			switch ( this->modelType_ )
			{
			case ConverterModelType::simpleConstantEff : {
				this->efficiency = DataIPShortCuts::rNumericArgs( 1 );
				break;
			}

			case ConverterModelType::curveFuncOfPower: {
				this->maxPower_ = DataIPShortCuts::rNumericArgs( 2 );
				this->curveNum_ = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 4 ) );
				if ( this->curveNum_ == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = " + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Curve was not found" );
					errorsFound = true;
				}
				break;
			}
			case ConverterModelType::notYetSet: {
				//do nothing
			}
			} // end switch

			this->standbyPower_ = DataIPShortCuts::rNumericArgs( 3 );

			this->zoneNum_ = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 5 ), DataHeatBalance::Zone );
			if ( this->zoneNum_ > 0 ) this->heatLossesDestination_ = ThermalLossDestination::zoneGains;
			if ( this->zoneNum_ == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 5 ) ) {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
				} else {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + " = " + DataIPShortCuts::cAlphaArgs( 5 ) );
					ShowContinueError( "Zone name not found. Inverter heat losses will not be added to a zone" );
					// continue with simulation but inverter losses not sent to a zone.
				}
			}
			this->zoneRadFract_ = DataIPShortCuts::rNumericArgs( 4 );

			SetupOutputVariable( "Converter AC to DC Efficiency []", this->efficiency, "System", "Average", this->name_ );
			SetupOutputVariable( "Converter AC Input Electric Power [W]", this->aCPowerIn, "System", "Average", this->name_ );
			SetupOutputVariable( "Converter AC Input Electric Energy [J]", this->aCEnergyIn, "System", "Sum", this->name_ );
			SetupOutputVariable( "Converter DC Output Electric Power [W]", this->dCPowerOut, "System", "Average", this->name_ );
			SetupOutputVariable( "Converter DC Output Electric Energy [J]", this->dCEnergyOut, "System", "Sum", this->name_ );

			SetupOutputVariable( "Converter Electric Loss Power [W]", this->conversionLossPower, "System", "Average", this->name_ );
			SetupOutputVariable( "Converter Electric Loss Energy [J]", this->conversionLossEnergy, "System", "Sum", this->name_ );
			SetupOutputVariable( "Converter Electric Loss Decrement Energy [J]", this->conversionLossEnergyDecrement, "System", "Sum", this->name_, _, "ElectricityProduced", "POWERCONVERSION", _, "Plant" );
			SetupOutputVariable( "Converter Thermal Loss Rate [W]", this->thermLossRate, "System", "Average", this->name_ );
			SetupOutputVariable( "Converter Thermal Loss Energy [J]", this->thermLossEnergy, "System", "Sum", this->name_ );
			SetupOutputVariable( "Converter Ancillary AC Electric Power [W]", this->ancillACuseRate, "System", "Average", this->name_ );
			SetupOutputVariable( "Converter Ancillary AC Electric Energy [J]", this->ancillACuseEnergy, "System", "Sum", this->name_,  _, "Electricity", "Cogeneration", "ACtoDCConverter Ancillary", "Plant" ); // called cogeneration for end use table
			if ( this->zoneNum_ > 0 ) {
					SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Storage:Converter", this->name_, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterConverter, this->qdotConvZone, _, this->qdotRadZone );
			}


		} else {
			ShowSevereError( routineName + " did not find power converter name = " + objectName);
			errorsFound = true;
		}

		if ( errorsFound ) {
			ShowFatalError( routineName + "Preceding errors terminate program." );
		}
	}

	void ACtoDCConverter::reinitAtBeginEnvironment()
	{
		this->ancillACuseRate   = 0.0;
		this->ancillACuseEnergy = 0.0;
		this->qdotConvZone      = 0.0;
		this->qdotRadZone       = 0.0;
	}

	void ACtoDCConverter::reinitZoneGainsAtBeginEnvironment()
	{
		this->qdotConvZone            = 0.0;
		this->qdotRadZone             = 0.0;
	}

	Real64 ACtoDCConverter::getThermLossRate()
	{
		return this->thermLossRate;
	}

	Real64 ACtoDCConverter::getDCPowerOut()
	{
		return this->dCPowerOut;
	}

	Real64 ACtoDCConverter::getDCEnergyOut()
	{
		return this->dCEnergyOut;
	}

	Real64 ACtoDCConverter::getACPowerIn()
	{
		return this->aCPowerIn;
	}

	Real64
	ACtoDCConverter::getLossRateForInputPower(
		Real64 const powerIntoConverter 
	)
	{
		this->aCPowerIn = powerIntoConverter;
		this->calcEfficiency();
		return ( 1.0 - this->efficiency ) * this->aCPowerIn;
	}

	void
	ACtoDCConverter::calcEfficiency()
	{
		switch ( modelType_ ) 
		{
			case ConverterModelType::notYetSet : 
			case ConverterModelType::simpleConstantEff : {
				break;
			}
			case ConverterModelType::curveFuncOfPower : {
				Real64 normalizedPower = this->aCPowerIn / this->maxPower_;
				this->efficiency = CurveManager::CurveValue( this->curveNum_, normalizedPower );
				break;
			}
		} // end switch
	}

	void
	ACtoDCConverter::simulate(
		Real64 const powerOutFromConverter
	)
	{
		//need to invert, find an aCPowerIn that produces the desired DC power out  

		// use last efficiency for initial guess
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr_ ) > 0.0 ) {
		
			this->aCPowerIn = powerOutFromConverter / this->efficiency;
			this->calcEfficiency(),
			this->aCPowerIn = powerOutFromConverter / this->efficiency;
			this->calcEfficiency(),

			this->dCPowerOut = this->aCPowerIn  * this->efficiency;

			if ( this->dCPowerOut == 0.0 ) {
				this->ancillACuseEnergy = this->standbyPower_ * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
				this->ancillACuseRate = this->standbyPower_;
			} else {
				this->ancillACuseRate   = 0.0;
				this->ancillACuseEnergy = 0.0;
			}
		
		} else { // not available
			this->aCPowerIn = 0.0;
			this->dCPowerOut = 0.0;
			this->ancillACuseRate = 0.0;
			this->ancillACuseEnergy = 0.0;
		}

		// update and report
		this->aCEnergyIn  = this->aCPowerIn * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->dCEnergyOut = this->dCPowerOut * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->conversionLossPower = this->aCPowerIn - this->dCPowerOut;
		this->conversionLossEnergy = this->conversionLossPower * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->conversionLossEnergyDecrement = -1.0 * this->conversionLossEnergy;
		this->thermLossRate    = this->aCPowerIn - this->dCPowerOut + this->ancillACuseRate;
		this->thermLossEnergy = this->thermLossRate * ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
		this->qdotConvZone    = this->thermLossRate * ( 1.0 - this->zoneRadFract_ );
		this->qdotRadZone     = this->thermLossRate * this->zoneRadFract_;
	}

	ElectricStorage::ElectricStorage( // main constructor
		std::string objectName
	):
			storedPower( 0.0 ),
			storedEnergy( 0.0 ),
			drawnPower( 0.0 ),
			drawnEnergy( 0.0 ),
			decrementedEnergyStored( 0.0 ),
			maxRainflowArrayBounds_( 100 ),
			maxRainflowArrayInc_( 100 ),
			myWarmUpFlag_( false ),
			storageModelMode_( StorageModelType::storageTypeNotSet ),
			availSchedPtr_( 0 ),
			heatLossesDestination_( ThermalLossDestination::heatLossNotDetermined ),
			zoneNum_( 0 ),
			zoneRadFract_( 0.0 ),
			startingEnergyStored_( 0.0 ),
			energeticEfficCharge_( 0.0 ),
			energeticEfficDischarge_( 0.0 ),
			maxPowerDraw_( 0.0 ),
			maxPowerStore_( 0.0 ),
			maxEnergyCapacity_( 0.0 ),
			parallelNum_( 0 ),
			seriesNum_( 0 ),
			numBattery_( 0 ),
			chargeCurveNum_( 0 ),
			dischargeCurveNum_( 0 ),
			cycleBinNum_( 0 ),
			startingSOC_( 0.0 ),
			maxAhCapacity_( 0.0 ),
			availableFrac_( 0.0 ),
			chargeConversionRate_( 0.0 ),
			chargedOCV_( 0.0 ),
			dischargedOCV_( 0.0 ),
			internalR_( 0.0 ),
			maxDischargeI_( 0.0 ),
			cutoffV_( 0.0 ),
			maxChargeRate_( 0.0 ),
			lifeCalculation_( BatteyDegredationModelType::degredationNotSet ),
			lifeCurveNum_( 0 ),
			thisTimeStepStateOfCharge_( 0.0 ),
			lastTimeStepStateOfCharge_( 0.0 ),
			pelNeedFromStorage_( 0.0 ),
			pelFromStorage_( 0.0 ),
			pelIntoStorage_( 0.0 ),
			qdotConvZone_( 0.0 ),
			qdotRadZone_( 0.0 ),
			timeElapsed_( 0.0 ),
			thisTimeStepAvailable_( 0.0 ),
			thisTimeStepBound_( 0.0 ),
			lastTimeStepAvailable_( 0.0 ),
			lastTimeStepBound_( 0.0 ),
			lastTwoTimeStepAvailable_( 0.0 ),
			lastTwoTimeStepBound_( 0.0 ),
			count0_( 0 ),
			electEnergyinStorage_( 0.0 ),
			thermLossRate_( 0.0 ),
			thermLossEnergy_( 0.0 ),
			storageMode_( 0 ),
			absoluteSOC_( 0.0 ),
			fractionSOC_( 0.0 ),
			batteryCurrent_( 0.0 ),
			batteryVoltage_( 0.0 ),
			batteryDamage_( 0.0 )
	{

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
			this->storageModelMode_ = StorageModelType::simpleBucketStorage;
		}

		testStorageIndex = InputProcessor::GetObjectItemNum("ElectricLoadCenter:Storage:Battery", objectName );
		if ( testStorageIndex > 0 ) {
			foundStorage = true;
			storageIDFObjectNum = testStorageIndex;
			DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Battery";
			this->storageModelMode_ = StorageModelType::kiBaMBattery;
		}

		if ( foundStorage ) {
			InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, storageIDFObjectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, iOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

			this->name          = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?

			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr_ == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			this->zoneNum_ = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 3 ), DataHeatBalance::Zone );
			if ( this->zoneNum_ > 0 ) this->heatLossesDestination_ = ThermalLossDestination::zoneGains;
			if ( this->zoneNum_ == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 3 ) ) {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
				} else {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					ShowContinueError( "Zone name not found. Storage heat losses will not be added to a zone" );
					// continue with simulation but storage losses not sent to a zone.
				}
			}
			this->zoneRadFract_ = DataIPShortCuts::rNumericArgs( 1 );

			switch ( this->storageModelMode_ )
			{
			
			case StorageModelType::simpleBucketStorage: {
				this->energeticEfficCharge_    = DataIPShortCuts::rNumericArgs( 2 );
				this->energeticEfficDischarge_ = DataIPShortCuts::rNumericArgs( 3 );
				this->maxEnergyCapacity_       = DataIPShortCuts::rNumericArgs( 4 );
				this->maxPowerDraw_            = DataIPShortCuts::rNumericArgs( 5 );
				this->maxPowerStore_           = DataIPShortCuts::rNumericArgs( 6 );
				this->startingEnergyStored_    = DataIPShortCuts::rNumericArgs( 7 );
				SetupOutputVariable( "Electric Storage Simple Charge State [J]", this->electEnergyinStorage_, "System", "Average", this->name ); // issue #4921
				break;
			}
			
			case StorageModelType::kiBaMBattery: {
				this->chargeCurveNum_ = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 4 ) ); //voltage calculation for charging
				if ( this->chargeCurveNum_ == 0 && ! DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + '=' + DataIPShortCuts::cAlphaArgs( 4 ) );
					errorsFound = true;
				} else if ( DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " cannot be blank. But no entry found." );
					errorsFound = true;
				} else if ( ! InputProcessor::SameString( CurveManager::GetCurveType( this->chargeCurveNum_ ), "RectangularHyperbola2" ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + '=' + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Curve Type must be RectangularHyperbola2 but was " + CurveManager::GetCurveType( this->chargeCurveNum_ ) );
					errorsFound = true;
				}
				this->dischargeCurveNum_ = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 5 ) ); // voltage calculation for discharging
				if ( this->dischargeCurveNum_ == 0 && ! DataIPShortCuts::lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + '=' + DataIPShortCuts::cAlphaArgs( 5 ) );
					errorsFound = true;
				} else if ( DataIPShortCuts::lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + " cannot be blank. But no entry found." );
					errorsFound = true;
				} else if ( ! InputProcessor::SameString( CurveManager::GetCurveType( this->dischargeCurveNum_ ), "RectangularHyperbola2" ) ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + '=' + DataIPShortCuts::cAlphaArgs( 5 ) );
					ShowContinueError( "Curve Type must be RectangularHyperbola2 but was " + CurveManager::GetCurveType( this->dischargeCurveNum_ ) );
					errorsFound = true;
				}

				if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "Yes" ) ) {
					this->lifeCalculation_ = BatteyDegredationModelType::lifeCalculationYes;
				} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "No" ) ) {
					this->lifeCalculation_ = BatteyDegredationModelType::lifeCalculationNo;
				} else {
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 6 ) + " = " + DataIPShortCuts::cAlphaArgs( 6 ) );
					ShowContinueError( "Yes or No should be selected. Default value No is used to continue simulation" );
					this->lifeCalculation_ = BatteyDegredationModelType::lifeCalculationNo;
				}

				if ( this->lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes ) {
					this->lifeCurveNum_ = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 7 ) ); //Battery life calculation
					if ( this->lifeCurveNum_ == 0 && ! DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
						ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + '=' + DataIPShortCuts::cAlphaArgs( 7 ) );
						errorsFound = true;
					} else if ( DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
						ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + " cannot be blank when " + DataIPShortCuts::cAlphaArgs( 6 ) + " = Yes. But no entry found." );
						errorsFound = true;
					} else if ( ! InputProcessor::SameString( CurveManager::GetCurveType( this->lifeCurveNum_ ), "DoubleExponentialDecay" ) ) {
						ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + '=' + DataIPShortCuts::cAlphaArgs( 7 ) );
						ShowContinueError( "Curve Type must be DoubleExponentialDecay but was " + CurveManager::GetCurveType( this->lifeCurveNum_ ) );
						errorsFound = true;
					}

					this->cycleBinNum_ = DataIPShortCuts::rNumericArgs( 14 );

					if ( ! errorsFound ) { // life cycle calculation for this battery, allocate arrays for degradation calculation
					//std::vector is zero base instead of 1, so first index is now 0. 
						this->b10_.resize( this->maxRainflowArrayBounds_ + 1, 0.0 );
						this->x0_.resize( this->maxRainflowArrayBounds_ + 1, 0 );
						this->nmb0_.resize( this->cycleBinNum_ , 0.0 );
						this->oneNmb0_.resize( this->cycleBinNum_, 0.0 );
					}
				}

				this->parallelNum_          = DataIPShortCuts::rNumericArgs( 2 );
				this->seriesNum_            = DataIPShortCuts::rNumericArgs( 3 );
				this->numBattery_           = this->parallelNum_ * this->seriesNum_ ;
				this->maxAhCapacity_        = DataIPShortCuts::rNumericArgs( 4 );
				this->startingSOC_          = DataIPShortCuts::rNumericArgs( 5 );
				this->availableFrac_        = DataIPShortCuts::rNumericArgs( 6 );
				this->chargeConversionRate_ = DataIPShortCuts::rNumericArgs( 7 );
				this->chargedOCV_           = DataIPShortCuts::rNumericArgs( 8 );
				this->dischargedOCV_        = DataIPShortCuts::rNumericArgs( 9 );
				this->internalR_            = DataIPShortCuts::rNumericArgs( 10 );
				this->maxDischargeI_        = DataIPShortCuts::rNumericArgs( 11 );
				this->cutoffV_              = DataIPShortCuts::rNumericArgs( 12 );
				this->maxChargeRate_        = DataIPShortCuts::rNumericArgs( 13 );

				SetupOutputVariable( "Electric Storage Operating Mode Index []", this->storageMode_, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Battery Charge State [Ah]", this->absoluteSOC_, "System", "Average", this->name );  // issue #4921
				SetupOutputVariable( "Electric Storage Charge Fraction []", this->fractionSOC_, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Total Current [A]", this->batteryCurrent_, "System", "Average", this->name );
				SetupOutputVariable( "Electric Storage Total Voltage [V]", this->batteryVoltage_, "System", "Average", this->name );

				if ( this->lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes ) {
					SetupOutputVariable( "Electric Storage Degradation Fraction []", this->batteryDamage_, "System", "Average", this->name );
				}
				break;
			}
			case StorageModelType::storageTypeNotSet: {
				// do nothing
				break;
			}

			} // switch storage model type

			SetupOutputVariable( "Electric Storage Charge Power [W]", this->storedPower, "System", "Average", this->name  );
			SetupOutputVariable( "Electric Storage Charge Energy [J]", this->storedEnergy, "System", "Sum", this->name  );
			SetupOutputVariable( "Electric Storage Production Decrement Energy [J]", this->decrementedEnergyStored, "System", "Sum", this->name , _, "ElectricityProduced", "ELECTRICSTORAGE", _, "Plant" );
			SetupOutputVariable( "Electric Storage Discharge Power [W]", this->drawnPower, "System", "Average", this->name  );
			SetupOutputVariable( "Electric Storage Discharge Energy [J]", this->drawnEnergy, "System", "Sum", this->name , _, "ElectricityProduced", "ELECTRICSTORAGE", _, "Plant" );
			SetupOutputVariable( "Electric Storage Thermal Loss Rate [W]", this->thermLossRate_, "System", "Average", this->name  );
			SetupOutputVariable( "Electric Storage Thermal Loss Energy [J]", this->thermLossEnergy_, "System", "Sum", this->name  );
			if ( DataGlobals::AnyEnergyManagementSystemInModel ) {
				if ( this->storageModelMode_ == StorageModelType::simpleBucketStorage ) {
					SetupEMSInternalVariable( "Electrical Storage Simple Maximum Capacity", this->name , "[J]", this->maxEnergyCapacity_ );
				} else if ( this->storageModelMode_ == StorageModelType::kiBaMBattery ) {
					SetupEMSInternalVariable( "Electrical Storage Battery Maximum Capacity", this->name , "[Ah]", this->maxAhCapacity_ );
				}
			}

			if ( this->zoneNum_ > 0 ) {
				switch ( this->storageModelMode_ )
				{
				case StorageModelType::simpleBucketStorage: {
					SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Storage:Simple", this->name , DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageSimple, this->qdotConvZone_, _, this->qdotRadZone_ );
					break;
				}
				case StorageModelType::kiBaMBattery: {
					SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Storage:Battery", this->name , DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageBattery, this->qdotConvZone_, _, this->qdotRadZone_ );
					break;
				}
				case StorageModelType::storageTypeNotSet: {
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
		this->pelNeedFromStorage_      = 0.0;
		this->pelFromStorage_          = 0.0;
		this->pelIntoStorage_          = 0.0;
		this->qdotConvZone_            = 0.0;
		this->qdotRadZone_             = 0.0;
		this->timeElapsed_             = 0.0;
		this->electEnergyinStorage_    = 0.0;
		this->storedPower              = 0.0;
		this->storedEnergy             = 0.0;
		this->decrementedEnergyStored  = 0.0;
		this->drawnPower               = 0.0;
		this->drawnEnergy              = 0.0;
		this->thermLossRate_           = 0.0;
		this->thermLossEnergy_         = 0.0;
		this->lastTimeStepStateOfCharge_ = this->startingEnergyStored_;
		this->thisTimeStepStateOfCharge_ = this->startingEnergyStored_;

		if ( this->storageModelMode_ == StorageModelType::kiBaMBattery ) {
			Real64 initialCharge = this->maxAhCapacity_ * this->startingSOC_;
			this->lastTwoTimeStepAvailable_ = initialCharge * this->availableFrac_;
			this->lastTwoTimeStepBound_ = initialCharge * ( 1.0 - this->availableFrac_ );
			this->lastTimeStepAvailable_ = initialCharge * this->availableFrac_;
			this->lastTimeStepBound_ = initialCharge * ( 1.0 - this->availableFrac_ );
			this->thisTimeStepAvailable_ = initialCharge * this->availableFrac_;
			this->thisTimeStepBound_ = initialCharge * ( 1.0 - this->availableFrac_ );
			if ( this->lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes ) {
				this->count0_ = 1; // Index 0 is for initial SOC, so new input starts from index 2.
				this->b10_[ 0 ] = this->startingSOC_; // the initial fractional SOC is stored as the reference
				this->x0_[ 0 ] = 0;
				for (auto loop = 1; loop < this->maxRainflowArrayBounds_ + 1; ++loop) {
					this->b10_[ loop ] = 0.0;
					this->x0_[ loop ] = 0;
				}
				for (auto loop = 0; loop < this->cycleBinNum_; ++loop) {
					this->oneNmb0_[ loop ] = 0.0;
					this->nmb0_[ loop ] = 0.0;
				}
				this->batteryDamage_ = 0.0;
			}
		}
		this->myWarmUpFlag_ = true;
	}

	void
	ElectricStorage::reinitZoneGainsAtBeginEnvironment()
	{
		this->qdotConvZone_            = 0.0;
		this->qdotRadZone_             = 0.0;
	}

	void
	ElectricStorage::timeCheckAndUpdate()
	{
		Real64 timeElapsedLoc = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
		if ( this->timeElapsed_ != timeElapsedLoc ) { //time changed, update last with "current" result from previous time
			if ( this->storageModelMode_ == StorageModelType::kiBaMBattery && this->lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes ) {
				//    At this point, the current values, last time step values and last two time step values have not been updated, hence:
				//    "ThisTimeStep*" actually points to the previous one time step
				//    "LastTimeStep*" actually points to the previous two time steps
				//    "LastTwoTimeStep" actually points to the previous three time steps

				//      Calculate the fractional SOC change between the "current" time step and the "previous one" time step
				Real64 deltaSOC1 = this->thisTimeStepAvailable_ +this->thisTimeStepBound_ - this->lastTimeStepAvailable_ - this->lastTimeStepBound_;
				deltaSOC1 /= this->maxAhCapacity_;

				//      Calculate the fractional SOC change between the "previous one" time step and the "previous two" time steps
				Real64 deltaSOC2 = this->lastTimeStepAvailable_ + this->lastTimeStepBound_ - this->lastTwoTimeStepAvailable_ - this->lastTwoTimeStepBound_;
				deltaSOC2 /= this->maxAhCapacity_;

				//     DeltaSOC2 = 0 may occur at the begining of each simulation environment.
				//     DeltaSOC1 * DeltaSOC2 means that the SOC from "LastTimeStep" is a peak or valley. Only peak or valley needs
				//     to call the rain flow algorithm
				if ( ( deltaSOC2 == 0 ) || ( ( deltaSOC1 * deltaSOC2 ) < 0 ) ) {
					//     Because we cannot determine whehter "ThisTimeStep" is a peak or valley (next time step is unknown yet), we
					//     use the "LastTimeStep" value for battery life calculation.
					Real64 input0 = ( this->lastTimeStepAvailable_ + this->lastTimeStepBound_ ) / this->maxAhCapacity_;
					this->b10_[ this->count0_ ] = input0;

					//        The arrary size needs to be increased when count = MaxRainflowArrayBounds. Please note that (MaxRainflowArrayBounds +1)
					//        is the index used in the subroutine RainFlow. So we cannot reallocate array size until count = MaxRainflowArrayBounds +1.
					if ( this->count0_ == this->maxRainflowArrayBounds_ ) {
						this->b10_.resize( this->maxRainflowArrayBounds_ + 1 + this->maxRainflowArrayInc_, 0.0 );
						this->x0_.resize( this->maxRainflowArrayBounds_ + 1 + this->maxRainflowArrayInc_, 0.0 );
						this->maxRainflowArrayBounds_ += this->maxRainflowArrayInc_;
					}

					this->rainflow( this->cycleBinNum_, input0, this->b10_, this->x0_, this->count0_, this->nmb0_, this->oneNmb0_);

					this->batteryDamage_ = 0.0;

					for ( auto binNum = 0; binNum < this->cycleBinNum_; ++binNum ) {
						//       Battery damage is calculated by accumulating the impact from each cycle.
						this->batteryDamage_ += this->oneNmb0_[ binNum ] / CurveManager::CurveValue( this->lifeCurveNum_, ( double( binNum ) / double( this->cycleBinNum_ ) ) );
					}
				}
			}

			this->lastTimeStepStateOfCharge_ = this->thisTimeStepStateOfCharge_;
			this->lastTwoTimeStepAvailable_  = this->lastTimeStepAvailable_;
			this->lastTwoTimeStepBound_      = this->lastTimeStepBound_;
			this->lastTimeStepAvailable_     = this->thisTimeStepAvailable_;
			this->lastTimeStepBound_         = this->thisTimeStepBound_;
			this->timeElapsed_               = timeElapsedLoc;

		} // end if time changed
	}

	void
	ElectricStorage::simulate(
		Real64 & powerCharge,
		Real64 & powerDischarge,
		bool & charging,
		bool & discharging,
		Real64 const controlSOCMaxFracLimit,
		Real64 const controlSOCMinFracLimit
	)
	{
		// pass thru to constrain function depending on storage model type 
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr_ ) == 0.0 ) { // storage not available
			discharging = false;
			powerDischarge = 0.0;
			charging = false;
			powerCharge = 0.0;
		}

		if ( this->storageModelMode_ == StorageModelType::simpleBucketStorage ) {
			this->simulateSimpleBucketModel( powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit,controlSOCMinFracLimit );
		} else if ( this->storageModelMode_ == StorageModelType::kiBaMBattery ) {
			this->simulateKineticBatteryModel( powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit,controlSOCMinFracLimit );
		}
	}

	void
	ElectricStorage::simulateSimpleBucketModel(
		Real64 & powerCharge,
		Real64 & powerDischarge,
		bool & charging,
		bool & discharging,
		Real64 const controlSOCMaxFracLimit,
		Real64 const controlSOCMinFracLimit
	)
	{
		// given arguments for how the storage operation would like to run storage charge or discharge
		// apply model constraints and adjust arguments accordingly

		if ( charging ) {

			if ( this->lastTimeStepStateOfCharge_ >= (this->maxEnergyCapacity_ * controlSOCMaxFracLimit) ) {
				// storage full!  no more allowed!
				powerCharge = 0.0;
				charging = false;
			}
			if ( powerCharge > this->maxPowerStore_ ) {
				powerCharge = this->maxPowerStore_;
			}

			//now check to see if charge would exceed capacity, and modify to just fill physical storage cap
			if ( ( this->lastTimeStepStateOfCharge_ + powerCharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge_ ) >= (this->maxEnergyCapacity_ * controlSOCMaxFracLimit) ) {
				powerCharge = ( ( this->maxEnergyCapacity_ * controlSOCMaxFracLimit) - this->lastTimeStepStateOfCharge_ ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge_ );
			}

		} //charging

		if ( discharging ) {

			if ( this->lastTimeStepStateOfCharge_ <= (this->maxEnergyCapacity_ * controlSOCMinFracLimit) ) {
				// storage empty  no more allowed!
				powerDischarge = 0.0;
				discharging = false;
			}
			if ( powerDischarge > this->maxPowerDraw_ ) {
				powerDischarge = this->maxPowerDraw_;
			}
			//now check if will empty this timestep, power draw is amplified by energetic effic
			if ( ( this->lastTimeStepStateOfCharge_ - powerDischarge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->energeticEfficDischarge_ ) <= ( this->maxEnergyCapacity_ * controlSOCMinFracLimit ) ) {
				powerDischarge = ( this->lastTimeStepStateOfCharge_ - ( this->maxEnergyCapacity_ * controlSOCMinFracLimit ) ) * this->energeticEfficDischarge_ / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
			}
		}

		if ( ( ! charging ) && ( ! discharging ) ) {
			this->thisTimeStepStateOfCharge_ = this->lastTimeStepStateOfCharge_;
			this->pelIntoStorage_ = 0.0;
			this->pelFromStorage_ = 0.0;
		}
		if ( charging ) {
			this->pelIntoStorage_ = powerCharge;
			this->pelFromStorage_ = 0.0;
			this->thisTimeStepStateOfCharge_ = this->lastTimeStepStateOfCharge_ + powerCharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->energeticEfficCharge_;
		}
		if ( discharging ) {
			this->pelIntoStorage_ = 0.0;
			this->pelFromStorage_ = powerDischarge;
			this->thisTimeStepStateOfCharge_ = this->lastTimeStepStateOfCharge_ - powerDischarge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->energeticEfficDischarge_;
			this->thisTimeStepStateOfCharge_ = max( this->thisTimeStepStateOfCharge_, 0.0 );
		}

		// updates and reports
		this->electEnergyinStorage_ = this->thisTimeStepStateOfCharge_; //[J]
		this->storedPower          = this->pelIntoStorage_;
		this->storedEnergy         = this->pelIntoStorage_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->decrementedEnergyStored = -1.0 * this->storedEnergy;
		this->drawnPower           = this->pelFromStorage_;
		this->drawnEnergy          = this->pelFromStorage_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->thermLossRate_        = max( this->storedPower * ( 1.0 - this->energeticEfficCharge_ ), this->drawnPower * ( 1.0 - this->energeticEfficDischarge_ ) );
		this->thermLossEnergy_      = this->thermLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		if ( this->zoneNum_ > 0 ) { // set values for zone heat gains
			this->qdotConvZone_ = ( 1.0 - this->zoneRadFract_ ) * this->thermLossRate_;
			this->qdotRadZone_  = ( this->zoneRadFract_ ) * this->thermLossRate_;
		}

	}

	void
	ElectricStorage::simulateKineticBatteryModel(
		Real64 & powerCharge,
		Real64 & powerDischarge,
		bool & charging,
		bool & discharging,
		Real64 const controlSOCMaxFracLimit,
		Real64 const controlSOCMinFracLimit
	)
	{

			//initialize locals
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
		Real64 E0d = 0.0;

		qmax = this->maxAhCapacity_;
		E0c = this->chargedOCV_;
		E0d = this->dischargedOCV_;
		k = this->chargeConversionRate_;
		c = this->availableFrac_;

		if ( charging ) {

			//*************************************************
			//The sign of power and current is negative in charging
			//*************************************************
			Real64 Pw = -powerCharge / this->numBattery_;
			q0 = this->lastTimeStepAvailable_ + this->lastTimeStepBound_;
			if ( q0 > qmax * controlSOCMaxFracLimit ) {
				//stop charging with controller signal for max state of charge
				Pw = 0.0;
				powerCharge = 0.0;
				charging = false;
				this->storageMode_ = 0;
				this->storedPower = 0.0;
				this->storedEnergy =  0.0;
				this->decrementedEnergyStored = 0.0;
				this->drawnPower = 0.0;
				this->drawnEnergy = 0.0;
				return;
			}

			I0 = 1.0; // Initial assumption
			T0 = std::abs( qmax / I0 ); // Initial Assumption
			qmaxf = qmax * k * c * T0 / ( 1.0 - std::exp( -k * T0 ) + c * ( k * T0 - 1.0 + std::exp( -k * T0 ) ) ); //Initial calculation of a function qmax(I)
			Real64 Xf = q0 / qmaxf;
			Ef = E0d + CurveManager::CurveValue( this->chargeCurveNum_, Xf ); //E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
			Volt = Ef - I0 * this->internalR_;
			Real64 Inew = 0.0;
			if ( Volt != 0.0 ) {
				Inew = Pw / Volt;
			} 
			Real64 Tnew = 0.0;
			if ( Inew != 0.0 ) {
				Tnew = qmaxf / std::abs( Inew );
			}
			Real64 error = 1.0;

			while ( error > 0.0001 ) { //Iteration process to get converged current(I)
				I0 = Inew;
				T0 = Tnew;
				qmaxf = qmax * k * c * T0 / ( 1.0 - std::exp( -k * T0 ) + c * ( k * T0 - 1.0 + std::exp( -k * T0 ) ) );
				Xf = q0 / qmaxf;
				Ef = E0d + CurveManager::CurveValue( this->chargeCurveNum_, Xf ); //E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
				Volt = Ef - I0 * this->internalR_;
				Inew = Pw / Volt;
				Tnew = std::abs( qmaxf / Inew ); // ***Always positive here
				error = std::abs( Inew - I0 );
			}

			Real64 dividend = -k * c * qmax + k * this->lastTimeStepAvailable_ * std::exp( -k * DataHVACGlobals::TimeStepSys ) + q0 * k * c * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) );
			Real64 divisor = 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) + c * ( k * DataHVACGlobals::TimeStepSys - 1 + std::exp( -k * DataHVACGlobals::TimeStepSys ) );
			Real64 Imax = dividend / divisor;
			// Below: This is the limit of charging current from Charge Rate Limit (input)
			Imax = max( Imax, - ( qmax - q0 ) * this->maxChargeRate_ );
				
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
		} 

		if ( discharging ) {
			//**********************************************
			//The sign of power and current is positive in discharging
			//**********************************************

			Real64 Pw = powerDischarge / this->numBattery_;
			q0 = this->lastTimeStepAvailable_ + this->lastTimeStepBound_;

			if ( q0 < qmax * controlSOCMinFracLimit ) {
				// stop discharging with controller signal for min state of charge
				Pw = 0.0;
				discharging = false;
				powerDischarge = 0.0;
				this->storageMode_ = 0;
				this->storedPower = 0.0;
				this->storedEnergy =  0.0;
				this->decrementedEnergyStored = 0.0;
				this->drawnPower = 0.0;
				this->drawnEnergy = 0.0;
				return;

			}

			bool const ok = this->determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, this->dischargeCurveNum_, k, c, qmax, E0c, this->internalR_ );
			if ( !ok ) {
				ShowFatalError( "ElectricLoadCenter:Storage:Battery named=\"" + this->name + "\". Battery discharge current could not be estimated due to iteration limit reached. " );
				//issue #5301, need more diagnostics for this. 
			}

			Real64 dividend = k * this->lastTimeStepAvailable_ * std::exp( -k * DataHVACGlobals::TimeStepSys ) + q0 * k * c * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) );
			Real64 divisor = 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) + c * ( k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp( -k * DataHVACGlobals::TimeStepSys ) );
			Real64 Imax = dividend / divisor;
			Imax = min( Imax, this->maxDischargeI_ );
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
				Ef = E0c + CurveManager::CurveValue( this->dischargeCurveNum_, Xf );
				Volt = Ef - I0 * this->internalR_;
			}

			if ( Volt < this->cutoffV_ ) {
				I0 = 0.0;
			}

		} // if discharging

		if ( ( ! charging ) && ( ! discharging ) ) {
			this->thisTimeStepAvailable_ = this->lastTimeStepAvailable_;
			this->thisTimeStepBound_ = this->lastTimeStepBound_;
			I0 = 0.0;
			Volt = 0.0;
			q0 = this->lastTimeStepAvailable_ + this->lastTimeStepBound_;
		} else {
			Real64 newAvailable = this->lastTimeStepAvailable_ * std::exp( -k * DataHVACGlobals::TimeStepSys ) + ( q0 * k * c - I0 ) * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) ) / k - I0 * c * ( k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp( -k * DataHVACGlobals::TimeStepSys ) ) / k;
			Real64 newBound = this->lastTimeStepBound_ * std::exp( -k * DataHVACGlobals::TimeStepSys ) + q0 * ( 1.0 - c ) * ( 1.0 - std::exp( -k * DataHVACGlobals::TimeStepSys ) ) - I0 * ( 1.0 - c ) * ( k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp( -k * DataHVACGlobals::TimeStepSys ) ) / k;
			this->thisTimeStepAvailable_ = max( 0.0, newAvailable );
			this->thisTimeStepBound_ = max( 0.0, newBound );
		}

		Pactual = I0 * Volt;
		Real64 TotalSOC = this->thisTimeStepAvailable_ + this->thisTimeStepBound_;

		//output1
		if ( TotalSOC > q0 ) {
			this->storageMode_ = 2;
			this->storedPower = -1.0 *Volt * I0 * this->numBattery_; //Issue #5303, fix sign issue
			this->storedEnergy =  -1.0 *Volt * I0 * this->numBattery_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
			this->decrementedEnergyStored = -1.0 * this->storedEnergy;
			this->drawnPower = 0.0;
			this->drawnEnergy = 0.0;

		} else if ( TotalSOC < q0 ) {
			this->storageMode_ = 1;
			this->storedPower = 0.0;
			this->storedEnergy = 0.0;
			this->decrementedEnergyStored = 0.0;
			this->drawnPower = Volt * I0 * this->numBattery_;
			this->drawnEnergy = Volt * I0 * this->numBattery_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		} else {
			this->storageMode_ = 0;
			this->storedPower = 0.0;
			this->storedEnergy = 0.0;
			this->decrementedEnergyStored = 0.0;
			this->drawnPower = 0.0;
			this->drawnEnergy = 0.0;
		}

		this->absoluteSOC_ = TotalSOC * this->numBattery_;
		this->fractionSOC_ = TotalSOC / qmax;
		this->batteryCurrent_ = I0 * this->parallelNum_;
		this->batteryVoltage_ = Volt * this->seriesNum_;
		this->thermLossRate_ = this->internalR_ * pow_2( I0 ) * this->numBattery_;
		this->thermLossEnergy_ = this->internalR_ * pow_2( I0 ) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->numBattery_;

		if ( this->zoneNum_ > 0 ) { // set values for zone heat gains
			this->qdotConvZone_ = ( ( 1.0 - this->zoneRadFract_ ) * this->thermLossRate_ ) * this->numBattery_;
			this->qdotRadZone_ = ( ( this->zoneRadFract_ ) * this->thermLossRate_ ) * this->numBattery_;
		}

		powerCharge = this->storedPower;
		powerDischarge = this->drawnPower;


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
		Real64 const Pw, // Power withdraw from each module, 
		Real64 const q0, // available charge last timestep, sum of available and bound 
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
		//add div by zero protection #5301
			if ( qmaxf != 0.0 ) {
				Xf = ( qmax - q0 ) / qmaxf;
			} else { 
				Xf = 1.0;
			} 

			
			Ef = E0c + CurveManager::CurveValue( CurveNum, Xf ); //E0c+Ad*Xf+Cd*X/(Dd-Xf)
			curVolt = Ef - curI0 * InternalR;
		//add div by zero protection #5301
			if ( curVolt != 0.0 ) {
				Inew = Pw / curVolt;
			} else {
				Inew = 1.0;
			}
			
		// add div by zero protection #5301
			if ( Inew != 0.0 ) {
				Tnew = qmaxf / Inew;
			} else {
				Tnew = 1.0;
			}
			
			error = std::abs( Inew - curI0 );
			++countForIteration;
			if ( countForIteration > 1000 ) {
				exceedIterationLimit = true;
				//Issue #5301 need more diagnostics for this case
				ShowWarningError( "ElectricStorage::determineCurrentForBatteryDischarge, iteration limit exceeded, failed to solve for discharge current." );
				ShowContinueError( "Last timestep charge available, q0 = " + General::RoundSigDigits( q0, 5 ) );
				ShowContinueError( "New Current, Inew = " + General::RoundSigDigits( Inew, 5 ) + " [Amps]"  );
				ShowContinueError( "Power discharge per module cell, Pw = " +General::RoundSigDigits( Pw, 5) +  " ");
				ShowContinueError( "Charge Conversion Rate, [1/h] change rate from bound charge energy to available charge, parameter k = " + General::RoundSigDigits( k, 5) );
				ShowContinueError( "parameter c = " + General::RoundSigDigits( c, 5) );
				ShowContinueError( "parameter qmax = " + General::RoundSigDigits( qmax, 5) );
				ShowContinueError( "Fully charged open circuit voltage, parameter E0c  = " + General::RoundSigDigits( E0c, 5) );
				ShowContinueError( "parameter InternalR = " + General::RoundSigDigits( InternalR, 5) );
				if ( qmaxf == 0.0 ) {
					ShowContinueError( "qmaxf was zero, would have divided by zero.");
				}
				if ( Inew == 0.0 ) {
					ShowContinueError( "Inew was zero, would have divided by zero. ");
				}
				if ( curVolt == 0.0 ) {
					ShowContinueError("curVolt was zero, would have divided by zero. ");
				}

				ShowContinueErrorTimeStamp( "ElectricStorage::determineCurrentForBatteryDischarge " );
				break;
			}
		}
		return (!exceedIterationLimit);

	}

	void
	ElectricStorage::rainflow(
		int const numbin, // numbin = constant value
		Real64 const input, // input = input value from other object (battery model)
		std::vector < Real64 > & B1, // stores values of points, calculated here - stored for next timestep
		std::vector < Real64 > & X, // stores values of two data point difference, calculated here - stored for next timestep
		int & count, // calculated here - stored for next timestep in main loop
		std::vector < Real64 > & Nmb, // calculated here - stored for next timestep in main loop
		std::vector < Real64 > & OneNmb // calculated here - stored for next timestep in main loop
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
		std::vector < Real64 > & A,
		int const m,
		int const n,
		std::vector < Real64 > & B
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
	):
			myOneTimeFlag_( true ),
			availSchedPtr_( 0 ),
			usageMode_( TransformerUse::usenotYetSet ),
			heatLossesDestination_( ThermalLossDestination::heatLossNotDetermined ),
			zoneNum_( 0 ),
			zoneRadFrac_( 0.0 ),
			ratedCapacity_( 0.0 ),
			phase_( 0 ),
			factorTempCoeff_( 0.0 ),
			tempRise_( 0.0 ),
			eddyFrac_( 0.0 ),
			performanceInputMode_( TransformerPerformanceInput::perfInputMethodNotSet ),
			ratedEfficiency_( 0.0 ),
			ratedPUL_( 0.0 ),
			ratedTemp_( 0.0 ),
			maxPUL_( 0.0 ),
			considerLosses_( true ),
			ratedNL_( 0.0 ),
			ratedLL_( 0.0 ),
			overloadErrorIndex_( 0 ),
			efficiency_( 0.0 ),
			powerIn_( 0.0 ),
			energyIn_( 0.0 ),
			powerOut_( 0.0 ),
			energyOut_( 0.0 ),
			noLoadLossRate_( 0.0 ),
			noLoadLossEnergy_( 0.0 ),
			loadLossRate_( 0.0 ),
			loadLossEnergy_( 0.0 ),
			thermalLossRate_( 0.0 ),
			thermalLossEnergy_( 0.0 ),
			elecUseMeteredUtilityLosses_( 0.0 ),
			powerConversionMeteredLosses_( 0.0 ),
			qdotConvZone_( 0.0 ),
			qdotRadZone_( 0.0 )
	{
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
			this->name_  = DataIPShortCuts::cAlphaArgs( 1 );
			// how to verify names are unique across objects? add to GlobalNames?
			if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
				this->availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
			} else {
				this->availSchedPtr_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
				if ( this->availSchedPtr_ == 0 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
					errorsFound = true;
				}
			}

			if ( DataIPShortCuts::lAlphaFieldBlanks( 3 ) ) {
				this->usageMode_ = TransformerUse::powerInFromGrid; //default
			} else if ( InputProcessor::SameString(DataIPShortCuts::cAlphaArgs( 3 ), "PowerInFromGrid" ) ) {
				this->usageMode_ = TransformerUse::powerInFromGrid;
			} else if ( InputProcessor::SameString(DataIPShortCuts::cAlphaArgs( 3 ), "PowerOutToGrid" ) ) {
				this->usageMode_ = TransformerUse::powerOutFromBldgToGrid;
			} else if ( InputProcessor::SameString(DataIPShortCuts::cAlphaArgs( 3 ), "LoadCenterPowerConditioning" ) ) {
				this->usageMode_ = TransformerUse::powerBetweenLoadCenterAndBldg;

			} else {
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 3 ) + " = " + DataIPShortCuts::cAlphaArgs( 3 ) );
					errorsFound = true;
			}

			this->zoneNum_ = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 4 ), DataHeatBalance::Zone );
			if ( this->zoneNum_ > 0 ) this->heatLossesDestination_ = ThermalLossDestination::zoneGains;
			if ( this->zoneNum_ == 0 ) {
				if ( DataIPShortCuts::lAlphaFieldBlanks( 4 ) ) {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
				} else {
					this->heatLossesDestination_ = ThermalLossDestination::lostToOutside;
					ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 4 ) + " = " + DataIPShortCuts::cAlphaArgs( 4 ) );
					ShowContinueError( "Zone name not found. Transformer heat losses will not be added to a zone" );
					// continue with simulation but storage losses not sent to a zone.
				}
			}
			this->zoneRadFrac_   = DataIPShortCuts::rNumericArgs( 1 );
			this->ratedCapacity_ = DataIPShortCuts::rNumericArgs( 2 );
			this->phase_         = DataIPShortCuts::rNumericArgs( 3 );

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 5 ), "Copper" ) ) {
				this->factorTempCoeff_ = 234.5;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 5 ), "Aluminum" ) ) {
				this->factorTempCoeff_ = 225.0;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + " = " + DataIPShortCuts::cAlphaArgs( 5 ) );
				errorsFound = true;
			}
			this->tempRise_ = DataIPShortCuts::rNumericArgs( 4 );
			this->eddyFrac_ = DataIPShortCuts::rNumericArgs( 5 );

			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "RatedLosses" ) ) {
				this->performanceInputMode_ = TransformerPerformanceInput::lossesMethod;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "NominalEfficiency" ) ) {
				this->performanceInputMode_ = TransformerPerformanceInput::efficiencyMethod;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" +  DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 6 ) + " = " + DataIPShortCuts::cAlphaArgs( 6 ) );
				errorsFound = true;
			}

			this->ratedNL_         = DataIPShortCuts::rNumericArgs( 6 );
			this->ratedLL_         = DataIPShortCuts::rNumericArgs( 7 );
			this->ratedEfficiency_ = DataIPShortCuts::rNumericArgs( 8 );
			this->ratedPUL_        = DataIPShortCuts::rNumericArgs( 9 );
			this->ratedTemp_       = DataIPShortCuts::rNumericArgs( 10 );
			this->maxPUL_          = DataIPShortCuts::rNumericArgs( 11 );
			//Check the input for MaxPUL if the performance input method is EfficiencyMethod
			if ( this->performanceInputMode_ == TransformerPerformanceInput::efficiencyMethod ) {
				if ( DataIPShortCuts::lNumericFieldBlanks( 11 ) ) {
					this->maxPUL_ = this->ratedPUL_;
				} else if ( this->maxPUL_ <= 0 || this->maxPUL_ > 1 ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cNumericFieldNames( 11 ) + "=[" + General::RoundSigDigits( DataIPShortCuts::rNumericArgs( 11 ), 3 ) + "]." );
					ShowContinueError( "Entered value must be > 0 and <= 1." );
					errorsFound = true;
				}
			}
			if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 7 ), "Yes" ) ) {
				this->considerLosses_ = true;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 7 ), "No" ) ) {
				this->considerLosses_ = false;
			} else {
				if ( this->usageMode_ == TransformerUse::powerInFromGrid ) {
					ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 7 ) + " = " + DataIPShortCuts::cAlphaArgs( 7 ) );
					errorsFound = true;
				}
			}

			int numAlphaBeforeMeter = 7;
			int numWiredMeters = numAlphas - numAlphaBeforeMeter;

			if ( this->usageMode_ == TransformerUse::powerInFromGrid ) {

				//Provide warning if no meter is wired to a transformer used to get power from the grid
				if ( numWiredMeters <= 0 ) {
					ShowWarningError( routineName + "ElectricLoadCenter:Transformer=\"" + this->name_ + "\":" );
					ShowContinueError( "ISOLATED Transformer: No meter wired to a transformer used to input power from grid" );
				}

				this->wiredMeterNames_.resize( numWiredMeters, "" );
				this->wiredMeterPtrs_.resize( numWiredMeters, 0 );
				this->specialMeter_.resize( numWiredMeters, false );

				//Meter check deferred because they may have not been "loaded" yet,
				for ( auto loopCount = 0; loopCount < numWiredMeters; ++loopCount ) {
					this->wiredMeterNames_[ loopCount ] = InputProcessor::MakeUPPERCase( DataIPShortCuts::cAlphaArgs( loopCount + numAlphaBeforeMeter + 1 ) );
					//Assign SpecialMeter as TRUE if the meter name is Electricity:Facility or Electricity:HVAC
					if ( InputProcessor::SameString( this->wiredMeterNames_[ loopCount ], "Electricity:Facility" ) || InputProcessor::SameString( this->wiredMeterNames_[ loopCount ], "Electricity:HVAC" ) ) {
						this->specialMeter_[ loopCount ] = true;
					} else {
						this->specialMeter_[ loopCount ] = false;
					}
				}
			}
			SetupOutputVariable( "Transformer Efficiency []", this->efficiency_, "System", "Average", this->name_ );
			SetupOutputVariable( "Transformer Input Electric Power [W]", this->powerIn_, "System", "Average", this->name_ );
			SetupOutputVariable( "Transformer Input Electric Energy [J]", this->energyIn_, "System", "Sum", this->name_ );
			SetupOutputVariable( "Transformer Output Electric Power [W]", this->powerOut_, "System", "Average", this->name_ );
			SetupOutputVariable( "Transformer Output Electric Energy [J]", this->energyOut_, "System", "Sum", this->name_ );
			SetupOutputVariable( "Transformer No Load Loss Rate [W]", this->noLoadLossRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Transformer No Load Loss Energy [J]", this->noLoadLossEnergy_, "System", "Sum", this->name_ );
			SetupOutputVariable( "Transformer Load Loss Rate [W]", this->loadLossRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Transformer Load Loss Energy [J]", this->loadLossEnergy_, "System", "Sum", this->name_ );
			SetupOutputVariable( "Transformer Thermal Loss Rate [W]", this->thermalLossRate_, "System", "Average", this->name_ );
			SetupOutputVariable( "Transformer Thermal Loss Energy [J]", this->thermalLossEnergy_, "System", "Sum", this->name_ );
			if ( this->usageMode_ == TransformerUse::powerInFromGrid ) { // power losses metered as an end use exterior equipment
				SetupOutputVariable( "Transformer Distribution Electric Loss Energy [J]", this->elecUseMeteredUtilityLosses_, "System", "Sum", this->name_, _, "Electricity", "ExteriorEquipment", "Transformer", "System" );
			}
			if ( this->usageMode_ == TransformerUse::powerOutFromBldgToGrid ) { 
				SetupOutputVariable( "Transformer Cogeneration Electric Loss Energy [J]", this->powerConversionMeteredLosses_, "System", "Sum", this->name_, _, "ElectricityProduced", "POWERCONVERSION", _, "System" );
			}
			if ( this->usageMode_ == TransformerUse::powerBetweenLoadCenterAndBldg ) {
				SetupOutputVariable( "Transformer Conversion Electric Loss Energy [J]", this->powerConversionMeteredLosses_, "System", "Sum", this->name_, _, "ElectricityProduced", "POWERCONVERSION", _, "System" );
			}


			if ( this->zoneNum_ > 0 ) {
				SetupZoneInternalGain( this->zoneNum_, "ElectricLoadCenter:Transformer", this->name_, DataHeatBalance::IntGainTypeOf_ElectricLoadCenterTransformer, this->qdotConvZone_, _, this->qdotRadZone_ );
			}

		} else {
			ShowSevereError( routineName + " did not find transformer name = " + objectName);
			errorsFound = true;
		}

		if ( errorsFound ) {
			ShowFatalError( routineName + "Preceding errors terminate program." );
		}
	}

	Real64
	ElectricTransformer::getLossRateForOutputPower(
		Real64 const powerOutOfTransformer
	) 
	{
		// TODO run model with power out level arg
		// use lagged value for now
		this->manageTransformers( powerOutOfTransformer );

		return this->totalLossRate_;

	}

	Real64
	ElectricTransformer::getLossRateForInputPower(
		Real64 const powerIntoTransformer
	)
	{
		// TODO run model with power in level arg
		//use lagged value for now.
		this->manageTransformers( powerIntoTransformer );
		return this->totalLossRate_;
	}

	void
	ElectricTransformer::manageTransformers(
		Real64 const surplusPowerOutFromLoadCenters
	)
	{
		Real64 const ambTempRef = 20.0; // reference ambient temperature (C)
		if ( this->myOneTimeFlag_ ) {
			// calculate rated no load losses and rated load losses if the performance input method is based on
			// nominal efficiency. This calculation is done only once

			if ( this->performanceInputMode_ == TransformerPerformanceInput::efficiencyMethod ) {

				Real64 resRef = this->factorTempCoeff_ + this->tempRise_ + ambTempRef;
				Real64 resSpecified = this->factorTempCoeff_ + this->ratedTemp_;
				Real64 resRatio = resSpecified / resRef;
				Real64 factorTempCorr = ( 1.0 - this->eddyFrac_ ) * resRatio + this->eddyFrac_ * ( 1.0 / resRatio );
				Real64 numerator = this->ratedCapacity_ * this->ratedPUL_ * ( 1.0 - this->ratedEfficiency_ );
				Real64 denominator = this->ratedEfficiency_ * ( 1.0 + pow_2( this->ratedPUL_ / this->maxPUL_ ) );

				this->ratedNL_ = numerator / denominator;
				this->ratedLL_ = this->ratedNL_ / ( factorTempCorr * pow_2( this->maxPUL_ ) );
			}
			this->myOneTimeFlag_ = false;
		}

		Real64 elecLoad = 0.0; // transformer load which may be power in or out depending on the usage mode
		Real64 pastElecLoad = 0.0; // transformer load at the previous timestep
		switch ( this->usageMode_ )
		{
		case TransformerUse::powerInFromGrid: {
			for ( std::size_t meterNum = 0; meterNum < this->wiredMeterPtrs_.size(); ++meterNum ) {

				if ( DataGlobals::MetersHaveBeenInitialized ) {

					elecLoad += GetInstantMeterValue( this->wiredMeterPtrs_[ meterNum ], 1 ) / DataGlobals::TimeStepZoneSec + GetInstantMeterValue( this->wiredMeterPtrs_[ meterNum ], 2 ) / ( DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour );
					// PastElecLoad store the metered value in the previous time step. This value will be used to check whether
					// a transformer is overloaded or not.
					pastElecLoad += GetCurrentMeterValue( this->wiredMeterPtrs_[ meterNum ] ) / DataGlobals::TimeStepZoneSec;
				} else {
					elecLoad = 0.0;
					pastElecLoad = 0.0;
				}

				// Because transformer loss has been accounted for by Electricity:Facility and Electricity:HVAC, the transformer
				// loss needs to be deducted from the metered value. Otherwise, double counting (circular relationship) occurs.
				if ( this->specialMeter_[ meterNum ] ) {
					elecLoad = elecLoad - this->loadLossRate_ - this->noLoadLossRate_;

					if ( elecLoad < 0 ) elecLoad = 0.0; //Essential check.
				}
			}

			this->powerOut_ = elecLoad; //the metered value is transformer's output in PowerInFromGrid mode
			break;
		}
		case TransformerUse::powerOutFromBldgToGrid : {
			this->powerIn_ = surplusPowerOutFromLoadCenters;
			elecLoad = surplusPowerOutFromLoadCenters; // TODO this is input but should be output with the losses, but we don't have them yet. 
			break;
		}
		case TransformerUse::powerBetweenLoadCenterAndBldg : {
			//TODO, new configuration for transformer, really part of the specific load center and connects it to the main building bus
			this->powerIn_ = surplusPowerOutFromLoadCenters;
			elecLoad = surplusPowerOutFromLoadCenters;
			break;
		}
		case TransformerUse::usenotYetSet : {
			// do nothing
			break;
		}
		} // switch usage mode

		// check availability schedule
		if ( ScheduleManager::GetCurrentScheduleValue( this->availSchedPtr_ ) > 0.0 ) {

			Real64 pUL = elecLoad / this->ratedCapacity_;

			if ( pUL > 1.0 ) {
				pUL = 1.0;
			}

			//Originally, PUL was used to check whether a transformer is overloaded (PUL > 1.0 or not). However, it was
			//found that ElecLoad obtained from GetInstantMeterVlaue() might refer to intermideiate values before
			//convergence. The intermediate values may issue false warning. This the reason why PastElecLoad obtained
			//by GetCurrentMeterValue() is used here to check overload issue.
			if ( ( pastElecLoad / this->ratedCapacity_ ) > 1.0 ) {
				if ( this->overloadErrorIndex_ == 0 ) {
					ShowSevereError( "Transformer Overloaded" );
					ShowContinueError( "Entered in ElectricLoadCenter:Transformer =" + this->name_ );
				}
				ShowRecurringSevereErrorAtEnd( "Transformer Overloaded: Entered in ElectricLoadCenter:Transformer =" + this->name_, this->overloadErrorIndex_ );
			}

			Real64 tempChange = std::pow( pUL, 1.6 ) * this->tempRise_;
			Real64 ambTemp = 20.0;
			if ( this->heatLossesDestination_ == ThermalLossDestination::zoneGains ) {

				ambTemp = DataHeatBalance::ZnAirRpt( this->zoneNum_ ).MeanAirTemp;
			} else {
				ambTemp = 20.0;
			}

			Real64 resRef = this->factorTempCoeff_ + this->tempRise_ + ambTempRef;
			Real64 resSpecified = this->factorTempCoeff_ + tempChange + ambTemp;
			Real64 resRatio = resSpecified / resRef;
			Real64 factorTempCorr = ( 1.0 - this->eddyFrac_ ) * resRatio + this->eddyFrac_ * ( 1.0 / resRatio );

			this->loadLossRate_ = this->ratedLL_ * pow_2( pUL ) * factorTempCorr;
			this->noLoadLossRate_ = this->ratedNL_;
		} else { //Transformer is not available.
			this->loadLossRate_ = 0.0;
			this->noLoadLossRate_ = 0.0;
		}

		this->totalLossRate_ = this->loadLossRate_ + this->noLoadLossRate_;

		switch ( this->usageMode_ )
		{
		case TransformerUse::powerInFromGrid: {
			this->powerIn_ = elecLoad + this->totalLossRate_;

			//Transformer losses are wired to the meter via the variable "%ElecUseUtility" only if transformer losses
			//are considered in utility cost. If transformer losses are not considered in utility cost, 0 is assigned
			//to the variable "%ElecUseUtility".
			if ( this->considerLosses_ ) {
				this->elecUseMeteredUtilityLosses_ = this->totalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
			} else {
				this->elecUseMeteredUtilityLosses_ = 0.0;
			}

			//Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
			//is assigned 0
			this->totalLossEnergy_ = this->totalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

			break;
		}

		case TransformerUse::powerOutFromBldgToGrid: 
		case TransformerUse::powerBetweenLoadCenterAndBldg : {
			this->powerOut_ = elecLoad - this->totalLossRate_;

			if ( this->powerOut_ < 0 ) this->powerOut_ = 0.0;

			this->powerConversionMeteredLosses_ = -1.0 * this->totalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

			//Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
			//is assigned 0
			this->elecUseMeteredUtilityLosses_ = 0.0;
			break;
		}

		case TransformerUse::usenotYetSet : {
			// do nothing
			assert( false );
		}
		} // switch

		if ( this->powerIn_ <= 0 ) {
			this->efficiency_ = 0.0;
		} else {
			this->efficiency_ = this->powerOut_ / this->powerIn_;
		}
		this->noLoadLossEnergy_ = this->noLoadLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->loadLossEnergy_ = this->loadLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		this->energyIn_ = this->powerIn_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->energyOut_ = this->powerOut_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//   Thermal loss rate may not be equal to Total loss rate. This is the case when surplus power is less than the
		//    calculated total loss rate for a cogeneration transformer. That is why "PowerIn - PowerOut" is used below.
		this->thermalLossRate_ = this->powerIn_ - this->powerOut_;
		this->thermalLossEnergy_ = this->thermalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		if ( this->zoneNum_ > 0 ) { // set values for zone heat gains
			this->qdotConvZone_ = ( 1.0 - this->zoneRadFrac_ ) * this->thermalLossRate_;
			this->qdotRadZone_ = ( this->zoneRadFrac_ ) * this->thermalLossRate_;
		}

	}

	void
	ElectricTransformer::setupMeterIndices()
	{
		if (this->usageMode_ == TransformerUse::powerInFromGrid ) {
			for ( std::size_t meterNum = 0; meterNum < this->wiredMeterNames_.size(); ++meterNum ) {

				this->wiredMeterPtrs_[ meterNum ] = GetMeterIndex( this->wiredMeterNames_[ meterNum ] );

				//Check whether the meter is an electricity meter
				//Index function is used here because some resource types are not Electricity but strings containing
				// Electricity such as ElectricityPurchased and ElectricityProduced.
				//It is not proper to have this check in GetInput routine because the meter index may have not been defined
				if ( ! has( GetMeterResourceType( this->wiredMeterPtrs_[ meterNum ] ), "Electricity" ) ) {
					EnergyPlus::ShowFatalError( "Non-electricity meter used for " + this->name_ );
				}
			}
		}
	}

	void
	ElectricTransformer::reinitAtBeginEnvironment()
	{
		this->efficiency_        = 0.0;
		this->powerIn_           = 0.0;
		this->energyIn_          = 0.0;
		this->powerOut_          = 0.0;
		this->energyOut_         = 0.0;
		this->noLoadLossRate_    = 0.0;
		this->noLoadLossEnergy_  = 0.0;
		this->loadLossRate_      = 0.0;
		this->loadLossEnergy_    = 0.0;
		this->thermalLossRate_   = 0.0;
		this->thermalLossEnergy_ = 0.0;
		this->elecUseMeteredUtilityLosses_    = 0.0;
		this->powerConversionMeteredLosses_  = 0.0;
		this->qdotConvZone_      = 0.0;
		this->qdotRadZone_       = 0.0;
	}

	void
	ElectricTransformer::reinitZoneGainsAtBeginEnvironment()
	{
		this->qdotConvZone_      = 0.0;
		this->qdotRadZone_       = 0.0;
	}

} // EnergyPlus namespace
