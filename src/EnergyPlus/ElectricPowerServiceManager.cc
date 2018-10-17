// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// C++ Headers
#include <memory>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1.hh>

// EnergyPlus Headers
#include <CTElectricGenerator.hh>
#include <CurveManager.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <EMSManager.hh>
#include <ElectricPowerServiceManager.hh>
#include <FuelCellElectricGenerator.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <ICEngineElectricGenerator.hh>
#include <InputProcessing/InputProcessor.hh>
#include <MicroCHPElectricGenerator.hh>
#include <MicroturbineElectricGenerator.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PVWatts.hh>
#include <Photovoltaics.hh>
#include <Plant/PlantLocation.hh>
#include <PlantUtilities.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WindTurbine.hh>

namespace EnergyPlus {

std::unique_ptr<ElectricPowerServiceManager> facilityElectricServiceObj;

void clearFacilityElectricPowerServiceObject()
{
    facilityElectricServiceObj.release();
}

void createFacilityElectricPowerServiceObject()
{
    facilityElectricServiceObj = std::unique_ptr<ElectricPowerServiceManager>(new ElectricPowerServiceManager());
}

void initializeElectricPowerServiceZoneGains() // namespace routine for handling call from InternalHeatGains
{
    // internal zone gains need to be re initialized for begin new environment earlier than the main call into manage electric power service
    if (facilityElectricServiceObj->newEnvironmentInternalGainsFlag && DataGlobals::BeginEnvrnFlag) {
        facilityElectricServiceObj->reinitZoneGainsAtBeginEnvironment();
        facilityElectricServiceObj->newEnvironmentInternalGainsFlag = false;
    }
    if (!DataGlobals::BeginEnvrnFlag) {
        facilityElectricServiceObj->newEnvironmentInternalGainsFlag = true;
    }
}

void ElectricPowerServiceManager::manageElectricPowerService(
    bool const firstHVACIteration,
    bool &SimElecCircuits,      // simulation convergence flag
    bool const UpdateMetersOnly // if true then don't resimulate generators, just update meters.
)
{
    if (getInputFlag_) {
        getPowerManagerInput();
        getInputFlag_ = false;
    }

    if (DataGlobals::MetersHaveBeenInitialized && setupMeterIndexFlag_) {
        setupMeterIndices();
        setupMeterIndexFlag_ = false;
    }

    if (DataGlobals::BeginEnvrnFlag && newEnvironmentFlag_) {
        reinitAtBeginEnvironment();
        newEnvironmentFlag_ = false;
    }
    if (!DataGlobals::BeginEnvrnFlag) newEnvironmentFlag_ = true;

    // retrieve data from meters for demand and production
    totalBldgElecDemand_ = GetInstantMeterValue(elecFacilityIndex_, 1) / DataGlobals::TimeStepZoneSec;
    totalHVACElecDemand_ = GetInstantMeterValue(elecFacilityIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    totalElectricDemand_ = totalBldgElecDemand_ + totalHVACElecDemand_;
    elecProducedPVRate_ = GetInstantMeterValue(elecProducedPVIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedWTRate_ = GetInstantMeterValue(elecProducedWTIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedStorageRate_ = GetInstantMeterValue(elecProducedStorageIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedCoGenRate_ = GetInstantMeterValue(elecProducedCoGenIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedPowerConversionRate_ =
        GetInstantMeterValue(elecProducedPowerConversionIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);

    wholeBldgRemainingLoad_ = totalElectricDemand_;

    if (UpdateMetersOnly) { // just update record keeping, don't resimulate load centers
        if (facilityPowerInTransformerPresent_) {
            facilityPowerInTransformerObj_->manageTransformers(0.0);
        }

        updateWholeBuildingRecords();
        return;
    }

    for (auto &e : elecLoadCenterObjs) {
        e->manageElecLoadCenter(firstHVACIteration, wholeBldgRemainingLoad_);
    }

    updateWholeBuildingRecords();
    // The transformer call should be put outside of the "Load Center" loop because
    // 1) A transformer may be for utility, not for load center
    // 2) A tansformer may be shared by multiple load centers
    if (facilityPowerInTransformerPresent_) {
        facilityPowerInTransformerObj_->manageTransformers(0.0);
    }

    updateWholeBuildingRecords();
    if (powerOutTransformerObj_ != nullptr) {
        powerOutTransformerObj_->manageTransformers(electSurplusRate_);
    }

    // Need to simulate through the Elec Manager at least twice to ensure that Heat Recovery information is included.
    // recheck this, may not be needed now that load centers are called more often.
    //  Does the IF condition also need to check if any thermal following strategies have been specified?
    //  That is, if only electrical following schemes, don't need to resimulate?
    if (firstHVACIteration) {
        SimElecCircuits = true;
    } else {
        SimElecCircuits = false;
    }
}

void ElectricPowerServiceManager::reinitZoneGainsAtBeginEnvironment()
{
    if (facilityPowerInTransformerPresent_) {
        facilityPowerInTransformerObj_->reinitZoneGainsAtBeginEnvironment();
    }
    if (powerOutTransformerObj_ != nullptr) {
        powerOutTransformerObj_->reinitZoneGainsAtBeginEnvironment();
    }
    if (numLoadCenters_ > 0) {
        for (auto &e : elecLoadCenterObjs) {
            e->reinitZoneGainsAtBeginEnvironment();
        }
    }
}

void ElectricPowerServiceManager::getPowerManagerInput()
{
    std::string const routineName = "ElectricPowerServiceManager  getPowerManagerInput ";

    numLoadCenters_ = inputProcessor->getNumObjectsFound("ElectricLoadCenter:Distribution");

    if (numLoadCenters_ > 0) {
        for (auto iLoadCenterNum = 1; iLoadCenterNum <= numLoadCenters_; ++iLoadCenterNum) {
            // call Electric Power Load Center constructor, in place
            elecLoadCenterObjs.emplace_back(new ElectPowerLoadCenter(iLoadCenterNum));
        }
    } else {
        // issue #4639. see if there are any generators, inverters, converters, or storage devcies, that really need a ElectricLoadCenter:Distribution
        bool errorsFound(false);
        int numGenLists = inputProcessor->getNumObjectsFound("ElectricLoadCenter:Generators");
        if (numGenLists > 0) {
            ShowSevereError("ElectricLoadCenter:Generators input object requires an ElectricLoadCenterDistribution input object.");
            errorsFound = true;
        }
        int numInverters = inputProcessor->getNumObjectsFound("ElectricLoadCenter:Inverter:Simple");
        numInverters += inputProcessor->getNumObjectsFound("ElectricLoadCenter:Inverter:FunctionOfPower");
        numInverters += inputProcessor->getNumObjectsFound("ElectricLoadCenter:Inverter:LookUpTable");
        if (numInverters > 0) {
            ShowSevereError("ElectricLoadCenter:Inverter:* input objects require an ElectricLoadCenter:Distribution input object.");
            errorsFound = true;
        }
        int numStorage = inputProcessor->getNumObjectsFound("ElectricLoadCenter:Storage:Simple");
        numStorage += inputProcessor->getNumObjectsFound("ElectricLoadCenter:Storage:Battery");
        if (numStorage > 0) {
            ShowSevereError("ElectricLoadCenter:Storage:* input objects require an ElectricLoadCenter:Distribution input object.");
            errorsFound = true;
        }
        int numGenerators = inputProcessor->getNumObjectsFound("Generator:InternalCombustionEngine");
        numGenerators += inputProcessor->getNumObjectsFound("Generator:CombustionTurbine");
        numGenerators += inputProcessor->getNumObjectsFound("Generator:MicroCHP");
        numGenerators += inputProcessor->getNumObjectsFound("Generator:FuelCell");
        numGenerators += inputProcessor->getNumObjectsFound("Generator:Photovoltaic");
        numGenerators += inputProcessor->getNumObjectsFound("Generator:WindTurbine");
        if (numGenerators > 0) {
            ShowSevereError("Electric generator input objects require an ElectricLoadCenter:Distribution input object.");
            errorsFound = true;
        }

        if (errorsFound) {
            ShowFatalError("Simulation halted because of missing input objects related to ElectricLoadCenter.");
        }

        // if user input did not include an Electric Load center, create a simple default one here for reporting purposes
        //   but only if there are any other electricity components set up (yet) for metering
        int anyElectricityPresent = GetMeterIndex("ELECTRICITY:FACILITY");
        int anyPlantLoadProfilePresent = inputProcessor->getNumObjectsFound("LoadProfile:Plant");
        if (anyElectricityPresent > 0 || anyPlantLoadProfilePresent > 0) {
            elecLoadCenterObjs.emplace_back(new ElectPowerLoadCenter(0));
            numLoadCenters_ = 1;
        }
    }

    // see if there are any transformers of the type powerInFromGrid
    numTransformers_ = inputProcessor->getNumObjectsFound("ElectricLoadCenter:Transformer");

    if (numTransformers_ > 0) {
        int numAlphas; // Number of elements in the alpha array
        int numNums;   // Number of elements in the numeric array
        int iOStat;    // IO Status when calling get input subroutine
        int facilityPowerInTransformerIDFObjNum = 0;
        bool foundInFromGridTransformer = false;

        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Transformer";
        for (auto loopTransformer = 1; loopTransformer <= numTransformers_; ++loopTransformer) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          loopTransformer,
                                          DataIPShortCuts::cAlphaArgs,
                                          numAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          numNums,
                                          iOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);

            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "PowerInFromGrid")) {
                if (!foundInFromGridTransformer) {
                    foundInFromGridTransformer = true;
                    facilityPowerInTransformerIDFObjNum = loopTransformer;
                    facilityPowerInTransformerName_ = DataIPShortCuts::cAlphaArgs(1);
                    facilityPowerInTransformerPresent_ = true;
                } else {
                    // should only have one transformer in input that is PowerInFromGrid
                    ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) +
                                     "\", invalid entry.");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                    ShowContinueError("Only one transformer with Usage PowerInFromGrid can be used, first one in input file will be used and the "
                                      "simulation continues...");
                }
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "PowerOutToGrid")) {
                if (powerOutTransformerObj_ == nullptr) {
                    ++numPowerOutTransformers_;
                    powerOutTransformerName_ = DataIPShortCuts::cAlphaArgs(1);
                    powerOutTransformerObj_ = std::unique_ptr<ElectricTransformer>(new ElectricTransformer(powerOutTransformerName_));

                } else {
                    ShowWarningError("Found more than one transformer set to PowerOutFromOnsiteGeneration, however only the first one will be used.");
                }
            }
        }
        if (foundInFromGridTransformer) {
            // call transformer constructor
            facilityPowerInTransformerObj_ = std::unique_ptr<ElectricTransformer>(new ElectricTransformer(facilityPowerInTransformerName_));
        }
    } // if transformers

    if (numLoadCenters_ > 0) {
        SetupOutputVariable("Facility Total Purchased Electric Power", OutputProcessor::Unit::W, electPurchRate_, "System", "Average", name_);
        SetupOutputVariable("Facility Total Purchased Electric Energy",
                            OutputProcessor::Unit::J,
                            electricityPurch_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricityPurchased",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable("Facility Total Surplus Electric Power", OutputProcessor::Unit::W, electSurplusRate_, "System", "Average", name_);
        SetupOutputVariable("Facility Total Surplus Electric Energy",
                            OutputProcessor::Unit::J,
                            electricitySurplus_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricitySurplusSold",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable("Facility Net Purchased Electric Power", OutputProcessor::Unit::W, electricityNetRate_, "System", "Average", name_);
        SetupOutputVariable("Facility Net Purchased Electric Energy",
                            OutputProcessor::Unit::J,
                            electricityNet_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricityNet",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Facility Total Building Electric Demand Power", OutputProcessor::Unit::W, totalBldgElecDemand_, "System", "Average", name_);
        SetupOutputVariable("Facility Total HVAC Electric Demand Power", OutputProcessor::Unit::W, totalHVACElecDemand_, "System", "Average", name_);
        SetupOutputVariable("Facility Total Electric Demand Power", OutputProcessor::Unit::W, totalElectricDemand_, "System", "Average", name_);

        SetupOutputVariable("Facility Total Produced Electric Power", OutputProcessor::Unit::W, electProdRate_, "System", "Average", name_);
        SetupOutputVariable("Facility Total Produced Electric Energy", OutputProcessor::Unit::J, electricityProd_, "System", "Sum", name_);

        reportPVandWindCapacity();

        sumUpNumberOfStorageDevices();

        checkLoadCenters(); // for issue #5302.
    }
}

void ElectricPowerServiceManager::setupMeterIndices()
{
    elecFacilityIndex_ = EnergyPlus::GetMeterIndex("Electricity:Facility");
    elecProducedCoGenIndex_ = EnergyPlus::GetMeterIndex("Cogeneration:ElectricityProduced");
    elecProducedPVIndex_ = EnergyPlus::GetMeterIndex("Photovoltaic:ElectricityProduced");
    elecProducedWTIndex_ = EnergyPlus::GetMeterIndex("WindTurbine:ElectricityProduced");
    elecProducedStorageIndex_ = EnergyPlus::GetMeterIndex("ElectricStorage:ElectricityProduced");
    elecProducedPowerConversionIndex_ = EnergyPlus::GetMeterIndex("PowerConversion:ElectricityProduced");

    if (numLoadCenters_ > 0) {
        for (auto &e : elecLoadCenterObjs) {
            e->setupLoadCenterMeterIndices();
        }
    }
    if (facilityPowerInTransformerPresent_) {
        facilityPowerInTransformerObj_->setupMeterIndices();
    }
}

void ElectricPowerServiceManager::reinitAtBeginEnvironment()
{
    wholeBldgRemainingLoad_ = 0.0;
    electricityProd_ = 0.0;
    electProdRate_ = 0.0;
    electricityPurch_ = 0.0;
    electPurchRate_ = 0.0;
    electSurplusRate_ = 0.0;
    electricitySurplus_ = 0.0;
    electricityNetRate_ = 0.0;
    electricityNet_ = 0.0;
    totalBldgElecDemand_ = 0.0;
    totalHVACElecDemand_ = 0.0;
    totalElectricDemand_ = 0.0;
    elecProducedPVRate_ = 0.0;
    elecProducedWTRate_ = 0.0;
    elecProducedStorageRate_ = 0.0;
    elecProducedCoGenRate_ = 0.0;

    if (numLoadCenters_ > 0) {
        for (auto &e : elecLoadCenterObjs) {
            e->reinitAtBeginEnvironment();
        }
    }
    if (facilityPowerInTransformerPresent_) {
        facilityPowerInTransformerObj_->reinitAtBeginEnvironment();
    }
    if (powerOutTransformerObj_ != nullptr) {
        powerOutTransformerObj_->reinitAtBeginEnvironment();
    }
}

void ElectricPowerServiceManager::verifyCustomMetersElecPowerMgr()
{
    for (std::size_t loop = 0; loop < elecLoadCenterObjs.size(); ++loop) {
        elecLoadCenterObjs[loop]->setupLoadCenterMeterIndices();
    }
}

void ElectricPowerServiceManager::updateWholeBuildingRecords()
{

    // main panel balancing.
    totalBldgElecDemand_ = GetInstantMeterValue(elecFacilityIndex_, 1) / DataGlobals::TimeStepZoneSec;
    totalHVACElecDemand_ = GetInstantMeterValue(elecFacilityIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    totalElectricDemand_ = totalBldgElecDemand_ + totalHVACElecDemand_;
    elecProducedPVRate_ = GetInstantMeterValue(elecProducedPVIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedWTRate_ = GetInstantMeterValue(elecProducedWTIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedStorageRate_ = GetInstantMeterValue(elecProducedStorageIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedCoGenRate_ = GetInstantMeterValue(elecProducedCoGenIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    elecProducedPowerConversionRate_ =
        GetInstantMeterValue(elecProducedPowerConversionIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);

    electProdRate_ = elecProducedCoGenRate_ + elecProducedPVRate_ + elecProducedWTRate_ + elecProducedStorageRate_ + elecProducedPowerConversionRate_;
    electricityProd_ = electProdRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // whole building

    // Report the Total Electric Power Purchased [W], If negative then there is extra power to be sold or stored.
    electPurchRate_ = totalElectricDemand_ - electProdRate_;
    // Check this value against a tolerance to aid in reporting.
    if (std::abs(electPurchRate_) < 0.0001) electPurchRate_ = 0.0;
    if (electPurchRate_ < 0.0) electPurchRate_ = 0.0; // don't want negative purchased...

    // Report the Total Electric Energy Purchased [J]
    electricityPurch_ = electPurchRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    // report the total electric surplus....
    electSurplusRate_ = electProdRate_ - totalElectricDemand_;
    if (std::abs(electSurplusRate_) < 0.0001) electSurplusRate_ = 0.0;
    if (electSurplusRate_ < 0.0) electSurplusRate_ = 0.0; // don't want negative surplus

    electricitySurplus_ = electSurplusRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    // report the net electricity , + is purchased, - is surplus
    electricityNetRate_ = totalElectricDemand_ - electProdRate_;

    electricityNet_ = electricityNetRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
}

void ElectricPowerServiceManager::reportPVandWindCapacity()
{
    // LEED report
    pvTotalCapacity_ = 0.0;
    windTotalCapacity_ = 0.0;
    for (auto &lc : elecLoadCenterObjs) {
        if (lc->numGenerators > 0) {
            for (auto &g : lc->elecGenCntrlObj) {
                if (g->compGenTypeOf_Num == DataGlobalConstants::iGeneratorPV) {
                    pvTotalCapacity_ += g->maxPowerOut;
                }
                if (g->compGenTypeOf_Num == DataGlobalConstants::iGeneratorWindTurbine) {
                    windTotalCapacity_ += g->maxPowerOut;
                }
            }
        }
    }
    // put in total capacity for PV and Wind for LEED report
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchLeedRenRatCap, "Photovoltaic", pvTotalCapacity_ / 1000, 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchLeedRenRatCap, "Wind", windTotalCapacity_ / 1000, 2);

    // future work: this legacy approach is relying on the correct power output to have been placed in the Generator list.  There could be a
    // difference between this control input and the actual size of the systems as defined in the generator objects themselves.  This method should be
    // replaced with queries that check the capacity from the generator models.
}

void ElectricPowerServiceManager::sumUpNumberOfStorageDevices()
{
    numElecStorageDevices = 0;
    for (auto &e : elecLoadCenterObjs) {
        if (e->storageObj != nullptr) {
            ++numElecStorageDevices;
        }
    }
}

void ElectricPowerServiceManager::checkLoadCenters()
{

    // issue #5302, detect if storage used on more than one load center. This is really a kind of GlobalNames issue.
    // expanded to all devices on a load center
    bool errorsFound = false;

    // first fill in a vector of names
    std::vector<std::string> storageNames;
    std::vector<std::string> genListNames;
    std::vector<std::string> inverterNames;
    std::vector<std::string> converterNames;
    std::vector<std::string> transformerNames;
    for (auto &e : elecLoadCenterObjs) {
        if (e->storageObj != nullptr) {
            storageNames.emplace_back(e->storageObj->name());
        }
        if (!e->elecGenCntrlObj.empty()) {
            genListNames.emplace_back(e->generatorListName());
        }
        if (e->inverterObj != nullptr) {
            inverterNames.emplace_back(e->inverterObj->name());
        }
        if (e->converterObj != nullptr) {
            converterNames.emplace_back(e->converterObj->name());
        }
        if (e->transformerObj != nullptr) {
            transformerNames.emplace_back(e->transformerObj->name());
        }
    }

    // then check the vectors for duplicates.
    for (std::size_t i = 0; i < storageNames.size(); ++i) {
        for (std::size_t j = 0; j < storageNames.size(); ++j) {
            if (storageNames[i] == storageNames[j] && i != j) {
                ShowSevereError("ElectricPowerServiceManager::checkLoadCenters, the electrical storage device named = " + storageNames[i] +
                                " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError("Electric Load Centers cannot share the same storage device.");
                errorsFound = true;
                break;
            }
        }
        if (errorsFound) {
            break;
        }
    }

    for (std::size_t i = 0; i < genListNames.size(); ++i) {
        for (std::size_t j = 0; j < genListNames.size(); ++j) {
            if (genListNames[i] == genListNames[j] && i != j) {
                ShowSevereError("ElectricPowerServiceManager::checkLoadCenters, the generator list named = " + genListNames[i] +
                                " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError("Electric Load Centers cannot share the same generator list (ElectricLoadCenter:Generators).");
                errorsFound = true;
                break;
            }
        }
        if (errorsFound) {
            break;
        }
    }

    for (std::size_t i = 0; i < inverterNames.size(); ++i) {
        for (std::size_t j = 0; j < inverterNames.size(); ++j) {
            if (inverterNames[i] == inverterNames[j] && i != j) {
                ShowSevereError("ElectricPowerServiceManager::checkLoadCenters, the inverter device named = " + inverterNames[i] +
                                " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError("Electric Load Centers cannot share the same inverter device.");
                errorsFound = true;
                break;
            }
        }
        if (errorsFound) {
            break;
        }
    }

    for (std::size_t i = 0; i < converterNames.size(); ++i) {
        for (std::size_t j = 0; j < converterNames.size(); ++j) {
            if (converterNames[i] == converterNames[j] && i != j) {
                ShowSevereError("ElectricPowerServiceManager::checkLoadCenters, the converter device named = " + converterNames[i] +
                                " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError("Electric Load Centers cannot share the same converter device.");
                errorsFound = true;
                break;
            }
        }
        if (errorsFound) {
            break;
        }
    }

    for (std::size_t i = 0; i < transformerNames.size(); ++i) {
        for (std::size_t j = 0; j < transformerNames.size(); ++j) {
            if (transformerNames[i] == transformerNames[j] && i != j) {
                ShowSevereError("ElectricPowerServiceManager::checkLoadCenters, the transformer device named = " + transformerNames[i] +
                                " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError("Electric Load Centers cannot share the same transformer device.");
                errorsFound = true;
                break;
            }
        }
        if (errorsFound) {
            break;
        }
    }

    if (errorsFound) { // throw fatal, these errors could fatal out in internal gains with missleading data
        ShowFatalError("ElectricPowerServiceManager::checkLoadCenters, preceding errors terminate program.");
    }
}

ElectPowerLoadCenter::ElectPowerLoadCenter( // constructor
    int const objectNum)
    : numGenerators(0), bussType(ElectricBussType::notYetSet), thermalProd(0.0), thermalProdRate(0.0), inverterPresent(false),
      subpanelFeedInRequest(0.0), subpanelFeedInRate(0.0), subpanelDrawRate(0.0), genElectricProd(0.0), genElectProdRate(0.0), storOpCVDrawRate(0.0),
      storOpCVFeedInRate(0.0), storOpCVChargeRate(0.0), storOpCVDischargeRate(0.0), storOpIsCharging(false), storOpIsDischarging(false),
      genOperationScheme_(GeneratorOpScheme::notYetSet), demandMeterPtr_(0), generatorsPresent_(false), myCoGenSetupFlag_(true), demandLimit_(0.0),
      trackSchedPtr_(0), dCElectricityProd_(0.0), dCElectProdRate_(0.0), dCpowerConditionLosses_(0.0), storagePresent_(false),
      transformerPresent_(false), totalPowerRequest_(0.0), totalThermalPowerRequest_(0.0), storageScheme_(StorageOpScheme::notYetSet),
      trackStorageOpMeterIndex_(0), converterPresent_(false), maxStorageSOCFraction_(1.0), minStorageSOCFraction_(0.0),
      designStorageChargePower_(0.0), designStorageChargePowerWasSet_(false), designStorageDischargePower_(0.0),
      designStorageDischargePowerWasSet_(false), storageChargeModSchedIndex_(0), storageDischargeModSchedIndex_(0), facilityDemandTarget_(0.0),
      facilityDemandTargetModSchedIndex_(0), eMSOverridePelFromStorage_(false), // if true, EMS calling for override
      eMSValuePelFromStorage_(0.0),                                             // value EMS is directing to use, power from storage [W]
      eMSOverridePelIntoStorage_(false),                                        // if true, EMS calling for override
      eMSValuePelIntoStorage_(0.0)                                              // value EMS is directing to use, power into storage [W]
{

    std::string const routineName = "ElectPowerLoadCenter constructor ";
    int numAlphas; // Number of elements in the alpha array
    int numNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound;

    DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Distribution";
    errorsFound = false;
    if (objectNum > 0) {
        inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      objectNum,
                                      DataIPShortCuts::cAlphaArgs,
                                      numAlphas,
                                      DataIPShortCuts::rNumericArgs,
                                      numNums,
                                      IOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);

        name_ = DataIPShortCuts::cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?

        if (!DataIPShortCuts::lAlphaFieldBlanks(2)) {
            generatorListName_ = DataIPShortCuts::cAlphaArgs(2);
            // check that

            int testIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Generators", generatorListName_);
            if (testIndex == 0) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                errorsFound = true;
            }
        }

        if (!DataIPShortCuts::lAlphaFieldBlanks(3)) {
            // Load the Generator Control Operation Scheme
            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "Baseload")) {
                genOperationScheme_ = GeneratorOpScheme::baseLoad;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "DemandLimit")) {
                genOperationScheme_ = GeneratorOpScheme::demandLimit;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "TrackElectrical")) {
                genOperationScheme_ = GeneratorOpScheme::trackElectrical;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "TrackSchedule")) {
                genOperationScheme_ = GeneratorOpScheme::trackSchedule;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "TrackMeter")) {
                genOperationScheme_ = GeneratorOpScheme::trackMeter;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "FollowThermal")) {
                genOperationScheme_ = GeneratorOpScheme::thermalFollow;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "FollowThermalLimitElectrical")) {
                genOperationScheme_ = GeneratorOpScheme::thermalFollowLimitElectrical;
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                errorsFound = true;
            }
        }

        demandLimit_ = DataIPShortCuts::rNumericArgs(1);

        trackSchedPtr_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(4));
        if ((trackSchedPtr_ == 0) && (genOperationScheme_ == GeneratorOpScheme::trackSchedule)) {
            if (!DataIPShortCuts::lAlphaFieldBlanks(4)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + DataIPShortCuts::cAlphaArgs(4));
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " = blank field.");
            }
            ShowContinueError("Schedule not found; Must be entered and valid when Generator Operation Scheme=TrackSchedule");
            errorsFound = true;
        }

        demandMeterName_ = UtilityRoutines::MakeUPPERCase(DataIPShortCuts::cAlphaArgs(5));
        // meters may not be "loaded" yet, defered check to later subroutine

        if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "AlternatingCurrent")) {
            bussType = ElectricBussType::aCBuss;
            DataIPShortCuts::cAlphaArgs(6) = "AlternatingCurrent";
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "DirectCurrentWithInverter")) {
            bussType = ElectricBussType::dCBussInverter;
            inverterPresent = true;
            DataIPShortCuts::cAlphaArgs(6) = "DirectCurrentWithInverter";
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "AlternatingCurrentWithStorage")) {
            bussType = ElectricBussType::aCBussStorage;
            storagePresent_ = true;
            DataIPShortCuts::cAlphaArgs(6) = "AlternatingCurrentWithStorage";
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "DirectCurrentWithInverterDCStorage")) {
            bussType = ElectricBussType::dCBussInverterDCStorage;
            inverterPresent = true;
            storagePresent_ = true;
            DataIPShortCuts::cAlphaArgs(6) = "DirectCurrentWithInverterDCStorage";
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "DirectCurrentWithInverterACStorage")) {
            bussType = ElectricBussType::dCBussInverterACStorage;
            inverterPresent = true;
            storagePresent_ = true;
            DataIPShortCuts::cAlphaArgs(6) = "DirectCurrentWithInverterACStorage";
        } else if (DataIPShortCuts::cAlphaArgs(6).empty()) {
            bussType = ElectricBussType::aCBuss;
            DataIPShortCuts::cAlphaArgs(6) = "AlternatingCurrent (field was blank)";
        } else {
            ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
            errorsFound = true;
        }

        if (inverterPresent) {
            if (!DataIPShortCuts::lAlphaFieldBlanks(7)) {
                inverterName = DataIPShortCuts::cAlphaArgs(7);
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(7) + " is blank, but buss type requires inverter.");
                errorsFound = true;
            }
        }

        if (storagePresent_) {
            if (!DataIPShortCuts::lAlphaFieldBlanks(8)) {
                storageName_ = DataIPShortCuts::cAlphaArgs(8);
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(8) + " is blank, but buss type requires storage.");
                errorsFound = true;
            }
        }

        if (!DataIPShortCuts::lAlphaFieldBlanks(9)) {
            // process transformer
            transformerName_ = DataIPShortCuts::cAlphaArgs(9);
            // only transformers of use type powerFromLoadCenterToBldg are really held in a load center, The legacy applications for transformers are
            // held at the higher Electric service level
            transformerPresent_ = true;
        }

        // Begin new content for grid supply and more control over storage
        // user selected storage operation scheme
        if (!DataIPShortCuts::lAlphaFieldBlanks(10)) {
            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(10), "TrackFacilityElectricDemandStoreExcessOnSite")) {
                storageScheme_ = StorageOpScheme::facilityDemandStoreExcessOnSite;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(10), "TrackMeterDemandStoreExcessOnSite")) {
                storageScheme_ = StorageOpScheme::meterDemandStoreExcessOnSite;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(10), "TrackChargeDischargeSchedules")) {
                storageScheme_ = StorageOpScheme::chargeDischargeSchedules;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(10), "FacilityDemandLeveling")) {
                storageScheme_ = StorageOpScheme::facilityDemandLeveling;
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(10) + " = " + DataIPShortCuts::cAlphaArgs(10));
                errorsFound = true;
            }
        } else { // blank (preserve legacy behavior for short files)
            storageScheme_ = StorageOpScheme::facilityDemandStoreExcessOnSite;
        }

        if (!DataIPShortCuts::lAlphaFieldBlanks(11)) {
            demandMeterName_ = DataIPShortCuts::cAlphaArgs(11);

        } else {
            if (storageScheme_ == StorageOpScheme::meterDemandStoreExcessOnSite) { // throw error
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(11) +
                                  ", cannot be blank when storage operation scheme is TrackMeterDemandStoreExcessOnSite");
                errorsFound = true;
            }
        }

        if (!DataIPShortCuts::lAlphaFieldBlanks(12)) {
            converterName_ = DataIPShortCuts::cAlphaArgs(12);
            converterPresent_ = true;
        } else {
            if (storageScheme_ == StorageOpScheme::chargeDischargeSchedules || storageScheme_ == StorageOpScheme::facilityDemandLeveling) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(12) + ", cannot be blank when storage scheme is " +
                                  DataIPShortCuts::cAlphaArgs(10));
                errorsFound = true;
            }
        }

        if (DataIPShortCuts::lNumericFieldBlanks(2)) {
            maxStorageSOCFraction_ = 1.0;
        } else {
            maxStorageSOCFraction_ = DataIPShortCuts::rNumericArgs(2);
        }
        if (DataIPShortCuts::lNumericFieldBlanks(3)) {
            minStorageSOCFraction_ = 0.0;
        } else {
            minStorageSOCFraction_ = DataIPShortCuts::rNumericArgs(3);
        }
        if (DataIPShortCuts::lNumericFieldBlanks(4)) {
            designStorageChargePowerWasSet_ = false;
        } else {
            designStorageChargePowerWasSet_ = true;
            designStorageChargePower_ = DataIPShortCuts::rNumericArgs(4);
        }
        if (DataIPShortCuts::lNumericFieldBlanks(5)) {
            designStorageDischargePowerWasSet_ = false;
        } else {
            designStorageDischargePowerWasSet_ = true;
            designStorageDischargePower_ = DataIPShortCuts::rNumericArgs(5);
        }

        if (DataIPShortCuts::lNumericFieldBlanks(6)) {
            if (storageScheme_ == StorageOpScheme::facilityDemandLeveling) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(6) + " = blank field.");
                errorsFound = true;
            }
        } else {
            facilityDemandTarget_ = DataIPShortCuts::rNumericArgs(6);
        }
        storageChargeModSchedIndex_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(13));
        if (storageChargeModSchedIndex_ == 0 && storageScheme_ == StorageOpScheme::chargeDischargeSchedules) {
            if (!DataIPShortCuts::lAlphaFieldBlanks(13)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(13) + " = " + DataIPShortCuts::cAlphaArgs(13));
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(13) + " = blank field.");
            }
            ShowContinueError("Schedule not found; Must be entered and valid when Storage Operation Scheme = TrackChargeDischargeSchedules");
            errorsFound = true;
        }

        storageDischargeModSchedIndex_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(14));
        if (storageDischargeModSchedIndex_ == 0 && storageScheme_ == StorageOpScheme::chargeDischargeSchedules) {
            if (!DataIPShortCuts::lAlphaFieldBlanks(14)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(14) + " = " + DataIPShortCuts::cAlphaArgs(14));
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(14) + " = blank field.");
            }
            ShowContinueError("Schedule not found; Must be entered and valid when Storage Operation Scheme = TrackChargeDischargeSchedules");
            errorsFound = true;
        }

        facilityDemandTargetModSchedIndex_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(15));
        if (facilityDemandTargetModSchedIndex_ == 0 && storageScheme_ == StorageOpScheme::facilityDemandLeveling) {
            if (!DataIPShortCuts::lAlphaFieldBlanks(15)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(15) + " = " + DataIPShortCuts::cAlphaArgs(15));
            } else {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(15) + " = blank field.");
            }
            ShowContinueError("Schedule not found; Must be entered and valid when Storage Operation Scheme = FacilityDemandLeveling");
            errorsFound = true;
        }
    } else { // object num == 0
        // just construct an empty object and return
        return;
    }

    // now that we are done with processing get input for ElectricLoadCenter:Distribution we can call child input objects without IP shortcut problems
    DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Generators";
    int genListObjectNum = inputProcessor->getObjectItemNum(DataIPShortCuts::cCurrentModuleObject, generatorListName_);
    if (genListObjectNum > 0) {
        inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      genListObjectNum,
                                      DataIPShortCuts::cAlphaArgs,
                                      numAlphas,
                                      DataIPShortCuts::rNumericArgs,
                                      numNums,
                                      IOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);

        // Calculate the number of generators in list
        numGenerators = numNums / 2; // note IDD needs Min Fields = 6
        if (mod((numAlphas - 1 + numNums), 5) != 0) ++numGenerators;
        int alphaCount = 2;
        for (auto genCount = 1; genCount <= numGenerators; ++genCount) {
            // call constructor in place
            generatorsPresent_ = true;
            elecGenCntrlObj.emplace_back(new GeneratorController(DataIPShortCuts::cAlphaArgs(alphaCount),
                                                                 DataIPShortCuts::cAlphaArgs(alphaCount + 1),
                                                                 DataIPShortCuts::rNumericArgs(2 * genCount - 1),
                                                                 DataIPShortCuts::cAlphaArgs(alphaCount + 2),
                                                                 DataIPShortCuts::rNumericArgs(2 * genCount)));
            ++alphaCount;
            ++alphaCount;
            ++alphaCount;
        }

        // issue #5299 check for non-zero values in thermal electric ratio if gen op scheme is thermalFollow*
        if (genOperationScheme_ == GeneratorOpScheme::thermalFollow || genOperationScheme_ == GeneratorOpScheme::thermalFollowLimitElectrical) {
            // check to make sure the user didn't input zeros for thermalToElectricControlRatio
            for (auto &g : elecGenCntrlObj) {
                if (g->nominalThermElectRatio <= 0.0) {
                    ShowWarningError("Generator operation needs to be based on following thermal loads and needs values for Rated Thermal to "
                                     "Electrical Power Ratio in " +
                                     DataIPShortCuts::cCurrentModuleObject + " named " + DataIPShortCuts::cAlphaArgs(1));
                }
            }
        }
    }

    if (!errorsFound && inverterPresent) {
        // call inverter constructor
        inverterObj = std::unique_ptr<DCtoACInverter>(new DCtoACInverter(inverterName));

        // Make sure only Generator::PVWatts are used with Inverter:PVWatts
        // Add up the total DC capacity and pass it to the inverter.
        if (inverterObj->modelType() == DCtoACInverter::InverterModelType::pvWatts) {
            Real64 totalDCCapacity = 0.0;
            for (const auto &generatorController : elecGenCntrlObj) {
                if (generatorController->generatorType != GeneratorController::GeneratorType::pvWatts) {
                    errorsFound = true;
                    ShowSevereError(routineName + "ElectricLoadCenter:Distribution=\"" + name_ + "\",");
                    ShowContinueError("ElectricLoadCenter:Inverter:PVWatts can only be used with Generator:PVWatts");
                    ShowContinueError("\"" + generatorController->name + "\" is of type " + generatorController->typeOfName);
                } else {
                    PVWatts::PVWattsGenerator &pvwGen = PVWatts::GetOrCreatePVWattsGenerator(generatorController->name);
                    totalDCCapacity += pvwGen.getDCSystemCapacity();
                }
            }
            if (!errorsFound) {
                inverterObj->setPVWattsDCCapacity(totalDCCapacity);
            }
        }
    }

    if (!errorsFound && storagePresent_) {
        // call storage constructor
        storageObj = std::unique_ptr<ElectricStorage>(new ElectricStorage(storageName_));
    }

    if (!errorsFound && transformerPresent_) {

        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Transformer";
        int transformerItemNum = inputProcessor->getObjectItemNum(DataIPShortCuts::cCurrentModuleObject, transformerName_);
        int iOStat;
        if (transformerItemNum > 0) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          transformerItemNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          numAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          numNums,
                                          iOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "LoadCenterPowerConditioning")) { // this is the right kind of transformer
                transformerObj = std::unique_ptr<ElectricTransformer>(new ElectricTransformer(transformerName_));
            } else {
                ShowWarningError("Transformer named " + transformerName_ + " associated with the load center named " + name_ + " should have " +
                                 DataIPShortCuts::cAlphaFieldNames(3) + " set to LoadCenterPowerConditioning.");
            }
        } else {
            ShowSevereError("Transformer named " + transformerName_ + ", was not found for the load center named " + name_);
            errorsFound = true;
        }
    }

    if (!errorsFound && converterPresent_) {
        // call AC to DC converter constructor
        converterObj = std::unique_ptr<ACtoDCConverter>(new ACtoDCConverter(converterName_));
    }

    // Setup general output variables for reporting in the electric load center
    SetupOutputVariable("Electric Load Center Produced Electric Power", OutputProcessor::Unit::W, genElectProdRate, "System", "Average", name_);
    SetupOutputVariable("Electric Load Center Produced Electric Energy", OutputProcessor::Unit::J, genElectricProd, "System", "Sum", name_);
    SetupOutputVariable("Electric Load Center Supplied Electric Power", OutputProcessor::Unit::W, subpanelFeedInRate, "System", "Average", name_);
    SetupOutputVariable("Electric Load Center Drawn Electric Power", OutputProcessor::Unit::W, subpanelDrawRate, "System", "Average", name_);
    SetupOutputVariable("Electric Load Center Produced Thermal Rate", OutputProcessor::Unit::W, thermalProdRate, "System", "Average", name_);
    SetupOutputVariable("Electric Load Center Produced Thermal Energy", OutputProcessor::Unit::J, thermalProd, "System", "Sum", name_);
    SetupOutputVariable("Electric Load Center Requested Electric Power", OutputProcessor::Unit::W, totalPowerRequest_, "System", "Average", name_);

    if (DataGlobals::AnyEnergyManagementSystemInModel && storagePresent_) {
        SetupEMSActuator("Electrical Storage", name_, "Power Draw Rate", "[W]", eMSOverridePelFromStorage_, eMSValuePelFromStorage_);
        SetupEMSActuator("Electrical Storage", name_, "Power Charge Rate", "[W]", eMSOverridePelIntoStorage_, eMSValuePelIntoStorage_);
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Preceding errors terminate program.");
    }
}

void ElectPowerLoadCenter::manageElecLoadCenter(bool const firstHVACIteration, Real64 &remainingWholePowerDemand)
{
    //
    subpanelFeedInRequest = remainingWholePowerDemand;

    if (generatorsPresent_) {
        dispatchGenerators(firstHVACIteration, remainingWholePowerDemand);

    } // if generators present
    updateLoadCenterGeneratorRecords();
    if (bussType == ElectricBussType::dCBussInverter || bussType == ElectricBussType::dCBussInverterACStorage) {
        inverterObj->simulate(genElectProdRate);
    }

    if (storagePresent_) {
        storageObj->timeCheckAndUpdate();
        dispatchStorage(subpanelFeedInRequest);
    }

    if (bussType == ElectricBussType::dCBussInverterDCStorage) {
        if (inverterObj != nullptr) {
            inverterObj->simulate(storOpCVFeedInRate);
        }
    }

    if (converterObj != nullptr) {
        converterObj->simulate(storOpCVDrawRate);
    }

    if (transformerObj != nullptr) {
        if (storOpCVFeedInRate > 0.0) {
            transformerObj->manageTransformers(storOpCVFeedInRate);
        } else if (storOpCVDrawRate > 0.0) {
            transformerObj->manageTransformers(subpanelDrawRate);
        }
    }
    updateLoadCenterGeneratorRecords();
}

void ElectPowerLoadCenter::dispatchGenerators(bool const firstHVACIteration,
                                              Real64 &remainingWholePowerDemand // power request in, remaining unmet request out
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
    Real64 remainingLoad = 0.0;
    Real64 customMeterDemand = 0.0;

    switch (genOperationScheme_) {

    case GeneratorOpScheme::baseLoad: {

        loadCenterElectricLoad = remainingWholePowerDemand;

        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0) {
                // Set the Operation Flag
                g->onThisTimestep = true;
                // Set the electric generator load request
                g->powerRequestThisTimestep = g->maxPowerOut;
            } else {
                g->onThisTimestep = false;
                g->powerRequestThisTimestep = 0.0;
            }

            // now handle EMS override
            if (g->eMSRequestOn) {
                g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                if (g->powerRequestThisTimestep > 0.0) {
                    g->onThisTimestep = true;
                } else {
                    g->onThisTimestep = false;
                }
            }

            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            totalPowerRequest_ += g->powerRequestThisTimestep;
            remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
        }
        break;
    }
    case GeneratorOpScheme::demandLimit: {
        // The Demand Limit scheme tries to have the generators meet all of the demand above the purchased Electric
        //  limit set by the user.
        remainingLoad = remainingWholePowerDemand - demandLimit_;
        loadCenterElectricLoad = remainingLoad;

        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
                // Set the Operation Flag
                g->onThisTimestep = true;

                // Set the electric generator load
                g->powerRequestThisTimestep = min(g->maxPowerOut, remainingLoad);

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            } else {
                g->onThisTimestep = false;
                g->powerRequestThisTimestep = 0.0;

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            }

            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            if (g->eMSRequestOn) {
                totalPowerRequest_ += max(g->eMSPowerRequest, 0.0);
            } else {
                if (g->powerRequestThisTimestep > 0.0) {
                    totalPowerRequest_ += g->maxPowerOut;
                    totalPowerRequest_ = min(loadCenterElectricLoad, totalPowerRequest_);
                }
            }
            remainingLoad -= g->electProdRate;             // Update remaining load to be met by this load center
            remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
        }
        break;
    }
    case GeneratorOpScheme::trackElectrical: {
        // The Track Electrical scheme tries to have the generators meet all of the electrical demand for the building.
        remainingLoad = remainingWholePowerDemand;
        loadCenterElectricLoad = remainingLoad;

        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
                // Set the Operation Flag
                g->onThisTimestep = true;

                // Set the electric generator load
                g->powerRequestThisTimestep = min(g->maxPowerOut, remainingLoad);

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            } else {
                g->onThisTimestep = false;
                g->powerRequestThisTimestep = 0.0;
                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            }

            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            if (g->eMSRequestOn) {
                totalPowerRequest_ += max(g->eMSPowerRequest, 0.0);
            } else {
                if (g->powerRequestThisTimestep > 0.0) {
                    totalPowerRequest_ += g->maxPowerOut;
                    totalPowerRequest_ = min(loadCenterElectricLoad, totalPowerRequest_);
                }
            }
            remainingLoad -= g->electProdRate;             // Update remaining load to be met by this load center
            remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
        }
        break;
    }
    case GeneratorOpScheme::trackSchedule: {
        // The Track Schedule scheme tries to have the generators meet the electrical demand determined from a schedule.
        //  Code is very similar to 'Track Electrical' except for initial RemainingLoad is replaced by SchedElecDemand
        //  and PV production is ignored.
        remainingLoad = ScheduleManager::GetCurrentScheduleValue(trackSchedPtr_);
        loadCenterElectricLoad = remainingLoad;

        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
                // Set the Operation Flag
                g->onThisTimestep = true;

                // Set the electric generator load
                g->powerRequestThisTimestep = min(g->maxPowerOut, remainingLoad);

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            } else {
                g->onThisTimestep = false;
                g->powerRequestThisTimestep = 0.0;

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            }

            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            if (g->eMSRequestOn) {
                totalPowerRequest_ += max(g->eMSPowerRequest, 0.0);
            } else {
                if (g->powerRequestThisTimestep > 0.0) {
                    totalPowerRequest_ += g->maxPowerOut;
                    totalPowerRequest_ = min(loadCenterElectricLoad, totalPowerRequest_);
                }
            }
            remainingLoad -= g->electProdRate;             // Update remaining load to be met by this load center
            remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
        }
        break;
    }
    case GeneratorOpScheme::trackMeter: {
        // The TRACK CUSTOM METER scheme tries to have the generators meet all of the
        //   electrical demand from a meter, it can also be a user-defined Custom Meter
        //   and PV is ignored.
        customMeterDemand = GetInstantMeterValue(demandMeterPtr_, 1) / DataGlobals::TimeStepZoneSec +
                            GetInstantMeterValue(demandMeterPtr_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);

        remainingLoad = customMeterDemand;
        loadCenterElectricLoad = remainingLoad;

        for (auto &g : elecGenCntrlObj) {
            if (ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
                // Set the Operation Flag
                g->onThisTimestep = true;
                // Set the electric generator load
                g->powerRequestThisTimestep = min(g->maxPowerOut, remainingLoad);

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            } else {
                g->onThisTimestep = false;
                g->powerRequestThisTimestep = 0.0;

                // now handle EMS override
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            }

            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            if (g->eMSRequestOn) {
                totalPowerRequest_ += max(g->eMSPowerRequest, 0.0);
            } else {
                if (g->powerRequestThisTimestep > 0.0) {
                    totalPowerRequest_ += g->maxPowerOut;
                    totalPowerRequest_ = min(loadCenterElectricLoad, totalPowerRequest_);
                }
            }
            remainingLoad -= g->electProdRate;             // Update remaining load to be met by this load center
            remainingWholePowerDemand -= g->electProdRate; // Update whole building remaining load
        }                                                  // end for
        break;
    }
    case GeneratorOpScheme::thermalFollow: {
        // Turn thermal load into an electrical load for cogenerators controlled to follow heat loads
        Real64 remainingThermalLoad = calcLoadCenterThermalLoad();
        Real64 loadCenterThermalLoad = remainingThermalLoad;
        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0 && remainingThermalLoad > 0.0) {

                if (g->nominalThermElectRatio > 0.0) {
                    remainingLoad = remainingThermalLoad / g->nominalThermElectRatio;
                    g->powerRequestThisTimestep = min(g->maxPowerOut, remainingLoad);
                    g->onThisTimestep = true;
                    // now handle EMS override
                    if (g->eMSRequestOn) {
                        g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                        if (g->powerRequestThisTimestep > 0.0) {
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
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            }

            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            if (g->eMSRequestOn) {
                totalThermalPowerRequest_ += (max(g->eMSPowerRequest, 0.0)) * g->nominalThermElectRatio;
                totalPowerRequest_ += (max(g->eMSPowerRequest, 0.0));
            } else {
                if (totalThermalPowerRequest_ < loadCenterThermalLoad && g->powerRequestThisTimestep > 0.0) {
                    Real64 excessThermalPowerRequest = totalThermalPowerRequest_ + g->maxPowerOut * g->nominalThermElectRatio - loadCenterThermalLoad;
                    if (excessThermalPowerRequest < 0.0) {
                        totalThermalPowerRequest_ += g->maxPowerOut * g->nominalThermElectRatio;
                        totalPowerRequest_ += g->maxPowerOut;
                    } else {
                        totalThermalPowerRequest_ = loadCenterThermalLoad;
                        if (g->nominalThermElectRatio > 0.0) {
                            totalPowerRequest_ += g->maxPowerOut - (excessThermalPowerRequest / g->nominalThermElectRatio);
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
        Real64 remainingThermalLoad = calcLoadCenterThermalLoad();
        // Total current electrical demand for the building is a secondary limit.
        remainingLoad = remainingWholePowerDemand;
        loadCenterElectricLoad = remainingWholePowerDemand;
        Real64 loadCenterThermalLoad = remainingThermalLoad;
        for (auto &g : elecGenCntrlObj) {
            if ((ScheduleManager::GetCurrentScheduleValue(g->availSchedPtr) > 0.0) && (remainingThermalLoad > 0.0) && (remainingLoad > 0.0)) {
                if (g->nominalThermElectRatio > 0.0) {
                    remainingLoad = min(remainingWholePowerDemand, remainingThermalLoad / g->nominalThermElectRatio);
                    g->powerRequestThisTimestep = min(g->maxPowerOut, remainingLoad);
                    g->onThisTimestep = true;
                    // now handle EMS override
                    if (g->eMSRequestOn) {
                        g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                        if (g->powerRequestThisTimestep > 0.0) {
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
                if (g->eMSRequestOn) {
                    g->powerRequestThisTimestep = max(g->eMSPowerRequest, 0.0);
                    if (g->powerRequestThisTimestep > 0.0) {
                        g->onThisTimestep = true;
                    } else {
                        g->onThisTimestep = false;
                    }
                }
            }
            // Get generator's actual electrical and thermal power outputs
            g->simGeneratorGetPowerOutput(g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

            if (g->eMSRequestOn) {
                totalThermalPowerRequest_ += (max(g->eMSPowerRequest, 0.0)) * g->nominalThermElectRatio;
                totalPowerRequest_ += (max(g->eMSPowerRequest, 0.0));
            } else {
                if (totalThermalPowerRequest_ < loadCenterThermalLoad && g->powerRequestThisTimestep > 0.0) {
                    Real64 excessThermalPowerRequest = totalThermalPowerRequest_ + g->maxPowerOut * g->nominalThermElectRatio - loadCenterThermalLoad;
                    if (excessThermalPowerRequest < 0.0) {
                        totalThermalPowerRequest_ += g->maxPowerOut * g->nominalThermElectRatio;
                        totalPowerRequest_ += g->maxPowerOut;
                    } else {
                        totalThermalPowerRequest_ = loadCenterThermalLoad;
                        if (g->nominalThermElectRatio > 0.0) {
                            totalPowerRequest_ += g->maxPowerOut - (excessThermalPowerRequest / g->nominalThermElectRatio);
                        }
                    }
                    totalPowerRequest_ = min(loadCenterElectricLoad, totalPowerRequest_);
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
    genElectProdRate = 0.0;
    genElectricProd = 0.0;
    for (auto &g : elecGenCntrlObj) {
        genElectProdRate += g->electProdRate;
        g->electricityProd = g->electProdRate * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        genElectricProd += g->electricityProd;
    }
}

void ElectPowerLoadCenter::dispatchStorage(Real64 const originalFeedInRequest // whole building remaining electric demand for this load center
)
{

    // 1. resolve generator power rate into storage operation control volume, by buss type
    switch (bussType) {
    case ElectricBussType::notYetSet:
    case ElectricBussType::aCBuss:
    case ElectricBussType::dCBussInverter: {
        // do nothing, no storage to manage
        break;
    }
    case ElectricBussType::aCBussStorage: {
        storOpCVGenRate = genElectProdRate;
        break;
    }
    case ElectricBussType::dCBussInverterDCStorage: {
        storOpCVGenRate = genElectProdRate;
        break;
    }
    case ElectricBussType::dCBussInverterACStorage: {
        // TODO call inverter model here?
        storOpCVGenRate = inverterObj->aCPowerOut();
        break;
    }
    } // end switch buss type

    // 2.  determine subpanel feed in and draw requests based on storage operation control scheme
    Real64 subpanelFeedInRequest = 0.0;
    Real64 subpanelDrawRequest = 0.0;
    switch (storageScheme_) {
    case StorageOpScheme::notYetSet: {
        // do nothing
        break;
    }
    case StorageOpScheme::facilityDemandStoreExcessOnSite: {
        subpanelFeedInRequest = originalFeedInRequest; // legacy behavior, storage dispatched to meet building load
        subpanelDrawRequest = 0.0;
        break;
    }
    case StorageOpScheme::meterDemandStoreExcessOnSite: {
        // Get meter rate
        subpanelFeedInRequest = GetInstantMeterValue(trackStorageOpMeterIndex_, 1) / DataGlobals::TimeStepZoneSec +
                                GetInstantMeterValue(trackStorageOpMeterIndex_, 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        subpanelDrawRequest = 0.0;
        break;
    }
    case StorageOpScheme::chargeDischargeSchedules: {
        // do not need to deal with subpanel rates here, charge or discharge is known from schedules and filled in below
        break;
    }
    case StorageOpScheme::facilityDemandLeveling: {
        Real64 demandTarget = facilityDemandTarget_ * ScheduleManager::GetCurrentScheduleValue(facilityDemandTargetModSchedIndex_);
        // compare target to
        Real64 deltaLoad = originalFeedInRequest - demandTarget;
        if (deltaLoad >= 0.0) {
            // subpanel should feed main panel
            subpanelFeedInRequest = deltaLoad;
            subpanelDrawRequest = 0.0;
        } else {
            // subpanel should draw from main panel
            subpanelFeedInRequest = 0.0;
            subpanelDrawRequest = std::abs(deltaLoad);
        }
        break;
    }
    }

    // 3. adjust feed in and draw rates from subpanel to storage operation control volume
    Real64 adjustedFeedInRequest = 0.0; // account for any inverter or transformer losses
    Real64 adjustedDrawRequest = 0.0;   // account for any converer or transformer losses

    switch (bussType) {
    case ElectricBussType::notYetSet:
    case ElectricBussType::aCBuss:
    case ElectricBussType::dCBussInverter: {
        // do nothing, no storage to manage
        break;
    }
    case ElectricBussType::aCBussStorage:
    case ElectricBussType::dCBussInverterACStorage: {
        if (transformerObj == nullptr) {
            adjustedFeedInRequest = subpanelFeedInRequest;
            adjustedDrawRequest = subpanelDrawRequest;
        } else {
            adjustedFeedInRequest = subpanelFeedInRequest + transformerObj->getLossRateForOutputPower(subpanelFeedInRequest);
            adjustedDrawRequest = subpanelDrawRequest - transformerObj->getLossRateForInputPower(subpanelDrawRequest);
        }
        break;
    }
    case ElectricBussType::dCBussInverterDCStorage: {
        // can we get updated power conditioning losses here?
        if (transformerObj == nullptr) {
            adjustedFeedInRequest = subpanelFeedInRequest + inverterObj->getLossRateForOutputPower(subpanelFeedInRequest);
            if (converterObj == nullptr) { // some operation schemes will never need a converter
                adjustedDrawRequest = subpanelDrawRequest;
            } else {
                adjustedDrawRequest = subpanelDrawRequest - converterObj->getLossRateForInputPower(subpanelDrawRequest);
            }
        } else {
            adjustedFeedInRequest = subpanelFeedInRequest + inverterObj->getLossRateForOutputPower(subpanelFeedInRequest) +
                                    transformerObj->getLossRateForOutputPower(subpanelFeedInRequest);
            if (converterObj == nullptr) {
                adjustedDrawRequest = subpanelDrawRequest - transformerObj->getLossRateForInputPower(subpanelDrawRequest);
            } else {
                adjustedDrawRequest = subpanelDrawRequest - converterObj->getLossRateForInputPower(subpanelDrawRequest) -
                                      transformerObj->getLossRateForInputPower(subpanelDrawRequest);
            }
        }
        break;
    }
    } // end switch buss type

    switch (storageScheme_) {
    case StorageOpScheme::notYetSet: {
        // do nothing
        break;
    }
    case StorageOpScheme::facilityDemandStoreExcessOnSite: // these are both the same because adjusted feed in request has already accounted for the
                                                           // difference
    case StorageOpScheme::meterDemandStoreExcessOnSite: {
        // this is the legacy behavior but with more limits from storage control operation information

        // no draws from main panel
        storOpCVDrawRate = 0.0;

        if (storOpCVGenRate < adjustedFeedInRequest) {
            // draw from storage
            storOpCVDischargeRate = adjustedFeedInRequest - storOpCVGenRate;
            storOpCVChargeRate = 0.0;
            storOpIsDischarging = true;
            storOpIsCharging = false;

        } else if (storOpCVGenRate > adjustedFeedInRequest) {
            // add to storage
            storOpCVDischargeRate = 0.0;
            storOpCVChargeRate = storOpCVGenRate - adjustedFeedInRequest;
            storOpIsCharging = true;
            storOpIsDischarging = false;

        } else if (storOpCVGenRate == adjustedFeedInRequest) {
            // do nothing
            storOpCVDischargeRate = 0.0;
            storOpCVChargeRate = 0.0;
            storOpIsCharging = false;
            storOpIsDischarging = false;
        }
        break;
    }

    case StorageOpScheme::chargeDischargeSchedules: {
        storOpCVChargeRate = designStorageChargePower_ * ScheduleManager::GetCurrentScheduleValue(storageChargeModSchedIndex_);
        storOpCVDischargeRate = designStorageDischargePower_ * ScheduleManager::GetCurrentScheduleValue(storageDischargeModSchedIndex_);
        Real64 genAndStorSum = storOpCVGenRate + storOpCVDischargeRate - storOpCVChargeRate;
        if (genAndStorSum >= 0.0) { // power to feed toward main panel
            storOpCVDrawRate = 0.0;
            storOpCVFeedInRate = genAndStorSum;
        } else { // shortfall, will need to draw from main panel (e.g. for grid charging)
            storOpCVFeedInRate = 0.0;
            storOpCVDrawRate = std::abs(genAndStorSum);
        }
        if (storOpCVChargeRate > 0.0) {
            storOpIsCharging = true;
        } else {
            storOpIsCharging = false;
        }
        if (storOpCVDischargeRate > 0.0) {
            storOpIsDischarging = true;
        } else {
            storOpIsDischarging = false;
        }
        break;
    }
    case StorageOpScheme::facilityDemandLeveling: {

        if (adjustedDrawRequest > 0.0) { // the only reason to draw instead of feed is to charge storage
            storOpCVFeedInRate = 0.0;
            storOpCVDrawRate = adjustedDrawRequest;
            storOpCVChargeRate = storOpCVDrawRate + storOpCVGenRate;
            storOpCVDischargeRate = 0.0;
            storOpIsCharging = true;
            storOpIsDischarging = false;
        }
        if (adjustedFeedInRequest > 0.0) {
            storOpCVDrawRate = 0.0;
            storOpCVFeedInRate = adjustedFeedInRequest;
            if (storOpCVGenRate < adjustedFeedInRequest) {
                // draw from storage
                storOpCVDischargeRate = adjustedFeedInRequest - storOpCVGenRate;
                storOpCVChargeRate = 0.0;
                storOpIsDischarging = true;
                storOpIsCharging = false;

            } else if (storOpCVGenRate > adjustedFeedInRequest) {
                // add to storage
                storOpCVDischargeRate = 0.0;
                storOpCVChargeRate = storOpCVGenRate - adjustedFeedInRequest;
                storOpIsCharging = true;
                storOpIsDischarging = false;

            } else if (storOpCVGenRate == adjustedFeedInRequest) {
                // do nothing
                storOpCVDischargeRate = 0.0;
                storOpCVChargeRate = 0.0;
                storOpIsCharging = false;
                storOpIsDischarging = false;
            }
        }
        break;
    }
    }

    // handle EMS overrides
    if (eMSOverridePelFromStorage_ || eMSOverridePelIntoStorage_) {
        if (eMSOverridePelFromStorage_ && !eMSOverridePelIntoStorage_) {
            // EMS is calling for specific discharge rate
            storOpCVDischargeRate = max(eMSValuePelFromStorage_, 0.0);
            storOpCVChargeRate = 0.0;
            storOpIsDischarging = true;
            storOpIsCharging = false;
        } else if (!eMSOverridePelFromStorage_ && eMSOverridePelIntoStorage_) {
            // EMS is calling for specific charge rate
            storOpCVChargeRate = max(eMSValuePelIntoStorage_, 0.0);
            storOpCVDischargeRate = 0.0;
            storOpIsDischarging = false;
            storOpIsCharging = true;
        } else if (eMSOverridePelFromStorage_ && eMSOverridePelIntoStorage_) {
            // EMS is asking to override both
            if (eMSValuePelIntoStorage_ > eMSValuePelFromStorage_) {
                storOpCVChargeRate = eMSValuePelIntoStorage_ - eMSValuePelFromStorage_;
                storOpCVDischargeRate = 0.0;
                storOpIsDischarging = false;
                storOpIsCharging = true;
            } else if (eMSValuePelIntoStorage_ < eMSValuePelFromStorage_) {
                storOpCVDischargeRate = eMSValuePelFromStorage_ - eMSValuePelIntoStorage_;
                storOpCVChargeRate = 0.0;
                storOpIsDischarging = true;
                storOpIsCharging = false;
            } else { // they equal just hold
                storOpCVDischargeRate = 0.0;
                storOpCVChargeRate = 0.0;
                storOpIsDischarging = false;
                storOpIsCharging = false;
            }
        }
    }

    // check against the controller limits
    if (designStorageChargePowerWasSet_) {
        storOpCVChargeRate = min(storOpCVChargeRate, designStorageChargePower_);
    }
    if (designStorageDischargePowerWasSet_) {
        storOpCVDischargeRate = min(storOpCVDischargeRate, designStorageDischargePower_);
    }

    // dispatch final request to storage device, calculate, update, and report storage device, passing what controller wants for SOC limits

    storageObj->simulate(
        storOpCVChargeRate, storOpCVDischargeRate, storOpIsCharging, storOpIsDischarging, maxStorageSOCFraction_, minStorageSOCFraction_);

    // rebalance with final charge and discharge rates
    Real64 genAndStorSum = storOpCVGenRate + storOpCVDischargeRate - storOpCVChargeRate;
    if (genAndStorSum >= 0.0) { // power to feed toward main panel
        storOpCVDrawRate = 0.0;
        storOpCVFeedInRate = genAndStorSum;
    } else { // shortfall, will need to draw from main panel (e.g. for grid charging)
        storOpCVFeedInRate = 0.0;
        storOpCVDrawRate = std::abs(genAndStorSum);
    }
}

void ElectPowerLoadCenter::setupLoadCenterMeterIndices()
{
    demandMeterPtr_ = EnergyPlus::GetMeterIndex(demandMeterName_);
    if ((demandMeterPtr_ == 0) && (genOperationScheme_ == GeneratorOpScheme::trackMeter)) { // throw error
        ShowFatalError("ElectPowerLoadCenter::setupLoadCenterMeterIndices  Did not find Meter named: " + demandMeterName_ +
                       " in ElectricLoadCenter:Distribution named " + name_);
    }

    if (storageScheme_ == StorageOpScheme::meterDemandStoreExcessOnSite) {
        trackStorageOpMeterIndex_ = EnergyPlus::GetMeterIndex(trackSorageOpMeterName_);
        if (trackStorageOpMeterIndex_ == 0) { //
            ShowFatalError("ElectPowerLoadCenter::setupLoadCenterMeterIndices  Did not find Meter named: " + trackSorageOpMeterName_ +
                           " in ElectricLoadCenter:Distribution named " + name_);
        }
    }
}

void ElectPowerLoadCenter::reinitAtBeginEnvironment()
{
    dCElectricityProd_ = 0.0;
    dCElectProdRate_ = 0.0;
    dCpowerConditionLosses_ = 0.0;
    genElectricProd = 0.0;
    genElectProdRate = 0.0;
    thermalProd = 0.0;
    thermalProdRate = 0.0;
    totalPowerRequest_ = 0.0;
    totalThermalPowerRequest_ = 0.0;
    subpanelFeedInRate = 0.0;
    subpanelDrawRate = 0.0;
    storOpCVDrawRate = 0.0;
    storOpCVFeedInRate = 0.0;
    storOpCVChargeRate = 0.0;
    storOpCVDischargeRate = 0.0;

    if (generatorsPresent_ && numGenerators > 0) {
        for (auto &g : elecGenCntrlObj) {
            g->reinitAtBeginEnvironment();
        }
    }

    if (transformerObj != nullptr) {
        transformerObj->reinitAtBeginEnvironment();
    }

    if (storageObj != nullptr) {
        storageObj->reinitAtBeginEnvironment();
    }

    if (inverterObj != nullptr) {
        inverterObj->reinitAtBeginEnvironment();
    }

    if (converterObj != nullptr) {
        converterObj->reinitAtBeginEnvironment();
    }
}

void ElectPowerLoadCenter::reinitZoneGainsAtBeginEnvironment()
{
    if (transformerObj != nullptr) {
        transformerObj->reinitZoneGainsAtBeginEnvironment();
    }

    if (storageObj != nullptr) {
        storageObj->reinitZoneGainsAtBeginEnvironment();
    }

    if (inverterObj != nullptr) {
        inverterObj->reinitZoneGainsAtBeginEnvironment();
    }

    if (converterObj != nullptr) {
        converterObj->reinitZoneGainsAtBeginEnvironment();
    }
}

std::string const &ElectPowerLoadCenter::transformerName() const
{
    return transformerName_;
}

std::string const &ElectPowerLoadCenter::generatorListName() const
{
    return generatorListName_;
}

void ElectPowerLoadCenter::updateLoadCenterGeneratorRecords()
{

    switch (bussType) {
    case ElectricBussType::aCBuss: {
        genElectProdRate = 0.0;
        genElectricProd = 0.0;
        for (auto &gc : elecGenCntrlObj) {
            genElectProdRate += gc->electProdRate;
            genElectricProd += gc->electricityProd;
        }
        // no inverter, no storage, so generator production equals subpanel feed in
        subpanelFeedInRate = genElectProdRate;
        if (transformerObj != nullptr) {
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(genElectProdRate);
        }
        subpanelDrawRate = 0.0;

        break;
    }
    case ElectricBussType::aCBussStorage: {
        genElectProdRate = 0.0;
        genElectricProd = 0.0;
        for (auto &gc : elecGenCntrlObj) {
            genElectProdRate += gc->electProdRate;
            genElectricProd += gc->electricityProd;
        }
        if (storageObj != nullptr) {
            subpanelFeedInRate = genElectProdRate + storOpCVDischargeRate - storOpCVChargeRate;
        } else {
            subpanelFeedInRate = genElectProdRate;
        }
        if (transformerObj != nullptr) {
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(subpanelFeedInRate);
        }
        subpanelDrawRate = 0.0;
        break;
    }
    case ElectricBussType::dCBussInverter: {
        genElectProdRate = 0.0;
        genElectricProd = 0.0;
        for (auto &gc : elecGenCntrlObj) {
            genElectProdRate += gc->electProdRate;
            genElectricProd += gc->electricityProd;
        }

        if (inverterObj != nullptr) {
            subpanelFeedInRate = inverterObj->aCPowerOut();
        }
        if (transformerObj != nullptr) {
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(subpanelFeedInRate);
        }
        subpanelDrawRate = 0.0;
        break;
    }

    case ElectricBussType::dCBussInverterDCStorage: {
        genElectProdRate = 0.0;
        genElectricProd = 0.0;
        for (auto &gc : elecGenCntrlObj) {
            genElectProdRate += gc->electProdRate;
            genElectricProd += gc->electricityProd;
        }
        if (inverterObj != nullptr) {
            subpanelFeedInRate = inverterObj->aCPowerOut();
        }

        if (converterObj != nullptr) {
            subpanelDrawRate = converterObj->aCPowerIn();
        }
        if (transformerObj != nullptr) {
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(subpanelFeedInRate);
            subpanelDrawRate += transformerObj->getLossRateForOutputPower(subpanelDrawRate);
        }
        break;
    }
    case ElectricBussType::dCBussInverterACStorage: {
        genElectProdRate = 0.0;
        genElectricProd = 0.0;
        for (auto &gc : elecGenCntrlObj) {
            genElectProdRate += gc->electProdRate;
            genElectricProd += gc->electricityProd;
        }
        if (inverterObj != nullptr && storagePresent_) {
            subpanelFeedInRate = inverterObj->aCPowerOut() + storOpCVDischargeRate - storOpCVChargeRate;
        }

        subpanelDrawRate = storOpCVDrawRate; // no converter for AC storage
        if (transformerObj != nullptr) {
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(subpanelFeedInRate);
            subpanelDrawRate += transformerObj->getLossRateForOutputPower(subpanelDrawRate);
        }
        break;
    }
    case ElectricBussType::notYetSet: {
        // do nothing
    }

    } // end switch
    thermalProdRate = 0.0;
    thermalProd = 0.0;
    for (auto &gc : elecGenCntrlObj) {
        thermalProdRate += gc->thermProdRate;
        thermalProd += gc->thermalProd;
    }
}

Real64 ElectPowerLoadCenter::calcLoadCenterThermalLoad()
{
    if (myCoGenSetupFlag_) {
        bool plantNotFound = false;
        for (auto &g : elecGenCntrlObj) {
            plantNotFound = false;
            PlantUtilities::ScanPlantLoopsForObject(g->compPlantName,
                                                    g->compPlantTypeOf_Num,
                                                    g->cogenLocation.loopNum,
                                                    g->cogenLocation.loopSideNum,
                                                    g->cogenLocation.branchNum,
                                                    g->cogenLocation.compNum,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    plantNotFound);
            if (!plantNotFound) g->plantInfoFound = true;
        }
        myCoGenSetupFlag_ = false;
    } // cogen setup

    // sum up "MyLoad" for all generators on this load center from plant structure
    Real64 thermalLoad = 0.0;
    for (auto &g : elecGenCntrlObj) {
        if (g->plantInfoFound) {
            thermalLoad += DataPlant::PlantLoop(g->cogenLocation.loopNum)
                               .LoopSide(g->cogenLocation.loopSideNum)
                               .Branch(g->cogenLocation.branchNum)
                               .Comp(g->cogenLocation.compNum)
                               .MyLoad;
        }
    }
    return thermalLoad;
}

GeneratorController::GeneratorController(std::string const &objectName,
                                         std::string const &objectType,
                                         Real64 ratedElecPowerOutput,
                                         std::string const &availSchedName,
                                         Real64 thermalToElectRatio)
    : compGenTypeOf_Num(0), compPlantTypeOf_Num(0), generatorType(GeneratorType::notYetSet), generatorIndex(0), maxPowerOut(0.0), availSchedPtr(0),
      powerRequestThisTimestep(0.0), onThisTimestep(false), eMSPowerRequest(0.0), eMSRequestOn(false), plantInfoFound(false),
      cogenLocation(PlantLocation(0, 0, 0, 0)), nominalThermElectRatio(0.0), dCElectricityProd(0.0), dCElectProdRate(0.0), electricityProd(0.0),
      electProdRate(0.0), thermalProd(0.0), thermProdRate(0.0), errCountNegElectProd_(0)
{

    std::string const routineName = "GeneratorController constructor ";
    bool errorsFound = false;

    name = objectName;
    typeOfName = objectType;
    if (UtilityRoutines::SameString(objectType, "Generator:InternalCombustionEngine")) {
        generatorType = GeneratorType::iCEngine;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorICEngine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_ICEngine;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:CombustionTurbine")) {
        generatorType = GeneratorType::combTurbine;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorCombTurbine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_CTurbine;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:MicroTurbine")) {
        generatorType = GeneratorType::microturbine;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorMicroturbine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_MicroTurbine;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:Photovoltaic")) {
        generatorType = GeneratorType::pV;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorPV;
        compPlantTypeOf_Num = DataPlant::TypeOf_PVTSolarCollectorFlatPlate;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:PVWatts")) {
        generatorType = GeneratorType::pvWatts;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorPVWatts;
        compPlantTypeOf_Num = DataPlant::TypeOf_Other;
    } else if (UtilityRoutines::SameString(objectType, "Generator:FuelCell")) {
        generatorType = GeneratorType::fuelCell;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorFuelCell;
        // fuel cell has two possible plant component types, stack cooler and exhaust gas HX.
        // exhaust gas HX is required and it assumed that it has more thermal capacity and is used for control
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_FCExhaust;
        // and the name of plant component is not the same as the generator because of child object references, so fetch that name
        FuelCellElectricGenerator::getFuelCellGeneratorHeatRecoveryInfo(name, compPlantName);
    } else if (UtilityRoutines::SameString(objectType, "Generator:MicroCHP")) {
        generatorType = GeneratorType::microCHP;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorMicroCHP;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_MicroCHP;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:WindTurbine")) {
        generatorType = GeneratorType::windTurbine;
        compGenTypeOf_Num = DataGlobalConstants::iGeneratorWindTurbine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Other;
    } else {
        ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + " invalid entry.");
        ShowContinueError("Invalid " + objectType + " associated with generator = " + objectName);
        errorsFound = true;
    }

    availSched = availSchedName;
    if (availSched.empty()) {
        availSchedPtr = DataGlobals::ScheduleAlwaysOn;
    } else {
        availSchedPtr = ScheduleManager::GetScheduleIndex(availSchedName);
        if (availSchedPtr <= 0) {
            ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + ", invalid entry.");
            ShowContinueError("Invalid availability schedule = " + availSchedName);
            ShowContinueError("Schedule was not found ");
            errorsFound = true;
        }
    }

    maxPowerOut = ratedElecPowerOutput, nominalThermElectRatio = thermalToElectRatio;

    SetupOutputVariable("Generator Requested Electric Power", OutputProcessor::Unit::W, powerRequestThisTimestep, "System", "Average", objectName);
    if (DataGlobals::AnyEnergyManagementSystemInModel) {
        SetupEMSInternalVariable("Generator Nominal Maximum Power", objectName, "[W]", maxPowerOut);
        SetupEMSInternalVariable("Generator Nominal Thermal To Electric Ratio", objectName, "[ratio]", nominalThermElectRatio);
        SetupEMSActuator("On-Site Generator Control", objectName, "Requested Power", "[W]", eMSRequestOn, eMSPowerRequest);
    }
}

void GeneratorController::reinitAtBeginEnvironment()
{
    onThisTimestep = false;
    dCElectricityProd = 0.0;
    dCElectProdRate = 0.0;
    electricityProd = 0.0;
    electProdRate = 0.0;
    thermalProd = 0.0;
    thermProdRate = 0.0;
}

void GeneratorController::simGeneratorGetPowerOutput(bool const runFlag,
                                                     Real64 const myElecLoadRequest,
                                                     bool const FirstHVACIteration, // Unused 2010 JANUARY
                                                     Real64 &electricPowerOutput,   // Actual generator electric power output
                                                     Real64 &thermalPowerOutput     // Actual generator thermal power output
)
{
    // Select and call models and also collect results for load center power conditioning and reporting
    switch (generatorType) {
    case GeneratorType::iCEngine: {
        ICEngineElectricGenerator::SimICEngineGenerator(
            DataGlobalConstants::iGeneratorICEngine, name, generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration);
        ICEngineElectricGenerator::GetICEGeneratorResults(
            DataGlobalConstants::iGeneratorICEngine, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::combTurbine: {
        CTElectricGenerator::SimCTGenerator(
            DataGlobalConstants::iGeneratorCombTurbine, name, generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration);
        CTElectricGenerator::GetCTGeneratorResults(
            DataGlobalConstants::iGeneratorCombTurbine, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::pV: {
        Photovoltaics::SimPVGenerator(DataGlobalConstants::iGeneratorPV, name, generatorIndex, runFlag, myElecLoadRequest);
        Photovoltaics::GetPVGeneratorResults(
            DataGlobalConstants::iGeneratorPV, generatorIndex, dCElectProdRate, dCElectricityProd, thermProdRate, thermalProd);
        electricPowerOutput = dCElectProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::pvWatts: {
        PVWatts::PVWattsGenerator &pvwattsGenerator(PVWatts::GetOrCreatePVWattsGenerator(name));
        pvwattsGenerator.calc();
        pvwattsGenerator.getResults(dCElectProdRate, dCElectricityProd, thermProdRate, thermalProd);
        electricPowerOutput = dCElectProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::fuelCell: {
        FuelCellElectricGenerator::SimFuelCellGenerator(
            DataGlobalConstants::iGeneratorFuelCell, name, generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration);
        FuelCellElectricGenerator::GetFuelCellGeneratorResults(
            DataGlobalConstants::iGeneratorFuelCell, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::microCHP: {
        MicroCHPElectricGenerator::SimMicroCHPGenerator(DataGlobalConstants::iGeneratorMicroCHP,
                                                        name,
                                                        generatorIndex,
                                                        runFlag,
                                                        false,
                                                        myElecLoadRequest,
                                                        DataPrecisionGlobals::constant_zero,
                                                        FirstHVACIteration);
        MicroCHPElectricGenerator::GetMicroCHPGeneratorResults(
            DataGlobalConstants::iGeneratorMicroCHP, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::microturbine: {
        MicroturbineElectricGenerator::SimMTGenerator(
            DataGlobalConstants::iGeneratorMicroturbine, name, generatorIndex, runFlag, myElecLoadRequest, FirstHVACIteration);
        MicroturbineElectricGenerator::GetMTGeneratorResults(
            DataGlobalConstants::iGeneratorMicroturbine, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::windTurbine: {
        WindTurbine::SimWindTurbine(DataGlobalConstants::iGeneratorWindTurbine, name, generatorIndex, runFlag, myElecLoadRequest);
        WindTurbine::GetWTGeneratorResults(
            DataGlobalConstants::iGeneratorWindTurbine, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::notYetSet: {
        // do nothing
        break;
    }
    } // end switch

    // check if generator production has gone wrong and is negative, reset to zero and warn
    if (electricPowerOutput < 0.0) {
        if (errCountNegElectProd_ == 0) {
            ShowWarningMessage(typeOfName + " named " + name + " is producing negative electric power, check generator inputs.");
            ShowContinueError("Electric power production rate =" + General::RoundSigDigits(electricPowerOutput, 4));
            ShowContinueError("The power will be set to zero, and the simulation continues... ");
        }
        ShowRecurringWarningErrorAtEnd(typeOfName + " named " + name + " is producing negative electric power ",
                                       errCountNegElectProd_,
                                       electricPowerOutput,
                                       electricPowerOutput);
        electricPowerOutput = 0.0;
    }
}

DCtoACInverter::DCtoACInverter(std::string const &objectName)
    : aCPowerOut_(0.0), aCEnergyOut_(0.0), efficiency_(0.0), dCPowerIn_(0.0), dCEnergyIn_(0.0), conversionLossPower_(0.0), conversionLossEnergy_(0.0),
      conversionLossEnergyDecrement_(0.0), thermLossRate_(0.0), thermLossEnergy_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0), ancillACuseRate_(0.0),
      ancillACuseEnergy_(0.0), modelType_(InverterModelType::notYetSet), availSchedPtr_(0),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0), zoneRadFract_(0.0), nominalVoltage_(0.0),
      nomVoltEfficiencyARR_(6, 0.0), curveNum_(0), ratedPower_(0.0), minPower_(0.0), maxPower_(0.0), minEfficiency_(0.0), maxEfficiency_(0.0),
      standbyPower_(0.0)
{
    // initialize
    nomVoltEfficiencyARR_.resize(6, 0.0);

    std::string const routineName = "DCtoACInverter constructor ";
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    // if/when add object class name to input object this can be simplified. for now search all possible types
    bool foundInverter = false;
    int testInvertIndex = 0;
    int invertIDFObjectNum = 0;

    testInvertIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Inverter:LookUpTable", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:LookUpTable";
        modelType_ = InverterModelType::cECLookUpTableModel;
    }
    testInvertIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Inverter:FunctionOfPower", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:FunctionOfPower";
        modelType_ = InverterModelType::curveFuncOfPower;
    }
    testInvertIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Inverter:Simple", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:Simple";
        modelType_ = InverterModelType::simpleConstantEff;
    }
    testInvertIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Inverter:PVWatts", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Inverter:PVWatts";
        modelType_ = InverterModelType::pvWatts;
    }

    if (foundInverter) {

        inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      invertIDFObjectNum,
                                      DataIPShortCuts::cAlphaArgs,
                                      NumAlphas,
                                      DataIPShortCuts::rNumericArgs,
                                      NumNums,
                                      IOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);

        name_ = DataIPShortCuts::cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?

        if (modelType_ == InverterModelType::pvWatts) {
            availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
            zoneNum_ = 0;
            heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            zoneRadFract_ = 0;
        } else {
            if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
                availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
            } else {
                availSchedPtr_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
                if (availSchedPtr_ == 0) {
                    ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) +
                                    "\", invalid entry.");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                    errorsFound = true;
                }
            }

            zoneNum_ = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(3), DataHeatBalance::Zone);
            if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
            if (zoneNum_ == 0) {
                if (DataIPShortCuts::lAlphaFieldBlanks(3)) {
                    heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                } else {
                    heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                    ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) +
                                     "\", invalid entry.");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                    ShowContinueError("Zone name not found. Inverter heat losses will not be added to a zone");
                    // continue with simulation but inverter losses not sent to a zone.
                }
            }
            zoneRadFract_ = DataIPShortCuts::rNumericArgs(1);
        }

        // now the input objects differ depending on class type
        switch (modelType_) {
        case InverterModelType::cECLookUpTableModel: {
            ratedPower_ = DataIPShortCuts::rNumericArgs(2);
            standbyPower_ = DataIPShortCuts::rNumericArgs(3);

            nominalVoltage_ = DataIPShortCuts::rNumericArgs(4);
            nomVoltEfficiencyARR_[0] = DataIPShortCuts::rNumericArgs(5);
            nomVoltEfficiencyARR_[1] = DataIPShortCuts::rNumericArgs(6);
            nomVoltEfficiencyARR_[2] = DataIPShortCuts::rNumericArgs(7);
            nomVoltEfficiencyARR_[3] = DataIPShortCuts::rNumericArgs(8);
            nomVoltEfficiencyARR_[4] = DataIPShortCuts::rNumericArgs(9);
            nomVoltEfficiencyARR_[5] = DataIPShortCuts::rNumericArgs(10);
            break;
        }
        case InverterModelType::curveFuncOfPower: {
            curveNum_ = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(4));
            if (curveNum_ == 0) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + DataIPShortCuts::cAlphaArgs(4));
                ShowContinueError("Curve was not found");
                errorsFound = true;
            }

            ratedPower_ = DataIPShortCuts::rNumericArgs(2);
            minEfficiency_ = DataIPShortCuts::rNumericArgs(3);
            maxEfficiency_ = DataIPShortCuts::rNumericArgs(4);
            minPower_ = DataIPShortCuts::rNumericArgs(5);
            maxPower_ = DataIPShortCuts::rNumericArgs(6);
            standbyPower_ = DataIPShortCuts::rNumericArgs(7);
            break;
        }
        case InverterModelType::simpleConstantEff: {
            efficiency_ = DataIPShortCuts::rNumericArgs(2);
            break;
        }
        case InverterModelType::notYetSet: {
            // do nothing
            break;
        }
        case InverterModelType::pvWatts: {
            pvWattsDCtoACSizeRatio_ = DataIPShortCuts::rNumericArgs(1);
            pvWattsInverterEfficiency_ = DataIPShortCuts::rNumericArgs(2);
            break;
        }

        } // end switch modelType

        SetupOutputVariable("Inverter DC to AC Efficiency", OutputProcessor::Unit::None, efficiency_, "System", "Average", name_);
        SetupOutputVariable("Inverter DC Input Electric Power", OutputProcessor::Unit::W, dCPowerIn_, "System", "Average", name_);
        SetupOutputVariable("Inverter DC Input Electric Energy", OutputProcessor::Unit::J, dCEnergyIn_, "System", "Sum", name_);
        SetupOutputVariable("Inverter AC Output Electric Power", OutputProcessor::Unit::W, aCPowerOut_, "System", "Average", name_);
        SetupOutputVariable("Inverter AC Output Electric Energy", OutputProcessor::Unit::J, aCEnergyOut_, "System", "Sum", name_);
        SetupOutputVariable("Inverter Conversion Loss Power", OutputProcessor::Unit::W, conversionLossPower_, "System", "Average", name_);
        SetupOutputVariable("Inverter Conversion Loss Energy", OutputProcessor::Unit::J, conversionLossEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Inverter Conversion Loss Decrement Energy",
                            OutputProcessor::Unit::J,
                            conversionLossEnergyDecrement_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricityProduced",
                            "POWERCONVERSION",
                            _,
                            "Plant");
        SetupOutputVariable("Inverter Thermal Loss Rate", OutputProcessor::Unit::W, thermLossRate_, "System", "Average", name_);
        SetupOutputVariable("Inverter Thermal Loss Energy", OutputProcessor::Unit::J, thermLossEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Inverter Ancillary AC Electric Power", OutputProcessor::Unit::W, ancillACuseRate_, "System", "Average", name_);
        SetupOutputVariable("Inverter Ancillary AC Electric Energy",
                            OutputProcessor::Unit::J,
                            ancillACuseEnergy_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "Electricity",
                            "Cogeneration",
                            "DCtoACInverter Ancillary",
                            "Plant"); // called cogeneration for end use table
        if (zoneNum_ > 0) {
            switch (modelType_) {
            case InverterModelType::simpleConstantEff: {
                SetupZoneInternalGain(zoneNum_,
                                      "ElectricLoadCenter:Inverter:Simple",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterSimple,
                                      qdotConvZone_,
                                      _,
                                      qdotRadZone_);
                break;
            }
            case InverterModelType::curveFuncOfPower: {
                SetupZoneInternalGain(zoneNum_,
                                      "ElectricLoadCenter:Inverter:FunctionOfPower",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,
                                      qdotConvZone_,
                                      _,
                                      qdotRadZone_);
                break;
            }
            case InverterModelType::cECLookUpTableModel: {
                SetupZoneInternalGain(zoneNum_,
                                      "ElectricLoadCenter:Inverter:LookUpTable",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,
                                      qdotConvZone_,
                                      _,
                                      qdotRadZone_);
                break;
            }
            case InverterModelType::notYetSet: {
                // do nothing
                break;
            }
            case InverterModelType::pvWatts: {
                break;
            }
            } // end switch modelType
        }
    } else {
        ShowSevereError(routineName + " did not find inverter name = " + objectName);
        errorsFound = true;
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Preceding errors terminate program.");
    }
}

void DCtoACInverter::reinitAtBeginEnvironment()
{
    ancillACuseRate_ = 0.0;
    ancillACuseEnergy_ = 0.0;
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

void DCtoACInverter::reinitZoneGainsAtBeginEnvironment()
{
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

void DCtoACInverter::setPVWattsDCCapacity(const Real64 dcCapacity)
{
    if (modelType_ != InverterModelType::pvWatts) {
        ShowFatalError("Setting the DC Capacity for the inverter only works with PVWatts Inverters.");
    }
    ratedPower_ = dcCapacity / pvWattsDCtoACSizeRatio_;
}

Real64 DCtoACInverter::pvWattsDCCapacity()
{
    return ratedPower_ * pvWattsDCtoACSizeRatio_;
}

Real64 DCtoACInverter::thermLossRate() const
{
    return thermLossRate_;
}

Real64 DCtoACInverter::aCPowerOut() const
{
    return aCPowerOut_;
}

Real64 DCtoACInverter::aCEnergyOut() const
{
    return aCEnergyOut_;
}

DCtoACInverter::InverterModelType DCtoACInverter::modelType() const
{
    return modelType_;
}

std::string const &DCtoACInverter::name() const
{
    return name_;
}

Real64 DCtoACInverter::getLossRateForOutputPower(Real64 const powerOutOfInverter)
{

    // need to invert, find a dCPowerIn that produces the desired AC power out
    // use last efficiency for initial guess
    if (efficiency_ > 0.0) {
        dCPowerIn_ = powerOutOfInverter / efficiency_;
    } else {
        dCPowerIn_ = powerOutOfInverter;
        calcEfficiency();
        dCPowerIn_ = powerOutOfInverter / efficiency_;
    }

    calcEfficiency();
    // one more update is close enough.
    if (efficiency_ > 0.0) {
        dCPowerIn_ = powerOutOfInverter / efficiency_;
    }
    calcEfficiency();
    return (1.0 - efficiency_) * dCPowerIn_;
}

void DCtoACInverter::calcEfficiency()
{
    switch (modelType_) {
    case InverterModelType::cECLookUpTableModel: {
        // we don't model voltage, so use nominal voltage
        Real64 normalizedPower = dCPowerIn_ / ratedPower_;

        // get efficiency
        if (normalizedPower <= 0.1) {
            // extrapolate or fix at 10% value? fix it for now
            efficiency_ = nomVoltEfficiencyARR_[0];
        } else if ((normalizedPower > 0.1) && (normalizedPower < 0.20)) {
            efficiency_ = nomVoltEfficiencyARR_[0] + ((normalizedPower - 0.1) / (0.2 - 0.1)) * (nomVoltEfficiencyARR_[1] - nomVoltEfficiencyARR_[0]);
        } else if (normalizedPower == 0.2) {
            efficiency_ = nomVoltEfficiencyARR_[1];
        } else if ((normalizedPower > 0.2) && (normalizedPower < 0.30)) {
            efficiency_ = nomVoltEfficiencyARR_[1] + ((normalizedPower - 0.2) / (0.3 - 0.2)) * (nomVoltEfficiencyARR_[2] - nomVoltEfficiencyARR_[1]);
        } else if (normalizedPower == 0.3) {
            efficiency_ = nomVoltEfficiencyARR_[2];
        } else if ((normalizedPower > 0.3) && (normalizedPower < 0.50)) {
            efficiency_ = nomVoltEfficiencyARR_[2] + ((normalizedPower - 0.3) / (0.5 - 0.3)) * (nomVoltEfficiencyARR_[3] - nomVoltEfficiencyARR_[2]);
        } else if (normalizedPower == 0.5) {
            efficiency_ = nomVoltEfficiencyARR_[3];
        } else if ((normalizedPower > 0.5) && (normalizedPower < 0.75)) {
            efficiency_ = nomVoltEfficiencyARR_[3] + ((normalizedPower - 0.5) / (0.75 - 0.5)) * (nomVoltEfficiencyARR_[4] - nomVoltEfficiencyARR_[3]);
        } else if (normalizedPower == 0.75) {
            efficiency_ = nomVoltEfficiencyARR_[4];
        } else if ((normalizedPower > 0.75) && (normalizedPower < 1.0)) {
            efficiency_ =
                nomVoltEfficiencyARR_[4] + ((normalizedPower - 0.75) / (1.0 - 0.75)) * (nomVoltEfficiencyARR_[5] - nomVoltEfficiencyARR_[4]);
        } else if (normalizedPower >= 1.0) {
            efficiency_ = nomVoltEfficiencyARR_[5];
        } else {
            assert(false);
        }

        efficiency_ = max(efficiency_, 0.0);
        efficiency_ = min(efficiency_, 1.0);

        break;
    }
    case InverterModelType::curveFuncOfPower: {

        Real64 normalizedPower = dCPowerIn_ / ratedPower_;
        efficiency_ = CurveManager::CurveValue(curveNum_, normalizedPower);
        efficiency_ = max(efficiency_, minEfficiency_);
        efficiency_ = min(efficiency_, maxEfficiency_);

        break;
    }
    case InverterModelType::pvWatts: {
        Real64 const etaref = 0.9637;
        Real64 const A = -0.0162;
        Real64 const B = -0.0059;
        Real64 const C = 0.9858;
        Real64 const pdc0 = ratedPower_ / pvWattsInverterEfficiency_;
        Real64 const plr = dCPowerIn_ / pdc0;
        Real64 ac = 0;

        if (plr > 0) {
            // normal operation
            Real64 eta = (A * plr + B / plr + C) * pvWattsInverterEfficiency_ / etaref;
            ac = dCPowerIn_ * eta;
            if (ac > ratedPower_) {
                // clipping
                ac = ratedPower_;
            }
            efficiency_ = ac / dCPowerIn_;
        } else {
            efficiency_ = 0.0;
        }
        break;
    }
    case InverterModelType::simpleConstantEff:
    case InverterModelType::notYetSet: {
        // do nothing
        break;
    }
    } // end switch
}

void DCtoACInverter::simulate(Real64 const powerIntoInverter)
{
    dCPowerIn_ = powerIntoInverter;
    dCEnergyIn_ = dCPowerIn_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    // check availability schedule
    if (ScheduleManager::GetCurrentScheduleValue(availSchedPtr_) > 0.0) {

        // now calculate Inverter based on model type
        calcEfficiency();
        aCPowerOut_ = efficiency_ * dCPowerIn_;
        aCEnergyOut_ = aCPowerOut_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);

        if (aCPowerOut_ == 0.0) {
            ancillACuseEnergy_ = standbyPower_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
            ancillACuseRate_ = standbyPower_;
        } else {
            ancillACuseRate_ = 0.0;
            ancillACuseEnergy_ = 0.0;
        }
    } else { // not available per schedule, inverter is dead.
        //  assume thermal shunt for DC in, but no standby electricity
        aCPowerOut_ = 0.0;
        aCEnergyOut_ = 0.0;
        ancillACuseRate_ = 0.0;
        ancillACuseEnergy_ = 0.0;
    }
    // update report variables
    conversionLossPower_ = dCPowerIn_ - aCPowerOut_;
    conversionLossEnergy_ = conversionLossPower_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    conversionLossEnergyDecrement_ = -1.0 * conversionLossEnergy_;
    thermLossRate_ = dCPowerIn_ - aCPowerOut_ + ancillACuseRate_;
    thermLossEnergy_ = thermLossRate_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    qdotConvZone_ = thermLossRate_ * (1.0 - zoneRadFract_);
    qdotRadZone_ = thermLossRate_ * zoneRadFract_;
}

ACtoDCConverter::ACtoDCConverter(std::string const &objectName)
    : efficiency_(0.0), aCPowerIn_(0.0), aCEnergyIn_(0.0), dCPowerOut_(0.0), dCEnergyOut_(0.0), conversionLossPower_(0.0), conversionLossEnergy_(0.0),
      conversionLossEnergyDecrement_(0.0), thermLossRate_(0.0), thermLossEnergy_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0), ancillACuseRate_(0.0),
      ancillACuseEnergy_(0.0), availSchedPtr_(0), modelType_(ConverterModelType::notYetSet),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0),
      zoneRadFract_(0.0), // radiative fraction for thermal losses to zone
      standbyPower_(0.0), maxPower_(0.0)
{

    std::string const routineName = "ACtoDCConverter constructor ";
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    // if/when add object class name to input object this can be simplified. for now search all possible types

    int testConvertIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Storage:Converter", objectName);

    if (testConvertIndex > 0) {
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Converter";

        inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      testConvertIndex,
                                      DataIPShortCuts::cAlphaArgs,
                                      NumAlphas,
                                      DataIPShortCuts::rNumericArgs,
                                      NumNums,
                                      IOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);

        name_ = DataIPShortCuts::cAlphaArgs(1);
        // need a new general approach for verify names are unique across objects,  next gen GlobalNames

        if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
            availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
        } else {
            availSchedPtr_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
            if (availSchedPtr_ == 0) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                errorsFound = true;
            }
        }

        if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "SimpleFixed")) {
            modelType_ = ConverterModelType::simpleConstantEff;
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "FunctionOfPower")) {
            modelType_ = ConverterModelType::curveFuncOfPower;
        } else {
            ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
            errorsFound = true;
        }

        switch (modelType_) {
        case ConverterModelType::simpleConstantEff: {
            efficiency_ = DataIPShortCuts::rNumericArgs(1);
            break;
        }

        case ConverterModelType::curveFuncOfPower: {
            maxPower_ = DataIPShortCuts::rNumericArgs(2);
            curveNum_ = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(4));
            if (curveNum_ == 0) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + DataIPShortCuts::cAlphaArgs(4));
                ShowContinueError("Curve was not found");
                errorsFound = true;
            }
            break;
        }
        case ConverterModelType::notYetSet: {
            // do nothing
        }
        } // end switch

        standbyPower_ = DataIPShortCuts::rNumericArgs(3);

        zoneNum_ = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(5), DataHeatBalance::Zone);
        if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
        if (zoneNum_ == 0) {
            if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            } else {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + DataIPShortCuts::cAlphaArgs(5));
                ShowContinueError("Zone name not found. Inverter heat losses will not be added to a zone");
                // continue with simulation but inverter losses not sent to a zone.
            }
        }
        zoneRadFract_ = DataIPShortCuts::rNumericArgs(4);

        SetupOutputVariable("Converter AC to DC Efficiency", OutputProcessor::Unit::None, efficiency_, "System", "Average", name_);
        SetupOutputVariable("Converter AC Input Electric Power", OutputProcessor::Unit::W, aCPowerIn_, "System", "Average", name_);
        SetupOutputVariable("Converter AC Input Electric Energy", OutputProcessor::Unit::J, aCEnergyIn_, "System", "Sum", name_);
        SetupOutputVariable("Converter DC Output Electric Power", OutputProcessor::Unit::W, dCPowerOut_, "System", "Average", name_);
        SetupOutputVariable("Converter DC Output Electric Energy", OutputProcessor::Unit::J, dCEnergyOut_, "System", "Sum", name_);
        SetupOutputVariable("Converter Electric Loss Power", OutputProcessor::Unit::W, conversionLossPower_, "System", "Average", name_);
        SetupOutputVariable("Converter Electric Loss Energy", OutputProcessor::Unit::J, conversionLossEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Converter Electric Loss Decrement Energy",
                            OutputProcessor::Unit::J,
                            conversionLossEnergyDecrement_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricityProduced",
                            "POWERCONVERSION",
                            _,
                            "Plant");
        SetupOutputVariable("Converter Thermal Loss Rate", OutputProcessor::Unit::W, thermLossRate_, "System", "Average", name_);
        SetupOutputVariable("Converter Thermal Loss Energy", OutputProcessor::Unit::J, thermLossEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Converter Ancillary AC Electric Power", OutputProcessor::Unit::W, ancillACuseRate_, "System", "Average", name_);
        SetupOutputVariable("Converter Ancillary AC Electric Energy",
                            OutputProcessor::Unit::J,
                            ancillACuseEnergy_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "Electricity",
                            "Cogeneration",
                            "ACtoDCConverter Ancillary",
                            "Plant"); // called cogeneration for end use table
        if (zoneNum_ > 0) {
            SetupZoneInternalGain(zoneNum_,
                                  "ElectricLoadCenter:Storage:Converter",
                                  name_,
                                  DataHeatBalance::IntGainTypeOf_ElectricLoadCenterConverter,
                                  qdotConvZone_,
                                  _,
                                  qdotRadZone_);
        }
    } else {
        ShowSevereError(routineName + " did not find power converter name = " + objectName);
        errorsFound = true;
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Preceding errors terminate program.");
    }
}

void ACtoDCConverter::reinitAtBeginEnvironment()
{
    ancillACuseRate_ = 0.0;
    ancillACuseEnergy_ = 0.0;
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

void ACtoDCConverter::reinitZoneGainsAtBeginEnvironment()
{
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

Real64 ACtoDCConverter::thermLossRate() const
{
    return thermLossRate_;
}

Real64 ACtoDCConverter::dCPowerOut() const
{
    return dCPowerOut_;
}

Real64 ACtoDCConverter::dCEnergyOut() const
{
    return dCEnergyOut_;
}

Real64 ACtoDCConverter::aCPowerIn() const
{
    return aCPowerIn_;
}

Real64 ACtoDCConverter::getLossRateForInputPower(Real64 const powerIntoConverter)
{
    aCPowerIn_ = powerIntoConverter;
    calcEfficiency();
    return (1.0 - efficiency_) * aCPowerIn_;
}

std::string const &ACtoDCConverter::name() const
{
    return name_;
}

void ACtoDCConverter::calcEfficiency()
{
    switch (modelType_) {
    case ConverterModelType::notYetSet:
    case ConverterModelType::simpleConstantEff: {
        break;
    }
    case ConverterModelType::curveFuncOfPower: {
        Real64 normalizedPower = aCPowerIn_ / maxPower_;
        efficiency_ = CurveManager::CurveValue(curveNum_, normalizedPower);
        break;
    }
    } // end switch
}

void ACtoDCConverter::simulate(Real64 const powerOutFromConverter)
{
    // need to invert, find an aCPowerIn that produces the desired DC power out

    // use last efficiency for initial guess
    if (ScheduleManager::GetCurrentScheduleValue(availSchedPtr_) > 0.0) {

        aCPowerIn_ = powerOutFromConverter / efficiency_;
        calcEfficiency(), aCPowerIn_ = powerOutFromConverter / efficiency_;
        calcEfficiency(),

            dCPowerOut_ = aCPowerIn_ * efficiency_;

        if (dCPowerOut_ == 0.0) {
            ancillACuseEnergy_ = standbyPower_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
            ancillACuseRate_ = standbyPower_;
        } else {
            ancillACuseRate_ = 0.0;
            ancillACuseEnergy_ = 0.0;
        }

    } else { // not available
        aCPowerIn_ = 0.0;
        dCPowerOut_ = 0.0;
        ancillACuseRate_ = 0.0;
        ancillACuseEnergy_ = 0.0;
    }

    // update and report
    aCEnergyIn_ = aCPowerIn_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    dCEnergyOut_ = dCPowerOut_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    conversionLossPower_ = aCPowerIn_ - dCPowerOut_;
    conversionLossEnergy_ = conversionLossPower_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    conversionLossEnergyDecrement_ = -1.0 * conversionLossEnergy_;
    thermLossRate_ = aCPowerIn_ - dCPowerOut_ + ancillACuseRate_;
    thermLossEnergy_ = thermLossRate_ * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    qdotConvZone_ = thermLossRate_ * (1.0 - zoneRadFract_);
    qdotRadZone_ = thermLossRate_ * zoneRadFract_;
}

ElectricStorage::ElectricStorage( // main constructor
    std::string const &objectName)
    : storedPower_(0.0), storedEnergy_(0.0), drawnPower_(0.0), drawnEnergy_(0.0), decrementedEnergyStored_(0.0), maxRainflowArrayBounds_(100),
      maxRainflowArrayInc_(100), myWarmUpFlag_(false), storageModelMode_(StorageModelType::storageTypeNotSet), availSchedPtr_(0),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0), zoneRadFract_(0.0), startingEnergyStored_(0.0),
      energeticEfficCharge_(0.0), energeticEfficDischarge_(0.0), maxPowerDraw_(0.0), maxPowerStore_(0.0), maxEnergyCapacity_(0.0), parallelNum_(0),
      seriesNum_(0), numBattery_(0), chargeCurveNum_(0), dischargeCurveNum_(0), cycleBinNum_(0), startingSOC_(0.0), maxAhCapacity_(0.0),
      availableFrac_(0.0), chargeConversionRate_(0.0), chargedOCV_(0.0), dischargedOCV_(0.0), internalR_(0.0), maxDischargeI_(0.0), cutoffV_(0.0),
      maxChargeRate_(0.0), lifeCalculation_(BatteyDegredationModelType::degredationNotSet), lifeCurveNum_(0), thisTimeStepStateOfCharge_(0.0),
      lastTimeStepStateOfCharge_(0.0), pelNeedFromStorage_(0.0), pelFromStorage_(0.0), pelIntoStorage_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0),
      timeElapsed_(0.0), thisTimeStepAvailable_(0.0), thisTimeStepBound_(0.0), lastTimeStepAvailable_(0.0), lastTimeStepBound_(0.0),
      lastTwoTimeStepAvailable_(0.0), lastTwoTimeStepBound_(0.0), count0_(0), electEnergyinStorage_(0.0), thermLossRate_(0.0), thermLossEnergy_(0.0),
      storageMode_(0), absoluteSOC_(0.0), fractionSOC_(0.0), batteryCurrent_(0.0), batteryVoltage_(0.0), batteryDamage_(0.0)
{

    std::string const routineName = "ElectricStorage constructor ";
    int numAlphas; // Number of elements in the alpha array
    int numNums;   // Number of elements in the numeric array
    int iOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    // if/when add object class name to input object this can be simplified. for now search all possible types
    bool foundStorage = false;
    int testStorageIndex = 0;
    int storageIDFObjectNum = 0;

    testStorageIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Storage:Simple", objectName);
    if (testStorageIndex > 0) {
        foundStorage = true;
        storageIDFObjectNum = testStorageIndex;
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Simple";
        storageModelMode_ = StorageModelType::simpleBucketStorage;
    }

    testStorageIndex = inputProcessor->getObjectItemNum("ElectricLoadCenter:Storage:Battery", objectName);
    if (testStorageIndex > 0) {
        foundStorage = true;
        storageIDFObjectNum = testStorageIndex;
        DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Storage:Battery";
        storageModelMode_ = StorageModelType::kiBaMBattery;
    }

    if (foundStorage) {
        inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      storageIDFObjectNum,
                                      DataIPShortCuts::cAlphaArgs,
                                      numAlphas,
                                      DataIPShortCuts::rNumericArgs,
                                      numNums,
                                      iOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);

        name_ = DataIPShortCuts::cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?

        if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
            availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
        } else {
            availSchedPtr_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
            if (availSchedPtr_ == 0) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                errorsFound = true;
            }
        }

        zoneNum_ = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(3), DataHeatBalance::Zone);
        if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
        if (zoneNum_ == 0) {
            if (DataIPShortCuts::lAlphaFieldBlanks(3)) {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            } else {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                ShowContinueError("Zone name not found. Storage heat losses will not be added to a zone");
                // continue with simulation but storage losses not sent to a zone.
            }
        }
        zoneRadFract_ = DataIPShortCuts::rNumericArgs(1);

        switch (storageModelMode_) {

        case StorageModelType::simpleBucketStorage: {
            energeticEfficCharge_ = DataIPShortCuts::rNumericArgs(2);
            energeticEfficDischarge_ = DataIPShortCuts::rNumericArgs(3);
            maxEnergyCapacity_ = DataIPShortCuts::rNumericArgs(4);
            maxPowerDraw_ = DataIPShortCuts::rNumericArgs(5);
            maxPowerStore_ = DataIPShortCuts::rNumericArgs(6);
            startingEnergyStored_ = DataIPShortCuts::rNumericArgs(7);
            SetupOutputVariable("Electric Storage Simple Charge State",
                                OutputProcessor::Unit::J,
                                electEnergyinStorage_,
                                "System",
                                "Average",
                                name_); // issue #4921
            break;
        }

        case StorageModelType::kiBaMBattery: {
            chargeCurveNum_ = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(4)); // voltage calculation for charging
            if (chargeCurveNum_ == 0 && !DataIPShortCuts::lAlphaFieldBlanks(4)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + '=' + DataIPShortCuts::cAlphaArgs(4));
                errorsFound = true;
            } else if (DataIPShortCuts::lAlphaFieldBlanks(4)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " cannot be blank. But no entry found.");
                errorsFound = true;
            } else {
                errorsFound |= CurveManager::CheckCurveDims(
                    chargeCurveNum_,   // Curve index
                    {1},               // Valid dimensions
                    routineName,       // Routine name
                    DataIPShortCuts::cCurrentModuleObject,  // Object Type
                    name_,             // Object Name
                    DataIPShortCuts::cAlphaFieldNames(4));  // Field Name
            }
            dischargeCurveNum_ = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(5)); // voltage calculation for discharging
            if (dischargeCurveNum_ == 0 && !DataIPShortCuts::lAlphaFieldBlanks(5)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + '=' + DataIPShortCuts::cAlphaArgs(5));
                errorsFound = true;
            } else if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + " cannot be blank. But no entry found.");
                errorsFound = true;
            } else {
                errorsFound |= CurveManager::CheckCurveDims(
                    dischargeCurveNum_,   // Curve index
                    {1},               // Valid dimensions
                    routineName,       // Routine name
                    DataIPShortCuts::cCurrentModuleObject,  // Object Type
                    name_,             // Object Name
                    DataIPShortCuts::cAlphaFieldNames(5));  // Field Name
            }

            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "Yes")) {
                lifeCalculation_ = BatteyDegredationModelType::lifeCalculationYes;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "No")) {
                lifeCalculation_ = BatteyDegredationModelType::lifeCalculationNo;
            } else {
                ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
                ShowContinueError("Yes or No should be selected. Default value No is used to continue simulation");
                lifeCalculation_ = BatteyDegredationModelType::lifeCalculationNo;
            }

            if (lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes) {
                lifeCurveNum_ = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(7)); // Battery life calculation
                if (lifeCurveNum_ == 0 && !DataIPShortCuts::lAlphaFieldBlanks(7)) {
                    ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) +
                                    "\", invalid entry.");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                    errorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(7)) {
                    ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) +
                                    "\", invalid entry.");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + " cannot be blank when " + DataIPShortCuts::cAlphaArgs(6) +
                                      " = Yes. But no entry found.");
                    errorsFound = true;
                } else {
                    errorsFound |= CurveManager::CheckCurveDims(
                        lifeCurveNum_,   // Curve index
                        {1},               // Valid dimensions
                        routineName,       // Routine name
                        DataIPShortCuts::cCurrentModuleObject,  // Object Type
                        name_,             // Object Name
                        DataIPShortCuts::cAlphaFieldNames(7));  // Field Name
                }

                cycleBinNum_ = DataIPShortCuts::rNumericArgs(14);

                if (!errorsFound) { // life cycle calculation for this battery, allocate arrays for degradation calculation
                                    // std::vector is zero base instead of 1, so first index is now 0.
                    b10_.resize(maxRainflowArrayBounds_ + 1, 0.0);
                    x0_.resize(maxRainflowArrayBounds_ + 1, 0);
                    nmb0_.resize(cycleBinNum_, 0.0);
                    oneNmb0_.resize(cycleBinNum_, 0.0);
                }
            }

            parallelNum_ = DataIPShortCuts::rNumericArgs(2);
            seriesNum_ = DataIPShortCuts::rNumericArgs(3);
            numBattery_ = parallelNum_ * seriesNum_;
            maxAhCapacity_ = DataIPShortCuts::rNumericArgs(4);
            startingSOC_ = DataIPShortCuts::rNumericArgs(5);
            availableFrac_ = DataIPShortCuts::rNumericArgs(6);
            chargeConversionRate_ = DataIPShortCuts::rNumericArgs(7);
            chargedOCV_ = DataIPShortCuts::rNumericArgs(8);
            dischargedOCV_ = DataIPShortCuts::rNumericArgs(9);
            internalR_ = DataIPShortCuts::rNumericArgs(10);
            maxDischargeI_ = DataIPShortCuts::rNumericArgs(11);
            cutoffV_ = DataIPShortCuts::rNumericArgs(12);
            maxChargeRate_ = DataIPShortCuts::rNumericArgs(13);

            SetupOutputVariable("Electric Storage Operating Mode Index", OutputProcessor::Unit::None, storageMode_, "System", "Average", name_);
            SetupOutputVariable("Electric Storage Battery Charge State",
                                OutputProcessor::Unit::Ah,
                                absoluteSOC_,
                                "System",
                                "Average",
                                name_); // issue #4921
            SetupOutputVariable("Electric Storage Charge Fraction", OutputProcessor::Unit::None, fractionSOC_, "System", "Average", name_);
            SetupOutputVariable("Electric Storage Total Current", OutputProcessor::Unit::A, batteryCurrent_, "System", "Average", name_);
            SetupOutputVariable("Electric Storage Total Voltage", OutputProcessor::Unit::V, batteryVoltage_, "System", "Average", name_);

            if (lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes) {
                SetupOutputVariable("Electric Storage Degradation Fraction", OutputProcessor::Unit::None, batteryDamage_, "System", "Average", name_);
            }
            break;
        }
        case StorageModelType::storageTypeNotSet: {
            // do nothing
            break;
        }

        } // switch storage model type

        SetupOutputVariable("Electric Storage Charge Power", OutputProcessor::Unit::W, storedPower_, "System", "Average", name_);
        SetupOutputVariable("Electric Storage Charge Energy", OutputProcessor::Unit::J, storedEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Electric Storage Production Decrement Energy",
                            OutputProcessor::Unit::J,
                            decrementedEnergyStored_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricityProduced",
                            "ELECTRICSTORAGE",
                            _,
                            "Plant");
        SetupOutputVariable("Electric Storage Discharge Power", OutputProcessor::Unit::W, drawnPower_, "System", "Average", name_);
        SetupOutputVariable("Electric Storage Discharge Energy",
                            OutputProcessor::Unit::J,
                            drawnEnergy_,
                            "System",
                            "Sum",
                            name_,
                            _,
                            "ElectricityProduced",
                            "ELECTRICSTORAGE",
                            _,
                            "Plant");
        SetupOutputVariable("Electric Storage Thermal Loss Rate", OutputProcessor::Unit::W, thermLossRate_, "System", "Average", name_);
        SetupOutputVariable("Electric Storage Thermal Loss Energy", OutputProcessor::Unit::J, thermLossEnergy_, "System", "Sum", name_);
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            if (storageModelMode_ == StorageModelType::simpleBucketStorage) {
                SetupEMSInternalVariable("Electrical Storage Simple Maximum Capacity", name_, "[J]", maxEnergyCapacity_);
            } else if (storageModelMode_ == StorageModelType::kiBaMBattery) {
                SetupEMSInternalVariable("Electrical Storage Battery Maximum Capacity", name_, "[Ah]", maxAhCapacity_);
            }
        }

        if (zoneNum_ > 0) {
            switch (storageModelMode_) {
            case StorageModelType::simpleBucketStorage: {
                SetupZoneInternalGain(zoneNum_,
                                      "ElectricLoadCenter:Storage:Simple",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageSimple,
                                      qdotConvZone_,
                                      _,
                                      qdotRadZone_);
                break;
            }
            case StorageModelType::kiBaMBattery: {
                SetupZoneInternalGain(zoneNum_,
                                      "ElectricLoadCenter:Storage:Battery",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageBattery,
                                      qdotConvZone_,
                                      _,
                                      qdotRadZone_);
                break;
            }
            case StorageModelType::storageTypeNotSet: {
                // do nothing
                break;
            }

            } // switch storage model type
        }
    } else { // storage not found
        ShowSevereError(routineName + " did not find storage name = " + objectName);
        errorsFound = true;
    }
    if (errorsFound) {
        ShowFatalError(routineName + "Preceding errors terminate program.");
    }
}

void ElectricStorage::reinitAtBeginEnvironment()
{
    pelNeedFromStorage_ = 0.0;
    pelFromStorage_ = 0.0;
    pelIntoStorage_ = 0.0;
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
    timeElapsed_ = 0.0;
    electEnergyinStorage_ = 0.0;
    storedPower_ = 0.0;
    storedEnergy_ = 0.0;
    decrementedEnergyStored_ = 0.0;
    drawnPower_ = 0.0;
    drawnEnergy_ = 0.0;
    thermLossRate_ = 0.0;
    thermLossEnergy_ = 0.0;
    lastTimeStepStateOfCharge_ = startingEnergyStored_;
    thisTimeStepStateOfCharge_ = startingEnergyStored_;

    if (storageModelMode_ == StorageModelType::kiBaMBattery) {
        Real64 initialCharge = maxAhCapacity_ * startingSOC_;
        lastTwoTimeStepAvailable_ = initialCharge * availableFrac_;
        lastTwoTimeStepBound_ = initialCharge * (1.0 - availableFrac_);
        lastTimeStepAvailable_ = initialCharge * availableFrac_;
        lastTimeStepBound_ = initialCharge * (1.0 - availableFrac_);
        thisTimeStepAvailable_ = initialCharge * availableFrac_;
        thisTimeStepBound_ = initialCharge * (1.0 - availableFrac_);
        if (lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes) {
            count0_ = 1;            // Index 0 is for initial SOC, so new input starts from index 1.
            b10_[0] = startingSOC_; // the initial fractional SOC is stored as the reference
            x0_[0] = 0.0;
            for (auto loop = 1; loop < maxRainflowArrayBounds_ + 1; ++loop) {
                b10_[loop] = 0.0;
                x0_[loop] = 0.0;
            }
            for (auto loop = 0; loop < cycleBinNum_; ++loop) {
                oneNmb0_[loop] = 0.0;
                nmb0_[loop] = 0.0;
            }
            batteryDamage_ = 0.0;
        }
    }
    myWarmUpFlag_ = true;
}

void ElectricStorage::reinitZoneGainsAtBeginEnvironment()
{
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

void ElectricStorage::reinitAtEndWarmup()
{
    // need to reset initial state of charge at beginning of environment but after warm up is complete
    lastTimeStepStateOfCharge_ = startingEnergyStored_;
    thisTimeStepStateOfCharge_ = startingEnergyStored_;
    if (storageModelMode_ == StorageModelType::kiBaMBattery) {
        Real64 initialCharge = maxAhCapacity_ * startingSOC_;
        lastTwoTimeStepAvailable_ = initialCharge * availableFrac_;
        lastTwoTimeStepBound_ = initialCharge * (1.0 - availableFrac_);
        lastTimeStepAvailable_ = initialCharge * availableFrac_;
        lastTimeStepBound_ = initialCharge * (1.0 - availableFrac_);
        thisTimeStepAvailable_ = initialCharge * availableFrac_;
        thisTimeStepBound_ = initialCharge * (1.0 - availableFrac_);
        if (lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes) {
            count0_ = 1;            // Index 0 is for initial SOC, so new input starts from index 1.
            b10_[0] = startingSOC_; // the initial fractional SOC is stored as the reference
            x0_[0] = 0.0;
            for (auto loop = 1; loop < maxRainflowArrayBounds_ + 1; ++loop) {
                b10_[loop] = 0.0;
                x0_[loop] = 0.0;
            }
            for (auto loop = 0; loop < cycleBinNum_; ++loop) {
                oneNmb0_[loop] = 0.0;
                nmb0_[loop] = 0.0;
            }
            batteryDamage_ = 0.0;
        }
    }
    myWarmUpFlag_ = false;
}

void ElectricStorage::timeCheckAndUpdate()
{

    if (myWarmUpFlag_ && !DataGlobals::WarmupFlag) {
        reinitAtEndWarmup();
    }

    Real64 timeElapsedLoc = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
    if (timeElapsed_ != timeElapsedLoc) { // time changed, update last with "current" result from previous time
        if (storageModelMode_ == StorageModelType::kiBaMBattery && lifeCalculation_ == BatteyDegredationModelType::lifeCalculationYes) {
            //    At this point, the current values, last time step values and last two time step values have not been updated, hence:
            //    "ThisTimeStep*" actually points to the previous one time step
            //    "LastTimeStep*" actually points to the previous two time steps
            //    "LastTwoTimeStep" actually points to the previous three time steps

            //      Calculate the fractional SOC change between the "current" time step and the "previous one" time step
            Real64 deltaSOC1 = thisTimeStepAvailable_ + thisTimeStepBound_ - lastTimeStepAvailable_ - lastTimeStepBound_;
            deltaSOC1 /= maxAhCapacity_;

            //      Calculate the fractional SOC change between the "previous one" time step and the "previous two" time steps
            Real64 deltaSOC2 = lastTimeStepAvailable_ + lastTimeStepBound_ - lastTwoTimeStepAvailable_ - lastTwoTimeStepBound_;
            deltaSOC2 /= maxAhCapacity_;

            //     DeltaSOC2 = 0 may occur at the begining of each simulation environment.
            //     DeltaSOC1 * DeltaSOC2 means that the SOC from "LastTimeStep" is a peak or valley. Only peak or valley needs
            //     to call the rain flow algorithm
            if ((deltaSOC2 == 0) || ((deltaSOC1 * deltaSOC2) < 0)) {
                //     Because we cannot determine whehter "ThisTimeStep" is a peak or valley (next time step is unknown yet), we
                //     use the "LastTimeStep" value for battery life calculation.
                Real64 input0 = (lastTimeStepAvailable_ + lastTimeStepBound_) / maxAhCapacity_;
                b10_[count0_] = input0;

                //        The arrary size needs to be increased when count = MaxRainflowArrayBounds. Please note that (MaxRainflowArrayBounds +1)
                //        is the index used in the subroutine RainFlow. So we cannot reallocate array size until count = MaxRainflowArrayBounds +1.
                if (count0_ == maxRainflowArrayBounds_) {
                    b10_.resize(maxRainflowArrayBounds_ + 1 + maxRainflowArrayInc_, 0.0);
                    x0_.resize(maxRainflowArrayBounds_ + 1 + maxRainflowArrayInc_, 0.0);
                    maxRainflowArrayBounds_ += maxRainflowArrayInc_;
                }

                rainflow(cycleBinNum_, input0, b10_, x0_, count0_, nmb0_, oneNmb0_);

                batteryDamage_ = 0.0;

                for (auto binNum = 0; binNum < cycleBinNum_; ++binNum) {
                    //       Battery damage is calculated by accumulating the impact from each cycle.
                    batteryDamage_ += oneNmb0_[binNum] / CurveManager::CurveValue(lifeCurveNum_, (double(binNum) / double(cycleBinNum_)));
                }
            }
        }

        lastTimeStepStateOfCharge_ = thisTimeStepStateOfCharge_;
        lastTwoTimeStepAvailable_ = lastTimeStepAvailable_;
        lastTwoTimeStepBound_ = lastTimeStepBound_;
        lastTimeStepAvailable_ = thisTimeStepAvailable_;
        lastTimeStepBound_ = thisTimeStepBound_;
        timeElapsed_ = timeElapsedLoc;

    } // end if time changed
}

void ElectricStorage::simulate(Real64 &powerCharge,
                               Real64 &powerDischarge,
                               bool &charging,
                               bool &discharging,
                               Real64 const controlSOCMaxFracLimit,
                               Real64 const controlSOCMinFracLimit)
{
    // pass thru to constrain function depending on storage model type
    if (ScheduleManager::GetCurrentScheduleValue(availSchedPtr_) == 0.0) { // storage not available
        discharging = false;
        powerDischarge = 0.0;
        charging = false;
        powerCharge = 0.0;
    }

    if (storageModelMode_ == StorageModelType::simpleBucketStorage) {
        simulateSimpleBucketModel(powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit, controlSOCMinFracLimit);
    } else if (storageModelMode_ == StorageModelType::kiBaMBattery) {
        simulateKineticBatteryModel(powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit, controlSOCMinFracLimit);
    }
}

std::string const &ElectricStorage::name() const
{
    return name_;
}

void ElectricStorage::simulateSimpleBucketModel(Real64 &powerCharge,
                                                Real64 &powerDischarge,
                                                bool &charging,
                                                bool &discharging,
                                                Real64 const controlSOCMaxFracLimit,
                                                Real64 const controlSOCMinFracLimit)
{
    // given arguments for how the storage operation would like to run storage charge or discharge
    // apply model constraints and adjust arguments accordingly

    if (charging) {

        if (lastTimeStepStateOfCharge_ >= (maxEnergyCapacity_ * controlSOCMaxFracLimit)) {
            // storage full!  no more allowed!
            powerCharge = 0.0;
            charging = false;
        }
        if (powerCharge > maxPowerStore_) {
            powerCharge = maxPowerStore_;
        }

        // now check to see if charge would exceed capacity, and modify to just fill physical storage cap
        if ((lastTimeStepStateOfCharge_ + powerCharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * energeticEfficCharge_) >=
            (maxEnergyCapacity_ * controlSOCMaxFracLimit)) {
            powerCharge = ((maxEnergyCapacity_ * controlSOCMaxFracLimit) - lastTimeStepStateOfCharge_) /
                          (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * energeticEfficCharge_);
        }
    } // charging

    if (discharging) {

        if (lastTimeStepStateOfCharge_ <= (maxEnergyCapacity_ * controlSOCMinFracLimit)) {
            // storage empty  no more allowed!
            powerDischarge = 0.0;
            discharging = false;
        }
        if (powerDischarge > maxPowerDraw_) {
            powerDischarge = maxPowerDraw_;
        }
        // now check if will empty this timestep, power draw is amplified by energetic effic
        if ((lastTimeStepStateOfCharge_ - powerDischarge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / energeticEfficDischarge_) <=
            (maxEnergyCapacity_ * controlSOCMinFracLimit)) {
            powerDischarge = (lastTimeStepStateOfCharge_ - (maxEnergyCapacity_ * controlSOCMinFracLimit)) * energeticEfficDischarge_ /
                             (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        }
    }

    if ((!charging) && (!discharging)) {
        thisTimeStepStateOfCharge_ = lastTimeStepStateOfCharge_;
        pelIntoStorage_ = 0.0;
        pelFromStorage_ = 0.0;
    }
    if (charging) {
        pelIntoStorage_ = powerCharge;
        pelFromStorage_ = 0.0;
        thisTimeStepStateOfCharge_ =
            lastTimeStepStateOfCharge_ + powerCharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * energeticEfficCharge_;
    }
    if (discharging) {
        pelIntoStorage_ = 0.0;
        pelFromStorage_ = powerDischarge;
        thisTimeStepStateOfCharge_ =
            lastTimeStepStateOfCharge_ - powerDischarge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / energeticEfficDischarge_;
        thisTimeStepStateOfCharge_ = max(thisTimeStepStateOfCharge_, 0.0);
    }

    // updates and reports
    electEnergyinStorage_ = thisTimeStepStateOfCharge_; //[J]
    storedPower_ = pelIntoStorage_;
    storedEnergy_ = pelIntoStorage_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    decrementedEnergyStored_ = -1.0 * storedEnergy_;
    drawnPower_ = pelFromStorage_;
    drawnEnergy_ = pelFromStorage_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    thermLossRate_ = max(storedPower_ * (1.0 - energeticEfficCharge_), drawnPower_ * (1.0 - energeticEfficDischarge_));
    thermLossEnergy_ = thermLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = (1.0 - zoneRadFract_) * thermLossRate_;
        qdotRadZone_ = (zoneRadFract_)*thermLossRate_;
    }
}

void ElectricStorage::simulateKineticBatteryModel(Real64 &powerCharge,
                                                  Real64 &powerDischarge,
                                                  bool &charging,
                                                  bool &discharging,
                                                  Real64 const controlSOCMaxFracLimit,
                                                  Real64 const controlSOCMinFracLimit)
{

    // initialize locals
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

    qmax = maxAhCapacity_;
    E0c = chargedOCV_;
    E0d = dischargedOCV_;
    k = chargeConversionRate_;
    c = availableFrac_;

    if (charging) {

        //*************************************************
        // The sign of power and current is negative in charging
        //*************************************************
        Real64 Pw = -powerCharge / numBattery_;
        q0 = lastTimeStepAvailable_ + lastTimeStepBound_;
        if (q0 > qmax * controlSOCMaxFracLimit) {
            // stop charging with controller signal for max state of charge
            Pw = 0.0;
            powerCharge = 0.0;
            charging = false;
            storageMode_ = 0;
            storedPower_ = 0.0;
            storedEnergy_ = 0.0;
            decrementedEnergyStored_ = 0.0;
            drawnPower_ = 0.0;
            drawnEnergy_ = 0.0;
            return;
        }

        I0 = 1.0;                                                                                       // Initial assumption
        T0 = std::abs(qmax / I0);                                                                       // Initial Assumption
        qmaxf = qmax * k * c * T0 / (1.0 - std::exp(-k * T0) + c * (k * T0 - 1.0 + std::exp(-k * T0))); // Initial calculation of a function qmax(I)
        Real64 Xf = q0 / qmaxf;
        Ef = E0d + CurveManager::CurveValue(chargeCurveNum_, Xf); // E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
        Volt = Ef - I0 * internalR_;
        Real64 Inew = 0.0;
        if (Volt != 0.0) {
            Inew = Pw / Volt;
        }
        Real64 Tnew = 0.0;
        if (Inew != 0.0) {
            Tnew = qmaxf / std::abs(Inew);
        }
        Real64 error = 1.0;

        while (error > 0.0001) { // Iteration process to get converged current(I)
            I0 = Inew;
            T0 = Tnew;
            qmaxf = qmax * k * c * T0 / (1.0 - std::exp(-k * T0) + c * (k * T0 - 1.0 + std::exp(-k * T0)));
            Xf = q0 / qmaxf;
            Ef = E0d + CurveManager::CurveValue(chargeCurveNum_, Xf); // E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
            Volt = Ef - I0 * internalR_;
            Inew = Pw / Volt;
            Tnew = std::abs(qmaxf / Inew); // ***Always positive here
            error = std::abs(Inew - I0);
        }

        Real64 dividend = -k * c * qmax + k * lastTimeStepAvailable_ * std::exp(-k * DataHVACGlobals::TimeStepSys) +
                          q0 * k * c * (1.0 - std::exp(-k * DataHVACGlobals::TimeStepSys));
        Real64 divisor = 1.0 - std::exp(-k * DataHVACGlobals::TimeStepSys) +
                         c * (k * DataHVACGlobals::TimeStepSys - 1 + std::exp(-k * DataHVACGlobals::TimeStepSys));
        Real64 Imax = dividend / divisor;
        // Below: This is the limit of charging current from Charge Rate Limit (input)
        Imax = max(Imax, -(qmax - q0) * maxChargeRate_);

        if (std::abs(I0) <= std::abs(Imax)) {
            I0 = Pw / Volt;
            Pactual = I0 * Volt;
        } else {
            I0 = Imax;
            qmaxf = 80.0; // Initial assumption to solve the equation using iterative method
            error = 10.0; // Initial assumption ...
            while (error > 0.001) {
                // *** I0(current) should be positive for this calculation
                Real64 RHS = (qmax * k * c * qmaxf / std::abs(I0)) /
                             (1.0 - std::exp(-k * qmaxf / std::abs(I0)) + c * (k * qmaxf / std::abs(I0) - 1.0 + std::exp(-k * qmaxf / std::abs(I0))));
                error = std::abs(qmaxf - RHS);
                qmaxf = RHS;
            }
        }
    }

    if (discharging) {
        //**********************************************
        // The sign of power and current is positive in discharging
        //**********************************************

        Real64 Pw = powerDischarge / numBattery_;
        q0 = lastTimeStepAvailable_ + lastTimeStepBound_;

        if (q0 < qmax * controlSOCMinFracLimit) {
            // stop discharging with controller signal for min state of charge
            Pw = 0.0;
            discharging = false;
            powerDischarge = 0.0;
            storageMode_ = 0;
            storedPower_ = 0.0;
            storedEnergy_ = 0.0;
            decrementedEnergyStored_ = 0.0;
            drawnPower_ = 0.0;
            drawnEnergy_ = 0.0;
            return;
        }

        bool const ok = determineCurrentForBatteryDischarge(I0, T0, Volt, Pw, q0, dischargeCurveNum_, k, c, qmax, E0c, internalR_);
        if (!ok) {
            ShowFatalError("ElectricLoadCenter:Storage:Battery named=\"" + name_ +
                           "\". Battery discharge current could not be estimated due to iteration limit reached. ");
            // issue #5301, need more diagnostics for this.
        }

        Real64 dividend = k * lastTimeStepAvailable_ * std::exp(-k * DataHVACGlobals::TimeStepSys) +
                          q0 * k * c * (1.0 - std::exp(-k * DataHVACGlobals::TimeStepSys));
        Real64 divisor = 1.0 - std::exp(-k * DataHVACGlobals::TimeStepSys) +
                         c * (k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp(-k * DataHVACGlobals::TimeStepSys));
        Real64 Imax = dividend / divisor;
        Imax = min(Imax, maxDischargeI_);
        if (std::abs(I0) <= Imax) {
            I0 = Pw / Volt;
            Pactual = I0 * Volt;
        } else {
            I0 = Imax;
            qmaxf = 10.0;        // Initial assumption to solve the equation using iterative method
            Real64 error = 10.0; // Initial assumption ...
            while (error > 0.001) {
                Real64 RHS = (qmax * k * c * qmaxf / I0) / (1.0 - std::exp(-k * qmaxf / I0) + c * (k * qmaxf / I0 - 1 + std::exp(-k * qmaxf / I0)));
                error = std::abs(qmaxf - RHS);
                qmaxf = RHS;
            }
            Real64 Xf = (qmax - q0) / qmaxf;
            Ef = E0c + CurveManager::CurveValue(dischargeCurveNum_, Xf);
            Volt = Ef - I0 * internalR_;
        }
        if (Volt < cutoffV_) {
            I0 = 0.0;
        }
    } // if discharging

    if ((!charging) && (!discharging)) {
        thisTimeStepAvailable_ = lastTimeStepAvailable_;
        thisTimeStepBound_ = lastTimeStepBound_;
        I0 = 0.0;
        Volt = 0.0;
        q0 = lastTimeStepAvailable_ + lastTimeStepBound_;
    } else {
        Real64 newAvailable = lastTimeStepAvailable_ * std::exp(-k * DataHVACGlobals::TimeStepSys) +
                              (q0 * k * c - I0) * (1.0 - std::exp(-k * DataHVACGlobals::TimeStepSys)) / k -
                              I0 * c * (k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp(-k * DataHVACGlobals::TimeStepSys)) / k;
        Real64 newBound = lastTimeStepBound_ * std::exp(-k * DataHVACGlobals::TimeStepSys) +
                          q0 * (1.0 - c) * (1.0 - std::exp(-k * DataHVACGlobals::TimeStepSys)) -
                          I0 * (1.0 - c) * (k * DataHVACGlobals::TimeStepSys - 1.0 + std::exp(-k * DataHVACGlobals::TimeStepSys)) / k;
        thisTimeStepAvailable_ = max(0.0, newAvailable);
        thisTimeStepBound_ = max(0.0, newBound);
    }

    Pactual = I0 * Volt;
    Real64 TotalSOC = thisTimeStepAvailable_ + thisTimeStepBound_;

    // output1
    if (TotalSOC > q0) {
        storageMode_ = 2;
        storedPower_ = -1.0 * Volt * I0 * numBattery_; // Issue #5303, fix sign issue
        storedEnergy_ = -1.0 * Volt * I0 * numBattery_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        decrementedEnergyStored_ = -1.0 * storedEnergy_;
        drawnPower_ = 0.0;
        drawnEnergy_ = 0.0;

    } else if (TotalSOC < q0) {
        storageMode_ = 1;
        storedPower_ = 0.0;
        storedEnergy_ = 0.0;
        decrementedEnergyStored_ = 0.0;
        drawnPower_ = Volt * I0 * numBattery_;
        drawnEnergy_ = Volt * I0 * numBattery_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    } else {
        storageMode_ = 0;
        storedPower_ = 0.0;
        storedEnergy_ = 0.0;
        decrementedEnergyStored_ = 0.0;
        drawnPower_ = 0.0;
        drawnEnergy_ = 0.0;
    }

    absoluteSOC_ = TotalSOC * numBattery_;
    fractionSOC_ = TotalSOC / qmax;
    batteryCurrent_ = I0 * parallelNum_;
    batteryVoltage_ = Volt * seriesNum_;
    thermLossRate_ = internalR_ * pow_2(I0) * numBattery_;
    thermLossEnergy_ = internalR_ * pow_2(I0) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * numBattery_;

    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = ((1.0 - zoneRadFract_) * thermLossRate_) * numBattery_;
        qdotRadZone_ = ((zoneRadFract_)*thermLossRate_) * numBattery_;
    }

    powerCharge = storedPower_;
    powerDischarge = drawnPower_;
}

Real64 ElectricStorage::drawnPower() const
{
    return drawnPower_;
}

Real64 ElectricStorage::storedPower() const
{
    return storedPower_;
}

Real64 ElectricStorage::drawnEnergy() const
{
    return drawnEnergy_;
}

Real64 ElectricStorage::storedEnergy() const
{
    return storedEnergy_;
}

bool ElectricStorage::determineCurrentForBatteryDischarge(Real64 &curI0,
                                                          Real64 &curT0,
                                                          Real64 &curVolt,
                                                          Real64 const Pw, // Power withdraw from each module,
                                                          Real64 const q0, // available charge last timestep, sum of available and bound
                                                          int const CurveNum,
                                                          Real64 const k,
                                                          Real64 const c,
                                                          Real64 const qmax,
                                                          Real64 const E0c,
                                                          Real64 const InternalR)
{
    curI0 = 10.0;         // Initial assumption
    curT0 = qmax / curI0; // Initial Assumption
    Real64 qmaxf = qmax * k * c * curT0 /
                   (1.0 - std::exp(-k * curT0) + c * (k * curT0 - 1.0 + std::exp(-k * curT0))); // Initial calculation of a function qmax(I)
    Real64 Xf = (qmax - q0) / qmaxf;
    Real64 Ef = E0c + CurveManager::CurveValue(CurveNum, Xf); // E0d+Ac*Xf+Cc*X/(Dc-Xf)
    curVolt = Ef - curI0 * InternalR;
    Real64 Inew = Pw / curVolt;
    Real64 Tnew = qmaxf / Inew;
    Real64 error = 1.0;
    int countForIteration = 0;
    bool exceedIterationLimit = false;

    while (error > 0.0001) { // Iteration process to get converged current(I)
        curI0 = Inew;
        curT0 = Tnew;
        qmaxf = qmax * k * c * curT0 / (1.0 - std::exp(-k * curT0) + c * (k * curT0 - 1.0 + std::exp(-k * curT0)));
        // add div by zero protection #5301
        if (qmaxf != 0.0) {
            Xf = (qmax - q0) / qmaxf;
        } else {
            Xf = 1.0;
        }

        Ef = E0c + CurveManager::CurveValue(CurveNum, Xf); // E0c+Ad*Xf+Cd*X/(Dd-Xf)
        curVolt = Ef - curI0 * InternalR;
        // add div by zero protection #5301
        if (curVolt != 0.0) {
            Inew = Pw / curVolt;
        } else {
            Inew = 1.0;
        }

        // add div by zero protection #5301
        if (Inew != 0.0) {
            Tnew = qmaxf / Inew;
        } else {
            Tnew = 1.0;
        }

        error = std::abs(Inew - curI0);
        ++countForIteration;
        if (countForIteration > 1000) {
            exceedIterationLimit = true;
            // Issue #5301 need more diagnostics for this case
            ShowWarningError(
                "ElectricStorage::determineCurrentForBatteryDischarge, iteration limit exceeded, failed to solve for discharge current.");
            ShowContinueError("Last timestep charge available, q0 = " + General::RoundSigDigits(q0, 5));
            ShowContinueError("New Current, Inew = " + General::RoundSigDigits(Inew, 5) + " [Amps]");
            ShowContinueError("Power discharge per module cell, Pw = " + General::RoundSigDigits(Pw, 5) + " ");
            ShowContinueError("Charge Conversion Rate, [1/h] change rate from bound charge energy to available charge, parameter k = " +
                              General::RoundSigDigits(k, 5));
            ShowContinueError("parameter c = " + General::RoundSigDigits(c, 5));
            ShowContinueError("parameter qmax = " + General::RoundSigDigits(qmax, 5));
            ShowContinueError("Fully charged open circuit voltage, parameter E0c  = " + General::RoundSigDigits(E0c, 5));
            ShowContinueError("parameter InternalR = " + General::RoundSigDigits(InternalR, 5));
            if (qmaxf == 0.0) {
                ShowContinueError("qmaxf was zero, would have divided by zero.");
            }
            if (Inew == 0.0) {
                ShowContinueError("Inew was zero, would have divided by zero. ");
            }
            if (curVolt == 0.0) {
                ShowContinueError("curVolt was zero, would have divided by zero. ");
            }

            ShowContinueErrorTimeStamp("ElectricStorage::determineCurrentForBatteryDischarge ");
            break;
        }
    }
    return (!exceedIterationLimit);
}

void ElectricStorage::rainflow(int const numbin,           // numbin = constant value
                               Real64 const input,         // input = input value from other object (battery model)
                               std::vector<Real64> &B1,    // stores values of points, calculated here - stored for next timestep
                               std::vector<Real64> &X,     // stores values of two data point difference, calculated here - stored for next timestep
                               int &count,                 // calculated here - stored for next timestep in main loop
                               std::vector<Real64> &Nmb,   // calculated here - stored for next timestep in main loop
                               std::vector<Real64> &OneNmb // calculated here - stored for next timestep in main loop
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

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // Array B1 stores the value of points
    // Array X stores the value of two data points' difference.

    int num;

    X[count] = input - B1[count - 1]; // calculate the difference between two data (current and previous)

    // Get rid of the data if it is not peak nor valley
    // The value of count means the number of peak or valley points added to the arrary B10/B1, not including the
    // first point B10(0)/B1(0). Therefore, even if count =2, B1(count-2) is still valid.
    if (count >= 3) {
        //  The following check on peak or valley may be not necessary in most times because the same check is made in the
        //  upper-level subroutine. However, it does not hurt to leave it here.
        if (X[count] * X[count - 1] >= 0) {
            X[count - 1] = B1[count] - B1[count - 2];
            shift(B1, count - 1, count, B1); // Get rid of (count-1) row in B1
            shift(X, count, count, X);
            --count; // If the value keep increasing or decreasing, get rid of the middle point.
        }            // Only valley and peak will be stored in the matrix, B1

        if ((count == 3) && (std::abs(X[2]) <= std::abs(X[3]))) {
            //  This means the starting point is included in X(2), a half cycle is counted according to the rain flow
            //  algorithm specified in the reference (Ariduru S. 2004)
            num = nint((std::abs(X[2]) * numbin * 10 + 5) / 10); // Count half cycle
            Nmb[num] += 0.5;
            // B1 = eoshift( B1, 1 ); // Once counting a half cycle, get rid of the value.
            B1.erase(B1.begin());
            B1.push_back(0.0);
            // X = eoshift( X, 1 );
            X.erase(X.begin());
            X.push_back(0.0);
            --count; // The number of matrix, B1 and X1 decrease.
        }
    } // Counting cyle end
    //*** Note: The value of "count" changes in the upper "IF LOOP"

    if (count >= 4) { // count 1 cycle
        while (std::abs(X[count]) > std::abs(X[count - 1])) {
            //  This means that the starting point is not included in X(count-1). a cycle is counted according to the rain flow
            //  algorithm specified in the reference (Ariduru S. 2004)
            num = nint((std::abs(X[count - 1]) * numbin * 10 + 5) / 10);
            ++Nmb[num];

            //     X(count-2) = ABS(X(count))-ABS(X(count-1))+ABS(X(count-2))
            X[count - 2] = B1[count] - B1[count - 3]; // Updating X needs to be done before shift operation below

            shift(B1, count - 1, count, B1); // Get rid of two data points one by one
            shift(B1, count - 2, count, B1); // Delete one point

            shift(X, count, count, X);     // Get rid of two data points one by one
            shift(X, count - 1, count, X); // Delete one point

            count -= 2;           // If one cycle is counted, two data points are deleted.
            if (count < 4) break; // When only three data points exists, one cycle cannot be counted.
        }
    }

    ++count;

    // Check the rest of the half cycles every time step
    OneNmb = Nmb; // Array Nmb (Bins) will be used for the next time step later.
                  // OneNmb is used to show the current output only.
}

void ElectricStorage::shift(std::vector<Real64> &A, int const m, int const n, std::vector<Real64> &B)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Y. KyungTae & W. Wang
    //       DATE WRITTEN   July-August, 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Utility subroutine for rainflow cycle counting

    int ShiftNum; // Loop variable

    for (ShiftNum = 1; ShiftNum <= m - 1; ++ShiftNum) {
        B[ShiftNum] = A[ShiftNum];
    }

    for (ShiftNum = m; ShiftNum <= n; ++ShiftNum) {
        B[ShiftNum] = A[ShiftNum + 1];
    }
}

// constructor
ElectricTransformer::ElectricTransformer(std::string const &objectName)
    : myOneTimeFlag_(true), availSchedPtr_(0), usageMode_(TransformerUse::usenotYetSet),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0), zoneRadFrac_(0.0), ratedCapacity_(0.0), phase_(0),
      factorTempCoeff_(0.0), tempRise_(0.0), eddyFrac_(0.0), performanceInputMode_(TransformerPerformanceInput::perfInputMethodNotSet),
      ratedEfficiency_(0.0), ratedPUL_(0.0), ratedTemp_(0.0), maxPUL_(0.0), considerLosses_(true), ratedNL_(0.0), ratedLL_(0.0),
      overloadErrorIndex_(0), efficiency_(0.0), powerIn_(0.0), energyIn_(0.0), powerOut_(0.0), energyOut_(0.0), noLoadLossRate_(0.0),
      noLoadLossEnergy_(0.0), loadLossRate_(0.0), loadLossEnergy_(0.0), thermalLossRate_(0.0), thermalLossEnergy_(0.0),
      elecUseMeteredUtilityLosses_(0.0), powerConversionMeteredLosses_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0)
{
    std::string const routineName = "ElectricTransformer constructor ";
    int numAlphas; // Number of elements in the alpha array
    int numNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    int transformerIDFObjectNum = 0;
    DataIPShortCuts::cCurrentModuleObject = "ElectricLoadCenter:Transformer";

    transformerIDFObjectNum = inputProcessor->getObjectItemNum("ElectricLoadCenter:Transformer", objectName);
    if (transformerIDFObjectNum > 0) {
        inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      transformerIDFObjectNum,
                                      DataIPShortCuts::cAlphaArgs,
                                      numAlphas,
                                      DataIPShortCuts::rNumericArgs,
                                      numNums,
                                      IOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);
        name_ = DataIPShortCuts::cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?
        if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
            availSchedPtr_ = DataGlobals::ScheduleAlwaysOn;
        } else {
            availSchedPtr_ = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
            if (availSchedPtr_ == 0) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                errorsFound = true;
            }
        }

        if (DataIPShortCuts::lAlphaFieldBlanks(3)) {
            usageMode_ = TransformerUse::powerInFromGrid; // default
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "PowerInFromGrid")) {
            usageMode_ = TransformerUse::powerInFromGrid;
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "PowerOutToGrid")) {
            usageMode_ = TransformerUse::powerOutFromBldgToGrid;
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(3), "LoadCenterPowerConditioning")) {
            usageMode_ = TransformerUse::powerBetweenLoadCenterAndBldg;

        } else {
            ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
            errorsFound = true;
        }

        zoneNum_ = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(4), DataHeatBalance::Zone);
        if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
        if (zoneNum_ == 0) {
            if (DataIPShortCuts::lAlphaFieldBlanks(4)) {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            } else {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + DataIPShortCuts::cAlphaArgs(4));
                ShowContinueError("Zone name not found. Transformer heat losses will not be added to a zone");
                // continue with simulation but storage losses not sent to a zone.
            }
        }
        zoneRadFrac_ = DataIPShortCuts::rNumericArgs(1);
        ratedCapacity_ = DataIPShortCuts::rNumericArgs(2);
        phase_ = DataIPShortCuts::rNumericArgs(3);

        if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(5), "Copper")) {
            factorTempCoeff_ = 234.5;
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(5), "Aluminum")) {
            factorTempCoeff_ = 225.0;
        } else {
            ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + DataIPShortCuts::cAlphaArgs(5));
            errorsFound = true;
        }
        tempRise_ = DataIPShortCuts::rNumericArgs(4);
        eddyFrac_ = DataIPShortCuts::rNumericArgs(5);

        if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "RatedLosses")) {
            performanceInputMode_ = TransformerPerformanceInput::lossesMethod;
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(6), "NominalEfficiency")) {
            performanceInputMode_ = TransformerPerformanceInput::efficiencyMethod;
        } else {
            ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
            errorsFound = true;
        }
        if (ratedCapacity_ == 0) {
            if (performanceInputMode_ == TransformerPerformanceInput::lossesMethod) {
                ShowWarningError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\".");
                ShowContinueError("Specified " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
                ShowContinueError("Specified " + DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(ratedCapacity_, 1));
                ShowContinueError("Transformer load and no load losses cannot be calculated with 0.0 rated capacity.");
                ShowContinueError("Simulation continues but transformer losses will be set to zero.");
            }
        }
        ratedNL_ = DataIPShortCuts::rNumericArgs(6);
        ratedLL_ = DataIPShortCuts::rNumericArgs(7);
        ratedEfficiency_ = DataIPShortCuts::rNumericArgs(8);
        ratedPUL_ = DataIPShortCuts::rNumericArgs(9);
        ratedTemp_ = DataIPShortCuts::rNumericArgs(10);
        maxPUL_ = DataIPShortCuts::rNumericArgs(11);
        // Check the input for MaxPUL if the performance input method is EfficiencyMethod
        if (performanceInputMode_ == TransformerPerformanceInput::efficiencyMethod) {
            if (DataIPShortCuts::lNumericFieldBlanks(11)) {
                maxPUL_ = ratedPUL_;
            } else if (maxPUL_ <= 0 || maxPUL_ > 1) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(11) + "=[" +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(11), 3) + "].");
                ShowContinueError("Entered value must be > 0 and <= 1.");
                errorsFound = true;
            }
        }
        if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(7), "Yes")) {
            considerLosses_ = true;
        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(7), "No")) {
            considerLosses_ = false;
        } else {
            if (usageMode_ == TransformerUse::powerInFromGrid) {
                ShowSevereError(routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + " = " + DataIPShortCuts::cAlphaArgs(7));
                errorsFound = true;
            }
        }

        int numAlphaBeforeMeter = 7;
        int numWiredMeters = numAlphas - numAlphaBeforeMeter;

        if (usageMode_ == TransformerUse::powerInFromGrid) {

            // Provide warning if no meter is wired to a transformer used to get power from the grid
            if (numWiredMeters <= 0) {
                ShowWarningError(routineName + "ElectricLoadCenter:Transformer=\"" + name_ + "\":");
                ShowContinueError("ISOLATED Transformer: No meter wired to a transformer used to input power from grid");
            }

            wiredMeterNames_.resize(numWiredMeters, "");
            wiredMeterPtrs_.resize(numWiredMeters, 0);
            specialMeter_.resize(numWiredMeters, false);

            // Meter check deferred because they may have not been "loaded" yet,
            for (auto loopCount = 0; loopCount < numWiredMeters; ++loopCount) {
                wiredMeterNames_[loopCount] = UtilityRoutines::MakeUPPERCase(DataIPShortCuts::cAlphaArgs(loopCount + numAlphaBeforeMeter + 1));
                // Assign SpecialMeter as TRUE if the meter name is Electricity:Facility or Electricity:HVAC
                if (UtilityRoutines::SameString(wiredMeterNames_[loopCount], "Electricity:Facility") ||
                    UtilityRoutines::SameString(wiredMeterNames_[loopCount], "Electricity:HVAC")) {
                    specialMeter_[loopCount] = true;
                } else {
                    specialMeter_[loopCount] = false;
                }
            }
        }
        SetupOutputVariable("Transformer Efficiency", OutputProcessor::Unit::None, efficiency_, "System", "Average", name_);
        SetupOutputVariable("Transformer Input Electric Power", OutputProcessor::Unit::W, powerIn_, "System", "Average", name_);
        SetupOutputVariable("Transformer Input Electric Energy", OutputProcessor::Unit::J, energyIn_, "System", "Sum", name_);
        SetupOutputVariable("Transformer Output Electric Power", OutputProcessor::Unit::W, powerOut_, "System", "Average", name_);
        SetupOutputVariable("Transformer Output Electric Energy", OutputProcessor::Unit::J, energyOut_, "System", "Sum", name_);
        SetupOutputVariable("Transformer No Load Loss Rate", OutputProcessor::Unit::W, noLoadLossRate_, "System", "Average", name_);
        SetupOutputVariable("Transformer No Load Loss Energy", OutputProcessor::Unit::J, noLoadLossEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Transformer Load Loss Rate", OutputProcessor::Unit::W, loadLossRate_, "System", "Average", name_);
        SetupOutputVariable("Transformer Load Loss Energy", OutputProcessor::Unit::J, loadLossEnergy_, "System", "Sum", name_);
        SetupOutputVariable("Transformer Thermal Loss Rate", OutputProcessor::Unit::W, thermalLossRate_, "System", "Average", name_);
        SetupOutputVariable("Transformer Thermal Loss Energy", OutputProcessor::Unit::J, thermalLossEnergy_, "System", "Sum", name_);
        if (usageMode_ == TransformerUse::powerInFromGrid) { // power losses metered as an end use exterior equipment
            SetupOutputVariable("Transformer Distribution Electric Loss Energy",
                                OutputProcessor::Unit::J,
                                elecUseMeteredUtilityLosses_,
                                "System",
                                "Sum",
                                name_,
                                _,
                                "Electricity",
                                "ExteriorEquipment",
                                "Transformer",
                                "System");
        }
        if (usageMode_ == TransformerUse::powerOutFromBldgToGrid) {
            SetupOutputVariable("Transformer Cogeneration Electric Loss Energy",
                                OutputProcessor::Unit::J,
                                powerConversionMeteredLosses_,
                                "System",
                                "Sum",
                                name_,
                                _,
                                "ElectricityProduced",
                                "POWERCONVERSION",
                                _,
                                "System");
        }
        if (usageMode_ == TransformerUse::powerBetweenLoadCenterAndBldg) {
            SetupOutputVariable("Transformer Conversion Electric Loss Energy",
                                OutputProcessor::Unit::J,
                                powerConversionMeteredLosses_,
                                "System",
                                "Sum",
                                name_,
                                _,
                                "ElectricityProduced",
                                "POWERCONVERSION",
                                _,
                                "System");
        }

        if (zoneNum_ > 0) {
            SetupZoneInternalGain(zoneNum_,
                                  "ElectricLoadCenter:Transformer",
                                  name_,
                                  DataHeatBalance::IntGainTypeOf_ElectricLoadCenterTransformer,
                                  qdotConvZone_,
                                  _,
                                  qdotRadZone_);
        }

    } else {
        ShowSevereError(routineName + " did not find transformer name = " + objectName);
        errorsFound = true;
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Preceding errors terminate program.");
    }
}

Real64 ElectricTransformer::getLossRateForOutputPower(Real64 const powerOutOfTransformer)
{
    manageTransformers(powerOutOfTransformer);
    return totalLossRate_;
}

Real64 ElectricTransformer::getLossRateForInputPower(Real64 const powerIntoTransformer)
{
    manageTransformers(powerIntoTransformer);
    return totalLossRate_;
}

void ElectricTransformer::manageTransformers(Real64 const surplusPowerOutFromLoadCenters)
{
    Real64 const ambTempRef = 20.0; // reference ambient temperature (C)
    if (myOneTimeFlag_) {
        // calculate rated no load losses and rated load losses if the performance input method is based on
        // nominal efficiency. This calculation is done only once

        if (performanceInputMode_ == TransformerPerformanceInput::efficiencyMethod) {

            Real64 resRef = factorTempCoeff_ + tempRise_ + ambTempRef;
            Real64 resSpecified = factorTempCoeff_ + ratedTemp_;
            Real64 resRatio = resSpecified / resRef;
            Real64 factorTempCorr = (1.0 - eddyFrac_) * resRatio + eddyFrac_ * (1.0 / resRatio);
            Real64 numerator = ratedCapacity_ * ratedPUL_ * (1.0 - ratedEfficiency_);
            Real64 denominator = ratedEfficiency_ * (1.0 + pow_2(ratedPUL_ / maxPUL_));

            ratedNL_ = numerator / denominator;
            ratedLL_ = ratedNL_ / (factorTempCorr * pow_2(maxPUL_));
        }
        myOneTimeFlag_ = false;
    }

    Real64 elecLoad = 0.0;     // transformer load which may be power in or out depending on the usage mode
    Real64 pastElecLoad = 0.0; // transformer load at the previous timestep
    switch (usageMode_) {
    case TransformerUse::powerInFromGrid: {
        for (std::size_t meterNum = 0; meterNum < wiredMeterPtrs_.size(); ++meterNum) {

            if (DataGlobals::MetersHaveBeenInitialized) {

                elecLoad += GetInstantMeterValue(wiredMeterPtrs_[meterNum], 1) / DataGlobals::TimeStepZoneSec +
                            GetInstantMeterValue(wiredMeterPtrs_[meterNum], 2) / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
                // PastElecLoad store the metered value in the previous time step. This value will be used to check whether
                // a transformer is overloaded or not.
                pastElecLoad += GetCurrentMeterValue(wiredMeterPtrs_[meterNum]) / DataGlobals::TimeStepZoneSec;
            } else {
                elecLoad = 0.0;
                pastElecLoad = 0.0;
            }

            // Because transformer loss has been accounted for by Electricity:Facility and Electricity:HVAC, the transformer
            // loss needs to be deducted from the metered value. Otherwise, double counting (circular relationship) occurs.
            if (specialMeter_[meterNum]) {
                elecLoad = elecLoad - loadLossRate_ - noLoadLossRate_;

                if (elecLoad < 0) elecLoad = 0.0; // Essential check.
            }
        }

        powerOut_ = elecLoad; // the metered value is transformer's output in PowerInFromGrid mode
        break;
    }
    case TransformerUse::powerOutFromBldgToGrid: {
        powerIn_ = surplusPowerOutFromLoadCenters;
        elecLoad = surplusPowerOutFromLoadCenters; // TODO this is input but should be output with the losses, but we don't have them yet.
        break;
    }
    case TransformerUse::powerBetweenLoadCenterAndBldg: {
        // TODO, new configuration for transformer, really part of the specific load center and connects it to the main building bus
        powerIn_ = surplusPowerOutFromLoadCenters;
        elecLoad = surplusPowerOutFromLoadCenters;
        break;
    }
    case TransformerUse::usenotYetSet: {
        // do nothing
        break;
    }
    } // switch usage mode

    // check availability schedule
    if (ratedCapacity_ > 0.0 && ScheduleManager::GetCurrentScheduleValue(availSchedPtr_) > 0.0) {

        Real64 pUL = elecLoad / ratedCapacity_;

        if (pUL > 1.0) {
            pUL = 1.0;
        }

        // Originally, PUL was used to check whether a transformer is overloaded (PUL > 1.0 or not). However, it was
        // found that ElecLoad obtained from GetInstantMeterVlaue() might refer to intermideiate values before
        // convergence. The intermediate values may issue false warning. This the reason why PastElecLoad obtained
        // by GetCurrentMeterValue() is used here to check overload issue.
        if ((pastElecLoad / ratedCapacity_) > 1.0) {
            if (overloadErrorIndex_ == 0) {
                ShowSevereError("Transformer Overloaded");
                ShowContinueError("Entered in ElectricLoadCenter:Transformer =" + name_);
            }
            ShowRecurringSevereErrorAtEnd("Transformer Overloaded: Entered in ElectricLoadCenter:Transformer =" + name_, overloadErrorIndex_);
        }

        Real64 tempChange = std::pow(pUL, 1.6) * tempRise_;
        Real64 ambTemp = 20.0;
        if (heatLossesDestination_ == ThermalLossDestination::zoneGains) {

            ambTemp = DataHeatBalance::ZnAirRpt(zoneNum_).MeanAirTemp;
        } else {
            ambTemp = 20.0;
        }

        Real64 resRef = factorTempCoeff_ + tempRise_ + ambTempRef;
        Real64 resSpecified = factorTempCoeff_ + tempChange + ambTemp;
        Real64 resRatio = resSpecified / resRef;
        Real64 factorTempCorr = (1.0 - eddyFrac_) * resRatio + eddyFrac_ * (1.0 / resRatio);

        loadLossRate_ = ratedLL_ * pow_2(pUL) * factorTempCorr;
        noLoadLossRate_ = ratedNL_;
    } else { // Transformer is not available.
        loadLossRate_ = 0.0;
        noLoadLossRate_ = 0.0;
    }

    totalLossRate_ = loadLossRate_ + noLoadLossRate_;

    switch (usageMode_) {
    case TransformerUse::powerInFromGrid: {
        powerIn_ = elecLoad + totalLossRate_;

        // Transformer losses are wired to the meter via the variable "%ElecUseUtility" only if transformer losses
        // are considered in utility cost. If transformer losses are not considered in utility cost, 0 is assigned
        // to the variable "%ElecUseUtility".
        if (considerLosses_) {
            elecUseMeteredUtilityLosses_ = totalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        } else {
            elecUseMeteredUtilityLosses_ = 0.0;
        }

        // Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
        // is assigned 0
        totalLossEnergy_ = totalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        break;
    }

    case TransformerUse::powerOutFromBldgToGrid:
    case TransformerUse::powerBetweenLoadCenterAndBldg: {
        powerOut_ = elecLoad - totalLossRate_;

        if (powerOut_ < 0) powerOut_ = 0.0;

        powerConversionMeteredLosses_ = -1.0 * totalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
        // is assigned 0
        elecUseMeteredUtilityLosses_ = 0.0;
        break;
    }

    case TransformerUse::usenotYetSet: {
        // do nothing
        assert(false);
    }
    } // switch

    if (powerIn_ <= 0) {
        efficiency_ = 0.0;
    } else {
        efficiency_ = powerOut_ / powerIn_;
    }
    noLoadLossEnergy_ = noLoadLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    loadLossEnergy_ = loadLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    energyIn_ = powerIn_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    energyOut_ = powerOut_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    //   Thermal loss rate may not be equal to Total loss rate. This is the case when surplus power is less than the
    //    calculated total loss rate for a cogeneration transformer. That is why "PowerIn - PowerOut" is used below.
    thermalLossRate_ = powerIn_ - powerOut_;
    thermalLossEnergy_ = thermalLossRate_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = (1.0 - zoneRadFrac_) * thermalLossRate_;
        qdotRadZone_ = (zoneRadFrac_)*thermalLossRate_;
    }
}

void ElectricTransformer::setupMeterIndices()
{
    if (usageMode_ == TransformerUse::powerInFromGrid) {
        for (std::size_t meterNum = 0; meterNum < wiredMeterNames_.size(); ++meterNum) {

            wiredMeterPtrs_[meterNum] = GetMeterIndex(wiredMeterNames_[meterNum]);

            // Check whether the meter is an electricity meter
            // Index function is used here because some resource types are not Electricity but strings containing
            // Electricity such as ElectricityPurchased and ElectricityProduced.
            // It is not proper to have this check in GetInput routine because the meter index may have not been defined
            if (!has(GetMeterResourceType(wiredMeterPtrs_[meterNum]), "Electricity")) {
                EnergyPlus::ShowFatalError("Non-electricity meter used for " + name_);
            }
        }
    }
}

void ElectricTransformer::reinitAtBeginEnvironment()
{
    efficiency_ = 0.0;
    powerIn_ = 0.0;
    energyIn_ = 0.0;
    powerOut_ = 0.0;
    energyOut_ = 0.0;
    noLoadLossRate_ = 0.0;
    noLoadLossEnergy_ = 0.0;
    loadLossRate_ = 0.0;
    loadLossEnergy_ = 0.0;
    thermalLossRate_ = 0.0;
    thermalLossEnergy_ = 0.0;
    elecUseMeteredUtilityLosses_ = 0.0;
    powerConversionMeteredLosses_ = 0.0;
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

void ElectricTransformer::reinitZoneGainsAtBeginEnvironment()
{
    qdotConvZone_ = 0.0;
    qdotRadZone_ = 0.0;
}

std::string const &ElectricTransformer::name() const
{
    return name_;
}

} // namespace EnergyPlus
