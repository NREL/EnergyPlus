// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/CTElectricGenerator.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/FuelCellElectricGenerator.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/ICEngineElectricGenerator.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/MicroturbineElectricGenerator.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PVWatts.hh>
#include <EnergyPlus/Photovoltaics.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindTurbine.hh>

namespace EnergyPlus {

void createFacilityElectricPowerServiceObject(EnergyPlusData &state)
{
    state.dataElectPwrSvcMgr->facilityElectricServiceObj = std::make_unique<ElectricPowerServiceManager>();
}

void initializeElectricPowerServiceZoneGains(EnergyPlusData &state) // namespace routine for handling call from InternalHeatGains
{
    // internal zone gains need to be re initialized for begin new environment earlier than the main call into manage electric power service
    if (state.dataElectPwrSvcMgr->facilityElectricServiceObj->newEnvironmentInternalGainsFlag && state.dataGlobal->BeginEnvrnFlag) {
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->reinitZoneGainsAtBeginEnvironment();
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->newEnvironmentInternalGainsFlag = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->newEnvironmentInternalGainsFlag = true;
    }
}

void ElectricPowerServiceManager::manageElectricPowerService(
    EnergyPlusData &state,
    bool const firstHVACIteration,
    bool &SimElecCircuits,      // simulation convergence flag
    bool const UpdateMetersOnly // if true then don't resimulate generators, just update meters.
)
{
    if (getInputFlag_) {
        getPowerManagerInput(state);
        getInputFlag_ = false;
    }

    if (state.dataGlobal->MetersHaveBeenInitialized && setupMeterIndexFlag_) {
        setupMeterIndices(state);
        setupMeterIndexFlag_ = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && newEnvironmentFlag_) {
        reinitAtBeginEnvironment();
        newEnvironmentFlag_ = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) newEnvironmentFlag_ = true;

    // retrieve data from meters for demand and production
    totalBldgElecDemand_ =
        GetInstantMeterValue(state, elecFacilityIndex_, OutputProcessor::TimeStepType::TimeStepZone) / state.dataGlobal->TimeStepZoneSec;
    totalHVACElecDemand_ = GetInstantMeterValue(state, elecFacilityIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                           (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    totalElectricDemand_ = totalBldgElecDemand_ + totalHVACElecDemand_;
    elecProducedPVRate_ = GetInstantMeterValue(state, elecProducedPVIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                          (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedWTRate_ = GetInstantMeterValue(state, elecProducedWTIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                          (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedStorageRate_ = GetInstantMeterValue(state, elecProducedStorageIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                               (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedCoGenRate_ = GetInstantMeterValue(state, elecProducedCoGenIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                             (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedPowerConversionRate_ = GetInstantMeterValue(state, elecProducedPowerConversionIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                                       (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);

    wholeBldgRemainingLoad_ = totalElectricDemand_;

    if (UpdateMetersOnly) { // just update record keeping, don't resimulate load centers
        if (facilityPowerInTransformerPresent_) {
            facilityPowerInTransformerObj_->manageTransformers(state, 0.0);
        }

        updateWholeBuildingRecords(state);
        return;
    }

    for (auto &e : elecLoadCenterObjs) {
        e->manageElecLoadCenter(state, firstHVACIteration, wholeBldgRemainingLoad_);
    }

    updateWholeBuildingRecords(state);
    // The transformer call should be put outside of the "Load Center" loop because
    // 1) A transformer may be for utility, not for load center
    // 2) A tansformer may be shared by multiple load centers
    if (facilityPowerInTransformerPresent_) {
        facilityPowerInTransformerObj_->manageTransformers(state, 0.0);
    }

    updateWholeBuildingRecords(state);
    if (powerOutTransformerObj_ != nullptr) {
        powerOutTransformerObj_->manageTransformers(state, electSurplusRate_);
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

void ElectricPowerServiceManager::getPowerManagerInput(EnergyPlusData &state)
{
    static constexpr std::string_view routineName = "ElectricPowerServiceManager  getPowerManagerInput ";

    numLoadCenters_ = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Distribution");

    if (numLoadCenters_ > 0) {
        for (auto iLoadCenterNum = 1; iLoadCenterNum <= numLoadCenters_; ++iLoadCenterNum) {
            // call Electric Power Load Center constructor, in place
            elecLoadCenterObjs.emplace_back(new ElectPowerLoadCenter(state, iLoadCenterNum));
        }
    } else {
        // issue #4639. see if there are any generators, inverters, converters, or storage devcies, that really need a ElectricLoadCenter:Distribution
        bool errorsFound(false);
        int numGenLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Generators");
        if (numGenLists > 0) {
            ShowSevereError(state, "ElectricLoadCenter:Generators input object requires an ElectricLoadCenterDistribution input object.");
            errorsFound = true;
        }
        int numInverters = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Inverter:Simple");
        numInverters += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Inverter:FunctionOfPower");
        numInverters += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Inverter:LookUpTable");
        if (numInverters > 0) {
            ShowSevereError(state, "ElectricLoadCenter:Inverter:* input objects require an ElectricLoadCenter:Distribution input object.");
            errorsFound = true;
        }
        int numStorage = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Storage:Simple");
        numStorage += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Storage:Battery");
        if (numStorage > 0) {
            ShowSevereError(state, "ElectricLoadCenter:Storage:* input objects require an ElectricLoadCenter:Distribution input object.");
            errorsFound = true;
        }
        int numGenerators = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Generator:InternalCombustionEngine");
        numGenerators += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Generator:CombustionTurbine");
        numGenerators += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Generator:MicroCHP");
        numGenerators += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Generator:FuelCell");
        numGenerators += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Generator:Photovoltaic");
        numGenerators += state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Generator:WindTurbine");
        if (numGenerators > 0) {
            ShowSevereError(state, "Electric generator input objects require an ElectricLoadCenter:Distribution input object.");
            errorsFound = true;
        }

        if (errorsFound) {
            ShowFatalError(state, "Simulation halted because of missing input objects related to ElectricLoadCenter.");
        }

        // if user input did not include an Electric Load center, create a simple default one here for reporting purposes
        //   but only if there are any other electricity components set up (yet) for metering
        int anyElectricityPresent = GetMeterIndex(state, "ELECTRICITY:FACILITY");
        int anyPlantLoadProfilePresent = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "LoadProfile:Plant");
        if (anyElectricityPresent > 0 || anyPlantLoadProfilePresent > 0) {
            elecLoadCenterObjs.emplace_back(new ElectPowerLoadCenter(state, 0));
            numLoadCenters_ = 1;
        }
    }

    // see if there are any transformers of the type powerInFromGrid
    numTransformers_ = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ElectricLoadCenter:Transformer");

    if (numTransformers_ > 0) {
        int numAlphas; // Number of elements in the alpha array
        int numNums;   // Number of elements in the numeric array
        int iOStat;    // IO Status when calling get input subroutine
        int facilityPowerInTransformerIDFObjNum = 0;
        bool foundInFromGridTransformer = false;

        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Transformer";
        for (auto loopTransformer = 1; loopTransformer <= numTransformers_; ++loopTransformer) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     loopTransformer,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     numAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     numNums,
                                                                     iOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "PowerInFromGrid")) {
                if (!foundInFromGridTransformer) {
                    foundInFromGridTransformer = true;
                    facilityPowerInTransformerIDFObjNum = loopTransformer;
                    facilityPowerInTransformerName_ = state.dataIPShortCut->cAlphaArgs(1);
                    facilityPowerInTransformerPresent_ = true;
                } else {
                    // should only have one transformer in input that is PowerInFromGrid
                    ShowWarningError(state,
                                     std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                    ShowContinueError(state,
                                      "Only one transformer with Usage PowerInFromGrid can be used, first one in input file will be used and the "
                                      "simulation continues...");
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "PowerOutToGrid")) {
                if (powerOutTransformerObj_ == nullptr) {
                    ++numPowerOutTransformers_;
                    powerOutTransformerName_ = state.dataIPShortCut->cAlphaArgs(1);
                    powerOutTransformerObj_ = std::make_unique<ElectricTransformer>(state, powerOutTransformerName_);

                } else {
                    ShowWarningError(state,
                                     "Found more than one transformer set to PowerOutFromOnsiteGeneration, however only the first one will be used.");
                }
            }
        }
        if (foundInFromGridTransformer) {
            // call transformer constructor
            facilityPowerInTransformerObj_ = std::make_unique<ElectricTransformer>(state, facilityPowerInTransformerName_);
        }
    } // if transformers

    if (numLoadCenters_ > 0) {
        SetupOutputVariable(state,
                            "Facility Total Purchased Electricity Rate",
                            OutputProcessor::Unit::W,
                            electPurchRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Facility Total Purchased Electricity Energy",
                            OutputProcessor::Unit::J,
                            electricityPurch_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricityPurchased",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Facility Total Surplus Electricity Rate",
                            OutputProcessor::Unit::W,
                            electSurplusRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Facility Total Surplus Electricity Energy",
                            OutputProcessor::Unit::J,
                            electricitySurplus_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricitySurplusSold",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Facility Net Purchased Electricity Rate",
                            OutputProcessor::Unit::W,
                            electricityNetRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Facility Net Purchased Electricity Energy",
                            OutputProcessor::Unit::J,
                            electricityNet_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricityNet",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Facility Total Building Electricity Demand Rate",
                            OutputProcessor::Unit::W,
                            totalBldgElecDemand_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Facility Total HVAC Electricity Demand Rate",
                            OutputProcessor::Unit::W,
                            totalHVACElecDemand_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Facility Total Electricity Demand Rate",
                            OutputProcessor::Unit::W,
                            totalElectricDemand_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);

        SetupOutputVariable(state,
                            "Facility Total Produced Electricity Rate",
                            OutputProcessor::Unit::W,
                            electProdRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Facility Total Produced Electricity Energy",
                            OutputProcessor::Unit::J,
                            electricityProd_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);

        reportPVandWindCapacity(state);

        sumUpNumberOfStorageDevices();

        checkLoadCenters(state);
    }
}

void ElectricPowerServiceManager::setupMeterIndices(EnergyPlusData &state)
{
    elecFacilityIndex_ = EnergyPlus::GetMeterIndex(state, "Electricity:Facility");
    elecProducedCoGenIndex_ = EnergyPlus::GetMeterIndex(state, "Cogeneration:ElectricityProduced");
    elecProducedPVIndex_ = EnergyPlus::GetMeterIndex(state, "Photovoltaic:ElectricityProduced");
    elecProducedWTIndex_ = EnergyPlus::GetMeterIndex(state, "WindTurbine:ElectricityProduced");
    elecProducedStorageIndex_ = EnergyPlus::GetMeterIndex(state, "ElectricStorage:ElectricityProduced");
    elecProducedPowerConversionIndex_ = EnergyPlus::GetMeterIndex(state, "PowerConversion:ElectricityProduced");

    if (numLoadCenters_ > 0) {
        for (auto &e : elecLoadCenterObjs) {
            e->setupLoadCenterMeterIndices(state);
        }
    }
    if (facilityPowerInTransformerPresent_) {
        facilityPowerInTransformerObj_->setupMeterIndices(state);
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

void ElectricPowerServiceManager::verifyCustomMetersElecPowerMgr(EnergyPlusData &state)
{
    for (std::size_t loop = 0; loop < elecLoadCenterObjs.size(); ++loop) {
        elecLoadCenterObjs[loop]->setupLoadCenterMeterIndices(state);
    }
}

void ElectricPowerServiceManager::updateWholeBuildingRecords(EnergyPlusData &state)
{

    // main panel balancing.
    totalBldgElecDemand_ =
        GetInstantMeterValue(state, elecFacilityIndex_, OutputProcessor::TimeStepType::TimeStepZone) / state.dataGlobal->TimeStepZoneSec;
    totalHVACElecDemand_ = GetInstantMeterValue(state, elecFacilityIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                           (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    totalElectricDemand_ = totalBldgElecDemand_ + totalHVACElecDemand_;
    elecProducedPVRate_ = GetInstantMeterValue(state, elecProducedPVIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                          (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedWTRate_ = GetInstantMeterValue(state, elecProducedWTIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                          (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedStorageRate_ = GetInstantMeterValue(state, elecProducedStorageIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                               (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedCoGenRate_ = GetInstantMeterValue(state, elecProducedCoGenIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                             (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    elecProducedPowerConversionRate_ = GetInstantMeterValue(state, elecProducedPowerConversionIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                                       (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);

    electProdRate_ = elecProducedCoGenRate_ + elecProducedPVRate_ + elecProducedWTRate_ + elecProducedStorageRate_ + elecProducedPowerConversionRate_;
    electricityProd_ = electProdRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour; // whole building

    // Report the Total Electric Power Purchased [W], If negative then there is extra power to be sold or stored.
    electPurchRate_ = totalElectricDemand_ - electProdRate_;
    // Check this value against a tolerance to aid in reporting.
    if (std::abs(electPurchRate_) < 0.0001) electPurchRate_ = 0.0;
    if (electPurchRate_ < 0.0) electPurchRate_ = 0.0; // don't want negative purchased...

    // Report the Total Electric Energy Purchased [J]
    electricityPurch_ = electPurchRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // report the total electric surplus....
    electSurplusRate_ = electProdRate_ - totalElectricDemand_;
    if (std::abs(electSurplusRate_) < 0.0001) electSurplusRate_ = 0.0;
    if (electSurplusRate_ < 0.0) electSurplusRate_ = 0.0; // don't want negative surplus

    electricitySurplus_ = electSurplusRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // report the net electricity , + is purchased, - is surplus
    electricityNetRate_ = totalElectricDemand_ - electProdRate_;

    electricityNet_ = electricityNetRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
}

void ElectricPowerServiceManager::reportPVandWindCapacity(EnergyPlusData &state)
{
    // LEED report
    pvTotalCapacity_ = 0.0;
    windTotalCapacity_ = 0.0;
    for (auto &lc : elecLoadCenterObjs) {
        if (lc->numGenerators > 0) {
            for (auto &g : lc->elecGenCntrlObj) {
                if (g->compGenTypeOf_Num == GeneratorType::PV) {
                    pvTotalCapacity_ += g->maxPowerOut;
                }
                if (g->compGenTypeOf_Num == GeneratorType::WindTurbine) {
                    windTotalCapacity_ += g->maxPowerOut;
                }
            }
        }
    }
    // put in total capacity for PV and Wind for LEED report
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedRenRatCap, "Photovoltaic", pvTotalCapacity_ / 1000, 2);
    OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedRenRatCap, "Wind", windTotalCapacity_ / 1000, 2);

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

void ElectricPowerServiceManager::checkLoadCenters(EnergyPlusData &state)
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
                ShowSevereError(state,
                                "ElectricPowerServiceManager::checkLoadCenters, the electrical storage device named = " + storageNames[i] +
                                    " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError(state, "Electric Load Centers cannot share the same storage device.");
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
                ShowSevereError(state,
                                "ElectricPowerServiceManager::checkLoadCenters, the generator list named = " + genListNames[i] +
                                    " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError(state, "Electric Load Centers cannot share the same generator list (ElectricLoadCenter:Generators).");
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
                ShowSevereError(state,
                                "ElectricPowerServiceManager::checkLoadCenters, the inverter device named = " + inverterNames[i] +
                                    " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError(state, "Electric Load Centers cannot share the same inverter device.");
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
                ShowSevereError(state,
                                "ElectricPowerServiceManager::checkLoadCenters, the converter device named = " + converterNames[i] +
                                    " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError(state, "Electric Load Centers cannot share the same converter device.");
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
                ShowSevereError(state,
                                "ElectricPowerServiceManager::checkLoadCenters, the transformer device named = " + transformerNames[i] +
                                    " is used in more than one ElectricLoadCenter:Distribution input object.");
                ShowContinueError(state, "Electric Load Centers cannot share the same transformer device.");
                errorsFound = true;
                break;
            }
        }
        if (errorsFound) {
            break;
        }
    }

    if (errorsFound) { // throw fatal, these errors could fatal out in internal gains with missleading data
        ShowFatalError(state, "ElectricPowerServiceManager::checkLoadCenters, preceding errors terminate program.");
    }
}

ElectPowerLoadCenter::ElectPowerLoadCenter(EnergyPlusData &state, int const objectNum)
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

    static constexpr std::string_view routineName = "ElectPowerLoadCenter constructor ";
    int numAlphas; // Number of elements in the alpha array
    int numNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound;

    state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Distribution";
    errorsFound = false;
    if (objectNum > 0) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 objectNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 numAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 numNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        name_ = state.dataIPShortCut->cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?

        if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            generatorListName_ = state.dataIPShortCut->cAlphaArgs(2);
            // check that

            int testIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Generators", generatorListName_);
            if (testIndex == 0) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                errorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
            // Load the Generator Control Operation Scheme
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "Baseload")) {
                genOperationScheme_ = GeneratorOpScheme::baseLoad;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DemandLimit")) {
                genOperationScheme_ = GeneratorOpScheme::demandLimit;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "TrackElectrical")) {
                genOperationScheme_ = GeneratorOpScheme::trackElectrical;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "TrackSchedule")) {
                genOperationScheme_ = GeneratorOpScheme::trackSchedule;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "TrackMeter")) {
                genOperationScheme_ = GeneratorOpScheme::trackMeter;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "FollowThermal")) {
                genOperationScheme_ = GeneratorOpScheme::thermalFollow;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "FollowThermalLimitElectrical")) {
                genOperationScheme_ = GeneratorOpScheme::thermalFollowLimitElectrical;
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                errorsFound = true;
            }
        }

        demandLimit_ = state.dataIPShortCut->rNumericArgs(1);

        trackSchedPtr_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
        if ((trackSchedPtr_ == 0) && (genOperationScheme_ == GeneratorOpScheme::trackSchedule)) {
            if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = blank field.");
            }
            ShowContinueError(state, "Schedule not found; Must be entered and valid when Generator Operation Scheme=TrackSchedule");
            errorsFound = true;
        }

        demandMeterName_ = UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(5));
        // meters may not be "loaded" yet, defered check to later subroutine

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "AlternatingCurrent")) {
            bussType = ElectricBussType::aCBuss;
            state.dataIPShortCut->cAlphaArgs(6) = "AlternatingCurrent";
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "DirectCurrentWithInverter")) {
            bussType = ElectricBussType::dCBussInverter;
            inverterPresent = true;
            state.dataIPShortCut->cAlphaArgs(6) = "DirectCurrentWithInverter";
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "AlternatingCurrentWithStorage")) {
            bussType = ElectricBussType::aCBussStorage;
            storagePresent_ = true;
            state.dataIPShortCut->cAlphaArgs(6) = "AlternatingCurrentWithStorage";
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "DirectCurrentWithInverterDCStorage")) {
            bussType = ElectricBussType::dCBussInverterDCStorage;
            inverterPresent = true;
            storagePresent_ = true;
            state.dataIPShortCut->cAlphaArgs(6) = "DirectCurrentWithInverterDCStorage";
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "DirectCurrentWithInverterACStorage")) {
            bussType = ElectricBussType::dCBussInverterACStorage;
            inverterPresent = true;
            storagePresent_ = true;
            state.dataIPShortCut->cAlphaArgs(6) = "DirectCurrentWithInverterACStorage";
        } else if (state.dataIPShortCut->cAlphaArgs(6).empty()) {
            bussType = ElectricBussType::aCBuss;
            state.dataIPShortCut->cAlphaArgs(6) = "AlternatingCurrent (field was blank)";
        } else {
            ShowSevereError(state,
                            std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", invalid entry.");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
            errorsFound = true;
        }

        if (inverterPresent) {
            if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                inverterName = state.dataIPShortCut->cAlphaArgs(7);
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(7) + " is blank, but buss type requires inverter.");
                errorsFound = true;
            }
        }

        if (storagePresent_) {
            if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                storageName_ = state.dataIPShortCut->cAlphaArgs(8);
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(8) + " is blank, but buss type requires storage.");
                errorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
            // process transformer
            transformerName_ = state.dataIPShortCut->cAlphaArgs(9);
            // only transformers of use type powerFromLoadCenterToBldg are really held in a load center, The legacy applications for transformers are
            // held at the higher Electric service level
            transformerPresent_ = true;
        }

        // Begin new content for grid supply and more control over storage
        // user selected storage operation scheme
        if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "TrackFacilityElectricDemandStoreExcessOnSite")) {
                storageScheme_ = StorageOpScheme::facilityDemandStoreExcessOnSite;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "TrackMeterDemandStoreExcessOnSite")) {
                storageScheme_ = StorageOpScheme::meterDemandStoreExcessOnSite;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "TrackChargeDischargeSchedules")) {
                storageScheme_ = StorageOpScheme::chargeDischargeSchedules;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "FacilityDemandLeveling")) {
                storageScheme_ = StorageOpScheme::facilityDemandLeveling;
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + " = " + state.dataIPShortCut->cAlphaArgs(10));
                errorsFound = true;
            }
        } else { // blank (preserve legacy behavior for short files)
            storageScheme_ = StorageOpScheme::facilityDemandStoreExcessOnSite;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
            trackSorageOpMeterName_ = state.dataIPShortCut->cAlphaArgs(11);

        } else {
            if (storageScheme_ == StorageOpScheme::meterDemandStoreExcessOnSite) { // throw error
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state,
                                  "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) +
                                      ", cannot be blank when storage operation scheme is TrackMeterDemandStoreExcessOnSite");
                errorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
            converterName_ = state.dataIPShortCut->cAlphaArgs(12);
            converterPresent_ = true;
        } else {
            if (storageScheme_ == StorageOpScheme::chargeDischargeSchedules || storageScheme_ == StorageOpScheme::facilityDemandLeveling) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state,
                                  "Invalid " + state.dataIPShortCut->cAlphaFieldNames(12) + ", cannot be blank when storage scheme is " +
                                      state.dataIPShortCut->cAlphaArgs(10));
                errorsFound = true;
            }
        }

        if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
            maxStorageSOCFraction_ = 1.0;
        } else {
            maxStorageSOCFraction_ = state.dataIPShortCut->rNumericArgs(2);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
            minStorageSOCFraction_ = 0.0;
        } else {
            minStorageSOCFraction_ = state.dataIPShortCut->rNumericArgs(3);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(4)) {
            designStorageChargePowerWasSet_ = false;
        } else {
            designStorageChargePowerWasSet_ = true;
            designStorageChargePower_ = state.dataIPShortCut->rNumericArgs(4);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(5)) {
            designStorageDischargePowerWasSet_ = false;
        } else {
            designStorageDischargePowerWasSet_ = true;
            designStorageDischargePower_ = state.dataIPShortCut->rNumericArgs(5);
        }

        if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
            if (storageScheme_ == StorageOpScheme::facilityDemandLeveling) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cNumericFieldNames(6) + " = blank field.");
                errorsFound = true;
            }
        } else {
            facilityDemandTarget_ = state.dataIPShortCut->rNumericArgs(6);
        }
        storageChargeModSchedIndex_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(13));
        if (storageChargeModSchedIndex_ == 0 && storageScheme_ == StorageOpScheme::chargeDischargeSchedules) {
            if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(13) + " = " + state.dataIPShortCut->cAlphaArgs(13));
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(13) + " = blank field.");
            }
            ShowContinueError(state, "Schedule not found; Must be entered and valid when Storage Operation Scheme = TrackChargeDischargeSchedules");
            errorsFound = true;
        }

        storageDischargeModSchedIndex_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(14));
        if (storageDischargeModSchedIndex_ == 0 && storageScheme_ == StorageOpScheme::chargeDischargeSchedules) {
            if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(14) + " = " + state.dataIPShortCut->cAlphaArgs(14));
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(14) + " = blank field.");
            }
            ShowContinueError(state, "Schedule not found; Must be entered and valid when Storage Operation Scheme = TrackChargeDischargeSchedules");
            errorsFound = true;
        }

        facilityDemandTargetModSchedIndex_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(15));
        if (facilityDemandTargetModSchedIndex_ == 0 && storageScheme_ == StorageOpScheme::facilityDemandLeveling) {
            if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(15) + " = " + state.dataIPShortCut->cAlphaArgs(15));
            } else {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(15) + " = blank field.");
            }
            ShowContinueError(state, "Schedule not found; Must be entered and valid when Storage Operation Scheme = FacilityDemandLeveling");
            errorsFound = true;
        }
    } else { // object num == 0
        // just construct an empty object and return
        return;
    }

    // now that we are done with processing get input for ElectricLoadCenter:Distribution we can call child input objects without IP shortcut problems
    state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Generators";
    int genListObjectNum =
        state.dataInputProcessing->inputProcessor->getObjectItemNum(state, state.dataIPShortCut->cCurrentModuleObject, generatorListName_);
    if (genListObjectNum > 0) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 genListObjectNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 numAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 numNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        // Calculate the number of generators in list
        numGenerators = numNums / 2; // note IDD needs Min Fields = 6
        if (mod((numAlphas - 1 + numNums), 5) != 0) ++numGenerators;
        int alphaCount = 2;
        for (auto genCount = 1; genCount <= numGenerators; ++genCount) {
            // call constructor in place
            generatorsPresent_ = true;
            elecGenCntrlObj.emplace_back(new GeneratorController(state,
                                                                 state.dataIPShortCut->cAlphaArgs(alphaCount),
                                                                 state.dataIPShortCut->cAlphaArgs(alphaCount + 1),
                                                                 state.dataIPShortCut->rNumericArgs(2 * genCount - 1),
                                                                 state.dataIPShortCut->cAlphaArgs(alphaCount + 2),
                                                                 state.dataIPShortCut->rNumericArgs(2 * genCount)));
            ++alphaCount;
            ++alphaCount;
            ++alphaCount;
        }

        // issue #5299 check for non-zero values in thermal electric ratio if gen op scheme is thermalFollow*
        if (genOperationScheme_ == GeneratorOpScheme::thermalFollow || genOperationScheme_ == GeneratorOpScheme::thermalFollowLimitElectrical) {
            // check to make sure the user didn't input zeros for thermalToElectricControlRatio
            for (auto &g : elecGenCntrlObj) {
                if (g->nominalThermElectRatio <= 0.0) {
                    ShowWarningError(state,
                                     "Generator operation needs to be based on following thermal loads and needs values for Rated Thermal to "
                                     "Electrical Power Ratio in " +
                                         state.dataIPShortCut->cCurrentModuleObject + " named " + state.dataIPShortCut->cAlphaArgs(1));
                }
            }
        }
    }

    if (!errorsFound && inverterPresent) {
        // call inverter constructor
        inverterObj = std::make_unique<DCtoACInverter>(state, inverterName);

        // Make sure only Generator::PVWatts are used with Inverter:PVWatts
        // Add up the total DC capacity and pass it to the inverter.
        if (inverterObj->modelType() == DCtoACInverter::InverterModelType::pvWatts) {
            Real64 totalDCCapacity = 0.0;
            for (const auto &generatorController : elecGenCntrlObj) {
                if (generatorController->generatorType != GeneratorType::PVWatts) {
                    errorsFound = true;
                    ShowSevereError(state, std::string{routineName} + "ElectricLoadCenter:Distribution=\"" + name_ + "\",");
                    ShowContinueError(state, "ElectricLoadCenter:Inverter:PVWatts can only be used with Generator:PVWatts");
                    ShowContinueError(state, "\"" + generatorController->name + "\" is of type " + generatorController->typeOfName);
                } else {
                    totalDCCapacity += generatorController->pvwattsGenerator->getDCSystemCapacity();

                    // Pass the inverter properties to the PVWatts generator class
                    generatorController->pvwattsGenerator->setDCtoACRatio(inverterObj->pvWattsDCtoACSizeRatio());
                    generatorController->pvwattsGenerator->setInverterEfficiency(inverterObj->pvWattsInverterEfficiency());
                }
            }
            if (!errorsFound) {
                inverterObj->setPVWattsDCCapacity(state, totalDCCapacity);
            }
        }
    }

    if (!errorsFound && storagePresent_) {
        // call storage constructor
        storageObj = std::make_unique<ElectricStorage>(state, storageName_);
    }

    if (!errorsFound && transformerPresent_) {

        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Transformer";
        int transformerItemNum =
            state.dataInputProcessing->inputProcessor->getObjectItemNum(state, state.dataIPShortCut->cCurrentModuleObject, transformerName_);
        int iOStat;
        if (transformerItemNum > 0) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     transformerItemNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     numAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     numNums,
                                                                     iOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3),
                                            "LoadCenterPowerConditioning")) { // this is the right kind of transformer
                transformerObj = std::make_unique<ElectricTransformer>(state, transformerName_);
            } else {
                ShowWarningError(state,
                                 "Transformer named " + transformerName_ + " associated with the load center named " + name_ + " should have " +
                                     state.dataIPShortCut->cAlphaFieldNames(3) + " set to LoadCenterPowerConditioning.");
            }
        } else {
            ShowSevereError(state, "Transformer named " + transformerName_ + ", was not found for the load center named " + name_);
            errorsFound = true;
        }
    }

    if (!errorsFound && converterPresent_) {
        // call AC to DC converter constructor
        converterObj = std::make_unique<ACtoDCConverter>(state, converterName_);
    }

    // Setup general output variables for reporting in the electric load center
    SetupOutputVariable(state,
                        "Electric Load Center Produced Electricity Rate",
                        OutputProcessor::Unit::W,
                        genElectProdRate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name_);
    SetupOutputVariable(state,
                        "Electric Load Center Produced Electricity Energy",
                        OutputProcessor::Unit::J,
                        genElectricProd,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        name_);
    SetupOutputVariable(state,
                        "Electric Load Center Supplied Electricity Rate",
                        OutputProcessor::Unit::W,
                        subpanelFeedInRate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name_);
    SetupOutputVariable(state,
                        "Electric Load Center Drawn Electricity Rate",
                        OutputProcessor::Unit::W,
                        subpanelDrawRate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name_);
    SetupOutputVariable(state,
                        "Electric Load Center Produced Thermal Rate",
                        OutputProcessor::Unit::W,
                        thermalProdRate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name_);
    SetupOutputVariable(state,
                        "Electric Load Center Produced Thermal Energy",
                        OutputProcessor::Unit::J,
                        thermalProd,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        name_);
    SetupOutputVariable(state,
                        "Electric Load Center Requested Electricity Rate",
                        OutputProcessor::Unit::W,
                        totalPowerRequest_,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name_);

    if (state.dataGlobal->AnyEnergyManagementSystemInModel && storagePresent_) {
        SetupEMSActuator(state, "Electrical Storage", name_, "Power Draw Rate", "[W]", eMSOverridePelFromStorage_, eMSValuePelFromStorage_);
        SetupEMSActuator(state, "Electrical Storage", name_, "Power Charge Rate", "[W]", eMSOverridePelIntoStorage_, eMSValuePelIntoStorage_);
    }

    if (errorsFound) {
        ShowFatalError(state, std::string{routineName} + "Preceding errors terminate program.");
    }
}

void ElectPowerLoadCenter::manageElecLoadCenter(EnergyPlusData &state, bool const firstHVACIteration, Real64 &remainingWholePowerDemand)
{
    //
    subpanelFeedInRequest = remainingWholePowerDemand;

    if (generatorsPresent_) {
        dispatchGenerators(state, firstHVACIteration, remainingWholePowerDemand);

    } // if generators present
    updateLoadCenterGeneratorRecords(state);
    if (bussType == ElectricBussType::dCBussInverter || bussType == ElectricBussType::dCBussInverterACStorage) {
        inverterObj->simulate(state, genElectProdRate);
    }

    if (storagePresent_) {
        storageObj->timeCheckAndUpdate(state);
        dispatchStorage(state, subpanelFeedInRequest);
    }

    if (bussType == ElectricBussType::dCBussInverterDCStorage) {
        if (inverterObj != nullptr) {
            inverterObj->simulate(state, storOpCVFeedInRate);
        }
    }

    if (converterObj != nullptr) {
        converterObj->simulate(state, storOpCVDrawRate);
    }

    if (transformerObj != nullptr) {
        if (storOpCVFeedInRate > 0.0) {
            transformerObj->manageTransformers(state, storOpCVFeedInRate);
        } else if (storOpCVDrawRate > 0.0) {
            transformerObj->manageTransformers(state, subpanelDrawRate);
        }
    }
    updateLoadCenterGeneratorRecords(state);
}

void ElectPowerLoadCenter::dispatchGenerators(EnergyPlusData &state,
                                              bool const firstHVACIteration,
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

            if (ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0) {
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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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

            if (ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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

            if (ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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
        remainingLoad = ScheduleManager::GetCurrentScheduleValue(state, trackSchedPtr_);
        loadCenterElectricLoad = remainingLoad;

        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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
        customMeterDemand =
            GetInstantMeterValue(state, demandMeterPtr_, OutputProcessor::TimeStepType::TimeStepZone) / state.dataGlobal->TimeStepZoneSec +
            GetInstantMeterValue(state, demandMeterPtr_, OutputProcessor::TimeStepType::TimeStepSystem) /
                (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);

        remainingLoad = customMeterDemand;
        loadCenterElectricLoad = remainingLoad;

        for (auto &g : elecGenCntrlObj) {
            if (ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0 && remainingLoad > 0.0) {
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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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
        Real64 remainingThermalLoad = calcLoadCenterThermalLoad(state);
        Real64 loadCenterThermalLoad = remainingThermalLoad;
        for (auto &g : elecGenCntrlObj) {

            if (ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0 && remainingThermalLoad > 0.0) {

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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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
        Real64 remainingThermalLoad = calcLoadCenterThermalLoad(state);
        // Total current electrical demand for the building is a secondary limit.
        remainingLoad = remainingWholePowerDemand;
        loadCenterElectricLoad = remainingWholePowerDemand;
        Real64 loadCenterThermalLoad = remainingThermalLoad;
        for (auto &g : elecGenCntrlObj) {
            if ((ScheduleManager::GetCurrentScheduleValue(state, g->availSchedPtr) > 0.0) && (remainingThermalLoad > 0.0) && (remainingLoad > 0.0)) {
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
            g->simGeneratorGetPowerOutput(
                state, g->onThisTimestep, g->powerRequestThisTimestep, firstHVACIteration, g->electProdRate, g->thermProdRate);

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
        g->electricityProd = g->electProdRate * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        genElectricProd += g->electricityProd;
    }
}

void ElectPowerLoadCenter::dispatchStorage(EnergyPlusData &state,
                                           Real64 const originalFeedInRequest // whole building remaining electric demand for this load center
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
        subpanelFeedInRequest =
            GetInstantMeterValue(state, trackStorageOpMeterIndex_, OutputProcessor::TimeStepType::TimeStepZone) / state.dataGlobal->TimeStepZoneSec +
            GetInstantMeterValue(state, trackStorageOpMeterIndex_, OutputProcessor::TimeStepType::TimeStepSystem) /
                (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        subpanelDrawRequest = 0.0;
        break;
    }
    case StorageOpScheme::chargeDischargeSchedules: {
        // do not need to deal with subpanel rates here, charge or discharge is known from schedules and filled in below
        break;
    }
    case StorageOpScheme::facilityDemandLeveling: {
        Real64 demandTarget = facilityDemandTarget_ * ScheduleManager::GetCurrentScheduleValue(state, facilityDemandTargetModSchedIndex_);
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
            adjustedFeedInRequest = subpanelFeedInRequest + transformerObj->getLossRateForOutputPower(state, subpanelFeedInRequest);
            adjustedDrawRequest = subpanelDrawRequest - transformerObj->getLossRateForInputPower(state, subpanelDrawRequest);
        }
        break;
    }
    case ElectricBussType::dCBussInverterDCStorage: {
        // can we get updated power conditioning losses here?
        if (transformerObj == nullptr) {
            adjustedFeedInRequest = subpanelFeedInRequest + inverterObj->getLossRateForOutputPower(state, subpanelFeedInRequest);
            if (converterObj == nullptr) { // some operation schemes will never need a converter
                adjustedDrawRequest = subpanelDrawRequest;
            } else {
                adjustedDrawRequest = subpanelDrawRequest - converterObj->getLossRateForInputPower(state, subpanelDrawRequest);
            }
        } else {
            adjustedFeedInRequest = subpanelFeedInRequest + inverterObj->getLossRateForOutputPower(state, subpanelFeedInRequest) +
                                    transformerObj->getLossRateForOutputPower(state, subpanelFeedInRequest);
            if (converterObj == nullptr) {
                adjustedDrawRequest = subpanelDrawRequest - transformerObj->getLossRateForInputPower(state, subpanelDrawRequest);
            } else {
                adjustedDrawRequest = subpanelDrawRequest - converterObj->getLossRateForInputPower(state, subpanelDrawRequest) -
                                      transformerObj->getLossRateForInputPower(state, subpanelDrawRequest);
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
        storOpCVChargeRate = designStorageChargePower_ * ScheduleManager::GetCurrentScheduleValue(state, storageChargeModSchedIndex_);
        storOpCVDischargeRate = designStorageDischargePower_ * ScheduleManager::GetCurrentScheduleValue(state, storageDischargeModSchedIndex_);
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
        state, storOpCVChargeRate, storOpCVDischargeRate, storOpIsCharging, storOpIsDischarging, maxStorageSOCFraction_, minStorageSOCFraction_);

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

void ElectPowerLoadCenter::setupLoadCenterMeterIndices(EnergyPlusData &state)
{
    demandMeterPtr_ = EnergyPlus::GetMeterIndex(state, demandMeterName_);
    if ((demandMeterPtr_ == 0) && (genOperationScheme_ == GeneratorOpScheme::trackMeter)) { // throw error
        ShowFatalError(state,
                       "ElectPowerLoadCenter::setupLoadCenterMeterIndices  Did not find Meter named: " + demandMeterName_ +
                           " in ElectricLoadCenter:Distribution named " + name_);
    }

    if (storageScheme_ == StorageOpScheme::meterDemandStoreExcessOnSite) {
        trackStorageOpMeterIndex_ = EnergyPlus::GetMeterIndex(state, trackSorageOpMeterName_);
        if (trackStorageOpMeterIndex_ == 0) { //
            ShowFatalError(state,
                           "ElectPowerLoadCenter::setupLoadCenterMeterIndices  Did not find Meter named: " + trackSorageOpMeterName_ +
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

void ElectPowerLoadCenter::updateLoadCenterGeneratorRecords(EnergyPlusData &state)
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
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(state, genElectProdRate);
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
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(state, subpanelFeedInRate);
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
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(state, subpanelFeedInRate);
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
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(state, subpanelFeedInRate);
            subpanelDrawRate += transformerObj->getLossRateForOutputPower(state, subpanelDrawRate);
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
            subpanelFeedInRate -= transformerObj->getLossRateForInputPower(state, subpanelFeedInRate);
            subpanelDrawRate += transformerObj->getLossRateForOutputPower(state, subpanelDrawRate);
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

Real64 ElectPowerLoadCenter::calcLoadCenterThermalLoad(EnergyPlusData &state)
{
    if (myCoGenSetupFlag_) {
        bool plantNotFound = false;
        for (auto &g : elecGenCntrlObj) {
            plantNotFound = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    g->compPlantName,
                                                    g->compPlantTypeOf_Num,
                                                    g->cogenLocation.loopNum,
                                                    g->cogenLocation.loopSideNum,
                                                    g->cogenLocation.branchNum,
                                                    g->cogenLocation.compNum,
                                                    plantNotFound,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _);
            if (!plantNotFound) g->plantInfoFound = true;
        }
        myCoGenSetupFlag_ = false;
    } // cogen setup

    // sum up "MyLoad" for all generators on this load center from plant structure
    Real64 thermalLoad = 0.0;
    for (auto &g : elecGenCntrlObj) {
        if (g->plantInfoFound) {
            thermalLoad += state.dataPlnt->PlantLoop(g->cogenLocation.loopNum)
                               .LoopSide(g->cogenLocation.loopSideNum)
                               .Branch(g->cogenLocation.branchNum)
                               .Comp(g->cogenLocation.compNum)
                               .MyLoad;
        }
    }
    return thermalLoad;
}

GeneratorController::GeneratorController(EnergyPlusData &state,
                                         std::string const &objectName,
                                         std::string const &objectType,
                                         Real64 ratedElecPowerOutput,
                                         std::string const &availSchedName,
                                         Real64 thermalToElectRatio)
    : compGenTypeOf_Num(GeneratorType::Unassigned), compPlantTypeOf_Num(0), generatorType(GeneratorType::Unassigned), generatorIndex(0),
      maxPowerOut(0.0), availSchedPtr(0), powerRequestThisTimestep(0.0), onThisTimestep(false), eMSPowerRequest(0.0), eMSRequestOn(false),
      plantInfoFound(false), cogenLocation(PlantLocation(0, 0, 0, 0)), nominalThermElectRatio(0.0), dCElectricityProd(0.0), dCElectProdRate(0.0),
      electricityProd(0.0), electProdRate(0.0), thermalProd(0.0), thermProdRate(0.0), pvwattsGenerator(nullptr), errCountNegElectProd_(0)
{

    static constexpr std::string_view routineName = "GeneratorController constructor ";

    name = objectName;
    typeOfName = objectType;
    if (UtilityRoutines::SameString(objectType, "Generator:InternalCombustionEngine")) {
        generatorType = GeneratorType::ICEngine;
        compGenTypeOf_Num = GeneratorType::ICEngine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_ICEngine;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:CombustionTurbine")) {
        generatorType = GeneratorType::CombTurbine;
        compGenTypeOf_Num = GeneratorType::CombTurbine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_CTurbine;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:MicroTurbine")) {
        generatorType = GeneratorType::Microturbine;
        compGenTypeOf_Num = GeneratorType::Microturbine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_MicroTurbine;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:Photovoltaic")) {
        generatorType = GeneratorType::PV;
        compGenTypeOf_Num = GeneratorType::PV;
        compPlantTypeOf_Num = DataPlant::TypeOf_PVTSolarCollectorFlatPlate;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:PVWatts")) {
        generatorType = GeneratorType::PVWatts;
        compGenTypeOf_Num = GeneratorType::PVWatts;
        compPlantTypeOf_Num = DataPlant::TypeOf_Other;

        int ObjNum =
            state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Generator:PVWatts", UtilityRoutines::MakeUPPERCase(objectName));
        assert(ObjNum >= 0);
        if (ObjNum == 0) {
            ShowFatalError(state, "Cannot find Generator:PVWatts " + objectName);
        }
        pvwattsGenerator = PVWatts::PVWattsGenerator::createFromIdfObj(state, ObjNum);
        pvwattsGenerator->setupOutputVariables(state);

    } else if (UtilityRoutines::SameString(objectType, "Generator:FuelCell")) {
        generatorType = GeneratorType::FuelCell;
        compGenTypeOf_Num = GeneratorType::FuelCell;
        // fuel cell has two possible plant component types, stack cooler and exhaust gas HX.
        // exhaust gas HX is required and it assumed that it has more thermal capacity and is used for control
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_FCExhaust;
        // and the name of plant component is not the same as the generator because of child object references, so fetch that name
        auto thisFC = FuelCellElectricGenerator::FCDataStruct::factory(state, name);
        compPlantName = dynamic_cast<FuelCellElectricGenerator::FCDataStruct *>(thisFC)->ExhaustHX.Name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:MicroCHP")) {
        generatorType = GeneratorType::MicroCHP;
        compGenTypeOf_Num = GeneratorType::MicroCHP;
        compPlantTypeOf_Num = DataPlant::TypeOf_Generator_MicroCHP;
        compPlantName = name;
    } else if (UtilityRoutines::SameString(objectType, "Generator:WindTurbine")) {
        generatorType = GeneratorType::WindTurbine;
        compGenTypeOf_Num = GeneratorType::WindTurbine;
        compPlantTypeOf_Num = DataPlant::TypeOf_Other;
    } else {
        ShowSevereError(state, std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + " invalid entry.");
        ShowContinueError(state, "Invalid " + objectType + " associated with generator = " + objectName);
    }

    availSched = availSchedName;
    if (availSched.empty()) {
        availSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    } else {
        availSchedPtr = ScheduleManager::GetScheduleIndex(state, availSchedName);
        if (availSchedPtr <= 0) {
            ShowSevereError(state, std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + ", invalid entry.");
            ShowContinueError(state, "Invalid availability schedule = " + availSchedName);
            ShowContinueError(state, "Schedule was not found ");
        } else {
            if (generatorType == GeneratorType::PVWatts) {
                ShowWarningError(state,
                                 std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject +
                                     ", Availability Schedule for Generator:PVWatts '" + objectName + "' will be be ignored (runs all the time).");
            } else if (generatorType == GeneratorType::PV) {
                // It should only warn if Performance type is SimplePV (DataPhotovoltaics::iSimplePVModel).
                // Except you need GetPVInput to have run already etc
                // Note: you can't use state.dataIPShortCut->cAlphaArgs etc or it'll override what will still need to be processed in
                // ElectPowerLoadCenter::ElectPowerLoadCenter after this function is called
                int PVNum =
                    state.dataInputProcessing->inputProcessor->getObjectItemNum(state, objectType, UtilityRoutines::MakeUPPERCase(objectName));
                int NumAlphas; // Number of PV Array parameter alpha names being passed
                int NumNums;   // Number of PV Array numeric parameters are being passed
                int IOStat;
                Array1D_string Alphas(5);   // Alpha items for object
                Array1D<Real64> Numbers(2); // Numeric items for object
                state.dataInputProcessing->inputProcessor->getObjectItem(state, objectType, PVNum, Alphas, NumAlphas, Numbers, NumNums, IOStat);
                if (UtilityRoutines::SameString(Alphas(3), "PhotovoltaicPerformance:Simple")) {
                    ShowWarningError(state,
                                     std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject +
                                         ", Availability Schedule for Generator:Photovoltaics '" + objectName +
                                         "' of Type PhotovoltaicPerformance:Simple will be be ignored (runs all the time).");
                    ShowContinueError(state,
                                      "To limit this Generator:Photovoltaic's output, please use the Inverter's availability schedule instead.");
                }
            }
        }
    }

    maxPowerOut = ratedElecPowerOutput, nominalThermElectRatio = thermalToElectRatio;

    SetupOutputVariable(state,
                        "Generator Requested Electricity Rate",
                        OutputProcessor::Unit::W,
                        powerRequestThisTimestep,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        objectName);
    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
        SetupEMSInternalVariable(state, "Generator Nominal Maximum Power", objectName, "[W]", maxPowerOut);
        SetupEMSInternalVariable(state, "Generator Nominal Thermal To Electric Ratio", objectName, "[ratio]", nominalThermElectRatio);
        SetupEMSActuator(state, "On-Site Generator Control", objectName, "Requested Power", "[W]", eMSRequestOn, eMSPowerRequest);
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

void GeneratorController::simGeneratorGetPowerOutput(EnergyPlusData &state,
                                                     bool const runFlag,
                                                     Real64 const myElecLoadRequest,
                                                     bool const FirstHVACIteration, // Unused 2010 JANUARY
                                                     Real64 &electricPowerOutput,   // Actual generator electric power output
                                                     Real64 &thermalPowerOutput     // Actual generator thermal power output
)
{
    // Select and call models and also collect results for load center power conditioning and reporting
    switch (generatorType) {
    case GeneratorType::ICEngine: {

        auto thisICE = ICEngineElectricGenerator::ICEngineGeneratorSpecs::factory(state, name);

        // dummy vars
        PlantLocation L(0, 0, 0, 0);
        Real64 tempLoad = myElecLoadRequest;

        // simulate
        dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->InitICEngineGenerators(state, runFlag, FirstHVACIteration);
        dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->CalcICEngineGeneratorModel(state, runFlag, tempLoad);
        dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->update(state);
        electProdRate = dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->ElecPowerGenerated;
        electricityProd = dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->ElecEnergyGenerated;
        thermProdRate = dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->QTotalHeatRecovered;
        thermalProd = dynamic_cast<ICEngineElectricGenerator::ICEngineGeneratorSpecs *>(thisICE)->TotalHeatEnergyRec;
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::CombTurbine: {

        auto thisCTE = CTElectricGenerator::CTGeneratorData::factory(state, name);
        // dummy vars
        PlantLocation L(0, 0, 0, 0);
        Real64 tempLoad = myElecLoadRequest;

        // simulate
        thisCTE->simulate(state, L, FirstHVACIteration, tempLoad, runFlag);
        dynamic_cast<CTElectricGenerator::CTGeneratorData *>(thisCTE)->InitCTGenerators(state, runFlag, FirstHVACIteration);
        dynamic_cast<CTElectricGenerator::CTGeneratorData *>(thisCTE)->CalcCTGeneratorModel(state, runFlag, tempLoad, FirstHVACIteration);
        electProdRate = dynamic_cast<CTElectricGenerator::CTGeneratorData *>(thisCTE)->ElecPowerGenerated;
        electricityProd = dynamic_cast<CTElectricGenerator::CTGeneratorData *>(thisCTE)->ElecEnergyGenerated;
        thermProdRate = dynamic_cast<CTElectricGenerator::CTGeneratorData *>(thisCTE)->QTotalHeatRecovered;
        thermalProd = dynamic_cast<CTElectricGenerator::CTGeneratorData *>(thisCTE)->TotalHeatEnergyRec;
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::PV: {
        Photovoltaics::SimPVGenerator(state, GeneratorType::PV, name, generatorIndex, runFlag, myElecLoadRequest);
        Photovoltaics::GetPVGeneratorResults(
            state, GeneratorType::PV, generatorIndex, dCElectProdRate, dCElectricityProd, thermProdRate, thermalProd);
        electricPowerOutput = dCElectProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::PVWatts: {
        pvwattsGenerator->calc(state);
        pvwattsGenerator->getResults(dCElectProdRate, dCElectricityProd, thermProdRate, thermalProd);
        electricPowerOutput = dCElectProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::FuelCell: {
        auto thisFC = FuelCellElectricGenerator::FCDataStruct::factory(state, name);
        dynamic_cast<FuelCellElectricGenerator::FCDataStruct *>(thisFC)->SimFuelCellGenerator(state, runFlag, myElecLoadRequest, FirstHVACIteration);
        electProdRate = dynamic_cast<FuelCellElectricGenerator::FCDataStruct *>(thisFC)->Report.ACPowerGen;
        electricityProd = dynamic_cast<FuelCellElectricGenerator::FCDataStruct *>(thisFC)->Report.ACEnergyGen;
        thermProdRate = dynamic_cast<FuelCellElectricGenerator::FCDataStruct *>(thisFC)->Report.qHX;
        thermalProd = dynamic_cast<FuelCellElectricGenerator::FCDataStruct *>(thisFC)->Report.HXenergy;
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::MicroCHP: {
        auto thisMCHP = MicroCHPElectricGenerator::MicroCHPDataStruct::factory(state, name);

        // simulate
        dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->InitMicroCHPNoNormalizeGenerators(state);

        if (!state.dataPlnt->PlantFirstSizeCompleted) break;

        dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->CalcMicroCHPNoNormalizeGeneratorModel(
            state, runFlag, false, myElecLoadRequest, DataPrecisionGlobals::constant_zero, FirstHVACIteration);
        dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->CalcUpdateHeatRecovery(state);
        dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->UpdateMicroCHPGeneratorRecords(state);

        electProdRate = dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->A42Model.ACPowerGen;
        electricityProd = dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->A42Model.ACEnergyGen;
        thermProdRate = dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->A42Model.QdotHR;
        thermalProd = dynamic_cast<MicroCHPElectricGenerator::MicroCHPDataStruct *>(thisMCHP)->A42Model.TotalHeatEnergyRec;
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::Microturbine: {
        auto thisMTG = MicroturbineElectricGenerator::MTGeneratorSpecs::factory(state, name);

        // dummy vars
        PlantLocation L(0, 0, 0, 0);
        Real64 tempLoad = myElecLoadRequest;

        // simulate
        dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->InitMTGenerators(state, runFlag, tempLoad, FirstHVACIteration);
        dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->CalcMTGeneratorModel(state, runFlag, tempLoad);
        dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->UpdateMTGeneratorRecords(state);
        electProdRate = dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->ElecPowerGenerated;
        electricityProd = dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->EnergyGen;
        thermProdRate = dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->QHeatRecovered;
        thermalProd = dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->ExhaustEnergyRec;
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::WindTurbine: {
        WindTurbine::SimWindTurbine(state, GeneratorType::WindTurbine, name, generatorIndex, runFlag, myElecLoadRequest);
        WindTurbine::GetWTGeneratorResults(
            state, GeneratorType::WindTurbine, generatorIndex, electProdRate, electricityProd, thermProdRate, thermalProd);
        electricPowerOutput = electProdRate;
        thermalPowerOutput = thermProdRate;
        break;
    }
    case GeneratorType::Unassigned: {
        // do nothing
        break;
    }
    } // end switch

    // check if generator production has gone wrong and is negative, reset to zero and warn
    if (electricPowerOutput < 0.0) {
        if (errCountNegElectProd_ == 0) {
            ShowWarningMessage(state, typeOfName + " named " + name + " is producing negative electric power, check generator inputs.");
            ShowContinueError(state, format("Electric power production rate ={:.4R}", electricPowerOutput));
            ShowContinueError(state, "The power will be set to zero, and the simulation continues... ");
        }
        ShowRecurringWarningErrorAtEnd(state,
                                       typeOfName + " named " + name + " is producing negative electric power ",
                                       errCountNegElectProd_,
                                       electricPowerOutput,
                                       electricPowerOutput);
        electricPowerOutput = 0.0;
    }
}

DCtoACInverter::DCtoACInverter(EnergyPlusData &state, std::string const &objectName)
    : aCPowerOut_(0.0), aCEnergyOut_(0.0), efficiency_(0.0), dCPowerIn_(0.0), dCEnergyIn_(0.0), conversionLossPower_(0.0), conversionLossEnergy_(0.0),
      conversionLossEnergyDecrement_(0.0), thermLossRate_(0.0), thermLossEnergy_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0), ancillACuseRate_(0.0),
      ancillACuseEnergy_(0.0), modelType_(InverterModelType::notYetSet), availSchedPtr_(0),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0), zoneRadFract_(0.0), nominalVoltage_(0.0),
      nomVoltEfficiencyARR_(6, 0.0), curveNum_(0), ratedPower_(0.0), minPower_(0.0), maxPower_(0.0), minEfficiency_(0.0), maxEfficiency_(0.0),
      standbyPower_(0.0)
{
    // initialize
    nomVoltEfficiencyARR_.resize(6, 0.0);

    static constexpr std::string_view routineName = "DCtoACInverter constructor ";
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    // if/when add object class name to input object this can be simplified. for now search all possible types
    bool foundInverter = false;
    int testInvertIndex = 0;
    int invertIDFObjectNum = 0;

    testInvertIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Inverter:LookUpTable", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Inverter:LookUpTable";
        modelType_ = InverterModelType::cECLookUpTableModel;
    }
    testInvertIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Inverter:FunctionOfPower", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Inverter:FunctionOfPower";
        modelType_ = InverterModelType::curveFuncOfPower;
    }
    testInvertIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Inverter:Simple", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Inverter:Simple";
        modelType_ = InverterModelType::simpleConstantEff;
    }
    testInvertIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Inverter:PVWatts", objectName);
    if (testInvertIndex > 0) {
        foundInverter = true;
        invertIDFObjectNum = testInvertIndex;
        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Inverter:PVWatts";
        modelType_ = InverterModelType::pvWatts;
    }

    if (foundInverter) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 invertIDFObjectNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        name_ = state.dataIPShortCut->cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?

        if (modelType_ == InverterModelType::pvWatts) {
            availSchedPtr_ = DataGlobalConstants::ScheduleAlwaysOn;
            zoneNum_ = 0;
            heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            zoneRadFract_ = 0;
        } else {
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                availSchedPtr_ = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                availSchedPtr_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (availSchedPtr_ == 0) {
                    ShowSevereError(state,
                                    std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    errorsFound = true;
                }
            }

            zoneNum_ = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);
            if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
            if (zoneNum_ == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                } else {
                    heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                    ShowWarningError(state,
                                     std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                    ShowContinueError(state, "Zone name not found. Inverter heat losses will not be added to a zone");
                    // continue with simulation but inverter losses not sent to a zone.
                }
            }
            zoneRadFract_ = state.dataIPShortCut->rNumericArgs(1);
        }

        // now the input objects differ depending on class type
        switch (modelType_) {
        case InverterModelType::cECLookUpTableModel: {
            ratedPower_ = state.dataIPShortCut->rNumericArgs(2);
            standbyPower_ = state.dataIPShortCut->rNumericArgs(3);

            nominalVoltage_ = state.dataIPShortCut->rNumericArgs(4);
            nomVoltEfficiencyARR_[0] = state.dataIPShortCut->rNumericArgs(5);
            nomVoltEfficiencyARR_[1] = state.dataIPShortCut->rNumericArgs(6);
            nomVoltEfficiencyARR_[2] = state.dataIPShortCut->rNumericArgs(7);
            nomVoltEfficiencyARR_[3] = state.dataIPShortCut->rNumericArgs(8);
            nomVoltEfficiencyARR_[4] = state.dataIPShortCut->rNumericArgs(9);
            nomVoltEfficiencyARR_[5] = state.dataIPShortCut->rNumericArgs(10);
            break;
        }
        case InverterModelType::curveFuncOfPower: {
            curveNum_ = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (curveNum_ == 0) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                ShowContinueError(state, "Curve was not found");
                errorsFound = true;
            }

            ratedPower_ = state.dataIPShortCut->rNumericArgs(2);
            minEfficiency_ = state.dataIPShortCut->rNumericArgs(3);
            maxEfficiency_ = state.dataIPShortCut->rNumericArgs(4);
            minPower_ = state.dataIPShortCut->rNumericArgs(5);
            maxPower_ = state.dataIPShortCut->rNumericArgs(6);
            standbyPower_ = state.dataIPShortCut->rNumericArgs(7);
            break;
        }
        case InverterModelType::simpleConstantEff: {
            efficiency_ = state.dataIPShortCut->rNumericArgs(2);
            break;
        }
        case InverterModelType::notYetSet: {
            // do nothing
            break;
        }
        case InverterModelType::pvWatts: {
            pvWattsDCtoACSizeRatio_ = state.dataIPShortCut->rNumericArgs(1);
            pvWattsInverterEfficiency_ = state.dataIPShortCut->rNumericArgs(2);
            break;
        }

        } // end switch modelType

        SetupOutputVariable(state,
                            "Inverter DC to AC Efficiency",
                            OutputProcessor::Unit::None,
                            efficiency_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Inverter DC Input Electricity Rate",
                            OutputProcessor::Unit::W,
                            dCPowerIn_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Inverter DC Input Electricity Energy",
                            OutputProcessor::Unit::J,
                            dCEnergyIn_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Inverter AC Output Electricity Rate",
                            OutputProcessor::Unit::W,
                            aCPowerOut_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Inverter AC Output Electricity Energy",
                            OutputProcessor::Unit::J,
                            aCEnergyOut_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Inverter Conversion Loss Power",
                            OutputProcessor::Unit::W,
                            conversionLossPower_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Inverter Conversion Loss Energy",
                            OutputProcessor::Unit::J,
                            conversionLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Inverter Conversion Loss Decrement Energy",
                            OutputProcessor::Unit::J,
                            conversionLossEnergyDecrement_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricityProduced",
                            "POWERCONVERSION",
                            _,
                            "Plant");
        SetupOutputVariable(state,
                            "Inverter Thermal Loss Rate",
                            OutputProcessor::Unit::W,
                            thermLossRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Inverter Thermal Loss Energy",
                            OutputProcessor::Unit::J,
                            thermLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Inverter Ancillary AC Electricity Rate",
                            OutputProcessor::Unit::W,
                            ancillACuseRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Inverter Ancillary AC Electricity Energy",
                            OutputProcessor::Unit::J,
                            ancillACuseEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "Electricity",
                            "Cogeneration",
                            "DCtoACInverter Ancillary",
                            "Plant"); // called cogeneration for end use table
        if (zoneNum_ > 0) {
            switch (modelType_) {
            case InverterModelType::simpleConstantEff: {
                SetupZoneInternalGain(state,
                                      zoneNum_,
                                      "ElectricLoadCenter:Inverter:Simple",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterSimple,
                                      &qdotConvZone_,
                                      nullptr,
                                      &qdotRadZone_);
                break;
            }
            case InverterModelType::curveFuncOfPower: {
                SetupZoneInternalGain(state,
                                      zoneNum_,
                                      "ElectricLoadCenter:Inverter:FunctionOfPower",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,
                                      &qdotConvZone_,
                                      nullptr,
                                      &qdotRadZone_);
                break;
            }
            case InverterModelType::cECLookUpTableModel: {
                SetupZoneInternalGain(state,
                                      zoneNum_,
                                      "ElectricLoadCenter:Inverter:LookUpTable",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,
                                      &qdotConvZone_,
                                      nullptr,
                                      &qdotRadZone_);
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
        ShowSevereError(state, std::string{routineName} + " did not find inverter name = " + objectName);
        errorsFound = true;
    }

    if (errorsFound) {
        ShowFatalError(state, std::string{routineName} + "Preceding errors terminate program.");
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

void DCtoACInverter::setPVWattsDCCapacity(EnergyPlusData &state, const Real64 dcCapacity)
{
    if (modelType_ != InverterModelType::pvWatts) {
        ShowFatalError(state, "Setting the DC Capacity for the inverter only works with PVWatts Inverters.");
    }
    ratedPower_ = dcCapacity / pvWattsDCtoACSizeRatio_;
}

Real64 DCtoACInverter::pvWattsDCCapacity()
{
    return ratedPower_ * pvWattsDCtoACSizeRatio_;
}

Real64 DCtoACInverter::pvWattsInverterEfficiency()
{
    return pvWattsInverterEfficiency_;
}

Real64 DCtoACInverter::pvWattsDCtoACSizeRatio()
{
    return pvWattsDCtoACSizeRatio_;
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

Real64 DCtoACInverter::getLossRateForOutputPower(EnergyPlusData &state, Real64 const powerOutOfInverter)
{

    // need to invert, find a dCPowerIn that produces the desired AC power out
    // use last efficiency for initial guess
    if (efficiency_ > 0.0) {
        dCPowerIn_ = powerOutOfInverter / efficiency_;
    } else {
        dCPowerIn_ = powerOutOfInverter;
        calcEfficiency(state);
        dCPowerIn_ = powerOutOfInverter / efficiency_;
    }

    calcEfficiency(state);
    // one more update is close enough.
    if (efficiency_ > 0.0) {
        dCPowerIn_ = powerOutOfInverter / efficiency_;
    }
    calcEfficiency(state);
    return (1.0 - efficiency_) * dCPowerIn_;
}

void DCtoACInverter::calcEfficiency(EnergyPlusData &state)
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
        efficiency_ = CurveManager::CurveValue(state, curveNum_, normalizedPower);
        efficiency_ = max(efficiency_, minEfficiency_);
        efficiency_ = min(efficiency_, maxEfficiency_);

        break;
    }
    case InverterModelType::pvWatts: {
        // This code is lifted from ssc cmod_pvwatts5.cpp:powerout() method.
        // It was easier to do this calculation here because we have a many to one relationship between inverter
        // and generator whereas theirs is one to one.
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
            // make sure no negative AC values (no parasitic nighttime losses calculated)
            if (ac < 0) ac = 0;
            efficiency_ = ac / dCPowerIn_;
        } else {
            efficiency_ = 1.0; // Set to a non-zero reasonable value (to avoid divide by zero error)
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

void DCtoACInverter::simulate(EnergyPlusData &state, Real64 const powerIntoInverter)
{
    dCPowerIn_ = powerIntoInverter;
    dCEnergyIn_ = dCPowerIn_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    // check availability schedule
    if (ScheduleManager::GetCurrentScheduleValue(state, availSchedPtr_) > 0.0) {

        // now calculate Inverter based on model type
        calcEfficiency(state);
        aCPowerOut_ = efficiency_ * dCPowerIn_;
        aCEnergyOut_ = aCPowerOut_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);

        if (aCPowerOut_ == 0.0) {
            ancillACuseEnergy_ = standbyPower_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
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
    conversionLossEnergy_ = conversionLossPower_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    conversionLossEnergyDecrement_ = -1.0 * conversionLossEnergy_;
    thermLossRate_ = dCPowerIn_ - aCPowerOut_ + ancillACuseRate_;
    thermLossEnergy_ = thermLossRate_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    qdotConvZone_ = thermLossRate_ * (1.0 - zoneRadFract_);
    qdotRadZone_ = thermLossRate_ * zoneRadFract_;
}

ACtoDCConverter::ACtoDCConverter(EnergyPlusData &state, std::string const &objectName)
    : efficiency_(0.0), aCPowerIn_(0.0), aCEnergyIn_(0.0), dCPowerOut_(0.0), dCEnergyOut_(0.0), conversionLossPower_(0.0), conversionLossEnergy_(0.0),
      conversionLossEnergyDecrement_(0.0), thermLossRate_(0.0), thermLossEnergy_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0), ancillACuseRate_(0.0),
      ancillACuseEnergy_(0.0), availSchedPtr_(0), modelType_(ConverterModelType::notYetSet),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0),
      zoneRadFract_(0.0), // radiative fraction for thermal losses to zone
      standbyPower_(0.0), maxPower_(0.0)
{

    static constexpr std::string_view routineName = "ACtoDCConverter constructor ";
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    // if/when add object class name to input object this can be simplified. for now search all possible types

    int testConvertIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Storage:Converter", objectName);

    if (testConvertIndex > 0) {
        state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Storage:Converter";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 testConvertIndex,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        name_ = state.dataIPShortCut->cAlphaArgs(1);
        // need a new general approach for verify names are unique across objects,  next gen GlobalNames

        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            availSchedPtr_ = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            availSchedPtr_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (availSchedPtr_ == 0) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                errorsFound = true;
            }
        }

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "SimpleFixed")) {
            modelType_ = ConverterModelType::simpleConstantEff;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "FunctionOfPower")) {
            modelType_ = ConverterModelType::curveFuncOfPower;
        } else {
            ShowSevereError(state,
                            std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", invalid entry.");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
            errorsFound = true;
        }

        switch (modelType_) {
        case ConverterModelType::simpleConstantEff: {
            efficiency_ = state.dataIPShortCut->rNumericArgs(1);
            break;
        }

        case ConverterModelType::curveFuncOfPower: {
            maxPower_ = state.dataIPShortCut->rNumericArgs(2);
            curveNum_ = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (curveNum_ == 0) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                ShowContinueError(state, "Curve was not found");
                errorsFound = true;
            }
            break;
        }
        case ConverterModelType::notYetSet: {
            // do nothing
        }
        } // end switch

        standbyPower_ = state.dataIPShortCut->rNumericArgs(3);

        zoneNum_ = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(5), state.dataHeatBal->Zone);
        if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
        if (zoneNum_ == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            } else {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                ShowWarningError(state,
                                 std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                ShowContinueError(state, "Zone name not found. Inverter heat losses will not be added to a zone");
                // continue with simulation but inverter losses not sent to a zone.
            }
        }
        zoneRadFract_ = state.dataIPShortCut->rNumericArgs(4);

        SetupOutputVariable(state,
                            "Converter AC to DC Efficiency",
                            OutputProcessor::Unit::None,
                            efficiency_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Converter AC Input Electricity Rate",
                            OutputProcessor::Unit::W,
                            aCPowerIn_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Converter AC Input Electricity Energy",
                            OutputProcessor::Unit::J,
                            aCEnergyIn_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Converter DC Output Electricity Rate",
                            OutputProcessor::Unit::W,
                            dCPowerOut_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Converter DC Output Electricity Energy",
                            OutputProcessor::Unit::J,
                            dCEnergyOut_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Converter Electricity Loss Rate",
                            OutputProcessor::Unit::W,
                            conversionLossPower_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Converter Electricity Loss Energy",
                            OutputProcessor::Unit::J,
                            conversionLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Converter Electricity Loss Decrement Energy",
                            OutputProcessor::Unit::J,
                            conversionLossEnergyDecrement_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricityProduced",
                            "POWERCONVERSION",
                            _,
                            "Plant");
        SetupOutputVariable(state,
                            "Converter Thermal Loss Rate",
                            OutputProcessor::Unit::W,
                            thermLossRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Converter Thermal Loss Energy",
                            OutputProcessor::Unit::J,
                            thermLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Converter Ancillary AC Electricity Rate",
                            OutputProcessor::Unit::W,
                            ancillACuseRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Converter Ancillary AC Electricity Energy",
                            OutputProcessor::Unit::J,
                            ancillACuseEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "Electricity",
                            "Cogeneration",
                            "ACtoDCConverter Ancillary",
                            "Plant"); // called cogeneration for end use table
        if (zoneNum_ > 0) {
            SetupZoneInternalGain(state,
                                  zoneNum_,
                                  "ElectricLoadCenter:Storage:Converter",
                                  name_,
                                  DataHeatBalance::IntGainTypeOf_ElectricLoadCenterConverter,
                                  &qdotConvZone_,
                                  nullptr,
                                  &qdotRadZone_);
        }
    } else {
        ShowSevereError(state, std::string{routineName} + " did not find power converter name = " + objectName);
        errorsFound = true;
    }

    if (errorsFound) {
        ShowFatalError(state, std::string{routineName} + "Preceding errors terminate program.");
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

Real64 ACtoDCConverter::getLossRateForInputPower(EnergyPlusData &state, Real64 const powerIntoConverter)
{
    aCPowerIn_ = powerIntoConverter;
    calcEfficiency(state);
    return (1.0 - efficiency_) * aCPowerIn_;
}

std::string const &ACtoDCConverter::name() const
{
    return name_;
}

void ACtoDCConverter::calcEfficiency(EnergyPlusData &state)
{
    switch (modelType_) {
    case ConverterModelType::notYetSet:
    case ConverterModelType::simpleConstantEff: {
        break;
    }
    case ConverterModelType::curveFuncOfPower: {
        Real64 normalizedPower = aCPowerIn_ / maxPower_;
        efficiency_ = CurveManager::CurveValue(state, curveNum_, normalizedPower);
        break;
    }
    } // end switch
}

void ACtoDCConverter::simulate(EnergyPlusData &state, Real64 const powerOutFromConverter)
{
    // need to invert, find an aCPowerIn that produces the desired DC power out

    // use last efficiency for initial guess
    if (ScheduleManager::GetCurrentScheduleValue(state, availSchedPtr_) > 0.0) {

        aCPowerIn_ = powerOutFromConverter / efficiency_;
        calcEfficiency(state), aCPowerIn_ = powerOutFromConverter / efficiency_;
        calcEfficiency(state),

            dCPowerOut_ = aCPowerIn_ * efficiency_;

        if (dCPowerOut_ == 0.0) {
            ancillACuseEnergy_ = standbyPower_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
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
    aCEnergyIn_ = aCPowerIn_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    dCEnergyOut_ = dCPowerOut_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    conversionLossPower_ = aCPowerIn_ - dCPowerOut_;
    conversionLossEnergy_ = conversionLossPower_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    conversionLossEnergyDecrement_ = -1.0 * conversionLossEnergy_;
    thermLossRate_ = aCPowerIn_ - dCPowerOut_ + ancillACuseRate_;
    thermLossEnergy_ = thermLossRate_ * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    qdotConvZone_ = thermLossRate_ * (1.0 - zoneRadFract_);
    qdotRadZone_ = thermLossRate_ * zoneRadFract_;
}

ElectricStorage::ElectricStorage( // main constructor
    EnergyPlusData &state,
    std::string const &objectName)
    : storedPower_(0.0), storedEnergy_(0.0), drawnPower_(0.0), drawnEnergy_(0.0), decrementedEnergyStored_(0.0), maxRainflowArrayBounds_(100),
      maxRainflowArrayInc_(100), myWarmUpFlag_(false), storageModelMode_(StorageModelType::storageTypeNotSet), availSchedPtr_(0),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0), zoneRadFract_(0.0), startingEnergyStored_(0.0),
      energeticEfficCharge_(0.0), energeticEfficDischarge_(0.0), maxPowerDraw_(0.0), maxPowerStore_(0.0), maxEnergyCapacity_(0.0), parallelNum_(0),
      seriesNum_(0), numBattery_(0), chargeCurveNum_(0), dischargeCurveNum_(0), cycleBinNum_(0), startingSOC_(0.0), maxAhCapacity_(0.0),
      availableFrac_(0.0), chargeConversionRate_(0.0), chargedOCV_(0.0), dischargedOCV_(0.0), internalR_(0.0), maxDischargeI_(0.0), cutoffV_(0.0),
      maxChargeRate_(0.0), lifeCalculation_(BatteryDegradationModelType::degredationNotSet), lifeCurveNum_(0), liIon_dcToDcChargingEff_(0.0),
      liIon_mass_(0.0), liIon_surfaceArea_(0.0), liIon_Cp_(0.0), liIon_heatTransferCoef_(0.0), liIon_Vfull_(0.0), liIon_Vexp_(0.0), liIon_Vnom_(0.0),
      liIon_Vnom_default_(0.0), liIon_Qfull_(0.0), liIon_Qexp_(0.0), liIon_Qnom_(0.0), liIon_C_rate_(0.0), thisTimeStepStateOfCharge_(0.0),
      lastTimeStepStateOfCharge_(0.0), pelNeedFromStorage_(0.0), pelFromStorage_(0.0), pelIntoStorage_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0),
      timeElapsed_(0.0), thisTimeStepAvailable_(0.0), thisTimeStepBound_(0.0), lastTimeStepAvailable_(0.0), lastTimeStepBound_(0.0),
      lastTwoTimeStepAvailable_(0.0), lastTwoTimeStepBound_(0.0), count0_(0), electEnergyinStorage_(0.0), thermLossRate_(0.0), thermLossEnergy_(0.0),
      storageMode_(0), absoluteSOC_(0.0), fractionSOC_(0.0), batteryCurrent_(0.0), batteryVoltage_(0.0), batteryDamage_(0.0), batteryTemperature_(0.0)
{

    static constexpr std::string_view routineName = "ElectricStorage constructor ";
    int numAlphas; // Number of elements in the alpha array
    int numNums;   // Number of elements in the numeric array
    int iOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    // if/when add object class name to input object this can be simplified. for now search all possible types
    bool foundStorage = false;
    int testStorageIndex = 0;
    int storageIDFObjectNum = 0;

    const std::array<std::pair<std::string, StorageModelType>, 3> storageTypes{
        {{"ElectricLoadCenter:Storage:Simple", StorageModelType::simpleBucketStorage},
         {"ElectricLoadCenter:Storage:Battery", StorageModelType::kiBaMBattery},
         {"ElectricLoadCenter:Storage:LiIonNMCBattery", StorageModelType::liIonNmcBattery}}};

    for (auto &item : storageTypes) {
        testStorageIndex = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, item.first, objectName);
        if (testStorageIndex > 0) {
            foundStorage = true;
            storageIDFObjectNum = testStorageIndex;
            state.dataIPShortCut->cCurrentModuleObject = item.first;
            storageModelMode_ = item.second;
            break;
        }
    }

    if (foundStorage) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 storageIDFObjectNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 numAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 numNums,
                                                                 iOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        name_ = state.dataIPShortCut->cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?

        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            availSchedPtr_ = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            availSchedPtr_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (availSchedPtr_ == 0) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                errorsFound = true;
            }
        }

        zoneNum_ = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);
        if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
        if (zoneNum_ == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            } else {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                ShowWarningError(state,
                                 std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                ShowContinueError(state, "Zone name not found. Storage heat losses will not be added to a zone");
                // continue with simulation but storage losses not sent to a zone.
            }
        }
        zoneRadFract_ = state.dataIPShortCut->rNumericArgs(1);

        switch (storageModelMode_) {

        case StorageModelType::simpleBucketStorage: {
            energeticEfficCharge_ = checkUserEfficiencyInput(state, state.dataIPShortCut->rNumericArgs(2), "CHARGING", name_, errorsFound);
            energeticEfficDischarge_ = checkUserEfficiencyInput(state, state.dataIPShortCut->rNumericArgs(3), "DISCHARGING", name_, errorsFound);
            maxEnergyCapacity_ = state.dataIPShortCut->rNumericArgs(4);
            maxPowerDraw_ = state.dataIPShortCut->rNumericArgs(5);
            maxPowerStore_ = state.dataIPShortCut->rNumericArgs(6);
            startingEnergyStored_ = state.dataIPShortCut->rNumericArgs(7);
            SetupOutputVariable(state,
                                "Electric Storage Simple Charge State",
                                OutputProcessor::Unit::J,
                                electEnergyinStorage_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_); // issue #4921
            break;
        }

        case StorageModelType::kiBaMBattery: {
            chargeCurveNum_ = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4)); // voltage calculation for charging
            if (chargeCurveNum_ == 0 && !state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + '=' + state.dataIPShortCut->cAlphaArgs(4));
                errorsFound = true;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " cannot be blank. But no entry found.");
                errorsFound = true;
            } else {
                errorsFound |= CurveManager::CheckCurveDims(state,
                                                            chargeCurveNum_,                            // Curve index
                                                            {1},                                        // Valid dimensions
                                                            routineName,                                // Routine name
                                                            state.dataIPShortCut->cCurrentModuleObject, // Object Type
                                                            name_,                                      // Object Name
                                                            state.dataIPShortCut->cAlphaFieldNames(4)); // Field Name
            }
            dischargeCurveNum_ = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5)); // voltage calculation for discharging
            if (dischargeCurveNum_ == 0 && !state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + '=' + state.dataIPShortCut->cAlphaArgs(5));
                errorsFound = true;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " cannot be blank. But no entry found.");
                errorsFound = true;
            } else {
                errorsFound |= CurveManager::CheckCurveDims(state,
                                                            dischargeCurveNum_,                         // Curve index
                                                            {1},                                        // Valid dimensions
                                                            routineName,                                // Routine name
                                                            state.dataIPShortCut->cCurrentModuleObject, // Object Type
                                                            name_,                                      // Object Name
                                                            state.dataIPShortCut->cAlphaFieldNames(5)); // Field Name
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "Yes")) {
                lifeCalculation_ = BatteryDegradationModelType::lifeCalculationYes;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "No")) {
                lifeCalculation_ = BatteryDegradationModelType::lifeCalculationNo;
            } else {
                ShowWarningError(state,
                                 std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, "Yes or No should be selected. Default value No is used to continue simulation");
                lifeCalculation_ = BatteryDegradationModelType::lifeCalculationNo;
            }

            if (lifeCalculation_ == BatteryDegradationModelType::lifeCalculationYes) {
                lifeCurveNum_ = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(7)); // Battery life calculation
                if (lifeCurveNum_ == 0 && !state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    ShowSevereError(state,
                                    std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                    errorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    ShowSevereError(state,
                                    std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                    ShowContinueError(state,
                                      "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + " cannot be blank when " +
                                          state.dataIPShortCut->cAlphaArgs(6) + " = Yes. But no entry found.");
                    errorsFound = true;
                } else {
                    errorsFound |= CurveManager::CheckCurveDims(state,
                                                                lifeCurveNum_,                              // Curve index
                                                                {1},                                        // Valid dimensions
                                                                routineName,                                // Routine name
                                                                state.dataIPShortCut->cCurrentModuleObject, // Object Type
                                                                name_,                                      // Object Name
                                                                state.dataIPShortCut->cAlphaFieldNames(7)); // Field Name
                }

                cycleBinNum_ = state.dataIPShortCut->rNumericArgs(14);

                if (!errorsFound) { // life cycle calculation for this battery, allocate arrays for degradation calculation
                                    // std::vector is zero base instead of 1, so first index is now 0.
                    b10_.resize(maxRainflowArrayBounds_ + 1, 0.0);
                    x0_.resize(maxRainflowArrayBounds_ + 1, 0);
                    nmb0_.resize(cycleBinNum_, 0.0);
                    oneNmb0_.resize(cycleBinNum_, 0.0);
                }
            }

            parallelNum_ = state.dataIPShortCut->rNumericArgs(2);
            seriesNum_ = state.dataIPShortCut->rNumericArgs(3);
            numBattery_ = parallelNum_ * seriesNum_;
            maxAhCapacity_ = state.dataIPShortCut->rNumericArgs(4);
            startingSOC_ = state.dataIPShortCut->rNumericArgs(5);
            availableFrac_ = state.dataIPShortCut->rNumericArgs(6);
            chargeConversionRate_ = state.dataIPShortCut->rNumericArgs(7);
            chargedOCV_ = state.dataIPShortCut->rNumericArgs(8);
            dischargedOCV_ = state.dataIPShortCut->rNumericArgs(9);
            internalR_ = state.dataIPShortCut->rNumericArgs(10);
            maxDischargeI_ = state.dataIPShortCut->rNumericArgs(11);
            cutoffV_ = state.dataIPShortCut->rNumericArgs(12);
            maxChargeRate_ = state.dataIPShortCut->rNumericArgs(13);

            break;
        }
        case StorageModelType::liIonNmcBattery: {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "KandlerSmith") || state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                lifeCalculation_ = BatteryDegradationModelType::lifeCalculationYes;
            } else {
                lifeCalculation_ = BatteryDegradationModelType::lifeCalculationNo;
            }
            seriesNum_ = static_cast<int>(state.dataIPShortCut->rNumericArgs(2));
            parallelNum_ = static_cast<int>(state.dataIPShortCut->rNumericArgs(3));
            startingSOC_ = state.dataIPShortCut->lNumericFieldBlanks(4) ? 0.5 : state.dataIPShortCut->rNumericArgs(4);
            liIon_dcToDcChargingEff_ = state.dataIPShortCut->lNumericFieldBlanks(5) ? 0.95 : state.dataIPShortCut->rNumericArgs(5);
            liIon_mass_ = state.dataIPShortCut->rNumericArgs(6);
            liIon_surfaceArea_ = state.dataIPShortCut->rNumericArgs(7);
            liIon_Cp_ = state.dataIPShortCut->lNumericFieldBlanks(8) ? 1500.0 : state.dataIPShortCut->rNumericArgs(8);
            liIon_heatTransferCoef_ = state.dataIPShortCut->lNumericFieldBlanks(9) ? 7.5 : state.dataIPShortCut->rNumericArgs(9);
            liIon_Vfull_ = state.dataIPShortCut->lNumericFieldBlanks(10) ? 4.2 : state.dataIPShortCut->rNumericArgs(10);
            liIon_Vexp_ = state.dataIPShortCut->lNumericFieldBlanks(11) ? 3.53 : state.dataIPShortCut->rNumericArgs(11);
            liIon_Vnom_ = state.dataIPShortCut->lNumericFieldBlanks(12) ? 3.342 : state.dataIPShortCut->rNumericArgs(12);
            if (liIon_Vfull_ < liIon_Vexp_ || liIon_Vexp_ < liIon_Vnom_) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state,
                                  state.dataIPShortCut->cNumericFieldNames(10) + " must be greater than " +
                                      state.dataIPShortCut->cNumericFieldNames(11) + ",");
                ShowContinueError(state, "which must be greater than " + state.dataIPShortCut->cNumericFieldNames(12) + ".");
                for (int i = 10; i <= 12; ++i) {
                    ShowContinueError(state,
                                      format("{} = {:.3R}", state.dataIPShortCut->cNumericFieldNames(i), state.dataIPShortCut->rNumericArgs(i)));
                }
                errorsFound = true;
            }
            liIon_Vnom_default_ = state.dataIPShortCut->lNumericFieldBlanks(13) ? 3.342 : state.dataIPShortCut->rNumericArgs(13);
            liIon_Qfull_ = state.dataIPShortCut->lNumericFieldBlanks(14) ? 3.2 : state.dataIPShortCut->rNumericArgs(14);
            liIon_Qexp_ =
                state.dataIPShortCut->lNumericFieldBlanks(15) ? 0.8075 * liIon_Qfull_ : state.dataIPShortCut->rNumericArgs(15) * liIon_Qfull_;
            liIon_Qnom_ =
                state.dataIPShortCut->lNumericFieldBlanks(16) ? 0.976875 * liIon_Qfull_ : state.dataIPShortCut->rNumericArgs(16) * liIon_Qfull_;
            if (liIon_Qexp_ >= liIon_Qnom_) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state,
                                  state.dataIPShortCut->cNumericFieldNames(16) + " must be greater than " +
                                      state.dataIPShortCut->cNumericFieldNames(15) + ".");
                for (int i = 15; i <= 16; ++i) {
                    ShowContinueError(state,
                                      format("{} = {:.3R}", state.dataIPShortCut->cNumericFieldNames(i), state.dataIPShortCut->rNumericArgs(i)));
                }
                errorsFound = true;
            }
            liIon_C_rate_ = state.dataIPShortCut->lNumericFieldBlanks(17) ? 1.0 : state.dataIPShortCut->rNumericArgs(17);
            internalR_ = state.dataIPShortCut->lNumericFieldBlanks(18) ? 0.09 : state.dataIPShortCut->rNumericArgs(18);

            maxAhCapacity_ = liIon_Qfull_ * parallelNum_;

            if (!errorsFound) {
                // Set the Lifetime model in SSC
                // I'm using a raw pointer here because the the battery_t constructor expects it.
                // The pointer is then passed into the battery_t where it is converted into a unique_ptr and persists along with that object.
                // Therefore I am not deleting this pointer here because that will be handled by the battery_t class.
                lifetime_t *battLifetime;
                if (lifeCalculation_ == BatteryDegradationModelType::lifeCalculationYes) {
                    battLifetime = new lifetime_nmc_t(state.dataHVACGlobal->TimeStepSys);
                } else {
                    // This sets a lifetime model where the capacity is always 100%.
                    std::vector<double> tblVals{{20, 0, 100, 20, 5000, 100, 20, 10000, 100, 80, 0, 100, 80, 1000, 100, 80, 2000, 100}};
                    util::matrix_t<double> battLifetimeMatrix(6, 3, &tblVals);
                    battLifetime = new lifetime_calendar_cycle_t(battLifetimeMatrix, state.dataHVACGlobal->TimeStepSys);
                }

                // Create the SSC battery object
                ssc_battery_ = std::unique_ptr<battery_t>(
                    new battery_t(state.dataHVACGlobal->TimeStepSys,
                                  battery_params::CHEM::LITHIUM_ION,
                                  new capacity_lithium_ion_t(maxAhCapacity_, // Capacity of the whole battery
                                                             startingSOC_ * 100.0,
                                                             100.0, // Reset later
                                                             0.0,   // Reset later
                                                             state.dataHVACGlobal->TimeStepSys),
                                  new voltage_dynamic_t(seriesNum_,
                                                        parallelNum_,
                                                        liIon_Vnom_default_,
                                                        liIon_Vfull_,
                                                        liIon_Vexp_,
                                                        liIon_Vnom_,
                                                        liIon_Qfull_, // Capacity of one cell
                                                        liIon_Qexp_,
                                                        liIon_Qnom_,
                                                        liIon_C_rate_,
                                                        internalR_,
                                                        state.dataHVACGlobal->TimeStepSys),
                                  battLifetime,
                                  new thermal_t(state.dataHVACGlobal->TimeStepSys,
                                                liIon_mass_,
                                                liIon_surfaceArea_,
                                                internalR_ * seriesNum_ / parallelNum_, // Electric resistance of the whole battery
                                                liIon_Cp_,
                                                liIon_heatTransferCoef_,
                                                20.0 // Picking a temperature for now, will reset before each run.
                                                ),
                                  nullptr));
                ssc_lastBatteryState_ = std::make_unique<battery_state>(ssc_battery_->get_state());
                ssc_initBatteryState_ = std::make_unique<battery_state>(ssc_battery_->get_state());
            }

            break;
        }
        case StorageModelType::storageTypeNotSet: {
            // do nothing
            break;
        }

        } // switch storage model type

        if (storageModelMode_ == StorageModelType::kiBaMBattery || storageModelMode_ == StorageModelType::liIonNmcBattery) {
            SetupOutputVariable(state,
                                "Electric Storage Operating Mode Index",
                                OutputProcessor::Unit::None,
                                storageMode_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_);
            SetupOutputVariable(state,
                                "Electric Storage Battery Charge State",
                                OutputProcessor::Unit::Ah,
                                absoluteSOC_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_); // issue #4921
            SetupOutputVariable(state,
                                "Electric Storage Charge Fraction",
                                OutputProcessor::Unit::None,
                                fractionSOC_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_);
            SetupOutputVariable(state,
                                "Electric Storage Total Current",
                                OutputProcessor::Unit::A,
                                batteryCurrent_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_);
            SetupOutputVariable(state,
                                "Electric Storage Total Voltage",
                                OutputProcessor::Unit::V,
                                batteryVoltage_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_);

            if (lifeCalculation_ == BatteryDegradationModelType::lifeCalculationYes) {
                SetupOutputVariable(state,
                                    "Electric Storage Degradation Fraction",
                                    OutputProcessor::Unit::None,
                                    batteryDamage_,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    name_);
            }
        }

        SetupOutputVariable(state,
                            "Electric Storage Charge Power",
                            OutputProcessor::Unit::W,
                            storedPower_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Electric Storage Charge Energy",
                            OutputProcessor::Unit::J,
                            storedEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Electric Storage Production Decrement Energy",
                            OutputProcessor::Unit::J,
                            decrementedEnergyStored_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricityProduced",
                            "ELECTRICSTORAGE",
                            _,
                            "Plant");
        SetupOutputVariable(state,
                            "Electric Storage Discharge Power",
                            OutputProcessor::Unit::W,
                            drawnPower_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Electric Storage Discharge Energy",
                            OutputProcessor::Unit::J,
                            drawnEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_,
                            _,
                            "ElectricityProduced",
                            "ELECTRICSTORAGE",
                            _,
                            "Plant");
        SetupOutputVariable(state,
                            "Electric Storage Thermal Loss Rate",
                            OutputProcessor::Unit::W,
                            thermLossRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Electric Storage Thermal Loss Energy",
                            OutputProcessor::Unit::J,
                            thermLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            if (storageModelMode_ == StorageModelType::simpleBucketStorage) {
                SetupEMSInternalVariable(state, "Electrical Storage Simple Maximum Capacity", name_, "[J]", maxEnergyCapacity_);
            } else if (storageModelMode_ == StorageModelType::kiBaMBattery) {
                SetupEMSInternalVariable(state, "Electrical Storage Battery Maximum Capacity", name_, "[Ah]", maxAhCapacity_);
            }
        }
        if (storageModelMode_ == StorageModelType::liIonNmcBattery) {
            SetupOutputVariable(state,
                                "Electric Storage Battery Temperature",
                                OutputProcessor::Unit::C,
                                batteryTemperature_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name_);
        }

        if (zoneNum_ > 0) {
            switch (storageModelMode_) {
            case StorageModelType::simpleBucketStorage: {
                SetupZoneInternalGain(state,
                                      zoneNum_,
                                      "ElectricLoadCenter:Storage:Simple",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageSimple,
                                      &qdotConvZone_,
                                      nullptr,
                                      &qdotRadZone_);
                break;
            }
            case StorageModelType::kiBaMBattery: {
                SetupZoneInternalGain(state,
                                      zoneNum_,
                                      "ElectricLoadCenter:Storage:Battery",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageBattery,
                                      &qdotConvZone_,
                                      nullptr,
                                      &qdotRadZone_);
                break;
            }
            case StorageModelType::liIonNmcBattery: {
                SetupZoneInternalGain(state,
                                      zoneNum_,
                                      "ElectricLoadCenter:Storage:LiIonNMCBattery",
                                      name_,
                                      DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageLiIonNmcBattery,
                                      &qdotConvZone_,
                                      nullptr,
                                      &qdotRadZone_);
                break;
            }
            case StorageModelType::storageTypeNotSet: {
                // do nothing
                break;
            }

            } // switch storage model type
        }
    } else { // storage not found
        ShowSevereError(state, std::string{routineName} + " did not find storage name = " + objectName);
        errorsFound = true;
    }
    if (errorsFound) {
        ShowFatalError(state, std::string{routineName} + "Preceding errors terminate program.");
    }
}

Real64 checkUserEfficiencyInput(EnergyPlusData &state, Real64 userInputValue, std::string whichType, std::string deviceName, bool &errorsFound)
{
    Real64 const minChargeEfficiency = 0.001;
    Real64 const minDischargeEfficiency = 0.001;

    // Fix for Defect #8867.  Do not allow either efficiency to be zero as it will lead to a divide by zero (NaN).
    if (UtilityRoutines::SameString(whichType, "CHARGING")) {
        if (userInputValue < minChargeEfficiency) {
            ShowSevereError(state,
                            format("ElectricStorage charge efficiency was too low.  This occurred for electric storage unit named " + deviceName));
            ShowContinueError(state, format("Please check your input value  for this electric storage unit and fix the charge efficiency."));
            errorsFound = true;
            return minChargeEfficiency;
        } else {
            return userInputValue;
        }
    } else if (UtilityRoutines::SameString(whichType, "DISCHARGING")) {
        if (userInputValue < minDischargeEfficiency) {
            ShowSevereError(state,
                            format("ElectricStorage discharge efficiency was too low.  This occurred for electric storage unit named " + deviceName));
            ShowContinueError(state, format("Please check your input value  for this electric storage unit and fix the discharge efficiency."));
            errorsFound = true;
            return minDischargeEfficiency;
        } else {
            return userInputValue;
        }
    } else { // This shouldn't happen but this will still allow a value to be returned.
        return userInputValue;
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
        if (lifeCalculation_ == BatteryDegradationModelType::lifeCalculationYes) {
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
    } else if (storageModelMode_ == StorageModelType::liIonNmcBattery) {
        // Copy the initial battery state to the last battery state
        *ssc_lastBatteryState_ = *ssc_initBatteryState_;
        ssc_battery_->set_state(*ssc_lastBatteryState_);
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
        if (lifeCalculation_ == BatteryDegradationModelType::lifeCalculationYes) {
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
    } else if (storageModelMode_ == StorageModelType::liIonNmcBattery) {
        // Copy the initial battery state to the last battery state
        *ssc_lastBatteryState_ = *ssc_initBatteryState_;
        ssc_battery_->set_state(*ssc_lastBatteryState_);
    }
    myWarmUpFlag_ = false;
}

void ElectricStorage::timeCheckAndUpdate(EnergyPlusData &state)
{

    if (myWarmUpFlag_ && !state.dataGlobal->WarmupFlag) {
        reinitAtEndWarmup();
    }

    Real64 timeElapsedLoc =
        state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed;
    if (timeElapsed_ != timeElapsedLoc) { // time changed, update last with "current" result from previous time
        if (storageModelMode_ == StorageModelType::kiBaMBattery && lifeCalculation_ == BatteryDegradationModelType::lifeCalculationYes) {
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
                    batteryDamage_ += oneNmb0_[binNum] / CurveManager::CurveValue(state, lifeCurveNum_, (double(binNum) / double(cycleBinNum_)));
                }
            }
        } else if (storageModelMode_ == StorageModelType::liIonNmcBattery) {
            *ssc_lastBatteryState_ = ssc_battery_->get_state();
        }

        lastTimeStepStateOfCharge_ = thisTimeStepStateOfCharge_;
        lastTwoTimeStepAvailable_ = lastTimeStepAvailable_;
        lastTwoTimeStepBound_ = lastTimeStepBound_;
        lastTimeStepAvailable_ = thisTimeStepAvailable_;
        lastTimeStepBound_ = thisTimeStepBound_;
        timeElapsed_ = timeElapsedLoc;

    } // end if time changed
}

void ElectricStorage::simulate(EnergyPlusData &state,
                               Real64 &powerCharge,
                               Real64 &powerDischarge,
                               bool &charging,
                               bool &discharging,
                               Real64 const controlSOCMaxFracLimit,
                               Real64 const controlSOCMinFracLimit)
{
    // pass thru to constrain function depending on storage model type
    if (ScheduleManager::GetCurrentScheduleValue(state, availSchedPtr_) == 0.0) { // storage not available
        discharging = false;
        powerDischarge = 0.0;
        charging = false;
        powerCharge = 0.0;
    }

    if (storageModelMode_ == StorageModelType::simpleBucketStorage) {
        simulateSimpleBucketModel(state, powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit, controlSOCMinFracLimit);
    } else if (storageModelMode_ == StorageModelType::kiBaMBattery) {
        simulateKineticBatteryModel(state, powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit, controlSOCMinFracLimit);
    } else if (storageModelMode_ == StorageModelType::liIonNmcBattery) {
        simulateLiIonNmcBatteryModel(state, powerCharge, powerDischarge, charging, discharging, controlSOCMaxFracLimit, controlSOCMinFracLimit);
    }
}

std::string const &ElectricStorage::name() const
{
    return name_;
}

void ElectricStorage::simulateSimpleBucketModel(EnergyPlusData &state,
                                                Real64 &powerCharge,
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
        if ((lastTimeStepStateOfCharge_ + powerCharge * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour * energeticEfficCharge_) >=
            (maxEnergyCapacity_ * controlSOCMaxFracLimit)) {
            powerCharge = ((maxEnergyCapacity_ * controlSOCMaxFracLimit) - lastTimeStepStateOfCharge_) /
                          (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour * energeticEfficCharge_);
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
        if ((lastTimeStepStateOfCharge_ - powerDischarge * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour /
                                              energeticEfficDischarge_) <= (maxEnergyCapacity_ * controlSOCMinFracLimit)) {
            powerDischarge = (lastTimeStepStateOfCharge_ - (maxEnergyCapacity_ * controlSOCMinFracLimit)) * energeticEfficDischarge_ /
                             (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
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
            lastTimeStepStateOfCharge_ + powerCharge * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour * energeticEfficCharge_;
    }
    if (discharging) {
        pelIntoStorage_ = 0.0;
        pelFromStorage_ = powerDischarge;
        thisTimeStepStateOfCharge_ = lastTimeStepStateOfCharge_ -
                                     powerDischarge * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour / energeticEfficDischarge_;
        thisTimeStepStateOfCharge_ = max(thisTimeStepStateOfCharge_, 0.0);
    }

    // updates and reports
    electEnergyinStorage_ = thisTimeStepStateOfCharge_; //[J]
    storedPower_ = pelIntoStorage_;
    storedEnergy_ = pelIntoStorage_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    decrementedEnergyStored_ = -1.0 * storedEnergy_;
    drawnPower_ = pelFromStorage_;
    drawnEnergy_ = pelFromStorage_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    thermLossRate_ = max(storedPower_ * (1.0 - energeticEfficCharge_), drawnPower_ * (1.0 - energeticEfficDischarge_));
    thermLossEnergy_ = thermLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = (1.0 - zoneRadFract_) * thermLossRate_;
        qdotRadZone_ = (zoneRadFract_)*thermLossRate_;
    }
}

void ElectricStorage::simulateKineticBatteryModel(EnergyPlusData &state,
                                                  Real64 &powerCharge,
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
        Ef = E0d + CurveManager::CurveValue(state, chargeCurveNum_, Xf); // E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
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
            Ef = E0d + CurveManager::CurveValue(state, chargeCurveNum_, Xf); // E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
            Volt = Ef - I0 * internalR_;
            Inew = Pw / Volt;
            Tnew = std::abs(qmaxf / Inew); // ***Always positive here
            error = std::abs(Inew - I0);
        }

        Real64 dividend = -k * c * qmax + k * lastTimeStepAvailable_ * std::exp(-k * state.dataHVACGlobal->TimeStepSys) +
                          q0 * k * c * (1.0 - std::exp(-k * state.dataHVACGlobal->TimeStepSys));
        Real64 divisor = 1.0 - std::exp(-k * state.dataHVACGlobal->TimeStepSys) +
                         c * (k * state.dataHVACGlobal->TimeStepSys - 1 + std::exp(-k * state.dataHVACGlobal->TimeStepSys));
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

        bool const ok = determineCurrentForBatteryDischarge(state, I0, T0, Volt, Pw, q0, dischargeCurveNum_, k, c, qmax, E0c, internalR_);
        if (!ok) {
            ShowFatalError(state,
                           "ElectricLoadCenter:Storage:Battery named=\"" + name_ +
                               "\". Battery discharge current could not be estimated due to iteration limit reached. ");
            // issue #5301, need more diagnostics for this.
        }

        Real64 dividend = k * lastTimeStepAvailable_ * std::exp(-k * state.dataHVACGlobal->TimeStepSys) +
                          q0 * k * c * (1.0 - std::exp(-k * state.dataHVACGlobal->TimeStepSys));
        Real64 divisor = 1.0 - std::exp(-k * state.dataHVACGlobal->TimeStepSys) +
                         c * (k * state.dataHVACGlobal->TimeStepSys - 1.0 + std::exp(-k * state.dataHVACGlobal->TimeStepSys));
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
            Ef = E0c + CurveManager::CurveValue(state, dischargeCurveNum_, Xf);
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
        Real64 newAvailable = lastTimeStepAvailable_ * std::exp(-k * state.dataHVACGlobal->TimeStepSys) +
                              (q0 * k * c - I0) * (1.0 - std::exp(-k * state.dataHVACGlobal->TimeStepSys)) / k -
                              I0 * c * (k * state.dataHVACGlobal->TimeStepSys - 1.0 + std::exp(-k * state.dataHVACGlobal->TimeStepSys)) / k;
        Real64 newBound = lastTimeStepBound_ * std::exp(-k * state.dataHVACGlobal->TimeStepSys) +
                          q0 * (1.0 - c) * (1.0 - std::exp(-k * state.dataHVACGlobal->TimeStepSys)) -
                          I0 * (1.0 - c) * (k * state.dataHVACGlobal->TimeStepSys - 1.0 + std::exp(-k * state.dataHVACGlobal->TimeStepSys)) / k;
        thisTimeStepAvailable_ = max(0.0, newAvailable);
        thisTimeStepBound_ = max(0.0, newBound);
    }

    Pactual = I0 * Volt;
    Real64 TotalSOC = thisTimeStepAvailable_ + thisTimeStepBound_;

    // output1
    if (TotalSOC > q0) {
        storageMode_ = 2;
        storedPower_ = -1.0 * Volt * I0 * numBattery_; // Issue #5303, fix sign issue
        storedEnergy_ = -1.0 * Volt * I0 * numBattery_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        decrementedEnergyStored_ = -1.0 * storedEnergy_;
        drawnPower_ = 0.0;
        drawnEnergy_ = 0.0;

    } else if (TotalSOC < q0) {
        storageMode_ = 1;
        storedPower_ = 0.0;
        storedEnergy_ = 0.0;
        decrementedEnergyStored_ = 0.0;
        drawnPower_ = Volt * I0 * numBattery_;
        drawnEnergy_ = Volt * I0 * numBattery_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

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
    thermLossEnergy_ = internalR_ * pow_2(I0) * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour * numBattery_;

    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = ((1.0 - zoneRadFract_) * thermLossRate_) * numBattery_;
        qdotRadZone_ = ((zoneRadFract_)*thermLossRate_) * numBattery_;
    }

    powerCharge = storedPower_;
    powerDischarge = drawnPower_;
}

void ElectricStorage::simulateLiIonNmcBatteryModel(EnergyPlusData &state,
                                                   Real64 &powerCharge,
                                                   Real64 &powerDischarge,
                                                   bool &charging,
                                                   bool &discharging,
                                                   Real64 const controlSOCMaxFracLimit,
                                                   Real64 const controlSOCMinFracLimit)
{

    // Copy the battery state from the end of last timestep
    battery_state battState = *ssc_lastBatteryState_;

    // Set the temperature the battery sees
    if (zoneNum_ > 0) {
        // If in a zone, use the zone temperature
        battState.thermal->T_room = state.dataHeatBalFanSys->ZT(zoneNum_);
    } else {
        // If outside, use outdoor temperature
        battState.thermal->T_room = state.dataEnvrn->OutDryBulbTemp;
    }
    ssc_battery_->set_state(battState);

    // Set the SOC limits
    ssc_battery_->changeSOCLimits(controlSOCMinFracLimit * 100.0, controlSOCMaxFracLimit * 100.0);

    // Set the current timestep length
    if (std::lround(ssc_battery_->get_params().dt_hr * 60.0) != std::lround(state.dataHVACGlobal->TimeStepSys * 60.0)) {
        ssc_battery_->ChangeTimestep(state.dataHVACGlobal->TimeStepSys);
    }

    // Run the battery
    // SAM uses negative values for charging, positive for discharging
    // E+ power/energy outputs are positive
    double power{0.0}; // Using double instead of Real64 because SSC is expecting a double
    if (charging) {
        power = -powerCharge;
    } else if (discharging) {
        power = powerDischarge;
    }
    power *= 0.001; // Convert to kW
    ssc_battery_->runPower(power);

    // Store outputs
    const battery_state &battState2{ssc_battery_->get_state()};
    if (battState2.P < 0.0) { // negative for charging
        storageMode_ = 2;
        powerCharge = fabs(battState2.P) * 1000.0; // kW -> W
        powerDischarge = 0.0;
        charging = true;
        discharging = false;
    } else if (battState2.P > 0.0) { // positive for discharging
        storageMode_ = 1;
        powerCharge = 0.0;
        powerDischarge = fabs(battState2.P) * 1000.0; // kW -> W
        charging = false;
        discharging = true;
    } else {
        storageMode_ = 0;
        powerCharge = 0.0;
        powerDischarge = 0.0;
        charging = false;
        discharging = false;
    }
    absoluteSOC_ = ssc_battery_->charge_total();
    fractionSOC_ = ssc_battery_->SOC() * 0.01; // % -> fraction
    batteryCurrent_ = ssc_battery_->I();
    batteryVoltage_ = ssc_battery_->V();
    batteryDamage_ = 1.0 - (ssc_battery_->charge_maximum_lifetime() / maxAhCapacity_);
    storedPower_ = powerCharge;
    storedEnergy_ = storedPower_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    drawnPower_ = powerDischarge;
    drawnEnergy_ = drawnPower_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    decrementedEnergyStored_ = -storedEnergy_;
    thermLossRate_ = battState2.thermal->heat_dissipated * 1000.0; // kW -> W
    thermLossEnergy_ = thermLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    batteryTemperature_ = battState2.thermal->T_batt;

    // Zone Heat Gains
    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = (1.0 - zoneRadFract_) * thermLossRate_;
        qdotRadZone_ = (zoneRadFract_)*thermLossRate_;
    }
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

Real64 ElectricStorage::stateOfChargeFraction() const
{
    return fractionSOC_;
}

Real64 ElectricStorage::batteryTemperature() const
{
    assert(storageModelMode_ == StorageModelType::liIonNmcBattery);
    return batteryTemperature_;
}

bool ElectricStorage::determineCurrentForBatteryDischarge(EnergyPlusData &state,
                                                          Real64 &curI0,
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
    Real64 Ef = E0c + CurveManager::CurveValue(state, CurveNum, Xf); // E0d+Ac*Xf+Cc*X/(Dc-Xf)
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

        Ef = E0c + CurveManager::CurveValue(state, CurveNum, Xf); // E0c+Ad*Xf+Cd*X/(Dd-Xf)
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
                state, "ElectricStorage::determineCurrentForBatteryDischarge, iteration limit exceeded, failed to solve for discharge current.");
            ShowContinueError(state, format("Last timestep charge available, q0 = {:.5R}", q0));
            ShowContinueError(state, format("New Current, Inew = {:.5R} [Amps]", Inew));
            ShowContinueError(state, format("Power discharge per module cell, Pw = {:.5R} ", Pw));
            ShowContinueError(
                state, format("Charge Conversion Rate, [1/h] change rate from bound charge energy to available charge, parameter k = {:.5R}", k));
            ShowContinueError(state, format("parameter c = {:.5R}", c));
            ShowContinueError(state, format("parameter qmax = {:.5R}", qmax));
            ShowContinueError(state, format("Fully charged open circuit voltage, parameter E0c  = {:.5R}", E0c));
            ShowContinueError(state, format("parameter InternalR = {:.5R}", InternalR));
            if (qmaxf == 0.0) {
                ShowContinueError(state, "qmaxf was zero, would have divided by zero.");
            }
            if (Inew == 0.0) {
                ShowContinueError(state, "Inew was zero, would have divided by zero. ");
            }
            if (curVolt == 0.0) {
                ShowContinueError(state, "curVolt was zero, would have divided by zero. ");
            }

            ShowContinueErrorTimeStamp(state, "ElectricStorage::determineCurrentForBatteryDischarge ");
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
ElectricTransformer::ElectricTransformer(EnergyPlusData &state, std::string const &objectName)
    : myOneTimeFlag_(true), availSchedPtr_(0), usageMode_(TransformerUse::usenotYetSet),
      heatLossesDestination_(ThermalLossDestination::heatLossNotDetermined), zoneNum_(0), zoneRadFrac_(0.0), ratedCapacity_(0.0), phase_(0),
      factorTempCoeff_(0.0), tempRise_(0.0), eddyFrac_(0.0), performanceInputMode_(TransformerPerformanceInput::perfInputMethodNotSet),
      ratedEfficiency_(0.0), ratedPUL_(0.0), ratedTemp_(0.0), maxPUL_(0.0), considerLosses_(true), ratedNL_(0.0), ratedLL_(0.0),
      overloadErrorIndex_(0), efficiency_(0.0), powerIn_(0.0), energyIn_(0.0), powerOut_(0.0), energyOut_(0.0), noLoadLossRate_(0.0),
      noLoadLossEnergy_(0.0), loadLossRate_(0.0), loadLossEnergy_(0.0), thermalLossRate_(0.0), thermalLossEnergy_(0.0),
      elecUseMeteredUtilityLosses_(0.0), powerConversionMeteredLosses_(0.0), qdotConvZone_(0.0), qdotRadZone_(0.0)
{
    static constexpr std::string_view routineName = "ElectricTransformer constructor ";
    int numAlphas; // Number of elements in the alpha array
    int numNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool errorsFound = false;
    int transformerIDFObjectNum = 0;
    state.dataIPShortCut->cCurrentModuleObject = "ElectricLoadCenter:Transformer";

    transformerIDFObjectNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ElectricLoadCenter:Transformer", objectName);
    if (transformerIDFObjectNum > 0) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 transformerIDFObjectNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 numAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 numNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        name_ = state.dataIPShortCut->cAlphaArgs(1);
        // how to verify names are unique across objects? add to GlobalNames?
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            availSchedPtr_ = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            availSchedPtr_ = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (availSchedPtr_ == 0) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                errorsFound = true;
            }
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
            usageMode_ = TransformerUse::powerInFromGrid; // default
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "PowerInFromGrid")) {
            usageMode_ = TransformerUse::powerInFromGrid;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "PowerOutToGrid")) {
            usageMode_ = TransformerUse::powerOutFromBldgToGrid;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "LoadCenterPowerConditioning")) {
            usageMode_ = TransformerUse::powerBetweenLoadCenterAndBldg;

        } else {
            ShowWarningError(state,
                             std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                 "\", invalid entry.");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
            errorsFound = true;
        }

        zoneNum_ = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(4), state.dataHeatBal->Zone);
        if (zoneNum_ > 0) heatLossesDestination_ = ThermalLossDestination::zoneGains;
        if (zoneNum_ == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
            } else {
                heatLossesDestination_ = ThermalLossDestination::lostToOutside;
                ShowWarningError(state,
                                 std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                ShowContinueError(state, "Zone name not found. Transformer heat losses will not be added to a zone");
                // continue with simulation but storage losses not sent to a zone.
            }
        }
        zoneRadFrac_ = state.dataIPShortCut->rNumericArgs(1);
        ratedCapacity_ = state.dataIPShortCut->rNumericArgs(2);
        phase_ = state.dataIPShortCut->rNumericArgs(3);

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Copper")) {
            factorTempCoeff_ = 234.5;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Aluminum")) {
            factorTempCoeff_ = 225.0;
        } else {
            ShowSevereError(state,
                            std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", invalid entry.");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
            errorsFound = true;
        }
        tempRise_ = state.dataIPShortCut->rNumericArgs(4);
        eddyFrac_ = state.dataIPShortCut->rNumericArgs(5);

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "RatedLosses")) {
            performanceInputMode_ = TransformerPerformanceInput::lossesMethod;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "NominalEfficiency")) {
            performanceInputMode_ = TransformerPerformanceInput::efficiencyMethod;
        } else {
            ShowSevereError(state,
                            std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", invalid entry.");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
            errorsFound = true;
        }
        if (ratedCapacity_ == 0) {
            if (performanceInputMode_ == TransformerPerformanceInput::lossesMethod) {
                ShowWarningError(state,
                                 std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\".");
                ShowContinueError(state, "Specified " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, format("Specified {} = {:.1R}", state.dataIPShortCut->cNumericFieldNames(2), ratedCapacity_));
                ShowContinueError(state, "Transformer load and no load losses cannot be calculated with 0.0 rated capacity.");
                ShowContinueError(state, "Simulation continues but transformer losses will be set to zero.");
            }
        }
        ratedNL_ = state.dataIPShortCut->rNumericArgs(6);
        ratedLL_ = state.dataIPShortCut->rNumericArgs(7);
        ratedEfficiency_ = state.dataIPShortCut->rNumericArgs(8);
        ratedPUL_ = state.dataIPShortCut->rNumericArgs(9);
        ratedTemp_ = state.dataIPShortCut->rNumericArgs(10);
        maxPUL_ = state.dataIPShortCut->rNumericArgs(11);
        // Check the input for MaxPUL if the performance input method is EfficiencyMethod
        if (performanceInputMode_ == TransformerPerformanceInput::efficiencyMethod) {
            if (state.dataIPShortCut->lNumericFieldBlanks(11)) {
                maxPUL_ = ratedPUL_;
            } else if (maxPUL_ <= 0 || maxPUL_ > 1) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(
                    state, format("Invalid {}=[{:.3R}].", state.dataIPShortCut->cNumericFieldNames(11), state.dataIPShortCut->rNumericArgs(11)));
                ShowContinueError(state, "Entered value must be > 0 and <= 1.");
                errorsFound = true;
            }
        }
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Yes")) {
            considerLosses_ = true;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "No")) {
            considerLosses_ = false;
        } else {
            if (usageMode_ == TransformerUse::powerInFromGrid) {
                ShowSevereError(state,
                                std::string{routineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid entry.");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + " = " + state.dataIPShortCut->cAlphaArgs(7));
                errorsFound = true;
            }
        }

        int numAlphaBeforeMeter = 7;
        int numWiredMeters = numAlphas - numAlphaBeforeMeter;

        if (usageMode_ == TransformerUse::powerInFromGrid) {

            // Provide warning if no meter is wired to a transformer used to get power from the grid
            if (numWiredMeters <= 0) {
                ShowWarningError(state, std::string{routineName} + "ElectricLoadCenter:Transformer=\"" + name_ + "\":");
                ShowContinueError(state, "ISOLATED Transformer: No meter wired to a transformer used to input power from grid");
            }

            wiredMeterNames_.resize(numWiredMeters, "");
            wiredMeterPtrs_.resize(numWiredMeters, 0);
            specialMeter_.resize(numWiredMeters, false);

            // Meter check deferred because they may have not been "loaded" yet,
            for (auto loopCount = 0; loopCount < numWiredMeters; ++loopCount) {
                wiredMeterNames_[loopCount] = UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(loopCount + numAlphaBeforeMeter + 1));
                // Assign SpecialMeter as TRUE if the meter name is Electricity:Facility or Electricity:HVAC
                if (UtilityRoutines::SameString(wiredMeterNames_[loopCount], "Electricity:Facility") ||
                    UtilityRoutines::SameString(wiredMeterNames_[loopCount], "Electricity:HVAC")) {
                    specialMeter_[loopCount] = true;
                } else {
                    specialMeter_[loopCount] = false;
                }
            }
        }
        SetupOutputVariable(state,
                            "Transformer Efficiency",
                            OutputProcessor::Unit::None,
                            efficiency_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Input Electricity Rate",
                            OutputProcessor::Unit::W,
                            powerIn_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Input Electricity Energy",
                            OutputProcessor::Unit::J,
                            energyIn_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Output Electricity Rate",
                            OutputProcessor::Unit::W,
                            powerOut_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Output Electricity Energy",
                            OutputProcessor::Unit::J,
                            energyOut_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Transformer No Load Loss Rate",
                            OutputProcessor::Unit::W,
                            noLoadLossRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Transformer No Load Loss Energy",
                            OutputProcessor::Unit::J,
                            noLoadLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Load Loss Rate",
                            OutputProcessor::Unit::W,
                            loadLossRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Load Loss Energy",
                            OutputProcessor::Unit::J,
                            loadLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Thermal Loss Rate",
                            OutputProcessor::Unit::W,
                            thermalLossRate_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name_);
        SetupOutputVariable(state,
                            "Transformer Thermal Loss Energy",
                            OutputProcessor::Unit::J,
                            thermalLossEnergy_,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name_);
        if (usageMode_ == TransformerUse::powerInFromGrid) { // power losses metered as an end use exterior equipment
            SetupOutputVariable(state,
                                "Transformer Distribution Electricity Loss Energy",
                                OutputProcessor::Unit::J,
                                elecUseMeteredUtilityLosses_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                name_,
                                _,
                                "Electricity",
                                "ExteriorEquipment",
                                "Transformer",
                                "System");
        }
        if (usageMode_ == TransformerUse::powerOutFromBldgToGrid) {
            SetupOutputVariable(state,
                                "Transformer Cogeneration Electricity Loss Energy",
                                OutputProcessor::Unit::J,
                                powerConversionMeteredLosses_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                name_,
                                _,
                                "ElectricityProduced",
                                "POWERCONVERSION",
                                _,
                                "System");
        }
        if (usageMode_ == TransformerUse::powerBetweenLoadCenterAndBldg) {
            SetupOutputVariable(state,
                                "Transformer Conversion Electricity Loss Energy",
                                OutputProcessor::Unit::J,
                                powerConversionMeteredLosses_,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                name_,
                                _,
                                "ElectricityProduced",
                                "POWERCONVERSION",
                                _,
                                "System");
        }

        if (zoneNum_ > 0) {
            SetupZoneInternalGain(state,
                                  zoneNum_,
                                  "ElectricLoadCenter:Transformer",
                                  name_,
                                  DataHeatBalance::IntGainTypeOf_ElectricLoadCenterTransformer,
                                  &qdotConvZone_,
                                  nullptr,
                                  &qdotRadZone_);
        }

    } else {
        ShowSevereError(state, std::string{routineName} + " did not find transformer name = " + objectName);
        errorsFound = true;
    }

    if (errorsFound) {
        ShowFatalError(state, std::string{routineName} + "Preceding errors terminate program.");
    }
}

Real64 ElectricTransformer::getLossRateForOutputPower(EnergyPlusData &state, Real64 const powerOutOfTransformer)
{
    manageTransformers(state, powerOutOfTransformer);
    return totalLossRate_;
}

Real64 ElectricTransformer::getLossRateForInputPower(EnergyPlusData &state, Real64 const powerIntoTransformer)
{
    manageTransformers(state, powerIntoTransformer);
    return totalLossRate_;
}

void ElectricTransformer::manageTransformers(EnergyPlusData &state, Real64 const surplusPowerOutFromLoadCenters)
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

            if (state.dataGlobal->MetersHaveBeenInitialized) {

                elecLoad += GetInstantMeterValue(state, wiredMeterPtrs_[meterNum], OutputProcessor::TimeStepType::TimeStepZone) /
                                state.dataGlobal->TimeStepZoneSec +
                            GetInstantMeterValue(state, wiredMeterPtrs_[meterNum], OutputProcessor::TimeStepType::TimeStepSystem) /
                                (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
                // PastElecLoad store the metered value in the previous time step. This value will be used to check whether
                // a transformer is overloaded or not.
                pastElecLoad += GetCurrentMeterValue(state, wiredMeterPtrs_[meterNum]) / state.dataGlobal->TimeStepZoneSec;
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
    if (ratedCapacity_ > 0.0 && ScheduleManager::GetCurrentScheduleValue(state, availSchedPtr_) > 0.0) {

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
                ShowSevereError(state, "Transformer Overloaded");
                ShowContinueError(state, "Entered in ElectricLoadCenter:Transformer =" + name_);
            }
            ShowRecurringSevereErrorAtEnd(state, "Transformer Overloaded: Entered in ElectricLoadCenter:Transformer =" + name_, overloadErrorIndex_);
        }

        Real64 tempChange = std::pow(pUL, 1.6) * tempRise_;
        Real64 ambTemp = 20.0;
        if (heatLossesDestination_ == ThermalLossDestination::zoneGains) {

            ambTemp = state.dataHeatBal->ZnAirRpt(zoneNum_).MeanAirTemp;
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
            elecUseMeteredUtilityLosses_ = totalLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        } else {
            elecUseMeteredUtilityLosses_ = 0.0;
        }

        // Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
        // is assigned 0
        totalLossEnergy_ = totalLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        break;
    }

    case TransformerUse::powerOutFromBldgToGrid:
    case TransformerUse::powerBetweenLoadCenterAndBldg: {
        powerOut_ = elecLoad - totalLossRate_;

        if (powerOut_ < 0) powerOut_ = 0.0;

        powerConversionMeteredLosses_ = -1.0 * totalLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

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
        efficiency_ = 1.0; // Set to something reasonable to avoid a divide by zero error
    } else {
        efficiency_ = powerOut_ / powerIn_;
    }
    noLoadLossEnergy_ = noLoadLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    loadLossEnergy_ = loadLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    energyIn_ = powerIn_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    energyOut_ = powerOut_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    //   Thermal loss rate may not be equal to Total loss rate. This is the case when surplus power is less than the
    //    calculated total loss rate for a cogeneration transformer. That is why "PowerIn - PowerOut" is used below.
    thermalLossRate_ = powerIn_ - powerOut_;
    thermalLossEnergy_ = thermalLossRate_ * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    if (zoneNum_ > 0) { // set values for zone heat gains
        qdotConvZone_ = (1.0 - zoneRadFrac_) * thermalLossRate_;
        qdotRadZone_ = (zoneRadFrac_)*thermalLossRate_;
    }
}

void ElectricTransformer::setupMeterIndices(EnergyPlusData &state)
{
    if (usageMode_ == TransformerUse::powerInFromGrid) {
        for (std::size_t meterNum = 0; meterNum < wiredMeterNames_.size(); ++meterNum) {

            wiredMeterPtrs_[meterNum] = GetMeterIndex(state, wiredMeterNames_[meterNum]);

            // Check whether the meter is an electricity meter
            // Index function is used here because some resource types are not Electricity but strings containing
            // Electricity such as ElectricityPurchased and ElectricityProduced.
            // It is not proper to have this check in GetInput routine because the meter index may have not been defined
            if (!has(GetMeterResourceType(state, wiredMeterPtrs_[meterNum]), "Electricity")) {
                EnergyPlus::ShowFatalError(state, "Non-electricity meter used for " + name_);
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
