// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

bool oneTimeCompRptHeaderFlag = true;

void BaseSizer::clear_state()
{
    oneTimeCompRptHeaderFlag = true;
}

void BaseSizer::initializeWithinEP(EnergyPlusData &state,
                                   const std::string &_compType,
                                   const std::string &_compName,
                                   bool const &_printWarningFlag,
                                   std::string const &_callingRoutine)
{
    this->initialized = true;
    this->compType = _compType;
    this->compName = _compName;
    this->printWarningFlag = _printWarningFlag;
    this->callingRoutine = _callingRoutine;
    this->stdRhoAir = DataEnvironment::StdRhoAir;
    this->sysSizingRunDone = DataSizing::SysSizingRunDone;
    this->zoneSizingRunDone = DataSizing::ZoneSizingRunDone;
    this->curSysNum = DataSizing::CurSysNum;
    this->curOASysNum = DataSizing::CurOASysNum;
    this->curZoneEqNum = DataSizing::CurZoneEqNum;
    this->curDuctType = DataSizing::CurDuctType;
    this->numPrimaryAirSys = DataHVACGlobals::NumPrimaryAirSys;
    this->numSysSizInput = DataSizing::NumSysSizInput;
    this->doSystemSizing = DataGlobals::DoSystemSizing;
    this->numZoneSizingInput = DataSizing::NumZoneSizingInput;
    this->doZoneSizing = DataGlobals::DoZoneSizing;
    this->curTermUnitSizingNum = DataSizing::CurTermUnitSizingNum;
    this->termUnitSingDuct = DataSizing::TermUnitSingDuct;
    this->termUnitPIU = DataSizing::TermUnitPIU;
    this->termUnitIU = DataSizing::TermUnitIU;
    this->zoneEqFanCoil = DataSizing::ZoneEqFanCoil;
    this->otherEqType = !(this->termUnitSingDuct || this->termUnitPIU || this->termUnitIU || this->zoneEqFanCoil);
    this->zoneEqUnitHeater = DataSizing::ZoneEqUnitHeater;
    this->zoneEqUnitVent = DataSizing::ZoneEqUnitVent;
    this->zoneEqVentedSlab = DataSizing::ZoneEqVentedSlab;
    this->zoneSizingInput = DataSizing::ZoneSizingInput;
    this->unitarySysEqSizing = DataSizing::UnitarySysEqSizing;
    this->oaSysEqSizing = DataSizing::OASysEqSizing;
    this->outsideAirSys = DataAirLoop::OutsideAirSys;
    this->termUnitSizing = DataSizing::TermUnitSizing;
    this->finalZoneSizing = DataSizing::FinalZoneSizing;
    this->termUnitFinalZoneSizing = DataSizing::TermUnitFinalZoneSizing;
    this->zoneEqSizing = DataSizing::ZoneEqSizing;
    this->sysSizingInputData = DataSizing::SysSizInput;
    this->finalSysSizing = DataSizing::FinalSysSizing;
    this->plantSizData = DataSizing::PlantSizData;
    this->primaryAirSystem = DataAirSystems::PrimaryAirSystem;

    this->airloopDOAS = state.dataAirLoopHVACDOAS.airloopDOAS;
    if (EnergyPlus::BaseSizer::isValidCoilType(this->compType)) { // coil reports fail if coilType is not one of DataHVACGlobals::cAllCoilTypes
        this->getCoilReportObject = true;
    }

    // global sizing data
    dataEMSOverrideON = DataSizing::DataEMSOverrideON;
    dataEMSOverride = DataSizing::DataEMSOverride;
    this->minOA = DataSizing::MinOA;
    this->dataConstantUsedForSizing = DataSizing::DataConstantUsedForSizing;
    this->dataFractionUsedForSizing = DataSizing::DataFractionUsedForSizing;
    this->dataFanIndex = DataSizing::DataFanIndex;
    this->dataFanEnumType = DataSizing::DataFanEnumType;

    // global Data* sizing constants
    this->dataPltSizHeatNum = DataSizing::DataPltSizHeatNum;
    this->dataWaterLoopNum = DataSizing::DataWaterLoopNum;
    this->dataPltSizCoolNum = DataSizing::DataPltSizCoolNum;
    this->dataWaterCoilSizHeatDeltaT = DataSizing::DataWaterCoilSizHeatDeltaT;
    this->dataWaterCoilSizCoolDeltaT = DataSizing::DataWaterCoilSizCoolDeltaT;
    this->dataCapacityUsedForSizing = DataSizing::DataCapacityUsedForSizing;

    this->dataDesInletAirHumRat = DataSizing::DataDesInletAirHumRat;
    this->dataDesOutletAirHumRat = DataSizing::DataDesOutletAirHumRat;
    this->dataDesOutletAirTemp = DataSizing::DataDesOutletAirTemp;
    this->dataDesInletWaterTemp = DataSizing::DataDesInletWaterTemp;
    this->dataFlowUsedForSizing = DataSizing::DataFlowUsedForSizing;
    this->dataWaterFlowUsedForSizing = DataSizing::DataWaterFlowUsedForSizing;

    this->dataSizingFraction = DataSizing::DataSizingFraction;
    this->dataDXSpeedNum = DataSizing::DataDXSpeedNum;
}

void BaseSizer::initializeFromAPI(Real64 const elevation) {
    this->clearState();
    this->initialized = true;
    this->compType = "API_component_type";
    this->compName = "API_component_name";
    this->printWarningFlag = false;
    this->callingRoutine = "called_from_API";
    Real64 barometricPressure = DataEnvironment::StdPressureSeaLevel * std::pow(1.0 - 2.25577e-05 * elevation, 5.2559);
    this->stdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(barometricPressure, 20.0, 0.0);
    this->getCoilReportObject = false;
}

void BaseSizer::addErrorMessage(std::string const &s) {
    this->lastErrorMessages.append(s).append("\n");
}

std::string BaseSizer::getLastErrorMessages() {
    std::string s(this->lastErrorMessages);
    this->lastErrorMessages = "";
    return s;
}

void BaseSizer::preSize(Real64 const _originalValue)
{
    if (this->sizingType == AutoSizingType::Unknown) {
        std::string msg = "Sizing Library Base Class: preSize, SizingType not defined.";
        this->addErrorMessage(msg);
        ShowSevereError(msg);
        ShowFatalError("Sizing type causes fatal error.");
    }
    this->originalValue = _originalValue;
    this->autoCalculate = false;
    this->errorType = EnergyPlus::AutoSizingResultType::NoError;
    this->initialized = false; // force use of Init then Size in subsequent calls
    this->hardSizeNoDesignRun = !(this->sysSizingRunDone || this->zoneSizingRunDone);

    if (this->dataFractionUsedForSizing == 0.0 && this->dataConstantUsedForSizing > 0.0) {
        this->errorType = AutoSizingResultType::ErrorType1;
        this->autoSizedValue = 0.0;
        this->autoCalculate = true;
        this->hardSizeNoDesignRun = false;
        std::string msg = "Sizing Library HeatingWaterflowSizer: DataConstantUsedForSizing and DataFractionUsedForSizing used for autocalculating " +
                          this->sizingString + " must both be greater than 0.";
        this->addErrorMessage(msg);
        ShowSevereError(msg);
    } else if (this->dataFractionUsedForSizing > 0.0) {
        this->autoCalculate = true;
        this->hardSizeNoDesignRun = false;
    }

    if (this->curSysNum > 0 && this->curSysNum <= this->numPrimaryAirSys) {
        if (this->sysSizingRunDone) {
            for (auto &sizingInput : this->sysSizingInputData) {
                if (sizingInput.AirLoopNum == this->curSysNum) {
                    this->sizingDesRunThisAirSys = true;
                    break;
                }
            }
        }
        if (this->unitarySysEqSizing.allocated())
            this->airLoopSysFlag =
                this->unitarySysEqSizing(this->curSysNum).CoolingCapacity || this->unitarySysEqSizing(this->curSysNum).HeatingCapacity;
        if (this->curOASysNum > 0) {
            this->oaSysFlag = this->oaSysEqSizing(this->curOASysNum).CoolingCapacity || this->oaSysEqSizing(this->curOASysNum).HeatingCapacity;
        }
    }

    if (this->curZoneEqNum > 0) {
        if (this->zoneEqSizing.allocated()) {
            this->sizingDesValueFromParent = this->zoneEqSizing(this->curZoneEqNum).DesignSizeFromParent;
        }
        if (this->zoneSizingRunDone) {
            for (auto &sizingInput : this->zoneSizingInput) {
                if (sizingInput.ZoneNum == this->curZoneEqNum) {
                    this->sizingDesRunThisZone = true;
                    break;
                }
            }
        }
        this->hardSizeNoDesignRun = false;
    }

    if (this->originalValue == DataSizing::AutoSize) {
        this->wasAutoSized = true;
        this->hardSizeNoDesignRun = false;
        if (!this->sizingDesRunThisAirSys && this->curSysNum > 0 && !this->autoCalculate) {
            if (!this->sysSizingRunDone) {
                std::string msg = "For autosizing of " + this->compType + ' ' + this->compName + ", a system sizing run must be done.";
                this->addErrorMessage(msg);
                ShowSevereError(msg);
                if (this->numSysSizInput == 0) {
                    std::string msg2 = "No \"Sizing:System\" objects were entered.";
                    this->addErrorMessage(msg2);
                    ShowContinueError(msg2);
                }
                if (!this->doSystemSizing) {
                    std::string msg2 = R"(The "SimulationControl" object did not have the field "Do System Sizing Calculation" set to Yes.)";
                    this->addErrorMessage(msg2);
                    ShowContinueError(msg2);
                }
                ShowFatalError("Program terminates due to previously shown condition(s).");
            }
        }
        if (!this->sizingDesRunThisZone && this->curZoneEqNum > 0 && !this->sizingDesValueFromParent && !this->autoCalculate) {
            if (!this->zoneSizingRunDone) {
                std::string msg = "For autosizing of " + this->compType + ' ' + this->compName + ", a zone sizing run must be done.";
                this->addErrorMessage(msg);
                ShowSevereError(msg);
                if (this->numZoneSizingInput == 0) {
                    std::string msg2 = "No \"Sizing:Zone\" objects were entered.";
                    this->addErrorMessage(msg2);
                    ShowContinueError(msg2);
                }
                if (!this->doZoneSizing) {
                    std::string msg2 = R"(The "SimulationControl" object did not have the field "Do Zone Sizing Calculation" set to Yes.)";
                    this->addErrorMessage(msg2);
                    ShowContinueError(msg2);
                }
                ShowFatalError("Program terminates due to previously shown condition(s).");
            }
        }
    } else {
        this->wasAutoSized = false;
    }
}

void BaseSizer::reportSizerOutput(std::string const &CompType,
                                  std::string const &CompName,
                                  std::string const &VarDesc,
                                  Real64 const VarValue,
                                  Optional_string_const UsrDesc,
                                  Optional<Real64 const> UsrValue)
{

    static constexpr auto Format_990("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n");
    static constexpr auto Format_991(" Component Sizing Information, {}, {}, {}, {:.5R}\n");

    // to do, make this a parameter. Unfortunately this function is used in MANY
    // places so it involves touching most of E+
    auto &outputFiles = EnergyPlus::OutputFiles::getSingleton();
    if (oneTimeCompRptHeaderFlag) {
        if (ReportSizingManager::MyOneTimeFlag) print(outputFiles.eio, Format_990);
        ReportSizingManager::MyOneTimeFlag = false;
        oneTimeCompRptHeaderFlag = false;
    }

    print(outputFiles.eio, Format_991, CompType, CompName, VarDesc, VarValue);
    // add to tabular output reports
    OutputReportPredefined::AddCompSizeTableEntry(CompType, CompName, VarDesc, VarValue);

    if (present(UsrDesc) && present(UsrValue)) {
        print(outputFiles.eio, Format_991, CompType, CompName, UsrDesc(), UsrValue());
        OutputReportPredefined::AddCompSizeTableEntry(CompType, CompName, UsrDesc, UsrValue);
    } else if (present(UsrDesc) || present(UsrValue)) {
        ShowFatalError("ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both.");
    }

    // add to SQL output
    if (sqlite) sqlite->addSQLiteComponentSizingRecord(CompType, CompName, VarDesc, VarValue);
    if (present(UsrDesc) && present(UsrValue)) {
        if (sqlite) sqlite->addSQLiteComponentSizingRecord(CompType, CompName, UsrDesc, UsrValue);
    }
}

void BaseSizer::selectSizerOutput(bool &errorsFound)
{
    if (this->printWarningFlag) {
        if (!this->wasAutoSized && (this->autoSizedValue == this->originalValue)) { // no sizing run done
            this->reportSizerOutput(this->compType, this->compName, "User-Specified " + this->sizingString, this->originalValue);
            this->autoSizedValue = this->originalValue;
        } else if (!this->wasAutoSized && this->autoSizedValue >= 0.0 && this->originalValue == 0.0) { // input was blank or zero
            this->reportSizerOutput(this->compType, this->compName, "User-Specified " + this->sizingString, this->originalValue);
            this->autoSizedValue = this->originalValue;
        } else if (this->wasAutoSized && this->autoSizedValue >= 0.0 && this->originalValue <= 0.0) { // autosized to 0 or greater and input is 0 or autosize
            // might need more logic here to catch everything correctly
            this->reportSizerOutput(this->compType, this->compName, "Design Size " + this->sizingString, this->autoSizedValue);
        } else if (this->autoSizedValue >= 0.0 && this->originalValue > 0.0) {
            if ((std::abs(this->autoSizedValue - this->originalValue) / this->originalValue) > DataSizing::AutoVsHardSizingThreshold) {
                this->reportSizerOutput(this->compType,
                                        this->compName,
                                        "Design Size " + this->sizingString,
                                        this->autoSizedValue,
                                        "User-Specified " + this->sizingString,
                                        this->originalValue);
            } else {
                this->reportSizerOutput(this->compType, this->compName, "User-Specified " + this->sizingString, this->originalValue);
            }
            if (DataGlobals::DisplayExtraWarnings) {
                if ((std::abs(this->autoSizedValue - this->originalValue) / this->originalValue) > DataSizing::AutoVsHardSizingThreshold) {
                    ShowMessage(this->callingRoutine + ": Potential issue with equipment sizing for " + this->compType + ' ' + this->compName);
                    ShowContinueError("User-Specified " + this->sizingString + " = " + General::RoundSigDigits(this->originalValue, 5));
                    ShowContinueError("differs from Design Size " + this->sizingString + " = " + General::RoundSigDigits(this->autoSizedValue, 5));
                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                }
            }
            if (!this->wasAutoSized) this->autoSizedValue = this->originalValue;
        } else {
            ShowSevereError(this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.");
            ShowContinueError("SizingString = " + this->sizingString + ", SizingResult = " + General::TrimSigDigits(this->originalValue, 1));
            this->errorType = AutoSizingResultType::ErrorType1;
        }
    } else if (!this->wasAutoSized && !this->autoCalculate) {
        this->autoSizedValue = this->originalValue;
    }

    if ( this->errorType != AutoSizingResultType::NoError ) {
        ShowSevereError("Developer Error: sizing of " + this->sizingString + " failed.");
        ShowContinueError("Occurs in " + this->compType + " " + this->compName);
        errorsFound = true;
    }
}
bool BaseSizer::isValidCoilType(std::string const &_compType)
{
    for (auto const &coilType : DataHVACGlobals::cAllCoilTypes) {
        if (UtilityRoutines::SameString(_compType, coilType)) {
            return true;
        }
    }
    return false;
}

bool BaseSizer::checkInitialized(bool &errorsFound) {
    if (!this->initialized) {
        errorsFound = true;
        this->errorType = AutoSizingResultType::ErrorType2;
        this->autoSizedValue = 0.0;
        ShowSevereError("Developer Error: sizing of " + this->sizingString + " failed.");
        ShowContinueError("Occurs in " + this->compType + " " + this->compName);
        return false;
    }
    return true;
}

void BaseSizer::overrideSizingString(std::string const &string)
{
    this->sizingString = string;
}


    void BaseSizer::clearState() {
        stdRhoAir = 0.0;

        getCoilReportObject = false; // provides access to coil reporting
        initialized = false;     // indicates initializeWithinEP was called
        errorType = AutoSizingResultType::NoError;
        sizingString = "";
        originalValue = 0.0;
        autoSizedValue = 0.0;
        wasAutoSized = false;
        hardSizeNoDesignRun = false;
        sizingDesRunThisAirSys = false;
        sizingDesRunThisZone = false;
        sizingDesValueFromParent = false;
        airLoopSysFlag = false;
        oaSysFlag = false;
        compType = "";
        compName = "";

        sysSizingRunDone = false;
        zoneSizingRunDone = false;
        curSysNum = 0;
        curOASysNum = 0;
        curZoneEqNum = 0;
        curDuctType = 0;
        curTermUnitSizingNum = 0; // index in zone equipment vector - for single duct, IU, and PIU
        numPrimaryAirSys = 0;
        numSysSizInput = 0;
        doSystemSizing = false;
        numZoneSizingInput = 0;
        doZoneSizing = false;
        autoCalculate = false; // indicator that AutoCalculate is used

        // terminal units
        termUnitSingDuct = false; // single duct terminal unit
        termUnitPIU = false;      // powered induction unit
        termUnitIU = false;       // induction terminal unit
        zoneEqFanCoil = false;    // fan coil zone equipment
        otherEqType = false;      // this covers the ELSE type switch
        zoneEqUnitHeater = false; // unit heater zone equipment
        zoneEqUnitVent = false;   // unit ventilator zone equipment
        zoneEqVentedSlab = false; // ventilated slab zone equipment

        // error message handling
        getLastErrorMessages();

        // global sizing data
        minOA = 0.0;

        // global Data* sizing constants
        dataEMSOverrideON = false;
        dataEMSOverride = 0.0;
        dataConstantUsedForSizing = 0.0;
        dataFractionUsedForSizing = 0.0;
        dataPltSizHeatNum = 0;
        dataWaterLoopNum = 0;
        dataFanIndex = -1;
        dataFanEnumType = -1;
        dataWaterCoilSizCoolDeltaT = 0.0;
        dataWaterCoilSizHeatDeltaT = 0.0;
        dataCapacityUsedForSizing = 0.0;
        dataPltSizCoolNum = 0;
        dataDesInletAirHumRat = 0.0;
        dataFlowUsedForSizing = 0.0;
        dataDesOutletAirHumRat = 0.0;
        dataDesInletWaterTemp = 0.0;
        dataDesOutletAirTemp = 0.0;
        dataWaterFlowUsedForSizing = 0.0;
        dataSizingFraction = 1.0;
        dataDXSpeedNum = 0.0;

        printWarningFlag = false;
        callingRoutine = "";
        sysSizingInputData.clear();
        zoneSizingInput.clear();
        unitarySysEqSizing.clear();
        oaSysEqSizing.clear();
        zoneEqSizing.clear();
        outsideAirSys.clear();
        termUnitSizing.clear();
        termUnitFinalZoneSizing.clear();
        finalZoneSizing.clear();
        finalSysSizing.clear();
        plantSizData.clear();
        primaryAirSystem.clear();
        airloopDOAS.clear();
    }

} // namespace EnergyPlus
