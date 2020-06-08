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

#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/api/TypeDefs.h>

namespace EnergyPlus {

    void BaseSizer::preSize(EnergyPlusData &EP_UNUSED(state), Real64 const _originalValue) {
        if (this->sizingType == AutoSizingType::Unknown) {
            ShowSevereError("Sizing Library Base Class: preSize");
            ShowFatalError("Developer Error: SizingType not defined.");
        }
        this->originalValue = _originalValue;
        this->hardSizeNoDesignRun = !(this->sysSizingRunDone || this->zoneSizingRunDone);

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
                        this->unitarySysEqSizing(this->curSysNum).CoolingCapacity ||
                        this->unitarySysEqSizing(this->curSysNum).HeatingCapacity;
            if (this->curOASysNum > 0) {
                this->oaSysFlag = this->oaSysEqSizing(this->curOASysNum).CoolingCapacity ||
                                  this->oaSysEqSizing(this->curOASysNum).HeatingCapacity;
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

            hardSizeNoDesignRun = false;
        }

        if (this->originalValue == DataSizing::AutoSize) {
            this->wasAutoSized = true;
            hardSizeNoDesignRun = false;
            if (!this->sizingDesRunThisAirSys && this->curSysNum > 0 &&
                this->sizingType != AutoSizingType::AutoCalculate) {
                if (!this->sysSizingRunDone) {
                    ShowSevereError("For autosizing of " + this->compType + ' ' + this->compName +
                                    ", a system sizing run must be done.");
                    if (this->numSysSizInput == 0) {
                        ShowContinueError("No \"Sizing:System\" objects were entered.");
                    }
                    if (!this->doSystemSizing) {
                        ShowContinueError(
                                R"(The "SimulationControl" object did not have the field "Do System Sizing Calculation" set to Yes.)");
                    }
                    ShowFatalError("Program terminates due to previously shown condition(s).");
                }
            }
            if (!this->sizingDesRunThisZone && this->curZoneEqNum > 0 && !this->sizingDesValueFromParent &&
                this->sizingType != AutoSizingType::AutoCalculate) {
                if (!this->zoneSizingRunDone) {
                    ShowSevereError("For autosizing of " + this->compType + ' ' + this->compName +
                                    ", a zone sizing run must be done.");
                    if (this->numZoneSizingInput == 0) {
                        ShowContinueError("No \"Sizing:Zone\" objects were entered.");
                    }
                    if (!this->doZoneSizing) {
                        ShowContinueError(
                                R"(The "SimulationControl" object did not have the field "Do Zone Sizing Calculation" set to Yes.)");
                    }
                    ShowFatalError("Program terminates due to previously shown condition(s).");
                }
            }
        }
    }

    void BaseSizer::reportSizerOutput(std::string const &CompType,
                                      std::string const &CompName,
                                      std::string const &VarDesc,
                                      Real64 const VarValue,
                                      Optional_string_const UsrDesc,
                                      Optional<Real64 const> UsrValue) {

        static bool MyOneTimeFlag(true);

        // Formats
        static constexpr auto Format_991(" Component Sizing Information, {}, {}, {}, {:.5R}\n");

        // to do, make this a parameter. Unfortunately this function is used in MANY
        // places so it involves touching most of E+
        auto &outputFiles = EnergyPlus::OutputFiles::getSingleton();
        if (MyOneTimeFlag) {
            static constexpr auto Format_990(
                    "! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n");
            if (ReportSizingManager::MyOneTimeFlag) print(outputFiles.eio, Format_990);
            ReportSizingManager::MyOneTimeFlag = false;
            MyOneTimeFlag = false;
        }

        print(outputFiles.eio, Format_991, CompType, CompName, VarDesc, VarValue);
        // add to tabular output reports
        OutputReportPredefined::AddCompSizeTableEntry(CompType, CompName, VarDesc, VarValue);

        if (present(UsrDesc) && present(UsrValue)) {
            print(outputFiles.eio, Format_991, CompType, CompName, UsrDesc(), UsrValue());
            OutputReportPredefined::AddCompSizeTableEntry(CompType, CompName, UsrDesc, UsrValue);
        } else if (present(UsrDesc) || present(UsrValue)) {
            ShowFatalError(
                    "ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both.");
        }

        // add to SQL output
        if (sqlite) sqlite->addSQLiteComponentSizingRecord(CompType, CompName, VarDesc, VarValue);
        if (present(UsrDesc) && present(UsrValue)) {
            if (sqlite) sqlite->addSQLiteComponentSizingRecord(CompType, CompName, UsrDesc, UsrValue);
        }
    }

    void BaseSizer::selectSizerOutput() {
        if (this->printWarningFlag) {
            if (this->wasAutoSized && this->autoSizedValue > 0.0) {
                this->reportSizerOutput(this->compType, this->compName,
                                        "Design Size " + this->sizingString, this->autoSizedValue);
            } else if (this->autoSizedValue > 0.0) {
                if ((std::abs(this->autoSizedValue - this->originalValue) / this->originalValue) >
                    DataSizing::AutoVsHardSizingThreshold) {
                    this->reportSizerOutput(this->compType,
                                            this->compName,
                                            "Design Size " + this->sizingString,
                                            this->autoSizedValue,
                                            "User-Specified " + this->sizingString,
                                            this->originalValue);
                } else {
                    this->reportSizerOutput(this->compType, this->compName,
                                            "User-Specified " + this->sizingString, this->originalValue);
                }
                if (DataGlobals::DisplayExtraWarnings) {
                    if ((std::abs(this->autoSizedValue - this->originalValue) / this->originalValue) >
                        DataSizing::AutoVsHardSizingThreshold) {
                        ShowMessage(this->callingRoutine + ": Potential issue with equipment sizing for " +
                                    this->compType + ' ' +
                                    this->compName);
                        ShowContinueError("User-Specified " + this->sizingString + " = " +
                                          General::RoundSigDigits(this->originalValue, 5));
                        ShowContinueError("differs from Design Size " + this->sizingString + " = " +
                                          General::RoundSigDigits(this->autoSizedValue, 5));
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(
                                "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
                this->autoSizedValue = this->originalValue;
            } else {
                ShowSevereError(this->callingRoutine + ' ' + this->compType + ' ' +
                                this->compName +
                                ", Developer Error: Component sizing incomplete.");
                ShowContinueError("SizingString = " + this->sizingString + ", SizingResult = " +
                                  General::TrimSigDigits(this->originalValue, 1));
            }
        }
    }
} // namespace EnergyPlus
