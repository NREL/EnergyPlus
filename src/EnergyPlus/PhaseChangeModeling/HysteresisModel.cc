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

#include <ObjexxFCL/Array1D.hh>

#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <EnergyPlus.hh>
#include <InputProcessing/InputProcessor.hh>
#include <PhaseChangeModeling/HysteresisModel.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HysteresisPhaseChange {

    bool getHysteresisModels(true);
    int numHysteresisModels = 0;
    std::vector<HysteresisPhaseChange> hysteresisPhaseChangeModels;

    HysteresisPhaseChange *HysteresisPhaseChange::factory(const std::string &objectName)
    {
        if (getHysteresisModels) {
            readAllHysteresisModels();
            getHysteresisModels = false;
        }
        for (auto &hm : hysteresisPhaseChangeModels) {
            if (hm.name == objectName) {
                return &hm;
            }
        }
        // because of the passive linking between materials and material property objects,
        // we don't know ahead of time for sure whether we will have a material property
        // so we can't return fatal here if it isn't found, just leave it null
        return nullptr;
    }

    Real64 HysteresisPhaseChange::getEnthalpy(Real64 T, Real64 Tc, Real64 tau1, Real64 tau2)
    {
        // Looks up the enthalpy on the characteristic curve defined by the parameters Tc, tau1, and tau2,
        // and the position on that curve defined by T.
        Real64 eta1 = (this->totalLatentHeat / 2) * exp(-2 * abs(T - Tc) / tau1);
        Real64 eta2 = (this->totalLatentHeat / 2) * exp(-2 * abs(T - Tc) / tau2);
        if (T <= Tc) {
            return (this->specificHeatSolid * T) + eta1;
        } else {
            return (this->specificHeatSolid * Tc) + this->totalLatentHeat + this->specificHeatLiquid * (T - Tc) - eta2;
        }
    }

    Real64 HysteresisPhaseChange::getCurrentSpecificHeat(
        Real64 prevTempTD, Real64 updatedTempTDT, Real64 phaseChangeTempReverse, int prevPhaseChangeState, int &phaseChangeState)
    {
        // Main public facing function; returns the current specific heat based on input properties, and current and previous conditions.
        // In a future version, this could be compartmentalized to track all states and histories, but it would require some further modification to
        // the HBFDManager
        Real64 TempLowPCM = this->peakTempMelting - this->deltaTempMeltingLow;
        Real64 TempHighPCM = this->peakTempMelting + this->deltaTempMeltingHigh;
        Real64 Tc;   // assigned later
        Real64 Tau1; // assigned later
        Real64 Tau2; // assigned later
        Real64 TempLowPCF = this->peakTempFreezing - this->deltaTempFreezingLow;
        Real64 TempHighPCF = this->peakTempFreezing + this->deltaTempFreezingHigh;
        Real64 Cp;
        Real64 phaseChangeDeltaT = prevTempTD - updatedTempTDT;

        // determine phase change state and curve characteristics based on delta T direction, updated temp, and previous state
        if (phaseChangeDeltaT <= 0) {
            Tc = this->peakTempMelting;
            Tau1 = this->deltaTempMeltingLow;
            Tau2 = this->deltaTempMeltingHigh;
            if (updatedTempTDT < TempLowPCM) {
                phaseChangeState = PhaseChangeStates::CRYSTALLIZED;
            } else if (updatedTempTDT >= TempLowPCM && updatedTempTDT <= TempHighPCM) {
                phaseChangeState = PhaseChangeStates::MELTING;
                if (prevPhaseChangeState == PhaseChangeStates::FREEZING || prevPhaseChangeState == PhaseChangeStates::TRANSITION) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                }
            } else if (updatedTempTDT > TempHighPCM) {
                phaseChangeState = PhaseChangeStates::LIQUID;
            }
        } else { // phaseChangeDeltaT > 0
            Tc = this->peakTempFreezing;
            Tau1 = this->deltaTempFreezingLow;
            Tau2 = this->deltaTempFreezingHigh;
            if (updatedTempTDT < TempLowPCF) {
                phaseChangeState = PhaseChangeStates::CRYSTALLIZED;
            } else if (updatedTempTDT >= TempLowPCF && updatedTempTDT <= TempHighPCF) {
                phaseChangeState = PhaseChangeStates::FREEZING;
                if (prevPhaseChangeState == PhaseChangeStates::MELTING || prevPhaseChangeState == PhaseChangeStates::TRANSITION) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                }
            } else if (updatedTempTDT > TempHighPCF) {
                phaseChangeState = PhaseChangeStates::LIQUID;
            }
        }

        // determine if we are transitioning or not
        if (prevPhaseChangeState == PhaseChangeStates::TRANSITION && phaseChangeState == PhaseChangeStates::CRYSTALLIZED) {
            this->phaseChangeTransition = true;
        } else if (prevPhaseChangeState == PhaseChangeStates::TRANSITION && phaseChangeState == PhaseChangeStates::FREEZING) {
            this->phaseChangeTransition = true;
            // this->phaseChangeState = 0; ?????
        } else if (prevPhaseChangeState == PhaseChangeStates::FREEZING && phaseChangeState == PhaseChangeStates::TRANSITION) {
            this->phaseChangeTransition = true;
        } else if (prevPhaseChangeState == PhaseChangeStates::CRYSTALLIZED && phaseChangeState == PhaseChangeStates::TRANSITION) {
            this->phaseChangeTransition = true;
        } else {
            this->phaseChangeTransition = false;
        }

        // now calculate the enthalpy appropriately
        if (!this->phaseChangeTransition) {
            this->enthOld = this->getEnthalpy(prevTempTD, Tc, Tau1, Tau2);
            this->enthNew = this->getEnthalpy(updatedTempTDT, Tc, Tau1, Tau2);
        } else {
            if (prevPhaseChangeState == PhaseChangeStates::FREEZING && phaseChangeState == PhaseChangeStates::TRANSITION) {
                this->enthRev =
                    this->getEnthalpy(phaseChangeTempReverse, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthOld - (this->specHeatTransition * prevTempTD));
                this->enthalpyM = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                this->enthalpyF = this->getEnthalpy(updatedTempTDT, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                if (this->enthNew < this->enthRev && this->enthNew >= this->enthalpyF && updatedTempTDT <= prevTempTD) {
                    phaseChangeState = PhaseChangeStates::FREEZING;
                    this->enthNew =
                        this->getEnthalpy(updatedTempTDT, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                } else if (this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthOld - (this->specHeatTransition * prevTempTD));
                } else if (this->enthNew < this->enthalpyF && updatedTempTDT > phaseChangeTempReverse) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                } else if (this->enthNew <= this->enthalpyM && updatedTempTDT <= phaseChangeTempReverse) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                }
            } else if (prevPhaseChangeState == PhaseChangeStates::TRANSITION && phaseChangeState == PhaseChangeStates::TRANSITION) {
                if (updatedTempTDT < phaseChangeTempReverse) {
                    Tc = this->peakTempMelting;
                    Tau1 = this->deltaTempMeltingLow;
                    Tau2 = this->deltaTempMeltingHigh;
                } else if (updatedTempTDT > phaseChangeTempReverse) {
                    Tc = this->peakTempFreezing;
                    Tau1 = this->deltaTempFreezingLow;
                    Tau2 = this->deltaTempFreezingHigh;
                }
                this->enthRev = this->getEnthalpy(phaseChangeTempReverse, Tc, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthOld - (this->specHeatTransition * prevTempTD));
                this->enthalpyM = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                this->enthalpyF = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                if (updatedTempTDT < phaseChangeTempReverse && this->enthNew > this->enthalpyF) {
                    phaseChangeState = PhaseChangeStates::FREEZING;
                    this->enthNew =
                        this->getEnthalpy(updatedTempTDT, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                } else if (this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM &&
                           (updatedTempTDT < prevTempTD || updatedTempTDT > prevTempTD)) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                } else if (this->enthNew <= this->enthalpyM && updatedTempTDT >= prevTempTD && this->enthNew > this->enthOld) {
                    phaseChangeState = PhaseChangeStates::MELTING;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                }
            } else if (prevPhaseChangeState == PhaseChangeStates::TRANSITION && phaseChangeState == PhaseChangeStates::CRYSTALLIZED) {
                this->enthRev =
                    this->getEnthalpy(phaseChangeTempReverse, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                this->enthalpyM = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                this->enthalpyF = this->getEnthalpy(updatedTempTDT, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                if (this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                } else if (this->enthNew <= this->enthalpyM && updatedTempTDT >= prevTempTD) {
                    phaseChangeState = PhaseChangeStates::MELTING;
                    this->enthNew = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                }
            } else if (prevPhaseChangeState == PhaseChangeStates::MELTING && phaseChangeState == PhaseChangeStates::TRANSITION) {
                this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthOld - (this->specHeatTransition * prevTempTD));
                this->enthalpyM = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                this->enthalpyF = this->getEnthalpy(updatedTempTDT, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                if (this->enthNew < this->enthOld && updatedTempTDT < prevTempTD) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthOld - (this->specHeatTransition * prevTempTD));
                } else if (this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM && updatedTempTDT < prevTempTD) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                } else if (this->enthNew >= this->enthalpyF && updatedTempTDT <= phaseChangeTempReverse) {
                    phaseChangeState = PhaseChangeStates::TRANSITION;
                    this->enthNew =
                        (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
                }
            } else if (prevPhaseChangeState == PhaseChangeStates::TRANSITION && phaseChangeState == PhaseChangeStates::FREEZING) {
                this->enthalpyM = this->getEnthalpy(updatedTempTDT, this->peakTempMelting, this->deltaTempMeltingLow, this->deltaTempMeltingHigh);
                this->enthalpyF = this->getEnthalpy(updatedTempTDT, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                this->enthRev =
                    this->getEnthalpy(phaseChangeTempReverse, this->peakTempFreezing, this->deltaTempFreezingLow, this->deltaTempFreezingHigh);
                this->enthNew = (this->specHeatTransition * updatedTempTDT) + (this->enthRev - (this->specHeatTransition * phaseChangeTempReverse));
            }
        }

        // then calculate the specific heat and return it
        if (!this->phaseChangeTransition) {
            if (this->enthNew == this->enthOld) {
                Cp = this->CpOld;
            } else {
                Cp = this->specHeat(prevTempTD, updatedTempTDT, Tc, Tau1, Tau2, this->enthOld, this->enthNew);
            }
        } else {
            Cp = this->specHeatTransition;
        }
        this->CpOld = Cp;
        return Cp;
    }

    Real64 HysteresisPhaseChange::specHeat(Real64 temperaturePrev,
                                           Real64 temperatureCurrent,
                                           Real64 criticalTemperature,
                                           Real64 tau1,
                                           Real64 tau2,
                                           Real64 EnthalpyOld,
                                           Real64 EnthalpyNew)
    {

        //	Tc                  ! Critical (Melting/Freezing) Temperature of PCM
        //	Tau1                ! Width of Melting Zone low
        //	Tau2                ! Width of Melting Zone high
        //	EnthalpyOld         ! Previous Timestep Nodal Enthalpy
        //	EnthalpyNew         ! Current Timestep Nodal Enthalpy

        Real64 T = temperatureCurrent;

        if (T < criticalTemperature) {
            Real64 DEta1 = -(this->totalLatentHeat * (T - criticalTemperature) * exp(-2 * abs(T - criticalTemperature) / tau1)) /
                           (tau1 * abs(T - criticalTemperature));
            Real64 Cp1 = this->specificHeatSolid;
            return (Cp1 + DEta1);
        } else if (T == criticalTemperature) {
            return (EnthalpyNew - EnthalpyOld) / (temperatureCurrent - temperaturePrev);
        } else if (T > criticalTemperature) {
            Real64 DEta2 = (this->totalLatentHeat * (T - criticalTemperature) * exp(-2 * abs(T - criticalTemperature) / tau2)) /
                           (tau2 * abs(T - criticalTemperature));
            Real64 Cp2 = this->specificHeatLiquid;
            return Cp2 + DEta2;
        } else {
            return 0;
        }
    }

    Real64 HysteresisPhaseChange::getConductivity(Real64 T)
    {
        if (T < this->peakTempMelting) {
            return this->fullySolidThermalConductivity;
        } else if (T > this->peakTempFreezing) {
            return this->fullyLiquidThermalConductivity;
        } else {
            return (this->fullySolidThermalConductivity + this->fullyLiquidThermalConductivity) / 2.0;
        }
    }

    Real64 HysteresisPhaseChange::getDensity(Real64 T)
    {
        if (T < this->peakTempMelting) {
            return this->fullySolidDensity;
        } else if (T > this->peakTempFreezing) {
            return this->fullyLiquidDensity;
        } else {
            return (this->fullySolidDensity + this->fullyLiquidDensity) / 2.0;
        }
    }

    void readAllHysteresisModels()
    {

        // convenience variables
        DataIPShortCuts::cCurrentModuleObject = "MaterialProperty:PhaseChangeHysteresis";
        numHysteresisModels = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        // loop over all hysteresis input instances, if zero, this will simply not do anything
        for (int hmNum = 1; hmNum <= numHysteresisModels; ++hmNum) {

            // just a few vars to pass in and out to GetObjectItem
            int ioStatus;
            int numAlphas;
            int numNumbers;

            // get the input data and store it in the Shortcuts structures
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject, hmNum, DataIPShortCuts::cAlphaArgs, numAlphas,
                                          DataIPShortCuts::rNumericArgs, numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);

            // the input processor validates the numeric inputs based on the IDD definition
            // still validate the name to make sure there aren't any duplicates or blanks
            // blanks are easy: fatal if blank
            if (DataIPShortCuts::lAlphaFieldBlanks[0]) {
                ShowFatalError("Invalid input for " + DataIPShortCuts::cCurrentModuleObject + " object: Name cannot be blank");
            }

            // we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
            for (auto &existingHysteresisModel : hysteresisPhaseChangeModels) {
                if (DataIPShortCuts::cAlphaArgs(1) == existingHysteresisModel.name) {
                    ShowFatalError("Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
                                   " object: Duplicate name found: " + existingHysteresisModel.name);
                }
            }

            // now build out a new hysteresis instance and add it to the vector
            HysteresisPhaseChange thisHM;
            thisHM.name = DataIPShortCuts::cAlphaArgs(1);
            thisHM.totalLatentHeat = DataIPShortCuts::rNumericArgs(1);
            thisHM.fullyLiquidThermalConductivity = DataIPShortCuts::rNumericArgs(2);
            thisHM.fullyLiquidDensity = DataIPShortCuts::rNumericArgs(3);
            thisHM.specificHeatLiquid = DataIPShortCuts::rNumericArgs(4);
            thisHM.deltaTempMeltingHigh = DataIPShortCuts::rNumericArgs(5);
            thisHM.peakTempMelting = DataIPShortCuts::rNumericArgs(6);
            thisHM.deltaTempMeltingLow = DataIPShortCuts::rNumericArgs(7);
            thisHM.fullySolidThermalConductivity = DataIPShortCuts::rNumericArgs(8);
            thisHM.fullySolidDensity = DataIPShortCuts::rNumericArgs(9);
            thisHM.specificHeatSolid = DataIPShortCuts::rNumericArgs(10);
            thisHM.deltaTempFreezingHigh = DataIPShortCuts::rNumericArgs(11);
            thisHM.peakTempFreezing = DataIPShortCuts::rNumericArgs(12);
            thisHM.deltaTempFreezingLow = DataIPShortCuts::rNumericArgs(13);
            thisHM.specHeatTransition = (thisHM.specificHeatSolid + thisHM.specificHeatLiquid) / 2.0;
            thisHM.CpOld = thisHM.specificHeatSolid;
            hysteresisPhaseChangeModels.push_back(thisHM);
        }
    }

    void clear_state()
    {
        numHysteresisModels = 0;
        getHysteresisModels = true;
        hysteresisPhaseChangeModels.clear();
    }

} // namespace HysteresisPhaseChange

} // namespace EnergyPlus
