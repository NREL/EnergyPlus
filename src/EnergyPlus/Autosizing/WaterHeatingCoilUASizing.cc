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

#include <EnergyPlus/Autosizing/WaterHeatingCoilUASizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

Real64 WaterHeatingCoilUASizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    static Array1D<Real64> Par(4);
    Real64 const Acc(0.0001); // Accuracy of result
    int const MaxIte(500);    // Maximum number of iterations
    int SolFla = 0;           // Flag of solver

    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataCapacityUsedForSizing > 0.0 && this->dataWaterFlowUsedForSizing > 0.0 && this->dataFlowUsedForSizing > 0.0) {
                Par(1) = this->dataCapacityUsedForSizing;
                Par(2) = double(this->dataCoilNum);
                Par(3) = double(this->dataFanOpMode); // fan operating mode
                Par(4) = 1.0;                         // part-load ratio
                Real64 UA0 = 0.001 * this->dataCapacityUsedForSizing;
                Real64 UA1 = this->dataCapacityUsedForSizing;
                // Invert the simple heating coil model: given the design inlet conditions and the design load,
                // find the design UA.
                General::SolveRoot(Acc, MaxIte, SolFla, this->autoSizedValue, WaterCoils::SimpleHeatingCoilUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + this->compName + "\"");
                    ShowContinueError("  Iteration limit exceeded in calculating coil UA");
                    ShowContinueError("  Lower UA estimate = " + General::TrimSigDigits(UA0, 6) + " W/m2-K (0.1% of Design Coil Load)");
                    ShowContinueError("  Upper UA estimate = " + General::TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                    ShowContinueError("  Final UA estimate when iterations exceeded limit = " + General::TrimSigDigits(this->autoSizedValue, 6) +
                                      " W/m2-K");
                    ShowContinueError("  Zone \"" + this->finalZoneSizing(this->curZoneEqNum).ZoneName +
                                      "\" coil sizing conditions (may be different than Sizing inputs):");
                    ShowContinueError("  Coil inlet air temperature     = " + General::TrimSigDigits(this->dataDesInletAirTemp, 3) + " C");
                    ShowContinueError("  Coil inlet air humidity ratio  = " + General::TrimSigDigits(this->dataDesInletAirHumRat, 3) +
                                      " kgWater/kgDryAir");
                    ShowContinueError("  Coil inlet air mass flow rate  = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 6) + " kg/s");
                    // TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
                    ShowContinueError("  Design Coil Capacity           = " + General::TrimSigDigits(this->dataDesignCoilCapacity, 3) + " W");
                    if (this->dataNomCapInpMeth) {
                        ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                        ShowContinueError("  Coil outlet air temperature    = " + General::TrimSigDigits(this->dataDesOutletAirTemp, 3) + " C");
                        ShowContinueError("  Coil outlet air humidity ratio = " + General::TrimSigDigits(this->dataDesOutletAirHumRat, 3) +
                                          " kgWater/kgDryAir");
                    } else if (this->termUnitSingDuct || this->termUnitPIU || this->termUnitIU || this->zoneEqFanCoil) {
                        ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                    } else {
                        ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                        ShowContinueError("  Coil outlet air temperature    = " +
                                          General::TrimSigDigits(this->finalZoneSizing(this->curZoneEqNum).HeatDesTemp, 3) + " C");
                        ShowContinueError("  Coil outlet air humidity ratio = " +
                                          General::TrimSigDigits(this->finalZoneSizing(this->curZoneEqNum).HeatDesHumRat, 3) + " kgWater/kgDryAir");
                    }
                    this->dataErrorsFound = true;
                } else if (SolFla == -2) {
                    ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + this->compName + "\"");
                    ShowContinueError("  Bad starting values for UA");
                    ShowContinueError("  Lower UA estimate = " + General::TrimSigDigits(UA0, 6) + " W/m2-K (0.1% of Design Coil Load)");
                    ShowContinueError("  Upper UA estimate = " + General::TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                    ShowContinueError("  Zone \"" + this->finalZoneSizing(this->curZoneEqNum).ZoneName +
                                      "\" coil sizing conditions (may be different than Sizing inputs):");
                    ShowContinueError("  Coil inlet air temperature     = " + General::TrimSigDigits(this->dataDesInletAirTemp, 3) + " C");
                    ShowContinueError("  Coil inlet air humidity ratio  = " + General::TrimSigDigits(this->dataDesInletAirHumRat, 3) +
                                      " kgWater/kgDryAir");
                    ShowContinueError("  Coil inlet air mass flow rate  = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 6) + " kg/s");
                    ShowContinueError("  Design Coil Capacity           = " + General::TrimSigDigits(this->dataDesignCoilCapacity, 3) + " W");
                    if (this->dataNomCapInpMeth) {
                        ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                        ShowContinueError("  Coil outlet air temperature    = " + General::TrimSigDigits(this->dataDesOutletAirTemp, 3) + " C");
                        ShowContinueError("  Coil outlet air humidity ratio = " + General::TrimSigDigits(this->dataDesOutletAirHumRat, 3) +
                                          " kgWater/kgDryAir");
                    } else if (this->termUnitSingDuct || this->termUnitPIU || this->termUnitIU || this->zoneEqFanCoil) {
                        ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                    } else {
                        ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                        ShowContinueError("  Coil outlet air temperature    = " +
                                          General::TrimSigDigits(this->finalZoneSizing(this->curZoneEqNum).HeatDesTemp, 3) + " C");
                        ShowContinueError("  Coil outlet air humidity ratio = " +
                                          General::TrimSigDigits(this->finalZoneSizing(this->curZoneEqNum).HeatDesHumRat, 3) + " kgWater/kgDryAir");
                    }
                    // TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
                    if (this->dataDesignCoilCapacity < this->dataCapacityUsedForSizing) {
                        ShowContinueError("  Inadequate water side capacity: in Plant Sizing for this hot water loop");
                        ShowContinueError("  increase design loop exit temperature and/or decrease design loop delta T");
                        ShowContinueError("  Plant Sizing object = " + this->plantSizData(this->dataPltSizHeatNum).PlantLoopName);
                        ShowContinueError("  Plant design loop exit temperature = " +
                                          General::TrimSigDigits(this->plantSizData(this->dataPltSizHeatNum).ExitTemp, 3) + " C");
                        ShowContinueError("  Plant design loop delta T          = " + General::TrimSigDigits(this->dataWaterCoilSizHeatDeltaT, 3) +
                                          " C");
                    }
                    this->dataErrorsFound = true;
                }
            } else {
                this->autoSizedValue = 1.0;
                if (this->dataWaterFlowUsedForSizing > 0.0 && this->dataCapacityUsedForSizing == 0.0) {
                    ShowWarningError("The design coil load used for UA sizing is zero for Coil:Heating:Water " + this->compName);
                    ShowContinueError("An autosize value for UA cannot be calculated");
                    ShowContinueError("Input a value for UA, change the heating design day, or raise");
                    ShowContinueError("  the zone heating design supply air temperature");
                    ShowContinueError("Water coil UA is set to 1 and the simulation continues.");
                }
            }
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataCapacityUsedForSizing >= DataHVACGlobals::SmallLoad && this->dataWaterFlowUsedForSizing > 0.0 &&
                this->dataFlowUsedForSizing > 0.0) {
                Par(1) = this->dataCapacityUsedForSizing;
                Par(2) = double(this->dataCoilNum);
                Par(3) = double(this->dataFanOpMode); // fan operating mode
                Par(4) = 1.0;                         // part-load ratio
                Real64 UA0 = 0.001 * this->dataCapacityUsedForSizing;
                Real64 UA1 = this->dataCapacityUsedForSizing;
                // Invert the simple heating coil model: given the design inlet conditions and the design load,
                // find the design UA.
                General::SolveRoot(Acc, MaxIte, SolFla, this->autoSizedValue, WaterCoils::SimpleHeatingCoilUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + this->compName + "\"");
                    ShowContinueError("  Iteration limit exceeded in calculating coil UA");
                    ShowContinueError("  Lower UA estimate = " + General::TrimSigDigits(UA0, 6) + " W/m2-K (1% of Design Coil Load)");
                    ShowContinueError("  Upper UA estimate = " + General::TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                    ShowContinueError("  Final UA estimate when iterations exceeded limit = " + General::TrimSigDigits(this->autoSizedValue, 6) +
                                      " W/m2-K");
                    ShowContinueError("  AirloopHVAC \"" + this->finalSysSizing(this->curSysNum).AirPriLoopName +
                                      "\" coil sizing conditions (may be different than Sizing inputs):");
                    ShowContinueError("  Coil inlet air temperature     = " + General::TrimSigDigits(this->dataDesInletAirTemp, 3) + " C");
                    ShowContinueError("  Coil inlet air humidity ratio  = " + General::TrimSigDigits(this->dataDesInletAirHumRat, 3) +
                                      " kgWater/kgDryAir");
                    ShowContinueError("  Coil inlet air mass flow rate  = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 6) + " kg/s");
                    ShowContinueError("  Design Coil Capacity           = " + General::TrimSigDigits(this->dataDesignCoilCapacity, 3) + " W");
                    ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                    if (this->dataNomCapInpMeth) {
                        ShowContinueError("  Coil outlet air temperature    = " + General::TrimSigDigits(this->dataDesOutletAirTemp, 3) + " C");
                        ShowContinueError("  Coil outlet air humidity ratio = " + General::TrimSigDigits(this->dataDesOutletAirHumRat, 3) +
                                          " kgWater/kgDryAir");
                    }
                    this->dataErrorsFound = true;
                } else if (SolFla == -2) {
                    ShowSevereError("Autosizing of heating coil UA failed for Coil:Heating:Water \"" + this->compName + "\"");
                    ShowContinueError("  Bad starting values for UA");
                    ShowContinueError("  Lower UA estimate = " + General::TrimSigDigits(UA0, 6) + " W/m2-K (1% of Design Coil Load)");
                    ShowContinueError("  Upper UA estimate = " + General::TrimSigDigits(UA1, 6) + " W/m2-K (100% of Design Coil Load)");
                    ShowContinueError("  AirloopHVAC \"" + this->finalSysSizing(this->curSysNum).AirPriLoopName +
                                      "\" coil sizing conditions (may be different than Sizing inputs):");
                    ShowContinueError("  Coil inlet air temperature     = " + General::TrimSigDigits(this->dataDesInletAirTemp, 3) + " C");
                    ShowContinueError("  Coil inlet air humidity ratio  = " + General::TrimSigDigits(this->dataDesInletAirHumRat, 3) +
                                      " kgWater/kgDryAir");
                    ShowContinueError("  Coil inlet air mass flow rate  = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 6) + " kg/s");
                    ShowContinueError("  Design Coil Capacity           = " + General::TrimSigDigits(this->dataDesignCoilCapacity, 3) + " W");
                    ShowContinueError("  Design Coil Load               = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 3) + " W");
                    if (this->dataNomCapInpMeth) {
                        ShowContinueError("  Coil outlet air temperature    = " + General::TrimSigDigits(this->dataDesOutletAirTemp, 3) + " C");
                        ShowContinueError("  Coil outlet air humidity ratio = " + General::TrimSigDigits(this->dataDesOutletAirHumRat, 3) +
                                          " kgWater/kgDryAir");
                    }
                    if (this->dataDesignCoilCapacity < this->dataCapacityUsedForSizing && !this->dataNomCapInpMeth) {
                        ShowContinueError("  Inadequate water side capacity: in Plant Sizing for this hot water loop");
                        ShowContinueError("  increase design loop exit temperature and/or decrease design loop delta T");
                        ShowContinueError("  Plant Sizing object = " + this->plantSizData(this->dataPltSizHeatNum).PlantLoopName);
                        ShowContinueError("  Plant design loop exit temperature = " +
                                          General::TrimSigDigits(this->plantSizData(this->dataPltSizHeatNum).ExitTemp, 3) + " C");
                        ShowContinueError("  Plant design loop delta T          = " + General::TrimSigDigits(this->dataWaterCoilSizHeatDeltaT, 3) +
                                          " C");
                    }
                    this->dataErrorsFound = true;
                }
            } else {
                this->autoSizedValue = 1.0;
                if (this->dataWaterFlowUsedForSizing > 0.0 && this->dataCapacityUsedForSizing < DataHVACGlobals::SmallLoad) {
                    ShowWarningError("The design coil load used for UA sizing is too small for Coil:Heating:Water " + this->compName);
                    ShowContinueError("An autosize value for UA cannot be calculated");
                    ShowContinueError("Input a value for UA, change the heating design day, or raise");
                    ShowContinueError("  the system heating design supply air temperature");
                    ShowContinueError("Water coil UA is set to 1 and the simulation continues.");
                }
            }
        }
    }
    if (this->dataErrorsFound) DataSizing::DataErrorsFound = true;
    if (this->isEpJSON) this->sizingString = "u-factor_times_area_value [W/K]";
    this->selectSizerOutput(errorsFound);
    if (this->getCoilReportObject && this->curSysNum <= DataHVACGlobals::NumPrimaryAirSys) {
        coilSelectionReportObj->setCoilUA(this->compName,
                                          this->compType,
                                          this->autoSizedValue,
                                          this->dataCapacityUsedForSizing,
                                          this->wasAutoSized,
                                          this->curSysNum,
                                          this->curZoneEqNum);
    }
    return this->autoSizedValue;
}

} // namespace EnergyPlus
