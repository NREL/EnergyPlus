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

#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

Real64 HeatingCapacitySizer::size(EnergyPlusData &state, Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(state, _originalValue);
    std::string DDNameFanPeak = "";
    std::string dateTimeFanPeak = "";
    Real64 DesVolFlow = 0.0;
    Real64 CoilInTemp = -999.0;
    Real64 CoilInHumRat = -999.0;
    Real64 CoilOutTemp = -999.0;
    Real64 CoilOutHumRat = -999.0;
    Real64 FanCoolLoad = 0.0;
    Real64 TotCapTempModFac = 1.0;
    Real64 DXFlowPerCapMinRatio = 1.0;
    Real64 DXFlowPerCapMaxRatio = 1.0;
    Real64 NominalCapacityDes = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilLoad = 0.0;
    Real64 OutAirFrac = 0.0;
    Real64 CpAirStd = Psychrometrics::PsyCpAirFnW(0.0);

    if (this->dataEMSOverrideON) {
        this->autoSizedValue = this->dataEMSOverride;
    } else if (this->dataConstantUsedForSizing >= 0 && this->dataFractionUsedForSizing > 0) {
        // back and forth if dataConstantUsedForSizing should be > or >= 0 to make this work for AutoCalculate
        this->autoSizedValue = this->dataConstantUsedForSizing * this->dataFractionUsedForSizing;
    } else {
        if (this->curZoneEqNum > 0) {
            if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
                this->autoSizedValue = _originalValue;
            } else if (this->zoneEqSizing(this->curZoneEqNum).DesignSizeFromParent) {
                this->autoSizedValue = this->zoneEqSizing(this->curZoneEqNum).DesHeatingLoad;
            } else {
                if (this->zoneEqSizing(this->curZoneEqNum).HeatingCapacity) {
                    NominalCapacityDes = this->zoneEqSizing(this->curZoneEqNum).DesHeatingLoad;
                    if (this->dataFlowUsedForSizing > 0.0) {
                        DesVolFlow = this->dataFlowUsedForSizing;
                    }
                } else if (this->dataCoolCoilCap > 0.0 && this->dataFlowUsedForSizing > 0.0) {
                    NominalCapacityDes = this->dataCoolCoilCap;
                    DesVolFlow = this->dataFlowUsedForSizing;
                } else if (int(this->finalZoneSizing.size()) > 0 &&
                           this->finalZoneSizing(this->curZoneEqNum).DesHeatMassFlow >= DataHVACGlobals::SmallMassFlow) {
                    if (this->dataFlowUsedForSizing > 0.0) {
                        DesVolFlow = this->dataFlowUsedForSizing;
                    }
                    if (this->termUnitPIU && (this->curTermUnitSizingNum > 0)) {
                        Real64 MinPriFlowFrac = this->termUnitSizing(this->curTermUnitSizingNum).MinFlowFrac;
                        if (this->termUnitSizing(this->curTermUnitSizingNum).InducesPlenumAir) {
                            CoilInTemp = (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU * MinPriFlowFrac) +
                                         (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneRetTempAtHeatPeak * (1.0 - MinPriFlowFrac));
                        } else {
                            CoilInTemp = (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU * MinPriFlowFrac) +
                                         (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneTempAtHeatPeak * (1.0 - MinPriFlowFrac));
                        }
                    } else if (this->zoneEqFanCoil) {
                        // use fan coil flow (i.e., set by parent) or flow used during sizing?
                        if (DesVolFlow > 0.0) {
                            DesMassFlow = DesVolFlow * DataEnvironment::StdRhoAir;
                        } else {
                            DesMassFlow = this->finalZoneSizing(this->curZoneEqNum).DesHeatMassFlow;
                        }
                        CoilInTemp =
                            this->setHeatCoilInletTempForZoneEqSizing(this->setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing(this->curZoneEqNum)),
                                                                      zoneEqSizing(this->curZoneEqNum),
                                                                      finalZoneSizing(this->curZoneEqNum));
                        CoilInHumRat =
                            this->setHeatCoilInletHumRatForZoneEqSizing(this->setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing(this->curZoneEqNum)),
                                                                        zoneEqSizing(this->curZoneEqNum),
                                                                        finalZoneSizing(this->curZoneEqNum));
                    } else if (this->termUnitIU && (this->curTermUnitSizingNum > 0)) {
                        CoilInTemp = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneTempAtHeatPeak;
                        CoilInHumRat = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                    } else if (this->termUnitSingDuct && (this->curTermUnitSizingNum > 0)) {
                        CoilInTemp = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU;
                        CoilInHumRat = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInHumRatTU;
                    } else {
                        if (DesVolFlow > 0.0) {
                            DesMassFlow = DesVolFlow * DataEnvironment::StdRhoAir;
                        } else {
                            DesMassFlow = this->finalZoneSizing(this->curZoneEqNum).DesHeatMassFlow;
                        }
                        CoilInTemp =
                            this->setHeatCoilInletTempForZoneEqSizing(this->setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing(this->curZoneEqNum)),
                                                                      zoneEqSizing(this->curZoneEqNum),
                                                                      finalZoneSizing(this->curZoneEqNum));
                        CoilInHumRat =
                            this->setHeatCoilInletHumRatForZoneEqSizing(this->setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing(this->curZoneEqNum)),
                                                                        zoneEqSizing(this->curZoneEqNum),
                                                                        finalZoneSizing(this->curZoneEqNum));
                    }
                    if ((this->termUnitSingDuct || this->termUnitPIU) && (this->curTermUnitSizingNum > 0)) {
                        CoilOutTemp = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).HeatDesTemp;
                        CoilOutHumRat = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).HeatDesHumRat;
                        Real64 CpAir = Psychrometrics::PsyCpAirFnW(CoilOutHumRat);
                        DesCoilLoad = CpAir * DataEnvironment::StdRhoAir * this->termUnitSizing(this->curTermUnitSizingNum).AirVolFlow *
                                      (CoilOutTemp - CoilInTemp);
                        DesVolFlow = this->termUnitSizing(this->curTermUnitSizingNum).AirVolFlow;
                    } else if (this->termUnitIU && (this->curTermUnitSizingNum > 0)) {
                        if (this->termUnitSizing(this->curTermUnitSizingNum).InducRat > 0.01) {
                            DesVolFlow = this->termUnitSizing(this->curTermUnitSizingNum).AirVolFlow /
                                         this->termUnitSizing(this->curTermUnitSizingNum).InducRat;
                            Real64 CpAir = Psychrometrics::PsyCpAirFnW(this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).HeatDesHumRat);
                            // the design heating coil load is the zone load minus whatever the central system does.Note that
                            // DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
                            DesCoilLoad = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatLoad -
                                          (CpAir * DataEnvironment::StdRhoAir * DesVolFlow *
                                           (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU -
                                            this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneTempAtHeatPeak));
                        } else {
                            DesCoilLoad = 0.0;
                        }
                    } else {
                        CoilOutTemp = this->finalZoneSizing(this->curZoneEqNum).HeatDesTemp;
                        CoilOutHumRat = this->finalZoneSizing(this->curZoneEqNum).HeatDesHumRat;
                        Real64 CpAir = Psychrometrics::PsyCpAirFnW(CoilOutHumRat);
                        DesCoilLoad = CpAir * this->finalZoneSizing(this->curZoneEqNum).DesHeatMassFlow * (CoilOutTemp - CoilInTemp);
                        DesVolFlow = this->finalZoneSizing(this->curZoneEqNum).DesHeatMassFlow / DataEnvironment::StdRhoAir;
                    }
                    NominalCapacityDes = max(0.0, DesCoilLoad);
                } else {
                    NominalCapacityDes = 0.0;
                    CoilOutTemp = -999.0;
                }
                if (this->dataCoolCoilCap > 0.0) {
                    this->autoSizedValue = NominalCapacityDes * this->dataHeatSizeRatio;
                } else {
                    this->autoSizedValue = NominalCapacityDes * this->dataHeatSizeRatio * this->dataFracOfAutosizedHeatingCapacity;
                }
                if (DataGlobals::DisplayExtraWarnings && this->autoSizedValue <= 0.0) {
                    ShowWarningMessage(this->callingRoutine + ": Potential issue with equipment sizing for " + this->compType + ' ' + this->compName);
                    ShowContinueError("...Rated Total Heating Capacity = " + General::TrimSigDigits(this->autoSizedValue, 2) + " [W]");
                    if (this->zoneEqSizing(this->curZoneEqNum).HeatingCapacity ||
                        (this->dataCoolCoilCap > 0.0 && this->dataFlowUsedForSizing > 0.0)) {
                        ShowContinueError("...Capacity passed by parent object to size child component = " +
                                          General::TrimSigDigits(NominalCapacityDes, 2) + " [W]");
                    } else {
                        if (CoilOutTemp > -999.0) {
                            ShowContinueError("...Air flow rate used for sizing = " + General::TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                            ShowContinueError("...Coil inlet air temperature used for sizing = " + General::TrimSigDigits(CoilInTemp, 2) + " [C]");
                            ShowContinueError("...Coil outlet air temperature used for sizing = " + General::TrimSigDigits(CoilOutTemp, 2) + " [C]");
                        } else {
                            ShowContinueError("...Capacity used to size child component set to 0 [W]");
                        }
                    }
                }
            }
        } else if (this->curSysNum > 0) {
            if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
                this->autoSizedValue = _originalValue;
            } else {
                this->dataFracOfAutosizedHeatingCapacity = 1.0;
                if (this->curOASysNum > 0) {
                    if (this->oaSysEqSizing(this->curOASysNum).AirFlow) {
                        DesVolFlow = this->oaSysEqSizing(this->curOASysNum).AirVolFlow;
                    } else if (this->oaSysEqSizing(this->curOASysNum).HeatingAirFlow) {
                        DesVolFlow = this->oaSysEqSizing(this->curOASysNum).HeatingAirVolFlow;
                    } else if (this->outsideAirSys(this->curOASysNum).AirLoopDOASNum > -1) {
                        DesVolFlow =
                            this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].SizingMassFlow / DataEnvironment::StdRhoAir;
                    } else {
                        DesVolFlow = this->finalSysSizing(this->curSysNum).DesOutAirVolFlow;
                    }
                } else {
                    if (this->finalSysSizing(this->curSysNum).HeatingCapMethod == DataSizing::FractionOfAutosizedHeatingCapacity) {
                        this->dataFracOfAutosizedHeatingCapacity = this->finalSysSizing(this->curSysNum).FractionOfAutosizedHeatingCapacity;
                    }
                    if (this->dataFlowUsedForSizing > 0.0) {
                        DesVolFlow = this->dataFlowUsedForSizing;
                    } else if (this->unitarySysEqSizing(this->curSysNum).AirFlow) {
                        DesVolFlow = this->unitarySysEqSizing(this->curSysNum).AirVolFlow;
                    } else if (this->unitarySysEqSizing(this->curSysNum).HeatingAirFlow) {
                        DesVolFlow = this->unitarySysEqSizing(this->curSysNum).HeatingAirVolFlow;
                    } else {
                        if (this->curDuctType == DataHVACGlobals::Main) {
                            if (this->finalSysSizing(this->curSysNum).SysAirMinFlowRat > 0.0 && !this->dataDesicRegCoil) {
                                DesVolFlow =
                                    this->finalSysSizing(this->curSysNum).SysAirMinFlowRat * this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                            } else {
                                DesVolFlow = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                            }
                        } else if (this->curDuctType == DataHVACGlobals::Cooling) {
                            if (this->finalSysSizing(this->curSysNum).SysAirMinFlowRat > 0.0 && !this->dataDesicRegCoil) {
                                DesVolFlow =
                                    this->finalSysSizing(this->curSysNum).SysAirMinFlowRat * this->finalSysSizing(this->curSysNum).DesCoolVolFlow;
                            } else {
                                DesVolFlow = this->finalSysSizing(this->curSysNum).DesCoolVolFlow;
                            }
                        } else if (this->curDuctType == DataHVACGlobals::Heating) {
                            DesVolFlow = this->finalSysSizing(this->curSysNum).DesHeatVolFlow;
                        } else if (this->curDuctType == DataHVACGlobals::Other) {
                            DesVolFlow = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                        } else {
                            DesVolFlow = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                        }
                    }
                }
                DesMassFlow = DataEnvironment::StdRhoAir * DesVolFlow;
                // get the outside air fraction
                if (this->curOASysNum > 0) {
                    OutAirFrac = 1.0;
                } else if (this->finalSysSizing(this->curSysNum).HeatOAOption == DataSizing::MinOA) {
                    if (DesVolFlow > 0.0) {
                        OutAirFrac = this->finalSysSizing(this->curSysNum).DesOutAirVolFlow / DesVolFlow;
                    } else {
                        OutAirFrac = 1.0;
                    }
                    OutAirFrac = std::min(1.0, std::max(0.0, OutAirFrac));
                } else {
                    OutAirFrac = 1.0;
                }
                // coil inlet temperature
                if (this->curOASysNum == 0 && this->primaryAirSystem(this->curSysNum).NumOAHeatCoils > 0) {
                    CoilInTemp = OutAirFrac * this->finalSysSizing(this->curSysNum).PreheatTemp +
                                 (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).HeatRetTemp;
                    CoilInHumRat = OutAirFrac * this->finalSysSizing(this->curSysNum).PreheatHumRat +
                                   (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).HeatRetHumRat; // include humrat for coil sizing reports
                } else if (this->curOASysNum > 0 && this->outsideAirSys(this->curOASysNum).AirLoopDOASNum > -1) {
                    CoilInTemp = this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].HeatOutTemp;
                    if (this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].m_FanIndex > -1 &&
                        this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].FanBlowTroughFlag &&
                        this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].m_FanTypeNum ==
                            SimAirServingZones::Fan_System_Object) {
                        int FanIndex = this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].m_FanIndex;
                        Real64 DeltaT = HVACFan::fanObjs[FanIndex]->getFanDesignTemperatureRise();
                        CoilInTemp += DeltaT;
                    }
                } else {
                    CoilInTemp = OutAirFrac * this->finalSysSizing(this->curSysNum).HeatOutTemp +
                                 (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).HeatRetTemp;
                    CoilInHumRat = OutAirFrac * this->finalSysSizing(this->curSysNum).HeatOutHumRat +
                                   (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).HeatRetHumRat; // include humrat for coil sizing reports
                }
                // coil load
                if (this->curOASysNum > 0) {
                    if (this->oaSysEqSizing(this->curOASysNum).HeatingCapacity) {
                        DesCoilLoad = this->oaSysEqSizing(this->curOASysNum).DesHeatingLoad;
                    } else if (this->dataDesicRegCoil) {
                        DesCoilLoad = CpAirStd * DesMassFlow * (this->dataDesOutletAirTemp - this->dataDesInletAirTemp);
                        CoilOutTemp = this->dataDesOutletAirTemp;
                    } else if (this->outsideAirSys(this->curOASysNum).AirLoopDOASNum > -1) {
                        DesCoilLoad = CpAirStd * DesMassFlow *
                                      (this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].PreheatTemp - CoilInTemp);
                        CoilOutTemp = this->airloopDOAS[outsideAirSys(this->curOASysNum).AirLoopDOASNum].PreheatTemp;
                    } else {
                        DesCoilLoad = CpAirStd * DesMassFlow * (this->finalSysSizing(this->curSysNum).PreheatTemp - CoilInTemp);
                        CoilOutTemp = this->finalSysSizing(this->curSysNum).PreheatTemp;
                        CoilOutHumRat = this->finalSysSizing(this->curSysNum).PreheatHumRat;
                    }
                } else {
                    if (this->unitarySysEqSizing(this->curSysNum).HeatingCapacity) {
                        DesCoilLoad = this->unitarySysEqSizing(this->curSysNum).DesHeatingLoad;
                        // CoilOutTemp = -999.0; // initialized at top
                        CoilOutTemp = this->finalSysSizing(this->curSysNum).HeatSupTemp;
                        CoilOutHumRat = this->finalSysSizing(this->curSysNum).HeatSupHumRat;
                    } else if (this->dataDesicRegCoil) {
                        DesCoilLoad = CpAirStd * DesMassFlow * (this->dataDesOutletAirTemp - this->dataDesInletAirTemp);
                        CoilOutTemp = this->dataDesOutletAirTemp;
                    } else {
                        DesCoilLoad = CpAirStd * DesMassFlow * (this->finalSysSizing(this->curSysNum).HeatSupTemp - CoilInTemp);
                        CoilOutTemp = this->finalSysSizing(this->curSysNum).HeatSupTemp;
                        CoilOutHumRat = this->finalSysSizing(this->curSysNum).HeatSupHumRat;
                    }
                }
                if (this->curSysNum <= DataHVACGlobals::NumPrimaryAirSys && this->airLoopControlInfo(this->curSysNum).UnitarySys) {
                    if (this->dataCoilIsSuppHeater) {
                        NominalCapacityDes = this->suppHeatCap;
                    } else if (this->dataCoolCoilCap > 0.0) {
                        NominalCapacityDes = this->dataCoolCoilCap;
                    } else {
                        // TRUE for all air loop parent equipment except UnitarySystem where flag is reset to FALSE after simulating
                        // This method allows downstream heating coils to size individually.Probably should do this for all air loop equipment
                        // ChangoverBypass model always sets AirLoopControlInfo%UnitarySys to FALSE so heating coil can individually size
                        if (this->airLoopControlInfo(this->curSysNum).UnitarySysSimulating &&
                            !UtilityRoutines::SameString(this->compType, "COIL:HEATING:WATER")) {
                            NominalCapacityDes = this->unitaryHeatCap;
                        } else {
                            if (DesCoilLoad >= DataHVACGlobals::SmallLoad) {
                                NominalCapacityDes = DesCoilLoad;
                            } else {
                                NominalCapacityDes = 0.0;
                            }
                        }
                    }
                    DesCoilLoad = NominalCapacityDes;
                } else if (this->curSysNum <= DataHVACGlobals::NumPrimaryAirSys &&
                           this->finalSysSizing(this->curSysNum).HeatingCapMethod == DataSizing::CapacityPerFloorArea) {
                    NominalCapacityDes = this->finalSysSizing(this->curSysNum).HeatingTotalCapacity;
                } else if (this->curSysNum <= DataHVACGlobals::NumPrimaryAirSys &&
                           this->finalSysSizing(this->curSysNum).HeatingCapMethod == DataSizing::HeatingDesignCapacity &&
                           this->finalSysSizing(this->curSysNum).HeatingTotalCapacity > 0.0) {
                    NominalCapacityDes = this->finalSysSizing(this->curSysNum).HeatingTotalCapacity;
                } else {
                    if (this->dataCoolCoilCap > 0.0) { // this line can't get executed with same logic above else
                        NominalCapacityDes = this->dataCoolCoilCap;
                    } else if (DesCoilLoad >= DataHVACGlobals::SmallLoad) {
                        NominalCapacityDes = DesCoilLoad;
                    } else {
                        NominalCapacityDes = 0.0;
                    }
                }
                this->autoSizedValue = NominalCapacityDes * this->dataHeatSizeRatio * this->dataFracOfAutosizedHeatingCapacity;
                if (DataGlobals::DisplayExtraWarnings && this->autoSizedValue <= 0.0) {
                    ShowWarningMessage(this->callingRoutine + ": Potential issue with equipment sizing for " + this->compType + ' ' + this->compName);
                    ShowContinueError("...Rated Total Heating Capacity = " + General::TrimSigDigits(this->autoSizedValue, 2) + " [W]");
                    if (CoilOutTemp > -999.0) {
                        ShowContinueError("...Air flow rate used for sizing = " + General::TrimSigDigits(DesVolFlow, 5) + " [m3/s]");
                        ShowContinueError("...Outdoor air fraction used for sizing = " + General::TrimSigDigits(OutAirFrac, 2));
                        ShowContinueError("...Coil inlet air temperature used for sizing = " + General::TrimSigDigits(CoilInTemp, 2) + " [C]");
                        ShowContinueError("...Coil outlet air temperature used for sizing = " + General::TrimSigDigits(CoilOutTemp, 2) + " [C]");
                    } else {
                        ShowContinueError("...Capacity passed by parent object to size child component = " + General::TrimSigDigits(DesCoilLoad, 2) +
                                          " [W]");
                    }
                }
            }
        } else if (this->dataNonZoneNonAirloopValue > 0) {
            this->autoSizedValue = this->dataNonZoneNonAirloopValue;
        } else if (!this->wasAutoSized) {
            this->autoSizedValue = this->originalValue;
        } else {
            std::string msg = this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.";
            ShowSevereError(msg);
            this->addErrorMessage(msg);
            msg = "SizingString = " + this->sizingString + ", SizingResult = " + General::TrimSigDigits(this->autoSizedValue, 1);
            ShowContinueError(msg);
            this->addErrorMessage(msg);
            errorsFound = true;
        }
    }
    if (!this->hardSizeNoDesignRun || this->dataScalableSizingON || this->dataScalableCapSizingON) {
        if (this->wasAutoSized && this->dataFractionUsedForSizing == 0.0) {
            // Note: the VolFlowPerRatedTotCap check is not applicable for VRF-FluidTCtrl coil model, which implements variable flow fans and
            // determines capacity using physical calculations instead of emperical curves
            bool FlagCheckVolFlowPerRatedTotCap = true;
            if (UtilityRoutines::SameString(this->compType, "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl") ||
                UtilityRoutines::SameString(this->compType, "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl"))
                FlagCheckVolFlowPerRatedTotCap = false;

            if (this->dataIsDXCoil && FlagCheckVolFlowPerRatedTotCap) {
                Real64 RatedVolFlowPerRatedTotCap = 0.0;
                if (this->autoSizedValue > 0.0) {
                    RatedVolFlowPerRatedTotCap = DesVolFlow / this->autoSizedValue;
                }
                if (RatedVolFlowPerRatedTotCap < DataHVACGlobals::MinRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT)) {
                    if (!this->dataEMSOverride && DataGlobals::DisplayExtraWarnings && this->printWarningFlag) {
                        ShowWarningError(this->callingRoutine + ' ' + this->compType + ' ' + this->compName);
                        ShowContinueError("..." + this->sizingString +
                                          " will be limited by the minimum rated volume flow per rated total capacity ratio.");
                        ShowContinueError("...DX coil volume flow rate (m3/s ) = " + General::TrimSigDigits(DesVolFlow, 6));
                        ShowContinueError("...Requested capacity (W ) = " + General::TrimSigDigits(this->autoSizedValue, 3));
                        ShowContinueError("...Requested flow/capacity ratio (m3/s/W ) = " + General::TrimSigDigits(RatedVolFlowPerRatedTotCap, 3));
                        ShowContinueError("...Minimum flow/capacity ratio (m3/s/W ) = " +
                                          General::TrimSigDigits(DataHVACGlobals::MinRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT), 3));
                    }

                    DXFlowPerCapMinRatio = (DesVolFlow / DataHVACGlobals::MinRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT)) /
                                           this->autoSizedValue; // set DX Coil Capacity Increase Ratio from Too Low Flow/Capacity Ratio
                    this->autoSizedValue = DesVolFlow / DataHVACGlobals::MinRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT);

                    if (!this->dataEMSOverride && DataGlobals::DisplayExtraWarnings && this->printWarningFlag) {
                        ShowContinueError("...Adjusted capacity ( W ) = " + General::TrimSigDigits(this->autoSizedValue, 3));
                    }
                } else if (RatedVolFlowPerRatedTotCap > DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT)) {
                    if (!this->dataEMSOverride && DataGlobals::DisplayExtraWarnings && this->printWarningFlag) {
                        ShowWarningError(this->callingRoutine + ' ' + this->compType + ' ' + this->compName);
                        ShowContinueError("..." + this->sizingString +
                                          " will be limited by the maximum rated volume flow per rated total capacity ratio.");
                        ShowContinueError("...DX coil volume flow rate ( m3/s ) = " + General::TrimSigDigits(DesVolFlow, 6));
                        ShowContinueError("...Requested capacity ( W ) = " + General::TrimSigDigits(this->autoSizedValue, 3));
                        ShowContinueError("...Requested flow/capacity ratio ( m3/s/W ) = " + General::TrimSigDigits(RatedVolFlowPerRatedTotCap, 3));
                        ShowContinueError("...Maximum flow/capacity ratio ( m3/s/W ) = " +
                                          General::TrimSigDigits(DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT), 3));
                    }

                    DXFlowPerCapMaxRatio = DesVolFlow / DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT) /
                                           this->autoSizedValue; // set DX Coil Capacity Decrease Ratio from Too High Flow/Capacity Ratio
                    this->autoSizedValue = DesVolFlow / DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap(DataHVACGlobals::DXCT);

                    if (!this->dataEMSOverride && DataGlobals::DisplayExtraWarnings && this->printWarningFlag) {
                        ShowContinueError("...Adjusted capacity ( W ) = " + General::TrimSigDigits(this->autoSizedValue, 3));
                    }
                }
            }
        }
    }

    // override sizing string
    if (this->overrideSizeString) {
        if (this->isEpJSON) this->sizingString = "nominal_capacity [W]";
    }
    if (this->dataScalableCapSizingON) {
        auto const SELECT_CASE_var(this->zoneEqSizing(this->curZoneEqNum).SizingMethod(DataHVACGlobals::HeatingCapacitySizing));
        if (SELECT_CASE_var == DataSizing::CapacityPerFloorArea) {
            this->sizingStringScalable = "(scaled by capacity / area) ";
        } else if (SELECT_CASE_var == DataSizing::FractionOfAutosizedHeatingCapacity ||
                   SELECT_CASE_var == DataSizing::FractionOfAutosizedCoolingCapacity) {
            this->sizingStringScalable = "(scaled by fractional multiplier) ";
        }
    }

    this->selectSizerOutput(errorsFound);

    if (this->isCoilReportObject && this->curSysNum <= DataHVACGlobals::NumPrimaryAirSys) {
        if (CoilInTemp > -999.0) { // set inlet air properties used during capacity sizing if available, allow for negative winter temps
            coilSelectionReportObj->setCoilEntAirTemp(state, this->compName, this->compType, CoilInTemp, this->curSysNum, this->curZoneEqNum);
            coilSelectionReportObj->setCoilEntAirHumRat(this->compName, this->compType, CoilInHumRat);
        }
        if (CoilOutTemp > -999.0) { // set outlet air properties used during capacity sizing if available
            coilSelectionReportObj->setCoilLvgAirTemp(this->compName, this->compType, CoilOutTemp);
            coilSelectionReportObj->setCoilLvgAirHumRat(this->compName, this->compType, CoilOutHumRat);
        }
        coilSelectionReportObj->setCoilHeatingCapacity(state,
                                                       this->compName,
                                                       this->compType,
                                                       this->autoSizedValue,
                                                       this->wasAutoSized,
                                                       this->curSysNum,
                                                       this->curZoneEqNum,
                                                       this->curOASysNum,
                                                       FanCoolLoad,
                                                       TotCapTempModFac,
                                                       DXFlowPerCapMinRatio,
                                                       DXFlowPerCapMaxRatio);
    }
    return this->autoSizedValue;
}

void HeatingCapacitySizer::clearState()
{
    BaseSizerWithScalableInputs::clearState();
}

} // namespace EnergyPlus
