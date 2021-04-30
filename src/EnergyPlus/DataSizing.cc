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

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>

namespace EnergyPlus::DataSizing {

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl
//       DATE WRITTEN   December 2000

// PURPOSE OF THIS MODULE:
// This data-only module contains type definitions and variables
// associated with HVAC system design flow rates, temperatures and
// capacities. This data is available to the HVAC component modules
// for their self sizing calculations.

Array1D_string const cOAFlowMethodTypes(NumOAFlowMethods,
                                        {"Flow/Person",
                                         "Flow/Zone",
                                         "Flow/Area",
                                         "AirChanges/Hour",
                                         "Sum",
                                         "Maximum",
                                         "IndoorAirQualityProcedure",
                                         "ProportionalControlBasedOnOccupancySchedule",
                                         "ProportionalControlBasedOnDesignOccupancy"});

//  days; includes effects of user multiplier
//  and user set flows)
//  of user input multiplier and flows
//  all design days, calculated only)
//  using user input system flow rates.
//  before applying user input sys flow rates.

Real64 TermUnitSizingData::applyTermUnitSizingCoolFlow(Real64 const &coolFlowWithOA, // Cooling flow rate with MinOA limit applied
                                                       Real64 const &coolFlowNoOA    // Cooling flow rate without MinOA limit applied
)
{
    // Apply DesignSpecification:AirTerminal:Sizing to cooling flow (could be vol flow or mass flow)
    Real64 coolFlowRatio = 1.0;
    if (this->SpecDesCoolSATRatio > 0.0) {
        coolFlowRatio = this->SpecDesSensCoolingFrac / this->SpecDesCoolSATRatio;
    } else {
        coolFlowRatio = this->SpecDesSensCoolingFrac;
    }
    Real64 adjustedFlow = coolFlowNoOA * coolFlowRatio + (coolFlowWithOA - coolFlowNoOA) * this->SpecMinOAFrac;
    return adjustedFlow;
}

Real64 TermUnitSizingData::applyTermUnitSizingHeatFlow(Real64 const &heatFlowWithOA, // Heating flow rate with MinOA limit applied
                                                       Real64 const &heatFlowNoOA    // Heating flow rate without MinOA limit applied
)
{
    // Apply DesignSpecification:AirTerminal:Sizing to heating flow (could be vol flow or mass flow)
    Real64 heatFlowRatio = 1.0;
    if (this->SpecDesHeatSATRatio > 0.0) {
        heatFlowRatio = this->SpecDesSensHeatingFrac / this->SpecDesHeatSATRatio;
    } else {
        heatFlowRatio = this->SpecDesSensHeatingFrac;
    }
    Real64 adjustedFlow = heatFlowNoOA * heatFlowRatio + (heatFlowWithOA - heatFlowNoOA) * this->SpecMinOAFrac;
    return adjustedFlow;
}

void ZoneSizingData::scaleZoneCooling(Real64 const ratio // Scaling ratio
)
{
    // Apply scaling ratio to TermUnitFinalZoneSizing cooling flow and load
    this->DesCoolVolFlow = this->DesCoolVolFlow * ratio;
    this->DesCoolMassFlow = this->DesCoolMassFlow * ratio;
    this->DesCoolLoad = this->DesCoolLoad * ratio;
    this->CoolFlowSeq = this->CoolFlowSeq * ratio;
    this->CoolLoadSeq = this->CoolLoadSeq * ratio;
}

void ZoneSizingData::scaleZoneHeating(Real64 const ratio // Scaling ratio
)
{
    // Apply scaling ratio to TermUnitFinalZoneSizing heating flow and load
    this->DesHeatVolFlow = this->DesHeatVolFlow * ratio;
    this->DesHeatMassFlow = this->DesHeatMassFlow * ratio;
    this->DesHeatLoad = this->DesHeatLoad * ratio;
    this->HeatFlowSeq = this->HeatFlowSeq * ratio;
    this->HeatLoadSeq = this->HeatLoadSeq * ratio;
}

void ZoneSizingData::zeroMemberData()
{
    if (!allocated(this->DOASSupMassFlowSeq)) return;
    std::fill(this->DOASSupMassFlowSeq.begin(), this->DOASSupMassFlowSeq.end(), 0.0);
    std::fill(this->DOASHeatLoadSeq.begin(), this->DOASHeatLoadSeq.end(), 0.0);
    std::fill(this->DOASCoolLoadSeq.begin(), this->DOASCoolLoadSeq.end(), 0.0);
    std::fill(this->DOASHeatAddSeq.begin(), this->DOASHeatAddSeq.end(), 0.0);
    std::fill(this->DOASLatAddSeq.begin(), this->DOASLatAddSeq.end(), 0.0);
    std::fill(this->DOASSupTempSeq.begin(), this->DOASSupTempSeq.end(), 0.0);
    std::fill(this->DOASSupHumRatSeq.begin(), this->DOASSupHumRatSeq.end(), 0.0);
    std::fill(this->DOASTotCoolLoadSeq.begin(), this->DOASTotCoolLoadSeq.end(), 0.0);
    std::fill(this->HeatFlowSeq.begin(), this->HeatFlowSeq.end(), 0.0);
    std::fill(this->HeatFlowSeqNoOA.begin(), this->HeatFlowSeqNoOA.end(), 0.0);
    std::fill(this->HeatLoadSeq.begin(), this->HeatLoadSeq.end(), 0.0);
    std::fill(this->HeatZoneTempSeq.begin(), this->HeatZoneTempSeq.end(), 0.0);
    std::fill(this->DesHeatSetPtSeq.begin(), this->DesHeatSetPtSeq.end(), 0.0);
    std::fill(this->HeatOutTempSeq.begin(), this->HeatOutTempSeq.end(), 0.0);
    std::fill(this->HeatZoneRetTempSeq.begin(), this->HeatZoneRetTempSeq.end(), 0.0);
    std::fill(this->HeatTstatTempSeq.begin(), this->HeatTstatTempSeq.end(), 0.0);
    std::fill(this->HeatZoneHumRatSeq.begin(), this->HeatZoneHumRatSeq.end(), 0.0);
    std::fill(this->HeatOutHumRatSeq.begin(), this->HeatOutHumRatSeq.end(), 0.0);
    std::fill(this->CoolFlowSeq.begin(), this->CoolFlowSeq.end(), 0.0);
    std::fill(this->CoolFlowSeqNoOA.begin(), this->CoolFlowSeqNoOA.end(), 0.0);
    std::fill(this->CoolLoadSeq.begin(), this->CoolLoadSeq.end(), 0.0);
    std::fill(this->CoolZoneTempSeq.begin(), this->CoolZoneTempSeq.end(), 0.0);
    std::fill(this->DesCoolSetPtSeq.begin(), this->DesCoolSetPtSeq.end(), 0.0);
    std::fill(this->CoolOutTempSeq.begin(), this->CoolOutTempSeq.end(), 0.0);
    std::fill(this->CoolZoneRetTempSeq.begin(), this->CoolZoneRetTempSeq.end(), 0.0);
    std::fill(this->CoolTstatTempSeq.begin(), this->CoolTstatTempSeq.end(), 0.0);
    std::fill(this->CoolZoneHumRatSeq.begin(), this->CoolZoneHumRatSeq.end(), 0.0);
    std::fill(this->CoolOutHumRatSeq.begin(), this->CoolOutHumRatSeq.end(), 0.0);

    this->CoolDesDay = ""; // name of a cooling design day
    this->HeatDesDay = ""; // name of a heating design day

    this->DesHeatMassFlow = 0.0;       // zone design heating air mass flow rate [kg/s]
    this->DesCoolMassFlow = 0.0;       // zone design cooling air mass flow rate [kg/s]
    this->DesHeatLoad = 0.0;           // zone design heating load [W]
    this->DesCoolLoad = 0.0;           // zone design cooling load [W]
    this->DesHeatDens = 0.0;           // zone design heating air density [kg/m3]
    this->DesCoolDens = 0.0;           // zone design cooling air density [kg/m3]
    this->DesHeatVolFlow = 0.0;        // zone design heating air volume flow rate [m3/s]
    this->DesCoolVolFlow = 0.0;        // zone design cooling air volume flow rate [m3/s]
    this->DesHeatVolFlowMax = 0.0;     // zone design heating maximum air volume flow rate [m3/s]
    this->DesCoolVolFlowMin = 0.0;     // zone design cooling minimum air volume flow rate [m3/s]
    this->DesHeatCoilInTemp = 0.0;     // zone heating coil design air inlet temperature [C]
    this->DesCoolCoilInTemp = 0.0;     // zone cooling coil design air inlet temperature [C]
    this->DesHeatCoilInHumRat = 0.0;   // zone heating coil design air inlet humidity ratio [kg/kg]
    this->DesCoolCoilInHumRat = 0.0;   // zone cooling coil design air inlet humidity ratio [kg/kg]
    this->DesHeatCoilInTempTU = 0.0;   // zone heating coil design air inlet temperature (supply air)([C]
    this->DesCoolCoilInTempTU = 0.0;   // zone cooling coil design air inlet temperature (supply air)[C]
    this->DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
    this->DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
    this->HeatMassFlow = 0.0;          // current zone heating air mass flow rate (HVAC time step)
    this->CoolMassFlow = 0.0;          // current zone cooling air mass flow rate (HVAC time step)
    this->HeatLoad = 0.0;              // current zone heating load (HVAC time step)
    this->CoolLoad = 0.0;              // current zone heating load (HVAC time step)
    this->HeatZoneTemp = 0.0;          // current zone temperature (heating, time step)
    this->HeatOutTemp = 0.0;           // current outdoor temperature (heating, time step)
    this->HeatZoneRetTemp = 0.0;       // current zone return temperature (heating, time step)
    this->HeatTstatTemp = 0.0;         // current zone thermostat temperature (heating, time step)
    this->CoolZoneTemp = 0.0;          // current zone temperature (cooling, time step)
    this->CoolOutTemp = 0.0;           // current Outdoor temperature (cooling, time step)
    this->CoolZoneRetTemp = 0.0;       // current zone return temperature (cooling, time step)
    this->CoolTstatTemp = 0.0;         // current zone thermostat temperature (cooling, time step)
    this->HeatZoneHumRat = 0.0;        // current zone humidity ratio (heating, time step)
    this->CoolZoneHumRat = 0.0;        // current zone humidity ratio (cooling, time step)
    this->HeatOutHumRat = 0.0;         // current outdoor humidity ratio (heating, time step)
    this->CoolOutHumRat = 0.0;         // current outdoor humidity ratio (cooling, time step)
    this->ZoneTempAtHeatPeak = 0.0;    // zone temp at max heating [C]
    this->ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
    this->OutTempAtHeatPeak = 0.0;     // outdoor temperature at max heating [C]
    this->ZoneTempAtCoolPeak = 0.0;    // zone temp at max cooling [C]
    this->ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
    this->OutTempAtCoolPeak = 0.0;     // outdoor temperature at max cooling [C]
    this->ZoneHumRatAtHeatPeak = 0.0;  // zone humidity ratio at max heating [kg/kg]
    this->ZoneHumRatAtCoolPeak = 0.0;  // zone humidity ratio at max cooling [kg/kg]
    this->OutHumRatAtHeatPeak = 0.0;   // outdoor humidity at max heating [kg/kg]
    this->OutHumRatAtCoolPeak = 0.0;   // outdoor humidity at max cooling [kg/kg]
    this->TimeStepNumAtHeatMax = 0;    // time step number (in day) at Heating peak
    this->TimeStepNumAtCoolMax = 0;    // time step number (in day) at cooling peak
    this->HeatDDNum = 0;               // design day index of design day causing heating peak
    this->CoolDDNum = 0;               // design day index of design day causing heating peak
    this->cHeatDDDate = "";            // date of design day causing heating peak
    this->cCoolDDDate = "";            // date of design day causing cooling peak
    this->DOASHeatLoad = 0.0;          // current heating load from DOAS supply air [W]
    this->DOASCoolLoad = 0.0;          // current cooling load from DOAS supply air [W]
    this->DOASSupMassFlow = 0.0;       // current mass flow rate of DOAS supply air [kg/s]
    this->DOASSupTemp = 0.0;           // current DOAS supply air temperature [C]
    this->DOASSupHumRat = 0.0;         // current DOAS supply air humidity ratio [kgWater/kgDryAir]
    this->DOASTotCoolLoad = 0.0;       // current total cooling load imposed by DOAS supply air [W]
}

void ZoneSizingData::allocateMemberArrays(int const numOfTimeStepInDay)
{
    this->HeatFlowSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolFlowSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatFlowSeqNoOA.dimension(numOfTimeStepInDay, 0.0);
    this->CoolFlowSeqNoOA.dimension(numOfTimeStepInDay, 0.0);
    this->HeatLoadSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolLoadSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatZoneTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DesHeatSetPtSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolZoneTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DesCoolSetPtSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatOutTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolOutTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatZoneRetTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatTstatTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolZoneRetTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolTstatTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatZoneHumRatSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolZoneHumRatSeq.dimension(numOfTimeStepInDay, 0.0);
    this->HeatOutHumRatSeq.dimension(numOfTimeStepInDay, 0.0);
    this->CoolOutHumRatSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASHeatLoadSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASCoolLoadSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASHeatAddSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASLatAddSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASSupMassFlowSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASSupTempSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASSupHumRatSeq.dimension(numOfTimeStepInDay, 0.0);
    this->DOASTotCoolLoadSeq.dimension(numOfTimeStepInDay, 0.0);
}

void resetHVACSizingGlobals(EnergyPlusData &state,
                            int const curZoneEqNum,
                            int const curSysNum,
                            bool &firstPassFlag) // called in zone equipment Report function
{
    // reset Data globals so that previously set variables are not used in other equipment models
    state.dataSize->DataTotCapCurveIndex = 0;
    state.dataSize->DataPltSizCoolNum = 0;
    state.dataSize->DataPltSizHeatNum = 0;
    state.dataSize->DataWaterLoopNum = 0;
    state.dataSize->DataCoilNum = 0;
    state.dataSize->DataFanOpMode = 0;
    state.dataSize->DataCoilIsSuppHeater = false;
    state.dataSize->DataIsDXCoil = false;
    state.dataSize->DataAutosizable = true;
    state.dataSize->DataEMSOverrideON = false;
    state.dataSize->DataScalableSizingON = false;
    state.dataSize->DataScalableCapSizingON = false;
    state.dataSize->DataSysScalableFlowSizingON = false;
    state.dataSize->DataSysScalableCapSizingON = false;
    state.dataSize->DataDesAccountForFanHeat = true;

    state.dataSize->DataDesInletWaterTemp = 0.0;
    state.dataSize->DataDesInletAirHumRat = 0.0;
    state.dataSize->DataDesInletAirTemp = 0.0;
    state.dataSize->DataDesOutletAirTemp = 0.0;
    state.dataSize->DataDesOutletAirHumRat = 0.0;
    state.dataSize->DataCoolCoilCap = 0.0;
    state.dataSize->DataFlowUsedForSizing = 0.0;
    state.dataSize->DataAirFlowUsedForSizing = 0.0;
    state.dataSize->DataWaterFlowUsedForSizing = 0.0;
    state.dataSize->DataCapacityUsedForSizing = 0.0;
    state.dataSize->DataDesignCoilCapacity = 0.0;
    state.dataSize->DataHeatSizeRatio = 1.0;
    state.dataSize->DataEMSOverride = 0.0;
    state.dataSize->DataBypassFrac = 0.0;
    state.dataSize->DataFracOfAutosizedCoolingAirflow = 1.0;
    state.dataSize->DataFracOfAutosizedHeatingAirflow = 1.0;
    state.dataSize->DataFlowPerCoolingCapacity = 0.0;
    state.dataSize->DataFlowPerHeatingCapacity = 0.0;
    state.dataSize->DataFracOfAutosizedCoolingCapacity = 1.0;
    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
    state.dataSize->DataAutosizedCoolingCapacity = 0.0;
    state.dataSize->DataAutosizedHeatingCapacity = 0.0;
    state.dataSize->DataConstantUsedForSizing = 0.0;
    state.dataSize->DataFractionUsedForSizing = 0.0;
    state.dataSize->DataNonZoneNonAirloopValue = 0.0;
    state.dataSize->DataZoneNumber = 0;
    state.dataSize->DataFanEnumType = -1;
    state.dataSize->DataFanIndex = -1;
    state.dataSize->DataWaterCoilSizCoolDeltaT = 0.0;
    state.dataSize->DataWaterCoilSizHeatDeltaT = 0.0;
    state.dataSize->DataNomCapInpMeth = false;
    state.dataSize->DataFanPlacement = zoneFanPlacement::zoneFanPlaceNotSet;
    state.dataSize->DataDXSpeedNum = 0;
    state.dataSize->DataCoilSizingAirInTemp = 0.0;
    state.dataSize->DataCoilSizingAirInHumRat = 0.0;
    state.dataSize->DataCoilSizingAirOutTemp = 0.0;
    state.dataSize->DataCoilSizingAirOutHumRat = 0.0;

    // These zone specific sizing variables are set in zone equipment to use for sizing.
    // Reset to avoid chance that second zone equipment will size using these variables set by first zone equipment to be sized
    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
    auto &UnitarySysEqSizing(state.dataSize->UnitarySysEqSizing);
    if (curZoneEqNum > 0) {

        if (ZoneEqSizing.size() == 0) {
            firstPassFlag = false;
            return;
        }

        ZoneEqSizing(curZoneEqNum).AirFlow = false;
        ZoneEqSizing(curZoneEqNum).CoolingAirFlow = false;
        ZoneEqSizing(curZoneEqNum).HeatingAirFlow = false;
        ZoneEqSizing(curZoneEqNum).SystemAirFlow = false;
        ZoneEqSizing(curZoneEqNum).Capacity = false;
        ZoneEqSizing(curZoneEqNum).CoolingCapacity = false;
        ZoneEqSizing(curZoneEqNum).HeatingCapacity = false;
        ZoneEqSizing(curZoneEqNum).AirVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).MaxHWVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).MaxCWVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).OAVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).DesCoolingLoad = 0.0;
        ZoneEqSizing(curZoneEqNum).DesHeatingLoad = 0.0;
        ZoneEqSizing(curZoneEqNum).CoolingAirVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).HeatingAirVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).SystemAirVolFlow = 0.0;
        ZoneEqSizing(curZoneEqNum).DesignSizeFromParent = false;
    }

    if (curSysNum > 0) {

        if (UnitarySysEqSizing.size() == 0) {
            firstPassFlag = false;
            return;
        }

        UnitarySysEqSizing(curSysNum).AirFlow = false;
        UnitarySysEqSizing(curSysNum).CoolingAirFlow = false;
        UnitarySysEqSizing(curSysNum).HeatingAirFlow = false;
        UnitarySysEqSizing(curSysNum).Capacity = false;
        UnitarySysEqSizing(curSysNum).CoolingCapacity = false;
        UnitarySysEqSizing(curSysNum).HeatingCapacity = false;
    }

    firstPassFlag = false;
}

void GetCoilDesFlowT(EnergyPlusData &state,
                     int SysNum,           // central air system index
                     Real64 CpAir,         // specific heat to be used in calculations [J/kgC]
                     Real64 &DesFlow,      // returned design mass flow [kg/s]
                     Real64 &DesExitTemp,  // returned design coil exit temperature [kg/s]
                     Real64 &DesExitHumRat // returned design coil exit humidity ratio [kg/kg]
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   September 2014
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function calculates the coil design air flow rate and exit temperature depending on the
    // cooling capacity control method

    // METHODOLOGY EMPLOYED:
    // energy and mass flow balance

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int DDAtSensPeak(0);
    int TimeStepAtSensPeak(0);
    int DDAtFlowPeak(0);
    int TimeStepAtFlowPeak(0);
    int CoolCapCtrl; // type of coil capacity control
    int PeakLoadType;
    int DDAtTotPeak(0);
    int TimeStepAtTotPeak(0);
    int TimeStepAtPeak(0);
    Real64 ZoneCoolLoadSum(0); // sum of zone cooling loads at the peak [W]
    Real64 AvgZoneTemp(0);     // average zone temperature [C]
    Real64 AvgSupTemp(0.0);    // average supply temperature for bypass control [C]
    Real64 TotFlow(0.0);       // total flow for bypass control [m3/s]
    Real64 MixTemp(0.0);       // mixed air temperature at the peak [C]

    auto &SysSizInput(state.dataSize->SysSizInput);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &SysSizPeakDDNum(state.dataSize->SysSizPeakDDNum);
    auto &CalcSysSizing(state.dataSize->CalcSysSizing);

    CoolCapCtrl = SysSizInput(SysNum).CoolCapControl;
    PeakLoadType = SysSizInput(SysNum).CoolingPeakLoadType;
    DDAtSensPeak = SysSizPeakDDNum(SysNum).SensCoolPeakDD;
    if (DDAtSensPeak > 0) {
        TimeStepAtSensPeak = SysSizPeakDDNum(SysNum).TimeStepAtSensCoolPk(DDAtSensPeak);
        DDAtFlowPeak = SysSizPeakDDNum(SysNum).CoolFlowPeakDD;
        TimeStepAtFlowPeak = SysSizPeakDDNum(SysNum).TimeStepAtCoolFlowPk(DDAtFlowPeak);
        DDAtTotPeak = SysSizPeakDDNum(SysNum).TotCoolPeakDD;
        TimeStepAtTotPeak = SysSizPeakDDNum(SysNum).TimeStepAtTotCoolPk(DDAtTotPeak);

        if (PeakLoadType == TotalCoolingLoad) {
            TimeStepAtPeak = TimeStepAtTotPeak;
        } else {
            TimeStepAtPeak = TimeStepAtSensPeak;
        }
    } else {
        if ((CoolCapCtrl == VT) || (CoolCapCtrl == Bypass)) {
            ShowWarningError(state,
                             "GetCoilDesFlow: AirLoopHVAC=" + SysSizInput(SysNum).AirPriLoopName + "has no time of peak cooling load for sizing.");
            ShowContinueError(state, "Using Central Cooling Capacity Control Method=VAV instead of Bypass or VT.");
            CoolCapCtrl = VAV;
        }
    }

    if (CoolCapCtrl == VAV) {
        DesExitTemp = FinalSysSizing(SysNum).CoolSupTemp;
        DesFlow = FinalSysSizing(SysNum).MassFlowAtCoolPeak / state.dataEnvrn->StdRhoAir;
        DesExitHumRat = FinalSysSizing(SysNum).CoolSupHumRat;
    } else if (CoolCapCtrl == OnOff) {
        DesExitTemp = FinalSysSizing(SysNum).CoolSupTemp;
        DesFlow = state.dataSize->DataAirFlowUsedForSizing;
        DesExitHumRat = FinalSysSizing(SysNum).CoolSupHumRat;
    } else if (CoolCapCtrl == VT) {
        if (FinalSysSizing(SysNum).CoolingPeakLoadType == SensibleCoolingLoad) {
            ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
            AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
        } else if (FinalSysSizing(SysNum).CoolingPeakLoadType == TotalCoolingLoad) {
            ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
            AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
        }
        DesExitTemp = max(FinalSysSizing(SysNum).CoolSupTemp,
                          AvgZoneTemp - ZoneCoolLoadSum / (state.dataEnvrn->StdRhoAir * CpAir * FinalSysSizing(SysNum).DesCoolVolFlow));
        DesFlow = FinalSysSizing(SysNum).DesCoolVolFlow;
        DesExitHumRat = Psychrometrics::PsyWFnTdbRhPb(state, DesExitTemp, 0.9, state.dataEnvrn->StdBaroPress, "GetCoilDesFlowT");
    } else if (CoolCapCtrl == Bypass) {
        if (FinalSysSizing(SysNum).CoolingPeakLoadType == SensibleCoolingLoad) {
            ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
            AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
        } else if (FinalSysSizing(SysNum).CoolingPeakLoadType == TotalCoolingLoad) {
            ZoneCoolLoadSum = CalcSysSizing(SysNum).SumZoneCoolLoadSeq(TimeStepAtPeak);
            AvgZoneTemp = CalcSysSizing(SysNum).CoolZoneAvgTempSeq(TimeStepAtPeak);
        }
        AvgSupTemp = AvgZoneTemp - ZoneCoolLoadSum / (state.dataEnvrn->StdRhoAir * CpAir * FinalSysSizing(SysNum).DesCoolVolFlow);
        TotFlow = FinalSysSizing(SysNum).DesCoolVolFlow;
        MixTemp = CalcSysSizing(SysNum).MixTempAtCoolPeak;
        DesExitTemp = FinalSysSizing(SysNum).CoolSupTemp;
        if (MixTemp > DesExitTemp) {
            DesFlow = TotFlow * max(0.0, min(1.0, ((MixTemp - AvgSupTemp) / (MixTemp - DesExitTemp))));
        } else {
            DesFlow = TotFlow;
        }
        DesExitHumRat = Psychrometrics::PsyWFnTdbRhPb(state, DesExitTemp, 0.9, state.dataEnvrn->StdBaroPress, "GetCoilDesFlowT");
    }
}

Real64 ZoneAirDistributionData::calculateEz(EnergyPlusData &state, int const ZoneNum) // Zone index
{
    Real64 zoneEz = 1.0;
    // Calc the zone supplied OA flow rate counting the zone air distribution effectiveness
    //  First check whether the zone air distribution effectiveness schedule exists, if yes uses it;
    //   otherwise uses the inputs of zone distribution effectiveness in cooling mode or heating mode
    if (this->ZoneADEffSchPtr > 0) {
        // Get schedule value for the zone air distribution effectiveness
        zoneEz = ScheduleManager::GetCurrentScheduleValue(state, this->ZoneADEffSchPtr);
    } else {
        Real64 zoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired;

        // Zone in cooling mode
        if (zoneLoad < 0.0) zoneEz = this->ZoneADEffCooling;

        // Zone in heating mode
        if (zoneLoad > 0.0) zoneEz = this->ZoneADEffHeating;
    }
    if (zoneEz <= 0.0) {
        // Enforce defaults
        zoneEz = 1.0;
    }
    return zoneEz;
}

} // namespace EnergyPlus::DataSizing
