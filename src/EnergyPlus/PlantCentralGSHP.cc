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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::PlantCentralGSHP {

// MODULE INFORMATION:
//       AUTHOR         PNNL
//       DATE WRITTEN   Feb 2013
//       MODIFIED       na
//       RE-ENGINEERED  na
// PURPOSE OF THIS MODULE:
// This module simulates the performance of the Central Plant GSHP systems
// It currently includes one object: ChillerHeaterPerformance:Electric:EIR.
// The other object available for this central CGSHP system such as HeatPumpPerformance:WaterToWater:EIR
//      will be implemented later.

// METHODOLOGY EMPLOYED:
//  Once the PlantLoopManager determines that the Central Plant GSHP
//  is available to meet a loop cooling and heating demands, it calls simulate
//  which in turn calls the electric PlantCentralGSHP model. The PlantCentralGSHP model is based on
//  polynomial fits of chiller/heater or heat pump performance data.

PlantComponent *WrapperSpecs::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data
    if (state.dataPlantCentralGSHP->getWrapperInputFlag) {
        GetWrapperInput(state);
        state.dataPlantCentralGSHP->getWrapperInputFlag = false;
    }

    // Now look for this particular object
    for (auto &thisWrapper : state.dataPlantCentralGSHP->Wrapper) {
        if (thisWrapper.Name == objectName) {
            return &thisWrapper;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "LocalPlantCentralGSHPFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void WrapperSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
{
    this->initialize(state, 0.0, calledFromLocation.loopNum);
    this->SizeWrapper(state);
}

void WrapperSpecs::getDesignCapacities(
    [[maybe_unused]] EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    MinLoad = 0.0;
    MaxLoad = 0.0;
    OptLoad = 0.0;
    if (calledFromLocation.loopNum == this->CWLoopNum) {   // Chilled water loop
        if (this->ControlMode == iCondType::SmartMixing) { // control mode is SmartMixing
            for (int NumChillerHeater = 1; NumChillerHeater <= this->ChillerHeaterNums; ++NumChillerHeater) {
                MaxLoad += this->ChillerHeater(NumChillerHeater).RefCapCooling * this->ChillerHeater(NumChillerHeater).MaxPartLoadRatCooling;
                OptLoad += this->ChillerHeater(NumChillerHeater).RefCapCooling * this->ChillerHeater(NumChillerHeater).OptPartLoadRatCooling;
                MinLoad += this->ChillerHeater(NumChillerHeater).RefCapCooling * this->ChillerHeater(NumChillerHeater).MinPartLoadRatCooling;
            }
        }
    } else if (calledFromLocation.loopNum == this->HWLoopNum) { // Hot water loop
        if (this->ControlMode == iCondType::SmartMixing) {      // control mode is SmartMixing
            for (int NumChillerHeater = 1; NumChillerHeater <= this->ChillerHeaterNums; ++NumChillerHeater) {
                MaxLoad += this->ChillerHeater(NumChillerHeater).RefCapClgHtg * this->ChillerHeater(NumChillerHeater).MaxPartLoadRatClgHtg;
                OptLoad += this->ChillerHeater(NumChillerHeater).RefCapClgHtg * this->ChillerHeater(NumChillerHeater).OptPartLoadRatClgHtg;
                MinLoad += this->ChillerHeater(NumChillerHeater).RefCapClgHtg * this->ChillerHeater(NumChillerHeater).MinPartLoadRatClgHtg;
            }
        } // End of control mode determination
    }
}

void WrapperSpecs::getSizingFactor(Real64 &SizFac)
{
    SizFac = 1.0;
}

void WrapperSpecs::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, [[maybe_unused]] bool RunFlag)
{
    if (calledFromLocation.loopNum != this->GLHELoopNum) {

        this->initialize(state, CurLoad, calledFromLocation.loopNum);
        this->CalcWrapperModel(state, CurLoad, calledFromLocation.loopNum);

    } else if (calledFromLocation.loopNum == this->GLHELoopNum) {
        PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                            calledFromLocation.loopNum,
                                                            this->GLHELoopSideNum,
                                                            DataPlant::TypeOf_CentralGroundSourceHeatPump,
                                                            this->GLHEInletNodeNum,
                                                            this->GLHEOutletNodeNum,
                                                            this->Report.GLHERate,
                                                            this->Report.GLHEInletTemp,
                                                            this->Report.GLHEOutletTemp,
                                                            this->Report.GLHEmdot,
                                                            FirstHVACIteration);

        // Use the first chiller heater's evaporator capacity ratio to determine dominant load
        this->SimulClgDominant = false;
        this->SimulHtgDominant = false;
        if (this->WrapperCoolingLoad > 0 && this->WrapperHeatingLoad > 0) {
            Real64 SimulLoadRatio = this->WrapperCoolingLoad / this->WrapperHeatingLoad;
            if (SimulLoadRatio > this->ChillerHeater(1).ClgHtgToCoolingCapRatio) {
                this->SimulClgDominant = true;
                this->SimulHtgDominant = false;
            } else {
                this->SimulHtgDominant = true;
                this->SimulClgDominant = false;
            }
        }
    }
}

void WrapperSpecs::SizeWrapper(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Yunzhi Huang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine is for sizing all the components under each 'CentralHeatPumpSystem' object,
    //  for which capacities and flow rates have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    //  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
    //  the evaporator (or load side) flow rate and the chilled water loop design delta T. The condenser
    //  flow (or source side) rate is calculated from the reference capacity, the COP, and the condenser
    //  loop design delta T.

    static constexpr std::string_view RoutineName("SizeCGSHPChillerHeater");

    bool ErrorsFound; // If errors detected in input

    // auto-size the chiller heater components
    if (this->ControlMode == iCondType::SmartMixing) {

        for (int NumChillerHeater = 1; NumChillerHeater <= this->ChillerHeaterNums; ++NumChillerHeater) {
            ErrorsFound = false;

            // find the appropriate Plant Sizing object
            int PltSizNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

            // if ( Wrapper( WrapperNum ).ChillerHeater( NumChillerHeater ).CondVolFlowRate == AutoSize ) {
            int PltSizCondNum = state.dataPlnt->PlantLoop(this->GLHELoopNum).PlantSizNum;
            //}

            Real64 tmpNomCap = this->ChillerHeater(NumChillerHeater).RefCapCooling;
            Real64 tmpEvapVolFlowRate = this->ChillerHeater(NumChillerHeater).EvapVolFlowRate;
            Real64 tmpCondVolFlowRate = this->ChillerHeater(NumChillerHeater).CondVolFlowRate;

            // auto-size the Evaporator Flow Rate
            if (PltSizNum > 0) {
                if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->ChillerHeater(NumChillerHeater).SizFac;
                    this->ChillerHeater(NumChillerHeater).tmpEvapVolFlowRate = tmpEvapVolFlowRate;
                    if (!this->ChillerHeater(NumChillerHeater).EvapVolFlowRateWasAutoSized)
                        tmpEvapVolFlowRate = this->ChillerHeater(NumChillerHeater).EvapVolFlowRate;

                } else {
                    if (this->ChillerHeater(NumChillerHeater).EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
                    this->ChillerHeater(NumChillerHeater).tmpEvapVolFlowRate = tmpEvapVolFlowRate;
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (this->ChillerHeater(NumChillerHeater).EvapVolFlowRateWasAutoSized) {
                        this->ChillerHeater(NumChillerHeater).EvapVolFlowRate = tmpEvapVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Initial Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate);
                        }
                    } else {
                        if (this->ChillerHeater(NumChillerHeater).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 &&
                            state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {

                            // Hardsized evaporator design volume flow rate for reporting
                            Real64 EvapVolFlowRateUser = this->ChillerHeater(NumChillerHeater).EvapVolFlowRate;
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate,
                                                         "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                                         EvapVolFlowRateUser);
                            tmpEvapVolFlowRate = EvapVolFlowRateUser;
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeChillerHeaterPerformanceElectricEIR: Potential issue with equipment sizing for " +
                                                    this->ChillerHeater(NumChillerHeater).Name);
                                    ShowContinueError(
                                        state, format("User-Specified Reference Chilled Water Flow Rate of {:.5R} [m3/s]", EvapVolFlowRateUser));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Reference Chilled Water Flow Rate of {:.5R} [m3/s]", tmpEvapVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            } else {
                if (this->ChillerHeater(NumChillerHeater).EvapVolFlowRateWasAutoSized) {
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        ShowSevereError(state, "Autosizing of CGSHP Chiller Heater evap flow rate requires a loop Sizing:Plant object");
                        ShowContinueError(state, "Occurs in CGSHP Chiller Heater Performance object=" + this->ChillerHeater(NumChillerHeater).Name);
                        ErrorsFound = true;
                    }
                } else {
                    if (this->ChillerHeater(NumChillerHeater).EvapVolFlowRate > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport &&
                        !this->mySizesReported) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     this->ChillerHeater(NumChillerHeater).Name,
                                                     "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                                     this->ChillerHeater(NumChillerHeater).EvapVolFlowRate);
                    }
                }
            }

            // auto-size the Reference Cooling Capacity
            // each individual chiller heater module is sized to be capable of supporting the total load on the wrapper
            if (PltSizNum > 0) {
                if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpEvapVolFlowRate > 0.0) {
                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                       DataGlobalConstants::CWInitConvTemp,
                                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                       RoutineName);

                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
                    tmpNomCap = Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * tmpEvapVolFlowRate;
                    if (!this->ChillerHeater(NumChillerHeater).RefCapCoolingWasAutoSized)
                        tmpNomCap = this->ChillerHeater(NumChillerHeater).RefCapCooling;
                } else {
                    if (this->ChillerHeater(NumChillerHeater).RefCapCoolingWasAutoSized) tmpNomCap = 0.0;
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (this->ChillerHeater(NumChillerHeater).RefCapCoolingWasAutoSized) {
                        this->ChillerHeater(NumChillerHeater).RefCapCooling = tmpNomCap;

                        // Now that we have the Reference Cooling Capacity, we need to also initialize the Heating side
                        // given the ratios
                        this->ChillerHeater(NumChillerHeater).RefCapClgHtg =
                            this->ChillerHeater(NumChillerHeater).RefCapCooling * this->ChillerHeater(NumChillerHeater).ClgHtgToCoolingCapRatio;

                        this->ChillerHeater(NumChillerHeater).RefPowerClgHtg =
                            (this->ChillerHeater(NumChillerHeater).RefCapCooling / this->ChillerHeater(NumChillerHeater).RefCOPCooling) *
                            this->ChillerHeater(NumChillerHeater).ClgHtgtoCogPowerRatio;

                        this->ChillerHeater(NumChillerHeater).RefCOPClgHtg =
                            this->ChillerHeater(NumChillerHeater).RefCapClgHtg / this->ChillerHeater(NumChillerHeater).RefPowerClgHtg;

                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Design Size Reference Capacity [W]",
                                                         tmpNomCap);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Initial Design Size Reference Capacity [W]",
                                                         tmpNomCap);
                        }
                    } else {
                        if (this->ChillerHeater(NumChillerHeater).RefCapCooling > 0.0 && tmpNomCap > 0.0 &&
                            state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {

                            // Hardsized nominal capacity cooling power for reporting
                            Real64 NomCapUser = this->ChillerHeater(NumChillerHeater).RefCapCooling;
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Design Size Reference Capacity [W]",
                                                         tmpNomCap,
                                                         "User-Specified Reference Capacity [W]",
                                                         NomCapUser);
                            tmpNomCap = NomCapUser;
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeChillerHeaterPerformanceElectricEIR: Potential issue with equipment sizing for " +
                                                    this->ChillerHeater(NumChillerHeater).Name);
                                    ShowContinueError(state, format("User-Specified Reference Capacity of {:.2R} [W]", NomCapUser));
                                    ShowContinueError(state, format("differs from Design Size Reference Capacity of {:.2R} [W]", tmpNomCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            } else {
                if (this->ChillerHeater(NumChillerHeater).RefCapCoolingWasAutoSized) {
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        ShowSevereError(state,
                                        "Size ChillerHeaterPerformance:Electric:EIR=\"" + this->ChillerHeater(NumChillerHeater).Name +
                                            "\", autosize error.");
                        ShowContinueError(state, "Autosizing of CGSHP Chiller Heater reference capacity requires");
                        ShowContinueError(state, "a cooling loop Sizing:Plant object.");
                        ErrorsFound = true;
                    }
                } else {
                    if (this->ChillerHeater(NumChillerHeater).RefCapCooling > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport &&
                        !this->mySizesReported) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     this->ChillerHeater(NumChillerHeater).Name,
                                                     "User-Specified Reference Capacity [W]",
                                                     this->ChillerHeater(NumChillerHeater).RefCapCooling);
                    }
                }
            }

            // auto-size the condenser volume flow rate
            // each individual chiller heater module is sized to be capable of supporting the total load on the wrapper
            if (PltSizCondNum > 0) {
                if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->GLHELoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->GLHELoopNum).FluidIndex,
                                                                   RoutineName);
                    // TODO: JM 2018-12-06 I wonder why Cp isn't calculated at the same temp as rho...
                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       state.dataPlnt->PlantLoop(this->GLHELoopNum).FluidName,
                                                                       this->ChillerHeater(NumChillerHeater).TempRefCondInCooling,
                                                                       state.dataPlnt->PlantLoop(this->GLHELoopNum).FluidIndex,
                                                                       RoutineName);
                    tmpCondVolFlowRate =
                        tmpNomCap *
                        (1.0 + (1.0 / this->ChillerHeater(NumChillerHeater).RefCOPCooling) * this->ChillerHeater(NumChillerHeater).OpenMotorEff) /
                        (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                    this->ChillerHeater(NumChillerHeater).tmpCondVolFlowRate = tmpCondVolFlowRate;
                    if (!this->ChillerHeater(NumChillerHeater).CondVolFlowRateWasAutoSized)
                        tmpCondVolFlowRate = this->ChillerHeater(NumChillerHeater).CondVolFlowRate;

                } else {
                    if (this->ChillerHeater(NumChillerHeater).CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
                    this->ChillerHeater(NumChillerHeater).tmpCondVolFlowRate = tmpCondVolFlowRate;
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (this->ChillerHeater(NumChillerHeater).CondVolFlowRateWasAutoSized) {
                        this->ChillerHeater(NumChillerHeater).CondVolFlowRate = tmpCondVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Initial Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate);
                        }
                    } else {
                        if (this->ChillerHeater(NumChillerHeater).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 &&
                            state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {

                            // Hardsized condenser design volume flow rate for reporting
                            Real64 CondVolFlowRateUser = this->ChillerHeater(NumChillerHeater).CondVolFlowRate;
                            BaseSizer::reportSizerOutput(state,
                                                         "ChillerHeaterPerformance:Electric:EIR",
                                                         this->ChillerHeater(NumChillerHeater).Name,
                                                         "Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate,
                                                         "User-Specified Reference Condenser Water Flow Rate [m3/s]",
                                                         CondVolFlowRateUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeChillerHeaterPerformanceElectricEIR: Potential issue with equipment sizing for " +
                                                    this->ChillerHeater(NumChillerHeater).Name);
                                    ShowContinueError(
                                        state, format("User-Specified Reference Condenser Water Flow Rate of {:.5R} [m3/s]", CondVolFlowRateUser));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Reference Condenser Water Flow Rate of {:.5R} [m3/s]", tmpCondVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            } else {
                if (this->ChillerHeater(NumChillerHeater).CondVolFlowRateWasAutoSized) {
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        ShowSevereError(state,
                                        "Size ChillerHeaterPerformance:Electric:EIR=\"" + this->ChillerHeater(NumChillerHeater).Name +
                                            "\", autosize error.");
                        ShowContinueError(state, "Autosizing of CGSHP Chiller Heater condenser flow rate requires");
                        ShowContinueError(state, "a condenser loop Sizing:Plant object.");
                        ErrorsFound = true;
                    }
                } else {
                    if (this->ChillerHeater(NumChillerHeater).CondVolFlowRate > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport &&
                        !this->mySizesReported) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeaterPerformance:Electric:EIR",
                                                     this->ChillerHeater(NumChillerHeater).Name,
                                                     "User-Specified Reference Condenser Water Flow Rate [m3/s]",
                                                     this->ChillerHeater(NumChillerHeater).CondVolFlowRate);
                    }
                }
            }

            if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->mySizesReported) {
                // create predefined report
                std::string equipName = this->ChillerHeater(NumChillerHeater).Name;
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchMechType, equipName, "ChillerHeaterPerformance:Electric:EIR");
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchMechNomEff, equipName, this->ChillerHeater(NumChillerHeater).RefCOPCooling);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchMechNomCap, equipName, this->ChillerHeater(NumChillerHeater).RefCapCooling);
            }

            if (ErrorsFound) {
                ShowFatalError(state, "Preceding sizing errors cause program termination");
            }
        }

        // sum individual volume flows and register wrapper inlets
        Real64 TotalEvapVolFlowRate = 0.0;
        Real64 TotalCondVolFlowRate = 0.0;
        Real64 TotalHotWaterVolFlowRate = 0.0;
        for (int NumChillerHeater = 1; NumChillerHeater <= this->ChillerHeaterNums; ++NumChillerHeater) {
            TotalEvapVolFlowRate += this->ChillerHeater(NumChillerHeater).tmpEvapVolFlowRate;
            TotalCondVolFlowRate += this->ChillerHeater(NumChillerHeater).tmpCondVolFlowRate;
            TotalHotWaterVolFlowRate += this->ChillerHeater(NumChillerHeater).DesignHotWaterVolFlowRate;
        }

        PlantUtilities::RegisterPlantCompDesignFlow(state, this->CHWInletNodeNum, TotalEvapVolFlowRate);
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->HWInletNodeNum, TotalHotWaterVolFlowRate);
        // save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->GLHEInletNodeNum, TotalCondVolFlowRate);

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            this->mySizesReported = true;
        }

        return;
    }
}

void GetWrapperInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Yunzhi Huang and Daeho Kang, PNNL
    //       DATE WRITTEN:    Feb 2013

    // PURPOSE OF THIS SUBROUTINE:
    //  This routine will get the input required by the Wrapper model.

    bool ErrorsFound(false); // True when input errors are found
    int NumAlphas;           // Number of elements in the alpha array
    int NumNums;             // Number of elements in the numeric array
    int IOStat;              // IO Status when calling get input subroutine

    state.dataIPShortCut->cCurrentModuleObject = "CentralHeatPumpSystem";
    state.dataPlantCentralGSHP->numWrappers =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

    if (state.dataPlantCentralGSHP->numWrappers <= 0) {
        ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
    }

    state.dataPlantCentralGSHP->Wrapper.allocate(state.dataPlantCentralGSHP->numWrappers);

    // Load arrays with electric EIR chiller data
    for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 WrapperNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        state.dataPlantCentralGSHP->Wrapper(WrapperNum).Name = state.dataIPShortCut->cAlphaArgs(1);

        // initialize nth chiller heater index (including identical units) for current wrapper
        int NumChHtrPerWrapper = 0;
        if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound)) {
            continue;
        }

        if (state.dataIPShortCut->cAlphaArgs(2) == "SMARTMIXING") {
            state.dataPlantCentralGSHP->Wrapper(WrapperNum).ControlMode = iCondType::SmartMixing;
        }

        state.dataPlantCentralGSHP->Wrapper(WrapperNum).CHWInletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(3),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                NodeInputManager::compFluidStream::Primary,
                                                DataLoopNode::ObjectIsNotParent); // node name : connection should be careful!
        state.dataPlantCentralGSHP->Wrapper(WrapperNum).CHWOutletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(4),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                NodeInputManager::compFluidStream::Primary,
                                                DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state,
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(3),
                                           state.dataIPShortCut->cAlphaArgs(4),
                                           "Chilled Water Nodes");

        state.dataPlantCentralGSHP->Wrapper(WrapperNum).GLHEInletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(5),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                NodeInputManager::compFluidStream::Secondary,
                                                DataLoopNode::ObjectIsNotParent); // node name : connection should be careful!
        state.dataPlantCentralGSHP->Wrapper(WrapperNum).GLHEOutletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(6),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                NodeInputManager::compFluidStream::Secondary,
                                                DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state,
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(5),
                                           state.dataIPShortCut->cAlphaArgs(6),
                                           "GLHE Nodes");

        state.dataPlantCentralGSHP->Wrapper(WrapperNum).HWInletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(7),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                NodeInputManager::compFluidStream::Tertiary,
                                                DataLoopNode::ObjectIsNotParent); // node name : connection should be careful!
        state.dataPlantCentralGSHP->Wrapper(WrapperNum).HWOutletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(8),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                NodeInputManager::compFluidStream::Tertiary,
                                                DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state,
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(7),
                                           state.dataIPShortCut->cAlphaArgs(8),
                                           "Hot Water Nodes");

        state.dataPlantCentralGSHP->Wrapper(WrapperNum).AncillaryPower = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
            state.dataPlantCentralGSHP->Wrapper(WrapperNum).SchedPtr = 0;
        } else {
            state.dataPlantCentralGSHP->Wrapper(WrapperNum).SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));
        }

        int NumberOfComp = (NumAlphas - 9) / 3;
        state.dataPlantCentralGSHP->Wrapper(WrapperNum).NumOfComp = NumberOfComp;
        state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp.allocate(NumberOfComp);

        if (state.dataPlantCentralGSHP->Wrapper(WrapperNum).NumOfComp == 0) {
            ShowSevereError(state,
                            "GetWrapperInput: No component names on " + state.dataIPShortCut->cCurrentModuleObject + '=' +
                                state.dataPlantCentralGSHP->Wrapper(WrapperNum).Name);
            ErrorsFound = true;
        } else {
            int Comp = 0;
            for (int loop = 10; loop <= NumAlphas; loop += 3) {
                ++Comp;
                state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperPerformanceObjectType =
                    state.dataIPShortCut->cAlphaArgs(loop);
                state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperComponentName = state.dataIPShortCut->cAlphaArgs(loop + 1);
                if (state.dataIPShortCut->lAlphaFieldBlanks(loop + 2)) {
                    state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).CHSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).CHSchedPtr =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(loop + 2));
                }
                state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperIdenticalObjectNum =
                    state.dataIPShortCut->rNumericArgs(1 + Comp);
                if (state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperPerformanceObjectType ==
                    "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR") {

                    // count number of chiller heaters (including identical units) for current wrapper
                    if (state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperIdenticalObjectNum > 1) {
                        NumChHtrPerWrapper += state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperIdenticalObjectNum;
                    } else {
                        ++NumChHtrPerWrapper;
                    }

                    // count total number of chiller heaters (not including identical units) for ALL wrappers
                    ++state.dataPlantCentralGSHP->numChillerHeaters;
                }
            }

            state.dataPlantCentralGSHP->Wrapper(WrapperNum).ChillerHeaterNums = NumChHtrPerWrapper;
        }

        if (ErrorsFound) {
            ShowFatalError(state,
                           "GetWrapperInput: Invalid " + state.dataIPShortCut->cCurrentModuleObject +
                               " Input, preceding condition(s) cause termination.");
        }

        // ALLOCATE ARRAYS
        if ((state.dataPlantCentralGSHP->numChillerHeaters == 0) &&
            (state.dataPlantCentralGSHP->Wrapper(WrapperNum).ControlMode == iCondType::SmartMixing)) {
            ShowFatalError(state,
                           "SmartMixing Control Mode in object " + state.dataIPShortCut->cCurrentModuleObject + " : " +
                               state.dataPlantCentralGSHP->Wrapper(WrapperNum).Name +
                               " need to apply to ChillerHeaterPerformance:Electric:EIR object(s).");
        }
    }

    if (state.dataPlantCentralGSHP->numChillerHeaters > 0) {

        for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
            state.dataPlantCentralGSHP->Wrapper(WrapperNum).ChillerHeater.allocate(state.dataPlantCentralGSHP->Wrapper(WrapperNum).ChillerHeaterNums);
        }
        GetChillerHeaterInput(state);
    }

    for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
        int ChillerHeaterNum = 0; // initialize nth chiller heater index (including identical units) for current wrapper
        for (int Comp = 1; Comp <= state.dataPlantCentralGSHP->Wrapper(WrapperNum).NumOfComp; ++Comp) {
            if (state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperPerformanceObjectType ==
                "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR") {
                std::string CompName = state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperComponentName;
                int CompIndex = UtilityRoutines::FindItemInList(CompName, state.dataPlantCentralGSHP->ChillerHeater);
                // User may enter invalid name rather than selecting one from the object list
                if (CompIndex <= 0) {
                    ShowSevereError(state, "GetWrapperInput: Invalid Chiller Heater Modules Performance Component Name =" + std::string{CompName});
                    ShowContinueError(state, "Select the name of ChillerHeaterPerformance:Electric:EIR object(s) from the object list.");
                    ShowFatalError(state, "Program terminates due to preceding condition.");
                }
                state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperPerformanceObjectIndex = CompIndex;
                if (state.dataPlantCentralGSHP->ChillerHeater(CompIndex).VariableFlow) {
                    state.dataPlantCentralGSHP->Wrapper(WrapperNum).VariableFlowCH = true;
                }
                for (int i_CH = 1; i_CH <= state.dataPlantCentralGSHP->Wrapper(WrapperNum).WrapperComp(Comp).WrapperIdenticalObjectNum; ++i_CH) {
                    // increment nth chiller heater index (including identical units) for current wrapper
                    ++ChillerHeaterNum;
                    state.dataPlantCentralGSHP->Wrapper(WrapperNum).ChillerHeater(ChillerHeaterNum) =
                        state.dataPlantCentralGSHP->ChillerHeater(CompIndex);
                }
            }
        }
    }

    // Release memory from temporary arrays; values now copied into their associated Wrapper in above loop
    if (allocated(state.dataPlantCentralGSHP->ChillerHeater)) state.dataPlantCentralGSHP->ChillerHeater.deallocate();

    // Set up output variables
    for (int WrapperNum = 1; WrapperNum <= state.dataPlantCentralGSHP->numWrappers; ++WrapperNum) {
    } // End of wrapper count
}

void WrapperSpecs::setupOutputVars(EnergyPlusData &state)
{
    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->Report.TotElecCooling,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ELECTRICITY",
                        "Cooling",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->Report.TotElecHeating,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ELECTRICITY",
                        "Heating",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Electricity Rate",
                        OutputProcessor::Unit::W,
                        this->Report.TotElecCoolingPwr,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Electricity Rate",
                        OutputProcessor::Unit::W,
                        this->Report.TotElecHeatingPwr,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Energy",
                        OutputProcessor::Unit::J,
                        this->Report.CoolingEnergy,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ENERGYTRANSFER",
                        "CHILLERS",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Energy",
                        OutputProcessor::Unit::J,
                        this->Report.HeatingEnergy,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ENERGYTRANSFER",
                        "BOILER",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Heater System Source Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->Report.GLHEEnergy,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ENERGYTRANSFER",
                        "HEATREJECTION",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Rate",
                        OutputProcessor::Unit::W,
                        this->Report.CoolingRate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Rate",
                        OutputProcessor::Unit::W,
                        this->Report.HeatingRate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->Report.GLHERate,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->Report.CHWmdot,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->Report.HWmdot,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->Report.GLHEmdot,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.CHWInletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.HWInletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.GLHEInletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Cooling Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.CHWOutletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Heating Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.HWOutletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Heater System Source Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.GLHEOutletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    if (this->ChillerHeaterNums > 0) {

        for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {

            SetupOutputVariable(state,
                                format("Chiller Heater Operation Mode Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::None,
                                this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Part Load Ratio Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::None,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Cycling Ratio Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::None,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Cooling Electricity Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::W,
                                this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Heating Electricity Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::W,
                                this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Cooling Electricity Energy Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::J,
                                this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Heating Electricity Energy Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::J,
                                this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Cooling Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::W,
                                this->ChillerHeater(ChillerHeaterNum).Report.QEvap,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Cooling Energy Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::J,
                                this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater False Load Heat Transfer Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::W,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater False Load Heat Transfer Energy Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::J,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoad,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Evaporator Inlet Temperature Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::C,
                                this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Evaporator Outlet Temperature Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::C,
                                this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Evaporator Mass Flow Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::kg_s,
                                this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Condenser Heat Transfer Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::W,
                                this->ChillerHeater(ChillerHeaterNum).Report.QCond,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Condenser Heat Transfer Energy Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::J,
                                this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater COP Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::W_W,
                                this->ChillerHeater(ChillerHeaterNum).Report.ActualCOP,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Capacity Temperature Modifier Multiplier Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::None,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFT,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater EIR Temperature Modifier Multiplier Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::None,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFT,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater EIR Part Load Modifier Multiplier Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::None,
                                this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLR,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Condenser Inlet Temperature Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::C,
                                this->ChillerHeater(ChillerHeaterNum).Report.CondInletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Condenser Outlet Temperature Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::C,
                                this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);

            SetupOutputVariable(state,
                                format("Chiller Heater Condenser Mass Flow Rate Unit {}", ChillerHeaterNum),
                                OutputProcessor::Unit::kg_s,
                                this->ChillerHeater(ChillerHeaterNum).Report.Condmdot,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->ChillerHeater(ChillerHeaterNum).Name);
        } // End of individual chiller heater count for current wrapper

    } // End of individual chiller heater output
}

void GetChillerHeaterInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Kyung Tae Yun, Mississippi State University
    //       DATE WRITTEN:    Feb 2013

    // PURPOSE OF THIS SUBROUTINE:
    //  This routine will get the input required by the ChillerHeaterPerformance:Electric:EIR model.

    bool CHErrorsFound(false);         // True when input errors are found
    bool FoundNegValue(false);         // Used to evaluate PLFFPLR curve objects
    int NumAlphas;                     // Number of elements in the alpha array
    int NumNums;                       // Number of elements in the numeric array
    int IOStat;                        // IO Status when calling get input subroutine
    Array1D<Real64> CurveValArray(11); // Used to evaluate PLFFPLR curve objects

    state.dataIPShortCut->cCurrentModuleObject = "ChillerHeaterPerformance:Electric:EIR";
    state.dataPlantCentralGSHP->numChillerHeaters =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

    if (state.dataPlantCentralGSHP->numChillerHeaters <= 0) {
        ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
        CHErrorsFound = true;
    }

    // Allocate temporary ChillerHeater and ChillerHeaterReport arrays
    if (allocated(state.dataPlantCentralGSHP->ChillerHeater)) state.dataPlantCentralGSHP->ChillerHeater.deallocate();
    state.dataPlantCentralGSHP->ChillerHeater.allocate(state.dataPlantCentralGSHP->numChillerHeaters);

    // Load arrays with electric EIR chiller data
    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= state.dataPlantCentralGSHP->numChillerHeaters; ++ChillerHeaterNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 ChillerHeaterNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, CHErrorsFound);

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondModeCooling = state.dataIPShortCut->cAlphaArgs(4);

        // Performance curves
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX =
            CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX == 0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(5) + '=' + state.dataIPShortCut->cAlphaArgs(5));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX =
            CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(6));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX == 0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX =
            CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(7));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX == 0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondModeHeating = state.dataIPShortCut->cAlphaArgs(8);

        // Performance curves
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX =
            CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(9));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX == 0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + state.dataIPShortCut->cAlphaArgs(9));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX =
            CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(10));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX == 0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + state.dataIPShortCut->cAlphaArgs(10));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX =
            CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(11));
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX == 0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + state.dataIPShortCut->cAlphaArgs(11));
            CHErrorsFound = true;
        }

        if (state.dataIPShortCut->cAlphaArgs(2) == "CONSTANTFLOW") {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow = true;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).VariableFlow = false;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "VARIABLEFLOW") {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow = false;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).VariableFlow = true;
        } else { // Assume a constant flow chiller if none is specified
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow = true;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).VariableFlow = false;
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
            ShowContinueError(state, "simulation assumes CONSTANTFLOW and continues..");
        }

        if (ChillerHeaterNum > 1) {
            if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow !=
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum - 1).ConstantFlow) {
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ConstantFlow = true;
                ShowWarningError(state,
                                 "Water flow mode is different from the other chiller heater(s) " + state.dataIPShortCut->cCurrentModuleObject + '=' +
                                     state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Simulation assumes CONSTANTFLOW and continues..");
            }
        }

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "WaterCooled")) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondenserType = iCondType::WaterCooled;
        } else {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cAlphaFieldNames(3) + '=' + state.dataIPShortCut->cAlphaArgs(3));
            ShowContinueError(state, "Valid entries is WaterCooled");
            CHErrorsFound = true;
        }

        // Chiller rated performance data
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling == DataSizing::AutoSize) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCoolingWasAutoSized = true;
        }
        if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state,
                              format("Entered in {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
            CHErrorsFound = true;
        }
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPCooling = state.dataIPShortCut->rNumericArgs(2);
        if (state.dataIPShortCut->rNumericArgs(2) == 0.0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state,
                              format("Entered in {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling = state.dataIPShortCut->rNumericArgs(3);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInCooling = state.dataIPShortCut->rNumericArgs(4);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondOutCooling = state.dataIPShortCut->rNumericArgs(5);

        // Reference Heating Mode Ratios for Capacity and Power
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgToCoolingCapRatio = state.dataIPShortCut->rNumericArgs(6);
        if (state.dataIPShortCut->rNumericArgs(6) == 0.0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state,
                              format("Entered in {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(6), state.dataIPShortCut->rNumericArgs(6)));
            CHErrorsFound = true;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgtoCogPowerRatio = state.dataIPShortCut->rNumericArgs(7);
        if (state.dataIPShortCut->rNumericArgs(7) == 0.0) {
            ShowSevereError(state, "Invalid " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state,
                              format("Entered in {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(7), state.dataIPShortCut->rNumericArgs(7)));
            CHErrorsFound = true;
        }

        if (!state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCoolingWasAutoSized) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapClgHtg =
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgToCoolingCapRatio *
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefPowerClgHtg =
                (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapCooling /
                 state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPCooling) *
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ClgHtgtoCogPowerRatio;
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCOPClgHtg =
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefCapClgHtg /
                state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).RefPowerClgHtg;
        }

        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg = state.dataIPShortCut->rNumericArgs(8);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondOutClgHtg = state.dataIPShortCut->rNumericArgs(9);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInClgHtg = state.dataIPShortCut->rNumericArgs(10);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(11);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(12);
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate == DataSizing::AutoSize) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).EvapVolFlowRateWasAutoSized = true;
        }
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondVolFlowRate = state.dataIPShortCut->rNumericArgs(13);
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondVolFlowRate == DataSizing::AutoSize) {
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).CondVolFlowRateWasAutoSized = true;
        }
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).DesignHotWaterVolFlowRate = state.dataIPShortCut->rNumericArgs(14);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OpenMotorEff = state.dataIPShortCut->rNumericArgs(15);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OptPartLoadRatCooling = state.dataIPShortCut->rNumericArgs(16);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OptPartLoadRatClgHtg = state.dataIPShortCut->rNumericArgs(17);
        state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).SizFac = state.dataIPShortCut->rNumericArgs(18);

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).SizFac <= 0.0)
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).SizFac = 1.0;

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OpenMotorEff < 0.0 ||
            state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).OpenMotorEff > 1.0) {
            ShowSevereError(state, "GetCurveInput: For " + state.dataIPShortCut->cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, format("{} = {:.3R}", state.dataIPShortCut->cNumericFieldNames(14), state.dataIPShortCut->rNumericArgs(14)));
            ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(14) + " must be greater than or equal to zero");
            ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(14) + " must be less than or equal to one");
            CHErrorsFound = true;
        }

        // Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX > 0) {
            Real64 CurveVal = CurveManager::CurveValue(state,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInCooling);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Capacity ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  "(+ or - 10%) at reference conditions for " + state.dataIPShortCut->cCurrentModuleObject + "= " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, format("Curve output at reference conditions = {:.3T}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX > 0) {
            Real64 CurveVal = CurveManager::CurveValue(state,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInCooling);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  "(+ or - 10%) at reference conditions for " + state.dataIPShortCut->cCurrentModuleObject + "= " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, format("Curve output at reference conditions = {:.3T}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX > 0) {
            Real64 CurveVal =
                CurveManager::CurveValue(state, state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX, 1.0);

            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0");
                ShowContinueError(state,
                                  "(+ or - 10%) at reference conditions for " + state.dataIPShortCut->cCurrentModuleObject + "= " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, format("Curve output at reference conditions = {:.3T}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX > 0) {
            FoundNegValue = false;
            for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                Real64 CurveValTmp = CurveManager::CurveValue(
                    state, state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX, double(CurveCheck / 10.0));
                if (CurveValTmp < 0.0) FoundNegValue = true;
                CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
            }
            if (FoundNegValue) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve shows negative values ");
                ShowContinueError(state, "for " + state.dataIPShortCut->cCurrentModuleObject + "= " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "EIR as a function of PLR curve output at various part-load ratios shown below:");
                ShowContinueError(state, "PLR   =  0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");

                ShowContinueError(state, fmt::format("Curve Output = {:7.2F}", fmt::join(CurveValArray, ",")));

                CHErrorsFound = true;
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX > 0) {
            Real64 CurveVal = CurveManager::CurveValue(state,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInClgHtg);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Capacity ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  "(+ or - 10%) at reference conditions for " + state.dataIPShortCut->cCurrentModuleObject + "= " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, format("Curve output at reference conditions = {:.3T}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX > 0) {
            Real64 CurveVal = CurveManager::CurveValue(state,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg,
                                                       state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).TempRefCondInClgHtg);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of temperature curve output is not equal to 1.0");
                ShowContinueError(state,
                                  "(+ or - 10%) at reference conditions for " + state.dataIPShortCut->cCurrentModuleObject + "= " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, format("Curve output at reference conditions = {:.3T}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX > 0) {
            Real64 CurveVal =
                CurveManager::CurveValue(state, state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX, 1.0);

            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0");
                ShowContinueError(state,
                                  "(+ or - 10%) at reference conditions for " + state.dataIPShortCut->cCurrentModuleObject + "= " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, format("Curve output at reference conditions = {:.3T}", CurveVal));
            }
        }

        if (state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX > 0) {
            FoundNegValue = false;
            for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                Real64 CurveValTmp = CurveManager::CurveValue(
                    state, state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX, double(CurveCheck / 10.0));
                if (CurveValTmp < 0.0) FoundNegValue = true;
                CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
            }
            if (FoundNegValue) {
                ShowWarningError(state, "Energy input ratio as a function of part-load ratio curve shows negative values ");
                ShowContinueError(state, "for " + state.dataIPShortCut->cCurrentModuleObject + "= " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "EIR as a function of PLR curve output at various part-load ratios shown below:");
                ShowContinueError(state, "PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");

                const auto curve_output = fmt::format("Curve Output = {:7.2F}", fmt::join(CurveValArray, ","));
                ShowContinueError(state, curve_output);

                CHErrorsFound = true;
            }
        }

        CurveManager::GetCurveMinMaxValues(state,
                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX,
                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MinPartLoadRatClgHtg,
                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MaxPartLoadRatClgHtg);

        CurveManager::GetCurveMinMaxValues(state,
                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX,
                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MinPartLoadRatCooling,
                                           state.dataPlantCentralGSHP->ChillerHeater(ChillerHeaterNum).MaxPartLoadRatCooling);
    }

    if (CHErrorsFound) {
        ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
    }
}

void WrapperSpecs::initialize(EnergyPlusData &state,
                              Real64 MyLoad, // Demand Load
                              int LoopNum    // Loop Number Index
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine is for initializations of the CentralHeatPumpSystem variables

    // METHODOLOGY EMPLOYED:
    //  Uses the status flags to trigger initializations.

    static constexpr std::string_view RoutineName("InitCGSHPHeatPump");

    if (this->setupOutputVarsFlag) {
        this->setupOutputVars(state);
        this->setupOutputVarsFlag = false;
    }

    if (this->MyWrapperFlag) {
        // Locate the chillers on the plant loops for later usage
        bool errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_CentralGroundSourceHeatPump,
                                                this->CWLoopNum,
                                                this->CWLoopSideNum,
                                                this->CWBranchNum,
                                                this->CWCompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->CHWInletNodeNum,
                                                _);

        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_CentralGroundSourceHeatPump,
                                                this->HWLoopNum,
                                                this->HWLoopSideNum,
                                                this->HWBranchNum,
                                                this->HWCompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->HWInletNodeNum,
                                                _);

        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_CentralGroundSourceHeatPump,
                                                this->GLHELoopNum,
                                                this->GLHELoopSideNum,
                                                this->GLHEBranchNum,
                                                this->GLHECompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->GLHEInletNodeNum,
                                                _);

        PlantUtilities::InterConnectTwoPlantLoopSides(state,
                                                      this->CWLoopNum,
                                                      this->CWLoopSideNum,
                                                      this->GLHELoopNum,
                                                      this->GLHELoopSideNum,
                                                      DataPlant::TypeOf_CentralGroundSourceHeatPump,
                                                      true);

        PlantUtilities::InterConnectTwoPlantLoopSides(state,
                                                      this->HWLoopNum,
                                                      this->HWLoopSideNum,
                                                      this->GLHELoopNum,
                                                      this->GLHELoopSideNum,
                                                      DataPlant::TypeOf_CentralGroundSourceHeatPump,
                                                      true);

        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->CWLoopNum, this->CWLoopSideNum, this->HWLoopNum, this->HWLoopSideNum, DataPlant::TypeOf_CentralGroundSourceHeatPump, true);

        if (this->VariableFlowCH) {
            // Reset flow priority
            if (LoopNum == this->CWLoopNum) {
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            } else if (LoopNum == this->HWLoopNum) {
                state.dataPlnt->PlantLoop(this->HWLoopNum)
                    .LoopSide(this->HWLoopSideNum)
                    .Branch(this->HWBranchNum)
                    .Comp(this->HWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            // check if setpoint on outlet node - chilled water loop
            if (state.dataLoopNodes->Node(this->CHWOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) {
                if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    if (!this->CoolSetPointErrDone) {
                        ShowWarningError(state, "Missing temperature setpoint on cooling side for CentralHeatPumpSystem named " + this->Name);
                        ShowContinueError(state,
                                          "  A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem, use a SetpointManager");
                        ShowContinueError(state,
                                          "  The overall loop setpoint will be assumed for CentralHeatPumpSystem. The simulation continues ... ");
                        this->CoolSetPointErrDone = true;
                    }
                } else {
                    // need call to EMS to check node
                    bool FatalError = false; // but not really fatal yet, but should be.
                    EMSManager::CheckIfNodeSetPointManagedByEMS(
                        state, this->CHWOutletNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, FatalError);
                    state.dataLoopNodes->NodeSetpointCheck(this->CHWOutletNodeNum).needsSetpointChecking = false;
                    if (FatalError) {
                        if (!this->CoolSetPointErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint on cooling side for CentralHeatPumpSystem named " + this->Name);
                            ShowContinueError(state, "A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ");
                            ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at the chiller side outlet node ");
                            ShowContinueError(state, "or use an EMS actuator to establish a setpoint at the outlet node ");
                            ShowContinueError(state, "The overall loop setpoint will be assumed for chiller side. The simulation continues ... ");
                            this->CoolSetPointErrDone = true;
                        }
                    }
                }
                this->CoolSetPointSetToLoop = true;
                state.dataLoopNodes->Node(this->CHWOutletNodeNum).TempSetPoint =
                    state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            }

            if (state.dataLoopNodes->Node(this->HWOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) {
                if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    if (!this->HeatSetPointErrDone) {
                        ShowWarningError(state, "Missing temperature setpoint on heating side for CentralHeatPumpSystem named " + this->Name);
                        ShowContinueError(state,
                                          "  A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem, use a SetpointManager");
                        ShowContinueError(state,
                                          "  The overall loop setpoint will be assumed for CentralHeatPumpSystem. The simulation continues ... ");
                        this->HeatSetPointErrDone = true;
                    }
                } else {
                    // need call to EMS to check node
                    bool FatalError = false; // but not really fatal yet, but should be.
                    EMSManager::CheckIfNodeSetPointManagedByEMS(
                        state, this->HWOutletNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, FatalError);
                    state.dataLoopNodes->NodeSetpointCheck(this->HWOutletNodeNum).needsSetpointChecking = false;
                    if (FatalError) {
                        if (!this->HeatSetPointErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint on heating side for CentralHeatPumpSystem named " + this->Name);
                            ShowContinueError(state, "A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ");
                            ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at the chiller side outlet node ");
                            ShowContinueError(state, "or use an EMS actuator to establish a setpoint at the outlet node ");
                            ShowContinueError(state, "The overall loop setpoint will be assumed for chiller side. The simulation continues ... ");
                            this->HeatSetPointErrDone = true;
                        }
                    }
                }
                this->HeatSetPointSetToLoop = true;
                state.dataLoopNodes->Node(this->HWOutletNodeNum).TempSetPoint =
                    state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
            }
        }
        this->MyWrapperFlag = false;
    }

    if (this->MyWrapperEnvrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {

        if (this->ControlMode == iCondType::SmartMixing) {

            this->CHWVolFlowRate = 0.0;
            this->HWVolFlowRate = 0.0;
            this->GLHEVolFlowRate = 0.0;

            for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                this->CHWVolFlowRate += this->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate;
                this->HWVolFlowRate += this->ChillerHeater(ChillerHeaterNum).DesignHotWaterVolFlowRate;
                this->GLHEVolFlowRate += this->ChillerHeater(ChillerHeaterNum).CondVolFlowRate;
            }

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

            this->CHWMassFlowRateMax = this->CHWVolFlowRate * rho;
            this->HWMassFlowRateMax = this->HWVolFlowRate * rho;
            this->GLHEMassFlowRateMax = this->GLHEVolFlowRate * rho;

            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->CHWMassFlowRateMax,
                                               this->CHWInletNodeNum,
                                               this->CHWOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->HWMassFlowRateMax,
                                               this->HWInletNodeNum,
                                               this->HWOutletNodeNum,
                                               this->HWLoopNum,
                                               this->HWLoopSideNum,
                                               this->HWBranchNum,
                                               this->HWCompNum);
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->GLHEMassFlowRateMax,
                                               this->GLHEInletNodeNum,
                                               this->GLHEOutletNodeNum,
                                               this->GLHELoopNum,
                                               this->GLHELoopSideNum,
                                               this->GLHEBranchNum,
                                               this->GLHECompNum);

            // Initialize nodes for individual chiller heaters
            for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRateMin = 0.0;
                this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRateMinAvail = 0.0;
                this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRateMax = rho * this->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate;
                this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRateMaxAvail =
                    rho * this->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate;
                this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRate = 0.0;
                this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateMin = 0.0;
                this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateMinAvail = 0.0;
                this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateMax = rho * this->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate;
                this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateMaxAvail =
                    rho * this->ChillerHeater(ChillerHeaterNum).EvapVolFlowRate;
                this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRate = 0.0;
                this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateRequest = 0.0;
            }
        }
        this->MyWrapperEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->MyWrapperEnvrnFlag = true;
    }

    if (this->CoolSetPointSetToLoop) {
        // IF (CurCoolingLoad > 0.0d0) THEN
        state.dataLoopNodes->Node(this->CHWOutletNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
    }
    // IF (CurHeatingLoad > 0.0d0) THEN
    if (this->HeatSetPointSetToLoop) {
        state.dataLoopNodes->Node(this->HWOutletNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
        // ENDIF
    }

    Real64 mdotCHW;  // Chilled water mass flow rate
    Real64 mdotHW;   // Hot water mass flow rate
    Real64 mdotGLHE; // Condenser water mass flow rate

    // Switch over the mass flow rate to the condenser loop, i.e., ground heat exchanger
    if (LoopNum == this->CWLoopNum) { // called for on cooling loop
        if (MyLoad < -1.0) {          // calling for cooling
            mdotCHW = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMax;
        } else {
            mdotCHW = 0.0;
        }
        if (this->WrapperHeatingLoad > 1.0) {
            mdotHW = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMax;
        } else {
            mdotHW = 0.0;
        }
        if ((MyLoad < -1.0) || (this->WrapperHeatingLoad > 1.0)) {
            mdotGLHE = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMax;
        } else {
            mdotGLHE = 0.0;
        }

    } else if (LoopNum == this->HWLoopNum) {
        if (MyLoad > 1.0) {
            mdotHW = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMax;
        } else {
            mdotHW = 0.0;
        }
        if (this->WrapperCoolingLoad > 1.0) {
            mdotCHW = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMax;
        } else {
            mdotCHW = 0.0;
        }
        if ((MyLoad > 1.0) || (this->WrapperCoolingLoad > 1.0)) {
            mdotGLHE = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMax;
        } else {
            mdotGLHE = 0.0;
        }

    } else if (LoopNum == this->GLHELoopNum) {
        if (this->WrapperCoolingLoad > 1.0) {
            mdotCHW = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMax;
        } else {
            mdotCHW = 0.0;
        }
        if (this->WrapperHeatingLoad > 1.0) {
            mdotHW = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMax;
        } else {
            mdotHW = 0.0;
        }
        if ((this->WrapperHeatingLoad > 1.0) || (this->WrapperCoolingLoad > 1.0)) {
            mdotGLHE = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMax;
        } else {
            mdotGLHE = 0.0;
        }
    }

    PlantUtilities::SetComponentFlowRate(
        state, mdotCHW, this->CHWInletNodeNum, this->CHWOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);

    PlantUtilities::SetComponentFlowRate(
        state, mdotHW, this->HWInletNodeNum, this->HWOutletNodeNum, this->HWLoopNum, this->HWLoopSideNum, this->HWBranchNum, this->HWCompNum);

    PlantUtilities::SetComponentFlowRate(state,
                                         mdotGLHE,
                                         this->GLHEInletNodeNum,
                                         this->GLHEOutletNodeNum,
                                         this->GLHELoopNum,
                                         this->GLHELoopSideNum,
                                         this->GLHEBranchNum,
                                         this->GLHECompNum);
}

void WrapperSpecs::CalcChillerModel(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  Simulate a ChillerHeaterPerformance:Electric:EIR using curve fit

    // METHODOLOGY EMPLOYED:
    //  Use empirical curve fits to model performance at off-reference conditions

    // REFERENCES:
    // 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

    static constexpr std::string_view RoutineName("CalcChillerHeaterModel");
    static constexpr std::string_view RoutineNameElecEIRChiller("CalcElectricEIRChillerModel");

    bool IsLoadCoolRemaining(true);
    bool NextCompIndicator(false);       // Component indicator when identical chiller heaters exist
    int CompNum = 0;                     // Component number in the loop  REAL(r64) :: FRAC
    int IdenticalUnitCounter = 0;        // Pointer to count number of identical unit passed
    Real64 CurAvailCHWMassFlowRate(0.0); // Maximum available mass flow rate for current chiller heater

    // Cooling load evaporator should meet
    Real64 EvaporatorLoad = this->WrapperCoolingLoad;

    // Chilled water inlet mass flow rate
    Real64 CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRate;

    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {

        // Initialize local variables for each chiller heater
        int CurrentMode = 0;
        state.dataPlantCentralGSHP->ChillerCapFT = 0.0;
        state.dataPlantCentralGSHP->ChillerEIRFT = 0.0;
        state.dataPlantCentralGSHP->ChillerEIRFPLR = 0.0;
        state.dataPlantCentralGSHP->ChillerPartLoadRatio = 0.0;
        state.dataPlantCentralGSHP->ChillerCyclingRatio = 0.0;
        state.dataPlantCentralGSHP->ChillerFalseLoadRate = 0.0;

        Real64 CHPower = 0.0;
        Real64 QCondenser = 0.0;
        Real64 QEvaporator = 0.0;
        Real64 FRAC = 1.0;
        Real64 ActualCOP = 0.0;
        Real64 EvapInletTemp = state.dataLoopNodes->Node(this->CHWInletNodeNum).Temp;
        Real64 CondInletTemp = state.dataLoopNodes->Node(this->GLHEInletNodeNum).Temp;
        Real64 EvapOutletTemp = EvapInletTemp;
        Real64 CondOutletTemp = CondInletTemp;
        this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = 0;

        // Find proper schedule values
        if (this->NumOfComp != this->ChillerHeaterNums) { // Identical units exist
            if (ChillerHeaterNum == 1) {
                IdenticalUnitCounter = 0;
                NextCompIndicator = false;
                CompNum = ChillerHeaterNum;
            }
            if (NextCompIndicator) {
                ++CompNum;
            }
            if (CompNum == 1) {
                if (ChillerHeaterNum != this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = false;
                } else if (ChillerHeaterNum == this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = true;
                }
            } else if (CompNum > 1) {
                if ((ChillerHeaterNum - ((ChillerHeaterNum - 1) - IdenticalUnitCounter)) != this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = false;
                } else if ((ChillerHeaterNum - ((ChillerHeaterNum - 1) - IdenticalUnitCounter)) ==
                           this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = true;
                }
            }
            ++IdenticalUnitCounter;
            int IdenticalUnitRemaining = this->WrapperComp(CompNum).WrapperIdenticalObjectNum - IdenticalUnitCounter;
            if (IdenticalUnitRemaining == 0) IdenticalUnitCounter = 0;
        } else if (this->NumOfComp == this->ChillerHeaterNums) {
            ++CompNum;
        }

        if (CompNum > this->NumOfComp) {
            ShowSevereError(state, "CalcChillerModel: ChillerHeater=\"" + this->Name + "\", calculated component number too big.");
            ShowContinueError(state, format("Max number of components=[{}], indicated component number=[{}].", this->NumOfComp, CompNum));
            ShowFatalError(state, "Program terminates due to preceding condition.");
        }

        Real64 EvapMassFlowRate; // Actual evaporator mass flow rate
        Real64 CondMassFlowRate; // Condenser mass flow rate

        // Check whether this chiller heater needs to run
        if (EvaporatorLoad > 0.0 && (ScheduleManager::GetCurrentScheduleValue(state, this->WrapperComp(CompNum).CHSchedPtr) > 0.0)) {
            IsLoadCoolRemaining = true;

            // Calculate density ratios to adjust mass flow rates from initialized ones
            // Hot water temperature is known, but evaporator mass flow rates will be adjusted in the following "Do" loop
            Real64 InitDensity = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
            Real64 EvapDensity = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   EvapInletTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
            Real64 CondDensity = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   CondInletTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);

            // Calculate density ratios to adjust mass flow rates from initialized ones

            // Fraction between standardized density and local density in the chilled water side
            Real64 CHWDensityRatio = EvapDensity / InitDensity;

            // Fraction between standardized density and local density in the condenser side
            Real64 GLHEDensityRatio = CondDensity / InitDensity;
            CondMassFlowRate = this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateMaxAvail;
            EvapMassFlowRate = this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRateMaxAvail;
            EvapMassFlowRate *= CHWDensityRatio;
            CondMassFlowRate *= GLHEDensityRatio;

            // Check available flows from plant and then adjust as necessary
            if (CurAvailCHWMassFlowRate == 0) { // The very first chiller heater to operate
                CurAvailCHWMassFlowRate = CHWInletMassFlowRate;
            } else if (ChillerHeaterNum > 1) {
                CurAvailCHWMassFlowRate -= this->ChillerHeater(ChillerHeaterNum - 1).EvapOutletNode.MassFlowRate;
            }
            EvapMassFlowRate = min(CurAvailCHWMassFlowRate, EvapMassFlowRate);
        } else {
            IsLoadCoolRemaining = false;
            EvapMassFlowRate = 0.0;
            CondMassFlowRate = 0.0;
            CurrentMode = 0;
        }

        // Chiller heater is on when cooling load for this chiller heater remains and chilled water available
        if (IsLoadCoolRemaining && (EvapMassFlowRate > 0) &&
            (ScheduleManager::GetCurrentScheduleValue(state, this->WrapperComp(CompNum).CHSchedPtr) > 0)) {
            // Indicate current mode is cooling-only mode. Simultaneous clg/htg mode will be set later
            CurrentMode = 1;

            // Assign proper performance curve information depending on the control mode
            // Cooling curve is used only for cooling-only mode, and the others (Simultaneous and heating) read the heating curve
            if (this->SimulClgDominant || this->SimulHtgDominant) {
                this->ChillerHeater(ChillerHeaterNum).RefCap = this->ChillerHeater(ChillerHeaterNum).RefCapClgHtg;
                this->ChillerHeater(ChillerHeaterNum).RefCOP = this->ChillerHeater(ChillerHeaterNum).RefCOPClgHtg;
                this->ChillerHeater(ChillerHeaterNum).TempRefEvapOut = this->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg;
                this->ChillerHeater(ChillerHeaterNum).TempRefCondIn = this->ChillerHeater(ChillerHeaterNum).TempRefCondInClgHtg;
                this->ChillerHeater(ChillerHeaterNum).TempRefCondOut = this->ChillerHeater(ChillerHeaterNum).TempRefCondOutClgHtg;
                this->ChillerHeater(ChillerHeaterNum).OptPartLoadRat = this->ChillerHeater(ChillerHeaterNum).OptPartLoadRatClgHtg;
                this->ChillerHeater(ChillerHeaterNum).CondMode = this->ChillerHeater(ChillerHeaterNum).CondModeHeating;
                this->ChillerHeater(ChillerHeaterNum).ChillerCapFTIDX = this->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX;
                this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTIDX = this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX;
                this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX = this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX;
            } else {
                this->ChillerHeater(ChillerHeaterNum).RefCap = this->ChillerHeater(ChillerHeaterNum).RefCapCooling;
                this->ChillerHeater(ChillerHeaterNum).RefCOP = this->ChillerHeater(ChillerHeaterNum).RefCOPCooling;
                this->ChillerHeater(ChillerHeaterNum).TempRefEvapOut = this->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling;
                this->ChillerHeater(ChillerHeaterNum).TempRefCondIn = this->ChillerHeater(ChillerHeaterNum).TempRefCondInCooling;
                this->ChillerHeater(ChillerHeaterNum).TempRefCondOut = this->ChillerHeater(ChillerHeaterNum).TempRefCondOutCooling;
                this->ChillerHeater(ChillerHeaterNum).OptPartLoadRat = this->ChillerHeater(ChillerHeaterNum).OptPartLoadRatCooling;
                this->ChillerHeater(ChillerHeaterNum).CondMode = this->ChillerHeater(ChillerHeaterNum).CondModeCooling;
                this->ChillerHeater(ChillerHeaterNum).ChillerCapFTIDX = this->ChillerHeater(ChillerHeaterNum).ChillerCapFTCoolingIDX;
                this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTIDX = this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTCoolingIDX;
                this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX = this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRCoolingIDX;
            }

            // Only used to read curve values
            CondOutletTemp = this->ChillerHeater(ChillerHeaterNum).TempRefCondOutCooling;
            Real64 CondTempforCurve;
            if (this->ChillerHeater(ChillerHeaterNum).CondMode == "ENTERINGCONDENSER") {
                CondTempforCurve = CondInletTemp;
            } else if (this->ChillerHeater(ChillerHeaterNum).CondMode == "LEAVINGCONDENSER") {
                CondTempforCurve = CondOutletTemp;
            } else {
                ShowWarningError(state, "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(ChillerHeaterNum).Name + "\":");
                ShowContinueError(state,
                                  format("Chiller condenser temperature for curve fit are not decided, defalt value= cond_leaving ({:.3R}).",
                                         state.dataPlantCentralGSHP->ChillerCapFT));
                CondTempforCurve = CondOutletTemp;
            }

            // Bind local variables from the curve
            Real64 MinPartLoadRat; // Min allowed operating fraction of full load
            Real64 MaxPartLoadRat; // Max allowed operating fraction of full load

            CurveManager::GetCurveMinMaxValues(state, this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX, MinPartLoadRat, MaxPartLoadRat);

            // Chiller reference capacity
            Real64 ChillerRefCap = this->ChillerHeater(ChillerHeaterNum).RefCap;
            Real64 ReferenceCOP = this->ChillerHeater(ChillerHeaterNum).RefCOP;
            Real64 TempLowLimitEout = this->ChillerHeater(ChillerHeaterNum).TempLowLimitEvapOut;
            Real64 EvapOutletTempSetPoint = this->ChillerHeater(ChillerHeaterNum).TempRefEvapOutCooling;
            state.dataPlantCentralGSHP->ChillerCapFT =
                CurveManager::CurveValue(state, this->ChillerHeater(ChillerHeaterNum).ChillerCapFTIDX, EvapOutletTempSetPoint, CondTempforCurve);

            if (state.dataPlantCentralGSHP->ChillerCapFT < 0) {
                if (this->ChillerHeater(ChillerHeaterNum).ChillerCapFTError < 1 && !state.dataGlobal->WarmupFlag) {
                    ++this->ChillerHeater(ChillerHeaterNum).ChillerCapFTError;
                    ShowWarningError(state, "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(ChillerHeaterNum).Name + "\":");
                    ShowContinueError(state,
                                      format(" ChillerHeater Capacity as a Function of Temperature curve output is negative ({:.3R}).",
                                             state.dataPlantCentralGSHP->ChillerCapFT));
                    ShowContinueError(state,
                                      format(" Negative value occurs using an Evaporator Outlet Temp of {:.1R} and a Condenser Inlet Temp of {:.1R}.",
                                             EvapOutletTempSetPoint,
                                             CondInletTemp));
                    ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
                } else if (!state.dataGlobal->WarmupFlag) {
                    ++this->ChillerHeater(ChillerHeaterNum).ChillerCapFTError;
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(ChillerHeaterNum).Name +
                            "\": ChillerHeater Capacity as a Function of Temperature curve output is negative warning continues...",
                        this->ChillerHeater(ChillerHeaterNum).ChillerCapFTErrorIndex,
                        state.dataPlantCentralGSHP->ChillerCapFT,
                        state.dataPlantCentralGSHP->ChillerCapFT);
                }
                state.dataPlantCentralGSHP->ChillerCapFT = 0.0;
            }

            // Calculate the specific heat of chilled water
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                               EvapInletTemp,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);

            // Calculate cooling load this chiller should meet and the other chillers are demanded
            EvapOutletTempSetPoint = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;

            // Minimum capacity of the evaporator
            Real64 EvaporatorCapMin =
                this->ChillerHeater(ChillerHeaterNum).MinPartLoadRatCooling * this->ChillerHeater(ChillerHeaterNum).RefCapCooling;

            // Remaining cooling load the other chiller heaters should meet
            Real64 CoolingLoadToMeet = min(this->ChillerHeater(ChillerHeaterNum).RefCapCooling, max(std::abs(EvaporatorLoad), EvaporatorCapMin));

            // Available chiller capacity as a function of temperature
            // Chiller available capacity at current operating conditions [W]
            Real64 AvailChillerCap = ChillerRefCap * state.dataPlantCentralGSHP->ChillerCapFT;

            // Set load this chiller heater should meet
            QEvaporator = min(CoolingLoadToMeet, (AvailChillerCap * MaxPartLoadRat));
            EvapOutletTemp = EvapOutletTempSetPoint;
            Real64 EvapDeltaTemp = EvapInletTemp - EvapOutletTemp;

            Real64 PartLoadRat; // Operating part load ratio

            // Calculate temperatures for constant flow and mass flow rates for variable flow
            if (EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                if (this->SimulHtgDominant) { // Evaporator operates at full capacity for heating
                    PartLoadRat = max(0.0, min((ChillerRefCap / AvailChillerCap), MaxPartLoadRat));
                    QEvaporator = AvailChillerCap * PartLoadRat;
                    EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
                    EvapOutletTemp = EvapInletTemp - EvapDeltaTemp;
                } else {                        // Cooling only mode or cooling dominant simultaneous htg/clg mode
                    if (this->VariableFlowCH) { // Variable flow
                        Real64 EvapMassFlowRateCalc = QEvaporator / EvapDeltaTemp / Cp;
                        if (EvapMassFlowRateCalc > EvapMassFlowRate) {
                            EvapMassFlowRateCalc = EvapMassFlowRate;
                            Real64 EvapDeltaTempCalc = QEvaporator / EvapMassFlowRate / Cp;
                            EvapOutletTemp = EvapInletTemp - EvapDeltaTempCalc;
                            if (EvapDeltaTempCalc > EvapDeltaTemp) {
                                QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                            }
                        }
                        EvapMassFlowRate = EvapMassFlowRateCalc;
                    } else { // Constant Flow
                        Real64 EvapOutletTempCalc = EvapInletTemp - EvapDeltaTemp;
                        if (EvapOutletTempCalc > EvapOutletTemp) { // Load to meet should be adjusted
                            EvapOutletTempCalc = EvapOutletTemp;
                            QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                        }
                        EvapOutletTemp = EvapOutletTempCalc;
                    } // End of flow control decision
                }     // End of operation mode
            } else {
                QEvaporator = 0.0;
                EvapOutletTemp = EvapInletTemp;
            }

            // Check evaporator temperature low limit and adjust capacity if needed
            if (EvapOutletTemp < TempLowLimitEout) {
                if ((EvapInletTemp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                    EvapOutletTemp = TempLowLimitEout;
                    EvapDeltaTemp = EvapInletTemp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    QEvaporator = 0.0;
                    EvapOutletTemp = EvapInletTemp;
                }
            }

            // Check if the outlet temperature exceeds the node minimum temperature and adjust capacity if needed
            if (EvapOutletTemp < this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin) {
                if ((this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp - this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin) >
                    DataPlant::DeltaTempTol) {
                    EvapOutletTemp = this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin;
                    EvapDeltaTemp = this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    QEvaporator = 0.0;
                    EvapOutletTemp = EvapInletTemp;
                }
            }

            // Calculate part load once more since evaporator capacity might be modified
            if (AvailChillerCap > 0.0) {
                PartLoadRat = max(0.0, min((QEvaporator / AvailChillerCap), MaxPartLoadRat));
            } else {
                PartLoadRat = 0.0;
            }

            // Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
            if (PartLoadRat < MinPartLoadRat) FRAC = min(1.0, (PartLoadRat / MinPartLoadRat));

            // set the module level variable used for reporting FRAC
            state.dataPlantCentralGSHP->ChillerCyclingRatio = FRAC;

            // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
            if (AvailChillerCap > 0.0) {
                PartLoadRat = max(PartLoadRat, MinPartLoadRat);
            } else {
                PartLoadRat = 0.0;
            }

            // set the module level variable used for reporting PLR
            state.dataPlantCentralGSHP->ChillerPartLoadRatio = PartLoadRat;

            // calculate the load due to false loading on chiller over and above water side load
            state.dataPlantCentralGSHP->ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - QEvaporator;
            if (state.dataPlantCentralGSHP->ChillerFalseLoadRate < DataHVACGlobals::SmallLoad) {
                state.dataPlantCentralGSHP->ChillerFalseLoadRate = 0.0;
            }

            // Determine chiller compressor power and transfer heat calculation
            state.dataPlantCentralGSHP->ChillerEIRFT =
                max(0.0, CurveManager::CurveValue(state, this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTIDX, EvapOutletTemp, CondTempforCurve));
            state.dataPlantCentralGSHP->ChillerEIRFPLR =
                max(0.0, CurveManager::CurveValue(state, this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX, PartLoadRat));

            if (ReferenceCOP <= 0.0) {
                CHPower = 0.0;
            } else {
                CHPower =
                    (AvailChillerCap / ReferenceCOP) * state.dataPlantCentralGSHP->ChillerEIRFPLR * state.dataPlantCentralGSHP->ChillerEIRFT * FRAC;
            }

            QCondenser =
                CHPower * this->ChillerHeater(ChillerHeaterNum).OpenMotorEff + QEvaporator + state.dataPlantCentralGSHP->ChillerFalseLoadRate;

            if (CHPower == 0.0) {
                ActualCOP = 0.0;
            } else {
                ActualCOP = (QEvaporator + state.dataPlantCentralGSHP->ChillerFalseLoadRate) / CHPower;
            }

            if (CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->GLHELoopNum).FluidName,
                                                            CondInletTemp,
                                                            state.dataPlnt->PlantLoop(this->GLHELoopNum).FluidIndex,
                                                            RoutineNameElecEIRChiller);
                CondOutletTemp = QCondenser / CondMassFlowRate / Cp + CondInletTemp;
            } else {
                ShowSevereError(state, "CalcChillerheaterModel: Condenser flow = 0, for Chillerheater=" + this->ChillerHeater(ChillerHeaterNum).Name);
                ShowContinueErrorTimeStamp(state, "");
            }

            // Determine load next chillers should meet
            if (EvaporatorLoad < QEvaporator) {
                EvaporatorLoad = 0.0; // No remaining load so the rest will be off
            } else {
                EvaporatorLoad -= QEvaporator;
            }

            // Initialize reporting variable when this chiller doesn't need to operate
            if (QEvaporator == 0.0) {
                CurrentMode = 0;
                state.dataPlantCentralGSHP->ChillerPartLoadRatio = 0.0;
                state.dataPlantCentralGSHP->ChillerCyclingRatio = 0.0;
                state.dataPlantCentralGSHP->ChillerFalseLoadRate = 0.0;
                EvapMassFlowRate = 0.0;
                CondMassFlowRate = 0.0;
                CHPower = 0.0;
                QCondenser = 0.0;
                EvapOutletTemp = EvapInletTemp;
                CondOutletTemp = CondInletTemp;
                EvaporatorLoad = 0.0;
            }

        } // End of calculation for cooling

        // Set variables to the arrays
        this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.MassFlowRate = EvapMassFlowRate;
        this->ChillerHeater(ChillerHeaterNum).CondOutletNode.MassFlowRate = CondMassFlowRate;
        this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.Temp = EvapOutletTemp;
        this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp = EvapInletTemp;
        this->ChillerHeater(ChillerHeaterNum).CondOutletNode.Temp = CondOutletTemp;
        this->ChillerHeater(ChillerHeaterNum).CondInletNode.Temp = CondInletTemp;
        this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = CurrentMode;
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatio = state.dataPlantCentralGSHP->ChillerPartLoadRatio;
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatio = state.dataPlantCentralGSHP->ChillerCyclingRatio;
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate = state.dataPlantCentralGSHP->ChillerFalseLoadRate;
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFT = state.dataPlantCentralGSHP->ChillerCapFT;
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFT = state.dataPlantCentralGSHP->ChillerEIRFT;
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLR = state.dataPlantCentralGSHP->ChillerEIRFPLR;
        this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower = CHPower;
        this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower = 0.0;
        this->ChillerHeater(ChillerHeaterNum).Report.QEvap = QEvaporator;
        this->ChillerHeater(ChillerHeaterNum).Report.QCond = QCondenser;
        this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp = EvapOutletTemp;
        this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTemp = EvapInletTemp;
        this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp = CondOutletTemp;
        this->ChillerHeater(ChillerHeaterNum).Report.CondInletTemp = CondInletTemp;
        this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot = EvapMassFlowRate;
        this->ChillerHeater(ChillerHeaterNum).Report.Condmdot = CondMassFlowRate;
        this->ChillerHeater(ChillerHeaterNum).Report.ActualCOP = ActualCOP;

        if (this->SimulClgDominant || this->SimulHtgDominant) { // Store for using these cooling side data in the hot water loop
            this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = CurrentMode;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatioSimul = state.dataPlantCentralGSHP->ChillerPartLoadRatio;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatioSimul = state.dataPlantCentralGSHP->ChillerCyclingRatio;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRateSimul = state.dataPlantCentralGSHP->ChillerFalseLoadRate;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFTSimul = state.dataPlantCentralGSHP->ChillerCapFT;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFTSimul = state.dataPlantCentralGSHP->ChillerEIRFT;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLRSimul = state.dataPlantCentralGSHP->ChillerEIRFPLR;
            this->ChillerHeater(ChillerHeaterNum).Report.CoolingPowerSimul = CHPower;
            this->ChillerHeater(ChillerHeaterNum).Report.QEvapSimul = QEvaporator;
            this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTempSimul = EvapOutletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTempSimul = EvapInletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.EvapmdotSimul = EvapMassFlowRate;
            if (this->SimulClgDominant) {
                this->ChillerHeater(ChillerHeaterNum).Report.QCondSimul = QCondenser;
                this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTempSimul = CondOutletTemp;
                this->ChillerHeater(ChillerHeaterNum).Report.CondInletTempSimul = CondInletTemp;
                this->ChillerHeater(ChillerHeaterNum).Report.CondmdotSimul = CondMassFlowRate;
            }
        }
    }
}

void WrapperSpecs::CalcChillerHeaterModel(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  Simulate a ChillerHeaterPerformance:Electric:EIR using curve fit

    // METHODOLOGY EMPLOYED:
    //  Use empirical curve fits to model performance at off-reference conditions

    // REFERENCES:
    // 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

    static constexpr std::string_view RoutineName("CalcChillerHeaterModel");
    static constexpr std::string_view RoutineNameElecEIRChiller("CalcElectricEIRChillerModel");

    bool IsLoadHeatRemaining(true);     // Ture if heating load remains for this chiller heater
    bool NextCompIndicator(false);      // Component indicator when identical chiller heaters exist
    int CompNum(0);                     // Component number
    int IdenticalUnitCounter = 0;       // Pointer to count number of identical unit passed
    int IdenticalUnitRemaining;         // Pointer to count number of identical unit available for a component
    Real64 CondenserLoad(0.0);          // Remaining heating load that this wrapper should meet
    Real64 CurAvailHWMassFlowRate(0.0); // Maximum available hot water mass within the wrapper bank

    CondenserLoad = this->WrapperHeatingLoad;
    Real64 HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRate;

    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {

        // Set module level inlet and outlet nodes and initialize other local variables
        int CurrentMode = 0;
        state.dataPlantCentralGSHP->ChillerPartLoadRatio = 0.0;
        state.dataPlantCentralGSHP->ChillerCyclingRatio = 0.0;
        state.dataPlantCentralGSHP->ChillerFalseLoadRate = 0.0;
        Real64 CHPower = 0.0;
        Real64 QCondenser = 0.0;
        Real64 QEvaporator = 0.0;
        Real64 FRAC = 1.0;
        Real64 CondDeltaTemp = 0.0;
        Real64 CoolingPower = 0.0;
        Real64 ActualCOP = 0.0;
        Real64 EvapInletTemp = state.dataLoopNodes->Node(this->GLHEInletNodeNum).Temp;
        Real64 CondInletTemp = state.dataLoopNodes->Node(this->HWInletNodeNum).Temp;
        Real64 EvapOutletTemp = EvapInletTemp;
        Real64 CondOutletTemp = CondInletTemp;

        // Find proper schedule values
        if (this->NumOfComp != this->ChillerHeaterNums) { // Identical units exist
            if (ChillerHeaterNum == 1) {
                IdenticalUnitCounter = 0;
                NextCompIndicator = false;
                CompNum = ChillerHeaterNum;
            }
            if (NextCompIndicator) {
                ++CompNum;
            }
            if (CompNum == 1) {
                if (ChillerHeaterNum != this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = false;
                } else if (ChillerHeaterNum == this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = true;
                }
            } else if (CompNum > 1) {
                if ((ChillerHeaterNum - ((ChillerHeaterNum - 1) - IdenticalUnitCounter)) != this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = false;
                } else if ((ChillerHeaterNum - ((ChillerHeaterNum - 1) - IdenticalUnitCounter)) ==
                           this->WrapperComp(CompNum).WrapperIdenticalObjectNum) {
                    NextCompIndicator = true;
                }
            }
            ++IdenticalUnitCounter;
            IdenticalUnitRemaining = this->WrapperComp(CompNum).WrapperIdenticalObjectNum - IdenticalUnitCounter;
            if (IdenticalUnitRemaining == 0) IdenticalUnitCounter = 0;
        } else if (this->NumOfComp == this->ChillerHeaterNums) {
            ++CompNum;
        }

        Real64 CondMassFlowRate; // Condenser mass flow rate through this chiller heater
        Real64 EvapMassFlowRate; // Evaporator mass flow rate through this chiller heater

        // Check to see if this chiller heater needs to run
        if (CondenserLoad > 0.0 && (ScheduleManager::GetCurrentScheduleValue(state, this->WrapperComp(CompNum).CHSchedPtr) > 0)) {
            IsLoadHeatRemaining = true;

            // Calculate density ratios to adjust mass flow rates from initialized ones
            // Hot water temperature is known, but condenser mass flow rates will be adjusted in the following "Do" loop
            Real64 InitDensity = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
            Real64 EvapDensity = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   EvapInletTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
            Real64 CondDensity = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   CondInletTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);

            // Calculate density ratios to adjust mass flow rates from initialized ones
            Real64 HWDensityRatio = CondDensity / InitDensity;
            Real64 GLHEDensityRatio = EvapDensity / InitDensity;
            EvapMassFlowRate = this->ChillerHeater(ChillerHeaterNum).EvapInletNode.MassFlowRateMaxAvail;
            CondMassFlowRate = this->ChillerHeater(ChillerHeaterNum).CondInletNode.MassFlowRateMaxAvail;
            EvapMassFlowRate *= GLHEDensityRatio;
            CondMassFlowRate *= HWDensityRatio;

            // Check flows from plant to adjust as necessary
            if (CurAvailHWMassFlowRate == 0) { // First chiller heater which is on
                CurAvailHWMassFlowRate = HWInletMassFlowRate;
            } else if (ChillerHeaterNum > 1) {
                CurAvailHWMassFlowRate -= this->ChillerHeater(ChillerHeaterNum - 1).CondOutletNode.MassFlowRate;
            }
            CondMassFlowRate = min(CurAvailHWMassFlowRate, CondMassFlowRate);

            // It is not enforced to be the smaller of CH max temperature and plant temp setpoint.
            // Hot water temperatures at the individual CHs' outlet may be greater than plant setpoint temp,
            // but should be lower than the CHs max temp
            CondOutletTemp = this->ChillerHeater(ChillerHeaterNum).TempRefCondOutClgHtg;
            CondDeltaTemp = CondOutletTemp - CondInletTemp;

            if (CondDeltaTemp < 0.0) { // Hot water temperature is greater than the maximum
                if (this->ChillerHeater(ChillerHeaterNum).ChillerEIRRefTempErrorIndex == 0) {
                    ShowSevereMessage(state,
                                      "CalcChillerHeaterModel: ChillerHeaterPerformance:Electric:EIR=\"" +
                                          this->ChillerHeater(ChillerHeaterNum).Name + "\", DeltaTemp < 0");
                    ShowContinueError(
                        state, format(" Reference Simultaneous Cooling-Heating Mode Leaving Condenser Water Temperature [{:.1R}]", CondOutletTemp));
                    ShowContinueError(state, format("is below condenser inlet temperature of [{:.1R}].", CondInletTemp));
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, " Reset reference temperature to one greater than the inlet temperature ");
                }
                ShowRecurringSevereErrorAtEnd(state,
                                              "ChillerHeaterPerformance:Electric:EIR=\"" + this->ChillerHeater(ChillerHeaterNum).Name +
                                                  "\": Reference temperature problems continue.",
                                              this->ChillerHeater(ChillerHeaterNum).ChillerEIRRefTempErrorIndex,
                                              CondDeltaTemp,
                                              CondDeltaTemp,
                                              _,
                                              "deltaC",
                                              "deltaC");
                QCondenser = 0.0;
                IsLoadHeatRemaining = false;
            }

            if (ChillerHeaterNum > 1) {
                // Operation mode needs to be set in a simultaneous clg/htg mode
                // Always off even heating load remains if this CH is assumed to be off in the loop 1
                if (this->SimulClgDominant) {
                    if (this->ChillerHeater(ChillerHeaterNum).Report.QEvapSimul == 0.0) {
                        CurrentMode = 0;
                        IsLoadHeatRemaining = false;
                    } else { // Heat recovery
                        CurrentMode = 3;
                    }
                }
            } // End of simulataneous clg/htg mode detemination

        } else { // chiller heater is off
            IsLoadHeatRemaining = false;
            CondMassFlowRate = 0.0;
            EvapMassFlowRate = 0.0;
            CurrentMode = 0;
            if (this->SimulClgDominant) {
                if (this->ChillerHeater(ChillerHeaterNum).Report.QEvapSimul > 0.0) {
                    CurrentMode = 4; // Simultaneous cooling dominant mode: 4
                }
            } // End of mode determination
        }     // End of system operation determinatoin

        if (IsLoadHeatRemaining && CondMassFlowRate > 0.0 &&
            (ScheduleManager::GetCurrentScheduleValue(state, this->WrapperComp(CompNum).CHSchedPtr) > 0)) { // System is on
            // Operation mode
            if (this->SimulHtgDominant) {
                if (this->ChillerHeater(ChillerHeaterNum).Report.QEvapSimul == 0.0) {
                    CurrentMode = 5; // No cooling necessary
                } else {             // Heat recovery mode. Both chilled water and hot water loops are connected. No condenser flow.
                    CurrentMode = 3;
                }
            }

            // Mode 3 and 5 use cooling side data stored from the chilled water loop
            // Mode 4 uses all data from the chilled water loop due to no heating demand
            if (this->SimulClgDominant || CurrentMode == 3) {
                CurrentMode = 3;
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                                   CondInletTemp,
                                                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                                   RoutineName);

                QCondenser = this->ChillerHeater(ChillerHeaterNum).Report.QCondSimul;

                if (this->VariableFlowCH) { // Variable flow
                    Real64 CondMassFlowRateCalc = QCondenser / CondDeltaTemp / Cp;
                    if (CondMassFlowRateCalc > CondMassFlowRate) {
                        CondMassFlowRateCalc = CondMassFlowRate;
                        Real64 CondDeltaTempCalc = QCondenser / CondMassFlowRate / Cp;
                        if (CondDeltaTempCalc > CondDeltaTemp) { // Load to meet should be adjusted
                            QCondenser = CondMassFlowRate * Cp * CondDeltaTemp;
                        }
                    }
                    CondMassFlowRate = CondMassFlowRateCalc;
                } else { // Constant flow control
                    Real64 CondDeltaTempCalc = QCondenser / CondMassFlowRate / Cp;
                    Real64 CondOutletTempCalc = CondDeltaTempCalc + CondInletTemp;
                    if (CondOutletTempCalc > CondOutletTemp) {
                        CondOutletTempCalc = CondOutletTemp;
                        QCondenser = CondMassFlowRate * Cp * CondDeltaTemp;
                    }
                    CondOutletTemp = CondOutletTempCalc;
                }

            } else { // Either Mode 2 or 3 or 5
                if (this->SimulHtgDominant) {
                    CurrentMode = 5;
                } else {
                    CurrentMode = 2;
                }

                state.dataPlantCentralGSHP->ChillerCapFT = 0.0;
                state.dataPlantCentralGSHP->ChillerEIRFT = 0.0;
                state.dataPlantCentralGSHP->ChillerEIRFPLR = 0.0;

                // Assign curve values to local data array
                this->ChillerHeater(ChillerHeaterNum).RefCap = this->ChillerHeater(ChillerHeaterNum).RefCapClgHtg;
                this->ChillerHeater(ChillerHeaterNum).RefCOP = this->ChillerHeater(ChillerHeaterNum).RefCOPClgHtg;
                this->ChillerHeater(ChillerHeaterNum).TempRefEvapOut = this->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg;
                this->ChillerHeater(ChillerHeaterNum).TempRefCondOut = this->ChillerHeater(ChillerHeaterNum).TempRefCondOutClgHtg;
                this->ChillerHeater(ChillerHeaterNum).OptPartLoadRat = this->ChillerHeater(ChillerHeaterNum).OptPartLoadRatClgHtg;
                this->ChillerHeater(ChillerHeaterNum).CondMode = this->ChillerHeater(ChillerHeaterNum).CondModeHeating;
                this->ChillerHeater(ChillerHeaterNum).ChillerCapFTIDX = this->ChillerHeater(ChillerHeaterNum).ChillerCapFTHeatingIDX;
                this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTIDX = this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTHeatingIDX;
                this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX = this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRHeatingIDX;

                Real64 CondTempforCurve; // Reference condenser temperature for the performance curve reading

                if (this->ChillerHeater(ChillerHeaterNum).CondMode == "ENTERINGCONDENSER") {
                    CondTempforCurve = CondInletTemp;
                } else if (this->ChillerHeater(ChillerHeaterNum).CondMode == "LEAVINGCONDENSER") {
                    CondTempforCurve = this->ChillerHeater(ChillerHeaterNum).TempRefCondOutClgHtg; //! CondOutletTemp
                } else {
                    ShowWarningError(state, "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(ChillerHeaterNum).Name + "\":");
                    ShowContinueError(state,
                                      format("Chiller condenser temperature for curve fit are not decided, default value= cond_leaving ({:.3R}).",
                                             state.dataPlantCentralGSHP->ChillerCapFT));
                    CondTempforCurve = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
                }

                Real64 MinPartLoadRat; // Min allowed operating fraction of full load
                Real64 MaxPartLoadRat; // Max allowed operating fraction of full load

                CurveManager::GetCurveMinMaxValues(state, this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX, MinPartLoadRat, MaxPartLoadRat);
                Real64 ChillerRefCap = this->ChillerHeater(ChillerHeaterNum).RefCap;
                Real64 ReferenceCOP = this->ChillerHeater(ChillerHeaterNum).RefCOP;
                EvapOutletTemp = this->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg;
                Real64 TempLowLimitEout = this->ChillerHeater(ChillerHeaterNum).TempLowLimitEvapOut;
                Real64 EvapOutletTempSetPoint = this->ChillerHeater(ChillerHeaterNum).TempRefEvapOutClgHtg;
                state.dataPlantCentralGSHP->ChillerCapFT =
                    CurveManager::CurveValue(state, this->ChillerHeater(ChillerHeaterNum).ChillerCapFTIDX, EvapOutletTempSetPoint, CondTempforCurve);

                if (state.dataPlantCentralGSHP->ChillerCapFT < 0) {
                    if (this->ChillerHeater(ChillerHeaterNum).ChillerCapFTError < 1 && !state.dataGlobal->WarmupFlag) {
                        ++this->ChillerHeater(ChillerHeaterNum).ChillerCapFTError;
                        ShowWarningError(state, "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(ChillerHeaterNum).Name + "\":");
                        ShowContinueError(state,
                                          format(" ChillerHeater Capacity as a Function of Temperature curve output is negative ({:.3R}).",
                                                 state.dataPlantCentralGSHP->ChillerCapFT));
                        ShowContinueError(
                            state,
                            format(" Negative value occurs using an Evaporator Outlet Temp of {:.1R} and a Condenser Inlet Temp of {:.1R}.",
                                   EvapOutletTempSetPoint,
                                   CondInletTemp));
                        ShowContinueErrorTimeStamp(state, " Resetting curve output to zero and continuing simulation.");
                    } else if (!state.dataGlobal->WarmupFlag) {
                        ++this->ChillerHeater(ChillerHeaterNum).ChillerCapFTError;
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            "ChillerHeaterPerformance:Electric:EIR \"" + this->ChillerHeater(ChillerHeaterNum).Name +
                                "\": ChillerHeater Capacity as a Function of Temperature curve output is negative warning continues...",
                            this->ChillerHeater(ChillerHeaterNum).ChillerCapFTErrorIndex,
                            state.dataPlantCentralGSHP->ChillerCapFT,
                            state.dataPlantCentralGSHP->ChillerCapFT);
                    }
                    state.dataPlantCentralGSHP->ChillerCapFT = 0.0;
                }

                // Available chiller capacity as a function of temperature
                Real64 AvailChillerCap = ChillerRefCap * state.dataPlantCentralGSHP->ChillerCapFT;

                Real64 PartLoadRat; // Operating part load ratio

                // Part load ratio based on reference capacity and available chiller capacity
                if (AvailChillerCap > 0) {
                    PartLoadRat = max(0.0, min((ChillerRefCap / AvailChillerCap), MaxPartLoadRat));
                } else {
                    PartLoadRat = 0.0;
                }

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                                   this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp,
                                                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                                   RoutineName);

                // Calculate evaporator heat transfer
                if (EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    QEvaporator = AvailChillerCap * PartLoadRat;
                    Real64 EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
                    EvapOutletTemp = EvapInletTemp - EvapDeltaTemp;
                }

                // Check that the evaporator outlet temp honors both plant loop temp low limit and also the chiller low limit
                if (EvapOutletTemp < TempLowLimitEout) {
                    if ((this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                        EvapOutletTemp = TempLowLimitEout;
                        Real64 EvapDeltaTemp = this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp - EvapOutletTemp;
                        QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                    } else {
                        EvapOutletTemp = this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp;
                        Real64 EvapDeltaTemp = this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp - EvapOutletTemp;
                        QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                    }
                }

                if (EvapOutletTemp < this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin) {
                    if ((this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp - this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin) >
                        DataPlant::DeltaTempTol) {
                        EvapOutletTemp = this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin;
                        Real64 EvapDeltaTemp = this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin - EvapOutletTemp;
                        QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                    } else {
                        EvapOutletTemp = this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin;
                        Real64 EvapDeltaTemp = this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.TempMin - EvapOutletTemp;
                        QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
                    }
                }

                // Evaporator operates at full load
                if (AvailChillerCap > 0.0) {
                    PartLoadRat = max(0.0, min((QEvaporator / AvailChillerCap), MaxPartLoadRat));
                } else {
                    PartLoadRat = 0.0;
                }

                // Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
                if (PartLoadRat < MinPartLoadRat) FRAC = min(1.0, (PartLoadRat / MinPartLoadRat));
                if (FRAC <= 0.0) FRAC = 1.0; // CR 9303 COP reporting issue, it should be greater than zero in this routine
                state.dataPlantCentralGSHP->ChillerCyclingRatio = FRAC;

                // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
                if (AvailChillerCap > 0.0) {
                    PartLoadRat = max(PartLoadRat, MinPartLoadRat);
                } else {
                    PartLoadRat = 0.0;
                }
                // Evaporator part load ratio
                state.dataPlantCentralGSHP->ChillerPartLoadRatio = PartLoadRat;

                // calculate the load due to false loading on chiller over and above water side load
                state.dataPlantCentralGSHP->ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - QEvaporator;
                if (state.dataPlantCentralGSHP->ChillerFalseLoadRate < DataHVACGlobals::SmallLoad) {
                    state.dataPlantCentralGSHP->ChillerFalseLoadRate = 0.0;
                }

                state.dataPlantCentralGSHP->ChillerEIRFT = max(
                    0.0, CurveManager::CurveValue(state, this->ChillerHeater(ChillerHeaterNum).ChillerEIRFTIDX, EvapOutletTemp, CondTempforCurve));
                state.dataPlantCentralGSHP->ChillerEIRFPLR =
                    max(0.0, CurveManager::CurveValue(state, this->ChillerHeater(ChillerHeaterNum).ChillerEIRFPLRIDX, PartLoadRat));
                CHPower =
                    (AvailChillerCap / ReferenceCOP) * state.dataPlantCentralGSHP->ChillerEIRFPLR * state.dataPlantCentralGSHP->ChillerEIRFT * FRAC;

                if (CHPower <= 0.0) {
                    ActualCOP = 0.0;
                } else {
                    ActualCOP = (QEvaporator + state.dataPlantCentralGSHP->ChillerFalseLoadRate) / CHPower;
                }

                QCondenser =
                    CHPower * this->ChillerHeater(ChillerHeaterNum).OpenMotorEff + QEvaporator + state.dataPlantCentralGSHP->ChillerFalseLoadRate;

                // Determine heating load for this heater and pass the remaining load to the next chiller heater
                Real64 CondenserCapMin = QCondenser * MinPartLoadRat;
                Real64 HeatingLoadToMeet = min(QCondenser, max(std::abs(CondenserLoad), CondenserCapMin));

                // Set load this chiller heater should meet and temperatures given
                QCondenser = min(HeatingLoadToMeet, QCondenser);

                Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                            CondInletTemp,
                                                            state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                            RoutineNameElecEIRChiller);

                // Calculate temperatures for constant flow and mass flow rate for variable flow
                // Limit mass for this chiller heater to the available mass at given temperature conditions
                // when mass calculated to meet the load is greater than the maximum available
                // then recalculate heating load this chiller heater can meet
                if (CurrentMode == 2 || this->SimulHtgDominant) {
                    if (CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance && CondDeltaTemp > 0.0) {
                        if (this->VariableFlowCH) { // Variable flow
                            Real64 CondMassFlowRateCalc = QCondenser / CondDeltaTemp / Cp;
                            if (CondMassFlowRateCalc > CondMassFlowRate) {
                                CondMassFlowRateCalc = CondMassFlowRate;
                                Real64 CondDeltaTempCalc = QCondenser / CondMassFlowRate / Cp;
                                if (CondDeltaTempCalc > CondDeltaTemp) { // Load to meet should be adjusted
                                    QCondenser = CondMassFlowRate * Cp * CondDeltaTemp;
                                }
                            }
                            CondMassFlowRate = CondMassFlowRateCalc;
                        } else { // Constant Flow at a fixed flow rate and capacity
                            Real64 CondDeltaTempCalc = QCondenser / CondMassFlowRate / Cp;
                            Real64 CondOutletTempCalc = CondDeltaTempCalc + CondInletTemp;
                            if (CondOutletTempCalc > CondOutletTemp) { // Load to meet should be adjusted
                                CondOutletTempCalc = CondOutletTemp;
                                QCondenser = CondMassFlowRate * Cp * CondDeltaTemp;
                            }
                            CondOutletTemp = CondOutletTempCalc;
                        }
                    } else {
                        QCondenser = 0.0;
                        CondOutletTemp = CondInletTemp;
                    }
                }

            } // End of calculation depending on the modes

            // Determine load next chiller heater meets
            if (CondenserLoad < QCondenser) { // Heating load is met by this chiller heater
                CondenserLoad = 0.0;
            } else {
                CondenserLoad -= QCondenser;
            }

            if (QCondenser == 0.0) {
                CurrentMode = 0;
                state.dataPlantCentralGSHP->ChillerPartLoadRatio = 0.0;
                state.dataPlantCentralGSHP->ChillerCyclingRatio = 0.0;
                state.dataPlantCentralGSHP->ChillerFalseLoadRate = 0.0;
                EvapMassFlowRate = 0.0;
                CondMassFlowRate = 0.0;
                CHPower = 0.0;
                QEvaporator = 0.0;
                EvapOutletTemp = EvapInletTemp;
                CondOutletTemp = CondInletTemp;
                CondenserLoad = 0.0;
            }

            // Heat recovery or cooling dominant modes need to use the evaporator side information
            if (CurrentMode == 3 || CurrentMode == 4) {
                state.dataPlantCentralGSHP->ChillerPartLoadRatio = this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatioSimul;
                state.dataPlantCentralGSHP->ChillerCyclingRatio = this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatioSimul;
                state.dataPlantCentralGSHP->ChillerFalseLoadRate = this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRateSimul;
                state.dataPlantCentralGSHP->ChillerCapFT = this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFTSimul;
                state.dataPlantCentralGSHP->ChillerEIRFT = this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFTSimul;
                state.dataPlantCentralGSHP->ChillerEIRFPLR = this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLRSimul;
                QEvaporator = this->ChillerHeater(ChillerHeaterNum).Report.QEvapSimul;
                EvapOutletTemp = this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTempSimul;
                EvapInletTemp = this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTempSimul;
                EvapMassFlowRate = this->ChillerHeater(ChillerHeaterNum).Report.EvapmdotSimul;
                if (this->SimulClgDominant) {
                    CHPower = this->ChillerHeater(ChillerHeaterNum).Report.CoolingPowerSimul;
                    this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower = 0.0;
                }
            }
        }

        // Check if it is mode 4, then skip binding local variables
        if (CurrentMode == 4) {
            this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = CurrentMode;
        } else {
            this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.MassFlowRate = EvapMassFlowRate;
            this->ChillerHeater(ChillerHeaterNum).CondOutletNode.MassFlowRate = CondMassFlowRate;
            this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.Temp = EvapOutletTemp;
            this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp = EvapInletTemp;
            this->ChillerHeater(ChillerHeaterNum).CondOutletNode.Temp = CondOutletTemp;
            this->ChillerHeater(ChillerHeaterNum).CondInletNode.Temp = CondInletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = CurrentMode;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatio = state.dataPlantCentralGSHP->ChillerPartLoadRatio;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatio = state.dataPlantCentralGSHP->ChillerCyclingRatio;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate = state.dataPlantCentralGSHP->ChillerFalseLoadRate;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFT = state.dataPlantCentralGSHP->ChillerCapFT;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFT = state.dataPlantCentralGSHP->ChillerEIRFT;
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLR = state.dataPlantCentralGSHP->ChillerEIRFPLR;
            this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower = CoolingPower;
            this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower = CHPower;
            this->ChillerHeater(ChillerHeaterNum).Report.QEvap = QEvaporator;
            this->ChillerHeater(ChillerHeaterNum).Report.QCond = QCondenser;
            this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp = EvapOutletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTemp = EvapInletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp = CondOutletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.CondInletTemp = CondInletTemp;
            this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot = EvapMassFlowRate;
            this->ChillerHeater(ChillerHeaterNum).Report.Condmdot = CondMassFlowRate;
            this->ChillerHeater(ChillerHeaterNum).Report.ActualCOP = ActualCOP;
        }
    }
}

void WrapperSpecs::CalcWrapperModel(EnergyPlusData &state, Real64 &MyLoad, int const LoopNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang, PNNL
    //       DATE WRITTEN   Feb 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  Calculate node information connected to plant & condenser loop

    // METHODOLOGY EMPLOYED:
    //  Use empirical curve fits to model performance at off-reference conditions

    Real64 CurHeatingLoad = 0.0;       // Total heating load chiller heater bank (wrapper) meets
    Real64 CHWOutletTemp;              // Chiller heater bank chilled water outlet temperature
    Real64 CHWOutletMassFlowRate;      // Chiller heater bank chilled water outlet mass flow rate
    Real64 HWOutletTemp;               // Chiller heater bank hot water outlet temperature
    Real64 GLHEOutletTemp;             // Chiller heater bank condenser loop outlet temperature
    Real64 GLHEOutletMassFlowRate;     // Chiller heater bank condenser loop outlet mass flow rate
    Real64 WrapperElecPowerCool(0.0);  // Chiller heater bank total cooling electricity [W]
    Real64 WrapperElecPowerHeat(0.0);  // Chiller heater bank total heating electricity [W]
    Real64 WrapperCoolRate(0.0);       // Chiller heater bank total cooling rate [W]
    Real64 WrapperHeatRate(0.0);       // Chiller heater bank total heating rate [W]
    Real64 WrapperGLHERate(0.0);       // Chiller heater bank total condenser heat transfer rate [W]
    Real64 WrapperElecEnergyCool(0.0); // Chiller heater bank total electric cooling energy [J]
    Real64 WrapperElecEnergyHeat(0.0); // Chiller heater bank total electric heating energy [J]
    Real64 WrapperCoolEnergy(0.0);     // Chiller heater bank total cooling energy [J]
    Real64 WrapperHeatEnergy(0.0);     // Chiller heater bank total heating energy [J]
    Real64 WrapperGLHEEnergy(0.0);     // Chiller heater bank total condenser heat transfer energy [J]

    // Chiller heater bank chilled water inlet mass flow rate
    Real64 CHWInletMassFlowRate = 0.0;

    Real64 HWInletMassFlowRate = 0.0;
    Real64 GLHEInletMassFlowRate = 0.0;
    Real64 CHWInletTemp = state.dataLoopNodes->Node(this->CHWInletNodeNum).Temp;

    // Chiller heater bank hot water inlet temperature
    Real64 HWInletTemp = state.dataLoopNodes->Node(this->HWInletNodeNum).Temp;

    // Chiller heater bank condenser loop inlet temperature
    Real64 GLHEInletTemp = state.dataLoopNodes->Node(this->GLHEInletNodeNum).Temp;

    Real64 CurCoolingLoad = 0.0; // Total cooling load chiller heater bank (wrapper) meets

    // Initiate loads and inlet temperatures each loop
    if (LoopNum == this->CWLoopNum) {
        CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRateMaxAvail;
        HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRate;
        GLHEInletMassFlowRate = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMaxAvail;
        int LoopSideNum = this->CWLoopSideNum;
        this->WrapperCoolingLoad = 0.0;
        CurCoolingLoad = std::abs(MyLoad);
        this->WrapperCoolingLoad = CurCoolingLoad;
        // Set actual mass flow rate at the nodes when it's locked
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == DataPlant::iFlowLock::Locked) {
            CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRate;
        }
        if (CHWInletMassFlowRate == 0.0) GLHEInletMassFlowRate = 0.0;

    } else if (LoopNum == this->HWLoopNum) {
        CHWInletMassFlowRate = state.dataLoopNodes->Node(this->CHWInletNodeNum).MassFlowRate;
        HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRateMaxAvail;
        GLHEInletMassFlowRate = state.dataLoopNodes->Node(this->GLHEInletNodeNum).MassFlowRateMaxAvail;
        int LoopSideNum = this->HWLoopSideNum;
        this->WrapperHeatingLoad = 0.0;
        CurHeatingLoad = MyLoad;
        this->WrapperHeatingLoad = CurHeatingLoad;
        // Set actual mass flow rate at the nodes when it's locked
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == DataPlant::iFlowLock::Locked) {
            HWInletMassFlowRate = state.dataLoopNodes->Node(this->HWInletNodeNum).MassFlowRate;
        }
        if (HWInletMassFlowRate == 0.0) GLHEInletMassFlowRate = 0.0;
    }

    if (LoopNum == this->CWLoopNum) {
        if (this->ControlMode == iCondType::SmartMixing) {
            if (CurCoolingLoad > 0.0 && CHWInletMassFlowRate > 0.0 && GLHEInletMassFlowRate > 0) {

                this->CalcChillerModel(state);
                this->UpdateChillerRecords(state);

                // Initialize local variables only for calculating mass-weighed temperatures
                CHWOutletTemp = 0.0;
                GLHEOutletTemp = 0.0;
                CHWOutletMassFlowRate = 0.0;
                GLHEOutletMassFlowRate = 0.0;

                for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {

                    // Calculated mass flow rate used by individual chiller heater and bypasses
                    CHWOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot;
                    CHWOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp *
                                     (this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot / CHWInletMassFlowRate);
                    WrapperElecPowerCool += this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower;
                    WrapperCoolRate += this->ChillerHeater(ChillerHeaterNum).Report.QEvap;
                    WrapperElecEnergyCool += this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy;
                    WrapperCoolEnergy += this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy;
                    if (GLHEInletMassFlowRate > 0.0) {
                        GLHEOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum).Report.Condmdot;
                        if (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate;
                        GLHEOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp *
                                          (this->ChillerHeater(ChillerHeaterNum).Report.Condmdot / GLHEInletMassFlowRate);
                        WrapperGLHERate += this->ChillerHeater(ChillerHeaterNum).Report.QCond;
                        WrapperGLHEEnergy += this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy;
                    } else {
                        GLHEInletMassFlowRate = 0.0;
                        GLHEOutletMassFlowRate = 0.0;
                        GLHEOutletTemp = GLHEInletTemp;
                        WrapperGLHERate = 0.0;
                        WrapperGLHEEnergy = 0.0;
                    }
                } // End of summation of mass flow rates and mass weighted temperatrue

                // Calculate temperatures for the mixed flows in the chiller bank
                Real64 CHWBypassMassFlowRate = CHWInletMassFlowRate - CHWOutletMassFlowRate;
                if (CHWBypassMassFlowRate > 0.0) {
                    CHWOutletTemp += CHWInletTemp * CHWBypassMassFlowRate / CHWInletMassFlowRate;
                } else {
                    // CHWOutletTemp = CHWOutletTemp; // Self-assignment commented out
                }

                if (GLHEInletMassFlowRate > 0.0) {
                    Real64 GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate;
                    if (GLHEBypassMassFlowRate > 0.0) {
                        GLHEOutletTemp += GLHEInletTemp * GLHEBypassMassFlowRate / GLHEInletMassFlowRate;
                    } else {
                        // GLHEOutletTemp = GLHEOutletTemp; // Self-assignment commented out
                    }
                } else {
                    GLHEOutletTemp = GLHEInletTemp;
                }

                HWOutletTemp = HWInletTemp;

                if (ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr) > 0) {
                    WrapperElecPowerCool += (this->AncillaryPower * ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr));
                }

                state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = CHWOutletTemp;
                state.dataLoopNodes->Node(this->HWOutletNodeNum).Temp = HWOutletTemp;
                state.dataLoopNodes->Node(this->GLHEOutletNodeNum).Temp = GLHEOutletTemp;

            } else {

                // Initialize local variables
                CHWOutletTemp = CHWInletTemp;
                HWOutletTemp = HWInletTemp;
                GLHEOutletTemp = GLHEInletTemp;

                for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                    this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.MassFlowRate = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).CondOutletNode.MassFlowRate = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.Temp = CHWInletTemp;
                    this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp = CHWInletTemp;
                    this->ChillerHeater(ChillerHeaterNum).CondOutletNode.Temp = GLHEInletTemp;
                    this->ChillerHeater(ChillerHeaterNum).CondInletNode.Temp = GLHEInletTemp;
                    this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = 0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatio = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatio = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFT = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFT = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLR = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.QEvap = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.QCond = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp = CHWOutletTemp;
                    this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTemp = CHWInletTemp;
                    this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp = GLHEOutletTemp;
                    this->ChillerHeater(ChillerHeaterNum).Report.CondInletTemp = GLHEInletTemp;
                    this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.Condmdot = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoad = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy = 0.0;
                    this->ChillerHeater(ChillerHeaterNum).Report.ActualCOP = 0.0;
                }
            }

            if (this->SimulHtgDominant || this->SimulClgDominant) {
                state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = CHWOutletTemp;
                this->Report.CHWInletTempSimul = CHWInletTemp;
                this->Report.CHWOutletTempSimul = CHWOutletTemp;
                this->Report.CHWmdotSimul = CHWInletMassFlowRate;
                this->Report.GLHEInletTempSimul = GLHEInletTemp;
                this->Report.GLHEOutletTempSimul = GLHEOutletTemp;
                this->Report.GLHEmdotSimul = GLHEInletMassFlowRate;
                this->Report.TotElecCoolingSimul = WrapperElecEnergyCool;
                this->Report.CoolingEnergySimul = WrapperCoolEnergy;
                this->Report.TotElecCoolingPwrSimul = WrapperElecPowerCool;
                this->Report.CoolingRateSimul = WrapperCoolRate;

            } else {

                state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = CHWOutletTemp;
                state.dataLoopNodes->Node(this->HWOutletNodeNum).Temp = HWOutletTemp;
                state.dataLoopNodes->Node(this->GLHEOutletNodeNum).Temp = GLHEOutletTemp;
                this->Report.CHWInletTemp = CHWInletTemp;
                this->Report.CHWOutletTemp = CHWOutletTemp;
                this->Report.HWInletTemp = HWInletTemp;
                this->Report.HWOutletTemp = HWOutletTemp;
                this->Report.GLHEInletTemp = GLHEInletTemp;
                this->Report.GLHEOutletTemp = GLHEOutletTemp;
                this->Report.CHWmdot = CHWInletMassFlowRate;
                this->Report.HWmdot = HWInletMassFlowRate;
                this->Report.GLHEmdot = GLHEInletMassFlowRate;
                this->Report.TotElecCooling = WrapperElecEnergyCool;
                this->Report.TotElecHeating = WrapperElecEnergyHeat;
                this->Report.CoolingEnergy = WrapperCoolEnergy;
                this->Report.HeatingEnergy = WrapperHeatEnergy;
                this->Report.GLHEEnergy = WrapperGLHEEnergy;
                this->Report.TotElecCoolingPwr = WrapperElecPowerCool;
                this->Report.TotElecHeatingPwr = WrapperElecPowerHeat;
                this->Report.CoolingRate = WrapperCoolRate;
                this->Report.HeatingRate = WrapperHeatRate;
                this->Report.GLHERate = WrapperGLHERate;
            }
            PlantUtilities::SetComponentFlowRate(state,
                                                 CHWInletMassFlowRate,
                                                 this->CHWInletNodeNum,
                                                 this->CHWOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);

            PlantUtilities::SetComponentFlowRate(state,
                                                 HWInletMassFlowRate,
                                                 this->HWInletNodeNum,
                                                 this->HWOutletNodeNum,
                                                 this->HWLoopNum,
                                                 this->HWLoopSideNum,
                                                 this->HWBranchNum,
                                                 this->HWCompNum);

            PlantUtilities::SetComponentFlowRate(state,
                                                 GLHEInletMassFlowRate,
                                                 this->GLHEInletNodeNum,
                                                 this->GLHEOutletNodeNum,
                                                 this->GLHELoopNum,
                                                 this->GLHELoopSideNum,
                                                 this->GLHEBranchNum,
                                                 this->GLHECompNum);

        } // End of cooling

    } else if (LoopNum == this->HWLoopNum) {               // Hot water loop
        if (this->ControlMode == iCondType::SmartMixing) { // Chiller heater component
            if (CurHeatingLoad > 0.0 && HWInletMassFlowRate > 0.0) {

                this->CalcChillerHeaterModel(state);
                this->UpdateChillerHeaterRecords(state);

                // Calculate individual CH units's temperatures and mass flow rates
                CHWOutletTemp = 0.0;
                HWOutletTemp = 0.0;
                GLHEOutletTemp = 0.0;
                CHWOutletMassFlowRate = 0.0;
                Real64 HWOutletMassFlowRate = 0.0;
                GLHEOutletMassFlowRate = 0.0;

                if (this->SimulHtgDominant || this->SimulClgDominant) {
                    if (this->SimulClgDominant) {
                        for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                            int CurrentMode = this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode;
                            CHWInletTemp = this->Report.CHWInletTempSimul;
                            GLHEInletTemp = this->Report.GLHEInletTempSimul;
                            CHWInletMassFlowRate = this->Report.CHWmdotSimul;
                            GLHEInletMassFlowRate = this->Report.GLHEmdotSimul;

                            if (CurrentMode != 0) {     // This chiller heater unit is on
                                if (CurrentMode == 3) { // Heat recovery mode. Both chilled water and hot water connections
                                    CHWOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum)
                                                                 .Report.EvapmdotSimul; // Wrapper evaporator side to plant chilled water loop
                                    HWOutletMassFlowRate +=
                                        this->ChillerHeater(ChillerHeaterNum).Report.Condmdot; // Wrapper condenser side to plant hot water loop
                                    if (HWInletMassFlowRate > 0.0) {
                                        HWOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp *
                                                        (this->ChillerHeater(ChillerHeaterNum).Report.Condmdot /
                                                         HWInletMassFlowRate); // Only calculate in the heat recovery mode
                                    } else {
                                        HWOutletTemp = HWInletTemp;
                                    }
                                } else { // Mode 4. Cooling-only mode with other heat recovery units. Condenser flows.
                                    CHWOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum)
                                                                 .Report.EvapmdotSimul; // Wrapper evaporator side to plant chilled water loop
                                    // Sum condenser node mass flow rates and mass weighed temperatures
                                    if (GLHEInletMassFlowRate > 0.0) {
                                        GLHEOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum).Report.CondmdotSimul;
                                        if (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate;
                                        GLHEOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTempSimul *
                                                          (this->ChillerHeater(ChillerHeaterNum).Report.CondmdotSimul / GLHEInletMassFlowRate);
                                        WrapperGLHERate += this->ChillerHeater(ChillerHeaterNum).Report.QCondSimul;
                                        WrapperGLHEEnergy += this->ChillerHeater(ChillerHeaterNum).Report.CondEnergySimul;
                                    } else {
                                        GLHEInletMassFlowRate = 0.0;
                                        GLHEOutletMassFlowRate = 0.0;
                                        GLHEOutletTemp = GLHEInletTemp;
                                        WrapperGLHERate = 0.0;
                                        WrapperGLHEEnergy = 0.0;
                                    }
                                }
                            } else { // This chiller heater is off
                                // Check if any unit is cooling only mode
                                if (ChillerHeaterNum == this->ChillerHeaterNums) { // All units are heat revocery mode. No condenser flow
                                    GLHEOutletMassFlowRate = 0.0;
                                    GLHEInletMassFlowRate = 0.0;
                                    GLHEOutletTemp = GLHEInletTemp;
                                } else { // At leaset, one of chiller heater units is cooling-only mode
                                         // GLHEOutletMassFlowRate = GLHEOutletMassFlowRate; // Self-assignment commented out
                                         // GLHEOutletTemp = GLHEOutletTemp; // Self-assignment commented out
                                }
                            }
                            // Calculate mass weighed chilled water temperatures
                            if (CHWInletMassFlowRate > 0.0) {
                                CHWOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTempSimul *
                                                 (this->ChillerHeater(ChillerHeaterNum).Report.EvapmdotSimul / CHWInletMassFlowRate);
                            } else {
                                CHWOutletTemp = CHWInletTemp;
                            }

                            WrapperElecPowerCool += this->ChillerHeater(ChillerHeaterNum).Report.CoolingPowerSimul; // Cooling electricity
                            WrapperCoolRate += this->ChillerHeater(ChillerHeaterNum).Report.QEvapSimul;
                            WrapperElecEnergyCool += this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergySimul;
                            WrapperCoolEnergy += this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergySimul;
                            // Avoid double counting wrapper energy use
                            WrapperElecPowerHeat = 0.0;
                            WrapperHeatRate = 0.0;
                            WrapperHeatEnergy = 0.0;
                        }

                        // Calculate chilled water temperature
                        if (CHWInletMassFlowRate > 0.0) {
                            Real64 CHWBypassMassFlowRate = CHWInletMassFlowRate - CHWOutletMassFlowRate;
                            if (CHWBypassMassFlowRate > 0.0) {
                                CHWOutletTemp += CHWInletTemp * CHWBypassMassFlowRate / CHWInletMassFlowRate;
                            } else { // No bypass withnin a wrapper
                                     // CHWOutletTemp = CHWOutletTemp; // Self-assignment commented out
                            }
                        } else {
                            CHWOutletTemp = CHWInletTemp;
                        }
                        // Calculate hot water outlet temperature
                        if (HWInletMassFlowRate > 0.0) {
                            Real64 HWBypassMassFlowRate = HWInletMassFlowRate - HWOutletMassFlowRate;
                            if (HWBypassMassFlowRate > 0.0) {
                                HWOutletTemp += HWInletTemp * HWBypassMassFlowRate / HWInletMassFlowRate;
                            } else {
                                // HWOutletTemp = HWOutletTemp; // Self-assignment commented out
                            }
                        } else {
                            HWOutletTemp = HWInletTemp;
                        }
                        // Calculate condenser outlet temperature
                        if (GLHEInletMassFlowRate > 0.0) {
                            Real64 GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate;
                            if (GLHEBypassMassFlowRate > 0.0) {
                                GLHEOutletTemp += GLHEInletTemp * GLHEBypassMassFlowRate / GLHEInletMassFlowRate;
                            } else {
                                // GLHEOutletTemp = GLHEOutletTemp; // Self-assignment commented out
                            }
                        } else {
                            GLHEOutletTemp = GLHEInletTemp;
                        }

                        // Add ancilliary power if scheduled
                        if (ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr) > 0) {
                            WrapperElecPowerCool += (this->AncillaryPower * ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr));
                        }

                        // Electricity should be counted once for cooling in this mode
                        WrapperElecEnergyHeat = 0.0;

                    } else if (this->SimulHtgDominant) { // Heating dominant simultaneous clg/htg mode

                        for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                            // Set temperatures and mass flow rates for the cooling side
                            int CurrentMode = this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode;
                            CHWInletTemp = this->Report.CHWInletTempSimul;
                            CHWInletMassFlowRate = this->Report.CHWmdotSimul;

                            if (CurrentMode != 0) {     // This chiller heater unit is on
                                if (CurrentMode == 3) { // Heat recovery mode. Both chilled water and hot water connections
                                    CHWOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum)
                                                                 .Report.EvapmdotSimul; // Wrapper evaporator side to plant chilled water loop
                                    HWOutletMassFlowRate +=
                                        this->ChillerHeater(ChillerHeaterNum).Report.Condmdot; // Wrapper condenser side to plant hot water loop
                                    if (CHWInletMassFlowRate > 0.0) {
                                        CHWOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTempSimul *
                                                         (this->ChillerHeater(ChillerHeaterNum).Report.EvapmdotSimul /
                                                          CHWInletMassFlowRate); // Only need to calculate in the heat recovery mode
                                    } else {
                                        CHWOutletTemp = CHWInletTemp;
                                    }
                                } else { // Mode 5. Heating only mode with other heat recovery units
                                    HWOutletMassFlowRate +=
                                        this->ChillerHeater(ChillerHeaterNum).Report.Condmdot; // Wrapper condenser side to plant hot water loop
                                    if (GLHEInletMassFlowRate > 0.0) {
                                        GLHEOutletMassFlowRate +=
                                            this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot; // Wrapper evaporator side to plant condenser loop
                                        if (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate;
                                        GLHEOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp *
                                                          (this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot / GLHEInletMassFlowRate);
                                        WrapperGLHERate += this->ChillerHeater(ChillerHeaterNum).Report.QEvap;
                                        WrapperGLHEEnergy += this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy;
                                    } else {
                                        GLHEInletMassFlowRate = 0.0;
                                        GLHEOutletMassFlowRate = 0.0;
                                        GLHEOutletTemp = GLHEInletTemp;
                                        WrapperGLHERate = 0.0;
                                        WrapperGLHEEnergy = 0.0;
                                    }
                                } // End of heat recovery mode

                            } else { // This chiller heater is off

                                // Check if any unit is heating only mode
                                if (ChillerHeaterNum == this->ChillerHeaterNums) { // All are heat revocery mode. No condenser flow
                                    GLHEOutletMassFlowRate = 0.0;
                                    GLHEInletMassFlowRate = 0.0;
                                    GLHEOutletTemp = GLHEInletTemp;
                                } else { // At leaset, one of chiller heater units is heating only mode
                                         // GLHEOutletMassFlowRate = GLHEOutletMassFlowRate; // Self-assignment commented out
                                         // GLHEOutletTemp = GLHEOutletTemp; // Self-assignment commented out
                                }
                            }

                            // Calculate mass weighed hot water temperatures
                            if (HWInletMassFlowRate > 0.0) {
                                HWOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp *
                                                (this->ChillerHeater(ChillerHeaterNum).Report.Condmdot /
                                                 HWInletMassFlowRate); // Always heating as long as heating load remains
                            } else {
                                HWOutletTemp = HWInletTemp;
                            }

                            WrapperElecPowerHeat += this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower;
                            WrapperHeatRate += this->ChillerHeater(ChillerHeaterNum).Report.QCond;
                            WrapperElecEnergyHeat += this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy;
                            WrapperHeatEnergy += this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy;

                            // Avoid double counting wrapper energy use
                            WrapperElecPowerCool = 0.0;
                            WrapperCoolRate = 0.0;
                        }
                        // Calculate chilled water outlet temperature
                        if (CHWInletMassFlowRate > 0.0) {
                            Real64 CHWBypassMassFlowRate = CHWInletMassFlowRate - CHWOutletMassFlowRate;
                            if (CHWBypassMassFlowRate > 0.0) {
                                CHWOutletTemp += CHWInletTemp * CHWBypassMassFlowRate / CHWInletMassFlowRate;
                            } else { // No bypass withnin a wrapper
                                     // CHWOutletTemp = CHWOutletTemp; // Self-assignment commented out
                            }
                        } else {
                            CHWOutletTemp = CHWInletTemp;
                        }
                        // Calculate hot water outlet temperature
                        if (HWInletMassFlowRate > 0.0) {
                            Real64 HWBypassMassFlowRate = HWInletMassFlowRate - HWOutletMassFlowRate;
                            if (HWBypassMassFlowRate > 0.0) {
                                HWOutletTemp += HWInletTemp * HWBypassMassFlowRate / HWInletMassFlowRate;
                            } else {
                                // HWOutletTemp = HWOutletTemp; // Self-assignment commented out
                            }
                        } else {
                            HWOutletTemp = HWInletTemp;
                        }
                        // Calculate condenser outlet temperature
                        if (GLHEInletMassFlowRate > 0.0) {
                            Real64 GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate;
                            if (GLHEBypassMassFlowRate > 0.0) {
                                GLHEOutletTemp += GLHEInletTemp * GLHEBypassMassFlowRate / GLHEInletMassFlowRate;
                            } else {
                                // GLHEOutletTemp = GLHEOutletTemp; // Self-assignment commented out
                            }
                        } else {
                            GLHEOutletTemp = GLHEInletTemp;
                        }

                        // Check if ancilliary power is used
                        if (ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr) > 0) {
                            WrapperElecPowerHeat += (this->AncillaryPower * ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr));
                        }

                        // Electricity should be counted once
                        WrapperElecEnergyCool = 0.0;

                    } // End of simultaneous clg/htg mode calculations

                } else { // Heating only mode (mode 2)

                    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                        HWOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum).Report.Condmdot;
                        HWOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp *
                                        this->ChillerHeater(ChillerHeaterNum).Report.Condmdot / HWInletMassFlowRate;
                        WrapperElecPowerHeat += this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower;
                        WrapperHeatRate += this->ChillerHeater(ChillerHeaterNum).Report.QCond;
                        WrapperElecEnergyHeat += this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy;
                        WrapperHeatEnergy += this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy;

                        if (GLHEInletMassFlowRate > 0.0) {
                            GLHEOutletMassFlowRate += this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot;
                            if (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate;
                            GLHEOutletTemp += this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp *
                                              (this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot / GLHEInletMassFlowRate);
                            WrapperGLHERate += this->ChillerHeater(ChillerHeaterNum).Report.QEvap;
                            WrapperGLHEEnergy += this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy;
                        } else { // No source water flow
                            GLHEOutletMassFlowRate = 0.0;
                            GLHEInletMassFlowRate = 0.0;
                            GLHEOutletTemp = GLHEInletTemp;
                            WrapperGLHERate = 0.0;
                            WrapperGLHEEnergy = 0.0;
                        }
                    }

                    // Calculate hot water outlet temperature
                    if (HWInletMassFlowRate > 0.0) {
                        Real64 HWBypassMassFlowRate = HWInletMassFlowRate - HWOutletMassFlowRate;
                        if (HWBypassMassFlowRate > 0.0) {
                            HWOutletTemp += HWInletTemp * HWBypassMassFlowRate / HWInletMassFlowRate;
                        } else {
                            // HWOutletTemp = HWOutletTemp; // Self-assignment commented out
                            if (HWOutletTemp > HWInletTemp) HWOutletTemp = HWInletTemp;
                        }
                    } else {
                        HWOutletTemp = HWInletTemp;
                    }

                    // Calculate condenser outlet temperature
                    if (GLHEInletMassFlowRate > 0.0) {
                        Real64 GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate;
                        if (GLHEBypassMassFlowRate > 0.0) {
                            GLHEOutletTemp += GLHEInletTemp * GLHEBypassMassFlowRate / GLHEInletMassFlowRate;
                        } else {
                            // GLHEOutletTemp = GLHEOutletTemp; // Self-assignment commented out
                        }
                    } else {
                        GLHEOutletTemp = GLHEInletTemp;
                    }

                    CHWOutletTemp = CHWInletTemp;

                    // Add ancilliary power if necessary
                    if (ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr) > 0) {
                        WrapperElecPowerHeat += (this->AncillaryPower * ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr));
                    }

                } // End of calculations

                PlantUtilities::SetComponentFlowRate(state,
                                                     CHWInletMassFlowRate,
                                                     this->CHWInletNodeNum,
                                                     this->CHWOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);

                PlantUtilities::SetComponentFlowRate(state,
                                                     HWInletMassFlowRate,
                                                     this->HWInletNodeNum,
                                                     this->HWOutletNodeNum,
                                                     this->HWLoopNum,
                                                     this->HWLoopSideNum,
                                                     this->HWBranchNum,
                                                     this->HWCompNum);

                PlantUtilities::SetComponentFlowRate(state,
                                                     GLHEInletMassFlowRate,
                                                     this->GLHEInletNodeNum,
                                                     this->GLHEOutletNodeNum,
                                                     this->GLHELoopNum,
                                                     this->GLHELoopSideNum,
                                                     this->GLHEBranchNum,
                                                     this->GLHECompNum);

                // Local variables
                this->Report.CHWInletTemp = CHWInletTemp;
                this->Report.CHWOutletTemp = CHWOutletTemp;
                this->Report.HWInletTemp = HWInletTemp;
                this->Report.HWOutletTemp = HWOutletTemp;
                this->Report.GLHEInletTemp = GLHEInletTemp;
                this->Report.GLHEOutletTemp = GLHEOutletTemp;
                this->Report.CHWmdot = CHWInletMassFlowRate;
                this->Report.HWmdot = HWInletMassFlowRate;
                this->Report.GLHEmdot = GLHEInletMassFlowRate;
                this->Report.TotElecCooling = WrapperElecEnergyCool;
                this->Report.TotElecHeating = WrapperElecEnergyHeat;
                this->Report.CoolingEnergy = WrapperCoolEnergy;
                this->Report.HeatingEnergy = WrapperHeatEnergy;
                this->Report.GLHEEnergy = WrapperGLHEEnergy;
                this->Report.TotElecCoolingPwr = WrapperElecPowerCool;
                this->Report.TotElecHeatingPwr = WrapperElecPowerHeat;
                this->Report.CoolingRate = WrapperCoolRate;
                this->Report.HeatingRate = WrapperHeatRate;
                this->Report.GLHERate = WrapperGLHERate;

                state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = CHWOutletTemp;
                state.dataLoopNodes->Node(this->HWOutletNodeNum).Temp = HWOutletTemp;
                state.dataLoopNodes->Node(this->GLHEOutletNodeNum).Temp = GLHEOutletTemp;

            } else { // Central chiller heater system is off

                CHWOutletTemp = CHWInletTemp;
                HWOutletTemp = HWInletTemp;
                GLHEOutletTemp = GLHEInletTemp;
                state.dataLoopNodes->Node(this->CHWOutletNodeNum).Temp = CHWOutletTemp;
                state.dataLoopNodes->Node(this->HWOutletNodeNum).Temp = HWOutletTemp;
                state.dataLoopNodes->Node(this->GLHEOutletNodeNum).Temp = GLHEOutletTemp;

                if (this->WrapperCoolingLoad == 0.0 && !this->SimulHtgDominant) {

                    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
                        this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.MassFlowRate = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).CondOutletNode.MassFlowRate = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).EvapOutletNode.Temp = CHWInletTemp;
                        this->ChillerHeater(ChillerHeaterNum).EvapInletNode.Temp = CHWInletTemp;
                        this->ChillerHeater(ChillerHeaterNum).CondOutletNode.Temp = GLHEInletTemp;
                        this->ChillerHeater(ChillerHeaterNum).CondInletNode.Temp = GLHEInletTemp;
                        this->ChillerHeater(ChillerHeaterNum).Report.CurrentMode = 0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerPartLoadRatio = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerCyclingRatio = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerCapFT = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFT = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerEIRFPLR = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.QEvap = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.QCond = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.EvapOutletTemp = CHWOutletTemp;
                        this->ChillerHeater(ChillerHeaterNum).Report.EvapInletTemp = CHWInletTemp;
                        this->ChillerHeater(ChillerHeaterNum).Report.CondOutletTemp = GLHEOutletTemp;
                        this->ChillerHeater(ChillerHeaterNum).Report.CondInletTemp = GLHEInletTemp;
                        this->ChillerHeater(ChillerHeaterNum).Report.Evapmdot = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.Condmdot = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoad = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy = 0.0;
                        this->ChillerHeater(ChillerHeaterNum).Report.ActualCOP = 0.0;
                    }

                    this->Report.CHWInletTemp = CHWInletTemp;
                    this->Report.CHWOutletTemp = CHWOutletTemp;
                    this->Report.HWInletTemp = HWInletTemp;
                    this->Report.HWOutletTemp = HWOutletTemp;
                    this->Report.GLHEInletTemp = GLHEInletTemp;
                    this->Report.GLHEOutletTemp = GLHEOutletTemp;
                    this->Report.CHWmdot = CHWInletMassFlowRate;
                    this->Report.HWmdot = HWInletMassFlowRate;
                    this->Report.GLHEmdot = GLHEInletMassFlowRate;
                    this->Report.TotElecCooling = WrapperElecEnergyCool;
                    this->Report.TotElecHeating = WrapperElecEnergyHeat;
                    this->Report.CoolingEnergy = WrapperCoolEnergy;
                    this->Report.HeatingEnergy = WrapperHeatEnergy;
                    this->Report.GLHEEnergy = WrapperGLHEEnergy;
                    this->Report.TotElecCoolingPwr = WrapperElecPowerCool;
                    this->Report.TotElecHeatingPwr = WrapperElecPowerHeat;
                    this->Report.CoolingRate = WrapperCoolRate;
                    this->Report.HeatingRate = WrapperHeatRate;
                    this->Report.GLHERate = WrapperGLHERate;

                    PlantUtilities::SetComponentFlowRate(state,
                                                         CHWInletMassFlowRate,
                                                         this->CHWInletNodeNum,
                                                         this->CHWOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);

                    PlantUtilities::SetComponentFlowRate(state,
                                                         HWInletMassFlowRate,
                                                         this->HWInletNodeNum,
                                                         this->HWOutletNodeNum,
                                                         this->HWLoopNum,
                                                         this->HWLoopSideNum,
                                                         this->HWBranchNum,
                                                         this->HWCompNum);

                    PlantUtilities::SetComponentFlowRate(state,
                                                         GLHEInletMassFlowRate,
                                                         this->GLHEInletNodeNum,
                                                         this->GLHEOutletNodeNum,
                                                         this->GLHELoopNum,
                                                         this->GLHELoopSideNum,
                                                         this->GLHEBranchNum,
                                                         this->GLHECompNum);
                }

            } // Heating loop calculation
        }
    }
}

void WrapperSpecs::UpdateChillerRecords(EnergyPlusData &state) // Wrapper number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Daeho Kang, PNNL
    //       DATE WRITTEN:    Feb 2013

    // PURPOSE OF THIS SUBROUTINE:
    //  Update chiller heater variables

    Real64 SecInTimeStep; // Number of seconds per HVAC system time step, to convert from W (J/s) to J
    int ChillerHeaterNum; // Chiller heater number

    SecInTimeStep = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    for (ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoad =
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy = this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy = this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy = this->ChillerHeater(ChillerHeaterNum).Report.QEvap * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy = this->ChillerHeater(ChillerHeaterNum).Report.QCond * SecInTimeStep;
        if (this->SimulClgDominant || this->SimulHtgDominant) {
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadSimul = this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoad;
            this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergySimul = this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy;
            this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergySimul = this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy;
            this->ChillerHeater(ChillerHeaterNum).Report.CondEnergySimul = this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy;
        }
    }
}

void WrapperSpecs::UpdateChillerHeaterRecords(EnergyPlusData &state) // Wrapper number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Daeho Kang, PNNL
    //       DATE WRITTEN:    Feb 2013

    // Number of seconds per HVAC system time step, to convert from W (J/s) to J
    Real64 SecInTimeStep = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    for (int ChillerHeaterNum = 1; ChillerHeaterNum <= this->ChillerHeaterNums; ++ChillerHeaterNum) {
        this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoad =
            this->ChillerHeater(ChillerHeaterNum).Report.ChillerFalseLoadRate * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.CoolingEnergy = this->ChillerHeater(ChillerHeaterNum).Report.CoolingPower * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.HeatingEnergy = this->ChillerHeater(ChillerHeaterNum).Report.HeatingPower * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.EvapEnergy = this->ChillerHeater(ChillerHeaterNum).Report.QEvap * SecInTimeStep;
        this->ChillerHeater(ChillerHeaterNum).Report.CondEnergy = this->ChillerHeater(ChillerHeaterNum).Report.QCond * SecInTimeStep;
    }
}
void WrapperSpecs::oneTimeInit_new([[maybe_unused]] EnergyPlusData &state)
{
}

void WrapperSpecs::oneTimeInit([[maybe_unused]] EnergyPlusData &state)
{
}

} // namespace EnergyPlus::PlantCentralGSHP
