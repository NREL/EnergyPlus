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
#include <iostream>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPhotovoltaics.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace PhotovoltaicThermalCollectors {

    // Module containing the routines dealing with the photovoltaic thermal collectors

    // MODULE INFORMATION:
    //       AUTHOR         Brent. Griffith
    //       DATE WRITTEN   June-August 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // collect models related to PVT or hybrid, photovoltaic - thermal solar collectors

    // METHODOLOGY EMPLOYED:
    // The approach is to have one PVT structure that works with different models.
    //  the PVT model reuses photovoltaic modeling in Photovoltaics.cc for electricity generation.
    //  the electric load center and "generator" is all accessed thru PV objects and models.
    //  this module is for the thermal portion of PVT.
    //  the first model is a "simple" or "ideal" model useful for sizing, early design, or policy analyses
    //  Simple PV/T model just converts incoming solar to electricity and temperature rise of a working fluid.

    int const SimplePVTmodel(1001);
    int const BIPVTmodel(1002);

    Real64 const SimplePVTWaterSizeFactor(1.905e-5); // [ m3/s/m2 ] average of collectors in SolarCollectors.idf

    int NumPVT(0); // count of all types of PVT in input file

    //   static bool GetInputFlag(true); // First time, input is "gotten"

    //   void clear_state()
    //   {
    //       GetInputFlag = true;
    //       NumPVT = 0;
    //       PVT.deallocate();
    //   }

    PlantComponent *PVTCollectorStruct::factory(EnergyPlusData &state, std::string const &objectName)
    {
        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        for (auto &thisComp : state.dataPhotovoltaicThermalCollector->PVT) {
            if (thisComp.Name == objectName) {
                return &thisComp;
            }
        }

        // If we didn't find it, fatal
        ShowFatalError(state, "Solar Thermal Collector Factory: Error getting inputs for object named: " + objectName);
        // Shut up the compiler
        return nullptr;
    }

    void PVTCollectorStruct::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
    {
        this->initialize(state, true);
        this->size(state);
    }

    void PVTCollectorStruct::simulate(EnergyPlusData &state,
                                      [[maybe_unused]] const PlantLocation &calledFromLocation,
                                      bool const FirstHVACIteration,
                                      [[maybe_unused]] Real64 &CurLoad,
                                      [[maybe_unused]] bool const RunFlag)
    {

        this->initialize(state, FirstHVACIteration);
        this->control(state);
        this->calculate(state);
        this->update(state);
    }

    void GetPVTcollectorsInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       K. Haddad, March 2020, add support to read inputs for BIPVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get input for PVT and BIPVT objects

        int Item;                // Item to be "gotten"
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        using ScheduleManager::GetScheduleIndex;

        // Object Data
        Array1D<SimplePVTModelStruct> tmpSimplePVTperf;
        Array1D<BIPVTModelStruct> tmpBIPVTperf;

        // first load the 'Simple' performance object info into temporary structure
        state.dataIPShortCut->cCurrentModuleObject = "SolarCollectorPerformance:PhotovoltaicThermal:Simple";
        int NumSimplePVTPerform = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        if (NumSimplePVTPerform > 0) GetPVTSimpleCollectorsInput(state, NumSimplePVTPerform, tmpSimplePVTperf);

        // load the 'BIPVT' performance object info into temporary structure
        state.dataIPShortCut->cCurrentModuleObject = "SolarCollectorPerformance:PhotovoltaicThermal:BIPVT";
        int NumBIPVTPerform = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        if (NumBIPVTPerform > 0) GetBIPVTCollectorsInput(state, NumBIPVTPerform, tmpBIPVTperf);

        // now get main PVT objects
        state.dataIPShortCut->cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
        state.dataPhotovoltaicThermalCollector->NumPVT =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        if (state.dataPhotovoltaicThermalCollector->NumPVT > 0)
            GetMainPVTInput(
                state, state.dataPhotovoltaicThermalCollector->NumPVT, state.dataPhotovoltaicThermalCollector->PVT, tmpSimplePVTperf, tmpBIPVTperf);
        if (allocated(tmpSimplePVTperf)) tmpSimplePVTperf.deallocate();
        if (allocated(tmpBIPVTperf)) tmpBIPVTperf.deallocate();
    }

    void GetPVTSimpleCollectorsInput(EnergyPlusData &state, int NumSimplePVTPerform, Array1D<SimplePVTModelStruct> &tmpSimplePVTperf)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       K. Haddad, March 2020, subroutine created from original code in subroutine
        //                      "GetPVTcollectorsInput" to read inputs for PVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get input for PVT Simple objects

        int Item;                // Item to be "gotten"
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        using ScheduleManager::GetScheduleIndex;

        tmpSimplePVTperf.allocate(NumSimplePVTPerform);
        for (Item = 1; Item <= NumSimplePVTPerform; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound))
                continue;
            tmpSimplePVTperf(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Fixed")) {
                tmpSimplePVTperf(Item).ThermEfficMode = ThermEfficEnum::FIXED;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Scheduled")) {
                tmpSimplePVTperf(Item).ThermEfficMode = ThermEfficEnum::SCHEDULED;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            tmpSimplePVTperf(Item).ThermalActiveFract = state.dataIPShortCut->rNumericArgs(1);
            tmpSimplePVTperf(Item).ThermEffic = state.dataIPShortCut->rNumericArgs(2);
            tmpSimplePVTperf(Item).ThermEffSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
            if ((tmpSimplePVTperf(Item).ThermEffSchedNum == 0) && (tmpSimplePVTperf(Item).ThermEfficMode == ThermEfficEnum::SCHEDULED)) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            tmpSimplePVTperf(Item).SurfEmissivity = state.dataIPShortCut->rNumericArgs(3);
        }
    }

    void GetBIPVTCollectorsInput(EnergyPlusData &state, int NumBIPVTPerform, Array1D<BIPVTModelStruct> &tmpBIPVTperf)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       K. Haddad, March 2020, subroutine created from original code in subroutine
        //                      "GetPVTcollectorsInput" to read inputs for BIPVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get input for BIPVT objects

        int Item;                // Item to be "gotten"
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        using ScheduleManager::GetScheduleIndex;

        tmpBIPVTperf.allocate(NumBIPVTPerform);
        for (Item = 1; Item <= NumBIPVTPerform; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound))
                continue;
            tmpBIPVTperf(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
            tmpBIPVTperf(Item).OSCMName = state.dataIPShortCut->cAlphaArgs(2);
            tmpBIPVTperf(Item).PVEffGapWidth = state.dataIPShortCut->rNumericArgs(1);
            tmpBIPVTperf(Item).EffCollHeight = state.dataIPShortCut->rNumericArgs(2);
            tmpBIPVTperf(Item).EffCollWidth = state.dataIPShortCut->rNumericArgs(3);
            tmpBIPVTperf(Item).PVTranAbsProduct = state.dataIPShortCut->rNumericArgs(4);
            tmpBIPVTperf(Item).BackMatTranAbsProduct = state.dataIPShortCut->rNumericArgs(5);
            tmpBIPVTperf(Item).PVAreaFract = state.dataIPShortCut->rNumericArgs(6);
            tmpBIPVTperf(Item).PVRTop = state.dataIPShortCut->rNumericArgs(7);
            tmpBIPVTperf(Item).PVRBot = state.dataIPShortCut->rNumericArgs(8);
            tmpBIPVTperf(Item).PVGEmiss = state.dataIPShortCut->rNumericArgs(9);
            tmpBIPVTperf(Item).BackMatEmiss = state.dataIPShortCut->rNumericArgs(10);
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                tmpBIPVTperf(Item).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                tmpBIPVTperf(Item).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                if (tmpBIPVTperf(Item).SchedPtr == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cAlphaFieldNames(3) + "not found=" + state.dataIPShortCut->cAlphaArgs(3) + " in " +
                                        state.dataIPShortCut->cCurrentModuleObject + " =" + tmpBIPVTperf(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
            }
        }
    }

    void GetMainPVTInput(EnergyPlusData &state,
                         int NumPVT,
                         Array1D<PVTCollectorStruct> &PVT,
                         Array1D<SimplePVTModelStruct> tmpSimplePVTperf,
                         Array1D<BIPVTModelStruct> tmpBIPVTperf)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       K. Haddad, March 2020, add linkage to BIPVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get input for main PVT objects

        int Item;                // Item to be "gotten"
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine

        PVT.allocate(NumPVT);
        for (Item = 1; Item <= NumPVT; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound))
                continue;
            state.dataPhotovoltaicThermalCollector->PVT(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataPhotovoltaicThermalCollector->PVT(Item).TypeNum = DataPlant::TypeOf_PVTSolarCollectorFlatPlate;

            state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataSurface->Surface);
            // check surface
            if (state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));

                    ShowContinueError(state, "Surface name cannot be blank.");
                } else {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Surface was not found.");
                }
                ErrorsFound = true;
            } else {

                if (!state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).ExtSolar) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Surface must be exposed to solar.");
                    ErrorsFound = true;
                }
                // check surface orientation, warn if upside down
                if ((state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Tilt < -95.0) ||
                    (state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Tilt > 95.0)) {
                    ShowWarningError(state,
                                     "Suspected input problem with " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " +
                                         state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Surface used for solar collector faces down");
                    ShowContinueError(state,
                                      format("Surface tilt angle (degrees from ground outward normal) = {:.2R}",
                                             state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Tilt));
                }
            } // check surface

            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(3) + ", name cannot be blank.");
                ErrorsFound = true;
            } else {
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelName = state.dataIPShortCut->cAlphaArgs(3);
                int ThisParamObj = UtilityRoutines::FindItemInList(state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelName, tmpSimplePVTperf);
                if (ThisParamObj > 0) {
                    state.dataPhotovoltaicThermalCollector->PVT(Item).Simple = tmpSimplePVTperf(ThisParamObj); // entire structure assigned
                    // do one-time setups on input data
                    state.dataPhotovoltaicThermalCollector->PVT(Item).AreaCol =
                        state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Area *
                        state.dataPhotovoltaicThermalCollector->PVT(Item).Simple.ThermalActiveFract;
                    state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelType = SimplePVTmodel;
                } else {
                    ThisParamObj = UtilityRoutines::FindItemInList(state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelName, tmpBIPVTperf);
                    if (ThisParamObj > 0) {
                        state.dataPhotovoltaicThermalCollector->PVT(Item).BIPVT = tmpBIPVTperf(ThisParamObj); // entire structure assigned
                        // do one-time setups on input data
                        state.dataPhotovoltaicThermalCollector->PVT(Item).AreaCol =
                            state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Area *
                            state.dataPhotovoltaicThermalCollector->PVT(Item).BIPVT.PVAreaFract;
                        state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelType = BIPVTmodel;
                    } else {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                        ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(3) + ", was not found.");
                        ErrorsFound = true;
                    }
                }
            }
            if (allocated(state.dataPhotovoltaic->PVarray)) { // then PV input gotten... but don't expect this to be true.
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVnum =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(4), state.dataPhotovoltaic->PVarray);
                // check PV
                if (state.dataPhotovoltaicThermalCollector->PVT(Item).PVnum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else {
                    state.dataPhotovoltaicThermalCollector->PVT(Item).PVname = state.dataIPShortCut->cAlphaArgs(4);
                    state.dataPhotovoltaicThermalCollector->PVT(Item).PVfound = true;
                }
            } else { // no PV or not yet gotten.
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVname = state.dataIPShortCut->cAlphaArgs(4);
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVfound = false;
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Water")) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType = WorkingFluidEnum::LIQUID;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Air")) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType = WorkingFluidEnum::AIR;
            } else {
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(5) + " field cannot be blank.");
                } else {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                }
                ErrorsFound = true;
            }

            if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::LIQUID) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).PlantInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(6),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                state.dataPhotovoltaicThermalCollector->PVT(Item).PlantOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(7),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   state.dataIPShortCut->cAlphaArgs(7),
                                                   "Water Nodes");

                state.dataPhotovoltaicThermalCollector->PVT(Item).WLoopSideNum = DataPlant::DemandSupply_No;
            }

            if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::AIR) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).HVACInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(8),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                state.dataPhotovoltaicThermalCollector->PVT(Item).HVACOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(9),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(8),
                                                   state.dataIPShortCut->cAlphaArgs(9),
                                                   "Air Nodes");
            }

            state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
            state.dataPhotovoltaicThermalCollector->PVT(Item).SizingInit = true;
            if (state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate == DataSizing::AutoSize) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRateWasAutoSized = true;
            }
            if (state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate != DataSizing::AutoSize) {

                if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::LIQUID) {
                    PlantUtilities::RegisterPlantCompDesignFlow(state,
                                                                state.dataPhotovoltaicThermalCollector->PVT(Item).PlantInletNodeNum,
                                                                state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate);
                } else if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::AIR) {
                    state.dataPhotovoltaicThermalCollector->PVT(Item).MaxMassFlowRate =
                        state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate * state.dataEnvrn->StdRhoAir;
                }
                state.dataPhotovoltaicThermalCollector->PVT(Item).SizingInit = false;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for photovoltaic thermal collectors");
        }
    }

    void PVTCollectorStruct::setupReportVars(EnergyPlusData &state)
    {
        SetupOutputVariable(
            state, "Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->Report.ThermPower, "System", "Average", this->Name);

        if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
            SetupOutputVariable(state,
                                "Generator Produced Thermal Energy",
                                OutputProcessor::Unit::J,
                                this->Report.ThermEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarWater",
                                "HeatProduced",
                                _,
                                "Plant");

        } else if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
            SetupOutputVariable(state,
                                "Generator Produced Thermal Energy",
                                OutputProcessor::Unit::J,
                                this->Report.ThermEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarAir",
                                "HeatProduced",
                                _,
                                "System");

            SetupOutputVariable(
                state, "Generator PVT Fluid Bypass Status", OutputProcessor::Unit::None, this->Report.BypassStatus, "System", "Average", this->Name);
        }

        SetupOutputVariable(
            state, "Generator PVT Fluid Inlet Temperature", OutputProcessor::Unit::C, this->Report.TinletWorkFluid, "System", "Average", this->Name);

        SetupOutputVariable(state,
                            "Generator PVT Fluid Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->Report.ToutletWorkFluid,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(
            state, "Generator PVT Fluid Mass Flow Rate", OutputProcessor::Unit::kg_s, this->Report.MdotWorkFluid, "System", "Average", this->Name);
    }

    void PVTCollectorStruct::initialize(EnergyPlusData &state, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       B. Griffith, May 2009, EMS setpoint check
        //                      K. Haddad, March 2020, add support for BIPVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // init for PVT

        static std::string const RoutineName("InitPVTcollectors");

        // Do the one time initializations
        if (this->MyOneTimeFlag) {
            this->setupReportVars(state);
            this->MyOneTimeFlag = false;
        }

        if (this->SetLoopIndexFlag) {
            if (allocated(state.dataPlnt->PlantLoop) && (this->PlantInletNodeNum > 0)) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->TypeNum,
                                                        this->WLoopNum,
                                                        this->WLoopSideNum,
                                                        this->WLoopBranchNum,
                                                        this->WLoopCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitPVTcollectors: Program terminated for previous conditions.");
                }
                this->SetLoopIndexFlag = false;
            }
        }

        // finish set up of PV, because PV get-input follows PVT's get input.
        if (!this->PVfound) {
            if (allocated(state.dataPhotovoltaic->PVarray)) {
                this->PVnum = UtilityRoutines::FindItemInList(this->PVname, state.dataPhotovoltaic->PVarray);
                if (this->PVnum == 0) {
                    ShowSevereError(state, "Invalid name for photovoltaic generator = " + this->PVname);
                    ShowContinueError(state, "Entered in flat plate photovoltaic-thermal collector = " + this->Name);
                } else {
                    this->PVfound = true;
                }
            } else {
                if ((!state.dataGlobal->BeginEnvrnFlag) && (!FirstHVACIteration)) {
                    ShowSevereError(state, "Photovoltaic generators are missing for Photovoltaic Thermal modeling");
                    ShowContinueError(state, "Needed for flat plate photovoltaic-thermal collector = " + this->Name);
                }
            }
        }

        if (!state.dataGlobal->SysSizingCalc && this->MySetPointCheckFlag && state.dataHVACGlobal->DoSetPointTest) {
            for (int PVTindex = 1; PVTindex <= state.dataPhotovoltaicThermalCollector->NumPVT; ++PVTindex) {
                if (state.dataPhotovoltaicThermalCollector->PVT(PVTindex).WorkingFluidType == WorkingFluidEnum::AIR) {
                    if (state.dataLoopNodes->Node(state.dataPhotovoltaicThermalCollector->PVT(PVTindex).HVACOutletNodeNum).TempSetPoint ==
                        DataLoopNode::SensedNodeFlagValue) {
                        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                            ShowSevereError(state, "Missing temperature setpoint for PVT outlet node  ");
                            ShowContinueError(state,
                                              "Add a setpoint manager to outlet node of PVT named " +
                                                  state.dataPhotovoltaicThermalCollector->PVT(PVTindex).Name);
                            state.dataHVACGlobal->SetPointErrorFlag = true;
                        } else {
                            // need call to EMS to check node
                            EMSManager::CheckIfNodeSetPointManagedByEMS(state,
                                                                        state.dataPhotovoltaicThermalCollector->PVT(PVTindex).HVACOutletNodeNum,
                                                                        EMSManager::SPControlType::iTemperatureSetPoint,
                                                                        state.dataHVACGlobal->SetPointErrorFlag);
                            if (state.dataHVACGlobal->SetPointErrorFlag) {
                                ShowSevereError(state, "Missing temperature setpoint for PVT outlet node  ");
                                ShowContinueError(state,
                                                  "Add a setpoint manager to outlet node of PVT named " +
                                                      state.dataPhotovoltaicThermalCollector->PVT(PVTindex).Name);
                                ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node of PVT");
                            }
                        }
                    }
                }
            }
            this->MySetPointCheckFlag = false;
        }

        if (!state.dataGlobal->SysSizingCalc && this->SizingInit && (this->WorkingFluidType == WorkingFluidEnum::AIR)) {
            this->size(state);
        }

        int InletNode = 0;
        int OutletNode = 0;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);
            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                InletNode = this->PlantInletNodeNum;
                OutletNode = this->PlantOutletNodeNum;
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
                OutletNode = this->HVACOutletNodeNum;
            } else {
                assert(false);
            }
        }

        if (state.dataGlobal->BeginEnvrnFlag && this->EnvrnInit) {

            this->MassFlowRate = 0.0;
            this->BypassDamperOff = true;
            this->CoolingUseful = false;
            this->HeatingUseful = false;
            this->Simple.LastCollectorTemp = 0.0;
            this->Simple.CollectorTemp = 0.0;
            this->BIPVT.LastCollectorTemp = 0.0;
            this->BIPVT.CollectorTemp = 0.0;
            this->Report.ThermEfficiency = 0.0;
            this->Report.ThermPower = 0.0;
            this->Report.ThermHeatGain = 0.0;
            this->Report.ThermHeatLoss = 0.0;
            this->Report.ThermEnergy = 0.0;
            this->Report.MdotWorkFluid = 0.0;
            this->Report.TinletWorkFluid = 0.0;
            this->Report.ToutletWorkFluid = 0.0;
            this->Report.BypassStatus = 0.0;

            {
                auto const SELECT_CASE_var(this->WorkingFluidType);

                if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {

                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->WLoopNum).FluidName,
                                                                   DataGlobalConstants::HWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->WLoopNum).FluidIndex,
                                                                   RoutineName);

                    this->MaxMassFlowRate = this->DesignVolFlowRate * rho;

                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       this->MaxMassFlowRate,
                                                       InletNode,
                                                       OutletNode,
                                                       this->WLoopNum,
                                                       this->WLoopSideNum,
                                                       this->WLoopBranchNum,
                                                       this->WLoopCompNum);

                    this->Simple.LastCollectorTemp = 23.0;

                } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                    this->Simple.LastCollectorTemp = 23.0;
                    this->BIPVT.LastCollectorTemp = 23.0;
                }
            }

            this->EnvrnInit = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) this->EnvrnInit = true;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);

            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                // heating only right now, so control flow requests based on incident solar;
                if (state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) > state.dataPhotovoltaic->MinIrradiance) {
                    this->MassFlowRate = this->MaxMassFlowRate;
                } else {
                    this->MassFlowRate = 0.0;
                }

                PlantUtilities::SetComponentFlowRate(
                    state, this->MassFlowRate, InletNode, OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum);
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                this->MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            }
        }
    }

    void PVTCollectorStruct::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing PVT flow rates that
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains hot water flow rate from the plant sizing array.

        bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing:System object and system sizing done

        // Indicator to hardsize and no sizing run
        bool HardSizeNoDesRun = !(state.dataSize->SysSizingRunDone || state.dataSize->ZoneSizingRunDone);

        if (state.dataSize->CurSysNum > 0) {
            CheckThisAirSystemForSizing(state, state.dataSize->CurSysNum, SizingDesRunThisAirSys);
        } else {
            SizingDesRunThisAirSys = false;
        }

        Real64 DesignVolFlowRateDes = 0.0; // Autosize design volume flow for reporting
        int PltSizNum = 0;                 // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound = false;

        if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {

            if (!allocated(state.dataSize->PlantSizData)) return;
            if (!allocated(state.dataPlnt->PlantLoop)) return;

            if (this->WLoopNum > 0) {
                PltSizNum = state.dataPlnt->PlantLoop(this->WLoopNum).PlantSizNum;
            }
            if (this->WLoopSideNum == DataPlant::SupplySide) {
                if (PltSizNum > 0) {
                    if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        DesignVolFlowRateDes = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate;
                    } else {
                        DesignVolFlowRateDes = 0.0;
                    }
                } else {
                    if (this->DesignVolFlowRateWasAutoSized) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            ShowSevereError(state, "Autosizing of PVT solar collector design flow rate requires a Sizing:Plant object");
                            ShowContinueError(state, "Occurs in PVT object=" + this->Name);
                            ErrorsFound = true;
                        }
                    } else { // Hardsized
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && this->DesignVolFlowRate > 0.0) {
                            BaseSizer::reportSizerOutput(state,
                                                         "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                         this->Name,
                                                         "User-Specified Design Flow Rate [m3/s]",
                                                         this->DesignVolFlowRate);
                        }
                    }
                }
            } else if (this->WLoopSideNum == DataPlant::DemandSide) {
                DesignVolFlowRateDes = this->AreaCol * SimplePVTWaterSizeFactor;
            }
            if (this->DesignVolFlowRateWasAutoSized) {
                this->DesignVolFlowRate = DesignVolFlowRateDes;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                 this->Name,
                                                 "Design Size Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateDes);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                 this->Name,
                                                 "Initial Design Size Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateDes);
                }
                PlantUtilities::RegisterPlantCompDesignFlow(state, this->PlantInletNodeNum, this->DesignVolFlowRate);

            } else { // Hardsized with sizing data
                if (this->DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport) {
                    Real64 DesignVolFlowRateUser = this->DesignVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                 this->Name,
                                                 "Design Size Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateDes,
                                                 "User-Specified Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(DesignVolFlowRateDes - DesignVolFlowRateUser) / DesignVolFlowRateUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, "SizeSolarCollector: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Design Flow Rate of {:.5R} [W]", DesignVolFlowRateUser));
                            ShowContinueError(state, format("differs from Design Size Design Flow Rate of {:.5R} [W]", DesignVolFlowRateDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        } // plant component

        if (this->WorkingFluidType == WorkingFluidEnum::AIR) {

            if (state.dataSize->CurSysNum > 0) {
                if (!this->DesignVolFlowRateWasAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (this->DesignVolFlowRate > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                     this->Name,
                                                     "User-Specified Design Flow Rate [m3/s]",
                                                     this->DesignVolFlowRate);
                    }
                } else {
                    CheckSysSizing(state, "SolarCollector:FlatPlate:PhotovoltaicThermal", this->Name);
                    if (state.dataSize->CurOASysNum > 0) {
                        DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow;
                    } else {
                        {
                            auto const SELECT_CASE_var(state.dataSize->CurDuctType);
                            if (SELECT_CASE_var == DataHVACGlobals::Main) {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).SysAirMinFlowRat *
                                                       state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                            } else if (SELECT_CASE_var == DataHVACGlobals::Cooling) {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).SysAirMinFlowRat *
                                                       state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesCoolVolFlow;
                            } else if (SELECT_CASE_var == DataHVACGlobals::Heating) {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesHeatVolFlow;
                            } else {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                            }
                        }
                    }
                    Real64 DesMassFlow = state.dataEnvrn->StdRhoAir * DesignVolFlowRateDes;
                    this->MaxMassFlowRate = DesMassFlow;
                }
                if (!HardSizeNoDesRun) {
                    if (this->DesignVolFlowRateWasAutoSized) {
                        this->DesignVolFlowRate = DesignVolFlowRateDes;
                        BaseSizer::reportSizerOutput(state,
                                                     "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                     this->Name,
                                                     "Design Size Design Flow Rate [m3/s]",
                                                     DesignVolFlowRateDes);
                        this->SizingInit = false;
                    } else {
                        if (this->DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0) {
                            Real64 DesignVolFlowRateUser = this->DesignVolFlowRate;
                            BaseSizer::reportSizerOutput(state,
                                                         "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                         this->Name,
                                                         "Design Size Design Flow Rate [m3/s]",
                                                         DesignVolFlowRateDes,
                                                         "User-Specified Design Flow Rate [m3/s]",
                                                         DesignVolFlowRateUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(DesignVolFlowRateDes - DesignVolFlowRateUser) / DesignVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeSolarCollector: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Design Flow Rate of {:.5R} [W]", DesignVolFlowRateUser));
                                    ShowContinueError(state, format("differs from Design Size Design Flow Rate of {:.5R} [W]", DesignVolFlowRateDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            } else if (state.dataSize->CurZoneEqNum > 0) {
                // PVT is not currently for zone equipment, should not come here.
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void PVTCollectorStruct::control(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       K. Haddad, March 2020, add support for BIPVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // make control decisions for PVT collector

        // METHODOLOGY EMPLOYED:
        // decide if PVT should be in cooling or heat mode and if it should be bypassed or not

        if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
            if ((this->PVTModelType == SimplePVTmodel) || (this->PVTModelType == BIPVTmodel)) {
                if (state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) > state.dataPhotovoltaic->MinIrradiance) {
                    // is heating wanted?
                    //  Outlet node is required to have a setpoint.
                    if (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint > state.dataLoopNodes->Node(this->HVACInletNodeNum).Temp) {
                        this->HeatingUseful = true;
                        this->CoolingUseful = false;
                        this->BypassDamperOff = true;
                    } else {
                        this->HeatingUseful = false;
                        this->CoolingUseful = true;
                        this->BypassDamperOff = false;
                    }
                } else {
                    // is cooling wanted?
                    if (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint < state.dataLoopNodes->Node(this->HVACInletNodeNum).Temp) {
                        this->CoolingUseful = true;
                        this->HeatingUseful = false;
                        this->BypassDamperOff = true;
                    } else {
                        this->CoolingUseful = false;
                        this->HeatingUseful = true;
                        this->BypassDamperOff = false;
                    }
                }
            }

        } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
            if (this->PVTModelType == SimplePVTmodel) {
                if (state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) > state.dataPhotovoltaic->MinIrradiance) {
                    // is heating wanted?
                    this->HeatingUseful = true;
                    this->BypassDamperOff = true;
                } else {
                    // is cooling wanted?
                    this->CoolingUseful = false;
                    this->BypassDamperOff = false;
                }
            }
        }
    }

    void PVTCollectorStruct::calculate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       K. Haddad, March 2020, add support for BIPVT objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate PVT collector thermal performance

        // METHODOLOGY EMPLOYED:

        static std::string const RoutineName("CalcPVTcollectors");

        if (this->PVTModelType == SimplePVTmodel) {
            SimplePVTcalculate(state);
        } else if (this->PVTModelType == BIPVTmodel) {
            BIPVTcalculate(state);
        }
    }

    void PVTCollectorStruct::SimplePVTcalculate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       K. Haddad, March 2020, subroutine crated from original code in subroutine
        //                      "PVTCollectorStruct::calculate()"
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate PVT Simple collector thermal

        // METHODOLOGY EMPLOYED:
        // Current model is "simple" fixed efficiency and simple night sky balance for cooling

        static std::string const RoutineName("CalcSimplePVTcollectors");

        int InletNode(0);

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);
            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                InletNode = this->PlantInletNodeNum;
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
            }
        }

        Real64 mdot = this->MassFlowRate;
        Real64 Tinlet = state.dataLoopNodes->Node(InletNode).Temp;

        Real64 BypassFraction(0.0);
        Real64 PotentialOutletTemp(0.0);

        if (this->HeatingUseful && this->BypassDamperOff && (mdot > 0.0)) {

            Real64 Eff(0.0);

            {
                auto const SELECT_CASE_var(this->Simple.ThermEfficMode);

                if (SELECT_CASE_var == ThermEfficEnum::FIXED) {
                    Eff = this->Simple.ThermEffic;
                } else if (SELECT_CASE_var == ThermEfficEnum::SCHEDULED) {
                    Eff = ScheduleManager::GetCurrentScheduleValue(state, this->Simple.ThermEffSchedNum);
                    this->Simple.ThermEffic = Eff;
                }
            }

            Real64 PotentialHeatGain = state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) * Eff * this->AreaCol;

            if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                Real64 Winlet = state.dataLoopNodes->Node(InletNode).HumRat;
                Real64 CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                if (mdot * CpInlet > 0.0) {
                    PotentialOutletTemp = Tinlet + PotentialHeatGain / (mdot * CpInlet);
                } else {
                    PotentialOutletTemp = Tinlet;
                }
                // now compare heating potential to setpoint and figure bypass fraction
                if (PotentialOutletTemp > state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint) { // need to modulate
                    if (Tinlet != PotentialOutletTemp) {
                        BypassFraction =
                            (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint - PotentialOutletTemp) / (Tinlet - PotentialOutletTemp);
                    } else {
                        BypassFraction = 0.0;
                    }
                    BypassFraction = max(0.0, BypassFraction);
                    PotentialOutletTemp = state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint;
                    PotentialHeatGain = mdot * Psychrometrics::PsyCpAirFnW(Winlet) * (PotentialOutletTemp - Tinlet);

                } else {
                    BypassFraction = 0.0;
                }
            } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
                Real64 CpInlet = Psychrometrics::CPHW(Tinlet);
                if (mdot * CpInlet != 0.0) { // protect divide by zero
                    PotentialOutletTemp = Tinlet + PotentialHeatGain / (mdot * CpInlet);
                } else {
                    PotentialOutletTemp = Tinlet;
                }
                BypassFraction = 0.0;
            }

            this->Report.ThermEfficiency = Eff;
            this->Report.ThermHeatGain = PotentialHeatGain;
            this->Report.ThermPower = this->Report.ThermHeatGain;
            this->Report.ThermEnergy = this->Report.ThermPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->Report.ThermHeatLoss = 0.0;
            this->Report.TinletWorkFluid = Tinlet;
            this->Report.MdotWorkFluid = mdot;
            this->Report.ToutletWorkFluid = PotentialOutletTemp;
            this->Report.BypassStatus = BypassFraction;

        } else if (this->CoolingUseful && this->BypassDamperOff && (mdot > 0.0)) {
            // calculate cooling using energy balance

            Real64 HrGround(0.0);
            Real64 HrAir(0.0);
            Real64 HcExt(0.0);
            Real64 HrSky(0.0);

            ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                this->SurfNum,
                                                                0.0,
                                                                DataHeatBalance::VerySmooth,
                                                                this->Simple.SurfEmissivity,
                                                                this->Simple.LastCollectorTemp,
                                                                HcExt,
                                                                HrSky,
                                                                HrGround,
                                                                HrAir);

            Real64 WetBulbInlet(0.0);
            Real64 DewPointInlet(0.0);
            Real64 CpInlet(0.0);

            if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                Real64 Winlet = state.dataLoopNodes->Node(InletNode).HumRat;
                CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                WetBulbInlet = Psychrometrics::PsyTwbFnTdbWPb(state, Tinlet, Winlet, state.dataEnvrn->OutBaroPress, RoutineName);
                DewPointInlet = Psychrometrics::PsyTdpFnTdbTwbPb(state, Tinlet, WetBulbInlet, state.dataEnvrn->OutBaroPress, RoutineName);
            } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
                CpInlet = Psychrometrics::CPHW(Tinlet);
            }
            Real64 Tcollector =
                (2.0 * mdot * CpInlet * Tinlet + this->AreaCol * (HrGround * state.dataEnvrn->OutDryBulbTemp + HrSky * state.dataEnvrn->SkyTemp +
                                                                  HrAir * state.dataSurface->Surface(this->SurfNum).OutDryBulbTemp +
                                                                  HcExt * state.dataSurface->Surface(this->SurfNum).OutDryBulbTemp)) /
                (2.0 * mdot * CpInlet + this->AreaCol * (HrGround + HrSky + HrAir + HcExt));

            PotentialOutletTemp = 2.0 * Tcollector - Tinlet;
            this->Report.ToutletWorkFluid = PotentialOutletTemp;
            // trap for air not being cooled below its wetbulb.
            if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                if (PotentialOutletTemp < DewPointInlet) {
                    //  water removal would be needed.. not going to allow that for now.  limit cooling to dew point and model bypass
                    if (Tinlet != PotentialOutletTemp) {
                        BypassFraction = (DewPointInlet - PotentialOutletTemp) / (Tinlet - PotentialOutletTemp);
                    } else {
                        BypassFraction = 0.0;
                    }
                    BypassFraction = max(0.0, BypassFraction);
                    PotentialOutletTemp = DewPointInlet;
                }
            }

            this->Report.MdotWorkFluid = mdot;
            this->Report.TinletWorkFluid = Tinlet;
            this->Report.ToutletWorkFluid = PotentialOutletTemp;
            this->Report.ThermHeatLoss = mdot * CpInlet * (Tinlet - this->Report.ToutletWorkFluid);
            this->Report.ThermHeatGain = 0.0;
            this->Report.ThermPower = -1.0 * this->Report.ThermHeatLoss;
            this->Report.ThermEnergy = this->Report.ThermPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->Report.ThermEfficiency = 0.0;
            this->Simple.LastCollectorTemp = Tcollector;
            this->Report.BypassStatus = BypassFraction;

        } else {
            this->Report.TinletWorkFluid = Tinlet;
            this->Report.ToutletWorkFluid = Tinlet;
            this->Report.ThermHeatLoss = 0.0;
            this->Report.ThermHeatGain = 0.0;
            this->Report.ThermPower = 0.0;
            this->Report.ThermEfficiency = 0.0;
            this->Report.ThermEnergy = 0.0;
            this->Report.BypassStatus = 1.0;
            this->Report.MdotWorkFluid = mdot;
        }
    }

    void PVTCollectorStruct::BIPVTcalculate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       K. Haddad, March 2020, subroutine created from original code in subroutine
        //                      "PVTCollectorStruct::calculate()" to model BIPVT systems.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate BIPVT collector thermal peformancce

        // METHODOLOGY EMPLOYED:
        // ???

        static std::string const RoutineName("CalcBIPVTcollectors");

        int InletNode = this->HVACInletNodeNum;
        Real64 mdot = this->MassFlowRate;
        Real64 Tinlet = state.dataLoopNodes->Node(InletNode).Temp;
        Real64 BypassFraction(0.0);
        Real64 PotentialOutletTemp(0.0);

        if (this->HeatingUseful && this->BypassDamperOff && (mdot > 0.0)) {

            Real64 Eff(0.3);
            Real64 PotentialHeatGain = state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) * Eff * this->AreaCol;
            BIPVT_MaxHeatGain_calculate(state);

            if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                Real64 Winlet = state.dataLoopNodes->Node(InletNode).HumRat;
                Real64 CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                if (mdot * CpInlet > 0.0) {
                    PotentialOutletTemp = Tinlet + PotentialHeatGain / (mdot * CpInlet);
                } else {
                    PotentialOutletTemp = Tinlet;
                }
                // now compare heating potential to setpoint and figure bypass fraction
                if (PotentialOutletTemp > state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint) { // need to modulate
                    if (Tinlet != PotentialOutletTemp) {
                        BypassFraction =
                            (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint - PotentialOutletTemp) / (Tinlet - PotentialOutletTemp);
                    } else {
                        BypassFraction = 0.0;
                    }
                    BypassFraction = max(0.0, BypassFraction);
                    PotentialOutletTemp = state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint;
                    PotentialHeatGain = mdot * Psychrometrics::PsyCpAirFnW(Winlet) * (PotentialOutletTemp - Tinlet);

                } else {
                    BypassFraction = 0.0;
                }
            }

            this->Report.ThermEfficiency = Eff;
            this->Report.ThermHeatGain = PotentialHeatGain;
            this->Report.ThermPower = this->Report.ThermHeatGain;
            this->Report.ThermEnergy = this->Report.ThermPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->Report.ThermHeatLoss = 0.0;
            this->Report.TinletWorkFluid = Tinlet;
            this->Report.MdotWorkFluid = mdot;
            this->Report.ToutletWorkFluid = PotentialOutletTemp;
            this->Report.BypassStatus = BypassFraction;

        } else if (this->CoolingUseful && this->BypassDamperOff && (mdot > 0.0)) {
            // calculate cooling using energy balance

            Real64 HrGround(0.0);
            Real64 HrAir(0.0);
            Real64 HcExt(0.0);
            Real64 HrSky(0.0);

            ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                this->SurfNum,
                                                                0.0,
                                                                DataHeatBalance::VerySmooth,
                                                                this->BIPVT.PVGEmiss,
                                                                this->BIPVT.LastCollectorTemp,
                                                                HcExt,
                                                                HrSky,
                                                                HrGround,
                                                                HrAir);

            Real64 WetBulbInlet(0.0);
            Real64 DewPointInlet(0.0);
            Real64 CpInlet(0.0);

            if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                Real64 Winlet = state.dataLoopNodes->Node(InletNode).HumRat;
                CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                WetBulbInlet = Psychrometrics::PsyTwbFnTdbWPb(state, Tinlet, Winlet, state.dataEnvrn->OutBaroPress, RoutineName);
                DewPointInlet = Psychrometrics::PsyTdpFnTdbTwbPb(state, Tinlet, WetBulbInlet, state.dataEnvrn->OutBaroPress, RoutineName);
            }

            Real64 Tcollector =
                (2.0 * mdot * CpInlet * Tinlet + this->AreaCol * (HrGround * state.dataEnvrn->OutDryBulbTemp + HrSky * state.dataEnvrn->SkyTemp +
                                                                  HrAir * state.dataSurface->Surface(this->SurfNum).OutDryBulbTemp +
                                                                  HcExt * state.dataSurface->Surface(this->SurfNum).OutDryBulbTemp)) /
                (2.0 * mdot * CpInlet + this->AreaCol * (HrGround + HrSky + HrAir + HcExt));

            PotentialOutletTemp = 2.0 * Tcollector - Tinlet;
            this->Report.ToutletWorkFluid = PotentialOutletTemp;
            // trap for air not being cooled below its wetbulb.
            if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                if (PotentialOutletTemp < DewPointInlet) {
                    //  water removal would be needed.. not going to allow that for now.  limit cooling to dew point and model bypass
                    if (Tinlet != PotentialOutletTemp) {
                        BypassFraction = (DewPointInlet - PotentialOutletTemp) / (Tinlet - PotentialOutletTemp);
                    } else {
                        BypassFraction = 0.0;
                    }
                    BypassFraction = max(0.0, BypassFraction);
                    PotentialOutletTemp = DewPointInlet;
                }
            }

            this->Report.MdotWorkFluid = mdot;
            this->Report.TinletWorkFluid = Tinlet;
            this->Report.ToutletWorkFluid = PotentialOutletTemp;
            this->Report.ThermHeatLoss = mdot * CpInlet * (Tinlet - this->Report.ToutletWorkFluid);
            this->Report.ThermHeatGain = 0.0;
            this->Report.ThermPower = -1.0 * this->Report.ThermHeatLoss;
            this->Report.ThermEnergy = this->Report.ThermPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->Report.ThermEfficiency = 0.0;
            this->BIPVT.LastCollectorTemp = Tcollector;
            this->Report.BypassStatus = BypassFraction;

        } else {
            this->Report.TinletWorkFluid = Tinlet;
            this->Report.ToutletWorkFluid = Tinlet;
            this->Report.ThermHeatLoss = 0.0;
            this->Report.ThermHeatGain = 0.0;
            this->Report.ThermPower = 0.0;
            this->Report.ThermEfficiency = 0.0;
            this->Report.ThermEnergy = 0.0;
            this->Report.BypassStatus = 1.0;
            this->Report.MdotWorkFluid = mdot;
        }
    }

    void PVTCollectorStruct::BIPVT_MaxHeatGain_calculate(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         K. Haddad
        //       DATE WRITTEN   March 2020
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the maximum heat transfer from the BIPVT system to the air stream in the channel behind the PV module

        // METHODOLOGY EMPLOYED:
        // ???

        // BIPVT system parameters
        Real64 rpvg_pv = this->BIPVT.PVRTop;       // thermal resistance of glass
        Real64 rpv_1 = this->BIPVT.PVRBot;         // thermal resistance of backing layer
        Real64 w = this->BIPVT.PVEffGapWidth;      // width of BIPVT panel
        Real64 l = this->BIPVT.EffCollHeight;      // length of BIPVT panel
        Real64 emiss_b = this->BIPVT.BackMatEmiss; // emissivity of backing surface
        Real64 emiss_2(0.85);                      // emissivity of bldg surface
        Real64 emiss_pvg = this->BIPVT.PVGEmiss;   // emissivity of glass surface

        // BIPVT model parameters
        Real64 tsurr, tsurrK;                  // surrouding temperature
        Real64 t1, t1K, t1_new;                // temperature of pv backing surface
        Real64 tpv, tpv_new;                   // temperature of pv surface
        Real64 tpvg, tpvgK, tpvg_new;          // temperature of pv glass cover
        Real64 tfout;                          // inlet and outlet fluid temperature
        Real64 tfavg;                          // average fluid temperature
        Real64 hconvf1(100.0);                 // heat transfer coefficient between fluid and backing surface
        Real64 hconvf2(100.0);                 // heat transfer coefficient between fluid and bldg surface
        Real64 hpvg_pv;                        // conductance of pv glass cover
        Real64 hpv_1;                          // conductance of pv backing
        Real64 hrad12;                         // radiative heat transfer coefficient between bldg surface and pv backing surface
        Real64 hrad_surr;                      // radiative heat transfer coefficient between pv glass cover and surrounding
        Real64 q;                              // heat transfer to fluid
        Real64 IAM_pv, b0_pv(0.1), b1_pv(0.0); // pv incidence angle modifier parameters
        Real64 IAM_bs, b0_bs(0.1), b1_bs(0.0); // back surface incidence angle modifier parameters
        Real64 small_num(1.0e-10);             // small real number
        const Real64 sigma(5.67e-8);           // stephan bolzmann constant

        // other parameters
        Real64 a, b, c, d;                               // constants
        Real64 err_tpvg(1.0), err_tpv(1.0), err_t1(1.0); // convergence errors for temperatures
        const Real64 tol(1.0e-3);                        // temperature convergence tolerance
        const Real64 rf(1.0);                            // relaxation factor
        const Real64 degc_to_kelvin(273.15);             // conversion constant degC to Kelvin
        Real64 ebal1, ebal2, ebal3;                      // energy balances on 3 surfaces
        Real64 jj[9];                                    // 3x3 array for coefficient matrix
        Real64 f[3];                                     // 3 element array for constant term
        Real64 y[3];                                     // solution array for tpvg,tpv, and t1
        int m(3);                                        // parameter for number of unknwons
        int i;                                           // index

        // boundary conditions parameters
        int InletNode = this->HVACInletNodeNum;
        Real64 tfin = state.dataLoopNodes->Node(InletNode).Temp;                   // inlet fluid temperature
        Real64 w_in = state.dataLoopNodes->Node(InletNode).HumRat;                 // inlet air humidity ratio
        Real64 cp_in = Psychrometrics::PsyCpAirFnW(w_in);                          // inlet air specific heat
        Real64 tamb = state.dataEnvrn->OutDryBulbTemp;                             // ambient temperature
        Real64 tsky = state.dataEnvrn->SkyTemp;                                    // sky temperature
        Real64 t2 = state.dataSurface->Surface(this->SurfNum).OutDryBulbTemp, t2K; // temperature of bldg surface
        Real64 HrGround(0.0);                                                      // radiation heat transfer coefficient to ground
        Real64 HrAir(0.0);                                                         // radiation heat transfer coefficient to atmosphere
        Real64 HcExt(0.0);                                                         // exterior convection heat transfer coefficient
        Real64 HrSky(0.0);                                                         // radiation heat transfer coefficien to sky
        Real64 mdot = this->MassFlowRate;                                          // fluid mass flow rate
        Real64 s(0.0);                                                             // solar radiation gain at pv surface
        Real64 s1(0.0);                                                            // solar radiation gain at pv backing surface
        Real64 temp = state.dataSurface->Surface(this->SurfNum).ExtConvCoeff;

        ConvectionCoefficients::InitExteriorConvectionCoeff(
            state, this->SurfNum, 0.0, DataHeatBalance::VerySmooth, emiss_pvg, this->BIPVT.LastCollectorTemp, HcExt, HrSky, HrGround, HrAir);
        t1 = (tamb + t2) / 2.0;
        tpv = (tamb + t2) / 2.0;
        tpvg = (tamb + t2) / 2.0;
        hpvg_pv = 1.0 / rpvg_pv;
        hpv_1 = 1.0 / rpv_1;
        tsurr = (tamb * HrGround + tamb * HrAir + tsky * HrSky) / (HrGround + HrAir + HrSky);
        hrad_surr = HrGround + HrAir + HrSky;
        IAM_pv = 1 - b0_pv * (1.0 / state.dataHeatBal->SurfCosIncidenceAngle(this->SurfNum) - 1.0) -
                 b1_pv * pow((1.0 / state.dataHeatBal->SurfCosIncidenceAngle(this->SurfNum) - 1), 2.0);
        IAM_pv = max(0.0, IAM_pv);
        s = state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) * this->BIPVT.PVAreaFract * IAM_pv;
        IAM_bs = 1 - b0_bs * (1.0 / state.dataHeatBal->SurfCosIncidenceAngle(this->SurfNum) - 1.0) -
                 b1_bs * pow((1.0 / state.dataHeatBal->SurfCosIncidenceAngle(this->SurfNum) - 1), 2.0);
        IAM_bs = max(0.0, IAM_bs);
        s1 = state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) * (1.0 - this->BIPVT.PVAreaFract) * IAM_bs;
        while ((err_t1 > tol) || (err_tpv > tol) || (err_tpvg > tol)) {
            a = -(w / (mdot * cp_in)) * (hconvf1 + hconvf2);
            b = (w / (mdot * cp_in)) * (hconvf1 * t1 + hconvf2 * t2);
            tfavg = (1.0 / (a * l)) * (tfin + b / a) * (std::exp(a * l) - 1.0) - b / a;
            t1K = t1 + degc_to_kelvin;
            t2K = t2 + degc_to_kelvin;
            tsurrK = tsurr + degc_to_kelvin;
            tpvgK = tpvg + 273.15;
            hrad12 = sigma * (pow(t1K, 2) + pow(t2K, 2)) * (t1K + t2K) / (1 / emiss_b + 1 / emiss_2 - 1);
            hrad_surr = sigma * emiss_pvg * (pow(tsurrK, 2) + pow(tpvgK, 2)) * (tsurrK + tpvgK);
            for (i = 0; i <= m - 1; i++) {
                f[i] = 0.0;
                y[i] = 0.0;
            }
            for (i = 0; i <= m ^ 2 - 1; i++) {
                jj[i] = 0.0;
            }
            jj[0] = HcExt + hrad_surr + hpvg_pv;
            jj[1] = -hpvg_pv;
            jj[2] = 0.0;
            jj[3] = hpvg_pv;
            jj[4] = -hpv_1 - hpvg_pv;
            jj[5] = hpv_1;
            jj[6] = 0.0;
            jj[7] = hpv_1;
            jj[8] = -hpv_1 - hconvf1 - hrad12;
            f[0] = HcExt * tamb + hrad_surr * tsurr;
            f[1] = -s;
            f[2] = -s1 - hconvf1 * tfavg - hrad12 * t2;
            solve_lin_sys_back_sub(jj, f, y);
            tpvg_new = y[0];
            tpv_new = y[1];
            t1_new = y[2];
            err_tpvg = std::abs((tpvg_new - tpvg) / (tpvg + small_num));
            err_tpv = std::abs((tpv_new - tpv) / (tpv + small_num));
            err_t1 = std::abs((t1_new - t1) / (t1 + small_num));
            tpvg = tpvg + rf * (tpvg_new - tpvg);
            tpv = tpv + rf * (tpv_new - tpv);
            t1 = t1 + rf * (t1_new - t1);
            tfout = (tfin + b / a) * std::exp(a * l) - b / a;
            q = mdot * cp_in * (tfout - tfin);
            ebal1 = s1 + hpv_1 * (tpv - t1) + hconvf1 * (tfavg - t1) + hrad12 * (t2 - t1);
            ebal2 = s + hpvg_pv * (tpvg - tpv) + hpv_1 * (t1 - tpv);
            ebal3 = HcExt * (tpvg - tamb) + hrad_surr * (tpvg - tsurr) + hpvg_pv * (tpvg - tpv);
        }
        /*
        std::cout << tfavg << "\n";
        for (i = 0; i < 3; i++) {
            std::cout << y[i] << "\n";
        }
        */
        // return q
    }

    void PVTCollectorStruct::solve_lin_sys_back_sub(Real64 jj[9], Real64 f[3], Real64 (&y)[3])
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         K. Haddad
        //       DATE WRITTEN   March 2020
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Solve a system of linear equations using Gaussian elimination and back substitution method.

        float sum, dummy1, dummy2, mm, small(1.0e-10);
        int i, j, kk, ii, p, k, m(3);
        bool coeff_not_zero;

        for (i = 0; i < m; i++) {
            y[i] = 0.0;
        }

        for (i = 0; i <= m - 2; i++) {
            coeff_not_zero = false;
            for (j = i; j <= m - 1; j++) {
                if (std::abs(jj[j * m + i]) > small) {
                    coeff_not_zero = true;
                    p = j;
                    break;
                }
            }

            if (coeff_not_zero) {
                if (p != i) {
                    dummy2 = f[i];
                    f[i] = f[p];
                    f[p] = dummy2;
                    for (j = 0; j <= m - 1; j++) {
                        dummy1 = jj[i * m + j];
                        jj[i * m + j] = jj[p * m + j];
                        jj[p * m + j] = dummy1;
                    }
                }
                for (j = i + 1; j <= m - 1; j++) {
                    if (std::abs(jj[i * m + i]) < small) jj[i * m + i] = small;
                    mm = jj[j * m + i] / jj[i * m + i];
                    f[j] = f[j] - mm * f[i];
                    for (k = 0; k <= m - 1; k++) {
                        jj[j * m + k] = jj[j * m + k] - mm * jj[i * m + k];
                    }
                }
            }
        }
        if (std::abs(jj[(m - 1) * m + m - 1]) < small) jj[(m - 1) * m + m - 1] = small;
        y[m - 1] = f[m - 1] / jj[(m - 1) * m + m - 1];
        sum = 0.0;
        for (i = 0; i <= m - 2; i++) {
            ii = m - 2 - i;
            for (j = ii; j <= m - 1; j++) {
                sum = sum + jj[ii * m + j] * y[j];
            }
            if (std::abs(jj[ii * m + ii]) < small) jj[ii * m + ii] = small;
            y[ii] = (f[ii] - sum) / jj[ii * m + ii];
            sum = 0.0;
        }
        /*
                std::cout << "test1" << "\n";
                for (i = 0; i < 3; i++)
                {
                        std::cout << y[i] << "\n";
                }
        */
    }

    void PVTCollectorStruct::update(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int InletNode;
        int OutletNode;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);
            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                InletNode = this->PlantInletNodeNum;
                OutletNode = this->PlantOutletNodeNum;

                PlantUtilities::SafeCopyPlantNode(state, InletNode, OutletNode);
                state.dataLoopNodes->Node(OutletNode).Temp = this->Report.ToutletWorkFluid;

            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
                OutletNode = this->HVACOutletNodeNum;

                // Set the outlet nodes for properties that just pass through & not used
                state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
                state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;
                state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMin = state.dataLoopNodes->Node(InletNode).MassFlowRateMin;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataLoopNodes->Node(InletNode).MassFlowRateMax;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;

                // Set outlet node variables that are possibly changed
                state.dataLoopNodes->Node(OutletNode).Temp = this->Report.ToutletWorkFluid;
                state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat; // assumes dewpoint bound on cooling ....
                state.dataLoopNodes->Node(OutletNode).Enthalpy =
                    Psychrometrics::PsyHFnTdbW(this->Report.ToutletWorkFluid, state.dataLoopNodes->Node(OutletNode).HumRat);
            }
        }
    }

    void GetPVTThermalPowerProduction(EnergyPlusData &state, int const PVindex, Real64 &ThermalPower, Real64 &ThermalEnergy)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int PVTnum(0);

        // first find PVT index that is associated with this PV generator
        for (int loop = 1; loop <= state.dataPhotovoltaicThermalCollector->NumPVT; ++loop) {
            if (!state.dataPhotovoltaicThermalCollector->PVT(loop).PVfound) continue;
            if (state.dataPhotovoltaicThermalCollector->PVT(loop).PVnum == PVindex) { // we found it
                PVTnum = loop;
            }
        }

        if (PVTnum > 0) {
            ThermalPower = state.dataPhotovoltaicThermalCollector->PVT(PVTnum).Report.ThermPower;
            ThermalEnergy = state.dataPhotovoltaicThermalCollector->PVT(PVTnum).Report.ThermEnergy;
        } else {
            ThermalPower = 0.0;
            ThermalEnergy = 0.0;
        }
    }

    int GetAirInletNodeNum(EnergyPlusData &state, std::string const &PVTName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given PVT and returns the air inlet node number.
        // If incorrect PVT name is given, ErrorsFound is returned as true and node number as zero.

        int NodeNum; // node number returned
        int WhichPVT;

        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        WhichPVT = UtilityRoutines::FindItemInList(PVTName, state.dataPhotovoltaicThermalCollector->PVT);
        if (WhichPVT != 0) {
            NodeNum = state.dataPhotovoltaicThermalCollector->PVT(WhichPVT).HVACInletNodeNum;
        } else {
            ShowSevereError(state, "GetAirInletNodeNum: Could not find SolarCollector FlatPlate PhotovoltaicThermal = \"" + PVTName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }
    int GetAirOutletNodeNum(EnergyPlusData &state, std::string const &PVTName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given PVT and returns the air outlet node number.
        // If incorrect PVT name is given, ErrorsFound is returned as true and node number as zero.

        int NodeNum; // node number returned
        int WhichPVT;

        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        WhichPVT = UtilityRoutines::FindItemInList(PVTName, state.dataPhotovoltaicThermalCollector->PVT);
        if (WhichPVT != 0) {
            NodeNum = state.dataPhotovoltaicThermalCollector->PVT(WhichPVT).HVACOutletNodeNum;
        } else {
            ShowSevereError(state, "GetAirInletNodeNum: Could not find SolarCollector FlatPlate PhotovoltaicThermal = \"" + PVTName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int getPVTindexFromName(EnergyPlusData &state, std::string const &objectName)
    {
        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        for (auto it = state.dataPhotovoltaicThermalCollector->PVT.begin(); it != state.dataPhotovoltaicThermalCollector->PVT.end(); ++it) {
            if (it->Name == objectName) {
                return static_cast<int>(std::distance(state.dataPhotovoltaicThermalCollector->PVT.begin(), it) + 1);
            }
        }

        // If we didn't find it, fatal
        ShowFatalError(state, "Solar Thermal Collector GetIndexFromName: Error getting inputs for object named: " + objectName);
        assert(false);
        return 0; // Shutup compiler
    }

    void simPVTfromOASys(EnergyPlusData &state, int const index, bool const FirstHVACIteration)
    {
        PlantLocation dummyLoc(0, 0, 0, 0);
        Real64 dummyCurLoad(0.0);
        bool dummyRunFlag(true);

        state.dataPhotovoltaicThermalCollector->PVT(index).simulate(state, dummyLoc, FirstHVACIteration, dummyCurLoad, dummyRunFlag);
    }

} // namespace PhotovoltaicThermalCollectors

} // namespace EnergyPlus
