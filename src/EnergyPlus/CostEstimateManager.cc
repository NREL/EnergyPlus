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
#include <algorithm>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/CostEstimateManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPhotovoltaics.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace CostEstimateManager {

    // Module containing the routines dealing with the Cost Estimation capability of EnergyPlus

    // MODULE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April-May 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // produce a construction cost estimate report based on
    // input and certain building calculations by EnergyPlus

    // METHODOLOGY EMPLOYED:
    // Routine gets called once, Just before tabular reports.
    // Cost Estimate objects are child objects that will inherit from
    // other input objects.
    // Uses a Line Item metaphor where each Cost Estimate object is a line
    // Create report using utility subroutines taken from OutputReportTabular (by J.Glazer)

    // Using/Aliasing
    void SimCostEstimate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         BGriffith
        //       DATE WRITTEN   April 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Entry point; manage calls to other subroutines

        if (state.dataCostEstimateManager->GetCostInput) {
            GetCostEstimateInput(state);
            state.dataCostEstimateManager->GetCostInput = false;
        }

        // Need to add check Costs before this will work properly

        if (state.dataGlobal->KickOffSimulation) return;

        if (state.dataCostEstimateManager->DoCostEstimate) {
            CalcCostEstimate(state);
        }
    }

    void GetCostEstimateInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         BGriffith
        //       DATE WRITTEN   April 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get Cost Estimation object input.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item; // Item to be "gotten"
        int NumCostAdjust;
        int NumRefAdjust;
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine

        state.dataCostEstimateManager->NumLineItems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ComponentCost:LineItem");

        if (state.dataCostEstimateManager->NumLineItems == 0) {
            state.dataCostEstimateManager->DoCostEstimate = false;
            return;
        } else {
            state.dataCostEstimateManager->DoCostEstimate = true;
            //    WriteTabularFiles = .TRUE.
        }

        if (!allocated(state.dataCostEstimateManager->CostLineItem)) {
            state.dataCostEstimateManager->CostLineItem.allocate(state.dataCostEstimateManager->NumLineItems);
        }
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
        cCurrentModuleObject = "ComponentCost:LineItem";

        for (Item = 1; Item <= state.dataCostEstimateManager->NumLineItems; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus);
            state.dataCostEstimateManager->CostLineItem(Item).LineName = state.dataIPShortCut->cAlphaArgs(1);
            state.dataCostEstimateManager->CostLineItem(Item).ParentObjType = state.dataIPShortCut->cAlphaArgs(3);
            state.dataCostEstimateManager->CostLineItem(Item).ParentObjName = state.dataIPShortCut->cAlphaArgs(4);
            state.dataCostEstimateManager->CostLineItem(Item).PerEach = state.dataIPShortCut->rNumericArgs(1);
            state.dataCostEstimateManager->CostLineItem(Item).PerSquareMeter = state.dataIPShortCut->rNumericArgs(2);
            state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap = state.dataIPShortCut->rNumericArgs(3);
            state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP = state.dataIPShortCut->rNumericArgs(4);
            state.dataCostEstimateManager->CostLineItem(Item).PerCubicMeter = state.dataIPShortCut->rNumericArgs(5);
            state.dataCostEstimateManager->CostLineItem(Item).PerCubMeterPerSec = state.dataIPShortCut->rNumericArgs(6);
            state.dataCostEstimateManager->CostLineItem(Item).PerUAinWattperDelK = state.dataIPShortCut->rNumericArgs(7);
            state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataIPShortCut->rNumericArgs(8);
        }

        // most input error checking to be performed later within Case construct in Calc routine.

        cCurrentModuleObject = "ComponentCost:Adjustments";
        NumCostAdjust = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (NumCostAdjust == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     1,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus);
            state.dataCostEstimateManager->CurntBldg.MiscCostperSqMeter = state.dataIPShortCut->rNumericArgs(1);
            state.dataCostEstimateManager->CurntBldg.DesignFeeFrac = state.dataIPShortCut->rNumericArgs(2);
            state.dataCostEstimateManager->CurntBldg.ContractorFeeFrac = state.dataIPShortCut->rNumericArgs(3);
            state.dataCostEstimateManager->CurntBldg.ContingencyFrac = state.dataIPShortCut->rNumericArgs(4);
            state.dataCostEstimateManager->CurntBldg.BondCostFrac = state.dataIPShortCut->rNumericArgs(5);
            state.dataCostEstimateManager->CurntBldg.CommissioningFrac = state.dataIPShortCut->rNumericArgs(6);
            state.dataCostEstimateManager->CurntBldg.RegionalModifier = state.dataIPShortCut->rNumericArgs(7);

        } else if (NumCostAdjust > 1) {
            ShowSevereError(state, cCurrentModuleObject + ": Only one instance of this object is allowed.");
            ErrorsFound = true;
        }

        cCurrentModuleObject = "ComponentCost:Reference";
        NumRefAdjust = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (NumRefAdjust == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     1,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus);
            state.dataCostEstimateManager->RefrncBldg.LineItemTot = state.dataIPShortCut->rNumericArgs(1);
            state.dataCostEstimateManager->RefrncBldg.MiscCostperSqMeter = state.dataIPShortCut->rNumericArgs(2);
            state.dataCostEstimateManager->RefrncBldg.DesignFeeFrac = state.dataIPShortCut->rNumericArgs(3);
            state.dataCostEstimateManager->RefrncBldg.ContractorFeeFrac = state.dataIPShortCut->rNumericArgs(4);
            state.dataCostEstimateManager->RefrncBldg.ContingencyFrac = state.dataIPShortCut->rNumericArgs(5);
            state.dataCostEstimateManager->RefrncBldg.BondCostFrac = state.dataIPShortCut->rNumericArgs(6);
            state.dataCostEstimateManager->RefrncBldg.CommissioningFrac = state.dataIPShortCut->rNumericArgs(7);
            state.dataCostEstimateManager->RefrncBldg.RegionalModifier = state.dataIPShortCut->rNumericArgs(8);

        } else if (NumRefAdjust > 1) {
            ShowSevereError(state, cCurrentModuleObject + " : Only one instance of this object is allowed.");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing cost estimate input");
        }

        CheckCostEstimateInput(state, ErrorsFound);

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing cost estimate input");
        }
    }

    void CheckCostEstimateInput(EnergyPlusData &state, bool &ErrorsFound) // Set to true if errors in input, fatal at end of routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         BGriffith
        //       DATE WRITTEN   April 2004
        //       MODIFIED       February 2005, M. J. Witte
        //                        Add subscript to DX coil variables due to new multimode DX coil
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the Cost Estimate based on inputs.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;            // do-loop counter for line items
        int ThisConstructID; // hold result of FindItem searching for Construct name
        int ThisSurfID;      // hold result from findItem
        int ThisZoneID;      // hold result from findItem

        std::string ThisConstructStr;
        auto &Zone(state.dataHeatBal->Zone);

        int thisCoil; // index of named coil in its derived type
        int thisChil;
        int thisPV;

        // Setup working data structure for line items
        for (Item = 1; Item <= state.dataCostEstimateManager->NumLineItems; ++Item) { // Loop thru cost line items

            state.dataCostEstimateManager->CostLineItem(Item).LineNumber = Item;

            {
                auto const SELECT_CASE_var(state.dataCostEstimateManager->CostLineItem(Item).ParentObjType);

                if (SELECT_CASE_var == "GENERAL") {

                } else if (SELECT_CASE_var == "CONSTRUCTION") {

                    // test input for problems
                    //  is PerSquareMeter non-zero? if it is are other cost per values set?
                    //   issue warning that 'Cost Estimate requested for Constructions with zero cost per unit area
                    if (state.dataCostEstimateManager->CostLineItem(Item).PerSquareMeter == 0) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\" Construction object needs non-zero construction costs per square meter");
                        ErrorsFound = true;
                    }

                    ThisConstructStr = state.dataCostEstimateManager->CostLineItem(Item).ParentObjName;
                    ThisConstructID = UtilityRoutines::FindItem(ThisConstructStr, state.dataConstruction->Construct);
                    if (ThisConstructID == 0) { // do any surfaces have the specified construction? If not issue warning.
                        ShowWarningError(state,
                                         "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                             "\" Construction=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName +
                                             "\", no surfaces have the Construction specified");
                        ShowContinueError(state, "No costs will be calculated for this Construction.");
                        //        ErrorsFound = .TRUE.
                        continue;
                    }

                } else if ((SELECT_CASE_var == "COIL:DX") || (SELECT_CASE_var == "COIL:COOLING:DX:SINGLESPEED") ||
                           (SELECT_CASE_var == "COIL:COOLING:DX")) {
                    // test if too many pricing methods are set in user input
                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0)) {
                        ShowSevereError(state,
                                        format("ComponentCost:LineItem: \"{}\", {}, too many pricing methods specified",
                                               state.dataCostEstimateManager->CostLineItem(Item).LineName,
                                               SELECT_CASE_var));
                        ErrorsFound = true;
                    }
                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0)) {
                        ShowSevereError(state,
                                        format("ComponentCost:LineItem: \"{}\", {}, too many pricing methods specified",
                                               state.dataCostEstimateManager->CostLineItem(Item).LineName,
                                               SELECT_CASE_var));
                        ErrorsFound = true;
                    }
                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0)) {
                        ShowSevereError(state,
                                        format("ComponentCost:LineItem: \"{}\", {}, too many pricing methods specified",
                                               state.dataCostEstimateManager->CostLineItem(Item).LineName,
                                               SELECT_CASE_var));
                        ErrorsFound = true;
                    }
                    //  check for wildcard * in object name..
                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == "*") { // wildcard, apply to all such components

                    } else if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ShowSevereError(state,
                                        format("ComponentCost:LineItem: \"{}\", {}, too many pricing methods specified",
                                               state.dataCostEstimateManager->CostLineItem(Item).LineName,
                                               SELECT_CASE_var));
                        ErrorsFound = true;

                    } else { // assume name is probably useful
                        bool coilFound = false;
                        auto &parentObjName = state.dataCostEstimateManager->CostLineItem(Item).ParentObjName;
                        if ((SELECT_CASE_var == "COIL:DX") || (SELECT_CASE_var == "COIL:COOLING:DX:SINGLESPEED")) {
                            if (UtilityRoutines::FindItem(parentObjName, state.dataDXCoils->DXCoil) > 0) coilFound = true;
                        } else if (SELECT_CASE_var == "COIL:COOLING:DX") {
                            if (CoilCoolingDX::factory(state, parentObjName) != -1) {
                                coilFound = true;
                            }
                        }
                        if (!coilFound) {
                            ShowWarningError(state,
                                             format("ComponentCost:LineItem: \"{}\", {}, invalid coil specified",
                                                    state.dataCostEstimateManager->CostLineItem(Item).LineName,
                                                    SELECT_CASE_var));
                            ShowContinueError(state,
                                              format("Coil Specified=\"{}\", calculations will not be completed for this item.",
                                                     state.dataCostEstimateManager->CostLineItem(Item).ParentObjName));
                        }
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {

                    // test if too many pricing methods are set in user input
                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0)) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Coil:Heating:Fuel, too many pricing methods specified");
                        ErrorsFound = true;
                    }
                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0)) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Coil:Heating:Fuel, too many pricing methods specified");
                        ErrorsFound = true;
                    }
                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0)) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Coil:Heating:Fuel, too many pricing methods specified");
                        ErrorsFound = true;
                    }
                    //  check for wildcard * in object name..
                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == "*") { // wildcard, apply to all such components

                    } else if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Coil:Heating:Fuel, need to specify a Reference Object Name");
                        ErrorsFound = true;

                    } else { // assume name is probably useful
                        thisCoil = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName,
                                                             state.dataHeatingCoils->HeatingCoil);
                        if (thisCoil == 0) {
                            ShowWarningError(state,
                                             "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                 "\", Coil:Heating:Fuel, invalid coil specified");
                            ShowContinueError(state,
                                              "Coil Specified=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName +
                                                  "\", calculations will not be completed for this item.");
                        }
                    }

                } else if (SELECT_CASE_var == "CHILLER:ELECTRIC") {
                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Chiller:Electric, need to specify a Reference Object Name");
                        ErrorsFound = true;
                    }
                    thisChil = 0;
                    int chillNum = 0;
                    for (auto &ch : state.dataPlantChillers->ElectricChiller) {
                        chillNum++;
                        if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == ch.Name) {
                            thisChil = chillNum;
                        }
                    }
                    if (thisChil == 0) {
                        ShowWarningError(state,
                                         "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                             "\", Chiller:Electric, invalid chiller specified.");
                        ShowContinueError(state,
                                          "Chiller Specified=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName +
                                              "\", calculations will not be completed for this item.");
                    }

                } else if (SELECT_CASE_var == "DAYLIGHTING:CONTROLS") {

                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == "*") { // wildcard, apply to all such components
                    } else if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Daylighting:Controls, need to specify a Reference Object Name");
                        ErrorsFound = true;
                    } else {
                        ThisZoneID = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, Zone);
                        if (ThisZoneID > 0) {
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataDaylightingData->ZoneDaylight(ThisZoneID).totRefPts;
                        } else {
                            ShowSevereError(state,
                                            "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                "\", Daylighting:Controls, need to specify a valid zone name");
                            ShowContinueError(state, "Zone specified=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName + "\".");
                            ErrorsFound = true;
                        }
                    }

                } else if (SELECT_CASE_var == "SHADING:ZONE:DETAILED") {
                    if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ThisSurfID =
                            UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, state.dataSurface->Surface);
                        if (ThisSurfID > 0) {
                            ThisZoneID = UtilityRoutines::FindItem(state.dataSurface->Surface(ThisSurfID).ZoneName, Zone);
                            if (ThisZoneID == 0) {
                                ShowSevereError(state,
                                                "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                    "\", Shading:Zone:Detailed, need to specify a valid zone name");
                                ShowContinueError(state, "Zone specified=\"" + state.dataSurface->Surface(ThisSurfID).ZoneName + "\".");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                "\", Shading:Zone:Detailed, need to specify a valid surface name");
                            ShowContinueError(state,
                                              "Surface specified=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName + "\".");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Shading:Zone:Detailed, specify a Reference Object Name");
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "LIGHTS") {

                    if ((state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) &&
                        (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0)) {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Lights, too many pricing methods specified");
                        ErrorsFound = true;
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap != 0.0) {
                        if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                            ThisZoneID = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, Zone);
                            if (ThisZoneID == 0) {
                                ShowSevereError(state,
                                                "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                    "\", Lights, need to specify a valid zone name");
                                ShowContinueError(state,
                                                  "Zone specified=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName + "\".");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                "\", Lights, need to specify a Reference Object Name");
                            ErrorsFound = true;
                        }
                    }

                } else if (SELECT_CASE_var == "GENERATOR:PHOTOVOLTAIC") {

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap != 0.0) {
                        if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                            thisPV = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName,
                                                               state.dataPhotovoltaic->PVarray);
                            if (thisPV > 0) {
                                if (state.dataPhotovoltaic->PVarray(thisPV).PVModelType != DataPhotovoltaics::PVModel::Simple) {
                                    ShowSevereError(state,
                                                    "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                        "\", Generator:Photovoltaic, only available for model type PhotovoltaicPerformance:Simple");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(state,
                                                "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                    "\", Generator:Photovoltaic, need to specify a valid PV array");
                                ShowContinueError(state,
                                                  "PV Array specified=\"" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjName + "\".");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                                "\", Generator:Photovoltaic, need to specify a Reference Object Name");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                            "\", Generator:Photovoltaic, need to specify a per-kilowatt cost ");
                        ErrorsFound = true;
                    }

                } else {
                    ShowWarningError(state,
                                     "ComponentCost:LineItem: \"" + state.dataCostEstimateManager->CostLineItem(Item).LineName +
                                         "\", invalid cost item -- not included in cost estimate.");
                    ShowContinueError(state, "... invalid object type=" + state.dataCostEstimateManager->CostLineItem(Item).ParentObjType);
                }
            }
        }
    }

    void CalcCostEstimate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         BGriffith
        //       DATE WRITTEN   April 2004
        //       MODIFIED       February 2005, M. J. Witte
        //                        Add subscript to DX coil variables due to new multimode DX coil
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the Cost Estimate based on inputs.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;            // do-loop counter for line items
        int ThisConstructID; // hold result of FindItem searching for Construct name
        int ThisSurfID;      // hold result from findItem
        int ThisZoneID;      // hold result from findItem

        auto &Zone(state.dataHeatBal->Zone);
        std::string ThisConstructStr;

        Array1D_bool uniqueSurfMask;
        Array1D<Real64> SurfMultipleARR;
        int surf;     // do-loop counter for checking for surfaces for uniqueness
        int thisCoil; // index of named coil in its derived type
        bool WildcardObjNames;
        int thisChil;
        int thisPV;
        Real64 Multipliers;

        // Setup working data structure for line items
        for (Item = 1; Item <= state.dataCostEstimateManager->NumLineItems; ++Item) { // Loop thru cost line items

            state.dataCostEstimateManager->CostLineItem(Item).LineNumber = Item;

            {
                auto const SELECT_CASE_var(state.dataCostEstimateManager->CostLineItem(Item).ParentObjType);

                if (SELECT_CASE_var == "GENERAL") {

                    state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                    state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                    state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                        state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;

                } else if (SELECT_CASE_var == "CONSTRUCTION") {

                    ThisConstructStr = state.dataCostEstimateManager->CostLineItem(Item).ParentObjName;
                    ThisConstructID = UtilityRoutines::FindItem(ThisConstructStr, state.dataConstruction->Construct);
                    // need to determine unique surfaces... some surfaces are shared by zones and hence doubled
                    uniqueSurfMask.dimension(state.dataSurface->TotSurfaces, true); // init to true and change duplicates to false
                    SurfMultipleARR.dimension(state.dataSurface->TotSurfaces, 1.0);
                    for (surf = 1; surf <= state.dataSurface->TotSurfaces; ++surf) {
                        if (state.dataSurface->Surface(surf).ExtBoundCond >= 1) {
                            if (state.dataSurface->Surface(surf).ExtBoundCond < surf) { // already cycled through
                                uniqueSurfMask(surf) = false;
                            }
                        }
                        if (state.dataSurface->Surface(surf).Construction == 0) { // throw out others for now
                            uniqueSurfMask(surf) = false;
                        }
                        if (state.dataSurface->Surface(surf).Zone > 0) {
                            SurfMultipleARR(surf) =
                                Zone(state.dataSurface->Surface(surf).Zone).Multiplier * Zone(state.dataSurface->Surface(surf).Zone).ListMultiplier;
                        }
                    }
                    // determine which surfaces have the construction type  and if any are duplicates..
                    Real64 Qty(0.0);
                    for (int i = 1; i <= state.dataSurface->TotSurfaces; ++i) {
                        auto const &s(state.dataSurface->Surface(i));
                        if (uniqueSurfMask(i) && (s.Construction == ThisConstructID)) Qty += s.Area * SurfMultipleARR(i);
                    }
                    state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty;
                    state.dataCostEstimateManager->CostLineItem(Item).Units = "m2";
                    state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerSquareMeter;
                    state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                        state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;

                    uniqueSurfMask.deallocate();
                    SurfMultipleARR.deallocate();

                } else if ((SELECT_CASE_var == "COIL:DX") || (SELECT_CASE_var == "COIL:COOLING:DX:SINGLESPEED")) {
                    WildcardObjNames = false;
                    thisCoil = 0;
                    //  check for wildcard * in object name..
                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == "*") { // wildcard, apply to all such components
                        WildcardObjNames = true;
                    } else if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        thisCoil =
                            UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, state.dataDXCoils->DXCoil);
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) {
                        if (WildcardObjNames) {
                            Real64 Qty(0.0);
                            for (auto const &e : state.dataDXCoils->DXCoil)
                                Qty += e.RatedTotCap(1);
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot cool cap.)";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                        if (thisCoil > 0) {
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataDXCoils->DXCoil(thisCoil).RatedTotCap(1) / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot cool cap.)";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0) {
                        if (WildcardObjNames) state.dataCostEstimateManager->CostLineItem(Item).Qty = double(state.dataDXCoils->NumDXCoils);
                        if (thisCoil > 0) state.dataCostEstimateManager->CostLineItem(Item).Qty = 1.0;
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0) {
                        if (WildcardObjNames) {
                            Real64 Qty(0.0);
                            for (auto const &e : state.dataDXCoils->DXCoil) {
                                int maxSpeed = e.RatedCOP.size();
                                Qty += e.RatedCOP(maxSpeed) * e.RatedTotCap(maxSpeed);
                            }
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*COP (total, rated) ";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                        if (thisCoil > 0) {
                            int maxSpeed = state.dataDXCoils->DXCoil(thisCoil).RatedCOP.size();
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataDXCoils->DXCoil(thisCoil).RatedCOP(maxSpeed) *
                                                                                    state.dataDXCoils->DXCoil(thisCoil).RatedTotCap(maxSpeed) /
                                                                                    1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*COP (total, rated) ";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX") {
                    WildcardObjNames = false;
                    auto &parentObjName = state.dataCostEstimateManager->CostLineItem(Item).ParentObjName;
                    bool coilFound = false;
                    //  check for wildcard * in object name..
                    if (parentObjName == "*") { // wildcard, apply to all such components
                        WildcardObjNames = true;
                    } else if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        // Purposefully not calling the factory here
                        // Input validation happens before we get to this point
                        // The factory throws a severe error when the coil is not found
                        // Finding the coil like this here to protects against another SevereError being thrown out of context
                        auto &v = state.dataCoilCooingDX->coilCoolingDXs;
                        auto isInCoils = [&parentObjName](const CoilCoolingDX &coil) { return coil.name == parentObjName; };
                        auto it = std::find_if(v.begin(), v.end(), isInCoils);
                        if (it != v.end()) {
                            thisCoil = std::distance(v.begin(), it);
                            coilFound = true;
                        }
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) {
                        if (WildcardObjNames) {
                            Real64 Qty(0.0);
                            for (auto const &e : state.dataCoilCooingDX->coilCoolingDXs)
                                Qty += e.performance.normalMode.ratedGrossTotalCap;
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot cool cap.)";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                        if (coilFound) {
                            state.dataCostEstimateManager->CostLineItem(Item).Qty =
                                state.dataCoilCooingDX->coilCoolingDXs[thisCoil].performance.normalMode.ratedGrossTotalCap / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot cool cap.)";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0) {
                        if (WildcardObjNames)
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = double(state.dataCoilCooingDX->coilCoolingDXs.size());
                        if (coilFound) state.dataCostEstimateManager->CostLineItem(Item).Qty = 1.0;
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0) {
                        if (WildcardObjNames) {
                            Real64 Qty(0.0);
                            for (auto const &e : state.dataCoilCooingDX->coilCoolingDXs) {
                                auto &maxSpeed = e.performance.normalMode.speeds.back();
                                Real64 COP = maxSpeed.original_input_specs.gross_rated_cooling_COP;
                                Qty += COP * e.performance.normalMode.ratedGrossTotalCap;
                            }
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*COP (total, rated) ";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                        if (coilFound) {
                            auto &maxSpeed = state.dataCoilCooingDX->coilCoolingDXs[thisCoil].performance.normalMode.speeds.back();
                            Real64 COP = maxSpeed.original_input_specs.gross_rated_cooling_COP;
                            state.dataCostEstimateManager->CostLineItem(Item).Qty =
                                COP * state.dataCoilCooingDX->coilCoolingDXs[thisCoil].performance.normalMode.ratedGrossTotalCap / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*COP (total, rated) ";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                    WildcardObjNames = false;
                    thisCoil = 0;
                    //  check for wildcard * in object name..
                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == "*") { // wildcard, apply to all such components
                        WildcardObjNames = true;
                    } else if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        thisCoil = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName,
                                                             state.dataHeatingCoils->HeatingCoil);
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0) {
                        if (WildcardObjNames) {
                            Real64 Qty(0.0);
                            for (auto const &e : state.dataHeatingCoils->HeatingCoil)
                                if (e.HCoilType_Num == 1) Qty += e.NominalCapacity;
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot heat cap.)";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                        if (thisCoil > 0) {
                            state.dataCostEstimateManager->CostLineItem(Item).Qty =
                                state.dataHeatingCoils->HeatingCoil(thisCoil).NominalCapacity / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot heat cap.)";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0) {
                        if (WildcardObjNames) state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataHeatingCoils->NumHeatingCoils;
                        if (thisCoil > 0) state.dataCostEstimateManager->CostLineItem(Item).Qty = 1.0;
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0) {
                        if (WildcardObjNames) {
                            Real64 Qty(0.0);
                            for (auto const &e : state.dataHeatingCoils->HeatingCoil)
                                if (e.HCoilType_Num == 1) Qty += e.Efficiency * e.NominalCapacity;
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = Qty / 1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*Eff (total, rated) ";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                        if (thisCoil > 0) {
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataHeatingCoils->HeatingCoil(thisCoil).Efficiency *
                                                                                    state.dataHeatingCoils->HeatingCoil(thisCoil).NominalCapacity /
                                                                                    1000.0;
                            state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*Eff (total, rated) ";
                            state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                            state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                        }
                    }

                } else if (SELECT_CASE_var == "CHILLER:ELECTRIC") {
                    thisChil = 0;
                    int chillNum = 0;
                    for (auto &ch : state.dataPlantChillers->ElectricChiller) {
                        chillNum++;
                        if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == ch.Name) {
                            thisChil = chillNum;
                        }
                    }
                    if ((thisChil > 0) && (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap > 0.0)) {
                        state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataPlantChillers->ElectricChiller(thisChil).NomCap / 1000.0;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (tot cool cap.)";
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                    }
                    if ((thisChil > 0) && (state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP > 0.0)) {
                        state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataPlantChillers->ElectricChiller(thisChil).COP *
                                                                                state.dataPlantChillers->ElectricChiller(thisChil).NomCap / 1000.0;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "kW*COP (total, rated) ";
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerKWCapPerCOP;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                    }
                    if ((thisChil > 0) && (state.dataCostEstimateManager->CostLineItem(Item).PerEach > 0.0)) {
                        state.dataCostEstimateManager->CostLineItem(Item).Qty = 1.0;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                    }

                } else if (SELECT_CASE_var == "DAYLIGHTING:CONTROLS") {

                    if (state.dataCostEstimateManager->CostLineItem(Item).ParentObjName == "*") { // wildcard, apply to all such components
                        state.dataCostEstimateManager->CostLineItem(Item).Qty =
                            sum(state.dataDaylightingData->ZoneDaylight, &DataDaylighting::ZoneDaylightCalc::totRefPts);
                    } else if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ThisZoneID = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, Zone);
                        if (ThisZoneID > 0) {
                            state.dataCostEstimateManager->CostLineItem(Item).Qty = state.dataDaylightingData->ZoneDaylight(ThisZoneID).totRefPts;
                        }
                    }

                    state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                    state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                    state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                        state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;

                } else if (SELECT_CASE_var == "SHADING:ZONE:DETAILED") {
                    if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                        ThisSurfID =
                            UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, state.dataSurface->Surface);
                        if (ThisSurfID > 0) {
                            ThisZoneID = UtilityRoutines::FindItem(state.dataSurface->Surface(ThisSurfID).ZoneName, Zone);
                            if (ThisZoneID > 0) {
                                state.dataCostEstimateManager->CostLineItem(Item).Qty =
                                    state.dataSurface->Surface(ThisSurfID).Area * Zone(ThisZoneID).Multiplier * Zone(ThisZoneID).ListMultiplier;
                                state.dataCostEstimateManager->CostLineItem(Item).Units = "m2";
                                state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                    state.dataCostEstimateManager->CostLineItem(Item).PerSquareMeter;
                                state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                    state.dataCostEstimateManager->CostLineItem(Item).Qty *
                                    state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                            }
                        }
                    }

                } else if (SELECT_CASE_var == "LIGHTS") {

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerEach != 0.0) {
                        state.dataCostEstimateManager->CostLineItem(Item).Qty = 1.0;
                        state.dataCostEstimateManager->CostLineItem(Item).Units = "Ea.";
                        state.dataCostEstimateManager->CostLineItem(Item).ValuePer = state.dataCostEstimateManager->CostLineItem(Item).PerEach;
                        state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                            state.dataCostEstimateManager->CostLineItem(Item).Qty * state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                    }

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap != 0.0) {
                        if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                            ThisZoneID = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName, Zone);
                            if (ThisZoneID > 0) {
                                Real64 Qty(0.0);
                                for (auto const &e : state.dataHeatBal->Lights)
                                    if (e.ZonePtr == ThisZoneID) Qty += e.DesignLevel;
                                state.dataCostEstimateManager->CostLineItem(Item).Qty =
                                    (Zone(ThisZoneID).Multiplier * Zone(ThisZoneID).ListMultiplier / 1000.0) *
                                    Qty; // this handles more than one light object per zone.
                                state.dataCostEstimateManager->CostLineItem(Item).Units = "kW";
                                state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                    state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                                state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                    state.dataCostEstimateManager->CostLineItem(Item).Qty *
                                    state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                            }
                        }
                    }

                } else if (SELECT_CASE_var == "GENERATOR:PHOTOVOLTAIC") {

                    if (state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap != 0.0) {
                        if (!state.dataCostEstimateManager->CostLineItem(Item).ParentObjName.empty()) {
                            thisPV = UtilityRoutines::FindItem(state.dataCostEstimateManager->CostLineItem(Item).ParentObjName,
                                                               state.dataPhotovoltaic->PVarray);
                            if (thisPV > 0) {
                                ThisZoneID = UtilityRoutines::FindItem(
                                    state.dataSurface->Surface(state.dataPhotovoltaic->PVarray(thisPV).SurfacePtr).ZoneName, Zone);
                                if (ThisZoneID == 0) {
                                    Multipliers = 1.0;
                                } else {
                                    Multipliers = Zone(ThisZoneID).Multiplier * Zone(ThisZoneID).ListMultiplier;
                                }
                                if (state.dataPhotovoltaic->PVarray(thisPV).PVModelType == DataPhotovoltaics::PVModel::Simple) {
                                    state.dataCostEstimateManager->CostLineItem(Item).Qty =
                                        1000.0 * state.dataPhotovoltaic->PVarray(thisPV).SimplePVModule.AreaCol *
                                        state.dataPhotovoltaic->PVarray(thisPV).SimplePVModule.PVEfficiency * Multipliers / 1000.0;
                                }
                                state.dataCostEstimateManager->CostLineItem(Item).Units = "kW (rated)";
                                state.dataCostEstimateManager->CostLineItem(Item).ValuePer =
                                    state.dataCostEstimateManager->CostLineItem(Item).PerKiloWattCap;
                                state.dataCostEstimateManager->CostLineItem(Item).LineSubTotal =
                                    state.dataCostEstimateManager->CostLineItem(Item).Qty *
                                    state.dataCostEstimateManager->CostLineItem(Item).ValuePer;
                            }
                        }
                    }
                }
            }
        }

        // now sum up the line items, result for the current building

        state.dataCostEstimateManager->CurntBldg.LineItemTot = sum(state.dataCostEstimateManager->CostLineItem, &CostLineItemStruct::LineSubTotal);
    }

} // namespace CostEstimateManager

} // namespace EnergyPlus
